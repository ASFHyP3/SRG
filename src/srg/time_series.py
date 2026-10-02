"""Sentinel-1 GSLC time series processing"""

import argparse
import logging
import shutil
from collections.abc import Iterable
from os import mkdir
from pathlib import Path
from secrets import token_hex
from shutil import copyfile

from hyp3lib.aws import upload_file_to_s3
from hyp3lib.fetch import download_file as download_from_http

from hyp3_srg import dem, utils


log = logging.getLogger(__name__)


def get_gslc_uris_from_s3(bucket: str, prefix: str = '') -> list[str]:
    """Retrieve granule (zip files) uris from the given s3 bucket and prefix.

    Args:
        bucket: the s3 bucket name
        prefix: the path after the bucket and before the file

    Returns:
        uris: a list of uris to the zip files
    """
    res = utils.s3_list_objects(bucket, prefix)
    keys = [item['Key'] for item in res['Contents']]
    uris = ['/'.join(['s3://' + bucket, key]) for key in keys]
    return uris


def load_products(uris: Iterable[str], overwrite: bool = False):
    """Load the products from the provided URIs

    Args:
        uris: list of URIs to the SRG GSLC products
        overwrite: overwrite existing products
    """
    work_dir = Path.cwd()
    granule_names = []
    for uri in uris:
        name = Path(Path(uri).name)
        geo_name = name.with_suffix('.geo')
        zip_name = name.with_suffix('.zip')

        product_exists = geo_name.exists() or zip_name.exists()
        if product_exists and not overwrite:
            pass
        elif uri.startswith('s3'):
            utils.download_from_s3(uri, dest_dir=work_dir)
        elif uri.startswith('http'):
            download_from_http(uri, directory=work_dir)
        elif len(Path(uri).parts) > 1:
            shutil.copy(uri, work_dir)

        if not geo_name.exists():
            shutil.unpack_archive(name.with_suffix('.zip'), work_dir)

        granule_names.append(str(name))

    return granule_names


def get_size_from_dem(dem_path: str) -> tuple[int, int]:
    """Get the length and width from a .rsc DEM file

    Args:
        dem_path: path to the .rsc dem file.

    Returns:
        dem_width, dem_length: tuple containing the dem width and dem length
    """
    with open(dem_path) as dem_file:
        width_line = dem_file.readline()
        dem_width = width_line.split()[1]
        length_line = dem_file.readline()
        dem_length = length_line.split()[1]

    return int(dem_width), int(dem_length)


def generate_wrapped_interferograms(
    looks: tuple[int, int],
    baselines: tuple[int, int],
    dem_shape: tuple[int, int],
    work_dir: Path,
) -> None:
    """Generates wrapped interferograms from GSLCs

    Args:
        looks: tuple containing the number range looks and azimuth looks
        baselines: tuple containing the time baseline and spatial baseline
        dem_shape: tuple containing the dem width and dem length
        work_dir: the directory containing the GSLCs
    """
    dem_width, dem_length = dem_shape
    looks_down, looks_across = looks
    time_baseline, spatial_baseline = baselines

    utils.call_stanford_module(
        'sentinel/sbas_list.py',
        args=[time_baseline, spatial_baseline],
        work_dir=work_dir,
    )

    sbas_args = [
        'sbas_list',
        '../elevation.dem.rsc',
        1,
        1,
        dem_width,
        dem_length,
        looks_down,
        looks_across,
    ]
    utils.call_stanford_module('sentinel/ps_sbas_igrams.py', args=sbas_args, work_dir=work_dir)


def unwrap_interferograms(dem_shape: tuple[int, int], unw_shape: tuple[int, int], work_dir: Path) -> None:
    """Unwraps wrapped interferograms in parallel

    Args:
        dem_shape: tuple containing the dem width and dem length
        unw_shape: tuple containing the width and length from the dem.rsc file
        work_dir: the directory containing the wrapped interferograms
    """
    dem_width, dem_length = dem_shape
    unw_width, unw_length = unw_shape

    reduce_dem_args = [
        '../elevation.dem',
        'dem',
        dem_width,
        dem_width // unw_width,
        dem_length // unw_length,
    ]
    utils.call_stanford_module('util/nbymi2', args=reduce_dem_args, work_dir=work_dir)
    utils.call_stanford_module('util/unwrap_parallel.py', args=[unw_width], work_dir=work_dir)


def compute_sbas_velocity_solution(
    threshold: float,
    do_tropo_correction: bool,
    unw_shape: tuple[int, int],
    work_dir: Path,
) -> None:
    """Computes the sbas velocity solution from the unwrapped interferograms

    Args:
        threshold: correlation threshold for picking reference points
        do_tropo_correction: whether or not to apply tropospheric correction
        unw_shape: tuple containing the width and length from the dem.rsc file
        work_dir: the directory containing the wrapped interferograms
    """
    unw_width, unw_length = unw_shape

    utils.call_stanford_module('sbas/sbas_setup.py', args=['sbas_list', 'geolist'], work_dir=work_dir)
    copyfile(work_dir / 'intlist', work_dir / 'unwlist')
    utils.call_stanford_module('util/sed.py', args=['s/int/unw/g', 'unwlist'], work_dir=work_dir)

    ref_point_args = ['unwlist', unw_width, unw_length, threshold]
    utils.call_stanford_module('int/findrefpoints', args=ref_point_args, work_dir=work_dir)

    if do_tropo_correction:
        tropo_correct_args = ['unwlist', unw_width, unw_length]
        utils.call_stanford_module('int/tropocorrect.py', args=tropo_correct_args, work_dir=work_dir)

    with open(work_dir / 'unwlist') as unw_list:
        num_unw_files = len(unw_list.readlines())

    with open(work_dir / 'geolist') as slc_list:
        num_slcs = len(slc_list.readlines())

    sbas_velocity_args = ['unwlist', num_unw_files, num_slcs, unw_width, 'ref_locs']
    utils.call_stanford_module('sbas/sbas', args=sbas_velocity_args, work_dir=work_dir)


def create_time_series(
    work_dir: Path,
    looks: tuple[int, int] = (6, 2),
    baselines: tuple[int, int] = (60, 1000),
    threshold: float = 0.5,
    do_tropo_correction: bool = True,
    process: str = 'sbas',
) -> None:
    """Creates a time series from a stack of GSLCs consisting of interferograms and a velocity solution

    Args:
        looks: tuple containing the number range looks and azimuth looks
        baselines: tuple containing the time baseline and spatial baseline
        threshold: correlation threshold for picking reference points
        do_tropo_correction: whether or not to apply tropospheric correction
        work_dir: the directory containing the GSLCs to do work in
        process: Time series processing ps or sbas
    """
    dem_shape = get_size_from_dem('elevation.dem.rsc')
    generate_wrapped_interferograms(looks=looks, baselines=baselines, dem_shape=dem_shape, work_dir=work_dir)

    unw_shape = get_size_from_dem(str(work_dir / 'dem.rsc'))
    unw_width, unw_length = unw_shape

    if process == 'ps':
        utils.call_stanford_module('ps/psfilter.py', args=['intlist', 'intlist', unw_width], work_dir=work_dir)
        [shutil.move(f'{intf}.interp', str(intf)) for intf in list(work_dir.glob('*.int'))]

    unwrap_interferograms(dem_shape=dem_shape, unw_shape=unw_shape, work_dir=work_dir)

    compute_sbas_velocity_solution(
        threshold=threshold,
        do_tropo_correction=do_tropo_correction,
        unw_shape=unw_shape,
        work_dir=work_dir,
    )


def create_time_series_product_name(
    granule_names: list[str],
    bounds: list[float],
    process: str = 'sbas',
):
    """Create a product name for the given granules.

    Args:
        granule_names: list of the granule names
        bounds: bounding box that was used to generate the GSLCs
        process: Time series processing ps or sbas

    Returns:
        the product name as a string.
    """
    prefix = f'S1_SRG_{process.upper()}'
    split_names = [granule.split('_') for granule in granule_names]

    absolute_orbit = split_names[0][7]
    if split_names[0][0] == 'S1A':
        relative_orbit = str(((int(absolute_orbit) - 73) % 175) + 1)
    else:
        relative_orbit = str(((int(absolute_orbit) - 27) % 175) + 1)

    start_dates = sorted([name[5] for name in split_names])
    earliest_granule = start_dates[0]
    latest_granule = start_dates[-1]

    def lat_string(lat):
        return ('N' if lat >= 0 else 'S') + f'{("%.1f" % abs(lat)).zfill(4)}'.replace('.', '_')  # noqa: UP031

    def lon_string(lon):
        return ('E' if lon >= 0 else 'W') + f'{("%.1f" % abs(lon)).zfill(5)}'.replace('.', '_')  # noqa: UP031

    return '_'.join(
        [
            prefix,
            relative_orbit,
            lon_string(bounds[0]),
            lat_string(bounds[1]),
            lon_string(bounds[2]),
            lat_string(bounds[3]),
            earliest_granule,
            latest_granule,
            token_hex(2).upper(),
        ]
    )


def package_time_series(
    granules: list[str], bounds: list[float], work_dir: Path | None = None, process: str = 'sbas'
) -> Path:
    """Package the time series into a product zip file.

    Args:
        granules: list of the granule names
        bounds: bounding box that was used to generate the GSLCs
        work_dir: Working directory for completed back-projection run
        process: Time series processing ps or sbas

    Returns:
        Path to the created zip file
    """
    if work_dir is None:
        work_dir = Path.cwd()
    ps_sbas_dir = work_dir / process
    product_name = create_time_series_product_name(granules, bounds, process)
    product_path = work_dir / product_name
    product_path.mkdir(exist_ok=True, parents=True)
    zip_path = work_dir / f'{product_name}.zip'
    utils.plot_displacement(ps_sbas_dir)

    to_keep = [
        # Metadata
        'sbas_list',
        'parameters',
        'ref_locs',
        'dem.rsc',
        # Datasets
        'dem',
        'locs',
        'npts',
        'displacement',
        'stackmht',
        'stacktime',
        'velocity',
        # png
        'displacement.png',
    ]
    intermediate_paths = (
        list(ps_sbas_dir.glob('*.int'))
        + list(ps_sbas_dir.glob('*.unw'))
        + list(ps_sbas_dir.glob('*.cc'))
        + list(ps_sbas_dir.glob('*.amp'))
    )
    intermediate = [f.name for f in intermediate_paths]
    to_keep += intermediate
    [shutil.copy(ps_sbas_dir / f, product_path / f) for f in to_keep]
    shutil.make_archive(str(product_path), 'zip', product_path)
    return zip_path


def time_series(
    granules: Iterable[str],
    bounds: list[float],
    use_gslc_prefix: bool,
    bucket: str | None = None,
    bucket_prefix: str = '',
    work_dir: Path | None = None,
    process: str = 'sbas',
    pbaseline: int = 1000,
    tbaseline: int = 60,
) -> None:
    """Create and package a time series stack from a set of Sentinel-1 GSLCs.

    Args:
        granules: List of Sentinel-1 GSLCs
        bounds: bounding box that was used to generate the GSLCs
        use_gslc_prefix: Whether to download input granules from S3
        bucket: AWS S3 bucket for uploading the final product(s)
        bucket_prefix: Add a bucket prefix to the product(s)
        work_dir: Working directory for processing
        process: Time series processing ps or sbas
        pbaseline: Perpendicular baseline limit
        tbaseline: Temporal baseline limit
    """
    if work_dir is None:
        work_dir = Path.cwd()
    ps_sbas_dir = work_dir / process
    if not ps_sbas_dir.exists():
        mkdir(ps_sbas_dir)

    if not (granules or use_gslc_prefix):
        raise ValueError('use_gslc_prefix must be True if granules not provided')

    if use_gslc_prefix:
        if granules:
            raise ValueError('granules must not be provided if use_gslc_prefix is True')
        if not (bucket and bucket_prefix):
            raise ValueError('bucket and bucket_prefix must be given if use_gslc_prefix is True')
        granules = get_gslc_uris_from_s3(bucket, f'{bucket_prefix}/GSLC_granules')

    granule_names = load_products(granules)
    dem_path = dem.download_dem_for_srg(bounds, work_dir)

    utils.create_param_file(dem_path, dem_path.with_suffix('.dem.rsc'), work_dir)
    utils.call_stanford_module('util/merge_slcs.py', work_dir=work_dir)

    create_time_series(work_dir=ps_sbas_dir, baselines=(tbaseline, pbaseline), process=process)

    zip_path = package_time_series(granule_names, bounds, work_dir, process)
    if bucket:
        upload_file_to_s3(zip_path, bucket, bucket_prefix)

    print(f'Finished time-series processing for {", ".join(granule_names)}!')


def main():
    """Entrypoint for the GSLC time series workflow.

    Example command:
    python -m hyp3_srg ++process time_series \
        S1A_IW_RAW__0SDV_20231229T134339_20231229T134411_051870_064437_4F42.geo \
        S1A_IW_RAW__0SDV_20231229T134404_20231229T134436_051870_064437_5F38.geo
    """
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('--bucket', help='AWS S3 bucket HyP3 for upload the final product(s)')
    parser.add_argument('--bucket-prefix', default='', help='Add a bucket prefix to product(s)')
    parser.add_argument(
        '--bounds',
        default=None,
        type=str.split,
        nargs='+',
        help='DEM extent bbox in EPSG:4326: [min_lon, min_lat, max_lon, max_lat].',
    )
    parser.add_argument(
        '--use-gslc-prefix',
        action='store_true',
        help=(
            'Download GSLC input granules from a subprefix located within the bucket and prefix given by the'
            ' --bucket and --bucket-prefix options'
        ),
    )
    parser.add_argument('--process', help='SBAS or PS processing')
    parser.add_argument('--pbaseline', default=1000, type=int, help='Perpendicular baseline')
    parser.add_argument('--tbaseline', default=60, type=int, help='Temporal baseline')
    parser.add_argument('granules', type=str.split, nargs='*', default='', help='GSLC granules.')
    args = parser.parse_args()

    args.granules = [item for sublist in args.granules for item in sublist]

    if args.bounds is not None:
        args.bounds = [float(item) for sublist in args.bounds for item in sublist]
        if len(args.bounds) != 4:
            parser.error('Bounds must have exactly 4 values: [min lon, min lat, max lon, max lat] in EPSG:4326.')

    time_series(**args.__dict__)


if __name__ == '__main__':
    main()
