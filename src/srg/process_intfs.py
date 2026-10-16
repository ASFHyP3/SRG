"""Sentinel-1 interferograms processing"""

import argparse
import shutil
from collections.abc import Iterable
from os import mkdir
from pathlib import Path
from secrets import token_hex

from hyp3lib.aws import upload_file_to_s3

from srg import dem, time_series, utils


def generate_wrapped_interferograms(
    looks: tuple[int, int],
    baselines: tuple[int, int],
    sbas_list: list[str],
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
    if len(sbas_list) > 0:
        sbasf = work_dir / 'sbas_list'
        pairs = [pair.replace(',', ' ') for pair in sbas_list]
        sbasf.write_text('\n'.join(pairs))

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


def create_intfs_product_name(
    granule_names: list[str],
    bounds: list[float],
):
    """Create a product name for the given granules.

    Args:
        granule_names: list of the granule names
        bounds: bounding box that was used to generate the GSLCs
        process: Time series processing ps or sbas

    Returns:
        the product name as a string.
    """
    prefix = 'S1_SRG_INTF'
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


def package_intfs(granules: list[str], bounds: list[float], work_dir: Path | None = None) -> Path:
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
    ps_sbas_dir = work_dir / 'intfs'
    product_name = create_intfs_product_name(granules, bounds)
    product_path = work_dir / product_name
    product_path.mkdir(exist_ok=True, parents=True)
    zip_path = work_dir / f'{product_name}.zip'

    to_keep = [
        # Metadata
        'bounds',
        'elevation.dem',
        'elevation.dem.rsc',
    ]
    shutil.copytree(ps_sbas_dir, product_path / 'intfs')
    [shutil.copy(work_dir / f, product_path / f) for f in to_keep]
    shutil.make_archive(str(product_path), 'zip', product_path)
    return zip_path


def process_intfs(
    granules: Iterable[str],
    bounds: list[float],
    use_gslc_prefix: bool,
    bucket: str | None = None,
    bucket_prefix: str = '',
    work_dir: Path | None = None,
    looks: tuple[int, int] = (6, 2),
    pbaseline: int = 1000,
    tbaseline: int = 60,
    sbas_list: list[str] = [],
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
    ps_sbas_dir = work_dir / 'intfs'
    if not ps_sbas_dir.exists():
        mkdir(ps_sbas_dir)

    if not (granules or use_gslc_prefix):
        raise ValueError('use_gslc_prefix must be True if granules not provided')

    if use_gslc_prefix:
        if granules:
            raise ValueError('granules must not be provided if use_gslc_prefix is True')
        if not (bucket and bucket_prefix):
            raise ValueError('bucket and bucket_prefix must be given if use_gslc_prefix is True')
        granules = time_series.get_gslc_uris_from_s3(bucket, f'{bucket_prefix}/GSLC_granules')
        bucket_prefix += '/INTF_products'

    granule_names = time_series.load_products(granules, work_dir=work_dir)

    dem_path = dem.download_dem_for_srg(bounds, work_dir)

    utils.create_param_file(dem_path, dem_path.with_suffix('.dem.rsc'), work_dir)
    utils.call_stanford_module('util/merge_slcs.py', work_dir=work_dir)

    dem_shape = time_series.get_size_from_dem('elevation.dem.rsc')
    generate_wrapped_interferograms(
        looks=looks, baselines=(tbaseline, pbaseline), sbas_list=sbas_list, dem_shape=dem_shape, work_dir=ps_sbas_dir
    )

    zip_path = package_intfs(granule_names, bounds, work_dir)

    if bucket:
        upload_file_to_s3(zip_path, bucket, bucket_prefix)

    print(f'Finished interferograms processing for {", ".join(granule_names)}!')


def main():
    """Entrypoint for the GSLC time series workflow.

    Example command:
    python -m srg ++process generate_intfs \
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
    parser.add_argument('--pbaseline', default=1000, type=int, help='Perpendicular baseline')
    parser.add_argument('--tbaseline', default=60, type=int, help='Temporal baseline')
    parser.add_argument('--sbas-list', type=str.split, nargs='*', default='', help='Custom pairs')
    parser.add_argument('granules', type=str.split, nargs='*', default='', help='GSLC granules.')
    args = parser.parse_args()

    args.granules = [item for sublist in args.granules for item in sublist]
    args.sbas_list = [item for sublist in args.sbas_list for item in sublist]

    if args.bounds is not None:
        args.bounds = [float(item) for sublist in args.bounds for item in sublist]
        if len(args.bounds) != 4:
            parser.error('Bounds must have exactly 4 values: [min lon, min lat, max lon, max lat] in EPSG:4326.')

    process_intfs(**args.__dict__)


if __name__ == '__main__':
    main()
