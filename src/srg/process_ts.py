"""Sentinel-1 custom time series processing"""

import argparse
import shutil
from collections.abc import Iterable
from os import mkdir
from pathlib import Path

from hyp3lib.aws import upload_file_to_s3
from hyp3lib.fetch import download_file as download_from_http

from srg import dem, process_intfs, time_series, utils


def check_bounds(bound: Path, bounds: list[float]):
    """Check if an interferogram product has the same bounds.

    Args:
        bound: File with the bounds for the interferogram product
        bounds: Input bounds

    Returns:
        check: True if input bounds are the same as product bounds
    """
    if len(bounds) == 0:
        return True
    sbounds = ' '.join([str(b) for b in bounds])
    with bound.open() as b:
        cbounds = b.readline()
        cbounds = cbounds.replace('\n', '')
        ibounds = cbounds.split()
        return ','.join(ibounds) == ','.join(sbounds.split())


def load_intfs(uris: Iterable[str], bounds: list[float], process: str):
    """Load the products from the provided URIs

    Args:
        uris: list of URIs to the SRG GSLC products
    """
    work_dir = Path.cwd()
    process_dir = work_dir / process
    intfs = []
    for i, uri in enumerate(uris):
        name = Path(Path(uri).name)

        if uri.startswith('s3'):
            utils.download_from_s3(uri, dest_dir=work_dir)
        elif uri.startswith('http'):
            download_from_http(uri, directory=work_dir)
        elif len(Path(uri).parts) > 1:
            shutil.copy(uri, work_dir)

        shutil.unpack_archive(name.with_suffix('.zip'), work_dir / f'tmp_{i}')
        if not check_bounds(work_dir / f'tmp_{i}/bounds', bounds):
            continue
        shutil.unpack_archive(name.with_suffix('.zip'), work_dir)
        shutil.move(work_dir / 'intfs/sbas_list', work_dir / f'intfs/sbas_list_{i}')
        shutil.move(work_dir / 'intfs/geolist', work_dir / f'intfs/geolist_{i}')
        shutil.move(work_dir / 'intfs/intlist', work_dir / f'intfs/intlist_{i}')
        shutil.move(work_dir / 'bounds', work_dir / f'bounds_{i}')

        intfs.append(str(name))

    tmp_dir = work_dir / 'intfs'
    if tmp_dir.is_dir():
        shutil.copytree(tmp_dir, process_dir, dirs_exist_ok=True)

    return intfs


def get_granules_pairs_from_sbas(sbas_list: list[str]):
    """Get interferometric pairs from sbas list.

    Args:
        sbas_list: List with sbas list lines

    Returns:
        granules: Granules in the sbas list
        intfs: List of strings with pair dates
    """
    granules = []
    intfs = []
    for intf in sbas_list:
        ref, sec, tb, pb = intf.split(',')
        granules.append(ref.split('/')[-1])
        granules.append(sec.split('/')[-1])
        date_ref = ref.split('_')[5].split('T')[0]
        date_sec = sec.split('_')[5].split('T')[0]
        sdates = f'{date_ref}_{date_sec}'
        intfs.append(sdates)
    return granules, intfs


def filter_intfs(work_dir: Path, intfs: list[str]):
    """Filter out interferograms using input list.

    Args:
        work_dir: Directory with interferogram products
        intfs: Wanted interferograms
    """
    cintfs = list(work_dir.glob('*.int'))
    amps = list(work_dir.glob('*.amp'))
    ccs = list(work_dir.glob('*.cc'))
    files = cintfs + amps + ccs
    for f in files:
        name = f.name.split('.')[0]
        if name not in intfs:
            f.unlink()


def missing_sbas(work_dir: Path, sbas_list: list[str]) -> list[str]:
    """Find missing interferograms from sbas list

    Args:
        work_dir: Directory with interferogram products
        sbas_list: List of string with lines from sbas list

    Returns:
        missing_list: SBAS list with missing pairs
    """
    granules, intfs = get_granules_pairs_from_sbas(sbas_list)
    cintfs = [cintf.name.split('.int')[0] for cintf in work_dir.glob('*.int')]
    missing_lines = []
    for i, ifs in enumerate(intfs):
        if ifs not in cintfs:
            items = sbas_list[i].split(',')
            items[0] = '../' + items[0]
            items[1] = '../' + items[1]
            sbas_list[i] = ','.join(items)
            missing_lines.append(sbas_list[i])
    return missing_lines


def create_sbas_list(tbaseline: int, pbaseline: int, bounds: list[float], work_dir: Path, process: str) -> list[str]:
    """Create sbas list from temporal and perpendicular baselines

    Args:
        tbaseline: Temporal basline limit
        pbaseline: Perpendicular baseline limit
        work_dir: Directory with the granules
        process: Time series processing ps or sbas

    Returns:
        sbas_list: SBAS list with pairs that satify the baselines
    """
    process_dir = work_dir / process
    dem_path = dem.download_dem_for_srg(bounds, work_dir)

    utils.create_param_file(dem_path, dem_path.with_suffix('.dem.rsc'), work_dir)
    utils.call_stanford_module('util/merge_slcs.py', work_dir=work_dir)
    utils.call_stanford_module('sentinel/sbas_list.py', args=[tbaseline, pbaseline], work_dir=process_dir)
    sbasf = process_dir / 'sbas_list'
    with sbasf.open() as f:
        lines = f.readlines()
        sbas_list = [','.join(line.split()) for line in lines]
    return sbas_list


def process_missing_intfs(missing: list[str], work_dir: Path, process: str, bounds: list[float]) -> None:
    """Process missing interferograms

    Args:
        missing: SBAS list with missing interferograms
        work_dir: Directory with the granules
        process: Time series processing ps or sbas
        bounds: bounding box that was used to generate the GSLCs
    """
    process_dir = work_dir / process
    if len(missing) > 0:
        cgranules, cintfs = get_granules_pairs_from_sbas(missing)
        missing_dir = work_dir / 'missing'
        mkdir(missing_dir)
        missing_intfs = missing_dir / 'intfs'
        granules_add = sorted({granule.replace('.geo', '.zip') for granule in cgranules})
        process_intfs.process_intfs(
            granules_add, bounds, use_gslc_prefix=False, work_dir=missing_dir, sbas_list=missing
        )
        missing_files = (
            list(missing_intfs.glob('*.int')) + list(missing_intfs.glob('*.amp')) + list(missing_intfs.glob('*.cc'))
        )
        missing_files += list(missing_intfs.glob('dem.rsc'))
        missing_add = list(missing_dir.glob('elevation.dem*'))
        missing_add += list(missing_dir.glob('bounds*'))
        for f in missing_files:
            if not (process_dir / f.name).exists():
                shutil.move(f, process_dir / f.name)
        for f in missing_add:
            if not (work_dir / f.name).exists():
                shutil.move(f, work_dir / f.name)


def process_time_series(
    granules: list[str],
    sbas_list: list[str],
    process: str,
    bounds: list[float],
    pbaseline: int = 1000,
    tbaseline: int = 60,
    bucket: str | None = None,
    bucket_prefix: str = '',
    use_gslc_prefix: bool = False,
    work_dir: Path | None = None,
    overwrite: bool = False,
) -> None:
    """Create and package a time series stack from a set of Sentinel-1 GSLCs.

    Args:
        granules: List of Sentinel-1 GSLCs
        sbas_list: List of strings from sbas list
        bounds: bounding box that was used to generate the GSLCs
        use_gslc_prefix: Whether to download input granules from S3
        bucket: AWS S3 bucket for uploading the final product(s)
        bucket_prefix: Add a bucket prefix to the product(s)
        work_dir: Working directory for processing
        process: Time series processing ps or sbas
        pbaseline: Perpendicular baseline limit
        tbaseline: Temporal baseline limit
        overwrite: Compute the interferograms again
    """
    if not use_gslc_prefix:
        if len(sbas_list) == 0 and len(granules) == 0:
            raise ValueError('You should give an sbas list or granules')
        elif len(sbas_list) > 0 and len(granules) > 0:
            raise ValueError('SBAS list and granules provided, please provide just one')

    if work_dir is None:
        work_dir = Path.cwd()
    process_dir = work_dir / process
    mkdir(process_dir)

    if use_gslc_prefix:
        if granules:
            raise ValueError('granules must not be provided if use_gslc_prefix is True')
        if not (bucket and bucket_prefix):
            raise ValueError('bucket and bucket_prefix must be given if use_gslc_prefix is True')
        granules = time_series.get_gslc_uris_from_s3(bucket, f'{bucket_prefix}/GSLC_granules')
        try:
            intfs = time_series.get_gslc_uris_from_s3(bucket, f'{bucket_prefix}/INTF_products')
        except KeyError:
            intfs = []
    else:
        cgranules, cintfs = get_granules_pairs_from_sbas(sbas_list)
        if len(granules) == 0:
            granules = [g.replace('.geo', '.zip') for g in cgranules]
        data_dir = Path(granules[0]).parent
        intfs = [str(intf) for intf in data_dir.glob('*INTF*')]

    granule_names = time_series.load_products(granules)

    if len(sbas_list) == 0:
        sbas_list = create_sbas_list(tbaseline, pbaseline, bounds, work_dir, process)

    if overwrite:
        intfs = []
    else:
        intfs = load_intfs(intfs, bounds, process)

    missing = missing_sbas(process_dir, sbas_list)
    process_missing_intfs(missing, work_dir, process, bounds)

    cgranules, cintfs = get_granules_pairs_from_sbas(sbas_list)

    filter_intfs(process_dir, cintfs)
    sbasf = process_dir / 'sbas_list'
    pairs = [pair.replace(',', ' ') for pair in sbas_list]
    sbasf.write_text('\n'.join(pairs))

    intf = process_dir / 'intlist'
    pairs = [str(f.name) for f in process_dir.glob('*.int')]
    intf.write_text('\n'.join(pairs))

    geo_list = process_dir / 'geolist'
    sort_granules = sorted({g for g in cgranules})
    sort_granules = [f'../{g}' for g in sort_granules]
    geo_list.write_text('\n'.join(sort_granules))

    dem_shape = time_series.get_size_from_dem(str(work_dir / 'elevation.dem.rsc'))

    unw_shape = time_series.get_size_from_dem(str(process_dir / 'dem.rsc'))
    unw_width, unw_length = unw_shape

    if process == 'ps':
        utils.call_stanford_module('ps/psfilter.py', args=['intlist', 'intlist', unw_width], work_dir=process_dir)
        [shutil.move(f'{intf}.interp', str(intf)) for intf in list(process_dir.glob('*.int'))]

    time_series.unwrap_interferograms(dem_shape=dem_shape, unw_shape=unw_shape, work_dir=process_dir)
    threshold: float = 0.5
    do_tropo_correction: bool = True

    time_series.compute_sbas_velocity_solution(
        threshold=threshold,
        do_tropo_correction=do_tropo_correction,
        unw_shape=unw_shape,
        work_dir=process_dir,
    )

    zip_path = time_series.package_time_series(granule_names, bounds, work_dir, process)
    if bucket:
        upload_file_to_s3(zip_path, bucket, bucket_prefix)


def main():
    """Entrypoint for the GSLC time series workflow.

    Example command:
    python -m hyp3_srg ++process process_ts \
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
    parser.add_argument('--sbas-list', type=str.split, nargs='*', default='', help='Custom pairs')
    parser.add_argument('--pbaseline', default=1000, type=int, help='Perpendicular baseline')
    parser.add_argument('--tbaseline', default=60, type=int, help='Temporal baseline')
    parser.add_argument('granules', type=str.split, nargs='*', default='', help='GSLC granules.')
    args = parser.parse_args()

    args.granules = [item for sublist in args.granules for item in sublist]
    args.sbas_list = [item for sublist in args.sbas_list for item in sublist]

    if args.bounds is not None:
        args.bounds = [float(item) for sublist in args.bounds for item in sublist]
        if len(args.bounds) != 4:
            parser.error('Bounds must have exactly 4 values: [min lon, min lat, max lon, max lat] in EPSG:4326.')

    process_time_series(**args.__dict__)
