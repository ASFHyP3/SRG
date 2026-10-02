"""GSLC back-projection processing"""

import argparse
import logging
import os
import zipfile
from collections.abc import Iterable
from pathlib import Path

from hyp3lib.aws import upload_file_to_s3
from shapely import unary_union

from hyp3_srg import dem, utils


log = logging.getLogger(__name__)


def check_required_files(required_files: Iterable, work_dir: Path) -> None:
    for file in required_files:
        if not (work_dir / file).exists():
            raise FileNotFoundError(f'Missing required file: {file}')


def clean_up_after_back_projection(work_dir: Path) -> None:
    patterns = ['*hgt*', 'dem*', 'DEM*', 'q*', '*positionburst*']
    for pattern in patterns:
        for f in work_dir.glob(pattern):
            f.unlink()


def back_project_granules(granule_orbit_pairs: Iterable, work_dir: Path, gpu: bool = False) -> None:
    """Back-project a set of Sentinel-1 level-0 granules using the CPU-based workflow.

    Args:
        granule_orbit_pairs: List of tuples of granule and orbit file paths
        work_dir: Working directory for processing
    """
    check_required_files(['elevation.dem', 'elevation.dem.rsc', 'params'], work_dir)

    if gpu:
        os.environ['CUDA_DEVICE_ORDER'] = 'PCI_BUS_ID'
        os.environ['CUDA_VISIBLE_DEVICES'] = '0'

    cmd = 'sentinel/sentinel_scene_multigpu.py' if gpu else 'sentinel/sentinel_scene_cpu.py'
    for granule_path, orbit_path in granule_orbit_pairs:
        args = [str(granule_path.with_suffix('')), str(orbit_path)]
        utils.call_stanford_module(cmd, args, work_dir=work_dir)

    clean_up_after_back_projection(work_dir)


def create_product(work_dir) -> Path:
    """Create a product zip file.
    Includes files needed for further processing (gslc, orbit, and parameter file).

    Args:
        work_dir: Working directory for completed back-projection run

    Returns:
        Path to the created zip file
    """
    gslc_path = list(work_dir.glob('S1*.geo'))[0]
    product_name = gslc_path.with_suffix('').name
    orbit_path = work_dir / f'{product_name}.orbtiming'
    rsc_path = work_dir / 'elevation.dem.rsc'
    bounds_path = work_dir / 'bounds'
    zip_path = work_dir / f'{product_name}.zip'

    parameter_file = work_dir / f'{product_name}.txt'
    input_granules = [x.with_suffix('').name for x in work_dir.glob('S1*.SAFE')]
    with open(parameter_file, 'w') as f:
        f.write('Process: back-projection\n')
        f.write(f'Input Granules: {", ".join(input_granules)}\n')

    # We don't compress the data because SLC data is psuedo-random
    with zipfile.ZipFile(zip_path, 'w', compression=zipfile.ZIP_STORED) as z:
        z.write(gslc_path, gslc_path.name)
        z.write(orbit_path, orbit_path.name)
        z.write(rsc_path, rsc_path.name)
        z.write(bounds_path, bounds_path.name)
        z.write(parameter_file, parameter_file.name)

    return zip_path


def back_project(
    granules: Iterable[str],
    bounds: list[float] | None = None,
    earthdata_username: str | None = None,
    earthdata_password: str | None = None,
    bucket: str | None = None,
    bucket_prefix: str = '',
    use_gslc_prefix: bool = False,
    work_dir: Path | None = None,
    gpu: bool = False,
):
    """Back-project a set of Sentinel-1 level-0 granules.

    Args:
        granules: List of Sentinel-1 level-0 granules to back-project
        bounds: DEM extent bounding box [min_lon, min_lat, max_lon, max_lat]
        earthdata_username: Username for NASA's EarthData service
        earthdata_password: Password for NASA's EarthData service
        bucket: AWS S3 bucket for uploading the final product(s)
        bucket_prefix: Add a bucket prefix to the product(s)
        use_gslc_prefix: Upload GSLCs to a subprefix
        work_dir: Working directory for processing
        gpu: Use the GPU-based version of the workflow
    """
    if use_gslc_prefix:
        if not (bucket and bucket_prefix):
            raise ValueError('bucket and bucket_prefix must be given if use_gslc_prefix is True')
        bucket_prefix += '/GSLC_granules'

    utils.set_creds('EARTHDATA', earthdata_username, earthdata_password)
    if work_dir is None:
        work_dir = Path.cwd()

    print('Downloading data...')
    bboxs = []
    granule_orbit_pairs = []
    for granule in granules:
        granule_path, granule_bbox = utils.download_raw_granule(granule, work_dir, unzip=True)
        orbit_path = utils.download_orbit(granule, work_dir)
        bboxs.append(granule_bbox)
        granule_orbit_pairs.append((granule_path, orbit_path))

    if bounds is None:
        bounds = list(unary_union(bboxs).buffer(0.1).bounds)

    dem_path = dem.download_dem_for_srg(bounds, work_dir)
    utils.create_param_file(dem_path, dem_path.with_suffix('.dem.rsc'), work_dir)

    back_project_granules(granule_orbit_pairs, work_dir=work_dir, gpu=gpu)

    utils.call_stanford_module('util/merge_slcs.py', work_dir=work_dir)

    zip_path = create_product(work_dir)
    if bucket:
        upload_file_to_s3(zip_path, bucket, bucket_prefix)

    print(f'Finished back-projection for {list(work_dir.glob("S1*.geo"))[0].with_suffix("").name}!')


def main():
    """Back Projection entrypoint.

    Example command:
    python -m hyp3_srg ++process back_projection \
        S1A_IW_RAW__0SDV_20231229T134339_20231229T134411_051870_064437_4F42-RAW \
        S1A_IW_RAW__0SDV_20231229T134404_20231229T134436_051870_064437_5F38-RAW
    """
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('--earthdata-username', default=None, help="Username for NASA's EarthData")
    parser.add_argument('--earthdata-password', default=None, help="Password for NASA's EarthData")
    parser.add_argument('--bucket', help='AWS S3 bucket HyP3 for upload the final product(s)')
    parser.add_argument('--bucket-prefix', default='', help='Add a bucket prefix to product(s)')
    parser.add_argument(
        '--use-gslc-prefix',
        action='store_true',
        help=(
            'Upload GSLC granules to a subprefix located within the bucket and prefix given by the'
            ' --bucket and --bucket-prefix options'
        ),
    )
    parser.add_argument(
        '--gpu',
        default=False,
        action='store_true',
        help='Use the GPU-based version of the workflow.',
    )
    parser.add_argument(
        '--bounds',
        default=None,
        type=str.split,
        nargs='+',
        help='DEM extent bbox in EPSG:4326: [min_lon, min_lat, max_lon, max_lat].',
    )
    parser.add_argument(
        'granules',
        type=str.split,
        nargs='+',
        help='Level-0 S1 granule(s) to back-project.',
    )
    args = parser.parse_args()

    args.granules = [item for sublist in args.granules for item in sublist]

    if args.bounds is not None:
        args.bounds = [float(item) for sublist in args.bounds for item in sublist]
        if len(args.bounds) != 4:
            parser.error('Bounds must have exactly 4 values: [min lon, min lat, max lon, max lat] in EPSG:4326.')

    back_project(**args.__dict__)


if __name__ == '__main__':
    main()
