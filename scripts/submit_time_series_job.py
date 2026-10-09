import argparse

import asf_search
import hyp3_sdk


def wkt_to_bbox(wkt: str) -> tuple[float, float, float, float]:
    """Extract bounding box from WKT POLYGON string.
    Returns: (min_lon, min_lat, max_lon, max_lat)
    """
    if not wkt.startswith('POLYGON((') or not wkt.endswith('))'):
        raise ValueError('WKT must be a POLYGON')

    # Extract coordinate pairs
    points_str = wkt[9:-2]  # Strip 'POLYGON((' and '))'
    coords = [tuple(map(float, point.strip().split())) for point in points_str.split(',')]

    lons, lats = zip(*coords)
    return min(lons), min(lats), max(lons), max(lats)


def bbox_to_wkt(min_lon: float, min_lat: float, max_lon: float, max_lat: float) -> str:
    return f'POLYGON(({min_lon} {min_lat},{max_lon} {min_lat},{max_lon} {max_lat},{min_lon} {max_lat},{min_lon} {min_lat}))'


def get_granules(
    path: int, start: str, end: str, min_lon: float, min_lat: float, max_lon: float, max_lat: float
) -> list[str]:
    granules: list[str] = []
    for polarization in (asf_search.POLARIZATION.VV, asf_search.POLARIZATION.VV_VH):
        results = asf_search.geo_search(
            platform=asf_search.PLATFORM.SENTINEL1,
            processingLevel=asf_search.PRODUCT_TYPE.RAW,
            beamMode=asf_search.BEAMMODE.IW,
            polarization=polarization,
            relativeOrbit=path,
            start=start,
            end=end,
            intersectsWith=bbox_to_wkt(min_lon, min_lat, max_lon, max_lat),
        )
        granules.extend(result.properties['sceneName'] for result in results)
    return granules


def submit_job(
    granules: list[str],
    min_lon: float,
    min_lat: float,
    max_lon: float,
    max_lat: float,
    hyp3_url: str,
    process: str,
    tbaseline: int,
    pbaseline: int,
    name: str | None = None,
    bucket: str | None = None,
    bucket_prefix: str | None = None,
) -> hyp3_sdk.Job:
    hyp3 = hyp3_sdk.HyP3(hyp3_url)
    prepared_job = {
        'job_type': 'SRG_TIME_SERIES',
        'job_parameters': {
            'granules': granules,
            'bounds': [min_lon, min_lat, max_lon, max_lat],
            'process': process,
            'tbaseline': tbaseline,
            'pbaseline': pbaseline,
        },
    }
    if name is not None:
        prepared_job['name'] = name
    if bucket is not None:
        prepared_job['bucket'] = bucket
        if bucket_prefix is not None:
            prepared_job['bucket_prefix'] = bucket_prefix
    return hyp3.submit_prepared_jobs(prepared_job)[0]


def get_args():
    parser = argparse.ArgumentParser(
        description='Submit time series job to HyP3',
        epilog="""
  Examples:
  # Using bounding box:
  python submit_time_series_job.py 64 2023-01-01 2023-02-01 --bbox -120.0 35.0 -119.0 36.0 --name "KernCounty-Asc050"
  
  # Using WKT polygon:
  python submit_time_series_job.py 64 2023-01-01 2023-02-01 --wkt "POLYGON((-120.0 35.0, -119.0 35.0, -119.0 36.0, -120.0 36.0, -120.0 35.0))"
        """,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )

    # Required positional arguments
    parser.add_argument('path', type=int, help='Path/track number, 1-175')
    parser.add_argument('start', help='Start of acquisition window, YYYY-MM-DD')
    parser.add_argument('end', help='End of acquisition window, YYYY-MM-DD')

    # Mutually exclusive spatial definition
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument(
        '--bbox',
        nargs=4,
        type=float,
        metavar=('MIN_LON', 'MIN_LAT', 'MAX_LON', 'MAX_LAT'),
        help='Bounding box as four floats: min_lon min_lat max_lon max_lat',
    )
    group.add_argument('--wkt', help='WKT string for the area of interest')

    # Optional arguments
    parser.add_argument('--name', help='Name for the job')
    parser.add_argument('--bucket', help='Bucket Name')
    parser.add_argument(
        '--bucket-prefix',
        help="Bucket prefix job products will be written to. The argument will be ignored if `--bucket` isn't provided. This argument also allows expansion of job name or job id by using `{name}` and `{job_id}` within the string.",
    )
    parser.add_argument('--process', choices=['sbas', 'ps'], default='sbas', help='Processing strategy (sbas or ps)')
    parser.add_argument('--tbaseline', type=int, default=60, help='Temporal baseline')
    parser.add_argument('--pbaseline', type=int, default=1000, help='Perpendicular baseline')
    parser.add_argument(
        '--hyp3-deployment',
        choices=['hyp3-lavas', 'hyp3-lavas-test'],
        default='hyp3-lavas',
        help='Name of the HyP3 deployment to submit to',
    )

    return parser.parse_args()


def main():
    args = get_args()

    if args.wkt:
        print(f'Parsing WKT: {args.wkt[:50]}...')
        args.min_lon, args.min_lat, args.max_lon, args.max_lat = wkt_to_bbox(args.wkt)
    else:
        args.min_lon, args.min_lat, args.max_lon, args.max_lat = args.bbox

    print('Processing configuration:')
    print(f'  Path: {args.path}')
    print(f'  Date range: {args.start} to {args.end}')
    print(f'  Bounding box: ({args.min_lon:.4f}, {args.min_lat:.4f}) to ({args.max_lon:.4f}, {args.max_lat:.4f})')
    print(f'  Deployment: {args.hyp3_deployment}')
    if args.name:
        print(f'  Job name: {args.name}')

    granule_names = get_granules(
        args.path, args.start, args.end, args.min_lon, args.min_lat, args.max_lon, args.max_lat
    )

    if len(granule_names) == 0:
        raise ValueError('No granules found for these search criteria')

    hyp3_url = f'https://{args.hyp3_deployment}.asf.alaska.edu'
    job = submit_job(
        granule_names,
        args.min_lon,
        args.min_lat,
        args.max_lon,
        args.max_lat,
        hyp3_url,
        args.process,
        args.tbaseline,
        args.pbaseline,
        args.name,
        args.bucket,
        args.bucket_prefix,
    )

    print(
        f'\nJob {job.job_id}, Name: {job.name}, submitted to {args.hyp3_deployment} for {len(granule_names)} granules'
    )

    print(f'\n{hyp3_url}/jobs/{job.job_id}')
    print('\nimport hyp3_sdk')
    print(f"hyp3 = hyp3_sdk.HyP3('{hyp3_url}')")
    print(f"job = hyp3.get_job_by_id('{job.job_id}')")


if __name__ == '__main__':
    main()
