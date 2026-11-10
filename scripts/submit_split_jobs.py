import argparse

import boto3
import botocore
import hyp3_sdk
from submit_time_series_job import get_granules, wkt_to_bbox


def submit_gslcs(
    granules: list[str],
    min_lon: float,
    min_lat: float,
    max_lon: float,
    max_lat: float,
    hyp3_url: str,
    name: str,
    bucket: str,
    bucket_prefix: str,
) -> hyp3_sdk.Job:
    hyp3 = hyp3_sdk.HyP3(hyp3_url)
    batches = int(len(granules) / 100) + 1
    sub_jobs = []
    for batch in range(batches):
        ini = batch * 100
        if batch == batches - 1:
            fin = batch * 100 + len(granules) % 100
        else:
            fin = (batch + 1) * 100
        jobs = []
        for granule in granules[ini:fin]:
            prepared_job = {
                'job_type': 'SRG_GSLC',
                'job_parameters': {
                    'granules': [granule],
                    'bounds': [min_lon, min_lat, max_lon, max_lat],
                },
            }
            if name is not None:
                prepared_job['name'] = name
            if bucket is not None:
                prepared_job['bucket'] = bucket
                if bucket_prefix is not None:
                    prepared_job['bucket_prefix'] = bucket_prefix
            jobs.append(prepared_job)
        if len(jobs) > 0:
            sub_jobs += hyp3.submit_prepared_jobs(jobs)
    print(f'{len(sub_jobs)} jobs have been submitted for {name}')
    return sub_jobs


def submit_ts(
    min_lons: list[float],
    min_lats: list[float],
    max_lons: list[float],
    max_lats: list[float],
    hyp3_url: str,
    processes: list[str],
    tbaselines: list[int],
    pbaselines: list[int],
    names: list[str],
    ignore: list[str],
    bucket: str,
    bucket_prefixes: list[str],
) -> hyp3_sdk.Job:
    hyp3 = hyp3_sdk.HyP3(hyp3_url)
    batches = int(len(names) / 100) + 1
    sub_jobs = []
    for batch in range(batches):
        ini = batch * 100
        if batch == batches - 1:
            fin = batch * 100 + len(names) % 100
        else:
            fin = (batch + 1) * 100
        jobs = []
        for i in range(ini, fin):
            if names[i] in ignore:
                continue
            prepared_job = {
                'job_type': 'SRG_TS',
                'job_parameters': {
                    'bounds': [min_lons[i], min_lats[i], max_lons[i], max_lats[i]],
                    'process': processes[i],
                    'tbaseline': tbaselines[i],
                    'pbaseline': pbaselines[i],
                },
            }
            if names[i] is not None:
                prepared_job['name'] = names[i]
            if bucket is not None:
                prepared_job['bucket'] = bucket
                if bucket_prefixes[i] is not None:
                    prepared_job['bucket_prefix'] = bucket_prefixes[i]
            jobs.append(prepared_job)
        sub_jobs += hyp3.submit_prepared_jobs(jobs)
    print(f'{len(sub_jobs)} jobs have been submitted')
    return sub_jobs


def get_args():
    parser = argparse.ArgumentParser(
        description='Submit time series job to HyP3',
        epilog="""
  Examples:
  # Run GSLC jobs and then time series job
  python submit_split_jobs.py --file jobs.csv --hyp3-deployment hyp3-lavas
  # Just running GSLC jobs
  python submit_split_jobs.py --file jobs.csv --hyp3-deployment hyp3-lavas --just-gslc
  # Just running time series jobs
  python submit_split_jobs.py --file jobs.csv --hyp3-deployment hyp3-lavas --just-ts
        """,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )

    # Required positional arguments
    parser.add_argument('--file', type=str, help='File with the job specifications')
    parser.add_argument(
        '--just-gslc',
        action='store_true',
        help=('If true only the GSLC jobs will be submitted'),
    )
    parser.add_argument(
        '--just-ts',
        action='store_true',
        help=('If true only the TS job will be submitted'),
    )
    parser.add_argument(
        '--hyp3-deployment',
        choices=['hyp3-lavas', 'hyp3-lavas-test'],
        default='hyp3-lavas',
        help='Name of the HyP3 deployment to submit to',
    )

    return parser.parse_args()


def main():
    args = get_args()

    jobs_file = open(args.file)
    jobs = [job.replace('\n', '') for job in jobs_file.readlines() if '#' not in job]
    jobs_file.close()

    hyp3_url = f'https://{args.hyp3_deployment}.asf.alaska.edu'
    bucket = 'lavas-data'
    hyp3 = hyp3_sdk.HyP3(hyp3_url)

    names, tbaselines, pbaselines, processes, bucket_prefixes = [], [], [], [], []
    min_lons, min_lats, max_lons, max_lats = [], [], [], []

    jobs_gslcs = []
    for job in jobs:
        spath, start, end, process, tbaseline, pbaseline, aoi, name = job.split(';')
        path = int(spath)
        if 'POLYGON' in aoi:
            min_lon, min_lat, max_lon, max_lat = wkt_to_bbox(aoi)
        else:
            min_lon, min_lat, max_lon, max_lat = (float(coord) for coord in aoi.split())
        granules = get_granules(path, start, end, min_lon, min_lat, max_lon, max_lat)

        if args.just_gslc or not args.just_ts:
            bucket_prefix = f'{name}_{path}/GSLC_granules'

            jobs_gslcs += submit_gslcs(
                granules,
                min_lon,
                min_lat,
                max_lon,
                max_lat,
                hyp3_url,
                f'{name}_{path}',
                bucket,
                bucket_prefix,
            )
        names.append(f'{name}_{path}')
        min_lons.append(min_lon)
        min_lats.append(min_lat)
        max_lons.append(max_lon)
        max_lats.append(max_lat)
        tbaselines.append(int(tbaseline))
        pbaselines.append(int(pbaseline))
        processes.append(process)
        bucket_prefixes.append(f'{name}_{path}')

    if not args.just_gslc and not args.just_ts:
        print('Please wait for the gslc jobs')
        hyp3.watch(hyp3_sdk.jobs.Batch(jobs_gslcs))

    if args.just_ts or not args.just_gslc:
        ignore = []
        for name in names:
            pending_jobs = hyp3.find_jobs(job_type='SRG_GSLC', name=name, status_code='PENDING').jobs
            pending_jobs += hyp3.find_jobs(job_type='SRG_GSLC', name=name, status_code='RUNNING').jobs
            cont = 'y'
            s3 = boto3.resource('s3', config=boto3.session.Config(signature_version=botocore.UNSIGNED))
            buck = s3.Bucket('lavas-data')
            objs = buck.objects.filter(Prefix=f'{name}/GSLC_granules')
            if len(list(objs)) == 0:
                print(f'No GSLCs found for {name}')
                cont = 'n'
            if len(pending_jobs) > 0:
                cont = input(f'There are currently {len(pending_jobs)} for {name} do you wish to process it (y/n):')
            if not cont[0].lower() == 'y':
                ignore.append(name)
        jobs_ts = submit_ts(
            min_lons,
            min_lats,
            max_lons,
            max_lats,
            hyp3_url,
            processes,
            tbaselines,
            pbaselines,
            names,
            ignore,
            bucket,
            bucket_prefixes,
        )
        cont = input('Do you want to wait until the time series jobs are done (y/n):')
        if cont[0].lower() == 'y':
            hyp3.watch(hyp3_sdk.jobs.Batch(jobs_ts))


if __name__ == '__main__':
    main()
