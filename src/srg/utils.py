import logging
import netrc
import os
import subprocess
from pathlib import Path
from platform import system
from zipfile import ZipFile

import asf_search
import matplotlib.pyplot as plt
import numpy
from boto3 import client
from s1_orbits import fetch_for_scene
from shapely.geometry import shape
from shapely.geometry.base import BaseGeometry


S3 = client('s3')
log = logging.getLogger(__name__)
EARTHDATA_HOST = 'urs.earthdata.nasa.gov'


def get_proc_home() -> Path:
    """Get the PROC_HOME environment variable, which is the location of the SRG modules.

    Returns:
        Path to the PROC_HOME directory
    """
    proc_home = os.environ.get('PROC_HOME', None)
    if proc_home is None:
        raise ValueError('PROC_HOME environment variable is not set. Location of Stanford modules is unknown.')
    return Path(proc_home)


def get_netrc() -> Path:
    """Get the location of the netrc file.

    Returns:
        Path to the netrc file
    """
    netrc_name = '_netrc' if system().lower() == 'windows' else '.netrc'
    netrc_file = Path.home() / netrc_name
    return netrc_file


def set_creds(service, username, password) -> None:
    """Set username/password environmental variables for a service.
    username/password are set using the following format:
    SERVICE_USERNAME, SERVICE_PASSWORD

    Args:
        service: Service to set credentials for
        username: Username for the service
        password: Password for the service
    """
    if username is not None:
        os.environ[f'{service.upper()}_USERNAME'] = username

    if password is not None:
        os.environ[f'{service.upper()}_PASSWORD'] = password


def find_creds_in_env(username_name, password_name) -> tuple[str | None, str | None]:
    """Find credentials for a service in the environment.

    Args:
        username_name: Name of the environment variable for the username
        password_name: Name of the environment variable for the password

    Returns:
        Tuple of the username and password found in the environment
    """
    if username_name in os.environ and password_name in os.environ:
        username = os.environ[username_name]
        password = os.environ[password_name]
        return username, password

    return None, None


def find_creds_in_netrc(service) -> tuple[str | None, str | None]:
    """Find credentials for a service in the netrc file.

    Args:
        service: Service to find credentials for

    Returns:
        Tuple of the username and password found in the netrc file
    """
    netrc_file = get_netrc()
    if netrc_file.exists():
        netrc_credentials = netrc.netrc(netrc_file)
        if service in netrc_credentials.hosts:
            username = netrc_credentials.hosts[service][0]
            password = netrc_credentials.hosts[service][2]
            return username, password

    return None, None


def get_earthdata_credentials() -> tuple[str, str]:
    """Get NASA EarthData credentials from the environment or netrc file.

    Returns:
        Tuple of the NASA EarthData username and password
    """
    username, password = find_creds_in_env('EARTHDATA_USERNAME', 'EARTHDATA_PASSWORD')
    if username and password:
        return username, password

    username, password = find_creds_in_netrc(EARTHDATA_HOST)
    if username and password:
        return username, password

    raise ValueError(
        'Please provide NASA EarthData credentials via the '
        'EARTHDATA_USERNAME and EARTHDATA_PASSWORD environment variables, or your netrc file.'
    )


def download_raw_granule(granule_name: str, output_dir: Path, unzip: bool = False) -> tuple[Path, BaseGeometry]:
    """Download a S1 granule using asf_search. Return its path
    and buffered extent.

    Args:
        granule_name: Name of the granule to download
        output_dir: Directory to save the granule in
        unzip: Unzip the granule if it is a zip file

    Returns:
        Tuple of the granule path and its extent as a Polygon
    """
    username, password = get_earthdata_credentials()
    session = asf_search.ASFSession().auth_with_creds(username, password)
    if not granule_name.endswith('-RAW'):
        granule_name += '-RAW'

    result = asf_search.granule_search([granule_name])[0]
    bbox = shape(result.geojson()['geometry'])

    zip_path = output_dir / f'{granule_name[:-4]}.zip'
    if not unzip:
        out_path = zip_path
        if not out_path.exists():
            result.download(path=output_dir, session=session)
    else:
        out_path = output_dir / f'{granule_name[:-4]}.SAFE'
        if not out_path.exists() and not zip_path.exists():
            result.download(path=output_dir, session=session)

        if not out_path.exists():
            with ZipFile(zip_path, 'r') as zip_ref:
                zip_ref.extractall('.')

        if zip_path.exists() and unzip:
            zip_path.unlink()

    return out_path, bbox


def download_orbit(granule_name: str, output_dir: Path) -> Path:
    """Download a S1 orbit file. Prefer using the ESA API,
    but fallback to ASF if needed.

    Args:
        granule_name: Name of the granule to download
        output_dir: Directory to save the orbit file in

    Returns:
        Path to the downloaded orbit file
    """
    orbit_path = fetch_for_scene(granule_name, dir=output_dir)
    return orbit_path


def create_param_file(dem_path: Path, dem_rsc_path: Path, output_dir: Path):
    """Create a parameter file for the processor.

    Args:
        dem_path: Path to the DEM file
        dem_rsc_path: Path to the DEM RSC file
        output_dir: Directory to save the parameter file in
    """
    lines = [str(dem_path), str(dem_rsc_path)]
    with open(output_dir / 'params', 'w') as f:
        f.write('\n'.join(lines))


def call_stanford_module(local_name, args: list = [], work_dir: Path | None = None) -> None:
    """Call a Stanford Processor modules (via subprocess) with the given arguments.

    Args:
        local_name: Name of the module to call (e.g. 'sentinel/sentinel_scene_cpu.py')
        work_dir: Directory to run the module in
        args: List of arguments to pass to the module
    """
    if work_dir is None:
        work_dir = Path.cwd()

    proc_home = get_proc_home()
    script = proc_home / local_name
    args = [str(x) for x in args]
    print(f'Calling {local_name} {" ".join(args)} in directory {work_dir}')
    subprocess.run([script, *args], cwd=work_dir, check=True)


def how_many_gpus():
    """Get the number of GPUs available on the system using Stanford script."""
    cmd = (get_proc_home() / 'sentinel' / 'howmanygpus').resolve()
    proc = subprocess.Popen(str(cmd), stdout=subprocess.PIPE, shell=True)
    (param, err) = proc.communicate()
    ngpus = int(str(param, 'UTF-8').split()[0])
    return ngpus


def plot_displacement(process_dir: Path) -> None:
    """Create a png image for the displacement product.

    Args:
        process_dir: Folder with the displacement product
    """
    infile = process_dir / 'displacement'
    # define our color tables
    upramp = numpy.linspace(100, 255, 120) / 255
    dnramp = numpy.linspace(255, 100, 120) / 255
    oneramp = numpy.ones(120)
    r = numpy.zeros(360)
    g = numpy.zeros(360)
    b = numpy.zeros(360)
    r[0:120] = upramp
    r[120:240] = oneramp
    r[240:360] = dnramp
    g[0:120] = dnramp
    g[120:240] = upramp
    g[240:360] = oneramp
    b[0:120] = oneramp
    b[120:240] = dnramp
    b[240:360] = upramp
    contour = 100.0
    # get params from rsc file
    fe = open(process_dir / 'dem.rsc')
    words = fe.readline()
    unwwidth = int(words.split()[1])
    words = fe.readline()
    unwlength = int(words.split()[1])
    fe.close()

    nsteps = int(os.path.getsize(infile) / unwwidth / 8 / unwlength)
    nlines = int(os.path.getsize(infile) / unwwidth / 8 / nsteps)
    scale = 1
    exponent = 0.3

    # initialize a zero filled array for display
    rgb = numpy.zeros((nlines, unwwidth, 3))

    # load data
    indata = numpy.fromfile(infile, dtype=numpy.single)
    if int(indata.shape[0] / unwwidth / 2) != 0:
        image = indata[(nsteps * nlines - nlines) * unwwidth * 2 : (nsteps * nlines) * unwwidth * 2]
        image = numpy.reshape(image, (-1, unwwidth * 2))
    # convert to mag/hgt
    mag = numpy.power(numpy.absolute(image[:, 0:unwwidth]), exponent)
    hgt = numpy.mod(image[:, unwwidth : 2 * unwwidth], contour)
    # create an integer array for mapping into rgb
    icolor = numpy.zeros(image.shape, dtype=numpy.int32)

    # scale for amplitude
    ampsum = sum(mag)
    ampi = nlines * unwwidth
    ampi = int(numpy.count_nonzero(mag))
    scalemag = (scale * 150 / (ampsum / ampi)) / 256
    print('scale factor ', scalemag)
    # potential overflows in amplitude
    mag = numpy.clip(mag * scalemag, 0, 1)

    # hgt in contour units
    numpy.rint(hgt / contour * 360, out=hgt)
    ihgt = hgt.astype(int)
    icolor = ihgt.clip(max=359)

    plt.figure(1, figsize=(10, 10))
    # cid = fig.canvas.mpl_connect('button_press_event', mouse_event_mht)

    # load color table values for image
    rgb[:, :, 0] = r[icolor[:, :]] * mag
    rgb[:, :, 1] = g[icolor[:, :]] * mag
    rgb[:, :, 2] = b[icolor[:, :]] * mag

    plt.imshow(rgb)
    plt.savefig(process_dir / 'displacement.png')


def get_s3_args(uri: str, dest_dir: Path | None = None) -> tuple[str, str, Path]:
    """Retrieve the arguments for downloading from an S3 bucket

    Args:
        uri: URI of the file to download
        dest_dir: the directory to place the downloaded file in

    Returns:
        bucket: the s3 bucket to download from
        key: the path to the file following the s3 bucket
        out_path: the destination path of the file to download
    """
    if dest_dir is None:
        dest_dir = Path.cwd()

    simple_s3_uri = Path(uri.replace('s3://', ''))
    bucket = simple_s3_uri.parts[0]
    key = '/'.join(simple_s3_uri.parts[1:])
    out_path = dest_dir / simple_s3_uri.parts[-1]
    return bucket, key, out_path


def s3_list_objects(bucket: str, prefix: str = '') -> dict:
    """List objects in bucket at prefix

    Args:
        bucket: the simple s3 bucket name
        prefix: the path within the bucket to search

    Returns:
        res: dictionary containing the response
    """
    bucket = bucket.replace('s3:', '').replace('/', '')
    res = S3.list_objects(Bucket=bucket, Prefix=prefix)
    return res


def download_from_s3(uri: str, dest_dir: Path | None = None) -> Path:
    """Download a file from an S3 bucket

    Args:
        uri: URI of the file to download
        dest_dir: the directory to place the downloaded file in
    """
    bucket, key, out_path = get_s3_args(uri, dest_dir)
    S3.download_file(bucket, key, out_path)
    return out_path
