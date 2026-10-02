"""Prepare a Copernicus GLO-30 DEM virtual raster (VRT) covering a given geometry"""

import logging
from pathlib import Path

import requests

from hyp3_srg import utils


log = logging.getLogger(__name__)


def ensure_egm_model_available():
    """Ensure the EGM module is available.
    Currently using the EGM2004 model provided by Stanford, but hope to switch to a public source.
    """
    url = 'https://lavas-data.s3.us-west-2.amazonaws.com/egm2008_geoid_grid'

    proc_home = utils.get_proc_home()
    egm_model_path = proc_home / 'DEM' / 'egm2008_geoid_grid'

    with requests.get(url, stream=True) as r:
        r.raise_for_status()
        with open(egm_model_path, 'wb') as f:
            for chunk in r.iter_content(chunk_size=8192):
                f.write(chunk)


def download_dem_for_srg(bounds: list[float], work_dir: Path):
    """Download the DEM for the given bounds - [min_lon, min_lat, max_lon, max_lat].

    Args:
        bounds: The bounds of the extent of the desired DEM - [min_lon, min_lat, max_lon, max_lat].
        work_dir: The directory to save the DEM in

    Returns:
        The path to the downloaded DEM
    """
    if bounds[0] >= bounds[2] or bounds[1] >= bounds[3]:
        raise ValueError(
            'Improper bounding box formatting, should be [max latitude, min latitude, min longitude, max longitude].'
        )

    dem_path = work_dir / 'elevation.dem'
    dem_rsc = work_dir / 'elevation.dem.rsc'

    with open(work_dir / 'bounds', 'w') as bounds_file:
        bounds_file.write(' '.join([str(bound) for bound in bounds]))

    ensure_egm_model_available()

    stanford_bounds = [bounds[i] for i in [3, 1, 0, 2]]
    args = [str(dem_path), str(dem_rsc), *stanford_bounds, '6', '2']
    utils.call_stanford_module('DEM/createDEMcop.py', args, work_dir=work_dir)
    return dem_path
