#!/usr/bin/python3

import os # for chdir, remove, getcwd

import gdal
from osgeo import gdal
import numpy as np

# ASF's Jupyter Notebook Module
from asf_notebook import path_exists
from asf_notebook import download_ASF_granule
from asf_notebook import new_directory
from asf_notebook import asf_unzip
from asf_notebook import earthdata_hyp3_login

#path = "/home/jovyan/notebooks/ASF/GEOS_657_Labs/lab_3_data"
#new_directory(path)
#os.chdir(path)

#filename = download_ASF_granule("ALPSRP185270680", 'L1.0')

