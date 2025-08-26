#!/usr/bin/env python3
#
#  download precise orbit files for zipfiles in directory
#

import sys
import os
import string
import time
import subprocess
from datetime import datetime

if len(sys.argv) < 1:
    print ('Usage: sentinel_orbitfiles_kp.py <update_flag="N">')

update_flag = 'N'
if len(sys.argv) > 1:
    update_flag = sys.argv[1]

# get list of zip files
filename = 'processed' # A file that tracks whether the corresponsing SLC has been generated. If so, it's EOF file has already been downloaded
eofs=[]
if os.path.isfile(filename):
    with open(filename,'r') as feof:
        eofs=feof.readlines()

print('      Downloading precise orbit files from ASF, if needed')
zipfiles = []
for file in os.listdir("."):
    if file.endswith(".zip"):
        SAFEname = file.replace('.zip','\n')
        if (SAFEname in eofs) and (update_flag=='N'):
            continue
        
        command = 'wget --content-disposition https://s1-orbits.asf.alaska.edu/scene/'+SAFEname.strip()
        print('        '+command)
        ret=os.system(command)

# Clean duplicates
command = 'rm *.EOF.*'
ret=os.system(command)

# Remove non-precise orbit files, because we don't want these to be processed until the best file is available
command = 'rm *RESORB*'
ret=os.system(command)

print ('      EOF (precise orbits) files downloaded!')
