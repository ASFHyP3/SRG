#!/usr/bin/env python3
#
#  download precise orbit files for zipfiles in directory
#

import os
import subprocess
import sys


if len(sys.argv) < 1:
    print ('Usage: sentinel_orbitfiles.py')

for file in os.listdir("."):
    if file.endswith(".zip"):
        scene = file.strip('.zip')
        subprocess.run(['wget', '--content-disposition', f'https://s1-orbits.asf.alaska.edu/scene/{scene}'])
