#!/usr/bin/env python3
#
#  download precise orbit files for zipfiles in directory
#

import os
import sys

import s1_orbits

if len(sys.argv) < 1:
    print ('Usage: sentinel_orbitfiles.py')

for file in os.listdir("."):
    if file.endswith(".zip"):
        scene = file.strip('.zip')
        s1_orbits.fetch_for_scene(scene)
