#!/usr/bin/env python3
#
#
#  ml_scene_level.py -- Create Scene-level files for a multilooked study area
#
#
# INPUTS:
        #              geolist_file: list of full_resolution *.geo files
        #                   looksac: Number of looks along x direction
        #                   looksdn: Number of looks along y direction
        #               update_flag: if files need to be recreated, set update_flag = 'Y'. If files do NOT
        #                            need to be recreated, keep update_flag = 'N'. (default = 'N')
        #                WATER_MASK: if you'd like to create a water mask and intersect it with the scene-level masks
        #                            set to 'Y' (default = N). Note: isn't currently functional
        #               LOOK_VECTOR: if you'd like to create look vectors for each multilooked .geo file, set to 'Y'
        #                            (default = 'N')

# FILE REQUIREMENTS:
        #            params: parameters file in the current directory
  
# GENERATES:
        #            *_multi.geo: multilooked geofiles with _multi tag
        #             geolist_ml: geolist equivalen to geolist_file, but pointing to the multilooked files
        #                    dem: multilooked DEM in current directory
        #                dem.rsc: multilooked DEM rsc file in current directory
        #           *_multi.mask: a mask designating where the acquisition has valid coverage. Note: if WATER_MASK
        #                         is 'Y' (and functional), then this mask includes valid coverage and land intersection   
        #             mask_water: water maks designating where pixels are land (not currently working!)
        #               *.labels: Label file for look vector 
        #           *.lookvector: Look vector file

import os
import subprocess
import sys
import time

if len(sys.argv)<4:
    print('Usage: ml_scene_level.py geolist_file looksac looksdn <udpate_flag=N> <WATER_MASK=N> <LOOK_VECTOR=N>')
    sys.exit(0)

##### ----- REQUIRED PARAMETERS ----- #####
geolist_file = sys.argv[1]
looksac = str(sys.argv[2])
looksdn = str(sys.argv[3])

##### ----- DEFAULT PARAMETERS ----- #####
update_flag = 'N'
WATER_MASK = 'N'
LOOK_VECTOR = 'N'


##### ----- CHECK FOR INPUTS ----- #####

if len(sys.argv)>4:
    update_flag = sys.argv[4]

if len(sys.argv)>5:
    WATER_MASK = sys.argv[5]

if len(sys.argv)>6:
    LOOK_VECTOR = sys.argv[6]

directory = os.getcwd()
print('\n  You are reading creating multilooked, scene-level files in: '+directory+'; and update_flag = '+update_flag)
print('  Your number of looks: '+looksac+' across and '+looksdn+' down')
print('  You will be processing with the following flags:')
print('    WATER_MASK: '+WATER_MASK)
print('    LOOK_VECTOR: '+LOOK_VECTOR)

##### ----- RUN SCRIPT ----- #####
# 1. Determine the full-resolution DEM size
with open('params','r') as fparams:
    demin = fparams.readline().strip()
    rscin = fparams.readline().strip()

with open(rscin,'r') as frsc:
    words=frsc.readline()
    demwidth=words.split()[1].strip()
    words=frsc.readline()
    demlength=words.split()[1].strip()

# 2. Create Multilooked .geo files
print('\n  ##### ----- Multilooking Geocoded SLCS ----- #####')
command = '$PROC_HOME/kp_scripts/util/multilook_slcs.py '+geolist_file+' '+demwidth+' '+looksac+' '+looksdn+' '+update_flag
print('    '+command)
ret=os.system(command)

# 3. Create a new DEM and DEM.RSC file for Multilooked DEM/study area
if (os.path.isfile('dem.rsc')==False) or (update_flag=='Y'):
    print('\n  ##### ----- Multilooking DEM ----- #####')
    command = '$PROC_HOME/kp_scripts/util/make_ml_demrsc.py '+rscin+' 1 1 '+demwidth+' '+demlength+' '+looksac+' '+looksdn
    print('    '+command)
    ret=os.system(command)

    # Create a reduced size DEM to match the multilooked files
    command = '$PROC_HOME/util/nbymi2 '+demin+' dem '+demwidth+' '+looksac+' '+looksdn
    print('    '+command)
    ret=os.system(command)

# 4. Get size of multilooked files from new rsc file
with open('dem.rsc','r') as fe:
    words=fe.readline()
    unwwidth=words.split()[1].strip()
    words=fe.readline()
    unwlength=words.split()[1].strip()

# 5. Create geolist for the multilooked .geo files in the current directory
command = 'ls *.geo |cat> geolist_ml'
ret=os.system(command)

# 6. Create Study area masks
print('\n  ##### ----- Generating Coverage Masks for Each GEocoded SLC ----- #####')
command = '$PROC_HOME/kp_scripts/util/make_coverage_masks.py geolist_ml '+unwwidth+' '+unwlength+' '+update_flag
print('    '+command)
ret=os.system(command)

# Make a water mask
if WATER_MASK=='Y':
    #print('\n    Generating Water Mask')

    # Merge the SLC coverage masks with the water mask (if designated)
    #print('    Finding Intersection of Each SLC mask with Water Mask')
    print('WATER MASK NOT CURRENTLY FUNCTIONL. TO BE UPDATED!')

# 7. Generate Look Vectors if LOOK_VECTOR = Y
if LOOK_VECTOR == 'Y':
    # Find the LOS look vector for each pixel, each multilooked .geo file   
    print('\n  ##### ----- Calculating Look Vectors for Each Multilooked Geocoded SLC ----- #####')

    with open(geolist_file,'r') as fgeo:
        geolist = fgeo.readlines()
    for k in range(len(geolist)):
        geolist[k] = geolist[k].strip()

    with open('orbitfiles','w') as forb:
        orbits = []
        for k in range(len(geolist)):
            newout = geolist[k].replace('.geo','.orbtiming')
            forb.write(newout+'\n')
            orbits.append(newout)

    norbs = len(orbits)
    for k in range(norbs):
        orbitname_in = orbits[k]
        lv_out = orbitname_in.split('/')[-1].replace('orbtiming','lookvector')

        if (os.path.isfile(lv_out) == False) or (update_flag=='Y'):
            command = '$PROC_HOME/sentinel/lookvector '+orbitname_in
            print('    '+command)
            ret=os.system(command)
            command = 'mv '+orbitname_in.replace('orbtiming','labels')+' .'
            ret=os.system(command)
            command = 'mv '+orbitname_in.replace('orbtiming','lookvector')+' .'
            ret=os.system(command)
        else:
            print('    '+lv_out+' already exists.')
    print('\nLook Vectors Generated!')






