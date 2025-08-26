#!/usr/bin/env python3
#
#
#  determine_consistency.py -- Determine if your list of geofiles contains acquisitions with consistent coverage. 
#                              Make a 'geolist_consistent' and a 'geolist_culled'
#                              Where consistent are the consistent *.geo files with the most temporal coverage
#                              and culled are a list of inconsistent coverage, where the coverage of each .geo file
#                              exceeds the input threshold.
#                              The coverage is determined in reference to the scenemask, where only pixels covered
#                              by the existing *.geo files are considered valid.
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
    print('Usage: determine_consistency.py geolist_file path_to_geo coverage_thresh consistent_thresh')
    sys.exit(0)

##### ----- REQUIRED PARAMETERS ----- #####
geolist_file = sys.argv[1]
path_to_geo = sys.argv[2]
coverage_thresh = str(sys.argv[3])
consistent_thresh = str(sys.argv[4])

##### ----- DEFAULT PARAMETERS ----- #####

##### ----- CHECK FOR INPUTS ----- #####

##### ----- RUN SCRIPT ----- #####
# 1. Determine the multilooked study area size
with open('dem.rsc','r') as fe:
    words=fe.readline()
    unwwidth=words.split()[1].strip()
    words=fe.readline()
    unwlength=words.split()[1].strip()

# 2. Open the geolist
with open(geolist_file,'r') as fgeo:
    geolist = fgeo.readlines()
    nslc = str(len(geolist))

# 3. Find the combined coverage (potentially-valid pixels)
print('\n  ##### ----- Determining Valid Coverage, Quantifying Coverage, and Selecting SLCs ----- #####')
command = '$PROC_HOME/kp_scripts/util/determine_valid '+geolist_file+' '+nslc+' '+unwwidth+' '+unwlength+' '+coverage_thresh
print('    '+command)
ret=os.system(command)

# 4. Determine if the consistent and culled geolist are the same and write to CONSISTENT_FLAG
with open('geolist_consistent','r') as fgeo:
    geolist_consistent = fgeo.readlines()
    nslc_consistent = len(geolist_consistent)

with open('geolist_culled','r') as fgeo:
    geolist_culled = fgeo.readlines()
    nslc_culled = len(geolist_culled)

culled_flag = 'N'
consistent_flag = 'Y'
if nslc_consistent == nslc_culled:
    if geolist_consistent == geolist_culled:
        print('\n    The Consistent and Culled Geolists are the same. You only need to process the Consistent')
    else:
        print('\n    The Consistent and Culled Geolists are NOT the same, but have the same number of SLCs')
        print('    Defaulting to Consistent geolist.')
elif nslc_culled-nslc_consistent<int(consistent_thresh):
    print('\n    Only '+str(nslc_culled-nslc_consistent)+' more SLCs in geolist_culled.')
    print('    Defaulting to Consistent geolist for simplicity.')
else:
    print('\n    There are '+str(nslc_culled-nslc_consistent)+' more SLCs in geolist_culled than geolist_consistent')
    print('    Processing geolist_culled and geolist_consistent')
    culled_flag = 'Y'
nslc_consistent = str(nslc_consistent)
nslc_culled = str(nslc_culled)

# Write out CONSISTENCY file
with open('CONSISTENT','w') as fcon:
    fcon.write('CONSISTENT_FLAG: '+consistent_flag+'\n')
    fcon.write('CULLED_FLAG: '+culled_flag+'\n')
    fcon.write('NSLC_CONSISTENT: '+nslc_consistent+'\n')
    fcon.write('NSLC_CULLED: '+nslc_culled+'\n')
    fcon.write('GEOLIST_CONSISTENT: '+'geolist_consistent'+'\n')
    fcon.write('GEOLIST_CULLED: '+'geolist_culled'+'\n')
    fcon.write('FULLRES_SUFFIX: '+'_fullres'+'\n')

# Make the geolist_consistent and geolist_culled lists that point to original SLCs (*.geo) in /GEO
print('\n    Writing out updated geolists pointing to high-resolution SLCs')
with open('geolist_consistent_fullres','w') as fgeo:
    for k in range(len(geolist_consistent)):
        newout = path_to_geo+'/'+geolist_consistent[k].replace('_multi','')
        fgeo.write(newout)


with open('geolist_culled_fullres','w') as fgeo:
    for k in range(len(geolist_culled)):
        newout = path_to_geo+'/'+geolist_culled[k].replace('_multi','')
        fgeo.write(newout)






