#!/usr/bin/env python3
#
#
#  cull_geolist_fromsim.py -- Determine if your list of geofiles contains acquisitions with bad data and cull from list
#                  1. Make sbaslist and intlist
#                  2. Form interferograms if they do not yet exist
#                  3. Make all similarity masks
#                  4. Find days with insufficient percentage of PS pixels relative to study area and based on input thresh 
#                  5. Make the final geolist
#                  6. Remake the daily all_similarity_mask for each .geo file
#
# INPUTS:
        #              geolist_file: list of full_resolution *.geo files
        #               path_to_geo: full path to the .geo files, (does not include final /)
        #                 scenemask: name of scenemask file containing potentially-valid pixels in the study area
        #                   looksac: Number of looks along x direction
        #                   looksdn: Number of looks along y direction
        #         inconsistent_flag: Indicates whether the geolist contains files that don't have matching coverage.
        #                            'N'=all files have same coverage, 'Y'=some files have different coverage

        #          temporalbaseline: maximum temporal baseline for forming interferograms (default=90)
        #           spatialbaseline: maximum spatial baseline for forming interferograms (default=1000)
        #                  psthresh: Threshold for determining PS pixels from mle estimator (default=2)
        #                 simthresh: Threshold for determining additional PS pixels from cosine similarity index
        #                            (default = 0.5)
        #               prct_thresh: Threshold for deciding whether to cull an SLC from the geolist if its coverage is
        #                            insufficient relative to the valid coverage indicated in scenemask
        #                            (default = 0.1, i.e. 10%)
# FILE REQUIREMENTS:
        #            params: parameters file in the current directory
        #           dem.rsc: multilooked dem.rsc file
        #  *.dem, *.dem.rsc: elevation files that params points to

# GENERATES:

import os
import subprocess
import sys
import time

if len(sys.argv)<7:
    print('Usage: cull_geolist_fromsim.py geolist_file path_to_geo scenemask looksac looksdn INCONSISTENT_FLAG <temporalbaseline=90> <spatialbaseline=1000> <psthresh=2> <simthresh=0.5> <prct_thresh=0.1>')
    sys.exit(0)

##### ----- REQUIRED PARAMETERS ----- #####
geolist_file = sys.argv[1]
path_to_geo = sys.argv[2]
scenemask = sys.argv[3]
looksac = sys.argv[4]
looksdn = sys.argv[5]
inconsistent_flag = sys.argv[6]

##### ----- DEFAULT PARAMETERS ----- #####
max_tb = '90'
max_sb = '1000'
psthresh = '2'
simthresh = '0.5'
prct_thresh = '0.1'

##### ----- CHECK FOR INPUTS ----- #####
args = len(sys.argv)

if args>7:
    max_tb = str(sys.argv[7])

if args>8:
    max_sb = str(sys.argv[8])

if args>9:
    psthresh = str(sys.argv[9])

if args>10:
    simthresh = str(sys.argv[10])

if args>11:
    prct_thresh = str(sys.argv[11])



##### ----- RUN SCRIPT ----- #####
# 1. Determine the multilooked study area size and path to original DEM
with open('dem.rsc','r') as fe:
    words=fe.readline()
    unwwidth=words.split()[1].strip()
    words=fe.readline()
    unwlength=words.split()[1].strip()

with open('params','r') as fparams:
    demin = fparams.readline().strip()
    rscin = fparams.readline().strip()

with open(rscin,'r') as fe:
    words = fe.readline()
    demwidth=words.split()[1].strip()
    words=fe.readline()
    demlength=words.split()[1].strip()

# 2. Make SBAS list
print('\n  Making SBAS list for Maximum Temporal Baseline = '+max_tb+' days')
command = '$PROC_HOME/kp_scripts/sentinel/sbas_list.py '+geolist_file+'_fullres '+path_to_geo+' sbaslist_'+max_tb+' '+max_tb+' '+max_sb
print('    '+command)
ret=os.system(command)

# 3. Make the corresponding intlist
command = '$PROC_HOME/kp_scripts/sbas/intlist_from_sbas.py sbaslist_'+max_tb
print('    '+command)
ret=os.system(command)

# 4. Make the interferograms in the list
print('\n  Making interferograms in sbaslist_'+max_tb)
command = '$PROC_HOME/kp_scripts/sentinel/ps_sbas_igrams.py sbaslist_'+max_tb+' '+rscin+' 1 1 '+demwidth+' '+demlength+' '+looksac+' '+looksdn
print('    '+command)
ret=os.system(command)

# 5. If inconsistent_flag is 'Y', create the interferogram masks
if inconsistent_flag == 'Y':
    print('\n  Making individual interferogram scene masks')
    command = '$PROC_HOME/kp_scripts/int/int_coverage_mask.py intlist_'+max_tb+' '+unwwidth
    print('    '+command)
    ret=os.system(command)
else:
    print('\n  All files in geolist are consistent, using single scene mask')

# 6. Make intlists for each .geo date
print('\n  Making intlist for each .geo file')
with open(geolist_file) as fgeo:
    geos = fgeo.readlines()

for geo in geos:
    indate = geo[:8]
    command = '$PROC_HOME/kp_scripts/sbas/intlist_from_sbas.py sbaslist_'+max_tb+' '+indate
    print('    '+command)
    ret=os.system(command)

nslc = len(geos)

# 6. Generate all_similarity_mask for each .geo file using cosine similarity index
print('\n  Making all_similarity_mask for each geocoded slc')
flag = '0'
if inconsistent_flag == 'Y':
    flag = '1'
command = '$PROC_HOME/kp_scripts/ps/make_simmasks_mask_parallel.py '+unwwidth+' '+geolist_file+' '+scenemask+' '+flag+' '+psthresh+' '+simthresh
print('    '+command)
ret=os.system(command)

# 7. Determine if there are SLCs that should be culled from the geolist
print('\n  Determining final geolist')
command = '$PROC_HOME/kp_scripts/util/cull_geolist_fromsim '+geolist_file+' '+unwwidth+' '+unwlength+' '+str(nslc)+' '+geolist_file+'_final '+scenemask+' '+prct_thresh
print('    '+command)
ret=os.system(command)

# 8. Determine if the final geolist is different than the input geolist
rerun = 'N'
with open(geolist_file+'_final','r') as fgeo:
    geolist_final = fgeo.readlines()
if len(geolist_final) != nslc:
    rerun = 'Y'
nslc = len(geolist_final)

# 9. If rerun = 'Y' then remake the all_similarity_mask for each SLC in updated geolist
if os.path.isdir('cosine_sim')==False:
    ret=os.system('mkdir cosine_sim')
ret=os.system('mv all_similarity* median_sim* pssim* scr* cosine_sim_par* cosine_sim')

if rerun == 'Y':
    print('\n  Remaking all_similarity_mask files for each date in final geolist')
    with open(geolist_file+'_final_fullres','w') as fgeo:
        for geo in geolist_final:
            outname = path_to_geo+'/'+geo.strip().replace('_multi','')+'\n'
            fgeo.write(outname)

    # Make New SBAS list
    command = '$PROC_HOME/kp_scripts/sentinel/sbas_list.py '+geolist_file+'_final_fullres '+path_to_geo+' sbaslist_'+max_tb+' '+max_tb+' '+max_sb
    print('    '+command)
    ret=os.system(command)

    command = '$PROC_HOME/kp_scripts/sbas/intlist_from_sbas.py sbaslist_'+max_tb
    print('    '+command)
    ret=os.system(command)

    # Make new intlist for each geolist
    for geo in geolist_final:
        indate = geo[:8]
        command = '$PROC_HOME/kp_scripts/sbas/intlist_from_sbas.py sbaslist_'+max_tb+' '+indate
        ret=os.system(command)

    # Recalculate the Similarity Masks
    command = '$PROC_HOME/kp_scripts/ps/make_simmasks_mask_parallel.py '+unwwidth+' '+geolist_file+'_final '+scenemask+' '+flag+' '+psthresh+' '+simthresh
    print('    '+command)
    ret=os.system(command)
     
    ret=os.system('mv all_similarity* median_sim* pssim* scr* cosine_sim_par* cosine_sim')
else:
    print('    No Rerun needed.')









