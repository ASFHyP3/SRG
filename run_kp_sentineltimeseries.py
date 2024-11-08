#!/usr/bin/env python3
#
#
#  run_kp_sentineltimeseries.py -- generate subdirectories for a study area and run all available paths given input fileid files
#
#
# INPUTS:
	# summary_file: file with study area information (default = './summary_paths'
        # upsample_x: factor with which to upsample the 30 m copernicus DEM along x (e.g. 30/upsample_x ~= range resolution) (default=6 --> ~5 m resolution)
        # upsample_y: factor with which to upsample the 30 m copernicus DEM along y (e.g. 30/upsample_y ~= azimuth resolution) (default=2 --> ~15 m resolution)
	# looksac: number of looks to take in range/longitude (default = 12 --> ~60 m resolution if upsample_x=6)
	# looksdn: number of looks to take in azimuth/latitude (default = 4 --> ~60 m resolution if upsample_y=4)
	# timebaseline: maximum temporal baseline for sbas (default = 90 days)
	# spatialbaseline: maximum spatial baseline for sbas (default = 1000 m)
	# TROPO_CORR: 'Y'- apply tropospheric correction (default = 'Y')
        # INPUT_UPDATE_FLAG: if you'd like to update everything without even checking (default='N')
        # REMOVE_ZIP: remove .zip files for L0 data after the file has been processed/merged (default='Y')
        # REMOVE_SAFE: remove .safe files for L0 data after the file has been processed/merged (default='Y')
        # REMOVE_ORBTIMING: remove .orbtiming files in RAW directory after the file has been processed/merged (default='Y')
        # REMOVE_RAW_GEO: remove .geo files from RAW directory after the file has been merged into its yyyymmdd.geo file in GEO (default='Y')
        # WATER_MASK: flag to create a water mask (CURRENTLY NOT FUNCTIONAL) (default, for now = 'N')
        # coverage_thresh: threshold for an SLC date coverage within its valid study area (default=30 [%])
        # consistent_thresh: threshold to determine whether the consistent or culled list should be used. If the number of culled(inconsistent) SLCs is greater than the number of consistent SLCs by 
        #                    the input consistent_thresh value, then the culled (inconsistent) list is used in further processing. (default = 5) 
        # LOOK_VECTOR: flag to make look vectors for the multilooked .geo files (necessary when VH-decomposition is functional) (Default='Y')
        # ORIGINAL_WORKFLOW: Create a velocity/displacement solution using the original workflow (no phase reconstruction)
        # HZ_WORKFLOW: Create a velocity/displacement solution using Howard's phase reconstruction workflow (single similarity mask to define PS pixels)
        # KP_WORKFLOW: Create a velocity/displacement solution using Karissa's/Ke's phase reconsutrction workflow (interferogram-specific similarity mask to define PS pixels)
        #              --> This is currently turned off. The script will work for now, but I already see potential bugs tha will arise as written if using the subscript outside of this script
        # ps_thresh: MLE estimator threshold for first pass at finding PS pixels (default=2)
        # simthresh: cosine similarity index threshold for second/third passes at finding PS pixels (default=0.5)
        # prct_coverage_thresh: when culling .geo dates from final geolist, use similarity masks to determine percentage of valid coverage of PS pixels. If <prct_coverage, exclude from list
        #                       (default = 0.1 --> 10%)
        # refthresh: threshold used to find reference pixels for tropospheric correction and interferogram calibration for SBAS, based on cosine similarity output. (default = 0.6)

import os
import getpass
import subprocess
import sys
import time
import copy

##### ----- SET UP DEFAULT PARAMETERS ----- #####
sum_file = 'summary_paths'
upsample_x = '6'
upsample_y = '2'
looksac = '12'
looksdn = '4'
timebaseline = '90'
spatialbaseline = '1000'
TROPO_CORR = 'Y'
thresh = '0.5'
INPUT_UPDATE_FLAG = 'N'
REMOVE_ZIP = 'Y'
REMOVE_SAFE = 'Y'
REMOVE_ORBTIMING = 'Y'
REMOVE_RAW_GEO = 'Y'
WATER_MASK = 'N'
coverage_thresh = '30'
consistent_thresh = '5'
LOOK_VECTOR = 'Y'
ORIGINAL_WORKFLOW = 'Y'
HZ_WORKFLOW = 'Y'
KP_WORKFLOW = 'N'
psthresh = '2'
simthresh = '0.5'
prct_coverage_thresh = '0.1'
refthresh = '0.6'

##### ----- READ INPUTS FILE, IF INDICATED ----- #####



##### -----  RUN DATA_ARCH.PY TO CHECK/CREATE DEM AND SET UP DATA ARCHITECTURE ----- #####
print('\n##### ---------- SETTING UP DATA ARCHITECTURE ----------  #####')
command = '$PROC_HOME/kp_scripts/data_arch.py '+sum_file+' '+upsample_x+' '+upsample_y
print(command)
ret = os.system(command)

# Read Update_Files_Flag
with open('Update_Files_Flag','r') as fflag:
    line=fflag.readline().strip()
    update_flag = line.split(':')[-1].strip()

# Check if User wants to overwrite existing files, even if matching DEM exists
if INPUT_UPDATE_FLAG == 'Y':
    update_flag = 'Y'

##### ----- DETERMING THE HOME AREA AND SUBDIRECTORIES TO CYCLE THROUGH ----- #####
# Open subdirectories and figure out which are available for processing
with open('subdirectories','r') as fsubdirs:
    subdirs = []
    for subdir in fsubdirs.readlines():
        if os.path.isfile(subdir.strip()+'/scenelist'):
            subdirs.append(subdir.strip())

# Get study area directory as 'home'
home = os.getcwd()

########## ---------- CYCLE THROUGH THE DIRECTORIES FOR PROCESSING ---------- ##########
for subdir in subdirs:
    print('\n   Moving to '+subdir)
    os.chdir(subdir.rstrip())
    if os.path.isdir('RAW') == False:
        ret = os.system('mkdir RAW')
    os.chdir('RAW')

    ##### ----- DOWNLOAD THE SENTINEL-1 DATA WITH DOWNLOAD_SENTINEL.PY ----- #####
    print('\n##### ----- DOWNLOADING RAW DATA FILES ----- #####')

    command = '$PROC_HOME/kp_scripts/download_sentinel.py ../scenelist '+update_flag
    print('  '+command)
    ret=os.system(command)

    ##### ----- CREATE GEOCODED SLCS IN EACH FLIGHT PATH DIRECTORY ----- #####
    print('\n##### ---------- GENERATING GEOCODED SLCS ---------- #####')
    path_to_geo = subdir+'/GEO'
    command = '$PROC_HOME/kp_scripts/generate_geo.py '+home+' '+path_to_geo+' '+update_flag+' '+REMOVE_ZIP+' '+REMOVE_SAFE+' '+REMOVE_ORBTIMING+' '+REMOVE_RAW_GEO
    print('  '+command)
    ret=os.system(command)


    ##### ----- GENERATING SLC-LEVEL FILES FOR MULTILOOKED DATA ----- #####
    print('\n##### ----- GENERATING SLC-LEVEL FILES FOR MULTILOOKED DATA ----- #####')
    os.chdir(path_to_geo)
    ret=os.system('cp '+home+'/params .')

    # Create sbas directory, named by number of looks in each dimension
    sbas_path = 'sbas_'+looksac+'_'+looksdn
    if os.path.isdir(sbas_path)==False:
        command = 'mkdir '+sbas_path
        print('  '+command)
        ret=os.system(command)
    os.chdir(sbas_path)

    # Create Geolist for full-resolution geocoded slcs
    command = 'ls ../*.geo |cat> geolist_full'
    ret=os.system(command)
    ret=os.system('cp '+home+'/params .')

    # Run the script to generate multilooked geocoded slcs, multilooked dem and dem.rsc, study area masks, and look vectors
    command = '$PROC_HOME/kp_scripts/ml_scene_level.py geolist_full '+looksac+' '+looksdn+' '+update_flag+' '+WATER_MASK+' '+LOOK_VECTOR
    print('  '+command)
    ret=os.system(command)    


    ##### ----- GETTING USEFUL PARAMETERS ----- #####
    # Get the Original DEM file size and New DEM file size
    with open(home+'/elevation.dem.rsc','r') as fe:
        words=fe.readline()
        demwidth=words.split()[1].strip()
        words = fe.readline()
        demlength = words.split()[1].strip()

    with open('dem.rsc','r') as fe:
        words=fe.readline()
        unwwidth=words.split()[1].strip()
        words=fe.readline()
        unwlength=words.split()[1].strip()


    ##### ----- DETERMINE CONSISTENT/INCONSISTENT COVERAGE FROM SCENE MASKS ----- #####
    command = '$PROC_HOME/kp_scripts/determine_consistency.py geolist_ml .. '+coverage_thresh+' '+consistent_thresh
    print('  '+command)
    ret=os.system(command)

    # Read in CONSISTENT file
    with open('CONSISTENT','r') as fcon:
        consistent_flag = fcon.readline().strip().split(':')[-1].strip()
        culled_flag = fcon.readline().strip().split(':')[-1].strip()
        nslc_consistent = fcon.readline().strip().split(':')[-1].strip()
        nslc_culled = fcon.readline().strip().split(':')[-1].strip()
       
    print('\n  CULLED_FLAG = '+culled_flag) 

    ##### ----- USE DAILY ALL-SIMILARITY MASKS TO FIND & CULL BAD SLCS FROM GEOLIST ----- #####
    print('\n  ##### ----- USING COSINE SIMILARITY INDEX TO FIND/CULL BAD SLCs ----- #####')

    if update_flag == 'Y':
        print('\n  Removing existing interferogram files because update_flag = Y')
        command = 'rm *.int *.amp *.cc *.unw'
        ret=os.system(command)

        if (update_flag == 'Y') and (os.path.isfile('intlist_'+max_tb)):
            with open('intlist_'+max_tb,'r') as fint:
                for intname in fint.readlines():
                    os.system('rm '+intname.strip())

    # Determine input parameters for script
    if culled_flag == 'Y':
        geolistin = 'geolist_culled'
        scenemaskin = 'mask_culled'
        flag = 'Y'
    else:
        geolistin = 'geolist_consistent'
        scenemaskin = 'mask_consistent'
        flag = 'N'

    max_tb = '180'
    max_sb = '1000'

    # Run the script
    command = '$PROC_HOME/kp_scripts/cull_geolist_fromsim.py '+geolistin+' '+path_to_geo+' '+scenemaskin+' '+looksac+' '+looksdn+' '+flag+' '+max_tb+' '+max_sb+' '+psthresh+' '+simthresh+' '+prct_coverage_thresh

    print('\n  Creating final geolist using daily all_similarity_mask')
    print('    '+command)
    ret=os.system(command)

    # Name some final input parameters
    geolistin = geolistin+'_final'

    ##### ----- DOUBLE-CHECK THE FINAL GEOLIST FOR CONSISTENCY ----- #####
    print('\n  ##### ----- DOUBLE-CHECK THE FINAL GEOLIST FOR CONSISTENCY ----- #####')
    # Double check the final geolist for consistency (just in case inconsistent interferograms were culled from the list)
    command = '$PROC_HOME/kp_scripts/determine_consistency.py '+geolistin+' .. '+coverage_thresh+' '+consistent_thresh
    print('  '+command)
    ret=os.system(command)

    # Read in CONSISTENT file
    with open('CONSISTENT','r') as fcon:
        consistent_flag = fcon.readline().strip().split(':')[-1].strip()
        culled_flag = fcon.readline().strip().split(':')[-1].strip()
        nslc_consistent = fcon.readline().strip().split(':')[-1].strip()
        nslc_culled = fcon.readline().strip().split(':')[-1].strip()

    # Determine final input parameters for remainder of the script
    if culled_flag == 'Y':
        geolistin = 'geolist_culled'
        scenemaskin = 'mask_culled'
        flag = 'Y'
    else:
        geolistin = 'geolist_consistent'
        scenemaskin = 'mask_consistent'
        flag = 'N'

    

    ##### ----- RUN ORIGINAL WORKFLOW IF ORIGINAL_WORKFLOW = 'Y' ----- #####
    if ORIGINAL_WORKFLOW == 'Y':
        print('\n  Running original workflow with '+geolistin)
        command = '$PROC_HOME/kp_scripts/original_sbas.py '+geolistin+' '+path_to_geo+' '+timebaseline+' '+spatialbaseline+' '+looksac+' '+looksdn+' '+scenemaskin+' '+flag+' '+psthresh+' '+simthresh+' '+refthresh+' '+TROPO_CORR
        print('    '+command)
        ret=os.system(command)


    ##### ----- RUN HOWARD'S PHASE RECONSTRUCTION WORKFLOW IF HZ_WORFKLOW = 'Y' ----- #####
    if HZ_WORKFLOW == 'Y':
        print('\n  Running phase reconstruction (single mask) & SBAS with '+geolistin)
        command = '$PROC_HOME/kp_scripts/PR_sbas_singlemask.py hz_ints '+geolistin+' '+path_to_geo+' '+timebaseline+' '+spatialbaseline+' '+looksac+' '+looksdn+' '+scenemaskin+' '+flag+' '+psthresh+' '+simthresh+' '+refthresh+' '+TROPO_CORR
        print('    '+command)
        ret=os.system(command)


'''
    ##### ----- RUN KARISSA'S ALIASING IDENTIFICATION AND PHASE RECONSTRUCTION WORKFLOW IF KP_WORKFLOW = 'Y' ----- #####
    if KP_WORKFLOW == 'Y':
        print('\n  Running phase reconstruction (interferogram-specific mask) & SBAS with '+geolistin)
        command = '$PROC_HOME/kp_scripts/PR_sbas_multimask.py kp_ints '+geolistin+' '+path_to_geo+' '+timebaseline+' '+spatialbaseline+' '+looksac+' '+looksdn+' '+scenemaskin+' '+flag+' '+psthresh+' '+simthresh+' '+refthresh+' '+TROPO_CORR
        print('    '+command)
        ret=os.system(command)
'''


