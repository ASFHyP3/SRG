#!/usr/bin/env python3
#
#
#  generate_geo.py -- Process sentinel files to individual geocoded SLCs, then merge same-day L0 scenes
#
#
# INPUTS:
        #              home: path to study area home, which has params file and .credentials file
	#    path_to_merged: path to where you want to store the merged (final) .geo files
        #       update_flag: if files need to be redownloaded/reprocessed, set update_flag = 'Y'. If files do NOT
        #                    need to be redownloaded, keep update_flag = 'N'. (default = 'N')
        #        REMOVE_ZIP: remove *.zip files after they have been processed (default = 'N')
        #       REMOVE_SAFE: remove *.SAFE directories after they have been processed (default = 'N')
        #  REMOVE_ORBTIMING: remove *.orbtiming* files from RAW after L0 files have been merged (default = 'N')
        #    REMOVE_RAW_GEO: remove intermediate .geo files (pre-merge) after they have been merged (default = 'N')

# FILE REQUIREMENTS:
        #            params: parameters file in processing directory (home), to be copied to RAW
        #      .credentials: file with Earthdata login, in processing directory (home), to be copied to RAW
  
# GENERATES:
    #                       /GEO: GEO directory that holds the merge .geo files and .orbtiming files
    #      [yyyymmdd]_merged.geo: merged *.geo files, in GEO directory. Even when there are not multiple *.geo files 
    #                             for a single day, the single scenes are renamed and placed in the GEO directory
    #[yyyymmdd]_merged.orbtiming: *.orbtiming files for each day, to be used to find baselines between acquisitions
    #                merged_list: list of .geo files that have already been merged to ../GEO/*.geo (to avoid reprocessing)
    #                  processed: list of *.zip files that have been processed
    #          preciseorbitfiles: list of available preciseorbitfiles for processing


    #                  RAW/*.geo: original .geo files for each .zip file. Recommended to delete after processing to 
    #                             save space
    #           RAW/*.orbtiming*: orbtiming files for each .geo file. Recommended to delete after processing to save space
    #                 RAW/*.SAFE: SAFE directory, unzipped *.zip file. Recommended to delete after processing to save space
    

import os
import subprocess
import sys
import time

if len(sys.argv)<3:
    print('Usage: generate_geo.py home path_to_merged <udpate_flag=N> <REMOVE_ZIP=N> <REMOVE_SAFE=N> <REMOVE_ORBTIMING=N> REMOVE_RAW_GEO=N>')
    sys.exit(0)

##### ----- REQUIRED PARAMETERS ----- #####
home = sys.argv[1]
path_to_merged = sys.argv[2]

##### ----- DEFAULT PARAMETERS ----- #####
update_flag = 'N'
REMOVE_ZIP = 'N'
REMOVE_SAFE = 'N'
REMOVE_ORBTIMING = 'N'
REMOVE_RAW_GEO = 'N'

##### ----- CHECK FOR INPUTS ----- #####

if len(sys.argv)>3:
    update_flag = sys.argv[3]

if len(sys.argv)>4:
    REMOVE_ZIP = sys.argv[4]

if len(sys.argv)>5:
    REMOVE_SAFE = sys.argv[5]

if len(sys.argv)>6:
    REMOVE_ORBTIMING = sys.argv[6]

if len(sys.argv)>7:
    REMOVE_RAW_GEO = sys.argv[7]

print('\n  You are reading creating *.geo files in: '+path_to_merged+'; and update_flag = '+update_flag)
print('  Your Home directory is: '+home)
print('  You will be processing with the following flags:')
print('    REMOVE_ZIP: '+REMOVE_ZIP)
print('    REMOVE_SAFE: '+REMOVE_SAFE)
print('    REMOVE_ORBTIMING: '+REMOVE_ORBTIMING)
print('    REMOVE_RAW_GEO: '+REMOVE_RAW_GEO)

##### ----- RUN SCRIPT ----- #####
# 1. Determine if you will be using the CPU or GPU version
print('\nRun the Processor -- sentinel_cpu.py or sentinel_gpu.py')
ret = os.system('which nvidia-smi')
if ret==0:
    q=subprocess.Popen("nvidia-smi", stdout=subprocess.PIPE, shell=True)
    (qq,err) = q.communicate()
    if len(qq.decode())==0:
        ret=-1

if ret == 0:
    command = '$PROC_HOME/kp_scripts/sentinel/sentinel_gpu.py '+update_flag
else:
    command = '$PROC_HOME/kp_scripts/sentinel/sentinel_cpu.py '+update_flag


# 2. Run through subdirectories and process L0 files, then Merge
ret = os.system('cp '+home+'/params .')
ret = os.system('cp '+home+'/.credentials .')
print('\nRunning Sentinel processor')
print('\n  '+command)
ret = os.system(command)

directory = os.getcwd()
print('\nIndividual SLCs generated in '+directory.strip())

# if needed, merge the SLCs
print('\n##### ----- Merging SLCs ----- #####')
if update_flag == 'Y':
    command2 = 'rm '+path_to_merged+'/*.geo'
    ret=os.system(command2)
command2 = '$PROC_HOME/kp_scripts/util/merge_slcs.py '+path_to_merged+' '+update_flag
print('\n  '+command2)
ret = os.system(command2)

# Delete completed files from the RAW directory to clear up space
print('\n##### ----- Cleaning Processing Directory ----- #####')
if REMOVE_ZIP == 'Y':
    command2 = 'rm *.zip'
    print('    '+command2)
    ret=os.system(command2)

if REMOVE_SAFE == 'Y':
    command2 = 'rm -r *.SAFE'
    print('    '+command2)
    ret=os.system(command2)

if REMOVE_ORBTIMING == 'Y':
    command2 = 'rm *.orbtiming*'
    print('    '+command2)
    ret=os.system(command2)

if REMOVE_RAW_GEO == 'Y':
    command2 = 'rm *.geo'
    print('    '+command2)
    ret=os.system(command2)

command2 = 'rm zipfiles ziplist* listofziplists'
print('    '+command2)
ret=os.system(command2)




