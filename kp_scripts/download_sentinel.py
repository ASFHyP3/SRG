#!/usr/bin/env python3
#
#
#  download_sentinel.py -- Get EarthData login (if needed) and Download Sentinel-1 files that need processing
#
#
# INPUTS:
	# scenelist_file: file with names of L0 files to download (default = 'scenelist')
        #       update_flag: if files need to be redownloaded/reprocessed, set update_flag = 'Y'. If files do NOT
        #                    need to be redownloaded, keep update_flag = 'N'. (default = 'N')

# FILE REQUIREMENTS:
  
# GENERATES:
	#      .credentials: File containing Erathdata login information (username and password)
	#    asf_downloaded: (via asfwget_notebook.py) list of files that have already been downloaded and could
        #                    be skipped for download (either because they already exist or because they've already 
        #                    been processed.
        #       *.zip files: (via asfwget_notebook.py) Sentinel-1 zip files that need to be processed

import os
import getpass
import subprocess
import sys
import time


##### ----- DEFAULT PARAMETERS ----- #####
scenelist_file = 'scenelist'
update_flag = 'N'

##### ----- CHECK FOR INPUTS ----- #####
if len(sys.argv)>=2:
    scenelist_file = str(sys.argv[1])

if len(sys.argv)>=3:
    update_flag = str(sys.argv[2])

print('\n  You are reading scene list: '+scenelist_file+'; and update_flag = '+update_flag)

##### ----- RUN SCRIPT ----- #####
# 1. Get ASF username and password for data download
print('    You will now be downloading files to the subdirectories for your study area. You may be prompted for you Earthdata username and password. Your password will not be displayed, but it will be stored temporarily in a file only you can access, which will then be deleted. \nNote: If the requested raw data file has already been processed, it will not be downloaded again unless Update_Files_Flag=="Y" AND the file has not already been deleted. If you want to download a newer or more complete file, delete the file from "path_[number]/RAW" before executing the following.')

if os.path.isfile('.credentials')==False:
    print('Earthdata username: '),
    EDuser=input()
    print('Earthdata pawword (no echo): '),
    EDpassword=getpass.getpass()
    f=open('.credentials','w')
    os.chmod('.credentials',0o600)
    f.write(EDuser+'\n')
    f.write(EDpassword+'\n')
    f.close()
else:
    print('\n  Already have Earthdata credentials')
    creds = open('.credentials','r')
    EDuser=creds.readline().rstrip()
    EDpassword=creds.readline().rstrip()
    creds.close()

# 2. Download scenes in each subdirectory (path number from summary_paths file)
directory = os.getcwd()
print('\n  ##### ----- Downloading L0 data files ----- #####')

print('\n    Downloading RAW files in '+directory)
command = '$PROC_HOME/kp_scripts/asfdata/asfwget_notebook.py '+EDuser+' '+EDpassword+' '+scenelist_file+' '+update_flag+' 32'
print('      $PROC_HOME/kp_scripts/asfdata/asfwget_notebook.py '+EDuser+' <hidden password> '+scenelist_file+' '+update_flag+' 32')
ret = os.system(command)

print('\n    All available files downloaded!')




