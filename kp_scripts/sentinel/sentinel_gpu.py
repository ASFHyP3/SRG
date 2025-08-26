#!/usr/bin/env python3
#
#  process stack of sentinel files to coregistered geocoded slcs from L0 data
#  
#  First, either download the zip files you want to process, then start this script:
#
#  sentinel_gpu_cloud.py
#
#  backproject using gpu integration
#
#  If not logged in you willbe prompted for username/password in vertex/Earthdata system
#

import sys
import os
import string
import time
import subprocess
from datetime import datetime

if len(sys.argv) < 1:
    print ('Usage: sentinel_gpu.py <update_flag=N> <vv or vh (def. vv)>')

# Default Parameters
update_flag = 'N'
pol='vv'

# Get optional inputs
if len(sys.argv) > 1:
    update_flag = sys.argv[1]

if len(sys.argv) > 2:
    pol=sys.argv[2]
    if pol == 'VH':
        pol='vh'

# and start
print ('\n##### ----- Processing stack of sentinel raw data products to coregistered geocoded slcs ----- #####')

# get the PATH of the script directory
PATH=os.path.dirname(os.path.abspath(sys.argv[0]))

# Load or create list of processed files
if update_flag == 'Y':
    processed = []
    fproc = open('processed','w')
else:
    if os.path.isfile('processed'):
        with open('processed','r') as fproc:
            processed = fproc.readlines()
        fproc = open('processed','a')
    else:
        processed = []
        fproc = open('processed','w')

# get list of L0 products that need to be processed (or updated, if update_flag = 'Y')
fziplist=open('zipfiles','w')
zipfiles = []
SAFEnames = []
num=0
unzipcommand=[]
for file in os.listdir("."):
    if file.endswith(".zip"):
        zipfile = file.strip()
        if (zipfile+'\n' in processed) and (update_flag == 'N'):
            print('\n    skipping '+zipfile.strip())
            continue
        print('\n    '+zipfile)
        zipfiles.append(file)
        unzipcommand.append(subprocess.Popen(['unzip', '-u',file]))
        SAFEnames.append(file[0:len(file)-4])
        fziplist.write(file+'\n')
        num=num+1

# unzip the zip files to safe files is the safefile doesn't already exist
for i in range(num):
    if os.path.isfile(SAFEnames[i])==False:
        unzipcommand[i].wait()

fziplist.close()

if len(zipfiles)>0:
    # download the precise orbit files for these zips
    command = PATH+'/sentinel_orbitfiles.py '+update_flag
    print('\n    '+command)
    ret=os.system(command)

    # list the precise orbit files
    ret=os.system('ls -1 *.EOF | cat > preciseorbitfiles')
    with open('preciseorbitfiles','r') as preciseorbitfiles:
        preciseorbitlist=preciseorbitfiles.readlines()


    ##### ----- Process in Parallel ----- #####
    #  how many gpus do we have?
    proc = subprocess.Popen("$PROC_HOME/sentinel/howmanygpus",stdout=subprocess.PIPE, shell=True)
    (param,err)=proc.communicate()
    ngpus=str(param,'UTF-8').split()[0]
    print ('gpus available: ',ngpus)

    command='$PROC_HOME/kp_scripts/sentinel/process_parallel.py zipfiles '+str(ngpus)
    print (command)
    ret=os.system(command)

    print ('\n    Loop over scenes complete.')
    
    # Write list of zipfiles to 'processed'
    for filename in zipfiles:
        if os.path.isfile(filename.replace('.zip','.geo')):
            fproc.write(filename+'\n')
    fproc.close()

    #  clean up a bit
    command = 'find . -name \*positionburst\* -delete'
    ret=os.system(command)







