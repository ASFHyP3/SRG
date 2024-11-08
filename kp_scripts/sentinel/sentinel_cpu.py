#!/usr/bin/env python3
#
#  process stack of sentinel files to coregistered geocoded slcs from L0 data.
#  
#  Either download the zip files you want to process, then start this script:
#
#  sentinel_cpu_kp.py
#
#  backproject using cpu, not gpu, integration
#

import sys
import os
import string
import time
import subprocess
from datetime import datetime


if len(sys.argv) < 1:
    print ('Usage: sentinel_cpu.py <update_flag=N> <vv or vh (def. vv)>')

# Default Parameters
update_flag = 'N'
pol='vv'

# Get Optional Inputs
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

# get list of L0 products
zipfiles = []
SAFEnames = []
num=0
unzipcommand=[]
for file in os.listdir("."):
    if file.endswith(".zip"):
        zipfiles.append(file)
        unzipcommand.append(subprocess.Popen(['unzip','-u',file]))
        SAFEnames.append(file[0:len(file)-4])
        num=num+1

# unzip the zip files to safe files if the safefile doesn't already exist
for i in range(num):
    if os.path.isfile(SAFEnames[i])==False:
        unzipcommand[i].wait()

# download the precise orbit files for these zips
command = '$PROC_HOME/kp_scripts/sentinel/sentinel_orbitfiles.py '+update_flag
print('\n    '+command)
ret=os.system(command)

# list the precise orbit files
ret=os.system('ls -1 *.EOF | cat > preciseorbitfiles')
with open('preciseorbitfiles','r') as preciseorbitfiles:
    preciseorbitlist=preciseorbitfiles.readlines()

# Load or create list of processed files
if update_flag == 'Y':
    processed = []
    fproc = open('processed','w')
else:
    if os.path.isfile('processed')==False:
        fproc = open('processed','w')
        processed = []
    else:
        with open('processed','r') as fproc:
            processed = fproc.readlines()
        fproc = open('processed','a')

# loop over directories and process each with sentinel_back.py
#   sentinel_back needs zipfile and precise orbit
print('\n##### ----- Processing Sentinel Scene ----- #####') 
for zipfile in zipfiles:
    print(zipfile)
    #  which precise orbit file for this scene?
    if (zipfile+'\n' in processed) and (update_flag == 'N'):
        print('\n    skipping '+zipfile.strip())
        continue
    print ('\n    zipfile: ',zipfile)
    #  H or V pol
    char1=zipfile.find('SSV_')
    if char1 < 0:
        char1=zipfile.find('SDV_')
        pol='vh'
        print ('    This is a dual pol acquisition '+pol)
    else:
        pol='vv'
        print ('    This is a single pol acquisition '+pol)
    if char1 < 0:
        char1=zipfile.find('SSH_')
        if char1 < 0:
            char1=zipfile.find('SDH_')
            pol='vh'
            print ('    This is a dual pol acquisition '+pol)
        else:
            pol='hh'
            print ('    This is a single pol acquisition '+pol)
    char2=zipfile[char1:].find('T')
    scenedate=zipfile[char1+4:char1+char2]

    doy=datetime.strptime(scenedate, '%Y%m%d').timetuple().tm_yday
    year=scenedate[0:4]   # day of year and year for scene
    print ('      doy ',doy,' ',year)
    if doy > 1:
        orbitfilestartdate = datetime.strptime(year+' '+str(doy-1),'%Y %j').strftime('%Y%m%d')
    else:
        lastdoy=datetime.strptime(str(int(year)-1)+'1231', '%Y%m%d').timetuple().tm_yday
        orbitfilestartdate = datetime.strptime(str(int(year)-1)+' '+str(lastdoy),'%Y %j').strftime('%Y%m%d')
    command='grep '+orbitfilestartdate+' preciseorbitfiles'
    proc = subprocess.Popen(command, stdout=subprocess.PIPE, shell=True)
    (orbitfilename, err) = proc.communicate()
    orbitfilename=str(orbitfilename,'UTF-8').rstrip()
    SAFEname=zipfile[0:len(zipfile)-4]

        
#    and process the scene if you have the precise orbit and it hasn't already been processed
    if len(orbitfilename) < 1:
        print ('    Skipping: Missing precise orbit file for ',zipfile)
    else:
        command='$PROC_HOME/kp_scripts/sentinel/sentinel_scene_cpu.py '+SAFEname+' '+orbitfilename+' ../params '+pol
        print('    '+command)
        ret = os.system(command)
        fproc.write(zipfile+'\n')
fproc.close()

print ('  Loop over scenes complete.')

#  clean up a bit
command = 'rm *positionburst*.out'
ret=os.system(command)


