#!/usr/bin/env python3
#
#  process list of sentinel files to coregistered geocoded slcs from L0 data
#  
#  backproject using gpu integration
#
#

import sys
import os
import string
import time
import subprocess
from datetime import datetime

#
if len(sys.argv) <2:
    print ('Usage: process_zip_list.py ziplist gpu_number <pol=vv>')
    sys.exit(1)

ziplist = sys.argv[1]
gpu_n=0
if len(sys.argv) > 2:
       gpu_n=sys.argv[2]
pol='vv'
if len(sys.argv) > 3:
       pol=sys.argv[3]

# read in files to be processed by this gpu
zipfiles=[]
fziplist=open(ziplist,'r')
zipfiles=fziplist.readlines()
fziplist.close()
for i in range(len(zipfiles)):
    zipfiles[i]=zipfiles[i].strip()
    
#print ('zipfiles: ',zipfiles)

# assign a gpu
os.environ['CUDA_DEVICE_ORDER'] = 'PCI_BUS_ID'
os.environ['CUDA_VISIBLE_DEVICES'] = gpu_n

for zipfile in zipfiles:
    print ('zipfile: ',zipfile)
    #  H or V pol
    char1=zipfile.find('SSV_')
    if char1 < 0:
        char1=zipfile.find('SDV_')
        print ('This is a dual pol acquisition')
    else:
        print ('This is a single pol acquisition')
    if char1 < 0:
        char1=zipfile.find('SSH_')
        if char1 < 0:
            char1=zipfile.find('SDH_')
            print ('This is a dual pol acquisition')
        else:
            print ('This is a single pol acquisition')
        #  if horizontal switch polarization (this is a hack, to be improved
        if pol == 'vv':
            pol='hh'
    char2=zipfile[char1:].find('T')
    scenedate=zipfile[char1+4:char1+char2]

    doy=datetime.strptime(scenedate, '%Y%m%d').timetuple().tm_yday
    year=scenedate[0:4]   # day of year and year for scene
    print ('doy ',doy,' ',year)
    if doy > 1:
        orbitfilestartdate = datetime.strptime(year+' '+str(doy-1),'%Y %j').strftime('%Y%m%d')
    else:
        lastdoy=datetime.strptime(str(int(year)-1)+'1231', '%Y%m%d').timetuple().tm_yday
        orbitfilestartdate = datetime.strptime(str(int(year)-1)+' '+str(lastdoy),'%Y %j').strftime('%Y%m%d')
#    print 'orbit file start date: ',orbitfilestartdate
    command='grep '+orbitfilestartdate+' preciseorbitfiles'
    proc = subprocess.Popen(command, stdout=subprocess.PIPE, shell=True)
    (orbitfilename, err) = proc.communicate()
    orbitfilename=str(orbitfilename,'UTF-8').rstrip()
    print ("Precise orbit file found:", orbitfilename)

    SAFEname=zipfile[0:len(zipfile)-4]

#    and process the scene if you have the precise orbit

    if len(orbitfilename) < 1:
        print ('Missing precise orbit file for ',zipfile)
        command = 'echo skipping file, missing precise orbit'
    else:
        command='$PROC_HOME/sentinel/sentinel_scene_multigpu.py '+SAFEname+' '+orbitfilename+' '+pol

    print (command)
    ret=os.system(command)

# clean extra position files
#command = 'find . -name \*positionburst\* -delete'
#ret=os.system(command)

print ('Loop over ziplist'+gpu_n+' complete.')







