#!/usr/bin/env python3
#
#  process stack of sentinel files to coregistered geocoded slcs from L0 data

import sys
import os
import string
import time
import subprocess
from datetime import datetime

ver=sys.version_info

    


    

if len(sys.argv) < 1:
    print 'Usage: sentinel_stack.py <vv or vh (def. vv)>'

pol='vv'
if len(sys.argv) > 1:
    pol=sys.argv[1]
    if pol == 'VH':
        pol='vh'

    print 'Processing ',pol,' polarization'

print 'Processing stack of sentinel raw data products to coregistered geocoded slcs'

# get the PATH of the script directory
PATH=os.path.dirname(os.path.abspath(sys.argv[0]))

# Create a 'params' file
params=open("params","w")
p1 = os.getcwd()+"/elevation.dem"
p2 = os.getcwd()+"/elevation.dem.rsc"
params.write(p1+'\n')
params.write(p2+'\n')
params.close()
print "DEM file set to elevation.dem"
print "RSC file set to elevation.dem.rsc"

# get list of L0 products
zipfiles = []
SAFEnames = []
for file in os.listdir("."):
    if file.endswith(".zip"):
            #print file
            zipfiles.append(file)
            command = 'unzip -u '+file
            print (command)
            ret = os.system(command)
            SAFEnames.append(file[0:len(file)-4])

#print "zipfiles: ",zipfiles
print "basenames: ",SAFEnames

# get the precise orbit files
ret=os.system('ls -1 *.EOF | cat > preciseorbitfiles')
preciseorbitfiles=open('preciseorbitfiles','r')
preciseorbitlist=preciseorbitfiles.readlines()
preciseorbitfiles.close()

print 'Precise orbit list:'
print preciseorbitlist
#print

# loop over directories and process each with sentinel_back.py
#   sentinel_back needs zipfile and precise orbit 
for zipfile in zipfiles:
    #  which precise orbit file for this scene?
    print 'zipfile: ',zipfile
    char1=zipfile.find('SSV_')
    if char1 < 0:
        char1=zipfile.find('SDV_')
        print 'This is a dual pol acquisition'
    else:
        print 'This is a single pol acquisition'
    char2=zipfile[char1:].find('T')
    scenedate=zipfile[char1+4:char1+char2]
#    orbitfilestartdate=int(scenedate)-1
#    orbitfilestopdate=int(scenedate)+1
#    orbitfilename = 'no_precise_orbit'
    doy=datetime.strptime(scenedate, '%Y%m%d').timetuple().tm_yday
    year=scenedate[0:4]   # day of year and year for scene
    print 'doy ',doy,' ',year
    if doy > 1:
        orbitfilestartdate = datetime.strptime(year+' '+str(doy-1),'%Y %j').strftime('%Y%m%d')
    else:
        lastdoy=datetime.strptime(str(int(year)-1)+'1231', '%Y%m%d').timetuple().tm_yday
        orbitfilestartdate = datetime.strptime(str(int(year)-1)+' '+str(lastdoy),'%Y %j').strftime('%Y%m%d')
#    print 'orbit file start date: ',orbitfilestartdate
    command='grep '+orbitfilestartdate+' preciseorbitfiles'
    proc = subprocess.Popen(command, stdout=subprocess.PIPE, shell=True)
    (orbitfilename, err) = proc.communicate()
    print "Precise orbit file found:", orbitfilename

    SAFEname=zipfile[0:len(zipfile)-4]

    if len(orbitfilename) < 1:
        print 'Missing precise orbit file for ',zipfile
        command = 'echo skipping file, missing precise orbit'
    else:
        command='$PROC_HOME/sentinel_scene.py '+SAFEname+' '+orbitfilename.strip()+' '+pol

    print (command)
    ret=os.system(command)

print 'Loop over scenes complete.'






