#!/usr/bin/env python3
#
#  process stack of sentinel files to coregistered geocoded slcs from L0 data
#  then store geocoded slc product in the google cloud
#  
#  First, either download the zip files you want to process, then start this script:
#
#  sentinel_gpu_cloud.py
#
#  backproject using gpu integration
#
#  Or, create a file with the suffix ".list" that contains the zip file granule names (i.e., starts with "S1A..." or "S1B...")
#  on each line.  If not logged in you willbe prompted for username/password in vertex/Earthdata system
#

import sys
import os
import string
import time
import subprocess
from datetime import datetime

ver=sys.version_info

    


    

if len(sys.argv) < 1:
    print ('Usage: sentinel_gpu_cloud.py <vv or vh (def. vv)>')
    print
    print ('Download data files from ASF if there exists a *.list file with zipfile granule names on each line.')
    print

pol='vv'
if len(sys.argv) > 1:
    pol=sys.argv[1]
    if pol == 'VH':
        pol='vh'

    print ('Processing ',pol,' polarization')

#  grab the least used gpu
proc = subprocess.Popen("$PROC_HOME/bestgpu.sh",stdout=subprocess.PIPE, shell=True)
(bestGPU,err)=proc.communicate()
bestGPU=str(int(bestGPU.strip()))
print ('GPU number: ',bestGPU)
os.environ['CUDA_DEVICE_ORDER'] = 'PCI_BUS_ID'
os.environ['CUDA_VISIBLE_DEVICES'] = bestGPU


# and start
print ('Processing stack of sentinel raw data products to coregistered geocoded slcs')

# do we download more data from ASF?
for file in os.listdir("."):
    if file.endswith(".list"):
        command = "$PROC_HOME/asfdata/asfdownload.py "+file
        print (command)
#        ret=os.system(command)

# get the PATH of the script directory
PATH=os.path.dirname(os.path.abspath(sys.argv[0]))

# Create a 'params' file
params=open("params","w")
p1 = os.getcwd()+"/elevation.dem"
p2 = os.getcwd()+"/elevation.dem.rsc"
params.write(p1+'\n')
params.write(p2+'\n')
params.close()
print ("DEM file set to elevation.dem")
print ("RSC file set to elevation.dem.rsc")

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
print ("basenames: ",SAFEnames)

# download the precise orbit files for these zips
command = '$PROC_HOME/sentinel_orbitfiles.py'
print (command)
ret=os.system(command)

# list the precise orbit files
ret=os.system('ls -1 *.EOF | cat > preciseorbitfiles')
preciseorbitfiles=open('preciseorbitfiles','r')
preciseorbitlist=preciseorbitfiles.readlines()
preciseorbitfiles.close()

print ('Precise orbit list:')
print (preciseorbitlist)
#print

# loop over directories and process each with sentinel_back.py
#   sentinel_back needs zipfile and precise orbit 
for zipfile in zipfiles:
    #  which precise orbit file for this scene?
    print ('zipfile: ',zipfile)
    char1=zipfile.find('SSV_')
    if char1 < 0:
        char1=zipfile.find('SDV_')
        print ('This is a dual pol acquisition')
    else:
        print ('This is a single pol acquisition')
    char2=zipfile[char1:].find('T')
    scenedate=zipfile[char1+4:char1+char2]
#    orbitfilestartdate=int(scenedate)-1
#    orbitfilestopdate=int(scenedate)+1
#    orbitfilename = 'no_precise_orbit'
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
    print ("Precise orbit file found:", orbitfilename)

    SAFEname=zipfile[0:len(zipfile)-4]

#    create a DEM file if none exists
    create_dem=1
    for file in os.listdir('.'):
        if file == 'elevation.dem':
            create_dem=0

    print ('create_dem= ',create_dem)

    if create_dem==1:
        command="$PROC_HOME/sentinel_coordinates_srtm30.py "+SAFEname
        print (command)
        ret=os.system(command)
        
#    if create_dem==1:
#        ret=os.system('$PROC_HOME/DEM/latlon.py')

#    and process the scene in you have the precise orbit

    if len(orbitfilename) < 1:
        print ('Missing precise orbit file for ',zipfile)
        command = 'echo skipping file, missing precise orbit'
    else:
        command='$PROC_HOME/sentinel_scene_gpu.py '+SAFEname+' '+str(orbitfilename.strip())+' '+pol

    print (command)
    ret=os.system(command)

    # store geo slc file in the google cloud
    command = 'rclone copy '+SAFEname+'.geo fringe:sentinelgeo'
    print (command)
    ret=os.system(command)

print ('Loop over scenes complete.')






