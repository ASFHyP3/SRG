#!/usr/bin/env python3

#  extract lat, lon limits for a 3 swath s1a/b scene

import string
import sys
import math
import os

# get home environment
# get the current environment
HOME = os.environ['PROC_HOME']
print ("HOME is "+HOME)

if len(sys.argv) < 2:
    print ('Usage: sentinel_coordinates_srtm.py SAFEname <full-res/quicklook f/q = q')
    sys.exit(1)

fullres='q'
if len(sys.argv)>2:
    if sys.argv[1]=='f':
        fullres='f'

print ('Full res/quicklook: ',fullres)


SAFEname=sys.argv[1]

# read the manifest.safe file
xmlfile=open(SAFEname+'.SAFE/manifest.safe','r')
xmllines=xmlfile.readlines()
xmlfile.close()

#  extract scene coordinates

for i in range(len(xmllines)):
    if '<coordinates' in xmllines[i]:
        start=i
        
coordinatelines=xmllines[start]

i=coordinatelines.find('<coordinates')
str1=coordinatelines[i:]

istart=str1.find('>')+1
istop=str1.find('</')
coordinates=str1[istart:istop]

latlons=coordinates.split(" ")

latmin=100
latmax=-100
lonmin=1000
lonmax=-1000
for i in range(len(latlons)):
    corner=latlons[i].split(",")
    latmin=min(latmin,float(corner[0]))-0.1
    latmax=max(latmax,float(corner[0]))+0.1
    lonmin=min(lonmin,float(corner[1]))-0.1
    lonmax=max(lonmax,float(corner[1]))+0.1

print (latmin,latmax,lonmin,lonmax)

fd=open('latloncoords','w')
fd.write(str(latmin)+'\n')
fd.write(str(lonmin)+'\n')
fd.write(str(latmax)+'\n')
fd.write(str(lonmax)+'\n')
fd.close()

# get the chosen 5 or 30 m srtm dem 
command = HOME+'/sentinel/latlon.srtm30.py '+fullres
print (command)
ret = os.system(command)

sys.exit(0)
