#!/usr/bin/env python3

#  extract lat, lon limits for a 3 swath s1a/b scene

import sys
ver=sys.version_info

    


    

import string
import sys
import math

if len(sys.argv) < 2:
    print 'Usage: sentinel_coordinates.py SAFEname'
    sys.exit(1)

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
    latmin=min(latmin,float(corner[0]))
    latmax=max(latmax,float(corner[0]))
    lonmin=min(lonmin,float(corner[1]))
    lonmax=max(lonmax,float(corner[1]))

print latmin,latmax,lonmin,lonmax

fd=open('latloncoords','w')
fd.write(str(latmin)+'\n')
fd.write(str(lonmin)+'\n')
fd.write(str(latmax)+'\n')
fd.write(str(lonmax)+'\n')
fd.close()

sys.exit(0)
