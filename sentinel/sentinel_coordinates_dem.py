#!/usr/bin/env python3

#  extract lat, lon limits for a 3 swath s1a/b scene

import sys
ver=sys.version_info

    


    

import string
import sys
import math
import os
from osgeo import gdal

if len(sys.argv) < 2:
    print ('Usage: sentinel_coordinates_dem.py SAFEname')
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
    latmin=min(latmin,float(corner[0]))-0.2
    latmax=max(latmax,float(corner[0]))+0.2
    lonmin=min(lonmin,float(corner[1]))-0.2
    lonmax=max(lonmax,float(corner[1]))+0.2

print (latmin,latmax,lonmin,lonmax)

fd=open('latloncoords','w')
fd.write(str(latmin)+'\n')
fd.write(str(lonmin)+'\n')
fd.write(str(latmax)+'\n')
fd.write(str(lonmax)+'\n')
fd.close()

## next few lines are for python versions with gdal.Translate installed
## otherwise use command line version following

# retrieve a dem from opentopography site
#ds = gdal.Open('/vsicurl/https://cloud.sdsc.edu/v1/AUTH_opentopography/Raster/SRTM_GL1_Ellip/SRTM_GL1_Ellip_srtm.vrt')
#ds = gdal.Translate('elevation.dem', ds, projWin = [lonmin, latmax, lonmax, latmin], format="ENVI", outputType=gdal.GDT_Int16)
#ds = None

command = 'gdal_translate -of ENVI -ot Int16 -projwin '+str(lonmin)+' '+str(latmax)+' '+str(lonmax)+' '+str(latmin)+' /vsicurl/https://cloud.sdsc.edu/v1/AUTH_opentopography/Raster/SRTM_GL1_Ellip/SRTM_GL1_Ellip_srtm.vrt elevation.dem'
print (command)
ret = os.system(command)

#  make a rsc file for processing
ds = gdal.Open('elevation.dem')
trans = ds.GetGeoTransform()

print (trans)
Xpixel=0.5
Yline=0.5
X0 = trans[0] + Xpixel*trans[1] + Yline*trans[2]
Y0 = trans[3] + Xpixel*trans[4] + Yline*trans[5]
# size of dem
xsize=ds.RasterXSize
ysize=ds.RasterYSize

print (X0, Y0, xsize, ysize)

fd=open('elevation.dem.rsc','w')
fd.write('WIDTH         '+str(xsize)+"\n")
fd.write('FILE_LENGTH   '+str(ysize)+"\n")
fd.write('X_FIRST       '+str(X0)+"\n")
fd.write('Y_FIRST       '+str(Y0)+"\n")
fd.write('X_STEP        '+str(trans[1])+"\n")
fd.write('Y_STEP        '+str(trans[5])+"\n")
fd.write('X_UNIT        degrees\n')
fd.write('Y_UNIT        degrees\n')
fd.write('Z_OFFSET      0\n')
fd.write('Z_SCALE       1\n')
fd.write('PROJECTION    LL\n')

fd.close()

#  patch invalid holes
command = '$PROC_HOME/patchinvalid elevation.dem'
print (command)
ret = os.system(command)

sys.exit(0)
