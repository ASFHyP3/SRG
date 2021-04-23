#!/usr/bin/env python3
#
#  create a nasadem-compatible archive using the copernicus 30m dem from amazon

import sys
import os
import string

if len(sys.argv)< 1:
    print ('Usage: build_cop_archive tileList.txt')
    sys.exit(1)

tilelistfile=sys.argv[1]

# read list of cop 30m tiles
f=open(tilelistfile,'r')
tilelist=f.readlines()
f.close()
#  clean up tilename
for i in range(len(tilelist)):
    tilelist[i]=tilelist[i].strip()

print (tilelist[1],tilelist[2])

#  path to geotiff file
for i in range(2):
    tiffile='../copall/'
    tiffile+=tilelist[i]
    tiffile+='/'
    tiffile+=tilelist[i]
    tiffile+='.tif'
    print (tiffile)

    #  decode tif file
    command = '$H/DEM/coptiffread '+tiffile+' '+tilelist[i]+'.dem'
    print (command)
    ret=os.system(command)


sys.exit()

