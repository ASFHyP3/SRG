#!/usr/bin/env python3

#  create a kml file and a tiff for a geocoded file from dem rsc file
#   you can create an mph, mht, mag, or shaded relief image

import sys
import string
import os
import math

if len(sys.argv) < 3:
    print ('Usage: kmlout.py geocodedfile demrsc_file')
    sys.exit(1)
    

geofile=sys.argv[1]
rscfile=sys.argv[2]

# what kind of file are we looking for?
print (geofile, type(geofile), geofile[-3:])
suffix = geofile[-3:]
tiffile='image.tif'
    
# get info from rsc file
rsc=open(rscfile,'r')
words=rsc.readline()
demwidth=words.split()[1]
words=rsc.readline()
demlength=words.split()[1]
words=rsc.readline()
demxfirst=words.split()[1]
words=rsc.readline()
demyfirst=words.split()[1]
words=rsc.readline()
xstep=words.split()[1]
words=rsc.readline()
ystep=words.split()[1]
rsc.close()

min_lat=float(demyfirst)+(int(demlength)-1)*float(ystep)
max_lat=float(demyfirst)
min_lon=float(demxfirst)
max_lon=float(demxfirst)+(int(demwidth)-1)*float(xstep)

#  create tif file
if suffix == 'int':
    command="/home/zebker/bin/dismphfile "+geofile+" "+demwidth
    print (command)
    ret=os.system(command)
    ret=os.system('mv dismph.tif image.tif')
if suffix == 'geo':
    command="/home/zebker/bin/dismagfile "+geofile+" "+demwidth
    print (command)
    ret=os.system(command)
    ret=os.system('mv dismag.tif image.tif')
if suffix == 'amp':
    command="/home/zebker/bin/dismagfile "+geofile+" "+demwidth
    print (command)
    ret=os.system(command)
    ret=os.system('mv dismag.tif image.tif')
if suffix == 'dem':
    command="/home/zebker/bin/disshadefile "+geofile+" "+demwidth
    print (command)
    ret=os.system(command)
    ret=os.system('mv disshade.tif image.tif')
if suffix == '.cc':
    command="/home/zebker/bin/dishgtfile "+geofile+" "+demwidth+" 1 1000000 1.2"
    print (command)
    ret=os.system(command)
    ret=os.system('mv dishgt.tif image.tif')




f=open("image.kml",'w')
f.write('<?xml version="1.0" encoding="UTF-8"?>'+"\n")
f.write('<kml xmlns="http://earth.google.com/kml/2.2">'+"\n")
f.write('<GroundOverlay>'+"\n")
f.write('    <name>Scene Title Here</name>'+"\n")
f.write('    <description>Sentinel-1</description>'+"\n")
f.write('    <Icon>'+"\n")
f.write('          <href>'+tiffile+'</href>'+"\n")
f.write('    </Icon>'+"\n")
f.write('    <LatLonBox>'+"\n")
f.write('        <north> '+str(min_lat)+' </north>'+"\n")
f.write('        <south> '+str(max_lat)+' </south>'+"\n")
f.write('        <east> '+str(max_lon)+' </east>'+"\n")
f.write('        <west> '+str(min_lon)+' </west>'+"\n")
f.write('    </LatLonBox>'+"\n")
f.write('</GroundOverlay>'+"\n")
f.write('</kml>'+"\n")
f.close()
