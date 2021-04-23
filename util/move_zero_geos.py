#!/usr/bin/env python3
#
# move geo files with all zeros to zerogeos directory

import sys
import os
import string
import subprocess

if len(sys.argv) < 3:
    print 'Usage: move_zero_geos.py geolist len'
    sys.exit(0)

width = sys.argv[2]

command = 'mkdir zerogeos'
ret = os.system(command)

command = '$PROC_HOME/util/cull_zero_geo '+sys.argv[1]+' '+width
print (command)
ret = os.system(command)

fgeo=open('geolist.zero','r')

geos=fgeo.read()
geos=geos.rstrip()
fgeo.close()
#print (geos.rstrip())

geolist=geos.split('\n')
#print (geolist)

for geo in geolist:
    #print (geo)
    command = 'mv '+geo.rstrip()+' zerogeos/'
    print (command)
    ret = os.system(command)
