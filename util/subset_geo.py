#!/usr/bin/env python3
#
# subset a list of geo files

import sys
import os
import string

ver=sys.version_info

    


    

if len(sys.argv) < 6:
    print 'Usage: subset_geo.py geolist len xstart ystart xwidth ywidth'
    sys.exit(0)

filelist=sys.argv[1]
filelen=sys.argv[2]
xstart=sys.argv[3]
ystart=sys.argv[4]
xwidth=sys.argv[5]
ywidth=sys.argv[6]

fgeolist=open(filelist,'r')
geolist=fgeolist.readlines()
fgeolist.close()

#  loop over files
for i in range(len(geolist)):
    command='$PROC_HOME/util/subsetcpx '+geolist[i].strip()+' '+geolist[i].strip().replace('../','').replace('.geo','.subset.geo')+' '+filelen+' '+xstart+' '+ystart+' '+xwidth+' '+ywidth
    print (command)
    ret=os.system(command)
