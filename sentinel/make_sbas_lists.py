#!/usr/bin/env python3

# create a list of sbas pairs for each geolist in listofgeolists (nearest and nearest+maxtb)

import sys
import subprocess
from datetime import datetime 
import os

if len(sys.argv) < 4:
    print('Usage: make_sbas_lists.py listofgeolists  geopath  max_temporal')
    sys.exit(1)

listofgeolists = sys.argv[1]
geopath = sys.argv[2]
maxtemporal=int(sys.argv[3])

maxspatial = 10000

# open listofgeolists
fgeo = open(listofgeolists,'r')
geos = fgeo.readlines()
fgeo.close()

# Make sbas lists
for geolist in geos:

    # nearest neighbor
    outname = geolist.replace('geolist','sbas').replace('.txt','_nearest').strip()
    command = '$PROC_HOME/sentinel/sbas_list_nearest.py '+geolist.strip()+' '+geopath+' 0 10000 '+outname
    print(command)
    ret=os.system(command)

    # nearest neighbor + maxtb
    outname = outname.replace('nearest',str(maxtemporal))
    command = '$PROC_HOME/sentinel/sbas_list_nearest.py '+geolist.strip()+' '+geopath+' '+str(maxtemporal)+' 10000 '+outname
    print(command)
    ret=os.system(command)

print ('#### ---- ALL SBAS LISTS WRITTEN ----- #####')
