#!/usr/bin/env python3

# create a list of sbas pairs for each geolist in listofgeolists (nearest and nearest+maxtb)

import sys
import subprocess
from datetime import datetime 
import os

if len(sys.argv) < 3:
    print('Usage: multi_sbas_setup.py listofgeolists sbas_suffix')
    sys.exit(1)

listofgeolists = sys.argv[1]
sbassuff = sys.argv[2]
#maxtemporal=int(sys.argv[3])

# open listofgeolists
fgeo = open(listofgeolists,'r')
geos = fgeo.readlines()
fgeo.close()

# Make sbas lists
for geolist in geos:

    geolist2 = geolist.replace('.txt','').strip()

    # get names of files
    sbas_list = geolist2.replace('geolist','sbas')
    sbas_list = sbas_list+sbassuff

    suff = geolist2.replace('geolist','')

    # Run the sbas setup
    command = '$PROC_HOME/sbas/sbas_setup_kp.py '+sbas_list+' '+geolist.strip()+' '+suff
    print(command)
    ret=os.system(command)

print ('#### ---- ALL SBAS STRUCTURES  WRITTEN ----- #####')
