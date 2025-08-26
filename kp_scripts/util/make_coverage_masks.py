#!/usr/bin/env python3

#  make_coverage_masks.py -- create masks for SLCs input geolist, to help determine satellite coverage

import sys
import os

if len(sys.argv) < 3:
    print ('Usage: make_coverage_masks.py geolist nr naz <update_flag=N>')
    sys.exit(1)

# Get your arguments from inputs
geolist=sys.argv[1]
nr=sys.argv[2]
naz=sys.argv[3]

update_flag = 'N'
if len(sys.argv)>3:
    update_flag = sys.argv[4]

# Run through geolist to get the appropriate mask

fgeos=open(geolist,'r')
geos=fgeos.readlines()
fgeos.close()
 
for line in geos:
    words=line.split('/')
    filename=words[-1].strip()
    outfile=filename.replace('geo','mask')
    if (os.path.isfile(outfile)==False) or (update_flag=='Y'):
        # make the mask
        command="$PROC_HOME/kp_scripts/util/coverage_mask "+filename+' '+nr+' '+naz+' '+outfile
        print('      '+command)
        ret=os.system(command)
