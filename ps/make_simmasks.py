#!/usr/bin/env python3

#  make_simmasks.py -- create ps mask for an input intlist, and many masks given optional input geolist

import sys
from datetime import datetime
import os
import math

if len(sys.argv) < 3:
    print ('Usage: make_simmasks.py igramlist length <geolist=none> <psthresh=2> <simthresh=0.4>')
    sys.exit(1)

igramlist=sys.argv[1]
length=sys.argv[2]

geolist = 'none'
if len(sys.argv)>3:
    geolist = sys.argv[3]

psthresh='2'
simthresh='0.4'
if len(sys.argv) > 4:
    psthresh=sys.argv[4]
if len(sys.argv) > 5:
    simthresh=sys.argv[5]

if geolist != 'none':
    fgeos=open(geolist,'r')
    geos=fgeos.readlines()
    fgeos.close()
 
    for line in geos:
        words=line.split('/')
        indate=words[-1][:8]
        intlist = 'intlist_'+indate
        print('\n##### ----- '+intlist+' ----- #####')
        # find the ps and store in various formats
        command="$PROC_HOME/ps/cosine_sim "+intlist+' '+length+' pssim '+psthresh+' '+simthresh
        print(command)
        ret=os.system(command)
        command = 'mv all_similarity_mask all_similarity_mask_'+indate
        ret=os.system(command)

# find the ps and store in various formats
command="$PROC_HOME/ps/cosine_sim "+igramlist+" "+length+" pssim "+psthresh+" "+simthresh
print(command)
ret=os.system(command)

