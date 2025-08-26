#!/usr/bin/env python3

#  make_simmasks_mask.py -- create ps mask for individual SLCs listed in geolist. 

# Included interferograms for each SLC are determined from the corresponding intlist for each.

import sys
from datetime import datetime
import os
import math

if len(sys.argv) < 3:
    print ('Usage: make_simmasks_mask.py geolist length <scenemask=none> <intmaskflag=0> <psthresh=2> <simthresh=0.5>')
    sys.exit(1)

geolist=sys.argv[1]
length=sys.argv[2]

# Default Parameters
scenemask = 'none'
intmaskflag = '0'
psthresh='2'
simthresh='0.5'

# Get optional input parameters
if len(sys.argv) > 3:
    scenemask=sys.argv[3]
if len(sys.argv) > 4:
    intmaskflag = sys.argv[4]
if len(sys.argv) > 5:
    psthresh=sys.argv[5]
if len(sys.argv) > 6:
    simthresh=sys.argv[6]

##### ----- Run cosine_sim_mask for each SLC in geolist ----- #####
# Import geolist
fgeos=open(geolist,'r')
geos=fgeos.readlines()
fgeos.close()
 
# Run through each SLC
for line in geos:
    words=line.split('/')
    indate=words[-1][:8]
    intlist = 'intlist_'+indate
    maskfile = indate+'_merged_multi.mask'

    if scenemask != 'none':
        # Create a temporary mask that is the intersection of the multilooked mask file for the SLC and the input scenemask
        print('  Intersecting SLC mask with '+scenemask)
        command = '$PROC_HOME/kp_scripts/int/int_simmask '+maskfile+' '+scenemask+' '+length+' '+maskfile+'_tempmask'
        print('     '+command)
        ret=os.system(command)
        maskfile = maskfile+'_tempmask'

    allsimfile = 'pssim_'+indate
    print('\n    ##### ----- '+intlist+' ----- #####')
    # find the ps and store in various formats
    command="$PROC_HOME/kp_scripts/ps/cosine_sim_mask "+intlist+' '+length+' '+allsimfile+' '+maskfile+' '+intmaskflag+' '+psthresh+' '+simthresh+' _'+indate
    print('    '+command)
    ret=os.system(command)

    if scenemask != 'none':
        ret=os.system('rm '+maskfile)
