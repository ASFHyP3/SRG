#!/usr/bin/env python3

#  unwrap_igrams - unwrap set of interferograms

import sys
ver=sys.version_info

import os
import sys
import math

if len(sys.argv) < 4:
    print ('Usage: unwrap_igrams_mask.py intlist len path_to_aux <intmaskflag=N> <scenemask=none> <lowpass box size=1>')
    sys.exit(1)

### --- REQUIRED PARAMETERS --- ###
filelist=sys.argv[1]
width=sys.argv[2]
path_to_aux=sys.argv[3]

### --- DEFAULT PARAMETERS --- ###
intmaskflag = 'N'
scenemask = 'none'
box=1

### --- CHECK PARAMETERS --- ###
args = len(sys.argv)
if args>4:
    intmaksflag = sys.argv[4]

if args>5:
    scenemask = sys.argv[5]

if args>6:
    box=sys.argv[6]

print(filelist,width,box)

with open(filelist,'r') as fint:
    intlist = fint.readlines()

for line in intlist:
    intfile=line.strip()
    print ('intfile: ',intfile)

    shortname = intfile.split('/')[-1]

    # Get other file names
    unwfile=intfile.replace('.int','.unw')
    lowpassfile=intfile+'.lowpass'
    cfile=path_to_aux+'/'+shortname.replace('.int','.cc')

    # Determine masking
    if intmaskflag == 'Y':
        mfile = ' -M '+path_to_aux+'/'+shortname.replace('.int','.mask')  
    elif scenemask != 'none':
        mfile = ' -M '+scenemask
    elif scenemask == 'none':
        mfile = ''

    if abs(float(box)-1) > 0.1:
        # lowpass filter a bit to aid unwrapping is desired
        ret=os.system('$PROC_HOME/ps/lowpass '+intfile+' '+width+' '+box)
        ret=os.system('$PROC_HOME/bin/snaphu '+lowpassfile+' '+width+' -d -o '+unwfile+' -c '+cfile+' '+mfile+' --mcf')
    else:
        ret=os.system('$PROC_HOME/bin/snaphu '+intfile+' '+width+' -d -o '+unwfile+' -c '+cfile+' '+mfile+' --mcf')

sys.exit()

