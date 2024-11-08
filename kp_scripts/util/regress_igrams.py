#!/usr/bin/env python3

#  regress_igrams - remove elevation-dependent height from unwrapped igrams

import sys
ver=sys.version_info

import os
import sys
import math

if len(sys.argv) < 4:
    print ('Usage: regress_igrams.py unwlist demfile len lines location_file <intmaskflag=N>')
    sys.exit(1)

### --- REQUIRED PARAMETERS --- ###
filelist=sys.argv[1]
demfile=sys.argv[2]
width=sys.argv[3]
lines=sys.argv[4]
locationfile=sys.argv[5]

### --- DEFAULT PARAMETERS --- ###
intmaskflag = 'N'

### --- CHECK INPUTS --- ###
if len(sys.argv)>6:
    intmaskflag = sys.argv[6]

with open(filelist,'r') as funw:
    unwlist=funw.readlines()

for line in unwlist:
    # Get input file name
    unwfile = line.rstrip()

    # Get output file name
    words=line.split('/')
    outfile=words[-1].rstrip()

    if os.path.isfile(outfile)==False:
        if intmaskflag == 'Y':
            # Cull the input reflocs file based on the interferogram mask, and make a temporary file to load in
            maskfile = unwfile.replace('.unw','.mask')
        else:
            maskfile=''

        command='$PROC_HOME/kp_scripts/util/regressheight '+unwfile+' '+outfile+' '+demfile+' '+locationfile+' '+width+' '+lines
        print('      '+command)
        ret=os.system(command)

sys.exit()

