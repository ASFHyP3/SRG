#!/usr/bin/env python3

#  psfilter.py -- create ps mask and interpolate ps from a second list

import sys
from datetime import datetime
import os
import math

if len(sys.argv) < 4:
    print ('Usage: psfilter.py igramlist interpolatelist length <psthresh> <simthresh>')
    sys.exit(1)

igramlist=sys.argv[1]
interpolatelist=sys.argv[2]
length=sys.argv[3]
psthresh='2'
simthresh='0.4'
if len(sys.argv) > 4:
    psthresh=sys.argv[4]
if len(sys.argv) > 5:
    simthresh=sys.argv[5]

# find the ps and store in various formats
command="$PROC_HOME/ps/cosine_sim "+igramlist+" "+length+" pssim "+psthresh+" "+simthresh
print(command)
ret=os.system(command)

# interpolate the files in the interpolatelist
finterp=open(interpolatelist,'r')
interp=finterp.readlines()
finterp.close()

for intfile in interp:
    command="$PROC_HOME/ps/psinterp "+intfile.strip()+" all_similarity_mask "+intfile.strip()+".interp "+length
    print(command)
    ret=os.system(command)
