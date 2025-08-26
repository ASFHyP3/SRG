#!/usr/bin/env python3

#  psfilter.py -- create ps mask and interpolate ps from a second list

import sys
from datetime import datetime
import os
import math

if len(sys.argv) < 4:
    print ('Usage: psfilter_hz.py interpolatelist length simmask')
    sys.exit(1)

interpolatelist=sys.argv[1]
length=sys.argv[2]
simmask=sys.argv[3]


# interpolate the files in the interpolatelist
finterp=open(interpolatelist,'r')
interp=finterp.readlines()
finterp.close()
for intfile in interp:
    if os.path.isfile(intfile.strip()+'.interp')==False:
        command="$PROC_HOME/ps/psinterp "+intfile.strip()+" "+simmask+" "+intfile.strip()+".interp "+length
        print(command)
        ret=os.system(command)
