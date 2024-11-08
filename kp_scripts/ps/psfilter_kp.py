#!/usr/bin/env python3

#  psfilter_kp.py -- create ps mask and interpolate ps from a second list

import sys
from datetime import datetime
import os
import math

if len(sys.argv) < 3:
    print ('Usage: psfilter_kp.py interpolatelist length')
    sys.exit(1)

interpolatelist=sys.argv[1]
length=sys.argv[2]


# create simmask and interpolate the files in the interpolatelist
finterp=open(interpolatelist,'r')
interp=finterp.readlines()
finterp.close()

for intfile in interp:
    if os.path.isfile(intfile.strip()+'.interp')==False:
        simmask='temp_mask'
        # Make the mask for the interferogram
        words=intfile.strip().split('_')
        date1=words[0]
        date2=words[1][:8]
        mask1='all_similarity_mask_'+date1
        mask2='all_similarity_mask_'+date2
        command='$PROC_HOME/kp_scripts/int/int_simmask '+mask1+' '+mask2+' '+length
        print(command)
        ret=os.system(command)
        command="$PROC_HOME/ps/psinterp "+intfile.strip()+" "+simmask+" "+intfile.strip()+".interp "+length
        print(command)
        ret=os.system(command)
