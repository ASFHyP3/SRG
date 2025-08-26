#!/usr/bin/env python3

#  int_coverage_mask.py -- create a coverage (valid pixel) mask for each interferogram in an input intlist

import sys
from datetime import datetime
import os
import math

if len(sys.argv) < 2:
    print ('Usage: int_coverage_mask.py intlist length')
    sys.exit(1)

intlist=sys.argv[1]
length=sys.argv[2]

# create coverage mask for the files in the intlist
fint=open(intlist,'r')
ints=fint.readlines()
fint.close()

for intfile in ints:
    outfile = intfile.strip().replace('.int','.mask')
    if os.path.isfile(outfile)==False:
        # Make the mask for the interferogram
        words=intfile.strip().split('_')
        date1=words[0]
        date2=words[1][:8]
        mask1=date1+'_merged_multi.mask'
        mask2=date2+'_merged_multi.mask'
        command='$PROC_HOME/kp_scripts/int/int_simmask '+mask1+' '+mask2+' '+length+' '+outfile
        print('    '+command)
        ret=os.system(command)
print('\n    Coverage Masks Generated For All Interferograms in List!')
