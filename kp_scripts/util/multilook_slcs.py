#!/usr/bin/env python3

#  multilook_slcs.py - multilook a list of geocoded slcs

import sys
import os
import sys
import math
import glob

if len(sys.argv) < 5:
    print('Usage: multilook_slcs.py geolist len across-looks down-looks <update_flag=N>')
    sys.exit(1)

filelist=sys.argv[1]
filelen=sys.argv[2]
xlooks=sys.argv[3]
ylooks=sys.argv[4]

update_flag = 'N'
if len(sys.argv)>5:
    update_flag = sys.argv[5]

# make a geolist for the directory, named as the input file
exists = []
if update_flag == 'N':
    exists = glob.glob('*.geo')

# open the geolist
f=open(filelist,'r')
files=f.readlines()
for line in files:
    infile=line.strip()
    words = infile.split('/') # only want to geo file name, not its path
    outfile=words[-1].replace('.geo','_multi.geo')
    if outfile in exists:
        print('    Skipping '+outfile+', already exists')
    else:
        print('    Multilooking '+infile)
        ret=os.system('$PROC_HOME/util/yujiepowlooks '+infile+' '+outfile+' '+filelen+' '+xlooks+' '+ylooks)
    
sys.exit()

