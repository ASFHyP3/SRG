#!/usr/bin/env python3
#
#
#  tropocorrect.py -- compensate troposphere using regression vs elevation
#
#   Also finds list of phase reference points for later use (e.g. sbas)
#
#

import os
import sys

if len(sys.argv)<4:
    print ('Usage: tropocorrect.py unwlist unwwidth unwlength <reference_threshold = 0.5>')
    sys.exit(0)

print (len(sys.argv))

unwlist = sys.argv[1]
unwwidth = sys.argv[2]
unwlength = sys.argv[3]
thresh = '0.5'
if len(sys.argv) > 4:
    thresh = sys.argv[4]

# determine some reference point locations
command = '$PROC_HOME/int/findrefpoints unwlist '+unwwidth+' '+unwlength+' '+thresh
print (command)
ret = os.system(command)

# regress vs hgt to remove tropo

isave=0
if isave==1:
    command = 'mkdir unwrapped_orig_files'  # save unwrapped files without correction
    print (command)
    ret = os.system(command)
    command = 'cp *.unw unwrapped_orig_files/'
    print (command)
    ret = os.system(command)

# do the regression
command = '$PROC_HOME/sentinel/regress_igrams.py unwlist dem '+unwwidth+' '+unwlength+' ref_locs'
print (command)
ret = os.system(command)

print('Troposphere regression complete')
