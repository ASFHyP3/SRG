#!/usr/bin/env python3
#
#
#  tropocorrect.py -- compensate troposphere using regression vs elevation
#
#  INPUTS:
#          unwlist: list of unwrapped files to be processed. Note: if they exist in the current
#                   directory, they will be overwritten. So, it's suggested that if you want to save
#                   both versions of the unwrapped interferograms for comparison, you will create a
#                   subdirectory, move to that directory, and have your unwrapped files in unwlist
#                   point to the files in the original directory.
#         unwwidth: Width of unwrapped files
#        unwlength: Length of unwrapped files
#
#

import os
import sys

if len(sys.argv)<4:
    print ('Usage: tropocorrect.py unwlist unwwidth unwlength <reference_threshold = 0.5>')
    sys.exit(0)


unwlist = sys.argv[1]
unwwidth = sys.argv[2]
unwlength = sys.argv[3]
thresh = '0.5'
if len(sys.argv) > 4:
    thresh = sys.argv[4]


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
