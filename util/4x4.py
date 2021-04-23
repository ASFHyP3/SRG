#!/usr/bin/env python3
#
#  take 4 more looks for a list in int amp or cc files
#

import sys
import os
import string
import time
import subprocess

if len(sys.argv) < 3:
    print "Usage: 4x4.py width looks\n",len(sys.argv)
    sys.exit(0)

width=sys.argv[1]
looks=sys.argv[2]

print width,looks

fd=open('../intlist','r')
intfiles=fd.readlines()
fd.close()

for file in intfiles:
    infile=file.strip()
    command = '$PROC_HOME/int/cpxlooks ../'+infile+' '+infile+'.4x4 '+width+' '+looks
    print (command)
    ret=os.system(command)

#  repeat for cc files

fd=open('../cclist','r')
ccfiles=fd.readlines()
fd.close()

for file in ccfiles:
    infile=file.strip()
    command = '$PROC_HOME/int/mhtlooks ../'+infile+' '+infile+'.4x4 '+width+' '+looks
    print (command)
    ret=os.system(command)

#  amp files

fd=open('../amplist','r')
ampfiles=fd.readlines()
fd.close()

for file in ampfiles:
    infile=file.strip()
    command = '$PROC_HOME/int/rilooks ../'+infile+' '+infile+'.4x4 '+width+' '+looks
    print (command)
    ret=os.system(command)


