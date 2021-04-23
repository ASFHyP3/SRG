#!/usr/bin/env python3
#
#  strip 4x4 suffix
#

import sys
import os
import string
import time
import subprocess

if len(sys.argv) < 1:
    print "Usage: strip4x4.py "
    sys.exit(0)

ret = os.system('ls -1 *4x4* | cat > 4x4list')

fd=open('4x4list','r')
files=fd.readlines()
fd.close()

for file in files:
    infile=file.strip()
    command = 'mv '+infile+' '+infile.replace('.4x4','')
    print (command)
    ret=os.system(command)

