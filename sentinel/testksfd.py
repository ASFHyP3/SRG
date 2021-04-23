#!/usr/bin/env python3

import sys
import os
import math
import string

import subprocess

ver=sys.version_info

    


    

if len(sys.argv) <  2:
    print 'Usage: testksfd.py length'
    sys.exit(1)

length=sys.argv[1]

fburst=open('filelistburst','r')
burst=fburst.readlines()
fburst.close()

for line in burst:
    words=line.split()
    burstfile=words[-1]
    command = '$PROC_HOME/burst_ks_fd '+burstfile+' '+length
    print (command)
    proc = subprocess.Popen(command, stdout=subprocess.PIPE, shell=True)
    (geos, err) = proc.communicate()
    print 'results ',geos

sys.exit(0)

