#!/usr/bin/env python3

import sys
import os
import math
import string

import subprocess

ver=sys.version_info

    


    

if len(sys.argv) <  2:
    print 'Usage: test_fdcoefs.py <length=value from real aperture proc>'
    sys.exit(1)

print len(sys.argv)

if len(sys.argv) > 1:
    length=sys.argv[1]

#command = 'ls -1 burst* | cat > filelistburst'
#print (command)
#ret = os.system(command)

fburst=open('filelistburst','r')
burst=fburst.readlines()
fburst.close()

i=0
for line in burst:
    words=line.split()
    burstfile=words[-1]    
    command='$PROC_HOME/testfdcoefs '+burstfile+' '+length
    print (command)
    ret=os.system(command)
    i=i+1
