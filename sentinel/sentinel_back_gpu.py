#!/usr/bin/env python3

import sys
import os
import math
import string

import subprocess

ver=sys.version_info

    


    

if len(sys.argv) <  2:
    print 'Usage: sentinel_back_gpu.py preciseorbitfile(*EOF) <length=value from real aperture proc>'
    sys.exit(1)

print len(sys.argv)

preciseorbitfile=sys.argv[1]
if len(sys.argv) > 2:
    length=sys.argv[2]
if len(sys.argv) == 2:
    #print 'reading rangesamples'
    fp=open('rangesamples','r')
    length=fp.readline()
    fp.close()

#  read in latlon bounds
fll=open('latlonlimits','r')
ll=fll.readlines()
fll.close()
latlons1=[]
latlons2=[]
latlons3=[]
latlons4=[]
for line in ll:
    words=line.split()
    latlons1.append(words[1])
    latlons2.append(words[2])
    latlons3.append(words[3])
    latlons4.append(words[4])

# create a position file for each burst
filelistburst = 'filelistburst'
command = 'ls -1 burst* | cat > '+filelistburst
print (command)
ret = os.system(command)

fburst=open('filelistburst','r')
burst=fburst.readlines()
fburst.close()

i=0
burstname=[]
for line in burst:
    words=line.split()
    burstfile=words[-1]    
    burstname.append(burstfile)
    if i == 0:
        commandbackproject = '$PROC_HOME/backprojectgpu slc db.'+burstname[i]
    if i>0:
        commandbackproject = commandbackproject+' db.'+burstname[i]
    
    i=i+1

print (command)backproject
ret = os.system(commandbackproject)
#answer = raw_input("finished a burst, you can exit now with ^z")

sys.exit(0)

