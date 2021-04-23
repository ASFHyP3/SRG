#!/usr/bin/env python3

import sys
import os
import math
import string

import subprocess

ver=sys.version_info

    


    

if len(sys.argv) <  2:
    print 'Usage: sentinel_back.py preciseorbitfile(*EOF) <length=value from real aperture proc>'
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
    #print words
    latlons1.append(words[1])
    latlons2.append(words[2])
    latlons3.append(words[3])
    latlons4.append(words[4])

#print ll
#print latlons1
#print latlons2
#print latlons3
#print latlons4


# create a position file for each burst
filelistburst = 'filelistburst'
command = 'ls -1 burst* | cat > '+filelistburst
print (command)
ret = os.system(command)

fburst=open('filelistburst','r')
burst=fburst.readlines()
fburst.close()
i=0

for line in burst:
    words=line.split()
    burstfile=words[-1]
    command = '$PROC_HOME/burst_angle_steering '+burstfile+' '+length
    print (command)
    proc = subprocess.Popen(command, stdout=subprocess.PIPE, shell=True)
    (geos, err) = proc.communicate()
    #print 'results ',geos
    #  and backproject
    command = '$PROC_HOME/backproject db.'+burstfile+' slc '+geos
    print (command)
    ret = os.system(command)
    i=i+1

sys.exit(0)

