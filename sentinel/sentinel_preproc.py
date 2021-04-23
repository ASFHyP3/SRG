#!/usr/bin/env python3

# some geometry and timing setup material

import sys
import os
import math
import string

import subprocess

ver=sys.version_info

    


    

if len(sys.argv) < 2:
    print ('Usage: sentinel_preproc.py preciseorbitfile(*EOF) length')
    sys.exit(1)

preciseorbitfile=sys.argv[1]
length=sys.argv[2]

# Create a 'params' file
params=open("params","w")
p1 = os.getcwd()+"/elevation.dem"
p2 = os.getcwd()+"/elevation.dem.rsc"
params.write(p1+'\n')
params.write(p2+'\n')
params.close()
print ("DEM file set to elevation.dem")
print ("RSC file set to elevation.dem.rsc")

# create orbtiming file for entire pass
command = '$PROC_HOME/precise_orbit_burst.py '+preciseorbitfile+' '+length
print (command)
ret = os.system(command)

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
    command = ' $PROC_HOME/sentineltiming precise_orbtiming '+burstfile+' '+length
#    print (command)
    ret = os.system(command)
    command='mv position.out position'+burstfile+'.out'
#    print (command)
    ret = os.system(command)
    #  create a db file for each
    command = '$PROC_HOME/burst_roidb '+burstfile+' db.'+burstfile+' precise_orbtiming '+'position'+burstfile+'.out '+length
#    print (command)
    ret = os.system(command)
    #  calibrate r0 for backprojection based on hawaii 30m data
    command = '$PROC_HOME/increment_param_db.py db.'+burstfile+' file r0 '+str(75)
    print (command)
    ret=os.system(command)
    i=i+1

#  get limits for bursts
command = '$PROC_HOME/tops_bounds precise_orbtiming'
#print (command)
ret = os.system(command)

sys.exit(0)
