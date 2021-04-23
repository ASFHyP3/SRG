#!/usr/bin/env python3
#
#  do the decoding and sorting of raw data for sentinel backprojection processing

import sys
import os
import subprocess

if len(sys.argv) < 3:
    print ('Usage: sentinel_scene.py SAFEname orbitfile(*EOF) <vv or vh (def. vv)>')

print ('Processing stack of sentinel raw data products to coregistered geocoded slcs')

SAFEname=sys.argv[1]
orbitfile=sys.argv[2]
pol='vv'
if len(sys.argv) > 3:
    pol=sys.argv[3]
    if pol == 'VH':
        pol='vh'

    print ('Processing ',pol,' polarization')

#print SAFEname,' ',orbitfile,' ',pol

# clean up first if necessary
#command = 'rm raw*' # swath* times*'
#print (command)
#ret=os.system(command)

# find the basename
command = 'ls '+SAFEname+'.SAFE/*'+pol+'*dat | grep -v annot | grep -v index'
print (command)
proc = subprocess.Popen(command, stdout=subprocess.PIPE, shell=True)
(datfile, err) = proc.communicate()
basename=str(datfile[0:len(datfile)-5],'UTF-8')

# read the orbitfile statevectors, store in orbtiming.full
command = '$PROC_HOME/sentinel/orbitstatevectors.py '+orbitfile
print (command)
ret=os.system(command)

# initialize the slc file
fe=open('elevation.dem.rsc','r')
words=fe.readline()
demwidth=words.split()[1]
words=fe.readline()
demlength=words.split()[1]
fe.close()
#print demwidth, demlength
command = '$PROC_HOME/sentinel/createslc '+demwidth+' '+demlength
print (command)
ret=os.system(command)

#  process the scene, three swaths
command = '$PROC_HOME/sentinel/sentinel_raw_process '+basename
print (command)
ret=os.system(command)

#  save slc file
command = 'mv slc '+SAFEname+'.geo'
print (command)
ret=os.system(command)

#  and save precise_orbtiming file for later use
command = 'cp precise_orbtiming '+SAFEname+'.orbtiming'
print (command)
ret=os.system(command)

sys.exit(0)




