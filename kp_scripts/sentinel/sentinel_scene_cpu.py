#!/usr/bin/env python3
#
#  do the decoding and sorting of raw data for sentinel backprojection processing

import sys
import os
import subprocess

if len(sys.argv) < 4:
    print ('Usage: sentinel_scene_cpu.py SAFEname orbitfile(*EOF) params_file <vv or vh (def. vv)>')

print ('Processing stack of sentinel raw data products to coregistered geocoded slcs')

SAFEname=sys.argv[1]
orbitfile=sys.argv[2]
params=sys.argv[3]
pol='vv'
if len(sys.argv) > 4:
    pol=sys.argv[4]
    if pol == 'VH':
        pol='vh'

    print ('Processing ',pol,' polarization')


# find the basename
command = 'ls '+SAFEname+'.SAFE/*'+pol+'*dat | grep -v annot | grep -v index'
print (command)
proc = subprocess.Popen(command, stdout=subprocess.PIPE, shell=True)
(datfile, err) = proc.communicate()
basename=str(datfile[0:len(datfile)-5],'UTF-8')

# read the orbitfile statevectors, store in orbtiming.full
command = '$PROC_HOME/sentinel/orbitstatevectors.py '+orbitfile+' '+SAFEname
print (command)
ret=os.system(command)

# extract path to DEM and resource file
fparam = open(params,'r')
demfile = fparam.readline().strip()
rscfile = fparam.readline().strip()
fparam.close()

# initialize the slc file
fe=open(rscfile,'r')
words=fe.readline()
demwidth=words.split()[1]
words=fe.readline()
demlength=words.split()[1]
fe.close()

command = '$PROC_HOME/sentinel/createslc '+demwidth+' '+demlength+' '+SAFEname+'.geo'
print (command)
ret=os.system(command)

#  process the scene, three swaths
command = '$PROC_HOME/sentinel//sentinel_raw_process_cpu '+basename
print (command)
ret=os.system(command)

sys.exit(0)


