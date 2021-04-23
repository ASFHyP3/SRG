#!/usr/bin/env python3
#
#  do the decoding and sorting of raw data for sentinel backprojection processing

import sys
import os
import subprocess

ver=sys.version_info

    


    

if len(sys.argv) < 3:
    print 'Usage: sentinel_scene.py SAFEname orbitfile(*EOF) <vv or vh (def. vv)>'

print 'Processing stack of sentinel raw data products to coregistered geocoded slcs'

SAFEname=sys.argv[1]
orbitfile=sys.argv[2]
pol='vv'
if len(sys.argv) > 3:
    pol=sys.argv[3]
    if pol == 'VH':
        pol='vh'

    print 'Processing ',pol,' polarization'

#print SAFEname,' ',orbitfile,' ',pol

# clean up first if necessary
command = 'rm raw*' # swath* times*'
print (command)
ret=os.system(command)

# find the basename
command = 'ls '+SAFEname+'.SAFE/*'+pol+'*dat | grep -v annot | grep -v index'
print (command)
proc = subprocess.Popen(command, stdout=subprocess.PIPE, shell=True)
(datfile, err) = proc.communicate()
basename=datfile[0:len(datfile)-5]
# decode raw data
command = '$PROC_HOME/sentinel_raw_memory '+basename
print (command)
ret=os.system(command)
#  sort the swaths (not needed after code consolidation)
#command = '$PROC_HOME/sortswath raw.dat swath.txt'
#print (command)
#ret=os.system(command)

# initialize the slc file
fe=open('elevation.dem.rsc','r')
words=fe.readline()
demwidth=words.split()[1]
words=fe.readline()
demlength=words.split()[1]
fe.close()
#print demwidth, demlength
command = '$PROC_HOME/createslc '+demwidth+' '+demlength
print (command)
ret=os.system(command)

# loop over swaths
i=0
for swath in range(3):
    i=i+1
    # some cleanup
    command = 'rm burst* db*'
    print (command)
    ret=os.system(command)
    # range compress
#    command = '$PROC_HOME/sortbursts raw.'+str(i)+'.dat'
#    print (command)
#    ret=os.system(command)
    command = '$PROC_HOME/process_realaperture raw.'+str(i)+'.dat'
    print (command)
    ret=os.system(command)
    # read number of samples
    fp=open('rangesamples','r')
    length=fp.readline()
    fp.close()
    # preprocess the swath
    command = '$PROC_HOME/sentinel_preproc.py '+orbitfile+' '+length
    print (command)
    ret=os.system(command)
    # backproject
    command = '$PROC_HOME/sentinel_back.py '+orbitfile
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









