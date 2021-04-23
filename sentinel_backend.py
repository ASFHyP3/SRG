#!/usr/bin/env python3
#
#
#  sentinel_backend.py -- post-igram processing for sentinel time series
#
#    optionally skip tropo correction for tests

import os
import subprocess
import sys
import time

if len(sys.argv)<4:
    print ( 'Usage: sentinel_backend.py timebaseline spatialbaseline ref-threshold <tropo corr y/n = y>')
    sys.exit(0)

timebaseline=sys.argv[1]
spatialbaseline=sys.argv[2]
thresh=sys.argv[3]
tropo='y'
if len(sys.argv)>4:
    tropo=sys.argv[4]

print ('You are creating a stack with baselines '+timebaseline+', '+spatialbaseline+' with threshold '+thresh+'\n\n')
print ('Tropo correction: ',tropo)

# create appropriate sbas list
command = '$PROC_HOME/sentinel/sbas_list.py '+timebaseline+' '+spatialbaseline
print (command)
ret = os.system(command)

# update intlist with new baseline limits
command = '$PROC_HOME/sentinel/new_intlist.py sbas_list'
print (command)
ret = os.system(command)

# size of multilooked int and unw files from .rsc file
fe=open('dem.rsc','r')
words=fe.readline()
unwwidth=words.split()[1]
words=fe.readline()
unwlength=words.split()[1]
fe.close()

# set up sbas ancillary files
command = '$PROC_HOME/sbas/sbas_setup.py sbas_list geolist'
print (command)
ret = os.system(command)

# gather parameters for sbas calculation
ret = os.system('cp intlist unwlist')
ret = os.system("sed -i 's/int/unw/g' unwlist")

proc = subprocess.Popen("wc -l < unwlist",stdout=subprocess.PIPE, shell=True)
(nunwfiles,err)=proc.communicate()
proc = subprocess.Popen("wc -l < geolist",stdout=subprocess.PIPE, shell=True)
(nslcs,err)=proc.communicate()
nunwfiles=str(nunwfiles,'UTF-8').rstrip()
nslcs=str(nslcs,'UTF-8').rstrip()

# troposphere correction using regression vs elevation
if tropo == 'y':
    command = '$PROC_HOME/int/tropocorrect.py unwlist '+unwwidth+' '+unwlength+' '+thresh
    print (command)
    ret = os.system(command)

# compute sbas velocity solution
if float(thresh) < 0.999:
    command = '$PROC_HOME/sbas/sbas unwlist '+nunwfiles+' '+nslcs+' '+unwwidth+' ref_locs'
else:
    command = '$PROC_HOME/sbas/sbas unwlist '+nunwfiles+' '+nslcs+' '+unwwidth

print (command)
ret = os.system(command)

print('Timeseries generated')
