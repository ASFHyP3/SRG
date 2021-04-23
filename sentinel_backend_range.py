#!/usr/bin/env python3
#
#
#  sentinel_backend.py -- post-igram processing for sentinel time series
#
#

import os
import subprocess
import sys
import time

if len(sys.argv)<4:
    print ( 'Usage: sentinel_backend_range.py min, max timebaselines min, max spatialbaselines ref-threshold')
    sys.exit(0)

mintimebaseline=sys.argv[1]
maxtimebaseline=sys.argv[2]
minspatialbaseline=sys.argv[3]
maxspatialbaseline=sys.argv[4]
thresh=sys.argv[5]

print ('You are creating a stack with baselines '+mintimebaseline+', '+maxtimebaseline+', '+minspatialbaseline+', '+maxspatialbaseline+' with threshold '+thresh+'\n\n')

# create appropriate sbas list
command = '$PROC_HOME/sentinel/sbas_list_range.py '+mintimebaseline+' '+maxtimebaseline+' '+minspatialbaseline+' '+maxspatialbaseline
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

# troposphere correction using regression vs elevation
command = '$PROC_HOME/sentinel/int/tropocorrect.py unwlist '+unwwidth+' '+unwlength+' '+thresh
print (command)
ret = os.system(command)

# compute sbas velocity solution
command = '$PROC_HOME/sbas/sbas unwlist '+nunwfiles.rstrip()+' '+nslcs.rstrip()+' '+unwwidth+' ref_locs'
print (command)
ret = os.system(command)

print('Timeseries generated')
