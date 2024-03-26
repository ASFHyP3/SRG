#!/usr/bin/env python3
#
#  process stack of sentinel files to coregistered geocoded slcs from L0 data
#  then store geocoded slc product in the google cloud
#  
#  First, either download the zip files you want to process, then start this script:
#
#  sentinel_gpu_cloud.py
#
#  backproject using gpu integration
#
#  Or, create a file with the suffix ".list" that contains the zip file granule names (i.e., starts with "S1A..." or "S1B...")
#  on each line.  If not logged in you willbe prompted for username/password in vertex/Earthdata system
#

import sys
import os
import string
import time
import subprocess
from datetime import datetime

# get the current environment
HOME = os.environ['MYHOME']+'/sentinel'

if len(sys.argv) < 1:
    print ('Usage: sentinel_gpu.py <vv or vh (def. vv)>')
    print

pol='vv'
if len(sys.argv) > 1:
    pol=sys.argv[1]
    if pol == 'VH':
        pol='vh'

    print ('Processing ',pol,' polarization')

# and start
print ('Processing stack of sentinel raw data products to coregistered geocoded slcs')

# get the PATH of the script directory
PATH=os.path.dirname(os.path.abspath(sys.argv[0]))

# Create a 'params' file
params=open("params","w")
p1 = os.getcwd()+"/elevation.dem"
p2 = os.getcwd()+"/elevation.dem.rsc"
params.write(p1+'\n')
params.write(p2+'\n')
params.close()
print ("DEM file set to elevation.dem")
print ("RSC file set to elevation.dem.rsc")

# get list of L0 products
fziplist=open('zipfiles','w')
zipfiles = []
SAFEnames = []
num=0
unzipcommand=[]
for file in os.listdir("."):
#    print (file)
    if file.endswith(".zip"):
            #print file
            zipfiles.append(file)
            unzipcommand.append(subprocess.Popen(['unzip', '-u',file]))
            SAFEnames.append(file[0:len(file)-4])
#            command = 'unzip -u '+file
#            print (command)
#            ret = os.system(command)
            SAFEnames.append(file[0:len(file)-4])
            fziplist.write(file+'\n')
            num=num+1

for i in range(num):
    unzipcommand[i].wait()

print ("zipfiles: ",zipfiles)
print ("basenames: ",SAFEnames)
fziplist.close()

# download the precise orbit files for these zips
command = HOME+'/sentinel_orbitfiles.py'
print (command)
ret=os.system(command)

# list the precise orbit files
ret=os.system('ls -1 *.EOF | cat > preciseorbitfiles')
preciseorbitfiles=open('preciseorbitfiles','r')
preciseorbitlist=preciseorbitfiles.readlines()
preciseorbitfiles.close()

print ('Precise orbit list:')
print (preciseorbitlist)

# process in parallel

#  how many gpus do we have?
proc = subprocess.Popen("$PROC_HOME/sentinel/howmanygpus",stdout=subprocess.PIPE, shell=True)
(param,err)=proc.communicate()
ngpus=str(param,'UTF-8').split()[0]
print ('gpus available: ',ngpus)

#proc = subprocess.Popen("nvidia-smi | grep 'NVIDIA ' | wc -l",stdout=subprocess#.PIPE, shell=True)
#(bytearray,err)=proc.communicate()
#ngpus=int(bytearray.decode())
#print (ngpus)
#ngpus=1

command='$PROC_HOME/sentinel/process_parallel.py zipfiles '+str(ngpus)
print (command)
ret=os.system(command)

print ('Loop over scenes complete.')

#  clean up a bit
command = 'find . -name \*positionburst\* -delete'
ret=os.system(command)







