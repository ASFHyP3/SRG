#!/usr/bin/env python3
#
#
#  process_parallel.py -- process multiple scenes in parallel, depending on number of gpus
#
#   note: assume that ziplist exists
#

import os
import subprocess
import sys

if len(sys.argv) <2:
    print ('Usage: process_parallel.py <filelist=ziplist> <ngpus=1> <pol=vv>')
    sys.exit(0)

zipfiles='ziplist'
if len(sys.argv)>1:
   zipfiles = sys.argv[1]
ngpus='1'
if len(sys.argv)>2:
   ngpus = sys.argv[2]
pol='vv'
if len(sys.argv)>3:
   pol = sys.argv[3]

print ('process_parallel args: ',zipfiles,ngpus,pol)

# first create multiple lists of zipfiles
command = '$PROC_HOME/util/splitziplist.py '+zipfiles+' '+ngpus  # split zips into many lists
print (command)
ret = os.system(command)

# how many ziplists do you form?
command = 'ls -1 ziplist* | cat > listofziplists'  # split into many lists
print (command)
ret = os.system(command)

flist = open('listofziplists','r')
list = flist.readlines()
flist.close()

# do we need to create a dem?  If so do so
create_dem=1
for file in os.listdir('.'):
    if file == 'elevation.dem':
        create_dem=0

print ('create_dem= ',create_dem)

if create_dem==1:
    fziplist=open(zipfiles,'r')
    firstzipfile=fziplist.readline()
    fziplist.close()
    firstzipfile=firstzipfile.strip()
    SAFEname=firstzipfile[0:len(firstzipfile)-4]
#    print ('first SAFEname: ',SAFEname)
    command="$PROC_HOME/sentinel/sentinel_coordinates_srtm30.py "+SAFEname  # nasadem
    command="$PROC_HOME/sentinel/sentinel_coordinates_copernicus.py "+SAFEname
    print (command)
    ret=os.system(command)

# process in parallel

num=0
unwcommand=[]
prochome = os.getenv('PROC_HOME')
for ziplist in list:
    if ziplist.rstrip() != 'ziplist':
        if os.path.getsize(ziplist.rstrip()) > 0:
            command = prochome+'/sentinel/process_zip_list.py '+ziplist.rstrip()+' '+str(num)
            print (command)
            command = prochome+'/sentinel/process_zip_list.py'
            print (command)
            #ret = os.system(command)
            unwcommand.append(subprocess.Popen([command,ziplist.rstrip(),str(num)]))
            num=num+1

for i in range(num):
    unwcommand[i].wait()

print('geo files created')

#  clean up a bit
#command = 'rm *.hgt* DEM* q*'
#command = 'rm *.hgt* positionburst* dem* DEM* q*'




