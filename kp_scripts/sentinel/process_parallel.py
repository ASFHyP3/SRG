#!/usr/bin/env python3
#
#
#  process_parallel_kp.py -- process multiple scenes in parallel, depending on number of gpus
#
#   note: assume that zipfiles exists
#

import os
import subprocess
import sys

if len(sys.argv) <2:
    print ('Usage: process_parallel_kp.py <filelist=zipfiles> <ngpus=1> <pol=vv>')
    sys.exit(0)

# Default Parameters
zipfiles='zipfiles'
ngpus='1'
pol='vv'

# Get Optional Parameters
if len(sys.argv)>1:
   zipfiles = sys.argv[1]

if len(sys.argv)>2:
   ngpus = sys.argv[2]

if len(sys.argv)>3:
   pol = sys.argv[3]

print ('    process_parallel_kp args: ',zipfiles,ngpus)

# first create multiple lists of zipfiles
command = '$PROC_HOME/util/splitziplist.py '+zipfiles+' '+ngpus  # split zips into many lists
print('\n    '+command)
ret = os.system(command)

# how many ziplists do you form?
command = 'ls -1 ziplist* | cat > listofziplists'  # split into many lists
print('\n    '+command)
ret = os.system(command)

flist = open('listofziplists','r')
list = flist.readlines()
flist.close()

##### ---- Process in Parallel ----- #####
num=0
unwcommand=[]
prochome = os.getenv('PROC_HOME')
for ziplist in list:
    if ziplist.rstrip() != 'ziplist':
        if os.path.getsize(ziplist.rstrip()) > 0:
            command = prochome+'/sentinel/process_zip_list.py '+ziplist.rstrip()+' '+str(num)
            print('    '+command)
            command = prochome+'/sentinel/process_zip_list.py'
            unwcommand.append(subprocess.Popen([command,ziplist.rstrip(),str(num)]))
            num=num+1

for i in range(num):
    unwcommand[i].wait()

print('geo files created')




