#!/usr/bin/env python3

import sys
import os
import math
import string
import subprocess

ver=sys.version_info

    


    

if len(sys.argv) <  6:
    print 'Usage: triplets.py length lines scale looksac looksdn'
    sys.exit(1)

# start with a list of *.geo files
command = 'ls -1 ../*.geo'
print (command)
proc = subprocess.Popen(command, stdout=subprocess.PIPE, shell=True)
(filelist, err) = proc.communicate()
filename=filelist.split()
nfiles=len(filename)

length=sys.argv[1]
lines=sys.argv[2]
scale=sys.argv[3]
looksac=sys.argv[4]
looksdn=sys.argv[5]

for i in range(nfiles-2):
    file1=filename[i]
    file2=filename[i+1]
    file3=filename[i+2]
    date1=file1[file1.find('20'):file1.find('20')+8]
    date2=file2[file2.find('20'):file2.find('20')+8]
    date3=file3[file3.find('20'):file3.find('20')+8]
    command = '$PROC_HOME/int/triplet.py '+file1+' '+file2+' '+file3+' '+length+' '+lines+' '+scale+' '+looksac+' '+looksdn
    print (command)
    ret=os.system(command)
    command = 'mv tripleint '+date1+date2+date3+'.int'
    print (command)
    ret=os.system(command)
    command = 'mv cc12 '+date1+date2+'.cc'
    ret=os.system(command)
    command = 'mv cc13 '+date1+date3+'.cc'
    ret=os.system(command)
    command = 'mv cc32 '+date3+date2+'.cc'
    ret=os.system(command)
    command = 'mv qint12 '+date1+date2+'.int'
    ret=os.system(command)
    command = 'mv qint13 '+date1+date3+'.int'
    ret=os.system(command)
    command = 'mv qint32 '+date3+date2+'.int'
    ret=os.system(command)

    # unwrap triplet
    command = '$PROC_HOME/bin/snaphu '+date1+date2+date3+'.int '+str(int(int(length)/int(looksac)))+' -d -o '+date1+date2+date3+'.unw -c '+date1+date2+'.cc --mcf' 
    print (command)
    ret=os.system(command)

sys.exit()
