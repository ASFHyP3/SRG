#!/usr/bin/env python3
#
#  download precise orbit files for zipfiles in directory
#

import sys
import os
import string
import time
import subprocess
from datetime import datetime

# environment setup

# get the current environment
#HOME = os.environ['MYHOME']+'/sentinel'

if len(sys.argv) < 1:
    print ('Usage: sentinel_orbitfiles.py')

#print ('Downloading sentinel precise orbit files for zipfiles in this directory')

# variables for parallel downloading
# get list of zip files
num=0
EOFlist=[]
prochome = os.getenv('PROC_HOME')
zipfiles = []
for file in os.listdir("."):
    if file.endswith(".zip"):
#            print (file)
            mission=file[0:3]
            if file.find("SDV_") > 0:
                acquisitiondate=file[file.find("SDV_")+4:file.find("SDV_")+12]
            if file.find("SSV_") > 0:
                acquisitiondate=file[file.find("SSV_")+4:file.find("SSV_")+12]
            if file.find("SDH_") > 0:
                acquisitiondate=file[file.find("SDH_")+4:file.find("SDH_")+12]
            if file.find("SSH_") > 0:
                acquisitiondate=file[file.find("SSH_")+4:file.find("SSH_")+12]

#            print (mission)
#            print (acquisitiondate)
#            print (os.environ['PROC_HOME'])
            command = os.environ['PROC_HOME']+"/EOFrequests/getEOF.py "+acquisitiondate+" "+mission
            print (command)
            # ret = os.system(command)
            EOFlist.append(subprocess.Popen([prochome+'/EOFrequests/getEOF.py',acquisitiondate,mission]))
            num=num+1

for i in range(num):
    EOFlist[i].wait()

print ('EOF files downloaded')
