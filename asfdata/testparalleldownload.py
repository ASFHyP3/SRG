#!/usr/bin/python
#
#
# test downloading in parallel
#

import os
import sys
import subprocess

print (len(sys.argv))
if len(sys.argv) < 2:
    print ("usage: testparalleldownload.py scenelist\n","  scenelist contains granule names starting with S1(A,B)")
    sys.exit(0)

scenefile=sys.argv[1]
fscenes=open(scenefile,'r')
scenelist=fscenes.readlines()
fscenes.close()

# get credentials and remove the file for security
fcred=open('.credentials','r')
username=fcred.readline().rstrip()
password=fcred.readline().rstrip()
fcred.close()
os.remove('.credentials')

AB=''
num=0
command=[]
for scene in scenelist:
    if len(scene)>1:
        if scene.find('S1A')>=0:
            AB='A'
        if scene.find('S1B')>=0:
            AB='B'
        print ('Downloading scene: ',scene.rstrip())
        command.append(subprocess.Popen(['wget','-qN','--user='+username,'--password='+password,'https://datapool.asf.alaska.edu/RAW/S'+AB+'/'+scene.replace('-RAW','').rstrip()+'.zip','--show-progress']))
        num=num+1

for i in range(num):
    command[i].wait()

print('Download complete.\n')

sys.exit()
