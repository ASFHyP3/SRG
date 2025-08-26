#!/usr/bin/env python3
#
#
#  asfwget - download set of asf raw sentinel products
#
#   command template: 
#  "https://datapool.asf.alaska.edu/RAW/SA/S1A_IW_RAW__0SDV_20200209T043049_20200209T043122_031171_039571_0730.zip"
#

import os
import sys
import subprocess

##### ----- DEFUALT PARAMETERS ----- #####
scenefile = 'scenelist'
redownloadflag = 'N'
npar=32 # number to download in parallel

if len(sys.argv) < 3:
    print ("usage: asfwget_notebook_kp.py username password <scenelist='scenelist'> <redownload_flag='N'> <npar=32>\n","  scenelist contains granule names starting with S1(A,B)")
    sys.exit(0)

##### ----- GET PARAMETERS ----- #####
username=sys.argv[1]
password=sys.argv[2]

if len(sys.argv)>3:
    scenefile=sys.argv[3]
if len(sys.argv)>4:
    redownloadflag = sys.argv[4]
if len(sys.argv)>5:
    npar=sys.argv[5]
    npar = int(npar)

##### ----- OPEN FILELIST with FILES TO DOWNLOAD ----- #####
fscenes=open(scenefile,'r')
scenelist=fscenes.readlines()
fscenes.close()

##### ----- OPEN REFERENCE FILE (FILES THAT HAVE ALREADY BEEN DOWNLOADED ----- #####
downloadfile = 'asf_downloaded'
existing = []
if os.path.isfile(downloadfile):
    with open(downloadfile,'r') as flist:
        existing=flist.readlines()
    flist=open(downloadfile,'a')
else:
    flist=open(downloadfile,'w')

##### ----- DOWNLOAD THE FILES IN SCENELIST ----- #####
AB=''
num=0
command=[]
for scene in scenelist:

    # check if file has already been downloaded
    if redownloadflag == 'N':
        if scene.rstrip()+'\n' in existing:
            continue

    # if redownload flag = yes or the file doesn't exist, download the file
    if len(scene)>1:
        # Check to make sure the file doesn't still exist
        filename = scene.replace('-RAW','').strip()+'.zip'
        if os.path.isfile(filename):
            continue

        if scene.find('S1A')>=0:
            AB='A'
        if scene.find('S1B')>=0:
            AB='B'
       
        print ('Downloading scene: ',scene.rstrip())
        command.append(subprocess.Popen(['wget','-qN','--user='+username,'--password='+password,'https://datapool.asf.alaska.edu/RAW/S'+AB+'/'+scene.replace('-RAW','').rstrip()+'.zip']))
        num=num+1
        flist.write(scene.rstrip()+'\n')
        if num % npar == 0:
            for i in range(num-npar,num):
                command[i].wait()

for i in range(num):
    command[i].wait()

flist.close()
print('\n    ASF Download complete!\n')

sys.exit()
