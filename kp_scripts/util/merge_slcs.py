#!/usr/bin/env python3
#
#  merge_slcs.py - merge slcs when same pass has multiple acquisitions
#

import sys
import os
import subprocess

if len(sys.argv) < 2:
    print ("Usage: merge_slcs.py path_to_merged <update_flag=N>")
    sys.exit(0)

path_to_merged = sys.argv[1]

# Defaults
update_flag = 'N'

# Get Optional Inputs
if len(sys.argv)>2:
    update_flag = sys.argv[2]

# size of geofiles
fparams = open('params','r')
demfile = fparams.readline().rstrip()
rscfile = fparams.readline().rstrip()
fparams.close()
fe=open(rscfile,'r')
words=fe.readline()
demwidth=words.split()[1]
words=fe.readline()
demlength=words.split()[1]
fe.close()

# make a list with all geo slc dates
command = 'ls -1 *.geo'
proc = subprocess.Popen(command, stdout=subprocess.PIPE, shell=True)
(geolist, err) = proc.communicate()
geofiles = geolist.split()
geofiles.sort()  # put in alphabetical order

datelist=[]
dategeo=[]
for i in range(len(geofiles)):
    geostring=str(geofiles[i],'UTF-8')
    date0=geostring[geostring.find('V_')+2:geostring.find('V_')+10]
    pol='V'
    if date0[3:5] == 'IW':
        pol='H'
    dategeo.append(geostring[geostring.find(pol+'_')+2:geostring.find(pol+'_')+10])
    if dategeo[i] in datelist:
        print (dategeo[i],' is in datelist')
        
    if dategeo[i] not in datelist:
        print (dategeo[i],' is not in list')
        datelist.append(dategeo[i])

# create a new list with all geos for a date
if (os.path.isfile('merged_list')) and (update_flag=='N'):
    fmerge=open('merged_list','r')
    merged=fmerge.readlines()
    fmerge.close()
    fmerge=open('merged_list','a')
else:
    fmerge=open('merged_list','w')

for i in range(len(datelist)):
    indices = [j for j, x in enumerate(dategeo) if x == datelist[i]]
    geostringout = path_to_merged+'/'+datelist[i]+'_merged.geo'
    orbtimingout = geostringout.replace('.geo','.orbtiming')
    if os.path.isdir(path_to_merged)==False:
        command='mkdir '+path_to_merged
        ret=os.system(command)
   
    if len(indices) > 1:
        check=0
        mergelist = []
        # check to see if all of the files have already been merged
        for j in range(len(indices)):
            geostring = str(geofiles[indices[j]],'UTF-8')
            if geostring+'\n' not in merged:
                mergelist.append(geostring)
                check=1
        if check==0:
            print('Already merged all files for date '+datelist[i])
            continue

        if os.path.isfile(geostringout):
            for j in range(len(mergelist)):
                geostring = mergelist[j]
                command = '$PROC_HOME/util/mergeslcs '+geostringout+' '+geostring+' '+geostringout.replace('.geo','.temp')+' '+demwidth+' '+demlength
                print(command)
                ret=os.system(command)
                # rename the outfile
                command = 'mv '+geostringout.replace('.geo','.temp')+' '+geostringout
                print(command)
                ret=os.system(command)
                # make sure we have a matching orbtiming file
                if os.path.isfile(orbtimingout)==False:
                    command = 'cp '+geostring.replace('.geo','.orbtiming')+' '+orbtimingout
                    print(command)
                    ret=os.system(command)
                fmerge.write(geostring+'\n')
        else:
            geostring1 = mergelist[0]
            for j in range(len(mergelist)-1):
                geostring2=mergelist[j+1]
                command = '$PROC_HOME/util/mergeslcs '+geostring1+' '+geostring2+' '+geostringout.replace('.geo','.temp')+' '+demwidth+' '+demlength
                print(command)
                ret=os.system(command)
                # rename the outfile
                command = 'mv '+geostringout.replace('.geo','.temp')+' '+geostringout
                print(command)
                ret=os.system(command)
                # make sure we have a matching orbtiming file
                if os.path.isfile(orbtimingout)==False:
                    command = 'cp '+geostring.replace('.geo','.orbtiming')+' '+orbtimingout
                    print(command)
                    ret=os.system(command)
                if j==0:
                    fmerge.write(geostring1+'\n')
                fmerge.write(geostring2+'\n')

                geostring1 = geostringout                
    else:
        if os.path.isfile(geostringout)==False:
            geostring = str(geofiles[indices[0]],'UTF-8')
            command = 'cp '+geostring+' '+geostringout
            print(command)
            ret=os.system(command)
            fmerge.write(geostring+'\n')
        if os.path.isfile(orbtimingout)==False:   
            command = 'cp '+geostring.replace('.geo','.orbtiming')+' '+orbtimingout
            print(command)
            ret=os.system(command)
            
fmerge.close()
sys.exit(0)
