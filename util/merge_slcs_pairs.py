#!/usr/bin/env python3
#
#  merge_slcs - merge slcs when same pass has two acquisitions
#

import sys
import os
import subprocess

if len(sys.argv) < 1:
    print ("Usage: merge_slcs.py")
    sys.exit(0)

# get all geo files
command = 'ls -1 *.geo'
proc = subprocess.Popen(command, stdout=subprocess.PIPE, shell=True)
(geolist, err) = proc.communicate()

geofiles = geolist.split()

geofiles.sort()  # put in alphabetical order
print ('Number of files: ',len(geofiles))

# size of geofiles
fe=open('elevation.dem.rsc','r')
words=fe.readline()
demwidth=words.split()[1]
words=fe.readline()
demlength=words.split()[1]
fe.close()

# remove any pre-existing merged geo files
for i in range(len(geofiles)):
    #  date from a geo file
    geofile0=str(geofiles[i],'UTF-8')
    date0=geofile0[geofile0.find('V_')+2:geofile0.find('V_')+10]
    print ('date0 ',date0,date0[3:5])
    if date0[3:5] == 'IW':
        print ('invalid date')
        pol='H'
    # is this a valid date?
    date0=geofile0[geofile0.find('H_')+2:geofile0.find('H_')+10]
    print ('date0 ',date0)
    if date0[3:5] == 'IW':
        print ('invalid date')
        pol='V'
    print ('polarization ',pol)

    #  any others with same date?
    for j in range(len(geofiles)):
        if j != i:
            geofile1=str(geofiles[j],'UTF-8')
            date1=geofile1[geofile1.find(pol+'V_')+2:geofile1.find(pol+'V_')+10]
            if date0 == date1:
                print ('Match ',geofile0,geofile1)
                time0start=geofile0[geofile0.find(pol+'_')+11:geofile0.find(pol+'_')+17]
                time0end=geofile0[geofile0.find(pol+'_')+27:geofile0.find(pol+'_')+33]
                time1start=geofile1[geofile1.find(pol+'_')+11:geofile1.find(pol+'_')+17]
                time1end=geofile1[geofile1.find(pol+'_')+27:geofile1.find(pol+'_')+33]
                print ('times ',time0start,time0end,time1start,time1end)
                # if start times match, remove one with later end time
                if time0start == time1start:
                    if int(time0end) > int(time1end):
                        os.system('mv '+geofile0+' orig_geos/')
                    if int(time1end) > int(time0end):
                        os.system('mv '+geofile1+' orig_geos/')

# and rescan remaining geos for merging
command = 'ls -1 *.geo'
proc = subprocess.Popen(command, stdout=subprocess.PIPE, shell=True)
(geolist, err) = proc.communicate()
geofiles = geolist.split()
geofiles.sort()  # put in alphabetical order
print ('Number of files: ',len(geofiles))

for i in range(len(geofiles)-1):
    #  extract the dates
    geostringi=str(geofiles[i].decode())
    date1=geostringi[geostringi.find(pol+'_')+2:geostringi.find(pol+'_')+10]
    geostringj=str(geofiles[i+1].decode())
    date2=geostringj[geostringj.find(pol+'_')+2:geostringj.find(pol+'_')+10]
    print (date1, date2)
    if date1 == date2:
        outfile=geostringi[0:33]+geostringj[33:]
        command = '$PROC_HOME/util/mergeslcs '+geostringi+' '+geostringj+' '+outfile+' '+demwidth+' '+demlength
        print (command)
        ret =os.system(command)
        # make sure we have a matching orbtiming file
        command = 'cp '+geostringi.replace('geo','orbtiming')+' '+outfile.replace('geo','orbtiming')
        print (command)
        ret = os.system(command)
        # save files in case
        if not os.path.exists('orig_geos'):
            os.mkdir('orig_geos')
        command = 'mv '+geostringi+' orig_geos'
        ret = os.system(command)
        command = 'mv '+geostringj+' orig_geos'
        ret = os.system(command)

