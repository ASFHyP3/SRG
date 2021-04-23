#!/usr/bin/env python3
#
#  merge_slcs - merge slcs when same pass has multiple acquisitions
#

import sys
import os
import subprocess

if len(sys.argv) < 1:
    print ("Usage: merge_slcs.py")
    sys.exit(0)

# create directory for old geofiles if necessary
if not os.path.exists('orig_geos'):
    os.mkdir('orig_geos')
# remove zero length geos if any are left over
ret=os.system('find . -size 0 -delete')

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

# what polarization are we working with
for i in range(len(geofiles)):
    #  date from a geo file
    geofile0=str(geofiles[i],'UTF-8')
    date0=geofile0[geofile0.find('V_')+2:geofile0.find('V_')+10]
    print ('date0 ',date0,date0[3:5])
    if date0[3:5] == 'IW':
#        print ('invalid date')
        pol='H'
    # is this a valid date?
    date0=geofile0[geofile0.find('H_')+2:geofile0.find('H_')+10]
#    print ('date0 ',date0)
    if date0[3:5] == 'IW':
#        print ('invalid date')
        pol='V'
    print ('file, polarization ',geofile0,pol)

# make a list with all geo slc dates
command = 'ls -1 *.geo'
proc = subprocess.Popen(command, stdout=subprocess.PIPE, shell=True)
(geolist, err) = proc.communicate()
geofiles = geolist.split()
geofiles.sort()  # put in alphabetical order
print ('Number of files: ',len(geofiles))
datelist=[]
dategeo=[]
for i in range(len(geofiles)):
    geostring=str(geofiles[i],'UTF-8')
    dategeo.append(geostring[geostring.find(pol+'_')+2:geostring.find(pol+'_')+10])
    if dategeo[i] in datelist:
        print (dategeo[i],' is in datelist')
        
    if dategeo[i] not in datelist:
        print (dategeo[i],' is not in list')
        datelist.append(dategeo[i])

#    print (i,datelist)

# create a new list with all geos for a date
for i in range(len(datelist)):
    indices = [j for j, x in enumerate(dategeo) if x == datelist[i]]
    print ('indices ',indices,len(indices))
    
    # if list has multiple entries start the merge operation
    if len(indices) > 1:
        print ('merging')
        geostringin=str(geofiles[indices[0]],'UTF-8')
        for j in range(len(indices)-1):
            #geostring1=str(geofiles[indices[j]],'UTF-8')
            geostring2=str(geofiles[indices[j+1]],'UTF-8')
            geostringout=geostringin[0:33]+geostring2[33:]
            print ('merge ',geostringin,geostring2,geostringout)
            command = '$PROC_HOME/util/mergeslcs '+geostringin+' '+geostring2+' '+geostringout+' '+demwidth+' '+demlength
            print (command)
            ret =os.system(command)
            # make sure we have a matching orbtiming file
            command = 'cp '+geostringin.replace('geo','orbtiming')+' '+geostringout.replace('geo','orbtiming')
            print (command)
            ret = os.system(command)
            # save orig files 
            command = 'mv '+geostringin+' orig_geos'
            ret = os.system(command)
            command = 'mv '+geostring2+' orig_geos'
            ret = os.system(command)
            geostringin=geostringout

sys.exit(0)

# remove any pre-existing merged geo files
for i in range(len(geofiles)):
    #  date from a geo file
    geofile0=str(geofiles[i],'UTF-8')
    date0=geofile0[geofile0.find('V_')+2:geofile0.find('V_')+10]
    print ('date0 ',date0,date0[3:5])
    if date0[3:5] == 'IW':
#        print ('invalid date')
        pol='H'
    # is this a valid date?
    date0=geofile0[geofile0.find('H_')+2:geofile0.find('H_')+10]
#    print ('date0 ',date0)
    if date0[3:5] == 'IW':
#        print ('invalid date')
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

