#!/usr/bin/env python3

# create a list of sbas pairs
#  max temporal = 0 generates minimal list of consecutive acquisitions

import sys
import subprocess
from datetime import datetime 
import os

if len(sys.argv) < 6:
    print('Usage: sbas_list_nearest.py geolist geopath max_temporal max_spatial sbas_out')
    sys.exit(1)

geolist = sys.argv[1]
geopath = sys.argv[2]
maxtemporal=float(sys.argv[3])
maxspatial=float(sys.argv[4])
outfile = sys.argv[5]

# open geolist
fgeo = open(geolist,'r')
geos = fgeo.readlines()
fgeo.close()

##  get a list of the geocoded slc files
#command = 'ls -1 '+geopath+'/*.geo'
#print (command)
#proc = subprocess.Popen(command, stdout=subprocess.PIPE, shell=True)
#(geos, err) = proc.communicate()

#  sort geofiles by date order
#geos=geos.split()
names_times=[]
#print(geos)

for i in range(0,len(geos)):
    #words=str(geos[i],'UTF-8').split('/')[-1]
    #wordstring=str(words)
    words = geos[i].strip().split('/')[-1]
    scenedate=words[:8]
    #print(scenedate)

    jd=datetime.strptime(str(scenedate), '%Y%m%d').toordinal()+1721424.5
    names_times.append(str(jd)+' '+geos[i].strip())

sortedgeos=sorted(names_times)

#  estimate baseline and create a file for the time-baseline plot
ftb=open(outfile,'w')

# create lists of dates and filenames, write out geolist
#geolist=open('geolist','w')
jdfile=open('jdlist','w')
jdlist=[]
for i in range(0,len(sortedgeos)):
#    geos[i]=sortedgeos[i].split()[1]
#    geolist.write(geos[i]+'\n')

    jdlist.append(float(sortedgeos[i].split()[0]))
    jdfile.write(str(jdlist[i])+'\n')

#geolist.close()
jdfile.close()
#print ('Julian day range: ',jdlist[0],jdlist[-1])
 
#  call the spatial baseline estimator
print ('Estimating baselines...')

for i in range(0,len(jdlist)):
    for j in range(0,i):
        # first do the temporal filter
        baseline2=int(abs(jdlist[i]-jdlist[j]))
        if (i-j==1) or (baseline2 <= maxtemporal):
           # # spatial baseline estimator
           # orbtimingi=geopath+geos[i].strip().replace('geo','orbtiming')
           # orbtimingj=geopath+geos[j].strip().replace('geo','orbtiming')
           # command = '$PROC_HOME/sentinel/geo2rdr/estimatebaseline '+orbtimingi+' '+orbtimingj
           # print(command)
           # proc = subprocess.Popen(command, stdout=subprocess.PIPE, shell=True)
           # (baseline1, err) = proc.communicate()

           # if (i-j==1) or (abs(float(baseline1)) <= maxspatial):
            geostri=geopath+geos[i].strip()
            geostrj=geopath+geos[j].strip()
           # ftb.write(geostrj+' '+geostri+' '+str(baseline2)+' '+str(baseline1,"UTF-8"))
            ftb.write(geostrj+' '+geostri+' '+str(baseline2)+' 0\n')

 
#if maxtemporal > 0:
    
#    for i in range(0,len(jdlist)):
#        for j in range(0,i):
#            #  first do temporal filter
#            baseline2=abs(jdlist[i]-jdlist[j])
#            if baseline2 <= maxtemporal:

#                #  spatial baseline estimator
#                orbtimingi=geos[i].strip().replace('geo','orbtiming')
#                orbtimingj=geos[j].strip().replace('geo','orbtiming')
#                command = '$PROC_HOME/sentinel/geo2rdr/estimatebaseline '+orbtimingi+' '+orbtimingj
#                print(command)
#                proc = subprocess.Popen(command, stdout=subprocess.PIPE, shell=True)
#                (baseline1, err) = proc.communicate()
#                #print (command,' baseline1: ',str(baseline1,"UTF-8"))
#                if abs(float(baseline1)) <= maxspatial:
#                    geostri=geos[i]
#                    geostrj=geos[j]
#                    ftb.write(geostrj+' '+geostri+' '+str(baseline2)+' '+str(baseline1,"UTF-8"))

#else:  #  zero max temporal means minimal list of pairs
#    for i in range(1,len(jdlist)):
#        j=i-1
#        orbtimingi=geos[i].strip().replace('geo','orbtiming')
#        orbtimingj=geos[j].strip().replace('geo','orbtiming')
        
#        baseline2=abs(jdlist[i]-jdlist[j])
#        geostri=geos[i]
#        geostrj=geos[j]
#        ftb.write(geostrj+' '+geostri+' '+str(baseline2)+' 100000000'+'\n')

print ('sbas list written')
ftb.close()
