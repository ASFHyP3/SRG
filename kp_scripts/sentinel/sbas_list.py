#!/usr/bin/env python3

# create a list of sbas pairs
#  max temporal = 0 generates minimal list of consecutive acquisitions

import sys
import subprocess
from datetime import datetime 
import os

if len(sys.argv) < 4:
    print('Usage: sbas_list_kp.py geolist geopath sbaslist_out max_temporal max_spatial')
    sys.exit(1)

geolist = sys.argv[1]
geopath = sys.argv[2]
sbaslist_out = sys.argv[3]
maxtemporal=float(sys.argv[4])
maxspatial=float(sys.argv[5])

#  get a list of the geocoded slc files
with open(geolist,'r') as fgeo:
    geos = fgeo.readlines()
for k in range(len(geos)):
    geofile = geos[k].split('/')[-1]
    geos[k] = geopath+'/'+geofile.strip()

#  sort geofiles by date order
names_times=[]

for i in range(0,len(geos)):
    words=geos[i].split('/')[-1]
    wordstring=str(words)
    scenedate=words[:8]

    jd=datetime.strptime(str(scenedate), '%Y%m%d').toordinal()+1721424.5
    names_times.append(str(jd)+' '+geos[i])

sortedgeos=sorted(names_times)

#  estimate baseline and create a file for the time-baseline plot
ftb=open(sbaslist_out,'w')

# create lists of dates and filenames, write out geolist
geolist=open(geolist+'_fullpath','w')
jdfile=open('jdlist','w')
jdlist=[]
for i in range(0,len(sortedgeos)):
    geos[i]=sortedgeos[i].split()[1]
    geolist.write(geos[i]+'\n')

    jdlist.append(float(sortedgeos[i].split()[0]))
    jdfile.write(str(jdlist[i])+'\n')

geolist.close()
jdfile.close()
print ('Julian day range: ',jdlist[0],jdlist[-1])
 
#  call the spatial baseline estimator
print ('Estimating baselines...')
if maxtemporal > 0:
    
    for i in range(0,len(jdlist)):
        for j in range(0,i):
            #  first do temporal filter
            baseline2=abs(jdlist[i]-jdlist[j])
            if baseline2 <= maxtemporal:

                #  spatial baseline estimator
                orbtimingi=geos[i].strip().replace('geo','orbtiming')
                orbtimingj=geos[j].strip().replace('geo','orbtiming')
                command = '$PROC_HOME/sentinel/geo2rdr/estimatebaseline '+orbtimingi+' '+orbtimingj
                proc = subprocess.Popen(command, stdout=subprocess.PIPE, shell=True)
                (baseline1, err) = proc.communicate()
                #print (command,' baseline1: ',str(baseline1,"UTF-8"))
                if abs(float(baseline1)) <= maxspatial:
                    geostri=geos[i]
                    geostrj=geos[j]
                    ftb.write(geostrj+' '+geostri+' '+str(baseline2)+' '+str(baseline1,"UTF-8"))

else:  #  zero max temporal means minimal list of pairs
    for i in range(1,len(jdlist)):
        j=i-1
        orbtimingi=geos[i].strip().replace('geo','orbtiming')
        orbtimingj=geos[j].strip().replace('geo','orbtiming')
        
        baseline2=abs(jdlist[i]-jdlist[j])
        geostri=geos[i]
        geostrj=geos[j]
        ftb.write(geostrj+' '+geostri+' '+str(baseline2)+' 100000000'+'\n')

print ('sbas list written')
ftb.close()
