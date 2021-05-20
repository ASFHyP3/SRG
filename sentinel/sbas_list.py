#!/usr/bin/env python3

# create a list of sbas pairs
#  max temporal = 0 generates minimal list of consecutive acquisitions

import sys
import subprocess
from datetime import datetime 
import os

if len(sys.argv) < 3:
    print ('Usage: sbas_list.py max_temporal max_spatial')
    sys.exit(1)

maxtemporal=float(sys.argv[1])
maxspatial=float(sys.argv[2])

#  get a list of the geocoded slc files
command = 'ls -1 ../*.geo'
print (command)
proc = subprocess.Popen(command, stdout=subprocess.PIPE, shell=True)
(geos, err) = proc.communicate()

#  sort geofiles by date order
geos=geos.split()
names_times=[]

for i in range(0,len(geos)):
    words=str(geos[i],'UTF-8')
    wordstring=str(words)
    char1=wordstring.find('SSV_')
    if char1 < 0:
        char1=wordstring.find('SDV_')
#        print ('This is a dual pol acquisition')
    else:
        print ('This is a single pol acquisition')
    if char1 < 0:
        char1=wordstring.find('SSH_')
        if char1 < 0:
            char1=wordstring.find('SDH_')
#            print ('This is a dual pol acquisition')
        else:
            print ('This is a single pol acquisition')

    char2=wordstring[char1:].find('T')

    scenedate=words[char1+4:char1+char2]

    jd=datetime.strptime(str(scenedate), '%Y%m%d').toordinal()+1721424.5
#    print ('Julian day ',jd)

    names_times.append(str(jd)+' '+str(geos[i]))

#print names_times
sortedgeos=sorted(names_times)
#print (sortedgeos)

#  estimate baseline and create a file for the time-baseline plot
ftb=open('sbas_list','w')

# create lists of dates and filenames, write out geolist
geolist=open('geolist','w')
jdfile=open('jdlist','w')
jdlist=[]
for i in range(0,len(sortedgeos)):
    geos[i]=sortedgeos[i].split()[1]
    geolist.write(geos[i].replace('b','',1).replace("'",'')+'\n')

    jdlist.append(float(sortedgeos[i].split()[0]))
    jdfile.write(str(jdlist[i])+'\n')

geolist.close()
jdfile.close()
print ('Julian day range: ',jdlist[0],jdlist[-1])
#print (geos)
 
#  call the spatial baseline estimator
print ('Estimating baselines...')
if maxtemporal > 0:
    
    for i in range(0,len(jdlist)):
        for j in range(0,i):
            #  spatial baseline estimator
            #        print ('type geos ',type(geos[i]))
            orbtimingi=geos[i].strip().replace('geo','orbtiming').replace('b','',1)
            orbtimingj=geos[j].strip().replace('geo','orbtiming').replace('b','',1)
            #        print ('type orbtiming ',type(orbtimingi),orbtimingi)
            command = '$PROC_HOME/sentinel/geo2rdr/estimatebaseline '+orbtimingi+' '+orbtimingj
            #        command = '$PROC_HOME/sentinel/geo2rdr/estimatebaseline '+geos[i].strip().replace('geo','orbtiming')+' '+geos[j].strip().replace('geo','orbtiming')
            #print (command)
            proc = subprocess.Popen(command, stdout=subprocess.PIPE, shell=True)
            (baseline1, err) = proc.communicate()
            if abs(float(baseline1)) <= maxspatial:
                
                baseline2=abs(jdlist[i]-jdlist[j])
                if baseline2 <= maxtemporal:
                    geostri=geos[i].replace('b','',1).replace("'",'')
                    geostrj=geos[j].replace('b','',1).replace("'",'')
                    ftb.write(geostrj+' '+geostri+' '+str(baseline2)+' '+str(baseline1,'UTF-8').strip()+'\n')
                    #                ftb.write(geostrj+' '+geostri+' '+str(baseline2)+' '+baseline1.decode().strip()+'\n')
                    #                ftb.write(geos[j].strip().replace('b','',1)+' '+geos[i].strip().replace('b','',1)+' '+str(baseline2).replace('b','',1)+' '+str(basel#ine1).replace('b','',1))

else:  #  zero max temporal means minimal list of pairs
    for i in range(1,len(jdlist)):
        j=i-1
        orbtimingi=geos[i].strip().replace('geo','orbtiming').replace('b','',1)
        orbtimingj=geos[j].strip().replace('geo','orbtiming').replace('b','',1)
        
        baseline2=abs(jdlist[i]-jdlist[j])
        geostri=geos[i].replace('b','',1).replace("'",'')
        geostrj=geos[j].replace('b','',1).replace("'",'')
        ftb.write(geostrj+' '+geostri+' '+str(baseline2)+' 100000000'+'\n')

print ('sbas list written')
ftb.close()
