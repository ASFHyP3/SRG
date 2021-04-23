#!/usr/bin/env python3

#  sbas_setup.py - create auxiliary files for Ann's sbas matlab code

import sys
from datetime import datetime
ver=sys.version_info

    


    


import os
import sys
import math

if len(sys.argv) < 3:
    print 'Usage: sbas_setup.py sbas_list (Peg_point_list.txt or geolist)'
    sys.exit(1)

sbas_list=sys.argv[1]
peg_list=sys.argv[2]

fpeg=open(peg_list,'r')
peg=fpeg.readlines()

slctime=list(float(i) for i in range(0, len(peg)))
deltatime=list(float(i) for i in range(0, len(peg)))
k=0
for line in peg:
    words=line.split()
#    print words[-1]
    slc=words[-1]
    first=slc.find('20')
    slcname=slc[first:first+8]
    slcyear=slcname[0:4]
    slcmon=slcname[4:6]
    slcday=slcname[6:8]
    jd=datetime.strptime(slcname, '%Y%m%d').toordinal()+1721424.5
    slctime[k] = jd #int(slcyear)+float(int(slcmon))/12.+float(int(slcday))/360.
    if k > 0:
        deltatime[k] = slctime[k]-slctime[k-1]
        #deltatime[k] = round((float(slctime[k])-float(slctime[k-1]))*360.)
      #  print k,slctime,deltatime,'\n'
    k=k+1

print
print 'slctime ',slctime
print 'deltime ',deltatime

ftimedeltas=open('timedeltas.out','w')  # save time intervals between slcs
for i in range(1,len(deltatime)):
    ftimedeltas.write(str(deltatime[i])+'\n')

fsbas=open(sbas_list,'r')
sbas=fsbas.readlines()

fbperp=open('Bperp.out','w')
fdeltime=open('deltime.out','w')
ftm=open('Tm.out','w')

id=0
for line in sbas:
    words=line.split()
    master=words[0]
    slave=words[1]
    timebaseline=words[2]
    spacebaseline=words[3]

#  get a short name for master and slave files
    first=master.find('20')
    mastername=master[first:first+8]
    masteryear=mastername[0:4]
    mastermon=mastername[4:6]
    masterday=mastername[6:8]
    slc1time=datetime.strptime(mastername, '%Y%m%d').toordinal()+1721424.5

    first=slave.find('20')
    slavename=slave[first:first+8]
    slaveyear=slavename[0:4]
    slavemon=slavename[4:6]
    slaveday=slavename[6:8]
    slc2time=datetime.strptime(slavename, '%Y%m%d').toordinal()+1721424.5

    iprint=0
    if iprint==1:
        print master
        print mastername
        print slave
        print slavename
        print timebaseline
        print spacebaseline
        print masteryear+' '+mastermon+' '+masterday+' '+str(slc1time)+'\n'

    # spatial baseline to Bperp.out
    fbperp.write(spacebaseline+'\n')

    # temporal baseline to deltime.out
    id=id+1
    fdeltime.write(str(id)+' '+timebaseline+' '+str(slc1time)+' '+str(slc2time)+'\n')

    # time interval matrix to Tm.out
    # which slc for master and slave?
    k=0
    for line in peg:
        words=line.split()
        slc=words[-1]
        first=slc.find('20')
        slcname=slc[first:first+8]
        if slcname == mastername:
            kmaster=k
        if slcname == slavename:
            kslave=k
        k=k+1

    times=list(float(0) for i in range (0,len(peg)))
#    print kmaster,' ',kslave
    if kmaster < kslave:
        for i in range(kmaster,kslave):
            times[i]=deltatime[i+1]
    if kmaster > kslave:
        for i in range(kslave,kmaster):
            times[i]=-deltatime[i+1]
    for i in range(0,len(times)-1):
        ftm.write(str(times[i])+' ')
    ftm.write('\n')

sys.exit()

