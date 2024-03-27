#!/usr/bin/env python3

#  create an orbtiming file for all bursts (1-11) using a precise orbit file

import sys
import string
import os
import sys
import math
from datetime import datetime
import struct

def readxmlparam(xmllines, param):
    for line in xmllines:
        if param in line:
            i=line.find(param)
            str1=line[i:];
            istart=str1.find('>')+1
            istop=str1.find('<')
            value=str1[istart:istop]
            #print line[i:],'\n'
            #print istart, istop, '\n'
            return value

def timeinseconds(timestring):
#    dt = datetime.strptime(timestring, 'TAI=%Y-%m-%dT%H:%M:%S.%f')
    dt = datetime.strptime(timestring, 'UTC=%Y-%m-%dT%H:%M:%S.%f')
#    dt = datetime.strptime(timestring, 'UT1=%Y-%m-%dT%H:%M:%S.%f')
    secs=dt.hour*3600+dt.minute*60+dt.second+dt.microsecond/1000000.0
    return secs

if len(sys.argv) < 2:
    print ('Usage: orbitstatevectors.py preciseorbitfile <SAFEname=null>')
    sys.exit(1)

orbitfile=sys.argv[1]
SAFEname=''
if len(sys.argv) >=2:
    SAFEname=sys.argv[2]
    
# read the precise file
xmlfile=open(orbitfile,'r')
xmllines=xmlfile.readlines()
xmlfile.close()

#  save orbit and timing information

#  extract each state vector
start=[]
stop=[]
for i in range(len(xmllines)):
    if '<OSV>' in xmllines[i]:
        start.append(i)
        
    if '</OSV>' in xmllines[i]:
        stop.append(i)

time=[]
x=[]
y=[]
z=[]
vx=[]
vy=[]
vz=[]
for i in range(len(start)):
    statelines=xmllines[start[i]:stop[i]]
    time.append(readxmlparam(statelines,'UTC'))
    x.append(readxmlparam(statelines,'X unit'))
    y.append(readxmlparam(statelines,'Y unit'))
    z.append(readxmlparam(statelines,'Z unit'))
    vx.append(readxmlparam(statelines,'VX unit'))
    vy.append(readxmlparam(statelines,'VY unit'))
    vz.append(readxmlparam(statelines,'VZ unit'))
    
orbinfo=open(SAFEname+'.orbtiming.full','w')
orbinfo.write('0 \n')
orbinfo.write('0 \n')
orbinfo.write('0 \n')
orbinfo.write(str(len(start))+'\n')
for i in range(len(start)):
    orbinfo.write(str(timeinseconds(time[i]))+' '+str(x[i])+' '+str(y[i])+' '+str(z[i])+' '+str(vx[i])+' '+str(vy[i])+' '+str(vz[i])+' 0.0 0.0 0.0')
    orbinfo.write('\n')

orbinfo.close()
sys.exit(0)

