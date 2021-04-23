#!/usr/bin/env python3

import sys
import os
import math
import string
import subprocess
from datetime import datetime

if len(sys.argv) <  7:
    print 'Usage: tripletsfilelist.py geolist length lines scale looksac looksdn <unwrap y/n=n>'
    print ('geo files one directory up, sizes are .geo size, scale is 1e-4 for Sentinel')
    sys.exit(1)

# read in list of files
geolist=sys.argv[1]
fd=open(geolist,'r')
filelist=fd.readlines()
fd.close()
print ('filelist ',filelist)
#  sort geofiles by date order
#  get a list of the geocoded slc files
#command = 'ls -1 ../*.geo'
#print (command)
#proc = subprocess.Popen(command, stdout=subprocess.PIPE, shell=True)
#(geos, err) = proc.communicate()
#print ('geos ',geos)
#geos=geos.split()
names_times=[]

for i in range(0,len(filelist)):
    words=filelist[i]
    char1=words.find('SSV_')
    if char1 < 0:
        char1=words.find('SDV_')
        #print 'This is a dual pol acquisition'
    #else:
        #print 'This is a single pol acquisition'
    char2=words[char1:].find('T')
    scenedate=words[char1+4:char1+char2]

    jd=datetime.strptime(scenedate, '%Y%m%d').toordinal()+1721424.5
    print 'Julian day ',jd

    names_times.append(str(jd)+' '+filelist[i])

#print names_times
filelist=sorted(names_times)
print filelist

filename=[]
for i in range(len(filelist)):
    words=filelist[i].split()
    filename.append(words[-1])

print filename

nfiles=len(filename)

length=sys.argv[2]
lines=sys.argv[3]
scale=sys.argv[4]
looksac=sys.argv[5]
looksdn=sys.argv[6]
unw='n'
if len(sys.argv) > 6:
    unw=sys.argv[7]
    print('unwrap: ',unw,'\n')

# save parameters for display
fpar=open('parameters','w')
fpar.write(str(int(int(length)/int(looksac)))+'\n')
fpar.write(str(int(int(lines)/int(looksdn)))+'\n')
fpar.write(str(nfiles)+'\n')
fpar.write(str(nfiles-2)+'\n')
fpar.close()

# jdlist for geo files
fjd=open('jdlist','w')
for i in range(0,len(filelist)):
    words=filelist[i]
    char1=words.find('SSV_')
    if char1 < 0:
        char1=words.find('SDV_')
        print 'This is a dual pol acquisition'
    else:
        print 'This is a single pol acquisition'
    char2=words[char1:].find('T')
    scenedate=words[char1+4:char1+char2]

    jd=datetime.strptime(scenedate, '%Y%m%d').toordinal()+1721424.5
    fjd.write(str(jd)+'\n')
    print 'Julian day ',jd

fjd.close()

# create list of triplets
prochome = os.getenv('PROC_HOME')
num=0
unwcommand=[]
ftripleint=open('tripleintlist','w')
ftripleunw=open('tripleunwlist','w')
for i in range(nfiles-2):
    file1=filename[i]
    file2=filename[i+1]
    file3=filename[i+2]
    date1=file1[file1.find('20'):file1.find('20')+8]
    date2=file2[file2.find('20'):file2.find('20')+8]
    date3=file3[file3.find('20'):file3.find('20')+8]
    command = '$PROC_HOME/int/triplet.py '+file1+' '+file2+' '+file3+' '+length+' '+lines+' '+scale+' '+looksac+' '+looksdn
    print (command)
    ret=os.system(command)
    command = 'mv tripleint '+date1+date2+date3+'.int'
    print (command)
    ret=os.system(command)
    ftripleint.write(date1+date2+date3+'.int'+'\n')
    command = 'mv cc12 '+date1+date2+'.cc'
    ret=os.system(command)
    command = 'mv cc13 '+date1+date3+'.cc'
    ret=os.system(command)
    command = 'mv cc32 '+date3+date2+'.cc'
    ret=os.system(command)
    command = 'mv qint12 '+date1+date2+'.int'
    ret=os.system(command)
    command = 'mv qint13 '+date1+date3+'.int'
    ret=os.system(command)
    command = 'mv qint32 '+date3+date2+'.int'
    ret=os.system(command)

    # unwrap the triple int files in parallel

    if unw == 'y':
        command = '$PROC_HOME/bin/snaphu '+date1+date2+date3+'.int '+str(int(int(length)/int(looksac)))+' -d -o '+date1+date2+date3+'.unw -c '+date1+date2+'.cc --mcf' 
        print (command)
        #ret=os.system(command)
        unwcommand.append(subprocess.Popen([prochome+'/bin/snaphu',date1+date2+date3+'.int',str(int(int(length)/int(looksac))),'-d','-o',date1+date2+date3+'.unw','-c',date1+date2+'.cc','--mcf']))
        num=num+1
        ftripleunw.write(date1+date2+date3+'.unw'+'\n')

ftripleint.close()
ftripleunw.close()
for i in range(num):
    unwcommand[i].wait()

#  clean up
ret=os.system('rm q*')

sys.exit()

#['$PROC_HOME/sentinel/unwrap_igrams.py',intlist.rstrip(),unwwidth,'1']
