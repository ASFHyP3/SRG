#!/usr/bin/env python3

import sys
import os
import math
import string
import subprocess
from datetime import datetime

if len(sys.argv) <  7:
    print ('Usage: residual_atmosphere.py geolist length lines scale looksac looksdn max-temporal_baseline <unwrap y/n=n>')
    print ('geo files one directory up, sizes are .geo size, scale is 0.25 for Sentinel')
    sys.exit(1)

# read in list of files
geolist=sys.argv[1]
fd=open(geolist,'r')
filelist=fd.readlines()
fd.close()

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

    names_times.append(str(jd)+' '+filelist[i])

#print names_times
filelist=sorted(names_times)
print (filelist)

filename=[]
for i in range(len(filelist)):
    words=filelist[i].split()
    filename.append(words[-1])

nfiles=len(filename)

length=sys.argv[2]
lines=sys.argv[3]
scale=sys.argv[4]
looksac=sys.argv[5]
looksdn=sys.argv[6]
maxtemp=sys.argv[7]
unw='n'
if len(sys.argv) > 7:
    unw=sys.argv[8]
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
fgeolist=open('geolist','w')
julianday=[]
for i in range(0,len(filelist)):
    words=filelist[i]
    char1=words.find('SSV_')
    if char1 < 0:
        char1=words.find('SDV_')
        print ('This is a dual pol acquisition')
    else:
        print ('This is a single pol acquisition')
    char2=words[char1:].find('T')
    scenedate=words[char1+4:char1+char2]

    jd=datetime.strptime(scenedate, '%Y%m%d').toordinal()+1721424.5
    fjd.write(str(jd)+'\n')
    fgeolist.write(filename[i]+'\n')
    julianday.append(float(jd))
    print ('Julian day ',jd,julianday[i])

fjd.close()
fgeolist.close()

# create list of interferograms
prochome = os.getenv('PROC_HOME')
num=0
unwcommand=[]
fintlist=open('intlist','w')
funwlist=open('unwlist','w')
fsbaslist=open('pseudosbaslist','w')
for i in range(nfiles-1):
    file1=filename[i]
    file2=filename[i+1]
    date1=file1[file1.find('20'):file1.find('20')+8]
    date2=file2[file2.find('20'):file2.find('20')+8]
    command = '$PROC_HOME/int/crossmul '+file1+' '+file2+' '+date1+'_'+date2+'.int '+date1+'_'+date2+'.amp '+length+' '+lines+' '+scale+' '+looksac+' '+looksdn
    print (command)
    ret=os.system(command)
    command = '$PROC_HOME/int/makecc '+date1+'_'+date2+'.int '+date1+'_'+date2+'.amp '+date1+'_'+date2+'.cc '+str(int(int(length)/int(looksac)))
    print (command)
    ret=os.system(command)

    # unwrap the int files in parallel

    if unw == 'y':
#        print ([prochome+'/bin/snaphu',date1+'_'+date2+'.int',str(int(int(length)/int(looksac))),'-d',' -o',date1+'_'+date2+'.unw','-c ',date1+'_'+date2+'.cc','--mcf'])
        unwcommand.append(subprocess.Popen([prochome+'/bin/snaphu',date1+'_'+date2+'.int',str(int(int(length)/int(looksac))),'-d','-o',date1+'_'+date2+'.unw','-c',date1+'_'+date2+'.cc','--mcf']))
#        unwcommand.append(subprocess.Popen([prochome+'/bin/snaphu',date1+date2+date3+'.int',str(int(int(length)/int(looksac))),'-d','-o',date1+date2+date3+'.unw','-c',date1+date2+'.cc','--mcf']))
        num=num+1

    # create the pseudo_sbas_list
    fintlist.write(date1+'_'+date2+'.int'+'\n')
    fsbaslist.write(file1+' '+file2+' '+str(julianday[i+1]-julianday[i])+' 1 '+'\n')
    funwlist.write(date1+'_'+date2+'.unw'+'\n')

fintlist.close()
funwlist.close()
fsbaslist.close()

for i in range(num):
    unwcommand[i].wait()

#  solve for time series

# create a reduced size dem to match multilooked files
command = '$PROC_HOME/util/nbymi2 ../elevation.dem dem '+length+' '+str(looksac)+' '+str(looksdn)
print (command)
ret = os.system(command)

# set up sbas ancillary files
command = '$PROC_HOME/sbas/sbas_setup.py pseudosbaslist geolist'
print (command)
ret = os.system(command)

proc = subprocess.Popen("wc -l < unwlist",stdout=subprocess.PIPE, shell=True)
(nunwfiles,err)=proc.communicate()
proc = subprocess.Popen("wc -l < geolist",stdout=subprocess.PIPE, shell=True)
(nslcs,err)=proc.communicate()

# troposphere correction using regression vs elevation
command = '$PROC_HOME/int/tropocorrect.py unwlist '+str(int(int(length)/int(looksac)))+' '+str(int(int(lines)/int(looksdn)))
print (command)
ret = os.system(command)

# compute sbas velocity solution
#command = '$PROC_HOME/sbas/sbas unwlist '+str(nunwfiles.decode()).rstrip()+' '+str(nslcs.decode()).rstrip()+' '+unwwidth+' ref_locs'
command = '$PROC_HOME/sbas/sbas unwlist '+str(nunwfiles.decode()).rstrip()+' '+str(nslcs.decode()).rstrip()+' '+str(int(int(length)/int(looksac)))+' ref_locs'
print (command)
ret = os.system(command)

# compute synthetic interferogram for each actual interferogram
for i in range(nfiles-1):
    file1=filename[i]
    file2=filename[i+1]
    date1=file1[file1.find('20'):file1.find('20')+8]
    date2=file2[file2.find('20'):file2.find('20')+8]
    command = '$PROC_HOME/int/synth_igram displacement '+date1+'_'+date2+'.synth.unw '+str(int(int(length)/int(looksac)))+' '+str(int(int(lines)/int(looksdn)))+' '+str(i+1)+' '+str(i+2)
    print (command)
    ret = os.system(command)

#  clean up

sys.exit()

