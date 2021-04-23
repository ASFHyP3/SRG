#!/usr/bin/env python3

#  FUNCTION

import sys
import os
import math
import string

ver=sys.version_info

    


    

if len(sys.argv)< 1:
    print 'Usage: latlon.py'
    sys.exit(1)
if len(sys.argv) > 1:
    print 'Usage: latlon.py'
    sys.exit(1)

latlon_file=open('latloncoords', 'r')
latlon=latlon_file.readlines()
coords=[]
#lat-up long-down
for line in latlon:
    coord = line.split()
    coords.append(coord[0])
print coords
latlon_file.close()
coords[0]=int(math.ceil(float(coords[0])))
coords[1]=int(math.floor(float(coords[1])))
coords[2]=int(math.ceil(float(coords[2])))
coords[3]=int(math.floor(float(coords[3])))
print coords

## lat = coords[2]
## long = coords[1]
## taking both ascending, descending orbits into consideration
lat = max(coords[2],coords[0])
long = min (coords[1],coords[3])

x = abs(coords[3]-coords[1])+1
y = abs(coords[2]-coords[0])+1

print 'lat long x y ',lat,long,x,y

#Find and download .dem files
filelist=[]
for i in range(lat-y,lat):
    for j in range(long,long+x):
        if i<0:
            if -i<10:
                loc='S0'+str(-i)
            else:
                loc='S'+str(-i)
        else:
            if i<10:
                loc='N0'+str(i)
            else:
                loc='N'+str(i)
        if j<0:
            if -j<10:
                loc=loc+'W00'+str(-j)
            elif -j<100:
                loc=loc+'W0'+str(-j)
            else:
                loc=loc+'W'+str(-j)
        else:
            if j<10:
                loc=loc+'E00'+str(j)
            elif j<100:
                loc=loc+'E0'+str(j)
            else: 
                loc=loc+'E'+str(j)
        print loc
        filelist.append(loc+'.dem')

#filelist.sort()

print 'Ordered request for files: ',filelist

region=''
arcsec=''
regions=['California','United_States','North_America','South_America','Eurasia']
flist1=[]
flist3=[]
addrlist1=[]
addrlist3=[]
templist=[]
remlist=[]
addresslist=[]

for file in filelist:
    asec='1arcsec'
    for reg in regions[0:2]:
#        address='http://www.stanford.edu/group/radar/SRTM/'+reg+'/'+asec+'/'+file        
        address='http://pangea.stanford.edu/sesfs/srtm/'+reg+'/'+asec+'/'+file
#        address='http://jukebox.stanford.edu/SRTM/'+reg+'/'+asec+'/'+file
        if (os.system('wget --spider '+address)==0):
            addrlist1.append(address)
            flist1.append(file)
            print 'file found: ',file
            break
    print flist1
    if len(addrlist1) == len(filelist):
          filelist=flist1
          addresslist=addrlist1
        
else:
    asec='3arcsec'
    for file in filelist:
        for reg in regions[2:5]:
#            address='http://www.stanford.edu/group/radar/SRTM/'+reg+'/'+asec+'/'+file            
            address='http://pangea.stanford.edu/sesfs/srtm/'+reg+'/'+asec+'/'+file
#            address='http://jukebox.stanford.edu/SRTM/'+reg+'/'+asec+'/'+file
            if (os.system('wget --spider '+address)==0):
                addrlist3.append(address)
                flist3.append(file)
                print 'file found: ',file
                print 'flist3: ',flist3
                break
    print flist3
    print 'addrlist3 ',addrlist3
    print 'filelist ',filelist
    if len(addrlist3) == len(filelist):
        flist1=flist3
        addrlist1=addrlist3
        filelist=flist3
        addresslist=addrlist3
    
    else:
        if len(addrlist3)>len(addrlist1):
            flist1=flist3
            addrlist1=addrlist3
            
        print flist1
        print addrlist1
        latlist=[]
        lonlist=[]

addresslist=addrlist1

print '#####################################'
print 'We need', filelist
print 'We found',flist1
filesfound=flist1

print 'flist1 ',flist1
print 'flist3 ',flist3
print 'addrlist1 ',addrlist1
print 'addrlist3 ',addrlist3
print 'filelist ',filelist


demlist = open('demfiles.txt','w')
for file in filelist:
    print 'filelist loop: ',file
    if file in filesfound:
        demlist.write(file+'\n')
    else:
        demlist.write('nofile\n')
demlist.close()


if len(flist1)==0:
    print 'dem file not created: files not found on server'
    sys.exit(1)
if len(flist1)<len(filelist):
    print 'Caution! missing some dem file!'
    notedem=open('DEMMISSINGREPORT.txt','w')
    notedem.write('We failed to find at least one DEM file \n')
    notedem.write('Check demfiles.txt to see which one is missing \n')
    notedem.write('We filled missing parts with all 0s \n')
    notedem.write('Hopefully its ocean!!! Otherwise please make your own DEM!')
    notedem.close()
    
print '#####################################'
print

rscfile=flist1[0].replace('.dem','.dem.rsc')
flist1.append(rscfile)
addresslist.append(addresslist[0].replace('.dem','.dem.rsc'))
demlist = open('demfiles.orig.txt','w')

for file in os.listdir('.'):
    if file in filelist:    # go through needed files
        if file in flist1:    #  go through found files
              addresslist.remove(addresslist[flist1.index(file)])
              flist1.remove(file)
        print 'Writing into demlist: ',file
        demlist.write(file+'\n')

print 'addresslist: ',addresslist
print 'filelist: ',filelist
print 'flist1: ',flist1

if not len(flist1)==0:

    for addr in addresslist:
        command = 'wget '+addr  #  retrieve rsc file from dem server'
        print (command)
        ret=os.system(command)

    for file in flist1:
        if not file.endswith('.rsc'):
            demlist.write(file+'\n')
    demlist.close()
    print 'list of dem files created'


file=open(rscfile)
strings=file.readlines()
width=strings[0].split()
step=strings[4].split()
#command='/home/yjzheng/scripts/sentinel/makeDEM '+str(lat)+' '+str(long)+' '+str(x)+' '+str(y)+' '+width[1]+' '+step[1]+' '+'demfiles.txt'
command='/home/zebker/DEM/makeDEM '+str(lat)+' '+str(long)+' '+str(x)+' '+str(y)+' '+width[1]+' '+step[1]+' '+'demfiles.txt'
print (command)
ret=os.system(command)
    
rsc=open('elevation.dem.rsc','r')
lines=rsc.readlines()
st=[]
num=[]
for line in lines:
    words=line.split()
    st.append(words[0])
    num.append(words[1])
print st
print num
rsc.close()

newrsc=open('elevation.dem.rsc','w')
for i in range(len(st)):
    newrsc.write(st[i]+((15-len(st[i]))*' '))
    newrsc.write(num[i]+'\n')
newrsc.close()
