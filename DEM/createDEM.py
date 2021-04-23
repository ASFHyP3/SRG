#!/usr/bin/env python3
#
#  latlon.srtm30.py -- create a DEM of the full sentinel frame using NASADEM
#
#  Default is 30 m postings, with argument 'f' create full-res DEM (6m x 15m)
#

#  FUNCTION

import sys
import os
import math
import string

if len(sys.argv)< 8:
    print ('Usage: createDEM.py demfile demrscfile toplat botlat leftlon rightlon upsamplex upsampley')
    sys.exit(1)

demfile=sys.argv[1]
demrscfile=sys.argv[2]
upsamplex='1'
upsamplexy='1'
if len(sys.argv)>6:
    upsamplex=sys.argv[7]
if len(sys.argv)>7:
    upsampley=sys.argv[8]

coords=[]
coords.append(float(sys.argv[4]))
coords.append(float(sys.argv[6]))
coords.append(float(sys.argv[3]))
coords.append(float(sys.argv[5]))
#print ('input  coords ',coords)
finalcoords=coords.copy()
coords[0]=int(math.ceil(float(coords[0])))
coords[1]=int(math.floor(float(coords[1])))
coords[2]=int(math.ceil(float(coords[2])))
coords[3]=int(math.floor(float(coords[3])))

latlon_file=open('latloncoords', 'w')
#lat-up long-right
for line in range(4):
    latlon_file.write(str(coords[line])+'\n')

latlon_file.close()
latlon_file=open('latloncoords', 'r')
latlon=latlon_file.readlines()
latlon_file.close()

#print (coords)

## taking both ascending, descending orbits into consideration
lat = max(coords[2],coords[0])
long = min (coords[1],coords[3])

x = abs(coords[3]-coords[1])+1
y = abs(coords[2]-coords[0])+1

#print 'lat long x y ',lat,long,x,y

#Find and download .dem files
filelist=[]
for i in range(lat-y,lat):
    for j in range(long,long+x):
        if i<0:
            if -i<10:
                loc='s0'+str(-i)
            else:
                loc='s'+str(-i)
        else:
            if i<10:
                loc='n0'+str(i)
            else:
                loc='n'+str(i)
        if j<0:
            if -j<10:
                loc=loc+'w00'+str(-j)
            elif -j<100:
                loc=loc+'w0'+str(-j)
            else:
                loc=loc+'w'+str(-j)
        else:
            if j<10:
                loc=loc+'e00'+str(j)
            elif j<100:
                loc=loc+'e0'+str(j)
            else: 
                loc=loc+'e'+str(j)
#        print (loc)
        filelist.append(loc+'.hgt')

#filelist.sort()

print ('Ordered request for files: ',filelist)

region=''
arcsec=''
#regions=['California','United_States','North_America','South_America','Eurasia']
regions=['NorthAmerica','Eurasia','SouthAmerica','Islands','Africa','Australia']
#regions=['Islands','NorthAmerica']
flist1=[]
addrlist1=[]
templist=[]
remlist=[]
addresslist=[]

for file in filelist:
    for reg in regions[0:len(regions)]:
#        address='http://www.stanford.edu/group/radar/SRTM/'+reg+'/'+asec+'/'+file        
#        address='http://pangea.stanford.edu/sesfs/srtm/'+reg+'/'+file
        address='http://pangea.stanford.edu/sesfs/srtm30/'+reg+'/'+file+' --no-check-certificate'
#        print ('Looking for: ',address)
#        address='http://jukebox.stanford.edu/SRTM/'+reg+'/'+asec+'/'+file
        if (os.system('wget --quiet --spider '+address)==0):
            addrlist1.append(address)
            flist1.append(file)
#            print ('file found: ',file)
            break
#    print flist1
    if len(addrlist1) == len(filelist):
          filelist=flist1
          addresslist=addrlist1
        
#    print flist1
#    print addrlist1
    latlist=[]
    lonlist=[]

addresslist=addrlist1

print ('#####################################')
print ('We need', filelist)
print ('We found',flist1)
filesfound=flist1

#print ('flist1 ',flist1)
#print ('addrlist1 ',addrlist1)
#print ('filelist ',filelist)


demlist = open('demfiles.txt','w')
for file in filelist:
#    print ('filelist loop: ',file)
    if file in filesfound:
        demlist.write(file+'\n')
    else:
        demlist.write('nofile\n')
demlist.close()


if len(flist1)==0:
    print ('dem file not created: files not found on server')
    sys.exit(1)
if len(flist1)<len(filelist):
    print ('Caution! missing some dem file!')
    notedem=open('DEMMISSINGREPORT.txt','w')
    notedem.write('We failed to find at least one DEM file \n')
    notedem.write('Check demfiles.txt to see which one is missing \n')
    notedem.write('We filled missing parts with all 0s \n')
    notedem.write('Hopefully its ocean!!! Otherwise please make your own DEM!')
    notedem.close()
    
print ('#####################################')
print ()

rscfile=flist1[0].replace('.hgt','.hgt.rsc')
flist1.append(rscfile)
addresslist.append(addresslist[0].replace('.hgt','.hgt.rsc'))
demlist = open('demfiles.orig.txt','w')

for file in os.listdir('.'):
    if file in filelist:    # go through needed files
        if file in flist1:    #  go through found files
              addresslist.remove(addresslist[flist1.index(file)])
              flist1.remove(file)
#        print ('Writing into demlist: ',file)
        demlist.write(file+'\n')

#print ('addresslist: ',addresslist)
#print ('filelist: ',filelist)
#print ('flist1: ',flist1)

if not len(flist1)==0:

    for addr in addresslist:
        command = 'wget --quiet '+addr  #  retrieve rsc file from dem server'
        print (command)
        ret=os.system(command)

    for file in flist1:
        if not file.endswith('.rsc'):
            demlist.write(file+'\n')
    demlist.close()
    print ('list of dem files created')


file=open(rscfile)
strings=file.readlines()
width=strings[0].split()
step=strings[4].split()
command=os.environ['PROC_HOME']+'/DEM/makeDEM '+str(lat)+' '+str(long)+' '+str(x)+' '+str(y)+' '+width[1]+' '+step[1]+' '+'demfiles.txt n'
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
#print (st)
#print (num)
rsc.close()

newrsc=open('elevation.dem.rsc','w')
for i in range(len(st)):
    newrsc.write(st[i]+((15-len(st[i]))*' '))
    newrsc.write(num[i]+'\n')
newrsc.close()

# trim dem to closer to scene size
#print ('lat long x y ',lat,long,x,y)
#print ('latlon ',latlon)
#print ('final coords ',finalcoords)

# adjust for asc/desc
#latmax = max(float(latlon[2].strip()),float(latlon[0].strip()))
#latmin = min (float(latlon[2].strip()),float(latlon[0].strip()))
#lonmax = max(float(latlon[1].strip()),float(latlon[3].strip()))
#lonmin = min (float(latlon[1].strip()),float(latlon[3].strip()))
#print ('latmax latmin lonmax lonmin ',latmax,latmin,lonmax,lonmin)
latmax = max(finalcoords[0],finalcoords[2])
latmin = min(finalcoords[0],finalcoords[2])
lonmax = max(finalcoords[1],finalcoords[3])
lonmin = min(finalcoords[1],finalcoords[3])
#print ('latmax latmin lonmax lonmin ',latmax,latmin,lonmax,lonmin)

command = 'mv elevation.dem q.dem; mv elevation.dem.rsc q.dem.rsc'
print (command)
ret = os.system(command)

# trim the dem
#top = str(int((lat-latmax)*3600))
#bot = str(int((lat-latmin)*3600))
#left = str(int((lonmin-long)*3600))
#right = str(int((lonmax-long)*3600))
top=str(latmax)
bot=str(latmin)
left=str(lonmin)
right=str(lonmax)
command = os.environ['PROC_HOME']+'/util/createspecialdem q.dem q.dem.rsc '+demfile+' '+demrscfile+' '+top+' '+bot+' '+left+' '+right+' '+upsamplex+' '+upsampley

print (command)
ret = os.system(command)

#  clean up
command='rm q.dem q.dem.rsc'
ret=os.system(command)
