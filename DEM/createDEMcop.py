#!/usr/bin/env python3
#
#  from latlon.srtm30.py -- create a DEM of the full sentinel frame using Copernicus 30m dem
#
#

#  FUNCTION

import sys
import os
import math
import string

if len(sys.argv)< 7:
    print ('Usage: createDEMcop.py demfile demrscfile toplat botlat leftlon rightlon <upsamplex=1> < upsampley=1>')
    sys.exit(1)

outdemfile=sys.argv[1]
outdemrscfile=sys.argv[2]
upsamplex='1'
upsampley='1'
#print (len(sys.argv))
if len(sys.argv)>7:
    upsamplex=sys.argv[7]
if len(sys.argv)>8:
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

## taking both ascending, descending orbits into consideration
lat = max(coords[2],coords[0])
long = min (coords[1],coords[3])

x = abs(coords[3]-coords[1])+1
y = abs(coords[2]-coords[0])+1

#print 'lat long x y ',lat,long,x,y

#Find and download .dem files
filelist=[]  # initialize list of dem files
fdem=open('demfiles.txt','w')
fdem.close()

for i in range(lat-y,lat):
    for j in range(long,long+x):
        #print ('tile: ',i,j)
        command = '$PROC_HOME/DEM/cop_dem.py '+str(i)+' '+str(j)
        #print (command)
        ret=os.system(command)

# scan the rsc files for width, xstep parameters
fdem=open('demfiles.txt','r')
demlist=fdem.readlines()
fdem.close()
rsclist=[]
xwidth=[]
xstep=[]
widthmin=100000
xstepmax=0
widthmax=0
xstepmin=100000
frsc=open('demrscparams','w')
k=0
for i in range(lat-y,lat):
    for j in range(long,long+x):
        demfile=demlist[k].rstrip()
        rsclist.append(demfile.replace('.dem','.dem.rsc').rstrip())
        #print ('Looking for ',rsclist[k])
        if os.path.exists(rsclist[k]):
            #print ('found')
            file=open(rsclist[k],'r')
            strings=file.readlines()
            file.close()
            widthstr=strings[0].split()
            xstepstr=strings[4].split()
            ystepstr=strings[5].split()
            heightstr=strings[1].split()
            xwidth.append(widthstr[1])
            xstep.append(xstepstr[1])
            widthmin=min(widthmin,int(widthstr[1]))
            xstepmax=max(xstepmax,float(xstepstr[1]))
            widthmax=max(widthmax,int(widthstr[1]))
            xstepmin=min(xstepmin,float(xstepstr[1]))
            frsc.write(demfile+'\n')
            frsc.write(widthstr[1]+'\n')
            frsc.write(heightstr[1]+'\n')
            frsc.write(xstepstr[1]+'\n')
            frsc.write(ystepstr[1]+'\n')
        else:
            #print ('not found')
            frsc.write('no_file\n')
            frsc.write('0\n')
            frsc.write('0\n')
            frsc.write('0\n')
            frsc.write('0\n')
        k=k+1

frsc.close()                
#print (rsclist)
#print (xwidth)
#print (xstep)
#print (widthmin, xstepmax, widthmax, xstepmin)

# upsample all dems to max width
if os.path.exists('updem'):
    os.system('rm updem')
if os.path.exists('updem.rsc'):
    os.system('rm updem.rsc')

command = '$PROC_HOME/DEM/mosaicDEM demrscparams '+str(x)+' '+str(y)
print (command)
ret=os.system(command)
# and we need an rsc file to go with this
fd=open('updem.rsc','w')
fd.write('WIDTH         '+str(x*widthmax)+"\n")
fd.write('FILE_LENGTH   '+str(y*int(heightstr[1]))+"\n")
fd.write('X_FIRST       '+str(long)+"\n")
fd.write('Y_FIRST       '+str(lat)+"\n")
fd.write('X_STEP        '+str(xstepmin)+"\n")
fd.write('Y_STEP        '+ystepstr[1]+"\n")
fd.write('X_UNIT        degrees\n')
fd.write('Y_UNIT        degrees\n')
fd.write('Z_OFFSET      0\n')
fd.write('Z_SCALE       1\n')
fd.write('PROJECTION    LL\n')
fd.close()

# we have a full size dem, needs to be trimmed closer to scene size
latmax = max(finalcoords[0],finalcoords[2])
latmin = min(finalcoords[0],finalcoords[2])
lonmax = max(finalcoords[1],finalcoords[3])
lonmin = min(finalcoords[1],finalcoords[3])
#print ('latmax latmin lonmax lonmin ',latmax,latmin,lonmax,lonmin)

# trim the dem
top=str(latmax)
bot=str(latmin)
left=str(lonmin)
right=str(lonmax)
command = '$PROC_HOME/DEM/createspecialdem updem updem.rsc '+outdemfile+' '+outdemrscfile+'  '+top+' '+bot+' '+left+' '+right+' '+upsamplex+' '+upsampley

print (command)
ret = os.system(command)

#  clean up
command='rm updem updem.rsc'
ret=os.system(command)


sys.exit()
