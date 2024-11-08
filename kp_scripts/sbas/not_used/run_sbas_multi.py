#!/usr/bin/env python3

# Organize files and run sbas_multi.f90 on interferograms from many lists

import sys
import subprocess
from datetime import datetime 
import os

if len(sys.argv) < 8:
    print('Usage: run_sbas_multi.py path2unwfiles sbaslist geolist path2geolists sbas_suffix len ref_locs')
    sys.exit(1)

path2unwfiles = sys.argv[1]
sbaslist = sys.argv[2]
geolist = sys.argv[3]
path2geolists = sys.argv[4]
suff = sys.argv[5]
length = int(sys.argv[6])
ref_locs=sys.argv[7]

##### ----- 1. USE SBASLIST TO MAKE THE UNWLIST ----- #####
fsbas = open(sbaslist,'r')
sbas = fsbas.readlines()
fsbas.close()

nunwfiles = len(sbas)
funw = open('unwlist_full','w')
for line in sbas:
    words = line.strip().split(' ')
    date1 = words[0].split('/')[-1][:8]
    date2 = words[1].split('/')[-1][:8]

    unwfile = path2unwfiles+date1+'_'+date2+'.unw'
    funw.write(unwfile+'\n')

print('unwlist_full written')
print('number of unwrapped interferograms in list: '+str(nunwfiles)+'\n')
funw.close()

##### ----- 2. READ INPUT GEOLIST AND STRIP OF PATH ----- #####
fgeo = open(geolist,'r')
geo = fgeo.readlines()
fgeo.close()

nslc = len(geo)
fgeostrip = open('geolist_strip','w')
for line in geo:
    geoname = line.strip().split('/')[-1]

    fgeostrip.write(geoname+'\n')
fgeostrip.close()

print('Geolist for input to sbas written')
print('Number of SLCs: '+str(nslc)+'\n')

##### ----- 3. CREATE LISTOFUNWLISTS ----- #####
command = 'ls '+path2geolists+'geolist_* |cat> listofgeolists'
print(command)
ret=os.system(command)

fgeolists = open('listofgeolists','r')
geolists = fgeolists.readlines()
fgeolists.close()

nunwlists = 0
funwlists = open('listofunwlists','w')
for line in geolists:
    if ((line.strip() != path2geolists+'geolist_full') and (line.strip() != path2geolists+'geolist_all')):
        sbas_name = line.strip().replace('geolist','sbas')+'_'+suff
        unwlist = line.strip().replace('geolist','unwlist')

        funwlists.write(unwlist+'\n')
        nunwlists+=1

        fsbas = open(sbas_name,'r')
        sbaslist = fsbas.readlines()
        fsbas.close()

        funw = open(unwlist,'w')
        for sbas in sbaslist:
            words = sbas.strip().split(' ')
            if len(words)>0:
                date1 = words[0].split('/')[-1][:8]
                date2 = words[1].split('/')[-1][:8]

                unwfile = path2unwfiles+date1+'_'+date2+'.unw'
                funw.write(unwfile+'\n')
        funw.close()
funwlists.close()

print('Unwlists and listofunwlists written!')
print('Number of unwlists: '+str(nunwlists)+'\n')


##### ----- 4. RUN SBAS_MULTI ----- #####
print('Running SBAS!')
command = '$PROC_HOME/sbas/sbas_multi unwlist_full '+str(nunwfiles)+' '+str(nslc)+' '+str(length)+' listofunwlists '+str(nunwlists)+' ref_locs'
print(command)
ret=os.system(command)
print('SBAS COMPLETE!')

