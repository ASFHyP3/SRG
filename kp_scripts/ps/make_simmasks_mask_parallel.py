#!/usr/bin/env python3

#  make_simmasks_mask_parallel.py -- create ps mask for an input intlist, and many masks given optional input geolist

import sys
from datetime import datetime
import os
import subprocess
import glob
import math

if len(sys.argv) < 3:
    print ('Usage: make_simmasks_mask_parallel.py length geolist <scenemask=none> <intmaskglag=0> <psthresh=2> <simthresh=0.5>')
    sys.exit(1)

# Get input parameters
length=sys.argv[1]
geolist = sys.argv[2]

# Optional input parameters
scenemask = 'none'
intmaskflag = '0'
psthresh='2'
simthresh='0.5'
if len(sys.argv) > 3:
    scenemask = sys.argv[3]
if len(sys.argv) > 4:
    intmaskflag = sys.argv[4]
if len(sys.argv) > 5:
    psthresh=sys.argv[5]
if len(sys.argv) > 6:
    simthresh=sys.argv[6]

# 1. Open geolist
fgeo=open(geolist,'r')
geos=fgeo.readlines()
fgeo.close()

# 2. determine which similarity masks still need to be made
geo_sim = []
with open('geolist_for_sim','w') as fgeolist:
    simmasks = glob.glob('all_similarity_mask_*')
    for geo in geos:
        words = geo.strip().split('/')
        date = words[-1][:8]
        name = 'all_similarity_mask_'+date
        if name not in simmasks:
            fgeolist.write(geo)
            geo_sim.append(geo.rstrip())
geofiles = 'geolist_for_sim'

# If there are similarity masks to make, make them
if len(geo_sim)>0:
    # 3. Create multiple lists of geolists to parallelize
    command = '$PROC_HOME/util/splitintlist.py '+geofiles
    print(command)
    ret=os.system(command)

    # 4. Run make_simmasks_mask.py in parallel for each geolist generated in (3)
      # NOTE: the splitintlist.py script assumes your outputs are named intlist###. Rather than generate a new script, I've just kept the outputs as intlist## and call those. They get deleted at the end of this script
    list_geo = []
    for k in range(30):
        if os.path.isfile('intlist'+str(k)):
            list_geo.append('intlist'+str(k))

    num=0
    simcommand=[]
    prochome = os.getenv('PROC_HOME')
    for geo in list_geo:
        if os.path.getsize(geo.rstrip())>0:
            command = '$PROC_HOME/kp_scripts/ps/make_simmasks_mask.py '+geo+' '+length+' '+scenemask+' '+intmaskflag+' '+psthresh+' '+simthresh
            print('    '+command)
            simcommand.append(subprocess.Popen([prochome+'/kp_scripts/ps/make_simmasks_mask.py',geo,length,scenemask,intmaskflag,psthresh,simthresh]))
            num=num+1

    for i in range(num):
        simcommand[i].wait()

    print('\n  All Similarity Masks Have Been Generated!')
else:
    print('\n  All Similarity Masks Have Already Been Generated!')

command = 'rm geolist_for_sim'
print(command)
ret=os.system(command)

for k in range(30):
    if os.path.isfile('intlist'+str(k)):
        command = 'rm intlist'+str(k)
        ret=os.system(command)

