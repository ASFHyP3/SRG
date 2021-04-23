#!/usr/bin/env python3

#  cross_grams - create vv/vh cross igrams

import sys
ver=sys.version_info

    


    


import os
import sys
import math

if len(sys.argv) < 3:
    print 'Usage: cross_igrams.py intlist xsize ysize <xlooks=1> <ylooks=xlooks>'
    sys.exit(1)

sbaslist=sys.argv[1]
xsize=sys.argv[2]
ysize=sys.argv[3]
xlooks=1

if len(sys.argv) > 4:
    xlooks=sys.argv[4]

ylooks=xlooks
if len(sys.argv) > 5:
    ylooks=sys.argv[5]

# sbaslist

sbasfiles=[]
fsbas=open(sbaslist,'r')
sbas=fsbas.readlines()
for line in sbas:
    words=line.split()
    basename=words[0].replace('int','vv_vh')
    primary='../sentinelhawaii/sbas/'+words[0]
    secondary='../sentinelhawaiivh/sbas/'+words[0]
    print primary
    print secondary
    intfile=basename+'.int'
    ampfile=basename+'.amp'

    command='$PROC_HOME/int/crossmulonly '+primary+' '+secondary+' '+intfile+' '+ampfile+' '+xsize+' '+ysize+' 1 '+str(xlooks)+' '+str(ylooks)
    print (command)
    ret=os.system(command)

sys.exit()

