#!/usr/bin/env python3

#  make_ml_demrsc - create a new demrsc file based on the input parameters provided
#    allow looks
#    create the intlist
#    don't subset if you don't need to

import sys
import string
import os
import math

if len(sys.argv) < 6:
    print ('Usage: make_ml_demrsc.py dem_rsc_file xstart ystart xsize ysize <xlooks=1> <ylooks=xlooks>')
    sys.exit(1)

# Default Parameters
xlooks = '1'

# Get Required Inputs
demrscfile=sys.argv[1]
xstart=sys.argv[2]
ystart=sys.argv[3]
xsize=sys.argv[4]
ysize=sys.argv[5]

if len(sys.argv) > 6:
    xlooks=sys.argv[6]

ylooks=xlooks
if len(sys.argv) > 7:
    ylooks=sys.argv[7]

##### ----- GENERATE NEW DEM.RSC FILE FOR MULTILOOKED AND/OR EXTRACTED PORTION ----- #####
# rsc file for extracted portion
frsc=open('dem.rsc','w')

# dem params
rsc=open(demrscfile,'r')
words=rsc.readline()
demwidth=words.split()[1]
frsc.write(words.replace(demwidth,str(int(int(xsize)/int(xlooks))))) # width
words=rsc.readline()
demlength=words.split()[1]
frsc.write(words.replace(demlength,str(int(int(ysize)/int(ylooks))))) #length
wordsxfirst=rsc.readline()
wordsyfirst=rsc.readline()
wordsxstep=rsc.readline()
demxstep=wordsxstep.split()[1]
wordsystep=rsc.readline()
demystep=wordsystep.split()[1]
xstep=str(float(demxstep)*int(xlooks))
ystep=str(float(demystep)*int(ylooks))
demxfirst=wordsxfirst.split()[1]
xfirst=str(float(demxfirst)+(int(xstart)-1)*float(demxstep))
frsc.write(wordsxfirst.replace(demxfirst,xfirst)) # x_first
demyfirst=wordsyfirst.split()[1]
yfirst=str(float(demyfirst)+(int(ystart)-1)*float(demystep))
frsc.write(wordsyfirst.replace(demyfirst,yfirst)) # y_first
frsc.write(wordsxstep.replace(demxstep,xstep)) # x_step
frsc.write(wordsystep.replace(demystep,ystep)) # y_step
words=rsc.readline()
frsc.write(words) # x_unit
words=rsc.readline()
frsc.write(words) # y_unit
words=rsc.readline()
frsc.write(words) # z_offset
words=rsc.readline()
frsc.write(words) # z_scale
words=rsc.readline()
frsc.write(words) # projection
words='xstart         '+xstart+'\n'
frsc.write(words)
words='ystart         '+ystart+'\n'
frsc.write(words)
words='xsize          '+xsize+'\n'
frsc.write(words)
words='ysize          '+ysize+'\n'
frsc.write(words)
rsc.close()

