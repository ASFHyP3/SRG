#!/usr/bin/env python3

#  ps_sbas_igrams - create set of sentinel interferogram subsets 
#    allow looks
#    create the intlist
#    don't subset if you don't need to

import sys
import string
import os
import math

if len(sys.argv) < 6:
    print ('Usage: ps_sbas_igrams.py sbas_list dem_rsc_file xstart ystart xsize ysize <xlooks=1> <ylooks=xlooks>')
    sys.exit(1)

sbaslist=sys.argv[1]
demrscfile=sys.argv[2]
xstart=sys.argv[3]
ystart=sys.argv[4]
xsize=sys.argv[5]
ysize=sys.argv[6]
xlooks=1

if len(sys.argv) > 7:
    xlooks=sys.argv[7]

ylooks=xlooks
if len(sys.argv) > 8:
    ylooks=sys.argv[8]
commonflag='n'
if len(sys.argv) > 10:
    commonflag=sys.argv[10]

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

# sbaslist

fintlist=open('intlist','w')

sbasfiles=[]
fsbas=open(sbaslist,'r')
sbas=fsbas.readlines()
for line in sbas:
    words=line.split()
    primary=words[0]
    secondary=words[1]
#  get a short names for primary and secondary files
    first=primary.find('20')
    primaryname=primary[first:first+8]
    first=secondary.find('20')
    secondaryname=secondary[first:first+8]

#    print 'primary secondary'
#    print primary
#    print secondary

    intfile=primaryname+'_'+secondaryname+'.int'
    ampfile=primaryname+'_'+secondaryname+'.amp'
    ccfile=primaryname+'_'+secondaryname+'.cc'

    fintlist.write(intfile)
    fintlist.write('\n')

    flag=0
    if int(xstart) == 1:
        if int(ystart) == 1:
            if int(xsize) == int(demwidth):
                if int(ysize) == int(demlength):
                   command='$PROC_HOME/int/crossmul '+primary+' '+secondary+' '+intfile+' '+ampfile+' '+xsize+' '+ysize+' 1.e-6 '+str(xlooks)+' '+str(ylooks)
                   print (command)
                   ret=os.system(command)
                   flag=1

    if flag == 0:
        command='$PROC_HOME/util/subsetmph '+primary+' primarypiece '+demwidth+' '+xstart+' '+ystart+' '+xsize+' '+ysize
        print (command)
        ret = os.system(command)
        command='$PROC_HOME/util/subsetmph '+secondary+' secondarypiece '+demwidth+' '+xstart+' '+ystart+' '+xsize+' '+ysize
        print (command)
        ret = os.system(command)
        command='$PROC_HOME/int/crossmul primarypiece secondarypiece '+intfile+' '+ampfile+' '+xsize+' '+ysize+' 1.e-6 '+str(xlooks)+' '+str(ylooks)
        print (command)
        ret=os.system(command)

    # correlation file next
    ret=os.system('$PROC_HOME/int/makecc '+' '+intfile+' '+ampfile+' '+ccfile+' '+str(int((int(xsize)/int(xlooks)))))

fintlist.close()

sys.exit()

