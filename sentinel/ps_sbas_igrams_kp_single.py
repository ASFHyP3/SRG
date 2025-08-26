#!/usr/bin/env python3

#  ps_sbas_igrams_kp_single - create set of sentinel interferogram subsets 
#    allow looks
#    create the intlist
#    don't subset if you don't need to

import sys
import string
import os
import math

if len(sys.argv) < 7:
    print ('Usage: ps_sbas_igrams_kp_single.py sbas_list dem_rsc_file xstart ystart xsize ysize indate <xlooks=1> <ylooks=xlooks>')
    sys.exit(1)

sbaslist=sys.argv[1]
demrscfile=sys.argv[2]
xstart=sys.argv[3]
ystart=sys.argv[4]
xsize=sys.argv[5]
ysize=sys.argv[6]
indate = sys.argv[7]
xlooks='1'

if len(sys.argv) > 8:
    xlooks=sys.argv[8]

ylooks=xlooks
if len(sys.argv) > 9:
    ylooks=sys.argv[9]
commonflag='n'

##### ----- GENERATE NEW DEM.RSC FILE FOR MULTILOOKED AND/OR EXTRACTED DEM ----- #####
command = '$PROC_HOME/util/make_ml_demrsc.py '+demrscfile+' '+xstart+' '+ystart+' '+xsize+' '+ysize+' '+xlooks+' '+ylooks
print('\n    '+command)
ret=os.system(command)

# Get original DEM parameters
with open(demrscfile,'r') as rsc:
    words=rsc.readlin()
    demwidth=words.split()[1]
    words=rsc.readline()
    demlength=words.split()[1]


# sbaslist
intlist=sbaslist.replace('sbas','int')
fintlist=open(intlist,'w')

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

    if primaryname != indate:
        if secondaryname != indate:
            continue

#    print 'primary secondary'
#    print primary
#    print secondary

    intfile=primaryname+'_'+secondaryname+'.int'
    ampfile=primaryname+'_'+secondaryname+'.amp'
    ccfile=primaryname+'_'+secondaryname+'.cc'

    # check to see if the file already exists
    fintlist.write(intfile+'\n')
    if os.path.isfile(intfile):
        continue
    #fintlist.write(intfile)
    #fintlist.write('\n')

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

