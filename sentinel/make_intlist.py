#!/usr/bin/env python3

#  make_intlist.py - generate intlist for all possible SLC primary images, based on SBAS list and geolist 
#    create the intlists (all in SBAS list, and if geolist given, individual dates)
#    For input into making the all_similarity_masks

import sys
import string
import os
import math

if len(sys.argv) < 2:
    print ('Usage: make_intlist.py sbas_list <geolist=none>')
    sys.exit(1)

sbaslist=sys.argv[1]
geolist='none'
if len(sys.argv)>2:
    geolist=sys.argv[2]

# first create the intlist based on the entire sbaslist
intlist='intlist'
fintlist=open('intlist','w')

sbasfiles=[]
fsbas=open(sbaslist,'r')
sbas=fsbas.readlines()
for line in sbas:
    words=line.split()
    primary=words[0]
    secondary=words[1]
    # get a short names for primary and secondary files
    first=primary.find('20')
    primaryname=primary[first:first+8]
    first=secondary.find('20')
    secondaryname=secondary[first:first+8]

    intfile=primaryname+'_'+secondaryname+'.int'

    # Write interferogram name to intlist
    fintlist.write(intfile+'\n')
    
fintlist.close()

if geolist != 'none':
    fgeos=open(geolist,'r')
    geos=fgeos.readlines()
    for geo in geos:
        words=geo.split('/')
        indate = words[-1][:8]
        print(indate)
    
        fintlist=open('intlist_'+indate,'w')
        for line in sbas:
            words=line.split()
            primary=words[0]
            secondary=words[1]
            # Get a short name for primary and secondary files
            first=primary.find('20')
            primaryname=primary[first:first+8]
            first=secondary.find('20')
            secondaryname=secondary[first:first+8]

            # Check to see if interferogram should be in this intlist
            if primaryname != indate:
                if secondaryname != indate:
                    continue

            intfile=primaryname+'_'+secondaryname+'.int'
            # Write interferogram name to intlist
            fintlist.write(intfile+'\n')
        fintlist.close()

    fgeos.close()
fsbas.close()

sys.exit()

