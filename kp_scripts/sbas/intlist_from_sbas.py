#!/usr/bin/env python3

#  intlist_from_sbas.py - write out intlist, either full set or subset the input sbaslist to include only interferograms that contain the optional input date 

import sys
import string
import os
import math

if len(sys.argv) < 2:
    print ('Usage: intlist_from_sbas.py sbas_list <indate=none>')
    sys.exit(1)

sbaslist=sys.argv[1]
indate='none'
if len(sys.argv) > 2:
    indate = sys.argv[2]

# Output sbaslist and intlist
intlist=sbaslist.replace('sbas','int')
if indate != 'none':
    intlist = 'intlist_'+indate
fintlist=open(intlist,'w')

# Read in full sbaslist
with open(sbaslist,'r') as fsbas:
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

    # If an input date was given, check to see if it should go in the list
    if indate != 'none':
        if primaryname != indate:
            if secondaryname != indate:
                continue

    intfile=primaryname+'_'+secondaryname+'.int'

    fintlist.write(intfile+'\n')

fintlist.close()

sys.exit()

