#!/usr/bin/env python3

# Turn an sbaslist into an intlist with full path to intfiles

import sys
import subprocess
from datetime import datetime 
import os

if len(sys.argv) < 2:
    print('Usage: make_intlist_from_sbas.py sbaslist <path2intfiles>')
    sys.exit(1)

sbaslist = sys.argv[1]
path2intfiles=''
if len(sys.argv) > 2:
    path2intfiles = sys.argv[2]


##### ----- 1. USE SBASLIST TO MAKE THE INTLIST ----- #####
fsbas = open(sbaslist,'r')
sbas = fsbas.readlines()
fsbas.close()

intlistname = sbaslist.replace('sbas','int')
fint = open(intlistname,'w')
for line in sbas:
    words = line.strip().split(' ')
    date1 = words[0].split('/')[-1][:8]
    date2 = words[1].split('/')[-1][:8]

    intfile = path2intfiles+date1+'_'+date2+'.int'
    fint.write(intfile+'\n')

print(intlistname+' written')
print('number of interferograms in list: '+str(len(sbas))+'\n')
fint.close()


