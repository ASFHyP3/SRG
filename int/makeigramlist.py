#!/usr/bin/env python3

# make a list of igrams to be formed

import sys
import os

if len(sys.argv) < 4:
    print 'Usage: makeigramlist.py slclist master_orbit(5 digits) slave_orbit(5 digits) igramlist'
    sys.exit(1)

slclist=sys.argv[1]
master_orbit=sys.argv[2]
slave_orbit=sys.argv[3]
igramlist=sys.argv[4]

f=open(slclist,'r')
files=f.readlines()
f.close()
f=open(igramlist,'w')

for i in range(0,len(files)):
    words=files[i].split()
    infile=words[0]
    slavefile=infile.replace(master_orbit,slave_orbit)
    f.write(infile+' '+slavefile+'\n')

f.close()

