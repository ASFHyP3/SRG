#!/usr/bin/env python3
#
# create difference interferograms

import sys
import os
import string

ver=sys.version_info

    


    

if len(sys.argv) < 4:
    print 'Usage: creatediffints.py dir1 dir2 len lines'
    sys.exit(0)

dir1=sys.argv[1]
dir2=sys.argv[2]
length=sys.argv[3]
lines=sys.argv[4]
print dir1,dir2,length,lines

fintlist=open('intlist','r')
intlist=fintlist.readlines()
fintlist.close()

for intfile in intlist:
    print intfile
    command='~/int/crossmulonly '+' '+dir1+'/'+intfile.strip()+' '+dir2+'/'+intfile.strip()+' '+intfile.strip()[0:len(intfile.strip())-4]+'.diff.int '+intfile.strip()[0:len(intfile.strip())-4]+'.diff.amp '+length+' '+lines+' 1 1 1'
    print (command)
    ret = os.system(command)


