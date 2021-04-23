#!/usr/bin/env python3
#
# split a long intlist file

import sys
import os
import string

intfiles='intlist'
if len(sys.argv)>1:
    intfiles=sys.argv[1]

fintlist=open(intfiles,'r')
intlist=fintlist.readlines()
fintlist.close()

#  create list files
fd=[]
npar=30
for i in range(npar):
    fd.append(str(i))
    fd[i]=open('intlist'+str(i),'w')

i=0
for intfile in intlist:
    print (intfile)
    k=i % npar
    fd[k].write(intfile)
    i=i+1

for i in range(npar):
    fd.append(str(i))
    fd[i].close()



