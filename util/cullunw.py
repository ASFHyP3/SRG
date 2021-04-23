#!/usr/bin/env python3

import sys
import os

fint=open('intlist','r')
fdone=open('finished','r')
ftodo=open('todo','w')

intlist=fint.readlines()
done=fdone.readlines()

for i in range(0,len(intlist)):
    words=intlist[i].split()
    intfile=words[-1]
    #    print intfile

    flag=0
    for j in range(0,len(done)):
        words=done[j].split()
        donefile=words[-1]
#        print intfile,donefile

        if intfile.replace('int','unw') == donefile:
            print ('found ',intfile)
            flag=1

    if flag ==0:
        print ('not found ',intfile)
        ftodo.write(intfile+'\n')

ftodo.close()

    
