#!/usr/bin/env python3
#
# split a long ziplist file, args are ziplist and number of gpus

import sys
import os
import string

zipfiles='ziplist'
if len(sys.argv)>1:
    zipfiles=sys.argv[1]
ngpus='1'
if len(sys.argv)>2:
    ngpus=sys.argv[2]

fziplist=open(zipfiles,'r')
ziplist=fziplist.readlines()
fziplist.close()

#  create list files
command='rm ziplist*'  #  delete old zip lists
print (command)
ret=os.system(command)

fd=[]
npar=int(ngpus)
npar=min(npar,len(ziplist))
print ('Creating ',npar,' zip lists')
for i in range(npar):
    fd.append(str(i))
    fd[i]=open('ziplist'+str(i),'w')

i=0
for zipfile in ziplist:
#    print (intfile)
    k=i % npar
    fd[k].write(zipfile)
    i=i+1

for i in range(npar):
    fd.append(str(i))
    fd[i].close()



