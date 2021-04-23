#!/usr/bin/env python3

# sort the pegpoint list

import sys
ver=sys.version_info

    


    

import os

if len(sys.argv) < 3:
    print 'Usage: sort_pegpoints.py Peg_point_list.txt Peg_points_sorted.txt'
    sys.exit(1)

peglist=sys.argv[1]
pegsorted=sys.argv[2]

fpeg=open(peglist,'r')

fsort=open(pegsorted,'w')

pegs=fpeg.readlines()
fpeg.close()
date=[None]*len(pegs)
both=date
for i in range(0,len(pegs)):
    words=pegs[i].split()
    name=words[-1]
    first=name.find('20')
    date[i]=name[first:first+8]
    #print words
    peg=pegs[i]
    eol=peg.find('\n')
    both[i]=date[i]+peg[0:eol]

    #fflat.write(flatfile)

#fflat.close()
print both
pegsort=sorted(both)

for i in range(0,len(pegs)):
    entry=pegsort[i]
    print entry
    first=entry.find(' ')
    newpeg=entry[first:-1]
    fsort.write(newpeg+'\n')

fsort.close()
