#!/usr/bin/env python3

# create a list of flat files from the list of sbas pairs

import sys
ver=sys.version_info

    


    

import os

if len(sys.argv) < 2:
    print 'Usage: flat_from_sbas_list.py sbas_list'
    sys.exit(1)

sbaslist=sys.argv[1]

fsbas=open(sbaslist,'r')

#  estimate baseline and create a file for the time-baseline plot
fflat=open('flatlist','w')

sbas=fsbas.readlines()
fsbas.close()
for i in range(0,len(sbas)):
    words=sbas[i].split()
    #print words

    file1=words[0]
    first=file1.find('20')
    name1=file1[first:first+8]
    file2=words[1]
    first=file2.find('20')
    name2=file2[first:first+8]
    flatfile=name1+'_'+name2+'.flat'+'\n'
    

    fflat.write(flatfile)

fflat.close()

