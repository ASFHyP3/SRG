#!/usr/bin/env python3

# take looks for a list of files

import sys
ver=sys.version_info

    


    

import os

if len(sys.argv) < 4:
    print 'Usage: looks_list.py filelist length looks-across <looks-down>'
    sys.exit(1)

filelist=sys.argv[1]
length=sys.argv[2]
looksac=sys.argv[3]

if len(sys.argv) < 5:
    looksdn=looksac
else:
    looksdn=sys.argv[4]

f=open(filelist,'r')
files=f.readlines()
f.close()

for i in range(0,len(files)):
    words=files[i].split()
    infile=words[0]
    k=infile.rfind('.')
    outfile=infile[0:k+2]
    #print len(words),words[0],len(files),k,infile,outfile
    command='$PROC_HOME/int/cpxlooks '+infile+' '+outfile+' '+length+' '+looksac+' '+looksdn
    print (command)
    ret=os.system(command)

