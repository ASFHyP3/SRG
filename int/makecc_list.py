#!/usr/bin/env python3

# create .c correlation file from a list

import sys
ver=sys.version_info

    


    

import os

if len(sys.argv) < 3:
    print 'Usage: makecc_list.py filelist length '
    sys.exit(1)

filelist=sys.argv[1]
length=sys.argv[2]

f=open(filelist,'r')
files=f.readlines()
f.close()

for i in range(0,len(files)):
    words=files[i].split()
    infile=words[0]
    k=infile.rfind('.')
    outfile=infile[0:k+1]
    #print len(words),words[0],len(files),k,infile,outfile
    command='$PROC_HOME/int/makecc '+infile+' '+outfile+'a '+outfile+'c '+length
    print (command)
    ret=os.system(command)

