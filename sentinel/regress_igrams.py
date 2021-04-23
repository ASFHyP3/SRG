#!/usr/bin/env python3

#  regress_igrams - remove elevation-dependent height from unwrapped igrams

import sys
ver=sys.version_info

    


    


import os
import sys
import math

if len(sys.argv) < 5:
    print ('Usage: regress_igrams.py unwlist demfile len lines location-file')
    sys.exit(1)

filelist=sys.argv[1]
demfile=sys.argv[2]
width=sys.argv[3]
lines=sys.argv[4]
locationfile=sys.argv[5]

fsbas=open(filelist,'r')
sbas=fsbas.readlines()
for line in sbas:
    words=line.split()
    unwfile=words[0]
#    print ('unwfile: ',unwfile)
    outfile=unwfile #.replace('unw','unw.reg')
    # deplane the file first
#    command='$PROC_HOME/util/deplane '+unwfile+' '+unwfile+' '+width+' '+lines
#    ret=os.system(command)
    command='$PROC_HOME/util/regressheight '+unwfile+' '+outfile+' '+demfile+' '+locationfile+' '+width+' '+lines #+' '+unwfile.replace('unw','int')
    ret=os.system(command)

sys.exit()

