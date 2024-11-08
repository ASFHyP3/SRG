#!/usr/bin/env python3

#  regress_igrams - remove elevation-dependent height from unwrapped igrams

import sys
ver=sys.version_info

    


    


import os
import sys
import math

if len(sys.argv) < 4:
    print ('Usage: regress_igrams.py unwlist demfile len lines location_file')
    sys.exit(1)

filelist=sys.argv[1]
demfile=sys.argv[2]
width=sys.argv[3]
lines=sys.argv[4]
locationfile=sys.argv[5]

fsbas=open(filelist,'r')
sbas=fsbas.readlines()
for line in sbas:
    unwfile = line.rstrip()
    words=line.split('/')
    outfile=words[-1].rstrip()
    if os.path.isfile(outfile)==False:
        command='$PROC_HOME/util/regressheight '+unwfile+' '+outfile+' '+demfile+' '+locationfile+' '+width+' '+lines
        print(command)
        ret=os.system(command)

sys.exit()

