#!/usr/bin/env python3

#  refpointsfromsim.py -- find stack reference points using cosine similarity

import sys
import os

if len(sys.argv) < 3:
    print ('Usage: refpointsfromsim.py igramlist length <psthresh> <simthresh>')
    sys.exit(1)

igramlist=sys.argv[1]
length=sys.argv[2]
psthresh='2'
simthresh='0.4'
if len(sys.argv) > 3:
    psthresh=sys.argv[3]
if len(sys.argv) > 4:
    simthresh=sys.argv[4]

# retrieve size of int/etc files from dem.rsc file
fe=open('dem.rsc','r')
words=fe.readline()
length=words.split()[1]
words=fe.readline()
lines=words.split()[1]
fe.close()

# find the ps and store in various formats
command="$PROC_HOME/ps/cosine_sim "+igramlist+" "+length+" refpointspssim "+psthresh+" "+simthresh
print(command)
ret=os.system(command)

# save as ref_locs files
command="$PROC_HOME/int/refpointsfromsim dummyarg "+length+" "+lines+" "+simthresh
print(command)
ret=os.system(command)
