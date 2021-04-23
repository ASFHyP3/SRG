#!/usr/bin/env python3
#
# upsample elevation.dem and elevation.dem.rsc and set bounds by points

import sys
import os
import string

if len(sys.argv) < 5:
    print ('Usage: upsample_dem.py top-point(y) bot-point left-point(x) rightpoint <upsampx=1> <upsampy=upsampx>')
    sys.exit(0)

top=sys.argv[1]
bot=sys.argv[2]
left=sys.argv[3]
right=sys.argv[4]
upsampx='1'
if len(sys.argv) > 5:
    upsampx=sys.argv[5]
upsampy=upsampx
if len(sys.argv) > 6:
    upsampy=sys.argv[6]

print (top,bot,left,right,upsampx,upsampy)

command = 'mv elevation.dem elevation.dem.orig; mv elevation.dem.rsc elevation.dem.rsc.orig'
print (command)
ret = os.system(command)

command = '$PROC_HOME/util/makespecialdem elevation.dem.orig elevation.dem.rsc.orig elevation.dem elevation.dem.rsc '+top+' '+bot+' '+left+' '+right+' '+upsampx+' '+upsampy
print (command)
ret = os.system(command)

