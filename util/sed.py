#!/usr/bin/env python3

#  replacement for sed

import sys
import string

if len(sys.argv) < 1:
    print ('Usage: sed.py command filename')
    sys.exit(1)
    
com=sys.argv[1]
filename=sys.argv[2]

# parse the command
tokens=com.split('/')

if tokens[0]=='s':
    instr=tokens[1]
    outstr=tokens[2]
    if tokens[3]=='g':
        print ('replacing '+instr+' with '+outstr)
        # read stream from file
        f=open(filename,'r')
        strm=f.read()
        f.close()
        strm=strm.replace(instr,outstr)
        f=open(filename,'w')
        f.write(strm)
        f.close()

