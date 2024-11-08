#!/usr/bin/env python3

# combine multiple sbas lists to find unique union of all of them

import sys
import subprocess
from datetime import datetime 
import os

if len(sys.argv) < 3:
    print('Usage: combine_sbas_lists.py listofsbas outfile')
    sys.exit(1)

listofsbas = sys.argv[1]
outfile = sys.argv[2]

# open listofsbas
fsbas = open(listofsbas,'r')
sbaslists = fsbas.readlines()
fsbas.close()

# Make sbas lists
sbas = []
for filename in sbaslists:
    print(filename.strip())
    # nearest neighbor
    fsbas = open(filename.strip(),'r')
    sbas_list = fsbas.readlines()
    for line in sbas_list:
        if line not in sbas:
            sbas.append(line)

fout = open(outfile,'w')
for line in sbas:
    fout.write(line)
fout.close()

print ('#### ---- FULL SBAS LISTS WRITTEN ----- #####')
