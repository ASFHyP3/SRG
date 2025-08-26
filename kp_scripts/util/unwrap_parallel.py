#!/usr/bin/env python3
#
#
#  unwrap_parallel.py -- unwrap multiple interferograms in parallel, up to 20 at a time
#
#   note: assume that unwlist, intlist, geolist all exist
#

import os
import subprocess
import sys
import glob

if len(sys.argv) <2:
    print ('Usage: unwrap_parallel.py intlist width <path_to_aux=.> <intmaskflag=N> <scenemask=none> <lowpass box size=1>')
    sys.exit(0)

### --- REQUIRED PARAMETERS --- ###
intfiles = sys.argv[1]
unwwidth = sys.argv[2]

### --- DEFAULT PARAMETERS --- ###
path_to_aux = '.'
intmaskflag = 'N'
scenemask = 'none'
box = '1'

### --- CHECK INPUTS --- ###
args = len(sys.argv)
if args>3:
    path_to_aux = sys.argv[3]

if args>4:
    intmaskflag = sys.argv[4]

if args>5:
    scenemask = sys.argv[5]

if args>6:
    box = sys.argv[6]

### --- RUN SCRIPT --- ###

# Read in the list of intlists
with open(intfiles,'r') as fint:
    ints = fint.readlines()

# compare intlist to unwlist and write out new intlist with files that have yet to be unwrapped
int_unw = []
with open('intlist_for_unwrapping','w') as fint:
    unws = glob.glob('*.unw*')
    for int_file in ints:
        name = int_file.strip().replace('.int','.unw')
       # name = name.replace('.unwerp','')
        if name not in unws:
            fint.write(int_file)
            int_unw.append(int_file.strip())
intfiles = 'intlist_for_unwrapping'

if len(int_unw)>0:
    # unwrap the interferograms, first creating multiple lists of them
    print('\n  Splitting input intlist into multiple files')
    command = '$PROC_HOME/util/splitintlist.py '+intfiles  # split igrams into many lists
    print('    '+command)
    ret = os.system(command)

    list_int = []
    for k in range(30):
        list_int.append('intlist'+str(k))

    # unwrap in parallel
    num=0
    unwcommand=[]
    prochome = os.getenv('PROC_HOME')
    for intlist in list_int:
        if os.path.getsize(intlist) > 0:
            command = prochome+'/kp_scripts/util/unwrap_igrams_mask.py '+intlist+' '+unwwidth+' '+path_to_aux+' '+intmaskflag+' '+scenemask+' '+box
            print('      '+command)
            unwcommand.append(subprocess.Popen([prochome+'/kp_scripts/util/unwrap_igrams_mask.py',intlist,unwwidth,path_to_aux,intmaskflag,scenemask,box]))
            num=num+1

    for i in range(num):
        unwcommand[i].wait()

    print('\n      Interferograms unwrapped!')

    print('      Cleaning Directory')
    for k in range(30):
        ret=os.system('rm intlist'+str(k))

else:
    print('\n      All interferograms already unwrapped!')

command = 'rm intlist_for_unwrapping'
print('      '+command)
ret=os.system(command)


