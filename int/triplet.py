#!/usr/bin/env python3

# compute an insar triple for closure

import sys
ver=sys.version_info

    


    

import os

if len(sys.argv) < 3:
    print 'Usage: triplet.py slc1 slc2 slc3 length lines scale looks-across <looks-down>'
    sys.exit(1)

slc1=sys.argv[1]
slc2=sys.argv[2]
slc3=sys.argv[3]
scale=sys.argv[6]
length=sys.argv[4]
lines=sys.argv[5]
looksac=sys.argv[7]
if len(sys.argv) < 8:
    looksdn=looksac
else:
    looksdn=sys.argv[8]

# form interferograms first
command='$PROC_HOME/int/crossmul '+slc1+' '+slc2+' qint12 qamp12 '+length+' '+lines+' '+scale+' '+looksac+' '+looksdn
print (command)
ret=os.system(command)
command='$PROC_HOME/int/crossmul '+slc3+' '+slc2+' qint32 qamp32 '+length+' '+lines+' '+scale+' '+looksac+' '+looksdn
print (command)
ret=os.system(command)
command='$PROC_HOME/int/crossmul '+slc1+' '+slc3+' qint13 qamp13 '+length+' '+lines+' '+scale+' '+looksac+' '+looksdn
print (command)
ret=os.system(command)

lengthout=str(int(int(length)/int(looksac)))
linesout=str(int(int(lines)/int(looksdn)))
print 'Output interferogram triple length: ',lengthout

# compute the correlation files
command = '$PROC_HOME/int/makecc qint12 qamp12 cc12 '+lengthout
print (command)
ret = os.system(command)
command = '$PROC_HOME/int/makecc qint32 qamp32 cc32 '+lengthout
print (command)
ret = os.system(command)
command = '$PROC_HOME/int/makecc qint13 qamp13 cc13 '+lengthout
print (command)
ret = os.system(command)


# and now the triple
command='$PROC_HOME/int/crossmulonly qint12 qint32 qint123 qamp123 '+lengthout+' '+linesout+' '+scale
print (command)
ret=os.system(command)
command='$PROC_HOME/int/crossmulonly qint123 qint13 tripleint tripleamp '+lengthout+' '+linesout+' '+scale
print (command)
ret=os.system(command)
