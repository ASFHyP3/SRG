#!/usr/bin/env python3
#
#
#  PR_singlemask.py -- Given a series of .geo files, generate SBAS time series with basic workflow
#                      - Each interferogram is treated identically
#                      - All pixels are used in unwrapping
#                      - Cosine_similarity index is used to find reference pixels for tropospheric correction & calibration
#                      - SBAS includes capabilities for consistent and inconsistent coverage
#
#
# INPUTS:

# FILE REQUIREMENTS:
  
# GENERATES:
    

import os
import subprocess
import sys
import time

##### ----- CHECK USAGE ----- #####

subdirname = sys.argv[1]
geolist_file = sys.argv[2]
path_to_GEO = sys.argv[3]
max_tb = sys.argv[4]
max_sb = sys.argv[5]
looksac = sys.argv[6]
looksdn = sys.argv[7]
scenemask = sys.argv[8]
INCONSISTENT = sys.argv[9]
psthresh = sys.argv[10]
simthresh = sys.argv[11]
refthresh = sys.argv[12]
TROPO_CORR = sys.argv[13]

##### ----- DEFAULT PARAMETERS ----- #####


##### ----- CHECK FOR INPUTS ----- #####

print('\n  You are reading geolist: '+geolist_file)
print('  You will be processing with the following parameters:')
print('    Max Temporal Baseline: '+max_tb)
print('    Max Spatial Baseline: '+max_sb)
print('    scenemask: '+scenemask)

if INCONSISTENT == 'N':
    print('    Coverage Flag: Consistent')
    flag = '0'
else:
    print('    Coverage Flag: Inconsistent')
    flag = '1'
print('    Tropospheric Correction: '+TROPO_CORR)

##### ----- RUN SCRIPT ----- #####
# 1. Make SBAS list given max_tb, max_sb, and geolist
print('\n  Making sbaslist and intlist')
command = '$PROC_HOME/kp_scripts/sentinel/sbas_list.py '+geolist_file+'_fullres '+path_to_GEO+' '+'sbaslist_'+max_tb+' '+max_tb+' '+max_sb
print('    '+command)
ret=os.system(command)

command = '$PROC_HOME/kp_scripts/sbas/intlist_from_sbas.py sbaslist_'+max_tb
print('    '+command)
ret=os.system(command)

# 2. Read in the params file and elevation.dem.rsc file
with open('params','r') as fpar:
   dem = fpar.readline().strip()
   rsc = fpar.readline().strip()

with open(rsc,'r') as frsc:
    demwidth = frsc.readline().strip().split()[-1]
    demlength = frsc.readline().strip().split()[-1]

with open('dem.rsc','r') as fe:
    unwwidth = fe.readline().strip().split()[-1]
    unwlength = fe.readline().strip().split()[-1]

# 3. Make the interferograms
print('\n  Making interferograms in sbaslist_'+max_tb)
command = '$PROC_HOME/kp_scripts/sentinel/ps_sbas_igrams.py sbaslist_'+max_tb+' '+rsc+' 1 1 '+demwidth+' '+demlength+' '+looksac+' '+looksdn
print('    '+command)
ret=os.system(command)

# 4. Make All Similarity Mask
suff = '_'+max_tb
print('\n  Making all_similarity_mask for intlist_'+max_tb+' and '+geolist_file)
command = '$PROC_HOME/kp_scripts/ps/cosine_sim_mask intlist_'+max_tb+' '+unwwidth+' pssim'+suff+' '+scenemask+' '+flag+' '+psthresh+' '+simthresh+' '+suff
print('    '+command)
ret=os.system(command)

# 5. Phase Reconstruction for the interferograms using the similarity mask
print('\n  Phase Reconstruction with all_similarity_mask_'+max_tb)
# Create the subdirectory if it doesn't exist and move there
if os.path.isdir(subdirname)==False:
    os.system('mkdir '+subdirname)

# Create an intlist for files that have not been reconstructed yet
fintnew = open('intlistTEMP','w')
INTS = []
with open('intlist_'+max_tb,'r') as fint:
    for name in fint.readlines():
        outname = subdirname+'/'+name.strip()
        if os.path.isfile(outname)==False:
            fintnew.write(name)
            INTS.append(name.strip())
fintnew.close()

# Run the interpolator
command = '$PROC_HOME/kp_scripts/ps/psfilter_hz.py intlistTEMP'+' '+unwwidth+' all_similarity_mask_'+max_tb
print('    '+command)
ret=os.system(command)

for name in INTS:
    namein = name+'.interp'
    ret=os.system('mv '+namein+' '+subdirname+'/'+name)

# Remove temporary list to keep things clean
ret=os.system('rm intlistTEMP')


# 6. Make ref_locs file
print('\n  Finding reference pixels from cosine similarity file')
simfile = 'all_similarity'+suff
scrfile = 'scr'+suff
out_reflocs = 'ref_locs'+suff
out_locs = 'locs'+suff

command = '$PROC_HOME/kp_scripts/int/refpointsfromsim '+simfile+' '+scrfile+' '+unwwidth+' '+unwlength+' '+out_reflocs+' '+out_locs+' '+refthresh
print('    '+command)
ret=os.system(command)

# 7. Unwrap Interferograms
print('\n  ##### ----- UNWRAPPING INTERFEROGRAMS IN PARALLEL ----- #####')

# Move to subdirectory and copy over important files
os.chdir(subdirname)

# Copy intlist and ref_locs to current directory
ret=os.system('cp ../intlist_'+max_tb+' .')
ret=os.system('cp ../sbaslist_'+max_tb+' .')
ret=os.system('cp ../'+out_reflocs+' .')
ret=os.system('cp ../'+scenemask+' .')
ret=os.system('cp ../'+geolist_file+' .')

# Unwrap
command = '$PROC_HOME/kp_scripts/util/unwrap_parallel.py intlist_'+max_tb+' '+unwwidth+' .. '+INCONSISTENT+' '+scenemask
print('    '+command)
ret=os.system(command)

# Make an unwlist file
funw = open('unwlist_'+max_tb,'w')
nints = 0
unwfiles = []
with open('intlist_'+max_tb,'r') as fint:
    for line in fint.readlines():
        outname = line.replace('.int','.unw')
        funw.write(outname)
        unwfiles.append(outname.strip())
        nints+=1
funw.close()
nints=str(nints)

# 8. Tropospheric Correction
if TROPO_CORR == 'Y':
    print('\n  ##### ----- APPLYING TROPOSPHERIC CORRECTION TO INTERFEROGRAMS ----- #####')
    if os.path.isdir('tropo_corr') == False:
        ret=os.system('mkdir tropo_corr')

    # Change to directory where you want the files saved
    os.chdir('tropo_corr')

    # Copy intlist to current directory and make corresponding unwlist
    ret=os.system('cp ../intlist_'+max_tb+' .')
    with open('intlist_'+max_tb,'r') as fint:
        ints = fint.readlines()

    with open('unwlistTEMP','w') as funw:
        for name in ints:
            unwfile = '../'+name.replace('.int','.unw')
            funw.write(unwfile)

    # Run the regression
    command = '$PROC_HOME/kp_scripts/util/regress_igrams.py unwlistTEMP ../../dem '+unwwidth+' '+unwlength+' ../ref_locs_'+max_tb+' '+INCONSISTENT
    print('    '+command)
    ret=os.system(command)

    # Remove temporary unwlist since it points to the wrong files for further processing
    ret=os.system('rm unwlistTEMP')

    ret=os.chdir('..')

# 9. SBAS Setup
print('\n  ##### ----- MAKING SBAS SETUP FILES ----- #####')
command = '$PROC_HOME/kp_scripts/sbas/sbas_setup.py sbaslist_'+max_tb+' '+geolist_file+' _'+max_tb
print('    '+command)
ret=os.system(command)

# Determine the number of slcs from the geolist
with open(geolist_file,'r') as fgeo:
    nslc=str(len(fgeo.readlines()))
    


# 10. Run SBAS
print('\n  ##### ----- RUNNING SBAS ON ORIGINAL INTERFEROGRAMS ----- #####')
suffix = '_'+max_tb
unwlistin = 'unwlist'+suffix
Tmfile = 'Tm'+suffix+'.out'
timedeltafile = 'timedeltas'+suffix+'.out'
deltimefile = 'deltime'+suffix+'.out'
reflocs_file = 'ref_locs'+suffix
auxdir = '../'
if TROPO_CORR == 'Y':
    unwlistin = '../'+unwlistin
    Tmfile = '../'+Tmfile
    timedeltafile = '../'+timedeltafile
    deltimefile = '../'+deltimefile
    reflocs_file = '../'+reflocs_file
    scenemask = '../'+scenemask
    ret=os.chdir('tropo_corr')

    auxdir = '../../'

# Temporarily move auxiliary files (.amp and (possible) .mask) to current directory
for unw in unwfiles:
   command = 'mv '+auxdir+unw.replace('.unw','.amp')+' .'
   ret = os.system(command)
   if INCONSISTENT=='Y':
       command = 'mv '+auxdir+unw.replace('.unw','.mask')+' .'
       ret=os.system(command)
    
# Run the SBAS script
command = '$PROC_HOME/kp_scripts/sbas/sbas_unique '+unwlistin+' '+nints+' '+nslc+' '+unwwidth+' '+Tmfile+' '+timedeltafile+' '+deltimefile+' '+reflocs_file+' '+scenemask+' '+INCONSISTENT
print('    '+command)
ret=os.system(command)

# Move files back
os.system('mv *.amp '+auxdir)
if INCONSISTENT=='Y':
    os.system('mv *.mask '+auxdir)

# Move back to SBAS directory
if TROPO_CORR == 'Y':
    os.chdir('..')

os.chdir('..')

print('\n  !!!!! ----- PHASE RECONSTRUCTION WITH SINGLE SIMILARITY MASK & SBAS WORKFLOW COMPLETE ----- !!!!!')

sys.exit(0)



