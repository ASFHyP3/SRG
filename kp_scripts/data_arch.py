#!/usr/bin/env python3
#
#
#  data_arch.py -- generate subdirectories for a study area, create DEM, and do file management
#
#
# INPUTS:
	# summary_file: file with study area information (default = 'summary_paths'
# GENERATES:
	# subdirectories: ascending, descending
	# sub-subdirectories: path_[number]
	# creates file 'subdirectores' which lists the full paths to each subdirectory
	# elevation.dem for study area
	# elevation.dem.rsc for study area
	# creates 'params' file for DEM files and copies to path_[number] subdirectories
	# copies fileids for each path_[number] to scenelist in appropriate subdirectory
	# puts extra DEM files into directory DEM

import os
import subprocess
import sys
import time

##### ----- DEFAULT PARAMETERS ----- #####
sum_file = 'summary_paths'
upsample_x = '6'     # 30 m dem / 6 = 5m posting (approximate Sentinel-1 resolution in range = 5 m)
upsample_y = '2'     # 30m dem / 2 = 15m posting (approximage Sentinel-1 resolution in azimuth = 15 m)

##### ----- CHECK FOR INPUTS ----- #####
if len(sys.argv)>=2:
    sum_file = str(sys.argv[1])

if len(sys.argv)>=3:
    upsample_x = str(sys.argv[2])

if len(sys.argv)>=4:
    upsample_y = str(sys.argv[3])

##### ----- RUN SCRIPT ----- #####
print ( "\n"+"##### ----- Set up data architecture for your study area ----- #####"+"\n")

# Read the data from summary file
print('    Reading summary file: '+sum_file)
fid = open(sum_file,'r')
f = fid.readlines()
fid.close()

# Get extents to create the special DEM (upsampled to approx. native posting of the satellite, 5x15 m)
print('      Study Area Extents:')
min_lon = f[0].split(':')[1].replace('\n','')
max_lon = f[1].split(':')[1].replace('\n','')
min_lat = f[2].split(':')[1].replace('\n','')
max_lat = f[3].split(':')[1].replace('\n','') 
print('        Minimum Longitude: '+min_lon)
print('        Maximum Longitude: '+max_lon)
print('        Minimum Latitude: '+min_lat)
print('        Maximum Latitude: '+max_lat+'\n')

# Check to see if elevation.dem/elevation.dem.rsc already exists
dem_flag = 0    # 0=correct DEM does not exist; 1=correct DEM exists
if os.path.isfile('elevation.dem.rsc') == True:
    # Check DEM input parameters
    input_file = 'DEM_input_params'
    if os.path.isfile(input_file) == True:
        with open(input_file,'r') as params:
            lines = params.readlines()
        xmin = lines[0].split(':')[-1].strip()
        xmax = lines[1].split(':')[-1].strip()
        ymin = lines[2].split(':')[-1].strip()
        ymax = lines[3].split(':')[-1].strip()
        xup = lines[4].split(':')[-1].strip()
        yup = lines[5].split(':')[-1].strip()
        # If existing DEM_input_params file matches input parameters to script, set dem_flag = 1 (exists)
        if (xmin==min_lon) and (xmax==max_lon) and (ymin==min_lat) and (ymax==max_lat) and (xup==upsample_x) and (yup==upsample_y):
            dem_flag = 1
            with open('Update_Files_Flag','w') as flag:
                flag.write('UPDATE_EXISTING: N')
        else:
            with open('Update_Files_Flag','w') as flag:
                flag.write('UPDATE_EXISTING: Y')
else:
    with open('Update_Files_Flag','w') as flag:
        flag.write('UPDATE_EXISTING: N')
            

# If elevation.dem.rsc does not exist, create a DEM
if dem_flag==0:
    command = '$PROC_HOME/DEM/createDEMcop.py elevation.dem elevation.dem.rsc '+max_lat+' '+min_lat+' '+min_lon+' '+max_lon+' '+upsample_x+' '+upsample_y
    print('    Creating DEM')
    print('      Upsampling 30 m DEM by '+upsample_x+' across and '+upsample_y+' down')
    print('        '+command)
    ret = os.system(command)
    print('\n      DEM created!')
    print('      Cleaning up directory:')
    command ='mkdir DEM'
    print('        '+command)
    ret = os.system(command)
    command = 'mv *.geoid dem* no_file latloncoords DEM'
    print('        '+command)
    ret = os.system(command)

    # create the params file
    print('\n      DEM file set to elevation.dem')
    print('      DEM resource (rsc) file set to elevation.dem.rsc')
    print('      Writing params file\n')
    with open('params','w') as params:
        params.write(os.getcwd()+'/elevation.dem'+'\n')
        params.write(os.getcwd()+'/elevation.dem.rsc'+'\n')

    with open('DEM_input_params','w') as demparams:
        demparams.write('XMIN: '+min_lon+'\n')
        demparams.write('XMAX: '+max_lon+'\n')
        demparams.write('YMIN: '+min_lat+'\n')
        demparams.write('YMAX: '+max_lat+'\n')
        demparams.write('Xupsample: '+upsample_x+'\n')
        demparams.write('Yupsample: '+upsample_y+'\n')
else:
    print('\n    DEM with matching parameters already exists, skipping creation!\n')

print('    Creating Data Architecture')
# Create data architecture based on the path lines
subdir = open('subdirectories','w')
for i in range(len(f)):
    if i<7:
        continue
    words = f[i].split(',')
    direction = words[1]
    if os.path.isdir(direction) == False:
        command = 'mkdir '+direction
        print('        '+command)
        ret = os.system(command)
    path_num = words[0]
    path_dir = direction+'/path_'+path_num
    subdir.write(os.getcwd()+'/'+path_dir+'\n')
    if os.path.isdir(path_dir) == False:
        command = 'mkdir '+path_dir
        print('        '+command)
        ret = os.system(command)
    fileidlist = 'fileids_'+direction+'_'+path_num
    if os.path.isfile(fileidlist)==True:
        command = 'cp '+fileidlist+' '+path_dir+'/scenelist'
        print('        '+command)
        ret = os.system(command)
    else:
        print('        WARNING: Cannot copy '+fileidlist+' to appropriate subdirectory because file does not exist!')
    command = 'cp params '+path_dir
    print('        '+command)
    ret = os.system(command)
subdir.close()

print('\n    Data architecture setup complete!')



