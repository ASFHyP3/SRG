#!/usr/bin/env python3

# compute the resamp_elevation file for elevation-dependent resampling

import sys
ver=sys.version_info

    


    


import os
import sys
import math

if len(sys.argv) < 3:
    print 'Usage: create_resamp_elevation.py masterdb slavedb'
    sys.exit(1)
    
masterdb=sys.argv[1]
slavedb=sys.argv[2]

#  read values from common peg point file
pegfile = open("pegpointfile",'r')
words = pegfile.readline().split()
lat=words[0]
words = pegfile.readline().split()
lon=words[0]
words = pegfile.readline().split()
hdg=words[0]
words = pegfile.readline().split()
rc=words[0]
words = pegfile.readline().split()
ht=words[0]
words = pegfile.readline().split()
vel=words[0]
pegfile.close()

# open the database files to use here
con1 = sqlite3.connect(masterdb)
con2 = sqlite3.connect(slavedb)

# create cursor
c1 = con1.cursor()
c2 = con2.cursor()

# create a resamp_elavation table to set the correction parameters
sql_mod.add_tbl(c2,'resamp_elevation')  # create table

# start with mocompbaseline parameters
sql_mod.add_param(c2,'resamp_elevation','peg_latitude')  # add a param
sql_mod.add_param(c2,'resamp_elevation','peg_longitude')  # add a param
sql_mod.add_param(c2,'resamp_elevation','peg_heading')  # add a param
sql_mod.add_param(c2,'resamp_elevation','orbit_sch1')  # add a param
sql_mod.add_param(c2,'resamp_elevation','orbit_sch2')  # add a param
sql_mod.add_param(c2,'resamp_elevation','mocomp_position_file1')  # add a param
sql_mod.add_param(c2,'resamp_elevation','mocomp_position_file2')  # add a param
sql_mod.add_param(c2,'resamp_elevation','ht')  # add a param
sql_mod.edit_param(c2,'resamp_elevation','peg_latitude',lat,'rad','real*8','mocomp peg point')
sql_mod.edit_param(c2,'resamp_elevation','peg_longitude',lon,'rad','real*8','mocomp peg point')
sql_mod.edit_param(c2,'resamp_elevation','peg_heading',hdg,'rad','real*8','mocomp peg point')
mastersch=masterdb.replace('db','position.sch')
slavesch=slavedb.replace('db','position.sch')
print mastersch
print slavesch
sql_mod.edit_param(c2,'resamp_elevation','orbit_sch1',mastersch,'-','char','')
sql_mod.edit_param(c2,'resamp_elevation','orbit_sch2',slavesch,'-','char','')
sql_mod.edit_param(c2,'resamp_elevation','mocomp_position_file1',masterdb.replace('db','position_mocomp'),'-','char','')
sql_mod.edit_param(c2,'resamp_elevation','mocomp_position_file2',slavedb.replace('db','position_mocomp'),'-','char','')
sql_mod.edit_param(c2,'resamp_elevation','ht',ht,'m','real*8','orbit altitude')
con2.commit()

# now add what is required for resamp_elevation correction
sql_mod.add_param(c2,'resamp_elevation','range_samples1')  # add a param
nr=sql_mod.valuei(c1,'file','number_of_range_bins')
sql_mod.edit_param(c2,'resamp_elevation','range_samples1',nr,'pixels','integer*4','')

sql_mod.add_param(c2,'resamp_elevation','patch_size')  # add a param
patch_size=sql_mod.valuei(c1,'file','patch_size')
sql_mod.edit_param(c2,'resamp_elevation','patch_size',patch_size,'lines','integer*4','')

sql_mod.add_param(c2,'resamp_elevation','valid_az_samples')  # add a param
na_valid=sql_mod.valuei(c1,'file','valid_az_samples')
sql_mod.edit_param(c2,'resamp_elevation','valid_az_samples',na_valid,'-','integer*4','')

sql_mod.add_param(c2,'resamp_elevation','number_of_lines')  # add a param
datafileraw=sql_mod.valuec(c1,'file','raw_data_file')
linelength1=sql_mod.valuei(c1,'file','bytes_per_line')
lines_data=int(os.path.getsize(datafileraw)/float(linelength1))
lines=int(lines_data-(patch_size-na_valid))
print "Valid lines in slc: ",lines,"\n"
sql_mod.edit_param(c2,'resamp_elevation','number_of_lines',lines,'-','integer*4','')

sql_mod.add_param(c2,'resamp_elevation','slant_range_pixel_spacing')  # add a param
fs=sql_mod.valuef(c1,'file','fs')
dr=299792458./2./fs
sql_mod.edit_param(c2,'resamp_elevation','slant_range_pixel_spacing',dr,'m','real*8','')

sql_mod.add_param(c2,'resamp_elevation','r0')  # add a param
nearrange1=sql_mod.valuef(c1,'file','r0')
sql_mod.edit_param(c2,'resamp_elevation','r0',nearrange1,'m','real*8','near range, m')

sql_mod.add_param(c2,'resamp_elevation','rc')  # add a param
sql_mod.edit_param(c2,'resamp_elevation','rc',rc,'m','real*8','radius of curvature')

sql_mod.add_param(c2,'resamp_elevation','velocity')  # add a param
sql_mod.edit_param(c2,'resamp_elevation','velocity',vel,'m/s','real*8','velocity')

sql_mod.add_param(c2,'resamp_elevation','range_looks')  # add a param
sql_mod.add_param(c2,'resamp_elevation','azimuth_looks')  # add a param
sql_mod.edit_param(c2,'resamp_elevation','range_looks',1,'looks','integer','one look')
sql_mod.edit_param(c2,'resamp_elevation','azimuth_looks',1,'looks','integer','one look')

sql_mod.add_param(c2,'resamp_elevation','fd')  # add a param
fd=sql_mod.valuef(c1,'file','fd')
sql_mod.edit_param(c2,'resamp_elevation','fd',fd,'prfs','real*8','fd, prfs')
sql_mod.add_param(c2,'resamp_elevation','prf')  # add a param
prf1=sql_mod.valuef(c1,'file','prf')
sql_mod.edit_param(c2,'resamp_elevation','prf',prf1,'Hz','real*8','prf of master')

sql_mod.add_param(c2,'resamp_elevation','wavelength')  # add a param
wvl=sql_mod.valuef(c1,'file','wvl')
sql_mod.edit_param(c2,'resamp_elevation','wavelength',wvl,'m','real*8','')

sql_mod.add_param(c2,'resamp_elevation','demfile')  # add a param
sql_mod.add_param(c2,'resamp_elevation','demrscfile')  # add a param
# dem info in params file
f = open("params",'r')
words = f.readline().split()
demfile=words[0]
words = f.readline().split()
demrscfile=words[0]
f.close()
sql_mod.edit_param(c2,'resamp_elevation','demfile',demfile,'-','char','')
sql_mod.edit_param(c2,'resamp_elevation','demrscfile',demrscfile,'-','char','')

con2.commit()

# close cursors
c1.close()
c2.close()

# close connections
con1.close()
con2.close()

# resamp with elevation resampling correction next

ret=os.system("/home/zebker/orbit/mocompbaseline "+slavedb+" resamp_elevation mocompbaseline.out") # need baseline info

###ret=os.system("/home/zebker/stdproc/mocomp/resamp_elevation "+slavedb+" resamp_elevation")

create_dem=1
for file in os.listdir('.'):
    if file == 'latloncoords':
        create_dem=0

ret=os.system("/home/zebker/alos/monica/resamp_elevation_part1 "+slavedb+" resamp_elevation")
if create_dem==1:
    ret=os.system('/home/zebker/alos/monica/latlon.py')

#ret=os.system("/home/mafinn/iscript/resamp_elevation "+slavedb+" resamp_elevation")
ret=os.system("/home/zebker/stdproc/mocomp/resamp_elevation "+slavedb+" resamp_elevation")



