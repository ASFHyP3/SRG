#!/usr/bin/env python3

import sys
import os
import math
import string

import subprocess

ver=sys.version_info

    


    

if len(sys.argv) <  2:
    print 'Usage: sentinel_back.py preciseorbitfile(*EOF) <length=value from real aperture proc>'
    sys.exit(1)

print len(sys.argv)

preciseorbitfile=sys.argv[1]
if len(sys.argv) > 2:
    length=sys.argv[2]
if len(sys.argv) == 2:
    #print 'reading rangesamples'
    fp=open('rangesamples','r')
    length=fp.readline()
    fp.close()

# Create a 'params' file
#params=open("params","w")
#p1 = os.getcwd()+"/elevation.dem"
#p2 = os.getcwd()+"/elevation.dem.rsc"
#params.write(p1+'\n')
#params.write(p2+'\n')
#params.close()
#print "DEM file set to elevation.dem"
#print "RSC file set to elevation.dem.rsc"

# Unzip *.zip files
#zipfiles = []
#basename = []
#for file in os.listdir("."):
#    if file.endswith(".zip"):
#            print file
#            command = 'unzip -u '+file
#            print (command)
#            ret = os.system(command)
#            basename.append(file[0:len(file)-4])
            # create the db file for this scene

#print basename

# Preprocess these with sentinel_preproc.py 
#command = '$PROC_HOME/sentinel_preproc.py '+preciseorbitfile+' '+length
#print (command)
#ret = os.system(command)

#  read in latlon bounds
fll=open('latlonlimits','r')
ll=fll.readlines()
fll.close()
latlons1=[]
latlons2=[]
latlons3=[]
latlons4=[]
for line in ll:
    words=line.split()
    #print words
    latlons1.append(words[1])
    latlons2.append(words[2])
    latlons3.append(words[3])
    latlons4.append(words[4])

#print ll
#print latlons1
#print latlons2
#print latlons3
#print latlons4


# create a position file for each burst
filelistburst = 'filelistburst'
command = 'ls -1 burst* | cat > '+filelistburst
print (command)
ret = os.system(command)

fburst=open('filelistburst','r')
burst=fburst.readlines()
fburst.close()
i=0
#print latlons1[0]
#print latlons2[1]
for line in burst:
    words=line.split()
    burstfile=words[-1]
    command = '$PROC_HOME/burst_angle_steering '+burstfile+' '+length
    print (command)
    proc = subprocess.Popen(command, stdout=subprocess.PIPE, shell=True)
    (geos, err) = proc.communicate()
    print 'results ',geos
    #  and backproject
    command = '$PROC_HOME/backproject db.'+burstfile+' slc '+geos
    print (command)
    ret = os.system(command)
    i=i+1

sys.exit(0)

# Create a list of db files
dbfilelist = 'dbfilelist'
command = 'ls -1 *db | cat > '+dbfilelist
print (command)
ret = os.system(command)
fdb=open('dbfilelist','r')
db=fdb.readlines()
fdb.close()

#  Loop over datasets, create dem if needed, then back project
ifile = 0
for line in db:
    ifile = ifile+1
    words=line.split()
    dbfile=words[-1]

    # set up the orbit data 
    con = sqlite3.connect(dbfile)
    c = con.cursor()
    leaderfile = sql_mod.valuec(c,'file','leader_file')
    rawdatafile = sql_mod.valuec(c,'file','raw_data_file')
    bytesperline = sql_mod.valuec(c,'file','bytes_per_line')
    command = '$PROC_HOME/sentineltiming '+leaderfile+' '+rawdatafile+' '+bytesperline
    print (command)
    ret = os.system(command)
    command = 'mv orbtiming '+dbfile.replace('db','orbtiming')
    print (command)
    ret = os.system(command)
    # insert orbinfo parameter
    sql_mod.add_param(c,'file','orbinfo')
    sql_mod.edit_param(c,'file','orbinfo',dbfile.replace('db','orbtiming'),'-','char','orbtiming file')
    # and position file
    sql_mod.add_param(c,'file','posfile')
    sql_mod.edit_param(c,'file','posfile',dbfile.replace('db','position'),'-','char','orbtiming file')
    # now timing limits
    ftime=open('firstaztime.out','r')
    azimuthtimesecondsrawdata=ftime.readline() # first time for raw data
    azimuthtimeinterval=ftime.readline()
    ftime.close()
    print azimuthtimesecondsrawdata, azimuthtimeinterval
    # offset for azimuth chirp length
    patchsize = sql_mod.valuef(c,'file','patch_size')
    validazsamples = sql_mod.valuef(c,'file','valid_az_samples')
    azimuthtimeseconds=str(float(azimuthtimesecondsrawdata)+(int(patchsize)-int(validazsamples))/2*float(azimuthtimeinterval))
    print 'Raw data starts at ',azimuthtimesecondsrawdata
    print 'SLC image starts at ',azimuthtimeseconds
    sql_mod.add_param(c,'file','azimuthTimeSecondsRawData')
    sql_mod.edit_param(c,'file','azimuthTimeSecondsRawData',azimuthtimesecondsrawdata,'-','float','first line azimuth time')
    sql_mod.add_param(c,'file','azimuthTimeSeconds')
    sql_mod.edit_param(c,'file','azimuthTimeSeconds',azimuthtimeseconds,'-','float','first line azimuth time')
    sql_mod.add_param(c,'file','azimuthTimeInterval')
    sql_mod.edit_param(c,'file','azimuthTimeInterval',azimuthtimeinterval,'-','float','first line azimuth time')

    con.commit()
    print 'orbinfo defined'

    # make a dem if needed
    if ifile == 1:
        command='$PROC_HOME/alos/alos_geo_slc/create_dem.py '+dbfile
        print (command)
        ret=os.system(command)
    
    # back project onto that dem
    command = '$PROC_HOME/alos/back/backproject '+dbfile+' '+dbfile.replace('db','slc')
    print (command)
    ret = os.system(command)

