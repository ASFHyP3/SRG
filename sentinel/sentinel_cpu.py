#!/usr/bin/env python3
#
#  process stack of sentinel files to coregistered geocoded slcs from L0 data
#  then perhaps store geocoded slc product in the google cloud
#  
#  First, either download the zip files you want to process, then start this script:
#
#  sentinel_cpu(_cloud).py
#
#  backproject using cpu, not gpu, integration
#
#  Or, create a file with the suffix ".list" that contains the zip file granule names (i.e., starts with "S1A..." or "S1B...")
#  on each line.  If not logged in you willbe prompted for username/password in vertex/Earthdata system
#

import sys
import os
import string
import time
import subprocess
import argparse
import getpass
from datetime import datetime

# get the current environment
HOME = os.environ['PROC_HOME']

def main(Data_Dir: str, Output_Dir: str, username: str, password: str, pol: str):

    # and start
    print ('Processing stack of sentinel raw data products to coregistered geocoded slcs')

    # do we download more data from ASF?
    for file in os.listdir("."):
        if file.endswith(".list"):
            file = open(file)
            lines = file.readlines()
            for line in lines:
                command = "wget -P "+ HOME + "/mydata/ --user \"" + username + "\" --password \"" + password + "\" "
                if(line[0:3] == "S1A"):
                    command += "https://datapool.asf.alaska.edu/RAW/SA/" + line
                    print("WGET REQUEST: ", command)
                else:
                    command += "https://datapool.asf.alaska.edu/RAW/SB/" + line
                    print("WGET REQUEST: ", command)
                ret = os.system(command)


    # get the PATH of the script directory
    PATH=os.path.dirname(os.path.abspath(sys.argv[0]))

    # Create a 'params' file
    params=open("params","w")
    p1 = "elevation.dem"
    p2 = "elevation.dem.rsc"
    params.write(p1+'\n')
    params.write(p2+'\n')
    params.close()
    print ("DEM file set to elevation.dem")
    print ("RSC file set to elevation.dem.rsc")

    # get list of L0 products
    zipfiles = []
    SAFEnames = []
    for file in os.listdir(Data_Dir):
        if file.endswith(".zip"):
                zipfiles.append(file)
                command = 'unzip -u '+Data_Dir+file
                print (command)
                ret = os.system(command)
                SAFEnames.append(file[0:len(file)-4])

    print ("basenames: ",SAFEnames)

    # download the precise orbit files for these zips
    command = HOME+'/sentinel/sentinel_orbitfiles.py ' + Data_Dir+" --username \""+username+"\" --password \""+password+"\"" 
    print (command)
    ret=os.system(command)

    # list the precise orbit files
    ret=os.system('ls -1 *.EOF | cat > preciseorbitfiles')
    preciseorbitfiles=open('preciseorbitfiles','r')
    preciseorbitlist=preciseorbitfiles.readlines()
    preciseorbitfiles.close()

    print ('Precise orbit list:')
    print (preciseorbitlist)

    # loop over directories and process each with sentinel_back.py
    #   sentinel_back needs zipfile and precise orbit 
    for zipfile in zipfiles:
        #  which precise orbit file for this scene?
        print ('zipfile: ',zipfile)
        char1=zipfile.find('SSV_')
        if char1 < 0:
            char1=zipfile.find('SDV_')
            print ('This is a dual pol acquisition')
        else:
            print ('This is a single pol acquisition')
        char2=zipfile[char1:].find('T')
        scenedate=zipfile[char1+4:char1+char2]
        doy=datetime.strptime(scenedate, '%Y%m%d').timetuple().tm_yday
        year=scenedate[0:4]   # day of year and year for scene
        print ('doy ',doy,' ',year)
        if doy > 1:
            orbitfilestartdate = datetime.strptime(year+' '+str(doy-1),'%Y %j').strftime('%Y%m%d')
        else:
            lastdoy=datetime.strptime(str(int(year)-1)+'1231', '%Y%m%d').timetuple().tm_yday
            orbitfilestartdate = datetime.strptime(str(int(year)-1)+' '+str(lastdoy),'%Y %j').strftime('%Y%m%d')
        command='grep '+orbitfilestartdate+' preciseorbitfiles'
        proc = subprocess.Popen(command, stdout=subprocess.PIPE, shell=True)
        (orbitfilename, err) = proc.communicate()
        orbitfilename=orbitfilename.rstrip()
        orbitfilename=str(orbitfilename).lstrip('b')
        SAFEname=zipfile[0:len(zipfile)-4]

    #    create a DEM file if none exists
        create_dem=1
        for file in os.listdir("."):
            if file == 'elevation.dem':
                create_dem=0

        print ('create_dem= ',create_dem)

        if create_dem==1:
            command=HOME+"/sentinel/sentinel_coordinates_srtm30.py "+SAFEname
            #command=HOME+"/sentinel_coordinates_copernicus.py "+SAFEname  # use copernicus dem
            print (command)
            ret=os.system(command)
            
    #    and process the scene in you have the precise orbit
        if len(orbitfilename) < 1:
            print ('Missing precise orbit file for ',zipfile)
            command = 'echo skipping file, missing precise orbit'
        else:
            command=HOME+'/sentinel/sentinel_scene_cpu.py '+SAFEname+' '+orbitfilename.strip()+' '+pol

            print (SAFEname)
            print (str(orbitfilename.strip()))
            print (pol)
            print (command)
        ret=os.system(command)

    print ('Loop over scenes complete.')

    #  clean up a bit
    command = 'rm *.hgt* positionburst* dem* DEM* q*'
    ret=os.system(command)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        prog='sentinel_cpu',
    )
    parser.add_argument("--username", type=str,
                        help="hyp3 username")
    parser.add_argument('--password', type=str, 
                        help="hyp3 password")
    parser.add_argument('--polarization', type=str, default="vv",
                        help="Specify vv or vh Polarization. Default=vv")
    args = parser.parse_args()

    main(HOME+"/mydata/","output_dir", args.username, args.password, args.polarization)




