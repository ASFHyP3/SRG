#!/usr/bin/env python3
#
#  process stack of sentinel files to coregistered geocoded slcs from L0 data
#  then store geocoded slc product in the google cloud
#  
#  First, either download the zip files you want to process, then start this script:
#
#  sentinel_gpu_cloud.py
#
#  backproject using gpu integration
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
from datetime import datetime

# get the current environment
HOME = os.environ['PROC_HOME']


def main(data_dir: str, username: str, password: str, pol: str, use_existing_data=False):

    # and start
    print ('Processing stack of sentinel raw data products to coregistered geocoded slcs')

    #  grab the least used gpu
    proc = subprocess.Popen(HOME+"/sentinel/bestgpu.sh",stdout=subprocess.PIPE, shell=True)
    (bestGPU,err)=proc.communicate()

    bestGPU=str(int(bestGPU.strip()))
    print ('GPU number: ',bestGPU)
    os.environ['CUDA_DEVICE_ORDER'] = 'PCI_BUS_ID'
    os.environ['CUDA_VISIBLE_DEVICES'] = '0'

    # do we download more data from ASF?
    if(not use_existing_data):
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


    # and start
    print ('Processing stack of sentinel raw data products to coregistered geocoded slcs')

    # get the PATH of the script directory
    PATH=os.path.dirname(os.path.abspath(sys.argv[0]))

    # Create a 'params' file
    params=open("params","w")
    p1 = os.getcwd()+"/elevation.dem"
    p2 = os.getcwd()+"/elevation.dem.rsc"
    params.write(p1+'\n')
    params.write(p2+'\n')
    params.close()
    print ("DEM file set to elevation.dem")
    print ("RSC file set to elevation.dem.rsc")

    # get list of L0 products
    zipfiles = []
    SAFEnames = []
    for file in os.listdir(data_dir):
        if file.endswith(".zip"):
                zipfiles.append(file)
                command = 'unzip -u '+data_dir+file
                print (command)
                ret = os.system(command)
                SAFEnames.append(file[0:len(file)-4])

    print ("zipfiles: ",zipfiles)
    print ("basenames: ",SAFEnames)

    # download the precise orbit files for these zips
    command = HOME+'/sentinel/sentinel_orbitfiles.py ' + data_dir +" --username \""+username+"\" --password \""+password+"\"" 
    print (command)
    ret=os.system(command)

    # list the precise orbit files
    ret=os.system('ls -1 *.EOF | cat > preciseorbitfiles')
    preciseorbitfiles=open('preciseorbitfiles','r')
    preciseorbitlist=preciseorbitfiles.readlines()
    preciseorbitfiles.close()

    print ('Precise orbit list:')
    print (preciseorbitlist)
    #print

    # loop over directories and process each with sentinel_back.py
    #   sentinel_back needs zipfile and precise orbit 
    for zipfile in zipfiles:
        #  which precise orbit file for this scene?
        print ('zipfile: ',zipfile)
        #  H or V pol
        char1=zipfile.find('SSV_')
        if char1 < 0:
            char1=zipfile.find('SDV_')
            print ('This is a dual pol acquisition')
        else:
            print ('This is a single pol acquisition')
        if char1 < 0:
            char1=zipfile.find('SSH_')
            if char1 < 0:
                char1=zipfile.find('SDH_')
                print ('This is a dual pol acquisition')
            else:
                print ('This is a single pol acquisition')
            #  if horizontal switch polarization (this is a hack, to be improved
            if pol == 'vv':
                pol='hh'
        char2=zipfile[char1:].find('T')
        scenedate=zipfile[char1+4:char1+char2]
    #    orbitfilestartdate=int(scenedate)-1
    #    orbitfilestopdate=int(scenedate)+1
    #    orbitfilename = 'no_precise_orbit'
        doy=datetime.strptime(scenedate, '%Y%m%d').timetuple().tm_yday
        year=scenedate[0:4]   # day of year and year for scene
        print ('doy ',doy,' ',year)
        if doy > 1:
            orbitfilestartdate = datetime.strptime(year+' '+str(doy-1),'%Y %j').strftime('%Y%m%d')
        else:
            lastdoy=datetime.strptime(str(int(year)-1)+'1231', '%Y%m%d').timetuple().tm_yday
            orbitfilestartdate = datetime.strptime(str(int(year)-1)+' '+str(lastdoy),'%Y %j').strftime('%Y%m%d')
    #    print 'orbit file start date: ',orbitfilestartdate
        command='grep '+orbitfilestartdate+' preciseorbitfiles'
        proc = subprocess.Popen(command, stdout=subprocess.PIPE, shell=True)
        (orbitfilename, err) = proc.communicate()
        orbitfilename=str(orbitfilename,'UTF-8').rstrip()
        print ("Precise orbit file found:", orbitfilename)

        SAFEname=zipfile[0:len(zipfile)-4]

    #    create a DEM file if none exists
        create_dem=1
        for file in os.listdir('.'):
            if file == 'elevation.dem':
                create_dem=0

        print ('create_dem= ',create_dem)

        if create_dem==1:
    #        command=HOME+"/sentinel_coordinates_srtm30.py "+SAFEname  # nasadem
            command=HOME+"/sentinel/sentinel_coordinates_copernicus.py "+SAFEname
            print (command)
            ret=os.system(command)
            
    #    and process the scene in you have the precise orbit

        if len(orbitfilename) < 1:
            print ('Missing precise orbit file for ',zipfile)
            command = 'echo skipping file, missing precise orbit'
        else:
    #        print ('traceback orbit file:')
    #        print (orbitfilename)
    #        print (orbitfilename.strip())
    #        print (str(orbitfilename.strip()))
    #        print (str(orbitfilename.strip().decode()))
    #        print ('type of orbitfilename ',type(orbitfilename),orbitfilename)
            command=HOME+'/sentinel/sentinel_scene_gpu.py '+SAFEname+' '+orbitfilename+' '+pol

        print (command)
        ret=os.system(command)

        # store geo slc file in the google cloud
        #command = HOME+'/bin/rclone copy '+SAFEname+'.geo fringe:sentinelgeo'
        #print (command)
        #ret=os.system(command)

    print ('Loop over scenes complete.')

    #  clean up a bit
    #command = 'rm *.hgt* DEM* q*'
    #command = 'rm *.hgt* positionburst* dem* DEM* q*'
    command = 'find . -name \*.hgt\* -delete'
    ret=os.system(command)
    command = 'find . -name dem\* -delete'
    ret=os.system(command)
    command = 'find . -name DEM\* -delete'
    ret=os.system(command)
    command = 'find . -name q\* -delete'
    ret=os.system(command)
    command = 'find . -name positionburst\* -delete'
    ret=os.system(command)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        prog='sentinel_gpu',
    )
    parser.add_argument("--username", type=str,
                        help="hyp3 username")
    parser.add_argument('--password', type=str, 
                        help="hyp3 password")
    parser.add_argument('--polarization', type=str, default="vv",
                        help="Specify vv or vh Polarization. Default=vv")
    parser.add_argument('--use_existing_data', type=bool, default=False)
    args = parser.parse_args()

    main(HOME+"/mydata/", args.username, args.password, args.polarization, args.use_existing_data)




