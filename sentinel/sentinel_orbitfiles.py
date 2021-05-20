#!/usr/bin/env python3
#
#  download precise orbit files for zipfiles in directory
#

import sys
import os
import string
import time
import subprocess
import argparse
from datetime import datetime

# environment setup

# environment setup
# get the current environment
HOME = os.environ['PROC_HOME']

parser = argparse.ArgumentParser(description='Convert a .geo file into a tiff')
parser.add_argument("Data_Dir", type=str)
parser.add_argument("--username", type=str,
                    help="hyp3 username")
parser.add_argument('--password', type=str, 
                    help="hyp3 password")
args = parser.parse_args()
Data_Dir = vars(args)["Data_Dir"]
username = args.username
password = args.password

print('Downloading sentinel precise orbit files for zipfiles in this directory')

# get list of zip files
zipfiles = []
for file in os.listdir(Data_Dir):
    if file.endswith(".zip"):
            mission=file[0:3]
            if file.find("SDV_") > 0:
                acquisitiondate=file[file.find("SDV_")+4:file.find("SDV_")+12]
            if file.find("SSV_") > 0:
                acquisitiondate=file[file.find("SSV_")+4:file.find("SSV_")+12]
            if file.find("SDH_") > 0:
                acquisitiondate=file[file.find("SDH_")+4:file.find("SDH_")+12]
            if file.find("SSH_") > 0:
                acquisitiondate=file[file.find("SSH_")+4:file.find("SSH_")+12]

            command = HOME+"/EOFrequests/getEOF.py "+acquisitiondate+" "+mission+" --username \""+username+"\" --password \""+password+"\""
            print (command)
            ret = os.system(command)
