#!/usr/bin/env python3

import os
import getpass
import readline

myhome = "$PROC_HOME"
os.environ["MYHOME"] = myhome

print ( "\n"+"Script to Process Sentinel-1 Stack Using Backprojection"+"\n")
print (    "This script allows you to download Sentinel-1 data from the ASF archive and process it to geocoded, phase compensated SLC files.  Each output file has a .geo suffix, and can be downloaded from your own data area in OpenSARlab or from Google drive.")
print (  "\n"+"Step 1.  Prepare your working area"+"\n"+"\n"+"Create a working directory for your analysis, and cd to that directory.")

command = "mkdir mydata\n"
print (command)
ret = os.system(command)
ret = os.chdir('mydata')

print (    "\nStep 2.  Download Sentinel raw data to your area\n"+
    "Log into the Vertex system at ASF and search for files you wish to process. Identify and select the L0 raw data products. Add these to your downloads cart. Open the cart by clicking on the Downloads button in the upper right part of your window. \n"+
    "\n"+
    "In the Vertex interface Downloads cart, click on Copy File IDs (not Data Download).  In the following command, paste your file IDs after the prompt.\n")

print("Paste file IDs, terminate with <cr> "),
fileids = raw_input()

print (fileids)

f=open('fileids','w')
f.write (fileids)
f.close()

command = myhome+"/asfdata/enter_file_ids.py"
#print (command)
ret = os.system(command)

print ( "\nNow execute the ASF download for your files. You will be prompted for your Earthdata username and password.  Your password will not be displayed, but it will be stored temporarily in a file only you can access, which will then be deleted. \nNote: If the requested raw data file exists, it will not be downloaded again.  If you want to download a newer or more complete file, delete the existing .zip file before executing the following.")

print ('Earthdata username: '),
EDuser=raw_input()
print ('Earthdata password (no echo): '),
EDpassword=getpass.getpass()
f=open('.credentials','w')
os.chmod('.credentials', 0o600)
f.write(EDuser+'\n')
f.write(EDpassword+'\n')
f.close()

#command = myhome+"/asfdata/asfwget_notebook.py scenelist"
#print (command)
#ret = os.system(command)

