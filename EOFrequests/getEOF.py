#!/usr/bin/env python3

import sys
ver=sys.version_info

import string, time
import os
import sys
import math
import subprocess
import datetime
from datetime import date

#print len(sys.argv)

ASF=1   # switch to retrieve from ESA or ASF
ESA=1-ASF

if len(sys.argv) < 3:
    print ('Usage: getEOF.py date(formatYYYYMMDD) mission(S1A or S1B)')
    sys.exit(0)

acquireddate=int(sys.argv[1])
mission=sys.argv[2]

# relevant orbit file starts one day before acquisition date

print ("Retrieving "+mission+" precise orbit file for date ",acquireddate)
year=int(acquireddate/10000)
month=int((acquireddate-year*10000)/100)
day=acquireddate-year*10000-month*100
#print ('YYYY MM DD ',year, month, day)
acq=datetime.date(year,month,day)
#print "acq= ",acq
#print "acq.toordinal= ",acq.toordinal()
orbitstartdate=date.fromordinal(acq.toordinal()-1)
#print ("orbitstartdate= ",orbitstartdate)
orbitstr=str(orbitstartdate)
#print ('orbitstr ',orbitstr.replace('-',''))

# retrieve orbit file from archive
if ESA==1:
    command="wget -q https://qc.sentinel1.eo.esa.int/aux_poeorb/?\&validity_start="+str(orbitstartdate)+"\&sentinel1__mission="+mission+" -O - | grep "+mission+"_OPER"
    print (command)
    proc = subprocess.Popen(command, stdout=subprocess.PIPE, shell=True)
    (fullquery, err) = proc.communicate()
    #print (fullquery)
    #print ('fullquery ',str(fullquery))
    #fullquery=str(fullquery)
    #print (type(fullquery))
    #print (fullquery.find("href="))
    #print (fullquery.find("EOF\""))
    orbitfilelink=fullquery[fullquery.find("href=")+6:fullquery.find("EOF\"")+3]
    orbitfile=fullquery[fullquery.find(mission):fullquery.find("EOF")+3]
    #print ('orbitfilelink ',orbitfilelink)
    #print ('orbitfile ',orbitfile)
    command = "wget -qN "+orbitfilelink
    print (command)
    ret = os.system(command)

if ASF==1:
    # get credentials 
    fcred=open('.credentials','r')
    username=fcred.readline().rstrip()
    password=fcred.readline().rstrip()
    #print (username)
    #print (password)
    fcred.close()
    string="_V"+orbitstr.replace('-','')
    command="wget -q https://s1qc.asf.alaska.edu/aux_poeorb -O - | grep "+mission+"_OPER | grep "+string
    print (command)
    proc = subprocess.Popen(command, stdout=subprocess.PIPE, shell=True)
    (fullquery, err) = proc.communicate()
#    print (str(fullquery,'UTF-8'))
    fullquery=str(fullquery,'UTF-8')
#    print (type(fullquery))
    orbitfilelink=fullquery[fullquery.find("href=")+6:fullquery.find("EOF\"")+3]
#    print (orbitfilelink)
    command = "wget -qN https://s1qc.asf.alaska.edu/aux_poeorb/"+orbitfilelink+" --user="+username+" --password="+password
#    print (command)
    ret = os.system(command)

