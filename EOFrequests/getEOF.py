#!/usr/bin/env python3

import sys
ver=sys.version_info

import string, time
import os
import sys
import math
import subprocess
import datetime
import argparse
from datetime import date

#print len(sys.argv)

def main(acquireddate:int, mission: str, username: str, password: str):

    HOME = os.environ['PROC_HOME']

    ASF=1  # switch to retrieve from ESA or ASF
    ESA=1-ASF

    # relevant orbit file starts one day before acquisition date

    print ("Retrieving "+mission+" precise orbit file for date ",acquireddate)
    year=int(acquireddate/10000)
    month=int((acquireddate-year*10000)/100)
    day=acquireddate-year*10000-month*100
    acq=datetime.date(year,month,day)
    orbitstartdate=date.fromordinal(acq.toordinal()-1)
    orbitstr=str(orbitstartdate)

    # retrieve orbit file from archive
    if ESA==1:
        command="wget -q https://qc.sentinel1.eo.esa.int/aux_poeorb/?\&validity_start="+str(orbitstartdate)+"\&sentinel1__mission="+mission+" -O - | grep "+mission+"_OPER"
        print (command)
        proc = subprocess.Popen(command, stdout=subprocess.PIPE, shell=True)
        (fullquery, err) = proc.communicate()
        orbitfilelink=fullquery[fullquery.find("href=")+6:fullquery.find("EOF\"")+3]
        orbitfile=fullquery[fullquery.find(mission):fullquery.find("EOF")+3]
        command = "wget -qN "+orbitfilelink
        print (command)
        ret = os.system(command)

    if ASF==1:
        string="_V"+orbitstr.replace('-','')
        command="wget -q https://s1qc.asf.alaska.edu/aux_poeorb -O - | grep "+mission+"_OPER | grep "+string
        print (command)
        proc = subprocess.Popen(command, stdout=subprocess.PIPE, shell=True)
        (fullquery, err) = proc.communicate()
        fullquery=str(fullquery,'UTF-8')
        orbitfilelink=fullquery[fullquery.find("href=")+6:fullquery.find("EOF\"")+3]
        command = "wget -qN https://s1qc.asf.alaska.edu/aux_poeorb/"+orbitfilelink+" --user=\""+username+"\" --password=\""+password+"\""
        ret = os.system(command)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        prog='getEOF',
    )
    parser.add_argument("date", type=int)
    parser.add_argument("mission", type=str)
    parser.add_argument("--username", type=str,
                        help="hyp3 username")
    parser.add_argument('--password', type=str, 
                        help="hyp3 password")
    args = parser.parse_args()
    main(args.date, args.mission, args.username, args.password)