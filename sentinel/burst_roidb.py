#!/usr/bin/env python3

import sys
ver=sys.version_info

    


    


import os
import sys
import math
from datetime import datetime

def readxmlparam(xmllines, param):
    for line in xmllines:
        if param in line:
            i=line.find(param)
            str1=line[i:];
            istart=str1.find('>')+1
            istop=str1.find('<')
            value=str1[istart:istop]
            #print line[i:],'\n'
            #print istart, istop, '\n'
            return value

def timeinseconds(timestring):
    dt = datetime.strptime(timestring, '%Y-%m-%dT%H:%M:%S.%f')
    secs=dt.hour*3600+dt.minute*60+dt.second+dt.microsecond/1000000.0
    return secs

if len(sys.argv) < 5:
    print 'Usage: burst_roidb.py burstfile orbtimingfile positionfile length'
    sys.exit(1)

burstfile=sys.argv[1]
orbtimingfile=sys.argv[2]
positionfile=sys.argv[3]
length=sys.argv[4]

# open a database file
dbname = burstfile+'.db'
if os.path.exists(dbname):
    ret=os.system("rm "+dbname)

con = sqlite3.connect(string.strip(dbname))

# create a cursor
c = con.cursor()

# compute a few values from parameters we'll need below
vel_light = 299792458.

burst=open(burstfile,'rb')
data=bytearray(burst.read(length*8)) 
coarse=0
i=0
for b in data[6:10]:
    coarse=coarse+(b&255)*(256**(3-i))
    i=i+1

fine=0
i=0
for b in data[10:12]:
    fine=fine+(b&255)*(256**(1-i))
    i=i+1

fine=(fine+0.5)*2**(-16)

print 'coarse ',coarse,' ',coarse%86400
print 'fine   ',fine,'\n'
startTime=(coarse%86400)+fine

print startTime

# open table for this subswath to save processing parameters
swathfile='file'
sql_mod.add_tbl(c,swathfile)  # create table
con.commit()

# start loading some parameters to table

sql_mod.add_param(c,swathfile,'slc_file')
slcfile='back.slc'
print 'slcfile = ',slcfile
sql_mod.edit_param(c,swathfile,'slc_file',slcfile,'-','CHAR','raw slc file')
sql_mod.add_param(c,swathfile,'roi_debug')
sql_mod.edit_param(c,swathfile,'roi_debug',0,'-','INT*4','roi debug parameter')
numberOfSamplesPerLine=int(length)
numberOfLines=int(1410)
sampledPixelSpacing=float(readxmlparam(xmllines,'rangePixelSpacing'))
sampledLineSpacing=float(readxmlparam(xmllines,'azimuthPixelSpacing'))
sql_mod.add_param(c,swathfile,'bytes_per_line')
sql_mod.edit_param(c,swathfile,'bytes_per_line',numberOfSamplesPerLine*8,'-','INT*4','bytes per line')
sql_mod.add_param(c,swathfile,'good_bytes_per_line')
sql_mod.edit_param(c,swathfile,'good_bytes_per_line',numberOfSamplesPerLine*8,'-','INT*4','good bytes per line')
sql_mod.add_param(c,swathfile,'first_line')
sql_mod.edit_param(c,swathfile,'first_line',0,'-','INT*4','first line (from 0)')
sql_mod.add_param(c,swathfile,'number_of_patches')
npatches=1
sql_mod.edit_param(c,swathfile,'number_of_patches',npatches,'-','INT*4','number of patches')
sql_mod.add_param(c,swathfile,'iq_pairs_to_skip')
sql_mod.edit_param(c,swathfile,'iq_pairs_to_skip',0,'-','INT*4','iq pairs to skip')

sql_mod.add_param(c,swathfile,'deskew')
sql_mod.edit_param(c,swathfile,'deskew','Y','-','CHAR','deskew flag')
sql_mod.add_param(c,swathfile,'squint_angle')
sql_mod.edit_param(c,swathfile,'squint_angle',0,'degrees','REAL*8','squint angle')
sql_mod.add_param(c,swathfile,'first_range_bin')
sql_mod.edit_param(c,swathfile,'first_range_bin',1,'-','INT*4','first range bin')
sql_mod.add_param(c,swathfile,'number_of_range_bins')
sql_mod.add_param(c,swathfile,'range_pixel_delta')
sql_mod.add_param(c,swathfile,'azimuth_pixel_delta')
sql_mod.add_param(c,swathfile,'fd')
sql_mod.add_param(c,swathfile,'fdd')
sql_mod.add_param(c,swathfile,'fddd')
sql_mod.add_param(c,swathfile,'caltone_location')
sql_mod.add_param(c,swathfile,'re')
sql_mod.add_param(c,swathfile,'velocity')
sql_mod.add_param(c,swathfile,'ht')
sql_mod.add_param(c,swathfile,'r0')
sql_mod.edit_param(c,swathfile,'number_of_range_bins',numberOfSamplesPerLine,'-','INT*4','number of range bins')
sql_mod.edit_param(c,swathfile,'range_pixel_delta',0,'pixels','REAL*8','range pixel delta')
sql_mod.edit_param(c,swathfile,'azimuth_pixel_delta',0,'pixels','real*8','azimuth pixel delta')
sql_mod.edit_param(c,swathfile,'fd',0,'prfs','real*8','Doppler centroid constant term')


sql_mod.edit_param(c,swathfile,'fdd',0,'prfs','real*8','Mocomp fdd')
sql_mod.edit_param(c,swathfile,'fddd',0,'prfs','real*8','Mocomp fddd')
sql_mod.edit_param(c,swathfile,'caltone_location',0.0,'pct_bw','real*8','Caltone location pct of bw')
semiMajorAxis=float(readxmlparam(xmllines,'ellipsoidSemiMajorAxis'))
sql_mod.edit_param(c,swathfile,'re',semiMajorAxis,'m','real*8','Local Earth radius')
sql_mod.edit_param(c,swathfile,'velocity',7590.,'m/s','real*8','mocomp velocity')
satelliteHeight=0 #float(readxmlparam(xmllines,'satelliteHeight'))
sql_mod.edit_param(c,swathfile,'ht',satelliteHeight,'m','real*8','orbit altitude')
zeroDopplerTimeFirstLine=readxmlparam(xmllines,'firstLineSensingTime')
zeroDopplerTimeLastLine=readxmlparam(xmllines,'lastLineSensingTime')
r0=float(readxmlparam(xmllines,'slantRangeTime'))*vel_light/2
sql_mod.edit_param(c,swathfile,'r0',r0,'m','real*8','Near range distance')
#
rawdataprf=float(readxmlparam(xmllines,'prf'))
#print 'PRF for acqusition: ',rawdataprf
prf=float(readxmlparam(xmllines,'azimuthFrequency'))
#print 'PRF for slc: ',prf
#prf=(numberOfLines-1)/abs(timeinseconds(zeroDopplerTimeLastLine)-timeinseconds(zeroDopplerTimeFirstLine))
sql_mod.add_param(c,swathfile,'prf')
sql_mod.edit_param(c,swathfile,'prf',prf,'Hz','real*4','prf')
sql_mod.add_param(c,swathfile,'rawdataprf')
sql_mod.edit_param(c,swathfile,'rawdataprf',rawdataprf,'Hz','real*4','prf')
sql_mod.add_param(c,swathfile,'azimuth_res')
sql_mod.add_param(c,swathfile,'azimuth_looks')
sql_mod.add_param(c,swathfile,'fs')
sql_mod.add_param(c,swathfile,'chirp_slope')
sql_mod.add_param(c,swathfile,'chirp_ext')
sql_mod.add_param(c,swathfile,'srmflag')
sql_mod.add_param(c,swathfile,'pulse_length')
sql_mod.edit_param(c,swathfile,'azimuth_res',3.,'m','real*8','Azimuth resolution')
sql_mod.edit_param(c,swathfile,'azimuth_looks',1,'-','INT*4','slc looks = 1')
#
# note: Radarsat: for some reason fs needs to be doubled for spotlight mode!
fs=float(readxmlparam(xmllines,'rangeSamplingRate'))
sql_mod.edit_param(c,swathfile,'fs',fs,'Hz','real*8','Range sample frequency')
#sql_mod.edit_param(c,swathfile,'chirp_slope',data['slope'],'Hz/s','real*8','Range chirp slope')
#sql_mod.edit_param(c,swathfile,'chirp_ext',0,'points','INT*4','Chirp extension, pts')
sql_mod.edit_param(c,swathfile,'srmflag','n','-','CHAR','disable secondary range migration')
pulseLength=float(readxmlparam(xmllines,'txPulseLength'))
sql_mod.edit_param(c,swathfile,'pulse_length',pulseLength,'s','real*8','Range chirp length')
sql_mod.add_param(c,swathfile,'wvl')
sql_mod.add_param(c,swathfile,'radarFrequency')
sql_mod.add_param(c,swathfile,'weighting')
sql_mod.add_param(c,swathfile,'range_filter')
sql_mod.add_param(c,swathfile,'resamp_int_r')
sql_mod.add_param(c,swathfile,'resamp_slope_r')
sql_mod.add_param(c,swathfile,'resamp_int_a')
sql_mod.add_param(c,swathfile,'resamp_slope_a')
sql_mod.add_param(c,swathfile,'resamp_int_r_delta')
sql_mod.add_param(c,swathfile,'resamp_slope_r_delta')
sql_mod.add_param(c,swathfile,'resamp_int_a_delta')
sql_mod.add_param(c,swathfile,'resamp_slope_a_delta')
wvl=vel_light/float(readxmlparam(xmllines,'radarFrequency'))
sql_mod.edit_param(c,swathfile,'wvl',wvl,'m','real*8','wavelength')
freq=float(readxmlparam(xmllines,'radarFrequency'))
sql_mod.edit_param(c,swathfile,'radarFrequency',freq,'m','real*8','radar center frequency')

sql_mod.edit_param(c,swathfile,'weighting',1.0,'-','real*8','Range weighting factor')
sql_mod.edit_param(c,swathfile,'range_filter',0.0,'-','real*8','range spectrum fractional truncation')
sql_mod.edit_param(c,swathfile,'resamp_int_r',0.0,'Pixels','real*8','Range resampling intercept')
sql_mod.edit_param(c,swathfile,'resamp_slope_r',0.0,'Pixels','real*8','Range resampling slope')
sql_mod.edit_param(c,swathfile,'resamp_int_a',0.0,'Pixels','real*8','Azimuth resampling intercept')
sql_mod.edit_param(c,swathfile,'resamp_slope_a',0.0,'Pixels','real*8','Azimuth resampling slope')
sql_mod.edit_param(c,swathfile,'resamp_int_r_delta',0.0,'Pixels','real*8','Range resampling intercept delta')
sql_mod.edit_param(c,swathfile,'resamp_slope_r_delta',0.0,'Pixels','real*8','Range resampling slope delta')
sql_mod.edit_param(c,swathfile,'resamp_int_a_delta',0.0,'Pixels','real*8','Azimuth resampling intercept delta')
sql_mod.edit_param(c,swathfile,'resamp_slope_a_delta',0.0,'Pixels','real*8','Azimuth resampling slope delta')
sql_mod.add_param(c,swathfile,'orbit_sch_file')
sql_mod.add_param(c,swathfile,'rc')

# add number of azimuth line to the databasefile
sql_mod.add_param(c,swathfile,'number_of_az_lines')
sql_mod.edit_param(c,swathfile,'number_of_az_lines',numberOfLines,'-','int*4','number of azimuth lines')
sql_mod.add_param(c,swathfile,'patch_size')
sql_mod.edit_param(c,swathfile,'patch_size',numberOfLines,'-','INT*4','azimuth lines per patch')
sql_mod.add_param(c,swathfile,'valid_az_samples')
sql_mod.edit_param(c,swathfile,'valid_az_samples',numberOfLines,'-','INT*4','valid az lines per patch')

# get line and time ordering as they flip these for some reason
lineTimeOrdering=readxmlparam(xmllines,'lineTimeOrdering')
sql_mod.add_param(c,swathfile,'lineTimeOrdering')
sql_mod.edit_param(c,swathfile,'lineTimeOrdering',lineTimeOrdering,'-','char','pass direction')
pixelTimeOrdering=readxmlparam(xmllines,'pixelTimeOrdering')
sql_mod.add_param(c,swathfile,'pixelTimeOrdering')
sql_mod.edit_param(c,swathfile,'pixelTimeOrdering',pixelTimeOrdering,'-','char','pass direction')

# for radarsat save params to account for azimuth-reversed image
sql_mod.add_param(c,swathfile,'raw_slc_first_line_time')
sql_mod.edit_param(c,swathfile,'raw_slc_first_line_time',timeinseconds(zeroDopplerTimeFirstLine),'seconds','real*8','time of first line in slc product')
sql_mod.add_param(c,swathfile,'raw_slc_last_line_time')
sql_mod.edit_param(c,swathfile,'raw_slc_last_line_time',timeinseconds(zeroDopplerTimeLastLine),'seconds','real*8','time of last line in slc product')

# extract ascending/descending 
passDirection=readxmlparam(xmllines,'passDirection')
sql_mod.add_param(c,swathfile,'passDirection')
sql_mod.edit_param(c,swathfile,'passDirection',passDirection,'-','char','pass direction')

# extract burst timing info
burstdata=[]
flag1=0
flag2=1
for line in xmllines:
    if '<swathTiming' in line:
        flag1=1
        
    if '</swathTiming' in line:
        flag2=0
            
    if flag1 and flag2:
        burstdata.append(line)

# burst size
linesPerBurst=readxmlparam(burstdata,'linesPerBurst')
samplesPerBurst=readxmlparam(burstdata,'samplesPerBurst')

azimuthTime=[]
azimuthTimeSeconds=[]
endTimeSeconds=[]
validLinesPerBurst=[]
firstValidLine=[]
lastValidLine=[]
firstValidSample=[]
lastValidSample=[]
start=[]
stop=[]
for i in range(len(burstdata)):
    if '<burst>' in burstdata[i]:
        start.append(i)
            
    if '</burst>' in burstdata[i]:
        stop.append(i)

for i in range(len(start)):
    burstlines=burstdata[start[i]:stop[i]]
    timestring=readxmlparam(burstlines,'azimuthTime')
    azimuthTime.append(timestring)
    azimuthTimeSeconds.append(timeinseconds(timestring))
    endTimeSeconds.append(timeinseconds(timestring)+(int(linesPerBurst)-1)/prf)
    # valid data region
    firstsample=readxmlparam(burstlines,'firstValidSample')
    #print len(firstsample.split())
    validchars=len(firstsample)
    halfvalidchars=int(validchars/2)
    validLinesPerBurst.append(int(linesPerBurst)-firstsample.count('-1'))
    firstValidLine.append(firstsample[0:halfvalidchars-1].count('-1')+1)
    lastValidLine.append(int(linesPerBurst)-firstsample[halfvalidchars:validchars-1].count('-1')-1)
    firstValidSample.append(firstsample.split()[int(len(firstsample.split())/2)])
    #print 'first sample ',firstsample.split()[int(len(firstsample.split())/2)]
    lastsample=readxmlparam(burstlines,'lastValidSample')
    lastValidSample.append(lastsample.split()[int(len(lastsample.split())/2)])

    #print azimuthTime
    #print azimuthTimeSeconds
    #print endTimeSeconds

    # estimate valid lines
for i in range(len(azimuthTime)-1):
    validburstlines=(azimuthTimeSeconds[i+1]-azimuthTimeSeconds[i])*prf-1
    #print validburstlines

# store in database
sql_mod.add_param(c,swathfile,'azimuthBursts')
sql_mod.edit_param(c,swathfile,'azimuthBursts',len(azimuthTime),'-','int*4','Bursts in SLC')
sql_mod.add_param(c,swathfile,'linesPerBurst')
sql_mod.edit_param(c,swathfile,'linesPerBurst',linesPerBurst,'-','int*4','Bursts in SLC')
sql_mod.add_param(c,swathfile,'samplesPerBurst')
sql_mod.edit_param(c,swathfile,'samplesPerBurst',samplesPerBurst,'-','int*4','Bursts in SLC')
for i in range(len(azimuthTime)):
    sql_mod.add_param(c,swathfile,'azimuthTime'+str(i+1))
    sql_mod.edit_param(c,swathfile,'azimuthTime'+str(i+1),azimuthTime[i],'-','int*4','Burst start time')
    sql_mod.add_param(c,swathfile,'azimuthTimeSeconds'+str(i+1))
    sql_mod.edit_param(c,swathfile,'azimuthTimeSeconds'+str(i+1),azimuthTimeSeconds[i],'-','int*4','Burst start time in seconds')
    sql_mod.add_param(c,swathfile,'validLinesPerBurst'+str(i+1))
    sql_mod.edit_param(c,swathfile,'validLinesPerBurst'+str(i+1),validLinesPerBurst[i],'-','int*4','valid azimuth lines in burst')
    sql_mod.add_param(c,swathfile,'firstValidLine'+str(i+1))
    sql_mod.edit_param(c,swathfile,'firstValidLine'+str(i+1),firstValidLine[i],'-','int*4','first valid azimuth line in burst')
    sql_mod.add_param(c,swathfile,'lastValidLine'+str(i+1))
    sql_mod.edit_param(c,swathfile,'lastValidLine'+str(i+1),lastValidLine[i],'-','int*4','last valid azimuth line in burst')
    sql_mod.add_param(c,swathfile,'firstValidSample'+str(i+1))
    sql_mod.edit_param(c,swathfile,'firstValidSample'+str(i+1),firstValidSample[i],'-','int*4','first valid range sample in burst')
    sql_mod.add_param(c,swathfile,'lastValidSample'+str(i+1))
    sql_mod.edit_param(c,swathfile,'lastValidSample'+str(i+1),lastValidSample[i],'-','int*4','last valid range sample in burst')
        
con.commit()
#print sql_mod.valuec(c,swathfile,'passDirection'),'from db file in roidb.py'

#  save orbit and timing information
orbitdata=[]
flag1=0
flag2=1
for line in xmllines:
    if '<orbitList' in line:
        flag1=1

    if '</orbitList' in line:
        flag2=0

    if flag1 and flag2:
        orbitdata.append(line)

#  extract each state vector
start=[]
stop=[]
for i in range(len(orbitdata)):
    if '<orbit>' in orbitdata[i]:
        start.append(i)
        
    if '</orbit>' in orbitdata[i]:
        stop.append(i)

time=[]
x=[]
y=[]
z=[]
vx=[]
vy=[]
vz=[]
for i in range(len(start)):
    statelines=orbitdata[start[i]:stop[i]]
    #print statelines,'\n\n'
    for j in range(len(statelines)):
        if '<position>' in statelines[j]:
            posstart=j+1
        if '</position>' in statelines[j]:
            posstop=j
        if '<velocity>' in statelines[j]:
            velstart=j+1
        if '</velocity>' in statelines[j]:
            velstop=j
    time.append(readxmlparam(statelines,'time'))
    x.append(readxmlparam(statelines[posstart:posstop],'x'))
    y.append(readxmlparam(statelines[posstart:posstop],'y'))
    z.append(readxmlparam(statelines[posstart:posstop],'z'))
    vx.append(readxmlparam(statelines[velstart:velstop],'x'))
    vy.append(readxmlparam(statelines[velstart:velstop],'y'))
    vz.append(readxmlparam(statelines[velstart:velstop],'z'))
    #print posstart,posstop,velstart,velstop
    #print statelines[posstart:posstop]
    #print statelines[velstart:velstop]
    
orbinfo=open('orbtiming','w')
if timeinseconds(zeroDopplerTimeFirstLine) < timeinseconds(zeroDopplerTimeLastLine):
    orbinfo.write(str(timeinseconds(zeroDopplerTimeFirstLine))+'\n')
    orbinfo.write(str(timeinseconds(zeroDopplerTimeLastLine))+'\n')

if timeinseconds(zeroDopplerTimeLastLine) < timeinseconds(zeroDopplerTimeFirstLine):
    orbinfo.write(str(timeinseconds(zeroDopplerTimeLastLine))+'\n')
    orbinfo.write(str(timeinseconds(zeroDopplerTimeFirstLine))+'\n')

orbinfo.write(str(numberOfLines)+'\n')
orbinfo.write(str(len(start))+'\n')
for i in range(len(start)):
    orbinfo.write(str(timeinseconds(time[i]))+' '+str(x[i])+' '+str(y[i])+' '+str(z[i])+' '+str(vx[i])+' '+str(vy[i])+' '+str(vz[i])+' 0.0 0.0 0.0')
    orbinfo.write('\n')

orbinfo.close()

#  overwrite with precise orbit
#command='$PROC_HOME/sentinel/preproc/precise_orbit.py '+xmlfilename.replace('xml','orb')
#print (command)
#ret=os.system(command)

#  burst deramp info
#print burstdata
azimuthSteeringRate=float(readxmlparam(xmllines,'azimuthSteeringRate'))
radarFrequency=float(readxmlparam(xmllines,'radarFrequency'))
azimuthTimeInterval=float(readxmlparam(xmllines,'azimuthTimeInterval'))
rangeSamplingRate=float(readxmlparam(xmllines,'rangeSamplingRate'))
slantRangeTime=float(readxmlparam(xmllines,'slantRangeTime')) # this is the first slant range time in imageInformation

ks=2*7500/299792458.*radarFrequency*azimuthSteeringRate*3.14159265359/180.

print 'approx ks = ',ks
sql_mod.add_param(c,swathfile,'azimuthSteeringRate')
sql_mod.add_param(c,swathfile,'radarFrequency')
sql_mod.add_param(c,swathfile,'azimuthTimeInterval')
sql_mod.add_param(c,swathfile,'rangeSamplingRate')
sql_mod.add_param(c,swathfile,'slantRangeTime')

sql_mod.edit_param(c,swathfile,'azimuthSteeringRate',azimuthSteeringRate,'-','real*8','azimuthSteeringRate')
sql_mod.edit_param(c,swathfile,'radarFrequency',radarFrequency,'-','real*8','transmit frequency')
sql_mod.edit_param(c,swathfile,'azimuthTimeInterval',azimuthTimeInterval,'-','real*8','azimuth pri')
sql_mod.edit_param(c,swathfile,'rangeSamplingRate',rangeSamplingRate,'-','real*8','range sample frequency')
sql_mod.edit_param(c,swathfile,'slantRangeTime',slantRangeTime,'-','real*8','time to near swath')

con.commit()
c.close()

con.close()


# azimuth fm rate polynomials
azfmrate=[];
flag1=0
flag2=1
for line in xmllines:
    if '<azimuthFmRateList' in line:
        flag1=1

    if '</azimuthFmRateList' in line:
        flag2=0

    if flag1 and flag2:
        azfmrate.append(line)

#print azfmrate

#  extract each polynomial
start=[]
stop=[]
for i in range(len(azfmrate)):
    if '<azimuthFmRate>' in azfmrate[i]:
        start.append(i)
        
    if '</azimuthFmRate>' in azfmrate[i]:
        stop.append(i)

#print start
#print stop
#print azfmrate[start[1]:stop[1]]

# note that there are 2 formats for azimuthFmRate!!

time=[]
t0=[]
poly=[]
c0=[]
c1=[]
c2=[]
for i in range(len(start)):
    fmratelines=azfmrate[start[i]:stop[i]]
    t0.append(readxmlparam(fmratelines,'t0'))
    time.append(readxmlparam(fmratelines,'azimuthTime'))
    poly.append(readxmlparam(fmratelines,'azimuthFmRatePolynomial'))
    c0.append(readxmlparam(fmratelines,'c0'))
    c1.append(readxmlparam(fmratelines,'c1'))
    c2.append(readxmlparam(fmratelines,'c2'))

#print 'poly ',poly
#print 'c0 ',c0
#print 'c1 ',c1
#print 'c2 ',c2
    
fmrateinfo=open('fmrateinfo','w')
fmrateinfo.write(str(len(start))+'\n')
for i in range(len(start)):
    fmrateinfo.write(str(timeinseconds(time[i]))+' '+str(t0[i])+' ')
    if c0[i] <> None:
        fmrateinfo.write(str(c0[i])+' '+str(c1[i])+' '+str(c2[i])+'\n')
    
    if poly[i] <> None:
        fmrateinfo.write(poly[i])
        fmrateinfo.write('\n')

fmrateinfo.close()

# doppler centroid polynomials
dc=[];
flag1=0
flag2=1
for line in xmllines:
    if '<dcEstimateList' in line:
        flag1=1

    if '</dcEstimateList' in line:
        flag2=0

    if flag1 and flag2:
        dc.append(line)

#  extract each polynomial
start=[]
stop=[]
for i in range(len(dc)):
    if '<dcEstimate>' in dc[i]:
        start.append(i)
        
    if '</dcEstimate>' in dc[i]:
        stop.append(i)

time=[]
t0=[]
poly=[]
for i in range(len(start)):
    dclines=dc[start[i]:stop[i]]
    t0.append(readxmlparam(dclines,'t0'))
    time.append(readxmlparam(dclines,'azimuthTime'))
    poly.append(readxmlparam(dclines,'geometryDcPolynomial'))
    
#print poly
dcinfo=open('dcinfo','w')
dcinfo.write(str(len(start))+'\n')
for i in range(len(start)):
    dcinfo.write(str(timeinseconds(time[i]))+' '+str(t0[i])+' ')
    dcinfo.write(poly[i])
    dcinfo.write('\n')

dcinfo.close()

