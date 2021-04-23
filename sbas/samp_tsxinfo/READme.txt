n is the number of slcs
N is the number of interferogram default N=(n-1)*n/2

~~~~~~~~Bperp.out: N*1 vector
   perpendicular baseline length at mid azimuth line for each interferogram

~~~~~~~~deltime.out: N*4 vector
  first column: interferogram ID number
  second column: time interval of the interferogram (if slc1 was acuquired later than slc2, then the value is negative.)
  third column: the data aqusition time of slc1
  forth column: the data aqusition time of slc2
 
  **time are calculated using decimal year
  http://www.mathworks.com/help/aerotbx/ug/decyear.html

~~~~~~~~Tm.out: N*(n-1) matrix SBAS time matrix
  the ith row -> the ith interferogram

  Assume we have 5 slcs and the 4 time intervals are:
  11 22 44 33 (in days)
  which means slc2 was acuqired 11 days later than slc1, slc3 was acuqired 22 days later than slc2...
  
  if the ith interferogram was formed by IMG1 = slc3 and IMG2 = slc5
  then the ith row in Tm should be [0 0 44 33]
  if the ith interferogram was formed by IMG1 = slc4 and IMG2 = slc1
  then the ith row in Tm should be [-11 -22 -44 0]