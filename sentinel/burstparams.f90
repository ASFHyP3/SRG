! get some parameters from a burst needed for processing

subroutine burstparams(rawdata, starttime, prf, samplefreq, pri, range0, wvl)

  integer*1 rawdata(80)
  real*8 starttime, prf, samplefreq, pri, range0
  real*8 fref, c, wvl
  real*8 swst
  real*8 d8
  real*8 samplefrequency
  integer irank, rangedecimation
  
  fref=37.53472224d0
  c=299792458.d0
  wvl=0.05546576
  starttime = d8(rawdata(1+68))
  prf = fref/in3(rawdata(1+50))*1.e6
  rangedecimation = iand(int(rawdata(1+40)),255)
  samplefreq = samplefrequency(rangedecimation)*1.e6
  swst = in3(rawdata(1+53))/fref*1.e-6;
  irank=iand(int(rawdata(1+49)), 31)
  pri = in3(rawdata(1+50))/fref*1.e-6;
  range0=c/2.*(irank*pri+swst);

  return
end subroutine burstparams

integer function in2(data)
  integer*1 data(*)
  in2=iand(int(data(2)),255)+256*iand(int(data(1)),255)
  return
end function in2

integer function in3(data)
  integer*1 data(*)
  in3=iand(int(data(3)),255)+256*iand(int(data(2)),255)+256*256*iand(int(data(1)),255)
  return
end function in3

integer function in4(data)
  integer*1 data(*)
  in4=iand(int(data(4)),255)+256*iand(int(data(3)),255)+256*256*iand(int(data(2)),255)+256*256*256*iand(int(data(1)),255)
  return
end function in4

real*8 function d8(data)
  integer*1 data(*)
  integer*1 b(8)
  real*8 d
  equivalence (b,d)

  b=data(1:8)
  d8=d
  !print *,b,d
  return
  end

real*8 function samplefrequency(rangeDecimation)
  real*8 fref
  integer rangeDecimation

  fref=37.53472224d0;

  select case (rangeDecimation)
  case (0)
     samplefrequency= 3./4.*4.*fref
  case (1)
     samplefrequency= 2./3.*4.*fref
  case (3)
     samplefrequency= 5./9.*4.*fref
  case (4)
     samplefrequency= 4./9.*4.*fref
  case (5)
     samplefrequency= 3./8.*4.*fref
  case (6)
     samplefrequency= 1./3.*4.*fref
  case (7)
     samplefrequency= 1./6.*4.*fref
  case (8)
     samplefrequency= 3./7.*4.*fref
  case (9)
     samplefrequency= 5./16.*4.*fref
  case (10)
     samplefrequency= 3./26.*4.*fref
  case (11)
     samplefrequency= 4./11.*4.*fref
  end select
  return
end function samplefrequency

