!c  read headers from sentinel data files
!

  integer*1 b(80)
  integer*1, allocatable :: in(:)
  character*200 rawfile, str
  integer*4 rangedecimation, pricount, burst
  real*8 fref, samplefrequency, sensingtime, d8

  if(iargc().lt.2)then
     print *,'Usage: readheader datafile len(cpxsamples)'
     stop
  end if

  call getarg(1,rawfile)
  call getarg(2,str)
  read(str,*)len

  !  some global defs
  fref=37.53472224d0
  pi=4.d0*atan2(1.,1.)

  allocate (in(len*8))
  ! input file
  open(11,file=rawfile,access='direct',recl=len*8)

  ! loop over each line
  ipri=0
  burst=0
  do i=1,1000000
     read(11,rec=i,err=99)in
     b=in(1:80)
     ! get some chirp parameters
        rangedecimation=iand(b(1+40),255)
        samplefreq=samplefrequency(rangedecimation)
        ipolarity=iand(in2(b(1+42)),32768)/32768
        ramprate=(-1)**(1-ipolarity)*iand(in2(b(1+42)),32767)*fref*fref/2**21
        ipolarity=iand(in2(b(1+44)),32768)/32768
        startfreq=rampRate/4./fref+(-1)**(1-ipolarity)*iand(in2(b(1+44)),32767)*fref/2**14
        pulselength=in3(b(1+46))/fref
        nsamps=in2(b(1+65))*2
        npts=pulselength*samplefreq
        nvalid=nsamps-npts
        swst=in3(b(1+53))
        pricount=in4(b(1+33))
        if(pricount-ipri.gt.1000)burst=burst+1
        ipri=pricount
        !print *,b(65:80)
        sensingtime=d8(b(69))
        print *,i,ramprate,pulselength,swst,pricount,pricount-ipri,burst,sensingtime
  end do
99 print *,'Processed lines: ',i-1,' of length ',len

end program

integer function in2(data)
  integer*1 data(*)
  in2=iand(data(2),255)+256*iand(data(1),255)
  return
end function in2

integer function in3(data)
  integer*1 data(*)
  in3=iand(data(3),255)+256*iand(data(2),255)+256*256*iand(data(1),255)
  return
end function in3

integer function in4(data)
  integer*1 data(*)
  in4=iand(data(4),255)+256*iand(data(3),255)+256*256*iand(data(2),255)+256*256*256*iand(data(1),255)
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



  
