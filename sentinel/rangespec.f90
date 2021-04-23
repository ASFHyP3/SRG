!c  range spectrum
!


  integer*1 b(80)
  complex in(30000),data(32768),ref(32768),accum(32768)
  character*200 rawfile, outfile, str
  integer*8 iplanf,iplani
  integer*4 ranfft, rangedecimation, pricount, burst
  real*8 fref, samplefrequency, t, phase, pi

  equivalence (b,in)
  
  if(iargc().lt.2)then
     print *,'Usage: rangespec rawdatafile len firstsample nsamples firstline nlines'
     stop
  end if

  call getarg(1,rawfile)
  call getarg(2,str)
  read(str,*)len
  call getarg(3,str)
  read(str,*)ifirst
  call getarg(4,str)
  read(str,*)nsamps
  call getarg(5,str)
  read(str,*)iline
  call getarg(6,str)
  read(str,*)nlines


  !  some global defs
  fref=37.53472224d0
  pi=4.d0*atan2(1.,1.)

  !  set up transforms using fftw3
  ranfft=nsamps
  call sfftw_plan_dft_1d(iplanf, ranfft, data, data, -1, 64)
  call sfftw_plan_dft_1d(iplani, ranfft, data, data,  1, 64)

  ! input file
  open(11,file=rawfile,access='direct',recl=len*8)

  ! loop over each line
  ipri=0
  burst=0
  accum=0.
  do i=iline,iline+nlines-1
     if(mod(i,2000).eq.0)print *,'At line ',i
     read(11,rec=i,err=99)(in(k),k=1,len)
     ! line 1 get some chirp parameters
     if(i.eq.iline)then
        rangedecimation=iand(b(1+40),255)
        samplefreq=samplefrequency(rangedecimation)
        ipolarity=iand(in2(b(1+42)),32768)/32768
        ramprate=(-1)**(1-ipolarity)*iand(in2(b(1+42)),32767)*fref*fref/2**21
        ipolarity=iand(in2(b(1+44)),32768)/32768
        startfreq=rampRate/4./fref+(-1)**(1-ipolarity)*iand(in2(b(1+44)),32767)*fref/2**14
        pulselength=in3(b(1+46))/fref
        nsamples=in2(b(1+65))*2
        npts=pulselength*samplefreq
        nvalid=nsamples-npts
     end if

     ! transform in range
     data(1:nsamps)=in(ifirst:ifirst-1+nsamps)
     call sfftw_execute_dft(iplanf,data,data)
     accum(1:nsamps)=accum(1:nsamps)+cabs(data(1:nsamps))
  end do
  open(31,file='rangespec.out')
  do i=1,nsamps
     write(31,*)cabs(accum(i))
  enddo
99 print *,'Processed lines: ',i-1,' of length ',nvalid,' plus header of 10'

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



  
