!c  don't process sentinel swath raw file in range, but split bursts
!c

  use omp_lib

  implicit none
  integer*1 b(80)
  complex*8 cb(10)
  complex (kind=4), allocatable ::  in(:),data(:),dataspec(:),ref(:),reftime(:)
  character*200 rawfile
  integer*8 iplanf,iplani
  integer*4 i,j,ipolarity,nlines,npts,nsamps,nvalid,k,kk,kkk,ipri
  integer*4 ranfft, rangedecimation, pricount, burst, line
  integer*4, allocatable ::  burstno(:),burstline(:),lineinburst(:)
  integer in2, in3, in4
  real*4 pulselength,ramprate,samplefreq,startfreq
  real*8 fref, samplefrequency, t, phase, pi

  equivalence (b,cb)
  
  if(iargc().lt.1)then
     print *,'Usage: process_realaperture rawdatafile'
     stop
  end if

  call getarg(1,rawfile)

  ! allocate some arrays
  allocate (in(30000),data(32768),dataspec(32768),ref(32768),reftime(32768))
  allocate (burstno(1000000),burstline(1000000),lineinburst(1000000))

  !  some global defs
  fref=37.53472224d0
  pi=4.d0*datan2(1.d0,1.d0)
  !  set up transforms using fftw3
  ranfft=32768
  call sfftw_plan_dft_1d(iplanf, ranfft, data, ref, -1, 64)
  call sfftw_plan_dft_1d(iplani, ranfft, data, ref,  1, 64)

  ! input file
  open(11,file=rawfile,access='direct',recl=30000*8)

  ! get the sorting instructions
  ipri=0
  burst=0
  burstline=0
  nlines=0
  do i=1,1000000
     if(mod(i,2000).eq.0)print *,'Sorting line ',i
     read(11,rec=i,err=99)in
     cb=in(1:10)
     pricount=in4(b(1+33))
     if(pricount-ipri.gt.1000)burst=burst+1
     burstline(burst)=burstline(burst)+1
     burstno(i)=burst
     lineinburst(i)=burstline(burst)
     ipri=pricount
     nlines=nlines+1
  end do
  nlines=i-1
99  print *,'Sorted ',i-1,' lines of raw data.',nlines
  close(11)

  open(11,file=rawfile,access='direct',recl=30000*8)
  ! decode some needed parameters first
  read(11,rec=1)in
  cb=in(1:10)
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

  ! open the output files
  do i=1,9
     open(30+i,file='rawdata'//char(48+i),access='direct',recl=(nvalid+10)*8)
  end do
  open(40,file='rawdata10',access='direct',recl=(nvalid+10)*8)
  open(41,file='rawdata11',access='direct',recl=(nvalid+10)*8)

  !  create a reference function
!!$  reftime=0.
!!$  do j=1,npts
!!$     t=(j-1)/samplefreq
!!$     phase=2.d0*pi*startfreq*t+pi*ramprate*t*t
!!$     reftime(j)=cmplx(cos(phase),sin(phase))/npts
!!$  end do
!!$  call sfftw_execute_dft(iplanf,reftime,ref)

! loop over each line
  !$OMP PARALLEL DO private(in,data,dataspec) &
  !$OMP shared(nlines,nsamps,iplanf,iplani,ref,burstno,lineinburst,nvalid)
  do i=1,nlines
     if(mod(i,2000).eq.0)print *,'At line ',i
     read(11,rec=i)in

     ! don't transform in range
     data(1:nsamps)=in(80/8+1:80/8+nsamps)
     data(nsamps+1:32768)=cmplx(0.,0.)
!!$     call sfftw_execute_dft(iplanf,data,dataspec)
!!$     dataspec=dataspec*conjg(ref)  ! multiply by ref
!!$     call sfftw_execute_dft(iplani,dataspec,data)  ! back to time domain
     if(burstno(i).ge.1.and.burstno(i).le.11)then
        write(30+burstno(i),rec=lineinburst(i))in(1:10),data(1:nvalid)
     end if
  end do
  !$OMP end parallel do

  print *,'Did not process lines: ',nlines,' of length ',nvalid,' plus header of 10'
  open(21,file='rangesamples')
  write(21,*)nvalid+10
  close(21)

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



! sorting instructions reference from sortbursts
!  open(12,file='sortbursts.out')
!  nlines=0
!  do i=1,1000000
!     read(12,*,end=99)k,burst,line
!     burstno(i)=burst
!     burstline(i)=line
!     nlines=nlines+1
!     print *,i,nlines,burstno(i),burstline(i)
!  end do
!99  print *,'Lines to process: ',nlines,i-1
!  close(12)


