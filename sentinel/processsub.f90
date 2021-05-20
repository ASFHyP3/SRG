!c  process sentinel swath raw file in range, split bursts
!c

  subroutine processsub(rawdata,nbytes)

  use omp_lib

  implicit none
  integer*1 b(80), rawdata(nbytes)
  integer*1 bin(30000*8),bytedata(32768*8)
  complex*8 cb(10), in(30000)
  complex (kind=4), allocatable :: data(:),dataspec(:),ref(:),reftime(:)
  character*200 rawfile
  integer*8 iplanf,iplani,nbytes,inlines,rawoffset
  integer*4 i,j,ipolarity,nlines,npts,nsamps,nvalid,k,kk,kkk,ipri
  integer*4 ranfft, rangedecimation, pricount, burst, line, linesmax
  integer*4, allocatable ::  burstno(:),burstline(:),lineinburst(:)
  integer in2, in3, in4
  real*4 pulselength,ramprate,samplefreq,startfreq
  real*8 fref, samplefrequency, t, phase, pi
  character*300 slcoutfile, orbitfile
  integer burstlines(100)
  character*200 burstfile(100)
  integer*8 filelen, ioffset
!  complex, allocatable :: rangeprocdata(:,:)
  integer*1, allocatable :: rangeprocdata(:,:)
  
  equivalence (b,cb)
  equivalence (in,bin)

  ! allocate some arrays
  allocate (data(32768),dataspec(32768),ref(32768),reftime(32768))
  allocate (burstno(1000000),burstline(1000000),lineinburst(1000000))

  !  some global defs
  fref=37.53472224d0
  pi=4.d0*datan2(1.d0,1.d0)
  !  set up transforms using fftw3
  ranfft=32768
  call sfftw_plan_dft_1d(iplanf, ranfft, data, ref, -1, 64)
  call sfftw_plan_dft_1d(iplani, ranfft, data, ref,  1, 64)

  ! get the sorting instructions
  ipri=0
  burst=0
  burstline=0
  nlines=0
  inlines=nbytes/30000/8
  do i=1,inlines
     if(mod(i,2000).eq.0)print *,'Sorting line ',i
     rawoffset=i-1
     rawoffset=rawoffset*30000*8
     bin=rawdata(1+rawoffset:30000*8+rawoffset) !read(11,rec=i,err=99)in
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
99  print *,'Sorted ',i-1,' lines of raw data.',nlines,' into ',burstno(nlines),' bursts.',burst
  print *,'Raw data size: ',nbytes
  close(11)

  ! max lines in bursts
!  burst=11
  linesmax=0
  do i=1,burst
     linesmax=max(linesmax,burstline(i))
  end do
!  print *,'burstline array: ',burstline(1:11),' max ',linesmax,' bursts ',burst
  
  ! decode some needed parameters first
  bin=rawdata(1:30000*8) !read(11,rec=1)in
  cb=in(1:10)
  rangedecimation=iand(int(b(1+40)),255)
  samplefreq=samplefrequency(rangedecimation)
  ipolarity=iand(in2(b(1+42)),32768)/32768
  ramprate=(-1)**(1-ipolarity)*iand(in2(b(1+42)),32767)*fref*fref/2**21
  ipolarity=iand(in2(b(1+44)),32768)/32768
  startfreq=rampRate/4./fref+(-1)**(1-ipolarity)*iand(in2(b(1+44)),32767)*fref/2**14
  pulselength=in3(b(1+46))/fref
  nsamps=in2(b(1+65))*2
  npts=pulselength*samplefreq
  nvalid=nsamps-npts
!  print *, rangedecimation, samplefreq, ipolarity, ramprate, startfreq, pulselength, nsamps, npts, nvalid


  ! allocate process data array
  allocate (rangeprocdata((nvalid+10)*8,linesmax*burst))
  
  !  create a reference function
  reftime=0.
  do j=1,npts
     t=(j-1)/samplefreq
     phase=2.d0*pi*startfreq*t+pi*ramprate*t*t
     reftime(j)=cmplx(cos(phase),sin(phase))/npts
  end do
  call sfftw_execute_dft(iplanf,reftime,ref)

! loop over each line
  !$OMP PARALLEL DO private(bytedata,data,dataspec,ioffset) &
  !$OMP shared(nlines,nsamps,iplanf,iplani,ref,burstno,lineinburst,nvalid,rangeprocdata,linesmax,rawdata)
  do i=1,nlines
     if(mod(i,2000).eq.0)print *,'At line ',i !,int(i,8)*30000*8,linesmax*(burstno(i)-1)+lineinburst(i)
     bytedata(1:30000*8-80)=rawdata(80+1+int(i-1,8)*30000*8:int(i,8)*30000*8) !     read(11,rec=i)in
     bytedata(nsamps*8+1:32768*8)=0
!     if(mod(i,2000).eq.1)print *,'At line ',i,in(1:20)

     ! transform in range
     call sfftw_execute_dft(iplanf,bytedata,dataspec) !data,dataspec)
     dataspec=dataspec*conjg(ref)  ! multiply by ref
     call sfftw_execute_dft(iplani,dataspec,bytedata)  ! back to time domain
     if(burstno(i).ge.1.and.burstno(i).le.burst)then
        ioffset=int(linesmax,8)*int((burstno(i)-1),8)+int(lineinburst(i),8)
        rangeprocdata(1:80, ioffset)=rawdata(1+int(i-1,8)*30000*8:80+int(i-1,8)*30000*8)
        rangeprocdata(81:80+nvalid*8, ioffset)=bytedata(1:nvalid*8)
     end if
  end do
  !$OMP end parallel do

  print *,'Processed lines: ',nlines,' of length ',nvalid,' plus header of 10'
!  open(21,file='rangesamples')
!  write(21,*)nvalid+10
!  close(21)

!  open(99,file='rangedata',access='direct',recl=(nvalid+10)*8)
!  do i=1,linesmax*burst
!     write(99,rec=i)rangeprocdata(:,i)
!  end do
!  close(99)
  
  !  create position files
  orbitfile='precise_orbtiming'

  call sentineltimingsub(rangeprocdata,nvalid+10,linesmax,burstline,burst,orbitfile)
  !  call backproject
  slcoutfile='slc'
  call backprojectgpusub(rangeprocdata, nvalid+10, burst*linesmax, burst, burstline, slcoutfile)
  
  deallocate (data,dataspec,ref,reftime,rangeprocdata)
  deallocate (burstno,burstline,lineinburst)

end subroutine processsub

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

