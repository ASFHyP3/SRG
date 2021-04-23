!  get steering angle coefficients for a burst
!

  integer burst, word, elevationbeam,azimuthbeam,rangedecimation
  integer*1, allocatable ::  b(:,:)
  complex, allocatable :: data(:,:), dopest(:,:), dopcent(:), header(:,:)
  complex, allocatable :: dopcentroid(:,:)
  real, allocatable :: fd(:,:), x(:), fdc(:), fdop(:), prf(:), t(:), ang(:)
  character*200 burstname,str
  integer*8 filelen
  real*8 pi, samplefrequency, c, range0
  real*8, allocatable :: ksarray(:), range(:)
  real*8 fdcoefs(2)

  pi=4.d0*atan2(1.d0,1.d0)
  fref=37.53472224
  c=299792458.d0

  if(iargc().lt.2)then
     print *,'Usage: burst_angle_steering burstfile len'
     stop
  end if

  call getarg(1,burstname)
  call getarg(2,str)
  read(str,*)len

!c  open a burst file
  lines=filelen(burstname)/8/len
  
!  print *,'Lines: ',lines,' of length ',len
  open(21,file=burstname,access='direct',recl=len*8*lines)

  allocate (data(len,lines), dopest(len,lines-1))
  allocate (dopcent(lines), fd(lines,11), x(lines),range(11),ksarray(11))
  allocate (dopcentroid(lines,11), fdc(11))
  allocate (b(80,lines), header(10,lines))
  allocate (fdop(lines),prf(lines),t(lines),ang(lines))

  read(21,rec=1)data
  close(21)

  call fdopcoefs(data,len,lines,fdcoefs)

  print *,'Subroutine returns fdcoeffs ',fdcoefs

end
