!!!!!
!
! compare_precise_orbit - see if interpolated precise orbit matches data file orbit
!
!!!!!

program compare_precise_orbit

  implicit none

  character*300 str,orbfile,prefile
  integer stat
  integer intp_orbit
  real*8 timeorbit(100),xx(3,100),vv(3,100),aa(3,100),x(3),v(3),a(3)
  real*8 timefirst,timeend,timeorbitdatafile
  real*8, dimension(3) :: xyz, vel
  integer :: i, nlines,numstatevec,numstatevecdatafile

  if(iargc().lt.2)then
     print *,'usage: compare_precise_orbit orbtiming_file precise_orbtiming_file'
     stop
  end if

  call getarg(1,orbfile)
  call getarg(2,prefile)

  !c read in the precise orbit state vectors
  open(21,file=prefile)
  read(21,*)timefirst
  read(21,*)timeend
  read(21,*)nlines
  read(21,*)numstatevec
  print *,'Number of precise state vectors: ',numstatevec
  !c  read in state vectors
  do i=1,numstatevec
     read(21,*)timeorbit(i),x,v,a
     xx(:,i)=x
     vv(:,i)=v
     aa(:,i)=a
     print '(f10.2,3f13.2,3f12.5)',timeorbit(i),x,v
  end do
  close(21)

  !c read in the datafile orbit state vectors
  open(21,file=orbfile)
  read(21,*)timefirst
  read(21,*)timeend
  read(21,*)nlines
  read(21,*)numstatevecdatafile
  !print *,'Number of slave state vectors: ',numstatevec
  !c  read in datafile state vectors
  do i=1,numstatevecdatafile
     read(21,*)timeorbitdatafile,x,v,a
     ! interpolate precise state vectors to datafile times
     stat=intp_orbit(timeorbit, xx, vv, numstatevec, &
          timeorbitdatafile, xyz, vel)
     print *,'Datafile state vector number, time ',i,timeorbitdatafile
     print '(a,3f13.2,3f12.5)','Datafile pos, vel: ',x,v
     print '(a,3f13.2,3f12.5)','Precise  pos, vel: ',xyz,vel
  end do
  close(21)

end program compare_precise_orbit

integer function intp_orbit(timeorbit, xx, vv, numstatevec, time, xyz_mid, vel_mid)

  implicit none
  integer ilocation, numstatevec
  real*8 timeorbit(*), xx(3,*), vv(3,*), xyz_mid(3), vel_mid(3), time
  ilocation=(time-timeorbit(1))/(timeorbit(2)-timeorbit(1))
  if(ilocation.lt.2)then
     ilocation=2
  end if
  if(ilocation.gt.numstatevec-2)then
     ilocation=numstatevec-2
  end if
  xyz_mid=0.
  vel_mid=0.
  call orbithermite(xx(1,ilocation-1),vv(1,ilocation-1),timeorbit(ilocation-1),time,xyz_mid,vel_mid)
  intp_orbit=0
  return 
end function intp_orbit
