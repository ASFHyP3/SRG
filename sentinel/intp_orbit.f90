integer function intp_orbit(timeorbit, xx, vv, numstatevec, time, xyz_mid, vel_mid)

  implicit none
  integer ilocation, numstatevec
  real*8 timeorbit(*), xx(3,*), vv(3,*), xyz_mid(3), vel_mid(3), time
  ilocation=(time-timeorbit(1))/(timeorbit(2)-timeorbit(1))
  if(ilocation.lt.2)then
     !print *,'ilocation set to 2 ',time,ilocation,timeorbit(1),timeorbit(2)
     ilocation=2
  end if
  if(ilocation.gt.numstatevec-2)then
     !print *,'ilocation set to ',numstatevec-2,time
     ilocation=numstatevec-2
  end if
!!$  ilocation=max(ilocation,2)  ! take care of times falling off the end
!!$  ilocation=min(ilocation,numstatevec-1)
  !print *,i,time,ilocation,xx(1,ilocation-1),vv(1,ilocation-1),timeorbit(ilocation-1)
  xyz_mid=0.
  vel_mid=0.
  call orbithermite(xx(1,ilocation-1),vv(1,ilocation-1),timeorbit(ilocation-1),time,xyz_mid,vel_mid)
  !print *,x,v
  intp_orbit=0
  return 

end function intp_orbit
