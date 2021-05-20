subroutine getburststatevectors(startime)

  implicit none
  integer i, j, itemp, nvect, ifirst
  real*8 startime
  real*8 time(10000),xx(3,10000),vv(3,10000),aa(3,10000)
  
  open(21,file='orbtiming.full')
  read(21,*)itemp
  read(21,*)itemp
  read(21,*)itemp
  read(21,*)nvect  !  number of state vector
  if(nvect.ge.10000)then
     print *,'too many points in orbit file'
     return
  end if
  
  !  read in all state vectors
  ifirst=0
  do i=1,nvect
     read(21,*)time(i),xx(:,i),vv(:,i),aa(:,i)
     if(ifirst.eq.0)then
        if(i.ge.2)then
           if(time(i).lt.time(i-1))ifirst=i
        end if
     end if
  end do
99 continue
  close(21)
!  print *,time(ifirst-1),time(ifirst),ifirst
  
  !  open output file
  open(22,file='precise_orbtiming')
  write(22,*)0
  write(22,*)0
  write(22,*)0
  write(22,*)11
  
  !  save the vectors for the current burst
  j=0
  do i=ifirst,nvect
!     print *,i,time(i),ifirst,nvect
     if(time(i).ge.startime-30.)then
        do j=i,i+10
           write(22,*)time(j),xx(:,j),vv(:,j),aa(:,j)
        end do
     end if
     if(j.gt.0)go to 98
  end do
98 continue
  close(22)
end subroutine getburststatevectors
