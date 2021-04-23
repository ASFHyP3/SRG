!c  get sentinel position and time vectors for backprojection processor

      character*360 orbitfile, burstfile, str
      double precision x(3),v(3),xold(3),vest(3)
      double precision xx(3,28),vv(3,28),t(28),time
      double precision timefirst,timedelta,timeorbit(28),timeline(100000)
      double precision avetime,refline,fine, d8
      double precision sumx,sumy,sumsqx,sumsqy,sumxy,ssxx,ssyy,ssxy
      integer*1, allocatable :: indata(:)
      integer*8 filelen


      if(iargc().lt.2)then
         print *,'usage: sentineltiming orbitfile burstfile linelength'
         stop
      end if

      call getarg(1,orbitfile)
      call getarg(2,burstfile)
      call getarg(3,str)
      read(str,*)len

      allocate (indata(len*8))

      nlines=filelen(burstfile)/len/8
!      print *,'Lines in burst: ',nlines
      open(21,file=burstfile,access='direct',recl=len*8)
      read(21,rec=1)indata

!c  time of first data point
!!$      icoarse=iand(indata(10),255)+iand(indata(9),255)*256+iand(indata(8),255)*256*256+iand(indata(7),255)*256*256*256
!!$      ifine=iand(indata(12),255)+iand(indata(11),255)*256
!!$      fine=(ifine+0.5)*(2.**(-16))
!!$
!!$      timefirst=mod(icoarse,86400)+fine
      timefirst=d8(indata(69))
!      print *,'first data line ',timefirst

!c  read the orbit file state vectors
      open(22,file=orbitfile)
      read(22,*)nvect
      read(22,*)nvect
      read(22,*)nvect
      read(22,*)nvect ! only 4th one counts
      do i=1,nvect
         read(22,*)timeorbit(i),xx(:,i),vv(:,i)
      end do
      close(22)
      timedelta=timeorbit(2)-timeorbit(1)

      open(31,file='position.out')
!c  read in the raw data file line by line
      do i=1,nlines
         read(21,rec=i)indata
!!$         icoarse=iand(indata(10),255)+iand(indata(9),255)*256+iand(indata(8),255)*256*256+iand(indata(7),255)*256*256*256
!!$         ifine=iand(indata(12),255)+iand(indata(11),255)*256
!!$         fine=(ifine+0.5)*(2.**(-16))
!!$         timeline(i)=mod(icoarse,86400)+fine
         timeline(i)=d8(indata(69))

!c  interpolate orbit to this time
         time=timeline(i)
         ilocation=(time-timeorbit(1))/timedelta
         !print *,time,ilocation,timeorbit(1),timedelta
         call orbithermite(xx(1,ilocation-1),vv(1,ilocation-1),timeorbit(ilocation-1),time,x,v)
!         print *,x,v
         write(31,'(i8,1x,f19.12,3(1x,f16.7),3(1x,f14.8))')i,time,x,v
      end do
      close(31)

      end

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

