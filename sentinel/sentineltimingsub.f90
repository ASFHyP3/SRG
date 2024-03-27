  !c  subroutine to get sentinel position and time vectors for backprojection processor

  subroutine sentineltimingsub(rangeprocdata,rangesamples,linesmax,lines,nbursts,orbitfile,SAFEname,swath)

    character*300 orbitfile, burstfile, str
    character*200 SAFEname
      double precision x(3),v(3),xold(3),vest(3)
      double precision xx(3,28),vv(3,28),t(28),time
      double precision timefirst,timedelta,timeorbit(28),timeline(100000)
      double precision avetime,refline,fine, d8
      double precision sumx,sumy,sumsqx,sumsqy,sumxy,ssxx,ssyy,ssxy
      integer*1, allocatable :: indata(:)
      integer*8 filelen
      integer lines(nbursts),rangesamples,linesmax,nbursts,i,swath
      integer*1 rangeprocdata(rangesamples*8,linesmax*nbursts)

      allocate (indata(rangesamples*8))

      ! how is SAFEname passed
      do isafe=1,len_trim(SAFEname)
         if (ichar(SAFEname(isafe:isafe)).eq.0)exit
      end do
      
      !c create a file with state vectors for scene
      indata=rangeprocdata(:,1)  ! read first line of burst 1
      timefirst=d8(indata(69))
      print *,'First line, first burst time: ',timefirst
      call getburststatevectors(timefirst,SAFEname(1:isafe-1))
      
      !c  read the orbit file state vectors
      open(22,file=orbitfile)
      read(22,*)nvect
      read(22,*)nvect
      read(22,*)nvect
      read(22,*)nvect ! only 4th one counts
      do i=1,nvect
         read(22,*)timeorbit(i),xx(:,i),vv(:,i)
!         print *,timeorbit(i),orbitfile
      end do
      close(22)
      timedelta=timeorbit(2)-timeorbit(1)

!  loop over bursts
      do iburst=1,nbursts
         indata=rangeprocdata(:,1+(iburst-1)*linesmax)  ! read first line of burst
         nlines=lines(iburst)  !filelen(burstfile)/len/8
         timefirst=d8(indata(69))
!         print *,'first data line ',timefirst
!         print *,indata(1:80)
         
         if(iburst.le.9)str=SAFEname(1:isafe-1)//'.positionburst'//char(48+iburst)//char(48+swath)//'.out'
         if(iburst.ge.10)str=SAFEname(1:isafe-1)//'.positionburst1'//char(48+iburst-10)//char(48+swath)//'.out'
         open(31,file=str)
         !c  read in the raw data file line by line
         do i=1,nlines
            indata=rangeprocdata(:,i+(iburst-1)*linesmax)
            timeline(i)=d8(indata(69))
!            print *,timeline(i),i
            !c  interpolate orbit to this time
            time=timeline(i)
            ilocation=(time-timeorbit(1))/timedelta
            !print *,time,ilocation,timeorbit(1),timedelta
            call orbithermite(xx(1,ilocation-1),vv(1,ilocation-1),timeorbit(ilocation-1),time,x,v)
            !         print *,x,v
            write(31,'(i8,1x,f19.12,3(1x,f16.7),3(1x,f14.8))')i,time,x,v
         end do
         close(31)
         
      end do
      deallocate(indata)
      
    end subroutine sentineltimingsub

