      real data(60000)
      character*60 file

      if(iargc().lt.2)then
         write(*,*)'usage: analyzemht infile len '
         stop
      end if

      call getarg(2,file)
      read(file,*)len

      call getarg(1,file)
      open(21,file=file,access='direct',recl=len*8)
      ampmax=-1.e30
      ampmin=1.e30
      hmax=-1.e30
      hmin=1.e30
      do j=1,100000
         read(21,rec=j,err=99)(data(k),k=1,len*2)
         do k=1,len
            ampmax=max(ampmax,data(k))
            ampmin=min(ampmin,data(k))
            hmax=max(hmax,data(k+len))
            hmin=min(hmin,data(k+len))
            if(data(k+len).gt.1.0)print *,j,k,data(k),data(k+len)
         end do
      end do
      close(21)
 99   continue
      print *,ampmin,ampmax,hmin,hmax
      end
