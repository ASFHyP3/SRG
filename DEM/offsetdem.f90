!c  offsetdem - add a constant to a dem

      character*360 infile, outfile, rscfile
      character*60 string
      character*14 str, str2
      character*14 strwidth,strlength,strxfirst,stryfirst,strxstep,strystep
      real toplat, botlat, leftlon, rightlon, newlat, newlon
      integer*2, allocatable :: idata(:,:),outdata(:,:)

      if(iargc().lt.4)then
         print *,'Usage: offsetdem indem rscfile outdem offset'
         stop
      end if

      call getarg(1,infile)
      call getarg(2,rscfile)
      call getarg(3,outfile)
      call getarg(4,str)
      read(str,*)offset

!c  open rsc file
      open(20,file=rscfile)

!c  read in the values that matter first
      read(20,'(a)')string
      read(string(1:14),'(a)')strwidth
      read(string(15:60),*)iwidth

      read(20,'(a)')string
      read(string(1:14),'(a)')strlength
      read(string(15:60),*)ilength

      read(20,'(a)')string
      read(string(1:14),'(a)')strxfirst
      read(string(15:60),*)xfirst

      read(20,'(a)')string
      read(string(1:14),'(a)')stryfirst
      read(string(15:60),*)yfirst

      read(20,'(a)')string
      read(string(1:14),'(a)')strxstep
      read(string(15:60),*)xstep

      read(20,'(a)')string
      read(string(1:14),'(a)')strystep
      read(string(15:60),*)ystep

!!$      read(20,'(a,i14)')strwidth,iwidth
!!$      read(20,'(a,i14)')strlength,ilength
!!$      read(20,'(a,i14)')strxfirst,ixfirst
!!$      read(20,'(a,i14)')stryfirst,iyfirst
!!$      read(20,'(a,e20.8)')strxstep,xstep
!!$      read(20,'(a,e20.8)')strystep,ystep

!c  add the offset to the dem
      allocate (idata(iwidth,ilength))
      open(21,file=infile,access='direct',recl=iwidth*2*ilength)
      open(31,file=outfile,access='direct',recl=iwidth*2*ilength)
      read(21,rec=1)idata
      idata=idata+offset
      write(31,rec=1)idata
      close(21)
      close(31)
      close(20)
      deallocate (idata)

      end
