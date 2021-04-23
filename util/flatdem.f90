!c  flatdem - 
!c   create a dem with constant elevation

      character*360 outfile, inrscfile
      character*14 str,strwidth,strlength
      integer*2, allocatable :: outdata(:,:)

      if(iargc().lt.2)then
         print *,'Usage: flatdem rscfile outfile <elev=100>'
         stop
      end if

      call getarg(1,inrscfile)
      call getarg(2,outfile)
      ielev=100
      if(iargc().ge.3)then
         call getarg(3,str)
         read(str,*)ielev
      end if

!c  open input rsc file
      open(20,file=inrscfile)

!c  read in the values that matter 
      read(20,'(a,i14)')strwidth,iwidth
      read(20,'(a,i14)')strlength,ilength
      close(20)
      print *,iwidth,ilength
      print *,strwidth
      print *,strlength

      allocate (outdata(iwidth,ilength))
      outdata=ielev
      open(31,file=outfile,access='direct',recl=ilength*iwidth*2)
      write(31,rec=1)outdata
      close(31)
      deallocate (outdata)

      end
