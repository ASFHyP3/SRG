      character*360 infile, outfile, inrscfile, outrscfile
      character*14 str, str2
      integer*2, allocatable :: idata(:,:),outdata(:,:)


      call getarg(1,infile)
      call getarg(2,outfile)
        
!c  open input rsc file, write new data to output rsc file
      open(20,file=infile)
      open(30,file=outfile)

      read(20,'(a,a)')str,str2
      read(str2,'(i14)')iwidth
      write(str2,'(i14)')iwidth
      write(30,'(a14,a14)')str,adjustl(str2)

      read(20,'(a,a)')str,str2
      read(str2,'(i14)')ilength
      write(str2,'(i14)')ilength
      write(30,'(a14,a14)')str,adjustl(str2)

      read(20,'(a,a)')str,str2
      read(str2,*)xfirst
      write(str2,'(f14.8)')xfirst
      write(30,'(a14,a14)')str,adjustl(str2)

      read(20,'(a,a)')str,str2
      read(str2,*)yfirst
      write(str2,'(f14.8)')yfirst
      write(30,'(a14,a14)')str,adjustl(str2)

      read(20,'(a,e20.8)')str,xstep
      write(30,'(a14,e15.8)')str,xstep

      read(20,'(a,e20.8)')str,ystep
      write(30,'(a14,e15.8)')str,ystep

      read(20,'(a,a)')str,str2
      write(30,'(a14,a14)')str,str2
      read(20,'(a,a)')str,str2
      write(30,'(a14,a14)')str,str2
      read(20,'(a,a)')str,str2
      write(30,'(a14,a14)')str,str2
      read(20,'(a,a)')str,str2
      write(30,'(a14,a14)')str,str2
      read(20,'(a,a)')str,str2
      write(30,'(a14,a14)')str,str2
      close(20)
      close(30)

      print *,iwidth,ilength,ixfirst,iyfirst
      print *,xstep,ystep

      end
