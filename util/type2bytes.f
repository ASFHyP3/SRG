c  type bytes from 2 files

      character*60 f1,f2,str
      integer*1 b(100000),bb(100000)

      if(iargc().lt.4)then
         print *,'Usage: type2bytes file1 file2 startbyte nbytes'
         stop
      end if

      call getarg(1,f1)
      call getarg(2,f2)
      call getarg(3,str)
      read(str,*)i0
      call getarg(4,str)
      read(str,*)nbytes

      ichan1=initdk(21,f1)
      ichan2=initdk(22,f2)

      i1=ioseek(ichan1,i0)
      i2=ioseek(ichan2,i0)

      n1=ioread(ichan1,b,nbytes)
      n2=ioread(ichan2,bb,nbytes)

      do i=1,nbytes
         print *,i+i0-1,iand(b(i),255),iand(bb(i),255)
      end do

      end


      
