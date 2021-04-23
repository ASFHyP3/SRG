!c - scalebyte, scale a byte image
      integer*1, allocatable :: a(:)
      character*360 f1,f2,str
      integer filelen


      if(iargc().lt.3)then
         print *,'usage: scalebyte infile outfile scale'
         stop
      end if
      call getarg(3,f1)
      read(f1,*)scale
      call getarg(1,f1)
      call getarg(2,f2)

      nbytes=filelen(f1)
      allocate (a(nbytes))
      open(21,file=f1,access='direct',recl=nbytes)
      open(22,file=f2,access='direct',recl=nbytes)

      read(21,rec=1)a
      a=iand(min(255,int(iand(a,255)*scale)),255)
      write(22,rec=1)a
    end program

