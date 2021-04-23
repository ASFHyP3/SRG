c - scalecpx, scale a complex image
      complex a(8192), b(8192)
      character*60 f1,f2,str


      if(iargc().lt.4)then
         print *,'usage: scalecpx infile outfile len scale'
         stop
      end if
      call getarg(3,f1)
      read(f1,*)len
      call getarg(4,f1)
      read(f1,*)scale
      call getarg(1,f1)
      call getarg(2,f2)

      open(21,file=f1,access='direct',recl=len*8)
      open(22,file=f2,access='direct',recl=len*8)

      do i=1,10000
         read(21,rec=i,err=99)(a(k),k=1,len)
         do k=1,len
            b(k)=a(k)*scale
         end do
         write(22,rec=i)(b(k),k=1,len)
      end do
 99   end

