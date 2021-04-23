      character*60 file,str
      real*8 val(4)

      if(iargc().lt.2)then
         print *,'Usage: plotmocomp file 1-4 1=time,2=s,3=c,4=h'
         stop
      end if

      call getarg(1,file)
      call getarg(2,str)
      read(str,*)n

      open(21,file=file)
      do i=1,1000000
         read(21,*,end=99)k,val
         print *,val(n)
      end do

 99   end
