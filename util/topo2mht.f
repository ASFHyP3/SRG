c  topo2mht - convert an i2 file to mht format, unity brightness

      real a(10000*2)
      integer*2 in(10000)
      character*60 infile,outfile,str

      if(iargc().lt.3)then
         print *,'usage: short2mht infile outfile linelength'
         stop
      end if

      call getarg(1,infile)
      call getarg(2,outfile)
      call getarg(3,str)
      read(str,*)len

      open(21,file=infile,access='direct',recl=2*len)
      open(31,file=outfile,access='direct',recl=4*len*2)

      do line=1,1000000
         read(21,rec=line,err=90)(in(k),k=1,len)
         do j=1,len
            a(j)=1.
            k=in(j)
            a(j+len)=iand(k,2*32768-1)
         end do
         write(31,rec=line)(a(k),k=1,len*2)
      end do
 90   continue
      end
