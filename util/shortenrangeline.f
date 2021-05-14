c  shorten a range line

      integer*1 b(32768)
      character*60 str,infile,outfile

      if(iargc().lt.4)then
         print *,'Usage: infile outfile inlength outlength'
         stop
      end if

      call getarg(1,infile)
      call getarg(2,outfile)
      call getarg(3,str)
      read(str,*)lenin
      call getarg(4,str)
      read(str,*)lenout

      open(21,file=infile,access='direct',recl=lenin)
      open(22,file=outfile,access='direct',recl=lenout)
      
      do i=1,100000
         read(21,rec=i)(b(k),k=1,lenin)
         write(22,rec=i)(b(k),k=1,lenout)
      end do
      end

