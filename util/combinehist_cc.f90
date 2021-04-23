      character*60 theory,hist,outfile

      if(iargc().lt.3)then
         print *,'usage: combinehist theoryfile histfile outfile'
         stop
      end if

      call getarg(1,theory)
      call getarg(2,hist)
      call getarg(3,outfile)

      open(21,file=theory)
      open(22,file=hist)
      open(31,file=outfile)

      do i=1,100
         read(21,*)cc,h2,h4,h8,h16,h32,h64
         read(22,*)hdata
         write(31,*)cc,h2,h4,h8,h16,h32,h64,hdata
      end do
      end
