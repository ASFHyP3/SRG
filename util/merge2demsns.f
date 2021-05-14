c  merge 2 SRTM dems north/south

      integer*2 a1(3601),a2(3601), out(3601)
      character*60 u,l,outfile

      if (iargc().lt.5)then
         print *,'Usage: merge2demsns upper-file lower-file outfile'
         stop
      end if

      call getarg(1,u)
      call getarg(2,l)
      call getarg(3,outfile)

      open(21,file=u,access='direct',recl=7202)
      open(22,file=l,access='direct',recl=7202)
      open(31,file=outfile,access='direct',recl=7202)

c  assume all files are i*2 3601 by 3601

      do lineout=1,3600
         read(21,rec=lineout)(out(k),k=1,3601)
         write(31,rec=lineout)out
      end do
      do lineout=3601,7201
         read(22,rec=lineout-3600)(out(k),k=1,3601)
         write(31,rec=lineout)out
      end do

      end
