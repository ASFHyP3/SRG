c  merge 4 SRTM dems

      integer*2 a1(3601),a2(3601), out(7201)
      character*60 ul, ur, ll, lr, outfile

      if (iargc().lt.5)then
         print *,'Usage: merge4dems upper-left-file upper-right lower-left lower-right outfile'
         stop
      end if

      call getarg(1,ul)
      call getarg(2,ur)
      call getarg(3,ll)
      call getarg(4,lr)
      call getarg(5,outfile)

      open(21,file=ul,access='direct',recl=7202)
      open(22,file=ur,access='direct',recl=7202)
      open(23,file=ll,access='direct',recl=7202)
      open(24,file=lr,access='direct',recl=7202)
      open(31,file=outfile,access='direct',recl=14402)

c  assume all files are i*2 3601 by 3601

      do lineout=1,3600
         read(21,rec=lineout)(out(k),k=1,3601)
         read(22,rec=lineout)(out(k),k=3601,7201)
         write(31,rec=lineout)out
      end do
      do lineout=3601,7201
         read(23,rec=lineout-3600)(out(k),k=1,3601)
         read(24,rec=lineout-3600)(out(k),k=3601,7201)
         write(31,rec=lineout)out
      end do

      end
