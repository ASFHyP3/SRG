c  merge 2 SRTM dems east-west

      integer*2 a1(3601),a2(3601), out(7201)
      character*60 l, r, outfile

      if (iargc().lt.3)then
         print *,'Usage: merge2demsew left-file right outfile'
         stop
      end if

      call getarg(1,l)
      call getarg(2,r)
      call getarg(3,outfile)

      open(21,file=l,access='direct',recl=7202)
      open(22,file=r,access='direct',recl=7202)
      open(31,file=outfile,access='direct',recl=14402)

c  assume all files are i*2 3601 by 3601

      do lineout=1,3601
         read(21,rec=lineout)(out(k),k=1,3601)
         read(22,rec=lineout)(out(k),k=3601,7201)
         write(31,rec=lineout)out
      end do

      end
