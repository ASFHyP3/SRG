c  merge 2 SRTM dems east-west, 3 arcsec version

      integer*2 a1(1201),a2(1201), out(2401)
      character*60 l, r, outfile

      if (iargc().lt.3)then
         print *,'Usage: merge2demsew left-file right outfile'
         stop
      end if

      call getarg(1,l)
      call getarg(2,r)
      call getarg(3,outfile)

      open(21,file=l,access='direct',recl=2402)
      open(22,file=r,access='direct',recl=2402)
      open(31,file=outfile,access='direct',recl=4802)

c  assume all files are i*2 1201 by 1201

      do lineout=1,1201
         read(21,rec=lineout)(out(k),k=1,1201)
         read(22,rec=lineout)(out(k),k=1201,2401)
         write(31,rec=lineout)out
      end do

      end
