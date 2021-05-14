c  merge 6 SRTM dems, 90 m, 2 across 3 down

      integer*2 a1(1201),a2(1201), a3(1201), a4(1201), out(2401)
      character*60 ul, ur, ll, lr, outfile
      character*60 ml,mr

      if (iargc().lt.5)then
         print *,'Usage: merge2x3dems3arcsec upper-left-file upper-right middle-left middle-right lower-left lower-right outfile'
         stop
      end if

      call getarg(1,ul)
      call getarg(2,ur)
      call getarg(3,ml)
      call getarg(4,mr)
      call getarg(5,ll)
      call getarg(6,lr)
      call getarg(7,outfile)

      open(21,file=ul,access='direct',recl=2402)
      open(22,file=ur,access='direct',recl=2402)
      open(23,file=ml,access='direct',recl=2402)
      open(24,file=mr,access='direct',recl=2402)
      open(25,file=ll,access='direct',recl=2402)
      open(26,file=lr,access='direct',recl=2402)
      open(41,file=outfile,access='direct',recl=4802)

c  assume all files are i*2 1201 by 1201

      do lineout=1,1200
         read(21,rec=lineout)(out(k),k=1,1201)
         read(22,rec=lineout)(out(k),k=1201,2401)
         write(41,rec=lineout)out
      end do
      do lineout=1201,2401
         read(23,rec=lineout-1200)(out(k),k=1,1201)
         read(24,rec=lineout-1200)(out(k),k=1201,2401)
         write(41,rec=lineout)out
      end do
      do lineout=2401,3601
         read(25,rec=lineout-2400)(out(k),k=1,1201)
         read(26,rec=lineout-2400)(out(k),k=1201,2401)
         write(41,rec=lineout)out
      end do

      end
