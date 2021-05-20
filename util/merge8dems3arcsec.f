c  merge 4 SRTM dems

      integer*2 a1(1201),a2(1201), a3(1201), a4(1201), out(4801)
      character*60 ul, ur, ll, lr, ull, lll, urr, lrr, outfile

      if (iargc().lt.5)then
         print *,'Usage: merge8dems3arcsec upper-left-left upper-left-file upper-right upper-right-right lower-left-left lower-left lower-right lower-right-right outfile'
         stop
      end if

      call getarg(1,ull)
      call getarg(2,ul)
      call getarg(3,ur)
      call getarg(4,urr)
      call getarg(5,lll)
      call getarg(6,ll)
      call getarg(7,lr)
      call getarg(8,lrr)
      call getarg(9,outfile)

      open(21,file=ull,access='direct',recl=2402)
      open(22,file=ul,access='direct',recl=2402)
      open(23,file=ur,access='direct',recl=2402)
      open(24,file=urr,access='direct',recl=2402)
      open(25,file=lll,access='direct',recl=2402)
      open(26,file=ll,access='direct',recl=2402)
      open(27,file=lr,access='direct',recl=2402)
      open(28,file=lrr,access='direct',recl=2402)
      open(31,file=outfile,access='direct',recl=9602)

c  assume all files are i*2 1201 by 1201

      do lineout=1,1200
         read(21,rec=lineout)(out(k),k=1,1201)
         read(22,rec=lineout)(out(k),k=1201,2401)
         read(23,rec=lineout)(out(k),k=2401,3601)
         read(24,rec=lineout)(out(k),k=3601,4801)
         write(31,rec=lineout)out
      end do
      do lineout=1201,2401
         read(25,rec=lineout-1200)(out(k),k=1,1201)
         read(26,rec=lineout-1200)(out(k),k=1201,2401)
         read(27,rec=lineout-1200)(out(k),k=2401,3601)
         read(28,rec=lineout-1200)(out(k),k=3601,4801)
         write(31,rec=lineout)out
      end do

      end
