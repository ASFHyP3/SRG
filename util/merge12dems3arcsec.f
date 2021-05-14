c  merge 12 SRTM dems

      integer*2 a1(1201),a2(1201), a3(1201), a4(1201), out(4801)
      character*60 ul, ur, ll, lr, ull, lll, urr, lrr, outfile
      character*60 ml,mr,mll,mrr

      if (iargc().lt.5)then
         print *,'Usage: merge12dems3arcsec upper-left-left upper-left-file upper-right upper-right-right middle-left-left middle-left middle-right middle-right-right lower-left-left lower-left lower-right lower-right-right outfile'
         stop
      end if

      call getarg(1,ull)
      call getarg(2,ul)
      call getarg(3,ur)
      call getarg(4,urr)
      call getarg(5,mll)
      call getarg(6,ml)
      call getarg(7,mr)
      call getarg(8,mrr)
      call getarg(9,lll)
      call getarg(10,ll)
      call getarg(11,lr)
      call getarg(12,lrr)
      call getarg(13,outfile)

      open(21,file=ull,access='direct',recl=2402)
      open(22,file=ul,access='direct',recl=2402)
      open(23,file=ur,access='direct',recl=2402)
      open(24,file=urr,access='direct',recl=2402)
      open(25,file=mll,access='direct',recl=2402)
      open(26,file=ml,access='direct',recl=2402)
      open(27,file=mr,access='direct',recl=2402)
      open(28,file=mrr,access='direct',recl=2402)
      open(29,file=lll,access='direct',recl=2402)
      open(30,file=ll,access='direct',recl=2402)
      open(31,file=lr,access='direct',recl=2402)
      open(32,file=lrr,access='direct',recl=2402)
      open(41,file=outfile,access='direct',recl=9602)

c  assume all files are i*2 1201 by 1201

      do lineout=1,1200
         read(21,rec=lineout)(out(k),k=1,1201)
         read(22,rec=lineout)(out(k),k=1201,2401)
         read(23,rec=lineout)(out(k),k=2401,3601)
         read(24,rec=lineout)(out(k),k=3601,4801)
         write(41,rec=lineout)out
      end do
      do lineout=1201,2401
         read(25,rec=lineout-1200)(out(k),k=1,1201)
         read(26,rec=lineout-1200)(out(k),k=1201,2401)
         read(27,rec=lineout-1200)(out(k),k=2401,3601)
         read(28,rec=lineout-1200)(out(k),k=3601,4801)
         write(41,rec=lineout)out
      end do
      do lineout=2401,3601
         read(29,rec=lineout-2400)(out(k),k=1,1201)
         read(30,rec=lineout-2400)(out(k),k=1201,2401)
         read(31,rec=lineout-2400)(out(k),k=2401,3601)
         read(32,rec=lineout-2400)(out(k),k=3601,4801)
         write(41,rec=lineout)out
      end do

      end
