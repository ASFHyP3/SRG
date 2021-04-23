c  merge 6 SRTM dems, 2 down, 3 across

      integer*2 a1(3601),a2(3601), a3(3601), out(10801)
      character*60 ul, um, ur, ll, lm, lr, outfile

      if (iargc().lt.5)then
         print *,'Usage: merge6dems upper-left-file upper-middle upper-right lower-left lower-middle lower-right outfile'
         stop
      end if

      call getarg(1,ul)
      call getarg(2,um)
      call getarg(3,ur)
      call getarg(4,ll)
      call getarg(5,lm)
      call getarg(6,lr)
      call getarg(7,outfile)

      open(21,file=ul,access='direct',recl=7202)
      open(22,file=um,access='direct',recl=7202)
      open(23,file=ur,access='direct',recl=7202)
      open(24,file=ll,access='direct',recl=7202)
      open(25,file=lm,access='direct',recl=7202)
      open(26,file=lr,access='direct',recl=7202)
      open(31,file=outfile,access='direct',recl=21602)

c  assume all files are i*2 3601 by 3601

      do lineout=1,3600
         read(21,rec=lineout)(out(k),k=1,3601)
         read(22,rec=lineout)(out(k),k=3601,7201)
         read(23,rec=lineout)(out(k),k=7201,10801)
         write(31,rec=lineout)out
      end do
      do lineout=3601,7201
         read(24,rec=lineout-3600)(out(k),k=1,3601)
         read(25,rec=lineout-3600)(out(k),k=3601,7201)
         read(26,rec=lineout-3600)(out(k),k=7201,10801)
         write(31,rec=lineout)out
      end do

      end
