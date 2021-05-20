c  merge 9 SRTM dems

      integer*2 a1(3601,3601),a2(3601),a3(3601), out(10801,3601)
      character*60 ul, um, ur, ml, mm, mr, ll, lm, lr, outfile

      if (iargc().lt.10)then
         print *,'Usage: merge9dems upper-left-file um ur ml mm mr ll lm lr outfile'
         stop
      end if

      call getarg(1,ul)
      call getarg(2,um)
      call getarg(4,ml)
      call getarg(5,mm)
      call getarg(6,mr)
      call getarg(8,lm)
      call getarg(3,ur)
      call getarg(7,ll)
      call getarg(9,lr)
      call getarg(10,outfile)

      open(31,file=outfile,access='direct',recl=21602)

c  assume all files are i*2 3601 by 3601

      open(21,file=ul,access='direct',recl=7202*3601)
      read(21,rec=1)a1
      close(21)
      out(1:3601,:)=a1
      open(21,file=um,access='direct',recl=7202*3601)
      read(21,rec=1)a1
      close(21)
      out(3601:7201,:)=a1
      open(21,file=ur,access='direct',recl=7202*3601)
      read(21,rec=1)a1
      close(21)
      out(7201:10801,:)=a1
      do lineout=1,3601
         write(31,rec=lineout)(out(k,lineout),k=1,10801)
      end do

      open(21,file=ml,access='direct',recl=7202*3601)
      read(21,rec=1)a1
      close(21)
      out(1:3601,:)=a1
      open(21,file=mm,access='direct',recl=7202*3601)
      read(21,rec=1)a1
      close(21)
      out(3601:7201,:)=a1
      open(21,file=mr,access='direct',recl=7202*3601)
      read(21,rec=1)a1
      close(21)
      out(7201:10801,:)=a1
      do lineout=3601,7201
         write(31,rec=lineout)(out(k,lineout-3600),k=1,10801)
      end do

      open(21,file=ll,access='direct',recl=7202*3601)
      read(21,rec=1)a1
      close(21)
      out(1:3601,:)=a1
      open(21,file=lm,access='direct',recl=7202*3601)
      read(21,rec=1)a1
      close(21)
      out(3601:7201,:)=a1
      open(21,file=lr,access='direct',recl=7202*3601)
      read(21,rec=1)a1
      close(21)
      out(7201:10801,:)=a1
      do lineout=7201,10801
         write(31,rec=lineout)(out(k,lineout-7200),k=1,10801)
      end do

      end
