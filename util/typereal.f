      real fdata(10000)
      character*60 file

      if(iargc().lt.4)then
         write(*,*)'usage: typereal infile len(real*4) record sample'
         stop
      end if

      call getarg(2,file)
      read(file,*)len
      call getarg(3,file)
      read(file,*)irec
      call getarg(4,file)
      read(file,*)i0


      call getarg(1,file)
      open(21,file=file,access='direct',recl=len*4)
      read(21,rec=irec)(fdata(k),k=1,len)
      close(21)

      write(*,*)(fdata(k),k=i0,i0+5)

      end
