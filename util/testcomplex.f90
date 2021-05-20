      complex data(32768)
      character*300 file

      if(iargc().lt.4)then
         write(*,*)'usage: testcomplex infile len xloc yloc'
         stop
      end if

      call getarg(2,file)
      read(file,*)len
      call getarg(3,file)
      read(file,*)i0
      call getarg(4,file)
      read(file,*)irec


      call getarg(1,file)
      open(21,file=file,access='direct',recl=len*8)
      read(21,rec=irec)(data(k),k=1,len)
      close(21)

      print *,'Complex value:',data(i0)

      end
