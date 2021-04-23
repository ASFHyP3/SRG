      byte data(400)
      integer idata(100)
      real fdata(100)
      character*60 file

      equivalence (data,idata),(data,fdata)

      if(iargc().lt.1)then
         write(*,*)'usage: typebytes infile <400-byte record number>'
      end if

      irec=1
      if(iargc().ge.2)then
         call getarg(2,file)
         read(file,*)irec
      end if

      call getarg(1,file)
      open(21,file=file,access='direct',recl=400)
      read(21,rec=irec)data
      close(21)

      write(*,*)'bytes'
      write(*,*)(iand(data(k),255),k=1,20)
      write(*,*)'integer 32 bit'
      write(*,*)(idata(k),k=1,5)
      write(*,*)'float 32 bit'
      write(*,*)(fdata(k),k=1,5)

      end
