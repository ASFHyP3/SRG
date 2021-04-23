      real*8, allocatable :: data(:,:)
      character*120 file

      if(iargc().lt.4)then
         write(*,*)'usage: typexyz infile len xloc yloc <lines>'
         stop
      end if

      call getarg(2,file)
      read(file,*)len
      call getarg(3,file)
      read(file,*)i0
      call getarg(4,file)
      read(file,*)irec0
      lines=1
      if(iargc().ge.5)then
         call getarg(5,file)
         read(file,*)lines
      end if
      call getarg(1,file)

      allocate (data(3,len))

      open(21,file=file,access='direct',recl=len*8*3)
      do j=1,lines
         read(21,rec=irec0+j-1)data
         print *,irec0+j-1,data(:,i0)
      end do
      close(21)

      end

