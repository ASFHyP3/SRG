      complex data(32768,5)
      character*300 file

      if(iargc().lt.4)then
         write(*,*)'usage: typecomplex infile len xloc yloc'
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
      do j=-2,2
         read(21,rec=irec+1+j)(data(k,j+3),k=1,len)
      end do
      close(21)

      print *,'Complex value:'
      do i=-2,2
         write(*,*)(data(k,i+3),k=i0+1-2,i0+1+2)
      end do
      print *,'Mag/phase:'
      do i=-2,2
         write(*,*)(cabs(data(k,i+3)),phase(data(k,i+3)),k=i0+1-2,i0+1+2)
      end do
      end

      function phase(q)
      complex q

      phase=atan2(aimag(q),real(q))
      return

      end
