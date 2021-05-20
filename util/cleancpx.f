      real data(65536)
      character*60 file

      if(iargc().lt.4)then
         write(*,*)'usage: cleancpx infile len pluslimit minuslimit'
         stop
      end if

      call getarg(2,file)
      read(file,*)len
      call getarg(3,file)
      read(file,*)plim
      call getarg(4,file)
      read(file,*)anlim


      call getarg(1,file)
      open(21,file=file,access='direct',recl=len*8)
      open(22,file='q',access='direct',recl=len*8)
      do j=1,100000
         read(21,rec=j,err=99)(data(k),k=1,len*2)
         do i=1,len*2,2
            if(data(i).lt.anlim)then
               print *,i/2+1,j,data(i)
               data(i)=anlim
            end if
            if(data(i).gt.plim)then
               print *,i/2+1,j,data(i)
               data(i)=plim
            end if
            if(data(i+1).lt.anlim)then
               print *,i/2+1,j,data(i+1)
               data(i+1)=anlim
            end if
            if(data(i+1).gt.plim)then
               print *,i/2+1,j,data(i+1)
               data(i+1)=plim
            end if
         end do
         write(22,rec=j)(data(k),k=1,len*2)
      end do
 99   close(21)
      close(22)
      end
