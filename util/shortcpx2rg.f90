!c  shortcpx2rg - convert an i*2 and a complex image magnitude to an rg file

      integer*2, allocatable :: i2(:)
      complex, allocatable :: c(:),out(:)
      character*60 filei2,filecpx,filerg,str
      integer stat,statb(13)

      if(iargc().lt.4)then
         print *,'Usage:  shortcpx2rg i2file cpxfile rgfile length'
         stop
      end if

      call getarg(1,filei2)
      call getarg(2,filecpx)
      call getarg(3,filerg)
      call getarg(4,str)
      read(str,*)len

!c  file size
      ierr=stat(filei2,statb)
      lines=statb(8)/len/2
      print *,'Lines in file: ',lines

      allocate(i2(len))
      allocate(c(len))
      allocate(out(len))

      open(21,file=filei2,access='direct',recl=len*2)
      open(22,file=filecpx,access='direct',recl=len*8)
      open(31,file=filerg,access='direct',recl=len*8)

      do i=1,lines
         read(21,rec=i)i2
         read(22,rec=i)c
         do j=1,len
            out(j)=cmplx(float(i2(j)),cabs(c(j)))
         end do
         write(31,rec=i)out
      end do

      end
