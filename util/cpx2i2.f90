!c  cpx2i2 - convert a complex image to i*2 

      integer*2, allocatable :: i2(:)
      real, allocatable :: shade(:)
      complex, allocatable :: c(:),out(:)
      character*60 filei2,filecpx,filerg,str
      integer stat,statb(13)

      if(iargc().lt.3)then
         print *,'Usage:  cpx2i2 cpxfile i2file length'
         stop
      end if

      call getarg(1,filecpx)
      call getarg(2,filei2)
      call getarg(3,str)
      read(str,*)len

!c  file size
      ierr=stat(filecpx,statb)
      linescpx=statb(8)/len/8
      print *,'Lines in i2 file: ',linescpx

      allocate(i2(len))
      allocate(c(len))
      allocate(out(len))
      allocate(shade(len))

      open(22,file=filei2,access='direct',recl=len*2)
      open(21,file=filecpx,access='direct',recl=len*8)

      do i=1,linesi2
         read(21,rec=i)c

         do j=1,len
            i2(j)=cabs(c(j))
         end do
         write(22,rec=i)i2
      end do

      end
