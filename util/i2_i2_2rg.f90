!c  i2_i2_2rg - convert two i*2 images to an rg file

      integer*2, allocatable :: i2(:),i22(:)
      complex, allocatable :: c(:),out(:)
      character*60 filei2,filei22,filerg,str
      integer stat,statb(13)

      if(iargc().lt.4)then
         print *,'Usage:  shortcpx2rg i2file1 i2file2 rgfile length '
         stop
      end if

      call getarg(1,filei2)
      call getarg(2,filei22)
      call getarg(3,filerg)
      call getarg(4,str)
      read(str,*)len

!c  file size
      ierr=stat(filei2,statb)
      linesi2=statb(8)/len/2
      print *,'Lines in i2 file: ',linesi2

      ierr=stat(filei22,statb)
      linesi22=statb(8)/len/2
      print *,'Lines in i22 file: ',linesi22
      lines=min(linesi2,linesi22)

      allocate(i2(len))
      allocate(i22(len))
      allocate(out(len))

      open(21,file=filei2,access='direct',recl=len*2)
      open(22,file=filei22,access='direct',recl=len*2)
      open(31,file=filerg,access='direct',recl=len*8)

      do i=1,lines
         read(21,rec=i)i2
         read(22,rec=i)i22
         do j=1,len
            out(j)=cmplx(i2(j),i22(j))
         end do
         write(31,rec=i)out
      end do

      end
