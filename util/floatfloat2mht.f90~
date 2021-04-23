!c  floatfloat2rg - convert 2 float files to an rg file

      real, allocatable :: float1(:),float2(:)
      complex, allocatable :: c(:),out(:)
      character*60 file1,file2,filerg,str
      integer stat,statb(13)

      if(iargc().lt.4)then
         print *,'Usage:  floatfloat2rg file1 file2 rgfile length'
         stop
      end if

      call getarg(1,file1)
      call getarg(2,file2)
      call getarg(3,filerg)
      call getarg(4,str)
      read(str,*)len

!c  file size
      ierr=stat(file1,statb)
      lines1=statb(8)/len/4
      print *,'Lines in file 1: ',lines1

      ierr=stat(file2,statb)
      lines2=statb(8)/len/4
      print *,'Lines in file: 2',lines2
      lines=min(lines1,lines2)

      allocate(float1(len))
      allocate(c(len))
      allocate(out(len))
      allocate(float2(len))

      open(21,file=file1,access='direct',recl=len*4)
      open(22,file=file2,access='direct',recl=len*4)
      open(31,file=filerg,access='direct',recl=len*8)

      do i=1,lines
         read(21,rec=i)float1
         read(22,rec=i)float2
         do j=1,len
            out(j)=cmplx(float1(j),float2(j))
         end do
         write(31,rec=i)out
      end do

      end
