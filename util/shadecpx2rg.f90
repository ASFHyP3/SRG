!c  shadecpx2rg - convert a shaded relief i*2 and a complex image magnitude to an rg file

      integer*2, allocatable :: i2(:)
      real, allocatable :: shade(:)
      complex, allocatable :: c(:),out(:)
      character*60 filei2,filecpx,filerg,str
      integer stat,statb(13)
      integer*8 nbytes,filelen

      if(iargc().lt.4)then
         print *,'Usage:  shadecpx2rg i2file cpxfile rgfile length <shadescale=1>'
         stop
      end if

      call getarg(1,filei2)
      call getarg(2,filecpx)
      call getarg(3,filerg)
      call getarg(4,str)
      read(str,*)len
      scale=1.
      if(iargc().ge.5)then
         call getarg(5,str)
         read(str,*)scale
      end if

!c  file size
      ierr=stat(filei2,statb)
      linesi2=statb(8)/len/2
      print *,'Lines in i2 file: ',linesi2

!      ierr=stat(filecpx,statb)
!      linescpx=statb(8)/len/8
      nbytes=filelen(filecpx)
      print *,'cpx file length, bytes: ',nbytes
      linescpx=nbytes/len/8
      print *,'Lines in cpx file: ',linescpx
      lines=min(linesi2,linescpx)

      allocate(i2(len))
      allocate(c(len))
      allocate(out(len))
      allocate(shade(len))

      open(21,file=filei2,access='direct',recl=len*2)
      open(22,file=filecpx,access='direct',recl=len*8)
      open(31,file=filerg,access='direct',recl=len*8)

      do i=1,lines
         read(21,rec=i)i2
!c  shade this line
         do j=1,len-1
            shade(j)=(i2(j+1)-i2(j))*scale+100
            if(shade(j).lt.0.)shade(j)=0.
            if(shade(j).gt.200.)shade(j)=200.
         end do
         shade(len)=0

         read(22,rec=i)c
         do j=1,len
            out(j)=cmplx(shade(j),cabs(c(j)))
         end do
         write(31,rec=i)out
      end do

      end
