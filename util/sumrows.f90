!c  add rows or columns of a complex file

      complex, allocatable :: a(:,:)
      complex sum
      character*60 str, file

      if(iargc().lt.3)then
         print *,'Usage: sumrows file xdim ydim'
         stop
      end if

      call getarg(1,file)
      call getarg(2,str)
      read(str,*)nx
      call getarg(3,str)
      read(str,*)ny

      allocate (a(nx,ny))

      open(21,file=file,access='direct',recl=nx*ny*8)
      read(21,rec=1)a
      close(21)

!c  loop over rows first
      do iy=1,ny
         sum=cmplx(0.,0.)
         do ix=1,nx
            sum=sum+a(ix,iy)
         end do
         print *,'rows ',iy,sum
      end do
      do ix=1,nx
         sum=cmplx(0.,0.)
         do iy=1,ny
            sum=sum+a(ix,iy)
         end do
         print *,'cols ',ix,sum
      end do
      end
