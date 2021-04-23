!c  stackcpx -- stack a bunch of complex mph format images

      use omp_lib

      complex, allocatable :: a(:),b(:),acc(:,:)
      character*100 filelist,str,outfile
      real out(65536)
      integer statb(13),stat

!!$      !$omp parallel
!!$      if(omp_get_thread_num().eq.1)&
!!$      print *, 'Max threads used: ', omp_get_num_threads()
!!$      !$omp end parallel

      pi=3.1415926535
      if(iargc().lt.4)then
         write(*,*)'usage: stackcpx filelist-file outfile length <lines> <first-to-conjg>'
         stop
      end if

      call getarg(1,filelist)
      call getarg(2,outfile)
      call getarg(3,str)
      read(str,*)npix
      if(iargc().ge.4)then
         call getarg(4,str)
         read(str,*)nlines
      end if
      nfirst=100000
      if(iargc().ge.5)then
         call getarg(5,str)
         read(str,*)nfirst
      end if

      allocate (acc(npix,nlines),a(npix))

      acc=cmplx(0.,0.)
      open(20,file=filelist)
      do i=1,100000
         read(20,*,end=100)str
         open(21,file=trim(str),access='direct',recl=npix*8)
         do j=1,nlines
            read(21,rec=j,err=99)a
            if(i.ge.nfirst)then
               acc(:,j)=acc(:,j)+conjg(a)
            else
               acc(:,j)=acc(:,j)+a
            end if
         end do
99       continue
         close(21)
      end do
100   continue
      close(20)

      open(22,file=trim(outfile),access='direct',recl=nlines*npix*8)
      write(22,rec=1)acc
      close(22)

      end
