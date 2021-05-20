!c  makecc -- make correlation coefficient image

      use omp_lib

      complex a(32768),b(32768)
      character*100 famp,fmph,fout
      real out(65536)
      integer statb(13),stat
      integer*8 filelen,nbytes

      !$omp parallel
      if(omp_get_thread_num().eq.1)&
      print *, 'Max threads used: ', omp_get_num_threads()
      !$omp end parallel

      pi=3.1415926535
      if(iargc().lt.4)then
         write(*,*)'usage: makecc intfile ampfile outfile length <lines>'
         stop
      end if

      call getarg(4,fmph)
      read(fmph,*)npix
      call getarg(1,fmph)
!      ierr=stat(fmph,statb)
!c      print *,'ierr, bytes: ',ierr,statb(8)
      if(iargc().ge.5)then
         call getarg(5,famp)
         read(famp,*)nlines
      else
         nbytes=filelen(fmph)
         nlines=nbytes/8/npix
      end if
      write(*,*)'makecc: bytes, pixels, lines in file: ',nbytes,npix,nlines
      call getarg(2,famp)
      call getarg(3,fout)

      open(21,file=fmph,form='unformatted',status='unknown',&
       access='direct',recl=npix*8)
      open(22,file=famp,form='unformatted',status='unknown',&
       access='direct',recl=npix*8)
      open(23,file=fout,form='unformatted',status='replace',&
       access='direct',recl=npix*8)

     !$omp parallel do shared(nlines,npix) private(line,i,amp,a,b,coco,out)
      do line=1,nlines
         if(mod(line,4096).eq.0)write(*,*)line
         read(21,rec=line)(a(k),k=1,npix)
         read(22,rec=line)(b(k),k=1,npix)
         do i=1,npix
            amp=(real(b(i))*aimag(b(i)))
            if(amp.gt.0.0)then
               coco=cabs(a(i))/amp
            else
               coco=0.
            end if
            out(i)=amp
            out(i+npix)=coco
            !if(coco.gt.1.0)print *,a(i),cabs(a(i)),real(b(i)),aimag(b(i))
         end do
         write(23,rec=line)(out(k),k=1,2*npix)
      end do
      !omp end parallel do

      end
