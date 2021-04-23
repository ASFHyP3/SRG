c  makecc -- make correlation coefficient image
      complex a(16384),b(16384)
      character*60 famp,fmph,fout
      real out(32768)
      integer statb(13),stat

      pi=3.1415926535
      if(iargc().lt.4)then
         write(*,*)'usage: makecc intfile ampfile outfile length <lines>'
         stop
      end if

      call getarg(4,fmph)
      read(fmph,*)npix
      call getarg(1,fmph)
      ierr=stat(fmph,statb)
      if(iargc().ge.5)then
         call getarg(5,famp)
         read(famp,*)nlines
      else
         nlines=statb(8)/npix/8
      end if
      call getarg(2,famp)
      call getarg(3,fout)

      open(21,file=fmph,form='unformatted',status='unknown',
     +  access='direct',recl=npix*8)
      open(22,file=famp,form='unformatted',status='unknown',
     +  access='direct',recl=npix*8)
      open(23,file=fout,form='unformatted',status='unknown',
     +  access='direct',recl=npix*8)

      do line=1,nlines
         if(mod(line,128).eq.0)write(*,*)line
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
         end do
         write(23,rec=line)(out(k),k=1,2*npix)
      end do

      end
