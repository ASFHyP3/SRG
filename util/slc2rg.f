c  slc2rg - pack two slc magnitudes into an rg amp file
      complex a(16384),b(16384),c(16384)
      character*60 f1,f2,fout
      real out(32768)
      integer statb(13),stat

      if(iargc().lt.4)then
         print *,'usage: slc2rg slc_file1 slc_file2 outfile length <lines>'
         stop
      end if

      call getarg(4,fout)
      read(fout,*)npix
      call getarg(1,f1)
      ierr=stat(f1,statb)
      if(iargc().ge.5)then
         call getarg(5,fout)
         read(fout,*)nlines
      else
         nlines=statb(8)/npix/8
      end if
      call getarg(2,f2)
      call getarg(3,fout)

      open(21,file=f1,form='unformatted',status='unknown',
     +  access='direct',recl=npix*8)
      open(22,file=f2,form='unformatted',status='unknown',
     +  access='direct',recl=npix*8)
      open(23,file=fout,form='unformatted',status='unknown',
     +  access='direct',recl=npix*8)

      do line=1,nlines
         if(mod(line,128).eq.0)write(*,*)line
         read(21,rec=line)(a(k),k=1,npix)
         read(22,rec=line)(b(k),k=1,npix)
         do k=1,npix
            amp1=cabs(a(k))
            amp2=cabs(b(k))
            c(k)=cmplx(amp1,amp2)
         end do
         write(23,rec=line)(c(k),k=1,npix)
      end do

      end
