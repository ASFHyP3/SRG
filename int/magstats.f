c  magstats - summarize statistics of magnitude of complex images
      complex a(16384)
      character*30 f
      double precision sum,sumsq
      integer statb(13),stat

      if(iargc().lt.2)then
         write(*,*)'usage: magstats infile length <lines>'
         stop
      end if

      call getarg(2,f)
      read(f,*)npix
      call getarg(1,f)
      ierr=stat(f,statb)
      if(iargc().ge.3)then
         call getarg(3,f)
         read(f,*)nlines
      else
         nlines=statb(8)/npix/8
      end if

      call getarg(1,f)
      open(21,file=f,form='unformatted',status='unknown',
     +  access='direct',recl=npix*8)


      kpix=0
      sum=0.
      sumsq=0.

      do line=1,nlines
         if(mod(line,128).eq.0)write(*,*)line
         read(21,rec=line)(a(k),k=1,npix)

         do i=1,npix
            sum=sum+cabs(a(i))
            sumsq=sumsq+cabs(a(i))*cabs(a(i))
            kpix=kpix+1
         end do
      end do

      ave=sum/kpix
      avesq=sumsq/kpix
      stdev=sqrt(avesq-ave**2)

      write(*,*)'Magnitudes-  mean, std. dev.: ',ave,stdev

      end
