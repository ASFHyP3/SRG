c  mhtstats - summarize statistics of mag/height images
      real a(16384)
      character*30 f
      double precision sum,sumsq,sumh,sumsqh
      integer statb(13),stat

      if(iargc().lt.2)then
         write(*,*)'usage: mhtstats infile length <lines>'
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
      sumsqh=0.
      sumh=0.

      do line=1,nlines
         if(mod(line,128).eq.0)write(*,*)line
         read(21,rec=line)(a(k),k=1,npix*2)

         do i=1,npix
            sum=sum+a(i)
            sumsq=sumsq+a(i)*a(i)
            sumh=sumh+a(i+npix)
            sumsqh=sumsqh+a(i+npix)*a(i+npix)
            kpix=kpix+1
         end do
      end do

      ave=sum/kpix
      aveh=sumh/kpix
      avesq=sumsq/kpix
      avesqh=sumsqh/kpix
      stdev=sqrt(avesq-ave**2)
      stdevh=sqrt(avesqh-aveh**2)

      write(*,*)'Magnitudes-  mean, std. dev.: ',ave,stdev
      write(*,*)
      write(*,*)'Heights-     mean, std. dev.: ',aveh,stdevh

      end
