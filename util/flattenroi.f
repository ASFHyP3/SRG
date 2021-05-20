c  flattenroisubset - flatten an i-gram given a dem in radar coords and
c   roi .in file, for a subset of the original interferogram
c   if dem file doesn't exist, use flat earth

      implicit real*8 (a-h,o-z)
      real*4 dem(10000),indat(10000)
      dimension a(3)
      character*60 f1,f2,f3,fin,str
      character*80 roiin(100)
      integer stat,statb(13)
      complex array(16384)

      if(iargc().lt.6)then
         print *,'usage: flattenroi demfile in-i-gram out-i-gram length lines roi.in',
     $      '<start pixel> <start line> <npix> <nlines>'
         print *,'If demfile does not exist, use curved earth.'
         stop
      end if

      call getarg(1,f1)   ! dem file
      ierr=stat(f1,statb)
      call getarg(4,f2)
      read(f2,*)len
      call getarg(5,f2)
      read(f2,*)lines
      call getarg(3,f2)   !  output interferogram
      call getarg(2,fin)  !  input interferogram
      call getarg(6,f3)   !  roi .in file
      if(iargc().ge.7)then
         call getarg(7,str)
         read(str,*)ip0
      else
         ip0=1
      end if
      if(iargc().ge.8)then
         call getarg(8,str)
         read(str,*)line0
      else
         line0=1
      end if
      if(iargc().ge.9)then
         call getarg(9,str)
         read(str,*)npix
      else
         npix=len
      end if
      if(iargc().ge.10)then
         call getarg(10,str)
         read(str,*)nlines
      else
         nlines=lines
      end if
      print *,'ip0, line0, npix, nlines ',ip0,line0,npix,nlines

      open(23,file=f3)
c  open demfile if file exists
      if(ierr.eq.0)open(21,file=f1,access='direct',recl=len*8)
c  input interferogram
      open(22,file=fin,access='direct',recl=npix*8)
c  open output interferogram
      open(24,file=f2,access='direct',recl=npix*8)

c  read in roi params
      do i=1,100
         read(23,'(a80)',end=10)roiin(i)
c         print *,i,roiin(i)
      end do
 10   print *,'roi lines read: ',i-1

      read(roiin(20),*)re
      read(roiin(22),*)h
      read(roiin(34),*)wvl
      read(roiin(23),*)rho0
      read(roiin(15),*)idum,numbins
      read(roiin(29),*)fs
      nlooks=numbins/len
      dr=299792456./fs/2.*nlooks
      print *,'roi derived parameters:'
      print *,'re=      ',re
      print *,'h =      ',h
      print *,'wvl=     ',wvl
      print *,'rho0=    ',rho0
      print *,'numbins= ',numbins
      print *,'fs =     ',fs
      print *,'nlooks = ',nlooks
      print *,'dr =     ',dr


      print '(a,$)','enter horizontal, vertical baselines, phi0, bhdot, bvdot (m/line) : '
      read(*,*)bh0,bv0,phi0,bhdot,bvdot
      
      do line=line0,line0+nlines-1
         bh=bh0+bhdot*(line-1)
         bv=bv0+bvdot*(line-1)
         a(1)=sqrt(bh**2+bv**2)
         a(2)=atan2(bv,bh)
         a(3)=phi0
c  if we have demfile, read it in, else use curved earth
         if(ierr.eq.0)then
            read(21,rec=line,err=99)(dem(k),k=1,len*2)
         else
            do k=len+1,len*2
               dem(k)=0.
               dem(k-len)=1.
            end do
         end if
         do j=1,len
            rho=rho0+(j-1)*dr
            theta=acos((rho**2+(h+re)**2-(re+dem(j+len))**2)/2./rho/(h+re))
c            if(j.eq.1)print *,rho,theta,re,h,dem(j+len)
            sinthetaminusalpha=sin(theta-a(2))
            bdotl = a(1)*sinthetaminusalpha
            arg   = 1. - 2. * bdotl / rho + (a(1)/rho)**2
            dem(j+len)=-4.*3.14159265/wvl*(rho*(1.d0-sqrt(arg)))-a(3)

         end do
c  correct input interferogram
         read(22,rec=line-line0+1)(array(k),k=ip0,ip0+npix-1)
         do k=ip0,ip0+npix-1
            phase=dem(k+len)
            array(k)=array(k)*cmplx(cos(phase),-sin(phase))
         end do
         write(24,rec=line-line0+1)(array(k),k=ip0,ip0+npix-1)
      end do
      
 99   end
      
