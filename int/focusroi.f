c  focusroi - generate autofocus corrections from roi input file

      complex a(16384),b(8192),c(8192),aa(16384)
      real d(16384)
      character*50 f1,f2
      character*80 roiin(100)

      pi=4.d0*atan2(1.d0,1.d0)
      if(iargc().lt.1)then
         print *,'usage: focusroi roi.in <datafile(trans1.dat)> <length(8192)> <startrow(500)> <stoprow(1000)>'
         stop
      end if

      call getarg(1,f1)
      open(21,file=f1)
      if(iargc().ge.3)then
         call getarg(3,f2)
         read(f2,*)lenlen
      else
         lenlen=8192
      end if
      if(iargc().ge.4)then
         call getarg(4,f2)
         read(f2,*)irow0
      else
         irow0=500
      end if
      if(iargc().ge.5)then
         call getarg(5,f2)
         read(f2,*)irow1
      else
         irow1=1000
      end if
      if(iargc().ge.2)then
         call getarg(2,f2)
      else
         f2='trans1.dat'
      end if

c  first do autofocus crosscorrelation
c  note: we assume central half of data are good
      
      len=lenlen/2
      call fftww(len,a,0)
      
      open(10,file=f2,form='unformatted',status='unknown',
     +   access='direct',recl=lenlen*8)

      do j=1,len
         d(j)=0.
      end do
      do i=irow0,irow1
         read(10,rec=i)(aa(k),k=1,lenlen)
         do j=1,len
            a(j)=aa(j+len/2)
         end do
         call fftww(len,a,-1)
c  split spectrum
         do j=1,len/2
            b(j)=a(j)
            b(j+len/2)=cmplx(0.,0.)
            c(j)=cmplx(0.,0.)
            c(j+len/2)=a(j+len/2)
         end do
c  inverse xfrm
         call fftww(len,b,1)
         call fftww(len,c,1)
c  detect
         do j=1,len
            b(j)=cmplx(cabs(b(j)),0.)
            c(j)=cmplx(cabs(c(j)),0.)
         end do
c  cross correlate
         call fftww(len,b,-1)
         call fftww(len,c,-1)
         do j=1,len
            b(j)=b(j)*conjg(c(j))
         end do
         call fftww(len,b,1)
c  accumulate
         do j=1,len
            d(j)=d(j)+cabs(b(j))
         end do
      end do

c  print and save cross-correlation
      open(31,file='focusroi.out')
      peak=0.
      do k=-30,30
         kk=k
         if(kk.le.0)kk=kk+len
         print *,k,d(kk)
         write(31,*)k,d(kk)
         if(d(kk).ge.peak)then
            ipeak=k-1
            peak=d(kk)
         end if
      end do
      print *,'Offset = ',ipeak
      close(31)

c  read in roi file
      do i=1,100
         read(21,'(a)',end=10)roiin(i)
c         print *,i,roiin(i)
      end do
 10   print *,'roi lines read: ',i-1
      close(21)

      read(roiin(20),*)re
      read(roiin(22),*)h
      read(roiin(34),*)wvl
      read(roiin(23),*)rho0
      read(roiin(15),*)idum,numbins
      read(roiin(16),*)azdelta,rangedelta
      read(roiin(29),*)fs
      read(roiin(37),*)rslope,rint
      read(roiin(38),*)drslope,drint
      read(roiin(12),*)lines
      read(roiin(28),*)looks
      read(roiin(21),*)v
      read(roiin(24),*)prf
      read(roiin(27),*)azres

c      nlooks=numbins/len
      dr=299792456./fs/2.
      print *
      print *,'roi derived parameters:'
      print *
      print *,'re=      ',re
      print *,'h =      ',h
      print *,'wvl=     ',wvl
      print *,'rho0=    ',rho0
      print *,'numbins= ',numbins
      print *,'az delta=',azdelta
      print *,'r delta =',rangedelta
      print *,'fs =     ',fs
c      print *,'nlooks = ',nlooks
      print *,'dr =     ',dr
      print *,'r slope= ',rslope
      print *,'r int =  ',rint
      print *,'delta r slope= ',drslope
      print *,'delta r int =  ',drint
      print *,'lines per patch: ',lines
      print *,'azimuth looks in roi: ',looks
      print *,'body fixed velocity: ',v
      print *,'prf =    ',prf
      print *,'az res = ',azres
      print *

c  get focusing parameters
      range=rho0+(irow0+irow1)/2.*dr
      veff= v * sqrt(re/(re+h))
      s = -2. * veff**2/(wvl*range)
      dxsamp=veff/prf
      np = range*wvl/(2.0*azres*dxsamp)+2
      tau=np/prf

      print *,'Processed chirp rate: ',s
      print *,'Number of points in reference: ',np
      print *,'Integration time, seconds: ',tau

      deltas=ipeak*2.*s/prf/tau

      print *,'deltas= ',deltas
c  get deltas for velocity, range
      snew=s+deltas
      veffnew=sqrt(-snew/2.*wvl*range)
      vnew= veffnew/sqrt(re/(re+h))
      print *,'If you want to keep range constant, use v= ',vnew

      rangenew=-2.*veff**2/snew/wvl-(irow0+irow1)/2.*dr
      print *,'If you want to keep velocity constant, use range= ',rangenew

      open(21,file='focusroi.out')
      write(21,*)vnew,rangenew
      close(21)

      end

      subroutine fftww(n,array,dir)
      complex array(*),out(65536)
      integer dir
      integer plani(16),planf(16)

      common /fftwcommon/planf,plani

c  plan creation if dir = 0
      if(dir.eq.0)then
         do i=2,16
            if(2**i.eq.n)go to 1
         end do
         write(*,*)'Illegal length'
         return
 1       call fftw_f77_create_plan(planf(i),n,-1,8)
         call fftw_f77_create_plan(plani(i),n,1,8)
         return
      end if

c  calculate transform
      if(dir.eq.-1)then
         if(n.eq.4)call fftw_f77_one(planf(2),array,out)
         if(n.eq.8)call fftw_f77_one(planf(3),array,out)
         if(n.eq.16)call fftw_f77_one(planf(4),array,out)
         if(n.eq.32)call fftw_f77_one(planf(5),array,out)
         if(n.eq.64)call fftw_f77_one(planf(6),array,out)
         if(n.eq.128)call fftw_f77_one(planf(7),array,out)
         if(n.eq.256)call fftw_f77_one(planf(8),array,out)
         if(n.eq.512)call fftw_f77_one(planf(9),array,out)
         if(n.eq.1024)call fftw_f77_one(planf(10),array,out)
         if(n.eq.2048)call fftw_f77_one(planf(11),array,out)
         if(n.eq.4096)call fftw_f77_one(planf(12),array,out)
         if(n.eq.8192)call fftw_f77_one(planf(13),array,out)
         if(n.eq.16384)call fftw_f77_one(planf(14),array,out)
         if(n.eq.32768)call fftw_f77_one(planf(15),array,out)
         if(n.eq.65536)call fftw_f77_one(planf(16),array,out)
      end if
      if(dir.eq. 1)then
         if(n.eq.4)call fftw_f77_one(plani(2),array,out)
         if(n.eq.8)call fftw_f77_one(plani(3),array,out)
         if(n.eq.16)call fftw_f77_one(plani(4),array,out)
         if(n.eq.32)call fftw_f77_one(plani(5),array,out)
         if(n.eq.64)call fftw_f77_one(plani(6),array,out)
         if(n.eq.128)call fftw_f77_one(plani(7),array,out)
         if(n.eq.256)call fftw_f77_one(plani(8),array,out)
         if(n.eq.512)call fftw_f77_one(plani(9),array,out)
         if(n.eq.1024)call fftw_f77_one(plani(10),array,out)
         if(n.eq.2048)call fftw_f77_one(plani(11),array,out)
         if(n.eq.4096)call fftw_f77_one(plani(12),array,out)
         if(n.eq.8192)call fftw_f77_one(plani(13),array,out)
         if(n.eq.16384)call fftw_f77_one(plani(14),array,out)
         if(n.eq.32768)call fftw_f77_one(plani(15),array,out)
         if(n.eq.65536)call fftw_f77_one(plani(16),array,out)
      end if

      return

      end


