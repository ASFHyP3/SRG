c  rg offset - offest between images stored as rg file
c     ampoffset -  estimate offsets in two complex images by 
c     cross-correlating magnitudes
c     modified 24apr98 to use fft correlations rather than time-domain
      
      parameter (NPTS = 32)
      parameter (NOFF = 20)
      parameter (NDISP= 8)

      complex temp(32768),data(16384,NPTS*2)
      complex a(NPTS,NPTS),aa(NPTS*2,NPTS*2)
      complex b(NPTS*2,NPTS*2),bb(NPTS*4,NPTS*4) !twice as many lines
      complex cpa(NPTS*4,NPTS*4),cpb(NPTS*4,NPTS*4)
      complex corr(8,8),corros(128,128)
      real red(16384,NPTS*2),green(16384,NPTS*2)
      real pa(NPTS*2,NPTS*2),pb(NPTS*4,NPTS*4)
      real c(-NOFF:NOFF,-NOFF:NOFF)
      integer ic(-NOFF-NDISP:NOFF+NDISP,-NOFF-NDISP:NOFF+NDISP)
      integer dsamp,ilin1(0:16384)

      character*60 file(4),str
      character*1 talk
      logical ex
      
c  run silent ?
      talk='y'
      if(iargc().ge.11)then
         call getarg(11,talk)
      end if

      if(talk.eq.'y')print *,'**** RG offsets from cross-correlation ****'
      if(talk.eq.'y')print *,' Capture range is +/- ',NOFF/2,' pixels'
      if(talk.eq.'y')print *,' Initializing ffts'
      do i=3,14
         k=2**i
         call fftww(k,a,0)
      end do

      if(iargc().lt.8)then
         print *,'usage:  rgoffset rgfile width firstac lastac nacr firstdn lastdn ndn <x0> <y0> <talk>'
         stop
      end if

      call getarg(1,file(1))
      inquire(file=file(1),exist=ex)
      if (.not.ex) then 
         print *,'ERROR...file does not exist !'
      end if	

      call getarg(2,str)
      read(str,*)len
      open(21, FILE = file(1), FORM = 'unformatted', 
     +     STATUS='old',ACCESS='direct', RECL=len*8)
      
      open(31,file='rgoffset.out',form='formatted',status='unknown')
      
      call getarg(3,str)
      read(str,*)isamp_s
      call getarg(4,str)
      read(str,*)isamp_f
      call getarg(5,str)
      read(str,*)nloc
      dsampac=float(isamp_f-isamp_s)/float(nloc-1)
      print *,'across step size: ',dsampac
      dsamp=dsampac
      if(dsampac-dsamp.ge.1.e10)print *,'Warning: non-integer across sampling'

      call getarg(6,str)
      read(str,*)isamp_sdn
      call getarg(7,str)
      read(str,*)isamp_fdn
      call getarg(8,str)
      read(str,*)nlocdn
      dsampdn=float(isamp_fdn-isamp_sdn)/float(nlocdn-1)
      print *,'down step size: ',dsampdn

      ndnloc=nlocdn
      do j=0,ndnloc-1
         ilin1(j)=isamp_sdn+j*dsampdn
      end do
      
      snr_min=2.
      
      ioffac=0
      ioffdn=0
      if(iargc().ge.9)then
         call getarg(9,str)
         read(str,*)ioffac
      end if
      if(iargc().ge.10)then
         call getarg(10,str)
         read(str,*)ioffdn
      end if

      open(21, FILE = file(1), FORM = 'unformatted', 
     +     STATUS='old',ACCESS='direct', RECL=len*8)
      
      do idnloc=0,ndnloc-1
         if(mod(idnloc,10).eq.0)print *,'On line, location ',idnloc,ilin1(idnloc)

         if(talk.eq.'y')print *
         if(talk.eq.'y')print *,'down file 1: ', ilin1(idnloc)

c  read in the data to data array
         irec=ilin1(idnloc)-NPTS/2-1 !offset in down
         !print *,'irec= ',irec
         do j=1,NPTS*2
            read(unit=21,rec=irec+j)(data(k,j),k=1,len)
            do k=1,len
               red(k,j)=real(data(k,j))
            end do
         end do
c  channel two data
         irec=ilin1(idnloc)-NPTS/2-1+ioffdn !offset in down
         !print *,'irec= ',irec
         do j=1,NPTS*2
            read(unit=21,rec=irec+j)(data(k,j),k=1,len)
            do k=1,len
               green(k,j)=aimag(data(k,j))
            end do
         end do
         do n=1,nloc
c  copy data from first image
            do j=1,NPTS         !read input data (stationary part)
               do i=1,NPTS
                  a(i,j)=red(i+(n-1)*dsamp+isamp_s,j+NPTS/2)
c                  print *,a(i,j)
               end do
            end do
c     estimate and remove the phase carriers on the data
            call dephase(a,NPTS)
c     interpolate the data by 2
            call interpolate(a,aa,NPTS)
c  detect and store interpolated result in pa, after subtracting the mean
            amean=0.
            do i=1,NPTS*2
               do j=1,NPTS*2
                  pa(i,j)=cabs(aa(i,j))
                  amean=amean+pa(i,j)
               end do
            end do
            amean=amean/NPTS**2/4.
            do i=1,NPTS*2
               do j=1,NPTS*2
                  pa(i,j)=pa(i,j)-amean
               end do
            end do
c            print *,(pa(k,NPTS),k=NPTS-3,NPTS+3)
c     read in channel 2 data (twice as much)
            do j=1,NPTS*2
               do i=1,NPTS*2
                  b(i,j)=green(i+ioffac-NPTS/2+(n-1)*dsamp+isamp_s,j)
               end do
            end do
c     estimate and remove the phase carriers on the data
            call dephase(b,NPTS*2)
c     interpolate the data by 2
            call interpolate(b,bb,NPTS*2)

c  detect and store interpolated result in pb, after subtracting the mean
            amean=0.
            do i=1,NPTS*4
               do j=1,NPTS*4
                  pb(i,j)=cabs(bb(i,j))
                  amean=amean+pb(i,j)
               end do
            end do
            amean=amean/NPTS**2/16.
            do i=1,NPTS*4
               do j=1,NPTS*4
                  cpb(i,j)=pb(i,j)-amean
               end do
            end do

c  get freq. domain cross-correlation
c  first put pa array in double-size to match pb
            do i=1,NPTS*4
               do j=1,NPTS*4
                  cpa(j,i)=cmplx(0.,0.)
               end do
            end do
            do i=1,NPTS*2
               do j=1,NPTS*2
                  cpa(i+NPTS,j+NPTS)=pa(i,j)
               end do
            end do
c  fft correlation
            call fft2d(cpa,NPTS*4,-1)
            call fft2d(cpb,NPTS*4,-1)
            do i=1,NPTS*4
               do j=1,NPTS*4
                  cpa(i,j)=conjg(cpa(i,j))*cpb(i,j)
               end do
            end do
            call fft2d(cpa,NPTS*4,1)
c  get peak
            cmax=0.
            do ioff=-NOFF,NOFF
               do joff=-NOFF,NOFF
                  koff=ioff
                  loff=joff
                  if(koff.le.0)koff=koff+NPTS*4
                  if(loff.le.0)loff=loff+NPTS*4
                  c(ioff,joff)=cabs(cpa(koff,loff))**2
                  if(c(ioff,joff).gt.cmax)then
                     cmax=max(cmax,c(ioff,joff))
                     ipeak=ioff
                     jpeak=joff
                  end if
c                  print *,cmax
               end do
            end do
c  get integer peak representation, calculate 'snr'
            cave=0.
            do ioff=-NOFF,NOFF
               do joff=-NOFF,NOFF
                  ic(ioff,joff)=100.*c(ioff,joff)/cmax
                  cave=cave+abs(c(ioff,joff))
               end do
            end do
            if(cave.lt.1.e-30)then
               snr=0.0
            else
               snr=cmax/(cave/(2*NOFF+1)**2)
            end if
!            print *,snr,cmax,cave,'  q'
            if(talk.eq.'y')print *
c  print out absolute correlations at original sampling rate
            if(talk.eq.'y')print *,'Absolute offsets, original sampling interval:'
            do kk=-NDISP*2,NDISP*2,2
               if(talk.eq.'y')print '(1x,17i4)',(ic(k,kk),k=-NDISP*2,NDISP*2,2)
            end do
            if(talk.eq.'y')print *
            if(talk.eq.'y')print *,'Expansion of peak, sample interval 0.5 * original:'
            do kk=jpeak-NDISP,jpeak+NDISP
               if(talk.eq.'y')print '(1x,17i4)',(ic(k,kk),k=ipeak-NDISP,ipeak+NDISP)
            end do
            if(talk.eq.'y')print *
c            print *,'Integer peaks at ',ioffdn+ipeak/2.,ioffac+jpeak/2.
c            print *
c  get interpolated peak location from fft and expand by 16
c  load corr with correlation surface
!            print *,'2  ','interpolate peak',ipeak,jpeak,-NOFF,NOFF
            if(ipeak.gt.NOFF-4)ipeak=NOFF-4
            if(ipeak.lt.-NOFF+4)ipeak=-NOFF+4
            if(jpeak.gt.NOFF-4)jpeak=NOFF-4
            if(jpeak.lt.-NOFF+4)jpeak=-NOFF+4
           do ii=1,8
               do jj=1,8
                  corr(ii,jj)=cmplx(c(ipeak+ii-4,jpeak+jj-4),0.)
               end do
            end do
            call interpolaten(corr,corros,8,16)
            peak=0.
            do ii=1,128
               do jj=1,128
                  if(cabs(corros(ii,jj)).gt.peak)then
                     peak=cabs(corros(ii,jj))
                     iip=ii
                     jjp=jj
                  end if
               end do
            end do
            offac=iip/32.-65/32.
            offdn=jjp/32.-65/32.
c            print *,'fft offac, offdn: ',offac,offdn
c  get interpolated peaks using quadratic approximation
c            offac=((c(ipeak-1,jpeak)-c(ipeak,jpeak))/(c(ipeak+1,jpeak)-
c     +             2.*c(ipeak,jpeak)+c(ipeak-1,jpeak))-0.5)/2.
c            offdn=((c(ipeak,jpeak-1)-c(ipeak,jpeak))/(c(ipeak,jpeak+1)-
c     +             2.*c(ipeak,jpeak)+c(ipeak,jpeak-1))-0.5)/2.
c            print *,'quadratic offac, offdn: ',offac,offdn



            if(talk.eq.'y')print *,'Interpolated across peak at ',offac+ioffac+ipeak/2.
            if(talk.eq.'y')print *,offac,ioffac,ipeak/2.
            if(talk.eq.'y')print *,'Interpolated down peak at   ',offdn+ioffdn+jpeak/2.
            if(talk.eq.'y')print *,'SNR: ',snr
           write(31,'(1x,i6,2x,f12.4,2x,i6,2x,f12.4,2x,f8.3)')
     +            (n-1)*dsamp+isamp_s,offac+ioffac+ipeak/2.,
     +            ilin1(idnloc),offdn+ioffdn+jpeak/2.,snr

         end do
      end do
      end

      subroutine dephase(a,n)
      complex a(n,n),csuma,csumd

c  estimate and remove phase carriers in a complex array
      csuma=cmplx(0.,0.)
      csumd=cmplx(0.,0.)
c  across first
      do i=1,n-1
         do j=1,n
            csuma=csuma+a(i,j)*conjg(a(i+1,j))
         end do
      end do
c  down next
      do i=1,n
         do j=1,n-1
            csumd=csumd+a(i,j)*conjg(a(i,j+1))
         end do
      end do

      pha=atan2(aimag(csuma),real(csuma))
      phd=atan2(aimag(csumd),real(csumd))
c      print *,'average phase across, down: ',pha,phd

c  remove the phases
      do i=1,n
         do j=1,n
            a(i,j)=a(i,j)*cmplx(cos(pha*i+phd*j),sin(pha*i+phd*j))
         end do
      end do

      return
      end

      subroutine interpolate(a,b,n)
      complex a(n,n),b(n*2,n*2)
c  zero out b array
      do i=1,n*2
         do j=1,n*2
            b(i,j)=cmplx(0.,0.)
         end do
      end do
c  interpolate by 2, assuming no carrier on data
      call fft2d(a,n,-1)
c  shift spectra around
      do i=1,n/2
         do j=1,n/2
            b(i,j)=a(i,j)
            b(i+3*n/2,j)=a(i+n/2,j)
            b(i,j+3*n/2)=a(i,j+n/2)
            b(i+3*n/2,j+3*n/2)=a(i+n/2,j+n/2)
         end do
      end do
c  inverse transform
      call fft2d(b,n*2,1)
      return
      end

      subroutine fft2d(data,n,isign)
      complex data(n,n), d(8192)

      do i = 1 , n
         call fftww(n,data(1,i),isign)
      end do
      do i = 1 , n
         do j = 1 , n
            d(j) = data(i,j)
         end do
         call fftww(n,d,isign)
         do j = 1 , n
            data(i,j) = d(j)/n/n
         end do
      end do

      return
      end

      subroutine interpolaten(a,b,n,novr)
      complex a(n,n),b(n*novr,n*novr)

c  zero out b array
      do i=1,n*novr
         do j=1,n*novr
            b(i,j)=cmplx(0.,0.)
         end do
      end do
c  interpolate by novr, assuming no carrier on data
      call fft2d(a,n,-1)
c  shift spectra around
      do i=1,n/2
         do j=1,n/2
            b(i,j)=a(i,j)
            b(i+(2*novr-1)*n/2,j)=a(i+n/2,j)
            b(i,j+(2*novr-1)*n/2)=a(i,j+n/2)
            b(i+(2*novr-1)*n/2,j+(2*novr-1)*n/2)=a(i+n/2,j+n/2)
         end do
      end do
c  inverse transform
      call fft2d(b,n*novr,1)
      return
      end


      subroutine fftww(n,array,dir)
      complex array(*),out(65536)
      integer dir
      integer*8 plani(16),planf(16)

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
