c     ampoffset -  estimate offsets in two complex images by 
c     cross-correlating magnitudes
c     modified 24apr98 to use fft correlations rather than time-domain
      
      parameter (NPTS = 64)
      parameter (NOFF = 20)
      parameter (NDISP= 8)

      complex temp(32768)
      complex a(NPTS,NPTS),aa(NPTS*2,NPTS*2)
      complex b(NPTS*2,NPTS*2),bb(NPTS*4,NPTS*4) !twice as many lines
      complex cpa(NPTS*4,NPTS*4),cpb(NPTS*4,NPTS*4)
      complex corr(8,8),corros(128,128)
      real pa(NPTS*2,NPTS*2),pb(NPTS*4,NPTS*4)
      real c(-NOFF:NOFF,-NOFF:NOFF)
      integer ic(-NOFF-NDISP:NOFF+NDISP,-NOFF-NDISP:NOFF+NDISP)
      integer dsamp,ilin1(0:255)

      character*60 file(4)
      character*1 talk
      logical ex
      
c  run silent ?
      talk='y'
      if(iargc().ge.1)then
         call getarg(1,talk)
      end if

      if(talk.eq.'y')write(*,*)'**** Amplitude cross-correlation offsets ****'
      if(talk.eq.'y')write(*,*)' Capture range is +/- ',NOFF/2,' pixels'
      if(talk.eq.'y')write(*,*)' Initializing ffts'
      do i=5,14
         k=2**i
         call cfft1d(k,a,0)
      end do


 10   if(talk.eq.'y')print '(a,$)','complex data filename 1: '
      read(*,'(a)')file(1)
      inquire(file=file(1),exist=ex)
      if (.not.ex) then 
         write(*,*)'ERROR...file does not exist !'
         goto 10
      end if	
      if(talk.eq.'y')print '(a,$)','number of across samples : '
      read(*,*)len
      open(21, FILE = file(1), FORM = 'unformatted', 
     +     STATUS='old',ACCESS='direct', RECL=len*8)
      
 20   if(talk.eq.'y')print '(a,$)','complex data filename 2: '
      read(*,'(a)')file(2)
      inquire(file=file(2),exist=ex)
      if (.not.ex) then 
         write(*,*)'ERROR...file does not exist !'
         goto 20
      end if	
      open(22, FILE = file(2), FORM = 'unformatted', 
     +     STATUS = 'old',ACCESS = 'direct', RECL = len*8) 
      
      if(talk.eq.'y')print '(a,$)','output offsets data filename: '
      read(*,'(a)')file(4)
      open(31,file=file(4),form='formatted',
     +     status='unknown')
      
      if(talk.eq.'y')print '(a,$)','file 1 init. across, final across, # across locations: '
      read(*,*)isamp_s,isamp_f,nloc
      dsamp=float(isamp_f-isamp_s)/float(nloc-1)
      if(talk.eq.'y')write(*,*)'across step size: ',dsamp
      if(talk.eq.'y')print '(a,$)','file 2 approximate integer offsets in across, down: '
      read(*,*)ioffac,ioffdn
      
      if(talk.eq.'y')print '(a,$)','number of down locations: '
      read(*,*)ndnloc
      
      if(talk.eq.'y')print *
      do j=0,ndnloc-1
         if(talk.eq.'y')print'(a,i3,a,$)','file 1 location down ',j+1,': '
         read(*,*)ilin1(j)
      end do
      
      if(talk.eq.'y')print '(a,$)','minimum required SNR to save offset data: '
      read(*,*)snr_min
      
      do idnloc=0,ndnloc-1
         if(talk.eq.'y')print *
         if(talk.eq.'y')write(*,*)'down file 1: ', ilin1(idnloc)

         do n=1,nloc

            if(talk.eq.'y')write(*,*)'reading file 1...',n
            
            irec=ilin1(idnloc)-1
            do j=1,NPTS         !read input data (stationary part)
               read(unit=21,rec=irec+j)(temp(k),k=1,len)
               do i=1,NPTS
                  a(i,j)=temp(i+(n-1)*dsamp+isamp_s)
c                  write(*,*)a(i,j)
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

c     read in file 2 data (twice as much)

            irec=ilin1(idnloc)+ioffdn-NPTS/2-1 !offset in down
            do j=1,NPTS*2
               read(unit=22,rec=irec+j)(temp(k),k=1,len)
               do i=1,NPTS*2
                  b(i,j)=temp(i-NPTS/2+(n-1)*dsamp+isamp_s+ioffac)
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
                  if(c(ioff,joff).ge.cmax)then
                     cmax=max(cmax,c(ioff,joff))
                     ipeak=ioff
                     jpeak=joff
                  end if
c                  write(*,*)cmax
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
            snr=cmax/(cave/(2*NOFF+1)**2)
            if(talk.eq.'y')print *
c  print out absolute correlations at original sampling rate
            if(talk.eq.'y')write(*,*)'Absolute offsets, original sampling interval:'
            do kk=-NDISP*2,NDISP*2,2
               if(talk.eq.'y')print '(1x,17i4)',(ic(k,kk),k=-NDISP*2,NDISP*2,2)
            end do
            if(talk.eq.'y')print *
            if(talk.eq.'y')write(*,*)'Expansion of peak, sample interval 0.5 * original:'
            do kk=jpeak-NDISP,jpeak+NDISP
               if(talk.eq.'y')print '(1x,17i4)',(ic(k,kk),k=ipeak-NDISP,ipeak+NDISP)
            end do
            if(talk.eq.'y')print *
c            write(*,*)'Integer peaks at ',ioffdn+ipeak/2.,ioffac+jpeak/2.
c            print *
c  get interpolated peak location from fft and expand by 16
c  load corr with correlation surface
            do ii=1,8
               do jj=1,8
                  corr(ii,jj)=cmplx(c(ipeak+ii-4,jpeak+jj-4),0.)
               end do
            end do
            call interpolaten(corr,corros,8,16)
            peak=0.
            do ii=1,128
               do jj=1,128
                  if(cabs(corros(ii,jj)).ge.peak)then
                     peak=cabs(corros(ii,jj))
                     iip=ii
                     jjp=jj
                  end if
               end do
            end do
            offac=iip/32.-65/32.
            offdn=jjp/32.-65/32.
c            write(*,*)'fft offac, offdn: ',offac,offdn

c  get interpolated peaks using quadratic approximation
c            offac=((c(ipeak-1,jpeak)-c(ipeak,jpeak))/(c(ipeak+1,jpeak)-
c     +             2.*c(ipeak,jpeak)+c(ipeak-1,jpeak))-0.5)/2.
c            offdn=((c(ipeak,jpeak-1)-c(ipeak,jpeak))/(c(ipeak,jpeak+1)-
c     +             2.*c(ipeak,jpeak)+c(ipeak,jpeak-1))-0.5)/2.
c            write(*,*)'quadratic offac, offdn: ',offac,offdn



            if(talk.eq.'y')write(*,*)'Interpolated across peak at ',offac+ioffac+ipeak/2.
            if(talk.eq.'y')write(*,*)'Interpolated down peak at   ',offdn+ioffdn+jpeak/2.
            if(talk.eq.'y')write(*,*)'SNR: ',snr
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
c      write(*,*)'average phase across, down: ',pha,phd

c  remove the phases
      do i=1,n
         do j=1,n
            a(i,j)=a(i,j)*cmplx(cos(pha*i+phd*j),sin(pha*i+phd*j))
         end do
      end do

c  check results
c      csuma=cmplx(0.,0.)
c      csumd=cmplx(0.,0.)
c  across first
c      do i=1,n-1
c         do j=1,n
c            csuma=csuma+a(i,j)*conjg(a(i+1,j))
c         end do
c      end do
c  down next
c      do i=1,n
c         do j=1,n-1
c            csumd=csumd+a(i,j)*conjg(a(i,j+1))
c         end do
c      end do
c
c      pha=atan2(aimag(csuma),real(csuma))
c      phd=atan2(aimag(csumd),real(csumd))
c      write(*,*)'check phase across, down: ',pha,phd
      
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
         call cfft1d(n,data(1,i),isign)
      end do
      do i = 1 , n
         do j = 1 , n
            d(j) = data(i,j)
         end do
         call cfft1d(n,d,isign)
         do j = 1 , n
            data(i,j) = d(j)
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

      subroutine cfft1d(n,c,dir)

      integer*4  n, dir, ier
      complex*8    c(*)

      if(dir .eq. 0) return
      if(dir.gt.0)then
         do i=1,n
            c(i)=c(i)/n
         end do
      end if
      call four1(c,n,dir)

      return
      end

c  four1 -- this is the four1 routine from numerical recipes
      SUBROUTINE FOUR1(DATA,NN,ISIGN)
      REAL*8 WR,WI,WPR,WPI,WTEMP,THETA
      DIMENSION DATA(*)
      N=2*NN
      J=1
      DO 11 I=1,N,2
        IF(J.GT.I)THEN
          TEMPR=DATA(J)
          TEMPI=DATA(J+1)
          DATA(J)=DATA(I)
          DATA(J+1)=DATA(I+1)
          DATA(I)=TEMPR
          DATA(I+1)=TEMPI
        ENDIF
        M=N/2
1       IF ((M.GE.2).AND.(J.GT.M)) THEN
          J=J-M
          M=M/2
        GO TO 1
        ENDIF
        J=J+M
11    CONTINUE
      MMAX=2
2     IF (N.GT.MMAX) THEN
        ISTEP=2*MMAX
        THETA=6.28318530717959D0/(ISIGN*MMAX)
        WPR=-2.D0*DSIN(0.5D0*THETA)**2
        WPI=DSIN(THETA)
        WR=1.D0
        WI=0.D0
        DO 13 M=1,MMAX,2
          DO 12 I=M,N,ISTEP
            J=I+MMAX
            TEMPR=SNGL(WR)*DATA(J)-SNGL(WI)*DATA(J+1)
            TEMPI=SNGL(WR)*DATA(J+1)+SNGL(WI)*DATA(J)
            DATA(J)=DATA(I)-TEMPR
            DATA(J+1)=DATA(I+1)-TEMPI
            DATA(I)=DATA(I)+TEMPR
            DATA(I+1)=DATA(I+1)+TEMPI
12        CONTINUE
          WTEMP=WR
          WR=WR*WPR-WI*WPI+WR
          WI=WI*WPR+WTEMP*WPI+WI
13      CONTINUE
        MMAX=ISTEP
      GO TO 2
      ENDIF
      RETURN
      END

