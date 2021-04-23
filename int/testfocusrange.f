c  create a test data set for focusrange

      complex a(8192,2048),b(2048),scene(2048),ref(2048),refest(2048)
      character*60 str

      pi=3.14159265359

      if(iargc().lt.2)then
         print *,'usage: testfocusrange slope slope-estimate'
         stop
      end if
      call getarg(1,str)
      read(str,*)slope
      call getarg(2,str)
      read(str,*)slopeest

      do i=1,2048
         b(i)=cmplx(0.,0.)
         scene(i)=cmplx(0.,0.)
         ref(i)=cmplx(0.,0.)
      end do

c  create the range reference
      tau=16e-6
      fs=16e6
      np=fs*tau
      ts=1./fs
      if(mod(np,2).eq.0)np=np+1

      k=0
      do i=-np/2,np/2
         t=i*ts
         phase=pi*slope*t*t
         ref(i+np/2+1)=cmplx(cos(phase),sin(phase))
      end do

c  create a scene
      scene(100)=1.
      scene(400)=2.
      scene(1250)=1.3

c  convolve with chirp
      len=2048
      call fft(ref,len,-1)
      call fft(scene,len,-1)
      do k=1,len
         scene(k)=scene(k)*ref(k)
      end do
      call fft(scene,len,1)

c  print out scene
      open(31,file='scene.out')
      do k=1,len
         write(31,*)cabs(scene(k))
      end do
      close(31)

c  create the estimated range reference
      tau=16e-6
      fs=16e6
      np=fs*tau
      ts=1./fs
      if(mod(np,2).eq.0)np=np+1

      k=0
      do i=-np/2,np/2
         t=i*ts
         phase=pi*slopeest*t*t
         refest(i+np/2+1)=cmplx(cos(phase),sin(phase))
      end do

c  correlate
      call fft(scene,len,-1)
      call fft(refest,len,-1)
      do k=1,len
         scene(k)=scene(k)*conjg(refest(k))
      end do
c  print out spectrum
      open(31,file='spectrum.out')
      do k=1,len
         write(31,*)cabs(ref(k))
      end do
      close(31)
      call fft(scene,len,1)

c  print out focused scene
      open(31,file='focusedscene.out')
      do k=1,len
         write(31,*)cabs(scene(k))
      end do
      close(31)

c  create the trans1.dat file
      do i=1,8192
         do j=1,len
            a(i,j)=scene(j)
         end do
      end do
      open(32,file='trans1.dat',access='direct',recl=8192*2048*8)
      write(32,rec=1)a
      close(32)

      end

c  fft -- this is the four1 routine from numerical recipes
      SUBROUTINE FFT(DATA,NN,ISIGN)
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

      
