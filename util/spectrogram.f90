!c  spectrogram -- plot image azimuth spectrogram 

      complex*8, allocatable :: data(:,:), out(:,:), array(:)
      character*160 file

      if(iargc().lt.3)then
         print *,'usage: spectrogram file length lines'
         stop
      end if

      call getarg(2,file)
      read(file,*)len
      call getarg(3,file)
      read(file,*)lines
      call getarg(1,file)

!c  fft length
      do i=1,16
         lenfft=2**i
         if(lenfft.ge.lines)go to 99
      end do
99      print *,'fft length ',lenfft

!c  allocate arrays
      allocate (data(len,lines),out(len,lenfft),array(lenfft))

      open(21,file=file,access='direct',recl=lines*len*8)
      open(22,file='spectrogram.out',access='direct',recl=lenfft*len*8)

      read(21,rec=1)data
      close(21)
      print *,'Data loaded.'
      do i=1,len
         array=cmplx(0.,0.)
         array(1:lines)=data(i,:)
         if(mod(i,1000).eq.0)print *,i
         call fft(array,lenfft,-1)
         out(i,:)=array
      end do

      write(22,rec=1)out
      close(22)

      end

!c  fft -- this is the four1 routine from numerical recipes
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
