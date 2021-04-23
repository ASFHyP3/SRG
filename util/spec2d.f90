!c  spec2d -- plot 2d spectrum of complex file

      complex*8, allocatable :: data(:,:), array(:)
      real*8, allocatable :: acspec(:), dnspec(:)
      character*160 file

      if(iargc().lt.4)then
         print *,'usage: spec2d file length startpix startline'
         stop
      end if

      call getarg(2,file)
      read(file,*)len
      call getarg(3,file)
      read(file,*)ipix
      call getarg(4,file)
      read(file,*)iline
      call getarg(1,file)

!c  fft length
      lines=1024
      do i=1,16
         lenfft=2**i
         if(lenfft.ge.lines)go to 99
      end do
99      print *,'fft length ',lenfft

!c  allocate arrays
      allocate (data(lenfft,lenfft),array(len),acspec(lenfft),dnspec(lenfft))
      open(21,file=file,access='direct',recl=len*8)
      open(22,file='spec2d.out',access='direct',recl=lenfft*lenfft*8)

      do i=iline,iline+lenfft-1
         read(21,rec=i)array
         data(:,i-iline+1)=array(ipix:ipix+lenfft-1)
      end do
      close(21)
!c  transform array
      call fft2d(data,lenfft,lenfft,-1)

      write(22,rec=1)data
      close(22)

!c  individual directional averages
      acspec=sum(cabs(data),2)
      dnspec=sum(cabs(data),1)
      open(23,file='acspec')
      open(24,file='dnspec')
      do i=1,lenfft
         write(23,*)acspec(i)
         write(24,*)dnspec(i)
      end do
      close(23)
      close(24)

      end

      subroutine fft2d(arr,n1,n2,dir)

      integer*4 n1, n2, dir
      complex*8 arr(n1,n2), dum(8192)

      do i = 1 , n2
         call fft(arr(1,i),n1,dir)
      end do
      do i = 1 , n1
         do j = 1 , n2
            dum(j) = arr(i,j)
         end do
         call fft(dum,n2,dir)
         do j = 1 , n2
             arr(i,j) = dum(j)
         end do
      end do
      return
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
