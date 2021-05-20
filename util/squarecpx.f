c     squarecpx - square a complex image

      complex data(8192)
      character*60 str,fin,fout

      if(iargc().lt.3)then
         print *,'usage: squarecpx infile outfile pixels <prescale>'
         stop
      end if

      call getarg(1,fin)
      call getarg(2,fout)
      call getarg(3,str)
      read(str,*)npix
      prescale=1.0
      if(iargc().ge.4)then
         call getarg(4,str)
         read(str,*)prescale
      end if

c     open the files
      open(21,file=fin,access='direct',recl=npix*8)
      open(22,file=fout,access='direct',recl=npix*8)

c     read in input data
      do line=1,100000
         read(21,rec=line,err=99)(data(k),k=1,npix)

         do k=1,npix
            data(k)=data(k)*prescale
            data(k)=data(k)*data(k)
         end do

         write(22,rec=line)(data(k),k=1,npix)
      end do

 99   continue

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
