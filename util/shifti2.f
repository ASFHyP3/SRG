c     shifti2 - shift an i2 image
      parameter (n=2048)
      integer*2 a(n,n)
      complex ca(n,n),csuma,csumb
      character*60 fin,fout,str
      integer statb(13)

      if(iargc().lt.5)then
         print *,'usage: shifti2 infile outfile length pixac pixdn'
         stop
      end if

      call getarg(1,fin)
      call getarg(2,fout)
      call getarg(3,str)
      read(str,*)len
      call getarg(4,str)
      read(str,*)pixac
      call getarg(5,str)
      read(str,*)pixdn

      open(21,file=fin,access='direct',recl=len*2)
      ierr=stat(fin,statb)
      lines=statb(8)/2/len
      print *,'Lines in file: ',lines
      open(22,file=fout,form='unformatted',access='direct',recl=len*2)

c  set arrays to zero
      do i=1,n
         do j=1,n
            ca(i,j)=cmplx(0.,0.)
         end do
      end do

      do line=1,lines
         read(21,rec=line)(a(j,line),j=1,len)
         do j=1,len
            ca(j,line)=cmplx(a(j,line),0.)
         end do
      end do
c     dephase the image
      call dephase(ca,n,csuma,csumb)
      print *,'dephased'

c     transform the image
      call fft2d(ca,n,-1)
      print *,'transformed'

c     apply a shift gradient in phase
      pi=4.*atan2(1.,1.)
      dphac=pixac*2.*pi/n
      dphdn=pixdn*2.*pi/n
      print *,'Delta phases: ',dphac,dphdn
      do i=-n/2+1,n/2
         do j=-n/2+1,n/2
            ii=i
            jj=j
            if(ii.lt.1)ii=ii+n
            if(jj.lt.1)jj=jj+n
            phac=(i-1)*dphac
            phdn=(j-1)*dphdn
            ca(ii,jj)=ca(ii,jj)*cmplx(cos(phac),sin(-phac))*cmplx(cos(phdn),sin(-phdn))
         end do
      end do
      print *,'gradient applied'

c     inverse transform
      call fft2d(ca,n,1)

c     write out result
      do i=1,lines
         do j=1,len
            a(j,i)=cabs(ca(j,i))/n/n
         end do
         write(22,rec=i)(a(k,i),k=1,len)
      end do
      end


      subroutine dephase(a,n,csuma,csumb)
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
      print *,'average phase across, down: ',pha,phd

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
         call four1(data(1,i),n,isign)
      end do
      do i = 1 , n
         do j = 1 , n
            d(j) = data(i,j)
         end do
         call four1(d,n,isign)
         do j = 1 , n
            data(i,j) = d(j)
         end do
      end do

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
