!c  zoomcpx -a piece of a complex image

      parameter (insize=64)
      character*60 infile,outfile,str
      complex, allocatable :: a(:,:),b(:,:)
      complex in(32768)
      


      if(iargc()<7)then
         print *,'Usage:  zoomcpx infile outfile len ix iy zoomx zoomy'
         stop
      end if

      call getarg(1,infile)
      call getarg(2,outfile)
      call getarg(3,str)
      read(str,*)len
      call getarg(4,str)
      read(str,*)ix
      call getarg(5,str)
      read(str,*)iy
      call getarg(6,str)
      read(str,*)izoomx
      call getarg(7,str)
      read(str,*)izoomy

!c  allocate arrays
      allocate(a(insize,insize))
      allocate(b(insize*izoomx,insize*izoomy))

      open(21,file=infile,access='direct',recl=len*8)
      do i=iy-insize/2,iy+insize/2-1
!c         print *,'reading line ',i,i-iy+insize/2+1
         read(21,rec=i)(in(k),k=1,len)
         do j=ix-insize/2,ix+insize/2-1
!c         print *,'filling line at pixel ',j,j-ix+insize/2+1
            a(j-ix+insize/2+1,i-iy+insize/2+1)=in(j)
         end do
      end do

!c  dephase the input array
!c      call dephase(a,insize)

!c  transform input array
      call fft2d(a,insize,insize,-1)

!c  weight interpolation filter
      do i=1,insize/2
         do j=1,insize/2
            weight=(0.08+0.92*cos((i-1.0)/(insize/2)*3.14159265/2.))
            weight=weight*(0.08+0.92*cos((j-1.0)/(insize/2)*3.14159265/2.))
            a(i,j)=a(i,j)*weight
            a(insize-i+1,j)=a(insize-i+1,j)*weight
            a(insize-i+1,insize-j+1)=a(insize-i+1,insize-j+1)*weight
            a(i,insize-j+1)=a(i,insize-j+1)*weight
         end do
      end do

!c  put in larger array for interpolation
      do i=1,insize/2
         do j=1,insize/2
            ii=i+insize*izoomy-insize/2
            jj=j+insize*izoomx-insize/2
!c            print *,"i j ii jj ",i,j,ii,jj,i+insize/2,j+insize/2
            b(j,i)=a(j,i)
            b(jj,i)=a(j+insize/2,i)
            b(jj,ii)=a(j+insize/2,i+insize/2)
            b(j,ii)=a(j,i+insize/2)
         end do
      end do

!c      open(31,file='btransform',access='direct',recl=insize*izoomx*8)
!c      do i=1,insize*izoomy
!c         write(31,rec=i)(b(k,i),k=1,insize*izoomx)
!c      end do


!c  inverse transform
      call fft2d(b,insize*izoomx,insize*izoomy,1)

      open(31,file=outfile,access='direct',recl=insize*izoomx*8)
      do i=1,insize*izoomy
         write(31,rec=i)(b(k,i),k=1,insize*izoomx)
      end do

      end

      
      subroutine fft2d(arr,n1,n2,dir)

      integer n1, n2, dir
      complex arr(n1,n2), dum(8192)

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
      
      subroutine dephase(a,n)
      complex a(n,n),csuma,csumd

!c  estimate and remove phase carriers in a complex array
      csuma=cmplx(0.,0.)
      csumd=cmplx(0.,0.)
!c  across first
      do i=1,n-1
         do j=1,n
            csuma=csuma+a(i,j)*conjg(a(i+1,j))
         end do
      end do
!c  down next
      do i=1,n
         do j=1,n-1
            csumd=csumd+a(i,j)*conjg(a(i,j+1))
         end do
      end do

      pha=atan2(aimag(csuma),real(csuma))
      phd=atan2(aimag(csumd),real(csumd))
!c      print *,'average phase across, down: ',pha,phd

!c  remove the phases
      do i=1,n
         do j=1,n
            a(i,j)=a(i,j)*cmplx(cos(pha*i+phd*j),sin(pha*i+phd*j))
         end do
      end do

      return
      end
