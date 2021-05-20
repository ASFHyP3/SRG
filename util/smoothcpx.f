c  smoothcpx - smooth a complex image

      complex data(8192),a(8192*8192),ref(8192*8192)
      character*60 str,fin,fout

      if(iargc().lt.5)then
         print *,'usage: smoothcpx infile outfile pixels lines box_size(odd) <copy-amplitudes 1/0 =0>'
         stop
      end if

      call getarg(1,fin)
      call getarg(2,fout)
      call getarg(3,str)
      read(str,*)npix
      call getarg(4,str)
      read(str,*)lines
      call getarg(5,str)
      read(str,*)isize
      icopy=0
      if(iargc().ge.6)then
         call getarg(6,str)
         read(str,*)icopy
      end if

c  open the files
      open(21,file=fin,access='direct',recl=npix*8)
      open(22,file=fout,access='direct',recl=npix*8)

c  get the power of two ge the data sizes
      do i=1,13
         if(npix.ge.2**i)lenx=2**(i+1)
         if(lines.ge.2**i)leny=2**(i+1)
      end do
      print *,'Transform lengths: ',lenx,leny

c  read in input data
      do line=1,lines
         read(21,rec=line)(data(k),k=1,npix)
c  transform in x
         do k=npix+1,lenx
            data(k)=cmplx(0.,0.)
         end do
         call fft(data,lenx,-1)
         do k=1,lenx
            a((line-1)*lenx+k)=data(k)  !store x transformed data in a
         end do
      end do
c      close(21)

c  transform data in y
      do k=1,lenx
         do line=1,lines
            data(line)=a((line-1)*lenx+k)
         end do
         do line=lines+1,leny
            data(line)=cmplx(0.,0.)
         end do
         call fft(data,leny,-1)
         do line=1,leny
            a((line-1)*lenx+k)=data(line)
         end do
      end do

c  transform averaging box
      do i=1,lenx*leny
         ref(i)=cmplx(0.,0.)
      end do
c  create box
      do i=-isize/2,isize/2
         do j=-isize/2,isize/2
            ii=i+1
            if(ii.lt.1)ii=ii+lenx
            jj=j+1
            if(jj.lt.1)jj=jj+leny
            ref((jj-1)*lenx+ii)=1./float(isize)/float(isize)
         end do
      end do

c  transform in x
      do line=1,leny
         call fft(ref((line-1)*lenx+1),lenx,-1)
      end do
c  transform in y
      do k=1,lenx
         do line=1,leny
            data(line)=ref((line-1)*lenx+k)
         end do
         call fft(data,leny,-1)
         do line=1,leny
            ref((line-1)*lenx+k)=data(line)
         end do
      end do
c$$$      open(31,file='qq',access='direct',recl=lenx*8)
c$$$      do line=1,leny
c$$$         write(31,rec=line)(ref(k+(line-1)*lenx),k=1,lenx)
c$$$      end do
c$$$      close(31)

c  cross multiply
      do i=1,lenx*leny
         a(i)=a(i)*ref(i)
      end do

c$$$      open(31,file='q',access='direct',recl=lenx*8)
c$$$      do line=1,leny
c$$$         write(31,rec=line)(a(k+(line-1)*lenx),k=1,lenx)
c$$$      end do
c$$$      close(31)

c  inverse transform in x
      do line=1,leny
         call fft(a((line-1)*lenx+1),lenx,1)
      end do
c  inverse transform in y
      do k=1,lenx
         do line=1,leny
            data(line)=a((line-1)*lenx+k)/leny
         end do
         call fft(data,leny,1)
         do line=1,leny
            a((line-1)*lenx+k)=data(line)/leny
         end do
      end do
      
c  pack into output array
      do line=1,lines
         if(icopy.eq.1)then
            read(21,rec=line)(data(k),k=1,npix)
            do k=1,npix
               amp=cabs(a(k+(line-1)*lenx))
               if(amp.gt.1.e-20)a(k+(line-1)*lenx)=a(k+(line-1)*lenx)/amp*cabs(data(k))
            end do
         end if
         write(22,rec=line)(a(k+(line-1)*lenx),k=1,npix)
      end do



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
