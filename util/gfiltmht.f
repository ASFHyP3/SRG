c  gfiltmht - adaptive filter for interferometric fringes in mht format
c   use smoothed spectrum to determine passband/weights
c   from idea of Dick Goldstein in 1997

      integer size,width
      parameter (size=32)
      complex t(size,size)
      real s(size,size),smooth(size,size)
      character*50 fin,fout
      real, allocatable :: indata(:,:),outdata(:,:)

      if(iargc().lt.5)then
         print *,'usage: gfilt infile outfile linelength alpha box-half-size <edge-pix>'
         stop
      end if
      iedge=0
      if(iargc().ge.6)then
         call getarg(6,fin)
         read(fin,*)iedge
      end if
      call getarg(3,fin)
      read(fin,*)len
      call getarg(4,fin)
      read(fin,*)alpha
      call getarg(5,fin)
      read(fin,*)isize
      call getarg(1,fin)
      call getarg(2,fout)

      allocate(indata(len*2,size))
      allocate(outdata(len*2,size))

c  some initializations
      call fftww(size,t,0)
      pi=3.14159265359

c  zero out outdata array
      do i=1,len
         do j=1,size
            outdata(i,j)=cmplx(0.,0.)
         end do
      end do
c  open files
      open(21,file=fin,access='direct',recl=len*8)
      open(22,file=fout,access='direct',recl=len*8)

c  write some zero lines if necessary
      do i=1,len
         outdata(i,1)=cmplx(0.,0.)
      end do
      if(iedge.gt.0)then
      do i=1,iedge
         write(22,rec=i)(outdata(k,1),k=1,len)
      end do
      end if

c  get data from input file
      do line=1,100000,size-2*iedge
c         print *,'At line ',line
c  read in size lines
         do i=1,size
            read(21,rec=line+i-1,err=90)(indata(k,i),k=1,len*2)
            lastline=line+i-1
         end do
c  extract part to filter, loop across
         do k=1,len,size-2*iedge
            do i=1,size
               do j=1,size
                  t(i,j)=indata(k+i-1+len,j)
               end do
            end do
c  filter that part
            call fft2d(t,size,size,-1)
c  create array of magnitudes
            do i=1,size
               do j=1,size
                  s(i,j)=cabs(t(i,j))
               end do
            end do
c  smooth the amplitudes
            wgt=(2*isize+1)*(2*isize+1)
            peak=0.
            do i=1,size
               do j=1,size
                  smooth(i,j)=0.
                  do ii=-isize,isize
                     do jj=-isize,isize
                        iii=i+ii
                        jjj=j+jj
                        if(iii.le.0)iii=iii+size
                        if(iii.gt.size)iii=iii-size
                        if(jjj.le.0)jjj=jjj+size
                        if(jjj.gt.size)jjj=jjj-size
                        smooth(i,j)=smooth(i,j)+s(iii,jjj)
                     end do
                  end do
                  smooth(i,j)=smooth(i,j)/wgt
                  peak=max(peak,smooth(i,j))
               end do
            end do
            if(peak.gt.1.e-28)then
c  weight the spectrum
            do i=1,size
               do j=1,size
                  t(i,j)=t(i,j)*(smooth(i,j)/peak)**alpha
c                  t(i,j)=(smooth(i,j)/peak)
               end do
            end do
            end if
c  inverse transform
            call fft2d(t,size,size,1)
c  put into output array
            do i=1+iedge,size-iedge
               do j=1+iedge,size-iedge
                  outdata(k+i-1+len,j)=real(t(i,j))/size/size
                  outdata(k+i-1,j)=indata(k+i-1,j)
               end do
            end do
         end do
c  write to disk
         do i=1+iedge,size-iedge
            write(22,rec=line+i-1)(outdata(k,i),k=1,len*2)
            lastwritten=line+i-1
         end do
         print *,'lastwritten ',lastwritten
      end do
            
 90   continue
      if(lastwritten.lt.lastline)then
      print *,'writing zero lines from',lastwritten+1,' to',lastline
c      type *,lastwritten,line,len,size
      do i=1,len
         outdata(i,1)=cmplx(0.,0.)
      end do

      do i=lastwritten+1,lastline
         write(22,rec=i)(outdata(k,1),k=1,len*2)
      end do
      end if

      end

      subroutine fft2d(arr,n1,n2,dir)

      integer*4 n1, n2, dir
      complex*8 arr(n1,n2), dum(8192)

      do i = 1 , n2
c         call fft(arr(1,i),n1,dir)
         call fftww(n1,arr(1,i),dir)
      end do
      do i = 1 , n1
         do j = 1 , n2
            dum(j) = arr(i,j)
         end do
c         call fft(dum,n2,dir)
         call fftww(n2,dum,dir)
         do j = 1 , n2
             arr(i,j) = dum(j)
         end do
      end do
      return
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

      subroutine fftww(n,array,dir)
      complex array(*),out(65536)
      integer dir
      integer*8 plani(16),planf(16)

      common /fftwcommon/planf,plani

!c  plan creation if dir = 0
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

!c  calculate transform
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


