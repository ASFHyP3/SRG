!  fdop - subroutine to get burst angle steering coefficients
!
!  derive from commanded azimuth beam position
!
!  from burst_angle_steering.f90:  get steering angle coefficients for a burst
!

subroutine fdopcoefs(data,len,lines,fdcoefs)

  complex data(len,lines)
  real*8 fdcoefs(2)

  integer word, elevationbeam,azimuthbeam
  integer*1 byte
  integer*1, allocatable ::  b(:,:)
  complex, allocatable ::  header(:,:)
  real, allocatable :: t(:), ang(:), prf(:)
  real*8 fref

  fref=37.53472224

  allocate (b(80,lines), header(10,lines))
  allocate (t(lines),ang(lines),prf(lines))
  !print *,'arrays allocated',len,lines,data(100:103,100:103)

     !  save headers
     header=data(1:10,:)
     call headerbytes(header,b,lines)
     !b=transfer(header,byte)

     ! get azimuth beam address
!  open(31,file='azibeam.out')
     do i=1,lines
        word=in2(b(1+60,i))
        elevationbeam=iand(ishft(word,-12),15)
        azimuthbeam=iand(word,1023)
        ang(i)=azimuthbeam/1024.*1.8-0.9
        prf(i)=fref/in3(b(1+50,i))*1.e6
        t(i)=(i-lines/2.)/prf(i)
!        write(31,*)i,azimuthbeam,ang(i),t(i)
     end do
!     close(31)

!  solve for angle as function of time
     call linfit(t(21),ang(21),lines-40,angc0,angc1)
     !print *,'Angle coeffs: ',angc0,angc1
     print *,angc0,angc1
     fdcoefs(1)=angc0
     fdcoefs(2)=angc1

return

end subroutine fdopcoefs


subroutine medfilt(fd,len,n)

  real, allocatable :: filt(:),temp(:)
  real fd(len)

  allocate (filt(len),temp(n))

  do i=1,n/2
     filt(i)=0.
     filt(len+1-i)=0.
  end do
  do i=n/2+1,len-n/2
     temp=fd(i-n/2:i+n/2)
     call sort(n,temp)
     filt(i)=temp(n/2+1)
  end do
  fd=filt
  return
end subroutine medfilt

      SUBROUTINE SORT(N,RA)
      DIMENSION RA(N)
      L=N/2+1
      IR=N
10    CONTINUE
        IF(L.GT.1)THEN
          L=L-1
          RRA=RA(L)
        ELSE
          RRA=RA(IR)
          RA(IR)=RA(1)
          IR=IR-1
          IF(IR.EQ.1)THEN
            RA(1)=RRA
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J.LE.IR)THEN
          IF(J.LT.IR)THEN
            IF(RA(J).LT.RA(J+1))J=J+1
          ENDIF
          IF(RRA.LT.RA(J))THEN
            RA(I)=RA(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        RA(I)=RRA
      GO TO 10
      END

subroutine linfit(xdata,ydata,npts,c0,c1)

   implicit none                                                                
   real*8 sumx,sumx2,sumxy,sumy,sumy2
   real xdata(npts),ydata(npts),c0,c1
   integer npts,i

   sumx=0.
   sumx2=0.
   sumxy=0.
   sumy=0.
   sumy2=0.
   do i=1,npts
      !print *,i,xdata(i),ydata(i)
      sumx  = sumx + xdata(i) 
      sumx2 = sumx2 + xdata(i) * xdata(i) 
      sumxy = sumxy + xdata(i) * ydata(i)
      sumy  = sumy + ydata(i)
      sumy2 = sumy2 + ydata(i) * ydata(i)
   end do

   c1 = (npts * sumxy  -  sumx * sumy) / (npts * sumx2 - sumx**2) 
   c0 = (sumy * sumx2  -  sumx * sumxy) / (npts * sumx2  -  sumx**2) 
   return

 end subroutine linfit

subroutine headerbytes(in, out, lines)

  integer*1 in(80,lines), out(80,lines)
  integer lines,i

  out=in

  return
end subroutine headerbytes

integer function in2(data)
  integer*1 data(*)
  in2=iand(int(data(2)),255)+256*iand(int(data(1)),255)
  return
end function in2

integer function in3(data)
  integer*1 data(*)
  in3=iand(int(data(3)),255)+256*iand(int(data(2)),255)+256*256*iand(int(data(1)),255)
  return
end function in3

real*8 function samplefrequency(rangeDecimation)
  real*8 fref
  integer rangeDecimation

  fref=37.53472224d0;

  select case (rangeDecimation)
  case (0)
     samplefrequency= 3./4.*4.*fref
  case (1)
     samplefrequency= 2./3.*4.*fref
  case (3)
     samplefrequency= 5./9.*4.*fref
  case (4)
     samplefrequency= 4./9.*4.*fref
  case (5)
     samplefrequency= 3./8.*4.*fref
  case (6)
     samplefrequency= 1./3.*4.*fref
  case (7)
     samplefrequency= 1./6.*4.*fref
  case (8)
     samplefrequency= 3./7.*4.*fref
  case (9)
     samplefrequency= 5./16.*4.*fref
  case (10)
     samplefrequency= 3./26.*4.*fref
  case (11)
     samplefrequency= 4./11.*4.*fref
  end select
  return
end function samplefrequency

