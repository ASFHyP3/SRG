! ks and fd for a sentinel burst

  integer burst, word, elevationbeam,azimuthbeam,rangedecimation
  integer*1, allocatable ::  b(:,:)
  complex, allocatable :: data(:,:), dopest(:,:), dopcent(:), header(:,:)
  complex, allocatable :: dopcentroid(:,:)
  real, allocatable :: fd(:,:), x(:), fdc(:)
  character*200 burstname,str
  integer*8 filelen
  real*8 pi, samplefrequency, c, range0
  real*8, allocatable :: ksarray(:), range(:)

  pi=4.d0*atan2(1.d0,1.d0)
  fref=37.53472224
  c=299792458.d0

  if(iargc().lt.2)then
     print *,'Usage: burst_ks_fd burstfile len'
     stop
  end if

  call getarg(1,burstname)
  call getarg(2,str)
  read(str,*)len

!c  open a burst file
  lines=filelen(burstname)/8/len
  
!  print *,'Lines: ',lines,' of length ',len
  open(21,file=burstname,access='direct',recl=len*8*lines)

  allocate (data(len,lines), dopest(len,lines-1))
  allocate (dopcent(lines), fd(lines,11), x(lines),range(11),ksarray(11))
  allocate (dopcentroid(lines,11), fdc(11))
  allocate (b(80,lines), header(10,lines))

  read(21,rec=1)data
     !  save headers
     header=data(1:10,:)
     call headerbytes(header,b,lines)
!     print *,'header transferred'
     data(1:10,:)=0.
     data=data/1.e6
     ! save a few needed parameters
     swst=in3(b(1+53,lines/2))/fref*1.e-6
     irank=iand(b(1+49,lines/2),31)
     pri=in3(b(1+50,lines/2))/fref*1.e-6
     range0=c/2.*(irank*pri+swst)
     rangedecimation=iand(b(1+40,lines/2),255)
     samplefreq=samplefrequency(rangedecimation)*1.e6
     !print *,'ranegdecimation samplefreq r0 ',rangedecimation,samplefreq,range0
     burstprf=fref/in3(b(1+50,lines/2))*1.e6

     ! create Doppler centroid difference array
     dopest=data(:,1:lines-1)*conjg(data(:,2:lines))

     do k=1,11
        irange=(k-1.)*(len-2001.)/11.+1001.
        range(k)=irange*c/2./samplefrequency(rangedecimation)/1.e6+range0
        !print *,'irange range ',irange,range(k)
        dopcentroid(1:lines-1,k)=sum(dopest(irange-1000:irange+1000,:),1)
        dopcentroid(lines,k)=dopcentroid(lines-1,k)

        !  smooth delta phases
        dopcent=dopcentroid(:,k)
        do i=3,lines-2
           dopcent(i)=sum(dopcentroid(i-2:i+2,k))/5.
        end do
        ! unwrap estimates
        fd(:,k)=atan2(aimag(dopcent),real(dopcent))

!!$     if(k.eq.2)then
!!$        open(31,file='fd.out')
!!$        do i=1,lines
!!$           write(31,*)i,fd(i)
!!$        end do
!!$        close(31)
!!$     end if
     call medfilt(fd(:,k),lines,21)  ! remove some outliers

!!$     open(31,file='fd.out')
!!$     do i=1,lines
!!$        write(31,*)i,fd(i)
!!$     end do
!!$     close(31)
     thresh=0.
     do i=1,lines-1
        !if(abs(fd(i+1)-fd(i)).ge.pi)fd(i+1)=fd(i+1)-2.*pi
        if(fd(i,k).lt.thresh-pi/2.0.and.fd(i+1,k).gt.thresh+pi/2.0)then
           fd(i+1:lines,k)=fd(i+1:lines,k)-2.*pi
           thresh=thresh-2.*pi
        end if
     end do
!     print *,'unwrapped'
     fd(:,k)=fd(:,k)/2./pi

     !  fit a line to the unwrapped fd's
     do i=1,lines
        x(i)=i
     enddo
     call linfit(x(21),fd(21,k),lines-40,c0,c1)
     !print *,'linear fitcoeffs: ',c0,c1

     ks=-c1*burstprf*burstprf
     ksarray(k)=ks
     fdfit=(c0+lines/2*c1)*burstprf
     fdc(k)=mod(fdfit,burstprf)
     if(fdfit.le.0)then
        if(fdc(k).le.-burstprf/2.)fdc(k)=fdc(k)+burstprf
     end if
     if(fdfit.ge.0)then
        if(fdc(k).ge.burstprf/2.)fdc(k)=fdc(k)-burstprf
     end if
     !print *,fdfit,fdc(k)

     !print *,' ks fdc prf mod ',ks,fdc(k),burstprf,(c0+lines/2*c1)*burstprf !c0*burstprf
  end do

  !print *,'c fs dr ',c,samplefrequency(rangedecimation),c/2./samplefrequency(rangedecimation)/1.e6


!!$  print *,'ks ',ksarray
!!$  print *,'fdc ',fdc
  print *,sum(ksarray)/11.,sum(fdc)/11.

  open(31,file='fd.out')
  do i=1,lines
     write(31,*)i,fd(i,:)
  end do
  close(31)
  open(31,file='fdfits.out')
  do i=1,11
     write(31,*)i,ksarray(i),fdc(i)
  end do
  close(31)
  
end program

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
  in2=iand(data(2),255)+256*iand(data(1),255)
  return
end function in2

integer function in3(data)
  integer*1 data(*)
  in3=iand(data(3),255)+256*iand(data(2),255)+256*256*iand(data(1),255)
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

     ! get azimuth beam address
!     do i=1,lines
!        word=in2(b(1+60,i))
!        elevationbeam=iand(ishft(word,-12),15)
!        azimuthbeam=iand(word,1023)
!        angle=azimuthbeam/1024.*1.8-0.9
!        fdop(i)=-2.*7000./0.06*angle*pi/180.
!        prf(i)=fref/in3(b(1+50,i))*1.e6
!        !print *,i,azimuthbeam,angle,fdop(i)
!     end do
!
