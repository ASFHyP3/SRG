! process a set of sentinel bursts

  integer burst, word, elevationbeam, azimuthbeam, rangedecimation
  integer*1, allocatable ::  b(:,:)
  complex, allocatable :: data(:,:), dopest(:,:), dopcent(:), cpxdop(:), datout(:,:), out(:,:), header(:,:)
  real, allocatable :: fd(:), x(:), fdop(:), prf(:)
  real*8, allocatable :: range0(:), samplefreq(:)
  character*200 burstname(20),str
  integer*8 filelen, iplanf, iplanr
  real*8 pi, fref, c, samplefrequency, range
  complex, allocatable :: a(:),ref(:), fullaper(:,:)

  pi=4.d0*atan2(1.d0,1.d0)
  fref=37.53472224d0
  c=299792.458d3
  v0=6400.

  looks=4
  lenfft=3072

  if(iargc().lt.1)then
     print *,'Usage: process_bursts len <fdop, prfs>'
     stop
  end if

  call getarg(1,str)
  read(str,*)len
  fdopused=0.0
  if(iargc().ge.2)then
     call getarg(2,str)
     read(str,*)fdopused
  end if

!c  open a set of burst files

  do i=1,9
     burstname(i)='burst'//char(48+i)
     lines=filelen(burstname(1))/8/len
     open(30+i,file=burstname(i),access='direct',recl=len*8*lines)
  end do
  burstname(10)='burst10'
  burstname(11)='burst11'
  open(40,file='burst10',access='direct',recl=len*8*lines)
  open(41,file='burst11',access='direct',recl=len*8*lines)

  open(51,file='sar.out',access='stream')
  open(52,file='fullres.out',access='stream')

  print *,'files opened'

!  fft inits
  allocate (ref(lenfft), a(lenfft))
  call sfftw_plan_dft_1d(iplanf, lenfft, ref, ref, -1, 64)
  call sfftw_plan_dft_1d(iplanr, lenfft, a, a, +1, 64)

  do burst=1,11
     print *,'Burst, lines: ',burst,lines
     allocate (data(len,lines), dopest(len,lines-1), dopcent(lines), fd(lines), x(lines))
     allocate (cpxdop(lines), datout(len,lenfft/looks), out(len/looks,lenfft/looks))
     allocate (b(80,lines), header(10,lines), fdop(lines), prf(lines), range0(lines), samplefreq(lines))
     allocate (fullaper(len,lenfft))

     read(burst+30,rec=1)data
     !  save headers
     header=data(1:9,:)
     call headerbytes(header,b,lines)
     icoarse=in4(b(1+6,1))
     fine=(in2(b(1+10,1))+0.5)*2.**(-16)
     numpri=in4(b(1+33,1))
     prf(1)=fref/in3(b(1+50,1))*1.e6
     print *,'acq. time, pri number: ',icoarse,fine,numpri,prf(1)

     data(1:10,:)=0.
     data=data/1.e6

     ! get azimuth beam address
     do i=1,lines
        word=in2(b(1+60,i))
        elevationbeam=iand(ishft(word,-12),15)
        azimuthbeam=iand(word,1023)
        angle=azimuthbeam/1024.*1.8-0.9
        fdop(i)=-2.*7000./0.06*angle*pi/180.
        prf(i)=fref/in3(b(1+50,i))*1.e6
        !print *,i,azimuthbeam,angle,fdop(i)
     end do
     ! estimate Doppler centroid
     dopest=data(:,1:lines-1)*conjg(data(:,2:lines))
     dopcent(1:lines-1)=sum(dopest,1)
     dopcent(lines)=dopcent(lines-1)
     ! unwrap estimates
     fd=atan2(aimag(dopcent),real(dopcent))
     call medfilt(fd,lines,5)  ! remove some outliers
     thresh=0.
     do i=1,lines-1
        !if(abs(fd(i+1)-fd(i)).ge.pi)fd(i+1)=fd(i+1)-2.*pi
        if(fd(i).lt.thresh-pi/2.0.and.fd(i+1).gt.thresh+pi/2.0)then
           fd(i+1:lines)=fd(i+1:lines)-2.*pi
           thresh=thresh-2.*pi
        end if
     end do
!     print *,'unwrapped'
     fd=fd/2./pi

!!$     do i=1,1400,100
!!$        print *,i,fd(i),fdop(i)/prf(i)
!!$     end do
     !  fit a line to the unwrapped fd's
     do i=1,lines
        x(i)=i
     enddo
     call linfit(x(21),fd(21),lines-40,c0,c1)
!!$     open(99,file='fd.txt')
!!$     do i=1,lines
!!$        write(99,*)i,fd(i),i*c1+c0
!!$     enddo
!!$     close(99)
     
     !print *,fd(21),c0,c1
     do i=1,lines
        !phase=pi*fd(i)*i
        ! use fitted line instead
        phase=pi*(i*c1+c0)*i
        cpxdop(i)=cmplx(cos(phase),sin(phase))
     end do

!!$     do i=1,len
!!$        data(i,:)=data(i,:)*cpxdop
!!$     end do
!     print *,'data flattened'

     ! sar processing
     ! get needed geometry and parameters
     do i=1,lines
        rangedecimation=iand(b(1+40,i),255)
        samplefreq(i)=samplefrequency(rangedecimation)*1.e6
        swst=in3(b(1+53,i))/fref*1.e-6
        irank=iand(b(1+49,i),31)
        pri=in3(b(1+50,i))/fref*1.e-6
        range0(i)=c/2.*(irank*pri+swst)
!        rangeend=range0+(len-1)/samplefreq*c/2.
!        print *,'range0 swst irank pri ',range0,swst,irank,pri,rangeend,rngedecimation
     end do
     print *,'range start, end, prf, samplefreq',range0(1),range0(1)+c/2.*(len-1)/samplefreq(1),1./pri,samplefreq(1)
     estrate=2*6775.*6775./0.0566/range0(1)

     rate=2*6775.*6775./0.0566/range0(1)
     rateend=2*6775.*6775./0.0566/(range0(1)+len/samplefreq(1)*c/2.)
     iaperture=1./pri/pri/rate
     iaperture=min(iaperture,lines)
     print *,lines,iaperture
     print *,'fdop used: ',fdopused,rate,estrate
     print *,'synthetic aperture pts near, far ',1/rate/pri/pri,1/rateend/pri/pri
     !  set aperture length
     range=range0(1)+(len/2)/samplefreq(1)*c/2.
     rate=2*6775.*6775./0.0566/range ! note plus sign for conjugation
     iaperture=0.9/rate/pri/pri
     print *,'aperture used: ',iaperture,range,rate
     do i=1,len
        !  assume these don't change within a burst
        range=range0(1)+(i-1)/samplefreq(1)*c/2.
        rate=2*v*v/0.0566/range ! note plus sign for conjugation
        iaperture=0.9/rate/pri/pri
        v=6700+(18-1)*5
        ref=0.
        !print *,'ref',lines,i
        do j=1,iaperture!lines-400
           t=(j-1-iaperture/2)/prf(1)
           !t=(j-1)/1681.!prf(j)
           phase=pi*rate*t*t+2.*pi*fdopused*t
           ref(j)=cmplx(cos(phase),sin(phase))
           if(j.eq.1.and.i.eq.1)print *,i,j,rate,t,phase
           if(j.eq.iaperture.and.i.eq.1)print *,i,j,rate,t,phase

!!$           if(j.ge.lines-4.and.i.eq.1)print *,i,j,rate,t,phase
!!$           if(j.le.5.and.i.eq.len)print *,i,j,rate,t,phase
!!$           if(j.ge.lines-4.and.i.eq.len)print *,i,j,rate,t,phase
        end do
!!$        if(i.eq.1)then
!!$           open(61,file='reftime')
!!$           do k=1,lines
!!$              write(61,*)cabs(ref2048(k)),ref2048(k)
!!$           end do
!!$           close(61)
!!$        end if
        call sfftw_execute_dft(iplanf,ref,ref)
        if(i.eq.1)then
           open(62,file='reffreq')
           do k=1,lenfft
              write(62,*)cabs(ref(k)),ref(k)
           end do
           close(62)
        end if
        !print *,rate,i
        a=0.
        a(1:lines)=data(i,:)
        call sfftw_execute_dft(iplanf,a,a)
        a=a*ref
        call sfftw_execute_dft(iplanr,a,a)
        fullaper(i,:)=a
     end do
!     write(52)fullaper
!!$     open(98,file='azspec',access='stream')
!!$     write(98)aout
!!$     close(98)
!!$     deallocate (a,aout)
        
     ! azimuth looks
     print *,'azimuth looks...'
     iout=0
     do i=1,lenfft-looks+1,looks
        iout=iout+1
        datout(:,iout)=sum(cabs(fullaper(:,i:i+looks-1)),2)
     end do
!     print *,'unfocus finished',burst

     ! range looks
     iout=0
     do i=1,len-looks+1,looks
        iout=iout+1
        out(iout,:)=sum(cabs(datout(i:i+looks-1,:)),1)
     end do
     write(51)out

     deallocate (data, dopest, dopcent, fd, x, cpxdop, datout, out, b, header, fdop, prf)
     deallocate (range0, samplefreq, fullaper)
  end do

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

integer function in4(data)
  integer*1 data(*)
  in4=iand(data(4),255)+256*iand(data(3),255)+256*256*iand(data(2),255)+256*256*256*iand(data(1),255)
  return
end function in4

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
