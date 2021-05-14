! process a set of sentinel bursts

  integer burst, word, elevationbeam,azimuthbeam
  integer*1, allocatable ::  b(:,:)
  complex, allocatable :: data(:,:), dopest(:,:), dopcent(:), cpxdop(:), datout(:,:), out(:,:), aout(:,:), a(:), header(:,:)
  real, allocatable :: fd(:), x(:), fdop(:), prf(:)
  character*200 burstname(20),str
  integer*8 filelen, iplanf
  real*8 pi

  pi=4.d0*atan2(1.d0,1.d0)
  fref=37.53472224

  lenunf=16

  if(iargc().lt.1)then
     print *,'Usage: process_bursts len'
     stop
  end if

  call getarg(1,str)
  read(str,*)len

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

  open(51,file='unf.out',access='stream')

  print *,'files opened'

  do burst=1,11
     print *,'Burst, lines: ',burst,lines
     allocate (data(len,lines), dopest(len,lines-1), dopcent(lines), fd(lines), x(lines))
     allocate (cpxdop(lines), datout(len,lines/lenunf), out(len/lenunf,lines/lenunf))
     allocate (b(80,lines), header(10,lines), fdop(lines), prf(lines))

     read(burst+30,rec=1)data
     !  save headers
     header=data(1:10,:)
     call headerbytes(header,b,lines)

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

     do i=1,len
        data(i,:)=data(i,:)*cpxdop
     end do
!     print *,'data flattened'

!!$     allocate(a(2048),aout(len,2048))
!!$     lenfft=2048
!!$     call sfftw_plan_dft_1d(iplanf, lenfft, a, a, -1, 64)
!!$     do i=1,len
!!$        a=0.
!!$        a(1:lines)=data(i,:)
!!$        call sfftw_execute_dft(iplanf,a,a)
!!$        aout(i,:)=a
!!$     end do
!!$     open(98,file='azspec',access='stream')
!!$     write(98)aout
!!$     close(98)
!!$     deallocate (a,aout)
        
     ! and finally do the unfocused processing
     iout=0
     do i=1,lines-lenunf+1,lenunf
        iout=iout+1
        datout(:,iout)=sum(data(:,i:i+lenunf-1),2)
     end do
     datout=cabs(datout)
!     print *,'unfocus finished',burst

     ! range looks
     iout=0
     do i=1,len-lenunf+1,lenunf
        iout=iout+1
        out(iout,:)=sum(cabs(datout(i:i+lenunf-1,:)),1)
     end do
     write(51)out

     deallocate (data, dopest, dopcent, fd, x, cpxdop, datout, out, b, header, fdop, prf)
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
