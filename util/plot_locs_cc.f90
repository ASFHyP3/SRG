!c  plot reference locations on top of average cc file

  complex*8, allocatable :: igram(:,:), igramave(:,:), amps(:,:), ampsave(:,:)
  real*4, allocatable :: cc(:,:), ccave(:,:), cctemp(:,:), unw(:,:), unwave(:,:)
  real*4, allocatable :: sum(:,:), sumsq(:,:), std(:,:), snr(:,:), rho(:,:)
  integer*4 loc(2,1000000)
  character*300 str,intfiles,ccfile(100000),locationfile,replace
  integer*8 filelen,lines
  real r(360), g(360), b(360)
  integer*1, allocatable :: rgb(:,:,:)

  if(iargc().lt.3)then
     print *,'Usage: plot_locs_cc intlist len ref_locs'
     stop
  end if

  call getarg(1,intfiles)
  call getarg(2,str)
  read(str,*)len
  call getarg(3,locationfile)

!c  get the locations
  open(21,file=locationfile)
  do i=1,1000000
     read(21,*,end=99)loc(1,i),loc(2,i)
  end do
99 nlocs=i-1
  print *,'Locations found: ',nlocs
  close(21)

!c  get list of cc files from intlist
  open(21, file=intfiles)
  do i=1,100000
     read(21,'(a)',end=98)str
     k=index(trim(str),'.int')
     ccfile(i)=str(1:k-1)//'.cc'
!     print *,i,str,k,ccfile(i)
  end do
98 nfiles=i-1
  print *,'cc files found: ',nfiles

  lines=filelen(ccfile(1))/len/8
  print *,'ccfile dimensions: ',len,lines
  allocate (cc(2*len,lines),ccave(2*len,lines))

!c  read in the cc files
  do i=1,nfiles
     open(21,file=ccfile(i),access='direct',recl=len*8*lines)
     read(21,rec=1)cc
     ccave=ccave+cc/nfiles
     close(21)
  end do

!c  create a tiff image of the average correlation
!c  allocate rgb array
      allocate (rgb(len,lines,4))

!c  set up color tables
      do i=1,120
         r(i) = float(i) * 2.13 * 155./255. + 100 
         g(i) = float (120 -i) * 2.13 * 155./255. + 100
         b(i) = 255
      end do
      do i=121,240
         ival = i - 121
         r(i)= 255. 
         g(i) = float (ival) *  2.13 * 155./255. + 100. 
         b(i) = float (239 - i) * 2.13 * 155./ 255. + 100. 
      end do
      do i=241,360
         ival = i - 241.
         r(i) = float (359 - i)  * 2.13 * 155. / 255. + 100.
         g(i) = 255. ;
         b(i) = float (ival) * 2.13 * 155. / 255. + 100.
      end do

!c  scale for amplitude
      sumamp=0.
      isum=0
      do i=16,lines-16,32
         do j=16,len-16,32
            isum=isum+1
            sumamp=sumamp+abs(ccave(j,i))**0.3
         end do
      end do
      scalemag = 150/(sumamp/isum)

!c  now process each line
      do i=1,lines !ifirst,ifirst+lines-1
         do j=1,len
            amp=abs(ccave(j,i))**0.3 * scalemag/255.
            if(amp.gt.0.999)amp=0.999
            phase=mod(ccave(j+len,i),1.2)*360./1.2
            iphase=mod(nint(phase),360)
            if(iphase.lt.1)then
               iphase=iphase+360
            end if
            if(iphase.gt.360)then
               iphase=iphase-360
            end if
            rgb(j,i,1)=r(iphase)*amp
            rgb(j,i,2)=g(iphase)*amp
            rgb(j,i,3)=b(iphase)*amp
            rgb(j,i,4)=255
            if(amp.le.1e-10)rgb(j,i,4)=0
         end do
      end do
!c  make locations red
      do i=1,nlocs
         rgb(loc(1,i)-1:loc(1,i)+1,loc(2,i)-1:loc(2,i)+1,1)=0
         rgb(loc(1,i)-1:loc(1,i)+1,loc(2,i)-1:loc(2,i)+1,2)=255
         rgb(loc(1,i)-1:loc(1,i)+1,loc(2,i)-1:loc(2,i)+1,3)=0
         rgb(loc(1,i)-1:loc(1,i)+1,loc(2,i)-1:loc(2,i)+1,4)=255
      end do
!c  save as a tiff
      str='plot_locs_cc.tif'
      call writetiff(rgb,len,lines,trim(str))

      allocate(igram(len,lines),igramave(len,lines),amps(len,lines),ampsave(len,lines))
      allocate(cctemp(len,lines), unw(len*2,lines), unwave(len*2,lines))
      allocate(sum(len,lines), sumsq(len,lines), std(len,lines), snr(len,lines), rho(len,lines))

!c  read in the amp, unw files
      igramave=cmplx(0.,0.)
      unwave=cmplx(0.,0.)
      ampsave=cmplx(0.,0.)
      sum=0.
      sumsq=0.
      do i=1,nfiles
         str=replace(ccfile(i),'cc','amp')
         open(21,file=str,access='direct',recl=len*8*lines)
         read(21,rec=1)amps
         close(21)
         str=replace(ccfile(i),'cc','unw')
         open(21,file=str,access='direct',recl=len*8*lines)
         read(21,rec=1)unw
         close(21)
         igramave=igramave+real(amps)*aimag(amps)*cmplx(cos(unw(len+1:2*len,:)),sin(unw(len+1:2*len,:)))/nfiles
         ampsave=ampsave+cmplx(real(amps)**2,aimag(amps)**2)/nfiles
         sum=sum+unw(len+1:2*len,:)/nfiles
         sumsq=sumsq+unw(len+1:2*len,:)**2/nfiles
      end do
      std=sqrt(sumsq-sum**2)  ! unwrapped phase standard deviation
      snr=2/std**2*(1+sqrt(1+4*std**2))
      rho=1/(1+1/snr)
! debugfiles
      open(31,file='igramtemp',access='stream')
      write(31)igramave
      close(31)
      open(31,file='amptemp',access='stream')
      write(31)ampsave
      close(31)
      ampsave=cmplx(sqrt(real(ampsave)),sqrt(aimag(ampsave)))
      cctemp=cabs(igramave)/(real(ampsave)*aimag(ampsave))
      ccave(1:len,:)=cabs(ampsave)
      ccave(len+1:2*len,:)=cctemp
      open(31,file='plot_locs_cc.cc',access='direct',recl=len*8)
      do i=1,lines
         write(31,rec=i)ccave(1:len,i),rho(:,i)
      end do
!      write(31,rec=1)ccave
      close(31)

  end

! ------------------
FUNCTION replace (s,text,rep)  RESULT(outs)
CHARACTER(*)        :: s,text,rep
CHARACTER(LEN(s)+100) :: outs     ! provide outs with extra 100 char len
INTEGER             :: i, nt, nr

outs = s ; nt = LEN_TRIM(text) ; nr = LEN_TRIM(rep)
DO
   i = INDEX(outs,text(:nt)) ; IF (i == 0) EXIT
   outs = outs(:i-1) // rep(:nr) // outs(i+nt:)
END DO
END FUNCTION replace
