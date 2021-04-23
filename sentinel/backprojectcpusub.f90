!!!!!
!
!  backprojectsub - subroutine to back project a sentinel burst to a regular grid defined by dem file
!   use gpu for complex integration
!
!   steps:
!    1. Inputs- get metadata, dem info, statevectors, sc position array
!    2. Loop over all bursts for subswath
!    3. Extract burst specific parameters from this bursts database
!    4. Read in range compressed data
!    5. Back project to each point in the dem
!
!!!!!

subroutine backprojectcpusub(rangeprocdata, rangesamples, linestotal, nbursts, lines, slcoutfile)
  
!  use sql_mod
  implicit none

  !  arguments
  complex rangeprocdata(rangesamples,linestotal)
  integer rangesamples, nbursts, linestotal, lines(nbursts)
  character*300 slcoutfile
  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !! DECLARE LOCAL VARIABLES
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  character*300 str,rawinfile,dbfile,demfile,demrscfile,burstfile
  character*300 orbtimingfile,units,type,table,posfile

  integer*1 header(80)
  integer stat,intp_orbit
  integer*4 azimuthBursts,samplesPerBurst
  integer*4 fddem, fdout, initdk
  integer*4 demwidth,demlength,idemwidth,idemlength,width,length
  integer*4 burst,nlines,numstatevec,iaperture,linesmax, ioffset
  integer*8 db
  real*8 pri, range0, samplefreq, starttime
  real*8 ptsperdempt, posting, deltalat,deltalon,firstlat,firstlon
  real*8 timeorbit(1000),xx(3,1000),vv(3,1000),aa(3,1000),x(3),v(3),a(3)
  real*8 timefirst,timeend,slantRangeTime,rangeSamplingRate,prf,startime(1000)
  real*8 satloc(3,100000),satvel(3,100000)
  real*8 :: lat, fd, ht, re, vmag
  real*8 :: azoff, fdcoefs(2)

!!!!Image limits
  real*8 tstart, tend, tline
  real*8 rngstart, rngend, rngpix, latlons(4)

!!!! Satellite positions
  real*8, dimension(3) :: xyz_mid, vel_mid, acc_mid
  real*8 :: tmid, rngmid, temp, t(100000)

  real*8 :: llh(3),xyz(3)
  real*8 :: satx(3), satv(3),sata(3)
  integer :: pixel,line,ith,i

  integer :: i_type, azline, intr,aperture 
  real*8 :: dtaz, dmrg, range,r,fracr,phase
  complex*8 :: val
  complex*16 :: cacc
  complex*8 :: pixelint

  real*4 :: timer0, timer1  
  ! array to hold each burst
  complex*8, allocatable :: burstdata(:,:)
  real*8 :: wvl, r0

  integer*8 filelen
  integer*4  rawdatalines,nbytes
  integer*4  iburst, j, irec
  real*8  azimuthTimeInterval,radarFrequency
  real*8  timecenterseconds,rawdataprf

  ! tops params
  real*8 ks,apertureground,aperturefd,aperturetime,d,bursttimespan
  integer burstpoints, naperture, napertureorig
  real*8 unitlookvector(3),td,fdc, frate, angc0, angc1

  ! declare some constants
  integer LLH_2_XYZ
  real*8 pi,rad2deg,deg2rad,sol 
  real*4 BAD_VALUE
  parameter(BAD_VALUE = -999999.0)
  parameter(LLH_2_XYZ=1)

  !c  types needed

  type :: ellipsoid 
     real*8 r_a           ! semi-major axis
     real*8 r_e2          ! eccentricity of earth ellisoid
  end type ellipsoid
  type(ellipsoid) :: elp

  elp%r_a=6378137.0
  elp%r_e2=0.0066943799901499996

  pi = 4.d0*atan2(1.d0,1.d0)
  sol = 299792458.d0
  rad2deg = 180.d0/pi
  deg2rad = pi/180.d0

  !c  get dem and rsc file names
  open(21,file='params')
  read(21,'(a)')demfile
  read(21,'(a)')demrscfile
  close(21)
!!$  print *,'dem file: ',demfile
!!$  print *,'demrscfile: ',demrscfile

  !c  read in the dem and its resource parameters
  open(21,file=demrscfile)
  read(21,'(a)')str
  read(str(15:60),*)demwidth
  read(21,'(a)')str
  read(str(15:60),*)demlength
  read(21,'(a)')str
  read(str(15:60),*)firstlon
  read(21,'(a)')str
  read(str(15:60),*)firstlat
  read(21,'(a)')str
  read(str(15:60),*)deltalon
  read(21,'(a)')str
  read(str(15:60),*)deltalat
  close(21)

  !c  open the dem file
  fddem=initdk(31,demfile)

  !c  loop over all bursts in this subswath
  linesmax=linestotal/nbursts
  !print *,'linesmax ',linesmax, nbursts
  
  do iburst=1,nbursts
     call burstparams(rangeprocdata(1,1+linesmax*(iburst-1)), starttime, prf, samplefreq, pri, range0, wvl)
     !print *,'burstparams ',starttime,prf,samplefreq,pri,range0,wvl
     slantRangeTime=2*range0/sol
     if(iburst.le.9)posfile='positionburst'//char(iburst+48)//'.out'
     if(iburst.ge.10)posfile='positionburst1'//char(iburst-10+48)//'.out'
!     print *,'accessing positionfile ',posfile,lines
     re=6378137.d0
     ht=700000.d0
     rawdatalines=lines(iburst)
!     print *,'raw data lines ', rawdatalines

  !c read in the orbit state vectors
     
  open(21,file='precise_orbtiming') !orbtimingfile)
  read(21,*)timefirst
  read(21,*)timeend
  read(21,*)nlines
  read(21,*)numstatevec
  !print *,'Number of state vectors: ',numstatevec

  !c  read in state vectors
  do i=1,numstatevec
     read(21,*)timeorbit(i),x,v,a
     xx(:,i)=x
     vv(:,i)=v
     aa(:,i)=a
     !print *,timeorbit(i)
  end do
  close(21)

  !c  read in position file
!  print *,'position file ',posfile
  open(21,file=posfile)
  do i=1,rawdatalines
     read(21,*,end=102)line,t(i),satloc(:,i),satvel(:,i)
  end do
102  close(21)

  ! get starting time
  timer0 = secnds(0.0)

  ! estimate doppler centroid
  call fdopcoefs(rangeprocdata(1,1+linesmax*(iburst-1)),rangesamples,rawdatalines,fdcoefs)
  angc0=fdcoefs(1)
  angc1=fdcoefs(2)
!  print *,'fd coefs ',angc0, angc1
  
  rangeprocdata(1:10,1+linesmax*(iburst-1):linesmax*iburst)=cmplx(0.,0.)
  
  !c  set the size of the aperture and its centroid (10 m antenna)
  v=satvel(:,rawdatalines/2)
  vmag=dsqrt(dot_product(v,v))
  vmag=vmag*(re/(re+ht))
  dmrg = sol/2.d0/rangeSamplingRate 
  temp=fd*prf*prf*wvl*(range0+rangesamples/2.*dmrg)/2.d0/vmag**2
  iaperture=temp
  !print *,'aperture pts, offset: ',aperture,iaperture,temp

  !  tops geometry params
  d=10.
  apertureground=range0*wvl/d
  bursttimespan=rawdatalines/prf

  ! refined numbers for tops using both ks and velocity
  aperturetime=(range0*wvl/2/vmag/vmag*prf)/(1.+range0*angc1*pi/180./vmag)
  burstpoints=aperturetime*prf*0.5  !  50% of nominal antenna aperture
  aperture=burstpoints

!c open the output file
  fdout=initdk(30,slcoutfile)

  !c  azimuth and range limits
  tstart = starttime
  dtaz = 1.d0 / prf 
  tend  = tstart + (rawdatalines-1)* dtaz
  tmid = 0.5d0*(tstart+tend)
  
  !print *, 'Start, stop Acquisition time: ', tstart,tend

  rngstart = slantRangeTime*sol/2.d0
  dmrg = sol/2.d0/samplefreq
  rngend = rngstart + (rangesamples-1)*dmrg
  rngmid = 0.5d0*(rngstart+rngend)

  ! geolocation of the current burst entended for TOPS scan
  call bounds(tstart-2.,tend+2.,rngstart,rngend,timeorbit,xx,vv,numstatevec,latlons)

!!!!Initialize satellite position
  tline = tmid
  stat =  intp_orbit(timeorbit, xx, vv, numstatevec, tline, xyz_mid, vel_mid)

  if (stat.ne.0) then
     print *, 'Cannot interpolate orbits at the center of scene.'
     stop
  endif

  ! azimuth compress
  ioffset = linesmax * (iburst-1) +1
  call azimuth_compress_cpu(rangeprocdata(1,ioffset),satloc,rawdatalines,rangesamples,demwidth,demlength,fdout,fddem, &
       deltalat,deltalon,firstlat,firstlon, &
       latlons,timeorbit,xx,vv,numstatevec,rngstart,rngend,tstart,tend, &
       tmid,xyz_mid,vel_mid,t,dtaz,dmrg,wvl,aperture,iaperture,angc0,angc1,prf)

end do  ! end bursts loop

return
end subroutine backprojectcpusub

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
        elevationbeam=iand(ishft(word,-12),15)!        elevationbeam=iand(ishft(word,-12),'000F'Z)
        azimuthbeam=iand(word,1023)!        azimuthbeam=iand(word,'03FF'Z)
        ang(i)=azimuthbeam/1024.*1.8-0.9
        prf(i)=fref/in3(b(1+50,i))*1.e6
        t(i)=(i-lines/2.)/prf(i)
!        write(31,*)i,azimuthbeam,ang(i),t(i)
     end do
!     close(31)

!  solve for angle as function of time
     call linfit(t(21),ang(21),lines-40,angc0,angc1)
     !print *,'Angle coeffs: ',angc0,angc1
     !print *,angc0,angc1
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

! get some parameters from a burst needed for processing
subroutine burstparams(rawdata, starttime, prf, samplefreq, pri, range0, wvl)

  integer*1 rawdata(80)
  real*8 starttime, prf, samplefreq, pri, range0
  real*8 fref, c, wvl
  real*8 swst
  real*8 d8
  real*8 samplefrequency
  integer irank, rangedecimation
  
  fref=37.53472224d0
  c=299792458.d0
  wvl=0.05546576
  starttime = d8(rawdata(1+68))
  prf = fref/in3(rawdata(1+50))*1.e6
  rangedecimation = iand(int(rawdata(1+40)), 255)!  rangedecimation = iand(rawdata(1+40), 'FF'Z)
  samplefreq = samplefrequency(rangedecimation)*1.e6
  swst = in3(rawdata(1+53))/fref*1.e-6;
  irank=iand(rawdata(1+49), '1F'Z)
  pri = in3(rawdata(1+50))/fref*1.e-6;
  range0=c/2.*(irank*pri+swst);

  return
end subroutine burstparams

real*8 function d8(data)
  integer*1 data(*)
  integer*1 b(8)
  real*8 d
  equivalence (b,d)

  b=data(1:8)
  d8=d
  !print *,b,d
  return
  end

