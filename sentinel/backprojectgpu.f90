!!!!!
!
!  backproject - back project a sentinel burst to a regular grid defined by dem file
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

program backprojectgpu
  use sql_mod
  implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !! DECLARE LOCAL VARIABLES
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  character*300 str,rawinfile,slcoutfile,dbfile,demfile,demrscfile
  character*300 orbtimingfile,units,type,table,posfile

  integer stat,intp_orbit
  integer*4 azimuthBursts,samplesPerBurst
  integer*4 fddem, fdout, initdk
  integer*4 demwidth,demlength,idemwidth,idemlength,width,length
  integer*4 burst,nlines,numstatevec,iaperture,nbursts
  integer*8 db
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

  if(iargc().lt.4)then
     print *,'usage: backproject slcoutfile file_db[1...n]'
     stop
  end if

  call getarg(1,slcoutfile)
  call getarg(2,str)
  nbursts=iargc()-1
  !print *,'Bursts in swath: ',nbursts

  !c  get dem and rsc file names
  open(21,file='params')
  read(21,'(a)')demfile
  read(21,'(a)')demrscfile
  close(21)
  !print *,'dem file: ',demfile
  !print *,'demrscfile: ',demrscfile

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
  do iburst=1,nbursts

  call getarg(1+iburst,dbfile)

  !c  open the database
  call open_db(db,dbfile)
  table='file'
  !call get_parami(db,table,'number_of_az_lines',linesPerBurst,units,type)
  call get_parami(db,table,'number_of_range_bins',samplesPerBurst,units,type)
  call get_paramd(db,table,'prf',prf,units,type)
  call get_paramd(db,table,'wvl',wvl,units,type)
  call get_paramd(db,table,'re',re,units,type)
  call get_paramd(db,table,'ht',ht,units,type)
  call get_paramd(db,table,'fd',fd,units,type)
  call get_paramc(db,table,'raw_data_file',rawinfile,units,type) ! input slc file
  call get_parami(db,table,'bytes_per_line',nbytes,units,type)
  rawdatalines=filelen(rawinfile)/nbytes
  call get_paramc(db,table,'orbinfo',orbtimingfile,units,type) ! orbit state vector file
  call get_paramc(db,table,'posfile',posfile,units,type) ! orbit position file
  call get_paramd(db,table,'azimuthTimeSecondsRawData',startime(1),units,type)

  call get_paramd(db,table,'r0',r0,units,type)
  slantRangeTime=2*r0/sol
  call get_paramd(db,table,'fs',rangeSamplingRate,units,type)
  call get_paramd(db,table,'azimuthTimeInterval',azimuthTimeInterval,units,type)
  call close_db(db)

  !c read in the orbit state vectors
  open(21,file=orbtimingfile)
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

  ! allocate input data for burst
  allocate(burstdata(samplesPerBurst,rawdatalines))

  !  read in rc data for burst
  open(23,file=rawinfile,access='direct',recl=rawdatalines*samplesPerBurst*8)
  read(23,rec=1)burstdata
  close(23)

  ! estimate doppler centroid
  call fdopcoefs(burstdata,samplesPerBurst,rawdatalines,fdcoefs)
  angc0=fdcoefs(1)
  angc1=fdcoefs(2)

  burstdata(1:10,:)=cmplx(0.,0.)

  !c  set the size of the aperture and its centroid (10 m antenna)
  v=satvel(:,rawdatalines/2)
  vmag=dsqrt(dot_product(v,v))
  vmag=vmag*(re/(re+ht))
  dmrg = sol/2.d0/rangeSamplingRate 
  temp=fd*prf*prf*wvl*(r0+samplesPerBurst/2.*dmrg)/2.d0/vmag**2
  iaperture=temp
  !print *,'aperture pts, offset: ',aperture,iaperture,temp

  !  tops geometry params
  d=10.
  apertureground=r0*wvl/d
  bursttimespan=rawdatalines/prf

  ! refined numbers for tops using both ks and velocity
  aperturetime=(r0*wvl/2/vmag/vmag*prf)/(1.+r0*angc1*pi/180./vmag)
  burstpoints=aperturetime*prf*0.5  !  50% of nominal antenna aperture
  aperture=burstpoints
  !print *,'vmag prf angc1 r0 wvl ',vmag,prf,angc1,r0,wvl
  print *,'Burst time span, points : ',bursttimespan,burstpoints

!c open the output file
  fdout=initdk(30,slcoutfile)

  !c  azimuth and range limits
  tstart = startime(1)
  dtaz = 1.d0 / prf 
  tend  = tstart + (rawdatalines-1)* dtaz
  tmid = 0.5d0*(tstart+tend)
  
  !print *, 'Start, stop Acquisition time: ', tstart,tend

  rngstart = slantRangeTime*sol/2.d0
  dmrg = sol/2.d0/rangeSamplingRate 
  rngend = rngstart + (samplesPerBurst-1)*dmrg
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
  call azimuth_compress(burstdata,satloc,rawdatalines,samplesPerBurst,demwidth,demlength,fdout,fddem, &
       deltalat,deltalon,firstlat,firstlon, &
       latlons,timeorbit,xx,vv,numstatevec,rngstart,rngend,tstart,tend, &
       tmid,xyz_mid,vel_mid,t,dtaz,dmrg,wvl,aperture,iaperture,angc0,angc1,prf)

  deallocate(burstdata)


end do  ! end bursts loop

end program backprojectgpu


