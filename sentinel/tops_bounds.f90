!!!!!
!
!  tops_bounds - get bounds for sentinel tops image by burst
!
!
!!!!!

program tops_bounds
  use sql_mod
  use omp_lib
  implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !! DECLARE LOCAL VARIABLES
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  character*300 str,rawinfile,slcoutfile,dbfile,demfile,demrscfile
  character*300 orbtimingfile,units,type,table,posfile

  integer stat,intp_orbit
  integer*4 azimuthBursts,samplesPerBurst

  integer*4 demwidth,demlength,idemwidth,idemlength,width,length
  integer*4 burst,nlines,numstatevec,iaperture
  integer*8 db
  real*8 ptsperdempt, posting, deltalat,deltalon,firstlat,firstlon
  real*8 timeorbit(1000),xx(3,1000),vv(3,1000),aa(3,1000),x(3),v(3),a(3)
  real*8 timefirst,timeend,slantRangeTime,rangeSamplingRate,prf,startime(1000)
  real*8 satloc(3,100000),satvel(3,100000)
  real*8 :: lat, fd, ht, re, vmag
  real*8, dimension(:),allocatable :: lon
  real*8 :: azoff
  integer*2, allocatable :: demin(:)

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

  real*4 :: timer0, timer1  
  ! array to hold each burst
  complex*8, allocatable :: burstdata(:,:)
  complex*8, allocatable :: outdata(:), baddata(:), testdata(:), origdata(:)
  real*8, allocatable :: testphase(:)
  real*8 :: wvl, r0

  integer*8 filelen
  integer*4  rawdatalines,nbytes
  integer*4  iburst, j, irec
  real*8  azimuthTimeInterval,radarFrequency
  real*8  timecenterseconds,rawdataprf

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

  if(iargc().lt.1)then
     print *,'usage: tops_bounds orbtimingfile '!slcoutfile '
     stop
  end if

  call getarg(1,orbtimingfile)
!  call getarg(2,slcoutfile)

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

  open(21,file='latlonlimits')
! loop over bursts
  do burst = 1,11
     if(burst.le.9)str='db.burst'//char(burst+48)
     if(burst.ge.10)str='db.burst'//char(1+48)//char(burst-10+48)
     dbfile=trim(str)
     !print *,trim(dbfile)
     !c  open the database
!     print *,'opening ',trim(dbfile)
     call open_db(db,trim(dbfile))
     table='file'
     !call get_parami(db,table,'number_of_az_lines',linesPerBurst,units,type)
     call get_parami(db,table,'number_of_range_bins',samplesPerBurst,units,type)
!     print *,'range bins ',samplesPerBurst
     call get_paramd(db,table,'prf',prf,units,type)
     call get_paramd(db,table,'wvl',wvl,units,type)
     call get_paramd(db,table,'re',re,units,type)
     call get_paramd(db,table,'ht',ht,units,type)
     call get_paramd(db,table,'fd',fd,units,type)
     call get_paramc(db,table,'raw_data_file',rawinfile,units,type) ! input slc file
     call get_parami(db,table,'bytes_per_line',nbytes,units,type)
     rawdatalines=filelen(rawinfile)/nbytes
     !call get_paramc(db,table,'orbinfo',orbtimingfile,units,type) ! orbit state vector file
     call get_paramc(db,table,'posfile',posfile,units,type) ! orbit position file
     call get_paramd(db,table,'azimuthTimeSecondsRawData',startime(burst),units,type)
     call get_paramd(db,table,'r0',r0,units,type)
     slantRangeTime=2*r0/sol
     call get_paramd(db,table,'fs',rangeSamplingRate,units,type)
     call get_paramd(db,table,'azimuthTimeInterval',azimuthTimeInterval,units,type)
     call close_db(db)
     tstart=startime(burst)
     tend=tstart+4*rawdatalines/prf ! add extra for swath swapping
     rngstart=r0
     rngend=r0+samplesPerBurst/rangeSamplingRate*sol/2.
     !print *,tstart,tend,rngstart,rngend,numstatevec
     call bounds(tstart,tend,rngstart,rngend,timeorbit,xx,vv,numstatevec,latlons)
     write(21,*)burst,latlons
     !print *,startime(burst),prf,wvl,db
  end do
!  print *,rangeSamplingRate,samplesPerBurst,samplesPerBurst/rangeSamplingRate*sol/2.

  ! geolocation of the current burst
!!$  do burst=2,10
!!$     tstart=startime(burst-1)
!!$     tend=startime(burst+1)
!!$     rngstart=r0
!!$     rngend=r0+samplesPerBurst/rangeSamplingRate*sol/2.
!!$     !print *,tstart,tend,rngstart,rngend,numstatevec
!!$     call bounds(tstart,tend,rngstart,rngend,timeorbit,xx,vv,numstatevec,latlons)
!!$     if(burst.eq.2)write(21,*)1,latlons
!!$     write(21,*)burst,latlons
!!$     if(burst.eq.10)write(21,*)11,latlons
!!$  end do
  close(21)
!!$  print *,'dbfiles read and bounds computed'
!!$
!!$  print *, 'elapsed time = ',timer1,' seconds'
end program tops_bounds

