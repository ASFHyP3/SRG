!!!!!
!
!  get_latlon_bounds - find a latlon bounding box for a sentinel burst file
!     for zero Doppler slc geometry only
!
!!!!!

program get_latlon_bounds
  use sql_mod
  use omp_lib
  implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !! DECLARE LOCAL VARIABLES
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  character*300 str,slcinfile,slcoutfile,dbfile,demfile,demrscfile
  character*300 orbtimingfile,units,type,table,fardbfile
  real*8 norm2
  integer stat,cnt,intp_orbit
  integer*8 latAcc,lonAcc,hgtAcc
  integer*8 azAcc,rgAcc
  integer*8 azOffAcc,rgOffAcc
  integer*4 azimuthBursts,linesPerBurst,samplesPerBurst,farsamplesPerBurst

  integer*4 demwidth,demlength,idemwidth,idemlength,width,length
  integer*4 burst,nlines,numstatevec,nrange,nswaths
  integer*8 db
  real*8 ptsperdempt, posting, deltalat,deltalon,firstlat,firstlon
  real*8 timeorbit(100),xx(3,100),vv(3,100),aa(3,100),x(3),v(3),a(3)
  real*8 slantRangeTime,rangeSamplingRate,prf,startime(100),azimuthTimeInterval
  real*8 farslantRangeTime,farrangeSamplingRate
  real*8 zminlat,zmaxlat,zminlon,zmaxlon,timeend,timefirst

!!!!Image limits
  real*8 tstart, tend, tline, tprev
  real*8 rngstart, rngend, rngpix, latlons(4)

!!!! Satellite positions
  real*8, dimension(3) :: xyz_mid, vel_mid, acc_mid
  real*8 :: tmid, rngmid, temp

  real*8 :: llh(3),xyz(3)
  real*8 :: satx(3), satv(3),sata(3)
  real*8 :: dr(3)
  integer :: pixel,line,ith,i

  integer :: i_type,k,conv
  real*8 :: dtaz, dmrg
  real*8 :: dopfact,fdop,fdopder
  real*8 :: fn, fnprime
  real*8 :: c1,c2

  integer :: numOutsideImage

  real*4 :: timer0, timer1  

  ! declare some constants
  integer LLH_2_XYZ
  real*8 pi,rad2deg,deg2rad,sol 

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
     print *,'usage: get_latlon_coords file_db <number of swaths = 1>'
     stop
  end if

  call getarg(1,dbfile)
  nswaths=1
  if(iargc().ge.2)then
     call getarg(2,str)
     read(str,*)nswaths
  end if

  !c  open the database
  call open_db(db,dbfile)
  table='file'
  call get_parami(db,table,'azimuthBursts',azimuthBursts,units,type) ! burst params
  call get_parami(db,table,'linesPerBurst',linesPerBurst,units,type)
  call get_parami(db,table,'samplesPerBurst',samplesPerBurst,units,type)
  nrange=samplesPerBurst
  call get_paramc(db,table,'orbinfo',orbtimingfile,units,type) ! orbit state vector file
  do burst=1,azimuthBursts
     if(burst.le.9)then
        call get_paramd(db,table,'azimuthTimeSeconds'//char(48+burst),startime(burst),units,type)
     else
        call get_paramd(db,table,'azimuthTimeSeconds'//'1'//char(48+burst-10),startime(burst),units,type)
     end if
  end do
  call get_paramd(db,table,'slantRangeTime',slantRangeTime,units,type)
  call get_paramd(db,table,'rangeSamplingRate',rangeSamplingRate,units,type)
  call get_paramd(db,table,'azimuthTimeInterval',azimuthTimeInterval,units,type)
  !call get_paramd(db,table,'rawdataprf',rawdataprf,units,type)
  call get_paramd(db,table,'rangeSamplingRate',rangeSamplingRate,units,type)
  call get_paramd(db,table,'slantRangeTime',slantRangeTime,units,type)
  call close_db(db)

  !c  if more than one swath get the ending time
  if(nswaths.gt.1)then
     str= dbfile(1:index(dbfile,'db.1')+2)//char(nswaths+48)
     print *,trim(str)
     call open_db(db,trim(str))
     table='file'
     call get_parami(db,table,'samplesPerBurst',farsamplesPerBurst,units,type)
     call get_paramd(db,table,'slantRangeTime',farslantRangeTime,units,type)
     call get_paramd(db,table,'rangeSamplingRate',farrangeSamplingRate,units,type)
     call close_db(db)
  end if
  
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

  ! get starting time
  timer0 = secnds(0.0)
  cnt = 0

  !c loop over bursts
  zminlat=1.e20
  zmaxlat=-1.e20
  zminlon=1.e20
  zmaxlon=-1.e20
  do burst=1,azimuthBursts
     tstart = startime(burst)
     dtaz = 1.d0 / prf ! Nazlooks / prf
     tend  = tstart + (linesPerBurst-1)* azimuthTimeInterval
     tmid = 0.5d0*(tstart+tend)

     print *, 'Burst ',burst,', Start, stop Acquisition time: ', tstart,tend

     rngstart = slantRangeTime*sol/2.d0
     dmrg = sol/2.d0/rangeSamplingRate
     rngend = rngstart + (samplesPerBurst-1)*dmrg
     if(nswaths.gt.1)then
        print *,farslantRangeTime,farsamplesPerBurst,farrangeSamplingRate
        rngend = farslantRangeTime*sol/2.d0+(farsamplesPerBurst-1)*sol/2.d0/farrangeSamplingRate
     end if
     rngmid = 0.5d0*(rngstart+rngend)
     print *,rngstart,rngend
!!$     print *, 'Near Range in m: ', rngstart 
!!$     print *, 'Far  Range in m: ', rngend
!!$     print *, 'Range sample spacing in m: ', dmrg

     length=linesPerBurst
     width=samplesPerBurst
!!$     print *, 'Radar Image Burst Lines: ', length
!!$     print *, 'Radar Image Burst Width: ', width

     !print *,tstart,tend,rngstart,rngend

     call bounds(tstart,tend,rngstart,rngend,timeorbit,xx,vv,numstatevec,latlons)

!!$     print *,'Min, max lat: ',latlons(1),latlons(2)
!!$     print *,'Min, max lon: ',latlons(3),latlons(4)

     zminlat=min(zminlat,latlons(1))
     zmaxlat=max(zmaxlat,latlons(2))
     zminlon=min(zminlon,latlons(3))
     zmaxlon=max(zmaxlon,latlons(4))
     
  enddo  !  end burst loop

  open(21,file='latloncoords')
  write(21,*)zminlat
  write(21,*)zminlon
  write(21,*)zmaxlat
  write(21,*)zmaxlon
  close(21)

end

