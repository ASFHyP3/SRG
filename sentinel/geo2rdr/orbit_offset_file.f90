!!!!!
!
! orbit_offset_file - create file of offsets in time and range from orbit solutions
!
!!!!!

program orbit_offset_file
  use sql_mod
  use omp_lib
  implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !! DECLARE LOCAL VARIABLES
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  character*300 str,dbmfile,dbsfile,demfile,demrscfile
  character*300 orbtimingfile,units,type,table
  real*8 norm2
  integer stat
  integer intp_orbit
  integer*4 azimuthBursts,linesPerBurst,samplesPerBurst
  integer*4 firstValidLine(100), lastValidLine(100)

  integer*4 demwidth,demlength
  integer*4 burst,nlines,numstatevec
  integer*8 dbm,dbs
  real*8 deltalat,deltalon,firstlat,firstlon
  real*8 timeorbit(100),xx(3,100),vv(3,100),aa(3,100),x(3),v(3),a(3)
  real*8 timefirst,timeend,slantRangeTime,rangeSamplingRate,prf,startime(100)
  real*8 cornerlat(9),cornerlon(9)
  real*8 extrashift(100)
  real*8, dimension(:),allocatable :: lat
  real*8, dimension(:),allocatable :: lon
  real*8, dimension(:),allocatable :: azshift
  real*8, dimension(:),allocatable :: rgm, rgmslave
  real*8, dimension(:),allocatable :: azt, aztslave
  real*8, dimension(:),allocatable :: rgoff,rgoffslave
  real*8, dimension(:),allocatable :: azoff,azoffslave
  integer*2, allocatable :: demin(:)

!!!!Image limits
  real*8 tstart, tend, tline, tprev
  real*8 rngstart, rngend, rngpix, latlons(4)

!!!! Satellite positions
  real*8, dimension(3) :: xyz_mid, vel_mid, acc_mid
  real*8 :: tmid, rngmid, temp

  real*8 :: llh(3),xyz(3)
  real*8 :: satx(3), satv(3),sata(3)
  real*8 :: dr(3)
  integer :: pixel,line,i

  integer :: i_type,k
  real*8 :: dtaz, dmrg

!!! Duplicated variables for slave computations
  integer*4 :: firstValidLineSlave(100),lastValidLineSlave(100)
  integer*4 :: samplesPerBurstSlave,linesPerBurstSlave,numstatevecSlave
  integer*4 :: azimuthburstsSlave,nrangeslave
  real*8 :: startimeslave(100),slantrangetimeslave,radarfrequencySlave
  real*8 :: rangesamplingrateSlave,prfslave,azimuthtimeintervalslave
  real*8 :: xxslave(3,100),vvslave(3,100),aaslave(3,100),timeorbitslave(100)
  real*8 :: tstartslave,tmidslave,tendslave,rngstartslave,rngmidslave,rngendslave
  real*8 :: dtazslave,dmrgslave
  real*8 :: tlineslave, xyz_midSlave(3),vel_midSlave(3)
  character*300 :: orbtimingfileslave

  real*4 :: timer0, timer1  

  integer*4  nrange

  real*8  azimuthTimeInterval,radarFrequency

  ! declare some constants
  integer LLH_2_XYZ
  real*8 pi,rad2deg,deg2rad,sol 
  real*4 BAD_VALUE

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

  BAD_VALUE = -999999.0
  LLH_2_XYZ=1

  if(iargc().lt.2)then
     print *,'usage: orbit_offset_file master_db slave_db'
     stop
  end if

  call getarg(1,dbmfile)
  call getarg(2,dbsfile)

  !c  open the master database and read some params
  call open_db(dbm,dbmfile)
  table='file'
  call get_parami(dbm,table,'azimuthBursts',azimuthBursts,units,type) ! burst params
  call get_parami(dbm,table,'linesPerBurst',linesPerBurst,units,type)
  call get_parami(dbm,table,'samplesPerBurst',samplesPerBurst,units,type)
  nrange=samplesPerBurst
  call get_paramd(dbm,table,'prf',prf,units,type)
  call get_paramc(dbm,table,'orbinfo',orbtimingfile,units,type) ! orbit state vector file
  do burst=1,azimuthBursts
     if(burst.le.9)then
        call get_paramd(dbm,table,'azimuthTimeSeconds'//char(48+burst),startime(burst),units,type)
        call get_parami(dbm,table,'firstValidLine'//char(48+burst),firstValidLine(burst),units,type)
        call get_parami(dbm,table,'lastValidLine'//char(48+burst),lastValidLine(burst),units,type)
     else
        call get_paramd(dbm,table,'azimuthTimeSeconds'//'1'//char(48+burst-10),startime(burst),units,type)
        call get_parami(dbm,table,'firstValidLine'//'1'//char(48+burst-10),firstValidLine(burst),units,type)
        call get_parami(dbm,table,'lastValidLine'//'1'//char(48+burst-10),lastValidLine(burst),units,type)
     end if
  end do
  call get_paramd(dbm,table,'slantRangeTime',slantRangeTime,units,type)
  call get_paramd(dbm,table,'rangeSamplingRate',rangeSamplingRate,units,type)
  call get_paramd(dbm,table,'azimuthTimeInterval',azimuthTimeInterval,units,type)
!!$  call get_paramc(dbm,table,'orbinfo',orbfile,units,type)
  call close_db(dbm)

  !c  now open the slave database get corresponding params
  call open_db(dbs,dbsfile)
  table='file'
  call get_parami(dbs,table,'azimuthBursts',azimuthBurstsSlave,units,type) ! burst params
  call get_parami(dbs,table,'linesPerBurst',linesPerBurstSlave,units,type)
  call get_parami(dbs,table,'samplesPerBurst',samplesPerBurstSlave,units,type)
  nrangeSlave=samplesPerBurstSlave
  call get_paramd(dbs,table,'prf',prfSlave,units,type)
!!$  call get_paramc(dbs,table,'slc_file',slcinfileSlave,units,type) ! input slc file
  call get_paramc(dbs,table,'orbinfo',orbtimingfileSlave,units,type) ! orbit state vector file
  do burst=1,azimuthBursts
     if(burst.le.9)then
        call get_paramd(dbs,table,'azimuthTimeSeconds'//char(48+burst),startimeSlave(burst),units,type)
        call get_parami(dbs,table,'firstValidLine'//char(48+burst),firstValidLineSlave(burst),units,type)
        call get_parami(dbs,table,'lastValidLine'//char(48+burst),lastValidLineSlave(burst),units,type)
     else
        call get_paramd(dbs,table,'azimuthTimeSeconds'//'1'//char(48+burst-10),startimeSlave(burst),units,type)
        call get_parami(dbs,table,'firstValidLine'//'1'//char(48+burst-10),firstValidLineSlave(burst),units,type)
        call get_parami(dbs,table,'lastValidLine'//'1'//char(48+burst-10),lastValidLineSlave(burst),units,type)
     end if
  end do
  call get_paramd(dbs,table,'slantRangeTime',slantRangeTimeSlave,units,type)
  call get_paramd(dbs,table,'azimuthTimeInterval',azimuthTimeIntervalSlave,units,type)
  call get_paramd(dbs,table,'rangeSamplingRate',rangeSamplingRateSlave,units,type)
  call get_paramd(dbs,table,'radarFrequency',radarFrequencySlave,units,type)
!!$  call get_paramc(dbs,table,'orbinfo',orbfileSlave,units,type)
  call close_db(dbs)

  dtaz = 1.d0 / prf 
  dtazslave = 1.d0 / prfslave
  print *,'Annotation start times and differences'
  do i=1,azimuthbursts
     if(i.eq.1)print *,i,startime(i),startimeslave(i)
     if(i.gt.1)print *,i,startime(i),startimeslave(i),  &
          startime(i)-startime(i-1),  &
          startimeslave(i)-startimeslave(i-1), &
          nint((startime(i)-startime(i-1))/dtaz),  &
          nint((startimeslave(i)-startimeslave(i-1))/dtazslave)
  end do
  !  need to account for the displacement of bursts from even spacing to calculate correct delta-deramp
  print *,'Extra shift for each burst'
  do i=1,azimuthbursts
     extrashift(i)=(startime(i)-startimeslave(i)-startime(1)+startimeslave(1))/dtaz
     print *,i,extrashift(i),nint(extrashift(i))
  end do

  !c  get dem and rsc file names
  open(21,file='params')
  read(21,'(a)')demfile
  read(21,'(a)')demrscfile
  close(21)

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

  ! open dem file
  open(22, file=demfile,access='direct',recl=2*demwidth,form='unformatted')

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
     !print '(f10.2,3f13.2,3f12.5)',timeorbit(i),x,v
  end do
  close(21)

  !c read in the slave orbit state vectors
  open(21,file=orbtimingfileSlave)
  read(21,*)timefirst
  read(21,*)timeend
  read(21,*)nlines
  read(21,*)numstatevecSlave
  !print *,'Number of slave state vectors: ',numstatevec
  !c  read in state vectors
  do i=1,numstatevecSlave
     read(21,*)timeorbitSlave(i),x,v,a
     xxSlave(:,i)=x
     vvSlave(:,i)=v
     aaSlave(:,i)=a
     !print '(f10.2,3f13.2,3f12.5)',timeorbit(i),x,v
     !print *,timeorbit(i)
  end do
  close(21)

  ! get starting time
  timer0 = secnds(0.0)

  ! allocate
  allocate(lat(demlength))
  allocate(lon(demwidth))
  allocate(azshift(demwidth))
  allocate(rgm(demwidth), rgmslave(demwidth))
  allocate(azt(demwidth), aztslave(demwidth))
  allocate(rgoff(demwidth), rgoffslave(demwidth))
  allocate(azoff(demwidth), azoffslave(demwidth))
  allocate(demin(demwidth))

  !c  save offsets in file 'orbitoffsetfile'
  open(41,file='orbitoffsetfile',access='direct',recl=demwidth*8)

  ! master times and ranges
  tstart = startime(1)
  dtaz = 1.d0 / prf ! Nazlooks / prf
  tend  = startime(azimuthBursts) + (linesPerBurst-1)* dtaz
  tmid = 0.5d0*(tstart+tend)

  rngstart = slantRangeTime*sol/2.d0
  dmrg = sol/2.d0/rangeSamplingRate !Nrnglooks * drho
  rngend = rngstart + (samplesPerBurst-1)*dmrg
  rngmid = 0.5d0*(rngstart+rngend)

  ! slave orbit timing
  tstartslave = startimeslave(1)
  dtazslave = 1.d0 / prfslave ! Nazlooks / prf
  tendslave  = startimeslave(azimuthBursts) + (linesPerBurstSlave-1)* dtazslave
  tmidslave = 0.5d0*(tstartslave+tendslave)
  
  rngstartslave = slantRangeTimeSlave*sol/2.d0
  dmrgslave = sol/2.d0/rangeSamplingRateSlave !Nrnglooks * drho
  rngendslave = rngstartslave + (samplesPerBurstSlave-1)*dmrgslave
  rngmidslave = 0.5d0*(rngstartslave+rngendslave)

  ! geolocation of master
  call bounds(tstart,tend,rngstart,rngend,timeorbit,xx,vv,numstatevec,latlons)
  !print *,'bounds of burst: ',latlons


!!!!Initialize satellite positions
  tline = tmid
  stat =  intp_orbit(timeorbit, xx, vv, numstatevec, tline, xyz_mid, vel_mid)
  if (stat.ne.0) then
     print *, 'Cannot interpolate orbits at the center of scene.'
     stop
  endif
!!$     print '(a,f10.2,3f12.2,3f12.4)','Sat midpoint t,x,v:       ',tline,xyz_mid,vel_mid

  tline = tmidslave
  stat =  intp_orbit(timeorbitslave, xxslave, vvslave, numstatevecslave, tline, xyz_midslave, vel_midslave)
  if (stat.ne.0) then
     print *, 'Cannot interpolate orbits at the center of scene.'
     stop
  endif
!!$     print '(a,f10.2,3f12.2,3f12.4)','Slave sat midpoint t,x,v: ',tline,xyz_midslave,vel_midslave

  ! loop over dem lines and get range and time for each
  
  !$OMP PARALLEL DO private(azt,rgm,rgoff,azoff,lat,lon,pixel)&
  !$OMP private(demin,llh,i_type,xyz,tline,rngpix)&
  !$OMP private(rgmslave,aztslave,rgoffslave,azoffslave,azshift)&
  !$OMP shared(demlength,BAD_VALUE,firstlat,firstlon,deltalat,deltalon) &
  !$OMP shared(latlons,deg2rad,LLH_2_XYZ,elp) &
  !$OMP shared(timeorbit,xx,vv,numstatevec,tmid,xyz_mid,vel_mid) &
  !$OMP shared(rngstart,dmrg,rngstartslave,dmrgslave,tstartslave,dtazslave,tstart,dtaz) &
  !$OMP shared(timeorbitslave,xxslave,vvslave,numstatevecslave,tmidslave,xyz_midSlave,vel_midSlave) &
  !$OMP shared(samplesPerBurst,linesPerBurst,samplesPerBurstslave,linesPerBurstslave)

  do i=1,demlength
     if(mod(i,1000).eq.0)print *,i
     lat(i)=firstlat+(i-1)*deltalat
     if(lat(i).ge.latlons(1).and.lat(i).le.latlons(2))then
        !!Read in this line from DEM
        read(22,rec=i)demin
        azt=0.
        aztslave=0.
        !c  pixel locations in the master file
        do pixel=1,demwidth
           lon(pixel)=firstlon+(pixel-1)*deltalon
           if(lon(pixel).ge.latlons(3).and.lon(pixel).le.latlons(4))then
              llh(1) = lat(i) * deg2rad
              llh(2) = lon(pixel) * deg2rad
              llh(3) = demin(pixel)
              i_type = LLH_2_XYZ
              call latlon(elp,xyz,llh,i_type)
              !                 print *,'xyz ',pixel,xyz
              call orbitrangetime(xyz,timeorbit,xx,vv,numstatevec,tmid,xyz_mid,vel_mid,tline,rngpix)
!!$        print *,'Master: ',tline,rngpix
              rgm(pixel) = rngpix
              azt(pixel) = tline
                 
              rgoff(pixel) = ((rngpix - rngstart)/dmrg) !- 1.0d0*(pixel-1)
              azoff(pixel) = ((tline - tstart)/dtaz) !- 1.0d0*(line-1)
                 
              !c   repeat for slave positions
              call orbitrangetime(xyz,timeorbitslave,xxslave,vvslave,numstatevecslave,tmidslave,  &
                   xyz_midSlave,vel_midSlave,tline,rngpix)

              rgmslave(pixel) = rngpix
              aztslave(pixel) = tline
              
              rgoffslave(pixel) = ((rngpix - rngstartslave)/dmrgslave) 
              azoffslave(pixel) = ((tline - tstartslave)/dtazslave) 
              
           end if
        end do ! pixel loop end here

        azshift=(aztslave-azt)-(tstartslave-tstart)+extrashift(burst)*dtaz
        write(41,rec=i)azshift
     end if  ! end check if latitude is in range
  end do  ! end line loop
  !$OMP end parallel do

  close(41)

  deallocate(lat,lon,azshift)
  deallocate(azt,rgm,aztslave,rgmslave)
  deallocate(azoff,rgoff,azoffslave,rgoffslave)

  timer1 = secnds(timer0)
  print *, 'elapsed time = ',timer1,' seconds'
end program orbit_offset_file

integer function intp_orbit(timeorbit, xx, vv, numstatevec, time, xyz_mid, vel_mid)

  implicit none
  integer ilocation, numstatevec
  real*8 timeorbit(*), xx(3,*), vv(3,*), xyz_mid(3), vel_mid(3), time
  ilocation=(time-timeorbit(1))/(timeorbit(2)-timeorbit(1))
  if(ilocation.lt.2)then
     !print *,'ilocation set to 2 ',time,ilocation,timeorbit(1),timeorbit(2)
     ilocation=2
  end if
  if(ilocation.gt.numstatevec-2)then
     !print *,'ilocation set to ',numstatevec-2,time
     ilocation=numstatevec-2
  end if
!!$  ilocation=max(ilocation,2)  ! take care of times falling off the end
!!$  ilocation=min(ilocation,numstatevec-1)
  !print *,i,time,ilocation,xx(1,ilocation-1),vv(1,ilocation-1),timeorbit(ilocation-1)
  xyz_mid=0.
  vel_mid=0.
  call orbithermite(xx(1,ilocation-1),vv(1,ilocation-1),timeorbit(ilocation-1),time,xyz_mid,vel_mid)
  !print *,x,v
  intp_orbit=0
  return 

end function intp_orbit

      subroutine orbitrangetime(xyz,timeorbit,xx,vv,numstatevec,tline0,satx0,satv0,tline,range)

        implicit none

        !c  inputs
        real*8 xyz(3)                          !  point on ground
        real*8 timeorbit(*), xx(3,*), vv(3,*)  !  orbit state vectors
        real*8 tline0, satx0(3),satv0(3)       !  initial search point 
        !c  outputs
        real*8 tline,range                     !  solution for orbit time, range
        !c  internal variables
        real*8 satx(3),satv(3),tprev,dr(3),dopfact,fn,c1,c2,fnprime,BAD_VALUE
        real*8 rngpix
        integer k, stat, intp_orbit, numstatevec

        BAD_VALUE=-999999.99999999d0

        !c  starting state
        tline = tline0
        satx = satx0
        satv = satv0

!!$        print *,'start satx v ',satx,satv

        do k=1,51!51
           tprev = tline  

           dr = xyz - satx
           rngpix = dsqrt(dot_product(dr,dr)) !norm2(dr)    

           dopfact = dot_product(dr,satv)
!!$            fdop = 0.5d0 * wvl * evalPoly1d_f(fdvsrng,rngpix)
!!$            fdopder = 0.5d0 * wvl * evalPoly1d_f(fddotvsrng,rngpix)

           fn = dopfact !- fdop * rngpix

           c1 = -dot_product(satv,satv) !(0.0d0 * dot(sata,dr) - dot(satv,satv))
           c2 = 0. !(fdop/rngpix + fdopder)

           fnprime = c1 + c2*dopfact

           !!            if (abs(fn) .le. 1.0d-5) then
           !!                conv = conv + 1
           !!                exit
           !!            endif

           tline = tline - fn / fnprime

           !!            print *, c1, c2, rngpix

           stat = intp_orbit(timeorbit,xx,vv,numstatevec,tline,satx,satv)

           if (stat.ne.0) then
              tline = BAD_VALUE
              rngpix = BAD_VALUE
              exit
           endif

           !            stat = computeAcceleration_f(orbit,tline,sata)
           !            if (stat.ne.0) then
           !                tline = BAD_VALUE
           !                rngpix = BAD_VALUE
           !                exit
           !            endif

!!!Check for convergence
           if (abs(tline - tprev).lt.5.0d-9) then
              !conv = conv + 1
              exit
           endif
        enddo  ! end iteration loop
        !numiter(pixel)=k

        dr = xyz - satx
        rngpix = dsqrt(dot_product(dr,dr)) !norm2(dr)
        range=rngpix
!!$        print *,'end   satx v ',satx,satv

        return
      end subroutine orbitrangetime
