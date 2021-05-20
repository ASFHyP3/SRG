!!!!!
!
!  overlap phase - create overlapped sections between bursts in sentinel data
!     for zero Doppler slc geometry only
!
!!!!!

program overlap_phase
  use sql_mod
  use omp_lib
  implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !! DECLARE LOCAL VARIABLES
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  character*300 str,slcinfile,dbmfile,dbsfile,demfile,demrscfile
  character*300 orbtimingfile,units,type,table
  real*8 norm2
  integer stat,intp_orbit,iphase
  integer*4 azimuthBursts,linesPerBurst,samplesPerBurst,overlap
  integer*4 firstValidLine(100), lastValidLine(100)

  integer*4 demwidth,demlength
  integer*4 burst,nlines,numstatevec
  integer*8 dbm,dbs
  real*8 deltalat,deltalon,firstlat,firstlon
  real*8 timeorbit(100),xx(3,100),vv(3,100),aa(3,100),x(3),v(3),a(3)
  real*8 timefirst,timeend,slantRangeTime,rangeSamplingRate,prf,startime(100)
  real*8 :: lat
  real*8, dimension(:),allocatable :: lon
  real*8, dimension(:),allocatable :: rgm, rgmslave
  real*8, dimension(:),allocatable :: azt, aztslave
  real*8, dimension(:),allocatable :: rgoff,rgoffslave
  real*8, dimension(:),allocatable :: azoff,azoffslave
  integer*2, allocatable :: demin(:)
  integer*4, allocatable :: hist(:,:)

!!!!Image limits
  real*8 tstart, tend, tline, tprev, tstartoverlap, tendoverlap, tstart2, tend2
  real*8 rngstart, rngend, rngpix, latlons(4)

!!!! Satellite positions
  real*8, dimension(3) :: xyz_mid, vel_mid, acc_mid
  real*8 :: tmid, rngmid

  real*8 :: llh(3),xyz(3)
  real*8 :: satx(3), satv(3),sata(3)
  real*8 :: dr(3)
  integer :: pixel,line,ith,i

  integer :: i_type,k
  real*8 :: dtaz, dmrg

!!! Duplicated variables for slave computations
  integer*4 :: firstValidLineSlave(100),lastValidLineSlave(100)
  integer*4 :: samplesPerBurstSlave,linesPerBurstSlave,numstatevecSlave
  integer*4 :: azimuthburstsSlave
  real*8 :: startimeslave(100),wvlslave,slantrangetimeslave,radarfrequencySlave
  real*8 :: rangesamplingrateSlave,prfslave,azimuthtimeintervalslave
  real*8 :: xxslave(3,100),vvslave(3,100),aaslave(3,100),timeorbitslave(100)
  real*8 :: tstartslave,tmidslave,tendslave,rngstartslave,rngmidslave,rngendslave
  real*8 :: tstartslave2, tendslave2
  real*8 :: dtazslave,dmrgslave
  real*8 :: tlineslave, xyz_midSlave(3),vel_midSlave(3)
  character*300 :: slcinfileSlave
  character*300 :: orbfileslave,orbtimingfileslave

  integer :: latline1, latline2

  real*4 :: timer0, timer1  
  ! array to hold each burst
  complex*8, allocatable :: burstdata(:,:),burstdataslave(:,:), baddata(:)
  complex*8, allocatable :: outdata(:),outdataslave(:)
  complex*8, allocatable :: burstdata2(:,:),burstdataslave2(:,:),outdata2(:),outdataslave2(:),outdata3(:)
  real*8 :: phase, wvl

  ! variables associated with offset carrier removal
  character*300 fmratefile, dcfile, orbfile, offsetfile
  integer*4  iburst, j, irec
  real*8  azimuthSteeringRate,azimuthTimeInterval,radarFrequency
  real*8  timecenterseconds,rawdataprf

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
     print *,'usage: overlap_phase master_db slave_db ' !overlap-pixels slcoutfile'
     stop
  end if

  call getarg(1,dbmfile)
  call getarg(2,dbsfile)

  !c  open the master database and read some params
  call open_db(dbm,dbmfile)
  table='file'
  !print *,trim(table)
  call get_parami(dbm,table,'azimuthBursts',azimuthBursts,units,type) ! burst params
  call get_parami(dbm,table,'linesPerBurst',linesPerBurst,units,type)
  call get_parami(dbm,table,'samplesPerBurst',samplesPerBurst,units,type)
  call get_paramd(dbm,table,'prf',prf,units,type)
  call get_paramd(dbm,table,'wvl',wvl,units,type)
  call get_paramc(dbm,table,'slc_file',slcinfile,units,type) ! input slc file
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
  !  call get_paramd(dbm,table,'rangeSamplingRate',rangeSamplingRate,units,type)
  call get_paramd(dbm,table,'radarFrequency',radarFrequency,units,type)
  call get_paramc(dbm,table,'orbinfo',orbfile,units,type)
  call close_db(dbm)

  !c  now open the slave database get corresponding params
  call open_db(dbs,dbsfile)
  table='file'
  !print *,trim(table)
  call get_parami(dbs,table,'azimuthBursts',azimuthBurstsSlave,units,type) ! burst params
  call get_parami(dbs,table,'linesPerBurst',linesPerBurstSlave,units,type)
  call get_parami(dbs,table,'samplesPerBurst',samplesPerBurstSlave,units,type)
  call get_paramd(dbs,table,'prf',prfSlave,units,type)
  call get_paramd(dbs,table,'wvl',wvlSlave,units,type)
  call get_paramc(dbs,table,'slc_file',slcinfileSlave,units,type) ! input slc file
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
  !  call get_paramd(dbs,table,'rangeSamplingRate',rangeSamplingRateSlave,units,type)
  call get_paramd(dbs,table,'azimuthTimeInterval',azimuthTimeIntervalSlave,units,type)
  call get_paramd(dbs,table,'rangeSamplingRate',rangeSamplingRateSlave,units,type)
  call get_paramd(dbs,table,'radarFrequency',radarFrequencySlave,units,type)
  call get_paramc(dbs,table,'orbinfo',orbfileSlave,units,type)
  call close_db(dbs)

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

  print *,'DEM parameters:'
  print *,demwidth,demlength,firstlon,firstlat,deltalon,deltalat
  print *
  ! open dem file
  open(25, file=demfile,access='direct',recl=2*demwidth,form='unformatted')

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

  !c read in the slave orbit state vectors
  open(21,file=orbtimingfileSlave)
  read(21,*)timefirst
  read(21,*)timeend
  read(21,*)nlines
  read(21,*)numstatevecSlave
  !print *,'Number of state vectors: ',numstatevec
  !c  read in state vectors
  do i=1,numstatevecSlave
     read(21,*)timeorbitSlave(i),x,v,a
     xxSlave(:,i)=x
     vvSlave(:,i)=v
     aaSlave(:,i)=a
     !print *,timeorbit(i)
  end do
  close(21)

  ! get starting time
  timer0 = secnds(0.0)

  !$OMP PARALLEL
  !$OMP MASTER
  ith = omp_get_num_threads() !total num threads
  !$OMP END MASTER
  !$OMP END PARALLEL
  print *, "threads",ith

  ! allocate
  allocate(lon(demwidth))
  allocate(rgm(demwidth), rgmslave(demwidth))
  allocate(azt(demwidth), aztslave(demwidth))
  allocate(rgoff(demwidth), rgoffslave(demwidth))
  allocate(azoff(demwidth), azoffslave(demwidth))
  allocate(demin(demwidth))
  ! allocate output data array based on dem size
  allocate(outdata(demwidth),outdataslave(demwidth))
  allocate(outdata2(demwidth),outdataslave2(demwidth),outdata3(demwidth))
  allocate(baddata(demwidth))
  allocate(hist(azimuthBursts-1,101))

  hist=0
  baddata=cmplx(0.,0.)

  open(40, file='overlapphases',access='direct',recl=8*demwidth,form='unformatted')
  open(50, file='overlapphases2',access='direct',recl=8*demwidth,form='unformatted')
  open(60, file='residual',access='direct',recl=8*demwidth,form='unformatted')
  open(70, file='amps',access='direct',recl=8*demwidth,form='unformatted')
  open(41, file='amps1',access='direct',recl=8*demwidth,form='unformatted')
  open(51, file='amps2',access='direct',recl=8*demwidth,form='unformatted')
  open(80, file='rangeoff',access='direct',recl=8*demwidth,form='unformatted')

  do line = 1, demlength
     write(40,rec=line)baddata  ! initialize output file
     write(50,rec=line)baddata  ! initialize output file
     write(60,rec=line)baddata  ! initialize output file
     write(70,rec=line)baddata  ! initialize output file
     write(41,rec=line)baddata  ! initialize output file
     write(51,rec=line)baddata  ! initialize output file
  end do

  !c open master and slave files
  open(21,file=slcinfile,access='direct',recl=8*samplesPerBurst)
  open(22,file=slcinfileslave,access='direct',recl=8*samplesPerBurstSlave)

  !c loop over bursts
  do burst=1,azimuthBursts-1
     ! master file first, start and stop times for current burst
     dtaz = 1.d0 / prf
     tstart = startime(burst)
     tend  = tstart + (linesPerBurst-1)* dtaz
     tstart2 = startime(burst+1)
     tend2  = tstart2 + (linesPerBurst-1)* dtaz
     overlap = (tend-tstart2)/dtaz

!!$     print *,firstValidLine(1:azimuthbursts)
!!$     print *,lastValidLine(1:azimuthbursts)
!!$     print *,firstValidLineSlave(1:azimuthbursts)
!!$     print *,lastValidLineSlave(1:azimuthbursts)
!!$     print *,linesperburst,linesperburstslave

     print '(a,i5,a,4f10.3)', 'Burst ',burst,', Start, stop Acquisition time: ', tstart,tend,tstart2,tend2
     print *,'Burst, overlap lines: ',burst,overlap
     tmid = tstart/2+tend/2
     !print *,'Initialize orbit to master time ',tmid
     stat =  intp_orbit(timeorbit, xx, vv, numstatevec, tmid, xyz_mid, vel_mid)
     if (stat.ne.0) then
        print *, 'Cannot interpolate orbits at the center of scene.'
        stop
     endif
     !print '(a,f10.2,3f12.2,3f12.4)','Sat midpoint t,x,v:       ',tmid,xyz_mid,vel_mid

     llh(1) = (firstlat+demlength/2*deltalat) * deg2rad
     llh(2) = (firstlon+demwidth/2*deltalon) * deg2rad
     llh(3) = 1000.
!!$     print *,'Scene center: ',llh(1)*180/pi,llh(2)*180/pi

     i_type = LLH_2_XYZ  !c  convert to xyz coordinates
     call latlon(elp,xyz,llh,i_type)
     call orbitrangetime(xyz,timeorbit,xx,vv,numstatevec,tmid,xyz_mid,vel_mid,tline,rngpix)

     rngstart = slantRangeTime*sol/2.d0
     dmrg = sol/2.d0/rangeSamplingRate !Nrnglooks * drho
     rngend = rngstart + (samplesPerBurst-1)*dmrg
     rngmid = 0.5d0*(rngstart+rngend)
!!$     print *, 'Near Range in m: ', rngstart 
!!$     print *, 'Far  Range in m: ', rngend
!!$     print *, 'Range sample spacing in m: ', dmrg

     ! slave orbit timing
     dtazslave = 1.d0 / prfslave 
     tstartslave = startimeslave(burst)
     tendslave  = tstartslave + (linesPerBurstSlave-1)* dtazslave
     tstartslave2 = startimeslave(burst+1)
     tendslave2  = tstartslave2 + (linesPerBurstSlave-1)* dtazslave
     !overlap = (tendslave-tstartslave2)/dtaz

     !print *,'Slave burst, overlap lines: ',burst,overlap
     tmidslave = tstartslave/2+tendslave/2
     !print *,'Initialize orbit to slave time ',tmidslave
     stat =  intp_orbit(timeorbit, xx, vv, numstatevec, tmidslave, xyz_midslave, vel_midslave)
     if (stat.ne.0) then
        print *, 'Cannot interpolate orbits at the center of scene.'
        stop
     endif
     !     print '(a,f10.2,3f12.2,3f12.4)','Sat midpoint t,x,v:       ',tmidslave,xyz_midslave,vel_midslave

     call orbitrangetime(xyz,timeorbitslave,xxslave,vvslave,numstatevecslave, &
          tmidslave,xyz_midslave,vel_midslave,tline,rngpix)
     !     print *,'slave scene center time, range ',tline,rngpix
     !print *,'t x v ',tmid,xyz_mid,vel_mid

     rngstartslave = slantRangeTimeSlave*sol/2.d0
     dmrgslave = sol/2.d0/rangeSamplingRateSlave !Nrnglooks * drho
     rngendslave = rngstartslave + (samplesPerBurstSlave-1)*dmrgslave
     rngmidslave = 0.5d0*(rngstartslave+rngendslave)

!!!!     overlap=linesPerBurst   !!debug facilitator
     allocate (burstdata(samplesPerBurst,overlap), burstdataslave(samplesPerBurstSlave,overlap))
     allocate (burstdata2(samplesPerBurst,overlap), burstdataslave2(samplesPerBurstSlave,overlap))

     ! read in overlap region from current burst, master and slave files
     do i=1,overlap
        read(21,rec=burst*linesPerBurst-overlap+i)burstdata(:,i)
     end do
     do i=1,overlap
        read(22,rec=burst*linesPerBurst-overlap+i)burstdataslave(:,i)
     end do
     ! now overlap region from succeeding burst, master and slave files
     do i=1,overlap
        read(21,rec=burst*linesPerBurst+i)burstdata2(:,i)
     end do
     do i=1,overlap
        read(22,rec=burst*linesPerBurst+i)burstdataslave2(:,i)
     end do

     ! geolocation of the current burst overlap region (end of first of pair)
     tstartoverlap= tstart+(linesPerBurst-overlap)*dtaz
     call bounds(tstartoverlap,tend,rngstart,rngend,timeorbit,xx,vv,numstatevec,latlons)
     ! get starting and stopping latitude lines
     latline1=(latlons(2)-firstlat)/deltalat
     latline2=(latlons(1)-firstlat)/deltalat
     !print *,'latlines ',latline1,latline2

     ! form interferogram in geocoded space

     !$OMP PARALLEL DO private(azt,rgm,rgoff,azoff,lat,lon,i)&
     !$OMP private(demin,llh,i_type,xyz,tline,rngpix)&
     !$OMP private(rgmslave,aztslave,rgoffslave,azoffslave,outdata,outdataslave,outdata3)&
     !$OMP private(phase,iphase,outdata2,outdataslave2)&
     !$OMP shared(demlength,BAD_VALUE,firstlat,firstlon,deltalat,deltalon) &
     !$OMP shared(latlons,deg2rad,LLH_2_XYZ,elp) &
     !$OMP shared(timeorbit,xx,vv,numstatevec,tmid,xyz_mid,vel_mid) &
     !$OMP shared(rngstart,dmrg,rngstartslave,dmrgslave,tstartslave,dtazslave,tstart,dtaz) &
     !$OMP shared(timeorbitslave,xxslave,vvslave,numstatevecslave,tmidslave,xyz_midSlave,vel_midSlave) &
     !$OMP shared(samplesPerBurst,linesPerBurst,samplesPerBurstslave,linesPerBurstslave,pi,wvl) &
     !$OMP shared(latline1,latline2,burst) &
     !$OMP shared(burstdata,burstdataslave,burstdata2,burstdataslave2,hist)

     do line = 1, demlength  !c  loop over dem line index
        !        print *,burst,line,firstlat+(line-1)*deltalat
        !!Initialize
        azt = BAD_VALUE
        rgm = BAD_VALUE
        rgoff = BAD_VALUE
        azoff = BAD_VALUE

        lat=firstlat+(line-1)*deltalat
        do i=1,demwidth
           lon(i)=firstlon+(i-1)*deltalon
        end do

        ! if outside range don't bother with computations
        !        print *,line,latlons
        if(lat.ge.latlons(1).and.lat.le.latlons(2))then

           !!Read in this line from DEM
           read(25,rec=line)demin

!!$           if (mod(line,1000).eq.1) then
!!$              print *, 'Processing line: ', line
!!$           endif

           !c loop over dem line

           do i=1,demwidth
              if(lon(i).ge.latlons(3).and.lon(i).le.latlons(4))then
                 llh(1) = lat * deg2rad
                 llh(2) = lon(i) * deg2rad
                 llh(3) = demin(i)
                 i_type = LLH_2_XYZ  !c  convert to xyz coordinates
                 call latlon(elp,xyz,llh,i_type)
                 call orbitrangetime(xyz,timeorbit,xx,vv,numstatevec,tmid,xyz_mid,vel_mid,tline,rngpix)
                 rgm(i) = rngpix
                 azt(i) = tline
                 rgoff(i) = ((rngpix - rngstart)/dmrg) !- 1.0d0*(pixel-1)
                 azoff(i) = ((tline - tstart)/dtaz) !- 1.0d0*(line-1)
!!!$                 print *,i,xyz
!!!$                 print *,rgm(i),azt(i),rgoff(i),azoff(i)

                 !c   repeat for slave positions
                 call orbitrangetime(xyz,timeorbitslave,xxslave,vvslave,numstatevecslave,tmidslave,  &
                      xyz_midSlave,vel_midSlave,tline,rngpix)
                 rgmslave(i) = rngpix
                 aztslave(i) = tline
                 rgoffslave(i) = ((rngpix - rngstartslave)/dmrgslave) 
                 azoffslave(i) = ((tline - tstartslave)/dtazslave) 
                 !print *,lat,lon(i),rgoff(i),rgoffslave(i)

              end if
           end do ! longitude loop ends here

           !c  for each line, get offsets into master burst and resample to grid
           call resample2grid(rgoff,azoff,rgm,burstdata,demwidth, &
                samplesPerBurst,linesPerBurst,overlap,firstValidLine(burst),lastValidLine(burst),wvl,1,outdata)
           call resample2grid(rgoffslave,azoffslave,rgmslave,burstdataslave,demwidth, &
                samplesPerBurstslave,linesPerBurstslave,overlap, &
                firstValidLineSlave(burst),lastValidLineSlave(burst),wvl,1,outdataslave)

           ! interfere
           do i=1,demwidth
              if(cabs(outdata(i)).le.1.e-5.or.cabs(outdataslave(i)).le.1.e-5)then
                 outdata(i)=cmplx(0.,0.)
                 outdataslave(i)=cmplx(0.,0.)
              end if
           end do
           write(41,rec=(burst-1)*(latline2-latline1)+line-latline1+1)cmplx(cabs(outdata),cabs(outdataslave))
           outdata=outdata*conjg(outdataslave)
           write(40,rec=(burst-1)*(latline2-latline1)+line-latline1+1)outdata 

           !c  now resample top of succeeding burst 
           call resample2grid(rgoff,azoff,rgm,burstdata2,demwidth, &
                samplesPerBurst,linesPerBurst,overlap,firstValidLine(burst+1),lastValidLine(burst+1),wvl,2,outdata2)
           call resample2grid(rgoffslave,azoffslave,rgmslave,burstdataslave2,demwidth, &
                samplesPerBurstslave,linesPerBurstslave,overlap, &
                firstValidLineSlave(burst+1),lastValidLineSlave(burst+1),wvl,2,outdataslave2)

           ! interfere
           do i=1,demwidth
              if(cabs(outdata2(i)).le.1.e-5.or.cabs(outdataslave2(i)).le.1.e-5)then
                 outdata2(i)=cmplx(0.,0.)
                 outdataslave2(i)=cmplx(0.,0.)
              end if
           end do
           write(51,rec=(burst-1)*(latline2-latline1)+line-latline1+1)cmplx(cabs(outdata2),cabs(outdataslave2))
           outdata2=outdata2*conjg(outdataslave2)
           write(50,rec=(burst-1)*(latline2-latline1)+line-latline1+1)outdata2 !cmplx(rgoff,rgoffslave) !outdata 

           ! compute residual
           do i=1,demwidth
              if(cabs(outdata(i)).le.1.e-5.or.cabs(outdata2(i)).le.1.e-5)then
                 outdata(i)=cmplx(0.,0.)
                 outdata2(i)=cmplx(0.,0.)
              end if
           end do
           outdata3=outdata*conjg(outdata2)
           write(60,rec=(burst-1)*(latline2-latline1)+line-latline1+1)outdata3
           write(70,rec=(burst-1)*(latline2-latline1)+line-latline1+1) &
                cmplx(cabs(outdata),cabs(outdata2))
           ! and save the range location for each point
           write(80,rec=(burst-1)*(latline2-latline1)+line-latline1+1)rgoff

           ! phase histogram
           do i=1,demwidth
              if(cabs(outdata3(i)).ge.1.e-5)then
                 phase=atan2(aimag(outdata3(i)),real(outdata3(i)))/2./pi*99.999
                 if(phase.lt.0.0)phase=phase+100
                 iphase=int(phase)+1
                 if(iphase.lt.1)print *,phase,iphase
                 if(iphase.gt.100)print *,phase,iphase
                 iphase=min(100,max(1,iphase))
                 hist(burst,iphase)=hist(burst,iphase)+1
              end if
           end do
                 
        end if ! end if that checks if inside latitude line bounds
     end do ! line loop ends here
     !$OMP END PARALLEL DO

     deallocate(burstdata,burstdataslave,burstdata2,burstdataslave2)

  end do

  ! save histogram
  open(31,file='histogram')
  do i=1,101
     write(31,*)i,hist(:,i)
  end do
  close(31)
     
end program overlap_phase

subroutine resample2grid(rgoff,azoff,rgm,burstdata,demwidth,samplesPerBurst,linesPerBurst, &
     overlap,firstValid,lastValid,wvl,topbot,outdata)

  ! topbot flag:  =1 then bottom of burst, =2 then top of burst
  implicit none
  real*8 :: rgoff(demwidth),azoff(demwidth),rgm(demwidth)
  complex*8 :: burstdata(samplesPerBurst,linesPerburst),outdata(demwidth),complex1,complex2
  integer :: i,intr,inta,overlap,linesPerBurst,samplesPerBurst,demwidth,topbot,firstValid,lastValid
  real*8 :: pi,wvl,phase,fracr,fraca

  pi=4.d0*datan2(1.d0,1.d0)
  outdata=cmplx(0.,0.)
  do i=1,demwidth
     if(rgoff(i).ge.1.and.rgoff(i).le.samplesPerBurst-1)then
        if(topbot.eq.1)azoff(i)=azoff(i)-(linesPerBurst-overlap)
        if(azoff(i).ge.1.and.azoff(i).le.overlap-1)then
           intr=rgoff(i)
           fracr=rgoff(i)-intr
           inta=azoff(i)
           fraca=azoff(i)-inta
           complex1=burstdata(intr,inta)*(1-fracr)+burstdata(intr+1,inta)*fracr
           complex2=burstdata(intr,inta+1)*(1-fracr)+burstdata(intr+1,inta+1)*fracr
           !outdata(i)=burstdata(nint(rgoff(i)),nint(azoff(i))) ! nearest neighbor
           if(topbot.eq.1)then
              if(inta.lt.lastValid)then ! skip if data not valid
                 outdata(i)=complex1*(1-fraca)+complex2*fraca
                 !outdata(i)=burstdata(nint(rgoff(i)),nint(azoff(i))) ! nearest neighbor
                 ! remove range propagation phase
                 phase=4.d0*pi/wvl*rgm(i)
                 outdata(i)=outdata(i)*cmplx(cos(phase),sin(phase))
                 !print *,inta,outdata(i),burstdata(intr,inta)
              end if
           else
              if(inta.gt.firstValid-1)then ! skip if data not valid
                 outdata(i)=complex1*(1-fraca)+complex2*fraca
                 !outdata(i)=burstdata(nint(rgoff(i)),nint(azoff(i))) ! nearest neighbor
                 ! remove range propagation phase
                 phase=4.d0*pi/wvl*rgm(i)
                 outdata(i)=outdata(i)*cmplx(cos(phase),sin(phase))
                 !print *,inta,outdata(i),burstdata(intr,inta)
              end if
           end if 
        end if
     end if
  end do

  return
end subroutine resample2grid

