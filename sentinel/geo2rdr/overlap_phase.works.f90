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
  character*300 str,slcinfile,slcoutfile,dbmfile,dbsfile,demfile,demrscfile
  character*300 orbtimingfile,units,type,table
  real*8 norm2
  integer stat,cnt,intp_orbit
  integer*4 azimuthBursts,linesPerBurst,samplesPerBurst,overlap
  integer*4 firstValidLine(100), lastValidLine(100)

  integer*4 demwidth,demlength,idemwidth,idemlength,width,length
  integer*4 burst,nlines,numstatevec
  integer*8 dbm,dbs
  real*8 ptsperdempt, posting, deltalat,deltalon,firstlat,firstlon
  real*8 timeorbit(100),xx(3,100),vv(3,100),aa(3,100),x(3),v(3),a(3)
  real*8 timefirst,timeend,slantRangeTime,rangeSamplingRate,prf,startime(100)
  real*8, dimension(:),allocatable :: lat
  real*8, dimension(:),allocatable :: lon
  real*8, dimension(:),allocatable :: dem
  real*8, dimension(:),allocatable :: rgm, rgmslave
  real*8, dimension(:),allocatable :: azt, aztslave
  real*8, dimension(:),allocatable :: rgoff,rgoffslave
  real*8, dimension(:),allocatable :: azoff,azoffslave
  real*4, dimension(:),allocatable :: oldazt, oldrgm, oldazoff, oldrgoff
  integer*2, allocatable :: demin(:)
  integer*4, allocatable :: numiter(:)

!!!!Image limits
  real*8 tstart, tend, tline, tprev, tstartoverlap, tendoverlap
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

!!! Duplicated variables for slave computations
  integer*4 :: firstValidLineSlave(100),lastValidLineSlave(100)
  integer*4 :: samplesPerBurstSlave,linesPerBurstSlave,numstatevecSlave
  integer*4 :: azimuthburstsSlave,nrangeslave
  real*8 :: startimeslave(100),wvlslave,slantrangetimeslave,radarfrequencySlave
  real*8 :: rangesamplingrateSlave,prfslave,azimuthtimeintervalslave
  real*8 :: xxslave(3,100),vvslave(3,100),aaslave(3,100),timeorbitslave(100)
  real*8 :: tstartslave,tmidslave,tendslave,rngstartslave,rngmidslave,rngendslave
  real*8 :: dtazslave,dmrgslave
  real*8 :: tlineslave, xyz_midSlave(3),vel_midSlave(3)
  character*300 :: slcinfileSlave,orbitfileSlave
  character*300 :: orbfileslave,orbtimingfileslave

  integer :: numOutsideImage

  real*4 :: timer0, timer1  
  ! array to hold each burst
  complex*8, allocatable :: burstdata(:,:),burstdataslave(:,:), baddata(:), radarcoords(:,:),firstoverlap(:)
  complex*8, allocatable :: outdata(:),olddata(:),ampdata(:),outdataslave(:),olddataslave(:)
  complex*8 :: complex1, complex2
  real*8 :: fraca, fracr, phase, wvl
  integer :: inta, intr

  ! variables for saving phases as a function of range and burst
  complex*8, allocatable :: overlap_phases(:)
  real*8, allocatable :: overlap_ranges(:)

  ! variables associated with offset carrier removal
  character*300 schfile, fmratefile, dcfile, orbfile, offsetfile
  integer*4  nrange
  integer*4  iburst, j, irec!, npolyorb, npolyfm, npolydc, 
  real*8  azimuthSteeringRate,azimuthTimeInterval,radarFrequency
  real*8  r_ro, r_ao!, elev
  real*8  frac, timecenterseconds,rawdataprf
  real*8  off
  complex*8, allocatable :: cphase(:), dataline(:)

  ! some leftover variables not used at present but needing declaration
  logical bistatic
  real*8 fdvsrng,fddotvsrng,orbit
  integer ilrl

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
     print *,'usage: overlap_phase master_db slave_db overlap-pixels slcoutfile'
     stop
  end if

  call getarg(1,dbmfile)
  call getarg(2,dbsfile)
  call getarg(3,str)
  read(str,*)overlap
  call getarg(4,slcoutfile)

  !c  open the master database and read some params
  call open_db(dbm,dbmfile)
  table='file'
  !print *,trim(table)
  call get_parami(dbm,table,'azimuthBursts',azimuthBursts,units,type) ! burst params
  call get_parami(dbm,table,'linesPerBurst',linesPerBurst,units,type)
  call get_parami(dbm,table,'samplesPerBurst',samplesPerBurst,units,type)
  nrange=samplesPerBurst
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
  nrangeSlave=samplesPerBurstSlave
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
  cnt = 0

  !$OMP PARALLEL
  !$OMP MASTER
  ith = omp_get_num_threads() !total num threads
  !$OMP END MASTER
  !$OMP END PARALLEL
  print *, "threads",ith

  ! allocate
  allocate(lat(demwidth))
  allocate(lon(demwidth))
  allocate(dem(demwidth))
  allocate(rgm(demwidth), oldrgm(demwidth), rgmslave(demwidth))
  allocate(azt(demwidth), oldazt(demwidth), aztslave(demwidth))
  allocate(rgoff(demwidth), oldrgoff(demwidth), rgoffslave(demwidth))
  allocate(azoff(demwidth), oldazoff(demwidth), azoffslave(demwidth))
  allocate(demin(demwidth))
  ! allocate output data array based on dem size
  allocate(outdata(demwidth),ampdata(demwidth),outdataslave(demwidth))
  allocate(baddata(demwidth))
  allocate(olddata(demwidth),olddataslave(demwidth))
  allocate(burstdata(samplesPerBurst,linesPerBurst),burstdataslave(samplesPerBurstSlave,linesPerBurstSlave))
  allocate(firstoverlap(demwidth),radarcoords(samplesPerBurstSlave,overlap),numiter(demwidth))
  allocate(overlap_phases(samplesPerBurstSlave))

  baddata=cmplx(0.,0.)

!!$  open(31, file='range',access='direct',recl=4*demwidth,form='unformatted')
!!$  open(32, file='azimuth',access='direct',recl=4*demwidth,form='unformatted')
!!$  open(33, file='rangeoff',access='direct',recl=4*demwidth,form='unformatted')
!!$  open(34, file='azimuthoff',access='direct',recl=4*demwidth,form='unformatted')
  open(30, file=trim(slcoutfile)//'master',access='direct',recl=8*demwidth,form='unformatted')
  open(40, file=trim(slcoutfile)//'slave',access='direct',recl=8*demwidth,form='unformatted')
  open(50, file=trim(slcoutfile)//'cross',access='direct',recl=8*demwidth,form='unformatted')
  open(60, file=trim(slcoutfile),access='direct',recl=8*samplesPerBurstSlave*overlap)
  open(70, file='overlap_phases',access='direct',recl=8*samplesPerBurstSlave)

  do line = 1, demlength
     write(30,rec=line)baddata  ! initialize output file
     write(40,rec=line)baddata  ! initialize output file
     write(50,rec=line)baddata  ! initialize output file
  end do

  !c loop over bursts
  do burst=1,azimuthBursts-1
     radarcoords=cmplx(0.,0.)
     overlap_phases=cmplx(0.,0.)
     ! master first
     tstart = startime(burst)
     dtaz = 1.d0 / prf ! Nazlooks / prf
     tend  = tstart + (linesPerBurst-1)* dtaz
     tmid = 0.5d0*(tstart+tend)

     print *, 'Burst ',burst,', Start, stop Acquisition time: ', tstart,tend
     tline = tstart
     stat =  intp_orbit(timeorbit, xx, vv, tline, xyz_mid, vel_mid)
!     print *,'Position at start of burst 1 ',xyz_mid
     tline = tend
     stat =  intp_orbit(timeorbit, xx, vv, tline, xyz_mid, vel_mid)
!     print *,'Position at end of burst 1 ',xyz_mid

     ! time for overlap region is at end of burst 1, start of burst 2, etc.
     tstartoverlap = startime(burst)+(linesPerBurst-overlap)*dtaz
     tmid = 0.5d0*(tstartoverlap+tend)
!     print *,'overlap region start, stop: ',tstartoverlap,tend
     tline = tstartoverlap
     stat =  intp_orbit(timeorbit, xx, vv, tline, xyz_mid, vel_mid)
!     print *,'Position at start of overlap ',xyz_mid
     tline = tend
     stat =  intp_orbit(timeorbit, xx, vv, tline, xyz_mid, vel_mid)
!     print *,'Position at end of overlap ',xyz_mid

     rngstart = slantRangeTime*sol/2.d0
     dmrg = sol/2.d0/rangeSamplingRate !Nrnglooks * drho
     rngend = rngstart + (samplesPerBurst-1)*dmrg
     rngmid = 0.5d0*(rngstart+rngend)
!!$     print *, 'Near Range in m: ', rngstart 
!!$     print *, 'Far  Range in m: ', rngend
!!$     print *, 'Range sample spacing in m: ', dmrg

     length=linesPerBurst
     width=samplesPerBurst
!!$     print *, 'Radar Image Burst Lines: ', length
!!$     print *, 'Radar Image Burst Width: ', width

     ! slave orbit timing
     tstartslave = startimeslave(burst)
     dtazslave = 1.d0 / prfslave ! Nazlooks / prf
     tendslave  = tstartslave + (linesPerBurstSlave-1)* dtazslave
     tmidslave = 0.5d0*(tstartslave+tendslave)

     ! time for slave overlap region is at end of burst 1, start of burst 2, etc.
     tstartoverlap = startimeslave(burst)+(linesPerBurstSlave-overlap)*dtazslave
     tmidslave = 0.5d0*(tstartoverlap+tendslave)
!     print *,'slave overlap region start, stop: ',tstartslave,tendslave
     tlineslave = tstartoverlap
     stat =  intp_orbit(timeorbitSlave, xxSlave, vvSlave, tlineslave, xyz_midSlave, vel_midSlave)
!     print *,'Slave position at start of overlap ',xyz_midSlave
     tlineslave = tendslave
     stat =  intp_orbit(timeorbitSlave, xxSlave, vvSlave, tlineSlave, xyz_midSlave, vel_midSlave)
!     print *,'Slave position at end of overlap ',xyz_midSlave

     rngstartslave = slantRangeTimeSlave*sol/2.d0
     dmrgslave = sol/2.d0/rangeSamplingRateSlave !Nrnglooks * drho
     rngendslave = rngstartslave + (samplesPerBurstSlave-1)*dmrgslave
     rngmidslave = 0.5d0*(rngstartslave+rngendslave)

     ! read in the master burst
     open(21,file=slcinfile,access='direct',recl=8*samplesPerBurst*linesPerBurst)
     read(21,rec=burst)burstdata
     close(21)

     ! read in the slave burst
     open(21,file=slcinfileslave,access='direct',recl=8*samplesPerBurstSlave*linesPerBurstSlave)
     read(21,rec=burst)burstdataslave
     close(21)

!!$! write first burst for test
!!$     if(burst.eq.1)then
!!$        open(21,file='bursttest',access='direct',recl=8*samplesPerBurst*linesPerBurst)
!!$        write(21,rec=1)burstdata
!!$        close(21)
!!$     end if
     ! geolocation of the current burst overlap region (end of first of pair)
     tstartoverlap= startime(burst)+(linesPerBurst-overlap)*dtaz
     call bounds(tstartoverlap,tend,rngstart,rngend,timeorbit,xx,vv,latlons)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     !! PROCESSING STEPS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!Initialize satellite positions
     tline = tmid
     stat =  intp_orbit(timeorbit, xx, vv, tline, xyz_mid, vel_mid)

     if (stat.ne.0) then
        print *, 'Cannot interpolate orbits at the center of scene.'
        stop
     endif
!     print *,'Satellite midpoint time,position,velocity: ',tline,xyz_mid,vel_mid

     tline = tmidslave
     stat =  intp_orbit(timeorbitslave, xxslave, vvslave, tline, xyz_midslave, vel_midslave)

     if (stat.ne.0) then
        print *, 'Cannot interpolate orbits at the center of scene.'
        stop
     endif
!     print *,'Slave satellite midpoint time,position,velocity: ',tline,xyz_midslave,vel_midslave

!!$     print *, "geo2rdr on ",ith,' threads...'

     numOutsideImage = 0

     do line = 1, demlength
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
        !print *,line,lat(1),latlons(1),latlons(2)
        if(lat(1).ge.latlons(1))then
           if(lat(1).le.latlons(2))then

              !!Read in this line from DEM
              read(22,rec=line)demin
              dem=demin

              if (mod(line,1000).eq.1) then
                 print *, 'Processing line: ', line
              endif
              conv = 0
              numiter=0
!c  pixel locations in the master file
              !$OMP PARALLEL DO private(pixel,i_type,k)&
              !$OMP private(xyz,llh,rngpix,tline,satx,satv)&
              !$OMP private(c1,c2,tprev,dr,stat,fn,fnprime)&
              !$OMP private(dopfact,fdop,fdopder,sata) &
              !$OMP shared(length,width,demwidth) &
              !$OMP shared(rgm,azt,rgoff,azoff) &
              !$OMP shared(line,elp,ilrl,tstart,tmid,rngstart,rngmid) &
              !$OMP shared(xyz_mid,vel_mid,acc_mid,fdvsrng,fddotvsrng) &
              !$OMP shared(lat,lon,dem,dtaz,dmrg,deg2rad,bistatic,sol) &
              !$OMP shared(numOutsideImage,wvl,orbit,conv)!,numiter) 
              do pixel = 1,demwidth 

                 llh(1) = lat(pixel) * deg2rad
                 llh(2) = lon(pixel) * deg2rad
                 llh(3) = dem(pixel)

                 i_type = LLH_2_XYZ
                 call latlon(elp,xyz,llh,i_type)

!!!!Actual iterations
                 tline = tmid
                 satx = xyz_mid
                 satv = vel_mid
                 sata = acc_mid

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

                    stat = intp_orbit(timeorbit,xx,vv,tline,satx,satv)

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
                    if (abs(tline - tprev).lt.5.0d-7) then
                       conv = conv + 1
                       exit
                    endif
                 enddo  ! end iteration loop
                 !numiter(pixel)=k

                 if(tline.lt.tstart) then
                    numOutsideImage = numOutsideImage + 1
                    goto 100
                 endif

                 if(tline.gt.tend) then
                    numOutsideImage = numOutsideImage + 1
                    goto 100
                 endif

                 dr = xyz - satx
                 rngpix = dsqrt(dot_product(dr,dr)) !norm2(dr)

                 if(rngpix.lt.rngstart) then
                    numOutsideImage = numOutsideImage + 1
                    goto 100
                 endif

                 if(rngpix.gt.rngend) then
                    numOutsideImage = numOutsideImage + 1
                    goto 100
                 endif

                 cnt = cnt + 1
                 rgm(pixel) = rngpix
                 azt(pixel) = tline

                 rgoff(pixel) = ((rngpix - rngstart)/dmrg) !- 1.0d0*(pixel-1)
                 azoff(pixel) = ((tline - tstart)/dtaz) !- 1.0d0*(line-1)

100              continue
              enddo   ! end pixel loop

              !$OMP END PARALLEL DO

              !print *,numiter
!c  corresponding pixel locations in the slave file
              !$OMP PARALLEL DO private(pixel,i_type,k)&
              !$OMP private(xyz,llh,rngpix,tline,satx,satv)&
              !$OMP private(c1,c2,tprev,dr,stat,fn,fnprime)&
!c              !$OMP private(dopfact,fdop,fdopder,sata) &
              !$OMP shared(length,width,demwidth) &
              !$OMP shared(rgmslave,aztslave,rgoffslave,azoffslave) &
              !$OMP shared(line,elp,tstartslave,tmidslave,rngstartslave,rngmidslave) &
              !$OMP shared(xyz_midSlave,vel_midSlave,acc_mid,fdvsrng,fddotvsrng) &
              !$OMP shared(lat,lon,dem,dtazslave,dmrgslave,deg2rad,sol) &
              !$OMP shared(numOutsideImage,wvl,conv) &
              !$OMP shared(timeorbitslave,xxslave,vvslave)

              do pixel = 1,demwidth 

                 llh(1) = lat(pixel) * deg2rad
                 llh(2) = lon(pixel) * deg2rad
                 llh(3) = dem(pixel)

                 i_type = LLH_2_XYZ
                 call latlon(elp,xyz,llh,i_type)
!!!!Actual iterations
                 tline = tmidslave
                 satx = xyz_midSlave
                 satv = vel_midSlave
                 !sata = acc_mid

                 do k=1,51
                    tprev = tline  

                    dr = xyz - satx
                    rngpix = dsqrt(dot_product(dr,dr)) !norm2(dr)    

                    fn = dot_product(dr,satv)
                    fnprime = -dot_product(satv,satv)

                    tline = tline - fn / fnprime
                    stat = intp_orbit(timeorbitslave,xxslave,vvslave,tline,satx,satv)

!!!Check for convergence
                    if (abs(tline - tprev).lt.5.0d-9) then
                       conv = conv + 1
                       exit
                    endif
                 enddo

                 if(tline.lt.tstartslave) then
                    numOutsideImage = numOutsideImage + 1
                    goto 102
                 endif

                 if(tline.gt.tendslave) then
                    numOutsideImage = numOutsideImage + 1
                    goto 102
                 endif

                 dr = xyz - satx
                 rngpix = dsqrt(dot_product(dr,dr)) !norm2(dr)

                 if(rngpix.lt.rngstartslave) then
                    numOutsideImage = numOutsideImage + 1
                    goto 102
                 endif

                 if(rngpix.gt.rngendslave) then
                    numOutsideImage = numOutsideImage + 1
                    goto 102
                 endif

                 cnt = cnt + 1
                 rgmslave(pixel) = rngpix
                 aztslave(pixel) = tline

                 rgoffslave(pixel) = ((rngpix - rngstartslave)/dmrgslave) 
                 azoffslave(pixel) = ((tline - tstartslave)/dtazslave) 

102              continue
              enddo   ! end pixel loop on slave

              !$OMP END PARALLEL DO

!c  get offsets into master burst and resample to grid
              rgoff=max(rgoff,1.d0)
              rgoff=min(rgoff,samplesPerBurst-1.d0)
              azoff=max(azoff,1.d0)
              azoff=min(azoff,linesPerBurst-1.d0)
              outdata=cmplx(0.,0.)
              !ampdata=cmplx(0.,0.)
              do i=1,demwidth
                 intr=rgoff(i)
                 fracr=rgoff(i)-intr
                 inta=azoff(i)
                 fraca=azoff(i)-inta
                 !if(inta.ge.1000)print *,inta
                 if(inta.gt.linesperBurst-overlap)then ! skip if data not valid
                    if(inta.lt.lastValidLine(burst)-1)then
                       ! interpolate in range first, then azimuth bilinearly
                       complex1=burstdata(intr,inta)*(1-fracr)+burstdata(intr+1,inta)*fracr
                       complex2=burstdata(intr,inta+1)*(1-fracr)+burstdata(intr+1,inta+1)*fracr
                       outdata(i)=complex1*(1-fraca)+complex2*fraca
                       !outdata(i)=burstdata(nint(rgoff(i)),nint(azoff(i))) ! nearest neighbor
                       ! remove range progation phase
                       phase=4.d0*pi/wvl*rgm(i)
                       outdata(i)=outdata(i)*cmplx(cos(phase),sin(phase))
                       !print *,inta,outdata(i),burstdata(intr,inta)
                    end if
                 end if
              end do

              ! only overwrite with nonzeros
              read(30,rec=line)olddata
101           continue
              do i=1,demwidth
                 if(cabs(outdata(i)).gt.1.e-5)then
                    olddata(i)=outdata(i)
                 end if
              end do
              write(30,rec=line)olddata

!c  resample slave to same grid points
              rgoffslave=max(rgoffslave,1.d0)
              rgoffslave=min(rgoffslave,samplesPerBurstSlave-1.d0)
              azoffslave=max(azoffslave,1.d0)
              azoffslave=min(azoffslave,linesPerBurstSlave-1.d0)
              outdataslave=cmplx(0.,0.)
              do i=1,demwidth
                 intr=rgoffslave(i)
                 fracr=rgoffslave(i)-intr
                 inta=azoffslave(i)
                 fraca=azoffslave(i)-inta
                 !print *,'intr inta ',intr,inta
                 if(inta.gt.linesperBurstSlave-overlap)then ! skip if data not valid
                    if(inta.lt.lastValidLineSlave(burst)-1)then
                       ! interpolate in range first, then azimuth bilinearly
                       complex1=burstdataslave(intr,inta)*(1-fracr)+burstdataslave(intr+1,inta)*fracr
                       complex2=burstdataslave(intr,inta+1)*(1-fracr)+burstdataslave(intr+1,inta+1)*fracr
                       outdataslave(i)=complex1*(1-fraca)+complex2*fraca
                       !outdata(i)=burstdata(nint(rgoff(i)),nint(azoff(i))) ! nearest neighbor
                       ! remove range progation phase
                       phase=4.d0*pi/wvl*rgmslave(i)
                       outdataslave(i)=outdataslave(i)*cmplx(cos(phase),sin(phase))
                       !print *,inta,outdata(i),burstdata(intr,inta)
                    end if
                 end if
              end do

              ! only overwrite with nonzeros
              read(40,rec=line)olddataslave

103           continue
              do i=1,demwidth
                 if(cabs(outdataslave(i)).gt.1.e-5)then
                    !print *,olddata(i),outdata(i)
                    !ampdata(i)=cmplx(cabs(olddata(i)),cabs(outdataslave(i)))
                    olddataslave(i)=olddata(i)*conjg(outdataslave(i))
                 end if
              end do
              write(40,rec=line)olddataslave
              !write(50,rec=line)ampdata
              !print *,'line written ',line,sum(cabs(olddata)),sum(cabs(ampdata))

           end if  !  first 'if' check whether inside latitude limits
        end if ! end if that checks if inside latitude line bounds

     end do ! line loop ends here

!     print *, 'Number of pixels outside the image: ', numOutsideImage
!     print *, 'Number of pixels with valid data:   ', cnt
!     print *, 'Number of pixels that converged:    ', conv

!c  now the subsequent overlapped burst
     ! master first
     tstart = startime(burst+1)
     dtaz = 1.d0 / prf ! Nazlooks / prf
     tend  = tstart + (linesPerBurst-1)* dtaz
     tmid = 0.5d0*(tstart+tend)

     print *, 'Burst ',burst+1,', Start, stop Acquisition time: ', tstart,tend
     tline = tstart
     stat =  intp_orbit(timeorbit, xx, vv, tline, xyz_mid, vel_mid)
!     print *,'Position at start of burst 2 ',xyz_mid
     tline = tend
     stat =  intp_orbit(timeorbit, xx, vv, tline, xyz_mid, vel_mid)
!     print *,'Position at end of burst 2 ',xyz_mid

     ! time for overlap region is at end of burst 1, start of burst 2, etc.
     tstartoverlap = startime(burst+1)
     tendoverlap =tstartoverlap+(overlap-1)*dtaz
     tmid = 0.5d0*(tstartoverlap+tendoverlap)
!     print *,'overlap region start, stop: ',tstartoverlap,tendoverlap
     tline = tstartoverlap
     stat =  intp_orbit(timeorbit, xx, vv, tline, xyz_mid, vel_mid)
!     print *,'Position at start of overlap ',xyz_mid
     tline = tendoverlap
     stat =  intp_orbit(timeorbit, xx, vv, tline, xyz_mid, vel_mid)
!     print *,'Position at end of overlap ',xyz_mid

     rngstart = slantRangeTime*sol/2.d0
     dmrg = sol/2.d0/rangeSamplingRate !Nrnglooks * drho
     rngend = rngstart + (samplesPerBurst-1)*dmrg
     rngmid = 0.5d0*(rngstart+rngend)
!!$     print *, 'Near Range in m: ', rngstart 
!!$     print *, 'Far  Range in m: ', rngend
!!$     print *, 'Range sample spacing in m: ', dmrg

     length=linesPerBurst
     width=samplesPerBurst
!!$     print *, 'Radar Image Burst Lines: ', length
!!$     print *, 'Radar Image Burst Width: ', width

     ! slave orbit timing
     tstartslave = startimeslave(burst+1)
     dtazslave = 1.d0 / prfslave ! Nazlooks / prf
     tendslave  = tstartslave + (linesPerBurstSlave-1)* dtazslave
     tmidslave = 0.5d0*(tstartslave+tendslave)

     ! time for slave overlap region is at end of burst 1, start of burst 2, etc.
     tstartoverlap = startimeslave(burst+1)
     tendoverlap = tstartoverlap+(overlap-1)*dtazslave
     tmidslave = 0.5d0*(tstartoverlap+tendoverlap)
!     print *,'slave overlap region start, stop: ',tstartoverlap,tendoverlap
     tlineslave = tstartoverlap
     stat =  intp_orbit(timeorbitSlave, xxSlave, vvSlave, tlineslave, xyz_midSlave, vel_midSlave)
!     print *,'Slave position at start of overlap ',xyz_midSlave
     tlineslave = tendoverlap
     stat =  intp_orbit(timeorbitSlave, xxSlave, vvSlave, tlineSlave, xyz_midSlave, vel_midSlave)
!     print *,'Slave position at end of overlap ',xyz_midSlave

     rngstartslave = slantRangeTimeSlave*sol/2.d0
     dmrgslave = sol/2.d0/rangeSamplingRateSlave !Nrnglooks * drho
     rngendslave = rngstartslave + (samplesPerBurstSlave-1)*dmrgslave
     rngmidslave = 0.5d0*(rngstartslave+rngendslave)

     print *,'master ',slcinfile
     print *,'slave  ',slcinfileslave

     ! read in the master burst
     open(21,file=slcinfile,access='direct',recl=8*samplesPerBurst*linesPerBurst)
     read(21,rec=burst+1)burstdata
     close(21)

     ! read in the slave burst
     open(21,file=slcinfileslave,access='direct',recl=8*samplesPerBurstSlave*linesPerBurstSlave)
     read(21,rec=burst+1)burstdataslave
     close(21)

!!$! write first burst for test
!!$     if(burst.eq.1)then
!!$        open(21,file='bursttest',access='direct',recl=8*samplesPerBurst*linesPerBurst)
!!$        write(21,rec=1)burstdata
!!$        close(21)
!!$     end if
     ! geolocation of the current burst
     tendoverlap =tstart+(overlap-1)*dtaz
     call bounds(tstart,tendoverlap,rngstart,rngend,timeorbit,xx,vv,latlons)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     !! PROCESSING STEPS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!Initialize satellite positions
     tline = tmid
     stat =  intp_orbit(timeorbit, xx, vv, tline, xyz_mid, vel_mid)

     if (stat.ne.0) then
        print *, 'Cannot interpolate orbits at the center of scene.'
        stop
     endif
!     print *,'Satellite midpoint time,position,velocity: ',tline,xyz_mid,vel_mid

     tline = tmidslave
     stat =  intp_orbit(timeorbitslave, xxslave, vvslave, tline, xyz_midslave, vel_midslave)

     if (stat.ne.0) then
        print *, 'Cannot interpolate orbits at the center of scene.'
        stop
     endif
!     print *,'Slave satellite midpoint time,position,velocity: ',tline,xyz_midslave,vel_midslave

!!$     print *, "geo2rdr on ",ith,' threads...'

     numOutsideImage = 0

     do line = 1, demlength
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
        !print *,line,lat(1),latlons(1),latlons(2)
        if(lat(1).ge.latlons(1))then
           if(lat(1).le.latlons(2))then

              !!Read in this line from DEM
              read(22,rec=line)demin
              dem=demin

              if (mod(line,1000).eq.1) then
                 print *, 'Processing line: ', line
              endif
              conv = 0
              numiter=0
!c  pixel locations in the master file
              !$OMP PARALLEL DO private(pixel,i_type,k)&
              !$OMP private(xyz,llh,rngpix,tline,satx,satv)&
              !$OMP private(c1,c2,tprev,dr,stat,fn,fnprime)&
              !$OMP private(dopfact,fdop,fdopder,sata) &
              !$OMP shared(length,width,demwidth) &
              !$OMP shared(rgm,azt,rgoff,azoff) &
              !$OMP shared(line,elp,ilrl,tstart,tmid,rngstart,rngmid) &
              !$OMP shared(xyz_mid,vel_mid,acc_mid,fdvsrng,fddotvsrng) &
              !$OMP shared(lat,lon,dem,dtaz,dmrg,deg2rad,bistatic,sol) &
              !$OMP shared(numOutsideImage,wvl,orbit,conv)!,numiter) 
              do pixel = 1,demwidth 

                 llh(1) = lat(pixel) * deg2rad
                 llh(2) = lon(pixel) * deg2rad
                 llh(3) = dem(pixel)

                 i_type = LLH_2_XYZ
                 call latlon(elp,xyz,llh,i_type)

!!!!Actual iterations
                 tline = tmid
                 satx = xyz_mid
                 satv = vel_mid
                 sata = acc_mid

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

                    stat = intp_orbit(timeorbit,xx,vv,tline,satx,satv)

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
                    if (abs(tline - tprev).lt.5.0d-7) then
                       conv = conv + 1
                       exit
                    endif
                 enddo  ! end iteration loop
                 !numiter(pixel)=k

                 if(tline.lt.tstart) then
                    numOutsideImage = numOutsideImage + 1
                    goto 200
                 endif

                 if(tline.gt.tend) then
                    numOutsideImage = numOutsideImage + 1
                    goto 200
                 endif

                 dr = xyz - satx
                 rngpix = dsqrt(dot_product(dr,dr)) !norm2(dr)

                 if(rngpix.lt.rngstart) then
                    numOutsideImage = numOutsideImage + 1
                    goto 200
                 endif

                 if(rngpix.gt.rngend) then
                    numOutsideImage = numOutsideImage + 1
                    goto 200
                 endif

                 cnt = cnt + 1
                 rgm(pixel) = rngpix
                 azt(pixel) = tline

                 rgoff(pixel) = ((rngpix - rngstart)/dmrg) !- 1.0d0*(pixel-1)
                 azoff(pixel) = ((tline - tstart)/dtaz) !- 1.0d0*(line-1)

200              continue
              enddo   ! end pixel loop

              !$OMP END PARALLEL DO

              !print *,numiter
!c  corresponding pixel locations in the slave file
              !$OMP PARALLEL DO private(pixel,i_type,k)&
              !$OMP private(xyz,llh,rngpix,tline,satx,satv)&
              !$OMP private(c1,c2,tprev,dr,stat,fn,fnprime)&
!c              !$OMP private(dopfact,fdop,fdopder,sata) &
              !$OMP shared(length,width,demwidth) &
              !$OMP shared(rgmslave,aztslave,rgoffslave,azoffslave) &
              !$OMP shared(line,elp,tstartslave,tmidslave,rngstartslave,rngmidslave) &
              !$OMP shared(xyz_midSlave,vel_midSlave,acc_mid,fdvsrng,fddotvsrng) &
              !$OMP shared(lat,lon,dem,dtazslave,dmrgslave,deg2rad,sol) &
              !$OMP shared(numOutsideImage,wvl,conv) &
              !$OMP shared(timeorbitslave,xxslave,vvslave)

              do pixel = 1,demwidth 

                 llh(1) = lat(pixel) * deg2rad
                 llh(2) = lon(pixel) * deg2rad
                 llh(3) = dem(pixel)

                 i_type = LLH_2_XYZ
                 call latlon(elp,xyz,llh,i_type)
!!!!Actual iterations
                 tline = tmidslave
                 satx = xyz_midSlave
                 satv = vel_midSlave
                 !sata = acc_mid

                 do k=1,51
                    tprev = tline  

                    dr = xyz - satx
                    rngpix = dsqrt(dot_product(dr,dr)) !norm2(dr)    

                    fn = dot_product(dr,satv)
                    fnprime = -dot_product(satv,satv)

                    tline = tline - fn / fnprime
                    stat = intp_orbit(timeorbitslave,xxslave,vvslave,tline,satx,satv)

!!!Check for convergence
                    if (abs(tline - tprev).lt.5.0d-9) then
                       conv = conv + 1
                       exit
                    endif
                 enddo

                 if(tline.lt.tstartslave) then
                    numOutsideImage = numOutsideImage + 1
                    goto 202
                 endif

                 if(tline.gt.tendslave) then
                    numOutsideImage = numOutsideImage + 1
                    goto 202
                 endif

                 dr = xyz - satx
                 rngpix = dsqrt(dot_product(dr,dr)) !norm2(dr)

                 if(rngpix.lt.rngstartslave) then
                    numOutsideImage = numOutsideImage + 1
                    goto 202
                 endif

                 if(rngpix.gt.rngendslave) then
                    numOutsideImage = numOutsideImage + 1
                    goto 202
                 endif

                 cnt = cnt + 1
                 rgmslave(pixel) = rngpix
                 aztslave(pixel) = tline

                 rgoffslave(pixel) = ((rngpix - rngstartslave)/dmrgslave) 
                 azoffslave(pixel) = ((tline - tstartslave)/dtazslave) 

202              continue
              enddo   ! end pixel loop on slave

              !$OMP END PARALLEL DO

!c  get offsets into master burst and resample to grid
              rgoff=max(rgoff,1.d0)
              rgoff=min(rgoff,samplesPerBurst-1.d0)
              azoff=max(azoff,1.d0)
              azoff=min(azoff,linesPerBurst-1.d0)
              outdata=cmplx(0.,0.)
              !ampdata=cmplx(0.,0.)
              do i=1,demwidth
                 intr=rgoff(i)
                 fracr=rgoff(i)-intr
                 inta=azoff(i)
                 fraca=azoff(i)-inta
                 !if(inta.ge.1000)print *,inta
                 if(inta.gt.firstValidLine(burst+1))then ! skip if data not valid
                    if(inta.lt.overlap)then
                       ! interpolate in range first, then azimuth bilinearly
                       complex1=burstdata(intr,inta)*(1-fracr)+burstdata(intr+1,inta)*fracr
                       complex2=burstdata(intr,inta+1)*(1-fracr)+burstdata(intr+1,inta+1)*fracr
                       outdata(i)=complex1*(1-fraca)+complex2*fraca
                       !outdata(i)=burstdata(nint(rgoff(i)),nint(azoff(i))) ! nearest neighbor
                       ! remove range progation phase
                       phase=4.d0*pi/wvl*rgm(i)
                       outdata(i)=outdata(i)*cmplx(cos(phase),sin(phase))
                       !print *,inta,outdata(i),burstdata(intr,inta)
                    end if
                 end if
              end do

              ! only overwrite with nonzeros
              read(30,rec=line)olddata
201           continue
              do i=1,demwidth
                 if(cabs(outdata(i)).gt.1.e-5)then
                    olddata(i)=outdata(i)
                 end if
              end do
              write(30,rec=line)olddata

!c  resample slave to same grid points
              rgoffslave=max(rgoffslave,1.d0)
              rgoffslave=min(rgoffslave,samplesPerBurstSlave-1.d0)
              azoffslave=max(azoffslave,1.d0)
              azoffslave=min(azoffslave,linesPerBurstSlave-1.d0)
              outdataslave=cmplx(0.,0.)
              do i=1,demwidth
                 intr=rgoffslave(i)
                 fracr=rgoffslave(i)-intr
                 inta=azoffslave(i)
                 fraca=azoffslave(i)-inta
                 !print *,'intr inta ',intr,inta
                 if(inta.gt.firstValidLineSlave(burst+1))then ! skip if data not valid
                    if(inta.lt.overlap)then
                       ! interpolate in range first, then azimuth bilinearly
                       complex1=burstdataslave(intr,inta)*(1-fracr)+burstdataslave(intr+1,inta)*fracr
                       complex2=burstdataslave(intr,inta+1)*(1-fracr)+burstdataslave(intr+1,inta+1)*fracr
                       outdataslave(i)=complex1*(1-fraca)+complex2*fraca
                       !outdata(i)=burstdata(nint(rgoff(i)),nint(azoff(i))) ! nearest neighbor
                       ! remove range progation phase
                       phase=4.d0*pi/wvl*rgmslave(i)
                       outdataslave(i)=outdataslave(i)*cmplx(cos(phase),sin(phase))
                       !print *,inta,outdata(i),burstdata(intr,inta)
                    end if
                 end if
              end do

              ! only overwrite with nonzeros
              read(50,rec=line)olddataslave
              read(40,rec=line)firstoverlap

203           continue
              do i=1,demwidth
                 intr=rgoffslave(i)
                 inta=azoffslave(i)
                 if(cabs(outdataslave(i)).gt.1.e-5)then
                    !print *,olddata(i),outdata(i)
                    !ampdata(i)=cmplx(cabs(olddata(i)),cabs(outdataslave(i)))
                    olddataslave(i)=olddata(i)*conjg(outdataslave(i))
                    if(inta.gt.firstValidLineSlave(burst+1))then ! skip if data not valid
                       if(inta.lt.overlap)then
                          radarcoords(intr,inta)=firstoverlap(i)*conjg(olddataslave(i))
                          overlap_phases(intr)=overlap_phases(intr)+radarcoords(intr,inta)
                       end if
                    end if
                 end if
              end do
              write(50,rec=line)olddataslave
              !print *,'line written ',line,sum(cabs(olddata)),sum(cabs(ampdata))

           end if  !  first 'if' check whether inside latitude limits
        end if ! end if that checks if inside latitude line bounds
     enddo
     write(60,rec=burst)radarcoords
     write(70,rec=burst)overlap_phases
  enddo  !  end burst loop

  deallocate(lat,lon,dem)
  deallocate(azt,rgm,aztslave,rgmslave)
  deallocate(azoff,rgoff,azoffslave,rgoffslave)
  deallocate(outdata)

  close(30)
  close(40)
  close(50)
  close(60)
  close(70)

  timer1 = secnds(timer0)
  print *, 'elapsed time = ',timer1,' seconds'
end program overlap_phase

integer function intp_orbit(timeorbit, xx, vv, time, xyz_mid, vel_mid)

  implicit none
  integer ilocation
  real*8 timeorbit(*), xx(3,*), vv(3,*), xyz_mid(3), vel_mid(3), time
  ilocation=(time-timeorbit(1))/(timeorbit(2)-timeorbit(1))
  ilocation=max(ilocation,2)  ! take care of times falling off the end
  !print *,i,time,ilocation,xx(1,ilocation-1),vv(1,ilocation-1),timeorbit(ilocation-1)
  call orbithermite(xx(1,ilocation-1),vv(1,ilocation-1),timeorbit(ilocation-1),time,xyz_mid,vel_mid)
  !print *,x,v
  intp_orbit=0
  return 

end function intp_orbit

!!$real*8 function norm2(x)
!!$  implicit none
!!$  real*8 x(3)
!!$  norm2=dsqrt(x(1)*x(1)+x(2)*x(2)+x(3)*x(3))
!!$  return
!!$end function norm2
