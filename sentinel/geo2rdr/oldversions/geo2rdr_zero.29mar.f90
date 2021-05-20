!!!!!
!
!  geo2rdr_zero - resample a sentinel slc to a regular grid defined by a dem file
!     for zero Doppler slc geometry only
!
!     modified 22nov17 by hz to add optional specific input file
!
!!!!!

program geo2rdr_zero
  use sql_mod
  use omp_lib
  implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !! DECLARE LOCAL VARIABLES
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  character*300 str,slcinfile,slcoutfile,dbfile,demfile,demrscfile
  character*300 orbtimingfile,units,type,table
  real*8 norm2
  integer stat,cnt,intp_orbit
  integer*8 latAcc,lonAcc,hgtAcc
  integer*8 azAcc,rgAcc
  integer*8 azOffAcc,rgOffAcc
  integer*4 azimuthBursts,linesPerBurst,samplesPerBurst
  integer*4 firstValidLine(100), lastValidLine(100)

  integer*4 demwidth,demlength,idemwidth,idemlength,width,length
  integer*4 burst,nlines,numstatevec
  integer*8 db
  real*8 ptsperdempt, posting, deltalat,deltalon,firstlat,firstlon
  real*8 timeorbit(100),xx(3,100),vv(3,100),aa(3,100),x(3),v(3),a(3)
  real*8 timefirst,timeend,slantRangeTime,rangeSamplingRate,prf,startime(100)
  real*8, dimension(:),allocatable :: lat
  real*8, dimension(:),allocatable :: lon
  real*8, dimension(:),allocatable :: dem
  real*8, dimension(:),allocatable :: rgm
  real*8, dimension(:),allocatable :: azt
  real*8, dimension(:),allocatable :: rgoff
  real*8, dimension(:),allocatable :: azoff
  real*4, dimension(:),allocatable :: distance
  real*4, dimension(:),allocatable :: oldazt, oldrgm, oldazoff, oldrgoff
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
  integer :: pixel,line,ith,i

  integer :: i_type,k,conv
  real*8 :: dtaz, dmrg
  real*8 :: dopfact,fdop,fdopder
  real*8 :: fn, fnprime
  real*8 :: c1,c2

  integer :: numOutsideImage

  real*4 :: timer0, timer1  
  ! array to hold each burst
  complex*8, allocatable :: burstdata(:,:)
  complex*8, allocatable :: outdata(:), baddata(:),olddata(:)
  complex*8 :: complex1, complex2
  real*8 :: fraca, fracr, phase, wvl
  integer :: inta, intr

  ! variables associated with offset carrier removal
  character*300 schfile, fmratefile, dcfile, orbfile, offsetfile
  integer*4  nrange
  integer*4  npolyorb, npolyfm, npolydc, iburst, j, irec
  real*8  azimuthSteeringRate,azimuthTimeInterval,radarFrequency
  real*8  r_ro, r_ao, elev
  real*8  fmtime(100),fmt0(100),fmc0(100),fmc1(100),fmc2(100)
  real*8  dctime(100),dct0(100),dcc0(100),dcc1(100),dcc2(100)
  real*8  orbtime(100),vx(100),vy(100),vz(100)
  real*8  vs, ks, trange,vxintp,vyintp,vzintp
  real*8  fmc0intp,fmc1intp,fmc2intp,dcc0intp,dcc1intp,dcc2intp
  real*8  frac, timecenterseconds,rawdataprf
  real*8  fmtau0,dctau0,fmt0intp,dct0intp
  real*8  off
  real*8, allocatable :: testphase(:)
  real*8, allocatable :: eta(:),ka(:),kt(:),etac(:),fnc(:),etaref(:)
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

  if(iargc().lt.3)then
     print *,'usage: geo2rdr_zero file_db slcoutfile posting(pts per dem point) <input_file=db_slcfile>'
     stop
  end if

  call getarg(1,dbfile)
  call getarg(2,slcoutfile)
  call getarg(3,str)
  read(str,*)ptsperdempt

  !c  open the database
  call open_db(db,dbfile)
  table='file'
  !print *,trim(table)
  call get_parami(db,table,'azimuthBursts',azimuthBursts,units,type) ! burst params
  call get_parami(db,table,'linesPerBurst',linesPerBurst,units,type)
  call get_parami(db,table,'samplesPerBurst',samplesPerBurst,units,type)
  nrange=samplesPerBurst
  call get_paramd(db,table,'prf',prf,units,type)
  call get_paramd(db,table,'wvl',wvl,units,type)
  call get_paramc(db,table,'slc_file',slcinfile,units,type) ! input slc file
  !c  if input file specified use that file instead
  if(iargc().ge.4)then
     call getarg(4,slcinfile)
  end if
  !c  end specific input file
  call get_paramc(db,table,'orbinfo',orbtimingfile,units,type) ! orbit state vector file
  do burst=1,azimuthBursts
     if(burst.le.9)then
        call get_paramd(db,table,'azimuthTimeSeconds'//char(48+burst),startime(burst),units,type)
        call get_parami(db,table,'firstValidLine'//char(48+burst),firstValidLine(burst),units,type)
        call get_parami(db,table,'lastValidLine'//char(48+burst),lastValidLine(burst),units,type)
     else
        call get_paramd(db,table,'azimuthTimeSeconds'//'1'//char(48+burst-10),startime(burst),units,type)
        call get_parami(db,table,'firstValidLine'//'1'//char(48+burst-10),firstValidLine(burst),units,type)
        call get_parami(db,table,'lastValidLine'//'1'//char(48+burst-10),lastValidLine(burst),units,type)
     end if
  end do
  call get_paramd(db,table,'slantRangeTime',slantRangeTime,units,type)
  call get_paramd(db,table,'rangeSamplingRate',rangeSamplingRate,units,type)
  call get_paramd(db,table,'azimuthSteeringRate',azimuthSteeringRate,units,type)
  call get_paramd(db,table,'azimuthTimeInterval',azimuthTimeInterval,units,type)
  call get_paramd(db,table,'rawdataprf',rawdataprf,units,type)
  call get_paramd(db,table,'rangeSamplingRate',rangeSamplingRate,units,type)
  call get_paramd(db,table,'radarFrequency',radarFrequency,units,type)
  call get_paramd(db,table,'slantRangeTime',slantRangeTime,units,type)
  call get_paramc(db,table,'fmrateinfo',fmratefile,units,type)
  call get_paramc(db,table,'dcinfo',dcfile,units,type)
  call get_paramc(db,table,'orbinfo',orbfile,units,type)
  call close_db(db)

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

  !c  read in metadata for the offset carrier removal

  !c  allocate variable arrays for carrier
  allocate (eta(linesPerBurst))
  allocate (ka(nrange),kt(nrange),etac(nrange),fnc(nrange))
  allocate (cphase(nrange),etaref(nrange),dataline(nrange))
  allocate (testphase(nrange))

  !c  fm rate polynomials
  open(21,file=fmratefile)
  read(21,*)npolyfm
  do i=1,npolyfm
     read(21,*)fmtime(i),fmt0(i),fmc0(i),fmc1(i),fmc2(i)
  end do
  close(21)
  
  !c  doppler centroid polynomials
  open(21,file=dcfile)
  read(21,*)npolydc
  do i=1,npolydc
     read(21,*)dctime(i),dct0(i),dcc0(i),dcc1(i),dcc2(i)
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
  allocate(rgm(demwidth), oldrgm(demwidth))
  allocate(azt(demwidth), oldazt(demwidth))
  allocate(rgoff(demwidth), oldrgoff(demwidth))
  allocate(azoff(demwidth), oldazoff(demwidth))
  allocate(distance(demwidth))
  allocate(demin(demwidth))
  ! allocate output data array based on dem size
  allocate(outdata(demwidth))
  allocate(baddata(demwidth))
  allocate(olddata(demwidth))
  allocate(burstdata(samplesPerBurst,linesPerBurst))

  baddata=cmplx(0.,0.)

  open(31, file='range',access='direct',recl=4*demwidth,form='unformatted')
  open(32, file='azimuth',access='direct',recl=4*demwidth,form='unformatted')
  open(33, file='rangeoff',access='direct',recl=4*demwidth,form='unformatted')
  open(34, file='azimuthoff',access='direct',recl=4*demwidth,form='unformatted')
  open(30, file=slcoutfile,access='direct',recl=8*demwidth,form='unformatted')

  do line = 1, demlength
     write(30,rec=line)baddata  ! initialize output file
  end do

  !c loop over bursts
  do burst=1,azimuthBursts
     tstart = startime(burst)
     dtaz = 1.d0 / prf ! Nazlooks / prf
     tend  = tstart + (linesPerBurst-1)* dtaz
     tmid = 0.5d0*(tstart+tend)

     print *, 'Burst ',burst,', Start, stop Acquisition time: ', tstart,tend
     !     print *, 'Stop Acquisition time: ', tend
     !print *, 'Azimuth line spacing in secs: ', dtaz

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

     !c  compute ks,vs
     do i=1,numstatevec-1
        if (timeorbit(i).le.tmid) then
           if (timeorbit(i+1).ge.tmid) then
              frac=(tmid-timeorbit(i))/(timeorbit(i+1)-timeorbit(i))
              vxintp=vv(1,i)+frac*(vv(1,i+1)-vv(1,i))
              vyintp=vv(2,i)+frac*(vv(2,i+1)-vv(2,i))
              vzintp=vv(3,i)+frac*(vv(3,i+1)-vv(3,i))
           end if
        end if
     end do
     vs=sqrt(vxintp**2+vyintp**2+vzintp**2)
     ks=2.d0*vs*radarFrequency/299792458.d0*azimuthSteeringRate*pi/180.d0

     !c  fm rate polynomial for this burst
     do i=1,npolyfm-1
        if (fmtime(i).le.tmid) then
           if (fmtime(i+1).ge.tmid) then
              frac=(tmid-fmtime(i))/(fmtime(i+1)-fmtime(i))
              fmc0intp=fmc0(i)+frac*(fmc0(i+1)-fmc0(i))
              fmc1intp=fmc1(i)+frac*(fmc1(i+1)-fmc1(i))
              fmc2intp=fmc2(i)+frac*(fmc2(i+1)-fmc2(i))
              fmt0intp=fmt0(i)+frac*(fmt0(i+1)-fmt0(i))
           end if
        end if
     end do
     if (tmid.gt.fmtime(npolyfm))then
        i=npolyfm-1
        frac=(tmid-fmtime(i))/(fmtime(i+1)-fmtime(i))
        fmc0intp=fmc0(i)+frac*(fmc0(i+1)-fmc0(i))
        fmc1intp=fmc1(i)+frac*(fmc1(i+1)-fmc1(i))
        fmc2intp=fmc2(i)+frac*(fmc2(i+1)-fmc2(i))
        fmt0intp=fmt0(i)+frac*(fmt0(i+1)-fmt0(i))
     end if

     !c  doppler centroid estimate for this burst
     if (dctime(1).gt.timecenterseconds) then
        dcc0intp=dcc0(1)
        dcc1intp=dcc1(1)
        dcc2intp=dcc2(1)
     end if
     
     do i=1,npolydc-1
        !            print *,i,dctime(i),dctime(i+1),timecenterseconds
        if (dctime(i).le.tmid) then
           if (dctime(i+1).ge.tmid) then
              frac=(tmid-dctime(i))/(dctime(i+1)-dctime(i))
              dcc0intp=dcc0(i)+frac*(dcc0(i+1)-dcc0(i))
              dcc1intp=dcc1(i)+frac*(dcc1(i+1)-dcc1(i))
              dcc2intp=dcc2(i)+frac*(dcc2(i+1)-dcc2(i))
              dct0intp=dct0(i)+frac*(dct0(i+1)-dct0(i))
           end if
        end if
     end do

     !c  define azimuth time array
     do i=1,linesPerBurst
        eta(i)=-linesPerBurst*azimuthTimeInterval/2.+(i-1)*azimuthTimeInterval
     end do
     !c  now range dependent arrays
     fmtau0=fmt0intp
     dctau0=dct0intp

     do i=1,nrange
        trange=slantRangeTime+(i-1)/rangeSamplingRate
        ka(i)=fmc0intp+fmc1intp*(trange-fmtau0)+fmc2intp*(trange-fmtau0)*(trange-fmtau0)
        fnc(i)=dcc0intp+dcc1intp*(trange-dctau0)+dcc2intp*(trange-dctau0)*(trange-dctau0)
        kt(i)=ka(i)*ks/(ka(i)-ks)
        etac(i)=-fnc(i)/ka(i)
        etaref(i)=etac(i)-etac(1)
     end do

!!$     print *,'Burst number: ',iburst
!!$     print *,'ks ',ks
!!$     print *,'vs ',vs
!!$     print *,'eta ',eta(1),eta(linesPerBurst)
!!$     print *,'ka ',ka(1),ka(nrange)
!!$     print *,'kt ',kt(1),kt(nrange)
!!$     print *,'fnc ',fnc(1),fnc(nrange)
!!$     print *,'etac ',etac(1),etac(nrange)
!!$     print *,'etaref ',etaref(1),etaref(nrange)
!!$     print *,'fm coeffs ',fmc0intp,fmc1intp,fmc2intp
!!$     print *,'dc coeffs ',dcc0intp,dcc1intp,dcc2intp
!!$     print *,'center time ',tmid
!!$     print *,'azimuth time ',eta(1),eta(linesPerBurst)

     ! read in the burst
     open(21,file=slcinfile,access='direct',recl=8*samplesPerBurst*linesPerBurst)
     read(21,rec=burst)burstdata
     close(21)

     ! geolocation of the current burst
     call bounds(tstart,tend,rngstart,rngend,timeorbit,xx,vv,latlons)
!!$     print *,'Min, max lat: ',latlons(1),latlons(2)
!!$     print *,'Min, max lon: ',latlons(3),latlons(4)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     !! PROCESSING STEPS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!$     print *, 'Geocoded Lines:  ', demlength
!!$     print *, 'Geocoded Samples:', demwidth 


!!!!Initialize satellite positions
     tline = tmid
     stat =  intp_orbit(timeorbit, xx, vv, tline, xyz_mid, vel_mid)

     if (stat.ne.0) then
        print *, 'Cannot interpolate orbits at the center of scene.'
        stop
     endif
     ! print *,'Satellite midpoint time,position,velocity: ',tline,xyz_mid,vel_mid

!!$     print *, "geo2rdr on ",ith,' threads...'

     numOutsideImage = 0

     do line = 1, demlength
        !!Initialize
        azt = BAD_VALUE
        rgm = BAD_VALUE
        rgoff = BAD_VALUE
        azoff = BAD_VALUE
        distance = BAD_VALUE

        !if(burst.eq.1)write(30,rec=line)baddata  ! initialize output file during first burst

        lat=firstlat+(line-1)*deltalat
        do i=1,demwidth
           lon(i)=firstlon+(i-1)*deltalon
        end do

        ! if outside range dont bother with computations
        !print *,line,lat(1),latlons(1),latlons(2)
        if(lat(1).ge.latlons(1))then
           if(lat(1).le.latlons(2))then

              !!Read in this line from DEM
              read(22,rec=line)demin
              dem=demin

              if (mod(line,1000).eq.1) then
                 print *, 'Processing line: ', line, numoutsideimage
              endif
              conv = 0

              !$OMP PARALLEL DO private(pixel,i_type,k)&
              !$OMP private(xyz,llh,rngpix,tline,satx,satv)&
              !$OMP private(c1,c2,tprev,dr,stat,fn,fnprime)&
              !$OMP private(dopfact,fdop,fdopder,sata) &
              !$OMP shared(length,width,demwidth) &
              !$OMP shared(rgm,azt,rgoff,azoff) &
              !$OMP shared(line,elp,ilrl,tstart,tmid,rngstart,rngmid) &
              !$OMP shared(xyz_mid,vel_mid,acc_mid,fdvsrng,fddotvsrng) &
              !$OMP shared(lat,lon,dem,dtaz,dmrg,deg2rad,bistatic,sol) &
              !$OMP shared(numOutsideImage,wvl,orbit,conv,distance) 
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
                    rngpix = norm2(dr)    

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
                    if (abs(tline - tprev).lt.5.0d-9) then
                       conv = conv + 1
                       exit
                    endif
                 enddo


                 if(tline.lt.tstart) then
                    numOutsideImage = numOutsideImage + 1
                    goto 100
                 endif

                 if(tline.gt.tend) then
                    numOutsideImage = numOutsideImage + 1
                    goto 100
                 endif

                 dr = xyz - satx
                 rngpix = norm2(dr)

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
                 distance(pixel) = tline - tprev

100              continue
              enddo   ! end pixel loop

              !$OMP END PARALLEL DO
!!$     write(31,rec=line)sngl(rgm)  !  range in meters to output pixel
!!$     write(32,rec=line)sngl(azt)  !  time in seconds to output pixel
!!$     write(33,rec=line)sngl(rgoff)  ! range offset in pixels for resampling
!!$     write(34,rec=line)sngl(azoff)  ! azimuth offset in pixels for resampling
              rgoff=max(rgoff,1.d0)
              rgoff=min(rgoff,samplesPerBurst-1.d0)
              azoff=max(azoff,1.d0)
              azoff=min(azoff,linesPerBurst-1.d0)
!!$              azoff=max(azoff,dble(firstValidLine(burst)))!1.d0)
!!$              azoff=min(azoff,dble(lastValidLine(burst)))!linesPerBurst-1.d0)
              outdata=cmplx(0.,0.)
              do i=1,demwidth
                 intr=rgoff(i)
                 fracr=rgoff(i)-intr
                 inta=azoff(i)
                 fraca=azoff(i)-inta
                 if(inta.gt.firstValidLine(burst))then ! skip if data not valid
                    if(inta.lt.lastValidLine(burst)-1)then
                       ! interpolate in range first, then azimuth bilinearly
                       complex1=burstdata(intr,inta)*(1-fracr)+burstdata(intr+1,inta)*fracr
                       complex2=burstdata(intr,inta+1)*(1-fracr)+burstdata(intr+1,inta+1)*fracr
                       outdata(i)=complex1*(1-fraca)+complex2*fraca
                       !outdata(i)=burstdata(nint(rgoff(i)),nint(azoff(i))) ! nearest neighbor
                       ! remove range progation phase
                       phase=4.d0*pi/wvl*rgm(i)
                       outdata(i)=outdata(i)*cmplx(cos(phase),sin(phase))
                    end if
                 end if
                 ! now compute the offset carrier phase, remove it
!!$                 off=azt(i)
!!$                 phase=-pi*kt(nint(rgoff(i)))*(-off**2-2*off*eta(nint(azoff(i)))+2*off*etaref(nint(rgoff(i))))
!!$                 outdata(i)=outdata(i)*cmplx(cos(phase),-sin(phase))
              end do

              !c  now apply the offset carrier removal term in absolute offset
!!$              do j=1,nrange
!!$                 !c  azimuth offsets for this location
!!$               r_ao = azcoef(1) + (i-1)*(azcoef(3) + &
!!$                 (i-1)*(azcoef(6) + (i-1)*azcoef(10))) + &
!!$                 (j-1)*(azcoef(2) + (j-1)*(azcoef(5) + &
!!$                 (j-1)*azcoef(9))) + &
!!$                 (j-1)*(i-1)*(azcoef(4) + (i-1)*azcoef(7) + &
!!$                 (j-1)*azcoef(8))
!!$               off=r_ao*azimuthTimeInterval
!!$               phase(j)=-pi*kt(j)*(-off**2-2*off*eta(i)+2*off*etaref(j))
!!$            end do
!!$
!!$            cphase=cmplx(sngl(cos(phase)),sngl(sin(phase)))
!!$            dataline=dataline*cphase


              ! only overwrite with nonzeros
              read(30,rec=line)olddata
              oldrgm=0.
              oldazt=0.
              oldrgm=0.
              oldazt=0.
              read(31,rec=line,err=101)oldrgm
              read(32,rec=line,err=101)oldazt
              read(33,rec=line,err=101)oldrgoff
              read(34,rec=line,err=101)oldazoff
101           continue
              do i=1,demwidth
                 if(cabs(outdata(i)).gt.1.e-5)then
                    olddata(i)=outdata(i)
                    oldazt(i)=azt(i)
                    oldrgm(i)=rgm(i)
                    oldrgoff(i)=rgoff(i)
                    oldazoff(i)=azoff(i)
                 end if
              end do
              write(30,rec=line)olddata
              write(31,rec=line)oldrgm
              write(32,rec=line)oldazt
              write(33,rec=line)oldrgoff
              write(34,rec=line)oldazoff

           end if
        end if ! end if that checks if inside latitude line bounds

     end do ! line loop ends here

     print *, 'Number of pixels outside the image: ', numOutsideImage
     print *, 'Number of pixels with valid data:   ', cnt
     print *, 'Number of pixels that converged:    ', conv

  enddo  !  end burst loop

  deallocate(lat,lon,dem)
  deallocate(azt,rgm)
  deallocate(azoff,rgoff)
  deallocate(distance)
  deallocate(outdata)

  timer1 = secnds(timer0)
  print *, 'elapsed time = ',timer1,' seconds'
end program geo2rdr_zero

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

real*8 function norm2(x)
  implicit none
  real*8 x(3)
  norm2=dsqrt(x(1)*x(1)+x(2)*x(2)+x(3)*x(3))
  return
end function norm2
