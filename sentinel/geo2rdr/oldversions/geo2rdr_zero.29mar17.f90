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

  ! get starting time
  timer0 = secnds(0.0)
  cnt = 0

!!$  !$OMP PARALLEL
!!$  !$OMP MASTER
!!$  ith = omp_get_num_threads() !total num threads
!!$  !$OMP END MASTER
!!$  !$OMP END PARALLEL
!!$  print *, "threads",ith

  ! allocate
  allocate(lat(demlength))
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
     print *, 'Near Range in m: ', rngstart 
     print *, 'Far  Range in m: ', rngend
     print *, 'Range sample spacing in m: ', dmrg

     length=linesPerBurst
     width=samplesPerBurst
     print *, 'Radar Image Burst Lines: ', length
     print *, 'Radar Image Burst Width: ', width


     ! read in the burst
     open(21,file=slcinfile,access='direct',recl=8*samplesPerBurst*linesPerBurst)
     read(21,rec=burst)burstdata
     close(21)

     ! geolocation of the current burst
     call bounds(tstart,tend,rngstart,rngend,timeorbit,xx,vv,numstatevec,latlons)
!!$     print *,'Min, max lat: ',latlons(1),latlons(2)
!!$     print *,'Min, max lon: ',latlons(3),latlons(4)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     !! PROCESSING STEPS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!$     print *, 'Geocoded Lines:  ', demlength
!!$     print *, 'Geocoded Samples:', demwidth 


!!!!Initialize satellite position
     tline = tmid
     stat =  intp_orbit(timeorbit, xx, vv, numstatevec, tline, xyz_mid, vel_mid)

     if (stat.ne.0) then
        print *, 'Cannot interpolate orbits at the center of scene.'
        stop
     endif
     print *,'Satellite midpoint time,position,velocity: ',tline,xyz_mid,vel_mid

     do line = 1, demlength
        !!Initialize
        azt = BAD_VALUE
        rgm = BAD_VALUE
        rgoff = BAD_VALUE
        azoff = BAD_VALUE
        distance = BAD_VALUE

        !if(burst.eq.1)write(30,rec=line)baddata  ! initialize output file during first burst

        lat(line)=firstlat+(line-1)*deltalat
        do i=1,demwidth
           lon(i)=firstlon+(i-1)*deltalon
        end do

        ! if outside range dont bother with computations
        !print *,line,lat(1),latlons(1),latlons(2)
        if(lat(line).ge.latlons(1))then
           if(lat(line).le.latlons(2))then

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

                 llh(1) = lat(line) * deg2rad
                 llh(2) = lon(pixel) * deg2rad
                 llh(3) = dem(pixel)
                 print *,line,pixel,llh

                 i_type = LLH_2_XYZ
                 call latlon(elp,xyz,llh,i_type)

                 tline = tmid
                 satx = xyz_mid
                 satv = vel_mid
                 sata = acc_mid

                 call orbitrangetime(xyz,timeorbit,xx,vv,numstatevec, &
                      tmid,satx,satv,tline,rngpix)
                 print *,xyz,tline,rngpix

                 rgm(pixel) = rngpix
                 azt(pixel) = tline

                 rgoff(pixel) = ((rngpix - rngstart)/dmrg) !- 1.0d0*(pixel-1)
                 azoff(pixel) = ((tline - tstart)/dtaz) !- 1.0d0*(line-1)
                 distance(pixel) = tline - tprev

100              continue
              enddo   ! end pixel loop

              !$OMP END PARALLEL DO

              call resample2grid(rgoff,azoff,rgm,burstdata,demwidth, &
                   samplesPerBurst,linesPerburst,linesPerBurst, &
                   firstValidLine(burst),lastValidLine(burst),wvl,1,outdata)

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

real*8 function norm2(x)
  implicit none
  real*8 x(3)
  norm2=dsqrt(x(1)*x(1)+x(2)*x(2)+x(3)*x(3))
  return
end function norm2
