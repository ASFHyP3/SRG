!!!!!
!
!  backproject - back project sentinel burst to a regular grid defined by dem file
!
!
!!!!!

program backproject
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
  integer*4 burst,nlines,numstatevec,iaperture,ifdpts,firstpix,lastpix
  integer*8 db
  real*8 ptsperdempt, posting, deltalat,deltalon,firstlat,firstlon
  real*8 timeorbit(1000),xx(3,1000),vv(3,1000),aa(3,1000),x(3),v(3),a(3)
  real*8 timefirst,timeend,slantRangeTime,rangeSamplingRate,prf,startime(1000)
  real*8 satloc(3,100000),satvel(3,100000)
  real*8 :: lat, fd, ht, re, vmag, veff
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
     print *,'usage: backproject file_db slcoutfile az-steer-c0 -c1'
     stop
  end if

  call getarg(1,dbfile)
  call getarg(2,slcoutfile)
  call getarg(3,str)
  read(str,*)angc0
  call getarg(4,str)
  read(str,*)angc1

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
  !r0=r0+75    ! range offest calibration set by 30 m dem hawaii image
  slantRangeTime=2*r0/sol
  call get_paramd(db,table,'fs',rangeSamplingRate,units,type)
  call get_paramd(db,table,'azimuthTimeInterval',azimuthTimeInterval,units,type)
  call close_db(db)

  !c print *,'dbfile read'

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

!  print *,'DEM parameters:'
!  print '(2i10,2f12.5,2f12.7)',demwidth,demlength,firstlon,firstlat,deltalon,deltalat
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

  !c  read in position file
!  print *,'position file ',posfile
  open(21,file=posfile)
  do i=1,rawdatalines
     read(21,*,end=102)line,t(i),satloc(:,i),satvel(:,i)
  end do
102  close(21)

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
  allocate(demin(demwidth))
  allocate(outdata(demwidth))
  allocate(testdata(demwidth),testphase(demwidth))
  allocate(baddata(demwidth))
  allocate(origdata(demwidth))
!  allocate(burstdata(rawdatalines,samplesPerBurst))
  allocate(burstdata(samplesPerBurst,rawdatalines))
  print *,'Samples per burst, raw data lines: ',samplesPerBurst,rawdatalines

  baddata=cmplx(0.,0.)

  !c  set the size of the aperture and its centroid (10 m antenna)
  v=satvel(:,rawdatalines/2)
  vmag=dsqrt(dot_product(v,v))
  vmag=vmag*(re/(re+ht))
  aperture=prf*r0*wvl/10./vmag*2  !  fill in between bursts
  dmrg = sol/2.d0/rangeSamplingRate 
!!$  temp=fd*prf*prf*wvl*(r0+samplesPerBurst/2.*dmrg)/2.d0/vmag**2
!!$  iaperture=temp
!!$  iaperture=ifdpts
!!$  print *,'aperture pts, aperture and fd offset: ',aperture,iaperture,temp

  !  tops geometry params
!  ks=6826.
  d=10.
  apertureground=r0*wvl/d
!!$  aperturefd=apertureground*2*vmag/wvl/r0
!!$  aperturetime=apertureground*2*vmag/ks/wvl/r0
!!$  print *,'Apertures ground, fd, time: ',apertureground,aperturefd,aperturetime
  bursttimespan=rawdatalines/prf
!!$  burstpoints=aperturetime/bursttimespan*float(rawdatalines)*0.5
!!$  print *,'Burst time span, points: ',bursttimespan,burstpoints

  ! refined numbers for tops using both ks and velocity
  aperturetime=r0*wvl/d/(ks*wvl*r0/2/vmag+vmag)
  aperturetime=(r0*wvl/2/vmag/vmag*prf)/(1.+r0*angc1*pi/180./vmag)
  burstpoints=aperturetime*prf*0.5  !  50% of nominal antenna aperture
  print *,'Burst time span, points: ',bursttimespan,burstpoints


  !c loop over bursts
  open(30, file=slcoutfile,access='direct',recl=8*demwidth,form='unformatted')

  azimuthBursts=1
  do burst=1,azimuthBursts
     tstart = startime(burst)
     dtaz = 1.d0 / prf 
     tend  = tstart + (rawdatalines-1)* dtaz
     tmid = 0.5d0*(tstart+tend)

     !print *, 'Burst ',burst,', Start, stop Acquisition time: ', tstart,tend

     rngstart = slantRangeTime*sol/2.d0
     dmrg = sol/2.d0/rangeSamplingRate 
     rngend = rngstart + (samplesPerBurst-1)*dmrg
     rngmid = 0.5d0*(rngstart+rngend)
!!$     print *, 'Near Range in m: ', rngstart 
!!$     print *, 'Far  Range in m: ', rngend
!!$     print *, 'Range sample spacing in m: ', dmrg
!!$
!!$     length=linesPerBurst
!!$     width=samplesPerBurst
!!$     print *, 'Radar Image Burst Lines: ', length
!!$     print *, 'Radar Image Burst Width: ', width

     ! geolocation of the current burst extended for tops scan
     print '(a,4f14.5)','Burst times and ranges: ',tstart,tend,rngstart,rngend
     call bounds(tstart-2.,tend+2.,rngstart,rngend,timeorbit,xx,vv,numstatevec,latlons)

     !  get line loop bounds
     !print *,'latlons ',latlons
     !print *,'first lat, lon, deltalat, lon, demwidth,demlength ', &
!          firstlat,firstlon,deltalat,deltalon,demwidth,demlength
     firstpix=((latlons(3)-firstlon)/ deltalon)+1
     if (firstpix .lt. 0)firstpix=0
     lastpix=((latlons(4)-firstlon)/ deltalon)+1
     if (lastpix .gt. demwidth)lastpix=demwidth
     !print *,'pixel loop limits ',firstpix, lastpix

     !  read in rc data from burst
     open(23,file=rawinfile,access='direct',recl=rawdatalines*samplesPerBurst*8)
     read(23,rec=1)burstdata
     close(23)
     burstdata(1:10,:)=cmplx(0.,0.)

     ! range compress
!     call range_compress(dbfile,table,burstdata)

!!!!Initialize satellite position
     tline = tmid
     stat =  intp_orbit(timeorbit, xx, vv, numstatevec, tline, xyz_mid, vel_mid)
!!$     print *,'Location of satellite at middle of burst: ',xyz_mid
!!$     print *,'Velocity of satellite at middle of burst: ',vel_mid
!!$     print *,'Burst start, stop times, middle: ',startime(1),startime(1)+rawdatalines/prf,tmid

     if (stat.ne.0) then
        print *, 'Cannot interpolate orbits at the center of scene.'
        stop
     endif

     do line = 1, demlength
        !if(mod(line,500).eq.1)print *,'dem line ',line
        !!Initialize
        azoff = BAD_VALUE
        outdata=cmplx(0.,0.)

        lat=firstlat+(line-1)*deltalat
        do i=1,demwidth
           lon(i)=firstlon+(i-1)*deltalon
        end do
        ! if outside range don't bother with computations
        if(lat.ge.latlons(1))then
           if(lat.le.latlons(2))then

              !!Read in this line from DEM
              read(22,rec=line)demin
              if (mod(line,1000).eq.1) then
                 print *, 'Processing line: ', line
              endif

              !$OMP PARALLEL DO private(i_type)&
              !$OMP private(xyz,llh,rngpix,tline,satx,satv,cacc,veff)&
              !$OMP private(azoff,azline,range,r,intr,fracr,val,phase)&
              !$OMP private(unitlookvector,fd,td,naperture,napertureorig,frate)&
              !$OMP shared(lat,lon,demin,burstdata,fdc,firstpix,lastpix) &
              !$OMP shared(demwidth,outdata,samplesPerBurst) &
              !$OMP shared (timeorbit,xx,vv,numstatevec) &
              !$OMP shared(elp,rngstart,tstart,tend,rngend) &
              !$OMP shared(tmid,xyz_mid,vel_mid,t,ks,burstpoints) &
              !$OMP shared(dtaz,dmrg,deg2rad,testdata,angc0,angc1) &
              !$OMP shared(wvl,pi,rawdatalines,satloc,aperture,iaperture) 
              do pixel = firstpix,lastpix

                 llh(1) = lat * deg2rad
                 llh(2) = lon(pixel) * deg2rad
                 llh(3) = demin(pixel)
                 !print *,line,pixel,llh
                 i_type = LLH_2_XYZ
                 call latlon(elp,xyz,llh,i_type)
                 tline = tmid
                 satx = xyz_mid
                 satv = vel_mid
!                 sata = acc_mid
                 !! get the zero doppler location of the satellite
                 call orbitrangetime(xyz,timeorbit,xx,vv,numstatevec, &
                      tmid,satx,satv,tline,rngpix)
                 !print *,lat,lon(pixel),tline,rngpix
                 ! tops geometry calcs
                 unitlookvector=(xyz-satx)/dsqrt(dot_product(xyz-satx,xyz-satx))
                 fd=2.d0/wvl*dot_product(satv,unitlookvector)
                 !print '(a,4f12.3,2i8)','look vector, fd ',unitlookvector,fd,line,pixel
!                 fdc=-400.
                 frate=-2*dot_product(satv,satv)/wvl/rngpix
                 veff=sqrt(dot_product(satv,satv))
!                 print *,'frate ',frate
                 td=(fd+fdc)/(ks-frate)
                 td=(rngpix*wvl/2/veff/veff*fd-rngpix*angc0*pi/180./veff)/(1.+rngpix*angc1*pi/180./veff)
                 !napertureorig=(td-rawdatalines/prf/2.d0)*rawdatalines+rawdatalines/2  ! naperture is the center of the aperture in raw data azimuth
                 naperture=td*prf+rawdatalines/2
                 !naperture=napertureorig
                 !print *,'td, naperture: ',td,naperture

                 if(naperture.ge.burstpoints/2.and.naperture.le.rawdatalines-burstpoints/2)then
                 !print *,'naperture orig ',napertureorig,' naperture new  ',naperture,td,fd,prf
!!$                    azoff = ((tline - tstart)/dtaz) !- 1.0d0*(line-1)
!!$                    if(range.ge.rngstart.and.range.lt.rngend-dmrg-dmrg) &
!!$                         !write(99,*)lat,lon(pixel),tline,rngpix,azoff,aperture,range-rngstart
!!$                    print *,'zero dop ',tline,tstart,azoff,dtaz,rngpix,rngpix-rngstart
!!$                    print *,'sat t p r ',t(nint(azoff)),satloc(:,nint(azoff))
!!$                    print *,dsqrt(dot_product(xyz-satloc(:,nint(azoff)),xyz-satloc(:,nint(azoff))))
!!$                    
!!$                    phase=4.d0*pi/wvl*rngpix
!!$                    testdata(pixel)=cmplx(cos(phase),sin(phase))
!!$                    testphase(pixel)=phase
                    !print *,phase,cos(phase),sin(phase),testdata(pixel)
                    !! back project
                    cacc=cmplx(0.,0.)
!                    iaperture=0
                    !!do azline=nint(azoff-aperture/2)+iaperture,nint(azoff+aperture/2)+iaperture
!                    do azline=1,rawdatalines
                    !print *,naperture-burstpoints/2,naperture+burstpoints/2

                    do azline=naperture-burstpoints/2,naperture+burstpoints/2
                       if(azline.ge.1.and.azline.le.rawdatalines)then
                          range=dsqrt(dot_product(xyz-satloc(:,azline),xyz-satloc(:,azline)))
                          if(range.ge.rngstart.and.range.lt.rngend-dmrg-dmrg)then
                             r=(range-rngstart)/dmrg
                             intr=r
                             fracr=r-intr
                             val=burstdata(intr+1,azline)*(1-fracr)+burstdata(intr+2,azline)*fracr
                             !print *,azline,r,val
                             !!val=burstdata(azline,nint(r)+1)
                             phase=4.d0*pi/wvl*range
                             cacc=cacc+val*cmplx(cos(phase),sin(phase))
                             !print *,azline-azoff,range,range-rngstart,phase,4.d0*pi/wvl*(range-rngstart)
                             !write(99,*)azline-azoff,range,range-rngstart,phase,4.d0*pi/wvl*(range-rngstart)
                          end if
                       end if
                    end do
                    outdata(pixel)=cacc
                 end if
              enddo   ! end pixel loop
              !$OMP END PARALLEL DO

              read(30,rec=line)origdata
              do pixel=1,demwidth
                 if(cabs(outdata(pixel)).gt.1.e-18)origdata(pixel)=outdata(pixel)
              end do
              write(30,rec=line)origdata

           end if
        end if ! end if that checks if inside latitude line bounds

     end do ! line loop ends here

  enddo  !  end burst loop

  close(30)
!!$  close(31)
  deallocate(lon,demin)
!!$  deallocate(azoff,rgoff)
  deallocate(outdata)

  timer1 = secnds(timer0)
  print *, 'elapsed time = ',timer1,' seconds'
end program backproject

