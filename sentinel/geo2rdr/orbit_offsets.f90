!!!!!
!
! orbit_offsets - estimate offsets in time and range from orbit solutions
!
!!!!!

program orbit_offsets
  use sql_mod
!  use omp_lib
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
  real*8, dimension(:),allocatable :: dem
  real*8, dimension(:),allocatable :: rgm, rgmslave
  real*8, dimension(:),allocatable :: azt, aztslave
  real*8, dimension(:),allocatable :: rgoff,rgoffslave
  real*8, dimension(:),allocatable :: azoff,azoffslave
  real*8 azcoeffs(10),rancoeffs(10)
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

  if(iargc().lt.2)then
     print *,'usage: orbit_offsets master_db slave_db'
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
  open(20,file='extrashifts')
  do i=1,azimuthbursts
     extrashift(i)=(startime(i)-startimeslave(i)-startime(1)+startimeslave(1))/dtaz
     print *,i,extrashift(i),nint(extrashift(i))
     write(20,*)i,nint(extrashift(i)),dtaz
  end do
  close(20)

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

!!$  print *,'DEM parameters:'
!!$  print *,demwidth,demlength,firstlon,firstlat,deltalon,deltalat
!!$  print *
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
  allocate(lat(demwidth))
  allocate(lon(demwidth))
  allocate(dem(demwidth))
  allocate(rgm(9), rgmslave(9))
  allocate(azt(9), aztslave(9))
  allocate(rgoff(9), rgoffslave(9))
  allocate(azoff(9), azoffslave(9))
  allocate(demin(demwidth))

!c  save bursts in file 'burstoffsets'
  open(31,file='burstoffsets')
  open(32,file='burstoffsetsrange')

  !c loop over bursts
  do k=1,azimuthBursts
     burst=k
     ! master first
     tstart = startime(burst)
     dtaz = 1.d0 / prf ! Nazlooks / prf
     tend  = tstart + (linesPerBurst-1)* dtaz
     tmid = 0.5d0*(tstart+tend)

     rngstart = slantRangeTime*sol/2.d0
     dmrg = sol/2.d0/rangeSamplingRate !Nrnglooks * drho
     rngend = rngstart + (samplesPerBurst-1)*dmrg
     rngmid = 0.5d0*(rngstart+rngend)

     ! slave orbit timing
     tstartslave = startimeslave(burst)
     dtazslave = 1.d0 / prfslave ! Nazlooks / prf
     tendslave  = tstartslave + (linesPerBurstSlave-1)* dtazslave
     tmidslave = 0.5d0*(tstartslave+tendslave)

     rngstartslave = slantRangeTimeSlave*sol/2.d0
     dmrgslave = sol/2.d0/rangeSamplingRateSlave !Nrnglooks * drho
     rngendslave = rngstartslave + (samplesPerBurstSlave-1)*dmrgslave
     rngmidslave = 0.5d0*(rngstartslave+rngendslave)

     print *,'                 Master       Slave ',burst
!!$     print *,'Time start  ',tstart,tstartslave
!!$     print *,'Time mid    ',tmid,tmidslave
!!$     print *,'Time end    ',tend,tendslave
!!$     print *,'Range start ',rngstart,rngstartslave
!!$     print *,'Range mid   ',rngmid,rngmidslave
!!$     print *,'Range end   ',rngend,rngendslave
!!$     print *

     ! geolocation of the current burst from master
     call bounds(tstart,tend,rngstart,rngend,timeorbit,xx,vv,numstatevec,latlons)
     !print *,'bounds of burst: ',latlons


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     !! PROCESSING STEPS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!Initialize satellite positions
     tline = tmid
!!$     print *,'master ',tline
!!$     print *,(timeorbit(kk),kk=1,numstatevec)
     stat =  intp_orbit(timeorbit, xx, vv, numstatevec, tline, xyz_mid, vel_mid)
     if (stat.ne.0) then
        print *, 'Cannot interpolate orbits at the center of scene.'
        stop
     endif
!!$     print '(a,f10.2,3f12.2,3f12.4)','Sat midpoint t,x,v:       ',tline,xyz_mid,vel_mid

     tline = tmidslave
!!$     print *,'slave ',tline
!!$     do kk=1,numstatevecslave
!!$        print '(f10.2,3f12.2,3f12.4)',timeorbitslave(kk),xxslave(:,kk),vvslave(:,kk)
!!$     end do
     stat =  intp_orbit(timeorbitslave, xxslave, vvslave, numstatevecslave, tline, xyz_midslave, vel_midslave)
     if (stat.ne.0) then
        print *, 'Cannot interpolate orbits at the center of scene.'
        stop
     endif
!!$     print '(a,f10.2,3f12.2,3f12.4)','Slave sat midpoint t,x,v: ',tline,xyz_midslave,vel_midslave

!  create a list of lat/lon points at corners of burst
     cornerlat(1)=latlons(1)
     cornerlon(1)=latlons(3)
     cornerlat(2)=latlons(2)
     cornerlon(2)=latlons(3)
     cornerlat(3)=latlons(1)
     cornerlon(3)=latlons(4)
     cornerlat(4)=latlons(2)
     cornerlon(4)=latlons(4)
!  add in intermediate points
     cornerlat(5)=(cornerlat(1)+cornerlat(2))/2.d0
     cornerlon(5)=(cornerlon(1)+cornerlon(2))/2.d0
     cornerlat(6)=(cornerlat(2)+cornerlat(3))/2.d0
     cornerlon(6)=(cornerlon(2)+cornerlon(3))/2.d0
     cornerlat(7)=(cornerlat(3)+cornerlat(4))/2.d0
     cornerlon(7)=(cornerlon(3)+cornerlon(4))/2.d0
     cornerlat(8)=(cornerlat(4)+cornerlat(2))/2.d0
     cornerlon(8)=(cornerlon(4)+cornerlon(2))/2.d0
     cornerlat(9)=(cornerlat(1)+cornerlat(3))/2.d0
     cornerlon(9)=(cornerlon(1)+cornerlon(3))/2.d0

     do i=1,9
        cornerlat(i)=max(min(cornerlat(i),firstlat),firstlat+(demlength-1)*deltalat)
        cornerlon(i)=min(max(cornerlon(i),firstlon),firstlon+(demwidth-1)*deltalon)
     end do
!!$     print *,'latitudes ',cornerlat
!!$     print *,'longitudes',cornerlon

! loop over the corner points and get range and time for each
     do i=1,9
        line=nint((cornerlat(i)-firstlat)/deltalat)+1
        !print *,'corner number, line in dem ',i,line

        !!Read in this line from DEM
        read(22,rec=line)demin
        dem=demin
!!$        print *,'line ',line,' read from dem'

!!$        conv = 0
!!$        numiter=0
!c  pixel locations in the master file
        pixel=nint((cornerlon(i)-firstlon)/deltalon)+1
        !print *,'corner number, pixel in dem ',i,pixel

        llh(1) = cornerlat(i) * deg2rad
        llh(2) = cornerlon(i) * deg2rad
        llh(3) = dem(pixel)
!!$        print *,'llh of corner ',llh(1)/deg2rad,llh(2)/deg2rad,llh(3)
!!$        print *,tmid,xyz_mid,vel_mid

        i_type = LLH_2_XYZ
        call latlon(elp,xyz,llh,i_type)

        call orbitrangetime(xyz,timeorbit,xx,vv,numstatevec,tmid,xyz_mid,vel_mid,tline,rngpix)
!!$        print *,'Master: ',tline,rngpix
        rgm(i) = rngpix
        azt(i) = tline

        rgoff(i) = ((rngpix - rngstart)/dmrg) !- 1.0d0*(pixel-1)
        azoff(i) = ((tline - tstart)/dtaz) !- 1.0d0*(line-1)

!c   repeat for slave positions
!        print '(f10.3,3f13.2,3f12.5)',tmidslave,xyz_midslave,vel_midslave
        call orbitrangetime(xyz,timeorbitslave,xxslave,vvslave,numstatevecslave,tmidslave,  &
             xyz_midSlave,vel_midSlave,tline,rngpix)
!!$        print *,'Slave:  ',tline,rngpix

        rgmslave(i) = rngpix
        aztslave(i) = tline
        
        rgoffslave(i) = ((rngpix - rngstartslave)/dmrgslave) 
        azoffslave(i) = ((tline - tstartslave)/dtazslave) 

!!$     print *,'Burst, corner, d range, time ', &
!!$          burst,i,rgm(i)-rgmslave(i),azt(i)-aztslave(i), &
!!$          (rgm(i)-rgmslave(i))/dmrg,(azt(i)-aztslave(i))/dtaz
!!$     print *,'rgm rgmslave azt aztslave ',rgm(i),rgmslave(i),azt(i),aztslave(i)

     end do ! corner loop ends here
     !  starting time delta for master and slave bursts
!!$     print *,'tstart tstartslave delta ',tstart,tstartslave,tstartslave-tstart

     !  fit azimuth offset in time to twisted 2nd order plane across burst
     call azoffsets(rgm,rgmslave,azt,aztslave,rgoffslave,azoffslave, &
          samplesPerBurst,linesPerBurst,azcoeffs,rancoeffs)

     !  subtract off starting time difference
!!$     print *,'azcoef 1, tstartslave,tstart,tstartslave-tstart ',azcoeffs(1),tstartslave,tstart,tstartslave-tstart
     azcoeffs(1)=azcoeffs(1)-(tstartslave-tstart)+extrashift(i)*dtaz
     write(31,*)burst,azcoeffs
     write(32,*)burst,rancoeffs

  enddo  !  end burst loop

  close(31)
  close(32)

  deallocate(lat,lon,dem)
  deallocate(azt,rgm,aztslave,rgmslave)
  deallocate(azoff,rgoff,azoffslave,rgoffslave)

  timer1 = secnds(timer0)
  print *, 'elapsed time = ',timer1,' seconds'
end program orbit_offsets

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

!!$real*8 function norm2(x)
!!$  implicit none
!!$  real*8 x(3)
!!$  norm2=dsqrt(x(1)*x(1)+x(2)*x(2)+x(3)*x(3))
!!$  return
!!$end function norm2


subroutine azoffsets(rgm,rgmslave,azt,aztslave,rgoffslave,azoffslave, &
     samplesPerBurst,linesPerBurst,azcoeffs,rancoeffs)

!c - do fits and save azimuth offsets

      implicit none
!c PARAMETER STATEMENTS:

      integer NPP,MP
      parameter (NPP=10, MP=9)

!      integer NP, NAZMAX, N_OVER, NBMAX, NLINESMAX

!c LOCAL VARIABLES:

      real*8 rgm(MP),rgmslave(MP),azt(MP),aztslave(MP),azcoeffs(NPP),rancoeffs(NPP)
      real*8 rgoffslave(MP),azoffslave(MP)
      integer samplesPerBurst, linesPerBurst

      integer ierr, istats, l1, l2, lr, lc, line, iargc, iflatten
      integer ist, istoff, iaz, npl, npl2, nplo, nr, naz, nl, i_numpnts
      integer ibs, ibe, irec, i_a1, i_r1, jrec, jrecp
      integer i, j, k, ii, ix, nb
      integer i_na, ibfcnt,i_ma, int_az_off, iprint
      integer ipl

      real*8 amean, slr, azsum, azoff1, r_st, rd, azs

      real*8 r_ranpos(MP),r_azpos(MP),r_sig(MP),r_ranoff(MP)
      real*8 r_azoff(MP),r_rancoef(NPP),r_azcoef(NPP)
      real*8 r_v(NPP,NPP),r_u(MP,NPP),r_w(NPP),r_chisq

      real*8 r_azcorner,r_racorner,r_ao

!c COMMON BLOCKS:

      integer i_fitparam(NPP),i_coef(NPP)
      common /fred/ i_fitparam,i_coef

!c FUNCTION STATEMENTS:

      external funcs

!c SAVE STATEMENTS:

!!$      print *,rgm
!!$      print *,rgmslave
!!$      print *,azt
!!$      print *,aztslave
!!$      print *,rgoffslave
!!$      print *,azoffslave
!!$      print *,samplesPerBurst, linesPerBurst

!c  let's default to 6 coefficients for now
      i_ma=6

!c  set offsets at corners 
      do i=1,MP
         r_ranpos(i) = rgoffslave(i) !float(lr)
         r_azpos(i) = azoffslave(i) !float(iaz)
         r_azoff(i) = aztslave(i)-azt(i) !r_at
         r_ranoff(i) = rgmslave(i)-rgm(i) !r_rt
         r_sig(i) = 1.0 ! + 1.d0/r_st
      end do
      i_numpnts=MP

!!$      write(6,*) ' '
!!$      write(6,*) 'Finished setting offsets...'
!!$      write(6,*) 'Number of points read = ',i_numpnts
      !write(6,*) 'Number of points allowed = ',MP

!c make two two dimensional quadratic fits for the offset fields
!c one of the azimuth offsets and the other for the range offsets

      do i = 1 , NPP
         r_rancoef(i) = 0.
         r_azcoef(i) = 0.
         i_coef(i) = 0
      end do

      do i=1,i_ma
         i_coef(i) = i
      enddo

!c azimuth offsets as a function range and azimuth

!!$      print *,'ranpos ',r_ranpos
!!$      print *,'azpos  ',r_azpos
!!$      print *,'azoff  ',r_azoff
!!$      print *,MP,NPP,i_numpnts
      call svdfit(r_ranpos,r_azpos,r_azoff,r_sig,i_numpnts, &
          r_azcoef,i_ma,r_u,r_v,r_w,MP,NPP,r_chisq)
      call svdfit(r_ranpos,r_azpos,r_ranoff,r_sig,i_numpnts, &
          r_rancoef,i_ma,r_u,r_v,r_w,MP,NPP,r_chisq)
!c      print *,'azcoef ',r_azcoef

!c limits of resampling offsets
       ist=1
       npl=samplesPerBurst
       nl=linesPerBurst
       ipl=1
      do i=1,4
         if(i.eq.1)then
            r_azcorner=ist
            r_racorner=ipl
         end if
         if(i.eq.2)then
            r_azcorner=ist
            r_racorner=npl-1
         end if
         if(i.eq.3)then
            r_azcorner=ist+nl
            r_racorner=ipl
         end if
         if(i.eq.4)then
            r_azcorner=ist+nl
            r_racorner=npl-1
         end if
         r_ao = r_azcoef(1) + r_azcorner*(r_azcoef(3) + &
             r_azcorner*(r_azcoef(6) + r_azcorner*r_azcoef(10))) + &
             r_racorner*(r_azcoef(2) + r_racorner*(r_azcoef(5) + &
             r_racorner*r_azcoef(9))) + &
             r_racorner*r_azcorner*(r_azcoef(4) + r_azcorner*r_azcoef(7) + &
             r_racorner*r_azcoef(8))
         if(i.eq.1)print *,'Upper left offsets, approx: ',r_ao
         if(i.eq.2)print *,'Upper right offsets, approx:',r_ao
         if(i.eq.3)print *,'Lower left offsets, approx: ',r_ao
         if(i.eq.4)print *,'Lower right offsets, approx:',r_ao
       enddo

       azcoeffs=r_azcoef
       rancoeffs=r_rancoef
!c  write fits
       open(41,file='azcoeffs.txt')
       write(41,*)r_azcoef(:)
       close(41)

       open(41,file='rancoeffs.txt')
       write(41,*)r_rancoef(:)
       close(41)
       return
     end subroutine azoffsets
      

      subroutine funcs(x,y,afunc,ma)
      
      real*8 afunc(ma),x,y
      real*8 cf(10)
      integer i_fitparam(10),i_coef(10)
      
      common /fred/ i_fitparam,i_coef
      
      data cf /10*0./
      
      do i=1,ma
         cf(i_coef(i))=1.
         afunc(i) = cf(1) + x*(cf(2) + x*(cf(5) + x*cf(9))) + &
           y*(cf(3) + y*(cf(6) + y*cf(10))) + x*y*(cf(4) + y*cf(7) + x*cf(8))
         cf(i_coef(i))=0.
      end do
      
      return
      end

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
