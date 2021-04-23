!!!!!
!
!  orbitstats -- get some orbit descriptors
!
!
!!!!!

program orbitstats
  use sql_mod
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
  real*8 :: lat, fd, ht, re, vmag, heading, headingdeg
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
     print *,'usage: orbitstats orbtimingfile '
     stop
  end if

  call getarg(1,orbtimingfile)

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

! look at burst 5 as it's in the middle
     burst=5
     dbfile='db.burst5'
     print *,trim(dbfile)
     !c  open the database
     call open_db(db,trim(dbfile))
     table='file'
     !call get_parami(db,table,'number_of_az_lines',linesPerBurst,units,type)
     call get_parami(db,table,'number_of_range_bins',samplesPerBurst,units,type)
     print *,'samps per burst: ',samplesPerBurst
     call get_paramd(db,table,'prf',prf,units,type)
     call get_paramd(db,table,'wvl',wvl,units,type)
     call get_paramd(db,table,'re',re,units,type)
     call get_paramd(db,table,'ht',ht,units,type)
     call get_paramd(db,table,'fd',fd,units,type)
     call get_paramc(db,table,'raw_data_file',rawinfile,units,type) ! input slc file
     call get_parami(db,table,'bytes_per_line',nbytes,units,type)
     rawdatalines=filelen(rawinfile//' ')/nbytes
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
!     print *,tstart,tend,rngstart,rngend,numstatevec
     call burstbounds(tstart,tend,rngstart,rngend,timeorbit,xx,vv,numstatevec,latlons,heading)
     headingdeg=heading*180/3.14159265
     if(headingdeg.lt.0.0)headingdeg=headingdeg+360
     print *,'Satellite heading: ',headingdeg

end program orbitstats

subroutine burstbounds(startime,endtime,startr,endr,timeorbit,xx,vv,numstatevec,latlons,r_geohdg)

  implicit none
  real*8 startime,endtime,startr,endr,latlons(4)
  real*8 xyzsatstart(3),velsatstart(3),xyzsatend(3),velsatend(3)
  real*8 xyzsatmid(3),velsatmid(3),xyz(3)
  real*8 timeorbit(*),xx(3,*),vv(3,*),llhsat(3),llh(3),xyzsat(3)
  real*8 vhat(3),that(3),nhat(3),chat(3)
  real*8 alpha,beta,gamm,delta(3),aa,bb,hgts(2),zsch,costheta,sintheta,dopfact
  real*8 pi,sol,r2d,d2r
  real*8 r_lati,r_loni,r_latf,r_lonf,r_geohdg
  real*8 rcurv,rng,r_e2,r_a,vmag,height
  real*8 min_lat,max_lat,min_lon,max_lon
  integer i_type,ind,iter,line,stat,numstatevec

  !  function types
  real*8 norm2
  integer intp_orbit

  integer XYZ_2_LLH

!c  types needed
  type :: ellipsoid 
     real*8 r_a           ! semi-major axis
     real*8 r_e2          ! eccentricity of earth ellisoid
  end type ellipsoid
  type(ellipsoid) :: elp

  type :: pegpoint 
     real*8 r_lat         ! peg latitude
     real*8 r_lon         ! peg longitude
     real*8 r_hdg         ! peg heading
  end type pegpoint
  type(pegpoint) :: peg
  
  type :: pegtrans 
     real*8 r_mat(3,3)    !transformation matrix SCH->XYZ
     real*8 r_matinv(3,3) !transformation matrix XYZ->SCH
     real*8 r_ov(3)       !Offset vector SCH->XYZ
     real*8 r_radcur      !peg radius of curvature
  end type pegtrans
  type(pegtrans) :: ptm

  elp%r_a=6378137.0
  elp%r_e2=0.0066943799901499996
  r_a=6378137.0
  r_e2=0.0066943799901499996

  pi = 4.d0*atan2(1.d0,1.d0)
  sol = 299792458.d0
  r2d = 180.d0/pi
  d2r = pi/180.d0

  XYZ_2_LLH=2
  dopfact=0.d0  ! zero Doppler geometry

! get spacecraft locations at start and end of burst for heading calc.
  stat = intp_orbit(timeorbit, xx, vv, numstatevec, startime, xyzsatstart, velsatstart)
  stat = intp_orbit(timeorbit, xx, vv, numstatevec, endtime, xyzsatend, velsatend)
  stat = intp_orbit(timeorbit, xx, vv, numstatevec, (startime+endtime)/2.d0, xyzsatmid, velsatmid)
!!$  print *,'start x,v ',xyzsatstart,velsatstart
!!$  print *,'end   x,v ',xyzsatend,velsatend
!!$  print *,'mid   x,v ',xyzsatmid,velsatmid

  if (stat.ne.0) then
     print *, 'Error getting statevector for bounds computation'
     call exit()
  endif
  i_type = XYZ_2_LLH
  call latlon(elp, xyzsatstart, llhsat, i_type)
  r_lati=llhsat(1)
  r_loni=llhsat(2)
  height = llhsat(3)
  call latlon(elp, xyzsatend, llhsat, i_type)
  r_latf=llhsat(1)
  r_lonf=llhsat(2)
  height = (height+llhsat(3))/2.d0
  call geo_hdg(r_a,r_e2,r_lati,r_loni,r_latf,r_lonf,r_geohdg)     

  ! now reference to midpoint in time
  call latlon(elp, xyzsatmid, llhsat, i_type)
  peg%r_lat = llhsat(1)
  peg%r_lon = llhsat(2)
  peg%r_hdg = r_geohdg
  call radar_to_xyz(elp, peg, ptm)
  rcurv = ptm%r_radcur

  hgts(1)=-100.d0  ! min max heights for margin in height of terrain
  hgts(2)=2000.d0
  min_lat=1.e10
  min_lon=1.e10
  max_lat=-1.e10
  max_lon=-1.e10

  ! start scanning by line
  do line=1,2
     if(line.eq.1)then
        vmag = dsqrt(dot_product(velsatstart,velsatstart)) !norm2(velsatstart)
        call unitvec(velsatstart, vhat)
        call tcnbasis(xyzsatstart, velsatstart, elp, that, chat, nhat)
        xyzsat=xyzsatstart
!!$        print *,'start xyzsat velsat ',xyzsat,velsatstart
!!$        print *,'start t c n '
!!$        print *,that
!!$        print *,chat
!!$        print *,nhat
!!$        print *,'vhat ',vhat
!!$        print *,'start xyz',xyzsat
     else
        vmag = dsqrt(dot_product(velsatend,velsatend)) !norm2(velsatend)
        call unitvec(velsatend, vhat)
        call tcnbasis(xyzsatend, velsatend, elp, that, chat, nhat)
        xyzsat=xyzsatend
!!$        print *,'end xyzsat velsat ',xyzsat,velsatend
!!$        print *,'end   t c n '
!!$        print *,that
!!$        print *,chat
!!$        print *,nhat
!!$        print *,'vhat ',vhat
!!$        print *,'end  xyz',xyzsat
     end if
     do ind=1,2
        if(ind.eq.1)rng=startr
        if(ind.eq.2)rng=endr
        
        do iter=1,2
           zsch = hgts(iter)
           aa =  height + rcurv
           bb = rcurv + zsch
           costheta = 0.5*((aa/rng) + (rng/aa) - (bb/aa)*(bb/rng))
           sintheta = sqrt(1.0d0 - costheta*costheta)
           gamm = costheta * rng
           alpha  = (dopfact - gamm * dot_product(nhat,vhat)) / dot_product(vhat,that)
           beta  = sqrt(rng*rng*sintheta*sintheta - alpha*alpha)
           delta = gamm * nhat + alpha * that + beta * chat
           xyz = xyzsat + delta
           i_type=XYZ_2_LLH
           call latlon(elp,xyz,llh,i_type)
           print *,'xyzsatmid-xyz ',xyzsatmid-xyz
!!$           print *,'line ind iter lat lon ',line,ind,iter,llh(1),llh(2)
!!$           print *,'xyz ',xyz
!!$           print *,'llh ',llh
!!$           print *,'xyzsat ',xyzsat
!!$           print *,'that ',that
!!$           print *,'chat ',chat
!!$           print *,'nhat ',nhat
!!$           print *,'alpha beta gamm ',alpha,beta,gamm
!!$           print *,'rng sintheta rcurv ',rng,sintheta,rcurv
!!$           print *,'r2s2 a2 diff ',rng*rng*sintheta*sintheta,alpha*alpha,rng*rng*sintheta*sintheta - alpha*alpha
           min_lat = min(min_lat, llh(1)*r2d)
           max_lat = max(max_lat, llh(1)*r2d)
           min_lon = min(min_lon, llh(2)*r2d)
           max_lon = max(max_lon, llh(2)*r2d)
        end do
     end do
  end do
  
!!$  print *, ' '
!!$  print *, 'Estimated DEM bounds needed for global height range: '
!!$  print *, 'Lat: ', min_lat, max_lat
!!$  print *, 'Lon: ', min_lon, max_lon
  print '(a,4f12.5)', 'Estimated DEM bounds: ', min_lat, max_lat, min_lon, max_lon
  latlons(1)=min_lat
  latlons(2)=max_lat
  latlons(3)=min_lon
  latlons(4)=max_lon

  return
  end

!c****************************************************************

	subroutine geo_hdg(r_a,r_e2,r_lati,r_loni,r_latf,r_lonf,r_geohdg)

!c****************************************************************
!c**
!c**	FILE NAME: geo_hdg.f
!c**
!c**     DATE WRITTEN:12/02/93 
!c**
!c**     PROGRAMMER:Scott Hensley
!c**
!c** 	FUNCTIONAL DESCRIPTION: This routine computes the heading along a geodesic
!c**     for either an ellipitical or spherical earth given the initial latitude
!c**     and longitude and the final latitude and longitude. 
!c**
!c**     ROUTINES CALLED:none
!c**  
!c**     NOTES: These results are based on the memo
!c**
!c**        "Summary of Mocomp Reference Line Determination Study" , IOM 3346-93-163
!c**
!c**      and the paper
!c**
!c**        "A Rigourous Non-iterative Procedure for Rapid Inverse Solution of Very
!c**         Long Geodesics" by E. M. Sadano, Bulletine Geodesique 1958
!c**
!c**     ALL ANGLES ARE ASSUMED TO BE IN RADIANS!   
!c**
!c**     UPDATE LOG:
!c**
!c*****************************************************************

       	implicit none

!c	INPUT VARIABLES:
        real*8 r_a                    !semi-major axis
	real*8 r_e2                   !square of eccentricity
        real*8 r_lati                 !starting latitude
        real*8 r_loni                 !starting longitude
        real*8 r_latf                 !ending latitude
        real*8 r_lonf                 !ending longitude  
     
!c   	OUTPUT VARIABLES:
        real*8 r_geohdg

!c	LOCAL VARIABLES:
        real*8 pi,r_t1,r_t2,r_e,r_ome2,r_sqrtome2,r_b0,r_f,r_ep,r_n
        real*8 r_k1,r_k2,r_k3,r_k4,r_k5,r_l,r_ac,r_bc,r_phi,r_phi0
        real*8 r_tanbetai,r_cosbetai,r_sinbetai,r_cosphi,r_sinphi
        real*8 r_tanbetaf,r_cosbetaf,r_sinbetaf,r_lambda,r_coslam,r_sinlam
        real*8 r_ca,r_cb,r_cc,r_cd,r_ce,r_cf,r_cg,r_ch,r_ci,r_cj,r_x,r_q
        real*8 r_sinlati,r_coslati,r_tanlatf,r_tanlati,r_coslon,r_sinlon
        real*8 r_sin2phi,r_cosph0,r_sinph0,r_cosbeta0,r_cos2sig,r_cos4sig
        real*8 r_cotalpha12,r_cotalpha21,r_lsign 
        logical l_first

!c	DATA STATEMENTS:
        data pi /3.141592653589793d0/
        data l_first /.true./ 

!c       SAVE STATEMENTS: (needed on Freebie only)
        save l_first,r_e,r_ome2,r_sqrtome2,r_b0,r_f,r_ep
        save r_n,r_k1,r_k2,r_k3,r_k4,r_k5
 
!c	FUNCTION STATEMENTS: none

!c  	PROCESSING STEPS:

        if(r_e2 .eq. 0)then   !use the simplier spherical formula

	   r_sinlati = sin(r_lati)
	   r_coslati = cos(r_lati)
           r_tanlatf = tan(r_latf)

           r_t1 =  r_lonf - r_loni
	   if(abs(r_t1) .gt. pi)then
	      r_t1 = -(2.d0*pi - abs(r_t1))*sign(1.d0,r_t1)
           endif 
 
           r_sinlon = sin(r_t1)
           r_coslon = cos(r_t1)
           r_t2 = r_coslati*r_tanlatf - r_sinlati*r_coslon

           r_geohdg = atan2(r_sinlon,r_t2)

        else   ! use the full ellipsoid formulation

          if(l_first)then 
             l_first = .false.
	     r_e = sqrt(r_e2)
	     r_ome2 = 1.d0 - r_e2
	     r_sqrtome2 = sqrt(r_ome2)
             r_b0 = r_a*r_sqrtome2
	     r_f = 1.d0 - r_sqrtome2
	     r_ep = r_e*r_f/(r_e2-r_f)
	     r_n = r_f/r_e2
	     r_k1 = (16.d0*r_e2*r_n**2 + r_ep**2)/r_ep**2   
             r_k2 = (16.d0*r_e2*r_n**2)/(16.d0*r_e2*r_n**2 + r_ep**2)
             r_k3 = (16.d0*r_e2*r_n**2)/r_ep**2
             r_k4 = (16.d0*r_n - r_ep**2)/(16.d0*r_e2*r_n**2 + r_ep**2)
             r_k5 = 16.d0/(r_e2*(16.d0*r_e2*r_n**2 + r_ep**2))
          endif

          r_tanlati = tan(r_lati)
          r_tanlatf = tan(r_latf)
          r_l  =  abs(r_lonf-r_loni)
          r_lsign = r_lonf - r_loni
          if(abs(r_lsign) .gt. pi)then
	     r_lsign = -(2.d0*pi - r_l)*sign(1.d0,-r_lsign)
	  endif
          r_sinlon = sin(r_l)
          r_coslon = cos(r_l)
 
          r_tanbetai = r_sqrtome2*r_tanlati
          r_tanbetaf = r_sqrtome2*r_tanlatf

          r_cosbetai = 1.d0/sqrt(1.d0 + r_tanbetai**2)
          r_cosbetaf = 1.d0/sqrt(1.d0 + r_tanbetaf**2)
          r_sinbetai = r_tanbetai*r_cosbetai        
          r_sinbetaf = r_tanbetaf*r_cosbetaf

          r_ac = r_sinbetai*r_sinbetaf        
          r_bc = r_cosbetai*r_cosbetaf        
 
          r_cosphi = r_ac + r_bc*r_coslon
          r_sinphi = sign(1.d0,r_sinlon)*sqrt(1.d0 - min(r_cosphi**2,1.d0))
          r_phi = abs(atan2(r_sinphi,r_cosphi))
          
          if(r_a*abs(r_phi) .gt. 1.0d-6)then

	     r_ca = (r_bc*r_sinlon)/r_sinphi
	     r_cb = r_ca**2
	     r_cc = (r_cosphi*(1.d0 - r_cb))/r_k1
	     r_cd = (-2.d0*r_ac)/r_k1
	     r_ce = -r_ac*r_k2
	     r_cf = r_k3*r_cc
	     r_cg = r_phi**2/r_sinphi
	     
	     r_x = ((r_phi*(r_k4 + r_cb) + r_sinphi*(r_cc + r_cd) + r_cg*(r_cf + r_ce))*r_ca)/r_k5
	     
	     r_lambda = r_l + r_x
	     
	     r_sinlam = sin(r_lambda)
	     r_coslam = cos(r_lambda)
	     
	     r_cosph0 = r_ac + r_bc*r_coslam
	     r_sinph0 = sign(1.d0,r_sinlam)*sqrt(1.d0 - r_cosph0**2)
	     
	     r_phi0 = abs(atan2(r_sinph0,r_cosph0))
	     
	     r_sin2phi = 2.d0*r_sinph0*r_cosph0
	     
	     r_cosbeta0 = (r_bc*r_sinlam)/r_sinph0
	     r_q = 1.d0 - r_cosbeta0**2
	     r_cos2sig = (2.d0*r_ac - r_q*r_cosph0)/r_q
	     r_cos4sig = 2.d0*(r_cos2sig**2 - .5d0)
	     
	     r_ch = r_b0*(1.d0 + (r_q*r_ep**2)/4.d0 - (3.d0*(r_q**2)*r_ep**4)/64.d0)
	     r_ci = r_b0*((r_q*r_ep**2)/4.d0 - ((r_q**2)*r_ep**4)/16.d0)
	     r_cj = (r_q**2*r_b0*r_ep**4)/128.d0
	     
	     r_t2 = (r_tanbetaf*r_cosbetai - r_coslam*r_sinbetai)
	     r_sinlon = r_sinlam*sign(1.d0,r_lsign)
	     
	     r_cotalpha12 = (r_tanbetaf*r_cosbetai - r_coslam*r_sinbetai)/r_sinlam
	     r_cotalpha21 = (r_sinbetaf*r_coslam - r_cosbetaf*r_tanbetai)/r_sinlam
	     
	     r_geohdg = atan2(r_sinlon,r_t2)
	     
          else
	     
	     r_geohdg = 0.0d0
!c             type*, 'Out to lunch...'
	     
          endif
 
	endif
       
        end  

