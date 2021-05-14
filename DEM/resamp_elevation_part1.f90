!c  resamp_elevation - initial guess at topo map in radar coordinates
!c      and computation of elevation resampling to remove stereoscopic resampling error

      use sql_mod
      use omp_lib

      implicit none
      integer width, length, iz
      real*4, allocatable :: zsch(:),elev_corr(:),lastzsch(:),lastcorr(:)
      real*4, allocatable :: interpcorr(:),interpzsch(:)
      real*4 frac
      real*8 lat,lon,z,rho
      real*4  r_phase,r_phase_zero,r_phase_corr,r_phase_corr_zero
      real*8, allocatable :: squintshift(:),toposhift(:)
      integer*2, allocatable :: dem(:,:)
      complex, allocatable ::  c_int(:),c_flat(:)
      real*8 sch_p_first(3),sch_p_last(3),sch(3),xyz(3),llh(3),csquint
      real*8 r2d,look,totalshift,refhgt,r_time
      integer pixel
      real*8 r0, pi, range, cosbeta, sinbeta, beta, rcurv, vel
      character*4 fname
      character*60 igramfile,positionfile1,positionfile2,demfile,demrscfile,str
      real*8 peglat,peglon,peghdg,time1,time2,s0,s1,s2
      real*8 s_mocomp(100000),t_mocomp(100000),mocbase(3,100000),midpoint(3,100000),r1(3),r2(3)
      real*8 rho1,rho2,r_los(3),r_schp(3),rmag,r0hat(3),rhat(3)
      real*8 r_xyzp(3),sch0(3),xyz0(3)
      integer i_mocomp(100000)
      real*8 dot,dotsch,tanbeta,x,deltalat,deltalon,firstlat,firstlon,cosalpha
      real*8 wvl,rspace,height,prf,fd,basemag,azspace,fdprfs
      real*8 arg,cosbetaprime,coslook,re,zprime,ztemp,rminoraxis,rlatg,st,ct,second
      real*8 basemid(3,100000),base1(3,100000),base2(3,100000),mid1(3,100000),mid2(3,100000)
      real*8 r20,r21,r10,r11
      character*60 dbname,tablename,units,type
      integer*8 db
      integer line,idemwidth,idemlength,iter,k,irec,is_mocomp,Nazlooks,Nrnglooks
      integer idemlat,idemlon,i_rec,i_type,i,j,linesperpatch,nazvalid

!c  types needed

	type :: ellipsoid 
	   real*8 r_a		! semi-major axis
	   real*8 r_e2		! eccentricity of earth ellisoid
	end type ellipsoid
	type(ellipsoid) :: elp

	type :: pegpoint 
	   real*8 r_lat		! peg latitude
	   real*8 r_lon		! peg longitude
	   real*8 r_hdg		! peg heading
        end type pegpoint
	type(pegpoint) :: peg
   
	type :: pegtrans 
	   real*8 r_mat(3,3)	!transformation matrix SCH->XYZ
	   real*8 r_matinv(3,3)	!transformation matrix XYZ->SCH
	   real*8 r_ov(3)	!Offset vector SCH->XYZ
	   real*8 r_radcur	!peg radius of curvature
	end type pegtrans
	type(pegtrans) :: ptm

        if(iargc().lt.2)then
           print *,'Usage: resamp_elevation_part1 database tablename'
           stop
        end if
        call getarg(1,dbname)
        call getarg(2,tablename)

!c  get parameters from database
        call open_db(db,dbname)
!        call get_paramc(db,tablename,'interferogram',igramfile,units,type)
        call get_parami(db,tablename,'range_samples1',width,units,type)
        call get_parami(db,tablename,'number_of_lines',length,units,type)
        call get_paramc(db,tablename,'orbit_sch1',positionfile1,units,type)
        call get_paramc(db,tablename,'orbit_sch2',positionfile2,units,type)
        call get_paramd(db,tablename,'slant_range_pixel_spacing',rspace,units,type)
        call get_paramd(db,tablename,'r0',r0,units,type)
        call get_paramd(db,tablename,'ht',height,units,type)
        call get_paramd(db,tablename,'rc',rcurv,units,type)
        call get_paramd(db,tablename,'velocity',vel,units,type)
        call get_parami(db,tablename,'range_looks',Nrnglooks,units,type)
        call get_parami(db,tablename,'azimuth_looks',Nazlooks,units,type)
        call get_paramd(db,tablename,'peg_latitude',peglat,units,type)
        call get_paramd(db,tablename,'peg_longitude',peglon,units,type)
        call get_paramd(db,tablename,'peg_heading',peghdg,units,type)
        call get_paramd(db,tablename,'fd',fdprfs,units,type)
        call get_paramd(db,tablename,'prf',prf,units,type)
        call get_paramd(db,tablename,'wavelength',wvl,units,type)
        call get_paramc(db,tablename,'demfile',demfile,units,type)
        call get_paramc(db,tablename,'demrscfile',demrscfile,units,type)
        call get_parami(db,tablename,'patch_size',linesperpatch,units,type)
        call get_parami(db,tablename,'valid_az_samples',nazvalid,units,type)
        call close_db(db)

!c  allocate variable arrays
        allocate (zsch(width))
        allocate (elev_corr(width))
        allocate (lastzsch(width))
        allocate (lastcorr(width))
        allocate (interpzsch(width))
        allocate (interpcorr(width))
        allocate (squintshift(width))
        allocate (toposhift(width))

!c  get geometry for this scene, get actual s from mocomp reference track
        open(21,file='reference_orbit.txt')
        do i=1,100000
           read(21,*,end=11)i_mocomp(i),t_mocomp(i),s_mocomp(i)
        end do
11      close(21)
        is_mocomp=(linesperpatch-nazvalid)/2
        !is_mocomp=(8192-2048)/2  ! note this is for patch size 8192 and 2048 good lines only

!c  some constants

      r2d=180./3.14159265359
      refhgt=0
      pi=4.d0*atan2(1.d0,1.d0)

      elp%r_a=6378137.0
      elp%r_e2=0.0066943799901499996

      peg%r_lat =  peglat
      peg%r_lon =  peglon
      peg%r_hdg =  peghdg

!c  get re and insert it into database
      rminoraxis=sqrt(1.-elp%r_e2)*elp%r_a
      rlatg = atan(tan(peg%r_lat)*elp%r_a*elp%r_a/(rminoraxis*rminoraxis));
      st = sin(rlatg);
      ct = cos(rlatg);
      arg = (ct*ct)/(elp%r_a*elp%r_a) + (st*st)/(rminoraxis*rminoraxis);
      re = 1./(sqrt(arg));
      call open_db(db,dbname)
        call add_param(db,tablename,'re')
        call edit_paramd(db,tablename,'re',re,'m','Local Earth radius')
      call close_db(db)      
      print *,'re, rminoraxis',re,rminoraxis
      print *,'Local earth radius of curvature: ',rcurv



!c  precalculate squint-related shift for each range location
      fd=fdprfs * prf
      line=1
      do pixel=1,width
         rho=r0+rspace*(pixel-1)
         tanbeta=-fd*(height+rcurv)*wvl*rho/vel/(rcurv**2+(height+rcurv)**2-rho**2)
         squintshift(pixel) = -rcurv*atan(tanbeta)
!c  make tx/rx midpoint correction here
!c         squintshift(pixel) = squintshift(pixel)-2*rho(pixel,line)/299792458.*vel*(rcurv/(height+rcurv))/2.
      end do

      print *,'ismocomp, length: ',ismocomp,length
!c  sch of s/c at line 1
      sch_p_first(1)=s_mocomp(is_mocomp)
      sch_p_first(2)=0.
      sch_p_first(3)=height

!c  sch of s/c at last line
      sch_p_last(1)=s_mocomp(is_mocomp+length)
      sch_p_last(2)=0.
      sch_p_last(3)=height
 
!c  initialize the transformation matrices
      call radar_to_xyz(elp,peg,ptm)

!c  lat lon of s/c, line 1
      i_type=0
      call convert_sch_to_xyz(ptm,sch_p_first,xyz,i_type)
      i_type=2
      call latlon(elp,xyz,llh,i_type)
      print *,sch_p_first,' sch s/c first'
      print *,llh(1)*r2d,llh(2)*r2d,llh(3),' lat/lon s/c line 1'
!c  lat lon of s/c, line last
      i_type=0
      call convert_sch_to_xyz(ptm,sch_p_last,xyz,i_type)
      i_type=2
      call latlon(elp,xyz,llh,i_type)
      print *,sch_p_last,' sch s/c last'
      print *,llh(1)*r2d,llh(2)*r2d,llh(3),' lat/lon s/c line last'
      print *

!c  sch of line 1, pixel 1
      line=1
      pixel=1
!c  save rho to the point
      rho=r0+rspace*(pixel-1)
      cosalpha=((height+rcurv)**2+rcurv**2-rho**2)/2./(height+rcurv)/rcurv
      sch(1)=s_mocomp(is_mocomp+line)
      sch(1)=sch(1)+squintshift(pixel)
      beta=-squintshift(pixel)/rcurv 
      csquint=-rcurv*acos(cosalpha)
      sch(2)=-rcurv*acos(cosalpha/cos(beta))
      sch(3)=0.
      s0=sch(1)

      print *,sch,' sch line 1 pixel 1'

!c  lat lon of location 1,1
      i_type=0
      call convert_sch_to_xyz(ptm,sch,xyz,i_type)

      i_type=2
      call latlon(elp,xyz,llh,i_type)

      print *,llh(1)*r2d,llh(2)*r2d,llh(3),' lat/lon 1,1'

      open(25,file='latloncoords',status='replace')
      write(25,*) llh(1)*r2d
      write(25,*) llh(2)*r2d
!c  sch of line last, pixel last
      line=length
      pixel=width
!c  save rho to the point
      rho=r0+rspace*(pixel-1)
      cosalpha=((height+rcurv)**2+rcurv**2-rho**2)/2./(height+rcurv)/rcurv
      sch(1)=s_mocomp(is_mocomp+line)
      sch(1)=sch(1)+squintshift(pixel)
      beta=-squintshift(pixel)/rcurv 
      csquint=-rcurv*acos(cosalpha)
      sch(2)=-rcurv*acos(cosalpha/cos(beta))
      sch(3)=0.
      azspace=(sch(1)-s0)/(length-1)

      print *,sch,' sch line last pixel last'

!c  lat lon of location
      i_type=0
      call convert_sch_to_xyz(ptm,sch,xyz,i_type)

      i_type=2
      call latlon(elp,xyz,llh,i_type)

      print *,llh(1)*r2d,llh(2)*r2d,llh(3),' lat/lon line last pixel last'

      write(25,*) llh(1)*r2d
      write(25,*) llh(2)*r2d
      close(25)


      end

