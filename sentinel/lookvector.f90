program lookvector
  ! compute an 'image' of the look vector to each pixel
  !  output image is e,n,u components of vector from ground to satellite

  use omp_lib

  implicit none
  real*8, allocatable :: timeorbit(:),xx(:,:),vv(:,:)
  real*8 timedelta,x(3),v(3),xxmid(3),vvmid(3),timemid
  real*8 startime,endtime,startr,endr,latlons(4)
  real*8 xyzsatstart(3),velsatstart(3),xyzsatend(3),velsatend(3)
  real*8 xyzsatmid(3),velsatmid(3),xyz(3),xyz0(3),xyz1(3),xyzvel(3),uvenu(3)
  real*8 llhsat(3),llh(3),xyzsat(3),velsat(3),time
  real*8 vhat(3),that(3),nhat(3),chat(3)
  real*8 alpha,beta,gamm,delta(3),aa,bb,hgts(2),zsch,costheta,sintheta,dopfact
  real*8 pi
  real*8 r_lati,r_loni,r_latf,r_lonf,r_geohdg,geohdg
  real*8 rcurv,rng,r_e2,r_a,vmag,height
  real*8 min_lat,max_lat,min_lon,max_lon
  real*8 firstlon, firstlat, deltalon, deltalat
  real*8 r,cosalpha,cosalphaprime,deltar,xspace,yspace
  real*8 r_p, r_local, rnum, rden, rclose
  real*8 uxyz(3),uenu(3),ue(3),un(3),uu(3),rlook(3),llhtest(3),uproj(3),llhmidpoint(3)
  real*8 llhtouse(3)
  real*8, allocatable :: lookvec(:,:)
  integer demwidth, demlength,status,buff(13),k,kk
  integer i_type,ind,iter,line,numstatevec,i,j,ret,iclose,jclose,nvect,ilocation
  character*300 posfile, str
  real*8 q,qq

  !  function types
  real*8 norm2
  integer intp_orbit

  integer unit

  !c  types needed
  type :: ellipsoid 
     real*8 r_a           ! semi-major axis
     real*8 r_e2          ! eccentricity of earth ellipsoid
  end type ellipsoid
  type(ellipsoid) :: elp

  elp%r_a=6378137.0
  elp%r_e2=0.0066943799901499996
  r_a=6378137.0
  r_e2=0.0066943799901499996

  r_p = sqrt(r_a**2-r_a**2*r_e2)
  pi = 4.d0*atan2(1.d0,1.d0)

  if(iargc().lt.1)then
     print *,'Usage: lookvector *.orbtiming'
     call exit(1)
  end if
  if(iargc().ge.1)call getarg(1,posfile)

  ! does the position file exist?
  call stat(posfile,buff,status)
  if(status.ne.0)then
     print *,'Orbtiming file does not exist. ',status
     call exit(1)
  end if

  ! read in the orbtiming file - usually state vectors at 10 sec centers
  open(22,file=posfile,action='read')
  read(22,*)startime
  read(22,*)endtime
  read(22,*)nvect
  read(22,*)nvect ! only 4th one counts
  allocate (timeorbit(nvect),xx(3,nvect),vv(3,nvect))
  do i=1,nvect
     read(22,*)timeorbit(i),xx(:,i),vv(:,i)
     !         print *,timeorbit(i),orbitfile
  end do
  close(22)
  timedelta=timeorbit(2)-timeorbit(1)

  ! estimate dem spacing at center of DEM
  open(21,file='dem.rsc',action='read')
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
  !  print *,demwidth,demlength,firstlon,firstlat,deltalon,deltalat

  !  allocate array to hold look vectors
  allocate(lookvec(3,demwidth))
  ! open lookvector file
  i=index(posfile,'.orbtiming')
  open(41,file=posfile(1:i)//'lookvector',access='stream')

  ! first do some initialization of variables to use in dem loop
  !  find the closest state vector to center of scene
  llh(1)=(firstlat+(demlength/2)*deltalat)*pi/180.
  llh(2)=(firstlon+(demwidth/2)*deltalon)*pi/180.
  llh(3)=0.
  llhmidpoint=llh
  call latlon(elp, xyz0, llh, 1)  ! location of dem center, zero height
!        call latlon(elp, xyz0,llhtest,2)
!        print *,'llh dem midpoint, zero height ',llhtest(1)*180/3.14159,llhtest(2)*180/3.14159,llhtest(3)
  !  get local earth radius
  !  print *,r_a, r_p, llh(1), cos(llh(1)), sin(llh(1))
  rnum=(r_a**2*cos(llh(1)))**2+(r_p**2*sin(llh(1)))**2
  rden=(r_a*cos(llh(1)))**2+(r_p*sin(llh(1)))**2
  r_local=sqrt(rnum/rden)
!    print *,'r local ',r_local

  jclose=0
  rclose=1.e20
  do i=1,nvect
     xyzsat=xx(:,i)
     if(rclose.gt.sqrt(dot_product(xyzsat-xyz0,xyzsat-xyz0)))then
        rclose=sqrt(dot_product(xyzsat-xyz0,xyzsat-xyz0))
        jclose=i
        xyzsatmid=xyzsat !  xyzsatmid now contains point of closest approach
!        print *,i,jclose,rclose
     end if
  end do
!          print *,'state vector at closest approach ',jclose,rclose
  if (jclose.lt.2.or.jclose.gt.nvect-1)then
     print *,'precise orbtiming range does not contain proper minimum'
     call exit
  end if

  q=sqrt(dot_product(xx(:,jclose-1)-xyz0,xx(:,jclose-1)-xyz0))
  qq=sqrt(dot_product(xx(:,jclose+1)-xyz0,xx(:,jclose+1)-xyz0))
  if(q.le.qq)then
     jclose=jclose-1
  end if
  if (jclose.lt.2)then
     print *,'precise orbtimimg range does not contain proper minimum'
     call exit
  end if
!    print *,'state vector at closest approach ',jclose,rclose
  xxmid=xx(:,jclose)   ! save position and velocity at state vector minimum
  vvmid=vv(:,jclose)
  timemid=timeorbit(jclose)
!  print *,'time xx vv ',timemid,xxmid,vvmid

  !  loop over dem
  do kk=1,demlength
     lookvec=0. !  zero out lookvec array for each line

     !$OMP PARALLEL DO private(llhtouse,llh,rclose) &
     !$OMP private(xyzsat,xyzsatmid,xyz0) &
     !$OMP private(time,ilocation,xyzvel,ret,llhtest,rlook,uxyz,xyz1,ue,xspace) &
     !$OMP private(uenu,un,yspace,uu,geohdg,r,uproj,cosalpha,cosalphaprime,deltar) &
     !$OMP shared(kk,pi,firstlat,firstlon,deltalat,deltalon,elp,nvect) &
     !$OMP shared(xx,vv,timeorbit,timedelta,xxmid,vvmid,timemid,r_local)

     do k=1,demwidth
        llhtouse(1)=(firstlat+(kk-1)*deltalat)*pi/180.
        llhtouse(2)=(firstlon+(k-1)*deltalon)*pi/180.
        llhtouse(3)=0.
        llh=llhtouse
        call latlon(elp, xyz0, llh, 1)  ! location of dem point, zero height
        ! now iterate to find closest range to this dem point
        call orbitrangetime(xyz0,timeorbit,xx,vv,nvect,timemid,xxmid,vvmid,time,rclose)
        ilocation=(time-timeorbit(1))/timedelta
        call orbithermite(xx(1,ilocation-1),vv(1,ilocation-1),timeorbit(ilocation-1),time,xyzsatmid,xyzvel)

!        print *,'Closest approach range, time: ',rclose,time
        ret=unit(xyzvel)
!        print *,'unit vector velocity at closest approach: ',xyzvel

        !  sanity check on llh values of satellite, dem
!        call latlon(elp, xyzsatmid,llhtest,2)
!        print *,'llh satellite location        ',llhtest(1)*180/3.14159,llhtest(2)*180/3.14159,llhtest(3)
!        call latlon(elp, xyz0,llhtest,2)
!        print *,'llh dem midpoint, zero height ',llhtest(1)*180/3.14159,llhtest(2)*180/3.14159,llhtest(3)

!        print *,'xyz coords of satellite closest approach ',xyzsatmid
!        print *,'xyz coords of dem midpoint, zero height  ',xyz0
        rlook=xyzsatmid-xyz0
!        print *,'Vector surface to satellite, xyz coords  ',rlook
        uxyz=rlook
        ret=unit(uxyz)
!        print *,'unit look vector to satellite            ',uxyz

        llh=llhtouse
        llh(2)=llh(2)+deltalon*pi/180.
        call latlon(elp, xyz1, llh, 1)
        !  print *,'vector to east ',xyz1-xyz0
        ue=xyz1-xyz0
        ret=unit(ue)
!        print *,'unit vector east, xyz coords  ',ue

        xspace=r_local*acos(dot_product(xyz0,xyz1)/sqrt(dot_product(xyz0,xyz0)*dot_product(xyz1,xyz1)))
!          print *,'xyz1 ',xyz1
!          print *,'xspace ',r_local*acos(dot_product(xyz0,xyz1)/sqrt(dot_product(xyz0,xyz0)*dot_product(xyz1,xyz1)))

        llh=llhtouse
        llh(1)=llh(1)-deltalat*pi/180.
        call latlon(elp, xyz1, llh, 1)
        !  print *,'vector to north ',xyz1-xyz0
        un=xyz1-xyz0
        ret=unit(un)
!        print *,'unit vector north, xyz coords ',un
        yspace=r_local*acos(dot_product(xyz0,xyz1)/sqrt(dot_product(xyz0,xyz0)*dot_product(xyz1,xyz1)))

        llh=llhtouse
        llh(3)=10.
        call latlon(elp, xyz1, llh, 1)  ! up direction
        uu=xyz1-xyz0
        ret=unit(uu)
        !  print *,'unit vector up, xyz coords    ',uu

        r=sqrt(dot_product(xyzsatmid-xyz0,xyzsatmid-xyz0))
!        print *,'Unit look vector, ground to satellite: ',(xyzsatmid-xyz0)/r
        uxyz=xyzsatmid-xyz0
        ret=unit(uxyz)
!        print *,'unit look vector xyz coords            ',uxyz

        uenu(1)=dot_product(uxyz,ue)
        uenu(2)=dot_product(uxyz,un)
        uenu(3)=dot_product(uxyz,uu)
!        print *,'unit look vector enu coords:           ',uenu
        ! project unit vector to surface and estimate heading
        uproj=uenu;
        uproj(3)=0;
        ret=unit(uproj);
!        print *,'Uenu projected onto surface            ',uproj

!!$        cosalpha=(dot_product(xyzsatmid,xyzsatmid)+r_local**2-r**2)/2/r_local/sqrt(dot_product(xyzsatmid,xyzsatmid))
!!$        cosalphaprime=(dot_product(xyzsatmid,xyzsatmid)+(r_local+1)**2-r**2)/2/(r_local+1)/sqrt(dot_product(xyzsatmid,xyzsatmid))
!!$        deltar=r_local*(acos(cosalphaprime)-acos(cosalpha))
        !  print *,'r cosalpha cosalphaprime deltar ',r,cosalpha,cosalphaprime,deltar

        !  save look vector to array
!        print *,'writing ',k,kk,uenu,demwidth,demlength
        lookvec(1:3,k)=uenu

        if ((k.eq.demwidth/2).and.(kk.eq.demlength/2))then  ! save some useful summary params for later use
           call velocity_heading(xyzsatmid,xyzvel,geohdg)
           ! write parameters for product labeling
           i=index(posfile,'.orbtiming')
           open(31,file=posfile(1:i)//'labels')
           write(31,*)geohdg*180./3.14159,xspace,yspace
           write(31,*)-(xyzsatmid-xyz0)/r,r
           write(31,*)llhmidpoint*180./3.14159265359
           write(31,*)uenu
           write(31,*)' Heading(deg) xpacing yspacing'
           write(31,*)'Unit Look vector satellite to ground,range'
           write(31,*)'LLH scene midpoint'
           write(31,*)'Unit look vector ground to satellite e,n, u coords'
           close(31)
        end if

     end do
     !$OMP END PARALLEL DO

     ! write line out
     write(41)lookvec

  end do

  ! close outfile
  close(41)


end program lookvector

!**************

subroutine velocity_heading(xyzsat,xyzvel,geohdg)

  ! approximate satellite heading from spherical earth formula
  ! for more precise result use geo_hdg code instead, this is good to a fraction of a degre

  implicit none

  real*8 xyzsat(3),xyzvel(3),geohdg ! args
  real*8 llhsat(3),xyz0(3),xyz1(3),llh(3)
  real*8 ue(3),un(3),uvenu(3)
  real*8 pi

  integer ret,unit

  !c  types needed
  type :: ellipsoid 
     real*8 r_a           ! semi-major axis
     real*8 r_e2          ! eccentricity of earth ellipsoid
  end type ellipsoid
  type(ellipsoid) :: elp

  elp%r_a=6378137.0
  elp%r_e2=0.0066943799901499996

  pi=4.d0*atan2(1.d0,1.d0)

  !  llh of satellite at current position
  call latlon(elp,xyzsat,llhsat,2)
  xyz0=xyzsat

  llh(1)=llhsat(1)
  llh(2)=llhsat(2)+1.e-4*pi/180 
  llh(3)=llhsat(3)
  call latlon(elp, xyz1, llh, 1)
  !     print *,'change longitude: ',llhsat,llh
  !     print *,'in xyz: ',xyz0,xyz1

  !  print *,'vector to east ',xyz1-xyz0
  ue=xyz1-xyz0
  ret=unit(ue)
  !     print *,'unit vector east, xyz coords  ',ue

  llh(1)=llhsat(1)+1.e-4*pi/180 
  llh(2)=llhsat(2) 
  llh(3)=llhsat(3)
  call latlon(elp, xyz1, llh, 1)
  !     print *,'change latitude: ',llhsat,llh
  !     print *,'in xyz: ',xyz0,xyz1
  !  print *,'vector to north ',xyz1-xyz0
  un=xyz1-xyz0
  ret=unit(un)

  uvenu(1)=dot_product(xyzvel,ue)
  uvenu(2)=dot_product(xyzvel,un)
  uvenu(3)=0.
  ret=unit(uvenu)

  !print *,timeorbit(i),atan2(uvenu(1),uvenu(2))*180/3.14159,r_geohdg*180/3.14159,' velocity geo_hdg'

  geohdg=atan2(uvenu(1),uvenu(2))
  return
end subroutine velocity_heading

integer function unit(u)
  real*8 u(3)
  u=u/sqrt(u(1)**2+u(2)**2+u(3)**2)
end function unit

