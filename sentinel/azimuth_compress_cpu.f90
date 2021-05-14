!!!!!!!!!!!!!!
!
!
!  azimuth compression subroutine for use in back projection processor
!
!
!!!!!!!!!!!!!!

subroutine azimuth_compress_cpu(burstdata,satloc,rawdatalines,samplesPerBurst,demwidth,demlength,fdout,fddem, &
       deltalat,deltalon,firstlat,firstlon, &
       latlons,timeorbit,xx,vv,numstatevec,rngstart,rngend,tstart,tend, &
       tmid,xyz_mid,vel_mid,t,dtaz,dmrg,wvl,aperture,iaperture,angc0,angc1,prf)

  implicit none

! parameters
  integer stat,intp_orbit
  integer*4 fddem, fdout, ioseek, ioread, iowrit
  integer*4 samplesPerBurst
  integer*4 demwidth,demlength
  integer*4 numstatevec,iaperture
  real*8 deltalat,deltalon,firstlat,firstlon
  real*8 timeorbit(1000),xx(3,1000),vv(3,1000)
  real*8 satloc(3,100000),satvel(3,100000)
  real*8 :: lat
  real*8, dimension(:),allocatable :: lon
  real*8 :: azoff
  integer*2, allocatable :: demin(:)
  real*8 :: angc0, angc1, prf
  
! internal parameters
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
  complex*8 :: pixelint

  ! array to hold burst
  complex*8 :: burstdata(rawdatalines,samplesPerBurst)
  complex*8, allocatable :: outdata(:), baddata(:)
  real*8 :: wvl

  integer*4  rawdatalines,nbytes

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

  !c  how much memory to allocate?
  lastline=int((latlons(1)-firstlat)/deltalat)
  firstline=int((latlons(2)-firstlat)/deltalat)
  if(firstline.lt.0)firstline=0
  if(lastline.lt.0)lastline=0
  if(firstline.gt.demlength-1)firstline=demlength-1
  if(lastline.gt.demlength-1)lastline=demlength-1
  nlines=lastline-firstline+1
  print 'Burst line limits, size (first,last,nlines) ',firstline,lastline,nlines

  arraysize=nlines*demwidth
  allocate(demin(0:arraysize-1), outdata(0:demwidth-1), lon(0:arraysize-1), indata(0:arraysize-1))
  allocate (xyzfit(0:nlines*9-1),coef(0:nlines*3-1))
  
  print *,pi,sol,rad2deg,deg2rad
  print *,'burstdata,satloc'
  print *,burstdata(1,1),burstdata(rawdatalines,samplesPerBurst)
  print *,satloc(:,1),satloc(:,rawdatalines)

  !c  set up longitude loop
  firstpix=(latlons(3)-firstlon)/deltalon
  if(firstpix.lt.0)firstpix=0
  lastpix=(latlons(4)-firstlon)/deltalon
  if(lastpix.gt.demwidth-1)lastpix=demwidth-1
  !c  define longitude array
  do i=0,demwidth-1
     lon(i)=firstlon+i*deltalon
  end do

  !c zero out data arraybefore integration
  outdata=cmplx(0.,0.)

  !c  process burst, begin by grabbing proper section of dem
  iaddr=(firstline)*demwidth*2
  nbytes=lseek(fddem,iaddr,SEEK_SET)
  iaddr=nlines*demwidth*2
  nbytes=read(fddem,demin,iaddr)
  if(nbytes.le.0)then
     print *,'dem read error ',nbytes
     print *,'iaddr ',iaddr
     print *,'nlines, demwidth ',nlines,demwidth
  end if

  !c  compute xyz array
  do loop=0,demwidth*nlines-1
     line=int((loop)/demwidth)
     pixel=loop-line*demwidth
     lat=firstlat+(line+firstline)*deltalat
     if(pixel.ge.firstpix.and.pixel.le.lastpix)then
        xyzoffset=(line*demwidth+pixel)*3
        llhlat = lat*deg2rad
        llhlon=(firstlon+pixel*deltalon)*deg2rad
        llhhgt=demin(xyzoffset/3)

        re=a/dsqrt(1.d0-e2*sin(llhhat)*sin(llhhat))

        xyz(xyzoffset+0) = (re + llhhgt)*cos(llhlat)*cos(llhlon)
        xyz(xyzoffset+1) = (re + llhhgt)*cos(llhlat)*sin(llhlon)
        xyz(xyzoffset+2) = (re - (re * e2) + llhhgt)*sin(llhlat)
     
        if(pixel.eq.firstpix)then
           xyzfit(line * 9 + 0)=xyz(xyzoffset+0)
           xyzfit(line * 9 + 1)=xyz(xyzoffset+1)
           xyzfit(line * 9 + 2)=xyz(xyzoffset+2)
        end if
        if(pixel.eq.firstpix + int((lastpix-firstpix)/2-1))then
           xyzfit(line * 9 + 3)=xyz(xyzoffset+0)
           xyzfit(line * 9 + 4)=xyz(xyzoffset+1)
           xyzfit(line * 9 + 5)=xyz(xyzoffset+2)
        end if
        if(pixel.eq.firstpix + 2 * int((lastpix-firstpix)/2-1))then
           xyzfit(line * 9 + 6)=xyz(xyzoffset+0)
           xyzfit(line * 9 + 7)=xyz(xyzoffset+1)
           xyzfit(line * 9 + 8)=xyz(xyzoffset+2)
        end if

     end if  ! end pixel test
  end do   !end setxyz loop
        

  ! begin loop over lines in dem for burst
  do line = firstline,lastline
     if(mod(line,100).eq.1)print *,'dem line ',line
     do ipix=0,2
        pixel=firstpix+ipix*int((lastpix-firstpix)/2-1)

        xyztemp(0)=xyzfit((line-firstline)*9 + ipix * 3 + 0)
        xyztemp(1)=xyzfit((line-firstline)*9 + ipix * 3 + 1)
        xyztemp(2)=xyzfit((line-firstline)*9 + ipix * 3 + 2)

        tline=omptmid
        satx = xyz_mid
        satv = vel_mid
        call orbitrangetime(xyztemp(0),timeorbit,xx,vv,numstatevec,omptmid,satx,satv,tline,rngpix)

        !  tops-specific geometry calculations
        unitlookvector=xyztemp-satx
        umag=dsqrt(unitlookvector(1)*unitlookvector(1)+unitlookvector(2)*unitlookvector(2)+unitlookvector(3)*unitlookvector(3))
        unitlookvector=unitlookvector/umag
        udotv=satv(1)*unitlookvector(1)+satv(2)*unitlookvector(2)+satv(3)*unitlookvector(3)
        fd=2.0/ompwvl*udotv
        veff=dsqrt(satv(1)*satv(1)+satv(2)*satv(2)+satv(3)*satv(3))
        td=(rngpix* (ompwvl)/2./veff/veff*fd-rngpix*(ompangc0)*pi/180./veff)/(1.+rngpix*(ompangc1)*pi/180./veff)
        naperture=td*(ompprf) + omprawdatalines/2

        if (ipix.eq.0)y1=naperture
        if (ipix.eq.1)y2=naperture
        if (ipix.eq.2)y3=naperture
      !c if at last pixel, save coefficients for azoff fit
      if (ipix.eq.2)then
         coef((line-firstline)*3+0)=0.5*(y1+y3-2.*y2)
         coef((line-firstline)*3+1)=0.5*(y3-y1)
         coef((line-firstline)*3+2)=y2
      end if

   end do ! end pixel loop
end do  ! end line loop










        !!Initialize
     azoff = BAD_VALUE
     outdata=cmplx(0.,0.)
!     print *,'line initialized ',line,BAD_VALUE,outdata(1),azoff

     lat=firstlat+(line)*deltalat
     ! if outside range dont bother with computations
!     print *,line,lat,latlons(1),latlons(2)
     if(lat.ge.latlons(1))then
        if(lat.le.latlons(2))then
           
           !!Read in this line from DEM
           nbytes=ioseek(fddem,int8((line-1)*demwidth*2))
           nbytes=ioread(fddem,demin,int8(demwidth*2))
!           print *,'bytes read ',fddem,nbytes,demin(1),demin(demwidth)
           !              read(22,rec=line)demin
           if (mod(line,1000).eq.1) then
              print *, 'Processing line: ', line
           endif
           
           !$OMP PARALLEL DO private(i_type)&
           !$OMP private(xyz,llh,rngpix,tline,satx,satv,cacc)&
           !$OMP private(azoff,azline,range,r,intr,fracr,val,phase)&
           !$OMP shared(lat,lon,demin,burstdata) &
           !$OMP shared(demwidth,outdata,samplesPerBurst) &
           !$OMP shared (timeorbit,xx,vv,numstatevec) &
           !$OMP shared(elp,rngstart,tstart,tend,rngend) &
           !$OMP shared(tmid,xyz_mid,vel_mid,t) &
           !$OMP shared(dtaz,dmrg,deg2rad) &
           !$OMP shared(wvl,pi,rawdatalines,satloc,aperture,iaperture) 
           do pixel = 1,demwidth 

              llh(1) = lat * deg2rad
              llh(2) = lon(pixel) * deg2rad
              llh(3) = demin(pixel)
!              print *,line,pixel,llh
              i_type = LLH_2_XYZ
              call latlon(elp,xyz,llh,i_type)
              tline = tmid
              satx = xyz_mid
              satv = vel_mid
!              print *,xyz,tline,satx
!                 sata = acc_mid
                 !! get the zero doppler location of the satellite
              call orbitrangetime(xyz,timeorbit,xx,vv,numstatevec, &
                   tmid,satx,satv,tline,rngpix)
!              print *,'tline rngpix ',tline,rngpix,dtaz,dmrg
!              print *,'pixelint args'
!              print *,aperture,iaperture,rawdatalines,xyz,rngstart,rngend,dmrg,pi,wvl
              if(tline.ge.tstart.and.tline.le.tend)then
                 azoff = ((tline - tstart)/dtaz) !- 1.0d0*(line-1)
                 phase=4.d0*pi/wvl*rngpix
!                 print *,burstdata(1,1),burstdata(rawdatalines,samplesPerBurst)
!                 print *,azoff,satloc(:,nint(azoff))
                 outdata(pixel)=pixelint(burstdata,satloc,azoff,aperture,iaperture,rawdatalines,xyz, &
                      rngstart,rngend,dmrg,pi,wvl)
                 
              end if
           enddo   ! end pixel loop
           !$OMP END PARALLEL DO

           !!Write line to output file
           nbytes=ioseek(fdout,int8((line-1)*demwidth*8))
           nbytes=iowrit(fdout,outdata,int8(demwidth*8))

        end if
     end if ! end if that checks if inside latitude line bounds
     
  end do ! line loop ends here

!!$  close(31)
  deallocate(lon,demin)
  return
end subroutine azimuth_compress_cpu

