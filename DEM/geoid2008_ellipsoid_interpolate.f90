  !c  convert geoid-based dem to ellipsoid-based dem, interpolate a 2.5 min. grid
  !c
  !c  Prime application: convert Copernicus DEM to an ellipsoid-based DEM

      implicit none
      character*300 demfile,rscfile,indemfile,inrscfile,str,gridfile
      integer*2, allocatable :: indem(:,:),dem(:,:)
      integer iwidth, ilength, i, j
      real*8 x0,y0,dx,dy,x,y !,f,rj2,rj4,rj6
      real*8, allocatable :: xx(:),ellipsoidhgt(:) !,sinml(:,:),cosml(:,:)
      real*4 grid(360*24+1,180*24+1)  ! size of 2.5 min grid

      if (iargc().lt.4)then
         print *,'Usage: geoid2008_ellipsoid_interpolate demfile-in rscfile-in demfile-out rscfile-out egm_geoid_grid'
         stop
      end if

      call getarg(1,indemfile)
      call getarg(2,inrscfile)
      call getarg(3,demfile)
      call getarg(4,rscfile)
      call getarg(5,gridfile)

!c  get dem parameters
      open(22,file=inrscfile)
      read(22,'(14x,i6)')iwidth
      read(22,'(14x,i6)')ilength
      read(22,'(a)')str
      read(str(15:60),*)x0
      read(22,'(a)')str
      read(str(15:60),*)y0
      read(22,'(a)')str
      read(str(15:60),*)dx
      read(22,'(a)')str
      read(str(15:60),*)dy
      close(22)

      print *,'width, length of dem: ',iwidth,ilength
      print *,'Starting lat, long:   ',y0,x0
      print *,'Delta lat, long:      ',dy,dx

      !c  read in geoid model grid on 2.5 min. spacing
      open(1,file=trim(gridfile),access='stream',status='old')
      read(1)grid
      close(1)
      print *,grid(100,100:110)
!c  create output rscfile
      call execute_command_line('cp '//trim(inrscfile)//' '//trim(rscfile))

!c  read in dem and apply correction
      allocate (indem(iwidth,ilength),dem(iwidth,ilength),xx(iwidth),ellipsoidhgt(iwidth))
      
      open(20,file=trim(indemfile),access='stream')
      read(20)indem
      close(20)
      
      do j=1,iwidth
         xx(j)=x0+(j-1)*dx ! set longitude values
      end do
      do i=1,ilength
         y=y0+(i-1)*dy
         if(mod(i,1000).eq.0)print *,'line ',i
         call geoid_interpolate(ellipsoidhgt,y,xx,iwidth,grid)
!         print *,'eh ',ellipsoidhgt
         dem(:,i)=nint(indem(:,i)+ellipsoidhgt)
      end do
      open(20,file=trim(demfile),access='stream')
      write(20)dem
      close(20)
      
    end program

    subroutine geoid_interpolate(ellipsoidhgt,y,xx,iwidth,grid)

       
      IMPLICIT REAL*8 (A-H,O-Z)
      real*4 grid(360*24+1,180*24+1),yline(360*24+1)
      REAL*8 ellipsoidhgt(iwidth),xx(iwidth)

      dx=1.d0/24
      dy=-1.d0/24

      ! get interpolated latitude (y) first
      iynorth=(y-90.)/dy+1
      iysouth=(y-90.)/dy+2
      ynorth=90.+iynorth*dy
      ysouth=90.+iysouth*dy
      fracy=(y-ynorth)/(ysouth-ynorth)
      yline=(1-fracy)*grid(:,iynorth)+fracy*grid(:,iysouth)

      ! loop over longitude
      do i=1,iwidth
         ixleft=(xx(i)+180.0)/dx+1
         ixright=(xx(i)+180.0)/dx+2
         xleft=-180.+ixleft*dx
         xright=-180.+ixright*dx
         fracx=(xx(i)-xleft)/(xright-xleft)
         q=(1-fracx)*yline(ixleft)+fracx*yline(ixright)
         ellipsoidhgt(i)=q
      end do

    end subroutine geoid_interpolate
