!c****************************************************************

        subroutine tcnbasis(pos,vel,elp,r_t,r_c,r_n)

        implicit none

!c      INPUT VARIABLES:
        real*8, dimension(3) :: pos
        real*8, dimension(3) :: vel
        
!c      OUTPUT VARIABLES:
        real*8, dimension(3) :: r_c, r_t, r_n

!c      LOCAL VARIABLES:
        real*8 tvt, tvc,tvn, r_a, r_e2, lat, lon, rad
        real*8 r_temp(3),r_llh(3)
        integer i, XYZ_2_LLH

!c  types needed

        type :: ellipsoid 
           real*8 r_a           ! semi-major axis
           real*8 r_e2          ! eccentricity of earth ellisoid
        end type ellipsoid
        type(ellipsoid) :: elp
        
        elp%r_a=6378137.0
        elp%r_e2=0.0066943799901499996
        XYZ_2_LLH=2

!c      compute a TCN basis vector set

        call latlon(elp,pos,r_llh,XYZ_2_LLH)
        lat = r_llh(1) 
        lon = r_llh(2) 
        rad = r_llh(3) 
        !print *,'lat lon hgt ',r_llh
        r_n(1) = -cos(lat)*cos(lon)
        r_n(2) = -cos(lat)*sin(lon)
        r_n(3) = -sin(lat)
        !print *,'normal vector ',r_n
        call cross(r_n,vel,r_temp)
        call unitvec(r_temp,r_c)

        call cross(r_c,r_n,r_temp)
        call unitvec(r_temp,r_t)

        return
        end subroutine tcnbasis

