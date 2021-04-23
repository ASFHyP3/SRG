	program testutmf
	
	real*8	easting(100),northing(100),lat(100),long(100)
	character*30 f
	real*8	a,esq,k0,eoffset,noffset,tx(100),ty(100)
	integer zone
	character*20 str
	
	a=6378137.d0
	esq=0.0066943800d0
	k0=0.9996
	
	call getarg(1,str)
	read(str,*)lat(1)
	call getarg(2,str)
	read(str,*)long(1)

	eoffset=0.
	noffset=0.
	numpts=1
	call utmc(lat, long, tx, ty, numpts, eoffset, noffset,
     &                a, esq, k0, zone)

	print *,'Converted to utm:'
	do i=1,numpts
	   print '(2f15.2, i10)',tx(i),ty(i),zone
	end do

	call utmll(tx, ty, lat, long, numpts, a, esq, k0, zone)
	do i=1,numpts
		print '(2f15.2,2f10.4)',ty(i),tx(i),lat(i),long(i)
	end do
	do i=1,numpts
		write(22, '(2f10.4)')lat(i),long(i)
	end do
	end
	



c
c convert the utm coordinates (northing & easting) of
c a target to lat/lon
c
c input parameters:
c          x- array of easting coordinates (in meters)
c          y- array of northing coordinates (in meters)
c        esq- eccentricity squared
c          a- semi-major (equatorial) axis of earth (in meters)
c         k0- center scale factor
c     numpts- number of points to convert
c       zone- zone number
c
c output parameters:
c        lat- array of latitude coordinates (in degrees)
c       long- array of longitude coordinates (in degrees)
c
c variables:
c          n- zone no.
c        dtr- degree to radian
c       xadj- easting adjustment
c       yadj- northing adjustment
c
c Reference: Synder, J. P. 1983, Map projection used by the US
c            geological survey(bulletin 1532)
c
c Added esq, a, and k0 as input parameters.  Added variable
c rm0.  Redefined PI to have double-precision accuracy.  Added
c the variables xadj and yadj.  Made x, y, lat, and long arrays
c and added the input parameter numpts.
c                                                      jgs 4/90
c
      subroutine utmll(x, y, lat, long, numpts, a, esq, k0, zone)
      implicit none
      integer*4 numpts, i, zone
      real*8 esq, a, k0, u1, u2, u3, lat1, esqsin2, lat1d, long0, dtr
      real*8 x(numpts), y(numpts), lat(numpts), long(numpts)
      real*8 rm, e1, u, epsq, t1, c1, rn1, r1, rm0
      real*8 xadj, yadj, tanlat1, d

      dtr = 4.d0 * datan(1.d0)/180.d0
      rm0 = 0.d0
      yadj = 1.d7
      long0 = float(zone)*6.d0 - 183.d0

      do i = 1, numpts
          rm = (y(i) - yadj)/k0 + rm0
          e1 = (1.d0 - dsqrt(1.d0 - esq))/(1.d0 + dsqrt(1.d0 - esq))
          u = rm/(a * (1.d0 - esq/4.d0 - (3.d0 * esq * esq/64.d0)
     &                      - (5.d0 * esq * esq * esq/256.d0) ) )
          u1 = (3.d0 * e1 / 2.d0 - (27.d0 * e1**3)/32.d0) * dsin(2.d0*u)
          u2 = (21.d0 * e1**2/16.d0 - (55.d0 * e1**4)/32.d0)
     &             * dsin(4.d0*u)
          u3 = (151.d0 * e1**3 / 96.d0) * dsin(6.d0*u)
          lat1 = u + u1 + u2 + u3
          lat1d = lat1/dtr

          esqsin2 = 1.d0 - esq*(dsin(lat1))**2
          epsq = esq/(1.d0 - esq)
          c1 = epsq * (dcos(lat1))**2
          tanlat1 = dsin(lat1)/dcos(lat1)
          t1 = tanlat1 * tanlat1
          rn1 = a/dsqrt(esqsin2)
          r1 = a*(1.d0 - esq)/dsqrt(esqsin2 * esqsin2 * esqsin2)
          xadj = 5.d5
          d = (x(i) - xadj)/(rn1 * k0)

          lat(i) = lat1d - ((rn1 * tanlat1/r1) * (d*d*0.5d0
     &             - (5.d0 + 3.d0*t1 - 10.d0*c1 + 4.d0*c1*c1
     &                     - 9.d0*epsq) * (d**4)/24.d0
     &             + (61.d0 + 90.d0*t1 + 298.d0*c1 + 45.d0*t1*t1
     &                      - 252.d0*epsq - 3.d0*c1*c1)
     &                 *(d**6)/720.d0) )/dtr
          long(i) = long0 + ((1.d0/dcos(lat1)) * (d
     &              - (1.d0 + 2.d0*t1 + c1) * (d**3)/6.d0
     &              + (5.d0 - 2.d0*c1 + 28.d0*t1 - 3.d0*c1*c1
     &                      + 8.d0*epsq + 24.d0*t1*t1)
     &                 *(d**5)/120.d0) )/dtr
      end do
      return
      end

	
c convert lat/lon to universal transverse mercator(utm)
c coordinates (easting, northing)
c
c	NOTE: modified UTM ! Delta y is always 1e+7 also on the
c	northern hemisphere. Therefore the northing is 1e+7 larger
c	than the true northing for latitudes larger than 0.
c	The advantage of this approach is that inversion is possible
c	without additional information on whether the location is on the
c	southern or northern hemisphere.
c 
c input parameters:
c       tlat- array of latitude coordinates
c       tlon- array of longitude coordinates
c     numpts- number of points to convert
c    eoffset- cross track error \ of the input image frame, use in
c    noffset- along track error / terrain correction processing only
c          a- semi-major (equatorial) axis of the earth (m)
c        esq- eccectricity square
c         k0- central scale factor
c
c output parameters:
c         tx- array of easting coordinates
c         ty- array of northing coordinates
c
c variables:
c        esq- eccentricity square
c       epsq- e-prime squared
c          a- semi major axis
c         k0- center scale factor
c          n- zone
c      long0- central meridian
c        dtr- degree to radian
c       xadj- easting adjustment
c       yadj- northing adjustment
c
c Reference: Synder, J. P. 1983, Map projection used by the US
c            geological survey(bulletin 1532)
c
      subroutine utmc(tlat, tlon, tx, ty, numpts, eoffset, noffset,
     &                a, esq, k0, zone)
      implicit none
      integer*4 numpts, n, zone, i
      real*8 epsq, a, lat, long, long0, a1, a2, a3, rn
      real*8 t, b1, b2, rm, rm0, dtr, eoffset, noffset
      real*8 tanlat, c
      real*8 tlat(numpts), tlon(numpts), yadj, xadj
      real*8 tx(numpts), ty(numpts)
      real*8 esq, k0
      real*8 e4, e6, c1, c2, c3, c4

      dtr = 4.d0 * datan(1.d0)/180.d0
      epsq = esq/(1.d0 - esq)

c calculate the zone no. and long0

      n = (180 + tlon(1))/6+1
      zone = n
      long0 = float(n-1)*6.d0 + 3.d0 - 180.d0
      do i = 1, numpts
          lat = tlat(i) * dtr
          long = tlon(i) * dtr
          rn = a/dsqrt(1.d0 - esq*(dsin(lat))**2)
          tanlat = dsin(lat)/dcos(lat)
          t = tanlat * tanlat
          c = epsq * (dcos(lat))**2

          a1 = dcos(lat) * (tlon(i) - long0) * dtr
          a2 = (1.d0 - t + c) * a1**3 / 6.d0
          a3 = (5.d0 - 18.d0*t + t**2 + 72.d0*c - 58.d0*epsq)
     &         * a1**5 / 120.d0

          xadj = 5.d5
          tx(i) = k0 * rn * (a1 + a2 + a3) + xadj + eoffset

          e4 = esq * esq
          e6 = e4 * esq
          c1 = 1.d0 - esq/4.d0 - 3.d0*e4/64.d0 - 5.d0*e6/256.d0
          c2 = 3.d0*esq/8.d0 + 3.d0*e4/32.d0 + 45.d0*e6/1024.d0
          c3 = 15.d0*e4/256.d0 + 45.d0*e6/1024.d0
          c4 = 35.d0*e6/3072.d0
          rm = a*(c1*lat - c2*dsin(2.d0*lat) + c3*dsin(4.d0*lat)
     &                   - c4*dsin(6.d0*lat))
          rm0 = 0.0
          b1 = (a1**2)/2.d0
     &         + (5.d0 - t + 9.d0*c + 4.d0 * c**2) * a1**4 / 24.d0
          b2 = (61.d0 - 58.d0*t + t**2 + 600.d0*c
     &                + 330.d0*epsq) * a1**6 / 720.d0
          yadj = 1.d7
          ty(i) = k0 * (rm - rm0 + rn*tanlat*(b1 + b2)) + yadj +
     &            noffset
      end do
      return
      end
