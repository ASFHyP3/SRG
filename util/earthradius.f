c earth radius as a a function of latitude
      implicit double precision (a-h, o-z)
      character*30 arg

      if(iargc().lt.1)then
         print *,'usage: earthradius latitude <xp> <yp> <zp>'
         print *,'  xp, yp, zp are spaceraft position coords in meters'
         stop
      end if

      call getarg(1,arg)
      read(arg,*)alat
      if(iargc().gt.1)then
         call getarg(2,arg)
         read(arg,*)xp
         call getarg(3,arg)
         read(arg,*)yp
         call getarg(4,arg)
         read(arg,*)zp
      else
         xp=0.
         yp=0.
         zp=0.
      end if

      pi=datan2(1.d0,1.d0)*4.d0
      ae=6378145.0
      e=0.08182

      x=abs(ae/sqrt(1-e*e*sin(alat*pi/180.)**2))*cos(alat*pi/180)
      z=abs(ae*(1-e*e)/sqrt(1-e*e*sin(alat*pi/180.)**2))*sin(alat*pi/180)

      r=sqrt(x*x+z*z)

      print *,'Radius: ',r

      if(iargc().gt.1)then
         sc =sqrt(xp*xp+yp*yp+zp*zp)
         height=sc-r
         
         print *,'SC distance, height: ',sc, height
      end if

      end
