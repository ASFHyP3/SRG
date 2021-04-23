c  interferometer height program - use roi .in file for parameters
c  invert interferometer data
c  data in amp/ph format with unwrapped phase

        parameter (length=12800)
	implicit double precision (a-h,o-z)
	real height,r(16384),array(length*2)
        dimension cec(length)
	character*30 f,f1,f2
        character*80 roiin(100)

	pi=4.d0*datan2(1.d0,1.d0)

c  open a file
      if(iargc().lt.4)then
         write(*,*)'usage: inverseroi interferogram outfile length roi.in '
         write(*,*)'Note: input file is an argument, not redirected input!!'
         stop
      end if

      write(*,*)'******* !!!!!!!!!!!! ************'
      write(*,*)'Warning!  This program has not been tested under Linux!!'
      write(*,*)'******* !!!!!!!!!!!! ************'
      write(*,*)

      call getarg(3,f1)
      read(f1,*)len
      call getarg(1,f1)
      call getarg(2,f2)
      open(20,file=f1,access='direct',recl=len*8)
      open(21,file=f2,access='direct',recl=len*8)

c  read in roi file
      call getarg(4,f)
      open(10,file=f)
      do i=1,100
c         write(*,*)'read line ',i
         read(10,'(a80)',end=10)roiin(i)
c         write(*,*)i,roiin(i)
      end do
 10   write(*,*)'roi lines read: ',i-1
      close(10)

      read(roiin(20),*)re
      read(roiin(22),*)h
      read(roiin(34),*)wvl
      read(roiin(23),*)srnear
      read(roiin(15),*)idum,numbins
      read(roiin(29),*)fs
      nlooks=numbins/len
      srspace=299792456./fs/2.*nlooks
      write(*,*)'roi derived parameters:'
      write(*,*)'re=      ',re
      write(*,*)'h =      ',h
      write(*,*)'wvl=     ',wvl
      write(*,*)'srnear=  ',srnear
      write(*,*)'numbins= ',numbins
      write(*,*)'fs =     ',fs
      write(*,*)'nlooks = ',nlooks
      write(*,*)'srspace= ',srspace

      print '(a,$)','Enter (real) ambiguity number to add to phases: '
      read(*,*)ramb
      iamb=nint(ramb)
c  get input params
        print '(a,$)','Enter horizontal, vertical baselines: '
        read(*,*)bh,bv
        print '(a,$)','Enter h,v baseline deltas: '
        read(*,*)bhdot,bvdot

        b=sqrt(bh**2+bv**2)
        ang=atan2(bv,bh)
c  get correct arcsin ambiguity
        theta=acos(h/srnear)
        thetaminusalpha=theta-ang
c  is thetaminusalpha less than 90 deg?

c        isign=1
c        if(abs(ang).ge.3.14159265359d0/2.d0)then
c           ang=pi+ang
c           isign=-1
c        end if

        write(*,*)'wvl b ang h srspace'
        print '(6f12.3)',wvl,b,ang,h,srspace
c  get rotation of coordinate system
        rho=srnear+len/2*srspace
        cosbeta=(re**2+(h+re)**2-rho**2)/2.d0/re/(h+re)
        beta=acos(cosbeta)*180.d0/pi
        costheta=(rho**2+(h+re)**2-re**2)/2.d0/rho/(h+re)
        theta=acos(costheta)*180.d0/pi
c  save beta at center of swath as beta0
        betazero = beta
        write(*,*)'Rotation of coordinates is (deg) : ',beta,theta
        hprime=(srnear+len/2*srspace)*cos((beta+theta)*pi/180.d0)
        write(*,*)'h prime = ',hprime
c  read in a sample line
        do j=1,len
           rho=srnear+j*srspace
           cosbeta=(re**2+(h+re)**2-rho**2)/2.d0/re/(h+re)
           beta=acos(cosbeta)*180.d0/pi
           costheta=(rho**2+(h+re)**2-re**2)/2.d0/rho/(h+re)
           theta=acos(costheta)*180.d0/pi
c  for circular earth correction (cec), get distance to swath center
           dist=re*(beta-betazero)*pi/180.d0
           cec(j)=dist**2/2.d0/re
           if(mod(j,100).eq.0)print '(i5,6f10.3)',j,beta,theta,
     +       b*sin((theta-ang)*pi/180.d0),dist,cec(j)
        end do

c  loop over lines
c        write(*,*) iamb
        if(iamb .eq. -99999) then
           read(20,rec=100)(array(k),k=1,2*len)
           b=sqrt((bh+bhdot*100)**2+(bv+bvdot*100)**2)
           ang=atan2(bv+bvdot*100,bh+bhdot*100)
           theta=acos(h/srnear)
           thetaminusalpha=theta-ang
           isign=1
           jsign=1
           if(abs(thetaminusalpha).ge.3.14159265359d0/2.d0)then
              ang=pi+ang
              jsign=-1
c              isign=-1
           end if

           
           do iamb = -1000, 1000, 2
              sr=srnear+(len/6-1)*srspace
              phasecycles=isign*array(len/6+len)/4./pi+iamb
              sinthetaminusalpha=-phasecycles*wvl/b
              if(abs(sinthetaminusalpha).ge.1.0d0)
     +             sinthetaminusalpha=0.999999
              q1=sinthetaminusalpha
              thetaminusalpha=asin(sinthetaminusalpha)
              theta=thetaminusalpha+ang
              theta1 = theta
              height=hprime-sr*dcos(theta+betazero*pi/180.d0)+cec(len
     $             /6)

              sr=srnear+(5*len/6-1)*srspace
              phasecycles=isign*array(5*len/6+len)/4./pi+iamb
              sinthetaminusalpha=-phasecycles*wvl/b
              if(abs(sinthetaminusalpha).ge.1.0d0)
     +             sinthetaminusalpha=0.999999
              q2=sinthetaminusalpha
              thetaminusalpha=asin(sinthetaminusalpha)
              theta=thetaminusalpha+ang
              height2=hprime-sr*dcos(theta+betazero*pi/180.d0)+cec(5*len
     $             /6)
              write(77,*) iamb, (theta1*180.d0/pi), (theta*180
     $             .d0/pi),(height), (height2)
     $             ,sngl(q1),sngl(q2)
           end do

        else
           do line=1,10000
           b=sqrt((bh+bhdot*line)**2+(bv+bvdot*line)**2)
           ang=atan2(bv+bvdot*line,bh+bhdot*line)
           theta=acos(h/srnear)
           thetaminusalpha=theta-ang

           jsign=1
           isign=1
           if(abs(thetaminusalpha).ge.3.14159265359d0/2.d0)then
           ang=pi+ang
           jsign=-1
c           isign=-1
        end if

c     read in a line of data
              read(20,rec=line,err=99)(array(k),k=1,2*len)
c     loop over pixels
              do i=1,len
                 sr=srnear+(i-1)*srspace
c     height for pixel:
                 phasecycles=isign*array(i+len)/4./pi+ramb
                 sinthetaminusalpha=-phasecycles*wvl/b
                 if(abs(sinthetaminusalpha).ge.1.0d0)
     +                sinthetaminusalpha=0.999999
                 thetaminusalpha=asin(sinthetaminusalpha)
                 theta=jsign*thetaminusalpha+ang
                 height=hprime-sr*dcos(theta+betazero*pi/180.d0)+cec(i)
                 
                 array(i+len)=height
                 if(i.eq.100.and.mod(line,32).eq.0)print '(6f11.3)',
     +                sinthetaminusalpha,phasecycles,theta*180.d0/pi
     $                ,height
                 if(i.eq.500.and.mod(line,32).eq.0)print '(1x,6f11.3)',
     +                sinthetaminusalpha,phasecycles,theta*180.d0/pi
     $                ,height
              end do
              write(21,rec=line)(array(k),k=1,2*len)
           end do
        end if
 99     end
      


      real*8function cosd(x)

      implicit none

      real*8 x
      real*8 D2R_
      parameter (D2R_=3.141592654/180.0)

      cosd = cos(x*D2R_)
      end

      real*8function acosd(x)

      implicit none

      real*8 x
      real*8 D2R_
      parameter (D2R_=3.141592654/180.0)

      acosd = acos(x) / D2R_
      end
c
c
c
      real*8function sind(x)

      implicit none

      real*8 x
      real*8 D2R_
      parameter (D2R_=3.141592654/180.0)

      sind = sin(x*D2R_)
      end
c
c
c
      real*8function atan2d(x,y)

      implicit none

      real*8 x,y
      real*8 R2D_
      parameter (R2D_=180.0/3.141592654)

      atan2d = R2D_*atan2(x,y)
      end





