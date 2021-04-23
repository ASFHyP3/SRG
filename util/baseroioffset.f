c  baseroioffset - fit a baseline to roi offset values
c     solve for baseline and its change along track

      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER(I-N)

      character*50 f1,f2
      character*80 roiin(100)

      pi=4.d0*atan2(1.d0,1.d0)
      if(iargc().lt.1)then
         print *,'usage: baseroioffset roi.in <bpar correction>'
         stop
      end if

      call getarg(1,f1)
      open(21,file=f1)
      if(iargc().ge.2)then
         call getarg(2,f1)
         read(f1,*)bparcorrection
      else
         bparcorrection=0.
      end if

c  read in roi file
      do i=1,100
         read(21,'(a80)',end=10)roiin(i)
c         if(nq.eq.0)go to 10
c         print *,i,roiin(i)
      end do
 10   print *,'roi lines read: ',i-1

      read(roiin(20),*)re
      read(roiin(22),*)h
      read(roiin(34),*)wvl
      read(roiin(23),*)rho0
      read(roiin(15),*)idum,numbins
      read(roiin(16),*)azdelta,rangedelta
      read(roiin(29),*)fs
      read(roiin(37),*)rslope,rint
      read(roiin(38),*)drslope,drint
      read(roiin(12),*)lines
      read(roiin(28),*)looks
c      nlooks=numbins/len
      dr=299792456./fs/2.
      print *
      print *,'roi derived parameters:'
      print *
      print *,'re=      ',re
      print *,'h =      ',h
      print *,'wvl=     ',wvl
      print *,'rho0=    ',rho0
      print *,'numbins= ',numbins
      print *,'az delta=',azdelta
      print *,'r delta =',rangedelta
      print *,'fs =     ',fs
c      print *,'nlooks = ',nlooks
      print *,'dr =     ',dr
      print *,'r slope= ',rslope
      print *,'r int =  ',rint
      print *,'delta r slope= ',drslope
      print *,'delta r int =  ',drint
      print *,'lines per patch: ',lines
      print *,'azimuth looks in roi: ',looks
      print *

      print *,'Some geometry: '
      print *
c     solve for coordinate rotation beta and hprime
      rho=rho0+numbins*dr/2
      cosbeta=(re**2+(h+re)**2-rho**2)/2.d0/re/(h+re)
      beta=acos(cosbeta)
      costheta=(rho**2+(h+re)**2-re**2)/2.d0/rho/(h+re)
      theta=acos(costheta)
      q=180./pi
      print '(a,3f10.2)',
     +   'Coordinate rotation, center look angle, range: ',beta*q,theta*q,rho
      hprime=rho*cos(beta+theta)

c  bperp from plane-parallel and flat earth
      bperp=-rslope*rho*tan(theta)
      bpar=-(rangedelta+rint+numbins/2.*rslope)*dr+bparcorrection
      b=sqrt(bperp**2+bpar**2)
      alpha=atan2(bperp,bpar)+theta-pi/2.
      bh=b*cos(alpha)
      bv=b*sin(alpha)

      print *
      print *,'Estimated baseline values:'
      print *
      print '(1x,a,2f8.2)','bperp, bpar =',bperp,bpar
      print '(1x,a,f8.2)' ,'alpha =      ',alpha*q
      print '(1x,a,2f8.2)','bh, bv =     ',bh,bv

      ha=wvl*rho*sin(theta)/2./bperp

      print '(1x,a,f8.2)' ,'amb. height =',ha
      print '(1x,a,f8.2)' ,'Suggested delta phase for flattening: ',
     +    -rslope*4.*pi/wvl*dr

c  get delta baseline, what is baseline plus one patch ?
      rslope=rslope+drslope
      rint=rint+drint
      bperp=-rslope*rho*tan(theta)
      bpar=-(rangedelta+rint+numbins/2.*rslope)*dr+bparcorrection
      b=sqrt(bperp**2+bpar**2)
      alpha=atan2(bperp,bpar)+theta-pi/2.
      bhprime=b*cos(alpha)
      bvprime=b*sin(alpha)
      bhdot=(bhprime-bh)/(lines/looks)
      bvdot=(bvprime-bv)/(lines/looks)

      print *
      print '(1x,a,2f10.6)','Baseline convergences bhdot,bvdot: ',bhdot,bvdot
c  check for consistency
c      b=sqrt(bh**2+bv**2)
c      alpha=atan2(bv,bh)


c         print '(a,3f12.3)','Inferred Bperp, Bpar, alpha: ',
c     +        b*cos(theta-alpha),b*sin(theta-alpha),
c     +        alpha*180./3.141592654

      end
