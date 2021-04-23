c  dem2radar - put dem into radar coordinates

      parameter (nmax=3601,mmax=3601)

      character*80 f1,f2,fout,str,roifile
      character*80 roiin(200)
      character*1  flipv
      real data1(nmax,nmax)
      complex data2(nmax,nmax)
      integer*2 toposigma(mmax,mmax),rottopo(mmax,mmax),topo(mmax,mmax)
      integer statb(13),stat
      complex sarin(10000)

      if(iargc().lt.1)then
         print *,'usage: dem2radar sarimage sarwidth demi2 demwidth rotation'
         print *,'       flip-v demposting rshift azshift roi.in'
         stop
      end if

      call getarg(1,f1)
      call getarg(2,str)
      read(str,*)lensar
      call getarg(3,f2)
      call getarg(4,str)
      read(str,*)lentopo
      call getarg(5,str)
      read(str,*)rot
      call getarg(6,flipv)
      call getarg(7,str)
      read(str,*)posting
      call getarg(8,str)
      read(str,*)rshift
      call getarg(9,str)
      read(str,*)azshift
      call getarg(10,roifile)

c  get parameters from roi.in file
      open(21,file=roifile)
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
      nrlooks=numbins/lensar
      dr=299792456./fs/2.*nrlooks
      read(roiin(21),*)v
      read(roiin(24),*)prf
      daz=v/prf*(re/(h+re))*looks*nrlooks
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
      print *,'nrlooks = ',nlooks
      print *,'dr =     ',dr
      print *,'daz =     ',daz
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
      q=180./3.14159265359
      print '(a,3f10.2)',
     +   'Coordinate rotation, center look angle, range: ',beta*q,theta*q,rho
      hprime=rho*cos(beta+theta)


c  get some scale factors
      rscale=rspace/posting
      azscale=azspace/posting
      rscale=1.
      azscale=1.

      print *,'Rscale, azscale = ',rscale,azscale

      fout='simsar.out'

c  zero out arrays
      do j=1,nmax
         do i=1,nmax
            data1(i,j)=0
            data2(i,j)=0
         end do
      end do
      do j=1,mmax
         do i=1,mmax
            topo(i,j)=0
         end do
      end do

c  read in sar image first
      open(21,file=f1,access='direct',recl=lensar*8)
      ierr=stat(f1,statb)
      linessar=statb(8)/lensar/8
      print *,'Lines in sar image: ',linessar

      do i=1,linessar
         if(mod(i,200).eq.0)print *,'reading ',i
         read(21,rec=i)(sarin(k),k=1,lensar)
         do j=1,lensar
            data1(j+nmax/2-lensar/2,i+nmax/2-linessar/2)=cabs(sarin(j))
         end do
      end do
      
      print *,'SAR data read in.'


c  read in topo image
      open(22,file=f2,access='direct',recl=lentopo*2)
      ierr=fstat(22,statb)
      linestopo=statb(8)/lentopo/2
      print *,'Lines in topo image: ',linestopo

      do i=1,linestopo
         if(mod(i,200).eq.0)print *,'reading ',i
         ii=i
         if(flipv.eq.'y')ii=linestopo-i+1
         if(flipv.eq.'y'.and.i.eq.1)print *,'Flipping vertically'
         read(22,rec=ii)(topo(j,i+mmax/2-lentopo/2),j=mmax/2-lentopo/2,mmax/2+lentopo/2-1)
      end do

      print *,'Data read in, rotating.'

c  rotate topo image first, looping over output coordinates
      pi=3.14159265259
      co=cos(rot*pi/180.)
      si=sin(rot*pi/180.)
      do i=1,nmax
         do j=1,nmax
            xout=j-nmax/2+rshift
            yout=i-nmax/2+azshift
            xout=xout*rscale
            yout=yout*azscale
            xin=xout*co+yout*si
            yin=-xout*si+yout*co
            jin=nint(xin)+nmax/2
            iin=nint(yin)+nmax/2
            if(jin.lt.1)jin=1
            if(jin.gt.mmax)jin=mmax
            if(iin.lt.1)iin=1
            if(iin.gt.mmax)iin=mmax
            rottopo(j,i)=topo(jin,iin)
         end do
      end do

c  convert ground to slant range

      beta0=acos((rho**2-(h+re)**2-re**2)/(2*re*(h+re)))
      y0=re*sin(beta0)   ! ground range to first sar point
      do i=1,linessar
         do j=mmax/2-lensar/2,mmax/2-lensar/2+lensar-1
            y=(j-1)*posting+y0
            beta=asin(y/re)
            rho=sqrt((re+h)**2+(re+rottopo(j,i))**2-
     1          2*(re+h)*(re+rottopo(j,i))*cos(beta))
         end do
      end do

c  combine images
      print *,'Combining.'

c  differentiate to get shaded relief
      first=0
      second=0
      k=0
      do i=1,mmax
         do j=2,mmax
            toposigma(j,i)=rottopo(j,i)-rottopo(j-1,i)
            first=first+toposigma(j,i)
            second=second+toposigma(j,i)**2
            k=k+1
         end do
         toposigma(1,i)=0
      end do
      first=first/k
      stdv=sqrt(second/k-first**2)
      do i=1,mmax
         do j=2,mmax
            if(toposigma(j,i).gt.first+2*stdv)toposigma(j,i)=first+2*stdv
         end do
      end do

      do j=1,nmax
         do i=1,nmax
            data2(i,j)=cmplx(data1(i,j),float(toposigma(i,j)))
c            data2(i,j)=cmplx(data1(i,j),0)
         end do
      end do

      open(31,file=fout,access='direct',recl=nmax*8)
      do i=1,nmax
         write(31,rec=i)(data2(j,i),j=1,nmax)
      end do

      end



