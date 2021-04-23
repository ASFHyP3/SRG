c  dem2radar - convert a dem i*2 file from dem fixed spacing to radar coords

      parameter (mmax=3601)

      character*80 f1,str,ftopo,froi,roiin(100),fsar
      character*1 flipv
      integer*2 topo(mmax,mmax),temp(mmax,mmax),t(mmax,mmax),rottopo(mmax,mmax)
      integer statb(13),stat
      integer*2 mht(mmax)
      real val(2),dist(2)
      real sar(mmax,mmax)
      complex sarin(10000),out(10000)

      pi=datan2(1.d0,1.d0)*4

      if(iargc().lt.1)then
         print *,'usage: dem2radar sarimage width outimage outlines topo-i2 topowidth posting rotation rshift azshift roi.in flip-v'
         stop
      end if

      call getarg(1,fsar)
      call getarg(2,str)
      read(str,*)lensar
      call getarg(3,f1)
      call getarg(4,str)
      read(str,*)linesout
      call getarg(5,ftopo)
      call getarg(6,str)
      read(str,*)lentopo
      call getarg(7,str)
      read(str,*)posting
      call getarg(8,str)
      read(str,*)rot
      call getarg(9,str)
      read(str,*)rshift
      call getarg(10,str)
      read(str,*)azshift
      call getarg(11,froi)
      call getarg(12,flipv)

c  save the parameters
      open(31,file='dem2radar.params')
      write(31,*)fsar
      write(31,*)lensar
      write(31,*)f1
      write(31,*)linesout
      write(31,*)ftopo
      write(31,*)lentopo
      write(31,*)posting
      write(31,*)rot
      write(31,*)rshift
      write(31,*)azshift
      write(31,*)froi
      write(31,*)flipv
      close(31)

c  open output file
      open(32,file=f1,access='direct',recl=lensar*2)
      open(33,file='dem2radar.mht',access='direct',recl=lensar*8)

c  get the sar image
      ierr=stat(fsar,statb)
      linessar=statb(8)/8/lensar
      print *,'Lines in sar image: ',linessar
      open(21,file=fsar,access='direct',recl=lensar*8)
      do i=1,linessar
         read(21,rec=i)(sarin(k),k=1,lensar)
         do j=1,lensar
            sar(j,i)=cabs(sarin(j))
         end do
      end do
      close(21)
      open(66,file='qsar',access='direct',recl=linessar*lensar*4)
      write(66,rec=1)((sar(j,i),i=1,linessar),j=1,lensar)
      close(66)
   
c  input roi data
      open(21,file=froi)

      do i=1,100
         read(21,'(a80)',end=10)roiin(i)
c         print *,i,roiin(i)
      end do
 10   print *,'roi lines read: ',i-1
      close(21)

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
      read(roiin(21),*)v
      read(roiin(24),*)prf
      dr=299792456./fs/2.*numbins/lensar
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
      print *,'nlooks = ',nlooks
      print *,'dr =     ',dr
      print *,'r slope= ',rslope
      print *,'r int =  ',rint
      print *,'velocity=',v
      print *,'prf=     ',prf
      print *,'delta r slope= ',drslope
      print *,'delta r int =  ',drint
      print *,'lines per patch: ',lines
      print *,'azimuth looks in roi: ',looks
      print *

      print *,'Some geometry: '
      print *
c  solve for coordinate rotation beta and hprime
      rho=rho0+numbins*dr/2
      cosbeta=(re**2+(h+re)**2-rho**2)/2.d0/re/(h+re)
      beta=acos(cosbeta)
      costheta=(rho**2+(h+re)**2-re**2)/2.d0/rho/(h+re)
      theta=acos(costheta)
      q=180./pi
      print *,'rho,rho0,numbins,dr'
      print *,rho,rho0,numbins,dr
      print '(a,2f10.2)',
     +   'Coordinate rotation, center look angle: ',beta*q,theta*q
      hprime=rho*cos(beta+theta)


c  get some scale factors
      rscale=dr/posting
      daz=(v/prf)*(re/(h+re))*looks*numbins/lensar
      azscale=daz/posting

      print *,'Rscale, azscale = ',rscale,azscale

      azspace=(v/prf)*(re/(h+re))*looks
      print *,'Output image azimuth spacing: ',azspace
c  read in heights
      ierr=stat(ftopo,statb)
      linestopo=statb(8)/2/lentopo
      print *,'Lines in topo image: ',linestopo

      open(23,file=ftopo,access='direct',recl=lentopo*2)

      do i=1,linestopo
         if(mod(i,200).eq.0)print *,'reading ',i
         ii=i
         if(flipv.eq.'y')ii=linestopo-i+1
         read(23,rec=ii)(topo(j,i),j=1,lentopo)
      end do

c  rotate and offset topo data
      pi=3.14159265259
      co=cos(rot*pi/180.)
      si=sin(rot*pi/180.)
      do i=1,mmax
         do j=1,mmax
            xout=j-mmax/2+rshift
            yout=i-mmax/2+azshift
c            xout=xout*rscale
c            yout=yout*azscale
            xin=xout*co+yout*si
            yin=-xout*si+yout*co
            jin=nint(xin)+mmax/2
            iin=nint(yin)+mmax/2
            if(jin.lt.1)jin=1
            if(jin.gt.lentopo)jin=lentopo
            if(iin.lt.1)iin=1
            if(iin.gt.linestopo)iin=linestopo
            rottopo(j,i)=topo(jin,iin)
         end do
      end do

      open(66,file='rottopo.out',access='direct',recl=mmax*mmax*2)
      write(66,rec=1)rottopo
      close(66)
      
c  first convert from ground to slant range for each line
      cosbeta=(re**2+(h+re)**2-rho0**2)/2.d0/re/(h+re)
      beta=acos(cosbeta)
      y0=re*beta
      print *,'Using y0 as: ',y0

      print *,'Converting to slant range.'
      do i=1,mmax
         if(mod(i,200).eq.0)print *,'At line ',i
c  zero out mht array
         do j=1,lensar
            temp(j,i)=0.
         end do

         do j=1,mmax
c  first get ground range
            y=y0+(j-1)*posting
            beta=y/re
            height=re+rottopo(j,i)
            rho=sqrt(-cos(beta)*2.d0*height*(h+re)+height**2+(h+re)**2)
            jbin=(rho-rho0)/dr+1
c            if(i.eq.990)print *,j,height,rho,jbin,topo(j,i)
            if(jbin.gt.lensar)jbin=lensar
            if(jbin.lt.1)jbin=1

            temp(jbin,i)=rottopo(j,i)
         end do

         if(k.eq.k)go to 99
c  fill in zeroes
c       loop over pixels
	   do j=2,mmax-1
	      iflag=1		!grow to edge flag
c       is it a zero pixel
	      if(temp(j,i).eq.0)then
		 do k=1,2
		    val(k)=0
		    dist(k)=0
		 end do
c       scan left
		 do k=j,1,-1
		    if(temp(k,i).ne.0.)go to 31
		 end do
		 iflag=0
		 go to 30
 31		 val(1)=temp(k,i)
		 dist(1)=j-k
 30		 continue
c       scan right
		 do k=j,mmax
		    if(temp(k,i).ne.0.)go to 41
		 end do
		 iflag=0
		 go to 40
 41		 val(2)=temp(k,i)
		 dist(2)=k-j
 40		 continue
c       do we grow to this point?
		 if(iflag.eq.1)then
c       get weighted sum
		    wgt=0
		    sum=0
		    do k=1,2
		       if(dist(k).gt.0.1)then
			  wgt=wgt+1./dist(k)
			  sum=sum+val(k)/dist(k)
		       end if
		    end do
		    if(wgt.ne.0.0)then
		       t(j,i)=sum/wgt
		    end if
		 end if
	      else
		 t(j,i)=temp(j,i)
	      end if
	   end do
 99        continue
      end do

      open(66,file='temp.out',access='direct',recl=mmax*mmax*2)
      write(66,rec=1)temp
      close(66)


c  resample each line in azimuth direction
      do line=1,linesout
         if(mod(line,200).eq.0)print *,'azimuth resampling at line ',line
         azloc=(line-1)*daz
c  how many lines in topo file does this correcpond to ?
         realline=azloc/posting+1
c  interpolate in azimuth
         frac=realline-int(realline)
         do i=1,lensar
            mht(i)= temp(i,int(realline))*(1-frac)+temp(i,int(realline)+1)*frac
            if(i.ge.2)then
               delta=mht(i)-mht(i-1)
            else
               delta=0
            end if
            out(i)=cmplx(sar(i,line),delta)
         end do
         write(32,rec=line)(mht(k),k=1,lensar)
         write(33,rec=line)(out(k),k=1,lensar)
      end do


      end



