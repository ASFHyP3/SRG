c  dem2radar - convert a dem i*2 file from dem fixed spacing to radar coords

      parameter (mmax=3601)

      character*80 f1,str,ftopo,froi,roiin(100),fsar
      character*1 flipv
      integer*2 topo(mmax,mmax),rottopo(mmax,mmax)
      integer statb(13),stat
      real val(2),dist(2)
      real sar(mmax,mmax)
      complex sarin(10000),out(10000)

      pi=datan2(1.d0,1.d0)*4

      if(iargc().lt.1)then
         print *,'usage: dem2radar sarimage width outimage outlines topo-i2 topowidth latposting lonposting rotation shiftlat shiftlon roi.in flip-v azlooks'
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
      read(str,*)postinglat
      call getarg(8,str)
      read(str,*)postinglon
      call getarg(9,str)
      read(str,*)rot
      call getarg(10,str)
      read(str,*)latshift
      call getarg(11,str)
      read(str,*)lonshift
      call getarg(12,froi)
      call getarg(13,flipv)
      call getarg(14,str)
      read(str,*)iazlooks

c  save the parameters
      open(31,file='dem2radar.params')
      write(31,*)fsar
      write(31,*)lensar
      write(31,*)f1
      write(31,*)linesout
      write(31,*)ftopo
      write(31,*)lentopo
      write(31,*)postinglat
      write(31,*)postinglon
      write(31,*)rot
      write(31,*)latshift
      write(31,*)lonshift
      write(31,*)froi
      write(31,*)flipv
      write(31,*)iazlooks
      close(31)

c  open output file
      open(32,file=f1,access='direct',recl=lensar*2)
      open(33,file='dem2radar.rg',access='direct',recl=lensar*8)

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
c      open(66,file='qsar',access='direct',recl=linessar*lensar*4)
c      write(66,rec=1)((sar(j,i),i=1,linessar),j=1,lensar)
c      close(66)
   
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
      read(roiin(28),*)nlooks
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
      daz=(v/prf)*(re/(h+re))*iazlooks
      azspace=(v/prf)*(re/(h+re))*iazlooks
      print *,'Output image azimuth spacing: ',azspace
c  read in heights
      ierr=stat(ftopo,statb)
      linestopo=statb(8)/2/lentopo
      print *,'Lines in topo image: ',linestopo

      open(23,file=ftopo,access='direct',recl=lentopo*2)

      do i=1,linestopo
         if(mod(i,500).eq.0)print *,'reading ',i
         ii=i
         if(flipv.eq.'y')ii=linestopo-i+1
         read(23,rec=ii)(topo(j,i),j=1,lentopo)
      end do

c  rotate and offset topo data
      pi=3.14159265259
      co=cos(rot*pi/180.)
      si=sin(rot*pi/180.)
c  first convert from ground to slant range for each line
c      cosbeta=(re**2+(h+re)**2-rho0**2)/2.d0/re/(h+re)
c      beta=acos(cosbeta)
c      xground0=re*beta
c      print *,'Using xground0 as: ',xground0
      cosbeta=(re**2+(h+re)**2-(rho0+lensar/2*dr)**2)/2.d0/re/(h+re)
      beta=acos(cosbeta)
      xgroundcenter=re*beta
      print *,'Using xgroundcenter as: ',xgroundcenter

c            print *,"i,j,xground,height,rho,rho0,ix,iy"
      do i=1,linestopo
         do j=1,lentopo
            x=(j-lentopo/2+lonshift)*postinglon
            y=(i-linestopo/2+latshift)*postinglat
            xout=x*co+y*si       !ground range in meters from scene center
            yout=-x*si+y*co
c  convert ground to slant range using elevation
            xground=xgroundcenter+xout
            beta=xground/re
            height=re+topo(j,i)
            rho=sqrt(-cos(beta)*2.d0*height*(h+re)+height**2+(h+re)**2)
            ix=nint((rho-rho0)/dr)
            iy=nint(yout/daz+linessar/2)
c            if(i.gt.1000)print *,i,j,xground,height,rho,rho0,ix,iy
            if(ix.lt.1)ix=1
            if(iy.lt.1)iy=1
            if(ix.gt.lensar)ix=lensar
            if(iy.gt.linessar)iy=linessar
            rottopo(ix,iy)=topo(j,i)
         end do
      end do

      open(66,file='rottopoholes.out',access='direct',recl=mmax*mmax*2)
      write(66,rec=1)rottopo
      close(66)

      call fillsub(rottopo,lensar,linessar)

      open(66,file='rottopo.out',access='direct',recl=mmax*mmax*2)
      write(66,rec=1)rottopo
      close(66)

      do line=1,linessar
         do i=1,lensar
            if(i.ge.2)then
               delta=rottopo(i,line)-rottopo(i-1,line)+15
            else
               delta=0
            end if
            out(i)=cmplx(sar(i,line),delta)
         end do
         write(32,rec=line)(rottopo(k,line),k=1,lensar)
         write(33,rec=line)(out(k),k=1,lensar)
      end do


      end



c  fillsub - subroutine to fill holes in image, i2 format assumed

	subroutine fillsub(array,npix,lines)

	parameter (maxsiz=4096)
        parameter (mmax=3601)
	integer*2 array(mmax,mmax)
	integer*2 d(maxsiz,maxsiz)
	real val(4),dist(4)

c       read in file, copy data into array d
	do i=1,maxsiz
	   do j=1,maxsiz
	      d(j,i)=0
	   end do
	end do
	do i=1,lines
	   do j=1,npix
	      d(j,i)=array(j,i)
              if(d(j,i).le.-999)d(j,i)=0
	   end do
	end do
        sum=0.
        wgt=0.

c       loop over lines
	do i=2,lines-1
c	   if(mod(i,32).eq.0)print *,'filling at line ',i
c       loop over pixels
	   do j=2,npix-1
	      iflag=1		!grow to edge flag
c       is it a zero pixel
	      if(d(j,i).eq.0)then
		 do k=1,4
		    val(k)=0
		    dist(k)=0
		 end do
c       scan up
		 do k=i,1,-1
		    if(d(j,k).ne.0.)go to 11
		 end do
		 iflag=0
		 go to 10
 11		 val(1)=d(j,k)
		 dist(1)=i-k
 10		 continue
c       scan down
		 do k=i,lines
		    if(d(j,k).ne.0.)go to 21
		 end do
		 iflag=0
		 go to 20
 21		 val(2)=d(j,k)
		 dist(2)=k-i
 20		 continue
c       scan left
		 do k=j,1,-1
		    if(d(k,i).ne.0.)go to 31
		 end do
		 iflag=0
		 go to 30
 31		 val(3)=d(k,i)
		 dist(3)=j-k
 30		 continue
c       scan right
		 do k=j,npix
		    if(d(k,i).ne.0.)go to 41
		 end do
		 iflag=0
		 go to 40
 41		 val(4)=d(k,i)
		 dist(4)=k-j
 40		 continue
c       do we grow to this point?
		 if(iflag.eq.1)then
c       get weighted sum
		    wgt=0
		    sum=0
		    do k=1,4
		       if(dist(k).gt.0.1)then
			  wgt=wgt+1./dist(k)
			  sum=sum+val(k)/dist(k)
		       end if
		    end do
		    if(wgt.ne.0.0)then
		       array(j,i)=sum/wgt
		    end if
c       print *,dist
c       print *,val
c       print *,sum,wgt
		 end if
	      else
		 array(j,i)=d(j,i)
	      end if
c              if(i.gt.500)print *,j,i,array(j,i),d(j,i),sum,wgt
	   end do
	end do
        return

	end

				
	
