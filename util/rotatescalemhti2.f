c  rotatescale - register sar images to topo data

      parameter (nmax=3601,mmax=3601)

      character*80 f1,f2,fout,str
      character*1  flipv
      byte data1(nmax,nmax),data2(nmax,nmax),toposigma(mmax,mmax)
      integer statb(13),stat

      if(iargc().lt.1)then
         print *,'usage: rotatescale sarimage sarwidth topoimage topowidth rotation'
         print *,'       flip-v posting sarrange sarazimuth rshift azshift'
         stop
      end if

      call getarg(5,str)
      read(str,*)rot
      call getarg(4,str)
      read(str,*)lentopo
      call getarg(2,str)
      read(str,*)lensar
      call getarg(1,f1)
      call getarg(3,f2)
      call getarg(6,flipv)
      call getarg(7,str)
      read(str,*)posting
      call getarg(8,str)
      read(str,*)rspace
      call getarg(9,str)
      read(str,*)azspace
      call getarg(10,str)
      read(str,*)rshift
      call getarg(11,str)
      read(str,*)azshift

c  get some scale factors
      rscale=rspace/posting
      azscale=azspace/posting

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
            toposigma(i,j)=0
         end do
      end do

c  read in sar image first
      open(21,file=f1,access='direct',recl=lensar)
      ierr=stat(f1,statb)
      linessar=statb(8)/lensar
      print *,'Lines in sar image: ',linessar

      do i=1,linessar
         if(mod(i,200).eq.0)print *,'reading ',i
         read(21,rec=i)(data1(j,i+nmax/2-lensar/2),j=nmax/2-lensar/2,nmax/2-lensar/2+lensar-1)
      end do
      
      print *,'SAR data read in.'


c  read in topo image
      open(22,file=f2,access='direct',recl=lentopo)
      ierr=fstat(22,statb)
      linestopo=statb(8)/lentopo
      print *,'Lines in topo image: ',linestopo

      do i=1,linestopo
         if(mod(i,200).eq.0)print *,'reading ',i
         ii=i
         if(flipv.eq.'y')ii=linestopo-i+1
         if(flipv.eq.'y'.and.i.eq.1)print *,'Flipping vertically'
         read(22,rec=ii)(toposigma(j,i+mmax/2-lentopo/2),j=mmax/2-lentopo/2,mmax/2-lentopo/2+lentopo-1)
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
            data2(j,i)=toposigma(jin,iin)
         end do
      end do

c  combine images
      print *,'Combining.'

      do j=1,nmax
         do i=1,nmax
            data2(i,j)=iand(iand(data1(i,j),240)+(iand(data2(i,j),240)/16),255)
         end do
      end do

      open(31,file=fout,access='direct',recl=nmax)
      do i=1,nmax
         write(31,rec=i)(data2(j,i),j=1,nmax)
      end do

      end



