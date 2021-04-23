c  shadei2hgt - generate a shaded relief map in mht format from an integer dem

      parameter (maxsize=5120)
      integer*2 indata(maxsize*maxsize),inputline(4*maxsize)
      real data(maxsize*maxsize)
      real hgt(maxsize*maxsize)
      real in
      character*30 f
      character*1 ians
      integer istat(13)
      common np,indata

      if(iargc().lt.2)then
         print *,'usage: shadei2hgt i2DEM length <shadescale(1)> <shademean(128)> <firstline> <nlines> <firstpix> <npix>'
         stop
      end if

      call getarg(2,f)
      read(f,*)npix
      call getarg(1,f)
      call stat(f,istat)
      lines=istat(8)/npix/2
      print *,'Lines in file: ',lines

      if(iargc().ge.3)then
         call getarg(3,f)
         read(f,*)shadescale
      else
         shadescale=1.
      end if
      if(iargc().ge.4)then
         call getarg(4,f)
         read(f,*)shademean
      else
         shademean=128.
      end if
      if(iargc().ge.5)then
         call getarg(5,f)
         read(f,*)line0
      else
         line0=1
      end if
      if(iargc().ge.6)then
         call getarg(6,f)
         read(f,*)lines
      end if
      if(iargc().ge.7)then
         call getarg(7,f)
         read(f,*)ip0
      else
         ip0=1
      end if
      if(iargc().ge.8)then
         call getarg(8,f)
         read(f,*)np
      else
         np=npix-ip0+1
      end if
      call getarg(1,f)
      
      open(21,file=f,form='unformatted',status='old',
     +   access='direct',recl=npix*2)
      ii=0
      do i=line0,line0+lines-1
         ii=ii+1
         read(21,rec=i)(inputline(k),k=1,npix)
         do k=1,np
            indata(k+(ii-1)*np)=inputline(k+ip0-1)
         end do
      end do

      print '(a,$)',' Illuminate from top (t), bottom (b), right (r), left(l): '
      read(*,'(a)')ians

      do i=2,lines-1
         do j=2,np-1
            jj=j
            if(ians.eq.'t')then
            sum=-in(jj-1,i-1)-in(jj,i-1)-in(jj+1,i-1)+
     +          in(jj-1,i)+in(jj,i)+in(jj+1,i)
            else if(ians.eq.'l')then
            sum=-in(jj-1,i-1)-in(jj-1,i)-in(jj-1,i+1)+
     +          in(jj+1,i-1)+in(jj+1,i)+in(jj+1,i+1)
            else if(ians.eq.'b')then
            sum=in(jj-1,i-1)+in(jj,i-1)+in(jj+1,i-1)-
     +          in(jj-1,i)-in(jj,i)-in(jj+1,i)
            else if(ians.eq.'r')then
            sum=in(jj-1,i-1)+in(jj-1,i)+in(jj-1,i+1)-
     +          in(jj+1,i-1)-in(jj+1,i)-in(jj+1,i+1)
            end if
         data((i-1)*np+j)=sum
         hgt((i-1)*np+j)=in(jj,i)
         end do
      end do

      datamax=-1.e20
      datamin=1.e20
      hgtmax=-1.e20
      hgtmin=1.e20
      sum=0.
      sumsq=0.
      do i=1,np*lines
         datamax=max(datamax,data(i))
         datamin=min(datamin,data(i))
         hgtmax=max(hgtmax,hgt(i))
         hgtmin=min(hgtmin,hgt(i))
         sum=sum+data(i)
         sumsq=sumsq+data(i)**2
      end do
      print *,'datamin,datamax,hgtmin,hgtmax',datamin,datamax,hgtmin,hgtmax
      datamean=sum/np/lines
      datavar=sumsq/np/lines-datamean**2
      print *,'mag mean, var, sigma: ',datamean,datavar,sqrt(datavar)
      do i=1,np*lines
         data(i)=data(i)*shadescale+shademean
         data(i)=max(data(i),0.)
         data(i)=min(data(i),shademean*2)
      end do

      open(22,file='shade.mht',form='unformatted',status='unknown',
     +        access='direct',recl=np*8)
      do line=1,lines
      write(22,rec=line)(data(k),k=line*np-np+1,line*np),(hgt(k),k=line*np-np+1,line*np)
      end do

      end


      real function in(i,j)
      parameter (maxsize=5120)
      integer*2 indata(maxsize*maxsize)
      common np,indata
      in=indata(i+(j-1)*np)
      return
      end
