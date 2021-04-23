      complex d(32768)
      integer, allocatable :: h(:)
      character*30 file
      double precision sum,sumsq


      if(iargc().lt.2)then
         print *,'usage: histpow file len <firstline> <lastline> <firstpix> <lastpix> <bins>'
         stop
      end if

      call getarg(2,file)
      read(file,*)np
      i0=1
      if(iargc().ge.3)then
         call getarg(3,file)
         read(file,*)i0
      end if
      i1=1000000
      if(iargc().ge.4)then
         call getarg(4,file)
         read(file,*)i1
      end if
      j0=1
      if(iargc().ge.5)then
         call getarg(5,file)
         read(file,*)j0
      end if
      j1=np
      if(iargc().ge.6)then
         call getarg(6,file)
         read(file,*)j1
      end if
      nbins=1000
      if(iargc().ge.7)then
         call getarg(7,file)
         read(file,*)nbins
      end if
      call getarg(1,file)

      allocate (h(0:nbins-1))

      sum=0.
      sumsq=0.
      n=0
      open(20,file=file,form='unformatted',access='direct',status='old',recl=np*8)

      do i=0,nbins-1
         h(i)=0
      end do

!  estimate the max value to plot on histogram
      powmax=0.
      do i=i0,i1,10
         read(20,rec=i,err=98)(d(k),k=1,np)
         do j=j0,j1,10
            sum=sum+cabs(d(j))**2
            sumsq=sumsq+cabs(d(j))**4
            n=n+1
         end do
      end do
      powmax=(sum/n)+3*dsqrt(sumsq/n-(sum/n)**2)
 98   print *,'Approx. max power to plot= ',powmax

      sum=0.
      sumsq=0.
      n=0
      do i=i0,i1
         read(20,rec=i,err=99)(d(k),k=1,np)
         do j=j0,j1
            k=int(cabs(d(j))**2/powmax*float(nbins))
            if(k.gt.nbins-1)k=nbins-1
            if(k.lt.0)k=0
            h(k)=h(k)+1
            sum=sum+cabs(d(j))**2
            sumsq=sumsq+cabs(d(j))**4
            n=n+1
         end do
      end do
 99   continue

      open(21,file='histpow.out')
      do i=0,nbins-1
         write(21,*)powmax*i/nbins,h(i)
      end do
      do i=0,nbins-20,20
         print '(1x,20i6)',(h(k),k=i,i+19)
      end do

      print *,'Mean, stdv= ',sngl(sum/n),sngl(dsqrt(sumsq/n-(sum/n)**2))
      
      end

