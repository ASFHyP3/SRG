      real d(100000)
      integer h(0:999)
      character*300 file,str
      double precision sum,sumsq


      if(iargc().lt.2)then
         print *,'usage: histphase file len <phasemax=pi> <phasemin=-pi> <firstline=1> <lastline=end> <firstpix=1> <lastpix=len>'
         stop
      end if

      call getarg(2,file)
      read(file,*)np
      phasemax=3.14159265359
      if(iargc().ge.3)then
         call getarg(3,str)
         read(str,*)phasemax
      end if
      phasemin=-3.14159265359
      if(iargc().ge.4)then
         call getarg(4,str)
         read(str,*)phasemin
      end if
      i0=1
      if(iargc().ge.5)then
         call getarg(5,file)
         read(file,*)i0
      end if
      i1=1000000
      if(iargc().ge.6)then
         call getarg(6,file)
         read(file,*)i1
      end if
      j0=1
      if(iargc().ge.7)then
         call getarg(7,file)
         read(file,*)j0
      end if
      j1=np
      if(iargc().ge.8)then
         call getarg(8,file)
         read(file,*)j1
      end if
      call getarg(1,file)

      sum=0.
      sumsq=0.
      n=0
      open(20,file=file,form='unformatted',access='direct',status='old',recl=np*8)

      do i=0,999
         h(i)=0
      end do

      do i=i0,i1
!        print *,'rec ',i,np,file
         read(20,rec=i,err=99)(d(k),k=1,np*2)
         do j=j0+np,j1+np
            k=nint((d(j)-phasemin)/(phasemax-phasemin)*1000.)
            if(k.gt.999)k=999
            if(k.lt.0)k=0
            h(k)=h(k)+1
!            print *,i,j,d(j+np),k
            sum=sum+(d(j))
            sumsq=sumsq+(d(j))**2
            n=n+1
         end do
      end do
 99   continue

      open(21,file='histphase.out')
      do i=0,999
         write(21,*)phasemin+(phasemax-phasemin)*i/1000.,h(i)
      end do
      do i=0,999,20
         print '(1x,20i6)',(h(k),k=i,i+19)
      end do

      print *,'Mean, stdv= ',sngl(sum/n),sngl(dsqrt(sumsq/n-(sum/n)**2))
      
      end

