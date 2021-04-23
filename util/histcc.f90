      real*4 d(32768),pdf(0:99)
      integer h(0:99)
      character*30 file
      double precision sum,sumsq


      if(iargc().lt.2)then
         print *,'usage: histcc file len <firstline> <lastline> <firstpix> <lastpix>'
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
      call getarg(1,file)

      sum=0.
      sumsq=0.
      n=0
      open(20,file=file,form='unformatted',access='direct',status='old',recl=np*8)

      do i=0,99
         h(i)=0
      end do

      do i=i0,i1
!        print *,'rec ',i,np,file
         read(20,rec=i,err=99)(d(k),k=1,np*2)
         do j=j0,j1
            k=int(d(j+np)*100.)
            if(k.gt.99)k=99
            if(k.lt.0)k=0
            h(k)=h(k)+1
!            print *,i,j,d(j+np),k
            sum=sum+d(j+np)
            sumsq=sumsq+d(j+np)**2
            n=n+1
         end do
      end do
 99   continue

      total=0.
      do i=0,99
         total=total+h(i)
      end do
      do i=0,99
      pdf(i)=h(i)/total
      end do

      open(21,file='hist.out')
      do i=0,99
         write(21,*)h(i)
      end do
      close(21)
      open(21,file='hist.pdf')
      do i=0,99
         write(21,*)pdf(i)
      end do
      do i=0,99,10
         print '(1x,10i6)',(h(k),k=i,i+9)
      end do

      print *,'Mean, stdv= ',sngl(sum/n),sngl(dsqrt(sumsq/n-(sum/n)**2))
      
      end

