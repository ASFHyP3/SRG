      integer*1 d(100000)
      integer h(0:255)
      character*60 file
      double precision sum,sumsq


      if(iargc().lt.6)then
         print *,'usage: hist file len firstline lastline firstbyte lastbyte <mask=-1> <skip=1> <print all bytes=1>'
         stop
      end if

      call getarg(2,file)
      read(file,*)np
      call getarg(3,file)
      read(file,*)i0
      call getarg(4,file)
      read(file,*)i1
      call getarg(5,file)
      read(file,*)j0
      call getarg(6,file)
      read(file,*)j1
      mask=-1
      if(iargc().ge.7)then
         call getarg(7,file)
         read(file,*)mask
      end if
      iskip=1
      if(iargc().ge.8)then
         call getarg(8,file)
         read(file,*)iskip
      end if
      iprint=0
      if(iargc().ge.9)then
         call getarg(9,file)
         read(file,*)iprint
      end if

      call getarg(1,file)

      sum=0.
      sumsq=0.
      n=0
      open(20,file=file,form='unformatted',access='direct',recl=np)

      do i=0,255
         h(i)=0
      end do

      do i=i0,i1
         read(20,rec=i)(d(k),k=1,np)
         do j=j0,j1,iskip
            ibyte=d(j)
            if(ibyte.lt.0)ibyte=ibyte+256
            k=iand(ibyte,mask)
            if(iprint.eq.1)print *,d(j),ibyte,k,sum,sumsq
            h(k)=h(k)+1
            sum=sum+k
            sumsq=sumsq+k*k
            n=n+1
         end do
      end do

      open(21,file='hist.out')
      do i=0,255
         write(21,*)h(i)
      end do
      do i=0,255,8
         print '(1x,8i7)',(h(k),k=i,i+7)
      end do

      print *,'Mean, stdv= ',sngl(sum/n),sngl(dsqrt(sumsq/n-(sum/n)**2))

         end

