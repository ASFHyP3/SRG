      integer*2 d(163840)
      integer h(-65536:65536)
      character*30 file
      double precision sum,sumsq


      if(iargc().lt.6)then
         write(*,*)'usage: histi2 file len firstline lastline firstpix lastpix <mask>'
         stop
      end if

      mask=65535
      if(iargc().ge.7)then
         call getarg(7,file)
         read(file,*)mask
      end if
      call getarg(6,file)
      read(file,*)j1
      call getarg(5,file)
      read(file,*)j0
      call getarg(4,file)
      read(file,*)i1
      call getarg(3,file)
      read(file,*)i0
      call getarg(2,file)
      read(file,*)np
      call getarg(1,file)

      sum=0.
      sumsq=0.
      n=0
      open(20,file=file,form='unformatted',status='old',
     1       access='direct',recl=np*2)

         do i=-65536,65536
            h(i)=0
         end do

         do i=i0,i1
            read(20,rec=i)(d(k),k=1,np)
            do j=j0,j1
               k=iand(d(j),mask)
               k=d(j)
c               write(*,*)k
c               if(k.lt.0)k=k+32768
               h(k)=h(k)+1
               sum=sum+k
               sumsq=sumsq+k*k
               n=n+1
            end do
         end do

         open(21,file='hist.out')
         do i=-65536,65536
            write(21,*)h(i)
         end do
         close(21)
         do i=int(sum/n-400),int(sum/n+399),8
         write(*,'(1x,8i7)')(h(k),k=i,i+7)
         end do
         do i=-65536,65536
            if(h(i).ne.0)print *,i,h(i)
         end do
         write(*,*)'Number of points: ',n,sum/n,sumsq/n
         write(*,*)'Mean, stdv= ',
     1          sngl(sum/n),sngl(dsqrt(sumsq/n-(sum/n)**2))

         end

