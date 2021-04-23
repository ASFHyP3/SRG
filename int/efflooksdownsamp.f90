!c  powlooks -  average looks in power

      use omp_lib

      complex*8, allocatable::  a(:,:),b(:),bsq(:),indata(:,:)
      complex*8 scalearray(1000000)
      character*300 fin,fout
      integer*8 filelen,nbytes,nthrds,ID
      real*8:: t1,t2
      
      !call OMP_SET_NUM_THREADS(2)
      !$omp parallel
      n=omp_get_num_threads()
      !$omp end parallel
      print *, 'Number of threads used: ', n
     
      if(iargc().lt.4)then
         write(*,*)'usage: powlooks infile outfile pixac looksac <looksdn>'
         stop
      end if

      call getarg(3,fin)
      read(fin,*)na
      call getarg(4,fin)
      read(fin,*)la
      if(iargc().ge.5)then
         call getarg(5,fin)
         read(fin,*)ld
      else
         ld=la
      end if
      call getarg(1,fin)
      call getarg(2,fout)
         

      open(21,file=fin,form='unformatted',access='direct',recl=na*8*ld)
      nbytes=filelen(fin)
      print *,nbytes,' bytes in file'
      nd=nbytes/8/na
      print *,'Lines in file: ',nd
      print *,'Output file width, pixels: ',na/la
      open(22,file=fout,form='unformatted',access='direct',recl=na/la*8)

!c  set up a scale
      read(21,rec=nd/ld/2)(scalearray(k),k=1,na*ld)
      scale=sum(cabs(scalearray(1:na*ld)))/na/ld
      print *,'Scale= ',scale
      
      t1=omp_get_wtime()
      !$omp parallel do shared(nd,ld,na,la,scale) private(j,lineout,b,bsq,a,jpix,sum1,sum1sq) 
      do line=1,nd/ld
         !c lineout=(line-1)/ld+1
         if(mod(line,1000).eq.1) print *,line
         !c allocate the local arrays
         allocate (a(na,ld),b(na),bsq(na))

         b=cmplx(0.,0.)
         bsq=cmplx(0.,0.)

!c  take looks down        
         read(21,rec=line,err=99)a
         a=a/scale
         b=sum(cabs(a)**2,2)
         bsq=sum(cabs(a)**4,2)

!c  take looks across
         jpix=0
         do j=1,na,la
            jpix=jpix+1
            sum1=0.
            sum1sq=0.
            do k=0,la-1
               sum1=sum1+real(b(j+k))
               sum1sq=sum1sq+real(bsq(j+k))
            end do
!c            b(jpix)=cmplx(sqrt(sum1),0.)
            sum1=sum1/la/ld
            sum1sq=sum1sq/la/ld
!c            eff(kk)=cmplx(sum(kk)**2/(sumsq(kk)-sum(kk)**2),0.)
            b(jpix)=cmplx(sum1**2/(sum1sq-sum1**2),0.)
!            b(jpix)=cmplx(sum1,sum1sq)
         end do
!c write out data
         write(22,rec=line)(b(k),k=1,na/la)
 99   continue
         deallocate(a,b,bsq)
      end do
      !$omp end parallel do
      t2=omp_get_wtime()

      print*,"Time Taken -->", real(t2-t1)

      end
