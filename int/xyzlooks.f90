!c  doublelooks -  average r*8 looks

      use omp_lib

      real*8, allocatable::  a(:,:,:),b(:,:)
      character*300 fin,fout
      integer*8 filelen,nbytes,nthrds,ID
      real*8:: t1,t2,sum1,sum2,sum3
      
      !call OMP_SET_NUM_THREADS(2)
      !$omp parallel
      n=omp_get_num_threads()
      !$omp end parallel
      print *, 'Number of threads used: ', n
     
      if(iargc().lt.4)then
         write(*,*)'usage: xyzlooks infile outfile pixac looksac <looksdn>'
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
         

      open(21,file=fin,form='unformatted',access='direct',recl=na*8*ld*3)
      nbytes=filelen(fin)
      print *,nbytes,' bytes in file'
      nd=nbytes/8/na/3
      print *,'Lines in file: ',nd
      print *,'Output file width, pixels: ',na/la
      open(22,file=fout,form='unformatted',access='direct',recl=na/la*8*3)
      
      
      t1=omp_get_wtime()
      !$omp parallel do shared(nd,ld,na,la) private(lineout,b,a,jpix,sum1,sum2,sum3,i) 
      do line=1,nd/ld
         !c lineout=(line-1)/ld+1
         if(mod(line,1000).eq.1) print *,line
         !c allocate the local arrays
         allocate (a(3,na,ld),b(3,na))

         b=0.d0
         
!c  take looks down        
         read(21,rec=line,err=99)a
         b=sum(a,3)/ld
!         print *,b(:,20000:20010)
                 
!c  take looks across
         jpix=0
         do j=1,na,la
            jpix=jpix+1
            b(1,jpix)=sum(b(1,j:j+la-1))/la
            b(2,jpix)=sum(b(2,j:j+la-1))/la
            b(3,jpix)=sum(b(3,j:j+la-1))/la
!            print *,sum1,sum2,sum3
!            sum1=0.
!            sum2=0.
!            sum3=0.
!            do k=0,la-1
!               sum1=sum1+b(1,j+k)
!               sum2=sum1+b(2,j+k)
!               sum3=sum1+b(3,j+k)
!            end do
            end do
      write(22,rec=line)b(:,1:na/la)
 99   continue
         deallocate(a,b)
      end do
      !$omp end parallel do
      t2=omp_get_wtime()

      print*,"Time Taken -->", real(t2-t1)

      end
