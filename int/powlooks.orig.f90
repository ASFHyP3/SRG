!c  powlooks -  average looks in power

      use omp_lib

      complex a(65536),b(65536)
      character*360 fin,fout
      integer*8 filelen,nbytes

!!$      !$omp parallel
!!$      n=omp_get_num_threads()
!!$      !$omp end parallel
      print *, 'Max threads used: ', n

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


      open(21,file=fin,form='unformatted',access='direct',recl=na*8)
      nbytes=filelen(fin)
      print *,nbytes,' bytes in file'
      nd=nbytes/8/na
      print *,'Lines in file: ',nd
      print *,'Output file width, pixels: ',na/la
      open(22,file=fout,form='unformatted',access='direct',recl=na/la*8)
      
      lineout=0
      !$omp parallel do shared(nd,ld,na,la) private(lineout,b,a,jpix,sum) 
      do line=1,nd,ld
         lineout=(line-1)/ld+1
         if(mod(line,1024).eq.1)write(*,*)line,lineout
         do j=1,na
            b(j)=cmplx(0.,0.)
         end do

!c  take looks down
         if(ld.eq.1)then
            read(21,rec=line,err=99)(a(k),k=1,na)
            do j=1,na
               b(j)=cabs(a(j))**2
            end do
         else
         do i=0,ld-1
            read(21,rec=line+i,err=99)(a(k),k=1,na)
            do j=1,na
               b(j)=b(j)+cabs(a(j))**2
            end do
         end do
         end if
!c  take looks across
         jpix=0
         do j=1,na,la
            jpix=jpix+1
            sum=0.
            do k=0,la-1
               sum=sum+real(b(j+k))
            end do
            b(jpix)=cmplx(sqrt(sum),0.)
         end do
         write(22,rec=lineout)(b(k),k=1,na/la)
 99   continue
      end do
      !$omp end parallel do

      end
