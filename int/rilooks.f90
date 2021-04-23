!c  rilooks -  average real,imaginary looks

      use omp_lib

      complex a(65536),b(65536)
      real b1(65536),b2(65536)
      character*160 fin,fout
      integer statb(13),fstat
      integer*8 filelen,nbytes

      !$omp parallel
      n=omp_get_num_threads()
      !$omp end parallel
      print *, 'Max threads used: ', n

      if(iargc().lt.4)then
         write(*,*)'usage: rilooks infile outfile length looksac <looksdn>'
         stop
      end if

      call getarg(1,fin)
      call getarg(3,fout)
      read(fout,*)na
      open(21,file=fin,form='unformatted',access='direct',recl=na*8)
      ierr=fstat(21,statb)
      nbytes=filelen(fin)
      !print *,nbytes," bytes in file (rilooks)"
!      nd=statb(8)/8/na
      nd=nbytes/8/na
      write(*,*)'rilooks: lines in file: ',nd
      call getarg(4,fout)
      read(fout,*)la
      if(iargc().ge.5)then
         call getarg(5,fout)
         read(fout,*)ld
      else
         ld=la
      end if
      call getarg(2,fout)
      open(22,file=fout,form='unformatted',access='direct',recl=na/la*8)
      
      !$omp parallel do private(b1,b2,a,jpix,sum1,sum2,b,lineout) &
      !$omp shared(nd,ld,na,la)
      do line=1,nd,ld
         if(mod(line,4096).eq.0)write(*,*)line
         lineout=(line-1)/ld+1
         do j=1,na
            b1(j)=0.
            b2(j)=0.
         end do

!c  take looks down
         do i=0,ld-1
            read(21,rec=line+i,err=99)(a(k),k=1,na)
            do j=1,na
               b1(j)=b1(j)+real(a(j))**2
               b2(j)=b2(j)+aimag(a(j))**2
            end do
        end do
!c  take looks across
         jpix=0
         do j=1,na,la
            jpix=jpix+1
            sum1=0.
            sum2=0.
            do k=0,la-1
               sum1=sum1+b1(j+k)
               sum2=sum2+b2(j+k)
            end do
            b(jpix)=cmplx(sqrt(sum1),sqrt(sum2))
         end do
         write(22,rec=lineout)(b(k),k=1,na/la)
99       continue
      end do
      !$omp end parallel do
      end





