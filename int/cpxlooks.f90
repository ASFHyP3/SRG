!c  cpxlooks -  average complex looks

      use omp_lib

      complex a(65536),b(65536),sum,pha,phd
      character*160 fin,fout
      integer statb(13),fstat
      integer*8 filelen,nbytes

      !$omp parallel
      n=omp_get_num_threads()
      !$omp end parallel
      print *, 'Max threads used: ', n
     
      if(iargc().lt.4)then
         write(*,*)'usage: cpxlooks infile outfile length looksac <looksdn> <ph-ac> <ph-dn>'
         stop
      end if

      call getarg(1,fin)
      call getarg(3,fout)
      read(fout,*)na
      open(21,file=fin,form='unformatted',access='direct',recl=na*8)
      ierr=fstat(21,statb)
      nbytes=filelen(fin)
      print *,nbytes," bytes in file"
!      nd=statb(8)/8/na
      nd=nbytes/8/na
      write(*,*)'Lines in file: ',nd
      call getarg(4,fout)
      read(fout,*)la
      if(iargc().ge.5)then
         call getarg(5,fout)
         read(fout,*)ld
      else
         ld=la
      end if
      if(iargc().ge.6)then
         call getarg(6,fout)
         read(fout,*)pa
      else
         pa=0.
      end if
      if(iargc().ge.7)then
         call getarg(7,fout)
         read(fout,*)pd
      else
         pd=0.
      end if
      call getarg(2,fout)
      open(22,file=fout,form='unformatted',access='direct',recl=na/la*8)
      

      pha=cmplx(cos(pa),sin(pa))
      phd=cmplx(cos(pd),sin(pd))

      lineout=0
      !$omp parallel do private(lineout,b,a,jpix,sum) &
      !$omp shared(nd,ld,na,la)
      do line=1,nd,ld
         if(mod(line,1000).eq.1)write(*,*)line
         lineout=(line-1)/ld+1
         do j=1,na
            b(j)=cmplx(0.,0.)
         end do

!c  take looks down
         do i=0,ld-1
            read(21,rec=line+i,err=99)(a(k),k=1,na)
            do j=1,na
               b(j)=b(j)+a(j)*pha**j*phd**(line+i)
            end do
         end do
!c  take looks across
         jpix=0
         do j=1,na,la
            jpix=jpix+1
            sum=cmplx(0.,0.)
            do k=0,la-1
               if(k.eq.0)sum=cmplx(0.,0.)
               sum=sum+b(j+k)
            end do
            b(jpix)=sum
         end do
         write(22,rec=lineout)(b(k),k=1,na/la)
 99   continue
      end do
      !$omp end parallel do
      end

