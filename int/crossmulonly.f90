!c  crossmul - cross multiply two files without oversampling, one conjugated, form int and amp file

      use omp_lib

      complex*8, allocatable:: in1(:,:),in2(:,:),igram(:,:),amp(:,:)
      complex*8, allocatable:: up1(:,:),up2(:,:),inline1(:),inline2(:)
      complex*8, allocatable:: igramacc(:),ampacc(:),igramtemp(:),amptemp(:)
      character*300 fin1,fin2,str,figram,famp
      integer*8 nbytes,filelen

      real*4, allocatable :: plannnnf(:),plannnni(:)  ! for fft upsampling
      integer*8 iplannnnf,iplannnni

      !$omp parallel
      n=omp_get_num_threads()
      !$omp end parallel
      print *, 'Max threads used: ', n

      if(iargc().lt.5)then
         write(*,*)'usage: crossmul infile1 infile2 outintfile outampfile length <valid_lines> <scale=1> <looksac> <looksdn>'
         print *,'scale is multiplied by each scene to prevent overflow'
         stop
      end if

      call getarg(1,fin1)
      call getarg(5,str)
      read(str,*)na
      looksac=1
      if(iargc().ge.8)then
         call getarg(8,str)
         read(str,*)looksac
      end if
      looksdn=looksac
      if(iargc().ge.9)then
         call getarg(9,str)
         read(str,*)looksdn
      end if
      open(21,file=fin1,form='unformatted',access='direct',recl=na*8*looksdn)
      nbytes=filelen(trim(fin1))
      nd=nbytes/8/na
      write(*,*)'Lines in file: ',nd
      call getarg(2,fin2)
      if(trim(fin1).ne.trim(fin2))then
         open(22,file=fin2,form='unformatted',access='direct',recl=na*8*looksdn)
      end if
      call getarg(3,figram)
      call getarg(4,famp)
      nvalid=nd
      if(iargc().ge.6)then
         call getarg(6,str)
         read(str,*)nvalid
      end if
      scale=1.0
      if(iargc().ge.7)then
         call getarg(7,str)
         read(str,*)scale
      end if
      print *,'Interferogram width: ',na/looksac

      open(32,file=figram,form='unformatted',access='direct',recl=na/looksac*8)
      open(33,file=famp,form='unformatted',access='direct',recl=na/looksac*8)

      !$omp parallel do private(in1,in2,up1,up2,inline1,inline2,igram,amp) &
      !$omp private(igramacc,ampacc,igramtemp,amptemp,j,k,i,line,plannnnf,plannnni) &
      !$omp shared(nvalid,looksdn,scale,na,nnn,iplannnnf,iplannnni) &
      !$omp shared(looksac,fin1,fin2)

      do line=1,nvalid/looksdn
         if(mod(line,1000).eq.0)print *,line
         !c  allocate the local arrays
         allocate (in1(na,looksdn),in2(na,looksdn),igram(na,looksdn),amp(na,looksdn))
         allocate (igramacc(na),ampacc(na),igramtemp(na/looksac),amptemp(na/looksac))
         allocate (up1(nnn*2,looksdn), up2(nnn*2,looksdn), inline1(nnn), inline2(nnn))
         allocate (plannnnf(nnn*4+15),plannnni(nnn*2*4+15))

!c     read in lines
         read(21,rec=line,err=98)in1
         if(fin1.ne.fin2)then
            read(22,rec=line,err=98)in2
         else
            in2=in1
         end if
98       continue
!c     cross-multiply and save amplitudes
         in1=in1*scale
         in2=in2*scale

         igram(1:na,:)=in1(1:na,:)*conjg(in2(1:na,:))
         amp(1:na,:)=cmplx(cabs(in1(1:na,:))**2,cabs(in2(1:na,:))**2)
!c     looks down 
         igramacc=sum(igram(1:na,:),2)
         ampacc=sum(amp(1:na,:),2)

!c     looks across
         do j=0,na/looksac-1
            igramtemp(j+1)=cmplx(0.,0.)
            amptemp(j+1)=cmplx(0.,0.)
            do k=1,looksac
               igramtemp(j+1)=igramtemp(j+1)+igramacc(j*looksac+k)
               amptemp(j+1)=amptemp(j+1)+ampacc(j*looksac+k)
            end do
            amptemp(j+1)=cmplx(sqrt(real(amptemp(j+1))),sqrt(aimag(amptemp(j+1))))
         end do

         write(32,rec=line)igramtemp
         write(33,rec=line)amptemp
 99   continue

         deallocate (in1, in2, up1, up2, igramtemp, amptemp, igramacc, ampacc, inline1, inline2, igram, amp, plannnnf,plannnni)
      end do
      !$omp end parallel do

      end


