!c  crossmul - cross multiply two files, one conjugated, form int and amp file
      complex*8, allocatable:: in1(:,:),in2(:,:),igram(:,:),amp(:,:)
      complex*8, allocatable:: up1(:,:),up2(:,:),inline1(:),inline2(:)
      complex*8, allocatable:: igramacc(:),ampacc(:),igramtemp(:),amptemp(:)
      character*100 fin1,fin2,str,figram,famp
      integer statb(13),fstat
      integer*8 nbytes,filelen

      real*4, allocatable :: plannnnf(:),plannnni(:)  ! for fft upsampling
      integer*8 iplannnnf,iplannnni

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
      !ierr=fstat(21,statb)
      !nd=statb(8)/8/na
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

!c  get ffts lengths for upsampling
      do i=1,16
         nnn=2**i
         if(nnn.ge.na)go to 11
      end do
11    print *,'FFT length: ',nnn
      call fftw_f77_create_plan(iplannnnf,nnn,-1,8)
      call fftw_f77_create_plan(iplannnni,nnn*2,1,8)
      allocate (plannnnf(nnn*4+15),plannnni(nnn*2*4+15))

!c  allocate arrays
      allocate (in1(na,looksdn),in2(na,looksdn),igram(na*2,looksdn),amp(na*2,looksdn))
      allocate (igramacc(na),ampacc(na),igramtemp(na/looksac),amptemp(na/looksac))
      allocate (up1(nnn*2,looksdn), up2(nnn*2,looksdn), inline1(nnn), inline2(nnn))

      open(32,file=figram,form='unformatted',access='direct',recl=na/looksac*8)
      open(33,file=famp,form='unformatted',access='direct',recl=na/looksac*8)

      do line=1,nvalid/looksdn
         if(mod(line,200).eq.0)print *,line
!c     read in lines
         read(21,rec=line,err=99)in1
         if(fin1.ne.fin2)then
            read(22,rec=line,err=99)in2
         else
            in2=in1
         end if
!c     cross-multiply and save amplitudes
         in1=in1*scale
         in2=in2*scale

         up1=cmplx(0.,0.)  ! upsample file 1
         do i=1,looksdn
            inline1(1:na)=in1(:,i)
            inline1(na+1:nnn)=cmplx(0.,0.)
            call fftw_f77_one(iplannnnf,inline1,plannnnf)
            up1(1:nnn/2,i)=inline1(1:nnn/2)
            up1(2*nnn-nnn/2+1:2*nnn,i)=inline1(nnn/2+1:nnn)
            call fftw_f77_one(iplannnni,up1(1,i),plannnni)
         end do
         up1=up1/nnn

         up2=cmplx(0.,0.)  ! upsample file 2
         do i=1,looksdn
            inline2(1:na)=in2(:,i)
            inline2(na+1:nnn)=cmplx(0.,0.)
            call fftw_f77_one(iplannnnf,inline2,plannnnf)
            up2(1:nnn/2,i)=inline2(1:nnn/2)
            up2(2*nnn-nnn/2+1:2*nnn,i)=inline2(nnn/2+1:nnn)
            call fftw_f77_one(iplannnni,up2(1,i),plannnni)
         end do
         up2=up2/nnn

         do k=1,looksdn
            igram(1:na*2,k)=up1(1:na*2,k)*conjg(up2(1:na*2,k))
            amp(1:na*2,k)=cmplx(cabs(up1(1:na*2,k))**2,cabs(up2(1:na*2,k))**2)
!c  reclaim the extra two across looks first
            do j=1,na
               igram(j,k) = igram(j*2-1,k)+igram(j*2,k)
               amp(j,k) = amp(j*2-1,k)+amp(j*2,k)
            end do
         end do

!c     looks down 
!!$         do j=1,na
!!$            igramacc(j)=cmplx(0.,0.)
!!$            ampacc(j)=cmplx(0.,0.)
!!$            do i=1,looksdn
!!$               igramacc(j)=igramacc(j)+igram(j,i)
!!$               ampacc(j)=ampacc(j)+amp(j,i)
!!$            end do
!!$         end do
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

      end do
 99   continue
      end


