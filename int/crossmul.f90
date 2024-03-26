!c  crossmul - cross multiply two files, one conjugated, form int and amp file
!
!  9 may 21: add ability to mask zero pixels in original images
!

      use omp_lib

      complex*8, allocatable:: in1(:,:),in2(:,:),igram(:,:),amp(:,:)
      complex*8, allocatable:: up1(:,:),up2(:,:),inline1(:),inline2(:)
      complex*8, allocatable:: igramacc(:),ampacc(:),igramtemp(:),amptemp(:)
      complex*8, allocatable :: temp(:),temp2(:,:), masksum(:)
      complex*8 :: data(32768),ref(32768), maskflag
      character*300 fin1,fin2,str,figram,famp
      character*1 maskyn
      integer, allocatable :: mask1flag(:),mask2flag(:),mask(:)
      integer*8 nbytes,filelen
      real*4, allocatable :: plannnnf(:),plannnni(:)  ! for fft upsampling
      integer*8 iplannnnf,iplannnni

      !$omp parallel
      n=omp_get_num_threads()
      !$omp end parallel
      print *, 'Max threads used: ', n

      if(iargc().lt.5)then
         write(*,*)'usage: crossmul infile1 infile2 outintfile outampfile length ', &
              '<valid_lines> <scale=1> <looksac> <looksdn> <mask y/n = y>'
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
      nbytes=filelen(fin1)
      nd=nbytes/8/na
      print *,'nbytes nd ',nbytes,nd
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
      write(*,*)'Lines in file: ',nd,', interferogram width: ',na/looksac
      maskyn='y'
      if(iargc().ge.10)then
         call getarg(10,maskyn)
      end if
!c  get ffts lengths for upsampling
      do i=1,24
         nnn=2**i
         if(nnn.ge.na)go to 11
      end do
11    continue
!      print *,'FFT length: ',nnn
!      call fftw_f77_create_plan(iplannnnf,nnn,-1,8)
!      call fftw_f77_create_plan(iplannnni,nnn*2,1,8)
      call sfftw_plan_dft_1d(iplannnnf, nnn, data, ref, -1, 64)
      call sfftw_plan_dft_1d(iplannnni, nnn*2, data, ref,  1, 64)

      open(32,file=figram,form='unformatted',access='direct',recl=na/looksac*8)
      open(33,file=famp,form='unformatted',access='direct',recl=na/looksac*8)

      !$omp parallel do private(in1,in2,up1,up2,inline1,inline2,igram,amp,temp,temp2) &
      !$omp private(igramacc,ampacc,igramtemp,amptemp,j,k,i,line,plannnnf,plannnni) &
      !$omp private(mask1flag,mask2flag,mask,maskflag,masksum) &
      !$omp shared(nvalid,looksdn,scale,na,nnn,iplannnnf,iplannnni) &
      !$omp shared(looksac,fin1,fin2,maskyn)

      do line=1,nvalid/looksdn
         if(mod(line,1000).eq.0)print *,line
         !c  allocate the local arrays
         allocate (in1(na,looksdn),in2(na,looksdn),igram(na*2,looksdn),amp(na*2,looksdn))
         allocate (igramacc(na),ampacc(na),igramtemp(na/looksac),amptemp(na/looksac))
         allocate (up1(nnn*2,looksdn), up2(nnn*2,looksdn), inline1(nnn), inline2(nnn))
         allocate (masksum(na),mask1flag(na/looksac),mask2flag(na/looksac),mask(na/looksac))
         allocate (temp(nnn),temp2(nnn*2,looksdn))

!c     read in lines
         read(21,rec=line,err=98)in1
         if(fin1.ne.fin2)then
            read(22,rec=line,err=98)in2
         else
            in2=in1
         end if
98       continue
         ! create masks if asked
         if (maskyn.eq.'y')then
            mask1flag=1
            mask2flag=1
            masksum=sum(in1,2)
            do i=1,na/looksac
               maskflag=sum(masksum((i-1)*looksac+1:i*looksac))
               if(abs(maskflag).le.1.e-6)mask1flag(i)=0
            end do
            masksum=sum(in2,2)
            do i=1,na/looksac
               maskflag=sum(masksum((i-1)*looksac+1:i*looksac))
               if(abs(maskflag).le.1.e-6)mask2flag(i)=0
            end do
            mask=mask1flag*mask2flag
!            if(line.eq.500)then
!               print *,mask1flag
!               print *,mask2flag
!               print *,mask
!            end if
         end if
!c     cross-multiply and save amplitudes
         in1=in1*scale
         in2=in2*scale

         up1=cmplx(0.,0.)  ! upsample file 1
         do i=1,looksdn
            inline1(1:na)=in1(:,i)
            inline1(na+1:nnn)=cmplx(0.,0.)
!            call fftw_f77_one(iplannnnf,inline1,plannnnf)
            call sfftw_execute_dft(iplannnnf,inline1,temp)
            up1(1:nnn/2,i)=temp(1:nnn/2)!inline1(1:nnn/2)
            up1(2*nnn-nnn/2+1:2*nnn,i)=temp(nnn/2+1:nnn)!inline1(nnn/2+1:nnn)
!            call fftw_f77_one(iplannnni,up1(1,i),plannnni)
            call sfftw_execute_dft(iplannnni,up1(1,i),temp2(1,i))
         end do
         up1=temp2/nnn/2.

         up2=cmplx(0.,0.)  ! upsample file 2
         do i=1,looksdn
            inline2(1:na)=in2(:,i)
            inline2(na+1:nnn)=cmplx(0.,0.)
!            call fftw_f77_one(iplannnnf,inline2,plannnnf)
            call sfftw_execute_dft(iplannnnf,inline2,temp)
            up2(1:nnn/2,i)=temp(1:nnn/2)
            up2(2*nnn-nnn/2+1:2*nnn,i)=temp(nnn/2+1:nnn)
!            call fftw_f77_one(iplannnni,up2(1,i),plannnni)
            call sfftw_execute_dft(iplannnni,up2(1,i),temp2(1,i))
         end do
         up2=temp2/nnn/2.

         igram(1:na*2,:)=up1(1:na*2,:)*conjg(up2(1:na*2,:))
         amp(1:na*2,:)=cmplx(cabs(up1(1:na*2,:))**2,cabs(up2(1:na*2,:))**2)
!c  reclaim the extra two across looks first
         do j=1,na
            igram(j,:) = igram(j*2-1,:)+igram(j*2,:)
            amp(j,:) = amp(j*2-1,:)+amp(j*2,:)
         end do

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

         ! multiply by mask if desired
         if (maskyn.eq.'y')then
!            if(line.eq.500)print *,igramtemp
            do j=1,na/looksac
               if(mask(j).eq.0)then
                  igramtemp(j)=0.
                  amptemp(j)=0.
               end if
            end do
!            if(line.eq.500)print *,igramtemp
         end if
         write(32,rec=line)igramtemp
         write(33,rec=line)amptemp
 99   continue

         deallocate (in1, in2, up1, up2, igramtemp, amptemp, igramacc, ampacc)
         deallocate (inline1, inline2, igram, amp, temp, temp2, mask, masksum, mask1flag, mask2flag)
      end do
      !$omp end parallel do

      end


