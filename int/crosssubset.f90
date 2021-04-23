!c  crosssubset - cross multiply two files, one conjugated, form int and amp file
!c        specify which part of the full files to use

      use omp_lib

      complex*8, allocatable:: in1(:,:),in2(:,:),igram(:,:),amp(:,:),raw1(:,:),raw2(:,:)
      complex*8, allocatable:: up1(:,:),up2(:,:),inline1(:),inline2(:)
      complex*8, allocatable:: igramacc(:),ampacc(:),igramtemp(:),amptemp(:)
      character*100 fin1,fin2,str,figram,famp
      integer*8 nbytes,filelen

      real*4, allocatable :: plannnnf(:),plannnni(:)  ! for fft upsampling
      integer*8 iplannnnf,iplannnni

      !$omp parallel
      n=omp_get_num_threads()
      !$omp end parallel
      print *, 'Max threads used: ', n

      if(iargc().lt.5)then
         write(*,*)'usage: crosssubset infile1 infile2 outintfile outampfile & 
             length <scale=1> <startx=1> <starty=1> & 
             <sizex=length> <sizey=all> <looksac=1> <looksdn=looksac>'
         print *,'scale is multiplied by each scene to prevent overflow'
         stop
      end if

      call getarg(1,fin1)
      call getarg(2,fin2)
      call getarg(3,figram)
      call getarg(4,famp)
      call getarg(5,str)
      read(str,*)len
      nbytes=filelen(trim(fin1))
      lines=nbytes/8/len
      write(*,*)'Lines in input file: ',lines
      scale=1.0
      if(iargc().ge.6)then
         call getarg(6,str)
         read(str,*)scale
      end if
      ixstart=1
      if(iargc().ge.7)then
         call getarg(7,str)
         read(str,*)ixstart
      end if
      iystart=1
      if(iargc().ge.8)then
         call getarg(8,str)
         read(str,*)iystart
      end if
      ixsize=len
      if(iargc().ge.9)then
         call getarg(9,str)
         read(str,*)ixsize
      end if
      iysize=lines
      if(iargc().ge.10)then
         call getarg(10,str)
         read(str,*)iysize
      end if
      looksac=1
      if(iargc().ge.11)then
         call getarg(11,str)
         read(str,*)looksac
      end if
      looksdn=looksac
      if(iargc().ge.12)then
         call getarg(12,str)
         read(str,*)looksdn
      end if
      open(21,file=fin1,form='unformatted',access='direct',recl=len*8)
      if(trim(fin1).ne.trim(fin2))then
         open(22,file=fin2,form='unformatted',access='direct',recl=len*8)
      end if
!c   interferogram sizes
      na=ixsize
      nd=iysize
      print *,'Interferogram width, length: ',na/looksac,nd/looksdn

!c  get ffts lengths for upsampling
      do i=1,16
         nnn=2**i
         if(nnn.ge.na)go to 11
      end do
11    print *,'FFT length: ',nnn
      call fftw_f77_create_plan(iplannnnf,nnn,-1,8)
      call fftw_f77_create_plan(iplannnni,nnn*2,1,8)

      open(32,file=figram,form='unformatted',access='direct',recl=na/looksac*8)
      open(33,file=famp,form='unformatted',access='direct',recl=na/looksac*8)

      !$omp parallel do private(in1,in2,up1,up2,inline1,inline2,igram,amp) &
      !$omp private(igramacc,ampacc,igramtemp,amptemp,j,k,i,line,plannnnf,plannnni) &
      !$omp private(raw1,raw2,lineout) &
      !$omp shared(nvalid,looksdn,scale,na,nnn,iplannnnf,iplannnni) &
      !$omp shared(looksac,fin1,fin2,ixstart,iystart,ixsize,iysize)

      do line=iystart,iystart+iysize-1,looksdn
         lineout=(line-iystart)/looksdn+1
         !print *,line,iystart,iystart+iysize-1,looksdn,lineout
         if(mod(lineout,1000).eq.0)print *,lineout
         !c  allocate the local arrays
         allocate (raw1(len,looksdn),raw2(len,looksdn))
         allocate (in1(na,looksdn),in2(na,looksdn),igram(na*2,looksdn),amp(na*2,looksdn))
         allocate (igramacc(na),ampacc(na),igramtemp(na/looksac),amptemp(na/looksac))
         allocate (up1(nnn*2,looksdn), up2(nnn*2,looksdn), inline1(nnn), inline2(nnn))
         allocate (plannnnf(nnn*4+15),plannnni(nnn*2*4+15))

!c     read in lines
         do i=0,looksdn-1
            read(21,rec=line+i,err=99)(raw1(k,i+1),k=1,len)
         end do
         in1(:,:)=raw1(ixstart:ixstart+na-1,:)
         if(fin1.ne.fin2)then
            do i=0,looksdn-1
               read(22,rec=line+i,err=99)(raw2(k,i+1),k=1,len)
            end do
         else
            raw2=raw1
         end if
         in2(:,:)=raw2(ixstart:ixstart+na-1,:)
         !print *,line,lineout
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

         write(32,rec=lineout)igramtemp
         write(33,rec=lineout)amptemp
 99   continue

         deallocate (raw1, raw2, in1, in2, up1, up2, igramtemp, amptemp, igramacc, & 
                     ampacc, inline1, inline2, igram, amp, plannnnf,plannnni)
      end do
      !$omp end parallel do

      end


