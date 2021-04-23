!c  hipass filter for mht interferogram phase

      use omp_lib

      complex*8, allocatable :: in(:,:),igram(:,:),igramfilt(:,:),filt(:,:),array(:,:)
      complex*8 csum
      real*4, allocatable :: out(:),phase(:),inmht(:,:),outmht(:,:)
      character*300 flist,fin,fout,str

      if(iargc().lt.3)then
         write(*,*)'usage: hipassmht infile outfile lengthx lengthy gaussian_box_size'
         stop
      end if

      call getarg(1,fin)
      call getarg(2,fout)
      call getarg(3,str)
      read(str,*)len
      call getarg(4,str)
      read(str,*)lines
      call getarg(5,str)
      read(str,*)box

!c  get sizes for ffts
      do i=1,20
         if(len.gt.2**i)ixfft=2**i
         if(lines.gt.2**i)iyfft=2**i
      end do
      ixfft=ixfft*2
      iyfft=iyfft*2
      print *,'FFT, box sizes: ',ixfft,iyfft,box

!c  allocate memory
      allocate(out(len*2))
      allocate(in(len,lines))
      allocate(igram(len,lines))
      allocate(igramfilt(ixfft,iyfft))
      allocate(filt(ixfft,iyfft),array(ixfft,iyfft))
      allocate(inmht(len*2,lines),outmht(len*2,lines))

!c  create lowpass filter first
      filt=cmplx(0.,0.)
      do ix=1,ixfft
         do iy=1,iyfft
            filt(ix,iy)=cmplx(0.,0.)
            if(ix.le.ixfft/2.and.iy.le.iyfft/2)r2=ix**2+iy**2
            if(ix.le.ixfft/2.and.iy.gt.iyfft/2)r2=ix**2+(iyfft-iy+1)**2
            if(ix.gt.ixfft/2.and.iy.le.iyfft/2)r2=(ixfft-ix+1)**2+iy**2
            if(ix.gt.ixfft/2.and.iy.gt.iyfft/2)r2=(ixfft-ix+1)**2+(iyfft-iy+1)**2
            arg=r2/2/box/box
            if(arg.gt.20)then
               filt(ix,iy)=cmplx(0.,0.)
            else
               filt(ix,iy)=exp(-arg)/(2*3.14159265*box*box)
               if(ix.eq.1.and.iy.eq.1)print *,arg,filt(1,1),cabs(filt(1,1)),1/(2*3.14159265*box*box)
            end if
         end do
      end do
!  filter stats
      fmax=0
      do i=1,ixfft
         do j=1,iyfft
            if(cabs(filt(i,j)).ge.fmax)fmax=cabs(filt(i,j))
         end do
      end do
      print *,'fmax= ',fmax
      open(99,file='tdfilter.dat',access='direct',recl=ixfft*iyfft*8)
      write(99,rec=1)filt
      close(99)
      call fft2d(filt,ixfft,iyfft,-1)
      open(99,file='filter.dat',access='direct',recl=ixfft*iyfft*8)
      write(99,rec=1)filt
      close(99)
      filt=filt/ixfft/iyfft

! read input file
      open(21,file=fin,access='stream')
      read(21)inmht
      close(21)

! load working array with phases
      array=cmplx(0.,0.)
      array(1:len,1:lines)=inmht(len+1:2*len,:)
! apply the filter
      call fft2d(array,ixfft,iyfft,-1)
      array=array*filt
      call fft2d(array,ixfft,iyfft,1)
! copy to output, subtracting to get hipass part
      outmht(len+1:2*len,:)=inmht(len+1:2*len,:)-array(1:len,1:lines)
      outmht(1:len,:)=inmht(1:len,:)

! and save
      open(21,file=fout,access='stream')
      write(21)outmht
      close(21)

      end

      subroutine fft2d(arr,ixfft,iyfft,dir)

        integer*4 ixfft,iyfft,dir
        complex*8 arr(ixfft,iyfft)
        integer*8 planf, plani
        real*4, allocatable :: work(:)

        allocate(work(max(ixfft,iyfft)*4+15))

        if(dir.eq.-1)then
           !print *,'forward start',ixfft,iyfft
           call fftw2d_f77_create_plan(planf, ixfft, iyfft, -1, 8)
           call fftwnd_f77_one(planf, arr, planfwork)
           call fftw_f77_destroy_plan(planf)
           !print *,'forward end'
        end if
        
        if(dir.eq.1)then
           !print *,'reverse start'
           call fftw2d_f77_create_plan(plani, ixfft, iyfft, +1, 8)
           call fftwnd_f77_one(plani, arr, planiwork)
           call fftw_f77_destroy_plan(plani)
           !print *,'reverse end'
        end if

        deallocate (work)

        return
        end
