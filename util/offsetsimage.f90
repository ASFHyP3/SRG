!c  offsetsimage - create an image of offset values
!c   rg image format for rgoffset file and output file

      parameter (m=1000000)
      character*60 file,str
      integer i1(m),i2(m)
      real a1(m),a2(m),snr(m)
      complex, allocatable :: out(:,:)

      if(iargc().lt.2)then
         print *,'Usage: offsetsimage offset-file upsample-number <acmean> <acwindow> <dnmean> <dnwindow>'
         print *,'  upsample-number is size of square for each point'
         stop
      end if

      call getarg(1,file)
      call getarg(2,str)
      read(str,*)iupsample
      acmean=0
      acwin=1.e20
      dnmean=0
      dnwin=1.e20
      if(iargc().ge.3)then
         call getarg(3,str)
         read(str,*)acmean
         call getarg(4,str)
         read(str,*)acwin
         call getarg(5,str)
         read(str,*)dnmean
         call getarg(6,str)
         read(str,*)dnwin
         print *,'Windows for filter set'
      end if
         

!c read in file
      maxac=0
      maxdn=0
      minac=1000000
      mindn=1000000
      open(21,file=file)
      do k=1,m
         read(21,*,end=10)i1(k),a1(k),i2(k),a2(k),snr(k)
         if(i1(k).gt.maxac)maxac=i1(k)
         if(i1(k).lt.minac)minac=i1(k)
         if(i2(k).gt.maxdn)maxdn=i2(k)
         if(i2(k).lt.mindn)mindn=i2(k)
      end do
 10   k=k-1
      print *,k,' points read'
      close(21)
!c  estimate sample spacing across and down
      ideltaac=1000000
      ideltadn=1000000
      do i=2,k
         if(iabs(i1(i)-i1(i-1)).ne.0)then
            if(iabs(i1(i)-i1(i-1)).lt.ideltaac)ideltaac=iabs(i1(i)-i1(i-1))
         end if
         if(iabs(i2(i)-i2(i-1)).ne.0)then
            if(iabs(i2(i)-i2(i-1)).lt.ideltadn)ideltadn=iabs(i2(i)-i2(i-1))
         end if
      end do
      print *,minac,maxac,ideltaac
      print *,mindn,maxdn,ideltadn

      open(21,file='offsetsrg',access='direct',status='replace',recl=maxac/ideltaac*iupsample*maxdn/ideltadn*iupsample*8)
      print *,'Offset image width is ',maxac/ideltaac*iupsample
      print *,'Offset image length is ',maxdn/ideltadn*iupsample

!c  filter the offsets if desired
      do i=1,k
         if(abs(a1(i)-acmean).gt.acwin)a1(i)=acmean
         if(abs(a2(i)-dnmean).gt.dnwin)a2(i)=dnmean
      end do

      allocate (out(maxac/ideltaac*iupsample,maxdn/ideltadn*iupsample))

      do i=1,k
         do j=1,iupsample
            do jj=1,iupsample
               out((i1(i)/ideltaac-1)*iupsample+j,(i2(i)/ideltadn-1)*iupsample+jj)=cmplx(a1(i),a2(i))
            end do
         end do
      end do
      write(21,rec=1)out
      close(21)

      end
