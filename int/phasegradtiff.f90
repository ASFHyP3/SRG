! phasegrad - compute (phase) gradient of mht image

      real*4, allocatable:: in(:,:),out(:,:)
      character*300 fin,fout,str
      integer*8 nbytes,filelen
      integer*1, allocatable :: rgb(:,:,:)
      integer*1 amp

      if(iargc().lt.3)then
         write(*,*)'usage: phasegradtiff infile outfile length looksac looksdn'
         stop
      end if

      call getarg(1,fin)
      call getarg(2,fout)
      call getarg(3,str)
      read(str,*)len
      call getarg(4,str)
      read(str,*)looksac
      call getarg(5,str)
      read(str,*)looksdn

      nbytes=filelen(fin)
!      nbytes=filelen(trim(fin))
      lines=nbytes/8/len
      write(*,*)'Lines in file: ',lines

      allocate(in(len*2,lines),out(len*2,lines),rgb(len,lines,4))

      open(21,file=fin,form='unformatted',access='direct',recl=len*lines*8)
      open(22,file=fout,form='unformatted',access='direct',recl=len*lines*8)

      read(21,rec=1)in
      out=0.

! across differences
      do line=1,lines
         do i=2,len
            out(i*2-1,line)=abs(in(i+len,line)-in(i-1+len,line))
         end do
      end do
! down differences
      do line=2,lines
         do i=1,len
            out(i*2,line)=abs(in(i+len,line)-in(i+len,line-1))
         end do
      end do

      write(22,rec=1)out
      close(21)
      close(22)

! scale amplitudes for tiff image
!c  scale for amplitude as in c++ version
      sum=0.
      isum=0
      sum2=0.
      isum2=0
      ifirst=1
      expo=0.3
      scale=1.
      do i=ifirst,ifirst+lines-1,32
         do j=32,len-32,32
            isum2=isum2+1
            sum2=sum2+in(j,i) !**expo
            !print *,i,j,cdata(j),sum2
         end do
      end do
      thresh=sum2/isum2/1000.
      print *,thresh,sum2,isum2
      do i=ifirst,ifirst+lines-1,32
         do j=32,len-32,32
            if(in(j,i).ge.thresh)then
               isum=isum+1
               sum=sum+in(j,i)**expo
               !print *,i,j,cdata(j),sum
            end if
         end do
      end do
      scalemag = scale*150/(sum/isum)
      print *,'scale factor: ',scalemag,scale,sum,isum,sum2,isum2

!c  now process each line
      do i=ifirst,ifirst+lines-1
         do j=1,len
            ampfloat=in(j,i)**expo * scalemag/255.
            if(ampfloat.gt.0.999)ampfloat=0.999
            rgb(j,i,1)=ampfloat*256.
            if(out(j*2,i).ge.looksdn*3.14159 .or. out(j*2-1,i).ge.looksac*3.14159)then
               rgb(j,i,2)=0
               rgb(j,i,3)=0
            else
               rgb(j,i,2)=ampfloat*256.
               rgb(j,i,3)=ampfloat*256.
            end if
            rgb(j,i,4)=255
            if(ampfloat.le.0.01)rgb(j,i,4)=0
         end do
      end do

!c  save as a tiff
      call writetiff(rgb,len,lines,'gradient.tif')

      end


