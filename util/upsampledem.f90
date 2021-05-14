!c  upsampledem - upsample a dem (i*2) file linearly

      character*360 infile, outfile, inrscfile, outrscfile
      character*14 str, str2
      integer*2, allocatable :: idata(:,:),outdata(:,:)

      if(iargc().lt.5)then
         print *,'Usage: upsampledem infile inrscfile outfile outrscfile upsamplefactor'
         stop
      end if

      call getarg(1,infile)
      call getarg(2,inrscfile)
      call getarg(3,outfile)
      call getarg(4,outrscfile)
      call getarg(5,str)
      read(str,*)ifactor
        
!c  open input rsc file, write new data to output rsc file
      open(20,file=inrscfile)
      open(30,file=outrscfile)

      read(20,'(a,i14)')str,iwidth
      newwidth=(iwidth-1)*ifactor+1
      write(str2,'(i14)')(iwidth-1)*ifactor+1
      write(30,'(a14,a14)')str,adjustl(str2)

      read(20,'(a,i14)')str,ilength
      write(str2,'(i14)')(ilength-1)*ifactor+1
      write(30,'(a14,a14)')str,adjustl(str2)

      read(20,'(a,i14)')str,ixfirst
      write(str2,'(i14)')ixfirst
      write(30,'(a14,a14)')str,adjustl(str2)

      read(20,'(a,i14)')str,iyfirst
      write(str2,'(i14)')iyfirst
      write(30,'(a14,a14)')str,adjustl(str2)

      read(20,'(a,e20.8)')str,xstep
      write(30,'(a14,e15.8)')str,xstep/ifactor

      read(20,'(a,e20.8)')str,ystep
      write(30,'(a14,e15.8)')str,ystep/ifactor

      read(20,'(a,a)')str,str2
      write(30,'(a14,a14)')str,str2
      read(20,'(a,a)')str,str2
      write(30,'(a14,a14)')str,str2
      read(20,'(a,a)')str,str2
      write(30,'(a14,a14)')str,str2
      read(20,'(a,a)')str,str2
      write(30,'(a14,a14)')str,str2
      read(20,'(a,a)')str,str2
      write(30,'(a14,a14)')str,str2
      close(20)
      close(30)

      print *,iwidth,ilength,newwidth,ifactor
      print *,xstep,ystep
!c  open input and output dem files
      allocate (idata(iwidth,2),outdata(newwidth,ifactor))
      open(21,file=infile,access='direct',recl=iwidth*2)
      open(31,file=outfile,access='direct',recl=newwidth*2)
!c  loop over input lines
      do i=1,ilength-1
         read(21,rec=i)idata(:,1)
         read(21,rec=i+1)idata(:,2)
         ! interpolate across
         do j=1,iwidth-1
            do k=1,ifactor
               index=(j-1)*ifactor+k
               outdata(index,1)=idata(j,1)+nint((idata(j+1,1)-idata(j,1))*float(k-1)/float(ifactor))
               outdata((j-1)*ifactor+k,ifactor)=idata(j,2)+(idata(j+1,2)-idata(j,2))*float(k-1)/float(ifactor)
               !print *,idata(j,1),idata(j+1,1),outdata((j-1)*ifactor+k,1)
               !print *,index,outdata(index,1)
            end do
         end do
         !interpolate down
         do k=2,ifactor
            outdata(:,k)=outdata(:,1)+(outdata(:,ifactor)-outdata(:,1))*float(k-1)/float(ifactor)
         end do
         !  write interpolated data
         do k=1,ifactor
            index=(i-1)*ifactor+k
            !print *,index
            write(31,rec=index)outdata(:,1)
         end do
         !print *,outdata(:,1)
      end do
!c  write last line
      write(31,rec=(ilength-1)*ifactor+1)outdata(:,ifactor)

      close(21)
      close(31)

      end
