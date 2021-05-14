!c  createspecialdem - version of makespecialdem that uses lat/lon locations
!c   create a dem with specific limits and upsample amoint
!c   derived from upsampledem - upsample a dem (i*2) file linearly

      character*360 infile, outfile, inrscfile, outrscfile
      character*60 string
      character*14 str, str2
      character*14 strwidth,strlength,strxfirst,stryfirst,strxstep,strystep
      real*8 toplat, botlat, leftlon, rightlon, newlat, newlon
      real*8 xfirst, yfirst, xstep, ystep
      integer*2, allocatable :: idata(:,:),outdata(:,:)

      if(iargc().lt.8)then
         print *,'Usage: createspecialdem infile inrscfile outfile ', &
              'outrscfile top-lat bot-lat left-lon ', &
              'right-lon <upsampx=1> <upsamppy=upsampx>'
         stop
      end if

      call getarg(1,infile)
      call getarg(2,inrscfile)
      call getarg(3,outfile)
      call getarg(4,outrscfile)
      call getarg(5,str)
      read(str,*)toplat
      call getarg(6,str)
      read(str,*)botlat
      call getarg(7,str)
      read(str,*)leftlon
      call getarg(8,str)
      read(str,*)rightlon
      ifactorx=1
      if(iargc().ge.9)then
         call getarg(9,str)
         read(str,*)ifactorx
      end if
      ifactory=ifactorx
      if(iargc().ge.10)then
         call getarg(10,str)
         read(str,*)ifactory
      end if

!c  open input rsc file, write new data to output rsc file
      open(20,file=inrscfile)
      open(30,file=outrscfile)

!c  read in the values that matter first
      read(20,'(a)')string
      read(string(1:14),'(a)')strwidth
      read(string(15:60),*)iwidth

      read(20,'(a)')string
      read(string(1:14),'(a)')strlength
      read(string(15:60),*)ilength

      read(20,'(a)')string
      read(string(1:14),'(a)')strxfirst
      read(string(15:60),*)xfirst

      read(20,'(a)')string
      read(string(1:14),'(a)')stryfirst
      read(string(15:60),*)yfirst

      read(20,'(a)')string
      read(string(1:14),'(a)')strxstep
      read(string(15:60),*)xstep

      read(20,'(a)')string
      read(string(1:14),'(a)')strystep
      read(string(15:60),*)ystep

!      print *,'original dem: ',xfirst,yfirst,xstep,ystep
!      print *,'new limits: ',toplat,botlat,leftlon,rightlon
!      print *,'delta lat, lon: ',toplat-botlat, rightlon-leftlon
!      print *,'size lat, lon: ',(toplat-botlat)/abs(ystep), (rightlon-leftlon)/abs(xstep)
!      print *,'double lines: ',(toplat-yfirst)/ystep+1,(botlat-yfirst)/ystep+1,(leftlon-xfirst)/xstep+1,(rightlon-xfirst)/xstep+1

      !c  compute limits
      latsize=nint((toplat-botlat)/abs(ystep))
      lonsize=nint((rightlon-leftlon)/abs(xstep))
      !c  take care of half pixel offsets in copernicus dem
      fracy=(toplat-yfirst)/ystep-int((toplat-yfirst)/ystep)
      fracx=(leftlon-xfirst)/xstep-int((leftlon-xfirst)/xstep)
!      print *,'fracx fracy ',fracx,fracy,fracx-1,fracy-1
      linetop=nint((toplat-yfirst)/ystep+fracy)
      linebot=linetop+latsize !nint((botlat-yfirst)/ystep)+1
      ileft=nint((leftlon-xfirst)/xstep+fracx)
      iright=ileft+lonsize !nint((rightlon-xfirst)/xstep)+1
!      print *,'linetop linebot ileft iright ',linetop,linebot,ileft,iright

      newwidth=(iright-ileft)*ifactorx+1
      newlength=(linebot-linetop)*ifactory+1
      newlat=yfirst+(linetop-1)*ystep
      newlon=xfirst+(ileft-1)*xstep
      print *,'newwidth newlength newlat newlon',newwidth,newlength,newlat,newlon
 
      write(str2,'(i14)')newwidth
      write(30,'(a14,a15)')strwidth,adjustl(str2)

      write(str2,'(i14)')newlength
      write(30,'(a14,a15)')strlength,adjustl(str2)

      write(str2,'(f14.8)')newlon
      write(30,'(a14,a15)')strxfirst,adjustl(str2)

      write(str2,'(f14.8)')newlat
      write(30,'(a14,a15)')stryfirst,adjustl(str2)

      write(30,'(a14,e15.8)')strxstep,xstep/ifactorx

      write(30,'(a14,e16.8)')strystep,ystep/ifactory

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

!c  write out the subsetted dem
      if(ifactorx.eq.1.and.ifactory.eq.1)then
         allocate (idata(iwidth,1),outdata(newwidth,1))
         open(21,file=infile,access='direct',recl=iwidth*2)
         open(31,file=outfile,access='direct',recl=newwidth*2)
         do i=linetop,linebot
            read(21,rec=max(i,1))idata
            write(31,rec=i-linetop+1)idata(ileft:iright,1)
         end do
         deallocate (idata, outdata)
      else
!     if(k.eq.k)stop

!         print *,'iwidth ilength newwidth ifactor',iwidth,ilength,newwidth,ifactorx,ifactory
!         print *,'xstep, ystep ',xstep,ystep
         !c  open input and output dem files
         allocate (idata(iwidth,2),outdata(newwidth,ifactory))
         open(21,file=infile,access='direct',recl=iwidth*2)
         open(31,file=outfile,access='direct',recl=newwidth*2)
         !c  loop over input lines
         do i=linetop,linebot-1
            read(21,rec=max(i,1))idata(:,1)
            read(21,rec=max(i+1,1))idata(:,2)
            ! interpolate across
            do j=ileft,iright-1
               do k=1,ifactorx
!                  index=(j-1)*ifactor+k
                  indexout=(j-ileft)*ifactorx+k
                  outdata(indexout,1)=idata(j,1)+nint((idata(j+1,1)-idata(j,1))*float(k-1)/float(ifactorx))
                  outdata(indexout,ifactory)=idata(j,2)+(idata(j+1,2)-idata(j,2))*float(k-1)/float(ifactorx)
                  !print *,idata(j,1),idata(j+1,1),outdata((j-1)*ifactor+k,1)
                  !print *,index,outdata(index,1)
               end do
            end do
            outdata(newwidth,1)=idata(iright,1)
            outdata(newwidth,2)=idata(iright,2)
            !interpolate down
            do k=2,ifactory
               outdata(:,k)=outdata(:,1)+(outdata(:,ifactory)-outdata(:,1))*float(k-1)/float(ifactory)
            end do
            !  write interpolated data
            do k=1,ifactory
               index=(i-linetop)*ifactory+k
!               print *,index
               write(31,rec=index)outdata(:,k)
            end do
            !print *,outdata(:,1)
         end do
         !c  write last line
         write(31,rec=newlength)outdata(:,ifactory)
      end if
      close(21)
      close(31)

!c  update the latloncoords file
      open(21,file='latloncoords')
      write(21,*)botlat
      write(21,*)leftlon
      write(21,*)toplat
      write(21,*)rightlon
      close(21)

      end
