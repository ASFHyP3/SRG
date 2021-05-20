! downsamplealosraw - reduce chirp bandwidth by a factor of two

      complex array(16384)
      character*60 infile,outfile,str
      integer*1 dat(32768),datout(16384)

      
      if(iargc() < 4)then
         print *,'usage:  downsamplealosraw infile outfile linelengthin linelengthout'
         stop
      end if

      call getarg(1,infile)
      call getarg(2,outfile)
      call getarg(3,str)
      read(str,*)linelength
      call getarg(4,str)
      read(str,*)linelengthout

! some constants for alos
      iheadersize=206*2
      offset=15.5
      ncomplex=(linelengthout-iheadersize)/2
      length=16384

! open data files
      open(21,file=infile,access='direct',recl=linelength)
      open(31,file=outfile,access='direct',recl=linelengthout)

! initialize input buffer
      do i=1,length*2
         dat(i)=0
      end do

      call fftww(length,array,0)
      call fftww(length/2,array,0)

! loop over lines
      do line=1,1000000
         if(mod(line,1000).eq.0)print *,line
         read(21,rec=line,err=99)(dat(k),k=1,linelength)
! copy over header first
         do i=1,iheadersize
            datout(i)=dat(i)
         end do
! load data into transform array
         do i=1,ncomplex
            partreal=iand(31,dat(i*2-1+iheadersize))-offset
            partimag=iand(31,dat(i*2+iheadersize))-offset
            array(i)=cmplx(partreal,partimag)
         end do
         do i=ncomplex+1,length
            array(i)=cmplx(0.,0.)
         end do
         call fftww(length,array,-1)
! save lower frequencies
!         do i=1,length/4
         do i=1,length/2
!            array(i)=array(length/2+length/4+i)
            array(i)=array(length/4+i)
         end do
         call fftww(length/2,array,1)
!         print *,(array(k)/length*2,k=1000,1003)
! put into byte array
         do i=1,ncomplex/2
            datout(iheadersize+i*2-1)=real(array(i))/length*2
            datout(iheadersize+i*2)=aimag(array(i))/length*2
         end do
         if(ncomplex/2+iheadersize.lt.linelengthout)then
            do i=ncomplex/2+iheadersize+1,linelengthout
               datout(i)=0
            end do
         end if
         write(31,rec=line)(datout(k),k=1,linelengthout)
      end do

 99   continue
      end

      subroutine fftww(n,array,dir)
      complex array(*),out(65536)
      integer dir
      integer*8 plani(16),planf(16)

      common /fftwcommon/planf,plani

!c  plan creation if dir = 0
      if(dir.eq.0)then
         do i=2,16
            if(2**i.eq.n)go to 1
         end do
         write(*,*)'Illegal length'
         return
 1       call fftw_f77_create_plan(planf(i),n,-1,8)
         call fftw_f77_create_plan(plani(i),n,1,8)
         return
      end if

!c  calculate transform
      if(dir.eq.-1)then
         if(n.eq.4)call fftw_f77_one(planf(2),array,out)
         if(n.eq.8)call fftw_f77_one(planf(3),array,out)
         if(n.eq.16)call fftw_f77_one(planf(4),array,out)
         if(n.eq.32)call fftw_f77_one(planf(5),array,out)
         if(n.eq.64)call fftw_f77_one(planf(6),array,out)
         if(n.eq.128)call fftw_f77_one(planf(7),array,out)
         if(n.eq.256)call fftw_f77_one(planf(8),array,out)
         if(n.eq.512)call fftw_f77_one(planf(9),array,out)
         if(n.eq.1024)call fftw_f77_one(planf(10),array,out)
         if(n.eq.2048)call fftw_f77_one(planf(11),array,out)
         if(n.eq.4096)call fftw_f77_one(planf(12),array,out)
         if(n.eq.8192)call fftw_f77_one(planf(13),array,out)
         if(n.eq.16384)call fftw_f77_one(planf(14),array,out)
         if(n.eq.32768)call fftw_f77_one(planf(15),array,out)
         if(n.eq.65536)call fftw_f77_one(planf(16),array,out)
      end if
      if(dir.eq. 1)then
         if(n.eq.4)call fftw_f77_one(plani(2),array,out)
         if(n.eq.8)call fftw_f77_one(plani(3),array,out)
         if(n.eq.16)call fftw_f77_one(plani(4),array,out)
         if(n.eq.32)call fftw_f77_one(plani(5),array,out)
         if(n.eq.64)call fftw_f77_one(plani(6),array,out)
         if(n.eq.128)call fftw_f77_one(plani(7),array,out)
         if(n.eq.256)call fftw_f77_one(plani(8),array,out)
         if(n.eq.512)call fftw_f77_one(plani(9),array,out)
         if(n.eq.1024)call fftw_f77_one(plani(10),array,out)
         if(n.eq.2048)call fftw_f77_one(plani(11),array,out)
         if(n.eq.4096)call fftw_f77_one(plani(12),array,out)
         if(n.eq.8192)call fftw_f77_one(plani(13),array,out)
         if(n.eq.16384)call fftw_f77_one(plani(14),array,out)
         if(n.eq.32768)call fftw_f77_one(plani(15),array,out)
         if(n.eq.65536)call fftw_f77_one(plani(16),array,out)
      end if

      return

      end


