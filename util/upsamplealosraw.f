c  upsamplealosraw - upsample an alos raw data file to double range points

      parameter (isize=8192)
      character*60 infile, outfile, str
      integer*1 idataout(isize*2*2),idata(isize*2)
      complex data(isize*2*2)
      integer istat(13)

      if(iargc().lt.3)then
         print *,'Usage: upsamplealosraw infile outfile infile-width (16384 max) good_bytes outfile-width'
         stop
      end if

      call getarg(1,infile)
      call getarg(2,outfile)
      call getarg(3,str)
      read(str,*)npix
      call getarg(4,str)
      read(str,*)igood
      call getarg(5,str)
      read(str,*)npixout
        
      call stat(infile,istat)
      lines=istat(8)/npix
      print *,'Lines in file: ',lines

      open(44,file='s.dat',access='direct',recl=isize*8*2)

c  open files
      open(21,file=infile,access='direct',recl=npix)
      open(31,file=outfile,access='direct',recl=npixout)
      call fftww(isize,data,0)
      call fftww(isize*2,data,0)

c  read in file, upsample by 2 across as we read in
      do i=1,lines
         if(mod(i,1000).eq.0)print *,i
         read(21,rec=i)(idata(k),k=1,npix)

c  copy header first
         do j=1,412
            idataout(j)=idata(j)
         end do
            
c  upsample across
         do j=1,igood/2
            qi=float(iand(idata(j*2-1+412),31))-15.5
            qq=float(iand(idata(j*2+412),31))-15.5
            data(j)=cmplx(qi,qq)
         end do
         if(igood/2.lt.isize)then
            do j=igood/2+1,isize
               data(j)=cmplx(0.,0.)
            end do
         end if
         call fftww(isize,data,-1)

c  debug - save spectrum
c         write(44,rec=i)(data(k),k=1,isize)

c  zero pad and inverse transform
         do j=1,isize/2
            data(isize+isize/2+j)=data(isize/2+j)
         end do
         do j=isize/2+1,isize+isize/2
            data(j)=cmplx(0.,0.)
         end do

         call fftww(isize*2,data,1)
c         write(44,rec=i)(data(k),k=1,isize*2)

         data=data/isize

         do j=1,npixout/2-412/2
            idataout(j*2-1+412)=iand(nint(real(data(j))),255)
            idataout(j*2+412)=iand(nint(aimag(data(j))),255)
         end do

c  write out data to file, x2 in range direction
         write(31,rec=i)(idataout(k),k=1,npixout)

      end do

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


