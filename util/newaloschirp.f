c  newaloschirp - change range modulation of an alos data set

      integer ranfft
      parameter (isize=16384)
      parameter (ranfft=16384)
      character*60 infile, outfile, str
      integer*1 idataout(isize*2*2),idata(isize*2)
      complex data(isize*2*2)
      integer istat(13)
      integer*8 iplanfranfft,iplaniranfft
      real*4    planfranfft(ranfft*4+15),planiranfft(ranfft*4+15)
      complex*8 ref(ranfft),newref(ranfft)

      if(iargc().lt.3)then
         print *,'Usage: newaloschirp infile outfile file-width (bytes, 32K max) good_bytes input_fs,slope,plen output_fs,slope,plen'
         stop
      end if

      call getarg(1,infile)
      call getarg(2,outfile)
      call getarg(3,str)
      read(str,*)npix
      call getarg(4,str)
      read(str,*)igood
      call getarg(5,str)
      read(str,*)fsin
      call getarg(6,str)
      read(str,*)slopein
      call getarg(7,str)
      read(str,*)plenin
      call getarg(8,str)
      read(str,*)fsout
      call getarg(9,str)
      read(str,*)slopeout
      call getarg(10,str)
      read(str,*)plenout
        
      call stat(infile,istat)
      lines=istat(8)/npix
      print *,'Lines in file: ',lines

c  open files, initialize transforms
      open(21,file=infile,access='direct',recl=npix)
      open(31,file=outfile,access='direct',recl=npix)
      call fftw_f77_create_plan(iplanfranfft,ranfft,-1,8)
      call fftw_f77_create_plan(iplaniranfft,ranfft,1,8)

c  create the new and old chirps
      ref(:)=cmplx(0.,0.)
      newref(:)=cmplx(0.,0.)
      pi=3.14159265359
      npts=fsin*plenin
      ts=1./fsin
      if(mod(npts,2) .eq. 0) npts=npts+1
      print *,'Input pulse length in points, time/sample in us: ',npts,ts*1.e6
      do i=-npts/2,npts/2
         t=i*ts
         phase = pi*slopein*t*t
         ref(i+npts/2+1)=cmplx(cos(phase),sin(phase))/isize
      end do

      npts=fsout*plenout
      ts=1./fsout
      if(mod(npts,2) .eq. 0) npts=npts+1
      print *,'Input pulse length in points, time/sample in us: ',npts,ts*1.e6
      do i=-npts/2,npts/2
         t=i*ts
         phase = pi*slopeout*t*t
         newref(i+npts/2+1)=cmplx(cos(phase),sin(phase))/isize
      end do

c  transform chirps
      call fftw_f77_one(iplanfranfft,ref,planfranfft)
      call fftw_f77_one(iplanfranfft,newref,planfranfft)

c  conjugate first chirp
      ref=conjg(ref)*4

      open(41,file='chirps')
      do k=1,ranfft
         write(41,*)k,cabs(ref(k)),cabs(newref(k))
      end do

c  read in file line by line
      do i=1,lines
         if(mod(i,1000).eq.0)print *,i
         read(21,rec=i)(idata(k),k=1,npix)

c         print *,(idata(k),k=1000,1010)
c  copy header first
         do j=1,412
            idataout(j)=idata(j)
         end do
            
c  range compress line
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
         call fftw_f77_one(iplanfranfft,data,planfranfft)
c  multiply by reference
         do j=1,ranfft
            data(j)=data(j)*ref(j)
         end do

c  now modulate by new chirp

c  multiply by new reference
         do j=1,ranfft
            data(j)=data(j)*newref(j)
         end do
         call fftw_f77_one(iplaniranfft,data,planiranfft)

c         print *,(data(k),k=500,503)
c  save as byte file
         do j=1,npix/2-412/2
            idataout(j*2-1+412)=iand(nint(real(data(j))),255)
            idataout(j*2+412)=iand(nint(aimag(data(j))),255)
         end do

c  write out data to file
         write(31,rec=i)(idataout(k),k=1,npix)
c         print *,(idataout(k),k=1000,1010)

      end do

      end

