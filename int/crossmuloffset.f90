!c  crossmul - cross multiply two files, one conjugated, form int and amp file
      complex in1(20480),in2(20480),intgram(20480),amp(20480),in2off(20480)
      complex intacc(20480),ampacc(20480),inttemp,amptemp
      character*60 fin1,fin2,str,fint,famp
      integer statb(13),fstat

      if(iargc().lt.5)then
         write(*,*)'usage: crossmul infile1 infile2 outintfile outampfile length>'
         write(*,*)'<scale=1> <looksac> <looksdn> <azoff> <rgoffpix1> <rgoffslope>'
         print *,'scale is multiplied by each scene to prevent overflow'
         stop
      end if

      call getarg(1,fin1)
      call getarg(5,str)
      read(str,*)na
      open(21,file=fin1,form='unformatted',access='direct',recl=na*8)
      ierr=fstat(21,statb)
      nd=statb(8)/8/na
      write(*,*)'Lines in file: ',nd
      call getarg(2,fin2)
      open(22,file=fin2,form='unformatted',access='direct',recl=na*8)
      call getarg(3,fint)
      call getarg(4,famp)
      scale=1.0
      if(iargc().ge.6)then
         call getarg(6,str)
         read(str,*)scale
      end if
      looksac=1
      if(iargc().ge.7)then
         call getarg(7,str)
         read(str,*)looksac
      end if
      looksdn=looksac
      if(iargc().ge.8)then
         call getarg(8,str)
         read(str,*)looksdn
      end if
      iazoff=0
      if(iargc().ge.9)then
         call getarg(9,str)
         read(str,*)iazoff
      end if
      rgoffpix1=0
      if(iargc().ge.10)then
         call getarg(10,str)
         read(str,*)rgoffpix1
      end if
      rgoffslope=0
      if(iargc().ge.11)then
         call getarg(11,str)
         read(str,*)rgoffslope
      end if
      open(32,file=fint,form='unformatted',access='direct',recl=na*8/looksac)
      open(33,file=famp,form='unformatted',access='direct',recl=na*8/looksac)

      do ldn=1,nd,looksdn
         do j=1,na
            intacc(j)=cmplx(0.,0.)
            ampacc(j)=cmplx(0.,0.)
         end do
         do kdn=0,looksdn-1
            line=ldn+kdn
            if(mod(line,1024).eq.0)write(*,*)line
!c     read in lines
            read(21,rec=line,err=99)(in1(k),k=1,na)
            irec=line+iazoff
            if(irec.lt.1)irec=1
            if(irec.gt.nd)irec=nd
            read(22,rec=irec,err=99)(in2(k),k=1,na)
!c  resample range line
            do j=1,na
               off=j+rgoffpix1+(j-1)*rgoffslope
               if(off.lt.0)off=0
               if(off.gt.na-1)off=na-1
               ioff=int(off)
               frac=off-ioff
               in2off(j)=in2(ioff)*(1-frac)+in2(ioff+1)*frac
            end do
            !print *,in2off(1:na)
!c     cross-multiply and save amplitudes
            do j=1,na
               in1(j)=in1(j)*scale
               in2(j)=in2off(j)*scale
               intgram(j)=(in1(j))*conjg(in2(j))
               amp1=cabs(in1(j))**2
               amp2=cabs(in2(j))**2
               amp(j)=cmplx(amp1,amp2)
            end do
!c     looks down first
            do j=1,na
               intacc(j)=intacc(j)+intgram(j)
               ampacc(j)=ampacc(j)+amp(j)
            end do
         end do
!c     looks across
         do j=0,na/looksac-1
            inttemp=cmplx(0.,0.)
            amptemp=cmplx(0.,0.)
            do k=1,looksac
               inttemp=inttemp+intacc(j*looksac+k)
               amptemp=amptemp+ampacc(j*looksac+k)
            end do
            intacc(j+1)=inttemp
            ampacc(j+1)=cmplx(sqrt(real(amptemp)),sqrt(aimag(amptemp)))
         end do
!c         print *,(line-1)/looksdn+1
         write(32,rec=(line-1)/looksdn+1)(intacc(k),k=1,na/looksac)
         write(33,rec=(line-1)/looksdn+1)(ampacc(k),k=1,na/looksac)
      end do
 99   continue
      end


