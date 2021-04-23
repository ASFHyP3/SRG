c  slc2intamp - create multilooked int and amp files from 2 slc files
      complex in1(20480),in2(20480),igram(20480),amp(20480),sum,pha,phd
      real amp1(20480),amp2(20480)
      character*30 fin1,fin2,fint,famp,str
      integer statb(13),fstat

      if(iargc().lt.5)then
         write(*,*)'usage: slc2intamp infile1 infile2 intoutfile ampoutfile '
         write(*,*)'  length looksac <looksdn> <ph-ac> <ph-dn>'
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
      call getarg(3,fint)
      call getarg(4,famp)
      if(iargc().ge.6)then
         call getarg(6,str)
         read(str,*)la
      else
         la=1
      end if
      if(iargc().ge.7)then
         call getarg(7,str)
         read(str,*)ld
      else
         ld=la
      end if
      if(iargc().ge.8)then
         call getarg(8,str)
         read(str,*)pa
      else
         pa=0.
      end if
      if(iargc().ge.9)then
         call getarg(9,str)
         read(str,*)pd
      else
         pd=0.
      end if
      open(22,file=fin2,form='unformatted',access='direct',recl=na*8)
      open(31,file=fint,form='unformatted',access='direct',recl=na/la*8)
      open(32,file=famp,form='unformatted',access='direct',recl=na/la*8)
      
c  phase ramp factors across and down
      pha=cmplx(cos(pa),sin(pa))
      phd=cmplx(cos(pd),sin(pd))

      lineout=0
      do line=1,nd,ld
         if(mod(line,256).eq.1)write(*,*)line
         lineout=lineout+1
c  zero out accumulators
         do j=1,na
            igram(j)=cmplx(0.,0.)
            amp1(j)=0.
            amp2(j)=0.
         end do

c  take looks down
         do i=0,ld-1
c  read in data from both files
            read(21,rec=line+i,err=99)(in1(k),k=1,na)
            read(22,rec=line+i,err=99)(in2(k),k=1,na)
c  cross multiply pixels first, applying phase ramp to igram
c   and accumulate powers for the amp file
            do j=1,na
               igram(j)=igram(j)+in1(j)*conjg(in2(j))*pha**j*phd**(line+i)
               amp1(j)=amp1(j)+cabs(in1(j))**2
               amp2(j)=amp2(j)+cabs(in2(j))**2
            end do
         end do
c  take looks across
         jpix=0
         do j=1,na,la
            jpix=jpix+1
            sum=cmplx(0.,0.)
            sum1=0.
            sum2=0.
            do k=0,la-1
               sum=sum+igram(j+k)
               sum1=sum1+amp1(j+k)
               sum2=sum2+amp2(j+k)
            end do
            igram(jpix)=sum
            amp(jpix)=cmplx(sqrt(sum1),sqrt(sum2))
         end do
         write(31,rec=lineout)(igram(k),k=1,na/la)
         write(32,rec=lineout)(amp(k),k=1,na/la)
      end do
 99   continue
      end

