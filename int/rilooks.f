c  rilooks -  average real,imaginary looks
      complex a(20480),b(20480)
      real b1(20480),b2(20480)
      character*30 fin,fout
      integer statb(13),fstat

      if(iargc().lt.4)then
         write(*,*)'usage: rilooks infile outfile length looksac <looksdn>'
         stop
      end if

      call getarg(1,fin)
      call getarg(3,fout)
      read(fout,*)na
      open(21,file=fin,form='unformatted',access='direct',recl=na*8)
      ierr=fstat(21,statb)
      nd=statb(8)/8/na
      write(*,*)'Lines in file: ',nd
      call getarg(4,fout)
      read(fout,*)la
      if(iargc().ge.5)then
         call getarg(5,fout)
         read(fout,*)ld
      else
         ld=la
      end if
      call getarg(2,fout)
      open(22,file=fout,form='unformatted',access='direct',recl=na/la*8)
      
      lineout=0
      do line=1,nd,ld
         if(mod(line,512).eq.1)write(*,*)line
         lineout=lineout+1
         do j=1,na
            b1(j)=0.
            b2(j)=0.
         end do

c  take looks down
         do i=0,ld-1
            read(21,rec=line+i,err=99)(a(k),k=1,na)
            do j=1,na
               b1(j)=b1(j)+real(a(j))**2
               b2(j)=b2(j)+aimag(a(j))**2
            end do
         end do
c  take looks across
         jpix=0
         do j=1,na,la
            jpix=jpix+1
            sum1=0.
            sum2=0.
            do k=0,la-1
               sum1=sum1+b1(j+k)
               sum2=sum2+b2(j+k)
            end do
            b(jpix)=cmplx(sqrt(sum1),sqrt(sum2))
         end do
         write(22,rec=lineout)(b(k),k=1,na/la)
      end do
 99   continue
      end





