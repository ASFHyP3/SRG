c  powlooks -  average looks in power
      complex a(20480),b(20480)
      character*30 fin,fout

      if(iargc().lt.4)then
         write(*,*)'usage: powlooks infile outfile pixac looksac <looksdn>'
         stop
      end if

      call getarg(3,fin)
      read(fin,*)na
      call getarg(4,fin)
      read(fin,*)la
      if(iargc().ge.5)then
         call getarg(5,fin)
         read(fin,*)ld
      else
         ld=la
      end if
      call getarg(1,fin)
      call getarg(2,fout)

      nd=100000

      open(21,file=fin,form='unformatted',access='direct',recl=na*8)
      open(22,file=fout,form='unformatted',access='direct',recl=na/la*8)
      
      lineout=0
      do line=1,nd,ld
         if(mod(line,1024).eq.1)write(*,*)line
         lineout=lineout+1
         do j=1,na
            b(j)=cmplx(0.,0.)
         end do

c  take looks down
         if(ld.eq.1)then
            read(21,rec=line,err=99)(a(k),k=1,na)
            do j=1,na
               b(j)=cabs(a(j))**2
            end do
         else
         do i=0,ld-1
            read(21,rec=line+i,err=99)(a(k),k=1,na)
            do j=1,na
               b(j)=b(j)+cabs(a(j))**2
            end do
         end do
         end if
c  take looks across
         jpix=0
         do j=1,na,la
            jpix=jpix+1
            sum=0.
            do k=0,la-1
               sum=sum+real(b(j+k))
            end do
            b(jpix)=cmplx(sqrt(sum),0.)
         end do
         write(22,rec=lineout)(b(k),k=1,na/la)
      end do
 99   continue
      end
