c  makeint - make an interferogram from a vexcel normalized int. and amp files
      complex a(20480)
      real in2(20480),in3(20480)
      character*60 f1in,f2in,f3in,fout,str
      integer statb(13),fstat

      if(iargc().lt.5)then
         write(*,*)'usage: makeint normintfile ampfile1 ampfile2 outfile length <lines>'
         stop
      end if

      call getarg(1,f1in)
      call getarg(2,f2in)
      call getarg(3,f3in)
      call getarg(5,fout)
      read(fout,*)na
      call getarg(4,fout)
c  open input files
      open(21,file=f1in,form='unformatted',access='direct',recl=na*8)
      ierr=fstat(21,statb)
      nd=statb(8)/8/na
      write(*,*)'Lines in file: ',nd
      if(iargc().ge.6)then
         call getarg(6,str)
         read(str,*)nd
      end if

      open(22,file=f2in,form='unformatted',access='direct',recl=na*4)
      open(23,file=f3in,form='unformatted',access='direct',recl=na*4)

      open(24,file=fout,form='unformatted',access='direct',recl=na*8)

      do line=1,nd
         if(mod(line,256).eq.1)write(*,*)line
         read(21,rec=line)(a(k),k=1,na)
         read(22,rec=line)(in2(k),k=1,na)
         read(23,rec=line)(in3(k),k=1,na)
         do j=1,na
            a(j)=a(j)*sqrt(in2(j)*in3(j))
         end do

         write(24,rec=line)(a(k),k=1,na)
      end do
      end

