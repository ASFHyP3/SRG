c  makeamp - make an amp file from two real power files
      complex a(20480)
      real in1(20480),in2(20480)
      character*30 f1in,f2in,fout
      integer statb(13),fstat

      if(iargc().lt.4)then
         write(*,*)'usage: makeamp infile1 infile2 outfile length'
         stop
      end if

      call getarg(1,f1in)
      call getarg(2,f2in)
      call getarg(4,fout)
      read(fout,*)na
      call getarg(3,fout)
      open(21,file=f1in,form='unformatted',access='direct',recl=na*4)
      ierr=fstat(21,statb)
      nd=statb(8)/4/na
      write(*,*)'Lines in file: ',nd
      open(22,file=f2in,form='unformatted',access='direct',recl=na*4)

      open(23,file=fout,form='unformatted',access='direct',recl=na*8)
      

      do line=1,nd
         if(mod(line,64).eq.1)write(*,*)line
         read(21,rec=line)(in1(k),k=1,na)
         read(22,rec=line)(in2(k),k=1,na)
         do j=1,na
            a(j)=cmplx(sqrt(in1(j)),sqrt(in2(j)))
         end do

         write(23,rec=line)(a(k),k=1,na)
      end do
      end

