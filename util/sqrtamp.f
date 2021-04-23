c  sqrtamp -  get square root of amp file
      real b1(20480)
      character*60 fin,fout
      integer statb(13),fstat

      if(iargc().lt.3)then
         write(*,*)'usage: sqrtamp infile outfile length'
         stop
      end if

      call getarg(1,fin)
      call getarg(3,fout)
      read(fout,*)na
      open(21,file=fin,form='unformatted',access='direct',recl=na*8)
      ierr=fstat(21,statb)
      nd=statb(8)/8/na
      write(*,*)'Lines in file: ',nd
      call getarg(2,fout)
      open(22,file=fout,form='unformatted',access='direct',recl=na*8)
      
      do line=1,nd
         if(mod(line,64).eq.1)write(*,*)line
         read(21,rec=line,err=99)(b1(k),k=1,na*2)
         do j=1,na*2
            b1(j)=sqrt(b1(j))
         end do
         write(22,rec=line)(b1(k),k=1,na*2)
      end do
 99   continue
      end





