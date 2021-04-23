c  crossdiv - cross divide two files, form int file
      complex in1(20480),in2(20480),int(20480),amp(20480)
      character*30 fin1,fin2,str,fint,famp
      integer statb(13),fstat

      if(iargc().lt.4)then
         write(*,*)'usage: crossdiv infile1 infile2 outfile length <scale=1>'
         print *,'scale is multiplied by each scene to prevent overflow'
         stop
      end if

      call getarg(1,fin1)
      call getarg(4,str)
      read(str,*)na
      open(21,file=fin1,form='unformatted',access='direct',recl=na*8)
      ierr=fstat(21,statb)
      nd=statb(8)/8/na
      write(*,*)'Lines in file: ',nd
      call getarg(2,fin2)
      open(22,file=fin2,form='unformatted',access='direct',recl=na*8)
      call getarg(3,fint)
      open(32,file=fint,form='unformatted',access='direct',recl=na*8)
      scale=1.0
      if(iargc().ge.5)then
         call getarg(5,str)
         read(str,*)scale
      end if
      
      do line=1,nd
         if(mod(line,64).eq.0)write(*,*)line
c  read in lines
            read(21,rec=line,err=99)(in1(k),k=1,na)
            read(22,rec=line,err=99)(in2(k),k=1,na)
c  cross-divide
            do j=1,na
               in1(j)=in1(j)*scale
               in2(j)=in2(j)*scale
               int(j)=in1(j)/in2(j)
            end do
         write(32,rec=line)(int(k),k=1,na)
      end do
 99   continue
      end


