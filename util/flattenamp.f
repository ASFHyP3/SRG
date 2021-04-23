c  flattenamp -  flatten brightness of amp file
      complex b1(20480)
      real acc(20480),s(20480)
      character*60 fin,fout
      integer statb(13),fstat

      if(iargc().lt.3)then
         write(*,*)'usage: flattenamp infile outfile length'
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
      
      do j=1,na
         acc(j)=0.
      end do

c  find average brightness
      do line=1,nd
         if(mod(line,1000).eq.1)write(*,*)line
         read(21,rec=line,err=99)(b1(k),k=1,na)
         do j=1,na
            acc(j)=b1(j)/nd+acc(j)
         end do
      end do
c  smooth
      do j=3,na-2
         s(j)=acc(j-2)+acc(j-1)+acc(j)+acc(j+1)+acc(j+2)
      end do
      s(1)=s(3)
      s(2)=s(3)
      s(na-1)=s(na-2)
      s(na)=s(na-2)

      do line=1,nd
         if(mod(line,1000).eq.1)write(*,*)line
         read(21,rec=line,err=99)(b1(k),k=1,na)
         do j=1,na
            b1(j)=b1(j)/s(j)/5
         end do
         write(22,rec=line)(b1(k),k=1,na)
      end do

 99   continue
      end

