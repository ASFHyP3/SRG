!c** cpx2float - convert cpx magnitude file to a single float file

      character*80 fin,fout,str
      integer statb(13),stat
      integer*8 n
      complex a(32768)
      real p(32768)

      if(iargc().lt.3)then
         write(*,*)'usage: cpx2float infile outfile len'
         stop
      end if
       
      call getarg(3,fout)
      read(fout,*)len
      call getarg(1,fin)
      call getarg(2,fout)

      ierr = stat(fin,statb)
      n=statb(8)
      if(n.le.0)n=n+2**31+2**31
      lines=n/len/8

      write(*,*)'File length, lines: ',lines

      open(21,file=fin,access='direct',recl=len*8)
      open(31,file=fout,access='direct',recl=len*4)

      do i=1,lines
         read(21,rec=i)(a(k),k=1,len)
         do k=1,len
            p(k)=cabs(a(k))
         end do
         write(31,rec=i)(p(k),k=1,len)
      end do

      end
