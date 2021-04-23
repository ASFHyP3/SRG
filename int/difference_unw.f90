!c  difference_unw - subtract one unw from another

      real, allocatable:: in1(:,:),in2(:,:),out(:,:)
      character*300 fin1,fin2,str,fout
      integer*8 nbytes,filelen

      if(iargc().lt.4)then
         write(*,*)'usage: difference_unw infile1 infile2 outfile length <valid_lines>'
         stop
      end if

      call getarg(1,fin1)
      call getarg(4,str)
      read(str,*)na
      nbytes=filelen(trim(fin1))
      nd=nbytes/8/na
      write(*,*)'Lines in file: ',nd
      call getarg(2,fin2)
      call getarg(3,fout)
      nvalid=nd
      if(iargc().ge.5)then
         call getarg(5,str)
         read(str,*)nvalid
      end if

      open(21,file=fin1,form='unformatted',access='direct',recl=na*nvalid*8)
      if(trim(fin1).ne.trim(fin2))then
         open(22,file=fin2,form='unformatted',access='direct',recl=na*nvalid*8)
      end if
      open(32,file=fout,form='unformatted',access='direct',recl=na*nvalid*8)

      !c  allocate the local arrays
      allocate (in1(na*2,nvalid),in2(na*2,nvalid),out(na*2,nvalid))

!c     read in lines
      read(21,rec=1)in1
      if(fin1.ne.fin2)then
         read(22,rec=1)in2
      else
         in2=in1
      end if
!c subtract

      out(1:na,:)=in1(1:na,:)
      out(na+1:na*2,:)=in1(na+1:na*2,:)-in2(na+1:na*2,:)

      write(32,rec=1)out

      deallocate (in1, in2, out)

      end


