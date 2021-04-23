!c  unw_interpolate : interpolate a new unw file from two existing ones

      real*4, allocatable:: in1(:,:),in2(:,:),out(:,:)
      character*300 fin1,fin2,str,fout
      integer*8 nbytes,filelen
      real*8 jd1, jd2, jdout
      real*4 frac

      if(iargc().lt.7)then
         write(*,*)'usage: unw_interpolate infile1 infile2 outfile jd1 jd2 jdout length <valid_lines>'
         stop
      end if

      call getarg(1,fin1)
      call getarg(2,fin2)
      call getarg(3,fout)
      call getarg(4,str)
      read(str,*)jd1
      call getarg(5,str)
      read(str,*)jd2
      call getarg(6,str)
      read(str,*)jdout
      call getarg(7,str)
      read(str,*)na
      if(iargc().ge.8)then
         call getarg(8,str)
         read(str,*)nd
      end if

      open(21,file=fin1,form='unformatted',access='stream')
      if(iargc().lt.8)then
         nbytes=filelen(trim(fin1))
         nd=nbytes/8/na
         write(*,*)'Lines in file: ',nd
      end if

      open(22,file=fin2,form='unformatted',access='stream')
      open(23,file=fout,form='unformatted',access='direct',recl=na*nd*8)

      allocate (in1(na*2,nd), in2(na*2,nd), out(na*2,nd))

      read(21)in1
      read(22)in2

!  calculate fraction and resample
      frac=(jdout-jd1)/(jd2-jd1)
      out=in1*(1-frac)+in2*frac

      write(23,rec=1)out

      deallocate (in1, in2, out)
      close(21)
      close(22)
      close(23)

      end


