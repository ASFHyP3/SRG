c  cmb2float2rg - combine 2 float pictures to an rg image

      real*4 in1(65536),in2(65536)
      complex rg(65536)
      character*60 file

      if(iargc().le.3)then
         write(6,*)'usage: infile 1  infile 2  outfile  length'
         stop
      end if

      call getarg(4,file)
      read(file,*)len
      call getarg(1,file)
      open(21,file=file,access='direct',recl=len*4)
      call getarg(2,file)
      open(22,file=file,access='direct',recl=len*4)
      call getarg(3,file)
      open(23,file=file,access='direct',recl=len*8)

      do line=1,100000
         read(21,rec=line,err=90)(in1(k),k=1,len)
         read(22,rec=line,err=90)(in2(k),k=1,len)
         do i=1,len
            rg(i)=cmplx(in1(i),in2(i))
         end do
         write(23,rec=line)(rg(k),k=1,len)
      end do
 90   lines=line-1
      write(6,*)'Lines read in: ',lines

      end
