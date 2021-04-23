c  cmb2px4b - combine 2 byte pictures, 4 bits each

      byte in1(4000*4000),in2(4000*4000)
      character*60 file

      if(iargc().le.3)then
         write(6,*)'usage: infile(upper) infile(lower) outfile length'
         stop
      end if

      call getarg(4,file)
      read(file,*)len
      call getarg(1,file)
      open(21,file=file,access='direct',recl=len)
      call getarg(2,file)
      open(22,file=file,access='direct',recl=len)
      call getarg(3,file)
      open(23,file=file,access='direct',recl=len)

      do line=1,100000
         read(21,rec=line,err=90)(in1(k),k=(line-1)*len+1,line*len)
         read(22,rec=line,err=90)(in2(k),k=(line-1)*len+1,line*len)
      end do
 90   lines=line-1
      write(6,*)'Lines read in: ',lines
      do i=1,len*lines
         in1(i)=ior(iand(in1(i),240),iand(iand(in2(i),240)/16,15))
      end do
      do line=1,lines
         write(23,rec=line)(in1(k),k=(line-1)*len+1,line*len)
      end do
      end
