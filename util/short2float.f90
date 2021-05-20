!c  short2float - convert a short integer file to a float file

  integer*2 indata(100000)
  real outdata(100000)
  character*120 infile,outfile,str

  if(iargc().lt.3)then
     print *,'Usage: short2float infile outfile linelength(complex short integers)'
     stop
  end if

  call getarg(1,infile)
  call getarg(2,outfile)
  call getarg(3,str)
  read(str,*)len

  open(21,file=infile,access='direct',recl=len*2)
  open(22,file=outfile,access='direct',recl=len*4)
  
  do i=1,10000000
     if(mod(i,1000).eq.0)print *,'converting line ',i
     read(21,rec=i,err=99)(indata(k),k=1,len)
     do k=1,len
        outdata(k)=indata(k)
     end do
     write(22,rec=i)(outdata(k),k=1,len)
  end do

99 continue
  end

