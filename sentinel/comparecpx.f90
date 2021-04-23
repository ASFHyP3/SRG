  complex a(100000),b(100000)
  character*300 f1,f2,str

  if(iargc().lt.3)then
     print *,'usage: comparecpx file1 file2 len line1 nlines samp1 nsamps'
     stop
  end if
  
  call getarg(1,f1)
  call getarg(2,f2)
  call getarg(3,str)
  read(str,*)len
  call getarg(4,str)
  read(str,*)l1
  call getarg(5,str)
  read(str,*)nl
  call getarg(6,str)
  read(str,*)i1
  call getarg(7,str)
  read(str,*)ni

  open(21,file=f1,access='direct',recl=len*8)
  open(22,file=f2,access='direct',recl=len*8)

  do line=l1,l1+nl-1
     read(21,rec=line)(a(k),k=1,len)
     print *,'F1 ',(a(k),k=i1,i1+ni-1)
     read(22,rec=line)(a(k),k=1,len)
     print *,'F2 ',(a(k),k=i1,i1+ni-1)
  end do

end program
