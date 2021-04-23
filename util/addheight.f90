!c  add a value to a dem

  integer*2, allocatable :: dem(:,:)
  character*300 demfile,outfile,str

  if(iargc().lt.4)then
     print *,'Usage: demfile outfile len lines value'
     stop
  end if

  call getarg(1,demfile)
  call getarg(2,outfile)
  call getarg(3,str)
  read(str,*)len
  call getarg(4,str)
  read(str,*)lines
  call getarg(5,str)
  read(str,*)ival

  allocate (dem(len,lines))

!c  read in the dem
  open(21,file=demfile,access='direct',recl=len*2*lines)
  read(21,rec=1)dem
  close(21)

!c save 
  open(21,file=outfile,access='direct',recl=len*2*lines)
  dem=dem+ival
  write(21,rec=1)dem
  close(21)

  end

