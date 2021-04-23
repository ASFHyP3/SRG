  

  complex, allocatable :: a(:,:)
  character*20 str

  if(iargc().le.1)then
     print *,'usage: createslc pixacross linesdown'
     stop
  end if

  call getarg(1,str)
  read(str,*)i
  call getarg(2,str)
  read(str,*)j

  allocate (a(i,j))

  a=cmplx(0.,0.)
  open(21,file='slc',access='stream')
  write(21)a
  close(21)
  end
