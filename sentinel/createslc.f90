  

  complex, allocatable :: a(:,:)
  character*20 str
  character*300 outfile

  if(iargc().le.1)then
     print *,'usage: createslc pixacross linesdown <outfile=slc>'
     stop
  end if

  call getarg(1,str)
  read(str,*)i
  call getarg(2,str)
  read(str,*)j

  outfile='slc'
  if(iargc().ge.3)call getarg(3,outfile)
  
  allocate (a(i,j))

  a=cmplx(0.,0.)
  open(21,file=trim(outfile),access='stream')
  write(21)a
  close(21)
  end
