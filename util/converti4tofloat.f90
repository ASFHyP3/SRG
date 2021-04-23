  !c converti4tofloat - convert file format

  integer*4 initdk, ioread, iowrit, nread, nwr, lun
  integer*4 fdin1, fdin2, fdout
  integer*4 width
  integer*4, allocatable :: a2(:)
  real*4, allocatable ::    b(:)
  character*360 name

  if(iargc() .lt. 3) then
     print *,'usage: converti4tofloat infile outfile width'
     stop
  end if

  call getarg(1, name)
  fdin2 = initdk(lun,name)
  call getarg(2,name)
  fdout = initdk(lun,name)
  call getarg(3,name)
  read(name,*) width

  allocate (a2(width), b(width))

  !c  loop over line number

  do i=1,10000000

     nread = ioread(fdin2,a2,width*4)
     if(nread .ne. width*4) stop 'end of file'
     b=float(a2)
     nwr = iowrit(fdout,b,width*4)
     
  end do

end 
