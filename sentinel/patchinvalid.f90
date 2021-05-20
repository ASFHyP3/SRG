  !c  patchinvalid
  !c
  !c  read in a dem file and fill -32768 holes with 0

  integer*2, allocatable :: dem(:)
  integer*8 filelen, npoints
  character*300 filename
  
  if(iargc().lt.1)then
     print *,'Usage: patchinvalid demfile'
     call exit
  end if

  call getarg(1,filename)
  
  npoints=filelen(filename)/2
  print *,'Points in dem: ',npoints

  allocate (dem(npoints))

  open(21,file=filename,access='direct',recl=npoints*2)
  read(21,rec=1)dem
  do i=1,npoints
     if(dem(i).eq.-32768)dem(i)=0
  end do
  write(21,rec=1)dem
  close(21)
end program
