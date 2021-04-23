!  scaledem - apply a linear transformation to a dem file
program scaledem

  integer*2, allocatable :: d(:,:)
  real mult
  character*300 f1,f2,str
  integer*8 filelen

  if(iargc().lt.3)then
     print *,'Usage: scaledem infile outfile len constant multiplier'
     print *,'       Formula is multiplier*dem+constant'
     stop
  end if

  call getarg(1,f1)
  call getarg(2,f2)
  call getarg(3,str)
  read(str,*)len
  call getarg(4,str)
  read(str,*)const
  call getarg(5,str)
  read(str,*)mult

  lines=filelen(f1)/len/2
  print *,'len lines ',len,lines

  allocate (d(len,lines))

  open(21,file=f1,form='unformatted',access='direct',recl=len*lines*2)
  open(22,file=f2,form='unformatted',access='direct',recl=len*lines*2)

!  read in file	
  read(21,rec=1)d

  d=d*mult+const

  write(22,rec=1)d

end program scaledem



