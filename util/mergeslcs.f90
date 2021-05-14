!
!  mergeslcs - merge 2 slcs for more complete coverage
!
!  usage: mergeslcs slc1 slc2 width <outfile>
!
!  if no outfile is supplied then the result overwrites slc1
!

  character*300 f1, f2, outfile, str
  complex*8, allocatable :: data1(:), data2(:)

  if(iargc().lt.5)then
     print *,'usage: mergeslcs slc1 slc2 outfile width length'
     stop
  end if

  call getarg(1,f1)
  call getarg(2,f2)
  call getarg(3,outfile)
  call getarg(4,str)
  read(str,*)nx
  call getarg(5,str)
  read(str,*)ny

  allocate (data1(nx), data2(nx))

  open(21,file=f1,access='direct',recl=nx*8)
  open(22,file=f2,access='direct',recl=nx*8)
  open(23,file=outfile,access='direct',recl=nx*8)

  do i=1,ny
     read(21,rec=i)data1
     read(22,rec=i)data2

     do j=1,nx
        if(cabs(data1(j)).le.1.d-15)data1(j)=data2(j)
     end do

     write(23,rec=i)data1

  end do

end program
