!c  sort raw sentinel product by swath
!
!    assume swaths are 10, 11, and 12
!    assume line length is 30000 complex

  complex data(30000)
  character*200 rawfile, swathfile, swath1, swath2, swath3
  
  if(iargc().lt.2)then
     print *,'Usage: sortswath rawdatafile swathfile'
     stop
  end if

  call getarg(1,rawfile)
  call getarg(2,swathfile)

  !  get base file name
  loc=index(rawfile,'.dat')
  print *,'base name: ',rawfile(1:loc-1)

  swath1=rawfile(1:loc-1)//'.1.dat'
  swath2=rawfile(1:loc-1)//'.2.dat'
  swath3=rawfile(1:loc-1)//'.3.dat'

  ! input files
  open(11,file=rawfile,access='direct',recl=30000*8)
  open(12,file=swathfile)
  ! output files
  open(21,file=swath1,access='stream')!,form='unformatted',recl=30000*8)
  open(22,file=swath2,access='stream')!,form='unformatted',recl=30000*8)
  open(23,file=swath3,access='stream')!,form='unformatted',recl=30000*8)

  ! loop over each line, sort by swath parameter
  do i=1,1000000
     if(mod(i,2000).eq.0)print *,'At line ',i
     read(12,*,end=99)iline,ielevation,iswath
     read(11,rec=i)data
     if(iswath.eq.10)write(21)data
     if(iswath.eq.11)write(22)data
     if(iswath.eq.12)write(23)data
  end do
99 print *,'Processed lines: ',i-1

end program


  
