!c  filestats - dump file status block params utility
  
  character*200 file
  integer statb(101)
  integer*8 statb2(100)
  integer*8 filelen,ibytes

  equivalence (statb2,statb)

  call getarg(1,file)

  !c get file stats
  ierr=stat(trim(file),statb)
  ibytes=statb(8)
  print *,'File length (bytes): ',ibytes,statb2(7)

  do i=1,13
     print *,i,statb(i)
  end do
  do i=1,13
     print *,i,statb2(i)
  end do

!c  compare with c reading
  i=filestats(trim(file))

end program
