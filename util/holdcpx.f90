!c
!c  holdcpx - compute 0, etc order hold of a complex image
!c

  complex*8, allocatable :: in(:,:), out(:,:)
  character*100 :: infile, outfile, str
  integer :: stat,statb(13)

  if (iargc().lt.5)then
     print *,'Usage: holdcpx infile outfile len zoomfactor order'
     print *,'  Note: only zero order implemented here'
     call exit(0)
  end if

  call getarg(1,infile)
  call getarg(2,outfile)
  call getarg(3,str)
  read (str,*)len
  call getarg(4,str)
  read (str,*)izoom
  call getarg(5,str)
  read (str,*)iorder

  ierr=stat(infile,statb)
  lines=statb(8)/len/8
  print *,'Input length, lines: ',len,lines

!c  allocate arrays
  allocate (in(len,lines), out(len*izoom,lines*izoom))

  open(21,file=infile,access='direct',recl=len*lines*8)
  read(21,rec=1)in
  close(21)

  do line=1,lines
     do lineline=1,izoom
        do i=1,len
           do ii=1,izoom
              out((i-1)*izoom+ii,(line-1)*izoom+lineline)=in(i,line)
           end do
        end do
     end do
  end do

  open(22,file=outfile,access='direct',recl=len*lines*izoom*izoom*8)
  write(22,rec=1)out
  close(22)

  end

