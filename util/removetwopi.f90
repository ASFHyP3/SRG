  !c  removetwopi - reference a set of unw files to a single point by subtracting 2 pi
  
  real*4, allocatable :: in(:,:),out(:,:)
  character*100 infile, outfile, str, filelist, fin(1000)
  integer statb(13)

  if (iargc().lt.4)then
     print *,'usage: removetwopi filelist(mht) len xloc yloc '
     stop
  end if

  call getarg(1,filelist)
  call getarg(2,str)
  read(str,*)len
  call getarg(3,str)
  read(str,*)ixloc
  call getarg(4,str)
  read(str,*)iyloc


  !c read in the input file list, containing unwrapped interferograms to be printed
  open(21,file=filelist)
  do i=1,10000
     print *,filelist
     read(21,*,end=11)fin(i)
  end do
11 close(21)
  nigrams=i-1
  print *,'Input files: ',nigrams

  !c get file length
  open(21,file=trim(fin(1)),form='unformatted',access='direct',recl=len*8)
  ierr=fstat(21,statb)
  lines=statb(8)/8/len
  !print *,'fin(1) len lines ierr ',fin(1),len,lines,ierr
  write(*,*)'Lines in input file: ',lines
  close(21)

  !c set output file params and open
  lenout=len
  linesout=lines
  allocate(in(len*2,lines),out(lenout*2,linesout))

  !c  loop over file list
  do igram=1,nigrams
     open(21,file=trim(fin(igram)),access='direct',recl=len*lines*8)
     read(21,rec=1)in

     ! refphase at point
     refphase=sum(in(len+ixloc-3:len+ixloc+3,iyloc-3:iyloc+3))/49.
     out=in-refphase
     !c  write out in mht format
     write(21,rec=1)out
     close(21)
  end do

end program
