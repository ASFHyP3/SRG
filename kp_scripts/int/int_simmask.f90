! given two input mask filenames, generate a new mask file for interpolation
! (phase reconstruction)
! saves as output filename (which will most-often be temp_mask)

! compile as: gfortran -o int_simmask int_simmask.f90

PROGRAM int_simmask
  IMPLICIT none

  !specifications
  INTEGER::i,j,stat,fstat,ierr,k
  INTEGER*8 ::nr, naz !image size
  INTEGER,DIMENSION(13)::statb
  CHARACTER(200)::filename1,filename2,outfile,str
  INTEGER*1,DIMENSION(:,:),ALLOCATABLE::mask1,mask2,newmask
  !integer*1,dimension(:,:),allocatable :: newmask

  !executions

  !flist: list of wrapped intrerferogram files
  !nfiles: number of files in flist
  !nslcs: number of slc files
  !len: width of the files
  IF(iargc().lt.3)THEN
     WRITE(*,*)'usage: int_simmask  mask1 mask2 len <outfile=temp_mask>'
     STOP
  END IF

  CALL getarg(1,filename1) !mask name #1
  CALL getarg(2,filename2) !mask name #2
  CALL getarg(3,str)
  READ(str,*)nr !width of files
  outfile='temp_mask'
  if(iargc().ge.4)then
     call getarg(4,outfile) !new mask output filename
  end if


  ! Figure out how many lines there are in file
  !open(unit=2, file=filename1, form="unformatted", access="direct", recl=nr*8)
  open(unit=2,file=filename1,status='old',access='stream')
  ierr=fstat(2,statb)
  naz=statb(8)/nr
  print*,naz
  close(2)

  ALLOCATE(mask1(nr,naz),mask2(nr,naz))

  ! Read mask 1 from file
  open(unit=10,file=filename1,access='stream')
  read(10)mask1
  close(10)

  ! Read mask 2 from file
  open(unit=20,file=filename2,access='stream')
  read(20)mask2
  close(20)

  ALLOCATE(newmask(nr,naz))

  newmask=0
  k=0
  ! Create the new matrix (intersection of original two) using bitwise AND
  do j=1,naz
     do i=1,nr
        if(mask1(i,j).ne.0.AND.mask2(i,j).ne.0)then
           newmask(i,j) = -1
           k=k+1
        end if
     end do
  end do


  ! Save new mask to file
  open(unit=30,file=outfile, form="unformatted",status="replace",access="direct",recl=nr*naz)
  write(30,rec=1)newmask
  close(30)

  print *,'Saved interferogram mask to ',outfile
  print *,'number of points ',k

end program int_simmask 
