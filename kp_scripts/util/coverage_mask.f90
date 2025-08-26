! Determine where the satellite acquired valid data (orbital coverage)
! Read in an SLC (or multilooked SLC) and find if data was acquired pixel by
! pixel (in parallel)

! Compile as: gfortran -o coverage_mask coverage_mask.f90

PROGRAM coverage_mask

  IMPLICIT none

  !specifications
  INTEGER::i,j
  INTEGER*8 ::nr, naz
  CHARACTER(200)::str
  CHARACTER(200)::filename,outfile
  REAL,DIMENSION(:,:),ALLOCATABLE::amp
  complex*8,dimension(:,:),allocatable :: slc
  integer*1,dimension(:,:),allocatable :: bytemask

  !executions

  !slcfile: name of SLC file to load
  !nr: number of range bins
  !naz: number of azimuth bins
  !outfile: name of output
  IF(iargc().lt.3)THEN
     WRITE(*,*)'usage: coverage_mask slcfile len wid outfile'
     STOP
  END IF

  CALL getarg(1,filename)
  CALL getarg(2,str)
  READ(str,*)nr !width of file
  CALL getarg(3,str)
  READ(str,*)naz !length of file
  CALL getarg(4,outfile)


  ! Allocate arrays
  ALLOCATE(amp(nr,naz),bytemask(nr,naz))
  ALLOCATE(slc(nr,naz))

  ! Load in the SLC file (filename)
  print *,'Reading ',filename
  OPEN(UNIT=10,FILE=filename,FORM='unformatted',ACCESS='direct',RECL=2*nr*naz*4,CONVERT='LITTLE_ENDIAN')
  READ(10,rec=1)slc ! read in complex SLC
  CLOSE(10)
  amp=cabs(slc)**2 ! calculate the amplitude (power) from SLC

  ! Create the mask given the SLC amplitude
  print *,'Calculating mask'
  bytemask=0
  do j=1,naz
     do i=1,nr
        if(amp(i,j).ne.0)then
           bytemask(i,j)=-1
        end if
     end do
  end do

  ! Write out the make to outfile
  open(unit=20,file=outfile,form='unformatted',status='replace',access='direct',recl=nr*naz)
  write(20,rec=1)bytemask
  close(20)
  print *,'Mask saved to ',outfile

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END PROGRAM coverage_mask

