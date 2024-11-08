! run ps detection using cosine similarlity on a set of wrapped files
! Read also correlation and amp files
! Read also mask files (if designated), in order to account for inconsistent
! coverage
!  this version allows for averaging multiple reference points if file is supplied
!  modified to use fftw3 28jan23

! Compile as: gfortran -o cull_geolist_fromsim cull_geolist_fromsim.f90 -fopenmp

PROGRAM cull_geolist_fromsim
  use omp_lib

  IMPLICIT none

  !!!!! ----- Specifications: Declare your variables ----- !!!!!
  INTEGER::stat
  INTEGER*8::nr,naz,nslc,i
  CHARACTER(200),DIMENSION(:),ALLOCATABLE::geonames
  CHARACTER(200)::geolist,geolist_out,str,scenemaskfile,strgeo,strmask
  REAL*8,DIMENSION(:),ALLOCATABLE::prct
  REAL*8::thresh,nvalid,nvalid_temp
  INTEGER*1,DIMENSION(:,:,:),ALLOCATABLE::masks
  INTEGER*1,DIMENSION(:,:),ALLOCATABLE::tempmask,scenemask 


  !!!!! ----- Executions ----- !!!!!

  ! geolist: input geolist to check its all_similarity_masks
  ! nr: width of the files
  ! naz: number of lines in the files
  ! nslc: number of slcs in geolist
  ! geolist_out: name of output geolist
  ! scenemaskfile: name of input scenemask
  ! thresh (optional): threshold percent to be included in final geolist (default = 0.1)

  IF(iargc().lt.6)THEN
     WRITE(*,*)'usage: cull_geolist_fromsim geolist nr naz nslc geolist_out scenemaskfile <thresh=0.1>'
     STOP
  END IF

  CALL getarg(1,geolist)
  CALL getarg(2,str)
  READ(str,*)nr !width of files
  CALL getarg(3,str)
  READ(str,*)naz 
  CALL getarg(4,str)
  READ(str,*)nslc
  call getarg(5,geolist_out)
  call getarg(6,scenemaskfile)

  thresh = 0.1
  if(iargc().ge.7)then
     call getarg(7,str)
     read(str,*)thresh
  end if

  !!!!! ----- ALLOCATE THE VARIABLES ----- !!!!!
  ALLOCATE(masks(nr,naz,nslc))
  ALLOCATE(geonames(nslc))
  ALLOCATE(scenemask(nr,naz))

  !!!!! ----- READ IN THE LIST OF .GEO FILES ----- !!!!!
  OPEN(UNIT=11,FILE=geolist,STATUS='old')
  DO i=1,nslc
     READ(11,'(A)',end=111,IOSTAT=stat)geonames(i)
  END DO
 111 continue
  CLOSE(11)

  !!!!! ----- READ IN THE REFERENCE SCENEMASK ----- !!!!!
  OPEN(UNIT=11,file=scenemaskfile,access='stream')
  READ(11)scenemask
  CLOSE(11)

  !!!!! ----- Determine the total number of potentially-valid pixels ----- #####
  nvalid = sum(abs(real(scenemask)))

  !!!!! ----- READ IN THE SLC MASKS IN PARALLEL ----- !!!!!
  ALLOCATE(prct(nslc))

  !$OMP parallel do private(strgeo,strmask,tempmask,nvalid_temp) &
  !$OMP shared(geonames,masks,prct,nvalid)
  DO i=1,nslc
     ALLOCATE(tempmask(nr,naz))

     strgeo=trim(adjustl(geonames(i)))
     strmask='all_similarity_mask_' // strgeo(:8)

     OPEN(UNIT=i+10,FILE=strmask,access='stream')
     READ(i+10)tempmask
     CLOSE(i+10)
     masks(:,:,i) = tempmask

     nvalid_temp = sum(abs(real(masks(:,:,i))))
     prct(i) = nvalid_temp/nvalid

     DEALLOCATE(tempmask)
  END DO
  !$OMP end parallel do


  !!!!! ----- WRITE OUT THE OUTPUT GEOLIST ----- !!!!!
  OPEN(UNIT=21,FILE=geolist_out,status='unknown',ACTION='write')
  DO i=1,nslc
     if (prct(i).ge.thresh)then
        WRITE(21,'(A)') trim(geonames(i))
     end if
  END DO
  CLOSE(21)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!FUNCTIONS

CONTAINS

! ------------------
FUNCTION Replace_Text (s,text,rep)  RESULT(outs)
CHARACTER(*)        :: s,text,rep
CHARACTER(LEN(s)+100) :: outs     ! provide outs with extra 100 char len
INTEGER             :: i, nt, nr

outs = s ; nt = LEN_TRIM(text) ; nr = LEN_TRIM(rep)
DO
   i = INDEX(outs,text(:nt)) ; IF (i == 0) EXIT
   outs = outs(:i-1) // rep(:nr) // outs(i+nt:)
END DO
END FUNCTION Replace_Text


END PROGRAM cull_geolist_fromsim

