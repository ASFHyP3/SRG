! Determine valid geocoded slcs and pixels within the scene.
! This will result in two new geolists:
!     geolist_consistent = list of SLCs that share consistent coverage. Script
!     maximizes number of SLCs with the same coverage (could be improved to
!     optimize temporal & spatial coverage)
!     
!     geolist_culled = list of SLCs that cover at least thresh% of the valid
!     study area. The valid area is the union of all scenes' coverage (determined
!     from the input masks). The default threshold is 30% of valid.
!
! The script also outputs summation maps (The number of valid acquisitions at
! each pixel):
!     sum_coverage_geolist_ml (based on all geocoded slcs that have been
!     processed)
!
!     sum_coverage_geolist_consistent (based on geolist_consistent)
!
!     sum_coverage_geolist_culled (based on geolist_culled)
!
! The script will also output the corresponding binary masks:
!     scene_mask_ml = indicates where sum_coverage_geolist_ml > 1
!
!     scene_mask_consistent = indicates where sum_coverage_geolist_consistent
!     equals the number of consistent SLCs
!
!     scene_mask_culled = indicates where sum_coverage_geolist_culled > 1

! NOTE: the script could be improved by excluding valid scene edges (e.g. far
! range or close range), since these are wildly inconsistent and we don't expect
! the accuracy at these locations to be very high.


! Compile with:
! gfortran -o determine_valid determine_valid.f90 -fopenmp -lrt -lpthread

PROGRAM determine_valid
  use omp_lib

  IMPLICIT none

  !!!!! SPECIFICATIONS !!!!!
 
  ! INPUTS
  INTEGER:: nslc
  INTEGER*8::nr,naz
  CHARACTER(100)::geolist
  REAL::thresh

  ! ERROR/STATUS CHECKS/COUNTING VARIABLES
  INTEGER::k,j,stat,ierr,stat2,ios
  INTEGER,DIMENSION(13)::statb
  CHARACTER(100)::str

  ! READING IN INPUTS LISTS AND DEFINING OTHER VARIABLES
  CHARACTER(100),DIMENSION(:),ALLOCATABLE::geonames

  ! DATA ARRAYS FOR INPUT MASKS
  INTEGER,DIMENSION(:,:,:),ALLOCATABLE::scenemasks
  INTEGER*1,DIMENSION(:,:),ALLOCATABLE::mask
  INTEGER,DIMENSION(:,:),ALLOCATABLE::mask_count

  ! LOADING IN THE MASKS
  CHARACTER(100)::strgeo,strmask

  ! ARRAYS FOR SUMMING VALID ACQUISITIONS
  INTEGER,DIMENSION(:,:),ALLOCATABLE::sum_full,sum_consistent,sum_culled
  REAL,DIMENSION(:,:),ALLOCATABLE::temp_array

  ! ARRAYS FOR IDENTIFYING POTENTIALLY VALID PIXELS (MASKS)
  INTEGER,DIMENSION(:,:),ALLOCATABLE::valid_full,valid_consistent,valid_culled
  INTEGER*8::nvalid_full,nvalid_consistent,nvalid_culled

  ! FOR DETERMINING COVERAGE GROUPS
  INTEGER,DIMENSION(:),ALLOCATABLE::pixel_counts,counts,counts_groups
  REAL,DIMENSION(:),ALLOCATABLE::prct_of_valid,bins,bins_groups
  REAL::dp
  INTEGER::nslc_consistent,nslc_culled
  INTEGER, DIMENSION(1) :: max_idx
  INTEGER::n,idx_consistent,counter,n_groups
  INTEGER,DIMENSION(:),ALLOCATABLE::index_consistent,index_culled

  ! NAMES FOR OUTPUT FILES
  CHARACTER(100)::geolist_consistent,geolist_culled
  CHARACTER(100)::sum_full_out,sum_consistent_out,sum_culled_out
  CHARACTER(100)::scenemask_full_out,scenemask_consistent_out,scenemask_culled_out

  ! ARRAYS FOR OUTPUT GEOLISTS
  CHARACTER(100),DIMENSION(:),ALLOCATABLE::geonames_consistent,geonames_culled

  ! MASK OUTPUT ARRAYS
  INTEGER*1,DIMENSION(:,:),ALLOCATABLE::mask_full,mask_consistent,mask_culled


  !!!!! EXECUTIONS !!!!!
  ! check to see if proper number of arguments were provided
  IF(iargc().lt.4)THEN
     WRITE(*,*)'usage: determine_valid geolist nslc nr naz <prct_thresh=30>'
     STOP
  END IF

  ! read in arguments and convert to appropriate type (where necessary)
    CALL getarg(1,geolist)
    CALL getarg(2,str)
    READ(str,*)nslc
    CALL getarg(3,str)
    READ(str,*)nr
    CALL getarg(4,str)
    READ(str,*)naz
    
    thresh=30
    IF(iargc().gt.4)THEN
       CALL getarg(5,str)
       READ(str,*)thresh
    END IF

  ! Allocate the list of .geo files and the scenemasks array
    ALLOCATE(geonames(nslc))
    ALLOCATE(scenemasks(nr,naz,nslc))

  ! Read in the geolist
    OPEN(unit=11,FILE=geolist,STATUS='old')
    READ(11,'(A)',end=110,IOSTAT=stat2)geonames
  110 continue
    CLOSE(11)

  !!!!! ----- READ IN ALL MASKS AND SUMMARIZE ----- !!!!!
   
     scenemasks=0
  ! Read in the scene masks
    !$OMP parallel do private(strgeo,strmask,mask) &
    !$OMP shared(geonames,scenemasks)
    DO k=1,nslc
       ALLOCATE(mask(nr,naz))
       strgeo = trim(adjustl(geonames(k)))
       strmask = Replace_Text(strgeo,'.geo','.mask')

       ! Read the corresponding bytemask file
       open(unit=k+10,file=strmask,access='stream')
       read(k+10)mask
       CLOSE(k+10)
       scenemasks(:,:,k)=mask

       DEALLOCATE(mask)
    END DO
    !$OMP end parallel do

    ! Get the sum of acquisitions at each pixel
    ALLOCATE(sum_full(nr,naz))
    sum_full=0
    sum_full=sum(abs(scenemasks),dim=3)

    ! Summarize Potentially-Valid Pixels
    ALLOCATE(valid_full(nr,naz))
    valid_full = 0
    !$OMP parallel do private(j) shared(valid_full,sum_full)
    DO k=1,nr
       DO j=1,naz
          if (sum_full(k,j) >= 1) then
             valid_full(k,j) = 1
          end if
       END DO
    END DO
    !$OMP end parallel do

    nvalid_full = sum(valid_full(:,:))
    print *,'      Number of Potentially Valid Pixels Across All Scenes: ',nvalid_full

    
  !!!!! ----- FIND COVERAGE GROUPS ----- !!!!!

    ! Determine the number of valid pixels in each scene
    ALLOCATE(pixel_counts(nslc))
    !$OMP parallel do private(mask_count) shared(pixel_counts,scenemasks)
    DO k=1,nslc
       ALLOCATE(mask_count(nr,naz))
       mask_count = scenemasks(:,:,k)
       pixel_counts(k) = sum(abs(mask_count))
       DEALLOCATE(mask_count)
    END DO
    !$OMP end parallel do

    ! Determine the percentage of valid pixels, relative to the number of pixels
    ! covered by at least one scene
    ALLOCATE(prct_of_valid(nslc))
    prct_of_valid = real(pixel_counts)/real(nvalid_full)*100.0

    ! Bin the coverage by 0.5% windows
    dp = 0.5 !step size for percentage bins
    n = int((100.0 - dp) / dp) + 1 ! number of bins

    ALLOCATE(bins(n))
    ! Fill the bins with percentage
    DO k=1,n
       bins(k) = (k-1) * dp
    END DO
  
    ! Find the number of scenes that correspond to each percentage bin
    ALLOCATE(counts(n))
    counts=0
    DO k=1,nslc
       DO j=1,n
          IF (j/=n) THEN
             IF (prct_of_valid(k).ge.bins(j).and.prct_of_valid(k).lt.bins(j)+dp)THEN
                counts(j)=counts(j)+1
                exit
             END IF
          ELSE
             IF(prct_of_valid(k).ge.bins(j).and.prct_of_valid(k).le.bins(j)+dp)THEN
                counts(j)=counts(j)+1
             END IF
          END IF
       END DO
    END DO

    ! List Coverage Groups
    print*,''
    print*,'      VALID COVERAGE GROUPS'
    n_groups = 0
    DO k=1,n
        if (counts(k)>0.and.bins(k).ge.thresh) then
           print*,''
           print*,'      Percentage Bin: ',bins(k),' - ',bins(k)+dp
           print*,'        Number of SLCs: ',counts(k)
           n_groups = n_groups+1
        end if
    END DO
    print*,''
    print*,'    Number of groups: ',n_groups

    ! Capture Coverage Groups with valid coverage greater than input threshold
    ALLOCATE(bins_groups(n_groups))
    ALLOCATE(counts_groups(n_groups))
    counter=1
    DO k=1,n
       if (counts(k)>0.and.bins(k).ge.thresh) then
           bins_groups(counter) = bins(k)
           counts_groups(counter) = counts(k)
           counter=counter+1
       end if       
    END DO

    print*,bins_groups

  !!!!! ----- CREATE ARRAYS AND GEOLIST FOR CONSISTENT COVERAGE ----- !!!!!

    ! Find the percentage bin corresponding to the maximum number of SLCs
    nslc_consistent = maxval(counts)
    max_idx = maxloc(counts)
    idx_consistent = max_idx(1)

    print*,''
    print*,'      Consistent Coverage Group: ',bins(idx_consistent),' - ',bins(idx_consistent)+dp
    print*,'                 Number of SLCS: ',nslc_consistent

    ! Consistent Geolist
    geolist_consistent = 'geolist_consistent'
    ALLOCATE(geonames_consistent(nslc_consistent))
    ALLOCATE(index_consistent(nslc_consistent))
    counter=1
    DO k=1,nslc
       if (idx_consistent/=n) THEN
          if (prct_of_valid(k).ge.bins(idx_consistent).and.prct_of_valid(k).lt.bins(idx_consistent)+dp) then
             geonames_consistent(counter) = geonames(k)
             index_consistent(counter) = k
             counter=counter+1
          end if
       else
          if (prct_of_valid(k).ge.bins(n).and.prct_of_valid(k).le.bins(n)+dp) then
             geonames_consistent(counter) = geonames(k)
             index_consistent(counter) = k
             counter=counter+1
          end if
       end if
    END DO

   
    ! Consistent Summation and Mask
    ALLOCATE(sum_consistent(nr,naz))
    ALLOCATE(valid_consistent(nr,naz))
    
    sum_consistent = sum(abs(scenemasks(:,:,index_consistent)),dim=3)
    valid_consistent = 0
    !$OMP parallel do private(j) shared(sum_consistent,valid_consistent)
    DO k=1,nr
       DO j=1,naz
          if (sum_consistent(k,j) == nslc_consistent) then
             valid_consistent(k,j) = 1
          end if
       END DO
    END DO
    !$OMP end parallel do
 
  !!!!! ----- CREATE ARRAYS AND GEOLIST FOR CULLED COVERAGE ----- !!!!!

    ! Find the percentage bin corresponding to the first group with coverage >
    ! input threshold
    nslc_culled = sum(counts_groups)

    print*,''
    print*,'      Culled Coverage Minimum Valid: ',bins_groups(1)
    print*,'                     Number of SLCS: ',nslc_culled


    ! Culled Geolist
    geolist_culled = 'geolist_culled'
    ALLOCATE(geonames_culled(nslc_culled))
    ALLOCATE(index_culled(nslc_culled))
    counter=1
    DO k=1,nslc
       if (prct_of_valid(k).ge.bins_groups(1)) then
          geonames_culled(counter) = geonames(k)
          index_culled(counter) = k
          counter=counter+1
       end if
    END DO

    ! Culled Summation and Mask
    ALLOCATE(sum_culled(nr,naz))
    ALLOCATE(valid_culled(nr,naz))

    sum_culled = sum(abs(scenemasks(:,:,index_culled)),dim=3)
    valid_culled = 0
    !$OMP parallel do private(j) shared(sum_culled,valid_culled)
    DO k=1,nr
       DO j=1,naz
          if (sum_culled(k,j)>=2) then
             valid_culled(k,j) = 1
          end if
       END DO
    END DO
    !$OMP end parallel do

  !!!!! ----- MAKE THE OUTPUT MASKS ----- !!!!!
    ALLOCATE(mask_full(nr,naz),mask_consistent(nr,naz),mask_culled(nr,naz))
    !$OMP parallel do private(j) &
    !$OMP shared(valid_full,valid_consistent,valid_culled,mask_full,mask_consistent,mask_culled)
    DO k=1,nr
       DO j=1,naz
          if (valid_full(k,j)==1) then
             mask_full(k,j)=-1
          end if
          if (valid_consistent(k,j)==1) then
             mask_consistent(k,j)=-1
          end if
          if (valid_culled(k,j)==1) then
             mask_culled(k,j) = -1
          end if
       END DO
    END DO
    !$OMP end parallel do

  
  !!!!! ----- WRITE OUT GEOLISTS, SUMS, AND MASKS ----- !!!!!
    print *,''
    print *,'       Writing out files...'

    ! Geolists
    OPEN(28,FILE=geolist_culled,status='unknown',ACTION='write')
    DO k=1,nslc_culled
       WRITE(28,'(A)') trim(geonames_culled(k))
    END DO
    CLOSE(28)

    OPEN(28,FILE=geolist_consistent,status='unknown',ACTION='write')
    DO k=1,nslc_consistent
       WRITE(28,'(A)') trim(geonames_consistent(k))
    END DO
    CLOSE(28)

    ! Summation Arrays
    sum_full_out = 'num_scenes_full.out'
    sum_consistent_out = 'num_scenes_consistent.out'
    sum_culled_out = 'num_scenes_culled.out'

    OPEN(28,FILE=sum_full_out,FORM='unformatted',ACCESS='stream')
    WRITE(28)sum_full
    CLOSE(28)

    OPEN(28,FILE=sum_consistent_out,FORM='unformatted',ACCESS='stream')
    WRITE(28)sum_consistent
    CLOSE(28)

    OPEN(28,FILE=sum_culled_out,FORM='unformatted',ACCESS='stream')
    WRITE(28)sum_culled
    CLOSE(28)

    ! Masks
    scenemask_full_out = 'mask_full'
    scenemask_consistent_out = 'mask_consistent'
    scenemask_culled_out = 'mask_culled'

    OPEN(28,FILE=scenemask_full_out,FORM='unformatted',ACCESS='stream')
    WRITE(28)mask_full
    CLOSE(28)

    OPEN(28,FILE=scenemask_consistent_out,FORM='unformatted',ACCESS='stream')
    WRITE(28)mask_consistent
    CLOSE(28)

    OPEN(28,FILE=scenemask_culled_out,FORM='unformatted',ACCESS='stream')
    WRITE(28)mask_culled
    CLOSE(28)
    
    PRINT*,'Data written'


  !!!!! FUNCTIONS !!!!!

  CONTAINS

  ! ------------------ Replace_Text ----------------------- !
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


END PROGRAM determine_valid
