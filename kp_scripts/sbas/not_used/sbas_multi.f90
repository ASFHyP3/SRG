! Run SBAS on unique pixel groups
! Each has own set of files that are read in sequentially
! Incomplete velocity solutions are interpolated to the full time series only between the 
! First and last valid dates (i.e. no extrapolation)

! Compile with:
! gfortran -o sbas_multi sbas_multi.f90 svd.f90 -fopenmp -lrt -lpthread

PROGRAM sbas_multi
  use omp_lib

  IMPLICIT none

  !!!!! SPECIFICATIONS !!!!!
 
  ! INPUTS
  INTEGER:: ncells,nslc,nunwlists
  INTEGER*8::nr
  CHARACTER(100)::filelist,str,listofunwlists,ref_locs_file

  ! ERROR/STATUS CHECKS/COUNTING VARIABLES
  INTEGER::i,k,kk,counts,j,stat,ierr,stat2,ios
  INTEGER,DIMENSION(13)::statb

  ! READING IN INPUTS LISTS AND DEFINING OTHER VARIABLES
  INTEGER::n_refs
  INTEGER*8::naz
  INTEGER,DIMENSION(:,:),ALLOCATABLE::ref_locs
  CHARACTER(100),DIMENSION(:),ALLOCATABLE::unwnames,cells

  ! DATA ARRAYS FOR SET OF ALL INTERFEROGRAMS
  INTEGER*1,DIMENSION(:,:,:),ALLOCATABLE::masks
  REAL,DIMENSION(:,:,:),ALLOCATABLE::phase,amps

  ! ARRAYS FOR SUMMARIZING/REDUCING FULL DATA SET
  INTEGER,DIMENSION(:,:),ALLOCATABLE::npts,idxf,idxl
  REAL,DIMENSION(:),ALLOCATABLE::phase_ref,disp
  REAL,DIMENSION(:,:),ALLOCATABLE::amp,stacktime,stack

  ! FOR LOADING FULL TIME SERIES INFORMATION (ARRAYS)
  REAL,DIMENSION(:),ALLOCATABLE::timedeltas,cumsumf
  REAL,DIMENSION(:,:),ALLOCATABLE::deltime
  CHARACTER(100),DIMENSION(:),ALLOCATABLE::geolist

  ! DATE VARIABLES
  CHARACTER(8)::date1,date2
  INTEGER :: year1, month1, day1
  INTEGER :: year2, month2, day2
  INTEGER :: days1, days2, days_between

  ! LOADING IN THE UNWRAPPED INTERFEROGRAMS, AMPLITUDES, AND MASKS; FIND REFERENCE PHASE AT EACH INTERFEROGRAM
  INTEGER*1,DIMENSION(:,:),ALLOCATABLE::mask
  CHARACTER(100)::strint,stramp,strmask
  REAL,DIMENSION(:,:),ALLOCATABLE::dat

  ! FULL SOLUTION
  REAL,DIMENSION(:,:,:),ALLOCATABLE::velocity

  ! SPECIFIC TO EACH PIXEL GROUP
  CHARACTER(100),DIMENSION(:),ALLOCATABLE::unwlist,geolist_temp
  CHARACTER(100)::geolist_name,Tmstr,timedeltas_file,pixel_file
  INTEGER::nunw_temp,nvalid,npix,ndays_temp,tempday,idx
  INTEGER,DIMENSION(:,:),ALLOCATABLE::pixels
  REAL,DIMENSION(:),ALLOCATABLE::timedeltas_temp,cumsumt,deltime_temp,temp1
  REAL,DIMENSION(:,:),ALLOCATABLE::Tm,Tminv,phase_temp,amp_temp,vel_temp


  !!!!! EXECUTIONS !!!!!
  ! check to see if proper number of arguments were provided
  IF(iargc().lt.6)THEN
     WRITE(*,*)'usage: sbas_multi filelist nunwfiles nslcs len listofunwlists nunwlists <ref_locations_file = x/2,y/2>)'
     STOP
  END IF

  ! read in arguments and convert to appropriate type (where necessary)
    CALL getarg(1,filelist)
    CALL getarg(2,str)
    READ(str,*)ncells
    CALL getarg(3,str)
    READ(str,*)nslc
    CALL getarg(4,str)
    READ(str,*)nr
    CALL getarg(5,listofunwlists)
    CALL getarg(6,str)
    READ(str,*)nunwlists

  ! allocate the list of unwrapped files and the reference pixel locations array
    ALLOCATE(cells(ncells),ref_locs(2,10000000))
    ALLOCATE(unwnames(nunwlists))

  ! read in the list of unwrapped files
    OPEN(UNIT=11,FILE=filelist,STATUS='old')
    READ(11,'(A)',end=10,IOSTAT=stat)cells
  10 continue
    CLOSE(11)

  ! Read in the first unwrapped interferogram name in order to determine the number of azimuth lines, given the known number of range lines
    OPEN(UNIT=21,FILE=trim(adjustl(cells(1))),FORM='unformatted',ACCESS='direct',RECL=nr*8)
    ierr=fstat(21,statb)
    naz=statb(8)/8/nr
    WRITE(*,*)' Lines in file: ',naz
    CLOSE(21)

  ! Read in the list of unwlists
    OPEN(unit=11,FILE=listofunwlists,STATUS='old')
    READ(11,'(A)',end=110,IOSTAT=stat2)unwnames
  110 continue
    CLOSE(11)


  ! Set list of reference points, default to scene center if no input file is given
    n_refs=1
    ref_locs(1,1)=nr/2
    ref_locs(2,1)=naz/2
    if(iargc().ge.7)then
        CALL getarg(7,str)
        READ(str,*)ref_locs_file
        open(unit=13,file=ref_locs_file)
        do i=1,10000000
           read(13,*,end=99)ref_locs(:,i)
        end do
   99   n_refs=i-1
        CLOSE(13)
    end if
    print *,'Number of reference points: ',n_refs

  ! Now that we know some additional parameters, allocate some of the arrays we will need
    ALLOCATE(phase(nr,naz,ncells),amps(nr,naz,ncells),masks(nr,naz,ncells))
    ALLOCATE(phase_ref(ncells), disp(nr))

  ! Set values in arrays equal to zero for now
    phase(:,:,:)=0.
    phase_ref = 0.
    amps=0.
    masks(:,:,:)=0
    

  !!!!! READ IN THE SBAS PARAMETERS NEEDED FOR FULL TIME SERIES !!!!!

  ! Create an array with data from deltime.out (currently hard-coded)
    ALLOCATE(deltime(4,ncells))
    OPEN(13,FILE='deltime.out',STATUS='old')
    DO i=1,ncells
       READ(13,*,IOSTAT=stat)deltime(:,i)
    END DO
    CLOSE(13)

  ! Read time from slc to slc (i.e. temporal baseline between nearest neighbor scenes
    ALLOCATE(timedeltas(nslc-1))
    open(13,file='timedeltas.out',status='old')
    read(13,*)timedeltas
    close(13)

  ! Make a cumulative sum of the timedeltas array for use during SBAS
    ALLOCATE(cumsumf(nslc-1))
    do i=1,nslc-1
        cumsumf(i) = sum(timedeltas(1:i))
    end do

  ! Read in Geolist to get the first date of the time series
    ALLOCATE(geolist(nslc))
    open(11,file='geolist',status='old')
    read(11,'(A)',end=112,IOSTAT=stat2)geolist
  112  continue
    close(11)

  ! Date of first SLC acquisition
    date1 = trim(adjustl(geolist(1)))
    READ(date1(1:4),*)year1
    READ(date1(5:6),*)month1
    READ(date1(7:8),*)day1

  ! Convert dates to days since a reference date (e.g., 1 January 1900)
    days1 = date_to_days(year1, month1, day1)


  ! read in unwrapped igrams using parallel computing
    !$OMP parallel do private(k,kk,dat,strint,stramp,strmask,mask,counts) &
    !$OMP shared(cells,ref_locs,deltime,nr,naz,ncells,phase,amps,phase_ref,masks)
    DO i=1,ncells
       ! allocate dat, which appears to be a “temporary” variable holding the unwrapped data
       allocate(dat(nr*2,naz))
       allocate(mask(nr,naz))

       ! get the correct names corresponding to the files that will be read in
       strint=trim(adjustl(cells(i)))
       stramp=Replace_Text(strint,'.unw','.amp')
       strmask=Replace_Text(strint,'.unw','.mask')

       ! open theinterferogram file, in binary format, directly. Requires conversion for reading.
       OPEN(UNIT=i+10,FILE=strint,FORM='unformatted',ACCESS='direct',RECL=2*nr*naz*4,CONVERT= 'LITTLE_ENDIAN')
       READ(i+10,rec=1)dat
       CLOSE(i+10)
       ! read the correct values from dat to get phase, and put in phase array
       phase(:,:,i)=dat((nr+1):2*nr,:)

       ! Read the corresponding amplitude file, same way as the unwrapped file
       OPEN(UNIT=i+10,FILE=stramp,FORM='unformatted',ACCESS='direct',RECL=2*nr*naz*4,CONVERT= 'LITTLE_ENDIAN')
       READ(i+10,rec=1)dat
       CLOSE(i+10)
       ! read the correct values from dat to get the amplitude powers, put in amps array
       amps(:,:,i)=dat(1:2*nr-1:2,:)**2+dat(2:2*nr:2,:)**2
       
       ! read the corresponding byte mask file
       open(unit=i+10,file=strmask,access='stream')
       read(i+10)mask
       CLOSE(i+10)
       masks(:,:,i)=mask
 
       ! Free up memory with deallocation
       deallocate(dat)
       deallocate(mask)

       ! compute the reference phase for ith interferogram (by taking average of all reference pixels)
       phase_ref(i)=0.
       counts = 0
       do k=1,n_refs
           if(masks(ref_locs(1,k),ref_locs(2,k),i).ne.0)then
              phase_ref(i)=phase_ref(i)+phase(ref_locs(1,k),ref_locs(2,k),i)
              counts=counts+1
           end if
       end do

       ! Calculate the number of valid reference pixels available and either divide the summed reference phase by that value or,
       ! if there were no valid pixels, take the average of the valid pixels as the reference phase.
       if (counts /= 0) then
           phase_ref(i)=phase_ref(i)/real(counts)
       else
           do k=1,nr
               do kk=1,naz
                   if (masks(k,kk,i) /= 0) then
                       phase_ref(i)=phase_ref(i) + phase(k,kk,i)
                       counts=counts+1
                   end if
               end do
           end do
           if (counts /= 0) then
               phase_ref(i)=phase_ref(i)/real(counts)
           end if
       end if
       ! if count still equals zero, then there are no valid pixels in this interferogram, and phase_ref(i) can be left 0

       ! subtract reference phase from each interferogram
       phase(:,:,i)=phase(:,:,i)-phase_ref(i)

    END DO
    !$OMP end parallel do

    ! adjust the amplitudes that were read in, since they were in powers, not amplitude.
    amps=sqrt(amps)

    PRINT*,'Data Loaded'

    !!!!!! SBAS LEAST SQUARES AND STACKING !!!!!!!
    ! at each pixel, solve for velocity at (nslc-1) time interval
    ! uses internal subroutine pinv (which relies on external fortran function svd.f90)
    ! uses built-in fortran intrinsic function matmul

    ! Allocate velocity solution matrix, stack, stack parameters, and first/last indices
    ALLOCATE(velocity(nr,naz,nslc-1))
    ALLOCATE(amp(nr,naz),stacktime(nr,naz),stack(nr,naz),npts(nr,naz),idxf(nr,naz),idxl(nr,naz))

    velocity(:,:,:)=-99999.
    amp(:,:)=0.
    stacktime=0.
    stack=0.
    npts = 0
    idxf = -99999
    idxl = -99999


    do i=1,nunwlists
        
        PRINT*,'Working on ',trim(adjustl(unwnames(i)))

        ! Read in the unwlist from unwnames(i) using subroutine read_list
        call read_list(trim(adjustl(unwnames(i))), unwlist, nunw_temp)
        
        PRINT*,'    ','unwlist read: ',nunw_temp
        if (nunw_temp == 0) then
            PRINT*,'    ','No Unwrapped File in Group, skipping...'
            cycle
        end if
        ! Read in the corresponding geolist to get number of valid slcs using subroutine read_list
        geolist_name = Replace_Text(trim(adjustl(unwnames(i))),'unwlist','geolist')
        call read_list(geolist_name, geolist_temp, nvalid)
        
        PRINT*,'    ','geolist read: ',nvalid

        ! Read in the pixel list similar to reading in the reference pixel file (I think)
        pixel_file = Replace_Text(trim(adjustl(unwnames(i))),'unwlist','pixels')
        PRINT*,'    ',pixel_file
       
        open(unit=66,file=pixel_file,status='OLD',action='READ')
        npix=0
        DO
            READ(66,'(A)',IOSTAT=ios)str
            if (ios /= 0) EXIT
            npix = npix+1
        END DO

        ALLOCATE(pixels(2,npix))
        REWIND 66

        do k=1,npix
            read(66,*,IOSTAT=ios)pixels(1,k),pixels(2,k)
            IF (ios /= 0) EXIT
        end do
        close(66)

        PRINT*,'    ','Pixel list read in: ',npix

        ! Create an array from 'Tm.out' (currently hard-coded)
        ALLOCATE(Tm(nunw_temp,nvalid-1))
        Tmstr = Replace_text(trim(adjustl(unwnames(i))),'unwlist','Tm')
        Tmstr = TRIM(Tmstr) // '.out'
        PRINT*,'    ',Tmstr
        OPEN(24,FILE=Tmstr,STATUS='old')
        do k=1,nunw_temp
            READ(24,*,IOSTAT=stat)(Tm(k,kk),kk=1,nvalid-1)
        end do
        CLOSE(24)

        PRINT*,'    ','Tm.out file read in'

        ! Read in the appropriate timedeltas array
        ALLOCATE(timedeltas_temp(nvalid-1))
        timedeltas_file = Replace_Text(Tmstr,'Tm','timedeltas')
        open(29,file=timedeltas_file,status='old')
        read(29,*)timedeltas_temp
        close(29)

        PRINT*,'    ','timedeltas file read in'

        !!!!! Allocate and calculate other arrays unique to the pixel group

        ! Calculate Tminv with internal subroutine pinv
        ALLOCATE(Tminv(nvalid-1,nunw_temp))
        call pinv(Tm,nunw_temp,nvalid-1,Tminv)
        DEALLOCATE(Tm)

        PRINT*,'    Tm inverted to Tminv'

        ! Make a cumulative sum of the timedeltas array in order to allocate the sub-velocity solution to the full
        ALLOCATE(cumsumt(nvalid-1))
        do k=1,nvalid-1
            cumsumt(k) = sum(timedeltas_temp(1:k))
        end do
 
        DEALLOCATE(timedeltas_temp)
        PRINT*,'    cumulative sum of timdeltas calculated'

        ! Date of first SLC acquisition
        date2 = trim(adjustl(geolist_temp(1)))
        READ(date2(1:4),*)year2
        READ(date2(5:6),*)month2
        READ(date2(7:8),*)day2

        ! Convert dates to days since a reference date (e.g., 1 January 1900)
        days2 = date_to_days(year2, month2, day2)

        ! Calculate the difference
        days_between = days2 - days1

        ! Adjust the cumsumt array to account for the time difference between the true first acquisition and this one
        cumsumt = cumsumt + days_between

        ! Find the indices of the first and last valid dates, to avoid extrapolation
        do k=1,nslc
            if (geolist (k) == geolist_temp(1)) then
                do kk=1,npix
                    idxf(pixels(1,kk),pixels(2,kk)) = k-1
                end do
            end if
            if (geolist (k) == geolist_temp(nvalid)) then
                do kk=1,npix
                    idxl(pixels(1,kk),pixels(2,kk)) = k-1
                end do
            end if
        end do

        ! Find the index of the unwrapped interferogram that is in the unwlist and extract phase and amplitude
        ALLOCATE(phase_temp(npix,nunw_temp),amp_temp(npix,nunw_temp))
        ALLOCATE(deltime_temp(nunw_temp))
        
        !$OMP parallel do private(kk,idx) shared(cells,unwlist,deltime,deltime_temp,phase,amps,pixels,phase_temp,amp_temp)
        do k=1,nunw_temp
            idx=-1
            do kk=1,ncells
                if (trim(adjustl(cells(kk))) == trim(adjustl(unwlist(k)))) then
                    idx = kk
                    exit
                end if
            end do

            if (idx /= -1) then
                deltime_temp(k) = deltime(2,idx)
                do kk=1,npix
                    phase_temp(kk,k) = phase(pixels(1,kk),pixels(2,kk),idx)
                    amp_temp(kk,k) = amps(pixels(1,kk),pixels(2,kk),idx)
                end do
            end if
        end do
        !$OMP end parallel do
        PRINT*,'    Phase and amplitude at each pixel in group extracted'

        ! Calculate the number of days across all valid interferograms
        ndays_temp = sum(deltime_temp)

        ! Run SBAS on each pixel and calculate stacks & stack parameters
        ALLOCATE(vel_temp(npix,nvalid-1))
        vel_temp=0

        ALLOCATE(temp1(nunw_temp))
        
        PRINT*,'    Calculating Velocity and Stack'
        !$OMP parallel do private(temp1) shared(npix,phase_temp,pixels,vel_temp,nunw_temp,ndays_temp,Tminv,npts,stacktime,amp,stack)
        do kk=1,npix
            temp1=phase_temp(kk,:)
            temp1=temp1(:)
            vel_temp(kk,:)=MATMUL(Tminv,temp1)

            npts(pixels(1,kk),pixels(2,kk)) = nunw_temp
            stacktime(pixels(1,kk),pixels(2,kk)) = ndays_temp

            if (nunw_temp /= 0) then
                amp(pixels(1,kk),pixels(2,kk)) = sum(amp_temp(kk,:))/nunw_temp
            end if

            if (ndays_temp /= 0) then
                stack(pixels(1,kk),pixels(2,kk)) = sum(phase_temp(kk,:))/ndays_temp
            end if

        end do
        !$OMP end parallel do

        DEALLOCATE(temp1)
        DEALLOCATE(Tminv)
        DEALLOCATE(phase_temp)
        DEALLOCATE(amp_temp)
        DEALLOCATE(deltime_temp)


        ! Fill in the appropriate velocity values in the full solution
        PRINT*,'    Interpolating velocity solution to full time series'
        if (idxf(pixels(1,1),pixels(2,1)) /= -99999)then
            tempday = 0
            if (idxf(pixels(1,1),pixels(2,1))>0) then
                tempday = cumsumt(idxf(pixels(1,1),pixels(2,1)))
            end if
            do k=1,nvalid-1
                do j = k,nslc-1
                    if (cumsumf(j).le.cumsumt(k).and.cumsumf(j)>tempday)then
                        do kk=1,npix
                            velocity(pixels(1,kk),pixels(2,kk),j) = vel_temp(kk,k)
                        end do
                    end if
                    if (cumsumf(j)==cumsumt(k))then
                        exit
                    end if
                end do
                tempday=cumsumt(k)
            end do
        end if

        DEALLOCATE(vel_temp)
        DEALLOCATE(pixels)
        DEALLOCATE(cumsumt)

    end do
    
    !!!!! WRITE RESULTS TO FILES !!!!!
    print *,'Writing velocity and displacement solutions'
    OPEN(28,FILE='velocity',FORM='unformatted',ACCESS='stream')
    WRITE(28)velocity
    close(28)

    OPEN(29,FILE='displacement',FORM='unformatted',ACCESS='direct',RECL=nr*8)

    ! integrate velocities for displacement with parallelization of outer loop (each time step)
    !$OMP parallel do private(j,k,disp) shared(nslc,naz,nr,velocity,timedeltas,amp)
    do i=1,nslc-1
       do j=1,naz
          disp=-99999
          do k=1,nr
              if (idxf(k,j) /= -99999)then
                  if (i.le.idxl(k,j).and.idxf(k,j)+1.le.i) then
                      disp(k)=sum(velocity(k,j,idxf(k,j)+1:i)*timedeltas(idxf(k,j)+1:i))
                  end if
              end if
          end do

          ! write out average amplitude and displacement for each azimuth line and time step pair
          write(29,rec=j+(i-1)*naz)amp(:,j),disp
       end do
    end do
    !$OMP end parallel do
    close(29)

    !!!!!! write stack files for further reference !!!!!!
    print *,'Writing stacks'
    ! Write out file for the number of valid unwrapped files for each pixel
    OPEN(28,FILE='npts',FORM='unformatted',ACCESS='stream')
    WRITE(28)npts
    close(28)

    ! Write out file for the total number of days spanned by the valid interferograms for each pixel
    OPEN(28,FILE='stacktime',FORM='unformatted',ACCESS='stream')
    WRITE(28)stacktime
    close(28)

    ! Write out the file describing the indices of the first valid date at each pixel. 0=t0
    OPEN(28,FILE='index_first',FORM='unformatted',ACCESS='stream')
    WRITE(28)idxf
    close(28)

    ! Write out the file describing the indices of the last valid date at each pixel. 0=t0
    OPEN(28,FILE='index_last',FORM='unformatted',ACCESS='stream')
    WRITE(28)idxl
    close(28)


    ! Write weighted amps and phases stack into stackmht, phases in rad/day (pixel-interleaved format?)
    OPEN(28,FILE='stackmht',FORM='unformatted',ACCESS='direct',RECL=nr*8)
    do i=1,naz
       WRITE(28,rec=i)amp(:,i),stack(:,i)
    end do
    CLOSE(28)

    ! Save nr, naz, nslc, ncells parameters in one file
    OPEN(28,FILE='parameters',STATUS='replace')
    WRITE(28,*) nr, naz, nslc, ncells
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


  !  ----------------------- pinv --------------------------- !
  subroutine pinv(mat,rows,cols,matinv)
    IMPLICIT none

    !specifications
    INTEGER,INTENT(IN)::rows,cols
    INTEGER :: ierr,kk,r,c
    REAL,DIMENSION(rows,cols),INTENT(IN)::mat
    REAL,DIMENSION(cols,rows)::matinv
    REAL*8,DIMENSION(:),allocatable::Sing
    REAL*8,DIMENSION(:,:),allocatable::Sinv,dmat,U,V,X,S

    allocate (dmat(rows,cols),Sinv(cols,cols))
    allocate (U(rows,cols),V(cols,cols),Sing(cols),S(cols,cols))

    !executions
    do r=1,rows
       do c=1,cols
          dmat(r,c)=mat(r,c)
          !print *,r,c,mat(r,c),dmat(r,c)
       end do
    end do

    ! Call routine svd, which was compiled with the sbas_multi.f90 function
    call svd(rows,cols,dmat,Sing,.true.,U,.true.,V,ierr)

    S=0.
    do c=1,cols
       S(c,c)=Sing(c)
    end do

    Sinv=0.
    DO r=1,cols
        if(abs(S(r,r)).gt.1.e-6)Sinv(r,r)=1./S(r,r)
    END DO

    ! Not sure where the sngl is coming from
    matinv(:,:)=sngl(MATMUL(MATMUL(V,Sinv),TRANSPOSE(U)))

    deallocate (dmat)
    deallocate (V)
    deallocate (Sing)
    deallocate (Sinv)
    deallocate (U)
  END subroutine pinv


  ! -------------------------- read_list ----------------------------- !
  subroutine read_list(filename, string_array, num_lines)
      implicit none

      ! Input arguments
      character(len=*), intent(in) :: filename
      ! Output arguments
      character(len=*), dimension(:), allocatable, intent(out) :: string_array
      integer, intent(out) :: num_lines

      integer :: i, ios, max_lines
      character(len=100) :: line
      integer, parameter :: default_max_lines = 10000

      ! Temporary variables
      integer :: line_count
      character(len=100), dimension(default_max_lines) :: temp_array

      ! Open the file for reading
      open(unit=23, file=filename, status='old', action='read', iostat=ios)
      if (ios /= 0) then
          print *, 'Error opening file: ', filename
          stop
      end if

      ! Initialize line counter
      line_count = 0

      ! Count lines
      do
          read(23, '(A)', iostat=ios) line
          if (ios /= 0) exit
          line_count = line_count + 1
      end do

      ! Close the file after counting
      close(23)

      ! Allocate array based on the number of lines
      allocate(string_array(line_count))

      ! Reopen the file for reading strings into array
      open(unit=23, file=filename, status='old', action='read', iostat=ios)
      if (ios /= 0) then
          print *, 'Error reopening file: ', filename
          stop
      end if

      ! Read lines into the array
      do i = 1, line_count
          read(23, '(A)', iostat=ios) string_array(i)
          if (ios /= 0) then
              print *, 'Error reading line ', i
              stop
          end if
      end do

      ! Close the file after reading
      close(23)

      ! Set the output argument
      num_lines = line_count

  end subroutine read_list

  ! ------------------ date_to_days ------------------------- !
  ! Function to convert a date to days since 1 January 1900
  function date_to_days(year, month, day) result(days)
      integer, intent(in) :: year, month, day
      integer :: days, i

      ! Reference date: 1 January 1900
      integer, parameter :: ref_year = 1900
      integer, parameter :: ref_month = 1
      integer, parameter :: ref_day = 1

      ! Days from reference date
      days = 0

      ! Add days for full years
      do i = ref_year, year - 1
          days = days + 365
          if (mod(i, 4) == 0 .and. (mod(i, 100) /= 0 .or. mod(i, 400) == 0)) then
              days = days + 1
          end if
      end do

      ! Add days for full months of current year
      do i = 1, month - 1
          days = days + days_in_month(year, i)
      end do

      ! Add days for current month
      days = days + day - ref_day

  end function date_to_days

  ! ----------------------- days_in_month ------------------- !
  ! Function to return the number of days in a given month of a given year
  function days_in_month(year, month) result(days)
      integer, intent(in) :: year, month
      integer :: days

      select case (month)
      case (1, 3, 5, 7, 8, 10, 12)
          days = 31
      case (4, 6, 9, 11)
          days = 30
      case (2)
          if (mod(year, 4) == 0 .and. (mod(year, 100) /= 0 .or. mod(year, 400) == 0)) then
              days = 29
          else
              days = 28
          end if
      case default
          days = 0
      end select

  end function days_in_month

END PROGRAM sbas_multi
