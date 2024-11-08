! Run SBAS on unique pixel groups
! Each pixel has a vector describing whether or not that interferogram should be
! included in the analysis.
! Optional: load in an array describing the maximum temporal baseline to use at
! each pixel. 
! Incomplete velocity solutions are interpolated prior to integration to
! displacement. No velocity extrapolation (filled with NaN values, or -99999)


! Compile with:
! gfortran -o sbas_unique sbas_unique.f90 svd.f90 -fopenmp -lrt -lpthread

PROGRAM sbas_unique
  use omp_lib

  IMPLICIT none

  !!!!! SPECIFICATIONS !!!!!
 
  ! INPUTS
  INTEGER:: ncells,nslc
  INTEGER*8::nr
  CHARACTER(100)::filelist,str,Tmfile,timedeltafile,deltimefile,reflocs_file,scenemaskfile,maxTBfile
  CHARACTER(4)::intmaskflag

  ! ERROR/STATUS CHECKS/COUNTING VARIABLES
  INTEGER::i,k,kk,counts,j,stat,ierr,stat2,ios
  INTEGER,DIMENSION(13)::statb

  ! READING IN INPUTS LISTS AND DEFINING OTHER VARIABLES
  INTEGER::n_refs
  INTEGER*8::naz,x,y
  INTEGER,DIMENSION(:,:),ALLOCATABLE::ref_locs
  CHARACTER(100),DIMENSION(:),ALLOCATABLE::cells

  ! DATA ARRAYS FOR SET OF ALL INTERFEROGRAMS
  INTEGER*1,DIMENSION(:,:,:),ALLOCATABLE::masks
  INTEGER*1,DIMENSION(:,:),ALLOCATABLE::scenemask
  REAL,DIMENSION(:,:,:),ALLOCATABLE::phase,amps
  INTEGER,DIMENSION(:,:),ALLOCATABLE::maxTB

  ! ARRAYS FOR SUMMARIZING/REDUCING FULL DATA SET
  INTEGER,DIMENSION(:,:),ALLOCATABLE::npts,idxf,idxl
  REAL,DIMENSION(:),ALLOCATABLE::phase_ref,disp
  REAL,DIMENSION(:,:),ALLOCATABLE::amp,stacktime,stack

  ! FOR LOADING FULL TIME SERIES INFORMATION (ARRAYS)
  REAL,DIMENSION(:),ALLOCATABLE::timedeltas
  REAL,DIMENSION(:,:),ALLOCATABLE::deltime

  ! LOADING IN THE UNWRAPPED INTERFEROGRAMS, AMPLITUDES, AND MASKS; FIND REFERENCE PHASE AT EACH INTERFEROGRAM
  INTEGER*1,DIMENSION(:,:),ALLOCATABLE::mask
  CHARACTER(100)::strint,stramp,strmask
  REAL,DIMENSION(:,:),ALLOCATABLE::dat

  ! FULL SOLUTION
  REAL,DIMENSION(:,:,:),ALLOCATABLE::velocity,velocity_interp

  ! PIXEL-WISE SBAS
  REAL,DIMENSION(:),ALLOCATABLE::v_in,v_out,phase_temp,amp_temp,deltime_temp
  REAL,DIMENSION(:,:),ALLOCATABLE::Tm,Tminv,Tm_temp,Tminv_temp
  INTEGER,DIMENSION(:),ALLOCATABLE::mask_vector
  INTEGER::nrows,ndays,idx
  


  !!!!! EXECUTIONS !!!!!
  ! check to see if proper number of arguments were provided
  IF(iargc().lt.7)THEN
     WRITE(*,*)'usage: sbas_unique intlist nints nslc nr Tmfile timedeltafile' // &
           ' deltimefile <reflocs_file=none> <scenemakfile=none>' // &
           ' <intmaskflag=N> <maxTBfile=none>'
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
    CALL getarg(5,Tmfile)
    CALL getarg(6,timedeltafile)
    CALL getarg(7,deltimefile)
    
  ! Set Default parameters and check for inputs
    reflocs_file = 'none'
    IF(iargc().ge.8)THEN
       CALL getarg(8,reflocs_file)
    END IF

    scenemaskfile = 'none'
    IF(iargc().ge.9)THEN
       CALL getarg(9,scenemaskfile)
    END IF

    intmaskflag = 'N'
    IF(iargc().ge.10)THEN
       CALL getarg(10,intmaskflag)
    END IF  

    maxTBfile='none'
    IF(iargc().ge.11)THEN
       CALL getarg(11,maxTBfile)
    END IF


  ! allocate the list of unwrapped files and the reference pixel locations array
    ALLOCATE(cells(ncells),ref_locs(2,10000000))

  ! read in the list of unwrapped interferogram files
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


  ! Set list of reference points, default to scene center if no input file is given
    n_refs=1
    ref_locs(1,1)=nr/2
    ref_locs(2,1)=naz/2
    if(reflocs_file.ne.'none')then
        open(unit=13,file=reflocs_file)
        do i=1,10000000
           read(13,*,end=99)ref_locs(:,i)
        end do
   99   n_refs=i-1
        CLOSE(13)
    end if
    print *,'Number of eligible reference points: ',n_refs


  ! Read in scenemask if available
    ALLOCATE(scenemask(nr,naz))
    scenemask=1
    IF(scenemaskfile.ne.'none')THEN
       open(unit=13,file=scenemaskfile,access='stream')
       read(13)scenemask
       CLOSE(13)
    END IF

  ! Read in the maxTBfile if available
    ALLOCATE(maxTB(nr,naz))
    maxTB=10000
    IF(maxTBfile.ne.'none')THEN
       open(unit=13,file=maxTBfile,access='stream')
       read(13)maxTB
       CLOSE(13)
    END IF



  ! Now that we know some additional parameters, allocate some of the arrays we will need
    ALLOCATE(phase(nr,naz,ncells),amps(nr,naz,ncells),masks(nr,naz,ncells))
    ALLOCATE(phase_ref(ncells))

  ! Set values in arrays equal to zero for now
    phase(:,:,:)=0.
    phase_ref = 0.
    amps=0.
    masks(:,:,:)=0
    

  !!!!! READ IN THE SBAS PARAMETERS NEEDED FOR FULL TIME SERIES !!!!!

  ! Load in the model matrix Tm
    ALLOCATE(Tm(ncells,nslc-1))
    OPEN(24,FILE=Tmfile,STATUS='old')
    do k=1,ncells
       READ(24,*,IOSTAT=stat)(Tm(k,kk),kk=1,nslc-1)
    end do
    CLOSE(24)


  ! Create an array with data from deltime.out (currently hard-coded)
    ALLOCATE(deltime(4,ncells))
    OPEN(13,FILE=deltimefile,STATUS='old')
    DO i=1,ncells
       READ(13,*,IOSTAT=stat)deltime(:,i)
    END DO
    CLOSE(13)

  ! Read time from slc to slc (i.e. temporal baseline between nearest neighbor scenes
    ALLOCATE(timedeltas(nslc-1))
    open(13,file=timedeltafile,status='old')
    read(13,*)timedeltas
    close(13)



  ! read in unwrapped igrams using parallel computing and calculate the
  ! reference phase for each interferogram, utilizing masks and maxTB, if
  ! available (declared in inputs)
    !$OMP parallel do private(k,kk,dat,strint,stramp,strmask,mask,counts,x,y) &
    !$OMP shared(cells,ref_locs,deltime,nr,naz,ncells,phase,amps,phase_ref,masks,scenemask,maxTB)
    DO i=1,ncells
       ! allocate dat, which appears to be a “temporary” variable holding the unwrapped data
       allocate(dat(nr*2,naz))
       IF(intmaskflag=='Y')THEN
          allocate(mask(nr,naz))
       END IF

       ! get the correct names corresponding to the files that will be read in
       strint=trim(adjustl(cells(i)))
       stramp=Replace_Text(strint,'.unw','.amp')

       ! open the interferogram file  directly. Requires conversion for reading.
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
       
       IF(intmaskflag=='Y')THEN
          strmask=Replace_Text(strint,'.unw','.mask')
          ! read the corresponding byte mask file
          open(unit=i+10,file=strmask,access='stream')
          read(i+10)mask
          CLOSE(i+10)
          masks(:,:,i)=mask
          deallocate(mask)
       END IF

       ! Free up memory with deallocation
       deallocate(dat)

       ! compute the reference phase for ith interferogram (by taking average of valid reference pixels)
       phase_ref(i)=0.
       counts = 0
       do k=1,n_refs
           x = ref_locs(1,k)
           y = ref_locs(2,k)
           IF(deltime(i,2).le.maxTB(x,y))THEN
              IF(intmaskflag=='Y')THEN
                 if(masks(x,y,i).ne.0)then
                    phase_ref(i)=phase_ref(i)+phase(x,y,i)
                    counts=counts+1
                 end if
              ELSE IF(scenemaskfile.ne.'none')THEN
                 if(scenemask(x,y).ne.0)then
                    phase_ref(i)=phase_ref(i)+phase(x,y,i)
                    counts=counts+1
                 end if
              ELSE
                 phase_ref(i)=phase_ref(i)+phase(x,y,i)
                 counts=counts+1
              END IF
           END IF
       end do

       ! Calculate the number of valid reference pixels available for the interferogram  and either divide the summed reference phase by that value or,
       ! if there were no valid pixels, take the average of the valid pixels as the reference phase.
       if (counts /= 0) then
           phase_ref(i)=phase_ref(i)/real(counts)
       else
           do k=1,nr
               do kk=1,naz
                   IF (deltime(i,2).le.maxTB(k,kk)) THEN
                      IF (intmaskflag=='Y')THEN
                         if (masks(k,kk,i).ne.0) then
                            phase_ref(i)=phase_ref(i) + phase(k,kk,i)
                            counts=counts+1
                         end if
                      ELSE IF (scenemaskfile.ne.'none') THEN
                         if (scenemask(k,kk).ne.0)then
                            phase_ref(i)=phase_ref(i) + phase(k,kk,i)
                            counts=counts+1
                         end if
                      ELSE
                         phase_ref(i)=phase_ref(i) + phase(k,kk,i)
                         counts=counts+1
                      END IF
                   END IF
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
    ALLOCATE(velocity(nr,naz,nslc-1),velocity_interp(nr,naz,nslc-1),Tminv(nslc-1,ncells))
    ALLOCATE(amp(nr,naz),stacktime(nr,naz),stack(nr,naz),npts(nr,naz),idxf(nr,naz),idxl(nr,naz))

    velocity(:,:,:)=0.0
    velocity_interp(:,:,:) = 0.0
    amp(:,:)=0.
    stacktime=0.
    stack=-99999.
    npts = 0
    idxf = -99999
    idxl = -99999

    ! Calculate the full Tminv so that you don't need to calculate it every
    ! pixel unless you need to 
    call pinv(Tm,ncells,nslc-1,Tminv)


    ! Run through each pixel, find the phase set that should be used, cull the
    ! rows from the phase vector and Tm, invert for Tminv, and then solve for
    ! preliminary velocity solution via matrix multiplication
    ALLOCATE(mask_vector(ncells))
    ALLOCATE(v_in(nslc-1),v_out(nslc-1))

    !$OMP parallel do collapse(2) private(mask_vector,nrows,phase_temp,amp_temp,deltime_temp) &
    !$OMP private(Tm_temp,idx,ndays,Tminv_temp,k,v_in,v_out) &
    !$OMP shared(scenemask,masks,maxTB,phase,Tm,amps,deltime,npts,stacktime,amp,stack) &
    !$OMP shared(velocity,velocity_interp,idxf,idxl,timedeltas)
    DO i=1,nr
       DO j=1,naz
          if(scenemask(i,j)==0)THEN
             cycle
          end if

          ! Find the appropriate mask vector so we can determine which rows to
          ! remove
          mask_vector = 1
          IF(intmaskflag=='Y')THEN
             mask_vector = masks(i,j,:)
          END IF
          IF(maxTBfile.ne.'none')THEN
             DO k=1,ncells
                if(deltime(k,2).gt.maxTB(i,j))THEN
                   mask_vector(k)=0
                end if
             END DO
          END IF


          ! create the temporary phase vector and Tm matrix
          nrows = sum(real(mask_vector))
          if (nrows==0)THEN
             cycle
          end if


          ALLOCATE(phase_temp(nrows),amp_temp(nrows),deltime_temp(nrows))
          if(nrows.ne.ncells)then
             idx = 1
             ALLOCATE(Tm_temp(nrows,nslc-1))
             DO k=1,ncells
                if(mask_vector(k).ne.0)THEN
                   phase_temp(idx) = phase(i,j,k)
                   Tm_temp(idx,:) = Tm(k,:)
                   amp_temp(idx) = amps(i,j,k)
                   deltime_temp(idx) = deltime(k,2)
                   idx=idx+1
                end if
             END DO
          else
             phase_temp = phase(i,j,:)
             amp_temp = amps(i,j,:)
             deltime_temp = deltime(:,2)
          end if

          ! Calculate the number of days across all valid interferograms 
          ndays = sum(deltime_temp)
          npts(i,j) = nrows
          stacktime(i,j) = ndays

          if (nrows /= 0) then
             amp(i,j) = sum(amp_temp)/real(nrows)
             stack(i,j) = sum(phase_temp)/real(ndays)
          end if


          if(nrows.ne.ncells)then
             ! Get the temporary Tminv matrix
             ! Calculate Tminv with internal subroutine pinv
             ALLOCATE(Tminv_temp(nslc-1,nrows))
             call pinv(Tm_temp,nrows,nslc-1,Tminv_temp)
             DEALLOCATE(Tm_temp)

          
             ! Get the preliminary velocity solution via matrix multiplication
             velocity(i,j,:)=MATMUL(Tminv_temp,phase_temp(:))

             ! Get the first and last valid date for the pixel
             DO k=1,nslc-1
                if (velocity(i,j,k).ne.0)THEN
                   idxf(i,j) = k
                   exit
                end if
             END DO
             DO k=1,nslc-1
                if (velocity(i,j,nslc-k).ne.0)THEN
                   idxl(i,j) = nslc-k
                   exit
                end if
             END DO
             deallocate(Tminv_temp)
          else
             velocity(i,j,:)=MATMUL(Tminv,phase_temp(:))
             idxf(i,j)=1
             idxl(i,j)=nslc-1
          end if

          ! Deallocate to save memory
          DEALLOCATE(phase_temp,amp_temp,deltime_temp)


          ! If there has been masking, check for any velocity values that are
          ! identically 0. If they are, interpolate between valid velocity
          ! estimates. No extrapolation though
          if(nrows.ne.ncells)then
             v_in = velocity(i,j,:)
             v_in = v_in(:)
             CALL interpolate_velocity(v_in,timedeltas,v_out)
             velocity_interp(i,j,:) = v_out
          else
             velocity_interp(i,j,:) = velocity(i,j,:)
          end if

       END DO
    END DO




    
    !!!!! WRITE RESULTS TO FILES !!!!!
    print *,'Writing velocity and displacement solutions'
    OPEN(28,FILE='velocity',FORM='unformatted',ACCESS='stream')
    WRITE(28)velocity
    close(28)

    if (intmaskflag=='Y'.or.maxTBfile.ne.'none') then
       OPEN(28,FILE='velocity_interp',FORM='unformatted',ACCESS='stream')
       WRITE(28)velocity_interp
       close(28)
    end if

    OPEN(29,FILE='displacement',FORM='unformatted',ACCESS='direct',RECL=nr*8)
    ALLOCATE(disp(nr))
    ! integrate velocities for displacement with parallelization of outer loop (each time step)
    !$OMP parallel do private(j,k,disp) shared(velocity,timedeltas,amp)
    do i=1,nslc-1
       do j=1,naz
          do k=1,nr
             disp(k) = sum(velocity(k,j,1:i)*timedeltas(1:i))
          end do

          ! write out average amplitude and displacement for each azimuth line and time step pair
          write(29,rec=j+(i-1)*naz)amp(:,j),disp
       end do
    end do
    !$OMP end parallel do
    close(29)

    if (intmaskflag=='Y'.or.maxTBfile.ne.'none') then
       OPEN(29,FILE='displacement_interp',FORM='unformatted',ACCESS='direct',RECL=nr*8)
       !$OMP parallel do private(j,k,disp) shared(velocity,timedeltas,amp)
       do i=1,nslc-1
          do j=1,naz
             do k=1,nr
                disp(k) = sum(velocity_interp(k,j,1:i)*timedeltas(1:i))
             end do

             ! write out average amplitude and displacement for each azimuth line
             ! and time step pair
             write(29,rec=j+(i-1)*naz)amp(:,j),disp
          end do
       end do
       !$OMP end parallel do
       close(29)

    end if


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


  ! ---------------------- interpolate_velocity ---------------------- !
  subroutine interpolate_velocity(velocity, dt, velocity_interp)
      implicit none
      real, dimension(:), intent(in) :: velocity, dt
      real, dimension(:), intent(out) :: velocity_interp
      integer :: i, start_idx, end_idx,k,sday
      real :: total_days, interp_value
      real :: vleft,vright,nday,dright,dleft
      real,dimension(:),allocatable :: v

      ! Copy the original velocity to the output (keeping non-zero values)
      velocity_interp = velocity

      ! Loop over the array and find intervals with zeros (NaNs)
      i = 1
      do while (i <= size(velocity))
          ! Skip if current element is non-zero
          if (velocity(i) /= 0.0) then
              i = i + 1
              cycle
          end if

          ! Find the start and end of a zero (NaN) region
          start_idx = i
          do while (i <= size(velocity) .and. velocity(i) == 0.0)
              i = i + 1
          end do
          end_idx = i

          ! Ensure there are valid values on both sides
          if (start_idx > 1 .and. end_idx <= size(velocity)) then
              if (velocity(start_idx - 1) /= 0.0 .and. velocity(end_idx) /= 0.0)then
                  ! Compute the total days between the endpoints
                  total_days = sum(dt(start_idx:end_idx-1))+1

                  ! Interpolate each zero position based on its relative days
                  ! weight
                  vleft = velocity(start_idx-1)
                  vright = velocity(end_idx)
                  do i = start_idx, end_idx - 1
                     nday = dt(i)
                     sday = int(1 + sum(dt(1:i)) - nday)
                     ALLOCATE(v(int(nday)))
                     DO k=sday,sday+int(nday)
                        dright = total_days-k
                        dleft = k
                        v(k) = ((dright*vleft)+(dleft*vright))/total_days
                     END DO
                     velocity_interp(i) = sum(v)/nday
                     DEALLOCATE(v)
                  end do
              end if
          end if
      end do
  end subroutine interpolate_velocity


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


END PROGRAM sbas_unique
