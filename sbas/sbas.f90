! run sbas on a set of unwrapped files
! Read also correlation and amp files
!  this version allows for averaging multiple reference points if file is supplied

PROGRAM sbas
  use omp_lib

  IMPLICIT none

  !specifications
  INTEGER::i,j,r,c,rows,cols,stat,fstat,ierr,looks,k,kk,n_refs,igramnumber
  real*8 jdprimary, jdsecondary
  INTEGER*8 ::nr, naz, reclphase, recsize, filelen !image size
  INTEGER::nslc !number of slcs
  INTEGER::ncells !number of cells (files in flist)
  INTEGER,DIMENSION(13)::statb
  CHARACTER(100)::filelist,str,ref_locs_file
  CHARACTER(100),DIMENSION(:),ALLOCATABLE::cells !array of file names
  CHARACTER(100)::strint,strunw,stramp,strcc
  REAL,DIMENSION(:,:,:),ALLOCATABLE::coh,phase,temp3
  REAL,DIMENSION(:,:),ALLOCATABLE::amp,dat,mask,temp2,stack,stacktime
  REAL,DIMENSION(:),ALLOCATABLE::temp1, phase_ref, disp
  REAL,DIMENSION(:,:),ALLOCATABLE::Tm,deltime,Tminv,ident
  REAL,DIMENSION(:),ALLOCATABLE::Bperp,timedeltas
  INTEGER::r_ref,az_ref !reference pixel location
  REAL,DIMENSION(:,:,:),ALLOCATABLE::velocity
  integer, dimension(:,:), allocatable :: ref_locs
  real, dimension(:,:,:),allocatable :: amps
  integer, dimension(:,:),allocatable :: npts

  !executions

  !flist: list of unwrapped files
  !nfiles: number of files in flist
  !nslcs: number of slc files
  !len: width of the files
  IF(iargc().lt.4)THEN
     WRITE(*,*)'usage: sbas unwlist nunwfiles nslcs len <ref_locations_file = x/2,y/2>)'
     STOP
  END IF

  CALL getarg(1,filelist)
  CALL getarg(2,str)
  READ(str,*)ncells !number of files
  CALL getarg(3,str)
  READ(str,*)nslc !number of slcs
  CALL getarg(4,str)
  READ(str,*)nr !width of files

  ALLOCATE(cells(ncells),ref_locs(2,10000000))

  OPEN(UNIT=1,FILE=filelist,STATUS='old')
  READ(1,'(A)',end=10,IOSTAT=stat)cells
!  PRINT*,cells(:) !print file names
10 continue
  CLOSE(1)

  OPEN(UNIT=2,FILE=trim(adjustl(cells(1))),FORM='unformatted',ACCESS='direct',RECL=nr*8)
  ierr=fstat(2,statb)
  naz=statb(8)/8/nr
  WRITE(*,*)' Lines in file: ',naz
  CLOSE(2)

  ! set list of reference points, default to scene center
  n_refs=1
  ref_locs(1,1)=nr/2
  ref_locs(2,1)=naz/2
  if(iargc().ge.5)then
     CALL getarg(5,str)
     READ(str,*)ref_locs_file ! ref pix file with list of locations
     open(unit=3,file=ref_locs_file)
     do i=1,10000000
        read(3,*,end=99)ref_locs(:,i)
     end do
99   n_refs=i-1
  end if
  print *,'Number of reference points: ',n_refs

  ALLOCATE(phase(nr,naz,ncells),amps(nr,naz,ncells))
  ALLOCATE(amp(nr,naz),npts(nr,naz),stacktime(nr,naz),stack(nr,naz))
  ALLOCATE(phase_ref(ncells), disp(nr))

  phase(:,:,:)=0
  amp(:,:)=0
  stacktime=0.
  stack=0.
  amps=0.

  ! read in sbas parameters needed for stacking and sbas
  !From tsxtime and tsxbaseline
  !Create an array with data from Tm.out
  ALLOCATE(Tm(ncells,nslc-1))
  OPEN(24,FILE='Tm.out',STATUS='old')
  do k=1,ncells
     READ(24,*,IOSTAT=stat)(Tm(k,kk),kk=1,nslc-1)
  end do
  CLOSE(24)
  !print *,Tm

  !Read Bperp.out into an array
  ALLOCATE(Bperp(ncells))
  OPEN(25,FILE='Bperp.out',STATUS='old')
  READ(25,*,IOSTAT=stat)Bperp
  CLOSE(25)

  !Create an array with data from deltime.out
  ALLOCATE(deltime(4,ncells))
  OPEN(26,FILE='deltime.out',STATUS='old')
  DO r=1,ncells
     READ(26,*,IOSTAT=stat)deltime(:,r)
  END DO
  CLOSE(26)

  ! Read time from slc to slc
  ALLOCATE(timedeltas(nslc-1))
  open(27,file='timedeltas.out',status='old')
  read(27,*)timedeltas
  close(27)

  !read in unwrapped igrams
  !$omp parallel do private(k,dat,strint,stramp) &
  !$omp shared(cells,ref_locs,deltime,nr,naz,ncells,phase,amps,phase_ref)
  DO i=1,ncells
     allocate (dat(nr*2,naz))
     strint=trim(adjustl(cells(i)))
     OPEN(UNIT=i+10,FILE=strint,FORM='unformatted',ACCESS='direct',RECL=2*nr*naz*4,CONVERT='LITTLE_ENDIAN')  
     READ(i+10,rec=1)dat
     CLOSE(i+10)
     phase(:,:,i)=dat((nr+1):2*nr,:)

     stramp=Replace_Text(strint,'.unw','.amp')
     OPEN(UNIT=i+10,FILE=stramp,FORM='unformatted',ACCESS='direct',RECL=2*nr*naz*4,CONVERT='LITTLE_ENDIAN')  
     READ(i+10,rec=1)dat
     CLOSE(i+10)
     amps(:,:,i)=dat(1:2*nr-1:2,:)**2+dat(2:2*nr:2,:)**2
 
     deallocate(dat)

    !  compute the reference phase for ith interferogram
     phase_ref(i)=0.
     do k=1,n_refs
        phase_ref(i)=phase_ref(i)+phase(nr+ref_locs(1,k),ref_locs(2,k),i)/n_refs
     end do

     ! subtract reference phase from each interferogram
     phase(:,:,i)=phase(:,:,i)-phase_ref(i)

  END DO
  !$omp end parallel do
  amps=sqrt(amps) ! amplitudes not powers

!  print *,'reference phases: ',phase_ref
!  print *,'deltime: ',deltime(2,:)

  ! stack phases and amps
  do i=1,ncells
     !$omp parallel do private(k) shared(nr,naz,amps,npts,stacktime,deltime,amp,stack,ncells)
     do j=1,nr
        do k=1,naz
           if(amps(j,k,i)>1.e-6)then
              npts(j,k)=npts(j,k)+1
              stacktime(j,k)=stacktime(j,k)+deltime(2,i)
              amp(j,k)=amp(j,k)+amps(j,k,i)
              stack(j,k)=stack(j,k)+phase(j,k,i)!-phase_ref(i)
           end if
           if(i.eq.ncells)then
              if(npts(j,k).eq.0)then
                 npts(j,k)=1
                 stacktime(j,k)=1.
              end if
           end if
        end do
     end do
     !$omp end parallel do
  END DO

  ! write stack files for further reference
  print *,'Writing stacks'
  OPEN(28,FILE='npts',FORM='unformatted',ACCESS='stream')
  WRITE(28)npts
  close(28)
  OPEN(28,FILE='stacktime',FORM='unformatted',ACCESS='stream')
  WRITE(28)stacktime
  close(28)
  !Write weighted amps and phases stack into stackmht, phases in rad/day
  amp=amp/npts
  stack=stack/stacktime
  OPEN(29,FILE='stackmht',FORM='unformatted',ACCESS='direct',RECL=nr*8)
  do i=1,naz
     WRITE(29,rec=i)amp(:,i),stack(:,i)
  end do
  CLOSE(29)

!Save nr, naz, nslc, ncells parameters in one file
  OPEN(15,FILE='parameters',STATUS='replace')
  WRITE(15,*) nr, naz, nslc, ncells
  CLOSE(15)

PRINT*,'Data loaded'

!SBAS least squares
!at each pixel, solve for velocity at (nslc-1) time interval
  ALLOCATE(velocity(nr,naz,nslc-1))
  velocity(:,:,:)=0
  ALLOCATE(Tminv(nslc-1,ncells))
  ALLOCATE(temp1(ncells))

  call pinv(Tm,ncells,nslc-1,Tminv)
!  print *,'inverse matrix computed'

  ! velocity solution at each pixel
  !$omp parallel do private(i,temp1) shared(naz,nr,phase_ref,velocity,Tminv)
  DO j=1,naz
     DO i=1,nr
        temp1=phase(i,j,:)!-phase_ref(:)
        temp1=temp1(:)
        velocity(i,j,:)=MATMUL(Tminv,temp1) 
     END DO
  END DO
  !$omp end parallel do
!  print *,'SBAS solution computed'
  DEALLOCATE(temp1)

  !Write velocity matrix into file
  OPEN(28,FILE='velocity',FORM='unformatted',ACCESS='stream')!direct',RECL=recsize)
  WRITE(28)velocity
  close(28)

  OPEN(29,FILE='displacement',FORM='unformatted',ACCESS='direct',RECL=nr*8)

  ! integrate velocities for displacement
  !$omp parallel do private(j,k,disp) shared(nslc,naz,nr,velocity,timedeltas,amp) 
  do i=1,nslc-1
     do j=1,naz
        do k=1,nr
           disp(k)=sum(velocity(k,j,1:i)*timedeltas(1:i))
        end do
!        print *,j+(i-1)*naz
        write(29,rec=j+(i-1)*naz)amp(:,j),disp
     end do
  end do
  !$omp end parallel do 
  close(29)
!  print *,'Displacements written'


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!FUNCTIONS

CONTAINS

  !strrep(str,orig,rep): replaces all instaces of orig in str with rep
  CHARACTER(30) FUNCTION strrep(str,orig,rep)
    IMPLICIT none
    !specifications
    INTEGER::i
    CHARACTER(100),INTENT(IN)::str
    CHARACTER(2),INTENT(IN)::orig,rep
    !executions
    strrep(:) = str(:)
    DO i=1,LEN(strrep)-1
       IF(strrep(i:i+1) == orig(:))THEN
          strrep(i:i+1) = rep(:)
       END IF
    END DO
  END FUNCTION strrep

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

  !mean2d(mat): calculates the mean value of a 2D matrix mat with r rows and c columns
  REAL FUNCTION mean2d(mat,rows,cols)
    IMPLICIT none
    REAL,INTENT(IN),DIMENSION(:,:) :: mat
    INTEGER :: r,c,rows,cols
    mean2d=0
    DO r=1,rows
       DO c=1,cols
          mean2d=mean2d+mat(r,c)
       END DO
    END DO
    mean2d=mean2d/SIZE(mat)
  END FUNCTION mean2d

!!$  FUNCTION pinv(mat,rows,cols)
!!$    IMPLICIT none
!!$    !specifications
!!$    INTEGER,INTENT(IN)::rows,cols
!!$    REAL,DIMENSION(rows,cols),INTENT(IN)::mat
!!$    REAL,DIMENSION(cols,rows)::pinv
!!$    REAL,DIMENSION(rows)::Sig
!!$    REAL,DIMENSION(rows,cols)::Sinv
!!$    REAL,DIMENSION(rows,rows)::U
!!$    REAL,DIMENSION(cols,cols)::V
!!$    !executions
!!$    CALL eigen(rows,MATMUL(mat,TRANSPOSE(mat)),Sig,U)
!!$    DO r=1,rows
!!$       DO c=1,cols
!!$          IF(r==c)THEN
!!$             Sinv(r,c)=1/SQRT(Sig(r))
!!$          ELSE
!!$             Sinv(r,c)=0
!!$          ENDIF
!!$       END DO
!!$    END DO
!!$    V=TRANSPOSE(MATMUL(TRANSPOSE(Sinv),MATMUL(TRANSPOSE(U),mat)))
!!$    pinv(:,:)=MATMUL(MATMUL(V,TRANSPOSE(Sinv)),TRANSPOSE(U))
!!$  END FUNCTION pinv

!  FUNCTION pinv(mat,rows,cols)
  subroutine pinv(mat,rows,cols,matinv)
    IMPLICIT none
    !specifications
    INTEGER,INTENT(IN)::rows,cols
    INTEGER :: ierr,kk
    REAL,DIMENSION(rows,cols),INTENT(IN)::mat
    REAL,DIMENSION(cols,rows)::matinv
    REAL*8,DIMENSION(:),allocatable::Sing
    REAL*8,DIMENSION(:,:),allocatable::Sinv,dmat,U,V,X,S

!    print *,'entering pinv, rows, cols ',rows,cols
    allocate (dmat(rows,cols),Sinv(cols,cols))
    allocate (U(rows,cols),V(cols,cols),Sing(cols),S(cols,cols))

    !executions
    !!CALL eigen(rows,MATMUL(mat,TRANSPOSE(mat)),Sing,U)
   ! print *,'mat',mat
    do r=1,rows
       do c=1,cols
          dmat(r,c)=mat(r,c)
          !print *,r,c,mat(r,c),dmat(r,c)
       end do
    end do

    call svd(rows,cols,dmat,Sing,.true.,U,.true.,V,ierr)

    S=0.
    do c=1,cols
       S(c,c)=Sing(c)
    end do

    Sinv=0.
    DO r=1,cols
          if(abs(S(r,r)).gt.1.e-6)Sinv(r,r)=1./S(r,r)
    END DO

!    print *,'computing pinv'
    matinv(:,:)=sngl(MATMUL(MATMUL(V,Sinv),TRANSPOSE(U)))

!    deallocate (dmat,Sinv,U,V,Sing)
    deallocate (dmat)
    deallocate (V)
    deallocate (Sing)
    deallocate (Sinv)
    deallocate (U)
  END subroutine pinv

END PROGRAM sbas

