! run sbas on a set of simulated data

PROGRAM sbas
  IMPLICIT none

  !specifications
  INTEGER::i,j,r,c,rows,cols,stat,fstat,ierr,looks,k,kk,n_refs
  INTEGER*8 ::nr, naz, reclphase !image size
  INTEGER::nslc !number of slcs
  INTEGER::ncells !number of cells (files in flist)
  INTEGER,DIMENSION(13)::statb
  CHARACTER(100)::filelist,str,ref_locs_file
  CHARACTER(100),DIMENSION(:),ALLOCATABLE::cells !array of file names
  CHARACTER(100)::strint,strunw,stramp,strcc
  REAL,DIMENSION(:),ALLOCATABLE::coh,phase,temp3
  REAL,DIMENSION(:,:),ALLOCATABLE::amp,dat,mask,temp2,stack
  REAL,DIMENSION(:),ALLOCATABLE::temp1, phase_ref
  COMPLEX,DIMENSION(:,:,:),ALLOCATABLE::cpx
  REAL,DIMENSION(:,:),ALLOCATABLE::Tm,deltime,Tminv,ident
  REAL,DIMENSION(:),ALLOCATABLE::Bperp,timedeltas
  INTEGER::r_ref,az_ref !reference pixel location
  REAL,DIMENSION(:),ALLOCATABLE::velocity
  INTEGER,DIMENSION(1:2)::maskShape,ampShape
  INTEGER,DIMENSION(1:3)::phaseShape
  integer, dimension(2,1000000) :: ref_locs
  real:: stacktime, disp

  !executions

  !flist: list of unwrapped files
  !nfiles: number of files in flist
  !nslcs: number of slc files
  !len: width of the files
  IF(iargc().lt.3)THEN
     WRITE(*,*)'usage: sbas unwlist nunwfiles nslcs'
     STOP
  END IF

  CALL getarg(1,filelist)
  CALL getarg(2,str)
  READ(str,*)ncells !number of files
  CALL getarg(3,str)
  READ(str,*)nslc !number of slcs

  ALLOCATE(cells(ncells))

  OPEN(UNIT=1,FILE=filelist,STATUS='old')
  READ(1,'(A)',end=10,IOSTAT=stat)cells
  !PRINT*,cells(:) !print file names
10 continue
  CLOSE(1)

  ALLOCATE(phase(ncells))

  phase(:)=0

  !read in unwrapped igram data
  open(22,file='simdata.txt')
  DO i=1,ncells
     read(22,*)phase(i)
  END DO
  PRINT*,'Unwrapped interferograms read in.'

!Save nr, naz, nslc, ncells parameters in one file
  OPEN(15,FILE='parameters',STATUS='replace')
  WRITE(15,*) nr, naz, nslc, ncells
  CLOSE(15)

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
  ALLOCATE(temp1(ncells*4))
  ALLOCATE(deltime(ncells,4))
  stacktime=0.
  OPEN(26,FILE='deltime.out',STATUS='old')
  READ(26,*,IOSTAT=stat)temp1
  DO r=1,ncells
     DO c=1,4
        deltime(r,c)=temp1((r-1)*4+c)
     END DO
     stacktime=stacktime+deltime(r,2)
  END DO
  CLOSE(26)
  DEALLOCATE(temp1)

! Read time from slc to slc
  ALLOCATE(timedeltas(nslc-1))
  open(27,file='timedeltas.out',status='old')
  read(27,*)timedeltas
  close(27)

PRINT*,'Data loaded, total stack time= ',stacktime

!SBAS least squares
!at each pixel, solve for velocity at (nslc-1) time interval
  ALLOCATE(velocity(nslc-1))
  velocity(:)=0
  ALLOCATE(Tminv(nslc-1,ncells))
  ALLOCATE(temp1(ncells))

  Tminv=pinv(Tm,ncells,nslc-1)
  print *,'inverse matrix computed'
!  open(99,file='Tminv')
!  do k=1,ncells
!     write(99,*)Tminv(k,:)
!  end do
!  close(99)
  allocate (ident(nslc-1,nslc-1))
  open(99,file='ident')
  ident=matmul(Tminv,Tm)
  write(99,*)ident
  close(99)
  
  velocity(:)=MATMUL(Tminv,phase) 

  !Write velocity into file
  OPEN(28,FILE='velocity')
  do i=1,nslc-1
     WRITE(28,*)sum(timedeltas(1:i)),velocity(i)
  end do
  close(28)

  disp=1.5
  print *,'disp test ',disp
  OPEN(29,FILE='displacement')
  do i=1,nslc-1
     disp=sum(velocity(1:i)*timedeltas(1:i))
!     print *,disp
     write(29,*)sum(timedeltas(1:i)),disp
  end do
  close(29)

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

  FUNCTION pinv(mat,rows,cols)
    IMPLICIT none
    !specifications
    INTEGER,INTENT(IN)::rows,cols
    INTEGER :: ierr,kk
    REAL,DIMENSION(rows,cols),INTENT(IN)::mat
    REAL,DIMENSION(cols,rows)::pinv
    REAL*8,DIMENSION(:),allocatable::Sing
    REAL*8,DIMENSION(:,:),allocatable::Sinv,dmat,U,V

    allocate (dmat(rows,cols),Sinv(cols,rows),U(rows,rows),V(cols,cols),Sing(cols))

    !executions
    !!CALL eigen(rows,MATMUL(mat,TRANSPOSE(mat)),Sing,U)
   ! print *,'mat',mat
    do r=1,rows
       do c=1,cols
          dmat(r,c)=mat(r,c)
          !print *,r,c,mat(r,c),dmat(r,c)
       end do
    end do
    !dmat=mat
    !print *,'dmat',dmat
    call svd(rows,cols,dmat,Sing,.true.,U,.true.,V,ierr)

    !print *,'ierr= ',ierr,rows,cols
    !print *,'S: ',Sing
    Sinv=0.
    DO r=1,rows
       DO c=1,cols
          IF(r==c)THEN
             if(abs(Sing(r)).gt.1.e-6)Sinv(c,r)=1/Sing(r)
             !print *,'Sinv: ',r,Sinv(c,r)
          ENDIF
       END DO
    END DO

!!$    print *,'Sinv'
!!$    do r=1,cols
!!$       print *,(Sinv(r,kk),kk=1,rows)
!!$    end do
!!$    print *,'U'
!!$    do r=1,rows
!!$       print *,(U(r,kk),kk=1,rows)
!!$    end do
!!$    print *,'V'
!!$    do r=1,cols
!!$       print *,(V(r,kk),kk=1,cols)
!!$    end do

    !V=TRANSPOSE(MATMUL(TRANSPOSE(Sinv),MATMUL(TRANSPOSE(U),mat)))
    pinv(:,:)=sngl(MATMUL(MATMUL(V,Sinv),TRANSPOSE(U)))
  END FUNCTION pinv

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

END PROGRAM sbas
