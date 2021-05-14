! run sbas on a set of unwrapped files
! Read also correlation and amp files
! Generate a mask
!  this version allows for averaging multiple reference points if file is supplied
!  sba_constrained lets us use constrained linear inversion

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
  REAL,DIMENSION(:,:,:),ALLOCATABLE::coh,phase,temp3
  REAL,DIMENSION(:,:),ALLOCATABLE::amp,dat,mask,temp2,stack
  REAL,DIMENSION(:),ALLOCATABLE::temp1, phase_ref, disp
  COMPLEX,DIMENSION(:,:,:),ALLOCATABLE::cpx
  REAL,DIMENSION(:,:),ALLOCATABLE::Tm,deltime,Tminv,ident,hth
  REAL,DIMENSION(:),ALLOCATABLE::Bperp,timedeltas
  INTEGER::r_ref,az_ref !reference pixel location
  REAL,DIMENSION(:,:,:),ALLOCATABLE::velocity
  INTEGER,DIMENSION(1:2)::maskShape,ampShape
  INTEGER,DIMENSION(1:3)::phaseShape
  integer, dimension(2,1000000) :: ref_locs
  real:: stacktime,mu

  !executions

  !flist: list of unwrapped files
  !nfiles: number of files in flist
  !nslcs: number of slc files
  !len: width of the files
  IF(iargc().lt.6)THEN
     WRITE(*,*)'usage: sbas unwlist nunwfiles nslcs len ref_locations_file mu'
     STOP
  END IF

  CALL getarg(1,filelist)
  CALL getarg(2,str)
  READ(str,*)ncells !number of files
  CALL getarg(3,str)
  READ(str,*)nslc !number of slcs
  CALL getarg(4,str)
  READ(str,*)nr !width of files
  CALL getarg(6,str)
  READ(str,*)mu !weight on constraint

  ALLOCATE(cells(ncells), disp(nr))

  OPEN(UNIT=1,FILE=filelist,STATUS='old')
  READ(1,'(A)',end=10,IOSTAT=stat)cells
  !PRINT*,cells(:) !print file names
10 continue
  CLOSE(1)

  OPEN(UNIT=2,FILE=cells(1),FORM='unformatted',ACCESS='direct',RECL=nr*8)
  ierr=fstat(2,statb)
  naz=statb(8)/8/nr
  WRITE(*,*)'Lines in file: ',naz
  CLOSE(2)

!From tsxtime and tsxbaseline
!Create an array with data from Tm.out
  ALLOCATE(Tm(ncells,nslc-1), hth(nslc-1,nslc-1))
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

! create the constraint matrix
  hth=0.
  do k=1,nslc-1
     hth(k,k)=2.
     if(k.gt.1)then
        hth(k,k-1)=-1.
        hth(k-1,k)=-1.
     end if
  end do
  hth(1,1)=1.
  hth(nslc-1,nslc-1)=1.
  open(28,file='hth')
  do k=1,nslc-1
     write(28,'(20f5.1)')hth(:,k)
  end do
  close(28)

  ! set list of reference points, default to scene center
  n_refs=1
  ref_locs(1,1)=nr/2
  ref_locs(2,1)=naz/2
  if(iargc().ge.5)then
     CALL getarg(5,str)
     READ(str,*)ref_locs_file ! ref pix file with list of locations
     open(unit=3,file=ref_locs_file)
     do i=1,1000000
        read(3,*,end=99)ref_locs(:,i)
     end do
99   n_refs=i-1
  end if
  print *,'Number of reference points: ',n_refs

!  ALLOCATE(coh(nr,naz,ncells))
  ALLOCATE(phase(nr,naz,ncells))
  ALLOCATE(amp(nr,naz))
  ALLOCATE(dat(2*nr,naz))
  ALLOCATE(temp1(2*nr*naz),phase_ref(ncells))
!  coh(:,:,:)=0
  phase(:,:,:)=0
  amp(:,:)=0
  dat(:,:)=0
  temp1(:)=0

  !read in unwrapped igrams
  DO i=1,ncells
     strint=cells(i)
     OPEN(UNIT=22,FILE=strint,FORM='unformatted',ACCESS='direct',RECL=2*nr*naz*4,CONVERT='LITTLE_ENDIAN')  
     READ(22,rec=1)dat
     CLOSE(22)
     phase(:,:,i)=dat((nr+1):2*nr,:)
  END DO
  PRINT*,'Unwrapped interferograms read in.'

  !read in amplitudes
  DO i=1,ncells
     strint=cells(i)
     stramp=Replace_Text(strint,'.unw','.amp')
     OPEN(UNIT=23,FILE=stramp,FORM='unformatted',ACCESS='direct',RECL=2*nr*naz*4,CONVERT='LITTLE_ENDIAN')  
     READ(23,rec=1)dat
     CLOSE(23)
     amp=amp+(dat(1:2*nr-1:2,:)**2+dat(2:2*nr:2,:)**2)/ncells
  END DO
  amp=sqrt(amp)

  PRINT*,'Amplitudes read in'

  DEALLOCATE(temp1)

  ALLOCATE(mask(nr,naz))
  mask(:,:)=1
  
  !store amplitudes and phases in complex array
  ALLOCATE(cpx(nr,naz,ncells))
  DO i=1,ncells
     cpx(:,:,i)=cmplx(amp(:,:),phase(:,:,i))
  END DO

!  print *,'ref_locs: ',ref_locs(:,1:n_refs)

!Save amp
  OPEN(11,FILE='amp',FORM='unformatted',ACCESS='direct',RECL=nr*naz*4)
  WRITE(11,rec=1)amp
  CLOSE(11)
!  OPEN(12,FILE='mask',FORM='unformatted',ACCESS='direct',RECL=nr*naz*4)
!  WRITE(12,rec=1)mask
!  CLOSE(12)

!Save dimensions of matrices
  ampShape=SHAPE(amp)
  maskShape=SHAPE(mask)
  phaseShape=SHAPE(phase)
  OPEN(14,FILE='dimensions',STATUS='replace')
  WRITE(14,*)ampShape(1),ampShape(2),maskShape(1),maskShape(2),phaseShape(1),phaseShape(2),phaseShape(3)
  CLOSE(14)

!Save nr, naz, nslc, ncells parameters in one file
  OPEN(15,FILE='parameters',STATUS='replace')
  WRITE(15,*) nr, naz, nslc, ncells
  CLOSE(15)

PRINT*,'Data loaded, total stack time= ',stacktime

!SBAS least squares
!at each pixel, solve for velocity at (nslc-1) time interval
  ALLOCATE(velocity(nr,naz,nslc-1))
  velocity(:,:,:)=0
  ALLOCATE(Tminv(nslc-1,ncells))
  ALLOCATE(temp1(ncells))

!  Tminv=pinv(Tm,ncells,nslc-1)   ! unconstrained solution
  Tminv = matmul(pinv(matmul(transpose(Tm),Tm)+mu*hth,nslc-1,nslc-1),transpose(Tm))

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
  
  !  compute the reference phase
  phase_ref=0.
  do i=1,n_refs
     phase_ref=phase_ref+phase(ref_locs(1,i),ref_locs(2,i),:)/n_refs
!     print *,phase_ref,ref_locs(1,i),ref_locs(2,i)
  end do

  !debug - write out referenced unwrapped igrams
!  open(23,file='q/stackmht',access='direct',recl=nr*8)
!  DO i=1,ncells
!     strint=cells(i)
!     OPEN(UNIT=22,FILE=strint,FORM='unformatted',ACCESS='direct',RECL=2*nr*naz*4,CONVERT='LITTLE_ENDIAN')  
!     READ(22,rec=1)dat
!     CLOSE(22)
!     OPEN(UNIT=22,FILE='q/'//strint,FORM='unformatted',ACCESS='direct',RECL=2*nr*4,CONVERT='LITTLE_ENDIAN')  
!     do k=1,naz
!     write(22,rec=k)dat(1:nr,k),dat(nr+1:nr*2,k)-phase_ref(i)
!     write(23,rec=k+(i-1)*naz)dat(1:nr,k),dat(nr+1:nr*2,k)-phase_ref(i)
!     end do
!     CLOSE(22)
!  END DO
!  PRINT*,'Phase-referenced unwrapped interferograms written.'
!  close(23)

  DO j=1,naz
     DO i=1,nr
        temp1=phase(i,j,:)-phase_ref(:)
        temp1=temp1(:)
        velocity(i,j,:)=MATMUL(Tminv,temp1) 
     END DO
  END DO
  DEALLOCATE(temp1)
  !Write velocity matrix into file
  OPEN(28,FILE='velocity',FORM='unformatted',ACCESS='direct',RECL=nr*naz*(nslc-1)*4)
  WRITE(28,rec=1)velocity
  OPEN(29,FILE='displacement',FORM='unformatted',ACCESS='direct',RECL=nr*8)
!  print *,nslc,naz,naz+(nslc-1)*naz
  do i=1,nslc-1
     do j=1,naz
        do k=1,nr
           disp(k)=sum(velocity(k,j,1:i)*timedeltas(1:i))
        end do
!        print *,j+(i-1)*naz
        write(29,rec=j+(i-1)*naz)amp(:,j),disp
     end do
  end do
  close(29)
!Create stack to plot images (using MATLAB)
  ALLOCATE(stack(nr,naz))
  stack=SUM(phase,3)/stacktime
  !Write stack matrix into file, both phase and amp/phase versions
  OPEN(29,FILE='stack',FORM='unformatted',ACCESS='direct',RECL=nr*naz*4)
  WRITE(29,rec=1)stack
  CLOSE(29)
  OPEN(29,FILE='stackmht',FORM='unformatted',ACCESS='direct',RECL=nr*8)
  do i=1,naz
     WRITE(29,rec=i)amp(:,i),stack(:,i)
  end do
  CLOSE(29)

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
    print *,'S: ',Sing
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
