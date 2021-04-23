PROGRAM testqrpinv
  IMPLICIT none
  !specifications
  INTEGER::k,kk,c,r,stat
  INTEGER::nslc !number of slcs
  INTEGER::ncells !number of cells (files in flist)
  REAL,DIMENSION(:,:),ALLOCATABLE::Tm,Tminv,ident,U,Q,W
  REAL,DIMENSION(:),ALLOCATABLE::V

  ncells=51
  nslc=17

!!$  ncells=4
!!$  nslc=4

  ALLOCATE(Tm(ncells,nslc-1))
  allocate(ident(nslc-1,nslc-1))

!  first read the Tm matrix
  OPEN(24,FILE='Tm.out',STATUS='old')
  READ(24,*,IOSTAT=stat)((Tm(k,kk),kk=1,nslc-1),k=1,ncells)
  CLOSE(24)
!!$  Tm(1,1)=63
!!$  Tm(2,1)=42
!!$  Tm(3,1)=0
!!$  Tm(4,1)=126
!!$  Tm(1,2)=41
!!$  Tm(2,2)=60
!!$  Tm(3,2)=-28
!!$  Tm(4,2)=82
!!$  Tm(1,3)=-88
!!$  Tm(2,3)=51
!!$  Tm(3,3)=56
!!$  Tm(4,3)=-71

  print *,'Printing Tm'
  do k=1,nslc-1
     print '(51f5.0)',(Tm(kk,k),kk=1,ncells)
  end do
  !print *,Tm
  print *
  print *

!  compute QR decomposition

  ALLOCATE(Tminv(nslc-1,ncells),V(ncells),U(ncells,ncells),Q(ncells,ncells),W(ncells,ncells))

  call QRfac(nslc-1,ncells,ncells,Tm,V,U,Q,W)

  print *
  print *,'Q matrix'
  print *,Q
  print *

  print *
  print *,'R matrix'
  print *,Tm
  print *

  Tminv=pinv(Tm,ncells,nslc-1)
!  call matrixinv(Tm,Tminv,nslc-1,ncells)
  print *,'Printing Tminv'
  print *,Tminv
  print *
  print *
  open(99,file='Tminv')
  do k=1,nslc-1
     write(99,*)(Tminv(kk,k),kk=1,ncells)
  end do
  close(99)

  print *,'shapes ',shape(Tm),shape(Tminv)

  ident=matmul(Tminv,Tm)
  print *,'Computed identity'
  open(99,file='identity')
  do k=1,nslc-1
     write(99,*)(ident(kk,k),kk=1,nslc-1)
     print *,(ident(kk,k),kk=1,nslc-1)
  end do
  print *,'Printed identity'
  close(99)

!FUNCTIONS

CONTAINS

  FUNCTION pinv(mat,rows,cols)
    IMPLICIT none
    !specifications
    INTEGER,INTENT(IN)::rows,cols
    REAL,DIMENSION(rows,cols),INTENT(IN)::mat
    REAL,DIMENSION(cols,rows)::pinv
    REAL,DIMENSION(rows)::Sig
    REAL,DIMENSION(rows,cols)::Sinv
    REAL,DIMENSION(rows,rows)::U
    REAL,DIMENSION(cols,cols)::V
    !executions
    CALL eigen(rows,MATMUL(mat,TRANSPOSE(mat)),Sig,U)
    DO r=1,rows
       DO c=1,cols
          IF(r==c)THEN
             Sinv(r,c)=1/SQRT(Sig(r))
          ELSE
             Sinv(r,c)=0
          ENDIF
       END DO
    END DO
    V=TRANSPOSE(MATMUL(TRANSPOSE(Sinv),MATMUL(TRANSPOSE(U),mat)))
    pinv(:,:)=MATMUL(MATMUL(V,TRANSPOSE(Sinv)),TRANSPOSE(U))
  END FUNCTION pinv

END PROGRAM testqrpinv
