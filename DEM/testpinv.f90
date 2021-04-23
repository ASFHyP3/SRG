PROGRAM testpinv
  IMPLICIT none
  !specifications
  INTEGER::k,kk,c,r,stat
  INTEGER::nslc !number of slcs
  INTEGER::ncells !number of cells (files in flist)
  REAL,DIMENSION(:,:),ALLOCATABLE::Tm,Tminv,ident

  ncells=51
  nslc=17

!!$  ncells=6
!!$  nslc=5

  ALLOCATE(Tm(ncells,nslc-1))
  allocate(ident(nslc-1,nslc-1))

  OPEN(24,FILE='Tm.out',STATUS='old')
  do k=1,ncells
     READ(24,*,IOSTAT=stat)(Tm(k,kk),kk=1,nslc-1)
  end do
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

  print *
  print *,'Printing Tm'
  do k=1,ncells
     print *,(Tm(k,kk),kk=1,nslc-1)
  end do
  print *
  print *

  ALLOCATE(Tminv(nslc-1,ncells))
  Tminv=pinv(Tm,ncells,nslc-1)
!  call matrixinv(Tm,Tminv,nslc-1,ncells)
  print *
  print *,'Printing Tminv'
  do k=1,nslc-1
     print *,(Tminv(k,kk),kk=1,ncells)
  end do

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
    INTEGER :: ierr,kk
    REAL,DIMENSION(rows,cols),INTENT(IN)::mat
    REAL,DIMENSION(cols,rows)::pinv
    REAL*8,DIMENSION(:),allocatable::Sing
    REAL*8,DIMENSION(:,:),allocatable::Sinv,dmat,U,V

    allocate (dmat(rows,cols),Sinv(cols,rows),U(rows,rows),V(cols,cols),Sing(rows))

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

    print *,'ierr= ',ierr,rows,cols
    print *,'S: ',Sing
    Sinv=0.
    DO r=1,rows
       DO c=1,cols
          IF(r==c)THEN
             Sinv(c,r)=1/Sing(r)
          ENDIF
       END DO
    END DO

    print *,'Sinv'
    do r=1,cols
       print *,(Sinv(r,kk),kk=1,rows)
    end do
    print *,'U'
    do r=1,rows
       print *,(U(r,kk),kk=1,rows)
    end do
    print *,'V'
    do r=1,cols
       print *,(V(r,kk),kk=1,cols)
    end do

    !V=TRANSPOSE(MATMUL(TRANSPOSE(Sinv),MATMUL(TRANSPOSE(U),mat)))
    pinv(:,:)=sngl(MATMUL(MATMUL(V,Sinv),TRANSPOSE(U)))
  END FUNCTION pinv

END PROGRAM testpinv
