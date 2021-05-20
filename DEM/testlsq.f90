PROGRAM testlsq
  IMPLICIT none
  !specifications
  INTEGER::k,kk,c,r,stat,i
  INTEGER::nslc !number of slcs
  INTEGER::ncells !number of cells (files in flist)
  REAL,DIMENSION(:,:),ALLOCATABLE::Tm
  REAL,DIMENSION(:),ALLOCATABLE::v,d

  ncells=51
  nslc=17

!!$  ncells=4
!!$  nslc=4

  ALLOCATE(Tm(ncells,nslc-1))

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

!  compute lsq solution

  ALLOCATE(d(ncells),v(ncells))
  do i=1,ncells
     d(i)=i
  end do

  print *,'calling lsq'

  call lsq(ncells,nslc-1,1,Tm,d,v)

  print *,'velocities: ',v

END PROGRAM testlsq
