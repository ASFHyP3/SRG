! run sbas on a set of unwrapped files, but use multiple reference points
! Read also correlation and amp files
! Generate a mask

PROGRAM sbas
  IMPLICIT none

  !specifications
  INTEGER::i,j,r,c,rows,cols,stat,fstat,ierr,looks,k,kk
  INTEGER*8 ::nr,naz,nr_s,naz_s, reclphase !image size
  INTEGER::nslc !number of slcs
  INTEGER::ncells !number of cells (files in flist)
  INTEGER,DIMENSION(13)::statb
  CHARACTER(100)::filelist,str,locfile
  CHARACTER(100),DIMENSION(:),ALLOCATABLE::cells !array of file names
  CHARACTER(100)::strint,strunw,stramp,strcc
  REAL,DIMENSION(:,:,:),ALLOCATABLE::coh,phase,phase_s,temp3
  REAL,DIMENSION(:,:),ALLOCATABLE::amp,dat,mask,amp_s,mask_s,temp2,stack
  REAL,DIMENSION(:),ALLOCATABLE::temp1
  COMPLEX,DIMENSION(:,:,:),ALLOCATABLE::cpx
  REAL,DIMENSION(:,:),ALLOCATABLE::Tm,deltime,Tminv,ident
  REAL,DIMENSION(:),ALLOCATABLE::Bperp,timedeltas,refphase
  INTEGER::r_ref(1000),az_ref(1000),nlocs !reference pixel locations
  REAL,DIMENSION(:,:,:),ALLOCATABLE::velocity
  INTEGER,DIMENSION(1:2)::maskShape,ampShape
  INTEGER,DIMENSION(1:3)::phaseShape

  !executions

  !flist: list of unwrapped files
  !nfiles: number of files in flist
  !nslcs: number of slc files
  !len: width of the files
  IF(iargc().lt.6)THEN
     WRITE(*,*)'usage: sbas_locations unwlist nunwfiles nslcs len looks locationsfile'
     STOP
  END IF

  CALL getarg(1,filelist)
  CALL getarg(2,str)
  READ(str,*)ncells !number of files
  CALL getarg(3,str)
  READ(str,*)nslc !number of slcs
  CALL getarg(4,str)
  READ(str,*)nr !width of files
  CALL getarg(5,str)
  READ(str,*)looks !number of looks
  call getarg(6,locfile)
  print *,'locfile: ',locfile

  ALLOCATE(cells(ncells))

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

!  get reference locations
  open(21,file=locfile)
  do i=1,1000
     read(21,*,end=99)r_ref(i),az_ref(i)
  end do
99 nlocs=i-1
  close(21)
  print *,'locations read in: ',nlocs

!  ALLOCATE(coh(nr,naz,ncells))
  ALLOCATE(phase(nr,naz,ncells))
  ALLOCATE(refphase(ncells))
  ALLOCATE(amp(nr,naz))
  ALLOCATE(dat(2*nr,naz))
  ALLOCATE(temp1(2*nr*naz))
!  coh(:,:,:)=0
  phase(:,:,:)=0
  amp(:,:)=0
  dat(:,:)=0
  temp1(:)=0

  !read in correlations
!!$  DO i=1,ncells
!!$     strint=cells(i)
!!$     strcc=Replace_Text(strint,'.unw','.cc')
!!$     OPEN(UNIT=21,FILE=strcc,FORM='unformatted',ACCESS='direct',RECL=2*nr*naz*4,CONVERT='LITTLE_ENDIAN')  
!!$     READ(21,rec=1)dat
!!$     CLOSE(21)
!!$     coh(:,:,i)=dat((nr+1):2*nr,:)
!!$  END DO
!!$  PRINT*,'Correlations read in, nr, naz: ',nr,naz

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
     stramp=Replace_Text(stramp,'.reg','')   !!  use older amp files
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

  !multilooks
  nr_s=FLOOR(REAL(nr/looks))
  naz_s=FLOOR(REAL(naz/looks))
  r_ref=r_ref/looks
  az_ref=az_ref/looks
  ALLOCATE(mask_s(nr_s,naz_s))
  ALLOCATE(amp_s(nr_s,naz_s))
  ALLOCATE(phase_s(nr_s,naz_s,ncells))
  IF(looks==1) THEN
     mask_s=mask
     amp_s=amp
     phase_s=phase
  ELSE
     mask_s(:,:)=0
     amp_s(:,:)=0
     phase_s(:,:,:)=0

     ALLOCATE(temp2(looks,looks))
     ALLOCATE(temp3(looks,looks,ncells))
     DO i=1,nr_s
        DO j=1,naz_s
           temp2=mask((i-1)*looks+1:i*looks,(j-1)*looks+1:j*looks)
           mask_s(i,j)=ANINT(mean2d(temp2,looks,looks))
           temp2=amp((i-1)*looks+1:i*looks,(j-1)*looks+1:j*looks)
           amp_s(i,j)=ANINT(mean2d(temp2,looks,looks))
           temp3=phase((i-1)*looks+1:i*looks,(j-1)*looks+1:j*looks,:)
           phase_s(i,j,:)=SUM(SUM(temp3,2),1)/looks/looks
        END DO
     END DO
  END IF

!Save amp_s, mask_s, phase_s in separate files
  OPEN(11,FILE='amp_s',FORM='unformatted',ACCESS='direct',RECL=nr_s*naz_s*4)
  WRITE(11,rec=1)amp_s
  CLOSE(11)
  OPEN(12,FILE='mask_s',FORM='unformatted',ACCESS='direct',RECL=nr_s*naz_s*4)
  WRITE(12,rec=1)mask_s
  CLOSE(12)
!!$  reclphase = nr_s*naz_s*ncells*4
!!$  print *,'phase_s recl ',reclphase
!!$  OPEN(13,FILE='phase_s',FORM='unformatted',ACCESS='direct',RECL=reclphase)
!!$  WRITE(13,rec=1)phase_s
!!$  CLOSE(13)

!Save dimensions of matrices
  ampShape=SHAPE(amp_s)
  maskShape=SHAPE(mask_s)
  phaseShape=SHAPE(phase_s)
  OPEN(14,FILE='dimensions',STATUS='replace')
  WRITE(14,*)ampShape(1),ampShape(2),maskShape(1),maskShape(2),phaseShape(1),phaseShape(2),phaseShape(3)
  CLOSE(14)

!Save nr_s, naz_s, looks, nslc, ncells parameters in one file
  OPEN(15,FILE='parameters',STATUS='replace')
  WRITE(15,*) nr_s, naz_s, looks, nslc, ncells
  CLOSE(15)

!From tsxtime and tsxbaseline
!Create an array with data from Tm.out
  !ALLOCATE(temp1(ncells*(nslc-1)))
  ALLOCATE(Tm(ncells,nslc-1))
  OPEN(24,FILE='Tm.out',STATUS='old')
  do k=1,ncells
     READ(24,*,IOSTAT=stat)(Tm(k,kk),kk=1,nslc-1)
  end do
  CLOSE(24)
  !print *,Tm
  !DEALLOCATE(temp1)

!Save the Bperp.out into an array
  ALLOCATE(Bperp(ncells))
  OPEN(25,FILE='Bperp.out',STATUS='old')
  READ(25,*,IOSTAT=stat)Bperp
  CLOSE(25)

!Create an array with data from deltime.out
  ALLOCATE(temp1(ncells*4))
  ALLOCATE(deltime(ncells,4))
  OPEN(26,FILE='deltime.out',STATUS='old')
  READ(26,*,IOSTAT=stat)temp1
  DO r=1,ncells
     DO c=1,4
        deltime(r,c)=temp1((r-1)*4+c)
     END DO
  END DO
  CLOSE(26)
  DEALLOCATE(temp1)

!!Create an array with data from timedeltas.out
!  ALLOCATE(timedeltas(ncells))
!  OPEN(27,FILE='timedeltas.out',STATUS='old')
!  READ(27,*,IOSTAT=stat)timedeltas
!  CLOSE(27)

  PRINT*,'Data loaded'

!SBAS least squares
!at each pixel, solve for velocity at (nslc-1) time interval
  ALLOCATE(velocity(nr_s,naz_s,nslc-1))
  velocity(:,:,:)=0
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
  
! compute reference phase
  refphase=0.
  do i=1,nlocs
     refphase=refphase+phase_s(r_ref(i),az_ref(i),:)/nlocs
  end do

  DO j=1,naz_s
     DO i=1,nr_s
        temp1=phase_s(i,j,:)-refphase(:) !phase_s(r_ref,az_ref,:)
!!!!        temp1=phase_s(i,j,:)
        temp1=temp1(:)
        velocity(i,j,:)=MATMUL(Tminv,temp1) 
     END DO
  END DO
  DEALLOCATE(temp1)
  !Write velocity matrix into file
  OPEN(28,FILE='velocity',FORM='unformatted',ACCESS='direct',RECL=nr_s*naz_s*(nslc-1)*4)
  WRITE(28,rec=1)velocity


!Create stack to plot images (using MATLAB)
  ALLOCATE(stack(nr_s,naz_s))
  stack=SUM(phase_s,3)
  !Write stack matrix into file
  OPEN(29,FILE='stack',FORM='unformatted',ACCESS='direct',RECL=nr_s*naz_s*4)
  WRITE(29,rec=1)stack

  CLOSE(28)
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
