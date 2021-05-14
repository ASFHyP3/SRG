PROGRAM makeDEM
  IMPLICIT none
  !specifications
  INTEGER::nhoriz,nvert,nfiles,stat,i,j,k,w,xl,xh,yl,yh,lat,long
  INTEGER(KIND=8)::reclen,m,n
  CHARACTER(300)::str,filename,file,fmt
  CHARACTER(300),DIMENSION(:),ALLOCATABLE::demfiles
  character*1 byteswap
  INTEGER(KIND=2),DIMENSION(:),ALLOCATABLE::temp
  INTEGER(KIND=2),DIMENSION(:,:),ALLOCATABLE::demfile,Tdemfile,dat
  REAL::step
  !executions
  !horiz: number of tiles along horizontal axis
  !vert: number of tiles along vertical axis
  !width: number of samples in 1arcsec/3arcsec .dem files
  !step size of dem file
  !demfiles: filename of .dem files list
  IF(iargc().lt.6)THEN
     WRITE(*,*)'usage: makeDEM lat long horiz vert width step demfiles'
     STOP
  END IF

  CALL getarg(1,str)
  READ(str,*)lat  ! starting latitude upper left
  CALL getarg(2,str)
  READ(str,*)long  ! starting longitude upper left
  CALL getarg(3,str)
  READ(str,*)nhoriz
  CALL getarg(4,str)
  READ(str,*)nvert
  nfiles=nhoriz*nvert !number of files
  CALL getarg(5,str)
  READ(str,*)w  !  each input DEM file size
  CALL getarg(6,str)
  READ(str,*)step
  CALL getarg(7,filename)
  byteswap='n'
  if(iargc().ge.8)then
     call getarg(8,str)
     read(str,'(a1)')byteswap
  end if
!  PRINT*,nfiles,' swap bytes: ',byteswap

  ALLOCATE(demfiles(nfiles))
  OPEN(UNIT=1,FILE=filename,STATUS='old')
  READ(1,'(A)',end=10,IOSTAT=stat)demfiles
10 continue
  CLOSE(1)
  PRINT*,'demfiles',(trim(demfiles(i)),i=1,nfiles)

  ALLOCATE(demfile(nvert*(w-1)+1,nhoriz*(w-1)+1))
  ALLOCATE(temp(w*w))
  ALLOCATE(dat(w,w))
  demfile=0

  DO j=1,nvert
     DO i=1,nhoriz
        dat(:,:)=0
        file=trim(demfiles((j-1)*nhoriz+i))
!        PRINT*,file
        if (byteswap.eq.'n')then
           OPEN(UNIT=21,FILE=file,err=20, STATUS='old', FORM='unformatted',ACCESS='direct',RECL=w*w*16,CONVERT='LITTLE_ENDIAN')
        else
           OPEN(UNIT=21,FILE=file,err=20, STATUS='old', FORM='unformatted',ACCESS='direct',RECL=w*w*16,CONVERT='BIG_ENDIAN')
        end if

        READ(21,rec=1,err=20)temp
        go to 21
20      print *,'file ',trim(file), 'not found, using zeroes'
        temp=0
21        continue !PRINT*,trim(file),' first element: ',temp(1)
        DO k=1,w
           dat(k,:)=temp((k-1)*w+1:k*w)
        END DO

     
        xl=(i-1)*(w-1)+1
        xh=(i)*(w-1)+1
 
        yl=(nvert-j)*(w-1)+1
        yh=(nvert-j+1)*(w-1)+1
     
!        PRINT*,xl,xh,yl,yh

        demfile(yl:yh,xl:xh)=dat

        CLOSE(21)
     END DO
  END DO

  WHERE(demfile<0) demfile=0

  m=SIZE(demfile,1)
  n=SIZE(demfile,2)
!  PRINT*,m
!  PRINT*,n

  ALLOCATE(Tdemfile(n,m))
  Tdemfile=TRANSPOSE(demfile)
  DEALLOCATE(demfile)

  m=SIZE(Tdemfile,1)
  n=SIZE(Tdemfile,2)
!  PRINT*,m,m/3601
!  PRINT*,n,n/3601

  filename='elevation.dem'
  INQUIRE(iolength=reclen)Tdemfile
  PRINT*,filename,' has record length, i*2 elements: ',reclen,m*n
  OPEN(22,FILE=filename,FORM='unformatted',ACCESS='stream',STATUS='replace')
  WRITE(22)Tdemfile
  CLOSE(22)

  filename='elevation.dem.rsc'
  print *,'DEM params: ',m,n,long,lat,step,-step
  OPEN(23,FILE=filename,STATUS='replace')
  WRITE(23,'(a13,i10)')'WIDTH',m
  WRITE(23,'(a13,i10)')'FILE_LENGTH',n
  WRITE(23,'(a13,i10)')'X_FIRST',long
  WRITE(23,'(a13,i10)')'Y_FIRST',lat
  WRITE(23,'(a13,e18.8)')'X_STEP',step
  WRITE(23,'(a13,e18.8)')'Y_STEP',-step
  WRITE(23,*)'X_UNIT            degrees'
  WRITE(23,*)'Y_UNIT            degrees'
  WRITE(23,*)'Z_OFFSET          0'
  WRITE(23,*)'Z_SCALE           1'
  WRITE(23,*)'PROJECTION        LL'
  CLOSE(23)

END PROGRAM makeDEM
