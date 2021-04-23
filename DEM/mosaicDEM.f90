PROGRAM mosaicDEM   ! version for non-repeated-line copernicus files
  IMPLICIT none
  !specifications
  INTEGER::nhoriz,nvert,nfiles,stat,i,j,k,w,xl,xh,yl,yh,lat,long,found,widthmin,widthmax,h,jreverse
  INTEGER(KIND=8)::reclen,m,n
  CHARACTER(300)::str,filename,file,fmt,demrscparams
  CHARACTER(300),DIMENSION(:),ALLOCATABLE::demfiles
  character(len=18):: ic
  character*1 byteswap
  INTEGER(KIND=2),DIMENSION(:),ALLOCATABLE::temp,width,length
  real*8, allocatable :: xspacing(:),yspacing(:)
  INTEGER(KIND=2),DIMENSION(:,:),ALLOCATABLE::demfile,Tdemfile,dat,updem
  REAL::step,xnew,xsample,frac
  integer::ix, widthrow,heightrow,heightmax,heightmin
  real*8 :: dlat,dlon,xstep,ystep,q,xspacingmax

  !executions
  !horiz: number of tiles along horizontal axis
  !vert: number of tiles along vertical axis
  !width: number of samples in 1arcsec/3arcsec .dem files
  !step size of dem file
  !demfiles: filename of .dem files list
  IF(iargc().lt.3)THEN
     WRITE(*,*)'usage: mosaicDEM demrscparams-file horiz-size vert-size'
     STOP
  END IF
  call getarg(1,demrscparams)
  call getarg(2,str)
  READ(str,*)nhoriz
  CALL getarg(3,str)
  READ(str,*)nvert
  nfiles=nhoriz*nvert !number of files
  byteswap='n'

  ALLOCATE(demfiles(nfiles), width(nfiles), xspacing(nfiles))
  allocate(length(nfiles), yspacing(nfiles))
  OPEN(UNIT=1,FILE=demrscparams,STATUS='old')
  widthmin=1000000
  widthmax=0
  heightmin=1000000
  heightmax=0
  do i=1,nfiles
     READ(1,'(A)')demfiles(i)
     read(1,*)width(i)
     read(1,*)length(i)
     read(1,*)xspacing(i)
     read(1,*)yspacing(i)
     widthmin=min(widthmin,width(i))
     widthmax=max(widthmax,width(i))
     heightmin=min(heightmin,length(i))
     heightmax=max(heightmax,length(i))
 !    xspacingmax=max(xspacingmax,xspacing(i))
  end do
  CLOSE(1)
!  PRINT*,'demfiles: ',(trim(demfiles(i))//' ',i=1,nfiles)
!  print *,width
!  print *,xspacing
!  print *,'width min max ',widthmin,widthmax

  open(22,file='updem',access='stream')!'direct',recl=widthmax*nhoriz*h*2)
!  loop over longitude in inner loop, latitude in  outer loop
  do j=1,nvert  ! loop over rows, latitude
     !  size of this row
     jreverse=nvert-j ! note this is zero based
     widthrow=0
     heightrow=0
     do i=1,nhoriz
        widthrow=max(widthrow,width(i+jreverse*nhoriz))
        heightrow=max(heightrow,length(i+jreverse*nhoriz))
     end do
     if (widthrow.eq.0)then
        widthrow=widthmax
        heightrow=heightmax
     end if
!     print *,widthrow,heightrow, ' widthrow'
     !w=width(1+jreverse*nhoriz)
     !h=length(1+jreverse*nhoriz)
     w=widthrow
     h=heightrow
     allocate(demfile(widthrow*nhoriz,heightrow))  !  size of full dem row
     allocate(dat(widthrow,heightrow)) ! size of a single tile
!     print *,'j,w,h,size(demfile,1),size(demfile,2),size(dat,1),size(dat,2)'
!     print *,j,w,h,size(demfile,1),size(demfile,2),size(dat,1),size(dat,2)
     !  read in a row, since width is only latitude dependent
     !  note that demfiles.txt was bottom to top, and we want to write top to bottom
     do i=1,nhoriz
        open(21,file=demfiles(i+jreverse*nhoriz),access='stream')
        read(21,end=10)dat
        print *,'dem file found, i,j,jreverse: ',trim(demfiles(i+jreverse*nhoriz)),i,j,jreverse
!        print *,' sum ',sum(dat)
        close(21)
        demfile(1+(i-1)*w:w+(i-1)*w,:)=dat(:,:)
        go to 11
10      continue
        print *,'File ',trim(demfiles(i+jreverse*nhoriz)),' not found, using zeros, i,j,jreverse=',i,j,jreverse
        demfile(1+(i-1)*widthrow:widthrow+(i-1)*widthrow,:)=0
11      continue
     end do

!     print *,'starting resampling',widthrow,heightrow,w,h,j
     !  resample the row to finer sampling
     xspacingmax=1.d0/widthmax
     allocate(updem(widthmax*nhoriz,heightrow))
!     print *,'updem size ',size(updem,1),size(updem,2)
     do i=1,widthmax*nhoriz
!        print *,i
        xnew=(i-1)*xspacingmax  ! x location, degrees, in new coords
        xsample=xnew*widthrow  ! x location, pixels, in old coords
        ix=int(xsample)
        frac=xsample-ix
!        print *,i,ix,frac,xnew,xsample
        if(ix.le.0)ix=0
        if(ix.ge.widthmax*nhoriz-1)ix=widthmax*nhoriz-1
        updem(i,:)=nint(demfile(ix+1,:)*(1.-frac)+demfile(ix+2,:)*frac)
     end do
     updem(widthmax*nhoriz,:)=updem(widthmax*nhoriz-1,:)  ! end column has no valid data so fill
     write(22)updem
     deallocate(demfile,dat,updem)
  end do

END PROGRAM mosaicDEM
