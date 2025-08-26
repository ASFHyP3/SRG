!
!
!  refpointsfromsim - find candidate reference points from existing similarity calculation
!
!  note: needed amplitude info is in file 'scr'

! Compile as: gfortran -o refpointsfromsim refpointsfromsim.f9

PROGRAM findrefpointssim
  
implicit none
character*300 simfile, str, scrfile, out_reflocs, out_locs
real, allocatable :: locs(:,:), scr(:,:), similarity(:,:)
integer*1, allocatable :: bytemask(:,:)
integer nx, ny, i, k, kk, ii, jj, nrefs, ierr, j
real thresh

if (iargc().lt.3)then
   print *,'Usage: refpointsfromsim simfile scrfile nx ny out_reflocs out_locs <simthresh=0.6>'
   stop
end if

call getarg(1,simfile)
call getarg(2,scrfile)
call getarg(3,str)
read(str,*)nx
call getarg(4,str)
read(str,*)ny
call getarg(5,out_reflocs)
call getarg(6,out_locs)

thresh=0.6
if (iargc().ge.7)then
   call getarg(7,str)
   read(str,*)thresh
end if

ALLOCATE(scr(nx*2,ny),bytemask(nx,ny),similarity(nx,ny))

  ! get similarity
  open(15,file=simfile,access='stream')
  read(15)similarity
  close(15)
  open(15,file=scrfile,access='stream')
  read(15)scr
  close(15)

  allocate (locs(nx,ny))
  open(20,file=out_reflocs)
  open(21,file=out_locs,access='stream')
  bytemask=0
  locs=0
  k=0
  do j=1,ny
     do i=1,nx
        if(similarity(i,j).ge.thresh)then
           bytemask(i,j)=-1
           locs(i,j)=1.
           k=k+1
           write(20,*)i,j
        end if
     end do
     write(21)scr(1:nx,j),locs(:,j)
  end do
  close(20)
  close(21)
  print *,'Ref points found: ',k

end PROGRAM findrefpointssim
