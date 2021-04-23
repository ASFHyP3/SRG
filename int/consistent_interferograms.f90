!
!
!  consistent_interferograms - find interferograms that agree with short baseline solution
!
!

implicit none
character*8 date1, date2
character*300 str, unwlist, unwfile(10000), geofile(10000)
real*4, allocatable ::  unw(:,:)
real*4 displacement(10000),netdisplacement,interferogram,tol
integer nx, ny, i, nigrams, j, ix, iy, nslcs, index1, index2, i1, i2, y, n
integer histy(100),histn(100),hista(100),histbin
real*8 jdlist(10000)

histy=0
histn=0
hista=0

if (iargc().lt.5)then
   print *,'Usage: consistent_interferograms unwlist len lines ix iy <tolerance-rad>'
   stop
end if

call getarg(1,unwlist)
call getarg(2,str)
read(str,*)nx
call getarg(3,str)
read(str,*)ny
call getarg(4,str)
read(str,*)ix
call getarg(5,str)
read(str,*)iy
tol=5;
if(iargc().ge.6)then
   call getarg(6,str)
   read(str,*)tol
end if

! get list of interferograms
open(21,file=unwlist)
do i=1,10000
   read(21,*,end=11)unwfile(i)
end do
11 nigrams=i-1
print *,'Interferograms: ',nigrams
close(21)

! get list of geofiles
open(21,file='geolist')
do i=1,10000
   read(21,'(a)',end=13)str
   geofile(i)=str
!   print *,i,str,' ',geofile(i)
end do
13 nslcs=i-1
print *,'Geofiles: ',nslcs
close(21)

! get list of julian days
open(21,file='jdlist')
do i=1,10000
   read(21,*,end=14)jdlist(i)
!   print *,i,str,' ',geofile(i)
end do
14 nslcs=i-1
print *,'Julian days: ',nslcs
close(21)
!print *,jdlist(1:nslcs)

!  array allocations
allocate (unw(nx*2,ny))

!  read in displacement solution at point of interest
open(21,file='displacement',access='stream')
displacement(1)=0.
do i=2,10000  !  first displacement should be zero
   read(21,end=12)unw
   displacement(i)=unw(nx+ix,iy)
end do
12 nslcs=i-1
print *,'Displacement steps: ',nslcs
close(21)

!print *,displacement(1:nslcs)
open(31,file='timeseries.out')
do i=1,nslcs
write(31,*)displacement(i)
end do

! loop through each interferogram
y=0
n=0
open(31,file='consistent')
open(32,file='inconsistent')
do i=1,nigrams
   str=trim(unwfile(i))
   !  open interferogram
   open(21,file=str,access='stream')
   read(21)unw
   close(21)

!   print *,trim(str)
   date1=str(1:8)
   date2=str(10:17)
!   print *,date1,' ',date2
!   ! get the timestep associated with each date
   do j=1,nslcs
      i1=index(geofile(j),date1)
      i2=index(geofile(j),date2)
      if(i1.ne.0)index1=j
      if(i2.ne.0)index2=j
!      print *,j,i1,i2
   end do
!   print *,index1,index2
   netdisplacement=displacement(index2)-displacement(index1)
   interferogram=unw(nx+ix,iy)
   histbin=nint(sngl(jdlist(index2)-jdlist(index1))/6.)
   hista(histbin)=hista(histbin)+1

!   print *,'disp igram ',netdisplacement,interferogram
   if(abs(netdisplacement-interferogram).le.tol)then
      y=y+1
      histy(histbin)=histy(histbin)+1
      write(31,*)trim(str),sngl(jdlist(index2)-jdlist(index1))
      print *,'Within tolerance: ',trim(str),' error= ',abs(netdisplacement-interferogram) &
           ,'baseline= ',sngl(jdlist(index2)-jdlist(index1))
   else
      n=n+1
      histn(histbin)=histn(histbin)+1
      write(32,*)trim(str),sngl(jdlist(index2)-jdlist(index1))
!      print *,'Interferogram out of tolerance: ',trim(str),' error= ',abs(netdisplacement-interferogram)
   end if
end do
close(31)
close(32)
open(31,file='hists')
do i=1,100
   write(31,*)i*6,hista(i),histy(i),histn(i)
end do
close(31)
open(31,file='histy')
do i=1,100
   write(31,*)i*6,histy(i)
end do
close(31)
open(31,file='histn')
do i=1,100
   write(31,*)i*6,histn(i)
end do
close(31)
open(31,file='hista')
do i=1,100
   write(31,*)i*6,hista(i)
end do
close(31)

print *,'Number within, out of tolerance: ',y,n
end

