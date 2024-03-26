!
!
!  findrefpoints - find candidate reference points from stack of unw and cc files
!
!    take some looks for better coherence estimates
!    find points with high correlation and minimal motion
!
!    logic modified 18NOV21 to take gaps into account
!

implicit none
character*300 unwlist, unwfile(100000), intfile(100000), ampfile(100000), unw, cc, str
complex, allocatable :: ampstack(:,:,:), amp(:,:)
complex, allocatable :: intstack(:,:,:), int(:,:)
real, allocatable :: ccstack(:,:,:), locs(:,:), ccpoint(:)
integer nx, ny, i, nigrams, k, kk, ii, jj, mx, my, looks, nrefs
real amp1, amp2, thresh
logical, allocatable :: lgvalid(:),lgthresh(:)

looks=4
thresh=0.6

if (iargc().lt.3)then
   print *,'Usage: findrefpoints unwlist nx ny <thresh=0.5>'
   stop
end if

call getarg(1,unwlist)
call getarg(2,str)
read(str,*)nx
call getarg(3,str)
read(str,*)ny
thresh=0.5
if (iargc().ge.4)then
   call getarg(4,str)
   read(str,*)thresh
end if

! get list of interferograms
open(21,file=unwlist)
do i=1,100000
   read(21,*,end=11)str
   k=index(str,'.unw')
   intfile(i)=str(1:k-1)//'.int'
   ampfile(i)=str(1:k-1)//'.amp'
!   print *,intfile(i),ampfile(i)
end do
11 nigrams=i-1
close(21)

!  array allocations
mx=nx/looks
my=ny/looks
print *,'Interferograms: ',nigrams,mx,my
allocate (int(nx,ny), amp(nx,ny), intstack(mx,my,nigrams), ampstack(mx,my,nigrams))
allocate (ccstack(mx*2,my,nigrams), locs(nx,ny))
allocate (ccpoint(nigrams), lgvalid(nigrams), lgthresh(nigrams))

! create multilooked stack
intstack=complex(0.,0.)
ampstack=complex(0.,0.)
locs=0
do i=1,nigrams
   open(21,file=intfile(i),access='direct',recl=nx*ny*8)
   read(21,rec=1)int
   close(21)
   open(21,file=ampfile(i),access='direct',recl=nx*ny*8)
   read(21,rec=1)amp
   close(21)
   ! multilook by looks
   do ii=1,mx
      do jj=1,my
         intstack(ii,jj,i)=sum(int(ii*looks-(looks-1):ii*looks,jj*looks-(looks-1):jj*looks))
         amp1=sum(real(amp(ii*looks-(looks-1):ii*looks,jj*looks-(looks-1):jj*looks))**2)
         amp2=sum(imag(amp(ii*looks-(looks-1):ii*looks,jj*looks-(looks-1):jj*looks))**2)
         ampstack(ii,jj,i)=complex(sqrt(amp1),sqrt(amp2))
         ccstack(ii,jj,i)=amp1*amp2
         if (amp1.le.1.e-10.or.amp2.le.1.e-10)then
            ccstack(ii+mx,jj,i)=0.
         else
            ccstack(ii+mx,jj,i)=cabs(intstack(ii,jj,i))/sqrt(amp1*amp2)
         end if
      end do
   end do
end do

kk=0
if(kk.eq.1)then
   open(99,file='ccstack',recl=mx*my*nigrams*8,access='direct')
   write(99,rec=1)ccstack
   close(99)
   open(99,file='intstack',recl=mx*my*nigrams*8,access='direct')
   write(99,rec=1)intstack
   close(99)
   open(99,file='ampstack',recl=mx*my*nigrams*8,access='direct')
   write(99,rec=1)ampstack
   close(99)
end if

!  which pixels have all cc > thresh?
open(31,file='ref_locs')
nrefs=0
do ii=1,mx
   do jj=1,my
      ccpoint=ccstack(ii+mx,jj,:)  ! timeseries of cc for this pixel (looks x looks)
      lgvalid=ccpoint<1.e-3  ! flag invalid cc elements
      if (all(lgvalid)) then  ! are all points invalid?
         k=0
      else
!         lgthresh=ccpoint>thresh ! flag where cc > threshold
!      print *,ii,jj,all(lgvalid),lgvalid
         k=1  ! first flag the point as possibly valid
         do kk=1,nigrams
            if (ccpoint(kk)>1.e-3 .and. ccpoint(kk)<thresh)k=0
         end do
      end if
!      print '(a,i4,i4,i4,100f5.2)' ,'loc cc k ',ii,jj,k,ccpoint
      if (k.eq.1) then  ! if all cc's are above threshold mark as a reference point
         locs(ii*looks-looks/2-1:ii*looks-looks/2+1,jj*looks-looks/2-1:jj*looks-looks/2+1)=1.
         write(31,*)ii*looks-looks/2,jj*looks-looks/2
         nrefs=nrefs+1
         !print *,'Candidate: ',ii,jj,sum(ccpoint(:))/nigrams,nrefs
      end if
   end do
end do
!      lgthresh=ccpoint>thresh  ! flag nonzero cc elements
!      print *,'valid',lgvalid
!      print *,all(lgvalid)
!      print *,'tghreshhold',lgthresh
!      print *,all(lgthresh)
!      !print *,thresh,maxval(ccpoint),minval(ccpoint)
      !print *,lg
      ! we want points where all inteferograms exceed threshold cc and cc is nonzero
!      k=1  ! point valid flag
!      if(.not.all(lgvalid))k=0
!      do i=1,nigrams ! loop over all averaged cc's at each point
!         if(lgvalid(i).and.(ccpoint(i).lt.thresh))k=0
!      end do
!      if (k.eq.1)then
!         locs(ii*looks-looks/2-1:ii*looks-looks/2+1,jj*looks-looks/2-1:jj*looks-looks/2+1)=1.
!         write(31,*)ii*looks-looks/2,jj*looks-looks/2
!         nrefs=nrefs+1
         !print *,'Candidate: ',ii,jj,sum(ccpoint(:))/nigrams,nrefs
!      end if
!   end do
!end do
close(31)
print *,'Found ',nrefs,' reference points'

open(99,file='locs',access='direct',recl=nx*8)
do i=1,ny
   write(99,rec=i)cabs(amp(:,i)),locs(:,i)
end do
close(99)

end

