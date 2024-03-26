!*****
!
!  psinterp algorithm from Wang and Chen (2022) translated to f90
!
  use omp_lib

  implicit none
  character*300 intfile, psfile, outfile, str
  integer nps, ibox, len, rdmin, rdmax, i, j, neigh, lines, k, jj, ii, irmin, irmax
  real alpha
  complex*8, dimension(:,:), allocatable :: igram, out
  integer*1, dimension(:,:), allocatable :: psmask
  integer boxsize,nvalid
  integer, allocatable :: iindex(:),jindex(:)
  real, allocatable :: r(:),r2(:)
  integer statb(13),fstat,ierr
  complex*8, allocatable :: cphase(:)
  complex*8 csum

  if(iargc().lt.4)then
     print *,'Usage: psinterp intfile psfile outfile len'
     call exit
  end if

  ! some hardwired params to be refined later
  nps=20  ! number of ps to use for spiral
  ibox=101  ! max box size
  alpha=1. ! exponent parameter for distance weighting
  rdmin=3  ! min scan distance
  rdmax=50 ! max scan distance

  call getarg(1,intfile)
  call getarg(2,psfile)
  call getarg(3,outfile)
  call getarg(4,str)
  read(str,*)len

  ! compute the scan indices array
  irmax=50
  boxsize=2*irmax+1
  allocate (iindex(boxsize*boxsize),jindex(boxsize*boxsize),r(boxsize*boxsize))
  nvalid=boxsize*boxsize
!  call spiralscan2(boxsize,iindex,jindex,r)!,nvalid)
!  do i=1,20
!     print *,i,iindex(i),jindex(i)
!  end do
!  print *,'Spiral indices set'
  call spiralscan(boxsize,iindex,jindex,r,nvalid)
!  do i=1,nvalid
!     print *,i,iindex(i),jindex(i)
!  end do
!  print *,'Spiral indices set',nvalid
!  if(i.eq.i)call exit
!  call spiralscan2(boxsize,iindex,jindex,r)
!print *,iindex
!print *,jindex

  ! read in int and ps files, open output file
  open(21,file=intfile,access='stream')
  ierr=fstat(21,statb)
  lines=statb(8)/len/8
  allocate (igram(len,lines),psmask(len,lines),out(len,lines))
  out=cmplx(0.,0.)

  read(21)igram
  close(21)
  open(21,file=psfile,access='stream')
  read(21)psmask
  close(21)
!  print *,'data read in'

  ! now start the interpolation

  allocate (r2(nps),cphase(nps))
  !$omp parallel do private(i,j,csum,r2,cphase,neigh,k,ii,jj) &
  !$omp shared(lines,len,iindex,jindex,r,igram,alpha,out)
  do j=1,lines
     do i=1,len
        csum=cmplx(0.,0.)
        r2=0.
        cphase=cmplx(0.,0.)
        neigh=0
        do k=1,nvalid
           ii=i+iindex(k)
           jj=j+jindex(k)
           if(ii.ge.1.and.ii.le.len.and.jj.ge.1.and.jj.le.lines)then  ! in bounds
              if(psmask(ii,jj).ne.0)then
                 neigh=neigh+1
                 if(neigh.gt.nps)go to 11
                 r2(neigh)=r(k)*r(k)
                 cphase(neigh)=igram(ii,jj)/(1.e-12+abs(igram(ii,jj)))
              end if
           end if !  end bounds check
        end do ! end scan of box
11      neigh=neigh-1
        ! sum the phases with weights
!        print *,cphase
        if(neigh.gt.0)then
           do k=1,neigh
              csum=csum+exp(-r2(k)/2/r2(neigh)**alpha)*cphase(k)
           end do
           out(i,j)=abs(igram(i,j))*csum/(1.e-12+abs(csum))
           !print *,'out ',out(i,j),igram(i,j)
!           print *,r2(1:neigh)
!           print *,r2(1:neigh)/r2(neigh)
!           print *,i,j,exp(-r2(1:neigh)/2/r2(neigh))
        end if
     end do
  end do
  !$omp end parallel do

  open(31,file=outfile,access='stream')
  write(31)out
  close(31)


end program

      subroutine spiralscan(boxsize,iindex,jindex,r,nvalid)

        integer boxsize,x,y,n,count,irmin,irmax,p,k,nvalid
        integer*4 iindex(boxsize*boxsize),jindex(boxsize*boxsize)
        real*4  r(boxsize*boxsize)
        integer*1, allocatable :: visited(:,:)

        allocate (visited(boxsize,boxsize))

        irmin=0
        irmax=boxsize/2
        visited(1,1)=-1
        k=1

        do i=1,irmax
           x=i
           y=0
           p=1-i
           if(i.gt.irmin)then
              iindex(k)=i
              jindex(k)=0
              k=k+1
              iindex(k)=-i
              jindex(k)=0
              k=k+1
              iindex(k)=0
              jindex(k)=i
              k=k+1
              iindex(k)=0
              jindex(k)=-i
              k=k+1
           end if
           visited(i+1,1)=-1
           visited(1,i+1)=-1
           flag=0

           do while(x.gt.y)
              if(flag.eq.0)then
                 y=y+1
                 if(p.le.0)then
                    p=p+2*y+1
                 else
                    x=x-1
                    p=p+2*y-2*x+1
                 end if
              else
                 flag=flag-1
              end if
              if(x.lt.y)go to 12
              do while(visited(x-1+1,y+1).eq.0)
                 x=x-1
                 flag=flag+1
              end do
              visited(x+1,y+1)=-1
              visited(y+1,x+1)=-1
              if(i.gt.irmin)then
                 iindex(k)=x
                 jindex(k)=y
                 k=k+1
                 iindex(k)=-x
                 jindex(k)=-y
                 k=k+1
                 iindex(k)=x
                 jindex(k)=-y
                 k=k+1
                 iindex(k)=-x
                 jindex(k)=y
                 k=k+1
                 if(x.ne.y)then
                    iindex(k)=y
                    jindex(k)=x
                    k=k+1
                    iindex(k)=-y
                    jindex(k)=-x
                    k=k+1
                    iindex(k)=y
                    jindex(k)=-x
                    k=k+1
                    iindex(k)=-y
                    jindex(k)=x
                    k=k+1
                 end if
              end if
              if(flag>0)x=x+1
           end do
12         continue
        end do
        nvalid=k-1
        do k = 1, nvalid
           r(k) = sqrt(real(iindex(k))**2.+real(jindex(k))**2.)
        end do

        return
      end subroutine spiralscan


! alternate spiral algorithm using squares intead of circles
      subroutine spiralscan2(boxsize,iindex,jindex,r)

        integer boxsize,x,y,n,count
        integer*4 iindex(boxsize*boxsize),jindex(boxsize*boxsize)
        integer*4, allocatable :: array(:,:),scan(:,:),xx(:)
        real*4  r(boxsize*boxsize)

        allocate (array(boxsize,boxsize),scan(boxsize,boxsize),xx(boxsize))
        do i=1,boxsize
           xx(i)=i-boxsize/2-1
        end do

        x=0
        y=1
        n=0
        count=boxsize;
        do i = 1,count
           x = x + 1
           array(x,y) = n
           n = n + 1
        end do
        do
           count = count  - 1
           do i = 1,count
              y = y + 1
              array(x,y) = n
              n = n + 1
           end do
           do i = 1,count
              x = x - 1
              array(x,y) = n
              n = n + 1
           end do
           if (n > boxsize*boxsize-1) exit
           count = count - 1
           do i = 1,count
              y = y - 1
              array(x,y) = n
              n = n + 1
           end do
           do i = 1,count
              x = x + 1
              array(x,y) = n
              n = n + 1
           end do
           if (n > boxsize*boxsize-1) exit
        end do
        scan=boxsize*boxsize-array
        
!c  create list of indices in scan order
        do i=1,boxsize
           do j=1,boxsize
              iindex(scan(i,j))=xx(i)
              jindex(scan(i,j))=xx(j)
           end do
        end do
        !c   the distance between center point and each scanned pixels
        do k = 1, boxsize*boxsize
           r(k) = sqrt(real(iindex(k))**2.+real(jindex(k))**2.)
        end do

      return
    end subroutine spiralscan2

