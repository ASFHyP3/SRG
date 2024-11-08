  integer boxsize
  integer, allocatable :: iindex(:),jindex(:)
  real, allocatable :: r(:)

        irmin=2
        irmax=1
        boxsize=2*irmax+1
        boxsize=3
        !  set up spiral scanning
        allocate (iindex(boxsize*boxsize),jindex(boxsize*boxsize),r(boxsize*boxsize))

        call spiralscan(boxsize,iindex,jindex,r)
        
        print *,iindex
        print *,jindex
        print *,r
        end


      subroutine spiralscan(boxsize,iindex,jindex,r)

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
    end subroutine spiralscan

