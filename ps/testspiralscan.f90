!*****
!
!  psiterp algorithm from Wang and Chen translated to f90
!

  implicit none

  integer boxsize,irmin,irmax,i,nvalid
  integer, allocatable :: iindex(:),jindex(:)
  real, allocatable :: r(:),r2(:)


  ! compute the scan indices array
  irmin=2
  irmax=3
  boxsize=2*irmax+1
  allocate (iindex(boxsize*boxsize),jindex(boxsize*boxsize),r(boxsize*boxsize))
  call spiralscan(boxsize,iindex,jindex,r,nvalid)
  print *,'Spiral indices set'
  do i=1,nvalid
     print *,i,iindex(i),jindex(i)
  end do

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
        return
      end subroutine spiralscan

