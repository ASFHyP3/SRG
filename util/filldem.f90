!  fill in srtm holes
program filldem

  integer*2, allocatable :: d(:,:),dd(:,:)
  real val(4),dist(4)
  character*300 f1,f2,str
  integer*8 filelen

  if(iargc().lt.3)then
     print *,'Usage: filldem infile outfile len'
     stop
  end if

  call getarg(1,f1)
  call getarg(2,f2)
  call getarg(3,str)
  read(str,*)len

  lines=filelen(f1)/len/2
  print *,'len lines ',len,lines

  allocate (d(len,lines),dd(len,lines))

  open(21,file=f1,form='unformatted',access='direct',recl=len*lines*2)
  open(22,file=f2,form='unformatted',access='direct',recl=len*lines*2)

!  read in file	
  read(21,rec=1)d

!  loop over lines
  do i=2,lines-1
     if(mod(i,1000).eq.0)print *,i
     !c  loop over pixels
     do j=2,len-1
        !c  is it a zero pixel
        if(d(j,i).lt.-30000)then
           val=0.
           dist=0.
           !c  scan up
           do k=i,1,-1
              if(d(j,k).ge.-30000)go to 11
           end do
           go to 10
11         val(1)=d(j,k)
           dist(1)=i-k
10         continue
           !c  scan down
           do k=i,lines
              if(d(j,k).ge.-30000)go to 21
           end do
           go to 20
21         val(2)=d(j,k)
           dist(2)=k-i
20         continue
           !c  scan left
           do k=j,1,-1
              if(d(k,i).ge.-30000)go to 31
           end do
           go to 30
31         val(3)=d(k,i)
           dist(3)=j-k
30         continue
           !c  scan right
           do k=j,len
              if(d(k,i).ge.-30000)go to 41
           end do
           go to 40
41         val(4)=d(k,i)
           dist(4)=k-j
40         continue
           !c  get weighted sum
           wgt=0
           sum=0
           do k=1,4
              if(dist(k).gt.0.1)then
                 wgt=wgt+1./dist(k)
                 sum=sum+val(k)/dist(k)
              end if
           end do
           if(wgt.ge.1.e-4)dd(j,i)=sum/wgt
           !print *,j,i,dist,val,sum,wgt
        else
           dd(j,i)=d(j,i)
        end if
     end do
  end do
  !c  copy back to d
  do i=1,lines
     do j=1,len
        d(j,i)=dd(j,i)
     end do
  end do
  print *,'Filtering pass complete'

  write(22,rec=1)d
end program filldem



