	real b(8192)
	real d(4096,4096),dd(4096,4096)
	real val(4),dist(4)
	character*50 f1,f2
	integer istat(12)
	save d,dd

	if(iargc().lt.3)then
	   type *,'usage: fill infile outfile nx'
	   stop
	end if

	call getarg(3,f1)
	read(f1,*)nx
	call getarg(1,f1)
	call getarg(2,f2)
	open(21,file=f1,form='unformatted',access='direct',recl=nx*8)
	open(22,file=f2,form='unformatted',access='direct',recl=nx*8)
	ierr=fstat(21,istat)
	ny=istat(8)/8/nx
	type *,'Lines: ',ny

c       read in file, copy phases into array d
	do i=1,ny
           read(21,rec=i)(b(k),k=1,nx*2)
	   do j=1,nx
	      d(j,i)=b(j+nx)
	      if(abs(b(j)).lt.1.e-20)d(j,i)=0.
	   end do
	end do
	type *,'File loaded.'

c       loop over lines
	do i=2,ny-1
	   if(mod(i,32).eq.0)type *,i
c       loop over pixels
	   do j=2,nx-1
	      iflag=1		!grow to edge flag
c       is it a zero pixel
	      if(d(j,i).eq.0.)then
		 do k=1,4
		    val(k)=0
		    dist(k)=0
		 end do
c       scan up
		 do k=i,1,-1
		    if(d(j,k).ne.0.)go to 11
		 end do
		 iflag=0
		 go to 10
 11		 val(1)=d(j,k)
		 dist(1)=i-k
 10		 continue
c       scan down
		 do k=i,ny
		    if(d(j,k).ne.0.)go to 21
		 end do
		 iflag=0
		 go to 20
 21		 val(2)=d(j,k)
		 dist(2)=k-i
 20		 continue
c       scan left
		 do k=j,1,-1
		    if(d(k,i).ne.0.)go to 31
		 end do
		 iflag=0
		 go to 30
 31		 val(3)=d(k,i)
		 dist(3)=j-k
 30		 continue
c       scan right
		 do k=j,nx
		    if(d(k,i).ne.0.)go to 41
		 end do
		 iflag=0
		 go to 40
 41		 val(4)=d(k,i)
		 dist(4)=k-j
 40		 continue
c       do we grow to this point?
		 if(iflag.eq.1)then
c       get weighted sum
		    wgt=0
		    sum=0
		    do k=1,4
		       if(dist(k).gt.0.1)then
			  wgt=wgt+1./dist(k)
			  sum=sum+val(k)/dist(k)
		       end if
		    end do
		    if(wgt.ne.0.0)dd(j,i)=sum/wgt
c       type *,dist
c       type *,val
c       type *,sum,wgt
		 end if
	      else
		 dd(j,i)=d(j,i)
	      end if
	   end do
	end do
c       copy back to b and write out
	do i=1,ny
	   read(21,rec=i)(b(k),k=1,nx)
	   do j=1,nx
	      b(j+nx)=dd(j,i)
	   end do
	   write(22,rec=i)(b(k),k=1,nx*2)
	end do
	type *,'Filtering pass complete'

	end

				
	
