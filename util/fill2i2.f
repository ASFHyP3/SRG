	program fill2i2
	
	parameter (nx=1280,ny=700)
	integer*2 d(nx,ny),dd(nx,ny)
	real val(4),dist(4)
	character*30 f1,f2
	
	type '(a,$)',' Input file ? '
	accept '(a)',f1
	
	type '(a,$)',' Output file ? '
	accept '(a)',f2
	
	type '(a,$)',' Set negative values to zero ? '
	accept '(a)',yn

	open(21,file=f1,form='unformatted',access='direct',recl=nx*ny*2)
	open(22,file=f2,form='unformatted',access='direct',recl=nx*ny*2)
c  read in file	
	read(21,rec=1)d

	if(yn.eq.'y')then
	   type *,' setting negative values to zero.'
	   do i=1,nx
	      do j=1,ny
		 if(d(i,j).le.0)d(i,j)=0
	      end do
	   end do
	end if
c  loop over lines
	do i=2,ny-1
	if(mod(i,32).eq.0)type *,i
c  loop over pixels
		do j=2,nx-1
c  is it a zero pixel
			if(d(j,i).eq.0)then
				do k=1,4
					val(k)=0
					dist(k)=0
				end do
c  scan up
				do k=i,1,-1
					if(d(j,k).ne.0)go to 11
				end do
				go to 10
11				val(1)=d(j,k)
				dist(1)=i-k
10				continue
c  scan down
				do k=i,ny
					if(d(j,k).ne.0)go to 21
				end do
				go to 20
21				val(2)=d(j,k)
				dist(2)=k-i
20				continue
c  scan left
				do k=j,1,-1
					if(d(k,i).ne.0)go to 31
				end do
				go to 30
31				val(3)=d(k,i)
				dist(3)=j-k
30				continue
c  scan right
				do k=j,nx
					if(d(k,i).ne.0)go to 41
				end do
				go to 40
41				val(4)=d(k,i)
				dist(4)=k-j
40				continue
c  get weighted sum
				wgt=0
				sum=0
				do k=1,4
					if(dist(k).gt.0.1)then
						wgt=wgt+1./dist(k)
						sum=sum+val(k)/dist(k)
					end if
				end do
				if(wgt.ne.0.0)dd(j,i)=sum/wgt
c				type *,dist
c				type *,val
c				type *,sum,wgt
			else
				dd(j,i)=d(j,i)
			end if
		end do
	end do
c  copy back to d
	do i=1,ny
		do j=1,nx
			d(j,i)=dd(j,i)
		end do
	end do
	type *,'Filtering pass complete'

	write(22,rec=1)d
	end

				
	
