c  fillsub - subroutine to fill holes in image, i2 format assumed

	subroutine fillsub(array,npix,lines)

	parameter (maxsiz=4096)
	integer*2 array(npix,*)
	integer*2 d(maxsiz,maxsiz)
	real val(4),dist(4)

c       read in file, copy phases into array d
	do i=1,lines
	   do j=1,npix
	      d(j,i)=array(j,i)
	   end do
	end do

c       loop over lines
	do i=2,nlines-1
!	   if(mod(i,32).eq.0)print *,i
c       loop over pixels
	   do j=2,npix-1
	      iflag=1		!grow to edge flag
c       is it a zero pixel
	      if(d(j,i).eq.0)then
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
		 do k=i,nlines
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
		 do k=j,npix
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
		    if(wgt.ne.0.0)then
		       array(j,i)=sum/wgt
		    end if
c       print *,dist
c       print *,val
c       print *,sum,wgt
		 end if
	      else
		 array(j,i)=d(j,i)
	      end if
	   end do
	end do

	end

				
	
