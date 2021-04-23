c nbynhgt.f -- average a height file n looks in range, n in azimuth
	integer*4 N_RA
	parameter (N_RA = 5120)
	integer*4 initdk, ioread, iowrit, nread, nwr, lun
	integer*4 fdin1, fdin2, fdout
	integer*4 isum(N_RA), width, wido
	dimension a1(N_RA*64),a2(N_RA*64),b(2*N_RA)
	dimension iind(64,64),jind(64,64)
	character*120 name
	
	i = iargc()
	if(i .lt. 5) then
	   write(6,*)
	1	'usage: nbymmaghgt magfile dtefile outfile width ',
	2	'acavg [dnmavg]'
	   stop
	end if

	call getarg(1, name)
	fdin1 = initdk(lun,name)
	call getarg(2, name)
	fdin2 = initdk(lun,name)
	call getarg(3,name)
	fdout = initdk(lun,name)
	call getarg(4,name)
	read(name,*) width
	call getarg(5,name)
	read(name,*) navg
	mavg = navg
	if(i .gt. 5) then
	   call getarg(6,name)
	   read(name,*) mavg
	end if

	do i = 1 , navg
	   do j = 1 , mavg
	      iind(i,j) = i
	      jind(i,j) = j
	   end do
	end do
	nbin  = 4*width*mavg
	wido  = width/navg
	nbout = 8*wido

c  loop over line number

	do i=1,100000
           if(mod(i,64).eq.0) print *,i*mavg
	   nread = ioread(fdin1,a1,nbin)
	   nread = ioread(fdin2,a2,nbin)
	   if(nread .ne. nbin) stop 'end of file'

	   do j = 1 , 2*wido
	      b(j)=0.
	   end do

	   do j = 1 , wido
	      isum(j)=0
	   end do

           do j=1,wido

c       flag the indicator for bad points

	      do k = 1 , navg
		 do l = 1, mavg
		    ioff = (j-1)*navg+1+iind(k,l)+(jind(k,l)-1)*width
		    if(a1(ioff) .gt. 0.) then
		       isum(j)    = isum(j) + 1
		       b(j)      = b(j) + a1(ioff)
		       b(j+wido) = b(j+wido) + a2(ioff)
		    end if
		 end do
	      end do
	      
	   end do

c       isum is number of good points

	   do j = 1 , wido

              if(isum(j).ge.1)then
		 b(j)=b(j)/isum(j)
		 b(j+wido)=b(j+wido)/isum(j)
              else
		 b(j)=0.0
		 b(j+wido)=0.
              end if
	      
           end do

           nwr = iowrit(fdout,b,nbout)

	end do
 99     continue
	end
