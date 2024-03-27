c nbymi2 -- average an i2 file n looks across, m looks down
	integer*4 N_RA
	parameter (N_RA = 70000)
	integer*4 initdk, ioread, iowrit, nread, nwr, lun
	integer*4 fdin1, fdin2, fdout
	integer*4 isum(N_RA), width, wido
	integer*2 a2(N_RA*64),b(N_RA)
	real*4    sum(N_RA)
	character*120 name
	
	i = iargc()
	if(i .lt. 4) then
	   print *,'usage: nbymi2 infile outfile width acavg [dnmavg]'
	   stop
	end if

	call getarg(1, name)
	fdin2 = initdk(lun,name)
	call getarg(2,name)
	fdout = initdk(lun,name)
	call getarg(3,name)
	read(name,*) width
	call getarg(4,name)
	read(name,*) navg
	mavg = navg
	if(i .gt. 4) then
	   call getarg(5,name)
	   read(name,*) mavg
	end if

	nbin  = width*mavg
	wido  = width/navg
	nbout = wido
	print *, 'num samples out ', wido

c  loop over line number

	do i=1,10000000
!           if(mod(i,1024).eq.0) print *,i*mavg
	   nread = ioread(fdin2,a2,nbin*2)
	   if(nread .ne. nbin*2) stop 'end of file'

	   do j = 1 , wido
	      sum(j)=0.
	   end do

           do j=1,wido

c  read in and average

	      do k = 1 , navg
		 do l = 1, mavg
		    ioff = (j-1)*navg+k+(l-1)*width
c		    print *,k,l,ioff,a2(ioff),a2(ioff+width)
		    val=a2(ioff)
                    sum(j) = sum(j) + val
		 end do
	      end do
	      
	   end do

	   alooks=navg*mavg
	   do j = 1 , wido
	      b(j)=nint(sum(j)/alooks)
           end do

           nwr = iowrit(fdout,b,nbout*2)

	end do
 99     continue
	end
