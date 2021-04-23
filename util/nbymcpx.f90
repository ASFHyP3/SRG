!c nbymcpx -- average a cpx file n looks across, m looks down
	integer*4 N_RA
	parameter (N_RA = 32000)
	integer*4 initdk, ioread, iowrit, nread, nwr, lun
	integer*4 fdin1, fdin2, fdout
	integer*4 isum(N_RA), width, wido
	complex*8     a(N_RA*64),b(N_RA),sum(N_RA)
	character*120 name
	
	i = iargc()
	if(i .lt. 4) then
	   write(*,*)'usage: nbymcpx infile outfile width acavg [dnmavg]'
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
	write(*,*)'num samples out ', wido

!c  loop over line number

	do i=1,100000
           if(mod(i,64).eq.0) write(*,*)i*mavg
	   nread = ioread(fdin2,a,nbin*8)
	   if(nread .ne. nbin*8) stop 'end of file'

	   do j = 1 , wido
	      sum(j)=0.
	   end do

           do j=1,wido

!c  read in and average

	      do k = 1 , navg
		 do l = 1, mavg
		    ioff = (j-1)*navg+k+(l-1)*width
!c		    write(*,*)k,l,ioff,a2(ioff),a2(ioff+width)
		       sum(j)      = sum(j) + a(ioff)
		 end do
	      end do
	      
	   end do

	   do j = 1 , wido
	      b(j)=sum(j)/navg/mavg
           end do

           nwr = iowrit(fdout,b,nbout*8)

	end do
 99     continue
	end
