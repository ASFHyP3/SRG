!c nbymmht -- average an mht file n looks across, m looks down

	real a(16384,32), b(16384),sum(16384)
	character*60 str,fin,fout
        integer*8 filelen, nbytes

	if(iargc().lt.4)then
	   print *,'usage: mhtlooks infile outfile width looksac <looksdn=looksac>'
	   stop
	end if

	call getarg(1, fin)
	call getarg(2, fout)
	call getarg(3,str)
	read(str,*) len
	call getarg(4,str)
	read(str,*) navg
	mavg = navg
	if(iargc() .gt. 4) then
	   call getarg(5,str)
	   read(str,*) mavg
	end if

	print *, 'num samples out ', len/navg
	open(21,file=fin,access='direct',recl=len*mavg*8)
	open(31,file=fout,access='direct',recl=len/navg*8)
        nbytes = filelen(fin)
        print *,'filesize outlines ',nbytes,nbytes/8/len/mavg

!c  loop over line number

	do i=1,nbytes/8/len/mavg
           if(mod(i,64).eq.0) print *,i*mavg
	   read(21,rec=i)((a(k,kk),k=1,len*2),kk=1,mavg)

	   do j = 1 , len/navg*2
	      sum(j)=0.
	   end do

           do j=1,len/navg

!c  read in and average

	      do k = 1 , navg
		 do l = 1, mavg
		    sum(j)=sum(j)+a((j-1)*navg+k,l)
		    sum(j+len/navg)=sum(j+len/navg)+a((j-1)*navg+k+len,l)
		 end do
	      end do
	      
	   end do

	   alooks=navg*mavg
	   do j = 1 , len/navg*2
	      b(j)=sum(j)/alooks
           end do

	   write(31,rec=i)(b(k),k=1,len/navg*2)

	end do

	end
