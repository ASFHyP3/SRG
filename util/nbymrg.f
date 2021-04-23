c nbymrg -- average an rg file n looks across, m looks down

	complex a(16384,32), b(16384),sum(16384)
	character*60 str,fin,fout

	if(iargc().lt.4)then
	   print *,'usage: nbymrg infile outfile width acavg [dnmavg]'
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

c  loop over line number

	do i=1,100000
           if(mod(i,64).eq.0) print *,i*mavg
	   read(21,rec=i,err=99)((a(k,kk),k=1,len),kk=1,mavg)

	   do j = 1 , len/navg
	      sum(j)=cmplx(0.,0.)
	   end do

           do j=1,len/navg

c  read in and average

	      do k = 1 , navg
		 do l = 1, mavg
		    sum(j)=sum(j)+a((j-1)*navg+k,l)
		 end do
	      end do
	      
	   end do

	   alooks=navg*mavg
	   do j = 1 , len/navg
	      b(j)=sum(j)/alooks
           end do

	   write(31,rec=i)(b(k),k=1,len/navg)

	end do
 99     continue
	end
