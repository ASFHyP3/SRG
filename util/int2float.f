	integer in(16384)
	real b(16384)
	character*30 fin,fout,str

	if(iargc().lt.3)then
	   write(*,*)'usage: int2float i4file floatfile len <scale=1>'
	   stop
	end if

	call getarg(3,str)
	read(str,*)npix
	call getarg(2,fout)
	call getarg(1,fin)
	scale=1.
	if(iargc().ge.4)then
	   call getarg(4,str)
	   read(str,*)scale
	end if

	open(21,file=fin,form='unformatted',status='old',
     +    access='direct',recl=npix*4)
	open(22,file=fout,form='unformatted',status='unknown',
     +    access='direct',recl=npix*4)

	nlines=1000000
        
	do i=1,nlines
                if(mod(i,256).eq.0)write(*,*)i
		read(21,rec=i,err=30)(in(k),k=1,npix)
		do j=1,npix
                   b(j)=in(j)*scale
		end do
		write(22,rec=i)(b(k),k=1,npix)
	end do
 30	continue

	end
