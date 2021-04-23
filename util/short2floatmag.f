c  short2floatmag -- convert int16 i,q data to a magnitude image
	integer*2 in(16384)
        integer statb(13),stat
	real out(8192)
	character*30 fin,fout,str

	if(iargc().lt.3)then
	   write(*,*)'usage: short2floatmag ini2file outfile len <nlines>'
	   stop
	end if

	call getarg(1,fin)
	call getarg(2,fout)
	call getarg(3,str)
	read(str,*)npix

	ierr=stat(fin,statb)
	nlines=statb(8)/4/npix
	write(*,*)statb(8),nlines

	if(iargc().ge.4)then
           call getarg(4,str)
	   read(str,*)nlines
        end if

	open(21,file=fin,form='unformatted',status='old',
     +    access='direct',recl=npix*4)
	open(22,file=fout,form='unformatted',status='unknown',
     +    access='direct',recl=npix*4)

	do i=1,nlines
                if(mod(i,256).eq.0)write(*,*)i
		read(21,rec=i,err=30)(in(k),k=1,npix*2)
		do j=1,npix
                   out(j)=sqrt(float(in(j*2-1))**2+float(in(j*2))**2)
		end do

		write(22,rec=i)(out(k),k=1,npix)
	end do
 30	continue

	end
