c  signedshort2float -- convert signed int16 to floats
	integer*2 in(16384)
        integer statb(13),stat
	real out(8192)
	character*30 fin,fout,str

	if(iargc().lt.3)then
	   print *,'usage: signedshort2float ini2file outfile len <nlines>'
	   stop
	end if

	call getarg(1,fin)
	call getarg(2,fout)
	call getarg(3,str)
	read(str,*)npix

	ierr=stat(fin,statb)
	nlines=statb(8)/2/npix
	print *,'bytes, lines: ',statb(8),nlines

	if(iargc().ge.4)then
           call getarg(4,str)
	   read(str,*)nlines
        end if

	open(21,file=fin,form='unformatted',status='old',
     +    access='direct',recl=npix*2)
	open(22,file=fout,form='unformatted',status='unknown',
     +    access='direct',recl=npix*4)

	do i=1,nlines
                if(mod(i,256).eq.0)print *,i
		read(21,rec=i,err=30)(in(k),k=1,npix)
		do j=1,npix
                   out(j)=float(in(j))
		end do

		write(22,rec=i)(out(k),k=1,npix)
	end do
 30	continue

	end
