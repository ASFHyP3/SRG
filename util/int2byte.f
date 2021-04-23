	integer*2 in(16384)
        integer statb(12)
	byte b(16384)
        integer hist(0:1000),hmin,hmax
	character*30 fin,fout,str
        character*1 y

	if(iargc().lt.4)then
	   write(*,*)'usage: topo2byte ini2file outbytefile len ignoreneg(y/n)'
	   stop
	end if
	call getarg(4,y)
	call getarg(3,str)
	read(str,*)npix
	call getarg(2,fout)
	call getarg(1,fin)

	open(21,file=fin,form='unformatted',status='old',
     +    access='direct',recl=npix*2)
	open(22,file=fout,form='unformatted',status='unknown',
     +    access='direct',recl=npix)
c	call stat(fin,statb)
c	nlines=statb(8)/2/npix
c	write(*,*)statb(8),nlines
	nlines=1000000

c  first calculate histogram
        hmin=32000
        hmax=-32000
        do i=1,nlines,16
           read(21,rec=i,err=10)(in(k),k=1,npix)
           do j=1,npix
              if(y.eq.'y'.or.y.eq.'Y')then
                 if(in(j).gt.0)then
                    hmax=max(hmax,in(j))
                    hmin=min(hmin,in(j))
                 end if
                 else

                 if(in(j).ne.-32000)then
                    hmax=max(hmax,in(j))
                    hmin=min(hmin,in(j))
                 end if
              end if
           end do
        end do
 10	write(*,*)'hmin, hmax= ',hmin,hmax
        range=hmax-hmin
        do i=0,1000
           hist(i)=0
        end do
        do i=1,nlines,32
           read(21,rec=i,err=20)(in(k),k=1,npix)
           do j=1,npix
              k=(in(j)-hmin)/range*1000.
              if(k.ge.1000)k=1000
              if(k.le.0)k=0
              hist(k)=hist(k)+1
           end do
        end do
c  type out histogram
 20	write(*,*)hist
c  get mean,top 1%, bottom 1%
        ipts=0
        a=0.
        do i=0,1000
           ipts=ipts+hist(i)
           a=a+i*hist(i)
        end do
        amean=a/ipts/1000*range+hmin
        write(*,*)'mean= ',amean,a/ipts
        write(*,*)'For the following, entering 0.01 will remove 1%.'
        write(*,'(a)')' Fraction to eliminate at top and bottom ? '
        read(*,*)frc
        i0=0
        do i=0,1000
           i0=i0+hist(i)
           if(float(i0).ge.ipts*frc)go to 1
        end do
 1      bottom=i/1000.*range+hmin
        i1=0
        do i=1000,0,-1
           i1=i1+hist(i)
           if(float(i1).ge.ipts*frc)go to 2
        end do
 2      top=i/1000.*range+hmin
        write(*,*)'bottom, top= ',bottom,top

	write(*,'(a)')' Enter desired bottom, top values: '
	read(*,*)bottom,top
        scale=256./(top-bottom)
        
	do i=1,nlines
                if(mod(i,64).eq.0)write(*,*)i
		read(21,rec=i,err=30)(in(k),k=1,npix)
		do j=1,npix
                   a=(in(j)-bottom)*scale
		   if(a.le.0.0)a=0.
			b(j)=iand(min(255,int(a)),255)
		end do
		write(22,rec=i)(b(k),k=1,npix)
	end do
 30	continue

	end
