!c  tilefile - create a more square mph file from a 1 tile wide strip

      complex*8, allocatable :: in(:,:)
      character*100 infile, outfile, str

      if (iargc().lt.5)then
         print *,'usage: tilefile infile outfile tiles_across len lines'
         stop
      end if

      call getarg(1,infile)
      call getarg(2,outfile)
      call getarg(3,str)
      read(str,*)nx
      call getarg(4,str)
      read(str,*)len
      call getarg(5,str)
      read(str,*)lines

      allocate(in(len,lines))

      open(21,file=infile,access='direct',recl=len*lines*8)
      open(22,file=outfile,access='direct',recl=len*8)

      do i=1,10000
         read(21,rec=i,err=100)in

         do j=1,lines  ! loop over records
            jrec=((i-1)/nx)*lines*nx+mod(i-1,nx)+1+(j-1)*nx
            write(22,rec=jrec)in(:,j)
         end do
      end do
100   continue
      end
