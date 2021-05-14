!c  tilefilemht - create a more square mht file from a 1 tile wide strip

      real*4, allocatable :: in(:,:),out(:,:)
      character*100 infile, outfile, str

      if (iargc().lt.3)then
         print *,'usage: tilefilemht infile outfile panels_across len lines <ampscale>'
         stop
      end if

      call getarg(1,infile)
      call getarg(2,outfile)
      call getarg(3,str)
      read(str,*)nx
      print *,'Tiles across: ',nx
      call getarg(4,str)
      read(str,*)len
      call getarg(5,str)
      read(str,*)lines
      ampscale=1.
      if(iargc().ge.6)then
         call getarg(6,str)
         read(str,*)ampscale
      end if

      allocate (in(len*2,lines),out(len*2*nx,lines))

      open(21,file=infile,access='direct',recl=len*2*lines*4)
      open(22,file=outfile,access='direct',recl=len*2*nx*lines*4)

      out=0.
      do i=1,10000,nx
         do k=0,nx-1
            read(21,rec=i+k,err=100)in  !  read in ith stacked file
            !print *,'read ',i
            do j=1,lines  ! loop over records
               out(1+k*len:len+k*len,j)=in(1:len,j)*ampscale
               out(1+k*len+nx*len:len+k*len+nx*len,j)=in(len+1:len*2,j)
            end do
         end do
         !print *,'computed'
         write(22,rec=(i-1)/nx+1)out
         out=0.
      end do
100   print *,'no more input files',i
      stop
      end
