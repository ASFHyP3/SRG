!c  write a tiff file

      subroutine writetiff(bytearray,nx,ny,filename)

      integer*1 bytearray(nx,ny,4)
      integer*1, allocatable :: buf(:)
      character*60 filename,fout
      integer*1 f(60),ioffbyte(4)
      equivalence (f,fout),(ioffset,ioffbyte)
      integer*4 ioffset,ioff
      integer*1 b255, bb(4)
      integer*4 i255256256256

      equivalence (bb,i255256256256)

      b255=-1
      bb(1)=-1
      bb(2)=0
      bb(3)=0
      bb(4)=0

!  allocate array
      allocate(buf(nx*ny*4+4096))
      fout=filename
      do i=1,60
         if(f(i).eq.32)exit
         if(f(i).le.0)exit
      end do
      ifile=i-1

!  header stuff first
      buf(1)=4*16+13
      buf(2)=buf(1)   ! first two bytes are 'll' for little endian
      buf(3)=0
      buf(4)=2*16+10

      ioffset=nx*ny*4+8  !offset to tags past data
      buf(5)=ioffbyte(4)!iand(ioffset,255*256*256*256)/256*256*256
      buf(6)=ioffbyte(3)!iand(ioffset,255*256*256)/256/256
      buf(7)=ioffbyte(2)!iand(ioffset,255*256)/256
      buf(8)=ioffbyte(1)!iand(ioffset,255)

!  now image data

      do i=1,ny
         do j=1,nx
            ioff=(i-1)*nx+j-1

               buf(ioff*4+1+8)=bytearray(j,i,1)
               buf(ioff*4+2+8)=bytearray(j,i,2)
               buf(ioff*4+3+8)=bytearray(j,i,3)
               buf(ioff*4+4+8)=bytearray(j,i,4)

         end do
      end do

!  ifd fields - each 12 bytes long
      ioff=nx*ny*4+8
      numtags=15  ! 15 tags
      buf(ioff+1)=0
      buf(ioff+2)=numtags

      ioff=ioff+2

      buf(ioff+1)=1  ! image width
      buf(ioff+2)=0
      buf(ioff+3)=0
      buf(ioff+4)=3
      buf(ioff+5)=0
      buf(ioff+6)=0
      buf(ioff+7)=0
      buf(ioff+8)=1
      buf(ioff+9)=iand(nx,255*256)/256
      buf(ioff+10)=iand(nx,255)
      buf(ioff+11)=0
      buf(ioff+12)=0

      ioff=ioff+12

      buf(ioff+1)=1  ! image height
      buf(ioff+2)=1
      buf(ioff+3)=0
      buf(ioff+4)=3
      buf(ioff+5)=0
      buf(ioff+6)=0
      buf(ioff+7)=0
      buf(ioff+8)=1
      buf(ioff+9)=iand(ny,255*256)/256
      buf(ioff+10)=iand(ny,255)
      buf(ioff+11)=0
      buf(ioff+12)=0

      ioff=ioff+12

      buf(ioff+1)=1  ! bits per sample
      buf(ioff+2)=2
      buf(ioff+3)=0
      buf(ioff+4)=3
      buf(ioff+5)=0
      buf(ioff+6)=0
      buf(ioff+7)=0
      buf(ioff+8)=4
      ioffset=nx*ny*4+194
      buf(ioff+9)=ioffbyte(4)!iand(n,255*256*256*256)/256/256/256
      buf(ioff+10)=ioffbyte(3)!iand(n,255*256*256)/256/256
      buf(ioff+11)=ioffbyte(2)!iand(n,255*256)/256
      buf(ioff+12)=ioffbyte(1)!iand(n,255)

      ioff=ioff+12

      buf(ioff+1)=1  ! compression flag
      buf(ioff+2)=3
      buf(ioff+3)=0
      buf(ioff+4)=3
      buf(ioff+5)=0
      buf(ioff+6)=0
      buf(ioff+7)=0
      buf(ioff+8)=1
      buf(ioff+9)=0
      buf(ioff+10)=1
      buf(ioff+11)=0
      buf(ioff+12)=0

      ioff=ioff+12

      buf(ioff+1)=1  ! photometric interpolation
      buf(ioff+2)=6
      buf(ioff+3)=0
      buf(ioff+4)=3
      buf(ioff+5)=0
      buf(ioff+6)=0
      buf(ioff+7)=0
      buf(ioff+8)=1
      buf(ioff+9)=0
      buf(ioff+10)=2
      buf(ioff+11)=0
      buf(ioff+12)=0

      ioff=ioff+12

      buf(ioff+1)=1  ! strip offset
      buf(ioff+2)=16+1
      buf(ioff+3)=0
      buf(ioff+4)=4
      buf(ioff+5)=0
      buf(ioff+6)=0
      buf(ioff+7)=0
      buf(ioff+8)=1
      buf(ioff+9)=0
      buf(ioff+10)=0
      buf(ioff+11)=0
      buf(ioff+12)=8

      ioff=ioff+12

      buf(ioff+1)=1  ! orientation
      buf(ioff+2)=16+2
      buf(ioff+3)=0
      buf(ioff+4)=3
      buf(ioff+5)=0
      buf(ioff+6)=0
      buf(ioff+7)=0
      buf(ioff+8)=1
      buf(ioff+9)=0
      buf(ioff+10)=1
      buf(ioff+11)=0
      buf(ioff+12)=0

      ioff=ioff+12

      buf(ioff+1)=1  ! sample per pixel
      buf(ioff+2)=16+5
      buf(ioff+3)=0
      buf(ioff+4)=3
      buf(ioff+5)=0
      buf(ioff+6)=0
      buf(ioff+7)=0
      buf(ioff+8)=1
      buf(ioff+9)=0
      buf(ioff+10)=4
      buf(ioff+11)=0
      buf(ioff+12)=0

      ioff=ioff+12


      buf(ioff+1)=1  ! rows per strip
      buf(ioff+2)=16+6
      buf(ioff+3)=0
      buf(ioff+4)=3
      buf(ioff+5)=0
      buf(ioff+6)=0
      buf(ioff+7)=0
      buf(ioff+8)=1
      buf(ioff+9)=iand(ny,255*256)/256
      buf(ioff+10)=iand(ny,255)
      buf(ioff+11)=0
      buf(ioff+12)=0

      ioff=ioff+12

      buf(ioff+1)=1  ! strip byte count
      buf(ioff+2)=16+7
      buf(ioff+3)=0
      buf(ioff+4)=4
      buf(ioff+5)=0
      buf(ioff+6)=0
      buf(ioff+7)=0
      buf(ioff+8)=1
      ioffset=nx*ny*4
      buf(ioff+9)=ioffbyte(4)!iand(n,255*256*256*256)/256/256/256
      buf(ioff+10)=ioffbyte(3)!iand(n,255*256*256)/256/256
      buf(ioff+11)=ioffbyte(2)!iand(n,255*256)/256
      buf(ioff+12)=ioffbyte(1)!iand(n,255)

      ioff=ioff+12

      buf(ioff+1)=1  ! minimum sample flag
      buf(ioff+2)=16+8
      buf(ioff+3)=0
      buf(ioff+4)=3
      buf(ioff+5)=0
      buf(ioff+6)=0
      buf(ioff+7)=0
      buf(ioff+8)=4
      n=nx*ny*4+202
!      buf(ioff+9)=iand(n,255*256*256*256)/256/256/256
      ioffset=n
      buf(ioff+9)=ioffbyte(4)!iand(n,i255256256256)/256/256/256
      buf(ioff+10)=ioffbyte(3)!iand(n,255*256*256)/256/256
      buf(ioff+11)=ioffbyte(2)!iand(n,255*256)/256
      buf(ioff+12)=ioffbyte(1)!iand(n,255)

      ioff=ioff+12

      buf(ioff+1)=1  ! maximum sample flag
      buf(ioff+2)=16+9
      buf(ioff+3)=0
      buf(ioff+4)=3
      buf(ioff+5)=0
      buf(ioff+6)=0
      buf(ioff+7)=0
      buf(ioff+8)=4
      ioffset=nx*ny*4+210
      buf(ioff+9)=ioffbyte(4)!iand(n,255*256*256*256)/256/256/256
      buf(ioff+10)=ioffbyte(3)!iand(n,255*256*256)/256/256
      buf(ioff+11)=ioffbyte(2)!iand(n,255*256)/256
      buf(ioff+12)=ioffbyte(1)!iand(n,255)

      ioff=ioff+12

      buf(ioff+1)=1  ! planar configuration
      buf(ioff+2)=16+12
      buf(ioff+3)=0
      buf(ioff+4)=3
      buf(ioff+5)=0
      buf(ioff+6)=0
      buf(ioff+7)=0
      buf(ioff+8)=1
      buf(ioff+9)=0
      buf(ioff+10)=1
      buf(ioff+11)=0
      buf(ioff+12)=0

      ioff=ioff+12

      buf(ioff+1)=1  ! sample format
      buf(ioff+2)=5*16+3
      buf(ioff+3)=0
      buf(ioff+4)=3
      buf(ioff+5)=0
      buf(ioff+6)=0
      buf(ioff+7)=0
      buf(ioff+8)=4
      ioffset=nx*ny*4+218
      buf(ioff+9)=ioffbyte(4)!iand(n,255*256*256*256)/256/256/256
      buf(ioff+10)=ioffbyte(3)!iand(n,255*256*256)/256/256
      buf(ioff+11)=ioffbyte(2)!iand(n,255*256)/256
      buf(ioff+12)=ioffbyte(1)!iand(n,255)

      ioff=ioff+12

      buf(ioff+1)=1  ! extra samples
      buf(ioff+2)=5*16+2
      buf(ioff+3)=0
      buf(ioff+4)=3
      buf(ioff+5)=0
      buf(ioff+6)=0
      buf(ioff+7)=0
      buf(ioff+8)=1
      buf(ioff+9)=0
      buf(ioff+10)=1
      buf(ioff+11)=0
      buf(ioff+12)=0

      ioff=ioff+12

      buf(ioff+1)=0  !end of directory
      buf(ioff+2)=0
      buf(ioff+3)=0
      buf(ioff+4)=0

      ioff=ioff+4

      buf(ioff+1)=0  !bits per channel
      buf(ioff+2)=8
      buf(ioff+3)=0  
      buf(ioff+4)=8
      buf(ioff+5)=0  
      buf(ioff+6)=8
      buf(ioff+7)=0  
      buf(ioff+8)=8

      ioff=ioff+8

      buf(ioff+1)=0  !minimum value
      buf(ioff+2)=0
      buf(ioff+3)=0  
      buf(ioff+4)=0
      buf(ioff+5)=0  
      buf(ioff+6)=0
      buf(ioff+7)=0  
      buf(ioff+8)=0

      ioff=ioff+8

      buf(ioff+1)=0  !maximum value
      buf(ioff+2)=b255
      buf(ioff+3)=0  
      buf(ioff+4)=b255
      buf(ioff+5)=0  
      buf(ioff+6)=b255
      buf(ioff+7)=0  
      buf(ioff+8)=b255

      ioff=ioff+8

      buf(ioff+1)=0  !sample format 1=unsigned integers
      buf(ioff+2)=1
      buf(ioff+3)=0  
      buf(ioff+4)=1
      buf(ioff+5)=0  
      buf(ioff+6)=1
      buf(ioff+7)=0  
      buf(ioff+8)=1

      ioff=ioff+8


      open(21,file=filename(1:ifile),access='direct',recl=ioff,status='replace')
      write(21,rec=1)(buf(k),k=1,ioff)
      close(21)

      return
      end
