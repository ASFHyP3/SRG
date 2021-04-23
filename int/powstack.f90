!c    powstack - stack a set of slcs in power
!c        

program powstack

      implicit none
      integer*4    width,length
      complex*8, allocatable :: array(:,:)
      real*4, allocatable :: power(:,:)
      character*300 outfile, listfile, fin(1000)
      character*100 str
      integer*4 i, j, intshift, nfiles

!c    **********  input parameters  **********
!c    
!c    enter input parameters
!c    
      if(iargc().lt.4)then
         print *,'Usage: powstack cpxfilelist outfile width length'
         call exit
      end if

      call getarg(1,listfile)
      call getarg(2,outfile)
      call getarg(3,str)
      read(str,*)width
      call getarg(4,str)
      read(str,*)length
 
!c    allocate arrays
      allocate(array(width,length), power(width,length))

!c read in the input file list
      open(21,file=listfile)
      do i=1,10000
         read(21,*,end=11)fin(i)
      end do
11    close(21)
      nfiles=i-1
      print *,'Input files: ',nfiles

!c  loop over the input files
      power=0.
      do i=1,nfiles
         open(21,file=fin(i),access='direct',recl=width*length*8)
         read(21,rec=1)array
         close(21)

         power=power+cabs(array)**2
      end do
      power=power/nfiles
      array=cmplx(sqrt(power),0.)

      open(21,file=outfile,access='direct',recl=width*length*8)
      write(21,rec=1)array
      close(21)
      
    end program powstack
