!c  phase2cpx - convert real*8 phase to a complex image

      real*8, allocatable :: r8(:)
      complex*8, allocatable :: c(:)
      character*360 filephase,filecpx,str
      integer stat,statb(13)

      if(iargc().lt.3)then
         print *,'Usage:  phase2cpx phasefile cpxfile length'
         stop
      end if

      call getarg(1,filephase)
      call getarg(2,filecpx)
      call getarg(3,str)
      read(str,*)len

      allocate(r8(len))
      allocate(c(len))

      open(22,file=filecpx,access='direct',recl=len*8)
      open(21,file=filephase,access='direct',recl=len*8)

      do i=1,100000
         read(21,rec=i,err=99)r8
         c=cmplx(sngl(cos(r8)),sngl(sin(r8)))
         write(22,rec=i)c
      end do
99    continue
      end
