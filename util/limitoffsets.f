c  limitoffsets - limit offsets to fixed hi and low values

      parameter (m=10000)
      character*60 file,str
      integer i1(m),i2(m)
      real a1(m),a2(m),snr(m)

      if(iargc().lt.3)then
         print *,'Usage: limitoffsets offset-file High Low'
         stop
      end if

      call getarg(1,file)
      call getarg(2,str)
      read(str,*)value_hi
      call getarg(3,str)
      read(str,*)value_low

!c read in file
      open(21,file=file)
      do k=1,m
         read(21,*,end=10)i1(k),a1(k),i2(k),a2(k),snr(k)
      end do
 10   k=k-1
      print *,k,' points read'
      close(21)
      call unlink(file)

      open(21,file=file,status='new')
      do i=1,k
         if(a1(i).ge.value_low.and.a1(i).le.value_hi)then
            if(a2(i).ge.value_low.and.a2(i).le.value_hi)then
               write(21,*)i1(i),a1(i),i2(i),a2(i),snr(i)
            end if
         end if
      end do
      close(21)

      end

