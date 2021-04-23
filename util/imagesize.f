      character*200 f
      integer*8 n,length,filelen
      byte b(200)
      equivalence (b,f)

      if(iargc().lt.1)then
         write(*,*)'usage: imagesize file <type>'
         write(*,*)' or    imagesize number-of-bytes n'
         write(*,*)' where type is b (byte) s (short) f (float) or c (complex)'
         write(*,*)' Default complex format.'
         stop
      end if
       
      if(iargc().eq.2)then
         call getarg(2,f)
         if(f(1:1).eq.'n')then
            call getarg(1,f)
            read(f,*)n
            go to 10
         end if
      end if

      call getarg(1,f)
!      print *,f
!      print *,trim(f)
!      print *,b
      length=filelen(f)
      n=length/8
      write(*,*)'File length, bytes, pixels: ',length,n
      if(iargc().ge.2)then
         call getarg(2,f)
         if(f.eq.'f')n=length/4
         if(f.eq.'s')n=length/2
         if(f.eq.'b')n=length
      end if

 10   k=sqrt(float(n))
      do i=k,10*k
         if((n/i)*i.eq.n)write(*,*)'possibility: ',i,'  by  ',n/i
      end do
      end
