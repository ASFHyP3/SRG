      subroutine cfft1d(n,c,dir)

      integer*4  n, dir, ier
      complex*8    c(*)

      if(dir .eq. 0) return

      call four1(c,n,dir)

      return
      end

