      complex a(1024),b(1024),crv

      open(21,file='q1',access='direct',form='unformatted',recl=1024*8)
      open(22,file='q2',access='direct',form='unformatted',recl=1024*8)

      do line=1,1024
         do j=1,1024
            c=abs(gausrv(iseed))
            phase=j/10.*10.*2+line/20.*20.*3.
            a(j)=crv(iseed)+2.
            b(j)=c*cmplx(cos(phase),sin(phase))
c            a(j)=cmplx(1.,0.)
c            b(j)=cmplx(cos(phase),sin(phase))
         end do
         write(21,rec=line)a
         write(22,rec=line)b
      end do

      end

      complex function crv(iseed)
      crv=cmplx(gausrv(iseed),gausrv(iseed))
      return
      end

      function gausrv(iseed)

      gausrv=0.0
      do i=1,12
      gausrv=gausrv+rand()
      end do
      gausrv=gausrv-6.
      return
      end
