      subroutine dephase(a,n)
      complex a(n,n),csuma,csumd

c  estimate and remove phase carriers in a complex array
      csuma=cmplx(0.,0.)
      csumd=cmplx(0.,0.)
c  across first
      do i=1,n-1
         do j=1,n
            csuma=csuma+a(i,j)*conjg(a(i+1,j))
         end do
      end do
c  down next
      do i=1,n
         do j=1,n-1
            csumd=csumd+a(i,j)*conjg(a(i,j+1))
         end do
      end do

      pha=atan2(aimag(csuma),real(csuma))
      phd=atan2(aimag(csumd),real(csumd))
      type *,'average phase across, down: ',pha,phd

c  remove the phases
      do i=1,n
         do j=1,n
            a(i,j)=a(i,j)*cmplx(cos(pha*i+phd*j),sin(pha*i+phd*j))
         end do
      end do

      return
      end
