c     phasestats - phase statistics of an mph image

      implicit none
      complex in(100000),ph(100000),acc
      integer  i,j,len,iac,idn,k,kac,kdn
      real sum, sumsq, phase, psum, psumsq
      character*80 a_input,a_output,c

      if(iargc().lt.6)then
         print *,'usage: phasestats infile length ac-start dn-start ac-size dn-size'
         stop
      end if
      call getarg(1,a_input)
      call getarg(2,c)
      read(c,*)len
      call getarg(3,c)
      read(c,*)iac
      call getarg(4,c)
      read(c,*)idn
      call getarg(5,c)
      read(c,*)kac
      call getarg(6,c)
      read(c,*)kdn
 
      open(unit=10,file=a_input,status='old',access='direct',form='unformatted',
     +     recl=8*len)

      k=0
      acc=cmplx(0.,0.)
      do i=idn,idn+kdn-1
         read(10,rec=i) (in(j), j=1,len)
         do j=iac,iac+kac-1
            k=k+1
            ph(k) = in(j)
            acc=acc+in(j)
         end do       
      end do

c  average phase stored in normalized accumulator
      acc=acc/cabs(acc)
      print *,'Mean phase: ',atan2(aimag(acc),real(acc))

c  steer all phases to mean, estimate standard deviation
      sum=0.
      sumsq=0.
      psum=0.
      psumsq=0.
      do i=1,k
         psum=psum+cabs(ph(i))**2
         psumsq=psumsq+cabs(ph(i))**4
         ph(i)=ph(i)*conjg(acc)
         phase=atan2(aimag(ph(i)),real(ph(i)))
         sum=sum+phase
         sumsq=sumsq+phase**2
      end do
      print *,'Zero mean, standard deviation: ',sum/k, sqrt(sumsq/k-(sum/k)**2)
      print *,'Power mean, std: ',psum/k,sqrt(psumsq/k-(psum/k)**2)

      end
