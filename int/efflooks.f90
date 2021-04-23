!c  efflooks - calculate effective looks

      implicit none
      complex rinput(100000)
      integer  len,ibox,i,j,kk,locx,locy
      character*80 a_input,a_output,c
      real sum,sumsq,eff
      integer n

      if(iargc().lt.5)then
         print *,'usage: efflooks infile len locx locy boxsize'
         stop
      end if
      call getarg(1,a_input)
      call getarg(2,c)
      read(c,*)len
      call getarg(3,c)
      read(c,*)locx
      call getarg(4,c)
      read(c,*)locy
      call getarg(5,c)
      read(c,*)ibox

!c   open the input file

      open(unit=10,file=a_input,access='direct',form='unformatted',recl=8*len)

      sum=0.
      sumsq=0.
      n=0
      do i=locy-ibox/2,locy+ibox/2
         read(10,rec=i)(rinput(kk), kk=1,len)
         do j=locx-ibox/2,locx+ibox/2
            sum=sum+cabs(rinput(j))**2
            sumsq=sumsq+cabs(rinput(j))**4
            n=n+1
         end do
      end do
      sum=sum/n
      sumsq=sumsq/n

      eff=sum**2/(sumsq-sum**2)
      print *,'Effective looks: ',eff,'   Average power, amp: ',sum,sqrt(sum)

      end
