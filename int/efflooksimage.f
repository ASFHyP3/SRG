c  efflooksimage - create an image of effective looks

      implicit none
      complex rinput(100000),eff(10000)
      integer  len,ibox,i,j,k,kk,l
      character*80 a_input,a_output,c
      real sum(10000),sumsq(10000)
      integer n(10000)

      if(iargc().lt.4)then
         print *,'usage: efflooksimage infile outfile size boxsize'
         stop
      end if
      call getarg(1,a_input)
      call getarg(2,a_output)
      call getarg(3,c)
      read(c,*)len
      call getarg(4,c)
      read(c,*)ibox

c   open the input and output files

      open(unit=10,file=a_input,access='direct',form='unformatted',recl=8*len)
      open(unit=11,file=a_output,access='direct',form='unformatted',recl=8*(len/ibox))

      print *
      print *,'Files opened...'
     
c   extract the data for each box, form eff looks estimates

      do i=1,10000
         if(mod(i,100).eq.0)print *,i
         do kk=1,10000
            sum(kk)=0.
            sumsq(kk)=0.
            n(kk)=0
         end do
         do j=1,ibox
            read(10,rec=(i-1)*ibox+j,err=20) (rinput(kk), kk=1,len)
            kk=0
            do k=1,len,ibox
               kk=kk+1
               do l=1,ibox
                  sum(kk)=sum(kk)+cabs(rinput(k-1+l))**2
                  sumsq(kk)=sumsq(kk)+cabs(rinput(k-1+l))**4
                  n(kk)=n(kk)+1
               end do
            end do
         end do
         do kk=1,len/ibox
            sum(kk)=sum(kk)/n(kk)
            sumsq(kk)=sumsq(kk)/n(kk)
            eff(kk)=cmplx(sum(kk)**2/(sumsq(kk)-sum(kk)**2),0.)
         end do
         write(11,rec=i)(eff(k),k=1,len/ibox)
      end do
 20   continue

      end
