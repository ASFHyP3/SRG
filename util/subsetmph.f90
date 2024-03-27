!c     subset an mph image

      implicit none
      complex*8, allocatable ::  in(:),out(:)
      integer  i,j,len,iac,idn,k,irec,kac,kdn
      character*360 a_input,a_output,c

      if(iargc().lt.7)then
         print *,'usage: subset infile outfile length ac-start dn-start ac-size dn-size'
         stop
      end if
      call getarg(1,a_input)
      call getarg(2,a_output)
      call getarg(3,c)
      read(c,*)len
      call getarg(4,c)
      read(c,*)iac
      call getarg(5,c)
      read(c,*)idn
      call getarg(6,c)
      read(c,*)kac
      call getarg(7,c)
      read(c,*)kdn
 
!c  allocate space
      allocate (in(len), out(kac))

      open(unit=10,file=a_input,status='old',access='direct',form='unformatted',recl=8*len)
      open(unit=11,file=a_output,status='unknown',access='direct',form='unformatted',recl=kac*8)

      irec=0
      do i=idn,idn+kdn-1
         read(10,rec=i,err=99)in
         out=in(iac:iac+kac-1)
         irec=irec+1
99       if(irec.le.0)then
            print *,'irec,i ',irec,i
            print *,a_input
            print *,a_output
         end if
         write(11,rec=irec)out
      end do
      print *,'Copied ',irec,' lines of ',kac,' pixels.'

      end
