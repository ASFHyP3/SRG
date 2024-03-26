c     subset an mph image

      implicit none
      complex in(100000),out(100000)
      integer  i,j,len,iac,idn,k,irec,kac,kdn
      character*80 a_input,a_output,c

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
 
      open(unit=10,file=a_input,status='old',access='direct',form='unformatted',
     +     recl=8*len)
      open(unit=11,file=a_output,status='unknown',access='direct',
     +     form='unformatted',recl=kac*8)

      irec=0
      do i=idn,idn+kdn-1
         read(10,rec=i,err=99) (in(j), j=1,len)
         k=0
         do j=iac,iac+kac-1
            k=k+1
            out(k) = in(j)
         end do       
         irec=irec+1
         write(11,rec=irec) (out(j),j=1,kac)
      end do
99    print *,'Copied ',irec,' lines of ',kac,' pixels.'

      end
