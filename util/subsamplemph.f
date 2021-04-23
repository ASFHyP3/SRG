c     subsample an mph image
      program subsamplemph

      implicit none
      complex in(100000),out(100000)
      integer  i,j,len,iac,idn,k,irec
      character*80 a_input,a_output,c

      if(iargc().lt.5)then
         type *,'usage: subsamplemph infile outfile length factor-ac factor-dn'
         stop
      end if
      call getarg(3,c)
      read(c,*)len
      call getarg(4,c)
      read(c,*)iac
      call getarg(5,c)
      read(c,*)idn
      call getarg(1,a_input)
      call getarg(2,a_output)

      open(unit=10,file=a_input,status='old',access='direct',form='unformatted',
     +     recl=8*len)
      open(unit=11,file=a_output,status='unknown',access='direct',
     +     form='unformatted',recl=(len/iac)*8)

      irec=0
      do i=1,100000,idn
         read(10,rec=i,err=99) (in(j), j=1,len)
         k=0
         do j=1,len,iac
            k=k+1
            out(k) = in(j)
         end do       
         irec=irec+1
         write(11,rec=irec) (out(j),j=1,len/iac)
      end do

 99   end
