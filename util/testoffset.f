      character*60 str
      complex b(10303),bb(10303)
      open(21,file='q',access='direct',recl=10303*8)
      open(22,file='qq',access='direct',recl=10303*8)

      call getarg(1,str)
      read(str,*)ipoff
      call getarg(2,str)
      read(str,*)lineoff

      do line=1,3000
         read(21,rec=line)b
         do k=1,10303-ipoff
            bb(k)=b(k+ipoff)
         end do
         write(22,rec=line+lineoff)bb
      end do

      end
