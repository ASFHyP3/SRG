c  create a test data set for the upsample program

      integer*2 d(2000,2000)

      do i=1,2000
         do j=1,2000
            d(j,i)=i+2*j
         end do
      end do

      open(21,file='testupsample.dat',access='direct',recl=2000*2000*2)
      write(21,rec=1)d
      close(21)
      end
