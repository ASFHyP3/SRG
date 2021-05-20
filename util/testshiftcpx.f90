      complex b(20,10)
      open(21,file='q',access='direct',recl=20*10*8)

      b=cmplx(0.,0.)
      b(9,5)=cmplx(1.,0.)
      write(21,rec=1)b

      end
