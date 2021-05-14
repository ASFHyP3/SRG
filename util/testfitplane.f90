
  real in(100,100),out(100,100)

  do i=1,100
     do j=1,100
        in(i,j)=i*2+j+4.5
     end do
  end do

  call fitplane(in,out,100,100)

  open(21,file='testfitplane.out',access='direct',recl=100*100*4)
  write(21,rec=1)out
  close(21)

end program
