!  test eigenvalue calcs

  real a(4,4),sing(4),u(4,4)

  a(1,1)=1
  a(1,2)=0.5
  a(1,3)=0.333333
  a(1,4)=0.25
  a(2,1)=0.5
  a(2,2)=1
  a(2,3)=0.666667
  a(2,4)=0.5
  a(3,1)=0.333333
  a(3,2)=0.66667
  a(3,3)=1
  a(3,4)=0.75
  a(4,1)=0.25
  a(4,2)=0.5
  a(4,3)=0.75
  a(4,4)=1

  call eigen(4,a,sing,u)

  print *,sing
  print *
  print *,u
  print *
  do i=1,4
     print *,u(i,:), '  Mag= ',sqrt(dot_product(u(i,:),u(i,:)))
  end do

  end

