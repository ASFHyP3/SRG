  integer*2 d(3601,3601),dd(1201,1201),ddd(2401,2401)

  d=0
  dd=0
  ddd=0
  open(21,file='n45w120.hgt',access='stream')
  write(21)d
  close(21)

  open(21,file='n46w120.hgt',access='stream')
  write(21)dd
  close(21)

  open(21,file='n47w120.hgt',access='stream')
  write(21)ddd
  close(21)

  end
