! simulate pahses

!  call is  /home/zebker/util/simphases 20180622_20180628.int 20180622_20180628.unw 6.0

  real a(1197*2,1312)
  character*300 intfile, unwfile,str

  na=1197
  nd=1312

  if(iargc().lt.3)then
     print *,'usage: simphases 20180622_20180628.int 20180622_20180628.unw 6.0'
     call exit()
  end if

  call getarg(1,intfile)
  call getarg(2,unwfile)
  call getarg(3,str)
  read(str,*)time

  open(21,file=intfile,access='direct',recl=na*nd*8)
  open(22,file=unwfile,access='direct',recl=na*nd*8)

  read(21,rec=1)a
  a(1:na,:)=sqrt(a(1:na:2,:)**2+a(2:na:2,:)**2)
  a(na+1:na*2,:)=time
  write(22,rec=1)a

  end
