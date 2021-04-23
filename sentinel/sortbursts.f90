!c  sortbursts - find which sentinel lines go to which burst
!


  integer*1 b(80)
  complex in(30000),data(32768),ref(32768)
  character*200 rawfile, outfile
  integer*4 burst,burstline(0:1000)
  real*8 fref, samplefrequency, t, phase, pi

  equivalence (b,in)
  
  if(iargc().lt.1)then
     print *,'Usage: sortbursts rawdatafile'
     stop
  end if

  call getarg(1,rawfile)

  ! input file
  open(11,file=rawfile,access='direct',recl=30000*8)
  open(30,file='sortbursts.out')

! loop over each line
  ipri=0
  burst=0
  burstline=0
  do i=1,1000000
     if(mod(i,2000).eq.0)print *,'At line ',i
     read(11,rec=i,err=99)in
     pricount=in4(b(1+33))
     if(pricount-ipri.gt.1000)burst=burst+1
     burstline(burst)=burstline(burst)+1
     write(30,*)i,burst,burstline(burst)
!     print *,i,ramprate,pulselength,swst,pricount,pricount-ipri,burst
     ipri=pricount
  end do
99  print *,'Sorted ',i-1,' lines of raw data.'
  close(11)
  close(30)
end program

integer function in2(data)
  integer*1 data(*)
  in2=iand(data(2),255)+256*iand(data(1),255)
  return
end function in2

integer function in3(data)
  integer*1 data(*)
  in3=iand(data(3),255)+256*iand(data(2),255)+256*256*iand(data(1),255)
  return
end function in3

integer function in4(data)
  integer*1 data(*)
  in4=iand(data(4),255)+256*iand(data(3),255)+256*256*iand(data(2),255)+256*256*256*iand(data(1),255)
  return
end function in4

