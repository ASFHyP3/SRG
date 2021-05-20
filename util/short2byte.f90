! short2byte -- convert a short file to a byte file, scale to fit

  integer*2 in(100000)
  integer*1 out(100000)
  character*100 infile,outfile,str

  if(iargc().lt.4)then
     print *,'usage: short2byte infile outfile len max_short_value <firstline=1> <nlines=all>'
     stop
  end if

  call getarg(1,infile)
  call getarg(2,outfile)
  call getarg(3,str)
  read(str,*)len
  call getarg(4,str)
  read(str,*)maxval
  istart=1
  if(iargc().ge.5)then
     call getarg(5,str)
     read(str,*)istart
  end if
  nlines=10000000
  if(iargc().ge.6)then
     call getarg(6,str)
     read(str,*)nlines
  end if

  open(21,file=infile,access='direct',recl=len*2)
  open(22,file=outfile,access='direct',recl=len)
  scale=maxval/256
  print *,'Scale is: ',scale

  lines=0
  do line=istart,istart+nlines-1
     read(21,rec=line,err=99)(in(k),k=1,len)
     do i=1,len
        out(i)=iand(in(i),-1)/scale
     end do
     write(22,rec=line-istart+1)(out(k),k=1,len)
     lines=lines+1
  end do
99 continue
  print *,'Wrote ',lines,' lines'

  end

