!c  call decode line from fortran

  character*300 infile
  integer*8 filelen, inbytes
  integer*1, allocatable :: rawdata(:)
  integer packetDataLength,swath,sigtyp,nq
  complex*8 cpxsamps(32768)

  call getarg(1,infile)
  inbytes=filelen(infile)

  allocate (rawdata(inbytes))
  open(11,file=infile,access='direct',recl=inbytes)
  read(11,rec=1)rawdata
  close(11)

  iptr=0
  do i=1,10

! get a couple of params
  packetDataLength=in2(rawdata(iptr+5))
  swath=rawdata(iptr+65)
  sigtyp=iand(rawdata(iptr+64),16*15)
  nq=in2(rawdata(iptr+66))

  print *,i,packetDataLength,swath,sigtyp,nq,iptr

  call decode_line(rawdata(iptr+68),nq,packetDataLength,cpxsamps,i)
  print *,cpxsamps(1001:1004)

  iptr=iptr+packetDataLength+7

  end do



  end


integer function in2(data)
  integer*1 data(*)
  in2=iand(data(2),255)+256*iand(data(1),255)
  return
end function in2
