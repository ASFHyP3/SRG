! create db file for backprojection processing

  use sql_mod

  character*300 dbfile, burstfile, orbtimingfile, posfile, str,table
  integer*8 db
  integer*1, allocatable :: data(:)
  real*8 fine, starttime, fref, pi, c, prf, pri, range0
  real*8 samplefreq, swst, re, ht, samplefrequency, d8, wvl
  integer rangedecimation

  pi=4.d0*atan2(1.d0,1.d0)
  fref=37.53472224d0
  c=299792.458d3
  wvl=0.05546576

  if(iargc().lt.5)then
     print *,'usage: burst_roidb burstfile dbfile orbtimingfile posfile len'
     stop
  end if

  call getarg(1,burstfile)
  call getarg(2,dbfile)
  call getarg(3,orbtimingfile)
  call getarg(4,posfile)
  call getarg(5,str)
  read(str,*)len

  allocate (data(len*8))

! get params from burst file
  open(21,file=burstfile,access='direct',recl=len*8)
  read(21,rec=1)data
!!$  icoarse=in4(data(1+6))
!!$  fine=(in2(data(1+10))+0.5)*2.**(-16)
!!$  starttime=mod(icoarse,86400)+fine
  starttime=d8(data(1+68))
  prf=fref/in3(data(1+50))*1.e6
!  print *,'acq. time, pri number: ',starttime,numpri,prf
  rangedecimation=iand(data(1+40),255)
  samplefreq=samplefrequency(rangedecimation)*1.e6
!  print *,'ranegdecimation samplefreq ',rangedecimation,samplefreq
  swst=in3(data(1+53))/fref*1.e-6
  irank=iand(data(1+49),31)
  pri=in3(data(1+50))/fref*1.e-6
  range0=c/2.*(irank*pri+swst)



  call open_db(db,dbfile)
  table='file'
  call add_tbl(db,table)
  call add_param(db,table,'raw_data_file')
  call edit_paramc(db,table,'raw_data_file',burstfile,'char','input rc file')
  call add_param(db,table,'orbinfo')
  call edit_paramc(db,table,'orbinfo',orbtimingfile,'char','input rc file')
  call add_param(db,table,'posfile')
  call edit_paramc(db,table,'posfile',posfile,'char','input rc file')
  call add_param(db,table,'number_of_range_bins')
  call edit_parami(db,table,'number_of_range_bins',len,'int','samples per line')
  call add_param(db,table,'bytes_per_line')
  call edit_parami(db,table,'bytes_per_line',len*8,'int','samples per line')
  call add_param(db,table,'prf')
  call edit_paramd(db,table,'prf',prf,'real','prf')
  call add_param(db,table,'wvl')
  call edit_paramd(db,table,'wvl',wvl,'real','wvl')
  call add_param(db,table,'re')
  call edit_paramd(db,table,'re',6378137.0d0,'real','re')
  call add_param(db,table,'ht')
  call edit_paramd(db,table,'ht',700000.d0,'real','ht')
  call add_param(db,table,'fd')
  call edit_paramd(db,table,'fd',0.0d0,'real','wvl')
  call add_param(db,table,'azimuthTimeSecondsRawData')
  call edit_paramd(db,table,'azimuthTimeSecondsRawData',starttime,'real','wvl')
  call add_param(db,table,'r0')
  call edit_paramd(db,table,'r0',range0,'real','wvl')
  call add_param(db,table,'fs')
  call edit_paramd(db,table,'fs',samplefreq,'real','wvl')
  call add_param(db,table,'azimuthTimeInterval')
  call edit_paramd(db,table,'azimuthTimeInterval',0.0d0,'real','wvl')
  call close_db(db)

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

real*8 function d8(data)
  integer*1 data(*)
  integer*1 b(8)
  real*8 d
  equivalence (b,d)

  b=data(1:8)
  d8=d
  !print *,b,d
  return
  end

real*8 function samplefrequency(rangeDecimation)
  real*8 fref
  integer rangeDecimation

  fref=37.53472224d0;

  select case (rangeDecimation)
  case (0)
     samplefrequency= 3./4.*4.*fref
  case (1)
     samplefrequency= 2./3.*4.*fref
  case (3)
     samplefrequency= 5./9.*4.*fref
  case (4)
     samplefrequency= 4./9.*4.*fref
  case (5)
     samplefrequency= 3./8.*4.*fref
  case (6)
     samplefrequency= 1./3.*4.*fref
  case (7)
     samplefrequency= 1./6.*4.*fref
  case (8)
     samplefrequency= 3./7.*4.*fref
  case (9)
     samplefrequency= 5./16.*4.*fref
  case (10)
     samplefrequency= 3./26.*4.*fref
  case (11)
     samplefrequency= 4./11.*4.*fref
  end select
  return
end function samplefrequency

