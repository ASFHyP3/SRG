!c  create simulated sbas data

  real, allocatable :: phase(:), noise(:), timedeltas(:)
  character*100 str

  IF(iargc().lt.2)THEN
     WRITE(*,*)'usage: make_sbas_sim_data nunwfiles nslcs <noise=0>'
     STOP
  END IF

  CALL getarg(1,str)
  READ(str,*)ncells !number of files
  CALL getarg(2,str)
  READ(str,*)nslc !number of slcs
  anoise=0.
  if(iargc().ge.3)then
     call getarg(3,str)
     read(str,*)anoise
  end if

  ALLOCATE(phase(ncells), noise(nslc), timedeltas(nslc))

! noise vector
  open(1,file='noisedata.txt')
  do i=1,nslc
     gausrv=0.
     do j=1,12
        gausrv=gausrv+rand()
     end do
     gausrv=gausrv-6.
     noise(i)=gausrv*anoise
     write(1,*)noise(i)
  end do
  close(1)
  open(1,file='timedeltas.out')
  timedeltas(1)=0.
  do i=1,nslc-1
     read(1,*)timedeltas(i+1)
  end do
  close(1)

  OPEN(UNIT=1,FILE='deltime.out',STATUS='old')
  do i=1,ncells
     READ(1,*)igram,dt,t0,t1
     if(i.eq.1)tref=t0
     if(i.eq.ncells)tlast=t1
     ! which slc does each come from?
     do k=1,nslc
        timek=sum(timedeltas(1:k))
        if(abs(t0-tref-timek).lt.1.)k0=k
        if(abs(t1-tref-timek).lt.1.)k1=k
     end do
!     print *,'igram ',i,' k0 k1 ',k0,k1
     ! simulated deformation history
     phase(i)=def(tref,t1)-def(tref,t0)+noise(k1)-noise(k0)
  end do
  CLOSE(1)

  !  write out simulated data
  open(1,file='simdata.txt')
  do i=1,ncells
     write(1,*)phase(i)
  end do
  close(1)

  !write out reference signal
  open(1,file='refdata.txt')
  do i=1,int(tlast-tref),12
!     print *,i,def(0.,float(i))
     write(1,*)def(0.,float(i))
  end do
  close(1)

  end

  function def(tref,t)
    if(t-tref.lt.250)then
       def=0.125*sin(2*3.14159*(t-tref)/100)-0.002*(t-tref)
    else
       if(t-tref.lt.500)then
          def=0.125*sin(2*3.14159*(t-tref)/100)-0.002*250
       else
          if(t-tref.lt.800)then
             def=0.125*sin(2*3.14159*(t-tref)/100)-0.002*250-0.002*(t-tref-500)
          end if
       end if
    end if
    return 
  end function def
