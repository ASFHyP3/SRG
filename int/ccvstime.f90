!  ccvstime - list of correlation and time offset for a pixel

  real*4, allocatable:: array(:,:)
  character*300 str,cclist,infile,outfile
  character*8 date1, date2
  integer*4 ix(100),iy(100)

  if(iargc().lt.5)then
     print *,'Usage: ccvstime cclist len lines ix1 iy1 ix2 iy2 ix3 ...'
     call exit
  end if

  call getarg(1,cclist)
  call getarg(2,str)
  read(str,*)len
  call getarg(3,str)
  read(str,*)lines
  npts=(iargc()-3)/2
  do i=1,npts
     call getarg(2+i*2,str)
     read(str,*)ix(i)
     call getarg(3+i*2,str)
     read(str,*)iy(i)
  end do

  allocate (array(len*2,lines))

  open(22,file='ccvals')
  open(1,file=cclist)
  do i=1,1000000
     read(1,'(a)',end=99)infile
!     print *,infile
     !  extract start, stop dates
     date1=infile(1:8)
     date2=infile(10:17)
!     print *,date1,' ',date2
     read(date1(1:4),'(i4)')iyear
     read(date1(5:6),'(i2)')imonth
     read(date1(7:8),'(i2)')iday
     jd1=jd(iyear,imonth,iday)
!     print *,jd1
     read(date2(1:4),'(i4)')iyear
     read(date2(5:6),'(i2)')imonth
     read(date2(7:8),'(i2)')iday
     jd2=jd(iyear,imonth,iday)
!     print *,jd2,iyear,imonth,iday
     open(21,file=infile,access='stream')
     read(21)array
     close(21)
     write(22,*)jd2-jd1,(array(len+ix(k),iy(k)),k=1,npts)
  end do
99 print *,'Done ',i
  close(1)
  close(22)

  end


      INTEGER FUNCTION JD (YEAR,MONTH,DAY)
!
!---COMPUTES THE JULIAN DATE (JD) GIVEN A GREGORIAN CALENDAR
!   DATE (YEAR,MONTH,DAY).
!
      INTEGER YEAR,MONTH,DAY,I,J,K
!
      I= YEAR
      J= MONTH
      K= DAY
!     
      JD= K-32075+1461*(I+4800+(J-14)/12)/4+367*(J-2-(J-14)/12*12)/12-3*((I+4900+(J-14)/12)/100)/4
!
      RETURN
      END

!Conversion from a Julian date to a Gregorian calendar date.
      SUBROUTINE GDATE (JD, YEAR,MONTH,DAY)
!
!---COMPUTES THE GREGORIAN CALENDAR DATE (YEAR,MONTH,DAY)
!   GIVEN THE JULIAN DATE (JD).
!
      INTEGER JD,YEAR,MONTH,DAY,I,J,K
!
      L= JD+68569
      N= 4*L/146097
      L= L-(146097*N+3)/4
      I= 4000*(L+1)/1461001
      L= L-1461*I/4+31
      J= 80*L/2447
      K= L-2447*J/80
      L= J/11
      J= J+2-12*L
      I= 100*(N-49)+I+L
!
      YEAR= I
      MONTH= J
      DAY= K
!
      RETURN
      END
