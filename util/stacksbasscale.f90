  !c  stacksbasscale - scale factor to get cumulative deformation scale
  
  character*300 str, filelist, file1, file2
  character*10 datestr
  integer*8 dateint
  real*8 firsttime,lasttime,cumulative
  integer dat(8)  ! array for conversion to julian date- year, month, day ...times...
  real*8 datejul

  if (iargc().lt.1)then
     print *,'usage: stacksbasscale sbaslist <wvl=1 m>'
     stop
  end if

  call getarg(1,filelist)
  wvl=1
  if(iargc().ge.2)then
     call getarg(2,str)
     read(str,*)wvl
  end if

  !c loop over filelist
  cumulative=0.
  firsttime=100000000
  lattime=0
  open(21,file=filelist)
  do i=1,100000
     read(21,'(a)',end=11)str
     loc=index(str,' ')
     file1=str(1:loc)
     ! extract date
     locdate=index(file1,'_2')
     datestr=file1(locdate+1:locdate+8)
     dat=0
     read(datestr(1:4),*)dat(1)
     read(datestr(5:6),*)dat(2)
     read(datestr(7:8),*)dat(3)
     call d2j(dat,datejul,ierr)
     firsttime=min(firsttime,datejul)
     lasttime=max(lasttime,datejul)
     str=str(loc+1:300)
     loc=index(str,' ')
     file2=str(1:loc)
    ! extract date from second file
     locdate=index(file2,'_2')
     datestr=file2(locdate+1:locdate+8)
     dat=0
     read(datestr(1:4),*)dat(1)
     read(datestr(5:6),*)dat(2)
     read(datestr(7:8),*)dat(3)
     call d2j(dat,datejul,ierr)
     firsttime=min(firsttime,datejul)
     lasttime=max(lasttime,datejul)

!!$     read(datestr,*)dateint
!!$     firsttime=min(firsttime,dateint)
!!$     lasttime=max(lasttime,dateint)
     str=str(loc+1:300)
!!$     print *,file1
!!$     print *,file2
!!$     print *,str
     read(str,*)basetime,basespat
     !print *,basetime,basespat
     cumulative=cumulative+basetime
  end do
11 close(21)
  nigrams=i-1
  print *,'Input files: ',nigrams
  print *,'cumulative time, firsttime,lasttime,timedelta ',cumulative,firsttime,lasttime,lasttime-firsttime
  print *,'Scale for radians: ',(lasttime-firsttime)/cumulative
  print *,'Scale for meters of deformation, wvl: ',(lasttime-firsttime)/cumulative*wvl/4./3.14159265
end program

subroutine d2j(dat,julian,ierr)
!-----------------------------------------------------------------------------------------------------------------------------------
! * Author:    John S. Urban
! * Version:   1.0 2015-12-21
! * Reference: From Wikipedia, the free encyclopedia 2015-12-19
! * There is no year zero
! * Julian Day must be non-negative
! * Julian Day starts at noon; while Civil Calendar date starts at midnight
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=:),parameter :: ident="@(#)d2j(3f): Converts proleptic Gregorian date array to Julian Day -JSU 2015-12-21"
integer,intent(in)         :: dat(8)   ! array like returned by DATE_AND_TIME(3f)
real(kind=8),intent(out)  :: julian   ! Julian Day (non-negative, but may be non-integer)
integer,intent(out)        :: ierr     ! Error return, 0 for successful execution,-1=invalid year,-2=invalid month,-3=invalid day,
                                       ! -4=invalid date (29th Feb, non leap-year)
   integer                 :: year, month, day, utc, hour, minute
   real(kind=8)           :: second
   integer                 :: A, Y, M, JDN
!-----------------------------------------------------------------------------------------------------------------------------------
   year   = dat(1)                        ! Year
   month  = dat(2)                        ! Month
   day    = dat(3)                        ! Day
   utc    = dat(4)*60                     ! Delta from UTC, convert from minutes to seconds
   hour   = dat(5)                        ! Hour
   minute = dat(6)                        ! Minute
   second = dat(7)-utc+dat(8)/1000.0d0    ! Second   ! correction for time zone and milliseconds
!-----------------------------------------------------------------------------------------------------------------------------------
   julian = -HUGE(99999)                  ! this is the date if an error occurs and IERR is < 0
!-----------------------------------------------------------------------------------------------------------------------------------
   if(year==0 .or. year .lt. -4713) then
      ierr=-1
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
!  You must compute first the number of years (Y) and months (M) since March 1st -4800 (March 1, 4801 BC)
   A=(14-month)/12 ! A will be 1 for January or Febuary, and 0 for other months, with integer truncation
   Y=year+4800-A
   M=month+12*A-3  ! M will be 0 for March and 11 for Febuary
!  All years in the BC era must be converted to astronomical years, so that 1BC is year 0, 2 BC is year "-1", etc.
!  Convert to a negative number, then increment towards zero
!  Staring from a Gregorian calendar date
   JDN=day + (153*M+2)/5 + 365*Y + Y/4 - Y/100 + Y/400 - 32045  !  with integer truncation
!  Finding the Julian date given the JDN (Julian day number) and time of day
   julian=JDN + dble(hour-12)/24.0d0 + dble(minute)/1440.0d0 + second/86400.0d0
!-----------------------------------------------------------------------------------------------------------------------------------
   if(julian.lt.0.d0) then                  ! Julian Day must be non-negative
      ierr=1
   else
      ierr=0
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine d2j
