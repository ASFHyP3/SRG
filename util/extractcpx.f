      program extractcpx
c  extractcpx - extract a region of cpx pixels from an image

      implicit none
      complex in(100000),out(100000)
      integer  i_is,i_ss,i_nl,i_nr,i,j,i_nrec
      character*80 a_input,a_output,c

      if(iargc().lt.6)then
         print *,'usage: extractcpx infile outfile length center-x,y xsize (ysize)'
         stop
      end if
      call getarg(6,c)
      read(c,*)i_nr
      if(iargc().ge.7)then
         call getarg(7,c)
         read(c,*)i_nl
      else
         i_nl=i_nr
      end if
      call getarg(3,c)
      read(c,*)i_nrec
      call getarg(4,c)
      read(c,*)i_ss
      call getarg(5,c)
      read(c,*)i_is
      call getarg(1,a_input)
      call getarg(2,a_output)

c   open the input and output files

      open(unit=10,file=a_input,status='old',access='direct',form='unformatted',
     +     recl=8*i_nrec)

      open(unit=11,file=a_output,status='unknown',access='direct',
     +     form='unformatted',recl=8*i_nr)

      print *
      print *,'Files opened...'
     
c   extract the data

      do i=i_is,i_is+i_nl-1
c         print *,'line: ',i
       read(10,rec=i-i_nl/2) (in(j), j=1,i_nrec)
       do j=i_ss,i_ss+i_nr-1
         out(j-i_ss+1) = in(j-i_nr/2)
       enddo       
       write(11,rec=i-i_is+1) (out(j),j=1,i_nr)
       if(mod(i-i_is+1,100) .eq. 0)then
         print *,' At record = ',i-i_is+1
       endif

      enddo

      end
