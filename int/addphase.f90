!c  addphase - add a constant phase to a complex file

      complex, allocatable ::  a(:)

      character*160 fin,fout,str
      integer*8 filelen,nbytes

      if(iargc().lt.3)then
         write(*,*)'usage: addphase infile outfile phase(rad)'
         stop
      end if

      call getarg(1,fin)
      call getarg(2,fout)
      call getarg(3,str)
      read(str,*)phase

      nbytes=filelen(fin)
      nd=nbytes/8
      print *,nd," complex values in file"

      allocate(a(nd))
      open(21,file=fin,form='unformatted',access='direct',recl=nd*8)
      open(22,file=fout,form='unformatted',access='direct',recl=nd*8)

      read(21,rec=1)a
      a=a*cmplx(cos(phase),sin(phase))
      write(22,rec=1)a

      end
