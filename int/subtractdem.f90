!  subtractdem - subtract a phase based on dem from a complex image

complex a(1328,6656)
real z(1328,6656)
character*60 str,infile,outfile,realdem

if(iargc().lt.4)then
   print *,'usage: subtractdem infile outfile realdem scale'
   stop
end if

call getarg(1,infile)
call getarg(2,outfile)
call getarg(3,realdem)
call getarg(4,str)
read(str,*)scale

open(1,file=infile,access='direct',recl=1328*6656*8)
open(2,file=outfile,access='direct',recl=1328*6656*8)
open(3,file=realdem,access='direct',recl=1328*6656*4)

read(1,rec=1)a
read(3,rec=1)z

do line=1,6656
   do i=1,1328
      phase=z(i,line)*scale
      a(i,line)=a(i,line)*cmplx(cos(phase),sin(phase))
   end do
end do

write(2,rec=1)a

end

