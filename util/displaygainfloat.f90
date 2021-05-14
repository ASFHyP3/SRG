!c  displaygainfloat - estimate gain for display float file

character*100 file,str
integer :: statb(13),stat
real*4,allocatable :: array(:,:)

if(iargc().lt.2)then
   print *,'Usage: displaygainfloat file len'
   stop
end if

call getarg(1,file)
call getarg(2,str)
read(str,*)len

!c  get file sizes
ierr=stat(file,statb)
lines=statb(8)/4/len
print *,'File length, lines: ',lines

!c  read in file to memory
allocate (array(len,lines))
open(21,file=file,access='direct',recl=lines*len*4)
read(21,rec=1)array
close(21)

!c  compute means with and without zeros
sum=0.
sumnozeros=0.
sumn=0
sumnozerosn=0
do j=1,lines
   do i=1,len
      sum=sum+array(i,j)
      sumn=sumn+1
      if(abs(array(i,j)).gt.1.e-10)then
         sumnozeros=sumnozeros+array(i,j)
         sumnozerosn=sumnozerosn+1
      end if
   end do
end do

sum=sum/sumn
sumnozeros=sumnozeros/sumnozerosn
print *,sum,sumnozeros,sum/sumnozeros

open(21,file='displaygain.out')
write(21,*)sum/sumnozeros
close(21)

end
