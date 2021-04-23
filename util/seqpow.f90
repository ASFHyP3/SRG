  !c  seqpow - create a sequence of amplitude images
  
  use omp_lib 

  complex*8, allocatable :: in(:,:)
  real*4, allocatable :: inlooks(:,:)
  character*300 infile, outfile, str, filelist, fin(10000)
  integer statb(13)
  integer*8 filelen

  if (iargc().lt.4)then
     print *,'usage: seqpow filelist outfile len <xlooks=1> <ylooks=xlooks>'
     stop
  end if

  call getarg(1,filelist)
  call getarg(2,outfile)
  call getarg(3,str)
  read(str,*)len
  looksx=1
  if (iargc().ge.4)then
     call getarg(4,str)
     read(str,*)looksx
  end if
  looksy=looksx
  if (iargc().ge.5)then
     call getarg(5,str)
     read(str,*)looksy
  end if

  !c read in the input file list, containing files to be sequenced
  open(21,file=filelist)
  do i=1,10000
     read(21,'(a)',end=11)fin(i)
!     print *,fin(i)
  end do
11 close(21)
  nigrams=i-1
  print *,'Input files: ',nigrams

  !c get file length
  lines=filelen(fin(1))/8/len
  print *,fin(1)
  write(*,*)'Lines in input file: ',lines

  !c set output file params and open
  lenout=(len/looksx)
  linesout=(lines/looksy)
  print *,'Output file size: ',lenout,linesout
  open(19,file=outfile,access='direct',recl=lenout*linesout*4)
  allocate(in(len,lines),inlooks(lenout,linesout))

  !c  loop over file list
  !$omp parallel do shared(nigrams,len,lines,lenout,linesout,looksx,looksy) private(line,ipix,i,j,in,inlooks,sum)
  do ifile=1,nigrams
     print *,'on file ',trim(fin(ifile))
     if(ifile.le.nigrams)then
        open(20+ifile,file=trim(fin(ifile)),access='direct',recl=len*lines*8)
        read(20+ifile,rec=1)in
        close(20+ifile)
     end if
     !print *,'file read'
     !c  take looks
     do line=1,linesout
        do ipix=1,lenout
           !print *,line,ipix
           sum=0.0
           do i=1,looksx
              do j=1,looksy
                 sum=sum+cabs(in((ipix-1)*looksx+i,(line-1)*looksy+j))**2
              end do
           end do
           inlooks(ipix,line)=sqrt(sum)
        end do
     end do
     !print *,'writing'
     write(19,rec=ifile)inlooks
  end do
  !$omp end parallel do

  close(19)

end program
