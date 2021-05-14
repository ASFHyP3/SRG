  !c  tilecpx - create a tiled file from a list of cpx files, take looks
  
  complex*8, allocatable :: in(:,:),out(:,:),inlooks(:,:),inline(:),outline(:)
  character*100 infile, outfile, str, filelist, fin(10000)
  integer statb(13)

  if (iargc().lt.4)then
     print *,'usage: tilecpx filelist(cpx) outfile tiles_across len <xlooks=1> <ylooks=xlooks>'
     stop
  end if

  call getarg(1,filelist)
  call getarg(2,outfile)
  call getarg(3,str)
  read(str,*)nx
  call getarg(4,str)
  read(str,*)len
  looksx=1
  if (iargc().ge.5)then
     call getarg(5,str)
     read(str,*)looksx
  end if
  looksy=looksx
  if (iargc().ge.6)then
     call getarg(6,str)
     read(str,*)looksy
  end if

  !c read in the input file list, containing unwrapped interferograms to be printed
  open(21,file=filelist)
  do i=1,10000
     read(21,*,end=11)fin(i)
  end do
11 close(21)
  nigrams=i-1
  print *,'Input files: ',nigrams

  !c get file length
  open(21,file=trim(fin(1)),form='unformatted',access='direct',recl=len*8)
  ierr=fstat(21,statb)
  lines=statb(8)/8/len
  !print *,'fin(1) len lines ierr ',fin(1),len,lines,ierr
  write(*,*)'Lines in input file: ',lines
  close(21)

  !c set output file params and open
  lenout=(len/looksx)
  linesout=(lines/looksy)
  print *,'Output tile size, tiled image length: ',lenout,linesout,lenout*nx
  ny=(nigrams-1)/nx+1
  open(22,file=outfile,access='direct',recl=lenout*nx*8)
  allocate(in(len,lines),out(lenout*nx,linesout),inlooks(lenout,linesout))
  allocate (inline(len),outline(lenout))

  !c  loop over file list
  kgram=-1
  do igram=1,nigrams,nx
     kgram=kgram+1  ! which block of files are we on?
     print *,'On file block ',kgram
     do ix=0,nx-1
        ifile=igram+ix
        inlooks=0.
        if(ifile.le.nigrams)then
           open(21,file=trim(fin(ifile)),access='direct',recl=len*lines*8)
           read(21,rec=1)in
           close(21)
        else
           in=0.
        end if
           !c  take looks on one interferogram
           do line=1,linesout
              inline=sum(in(:,(line-1)*looksy+1:(line-1)*looksy+looksy),2)
              do ipix=1,lenout
                 outline(ipix)=sum(inline((ipix-1)*looksx+1:(ipix-1)*looksx+looksx))/looksx/looksy
              end do
              out(ix*lenout+1:ix*lenout+lenout,line)=outline
           end do
     end do  ! end loop over nx

     !c  write out in mht format
     do j=1,linesout  ! loop over records in output file
        write(22,rec=j+kgram*linesout)out(:,j)
     end do
  end do

end program
