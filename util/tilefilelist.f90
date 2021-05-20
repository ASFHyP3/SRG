  !c  tilefilelist - create a tiled file from a list of complex files, take looks
  
  complex*8, allocatable :: in(:,:),out(:),inlooks(:,:)
  character*100 infile, outfile, str, filelist, fin(1000)
  integer statb(13)
  integer*8 filelen

  if (iargc().lt.5)then
     print *,'usage: tilefilelist filelist(cpx) outfile tiles_across len <xlooks=1> <ylooks=xlooks>'
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

  !c read in the input file list, containing interferograms to be analyzed
  open(21,file=filelist)
  do i=1,10000
     read(21,'(a)',end=11)fin(i)
  end do
11 close(21)
  nigrams=i-1
  print *,'Input files: ',nigrams

  !c get file length
  lines=filelen(trim(fin(1)))/8/len
  write(*,*)'Lines in input file: ',lines

  !c set output file params and open
  lenout=(len/looksx)
  linesout=(lines/looksy)
  print *,'Output tile size, tiled image length: ',lenout,linesout,lenout*nx
  open(22,file=outfile,access='direct',recl=lenout*8)
  allocate(in(len,lines),out(lenout),inlooks(lenout,linesout))

  !c  loop over file list
  do ifile=1,nigrams
     !print *,'on file ',trim(fin(ifile))
     open(21,file=trim(fin(ifile)),access='direct',recl=len*lines*8)
     read(21,rec=1)in
     close(21)
     !print *,'file read'
     !c  take looks
     do line=1,linesout
        do ipix=1,lenout
           !print *,line,ipix
           inlooks(ipix,line)=cmplx(0.,0.)
           do i=1,looksx
              do j=1,looksy
                 inlooks(ipix,line)=inlooks(ipix,line)+in((ipix-1)*looksx+i,(line-1)*looksy+j)
              end do
           end do
        end do
     end do
     !print *,'writing'
     do j=1,linesout  ! loop over records in output file
        jrec=((ifile-1)/nx)*linesout*nx+mod(ifile-1,nx)+1+(j-1)*nx
        write(22,rec=jrec)inlooks(:,j)
     end do
  end do

end program
