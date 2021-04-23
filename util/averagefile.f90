!c  averagemht - compute an average of a list of mht format files

      real*4, allocatable :: avemaghgt(:,:),maghgt(:,:)
      character*100 flist,fin(10000),fout,str
      integer statb(13),fstat

      if(iargc().lt.3)then
         write(*,*)'usage: averagemht filelist outfile length'
         write(*,*)'  NOTE memory needed: 2 x file size'
         stop
      end if

      call getarg(1,flist)
      call getarg(2,fout)
      call getarg(3,str)
      read(str,*)len

!c read in the input file list, containing files to be averaged
      open(21,file=flist)
      do i=1,10000
         read(21,*,end=11)fin(i)
      end do
11    close(21)
      nfiles=i-1
      print *,'Files to be averaged: ',nfiles

!c get file length
      open(21,file=trim(fin(1)),form='unformatted',access='direct',recl=len*8)
      ierr=fstat(21,statb)
      lines=statb(8)/8/len
      write(*,*)'Lines in file: ',lines,'      file: ',trim(fin(1))
      close(21)

!c  allocate memory
      allocate (avemaghgt(len*2,lines))
      allocate (maghgt(len*2,lines))

      avemaghgt=0.

      do i=1,nfiles
         print *,'reading file ',i,' ',trim(fin(i))
         open(100+i,file=trim(fin(i)),access='direct',recl=len*lines*8)
         read(100+i,rec=1)maghgt
         close(100+i)
         avemaghgt=avemaghgt+maghgt
      end do
      avemaghgt=avemaghgt/nfiles

!c  open output file
      open(20,file=fout,form='unformatted',access='direct',recl=len*lines*8)
      write(20,rec=1)avemaghgt
      close(20)

      end
