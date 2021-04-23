!c  addigrams - add two files without oversampling

      use omp_lib

      complex*8, allocatable:: in1(:),in2(:),igram(:)
      character*300 fin1,fin2,str,figram,famp
      integer*8 nbytes,filelen

      !$omp parallel
      !n=omp_get_num_threads()
      !$omp end parallel
      print *, 'Max threads used: ', n

      if(iargc().lt.4)then
         write(*,*)'usage: addigrams infile1 infile2 outfile length <valid_lines> <scale=1>'
         print *,'scale is multiplied by each scene to prevent overflow'
         stop
      end if

      call getarg(1,fin1)
      call getarg(4,str)
      read(str,*)na
      open(21,file=fin1,form='unformatted',access='direct',recl=na*8)
      nbytes=filelen(trim(fin1))
      nd=nbytes/8/na
      write(*,*)'Lines in file: ',nd
      call getarg(2,fin2)
      if(trim(fin1).ne.trim(fin2))then
         open(22,file=fin2,form='unformatted',access='direct',recl=na*8)
      end if
      call getarg(3,figram)
      nvalid=nd
      if(iargc().ge.5)then
         call getarg(5,str)
         read(str,*)nvalid
      end if
      scale=1.0
      if(iargc().ge.6)then
         call getarg(6,str)
         read(str,*)scale
      end if

      open(32,file=figram,form='unformatted',access='direct',recl=na*8)

      !$omp parallel do private(in1,in2,up1,up2,inline1,inline2,igram,amp) &
      !$omp private(igramacc,ampacc,igramtemp,amptemp,j,k,i,line,plannnnf,plannnni) &
      !$omp shared(nvalid,looksdn,scale,na,nnn,iplannnnf,iplannnni) &
      !$omp shared(looksac,fin1,fin2)

      do line=1,nvalid
         if(mod(line,1000).eq.0)print *,line
         !c  allocate the local arrays
         allocate (in1(na),in2(na),igram(na))

!c     read in lines
         read(21,rec=line,err=98)in1
         if(fin1.ne.fin2)then
            read(22,rec=line,err=98)in2
         else
            in2=in1
         end if
98       continue
!c  add
         in1=in1*scale
         in2=in2*scale

         igram(1:na)=in1+in2

         write(32,rec=line)igram
 99   continue

         deallocate (in1, in2, igram)
      end do
      !$omp end parallel do

      end


