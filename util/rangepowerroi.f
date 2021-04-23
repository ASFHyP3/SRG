!c  rangepowerroi - power vs. range of an entire raw file from roi.in file

      integer*1 d(32768)
      integer statb(13)
      real pow(8192),powline(8192)
      character*60 file,roifile,str
      double precision sum,sumsq
      parameter (nave=10)   !points summed for each power estimate


      if(iargc().lt.1)then
         print *,'usage: hist roiin_file <firstline> <lastline> <ave=15.5>'
         stop
      end if

      call getarg(1,roifile)
      open(20,file=roifile)
      do i=1,100
         read(20,*,end=20)str
         if(i.eq.1)then
            do k=1,60
               if(str(k:k).eq.' ')go to 10
            end do
 10         file=str(1:k-1)
         end if
         if(i.eq.7)read(str,*)len
         if(i.eq.8)read(str,*)lengood
         if(i.eq.11)read(str,*)iqskip
      end do
 20   close(20)

!c  how big is the file?
      call stat(file,statb)
      lines=statb(8)/len
      print *,'Lines in file: ',lines
      linefirst=1
      linelast=lines
      if(iargc().ge.2)then
         call getarg(2,str)
         read(str,*)linefirst
      end if
      if(iargc().ge.3)then
         call getarg(3,str)
         read(str,*)linelast
      end if
      ave=15.5
      if(iargc().ge.4)then
         call getarg(4,str)
         read(str,*)ave
      end if

      open(21,file=file,access='direct',recl=len)

      do i=linefirst,linelast
         if(mod(i,2000).eq.0)print *,i
         read(21,rec=i)(d(k),k=1,len)
         n=0
         do j=1,len/nave
            pow(j)=0.
         end do
         do j=iqskip*2+1,lengood,nave
            sumsq=0.
            do jj=0,9
               k=iand(d(j+jj),255)
               sumsq=sumsq+(k-ave)*(k-ave)
            end do
            powline((j-1)/nave+1)=sumsq
            n=n+1
         end do
         do j=1,n
            pow(j)=pow(j)+powline(j)/nave
         end do
      end do
      close(21)

      open(22,file='power.out')
      do i=1,n
         write(22,*)pow(i)
      end do

      do i=1,n,4
         print '(1x,4f12.2)',(pow(k),k=i,i+3)
      end do

      end

