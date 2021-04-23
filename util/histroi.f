!c  histroi - histogram of an entire raw file from roi.in file

      integer*1 d(32768)
      integer h(0:255),statb(13)
      character*60 file,roifile,str
      double precision sum,sumsq


      if(iargc().lt.1)then
         print *,'usage: hist roiin_file <firstline> <lastline>'
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


      open(21,file=file,access='direct',recl=len)

      sum=0.
      sumsq=0.
      n=0

      do i=0,255
         h(i)=0
      end do

      do i=linefirst,linelast
         if(mod(i,2000).eq.0)print *,i
         read(21,rec=i)(d(k),k=1,len)
         do j=iqskip*2+1,lengood
            k=iand(d(j),255)
            h(k)=h(k)+1
            sum=sum+k
            sumsq=sumsq+k*k
            n=n+1
         end do
      end do
      close(21)

      open(22,file='hist.out')
      do i=0,255
         write(22,*)h(i)
      end do

!c  don't display last zero-entry bins
      do i=255,0,-1
         if(h(i).ne.0)go to 30
      end do
 30   print *,'Largest bin used: ',i

      do j=0,i,4
         print '(1x,4i12)',(h(k),k=j,j+3)
      end do

      print *,'Mean, stdv= ',sngl(sum/n),sngl(dsqrt(sumsq/n-(sum/n)**2))

      end

