c  slc2corr - create multilooked correlation file from 2 slc files
      complex in1(20480,16),in2(20480,16),int(20480,16),sum,pha,phd
      complex outint(20480),outamp(20480)
      real amp1(20480,16),amp2(20480,16),out(20480)
      character*30 fin1,fin2,fout,str,fint,famp
      integer statb(13),fstat

      if(iargc().lt.4)then
         write(*,*)'usage: slc2corr infile1 infile2 outfile length <looksac=1> <looksdn>'
         write(*,*)' <dephased int file> <amp file>'
         stop
      end if

      call getarg(1,fin1)
      call getarg(4,str)
      read(str,*)na
      open(21,file=fin1,form='unformatted',access='direct',recl=na*8)
      ierr=fstat(21,statb)
      nd=statb(8)/8/na
      write(*,*)'Lines in file: ',nd
      call getarg(2,fin2)
      call getarg(3,fout)
      if(iargc().ge.5)then
         call getarg(5,str)
         read(str,*)la
      else
         la=1
      end if
      if(iargc().ge.6)then
         call getarg(6,str)
         read(str,*)ld
      else
         ld=la
      end if
      if(iargc().ge.7)then
         call getarg(7,fint)
         open(32,file=fint,form='unformatted',access='direct',recl=na/la*8)
      end if
      if(iargc().ge.8)then
         call getarg(8,famp)
         open(33,file=famp,form='unformatted',access='direct',recl=na/la*8)
      end if

      if(ld.gt.16)then
         write(*,*)'Memory limitation; max looks down is 16.'
         stop
      end if

      open(22,file=fin2,form='unformatted',access='direct',recl=na*8)
      open(31,file=fout,form='unformatted',access='direct',recl=na/la*8)
      
c      open(41,file='qtest',form='unformatted',access='direct',recl=na*ld*8)
      
      lineout=0
      do line=1,nd,ld
         if(mod(lineout,64).eq.0)write(*,*)line
         lineout=lineout+1

c  read in ld lines, cross multiply
         do i=0,ld-1
c  read in data from both files
            read(21,rec=line+i,err=99)(in1(k,i+1),k=1,na)
            read(22,rec=line+i,err=99)(in2(k,i+1),k=1,na)
c  cross-multiply and save amplitudes
            do j=1,na
               int(j,i+1)=in1(j,i+1)*conjg(in2(j,i+1))
               amp1(j,i+1)=cabs(in1(j,i+1))**2
               amp2(j,i+1)=cabs(in2(j,i+1))**2
            end do
         end do

c  dephase data
         do j=1,na,la
            pha=cmplx(0.,0.)
            phd=cmplx(0.,0.)
c  across first
               do k=1,la-1
                  do kk=1,ld
                     pha=pha+int(j+k-1,kk)*conjg(int(j+k,kk))
                  end do
               end do
c  down next
               do k=1,la
                  do kk=1,ld-1
                     phd=phd+int(j+k-1,kk)*conjg(int(j+k-1,kk+1))
                  end do
               end do
c  remove phases
               phasea=atan2(aimag(pha),real(pha))
               phased=atan2(aimag(phd),real(phd))
c               phasea=0.
c               phased=0.
c               write(*,*)phasea,phased

               sum1=0.
               sum2=0.
               sum=cmplx(0.,0.)
               do k=1,la
                  do kk=1,ld
                     sum1=sum1+amp1(j+k-1,kk)
                     sum2=sum2+amp2(j+k-1,kk)
                     sum=sum+int(j+k-1,kk)
                  end do
               end do
               coco=cabs(sum)/sqrt(sum1*sum2)

               write(*,*)'before',coco,phasea,phased
               do kk=1,ld
                  write(*,*)(atan2(aimag(int(j+k-1,kk)),real(int(j+k-1,kk))),k=1,la)
               end do

               do k=1,la
                  do kk=1,ld
c               write(*,*)'before ',int(j+k-1,kk)
                     int(j+k-1,kk)=int(j+k-1,kk)*cmplx(cos(phasea*k+phased*kk),sin(phasea*k+phased*kk))
c               write(*,*)'after  ',int(j+k-1,kk)
                  end do
               end do

               sum1=0.
               sum2=0.
               sum=cmplx(0.,0.)
               do k=1,la
                  do kk=1,ld
                     sum1=sum1+amp1(j+k-1,kk)
                     sum2=sum2+amp2(j+k-1,kk)
                     sum=sum+int(j+k-1,kk)
                  end do
               end do
               coco=cabs(sum)/sqrt(sum1*sum2)

               write(*,*)'after',coco
               do kk=1,ld
                  write(*,*)(atan2(aimag(int(j+k-1,kk)),real(int(j+k-1,kk))),k=1,la)
               end do


         end do  !end dephasing


c         write(41,rec=lineout)((int(k,kk),k=1,na),kk=1,ld)
      
c  finally we are ready to calculate correlation
         jout=0
         do j=1,na,la
            jout=jout+1
            sum=cmplx(0.,0.)
            sum1=0.
            sum2=0.
            do k=1,la
               do kk=1,ld
                  sum=sum+int(j+k-1,kk)
                  sum1=sum1+amp1(j+k-1,kk)
                  sum2=sum2+amp2(j+k-1,kk)
               end do
            end do
c  get correlation and amplitude
            amplitude=sqrt(sum1*sum2)
            if(amplitude.gt.0.0)then
               coco=cabs(sum)/amplitude
            else
               coco=0.
            end if
            out(jout)=amplitude
            out(jout+na/la)=coco
            outint(jout)=sum
            outamp(jout)=cmplx(sqrt(sum1),sqrt(sum2))
         end do

         write(31,rec=lineout)(out(k),k=1,na/la*2)
         if(iargc().ge.7)write(32,rec=lineout)(outint(k),k=1,na/la)
         if(iargc().ge.8)write(33,rec=lineout)(outamp(k),k=1,na/la)
      end do
 99   continue
      end


