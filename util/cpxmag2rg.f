!c** cpxmag2rg - convert two cpx magnitude files to a single rg file

      character*180 f1,f2,fout,str
      integer statb(13),stat
      integer*8 n, filelen
      complex a1(32768),a2(32768)
      real p(32768)

      if(iargc().lt.4)then
         write(*,*)'usage: cpxmag2rg cpxfile-1 cpxfile-2 rgfile len <ac-offset> <dn-offset>'
         stop
      end if
       
      call getarg(4,fout)
      read(fout,*)len
      call getarg(1,f1)
      call getarg(2,f2)
      call getarg(3,fout)
      iacoff=0
      if(iargc().ge.5)then
         call getarg(5,str)
         read(str,*)iacoff
      end if
      idnoff=0
      if(iargc().ge.6)then
         call getarg(6,str)
         read(str,*)idnoff
      end if


!      ierr = stat(f1,statb)
!      n=statb(8)
!      if(n.le.0)n=n+2**31+2**31
      n=filelen(f1)
      lines=n/len/8

      write(*,*)'File length, lines: ',lines

      open(21,file=f1,access='direct',recl=len*8)
      open(22,file=f2,access='direct',recl=len*8)
      open(31,file=fout,access='direct',recl=len*8)

      do i=1,lines
         read(21,rec=i)(a1(k),k=1,len)
         igrn=i+idnoff
         if(igrn.lt.1)igrn=1
         if(igrn.gt.lines)igrn=lines
         read(22,rec=igrn)(a2(k),k=1,len)
         do k=1,len
            p(k)=0.
            if(k+iacoff.gt.0.and.k+iacoff.le.len)p(k)=cabs(a2(k+iacoff))
         end do
         write(31,rec=i)(cabs(a1(k)),p(k),k=1,len)
      end do

      end
