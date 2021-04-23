c  medfilt - median value filter an i2 image

      integer*2 in(10801,10801),out(10801*2)
      integer statb(13),stat
      dimension t(1000)
      character*30 f,ff

      if(iargc().lt.4)then
         print *,'usage: medfilti2 i*2-infile i*2-outfile width filt-size(odd)'
         stop
      end if

      call getarg(4,f)
      read(f,*)nsize
      call getarg(3,f)
      read(f,*)npix
      call getarg(2,ff)
      call getarg(1,f)

      ierr=stat(f,statb)
      nbytes=statb(8)
      nlines=nbytes/npix/2
      print *,'Lines: ',nlines
      open(21,file=f,form='unformatted',status='old',
     +   access='direct',recl=npix*2)

      open(22,file=ff,form='unformatted',status='unknown',
     +   access='direct',recl=npix*2)
      do i=1,nlines
         read(21,rec=i)(in(k,i),k=1,npix)
      end do
      print *,'Data read in, computing starts.'

      do k=1,npix
         out(k)=0
      end do
      do i=1,nsize/2
         write(22,rec=i)(out(k),k=1,npix)
      end do
      print *,'Zerolines at top written.'

      do i=nsize/2+1,nlines-nsize/2-1
         !print *,i
         if(mod(i,32).eq.0)print *,i
         do j=nsize/2,npix-nsize/2
            k=0
            do ii=-nsize/2,nsize/2
c  put values in a vector
               do jj=-nsize/2,nsize/2
                  k=k+1
                  t(k)=in(j+jj+npix,i+ii)
               end do
            end do
c  sort
            call sort(nsize*nsize,t)
            !print *,'sorted ',j,nsize,t(nsize*nsize/2),i,j+npix
            !out(j)=in(j,i)
            out(j)=t(nsize*nsize/2)
            !print *,'saved into out ',j,i,j+npix
         end do
         write(22,rec=i)(out(k),k=1,npix)
      end do
      do k=1,npix
         out(k)=0
      end do
c  keep output file the right size
      do i=nlines-nsize/2,nlines
         write(22,rec=i)(out(k),k=1,npix)
      end do

      end

      SUBROUTINE SORT(N,RA)
      DIMENSION RA(N)
      L=N/2+1
      IR=N
10    CONTINUE
        IF(L.GT.1)THEN
          L=L-1
          RRA=RA(L)
        ELSE
          RRA=RA(IR)
          RA(IR)=RA(1)
          IR=IR-1
          IF(IR.EQ.1)THEN
            RA(1)=RRA
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J.LE.IR)THEN
          IF(J.LT.IR)THEN
            IF(RA(J).LT.RA(J+1))J=J+1
          ENDIF
          IF(RRA.LT.RA(J))THEN
            RA(I)=RA(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        RA(I)=RRA
      GO TO 10
      END
