! run ps detection using cosine similarlity on a set of wrapped files
! Read also correlation and amp files
!  this version allows for averaging multiple reference points if file is supplied
!  modified to use fftw3 28jan23

PROGRAM cosine_sim
  use omp_lib

  IMPLICIT none

  !specifications
  INTEGER::i,j,r,c,rows,cols,stat,fstat,ierr,looks,k,kk,n_refs,igramnumber,iter
  real*8 jdprimary, jdsecondary
  INTEGER*8 ::nr, naz, reclphase, recsize, planf, plani !image size
  INTEGER::ncells !number of cells (files in flist)
  INTEGER,DIMENSION(13)::statb
  CHARACTER(200)::filelist,str,ref_locs_file, outfile
  CHARACTER(200),DIMENSION(:),ALLOCATABLE::cells !array of file names
  CHARACTER(200)::strint,strunw,stramp,strcc
  REAL,DIMENSION(:,:,:),ALLOCATABLE::phase,amps !,coh,temp3
  REAL,DIMENSION(:,:),ALLOCATABLE::amp,dat,mask,temp2,stack,stacktime,scr,similarity,allsimilarity
  complex*8,dimension(:,:,:),allocatable :: igrams
  real*4 psthresh,simthresh,similaritymean
  integer*1,dimension(:,:),allocatable :: bytemask

  !executions

  !flist: list of wrapped intrerferogram files
  !nfiles: number of files in flist
  !nslcs: number of slc files
  !len: width of the files
  IF(iargc().lt.3)THEN
     WRITE(*,*)'usage: cosine_sim igramlist len outfile <psthresh=2> <simthresh=0.5>'
     STOP
  END IF

  CALL getarg(1,filelist)
  CALL getarg(2,str)
  READ(str,*)nr !width of files
  call getarg(3,outfile)
  psthresh=2.
  simthresh=0.5
  if(iargc().ge.4)then
     call getarg(4,str)
     read(str,*)psthresh
  end if
  if(iargc().ge.5)then
     call getarg(5,str)
     read(str,*)simthresh
  end if

  ALLOCATE(cells(10000))

  OPEN(UNIT=1,FILE=filelist,STATUS='old')
  do ncells=1,10000
     READ(1,'(A)',end=10,IOSTAT=stat)cells(ncells)
  end do
!  PRINT*,cells(1:10) !print file names
10 continue
  CLOSE(1)
  ncells=ncells-1

  OPEN(UNIT=2,FILE=cells(1),FORM='unformatted',ACCESS='direct',RECL=nr*8)
  ierr=fstat(2,statb)
  naz=statb(8)/8/nr
  CLOSE(2)
  print *,'Number of interferograms: ',ncells,', Lines per file: ',naz

  ALLOCATE(igrams(nr,naz,ncells),scr(nr,naz),similarity(nr,naz),allsimilarity(nr,naz))
  ALLOCATE(amp(nr,naz),bytemask(nr,naz),amps(nr,naz,ncells))

  !read in wrapped igrams
  !$omp parallel do private(strint) shared(igrams,ncells,amps)
  DO i=1,ncells
     strint=cells(i)
     OPEN(UNIT=i+10,FILE=strint,FORM='unformatted',ACCESS='direct',RECL=2*nr*naz*4,CONVERT='LITTLE_ENDIAN')  
     READ(i+10,rec=1)igrams(:,:,i)
     CLOSE(i+10)
     amps(:,:,i)=cabs(igrams(:,:,i))**2
!     amp=amp+cabs(igrams(:,:,i))**2
  END DO
  !$omp end parallel do
  amp=sqrt(sum(amps,3)/ncells) ! save amplitude average for later use if desred
  deallocate (amps)

  ! first step is to get candidate points from mle
  ! compute scr for each point and free up some memory
  r=nr
  c=naz
  call mlesub(igrams,r,c,ncells,scr)
  allocate(phase(nr,naz,ncells))
  !$omp parallel do shared(phase,igrams,ncells)
  do i=1,ncells
     phase(:,:,i)=atan2(aimag(igrams(:,:,i)),real(igrams(:,:,i)))
  end do
  !$omp end parallel do
  deallocate (igrams)

!Save nr, naz, nslc, ncells parameters in one file
  OPEN(15,FILE='cosine_sim_parameters',STATUS='replace')
  WRITE(15,*) nr, naz, ncells, psthresh, simthresh
  CLOSE(15)

  PRINT*,'Mle pass complete'
  ! save scr for later analysis
  open(15,file='scr_floats',access='stream')
  write(15)scr
  close(15)
  open(15,file='scr',access='stream')
  do i=1,naz
     write(15)amp(:,i),scr(:,i)
  end do
  close(15)
  print *,'Saved scr estimates as floats (scr_floats) and mht (scr) formats'

  ! save byte mask for scr's above threshold
  bytemask=0
  k=0
  do j=1,naz
     do i=1,nr
        if(scr(i,j).ge.psthresh)then
           bytemask(i,j)=-1
           k=k+1
        end if
     end do
  end do
  open(15,file='scr_mask',access='stream')
  write(15)bytemask
  close(15)
  print *,'Saved scr mask as scr_mask, number= ',k

  ! step 2.  remove false positives by cosine similarity filter 
  ! compute median_similarity - for each ps pixel (>th), compute its phase similarity with
  !  nearby PS and return the median of similarity measurements

  call median_similarity(phase,scr,r,c,ncells,psthresh,similarity)
  ! save median similarity for later analysis
  open(15,file='median_similarity',access='stream')
  write(15)similarity
  close(15)
  print *,'Saved median similarity (median_similarity) as floats with ps threshold ',psthresh

  ! save byte mask for median similarity above threshold
  bytemask=0
  k=0
  similaritymean=0.
  do j=1,naz
     do i=1,nr
        if(similarity(i,j).ge.simthresh)then
           bytemask(i,j)=-1
           k=k+1
           similaritymean=similaritymean+similarity(i,j)
        end if
     end do
  end do
  similaritymean=similaritymean/k
  open(15,file='median_sim_mask',access='stream')
  write(15)bytemask
  close(15)
  print *,'Saved median similarity mask as median_sim_mask, number, mean= ',k,similaritymean

  ! step 3.  scan all ppoints for similarity with refined ps set, adding extra discoveries
  ! and now get the similarity everywhere, adding in newly discovered points
!  call all_similarity(phase,similarity,r,c,ncells,simthresh,allsimilarity)

  do iter=1,3
     print *,'Iteration ',iter
     if(iter.gt.1)similarity=allsimilarity
     call all_similarity(phase,similarity,r,c,ncells,simthresh,allsimilarity)
!     print *,'finished ',iter
  end do

  ! save in mht format
  open(15,file=outfile,access='stream')
  do i=1,naz
     write(15)amp(:,i),allsimilarity(:,i)
  end do
  ! and also as floats
  open(15,file='all_similarity',access='stream')
  write(15)allsimilarity
  close(15)
  print *,'Saved final similarity as floats (all_similarity) and mht ',trim(outfile),' with sim threshold ',simthresh
  ! and finally a byte mask over the sim_thresh
  bytemask=0
  k=0
  similaritymean=0.
  kk=0
 do j=1,naz
     do i=1,nr
        if(allsimilarity(i,j).ge.simthresh)then
           bytemask(i,j)=-1
           k=k+1
           similaritymean=similaritymean+similarity(i,j)
        end if
        if(abs(allsimilarity(i,j)).ge.1.e-3)then
           kk=kk+1
        end if
     end do
  end do
  similaritymean=similaritymean/k
  open(15,file='all_similarity_mask',access='stream')
  write(15)bytemask
  close(15)
  print *,'Saved all_similarity mask as all_similarity_mask, number, mean= ',k,similaritymean
  print *,'Fractional coverage: ',float(kk)/float(naz)/float(nr)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!FUNCTIONS

CONTAINS

  !strrep(str,orig,rep): replaces all instaces of orig in str with rep
  CHARACTER(30) FUNCTION strrep(str,orig,rep)
    IMPLICIT none
    !specifications
    INTEGER::i
    CHARACTER(100),INTENT(IN)::str
    CHARACTER(2),INTENT(IN)::orig,rep
    !executions
    strrep(:) = str(:)
    DO i=1,LEN(strrep)-1
       IF(strrep(i:i+1) == orig(:))THEN
          strrep(i:i+1) = rep(:)
       END IF
    END DO
  END FUNCTION strrep

! ------------------
FUNCTION Replace_Text (s,text,rep)  RESULT(outs)
CHARACTER(*)        :: s,text,rep
CHARACTER(LEN(s)+100) :: outs     ! provide outs with extra 100 char len
INTEGER             :: i, nt, nr

outs = s ; nt = LEN_TRIM(text) ; nr = LEN_TRIM(rep)
DO
   i = INDEX(outs,text(:nt)) ; IF (i == 0) EXIT
   outs = outs(:i-1) // rep(:nr) // outs(i+nt:)
END DO
END FUNCTION Replace_Text

  !mean2d(mat): calculates the mean value of a 2D matrix mat with r rows and c columns
  REAL FUNCTION mean2d(mat,rows,cols)
    IMPLICIT none
    REAL,INTENT(IN),DIMENSION(:,:) :: mat
    INTEGER :: r,c,rows,cols
    mean2d=0
    DO r=1,rows
       DO c=1,cols
          mean2d=mean2d+mat(r,c)
       END DO
    END DO
    mean2d=mean2d/SIZE(mat)
  END FUNCTION mean2d

  subroutine mlesub(igrams,len,lines,nigrams,scrout)
!c  mlestack - apply mle estimator to phases from stacked time series of igrams

    use omp_lib
    implicit none
    complex*8, allocatable :: in(:,:),igram(:,:),igramfilt(:,:),filt(:,:),work(:,:)
    complex*8 csum,igrams(len,lines,nigrams)
    complex*8, allocatable :: valid(:)
    real*4, allocatable :: out(:),phase(:)
    real*4 scrout(len,lines)
    real*4 pdfs(200,100),prob(100)
    real*4 pi,rho,rhoest,thresh,probmax,pow,ph,screst,x,y,dx,aveps,avemag,box,aveph,amp,scr
    integer*4, allocatable :: iph(:)
    character*100 flist,fin(10000),fout,str
    integer statb(13),fstat,len,lines,nigrams,ibox,nvalid,kmax,krho,ixfft,iyfft,iy,ix,line,ixbins

    !$omp parallel
    ! n=omp_get_num_threads()
    !$omp end parallel
    !print *, 'Max threads used: ', n
     
    ! default to box size 10
    box=10.

!    print *,'Interferograms analyzed: ',nigrams

!c  get sizes for ffts
      do i=1,20
         if(len.gt.2**i)ixfft=2**i
         if(lines.gt.2**i)iyfft=2**i
      end do
      ixfft=ixfft*2
      iyfft=iyfft*2
!      print *,'FFT sizes: ',ixfft,iyfft

!c  allocate memory
      allocate(out(len*2))
      allocate(in(len,nigrams))
      allocate(phase(nigrams))
      allocate(iph(nigrams))
      allocate(valid(nigrams))
      allocate(filt(ixfft,iyfft))

!c  precompute slc pdfs as function of rho and theta
      ixbins=200
      pi=4*atan2(1.,1.)
      dx=2*pi/ixbins

      do k=1,100
         rho=(k-1)/100.
         scr=1/(1./rho-1.)
!         print *,rho,scr
         do i=1,ixbins
            ph=-pi+dx/2.+(i-1)*dx
            pdfs(i,k)=1./2./pi*exp(-scr*(sin(ph)**2))*(exp(-scr*cos(ph)**2)+ &
            sqrt(pi*scr)*cos(ph)*(1-erf(-sqrt(scr)*cos(ph))))
         end do
      end do

!c  make a filter directly in frequency domain
      do ix=1,ixfft
         if(ix.le.ixfft/2)then
            x=sinc((ix-1)/(ixfft/box))
         else
            x=sinc((ixfft-ix+1)/(ixfft/box))
         end if
         
         do iy=1,iyfft
            if(iy.le.iyfft/2)then
               y=sinc((iy-1)/(iyfft/box))
            else
               y=sinc((iyfft-iy+1)/(iyfft/box))
            end if

            filt(ix,iy)=cmplx(x*y, 0.)

         end do
      end do

      filt=conjg(filt)

!c  save filter for debug purposes
!      print *,'Writing filter, sizes: ',ixfft,iyfft
!      open(24,file='filter',access='direct',recl=ixfft*iyfft*8)
!      write(24,rec=1)filt
!      close(24)
!      open(99,file='inmlesub',access='stream')
!      write(99)igrams(:,:,1)
!      close(99)
!      print *,'Filtering interferograms '
      allocate(igramfilt(ixfft,iyfft))
      call sfftw_plan_dft_2d(planf,ixfft,iyfft,igramfilt,igramfilt,-1,64)
      call sfftw_plan_dft_2d(plani,ixfft,iyfft,igramfilt,igramfilt,+1,64)
      deallocate(igramfilt)
      !$omp parallel do private(i,igram,igramfilt,kk,k) &
      !$omp shared(nigrams,lines,len,filt,ixfft,iyfft,fin,igrams,planf,plani)
      do i=1,nigrams
         if(mod(i,100).eq.1)print *,'Filtering igram ',i
         allocate(igramfilt(ixfft,iyfft))
         igramfilt=cmplx(0.,0.)
         igramfilt(1:len,1:lines)=igrams(:,:,i)
         call sfftw_execute_dft(planf,igramfilt,igramfilt)
         igramfilt=igramfilt*filt
         call sfftw_execute_dft(plani,igramfilt,igramfilt)
         igramfilt(1:len,1:lines)=igramfilt(1:len,1:lines)/cabs(igramfilt(1:len,1:lines))
         do k=1,len
            do kk=1,lines
               if(isnan(real(igramfilt(k,kk))).or.isnan(aimag(igramfilt(k,kk))))then
                  igramfilt(k,kk)=cmplx(0.,0.)
               end if
            end do
         end do
         igrams(1:len,1:lines,i)=igrams(1:len,1:lines,i)*conjg(igramfilt(1:len,1:lines))!/cabs(igramfilt(1:len,1:lines))
         deallocate (igramfilt)
      end do
      !$omp end parallel do
      call sfftw_destroy_plan(planf)
      call sfftw_destroy_plan(plani)

!c  open output files for debug if desired
!      fout='q'
!      open(20,file=fout,form='unformatted',access='direct',recl=len*8)
!      open(21,file=trim(fout)//'psonly',form='unformatted',access='direct',recl=len*4)

!c  loop over lines
      !$omp parallel do private(line,in,csum,i,k,pow,amp,phase,aveph) &
      !$omp private(iph,probmax,krho,prob,kmax,rhoest,screst,out) &
      !$omp private(nvalid,valid,thresh,avemag) & 
      !$omp shared(lines,nigrams,len,pi,ixbins,pdfs,igrams,scrout)
      do line=1,lines
        if(mod(line,1000).eq.1)print *,'At line: ',line

         !c  read line from each interferogram
         do i=1,nigrams
            in(:,i)=igrams(:,line,i)
!           read(100+i,rec=line)in(:,i)
         end do

         !c  loop over pixels in the line
         do k=1,len
            !c make a list of valid points
            avemag=0.
            do i=1,nigrams
               avemag=avemag+cabs(in(k,i))
            end do
            thresh=avemag/1000.
            nvalid=0
            do i=1,nigrams
               if (cabs(in(k,i)).ge.thresh)then
                  nvalid=nvalid+1
                  valid(nvalid)=in(k,i)
               end if
            end do
            !c  force each sequence to zero mean
            csum=sum(valid(1:nvalid))
            csum=csum/cabs(csum)
            valid(1:nvalid)=valid(1:nvalid)*conjg(csum)

            !c  sqrt(average powers) to store as amplitude value
            pow=0.
            pow=sum(cabs(valid(1:nvalid))**2)
            amp=sqrt(pow/nvalid)
            if(isnan(amp))amp=0.
            !c  get the phase series
            phase(1:nvalid)=atan2(aimag(valid(1:nvalid)),real(valid(1:nvalid)))
!            print *,valid(1:nvalid)
            !c  apply mle
            aveph=sum(phase(1:nvalid))
            aveph=aveph/nvalid
            do i=1,nvalid
               iph(i)=(phase(i)+pi)/2./pi*ixbins+1
               if(iph(i).lt.-100000)iph(i)=1
            end do
            probmax=-1.e12
            !!!$omp parallel do private(krho,prob,i) 
            !!!$omp+ shared(pdfs,iph,probmax,nvalid)
            do krho=1,100
               prob(krho)=0.
               do i=1,nvalid
                  prob(krho)=prob(krho)+alog(pdfs(iph(i),krho)+1.e-12)
               end do
               if(prob(krho).ge.probmax)then
                  kmax=krho
                  probmax=prob(krho)
               end if
            end do
            !!!$omp end parallel do
            rhoest=(kmax-1)/100.
            screst=1./(1./rhoest-1.)

            !c  save result for pixel
            out(k)=amp
            out(k+len)=screst

         end do
         
         !c  write out line
!         write(20,rec=line)out
!         write(21,rec=line)(out(k),k=len+1,len*2)
         scrout(:,line)=out(len+1:len*2)
      end do
      !$omp end parallel do
      return
    end subroutine mlesub

    real function sinc(q)
      real*4 q,pi
      parameter (pi=3.14159265359)

        if(abs(q).le.1.e-6)then
           sinc=1.
        else
           sinc=sin(pi*q)/pi/q
        end if
        return
      end function sinc

      subroutine fft2d(arr,ixfft,iyfft,dir)

        integer*4 ixfft,iyfft,dir
        complex*8 arr(ixfft,iyfft)
        integer*8 planf, plani

        if(dir.eq.dir)return
        if(dir.eq.-1)then
!           print *,'forward start',ixfft,iyfft
!           call fftw2d_f77_create_plan(planf, ixfft, iyfft, -1, 8)
!           call fftwnd_f77_one(planf, arr, work)
!           call fftw_f77_destroy_plan(planf)
           call sfftw_plan_dft_2d(planf,ixfft,iyfft,arr,arr,-1,64)
           call sfftw_execute_dft(planf,arr,arr)
           call sfftw_destroy_plan(planf)
!           print *,'forward end'
        end if
        
        if(dir.eq.1)then
!           print *,'reverse start'
!           call fftw2d_f77_create_plan(plani, ixfft, iyfft, +1, 8)
!           call fftwnd_f77_one(plani, arr, work)
!           call fftw_f77_destroy_plan(plani)
           call sfftw_plan_dft_2d(plani,ixfft,iyfft,arr,arr,+1,64)
           call sfftw_execute_dft(plani,arr,arr)
           call sfftw_destroy_plan(plani)
!           print *,'reverse end'
        end if

        return
      end subroutine fft2d
           
      subroutine median_similarity(phase,scr,len,lines,nigrams,psthresh,similarity)
        !c  median similarity of each ps point with neighbors in some distance range
        ! compute median_similarity - for each ps pixel (>th), compute its phase similarity with
        !  nearby PS and return the median of similarity measurements
        
        use omp_lib
        implicit none

        real*4 scr(len,lines),similarity(len,lines),phase(len,lines,nigrams)
        real*4 rmin, rmax, psthresh, dist, cossim(20)
        integer len,lines,i,j,ii,jj,nigrams,irmin,irmax,neigh,k,boxsize
        integer*4, allocatable :: iindex(:),jindex(:)
        real*4, allocatable :: r(:)

        irmin=2
        irmax=50
        boxsize=2*irmax+1

        !  set up spiral scanning
        allocate (iindex(boxsize*boxsize),jindex(boxsize*boxsize),r(boxsize*boxsize))

        call spiralscan(boxsize,iindex,jindex,r)
        
        ! how many are we starting with?
        k=0
        do j=1,lines
           do i=1,len
              if(scr(i,j).ge.psthresh)k=k+1
           end do
        end do
!        print *,'Starting median pass with number of ps = ',k

        ! loop over all ps pixels and compute median similarity
        !$omp parallel do private(j,k,neigh,cossim,dist,ii,jj) &
        !$omp shared(lines,len,boxsize,iindex,jindex,r,irmin,irmax,scr,psthresh,nigrams,similarity,phase)
        do i=1,len
           do j=1,lines
              similarity(i,j)=0.
              if(scr(i,j).ge.psthresh)then  ! found an scr ps candidate
                 ! find neighbor ps
                 neigh=0
                 cossim=0.
                 do k=1,boxsize*boxsize
                    if(neigh.lt.20)then ! only continue if less than 20 neighbors found
                       ii=i+iindex(k)
                       jj=j+jindex(k)
                       dist=r(k)
                    !                 do ii=-irmax,irmax
                    !                    do jj=-irmax,irmax
                    !                       dist=(ii*ii+jj*jj)
                       if(dist.ge.irmin.and.dist.le.irmax)then  ! in ring to check
                          if(ii.ge.1.and.ii.le.len.and.jj.ge.1.and.jj.le.lines)then  ! in bounds
                             
                             if(scr(ii,jj).ge.psthresh)then ! found a neighbor ps candidate
                                neigh=neigh+1
                                if(neigh.le.20)then
                                   cossim(neigh)=sum(cos(phase(i,j,:)-phase(ii,jj,:)))/nigrams
                                   !else
                                   !   exit
                                end if
                             end if  !  end threshold test
                          end if  ! end in-bounds test
                       end if  !  end distance test
                    end if  ! end less than 20 test
                    !                    if(neigh.ge.20)exit
                 end do
                 !                    similarity(i,j)=0.
                 if(neigh.ge.1)then  ! if we found neighbor ps candidates
                    if(neigh.eq.1)then
                       similarity(i,j)=cossim(1)
                    else
                       call sort(neigh,cossim)
                       similarity(i,j)=cossim(neigh/2)
                    end if
!                    print *,i,j,neigh,similarity(i,j),cossim
                 end if
              end if ! end initial candidate test
           end do ! end line loop
        end do ! end pixel loop
!           open(21,file='median_sim',access='stream')  ! for debug if desired
!           write(21)similarity
!           close(21)
        
        return
      end subroutine median_similarity

      subroutine all_similarity(phase,similarity,len,lines,nigrams,simthresh,allsimilarity)
        !  all_similarity - return similarity for all pixels vis a vis ps points
        !   only add newly discovered pixels, do not remove previous ones

        use omp_lib
        implicit none

        real*4 allsimilarity(len,lines),similarity(len,lines),phase(len,lines,nigrams)
        real*4 rmin, rmax, dist, cossim(20),simthresh
        integer len,lines,i,j,ii,jj,nigrams,irmin,irmax,neigh,boxsize,k
        integer*4, allocatable :: iindex(:),jindex(:)
        real*4, allocatable :: r(:)

        irmin=2
        irmax=50
        boxsize=2*irmax+1

        !  set up spiral scanning
        allocate (iindex(boxsize*boxsize),jindex(boxsize*boxsize),r(boxsize*boxsize))

        call spiralscan(boxsize,iindex,jindex,r)

        ! loop over all pixels and find ps
        !$omp parallel do private(i,neigh,cossim,dist,ii,jj) &
        !$omp shared(lines,len,boxsize,iindex,jindex,r,irmin,irmax,similarity,psthresh,nigrams,allsimilarity)

        do j=1,lines
           if(mod(j,1000).eq.1)print *,'Computing similarity at line ',j
           do i=1,len
              allsimilarity(i,j)=0.
              if(similarity(i,j).ge.simthresh)then
                 allsimilarity(i,j)=similarity(i,j)
              else
                 ! find neighbor ps
                 neigh=0
                 cossim=0.
                 do k=1,boxsize*boxsize
                    if(neigh.lt.20)then !search until we get 20 neighbors
                       ii=i+iindex(k)
                       jj=j+jindex(k)
                       dist=r(k)
                       !             do ii=-irmax,irmax
                       !                 do jj=-irmax,irmax
                       !                    dist=(ii*ii+jj*jj)
                       !                    if(dist.ge.irmin**2.and.dist.le.irmax**2)then  ! in ring to check
                       if(dist.ge.irmin.and.dist.le.irmax)then
                          if(ii.ge.1.and.ii.le.len.and.jj.ge.1.and.jj.le.lines)then  ! in bounds
                             if(similarity(ii,jj).ge.simthresh)then ! found a neighbor
                                neigh=neigh+1
                                if(neigh.le.20)then
                                   cossim(neigh)=sum(cos(phase(i,j,:)-phase(ii,jj,:)))/nigrams
                                   !                          else
                                   !                             exit
                                end if
                             end if  !  end threshold test
                          end if  ! end in-bounds test
                       end if  !  end distance test
                    end if  ! end test if reached 20 neighbors
                 end do
                 !                 if(neigh.ge.20)exit
                 !             end do
                 !              allsimilarity(i,j)=0.
                 if(neigh.ge.1)then
                    if(neigh.eq.1)then
                       allsimilarity(i,j)=cossim(1)
                    else
                       allsimilarity(i,j)=sum(cossim(1:neigh))/neigh
                    end if
                    ! print *,neigh,i,j,similarity(i,j)
                 end if
              end if  ! finished looking for neighbors option
           end do  ! end of pixel loop
        end do  !end of line loop
        !$omp end parallel do
!        open(21,file='all_sim',access='stream')  ! for dbug if desired
!        write(21)allsimilarity
!        close(21)

        ! count number above threshold after computation
        k=0
        do j=1,lines
           do i=1,len
              if(allsimilarity(i,j).ge.simthresh)k=k+1
           end do
        end do
        print *,'Number of similarity pixels found: ',k

        return
      end subroutine all_similarity

      SUBROUTINE SORT(N,RA)
        integer N,IR,J,L,I
        real*4 ra(N),rra

!      DIMENSION RA(N)
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

      subroutine spiralscan(boxsize,iindex,jindex,r)

        integer boxsize,x,y,n,count
        integer*4 iindex(boxsize*boxsize),jindex(boxsize*boxsize)
        integer*4, allocatable :: array(:,:),scan(:,:),xx(:)
        real*4  r(boxsize*boxsize)

        allocate (array(boxsize,boxsize),scan(boxsize,boxsize),xx(boxsize))
        do i=1,boxsize
           xx(i)=i-boxsize/2-1
        end do

        x=0
        y=1
        n=0
        count=boxsize;
        do i = 1,count
           x = x + 1
           array(x,y) = n
           n = n + 1
        end do
        do
           count = count  - 1
           do i = 1,count
              y = y + 1
              array(x,y) = n
              n = n + 1
           end do
           do i = 1,count
              x = x - 1
              array(x,y) = n
              n = n + 1
           end do
           if (n > boxsize*boxsize-1) exit
           count = count - 1
           do i = 1,count
              y = y - 1
              array(x,y) = n
              n = n + 1
           end do
           do i = 1,count
              x = x + 1
              array(x,y) = n
              n = n + 1
           end do
           if (n > boxsize*boxsize-1) exit
        end do
        scan=boxsize*boxsize-array
        
!c  create list of indices in scan order
        do i=1,boxsize
           do j=1,boxsize
              iindex(scan(i,j))=xx(i)
              jindex(scan(i,j))=xx(j)
           end do
        end do
        !c   the distance between center point and each scanned pixels
        do k = 1, boxsize*boxsize
           r(k) = sqrt(real(iindex(k))**2.+real(jindex(k))**2.)
        end do

      return
    end subroutine spiralscan

END PROGRAM cosine_sim

