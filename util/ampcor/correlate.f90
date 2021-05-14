!c****************************************************************

      subroutine correlate(r_imgi,r_imgj,i_wsxi,i_wsyi,i_wsxj,i_wsyj, &
                 i_avgx,i_avgy,i_ovs,r_meani, &
                 r_stdvi,r_meanj,r_stdvj,r_peak,r_noise,r_cov,r_eval1, &
                 r_eval2,r_evec1,r_evec2,r_imgc, &
                 i_shftx,i_shfty,i_edge,i_flag,l_debug)
      
!c****************************************************************
!c**   
!c**   FILE NAME: correlate.f
!c**   
!c**   DATE WRITTEN: /10/10/92
!c**   
!c**   PROGRAMMER:Scott Hensley / Scott Shaffer
!c**   
!c**   FUNCTIONAL DESCRIPTION: This routine will do amplitude correlation
!c**     
!c**   on two specified input files.
!c**   
!c**   ROUTINES CALLED:none
!c**   
!c**   NOTES: none
!c**   
!c**   UPDATE LOG:
!c**   
!c**   Date      Description                              Person
!c**   ----      -----------                              ------
!c**   /12/12/94   Modified to work with real data.         SH
!c**   /02/22/95   Modified to work oversampled data.      SS/SH
!c**   
!c*****************************************************************
      
      use omp_lib

      implicit none
      
!c     INPUT VARIABLES:
      integer i_idx,i_idy
      parameter(i_idx=2048)
      parameter(i_idy=2048)

      integer i_wsyi,i_wsxi,i_wsyj,i_wsxj,i_ovs
      integer i_avgy,i_avgx,i_wsayi,i_wsaxi
      integer i_wsayj,i_wsaxj,i_wsaxyi,i_wsaxyj

      real r_imi,r_imgi,r_imgc
      real r_imj,r_imgj
      dimension r_imi(i_idx,i_idy)
      dimension r_imgc(i_idx,i_idy)
      dimension r_imj(i_idx,i_idy)
      dimension r_imgi(i_idx,i_idy)
      dimension r_imgj(i_idx,i_idy)      

!c     OUTPUT VARIABLES:
      real*4 r_shfty,r_shftx,r_peak,r_shrp,r_meani,r_meanj
      real*4 r_stdvi,r_stdvj,r_noise,r_cov(3),r_eval1,r_sum
      real*4 r_eval2,r_evec1(2),r_evec2(2)
      
!c     LOCAL VARIABLES:
      integer i,j,m,n,ix,iy,ixx,iyy,i_shfty,i_shftx,io,k
      integer i_cnti,i_cntj,i_cntai,i_cntaj,i_edge(2),i_flag

      real r_sumc,r_sumi,r_smqi
      real r_sumj(0:i_idx,0:i_idy)
      real r_smqj(0:i_idx,0:i_idy)
      real r_crpd(0:i_idx,0:i_idy)
      real r_corr(0:i_idx,0:i_idy)
      real r_corn(0:i_idx,0:i_idy)
      real r_denom
      real acc1(0:3),acc2(0:3)

      real*4 r_dxx,r_dyy,r_dxy,r_n2,r_n4,r_u,r_u2

      logical l_init,l_debug
      
!c     DATA STATEMENTS:
      data l_init /.false./
      
!C     FUNCTION STATEMENTS:
      
!c     PROCESSING STEPS:
!      print *,'correlate called'
      !$omp parallel
!      print *, 'Max threads used: ', omp_get_num_threads()
      !$omp end parallel

      if(l_debug)then
         write(6,*) ' '
         write(6,*) ' Debug Statements ** Inputs ** '
         write(6,*) 'r_imgi(1,1),r_imgj(1,1) = ',r_imgi(1,1),r_imgj(1,1)
         write(6,*) ' r_imgi(i_wsxi,i_wsyi),r_imgj(i_wsxj,i_wsyj) = ',r_imgi(i_wsxi,i_wsyi),r_imgj(i_wsxj,i_wsyj) 
         write(6,*) 'i_wsxi and i_wsyi = ',i_wsxi,i_wsyi
         write(6,*) 'i_wsxj and i_wsyj = ',i_wsxj,i_wsyj
         write(6,*) 'i_avgx and i_avgy = ',i_avgx,i_avgy
         write(6,*) 'r_meani and r_stdvi = ',r_meani,r_stdvi
         write(6,*) 'r_meanj and r_stdvj = ',r_meanj,r_stdvj
         write(6,*) 'r_peak and r_noise = ',r_peak,r_noise
         write(6,*) 'r_shftx and r_shfty = ',r_shftx,r_shfty
         write(6,*) 'i_edge and i_flag = ',i_edge(1),i_edge(2),i_flag
      endif

      i_edge(1)=0
      i_edge(2)=0
      if ( i_avgy .le. 0 ) i_avgy=1
      if ( i_avgx .le. 0 ) i_avgx=1
      i_wsayi=i_wsyi/i_avgy
      i_wsaxi=i_wsxi/i_avgx
      i_wsayj=i_wsyj/i_avgy
      i_wsaxj=i_wsxj/i_avgx
      i_wsaxyi=i_wsayi*i_wsaxi
      i_wsaxyj=i_wsayj*i_wsaxj/i_ovs

      r_cov(1)=0.
      r_cov(2)=0.
      r_cov(3)=0. 

!c     compute mean and standard deviations on blocks 

      i_cntai = 0
      i_cntaj = 0
      r_sumi = 0.
      r_smqi = 0.
      do iy=1,i_wsayj
         do ix=1,i_wsaxj
            r_imgc(ix,iy) = 0.
            r_imi(ix,iy) = 0.
            r_imj(ix,iy) = 0.
            i_cnti=0
            i_cntj=0
            if(i_avgy .ne. 1 .or. i_avgx .ne. 1)then
               do iyy=(iy-1)*i_avgy+1,iy*i_avgy
                  do ixx=(ix-1)*i_avgx+1,ix*i_avgx
                     if ( iyy .le. i_wsyi .and. ixx .le. i_wsxi ) then
                        if ( r_imgi(ixx,iyy) .ne. 0. ) then
                           i_cnti = i_cnti+1
                           r_imi(ix,iy) = r_imi(ix,iy) + r_imgi(ixx,iyy)
                        endif
                     endif
                     if ( r_imgj(ixx,iyy) .ne. 0. ) then
                        i_cntj = i_cntj+1
                        r_imj(ix,iy) = r_imj(ix,iy) + r_imgj(ixx,iyy)
                     endif
                  enddo
               enddo
               if ( i_cnti .ne. 0 ) then
                  i_cntai = i_cntai+1
                  r_imi(ix,iy) = r_imi(ix,iy)/i_cnti
                  r_sumi = r_sumi + r_imi(ix,iy)
                  r_smqi = r_smqi + r_imi(ix,iy)**2
               endif
               if ( i_cntj .ne. 0 ) then
                  r_imj(ix,iy) = r_imj(ix,iy)/i_cntj
                  i_cntaj = i_cntaj+1
               endif
            else
               r_imj(ix,iy) = r_imgj(ix,iy)
               if(ix .le. i_wsxi .and. iy .le. i_wsyi)then
                  r_imi(ix,iy) = r_imgi(ix,iy)
                  if(r_imi(ix,iy) .ne. 0)then
                     i_cntai = i_cntai+1
                     r_sumi = r_sumi + r_imi(ix,iy)
                     r_smqi = r_smqi + r_imi(ix,iy)**2
                  endif
               endif
               if(r_imj(ix,iy) .ne. 0)then
                  i_cntaj = i_cntaj+1
               endif   
            endif               !no averaging
         enddo
      enddo


      if ( i_cntai .ne. 0 ) then
         r_meani = r_sumi/i_cntai
         r_stdvi = sqrt((r_smqi/i_cntai)-r_meani**2)
      else
         r_meani = 0.
      endif

      if (i_cntai .ge. 0.9*i_wsaxyi .and. i_cntaj .ge. 0.9*i_wsaxyj ) then !have enough real estate

         do iy=0,i_wsayj-1
            r_sumj(0,iy) = 0.
            r_smqj(0,iy) = 0.
            do io = 1,i_ovs
               r_sumj(io,iy) = 0.
               r_smqj(io,iy) = 0.
               acc1=0.
               acc2=0.
!$omp parallel do !private(acc1,acc2,k,ix) shared(i_wsaxi,i_ovs,iy,io,r_imj)
               do ix=0,(i_wsaxi-1)*i_ovs,i_ovs
                  k=omp_get_thread_num()
!                  print *,ix,k
                  acc1(k) = acc1(k) + r_imj(ix+io,iy+1)
                  acc2(k) = acc2(k) + r_imj(ix+io,iy+1)**2
!                  r_sumj(io,iy) = r_sumj(io,iy) + r_imj(ix+io,iy+1)
!                  r_smqj(io,iy) = r_smqj(io,iy) + r_imj(ix+io,iy+1)**2
               enddo
!$omp end parallel do
               r_sumj(io,iy)=acc1(0)+acc1(1)+acc1(2)+acc1(3)
               r_smqj(io,iy)=acc2(0)+acc2(1)+acc2(2)+acc2(3)
            enddo

            do ix=i_ovs+1,i_wsaxj - (i_wsaxi-1)*i_ovs
               r_sumj(ix,iy) = r_sumj(ix-i_ovs,iy) - r_imj(ix-i_ovs,iy+1) +r_imj(ix+(i_wsaxi-1)*i_ovs,iy+1)
               r_smqj(ix,iy) = r_smqj(ix-i_ovs,iy) - r_imj(ix-i_ovs,iy+1)**2 +r_imj(ix+(i_wsaxi-1)*i_ovs,iy+1)**2
            enddo
         enddo

         do ix=0,i_wsaxj - (i_wsaxi-1)*i_ovs-1
            do io=1,i_ovs
               r_sumj(ix,io-1)=0.
               r_smqj(ix,io-1)=0.
               acc1=0.
               acc2=0.
!$omp parallel do !private(acc1,acc2,k,ix) shared(i_wsaxi,i_ovs,iy,io,r_imj)
               do iy=0,(i_wsayi-1)*i_ovs,i_ovs
                  k=omp_get_thread_num()
!                  print *,ix,k
                  acc1(k) = acc1(k) +r_sumj(ix+1,iy+io-1)
                  acc2(k) = acc2(k) +r_smqj(ix+1,iy+io-1)
               enddo
!$omp end parallel do
               r_sumj(ix,io-1)=acc1(0)+acc1(1)+acc1(2)+acc1(3)
               r_smqj(ix,io-1)=acc2(0)+acc2(1)+acc2(2)+acc2(3)
            enddo

!!!$omp parallel do !private(acc1,acc2,k,ix) shared(i_wsaxi,i_ovs,iy,io,r_imj)
            do iy=i_ovs,i_wsayj - (i_wsayi-1)*i_ovs-1
               r_sumj(ix,iy) = r_sumj(ix,iy-i_ovs) - r_sumj(ix+1,iy-i_ovs)+r_sumj(ix+1,iy+(i_wsayi-1)*i_ovs)
               r_smqj(ix,iy) = r_smqj(ix,iy-i_ovs) - r_smqj(ix+1,iy-i_ovs)+r_smqj(ix+1,iy+(i_wsayi-1)*i_ovs)
            enddo
!!!$omp end parallel do
         enddo

!c         type *,' '
!c         do ix=0,i_wsaxj - (i_wsaxi-1)*i_ovs-1
!c            do iy=0,i_wsayj - (i_wsayi-1)*i_ovs-1
!c               r_sum=0.
!c               do ixx=ix+1,ix+i_wsaxi*i_ovs,i_ovs
!c                  do iyy=iy+1,iy+i_wsayi*i_ovs,i_ovs
!c                     r_sum=r_sum+r_imj(ixx,iyy)
!c                  enddo
!c               enddo
!c               type *,ix,iy,r_sumj(ix,iy),r_sum,r_sumj(ix,iy)-r_sum
!c            enddo
!c         enddo

         i_shftx = 0
         i_shfty = 0
         r_peak = -9.e27
         do m=0,i_wsaxj - (i_wsaxi-1)*i_ovs-1
            print *,'m ',m
            do n=0,i_wsayj - (i_wsayi-1)*i_ovs-1
               r_sumc = 0.
               do j=1,i_wsayi
                  acc1=0.
!!!$omp parallel do private(k,i) shared(acc1,m,n,j,i_ovs,i_wsaxi)
                  do i=1,i_wsaxi
                     k=omp_get_thread_num()
!                  print *,i,k,i_ovs,m,n,j
                     acc1(k) = acc1(k) + r_imi(i,j)*r_imj((i-1)*i_ovs+m+1,(j-1)*i_ovs+n+1)
!                     r_sumc = r_sumc + r_imi(i,j)*r_imj((i-1)*i_ovs+m+1,(j-1)*i_ovs+n+1)
                  enddo
!!!$omp end parallel do
!                  print *,j,acc1,r_sumc
                  r_sumc=r_sumc+acc1(0)+acc1(1)+acc1(2)+acc1(3)
               enddo
!               print *,'r_sumc ',r_sumc
               r_crpd(m,n) = r_sumc
               r_corr(m,n) = r_sumc - r_meani*r_sumj(m,n)
               r_denom = (r_stdvi*sqrt((r_smqj(m,n)*i_wsaxyi)-(r_sumj(m,n))**2))
               if ( r_denom .gt. 0. ) then
                  r_corn(m,n) = r_corr(m,n)/r_denom
               else
                  r_corn(m,n) = 0.
               endif
               r_imgc(m+1,n+1) = r_corn(m,n)
!c               if(i_wsxi .eq. 112)then
!c                  type*, 'r_c = ',m,n,r_corn(m,n),r_crpd(m,n),r_meani*r_sumj(m,n),
!c     +                 r_crpd(m,n)-r_meani*r_sumj(m,n),r_sumj(m,n),r_denom
!c               endif
               if ( r_peak .lt. r_corn(m,n)) then
                  r_peak = r_corn(m,n)
                  i_shftx = m
                  i_shfty = n
               endif
            enddo
         enddo

!c     commpute the curvature of the corrrelation surface to estimate the
!c     goodness of the match

         if ( r_peak .gt. 0. ) then

            ix = i_shftx
            iy = i_shfty
            if ( iy .eq. 0 .or. iy .eq. i_wsayj - (i_wsayi-1)*i_ovs-1 )i_edge(1)=1
            if ( ix .eq. 0 .or. ix .eq. i_wsaxj - (i_wsaxi-1)*i_ovs-1 )i_edge(2)=1
            r_shftx = float(ix*i_avgx)/i_ovs
            r_shfty = float(iy*i_avgy)/i_ovs
            r_meanj = r_sumj(ix,iy)/i_wsaxyi
            r_stdvj = sqrt((r_smqj(ix,iy)/i_wsaxyi)-r_meanj**2)
            r_shrp = (r_peak-(r_corn(max(ix-1,1),iy)+r_corn(min(ix+1,i_wsaxj - (i_wsaxi-1)*i_ovs-1),iy))/2.)
            i_flag = 0

            if ( ix .eq. 0 ) then
               if ( iy .eq. 0 ) then
                  r_dxx = -(r_corn(ix+1,iy)+r_corn(ix+1,iy)-2*r_corn(ix,iy))/(i_avgx**2)
                  r_dyy = -(r_corn(ix,iy+1)+r_corn(ix,iy+1)-2*r_corn(ix,iy))/(i_avgy**2)
                  r_dxy = 0.
                  r_dxx = r_dxx/4 ! added emperically
                  r_dyy = r_dyy/4
                  r_dxy = r_dxy/4
                  r_peak = r_peak/4
               else if ( iy .eq. i_wsayj - (i_wsayi-1)*i_ovs-1 ) then
                  r_dxx = -(r_corn(ix+1,iy)+r_corn(ix+1,iy)-2*r_corn(ix,iy))/(i_avgx**2)
                  r_dyy = -(r_corn(ix,iy-1)+r_corn(ix,iy-1)-2*r_corn(ix,iy))/(i_avgy**2)
                  r_dxy = 0
                  r_dxx = r_dxx/4 ! added emperically
                  r_dyy = r_dyy/4
                  r_dxy = r_dxy/4
                  r_peak = r_peak/4
               else
                  r_dxx = -(r_corn(ix+1,iy)+r_corn(ix+1,iy)-2*r_corn(ix,iy))/(i_avgx**2)
                  r_dyy = -(r_corn(ix,iy+1)+r_corn(ix,iy-1)-2*r_corn(ix,iy))/(i_avgy**2)
                  r_dxy = 2*(r_corn(ix+1,iy+1)-r_corn(ix+1,iy-1))/(4*i_avgx*i_avgy)
                  r_dxx = r_dxx/2 ! added emperically
                  r_dyy = r_dyy/2
                  r_dxy = r_dxy/2
                  r_peak = r_peak/2
               endif
            else if ( ix .eq. i_wsaxj - (i_wsaxi-1)*i_ovs-1 ) then
               if ( iy .eq. 0 ) then
                  r_dxx = -(r_corn(ix-1,iy)+r_corn(ix-1,iy)-2*r_corn(ix,iy))/(i_avgx**2)
                  r_dyy = -(r_corn(ix,iy+1)+r_corn(ix,iy+1)-2*r_corn(ix,iy))/(i_avgy**2)
                  r_dxy = 0
                  r_dxx = r_dxx/4 ! added emperically
                  r_dyy = r_dyy/4
                  r_dxy = r_dxy/4
                  r_peak = r_peak/4
               else if ( iy .eq. i_wsayj - (i_wsayi-1)*i_ovs-1 ) then
                  r_dxx = -(r_corn(ix-1,iy)+r_corn(ix-1,iy)-2*r_corn(ix,iy))/(i_avgx**2)
                  r_dyy = -(r_corn(ix,iy-1)+r_corn(ix,iy-1)-2*r_corn(ix,iy))/(i_avgy**2)
                  r_dxy = 0
                  r_dxx = r_dxx/4 ! added emperically
                  r_dyy = r_dyy/4
                  r_dxy = r_dxy/4
                  r_peak = r_peak/4
               else
                  r_dxx = -(r_corn(ix-1,iy)+r_corn(ix-1,iy)-2*r_corn(ix,iy))/(i_avgx**2)
                  r_dyy = -(r_corn(ix,iy+1)+r_corn(ix,iy-1)-2*r_corn(ix,iy))/(i_avgy**2)
                  r_dxy = 2*(r_corn(ix-1,iy-1)-r_corn(ix-1,iy+1))/(4*i_avgx*i_avgy)
                  r_dxx = r_dxx/2 ! added emperically
                  r_dyy = r_dyy/2
                  r_dxy = r_dxy/2
                  r_peak = r_peak/2
               endif
            else if ( iy .eq. 0 ) then
               r_dxx = -(r_corn(ix+1,iy)+r_corn(ix-1,iy)-2*r_corn(ix,iy))/(i_avgx**2)
               r_dyy = -(r_corn(ix,iy+1)+r_corn(ix,iy+1)-2*r_corn(ix,iy))/(i_avgy**2)
               r_dxy = 2*(r_corn(ix+1,iy+1)-r_corn(ix-1,iy+1))/(4*i_avgx*i_avgy)
               r_dxx = r_dxx/2  ! added emperically
               r_dyy = r_dyy/2
               r_dxy = r_dxy/2
               r_peak = r_peak/2
            else if ( iy .eq. i_wsayj - (i_wsayi-1)*i_ovs-1 ) then
               r_dxx = -(r_corn(ix+1,iy)+r_corn(ix-1,iy)-2*r_corn(ix,iy))/(i_avgx**2)
               r_dyy = -(r_corn(ix,iy-1)+r_corn(ix,iy-1)-2*r_corn(ix,iy))/(i_avgy**2)
               r_dxy = 2*(r_corn(ix-1,iy-1)-r_corn(ix+1,iy-1))/(4*i_avgx*i_avgy)
               r_dxx = r_dxx/2  ! added emperically
               r_dyy = r_dyy/2
               r_dxy = r_dxy/2
               r_peak = r_peak/2
            else
               r_dxx = -(r_corn(ix+1,iy)+r_corn(ix-1,iy)-2*r_corn(ix,iy))/(i_avgx**2)
               r_dyy = -(r_corn(ix,iy+1)+r_corn(ix,iy-1)-2*r_corn(ix,iy))/(i_avgy**2)
               r_dxy = (r_corn(ix+1,iy+1)+r_corn(ix-1,iy-1)-r_corn(ix+1,iy-1)-r_corn(ix-1,iy+1))/(4*i_avgx*i_avgy)
            endif

            r_n2 = max(1.-r_peak,0.e0)
            r_noise = sqrt(r_n2)
            r_dxx = r_dxx*i_wsaxyi
            r_dyy = r_dyy*i_wsaxyi
            r_dxy = r_dxy*i_wsaxyi

            r_n4 = r_n2**2
            r_n2 = r_n2*2
            r_n4 = r_n4*.5*i_wsaxyi

            r_u = r_dxy**2-r_dxx*r_dyy
            r_u2 = r_u**2       !                    *i_avgx*i_avgy/i_wsaxyi
            if ( r_u .eq. 0 ) then
               r_cov(1)=99.
               r_cov(2)=99.
               r_cov(3)=0.
               i_flag=1
            else
               r_cov(1)=(-r_n2*r_u*r_dyy+r_n4*(r_dyy**2+r_dxy**2))/r_u2
               r_cov(2)=(-r_n2*r_u*r_dxx+r_n4*(r_dxx**2+r_dxy**2))/r_u2
               r_cov(3)=((r_n2*r_u      -r_n4*(r_dxx+r_dyy))*r_dxy)/r_u2
            endif
            r_u=sqrt((r_cov(1)+r_cov(2))**2.-4.*(r_cov(1)*r_cov(2)-r_cov(3)**2))
            r_eval1=(r_cov(1)+r_cov(2)+r_u)/2.
            r_eval2=(r_cov(1)+r_cov(2)-r_u)/2.
            if ( r_eval1 .le. 0 .or. r_eval2 .le. 0 ) then
            endif
            
            if ( r_cov(3) .eq. 0 ) then
               if ( r_cov(1) .ge. r_cov(2) ) then
                  r_evec1(1)=1.
                  r_evec1(2)=0.
                  r_evec2(1)=0.
                  r_evec2(2)=1.
               else
                  r_evec1(1)=0.
                  r_evec1(2)=1.
                  r_evec2(1)=1.
                  r_evec2(2)=0.
               endif
            else
               if ( r_cov(1)-r_eval1 .ne. 0. ) then
                  r_evec1(1)=-r_cov(3)/(r_cov(1)-r_eval1)
               else
                  write(6,*) 'e vector 1 error'
                  r_evec1(1)=999.
               endif
               r_evec1(2)=1.
               r_u=sqrt(r_evec1(1)**2+r_evec1(2)**2)
               r_evec1(1)=r_evec1(1)/r_u
               r_evec1(2)=r_evec1(2)/r_u

               if ( r_cov(1)-r_eval2 .ne. 0. ) then
                  r_evec2(1)=-r_cov(3)/(r_cov(1)-r_eval2)
               else
                  write(6,*) 'e vector 2 error'
                  r_evec2(1)=999.
               endif
               r_evec2(2)=1.
               r_u=sqrt(r_evec2(1)**2+r_evec2(2)**2)
               r_evec2(1)=r_evec2(1)/r_u
               r_evec2(2)=r_evec2(2)/r_u
            endif

            r_evec1(1)=r_evec1(1)*sqrt(abs(r_eval1)) 
            r_evec1(2)=r_evec1(2)*sqrt(abs(r_eval1)) 
            r_evec2(1)=r_evec2(1)*sqrt(abs(r_eval2)) 
            r_evec2(2)=r_evec2(2)*sqrt(abs(r_eval2)) 

         else

            r_shfty=0
            r_shftx=0
            r_shrp=0.
            i_flag=1
            write(6,*) 'correlation error'

         endif

      else

         r_shfty=0
         r_shftx=0
         r_shrp=0.
         i_flag=1

      endif


      if(l_debug)then
         write(6,*) ' '
         write(6,*) 'Exit values'
         write(6,*) 'i_wsxi and i_wsyi = ',i_wsxi,i_wsyi
         write(6,*) 'i_wsxj and i_wsyj = ',i_wsxj,i_wsyj
         write(6,*) 'i_avgx and i_avgy = ',i_avgx,i_avgy
         write(6,*) 'r_meani and r_stdvi = ',r_meani,r_stdvi
         write(6,*) 'r_meanj and r_stdvj = ',r_meanj,r_stdvj
         write(6,*) 'r_peak and r_noise = ',r_peak,r_noise
         write(6,*) 'r_cov = ',r_cov(1),r_cov(2),r_cov(3)
         write(6,*) 'r_eval1 and r_eval2 = ',r_eval1,r_eval2
         write(6,*) 'r_evec1 and r_evec2 = ',r_evec1(1),r_evec1(2),r_evec2(1), r_evec2(2)
         write(6,*) 'r_shftx and r_shfty = ',r_shftx,r_shfty
         write(6,*) 'i_edge and i_flag = ',i_edge(1),i_edge(2),i_flag
      endif
      
      return

      end
