!c  fitionophase - fit ionosphere split spectrum phase to low order polynomial
!c  third order polynomial version

      parameter (MP=100000)
      parameter (NPP=10)

      character*300 file, str, filetocorrect, ccfile
      complex*8, allocatable :: data(:,:)
      real*4, allocatable :: ccdata(:,:)
      integer*8 filelen, nbytes, lines, n, i, j
      real*8 sum, sumsq

      real*8,allocatable ::  xd(:),yd(:),sig(:),phase(:)
      real*8, allocatable :: u(:,:)

      real*8  coef(NPP),v(NPP,NPP),w(NPP)
      real*8  chisq

      integer icoef(NPP)
      common /coefcomm/icoef

      if(iargc().lt.1)then
         write(*,*)'usage: fitionophase split-spectrum-cpx len file-to-correct factor <cc file> <ccthreshold>'
         stop
      end if
      call getarg(1,file)
      call getarg(2,str)
      read(str,*)len
      if(iargc().ge.3)call getarg(3,filetocorrect)
      if(iargc().ge.4)call getarg(4,str)
      if(iargc().ge.4)read(str,*)factor
      if(iargc().ge.5)call getarg(5,ccfile)
      if(iargc().ge.6)call getarg(6,str)
      if(iargc().ge.6)read(str,*)thresh

      nbytes=filelen(file)
      lines=nbytes/len/8
      print *,'File length, lines: ',lines

      allocate (data(len,lines))
      allocate (ccdata(2*len,lines))

      open(21,file=file,form='unformatted',access='direct',recl=lines*len*8)
      read(21,rec=1)data
      close(21)
      if(iargc().ge.5)then
      open(21,file=ccfile,form='unformatted',access='direct',recl=lines*len*8)
      read(21,rec=1)ccdata
      close(21)
      end if

! get approximate average magnitude
      n=0
      sum=0.
      sumsq=0.
      do i=1,len,10
         do j=1,lines,10
            sum=sum+cabs(data(i,j))
            sumsq=sumsq+cabs(data(i,j))**2
            n=n+1
         end do
      end do
      ave=sum/n
      var=sumsq/n-ave**2
      std=sqrt(var)
      print *,'ave std ',ave,std,n

! create array to fit and put in format for svdfit
      allocate (xd(n),yd(n),phase(n),sig(n),u(n,NPP))
      nn=0

      if(iargc().ge.5)then
         do i=1,len,10
            do j=1,lines,10
               if(ccdata(i+len,j).ge.thresh)then
                  nn=nn+1
                  xd(nn)=i
                  yd(nn)=j
                  phase(nn)=atan2(aimag(data(i,j)),real(data(i,j)))
                  sig(nn)=1.
               end if
            end do
         end do
      else
         do i=1,len,10
            do j=1,lines,10
               if(cabs(data(i,j)).ge.ave/100.)then
                  nn=nn+1
                  xd(nn)=i
                  yd(nn)=j
                  phase(nn)=atan2(aimag(data(i,j)),real(data(i,j)))
                  sig(nn)=1.
               end if
            end do
         end do
      end if

!c     get mean, standard deviation of raw data
      sum=0.
      sumsq=0.
      do i=1,nn
         sum=sum+phase(i)
         sumsq=sumsq+phase(i)**2
      end do
      print *,'Raw data mean, std: ',sum/nn,sqrt(sumsq/nn-(sum/nn)**2),nn

!c     fit phases, 2D dependence
      ma=10
      icoef(1)=1
      icoef(2)=2
      icoef(3)=3
      icoef(4)=4
      icoef(5)=5
      icoef(6)=6
      icoef(7)=7
      icoef(8)=8
      icoef(9)=9
      icoef(10)=10
!!$      do i=1,nn,84
!!$         print *,i,xd(i),yd(i),phase(i),sig(i)
!!$      end do
      call svdfit(xd,yd,phase,sig,nn,coef,ma,u,v,w,nn,NPP,chisq)
!!$c     write(*,*)'chi square: ',chisq
      open(21,file='ionocoeffs')
      write(*,*)'coefficients: '
      do j=1,ma
         write(*,*)j,coef(j)
         write(21,*)j,coef(j)
      end do
      close(21)
      print *

      slpdn=coef(2)
      slpac=coef(3)
      c=coef(1)
      write(*,*)
      write(*,*)'         Slope down  Slope across  Intercept: '
      write(*,*)'Phase:  ',slpdn,slpac,c
      print *

!c  create a fit residual file
      open(21,file='phaseresidual',form='unformatted',access='direct',recl=lines*len*8)
      do i=1,len
         do j=1,lines
            phaseest=coef(10)*i**3+coef(9)*j**3+coef(8)*i**2*j+coef(7)*i*j**2+ &
                 coef(6)*(i**2)+coef(5)*(j**2)+ &
                 coef(4)*i*j+coef(3)*i+coef(2)*j+coef(1)

            data(i,j)=data(i,j)*cmplx(cos(phaseest),-sin(phaseest))
         end do
      end do
      write(21,rec=1)data
      close(21)

!c  correct an interferogram if you want
      if(iargc().ge.3)then
         open(21,file=filetocorrect,form='unformatted',access='direct',recl=lines*len*8)
         read(21,rec=1)data
         close(21)
         do i=1,len
            do j=1,lines
               phaseest=coef(10)*i**3+coef(9)*j**3+coef(8)*i**2*j+coef(7)*i*j**2+ &
                    coef(6)*(i**2)+coef(5)*(j**2)+ &
                    coef(4)*i*j+coef(3)*i+coef(2)*j+coef(1)
               phaseest=phaseest*factor
               data(i,j)=data(i,j)*cmplx(cos(phaseest),-sin(phaseest))
            end do
         end do
         open(21,file='ionocorrected',access='direct',recl=lines*len*8)
         write(21,rec=1)data
         close(21)
      end if

!c  correct an interferogram with no fit
      open(21,file=file,form='unformatted',access='direct',recl=lines*len*8)
      read(21,rec=1)data
      close(21)
      do i=1,len
         do j=1,lines
            phaseest=atan2(aimag(data(i,j)),real(data(i,j)))
            phaseest=phaseest*factor
            data(i,j)=data(i,j)*cmplx(cos(phaseest),-sin(phaseest))
         end do
      end do
      open(21,file='ionocorrectednofit',access='direct',recl=lines*len*8)
      write(21,rec=1)data
      close(21)

      end


	 subroutine funcs(x,y,afunc,ma)

         integer icoef(10)
         common /coefcomm/icoef


         real*8 afunc(ma),x,y
         real*8 cf(10)

         data cf( 1) /0./
         data cf( 2) /0./
         data cf( 3) /0./
         data cf( 4) /0./
         data cf( 5) /0./
         data cf( 6) /0./
         data cf( 7) /0./
         data cf( 8) /0./
         data cf( 9) /0./
         data cf( 10) /0./

        do i=1,ma
             cf(icoef(i))=1.
             afunc(i)=cf(10)*x**3+cf(9)*y**3+cf(8)*x**2*y+cf(7)*x*y**2+ &
                      cf(6)*(x**2)+cf(5)*(y**2)+cf(4)*x*y+ &
                      cf(3)*x+cf(2)*y+cf(1)
             cf(i)=0.
        end do

	return
	end    

      subroutine svdfit(x,y,z,sig,ndata,a,ma,u,v,w,mp,np,chisq)
      implicit real*8 (a-h,o-z)
      parameter(nmax=300000,mmax=10,tol=1.e-6)
      dimension x(ndata),y(ndata),z(ndata),sig(ndata),a(ma),v(np,np), &
          u(mp,np),w(np),b(nmax),afunc(mmax)
      write(*,*)'evaluating basis functions...',ndata,ma
      do 12 i=1,ndata
        call funcs(x(i),y(i),afunc,ma)
        tmp=1./sig(i)
        do 11 j=1,ma
          u(i,j)=afunc(j)*tmp
11      continue
        b(i)=z(i)*tmp
12    continue
      write(*,*)'SVD...'
      call svdcmp(u,ndata,ma,mp,np,w,v)
      wmax=0.
      do 13 j=1,ma
        if(w(j).gt.wmax)wmax=w(j)
13    continue
      thresh=tol*wmax
	write(*,*)'eigen value threshold',thresh
      do 14 j=1,ma
!c	write(*,*)j,w(j)
        if(w(j).lt.thresh)w(j)=0.
14    continue
!c      write(*,*)'calculating coefficients...'
      call svbksb(u,w,v,ndata,ma,mp,np,b,a)
      chisq=0.
!c      write(*,*)'evaluating chi square...'
      do 16 i=1,ndata
        call funcs(x(i),y(i),afunc,ma)
        sum=0.
        do 15 j=1,ma
          sum=sum+a(j)*afunc(j)
15      continue
        chisq=chisq+((z(i)-sum)/sig(i))**2
16    continue
      return
      end

      subroutine svbksb(u,w,v,m,n,mp,np,b,x)
      implicit real*8 (a-h,o-z)
      parameter (nmax=100)
      dimension u(mp,np),w(np),v(np,np),b(mp),x(np),tmp(nmax)
      do 12 j=1,n
        s=0.
        if(w(j).ne.0.)then
          do 11 i=1,m
            s=s+u(i,j)*b(i)
11        continue
          s=s/w(j)
        endif
        tmp(j)=s
12    continue
      do 14 j=1,n
        s=0.
        do 13 jj=1,n
          s=s+v(j,jj)*tmp(jj)
13      continue
        x(j)=s
14    continue
      return
      end

      subroutine svdcmp(a,m,n,mp,np,w,v)
      implicit real*8 (a-h,o-z)
      parameter (nmax=100)
      dimension a(mp,np),w(np),v(np,np),rv1(nmax)
      g=0.0
      scale=0.0
      anorm=0.0
      do 25 i=1,n
        l=i+1
        rv1(i)=scale*g
        g=0.0
        s=0.0
        scale=0.0
        if (i.le.m) then
          do 11 k=i,m
            scale=scale+abs(a(k,i))
11        continue
          if (scale.ne.0.0) then
            do 12 k=i,m
              a(k,i)=a(k,i)/scale
              s=s+a(k,i)*a(k,i)
12          continue
            f=a(i,i)
            g=-sign(sqrt(s),f)
            h=f*g-s
            a(i,i)=f-g
            if (i.ne.n) then
              do 15 j=l,n
                s=0.0
                do 13 k=i,m
                  s=s+a(k,i)*a(k,j)
13              continue
                f=s/h
                do 14 k=i,m
                  a(k,j)=a(k,j)+f*a(k,i)
14              continue
15            continue
            endif
            do 16 k= i,m
              a(k,i)=scale*a(k,i)
16          continue
          endif
        endif
        w(i)=scale *g
        g=0.0
        s=0.0
        scale=0.0
        if ((i.le.m).and.(i.ne.n)) then
          do 17 k=l,n
            scale=scale+abs(a(i,k))
17        continue
          if (scale.ne.0.0) then
            do 18 k=l,n
              a(i,k)=a(i,k)/scale
              s=s+a(i,k)*a(i,k)
18          continue
            f=a(i,l)
            g=-sign(sqrt(s),f)
            h=f*g-s
            a(i,l)=f-g
            do 19 k=l,n
              rv1(k)=a(i,k)/h
19          continue
            if (i.ne.m) then
              do 23 j=l,m
                s=0.0
                do 21 k=l,n
                  s=s+a(j,k)*a(i,k)
21              continue
                do 22 k=l,n
                  a(j,k)=a(j,k)+s*rv1(k)
22              continue
23            continue
            endif
            do 24 k=l,n
              a(i,k)=scale*a(i,k)
24          continue
          endif
        endif
        anorm=max(anorm,(abs(w(i))+abs(rv1(i))))
25    continue
      do 32 i=n,1,-1
        if (i.lt.n) then
          if (g.ne.0.0) then
            do 26 j=l,n
              v(j,i)=(a(i,j)/a(i,l))/g
26          continue
            do 29 j=l,n
              s=0.0
              do 27 k=l,n
                s=s+a(i,k)*v(k,j)
27            continue
              do 28 k=l,n
                v(k,j)=v(k,j)+s*v(k,i)
28            continue
29          continue
          endif
          do 31 j=l,n
            v(i,j)=0.0
            v(j,i)=0.0
31        continue
        endif
        v(i,i)=1.0
        g=rv1(i)
        l=i
32    continue
      do 39 i=n,1,-1
        l=i+1
        g=w(i)
        if (i.lt.n) then
          do 33 j=l,n
            a(i,j)=0.0
33        continue
        endif
        if (g.ne.0.0) then
          g=1.0/g
          if (i.ne.n) then
            do 36 j=l,n
              s=0.0
              do 34 k=l,m
                s=s+a(k,i)*a(k,j)
34            continue
              f=(s/a(i,i))*g
              do 35 k=i,m
                a(k,j)=a(k,j)+f*a(k,i)
35            continue
36          continue
          endif
          do 37 j=i,m
            a(j,i)=a(j,i)*g
37        continue
        else
          do 38 j= i,m
            a(j,i)=0.0
38        continue
        endif
        a(i,i)=a(i,i)+1.0
39    continue
      do 49 k=n,1,-1
        do 48 its=1,30
          do 41 l=k,1,-1
            nm=l-1
            if ((abs(rv1(l))+anorm).eq.anorm)  go to 2
            if ((abs(w(nm))+anorm).eq.anorm)  go to 1
41        continue
1         c=0.0
          s=1.0
          do 43 i=l,k
            f=s*rv1(i)
            if ((abs(f)+anorm).ne.anorm) then
              g=w(i)
              h=sqrt(f*f+g*g)
              w(i)=h
              h=1.0/h
              c= (g*h)
              s=-(f*h)
              do 42 j=1,m
                y=a(j,nm)
                z=a(j,i)
                a(j,nm)=(y*c)+(z*s)
                a(j,i)=-(y*s)+(z*c)
42            continue
            endif
43        continue
2         z=w(k)
          if (l.eq.k) then
            if (z.lt.0.0) then
              w(k)=-z
              do 44 j=1,n
                v(j,k)=-v(j,k)
44            continue
            endif
            go to 3
          endif
          if (its.eq.30) print *, '***** no convergence in 30 iterations *****'
          x=w(l)
          nm=k-1
          y=w(nm)
          g=rv1(nm)
          h=rv1(k)
          f=((y-z)*(y+z)+(g-h)*(g+h))/(2.0*h*y)
          g=sqrt(f*f+1.0)
          f=((x-z)*(x+z)+h*((y/(f+sign(g,f)))-h))/x
          c=1.0
          s=1.0
          do 47 j=l,nm
            i=j+1
            g=rv1(i)
            y=w(i)
            h=s*g
            g=c*g
            z=sqrt(f*f+h*h)
            rv1(j)=z
            c=f/z
            s=h/z
            f= (x*c)+(g*s)
            g=-(x*s)+(g*c)
            h=y*s
            y=y*c
            do 45 nm=1,n
              x=v(nm,j)
              z=v(nm,i)
              v(nm,j)= (x*c)+(z*s)
              v(nm,i)=-(x*s)+(z*c)
45          continue
            z=sqrt(f*f+h*h)
            w(j)=z
            if (z.ne.0.0) then
              z=1.0/z
              c=f*z
              s=h*z
            endif
            f= (c*g)+(s*y)
            x=-(s*g)+(c*y)
            do 46 nm=1,m
              y=a(nm,j)
              z=a(nm,i)
              a(nm,j)= (y*c)+(z*s)
              a(nm,i)=-(y*s)+(z*c)
46          continue
47        continue
          rv1(l)=0.0
          rv1(k)=f
          w(k)=x
48      continue
3       continue
49    continue
      return
      end
