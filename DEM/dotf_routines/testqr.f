c
c     Numerical Analysis:
c     Mathematics of Scientific Computing
c     Third Edition
c     D.R. Kincaid & E.W. Cheney
c     Brooks/Cole Publ., 2002
c     Copyright (c) 1996
c
c     Section 5.3
c
c     Example of QR-factorization using Householder transformations
c
c 
c     file: ex1s53.f
c
      parameter (n=3,ia=4,m=4)
      dimension A(ia,n),Q(ia,m),U(ia,m),W(ia,m),V(m)
      data (A(i,1),i=1,m) /63.,42.,0.,126./
      data (A(i,2),i=1,m) /41.,60.,-28.,82./
      data (A(i,3),i=1,m) /-88.,51.,56.,-71./
c
      print *  
      print *,' QR-factorization using Householder transformations'
      print *,' Section 5.3, Kincaid-Cheney'
      print *
      print *,' Matrix A'
      call prtmtx(n,ia,m,A)      
c
      call QRfac(n,ia,m,A,V,U,Q,W)
c
      print *,' Matrix Q'
      call prtmtx(m,ia,m,Q)      
      call mult(n,ia,m,Q,A)
      print *,' Check A = QR'
      call prtmtx(n,ia,m,A)
c
      stop
      end
c
      subroutine QRfac(n,ia,m,A,V,U,Q,W)
c
c     QR-factorization
c
      dimension A(ia,n),Q(ia,m),U(ia,m),W(ia,m),V(m)
c
      call setoI(m,ia,m,Q)
      do 2 k=1,n
         mm = m - k + 1
         nn = n - k + 1
         call setoI(m,ia,m,W)
         call UtimesA(nn,ia,mm,A(k,k),V(k),U(k,k),W(k,k))
         print *,' Matrix A'
         call prtmtx(n,ia,m,A)
         print *,' Matrix W'
         call prtmtx(m,ia,m,W)
         call mult(m,ia,m,W,Q)
         print *,' Matrix Q'
         call prtmtx(m,ia,m,Q)
 2    continue
      call trans(m,ia,m,Q)
c
      return
      end
c
      subroutine setoI(n,ia,m,Q)
c
c     set Q to I
c
      dimension Q(ia,n)
c
      do 3 i=1,m
         do 2 j=1,n
            Q(i,j) = 0.0
 2       continue
         Q(i,i) = 1.0
 3    continue
c
      return
      end
c
      subroutine prtmtx(n,ia,m,A)
c    
c     print array A
c
      dimension A(ia,n)
c
      do 2 i=1,m
         print 3,(A(i,j),j=1,n)
 2    continue
      print *
c
      return
 3    format(2x,4(e13.6,2x))
      end
c
      subroutine UtimesA(n,ia,m,A,V,U,W)
c
c     compute UA
c
      dimension A(ia,n),U(ia,m),V(m),W(ia,m)
c
      call findV(n,ia,m,A,V)
      print *,' V vector'
      call prtmtx(1,ia,m,V)
      call formU(m,ia,V,U)
      print *,' Matrix U'
      call prtmtx(m,ia,m,U)
      call mult(n,ia,m,U,A)      
      call formW(m,ia,U,W)
c
      return
      end
c
      subroutine findV(n,ia,m,A,V)
c
c     determine vector V
c
      dimension A(ia,n),V(m)
c
      beta = - sqrt(prod(m,A(1,1),A(1,1)))
      V(1) = A(1,1) - beta
      dem = V(1)*V(1) + prod(m-1,A(2,1),A(2,1))
      alpha = sqrt(2.0/dem)
      V(1) = alpha*(A(1,1) - beta)
      do 2 i=2,m
         V(i) = alpha*A(i,1)
 2    continue
c
      return
      end
c
      function prod(n,x,y)
      dimension x(n),y(n)
c
      sum = 0.0
      do 2 i=1,n
         sum = sum + x(i)*y(i)
 2    continue
      prod = sum
c
      return
      end
c
      subroutine formU(m,ia,V,U)
c
c     compute the unitary factor U
c
      dimension U(ia,m),V(m)
c
      do 3 i=1,m
         do 2 j=1,m
            U(i,j) = - V(i)*V(j)
 2       continue
         U(i,i) = 1.0 + U(i,i)
 3    continue
c
      return
      end
c
      subroutine formW(m,ia,U,W)
c
c     store U in W
c
      dimension U(ia,m),W(ia,m)
c
      do 2 i=1,m
         do 2 j=1,m
            W(i,j)=U(i,j)
 2    continue
c
      return
      end
c
      subroutine trans(n,ia,m,A)
c
c     compute transpose of the matrix A
c
      dimension A(ia,n),C(4,4)
c
      do 2 i=1,m
         do 2 j=1,n
            C(i,j) = A(j,i)
 2    continue
c
      do 3 i=1,m
         do 3 j=1,n
            A(i,j) = C(i,j)
 3    continue
c
      return
      end
c
      subroutine mult(n,ia,m,B,A)
c
c     compute the matrix-matrix product
c
      dimension B(ia,m),A(ia,n),C(4,4)
c
      do 3 j=1,n
         do 3 i=1,m
            sum = 0.0
            do 2 k=1,m
               sum = sum + B(i,k)*A(k,j)
 2          continue
            C(i,j) = sum
 3    continue
c
      do 4 i=1,m
         do 4 j=1,n
            A(i,j) = C(i,j)
 4    continue
c
      return
      end
