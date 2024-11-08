subroutine svd ( m, n, a, w, matu, u, matv, v, ierr )

!*****************************************************************************80
!
!! SVD computes the singular value decomposition for a real matrix.
!
!  Discussion:
!
!    This subroutine determines the singular value decomposition
!
!      A = U * S * V'
!
!    of a real M by N rectangular matrix.  Householder bidiagonalization
!    and a variant of the QR algorithm are used.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 October 2009
!
!  Author:
!
!    Original FORTRAN77 version by Smith, Boyle, Dongarra, Garbow, Ikebe,
!    Klema, Moler.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Golub, Christian Reinsch,
!    Numerische Mathematik,
!    Volume 14, 1970, pages 403-420.
!
!    James Wilkinson, Christian Reinsch,
!    Handbook for Automatic Computation,
!    Volume II, Linear Algebra, Part 2,
!    Springer, 1971,
!    ISBN: 0387054146,
!    LC: QA251.W67.
!
!    Brian Smith, James Boyle, Jack Dongarra, Burton Garbow,
!    Yasuhiko Ikebe, Virginia Klema, Cleve Moler,
!    Matrix Eigensystem Routines, EISPACK Guide,
!    Lecture Notes in Computer Science, Volume 6,
!    Springer Verlag, 1976,
!    ISBN13: 978-3540075462,
!    LC: QA193.M37.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of rows of A and U.
!
!    Input, integer ( kind = 4 ) N, the number of columns of A and U, and
!    the order of V.
!
!    Input, real ( kind = 8 ) A(M,N), the M by N matrix to be decomposed.
!
!    Output, real ( kind = 8 ) W(N), the singular values of A.  These are the
!    diagonal elements of S.  They are unordered.  If an error exit is
!    made, the singular values should be correct for indices
!    IERR+1, IERR+2,..., N.
!
!    Input, logical MATU, should be set to TRUE if the U matrix in the
!    decomposition is desired, and to FALSE otherwise.
!
!    Output, real ( kind = 8 ) U(M,N), contains the matrix U, with orthogonal
!    columns, of the decomposition, if MATU has been set to TRUE.  Otherwise
!    U is used as a temporary array.  U may coincide with A.
!    If an error exit is made, the columns of U corresponding
!    to indices of correct singular values should be correct.
!
!    Input, logical MATV, should be set to TRUE if the V matrix in the
!    decomposition is desired, and to FALSE otherwise.
!
!    Output, real ( kind = 8 ) V(N,N), the orthogonal matrix V of the
!    decomposition if MATV has been set to TRUE.  Otherwise V is not referenced.
!    V may also coincide with A if U is not needed.  If an error
!    exit is made, the columns of V corresponding to indices of
!    correct singular values should be correct.
!
!    Output, integer ( kind = 4 ) IERR, error flag.
!    0, for normal return,
!    K, if the K-th singular value has not been determined after 30 iterations.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) c
  real ( kind = 8 ) f
  real ( kind = 8 ) g
  real ( kind = 8 ) h
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ierr
  integer ( kind = 4 ) its
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) kk
  integer ( kind = 4 ) k1
  integer ( kind = 4 ) l
  integer ( kind = 4 ) ll
  integer ( kind = 4 ) l1
  logical matu
  logical matv
  integer ( kind = 4 ) mn
  real ( kind = 8 ) pythag
  real ( kind = 8 ) rv1(n)
  real ( kind = 8 ) s
  real ( kind = 8 ) scale
  real ( kind = 8 ) tst1
  real ( kind = 8 ) tst2
  real ( kind = 8 ) u(m,n)
  real ( kind = 8 ) v(n,n)
  real ( kind = 8 ) w(n)
  real ( kind = 8 ) x
  real ( kind = 8 ) y
  real ( kind = 8 ) z

  ierr = 0
  u(1:m,1:n) = a(1:m,1:n)
!
!  Householder reduction to bidiagonal form.
!
  g = 0.0D+00
  scale = 0.0D+00
  x = 0.0D+00

  do i = 1, n

    l = i + 1
    rv1(i) = scale * g
    g = 0.0D+00
    s = 0.0D+00
    scale = 0.0D+00

    if ( i <= m ) then

      scale = sum ( abs ( u(i:m,i) ) )

      if ( scale /= 0.0D+00 ) then

        u(i:m,i) = u(i:m,i) / scale

        s = sum ( u(i:m,i)**2 )

        f = u(i,i)
        g = - sign ( sqrt ( s ), f )
        h = f * g - s
        u(i,i) = f - g

        if ( i /= n ) then

          do j = l, n
            s = dot_product ( u(i:m,i), u(i:m,j) )
            u(i:m,j) = u(i:m,j) + s * u(i:m,i) / h
          end do

        end if

        u(i:m,i) = scale * u(i:m,i)

      end if

    end if

    w(i) = scale * g
    g = 0.0D+00
    s = 0.0D+00
    scale = 0.0D+00

    if ( i <= m .and. i /= n ) then

      scale = sum ( abs ( u(i,l:n) ) )

      if ( scale /= 0.0D+00 ) then

        u(i,l:n) = u(i,l:n) / scale
        s = sum ( u(i,l:n)**2 )
        f = u(i,l)
        g = - sign ( sqrt ( s ), f )
        h = f * g - s
        u(i,l) = f - g
        rv1(l:n) = u(i,l:n) / h

        if ( i /= m ) then

          do j = l, m

            s = dot_product ( u(j,l:n), u(i,l:n) )

            u(j,l:n) = u(j,l:n) + s * rv1(l:n)

          end do

        end if

        u(i,l:n) = scale * u(i,l:n)

      end if

    end if

    x = max ( x, abs ( w(i) ) + abs ( rv1(i) ) )

  end do
!
!  Accumulation of right-hand transformations.
!
  if ( matv ) then

    do i = n, 1, -1

      if ( i /= n ) then

         if ( g /= 0.0D+00 ) then

          v(l:n,i) = ( u(i,l:n) / u(i,l) ) / g

          do j = l, n

            s = dot_product ( u(i,l:n), v(l:n,j) )

            v(l:n,j) = v(l:n,j) + s * v(l:n,i)

          end do

        end if

        v(i,l:n) = 0.0D+00
        v(l:n,i) = 0.0D+00

      end if

      v(i,i) = 1.0D+00
      g = rv1(i)
      l = i

    end do

  end if
!
!  Accumulation of left-hand transformations.
!
  if ( matu ) then

    mn = min ( m, n )

    do i = min ( m, n ), 1, -1

      l = i + 1
      g = w(i)

      if ( i /= n ) then
        u(i,l:n) = 0.0D+00
      end if

      if ( g /= 0.0D+00 ) then

        if ( i /= mn ) then

          do j = l, n
            s = dot_product ( u(l:m,i), u(l:m,j) )
            f = ( s / u(i,i) ) / g
            u(i:m,j) = u(i:m,j) + f * u(i:m,i)
          end do

        end if

        u(i:m,i) = u(i:m,i) / g

      else

        u(i:m,i) = 0.0D+00

      end if

      u(i,i) = u(i,i) + 1.0D+00

    end do

  end if
!
!  Diagonalization of the bidiagonal form.
!
  tst1 = x

  do kk = 1, n

     k1 = n - kk
     k = k1 + 1
     its = 0
!
!  Test for splitting.
!
520  continue

     do ll = 1, k

       l1 = k - ll
       l = l1 + 1
       tst2 = tst1 + abs ( rv1(l) )

       if ( tst2 == tst1 ) then
         go to 565
       end if

       tst2 = tst1 + abs ( w(l1) )

       if ( tst2 == tst1 ) then
         exit
       end if

     end do
!
!  Cancellation of rv1(l) if L greater than 1.
!
     c = 0.0D+00
     s = 1.0D+00

     do i = l, k

       f = s * rv1(i)
       rv1(i) = c * rv1(i)
       tst2 = tst1 + abs ( f )

       if ( tst2 == tst1 ) then
         go to 565
       end if

       g = w(i)
       h = pythag ( f, g )
       w(i) = h
       c = g / h
       s = -f / h

       if ( matu ) then

         do j = 1, m
           y = u(j,l1)
           z = u(j,i)
           u(j,l1) = y * c + z * s
           u(j,i) = -y * s + z * c
         end do

       end if

    end do
!
!  Test for convergence.
!
565 continue

    z = w(k)

    if ( l == k ) then
      go to 650
    end if
!
!  Shift from bottom 2 by 2 minor.
!
    if ( its >= 30 ) then
      ierr = k
      return
    end if

    its = its + 1
    x = w(l)
    y = w(k1)
    g = rv1(k1)
    h = rv1(k)
    f = 0.5D+00 * ( ( ( g + z ) / h ) * ( ( g - z ) / y ) + y / h - h / y )
    g = pythag ( f, 1.0D+00 )
    f = x - ( z / x ) * z + ( h / x ) * ( y / ( f + sign ( g, f ) ) - h )
!
!  Next QR transformation.
!
    c = 1.0D+00
    s = 1.0D+00

    do i1 = l, k1

      i = i1 + 1
      g = rv1(i)
      y = w(i)
      h = s * g
      g = c * g
      z = pythag ( f, h )
      rv1(i1) = z
      c = f / z
      s = h / z
      f = x * c + g * s
      g = -x * s + g * c
      h = y * s
      y = y * c

      if ( matv ) then

        do j = 1, n
          x = v(j,i1)
          z = v(j,i)
          v(j,i1) = x * c + z * s
          v(j,i) = -x * s + z * c
        end do

      end if

      z = pythag ( f, h )
      w(i1) = z
!
!  Rotation can be arbitrary if Z is zero.
!
      if ( z /= 0.0D+00 ) then
        c = f / z
        s = h / z
      end if

      f = c * g + s * y
      x = - s * g + c * y

      if ( matu ) then

        do j = 1, m
          y = u(j,i1)
          z = u(j,i)
          u(j,i1) = y * c + z * s
          u(j,i) = - y * s + z * c
        end do

      end if

    end do

    rv1(l) = 0.0D+00
    rv1(k) = f
    w(k) = x
    go to 520
!
!  Convergence.
!
650 continue

    if ( z <= 0.0D+00 ) then

      w(k) = - z

      if ( matv ) then
        v(1:n,k) = - v(1:n,k)
      end if

    end if

  end do

  return
end


function pythag ( a, b )

!*****************************************************************************80
!
!! PYTHAG computes SQRT ( A * A + B * B ) carefully.
!
!  Discussion:
!
!    The formula
!
!      PYTHAG = sqrt ( A * A + B * B )
!
!    is reasonably accurate, but can fail if, for example, A^2 is larger
!    than the machine overflow.  The formula can lose most of its accuracy
!    if the sum of the squares is very large or very small.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 October 2009
!
!  Author:
!
!    Original FORTRAN77 version by Smith, Boyle, Dongarra, Garbow, Ikebe,
!    Klema, Moler.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    James Wilkinson, Christian Reinsch,
!    Handbook for Automatic Computation,
!    Volume II, Linear Algebra, Part 2,
!    Springer, 1971,
!    ISBN: 0387054146,
!    LC: QA251.W67.
!
!    Brian Smith, James Boyle, Jack Dongarra, Burton Garbow,
!    Yasuhiko Ikebe, Virginia Klema, Cleve Moler,
!    Matrix Eigensystem Routines, EISPACK Guide,
!    Lecture Notes in Computer Science, Volume 6,
!    Springer Verlag, 1976,
!    ISBN13: 978-3540075462,
!    LC: QA193.M37.
!
!  Modified:
!
!    04 February 2003
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the two legs of a right triangle.
!
!    Output, real ( kind = 8 ) PYTHAG, the length of the hypotenuse.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) p
  real ( kind = 8 ) pythag
  real ( kind = 8 ) r
  real ( kind = 8 ) s
  real ( kind = 8 ) t
  real ( kind = 8 ) u

  p = max ( abs ( a ), abs ( b ) )

  if ( p /= 0.0D+00 ) then

    r = ( min ( abs ( a ), abs ( b ) ) / p )**2

    do

      t = 4.0D+00 + r

      if ( t == 4.0D+00 ) then
        exit
      end if

      s = r / t
      u = 1.0D+00 + 2.0D+00 * s
      p = u * p
      r = ( s / u )**2 * r

    end do

  end if

  pythag = p

  return
end
