subroutine imtqlx ( n, d, e, z )
  use mod_kinds, only : i4, dp
        !dir$ code_align : 32 :: imtqlx
        !dir$ optimize : 3
        !dir$ attributes optimization_parameters: TARGET_ARCH=skylake_avx512 :: imtqlx
!*****************************************************************************80
!
!! IMTQLX diagonalizes a symmetric tridiagonal matrix.
!
!  Discussion:
!
!    This routine is a slightly modified version of the EISPACK routine to 
!    perform the implicit QL algorithm on a symmetric tridiagonal matrix. 
!
!    The authors thank the authors of EISPACK for permission to use this
!    routine. 
!
!    It has been modified to produce the product Q' * Z, where Z is an input 
!    vector and Q is the orthogonal matrix diagonalizing the input matrix.  
!    The changes consist (essentially) of applying the orthogonal 
!    transformations directly to Z as they are generated.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 December 2009
!
!  Author:
!
!    Original FORTRAN77 version by Sylvan Elhay, Jaroslav Kautsky.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Sylvan Elhay, Jaroslav Kautsky,
!    Algorithm 655: IQPACK, FORTRAN Subroutines for the Weights of 
!    Interpolatory Quadrature,
!    ACM Transactions on Mathematical Software,
!    Volume 13, Number 4, December 1987, pages 399-415.
!
!    Roger Martin, James Wilkinson,
!    The Implicit QL Algorithm,
!    Numerische Mathematik,
!    Volume 12, Number 5, December 1968, pages 377-383.
!
!  Parameters:
!
!    Input, integer(i4) N, the order of the matrix.
!
!    Input/output, real(kind=dp) D(N), the diagonal entries of the matrix.
!    On output, the information in D has been overwritten.
!
!    Input/output, real(kind=dp) E(N), the subdiagonal entries of the 
!    matrix, in entries E(1) through E(N-1).  On output, the information in
!    E has been overwritten.
!
!    Input/output, real(kind=dp) Z(N).  On input, a vector.  On output,
!    the value of Q' * Z, where Q is the matrix that diagonalizes the
!    input symmetric tridiagonal matrix.
!
  implicit none

  integer(i4) n

  real(kind=dp) b
  real(kind=dp) c
  real(kind=dp) d(n)
  real(kind=dp) e(n)
  !dir$ assume_aligned d:64,e:64
  real(kind=dp) f
  real(kind=dp) g
  integer(i4) i
  integer(i4) ii
  integer(i4), parameter :: itn = 30
  integer(i4) j
  integer(i4) k
  integer(i4) l
  integer(i4) m
  integer(i4) mml
  real(kind=dp) p
  real(kind=dp) prec
  real(kind=dp) r
  real(kind=dp) s
  real(kind=dp) z(n)
  !dir$ assume_aligned z:64
  prec = epsilon ( prec )

  if ( n == 1 ) then
    return
  end if

  e(n) = 0.0_dp

  do l = 1, n

    j = 0

    do

      do m = l, n

        if ( m == n ) then
          exit
        end if

        if ( abs ( e(m) ) <= prec * ( abs ( d(m) ) + abs ( d(m+1) ) ) ) then
          exit
        end if

      end do

      p = d(l)

      if ( m == l ) then
        exit
      end if

      if ( itn <= j ) then
        !write ( *, '(a)' ) ' '
        !write ( *, '(a)' ) 'IMTQLX - Fatal error!'
        !write ( *, '(a)' ) '  Iteration limit exceeded.'
        !write ( *, '(a,i8)' ) '  J = ', j
        !write ( *, '(a,i8)' ) '  L = ', l
        !write ( *, '(a,i8)' ) '  M = ', m
        !write ( *, '(a,i8)' ) '  N = ', n
         !stop 1
         return
      end if

      j = j + 1
      g = ( d(l+1) - p ) / ( 2.0_dp * e(l) )
      r =  sqrt ( g * g + 1.0_dp )
      g = d(m) - p + e(l) / ( g + sign ( r, g ) )
      s = 1.0_dp
      c = 1.0_dp
      p = 0.0_dp
      mml = m - l

      do ii = 1, mml

        i = m - ii
        f = s * e(i)
        b = c * e(i)

        if ( abs ( g ) <= abs ( f ) ) then
          c = g / f
          r =  sqrt ( c * c + 1.0_dp )
          e(i+1) = f * r
          s = 1.0_dp / r
          c = c * s
        else
          s = f / g
          r =  sqrt ( s * s + 1.0_dp )
          e(i+1) = g * r
          c = 1.0_dp / r
          s = s * c
        end if

        g = d(i+1) - p
        r = ( d(i) - g ) * s + 2.0_dp * c * b
        p = s * r
        d(i+1) = g + p
        g = c * r - b
        f = z(i+1)
        z(i+1) = s * z(i) + c * f
        z(i) = c * z(i) - s * f

      end do

      d(l) = d(l) - p
      e(l) = g
      e(m) = 0.0_dp

    end do

  end do
!
!  Sorting.
!
  do ii = 2, n

    i = ii - 1
    k = i
    p = d(i)

    do j = ii, n
      if ( d(j) < p ) then
        k = j
        p = d(j)
      end if
    end do

    if ( k /= i ) then
      d(k) = d(i)
      d(i) = p
      p = z(i)
      z(i) = z(k)
      z(k) = p
    end if

  end do

  return
end
subroutine p_exponential_product ( p, b, table )
        use mod_kinds, only : i4, dp
        use omp_lib
        !dir$ code_align : 32 :: p_exponential_product
        !dir$ optimize : 3
        !dir$ attributes optimization_parameters: TARGET_ARCH=skylake_avx512 :: p_exponential_product
!*****************************************************************************80
!
!! P_EXPONENTIAL_PRODUCT: exponential products for P(n,x).
!
!  Discussion:
!
!    Let P(n,x) represent the Legendre polynomial of degree n.  
!
!    For polynomial chaos applications, it is of interest to know the
!    value of the integrals of products of exp(B*X) with every possible pair
!    of basis functions.  That is, we'd like to form
!
!      Tij = Integral ( -1.0 <= X <= +1.0 ) exp(B*X) * P(I,X) * P(J,X) dx
!
!    We will estimate these integrals using Gauss-Legendre quadrature.
!    Because of the exponential factor exp(B*X), the quadrature will not 
!    be exact.
!
!    However, when B = 0, the quadrature is exact.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer(i4) P, the maximum degree of the polyonomial 
!    factors.  0 <= P.
!
!    Input, real(kind=dp) B, the coefficient of X in the exponential factor.
!
!    Output, real(kind=dp) TABLE(0:P,0:P), the table of integrals.  
!
  implicit none

  integer(i4) p

  real(kind=dp) b
  real(kind=dp) h_table(0:p)
  !dir$ assume_aligned h_table : 64
  integer(i4) i
  integer(i4) j
  integer(i4) k
  integer(i4) order
  real(kind=dp) table(0:p,0:p)
  !dir$ assume_aligned table:64
  real(kind=dp), allocatable :: w_table(:)
  !dir$ attributes align : 64 :: w_table
  real(kind=dp) x(1)
  real(kind=dp), allocatable :: x_table(:)
  !dir$ attributes align : 64 :: x_table

  table(0:p,0:p) = 0.0_dp

  order = ( 3 * p + 4 ) / 2

  allocate ( x_table(1:order) )
  allocate ( w_table(1:order) )

  call p_quadrature_rule ( order, x_table, w_table )

  do k = 1, order

    x(1) = x_table(k)
    call p_polynomial_value ( 1, p, x, h_table )
!
!  The following formula is an outer product in H_TABLE.
!
    do j = 0, p
      !$omp simd reduction(+:table) linear(i:1)
      do i = 0, p
        table(i,j) = table(i,j) &
          + w_table(k) * exp ( b * x(1) ) * h_table(i) * h_table(j)
      end do
    end do

  end do

  deallocate ( w_table )
  deallocate ( x_table )

  return
end
subroutine p_integral ( n, value )
       use mod_kinds, only : i4, dp
        !dir$ attributes vector :: p_integral
        !dir$ code_align : 32 :: p_integral
        !dir$ optimize : 3
        !dir$ attributes optimization_parameters: TARGET_ARCH=skylake_avx512 :: p_integral
!*****************************************************************************80
!
!! P_INTEGRAL evaluates a monomial integral associated with P(n,x).
!
!  Discussion:
!
!    The integral:
!
!      integral ( -1 <= x < +1 ) x^n dx
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 March 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer(i4) N, the exponent.
!    0 <= N.
!
!    Output, real(kind=dp) VALUE, the value of the integral.
!
  implicit none

  integer(i4) n
  real(kind=dp) value

  if ( mod ( n, 2 ) == 1 ) then
    value = 0.0_dp
  else
    value = 2.0_dp / real ( n + 1, kind = 8 )
  end if

  return
end
subroutine p_polynomial_coefficients ( n, c )
         use mod_kinds, only : i4, dp
        !dir$ code_align : 32 :: p_polynomial_coefficients 
        !dir$ optimize : 3
        !dir$ attributes optimization_parameters: TARGET_ARCH=skylake_avx512 :: p_polynomial_coefficients 
!*****************************************************************************80
!
!! P_POLYNOMIAL_COEFFICIENTS: coefficients of Legendre polynomials P(n,x).
!
!  Discussion:
!
!     1
!     0     1
!    -1/2   0      3/2
!     0    -3/2    0     5/2
!     3/8   0    -30/8   0     35/8
!     0    15/8    0   -70/8    0     63/8
!    -5/16  0    105/16  0   -315/16   0    231/16
!     0   -35/16   0   315/16   0   -693/16   0    429/16
!
!     1.00000
!     0.00000  1.00000
!    -0.50000  0.00000  1.50000
!     0.00000 -1.50000  0.00000  2.5000
!     0.37500  0.00000 -3.75000  0.00000  4.37500
!     0.00000  1.87500  0.00000 -8.75000  0.00000  7.87500
!    -0.31250  0.00000  6.56250  0.00000 -19.6875  0.00000  14.4375
!     0.00000 -2.1875   0.00000  19.6875  0.00000 -43.3215  0.00000  26.8125
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 February 2003
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!    Daniel Zwillinger, editor,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition,
!    CRC Press, 1996.
!
!  Parameters:
!
!    Input, integer(i4) N, the highest order polynomial to evaluate.
!    Note that polynomials 0 through N will be evaluated.
!
!    Output, real(kind=dp) C(0:N,0:N), the coefficients of the 
!    Legendre polynomials of degree 0 through N.
!
  implicit none

  integer(i4) n

  real(kind=dp) c(0:n,0:n)
  !dir$ assume_aligned c:64
  integer(i4) i

  if ( n < 0 ) then
    return
  end if

  c(0:n,0:n) = 0.0_dp

  c(0,0) = 1.0_dp

  if ( n <= 0 ) then
    return
  end if

  c(1,1) = 1.0_dp
 
  do i = 2, n
    c(i,0:i-2) =          real (   - i + 1, kind = 8 ) * c(i-2,0:i-2) &
                        / real (     i,     kind = 8 )
    c(i,1:i) = c(i,1:i) + real ( i + i - 1, kind = 8 ) * c(i-1,0:i-1) &
                        / real (     i,     kind = 8 )
  end do
 
  return
end
subroutine p_polynomial_prime ( m, n, x, vp )
         use mod_kinds, only : i4, dp
         use omp_lib
        !dir$ code_align : 32 :: p_polynomial_prime
        !dir$ optimize : 3
        !dir$ attributes optimization_parameters: TARGET_ARCH=skylake_avx512 :: p_polynomial_prime
!*****************************************************************************80
!
!! P_POLYNOMIAL_PRIME evaluates the derivative of Legendre polynomials P(n,x).
!
!  Discussion:
!
!    P(0,X) = 1
!    P(1,X) = X
!    P(N,X) = ( (2*N-1)*X*P(N-1,X)-(N-1)*P(N-2,X) ) / N
!
!    P'(0,X) = 0
!    P'(1,X) = 1
!    P'(N,X) = ( (2*N-1)*(P(N-1,X)+X*P'(N-1,X)-(N-1)*P'(N-2,X) ) / N
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 March 2012
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!    Daniel Zwillinger, editor,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition,
!    CRC Press, 1996.
!
!  Parameters:
!
!    Input, integer(i4) M, the number of evaluation points.
!
!    Input, integer(i4) N, the highest order polynomial to evaluate.
!    Note that polynomials 0 through N will be evaluated.
!
!    Input, real(kind=dp) X(M), the evaluation points.
!
!    Output, real(kind=dp) VP(M,0:N), the values of the derivatives of the
!    Legendre polynomials of order 0 through N.
!
  implicit none

  integer(i4) m
  integer(i4) n

  integer(i4) i
  real(kind=dp) v(m,0:n)
  real(kind=dp) vp(m,0:n)
  !dir$ assume_aligned v:64, vp:64
  real(kind=dp) x(m)
  !dir$ assume_aligned x:64
  if ( n < 0 ) then
    return
  end if

  v(1:m,0) = 1.0_dp
  vp(1:m,0) = 0.0_dp

  if ( n < 1 ) then
    return
  end if

  v(1:m,1) = x(1:m)
  vp(1:m,1) = 1.0_dp
  !$omp simd linear(i:2)
  do i = 2, n
 
    v(1:m,i) = ( real ( 2 * i - 1, kind = 8 ) * x(1:m) * v(1:m,i-1)   &
               - real (     i - 1, kind = 8 ) *          v(1:m,i-2) ) &
               / real (     i,     kind = 8 )
 
    vp(1:m,i) = ( real ( 2 * i - 1, kind = 8 ) * ( v(1:m,i-1) &
                                                   + x(1:m) * vp(1:m,i-1) ) &
                - real (     i - 1, kind = 8 ) *   vp(1:m,i-2)               ) &
                / real (     i,     kind = 8 )
 
  end do
 
  return
end
subroutine p_polynomial_prime2 ( m, n, x, vpp )
         use mod_kinds, only : i4, dp
         use omp_lib
        !dir$ code_align : 32 :: p_polynomial_prime
        !dir$ optimize : 3
        !dir$ attributes optimization_parameters: TARGET_ARCH=skylake_avx512 :: p_polynomial_prime
!*****************************************************************************80
!
!! P_POLYNOMIAL_PRIME2: second derivative of Legendre polynomials P(n,x).
!
!  Discussion:
!
!    P(0,X) = 1
!    P(1,X) = X
!    P(N,X) = ( (2*N-1)*X*P(N-1,X)-(N-1)*P(N-2,X) ) / N
!
!    P'(0,X) = 0
!    P'(1,X) = 1
!    P'(N,X) = ( (2*N-1)*(P(N-1,X)+X*P'(N-1,X)-(N-1)*P'(N-2,X) ) / N
!
!    P"(0,X) = 0
!    P"(1,X) = 0
!    P"(N,X) = ( (2*N-1)*(2*P(N-1,X)+X*P"(N-1,X)-(N-1)*P"(N-2,X) ) / N
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    03 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!    Daniel Zwillinger, editor,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition,
!    CRC Press, 1996.
!
!  Parameters:
!
!    Input, integer(i4) M, the number of evaluation points.
!
!    Input, integer(i4) N, the highest order polynomial to evaluate.
!    Note that polynomials 0 through N will be evaluated.
!
!    Input, real(kind=dp) X(M), the evaluation points.
!
!    Output, real(kind=dp) VPP(M,0:N), the second derivative of the
!    Legendre polynomials of order 0 through N.
!
  implicit none

  integer(i4) m
  integer(i4) n

  integer(i4) i
  real(kind=dp) v(m,0:n)
  real(kind=dp) vp(m,0:n)
  !dir$ attributes align : 64 :: v, vp
  real(kind=dp) vpp(m,0:n)
  real(kind=dp) x(m)
  !dir$ assume_aligned x:64
  !dir$ assume_aligned vpp:64
  if ( n < 0 ) then
    return
  end if

  v(1:m,0) = 1.0_dp
  vp(1:m,0) = 0.0_dp
  vpp(1:m,0) = 0.0_dp

  if ( n < 1 ) then
    return
  end if

  v(1:m,1) = x(1:m)
  vp(1:m,1) = 1.0_dp
  vpp(1:m,1) = 0.0_dp
 
  !$omp simd linear(i:1) unroll partial(4)
  do i = 2, n
 
    v(1:m,i) = &
      ( real ( 2 * i - 1, kind = 8 ) * x(1:m) * v(1:m,i-1)   &
      - real (     i - 1, kind = 8 ) *          v(1:m,i-2) ) &
      / real (     i,     kind = 8 )
 
    vp(1:m,i) = &
      ( real ( 2 * i - 1, kind = 8 ) * ( v(1:m,i-1) + x(1:m) * vp(1:m,i-1) ) &
      - real (     i - 1, kind = 8 ) *   vp(1:m,i-2)               ) &
      / real (     i,     kind = 8 )

    vpp(1:m,i) = &
      ( real ( 2 * i - 1, kind = 8 ) * ( 2.0_dp * vp(1:m,i-1) &
                                         + x(1:m) * vpp(1:m,i-1) ) &
      - real (     i - 1, kind = 8 ) *   vpp(1:m,i-2)               ) &
      / real (     i,     kind = 8 )

  end do
 
  return
end
subroutine p_polynomial_value ( m, n, x, v )
        use mod_kinds, only : i4, dp
         use omp_lib
         !dir$ attributes forceinline :: p_polynomial_value
        !dir$ code_align : 32 :: p_polynomial_value
        !dir$ optimize : 3
        !dir$ attributes optimization_parameters: TARGET_ARCH=skylake_avx512 :: p_polynomial_value
!*****************************************************************************80
!
!! P_POLYNOMIAL_VALUE evaluates the Legendre polynomials P(n,x).
!
!  Discussion:
!
!    P(n,1) = 1.
!    P(n,-1) = (-1)^N.
!    | P(n,x) | <= 1 in [-1,1].
!
!    The N zeroes of P(n,x) are the abscissas used for Gauss-Legendre
!    quadrature of the integral of a function F(X) with weight function 1
!    over the interval [-1,1].
!
!    The Legendre polynomials are orthogonal under the inner product defined
!    as integration from -1 to 1:
!
!      Integral ( -1 <= X <= 1 ) P(I,X) * P(J,X) dX 
!        = 0 if I =/= J
!        = 2 / ( 2*I+1 ) if I = J.
!
!    Except for P(0,X), the integral of P(I,X) from -1 to 1 is 0.
!
!    A function F(X) defined on [-1,1] may be approximated by the series
!      C0*P(0,x) + C1*P(1,x) + ... + CN*P(n,x)
!    where
!      C(I) = (2*I+1)/(2) * Integral ( -1 <= X <= 1 ) F(X) P(I,x) dx.
!
!    The formula is:
!
!      P(n,x) = (1/2^N) * sum ( 0 <= M <= N/2 ) C(N,M) C(2N-2M,N) X^(N-2*M)
!
!  Differential equation:
!
!    (1-X*X) * P(n,x)'' - 2 * X * P(n,x)' + N * (N+1) = 0
!
!  First terms:
!
!    P( 0,x) =      1
!    P( 1,x) =      1 X
!    P( 2,x) = (    3 X^2 -       1)/2
!    P( 3,x) = (    5 X^3 -     3 X)/2
!    P( 4,x) = (   35 X^4 -    30 X^2 +     3)/8
!    P( 5,x) = (   63 X^5 -    70 X^3 +    15 X)/8
!    P( 6,x) = (  231 X^6 -   315 X^4 +   105 X^2 -     5)/16
!    P( 7,x) = (  429 X^7 -   693 X^5 +   315 X^3 -    35 X)/16
!    P( 8,x) = ( 6435 X^8 - 12012 X^6 +  6930 X^4 -  1260 X^2 +   35)/128
!    P( 9,x) = (12155 X^9 - 25740 X^7 + 18018 X^5 -  4620 X^3 +  315 X)/128
!    P(10,x) = (46189 X^10-109395 X^8 + 90090 X^6 - 30030 X^4 + 3465 X^2-63)/256
!
!  Recursion:
!
!    P(0,x) = 1
!    P(1,x) = x
!    P(n,x) = ( (2*n-1)*x*P(n-1,x)-(n-1)*P(n-2,x) ) / n
!
!    P'(0,x) = 0
!    P'(1,x) = 1
!    P'(N,x) = ( (2*N-1)*(P(N-1,x)+X*P'(N-1,x)-(N-1)*P'(N-2,x) ) / N
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 March 2012
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!    Daniel Zwillinger, editor,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition,
!    CRC Press, 1996.
!
!  Parameters:
!
!    Input, integer(i4) M, the number of evaluation points.
!
!    Input, integer(i4) N, the highest order polynomial to evaluate.
!    Note that polynomials 0 through N will be evaluated.
!
!    Input, real(kind=dp) X(M), the evaluation points.
!
!    Output, real(kind=dp) V(M,0:N), the values of the Legendre polynomials 
!    of order 0 through N at the points X.
!
  implicit none

  integer(i4) m
  integer(i4) n

  integer(i4) i
  real(kind=dp) v(m,0:n)
  real(kind=dp) x(m)
  !dir$ assume_aligned v:64,x:64
  if ( n < 0 ) then
    return
  end if

  v(1:m,0) = 1.0_dp

  if ( n < 1 ) then
    return
  end if

  v(1:m,1) = x(1:m)
  !$omp simd linear(i:1) unroll partial(4)
  do i = 2, n
 
    v(1:m,i) = ( real ( 2 * i - 1, kind = 8 ) * x(1:m) * v(1:m,i-1)   &
               - real (     i - 1, kind = 8 ) *          v(1:m,i-2) ) &
               / real (     i,     kind = 8 )
 
  end do
 
  return
end
subroutine p_polynomial_values ( n_data, n, x, fx )
       use mod_kinds, only : i4, dp
         
         !dir$ attributes forceinline :: p_polynomial_values
        !dir$ code_align : 32 :: p_polynomial_values
        !dir$ optimize : 3
        !dir$ attributes optimization_parameters: TARGET_ARCH=skylake_avx512 :: p_polynomial_values
!*****************************************************************************80
!
!! P_POLYNOMIAL_VALUES: selected values of the Legendre polynomials P(n,x).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 March 2012
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Parameters:
!
!    Input/output, integer(i4) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer(i4) N, the order of the function.
!
!    Output, real(kind=dp) X, the point where the function is evaluated.
!
!    Output, real(kind=dp) FX, the value of the function.
!
  implicit none

  integer(i4), parameter :: n_max = 22

  real(kind=dp) fx
  real(kind=dp), save, dimension ( n_max ) :: fx_vec = (/ &
     0.1000000000000000D+01, &
     0.2500000000000000_dp, &
    -0.4062500000000000_dp, &
    -0.3359375000000000_dp, &
     0.1577148437500000_dp, &
     0.3397216796875000_dp, &
     0.2427673339843750D-01, &
    -0.2799186706542969_dp, &
    -0.1524540185928345_dp, &
     0.1768244206905365_dp, &
     0.2212002165615559_dp, &
     0.0000000000000000_dp, &
    -0.1475000000000000_dp, &
    -0.2800000000000000_dp, &
    -0.3825000000000000_dp, &
    -0.4400000000000000_dp, &
    -0.4375000000000000_dp, &
    -0.3600000000000000_dp, &
    -0.1925000000000000_dp, &
     0.8000000000000000D-01, &
     0.4725000000000000_dp, &
     0.1000000000000000D+01 /)
  integer(i4) n
  integer(i4) n_data
  integer(i4), save, dimension ( n_max ) :: n_vec = (/ &
     0,  1,  2, &
     3,  4,  5, &
     6,  7,  8, &
     9, 10,  3, &
     3,  3,  3, &
     3,  3,  3, &
     3,  3,  3, &
     3 /)
  real(kind=dp) x
  real(kind=dp), save, dimension ( n_max ) :: x_vec = (/ &
    0.25_dp, &
    0.25_dp, &
    0.25_dp, &
    0.25_dp, &
    0.25_dp, &
    0.25_dp, &
    0.25_dp, &
    0.25_dp, &
    0.25_dp, &
    0.25_dp, &
    0.25_dp, &
    0.00_dp, &
    0.10_dp, &
    0.20_dp, &
    0.30_dp, &
    0.40_dp, &
    0.50_dp, &
    0.60_dp, &
    0.70_dp, &
    0.80_dp, &
    0.90_dp, &
    1.00_dp /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    n = 0
    x = 0.0_dp
    fx = 0.0_dp
  else
    n = n_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
subroutine p_polynomial_zeros ( nt, t )
       use mod_kinds, only : i4, dp
       use omp_lib 
        !dir$ attributes forceinline :: p_polynomial_zeroes
        !dir$ code_align : 32 :: p_polynomial_zeroes
        !dir$ optimize : 3
        !dir$ attributes optimization_parameters: TARGET_ARCH=skylake_avx512 :: p_polynomial_zeroes
!*****************************************************************************80
!
!! P_POLYNOMIAL_ZEROS: zeros of Legendre function P(n,x).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 March 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer(i4) NT, the order of the rule.
!
!    Output, real(kind=dp) T(NT), the zeros.
!
  implicit none

  integer(i4) nt

  real(kind=dp) bj(nt)
  !dir$ attributes align : 64 :: bj
  integer(i4) i
  real(kind=dp) t(nt)
  !dir$ assume_aligned t:64
  real(kind=dp) wts(nt)
  !dir$ attributes align : 64 :: wts
  t(1:nt) = 0.0_dp
  !$omp simd linear(i:1) unroll partial(2)
  do i = 1, nt
    bj(i) = real ( i * i, kind = 8 ) / real ( 4 * i * i - 1, kind = 8 )
  end do
  bj(1:nt) = sqrt ( bj(1:nt) )

  wts(1:nt) = 0.0_dp
  wts(1) = sqrt ( 2.0_dp )

  call imtqlx ( nt, t, bj, wts )

  return
end
subroutine p_power_product ( p, e, table )
        use mod_kinds, only : i4, dp
        use omp_lib  
         
        !dir$ code_align : 32 :: p_polynomial_values
        !dir$ optimize : 3
        !dir$ attributes optimization_parameters: TARGET_ARCH=skylake_avx512 :: p_power_product
!*****************************************************************************80
!
!! P_POWER_PRODUCT: power products for Legendre polynomial P(n,x).
!
!  Discussion:
!
!    Let P(n,x) represent the Legendre polynomial of degree n.  
!
!    For polynomial chaos applications, it is of interest to know the
!    value of the integrals of products of X with every possible pair
!    of basis functions.  That is, we'd like to form
!
!      Tij = Integral ( -1.0 <= X <= +1.0 ) X^E * P(i,x) * P(j,x) dx
!
!    We will estimate these integrals using Gauss-Legendre quadrature.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer(i4) P, the maximum degree of the polyonomial 
!    factors.  0 <= P.
!
!    Input, integer(i4) E, the exponent of X in the integrand.
!    0 <= E.
!
!    Output, real(kind=dp) TABLE(0:P,0:P), the table of integrals.  
!
  implicit none

  integer(i4) p

  integer(i4) e
  real(kind=dp) h_table(0:p)
  !dir$ assume_aligned h_table:64
  integer(i4) i
  integer(i4) j
  integer(i4) k
  integer(i4) order
  real(kind=dp) :: wt,ht
  real(kind=dp) table(0:p,0:p)
  !dir$ attributes align : 64 :: table
  real(kind=dp), allocatable :: w_table(:)
  !dir$ attributes align : 64 :: w_table
  real(kind=dp) x(1)
  real(kind=dp), allocatable :: x_table(:)
  !dir$ attributes align : 64 :: x_table
  table(0:p,0:p) = 0.0_dp

  order = p + 1 + ( ( e + 1 ) / 2 )
  wt=0.0_dp
  ht=0.0_dp
  allocate ( x_table(order) )
  allocate ( w_table(order) )

  call p_quadrature_rule ( order, x_table, w_table )
  
  do k = 1, order
    wt=w_table(k)
    x(1) = x_table(k)
    call p_polynomial_value ( 1, p, x, h_table )
!
!  The following formula is an outer product in H_TABLE.
!
    if ( e == 0 ) then
      do i = 0, p
         ht=h_table(i)
         !$omp simd reduction(+:table)
         do j = 0, p
          table(i,j) = table(i,j) + wt * ht * h_table(j)
        end do
      end do
    else
      do i = 0, p
         ht=h_table(i)
        !$omp simd reduction(+:table)
        do j = 0, p
          table(i,j) = table(i,j) &
            + wt * x(1) ** e * ht * h_table(j)
        end do
      end do
    end if

  end do

  deallocate ( w_table )
  deallocate ( x_table )

  return
end
subroutine p_quadrature_rule ( nt, t, wts )
        use mod_kinds, only : i4, dp
        use omp_lib  
        
        !dir$ code_align : 32 :: p_polynomial_values
        !dir$ optimize : 3
        !dir$ attributes optimization_parameters: TARGET_ARCH=skylake_avx512 :: p_quadrature_rule
!*****************************************************************************80
!
!! P_QUADRATURE_RULE: quadrature for Legendre function P(n,x).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 March 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer(i4) NT, the order of the rule.
!
!    Output, real(kind=dp) T(NT), WTS(NT), the points and weights
!    of the rule.
!
  implicit none

  integer(i4) nt

  real(kind=dp) bj(nt)
  !dir$ attributes align : 64 :: bj
  integer(i4) i
  real(kind=dp) t(nt)
  real(kind=dp) wts(nt)
  !dir$ assume_aligned t:64,wts:64
  t(1:nt) = 0.0_dp
  !$omp simd linear(i:1) unroll partial(4)
  do i = 1, nt
    bj(i) = real ( i * i, kind = 8 ) / real ( 4 * i * i - 1, kind = 8 )
  end do
  bj(1:nt) = sqrt ( bj(1:nt) )

  wts(1) = sqrt ( 2.0_dp )
  wts(2:nt) = 0.0_dp

  call imtqlx ( nt, t, bj, wts )

  wts(1:nt) = wts(1:nt) * wts(1:nt)

  return
end
subroutine pm_polynomial_value ( mm, n, m, x, cx )
        use mod_kinds, only : i4, dp
        use omp_lib  
        
        !dir$ code_align : 32 :: pm_polynomial_value
        !dir$ optimize : 3
        !dir$ attributes optimization_parameters: TARGET_ARCH=skylake_avx512 :: pm_polynomial_value
!*****************************************************************************80
!
!! PM_POLYNOMIAL_VALUE evaluates the Legendre polynomials Pm(n,m,x).
!
!  Differential equation:
!
!    (1-X*X) * Y'' - 2 * X * Y + ( N (N+1) - (M*M/(1-X*X)) * Y = 0
!
!  First terms:
!
!    M = 0  ( = Legendre polynomials of first kind P(N,X) )
!
!    Pm(0,0,x) =    1
!    Pm(1,0,x) =    1 X
!    Pm(2,0,x) = (  3 X^2 -   1)/2
!    Pm(3,0,x) = (  5 X^3 -   3 X)/2
!    Pm(4,0,x) = ( 35 X^4 -  30 X^2 +   3)/8
!    Pm(5,0,x) = ( 63 X^5 -  70 X^3 +  15 X)/8
!    Pm(6,0,x) = (231 X^6 - 315 X^4 + 105 X^2 -  5)/16
!    Pm(7,0,x) = (429 X^7 - 693 X^5 + 315 X^3 - 35 X)/16
!
!    M = 1
!
!    Pm(0,1,x) =   0
!    Pm(1,1,x) =   1 * SQRT(1-X^2)
!    Pm(2,1,x) =   3 * SQRT(1-X^2) * X
!    Pm(3,1,x) = 1.5 * SQRT(1-X^2) * (5*X^2-1)
!    Pm(4,1,x) = 2.5 * SQRT(1-X^2) * (7*X^3-3*X)
!
!    M = 2
!
!    Pm(0,2,x) =   0
!    Pm(1,2,x) =   0
!    Pm(2,2,x) =   3 * (1-X^2)
!    Pm(3,2,x) =  15 * (1-X^2) * X
!    Pm(4,2,x) = 7.5 * (1-X^2) * (7*X^2-1)
!
!    M = 3
!
!    Pm(0,3,x) =   0
!    Pm(1,3,x) =   0
!    Pm(2,3,x) =   0
!    Pm(3,3,x) =  15 * (1-X^2)^1.5
!    Pm(4,3,x) = 105 * (1-X^2)^1.5 * X
!
!    M = 4
!
!    Pm(0,4,x) =   0
!    Pm(1,4,x) =   0
!    Pm(2,4,x) =   0
!    Pm(3,4,x) =   0
!    Pm(4,4,x) = 105 * (1-X^2)^2
!
!  Recursion:
!
!    if N < M:
!      Pm(N,M,x) = 0
!    if N = M:
!      Pm(N,M,x) = (2*M-1)!! * (1-X*X)^(M/2) where N!! means the product of
!      all the odd integers less than or equal to N.
!    if N = M+1:
!      Pm(N,M,x) = X*(2*M+1)*Pm(M,M,x)
!    if M+1 < N:
!      Pm(N,M,x) = ( X*(2*N-1)*Pm(N-1,M,x) - (N+M-1)*Pm(N-2,M,x) )/(N-M)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    14 May 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!  Parameters:
!
!    Input, integer(i4) MM, the number of evaluation points.
!
!    Input, integer(i4) N, the maximum first index of the Legendre
!    function, which must be at least 0.
!
!    Input, integer(i4) M, the second index of the Legendre function,
!    which must be at least 0, and no greater than N.
!
!    Input, real(kind=dp) X(MM), the point at which the function is to be
!    evaluated.
!
!    Output, real(kind=dp) CX(MM,0:N), the function values.
!
  implicit none

  integer(i4) mm
  integer(i4) n

  real(kind=dp) cx(mm,0:n)
  !dir$ assume_aligned cx:64
  real(kind=dp) fact
  integer(i4) j
  integer(i4) m
  real(kind=dp) :: xx
  real(kind=dp) x(mm)
  !dir$ attributes align : 64 :: x
  cx(1:mm,0:n) = 0.0_dp
!
!  J = M is the first nonzero function.
!
  if ( m <= n ) then
    cx(1:mm,m) = 1.0_dp

    fact = 1.0_dp
    !$omp simd reduction(*:cx) linear(j:1)
    do j = 1, m
      xx=x(1:mm)
      cx(1:mm,m) = - cx(1:mm,m) * fact * sqrt ( 1.0_dp - xx*xx )
      fact = fact + 2.0_dp
    end do

  end if
!
!  J = M + 1 is the second nonzero function.
!
  if ( m + 1 <= n ) then
    cx(1:mm,m+1) = x(1:mm) * real ( 2 * m + 1, kind = 8 ) * cx(1:mm,m)
  end if
!
!  Now we use a three term recurrence.
!
  !$omp simd linear(j:n)
  do j = m + 2, n
    cx(1:mm,j) = ( real ( 2 * j     - 1, kind = 8 ) * x(1:mm) * cx(1:mm,j-1) &
                 + real (   - j - m + 1, kind = 8 ) *           cx(1:mm,j-2) ) &
                 / real (     j - m,     kind = 8 )
  end do

  return
end
subroutine pm_polynomial_values ( n_data, n, m, x, fx )
        use mod_kinds, only : i4, dp
        use omp_lib  
         !dir$ attributes forceinline :: pm_polynomial_values
        !dir$ code_align : 32 :: pm_polynomial_values
        !dir$ optimize : 3
        !dir$ attributes optimization_parameters: TARGET_ARCH=skylake_avx512 :: pm_polynomial_values
!*****************************************************************************80
!
!! PM_POLYNOMIAL_VALUES: selected values of Legendre polynomials Pm(n,m,x).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 August 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Parameters:
!
!    Input/output, integer(i4) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer(i4) N, integer(i4) M, 
!    real(kind=dp) X, the arguments of the function.
!
!    Output, real(kind=dp) FX, the value of the function.
!
  implicit none

  integer(i4), parameter :: n_max = 20

  real(kind=dp) fx
  real(kind=dp), save, dimension ( n_max ) :: fx_vec = (/ &
     0.0000000000000000_dp, &
    -0.5000000000000000_dp, &
     0.0000000000000000_dp, &
     0.3750000000000000_dp, &
     0.0000000000000000_dp, &
    -0.8660254037844386_dp, &
    -0.1299038105676658D+01, &
    -0.3247595264191645_dp, &
     0.1353164693413185D+01, &
    -0.2800000000000000_dp, &
     0.1175755076535925D+01, &
     0.2880000000000000D+01, &
    -0.1410906091843111D+02, &
    -0.3955078125000000D+01, &
    -0.9997558593750000D+01, &
     0.8265311444100484D+02, &
     0.2024442836815152D+02, &
    -0.4237997531890869D+03, &
     0.1638320624828339D+04, &
    -0.2025687389227225D+05 /)
  integer(i4) m
  integer(i4), save, dimension ( n_max ) :: m_vec = (/ &
    0, 0, 0, 0, &
    0, 1, 1, 1, &
    1, 0, 1, 2, &
    3, 2, 2, 3, &
    3, 4, 4, 5 /)
  integer(i4) n
  integer(i4) n_data
  integer(i4), save, dimension ( n_max ) :: n_vec = (/ &
    1,  2,  3,  4, &
    5,  1,  2,  3, &
    4,  3,  3,  3, &
    3,  4,  5,  6, &
    7,  8,  9, 10 /)
  real(kind=dp) x
  real(kind=dp), save, dimension ( n_max ) :: x_vec = (/ &
    0.00_dp, &
    0.00_dp, &
    0.00_dp, &
    0.00_dp, &
    0.00_dp, &
    0.50_dp, &
    0.50_dp, &
    0.50_dp, &
    0.50_dp, &
    0.20_dp, &
    0.20_dp, &
    0.20_dp, &
    0.20_dp, &
    0.25_dp, &
    0.25_dp, &
    0.25_dp, &
    0.25_dp, &
    0.25_dp, &
    0.25_dp, &
    0.25_dp /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    n = 0
    m = 0
    x = 0.0_dp
    fx = 0.0_dp
  else
    n = n_vec(n_data)
    m = m_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
subroutine pmn_polynomial_value ( mm, n, m, x, cx )
        use mod_kinds, only : i4, dp
        use omp_lib  
         
        !dir$ code_align : 32 :: pmn_polynomial_value
        !dir$ optimize : 3
        !dir$ attributes optimization_parameters: TARGET_ARCH=skylake_avx512 :: pmn_polynomial_value
!*****************************************************************************80
!
!! PMN_POLYNOMIAL_VALUE: normalized Legendre polynomial Pmn(n,m,x).
!
!  Discussion:
!
!    The unnormalized associated Legendre functions P_N^M(X) have
!    the property that
!
!      Integral ( -1 <= X <= 1 ) ( P_N^M(X) )^2 dX 
!      = 2 * ( N + M )! / ( ( 2 * N + 1 ) * ( N - M )! )
!
!    By dividing the function by the square root of this term,
!    the normalized associated Legendre functions have norm 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 March 2005
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!  Parameters:
!
!    Input, integer(i4) MM, the number of evaluation points.
!
!    Input, integer(i4) N, the maximum first index of the Legendre
!    function, which must be at least 0.
!
!    Input, integer(i4) M, the second index of the Legendre function,
!    which must be at least 0, and no greater than N.
!
!    Input, real(kind=dp) X(MM), the evaluation points.
!
!    Output, real(kind=dp) CX(MM,0:N), the function values.
!
  implicit none

  integer(i4) mm
  integer(i4) n

  real(kind=dp) cx(mm,0:n)
  real(kind=dp) factor
  integer(i4) j
  integer(i4) m
  real(kind=dp) r8_factorial
  real(kind=dp) :: xx
  real(kind=dp) x(mm)

  if ( m < 0 ) then
    !write ( *, '(a)' ) ' '
    !write ( *, '(a)' ) 'PMN_POLYNOMIAL_VALUE - Fatal error!'
    !write ( *, '(a,i8)' ) '  Input value of M is ', m
    !write ( *, '(a)' ) '  but M must be nonnegative.'
    !stop 1
    return
  end if
 
  if ( n < m ) then
    !write ( *, '(a)' ) ' '
    !write ( *, '(a)' ) 'PMN_POLYNOMIAL_VALUE - Fatal error!'
    !write ( *, '(a,i8)' ) '  Input value of M = ', m
    !write ( *, '(a,i8)' ) '  Input value of N = ', n
    !write ( *, '(a)' ) '  but M must be less than or equal to N.'
    !stop 1
    return
  end if

  cx(1:mm,0:n) = 0.0_dp

  if ( m <= n ) then
    cx(1:mm,m) = 1.0_dp
    factor = 1.0_dp
    !$omp simd linear(j:1) reduction(*:cx)
    do j = 1, m
      xx=x(1:mm)
      cx(1:mm,m) = - cx(1:mm,m) * factor * sqrt ( 1.0_dp - xx*xx)
      factor = factor + 2.0_dp
    end do
  end if

  if ( m + 1 <= n ) then
    cx(1:mm,m+1) = x(1:mm) * real ( 2 * m + 1, kind = 8 ) * cx(1:mm,m)
  end if

  do j = m + 2, n
    cx(1:mm,j) = ( real ( 2 * j     - 1, kind = 8 ) * x(1:mm) * cx(1:mm,j-1) &
                 + real (   - j - m + 1, kind = 8 ) *           cx(1:mm,j-2) ) &
                 / real (     j - m,     kind = 8 )
  end do
!
!  Normalization.
!
  !$omp simd reduction(*:cx) linear(j:1)
  do j = m, n
    factor = sqrt ( ( real ( 2 * j + 1, kind = 8 ) * r8_factorial ( j - m ) ) &
      / ( 2.0_dp * r8_factorial ( j + m ) ) )
    cx(1:mm,j) = cx(1:mm,j) * factor
  end do

  return
end
subroutine pmn_polynomial_values ( n_data, n, m, x, fx )
        use mod_kinds, only : i4, dp
       
        !dir$ attributes forceinline ::  pmn_polynomial_values
        !dir$ code_align : 32 :: pmn_polynomial_values
        !dir$ optimize : 3
        !dir$ attributes optimization_parameters: TARGET_ARCH=skylake_avx512 :: pmn_polynomial_values
!*****************************************************************************80
!
!! PMN_POLYNOMIAL_VALUES: selected values of the normalized Legendre polynomial Pmn(n,m,x).
!
!  Discussion:
!
!    In Mathematica, the unnormalized function can be evaluated by:
!
!      LegendreP [ n, m, x ]
!
!    The function is normalized by dividing by the factor:
!
!      sqrt ( 2 * ( n + m )! / ( 2 * n + 1 ) / ( n - m )! )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 September 2010
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Parameters:
!
!    Input/output, integer(i4) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer(i4) N, integer(i4) M, 
!    real(kind=dp) X, the arguments of the function.
!
!    Output, real(kind=dp) FX, the value of the function.
!
  implicit none

  integer(i4), parameter :: n_max = 21

  real(kind=dp) fx
  real(kind=dp), save, dimension ( n_max ) :: fx_vec = (/ &
    0.7071067811865475_dp, &
    0.6123724356957945_dp, &
   -0.7500000000000000_dp, &
   -0.1976423537605237_dp, &
   -0.8385254915624211_dp, &
    0.7261843774138907_dp, &
   -0.8184875533567997_dp, &
   -0.1753901900050285_dp, &
    0.9606516343087123_dp, &
   -0.6792832849776299_dp, &
   -0.6131941618102092_dp, &
    0.6418623720763665_dp, &
    0.4716705890038619_dp, &
   -0.1018924927466445D+01, &
    0.6239615396237876_dp, &
    0.2107022704608181_dp, &
    0.8256314721961969_dp, &
   -0.3982651281554632_dp, &
   -0.7040399320721435_dp, &
    0.1034723155272289D+01, &
   -0.5667412129155530_dp /)
  integer(i4) m
  integer(i4), save, dimension ( n_max ) :: m_vec = (/ &
    0, 0, 1, 0, &
    1, 2, 0, 1, &
    2, 3, 0, 1, &
    2, 3, 4, 0, &
    1, 2, 3, 4, &
    5 /)
  integer(i4) n
  integer(i4) n_data
  integer(i4), save, dimension ( n_max ) :: n_vec = (/ &
    0,  1,  1,  2, &
    2,  2,  3,  3, &
    3,  3,  4,  4, &
    4,  4,  4,  5, &
    5,  5,  5,  5, &
    5 /)
  real(kind=dp) x
  real(kind=dp), save, dimension ( n_max ) :: x_vec = (/ &
    0.50_dp, &
    0.50_dp, &
    0.50_dp, &
    0.50_dp, &
    0.50_dp, &
    0.50_dp, &
    0.50_dp, &
    0.50_dp, &
    0.50_dp, &
    0.50_dp, &
    0.50_dp, &
    0.50_dp, &
    0.50_dp, &
    0.50_dp, &
    0.50_dp, &
    0.50_dp, &
    0.50_dp, &
    0.50_dp, &
    0.50_dp, &
    0.50_dp, &
    0.50_dp /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    n = 0
    m = 0
    x = 0.0_dp
    fx = 0.0_dp
  else
    n = n_vec(n_data)
    m = m_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
subroutine pmns_polynomial_value ( mm, n, m, x, cx )
        use mod_kinds, only : i4, dp
        use omp_lib  
       
        !dir$ code_align : 32 :: pmns_polynomial_value
        !dir$ optimize : 3
        !dir$ attributes optimization_parameters: TARGET_ARCH=skylake_avx512 :: pmns_polynomial_value
!*****************************************************************************80
!
!! PMNS_POLYNOMIAL_VALUE: sphere-normalized Legendre polynomial Pmns(n,m,x).
!
!  Discussion:
!
!    The unnormalized associated Legendre functions P_N^M(X) have
!    the property that
!
!      Integral ( -1 <= X <= 1 ) ( P_N^M(X) )^2 dX 
!      = 2 * ( N + M )! / ( ( 2 * N + 1 ) * ( N - M )! )
!
!    By dividing the function by the square root of this term,
!    the normalized associated Legendre functions have norm 1.
!
!    However, we plan to use these functions to build spherical
!    harmonics, so we use a slightly different normalization factor of
!
!      sqrt ( ( ( 2 * N + 1 ) * ( N - M )! ) / ( 4 * pi * ( N + M )! ) ) 
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    03 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!  Parameters:
!
!    Input, integer(i4) MM, the number of evaluation points.
!
!    Input, integer(i4) N, the maximum first index of the Legendre
!    function, which must be at least 0.
!
!    Input, integer(i4) M, the second index of the Legendre function,
!    which must be at least 0, and no greater than N.
!
!    Input, real(kind=dp) X(MM), the evaluation points.
!
!    Output, real(kind=dp) CX(MM,0:N), the function values.
!
  implicit none

  integer(i4) mm
  integer(i4) n

  real(kind=dp) cx(mm,0:n)
  !dir$ assume_aligned cx:64
  real(kind=dp) factor
  integer(i4) j
  integer(i4) m
  real(kind=dp) r8_factorial
  real(kind=dp) :: xx
  real(kind=dp), parameter :: r8_pi = 3.141592653589793_dp
  real(kind=dp) x(mm)
  !dir$ assume_aligned x:64

  cx(1:mm,0:n) = 0.0_dp

  if ( m <= n ) then
    cx(1:mm,m) = 1.0_dp
    factor = 1.0_dp
    !$omp simd reduction(*:cx) linear(j:1)
    do j = 1, m
      xx=x(1:mm)
      cx(1:mm,m) = - cx(1:mm,m) * factor * sqrt ( 1.0_dp - xx)
      factor = factor + 2.0_dp
    end do
  end if

  if ( m + 1 <= n ) then
    cx(1:mm,m+1) = x(1:mm) * real ( 2 * m + 1, kind = 8 ) * cx(1:mm,m)
  end if

  do j = m + 2, n
    cx(1:mm,j) = ( real ( 2 * j     - 1, kind = 8 ) * x(1:mm) * cx(1:mm,j-1) &
                 + real (   - j - m + 1, kind = 8 ) *           cx(1:mm,j-2) ) &
                 / real (     j - m,     kind = 8 )
  end do
!
!  Normalization.
!
  !$omp simd reduction(*:cx) linear(j:1)
  do j = m, n
    factor = sqrt ( ( real ( 2 * j + 1, kind = 8 ) * r8_factorial ( j - m ) ) &
      / ( 4.0_dp * r8_pi * r8_factorial ( j + m ) ) )
    cx(1:mm,j) = cx(1:mm,j) * factor
  end do

  return
end
subroutine pmns_polynomial_values ( n_data, n, m, x, fx )
        use mod_kinds, only : i4, dp
       
        !dir$ attributes forceinline :: pmns_polynomial_values
        !dir$ code_align : 32 :: pmns_polynomial_values
        !dir$ optimize : 3
        !dir$ attributes optimization_parameters: TARGET_ARCH=skylake_avx512 :: pmns_polynomial_values
!*****************************************************************************80
!
!! PMNS_POLYNOMIAL_VALUES: selected values of the sphere-normalized Legendre polynomial Pmns(n,m,x).
!
!  Discussion:
!
!    In Mathematica, the unnormalized function can be evaluated by:
!
!      LegendreP [ n, m, x ]
!
!    The function is normalized by dividing by the factor:
!
!      sqrt ( 4 * pi * ( n + m )! / ( 2 * n + 1 ) / ( n - m )! )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 September 2010
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Parameters:
!
!    Input/output, integer(i4) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer(i4) N, integer(i4) M, 
!    real(kind=dp) X, the arguments of the function.
!
!    Output, real(kind=dp) FX, the value of the function.
!
  implicit none

  integer(i4), parameter :: n_max = 21

  real(kind=dp) fx
  real(kind=dp), save, dimension ( n_max ) :: fx_vec = (/ &
     0.2820947917738781_dp, &
     0.2443012559514600_dp, &
    -0.2992067103010745_dp, &
    -0.07884789131313000_dp, &
    -0.3345232717786446_dp, &
     0.2897056515173922_dp, &
    -0.3265292910163510_dp, &
    -0.06997056236064664_dp, &
     0.3832445536624809_dp, &
    -0.2709948227475519_dp, &
    -0.2446290772414100_dp, &
     0.2560660384200185_dp, &
     0.1881693403754876_dp, &
    -0.4064922341213279_dp, &
     0.2489246395003027_dp, &
     0.08405804426339821_dp, &
     0.3293793022891428_dp, &
    -0.1588847984307093_dp, &
    -0.2808712959945307_dp, &
     0.4127948151484925_dp, &
    -0.2260970318780046_dp /)
  integer(i4) m
  integer(i4), save, dimension ( n_max ) :: m_vec = (/ &
    0, 0, 1, 0, &
    1, 2, 0, 1, &
    2, 3, 0, 1, &
    2, 3, 4, 0, &
    1, 2, 3, 4, &
    5 /)
  integer(i4) n
  integer(i4) n_data
  integer(i4), save, dimension ( n_max ) :: n_vec = (/ &
    0,  1,  1,  2, &
    2,  2,  3,  3, &
    3,  3,  4,  4, &
    4,  4,  4,  5, &
    5,  5,  5,  5, &
    5 /)
  real(kind=dp) x
  real(kind=dp), save, dimension ( n_max ) :: x_vec = (/ &
    0.50_dp, &
    0.50_dp, &
    0.50_dp, &
    0.50_dp, &
    0.50_dp, &
    0.50_dp, &
    0.50_dp, &
    0.50_dp, &
    0.50_dp, &
    0.50_dp, &
    0.50_dp, &
    0.50_dp, &
    0.50_dp, &
    0.50_dp, &
    0.50_dp, &
    0.50_dp, &
    0.50_dp, &
    0.50_dp, &
    0.50_dp, &
    0.50_dp, &
    0.50_dp /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    n = 0
    m = 0
    x = 0.0_dp
    fx = 0.0_dp
  else
    n = n_vec(n_data)
    m = m_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
subroutine pn_pair_product ( p, table )
        use mod_kinds, only : i4, dp
        use omp_lib  
       
        !dir$ code_align : 32 :: pn_pair_product
        !dir$ optimize : 3
        !dir$ attributes optimization_parameters: TARGET_ARCH=skylake_avx512 :: pn_pair_product
!*****************************************************************************80
!
!! PN_PAIR_PRODUCT: pair products for normalized Legendre polynomial Pn(n,x).
!
!  Discussion:
!
!    Let Pn(n,x) represent the normalized Legendre polynomial of degree n.  
!
!    To check orthonormality, we compute
!
!      Tij = Integral ( -1.0 <= X <= +1.0 ) Pn(i,x) * Pn(j,x) dx
!
!    We will estimate these integrals using Gauss-Legendre quadrature.
!
!    The computed table should be the identity matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer(i4) P, the maximum degree of the polyonomial 
!    factors.  0 <= P.
!
!    Output, real(kind=dp) TABLE(0:P,0:P), the table of integrals.  
!
  implicit none

  integer(i4) p

  real(kind=dp) h_table(0:p)
  !dir$ attributes align : 64 :: h_table
  integer(i4) i
  integer(i4) j
  integer(i4) k
  integer(i4) order
  real(kind=dp) table(0:p,0:p)
  !dir$ assume_aligned table:64
  real(kind=dp), allocatable :: w_table(:)
  !dir$ attributes align : 64 :: w_table
  real(kind=dp) x(1)
  real(kind=dp) :: wt,ht
  real(kind=dp), allocatable :: x_table(:)
  !dir$ attributes align : 64 :: x_table
  table(0:p,0:p) = 0.0_dp

  order = p + 1

  allocate ( x_table(order) )
  allocate ( w_table(order) )

  call p_quadrature_rule ( order, x_table, w_table )
  wt=0.0_dp
  ht=0.0_dp
  do k = 1, order
    wt=w_table(k)
    x(1) = x_table(k)
    call pn_polynomial_value ( 1, p, x, h_table )

    do j = 0, p
      ht=h_table(j)
      !$omp simd reduction(+:table) linear(i:1)
      do i = 0, p
        table(i,j) = table(i,j) + wt * h_table(i) * ht
      end do
    end do

  end do

  deallocate ( w_table )
  deallocate ( x_table )

  return
end
subroutine pn_polynomial_coefficients ( n, c )
        use mod_kinds, only : i4, dp
        use omp_lib  
       
        !dir$ code_align : 32 :: pn_polynomial_coefficients 
        !dir$ optimize : 3
        !dir$ attributes optimization_parameters: TARGET_ARCH=skylake_avx512 :: pn_polynomial_coefficients 
!*****************************************************************************80
!
!! PN_POLYNOMIAL_COEFFICIENTS: coefficients of normalized Legendre Pn(n,x).
!
!  Discussion:
!
!    Pn(n,x) = P(n,x) * sqrt ( (2n+1)/2 )
!
!          1       x       x^2     x^3     x^4      x^5    x^6     x^7
!
!    0   0.707
!    1   0.000   1.224
!    2  -0.790   0.000   2.371
!    3   0.000  -2.806   0.000   4.677
!    4   0.795   0.000  -7.954   0.000   9.280
!    5   0.000   4.397   0.000 -20.520   0.000   18.468
!    6  -0.796   0.000  16.731   0.000 -50.193    0.000  36.808
!    7   0.000  -5.990   0.000  53.916   0.000 -118.616   0.000  73.429 
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    17 October 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!    Daniel Zwillinger, editor,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition,
!    CRC Press, 1996.
!
!  Parameters:
!
!    Input, integer(i4) N, the highest order polynomial to evaluate.
!    Note that polynomials 0 through N will be evaluated.
!
!    Output, real(kind=dp) C(0:N,0:N), the coefficients of the 
!    normalized Legendre polynomials of degree 0 through N.
!
  implicit none

  integer(i4) n

  real(kind=dp) c(0:n,0:n)
  !dir$ assume_aligned c:64
  integer(i4) i
  real(kind=dp) t

  if ( n < 0 ) then
    return
  end if
!
!  Compute P(i,x) coefficients.
!
  c(0:n,0:n) = 0.0_dp

  c(0,0) = 1.0_dp

  if ( 0 < n ) then
    c(1,1) = 1.0_dp
  end if
  !$omp simd linear(i:1)
  do i = 2, n
    c(i,0:i-2) =          real (   - i + 1, kind = 8 ) * c(i-2,0:i-2) &
                        / real (     i,     kind = 8 )
    c(i,1:i) = c(i,1:i) + real ( i + i - 1, kind = 8 ) * c(i-1,0:i-1) &
                        / real (     i,     kind = 8 )
  end do
!
!  Normalize them.
!
  !$omp simd reduction(*:c) linear(i:1) unroll partial(4)
  do i = 0, n
    t = sqrt ( real ( 2 * i + 1, kind = 8 ) / 0.5_dp )
    c(i,0:i) = c(i,0:i) * t
  end do
 
  return
end
subroutine pn_polynomial_value ( m, n, x, v )
        use mod_kinds, only : i4, dp
        use omp_lib  
        !dir$ attributes forceinline :: pn_polynomial_value
        !dir$ code_align : 32 :: pn_polynomial_value
        !dir$ optimize : 3
        !dir$ attributes optimization_parameters: TARGET_ARCH=skylake_avx512 :: pn_polynomial_value
!*****************************************************************************80
!
!! PN_POLYNOMIAL_VALUE evaluates the normalized Legendre polynomials Pn(n,x).
!
!  Discussion:
!
!    The normalized Legendre polynomials are orthonormal under the inner product
!    defined as integration from -1 to 1:
!
!      Integral ( -1 <= x <= +1 ) Pn(i,x) * Pn(j,x) dx = delta(i,j)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    14 March 2012
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!    Daniel Zwillinger, editor,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition,
!    CRC Press, 1996.
!
!  Parameters:
!
!    Input, integer(i4) M, the number of evaluation points.
!
!    Input, integer(i4) N, the highest order polynomial to evaluate.
!    Note that polynomials 0 through N will be evaluated.
!
!    Input, real(kind=dp) X(M), the evaluation points.
!
!    Output, real(kind=dp) V(M,0:N), the values of the Legendre polynomials 
!    of order 0 through N at the points X.
!
  implicit none

  integer(i4) m
  integer(i4) n

  integer(i4) j
  real(kind=dp) norm
  real(kind=dp) v(m,0:n)
  !dir$ assume_aligned v:64
  real(kind=dp) x(m)
  !dir$ assume_aligned x:64
  call p_polynomial_value ( m, n, x, v )

  !$omp simd reduction(/:v) unroll partial(2)
  do j = 0, n
    norm = sqrt ( 2.0_dp / real ( 2 * j + 1, kind = 8 ) )
    v(1:m,j) = v(1:m,j) / norm
  end do
 
  return
end
subroutine pn_polynomial_values ( n_data, n, x, fx )
        use mod_kinds, only : i4, dp
        
        !dir$ attributes forceinline :: pn_polynomial_values
        !dir$ code_align : 32 :: pn_polynomial_values
        !dir$ optimize : 3
        !dir$ attributes optimization_parameters: TARGET_ARCH=skylake_avx512 :: pn_polynomial_values
!*****************************************************************************80
!
!! PN_POLYNOMIAL_VALUES: selected values of the normalized Legendre polynomials.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 March 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Parameters:
!
!    Input/output, integer(i4) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer(i4) N, the order of the function.
!
!    Output, real(kind=dp) X, the point where the function is evaluated.
!
!    Output, real(kind=dp) FX, the value of the function.
!
  implicit none

  integer(i4), parameter :: n_max = 22

  real(kind=dp) fx
  real(kind=dp), save, dimension ( n_max ) :: fx_vec = (/ &
    0.7071067811865475_dp, & 
    0.3061862178478972_dp, & 
   -0.642337649721702_dp, & 
   -0.6284815141846855_dp, & 
    0.3345637065282053_dp, & 
    0.7967179601799685_dp, & 
    0.06189376866246124_dp, & 
   -0.766588850921089_dp, & 
   -0.4444760242953344_dp, & 
    0.5450094674858101_dp, & 
    0.7167706229835538_dp, & 
    0.0000000000000000_dp, & 
   -0.2759472322745781_dp, & 
   -0.5238320341483518_dp, & 
   -0.7155919752205163_dp, & 
   -0.823164625090267_dp, &  
   -0.8184875533567997_dp, &  
   -0.6734983296193094_dp, &  
   -0.360134523476992_dp, &  
    0.1496662954709581_dp, &  
    0.8839665576253438_dp, & 
    1.870828693386971_dp /)
  integer(i4) n
  integer(i4) n_data
  integer(i4), save, dimension ( n_max ) :: n_vec = (/ &
     0,  1,  2, &
     3,  4,  5, &
     6,  7,  8, &
     9, 10,  3, &
     3,  3,  3, &
     3,  3,  3, &
     3,  3,  3, &
     3 /)
  real(kind=dp) x
  real(kind=dp), save, dimension ( n_max ) :: x_vec = (/ &
    0.25_dp, &
    0.25_dp, &
    0.25_dp, &
    0.25_dp, &
    0.25_dp, &
    0.25_dp, &
    0.25_dp, &
    0.25_dp, &
    0.25_dp, &
    0.25_dp, &
    0.25_dp, &
    0.00_dp, &
    0.10_dp, &
    0.20_dp, &
    0.30_dp, &
    0.40_dp, &
    0.50_dp, &
    0.60_dp, &
    0.70_dp, &
    0.80_dp, &
    0.90_dp, &
    1.00_dp /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    n = 0
    x = 0.0_dp
    fx = 0.0_dp
  else
    n = n_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
function r8_factorial ( n )
        use mod_kinds, only : i4, dp
       
        !dir$ attributes forceinline :: r8_factorial
        !dir$ code_align : 32 :: r8_factorial
        !dir$ optimize : 3
        !dir$ attributes optimization_parameters: TARGET_ARCH=skylake_avx512 :: r8_factorial
!*****************************************************************************80
!
!! R8_FACTORIAL computes the factorial of N.
!
!  Discussion:
!
!    factorial ( N ) = product ( 1 <= I <= N ) I
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer(i4) N, the argument of the factorial function.
!    If N is less than 1, the function value is returned as 1.
!
!    Output, real(kind=dp) R8_FACTORIAL, the factorial of N.
!
  implicit none

  real(kind=dp) r8_factorial
  integer(i4) i
  integer(i4) n

  r8_factorial = 1.0_dp

  do i = 1, n
    r8_factorial = r8_factorial * real ( i, kind = 8 )
  end do

  return
end


!subroutine r8vec_linspace ( n, a, b, x )

!*****************************************************************************80
!
!! R8VEC_LINSPACE creates a vector of linearly spaced values.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    4 points evenly spaced between 0 and 12 will yield 0, 4, 8, 12.
!
!    In other words, the interval is divided into N-1 even subintervals,
!    and the endpoints of intervals are used as the points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 March 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer(i4) N, the number of entries in the vector.
!
!    Input, real(kind=dp) A_FIRST, A_LAST, the first and last entries.
!
!    Output, real(kind=dp) X(N), a vector of linearly spaced data.
!
  !implicit none

 ! integer(i4) n

 ! real(kind=dp) a
 ! real(kind=dp) b
 ! integer(i4) i
 ! real(kind=dp) x(n)

 ! if ( n == 1 ) then

 !   x(1) = ( a + b ) / 2.0_dp

 ! else

   ! do i = 1, n
  !    x(i) = ( real ( n - i,     kind = 8 ) * a   &
   !          + real (     i - 1, kind = 8 ) * b ) &
   !          / real ( n     - 1, kind = 8 )
   ! end do

  !end if

  !return
!end

