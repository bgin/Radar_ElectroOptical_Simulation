
module tabulated_quad

  use mod_kinds, only : i4,sp,dp

#if !defined(DIVSION_REPLACEMENT)
#define DIVISION_REPLACEMENT 1
#endif

#if 0
subroutine avint ( ntab, xtab, ytab, a, b, result )  

!*****************************************************************************80
!
!! AVINT estimates the integral of unevenly spaced data.
!
!  Discussion:
!
!    The data is given as NTAB pairs of values 
!    ( XTAB(1:NTAB), YTAB(1:NTAB) ).
!
!    The quadrature method uses overlapping parabolas and smoothing.
!
!  Modified:
!
!    10 February 2006
!
!  Reference:
!
!    Philip Davis, Philip Rabinowitz,
!    Methods of Numerical Integration,
!    Second Edition,
!    Dover, 2007,
!    ISBN: 0486453391,
!    LC: QA299.3.D28.
!
!    Paul Hennion,
!    Algorithm 77:
!    Interpolation, Differentiation and Integration,
!    Communications of the ACM,
!    Volume 5, page 96, 1962.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NTAB, the number of entries in XTAB and
!    YTAB.  NTAB must be at least 2.
!
!    Input, real ( kind = 8 ) XTAB(NTAB), the abscissas at which the
!    function values are given.  The XTAB's must be distinct
!    and in ascending order.
!
!    Input, real ( kind = 8 ) YTAB(NTAB), the function values,
!    YTAB(I) = F(XTAB(I)).
!
!    Input, real ( kind = 8 ) A, the lower limit of integration.  A should
!    be, but need not be, near one endpoint of the interval
!    (X(1), X(NTAB)).
!
!    Input, real ( kind = 8 ) B, the upper limit of integration.  B should
!    be, but need not be, near one endpoint of the interval
!    (X(1), X(NTAB)).
!
!    Output, real ( kind = 8 ) RESULT, the approximate value of the integral.
!
  implicit none

  integer ( kind = i4 ) ntab

  real ( kind = dp ) a
  real ( kind = dp ) b
  real ( kind = dp ) ba
  real ( kind = dp ) bb
  real ( kind = dp ) bc
  real ( kind = dp ) ca
  real ( kind = dp ) cb
  real ( kind = dp ) cc
  real ( kind = dp ) fa
  real ( kind = dp ) fb
  integer ( kind = i4 ) i
  integer ( kind = i4 ) inlft
  integer ( kind = i4 ) inrt
  integer ( kind = i4 ) istart
  integer ( kind = i4 ) istop
  real ( kind = dp ) result
  real ( kind = dp ) slope
  real ( kind = dp ) syl
  real ( kind = dp ) syl2
  real ( kind = dp ) syl3
  real ( kind = dp ) syu
  real ( kind = dp ) syu2
  real ( kind = dp ) syu3
  real ( kind = dp ) term1
  real ( kind = dp ) term2
  real ( kind = dp ) term3
  real ( kind = 8 ) total
  real ( kind = 8 ) x1
  real ( kind = 8 ) x12
  real ( kind = 8 ) x13
  real ( kind = 8 ) x2
  real ( kind = 8 ) x23
  real ( kind = 8 ) x3
  real ( kind = 8 ) xtab(ntab)
  real ( kind = 8 ) ytab(ntab)

  result = 0.0D+00

  if ( a == b ) then
    return
  end if

  if ( b < a ) then
  end if

  if ( ntab < 2 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'AVINT - Fatal error!'
    write ( *, '(a,i8)' ) '  NTAB is less than 3.  NTAB = ', ntab
    stop
  end if

  do i = 2, ntab
 
    if ( xtab(i) <= xtab(i-1) ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'AVINT - Fatal error!'
      write ( *, '(a)' ) '  XTAB(I) is not greater than XTAB(I-1).'
      write ( *, '(a,i8)' ) '  Here, I = ', I
      write ( *, '(a,g14.6)' ) '  XTAB(I-1) = ', xtab(i-1)
      write ( *, '(a,g14.6)' ) '  XTAB(I) =   ', xtab(i)
      stop
    end if
 
  end do
!
!  Special case for NTAB = 2.
!
  if ( ntab == 2 ) then
    slope = ( ytab(2) - ytab(1) ) / ( xtab(2) - xtab(1) )
    fa = ytab(1) + slope * ( a - xtab(1) )
    fb = ytab(2) + slope * ( b - xtab(2) )
    result = 0.5D+00 * ( fa + fb ) * ( b - a )
    return
  end if

  if ( xtab(ntab-2) < a .or. b < xtab(3) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'AVINT - Fatal error!'
    write ( *, '(a)' ) '  There were less than 3 function values'
    write ( *, '(a)' ) '  between the limits of integration.'
    stop
  end if

  i = 1
  do

    if ( a <= xtab(i) ) then
      exit
    end if

    i = i + 1

  end do

  inlft = i

  i = ntab

  do

    if ( xtab(i) <= b ) then
      exit
    end if

    i = i - 1

  end do

  inrt = i

  if ( inrt - inlft < 2 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'AVINT - Fatal error!'
    write ( *, '(a)' ) '  There were less than 3 function values'
    write ( *, '(a)' ) '  between the limits of integration.'
    stop
  end if

  if ( inlft == 1 ) then
    istart = 2
  else
    istart = inlft
  end if

  if ( inrt == ntab ) then
    istop = ntab - 1
  else
    istop = inrt
  end if

  total = 0.0D+00

  syl = a
  syl2 = syl * syl
  syl3 = syl2 * syl

  do i = istart, istop

    x1 = xtab(i-1)
    x2 = xtab(i)
    x3 = xtab(i+1)

    x12 = x1 - x2
    x13 = x1 - x3
    x23 = x2 - x3

    term1 =   ( ytab(i-1) ) / ( x12 * x13 )
    term2 = - ( ytab(i)   ) / ( x12 * x23 )
    term3 =   ( ytab(i+1) ) / ( x13 * x23 )

    ba = term1 + term2 + term3
    bb = - ( x2 + x3 ) * term1 - ( x1 + x3 ) * term2 - ( x1 + x2 ) * term3
    bc = x2 * x3 * term1 + x1 * x3 * term2 + x1 * x2 * term3

    if ( i == istart ) then
      ca = ba
      cb = bb
      cc = bc
    else
      ca = 0.5D+00 * ( ba + ca )
      cb = 0.5D+00 * ( bb + cb )
      cc = 0.5D+00 * ( bc + cc )
    end if

    syu = x2
    syu2 = syu * syu
    syu3 = syu2 * syu

    total = total + ca * ( syu3 - syl3 ) / 3.0D+00 &
                  + cb * ( syu2 - syl2 ) / 2.0D+00 &
                  + cc * ( syu  - syl )
    ca = ba
    cb = bb
    cc = bc

    syl  = syu
    syl2 = syu2
    syl3 = syu3

  end do

  syu = b
  syu2 = syu * syu
  syu3 = syu2 * syu

  result = total + ca * ( syu3 - syl3 ) / 3.0D+00 &
                 + cb * ( syu2 - syl2 ) / 2.0D+00 &
                 + cc * ( syu  - syl  )
  
  return
end subroutine avint
#endif












subroutine class_r8 ( kind, n, alpha, beta, b, a, muzero )

!*****************************************************************************80
!
!! CLASS sets recurrence coeeficients for various orthogonal polynomials.
!
!  Discussion:
!
!    CLASS supplies the coefficients A(J), B(J) of the recurrence relation
!
!      B(J)*P(J) (X) = (X-A(J))*P(J-1)(X) - B(J-1)*P(J-2)(X)
!
!    for the various classical (normalized) orthogonal polynomials,
!    and the zero-th moment
!
!      MUZERO = Integral W(X) DX
!
!    of the given polynomial's weight function W(X).  Since the
!    polynomials are orthonormalized, the tridiagonal matrix is
!    guaranteed to be symmetric.
!
!  Modified:
!
!    18 December 2002
!
!  Parameters:
!
!    Input, real ( kind = 8 ) ALPHA, BETA, parameters needed for Laguerre 
!    and Jacobi polynomials.
!
!    Input, integer ( kind = 4 ) KIND, specifies which polynomial is to be handled:
!    1: Legendre polynomials P(X) on (-1, +1),
!    W(X) = 1.
!    2: Chebyshev polynomials of the first kind T(X) on (-1, +1),
!    W(X) = 1 / SQRT(1 - X*X)
!    3: Chebyshev polynomials of the second kind U(X) on (-1, +1),
!    W(X) = SQRT(1 - X*X)
!    4: Hermite polynomials H(X) on (-infinity,+infinity),
!    W(X) = EXP(-X**2)
!    5: Jacobi polynomials P(ALPHA,BETA)(X) on (-1, +1),
!    W(X) = (1-X)**ALPHA + (1+X)**BETA,
!    ALPHA and BETA greater than -1.
!    6: Laguerre polynomials, L(ALPHA)(X) on (0, +infinity),
!    W(X) = EXP(-X) * X**ALPHA,
!    ALPHA greater than -1.
!
!    Input, integer ( kind = 4 ) N, specifies the number of coefficients to
!    calculate.
!
!    Input, real ( kind = 8 ) ALPHA, the value of the ALPHA parameter,
!    required only for Jacobi or Laguerre polynomials.
!
!    Input, real ( kind = 8 ) BETA, the value of the BETA parameter,
!    required only for Jacobi polynomials.
!
!    Output, real ( kind = 8 ) B(N-1), the offdiagonal coefficients.
!
!    Output, real ( kind = 8 ) A(N), the diagonal coefficients.
!
!    Output, real ( kind = 8 ) MUZERO, the zero-th moment, Integral W(X) DX,
!    of the polynomial's weight function over its interval of
!    definition.
!
  implicit none

  integer ( kind = i4 ) n

  real ( kind = dp ) a(n)
  real ( kind = dp ) abi
  real ( kind = dp ) alpha
  real ( kind = dp ) b(n-1)
  real ( kind = dp ) beta
  real ( kind = dp ) gamma
  integer ( kind = i4 ) i
  integer ( kind = i4 ) kind
  real ( kind = dp ) muzero
  real ( kind = dp ), parameter :: pi = 3.14159265358979323846264338328_dp
!
!  KIND = 1:
!
!  Legendre polynomials P(X) on (-1, +1),
!  W(X) = 1.
!
  if ( kind == 1 ) then
 
    muzero = 2.0_dp
 
    a(1:n) = 0.0_dp
 
    do i = 1, n-1
      b(i) = real ( i, kind = 8 ) &
        / sqrt ( 4.0_dp * real ( i * i, kind = 8 ) - 1.0_dp )
    end do
!
!  KIND = 2:
!
!  Chebyshev polynomials of the first kind T(X) on (-1, +1),
!  W(X) = 1 / SQRT(1 - X*X)
!
  else if ( kind == 2 ) then
 
    muzero = pi
    a(1:n) = 0.0_dp
    b(1) = 0.707106781186547524400844362105_dp
    b(1:n-1) = 0.5_dp
!
!  KIND = 3:
!
!  Chebyshev polynomials of the second kind U(X) on (-1, +1),
!  W(X) = SQRT(1 - X*X)
!
  else if ( kind == 3 ) then
 
    muzero = 1.57079632679489661923132169164_dp
    a(1:n) = 0.0_dp
    b(1:n-1) = 0.5_dp
!
!  KIND = 4:
!
!  Hermite polynomials H(X) on (-infinity,+infinity),
!  W(X) = EXP(-X**2)
!
  else if ( kind == 4 ) then
 
    muzero = 1.772453850905516027298167483341_dp
    a(1:n) = 0.0_dp
    do i = 1, n-1
      b(i) = sqrt ( real ( i, kind = 8 ) * 0.5_dp )
    end do
!
!  KIND = 5:
!
!  Jacobi polynomials P(ALPHA,BETA)(X) on (-1, +1),
!  W(X) = (1-X)**ALPHA + (1+X)**BETA,
!  ALPHA and BETA greater than -1
!
  else if ( kind == 5 ) then
 
    muzero = 2.0_dp**( alpha + beta + 1.0_dp ) * gamma ( alpha + 1.0_dp ) &
      * gamma ( beta + 1.0_dp ) / gamma ( 2.0_dp + alpha + beta )
 
    do i = 1, n
      a(i) = ( beta**2 - alpha**2 ) / &
        ( ( 2.0_dp * real ( i - 1, kind = 8 ) + alpha + beta ) &
        * ( 2.0_dp * real ( i, kind = 8 ) + alpha + beta ) )
    end do
 
    abi = 2.0_dp + alpha + beta
    b(1) = sqrt ( 4.0_dp * ( 1.0_dp + alpha ) * ( 1.0_dp + beta ) &
      / ( ( abi + 1.0_dp ) * abi * abi ) )
 
    do i = 2, n-1
      abi = real ( 2 * i ) + alpha + beta
      b(i) = sqrt ( 4.0_dp * real ( i, kind = dp ) &
        * ( real ( i, kind = dp ) + alpha ) &
        * ( real ( i, kind = dp ) + beta ) &
        * ( real ( i, kind = dp ) + alpha + beta ) / &
        ( ( abi * abi - 1.0_dp ) * abi * abi ) )
    end do
!
!  KIND = 6:
!
!  Laguerre polynomials
!
!  L(ALPHA)(X) on (0, +infinity),
!  W(X) = EXP(-X) * X**ALPHA,
!  ALPHA greater than -1.
!
  else if ( kind == 6 ) then
 
    muzero = gamma ( alpha + 1.0_dp )
 
    do i = 1, n
      a(i) = 2.0_dp * real ( i, kind = dp ) - 1.0_dp + alpha
    end do
 
    do i = 1, n-1
      b(i) = sqrt ( real ( i, kind = dp ) * ( real ( i, kind = dp ) + alpha ) )
    end do
 
  end if
 
  return
end subroutine class_r8


subroutine class_r4 ( kind, n, alpha, beta, b, a, muzero )

!*****************************************************************************80
!
!! CLASS sets recurrence coeeficients for various orthogonal polynomials.
!
!  Discussion:
!
!    CLASS supplies the coefficients A(J), B(J) of the recurrence relation
!
!      B(J)*P(J) (X) = (X-A(J))*P(J-1)(X) - B(J-1)*P(J-2)(X)
!
!    for the various classical (normalized) orthogonal polynomials,
!    and the zero-th moment
!
!      MUZERO = Integral W(X) DX
!
!    of the given polynomial's weight function W(X).  Since the
!    polynomials are orthonormalized, the tridiagonal matrix is
!    guaranteed to be symmetric.
!
!  Modified:
!
!    18 December 2002
!
!  Parameters:
!
!    Input, real ( kind = 8 ) ALPHA, BETA, parameters needed for Laguerre 
!    and Jacobi polynomials.
!
!    Input, integer ( kind = 4 ) KIND, specifies which polynomial is to be handled:
!    1: Legendre polynomials P(X) on (-1, +1),
!    W(X) = 1.
!    2: Chebyshev polynomials of the first kind T(X) on (-1, +1),
!    W(X) = 1 / SQRT(1 - X*X)
!    3: Chebyshev polynomials of the second kind U(X) on (-1, +1),
!    W(X) = SQRT(1 - X*X)
!    4: Hermite polynomials H(X) on (-infinity,+infinity),
!    W(X) = EXP(-X**2)
!    5: Jacobi polynomials P(ALPHA,BETA)(X) on (-1, +1),
!    W(X) = (1-X)**ALPHA + (1+X)**BETA,
!    ALPHA and BETA greater than -1.
!    6: Laguerre polynomials, L(ALPHA)(X) on (0, +infinity),
!    W(X) = EXP(-X) * X**ALPHA,
!    ALPHA greater than -1.
!
!    Input, integer ( kind = 4 ) N, specifies the number of coefficients to
!    calculate.
!
!    Input, real ( kind = 8 ) ALPHA, the value of the ALPHA parameter,
!    required only for Jacobi or Laguerre polynomials.
!
!    Input, real ( kind = 8 ) BETA, the value of the BETA parameter,
!    required only for Jacobi polynomials.
!
!    Output, real ( kind = 8 ) B(N-1), the offdiagonal coefficients.
!
!    Output, real ( kind = 8 ) A(N), the diagonal coefficients.
!
!    Output, real ( kind = 8 ) MUZERO, the zero-th moment, Integral W(X) DX,
!    of the polynomial's weight function over its interval of
!    definition.
!
  implicit none

  integer ( kind = i4 ) n

  real ( kind = sp ) a(n)
  real ( kind = sp ) abi
  real ( kind = sp ) alpha
  real ( kind = sp ) b(n-1)
  real ( kind = sp ) beta
  real ( kind = sp ) gamma
  integer ( kind = i4 ) i
  integer ( kind = i4 ) kind
  real ( kind = sp ) muzero
  real ( kind = sp ), parameter :: pi = 3.14159265358979323846264338328_sp
!
!  KIND = 1:
!
!  Legendre polynomials P(X) on (-1, +1),
!  W(X) = 1.
!
  if ( kind == 1 ) then
 
    muzero = 2.0_sp
 
    a(1:n) = 0.0_sp
 
    do i = 1, n-1
      b(i) = real ( i, kind = 4 ) &
        / sqrt ( 4.0_sp * real ( i * i, kind = 4 ) - 1.0_sp )
    end do
!
!  KIND = 2:
!
!  Chebyshev polynomials of the first kind T(X) on (-1, +1),
!  W(X) = 1 / SQRT(1 - X*X)
!
  else if ( kind == 2 ) then
 
    muzero = pi
    a(1:n) = 0.0_sp
    b(1) = 0.707106781186547524400844362105_sp
    b(1:n-1) = 0.5_sp
!
!  KIND = 3:
!
!  Chebyshev polynomials of the second kind U(X) on (-1, +1),
!  W(X) = SQRT(1 - X*X)
!
  else if ( kind == 3 ) then
 
    muzero = 1.57079632679489661923132169164_sp
    a(1:n) = 0.0_sp
    b(1:n-1) = 0.5_sp
!
!  KIND = 4:
!
!  Hermite polynomials H(X) on (-infinity,+infinity),
!  W(X) = EXP(-X**2)
!
  else if ( kind == 4 ) then
 
    muzero = 1.772453850905516027298167483341_sp
    a(1:n) = 0.0_sp
    do i = 1, n-1
      b(i) = sqrt ( real ( i, kind = 4 ) * 0.5_sp )
    end do
!
!  KIND = 5:
!
!  Jacobi polynomials P(ALPHA,BETA)(X) on (-1, +1),
!  W(X) = (1-X)**ALPHA + (1+X)**BETA,
!  ALPHA and BETA greater than -1
!
  else if ( kind == 5 ) then
 
    muzero = 2.0_sp**( alpha + beta + 1.0_sp ) * gamma ( alpha + 1.0_sp ) &
      * gamma ( beta + 1.0_sp ) / gamma ( 2.0_sp + alpha + beta )
 
    do i = 1, n
      a(i) = ( beta**2 - alpha**2 ) / &
        ( ( 2.0_sp * real ( i - 1, kind = 4 ) + alpha + beta ) &
        * ( 2.0_sp * real ( i, kind = 4 ) + alpha + beta ) )
    end do
 
    abi = 2.0_sp + alpha + beta
    b(1) = sqrt ( 4.0_sp * ( 1.0_sp + alpha ) * ( 1.0_sp + beta ) &
      / ( ( abi + 1.0_sp ) * abi * abi ) )
 
    do i = 2, n-1
      abi = real ( 2 * i,kind = sp ) + alpha + beta
      b(i) = sqrt ( 4.0_sp * real ( i, kind = sp ) &
        * ( real ( i, kind = sp ) + alpha ) &
        * ( real ( i, kind = sp ) + beta ) &
        * ( real ( i, kind = sp ) + alpha + beta ) / &
        ( ( abi * abi - 1.0_sp ) * abi * abi ) )
    end do
!
!  KIND = 6:
!
!  Laguerre polynomials
!
!  L(ALPHA)(X) on (0, +infinity),
!  W(X) = EXP(-X) * X**ALPHA,
!  ALPHA greater than -1.
!
  else if ( kind == 6 ) then
 
    muzero = gamma ( alpha + 1.0_sp )
 
    do i = 1, n
      a(i) = 2.0_sp * real ( i, kind = sp ) - 1.0_sp + alpha
    end do
 
    do i = 1, n-1
      b(i) = sqrt ( real ( i, kind = sp ) * ( real ( i, kind = sp ) + alpha ) )
    end do
 
  end if
 
  return
end subroutine class_r4




subroutine cspint_r8 ( ntab, xtab, ftab, a, b, y, e, work, result )

!*****************************************************************************80
!
!! CSPINT estimates the integral of a tabulated function.
!
!  Discussion:
!
!    The routine is given the value of a function F(X) at a set of 
!    nodes XTAB, and estimates
!
!      Integral ( A <= X <= B ) F(X) DX
!
!    by computing the cubic natural spline S(X) that interpolates
!    F(X) at the nodes, and then computing
!
!      Integral ( A <= X <= B ) S(X) DX
!
!    exactly.
!
!    Other output from the program includes the definite integral
!    from X(1) to X(I) of S(X), and the coefficients necessary for
!    the user to evaluate the spline S(X) at any point.
!
!  Modified:
!
!    10 February 2006
!
!  Reference:
!
!    Philip Davis, Philip Rabinowitz,
!    Methods of Numerical Integration,
!    Second Edition,
!    Dover, 2007,
!    ISBN: 0486453391,
!    LC: QA299.3.D28.
!
!    Carl DeBoor,
!    A Practical Guide to Splines,
!    Springer, 2001,
!    ISBN: 0387953663.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NTAB, the number of entries in FTAB and
!    XTAB.  NTAB must be at least 3.
!
!    Input, real ( kind = 8 ) XTAB(NTAB), contains the points at which the
!    function was evaluated.  The XTAB's must be distinct and
!    in ascending order.
!
!    Input, real ( kind = 8 ) FTAB(NTAB), contains the tabulated values of
!    the function, FTAB(I) = F(XTAB(I)).
!
!    Input, real ( kind = 8 ) A, lower limit of integration.
!
!    Input, real ( kind = 8 ) B, upper limit of integration.
!
!    Output, real ( kind = 8 ) Y(3,NTAB), will contain the coefficients
!    of the interpolating natural spline over each subinterval.
!    For XTAB(I) <= X <= XTAB(I+1),
!      S(X) = FTAB(I) + Y(1,I)*(X-XTAB(I))
!                   + Y(2,I)*(X-XTAB(I))**2
!                   + Y(3,I)*(X-XTAB(I))**3
!
!    Output, real ( kind = 8 ) E(NTAB), E(I) = the definite integral from
!    XTAB(1) to XTAB(I) of S(X).
!
!    Workspace, real ( kind = 8 ) WORK(NTAB).
!
!    Output, real ( kind = 8 ) RESULT, the estimated value of the integral.
!
  implicit none

  integer ( kind = i4 ) ntab

  real ( kind = dp ) a
  real ( kind = dp ) b
  real ( kind = dp ) e(ntab)
  real ( kind = dp ) ftab(ntab)
  integer ( kind = i4 ) i
  integer ( kind = i4 ) j
  real ( kind = dp ) r
  real ( kind = dp ) result
  real ( kind = dp ) s
  real ( kind = dp ) term
  real ( kind = dp ) u
  real ( kind = dp ) work(ntab)
  real ( kind = dp ) xtab(ntab)
  real ( kind = dp ) y(3,ntab)

  if ( ntab < 3 ) then
    !write ( *, '(a)' ) ' '
    !write ( *, '(a)' ) 'CSPINT - Fatal error!'
    !write ( *, '(a,i8)' ) '  NTAB must be at least 3, but input NTAB = ', ntab
    return
  end if
 
  do i = 1, ntab-1
 
    if ( xtab(i+1) <= xtab(i) ) then
      !write ( *, '(a)' ) ' '
      !write ( *, '(a)' ) 'CSPINT - Fatal error!'
      !write ( *, '(a)' ) '  Nodes not in strict increasing order.'
      !write ( *, '(a,i8)' ) '  XTAB(I) <= XTAB(I-1) for I=',i
      !write ( *, '(a,g14.6)' ) '  XTAB(I) = ',xtab(i)
      !write ( *, '(a,g14.6)' ) '  XTAB(I-1) = ',xtab(i-1)
      return
    end if
 
  end do
 
  s = 0.0_dp
  do i = 1, ntab-1
    r = ( ftab(i+1) - ftab(i) ) / ( xtab(i+1) - xtab(i) )
    y(2,i) = r - s
    s = r
  end do
 
  result = 0.0_dp
  s = 0.0_dp
  r = 0.0_dp
  y(2,1) = 0.0_dp
  y(2,ntab) = 0.0_dp
 
  do i = 2, ntab-1
    y(2,i) = y(2,i) + r * y(2,i-1)
    work(i) = 2.0_dp * ( xtab(i-1) - xtab(i+1) ) - r * s
    s = xtab(i+1) - xtab(i)
    r = s / work(i)
  end do
 
  do j = 2, ntab-1
    i = ntab+1-j
    y(2,i) = ( ( xtab(i+1) - xtab(i) ) * y(2,i+1) - y(2,i) ) / work(i)
  end do
 
  do i = 1, ntab-1
    s = xtab(i+1) - xtab(i)
    r = y(2,i+1) - y(2,i)
    y(3,i) = r / s
    y(2,i) = 3.0_dp * y(2,i)
    y(1,i) = ( ftab(i+1) - ftab(i) ) / s - ( y(2,i) + r ) * s
  end do
 
  e(1) = 0.0_dp
  do i = 1, ntab-1
    s = xtab(i+1)-xtab(i)
    term = ((( y(3,i) * 0.25_dp * s + y(2,i) / 3.0_dp ) * s &
      + y(1,i) * 0.5_dp ) * s + ftab(i) ) * s
    e(i+1) = e(i) + term
  end do
!
!  Determine where the endpoints A and B lie in the mesh of XTAB's.
!
  r = a
  u = 1.0_dp
 
  do j = 1, 2
!
!  The endpoint is less than or equal to XTAB(1).
!
    if ( r <= xtab(1) ) then
      result = result - u * ( ( r - xtab(1) ) * y(1,1) * 0.5_dp &
        + ftab(1) ) * ( r - xtab(1) )
!
!  The endpoint is greater than or equal to XTAB(NTAB).
!
    else if ( xtab(ntab) <= r ) then

      result = result -u * ( e(ntab) + ( r - xtab(ntab) ) &
        * ( ftab(ntab) + 0.5_dp * ( ftab(ntab-1) &
        + ( xtab(ntab) - xtab(ntab-1) ) * y(1,ntab-1) ) &
        * ( r - xtab(ntab) )))
!
!  The endpoint is strictly between XTAB(1) and XTAB(NTAB).
!
    else

      do i = 1, ntab-1
 
        if ( r <= xtab(i+1) ) then
          r = r - xtab(i)
          result = result - u * ( e(i) + ( ( ( &
              y(3,i) * 0.25_dp  * r &
            + y(2,i) / 3.0_dp ) * r &
            + y(1,i) * 0.5_dp ) * r + ftab(i) ) * r )
          go to 120
        end if
 
      end do
 
    end if
 
  120   continue
 
    u = -1.0_dp
    r = b
 
  end do
 
  return
end subroutine cspint_r8


subroutine cspint_r4 ( ntab, xtab, ftab, a, b, y, e, work, result )

!*****************************************************************************80
!
!! CSPINT estimates the integral of a tabulated function.
!
!  Discussion:
!
!    The routine is given the value of a function F(X) at a set of 
!    nodes XTAB, and estimates
!
!      Integral ( A <= X <= B ) F(X) DX
!
!    by computing the cubic natural spline S(X) that interpolates
!    F(X) at the nodes, and then computing
!
!      Integral ( A <= X <= B ) S(X) DX
!
!    exactly.
!
!    Other output from the program includes the definite integral
!    from X(1) to X(I) of S(X), and the coefficients necessary for
!    the user to evaluate the spline S(X) at any point.
!
!  Modified:
!
!    10 February 2006
!
!  Reference:
!
!    Philip Davis, Philip Rabinowitz,
!    Methods of Numerical Integration,
!    Second Edition,
!    Dover, 2007,
!    ISBN: 0486453391,
!    LC: QA299.3.D28.
!
!    Carl DeBoor,
!    A Practical Guide to Splines,
!    Springer, 2001,
!    ISBN: 0387953663.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NTAB, the number of entries in FTAB and
!    XTAB.  NTAB must be at least 3.
!
!    Input, real ( kind = 8 ) XTAB(NTAB), contains the points at which the
!    function was evaluated.  The XTAB's must be distinct and
!    in ascending order.
!
!    Input, real ( kind = 8 ) FTAB(NTAB), contains the tabulated values of
!    the function, FTAB(I) = F(XTAB(I)).
!
!    Input, real ( kind = 8 ) A, lower limit of integration.
!
!    Input, real ( kind = 8 ) B, upper limit of integration.
!
!    Output, real ( kind = 8 ) Y(3,NTAB), will contain the coefficients
!    of the interpolating natural spline over each subinterval.
!    For XTAB(I) <= X <= XTAB(I+1),
!      S(X) = FTAB(I) + Y(1,I)*(X-XTAB(I))
!                   + Y(2,I)*(X-XTAB(I))**2
!                   + Y(3,I)*(X-XTAB(I))**3
!
!    Output, real ( kind = 8 ) E(NTAB), E(I) = the definite integral from
!    XTAB(1) to XTAB(I) of S(X).
!
!    Workspace, real ( kind = 8 ) WORK(NTAB).
!
!    Output, real ( kind = 8 ) RESULT, the estimated value of the integral.
!
  implicit none

  integer ( kind = i4 ) ntab

  real ( kind = sp ) a
  real ( kind = sp ) b
  real ( kind = sp ) e(ntab)
  real ( kind = sp ) ftab(ntab)
  integer ( kind = i4 ) i
  integer ( kind = i4 ) j
  real ( kind = sp ) r
  real ( kind = sp ) result
  real ( kind = sp ) s
  real ( kind = sp ) term
  real ( kind = sp ) u
  real ( kind = sp ) work(ntab)
  real ( kind = sp ) xtab(ntab)
  real ( kind = sp ) y(3,ntab)

  if ( ntab < 3 ) then
    !write ( *, '(a)' ) ' '
    !write ( *, '(a)' ) 'CSPINT - Fatal error!'
    !write ( *, '(a,i8)' ) '  NTAB must be at least 3, but input NTAB = ', ntab
    return
  end if
 
  do i = 1, ntab-1
 
    if ( xtab(i+1) <= xtab(i) ) then
      !write ( *, '(a)' ) ' '
      !write ( *, '(a)' ) 'CSPINT - Fatal error!'
      !write ( *, '(a)' ) '  Nodes not in strict increasing order.'
      !write ( *, '(a,i8)' ) '  XTAB(I) <= XTAB(I-1) for I=',i
      !write ( *, '(a,g14.6)' ) '  XTAB(I) = ',xtab(i)
      !write ( *, '(a,g14.6)' ) '  XTAB(I-1) = ',xtab(i-1)
      return
    end if
 
  end do
 
  s = 0.0_sp
  do i = 1, ntab-1
    r = ( ftab(i+1) - ftab(i) ) / ( xtab(i+1) - xtab(i) )
    y(2,i) = r - s
    s = r
  end do
 
  result = 0.0_sp
  s = 0.0_sp
  r = 0.0_sp
  y(2,1) = 0.0_sp
  y(2,ntab) = 0.0_sp
 
  do i = 2, ntab-1
    y(2,i) = y(2,i) + r * y(2,i-1)
    work(i) = 2.0_dp * ( xtab(i-1) - xtab(i+1) ) - r * s
    s = xtab(i+1) - xtab(i)
    r = s / work(i)
  end do
 
  do j = 2, ntab-1
    i = ntab+1-j
    y(2,i) = ( ( xtab(i+1) - xtab(i) ) * y(2,i+1) - y(2,i) ) / work(i)
  end do
 
  do i = 1, ntab-1
    s = xtab(i+1) - xtab(i)
    r = y(2,i+1) - y(2,i)
    y(3,i) = r / s
    y(2,i) = 3.0_sp * y(2,i)
    y(1,i) = ( ftab(i+1) - ftab(i) ) / s - ( y(2,i) + r ) * s
  end do
 
  e(1) = 0.0_sp
  do i = 1, ntab-1
    s = xtab(i+1)-xtab(i)
    term = ((( y(3,i) * 0.25_sp * s + y(2,i) / 3.0_sp ) * s &
      + y(1,i) * 0.5_sp ) * s + ftab(i) ) * s
    e(i+1) = e(i) + term
  end do
!
!  Determine where the endpoints A and B lie in the mesh of XTAB's.
!
  r = a
  u = 1.0_sp
 
  do j = 1, 2
!
!  The endpoint is less than or equal to XTAB(1).
!
    if ( r <= xtab(1) ) then
      result = result - u * ( ( r - xtab(1) ) * y(1,1) * 0.5_sp &
        + ftab(1) ) * ( r - xtab(1) )
!
!  The endpoint is greater than or equal to XTAB(NTAB).
!
    else if ( xtab(ntab) <= r ) then

      result = result -u * ( e(ntab) + ( r - xtab(ntab) ) &
        * ( ftab(ntab) + 0.5_sp * ( ftab(ntab-1) &
        + ( xtab(ntab) - xtab(ntab-1) ) * y(1,ntab-1) ) &
        * ( r - xtab(ntab) )))
!
!  The endpoint is strictly between XTAB(1) and XTAB(NTAB).
!
    else

      do i = 1, ntab-1
 
        if ( r <= xtab(i+1) ) then
          r = r - xtab(i)
          result = result - u * ( e(i) + ( ( ( &
              y(3,i) * 0.25_sp  * r &
            + y(2,i) / 3.0_sp ) * r &
            + y(1,i) * 0.5_sp ) * r + ftab(i) ) * r )
          go to 120
        end if
 
      end do
 
    end if
 
  120   continue
 
    u = -1.0_sp
    r = b
 
  end do
 
  return
end subroutine cspint_r4






subroutine cubint_r8 ( ntab, xtab, ftab, ia, ib, result, error )

!*****************************************************************************80
!
!! CUBINT approximates an integral using cubic interpolation of data.
!
!  Discussion:
!
!    The integral to be approximated is
! 
!      Integral ( XTAB(IB) <= X <= XTAB(IA) ) F(X) DX
!
!    The routine estimates the error in integration.
!
!  Modified:
!
!    10 February 2006
!
!  Reference:
!
!    Philip Davis, Philip Rabinowitz,
!    Methods of Numerical Integration,
!    Second Edition,
!    Dover, 2007,
!    ISBN: 0486453391,
!    LC: QA299.3.D28.
!
!    Philip Gill, GF Miller,
!    An algorithm for the integration of unequally spaced data,
!    The Computer Journal, 
!    Number 15, Number 1, 1972, pages 80-83.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NTAB, the number of tabulated points.
!    NTAB must be at least 4.
!
!    Input, real ( kind = 8 ) XTAB(NTAB), contains the points at which the
!    function was tabulated.  XTAB should contain distinct
!    values, given in ascending order.
!
!    Input, real ( kind = 8 ) FTAB(NTAB), contains the tabulated function
!    values, FTAB(I) = F(XTAB(I)).
!
!    Input, integer ( kind = 4 ) IA, the entry of XTAB at which integration
!    is to begin.  IA must be no less than 1 and no greater
!    than NTAB.
!
!    Input, integer ( kind = 4 ) IB, the entry of XTAB at which integration
!    is to end.  IB must be no less than 1 and no greater than
!    NTAB.
!
!    Output, real ( kind = 8 ) RESULT, the approximate value of the
!    integral from XTAB(IA) to XTAB(IB) of the function.
!
!    Output, real ( kind = 8 ) ERROR, an estimate of the error in
!    integration.
!
  implicit none

  integer ( kind = i4 ) ntab

  real ( kind = dp ) c
  real ( kind = dp ) d1
  real ( kind = dp ) d2
  real ( kind = dp ) d3
  real ( kind = dp ) error
  real ( kind = dp ) ftab(ntab)
  real ( kind = dp ) h1
  real ( kind = dp ) h2
  real ( kind = dp ) h3
  real ( kind = dp ) h4
  integer ( kind = i4 ) i
  integer ( kind = i4 ) ia
  integer ( kind = i4 ) ib
  integer ( kind = i4 ) ind
  integer ( kind = i4 ) it
  integer ( kind = i4 ) j
  integer ( kind = i4 ) k
  real ( kind = dp ) r1
  real ( kind = dp ) r2
  real ( kind = dp ) r3
  real ( kind = dp ) r4
  real ( kind = dp ) result
  real ( kind = dp ) s
  real ( kind = dp ) term
  real ( kind = dp ) xtab(ntab)

  result = 0.0_dp
  error = 0.0_dp
 
  if ( ia == ib ) then
    return
  end if
 
  if ( ntab < 4 ) then
    !write ( *, '(a)' ) ' '
    !write ( *, '(a)' ) 'CUBINT - Fatal error!'
    !write ( *, '(a,i8)' ) '  NTAB must be at least 4, but input NTAB = ', ntab
     !stop
    return
  end if
 
  if ( ia < 1 ) then
    !write ( *, '(a)' ) ' '
    !write ( *, '(a)' ) 'CUBINT - Fatal error!'
    !write ( *, '(a,i8)' ) '  IA must be at least 1, but input IA = ', ia
     !stop
    return
  end if
 
  if ( ntab < ia ) then
    !write ( *, '(a)' ) ' '
    !write ( *, '(a)' ) 'CUBINT - Fatal error!'
    !write ( *, '(a,i8)' ) '  IA must be <= NTAB, but input IA = ', ia
     !stop
    return
  end if
 
  if ( ib < 1 ) then
    !write ( *, '(a)' ) ' '
    !write ( *, '(a)' ) 'CUBINT - Fatal error!'
    !write ( *, '(a,i8)' ) '  IB must be at least 1, but input IB = ', ib
     !stop
    return
  end if
 
  if ( ntab < ib ) then
    !write ( *, '(a)' ) ' '
    !write ( *, '(a)' ) 'CUBINT - Fatal error!'
    !write ( *, '(a,i8)' ) '  IB must be <= NTAB, but input IB = ', ib
     !stop
    return
  end if
!
!  Temporarily switch IA and IB, and store minus sign in IND
!  so that, while integration is carried out from low X's
!  to high ones, the sense of the integral is preserved.
!
  if ( ib < ia ) then
    ind = -1
    it = ib
    ib = ia
    ia = it
  else
    ind = 1
  end if
 
  s = 0.0_dp
  c = 0.0_dp
  r4 = 0.0_dp
  j = ntab-2
  if ( ia < ntab-1 .or. ntab == 4 ) then
    j = max ( 3, ia )
  end if

  k = 4
  if ( 2 < ib .or. ntab == 4 ) then
    k = min ( ntab, ib + 2 ) - 1
  end if
 
  do i = j, k
 
    if ( i <= j ) then
 
      h2 = xtab(j-1) - xtab(j-2)
      d3 = ( ftab(j-1) - ftab(j-2) ) / h2
      h3 = xtab(j) - xtab(j-1)
      d1 = ( ftab(j) - ftab(j-1) ) / h3
      h1 = h2 + h3
      d2 = ( d1 - d3 ) / h1
      h4 = xtab(j+1) - xtab(j)
      r1 = ( ftab(j+1) - ftab(j) ) / h4
      r2 = ( r1 - d1 ) / ( h4 + h3 )
      h1 = h1 + h4
      r3 = (r2-d2) / h1
 
      if ( ia <= 1 ) then
        result = h2 * ( ftab(1) + h2 * ( 0.5_dp * d3 - h2 &
          * ( d2 / 6.0_dp -(h2+h3+h3)*r3/12.0_dp)))
        s = -h2**3 * (h2*(3.0_dp*h2+5.0_dp*h4)+10.0_dp*h3*h1) / 60.0_dp
      end if
 
    else
 
      h4 = xtab(i+1) - xtab(i)
      r1 = ( ftab(i+1) - ftab(i) ) / h4
      r4 = h4 + h3
      r2 = ( r1 - d1 ) / r4
      r4 = r4 + h2
      r3 = ( r2 - d2 ) / r4
      r4 = ( r3 - d3 ) / ( r4 + h1 )
 
    end if
 
    if ( ia < i .and. i <= ib ) then
 
      term = h3 * ( ( ftab(i) + ftab(i-1) ) * 0.5_dp &
        -h3 * h3 * ( d2 + r2 + ( h2 - h4 ) * r3 ) / 12.0_dp )
      result = result + term
      c = h3**3 * ( 2.0_dp * h3 * h3 &
        + 5.0_dp * ( h3 * ( h4 + h2 ) + 2.0_dp * h2 * h4 ) ) / 120.0_dp
      error = error + (c+s)*r4
 
      if ( i /= j ) then
        s = c
      else
        s = s + c + c
      end if
 
    else
 
      error = error + r4 * s
 
    end if
 
    if ( k <= i ) then
 
      if ( ntab <= ib ) then
        term = h4 * ( ftab(ntab) - h4 * ( 0.5_dp * r1 &
          + h4 * ( r2 / 6.0_dp + ( h3 + h3 + h4 ) * r3 / 12.0_dp )))
        result = result + term
        error = error - h4**3 * r4 * &
          ( h4 * ( 3.0_dp * h4 + 5.0_dp * h2 ) &
          + 10.0_dp * h3 * ( h2 + h3 + h4 ) ) / 60.0_dp
      end if
 
      if ( ntab-1 <= ib ) then
        error = error + s * r4
      end if

    else

      h1 = h2
      h2 = h3
      h3 = h4
      d1 = r1
      d2 = r2
      d3 = r3
    end if
 
  end do
!
!  Restore original values of IA and IB, reverse signs
!  of RESULT and ERROR, to account for integration
!  that proceeded from high X to low X.
!
  if ( ind /= 1 ) then
    it = ib
    ib = ia
    ia = it
    result = -result
    error = -error
  end if
 
  return
end subroutine cubint_r8



subroutine cubint_r4 ( ntab, xtab, ftab, ia, ib, result, error )

!*****************************************************************************80
!
!! CUBINT approximates an integral using cubic interpolation of data.
!
!  Discussion:
!
!    The integral to be approximated is
! 
!      Integral ( XTAB(IB) <= X <= XTAB(IA) ) F(X) DX
!
!    The routine estimates the error in integration.
!
!  Modified:
!
!    10 February 2006
!
!  Reference:
!
!    Philip Davis, Philip Rabinowitz,
!    Methods of Numerical Integration,
!    Second Edition,
!    Dover, 2007,
!    ISBN: 0486453391,
!    LC: QA299.3.D28.
!
!    Philip Gill, GF Miller,
!    An algorithm for the integration of unequally spaced data,
!    The Computer Journal, 
!    Number 15, Number 1, 1972, pages 80-83.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NTAB, the number of tabulated points.
!    NTAB must be at least 4.
!
!    Input, real ( kind = 8 ) XTAB(NTAB), contains the points at which the
!    function was tabulated.  XTAB should contain distinct
!    values, given in ascending order.
!
!    Input, real ( kind = 8 ) FTAB(NTAB), contains the tabulated function
!    values, FTAB(I) = F(XTAB(I)).
!
!    Input, integer ( kind = 4 ) IA, the entry of XTAB at which integration
!    is to begin.  IA must be no less than 1 and no greater
!    than NTAB.
!
!    Input, integer ( kind = 4 ) IB, the entry of XTAB at which integration
!    is to end.  IB must be no less than 1 and no greater than
!    NTAB.
!
!    Output, real ( kind = 8 ) RESULT, the approximate value of the
!    integral from XTAB(IA) to XTAB(IB) of the function.
!
!    Output, real ( kind = 8 ) ERROR, an estimate of the error in
!    integration.
!
  implicit none

  integer ( kind = i4 ) ntab

  real ( kind = sp ) c
  real ( kind = sp ) d1
  real ( kind = sp ) d2
  real ( kind = sp ) d3
  real ( kind = sp ) error
  real ( kind = sp ) ftab(ntab)
  real ( kind = sp ) h1
  real ( kind = sp ) h2
  real ( kind = sp ) h3
  real ( kind = sp ) h4
  integer ( kind = i4 ) i
  integer ( kind = i4 ) ia
  integer ( kind = i4 ) ib
  integer ( kind = i4 ) ind
  integer ( kind = i4 ) it
  integer ( kind = i4 ) j
  integer ( kind = i4 ) k
  real ( kind = sp ) r1
  real ( kind = sp ) r2
  real ( kind = sp ) r3
  real ( kind = sp ) r4
  real ( kind = sp ) result
  real ( kind = sp ) s
  real ( kind = sp ) term
  real ( kind = sp ) xtab(ntab)

  result = 0.0_sp
  error = 0.0_sp
 
  if ( ia == ib ) then
    return
  end if
 
  if ( ntab < 4 ) then
    !write ( *, '(a)' ) ' '
    !write ( *, '(a)' ) 'CUBINT - Fatal error!'
    !write ( *, '(a,i8)' ) '  NTAB must be at least 4, but input NTAB = ', ntab
     !stop
    return
  end if
 
  if ( ia < 1 ) then
    !write ( *, '(a)' ) ' '
    !write ( *, '(a)' ) 'CUBINT - Fatal error!'
    !write ( *, '(a,i8)' ) '  IA must be at least 1, but input IA = ', ia
     !stop
    return
  end if
 
  if ( ntab < ia ) then
    !write ( *, '(a)' ) ' '
    !write ( *, '(a)' ) 'CUBINT - Fatal error!'
    !write ( *, '(a,i8)' ) '  IA must be <= NTAB, but input IA = ', ia
     !stop
    return
  end if
 
  if ( ib < 1 ) then
    !write ( *, '(a)' ) ' '
    !write ( *, '(a)' ) 'CUBINT - Fatal error!'
    !write ( *, '(a,i8)' ) '  IB must be at least 1, but input IB = ', ib
     !stop
    return
  end if
 
  if ( ntab < ib ) then
    !write ( *, '(a)' ) ' '
    !write ( *, '(a)' ) 'CUBINT - Fatal error!'
    !write ( *, '(a,i8)' ) '  IB must be <= NTAB, but input IB = ', ib
     !stop
    return
  end if
!
!  Temporarily switch IA and IB, and store minus sign in IND
!  so that, while integration is carried out from low X's
!  to high ones, the sense of the integral is preserved.
!
  if ( ib < ia ) then
    ind = -1
    it = ib
    ib = ia
    ia = it
  else
    ind = 1
  end if
 
  s = 0.0_sp
  c = 0.0_sp
  r4 = 0.0_sp
  j = ntab-2
  if ( ia < ntab-1 .or. ntab == 4 ) then
    j = max ( 3, ia )
  end if

  k = 4
  if ( 2 < ib .or. ntab == 4 ) then
    k = min ( ntab, ib + 2 ) - 1
  end if
 
  do i = j, k
 
    if ( i <= j ) then
 
      h2 = xtab(j-1) - xtab(j-2)
      d3 = ( ftab(j-1) - ftab(j-2) ) / h2
      h3 = xtab(j) - xtab(j-1)
      d1 = ( ftab(j) - ftab(j-1) ) / h3
      h1 = h2 + h3
      d2 = ( d1 - d3 ) / h1
      h4 = xtab(j+1) - xtab(j)
      r1 = ( ftab(j+1) - ftab(j) ) / h4
      r2 = ( r1 - d1 ) / ( h4 + h3 )
      h1 = h1 + h4
      r3 = (r2-d2) / h1
 
      if ( ia <= 1 ) then
        result = h2 * ( ftab(1) + h2 * ( 0.5_sp * d3 - h2 &
          * ( d2 / 6.0_sp -(h2+h3+h3)*r3/12.0_sp)))
        s = -h2**3 * (h2*(3.0_sp*h2+5.0_sp*h4)+10.0_sp*h3*h1) / 60.0_sp
      end if
 
    else
 
      h4 = xtab(i+1) - xtab(i)
      r1 = ( ftab(i+1) - ftab(i) ) / h4
      r4 = h4 + h3
      r2 = ( r1 - d1 ) / r4
      r4 = r4 + h2
      r3 = ( r2 - d2 ) / r4
      r4 = ( r3 - d3 ) / ( r4 + h1 )
 
    end if
 
    if ( ia < i .and. i <= ib ) then
 
      term = h3 * ( ( ftab(i) + ftab(i-1) ) * 0.5_sp &
        -h3 * h3 * ( d2 + r2 + ( h2 - h4 ) * r3 ) / 12.0_sp )
      result = result + term
      c = h3**3 * ( 2.0_sp * h3 * h3 &
        + 5.0_sp * ( h3 * ( h4 + h2 ) + 2.0_sp * h2 * h4 ) ) / 120.0_sp
      error = error + (c+s)*r4
 
      if ( i /= j ) then
        s = c
      else
        s = s + c + c
      end if
 
    else
 
      error = error + r4 * s
 
    end if
 
    if ( k <= i ) then
 
      if ( ntab <= ib ) then
        term = h4 * ( ftab(ntab) - h4 * ( 0.5_sp * r1 &
          + h4 * ( r2 / 6.0_sp + ( h3 + h3 + h4 ) * r3 / 12.0_sp )))
        result = result + term
        error = error - h4**3 * r4 * &
          ( h4 * ( 3.0_sp * h4 + 5.0_sp * h2 ) &
          + 10.0_sp * h3 * ( h2 + h3 + h4 ) ) / 60.0_sp
      end if
 
      if ( ntab-1 <= ib ) then
        error = error + s * r4
      end if

    else

      h1 = h2
      h2 = h3
      h3 = h4
      d1 = r1
      d2 = r2
      d3 = r3
    end if
 
  end do
!
!  Restore original values of IA and IB, reverse signs
!  of RESULT and ERROR, to account for integration
!  that proceeded from high X to low X.
!
  if ( ind /= 1 ) then
    it = ib
    ib = ia
    ia = it
    result = -result
    error = -error
  end if
 
  return
end subroutine cubint_r4






subroutine filon_cos_r8( ntab, ftab, a, b, t, result )

!*****************************************************************************80
!
!! FILON_COS uses Filon's method on integrals with a cosine factor.
!
!  Discussion:
!
!    The integral to be approximated has the form:
!
!      Integral ( A <= X <= B ) F(X) * COS(T*X) dX
!
!    where T is user specified.
!
!    The function is interpolated over each subinterval by
!    a parabolic arc.
!
!  Modified:
!
!    10 February 2006
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
!    Stephen Chase, Lloyd Fosdick,
!    An Algorithm for Filon Quadrature,
!    Communications of the Association for Computing Machinery,
!    Volume 12, Number 8, August 1969, pages 453-457.
!
!    Stephen Chase, Lloyd Fosdick,
!    Algorithm 353:
!    Filon Quadrature,
!    Communications of the Association for Computing Machinery,
!    Volume 12, Number 8, August 1969, pages 457-458.
!
!    Philip Davis, Philip Rabinowitz,
!    Methods of Numerical Integration,
!    Second Edition,
!    Dover, 2007,
!    ISBN: 0486453391,
!    LC: QA299.3.D28.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NTAB, the number of data points.
!    NTAB must be odd, and greater than 1.
!
!    Input, real ( kind = 8 ) FTAB(NTAB), contains the value of the function
!    at A, A+H, A+2*H, ... , B-H, B, where H = (B-A)/(NTAB-1).
!
!    Input, real ( kind = 8 ) A, B, the limits of integration.
!
!    Input, real ( kind = 8 ) T, the multiplier of the X argument of the cosine.
!
!    Output, real ( kind = 8 ) RESULT, the approximate value of the integral.
!
  implicit none

  integer ( kind = i4 ) ntab

  real ( kind = dp ) a
  real ( kind = dp ) alpha
  real ( kind = dp ) b
  real ( kind = dp ) beta
  real ( kind = dp ) c2n
  real ( kind = dp ) c2nm1
  real ( kind = dp ) cost
  real ( kind = dp ) ftab(ntab)
  real ( kind = dp ) gamma
  real ( kind = dp ) h
  real ( kind = dp ) result
  real ( kind = dp ) sint
  real ( kind = dp ) t
  real ( kind = dp ) theta
  real ( kind = dp ) xtab(ntab)

  if ( a == b ) then
    result = 0.0_dp
    return
  end if
 
  if ( ntab <= 1 ) then
    !write ( *, '(a)' ) ' '
    !write ( *, '(a)' ) 'FILON_COS - Fatal error!'
    !write ( *, '(a)' ) '  NTAB < 2'
    !write ( *, '(a,i8)' ) '  NTAB = ', ntab
     !stop
    return
  end if
 
  if ( mod ( ntab, 2 ) /= 1 ) then
    !write ( *, '(a)' ) ' '
    !write ( *, '(a)' ) 'FILON_COS - Fatal error!'
    !write ( *, '(a)' ) '  NTAB must be odd.'
    !write ( *, '(a,i8)' ) '  NTAB = ', ntab
     !stop
    return
  end if
!
!  Set up a vector of the NTAB X values.
! 
  call r8vec_even ( ntab, a, b, xtab )

  h = ( b - a ) / real ( ntab - 1, kind = 8 )

  theta = t * h
  sint = sin ( theta )
  cost = cos ( theta )

  if ( 6.0_dp * abs ( theta ) <= 1.0_dp ) then
#if (DIVIDION_REPLACEMENT) == 0
    alpha = 2.0_dp * theta**3 /   45.0_dp &
          - 2.0_dp * theta**5 /  315.0_dp &
          + 2.0_dp * theta**7 / 4725.0_dp
  
    beta =  2.0_dp            /     3.0_dp &
          + 2.0_dp * theta**2 /    15.0_dp &
          - 4.0_dp * theta**4 /   105.0_dp &
          + 2.0_dp * theta**6 /   567.0_dp &
          - 4.0_dp * theta**8 / 22275.0_dp

    gamma = 4.0_dp            /      3.0_dp &
          - 2.0_dp * theta**2 /     15.0_dp &
          +           theta**4 /    210.0_dp &
          -           theta**6 /  11340.0_dp
#else
     alpha = 2.0_dp * theta**3 *  0.022222222222222222222222222222222_dp &
           - 2.0_dp * theta**5 *  0.002857142857142857142857142857_dp &
           + 2.0_dp * theta**7 *  0.000211640211640211640211640212_dp
  
     beta =  2.0_dp            *   0.333333333333333333333333333333333_dp &
          + 2.0_dp * theta**2 *   0.066666666666666666666666666667_dp &
          - 4.0_dp * theta**4 *   0.009523809523809523809523809524_dp &
          + 2.0_dp * theta**6 *   0.00176366843033509700176366843_dp &
          - 4.0_dp * theta**8 *   0.000044893378226711560044893378_dp

    gamma = 4.0_dp             *    0.333333333333333333333333333333333333_dp &
          - 2.0_dp * theta**2  *    0.066666666666666666666666666667_dp &
          +           theta**4 *    0.004761904761904761904761904762_dp &
          -           theta**6 *    0.000088183421516754850088183422_dp
#endif
    
  else

    alpha = ( theta**2 + theta * sint * cost &
      - 2.0_dp * sint**2 ) / theta**3

    beta = ( 2.0_dp * theta + 2.0_dp * theta * cost**2 &
      - 4.0_dp * sint * cost ) / theta**3

    gamma = 4.0_dp * ( sint - theta * cost ) / theta**3
  
  end if

  c2n = sum ( ftab(1:ntab:2) * cos ( t * xtab(1:ntab:2) ) ) &
    - 0.5_dp * ( ftab(ntab) * cos ( t * xtab(ntab) ) &
                + ftab(1) * cos ( t * xtab(1) ) )

  c2nm1 = sum ( ftab(2:ntab-1:2) * cos ( t * xtab(2:ntab-1:2) ) )
 
  result = h * ( &
      alpha * ( ftab(ntab) * sin ( t * xtab(ntab) ) & 
              - ftab(1)    * sin ( t * xtab(1) ) ) &
    + beta * c2n &
    + gamma * c2nm1 )

  return
end subroutine filon_cos_r8


subroutine filon_cos_r4( ntab, ftab, a, b, t, result )

!*****************************************************************************80
!
!! FILON_COS uses Filon's method on integrals with a cosine factor.
!
!  Discussion:
!
!    The integral to be approximated has the form:
!
!      Integral ( A <= X <= B ) F(X) * COS(T*X) dX
!
!    where T is user specified.
!
!    The function is interpolated over each subinterval by
!    a parabolic arc.
!
!  Modified:
!
!    10 February 2006
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
!    Stephen Chase, Lloyd Fosdick,
!    An Algorithm for Filon Quadrature,
!    Communications of the Association for Computing Machinery,
!    Volume 12, Number 8, August 1969, pages 453-457.
!
!    Stephen Chase, Lloyd Fosdick,
!    Algorithm 353:
!    Filon Quadrature,
!    Communications of the Association for Computing Machinery,
!    Volume 12, Number 8, August 1969, pages 457-458.
!
!    Philip Davis, Philip Rabinowitz,
!    Methods of Numerical Integration,
!    Second Edition,
!    Dover, 2007,
!    ISBN: 0486453391,
!    LC: QA299.3.D28.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NTAB, the number of data points.
!    NTAB must be odd, and greater than 1.
!
!    Input, real ( kind = 8 ) FTAB(NTAB), contains the value of the function
!    at A, A+H, A+2*H, ... , B-H, B, where H = (B-A)/(NTAB-1).
!
!    Input, real ( kind = 8 ) A, B, the limits of integration.
!
!    Input, real ( kind = 8 ) T, the multiplier of the X argument of the cosine.
!
!    Output, real ( kind = 8 ) RESULT, the approximate value of the integral.
!
  implicit none

  integer ( kind = i4 ) ntab

  real ( kind = sp ) a
  real ( kind = sp ) alpha
  real ( kind = sp ) b
  real ( kind = sp ) beta
  real ( kind = sp ) c2n
  real ( kind = sp ) c2nm1
  real ( kind = sp ) cost
  real ( kind = sp ) ftab(ntab)
  real ( kind = sp ) gamma
  real ( kind = sp ) h
  real ( kind = sp ) result
  real ( kind = sp ) sint
  real ( kind = sp ) t
  real ( kind = sp ) theta
  real ( kind = sp ) xtab(ntab)

  if ( a == b ) then
    result = 0.0_sp
    return
  end if
 
  if ( ntab <= 1 ) then
    !write ( *, '(a)' ) ' '
    !write ( *, '(a)' ) 'FILON_COS - Fatal error!'
    !write ( *, '(a)' ) '  NTAB < 2'
    !write ( *, '(a,i8)' ) '  NTAB = ', ntab
     !stop
    return
  end if
 
  if ( mod ( ntab, 2 ) /= 1 ) then
    !write ( *, '(a)' ) ' '
    !write ( *, '(a)' ) 'FILON_COS - Fatal error!'
    !write ( *, '(a)' ) '  NTAB must be odd.'
    !write ( *, '(a,i8)' ) '  NTAB = ', ntab
     !stop
    return
  end if
!
!  Set up a vector of the NTAB X values.
! 
  call r4vec_even ( ntab, a, b, xtab )

  h = ( b - a ) / real ( ntab - 1, kind = sp )

  theta = t * h
  sint = sin ( theta )
  cost = cos ( theta )

  if ( 6.0_sp * abs ( theta ) <= 1.0_sp ) then
#if (DIVISION_REPLACEMENT) == 0
    alpha = 2.0_sp * theta**3 /   45.0_sp &
          - 2.0_sp * theta**5 /  315.0_sp &
          + 2.0_sp * theta**7 / 4725.0_sp
  
    beta =  2.0_sp            /     3.0_sp &
          + 2.0_sp * theta**2 /    15.0_sp &
          - 4.0_sp * theta**4 /   105.0_sp &
          + 2.0_sp * theta**6 /   567.0_sp &
          - 4.0_sp * theta**8 / 22275.0_sp

    gamma = 4.0_sp            /      3.0_sp &
          - 2.0_sp * theta**2 /     15.0_sp &
          +           theta**4 /    210.0_sp &
          -           theta**6 /  11340.0_sp
#else
     alpha = 2.0_sp * theta**3 *  0.022222222222222222222222222222222_sp &
           - 2.0_sp * theta**5 *  0.002857142857142857142857142857_sp &
           + 2.0_sp * theta**7 *  0.000211640211640211640211640212_sp
  
     beta =  2.0_sp            *   0.333333333333333333333333333333333_sp &
          + 2.0_sp * theta**2 *   0.066666666666666666666666666667_sp &
          - 4.0_sp * theta**4 *   0.009523809523809523809523809524_sp &
          + 2.0_sp * theta**6 *   0.00176366843033509700176366843_sp &
          - 4.0_sp * theta**8 *   0.000044893378226711560044893378_sp

    gamma = 4.0_sp             *    0.333333333333333333333333333333333333_sp &
          - 2.0_sp * theta**2  *    0.066666666666666666666666666667_sp &
          +           theta**4 *    0.004761904761904761904761904762_sp &
          -           theta**6 *    0.000088183421516754850088183422_sp
#endif

  else

    alpha = ( theta**2 + theta * sint * cost &
      - 2.0_sp * sint**2 ) / theta**3

    beta = ( 2.0_sp * theta + 2.0_sp * theta * cost**2 &
      - 4.0_sp * sint * cost ) / theta**3

    gamma = 4.0_sp * ( sint - theta * cost ) / theta**3
  
  end if

  c2n = sum ( ftab(1:ntab:2) * cos ( t * xtab(1:ntab:2) ) ) &
    - 0.5_sp * ( ftab(ntab) * cos ( t * xtab(ntab) ) &
                + ftab(1) * cos ( t * xtab(1) ) )

  c2nm1 = sum ( ftab(2:ntab-1:2) * cos ( t * xtab(2:ntab-1:2) ) )
 
  result = h * ( &
      alpha * ( ftab(ntab) * sin ( t * xtab(ntab) ) & 
              - ftab(1)    * sin ( t * xtab(1) ) ) &
    + beta * c2n &
    + gamma * c2nm1 )

  return
end subroutine filon_cos_r4

subroutine filon_sin_r8( ntab, ftab, a, b, t, result )

!*****************************************************************************80
!
!! FILON_SIN uses Filon's method on integrals with a sine factor.
!
!  Discussion:
!
!    The integral to be approximated has the form
!
!      Integral ( A <= X <= B ) F(X) * SIN(T*X) dX
!
!    where T is user specified.
!
!    The function is interpolated over each subinterval by
!    a parabolic arc.
!
!  Modified:
!
!    10 February 2006
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
!    Stephen Chase, Lloyd Fosdick,
!    An Algorithm for Filon Quadrature,
!    Communications of the Association for Computing Machinery,
!    Volume 12, Number 8, August 1969, pages 453-457.
!
!    Stephen Chase, Lloyd Fosdick,
!    Algorithm 353:
!    Filon Quadrature,
!    Communications of the Association for Computing Machinery,
!    Volume 12, Number 8, August 1969, pages 457-458.
!
!    Philip Davis, Philip Rabinowitz,
!    Methods of Numerical Integration,
!    Second Edition,
!    Dover, 2007,
!    ISBN: 0486453391,
!    LC: QA299.3.D28.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NTAB, the number of data points, 
!    including the endpoints.  NTAB must be odd, and greater than 1.
!
!    Input, real ( kind = 8 ) FTAB(NTAB), contains the value of the function
!    at A, A+H, A+2*H, ... , B-H, B, where H = (B-A)/(NTAB-1).
!
!    Input, real ( kind = 8 ) A, B, the limits of integration.
!
!    Input, real ( kind = 8 ) T, multiplier of the X argument of the sine.
!
!    Output, real ( kind = 8 ) RESULT, the approximate value of the integral.
!
  implicit none

  integer ( kind = i4 ) ntab

  real ( kind = dp ) a
  real ( kind = dp ) alpha
  real ( kind = dp ) b
  real ( kind = dp ) beta
  real ( kind = dp ) cost
  real ( kind = dp ) ftab(ntab)
  real ( kind = dp ) gamma
  real ( kind = dp ) h
  real ( kind = dp ) result
  real ( kind = dp ) s2n
  real ( kind = dp ) s2nm1
  real ( kind = dp ) sint
  real ( kind = dp ) t
  real ( kind = dp ) theta
  real ( kind = dp ) xtab(ntab)

  if ( a == b ) then
    result = 0.0_dp
    return
  end if
 
  if ( ntab <= 1 ) then
    !write ( *, '(a)' ) ' '
    !write ( *, '(a)' ) 'FILON_SIN - Fatal error!'
    !write ( *, '(a)' ) '  NTAB < 2'
    !write ( *, '(a,i8)' ) '  NTAB = ',ntab
     !stop
    return
  end if
 
  if ( mod ( ntab, 2 ) /= 1 ) then
    !write ( *, '(a)' ) ' '
    !write ( *, '(a)' ) 'FILON_SIN - Fatal error!'
    !write ( *, '(a)' ) '  NTAB must be odd.'
    !write ( *, '(a,i8)' ) '  NTAB = ',ntab
     !stop
    return
  end if
!
!  Set up a vector of the NTAB X values.
! 
  call r8vec_even ( ntab, a, b, xtab )

  h = ( b - a ) / real ( ntab - 1, kind = dp )
  theta = t * h

  sint = sin ( theta )
  cost = cos ( theta )

  if ( 6.0_dp * abs ( theta ) <= 1.0_dp ) then
#if (DIVISION_REPLACEMENT) == 0
    alpha = 2.0_dp * theta**3 /   45.0_dp &
          - 2.0_dp * theta**5 /  315.0_dp &
          + 2.0_dp * theta**7 / 4725.0_dp
  
    beta =  2.0_dp            /     3.0_dp &
          + 2.0_dp * theta**2 /    15.0_dp &
          - 4.0_dp * theta**4 /   105.0_dp &
          + 2.0_dp * theta**6 /   567.0_dp &
          - 4.0_dp * theta**8 / 22275.0_dp

    gamma = 4.0_dp             /      3.0_dp &
          - 2.0_dp * theta**2  /     15.0_dp &
          +           theta**4 /    210.0_dp &
          -           theta**6 /  11340.0_dp
#else
    alpha = 2.0_dp * theta**3  *  0.022222222222222222222222222222222_dp &
           - 2.0_dp * theta**5 *  0.002857142857142857142857142857_dp &
           + 2.0_dp * theta**7 *  0.000211640211640211640211640212_dp
  
     beta =  2.0_dp           *   0.333333333333333333333333333333333_dp &
          + 2.0_dp * theta**2 *   0.066666666666666666666666666667_dp &
          - 4.0_dp * theta**4 *   0.009523809523809523809523809524_dp &
          + 2.0_dp * theta**6 *   0.00176366843033509700176366843_dp &
          - 4.0_dp * theta**8 *   0.000044893378226711560044893378_dp

    gamma = 4.0_dp             *    0.333333333333333333333333333333333333_dp &
          - 2.0_dp  * theta**2 *   0.066666666666666666666666666667_dp &
          +           theta**4 *    0.004761904761904761904761904762_dp &
          -           theta**6 *    0.000088183421516754850088183422_dp
#endif

  else
 
    alpha = ( theta**2 + theta * sint * cost &
      - 2.0_dp * sint**2 ) / theta**3

    beta = ( 2.0_dp * theta + 2.0_dp * theta * cost**2 &
      - 4.0_dp * sint * cost ) / theta**3

    gamma = 4.0_dp * ( sint - theta * cost ) / theta**3
 
  end if
  
  s2n = sum ( ftab(1:ntab:2) * sin ( t * xtab(1:ntab:2) ) ) &
    - 0.5_dp * ( ftab(ntab) * sin ( t * xtab(ntab) ) &
                + ftab(1) * sin ( t * xtab(1) ) )

  s2nm1 = sum ( ftab(2:ntab-1:2) * sin ( t * xtab(2:ntab-1:2) ) )

  result = h * ( &
      alpha * ( ftab(1) * cos ( t * xtab(1) ) &
              - ftab(ntab) * cos ( t * xtab(ntab) ) ) &
    + beta * s2n &
    + gamma * s2nm1 )
 
  return
end subroutine filon_sin_r8


subroutine filon_sin_r4( ntab, ftab, a, b, t, result )

!*****************************************************************************80
!
!! FILON_SIN uses Filon's method on integrals with a sine factor.
!
!  Discussion:
!
!    The integral to be approximated has the form
!
!      Integral ( A <= X <= B ) F(X) * SIN(T*X) dX
!
!    where T is user specified.
!
!    The function is interpolated over each subinterval by
!    a parabolic arc.
!
!  Modified:
!
!    10 February 2006
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
!    Stephen Chase, Lloyd Fosdick,
!    An Algorithm for Filon Quadrature,
!    Communications of the Association for Computing Machinery,
!    Volume 12, Number 8, August 1969, pages 453-457.
!
!    Stephen Chase, Lloyd Fosdick,
!    Algorithm 353:
!    Filon Quadrature,
!    Communications of the Association for Computing Machinery,
!    Volume 12, Number 8, August 1969, pages 457-458.
!
!    Philip Davis, Philip Rabinowitz,
!    Methods of Numerical Integration,
!    Second Edition,
!    Dover, 2007,
!    ISBN: 0486453391,
!    LC: QA299.3.D28.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NTAB, the number of data points, 
!    including the endpoints.  NTAB must be odd, and greater than 1.
!
!    Input, real ( kind = 8 ) FTAB(NTAB), contains the value of the function
!    at A, A+H, A+2*H, ... , B-H, B, where H = (B-A)/(NTAB-1).
!
!    Input, real ( kind = 8 ) A, B, the limits of integration.
!
!    Input, real ( kind = 8 ) T, multiplier of the X argument of the sine.
!
!    Output, real ( kind = 8 ) RESULT, the approximate value of the integral.
!
  implicit none

  integer ( kind = i4 ) ntab

  real ( kind = sp ) a
  real ( kind = sp ) alpha
  real ( kind = sp ) b
  real ( kind = sp ) beta
  real ( kind = sp ) cost
  real ( kind = sp ) ftab(ntab)
  real ( kind = sp ) gamma
  real ( kind = sp ) h
  real ( kind = sp ) result
  real ( kind = sp ) s2n
  real ( kind = sp ) s2nm1
  real ( kind = sp ) sint
  real ( kind = sp ) t
  real ( kind = sp ) theta
  real ( kind = sp ) xtab(ntab)

  if ( a == b ) then
    result = 0.0_sp
    return
  end if
 
  if ( ntab <= 1 ) then
    !write ( *, '(a)' ) ' '
    !write ( *, '(a)' ) 'FILON_SIN - Fatal error!'
    !write ( *, '(a)' ) '  NTAB < 2'
    !write ( *, '(a,i8)' ) '  NTAB = ',ntab
     !stop
    return
  end if
 
  if ( mod ( ntab, 2 ) /= 1 ) then
    !write ( *, '(a)' ) ' '
    !write ( *, '(a)' ) 'FILON_SIN - Fatal error!'
    !write ( *, '(a)' ) '  NTAB must be odd.'
    !write ( *, '(a,i8)' ) '  NTAB = ',ntab
     !stop
    return
  end if
!
!  Set up a vector of the NTAB X values.
! 
  call r8vec_even ( ntab, a, b, xtab )

  h = ( b - a ) / real ( ntab - 1, kind = sp )
  theta = t * h

  sint = sin ( theta )
  cost = cos ( theta )

  if ( 6.0_sp * abs ( theta ) <= 1.0_sp ) then
#if (DIVISION_REPLACEMENT) == 0
    alpha = 2.0_sp * theta**3 /   45.0_sp &
          - 2.0_sp * theta**5 /  315.0_sp &
          + 2.0_sp * theta**7 / 4725.0_sp
  
    beta =  2.0_sp            /     3.0_sp &
          + 2.0_sp * theta**2 /    15.0_sp &
          - 4.0_sp * theta**4 /   105.0_sp &
          + 2.0_sp * theta**6 /   567.0_sp &
          - 4.0_sp * theta**8 / 22275.0_sp

    gamma = 4.0_sp             /      3.0_sp &
          - 2.0_sp * theta**2  /     15.0_sp &
          +           theta**4 /    210.0_sp &
          -           theta**6 /  11340.0_sp
#else
    alpha = 2.0_sp * theta**3  *  0.022222222222222222222222222222222_sp &
           - 2.0_sp * theta**5 *  0.002857142857142857142857142857_sp &
           + 2.0_sp * theta**7 *  0.000211640211640211640211640212_sp
  
     beta =  2.0_sp           *   0.333333333333333333333333333333333_sp &
          + 2.0_sp * theta**2 *   0.066666666666666666666666666667_sp &
          - 4.0_sp * theta**4 *   0.009523809523809523809523809524_sp &
          + 2.0_sp * theta**6 *   0.00176366843033509700176366843_sp &
          - 4.0_sp * theta**8 *   0.000044893378226711560044893378_sp

    gamma = 4.0_sp             *    0.333333333333333333333333333333333333_sp &
          - 2.0_sp  * theta**2 *   0.066666666666666666666666666667_sp &
          +           theta**4 *    0.004761904761904761904761904762_sp &
          -           theta**6 *    0.000088183421516754850088183422_sp
#endif

  else
 
    alpha = ( theta**2 + theta * sint * cost &
      - 2.0_sp * sint**2 ) / theta**3

    beta = ( 2.0_sp * theta + 2.0_sp * theta * cost**2 &
      - 4.0_sp * sint * cost ) / theta**3

    gamma = 4.0_sp * ( sint - theta * cost ) / theta**3
 
  end if
  
  s2n = sum ( ftab(1:ntab:2) * sin ( t * xtab(1:ntab:2) ) ) &
    - 0.5_sp * ( ftab(ntab) * sin ( t * xtab(ntab) ) &
                + ftab(1) * sin ( t * xtab(1) ) )

  s2nm1 = sum ( ftab(2:ntab-1:2) * sin ( t * xtab(2:ntab-1:2) ) )

  result = h * ( &
      alpha * ( ftab(1) * cos ( t * xtab(1) ) &
              - ftab(ntab) * cos ( t * xtab(ntab) ) ) &
    + beta * s2n &
    + gamma * s2nm1 )
 
  return
end subroutine filon_sin_r4




function gamma ( x )

!*****************************************************************************80
!
!! GAMMA calculates the Gamma function for a real argument X.
!
!  Definition:
!
!    GAMMA(X) = Integral ( 0 <= T <= Infinity ) T**(X-1) EXP(-T) DT
!
!  Recursion:
!
!    GAMMA(X+1) = X * GAMMA(X)
!
!  Special values:
!
!    GAMMA(0.5) = SQRT(PI)
!    If N is a positive integer, GAMMA(N+1) = N!, the standard factorial.
!
!  Discussion:
!
!    Computation is based on an algorithm outlined in reference 1.
!    The program uses rational functions that approximate the GAMMA
!    function to at least 20 significant decimal digits.  Coefficients
!    for the approximation over the interval (1,2) are unpublished.
!    Those for the approximation for X .GE. 12 are from reference 2.
!    The accuracy achieved depends on the arithmetic system, the
!    compiler, the intrinsic functions, and proper selection of the
!    machine-dependent constants.
!
!  Machine-dependent constants:
!
!    BETA: radix for the floating-point representation.
!    MAXEXP: the smallest positive power of BETA that overflows.
!    XBIG: the largest argument for which GAMMA(X) is representable
!      in the machine, i.e., the solution to the equation
!      GAMMA(XBIG) = BETA**MAXEXP.
!    XMININ: the smallest positive floating-point number such that
!      1/XMININ is machine representable.
!
!    Approximate values for some important machines are:
!
!                               BETA       MAXEXP        XBIG
!
!    CRAY-1         (S.P.)        2         8191        966.961
!    Cyber 180/855
!      under NOS    (S.P.)        2         1070        177.803
!    IEEE (IBM/XT,
!      SUN, etc.)   (S.P.)        2          128        35.040
!    IEEE (IBM/XT,
!      SUN, etc.)   (D.P.)        2         1024        171.624
!    IBM 3033       (D.P.)       16           63        57.574
!    VAX D-Format   (D.P.)        2          127        34.844
!    VAX G-Format   (D.P.)        2         1023        171.489
!
!                               XMININ
!
!    CRAY-1         (S.P.)   1.84D-2466
!    Cyber 180/855
!      under NOS    (S.P.)   3.14D-294
!    IEEE (IBM/XT,
!      SUN, etc.)   (S.P.)   1.18D-38
!    IEEE (IBM/XT,
!      SUN, etc.)   (D.P.)   2.23D-308
!    IBM 3033       (D.P.)   1.39D-76
!    VAX D-Format   (D.P.)   5.88D-39
!    VAX G-Format   (D.P.)   1.12D-308
!
!  Author:
!
!    William Cody and L. Stoltz,
!    Applied Mathematics Division,
!    Argonne National Laboratory,
!    Argonne, Illinois, 60439.
!
!  Reference:
!
!    William Cody,
!    An Overview of Software Development for Special Functions,
!    in Numerical Analysis Dundee, 1975,
!    edited by GA Watson,
!    Lecture Notes in Mathematics, 506,
!    Springer, 1976.
!
!    John Hart, Ward Cheney, Charles Lawson, Hans Maehly, Charles Mesztenyi, 
!    John Rice, Henry Thatcher, Christoph Witzgall,
!    Computer Approximations,
!    Wiley, 1968.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) GAMMA, the value of the function.  The program
!    returns the a huge value for singularities or when overflow would occur.
!    The computation is believed to be free of underflow and overflow.
!
  implicit none

  real ( kind = dp ), parameter, dimension ( 7 ) :: c = (/ &
    -1.910444077728-03_dp, &
     8.4171387781295-04_dp, &
    -5.952379913043012-04_dp, &
     7.93650793500350248-04_dp, &
    -2.777777777777681622553-03_dp, &
     8.333333333333333331554247-02_dp, &
     5.7083835261-03_dp /)
  real ( kind = dp ) fact
  real ( kind = dp ) gamma
  integer ( kind = i4 ) i
  integer ( kind = i4 ) n
  real ( kind = dp ), parameter, dimension ( 8 ) :: p = (/ &
    -1.71618513886549492533811_dp, &
     2.47656508055759199108314e+01_dp, &
    -3.79804256470945635097577e+02_dp, &
     6.29331155312818442661052e+02_dp, &
     8.66966202790413211295064e+02_dp, &
    -3.14512729688483675254357e+04_dp, &
    -3.61444134186911729807069e+04_dp, &
     6.64561438202405440627855e+04_dp /)
  logical parity
  real ( kind = dp ), parameter :: PI = &
    3.14159265358979323846264338327950288419716939937510_dp
  real ( kind = dp ), parameter, dimension ( 8 ) :: q = (/ &
    -3.08402300119738975254353e+01_dp, &
     3.15350626979604161529144e+02_dp, &
    -1.01515636749021914166146e+03_dp, &
    -3.10777167157231109440444e+03_dp, &
     2.25381184209801510330112e+04_dp, &
     4.75584627752788110767815e+03_dp, &
    -1.34659959864969306392456e+05_dp, &
    -1.15132259675553483497211e+05_dp /)
  real ( kind = dp ), parameter :: SQRTPI = 0.9189385332046727417803297_dp
  real ( kind = dp ) sum1
  real ( kind = dp ) x
  real ( kind = dp ), parameter :: XBIG = 35.040_dp
  real ( kind = dp ) xden
  real ( kind = dp ), parameter :: XMININ = 1.18-38_dp
  real ( kind = dp ) xnum
  real ( kind = dp ) y
  real ( kind = dp ) y1
  real ( kind = dp ) ysq
  real ( kind = dp ) z

  parity = .false.
  fact = 1.0_dp
  n = 0
  y = x
!
!  Argument is negative.
!
  if ( y <= 0.0_dp ) then

    y = - x
    y1 = aint ( y )
    gamma = y - y1

    if ( gamma /= 0.0_dp ) then

      if ( y1 /= aint ( y1 * 0.5_dp ) * 2.0_dp ) then
        parity = .true.
      end if

      fact = - PI / sin ( PI * gamma )
      y = y + 1.0_dp

    else

      gamma = huge ( gamma )
      return

    end if

  end if
!
!  Argument < EPS
!
  if ( y < epsilon ( y ) ) then

    if ( XMININ <= y ) then
      gamma = 1.0_dp / y
    else
      gamma = huge ( gamma )
      return
    end if

  else if ( y < 12.0_dp ) then

    y1 = y
!
!  0.0D+00 < argument < 1.0D+00
!
    if ( y < 1.0_dp ) then
      z = y
      y = y + 1.0_dp
!
!  1.0D+00 < argument < 12.0, reduce argument if necessary.
!
    else
      n = int ( y ) - 1
      y = y - real ( n, kind = dp )
      z = y - 1.0_dp
    end if
!
!  Evaluate approximation for 1.0D+00 < argument < 2.0.
!
    xnum = 0.0_dp
    xden = 1.0_dp
    do i = 1, 8
      xnum = ( xnum + p(i) ) * z
      xden = xden * z + q(i)
    end do

    gamma = xnum / xden + 1.0_dp
!
!  Adjust result for case  0.0D+00 < argument < 1.0.
!
    if ( y1 < y ) then
      gamma = gamma / y1
!
!  Adjust result for case  2.0D+00 < argument < 12.0.
!
    else if ( y < y1 ) then

      do i = 1, n
        gamma = gamma * y
        y = y + 1.0_dp
      end do

    end if
!
!  Evaluate for 12 <= argument.
!
  else

    if ( y <= XBIG ) then

      ysq = y**2
      sum1 = c(7)
      do i = 1, 6
        sum1 = sum1 / ysq + c(i)
      end do
      sum1 = sum1 / y - y + SQRTPI
      sum1 = sum1 + ( y - 0.5_dp ) * log ( y )
      gamma = exp ( sum1 )

    else

      gamma = huge ( gamma )
      return

    end if

  end if
!
!  Final adjustments and return.
!
  if ( parity ) then
    gamma = - gamma
  end if

  if ( fact /= 1.0_dp ) then
    gamma = fact / gamma
  end if

  return
end function gamma


 


subroutine gausq2 ( n, d, e, z, ierr )

!*****************************************************************************80
!
!! GAUSQ2 finds the eigenvalues of a symmetric tridiagonal matrix.
!
!  Discussion:
!
!    GAUSQ2 finds the eigenvalues and first components of the
!    eigenvectors of a symmetric tridiagonal matrix by the implicit QL
!    method.
!
!    GAUSQ2 is a translation of an ALGOL procedure as modified by
!    Dubrulle.
!
!    GAUSQ2 is a modified version of the EISPACK routine IMTQL2.
!
!  Modified:
!
!    30 October 2000
!
!  Reference:
!
!    Roger Martin, James Wilkinson,
!    The Implicit QL Algorithm,
!    Numerische Mathematik,
!    Volume 12, Number 5, December 1968, pages 377-383.
!
!    Augustin Dubrulle,
!    A short note on the implicit QL algorithm for symmetric
!    tridiagonal matrices,
!    Numerische Mathematik,
!    Volume 15, Number 5, September 1970, page 450.
!
!    James Wilkinson, Christian Reinsch,
!    Handbook for Automatic Computation,
!    Volume II, Linear Algebra, Part 2,
!    Springer, 1971,
!    ISBN: 0387054146.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, is the order of the matrix.
!
!    Input/output, real ( kind = 8 ) D(N).
!    On input, D contains the diagonal elements of the matrix.
!    On output, D contains the eigenvalues in ascending order.
!    If an error exit is made, the eigenvalues are correct but
!    unordered for indices 1, 2, ..., IERR-1;
!
!    Input/output, real ( kind = 8 ) E(N).
!    On input, E contains the subdiagonal elements of the input matrix
!    in its first N-1 positions.  E(N) is arbitrary.
!    On output, E has been destroyed.
!
!    Input/output, real ( kind = 8 ) Z(N).
!    On input, Z contains the first row of the identity matrix.
!    On output, Z contains the first components of the orthonormal
!    eigenvectors of the symmetric tridiagonal matrix.  If an error exit is
!    made, Z contains the eigenvectors associated with the stored
!    eigenvalues.
!
!    Output, integer ( kind = 4 ) IERR.
!    0, for normal return,
!    J, if the j-th eigenvalue has not been determined after 30 iterations.
!
  implicit none

  integer ( kind = i4 ) n

  real ( kind = dp ) b
  real ( kind = dp ) c
  real ( kind = dp ) d(n)
  real ( kind = dp ) e(n)
  real ( kind = dp ) epmach
  real ( kind = dp ) f
  real ( kind = dp ) g
  integer ( kind = i4 ) i
  integer ( kind = i4 ) ierr
  integer ( kind = i4 ) ii
  integer ( kind = i4 ) j
  integer ( kind = i4 ) k
  integer ( kind = i4 ) l
  integer ( kind = i4 ) m
  integer ( kind = i4 ) mml
  real ( kind = dp ) p
  real ( kind = dp ) r
  real ( kind = dp ) s
  real ( kind = dp ) temp
  real ( kind = dp ) z(n)

  epmach = epsilon ( epmach )
 
  ierr = 0

  if ( n == 1 ) then
    return
  end if
 
  e(n) = 0.0_dp
 
  do l = 1, n
 
    j = 0
!
!  Look for a small sub-diagonal element
!
    do m = l, n

      if ( m == n ) then
        exit
      end if

      if ( abs ( e(m) ) <= epmach * ( abs ( d(m) ) + abs ( d(m+1) ) ) ) then
        exit
      end if

    end do
 
10  continue
 
    p = d(l)

    if ( m == l ) then
      go to 20
    end if
 
    if ( j == 30 ) then
      ierr = l
      return
    end if
 
    j = j + 1
!
!  Form shift
!
    g = ( d(l+1) - p ) / ( 2.0_dp * e(l) )
    r = sqrt ( g * g + 1.0_dp )
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
        r = sqrt ( c * c + 1.0_dp )
        e(i+1) = f * r
        s = 1.0_dp / r
        c = c * s
 
      else
 
        s = f / g
        r = sqrt ( s * s + 1.0_dp )
        e(i+1) = g * r
        c = 1.0_dp / r
        s = s * c
 
      end if
 
      g = d(i+1) - p
      r = ( d(i) - g ) * s + 2.0_dp * c * b
      p = s * r
      d(i+1) = g + p
      g = c * r - b
!
!  Form the first component of the vector.
!
      f = z(i+1)
      z(i+1) = s * z(i) + c * f
      z(i) = f * z(i) - s * f
    end do
 
    d(l) = d(l) - p
    e(l) = g
    e(m) = 0.0_dp
    go to 10
 
20  continue
 
  end do
!
!  Order the eigenvalues and eigenvectors.
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

      temp = z(i)
      z(i) = z(k)
      z(k) = temp

    end if
 
  end do
 
  return
end subroutine gausq2

subroutine gaussq ( kind, norder, alpha, beta, kpts, endpts, b, xtab, &
  weight )

!*****************************************************************************80
!
!! GAUSSQ computes a Gauss quadrature rule.
!
!  Discussion:
!
!    GAUSSQ computes the nodes and weights for Gaussian-type quadrature
!    rules with pre-assigned nodes.
!
!    These are used when one wishes to approximate
!
!      Integral ( A <= X <= B )  F(X) W(X) DX
!
!    by
!
!      Sum ( 1 <= J <= NORDER ) WEIGHT(I) * F(XTAB(I))
!
!
!    GAUSSQ includes six integration rules that are applicable
!    to this problem, for particular weight functions and particular
!    intervals, including infinite and semi-infinite intervals.
!
!    Associated with each weight function W(X) is a set of
!    orthogonal polynomials.  The nodes XTAB are just the zeroes
!    of the proper NORDER-th degree polynomial.
!
!    GAUSSQ allows the user to modify the rule to require that
!    one or both of the endpoints of the interval are to be
!    included as quadrature nodes.
!
!  Modified:
!
!    30 October 2000
!
!  Reference:
!
!    Gene Golub, John Welsch,
!    Calculation of Gaussian Quadrature Rules,
!    Mathematics of Computation,
!    Volume 23, Number 106, April 1969, pages 221-230.
!
!    Gene Golub,
!    Some Modified Matrix Eigenvalue Problems,
!    SIAM Review,
!    Volume 15, Number 2, Part 1, April 1973, pages 318-334.
!
!    Arthur Stroud, Don Secrest,
!    Gaussian Quadrature Formulas,
!    Prentice-Hall, 1966,
!    LC: QA299.4G3S7.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) KIND, chooses the rule to be calculated.
!    1:  Legendre quadrature,
!         W(X) = 1
!         on (-1, 1)
!    2:  Chebyshev quadrature of the first kind
!         W(X) = 1/SQRT(1 - X*X)
!         on (-1, +1)
!    3:  Chebyshev quadrature of the second kind
!         W(X) = SQRT(1 - X*X)
!         on (-1, 1)
!    4:  Hermite quadrature,
!         W(X) = EXP(-X*X)
!         on (-infinity, +infinity)
!    5:  Jacobi quadrature,
!         W(X) = (1-X)**ALPHA * (1+X)**BETA
!         on (-1, 1),
!         -1 < ALPHA, -1 < BETA.
!         Note that KIND = 2 and 3 are a special case of this.
!    6:  Generalized Laguerre quadrature,
!         W(X) = EXP(-X)*X**ALPHA
!         on (0, +infinity),
!         -1 < ALPHA
!
!    Input, integer ( kind = 4 ) NORDER, the number of points used for the quadrature rule.
!
!    Input, real ( kind = 8 ) ALPHA, is only required for Gauss-Jacobi and
!    Gauss-Laguerre quadrature.  Its value is ignored in other cases.
!
!    Input, real ( kind = 8 ) BETA, is only required for Gauss-Jacobi
!    quadrature.  Its value is ignored in other cases.
!
!    Input, integer ( kind = 4 ) KPTS, is normally zero.
!    If KPTS is nonzero, it signals that one or both of the
!    endpoints of the interval is required to be a node.
!    This is called Gauss-Radau or Gauss-Lobatto quadrature.
!    Then KPTS is the number of endpoints that must be
!    included, either 1 or 2.
!
!    Input, real ( kind = 8 ) ENDPTS(2).
!    If KPTS is 1 or 2, ENDPTS contains the locations of the
!    endpoints to be fixed.
!
!    Workspace, real ( kind = 8 ) B(NORDER).
!
!    Output, real ( kind = 8 ) XTAB(NORDER), the nodes for the quadrature rule.
!
!    Output, real ( kind = 8 ) WEIGHT(NORDER), the weights for the 
!    quadrature rule.
!
  implicit none

  integer ( kind = i4 ) norder

  real ( kind = dp ) alpha
  real ( kind = dp ) b(norder)
  real ( kind = dp ) beta
  real ( kind = dp ) endpts(2)
  real ( kind = dp ) gam
  integer ( kind = i4 ) i
  integer ( kind = i4 ) ierr
  integer ( kind = i4 ) kind
  integer ( kind = i4 ) kpts
  real ( kind = dp ) muzero
  real ( kind = dp ) solve
  real ( kind = dp ) t1
  real ( kind = dp ) weight(norder)
  real ( kind = dp ) xtab(norder)
!
!  Get the diagonal coefficients XTAB(1:NORDER) and off-diagonal
!  coefficients B(1:NORDER-1) and MUZERO.
!
  call class_r8 ( kind, norder, alpha, beta, b, xtab, muzero )
!
!  The matrix of coefficients is assumed to be symmetric.
!  The array XTAB contains the diagonal elements, the array
!  B the off-diagonal elements.
!  Make appropriate changes in the lower right 2 by 2 submatrix.
!
!  If KPTS = 1, only XTAB(NORDER) must be changed.
!
  if ( kpts == 1 ) then
 
    xtab(norder) = endpts(1) &
      + solve ( endpts(1), norder, xtab, b ) * b(norder-1)**2
!
!  If KPTS = 2, XTAB(NORDER) and B(NORDER-1) must be recomputed.
!
  else if ( kpts == 2 ) then
 
    gam = solve ( endpts(1), norder, xtab, b )
    t1 = ( ( endpts(1) - endpts(2) ) &
      / ( solve ( endpts(2), norder, xtab, b ) - gam ) )
    b(norder-1) = sqrt ( t1 )
    xtab(norder) = endpts(1) + gam * t1
 
  end if
!
!  The indices of the elements of B run from 1 to NORDER-1.
!  The value of B(NORDER) is of no importance.
!
!  Now compute the eigenvalues of the symmetric tridiagonal
!  matrix, which has been modified as necessary.
!
!  The method used is a QL-type method with origin shifting.
!
  weight(1) = 1.0_dp
  weight(2:norder) = 0.0_dp
 
  call gausq2 ( norder, xtab, b, weight, ierr )
 
  do i = 1, norder
    weight(i) = muzero * weight(i)**2
  end do
 
  return
end subroutine gaussq



subroutine hiordq_r8( ntab, delt, y, work, result )

!*****************************************************************************80
!
!! HIORDQ approximates the integral of a function using equally spaced data.
!
!  Discussion:
!
!    The method applies the trapezoidal rule to various subsets of the
!    data, and then applies Richardson extrapolation.
!
!  Modified:
!
!    10 February 2006
!
!  Author:
!
!    Alan Kaylor Cline,
!    Department of Computer Science,
!    University of Texas at Austin.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NTAB, number of data points.
!
!    Input, real ( kind = 8 ) DELT, the spacing between the X values of the
!    data.  The actual X values are not needed!
!
!    Input, real ( kind = 8 ) Y(NTAB), the Y values of the data.
!
!    Work array, real ( kind = 8 ) WORK(2*(NTAB-1)).  The actual minimum amount
!    of workspace required is two times the number of integer
!    divisors of NTAB-1.
!
!    Output, real ( kind = 8 ) RESULT, the approximation to the integral.
!
  implicit none

  integer ( kind = i4 ) ntab

  real ( kind = dp ) delt
  real ( kind = dp ) fac
  integer ( kind = i4 ) i
  integer ( kind = i4 ) j
  integer ( kind = i4 ) jbak
  integer ( kind = i4 ) jj
  integer ( kind = i4 ) k
  real ( kind = dp ) result
  real ( kind = dp ) sum2
  real ( kind = dp ) sum1
  real ( kind = dp ) work(2*(ntab-1))
  real ( kind = dp ) y(ntab)
!
!  Determine initial trapezoidal rule
!
  sum1 = ( y(1) + y(ntab) ) * 0.5_dp
  j = -1
 
  do k = 1, ntab-1
!
!  Check if K divides NTAB-1
!
    if ( ( ( ntab - 1 ) / k ) * k == ntab - 1 ) then
!
!  Determine the K-point trapezoidal rule.
!
      sum2 = -sum1
      do i = 1, ntab, (ntab-1)/k
        sum2 = sum2 + y(i)
      end do
 
      j = j + 2
      work(j) = delt * sum2 * real ( ( ntab - 1 ) / k, kind = dp )
      work(j+1) = real ( ( ( ntab - 1 ) / k )**2, kind = dp )
!
!  Apply Richardson extrapolation.
!
      if ( k /= 1 ) then
 
        do jj = 3, j, 2
          jbak = j+1-jj
          fac = work(j+1) / ( work(j+1) - work(jbak+1) )
          work(jbak) = work(jbak+2) + fac * ( work(jbak) - work(jbak+2) )
        end do
 
      end if
 
    end if
 
  end do
 
  result = work(1)
 
  return
end subroutine hiordq_r8



subroutine hiordq_r4( ntab, delt, y, work, result )

!*****************************************************************************80
!
!! HIORDQ approximates the integral of a function using equally spaced data.
!
!  Discussion:
!
!    The method applies the trapezoidal rule to various subsets of the
!    data, and then applies Richardson extrapolation.
!
!  Modified:
!
!    10 February 2006
!
!  Author:
!
!    Alan Kaylor Cline,
!    Department of Computer Science,
!    University of Texas at Austin.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NTAB, number of data points.
!
!    Input, real ( kind = 8 ) DELT, the spacing between the X values of the
!    data.  The actual X values are not needed!
!
!    Input, real ( kind = 8 ) Y(NTAB), the Y values of the data.
!
!    Work array, real ( kind = 8 ) WORK(2*(NTAB-1)).  The actual minimum amount
!    of workspace required is two times the number of integer
!    divisors of NTAB-1.
!
!    Output, real ( kind = 8 ) RESULT, the approximation to the integral.
!
  implicit none

  integer ( kind = i4 ) ntab

  real ( kind = sp ) delt
  real ( kind = sp ) fac
  integer ( kind = i4 ) i
  integer ( kind = i4 ) j
  integer ( kind = i4 ) jbak
  integer ( kind = i4 ) jj
  integer ( kind = i4 ) k
  real ( kind = sp ) result
  real ( kind = sp ) sum2
  real ( kind = sp ) sum1
  real ( kind = sp ) work(2*(ntab-1))
  real ( kind = sp ) y(ntab)
!
!  Determine initial trapezoidal rule
!
  sum1 = ( y(1) + y(ntab) ) * 0.5_sp
  j = -1
 
  do k = 1, ntab-1
!
!  Check if K divides NTAB-1
!
    if ( ( ( ntab - 1 ) / k ) * k == ntab - 1 ) then
!
!  Determine the K-point trapezoidal rule.
!
      sum2 = -sum1
      do i = 1, ntab, (ntab-1)/k
        sum2 = sum2 + y(i)
      end do
 
      j = j + 2
      work(j) = delt * sum2 * real ( ( ntab - 1 ) / k, kind = dp )
      work(j+1) = real ( ( ( ntab - 1 ) / k )**2, kind = dp )
!
!  Apply Richardson extrapolation.
!
      if ( k /= 1 ) then
 
        do jj = 3, j, 2
          jbak = j+1-jj
          fac = work(j+1) / ( work(j+1) - work(jbak+1) )
          work(jbak) = work(jbak+2) + fac * ( work(jbak) - work(jbak+2) )
        end do
 
      end if
 
    end if
 
  end do
 
  result = work(1)
 
  return
end subroutine hiordq_r4



 subroutine plint_r8( ntab, xtab, ftab, a, b, result )

!*****************************************************************************80
!
!! PLINT approximates the integral of unequally spaced data.
!
!  Discussion:
!
!    The method uses piecewise linear interpolation.
!
!  Modified:
!
!    10 February 2006
!
!  Reference:
!
!    Philip Davis, Philip Rabinowitz,
!    Methods of Numerical Integration,
!    Second Edition,
!    Dover, 2007,
!    ISBN: 0486453391,
!    LC: QA299.3.D28.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NTAB, the number of entries in FTAB and
!    XTAB.  NTAB must be at least 2.
!
!    Input, real ( kind = 8 ) XTAB(NTAB), the abscissas at which the
!    function values are given.  The XTAB's must be distinct
!    and in ascending order.
!
!    Input, real ( kind = 8 ) FTAB(NTAB), the function values, 
!    FTAB(I) = F(XTAB(I)).
!
!    Input, real ( kind = 8 ) A, the lower limit of integration.  A should
!    be, but need not be, near one endpoint of the interval
!    (X(1), X(NTAB)).
!
!    Input, real ( kind = 8 ) B, the upper limit of integration.  B should
!    be, but need not be, near one endpoint of the interval
!    (X(1), X(NTAB)).
!
!    Output, real ( kind = 8 ) RESULT, the approximate value of the integral.
!
  implicit none

  integer ( kind = i4 ) ntab

  real ( kind = dp ) a
  real ( kind = dp ) b
  real ( kind = dp ) fa
  real ( kind = dp ) fb
  real ( kind = dp ) ftab(ntab)
  integer ( kind = i4 ) i
  integer ( kind = i4 ) ihi
  integer ( kind = i4 ) ilo
  integer ( kind = i4 ) ind
  real ( kind = dp ) result
  real ( kind = dp ) slope
  real ( kind = dp ) syl
  real ( kind = dp ) xtab(ntab)

  result = 0.0_dp
!
!  Check the parameters:
!
  if ( ntab < 2 ) then
    !write ( *, '(a)' ) ' '
    !write ( *, '(a)' ) 'PLINT - Fatal error!'
    !write ( *, '(a,i8)' ) '  NTAB < 2, NTAB = ', ntab
    !stop
    return
  end if
 
  do i = 2, ntab
    if ( xtab(i) <= xtab(i-1) ) then
     ! write ( *, '(a)' ) ' '
      !write ( *, '(a)' ) 'PLINT - Fatal error!'
      !write ( *, '(a)' ) '  Nodes not in strict increasing order.'
      !write ( *, '(a,i8)' ) '  XTAB(I) <= XTAB(I-1) for I = ', i
     ! write ( *, '(a,g14.6)' ) '  XTAB(I)   = ', xtab(i)
     ! write ( *, '(a,g14.6)' ) '  XTAB(I-1) = ', xtab(i-1)
      !stop
      return
    end if
  end do

  if ( a == b ) then
    return
  end if
!
!  If B < A, temporarily switch A and B, and store sign.
!
  if ( b < a ) then
    syl = b
    b = a
    a = syl
    ind = -1
  else
    syl = a
    ind = 1
  end if
!
!  Find ILO and IHI so that A <= XTAB(ILO) <= XTAB(IHI) <= B
!  with the possible exception that A and B may be in the same
!  interval, or completely to the right or left of the XTAB's.
!
  ilo = ntab + 1

  do i = 1, ntab
    if ( a <= xtab(i) ) then
      ilo = i
      exit
    end if
  end do

  ihi = 0

  do i = ntab, 1, -1
    if ( xtab(i) <= b ) then
      ihi = i
      exit
    end if
  end do
!
!  Treat special cases where A, B lie both to left or both to right
!  of XTAB interval, or in between same pair of XTAB's.
!
  if ( ihi == 0 ) then

    slope = ( ftab(2) - ftab(1) ) / ( xtab(2) - xtab(1) )
    fa = ftab(1) + slope * ( a - xtab(1) )
    fb = ftab(1) + slope * ( b - xtab(1) )
    result = 0.5_dp * ( b - a ) * ( fa + fb )

  else if ( ilo == ntab + 1 ) then

    slope = ( ftab(ntab) - ftab(ntab-1) ) / ( xtab(ntab) - xtab(ntab-1) )
    fa = ftab(ntab-1) + slope * ( a - xtab(ntab-1) )
    fb = ftab(ntab-1) + slope * ( b - xtab(ntab-1) )
    result = 0.5_dp * ( b - a ) * ( fa + fb )

  else if ( ihi + 1 == ilo ) then

    slope = ( ftab(ilo) - ftab(ihi) ) / ( xtab(ilo) - xtab(ihi) )
    fa = ftab(ihi) + slope * ( a - xtab(ihi) )
    fb = ftab(ihi) + slope * ( b - xtab(ihi) )
    result = 0.5_dp * ( b - a ) * ( fa + fb )

  else
!
!  Carry out approximate integration.  We know that ILO is no greater
!  than IHI-1, but equality is possible; A and B may be on either side
!  of a single XTAB(I).  That's OK, then the loop below won't be executed
!  at all.
!
    result = 0.0_dp
    do i = ilo, ihi-1
      result = result + 0.5_dp * ( xtab(i+1) - xtab(i) ) &
        * ( ftab(i) + ftab(i+1) )
    end do
!
!  Add contribution from A-ILO and IHI-B.
!  Still have to watch out if ILO = 1 or IHI=NTAB...
!
    if ( ilo == 1 ) then
      slope = ( ftab(2) - ftab(1) ) / ( xtab(2) - xtab(1) )
      fa = ftab(1) + slope * ( a - xtab(1) )
      result = result + 0.5_dp * ( xtab(ilo) - a ) * ( fa + ftab(ilo) )
    else
      slope = ( ftab(ilo) - ftab(ilo-1) ) / ( xtab(ilo) - xtab(ilo-1) )
      fa = ftab(ilo-1) + slope * ( a - xtab(ilo-1) )
      result = result + 0.5_dp * ( xtab(ilo) - a ) * ( fa + ftab(ilo) )
    end if
 
    if ( ihi == ntab ) then
      slope = ( ftab(ntab) - ftab(ntab-1) ) / ( xtab(ntab) - xtab(ntab-1) )
      fb = ftab(ntab-1) + slope * ( b - xtab(ntab-1) )
      result = result + 0.5_dp * ( b - xtab(ntab) ) * ( fb + ftab(ntab) )
    else
      slope = ( ftab(ihi+1) - ftab(ihi) ) / ( xtab(ihi+1) - xtab(ihi) )
      fb = ftab(ihi) + slope * ( b - xtab(ihi) )
      result = result + 0.5_dp * ( b - xtab(ihi) ) * ( fb + ftab(ihi) )
    end if

  end if
!
!  Restore original values of A and B, reverse sign of integral
!  because of earlier switch.
! 
  if ( ind /= 1 ) then
    ind = 1
    syl = b
    b = a
    a = syl
    result = -result
  end if
 
  return
end subroutine plint_r8


subroutine plint_r4( ntab, xtab, ftab, a, b, result )

!*****************************************************************************80
!
!! PLINT approximates the integral of unequally spaced data.
!
!  Discussion:
!
!    The method uses piecewise linear interpolation.
!
!  Modified:
!
!    10 February 2006
!
!  Reference:
!
!    Philip Davis, Philip Rabinowitz,
!    Methods of Numerical Integration,
!    Second Edition,
!    Dover, 2007,
!    ISBN: 0486453391,
!    LC: QA299.3.D28.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NTAB, the number of entries in FTAB and
!    XTAB.  NTAB must be at least 2.
!
!    Input, real ( kind = 8 ) XTAB(NTAB), the abscissas at which the
!    function values are given.  The XTAB's must be distinct
!    and in ascending order.
!
!    Input, real ( kind = 8 ) FTAB(NTAB), the function values, 
!    FTAB(I) = F(XTAB(I)).
!
!    Input, real ( kind = 8 ) A, the lower limit of integration.  A should
!    be, but need not be, near one endpoint of the interval
!    (X(1), X(NTAB)).
!
!    Input, real ( kind = 8 ) B, the upper limit of integration.  B should
!    be, but need not be, near one endpoint of the interval
!    (X(1), X(NTAB)).
!
!    Output, real ( kind = 8 ) RESULT, the approximate value of the integral.
!
  implicit none

  integer ( kind = i4 ) ntab

  real ( kind = sp ) a
  real ( kind = sp ) b
  real ( kind = sp ) fa
  real ( kind = sp ) fb
  real ( kind = sp ) ftab(ntab)
  integer ( kind = i4 ) i
  integer ( kind = i4 ) ihi
  integer ( kind = i4 ) ilo
  integer ( kind = i4 ) ind
  real ( kind = sp ) result
  real ( kind = sp ) slope
  real ( kind = sp ) syl
  real ( kind = sp ) xtab(ntab)

  result = 0.0_sp
!
!  Check the parameters:
!
  if ( ntab < 2 ) then
    !write ( *, '(a)' ) ' '
    !write ( *, '(a)' ) 'PLINT - Fatal error!'
    !write ( *, '(a,i8)' ) '  NTAB < 2, NTAB = ', ntab
    !stop
    return
  end if
 
  do i = 2, ntab
    if ( xtab(i) <= xtab(i-1) ) then
     ! write ( *, '(a)' ) ' '
      !write ( *, '(a)' ) 'PLINT - Fatal error!'
      !write ( *, '(a)' ) '  Nodes not in strict increasing order.'
      !write ( *, '(a,i8)' ) '  XTAB(I) <= XTAB(I-1) for I = ', i
     ! write ( *, '(a,g14.6)' ) '  XTAB(I)   = ', xtab(i)
     ! write ( *, '(a,g14.6)' ) '  XTAB(I-1) = ', xtab(i-1)
      !stop
      return
    end if
  end do

  if ( a == b ) then
    return
  end if
!
!  If B < A, temporarily switch A and B, and store sign.
!
  if ( b < a ) then
    syl = b
    b = a
    a = syl
    ind = -1
  else
    syl = a
    ind = 1
  end if
!
!  Find ILO and IHI so that A <= XTAB(ILO) <= XTAB(IHI) <= B
!  with the possible exception that A and B may be in the same
!  interval, or completely to the right or left of the XTAB's.
!
  ilo = ntab + 1

  do i = 1, ntab
    if ( a <= xtab(i) ) then
      ilo = i
      exit
    end if
  end do

  ihi = 0

  do i = ntab, 1, -1
    if ( xtab(i) <= b ) then
      ihi = i
      exit
    end if
  end do
!
!  Treat special cases where A, B lie both to left or both to right
!  of XTAB interval, or in between same pair of XTAB's.
!
  if ( ihi == 0 ) then

    slope = ( ftab(2) - ftab(1) ) / ( xtab(2) - xtab(1) )
    fa = ftab(1) + slope * ( a - xtab(1) )
    fb = ftab(1) + slope * ( b - xtab(1) )
    result = 0.5_dp * ( b - a ) * ( fa + fb )

  else if ( ilo == ntab + 1 ) then

    slope = ( ftab(ntab) - ftab(ntab-1) ) / ( xtab(ntab) - xtab(ntab-1) )
    fa = ftab(ntab-1) + slope * ( a - xtab(ntab-1) )
    fb = ftab(ntab-1) + slope * ( b - xtab(ntab-1) )
    result = 0.5_sp * ( b - a ) * ( fa + fb )

  else if ( ihi + 1 == ilo ) then

    slope = ( ftab(ilo) - ftab(ihi) ) / ( xtab(ilo) - xtab(ihi) )
    fa = ftab(ihi) + slope * ( a - xtab(ihi) )
    fb = ftab(ihi) + slope * ( b - xtab(ihi) )
    result = 0.5_sp * ( b - a ) * ( fa + fb )

  else
!
!  Carry out approximate integration.  We know that ILO is no greater
!  than IHI-1, but equality is possible; A and B may be on either side
!  of a single XTAB(I).  That's OK, then the loop below won't be executed
!  at all.
!
    result = 0.0_sp
    do i = ilo, ihi-1
      result = result + 0.5_sp * ( xtab(i+1) - xtab(i) ) &
        * ( ftab(i) + ftab(i+1) )
    end do
!
!  Add contribution from A-ILO and IHI-B.
!  Still have to watch out if ILO = 1 or IHI=NTAB...
!
    if ( ilo == 1 ) then
      slope = ( ftab(2) - ftab(1) ) / ( xtab(2) - xtab(1) )
      fa = ftab(1) + slope * ( a - xtab(1) )
      result = result + 0.5_sp * ( xtab(ilo) - a ) * ( fa + ftab(ilo) )
    else
      slope = ( ftab(ilo) - ftab(ilo-1) ) / ( xtab(ilo) - xtab(ilo-1) )
      fa = ftab(ilo-1) + slope * ( a - xtab(ilo-1) )
      result = result + 0.5_sp * ( xtab(ilo) - a ) * ( fa + ftab(ilo) )
    end if
 
    if ( ihi == ntab ) then
      slope = ( ftab(ntab) - ftab(ntab-1) ) / ( xtab(ntab) - xtab(ntab-1) )
      fb = ftab(ntab-1) + slope * ( b - xtab(ntab-1) )
      result = result + 0.5_sp * ( b - xtab(ntab) ) * ( fb + ftab(ntab) )
    else
      slope = ( ftab(ihi+1) - ftab(ihi) ) / ( xtab(ihi+1) - xtab(ihi) )
      fb = ftab(ihi) + slope * ( b - xtab(ihi) )
      result = result + 0.5_sp * ( b - xtab(ihi) ) * ( fb + ftab(ihi) )
    end if

  end if
!
!  Restore original values of A and B, reverse sign of integral
!  because of earlier switch.
! 
  if ( ind /= 1 ) then
    ind = 1
    syl = b
    b = a
    a = syl
    result = -result
  end if
 
  return
end subroutine plint_r4




subroutine r8vec_even ( n, alo, ahi, a )

!*****************************************************************************80
!
!! R8VEC_EVEN returns N values, evenly spaced between ALO and AHI.
!
!  Modified:
!
!    17 February 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of values.
!
!    Input, real ( kind = 8 ) ALO, AHI, the low and high values.
!
!    Output, real ( kind = 8 ) A(N), N evenly spaced values.
!    Normally, A(1) = ALO and A(N) = AHI.
!    However, if N = 1, then A(1) = 0.5*(ALO+AHI).
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) ahi
  real ( kind = 8 ) alo
  integer ( kind = 4 ) i

  if ( n == 1 ) then

    a(1) = 0.5D+00 * ( alo + ahi )

  else

    do i = 1, n
      a(i) = ( real ( n - i,     kind = 8 ) * alo   &
             + real (     i - 1, kind = 8 ) * ahi ) &
             / real ( n     - 1, kind = 8 )
    end do

  end if

  return
end subroutine r8vec_even


   
 subroutine r4vec_even ( n, alo, ahi, a )

!*****************************************************************************80
!
!! R8VEC_EVEN returns N values, evenly spaced between ALO and AHI.
!
!  Modified:
!
!    17 February 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of values.
!
!    Input, real ( kind = 8 ) ALO, AHI, the low and high values.
!
!    Output, real ( kind = 8 ) A(N), N evenly spaced values.
!    Normally, A(1) = ALO and A(N) = AHI.
!    However, if N = 1, then A(1) = 0.5*(ALO+AHI).
!
  implicit none

  integer ( kind = i4 ) n

  real ( kind = dp ) a(n)
  real ( kind = dp ) ahi
  real ( kind = dp ) alo
  integer ( kind = i4 ) i

  if ( n == 1 ) then

    a(1) = 0.5_dp * ( alo + ahi )

  else

    do i = 1, n
      a(i) = ( real ( n - i,     kind = dp ) * alo   &
             + real (     i - 1, kind = dp ) * ahi ) &
             / real ( n     - 1, kind = dp )
    end do

  end if

  return
end subroutine r4vec_even


 
  


subroutine simpne_r4( ntab, x, y, result )

!*****************************************************************************80
!
!! SIMPNE approximates the integral of unevenly spaced data.
!
!  Discussion:
!
!    The routine repeatedly interpolates a 3-point Lagrangian polynomial 
!    to the data and integrates that exactly.
!
!  Modified:
!
!    10 February 2006
!
!  Reference:
!
!    Philip Davis, Philip Rabinowitz,
!    Methods of Numerical Integration,
!    Second Edition,
!    Dover, 2007,
!    ISBN: 0486453391,
!    LC: QA299.3.D28.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NTAB, number of data points.  NTAB must be at least 3.
!
!    Input, real ( kind = 8 ) X(NTAB), contains the X values of the data,
!    in order.
!
!    Input, real ( kind = 8 ) Y(NTAB), contains the Y values of the data.
!
!    Output, real ( kind = 8 ) RESULT.
!    RESULT is the approximate value of the integral.
!
  implicit none

  integer ( kind = i4 ) ntab

  real ( kind = sp ) del(3)
  real ( kind = sp ) e
  real ( kind = sp ) f
  real ( kind = sp ) feints
  real ( kind = sp ) g(3)
  integer ( kind = i4 ) i
  integer ( kind = i4 ) n
  real ( kind = sp ) pi(3)
  real ( kind = sp ) result
  real ( kind = sp ) sum1
  real ( kind = sp ) x(ntab)
  real ( kind = sp ) x1
  real ( kind = sp ) x2
  real ( kind = sp ) x3
  real ( kind = sp ) y(ntab)

  result = 0.0_sp
 
  if ( ntab <= 2 ) then
    !write ( *, '(a)' ) ' '
    !write ( *, '(a)' ) 'SIMPNE - Fatal error!'
    !write ( *, '(a)' ) '  NTAB <= 2.'
    !stop
    return
  end if
 
  n = 1
 
  do
 
    x1 = x(n)
    x2 = x(n+1)
    x3 = x(n+2)
    e = x3 * x3- x1 * x1
    f = x3 * x3 * x3 - x1 * x1 * x1
    feints = x3 - x1

    del(1) = x3 - x2
    del(2) = x1 - x3
    del(3) = x2 - x1

    g(1) = x2 + x3
    g(2) = x1 + x3
    g(3) = x1 + x2

    pi(1) = x2 * x3
    pi(2) = x1 * x3
    pi(3) = x1 * x2
 
    sum1 = 0.0_sp
    do i = 1, 3
      sum1 = sum1 + y(n-1+i) * del(i) &
        * ( f / 3.0_sp - g(i) * 0.5_sp * e + pi(i) * feints )
    end do
    result = result - sum1 / ( del(1) * del(2) * del(3) )
 
    n = n + 2

    if ( ntab <= n + 1 ) then
      exit
    end if

  end do
 
  if ( mod ( ntab, 2 ) /= 0 ) then
    return
  end if

  n = ntab - 2
  x3 = x(ntab)
  x2 = x(ntab-1)
  x1 = x(ntab-2)
  e = x3 * x3 - x2 * x2
  f = x3 * x3 * x3 - x2 * x2 * x2
  feints = x3 - x2

  del(1) = x3 - x2
  del(2) = x1 - x3
  del(3) = x2 - x1

  g(1) = x2 + x3
  g(2) = x1 + x3
  g(3) = x1 + x2

  pi(1) = x2 * x3
  pi(2) = x1 * x3
  pi(3) = x1 * x2
 
  sum1 = 0.0_sp
  do i = 1, 3
    sum1 = sum1 + y(n-1+i) * del(i) * &
      ( f / 3.0_sp - g(i) * 0.5_sp * e + pi(i) * feints )
  end do
 
  result = result - sum1 / ( del(1) * del(2) * del(3) )
 
  return
end subroutine simpne_r4


subroutine simpne_r8( ntab, x, y, result )

!*****************************************************************************80
!
!! SIMPNE approximates the integral of unevenly spaced data.
!
!  Discussion:
!
!    The routine repeatedly interpolates a 3-point Lagrangian polynomial 
!    to the data and integrates that exactly.
!
!  Modified:
!
!    10 February 2006
!
!  Reference:
!
!    Philip Davis, Philip Rabinowitz,
!    Methods of Numerical Integration,
!    Second Edition,
!    Dover, 2007,
!    ISBN: 0486453391,
!    LC: QA299.3.D28.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NTAB, number of data points.  NTAB must be at least 3.
!
!    Input, real ( kind = 8 ) X(NTAB), contains the X values of the data,
!    in order.
!
!    Input, real ( kind = 8 ) Y(NTAB), contains the Y values of the data.
!
!    Output, real ( kind = 8 ) RESULT.
!    RESULT is the approximate value of the integral.
!
  implicit none

  integer ( kind = i4 ) ntab

  real ( kind = dp ) del(3)
  real ( kind = dp ) e
  real ( kind = dp ) f
  real ( kind = dp ) feints
  real ( kind = dp ) g(3)
  integer ( kind = i4 ) i
  integer ( kind = i4 ) n
  real ( kind = dp ) pi(3)
  real ( kind = dp ) result
  real ( kind = dp ) sum1
  real ( kind = dp ) x(ntab)
  real ( kind = dp ) x1
  real ( kind = dp ) x2
  real ( kind = dp ) x3
  real ( kind = dp ) y(ntab)

  result = 0.0_dp
 
  if ( ntab <= 2 ) then
    !write ( *, '(a)' ) ' '
    !write ( *, '(a)' ) 'SIMPNE - Fatal error!'
    !write ( *, '(a)' ) '  NTAB <= 2.'
    !stop
    return
  end if
 
  n = 1
 
  do
 
    x1 = x(n)
    x2 = x(n+1)
    x3 = x(n+2)
    e = x3 * x3- x1 * x1
    f = x3 * x3 * x3 - x1 * x1 * x1
    feints = x3 - x1

    del(1) = x3 - x2
    del(2) = x1 - x3
    del(3) = x2 - x1

    g(1) = x2 + x3
    g(2) = x1 + x3
    g(3) = x1 + x2

    pi(1) = x2 * x3
    pi(2) = x1 * x3
    pi(3) = x1 * x2
 
    sum1 = 0.0_dp
    do i = 1, 3
      sum1 = sum1 + y(n-1+i) * del(i) &
        * ( f / 3.0_dp - g(i) * 0.5_dp * e + pi(i) * feints )
    end do
    result = result - sum1 / ( del(1) * del(2) * del(3) )
 
    n = n + 2

    if ( ntab <= n + 1 ) then
      exit
    end if

  end do
 
  if ( mod ( ntab, 2 ) /= 0 ) then
    return
  end if

  n = ntab - 2
  x3 = x(ntab)
  x2 = x(ntab-1)
  x1 = x(ntab-2)
  e = x3 * x3 - x2 * x2
  f = x3 * x3 * x3 - x2 * x2 * x2
  feints = x3 - x2

  del(1) = x3 - x2
  del(2) = x1 - x3
  del(3) = x2 - x1

  g(1) = x2 + x3
  g(2) = x1 + x3
  g(3) = x1 + x2

  pi(1) = x2 * x3
  pi(2) = x1 * x3
  pi(3) = x1 * x2
 
  sum1 = 0.0_dp
  do i = 1, 3
    sum1 = sum1 + y(n-1+i) * del(i) * &
      ( f / 3.0_dp - g(i) * 0.5_dp * e + pi(i) * feints )
  end do
 
  result = result - sum1 / ( del(1) * del(2) * del(3) )
 
  return
end subroutine simpne_r8




subroutine simpsn_r8 ( ntab, h, y, result )

!*****************************************************************************80
!
!! SIMPSN approximates the integral of evenly spaced data.
!
!  Discussion:
!
!    Simpson's rule is used.
!
!  Modified:
!
!    10 February 2006
!
!  Reference:
!
!    Philip Davis, Philip Rabinowitz,
!    Methods of Numerical Integration,
!    Second Edition,
!    Dover, 2007,
!    ISBN: 0486453391,
!    LC: QA299.3.D28.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NTAB, the number of data points.  NTAB must be at least 3.
!
!    Input, real ( kind = 8 ) H, specifies the increment between the
!    X values.  Note that the actual X values are not needed,
!    just the constant spacing!
!
!    Input, real ( kind = 8 ) Y(NTAB), the data.
!
!    Output, real ( kind = 8 ) RESULT, the value of the integral
!    from the first to the last point.
!
  implicit none

  integer ( kind = i4 ) ntab

  real ( kind = dp ) del(3)
  real ( kind = dp ) f
  real ( kind = dp ) g(3)
  real ( kind = dp ) h
  integer ( kind = i4 ) i
  integer ( kind = i4 ) n
  real ( kind = dp ) pii(3)
  real ( kind = dp ) result
  real ( kind = dp ) sum1
  real ( kind = dp ) y(ntab)

  result = 0.0_dp
 
  if ( ntab <= 2 ) then
    !write ( *, '(a)' ) ' '
    !write ( *, '(a)' ) 'SIMPSN - Fatal error!'
    !write ( *, '(a,i8)' ) '  NTAB < 2, NTAb = ', ntab
    !stop
    return
  end if
 
  if ( mod ( ntab, 2 ) == 0 ) then
    n = ntab - 1
  else
    n = ntab
  end if
 
  result = y(1) + y(n) + 4.0_dp * y(n-1)
  do i = 2, n-2, 2
    result = result + 4.0_dp * y(i) + 2.0_dp * y(i+1)
  end do
  result = h * result / 3.0_dp
 
  if ( mod ( ntab, 2 ) == 1 ) then
    return
  end if
 
  f = h**3
  del(1) = h
  del(2) = -2.0_dp * h
  del(3) = h
  g(1) = h
  g(2) = 0.0_dp
  g(3) = -h
  pii(1) = 0.0_dp
  pii(2) = -h**2
  pii(3) = 0.0_dp
  n = n-1
 
  sum1 = 0.0_dp
  do i = 1, 3
    sum1 = sum1 + y(n-1+i) * del(i) * &
      ( f / 3.0_dp - g(i) * 0.5_dp * h**2 + pii(i) * h )
  end do
 
  result = result + 0.5_dp * sum1 / h**3
 
  return
end subroutine simpsn_r8


subroutine simpsn_r4 ( ntab, h, y, result )

!*****************************************************************************80
!
!! SIMPSN approximates the integral of evenly spaced data.
!
!  Discussion:
!
!    Simpson's rule is used.
!
!  Modified:
!
!    10 February 2006
!
!  Reference:
!
!    Philip Davis, Philip Rabinowitz,
!    Methods of Numerical Integration,
!    Second Edition,
!    Dover, 2007,
!    ISBN: 0486453391,
!    LC: QA299.3.D28.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NTAB, the number of data points.  NTAB must be at least 3.
!
!    Input, real ( kind = 8 ) H, specifies the increment between the
!    X values.  Note that the actual X values are not needed,
!    just the constant spacing!
!
!    Input, real ( kind = 8 ) Y(NTAB), the data.
!
!    Output, real ( kind = 8 ) RESULT, the value of the integral
!    from the first to the last point.
!
  implicit none

  integer ( kind = i4 ) ntab

  real ( kind = sp ) del(3)
  real ( kind = sp ) f
  real ( kind = sp ) g(3)
  real ( kind = sp ) h
  integer ( kind = i4 ) i
  integer ( kind = i4 ) n
  real ( kind = sp ) pii(3)
  real ( kind = sp ) result
  real ( kind = sp ) sum1
  real ( kind = sp ) y(ntab)

  result = 0.0_sp
 
  if ( ntab <= 2 ) then
    !write ( *, '(a)' ) ' '
    !write ( *, '(a)' ) 'SIMPSN - Fatal error!'
    !write ( *, '(a,i8)' ) '  NTAB < 2, NTAb = ', ntab
    !stop
    return
  end if
 
  if ( mod ( ntab, 2 ) == 0 ) then
    n = ntab - 1
  else
    n = ntab
  end if
 
  result = y(1) + y(n) + 4.0_sp * y(n-1)
  do i = 2, n-2, 2
    result = result + 4.0_sp * y(i) + 2.0_sp * y(i+1)
  end do
  result = h * result / 3.0_sp
 
  if ( mod ( ntab, 2 ) == 1 ) then
    return
  end if
 
  f = h**3
  del(1) = h
  del(2) = -2.0_sp * h
  del(3) = h
  g(1) = h
  g(2) = 0.0_sp
  g(3) = -h
  pii(1) = 0.0_sp
  pii(2) = -h**2
  pii(3) = 0.0_sp
  n = n-1
 
  sum1 = 0.0_sp
  do i = 1, 3
    sum1 = sum1 + y(n-1+i) * del(i) * &
      ( f / 3.0_sp - g(i) * 0.5_sp * h**2 + pii(i) * h )
  end do
 
  result = result + 0.5_sp * sum1 / h**3
 
  return
end subroutine simpsn_r4


function solve ( shift, n, a, b )

!*****************************************************************************80
!
!! SOLVE solves a special linear system.
!
!  Discussion:
!
!    SOLVE solves for the N-th component of the solution DELTA to the equation
!
!      (Jn - shift*Identity) * DELTA  = En,
!
!    En is the vector of all zeroes except for 1 in the N-th position.
!
!    The matrix Jn is symmetric tridiagonal, with diagonal
!    elements A(I), off-diagonal elements B(I).  This equation
!    must be solved to obtain the appropriate changes in the lower
!    2 by 2 submatrix of coefficients for orthogonal polynomials.
!
!  Modified:
!
!    30 October 2000
!
!  Parameters:
!
!    Input, real ( kind = 8 ) SHIFT, the value of the factor that multiplies
!    the identity matrix, in the definition of the system matrix.
!
!    Input, integer ( kind = 4 ) N, the index of the desired component.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) alpha
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) shift
  real ( kind = 8 ) solve

  alpha = a(1) - shift
  do i = 2, n-1
    alpha = a(i) - shift - b(i-1)**2 / alpha
  end do
 
  solve = 1.0D+00 / alpha
 
  return
end function solve


subroutine wedint_r8 ( ntab, h, ftab, result )

!*****************************************************************************80
!
!! WEDINT uses Weddle's rule to integrate data at equally spaced points.
!
!  Modified:
!
!    10 February 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NTAB, is the number of data points.  
!    (NTAB-1) must be divisible by 6.
!
!    Input, real ( kind = 8 ) H, is the spacing between the points at which
!    the data was evaluated.
!
!    Input, real ( kind = 8 ) FTAB(NTAB), contains the tabulated data values.
!
!    Output, real ( kind = 8 ) RESULT, is the approximation to the integral.
!
  implicit none

  integer ( kind = i4 ) ntab

  real ( kind = dp ) ftab(ntab)
  real ( kind = dp ) h
  integer ( kind = i4 ) i
  real ( kind = dp ) result

  result = 0.0_dp
 
  if ( ntab <= 1 ) then
    !write ( *, '(a)' ) ' '
    !write ( *, '(a)' ) 'WEDINT - Fatal error!'
    !write ( *, '(a)' ) '  NTAB < 2'
    !write ( *, '(a,i8)' ) '  NTAB = ', ntab
    !stop
    return
  end if
 
  if ( mod ( ntab, 6 ) /= 1 ) then
    !write ( *, '(a)' ) ' '
    !write ( *, '(a)' ) 'WEDINT - Fatal error!'
    !write ( *, '(a)' ) '  NTAB must equal 6*N+1 for some N!'
    !stop
    return
  end if
 
  do i = 1, ntab-6, 6
    result = result & 
      +           ftab(i)   &
      + 5.0_dp  * ftab(i+1) &
      +           ftab(i+2) &
      + 6.0_dp  * ftab(i+3) &
      +           ftab(i+4) &
      + 5.0_dp  * ftab(i+5) &
      +           ftab(i+6)
  end do
 
  result = 3.0_dp * h * result / 10.0_dp
 
  return
end subroutine wedint_r8


subroutine wedint_r4 ( ntab, h, ftab, result )

!*****************************************************************************80
!
!! WEDINT uses Weddle's rule to integrate data at equally spaced points.
!
!  Modified:
!
!    10 February 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NTAB, is the number of data points.  
!    (NTAB-1) must be divisible by 6.
!
!    Input, real ( kind = 8 ) H, is the spacing between the points at which
!    the data was evaluated.
!
!    Input, real ( kind = 8 ) FTAB(NTAB), contains the tabulated data values.
!
!    Output, real ( kind = 8 ) RESULT, is the approximation to the integral.
!
  implicit none

  integer ( kind = i4 ) ntab

  real ( kind = sp ) ftab(ntab)
  real ( kind = sp ) h
  integer ( kind = i4 ) i
  real ( kind = sp ) result

  result = 0.0_sp
 
  if ( ntab <= 1 ) then
    !write ( *, '(a)' ) ' '
    !write ( *, '(a)' ) 'WEDINT - Fatal error!'
    !write ( *, '(a)' ) '  NTAB < 2'
    !write ( *, '(a,i8)' ) '  NTAB = ', ntab
    !stop
    return
  end if
 
  if ( mod ( ntab, 6 ) /= 1 ) then
    !write ( *, '(a)' ) ' '
    !write ( *, '(a)' ) 'WEDINT - Fatal error!'
    !write ( *, '(a)' ) '  NTAB must equal 6*N+1 for some N!'
    !stop
    return
  end if
 
  do i = 1, ntab-6, 6
    result = result & 
      +           ftab(i)   &
      + 5.0_sp  * ftab(i+1) &
      +           ftab(i+2) &
      + 6.0_sp  * ftab(i+3) &
      +           ftab(i+4) &
      + 5.0_sp  * ftab(i+5) &
      +           ftab(i+6)
  end do
 
  result = 3.0_sp * h * result / 10.0_sp
 
  return
end subroutine wedint_r4


end module tabulated_quad
