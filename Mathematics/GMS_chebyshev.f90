subroutine chebyshev_coefficients ( a, b, n, f, c )

!*****************************************************************************80
!
!! CHEBYSHEV_COEFFICIENTS determines Chebyshev interpolation coefficients.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 September 2011
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Roger Broucke,
!    Algorithm 446:
!    Ten Subroutines for the Manipulation of Chebyshev Series,
!    Communications of the ACM,
!    Volume 16, Number 4, April 1973, pages 254-256.
!
!    William Press, Brian Flannery, Saul Teukolsky, William Vetterling,
!    Numerical Recipes in FORTRAN: The Art of Scientific Computing,
!    Second Edition,
!    Cambridge University Press, 1992,
!    ISBN: 0-521-43064-X,
!    LC: QA297.N866.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the domain of definition.
!
!    Input, integer ( kind = 4 ) N, the order of the interpolant.
!
!    Input, real ( kind = 8 ), external :: F ( X ), an external function.
!
!    Output, real ( kind = 8 ) C(N), the Chebyshev coefficients.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) angle
  real ( kind = 8 ) b
  real ( kind = 8 ) c(n)
  real ( kind = 8 ), external :: f
  real ( kind = 8 ) fx(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) x

  do i = 1, n
    angle = real ( 2 * i - 1, kind = 8 ) * pi / real ( 2 * n, kind = 8 )
    x = cos ( angle )
    x = 0.5D+00 * ( a + b ) + x * 0.5D+00 * ( b - a )
    fx(i) = f ( x );
  end do

  do i = 1, n
    c(i) = 0.0D+00
    do j = 1, n
      angle = real ( ( i - 1 ) * ( 2 * j - 1 ), kind = 8 ) * pi &
        / real ( 2 * n, kind = 8 )
      c(i) = c(i) + fx(j) * cos ( angle )
    end do
  end do

  c(1:n) = 2.0D+00 * c(1:n) / real ( n, kind = 8 )

  return
end
subroutine chebyshev_interpolant ( a, b, n, c, m, x, cf )

!*****************************************************************************80
!
!! CHEBYSHEV_INTERPOLANT evaluates a Chebyshev interpolant.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 September 2011
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Roger Broucke,
!    Algorithm 446:
!    Ten Subroutines for the Manipulation of Chebyshev Series,
!    Communications of the ACM,
!    Volume 16, Number 4, April 1973, pages 254-256.
!
!    William Press, Brian Flannery, Saul Teukolsky, William Vetterling,
!    Numerical Recipes in FORTRAN: The Art of Scientific Computing,
!    Second Edition,
!    Cambridge University Press, 1992,
!    ISBN: 0-521-43064-X,
!    LC: QA297.N866.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the domain of definition.
!
!    Input, integer ( kind = 4 ) N, the order of the polynomial.
!
!    Input, real ( kind = 8 ) C(N), the Chebyshev coefficients.
!
!    Input, integer ( kind = 4 ) M, the number of points.
!
!    Input, real ( kind = 8 ) X(M), the point at which the polynomial is
!    to be evaluated.
!
!    Output, real ( kind = 8 ) CF(M), the value of the Chebyshev
!    polynomial at X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c(n)
  real ( kind = 8 ) cf(m)
  real ( kind = 8 ) di
  real ( kind = 8 ) dip1
  real ( kind = 8 ) dip2
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) x(m)
  real ( kind = 8 ) y

  do j = 1, m

    dip1 = 0.0D+00
    di = 0.0D+00
    y = ( 2.0D+00 * x(j) - a  - b ) / ( b - a )

    do i = n, 2, -1
      dip2 = dip1
      dip1 = di
      di = 2.0D+00 * y * dip1 - dip2 + c(i)
    end do

    cf(j) = y * di - dip1 + 0.5D+00 * c(1)

  end do

  return
end
subroutine chebyshev_zeros ( n, x )

!*****************************************************************************80
!
!! CHEBYSHEV_ZEROS returns zeroes of the Chebyshev polynomial T(N,X).
!
!  Discussion:
!
!    We produce the Chebyshev zeros in ascending order.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    14 September 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the polynomial.
!
!    Output, real ( kind = 8 ) X(N), the zeroes of T(N)(X).
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) angle
  integer ( kind = 4 ) i
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) x(n)

  do i = 1, n
    angle = real ( 2 * ( n - i ) + 1, kind = 8 ) * pi / real ( 2 * n, kind = 8 )
    x(i) = cos ( angle )
  end do

  return
end

