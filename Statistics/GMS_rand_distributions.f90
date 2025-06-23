MODULE rand_scalar_distributions
! A module for random number generation from the following distributions:
!
!     Distribution                    Function/subroutine name
!
!     Normal (Gaussian)               random_normal
!     Gamma                           random_gamma
!     Chi-squared                     random_chisq
!     Exponential                     random_exponential
!     Weibull                         random_Weibull
!     Beta                            random_beta
!     t                               random_t
!     Multivariate normal             random_mvnorm
!     Generalized inverse Gaussian    random_inv_gauss
!     Poisson                         random_Poisson
!     Binomial                        random_binomial1   *
!                                     random_binomial2   *
!     Negative binomial               random_neg_binomial
!    
!     Cauchy                          random_Cauchy
!
!  Generate a random ordering of the integers 1 .. N
!                                     random_order
!     Initialize (seed) the uniform random number generator for ANY compiler
!                                     seed_random_number

!     Lognormal - see note below.

!  ** Two functions are provided for the binomial distribution.
!  If the parameter values remain constant, it is recommended that the
!  first function is used (random_binomial1).   If one or both of the
!  parameters change, use the second function (random_binomial2).

! The compilers own random number generator, SUBROUTINE RANDOM_NUMBER(r),
! is used to provide a source of uniformly distributed random numbers.

! N.B. At this stage, only one random number is generated at each call to
!      one of the functions above.

! The module uses the following functions which are included here:
! bin_prob to calculate a single binomial probability
! lngamma  to calculate the logarithm to base e of the gamma function

! Some of the code is adapted from Dagpunar's book:
!     Dagpunar, J. 'Principles of random variate generation'
!     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9
!
! In most of Dagpunar's routines, there is a test to see whether the value
! of one or two floating-point parameters has changed since the last call.
! These tests have been replaced by using a logical variable FIRST.
! This should be set to .TRUE. on the first call using new values of the
! parameters, and .FALSE. if the parameter values are the same as for the
! previous call.

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Lognormal distribution
! If X has a lognormal distribution, then log(X) is normally distributed.
! Here the logarithm is the natural logarithm, that is to base e, sometimes
! denoted as ln.  To generate random variates from this distribution, generate
! a random deviate from the normal distribution with mean and variance equal
! to the mean and variance of the logarithms of X, then take its exponential.

! Relationship between the mean & variance of log(X) and the mean & variance
! of X, when X has a lognormal distribution.
! Let m = mean of log(X), and s^2 = variance of log(X)
! Then
! mean of X     = exp(m + 0.5s^2)
! variance of X = (mean(X))^2.[exp(s^2) - 1]

! In the reverse direction (rarely used)
! variance of log(X) = log[1 + var(X)/(mean(X))^2]
! mean of log(X)     = log(mean(X) - 0.5var(log(X))

! N.B. The above formulae relate to population parameters; they will only be
!      approximate if applied to sample values.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Version 1.13, 2 October 2000
! Changes from version 1.01
! 1. The random_order, random_Poisson & random_binomial routines have been
!    replaced with more efficient routines.
! 2. A routine, seed_random_number, has been added to seed the uniform random
!    number generator.   This requires input of the required number of seeds
!    for the particular compiler from a specified I/O unit such as a keyboard.
! 3. Made compatible with Lahey's ELF90.
! 4. Marsaglia & Tsang algorithm used for random_gamma when shape parameter > 1.
! 5. INTENT for array f corrected in random_mvnorm.

!     Author: Alan Miller
  !     e-mail: amiller @ bigpond.net.au
! Slightly modified by Bernard Gingold on 23-01-2021

use mod_kinds, only : i4, sp, dp
IMPLICIT NONE
REAL, PRIVATE      :: zero = 0.0_sp, half = 0.5_sp, one = 1.0_sp, two = 2.0_sp,   &
                      vsmall = TINY(1.0_sp), vlarge = HUGE(1.0_sp)
PRIVATE            :: integral



CONTAINS


#if defined (__ICC) || defined (__INTEL_COMPILER)
!DIR$ ATTRIBUTES INLINE :: random_normal

#endif
FUNCTION random_normal() RESULT(fn_val)
 


! Adapted from the following Fortran 77 code
!      ALGORITHM 712, COLLECTED ALGORITHMS FROM ACM.
!      THIS WORK PUBLISHED IN TRANSACTIONS ON MATHEMATICAL SOFTWARE,
!      VOL. 18, NO. 4, DECEMBER, 1992, PP. 434-435.

!  The function random_normal() returns a normally distributed pseudo-random
!  number with zero mean and unit variance.

!  The algorithm uses the ratio of uniforms method of A.J. Kinderman
!  and J.F. Monahan augmented with quadratic bounding curves.

REAL(kind=sp) :: fn_val

!     Local variables
REAL(kind=sp)     :: s = 0.449871_sp, t = -0.386595_sp, a = 0.19600_sp, b = 0.25472_sp,    &
                     r1 = 0.27597_sp, r2 = 0.27846_sp, u, v, x, y, q

!     Generate P = (u,v) uniform in rectangle enclosing acceptance region

DO
  CALL RANDOM_NUMBER(u)
  CALL RANDOM_NUMBER(v)
  v = 1.7156_sp * (v - half)

!     Evaluate the quadratic form
  x = u - s
  y = ABS(v) - t
  q = x**2 + y*(a*y - b*x)

!     Accept P if inside inner ellipse
  IF (q < r1) EXIT
!     Reject P if outside outer ellipse
  IF (q > r2) CYCLE
!     Reject P if outside acceptance region
  IF (v**2 < -4.0_sp*LOG(u)*u**2) EXIT
END DO

!     Return ratio of P's coordinates as the normal deviate
fn_val = v/u
RETURN

END FUNCTION random_normal

#if defined (__ICC) || defined (__INTEL_COMPILER)
!DIR$ ATTRIBUTES INLINE :: random_normal_clamped

#endif
FUNCTION random_normal_clamped() RESULT(fn_val)
  


! Adapted from the following Fortran 77 code
!      ALGORITHM 712, COLLECTED ALGORITHMS FROM ACM.
!      THIS WORK PUBLISHED IN TRANSACTIONS ON MATHEMATICAL SOFTWARE,
!      VOL. 18, NO. 4, DECEMBER, 1992, PP. 434-435.

!  The function random_normal() returns a normally distributed pseudo-random
!  number with zero mean and unit variance.

!  The algorithm uses the ratio of uniforms method of A.J. Kinderman
!  and J.F. Monahan augmented with quadratic bounding curves.
!  **Changed ny Bernard Gingold on 21/01/2024 to be used exclusively for the clamped output
!  generation forced to lie in [0.1-1.47079632679489661923132] angle (rad) measure.
!

REAL(kind=sp) :: fn_val

!     Local variables
REAL(kind=sp)     :: s = 0.449871_sp, t = -0.386595_sp, a = 0.19600_sp, b = 0.25472_sp,    &
                     r1 = 0.27597_sp, r2 = 0.27846_sp
REAL(kind=sp)     :: u, v, x, y, q,tmp
REAL(kind=sp), parameter :: C01 = 0.1_sp ! angmin
REAL(kind=sp), parameter :: C147079632679489661923132 = 1.47079632679489661923132_sp ! angmax
REAL(kind=sp), parameter :: posv   = -1.0_sp
REAL(kind=sp), parameter :: C157079632679489661923132 = 1.57079632679489661923132_sp
!     Generate P = (u,v) uniform in rectangle enclosing acceptance region

DO
  CALL RANDOM_NUMBER(u)
  CALL RANDOM_NUMBER(v)
  v = 1.7156_sp * (v - half)

!     Evaluate the quadratic form
  x = u - s
  y = ABS(v) - t
  q = x**2 + y*(a*y - b*x)

!     Accept P if inside inner ellipse
  IF (q < r1) EXIT
!     Reject P if outside outer ellipse
  IF (q > r2) CYCLE
!     Reject P if outside acceptance region
  IF (v**2 < -4.0_sp*LOG(u)*u**2) EXIT
END DO

!     Return ratio of P's coordinates as the normal deviate
! Added clamping test
  tmp = v/u
  if(tmp>=C157079632679489661923132) then 
      fn_val=C147079632679489661923132
  else if(tmp<0.0_sp) then 
      fn_val=tmp*posv
  else if(tmp==0.0_sp.or.tmp<C01) then 
      fn_val=C01
  else
      fn_val=tmp
  end if
RETURN

END FUNCTION random_normal_clamped


#if defined (__ICC) || defined (__INTEL_COMPILER)
 !DIR$ ATTRIBUTES INLINE :: random_gamma
 
#endif
  FUNCTION random_gamma(s,first) RESULT(fn_val)
   

! Adapted from Fortran 77 code from the book:
!     Dagpunar, J. 'Principles of random variate generation'
!     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

!     FUNCTION GENERATES A RANDOM GAMMA VARIATE.
!     CALLS EITHER random_gamma1 (S > 1.0)
!     OR random_exponential (S = 1.0)
!     OR random_gamma2 (S < 1.0).

!     S = SHAPE PARAMETER OF DISTRIBUTION (0 < REAL).

REAL(kind=sp), INTENT(IN)    :: s
LOGICAL, INTENT(IN) :: first
REAL(kind=sp)                :: fn_val

! Input checking removed.

IF (s > one) THEN
  fn_val = random_gamma1(s, first)
ELSE IF (s < one) THEN
  fn_val = random_gamma2(s)
ELSE
  fn_val = random_exponential()
END IF

RETURN
END FUNCTION random_gamma



#if defined (__ICC) || defined (__INTEL_COMPILER)
!DIR$ ATTRIBUTES INLINE :: random_gamma1

#endif
FUNCTION random_gamma1(s,first) RESULT(fn_val)
    
    
! Uses the algorithm in
! Marsaglia, G. and Tsang, W.W. (2000) `A simple method for generating
! gamma variables', Trans. om Math. Software (TOMS), vol.26(3), pp.363-372.

! Generates a random gamma deviate for shape parameter s >= 1.

REAL(kind=sp), INTENT(IN)    :: s
LOGICAL, INTENT(IN)          :: first
REAL(kind=sp)                :: fn_val

! Local variables
REAL(kind=sp), SAVE  :: c, d
#if defined(_OPENMP)
!$omp threadprivate (c,d)
#endif
REAL(kind=sp)        :: u, v, x

IF (first) THEN
  d = s - one/3._sp
  c = one/SQRT(9.0_sp*d)
END IF

! Start of main loop
DO

! Generate v = (1+cx)^3 where x is random normal; repeat if v <= 0.

  DO
    x = random_normal()
    v = (one + c*x)**3
    IF (v > zero) EXIT
  END DO

! Generate uniform variable U

  CALL RANDOM_NUMBER(u)
  IF (u < one - 0.0331_sp*x**4) THEN
    fn_val = d*v
    EXIT
  ELSE IF (LOG(u) < half*x**2 + d*(one - v + LOG(v))) THEN
    fn_val = d*v
    EXIT
  END IF
END DO

RETURN
END FUNCTION random_gamma1





#if defined (__ICC) || defined (__INTEL_COMPILER)
 !DIR$ ATTRIBUTES INLINE :: random_gamma2
 
#endif
  FUNCTION random_gamma2(s) RESULT(fn_val)
    

! Adapted from Fortran 77 code from the book:
!     Dagpunar, J. 'Principles of random variate generation'
!     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

! FUNCTION GENERATES A RANDOM VARIATE IN [0,INFINITY) FROM
! A GAMMA DISTRIBUTION WITH DENSITY PROPORTIONAL TO
! GAMMA2**(S-1) * EXP(-GAMMA2),
! USING A SWITCHING METHOD.

!    S = SHAPE PARAMETER OF DISTRIBUTION
!          (REAL < 1.0)

REAL(kind=sp), INTENT(IN)    :: s
!LOGICAL, INTENT(IN)          :: first
REAL(kind=sp)                :: fn_val

!     Local variables
REAL(kind=sp)       :: r, x, w
REAL(kind=sp), SAVE :: a, p, c, uf, vr, d
#if defined(_OPENMP)
!$omp threadprivate(a,p,c,uf,vr,d)
#endif
#if 0
IF (first) THEN                        ! Initialization, if necessary
  a = one - s
  p = a/(a + s*EXP(-a))
  IF (s < vsmall) THEN
     fn_val = vsmall
     return
  END IF
  c = one/s
  uf = p*(vsmall/a)**s
  vr = one - vsmall
  d = a*LOG(a)
END IF
#endif 
DO
  CALL RANDOM_NUMBER(r)
  IF (r >= vr) THEN
    CYCLE
  ELSE IF (r > p) THEN
    x = a - LOG((one - r)/(one - p))
    w = a*LOG(x)-d
  ELSE IF (r > uf) THEN
    x = a*(r/p)**c
    w = x
  ELSE
    fn_val = zero
    RETURN
  END IF

  CALL RANDOM_NUMBER(r)
  IF (one-r <= w .AND. r > zero) THEN
    IF (r*(w + one) >= one) CYCLE
    IF (-LOG(r) <= w) CYCLE
  END IF
  EXIT
END DO

fn_val = x
RETURN

END FUNCTION random_gamma2



#if defined (__ICC) || defined (__INTEL_COMPILER)
 !DIR$ ATTRIBUTES INLINE :: random_chisq
  
#endif
FUNCTION random_chisq(ndf, first) RESULT(fn_val)
 


!     Generates a random variate from the chi-squared distribution with
!     ndf degrees of freedom

INTEGER, INTENT(IN) :: ndf
LOGICAL, INTENT(IN) :: first
REAL(kind=sp)                :: fn_val

fn_val = two * random_gamma(half*ndf, first)
RETURN

END FUNCTION random_chisq





#if defined (__ICC) || defined (__INTEL_COMPILER)
!DIR$ ATTRIBUTES INLINE :: random_exponential

#endif
  FUNCTION random_exponential() RESULT(fn_val)
    

! Adapted from Fortran 77 code from the book:
!     Dagpunar, J. 'Principles of random variate generation'
!     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

! FUNCTION GENERATES A RANDOM VARIATE IN [0,INFINITY) FROM
! A NEGATIVE EXPONENTIAL DlSTRIBUTION WlTH DENSITY PROPORTIONAL
! TO EXP(-random_exponential), USING INVERSION.

REAL(kind=sp)  :: fn_val

!     Local variable
REAL(kind=sp)  :: r

DO
  CALL RANDOM_NUMBER(r)
  IF (r > zero) EXIT
END DO

fn_val = -LOG(r)
RETURN

END FUNCTION random_exponential


#if defined (__ICC) || defined (__INTEL_COMPILER)
!DIR$ ATTRIBUTES INLINE :: random_exponential_clamped

#endif
  FUNCTION random_exponential_clamped() RESULT(fn_val)
    

! Adapted from Fortran 77 code from the book:
!     Dagpunar, J. 'Principles of random variate generation'
!     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

! FUNCTION GENERATES A RANDOM VARIATE IN [0,INFINITY) FROM
! A NEGATIVE EXPONENTIAL DlSTRIBUTION WlTH DENSITY PROPORTIONAL
! TO EXP(-random_exponential), USING INVERSION.

REAL(kind=sp)  :: fn_val
REAL(kind=sp), parameter :: C01 = 0.1_sp ! angmin
REAL(kind=sp), parameter :: C147079632679489661923132 = 1.47079632679489661923132_sp ! angmax
REAL(kind=sp), parameter :: posv   = -1.0_sp
REAL(kind=sp), parameter :: C157079632679489661923132 = 1.57079632679489661923132_sp

!     Local variable
REAL(kind=sp)  :: r,tmp

DO
  CALL RANDOM_NUMBER(r)
  IF (r > zero) EXIT
END DO

tmp = -LOG(r)
if(tmp>=C157079632679489661923132) then
    fn_val=C147079632679489661923132
else if(tmp<zero) then
   fn_val=tmp*posv
else if(tmp==zero.or.tmp<C01) then
   fn_val=C01
else
fn_val = tmp
end if
RETURN

END FUNCTION random_exponential_clamped



#if defined (__ICC) || defined (__INTEL_COMPILER)
!DIR$ ATTRIBUTES INLINE :: random_Weibull
    
#endif
FUNCTION random_Weibull(a) RESULT(fn_val)
    


!     Generates a random variate from the Weibull distribution with
!     probability density:
!                      a
!               a-1  -x
!     f(x) = a.x    e

REAL(kind=sp), INTENT(IN) :: a
REAL(kind=sp)             :: fn_val

!     For speed, there is no checking that a is not zero or very small.

fn_val = random_exponential() ** (one/a)
RETURN

END FUNCTION random_Weibull


#if defined (__ICC) || defined (__INTEL_COMPILER)
!DIR$ ATTRIBUTES INLINE :: random_Weibull_clamped
   
#endif
FUNCTION random_Weibull_clamped(a) RESULT(fn_val)
    


!     Generates a random variate from the Weibull distribution with
!     probability density:
!                      a
!               a-1  -x
!     f(x) = a.x    e
! 'a' -- must be greater then 0.9!!

REAL(kind=sp), INTENT(IN) :: a
REAL(kind=sp)             :: fn_val

!     For speed, there is no checking that a is not zero or very small.

fn_val = random_exponential_clamped() ** (one/a)
RETURN

END FUNCTION random_Weibull_clamped





#if defined (__ICC) || defined (__INTEL_COMPILER)
 !DIR$ ATTRIBUTES INLINE :: random_beta
   
#endif
  FUNCTION random_beta(aa,bb,first) RESULT(fn_val)
   


! Adapted from Fortran 77 code from the book:
!     Dagpunar, J. 'Principles of random variate generation'
!     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

! FUNCTION GENERATES A RANDOM VARIATE IN [0,1]
! FROM A BETA DISTRIBUTION WITH DENSITY
! PROPORTIONAL TO BETA**(AA-1) * (1-BETA)**(BB-1).
! USING CHENG'S LOG LOGISTIC METHOD.

!     AA = SHAPE PARAMETER FROM DISTRIBUTION (0 < REAL)
!     BB = SHAPE PARAMETER FROM DISTRIBUTION (0 < REAL)

REAL(kind=sp), INTENT(IN)    :: aa, bb
LOGICAL, INTENT(IN) :: first
REAL(kind=sp)                :: fn_val

!     Local variables
REAL(kind=sp), PARAMETER  :: aln4 = 1.3862944_sp
REAL(kind=sp)             :: a, b, g, r, s, x, y, z
REAL(kind=sp), SAVE       :: d, f, h, t, c
#if defined(_OPENMP)
!$omp threadprivate(d,f,h,t,c)
#endif
LOGICAL, SAVE    :: swap
#if defined(_OPENMP)
!$omp threadprivate(swap)
#endif

IF (first) THEN                        ! Initialization, if necessary
  a = aa
  b = bb
  swap = b > a
  IF (swap) THEN
    g = b
    b = a
    a = g
  END IF
  d = a/b
  f = a+b
  IF (b > one) THEN
    h = SQRT((two*a*b - f)/(f - two))
    t = one
  ELSE
    h = b
    t = one/(one + (a/(vlarge*b))**b)
  END IF
  c = a+h
END IF

DO
  CALL RANDOM_NUMBER(r)
  CALL RANDOM_NUMBER(x)
  s = r*r*x
  IF (r < vsmall .OR. s <= zero) CYCLE
  IF (r < t) THEN
    x = LOG(r/(one - r))/h
    y = d*EXP(x)
    z = c*x + f*LOG((one + d)/(one + y)) - aln4
    IF (s - one > z) THEN
      IF (s - s*z > one) CYCLE
      IF (LOG(s) > z) CYCLE
    END IF
    fn_val = y/(one + y)
  ELSE
    IF (4.0_sp*s > (one + one/d)**f) CYCLE
    fn_val = one
  END IF
  EXIT
END DO

IF (swap) fn_val = one - fn_val
RETURN
END FUNCTION random_beta



#if defined (__ICC) || defined (__INTEL_COMPILER)
    !DIR$ ATTRIBUTES INLINE :: random_t
  
#endif 
  FUNCTION random_t(m) RESULT(fn_val)
    


! Adapted from Fortran 77 code from the book:
!     Dagpunar, J. 'Principles of random variate generation'
!     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

! FUNCTION GENERATES A RANDOM VARIATE FROM A
! T DISTRIBUTION USING KINDERMAN AND MONAHAN'S RATIO METHOD.

!     M = DEGREES OF FREEDOM OF DISTRIBUTION
!           (1 <= 1NTEGER)

INTEGER, INTENT(IN) :: m
REAL(kind=sp)               :: fn_val

!     Local variables
REAL(kind=sp), SAVE      :: s, c, a, f, g
#if defined(_OPENMP)
!$omp threadprivate(s,c,a,f,g)
#endif
REAL(kind=sp)            :: r, x, v

REAL(kind=sp), PARAMETER :: three = 3.0_sp, four = 4.0_sp, quart = 0.25_sp,   &
                   five = 5.0_sp, sixteen = 16.0_sp
INTEGER         :: mm = 0



IF (m /= mm) THEN                    ! Initialization, if necessary
  s = m
  c = -quart*(s + one)
  a = four/(one + one/s)**c
  f = sixteen/a
  IF (m > 1) THEN
    g = s - one
    g = ((s + one)/g)**c*SQRT((s+s)/g)
  ELSE
    g = one
  END IF
  mm = m
END IF

DO
  CALL RANDOM_NUMBER(r)
  IF (r <= zero) CYCLE
  CALL RANDOM_NUMBER(v)
  x = (two*v - one)*g/r
  v = x*x
  IF (v > five - a*r) THEN
    IF (m >= 1 .AND. r*(v + three) > f) CYCLE
    IF (r > (one + v/s)**c) CYCLE
  END IF
  EXIT
END DO

fn_val = x
RETURN
END FUNCTION random_t



#if defined (__ICC) || (defined __INTEL_COMPILER)
 !DIR$ ATTRIBUTES INLINE :: random_mvnorm
    
#endif
  SUBROUTINE random_mvnorm(n,h,d,f,first,x,ier)
   


! Adapted from Fortran 77 code from the book:
!     Dagpunar, J. 'Principles of random variate generation'
!     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

! N.B. An extra argument, ier, has been added to Dagpunar's routine

!     SUBROUTINE GENERATES AN N VARIATE RANDOM NORMAL
!     VECTOR USING A CHOLESKY DECOMPOSITION.

! ARGUMENTS:
!        N = NUMBER OF VARIATES IN VECTOR
!           (INPUT,INTEGER >= 1)
!     H(J) = J'TH ELEMENT OF VECTOR OF MEANS
!           (INPUT,REAL)
!     X(J) = J'TH ELEMENT OF DELIVERED VECTOR
!           (OUTPUT,REAL)
!
!    D(J*(J-1)/2+I) = (I,J)'TH ELEMENT OF VARIANCE MATRIX (J> = I)
!            (INPUT,REAL)
!    F((J-1)*(2*N-J)/2+I) = (I,J)'TH ELEMENT OF LOWER TRIANGULAR
!           DECOMPOSITION OF VARIANCE MATRIX (J <= I)
!            (OUTPUT,REAL)

!    FIRST = .TRUE. IF THIS IS THE FIRST CALL OF THE ROUTINE
!    OR IF THE DISTRIBUTION HAS CHANGED SINCE THE LAST CALL OF THE ROUTINE.
!    OTHERWISE SET TO .FALSE.
!            (INPUT,LOGICAL)

!    ier = 1 if the input covariance matrix is not +ve definite
!        = 0 otherwise

INTEGER(kind=i4), INTENT(IN)   :: n
REAL(kind=sp), INTENT(IN)      :: h(:), d(:)   ! d(n*(n+1)/2)
REAL(kind=sp), INTENT(IN OUT)  :: f(:)         ! f(n*(n+1)/2)
REAL(kind=sp), INTENT(OUT)     :: x(:)
LOGICAL, INTENT(IN)   :: first
INTEGER(kind=i4), INTENT(OUT)  :: ier

!     Local variables
INTEGER(kind=i4)       :: j, i, m
REAL(kind=sp)          :: y, v
INTEGER(kind=i4), SAVE :: n2
#if defined(_OPENMP)
!$omp threadprivate(n2)
#endif

ier = 0
IF (first) THEN                        ! Initialization, if necessary
  n2 = 2*n
  IF (d(1) < zero) THEN
    ier = 1
    RETURN
  END IF

  f(1) = SQRT(d(1))
  y = one/f(1)
  DO j = 2,n
    f(j) = d(1+j*(j-1)/2) * y
  END DO

  DO i = 2,n
    v = d(i*(i-1)/2+i)
    DO m = 1,i-1
      v = v - f((m-1)*(n2-m)/2+i)**2
    END DO

    IF (v < zero) THEN
      ier = 1
      RETURN
    END IF

    v = SQRT(v)
    y = one/v
    f((i-1)*(n2-i)/2+i) = v
    DO j = i+1,n
      v = d(j*(j-1)/2+i)
      DO m = 1,i-1
        v = v - f((m-1)*(n2-m)/2+i)*f((m-1)*(n2-m)/2 + j)
      END DO ! m = 1,i-1
      f((i-1)*(n2-i)/2 + j) = v*y
    END DO ! j = i+1,n
  END DO ! i = 2,n
END IF

x(1:n) = h(1:n)
DO j = 1,n
  y = random_normal()
  DO i = j,n
    x(i) = x(i) + f((j-1)*(n2-j)/2 + i) * y
  END DO ! i = j,n
END DO ! j = 1,n

RETURN
END SUBROUTINE random_mvnorm


#if defined (__ICC) || (defined __INTEL_COMPILER)
 !DIR$ ATTRIBUTES INLINE :: random_mvnorm_clamped
   
#endif
  SUBROUTINE random_mvnorm_clamped(n,h,d,f,first,x,ier)
   


! Adapted from Fortran 77 code from the book:
!     Dagpunar, J. 'Principles of random variate generation'
!     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

! N.B. An extra argument, ier, has been added to Dagpunar's routine

!     SUBROUTINE GENERATES AN N VARIATE RANDOM NORMAL
!     VECTOR USING A CHOLESKY DECOMPOSITION.

! ARGUMENTS:
!        N = NUMBER OF VARIATES IN VECTOR
!           (INPUT,INTEGER >= 1)
!     H(J) = J'TH ELEMENT OF VECTOR OF MEANS
!           (INPUT,REAL)
!     X(J) = J'TH ELEMENT OF DELIVERED VECTOR
!           (OUTPUT,REAL)
!
!    D(J*(J-1)/2+I) = (I,J)'TH ELEMENT OF VARIANCE MATRIX (J> = I)
!            (INPUT,REAL)
!    F((J-1)*(2*N-J)/2+I) = (I,J)'TH ELEMENT OF LOWER TRIANGULAR
!           DECOMPOSITION OF VARIANCE MATRIX (J <= I)
!            (OUTPUT,REAL)

!    FIRST = .TRUE. IF THIS IS THE FIRST CALL OF THE ROUTINE
!    OR IF THE DISTRIBUTION HAS CHANGED SINCE THE LAST CALL OF THE ROUTINE.
!    OTHERWISE SET TO .FALSE.
!            (INPUT,LOGICAL)

!    ier = 1 if the input covariance matrix is not +ve definite
!        = 0 otherwise

INTEGER(kind=i4), INTENT(IN)   :: n
REAL(kind=sp), INTENT(IN)      :: h(:), d(:)   ! d(n*(n+1)/2)
REAL(kind=sp), INTENT(IN OUT)  :: f(:)         ! f(n*(n+1)/2)
REAL(kind=sp), INTENT(OUT)     :: x(:)
LOGICAL, INTENT(IN)   :: first
INTEGER(kind=i4), INTENT(OUT)  :: ier

!     Local variables
INTEGER(kind=i4)       :: j, i, m
REAL(kind=sp)          :: y, v
INTEGER(kind=i4), SAVE :: n2
#if defined(_OPENMP)
!$omp threadprivate(n2)
#endif

ier = 0
IF (first) THEN                        ! Initialization, if necessary
  n2 = 2*n
  IF (d(1) < zero) THEN
    ier = 1
    RETURN
  END IF

  f(1) = SQRT(d(1))
  y = one/f(1)
  DO j = 2,n
    f(j) = d(1+j*(j-1)/2) * y
  END DO

  DO i = 2,n
    v = d(i*(i-1)/2+i)
    DO m = 1,i-1
      v = v - f((m-1)*(n2-m)/2+i)**2
    END DO

    IF (v < zero) THEN
      ier = 1
      RETURN
    END IF

    v = SQRT(v)
    y = one/v
    f((i-1)*(n2-i)/2+i) = v
    DO j = i+1,n
      v = d(j*(j-1)/2+i)
      DO m = 1,i-1
        v = v - f((m-1)*(n2-m)/2+i)*f((m-1)*(n2-m)/2 + j)
      END DO ! m = 1,i-1
      f((i-1)*(n2-i)/2 + j) = v*y
    END DO ! j = i+1,n
  END DO ! i = 2,n
END IF

x(1:n) = h(1:n)
DO j = 1,n
  y = random_normal_clamped()
  DO i = j,n
    x(i) = x(i) + f((j-1)*(n2-j)/2 + i) * y
  END DO ! i = j,n
END DO ! j = 1,n

RETURN
END SUBROUTINE random_mvnorm_clamped 




#if defined (__ICC) || defined (__INTEL_COMPILER)
 !DIR$ ATTRIBUTES INLINE :: random_inv_gauss
   
#endif
FUNCTION random_inv_gauss(h,b, first) RESULT(fn_val)
   

! Adapted from Fortran 77 code from the book:
!     Dagpunar, J. 'Principles of random variate generation'
!     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

! FUNCTION GENERATES A RANDOM VARIATE IN [0,INFINITY] FROM
! A REPARAMETERISED GENERALISED INVERSE GAUSSIAN (GIG) DISTRIBUTION
! WITH DENSITY PROPORTIONAL TO  GIG**(H-1) * EXP(-0.5*B*(GIG+1/GIG))
! USING A RATIO METHOD.

!     H = PARAMETER OF DISTRIBUTION (0 <= REAL)
!     B = PARAMETER OF DISTRIBUTION (0 < REAL)

REAL(kind=sp), INTENT(IN)    :: h, b
LOGICAL, INTENT(IN) :: first
REAL(kind=sp)                :: fn_val

!     Local variables
REAL(kind=sp)            :: ym, xm, r, w, r1, r2, x
REAL(kind=sp), SAVE      :: a, c, d, e
REAL(kind=sp), PARAMETER :: quart = 0.25_sp
#if defined(_OPENMP)
!$omp threadprivate(a,c,d,e)
#endif

IF (first) THEN                        ! Initialization, if necessary
  IF (h > quart*b*SQRT(vlarge)) THEN
     fn_val = vlarge
     return
  END IF
  e = b*b
  d = h + one
  ym = (-d + SQRT(d*d + e))/b
  IF (ym < vsmall) THEN
     fn_val = vsmall
     return
  END IF

  d = h - one
  xm = (d + SQRT(d*d + e))/b
  d = half*d
  e = -quart*b
  r = xm + one/xm
  w = xm*ym
  a = w**(-half*h) * SQRT(xm/ym) * EXP(-e*(r - ym - one/ym))
  IF (a < vsmall) THEN
     fn_val = vlarge
     return
  END IF
  c = -d*LOG(xm) - e*r
END IF

DO
  CALL RANDOM_NUMBER(r1)
  IF (r1 <= zero) CYCLE
  CALL RANDOM_NUMBER(r2)
  x = a*r2/r1
  IF (x <= zero) CYCLE
  IF (LOG(r1) < d*LOG(x) + e*(x + one/x) + c) EXIT
END DO

fn_val = x

RETURN
END FUNCTION random_inv_gauss




  FUNCTION random_Poisson(mu, first) RESULT(ival)
    

!**********************************************************************
!     Translated to Fortran 90 by Alan Miller from:
!                           RANLIB
!
!     Library of Fortran Routines for Random Number Generation
!
!                    Compiled and Written by:
!
!                         Barry W. Brown
!                          James Lovato
!
!             Department of Biomathematics, Box 237
!             The University of Texas, M.D. Anderson Cancer Center
!             1515 Holcombe Boulevard
!             Houston, TX      77030
!
! This work was supported by grant CA-16672 from the National Cancer Institute.

!                    GENerate POIsson random deviate

!                            Function

! Generates a single random deviate from a Poisson distribution with mean mu.

!                            Arguments

!     mu --> The mean of the Poisson distribution from which
!            a random deviate is to be generated.
!                              REAL mu

!                              Method

!     For details see:

!               Ahrens, J.H. and Dieter, U.
!               Computer Generation of Poisson Deviates
!               From Modified Normal Distributions.
!               ACM Trans. Math. Software, 8, 2
!               (June 1982),163-179

!     TABLES: COEFFICIENTS A0-A7 FOR STEP F. FACTORIALS FACT
!     COEFFICIENTS A(K) - FOR PX = FK*V*V*SUM(A(K)*V**K)-DEL

!     SEPARATION OF CASES A AND B

!     .. Scalar Arguments ..
REAL(kind=sp), INTENT(IN)    :: mu
LOGICAL,       INTENT(IN) :: first
INTEGER(kind=i4)             :: ival
!     ..
!     .. Local Scalars ..
REAL(kind=sp)          :: b1, b2, c, c0, c1, c2, c3, del, difmuk, e, fk, fx, fy, g,  &
                 omega, px, py, t, u, v, x, xx
REAL(kind=sp), SAVE    :: s, d, p, q, p0
#if defined(_OPENMP)
!$omp threadprivate(s,d,p,q,p0)
#endif
INTEGER(kind=i4)       :: j, k, kflag
LOGICAL, SAVE :: full_init
#if defined(_OPENMP)
!$omp threadprivate(full_init)
#endif
INTEGER(kind=i4), SAVE :: l, m
#if defined(_OPENMP)
!$omp threadprivate(l,m)
#endif
!     ..
!     .. Local Arrays ..
REAL(kind=sp), SAVE    :: pp(35)
#if defined(_OPENMP)
!$omp threadprivate(pp)
#endif
!     ..
!     .. Data statements ..
REAL(kind=sp), PARAMETER :: a0 = -.5_sp, a1 = .3333333_sp, a2 = -.2500068_sp, a3 = .2000118_sp,  &
                   a4 = -.1661269_sp, a5 = .1421878_sp, a6 = -.1384794_sp,   &
                   a7 = .1250060_sp

REAL(kind=sp), PARAMETER :: fact(10) = (/ 1._sp, 1._sp, 2._sp, 6._sp, 24._sp, 120._sp, 720._sp, 5040._sp,  &
                                 40320._sp, 362880._sp /)

!     ..
!     .. Executable Statements ..
IF (mu > 10.0_sp) THEN
!     C A S E  A. (RECALCULATION OF S, D, L IF MU HAS CHANGED)

  IF (first) THEN
    s = SQRT(mu)
    d = 6.0_sp*mu*mu

!             THE POISSON PROBABILITIES PK EXCEED THE DISCRETE NORMAL
!             PROBABILITIES FK WHENEVER K >= M(MU). L=IFIX(MU-1.1484)
!             IS AN UPPER BOUND TO M(MU) FOR ALL MU >= 10 .

    l = mu - 1.1484_sp
    full_init = .false.
  END IF


!     STEP N. NORMAL SAMPLE - random_normal() FOR STANDARD NORMAL DEVIATE

  g = mu + s*random_normal()
  IF (g > 0.0_sp) THEN
    ival = g

!     STEP I. IMMEDIATE ACCEPTANCE IF ival IS LARGE ENOUGH

    IF (ival>=l) RETURN

!     STEP S. SQUEEZE ACCEPTANCE - SAMPLE U

    fk = ival
    difmuk = mu - fk
    CALL RANDOM_NUMBER(u)
    IF (d*u >= difmuk*difmuk*difmuk) RETURN
  END IF

!     STEP P. PREPARATIONS FOR STEPS Q AND H.
!             (RECALCULATIONS OF PARAMETERS IF NECESSARY)
!             .3989423=(2*PI)**(-.5)  .416667E-1=1./24.  .1428571=1./7.
!             THE QUANTITIES B1, B2, C3, C2, C1, C0 ARE FOR THE HERMITE
!             APPROXIMATIONS TO THE DISCRETE NORMAL PROBABILITIES FK.
!             C=.1069/MU GUARANTEES MAJORIZATION BY THE 'HAT'-FUNCTION.

  IF (.NOT. full_init) THEN
    omega = .3989423_sp/s
    b1 = .4166667E-1_sp/mu
    b2 = .3_sp*b1*b1
    c3 = .1428571_sp*b1*b2
    c2 = b2 - 15._sp*c3
    c1 = b1 - 6._sp*b2 + 45._sp*c3
    c0 = 1. - b1 + 3._sp*b2 - 15._sp*c3
    c = .1069_sp/mu
    full_init = .true.
  END IF

  IF (g < 0.0_sp) GO TO 50

!             'SUBROUTINE' F IS CALLED (KFLAG=0 FOR CORRECT RETURN)

  kflag = 0
  GO TO 70

!     STEP Q. QUOTIENT ACCEPTANCE (RARE CASE)

  40 IF (fy-u*fy <= py*EXP(px-fx)) RETURN

!     STEP E. EXPONENTIAL SAMPLE - random_exponential() FOR STANDARD EXPONENTIAL
!             DEVIATE E AND SAMPLE T FROM THE LAPLACE 'HAT'
!             (IF T <= -.6744 THEN PK < FK FOR ALL MU >= 10.)

  50 e = random_exponential()
  CALL RANDOM_NUMBER(u)
  u = u + u - one
  t = 1.8_sp + SIGN(e, u)
  IF (t <= (-.6744_sp)) GO TO 50
  ival = mu + s*t
  fk = ival
  difmuk = mu - fk

!             'SUBROUTINE' F IS CALLED (KFLAG=1 FOR CORRECT RETURN)

  kflag = 1
  GO TO 70

!     STEP H. HAT ACCEPTANCE (E IS REPEATED ON REJECTION)

  60 IF (c*ABS(u) > py*EXP(px+e) - fy*EXP(fx+e)) GO TO 50
  RETURN

!     STEP F. 'SUBROUTINE' F. CALCULATION OF PX, PY, FX, FY.
!             CASE ival < 10 USES FACTORIALS FROM TABLE FACT

  70 IF (ival>=10) GO TO 80
  px = -mu
  py = mu**ival/fact(ival+1)
  GO TO 110

!             CASE ival >= 10 USES POLYNOMIAL APPROXIMATION
!             A0-A7 FOR ACCURACY WHEN ADVISABLE
!             .8333333E-1=1./12.  .3989423=(2*PI)**(-.5)

  80 del = .8333333E-1_sp/fk
  del = del - 4.8_sp*del*del*del
  v = difmuk/fk
  IF (ABS(v)>0.25) THEN
    px = fk*LOG(one + v) - difmuk - del
  ELSE
    px = fk*v*v* (((((((a7*v+a6)*v+a5)*v+a4)*v+a3)*v+a2)*v+a1)*v+a0) - del
  END IF
  py = .3989423_sp/SQRT(fk)
  110 x = (half - difmuk)/s
  xx = x*x
  fx = -half*xx
  fy = omega* (((c3*xx + c2)*xx + c1)*xx + c0)
  IF (kflag <= 0) GO TO 40
  GO TO 60

!---------------------------------------------------------------------------
!     C A S E  B.    mu < 10
!     START NEW TABLE AND CALCULATE P0 IF NECESSARY

ELSE
  IF (first) THEN
    m = MAX(1, INT(mu))
    l = 0
    p = EXP(-mu)
    q = p
    p0 = p
  END IF

!     STEP U. UNIFORM SAMPLE FOR INVERSION METHOD

  DO
    CALL RANDOM_NUMBER(u)
    ival = 0
    IF (u <= p0) RETURN

!     STEP T. TABLE COMPARISON UNTIL THE END PP(L) OF THE
!             PP-TABLE OF CUMULATIVE POISSON PROBABILITIES
!             (0.458=PP(9) FOR MU=10)

    IF (l == 0) GO TO 150
    j = 1
    IF (u > 0.458_sp) j = MIN(l, m)
    DO k = j, l
      IF (u <= pp(k)) GO TO 180
    END DO
    IF (l == 35) CYCLE

!     STEP C. CREATION OF NEW POISSON PROBABILITIES P
!             AND THEIR CUMULATIVES Q=PP(K)

    150 l = l + 1
    DO k = l, 35
      p = p*mu / k
      q = q + p
      pp(k) = q
      IF (u <= q) GO TO 170
    END DO
    l = 35
  END DO

  170 l = k
  180 ival = k
  RETURN
END IF

RETURN
END FUNCTION random_Poisson



  FUNCTION random_Poisson_clamped(mu, first) RESULT(ival)
    

!**********************************************************************
!     Translated to Fortran 90 by Alan Miller from:
!                           RANLIB
!
!     Library of Fortran Routines for Random Number Generation
!
!                    Compiled and Written by:
!
!                         Barry W. Brown
!                          James Lovato
!
!             Department of Biomathematics, Box 237
!             The University of Texas, M.D. Anderson Cancer Center
!             1515 Holcombe Boulevard
!             Houston, TX      77030
!
! This work was supported by grant CA-16672 from the National Cancer Institute.

!                    GENerate POIsson random deviate

!                            Function

! Generates a single random deviate from a Poisson distribution with mean mu.

!                            Arguments

!     mu --> The mean of the Poisson distribution from which
!            a random deviate is to be generated.
!                              REAL mu

!                              Method

!     For details see:

!               Ahrens, J.H. and Dieter, U.
!               Computer Generation of Poisson Deviates
!               From Modified Normal Distributions.
!               ACM Trans. Math. Software, 8, 2
!               (June 1982),163-179

!     TABLES: COEFFICIENTS A0-A7 FOR STEP F. FACTORIALS FACT
!     COEFFICIENTS A(K) - FOR PX = FK*V*V*SUM(A(K)*V**K)-DEL

!     SEPARATION OF CASES A AND B

!     .. Scalar Arguments ..
REAL(kind=sp), INTENT(IN)    :: mu
LOGICAL,       INTENT(IN) :: first
INTEGER(kind=i4)             :: ival
!     ..
!     .. Local Scalars ..
REAL(kind=sp)          :: b1, b2, c, c0, c1, c2, c3, del, difmuk, e, fk, fx, fy, g,  &
                 omega, px, py, t, u, v, x, xx
REAL(kind=sp), SAVE    :: s, d, p, q, p0
#if defined(_OPENMP)
!$omp threadprivate(s,d,p,q,p0)
#endif
INTEGER(kind=i4)       :: j, k, kflag
LOGICAL, SAVE :: full_init
#if defined(_OPENMP)
!$omp threadprivate(full_init)
#endif
INTEGER(kind=i4), SAVE :: l, m
#if defined(_OPENMP)
!$omp threadprivate(l,m)
#endif
!     ..
!     .. Local Arrays ..
REAL(kind=sp), SAVE    :: pp(35)
#if defined(_OPENMP)
!$omp threadprivate(pp)
#endif
!     ..
!     .. Data statements ..
REAL(kind=sp), PARAMETER :: a0 = -.5_sp, a1 = .3333333_sp, a2 = -.2500068_sp, a3 = .2000118_sp,  &
                   a4 = -.1661269_sp, a5 = .1421878_sp, a6 = -.1384794_sp,   &
                   a7 = .1250060_sp

REAL(kind=sp), PARAMETER :: fact(10) = (/ 1._sp, 1._sp, 2._sp, 6._sp, 24._sp, 120._sp, 720._sp, 5040._sp,  &
                                 40320._sp, 362880._sp /)

!     ..
!     .. Executable Statements ..
IF (mu > 10.0_sp) THEN
!     C A S E  A. (RECALCULATION OF S, D, L IF MU HAS CHANGED)

  IF (first) THEN
    s = SQRT(mu)
    d = 6.0_sp*mu*mu

!             THE POISSON PROBABILITIES PK EXCEED THE DISCRETE NORMAL
!             PROBABILITIES FK WHENEVER K >= M(MU). L=IFIX(MU-1.1484)
!             IS AN UPPER BOUND TO M(MU) FOR ALL MU >= 10 .

    l = mu - 1.1484_sp
    full_init = .false.
  END IF


!     STEP N. NORMAL SAMPLE - random_normal() FOR STANDARD NORMAL DEVIATE

  g = mu + s*random_normal_clamped()
  IF (g > 0.0_sp) THEN
    ival = g

!     STEP I. IMMEDIATE ACCEPTANCE IF ival IS LARGE ENOUGH

    IF (ival>=l) RETURN

!     STEP S. SQUEEZE ACCEPTANCE - SAMPLE U

    fk = ival
    difmuk = mu - fk
    CALL RANDOM_NUMBER(u)
    IF (d*u >= difmuk*difmuk*difmuk) RETURN
  END IF

!     STEP P. PREPARATIONS FOR STEPS Q AND H.
!             (RECALCULATIONS OF PARAMETERS IF NECESSARY)
!             .3989423=(2*PI)**(-.5)  .416667E-1=1./24.  .1428571=1./7.
!             THE QUANTITIES B1, B2, C3, C2, C1, C0 ARE FOR THE HERMITE
!             APPROXIMATIONS TO THE DISCRETE NORMAL PROBABILITIES FK.
!             C=.1069/MU GUARANTEES MAJORIZATION BY THE 'HAT'-FUNCTION.

  IF (.NOT. full_init) THEN
    omega = .3989423_sp/s
    b1 = .4166667E-1_sp/mu
    b2 = .3_sp*b1*b1
    c3 = .1428571_sp*b1*b2
    c2 = b2 - 15._sp*c3
    c1 = b1 - 6._sp*b2 + 45._sp*c3
    c0 = 1. - b1 + 3._sp*b2 - 15._sp*c3
    c = .1069_sp/mu
    full_init = .true.
  END IF

  IF (g < 0.0_sp) GO TO 50

!             'SUBROUTINE' F IS CALLED (KFLAG=0 FOR CORRECT RETURN)

  kflag = 0
  GO TO 70

!     STEP Q. QUOTIENT ACCEPTANCE (RARE CASE)

  40 IF (fy-u*fy <= py*EXP(px-fx)) RETURN

!     STEP E. EXPONENTIAL SAMPLE - random_exponential() FOR STANDARD EXPONENTIAL
!             DEVIATE E AND SAMPLE T FROM THE LAPLACE 'HAT'
!             (IF T <= -.6744 THEN PK < FK FOR ALL MU >= 10.)

  50 e = random_exponential()
  CALL RANDOM_NUMBER(u)
  u = u + u - one
  t = 1.8_sp + SIGN(e, u)
  IF (t <= (-.6744_sp)) GO TO 50
  ival = mu + s*t
  fk = ival
  difmuk = mu - fk

!             'SUBROUTINE' F IS CALLED (KFLAG=1 FOR CORRECT RETURN)

  kflag = 1
  GO TO 70

!     STEP H. HAT ACCEPTANCE (E IS REPEATED ON REJECTION)

  60 IF (c*ABS(u) > py*EXP(px+e) - fy*EXP(fx+e)) GO TO 50
  RETURN

!     STEP F. 'SUBROUTINE' F. CALCULATION OF PX, PY, FX, FY.
!             CASE ival < 10 USES FACTORIALS FROM TABLE FACT

  70 IF (ival>=10) GO TO 80
  px = -mu
  py = mu**ival/fact(ival+1)
  GO TO 110

!             CASE ival >= 10 USES POLYNOMIAL APPROXIMATION
!             A0-A7 FOR ACCURACY WHEN ADVISABLE
!             .8333333E-1=1./12.  .3989423=(2*PI)**(-.5)

  80 del = .8333333E-1_sp/fk
  del = del - 4.8_sp*del*del*del
  v = difmuk/fk
  IF (ABS(v)>0.25) THEN
    px = fk*LOG(one + v) - difmuk - del
  ELSE
    px = fk*v*v* (((((((a7*v+a6)*v+a5)*v+a4)*v+a3)*v+a2)*v+a1)*v+a0) - del

  END IF
  py = .3989423_sp/SQRT(fk)
  110 x = (half - difmuk)/s
  xx = x*x
  fx = -half*xx
  fy = omega* (((c3*xx + c2)*xx + c1)*xx + c0)
  IF (kflag <= 0) GO TO 40
  GO TO 60

!---------------------------------------------------------------------------
!     C A S E  B.    mu < 10
!     START NEW TABLE AND CALCULATE P0 IF NECESSARY

ELSE
  IF (first) THEN
    m = MAX(1, INT(mu))
    l = 0
    p = EXP(-mu)
    q = p
    p0 = p
  END IF

!     STEP U. UNIFORM SAMPLE FOR INVERSION METHOD

  DO
    CALL RANDOM_NUMBER(u)
    ival = 0
    IF (u <= p0) RETURN

!     STEP T. TABLE COMPARISON UNTIL THE END PP(L) OF THE
!             PP-TABLE OF CUMULATIVE POISSON PROBABILITIES
!             (0.458=PP(9) FOR MU=10)

    IF (l == 0) GO TO 150
    j = 1
    IF (u > 0.458_sp) j = MIN(l, m)
    DO k = j, l
      IF (u <= pp(k)) GO TO 180
    END DO
    IF (l == 35) CYCLE

!     STEP C. CREATION OF NEW POISSON PROBABILITIES P
!             AND THEIR CUMULATIVES Q=PP(K)

    150 l = l + 1
    DO k = l, 35
      p = p*mu / k
      q = q + p
      pp(k) = q
      IF (u <= q) GO TO 170
    END DO
    l = 35
  END DO

  170 l = k
  180 ival = k
  RETURN
END IF

RETURN
END FUNCTION random_Poisson_clamped 




#if defined (__ICC) || defined (__INTEL_COMPILER)
!DIR$ ATTRIBUTES INLINE :: random_binomial1
  
#endif
  FUNCTION random_binomial1(n,p,first)    RESULT(ival)
    
    


! FUNCTION GENERATES A RANDOM BINOMIAL VARIATE USING C.D.Kemp's method.
! This algorithm is suitable when many random variates are required
! with the SAME parameter values for n & p.

!    P = BERNOULLI SUCCESS PROBABILITY
!           (0 <= REAL <= 1)
!    N = NUMBER OF BERNOULLI TRIALS
!           (1 <= INTEGER)
!    FIRST = .TRUE. for the first call using the current parameter values
!          = .FALSE. if the values of (n,p) are unchanged from last call

! Reference: Kemp, C.D. (1986). `A modal method for generating binomial
!            variables', Commun. Statist. - Theor. Meth. 15(3), 805-813.

INTEGER(kind=i4), INTENT(IN) :: n
REAL(kind=sp), INTENT(IN)    :: p
LOGICAL, INTENT(IN) :: first
INTEGER(kind=i4)             :: ival

!     Local variables

INTEGER(kind=i4)         :: ru, rd
INTEGER(kind=i4), SAVE   :: r0
#if defined(_OPENMP)
!$omp threadprivate(r0)
#endif
REAL(kind=sp)            :: u, pd, pu
REAL(kind=sp), SAVE      :: odds_ratio, p_r
#if defined(_OPENMP)
!$omp threadprivate(odds_ratio,p_r)
#endif
REAL(kind=sp), PARAMETER :: zero = 0.0_sp, one = 1.0_sp

IF (first) THEN
  r0 = (n+1)*p
  p_r = bin_prob(n, p, r0)
  odds_ratio = p / (one - p)
END IF

CALL RANDOM_NUMBER(u)
u = u - p_r
IF (u < zero) THEN
  ival = r0
  RETURN
END IF

pu = p_r
ru = r0
pd = p_r
rd = r0
DO
  rd = rd - 1
  IF (rd >= 0) THEN
    pd = pd * (rd+1) / (odds_ratio * (n-rd))
    u = u - pd
    IF (u < zero) THEN
      ival = rd
      RETURN
    END IF
  END IF

  ru = ru + 1
  IF (ru <= n) THEN
    pu = pu * (n-ru+1) * odds_ratio / ru
    u = u - pu
    IF (u < zero) THEN
      ival = ru
      RETURN
    END IF
  END IF
END DO

!     This point should not be reached, but just in case:

ival = r0
RETURN

END FUNCTION random_binomial1



#if defined (__ICC) || defined (__INTEL_COMPILER)

    !DIR$ ATTRIBUTES INLINE :: bin_prob
#endif
  FUNCTION bin_prob(n,p,r) RESULT(fn_val)
   

!     Calculate a binomial probability

INTEGER(kind=i4), INTENT(IN) :: n, r
REAL(kind=sp), INTENT(IN)    :: p
REAL(kind=sp)                :: fn_val

!     Local variable
REAL                :: one = 1.0

fn_val = EXP( lngamma(DBLE(n+1)) - lngamma(DBLE(r+1)) - lngamma(DBLE(n-r+1)) &
              + r*LOG(p) + (n-r)*LOG(one - p) )
RETURN

END FUNCTION bin_prob



#if defined __ICC || defined __INTEL_COMPILER
 !DIR$ ATTRIBUTES INLINE :: lngamma
    
#endif
  FUNCTION lngamma(x) RESULT(fn_val)
   

! Logarithm to base e of the gamma function.
!
! Accurate to about 1.e-14.
! Programmer: Alan Miller

! Latest revision of Fortran 77 version - 28 February 1988

REAL (kind=dp), INTENT(IN) :: x
REAL (kind=dp)             :: fn_val

!       Local variables

REAL (kind=dp) :: a1 = -4.166666666554424D-02, a2 = 2.430554511376954D-03,  &
             a3 = -7.685928044064347D-04, a4 = 5.660478426014386D-04,  &
             temp, arg, product, lnrt2pi = 9.189385332046727D-1,       &
             pi = 3.141592653589793D0
LOGICAL   :: reflect

!       lngamma is not defined if x = 0 or a negative integer.

IF (x > 0.d0) GO TO 10
IF (x /= INT(x)) GO TO 10
fn_val = 0.d0
RETURN

!       If x < 0, use the reflection formula:
!               gamma(x) * gamma(1-x) = pi * cosec(pi.x)

10 reflect = (x < 0.d0)
IF (reflect) THEN
  arg = 1.d0 - x
ELSE
  arg = x
END IF

!       Increase the argument, if necessary, to make it > 10.

product = 1.d0
20 IF (arg <= 10.d0) THEN
  product = product * arg
  arg = arg + 1.d0
  GO TO 20
END IF

!  Use a polynomial approximation to Stirling's formula.
!  N.B. The real Stirling's formula is used here, not the simpler, but less
!       accurate formula given by De Moivre in a letter to Stirling, which
!       is the one usually quoted.

arg = arg - 0.5D0
temp = 1.d0/arg**2
fn_val = lnrt2pi + arg * (LOG(arg) - 1.d0 + &
                  (((a4*temp + a3)*temp + a2)*temp + a1)*temp) - LOG(product)
IF (reflect) THEN
  temp = SIN(pi * x)
  fn_val = LOG(pi/temp) - fn_val
END IF
RETURN
END FUNCTION lngamma




  FUNCTION random_binomial2(n,pp,first) RESULT(ival)
   
    

!**********************************************************************
!     Translated to Fortran 90 by Alan Miller from:
!                              RANLIB
!
!     Library of Fortran Routines for Random Number Generation
!
!                      Compiled and Written by:
!
!                           Barry W. Brown
!                            James Lovato
!
!               Department of Biomathematics, Box 237
!               The University of Texas, M.D. Anderson Cancer Center
!               1515 Holcombe Boulevard
!               Houston, TX      77030
!
! This work was supported by grant CA-16672 from the National Cancer Institute.

!                    GENerate BINomial random deviate

!                              Function

!     Generates a single random deviate from a binomial
!     distribution whose number of trials is N and whose
!     probability of an event in each trial is P.

!                              Arguments

!     N  --> The number of trials in the binomial distribution
!            from which a random deviate is to be generated.
!                              INTEGER N

!     P  --> The probability of an event in each trial of the
!            binomial distribution from which a random deviate
!            is to be generated.
!                              REAL P

!     FIRST --> Set FIRST = .TRUE. for the first call to perform initialization
!               the set FIRST = .FALSE. for further calls using the same pair
!               of parameter values (N, P).
!                              LOGICAL FIRST

!     random_binomial2 <-- A random deviate yielding the number of events
!                from N independent trials, each of which has
!                a probability of event P.
!                              INTEGER random_binomial

!                              Method

!     This is algorithm BTPE from:

!         Kachitvichyanukul, V. and Schmeiser, B. W.
!         Binomial Random Variate Generation.
!         Communications of the ACM, 31, 2 (February, 1988) 216.

!**********************************************************************

!*****DETERMINE APPROPRIATE ALGORITHM AND WHETHER SETUP IS NECESSARY

!     ..
!     .. Scalar Arguments ..
REAL(kind=sp), INTENT(IN)    :: pp
INTEGER(kind=i4), INTENT(IN) :: n
LOGICAL, INTENT(IN) :: first
INTEGER(kind=i4)            :: ival
!     ..
!     .. Local Scalars ..
REAL(kind=sp)            :: alv, amaxp, f, if1, f1, if2,f2, u, v, iw,w, iw2,w2, x, ix1,x1, ix2,x2, ynorm, iz,z, iz2,z2
REAL(kind=sp), PARAMETER :: zero = 0.0_sp, half = 0.5_sp, one = 1.0_sp
REAL(kind=sp), PARAMETER :: C000000601250601250601251 = 0.00000601250601250601251_sp
INTEGER(kind=i4)         :: i, ix, k, mp
INTEGER(kind=i4), SAVE   :: m
#if defined(_OPENMP)
!$omp threadprivate(m)
#endif
REAL(kind=sp), SAVE      :: p, q, xnp, ffm, fm, xnpq, p1, xm, xl, xr, c, al, xll,  &
                   xlr, p2, p3, p4, qn, r, g
#if defined(_OPENMP)
!$omp threadprivate(p, q, xnp, ffm, fm, xnpq, p1, xm, xl, xr, c, al, xll)
!$omp threadprivate(xlr, p2, p3, p4, qn, r, g)
#endif

!     ..
!     .. Executable Statements ..

!*****SETUP, PERFORM ONLY WHEN PARAMETERS CHANGE

IF (first) THEN
  p = MIN(pp, one-pp)
  q = one - p
  xnp = n * p
END IF

IF (xnp > 30._sp) THEN
  IF (first) THEN
    ffm = xnp + p
    m = ffm
    fm = m
    xnpq = xnp * q
    p1 = INT(2.195_sp*SQRT(xnpq) - 4.6_sp*q) + half
    xm = fm + half
    xl = xm - p1
    xr = xm + p1
    c = 0.134_sp + 20.5_sp / (15.3_sp + fm)
    al = (ffm-xl) / (ffm - xl*p)
    xll = al * (one + half*al)
    al = (xr - ffm) / (xr*q)
    xlr = al * (one + half*al)
    p2 = p1 * (one + c + c)
    p3 = p2 + c / xll
    p4 = p3 + c / xlr
  END IF

!*****GENERATE VARIATE, Binomial mean at least 30.

  20 CALL RANDOM_NUMBER(u)
  u = u * p4
  CALL RANDOM_NUMBER(v)

!     TRIANGULAR REGION

  IF (u <= p1) THEN
    ix = xm - p1 * v + u
    GO TO 110
  END IF

!     PARALLELOGRAM REGION

  IF (u <= p2) THEN
    x = xl + (u-p1) / c
    v = v * c + one - ABS(xm-x) / p1
    IF (v > one .OR. v <= zero) GO TO 20
    ix = x
  ELSE

!     LEFT TAIL

    IF (u <= p3) THEN
      ix = xl + LOG(v) / xll
      IF (ix < 0) GO TO 20
      v = v * (u-p2) * xll
    ELSE

!     RIGHT TAIL

      ix = xr - LOG(v) / xlr
      IF (ix > n) GO TO 20
      v = v * (u-p3) * xlr
    END IF
  END IF

!*****DETERMINE APPROPRIATE WAY TO PERFORM ACCEPT/REJECT TEST

  k = ABS(ix-m)
  IF (k <= 20 .OR. k >= xnpq/2-1) THEN

!     EXPLICIT EVALUATION

    f = one
    r = p / q
    g = (n+1) * r
    IF (m < ix) THEN
      mp = m + 1
      DO i = mp, ix
        f = f * (g/i-r)
      END DO

    ELSE IF (m > ix) THEN
      ix1 = ix + 1
      DO i = ix1, m
        f = f / (g/i-r)
      END DO
    END IF

    IF (v > f) THEN
      GO TO 20
    ELSE
      GO TO 110
    END IF
  END IF

!     SQUEEZING USING UPPER AND LOWER BOUNDS ON LOG(F(X))

  amaxp = (k/xnpq) * ((k*(k/3. + .625) + .1666666666666_sp)/xnpq + half)
  ynorm = -k * k / (2._sp*xnpq)
  alv = LOG(v)
  IF (alv<ynorm - amaxp) GO TO 110
  IF (alv>ynorm + amaxp) GO TO 20

!     STIRLING'S (actually de Moivre's) FORMULA TO MACHINE ACCURACY FOR
!     THE FINAL ACCEPTANCE/REJECTION TEST

  x1 = ix + 1
  ix1= 1.0_sp/x1
  f1 = fm + one
  if1= 1.0_sp/f1
  z  = n + 1 - fm
  iz = 1.0_sp/z
  w  = n - ix + one
  iw = 1.0_sp/w
  z2 = z * z
  iz2 = 1.0_sp/z2
  x2  = x1 * x1
  ix2 = 1.0_sp/x2
  f2  = f1 * f1
  if2 = 1.0_sp/f2
  w2  = w * w
  iw2 = 1.0_sp/w2
#if 0
  IF (alv - (xm*LOG(f1/x1) + (n-m+half)*LOG(z/w) + (ix-m)*LOG(w*p/(x1*q)) +    &
      (13860._sp-(462._sp-(132._sp-(99._sp-140._sp/f2)/f2)/f2)/f2)/f1/166320._sp +               &
      (13860._sp-(462._sp-(132._sp-(99._sp-140._sp/z2)/z2)/z2)/z2)/z/166320._sp +                &
      (13860._sp-(462._sp-(132._sp-(99._sp-140._sp/x2)/x2)/x2)/x2)/x1/166320._sp +               &
      (13860._sp-(462._sp-(132._sp-(99._sp-140._sp/w2)/w2)/w2)/w2)/w/166320._sp) > zero) THEN
    GO TO 20
  ELSE
    GO TO 110
  END IF
#endif

 IF (alv - (xm*LOG(f1/x1) + (n-m+half)*LOG(z/w) + (ix-m)*LOG(w*p/(x1*q)) +    &
      (13860._sp-(462._sp-(132._sp-(99._sp-140._sp*if2)*if2)*if2)*if2)*if1*C000000601250601250601251 +    &
      (13860._sp-(462._sp-(132._sp-(99._sp-140._sp*iz2)*iz2)*iz2)*iz2)*iz*C000000601250601250601251  +    &
      (13860._sp-(462._sp-(132._sp-(99._sp-140._sp*ix2)*ix2)*ix2)*ix2)*ix1*C000000601250601250601251 +    &
      (13860._sp-(462._sp-(132._sp-(99._sp-140._sp*iw2)*iw2)*iw2)*iw2)*iw*C000000601250601250601251) > zero) THEN
    GO TO 20
  ELSE
    GO TO 110
  END IF

ELSE
!     INVERSE CDF LOGIC FOR MEAN LESS THAN 30
  IF (first) THEN
    qn = q ** n
    r = p / q
    g = r * (n+1)
  END IF

  90 ix = 0
  f = qn
  CALL RANDOM_NUMBER(u)
  100 IF (u >= f) THEN
    IF (ix > 110) GO TO 90
    u = u - f
    ix = ix + 1
    f = f * (g/ix - r)
    GO TO 100
  END IF
END IF

110 IF (pp > half) ix = n - ix
ival = ix
RETURN

END FUNCTION random_binomial2




#if defined (__ICC) || defined (__INTEL_COMPILER)
 !DIR$ ATTRIBUTES INLINE :: random_neg_binomial
  
#endif
  FUNCTION random_neg_binomial(sk, p) RESULT(ival)
   

! Adapted from Fortran 77 code from the book:
!     Dagpunar, J. 'Principles of random variate generation'
!     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

! FUNCTION GENERATES A RANDOM NEGATIVE BINOMIAL VARIATE USING UNSTORED
! INVERSION AND/OR THE REPRODUCTIVE PROPERTY.

!    SK = NUMBER OF FAILURES REQUIRED (Dagpunar's words!)
!       = the `power' parameter of the negative binomial
!           (0 < REAL)
!    P = BERNOULLI SUCCESS PROBABILITY
!           (0 < REAL < 1)

! THE PARAMETER H IS SET SO THAT UNSTORED INVERSION ONLY IS USED WHEN P <= H,
! OTHERWISE A COMBINATION OF UNSTORED INVERSION AND
! THE REPRODUCTIVE PROPERTY IS USED.

REAL(kind=sp), INTENT(IN)   :: sk, p
INTEGER(kind=i4)            :: ival

!     Local variables
! THE PARAMETER ULN = -LOG(MACHINE'S SMALLEST REAL NUMBER).

REAL(kind=sp), PARAMETER    :: h = 0.7_sp
REAL(kind=sp)              :: q, x, st, uln, v, r, s, y, g
INTEGER(kind=i4)            :: k, i, n



q = one - p
x = zero
st = sk
IF (p > h) THEN
  v = one/LOG(p)
  k = st
  DO i = 1,k
    DO
      CALL RANDOM_NUMBER(r)
      IF (r > zero) EXIT
    END DO
    n = v*LOG(r)
    x = x + n
  END DO
  st = st - k
END IF

s = zero
uln = -LOG(vsmall)
IF (st > -uln/LOG(q)) THEN
   ival = -1
   return
END IF

y = q**st
g = st
CALL RANDOM_NUMBER(r)
DO
  IF (y > r) EXIT
  r = r - y
  s = s + one
  y = y*p*g/s
  g = g + one
END DO

ival = x + s + half
RETURN
END FUNCTION random_neg_binomial


#if defined (__ICC) || defined (__INTEL_COMPILER)
 !DIR$ ATTRIBUTES INLINE :: random_von_Mises
    
#endif
FUNCTION random_von_Mises(k, first) RESULT(fn_val)

!     Algorithm VMD from:
!     Dagpunar, J.S. (1990) `Sampling from the von Mises distribution via a
!     comparison of random numbers', J. of Appl. Statist., 17, 165-168.

!     Fortran 90 code by Alan Miller
!     CSIRO Division of Mathematical & Information Sciences

!     Arguments:
!     k (real)        parameter of the von Mises distribution.
!     first (logical) set to .TRUE. the first time that the function
!                     is called, or the first time with a new value
!                     for k.   When first = .TRUE., the function sets
!                     up starting values and may be very much slower.

REAL, INTENT(IN)     :: k
LOGICAL, INTENT(IN)  :: first
REAL                 :: fn_val

!     Local variables

INTEGER          :: j, n
INTEGER, SAVE    :: nk
#if defined(_OPENMP)
!$omp threadprivate(nk)
#endif
REAL, PARAMETER  :: pi = 3.14159265_sp
REAL, SAVE       :: p(20), theta(0:20)
#if defined(_OPENMP)
!$omp threadprivate(p,theta)
#endif
REAL             :: sump, r, th, lambda, rlast
REAL (dp)        :: dk

IF (first) THEN                        ! Initialization, if necessary
  IF (k < zero) THEN
    !WRITE(*, *) '** Error: argument k for random_von_Mises = ', k
    RETURN
  END IF

  nk = k + k + one
  IF (nk > 20) THEN
    !WRITE(*, *) '** Error: argument k for random_von_Mises = ', k
    RETURN
  END IF

  dk = k
  theta(0) = zero
  IF (k > half) THEN

!     Set up array p of probabilities.

    sump = zero
    DO j = 1, nk
      IF (j < nk) THEN
        theta(j) = ACOS(one - j/k)
      ELSE
        theta(nk) = pi
      END IF

!     Numerical integration of e^[k.cos(x)] from theta(j-1) to theta(j)

      CALL integral(theta(j-1), theta(j), p(j), dk)
      sump = sump + p(j)
    END DO
    p(1:nk) = p(1:nk) / sump
  ELSE
    p(1) = one
    theta(1) = pi
  END IF                         ! if k > 0.5
END IF                           ! if first

CALL RANDOM_NUMBER(r)
DO j = 1, nk
  r = r - p(j)
  IF (r < zero) EXIT
END DO
r = -r/p(j)

DO
  th = theta(j-1) + r*(theta(j) - theta(j-1))
  lambda = k - j + one - k*COS(th)
  n = 1
  rlast = lambda

  DO
    CALL RANDOM_NUMBER(r)
    IF (r > rlast) EXIT
    n = n + 1
    rlast = r
  END DO

  IF (n .NE. 2*(n/2)) EXIT         ! is n even?
  CALL RANDOM_NUMBER(r)
END DO

fn_val = SIGN(th, (r - rlast)/(one - rlast) - half)
RETURN
END FUNCTION random_von_Mises


#if defined (__ICC) || defined (__INTEL_COMPILER)
!DIR$ ATTRIBUTES INLINE :: integral
  
#endif
SUBROUTINE integral(a, b, result, dk)

!     Gaussian integration of exp(k.cosx) from a to b.

REAL (dp), INTENT(IN) :: dk
REAL, INTENT(IN)      :: a, b
REAL, INTENT(OUT)     :: result

!     Local variables

REAL (dp)  :: xmid, range, x1, x2,                                    &
  x(3) = (/0.238619186083197_dp, 0.661209386466265_dp, 0.932469514203152_dp/), &
  w(3) = (/0.467913934572691_dp, 0.360761573048139_dp, 0.171324492379170_dp/)
INTEGER    :: i

xmid = (a + b)/2._dp
range = (b - a)/2._dp

result = 0._dp
DO i = 1, 3
  x1 = xmid + x(i)*range
  x2 = xmid - x(i)*range
  result = result + w(i)*(EXP(dk*COS(x1)) + EXP(dk*COS(x2)))
END DO

result = result * range
!RETURN
END SUBROUTINE integral



#if defined (__ICC) || defined (__INTEL_COMPILER)
!DIR$ ATTRIBUTES INLINE :: random_Cauchy
   
#endif
  FUNCTION random_Cauchy() RESULT(fn_val)
    

!     Generate a random deviate from the standard Cauchy distribution

REAL(kind=sp)    :: fn_val

!     Local variables
REAL(kind=sp)     :: v(2)

DO
  CALL RANDOM_NUMBER(v)
  v = two*(v - half)
  IF (ABS(v(2)) < vsmall) CYCLE               ! Test for zero
  IF (v(1)**2 + v(2)**2 < one) EXIT
END DO
fn_val = v(1) / v(2)

RETURN
END FUNCTION random_Cauchy



SUBROUTINE random_order(order, n)

!     Generate a random ordering of the integers 1 ... n.

INTEGER(kind=i4), INTENT(IN)  :: n
INTEGER(kind=i4), INTENT(OUT) :: order(n)

!     Local variables

INTEGER(kind=i4) :: i, j, k
REAL(kind=sp)    :: wk

DO i = 1, n
  order(i) = i
END DO

!     Starting at the end, swap the current last indicator with one
!     randomly chosen from those preceeding it.

DO i = n, 2, -1
  CALL RANDOM_NUMBER(wk)
  j = 1 + i * wk
  IF (j < i) THEN
    k = order(i)
    order(i) = order(j)
    order(j) = k
  END IF
END DO

RETURN
END SUBROUTINE random_order



!SUBROUTINE seed_random_number(iounit)

!INTEGER, INTENT(IN)  :: iounit

! Local variables

!INTEGER              :: k
!INTEGER, ALLOCATABLE :: seed(:)

!CALL RANDOM_SEED(SIZE=k)
!ALLOCATE( seed(k) )

!WRITE(*, '(a, i2, a)')' Enter ', k, ' integers for random no. seeds: '
!READ(*, *) seed
!WRITE(iounit, '(a, (7i10))') ' Random no. seeds: ', seed
!CALL RANDOM_SEED(PUT=seed)

!DEALLOCATE( seed )

!RETURN
!END SUBROUTINE seed_random_number


END MODULE rand_scalar_distributions
