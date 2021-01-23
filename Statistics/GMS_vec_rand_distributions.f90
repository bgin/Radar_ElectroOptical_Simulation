MODULE mod_vec_rand_distr
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
!     
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



!     Author: Alan Miller
  !     e-mail: amiller @ bigpond.net.au
  ! Explicitly vectorized by Bernard Gingold on 22-01-2020, contact: beniekg@gmail.com
!   Some functions were not explicitly vectorized and they were removed.

  use mod_kinds, only : i4, sp, dp
  use mod_vectypes
  use mod_vecconsts
IMPLICIT NONE


CONTAINS


!FUNCTION random_normal() RESULT(fn_val)

! Adapted from the following Fortran 77 code
!      ALGORITHM 712, COLLECTED ALGORITHMS FROM ACM.
!      THIS WORK PUBLISHED IN TRANSACTIONS ON MATHEMATICAL SOFTWARE,
!      VOL. 18, NO. 4, DECEMBER, 1992, PP. 434-435.

!  The function random_normal() returns a normally distributed pseudo-random
!  number with zero mean and unit variance.

!  The algorithm uses the ratio of uniforms method of A.J. Kinderman
!  and J.F. Monahan augmented with quadratic bounding curves.

!REAL :: fn_val

!     Local variables
!REAL     :: s = 0.449871, t = -0.386595, a = 0.19600, b = 0.25472,    &
!            r1 = 0.27597, r2 = 0.27846, u, v, x, y, q

!     Generate P = (u,v) uniform in rectangle enclosing acceptance region

!DO
!  CALL RANDOM_NUMBER(u)
!  CALL RANDOM_NUMBER(v)
!  v = 1.7156 * (v - half)

!     Evaluate the quadratic form
!  x = u - s
!  y = ABS(v) - t
!  q = x**2 + y*(a*y - b*x)

!     Accept P if inside inner ellipse
!  IF (q < r1) EXIT
!     Reject P if outside outer ellipse
!  IF (q > r2) CYCLE
!     Reject P if outside acceptance region
!  IF (v**2 < -4.0*LOG(u)*u**2) EXIT
!END DO

!     Return ratio of P's coordinates as the normal deviate
!fn_val = v/u
!RETURN

!END FUNCTION random_normal

#endif

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
function random_normal_ymm8r4() result(vrand) !GCC$ ATTRIBUTES hot :: random_normal_ymm8r4 !GCC$ ATTRIBUTES aligned(32) :: random_normal_ymm8r4 !GCC$ ATTRIBUTES inline :: random_normal_ymm8r4
#elif defined __ICC || defined __INTEL_COMPILER
function random_normal_ymm8r4() result(vrand)
  !DIR$ ATTRIBUTES INLINE :: random_normal_ymm8r4
  !DIR$ ATTRIBUTES ALIGN : 32 :: random_normal_ymm8r4
  !DIR$ ATTRIBUTES VECTOR :: random_normal_ymm8r4
#endif
  ! Adapted from the following Fortran 77 code
!      ALGORITHM 712, COLLECTED ALGORITHMS FROM ACM.
!      THIS WORK PUBLISHED IN TRANSACTIONS ON MATHEMATICAL SOFTWARE,
!      VOL. 18, NO. 4, DECEMBER, 1992, PP. 434-435.

!  The function random_normal() returns a normally distributed pseudo-random
!  number with zero mean and unit variance.

!  The algorithm uses the ratio of uniforms method of A.J. Kinderman
  !  and J.F. Monahan augmented with quadratic bounding curves.
  type(YMM8r4_t) :: vrand
  ! Locals
  type(YMM8r4_t), automatic :: s,t,a,b,r1,r2,u,v,x,y,q,t0
  ! Exec code
  s.v  = 0.449871_sp
  t.v  = -0.386595_sp
  a.v  = 0.19600_sp
  b.v  = 0.25472_sp
  r1.v = 0.27597_sp
  r2.v = 0.27846_sp
  u.v  = v8r4_n0.v
  v.v  = v8r4_n0.v
  x.v  = v8r4_n0.v
  y.v  = v8r4_n0.v
  q.v  = v8r4_n0.v
  t0.v = 1.7156_sp
  do
     call random_number(u.v)
     call random_number(v.v)
     v.v = t0.v*(v.v-v8r4_half)
     !     Evaluate the quadratic form
     x.v = u.v-s.v
     y.v = abs(v.v)-t.v
     q.v = x.v*x.v+y.v*(a.v*y.v-b.v*x.v)
     !     Accept P if inside inner ellipse
     if(all(q.v < r1.v)) exit
     !     Reject P if outside outer ellipse
     if(all(q.v > r2.v) exit
     !     Reject P if outside acceptance region
     if(all(v.v*v.v < v8r4_neg4*log(u.v)*u.v*u.v)) exit
  end do
  !     Return ratio of P's coordinates as the normal deviate
  vrand = v8r4_n0
  vrand = v.v/u.v
end function random_normal_ymm8r4

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
function random_normal_zmm16r4() result(vrand) !GCC$ ATTRIBUTES hot :: random_normal_zmm16r4 !GCC$ ATTRIBUTES aligned(32) :: random_normal_zmm16r4 !GCC$ ATTRIBUTES inline :: random_normal_zmm16r4
#elif defined __ICC || defined __INTEL_COMPILER
function random_normal_zmm16r4() result(vrand)
  !DIR$ ATTRIBUTES INLINE :: random_normal_ymm8r4
  !DIR$ ATTRIBUTES ALIGN : 32 :: random_normal_ymm8r4
  !DIR$ ATTRIBUTES VECTOR :: random_normal_ymm8r4
#endif
  ! Adapted from the following Fortran 77 code
!      ALGORITHM 712, COLLECTED ALGORITHMS FROM ACM.
!      THIS WORK PUBLISHED IN TRANSACTIONS ON MATHEMATICAL SOFTWARE,
!      VOL. 18, NO. 4, DECEMBER, 1992, PP. 434-435.

!  The function random_normal() returns a normally distributed pseudo-random
!  number with zero mean and unit variance.

!  The algorithm uses the ratio of uniforms method of A.J. Kinderman
  !  and J.F. Monahan augmented with quadratic bounding curves.
  type(ZMM16r4_t) :: vrand
  ! Locals
  type(ZMM16r4_t), automatic :: s,t,a,b,r1,r2,u,v,x,y,q,t0
  ! Exec code
  s.v  = 0.449871_sp
  t.v  = -0.386595_sp
  a.v  = 0.19600_sp
  b.v  = 0.25472_sp
  r1.v = 0.27597_sp
  r2.v = 0.27846_sp
  u.v  = v16r4_n0.v
  v.v  = v16r4_n0.v
  x.v  = v16r4_n0.v
  y.v  = v16r4_n0.v
  q.v  = v16r4_n0.v
  t0.v = 1.7156_sp
  do
     call random_number(u.v)
     call random_number(v.v)
     v.v = t0.v*(v.v-v16r4_half)
     !     Evaluate the quadratic form
     x.v = u.v-s.v
     y.v = abs(v.v)-t.v
     q.v = x.v*x.v+y.v*(a.v*y.v-b.v*x.v)
     !     Accept P if inside inner ellipse
     if(all(q.v < r1.v)) exit
     !     Reject P if outside outer ellipse
     if(all(q.v > r2.v) exit
     !     Reject P if outside acceptance region
     if(all(v.v*v.v < v16r4_neg4*log(u.v)*u.v*u.v)) exit
  end do
  !     Return ratio of P's coordinates as the normal deviate
  vrand = v16r4_n0
  vrand = v.v/u.v
end function random_normal_zmm16r4



#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
function random_gamma_ymm8r4(s,first) result(vrand) !GCC$ ATTRIBUTES hot :: random_gamma_ymm8r4 !GCC$ ATTRIBUTES aligned(32) :: random_gamma_ymm8r4 !GCC$ ATTRIBUTES inline :: random_gamma_ymm8r4
#elif defined __ICC || defined __INTEL_COMPILER
function random_gamma_ymm8r4(s,first) result(vrand)
  !DIR$ ATTRIBUTES INLINE :: random_gamma_ymm8r4
  !DIR$ ATTRIBUTES ALIGN : 32 :: random_gamma_ymm8r4
  !DIR$ ATTRIBUTES VECTOR :: random_gamma_ymm8r4
#endif
  type(YMM8r4_t),     intent(in) :: s
  logical(kind=i4),   intent(in) :: first
  type(YMM8r4_t) :: vrand
  ! Exec code ...
  if(all(s.v > v8r4_n1.v)) then
     vrand.v = random_gamma1_ymm8r4(s,first)
  else if(all(s.v < v8r4_n1.v)) then
     vrand.v = random_gamma2_ymm8r4(s,first)
  else
     vrand.v = random_exponential_ymm8r4()
  end if
end function random_gamma_ymm8r4


#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
function random_gamma_zmm16r4(s,first) result(vrand) !GCC$ ATTRIBUTES hot :: random_gamma_zmm16r4 !GCC$ ATTRIBUTES aligned(32) :: random_gamma_zmm16r4 !GCC$ ATTRIBUTES inline :: random_gamma_zmm16r4
#elif defined __ICC || defined __INTEL_COMPILER
function random_gamma_zmm16r4(s,first) result(vrand)
  !DIR$ ATTRIBUTES INLINE :: random_gamma_zmm16r4
  !DIR$ ATTRIBUTES ALIGN : 32 :: random_gamma_zmm16r4
  !DIR$ ATTRIBUTES VECTOR :: random_gamma_zmm16r4
#endif
  type(ZMM16r4_t),     intent(in) :: s
  logical(kind=i4),   intent(in) :: first
  type(ZMM16r4_t) :: vrand
  ! Exec code ...
  if(all(s.v > v16r4_n1.v)) then
     vrand.v = random_gamma1_zmm16r4(s,first)
  else if(all(s.v < v16r4_n1.v)) then
     vrand.v = random_gamma2_zmm16r4(s,first)
  else
     vrand.v = random_exponential_zmm16r4()
  end if
end function random_gamma_zmm16r4






#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
function random_gamma1_ymm8r4(s,first) result(vrand) !GCC$ ATTRIBUTES hot :: random_gamma1_ymm8r4 !GCC$ ATTRIBUTES inline :: random_gamma1_ymm8r4 !GCC$ ATTRIBUTES aligned(32) :: random_gamma1_ymm8r4
#elif defined __ICC || defined __INTEL_COMPILER
function random_gamma1_ymm8r4(s,first)
  !DIR$ ATTRIBUTES INLINE :: random_gamma1_ymm8r4
  !DIR$ ATTRIBUTES ALIGN : 32 :: random_gamma1_ymm8r4
  !DIR$ ATTRIBUTES VECTOR :: random_gamma1_ymm8r4
#endif
  type(YMM8r4_t),   intent(in) :: s
  logical,          intent(in) :: first
  type(YMM8r4_t) :: vrand
  ! Locals
  type(YMM8r4_t), save :: c,d
  type(YMM8r4_t), automatic :: u,v,x
  type(YMM8r4_t), parameter :: c0 = YMM8r4_t(0.0331_sp)
  ! Exec code
  c.v = v8r4_n0.v
  d.v = v8r4_n0.v
  u.v = v8r4_n0.v
  v.v = v8r4_n0.v
  x.v = v8r4_n0.v
  if(first) then
     d.v = s.v-v8r4_1over3.v
     c.v = v8r4_one.v/sqrt(v8r4_n9.v*d.v)
  end if
  do
     ! Generate v = (1+cx)^3 where x is random normal; repeat if v <= 0.

     do
        x.v = random_normal_ymm8r4()
        v.v = (v8r4_one.v+c.v*x.v)**3
        if(all(v.v) > v8r4_n0.v) exit
     end do
     ! Generate uniform variable U
     call random_number(u.v)
     where(u.v < v8r4_n1.v-c0.v*x.v**4)
        vrand.v = d.v/v.v
        exit
     elsewhere(log(u.v) < v8r4_half.v*x.v**2+d.v*(v8r4_n1.v-v.v+log(v.v)))
        vrand.v = d.v/v.v
     end where
  end do
end function random_gamma1_ymm8r4

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
function random_gamma1_zmm16r4(s,first) result(vrand) !GCC$ ATTRIBUTES hot :: random_gamma1_zmm16r4 !GCC$ ATTRIBUTES inline :: random_gamma1_zmm16r4 !GCC$ ATTRIBUTES aligned(32) :: random_gamma1_zmm16r4
#elif defined __ICC || defined __INTEL_COMPILER
function random_gamma1_zmm16r4(s,first)
  !DIR$ ATTRIBUTES INLINE :: random_gamma1_zmm16r4
  !DIR$ ATTRIBUTES ALIGN : 32 :: random_gamma1_zmm16r4
  !DIR$ ATTRIBUTES VECTOR :: random_gamma1_zmm16r4
#endif
  type(ZMM16r4_t),   intent(in) :: s
  logical,          intent(in) :: first
  type(ZMM16r4_t) :: vrand
  ! Locals
  type(ZMM16r4_t), save :: c,d
  type(ZMM16r4_t), automatic :: u,v,x
  type(ZMM16r4_t), parameter :: c0 = ZMM16r4_t(0.0331_sp)
  ! Exec code
  c.v = v16r4_n0.v
  d.v = v16r4_n0.v
  u.v = v16r4_n0.v
  v.v = v16r4_n0.v
  x.v = v16r4_n0.v
  if(first) then
     d.v = s.v-v16r4_1over3.v
     c.v = v16r4_one.v/sqrt(v16r4_n9.v*d.v)
  end if
  do
     ! Generate v = (1+cx)^3 where x is random normal; repeat if v <= 0.

     do
        x.v = random_normal_zmm16r4()
        v.v = (v16r4_one.v+c.v*x.v)**3
        if(all(v.v) > v16r4_n0.v) exit
     end do
     ! Generate uniform variable U
     call random_number(u.v)
     where(u.v < v16r4_n1.v-c0.v*x.v**4)
        vrand.v = d.v/v.v
        exit
     elsewhere(log(u.v) < v16r4_half.v*x.v**2+d.v*(v16r4_n1.v-v.v+log(v.v)))
        vrand.v = d.v/v.v
     end where
  end do
end function random_gamma1_zmm16r4
  
  

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
function random_gamma2_ymm8r4(s,first) result(vrand) !GCC$ ATTRIBUTES hot :: random_gamma2_ymm8r4 !GCC$ ATTRIBUTES inline :: random_gamma2_ymm8r4 !GCC$ ATTRIBUTES aligned(32) :: random_gamma2_ymm8r4 
#elif defined __ICC || defined __INTEL_COMPILER
function random_gamma2_ymm8r4(s,first) result(vrand)
  !DIR$ ATTRIBUTES INLINE :: random_gamma2_ymm8r4
  !DIR$ ATTRIBUTES ALIGN : 32 :: random_gamma2_ymm8r4
  !DIR$ ATTRIBUTES VECTOR :: random_gamma2_ymm8r4
#endif
  type(YMM8r4_t),      intent(in) :: s
  logical(kind=i4),    intent(in) :: first
  type(YMM8r4_t) :: vrand
  ! Locals
  type(YMM8r4_t), automatic :: r,x,w
  type(YMM8r4_t), save :: a,p,c,uf,vr,d
  ! Exec code ....
  r.v = v8r4_n0.v
  x.v = v8r4_n0.v
  w.v = v8r4_n0.v
  if(first) then
     a.v = v8r4_n1.v-s.v
     p.v = a.v/(a.v+s.v*exp(-a.v))
     if(all(s.v<v8r4_tiny.v)) then
        vrand.v = v8r4_neg1.v
        return
     end if
     c.v  = v8r4_n1.v/s.v
     uf.v = p.v*(v8r4_tiny.v/a.v)**s.v
     vr.v = v8r4_n1.v-v8r4_tiny.v
     d.v  = a.v*log(a.v)
  end if

  do
     call random_number(r.v)
     if(all(r.v >= vr.v)) then
        cycle
     else if(all(r.v > p.v)) then
        x.v = a.v-log((v8r4_n1.v-r.v)/(v8r4_n1.v-p.v))
        w.v = a.v*log(x.v)-d.v
     else if(all(r.v > uf.v)) then
        x.v = a.v*(r.v/p.v)**c.v
        w.v = x.v
     else
        vrand.v = v8r4_n0.v
        return
     end if
     call random_number(r.v)
     if(all(v8r4_n1.v<=w.v .and. r.v>v8r4_n0.v)) then
        if(all(r.v*(w.v+v8r4_n1.v)>=v8r4_n1.v)) cycle
        if(all(-log(r.v)<=w.v)) cycle
     end if
     exit
  end do
  vrand.v = x.v
end function random_gamma2_ymm8r4


#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
function random_gamma2_zmm16r4(s,first) result(vrand) !GCC$ ATTRIBUTES hot :: random_gamma2_zmm16r4 !GCC$ ATTRIBUTES inline :: random_gamma2_zmm16r4 !GCC$ ATTRIBUTES aligned(32) :: random_gamma2_zmm16r4 
#elif defined __ICC || defined __INTEL_COMPILER
function random_gamma2_zmm16r4(s,first) result(vrand)
  !DIR$ ATTRIBUTES INLINE :: random_gamma2_zmm16r4
  !DIR$ ATTRIBUTES ALIGN : 32 :: random_gamma2_zmm16r4
  !DIR$ ATTRIBUTES VECTOR :: random_gamma2_zmm16r4
#endif
  type(ZMM16r4_t),      intent(in) :: s
  logical(kind=i4),    intent(in) :: first
  type(ZMM16r4_t) :: vrand
  ! Locals
  type(ZMM16r4_t), automatic :: r,x,w
  type(ZMM16r4_t), save :: a,p,c,uf,vr,d
  ! Exec code ....
  r.v = v16r4_n0.v
  x.v = v16r4_n0.v
  w.v = v16r4_n0.v
  if(first) then
     a.v = v16r4_n1.v-s.v
     p.v = a.v/(a.v+s.v*exp(-a.v))
     if(all(s.v<v16r4_tiny.v)) then
        vrand.v = v16r4_neg1.v
        return
     end if
     c.v  = v16r4_n1.v/s.v
     uf.v = p.v*(v16r4_tiny.v/a.v)**s.v
     vr.v = v16r4_n1.v-v16r4_tiny.v
     d.v  = a.v*log(a.v)
  end if

  do
     call random_number(r.v)
     if(all(r.v >= vr.v)) then
        cycle
     else if(all(r.v > p.v)) then
        x.v = a.v-log((v16r4_n1.v-r.v)/(v16r4_n1.v-p.v))
        w.v = a.v*log(x.v)-d.v
     else if(all(r.v > uf.v)) then
        x.v = a.v*(r.v/p.v)**c.v
        w.v = x.v
     else
        vrand.v = v16r4_n0.v
        return
     end if
     call random_number(r.v)
     if(all(v16r4_n1.v<=w.v .and. r.v>v16r4_n0.v)) then
        if(all(r.v*(w.v+v16r4_n1.v)>=v16r4_n1.v)) cycle
        if(all(-log(r.v)<=w.v)) cycle
     end if
     exit
  end do
  vrand.v = x.v
end function random_gamma2_zmm16r4

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
function random_chisq_ymm8r4(ndf,first) result(vrand) !GCC$ ATTRIBUTES hot :: random_chisq_ymm8r4 !GCC$ ATTRIBUTES aligned(32) :: random_chisq_ymm8r4 !GCC$ ATTRIBUTES inline :: random_chisq_ymm8r4
#elif defined __ICC || defined __INTEL_COMPILER
function random_chisq_ymm8r4(ndf,first) result(vrand)
  !DIR$ ATTRIBUTES INLINE :: random_chisq_ymm8r4
  !DIR$ ATTRIBUTES ALIGN : 32 :: random_chisq_ymm8r4
  !DIR$ ATTRIBUTES VECTOR :: random_chisq_ymm8r4
#endif
  type(YMM8r4_t),   intent(in) :: ndf
  logical(kind=i4), intent(in) :: first
  type(YMM8r4_t) :: vrand
  ! Exec code ...
  vrand.v = v8r4_n2.v*random_gamma_ymm8r4(v8r4_half.v*ndf.v,first)
end function random_chisq_ymm8r4

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
function random_chisq_zmm16r4(ndf,first) result(vrand) !GCC$ ATTRIBUTES hot :: random_chisq_zmm16r4 !GCC$ ATTRIBUTES aligned(32) :: random_chisq_zmm16r4 !GCC$ ATTRIBUTES inline :: random_chisq_zmm16r4
#elif defined __ICC || defined __INTEL_COMPILER
function random_chisq_zmm16r4(ndf,first) result(vrand)
  !DIR$ ATTRIBUTES INLINE :: random_chisq_zmm16r4
  !DIR$ ATTRIBUTES ALIGN : 32 :: random_chisq_zmm16r4
  !DIR$ ATTRIBUTES VECTOR :: random_chisq_zmm16r4
#endif
  type(ZMM16r4_t),   intent(in) :: ndf
  logical(kind=i4), intent(in) :: first
  type(ZMM16r4_t) :: vrand
  ! Exec code ...
  vrand.v = v16r4_n2.v*random_gamma_zmm16r4(v8r4_half.v*ndf.v,first)
end function random_chisq_zmm16r4


#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
function random_exponential_ymm8r4() result(vrand) !GCC$ ATTRIBUTES hot :: random_exponential_ymm8r4 !GCC$ ATTRIBUTES inline :: random_exponential_ymm8r4 !GCC$ ATTRIBUTES aligned(32) :: random_exponential_ymm8r4
#elif defined __ICC || defined __INTEL_COMPILER
  function random_exponential_ymm8r4() result(vrand)
    !DIR$ ATTRIBUTES INLINE :: random_exponential_ymm8r4
    !DIR$ ATTRIBUTES ALIGN : 32 :: random_exponential_ymm8r4
    !DIR$ ATTRIBUTES VECTOR :: random_exponential_ymm8r4
#endif
    ! Adapted from Fortran 77 code from the book:
!     Dagpunar, J. 'Principles of random variate generation'
!     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

! FUNCTION GENERATES A RANDOM VARIATE IN [0,INFINITY) FROM
! A NEGATIVE EXPONENTIAL DlSTRIBUTION WlTH DENSITY PROPORTIONAL
    ! TO EXP(-random_exponential), USING INVERSION.
    type(YMM8r4_t) :: vrand
    type(YMM8r4_t), automatic :: r
    ! Exec code ...
    do
       call random_number(r.v)
       if(all(r.v>v8r4_n0.v)) exit
    end do
    vrand.v = -log(r.v)
end function random_exponential_ymm8r4


#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
function random_exponential_zmm16r4() result(vrand) !GCC$ ATTRIBUTES hot :: random_exponential_zmm16r4 !GCC$ ATTRIBUTES inline :: random_exponential_zmm16r4 !GCC$ ATTRIBUTES aligned(32) :: random_exponential_zmm16r4
#elif defined __ICC || defined __INTEL_COMPILER
  function random_exponential_zmm16r4() result(vrand)
    !DIR$ ATTRIBUTES INLINE :: random_exponential_zmm16r4
    !DIR$ ATTRIBUTES ALIGN : 32 :: random_exponential_zmm16r4
    !DIR$ ATTRIBUTES VECTOR :: random_exponential_zmm16r4
#endif
    ! Adapted from Fortran 77 code from the book:
!     Dagpunar, J. 'Principles of random variate generation'
!     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

! FUNCTION GENERATES A RANDOM VARIATE IN [0,INFINITY) FROM
! A NEGATIVE EXPONENTIAL DlSTRIBUTION WlTH DENSITY PROPORTIONAL
    ! TO EXP(-random_exponential), USING INVERSION.
    type(ZMM16r4_t) :: vrand
    type(ZMM16r4_t), automatic :: r
    ! Exec code ...
    do
       call random_number(r.v)
       if(all(r.v>v16r4_n0.v)) exit
    end do
    vrand.v = -log(r.v)
end function random_exponential_zmm16r4  

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
function random_weibull_ymm8r4(a) result(vrand) !GCC$ ATTRIBUTES hot :: random_weibull_ymm8r4 !GCC$ ATTRIBUTES inline :: random_weibull_ymm8r4 !GCC$ ATTRIBUTES aligned(32) :: random_weibull_ymm8r4
#elif defined __ICC || defined __INTEL_COMPILER
function random_weibull_ymm8r4(a) result(vrand)
  !DIR$ ATTRIBUTES INLINE :: random_weibull_ymm8r4
  !DIR$ ATTRIBUTES ALIGN : 32 :: random_weibull_ymm8r4
  !DIR$ ATTRIBUTES VECTOR :: random_wiebull_ymm8r4
#endif
  !     Generates a random variate from the Weibull distribution with
!     probability density:
!                      a
!               a-1  -x
!     f(x) = a.x    e
  type(YMM8r4_t),  intent(in) :: a
  type(YMM8r4_t) :: vrand
  ! Exec code ...
  vrand.v = random_exponentional_ymm8r4()**(v8r4_n1.v/a.v)
end function random_weibull_ymm8r4

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
function random_weibull_zmm16r4(a) result(vrand) !GCC$ ATTRIBUTES hot :: random_weibull_zmm16r4 !GCC$ ATTRIBUTES inline :: random_weibull_zmm16r4 !GCC$ ATTRIBUTES aligned(32) :: random_weibull_zmm16r4
#elif defined __ICC || defined __INTEL_COMPILER
function random_weibull_zmm16r4(a) result(vrand)
  !DIR$ ATTRIBUTES INLINE :: random_weibull_zmm16r4
  !DIR$ ATTRIBUTES ALIGN : 32 :: random_weibull_zmm16r4
  !DIR$ ATTRIBUTES VECTOR :: random_wiebull_zmm16r4
#endif
  !     Generates a random variate from the Weibull distribution with
!     probability density:
!                      a
!               a-1  -x
!     f(x) = a.x    e
  type(ZMM16r4_t),  intent(in) :: a
  type(ZMM16r4_t) :: vrand
  ! Exec code ...
  vrand.v = random_exponentional_zmm16r4()**(v16r4_n1.v/a.v)
end function random_weibull_ymm8r4


#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
function random_beta_ymm8r4(aa,bb,first) result(vrand) !GCC$ ATTRIBUTES hot :: random_beta_ymm8r4 !GCC$ ATTRIBUTES aligned(32) :: random_beta_ymm8r4 !GCC$ ATTRIBUTES inline :: random_beta_ymm8r4
#elif defined __ICC || defined __INTEL_COMPILER
function random_beta_ymm8r4(aa,bb,first) result(vrand)
    !DIR$ ATTRIBUTES INLINE :: random_beta_ymm8r4
  !DIR$ ATTRIBUTES ALIGN : 32 :: random_beta_ymm8r4
  !DIR$ ATTRIBUTES VECTOR :: random_beta_ymm8r4
#endif
  ! Adapted from Fortran 77 code from the book:
!     Dagpunar, J. 'Principles of random variate generation'
!     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

! FUNCTION GENERATES A RANDOM VARIATE IN [0,1]
! FROM A BETA DISTRIBUTION WITH DENSITY
! PROPORTIONAL TO BETA**(AA-1) * (1-BETA)**(BB-1).
! USING CHENG'S LOG LOGISTIC METHOD.

!     AA = SHAPE PARAMETER FROM DISTRIBUTION (0 < REAL)
!     BB = SHAPE PARAMETER FROM DISTRIBUTION (0 < REAL)
  type(YMM8r4_t),   intent(in) :: aa
  type(YMM8r4_t),   intent(in) :: bb
  logical(kind=i4), intent(in) :: first
  type(YMM8r4_t) :: vrand
  ! Locals
  type(YMM8r4_t), parameter :: aln4 = YMM8r4_t(1.3862944_sp)
  type(YMM8r4_t), automatic :: a,b,g,r,s,x,y,z
  type(YMM8r4_t), save :: d,f,h,t,c
  logical(kind=i4), save :: swap
  ! Exec code ....
  a.v = v8r4_n0.v
  b.v = v8r4_n0.v
  g.v = v8r4_n0.v
  r.v = v8r4_n0.v
  s.v = v8r4_n0.v
  x.v = v8r4_n0.v
  y.v = v8r4_n0.v
  z.v = v8r4_n0.v
  if(first) then
     a.v = aa.v
     b.v = bb.v
     if(swap=all(b.v>a.v)) then
        g.v = b.v
        b.v = a.v
        a.v = g.v
     end if
     d.v = a.v/b.v
     f.v = a.v+b.v
     if(all(b.v>v8r4_n1.v)) then
        h.v = sqrt((v8r4_n2.v*a.v*b.v-f.v)/(f.v-v8r4_n2.v))
        t.v = v8r4_n1.v
     else
        h.v = b.v
        t.v = v8r4_n1.v/(v8r4_n1.v+(a.v/(v8r4_huge.v*b.v))**b.v)
     end if
     c.v = a.v+h.v
  end if

  do
     call random_number(r.v)
     call random_number(x.v)
     s.v = r.v*r.v*x.v
     if(all(r.v<v8r4_tiny.v .or. s.v<=v8r4_n0.v)) cycle
     if(all(r.v<t.v)) then
        x.v = log(r.v/(v8r4_n1.v-r.v))/h.v
        y.v = d.v*exp(x.v)
        z.v = c.v*x.v+f.v*log((v8r4_n1.v+d.v)/(v8r4_n1.v+y.v))-aln4.v
        if(all(s.v-v8r4_n1.v>z.v)) then
           if(all(s.v-s.v*z.v>v8r4_n1.v)) cycle
           if(all(log(s.v)>z.v)) cycle
        end if
        vrand.v = y.v/(v8r4_n1.v+y.v)
     else
        if(all(v8r4_n4.v*s.v>(v8r4_n1.v+v8r4_n1.v/d.v)**f.v)) cycle
        vrand.v = v8r4_n1.v
     end if
     exit
  end do
  if(swap) vrand.v = v8r4_n1.v-vrand.v
end function random_beta_ymm8r4
  

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
function random_beta_zmm16r4(aa,bb,first) result(vrand) !GCC$ ATTRIBUTES hot :: random_beta_zmm16r4 !GCC$ ATTRIBUTES aligned(32) :: random_beta_zmm16r4 !GCC$ ATTRIBUTES inline :: random_beta_zmm16r4
#elif defined __ICC || defined __INTEL_COMPILER
function random_beta_zmm16r4(aa,bb,first) result(vrand)
    !DIR$ ATTRIBUTES INLINE :: random_beta_zmm16r4
  !DIR$ ATTRIBUTES ALIGN : 32 :: random_beta_zmm16r4
  !DIR$ ATTRIBUTES VECTOR :: random_beta_zmm16r4
#endif
  ! Adapted from Fortran 77 code from the book:
!     Dagpunar, J. 'Principles of random variate generation'
!     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

! FUNCTION GENERATES A RANDOM VARIATE IN [0,1]
! FROM A BETA DISTRIBUTION WITH DENSITY
! PROPORTIONAL TO BETA**(AA-1) * (1-BETA)**(BB-1).
! USING CHENG'S LOG LOGISTIC METHOD.

!     AA = SHAPE PARAMETER FROM DISTRIBUTION (0 < REAL)
!     BB = SHAPE PARAMETER FROM DISTRIBUTION (0 < REAL)
  type(ZMM16r4_t),   intent(in) :: aa
  type(ZMM16r4_t),   intent(in) :: bb
  logical(kind=i4), intent(in) :: first
  type(ZMM16r4_t) :: vrand
  ! Locals
  type(ZMM16r4_t), parameter :: aln4 = ZMM16r4_t(1.3862944_sp)
  type(ZMM16r4_t), automatic :: a,b,g,r,s,x,y,z
  type(ZMM16r4_t), save :: d,f,h,t,c
  logical(kind=i4), save :: swap
  ! Exec code ....
  a.v = v16r4_n0.v
  b.v = v16r4_n0.v
  g.v = v16r4_n0.v
  r.v = v16r4_n0.v
  s.v = v16r4_n0.v
  x.v = v16r4_n0.v
  y.v = v16r4_n0.v
  z.v = v16r4_n0.v
  if(first) then
     a.v = aa.v
     b.v = bb.v
     if(swap=all(b.v>a.v)) then
        g.v = b.v
        b.v = a.v
        a.v = g.v
     end if
     d.v = a.v/b.v
     f.v = a.v+b.v
     if(all(b.v>v8r4_n1.v)) then
        h.v = sqrt((v16r4_n2.v*a.v*b.v-f.v)/(f.v-v16r4_n2.v))
        t.v = v8r4_n1.v
     else
        h.v = b.v
        t.v = v16r4_n1.v/(v8r4_n1.v+(a.v/(v16r4_huge.v*b.v))**b.v)
     end if
     c.v = a.v+h.v
  end if

  do
     call random_number(r.v)
     call random_number(x.v)
     s.v = r.v*r.v*x.v
     if(all(r.v<v16r4_tiny.v .or. s.v<=v16r4_n0.v)) cycle
     if(all(r.v<t.v)) then
        x.v = log(r.v/(v16r4_n1.v-r.v))/h.v
        y.v = d.v*exp(x.v)
        z.v = c.v*x.v+f.v*log((v16r4_n1.v+d.v)/(v16r4_n1.v+y.v))-aln4.v
        if(all(s.v-v16r4_n1.v>z.v)) then
           if(all(s.v-s.v*z.v>v16r4_n1.v)) cycle
           if(all(log(s.v)>z.v)) cycle
        end if
        vrand.v = y.v/(v16r4_n1.v+y.v)
     else
        if(all(v16r4_n4.v*s.v>(v16r4_n1.v+v16r4_n1.v/d.v)**f.v)) cycle
        vrand.v = v16r4_n1.v
     end if
     exit
  end do
  if(swap) vrand.v = v16r4_n1.v-vrand.v
end function random_beta_zmm16r4


#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
function random_t_ymm8r4(m) result(vrand) !GCC$ ATTRIBUTES hot :: random_t_ymm8r4 !GCC$ ATTRIBUTES aligned(32) :: random_t_ymm8r4 !GCC$ ATTRIBUTES inline :: random_t_ymm8r4
#elif defined __ICC || defined __INTEL_COMPILER
function random_t_ymm8r4(m) result(vrand)
  !DIR$ ATTRIBUTES INLINE :: random_t_ymm8r4
  !DIR$ ATTRIBUTES ALIGN : 32 :: random_t_ymm8r4
  !DIR$ ATTRIBUTES VECTOR :: random_t_ymm8r4
#endif
  ! Adapted from Fortran 77 code from the book:
!     Dagpunar, J. 'Principles of random variate generation'
!     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

! FUNCTION GENERATES A RANDOM VARIATE FROM A
! T DISTRIBUTION USING KINDERMAN AND MONAHAN'S RATIO METHOD.

!     M = DEGREES OF FREEDOM OF DISTRIBUTION
!           (1 <= 1NTEGER)
  integer(kind=i4),    intent(in) :: m
  type(YMM8r4_t) :: vrand
  ! Locals
  type(YMM8r4_t), save :: s,c,a,f,g
  type(YMM8r4_t), automatic :: r,x,v,t0
  integer(kind=i4), automatic :: mm = 0
  ! Exec code ....
  if(m < 1) then
     vrand.v = v8r4_huge
     return
  end if
  r.v = v8r4_n0.v
  x.v = v8r4_n0.v
  v.v = v8r4_n0.v
  if(m /= mm) then
     s.v = real(m,kind=sp)
     c.v = -v8r4_quart.v*(s.v+v8r4_n1.v)
     a.v = v8r4_n4.v/(v8r4_n1.v+v8r4_n1.v/s.v)**c.v
     f.v = v8r4_n16.v/a.v
     if(m>1) then
        g.v  = s.v-v8r4_n1.v
        t0.v = v8r4_n1.v/g.v
        g.v  = ((s.v+v8r4_n1.v)*t0.v)**c*sqrt((s.v+s.v)*t0.v
     else
        g.v = v8r4_n1.v
     end if
     mm = n
  end if

  do
     call random_number(r.v)
     if(all(r.v<=v8r4_n0.v)) cycle
     call random_number(v.v)
     x.v = (v8r4_n2.v*v.v-v8r4_n1.v)*g.v/r.v
     v.v = x.v*x.v
     if(all(v.v>v8r4_n5.v-a.v*r.v)) then
        if(m>=1 .and. all(r.v*(v.v+v8r4_n3.v)>f.v)) cycle
        if(all(r.v>(v8r4_n1.v+v.vs.v)**c.v)) cycle
     end if
     exit
  end do
  vrand.v = x.v
end function random_t_ymm8r4

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
function random_t_zmm16r4(m) result(vrand) !GCC$ ATTRIBUTES hot :: random_t_zmm16r4 !GCC$ ATTRIBUTES aligned(32) :: random_t_zmm16r4 !GCC$ ATTRIBUTES inline :: random_t_zmm16r4
#elif defined __ICC || defined __INTEL_COMPILER
function random_t_zmm16r4(m) result(vrand)
  !DIR$ ATTRIBUTES INLINE :: random_t_zmm16r4
  !DIR$ ATTRIBUTES ALIGN : 32 :: random_t_zmm16r4
  !DIR$ ATTRIBUTES VECTOR :: random_t_zmm16r4
#endif
  ! Adapted from Fortran 77 code from the book:
!     Dagpunar, J. 'Principles of random variate generation'
!     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

! FUNCTION GENERATES A RANDOM VARIATE FROM A
! T DISTRIBUTION USING KINDERMAN AND MONAHAN'S RATIO METHOD.

!     M = DEGREES OF FREEDOM OF DISTRIBUTION
!           (1 <= 1NTEGER)
  integer(kind=i4),    intent(in) :: m
  type(ZMM16r4_t) :: vrand
  ! Locals
  type(ZMM16r4_t), save :: s,c,a,f,g
  type(ZMM16r4_t), automatic :: r,x,v,t0
  integer(kind=i4), automatic :: mm = 0
  ! Exec code ....
  if(m < 1) then
     vrand.v = v16r4_huge
     return
  end if
  r.v = v16r4_n0.v
  x.v = v16r4_n0.v
  v.v = v16r4_n0.v
  if(m /= mm) then
     s.v = real(m,kind=sp)
     c.v = -v16r4_quart.v*(s.v+v16r4_n1.v)
     a.v = v16r4_n4.v/(v16r4_n1.v+v16r4_n1.v/s.v)**c.v
     f.v = v16r4_n16.v/a.v
     if(m>1) then
        g.v  = s.v-v16r4_n1.v
        t0.v = v16r4_n1.v/g.v
        g.v  = ((s.v+v16r4_n1.v)*t0.v)**c*sqrt((s.v+s.v)*t0.v
     else
        g.v = v16r4_n1.v
     end if
     mm = n
  end if

  do
     call random_number(r.v)
     if(all(r.v<=v16r4_n0.v)) cycle
     call random_number(v.v)
     x.v = (v16r4_n2.v*v.v-v16r4_n1.v)*g.v/r.v
     v.v = x.v*x.v
     if(all(v.v>v16r4_n5.v-a.v*r.v)) then
        if(m>=1 .and. all(r.v*(v.v+v16r4_n3.v)>f.v)) cycle
        if(all(r.v>(v16r4_n1.v+v.vs.v)**c.v)) cycle
     end if
     exit
  end do
  vrand.v = x.v
end function random_t_zmm16r4

#if defined __GFORTAN__ && !defined __INTEL_COMPILER
subroutine random_mvnorm_ymm8r4(n,h,d,f,first,x,ier) !GCC$ ATTRIBUTES aligned(32) :: random_mvnorm_ymm8r4 !GCC$ ATTRIBUTES hot :: random_mvnorm_ymm8r4 !GCC$ ATTRIBUTES inline :: random_mvnorm_ymm8r4
#elif defined __ICC || defined __INTEL_COMPILER
subroutine random_mvnorm_ymm8r4(n,h,d,f,first,x,ier)
  !DIR$ ATTRIBUTES INLINE :: random_mvnorm_ymm8r4
  !DIR$ ATTRIBUTES ALIGN : 32 :: random_mvnorm_ymm8r4
  !DIR$ ATTRIBUTES VECTOR :: random_mvnorm_ymm8r4
#endif
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
  use omp_lib
#endif
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
  integer(kind=i4),             intent(in) :: n
  type(YMM8r4_t), dimension(:), intent(in) :: h,d
#if defined __INTEL_COMPILER
  !DIR$ ASSUME_ALIGNED h:32,d:32
#endif
  type(YMM8r4_t), dimension(:), intent(inout) :: f
#if defined __INTEL_COMPILER
  !DIR$ ASSUME_ALIGNED f:32
#endif
  type(YMM8r4_t), dimension(:), intent(out)  :: x
#if defined __INTEL_COMPILER
  !DIR$ ASSUME_ALIGNED x:32
#endif
  logical(kind=i4),             intent(in)   :: first
  integer(kind=i4),             intent(in)   :: ier
  ! Locals
  type(YMM8r4_t), automatic :: y,v
  integer(kind=i4), automatic :: j,i,m,idx,jdx
  integer(kind=i4), save :: n2
  ! Exec code ...
  ier = 0
  idx = 0
  jdx = 0
  if(first) then
     n2 = n+n
     if(all(d(1).v<v8r4_n0.v)) then
        ier = 1
        return
     end if
     f(1).v = sqrt(d(1).v)
     y.v = v8r4_n1.v/f(1).v
#if defined __ICC || defined __INTEL_COMPILER
     !DIR$ VECTOR ALWAYS
     !DIR$ VECTOR ALIGNED
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
     !$OMP SIMD ALIGNED(f:32,d:32,y:32)
#endif
     do j=2, n
        f(j).v = d(1+j*(j-2)/2).v*y.v
     end do

     do i = 2, n
        v.v = d(i*(i-1)/2+i).v
#if defined __ICC || defined __INTEL_COMPILER
     !DIR$ VECTOR ALWAYS
     !DIR$ VECTOR ALIGNED
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
     !$OMP SIMD ALIGNED(f:32)
#endif
        do m = 1, i-1
           idx = (m-1)*(n2-m)/2+i
           v.v = v.v-(f(idx).v*(f(idx).v)
        end do
        
        if(all(v.v<v8r4_n0.v)) then
           ier = 1
           return
        end if

        v.v = sqrt(v.v)
        y.v = v8r4_n1.v/v.v
        f((i-1)*(n2-i)/2+i).v = v.v
        do j = i+1,n
           v.v = d(j*(j-1)/2+i).v = v.v
#if defined __ICC || defined __INTEL_COMPILER
     !DIR$ VECTOR ALWAYS
     !DIR$ VECTOR ALIGNED
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
     !$OMP SIMD ALIGNED(f:32)
#endif
           do m = 1, i-1
              idx = (m-1)*(n2-m)/2+i
              jdx = (m-1)*(n2-m)/2+j
              v.v = v.v-f(idx).v*f(jdx).v
           end do
           f((i-1)*(n2-i)/2+j).v = v.v*y.v
        end do
     end do
  end if

  x(1:n).v = h(1:n).v
  do j = 1, n
     y.v = random_normal_ymm8r4()
#if defined __ICC || defined __INTEL_COMPILER
     !DIR$ VECTOR ALWAYS
     !DIR$ VECTOR ALIGNED
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
     !$OMP SIMD ALIGNED(f:32,x:32)
#endif     
     do i = j, n
        x(i).v = x(i).v+f((j-1)*(n2-j)/2+i).v*y.v
     end do
  end do
end subroutine random_mvnorm_ymm8r4

#if defined __GFORTAN__ && !defined __INTEL_COMPILER
subroutine random_mvnorm_zmm16r4(n,h,d,f,first,x,ier) !GCC$ ATTRIBUTES aligned(32) :: random_mvnorm_zmm16r4 !GCC$ ATTRIBUTES hot :: random_mvnorm_zmm16r4 !GCC$ ATTRIBUTES inline :: random_mvnorm_zmm16r4
#elif defined __ICC || defined __INTEL_COMPILER
subroutine random_mvnorm_zmm16r4(n,h,d,f,first,x,ier)
  !DIR$ ATTRIBUTES INLINE :: random_mvnorm_zmm16r4
  !DIR$ ATTRIBUTES ALIGN : 32 :: random_mvnorm_zmm16r4
  !DIR$ ATTRIBUTES VECTOR :: random_mvnorm_zmm16r4
#endif
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
  use omp_lib
#endif
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
  integer(kind=i4),             intent(in) :: n
  type(ZMM16r4_t), dimension(:), intent(in) :: h,d
#if defined __INTEL_COMPILER
  !DIR$ ASSUME_ALIGNED h:64,d:64
#endif
  type(ZMM16r4_t), dimension(:), intent(inout) :: f
#if defined __INTEL_COMPILER
  !DIR$ ASSUME_ALIGNED f:64
#endif
  type(ZMM16r4_t), dimension(:), intent(out)  :: x
#if defined __INTEL_COMPILER
  !DIR$ ASSUME_ALIGNED x:64
#endif
  logical(kind=i4),             intent(in)   :: first
  integer(kind=i4),             intent(in)   :: ier
  ! Locals
  type(ZMM16r4_t), automatic :: y,v
  integer(kind=i4), automatic :: j,i,m,idx,jdx
  integer(kind=i4), save :: n2
  ! Exec code ...
  ier = 0
  idx = 0
  jdx = 0
  if(first) then
     n2 = n+n
     if(all(d(1).v<v16r4_n0.v)) then
        ier = 1
        return
     end if
     f(1).v = sqrt(d(1).v)
     y.v = v16r4_n1.v/f(1).v
#if defined __ICC || defined __INTEL_COMPILER
     !DIR$ VECTOR ALWAYS
     !DIR$ VECTOR ALIGNED
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
     !$OMP SIMD ALIGNED(f:64,d:64,y:64)
#endif
     do j=2, n
        f(j).v = d(1+j*(j-2)/2).v*y.v
     end do

     do i = 2, n
        v.v = d(i*(i-1)/2+i).v
#if defined __ICC || defined __INTEL_COMPILER
     !DIR$ VECTOR ALWAYS
     !DIR$ VECTOR ALIGNED
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
     !$OMP SIMD ALIGNED(f:64)
#endif
        do m = 1, i-1
           idx = (m-1)*(n2-m)/2+i
           v.v = v.v-(f(idx).v*(f(idx).v)
        end do
        
        if(all(v.v<v16r4_n0.v)) then
           ier = 1
           return
        end if

        v.v = sqrt(v.v)
        y.v = v16r4_n1.v/v.v
        f((i-1)*(n2-i)/2+i).v = v.v
        do j = i+1,n
           v.v = d(j*(j-1)/2+i).v = v.v
#if defined __ICC || defined __INTEL_COMPILER
     !DIR$ VECTOR ALWAYS
     !DIR$ VECTOR ALIGNED
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
     !$OMP SIMD ALIGNED(f:64)
#endif
           do m = 1, i-1
              idx = (m-1)*(n2-m)/2+i
              jdx = (m-1)*(n2-m)/2+j
              v.v = v.v-f(idx).v*f(jdx).v
           end do
           f((i-1)*(n2-i)/2+j).v = v.v*y.v
        end do
     end do
  end if

  x(1:n).v = h(1:n).v
  do j = 1, n
     y.v = random_normal_zmm16r4()
#if defined __ICC || defined __INTEL_COMPILER
     !DIR$ VECTOR ALWAYS
     !DIR$ VECTOR ALIGNED
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
     !$OMP SIMD ALIGNED(f:64,x:64)
#endif     
     do i = j, n
        x(i).v = x(i).v+f((j-1)*(n2-j)/2+i).v*y.v
     end do
  end do
end subroutine random_mvnorm_zmm16r4

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
function rand_inv_gauss_ymm8r4(h,b,first) result(v) !GCC$ ATTRIBUTES hot :: rand_inv_gauss_ymm8r4 !GCC$ ATTRIBUTES aligned(32) :: rand_inv_gauss_ymm8r4 !GCC$ ATTRIBUTES inline :: rand_inv_gauss_ymm8r4
#elif defined __ICC || defined __INTEL_COMPILER
function rand_inv_gauss_ymm8r4(h,b,first) result(v)
  !DIR$ ATTRIBUTES INLINE :: rand_inv_gauss_ymm8r4
  !DIR$ ATTRIBUTES ALIGN : 32 :: rand_inv_gauss_ymm8r4
  !DIR$ ATTRIBUTES VECTOR :: rand_inv_gauss_ymm8r4
#endif
  type(YMM8r4_t),    intent(in) :: h,b
  logical(kind=i4),  intent(in) :: first
  type(YMM8r4_t) :: v
  ! Locals
  type(YMM8r4_t), automatic :: ym,xm,r,w,r1,r2,x
  type(YMM8r4_t), save :: a,c,d,e
  ! Locals
  ym.v = v8r4_n0.v
  xm.v = v8r4_n0.v
  r.v  = v8r4_n0.v
  w.v  = v8r4_n0.v
  r1.v = v8r4_n0.v
  r2.v = v8r4_n0.v
  x.v  = v8r4_n0.v
  if(first) then
     if(all(h.v>v8r4_quart.v*b.v*sqrt(v8r4_huge.v))) then
        v.v = v8r4_huge.v
        return
     end if
     e.v = b.v*b.v
     d.v = h.v+v8r4_n1.v
     ym.v = (-d.v+sqrt(d.v*d.v+e.v))/b.v
     if(all(ym.v<v8r4_tiny.v)) then
        v.v = v8r4_tiny.v
        return
     end if
     d.v = h.v-v8r4_n0.v
     xm.v = (d.v+sqrt(d.v*d.v+e.v))/b.v
     d.v = v8r4_half.v*d.v
     e.v = -v8r4_half.v*b.v
     r.v = xm.v+v8r4_n1.v/xm.v
     w.v = xm.v*ym.v
     a.v = w.v**(-v8r4_half.v*h.v)*sqrt(xm.v/ym.v)*exp(-e.v*(r.v-ym.v-v8r4_n1.v/ym.v))
     if(all(a.v<v8r4_tiny.v)) then
        v.v = v8r4_tiny.v
        return
     end if
     c.v = -d.v*log(xm.v)-e.v*r.v
  end if

  do
     call random_number(r1.v)
     if(all(r1<=v8r4_n0.v)) cycle
     call random_number(r2.v)
     x.v = a.v*r2.v/r1.v
     if(all(x.v<=v8r4_n0.v)) cycle
     if(all(log(r1.v)<d.v*log(x.v)+e.v*(x.v+v8r4_n1.v/x.v)+c.v)) exit
  end do
  v.v = x.v
end function rand_inv_gauss_ymm8r4


#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
function rand_inv_gauss_zmm16r4(h,b,first) result(v) !GCC$ ATTRIBUTES hot :: rand_inv_gauss_zmm16r4 !GCC$ ATTRIBUTES aligned(32) :: rand_inv_gauss_zmm16r4 !GCC$ ATTRIBUTES inline :: rand_inv_gauss_zmm16r4
#elif defined __ICC || defined __INTEL_COMPILER
function rand_inv_gauss_zmm16r4(h,b,first) result(v)
  !DIR$ ATTRIBUTES INLINE :: rand_inv_gauss_zmm16r4
  !DIR$ ATTRIBUTES ALIGN : 32 :: rand_inv_gauss_zmm16r4
  !DIR$ ATTRIBUTES VECTOR :: rand_inv_gauss_zmm16r4
#endif
  type(ZMM16r4_t),    intent(in) :: h,b
  logical(kind=i4),  intent(in) :: first
  type(ZMM16r4_t) :: v
  ! Locals
  type(ZMM16r4_t), automatic :: ym,xm,r,w,r1,r2,x
  type(ZMM16r4_t), save :: a,c,d,e
  ! Locals
  ym.v = v16r4_n0.v
  xm.v = v16r4_n0.v
  r.v  = v16r4_n0.v
  w.v  = v16r4_n0.v
  r1.v = v16r4_n0.v
  r2.v = v16r4_n0.v
  x.v  = v16r4_n0.v
  if(first) then
     if(all(h.v>v16r4_quart.v*b.v*sqrt(v16r4_huge.v))) then
        v.v = v16r4_huge.v
        return
     end if
     e.v = b.v*b.v
     d.v = h.v+v16r4_n1.v
     ym.v = (-d.v+sqrt(d.v*d.v+e.v))/b.v
     if(all(ym.v<v16r4_tiny.v)) then
        v.v = v16r4_tiny.v
        return
     end if
     d.v = h.v-v16r4_n0.v
     xm.v = (d.v+sqrt(d.v*d.v+e.v))/b.v
     d.v = v16r4_half.v*d.v
     e.v = -v16r4_half.v*b.v
     r.v = xm.v+v16r4_n1.v/xm.v
     w.v = xm.v*ym.v
     a.v = w.v**(-v16r4_half.v*h.v)*sqrt(xm.v/ym.v)*exp(-e.v*(r.v-ym.v-v16r4_n1.v/ym.v))
     if(all(a.v<v16r4_tiny.v)) then
        v.v = v16r4_tiny.v
        return
     end if
     c.v = -d.v*log(xm.v)-e.v*r.v
  end if

  do
     call random_number(r1.v)
     if(all(r1<=v16r4_n0.v)) cycle
     call random_number(r2.v)
     x.v = a.v*r2.v/r1.v
     if(all(x.v<=v16r4_n0.v)) cycle
     if(all(log(r1.v)<d.v*log(x.v)+e.v*(x.v+v16r4_n1.v/x.v)+c.v)) exit
  end do
  v.v = x.v
end function rand_inv_gauss_zmm16r4


  
END MODULE mod_vec_rand_distr
