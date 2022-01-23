MODULE service
   use mod_kinds, only : i4,sp,dp
  IMPLICIT NONE
  !
  !INTEGER(i4), PARAMETER :: sp = SELECTED_REAL_KIND(6,37)
  !INTEGER(i4), PARAMETER :: dp = SELECTED_REAL_KIND(15,307)
  !INTEGER(i4), PARAMETER :: QP = SELECTED_REAL_KIND(33,4931)
  !   Integers:
  !     assume integers are represented in the S-digit, base-A form
  !
  !                sign ( X(S-1)*A**(S-1) + ... + X(1)*A + X(0) )
  !
  !                where 0 <= X(I) < A for I=0,...,S-1.
  !     radix_int  = A, the base.
  !     digits_int = S, the number of base-A digits.
  !     huge_int   = A**S - 1, the largest magnitude.
  !
  !   Floating-Point Numbers:
  !     Assume floating-point numbers are represented in the T-digit,
  !     base-B form
  !                sign (B**E)*( (X(1)/B) + ... + (X(T)/B**T) )
  !
  !                where 0 <= X(I) < B for I=1,...,T,
  !                0 < X(1), and EMIN <= E <= EMAX.
  !     radix_fp  = B, the base.
  !
  !   Single-Precision:
  !     digits_sp  = T, the number of base-B digits.
  !     min_exp_sp = EMIN, the smallest exponent E.
  !     max_exp_sp = EMAX, the largest exponent E.
  !
  !   Double-Precision:
  !     digits_dp  = T, the number of base-B digits.
  !     min_exp_dp = EMIN, the smallest exponent E.
  !     max_exp_dp = EMAX, the largest exponent E.
  INTEGER(i4), PARAMETER :: radix_int = RADIX(1), digits_int = DIGITS(1), &
    huge_int = HUGE(1), radix_fp = RADIX(1._sp), &
    digits_sp = DIGITS(1._sp), min_exp_sp = MINEXPONENT(1._sp), &
    max_exp_sp = MAXEXPONENT(1._sp), digits_dp = DIGITS(1._dp), &
    min_exp_dp = MINEXPONENT(1._dp), max_exp_dp = MAXEXPONENT(1._dp)
  !   tiny_sp        = B**(EMIN-1), the smallest positive magnitude.
  !   huge_sp        = B**EMAX*(1 - B**(-T)), the largest magnitude.
  !   eps_2_sp       = B**(-T), the smallest relative spacing.
  !   eps_sp         = B**(1-T), the largest relative spacing.
  !   log10_radix_sp = LOG10(B)
  !
  !   Assume single precision numbers are represented in the T-digit,
  !   base-B form
  !
  !              sign (B**E)*( (X(1)/B) + ... + (X(T)/B**T) )
  !
  !   where 0 <= X(I) < B for I=1,...,T, 0 < X(1), and
  !   EMIN <= E <= EMAX.
  REAL(sp), PARAMETER :: tiny_sp = TINY(1._sp), huge_sp = HUGE(1._sp), &
    eps_2_sp = EPSILON(1._sp)/RADIX(1._sp), eps_sp = EPSILON(1._sp), &
    log10_radix_sp = LOG10( REAL( RADIX(1._sp), sp ) )
  !   tiny_dp        = B**(EMIN-1), the smallest positive magnitude.
  !   huge_dp        = B**EMAX*(1 - B**(-T)), the largest magnitude.
  !   eps_2_dp       = B**(-T), the smallest relative spacing.
  !   eps_dp         = B**(1-T), the largest relative spacing.
  !   log10_radix_dp = LOG10(B)
  !
  !   Assume double precision numbers are represented in the T-digit,
  !   base-B form
  !
  !              sign (B**E)*( (X(1)/B) + ... + (X(T)/B**T) )
  !
  !   where 0 <= X(I) < B for I=1,...,T, 0 < X(1), and
  !   EMIN <= E <= EMAX.
  REAL(dp), PARAMETER :: tiny_dp = TINY(1._dp), huge_dp = HUGE(1._dp), &
    eps_2_dp = EPSILON(1._dp)/RADIX(1._dp), eps_dp = EPSILON(1._dp), &
    log10_radix_dp = LOG10( REAL( RADIX(1._dp), dp ) )
  !
!CONTAINS
!  include"ivout.f90"
!  include"svout.f90"
!  include"dvout.f90"
END MODULE service
