

#if defined __GFORTRAN__ && (!defined(__INTEL_COMPILER) || !defined(__ICC))
      LOGICAL          FUNCTION LSAME( CA, CB ) !GCC$ ATTRIBUTES inline :: LSAME
#elif defined(__ICC) || defined(__INTEL_COMPILER)
      LOGICAL          FUNCTION LSAME( CA, CB)
      !DIR$ ATTRIBUTES INLINE :: LSAME
#endif

!*
!*  -- LAPACK auxiliary routine (preliminary version) --
!*     Univ. of Tennessee, Oak Ridge National Lab, Argonne National Lab,
!*     Courant Institute, NAG Ltd., and Rice University
!*     March 26, 1990
!*
!*     .. Scalar Arguments ..
      CHARACTER          CA, CB
!*     ..
!*
!*  Purpose
!*  =======
!*
!*  LSAME returns .TRUE. if CA is the same letter as CB regardless of
!*  case.
!*
!*  This version of the routine is only correct for ASCII code.
!*  Installers must modify the routine for other character-codes.
!*
!*  For EBCDIC systems the constant IOFF must be changed to -64.
!*  For CDC systems using 6-12 bit representations, the system-
!*  specific code in comments must be activated.
!*
!*  Arguments
!*  =========
!*
!*  CA      (input) CHARACTER*1
!*  CB      (input) CHARACTER*1
!*          CA and CB specify the single characters to be compared.
!*
!*
!*     .. Parameters ..
      INTEGER            IOFF
      PARAMETER        ( IOFF = 32 )
!*     ..
!*     .. Intrinsic Functions ..
      INTRINSIC          ICHAR
!*     ..
!*     .. Executable Statements ..
!*
!*     Test if the characters are equal
!*
      LSAME = CA.EQ.CB
!*
!*     Now test for equivalence
!*
      IF( .NOT.LSAME ) THEN
         LSAME = ICHAR( CA ) - IOFF.EQ.ICHAR( CB )
      END IF
      IF( .NOT.LSAME ) THEN
         LSAME = ICHAR( CA ).EQ.ICHAR( CB ) - IOFF
      END IF

END FUNCTION LSAME


!*!> DLAMCH determines double precision machine parameters.
!*> \endverbatim
!*
!*  Arguments:
!*  ==========
!*
!*> \param[in] CMACH
!*> \verbatim
!*>          CMACH is CHARACTER*1
!*>          Specifies the value to be returned by DLAMCH:
!*>          = 'E' or 'e',   DLAMCH := eps
!*>          = 'S' or 's ,   DLAMCH := sfmin
!*>          = 'B' or 'b',   DLAMCH := base
!*>          = 'P' or 'p',   DLAMCH := eps*base
!*>          = 'N' or 'n',   DLAMCH := t
!*>          = 'R' or 'r',   DLAMCH := rnd
!*>          = 'M' or 'm',   DLAMCH := emin
!*>          = 'U' or 'u',   DLAMCH := rmin
!*>          = 'L' or 'l',   DLAMCH := emax
!*>          = 'O' or 'o',   DLAMCH := rmax
!*>          where
!*>          eps   = relative machine precision
!*>          sfmin = safe minimum, such that 1/sfmin does not overflow
!*>          base  = base of the machine
!*>          prec  = eps*base
!*>          t     = number of (base) digits in the mantissa
!*>          rnd   = 1.0 when rounding occurs in addition, 0.0 otherwise
!*>          emin  = minimum exponent before (gradual) underflow
!*>          rmin  = underflow threshold - base**(emin-1)
!*>          emax  = largest exponent before overflow
!*>          rmax  = overflow threshold  - (base**emax)*(1-eps)
!*> \endverbatim
!*
!*  Authors:
!*  ========
!*
!*> \author Univ. of Tennessee
!*> \author Univ. of California Berkeley
!*> \author Univ. of Colorado Denver
!*> \author NAG Ltd.
!*
!*> \date December 2016
!*
!*> \ingroup auxOTHERauxiliary
!*
!*  =====================================================================

      DOUBLE PRECISION FUNCTION DLAMCH( CMACH )
!*
!*  -- LAPACK auxiliary routine (version 3.7.0) --
!*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!*     December 2016
!*
!*     .. Scalar Arguments ..
      CHARACTER          CMACH
!*     ..
!*
!* =====================================================================
!*
!*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!*     ..
!*     .. Local Scalars ..
      DOUBLE PRECISION   RND, EPS, SFMIN, SMALL, RMACH
!*     ..
!*     .. External Functions ..
!      LOGICAL            LSAME
!      EXTERNAL           LSAME
!*     ..
!*     .. Intrinsic Functions ..
      INTRINSIC          DIGITS, EPSILON, HUGE, MAXEXPONENT, &
                         MINEXPONENT, RADIX, TINY
!*     ..
!*     .. Executable Statements ..
!*
!*
!*     Assume rounding, not chopping. Always.
!*
      RND = ONE
!*
      IF( ONE.EQ.RND ) THEN
         EPS = EPSILON(ZERO) * 0.5
      ELSE
         EPS = EPSILON(ZERO)
      END IF
!*
      IF( LSAME( CMACH, 'E' ) ) THEN
         RMACH = EPS
      ELSE IF( LSAME( CMACH, 'S' ) ) THEN
         SFMIN = TINY(ZERO)
         SMALL = ONE / HUGE(ZERO)
         IF( SMALL.GE.SFMIN ) THEN
!*
!*           Use SMALL plus a bit, to avoid the possibility of rounding
!*           causing overflow when computing  1/sfmin.
!*
            SFMIN = SMALL*( ONE+EPS )
         END IF
         RMACH = SFMIN
      ELSE IF( LSAME( CMACH, 'B' ) ) THEN
         RMACH = RADIX(ZERO)
      ELSE IF( LSAME( CMACH, 'P' ) ) THEN
         RMACH = EPS * RADIX(ZERO)
      ELSE IF( LSAME( CMACH, 'N' ) ) THEN
         RMACH = DIGITS(ZERO)
      ELSE IF( LSAME( CMACH, 'R' ) ) THEN
         RMACH = RND
      ELSE IF( LSAME( CMACH, 'M' ) ) THEN
         RMACH = MINEXPONENT(ZERO)
      ELSE IF( LSAME( CMACH, 'U' ) ) THEN
         RMACH = tiny(zero)
      ELSE IF( LSAME( CMACH, 'L' ) ) THEN
         RMACH = MAXEXPONENT(ZERO)
      ELSE IF( LSAME( CMACH, 'O' ) ) THEN
         RMACH = HUGE(ZERO)
      ELSE
         RMACH = ZERO
      END IF

      DLAMCH = RMACH
     

END FUNCTION DLAMCH

DOUBLE PRECISION FUNCTION DLAMC3( A, B )
!*
!*  -- LAPACK auxiliary routine (version 3.7.0) --
!*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!*     November 2010
!*
!*     .. Scalar Arguments ..
      DOUBLE PRECISION   A, B
!*     ..
!* =====================================================================
!*
!*     .. Executable Statements ..
!*
      DLAMC3 = A + B
!*
   

END FUNCTION DLAMC3


#if defined(__GFORTRAN__) && (!defined(__ICC) || !defined(__INTEL_COMPILER))
INTEGER FUNCTION IDAMAX(N,DX,INCX) !GCC$ ATTRIBUTES aligned(32) :: IDAMAX !GCC$ ATTRIBUTES PURE :: IDAMAX !GCC$ ATTRIBUTES INLINE :: IDAMAX
#elif defined(__ICC) || defined(__INTEL_COMPILER)
INTEGER FUNCTION IDAMAX(N,DX,INCX)
!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: IDAMAX
!DIR$ ATTRIBUTES FORCEINLINE :: IDAMAX
#endif
   implicit none

!*
!*  -- Reference BLAS level1 routine (version 3.8.0) --
!*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
!*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!*     November 2017
!*
!*     .. Scalar Arguments ..
      INTEGER INCX,N
!*     ..
!*     .. Array Arguments ..
      DOUBLE PRECISION DX(*)
#if defined(__ICC) || defined(__INTEL_COMPILER)
      DOUBLE PRECISION DX(*)
!DIR$ ASSUME_ALIGNED DX:64
#endif

!*     ..
!*
!*  =====================================================================
!*
!*     .. Local Scalars ..
      DOUBLE PRECISION DMAX
      INTEGER I,IX
!*     ..
!*     .. Intrinsic Functions ..
      INTRINSIC DABS
!*     ..
      IDAMAX = 0
      IF (N.LT.1 .OR. INCX.LE.0) RETURN
      IDAMAX = 1
      IF (N.EQ.1) RETURN
      IF (INCX.EQ.1) THEN
!*
!*        code for increment equal to 1
!*
         DMAX = DABS(DX(1))
         DO I = 2,N
            IF (DABS(DX(I)).GT.DMAX) THEN
               IDAMAX = I
               DMAX = DABS(DX(I))
            END IF
         END DO
      ELSE
!*
!*        code for increment not equal to 1
!*
         IX = 1
         DMAX = DABS(DX(1))
         IX = IX + INCX
         DO I = 2,N
            IF (DABS(DX(IX)).GT.DMAX) THEN
               IDAMAX = I
               DMAX = DABS(DX(IX))
            END IF
            IX = IX + INCX
         END DO
      END IF
     
END FUNCTION IDAMAX


#if 0
*>
*> \verbatim
*>
*> DISNAN returns .TRUE. if its argument is NaN, and .FALSE.
*> otherwise.  To be replaced by the Fortran 2003 intrinsic in the
*> future.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] DIN
*> \verbatim
*>          DIN is DOUBLE PRECISION
*>          Input to test for NaN.
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \date June 2017
*
*> \ingroup OTHERauxiliary
*
*  =====================================================================
#endif
#if defined(__GFORTRAN__) && (!defined(__ICC) || !defined(__INTEL_COMPILER))
LOGICAL FUNCTION DISNAN( DIN ) !GCC$ ATTRIBUTES INLINE :: DISNAN !GCC$ ATTRIBUTES ALIGNED(32) :: DISNAN
#elif defined(__ICC) || defined(__INTEL_COMPILER)
  LOGICAL FUNCTION DISNAN( DIN)
    !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: DISNAN
    !DIR$ ATTRIBUTES FORCEINLINE :: DISNAN
#endif
!*
!*  -- LAPACK auxiliary routine (version 3.7.1) --
!*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!*     June 2017
!*
!*     .. Scalar Arguments ..
      DOUBLE PRECISION, INTENT(IN) :: DIN
!*     ..
!*
!*  =====================================================================
!*
!*  .. External Functions ..
!      LOGICAL DLAISNAN
!      EXTERNAL DLAISNAN
!*  ..
!*  .. Executable Statements ..
      DISNAN = DLAISNAN(DIN,DIN)
     
END FUNCTION

#if 0
*> This routine is not for general use.  It exists solely to avoid
*> over-optimization in DISNAN.
*>
*> DLAISNAN checks for NaNs by comparing its two arguments for
*> inequality.  NaN is the only floating-point value where NaN != NaN
*> returns .TRUE.  To check for NaNs, pass the same variable as both
*> arguments.
*>
*> A compiler must assume that the two arguments are
*> not the same variable, and the test will not be optimized away.
*> Interprocedural or whole-program optimization may delete this
*> test.  The ISNAN functions will be replaced by the correct
*> Fortran 03 intrinsic once the intrinsic is widely available.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] DIN1
*> \verbatim
*>          DIN1 is DOUBLE PRECISION
*> \endverbatim
*>
*> \param[in] DIN2
*> \verbatim
*>          DIN2 is DOUBLE PRECISION
*>          Two numbers to compare for inequality.
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \date June 2017
*
*> \ingroup OTHERauxiliary
*
*  =====================================================================
#endif
#if defined(__GFORTRAN__) && (!defined(__ICC) || !defined(__INTEL_COMPILER))
LOGICAL FUNCTION DLAISNAN( DIN1, DIN2 ) !GCC$ ATTRIBUTES INLINE :: DLAISNAN !GCC$ ATTRIBUTES aligned(32) :: DLAISNAN
#elif defined(__ICC) || defined(__INTEL_COMPILER)
  LOGICAL FUNCTION DLAISNAN(DIN1,DIN2)
    !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: DLAISNAN
    !DIR$ ATTRIBUTES FORCEINLINE :: DLAISNAN
#endif
!*
!*  -- LAPACK auxiliary routine (version 3.7.1) --
!*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!*     June 2017
!*
!*     .. Scalar Arguments ..
      DOUBLE PRECISION, INTENT(IN) :: DIN1, DIN2
!*     ..
!*
!*  =====================================================================
!*
!*  .. Executable Statements ..
      DLAISNAN = (DIN1.NE.DIN2)
     
END FUNCTION
