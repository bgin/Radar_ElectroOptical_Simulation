!
! Compare_Float_Numbers
!
! Module containing routines to perform equality and relational
! comparisons on floating point numbers.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 01-Apr-2003
!                       paul.vandelst@noaa.gov
! Modified by Bernard Gingold (beniekg@gmail.com) on 03/04/2019
!  

MODULE mod_fpcompare


  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE mod_kinds, ONLY: i4, sp, dp
  USE mod_vectypes, ONLY : XMM2i4_t, XMM4i4_t, XMM2r8_t, XMM4r4_t,YMM4r8_t,YMM8r4_t,YMM8i4_t, ZMM16i4_t, ZMM16r4_t,ZMM8r8_t, &
                           Mask2_t, Mask4_t, Mask8_t, Mask16_t
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  ! Parameters
  PUBLIC :: DEFAULT_N_SIGFIG
  ! Operators
  PUBLIC :: OPERATOR (.EqualTo.)
  PUBLIC :: OPERATOR (.GreaterThan.)
  PUBLIC :: OPERATOR (.LessThan.)
 
  ! Procedures
  PUBLIC :: Compare_Float
  PUBLIC :: Tolerance
  PUBLIC :: Compares_Within_Tolerance
  ! Non-overloaded vectorized procedures.
  PUBLIC :: xmm2r8_fpcmp_xmm2r8
  PUBLIC :: xmm4r4_fpcmp_xmm4r4
  PUBLIC :: ymm4r8_fpcmp_ymm4r8
  PUBLIC :: ymm8r4_fpcmp_ymm8r4
  PUBLIC :: zmm16r4_fpcmp_zmm16r4
  PUBLIC :: zmm8r8_fpcmp_zmm8r8
  PUBLIC :: xmm2r8_equalto_xmm2r8
  PUBLIC :: xmm4r4_equalto_xmm4r4
  PUBLIC :: ymm4r8_equalto_ymm4r8
  PUBLIC :: ymm8r4_equalto_ymm8r4
  PUBLIC :: zmm16r4_equalto_zmm16r4
  PUBLIC :: zmm8r8_equalto_zmm8r8
  PUBLIC :: xmm2r8_rgt_xmm2r8
  PUBLIC :: xmm4r4_rgt_xmm4r4
  PUBLIC :: ymm4r8_rgt_ymm4r8
  PUBLIC :: ymm8r4_rgt_ymm8r4
  PUBLIC :: zmm16r4_rgt_zmm16r4
  PUBLIC :: zmm8r8_rgt_zmm8r8
  PUBLIC :: xmm2r8_rlt_xmm2r8
  PUBLIC :: xmm4r4_rlt_xmm4r4
  PUBLIC :: ymm4r8_rlt_ymm4r8
  PUBLIC :: ymm8r4_rlt_ymm8r4
  PUBLIC :: zmm16r4_rlt_zmm16r4
  PUBLIC :: zmm8r8_rlt_zmm8r8
  PUBLIC :: xmm2r8_tol_xmm2r8
  PUBLIC :: xmm4r4_tol_xmm4r4
  PUBLIC :: ymm4r8_tol_ymm4r8
  PUBLIC :: ymm8r4_tol_ymm8r4
  PUBLIC :: zmm16r4_tol_zmm16r4
  PUBLIC :: zmm8r8_tol_zmm8r8
  PUBLIC :: xmm2r8_cwt_xmm2r8
  PUBLIC :: xmm4r4_cwt_xmm4r4
  PUBLIC :: ymm4r8_cwt_ymm4r8
  PUBLIC :: ymm8r4_cwt_ymm8r4
  PUBLIC :: zmm8r8_cwt_zmm8r8
  PUBLIC :: zmm16r4_cwt_zmm16r4
  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  INTERFACE Compare_Float
    MODULE PROCEDURE Compare_Real_Single
    MODULE PROCEDURE Compare_Real_Double
    MODULE PROCEDURE Compare_Complex_Single
    MODULE PROCEDURE Compare_Complex_Double
  END INTERFACE Compare_Float

  INTERFACE OPERATOR (.EqualTo.)
    MODULE PROCEDURE EqualTo_Real_Single
    MODULE PROCEDURE EqualTo_Real_Double
    MODULE PROCEDURE EqualTo_Complex_Single
    MODULE PROCEDURE EqualTo_Complex_Double
  END INTERFACE OPERATOR (.EqualTo.)

  INTERFACE OPERATOR (.GreaterThan.)
    MODULE PROCEDURE Is_Greater_Than_Single
    MODULE PROCEDURE Is_Greater_Than_Double
  END INTERFACE OPERATOR (.GreaterThan.)

  INTERFACE OPERATOR (.LessThan.)
    MODULE PROCEDURE Is_Less_Than_Single
    MODULE PROCEDURE Is_Less_Than_Double
  END INTERFACE OPERATOR (.LessThan.)

  INTERFACE Tolerance
    MODULE PROCEDURE Tolerance_Real_Single
    MODULE PROCEDURE Tolerance_Real_Double
    MODULE PROCEDURE Tolerance_Complex_Single
    MODULE PROCEDURE Tolerance_Complex_Double
  END INTERFACE Tolerance

  INTERFACE Compares_Within_Tolerance
    MODULE PROCEDURE cwt_Real_Single
    MODULE PROCEDURE cwt_Real_Double
    MODULE PROCEDURE cwt_Complex_Single
    MODULE PROCEDURE cwt_Complex_Double
  END INTERFACE Compares_Within_Tolerance

  ! -----------------
  ! Module parameters
  ! -----------------
  ! Module Version Id
  
  character(*), parameter :: module_version_id = &
       '$Id: GMS_mod_fpcompare.f90 -00200 2019-04-03 20:28 Bernard Gingold, contact: beniekg@gmail.com '
  ! PAOS constants
  type(XMM2r8_t), parameter :: XMM2r8_ZERO = XMM2r8_t(0.0_dp)
  type(XMM4r4_t), parameter :: XMM4r4_ZERO = XMM4r4_t(0.0_sp)
  type(YMM4r8_t), parameter :: YMM4r8_ZERO = YMM4r8_t(0.0_dp)
  type(YMM8r4_t), parameter :: YMM8r4_ZERO = YMM8r4_t(0.0_sp)
  type(ZMM8r8_t), parameter :: ZMM8r8_ZERO = ZMM8r8_t(0.0_dp)
  type(ZMM16r4_t), parameter :: ZMM16r4_ZERO = ZMM16r4_t(0.0_sp)
  type(XMM2r8_t), parameter :: XMM2r8_ONE = XMM2r8_t(1.0_dp)
  type(XMM4r4_t), parameter :: XMM4r4_ONE = XMM4r4_t(1.0_sp)
  type(YMM4r8_t), parameter :: YMM4r8_ONE = YMM4r8_t(1.0_dp)
  type(YMM8r4_t), parameter :: YMM8r4_ONE = YMM8r4_t(1.0_sp)
  type(ZMM8r8_t), parameter :: ZMM8r8_ONE = ZMM8r8_t(1.0_dp)
  type(ZMM16r4_t), parameter :: ZMM16r4_ONE = ZMM16r4_t(1.0_sp)
  type(XMM2r8_t), parameter :: XMM2r8_TEN = XMM2r8_t(10.0_dp)
  type(XMM4r4_t), parameter :: XMM4r4_TEN = XMM4r4_t(10.0_sp)
  type(YMM4r8_t), parameter :: YMM4r8_TEN = YMM4r8_t(10.0_dp)
  type(YMM8r4_t), parameter :: YMM8r4_TEN = YMM8r4_t(10.0_sp)
  type(ZMM8r8_t), parameter :: ZMM8r8_TEN = ZMM8r8_t(10.0_dp)
  type(ZMM16r4_t), parameter :: ZMM16r4_TEN = ZMM16r4_t(10.0_sp)
  type(XMM2r8_t), parameter :: XMM2r8_HUNDRED = XMM2r8_t(100.0_dp)
  type(XMM4r4_t), parameter :: XMM4r4_HUNDRED = XMM4r4_t(100.0_sp)
  type(YMM4r8_t), parameter :: YMM4r8_HUNDRED = YMM4r8_t(100.0_dp)
  type(YMM8r4_t), parameter :: YMM8r4_HUNDRED = YMM8r4_t(100.0_sp)
  type(ZMM8r8_t), parameter :: ZMM8r8_HUNDRED = ZMM8r8_t(100.0_dp)
  type(ZMM16r4_t), parameter :: ZMM16r4_HUNDRED = ZMM16r4_t(100.0_sp)
  type(XMM2r8_t), parameter :: XMM2r8_CUTOFF_EPS = XMM2r8_t(1.0e-15_dp)
  type(XMM4r4_t), parameter :: XMM4r4_CUTOFF_EPS = XMM4r4_t(1.0e-15_sp)
  type(YMM4r8_t), parameter :: YMM4r8_CUTOFF_EPS = YMM4r8_t(1.0e-15_dp)
  type(YMM8r4_t), parameter :: YMM8r4_CUTOFF_EPS = YMM8r4_t(1.0e-15_sp)
  type(ZMM8r8_t), parameter :: ZMM8r8_CUTOFF_EPS = ZMM8r8_t(1.0e-15_dp)
  type(ZMM16r4_t), parameter :: ZMM16r4_CUTOFF_EPS = ZMM16r4_t(1.0e-15_sp)
  ! Numeric literals
  REAL(sp), PARAMETER :: SP_ZERO = 0.0_sp
  REAL(dp), PARAMETER :: DP_ZERO = 0.0_dp
  REAL(sp), PARAMETER :: SP_ONE = 1.0_sp
  REAL(dp), PARAMETER :: DP_ONE = 1.0_dp
  REAL(sp), PARAMETER :: SP_TEN = 10.0_sp
  REAL(dp), PARAMETER :: DP_TEN = 10.0_dp
  REAL(sp), PARAMETER :: SP_HUNDRED = 100.0_sp
  REAL(dp), PARAMETER :: DP_HUNDRED = 100.0_dp
  REAL(sp), PARAMETER :: SP_COMPARE_CUTOFF = 1.0e-15_sp
  REAL(dp), PARAMETER :: DP_COMPARE_CUTOFF = 1.0e-15_dp
  ! Default number of significant figures
  INTEGER(kind=i4), PARAMETER :: DEFAULT_N_SIGFIG = 6
  

CONTAINS


!----------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       .EqualTo.
!
! PURPOSE:
!       Relational operator to test the equality of REAL operands.
!
! CALLING SEQUENCE:
!       IF ( x .EqualTo. y ) THEN
!         .....
!       END IF
!
! OPERANDS:
!       x, y:        Two congruent floating point data objects to compare.
!                    UNITS:      N/A
!                    TYPE:       REAL(Single),    REAL(Double)
!                                COMPLEX(Single), COMPLEX(Double)
!                    DIMENSION:  Scalar, or any allowed rank array.
!
! OPERATOR RESULT:
!       (x .EqualTo. y)    The result is a logical value indicating whether
!                          the operands are equal to within numerical precision
!                          UNITS:      N/A
!                          TYPE:       LOGICAL
!                          DIMENSION:  Same as operands.
!
! PROCEDURE:
!       The test performed is
!
!         ABS( x - y ) < SPACING( MAX(ABS(x),ABS(y)) )
!
!       If the result is .TRUE., the numbers are considered equal. For complex
!       input the test is applied separately to the real and imaginary parts.
!:sdoc-:
!----------------------------------------------------------------------------------
!DIR$ ATTRIBUTES INLINE :: EqualTo_Real_Single
  ELEMENTAL FUNCTION EqualTo_Real_Single( x, y ) RESULT( EqualTo )
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: EqualTo_Real_Single
    REAL(kind=sp), INTENT(IN)  :: x, y
    LOGICAL :: EqualTo
    EqualTo = ABS(x-y) < SPACING( MAX(ABS(x),ABS(y)) )
  END FUNCTION EqualTo_Real_Single
  
!DIR$ ATTRIBUTES INLINE :: EqualTo_Real_Double
  ELEMENTAL FUNCTION EqualTo_Real_Double( x, y ) RESULT( EqualTo )
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: EqualTo_Real_Double
    REAL(kind=dp), INTENT(IN)  :: x, y
    LOGICAL :: EqualTo
    EqualTo = ABS(x-y) < SPACING( MAX(ABS(x),ABS(y)) )
  END FUNCTION EqualTo_Real_Double
  
!DIR$ ATTRIBUTES INLINE :: EqualTo_Complex_Single
  ELEMENTAL FUNCTION EqualTo_Complex_Single( x, y ) RESULT( EqualTo )
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: EqualTo_Complex_Single
    COMPLEX(kind=sp), INTENT(IN)  :: x, y
    LOGICAL :: EqualTo
    REAL(kind=sp) :: rx, ix
    REAL(kind=sp) :: ry, iy
    rx = REAL(x,sp); ix = AIMAG(x)
    ry = REAL(y,sp); iy = AIMAG(y)
    EqualTo = EqualTo_Real_Single( rx, ry ) .AND. EqualTo_Real_Single( ix, iy )
  END FUNCTION EqualTo_Complex_Single

!DIR$ ATTRIBUTES INLINE :: EqualTo_Complex_Double
  ELEMENTAL FUNCTION EqualTo_Complex_Double( x, y ) RESULT( EqualTo )
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: EqualTo_Complex_Double
    COMPLEX(kind=dp), INTENT(IN)  :: x, y
    LOGICAL :: EqualTo
    REAL(kind=dp) :: rx, ix
    REAL(kin=dp) :: ry, iy
    rx = REAL(x,dp); ix = AIMAG(x)
    ry = REAL(y,dp); iy = AIMAG(y)
    EqualTo = EqualTo_Real_Double( rx, ry ) .AND. EqualTo_Real_Double( ix, iy )
  END FUNCTION EqualTo_Complex_Double


!----------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       .GreaterThan.
!
! PURPOSE:
!       Relational operator to test if one REAL operand is greater than another.
!
! CALLING SEQUENCE:
!       IF ( x .GreaterThan. y ) THEN
!         .....
!       END IF
!
! OPERANDS:
!       x, y:        Two congruent floating point data objects to compare.
!                    UNITS:      N/A
!                    TYPE:       REAL(Single)   [ == default real]
!                                  OR
!                                REAL(Double)
!                    DIMENSION:  Scalar, or any allowed rank array.
!
! OPERATOR RESULT:
!       (x .GreaterThan. y)    The result is a logical value indicating whether
!                              the operand x is greater than y by more than
!                              the spacing between representable floating point
!                              numbers.
!                              UNITS:      N/A
!                              TYPE:       LOGICAL
!                              DIMENSION:  Same as operands.
!
! PROCEDURE:
!       The test performed is
!
!         ( x - y ) >= SPACING( MAX(ABS(x),ABS(y)) )
!
!       If the result is .TRUE., x is considered greater than y.
!:sdoc-:
!----------------------------------------------------------------------------------

 !DIR$ ATTRIBUTES INLINE :: Is_Greater_Than_Single
  ELEMENTAL FUNCTION Is_Greater_Than_Single( x, y ) RESULT ( Greater_Than )
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: Is_Greater_Than_Single
    REAL(kind=sp), INTENT(IN) :: x, y
    LOGICAL :: Greater_Than
    IF ( (x-y) >= SPACING( MAX( ABS(x), ABS(y) ) ) ) THEN
      Greater_Than = .TRUE.
    ELSE
      Greater_Than = .FALSE.
    END IF
  END FUNCTION Is_Greater_Than_Single

!DIR$ ATTRIBUTES INLINE :: Is_Greater_Than_Double
  ELEMENTAL FUNCTION Is_Greater_Than_Double( x, y ) RESULT ( Greater_Than )
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: Is_Greater_Than_Double
    REAL(kind=dp), INTENT(IN) :: x, y
    LOGICAL :: Greater_Than
    IF ( (x-y) >= SPACING( MAX( ABS(x), ABS(y) ) ) ) THEN
      Greater_Than = .TRUE.
    ELSE
      Greater_Than = .FALSE.
    END IF
  END FUNCTION Is_Greater_Than_Double


!----------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       .LessThan.
!
! PURPOSE:
!       Relational operator to test if one REAL operand is less than another.
!
! CALLING SEQUENCE:
!       IF ( x .LessThan. y ) THEN
!         .....
!       END IF
!
! OPERANDS:
!       x, y:        Two congruent floating point data objects to compare.
!                    UNITS:      N/A
!                    TYPE:       REAL(Single)   [ == default real]
!                                  OR
!                                REAL(Double)
!                    DIMENSION:  Scalar, or any allowed rank array.
!
! OPERATOR RESULT:
!       (x .LessThan. y)    The result is a logical value indicating whether
!                           the operand x is less than y by more than the
!                           spacing between representable floating point
!                           numbers.
!                           UNITS:      N/A
!                           TYPE:       LOGICAL
!                           DIMENSION:  Same as operands.
!
! PROCEDURE:
!       The test performed is
!
!         ( y - x ) >= SPACING( MAX(ABS(x),ABS(y)) )
!
!       If the result is .TRUE., x is considered less than y.
!:sdoc-:
  !----------------------------------------------------------------------------------
!DIR$ ATTRIBUTES INLINE :: Is_Less_Than_Single
  ELEMENTAL FUNCTION Is_Less_Than_Single( x, y ) RESULT ( Less_Than )
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: Is_Less_Than_Single
    REAL(kind=sp), INTENT(IN) :: x, y
    LOGICAL :: Less_Than
    IF ( (y-x) >= SPACING( MAX( ABS(x), ABS(y) ) ) ) THEN
      Less_Than = .TRUE.
    ELSE
      Less_Than = .FALSE.
    END IF
  END FUNCTION Is_Less_Than_Single

!DIR$ ATTRIBUTES INLINE :: Is_Less_Than_Double
  ELEMENTAL FUNCTION Is_Less_Than_Double( x, y ) RESULT ( Less_Than )
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: Is_Less_Than_Double
    REAL(kind=dp), INTENT(IN) :: x, y
    LOGICAL :: Less_Than
    IF ( (y-x) >= SPACING( MAX( ABS(x), ABS(y) ) ) ) THEN
      Less_Than = .TRUE.
    ELSE
      Less_Than = .FALSE.
    END IF
  END FUNCTION Is_Less_Than_Double


!----------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       Compare_Float
!
! PURPOSE:
!       Function to compare floating point scalars and arrays with adjustible
!       precision tolerance.
!
! CALLING SEQUENCE:
!       Result = Compare_Float( x, y,            &  ! Input
!                               ULP    =ULP    , &  ! Optional input
!                               Percent=Percent  )  ! Optional input
!
! INPUT ARGUMENTS:
!       x, y:        Two congruent floating point data objects to compare.
!                    UNITS:      N/A
!                    TYPE:       REAL(Single)   [ == default real]
!                                  OR
!                                REAL(Double)
!                                  OR
!                                COMPLEX(Single)
!                                  OR
!                                COMPLEX(Double)
!                    DIMENSION:  Scalar, or any allowed rank array.
!                    ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ULP:         Unit of data precision. The acronym stands for "unit in
!                    the last place," the smallest possible increment or decrement
!                    that can be made using a machine's floating point arithmetic.
!                    A 0.5 ulp maximum error is the best you could hope for, since
!                    this corresponds to always rounding to the nearest representable
!                    floating-point number. Value must be positive - if a negative
!                    value is supplied, the absolute value is used.
!                    If not specified, the default value is 1.
!                    This argument is ignored if the Percent optioanl argument is specifed.
!                    UNITS:      N/A
!                    TYPE:       INTEGER
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Percent:     Specify a percentage difference value to use in comparing
!                    the numbers rather than testing within some numerical
!                    limit. The ULP argument is ignored if this argument is
!                    specified.
!                    UNITS:      N/A
!                    TYPE:       REAL(Single)  for REAL(Single) or COMPLEX(Single) x,y
!                                  OR
!                                REAL(Double)  for REAL(Double) or COMPLEX(Double) x,y
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! FUNCTION RESULT:
!       Result:      The return value is a logical value indicating whether
!                    the inputs are equal (to within the required precision)
!                    .TRUE.  - if the floating point numbers are equal to
!                              within the specified tolerance. 
!                    .FALSE. - if the floating point numbers are different.
!                    UNITS:      N/A
!                    TYPE:       LOGICAL
!                    DIMENSION:  Scalar
!
! PROCEDURE:
!       ULP Test
!       --------
!       The test performed is
!
!         ABS( x - y ) < ( ULP * SPACING( MAX(ABS(x),ABS(y)) ) )
!
!       If the result is .TRUE., the numbers are considered equal.
!
!       The intrinsic function SPACING(x) returns the absolute spacing of numbers
!       near the value of x,
!
!                      {     EXPONENT(x)-DIGITS(x)
!                      {  2.0                        for x /= 0
!         SPACING(x) = {
!                      {  
!                      {  TINY(x)                    for x == 0
!
!       The ULP optional argument scales the comparison.
!
!       James Van Buskirk and James Giles suggested this method for floating
!       point comparisons in the comp.lang.fortran newsgroup.
!
!
!       Percent Test
!       ------------
!       The test performed is
!
!         100.0 * ABS((x-y)/x) < Percent
!
!       If the result is .TRUE., the numbers are considered equal.
!
!
!       For complex numbers, the same test is applied to both the real and
!       imaginary parts and each result is ANDed.
!:sdoc-:
!----------------------------------------------------------------------------------
!DIR$ ATTRIBUTES INLINE :: Compare_Real_Single
  ELEMENTAL FUNCTION Compare_Real_Single( x, y, ULP, Percent ) RESULT( Compare )
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: Compare_Real_Single
    ! Arguments
    REAL(kind=sp),           INTENT(IN) :: x
    REAL(kind=sp),           INTENT(IN) :: y
    INTEGER(kind=int4)     , OPTIONAL, INTENT(IN) :: ULP
    REAL(kind=sp), OPTIONAL, INTENT(IN) :: Percent
    ! Function result
    LOGICAL :: Compare
    ! Local variables
    LOGICAL      :: ULP_Test
    REAL(kind=sp) :: Rel
    
    ! Set up
    ! ------
    ULP_Test = .TRUE.
    IF ( PRESENT(ULP) ) THEN
      Rel = REAL(ABS(ULP), sp)
    ELSE
      Rel = SP_ONE
    END IF
    IF ( PRESENT(Percent) ) THEN
      ULP_Test = .FALSE.
      ! Test for zero x (elementals can't be recursive)
      IF ( ABS(x) < ( SPACING( MAX(ABS(x),SP_ZERO) ) ) ) ULP_Test = .TRUE.
    END IF
    
    ! Compare the numbers
    ! -------------------
    IF ( ULP_Test ) THEN
      Compare = ABS(x-y) < ( Rel * SPACING( MAX(ABS(x),ABS(y)) ) )
    ELSE
      Compare = SP_HUNDRED*ABS((x-y)/x) < Percent
    END IF
  END FUNCTION Compare_Real_Single

!DIR$ ATTRIBUTES INLINE :: Compare_Real_Double
  ELEMENTAL FUNCTION Compare_Real_Double( x, y, ULP, Percent ) RESULT( Compare )
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: Compare_Real_Double
    ! Arguments
    REAL(kind=dp),           INTENT(IN) :: x
    REAL(kind=dp),           INTENT(IN) :: y
    INTEGER(kind=int4)     , OPTIONAL, INTENT(IN) :: ULP
    REAL(kind=dp), OPTIONAL, INTENT(IN) :: Percent
    ! Function result
    LOGICAL :: Compare
    ! Local variables
    LOGICAL      :: ULP_Test
    REAL(kind=dp) :: Rel
    
    ! Set up
    ! ------
    ULP_Test = .TRUE.
    IF ( PRESENT(ULP) ) THEN
      Rel = REAL(ABS(ULP), dp)
    ELSE
      Rel = DP_ONE
    END IF
    IF ( PRESENT(Percent) ) THEN
      ULP_Test = .FALSE.
      ! Test for zero x (elementals can't be recursive)
      IF ( ABS(x) < ( SPACING( MAX(ABS(x),DP_ZERO) ) ) ) ULP_Test = .TRUE.
    END IF
    
    ! Compare the numbers
    ! -------------------
    IF ( ULP_Test ) THEN
      Compare = ABS(x-y) < ( Rel * SPACING( MAX(ABS(x),ABS(y)) ) )
    ELSE
      Compare = DP_HUNDRED*ABS((x-y)/x) < Percent
    END IF
  END FUNCTION Compare_Real_Double

!DIR$ ATTRIBUTES INLINE :: Compare_Complex_Single
  ELEMENTAL FUNCTION Compare_Complex_Single( x, y, ULP, Percent ) RESULT( Compare )
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: Compare_Complex_Single
    ! Arguments
    COMPLEX(kind=sp),           INTENT(IN) :: x
    COMPLEX(kind=sp),           INTENT(IN) :: y
    INTEGER(kind=int4)        , OPTIONAL, INTENT(IN) :: ULP
    REAL(kind=sp)   , OPTIONAL, INTENT(IN) :: Percent
    ! Function result
    LOGICAL :: Compare
    ! Local variables
    REAL(kind=sp) :: xr, xi
    REAL(kind=sp) :: yr, yi
    
    ! Separate real and complex parts
    ! -------------------------------
    xr=REAL(x,sp); xi=AIMAG(x)
    yr=REAL(y,sp); yi=AIMAG(y)
    
    ! Compare each part separately
    ! ----------------------------
    Compare = Compare_Real_Single(xr,yr,ULP=ULP,Percent=Percent) .AND. &
              Compare_Real_Single(xi,yi,ULP=ULP,Percent=Percent)
  END FUNCTION Compare_Complex_Single

!DIR$ ATTRIBUTES INLINE :: Compare_Complex_Double
  ELEMENTAL FUNCTION Compare_Complex_Double( x, y, ULP, Percent ) RESULT( Compare )
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: Compare_Complex_Double
    ! Arguments
    COMPLEX(kind=dp),           INTENT(IN) :: x
    COMPLEX(kind=dp),           INTENT(IN) :: y
    INTEGER(kind=int4)        , OPTIONAL, INTENT(IN) :: ULP
    REAL(kind=dp)   , OPTIONAL, INTENT(IN) :: Percent
    ! Function result
    LOGICAL :: Compare
    ! Local variables
    REAL(kind=dp) :: xr, xi
    REAL(kind=dp) :: yr, yi
    
    ! Separate real and complex parts
    ! -------------------------------
    xr=REAL(x,dp); xi=AIMAG(x)
    yr=REAL(y,dp); yi=AIMAG(y)
    
    ! Compare each part separately
    ! ----------------------------
    Compare = Compare_Real_Double(xr,yr,ULP=ULP,Percent=Percent) .AND. &
              Compare_Real_Double(xi,yi,ULP=ULP,Percent=Percent)
  END FUNCTION Compare_Complex_Double


!----------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       Tolerance
!
! PURPOSE:
!       Elemental function to compute a tolerance value for a given input for a
!       specified number of significant figures.
!
! CALLING SEQUENCE:
!       Result = Tolerance( x, n )
!
! INPUT ARGUMENTS:
!       x:           Floating point value for which a tolerance value is required.
!                    UNITS:      N/A
!                    TYPE:       REAL(Single)   [ == default real]
!                                  OR
!                                REAL(Double)
!                                  OR
!                                COMPLEX(Single)
!                                  OR
!                                COMPLEX(Double)
!                    DIMENSION:  Scalar or any rank array.
!                    ATTRIBUTES: INTENT(IN)
!
!       n:           The approximate number of significant figures for which the 
!                    tolerance is required.
!                    UNITS:      N/A
!                    TYPE:       INTEGER
!                    DIMENSION:  Scalar or same as input x.
!                    ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Result:      The return value is a tolerance value that can be used to
!                    compare two numbers.
!                    UNITS:      N/A
!                    TYPE:       Same as input x.
!                    DIMENSION:  Same as input x.
!:sdoc-:
!----------------------------------------------------------------------------------
!DIR$ ATTRIBUTES INLINE :: Tolerance_Real_Single
  ELEMENTAL FUNCTION Tolerance_Real_Single(x,n) RESULT( Tolerance )
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: Tolerance_Real_Single
    REAL(kind=sp), INTENT(IN) :: x
    INTEGER(kind=i4)     , INTENT(IN) :: n
    REAL(kind=sp) :: Tolerance
    INTEGER(kind=i4) :: e
    IF (ABS(x) > SP_ZERO) THEN
      e = FLOOR(LOG10(ABS(x))) - n
      Tolerance = SP_TEN**e
    ELSE
      Tolerance = SP_ONE
    END IF
  END FUNCTION Tolerance_Real_Single
  
!DIR$ ATTRIBUTES INLINE :: Tolerance_Real_Double  
  ELEMENTAL FUNCTION Tolerance_Real_Double(x,n) RESULT( Tolerance )
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: Tolerance_Real_Double
    REAL(kind=dp), INTENT(IN) :: x
    INTEGER(kind=i4),      INTENT(IN) :: n
    REAL(kind=dp) :: Tolerance
    INTEGER :: e
    IF (ABS(x) > DP_ZERO) THEN
      e = FLOOR(LOG10(ABS(x))) - n
      Tolerance = DP_TEN**e
    ELSE
      Tolerance = DP_ONE
    END IF
  END FUNCTION Tolerance_Real_Double

!DIR$ ATTRIBUTES INLINE :: Tolerance_Complex_Single
  ELEMENTAL FUNCTION Tolerance_Complex_Single(x,n) RESULT( Tolerance )
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: Tolerance_Complex_Single
    COMPLEX(kind=sp), INTENT(IN) :: x
    INTEGER(kind=i4),         INTENT(IN) :: n
    COMPLEX(kind=sp) :: Tolerance
    REAL(kind=sp) :: tr, ti
    tr = Tolerance_Real_Single(REAL(x,sp),n)
    ti = Tolerance_Real_Single(AIMAG(x),n)
    Tolerance = CMPLX(tr,ti,sp)
  END FUNCTION Tolerance_Complex_Single

!DIR$ ATTRIBUTES INLINE :: Tolerance_Complex_Double
  ELEMENTAL FUNCTION Tolerance_Complex_Double(x,n) RESULT( Tolerance )
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: Tolerance_Complex_Double
    COMPLEX(kind=dp), INTENT(IN) :: x
    INTEGER(kind=i4),         INTENT(IN) :: n
    COMPLEX(kind=dp) :: Tolerance
    REAL(kind=dp) :: tr, ti
    tr = Tolerance_Real_Double(REAL(x,dp),n)
    ti = Tolerance_Real_Double(AIMAG(x),n)
    Tolerance = CMPLX(tr,ti,dp)
  END FUNCTION Tolerance_Complex_Double


!----------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       Compares_Within_Tolerance
!
! PURPOSE:
!       Elemental function to determine if two values are comparable withing
!       a given tolerance determined by the number of significant figures
!       used in the comparison.
!
! CALLING SEQUENCE:
!       Result = Compare_Within_Tolerance( x, y, n, cutoff=cutoff )
!
! INPUTS:
!       x, y:        Floating point values to be compared.
!                    UNITS:      N/A
!                    TYPE:       REAL(Single)   [ == default real]
!                                  OR
!                                REAL(Double)
!                                  OR
!                                COMPLEX(Single)
!                                  OR
!                                COMPLEX(Double)
!                    DIMENSION:  Scalar or any rank array.
!                    ATTRIBUTES: INTENT(IN)
!
!       n:           The approximate number of significant figures for which the 
!                    tolerance is required.
!                    UNITS:      N/A
!                    TYPE:       INTEGER
!                    DIMENSION:  Scalar or same as input x, y.
!                    ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       cutoff:      Floating point value below which the comparison is not
!                    performed. In this case, the function result will be .TRUE.
!                    If not specified, the default value is 1.0e-15 for real
!                    input, or (1.0e-15,1.0e-15) for complex input.
!                    UNITS:      N/A
!                    TYPE:       Same as input x
!                    DIMENSION:  Scalar or same as input x, y.
!                    ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Result:      The return value is a logical value indicating if the 
!                    comparison was successful or not.
!                    If .TRUE. , the two numbers compare within the prescribed
!                                tolerance, or
!                       .FALSE., they do not.
!                    UNITS:      N/A
!                    TYPE:       LOGICAL
!                    DIMENSION:  Same as input x, y.
!:sdoc-:
!----------------------------------------------------------------------------------
!DIR$ ATTRIBUTES INLINE :: cwt_Real_Single
  ELEMENTAL FUNCTION cwt_Real_Single(x,y,n,cutoff) RESULT(is_comparable)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: cwt_Real_Single
    REAL(kind=sp),           INTENT(IN) :: x, y
    INTEGER(kind=i4),                INTENT(IN) :: n
    REAL(kind=sp), OPTIONAL, INTENT(IN) :: cutoff
    LOGICAL :: is_comparable
    REAL(kind=sp) :: c
    IF ( PRESENT(cutoff) ) THEN
      c = cutoff
    ELSE
      c = SP_COMPARE_CUTOFF
    END IF
    is_comparable = .TRUE.
    IF ( ABS(x) > c .OR. ABS(y) > c ) is_comparable = ABS(x-y) < Tolerance(x,n)
  END FUNCTION cwt_Real_Single

!DIR$ ATTRIBUTES INLINE :: cwt_Real_Double
  ELEMENTAL FUNCTION cwt_Real_Double(x,y,n,cutoff) RESULT(is_comparable)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: cwt_Real_Double
    REAL(kind=dp),           INTENT(IN) :: x, y
    INTEGER(kind=i4),                INTENT(IN) :: n
    REAL(kind=dp), OPTIONAL, INTENT(IN) :: cutoff
    LOGICAL :: is_comparable
    REAL(kind=dp) :: c
    IF ( PRESENT(cutoff) ) THEN
      c = cutoff
    ELSE
      c = DP_COMPARE_CUTOFF
    END IF
    is_comparable = .TRUE.
    IF ( ABS(x) > c .OR. ABS(y) > c ) is_comparable = ABS(x-y) < Tolerance(x,n)
  END FUNCTION cwt_Real_Double

!DIR$ ATTRIBUTES INLINE :: cwt_Complex_Single
  ELEMENTAL FUNCTION cwt_Complex_Single(x,y,n,cutoff) RESULT(is_comparable)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: cwt_Complex_Single
    COMPLEX(kind=sp),           INTENT(IN) :: x, y
    INTEGER(kind=i4),                   INTENT(IN) :: n
    COMPLEX(kind=sp), OPTIONAL, INTENT(IN) :: cutoff
    LOGICAL :: is_comparable
    COMPLEX(kind=sp) :: c
    IF ( PRESENT(cutoff) ) THEN
      c = cutoff
    ELSE
      c = CMPLX(SP_COMPARE_CUTOFF,SP_COMPARE_CUTOFF,sp)
    END IF
    is_comparable = cwt_Real_Single(REAL(x,sp),REAL(y,sp),n,cutoff=REAL(c,sp) ) .AND. &
                    cwt_Real_Single(AIMAG(x),AIMAG(y),n,cutoff=AIMAG(c))
  END FUNCTION cwt_Complex_Single

!DIR$ ATTRIBUTES INLINE :: cwt_Complex_Double
  ELEMENTAL FUNCTION cwt_Complex_Double(x,y,n,cutoff) RESULT(is_comparable)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: cwt_Complex_Double
    COMPLEX(kind=dp),           INTENT(IN) :: x, y
    INTEGER(kind=i4),                   INTENT(IN) :: n
    COMPLEX(kind=dp), OPTIONAL, INTENT(IN) :: cutoff
    LOGICAL :: is_comparable
    COMPLEX(kind=dp) :: c
    IF ( PRESENT(cutoff) ) THEN
      c = cutoff
    ELSE
      c = CMPLX(DP_COMPARE_CUTOFF,DP_COMPARE_CUTOFF,dp)
    END IF
    is_comparable = cwt_Real_Double(REAL(x,dp),REAL(y,dp),n,cutoff=REAL(c,dp) ) .AND. &
                    cwt_Real_Double(AIMAG(x),AIMAG(y),n,cutoff=AIMAG(c))
  END FUNCTION cwt_Complex_Double

!============================================================================================!
!                                   Vector (PAOS) functions
!============================================================================================!

!DIR$ ATTRIBUTES INLINE :: xmm2r8_equalto_xmm2r8
  pure elemental function xmm2r8_equalto_xmm2r8(x,y) result(compare)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: xmm2r8_equalto_xmm2r8
!DIR$ ATTRIBUTES VECTOR        :: xmm2r8_equalto_xmm2r8
    type(XMM2r8_t),      intent(in) :: x
    type(XMM2r8_t),      intent(in) :: y
    ! Locals/return
    type(Mask2_t),  automatic :: compare
    ! EXec code....
     compare.m = ABS(x.v-y.v) < SPACING(MAX(ABS(x.v),ABS(y.v)))
  end function xmm2r8_equalto_xmm2r8

!DIR$ ATTRIBUTES INLINE :: xmm4r4_equalto_xmm4r4   
  pure elemental function xmm4r4_equalto_xmm4r4(x,y) result(compare)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: xmm4r4_equalto_xmm4r4
!DIR$ ATTRIBUTES VECTOR        :: xmm4r4_equalto_xmm4r4
    type(XMM4r4_t),       intent(in) :: x
    type(XMM4r4_t),       intent(in) :: y
    ! Locals/return

    type(Mask4_t), automatic :: compare
    ! Exec code ....
    compare.m = ABS(x.v-y.v) < SPACING(MAX(ABS(x.v),ABS(y.v)))
  end function xmm4r4_equalto_xmm4r4

!DIR$ ATTRIBUTES INLINE :: ymm4r8_equalto_ymm4r8
  pure elemental function ymm4r8_equalto_ymm4r8(x,y) result(compare)
    !DIR$ ATTRIBUTES CODE_ALIGN:32 :: ymm4r8_equalto_ymm4r8
    !DIR$ ATTRIBUTES VECTOR :: ymm4r8_equalto_ymm4r8
    type(YMM4r8_t),        intent(in) :: x
    type(YMM4r8_t),        intent(in) :: y
    ! Locals/return
    type(Mask4_t) :: compare
    compare.m = ABS(x.v-y.v) < SPACING(MAX(ABS(x.v),ABS(y.v)))
  end function ymm4r8_equalto_ymm4r8

  !DIR$ ATTRIBUTES INLINE :: ymm8r4_equalto_ymm8r4
  pure elemental function ymm8r4_equalto_ymm8r4(x,y) result(compare)
    !DIR$ ATTRIBUTES CODE_ALIGN:32 :: ymm8r4_equalto_ymm8r4
    !DIR$ ATTRIBUTES VECTOR :: ymm8r4_equalto_ymm8r4
    type(YMM8r4_t),        intent(in) :: x
    type(YMM8r4_t),        intent(in) :: y
    ! Locals/return
    type(Mask8_t) :: compare
    ! Exec code ...
    compare.m = ABS(x.v-y.v) < SPACING(MAX(ABS(x.v),ABS(y.v)))
  end function ymm8r4_equalto_ymm8r4

  !DIR$ ATTRIBUTES INLINE :: zmm16r4_equalto_zmm16r4
  pure elemental function zmm16r4_equalto_zmm16r4(x,y) result(compare)
    !DIR$ ATTRIBUTES CODE_ALIGN:32 :: zmm16r4_equalto_zmm16r4
    !DIR$ ATTRIBUTES VECTOR :: zmm16r4_equalto_zmm16r4
    type(ZMM16r4_t),       intent(in) :: x
    type(ZMM16r4_t),       intent(in) :: y
    ! Locals/return
    type(Mask16_t) :: compare
    ! Exec code ....
    compare.m = ABS(x.v-y.v) < SPACING(MAX(ABS(x.v),ABS(y.v)))
  end function zmm16r4_equalto_zmm16r4

  !DIR$ ATTRIBUTES INLINE :: zmm8r8_equalto_zmm8r8
  pure elemental function zmm8r8_equalto_zmm8r8(x,y) result(compare)
    !DIR$ ATTRIBUTES CODE_ALIGN:32 :: zmm8r8_equalto_zmm8r8
    !DIR$ ATTRIBUTES VECTOR :: zmm8r8_equalto_zmm8r8
    type(ZMM8r8_t),        intent(in) :: x
    type(ZMM8r8_t),        intent(in) :: y
    ! Locals
    type(Mask8_t) :: compare
    compare.m = ABS(x.v-y.v) < SPACING(MAX(ABS(x.v),ABS(y.v)))
  end function zmm8r8_equalto_zmm8r8

  !DIR$ ATTRIBUTES INLINE :: xmm2r8_rgt_xmm2r8
  pure elemental function xmm2r8_rgt_xmm2r8(x,y) result(gt)
    !DIR$ ATTRIBUTES CODE_ALIGN:32 :: xmm2r8_rgt_xmm2r8
    !DIR$ ATTRIBUTES VECTOR :: xmm2r8_rgt_xmm2r8
    type(XMM2r8_t),      intent(in) :: x
    type(XMM2r8_t),      intent(in) :: y
    ! Locals
    type(Mask2_t) :: gt
    type(Mask2_t), automatic :: vtmp
    
    ! Exec code ....
    vtmp.m = (x.v-y.v) >= SPACING(MAX(ABS(x.v),ABS(y.v)))
    if( vtmp.m(0) .and. vtmp.m(1)) then
       gt.v = .true.
    else
       gt.v = .false.
    end if
  end function xmm2r8_rgt_xmm2r8

  !DIR$ ATTRIBUTES INLINE :: xmm4r4_rgt_xmm4r4
  pure elemental function xmm4r4_rgt_xmm4r4(x,y) result(gt)
    !DIR$ ATTRIBUTES CODE_ALIGN:32 :: xmm4r4_rgt_xmm4r4
    !DIR$ ATTRIBUTES VECTOR :: xmm4r4_rgt_xmm4r4
    type(XMM4r4_t),     intent(in) :: x
    type(XMM4r4_t),     intent(in) :: y
    ! Locals
    type(Mask4_t) :: gt
    type(Mask4_t), automatic :: vtmp
    ! Exec code ....
    vtmp.m = (x.v-y.v) >= SPACING(MAX(ABS(x.v),ABS(y.v)))
    if(vtmp.m(0).and.vtmp.m(1).and. &
         vtmp.m(2).and.vtmp.m(3)) then
            gt.m = .true.
    else
            gt.m = .false.
    end if
  end function xmm4r4_rgt_xmm4r4

  !DIR$ ATTRIBUTES INLINE :: ymm4r8_rgt_ymm4r8
  pure elemental function ymm4r8_rgt_ymm4r8(x,y) result(gt)
    !DIR$ ATTRIBUTES CODE_ALIGN:32 :: ymm4r8_rgt_ymm4r8
    !DIR$ ATTRIBUTES VECTOR  :: ymm4r8_rgt_ymm4r8
    type(YMM4r8_t),          intent(in) :: x
    type(YMM4r8_t),          intent(in) :: y
    ! Locals
    type(Mask4_t) :: gt
    type(Mask4_t), automatic :: vtmp
    ! Exec code ....
    vtmp.m = (x.v-y.v) >= SPACING(MAX(ABS(x.v),ABS(y.v)))
    if(vtmp.m(0).and.vtmp.m(1).and. &
         vtmp.m(2).and.vtmp.m(3)) then
       gt.m = .true.
    else
       gt.m = .false.
    end if
  end function ymm4r8_rgt_ymm4r8

  !DIR$ ATTRIBUTES INLINE :: ymm8r4_rgt_ymm8r4
  pure elemental function ymm8r4_rgt_ymm8r4(x,y) result(gt)
    !DIR$ ATTRIBUTES CODE_ALIGN:32 :: ymm8r4_rgt_ymm8r4
    !DIR$ ATTRIBUTES VECTOR :: ymm8r4_rgt_ymm8r4
    type(YMM8r4_t),       intent(in) :: x
    type(YMM8r4_t),       intent(in) :: y
    ! Locals
    type(Mask8_t) :: gt
    type(Mask8_t), automatic :: vtmp
   
    ! Exec code ....
    vtmp.m = (x.v-y.v) >= SPACING(MAX(ABS(x.v),ABS(y.v)))
   
    if(ALL(vtmp.m)) then
       gt.m = .true.
    else
       gt.m = .false.
    end if
  end function ymm8r4_rgt_ymm8r4

  !DIR$ ATTRIBUTES INLINE :: zmm16r4_rgt_zmm16r4
  pure elemental function zmm16r4_rgt_zmm16r4(x,y) result(gt)
    !DIR$ ATTRIBUTES CODE_ALIGN:32 :: zmm16r4_rgt_zmm16r4
    !DIR$ ATTRIBUTES VECTOR :: zmm16r4_rgt_zmm16r4
    type(ZMM16r4_t),   intent(in) :: x
    type(ZMM16r4_t),   intent(in) :: y
    ! Locals
    type(Mask16_t) :: gt
    type(Mask16_t), automatic :: vtmp
  
    ! Exec code ....
    vtmp.m = (x.v-y.v) >= SPACING(MAX(ABS(x.v),ABS(y.v)))
   
    if(ALL(vtmp.m)) then
       gt.m = .true.
    else
       gt.m = .false.
    end if
  end function zmm16r4_rgt_zmm16r4

  !DIR$ ATTRIBUTES INLINE :: zmm8r8_rgt_zmm8r8
  pure elemental function zmm8r8_rgt_zmm8r8(x,y) result(gt)
    !DIR$ ATTRIBUTES CODE_ALIGN:32 :: zmm8r8_rgt_zmm8r8
    !DIR$ ATTRIBUTES VECTOR :: zmm8r8_rgt_zmm8r8
    type(ZMM8r8_t),      intent(in) :: x
    type(ZMM8r8_t),      intent(in) :: y
    ! Locals
    type(Mask8_t) :: gt
    type(Mask8_t), automatic :: vtmp
   
    ! Exec code .....
    vtmp.m = (x.v-y.v) >= SPACING(MAX(ABS(x.v),ABS(y.v)))
   
    if(ALL(vtmp.m)) then
       gt.m = .true.
    else
       gt.m = .false.
    end if
  end function zmm8r8_rgt_zmm8r8

  !DIR$ ATTRIBUTES INLINE :: xmm2r8_rlt_xmm2r8
  pure elemental function xmm2r8_rlt_xmm2r8(x,y) result(gt)
    !DIR$ ATTRIBUTES CODE_ALIGN:32 :: xmm2r8_rlt_xmm2r8
    !DIR$ ATTRIBUTES VECTOR :: xmm2r8_rlt_xmm2r8
    type(XMM2r8_t),        intent(in) :: x
    type(XMM2r8_t),        intent(in) :: y
    ! Locals
    type(Mask2_t) :: gt
    type(Mask2_t), automatic :: vtmp
    ! Exec code ....
    vtmp.m = (y.v-x.v) >= SPACING(MAX(ABS(x.v),ABS(y.v)))
    if(ALL(vtmp.m)) then
       gt.m = .true.
    else
       gt.m = .false.
    end if
  end function xmm2r8_rlt_xmm2r8

  !DIR$ ATTRIBUTES INLINE :: xmm4r4_rlt_xmm4r4
  pure elemental function xmm4r4_rlt_xmm4r4(x,y) result(gt)
    !DIR$ ATTRIBUTES CODE_ALIGN:32 :: xmm4r4_rlt_xmm4r4
    !DIR$ ATTRIBUTES VECTOR :: xmm4r4_rlt_xmm4r4
    type(XMM4r4_t),       intent(in) :: x
    type(XMM4r4_t),       intent(in) :: y
    ! Locals
    type(Mask4_t) :: gt
    type(Mask4_t), automatic :: vtmp
   
    !  EXec code.....
    vtmp.m = (y.v-x.v) >= SPACING(MAX(ABS(x.v),ABS(y.v)))
   
    if(ALL(vtmp.m)) then
       gt.m = .true.
    else
       gt.m = .false.
    end if
  end function xmm4r4_rlt_xmm4r4

  !DIR$ ATTRIBUTES INLINE :: ymm4r8_rlt_ymm4r8
  pure elemental function ymm4r8_rlt_ymm4r8(x,y) result(gt)
    !DIR$ ATTRIBUTES CODE_ALIGN:32 :: ymm4r8_rlt_ymm4r8
    !DIR$ ATTRIBUTES VECTOR :: ymm4r8_rlt_ymm4r8
    type(YMM4r8_t),       intent(in) :: x
    type(YMM4r8_t),       intent(in) :: y
    ! Locals
    type(Mask4_t) :: gt
    type(Mask4_t), automatic :: vtmp
  
    ! Exec code ....
    vtmp.m = (y.v-x.v) >= SPACING(MAX(ABS(x.v),ABS(y.v)))
   
       
    if(ALL(vtmp.m)) then
       gt.m = .true.
    else
       gt.m = .false.
    end if
  end function ymm4r8_rlt_ymm4r8

  !DIR$ ATTRIBUTES INLINE :: ymm8r4_rlt_ymm8r4
  pure elemental function ymm8r4_rlt_ymm8r4(x,y) result(gt)
    !DIR$ ATTRIBUTES CODE_ALIGN:32 :: ymm8r4_rlt_ymm8r4
    !DIR$ ATTRIBUTES VECTOR :: ymm8r4_rlt_ymm8r4
    type(YMM8r4_t),      intent(in) :: x
    type(YMM8r4_t),      intent(in) :: y
    ! Locals
    type(Mask8_t) :: gt
    type(Mask8_t), automatic :: vtmp
   
    ! Exec code ...
    vtmp.m = (y.v-x.v) >= SPACING(MAX(ABS(x.v),ABS(y.v)))
   
    if(ALL(vtmp.m)) then
       gt.m = .true.
    else
       gt.m = .false.
    end if
  end function ymm8r4_rlt_ymm8r4

  !DIR$ ATTRIBUTES INLINE :: zmm8r8_rlt_zmm8r8
  pure elemental function zmm8r8_rlt_zmm8r8(x,y) result(gt)
    !DIR$ ATTRIBUTES CODE_ALIGN:32 :: zmm8r8_rlt_zmm8r8
    !DIR$ ATTRIBUTES VECTOR :: zmm8r8_rlt_zmm8r8
    type(ZMM8r8_t),        intent(in) :: x
    type(ZMM8r8_t),        intent(in) :: y
    ! Locals
    type(Mask8_t) :: gt
    type(Mask8_t), automatic :: vtmp
   
    ! EXec code .....
    vtmp.m = (y.v-x.v) >= SPACING(MAX(ABS(x.v),ABS(y.v)))
   
    if(ALL(vtmp.m)) then
       gt.m = .true.
    else
       gt.m = .false.
    end if
  end function zmm8r8_rlt_zmm8r8

  !DIR$ ATTRIBUTES INLINE :: zmm16r4_rlt_zmm16r4
  pure elemental function zmm16r4_rlt_zmm16r4(x,y) result(gt)
    !DIR$ ATTRIBUTES CODE_ALIGN:32 :: zmm16r4_rlt_zmm16r4
    !DIR$ ATTRIBUTES VECTOR :: zmm16r4_rlt_zmm16r4
    type(ZMM16r4_t),       intent(in) :: x
    type(ZMM16r4_t),       intent(in) :: y
    ! lLocals
    type(Mask16_t) :: gt
    type(Mask16_t), automatic :: vtmp
   
    ! Exec code....
    vtmp.m = (y.v-x.v) >= SPACING(MAX(ABS(x.v),ABS(y.v)))
   
    if(ALL(vtmp.m)) then
       gt.m = .true.
    else
       gt.m = .false.
    end if
  end function zmm16r4_rlt_zmm16r4

  !DIR$ ATTRIBUTES INLINE :: xmm2r8_tol_xmm2r8
  pure elemental function xmm2r8_tol_xmm2r8(x,n) result(tol)
    !DIR$ ATTRIBUTES CODE_ALIGN:32 :: xmm2r8_tol_xmm2r8
    !DIR$ ATTRIBUTES VECTOR :: xmm2r8_tol_xmm2r8
    type(XMM2r8_t),     intent(in) :: x
    type(XMM2i4_t),     intent(in) :: n
    ! Locals/eeturn
    type(XMM2r8_t) :: tol
    type(XMM2i4_t), automatic :: e
    type(Mask2_t), automatic :: vtmp
   
    ! Exec code ....
    vtmp.m = ABS(x.v) > XMM2r8_ZERO.v
   
    if(ALL(vtmp.m)) then
       e.v = FLOOR(LOG10(ABS(x.v))) - n.v
       tol.v = XMM2r8_TEN.v**e.v
    else
       tol.v = XMM2r8_ONE.v
    end if
  end function xmm2r8_tol_xmm2r8

  !DIR$ ATTRIBUTES INLINE :: xmm4r4_tol_xmm4r4
  pure elemental function xmm4r4_tol_xmm4r4(x,n) result(tol)
    !DIR$ ATTRIBUTES CODE_ALIGN:32 :: xmm4r4_tol_xmm4r4
    !DIR$ ATTRIBUTES VECTOR :: xmm4r4_tol_xmm4r4
    type(XMM4r4_t),     intent(in) :: x
    type(XMM4i4_t),     intent(in) :: n
    ! Locals/returns
    type(XMM4r4_t) :: tol
    type(XMM4i4_t), automatic :: e
    type(Mask_t),   automatic :: vtmp
   
    ! EXec code ....
    vtmp.m = ABS(x.v) > XMM4r4_ZERO.v
   
    if(ALL(vtmp.m)) then
       e.v = FLOOR(LOG10(ABS(x.v))) - n.v
       tol.v = XMM4r4_TEN.v**e.v
    else
       tol.v = XMM4r4_ONE.v
    end if
  end function xmm4r4_tol_xmm4r4

  !DIR$ ATTRIBUTES INLINE :: ymm4r8_tol_ymm4r8
  pure elemental function ymm4r8_tol_ymm4r8(x,n) result(tol)
    !DIR$ ATTRIBUTES CODE_ALIGN:32 :: ymm4r8_tol_ymm4r8
    !DIR$ ATTRIBUTES VECTOR :: ymm4r8_tol_ymm4r8
    type(YMM4r8_t),      intent(in) :: x
    type(XMM4i4_t),      intent(in) :: n
    ! Locals
    type(YMM4r8_t) :: tol
    type(XMM4i4_t), automatic ::  e
    type(Mask8_t),  automatic :: vtmp
    
    ! EXec code ....
    vtmp.m = ABS(x.v) > YMM4r8_ZERO.v
   
    if(ALL(vtmp.m)) then
       e.v = FLOOR(LOG10(ABS(x.v))) - n.v
       tol.v = YMM4r8_TEN.v**e.v
    else
       tol.v = YMM4r8_ONE.v
    end if
  end function ymm4r8_tol_ymm4r8

  !DIR$ ATTRIBUTES INLINE ::  ymm8r4_tol_ymm8r4
  pure elemental function ymm8r4_tol_ymm8r4(x,n) result(tol)
    !DIR$ ATTRIBUTES CODE_ALIGN:32 :: ymm8r4_tol_ymm8r4
    !DIR$ ATTRIBUTES VECTOR :: ymm8r4_tol_ymm8r4
    type(YMM8r4_t),                     intent(in) :: x
    type(YMM8i4_t),                     intent(in) :: n
    ! LOcals
    type(YMM8r4_t) :: tol
    type(YMM8i4_t), automatic :: e
    type(Mask8_t), automatic :: vtmp
   
    ! Exec code ....
    vtmp.m = ABS(x.v) > YMM8r4_ZERO.v
   
    if(ALL(vtmp.m))) then
       e.v  = FLOOR(LOG10(ABS(x.v))) - n.v
       tol.v = YMM8r4_TEN**e.v
    else
       tol.v = YMM8r4_ONE.v
    end if
  end function ymm8r4_tol_ymm8r4

  !DIR$ ATTRIBUTES INLINE :: zmm16r4_tol_zmm16r4
  pure elemental function zmm16r4_tol_zmm16r4(x,n) result(tol)
    !DIR$ ATTRIBUTES CODE_ALIGN:32 :: zmm16r4_tol_zmm16r4
    !DIR$ ATTRIBUTES VECTOR :: zmm16r4_tol_zmm16r4
    type(ZMM16r4_t),                     intent(in) :: x
    type(ZMM16i4_t),                     intent(in) :: n
    ! Locals
!DIR$ ATTRIBUTES ALIGN : 64 :: tol
    type(ZMM16r4_t) :: tol
!DIR$ ATTRIBUTES ALIGN : 64 :: e
    type(ZMM16i4_t),  automatic :: e
!DIR$ ATTRIBUTES ALIGN : 64 :: vtmp
    type(Mask16_t), automatic :: vtmp
    
    ! EXec code ....
    vtmp.m = ABS(x.v) > ZMM16r4_ZERO.v
   
    if(ALL(vtmp.m))) then
       e.v = FLOOR(LOG10(ABS(x.v))) - n.v
       tol.v = ZMM16r4_TEN**e.v
    else
       tol.v = ZMM16r4_ONE.v
    end if
    
  end function zmm16r4_tol_zmm16r4

  !DIR$ ATTRIBUTES INLINE :: zmm8r8_tol_zmm8r8
  pure elemental function zmm8r8_tol_zmm8r8(x,n) result(tol)
    !DIR$ ATTRIBUTES CODE_ALIGN:32 :: zmm8r8_tol_zmm8r8
    !DIR$ ATTRIBUTES VECTOR :: zmm8r8_tol_zmm8r8
    type(ZMM8r8_t),                     intent(in) :: x
    integer(kind=int4), dimension(0:7), intent(in) :: n
    ! Locals
!DIR$  ATTRIBUTES ALIGN : 64 :: tol
    type(ZMM8r8_t) :: tol
!DIR$ ATTRIBUTES ALIGN : 64 :: e
    integer(kind=int4), dimension(0:7), automatic :: e
    !DIR$ ATTRIBUTES ALIGN : 64 :: vtmp
    type(Mask8_t), automatic :: vtmp
  
    ! EXec code ....
    vtmp.m = ABS(x.v) > ZMM8r8_ZERO.v
    
    if(ALL(vtmp.m))) then
       e = FLOOR(LOG10(ABS(x.v))) - n
       tol.v = ZMM8r8_TEN**e
    else
       tol.v = ZMM8r8_ONE.v
    end if
  end function zmm8r8_tol_zmm8r8

  !DIR$ ATTRIBUTES INLINE ::  xmm2r8_cwt_xmm2r8
  pure elemental function xmm2r8_cwt_xmm2r8(x,y,n,cutoff) result(isc)
    !DIR$ ATTRIBUTES CODE_ALIGN:32 :: xmm2r8_cwt_xmm2r8
    !DIR$ ATTRIBUTES VECTOR :: xmm2r8_cwt_xmm2r8
    type(XMM2r8_t),        intent(in) :: x
    type(XMM2r8_t),        intent(in) :: y
    type(XMM2i4_t),        intent(in) :: n
    type(XMM2r8_t),        intent(in), optional :: cutoff
    ! LOcals
    type(XMM2r8_t), automatic :: c,t
    type(Mask2_t) :: isc
    
    type(Mask2_t), automatic :: vtmp1,vtmp2
   
    ! EXec code ....
    if(present(cutoff)) then
       c = cutoff
    else
       c = XMM2r8_CUTOFF_EPS
    end if
    isc = .true.
    vtmp1.m = ABS(x.v) > c.v
   
    vtmp2.m = ABS(y.v) > c.v
 
    if(ALL(vtmp1.m).or.ALL(vtmp2.m))) then
       t = xmm2r8_tol_xmm2r8(x,n)
       isc.m = ABS(x.v-y.v) < t.v
    end if
  end function xmm2r8_cwt_xmm2r8

!DIR$ ATTRIBUTES INLINE :: xmm4r4_cwt_xmm4r4
  pure elemental function xmm4r4_cwt_xmm4r4(x,y,n,cutoff) result(isc)
    !DIR$ ATTRIBUTES CODE_ALIGN:32 :: xmm4r4_cwt_xmm4r4
    !DIR$ ATTRIBUTES VECTOR :: xmm4r4_cwt_xmm4r4
    type(XMM4r4_t),       intent(in) :: x
    type(XMM4r4_t),       intent(in) :: y
    type(XMM4i4_t),       intent(in) :: n
    type(XMM4r4_t),       intent(in), optional :: cutoff
    ! Locals
    type(Mask4_t) :: isc
    type(XMM4r4_t), automatic :: c,t
    type(Mask2_t), automatic :: vtmp1,vtmp2
   
    ! EXec code ....
    if(present(cutoff)) then
       c = cutoff
    else
       c = XMM4r4_CUTOFF_EPS
    end if
    isc = .true.
    vtmp1.m = ABS(x.v) > c.v
  
    vtmp2.m = ABS(y.v) > c.v
   
    if(ALL(vtmp1.m).or.ALL(vtmp2.m))) then
       t = xmm4r4_tol_xmm4r4(x,n)
       isc.m = ABS(x.v-y.v) < t.v
    end if
  end function xmm4r4_cwt_xmm4r4

 !DIR$ ATTRIBUTES INLINE :: ymm4r8_cwt_ymm4r8
  pure elemental function ymm4r8_cwt_ymm4r8(x,y,n,cutoff) result(isc)
    !DIR$ ATTRIBUTES CODE_ALIGN:32 :: ymm4r8_cwt_ymm4r8
    !DIR$ ATTRIBUTES VECTOR :: ymm4r8_cwt_ymm4r8
    type(YMM4r8_t),      intent(in) :: x
    type(YMM4r8_t),      intent(in) :: y
    type(XMM4i4_t),      intent(in) :: n
    type(YMM4r8_t),      intent(in), optional :: cutoff
    ! Locals
    type(Mask4_t) :: isc
 !DIR$ ATTRIBUTES ALIGN : 32 :: c,t
    type(YMM4r8_t), automatic :: c,t
    type(Mask4_t),  automatic :: vtmp1,vtmp2

    ! Exec code ...
    if(present(cutoff)) then
       c = cutoff
    else
       c = YMM4r8_CUTOFF_EPS
    end if
    isc = .true.
    vtmp1.m = ABS(x.v) > c.v
  
    vtmp2.m = ABS(y.v) > c.v
   
    if(ALL(vtmp1.m).or.ALL(vtmp2.m))) then
       t = ymm4r8_tol_ymm4r8(x,n)
       isc.m = ABS(x.v-y.v) < t.v
    end if
  end function ymm4r8_cwt_ymm4r8

  !DIR$ ATTRIBUTES INLINE :: ymm8r4_cwt_ymm8r4
  pure elemental function ymm8r4_cwt_ymm8r4(x,y,n,cutoff) result(isc)
    !DIR$ ATTRIBUTES CODE_ALIGN:32 :: ymm8r4_cwt_ymm8r4
    !DIR$ ATTRIBUTES VECTOR :: ymm8r4_cwt_ymm8r4
    type(YMM8r4_t),       intent(in) :: x
    type(YMM8r4_t),       intent(in) :: y
    type(YMM8i4_t),       intent(in) :: n
    type(YMM8r4_t),       intent(in), optional :: cutoff
    ! LOcals
    type(Mask8_t) :: isc
    !DIR$ ATTRIBUTES ALIGN : 32 :: c,t
    type(YMM8r4_t), automatic :: c,t
    type(Mask8_t),  automatic :: vtmp1,vtmp2
  
    ! EXec code ....
    if(present(cutoff)) then
       c = cutoff
    else
       c = YMM8r4_CUTOFF_EPS
    end if
    isc = .true.
    vtmp1.m = ABS(x.v) > c.v
   
    vtmp2.m = ABS(y.v) > c.v
    
    if(ALL(vtmp1.m).or.ALL(vtmp2.m))) then
       t = ymm8r4_tol_ymm8r4(x,n)
       isc.m = ABS(x.v-y.v) < t.v
    end if
  end function ymm8r4_cwt_ymm8r4

  !DIR$ ATTRIBUTES INLINE :: zmm8r8_cwt_zmm8r8
  pure elemental function zmm8r8_cwt_zmm8r8(x,y,n,cutoff) result(isc)
    !DIR$ ATTRIBUTES CODE_ALIGN:32 :: zmm8r8_cwt_zmm8r8
    !DIR$ ATTRIBUTES VECTOR :: zmm8r8_cwt_zmm8r8
    type(ZMM8r8_t),        intent(in) :: x
    type(ZMM8r8_t),        intent(in) :: y
    type(YMM8i4_t),        intent(in) :: n
    type(ZMM8r8_t),        intent(in), optional :: cutoff
    !Locals
    type(Mask8_t) :: isc
    !DIR$ ATTRIBUTES ALIGN : 64 :: c,t
    type(ZMM8r8_t), automatic :: c,t
    type(Mask8_t),  automatic :: vtmp1,vtmp2
   
    ! Exec code ...
    if(present(cutoff)) then
       c = cutoff
    else
       c = ZMM8r8_CUTOFF_EPS
    end if
    isc = .true.
    vtmp1.m = ABS(x.v) > c.v
    
    vtmp2.m = ABS(y.v) > c.v
    
    if(ALL(vtmp1.m).or.ALL(vtmp2.m))) then
       t = zmm8r8_tol_zmm8r8(x,n)
       isc.m = ABS(x.v-y.v) < t.v
    end if
  end function zmm8r8_cwt_zmm8r8

  !DIR$ ATTRIBUTES INLINE :: zmm16r4_cwt_zmm16r4
  pure elemental function zmm16r4_cwt_zmm16r4(x,y,n,cutoff) result(isc)
    !DIR$ ATTRIBUTES CODE_ALIGN:32 :: zmm16r4_cwt_zmm16r4
    !DIR$ ATTRIBUTES VECTOR :: zmm16r4_cwt_zmm16r4
    type(ZMM16r4_t),       intent(in) :: x
    type(ZMM16r4_t),       intent(in) :: y
    type(ZMM16i4_t),       intent(in) :: n
    type(ZMM16r4_t),       intent(in), optional :: cutoff
    ! Locals
    type(Mask16_t) :: isc
    !DIR$ ATTRIBUTES ALIGN : 64 :: c,t
    type(ZMM16r4_t), automatic :: c,t
    type(Mask16_t),  automatic :: vtmp1,vtmp2

    ! Exec code ......
    if(present(cutoff)) then
       c = cutoff
    else
       c = ZMM16r4_CUTOFF_EPS
    end if
    isc = .true.
    vtmp1.m = ABS(x.v) > c.v
   
    vtmp2.m = ABS(y.v) > c.v
   
    if(ALL(vtmp1.m).or.ALL(vtmp2.m))) then
       t = zmm16r4_tol_zmm16r4(x,n)
       isc.m = ABS(x.v-y.v) < t.v
    end if
  end function zmm16r4_cwt_zmm16r4
  
  
END MODULE mod_fpcompare
