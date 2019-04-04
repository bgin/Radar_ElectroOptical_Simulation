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
  USE mod_kinds, ONLY: int4, sp, dp
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
  !CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  !  '$Id: Compare_Float_Numbers.f90 60152 2015-08-13 19:19:13Z paul.vandelst@noaa.gov $'
  character(*), parameter :: module_version_id = &
       '$Id: GMS_mod_fpcompare.f90 -00200 2019-04-03 20:28 Bernard Gingold, contact: beniekg@gmail.com '
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
  INTEGER(kind=int4), PARAMETER :: DEFAULT_N_SIGFIG = 6


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
    INTEGER(kind=int4)     , INTENT(IN) :: n
    REAL(kind=sp) :: Tolerance
    INTEGER(kind=int4) :: e
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
    INTEGER(kind=int4),      INTENT(IN) :: n
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
    INTEGER(kind=int4),         INTENT(IN) :: n
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
    INTEGER(kind=int4),         INTENT(IN) :: n
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
    INTEGER(kind=int4),                INTENT(IN) :: n
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
    INTEGER(kind=int4),                INTENT(IN) :: n
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
    INTEGER(kind=int4),                   INTENT(IN) :: n
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
    INTEGER(kind=int4),                   INTENT(IN) :: n
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

END MODULE mod_fpcompare
