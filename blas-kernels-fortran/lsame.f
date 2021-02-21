
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      LOGICAL          FUNCTION LSAME( CA, CB ) !GCC$ ATTRIBUTES inline :: LSAME
#elif defined __ICC || defined __INTEL_COMPILER
      LOGICAL          FUNCTION LSAME( CA, CB)
      !DIR$ ATTRIBUTES INLINE :: LSAME
#endif
*
*  -- LAPACK auxiliary routine (preliminary version) --
*     Univ. of Tennessee, Oak Ridge National Lab, Argonne National Lab,
*     Courant Institute, NAG Ltd., and Rice University
*     March 26, 1990
*
*     .. Scalar Arguments ..
      CHARACTER          CA, CB
*     ..
*
*  Purpose
*  =======
*
*  LSAME returns .TRUE. if CA is the same letter as CB regardless of
*  case.
*
*  This version of the routine is only correct for ASCII code.
*  Installers must modify the routine for other character-codes.
*
*  For EBCDIC systems the constant IOFF must be changed to -64.
*  For CDC systems using 6-12 bit representations, the system-
*  specific code in comments must be activated.
*
*  Arguments
*  =========
*
*  CA      (input) CHARACTER*1
*  CB      (input) CHARACTER*1
*          CA and CB specify the single characters to be compared.
*
*
*     .. Parameters ..
      INTEGER            IOFF
      PARAMETER        ( IOFF = 32 )
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ICHAR
*     ..
*     .. Executable Statements ..
*
*     Test if the characters are equal
*
      LSAME = CA.EQ.CB
*
*     Now test for equivalence
*
      IF( .NOT.LSAME ) THEN
         LSAME = ICHAR( CA ) - IOFF.EQ.ICHAR( CB )
      END IF
      IF( .NOT.LSAME ) THEN
         LSAME = ICHAR( CA ).EQ.ICHAR( CB ) - IOFF
      END IF
*
      RETURN
*
*  The following comments contain code for CDC systems using 6-12 bit
*  representations.
*
*     .. Parameters ..
*     INTEGER            ICIRFX
*     PARAMETER        ( ICIRFX=62 )
*     .. Scalar arguments ..
*     CHARACTER*1        CB
*     .. Array arguments ..
*     CHARACTER*1        CA(*)
*     .. Local scalars ..
*     INTEGER            IVAL
*     .. Intrinsic functions ..
*     INTRINSIC          ICHAR, CHAR
*     .. Executable statements ..
*
*     See if the first character in string CA equals string CB.
*
*     LSAME = CA(1) .EQ. CB .AND. CA(1) .NE. CHAR(ICIRFX)
*
*     IF (LSAME) RETURN
*
*     The characters are not identical. Now check them for equivalence.
*     Look for the 'escape' character, circumflex, followed by the
*     letter.
*
*     IVAL = ICHAR(CA(2))
*     IF (IVAL.GE.ICHAR('A') .AND. IVAL.LE.ICHAR('Z')) THEN
*        LSAME = CA(1) .EQ. CHAR(ICIRFX) .AND. CA(2) .EQ. CB
*     END IF
*
*     RETURN
*
*     End of LSAME
*
      END
