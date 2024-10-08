      INTEGER FUNCTION EOLN( LN, LLN )
*     .. Scalar Arguments ..
      INTEGER            LLN
*     .. Array Arguments ..
      CHARACTER          LN( * )
*
*  Return the index of the last non-blank character in the last word
*  (token) of LN.
*
*
*  -- Written in December-1993.
*     GEMM-Based Level 3 BLAS.
*     Per Ling, Institute of Information Processing,
*     University of Umea, Sweden.
*
*
*     .. Local Scalars ..
      INTEGER            IE
*     ..
*     .. Executable Statements ..
*
*     Find the the end of the last word (token) of LN.
*
      IE = LLN
   10 IF( ( LN( IE ).EQ.' ' ).AND.( IE.GE.1 ) )THEN
         IE = IE - 1
         GO TO 10
      END IF
      EOLN = IE
*
      RETURN
*
*     End of EOLN.
*
      END
