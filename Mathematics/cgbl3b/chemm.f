      SUBROUTINE CHEMM ( SIDE, UPLO, M, N, ALPHA, A, LDA, B, LDB,
     $                   BETA, C, LDC )
*     .. Scalar Arguments ..
      CHARACTER*1        SIDE, UPLO
      INTEGER            M, N, LDA, LDB, LDC
      COMPLEX            ALPHA, BETA
*     .. Array Arguments ..
      COMPLEX            A( LDA, * ), B( LDB, * ), C( LDC, * )
*     ..
*
*  Purpose
*  =======
*
*  CHEMM  performs one of the matrix-matrix operations
*
*     C := alpha*A*B + beta*C,
*
*  or
*
*     C := alpha*B*A + beta*C,
*
*  where alpha and beta are scalars, A is an hermitian matrix and  B and
*  C are m by n matrices.
*
*  Parameters
*  ==========
*
*  SIDE   - CHARACTER*1.
*           On entry,  SIDE  specifies whether  the  hermitian matrix  A
*           appears on the  left or right  in the  operation as follows:
*
*              SIDE = 'L' or 'l'   C := alpha*A*B + beta*C,
*
*              SIDE = 'R' or 'r'   C := alpha*B*A + beta*C,
*
*           Unchanged on exit.
*
*  UPLO   - CHARACTER*1.
*           On  entry,   UPLO  specifies  whether  the  upper  or  lower
*           triangular  part  of  the  hermitian  matrix   A  is  to  be
*           referenced as follows:
*
*              UPLO = 'U' or 'u'   Only the upper triangular part of the
*                                  hermitian matrix is to be referenced.
*
*              UPLO = 'L' or 'l'   Only the lower triangular part of the
*                                  hermitian matrix is to be referenced.
*
*           Unchanged on exit.
*
*  M      - INTEGER.
*           On entry,  M  specifies the number of rows of the matrix  C.
*           M  must be at least zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry, N specifies the number of columns of the matrix C.
*           N  must be at least zero.
*           Unchanged on exit.
*
*  ALPHA  - COMPLEX      .
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  A      - COMPLEX       array of DIMENSION ( LDA, ka ), where ka is
*           m  when  SIDE = 'L' or 'l'  and is n  otherwise.
*           Before entry  with  SIDE = 'L' or 'l',  the  m by m  part of
*           the array  A  must contain the  hermitian matrix,  such that
*           when  UPLO = 'U' or 'u', the leading m by m upper triangular
*           part of the array  A  must contain the upper triangular part
*           of the  hermitian matrix and the  strictly  lower triangular
*           part of  A  is not referenced,  and when  UPLO = 'L' or 'l',
*           the leading  m by m  lower triangular part  of the  array  A
*           must  contain  the  lower triangular part  of the  hermitian
*           matrix and the  strictly upper triangular part of  A  is not
*           referenced.
*           Before entry  with  SIDE = 'R' or 'r',  the  n by n  part of
*           the array  A  must contain the  hermitian matrix,  such that
*           when  UPLO = 'U' or 'u', the leading n by n upper triangular
*           part of the array  A  must contain the upper triangular part
*           of the  hermitian matrix and the  strictly  lower triangular
*           part of  A  is not referenced,  and when  UPLO = 'L' or 'l',
*           the leading  n by n  lower triangular part  of the  array  A
*           must  contain  the  lower triangular part  of the  hermitian
*           matrix and the  strictly upper triangular part of  A  is not
*           referenced.
*           Note that the imaginary parts  of the diagonal elements need
*           not be set, they are assumed to be zero.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the  calling (sub) program. When  SIDE = 'L' or 'l'  then
*           LDA must be at least  max( 1, m ), otherwise  LDA must be at
*           least max( 1, n ).
*           Unchanged on exit.
*
*  B      - COMPLEX       array of DIMENSION ( LDB, n ).
*           Before entry, the leading  m by n part of the array  B  must
*           contain the matrix B.
*           Unchanged on exit.
*
*  LDB    - INTEGER.
*           On entry, LDB specifies the first dimension of B as declared
*           in  the  calling  (sub)  program.   LDB  must  be  at  least
*           max( 1, m ).
*           Unchanged on exit.
*
*  BETA   - COMPLEX      .
*           On entry,  BETA  specifies the scalar  beta.  When  BETA  is
*           supplied as zero then C need not be set on input.
*           Unchanged on exit.
*
*  C      - COMPLEX       array of DIMENSION ( LDC, n ).
*           Before entry, the leading  m by n  part of the array  C must
*           contain the matrix  C,  except when  beta  is zero, in which
*           case C need not be set on entry.
*           On exit, the array  C  is overwritten by the  m by n updated
*           matrix.
*
*  LDC    - INTEGER.
*           On entry, LDC specifies the first dimension of C as declared
*           in  the  calling  (sub)  program.   LDC  must  be  at  least
*           max( 1, m ).
*           Unchanged on exit.
*
*
*  Level 3 Blas routine.
*
*  -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*  -- Rewritten in May-1994.
*     GEMM-Based Level 3 BLAS.
*     Per Ling, Institute of Information Processing,
*     University of Umea, Sweden.
*
*
*     .. Local Scalars ..
      INTEGER            INFO, NROWA
      LOGICAL            LSIDE, UPPER
      INTEGER            I, II, IX, ISEC, J, JJ, JX, JSEC
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN, REAL, CMPLX, CONJG
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
      EXTERNAL           CGEMM, CCOPY
*     .. Parameters ..
      REAL               ZERO
      COMPLEX            CZERO, CONE
      PARAMETER        ( ZERO = 0.0E+0,
     $                   CZERO = ( 0.0E+0, 0.0E+0 ),
     $                   CONE = ( 1.0E+0, 0.0E+0 ) )
*     .. User specified parameters for CHEMM ..
      INTEGER            RCB, CB
      PARAMETER        ( RCB = 128, CB = 64 )
*     .. Local Arrays ..
      COMPLEX            T1( RCB, RCB )
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      LSIDE = LSAME( SIDE, 'L' )
      UPPER = LSAME( UPLO, 'U' )
      IF( LSIDE )THEN
         NROWA = M
      ELSE
         NROWA = N
      END IF
      INFO = 0
      IF( ( .NOT.LSIDE ).AND.( .NOT.LSAME( SIDE, 'R' ) ) )THEN
         INFO = 1
      ELSE IF( ( .NOT.UPPER ).AND.( .NOT.LSAME( UPLO, 'L' ) ) )THEN
         INFO = 2
      ELSE IF( M.LT.0 )THEN
         INFO = 3
      ELSE IF( N.LT.0 )THEN
         INFO = 4
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 7
      ELSE IF( LDB.LT.MAX( 1, M ) )THEN
         INFO = 9
      ELSE IF( LDC.LT.MAX( 1, M ) )THEN
         INFO = 12
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'CHEMM ', INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.
     $    ( ( ALPHA.EQ.CZERO ).AND.( BETA.EQ.CONE ) ) )
     $   RETURN
*
*     And when alpha.eq.zero.
*
      IF( ALPHA.EQ.CZERO )THEN
         CALL CGEMM ( 'N', 'N', M, N, 0, CZERO, A, MAX( LDA, LDB ),
     $                                B, MAX( LDA, LDB ), BETA, C, LDC )
         RETURN
      END IF
*
*     Start the operations.
*
      IF( LSIDE )THEN
         IF( UPPER )THEN
*
*           Form  C := alpha*A*B + beta*C. Left, Upper.
*
            DO 60, II = 1, M, RCB
               ISEC = MIN( RCB, M-II+1 )
*
*              T1 := A, a upper triangular diagonal block of A is copied
*              to the upper triangular part of T1.
*
               DO 10, I = II, II+ISEC-1
                  CALL CCOPY ( I-II+1, A( II, I ), 1, T1( 1, I-II+1 ),
     $                                                               1 )
   10          CONTINUE
*
*              Set the imaginary part of diagonal elements of T1
*              to zero.
*
               DO 20, I = 1, ISEC
                  T1( I, I ) = CMPLX( REAL( T1( I, I ) ), ZERO )
   20          CONTINUE
*
*              T1 :=  conjg( A' ), the conjugated transpose of a
*              strictly upper triangular diagonal block of A is copied
*              to the strictly lower triangular part of T1. Notice that
*              T1 is referenced by row and that the maximum length of a
*              vector referenced is CB.
*
               DO 50, JJ = II, II+ISEC-1, CB
                  JSEC = MIN( CB, II+ISEC-JJ )
                  DO 40, J = JJ+1, II+ISEC-1
                     DO 30, I = JJ, J-1
                        T1( J-II+1, I-II+1 ) = CONJG( A( I, J ) )
   30                CONTINUE
   40             CONTINUE
   50          CONTINUE
*
*              C := alpha*T1*B + beta*C, a horizontal block of C is
*              updated using the general matrix multiply, CGEMM. T1
*              corresponds to a full diagonal block of the matrix A.
*
               CALL CGEMM ( 'N', 'N', ISEC, N, ISEC, ALPHA, T1( 1, 1 ),
     $                     RCB, B( II, 1 ), LDB, BETA, C( II, 1 ), LDC )
*
*              C := alpha*conjg( A' )*B + C and C := alpha*A*B + C,
*              matrix multiply operations involving rectangular blocks
*              of A.
*
               IF( II.GT.1 )THEN
                  CALL CGEMM ( 'C', 'N', ISEC, N, II-1, ALPHA,
     $                                  A( 1, II ), LDA, B( 1, 1 ), LDB,
     $                                           CONE, C( II, 1 ), LDC )
               END IF
               IF( II+ISEC.LE.M )THEN
                  CALL CGEMM ( 'N', 'N', ISEC, N, M-II-ISEC+1, ALPHA,
     $                           A( II, II+ISEC ), LDA, B( II+ISEC, 1 ),
     $                                      LDB, CONE, C( II, 1 ), LDC )
               END IF
   60       CONTINUE
         ELSE
*
*           Form  C := alpha*A*B + beta*C. Left, Lower.
*
            DO 120, IX = M, 1, -RCB
               II = MAX( 1, IX-RCB+1 )
               ISEC = IX-II+1
*
*              T1 := A, a lower triangular diagonal block of A is copied
*              to the lower triangular part of T1.
*
               DO 70, I = II, II+ISEC-1
                  CALL CCOPY ( II+ISEC-I, A( I, I ), 1,
     $                                         T1( I-II+1, I-II+1 ), 1 )
   70          CONTINUE
*
*              Set the imaginary part of diagonal elements of T1
*              to zero.
*
               DO 80, I = 1, ISEC
                  T1( I, I ) = CMPLX( REAL( T1( I, I ) ), ZERO )
   80          CONTINUE
*
*              T1 :=  conjg( A' ), the conjugated transpose of a
*              strictly lower triangular diagonal block of A is copied
*              to the strictly upper triangular part of T1. Notice that
*              T1 is referenced by row and that the maximum length of a
*              vector referenced is CB.
*
               DO 110, JX = II+ISEC-1, II, -CB
                  JJ = MAX( II, JX-CB+1 )
                  JSEC = JX-JJ+1
                  DO 100, J = II, JJ+JSEC-2
                     DO 90, I = J+1, II+ISEC-1
                        T1( J-II+1, I-II+1 ) = CONJG( A( I, J ) )
   90                CONTINUE
  100             CONTINUE
  110          CONTINUE
*
*              C := alpha*T1*B + beta*C, a horizontal block of C is
*              updated using the general matrix multiply, CGEMM. T1
*              corresponds to a full diagonal block of the matrix A.
*
               CALL CGEMM ( 'N', 'N', ISEC, N, ISEC, ALPHA, T1( 1, 1 ),
     $                     RCB, B( II, 1 ), LDB, BETA, C( II, 1 ), LDC )
*
*              C := alpha*conjg( A' )*B + C and C := alpha*A*B + C,
*              matrix multiply operations involving rectangular blocks
*              of A.
*
               IF( II+ISEC.LE.M )THEN
                  CALL CGEMM ( 'C', 'N', ISEC, N, M-II-ISEC+1, ALPHA,
     $                           A( II+ISEC, II ), LDA, B( II+ISEC, 1 ),
     $                                      LDB, CONE, C( II, 1 ), LDC )
               END IF
               IF( II.GT.1 )THEN
                  CALL CGEMM ( 'N', 'N', ISEC, N, II-1, ALPHA,
     $                                  A( II, 1 ), LDA, B( 1, 1 ), LDB,
     $                                           CONE, C( II, 1 ), LDC )
               END IF
  120       CONTINUE
         END IF
      ELSE
         IF( UPPER )THEN
*
*           Form  C := alpha*B*A + beta*C. Right, Upper.
*
            DO 180, JJ = 1, N, RCB
               JSEC = MIN( RCB, N-JJ+1 )
*
*              T1 := A, a upper triangular diagonal block of A is copied
*              to the upper triangular part of T1.
*
               DO 130, J = JJ, JJ+JSEC-1
                  CALL CCOPY ( J-JJ+1, A( JJ, J ), 1, T1( 1, J-JJ+1 ),
     $                                                               1 )
  130          CONTINUE
*
*              Set the imaginary part of diagonal elements of T1
*              to zero.
*
               DO 140, J = 1, JSEC
                  T1( J, J ) = CMPLX( REAL( T1( J, J ) ), ZERO )
  140          CONTINUE
*
*              T1 :=  conjg( A' ), the conjugated transpose of a
*              strictly upper triangular diagonal block of A is copied
*              to the strictly lower triangular part of T1. Notice that
*              T1 is referenced by row and that the maximum length of a
*              vector referenced is CB.
*
               DO 170, II = JJ, JJ+JSEC-1, CB
                  ISEC = MIN( CB, JJ+JSEC-II )
                  DO 160, I = II+1, JJ+JSEC-1
                     DO 150, J = II, I-1
                        T1( I-JJ+1, J-JJ+1 ) = CONJG( A( J, I ) )
  150                CONTINUE
  160             CONTINUE
  170          CONTINUE
*
*              C := alpha*B*T1 + beta*C, a vertical block of C is
*              updated using the general matrix multiply, CGEMM. T1
*              corresponds to a full diagonal block of the matrix A.
*
               CALL CGEMM ( 'N', 'N', M, JSEC, JSEC, ALPHA, B( 1, JJ ),
     $                     LDB, T1( 1, 1 ), RCB, BETA, C( 1, JJ ), LDC )
*
*              C := alpha*B*A + C and C := alpha*B*conjg( A' ) + C,
*              matrix multiply operations involving rectangular blocks
*              of A.
*
               IF( JJ.GT.1 )THEN
                  CALL CGEMM ( 'N', 'N', M, JSEC, JJ-1, ALPHA,
     $                                  B( 1, 1 ), LDB, A( 1, JJ ), LDA,
     $                                           CONE, C( 1, JJ ), LDC )
               END IF
               IF( JJ+JSEC.LE.N )THEN
                  CALL CGEMM ( 'N', 'C', M, JSEC, N-JJ-JSEC+1, ALPHA,
     $                           B( 1, JJ+JSEC ), LDB, A( JJ, JJ+JSEC ),
     $                                      LDA, CONE, C( 1, JJ ), LDC )
               END IF
  180       CONTINUE
         ELSE
*
*           Form  C := alpha*B*A + beta*C. Right, Lower.
*
            DO 240, JX = N, 1, -RCB
               JJ = MAX( 1, JX-RCB+1 )
               JSEC = JX-JJ+1
*
*              T1 := A, a lower triangular diagonal block of A is copied
*              to the lower triangular part of T1.
*
               DO 190, J = JJ, JJ+JSEC-1
                  CALL CCOPY ( JJ+JSEC-J, A( J, J ), 1,
     $                                         T1( J-JJ+1, J-JJ+1 ), 1 )
  190          CONTINUE
*
*              Set the imaginary part of diagonal elements of T1
*              to zero.
*
               DO 200, J = 1, JSEC
                  T1( J, J ) = CMPLX( REAL( T1( J, J ) ), ZERO )
  200          CONTINUE
*
*              T1 :=  conjg( A' ), the conjugated transpose of a
*              strictly lower triangular diagonal block of A is copied
*              to the strictly upper triangular part of T1. Notice that
*              T1 is referenced by row and that the maximum length of a
*              vector referenced is CB.
*
               DO 230, IX = JJ+JSEC-1, JJ, -CB
                  II = MAX( JJ, IX-CB+1 )
                  ISEC = IX-II+1
                  DO 220, I = JJ, II+ISEC-2
                     DO 210, J = I+1, JJ+JSEC-1
                        T1( I-JJ+1, J-JJ+1 ) = CONJG( A( J, I ) )
  210                CONTINUE
  220             CONTINUE
  230          CONTINUE
*
*              C := alpha*B*T1 + beta*C, a vertical block of C is
*              updated using the general matrix multiply, CGEMM. T1
*              corresponds to a full diagonal block of the matrix A.
*
               CALL CGEMM ( 'N', 'N', M, JSEC, JSEC, ALPHA,
     $                                 B( 1, JJ ), LDB, T1( 1, 1 ), RCB,
     $                                           BETA, C( 1, JJ ), LDC )
*
*              C := alpha*B*A + C and C := alpha*B*conjg( A' ) + C,
*              matrix multiply operations involving rectangular blocks
*              of A.
*
               IF( JJ+JSEC.LE.N )THEN
                  CALL CGEMM ( 'N', 'N', M, JSEC, N-JJ-JSEC+1, ALPHA,
     $                           B( 1, JJ+JSEC ), LDB, A( JJ+JSEC, JJ ),
     $                                      LDA, CONE, C( 1, JJ ), LDC )
               END IF
               IF( JJ.GT.1 )THEN
                  CALL CGEMM ( 'N', 'C', M, JSEC, JJ-1, ALPHA,
     $                                  B( 1, 1 ), LDB, A( JJ, 1 ), LDA,
     $                                           CONE, C( 1, JJ ), LDC )
               END IF
  240       CONTINUE
         END IF
      END IF
*
      RETURN
*
*     End of CHEMM.
*
      END
