      SUBROUTINE CHER2K( UPLO, TRANS, N, K, ALPHA, A, LDA, B, LDB,
     $                   BETA, C, LDC )
*     .. Scalar Arguments ..
      CHARACTER*1        UPLO, TRANS
      INTEGER            N, K, LDA, LDB, LDC
      REAL               BETA
      COMPLEX            ALPHA
*     .. Array Arguments ..
      COMPLEX            A( LDA, * ), B( LDB, * ), C( LDC, * )
*     ..
*
*  Purpose
*  =======
*
*  CHER2K  performs one of the hermitian rank 2k operations
*
*     C := alpha*A*conjg( B' ) + conjg( alpha )*B*conjg( A' ) + beta*C,
*
*  or
*
*     C := alpha*conjg( A' )*B + conjg( alpha )*conjg( B' )*A + beta*C,
*
*  where  alpha and beta  are scalars with  beta  real,  C is an  n by n
*  hermitian matrix and  A and B  are  n by k matrices in the first case
*  and  k by n  matrices in the second case.
*
*  Parameters
*  ==========
*
*  UPLO   - CHARACTER*1.
*           On  entry,   UPLO  specifies  whether  the  upper  or  lower
*           triangular  part  of the  array  C  is to be  referenced  as
*           follows:
*
*              UPLO = 'U' or 'u'   Only the  upper triangular part of  C
*                                  is to be referenced.
*
*              UPLO = 'L' or 'l'   Only the  lower triangular part of  C
*                                  is to be referenced.
*
*           Unchanged on exit.
*
*  TRANS  - CHARACTER*1.
*           On entry,  TRANS  specifies the operation to be performed as
*           follows:
*
*              TRANS = 'N' or 'n'    C := alpha*A*conjg( B' )          +
*                                         conjg( alpha )*B*conjg( A' ) +
*                                         beta*C.
*
*              TRANS = 'C' or 'c'    C := alpha*conjg( A' )*B          +
*                                         conjg( alpha )*conjg( B' )*A +
*                                         beta*C.
*
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry,  N specifies the order of the matrix C.  N must be
*           at least zero.
*           Unchanged on exit.
*
*  K      - INTEGER.
*           On entry with  TRANS = 'N' or 'n',  K  specifies  the number
*           of  columns  of the  matrices  A and B,  and on  entry  with
*           TRANS = 'C' or 'c',  K  specifies  the number of rows of the
*           matrices  A and B.  K must be at least zero.
*           Unchanged on exit.
*
*  ALPHA  - COMPLEX      .
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  A      - COMPLEX       array of DIMENSION ( LDA, ka ), where ka is
*           k  when  TRANS = 'N' or 'n',  and is  n  otherwise.
*           Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
*           part of the array  A  must contain the matrix  A,  otherwise
*           the leading  k by n  part of the array  A  must contain  the
*           matrix A.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
*           then  LDA must be at least  max( 1, n ), otherwise  LDA must
*           be at least  max( 1, k ).
*           Unchanged on exit.
*
*  B      - COMPLEX       array of DIMENSION ( LDB, kb ), where kb is
*           k  when  TRANS = 'N' or 'n',  and is  n  otherwise.
*           Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
*           part of the array  B  must contain the matrix  B,  otherwise
*           the leading  k by n  part of the array  B  must contain  the
*           matrix B.
*           Unchanged on exit.
*
*  LDB    - INTEGER.
*           On entry, LDB specifies the first dimension of B as declared
*           in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
*           then  LDB must be at least  max( 1, n ), otherwise  LDB must
*           be at least  max( 1, k ).
*           Unchanged on exit.
*
*  BETA   - REAL.
*           On entry, BETA specifies the scalar beta.
*           Unchanged on exit.
*
*  C      - COMPLEX       array of DIMENSION ( LDC, n ).
*           Before entry  with  UPLO = 'U' or 'u',  the leading  n by n
*           upper triangular part of the array C must contain the upper
*           triangular part  of the  hermitian matrix  and the strictly
*           lower triangular part of C is not referenced.  On exit, the
*           upper triangular part of the array  C is overwritten by the
*           upper triangular part of the updated matrix.
*           Before entry  with  UPLO = 'L' or 'l',  the leading  n by n
*           lower triangular part of the array C must contain the lower
*           triangular part  of the  hermitian matrix  and the strictly
*           upper triangular part of C is not referenced.  On exit, the
*           lower triangular part of the array  C is overwritten by the
*           lower triangular part of the updated matrix.
*           Note that the imaginary parts of the diagonal elements need
*           not be set,  they are assumed to be zero,  and on exit they
*           are set to zero.
*
*  LDC    - INTEGER.
*           On entry, LDC specifies the first dimension of C as declared
*           in  the  calling  (sub)  program.   LDC  must  be  at  least
*           max( 1, n ).
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
      INTEGER            I, II, IX, ISEC, J, JJ, JX, JSEC
      LOGICAL            UPPER, NOTR
      COMPLEX            CBETA
*     .. Intrinsic Functions ..
      INTRINSIC          MIN, MAX, REAL, CMPLX, CONJG
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
      EXTERNAL           CGEMM, CAXPY, CSCAL
*     .. Parameters ..
      REAL               ONE, ZERO
      COMPLEX            CONE, CZERO
      PARAMETER        ( ONE = 1.0E+0, ZERO = 0.0E+0,
     $                   CONE = ( 1.0E+0, 0.0E+0 ),
     $                   CZERO = ( 0.0E+0, 0.0E+0 ) )
*     .. User specified parameters for CHER2K ..
      INTEGER            RCB, CB
      PARAMETER        ( RCB = 128, CB = 64 )
*     .. Local Arrays ..
      COMPLEX            T1( RCB, RCB )
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      UPPER = LSAME( UPLO, 'U' )
      NOTR = LSAME( TRANS, 'N' )
      IF( NOTR )THEN
         NROWA = N
      ELSE
         NROWA = K
      END IF
      INFO = 0
      IF( ( .NOT.UPPER ).AND.( .NOT.LSAME( UPLO , 'L' ) ) )THEN
         INFO = 1
      ELSE IF( ( .NOT.NOTR ).AND.( .NOT.LSAME( TRANS, 'C' ) ) )THEN
         INFO = 2
      ELSE IF( N.LT.0 )THEN
         INFO = 3
      ELSE IF( K.LT.0 )THEN
         INFO = 4
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 7
      ELSE IF( LDB.LT.MAX( 1, NROWA ) )THEN
         INFO = 9
      ELSE IF( LDC.LT.MAX( 1, N ) )THEN
         INFO = 12
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'CHER2K', INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( ( N.EQ.0 ).OR.
     $    ( ( ( ALPHA.EQ.CZERO ).OR.( K.EQ.0 ) ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
*
      CBETA = CMPLX( BETA, ZERO )
*
*     And when alpha.eq.zero or k.eq.0.
*
      IF( ( ALPHA.EQ.CZERO ).OR.( K.EQ.0 ) )THEN
         IF( UPPER )THEN
            C( 1, 1 ) = CMPLX( BETA*REAL( C( 1, 1 ) ), ZERO )
            DO 10, I = 2, N
               CALL CSCAL ( I-1, CBETA, C( 1, I ), 1 )
               C( I, I ) = CMPLX( BETA*REAL( C( I, I ) ), ZERO )
   10       CONTINUE
         ELSE
            DO 20, I = 1, N-1
               C( I, I ) = CMPLX( BETA*REAL( C( I, I ) ), ZERO )
               CALL CSCAL ( N-I, CBETA, C( I+1, I ), 1 )
   20       CONTINUE
            C( N, N ) = CMPLX( BETA*REAL( C( N, N ) ), ZERO )
         END IF
         RETURN
      END IF
*
*     Start the operations.
*
      IF( UPPER )THEN
         IF( NOTR )THEN
*
*           Form  C := alpha*A*conjg( B' ) +
*           conjg( alpha )*B*conjg( A' ) + beta*C. Upper, Notr.
*
            DO 90, II = 1, N, RCB
               ISEC = MIN( RCB, N-II+1 )
*
*              T1 := alpha*A*conjg( B' ), matrix multiply on rectangular
*              blocks of A and B. T1 is a square block.
*
               CALL CGEMM ( 'N', 'C', ISEC, ISEC, K, ALPHA, A( II, 1 ),
     $                    LDA, B( II, 1 ), LDB, CZERO, T1( 1, 1 ), RCB )
*
*              C :=  beta*C, a upper triangular diagonal block of C is
*              updated with beta.
*
               IF( BETA.NE.ONE )THEN
                  DO 30, I = II, II+ISEC-1
                     CALL CSCAL ( I-II+1, CBETA, C( II, I ), 1 )
   30             CONTINUE
               END IF
*
*              C := T1 + C, the upper triangular part of T1 is added to
*              the upper triangular diagonal block of C.
*
               DO 40, I = II, II+ISEC-1
                  CALL CAXPY ( I-II+1, CONE, T1( 1, I-II+1 ), 1,
     $                                                   C( II, I ), 1 )
   40          CONTINUE
*
*              C := conjg( T1' ) + C, the conjugated transpose of the
*              lower triangular part of T1 is added to the upper
*              triangular diagonal block of C. Notice that T1 is
*              referenced by row and that the maximum length of a vector
*              referenced by CAXPY is CB.
*
               DO 70, JJ = II, II+ISEC-1, CB
                  JSEC = MIN( CB, II+ISEC-JJ )
                  DO 60, I = JJ, II+ISEC-1
                     DO 50, J = JJ, MIN( JJ+JSEC-1, I )
                        C( J, I ) = C( J, I ) +
     $                                    CONJG( T1( I-II+1, J-II+1 ) )
   50                CONTINUE
   60             CONTINUE
   70          CONTINUE
*
*              Set the imaginary part of diagonal elements of C
*              to zero.
*
               DO 80, I = II, II+ISEC-1
                  C( I, I ) = CMPLX( REAL( C( I, I ) ), ZERO )
   80          CONTINUE
*
*              C := alpha*A*conjg( B' ) + beta*C  and
*              C := conjg( alpha )*B*conjg( A' ) + C, matrix multiply
*              updating upper vertical blocks of C.
*
               IF( II.GT.1 )THEN
                  CALL CGEMM ( 'N', 'C', II-1, ISEC, K, ALPHA,
     $                           A( 1, 1 ), LDA, B( II, 1 ), LDB, CBETA,
     $                                                 C( 1, II ), LDC )
                  CALL CGEMM ( 'N', 'C', II-1, ISEC, K, CONJG( ALPHA ),
     $                            B( 1, 1 ), LDB, A( II, 1 ), LDA, CONE,
     $                                                 C( 1, II ), LDC )
               END IF
   90       CONTINUE
         ELSE
*
*           Form  C := alpha*conjg( A' )*B +
*           conjg( alpha )*conjg( B' )*A + beta*C. Upper, Trans.
*
            DO 160, II = 1, N, RCB
               ISEC = MIN( RCB, N-II+1 )
*
*              T1 := alpha*conjg( A' )*B, matrix multiply on
*              rectangular blocks of A and B. T1 is a square block.
*
               CALL CGEMM ( 'C', 'N', ISEC, ISEC, K, ALPHA, A( 1, II ),
     $                    LDA, B( 1, II ), LDB, CZERO, T1( 1, 1 ), RCB )
*
*              C :=  beta*C, a upper triangular diagonal block of C is
*              updated with beta.
*
               IF( BETA.NE.ONE )THEN
                  DO 100, I = II, II+ISEC-1
                     CALL CSCAL ( I-II+1, CBETA, C( II, I ), 1 )
  100             CONTINUE
               END IF
*
*              C := T1 + C, the upper triangular part of T1 is added to
*              the upper triangular diagonal block of C.
*
               DO 110, I = II, II+ISEC-1
                  CALL CAXPY ( I-II+1, CONE, T1( 1, I-II+1 ), 1,
     $                                                   C( II, I ), 1 )
  110          CONTINUE
*
*              C := conjg( T1' ) + C, the conjugated transpose of the
*              lower triangular part of T1 is added to the upper
*              triangular diagonal block of C. Notice that T1 is
*              referenced by row and that the maximum length of a vector
*              referenced by CAXPY is CB.
*
               DO 140, JJ = II, II+ISEC-1, CB
                  JSEC = MIN( CB, II+ISEC-JJ )
                  DO 130, I = JJ, II+ISEC-1
                     DO 120, J = JJ, MIN( JJ+JSEC-1, I )
                        C( J, I ) = C( J, I ) +
     $                                    CONJG( T1( I-II+1, J-II+1 ) )
  120                CONTINUE
  130             CONTINUE
  140          CONTINUE
*
*              Set the imaginary part of diagonal elements of C
*              to zero.
*
               DO 150, I = II, II+ISEC-1
                  C( I, I ) = CMPLX( REAL( C( I, I ) ), ZERO )
  150          CONTINUE
*
*              C := alpha*conjg( A' )*B + beta*C  and
*              C := alpha*conjg( B' )*A + C, matrix multiply on upper
*              vertical blocks of C.
*
               IF( II.GT.1 )THEN
                  CALL CGEMM ( 'C', 'N', II-1, ISEC, K, ALPHA,
     $                           A( 1, 1 ), LDA, B( 1, II ), LDB, CBETA,
     $                                                 C( 1, II ), LDC )
                  CALL CGEMM ( 'C', 'N', II-1, ISEC, K, CONJG( ALPHA ),
     $                            B( 1, 1 ), LDB, A( 1, II ), LDA, CONE,
     $                                                 C( 1, II ), LDC )
               END IF
  160       CONTINUE
         END IF
      ELSE
         IF( NOTR )THEN
*
*           Form  C := alpha*A*conjg( B' ) +
*           alpha*B*conjg( A' ) + beta*C. Lower, Notr.
*
            DO 230, IX = N, 1, -RCB
               II = MAX( 1, IX-RCB+1 )
               ISEC = IX-II+1
*
*              T1 := alpha*A*conjg( B' ), matrix multiply on rectangular
*              blocks of A and B. T1 is a square block.
*
               CALL CGEMM ( 'N', 'C', ISEC, ISEC, K, ALPHA, A( II, 1 ),
     $                    LDA, B( II, 1 ), LDB, CZERO, T1( 1, 1 ), RCB )
*
*              C :=  beta*C, a lower triangular diagonal block of C is
*              updated with beta.
*
               IF( BETA.NE.ONE )THEN
                  DO 170, I = II, II+ISEC-1
                     CALL CSCAL ( II+ISEC-I, CBETA, C( I, I ), 1 )
  170             CONTINUE
               END IF
*
*              C := T1 + C, the lower triangular part of T1 is added to
*              the lower triangular diagonal block of C.
*
               DO 180, I = II, II+ISEC-1
                  CALL CAXPY ( II+ISEC-I, CONE, T1( I-II+1, I-II+1 ), 1,
     $                                                    C( I, I ), 1 )
  180          CONTINUE
*
*              C := conjg( T1' ) + C, the conjugated transpose of the
*              upper triangular part of T1 is added to the lower
*              triangular diagonal block of C. Notice that T1 is
*              referenced by row and that the maximum length of a vector
*              referenced by CAXPY is CB.
*
               DO 210, JX = II+ISEC-1, II, -CB
                  JJ = MAX( II, JX-CB+1 )
                  JSEC = JX-JJ+1
                  DO 200, I = II, JJ+JSEC-1
                     DO 190, J = MAX( JJ, I), JJ+JSEC-1
                        C( J, I ) = C( J, I ) +
     $                                    CONJG( T1( I-II+1, J-II+1 ) )
  190                CONTINUE
  200             CONTINUE
  210          CONTINUE
*
*              Set the imaginary part of diagonal elements of C
*              to zero.
*
               DO 220, I = II, II+ISEC-1
                  C( I, I ) = CMPLX( REAL( C( I, I ) ), ZERO )
  220          CONTINUE
*
*              C := alpha*A*conjg( B' ) + beta*C  and
*              C := alpha*B*conjg( A' ) + C, matrix multiply on lower
*              vertical blocks of C.
*
               IF( II+ISEC.LE.N )THEN
                  CALL CGEMM ( 'N', 'C', N-II-ISEC+1, ISEC, K, ALPHA,
     $                            A( II+ISEC, 1 ), LDA, B( II, 1 ), LDB,
     $                                    CBETA, C( II+ISEC, II ), LDC )
                  CALL CGEMM ( 'N', 'C', N-II-ISEC+1, ISEC, K,
     $                CONJG( ALPHA ), B( II+ISEC, 1 ), LDB, A( II, 1 ),
     $                                LDA, CONE, C( II+ISEC, II ), LDC )
               END IF
  230       CONTINUE
         ELSE
*
*           Form  C := alpha*conjg( A' )*B +
*           alpha*conjg( B' )*A + beta*C. Lower, Trans.
*
            DO 300, IX = N, 1, -RCB
               II = MAX( 1, IX-RCB+1 )
               ISEC = IX-II+1
*
*              T1 := alpha*A*conjg( B' ), matrix multiply on rectangular
*              blocks of A and B. T1 is a square block.
*
               CALL CGEMM ( 'C', 'N', ISEC, ISEC, K, ALPHA, A( 1, II ),
     $                    LDA, B( 1, II ), LDB, CZERO, T1( 1, 1 ), RCB )
*
*              C :=  beta*C, a lower triangular diagonal block of C is
*              updated with beta.
*
               IF( BETA.NE.ONE )THEN
                  DO 240, I = II, II+ISEC-1
                     CALL CSCAL ( II+ISEC-I, CBETA, C( I, I ), 1 )
  240             CONTINUE
               END IF
*
*              C := T1 + C, the lower triangular part of T1 is added to
*              the lower triangular diagonal block of C.
*
               DO 250, I = II, II+ISEC-1
                  CALL CAXPY ( II+ISEC-I, CONE, T1( I-II+1, I-II+1 ), 1,
     $                                                    C( I, I ), 1 )
  250          CONTINUE
*
*              C := conjg( T1' ) + C, the conjugated transpose of the
*              upper triangular part of T1 is added to the lower
*              triangular diagonal block of C. Notice that T1 is
*              referenced by row and that the maximum length of a vector
*              referenced by CAXPY is CB.
*
               DO 280, JX = II+ISEC-1, II, -CB
                  JJ = MAX( II, JX-CB+1 )
                  JSEC = JX-JJ+1
                  DO 270, I = II, JJ+JSEC-1
                     DO 260, J = MAX( JJ, I), JJ+JSEC-1
                        C( J, I ) = C( J, I ) +
     $                                    CONJG( T1( I-II+1, J-II+1 ) )
  260                CONTINUE
  270             CONTINUE
  280          CONTINUE
*
*              Set the imaginary part of diagonal elements of C
*              to zero.
*
               DO 290, I = II, II+ISEC-1
                  C( I, I ) = CMPLX( REAL( C( I, I ) ), ZERO )
  290          CONTINUE
*
*              C := alpha*conjg( A' )*B + beta*C  and
*              C := alpha*conjg( B' )*A + C, matrix multiply on lower
*              vertical blocks of C.
*
               IF( II+ISEC.LE.N )THEN
                  CALL CGEMM ( 'C', 'N', N-II-ISEC+1, ISEC, K, ALPHA,
     $                            A( 1, II+ISEC ), LDA, B( 1, II ), LDB,
     $                                    CBETA, C( II+ISEC, II ), LDC )
                  CALL CGEMM ( 'C', 'N', N-II-ISEC+1, ISEC, K,
     $                CONJG( ALPHA ), B( 1, II+ISEC ), LDB, A( 1, II ),
     $                                LDA, CONE, C( II+ISEC, II ), LDC )
               END IF
  300       CONTINUE
         END IF
      END IF
*
      RETURN
*
*     End of CHER2K.
*
      END
