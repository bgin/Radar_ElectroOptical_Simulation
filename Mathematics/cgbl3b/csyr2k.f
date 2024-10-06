      SUBROUTINE CSYR2K( UPLO, TRANS, N, K, ALPHA, A, LDA, B, LDB,
     $                   BETA, C, LDC )
*     .. Scalar Arguments ..
      CHARACTER*1        UPLO, TRANS
      INTEGER            N, K, LDA, LDB, LDC
      COMPLEX            ALPHA, BETA
*     .. Array Arguments ..
      COMPLEX            A( LDA, * ), B( LDB, * ), C( LDC, * )
*     ..
*
*  Purpose
*  =======
*
*  CSYR2K  performs one of the symmetric rank 2k operations
*
*     C := alpha*A*B' + alpha*B*A' + beta*C,
*
*  or
*
*     C := alpha*A'*B + alpha*B'*A + beta*C,
*
*  where  alpha and beta  are scalars,  C is an  n by n symmetric matrix
*  and  A and B  are  n by k  matrices  in the  first  case  and  k by n
*  matrices in the second case.
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
*              TRANS = 'N' or 'n'    C := alpha*A*B' + alpha*B*A' +
*                                         beta*C.
*
*              TRANS = 'T' or 't'    C := alpha*A'*B + alpha*B'*A +
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
*           TRANS = 'T' or 't',  K  specifies  the number of rows of the
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
*  BETA   - COMPLEX      .
*           On entry, BETA specifies the scalar beta.
*           Unchanged on exit.
*
*  C      - COMPLEX       array of DIMENSION ( LDC, n ).
*           Before entry  with  UPLO = 'U' or 'u',  the leading  n by n
*           upper triangular part of the array C must contain the upper
*           triangular part  of the  symmetric matrix  and the strictly
*           lower triangular part of C is not referenced.  On exit, the
*           upper triangular part of the array  C is overwritten by the
*           upper triangular part of the updated matrix.
*           Before entry  with  UPLO = 'L' or 'l',  the leading  n by n
*           lower triangular part of the array C must contain the lower
*           triangular part  of the  symmetric matrix  and the strictly
*           upper triangular part of C is not referenced.  On exit, the
*           lower triangular part of the array  C is overwritten by the
*           lower triangular part of the updated matrix.
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
      INTEGER            I, II, IX, ISEC, JJ, JX, JSEC
      LOGICAL            UPPER, NOTR
*     .. Intrinsic Functions ..
      INTRINSIC          MIN, MAX
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
      EXTERNAL           CGEMM, CAXPY, CSCAL
*     .. Parameters ..
      COMPLEX            ONE, ZERO
      PARAMETER        ( ONE = ( 1.0E+0, 0.0E+0 ),
     $                   ZERO = ( 0.0E+0, 0.0E+0 ) )
*     .. User specified parameters for CSYR2K ..
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
      ELSE IF( ( .NOT.NOTR ).AND. ( .NOT.LSAME( TRANS, 'T' ) ) )THEN
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
         CALL XERBLA( 'CSYR2K', INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( ( N.EQ.0 ).OR.
     $    ( ( ( ALPHA.EQ.ZERO ).OR.( K.EQ.0 ) ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
*
*     And when alpha.eq.zero or k.eq.0.
*
      IF( ( ALPHA.EQ.ZERO ).OR.( K.EQ.0 ) )THEN
         IF( UPPER )THEN
            DO 10, I = 1, N
               CALL CSCAL ( I, BETA, C( 1, I ), 1 )
   10       CONTINUE
         ELSE
            DO 20, I = 1, N
               CALL CSCAL ( N-I+1, BETA, C( I, I ), 1 )
   20       CONTINUE
         END IF
         RETURN
      END IF
*
*     Start the operations.
*
      IF( UPPER )THEN
         IF( NOTR )THEN
*
*           Form  C := alpha*A*B' + alpha*B*A' + beta*C. Upper, Notr.
*
            DO 70, II = 1, N, RCB
               ISEC = MIN( RCB, N-II+1 )
*
*              T1 := alpha*A*B', general matrix multiply on rectangular
*              blocks of A and B. T1 is square.
*
               CALL CGEMM ( 'N', 'T', ISEC, ISEC, K, ALPHA, A( II, 1 ),
     $                     LDA, B( II, 1 ), LDB, ZERO, T1( 1, 1 ), RCB )
*
*              C :=  beta*C, a upper triangular diagonal block of C is
*              updated with beta.
*
               IF( BETA.NE.ONE )THEN
                  DO 30, I = II, II+ISEC-1
                     CALL CSCAL ( I-II+1, BETA, C( II, I ), 1 )
   30             CONTINUE
               END IF
*
*              C := T1 + C, the upper triangular part of T1 is added to
*              the upper triangular diagonal block of C.
*
               DO 40, I = II, II+ISEC-1
                  CALL CAXPY ( I-II+1, ONE, T1( 1, I-II+1 ), 1,
     $                                                   C( II, I ), 1 )
   40          CONTINUE
*
*              C := T1' + C, the transpose of the lower triangular part
*              of T1 is added to the upper triangular diagonal block
*              of C. Notice that T1 is referenced by row and that the
*              maximum length of a vector referenced by CAXPY is CB.
*
               DO 60, JJ = II, II+ISEC-1, CB
                  JSEC = MIN( CB, II+ISEC-JJ )
                  DO 50, I = JJ, II+ISEC-1
                     CALL CAXPY ( MIN( JSEC, I-JJ+1 ), ONE,
     $                       T1( I-II+1, JJ-II+1 ), RCB, C( JJ, I ), 1 )
   50             CONTINUE
   60          CONTINUE
*
*              C := alpha*A*B' + beta*C  and  C := alpha*B*A' + C,
*              general matrix multiply on upper vertical blocks of C.
*
               IF( II.GT.1 )THEN
                  CALL CGEMM ( 'N', 'T', II-1, ISEC, K, ALPHA,
     $                            A( 1, 1 ), LDA, B( II, 1 ), LDB, BETA,
     $                                                 C( 1, II ), LDC )
                  CALL CGEMM ( 'N', 'T', II-1, ISEC, K, ALPHA,
     $                             B( 1, 1 ), LDB, A( II, 1 ), LDA, ONE,
     $                                                 C( 1, II ), LDC )
               END IF
   70       CONTINUE
         ELSE
*
*           Form  C := alpha*A'*B + alpha*B'*A + beta*C. Upper, Trans.
*
            DO 120, II = 1, N, RCB
               ISEC = MIN( RCB, N-II+1 )
*
*              T1 := alpha*A'*B, general matrix multiply on rectangular
*              blocks of A and B. T1 is square.
*
               CALL CGEMM ( 'T', 'N', ISEC, ISEC, K, ALPHA, A( 1, II ),
     $                     LDA, B( 1, II ), LDB, ZERO, T1( 1, 1 ), RCB )
*
*              C :=  beta*C, a upper triangular diagonal block of C is
*              updated with beta.
*
               IF( BETA.NE.ONE )THEN
                  DO 80, I = II, II+ISEC-1
                     CALL CSCAL ( I-II+1, BETA, C( II, I ), 1 )
   80             CONTINUE
               END IF
*
*              C := T1 + C, the upper triangular part of T1 is added to
*              the upper triangular diagonal block of C.
*
               DO 90, I = II, II+ISEC-1
                  CALL CAXPY ( I-II+1, ONE, T1( 1, I-II+1 ), 1,
     $                                                   C( II, I ), 1 )
   90          CONTINUE
*
*              C := T1' + C, the transpose of the lower triangular part
*              of T1 is added to the upper triangular diagonal block
*              of C. Notice that T1 is referenced by row and that the
*              maximum length of a vector referenced by CAXPY is CB.
*
               DO 110, JJ = II, II+ISEC-1, CB
                  JSEC = MIN( CB, II+ISEC-JJ )
                  DO 100, I = JJ, II+ISEC-1
                     CALL CAXPY ( MIN( JSEC, I-JJ+1 ), ONE,
     $                       T1( I-II+1, JJ-II+1 ), RCB, C( JJ, I ), 1 )
  100             CONTINUE
  110          CONTINUE
*
*              C := alpha*A'*B + beta*C  and  C := alpha*B'*A + C,
*              general matrix multiply on upper vertical blocks of C.
*
               IF( II.GT.1 )THEN
                  CALL CGEMM ( 'T', 'N', II-1, ISEC, K, ALPHA,
     $                            A( 1, 1 ), LDA, B( 1, II ), LDB, BETA,
     $                                                 C( 1, II ), LDC )
                  CALL CGEMM ( 'T', 'N', II-1, ISEC, K, ALPHA,
     $                             B( 1, 1 ), LDB, A( 1, II ), LDA, ONE,
     $                                                 C( 1, II ), LDC )
               END IF
  120       CONTINUE
         END IF
      ELSE
         IF( NOTR )THEN
*
*           Form  C := alpha*A*B' + alpha*B*A' + beta*C. Lower, Notr.
*
            DO 170, IX = N, 1, -RCB
               II = MAX( 1, IX-RCB+1 )
               ISEC = IX-II+1
*
*              T1 := alpha*A*B', general matrix multiply on rectangular
*              blocks of A and B. T1 is square.
*
               CALL CGEMM ( 'N', 'T', ISEC, ISEC, K, ALPHA, A( II, 1 ),
     $                     LDA, B( II, 1 ), LDB, ZERO, T1( 1, 1 ), RCB )
*
*              C :=  beta*C, a lower triangular diagonal block of C is
*              updated with beta.
*
               IF( BETA.NE.ONE )THEN
                  DO 130, I = II, II+ISEC-1
                     CALL CSCAL ( II+ISEC-I, BETA, C( I, I ), 1 )
  130             CONTINUE
               END IF
*
*              C := T1 + C, the lower triangular part of T1 is added to
*              the lower triangular diagonal block of C.
*
               DO 140, I = II, II+ISEC-1
                  CALL CAXPY ( II+ISEC-I, ONE, T1( I-II+1, I-II+1 ), 1,
     $                                                    C( I, I ), 1 )
  140          CONTINUE
*
*              C := T1' + C, the transpose of the upper triangular part
*              of T1 is added to the lower triangular diagonal block
*              of C. Notice that T1 is referenced by row and that the
*              maximum length of a vector referenced by CAXPY is CB.
*
               DO 160, JX = II+ISEC-1, II, -CB
                  JJ = MAX( II, JX-CB+1 )
                  JSEC = JX-JJ+1
                  DO 150, I = II, JJ+JSEC-1
                     CALL CAXPY ( MIN( JSEC, JJ+JSEC-I ), ONE,
     $                        T1( I-II+1, MAX( JJ-II+1, I-II+1 ) ), RCB,
     $                                         C( MAX( JJ, I ), I ), 1 )
  150             CONTINUE
  160          CONTINUE
*
*              C := alpha*A*B' + beta*C  and  C := alpha*B*A' + C,
*              general matrix multiply on lower vertical blocks of C.
*
               IF( II+ISEC.LE.N )THEN
                  CALL CGEMM ( 'N', 'T', N-II-ISEC+1, ISEC, K, ALPHA,
     $                            A( II+ISEC, 1 ), LDA, B( II, 1 ), LDB,
     $                                     BETA, C( II+ISEC, II ), LDC )
                  CALL CGEMM ( 'N', 'T', N-II-ISEC+1, ISEC, K, ALPHA,
     $                            B( II+ISEC, 1 ), LDB, A( II, 1 ), LDA,
     $                                      ONE, C( II+ISEC, II ), LDC )
               END IF
  170       CONTINUE
         ELSE
*
*           Form  C := alpha*A'*B + alpha*B'*A + beta*C. Lower, Trans.
*
            DO 220, IX = N, 1, -RCB
               II = MAX( 1, IX-RCB+1 )
               ISEC = IX-II+1
*
*              T1 := alpha*A*B', general matrix multiply on rectangular
*              blocks of A and B. T1 is square.
*
               CALL CGEMM ( 'T', 'N', ISEC, ISEC, K, ALPHA, A( 1, II ),
     $                     LDA, B( 1, II ), LDB, ZERO, T1( 1, 1 ), RCB )
*
*              C :=  beta*C, a lower triangular diagonal block of C is
*              updated with beta.
*
               IF( BETA.NE.ONE )THEN
                  DO 180, I = II, II+ISEC-1
                     CALL CSCAL ( II+ISEC-I, BETA, C( I, I ), 1 )
  180             CONTINUE
               END IF
*
*              C := T1 + C, the lower triangular part of T1 is added to
*              the lower triangular diagonal block of C.
*
               DO 190, I = II, II+ISEC-1
                  CALL CAXPY ( II+ISEC-I, ONE, T1( I-II+1, I-II+1 ), 1,
     $                                                    C( I, I ), 1 )
  190          CONTINUE
*
*              C := T1' + C, the transpose of the upper triangular part
*              of T1 is added to the lower triangular diagonal block
*              of C. Notice that T1 is referenced by row and that the
*              maximum length of a vector referenced by CAXPY is CB.
*
               DO 210, JX = II+ISEC-1, II, -CB
                  JJ = MAX( II, JX-CB+1 )
                  JSEC = JX-JJ+1
                  DO 200, I = II, JJ+JSEC-1
                     CALL CAXPY ( MIN( JSEC, JJ+JSEC-I ), ONE,
     $                        T1( I-II+1, MAX( JJ-II+1, I-II+1 ) ), RCB,
     $                                         C( MAX( JJ, I ), I ), 1 )
  200             CONTINUE
  210          CONTINUE
*
*              C := alpha*A'*B + beta*C  and  C := alpha*B'*A + C,
*              general matrix multiply on lower vertical blocks of C.
*
               IF( II+ISEC.LE.N )THEN
                  CALL CGEMM ( 'T', 'N', N-II-ISEC+1, ISEC, K, ALPHA,
     $                            A( 1, II+ISEC ), LDA, B( 1, II ), LDB,
     $                                     BETA, C( II+ISEC, II ), LDC )
                  CALL CGEMM ( 'T', 'N', N-II-ISEC+1, ISEC, K, ALPHA,
     $                            B( 1, II+ISEC ), LDB, A( 1, II ), LDA,
     $                                      ONE, C( II+ISEC, II ), LDC )
               END IF
  220       CONTINUE
         END IF
      END IF
*
      RETURN
*
*     End of CSYR2K.
*
      END
