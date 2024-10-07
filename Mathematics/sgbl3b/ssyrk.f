      SUBROUTINE SSYRK( UPLO, TRANS, N, K, ALPHA, A, LDA,
     $                   BETA, C, LDC )
*     .. Scalar Arguments ..
      CHARACTER*1        UPLO, TRANS
      INTEGER            N, K, LDA, LDC
      REAL               ALPHA, BETA
*     .. Array Arguments ..
      REAL               A( LDA, * ), C( LDC, * )
*     ..
*
*  Purpose
*  =======
*
*  SSYRK  performs one of the symmetric rank k operations
*
*     C := alpha*A*A' + beta*C,
*
*  or
*
*     C := alpha*A'*A + beta*C,
*
*  where  alpha and beta  are scalars, C is an  n by n  symmetric matrix
*  and  A  is an  n by k  matrix in the first case and a  k by n  matrix
*  in the second case.
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
*              TRANS = 'N' or 'n'   C := alpha*A*A' + beta*C.
*
*              TRANS = 'T' or 't'   C := alpha*A'*A + beta*C.
*
*              TRANS = 'C' or 'c'   C := alpha*A'*A + beta*C.
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
*           of  columns   of  the   matrix   A,   and  on   entry   with
*           TRANS = 'T' or 't' or 'C' or 'c',  K  specifies  the  number
*           of rows of the matrix  A.  K must be at least zero.
*           Unchanged on exit.
*
*  ALPHA  - REAL.
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  A      - REAL array of DIMENSION ( LDA, ka ), where ka is
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
*  BETA   - REAL.
*           On entry, BETA specifies the scalar beta.
*           Unchanged on exit.
*
*  C      - REAL array of DIMENSION ( LDC, n ).
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
*  -- Rewritten in December-1993.
*     GEMM-Based Level 3 BLAS.
*     Per Ling, Institute of Information Processing,
*     University of Umea, Sweden.
*
*
*     .. Local Scalars ..
      INTEGER            INFO, NROWA
      INTEGER            I, II, IX, ISEC, L, LL, LSEC
      LOGICAL            UPPER, NOTR, CLDA, SMALLN, TINYK
      REAL               DELTA
*     .. Intrinsic Functions ..
      INTRINSIC          MIN, MAX
*     .. External Functions ..
      LOGICAL            LSAME, SBIGP, SCLD
      EXTERNAL           LSAME, SBIGP, SCLD
*     .. External Subroutines ..
      EXTERNAL           XERBLA
      EXTERNAL           SGEMM, SGEMV, SSYR, SCOPY, SSCAL
*     .. Parameters ..
      REAL               ONE, ZERO
      INTEGER            SIP41, SIP42
      PARAMETER        ( ONE = 1.0E+0, ZERO = 0.0E+0,
     $                   SIP41 = 41, SIP42 = 42 )
*     .. User specified parameters for SSYRK ..
      INTEGER            RB, CB, RCB
      PARAMETER        ( RCB = 64, RB = 64, CB = 64 )
*     .. Local Arrays ..
      REAL               T1( RB, CB ), T2( RCB, RCB ), T3( RCB, RCB )
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
      IF( ( .NOT.UPPER ).AND.( .NOT.LSAME( UPLO, 'L' ) ) )THEN
         INFO = 1
      ELSE IF( ( .NOT.NOTR ).AND.( .NOT.LSAME( TRANS, 'T' ) ).AND.
     $                               ( .NOT.LSAME( TRANS, 'C' ) ) )THEN
         INFO = 2
      ELSE IF( N.LT.0 )THEN
         INFO = 3
      ELSE IF( K.LT.0 )THEN
         INFO = 4
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 7
      ELSE IF( LDC.LT.MAX( 1, N ) )THEN
         INFO = 10
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'SSYRK ', INFO )
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
               CALL SSCAL ( I, BETA, C( 1, I ), 1 )
   10       CONTINUE
         ELSE
            DO 20, I = 1, N
               CALL SSCAL ( N-I+1, BETA, C( I, I ), 1 )
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
*           Form  C := alpha*A*A' + beta*C. Upper, Notr.
*
            SMALLN = .NOT.SBIGP( SIP41 , N, K )
            IF( SMALLN )THEN
               TINYK = .NOT.SBIGP( SIP42 , N, K )
               DO 90, II = 1, N, RCB
                  ISEC = MIN( RCB, N-II+1 )
*
*                 C := alpha*A*A' + beta*C, general matrix multiply on
*                 upper vertical blocks of C.
*
                  IF( II.GT.1 )THEN
                     CALL SGEMM ( 'N', 'T', II-1, ISEC, K, ALPHA,
     $                                 A( 1, 1 ), LDA, A( II, 1 ), LDA,
     $                                          BETA, C( 1, II ), LDC )
                  END IF
                  IF( TINYK )THEN
*
*                    C :=  beta*C, a upper triangular diagonal block
*                    of C is updated with beta.
*
                     IF( BETA.NE.ONE )THEN
                        DO 30, I = II, II+ISEC-1
                           CALL SSCAL ( I-II+1, BETA, C( II, I ), 1 )
   30                   CONTINUE
                     END IF
*
*                    C := alpha*A*A' + C, symmetric matrix multiply.
*                    C is a symmetric diagonal block having upper
*                    triangular storage format.
*
                     DO 40, L = 1, K
                        CALL SSYR  ( 'U', ISEC, ALPHA, A( II, L ), 1,
     $                                               C( II, II ), LDC )
   40                CONTINUE
                  ELSE
*
*                    T2 := C, a upper triangular diagonal block of the
*                    symmetric matrix C is copied to the upper
*                    triangular part of T2.
*
                     DO 50, I = II, II+ISEC-1
                        CALL SCOPY ( I-II+1, C( II, I ), 1,
     $                                             T2( 1, I-II+1 ), 1 )
   50                CONTINUE
*
*                    T2 :=  beta*T2, the upper triangular part of T2 is
*                    updated with beta.
*
                     IF( BETA.NE.ONE )THEN
                        DO 60, I = II, II+ISEC-1
                           CALL SSCAL ( I-II+1, BETA,
     $                                             T2( 1, I-II+1 ), 1 )
   60                   CONTINUE
                     END IF
*
*                    T2 := alpha*A*A' + T2, symmetric matrix multiply.
*                    T2 contains a symmetric block having upper
*                    triangular storage format.
*
                     DO 70, L = 1, K
                        CALL SSYR  ( 'U', ISEC, ALPHA, A( II, L ), 1,
     $                                                T2( 1, 1 ), RCB )
   70                CONTINUE
*
*                    C := T2, the upper triangular part of T2 is copied
*                    back to C.
*
                     DO 80, I = II, II+ISEC-1
                        CALL SCOPY ( I-II+1, T2( 1, I-II+1 ), 1,
     $                                                  C( II, I ), 1 )
   80                CONTINUE
                  END IF
   90          CONTINUE
            ELSE
               DO 130, II = 1, N, RB
                  ISEC = MIN( RB, N-II+1 )
*
*                 C := alpha*A*A' + beta*C, general matrix multiply on
*                 upper vertical blocks of C.
*
                  IF( II.GT.1 )THEN
                     CALL SGEMM ( 'N', 'T', II-1, ISEC, K, ALPHA,
     $                                 A( 1, 1 ), LDA, A( II, 1 ), LDA,
     $                                          BETA, C( 1, II ), LDC )
                  END IF
                  DELTA = BETA
                  DO 120, LL = 1, K, CB
                     LSEC = MIN( CB, K-LL+1 )
*
*                    T1 := A, a rectangular block of A is copied to T1.
*
                     DO 100, L = LL, LL+LSEC-1
                        CALL SCOPY ( ISEC, A( II, L ), 1,
     $                                             T1( 1, L-LL+1 ), 1 )
  100                CONTINUE
*
*                    C := alpha*T1*T1' + delta*C, C is symmetric having
*                    triangular storage format. Delta is used instead
*                    of beta to avoid updating the block of C with beta
*                    multiple times.
*
                     DO 110, I = II, II+ISEC-1
                        CALL SGEMV ( 'N', I-II+1, LSEC, ALPHA,
     $                             T1( 1, 1 ), RB, T1( I-II+1, 1 ), RB,
     $                                           DELTA, C( II, I ), 1 )
  110                CONTINUE
                     DELTA = ONE
  120             CONTINUE
  130          CONTINUE
            END IF
         ELSE
*
*           Form  C := alpha*A'*A + beta*C. Upper, Trans.
*
            SMALLN = .NOT.SBIGP( SIP41 , N, K )
            IF( SMALLN )THEN
               TINYK = .NOT.SBIGP( SIP42 , N, K )
               DO 220, II = 1, N, RCB
                  ISEC = MIN( RCB, N-II+1 )
*
*                 C := alpha*A'*A + beta*C, general matrix multiply on
*                 upper vertical blocks of C.
*
                  IF( II.GT.1 )THEN
                     CALL SGEMM ( 'T', 'N', II-1, ISEC, K, ALPHA,
     $                                 A( 1, 1 ), LDA, A( 1, II ), LDA,
     $                                          BETA, C( 1, II ), LDC )
                  END IF
                  IF( TINYK )THEN
*
*                    C :=  beta*C, a upper triangular diagonal block
*                    of C is updated with beta.
*
                     IF( BETA.NE.ONE )THEN
                        DO 140, I = II, II+ISEC-1
                           CALL SSCAL ( I-II+1, BETA, C( II, I ), 1 )
  140                   CONTINUE
                     END IF
*
*                    C := alpha*A*A' + C, symmetric matrix multiply.
*                    C is a symmetric diagonal block having upper
*                    triangular storage format.
*
                     DO 150, L = 1, K
                        CALL SSYR  ( 'U', ISEC, ALPHA, A( L, II ), LDA,
     $                                               C( II, II ), LDC )
  150                CONTINUE
                  ELSE
*
*                    T2 := C, a upper triangular diagonal block of the
*                    symmetric matrix C is copied to the upper
*                    triangular part of T2.
*
                     DO 160, I = II, II+ISEC-1
                        CALL SCOPY ( I-II+1, C( II, I ), 1,
     $                                             T2( 1, I-II+1 ), 1 )
  160                CONTINUE
*
*                    T2 :=  beta*T2, the upper triangular part of T2 is
*                    updated with beta.
*
                     IF( BETA.NE.ONE )THEN
                        DO 170, I = II, II+ISEC-1
                           CALL SSCAL ( I-II+1, BETA,
     $                                             T2( 1, I-II+1 ), 1 )
  170                   CONTINUE
                     END IF
                     DO 200, LL = 1, K, RCB
                        LSEC = MIN( RCB, K-LL+1 )
*
*                       T3 :=  A', the transpose of a square block of A
*                       is copied to T3.
*
                        DO 180, I = II, II+ISEC-1
                           CALL SCOPY ( LSEC, A( LL, I ), 1,
     $                                           T3( I-II+1, 1 ), RCB )
  180                   CONTINUE
*
*                       T2 := alpha*T3*T3' + T2, symmetric matrix
*                       multiply. T2 contains a symmetric block having
*                       upper triangular storage format.
*
                        DO 190, L = LL, LL+LSEC-1
                           CALL SSYR  ( 'U', ISEC, ALPHA,
     $                            T3( 1, L-LL+1 ), 1, T2( 1, 1 ), RCB )
  190                   CONTINUE
  200                CONTINUE
*
*                    C := T2, the upper triangular part of T2 is copied
*                    back to C.
*
                     DO 210, I = II, II+ISEC-1
                        CALL SCOPY ( I-II+1, T2( 1, I-II+1 ), 1,
     $                                                  C( II, I ), 1 )
  210                CONTINUE
                  END IF
  220          CONTINUE
            ELSE
               CLDA = SCLD( LDA )
               DO 270, II = 1, N, RB
                  ISEC = MIN( RB, N-II+1 )
*
*                 C := alpha*A'*A + beta*C, general matrix multiply on
*                 upper vertical blocks of C.
*
                  IF( II.GT.1 )THEN
                     CALL SGEMM ( 'T', 'N', II-1, ISEC, K, ALPHA,
     $                                 A( 1, 1 ), LDA, A( 1, II ), LDA,
     $                                          BETA, C( 1, II ), LDC )
                  END IF
                  DELTA = BETA
                  DO 260, LL = 1, K, CB
                     LSEC = MIN( CB, K-LL+1 )
*
*                    T1 := A', the transpose of a rectangular block
*                    of A is copied to T1.
*
                     IF( CLDA )THEN
                        DO 230, I = II, II+ISEC-1
                           CALL SCOPY ( LSEC, A( LL, I ), 1,
     $                                            T1( I-II+1, 1 ), RB )
  230                   CONTINUE
                     ELSE
                        DO 240, L = LL, LL+LSEC-1
                           CALL SCOPY ( ISEC, A( L, II ), LDA,
     $                                             T1( 1, L-LL+1 ), 1 )
  240                   CONTINUE
                     END IF
*
*                    C := alpha*T1*T1' + delta*C, C is symmetric having
*                    triangular storage format. Delta is used instead
*                    of beta to avoid updating the block of C with beta
*                    multiple times.
*
                     DO 250, I = II, II+ISEC-1
                        CALL SGEMV ( 'N', I-II+1, LSEC, ALPHA,
     $                             T1( 1, 1 ), RB, T1( I-II+1, 1 ), RB,
     $                                           DELTA, C( II, I ), 1 )
  250                CONTINUE
                     DELTA = ONE
  260             CONTINUE
  270          CONTINUE
            END IF
         END IF
      ELSE
         IF( NOTR )THEN
*
*           Form  C := alpha*A*A' + beta*C. Lower, Notr.
*
            SMALLN = .NOT.SBIGP( SIP41 , N, K )
            IF( SMALLN )THEN
               TINYK = .NOT.SBIGP( SIP42 , N, K )
               DO 340, IX = N, 1, -RCB
                  II = MAX( 1, IX-RCB+1 )
                  ISEC = IX-II+1
                  IF( TINYK )THEN
*
*                    C :=  beta*C, a lower triangular diagonal block
*                    of C is updated with beta.
*
                     IF( BETA.NE.ONE )THEN
                        DO 280, I = II, II+ISEC-1
                           CALL SSCAL ( II+ISEC-I, BETA, C( I, I ), 1 )
  280                   CONTINUE
                     END IF
*
*                    C := alpha*A*A' + C, symmetric matrix multiply.
*                    C is a symmetric diagonal block having lower
*                    triangular storage format.
*
                     DO 290, L = 1, K
                        CALL SSYR  ( 'L', ISEC, ALPHA, A( II, L ), 1,
     $                                               C( II, II ), LDC )
  290                CONTINUE
                  ELSE
*
*                    T2 := C, a lower triangular diagonal block of the
*                    symmetric matrix C is copied to the lower
*                    triangular part of T2.
*
                     DO 300, I = II, II+ISEC-1
                        CALL SCOPY ( II+ISEC-I, C( I, I ), 1,
     $                                        T2( I-II+1, I-II+1 ), 1 )
  300                CONTINUE
*
*                    T2 :=  beta*T2, the lower triangular part of T2 is
*                    updated with beta.
*
                     IF( BETA.NE.ONE )THEN
                        DO 310, I = II, II+ISEC-1
                           CALL SSCAL ( II+ISEC-I, BETA,
     $                                        T2( I-II+1, I-II+1 ), 1 )
  310                   CONTINUE
                     END IF
*
*                    T2 := alpha*A*A' + T2, symmetric matrix multiply.
*                    T2 contains a symmetric block having lower
*                    triangular storage format.
*
                     DO 320, L = 1, K
                        CALL SSYR  ( 'L', ISEC, ALPHA, A( II, L ), 1,
     $                                                T2( 1, 1 ), RCB )
  320                CONTINUE
*
*                    C := T2, the lower triangular part of T2 is copied
*                    back to C.
*
                     DO 330, I = II, II+ISEC-1
                        CALL SCOPY ( II+ISEC-I, T2( I-II+1, I-II+1 ),
     $                                                1, C( I, I ), 1 )
  330                CONTINUE
                  END IF
*
*                 C := alpha*A*A' + beta*C, general matrix multiply on
*                 lower vertical blocks of C.
*
                  IF( II+ISEC.LE.N )THEN
                     CALL SGEMM ( 'N', 'T', N-II-ISEC+1, ISEC, K,
     $                         ALPHA, A( II+ISEC, 1 ), LDA, A( II, 1 ),
     $                               LDA, BETA, C( II+ISEC, II ), LDC )
                  END IF
  340          CONTINUE
            ELSE
               DO 380, IX = N, 1, -RB
                  II = MAX( 1, IX-RB+1 )
                  ISEC = IX-II+1
                  DELTA = BETA
                  DO 370, LL = 1, K, CB
                     LSEC = MIN( CB, K-LL+1 )
*
*                    T1 := A, a rectangular block of A is copied to T1.
*
                     DO 350, L = LL, LL+LSEC-1
                        CALL SCOPY ( ISEC, A( II, L ), 1,
     $                                             T1( 1, L-LL+1 ), 1 )
  350                CONTINUE
*
*                    C := alpha*T1*T1' + delta*C, C is symmetric having
*                    triangular storage format. Delta is used instead
*                    of beta to avoid updating the block of C with beta
*                    multiple times.
*
                     DO 360, I = II, II+ISEC-1
                        CALL SGEMV ( 'N', II+ISEC-I, LSEC, ALPHA,
     $                        T1( I-II+1, 1 ), RB, T1( I-II+1, 1 ), RB,
     $                                            DELTA, C( I, I ), 1 )
  360                CONTINUE
                     DELTA = ONE
  370             CONTINUE
*
*                 C := alpha*A*A' + beta*C, general matrix multiply on
*                 lower vertical blocks of C.
*
                  IF( II+ISEC.LE.N )THEN
                     CALL SGEMM ( 'N', 'T', N-II-ISEC+1, ISEC, K,
     $                         ALPHA, A( II+ISEC, 1 ), LDA, A( II, 1 ),
     $                               LDA, BETA, C( II+ISEC, II ), LDC )
                  END IF
  380          CONTINUE
            END IF
         ELSE
*
*           Form  C := alpha*A'*A + beta*C. Lower, Trans.
*
            SMALLN = .NOT.SBIGP( SIP41 , N, K )
            IF( SMALLN )THEN
               TINYK = .NOT.SBIGP( SIP42 , N, K )
               DO 470, IX = N, 1, -RCB
                  II = MAX( 1, IX-RCB+1 )
                  ISEC = IX-II+1
                  IF( TINYK )THEN
*
*                    C :=  beta*C, a lower triangular diagonal block
*                    of C is updated with beta.
*
                     IF( BETA.NE.ONE )THEN
                        DO 390, I = II, II+ISEC-1
                           CALL SSCAL ( II+ISEC-I, BETA, C( I, I ), 1 )
  390                   CONTINUE
                     END IF
*
*                    C := alpha*A*A' + C, symmetric matrix multiply.
*                    C is a symmetric diagonal block having lower
*                    triangular storage format.
*
                     DO 400, L = 1, K
                        CALL SSYR  ( 'L', ISEC, ALPHA, A( L, II ), LDA,
     $                                               C( II, II ), LDC )
  400                CONTINUE
                  ELSE
*
*                    T2 := C, a lower triangular diagonal block of the
*                    symmetric matrix C is copied to the lower
*                    triangular part of T2.
*
                     DO 410, I = II, II+ISEC-1
                        CALL SCOPY ( II+ISEC-I, C( I, I ), 1,
     $                                        T2( I-II+1, I-II+1 ), 1 )
  410                CONTINUE
*
*                    T2 :=  beta*T2, the lower triangular part of T2 is
*                    updated with beta.
*
                     IF( BETA.NE.ONE )THEN
                        DO 420, I = II, II+ISEC-1
                           CALL SSCAL ( II+ISEC-I, BETA,
     $                                        T2( I-II+1, I-II+1 ), 1 )
  420                   CONTINUE
                     END IF
                     DO 450, LL = 1, K, RCB
                        LSEC = MIN( RCB, K-LL+1 )
*
*                       T3 :=  A', the transpose of a square block of A
*                       is copied to T3.
*
                        DO 430, I = II, II+ISEC-1
                           CALL SCOPY ( LSEC, A( LL, I ), 1,
     $                                           T3( I-II+1, 1 ), RCB )
  430                   CONTINUE
*
*                       T2 := alpha*T3*T3' + T2, symmetric matrix
*                       multiply. T2 contains a symmetric block having
*                       lower triangular storage format.
*
                        DO 440, L = LL, LL+LSEC-1
                           CALL SSYR  ( 'L', ISEC, ALPHA,
     $                            T3( 1, L-LL+1 ), 1, T2( 1, 1 ), RCB )
  440                   CONTINUE
  450                CONTINUE
*
*                    C := T2, the lower triangular part of T2 is copied
*                    back to C.
*
                     DO 460, I = II, II+ISEC-1
                        CALL SCOPY ( II+ISEC-I, T2( I-II+1, I-II+1 ),
     $                                                1, C( I, I ), 1 )
  460                CONTINUE
                  END IF
*
*                 C := alpha*A'*A + beta*C, general matrix multiply on
*                 lower vertical blocks of C.
*
                  IF( II+ISEC.LE.N )THEN
                     CALL SGEMM ( 'T', 'N', N-II-ISEC+1, ISEC, K,
     $                         ALPHA, A( 1, II+ISEC ), LDA, A( 1, II ),
     $                               LDA, BETA, C( II+ISEC, II ), LDC )
                  END IF
  470          CONTINUE
            ELSE
               CLDA = SCLD( LDA )
               DO 520, IX = N, 1, -RB
                  II = MAX( 1, IX-RB+1 )
                  ISEC = IX-II+1
                  DELTA = BETA
                  DO 510, LL = 1, K, CB
                     LSEC = MIN( CB, K-LL+1 )
*
*                    T1 := A', the transpose of a rectangular block
*                    of A is copied to T1.
*
                     IF( CLDA )THEN
                        DO 480, I = II, II+ISEC-1
                           CALL SCOPY ( LSEC, A( LL, I ), 1,
     $                                            T1( I-II+1, 1 ), RB )
  480                   CONTINUE
                     ELSE
                        DO 490, L = LL, LL+LSEC-1
                           CALL SCOPY ( ISEC, A( L, II ), LDA,
     $                                             T1( 1, L-LL+1 ), 1 )
  490                   CONTINUE
                     END IF
*
*                    C := alpha*T1*T1' + delta*C, C is symmetric having
*                    triangular storage format. Delta is used instead
*                    of beta to avoid updating the block of C with beta
*                    multiple times.
*
                     DO 500, I = II, II+ISEC-1
                        CALL SGEMV ( 'N', II+ISEC-I, LSEC, ALPHA,
     $                        T1( I-II+1, 1 ), RB, T1( I-II+1, 1 ), RB,
     $                                            DELTA, C( I, I ), 1 )
  500                CONTINUE
                     DELTA = ONE
  510             CONTINUE
*
*                 C := alpha*A'*A + beta*C, general matrix multiply on
*                 lower vertical blocks of C.
*
                  IF( II+ISEC.LE.N )THEN
                     CALL SGEMM ( 'T', 'N', N-II-ISEC+1, ISEC, K,
     $                         ALPHA, A( 1, II+ISEC ), LDA, A( 1, II ),
     $                               LDA, BETA, C( II+ISEC, II ), LDC )
                  END IF
  520          CONTINUE
            END IF
         END IF
      END IF
*
      RETURN
*
*     End of SSYRK.
*
      END
