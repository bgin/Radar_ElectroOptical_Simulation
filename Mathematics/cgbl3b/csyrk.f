      SUBROUTINE CSYRK ( UPLO, TRANS, N, K, ALPHA, A, LDA,
     $                   BETA, C, LDC )
*     .. Scalar Arguments ..
      CHARACTER*1        UPLO, TRANS
      INTEGER            N, K, LDA, LDC
      COMPLEX            ALPHA, BETA
*     .. Array Arguments ..
      COMPLEX            A( LDA, * ), C( LDC, * )
*     ..
*
*  Purpose
*  =======
*
*  CSYRK  performs one of the symmetric rank k operations
*
*     C := alpha*A*A' + beta*C,
*
*  or
*
*     C := alpha*A'*A + beta*C,
*
*  where  alpha and beta  are scalars,  C is an  n by n symmetric matrix
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
*           TRANS = 'T' or 't',  K  specifies  the number of rows of the
*           matrix A.  K must be at least zero.
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
      INTEGER            I, II, IX, ISEC, L, LL, LSEC
      LOGICAL            UPPER, NOTR, CLDA, SMALLN, TINYK
      COMPLEX            DELTA
*     .. Intrinsic Functions ..
      INTRINSIC          MIN, MAX
*     .. External Functions ..
      LOGICAL            LSAME, CBIGP, CCLD
      EXTERNAL           LSAME, CBIGP, CCLD
*     .. External Subroutines ..
      EXTERNAL           XERBLA
      EXTERNAL           CGEMM, CGEMV, CCOPY, CSCAL
*     .. Parameters ..
      COMPLEX            ONE, ZERO
      INTEGER            CIP41, CIP42
      PARAMETER        ( ONE = ( 1.0E+0, 0.0E+0 ),
     $                   ZERO = ( 0.0E+0, 0.0E+0 ),
     $                   CIP41 = 41, CIP42 = 42 )
*     .. User specified parameters for CSYRK ..
      INTEGER            RB, CB, RCB
      PARAMETER        ( RCB = 64, RB = 64, CB = 64 )
*     .. Local Arrays ..
      COMPLEX            T1( RB, CB ), T2( RCB, RCB ), T3( RCB, RCB )
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
      ELSE IF( ( ( .NOT.NOTR ) ).AND.( .NOT.LSAME( TRANS, 'T' ) ) )THEN
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
         CALL XERBLA( 'CSYRK ', INFO )
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
*           Form  C := alpha*A*A' + beta*C. Upper, Notr.
*
            SMALLN = .NOT.CBIGP( CIP41 , N, K )
            IF( SMALLN )THEN
               TINYK = .NOT.CBIGP( CIP42 , N, K )
               DO 110, II = 1, N, RCB
                  ISEC = MIN( RCB, N-II+1 )
*
*                 C := alpha*A*A' + beta*C, general matrix multiply on
*                 upper vertical blocks of C.
*
                  IF( II.GT.1 )THEN
                     CALL CGEMM ( 'N', 'T', II-1, ISEC, K, ALPHA,
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
                           CALL CSCAL ( I-II+1, BETA, C( II, I ), 1 )
   30                   CONTINUE
                     END IF
*
*                    C := alpha*A*A' + C, symmetric matrix multiply.
*                    C is a symmetric diagonal block having upper
*                    triangular storage format.
*
                     DO 50, I = II, II+ISEC-1
                        DO 40, L = 1, K
                           CALL CAXPY ( I-II+1, ALPHA*A( I, L ),
     $                                   A( II, L ), 1, C( II, I ), 1 )
   40                   CONTINUE
   50                CONTINUE
                  ELSE
*
*                    T2 := C, a upper triangular diagonal block of the
*                    symmetric matrix C is copied to the upper
*                    triangular part of T2.
*
                     DO 60, I = II, II+ISEC-1
                        CALL CCOPY ( I-II+1, C( II, I ), 1,
     $                                             T2( 1, I-II+1 ), 1 )
   60                CONTINUE
*
*                    T2 :=  beta*T2, the upper triangular part of T2 is
*                    updated with beta.
*
                     IF( BETA.NE.ONE )THEN
                        DO 70, I = II, II+ISEC-1
                           CALL CSCAL ( I-II+1, BETA,
     $                                             T2( 1, I-II+1 ), 1 )
   70                   CONTINUE
                     END IF
*
*                    T2 := alpha*A*A' + T2, symmetric matrix multiply.
*                    T2 contains a symmetric block having upper
*                    triangular storage format.
*
                     DO 90, I = II, II+ISEC-1
                        DO 80, L = 1, K
                           CALL CAXPY ( I-II+1, ALPHA*A( I, L ),
     $                              A( II, L ), 1, T2( 1, I-II+1 ), 1 )
   80                   CONTINUE
   90                CONTINUE
*
*                    C := T2, the upper triangular part of T2 is copied
*                    back to C.
*
                     DO 100, I = II, II+ISEC-1
                        CALL CCOPY ( I-II+1, T2( 1, I-II+1 ), 1,
     $                                                  C( II, I ), 1 )
  100                CONTINUE
                  END IF
  110          CONTINUE
            ELSE
               DO 150, II = 1, N, RB
                  ISEC = MIN( RB, N-II+1 )
*
*                 C := alpha*A*A' + beta*C, general matrix multiply on
*                 upper vertical blocks of C.
*
                  IF( II.GT.1 )THEN
                     CALL CGEMM ( 'N', 'T', II-1, ISEC, K, ALPHA,
     $                                 A( 1, 1 ), LDA, A( II, 1 ), LDA,
     $                                          BETA, C( 1, II ), LDC )
                  END IF
                  DELTA = BETA
                  DO 140, LL = 1, K, CB
                     LSEC = MIN( CB, K-LL+1 )
*
*                    T1 := A, a rectangular block of A is copied to T1.
*
                     DO 120, L = LL, LL+LSEC-1
                        CALL CCOPY ( ISEC, A( II, L ), 1,
     $                                             T1( 1, L-LL+1 ), 1 )
  120                CONTINUE
*
*                    C := alpha*T1*T1' + delta*C, C is symmetric having
*                    triangular storage format. Delta is used instead
*                    of beta to avoid updating the block of C with beta
*                    multiple times.
*
                     DO 130, I = II, II+ISEC-1
                        CALL CGEMV ( 'N', I-II+1, LSEC, ALPHA,
     $                             T1( 1, 1 ), RB, T1( I-II+1, 1 ), RB,
     $                                           DELTA, C( II, I ), 1 )
  130                CONTINUE
                     DELTA = ONE
  140             CONTINUE
  150          CONTINUE
            END IF
         ELSE
*
*           Form  C := alpha*A'*A + beta*C. Upper, Trans.
*
            SMALLN = .NOT.CBIGP( CIP41 , N, K )
            IF( SMALLN )THEN
               TINYK = .NOT.CBIGP( CIP42 , N, K )
               DO 260, II = 1, N, RCB
                  ISEC = MIN( RCB, N-II+1 )
*
*                 C := alpha*A'*A + beta*C, general matrix multiply on
*                 upper vertical blocks of C.
*
                  IF( II.GT.1 )THEN
                     CALL CGEMM ( 'T', 'N', II-1, ISEC, K, ALPHA,
     $                                 A( 1, 1 ), LDA, A( 1, II ), LDA,
     $                                          BETA, C( 1, II ), LDC )
                  END IF
                  IF( TINYK )THEN
*
*                    C :=  beta*C, a upper triangular diagonal block
*                    of C is updated with beta.
*
                     IF( BETA.NE.ONE )THEN
                        DO 160, I = II, II+ISEC-1
                           CALL CSCAL ( I-II+1, BETA, C( II, I ), 1 )
  160                   CONTINUE
                     END IF
*
*                    C := alpha*A*A' + C, symmetric matrix multiply.
*                    C is a symmetric diagonal block having upper
*                    triangular storage format.
*
                     DO 180, I = II, II+ISEC-1
                        DO 170, L = 1, K
                           CALL CAXPY ( I-II+1, ALPHA*A( L, I ),
     $                                 A( L, II ), LDA, C( II, I ), 1 )
  170                   CONTINUE
  180                CONTINUE
                  ELSE
*
*                    T2 := C, a upper triangular diagonal block of the
*                    symmetric matrix C is copied to the upper
*                    triangular part of T2.
*
                     DO 190, I = II, II+ISEC-1
                        CALL CCOPY ( I-II+1, C( II, I ), 1,
     $                                             T2( 1, I-II+1 ), 1 )
  190                CONTINUE
*
*                    T2 :=  beta*T2, the upper triangular part of T2 is
*                    updated with beta.
*
                     IF( BETA.NE.ONE )THEN
                        DO 200, I = II, II+ISEC-1
                           CALL CSCAL ( I-II+1, BETA,
     $                                             T2( 1, I-II+1 ), 1 )
  200                   CONTINUE
                     END IF
                     DO 240, LL = 1, K, RCB
                        LSEC = MIN( RCB, K-LL+1 )
*
*                       T3 :=  A', the transpose of a square block of A
*                       is copied to T3.
*
                        DO 210, I = II, II+ISEC-1
                           CALL CCOPY ( LSEC, A( LL, I ), 1,
     $                                           T3( I-II+1, 1 ), RCB )
  210                   CONTINUE
*
*                       T2 := alpha*T3*T3' + T2, symmetric matrix
*                       multiply. T2 contains a symmetric block having
*                       upper triangular storage format.
*
                        DO 230, I = II, II+ISEC-1
                           DO 220, L = LL, LL+LSEC-1
                              CALL CAXPY ( I-II+1,
     $                                      ALPHA*T3( I-II+1, L-LL+1 ),
     $                                              T3( 1, L-LL+1 ), 1,
     $                                             T2( 1, I-II+1 ), 1 )
  220                      CONTINUE
  230                   CONTINUE
  240                CONTINUE
*
*                    C := T2, the upper triangular part of T2 is copied
*                    back to C.
*
                     DO 250, I = II, II+ISEC-1
                        CALL CCOPY ( I-II+1, T2( 1, I-II+1 ), 1,
     $                                                  C( II, I ), 1 )
  250                CONTINUE
                  END IF
  260          CONTINUE
            ELSE
               CLDA = CCLD( LDA )
               DO 310, II = 1, N, RB
                  ISEC = MIN( RB, N-II+1 )
*
*                 C := alpha*A'*A + beta*C, general matrix multiply on
*                 upper vertical blocks of C.
*
                  IF( II.GT.1 )THEN
                     CALL CGEMM ( 'T', 'N', II-1, ISEC, K, ALPHA,
     $                                 A( 1, 1 ), LDA, A( 1, II ), LDA,
     $                                          BETA, C( 1, II ), LDC )
                  END IF
                  DELTA = BETA
                  DO 300, LL = 1, K, CB
                     LSEC = MIN( CB, K-LL+1 )
*
*                    T1 := A', the transpose of a rectangular block
*                    of A is copied to T1.
*
                     IF( CLDA )THEN
                        DO 270, I = II, II+ISEC-1
                           CALL CCOPY ( LSEC, A( LL, I ), 1,
     $                                            T1( I-II+1, 1 ), RB )
  270                   CONTINUE
                     ELSE
                        DO 280, L = LL, LL+LSEC-1
                           CALL CCOPY ( ISEC, A( L, II ), LDA,
     $                                             T1( 1, L-LL+1 ), 1 )
  280                   CONTINUE
                     END IF
*
*                    C := alpha*T1*T1' + delta*C, C is symmetric having
*                    triangular storage format. Delta is used instead
*                    of beta to avoid updating the block of C with beta
*                    multiple times.
*
                     DO 290, I = II, II+ISEC-1
                        CALL CGEMV ( 'N', I-II+1, LSEC, ALPHA,
     $                             T1( 1, 1 ), RB, T1( I-II+1, 1 ), RB,
     $                                           DELTA, C( II, I ), 1 )
  290                CONTINUE
                     DELTA = ONE
  300             CONTINUE
  310          CONTINUE
            END IF
         END IF
      ELSE
         IF( NOTR )THEN
*
*           Form  C := alpha*A*A' + beta*C. Lower, Notr.
*
            SMALLN = .NOT.CBIGP( CIP41 , N, K )
            IF( SMALLN )THEN
               TINYK = .NOT.CBIGP( CIP42 , N, K )
               DO 400, IX = N, 1, -RCB
                  II = MAX( 1, IX-RCB+1 )
                  ISEC = IX-II+1
                  IF( TINYK )THEN
*
*                    C :=  beta*C, a lower triangular diagonal block
*                    of C is updated with beta.
*
                     IF( BETA.NE.ONE )THEN
                        DO 320, I = II, II+ISEC-1
                           CALL CSCAL ( II+ISEC-I, BETA, C( I, I ), 1 )
  320                   CONTINUE
                     END IF
*
*                    C := alpha*A*A' + C, symmetric matrix multiply.
*                    C is a symmetric diagonal block having lower
*                    triangular storage format.
*
                     DO 340, I = II, II+ISEC-1
                        DO 330, L = 1, K
                           CALL CAXPY ( II+ISEC-I, ALPHA*A( I, L ),
     $                                   A( I, L ), 1, C( I, I ), 1 )
  330                   CONTINUE
  340                CONTINUE
                  ELSE
*
*                    T2 := C, a lower triangular diagonal block of the
*                    symmetric matrix C is copied to the lower
*                    triangular part of T2.
*
                     DO 350, I = II, II+ISEC-1
                        CALL CCOPY ( II+ISEC-I, C( I, I ), 1,
     $                                        T2( I-II+1, I-II+1 ), 1 )
  350                CONTINUE
*
*                    T2 :=  beta*T2, the lower triangular part of T2 is
*                    updated with beta.
*
                     IF( BETA.NE.ONE )THEN
                        DO 360, I = II, II+ISEC-1
                           CALL CSCAL ( II+ISEC-I, BETA,
     $                                        T2( I-II+1, I-II+1 ), 1 )
  360                   CONTINUE
                     END IF
*
*                    T2 := alpha*A*A' + T2, symmetric matrix multiply.
*                    T2 contains a symmetric block having lower
*                    triangular storage format.
*
                     DO 380, I = II, II+ISEC-1
                        DO 370, L = 1, K
                           CALL CAXPY ( II+ISEC-I, ALPHA*A( I, L ),
     $                          A( I, L ), 1, T2( I-II+1, I-II+1 ), 1 )
  370                   CONTINUE
  380                CONTINUE
*
*                    C := T2, the lower triangular part of T2 is copied
*                    back to C.
*
                     DO 390, I = II, II+ISEC-1
                        CALL CCOPY ( II+ISEC-I, T2( I-II+1, I-II+1 ),
     $                                                1, C( I, I ), 1 )
  390                CONTINUE
                  END IF
*
*                 C := alpha*A*A' + beta*C, general matrix multiply on
*                 lower vertical blocks of C.
*
                  IF( II+ISEC.LE.N )THEN
                     CALL CGEMM ( 'N', 'T', N-II-ISEC+1, ISEC, K,
     $                         ALPHA, A( II+ISEC, 1 ), LDA, A( II, 1 ),
     $                               LDA, BETA, C( II+ISEC, II ), LDC )
                  END IF
  400          CONTINUE
            ELSE
               DO 440, IX = N, 1, -RB
                  II = MAX( 1, IX-RB+1 )
                  ISEC = IX-II+1
                  DELTA = BETA
                  DO 430, LL = 1, K, CB
                     LSEC = MIN( CB, K-LL+1 )
*
*                    T1 := A, a rectangular block of A is copied to T1.
*
                     DO 410, L = LL, LL+LSEC-1
                        CALL CCOPY ( ISEC, A( II, L ), 1,
     $                                             T1( 1, L-LL+1 ), 1 )
  410                CONTINUE
*
*                    C := alpha*T1*T1' + delta*C, C is symmetric having
*                    triangular storage format. Delta is used instead
*                    of beta to avoid updating the block of C with beta
*                    multiple times.
*
                     DO 420, I = II, II+ISEC-1
                        CALL CGEMV ( 'N', II+ISEC-I, LSEC, ALPHA,
     $                        T1( I-II+1, 1 ), RB, T1( I-II+1, 1 ), RB,
     $                                            DELTA, C( I, I ), 1 )
  420                CONTINUE
                     DELTA = ONE
  430             CONTINUE
*
*                 C := alpha*A*A' + beta*C, general matrix multiply on
*                 lower vertical blocks of C.
*
                  IF( II+ISEC.LE.N )THEN
                     CALL CGEMM ( 'N', 'T', N-II-ISEC+1, ISEC, K,
     $                         ALPHA, A( II+ISEC, 1 ), LDA, A( II, 1 ),
     $                               LDA, BETA, C( II+ISEC, II ), LDC )
                  END IF
  440          CONTINUE
            END IF
         ELSE
*
*           Form  C := alpha*A'*A + beta*C. Lower, Trans.
*
            SMALLN = .NOT.CBIGP( CIP41 , N, K )
            IF( SMALLN )THEN
               TINYK = .NOT.CBIGP( CIP42 , N, K )
               DO 550, IX = N, 1, -RCB
                  II = MAX( 1, IX-RCB+1 )
                  ISEC = IX-II+1
                  IF( TINYK )THEN
*
*                    C :=  beta*C, a lower triangular diagonal block
*                    of C is updated with beta.
*
                     IF( BETA.NE.ONE )THEN
                        DO 450, I = II, II+ISEC-1
                           CALL CSCAL ( II+ISEC-I, BETA, C( I, I ), 1 )
  450                   CONTINUE
                     END IF
*
*                    C := alpha*A*A' + C, symmetric matrix multiply.
*                    C is a symmetric diagonal block having lower
*                    triangular storage format.
*
                     DO 470, I = II, II+ISEC-1
                        DO 460, L = 1, K
                           CALL CAXPY ( II+ISEC-I, ALPHA*A( L, I ),
     $                                   A( L, I ), LDA, C( I, I ), 1 )
  460                   CONTINUE
  470                CONTINUE
                  ELSE
*
*                    T2 := C, a lower triangular diagonal block of the
*                    symmetric matrix C is copied to the lower
*                    triangular part of T2.
*
                     DO 480, I = II, II+ISEC-1
                        CALL CCOPY ( II+ISEC-I, C( I, I ), 1,
     $                                        T2( I-II+1, I-II+1 ), 1 )
  480                CONTINUE
*
*                    T2 :=  beta*T2, the lower triangular part of T2 is
*                    updated with beta.
*
                     IF( BETA.NE.ONE )THEN
                        DO 490, I = II, II+ISEC-1
                           CALL CSCAL ( II+ISEC-I, BETA,
     $                                        T2( I-II+1, I-II+1 ), 1 )
  490                   CONTINUE
                     END IF
                     DO 530, LL = 1, K, RCB
                        LSEC = MIN( RCB, K-LL+1 )
*
*                       T3 :=  A', the transpose of a square block of A
*                       is copied to T3.
*
                        DO 500, I = II, II+ISEC-1
                           CALL CCOPY ( LSEC, A( LL, I ), 1,
     $                                           T3( I-II+1, 1 ), RCB )
  500                   CONTINUE
*
*                       T2 := alpha*T3*T3' + T2, symmetric matrix
*                       multiply. T2 contains a symmetric block having
*                       lower triangular storage format.
*
                        DO 520, I = II, II+ISEC-1
                           DO 510, L = LL, LL+LSEC-1
                              CALL CAXPY ( II+ISEC-I,
     $                                      ALPHA*T3( I-II+1, L-LL+1 ),
     $                                         T3( I-II+1, L-LL+1 ), 1,
     $                                        T2( I-II+1, I-II+1 ), 1 )
  510                      CONTINUE
  520                   CONTINUE
  530                CONTINUE
*
*                    C := T2, the lower triangular part of T2 is copied
*                    back to C.
*
                     DO 540, I = II, II+ISEC-1
                        CALL CCOPY ( II+ISEC-I, T2( I-II+1, I-II+1 ),
     $                                                1, C( I, I ), 1 )
  540                CONTINUE
                  END IF
*
*                 C := alpha*A'*A + beta*C, general matrix multiply on
*                 lower vertical blocks of C.
*
                  IF( II+ISEC.LE.N )THEN
                     CALL CGEMM ( 'T', 'N', N-II-ISEC+1, ISEC, K,
     $                         ALPHA, A( 1, II+ISEC ), LDA, A( 1, II ),
     $                               LDA, BETA, C( II+ISEC, II ), LDC )
                  END IF
  550          CONTINUE
            ELSE
               CLDA = CCLD( LDA )
               DO 600, IX = N, 1, -RB
                  II = MAX( 1, IX-RB+1 )
                  ISEC = IX-II+1
                  DELTA = BETA
                  DO 590, LL = 1, K, CB
                     LSEC = MIN( CB, K-LL+1 )
*
*                    T1 := A', the transpose of a rectangular block
*                    of A is copied to T1.
*
                     IF( CLDA )THEN
                        DO 560, I = II, II+ISEC-1
                           CALL CCOPY ( LSEC, A( LL, I ), 1,
     $                                            T1( I-II+1, 1 ), RB )
  560                   CONTINUE
                     ELSE
                        DO 570, L = LL, LL+LSEC-1
                           CALL CCOPY ( ISEC, A( L, II ), LDA,
     $                                             T1( 1, L-LL+1 ), 1 )
  570                   CONTINUE
                     END IF
*
*                    C := alpha*T1*T1' + delta*C, C is symmetric having
*                    triangular storage format. Delta is used instead
*                    of beta to avoid updating the block of C with beta
*                    multiple times.
*
                     DO 580, I = II, II+ISEC-1
                        CALL CGEMV ( 'N', II+ISEC-I, LSEC, ALPHA,
     $                        T1( I-II+1, 1 ), RB, T1( I-II+1, 1 ), RB,
     $                                            DELTA, C( I, I ), 1 )
  580                CONTINUE
                     DELTA = ONE
  590             CONTINUE
*
*                 C := alpha*A'*A + beta*C, general matrix multiply on
*                 lower vertical blocks of C.
*
                  IF( II+ISEC.LE.N )THEN
                     CALL CGEMM ( 'T', 'N', N-II-ISEC+1, ISEC, K,
     $                         ALPHA, A( 1, II+ISEC ), LDA, A( 1, II ),
     $                               LDA, BETA, C( II+ISEC, II ), LDC )
                  END IF
  600          CONTINUE
            END IF
         END IF
      END IF
*
      RETURN
*
*     End of CSYRK.
*
      END
