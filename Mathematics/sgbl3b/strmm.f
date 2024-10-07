      SUBROUTINE STRMM( SIDE, UPLO, TRANSA, DIAG, M, N, ALPHA, A, LDA,
     $                   C, LDC )
*     .. Scalar Arguments ..
      CHARACTER*1        SIDE, UPLO, TRANSA, DIAG
      INTEGER            M, N, LDA, LDC
      REAL               ALPHA
*     .. Array Arguments ..
      REAL               A( LDA, * ), C( LDC, * )
*     ..
*
*  Purpose
*  =======
*
*  STRMM  performs one of the matrix-matrix operations
*
*     C := alpha*op( A )*C,   or   C := alpha*C*op( A ),
*
*  where  alpha  is a scalar,  C  is an m by n matrix,  A  is a unit, or
*  non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
*
*     op( A ) = A   or   op( A ) = A'.
*
*  Parameters
*  ==========
*
*  SIDE   - CHARACTER*1.
*           On entry,  SIDE specifies whether  op( A ) multiplies C from
*           the left or right as follows:
*
*              SIDE = 'L' or 'l'   C := alpha*op( A )*C.
*
*              SIDE = 'R' or 'r'   C := alpha*C*op( A ).
*
*           Unchanged on exit.
*
*  UPLO   - CHARACTER*1.
*           On entry, UPLO specifies whether the matrix A is an upper or
*           lower triangular matrix as follows:
*
*              UPLO = 'U' or 'u'   A is an upper triangular matrix.
*
*              UPLO = 'L' or 'l'   A is a lower triangular matrix.
*
*           Unchanged on exit.
*
*  TRANSA - CHARACTER*1.
*           On entry, TRANSA specifies the form of op( A ) to be used in
*           the matrix multiplication as follows:
*
*              TRANSA = 'N' or 'n'   op( A ) = A.
*
*              TRANSA = 'T' or 't'   op( A ) = A'.
*
*              TRANSA = 'C' or 'c'   op( A ) = A'.
*
*           Unchanged on exit.
*
*  DIAG   - CHARACTER*1.
*           On entry, DIAG specifies whether or not A is unit triangular
*           as follows:
*
*              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
*
*              DIAG = 'N' or 'n'   A is not assumed to be unit
*                                  triangular.
*
*           Unchanged on exit.
*
*  M      - INTEGER.
*           On entry, M specifies the number of rows of C. M must be at
*           least zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry, N specifies the number of columns of C.  N must be
*           at least zero.
*           Unchanged on exit.
*
*  ALPHA  - REAL.
*           On entry,  ALPHA specifies the scalar  alpha. When  alpha is
*           zero then  A is not referenced and  C need not be set before
*           entry.
*           Unchanged on exit.
*
*  A      - REAL array of DIMENSION ( LDA, k ), where k is m
*           when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
*           Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
*           upper triangular part of the array  A must contain the upper
*           triangular matrix  and the strictly lower triangular part of
*           A is not referenced.
*           Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
*           lower triangular part of the array  A must contain the lower
*           triangular matrix  and the strictly upper triangular part of
*           A is not referenced.
*           Note that when  DIAG = 'U' or 'u',  the diagonal elements of
*           A  are not referenced either,  but are assumed to be  unity.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
*           LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
*           then LDA must be at least max( 1, n ).
*           Unchanged on exit.
*
*  C      - REAL array of DIMENSION ( LDC, n ).
*           Before entry,  the leading  m by n part of the array  C must
*           contain the matrix  C,  and  on exit  is overwritten  by the
*           transformed matrix.
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
*  -- Rewritten in December-1993.
*     GEMM-Based Level 3 BLAS.
*     Per Ling, Institute of Information Processing,
*     University of Umea, Sweden.
*
*
*     .. Local Scalars ..
      INTEGER            INFO, NROWA, OFFD
      LOGICAL            LSIDE, UPPER, NOTR, NOUNIT, CLDC, SMALLN,
     $                   TINYN, TINYM
      INTEGER            I, II, IX, ISEC, J, JJ, JX, JSEC, TSEC
      REAL               GAMMA, DELTA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN, MOD
*     .. External Functions ..
      LOGICAL            LSAME, SBIGP, SCLD
      EXTERNAL           LSAME, SBIGP, SCLD
*     .. External Subroutines ..
      EXTERNAL           XERBLA
      EXTERNAL           SGEMM, SGEMV, STRMV, SCOPY
*     .. Parameters ..
      REAL               ZERO, ONE
      INTEGER            SIP81, SIP82, SIP83
      PARAMETER        ( ZERO = 0.0E+0, ONE = 1.0E+0,
     $                   SIP81 = 81, SIP82 = 82, SIP83 = 83 )
*     .. User specified parameters for STRMM ..
      INTEGER            RB, CB, RCB
      PARAMETER        ( RCB = 64, RB = 64, CB = 64 )
      REAL               T1( RB, CB ), T2( CB, CB ), T3( RCB, RCB )
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      LSIDE = LSAME( SIDE, 'L' )
      UPPER = LSAME( UPLO, 'U' )
      NOTR = LSAME( TRANSA, 'N' )
      NOUNIT = LSAME( DIAG, 'N' )
      IF( NOUNIT )THEN
         OFFD = 0
      ELSE
         OFFD = 1
      END IF
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
      ELSE IF( ( .NOT.NOTR ).AND.( .NOT.LSAME( TRANSA, 'T' ) ).AND.
     $                               ( .NOT.LSAME( TRANSA, 'C' ) ) )THEN
         INFO = 3
      ELSE IF( ( .NOT.NOUNIT ).AND.( .NOT.LSAME( DIAG, 'U' ) ) )THEN
         INFO = 4
      ELSE IF( M.LT.0 )THEN
         INFO = 5
      ELSE IF( N.LT.0 )THEN
         INFO = 6
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 9
      ELSE IF( LDC.LT.MAX( 1, M ) )THEN
         INFO = 11
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'STRMM ', INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ) )
     $   RETURN
*
*     And when alpha.eq.zero.
*
      IF( ALPHA.EQ.ZERO )THEN
         CALL SGEMM ( 'N', 'N', M, N, 0, ZERO, C, MAX( LDA, LDC ), C,
     $                                   MAX( LDA, LDC ), ZERO, C, LDC )
         RETURN
      END IF
*
*     Start the operations.
*
      IF( LSIDE )THEN
         IF( UPPER )THEN
            IF( NOTR )THEN
*
*              Form  C := alpha*A*C. Left, Upper, No transpose.
*
               SMALLN = .NOT.SBIGP( SIP81, M, N )
               IF( SMALLN )THEN
                  TINYN = .NOT.SBIGP( SIP82, M, N )
                  DO 40, II = 1, M, RCB
                     ISEC = MIN( RCB, M-II+1 )
*
*                    C := alpha*C, scale the rectangular block of C
*                    with alpha.
*
                     IF( ALPHA.NE.ONE )
     $                  CALL SGEMM ( 'N', 'N', ISEC, N, 0, ZERO, A, LDA,
     $                                  C, LDC, ALPHA, C( II, 1 ), LDC )
                     IF( TINYN )THEN
*
*                       C := A*C, triangular matrix multiply involving
*                       a upper triangular diagonal block of A.
*
                        DO 10, J = 1, N
                           CALL STRMV ( 'U', 'N', DIAG, ISEC,
     $                                 A( II, II ), LDA, C( II, J ), 1 )
   10                   CONTINUE
                     ELSE
*
*                       T3 := A, a upper unit or non-unit triangular
*                       diagonal block of A is copied to the upper
*                       triangular part of T3.
*
                        DO 20, I = II+OFFD, II+ISEC-1
                           CALL SCOPY ( I-II+1-OFFD, A( II, I ), 1,
     $                                              T3( 1, I-II+1 ), 1 )
   20                   CONTINUE
*
*                       C := T3*C, triangular matrix multiply involving
*                       a upper triangular diagonal block of A stored
*                       in T3.
*
                        DO 30, J = 1, N
                           CALL STRMV ( 'U', 'N', DIAG, ISEC,
     $                                  T3( 1, 1 ), RCB, C( II, J ), 1 )
   30                   CONTINUE
                     END IF
*
*                    C := alpha*A*C + C, general matrix multiply
*                    involving a rectangular block of A.
*
                     IF( II+ISEC.LE.M )THEN
                        CALL SGEMM ( 'N', 'N', ISEC, N, M-II-ISEC+1,
     $                                     ALPHA, A( II, II+ISEC ), LDA,
     $                                        C( II+ISEC, 1 ), LDC, ONE,
     $                                                 C( II, 1 ), LDC )
                     END IF
   40             CONTINUE
               ELSE
                  DELTA = ALPHA
                  CLDC = SCLD( LDC )
                  DO 110, II = 1, M, CB
                     ISEC = MIN( CB, M-II+1 )
*
*                    T2 := A', the transpose of a upper unit or non-unit
*                    triangular diagonal block of A is copied to the
*                    lower triangular part of T2.
*
                     DO 50, I = II+OFFD, II+ISEC-1
                        CALL SCOPY ( I-II+1-OFFD, A( II, I ), 1,
     $                                             T2( I-II+1, 1 ), CB )
   50                CONTINUE
                     DO 100, JJ = 1, N, RB
                        JSEC = MIN( RB, N-JJ+1 )
*
*                       T1 := C', the transpose of a rectangular block
*                       of C is copied to T1.
*
                        IF( CLDC )THEN
                           DO 60, J = JJ, JJ+JSEC-1
                              CALL SCOPY ( ISEC, C( II, J ), 1,
     $                                             T1( J-JJ+1, 1 ), RB )
   60                      CONTINUE
                        ELSE
                           DO 70, I = II, II+ISEC-1
                              CALL SCOPY ( JSEC, C( I, JJ ), LDC,
     $                                              T1( 1, I-II+1 ), 1 )
   70                      CONTINUE
                        END IF
*
*                       T1 := gamma*T1*T2 + delta*T1, triangular matrix
*                       multiply where the value of delta depends on
*                       whether T2 stores a unit or non-unit triangular
*                       block. Gamma and tsec are used to compensate for
*                       a deficiency in SGEMV that appears if the second
*                       dimension (tsec) is zero.
*
                        DO 80, I = II, II+ISEC-1
                           IF( NOUNIT )THEN
                              DELTA = ALPHA*T2( I-II+1, I-II+1 )
                           END IF
                           GAMMA = ALPHA
                           TSEC = II+ISEC-1-I
                           IF( TSEC.EQ.0 )THEN
                              TSEC = 1
                              GAMMA = ZERO
                           END IF
                           CALL SGEMV ( 'N', JSEC, TSEC, GAMMA,
     $                        T1( 1, I-II+2 ), RB, T2( I-II+2, I-II+1 ),
     $                                    1, DELTA, T1( 1, I-II+1 ), 1 )
   80                   CONTINUE
*
*                       C := T1', the transpose of T1 is copied back
*                       to C.
*
                        DO 90, J = JJ, JJ+JSEC-1
                           CALL SCOPY ( ISEC, T1( J-JJ+1, 1 ), RB,
     $                                                   C( II, J ), 1 )
   90                   CONTINUE
  100                CONTINUE
*
*                    C := alpha*A*C + C, general matrix multiply
*                    involving a rectangular block of A.
*
                     IF( II+ISEC.LE.M )THEN
                        CALL SGEMM ( 'N', 'N', ISEC, N, M-II-ISEC+1,
     $                                     ALPHA, A( II, II+ISEC ), LDA,
     $                                        C( II+ISEC, 1 ), LDC, ONE,
     $                                                 C( II, 1 ), LDC )
                     END IF
  110             CONTINUE
               END IF
            ELSE
*
*              Form  C := alpha*A'*C. Left, Upper, Transpose.
*
               SMALLN = .NOT.SBIGP( SIP81, M, N )
               IF( SMALLN )THEN
                  TINYN = .NOT.SBIGP( SIP82, M, N )
                  DO 150, II = M-MOD( M-1, RCB ), 1, -RCB
                     ISEC = MIN( RCB, M-II+1 )
*
*                    C := alpha*C, scale the rectangular block of C
*                    with alpha.
*
                     IF( ALPHA.NE.ONE )
     $                  CALL SGEMM ( 'T', 'N', ISEC, N, 0, ZERO, A, LDA,
     $                                  C, LDC, ALPHA, C( II, 1 ), LDC )
                     IF( TINYN )THEN
*
*                       C := A'*C, triangular matrix multiply involving
*                       a upper triangular diagonal block of A.
*
                        DO 120, J = 1, N
                           CALL STRMV ( 'U', 'T', DIAG, ISEC,
     $                                 A( II, II ), LDA, C( II, J ), 1 )
  120                   CONTINUE
                     ELSE
*
*                       T3 :=  A', the transpose of a upper unit or
*                       non-unit triangular diagonal block of A is
*                       copied to the lower triangular part of T3.
*
                        DO 130, I = II+OFFD, II+ISEC-1
                           CALL SCOPY ( I-II+1-OFFD, A( II, I ), 1,
     $                                            T3( I-II+1, 1 ), RCB )
  130                   CONTINUE
*
*                       C := T3*C, triangular matrix multiply involving
*                       the transpose of a upper triangular diagonal
*                       block of A stored in T3.
*
                        DO 140, J = 1, N
                           CALL STRMV ( 'L', 'N', DIAG, ISEC,
     $                                  T3( 1, 1 ), RCB, C( II, J ), 1 )
  140                   CONTINUE
                     END IF
*
*                    C := alpha*A'*C + C, general matrix multiply
*                    involving the transpose of a rectangular block
*                    of A.
*
                     IF( II.GT.1 )THEN
                        CALL SGEMM ( 'T', 'N', ISEC, N, II-1, ALPHA,
     $                                  A( 1, II ), LDA, C( 1, 1 ), LDC,
     $                                            ONE, C( II, 1 ), LDC )
                     END IF
  150             CONTINUE
               ELSE
                  DELTA = ALPHA
                  CLDC = SCLD( LDC )
                  DO 210, II = M-MOD( M-1, CB ), 1, -CB
                     ISEC = MIN( CB, M-II+1 )
                     DO 200, JJ = 1, N, RB
                        JSEC = MIN( RB, N-JJ+1 )
*
*                       T1 := C', the transpose of a rectangular block
*                       of C is copied to T1.
*
                        IF( CLDC )THEN
                           DO 160, J = JJ, JJ+JSEC-1
                              CALL SCOPY ( ISEC, C( II, J ), 1,
     $                                             T1( J-JJ+1, 1 ), RB )
  160                      CONTINUE
                        ELSE
                           DO 170, I = II, II+ISEC-1
                              CALL SCOPY ( JSEC, C( I, JJ ), LDC,
     $                                              T1( 1, I-II+1 ), 1 )
  170                      CONTINUE
                        END IF
*
*                       T1 := gamma*T1*A + delta*T1, triangular matrix
*                       multiply where the value of delta depends on
*                       whether A is a unit or non-unit triangular
*                       matrix. Gamma and tsec are used to compensate
*                       for a deficiency in SGEMV that appears if the
*                       second dimension (tsec) is zero.
*
                        DO 180, I = II+ISEC-1, II, -1
                           IF( NOUNIT )THEN
                              DELTA = ALPHA*A( I, I )
                           END IF
                           GAMMA = ALPHA
                           TSEC = I-II
                           IF( TSEC.EQ.0 )THEN
                              TSEC = 1
                              GAMMA = ZERO
                           END IF
                           CALL SGEMV ( 'N', JSEC, TSEC, GAMMA,
     $                                    T1( 1, 1 ), RB, A( II, I ), 1,
     $                                       DELTA, T1( 1, I-II+1 ), 1 )
  180                   CONTINUE
*
*                       C := T1', the transpose of T1 is copied back
*                       to C.
*
                        DO 190, J = JJ, JJ+JSEC-1
                           CALL SCOPY ( ISEC, T1( J-JJ+1, 1 ), RB,
     $                                                   C( II, J ), 1 )
  190                   CONTINUE
  200                CONTINUE
*
*                    C := alpha*A'*C + C, general matrix multiply
*                    involving the transpose of a rectangular block
*                    of A.
*
                     IF( II.GT.1 )THEN
                        CALL SGEMM ( 'T', 'N', ISEC, N, II-1, ALPHA,
     $                                  A( 1, II ), LDA, C( 1, 1 ), LDC,
     $                                            ONE, C( II, 1 ), LDC )
                     END IF
  210             CONTINUE
               END IF
            END IF
         ELSE
            IF( NOTR )THEN
*
*              Form  C := alpha*A*C. Left, Lower, No transpose.
*
               SMALLN = .NOT.SBIGP( SIP81, M, N )
               IF( SMALLN )THEN
                  TINYN = .NOT.SBIGP( SIP82, M, N )
                  DO 250, IX = M, 1, -RCB
                     II = MAX( 1, IX-RCB+1 )
                     ISEC = IX-II+1
*
*                    C := alpha*C, scale the rectangular block of C
*                    with alpha.
*
                     IF( ALPHA.NE.ONE )
     $                  CALL SGEMM ( 'N', 'N', ISEC, N, 0, ZERO, A, LDA,
     $                                  C, LDC, ALPHA, C( II, 1 ), LDC )
                     IF( TINYN )THEN
*
*                       C := A*C, triangular matrix multiply involving
*                       a lower triangular diagonal block of A.
*
                        DO 220, J = 1, N
                           CALL STRMV ( 'L', 'N', DIAG, ISEC,
     $                                 A( II, II ), LDA, C( II, J ), 1 )
  220                   CONTINUE
                     ELSE
*
*                       T3 := A, a lower unit or non-unit triangular
*                       diagonal block of A is copied to the lower
*                       triangular part of T3.
*
                        DO 230, I = II, II+ISEC-1-OFFD
                           CALL SCOPY ( II+ISEC-I-OFFD, A( I+OFFD, I ),
     $                                 1, T3( I-II+1+OFFD, I-II+1 ), 1 )
  230                   CONTINUE
*
*                       C := T3*C, triangular matrix multiply involving
*                       a lower triangular diagonal block of A stored
*                       in T3.
*
                        DO 240, J = 1, N
                           CALL STRMV ( 'L', 'N', DIAG, ISEC,
     $                                  T3( 1, 1 ), RCB, C( II, J ), 1 )
  240                   CONTINUE
                     END IF
*
*                    C := alpha*A'*C + C, general matrix multiply
*                    involving a rectangular block of A.
*
                     IF( II.GT.1 )THEN
                        CALL SGEMM ( 'N', 'N', ISEC, N, II-1, ALPHA,
     $                                  A( II, 1 ), LDA, C( 1, 1 ), LDC,
     $                                            ONE, C( II, 1 ), LDC )
                     END IF
  250             CONTINUE
               ELSE
                  DELTA = ALPHA
                  CLDC = SCLD( LDC )
                  DO 320, IX = M, 1, -CB
                     II = MAX( 1, IX-CB+1 )
                     ISEC = IX-II+1
*
*                    T2 := A', the transpose of a lower unit or non-unit
*                    triangular diagonal block of A is copied to the
*                    upper triangular part of T2.
*
                     DO 260, I = II, II+ISEC-1-OFFD
                        CALL SCOPY ( II+ISEC-I-OFFD, A( I+OFFD, I ),
     $                                1, T2( I-II+1, I-II+1+OFFD ), CB )
  260                CONTINUE
                     DO 310, JJ = 1, N, RB
                        JSEC = MIN( RB, N-JJ+1 )
*
*                       T1 := C', the transpose of a rectangular block
*                       of C is copied to T1.
*
                        IF( CLDC )THEN
                           DO 270, J = JJ, JJ+JSEC-1
                              CALL SCOPY ( ISEC, C( II, J ), 1,
     $                                             T1( J-JJ+1, 1 ), RB )
  270                      CONTINUE
                        ELSE
                           DO 280, I = II, II+ISEC-1
                              CALL SCOPY ( JSEC, C( I, JJ ), LDC,
     $                                              T1( 1, I-II+1 ), 1 )
  280                      CONTINUE
                        END IF
*
*                       T1 := gamma*T1*T2 + delta*T1, triangular matrix
*                       multiply where the value of delta depends on
*                       whether T2 stores a unit or non-unit triangular
*                       block. Gamma and tsec are used to compensate for
*                       a deficiency in SGEMV that appears if the second
*                       dimension (tsec) is zero.
*
                        DO 290, I = II+ISEC-1, II, -1
                           IF( NOUNIT )THEN
                              DELTA = ALPHA*T2( I-II+1, I-II+1 )
                           END IF
                           GAMMA = ALPHA
                           TSEC = I-II
                           IF( TSEC.EQ.0 )THEN
                              TSEC = 1
                              GAMMA = ZERO
                           END IF
                           CALL SGEMV ( 'N', JSEC, TSEC, GAMMA,
     $                               T1( 1, 1 ), RB, T2( 1, I-II+1 ), 1,
     $                                       DELTA, T1( 1, I-II+1 ), 1 )
  290                   CONTINUE
*
*                       C := T1', the transpose of T1 is copied back
*                       to C.
*
                        DO 300, J = JJ, JJ+JSEC-1
                           CALL SCOPY ( ISEC, T1( J-JJ+1, 1 ), RB,
     $                                                   C( II, J ), 1 )
  300                   CONTINUE
  310                CONTINUE
*
*                    C := alpha*A'*C + C, general matrix multiply
*                    involving a rectangular block of A.
*
                     IF( II.GT.1 )THEN
                        CALL SGEMM ( 'N', 'N', ISEC, N, II-1, ALPHA,
     $                                  A( II, 1 ), LDA, C( 1, 1 ), LDC,
     $                                            ONE, C( II, 1 ), LDC )
                     END IF
  320             CONTINUE
               END IF
            ELSE
*
*              Form  C := alpha*A'*C. Left, Lower, Transpose.
*
               SMALLN = .NOT.SBIGP( SIP81, M, N )
               IF( SMALLN )THEN
                  TINYN = .NOT.SBIGP( SIP82, M, N )
                  DO 360, IX = MOD( M-1, RCB )+1, M, RCB
                     II = MAX( 1, IX-RCB+1 )
                     ISEC = IX-II+1
*
*                    C := alpha*C, scale the rectangular block of C
*                    with alpha.
*
                     IF( ALPHA.NE.ONE )
     $                  CALL SGEMM ( 'T', 'N', ISEC, N, 0, ZERO, A, LDA,
     $                                  C, LDC, ALPHA, C( II, 1 ), LDC )
                     IF( TINYN )THEN
*
*                       C := A'*C, triangular matrix multiply involving
*                       a lower triangular diagonal block of A.
*
                        DO 330, J = 1, N
                           CALL STRMV ( 'L', 'T', DIAG, ISEC,
     $                                 A( II, II ), LDA, C( II, J ), 1 )
  330                   CONTINUE
                     ELSE
*
*                       T3 :=  A', the transpose of a lower unit or
*                       non-unit triangular diagonal block of A is
*                       copied to the upper triangular part of T3.
*
                        DO 340, I = II, II+ISEC-1-OFFD
                           CALL SCOPY ( II+ISEC-I-OFFD, A( I+OFFD, I ),
     $                               1, T3( I-II+1, I-II+1+OFFD ), RCB )
  340                   CONTINUE
*
*                       C := alpha*T3*C, triangular matrix multiply
*                       involving the transpose of a lower triangular
*                       diagonal block of A stored in T3.
*
                        DO 350, J = 1, N
                           CALL STRMV ( 'U', 'N', DIAG, ISEC,
     $                                  T3( 1, 1 ), RCB, C( II, J ), 1 )
  350                   CONTINUE
                     END IF
*
*                    C := alpha*A'*C + C, general matrix multiply
*                    involving the transpose of a rectangular block
*                    of A.
*
                     IF( II+ISEC.LE.M )THEN
                        CALL SGEMM ( 'T', 'N', ISEC, N, M-II-ISEC+1,
     $                                     ALPHA, A( II+ISEC, II ), LDA,
     $                                        C( II+ISEC, 1 ), LDC, ONE,
     $                                                 C( II, 1 ), LDC )
                     END IF
  360             CONTINUE
               ELSE
                  DELTA = ALPHA
                  CLDC = SCLD( LDC )
                  DO 420, IX = MOD( M-1, CB )+1, M, CB
                     II = MAX( 1, IX-CB+1 )
                     ISEC = IX-II+1
                     DO 410, JJ = 1, N, RB
                        JSEC = MIN( RB, N-JJ+1 )
*
*                       T1 := C', the transpose of a rectangular block
*                       of C is copied to T1.
*
                        IF( CLDC )THEN
                           DO 370, J = JJ, JJ+JSEC-1
                              CALL SCOPY ( ISEC, C( II, J ), 1,
     $                                             T1( J-JJ+1, 1 ), RB )
  370                      CONTINUE
                        ELSE
                           DO 380, I = II, II+ISEC-1
                              CALL SCOPY ( JSEC, C( I, JJ ), LDC,
     $                                              T1( 1, I-II+1 ), 1 )
  380                      CONTINUE
                        END IF
*
*                       T1 := gamma*T1*A + delta*T1, triangular matrix
*                       multiply where the value of delta depends on
*                       whether A is a unit or non-unit triangular
*                       matrix. Gamma and tsec are used to compensate
*                       for a deficiency in SGEMV that appears if the
*                       second dimension (tsec) is zero.
*
                        DO 390, I = II, II+ISEC-1
                           IF( NOUNIT )THEN
                              DELTA = ALPHA*A( I, I )
                           END IF
                           GAMMA = ALPHA
                           TSEC = II+ISEC-1-I
                           IF( TSEC.EQ.0 )THEN
                              TSEC = 1
                              GAMMA = ZERO
                           END IF
                           CALL SGEMV ( 'N', JSEC, TSEC, GAMMA,
     $                              T1( 1, I-II+2 ), RB, A( I+1, I ), 1,
     $                                       DELTA, T1( 1, I-II+1 ), 1 )
  390                   CONTINUE
*
*                       C := T1', the transpose of T1 is copied back
*                       to C.
*
                        DO 400, J = JJ, JJ+JSEC-1
                           CALL SCOPY ( ISEC, T1( J-JJ+1, 1 ), RB,
     $                                                   C( II, J ), 1 )
  400                   CONTINUE
  410                CONTINUE
*
*                    C := alpha*A'*C + C, general matrix multiply
*                    involving the transpose of a rectangular block
*                    of A.
*
                     IF( II+ISEC.LE.M )THEN
                        CALL SGEMM ( 'T', 'N', ISEC, N, M-II-ISEC+1,
     $                                     ALPHA, A( II+ISEC, II ), LDA,
     $                                        C( II+ISEC, 1 ), LDC, ONE,
     $                                                 C( II, 1 ), LDC )
                     END IF
  420             CONTINUE
               END IF
            END IF
         END IF
      ELSE
         IF( UPPER )THEN
            IF( NOTR )THEN
*
*              Form  C := alpha*C*A. Right, Upper, No transpose.
*
               TINYM = .NOT.SBIGP( SIP83, M, N )
               IF( TINYM )THEN
                  DO 440, JJ = N-MOD( N-1, RCB ), 1, -RCB
                     JSEC = MIN( RCB, N-JJ+1 )
*
*                    C := alpha*C, scale the rectangular block of C
*                    with alpha.
*
                     IF( ALPHA.NE.ONE )
     $                  CALL SGEMM ( 'N', 'N', M, JSEC, 0, ZERO, C, LDC,
     $                                  A, LDA, ALPHA, C( 1, JJ ), LDC )
*
*                    C := C*A, triangular matrix multiply involving a
*                    upper triangular diagonal block of A.
*
                     DO 430, I = 1, M
                        CALL STRMV ( 'U', 'T', DIAG, JSEC,
     $                               A( JJ, JJ ), LDA, C( I, JJ ), LDC )
  430                CONTINUE
*
*                    C := alpha*C*A + C, general matrix multiply
*                    involving a rectangular block of A.
*
                     IF( JJ.GT.1 )THEN
                        CALL SGEMM ( 'N', 'N', M, JSEC, JJ-1, ALPHA,
     $                                  C( 1, 1 ), LDC, A( 1, JJ ), LDA,
     $                                            ONE, C( 1, JJ ), LDC )
                     END IF
  440             CONTINUE
               ELSE
                  DELTA = ALPHA
                  DO 480, JJ = N-MOD( N-1, CB ), 1, -CB
                     JSEC = MIN( CB, N-JJ+1 )
                     DO 470, II = 1, M, RB
                        ISEC = MIN( RB, M-II+1 )
*
*                       T1 := C, a rectangular block of C is copied
*                       to T1.
*
                        DO 450, J = JJ, JJ+JSEC-1
                           CALL SCOPY ( ISEC, C( II, J ), 1,
     $                                              T1( 1, J-JJ+1 ), 1 )
  450                   CONTINUE
*
*                       C := gamma*T1*A + delta*C, triangular matrix
*                       multiply where the value of delta depends on
*                       whether A is a unit or non-unit triangular
*                       matrix. Gamma and tsec are used to compensate
*                       for a deficiency in SGEMV that appears if the
*                       second dimension (tsec) is zero.
*
                        DO 460, J = JJ+JSEC-1, JJ, -1
                           IF( NOUNIT )THEN
                              DELTA = ALPHA*A( J, J )
                           END IF
                           GAMMA = ALPHA
                           TSEC = J-JJ
                           IF( TSEC.EQ.0 )THEN
                              TSEC = 1
                              GAMMA = ZERO
                           END IF
                           CALL SGEMV ( 'N', ISEC, TSEC, GAMMA,
     $                                    T1( 1, 1 ), RB, A( JJ, J ), 1,
     $                                            DELTA, C( II, J ), 1 )
  460                   CONTINUE
  470                CONTINUE
*
*                    C := alpha*C*A + C, general matrix multiply
*                    involving a rectangular block of A.
*
                     IF( JJ.GT.1 )THEN
                        CALL SGEMM ( 'N', 'N', M, JSEC, JJ-1, ALPHA,
     $                                  C( 1, 1 ), LDC, A( 1, JJ ), LDA,
     $                                            ONE, C( 1, JJ ), LDC )
                     END IF
  480             CONTINUE
               END IF
            ELSE
*
*              Form  C := alpha*C*A'. Right, Upper, Transpose.
*
               TINYM = .NOT.SBIGP( SIP83, M, N )
               IF( TINYM )THEN
                  DO 500, JJ = 1, N, RCB
                     JSEC = MIN( RCB, N-JJ+1 )
*
*                    C := alpha*C, scale the rectangular block of C
*                    with alpha.
*
                     IF( ALPHA.NE.ONE )
     $                  CALL SGEMM ( 'N', 'T', M, JSEC, 0, ZERO, C, LDC,
     $                                  A, LDA, ALPHA, C( 1, JJ ), LDC )
*
*                    C := C*A', triangular matrix multiply involving a
*                    upper triangular diagonal block of A.
*
                     DO 490, I = 1, M
                        CALL STRMV ( 'U', 'N', DIAG, JSEC,
     $                               A( JJ, JJ ), LDA, C( I, JJ ), LDC )
  490                CONTINUE
*
*                    C := alpha*C*A' + C, general matrix multiply
*                    involving the transpose of a rectangular block
*                    of A.
*
                     IF( JJ+JSEC.LE.N )THEN
                        CALL SGEMM ( 'N', 'T', M, JSEC, N-JJ-JSEC+1,
     $                                      ALPHA, C( 1, JJ+JSEC ), LDC,
     $                                       A( JJ, JJ+JSEC ), LDA, ONE,
     $                                                 C( 1, JJ ), LDC )
                     END IF
  500             CONTINUE
               ELSE
                  DELTA = ALPHA
                  DO 550, JJ = 1, N, CB
                     JSEC = MIN( CB, N-JJ+1 )
*
*                    T2 := A', the transpose of a upper unit or non-unit
*                    triangular diagonal block of A is copied to the
*                    lower triangular part of T2.
*
                     DO 510, J = JJ+OFFD, JJ+JSEC-1
                        CALL SCOPY ( J-JJ+1-OFFD, A( JJ, J ), 1,
     $                                             T2( J-JJ+1, 1 ), CB )
  510                CONTINUE
                     DO 540, II = 1, M, RB
                        ISEC = MIN( RB, M-II+1 )
*
*                       T1 := C, a rectangular block of C is copied
*                       to T1.
*
                        DO 520, J = JJ, JJ+JSEC-1
                           CALL SCOPY ( ISEC, C( II, J ), 1,
     $                                              T1( 1, J-JJ+1 ), 1 )
  520                   CONTINUE
*
*                       C := gamma*T1*T2 + delta*C, triangular matrix
*                       multiply where the value of delta depends on
*                       whether T2 is a unit or non-unit triangular
*                       matrix. Gamma and tsec are used to compensate
*                       for a deficiency in SGEMV that appears if the
*                       second dimension (tsec) is zero.
*
                        DO 530, J = JJ, JJ+JSEC-1
                           IF( NOUNIT )THEN
                              DELTA = ALPHA*T2( J-JJ+1, J-JJ+1 )
                           END IF
                           GAMMA = ALPHA
                           TSEC = JJ+JSEC-1-J
                           IF( TSEC.EQ.0 )THEN
                              TSEC = 1
                              GAMMA = ZERO
                           END IF
                           CALL SGEMV ( 'N', ISEC, TSEC, GAMMA,
     $                        T1( 1, J-JJ+2 ), RB, T2( J-JJ+2, J-JJ+1 ),
     $                                         1, DELTA, C( II, J ), 1 )
  530                   CONTINUE
  540                CONTINUE
*
*                    C := alpha*C*A' + C, general matrix multiply
*                    involving the transpose of a rectangular block
*                    of A.
*
                     IF( JJ+JSEC.LE.N )THEN
                        CALL SGEMM ( 'N', 'T', M, JSEC, N-JJ-JSEC+1,
     $                                      ALPHA, C( 1, JJ+JSEC ), LDC,
     $                                       A( JJ, JJ+JSEC ), LDA, ONE,
     $                                                 C( 1, JJ ), LDC )
                     END IF
  550             CONTINUE
               END IF
            END IF
         ELSE
            IF( NOTR )THEN
*
*              Form  C := alpha*C*A. Right, Lower, No transpose.
*
               TINYM = .NOT.SBIGP( SIP83, M, N )
               IF( TINYM )THEN
                  DO 570, JX = MOD( N-1, RCB )+1, N, RCB
                     JJ = MAX( 1, JX-RCB+1 )
                     JSEC = JX-JJ+1
*
*                    C := alpha*C, scale the rectangular block of C
*                    with alpha.
*
                     IF( ALPHA.NE.ONE )
     $                  CALL SGEMM ( 'N', 'N', M, JSEC, 0, ZERO, C, LDC,
     $                                  A, LDA, ALPHA, C( 1, JJ ), LDC )
*
*                    C := C*A, triangular matrix multiply involving a
*                    lower triangular diagonal block of A.
*
                     DO 560, I = 1, M
                        CALL STRMV ( 'L', 'T', DIAG, JSEC,
     $                               A( JJ, JJ ), LDA, C( I, JJ ), LDC )
  560                CONTINUE
*
*                    C := alpha*C*A + C, general matrix multiply
*                    involving a rectangular block of A.
*
                     IF( JJ+JSEC.LE.N )THEN
                        CALL SGEMM ( 'N', 'N', M, JSEC, N-JJ-JSEC+1,
     $                                      ALPHA, C( 1, JJ+JSEC ), LDC,
     $                                       A( JJ+JSEC, JJ ), LDA, ONE,
     $                                                 C( 1, JJ ), LDC )
                     END IF
  570             CONTINUE
               ELSE
                  DELTA = ALPHA
                  DO 610, JX = MOD( N-1, CB )+1, N, CB
                     JJ = MAX( 1, JX-CB+1 )
                     JSEC = JX-JJ+1
                     DO 600, II = 1, M, RB
                        ISEC = MIN( RB, M-II+1 )
*
*                       T1 := C, a rectangular block of C is copied
*                       to T1.
*
                        DO 580, J = JJ, JJ+JSEC-1
                           CALL SCOPY ( ISEC, C( II, J ), 1,
     $                                              T1( 1, J-JJ+1 ), 1 )
  580                   CONTINUE
*
*                       C := gamma*T1*A + delta*C, triangular matrix
*                       multiply where the value of delta depends on
*                       whether A is a unit or non-unit triangular
*                       matrix. Gamma and tsec are used to compensate
*                       for a deficiency in SGEMV that appears if the
*                       second dimension (tsec) is zero.
*
                        DO 590, J = JJ, JJ+JSEC-1
                           IF( NOUNIT )THEN
                              DELTA = ALPHA*A( J, J )
                           END IF
                           GAMMA = ALPHA
                           TSEC = JJ+JSEC-1-J
                           IF( TSEC.EQ.0 )THEN
                              TSEC = 1
                              GAMMA = ZERO
                           END IF
                           CALL SGEMV ( 'N', ISEC, TSEC, GAMMA,
     $                              T1( 1, J-JJ+2 ), RB, A( J+1, J ), 1,
     $                                            DELTA, C( II, J ), 1 )
  590                   CONTINUE
  600                CONTINUE
*
*                    C := alpha*C*A + C, general matrix multiply
*                    involving a rectangular block of A.
*
                     IF( JJ+JSEC.LE.N )THEN
                        CALL SGEMM ( 'N', 'N', M, JSEC, N-JJ-JSEC+1,
     $                                      ALPHA, C( 1, JJ+JSEC ), LDC,
     $                                       A( JJ+JSEC, JJ ), LDA, ONE,
     $                                                 C( 1, JJ ), LDC )
                     END IF
  610             CONTINUE
               END IF
            ELSE
*
*              Form  C := alpha*C*A'. Right, Lower, Transpose.
*
               TINYM = .NOT.SBIGP( SIP83, M, N )
               IF( TINYM )THEN
                  DO 630, JX = N, 1, -RCB
                     JJ = MAX( 1, JX-RCB+1 )
                     JSEC = JX-JJ+1
*
*                    C := alpha*C, scale the rectangular block of C
*                    with alpha.
*
                     IF( ALPHA.NE.ONE )
     $                  CALL SGEMM ( 'N', 'T', M, JSEC, 0, ZERO, C, LDC,
     $                                  A, LDA, ALPHA, C( 1, JJ ), LDC )
*
*                    C := C*A', triangular matrix multiply involving
*                    a lower triangular diagonal block of A.
*
                     DO 620, I = 1, M
                        CALL STRMV ( 'L', 'N', DIAG, JSEC,
     $                               A( JJ, JJ ), LDA, C( I, JJ ), LDC )
  620                CONTINUE
*
*                    C := alpha*C*A' + C, general matrix multiply
*                    involving the transpose of a rectangular block
*                    of A.
*
                     IF( JJ.GT.1 )THEN
                        CALL SGEMM ( 'N', 'T', M, JSEC, JJ-1, ALPHA,
     $                                  C( 1, 1 ), LDC, A( JJ, 1 ), LDA,
     $                                            ONE, C( 1, JJ ), LDC )
                     END IF
  630             CONTINUE
               ELSE
                  DELTA = ALPHA
                  DO 680, JX = N, 1, -CB
                     JJ = MAX( 1, JX-CB+1 )
                     JSEC = JX-JJ+1
*
*                    T2 := A', the transpose of a lower unit or non-unit
*                    triangular diagonal block of A is copied to the
*                    upper triangular part of T2.
*
                     DO 640, J = JJ, JJ+JSEC-1-OFFD
                        CALL SCOPY ( JJ+JSEC-J-OFFD, A( J+OFFD, J ),
     $                                1, T2( J-JJ+1, J-JJ+1+OFFD ), CB )
  640                CONTINUE
                     DO 670, II = 1, M, RB
                        ISEC = MIN( RB, M-II+1 )
*
*                       T1 := C, a rectangular block of C is copied
*                       to T1.
*
                        DO 650, J = JJ, JJ+JSEC-1
                           CALL SCOPY ( ISEC, C( II, J ), 1,
     $                                              T1( 1, J-JJ+1 ), 1 )
  650                   CONTINUE
*
*                       C := gamma*T1*T2 + delta*C, triangular matrix
*                       multiply where the value of delta depends on
*                       whether T2 is a unit or non-unit triangular
*                       matrix. Gamma and tsec are used to compensate
*                       for a deficiency in SGEMV that appears if the
*                       second dimension (tsec) is zero.
*
                        DO 660, J = JJ+JSEC-1, JJ, -1
                           IF( NOUNIT )THEN
                              DELTA = ALPHA*T2( J-JJ+1, J-JJ+1 )
                           END IF
                           GAMMA = ALPHA
                           TSEC = J-JJ
                           IF( TSEC.EQ.0 )THEN
                              TSEC = 1
                              GAMMA = ZERO
                           END IF
                           CALL SGEMV ( 'N', ISEC, TSEC, GAMMA,
     $                               T1( 1, 1 ), RB, T2( 1, J-JJ+1 ), 1,
     $                                            DELTA, C( II, J ), 1 )
  660                   CONTINUE
  670                CONTINUE
*
*                    C := alpha*C*A' + C, general matrix multiply
*                    involving the transpose of a rectangular block
*                    of A.
*
                     IF( JJ.GT.1 )THEN
                        CALL SGEMM ( 'N', 'T', M, JSEC, JJ-1, ALPHA,
     $                                  C( 1, 1 ), LDC, A( JJ, 1 ), LDA,
     $                                            ONE, C( 1, JJ ), LDC )
                     END IF
  680             CONTINUE
               END IF
            END IF
         END IF
      END IF
*
      RETURN
*
*     End of STRMM.
*
      END
