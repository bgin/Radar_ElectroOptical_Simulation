      SUBROUTINE CTRSM ( SIDE, UPLO, TRANSA, DIAG, M, N, ALPHA, A, LDA,
     $                   C, LDC )
*     .. Scalar Arguments ..
      CHARACTER*1        SIDE, UPLO, TRANSA, DIAG
      INTEGER            M, N, LDA, LDC
      COMPLEX            ALPHA
*     .. Array Arguments ..
      COMPLEX            A( LDA, * ), C( LDC, * )
*     ..
*
*  Purpose
*  =======
*
*  CTRSM  solves one of the matrix equations
*
*     op( A )*X = alpha*C,   or   X*op( A ) = alpha*C,
*
*  where alpha is a scalar, X and C are m by n matrices, A is a unit, or
*  non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
*
*     op( A ) = A   or   op( A ) = A'   or   op( A ) = conjg( A' ).
*
*  The matrix X is overwritten on C.
*
*  Parameters
*  ==========
*
*  SIDE   - CHARACTER*1.
*           On entry, SIDE specifies whether op( A ) appears on the left
*           or right of X as follows:
*
*              SIDE = 'L' or 'l'   op( A )*X = alpha*C.
*
*              SIDE = 'R' or 'r'   X*op( A ) = alpha*C.
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
*              TRANSA = 'C' or 'c'   op( A ) = conjg( A' ).
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
*  ALPHA  - COMPLEX      .
*           On entry,  ALPHA specifies the scalar  alpha. When  alpha is
*           zero then  A is not referenced and  C need not be set before
*           entry.
*           Unchanged on exit.
*
*  A      - COMPLEX       array of DIMENSION ( LDA, k ), where k is m
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
*  C      - COMPLEX       array of DIMENSION ( LDC, n ).
*           Before entry,  the leading  m by n part of the array  C must
*           contain  the  right-hand  side  matrix  C,  and  on exit  is
*           overwritten by the solution matrix  X.
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
      INTEGER            INFO, NROWA, OFFD
      LOGICAL            LSIDE, UPPER, NOTR, NOCONJ, NOUNIT,
     $                   CLDC, SMALLN, TINYN, TINYM
      INTEGER            I, II, IX, ISEC, J, JJ, JX, JSEC, TSEC
      COMPLEX            GAMMA, DELTA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN, MOD, CONJG
*     .. External Functions ..
      LOGICAL            LSAME, CBIGP, CCLD
      EXTERNAL           LSAME, CBIGP, CCLD
*     .. External Subroutines ..
      EXTERNAL           XERBLA
      EXTERNAL           CGEMM, CGEMV, CTRSV, CCOPY
*     .. Parameters ..
      COMPLEX            ZERO, ONE
      INTEGER            CIP91, CIP92, CIP93
      PARAMETER        ( ZERO = ( 0.0E+0, 0.0E+0 ),
     $                   ONE = ( 1.0E+0, 0.0E+0 ),
     $                   CIP91 = 91, CIP92 = 92, CIP93 = 93 )
*     .. User specified parameters for CTRSM ..
      INTEGER            RB, CB, RCB
      PARAMETER        ( RCB = 64, RB = 64, CB = 64 )
      COMPLEX            T1( RB, CB ), T2( CB, CB ), T3( RCB, RCB )
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      LSIDE = LSAME( SIDE, 'L' )
      UPPER = LSAME( UPLO, 'U' )
      NOTR = LSAME( TRANSA, 'N' )
      NOCONJ = LSAME( TRANSA, 'T' )
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
      INFO   = 0
      IF( ( .NOT.LSIDE ).AND.( .NOT.LSAME( SIDE, 'R' ) ) )THEN
         INFO = 1
      ELSE IF( ( .NOT.UPPER ).AND.( .NOT.LSAME( UPLO, 'L' ) ) )THEN
         INFO = 2
      ELSE IF( ( ( .NOT.NOTR ).AND.( .NOT.NOCONJ ) ).AND.
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
         CALL XERBLA( 'CTRSM ', INFO )
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
         CALL CGEMM ( 'N', 'N', M, N, 0, ZERO, C, MAX( LDA, LDC ), C,
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
*              Solve  A*X = alpha*C. Left, Upper, No transpose.
*
               SMALLN = .NOT.CBIGP( CIP91, M, N )
               IF( SMALLN )THEN
                  TINYN = .NOT.CBIGP( CIP92, M, N )
                  DO 40, II = M-MOD( M-1, RCB ), 1, -RCB
                     ISEC = MIN( RCB, M-II+1 )
*
*                    C := -1*A*C + alpha*C, general matrix multiply
*                    involving a rectangular block of A.
*
                     CALL CGEMM ( 'N', 'N', ISEC, N, M-II-ISEC+1, -ONE,
     $                      A( II, II+ISEC ), LDA, C( II+ISEC, 1 ), LDC,
     $                                          ALPHA, C( II, 1 ), LDC )
                     IF( TINYN )THEN
*
*                       Solve A*X = C, triangular system solve involving
*                       a upper triangular diagonal block of A. The
*                       block of X is overwritten on C.
*
                        DO 10, J = 1, N
                           CALL CTRSV ( 'U', 'N', DIAG, ISEC,
     $                                 A( II, II ), LDA, C( II, J ), 1 )
   10                   CONTINUE
                     ELSE
*
*                       T3 := A, a upper unit or non-unit triangular
*                       diagonal block of A is copied to the upper
*                       triangular part of T3.
*
                        DO 20, I = II+OFFD, II+ISEC-1
                           CALL CCOPY ( I-II+1-OFFD, A( II, I ), 1,
     $                                              T3( 1, I-II+1 ), 1 )
   20                   CONTINUE
*
*                       Solve T3*X = C, triangular system solve
*                       involving a upper triangular diagonal block of A
*                       stored in T3. The block of X is overwritten
*                       on C.
*
                        DO 30, J = 1, N
                           CALL CTRSV ( 'U', 'N', DIAG, ISEC,
     $                                  T3( 1, 1 ), RCB, C( II, J ), 1 )
   30                   CONTINUE
                     END IF
   40             CONTINUE
               ELSE
                  DELTA = ONE
                  CLDC = CCLD( LDC )
                  DO 110, II = M-MOD( M-1, CB ), 1, -CB
                     ISEC = MIN( CB, M-II+1 )
*
*                    C := -1*A*C + alpha*C, general matrix multiply
*                    involving a rectangular block of A.
*
                     CALL CGEMM ( 'N', 'N', ISEC, N, M-II-ISEC+1, -ONE,
     $                      A( II, II+ISEC ), LDA, C( II+ISEC, 1 ), LDC,
     $                                          ALPHA, C( II, 1 ), LDC )
*
*                    T2 := A', the transpose of a upper unit or non-unit
*                    triangular diagonal block of A is copied to the
*                    lower triangular part of T2.
*
                     DO 50, I = II+OFFD, II+ISEC-1
                        CALL CCOPY ( I-II+1-OFFD, A( II, I ), 1,
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
                              CALL CCOPY ( ISEC, C( II, J ), 1,
     $                                             T1( J-JJ+1, 1 ), RB )
   60                      CONTINUE
                        ELSE
                           DO 70, I = II, II+ISEC-1
                              CALL CCOPY ( JSEC, C( I, JJ ), LDC,
     $                                              T1( 1, I-II+1 ), 1 )
   70                      CONTINUE
                        END IF
*
*                       T1 := gamma*T1*T2 + delta*T1, triangular matrix
*                       multiply where the values of gamma and delta
*                       depend on whether T2 stores a unit or non-unit
*                       triangular block. Gamma and tsec are also used
*                       to compensate for a deficiency in CGEMV that
*                       appears if the second dimension (tsec) is zero.
*
                        DO 80, I = II+ISEC-1, II, -1
                           IF( NOUNIT )THEN
                              DELTA = ONE/T2( I-II+1, I-II+1 )
                           END IF
                           GAMMA = -DELTA
                           TSEC = II+ISEC-1-I
                           IF( TSEC.EQ.0 )THEN
                              TSEC = 1
                              GAMMA = ZERO
                           END IF
                           CALL CGEMV ( 'N', JSEC, TSEC, GAMMA,
     $                        T1( 1, I-II+2 ), RB, T2( I-II+2, I-II+1 ),
     $                                    1, DELTA, T1( 1, I-II+1 ), 1 )
   80                   CONTINUE
*
*                       C := T1', the transpose of T1 is copied back
*                       to C.
*
                        DO 90, J = JJ, JJ+JSEC-1
                           CALL CCOPY ( ISEC, T1( J-JJ+1, 1 ), RB,
     $                                                   C( II, J ), 1 )
   90                   CONTINUE
  100                CONTINUE
  110             CONTINUE
               END IF
            ELSE
*
*              Solve  A'*X = alpha*C  or  conjg( A' )*X = alpha*C.
*              Left, Upper, Transpose or Conjugated transpose.
*
               SMALLN = .NOT.CBIGP( CIP91, M, N )
               IF( SMALLN )THEN
                  TINYN = .NOT.CBIGP( CIP92, M, N )
                  DO 150, II = 1, M, RCB
                     ISEC = MIN( RCB, M-II+1 )
*
*                    C := -1*A'*C + alpha*C  or
*                    C := -1*conjg( A' )*C + alpha*C, matrix multiply
*                    involving the transpose of a rectangular block
*                    of A.
*
                     CALL CGEMM ( TRANSA, 'N', ISEC, N, II-1, -ONE,
     $                                  A( 1, II ), LDA, C( 1, 1 ), LDC,
     $                                          ALPHA, C( II, 1 ), LDC )
                     IF( TINYN )THEN
*
*                       Solve A'*X = C  or  conjg( A' )*X = C,
*                       triangular system solve involving a upper
*                       triangular diagonal block of A. The block of X
*                       is overwritten on C.
*
                        DO 120, J = 1, N
                           CALL CTRSV ( 'U', TRANSA, DIAG, ISEC,
     $                                 A( II, II ), LDA, C( II, J ), 1 )
  120                   CONTINUE
                     ELSE
*
*                       T3 :=  A, a unit or non-unit triangular diagonal
*                       block of A is copied to T3.
*
                        DO 130, I = II+OFFD, II+ISEC-1
                           CALL CCOPY ( I-II+1-OFFD, A( II, I ), 1,
     $                                              T3( 1, I-II+1 ), 1 )
  130                   CONTINUE
*
*                       Solve T3'*X = C  or  conjg( T3' )*X = C,
*                       triangular system solve involving a upper
*                       triangular diagonal block of A stored in T3. The
*                       block of X is overwritten on C.
*
                        DO 140, J = 1, N
                           CALL CTRSV ( 'U', TRANSA, DIAG, ISEC,
     $                                  T3( 1, 1 ), RCB, C( II, J ), 1 )
  140                   CONTINUE
                     END IF
  150             CONTINUE
               ELSE
                  DELTA = ONE
                  CLDC = CCLD( LDC )
                  DO 240, II = 1, M, CB
                     ISEC = MIN( CB, M-II+1 )
*
*                    C := -1*A'*C + alpha*C  or
*                    C := -1*conjg( A' )*C + alpha*C, matrix multiply
*                    involving the transpose of a rectangular block
*                    of A.
*
                     CALL CGEMM ( TRANSA, 'N', ISEC, N, II-1, -ONE,
     $                                  A( 1, II ), LDA, C( 1, 1 ), LDC,
     $                                          ALPHA, C( II, 1 ), LDC )
*
*                    T2 := A or T2 := conjg( A ), a unit or non-unit
*                    upper triangular diagonal block of A is copied to
*                    the upper triangular part of T2.
*
                     IF( NOCONJ )THEN
                        DO 160, J = II+OFFD, II+ISEC-1
                           CALL CCOPY ( J-II+1-OFFD, A( II, J ), 1,
     $                                              T2( 1, J-II+1 ), 1 )
  160                   CONTINUE
                     ELSE
                        DO 180, J = II+OFFD, II+ISEC-1
                           DO 170, I = II, J-OFFD
                              T2( I-II+1, J-II+1 ) = CONJG( A( I, J ) )
  170                      CONTINUE
  180                   CONTINUE
                     END IF
                     DO 230, JJ = 1, N, RB
                        JSEC = MIN( RB, N-JJ+1 )
*
*                       T1 := C', the transpose of a rectangular block
*                       of C is copied to T1.
*
                        IF( CLDC )THEN
                           DO 190, J = JJ, JJ+JSEC-1
                              CALL CCOPY ( ISEC, C( II, J ), 1,
     $                                             T1( J-JJ+1, 1 ), RB )
  190                      CONTINUE
                        ELSE
                           DO 200, I = II, II+ISEC-1
                              CALL CCOPY ( JSEC, C( I, JJ ), LDC,
     $                                              T1( 1, I-II+1 ), 1 )
  200                      CONTINUE
                        END IF
*
*                       T1 := gamma*T1*A + delta*T1, triangular matrix
*                       multiply where the values of gamma and delta
*                       depend on whether A is a unit or non-unit
*                       triangular matrix. Gamma and tsec are also used
*                       to compensate for a deficiency in CGEMV that
*                       appears if the second dimension (tsec) is zero.
*
                        DO 210, I = II, II+ISEC-1
                           IF( NOUNIT )THEN
                              DELTA = ONE/T2( I-II+1, I-II+1 )
                           END IF
                           GAMMA = -DELTA
                           TSEC = I-II
                           IF( TSEC.EQ.0 )THEN
                              TSEC = 1
                              GAMMA = ZERO
                           END IF
                           CALL CGEMV ( 'N', JSEC, TSEC, GAMMA,
     $                               T1( 1, 1 ), RB, T2( 1, I-II+1 ), 1,
     $                                       DELTA, T1( 1, I-II+1 ), 1 )
  210                   CONTINUE
*
*                       C := T1', the transpose of T1 is copied back
*                       to C.
*
                        DO 220, J = JJ, JJ+JSEC-1
                           CALL CCOPY ( ISEC, T1( J-JJ+1, 1 ), RB,
     $                                                   C( II, J ), 1 )
  220                   CONTINUE
  230                CONTINUE
  240             CONTINUE
               END IF
            END IF
         ELSE
            IF( NOTR )THEN
*
*              Solve  A*X = alpha*C. Left, Lower, No transpose.
*
               SMALLN = .NOT.CBIGP( CIP91, M, N )
               IF( SMALLN )THEN
                  TINYN = .NOT.CBIGP( CIP92, M, N )
                  DO 280, IX = MOD( M-1, RCB )+1, M, RCB
                     II = MAX( 1, IX-RCB+1 )
                     ISEC = IX-II+1
*
*                    C := -1*A*C + alpha*C, general matrix multiply
*                    involving a rectangular block of A.
*
                     CALL CGEMM ( 'N', 'N', ISEC, N, II-1, -ONE,
     $                                  A( II, 1 ), LDA, C( 1, 1 ), LDC,
     $                                          ALPHA, C( II, 1 ), LDC )
                     IF( TINYN )THEN
*
*                       Solve A*X = C, triangular system solve involving
*                       a lower triangular diagonal block of A. The
*                       block of X is overwritten on C.
*
                        DO 250, J = 1, N
                           CALL CTRSV ( 'L', 'N', DIAG, ISEC,
     $                                 A( II, II ), LDA, C( II, J ), 1 )
  250                   CONTINUE
                     ELSE
*
*                       T3 := A, a lower unit or non-unit triangular
*                       diagonal block of A is copied to the lower
*                       triangular part of T3. The block of X is
*                       overwritten on C.
*
                        DO 260, I = II, II+ISEC-1-OFFD
                           CALL CCOPY ( II+ISEC-I-OFFD, A( I+OFFD, I ),
     $                                 1, T3( I-II+1+OFFD, I-II+1 ), 1 )
  260                   CONTINUE
*
*                       Solve T3*X = C, triangular system solve
*                       involving a lower triangular diagonal block of A
*                       stored in T3. The block of X is overwritten
*                       on C.
*
                        DO 270, J = 1, N
                           CALL CTRSV ( 'L', 'N', DIAG, ISEC,
     $                                  T3( 1, 1 ), RCB, C( II, J ), 1 )
  270                   CONTINUE
                     END IF
  280             CONTINUE
               ELSE
                  DELTA = ONE
                  CLDC = CCLD( LDC )
                  DO 350, IX = MOD( M-1, CB )+1, M, CB
                     II = MAX( 1, IX-CB+1 )
                     ISEC = IX-II+1
*
*                    C := -1*A*C + alpha*C, general matrix multiply
*                    involving a rectangular block of A.
*
                     CALL CGEMM ( 'N', 'N', ISEC, N, II-1, -ONE,
     $                                  A( II, 1 ), LDA, C( 1, 1 ), LDC,
     $                                          ALPHA, C( II, 1 ), LDC )
*
*                    T2 := A', the transpose of a lower unit or non-unit
*                    triangular diagonal block of A is copied to the
*                    upper triangular part of T2.
*
                     DO 290, I = II, II+ISEC-1-OFFD
                        CALL CCOPY ( II+ISEC-I-OFFD, A( I+OFFD, I ),
     $                                1, T2( I-II+1, I-II+1+OFFD ), CB )
  290                CONTINUE
                     DO 340, JJ = 1, N, RB
                        JSEC = MIN( RB, N-JJ+1 )
*
*                       T1 := C', the transpose of a rectangular block
*                       of C is copied to T1.
*
                        IF( CLDC )THEN
                           DO 300, J = JJ, JJ+JSEC-1
                              CALL CCOPY ( ISEC, C( II, J ), 1,
     $                                             T1( J-JJ+1, 1 ), RB )
  300                      CONTINUE
                        ELSE
                           DO 310, I = II, II+ISEC-1
                              CALL CCOPY ( JSEC, C( I, JJ ), LDC,
     $                                              T1( 1, I-II+1 ), 1 )
  310                      CONTINUE
                        END IF
*
*                       T1 := gamma*T1*T2 + delta*T1, triangular matrix
*                       multiply where the values of gamma and delta
*                       depend on whether T2 stores a unit or non-unit
*                       triangular block. Gamma and tsec are also used
*                       to compensate for a deficiency in CGEMV that
*                       appears if the second dimension (tsec) is zero.
*
                        DO 320, I = II, II+ISEC-1
                           IF( NOUNIT )THEN
                              DELTA = ONE/T2( I-II+1, I-II+1 )
                           END IF
                           GAMMA = -DELTA
                           TSEC = I-II
                           IF( TSEC.EQ.0 )THEN
                              TSEC = 1
                              GAMMA = ZERO
                           END IF
                           CALL CGEMV ( 'N', JSEC, TSEC, GAMMA,
     $                               T1( 1, 1 ), RB, T2( 1, I-II+1 ), 1,
     $                                       DELTA, T1( 1, I-II+1 ), 1 )
  320                   CONTINUE
*
*                       C := T1', the transpose of T1 is copied back
*                       to C.
*
                        DO 330, J = JJ, JJ+JSEC-1
                           CALL CCOPY ( ISEC, T1( J-JJ+1, 1 ), RB,
     $                                                   C( II, J ), 1 )
  330                   CONTINUE
  340                CONTINUE
  350             CONTINUE
               END IF
            ELSE
*
*              Solve  A'*X = alpha*C  or  conjg( A' )*X = alpha*C.
*              Left, Lower, Transpose or Conjugated transpose.
*
               SMALLN = .NOT.CBIGP( CIP91, M, N )
               IF( SMALLN )THEN
                  TINYN = .NOT.CBIGP( CIP92, M, N )
                  DO 390, IX = M, 1, -RCB
                     II = MAX( 1, IX-RCB+1 )
                     ISEC = IX-II+1
*
*                    C := -1*A'*C + alpha*C  or
*                    C := -1*conjg( A' )*C + alpha*C, matrix multiply
*                    involving the transpose of a rectangular block
*                    of A.
*
                     CALL CGEMM ( TRANSA, 'N', ISEC, N, M-II-ISEC+1,
     $                     -ONE, A( II+ISEC, II ), LDA, C( II+ISEC, 1 ),
     $                                     LDC, ALPHA, C( II, 1 ), LDC )
                     IF( TINYN )THEN
*
*                       Solve A'*X = C  or  conjg( A' )*X = C,
*                       triangular system solve involving a lower
*                       triangular diagonal block of A. The block of X
*                       is overwritten on C.
*
                        DO 360, J = 1, N
                           CALL CTRSV ( 'L', TRANSA, DIAG, ISEC,
     $                                 A( II, II ), LDA, C( II, J ), 1 )
  360                   CONTINUE
                     ELSE
*
*                       T3 :=  A, a unit or non-unit triangular diagonal
*                       block of A is copied to T3.
*
                        DO 370, I = II, II+ISEC-1-OFFD
                           CALL CCOPY ( II+ISEC-I-OFFD, A( I+OFFD, I ),
     $                                 1, T3( I-II+1+OFFD, I-II+1 ), 1 )
  370                   CONTINUE
*
*                       Solve T3'*X = C  or  conjg( T3' )*X = C,
*                       triangular system solve involving a lower
*                       triangular diagonal block of A stored in T3. The
*                       block of X is overwritten on C.
*
                        DO 380, J = 1, N
                           CALL CTRSV ( 'L', TRANSA, DIAG, ISEC,
     $                                  T3( 1, 1 ), RCB, C( II, J ), 1 )
  380                   CONTINUE
                     END IF
  390             CONTINUE
               ELSE
                  DELTA = ONE
                  CLDC = CCLD( LDC )
                  DO 480, IX = M, 1, -CB
                     II = MAX( 1, IX-CB+1 )
                     ISEC = IX-II+1
*
*                    C := -1*A'*C + alpha*C  or
*                    C := -1*conjg( A' )*C + alpha*C, matrix multiply
*                    involving the transpose of a rectangular block
*                    of A.
*
                     CALL CGEMM ( TRANSA, 'N', ISEC, N, M-II-ISEC+1,
     $                     -ONE, A( II+ISEC, II ), LDA, C( II+ISEC, 1 ),
     $                                     LDC, ALPHA, C( II, 1 ), LDC )
*
*                    T2 := A or T2 := conjg( A ), a unit or non-unit
*                    lower triangular diagonal block of A is copied to
*                    the lower triangular part of T2.
*
                     IF( NOCONJ )THEN
                        DO 400, J = II, II+ISEC-1-OFFD
                           CALL CCOPY ( II+ISEC-J-OFFD, A( J+OFFD, J ),
     $                                 1, T2( J-II+1+OFFD, J-II+1 ), 1 )
  400                   CONTINUE
                     ELSE
                        DO 420, J = II, II+ISEC-1-OFFD
                           DO 410, I = J+OFFD, II+ISEC-1
                              T2( I-II+1, J-II+1 ) = CONJG( A( I, J ) )
  410                      CONTINUE
  420                   CONTINUE
                     END IF
                     DO 470, JJ = 1, N, RB
                        JSEC = MIN( RB, N-JJ+1 )
*
*                       T1 := C', the transpose of a rectangular block
*                       of C is copied to T1.
*
                        IF( CLDC )THEN
                           DO 430, J = JJ, JJ+JSEC-1
                              CALL CCOPY ( ISEC, C( II, J ), 1,
     $                                             T1( J-JJ+1, 1 ), RB )
  430                      CONTINUE
                        ELSE
                           DO 440, I = II, II+ISEC-1
                              CALL CCOPY ( JSEC, C( I, JJ ), LDC,
     $                                              T1( 1, I-II+1 ), 1 )
  440                      CONTINUE
                        END IF
*
*                       T1 := gamma*T1*A + delta*T1, triangular matrix
*                       multiply where the values of gamma and delta
*                       depend on whether A is a unit or non-unit
*                       triangular matrix. Gamma and tsec are also used
*                       to compensate for a deficiency in CGEMV that
*                       appears if the second dimension (tsec) is zero.
*
                        DO 450, I = II+ISEC-1, II, -1
                           IF( NOUNIT )THEN
                              DELTA = ONE/T2( I-II+1, I-II+1 )
                           END IF
                           GAMMA = -DELTA
                           TSEC = II+ISEC-1-I
                           IF( TSEC.EQ.0 )THEN
                              TSEC = 1
                              GAMMA = ZERO
                           END IF
                           CALL CGEMV ( 'N', JSEC, TSEC, GAMMA,
     $                        T1( 1, I-II+2 ), RB, T2( I-II+2, I-II+1 ),
     $                                    1, DELTA, T1( 1, I-II+1 ), 1 )
  450                   CONTINUE
*
*                       C := T1', the transpose of T1 is copied back
*                       to C.
*
                        DO 460, J = JJ, JJ+JSEC-1
                           CALL CCOPY ( ISEC, T1( J-JJ+1, 1 ), RB,
     $                                                   C( II, J ), 1 )
  460                   CONTINUE
  470                CONTINUE
  480             CONTINUE
               END IF
            END IF
         END IF
      ELSE
         IF( UPPER )THEN
            IF( NOTR )THEN
*
*              Solve  X*A = alpha*C. Right, Upper, No transpose.
*
               TINYM = .NOT.CBIGP( CIP93, M, N )
               IF( TINYM )THEN
                  DO 500, JJ = 1, N, RCB
                     JSEC = MIN( RCB, N-JJ+1 )
*
*                    C := -1*C*A + alpha*C, general matrix multiply
*                    involving a rectangular block of A.
*
                     CALL CGEMM ( 'N', 'N', M, JSEC, JJ-1, -ONE,
     $                                  C( 1, 1 ), LDC, A( 1, JJ ), LDA,
     $                                          ALPHA, C( 1, JJ ), LDC )
*
*                    Solve X*A = C, triangular system solve involving
*                    a upper triangular diagonal block of A. The block
*                    of X is overwritten on C.
*
                     DO 490, I = 1, M
                        CALL CTRSV ( 'U', 'T', DIAG, JSEC,
     $                               A( JJ, JJ ), LDA, C( I, JJ ), LDC )
  490                CONTINUE
  500             CONTINUE
               ELSE
                  DELTA = ONE
                  DO 550, JJ = 1, N, CB
                     JSEC = MIN( CB, N-JJ+1 )
*
*                    C := -1*C*A + alpha*C, general matrix multiply
*                    involving a rectangular block of A.
*
                     CALL CGEMM ( 'N', 'N', M, JSEC, JJ-1, -ONE,
     $                                  C( 1, 1 ), LDC, A( 1, JJ ), LDA,
     $                                          ALPHA, C( 1, JJ ), LDC )
                     DO 540, II = 1, M, RB
                        ISEC = MIN( RB, M-II+1 )
*
*                       T1 := C, a rectangular block of C is copied
*                       to T1.
*
                        DO 510, J = JJ, JJ+JSEC-1
                           CALL CCOPY ( ISEC, C( II, J ), 1,
     $                                              T1( 1, J-JJ+1 ), 1 )
  510                   CONTINUE
*
*                       C := gamma*T1*A + delta*C, triangular matrix
*                       multiply where the values of gamma and delta
*                       depend on whether A is a unit or non-unit
*                       triangular matrix. Gamma and tsec are also used
*                       to compensate for a deficiency in CGEMV that
*                       appears if the second dimension (tsec) is zero.
*
                        DO 520, J =  JJ, JJ+JSEC-1
                           IF( NOUNIT )THEN
                              DELTA = ONE/A( J, J )
                           END IF
                           GAMMA = -DELTA
                           TSEC = J-JJ
                           IF( TSEC.EQ.0 )THEN
                              TSEC = 1
                              GAMMA = ZERO
                           END IF
                           CALL CGEMV ( 'N', ISEC, TSEC, GAMMA,
     $                                    T1( 1, 1 ), RB, A( JJ, J ), 1,
     $                                       DELTA, T1( 1, J-JJ+1 ), 1 )
  520                   CONTINUE
*
*                       C := T1, T1 is copied back to C.
*
                        DO 530, J = JJ, JJ+JSEC-1
                           CALL CCOPY ( ISEC, T1( 1, J-JJ+1 ), 1,
     $                                                   C( II, J ), 1 )
  530                   CONTINUE
  540                CONTINUE
  550             CONTINUE
               END IF
            ELSE
*
*              Solve  X*A' = alpha*C  or X*conjg( A' ) = alpha*C.
*              Right, Upper, Transpose or Conjugated transpose.
*
               TINYM = .NOT.CBIGP( CIP93, M, N )
               IF( TINYM )THEN
                  DO 580, JJ = N-MOD( N-1, RCB ), 1, -RCB
                     JSEC = MIN( RCB, N-JJ+1 )
*
*                    C := -1*C*A' + alpha*C  or
*                    C := -1*C*conjg( A' ) + alpha*C, matrix multiply
*                    involving the transpose of a rectangular block
*                    of A.
*
                     CALL CGEMM ( 'N', TRANSA, M, JSEC, N-JJ-JSEC+1,
     $                     -ONE, C( 1, JJ+JSEC ), LDC, A( JJ, JJ+JSEC ),
     $                                     LDA, ALPHA, C( 1, JJ ), LDC )
*
*                    T3 := A', the transpose of a upper unit or non-unit
*                    triangular diagonal block of A is copied to T3.
*
                     DO 560, J = JJ+OFFD, JJ+JSEC-1
                        CALL CCOPY ( J-JJ+1-OFFD, A( JJ, J ), 1,
     $                                            T3( J-JJ+1, 1 ), RCB )
  560                CONTINUE
*
*                    Solve X*T3' = C  or X*conjg( T3' ) = C, triangular
*                    system solve involving the transpose of a upper
*                    triangular diagonal block of A stored in T3. The
*                    block of X is overwritten on C.
*
                     DO 570, I = 1, M
                        CALL CTRSV ( 'L', TRANSA, DIAG, JSEC,
     $                                T3( 1, 1 ), RCB, C( I, JJ ), LDC )
  570                CONTINUE
  580             CONTINUE
               ELSE
                  DELTA = ONE
                  DO 660, JJ = N-MOD( N-1, CB ), 1, -CB
                     JSEC = MIN( CB, N-JJ+1 )
*
*                    C := -1*C*A' + alpha*C  or
*                    C := -1*C*conjg( A' ) + alpha*C, matrix multiply
*                    involving the transpose of a rectangular block
*                    of A.
*
                     CALL CGEMM ( 'N', TRANSA, M, JSEC, N-JJ-JSEC+1,
     $                     -ONE, C( 1, JJ+JSEC ), LDC, A( JJ, JJ+JSEC ),
     $                                     LDA, ALPHA, C( 1, JJ ), LDC )
*
*                    T2 := A' or T2 := conjg( A' ), the transpose of a
*                    unit or non-unit upper triangular diagonal block of
*                    A is copied to the lower triangular part of T2.
*
                     IF( NOCONJ )THEN
                        DO 590, J = JJ+OFFD, JJ+JSEC-1
                           CALL CCOPY ( J-JJ+1-OFFD, A( JJ, J ), 1,
     $                                             T2( J-JJ+1, 1 ), CB )
  590                   CONTINUE
                     ELSE
                        DO 610, J = JJ+OFFD, JJ+JSEC-1
                           DO 600, I = JJ, J-OFFD
                              T2( J-JJ+1, I-JJ+1 ) = CONJG( A( I, J ) )
  600                      CONTINUE
  610                   CONTINUE
                     END IF
                     DO 650, II = 1, M, RB
                        ISEC = MIN( RB, M-II+1 )
*
*                       T1 := C, a rectangular block of C is copied
*                       to T1.
*
                        DO 620, J = JJ, JJ+JSEC-1
                           CALL CCOPY ( ISEC, C( II, J ), 1,
     $                                              T1( 1, J-JJ+1 ), 1 )
  620                   CONTINUE
*
*                       C := gamma*T1*T2 + delta*C, triangular matrix
*                       multiply where the values of gamma and delta
*                       depend on whether T2 is a unit or non-unit
*                       triangular matrix. Gamma and tsec are also used
*                       to compensate for a deficiency in CGEMV that
*                       appears if the second dimension (tsec) is zero.
*
                        DO 630, J = JJ+JSEC-1, JJ, -1
                           IF( NOUNIT )THEN
                              DELTA = ONE/T2( J-JJ+1, J-JJ+1 )
                           END IF
                           GAMMA = -DELTA
                           TSEC = JJ+JSEC-1-J
                           IF( TSEC.EQ.0 )THEN
                              TSEC = 1
                              GAMMA = ZERO
                           END IF
                           CALL CGEMV ( 'N', ISEC, TSEC, GAMMA,
     $                        T1( 1, J-JJ+2 ), RB, T2( J-JJ+2, J-JJ+1 ),
     $                                    1, DELTA, T1( 1, J-JJ+1 ), 1 )
  630                   CONTINUE
*
*                       C := T1, T1 is copied back to C.
*
                        DO 640, J = JJ, JJ+JSEC-1
                           CALL CCOPY ( ISEC, T1( 1, J-JJ+1 ), 1,
     $                                                   C( II, J ), 1 )
  640                   CONTINUE
  650                CONTINUE
  660             CONTINUE
               END IF
            END IF
         ELSE
            IF( NOTR )THEN
*
*              Solve  X*A = alpha*C. Right, Lower, No transpose.
*
               TINYM = .NOT.CBIGP( CIP93, M, N )
               IF( TINYM )THEN
                  DO 680, JX = N, 1, -RCB
                     JJ = MAX( 1, JX-RCB+1 )
                     JSEC = JX-JJ+1
*
*                    C := -1*C*A + alpha*C, general matrix multiply
*                    involving a rectangular block of A.
*
                     CALL CGEMM ( 'N', 'N', M, JSEC, N-JJ-JSEC+1, -ONE,
     $                           C( 1, JJ+JSEC ), LDC, A( JJ+JSEC, JJ ),
     $                                     LDA, ALPHA, C( 1, JJ ), LDC )
*
*                    Solve X*A = C, triangular system solve involving
*                    a lower triangular diagonal block of A. The block
*                    of X is overwritten on C.
*
                     DO 670, I = 1, M
                        CALL CTRSV ( 'L', 'T', DIAG, JSEC,
     $                               A( JJ, JJ ), LDA, C( I, JJ ), LDC )
  670                CONTINUE
  680             CONTINUE
               ELSE
                  DELTA = ONE
                  DO 730, JX = N, 1, -CB
                     JJ = MAX( 1, JX-CB+1 )
                     JSEC = JX-JJ+1
*
*                    C := -1*C*A + alpha*C, general matrix multiply
*                    involving a rectangular block of A.
*
                     CALL CGEMM ( 'N', 'N', M, JSEC, N-JJ-JSEC+1, -ONE,
     $                           C( 1, JJ+JSEC ), LDC, A( JJ+JSEC, JJ ),
     $                                     LDA, ALPHA, C( 1, JJ ), LDC )
                     DO 720, II = 1, M, RB
                        ISEC = MIN( RB, M-II+1 )
*
*                       T1 := C, a rectangular block of C is copied
*                       to T1.
*
                        DO 690, J = JJ, JJ+JSEC-1
                           CALL CCOPY ( ISEC, C( II, J ), 1,
     $                                              T1( 1, J-JJ+1 ), 1 )
  690                   CONTINUE
*
*                       C := gamma*T1*A + delta*C, triangular matrix
*                       multiply where the values of gamma and delta
*                       depend on whether A is a unit or non-unit
*                       triangular matrix. Gamma and tsec are also used
*                       to compensate for a deficiency in CGEMV that
*                       appears if the second dimension (tsec) is zero.
*
                        DO 700, J = JJ+JSEC-1, JJ, -1
                           IF( NOUNIT )THEN
                              DELTA = ONE/A( J, J )
                           END IF
                           GAMMA = -DELTA
                           TSEC = JJ+JSEC-1-J
                           IF( TSEC.EQ.0 )THEN
                              TSEC = 1
                              GAMMA = ZERO
                           END IF
                           CALL CGEMV ( 'N', ISEC, TSEC, GAMMA,
     $                              T1( 1, J-JJ+2 ), RB, A( J+1, J ), 1,
     $                                       DELTA, T1( 1, J-JJ+1 ), 1 )
  700                   CONTINUE
*
*                       C := T1, T1 is copied back to C.
*
                        DO 710, J = JJ, JJ+JSEC-1
                           CALL CCOPY ( ISEC, T1( 1, J-JJ+1 ), 1,
     $                                                   C( II, J ), 1 )
  710                   CONTINUE
  720                CONTINUE
  730             CONTINUE
               END IF
            ELSE
*
*              Solve  X*A' = alpha*C  or X*conjg( A' ) = alpha*C.
*              Right, Lower, Transpose or Conjugated transpose.
*
               TINYM = .NOT.CBIGP( CIP93, M, N )
               IF( TINYM )THEN
                  DO 760, JX = MOD( N-1, RCB )+1, N, RCB
                     JJ = MAX( 1, JX-RCB+1 )
                     JSEC = JX-JJ+1
*
*                    C := -1*C*A' + alpha*C  or
*                    C := -1*C*conjg( A' ) + alpha*C, matrix multiply
*                    involving the transpose of a rectangular block
*                    of A.
*
                     CALL CGEMM ( 'N', TRANSA, M, JSEC, JJ-1, -ONE,
     $                                  C( 1, 1 ), LDC, A( JJ, 1 ), LDA,
     $                                          ALPHA, C( 1, JJ ), LDC )
*
*                    T3 := A', the transpose of a lower unit or non-unit
*                    triangular diagonal block of A is copied to T3.
*
                     DO 740, J = JJ, JJ+JSEC-1-OFFD
                        CALL CCOPY ( JJ+JSEC-J-OFFD, A( J+OFFD, J ),
     $                               1, T3( J-JJ+1, J-JJ+1+OFFD ), RCB )
  740                CONTINUE
*
*                    Solve X*T3' = C  or X*conjg( T3' ) = C, triangular
*                    system solve involving the transpose of a lower
*                    triangular diagonal block of A stored in T3. The
*                    block of X is overwritten on C.
*
                     DO 750, I = 1, M
                        CALL CTRSV ( 'U', TRANSA, DIAG, JSEC,
     $                                T3( 1, 1 ), RCB, C( I, JJ ), LDC )
  750                CONTINUE
  760             CONTINUE
               ELSE
                  DELTA = ONE
                  DO 840, JX = MOD( N-1, CB )+1, N, CB
                     JJ = MAX( 1, JX-CB+1 )
                     JSEC = JX-JJ+1
*
*                    C := -1*C*A' + alpha*C  or
*                    C := -1*C*conjg( A' ) + alpha*C, matrix multiply
*                    involving the transpose of a rectangular block
*                    of A.
*
                     CALL CGEMM ( 'N', TRANSA, M, JSEC, JJ-1, -ONE,
     $                                  C( 1, 1 ), LDC, A( JJ, 1 ), LDA,
     $                                          ALPHA, C( 1, JJ ), LDC )
*
*                    T2 := A' or T2 := conjg( A' ), the transpose of a
*                    unit or non-unit lower triangular diagonal block of
*                    A is copied to the lower triangular part of T2.
*
                     IF( NOCONJ )THEN
                        DO 770, J = JJ, JJ+JSEC-1-OFFD
                           CALL CCOPY ( JJ+JSEC-J-OFFD, A( J+OFFD, J ),
     $                                1, T2( J-JJ+1, J-JJ+1+OFFD ), CB )
  770                   CONTINUE
                     ELSE
                        DO 790, J = JJ, JJ+JSEC-1-OFFD
                           DO 780, I = J+OFFD, JJ+JSEC-1
                              T2( J-JJ+1, I-JJ+1 ) = CONJG( A( I, J ) )
  780                      CONTINUE
  790                   CONTINUE
                     END IF
                     DO 830, II = 1, M, RB
                        ISEC = MIN( RB, M-II+1 )
*
*                       T1 := C, a rectangular block of C is copied
*                       to T1.
*
                        DO 800, J = JJ, JJ+JSEC-1
                           CALL CCOPY ( ISEC, C( II, J ), 1,
     $                                              T1( 1, J-JJ+1 ), 1 )
  800                   CONTINUE
*
*                       C := gamma*T1*T2 + delta*C, triangular matrix
*                       multiply where the values of gamma and delta
*                       depend on whether T2 is a unit or non-unit
*                       triangular matrix. Gamma and tsec are also used
*                       to compensate for a deficiency in CGEMV that
*                       appears if the second dimension (tsec) is zero.
*
                        DO 810, J = JJ, JJ+JSEC-1
                           IF( NOUNIT )THEN
                              DELTA = ONE/T2( J-JJ+1, J-JJ+1 )
                           END IF
                           GAMMA = -DELTA
                           TSEC = J-JJ
                           IF( TSEC.EQ.0 )THEN
                              TSEC = 1
                              GAMMA = ZERO
                           END IF
                           CALL CGEMV ( 'N', ISEC, TSEC, GAMMA,
     $                               T1( 1, 1 ), RB, T2( 1, J-JJ+1 ), 1,
     $                                       DELTA, T1( 1, J-JJ+1 ), 1 )
  810                   CONTINUE
*
*                       C := T1, T1 is copied back to C.
*
                        DO 820, J = JJ, JJ+JSEC-1
                           CALL CCOPY ( ISEC, T1( 1, J-JJ+1 ), 1,
     $                                                   C( II, J ), 1 )
  820                   CONTINUE
  830                CONTINUE
  840             CONTINUE
               END IF
            END IF
         END IF
      END IF
*
      RETURN
*
*     End of CTRSM.
*
      END
