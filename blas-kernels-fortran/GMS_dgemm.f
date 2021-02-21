
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
SUBROUTINE DGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,&
BETA,C,LDC,INFO,MB,NB,NBT, KB) !GCC$ ATTRIBUTES aligned(32) :: DGEMM !GCC$ ATTRIBUTES hot :: DGEMM !GCC$ ATTRIBUTES no_stack_protector :: DGEMM !GCC$ ATTRIBUTES no_profile_instrument_function :: DGEMM
!     *     .. Scalar Arguments ..
      use omp_lib
      implicit none
      CHARACTER*1        TRANSA, TRANSB
      INTEGER            M, N, K, LDA, LDB, LDC, INFO
      INTEGER            MB, NB, NBT, KB 
      DOUBLE PRECISION   ALPHA, BETA
!*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), C( LDC, * )
#if 0
*     ..
*
*  Purpose
*  =======
*
*  DGEMM  performs one of the matrix-matrix operations
*
*     C := alpha*op( A )*op( B ) + beta*C,
*
*  where  op( X ) is one of
*
*     op( X ) = X   or   op( X ) = X',
*
*  alpha and beta are scalars, and A, B and C are matrices, with op( A )
*  an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
*
*  Parameters
*  ==========
*
*  TRANSA - CHARACTER*1.
*           On entry, TRANSA specifies the form of op( A ) to be used in
*           the matrix multiplication as follows:
*
*              TRANSA = 'N' or 'n',  op( A ) = A.
*
*              TRANSA = 'T' or 't',  op( A ) = A'.
*
*              TRANSA = 'C' or 'c',  op( A ) = A'.
*
*           Unchanged on exit.
*
*  TRANSB - CHARACTER*1.
*           On entry, TRANSB specifies the form of op( B ) to be used in
*           the matrix multiplication as follows:
*
*              TRANSB = 'N' or 'n',  op( B ) = B.
*
*              TRANSB = 'T' or 't',  op( B ) = B'.
*
*              TRANSB = 'C' or 'c',  op( B ) = B'.
*
*           Unchanged on exit.
*
*  M      - INTEGER.
*           On entry,  M  specifies  the number  of rows  of the  matrix
*           op( A )  and of the  matrix  C.  M  must  be at least  zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry,  N  specifies the number  of columns of the matrix
*           op( B ) and the number of columns of the matrix C. N must be
*           at least zero.
*           Unchanged on exit.
*
*  K      - INTEGER.
*           On entry,  K  specifies  the number of columns of the matrix
*           op( A ) and the number of rows of the matrix op( B ). K must
*           be at least  zero.
*           Unchanged on exit.
*
*  ALPHA  - DOUBLE PRECISION.
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  A      - DOUBLE PRECISION array of DIMENSION ( LDA, ka ), where ka is
*           k  when  TRANSA = 'N' or 'n',  and is  m  otherwise.
*           Before entry with  TRANSA = 'N' or 'n',  the leading  m by k
*           part of the array  A  must contain the matrix  A,  otherwise
*           the leading  k by m  part of the array  A  must contain  the
*           matrix A.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. When  TRANSA = 'N' or 'n' then
*           LDA must be at least  max( 1, m ), otherwise  LDA must be at
*           least  max( 1, k ).
*           Unchanged on exit.
*
*  B      - DOUBLE PRECISION array of DIMENSION ( LDB, kb ), where kb is
*           n  when  TRANSB = 'N' or 'n',  and is  k  otherwise.
*           Before entry with  TRANSB = 'N' or 'n',  the leading  k by n
*           part of the array  B  must contain the matrix  B,  otherwise
*           the leading  n by k  part of the array  B  must contain  the
*           matrix B.
*           Unchanged on exit.
*
*  LDB    - INTEGER.
*           On entry, LDB specifies the first dimension of B as declared
*           in the calling (sub) program. When  TRANSB = 'N' or 'n' then
*           LDB must be at least  max( 1, k ), otherwise  LDB must be at
*           least  max( 1, n ).
*           Unchanged on exit.
*
*  BETA   - DOUBLE PRECISION.
*           On entry,  BETA  specifies the scalar  beta.  When  BETA  is
*           supplied as zero then C need not be set on input.
*           Unchanged on exit.
*
*  C      - DOUBLE PRECISION array of DIMENSION ( LDC, n ).
*           Before entry, the leading  m by n  part of the array  C must
*           contain the matrix  C,  except when  beta  is zero, in which
*           case C need not be set on entry.
*           On exit, the array  C  is overwritten by the  m by n  matrix
*           ( alpha*op( A )*op( B ) + beta*C ).
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
*  -- Modified in October-1997.
*     Superscalar GEMM-Based Level 3 BLAS (Version 0.1).
*     Per Ling, Department of Computing Science,
*     Umea University, Sweden.
*
*
*     .. Local Scalars ..
*  -- Modified by Bernard Gingold in 21-02-2021  
      INTEGER            I, II, ISEC, UISEC, J, JJ, JSEC, UJSEC, &
                         L, LL, LSEC, ULSEC, NROWA, NROWB
      LOGICAL            NOTA, NOTB
      DOUBLE PRECISION   DELTA
      DOUBLE PRECISION   F11, F12, F21, F22, F31, F32, F41, F42
      DOUBLE PRECISION   F13, F14, F23, F24, F33, F34, F43, F44
      DOUBLE PRECISION   TMP0,TMP1,TMP2,TMP3
      DOUBLE PRECISION   C0,C1,C2,C3,C4,C5,C6,C7,C8
                         
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN, MOD
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      !EXTERNAL           XERBLA
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER        ( ZERO = 0.0D+0, ONE = 1.0D+0 )
!*     .. User specified parameters for DGEMM ..
!      Moved to subroutine arguments.
     ! PARAMETER        ( MB = 32, NB = 1024, NBT = 96, KB = 32 )
      DOUBLE PRECISION   T1( KB, MB ), T2( KB, NBT )
*     ..
*     .. Executable Statements ..
*
*     Set  NOTA  and  NOTB  as  true if  A  and  B  respectively are not
*     transposed and set NROWA and NROWB as the number of rows of A  and
*     the number of rows of B respectively.
*
#endif
      NOTA = LSAME( TRANSA, 'N' )
      NOTB = LSAME( TRANSB, 'N' )
      IF ( NOTA ) THEN
         NROWA = M
      ELSE
         NROWA = K
      END IF
      IF ( NOTB ) THEN
         NROWB = K
      ELSE
         NROWB = N
      END IF
*
*     Test the input parameters.
*
      INFO = 0
      IF( ( .NOT.NOTA ).AND.( .NOT. LSAME( TRANSA,'C' ) )
     $                          .AND.( .NOT.LSAME( TRANSA, 'T' ) ) )THEN
         INFO = 1
      ELSE IF( ( .NOT.NOTB ).AND.( .NOT.LSAME( TRANSB, 'C' ) )
     $                          .AND.( .NOT.LSAME( TRANSB, 'T' ) ) )THEN
         INFO = 2
      ELSE IF( M.LT.0 )THEN
         INFO = 3
      ELSE IF( N.LT.0 )THEN
         INFO = 4
      ELSE IF( K.LT.0 )THEN
         INFO = 5
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 8
      ELSE IF( LDB.LT.MAX( 1, NROWB ) )THEN
         INFO = 10
      ELSE IF( LDC.LT.MAX( 1, M ) )THEN
         INFO = 13
      END IF
      IF( INFO.NE.0 )THEN
!CALL XERBLA( 'DGEMM ', INFO )
          RETURN
      END IF
*
*     Quick return if possible.
*
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.
     $     ( ( ( ALPHA.EQ.ZERO ).OR.( K.EQ.0 ) ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
*
*     And when alpha.eq.zero.
*
      IF( ( ALPHA.EQ.ZERO ).OR.( K.EQ.0 ) )THEN
         IF( BETA.EQ.ZERO )THEN
            UISEC = M-MOD( M, 4 )
            DO 30, J = 1, N
               DO 10, I = 1, UISEC, 4
                  C( I, J ) = ZERO
                  C( I+1, J ) = ZERO
                  C( I+2, J ) = ZERO
                  C( I+3, J ) = ZERO
   10          CONTINUE
               DO 20, I = UISEC+1, M
                  C( I, J ) = ZERO
   20          CONTINUE
   30       CONTINUE
         ELSE
            UISEC = M-MOD( M, 4 )
            DO 60, J = 1, N
               DO 40, I = 1, UISEC, 4
                  C( I, J ) = BETA*C( I, J )
                  C( I+1, J ) = BETA*C( I+1, J )
                  C( I+2, J ) = BETA*C( I+2, J )
                  C( I+3, J ) = BETA*C( I+3, J )
   40          CONTINUE
               DO 50, I = UISEC+1, M
                  C( I, J ) = BETA*C( I, J )
   50          CONTINUE
   60       CONTINUE
         END IF
         RETURN
      END IF
      T1(:,:) = ZERO
      T2(:,:) = ZERO
*
*     Start the operations.
*     
      IF (NOTB) THEN
*
*        Form  C := alpha*A*B + beta*C or C := alpha*A'*B + beta*C.
*
         TMP0 = 0.0D0
         TMP1 = 0.0D0
         TMP2 = 0.0D0
         TMP3 = 0.0D0
         DO 250 JJ = 1, N, NB
            JSEC = MIN( NB, N-JJ+1 )
            UJSEC = JSEC-MOD( JSEC, 4 )
            DO 240 LL = 1, K, KB
               LSEC = MIN( KB, K-LL+1 )
               ULSEC = LSEC-MOD( LSEC, 2 )
*
*              Determine if the block of C should be updated with
*              beta or not.
*
               DELTA = ONE
               IF( LL.EQ.1 ) DELTA = BETA
*
               DO 230 II = 1, M, MB
                  ISEC = MIN( MB, M-II+1 )
*
*                 T1 := alpha*A' or T1 := alpha*A, copy the transpose
*                 or the non-transpose of a rectangular block of
*                 alpha*A to T1.
*
                  UISEC = ISEC-MOD( ISEC, 2 )
                  IF( NOTA )THEN
                     DO 80, L = LL, LL+ULSEC-1, 2
                        DO 70, I = II, II+UISEC-1, 2
                           T1( L-LL+1, I-II+1 ) = ALPHA*A( I, L )
                           T1( L-LL+2, I-II+1 ) = ALPHA*A( I, L+1 )
                           T1( L-LL+1, I-II+2 ) = ALPHA*A( I+1, L )
                           T1( L-LL+2, I-II+2 ) = ALPHA*A( I+1, L+1 )
   70                   CONTINUE
                        IF( UISEC.LT.ISEC )THEN
                           T1( L-LL+1, ISEC ) = ALPHA*A( II+ISEC-1, L )
                           T1( L-LL+2, ISEC ) = ALPHA*A( II+ISEC-1, L+1)
                                             
                        END IF
   80                CONTINUE
                     IF( ULSEC.LT.LSEC )THEN
                        DO 90, I = II, II+ISEC-1
                           T1( LSEC, I-II+1 ) = ALPHA*A( I, LL+LSEC-1 )
   90                   CONTINUE
                     END IF
                  ELSE
                     DO 110, I = II, II+UISEC-1, 2
!$OMP SIMD 
                        DO 100, L = LL, LL+ULSEC-1, 2
                           T1( L-LL+1, I-II+1 )  = ALPHA*A( L, I )
                           T1( L-LL+1, I-II+2 ) = ALPHA*A( L, I+1 )
                           T1( L-LL+2, I-II+1 ) = ALPHA*A( L+1, I )
                           T1( L-LL+2, I-II+2 ) = ALPHA*A( L+1, I+1 )
  100                   CONTINUE
                        IF( ULSEC.LT.LSEC )THEN
                           T1( LSEC, I-II+1 ) = ALPHA*A( LL+LSEC-1, I )
                           T1( LSEC, I-II+2 ) = ALPHA*A( LL+LSEC-1, I+1)
                                             
                        END IF
  110                CONTINUE
                     IF( UISEC.LT.ISEC )THEN
                        DO 120, L = LL, LL+LSEC-1
                           T1( L-LL+1, ISEC ) = ALPHA*A( L, II+ISEC-1 )
  120                   CONTINUE
                     END IF
                  END IF
*
*                 C := T1'*B + beta*C, update a rectangular block
*                 of C using 4 by 4 unrolling.
*
                  UISEC = ISEC-MOD( ISEC, 4 )
                  DO 170 J = JJ, JJ+UJSEC-1, 4
                     DO 140 I = II, II+UISEC-1, 4
                        F11 = DELTA*C( I,J )
                        F21 = DELTA*C( I+1,J )
                        F12 = DELTA*C( I,J+1 )
                        F22 = DELTA*C( I+1,J+1 )
                        F13 = DELTA*C( I,J+2 )
                        F23 = DELTA*C( I+1,J+2 )
                        F14 = DELTA*C( I,J+3 )
                        F24 = DELTA*C( I+1,J+3 )
                        F31 = DELTA*C( I+2,J )
                        F41 = DELTA*C( I+3,J )
                        F32 = DELTA*C( I+2,J+1 )
                        F42 = DELTA*C( I+3,J+1 )
                        F33 = DELTA*C( I+2,J+2 )
                        F43 = DELTA*C( I+3,J+2 )
                        F34 = DELTA*C( I+2,J+3 )
                        F44 = DELTA*C( I+3,J+3 )

                        DO 130 L = LL, LL+LSEC-1
                           F11 = F11 + T1( L-LL+1, I-II+1 )*B( L, J )
                           F21 = F21 + T1( L-LL+1, I-II+2 )*B( L, J )
                           F12 = F12 + T1( L-LL+1, I-II+1 )*B( L, J+1 )
                           F22 = F22 + T1( L-LL+1, I-II+2 )*B( L, J+1 )
                           F13 = F13 + T1( L-LL+1, I-II+1 )*B( L, J+2 )
                           F23 = F23 + T1( L-LL+1, I-II+2 )*B( L, J+2 )
                           F14 = F14 + T1( L-LL+1, I-II+1 )*B( L, J+3 )
                           F24 = F24 + T1( L-LL+1, I-II+2 )*B( L, J+3 )
                           F31 = F31 + T1( L-LL+1, I-II+3 )*B( L, J )
                           F41 = F41 + T1( L-LL+1, I-II+4 )*B( L, J )
                           F32 = F32 + T1( L-LL+1, I-II+3 )*B( L, J+1 )
                           F42 = F42 + T1( L-LL+1, I-II+4 )*B( L, J+1 )
                           F33 = F33 + T1( L-LL+1, I-II+3 )*B( L, J+2 )
                           F43 = F43 + T1( L-LL+1, I-II+4 )*B( L, J+2 )
                           F34 = F34 + T1( L-LL+1, I-II+3 )*B( L, J+3 )
                           F44 = F44 + T1( L-LL+1, I-II+4 )*B( L, J+3 )
  130                   CONTINUE
                        C( I,J ) = F11
                        C( I+1, J ) = F21
                        C( I, J+1 ) = F12
                        C( I+1, J+1 ) = F22
                        C( I, J+2 ) = F13
                        C( I+1, J+2 ) = F23
                        C( I, J+3 ) = F14
                        C( I+1, J+3 ) = F24
                        C( I+2, J ) = F31
                        C( I+3, J ) = F41
                        C( I+2, J+1 ) = F32
                        C( I+3, J+1 ) = F42
                        C( I+2, J+2 ) = F33
                        C( I+3, J+2 ) = F43
                        C( I+2, J+3 ) = F34
                        C( I+3, J+3 ) = F44
  140                CONTINUE
                     IF( UISEC.LT.ISEC )THEN
                        DO 160 I = II+UISEC, II+ISEC-1
                           F11 = DELTA*C( I, J )
                           F12 = DELTA*C( I, J+1 )
                           F13 = DELTA*C( I, J+2 )
                           F14 = DELTA*C( I, J+3 )
!$OMP SIMD PRIVATE(TMP0,TMP1,TMP2,TMP3) REDUCTION(+:F11,F12,F13,F14)
                           DO 150 L = LL, LL+LSEC-1
                              TMP0 = T1( L-LL+1, I-II+1 )*B(L,J)
                              F11 = F11 + TMP0
                              TMP1 =  T1( L-LL+1, I-II+1 )*B( L, J+1 )
                              F12 = F12 + TMP1
                              TMP2 = T1( L-LL+1, I-II+1 )*B( L, J+2 )
                              F13 = F13 + TMP2
                              TMP3 = T1( L-LL+1, I-II+1 )*B( L, J+3 )
                              F14 = F14 + TMP3                             
  150                      CONTINUE
                           C( I, J ) = F11
                           C( I, J+1 ) = F12
                           C( I, J+2 ) = F13
                           C( I, J+3 ) = F14
  160                   CONTINUE
                     END IF
  170             CONTINUE
                  IF( UJSEC.LT.JSEC )THEN
                     TMP0 = 0.0D0
                     TMP1 = 0.0D0
                     TMP2 = 0.0D0
                     TMP3 = 0.0D0
                     DO 220 J = JJ+UJSEC, JJ+JSEC-1
                        DO 190 I = II, II+UISEC-1, 4
                           F11 = DELTA*C( I,J )
                           F21 = DELTA*C( I+1, J )
                           F31 = DELTA*C( I+2, J )
                           F41 = DELTA*C( I+3, J )
!$OMP SIMD PRIVATE(TMP0,TMP1,TMP2,TMP3) REDUCTION(+:F11,F21,F32,F41)
                           DO 180 L = LL, LL+LSEC-1
                              TMP0 = T1( L-LL+1, I-II+1 )*B( L, J )
                              F11  = F11 + TMP0
                              TMP1 = T1( L-LL+1, I-II+2 )*B( L, J )
                              F21  = F21 + TMP1
                              TMP2 = T1( L-LL+1, I-II+3 )*B( L, J )
                              F31  = F31 + TMP2
                              TMP3 = T1( L-LL+1, I-II+4 )*B( L, J )
                              F41  + F41 + TMP3
  180                      CONTINUE
                           C( I,J ) = F11
                           C( I+1, J ) = F21
                           C( I+2, J ) = F31
                           C( I+3, J ) = F41
  190                   CONTINUE
                        DO 210 I = II+UISEC, II+ISEC-1
                           F11 = DELTA*C( I, J )
                           DO 200 L = LL, LL+LSEC-1
                              F11 = F11 + T1( L-LL+1, I-II+1 )*B( L, J )
  200                      CONTINUE
                           C( I, J ) = F11
  210                   CONTINUE
  220                CONTINUE
                  END IF
  230          CONTINUE
  240       CONTINUE
  250    CONTINUE
      ELSE
*
*        Form  C := alpha*A*B' + beta*C or C := alpha*A'*B' + beta*C.
*
         C0  = ZERO
         C1  = ZERO
         C2  = ZERO
         C3  = ZERO
         C4  = ZERO
         C5  = ZERO
         C6  = ZERO
         C7  = ZERO
         C8  = ZERO
         
         DO 470 JJ = 1, N, NBT
            JSEC = MIN( NBT, N-JJ+1 )
            DO 460 LL = 1, K, KB
               LSEC = MIN( KB, K-LL+1 )
*
*              Determine if the block of C should be updated with
*              beta or not.
*
               DELTA = ONE
               IF( LL.EQ.1 ) DELTA = BETA
*
*              T2 := alpha*B', copy the transpose of a rectangular
*              block of alpha*A to T2.
*
               ULSEC = LSEC-MOD( LSEC, 2 )
               UJSEC = JSEC-MOD( JSEC, 2 )
               DO 270, L = LL, LL+ULSEC-1, 2
                  DO 260, J = JJ, JJ+UJSEC-1, 2
                     T2( L-LL+1, J-JJ+1 ) = ALPHA*B( J, L )
                     T2( L-LL+2, J-JJ+1 ) = ALPHA*B( J, L+1 )
                     T2( L-LL+1, J-JJ+2 ) = ALPHA*B( J+1, L )
                     T2( L-LL+2, J-JJ+2 ) = ALPHA*B( J+1, L+1 )
  260             CONTINUE
                  IF( UJSEC.LT.JSEC )THEN
                     T2( L-LL+1, JSEC ) = ALPHA*B( JJ+JSEC-1, L )
                     T2( L-LL+2, JSEC ) = ALPHA*B( JJ+JSEC-1, L+1 )
                  END IF
  270          CONTINUE
               IF( ULSEC.LT.LSEC )THEN
                  DO 280, J = JJ, JJ+JSEC-1
                     T2( LSEC, J-JJ+1 ) = ALPHA*B( J, LL+LSEC-1 )
  280             CONTINUE
               END IF
*
               UJSEC = JSEC-MOD( JSEC, 4 )
               DO 450 II = 1, M, MB
                  ISEC = MIN( MB, M-II+1 )
*
*                 T1 := alpha*A' or T1 := alpha*A, copy the transpose
*                 or the non-transpose of a rectangular block of
*                 alpha*A to T1.
*
                  UISEC = ISEC-MOD( ISEC, 2 )
                  IF( NOTA )THEN
                     DO 300, L = LL, LL+ULSEC-1, 2
                        DO 290, I = II, II+UISEC-1, 2
                           T1( L-LL+1, I-II+1 ) = A( I, L )
                           T1( L-LL+2, I-II+1 ) = A( I, L+1 )
                           T1( L-LL+1, I-II+2 ) = A( I+1, L )
                           T1( L-LL+2, I-II+2 ) = A( I+1, L+1 )
  290                   CONTINUE
                        IF( UISEC.LT.ISEC )THEN
                           T1( L-LL+1, ISEC ) = A( II+ISEC-1, L )
                           T1( L-LL+2, ISEC ) = A( II+ISEC-1, L+1 )
                        END IF
  300                CONTINUE
                     IF( ULSEC.LT.LSEC )THEN
                        DO 310, I = II, II+ISEC-1
                           T1( LSEC, I-II+1 ) = A( I, LL+LSEC-1 )
  310                   CONTINUE
                     END IF
                  ELSE
                     DO 330, I = II, II+UISEC-1, 2
!$OMP SIMD
                        DO 320, L = LL, LL+ULSEC-1, 2
                           T1( L-LL+1, I-II+1 ) = A( L, I )
                           T1( L-LL+1, I-II+2 ) = A( L, I+1 )
                           T1( L-LL+2, I-II+1 ) = A( L+1, I )
                           T1( L-LL+2, I-II+2 ) = A( L+1, I+1 )
  320                   CONTINUE
                        IF( ULSEC.LT.LSEC )THEN
                           T1( LSEC, I-II+1 ) = A( LL+LSEC-1, I )
                           T1( LSEC, I-II+2 ) = A( LL+LSEC-1, I+1 )
                        END IF
  330                CONTINUE
                     IF( UISEC.LT.ISEC )THEN
                        DO 340, L = LL, LL+LSEC-1
                           T1( L-LL+1, ISEC ) = A( L, II+ISEC-1 )
  340                   CONTINUE
                     END IF
                  END IF
*
*                 C := T1'*B + beta*C, update a rectangular block
*                 of C using 4 by 4 unrolling.
*
                  UISEC = ISEC-MOD( ISEC, 4 )
                  DO 390 J = JJ, JJ+UJSEC-1, 4
                     DO 360 I = II, II+UISEC-1, 4
                        F11 = DELTA*C( I,J )
                        F21 = DELTA*C( I+1,J )
                        F12 = DELTA*C( I,J+1 )
                        F22 = DELTA*C( I+1,J+1 )
                        F13 = DELTA*C( I,J+2 )
                        F23 = DELTA*C( I+1,J+2 )
                        F14 = DELTA*C( I,J+3 )
                        F24 = DELTA*C( I+1,J+3 )
                        F31 = DELTA*C( I+2,J )
                        F41 = DELTA*C( I+3,J )
                        F32 = DELTA*C( I+2,J+1 )
                        F42 = DELTA*C( I+3,J+1 )
                        F33 = DELTA*C( I+2,J+2 )
                        F43 = DELTA*C( I+3,J+2 )
                        F34 = DELTA*C( I+2,J+3 )
                        F44 = DELTA*C( I+3,J+3 )
!$OMP SIMD PRIVATE(C0,C1,C2,C3,C4,C5,C6,C7)  REDUCTION(+:F11,F21,F22,F13,F23,F14,F24,F31,F41,F32,F42,F33,F43,F34,F44)
                        DO 350 L = LL, LL+LSEC-1
                           C0  = T1(L-LL+1,I-II+1)
                           C1  = T2(L-LL+1,J-JJ+1)
                           F11 = F11 + C0 * C1
                           C2  = T1(L-LL+1,I-II+2)
                           F21 = F21 + C2 * C1
                           C3 = T2(L-LL+1,J-JJ+2)
                           F12 = F12 + C0 * C3
                           F22 = F22 + C2 * C3
                           C4 = T1(L-LL+1,I-II+3)
                           C5 = T2(L-LL+1,J-JJ+3)
                           F13 = F13 + C0 * C4
                           F23 = F23 + C2 * C4
                           C6 = T1(L-LL+1,I-II+4)
                           C7 = T2(L-LL+1,J-JJ+4)
                           F14 = F14 + C0 * C7
                           F24 = F24 + C2 * C7
                           F31 = F31 + C4 * C1
                           F41 = F41 + C6 * C1
                           F32 = F32 + C4 * C3
                           F42 = F42 + C6 * C3
                           F33 = F33 + C4 * C5
                           F43 = F43 + C6 * C5
                           F34 = F34 + C4 * C7
                           F44 = F44 + C6 * C7
                           
  350                   CONTINUE
                        C( I,J ) = F11
                        C( I+1, J ) = F21
                        C( I, J+1 ) = F12
                        C( I+1, J+1 ) = F22
                        C( I, J+2 ) = F13
                        C( I+1, J+2 ) = F23
                        C( I, J+3 ) = F14
                        C( I+1, J+3 ) = F24
                        C( I+2, J ) = F31
                        C( I+3, J ) = F41
                        C( I+2, J+1 ) = F32
                        C( I+3, J+1 ) = F42
                        C( I+2, J+2 ) = F33
                        C( I+3, J+2 ) = F43
                        C( I+2, J+3 ) = F34
                        C( I+3, J+3 ) = F44
  360                CONTINUE
                     IF( UISEC.LT.ISEC )THEN
                        DO 380 I = II+UISEC, II+ISEC-1
                           F11 = DELTA*C( I, J )
                           F12 = DELTA*C( I, J+1 )
                           F13 = DELTA*C( I, J+2 )
                           F14 = DELTA*C( I, J+3 )
!$OMP SIMD REDUCTION(+:F11,F12,F13,F14)
                           DO 370 L = LL, LL+LSEC-1
                              F11 = F11 + T1( L-LL+1, I-II+1 )* &
                                                   T2( L-LL+1, J-JJ+1 )
                              F12 = F12 + T1( L-LL+1, I-II+1 )* &
                                                   T2( L-LL+1, J-JJ+2 )
                              F13 = F13 + T1( L-LL+1, I-II+1 )* &
                                                   T2( L-LL+1, J-JJ+3 )
                              F14 = F14 + T1( L-LL+1, I-II+1 )* &
                                                   T2( L-LL+1, J-JJ+4 )
  370                      CONTINUE
                           C( I, J ) = F11
                           C( I, J+1 ) = F12
                           C( I, J+2 ) = F13
                           C( I, J+3 ) = F14
  380                   CONTINUE
                     END IF
  390             CONTINUE
                  IF( UJSEC.LT.JSEC )THEN
                     DO 440 J = JJ+UJSEC, JJ+JSEC-1
                        DO 410 I = II, II+UISEC-1, 4
                           F11 = DELTA*C( I, J )
                           F21 = DELTA*C( I+1, J )
                           F31 = DELTA*C( I+2, J )
                           F41 = DELTA*C( I+3, J )
!$OMP SIMD REDUCTION(+:F11,F21,F31,F41)
                           DO 400 L = LL, LL+LSEC-1
                              F11 = F11 + T1( L-LL+1, I-II+1 )* &
                                                   T2( L-LL+1, J-JJ+1 )
                              F21 = F21 + T1( L-LL+1, I-II+2 )* &
                                                   T2( L-LL+1, J-JJ+1 )
                              F31 = F31 + T1( L-LL+1, I-II+3 )* &
                                                   T2( L-LL+1, J-JJ+1 )
                              F41 = F41 + T1( L-LL+1, I-II+4 )* &
                                                  T2( L-LL+1, J-JJ+1 )
  400                      CONTINUE
                           C( I,J ) = F11
                           C( I+1, J ) = F21
                           C( I+2, J ) = F31
                           C( I+3, J ) = F41
  410                   CONTINUE
                        DO 430 I = II+UISEC, II+ISEC-1
                           F11 = DELTA*C( I, J )
                           DO 420 L = LL, LL+LSEC-1
                              F11 = F11 + T1( L-LL+1, I-II+1 )* &
                                                   T2( L-LL+1, J-JJ+1 )
  420                      CONTINUE
                           C( I, J ) = F11
  430                   CONTINUE
  440                CONTINUE
                  END IF
  450          CONTINUE
  460       CONTINUE
  470    CONTINUE
      END IF
*
      RETURN
*
*     End of DGEMM.
*
      END
#elif defined __ICC || defined __INTEL_COMPILER

SUBROUTINE DGEMM_Haswell_AVX2(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,&
                          BETA,C,LDC,INFO,MB,NB,NBT, KB) 
!     *     .. Scalar Arguments ..
!DIR$ ATTRIBUTES VECTOR:PROCESSOR(haswell) :: DGEMM_Haswell_AVX2
!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: DGEMM_Haswell_AVX2

      implicit none
      CHARACTER*1        TRANSA, TRANSB
      INTEGER            M, N, K, LDA, LDB, LDC, INFO
      INTEGER            MB, NB, NBT, KB 
      DOUBLE PRECISION   ALPHA, BETA
!*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), C( LDC, * )
      
#if 0
*     ..
*
*  Purpose
*  =======
*
*  DGEMM  performs one of the matrix-matrix operations
*
*     C := alpha*op( A )*op( B ) + beta*C,
*
*  where  op( X ) is one of
*
*     op( X ) = X   or   op( X ) = X',
*
*  alpha and beta are scalars, and A, B and C are matrices, with op( A )
*  an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
*
*  Parameters
*  ==========
*
*  TRANSA - CHARACTER*1.
*           On entry, TRANSA specifies the form of op( A ) to be used in
*           the matrix multiplication as follows:
*
*              TRANSA = 'N' or 'n',  op( A ) = A.
*
*              TRANSA = 'T' or 't',  op( A ) = A'.
*
*              TRANSA = 'C' or 'c',  op( A ) = A'.
*
*           Unchanged on exit.
*
*  TRANSB - CHARACTER*1.
*           On entry, TRANSB specifies the form of op( B ) to be used in
*           the matrix multiplication as follows:
*
*              TRANSB = 'N' or 'n',  op( B ) = B.
*
*              TRANSB = 'T' or 't',  op( B ) = B'.
*
*              TRANSB = 'C' or 'c',  op( B ) = B'.
*
*           Unchanged on exit.
*
*  M      - INTEGER.
*           On entry,  M  specifies  the number  of rows  of the  matrix
*           op( A )  and of the  matrix  C.  M  must  be at least  zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry,  N  specifies the number  of columns of the matrix
*           op( B ) and the number of columns of the matrix C. N must be
*           at least zero.
*           Unchanged on exit.
*
*  K      - INTEGER.
*           On entry,  K  specifies  the number of columns of the matrix
*           op( A ) and the number of rows of the matrix op( B ). K must
*           be at least  zero.
*           Unchanged on exit.
*
*  ALPHA  - DOUBLE PRECISION.
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  A      - DOUBLE PRECISION array of DIMENSION ( LDA, ka ), where ka is
*           k  when  TRANSA = 'N' or 'n',  and is  m  otherwise.
*           Before entry with  TRANSA = 'N' or 'n',  the leading  m by k
*           part of the array  A  must contain the matrix  A,  otherwise
*           the leading  k by m  part of the array  A  must contain  the
*           matrix A.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. When  TRANSA = 'N' or 'n' then
*           LDA must be at least  max( 1, m ), otherwise  LDA must be at
*           least  max( 1, k ).
*           Unchanged on exit.
*
*  B      - DOUBLE PRECISION array of DIMENSION ( LDB, kb ), where kb is
*           n  when  TRANSB = 'N' or 'n',  and is  k  otherwise.
*           Before entry with  TRANSB = 'N' or 'n',  the leading  k by n
*           part of the array  B  must contain the matrix  B,  otherwise
*           the leading  n by k  part of the array  B  must contain  the
*           matrix B.
*           Unchanged on exit.
*
*  LDB    - INTEGER.
*           On entry, LDB specifies the first dimension of B as declared
*           in the calling (sub) program. When  TRANSB = 'N' or 'n' then
*           LDB must be at least  max( 1, k ), otherwise  LDB must be at
*           least  max( 1, n ).
*           Unchanged on exit.
*
*  BETA   - DOUBLE PRECISION.
*           On entry,  BETA  specifies the scalar  beta.  When  BETA  is
*           supplied as zero then C need not be set on input.
*           Unchanged on exit.
*
*  C      - DOUBLE PRECISION array of DIMENSION ( LDC, n ).
*           Before entry, the leading  m by n  part of the array  C must
*           contain the matrix  C,  except when  beta  is zero, in which
*           case C need not be set on entry.
*           On exit, the array  C  is overwritten by the  m by n  matrix
*           ( alpha*op( A )*op( B ) + beta*C ).
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
*  -- Modified in October-1997.
*     Superscalar GEMM-Based Level 3 BLAS (Version 0.1).
*     Per Ling, Department of Computing Science,
*     Umea University, Sweden.
*
*
*     .. Local Scalars ..
*  -- Modified by Bernard Gingold in 21-02-2021  
      INTEGER            I, II, ISEC, UISEC, J, JJ, JSEC, UJSEC, &
                         L, LL, LSEC, ULSEC, NROWA, NROWB
      LOGICAL            NOTA, NOTB
      DOUBLE PRECISION   DELTA
      DOUBLE PRECISION   F11, F12, F21, F22, F31, F32, F41, F42
      DOUBLE PRECISION   F13, F14, F23, F24, F33, F34, F43, F44
      DOUBLE PRECISION   TMP0,TMP1,TMP2,TMP3
      DOUBLE PRECISION   C0,C1,C2,C3,C4,C5,C6,C7,C8
                         
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN, MOD
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      !EXTERNAL           XERBLA
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER        ( ZERO = 0.0D+0, ONE = 1.0D+0 )
!*     .. User specified parameters for DGEMM ..
!      Moved to subroutine arguments.
     ! PARAMETER        ( MB = 32, NB = 1024, NBT = 96, KB = 32 )
      DOUBLE PRECISION   T1( KB, MB ), T2( KB, NBT )
      !DIR$ ATTRIBUTES ALIGN : 32 :: T1,T2
*     ..
*     .. Executable Statements ..
*
*     Set  NOTA  and  NOTB  as  true if  A  and  B  respectively are not
*     transposed and set NROWA and NROWB as the number of rows of A  and
*     the number of rows of B respectively.
*
#endif
      NOTA = LSAME( TRANSA, 'N' )
      NOTB = LSAME( TRANSB, 'N' )
      IF ( NOTA ) THEN
         NROWA = M
      ELSE
         NROWA = K
      END IF
      IF ( NOTB ) THEN
         NROWB = K
      ELSE
         NROWB = N
      END IF
*
*     Test the input parameters.
*
      INFO = 0
      IF( ( .NOT.NOTA ).AND.( .NOT. LSAME( TRANSA,'C' ) )
     $                          .AND.( .NOT.LSAME( TRANSA, 'T' ) ) )THEN
         INFO = 1
      ELSE IF( ( .NOT.NOTB ).AND.( .NOT.LSAME( TRANSB, 'C' ) )
     $                          .AND.( .NOT.LSAME( TRANSB, 'T' ) ) )THEN
         INFO = 2
      ELSE IF( M.LT.0 )THEN
         INFO = 3
      ELSE IF( N.LT.0 )THEN
         INFO = 4
      ELSE IF( K.LT.0 )THEN
         INFO = 5
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 8
      ELSE IF( LDB.LT.MAX( 1, NROWB ) )THEN
         INFO = 10
      ELSE IF( LDC.LT.MAX( 1, M ) )THEN
         INFO = 13
      END IF
      IF( INFO.NE.0 )THEN
!CALL XERBLA( 'DGEMM ', INFO )
          RETURN
      END IF
*
*     Quick return if possible.
*
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.
     $     ( ( ( ALPHA.EQ.ZERO ).OR.( K.EQ.0 ) ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
*
*     And when alpha.eq.zero.
*
      IF( ( ALPHA.EQ.ZERO ).OR.( K.EQ.0 ) )THEN
         IF( BETA.EQ.ZERO )THEN
            UISEC = M-MOD( M, 4 )
            DO 30, J = 1, N
               DO 10, I = 1, UISEC, 4
                  C( I, J ) = ZERO
                  C( I+1, J ) = ZERO
                  C( I+2, J ) = ZERO
                  C( I+3, J ) = ZERO
   10          CONTINUE
               DO 20, I = UISEC+1, M
                  C( I, J ) = ZERO
   20          CONTINUE
   30       CONTINUE
         ELSE
            UISEC = M-MOD( M, 4 )
            DO 60, J = 1, N
               DO 40, I = 1, UISEC, 4
                  C( I, J ) = BETA*C( I, J )
                  C( I+1, J ) = BETA*C( I+1, J )
                  C( I+2, J ) = BETA*C( I+2, J )
                  C( I+3, J ) = BETA*C( I+3, J )
   40          CONTINUE
               DO 50, I = UISEC+1, M
                  C( I, J ) = BETA*C( I, J )
   50          CONTINUE
   60       CONTINUE
         END IF
         RETURN
      END IF
      T1(:,:) = ZERO
      T2(:,:) = ZERO
*
*     Start the operations.
*     
      IF (NOTB) THEN
*
*        Form  C := alpha*A*B + beta*C or C := alpha*A'*B + beta*C.
*
         TMP0 = 0.0D0
         TMP1 = 0.0D0
         TMP2 = 0.0D0
         TMP3 = 0.0D0
         DO 250 JJ = 1, N, NB
            JSEC = MIN( NB, N-JJ+1 )
            UJSEC = JSEC-MOD( JSEC, 4 )
            DO 240 LL = 1, K, KB
               LSEC = MIN( KB, K-LL+1 )
               ULSEC = LSEC-MOD( LSEC, 2 )
*
*              Determine if the block of C should be updated with
*              beta or not.
*
               DELTA = ONE
               IF( LL.EQ.1 ) DELTA = BETA
*
               DO 230 II = 1, M, MB
                  ISEC = MIN( MB, M-II+1 )
*
*                 T1 := alpha*A' or T1 := alpha*A, copy the transpose
*                 or the non-transpose of a rectangular block of
*                 alpha*A to T1.
*
                  UISEC = ISEC-MOD( ISEC, 2 )
                  IF( NOTA )THEN
                     DO 80, L = LL, LL+ULSEC-1, 2
                        DO 70, I = II, II+UISEC-1, 2
                           T1( L-LL+1, I-II+1 ) = ALPHA*A( I, L )
                           T1( L-LL+2, I-II+1 ) = ALPHA*A( I, L+1 )
                           T1( L-LL+1, I-II+2 ) = ALPHA*A( I+1, L )
                           T1( L-LL+2, I-II+2 ) = ALPHA*A( I+1, L+1 )
   70                   CONTINUE
                        IF( UISEC.LT.ISEC )THEN
                           T1( L-LL+1, ISEC ) = ALPHA*A( II+ISEC-1, L )
                           T1( L-LL+2, ISEC ) = ALPHA*A( II+ISEC-1, L+1)
                                             
                        END IF
   80                CONTINUE
                     IF( ULSEC.LT.LSEC )THEN
                        DO 90, I = II, II+ISEC-1
                           T1( LSEC, I-II+1 ) = ALPHA*A( I, LL+LSEC-1 )
   90                   CONTINUE
                     END IF
                  ELSE
                     DO 110, I = II, II+UISEC-1, 2
!$OMP SIMD 
                        DO 100, L = LL, LL+ULSEC-1, 2
                           T1( L-LL+1, I-II+1 )  = ALPHA*A( L, I )
                           T1( L-LL+1, I-II+2 ) = ALPHA*A( L, I+1 )
                           T1( L-LL+2, I-II+1 ) = ALPHA*A( L+1, I )
                           T1( L-LL+2, I-II+2 ) = ALPHA*A( L+1, I+1 )
  100                   CONTINUE
                        IF( ULSEC.LT.LSEC )THEN
                           T1( LSEC, I-II+1 ) = ALPHA*A( LL+LSEC-1, I )
                           T1( LSEC, I-II+2 ) = ALPHA*A( LL+LSEC-1, I+1)
                                             
                        END IF
  110                CONTINUE
                     IF( UISEC.LT.ISEC )THEN
                        DO 120, L = LL, LL+LSEC-1
                           T1( L-LL+1, ISEC ) = ALPHA*A( L, II+ISEC-1 )
  120                   CONTINUE
                     END IF
                  END IF
*
*                 C := T1'*B + beta*C, update a rectangular block
*                 of C using 4 by 4 unrolling.
*
                  UISEC = ISEC-MOD( ISEC, 4 )
                  DO 170 J = JJ, JJ+UJSEC-1, 4
                     DO 140 I = II, II+UISEC-1, 4
                        F11 = DELTA*C( I,J )
                        F21 = DELTA*C( I+1,J )
                        F12 = DELTA*C( I,J+1 )
                        F22 = DELTA*C( I+1,J+1 )
                        F13 = DELTA*C( I,J+2 )
                        F23 = DELTA*C( I+1,J+2 )
                        F14 = DELTA*C( I,J+3 )
                        F24 = DELTA*C( I+1,J+3 )
                        F31 = DELTA*C( I+2,J )
                        F41 = DELTA*C( I+3,J )
                        F32 = DELTA*C( I+2,J+1 )
                        F42 = DELTA*C( I+3,J+1 )
                        F33 = DELTA*C( I+2,J+2 )
                        F43 = DELTA*C( I+3,J+2 )
                        F34 = DELTA*C( I+2,J+3 )
                        F44 = DELTA*C( I+3,J+3 )

                        DO 130 L = LL, LL+LSEC-1
                           !call _mm_prefetch(T1(L+32,I),FOR_K_PREFETCH_T1,.false.)
                           F11 = F11 + T1( L-LL+1, I-II+1 )*B( L, J )
                           F21 = F21 + T1( L-LL+1, I-II+2 )*B( L, J )
                           F12 = F12 + T1( L-LL+1, I-II+1 )*B( L, J+1 )
                           F22 = F22 + T1( L-LL+1, I-II+2 )*B( L, J+1 )
                           F13 = F13 + T1( L-LL+1, I-II+1 )*B( L, J+2 )
                           F23 = F23 + T1( L-LL+1, I-II+2 )*B( L, J+2 )
                           F14 = F14 + T1( L-LL+1, I-II+1 )*B( L, J+3 )
                           F24 = F24 + T1( L-LL+1, I-II+2 )*B( L, J+3 )
                           F31 = F31 + T1( L-LL+1, I-II+3 )*B( L, J )
                           F41 = F41 + T1( L-LL+1, I-II+4 )*B( L, J )
                           F32 = F32 + T1( L-LL+1, I-II+3 )*B( L, J+1 )
                           F42 = F42 + T1( L-LL+1, I-II+4 )*B( L, J+1 )
                           F33 = F33 + T1( L-LL+1, I-II+3 )*B( L, J+2 )
                           F43 = F43 + T1( L-LL+1, I-II+4 )*B( L, J+2 )
                           F34 = F34 + T1( L-LL+1, I-II+3 )*B( L, J+3 )
                           F44 = F44 + T1( L-LL+1, I-II+4 )*B( L, J+3 )
  130                   CONTINUE
                        C( I,J ) = F11
                        C( I+1, J ) = F21
                        C( I, J+1 ) = F12
                        C( I+1, J+1 ) = F22
                        C( I, J+2 ) = F13
                        C( I+1, J+2 ) = F23
                        C( I, J+3 ) = F14
                        C( I+1, J+3 ) = F24
                        C( I+2, J ) = F31
                        C( I+3, J ) = F41
                        C( I+2, J+1 ) = F32
                        C( I+3, J+1 ) = F42
                        C( I+2, J+2 ) = F33
                        C( I+3, J+2 ) = F43
                        C( I+2, J+3 ) = F34
                        C( I+3, J+3 ) = F44
  140                CONTINUE
                     IF( UISEC.LT.ISEC )THEN
                        DO 160 I = II+UISEC, II+ISEC-1
                           F11 = DELTA*C( I, J )
                           F12 = DELTA*C( I, J+1 )
                           F13 = DELTA*C( I, J+2 )
                           F14 = DELTA*C( I, J+3 )
!$OMP SIMD PRIVATE(TMP0,TMP1,TMP2,TMP3) REDUCTION(+:F11,F12,F13,F14)
                           DO 150 L = LL, LL+LSEC-1
                              TMP0 = T1( L-LL+1, I-II+1 )*B(L,J)
                              F11 = F11 + TMP0
                              TMP1 =  T1( L-LL+1, I-II+1 )*B( L, J+1 )
                              F12 = F12 + TMP1
                              TMP2 = T1( L-LL+1, I-II+1 )*B( L, J+2 )
                              F13 = F13 + TMP2
                              TMP3 = T1( L-LL+1, I-II+1 )*B( L, J+3 )
                              F14 = F14 + TMP3                             
  150                      CONTINUE
                           C( I, J ) = F11
                           C( I, J+1 ) = F12
                           C( I, J+2 ) = F13
                           C( I, J+3 ) = F14
  160                   CONTINUE
                     END IF
  170             CONTINUE
                  IF( UJSEC.LT.JSEC )THEN
                     TMP0 = 0.0D0
                     TMP1 = 0.0D0
                     TMP2 = 0.0D0
                     TMP3 = 0.0D0
                     DO 220 J = JJ+UJSEC, JJ+JSEC-1
                        DO 190 I = II, II+UISEC-1, 4
                           F11 = DELTA*C( I,J )
                           F21 = DELTA*C( I+1, J )
                           F31 = DELTA*C( I+2, J )
                           F41 = DELTA*C( I+3, J )
!$OMP SIMD PRIVATE(TMP0,TMP1,TMP2,TMP3) REDUCTION(+:F11,F21,F32,F41)
                           DO 180 L = LL, LL+LSEC-1
                              TMP0 = T1( L-LL+1, I-II+1 )*B( L, J )
                              F11  = F11 + TMP0
                              TMP1 = T1( L-LL+1, I-II+2 )*B( L, J )
                              F21  = F21 + TMP1
                              TMP2 = T1( L-LL+1, I-II+3 )*B( L, J )
                              F31  = F31 + TMP2
                              TMP3 = T1( L-LL+1, I-II+4 )*B( L, J )
                              F41  + F41 + TMP3
  180                      CONTINUE
                           C( I,J ) = F11
                           C( I+1, J ) = F21
                           C( I+2, J ) = F31
                           C( I+3, J ) = F41
  190                   CONTINUE
                        DO 210 I = II+UISEC, II+ISEC-1
                           F11 = DELTA*C( I, J )
                           DO 200 L = LL, LL+LSEC-1
                              F11 = F11 + T1( L-LL+1, I-II+1 )*B( L, J )
  200                      CONTINUE
                           C( I, J ) = F11
  210                   CONTINUE
  220                CONTINUE
                  END IF
  230          CONTINUE
  240       CONTINUE
  250    CONTINUE
      ELSE
*
*        Form  C := alpha*A*B' + beta*C or C := alpha*A'*B' + beta*C.
*
         C0  = ZERO
         C1  = ZERO
         C2  = ZERO
         C3  = ZERO
         C4  = ZERO
         C5  = ZERO
         C6  = ZERO
         C7  = ZERO
         C8  = ZERO
         
         DO 470 JJ = 1, N, NBT
            JSEC = MIN( NBT, N-JJ+1 )
            DO 460 LL = 1, K, KB
               LSEC = MIN( KB, K-LL+1 )
*
*              Determine if the block of C should be updated with
*              beta or not.
*
               DELTA = ONE
               IF( LL.EQ.1 ) DELTA = BETA
*
*              T2 := alpha*B', copy the transpose of a rectangular
*              block of alpha*A to T2.
*
               ULSEC = LSEC-MOD( LSEC, 2 )
               UJSEC = JSEC-MOD( JSEC, 2 )
               DO 270, L = LL, LL+ULSEC-1, 2
                  DO 260, J = JJ, JJ+UJSEC-1, 2
                     T2( L-LL+1, J-JJ+1 ) = ALPHA*B( J, L )
                     T2( L-LL+2, J-JJ+1 ) = ALPHA*B( J, L+1 )
                     T2( L-LL+1, J-JJ+2 ) = ALPHA*B( J+1, L )
                     T2( L-LL+2, J-JJ+2 ) = ALPHA*B( J+1, L+1 )
  260             CONTINUE
                  IF( UJSEC.LT.JSEC )THEN
                     T2( L-LL+1, JSEC ) = ALPHA*B( JJ+JSEC-1, L )
                     T2( L-LL+2, JSEC ) = ALPHA*B( JJ+JSEC-1, L+1 )
                  END IF
  270          CONTINUE
               IF( ULSEC.LT.LSEC )THEN
                  DO 280, J = JJ, JJ+JSEC-1
                     T2( LSEC, J-JJ+1 ) = ALPHA*B( J, LL+LSEC-1 )
  280             CONTINUE
               END IF
*
               UJSEC = JSEC-MOD( JSEC, 4 )
               DO 450 II = 1, M, MB
                  ISEC = MIN( MB, M-II+1 )
*
*                 T1 := alpha*A' or T1 := alpha*A, copy the transpose
*                 or the non-transpose of a rectangular block of
*                 alpha*A to T1.
*
                  UISEC = ISEC-MOD( ISEC, 2 )
                  IF( NOTA )THEN
                     DO 300, L = LL, LL+ULSEC-1, 2
                        DO 290, I = II, II+UISEC-1, 2
                           T1( L-LL+1, I-II+1 ) = A( I, L )
                           T1( L-LL+2, I-II+1 ) = A( I, L+1 )
                           T1( L-LL+1, I-II+2 ) = A( I+1, L )
                           T1( L-LL+2, I-II+2 ) = A( I+1, L+1 )
  290                   CONTINUE
                        IF( UISEC.LT.ISEC )THEN
                           T1( L-LL+1, ISEC ) = A( II+ISEC-1, L )
                           T1( L-LL+2, ISEC ) = A( II+ISEC-1, L+1 )
                        END IF
  300                CONTINUE
                     IF( ULSEC.LT.LSEC )THEN
                        DO 310, I = II, II+ISEC-1
                           T1( LSEC, I-II+1 ) = A( I, LL+LSEC-1 )
  310                   CONTINUE
                     END IF
                  ELSE
                     DO 330, I = II, II+UISEC-1, 2
!$OMP SIMD
                        DO 320, L = LL, LL+ULSEC-1, 2
                           T1( L-LL+1, I-II+1 ) = A( L, I )
                           T1( L-LL+1, I-II+2 ) = A( L, I+1 )
                           T1( L-LL+2, I-II+1 ) = A( L+1, I )
                           T1( L-LL+2, I-II+2 ) = A( L+1, I+1 )
  320                   CONTINUE
                        IF( ULSEC.LT.LSEC )THEN
                           T1( LSEC, I-II+1 ) = A( LL+LSEC-1, I )
                           T1( LSEC, I-II+2 ) = A( LL+LSEC-1, I+1 )
                        END IF
  330                CONTINUE
                     IF( UISEC.LT.ISEC )THEN
                        DO 340, L = LL, LL+LSEC-1
                           T1( L-LL+1, ISEC ) = A( L, II+ISEC-1 )
  340                   CONTINUE
                     END IF
                  END IF
*
*                 C := T1'*B + beta*C, update a rectangular block
*                 of C using 4 by 4 unrolling.
*
                  UISEC = ISEC-MOD( ISEC, 4 )
                  DO 390 J = JJ, JJ+UJSEC-1, 4
                     DO 360 I = II, II+UISEC-1, 4
                        F11 = DELTA*C( I,J )
                        F21 = DELTA*C( I+1,J )
                        F12 = DELTA*C( I,J+1 )
                        F22 = DELTA*C( I+1,J+1 )
                        F13 = DELTA*C( I,J+2 )
                        F23 = DELTA*C( I+1,J+2 )
                        F14 = DELTA*C( I,J+3 )
                        F24 = DELTA*C( I+1,J+3 )
                        F31 = DELTA*C( I+2,J )
                        F41 = DELTA*C( I+3,J )
                        F32 = DELTA*C( I+2,J+1 )
                        F42 = DELTA*C( I+3,J+1 )
                        F33 = DELTA*C( I+2,J+2 )
                        F43 = DELTA*C( I+3,J+2 )
                        F34 = DELTA*C( I+2,J+3 )
                        F44 = DELTA*C( I+3,J+3 )
!$OMP SIMD PRIVATE(C0,C1,C2,C3,C4,C5,C6,C7)  REDUCTION(+:F11,F21,F22,F13,F23,F14,F24,F31,F41,F32,F42,F33,F43,F34,F44)
                        DO 350 L = LL, LL+LSEC-1
                           C0  = T1(L-LL+1,I-II+1)
                           C1  = T2(L-LL+1,J-JJ+1)
                           F11 = F11 + C0 * C1
                           C2  = T1(L-LL+1,I-II+2)
                           F21 = F21 + C2 * C1
                           C3 = T2(L-LL+1,J-JJ+2)
                           F12 = F12 + C0 * C3
                           F22 = F22 + C2 * C3
                           C4 = T1(L-LL+1,I-II+3)
                           C5 = T2(L-LL+1,J-JJ+3)
                           F13 = F13 + C0 * C4
                           F23 = F23 + C2 * C4
                           C6 = T1(L-LL+1,I-II+4)
                           C7 = T2(L-LL+1,J-JJ+4)
                           F14 = F14 + C0 * C7
                           F24 = F24 + C2 * C7
                           F31 = F31 + C4 * C1
                           F41 = F41 + C6 * C1
                           F32 = F32 + C4 * C3
                           F42 = F42 + C6 * C3
                           F33 = F33 + C4 * C5
                           F43 = F43 + C6 * C5
                           F34 = F34 + C4 * C7
                           F44 = F44 + C6 * C7
                           
  350                   CONTINUE
                        C( I,J ) = F11
                        C( I+1, J ) = F21
                        C( I, J+1 ) = F12
                        C( I+1, J+1 ) = F22
                        C( I, J+2 ) = F13
                        C( I+1, J+2 ) = F23
                        C( I, J+3 ) = F14
                        C( I+1, J+3 ) = F24
                        C( I+2, J ) = F31
                        C( I+3, J ) = F41
                        C( I+2, J+1 ) = F32
                        C( I+3, J+1 ) = F42
                        C( I+2, J+2 ) = F33
                        C( I+3, J+2 ) = F43
                        C( I+2, J+3 ) = F34
                        C( I+3, J+3 ) = F44
  360                CONTINUE
                     IF( UISEC.LT.ISEC )THEN
                        DO 380 I = II+UISEC, II+ISEC-1
                           F11 = DELTA*C( I, J )
                           F12 = DELTA*C( I, J+1 )
                           F13 = DELTA*C( I, J+2 )
                           F14 = DELTA*C( I, J+3 )
!$OMP SIMD REDUCTION(+:F11,F12,F13,F14)
                           DO 370 L = LL, LL+LSEC-1
                              F11 = F11 + T1( L-LL+1, I-II+1 )* &
                                                   T2( L-LL+1, J-JJ+1 )
                              F12 = F12 + T1( L-LL+1, I-II+1 )* &
                                                   T2( L-LL+1, J-JJ+2 )
                              F13 = F13 + T1( L-LL+1, I-II+1 )* &
                                                   T2( L-LL+1, J-JJ+3 )
                              F14 = F14 + T1( L-LL+1, I-II+1 )* &
                                                   T2( L-LL+1, J-JJ+4 )
  370                      CONTINUE
                           C( I, J ) = F11
                           C( I, J+1 ) = F12
                           C( I, J+2 ) = F13
                           C( I, J+3 ) = F14
  380                   CONTINUE
                     END IF
  390             CONTINUE
                  IF( UJSEC.LT.JSEC )THEN
                     DO 440 J = JJ+UJSEC, JJ+JSEC-1
                        DO 410 I = II, II+UISEC-1, 4
                           F11 = DELTA*C( I, J )
                           F21 = DELTA*C( I+1, J )
                           F31 = DELTA*C( I+2, J )
                           F41 = DELTA*C( I+3, J )
!$OMP SIMD REDUCTION(+:F11,F21,F31,F41)
                           DO 400 L = LL, LL+LSEC-1
                              F11 = F11 + T1( L-LL+1, I-II+1 )* &
                                                   T2( L-LL+1, J-JJ+1 )
                              F21 = F21 + T1( L-LL+1, I-II+2 )* &
                                                   T2( L-LL+1, J-JJ+1 )
                              F31 = F31 + T1( L-LL+1, I-II+3 )* &
                                                   T2( L-LL+1, J-JJ+1 )
                              F41 = F41 + T1( L-LL+1, I-II+4 )* &
                                                  T2( L-LL+1, J-JJ+1 )
  400                      CONTINUE
                           C( I,J ) = F11
                           C( I+1, J ) = F21
                           C( I+2, J ) = F31
                           C( I+3, J ) = F41
  410                   CONTINUE
                        DO 430 I = II+UISEC, II+ISEC-1
                           F11 = DELTA*C( I, J )
                           DO 420 L = LL, LL+LSEC-1
                              F11 = F11 + T1( L-LL+1, I-II+1 )* &
                                                   T2( L-LL+1, J-JJ+1 )
  420                      CONTINUE
                           C( I, J ) = F11
  430                   CONTINUE
  440                CONTINUE
                  END IF
  450          CONTINUE
  460       CONTINUE
  470    CONTINUE
      END IF
*
      RETURN
*
*     End of DGEMM.
*
      END


#endif
      


