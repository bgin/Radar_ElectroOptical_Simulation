

#if !defined(__GMS_LAPACK_USE_OMP__)
#defined __GMS_LAPACK_USE_OMP__ 1
#endif
  
#if 0

*
*  Arguments:
*  ==========
*
*> \param[in] NORM
*> \verbatim
*>          NORM is CHARACTER*1
*>          Specifies the value to be returned in ZLANGE as described
*>          above.
*> \endverbatim
*>
*> \param[in] M
*> \verbatim
*>          M is INTEGER
*>          The number of rows of the matrix A.  M >= 0.  When M = 0,
*>          ZLANGE is set to zero.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The number of columns of the matrix A.  N >= 0.  When N = 0,
*>          ZLANGE is set to zero.
*> \endverbatim
*>
*> \param[in] A
*> \verbatim
*>          A is COMPLEX*16 array, dimension (LDA,N)
*>          The m by n matrix A.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the array A.  LDA >= max(M,1).
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is DOUBLE PRECISION array, dimension (MAX(1,LWORK)),
*>          where LWORK >= M when NORM = 'I'; otherwise, WORK is not
*>          referenced.
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \date December 2016
*
*> \ingroup complex16GEauxiliary
*
*  =====================================================================
#endif
#if defined(__GFORTRAN__) && (!defined(__ICC) || !defined(__INTEL_COMPILER))
DOUBLE PRECISION FUNCTION ZLANGE(NORM, M, N, A, LDA, WORK) !GCC$ ATTRIBUTES HOT :: ZLANGE !GCC$ ATTRIBUTES aligned(32) :: ZLANGE !GCC$ ATTRIBUTES no_stack_protector :: ZLANGE
#elif defined(__ICC) || defined(__INTEL_COMPILER)
  DOUBLE PRECISION FUNCTION ZLANGE(NORM,M,N,A,LDA,WORK)
    !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ZLANGE
    !DIR$ OPTIMIZE : 3
    !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: ZLANGE
#endif
!*
!*  -- LAPACK auxiliary routine (version 3.7.0) --
!*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!*     December 2016
    !*

    use omp_lib

      IMPLICIT NONE
!*     .. Scalar Arguments ..
      CHARACTER          NORM
      INTEGER            LDA, M, N
!*     ..
      !*     .. Array Arguments ..
      
      !DOUBLE PRECISION   WORK( * )
      !COMPLEX*16         A( LDA, * )
      DOUBLE PRECISION, DIMENSION(:),   ALLOCATABLE :: WORK
      COMPLEX(16),      DIMENSION(:,:), ALLOCATABLE :: A
!#elif defined(__ICC) || defined(__INTEL_COMPILER)
!      DOUBLE PRECISION  WORK(*)
!      COMPLEX*16         A( LDA, * )
!      !DIR$ ASSUME_ALIGNED WORK:64
!      !DIR$ ASSUME_ALIGNED A:64
!#endif
!*     ..
!*
!* =====================================================================
!*
!*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!*     ..
!*     .. Local Scalars ..
      INTEGER            I, J
      DOUBLE PRECISION   SUM, VALUE, TEMP
!*     ..
!*     .. Local Arrays ..
      DOUBLE PRECISION   SSQ( 2 ), COLSSQ( 2 )
!*     ..
!*     .. External Functions ..
      LOGICAL            LSAME, DISNAN
      EXTERNAL           LSAME, DISNAN
!*     ..
!*     .. External Subroutines ..
!      EXTERNAL           ZLASSQ, DCOMBSSQ
!*     ..
!*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MIN, SQRT
!*     ..
!*     .. Executable Statements ..
!*
      IF( MIN( M, N ).EQ.0 ) THEN
         VALUE = ZERO
      ELSE IF( LSAME( NORM, 'M' ) ) THEN
!*
!*        Find max(abs(A(i,j))).
!*
         VALUE = ZERO
         DO 20 J = 1, N
            DO 10 I = 1, M
               TEMP = ABS( A( I, J ) )
               IF( VALUE.LT.TEMP .OR. DISNAN( TEMP ) ) VALUE = TEMP
   10       CONTINUE
   20    CONTINUE
      ELSE IF( ( LSAME( NORM, 'O' ) ) .OR. ( NORM.EQ.'1' ) ) THEN
!*
!*        Find norm1(A).
!*
         VALUE = ZERO
#if (__GMS_LAPACK_USE_OMP__) == 1
         !$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(SHARED) PRIVATE(J,SUM)
#endif
         DO 40 J = 1, N
            SUM = ZERO
            !$OMP SIMD REDUCTION(+:SUM) ALIGNED(A:64) LINEAR(I:1)
            DO 30 I = 1, M
               SUM = SUM + ABS( A( I, J ) )
   30       CONTINUE
            IF( VALUE.LT.SUM .OR. DISNAN( SUM ) ) VALUE = SUM
   40    CONTINUE
      ELSE IF( LSAME( NORM, 'I' ) ) THEN
!*
!*        Find normI(A).
         !*

         !$OMP SIMD ALIGNED(WORK:64) LINEAR(I:1)
         DO 50 I = 1, M
            WORK( I ) = ZERO
50       CONTINUE
#if (__GMS_LAPACK_USE_OMP__) == 1
         !$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(SHARED) PRIVATE(J)
#endif            
         DO 70 J = 1, N
            !$OMP SIMD REDUCTION(+:WORK) ALIGNED(WORK:64,A:64)
            DO 60 I = 1, M
               WORK( I ) = WORK( I ) + ABS( A( I, J ) )
   60       CONTINUE
   70    CONTINUE
         VALUE = ZERO
         DO 80 I = 1, M
            TEMP = WORK( I )
            IF( VALUE.LT.TEMP .OR. DISNAN( TEMP ) ) VALUE = TEMP
   80    CONTINUE
      ELSE IF( ( LSAME( NORM, 'F' ) ) .OR. ( LSAME( NORM, 'E' ) ) ) THEN
!*
!*        Find normF(A).
!*        SSQ(1) is scale
!*        SSQ(2) is sum-of-squares
!*        For better accuracy, sum each column separately.
!*
         SSQ( 1 ) = ZERO
         SSQ( 2 ) = ONE
         DO 90 J = 1, N
            COLSSQ( 1 ) = ZERO
            COLSSQ( 2 ) = ONE
            CALL ZLASSQ( M, A( 1, J ), 1, COLSSQ( 1 ), COLSSQ( 2 ) )
            CALL DCOMBSSQ( SSQ, COLSSQ )
   90    CONTINUE
         VALUE = SSQ( 1 )*SQRT( SSQ( 2 ) )
      END IF

      ZLANGE = VALUE
END FUNCTION

#if 0
*  Arguments:
*  ==========
*
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The number of elements to be used from the vector X.
*> \endverbatim
*>
*> \param[in] X
*> \verbatim
*>          X is COMPLEX*16 array, dimension (1+(N-1)*INCX)
*>          The vector x as described above.
*>             x( i )  = X( 1 + ( i - 1 )*INCX ), 1 <= i <= n.
*> \endverbatim
*>
*> \param[in] INCX
*> \verbatim
*>          INCX is INTEGER
*>          The increment between successive values of the vector X.
*>          INCX > 0.
*> \endverbatim
*>
*> \param[in,out] SCALE
*> \verbatim
*>          SCALE is DOUBLE PRECISION
*>          On entry, the value  scale  in the equation above.
*>          On exit, SCALE is overwritten with the value  scl .
*> \endverbatim
*>
*> \param[in,out] SUMSQ
*> \verbatim
*>          SUMSQ is DOUBLE PRECISION
*>          On entry, the value  sumsq  in the equation above.
*>          On exit, SUMSQ is overwritten with the value  ssq .
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \date December 2016
*
*> \ingroup complex16OTHERauxiliary
*
*  =====================================================================
#endif
#if defined(__GFORTRAN__) && (!defined(__ICC) || !defined(__INTEL_COMPILER))
SUBROUTINE ZLASSQ( N, X, INCX, SCALE, SUMSQ ) !GCC$ ATTRIBUTES INLINE :: ZLASSQ !GCC$ ATTRIBUTES aligned(32) :: ZLASSQ
#elif defined(__ICC) || defined(__INTEL_COMPILER)
  SUBROUTINE ZLASSQ(N,X,INCX,SCALE,SUMSQ)
    !DIR$ ATTRIBUTES FORCEINLINE :: ZLASSQ
    !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ZLASSQ
    !DIR$ OPTIMIZE : 3
#endif
     implicit none
!*
!*  -- LAPACK auxiliary routine (version 3.7.0) --
!*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!*     December 2016
!*
!*     .. Scalar Arguments ..
      INTEGER            INCX, N
      DOUBLE PRECISION   SCALE, SUMSQ
!*     ..
!*     .. Array Arguments ..
      COMPLEX*16         X( * )
!*     ..
!*
!* =====================================================================
!*
!*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
!*     ..
!*     .. Local Scalars ..
      INTEGER            IX
      DOUBLE PRECISION   TEMP1,TMP
!*     ..
!*     .. External Functions ..
      LOGICAL            DISNAN
      EXTERNAL           DISNAN
!*     ..
!*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, DIMAG
!*     ..
!*     .. Executable Statements ..
!*
      IF( N.GT.0 ) THEN
         DO 10 IX = 1, 1 + ( N-1 )*INCX, INCX
            TEMP1 = ABS( DBLE( X( IX ) ) )
            IF( TEMP1.GT.ZERO.OR.DISNAN( TEMP1 ) ) THEN
               IF( SCALE.LT.TEMP1 ) THEN
                  TMP = SCALE/TEMP1
                  !SUMSQ = 1 + SUMSQ*( SCALE / TEMP1 )**2
                  SUMSQ = 1+SUMSQ*(TMP*TMP)
                  SCALE = TEMP1
               ELSE
                  TMP = TEMP1/SCALE
                  !SUMSQ = SUMSQ + ( TEMP1 / SCALE )**2
                  SUMSQ = SUMSQ+TMP*TMP
               END IF
            END IF
            TEMP1 = ABS( DIMAG( X( IX ) ) )
            IF( TEMP1.GT.ZERO.OR.DISNAN( TEMP1 ) ) THEN
               IF( SCALE.LT.TEMP1 ) THEN
                  TMP = SCALE/TEMP1
                  !SUMSQ = 1 + SUMSQ*( SCALE / TEMP1 )**2
                  SUMSQ = 1+SUMSQ*(TMP*TMP)
                  SCALE = TEMP1
               ELSE
                  TMP = TEMP1/SCALE
                  SUMSQ = SUMSQ + TMP*TMP
               END IF
            END IF
   10    CONTINUE
      END IF

END SUBROUTINE

#if 0
*  Arguments:
*  ==========
*
*> \param[in,out] V1
*> \verbatim
*>          V1 is DOUBLE PRECISION array, dimension (2).
*>          The first scaled sum.
*>          V1(1) = V1_scale, V1(2) = V1_sumsq.
*> \endverbatim
*>
*> \param[in] V2
*> \verbatim
*>          V2 is DOUBLE PRECISION array, dimension (2).
*>          The second scaled sum.
*>          V2(1) = V2_scale, V2(2) = V2_sumsq.
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \date November 2018
*
*> \ingroup OTHERauxiliary
*
*  =====================================================================
#endif
#if defined(__GFORTRAN__) && (!defined(__ICC) || !defined(__INTEL_COMPILER))
SUBROUTINE DCOMBSSQ( V1, V2 ) !GCC$ ATTRIBUTES INLINE :: DCOMBSSQ !GCC$ ATTRIBUTES aligned(32) :: DCOMBSSQ
#elif defined(__ICC) || defined(__INTEL_COMPILER)
  SUBROUTINE DCOMBSSQ(V1,V2)
    !DIR$ ATTRIBUTES FORCEINLINE :: DCOMBSSQ
    !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: DCOMBSSQ
    !DIR$ OPTIMIZE : 3
#endif
!*
!*  -- LAPACK auxiliary routine (version 3.7.0) --
!*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!*!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!*     November 2018
!*
!*     .. Array Arguments ..
      DOUBLE PRECISION   V1( 2 ), V2( 2 )
!*     ..
!*
!* =====================================================================
!*
!*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
!*     ..
!*     .. Executable Statements ..
!*
      IF( V1( 1 ).GE.V2( 1 ) ) THEN
         IF( V1( 1 ).NE.ZERO ) THEN
            V1( 2 ) = V1( 2 ) + ( V2( 1 ) / V1( 1 ) )**2 * V2( 2 )
         END IF
      ELSE
         V1( 2 ) = V2( 2 ) + ( V1( 1 ) / V2( 1 ) )**2 * V1( 2 )
         V1( 1 ) = V2( 1 )
      END IF
    
END SUBROUTINE 


#if 0
*> ZLACPY copies all or part of a two-dimensional matrix A to another
*> matrix B.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] UPLO
*> \verbatim
*>          UPLO is CHARACTER*1
*>          Specifies the part of the matrix A to be copied to B.
*>          = 'U':      Upper triangular part
*>          = 'L':      Lower triangular part
*>          Otherwise:  All of the matrix A
*> \endverbatim
*>
*> \param[in] M
*> \verbatim
*>          M is INTEGER
*>          The number of rows of the matrix A.  M >= 0.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The number of columns of the matrix A.  N >= 0.
*> \endverbatim
*>
*> \param[in] A
*> \verbatim
*>          A is COMPLEX*16 array, dimension (LDA,N)
*>          The m by n matrix A.  If UPLO = 'U', only the upper trapezium
*>          is accessed; if UPLO = 'L', only the lower trapezium is
*>          accessed.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the array A.  LDA >= max(1,M).
*> \endverbatim
*>
*> \param[out] B
*> \verbatim
*>          B is COMPLEX*16 array, dimension (LDB,N)
*>          On exit, B = A in the locations specified by UPLO.
*> \endverbatim
*>
*> \param[in] LDB
*> \verbatim
*>          LDB is INTEGER
*>          The leading dimension of the array B.  LDB >= max(1,M).
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \date December 2016
*
*> \ingroup complex16OTHERauxiliary
*
*  =====================================================================
#endif
#if defined(__GFORTRAN__) && (!defined(__ICC) || !defined(__INTEL_COMPILER))
SUBROUTINE ZLACPY(UPLO, M, N, A, LDA, B, LDB) !GCC$ ATTRIBUTES hot :: ZLACPY !GCC$ ATTRIBUTES aligned(32) :: ZLACPY !GCC$ ATTRIBUTES no_stack_protector :: ZLACPY
#elif defined(__ICC) || defined(__INTEL_COMPILER)
  SUBROUTINE ZLACPY(UPLO,M,N,A,LDA,B,LDB)
    !DIR$ ATTRIBUTES INLINE :: ZLACPY
    !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ZLACPY
    !DIR$ OPTIMIZE : 3
    !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: ZLACPY
#endif
!*
!*  -- LAPACK auxiliary routine (version 3.7.0) --
!*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!*     December 2016
!*
    !*     .. Scalar Arguments ..

      use omp_lib

      implicit none
      CHARACTER          UPLO
      INTEGER            LDA, LDB, M, N
!*     ..
      !*     .. Array Arguments ..
  
      !COMPLEX*16         A( LDA, * ), B( LDB, * )
      COMPLEX(16), DIMENSION(:,:), ALLOCATABLE :: A
      COMPLEX(16), DIMENSION(:,:), ALLOCATABLE :: B
!#elif defined(__ICC) || defined(__INTEL_COMPILER)
!      COMPLEX*16         A( LDA, * ), B( LDB, * )
!      !DIR$ ASSUME_ALIGNED A:64
!      !DIR$ ASSUME_ALIGNED B:64
!#endif
!*     ..
!*
!*  =====================================================================
!*
!*     .. Local Scalars ..
      INTEGER            I, J
!*     ..
!*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
!*     ..
!*     .. Intrinsic Functions ..
      INTRINSIC          MIN
!*     ..
!*     .. Executable Statements ..
!*
      IF( LSAME( UPLO, 'U' ) ) THEN
#if (__GMS_LAPACK_USE_OMP__) == 1
         !$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(SHARED) PRIVATE(J)
#endif
         DO 20 J = 1, N
            !$OMP SIMD ALIGNED(B:64,A:64) LINEAR(I:1)
            DO 10 I = 1, MIN( J, M )
               B( I, J ) = A( I, J )
   10       CONTINUE
   20    CONTINUE
!*
        ELSE IF( LSAME( UPLO, 'L' ) ) THEN
#if (__GMS_LAPACK_USE_OMP__) == 1
         !$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(SHARED) PRIVATE(J)
#endif               
         DO 40 J = 1, N
            !$OMP SIMD ALIGNED(B:64,A:64) LINEAR(I:1)
            DO 30 I = J, M
               B( I, J ) = A( I, J )
   30       CONTINUE
   40    CONTINUE
!*
        ELSE
#if (__GMS_LAPACK_USE_OMP__) == 1
         !$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(SHARED) PRIVATE(J)
#endif                 
         DO 60 J = 1, N
            !$OMP SIMD ALIGNED(B:64,A:64) LINEAR(I:1)
           DO 50 I = 1, M
               B( I, J ) = A( I, J )
   50       CONTINUE
   60    CONTINUE
      END IF

END SUBROUTINE

#if 0
*  Arguments:
*  ==========
*
*> \param[in] FORWRD
*> \verbatim
*>          FORWRD is LOGICAL
*>          = .TRUE., forward permutation
*>          = .FALSE., backward permutation
*> \endverbatim
*>
*> \param[in] M
*> \verbatim
*>          M is INTEGER
*>          The number of rows of the matrix X. M >= 0.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The number of columns of the matrix X. N >= 0.
*> \endverbatim
*>
*> \param[in,out] X
*> \verbatim
*>          X is COMPLEX*16 array, dimension (LDX,N)
*>          On entry, the M by N matrix X.
*>          On exit, X contains the permuted matrix X.
*> \endverbatim
*>
*> \param[in] LDX
*> \verbatim
*>          LDX is INTEGER
*>          The leading dimension of the array X, LDX >= MAX(1,M).
*> \endverbatim
*>
*> \param[in,out] K
*> \verbatim
*>          K is INTEGER array, dimension (N)
*>          On entry, K contains the permutation vector. K is used as
*>          internal workspace, but reset to its original value on
*>          output.
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \date December 2016
*
*> \ingroup complex16OTHERauxiliary
*
*  =====================================================================
#endif
#if defined(__GFORTRAN__) && (!defined(__ICC) || !defined(__INTEL_COMPILER))
SUBROUTINE ZLAPMT(FORWRD, M, N, X, LDX, K) !GCC$ ATTRIBUTES hot :: ZLAPMT !GCC$ ATTRIBUTES aligned(32) :: ZLAPMT !GCC$ ATTRIBUTES no_stack_protector :: ZLAPMT
#elif defined(__ICC) || defined(__INTEL_COMPILER)
  SUBROUTINE ZLAPMT(FORWRD, M, N, X, LDX, K)
   !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ZLAPMT
    !DIR$ OPTIMIZE : 3
    !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=Haswell :: ZLAPMT
#endif
!*
!*  -- LAPACK auxiliary routine (version 3.7.0) --
!*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!*     December 2016
    !*
      implicit none
!*     .. Scalar Arguments ..
      LOGICAL            FORWRD
      INTEGER            LDX, M, N
!*     ..
!*     .. Array Arguments ..
      INTEGER            K( * )
      COMPLEX*16         X( LDX, * )
!*     ..
!*
!*  =====================================================================
!*
!*     .. Local Scalars ..
      INTEGER            I, II, IN, J
      COMPLEX*16         TEMP
!*     ..
!*     .. Executable Statements ..
!*
      IF( N.LE.1 ) &
         RETURN
!*
      DO 10 I = 1, N
         K( I ) = -K( I )
   10 CONTINUE
!*
      IF( FORWRD ) THEN
!*
!*        Forward permutation
!*
         DO 50 I = 1, N
!*
            IF( K( I ).GT.0 ) &
               GO TO 40
!*
            J = I
            K( J ) = -K( J )
            IN = K( J )
!*
   20       CONTINUE
            IF( K( IN ).GT.0 ) &
               GO TO 40
!*
            DO 30 II = 1, M
               TEMP = X( II, J )
               X( II, J ) = X( II, IN )
               X( II, IN ) = TEMP
   30       CONTINUE
!*
            K( IN ) = -K( IN )
            J = IN
            IN = K( IN )
            GO TO 20
!*
   40       CONTINUE
!*
   50    CONTINUE
!*
      ELSE
!*
!*        Backward permutation
!*
         DO 90 I = 1, N
!*
            IF( K( I ).GT.0 ) &
               GO TO 80
!*
            K( I ) = -K( I )
            J = K( I )
   60       CONTINUE
            IF( J.EQ.I ) &
               GO TO 80
!*
            DO 70 II = 1, M
               TEMP = X( II, I )
               X( II, I ) = X( II, J )
               X( II, J ) = TEMP
   70       CONTINUE
!*
            K( J ) = -K( J )
            J = K( J )
            GO TO 60
!*
   80       CONTINUE
!*
   90    CONTINUE
!*
      END IF

END SUBROUTINE

#if 0
*  Arguments:
*  ==========
*
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The order of the elementary reflector.
*> \endverbatim
*>
*> \param[in,out] ALPHA
*> \verbatim
*>          ALPHA is COMPLEX*16
*>          On entry, the value alpha.
*>          On exit, it is overwritten with the value beta.
*> \endverbatim
*>
*> \param[in,out] X
*> \verbatim
*>          X is COMPLEX*16 array, dimension
*>                         (1+(N-2)*abs(INCX))
*>          On entry, the vector x.
*>          On exit, it is overwritten with the vector v.
*> \endverbatim
*>
*> \param[in] INCX
*> \verbatim
*>          INCX is INTEGER
*>          The increment between elements of X. INCX > 0.
*> \endverbatim
*>
*> \param[out] TAU
*> \verbatim
*>          TAU is COMPLEX*16
*>          The value tau.
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \date November 2017
*
*> \ingroup complex16OTHERauxiliary
*
*  =====================================================================
#endif

#if defined(__GFORTRAN__) && (!defined(__ICC) || !defined(__INTEL_COMPILER))
SUBROUTINE ZLARFG( N, ALPHA, X, INCX, TAU ) !GCC$ ATTRIBUTES hot :: ZLARFG !GCC$ ATTRIBUTES aligned(32) :: ZLARFG !GCC$ ATTRIBUTES no_stack_protector :: ZLARFG
#elif defined(__ICC) || defined(__INTEL_COMPILER)
SUBROUTINE ZLARFG( N, ALPHA, X, INCX, TAU )
 !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ZLARFG
    !DIR$ OPTIMIZE : 3
    !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=Haswell :: ZLARFG
#endif
!*
!*  -- LAPACK auxiliary routine (version 3.8.0) --
!*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!*     November 2017
        !*
        implicit none
!*     .. Scalar Arguments ..
      INTEGER            INCX, N
      COMPLEX*16         ALPHA, TAU
!*     ..
!*     .. Array Arguments ..
      COMPLEX*16         X( * )
!*     ..
!*
!*  =====================================================================
!*
!*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!*     ..
!*     .. Local Scalars ..
      INTEGER            J, KNT
      DOUBLE PRECISION   ALPHI, ALPHR, BETA, RSAFMN, SAFMIN, XNORM
!*     ..
!*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH, DLAPY3, DZNRM2
      COMPLEX*16         ZLADIV
      EXTERNAL           DLAMCH, DLAPY3, DZNRM2, ZLADIV
!*     ..
!*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, DCMPLX, DIMAG, SIGN
!*     ..
!*     .. External Subroutines ..
!      EXTERNAL           ZDSCAL, ZSCAL
!*     ..
!*     .. Executable Statements ..
!*
      IF( N.LE.0 ) THEN
         TAU = ZERO
         RETURN
      END IF
!*
      XNORM = DZNRM2( N-1, X, INCX )
      ALPHR = DBLE( ALPHA )
      ALPHI = DIMAG( ALPHA )
!*
      IF( XNORM.EQ.ZERO .AND. ALPHI.EQ.ZERO ) THEN
!*
!*        H  =  I
!*
         TAU = ZERO
      ELSE
!*
!*        general case
!*
         BETA = -SIGN( DLAPY3( ALPHR, ALPHI, XNORM ), ALPHR )
         SAFMIN = DLAMCH( 'S' ) / DLAMCH( 'E' )
         RSAFMN = ONE / SAFMIN
!*
         KNT = 0
         IF( ABS( BETA ).LT.SAFMIN ) THEN
!*
!*           XNORM, BETA may be inaccurate; scale X and recompute them
!*
   10       CONTINUE
            KNT = KNT + 1
            CALL ZDSCAL( N-1, RSAFMN, X, INCX )
            BETA = BETA*RSAFMN
            ALPHI = ALPHI*RSAFMN
            ALPHR = ALPHR*RSAFMN
            IF( (ABS( BETA ).LT.SAFMIN) .AND. (KNT .LT. 20) ) &
                GO TO 10
!*
!*           New BETA is at most 1, at least SAFMIN
!*
            XNORM = DZNRM2( N-1, X, INCX )
            ALPHA = DCMPLX( ALPHR, ALPHI )
            BETA = -SIGN( DLAPY3( ALPHR, ALPHI, XNORM ), ALPHR )
         END IF
         TAU = DCMPLX( ( BETA-ALPHR ) / BETA, -ALPHI / BETA )
         ALPHA = ZLADIV( DCMPLX( ONE ), ALPHA-BETA )
         CALL ZSCAL( N-1, ALPHA, X, INCX )
!*
!*        If ALPHA is subnormal, it may lose relative accuracy
!*
         DO 20 J = 1, KNT
            BETA = BETA*SAFMIN
 20      CONTINUE
         ALPHA = BETA
      END IF

END SUBROUTINE 


#if defined(__GFORTRAN__) && (!defined(__ICC) || !defined(__INTEL_COMPILER))
SUBROUTINE ZSCAL(N,ZA,ZX,INCX) !GCC$ ATTRIBUTES INLINE :: ZSCAL !GCC$ ATTRIBUTES aligned(32) :: ZSCAL
#elif defined(__ICC) || defined(__INTEL_COMPILER)
  SUBROUTINE ZSCAL(N,ZA,ZX,INCX)
    !DIR$ ATTRIBUTES FORCEINLINE :: ZSCAL
 !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ZSCAL
    !DIR$ OPTIMIZE : 3
    !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: ZSCAL
#endif

    use omp_lib

     implicit none
!*
!*  -- Reference BLAS level1 routine (version 3.8.0) --
!*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
!*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!*     November 2017
!*
!*     .. Scalar Arguments ..
      COMPLEX*16 ZA
      INTEGER INCX,N
!*     ..
      !*     .. Array Arguments ..

      !COMPLEX*16 ZX(*)
      COMPLEX(16), DIMENSION(:), ALLOCATABLE :: ZX
!#elif defined(__ICC) || defined(__INTEL_COMPILER)
!      COMPLEX(16) ZX(*)
!      !DIR$ ASSUME_ALIGNED ZX:64
!#endif
!*     ..
!*
!*  =====================================================================
!*
!*     .. Local Scalars ..
      INTEGER I,NINCX
!*     ..
      !IF (N.LE.0 .OR. INCX.LE.0) RETURN
      IF (INCX.EQ.1) THEN
!*
!*        code for increment equal to 1
         !*
#if defined(__GFORTRAN__) && (!defined(__ICC) || !defined(__INTEL_COMPILER))
         !$OMP SIMD ALIGNED(ZX:64) LINEAR(I:1)
#elif defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ VECTOR ALIGNED
         !DIR$ SIMD
#endif
         DO I = 1,N
            ZX(I) = ZA*ZX(I)
         END DO
      ELSE
!*
!*        code for increment not equal to 1
!*
         NINCX = N*INCX

         !$OMP SIMD ALIGNED(ZX:64)
         DO I = 1,NINCX,INCX
            ZX(I) = ZA*ZX(I)
         END DO
      END IF
    
END SUBROUTINE


#if defined(__GFORTRAN__) && (!defined(__ICC) || !defined(__INTEL_COMPILER))
SUBROUTINE ZDSCAL(N,DA,ZX,INCX) !GCC$ ATTRIBUTES INLINE :: ZDSCAL !GCC$ ATTRIBUTES aligned(32) :: ZDSCAL
#elif defined(__ICC) || defined(__INTEL_COMPILER)
  SUBROUTINE ZDSCAL(N,DA,ZX,INCX)
  !DIR$ ATTRIBUTES FORCEINLINE :: ZDSCAL
 !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ZDSCAL
    !DIR$ OPTIMIZE : 3
    !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: ZDSCAL
#endif
!*
!*  -- Reference BLAS level1 routine (version 3.8.0) --
!*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
!*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!*     November 2017
!*
    !*     .. Scalar Arguments ..
#if defined(__GFORTRAN__) && (!defined(__ICC) || !defined(__INTEL_COMPILER))
      use omp_lib
#endif
      implicit none    
      DOUBLE PRECISION DA
      INTEGER INCX,N
!*     ..
      !*     .. Array Arguments ..
    
      !COMPLEX*16 ZX(*)
      COMPLEX(16), DIMENSION(:), ALLOCATABLE :: ZX

!*     ..
!*
!*  =====================================================================
!*
!*     .. Local Scalars ..
      INTEGER I,NINCX
!*     ..
!*     .. Intrinsic Functions ..
      INTRINSIC DCMPLX
!*     ..
!      IF (N.LE.0 .OR. INCX.LE.0) RETURN
      IF (INCX.EQ.1) THEN
!*
!*        code for increment equal to 1
         !*

         !$OMP SIMD ALIGNED(ZX:64) LINEAR(I:1)
         DO I = 1,N
            ZX(I) = DCMPLX(DA,0.0d0)*ZX(I)
         END DO
      ELSE
!*
!*        code for increment not equal to 1
!*
         NINCX = N*INCX

         !$OMP SIMD ALIGNED(ZX:64)
         DO I = 1,NINCX,INCX
            ZX(I) = DCMPLX(DA,0.0d0)*ZX(I)
         END DO
      END IF
    
END SUBROUTINE

    
! Authors:
!*  ========
!*
!*> \author Univ. of Tennessee
!*> \author Univ. of California Berkeley
!*> \author Univ. of Colorado Denver
!*> \author NAG Ltd.
!*
!*> \date December 2016
!*
!*> \ingroup complex16OTHERauxiliary
!*
!*  =====================================================================
#if defined(__GFORTRAN__) && (!defined(__ICC) || !defined(__INTEL_COMPILER))
SUBROUTINE ZLASET(UPLO, M, N, ALPHA, BETA, A, LDA) !GCC$ ATTRIBUTES hot :: ZLASET !GCC$ ATTRIBUTES aligned(32) :: ZLASET !GCC$ ATTRIBUTES no_stack_protector :: ZLASET
#elif defined(__ICC) || defined(__INTEL_COMPILER)
  SUBROUTINE ZLASET(UPLO, M, N, ALPHA, BETA, A, LDA)
  !DIR$ ATTRIBUTES FORCEINLINE :: ZLASET
 !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ZLASET
    !DIR$ OPTIMIZE : 3
    !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: ZLASET
#endif

    use omp_lib

       implicit none
!*
!*  -- LAPACK auxiliary routine (version 3.7.0) --
!*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!*     December 2016
!*
!*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            LDA, M, N
      COMPLEX*16         ALPHA, BETA
!*     ..
      !*     .. Array Arguments ..

      !COMPLEX*16         A( LDA, * )
      COMPLEX(16), DIMENSION(:,:), ALLOCATABLE :: A
!#elif defined(__ICC) || defined(__INTEL_COMPILER)
!      COMPLEX(16), DIMENSION(LDA,*) :: A
!      !DIR$ ASSUME_ALIGNED A:64
!#endif
!*     ..
!!*
!*  =====================================================================
!*
!*     .. Local Scalars ..
      INTEGER            I, J
!*     ..
!*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
!*     ..
!*     .. Intrinsic Functions ..
      INTRINSIC          MIN
!*     ..
!*     .. Executable Statements ..
!*
      IF( LSAME( UPLO, 'U' ) ) THEN
!*
!*        Set the diagonal to BETA and the strictly upper triangular
!*        part of the array to ALPHA.
         !*
#if (__GMS_LAPACK_USE_OMP__) == 1
         !$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(SHARED) PRIVATE(J)
#endif
         DO 20 J = 2, N
            !$OMP SIMD ALIGNED(A:64) LINEAR(I:1)
            DO 10 I = 1, MIN( J-1, M )
               A( I, J ) = ALPHA
   10       CONTINUE
   20    CONTINUE
         DO 30 I = 1, MIN( N, M )
            A( I, I ) = BETA
   30    CONTINUE
!*
      ELSE IF( LSAME( UPLO, 'L' ) ) THEN
!*
!*        Set the diagonal to BETA and the strictly lower triangular
!*        part of the array to ALPHA.
         !*
#if (__GMS_LAPACK_USE_OMP__) == 1
         !$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(SHARED) PRIVATE(J)
#endif
         DO 50 J = 1, MIN( M, N )
            !$OMP SIMD ALIGNED(A:64) LINEAR(I:1)
            DO 40 I = J + 1, M
               A( I, J ) = ALPHA
   40       CONTINUE
   50    CONTINUE
         DO 60 I = 1, MIN( N, M )
            A( I, I ) = BETA
   60    CONTINUE
!*
      ELSE
!*
!*        Set the array to BETA on the diagonal and ALPHA on the
!*        offdiagonal.
         !*
#if (__GMS_LAPACK_USE_OMP__) == 1
         !$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(SHARED) PRIVATE(J)
#endif
         DO 80 J = 1, N
            !$OMP SIMD ALIGNED(A:64) LINEAR(I:1)
            DO 70 I = 1, M
               A( I, J ) = ALPHA
   70       CONTINUE
   80    CONTINUE
         DO 90 I = 1, MIN( M, N )
            A( I, I ) = BETA
   90    CONTINUE
      END IF

END SUBROUTINE

#if 0
*  Arguments:
*  ==========
*
*> \param[in] SIDE
*> \verbatim
*>          SIDE is CHARACTER*1
*>          = 'L': form P * C
*>          = 'R': form C * P
*> \endverbatim
*>
*> \param[in] M
*> \verbatim
*>          M is INTEGER
*>          The number of rows of the matrix C.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The number of columns of the matrix C.
*> \endverbatim
*>
*> \param[in] V
*> \verbatim
*>          V is COMPLEX*16 array, dimension
*>                  (1 + (M-1)*abs(INCV)) if SIDE = 'L'
*>                  (1 + (N-1)*abs(INCV)) if SIDE = 'R'
*>          The vector v in the representation of P. V is not used
*>          if TAU = 0.
*> \endverbatim
*>
*> \param[in] INCV
*> \verbatim
*>          INCV is INTEGER
*>          The increment between elements of v. INCV <> 0
*> \endverbatim
*>
*> \param[in] TAU
*> \verbatim
*>          TAU is COMPLEX*16
*>          The value tau in the representation of P.
*> \endverbatim
*>
*> \param[in,out] C1
*> \verbatim
*>          C1 is COMPLEX*16 array, dimension
*>                         (LDC,N) if SIDE = 'L'
*>                         (M,1)   if SIDE = 'R'
*>          On entry, the n-vector C1 if SIDE = 'L', or the m-vector C1
*>          if SIDE = 'R'.
*>
*>          On exit, the first row of P*C if SIDE = 'L', or the first
*>          column of C*P if SIDE = 'R'.
*> \endverbatim
*>
*> \param[in,out] C2
*> \verbatim
*>          C2 is COMPLEX*16 array, dimension
*>                         (LDC, N)   if SIDE = 'L'
*>                         (LDC, N-1) if SIDE = 'R'
*>          On entry, the (m - 1) x n matrix C2 if SIDE = 'L', or the
*>          m x (n - 1) matrix C2 if SIDE = 'R'.
*>
*>          On exit, rows 2:m of P*C if SIDE = 'L', or columns 2:m of C*P
*>          if SIDE = 'R'.
*> \endverbatim
*>
*> \param[in] LDC
*> \verbatim
*>          LDC is INTEGER
*>          The leading dimension of the arrays C1 and C2.
*>          LDC >= max(1,M).
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is COMPLEX*16 array, dimension
*>                      (N) if SIDE = 'L'
*>                      (M) if SIDE = 'R'
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \date December 2016
*
*> \ingroup complex16OTHERcomputational
*
*  =====================================================================
#endif
#if defined(__GFORTRAN__) && (!defined(__ICC) || !defined(__INTEL_COMPILER))
SUBROUTINE ZLATZM(SIDE, M, N, V, INCV, TAU, C1, C2, LDC, WORK) !GCC$ ATTRIBUTES hot :: ZLATZM !GCC$ ATTRIBUTES aligned(32) :: ZLATZM !GCC$ ATTRIBUTES no_stack_protector :: ZLATZM
#elif defined(__ICC) || defined(__INTEL_COMPILER)
SUBROUTINE ZLATZM(SIDE, M, N, V, INCV, TAU, C1, C2, LDC, WORK)
  !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ZLATZM
    !DIR$ OPTIMIZE : 3
    !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=Haswell :: ZLATZM
#endif
!*
!*  -- LAPACK computational routine (version 3.7.0) --
!*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!*     December 2016
!*
  !*     .. Scalar Arguments ..
      implicit none
      CHARACTER          SIDE
      INTEGER            INCV, LDC, M, N
      COMPLEX*16         TAU
!*     ..
!*     .. Array Arguments ..
      COMPLEX*16         C1( LDC, * ), C2( LDC, * ), V( * ), WORK( * )
!*     ..
!*
!*  =====================================================================
!*
!*     .. Parameters ..
      COMPLEX*16         ONE, ZERO
      PARAMETER          ( ONE = ( 1.0D+0, 0.0D+0 ), &
                         ZERO = ( 0.0D+0, 0.0D+0 ) )
!*     ..
!*     .. External Subroutines ..
!      EXTERNAL           ZAXPY, ZCOPY, ZGEMV, ZGERC, ZGERU, ZLACGV
!*     ..
!*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
!*     ..
!*     .. Intrinsic Functions ..
      INTRINSIC          MIN
!*     ..
!*     .. Executable Statements ..
!*
      IF( ( MIN( M, N ).EQ.0 ) .OR. ( TAU.EQ.ZERO ) ) &
          RETURN
!*
      IF( LSAME( SIDE, 'L' ) ) THEN
!*
!*        w :=  ( C1 + v**H * C2 )**H
!*
         CALL ZCOPY( N, C1, LDC, WORK, 1 )
         CALL ZLACGV( N, WORK, 1 )
         CALL ZGEMV( 'Conjugate transpose', M-1, N, ONE, C2, LDC, V, &
                    INCV, ONE, WORK, 1 )
!*
!*        [ C1 ] := [ C1 ] - tau* [ 1 ] * w**H
!*        [ C2 ]    [ C2 ]        [ v ]
!*
         CALL ZLACGV( N, WORK, 1 )
         CALL ZAXPY( N, -TAU, WORK, 1, C1, LDC )
         CALL ZGERU( M-1, N, -TAU, V, INCV, WORK, 1, C2, LDC )
!*
      ELSE IF( LSAME( SIDE, 'R' ) ) THEN
!*
!*        w := C1 + C2 * v
!*
         CALL ZCOPY( M, C1, 1, WORK, 1 )
         CALL ZGEMV( 'No transpose', M, N-1, ONE, C2, LDC, V, INCV, ONE, &
                     WORK, 1 )
!*
!*        [ C1, C2 ] := [ C1, C2 ] - tau* w * [ 1 , v**H]
!*
         CALL ZAXPY( M, -TAU, WORK, 1, C1, 1 )
         CALL ZGERC( M, N-1, -TAU, WORK, 1, V, INCV, C2, LDC )
      END IF
!*
END SUBROUTINE

#if 0
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \date November 2017
*
*> \ingroup complex16_blas_level1
*
*> \par Further Details:
*  =====================
*>
*> \verbatim
*>
*>     jack dongarra, linpack, 4/11/78.
*>     modified 12/3/93, array(1) declarations changed to array(*)
*> \endverbatim
*>
*  =====================================================================
#endif
#if defined(__GFORTRAN__) && (!defined(__ICC) || !defined(__INTEL_COMPILER))
SUBROUTINE ZCOPY(N,ZX,INCX,ZY,INCY) !GCC$ ATTRIBUTES inline :: ZCOPY !GCC$ ATTRIBUTES aligned(32) :: ZCOPY
#elif defined(__ICC) || defined(__INTEL_COMPILER)
SUBROUTINE ZCOPY(N,ZX,INCX,ZY,INCY)
    !DIR$ ATTRIBUTES FORCEINLINE :: ZCOPY
    !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ZCOPY
    !DIR$ OPTIMIZE : 3
    !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: ZCOPY

#endif
        use omp_lib
       implicit none
!*
!*  -- Reference BLAS level1 routine (version 3.8.0) --
!*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
!*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!*     November 2017
!*
!*     .. Scalar Arguments ..
      INTEGER INCX,INCY,N
!*     ..
!*     .. Array Arguments ..
      ! COMPLEX*16 ZX(*),ZY(*)
      COMPLEX(16), DIMENSION(:), ALLOCATABLE :: ZX
      COMPLEX(16), DIMENSION(:), ALLOCATABLE :: ZY
!*     ..
!*
!*  =====================================================================
!*
!*     .. Local Scalars ..
      INTEGER I,IX,IY
!*     ..
      IF (N.LE.0) RETURN
      IF (INCX.EQ.1 .AND. INCY.EQ.1) THEN
!*
!*        code for both increments equal to 1
         !*
         !$OMP SIMD ALIGNED(ZY:64,ZX) LINEAR(I:1)
         DO I = 1,N
          ZY(I) = ZX(I)
         END DO
      ELSE
!*
!*        code for unequal increments or equal increments
!*          not equal to 1
!*
         IX = 1
         IY = 1
         IF (INCX.LT.0) IX = (-N+1)*INCX + 1
         IF (INCY.LT.0) IY = (-N+1)*INCY + 1
         !$OMP SIMD ALIGNED(ZY:64,ZY) 
         DO I = 1,N
            ZY(IY) = ZX(IX)
            IX = IX + INCX
            IY = IY + INCY
         END DO
      END IF     
END SUBROUTINE

#if 0
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \date November 2017
*
*> \ingroup complex16_blas_level1
*
*> \par Further Details:
*  =====================
*>
*> \verbatim
*>
*>     jack dongarra, 3/11/78.
*>     modified 12/3/93, array(1) declarations changed to array(*)
*> \endverbatim
*>
*  =====================================================================
#endif
#if defined(__GFORTRAN__) && (!defined(__ICC) || !defined(__INTEL_COMPILER))
SUBROUTINE ZAXPY(N,ZA,ZX,INCX,ZY,INCY) !GCC$ ATTRIBUTES INLINE :: ZAXPY !GCC$ ATTRIBUTES aligned(32) :: ZAXPY
#elif defined(__ICC) || defined(__INTEL_COMPILER)
  SUBROUTINE ZAXPY(N,ZA,ZX,INCX,ZY,INCY)
 !DIR$ ATTRIBUTES FORCEINLINE :: ZCOPY
    !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ZCOPY
    !DIR$ OPTIMIZE : 3
    !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: ZCOPY
#endif
       use omp_lib
       implicit none
!*
!*  -- Reference BLAS level1 routine (version 3.8.0) --
!*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
!*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!*     November 2017
!*
!*     .. Scalar Arguments ..
      COMPLEX*16 ZA
      INTEGER INCX,INCY,N
!*     ..
!*     .. Array Arguments ..
      !      COMPLEX*16 ZX(*),ZY(*)
      COMPLEX(16), DIMENSION(:), ALLOCATABLE :: ZX
      COMPLEX(16), DIMENSION(:), ALLOCATABLE :: ZY
!*     ..
!*
!*  =====================================================================
!*
!*     .. Local Scalars ..
      INTEGER I,IX,IY
!*     ..
!*     .. External Functions ..
      !DOUBLE PRECISION DCABS1
      !EXTERNAL DCABS1
!*     ..
      
      IF (DCABS1(ZA).EQ.0.0d0) RETURN
      IF (INCX.EQ.1 .AND. INCY.EQ.1) THEN
!*
!*        code for both increments equal to 1
         !*
         !$OMP SIMD ALIGNED(ZY:64,ZX) LINEAR(I:1)
         DO I = 1,N
            ZY(I) = ZY(I) + ZA*ZX(I)
         END DO
      ELSE
!*
!*        code for unequal increments or equal increments
!*          not equal to 1
!*
         IX = 1
         IY = 1
         IF (INCX.LT.0) IX = (-N+1)*INCX + 1
         IF (INCY.LT.0) IY = (-N+1)*INCY + 1
         !$OMP SIMD ALIGNED(ZY:64,ZX)
         DO I = 1,N
            ZY(IY) = ZY(IY) + ZA*ZX(IX)
            IX = IX + INCX
            IY = IY + INCY
         END DO
      END IF
END SUBROUTINE
   
!*> \author Univ. of Tennessee
!*> \author Univ. of California Berkeley
!*> \author Univ. of Colorado Denver
!*> \author NAG Ltd.
!*
!*> \date November 2017
!*
!*> \ingroup double_blas_level1
!*
!*  =====================================================================
      DOUBLE PRECISION FUNCTION DCABS1(Z)
!*
!*  -- Reference BLAS level1 routine (version 3.8.0) --
!*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
!*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!*     November 2017
!*
!*     .. Scalar Arguments ..
      COMPLEX*16 Z
!*     ..
!*     ..
!*  =====================================================================
!*
!*     .. Intrinsic Functions ..
      INTRINSIC ABS,DBLE,DIMAG
      DCABS1 = ABS(DBLE(Z)) + ABS(DIMAG(Z))
END FUNCTION     

#if 0
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \date December 2016
*
*> \ingroup complex16_blas_level2
*
*> \par Further Details:
*  =====================
*>
*> \verbatim
*>
*>  Level 2 Blas routine.
*>  The vector and matrix arguments are not referenced when N = 0, or M = 0
*>
*>  -- Written on 22-October-1986.
*>     Jack Dongarra, Argonne National Lab.
*>     Jeremy Du Croz, Nag Central Office.
*>     Sven Hammarling, Nag Central Office.
*>     Richard Hanson, Sandia National Labs.
*> \endverbatim
*>
*  =====================================================================
#endif
#if defined(__GFORTRAN__) && (!defined(__ICC) || !defined(__INTEL_COMPILER))
SUBROUTINE ZGEMV(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY) !GCC$ ATTRIBUTES hot :: ZGEMV !GCC$ ATTRIBUTES aligned(32) :: ZGEMV !GCC$ ATTRIBUTES no_stack_protector :: ZGEMV
#elif defined(__ICC) || defined(__INTEL_COMPILER)
SUBROUTINE ZGEMV(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
  !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ZGEMV
    !DIR$ OPTIMIZE : 3
    !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: ZGEMV
#endif
      use omp_lib
      implicit none
!*
!*  -- Reference BLAS level2 routine (version 3.7.0) --
!*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
!*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!*     December 2016
!*
!*     .. Scalar Arguments ..
      COMPLEX*16 ALPHA,BETA
      INTEGER INCX,INCY,LDA,M,N
      CHARACTER TRANS
!*     ..
!*     .. Array Arguments ..
      ! COMPLEX*16 A(LDA,*),X(*),Y(*)
      COMPLEX(16), DIMENSION(:,:), ALLOCATABLE :: A
      COMPLEX(16), DIMENSION(:),   ALLOCATABLE :: X
      COMPLEX(16), DIMENSION(:),   ALLOCATABLE :: Y
!*     ..
!*
!*  =====================================================================
!*
!*     .. Parameters ..
      COMPLEX*16 ONE
      PARAMETER (ONE= (1.0D+0,0.0D+0))
      COMPLEX*16 ZERO
      PARAMETER (ZERO= (0.0D+0,0.0D+0))
!*     ..
!*     .. Local Scalars ..
      COMPLEX*16 TEMP
      INTEGER I,INFO,IX,IY,J,JX,JY,KX,KY,LENX,LENY
      LOGICAL NOCONJ
!*     ..
!*     .. External Functions ..
      LOGICAL LSAME
      EXTERNAL LSAME
!*     ..
!*     .. External Subroutines ..
!      EXTERNAL XERBLA
!*     ..
!*     .. Intrinsic Functions ..
      INTRINSIC DCONJG,MAX
!*     ..
!*
!*     Test the input parameters.
!*
!      INFO = 0
!      IF (.NOT.LSAME(TRANS,'N') .AND. .NOT.LSAME(TRANS,'T') .AND.
!     +    .NOT.LSAME(TRANS,'C')) THEN
!          INFO = 1
!      ELSE IF (M.LT.0) THEN
!          INFO = 2
!      ELSE IF (N.LT.0) THEN
!          INFO = 3
!      ELSE IF (LDA.LT.MAX(1,M)) THEN
!          INFO = 6
!      ELSE IF (INCX.EQ.0) THEN
!          INFO = 8
!      ELSE IF (INCY.EQ.0) THEN
!          INFO = 11
!      END IF
!      IF (INFO.NE.0) THEN
!!          CALL XERBLA('ZGEMV ',INFO)
!          RETURN
!      END IF
!*
!*     Quick return if possible.
!*
!      IF ((M.EQ.0) .OR. (N.EQ.0) .OR.
!     +    ((ALPHA.EQ.ZERO).AND. (BETA.EQ.ONE))) RETURN
!*
      NOCONJ = LSAME(TRANS,'T')
!*
!*     Set  LENX  and  LENY, the lengths of the vectors x and y, and set
!*     up the start points in  X  and  Y.
!*
      IF (LSAME(TRANS,'N')) THEN
          LENX = N
          LENY = M
      ELSE
          LENX = M
          LENY = N
      END IF
      IF (INCX.GT.0) THEN
          KX = 1
      ELSE
          KX = 1 - (LENX-1)*INCX
      END IF
      IF (INCY.GT.0) THEN
          KY = 1
      ELSE
          KY = 1 - (LENY-1)*INCY
      END IF
!*
!*     Start the operations. In this version the elements of A are
!*     accessed sequentially with one pass through A.
!*
!*     First form  y := beta*y.
!*
      IF (BETA.NE.ONE) THEN
          IF (INCY.EQ.1) THEN
             IF (BETA.EQ.ZERO) THEN
                 !$OMP SIMD ALIGNED(Y:64) LINEAR(I:1)
                  DO 10 I = 1,LENY
                      Y(I) = ZERO
   10             CONTINUE
             ELSE
                  !$OMP SIMD ALIGNED(Y:64) LINEAR(I:1)    
                  DO 20 I = 1,LENY
                      Y(I) = BETA*Y(I)
   20             CONTINUE
              END IF
          ELSE
              IY = KY
              IF (BETA.EQ.ZERO) THEN
                  !$OMP SIMD ALIGNED(Y:64) LINEAR(I:1)
                  DO 30 I = 1,LENY
                      Y(IY) = ZERO
                      IY = IY + INCY
   30             CONTINUE
              ELSE
                  !$OMP SIMD ALIGNED(Y:64) LINEAR(I:1)    
                  DO 40 I = 1,LENY
                      Y(IY) = BETA*Y(IY)
                      IY = IY + INCY
   40             CONTINUE
              END IF
          END IF
      END IF
      IF (ALPHA.EQ.ZERO) RETURN
      IF (LSAME(TRANS,'N')) THEN
!*
!*        Form  y := alpha*A*x + y.
!*
          JX = KX
          IF (INCY.EQ.1) THEN
#if (__GMS_LAPACK_USE_OMP__) == 1
             !$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(SHARED) PRIVATE(J,TEMP,JX)
#endif
              DO 60 J = 1,N
                 TEMP = ALPHA*X(JX)
                  !$OMP SIMD ALIGNED(Y:64,A) LINEAR(I:1)
                  DO 50 I = 1,M
                      Y(I) = Y(I) + TEMP*A(I,J)
   50             CONTINUE
                  JX = JX + INCX
   60         CONTINUE
          ELSE
#if (__GMS_LAPACK_USE_OMP__) == 1
             !$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(SHARED) PRIVATE(J,TEMP,IY,JX)
#endif                  
              DO 80 J = 1,N
                  TEMP = ALPHA*X(JX)
                  IY = KY
                  !$OMP SIMD ALIGNED(Y:64,A) LINEAR(I:1) UNROLL PARTIAL(10)
                  DO 70 I = 1,M
                      Y(IY) = Y(IY) + TEMP*A(I,J)
                      IY = IY + INCY
   70             CONTINUE
                  JX = JX + INCX
   80         CONTINUE
          END IF
      ELSE
!*
!*        Form  y := alpha*A**T*x + y  or  y := alpha*A**H*x + y.
!*
          JY = KY
          IF (INCX.EQ.1) THEN
#if (__GMS_LAPACK_USE_OMP__) == 1
             !$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(SHARED) PRIVATE(J,TEMP,JY)
#endif                 
              DO 110 J = 1,N
                  TEMP = ZERO
                  IF (NOCONJ) THEN
                      !$OMP SIMD ALIGNED(Y:64,A) REDUCTION(+:TEMP) UNROLL PARTIAL(10)
                      DO 90 I = 1,M
                          TEMP = TEMP + A(I,J)*X(I)
   90                 CONTINUE
                  ELSE
                      !$OMP SIMD ALIGNED(Y:64,A) LINEAR(I:1) REDUCTION(+:TEMP)  UNROLL PARTIAL(10)    
                      DO 100 I = 1,M
                          TEMP = TEMP + DCONJG(A(I,J))*X(I)
  100                 CONTINUE
                  END IF
                  Y(JY) = Y(JY) + ALPHA*TEMP
                  JY = JY + INCY
  110         CONTINUE
           ELSE
#if (__GMS_LAPACK_USE_OMP__) == 1
             !$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(SHARED) PRIVATE(J,TEMP,JY,IX,JY)
#endif                     
              DO 140 J = 1,N
                  TEMP = ZERO
                  IX = KX
                  IF (NOCONJ) THEN
                       !$OMP SIMD ALIGNED(Y:64,A) REDUCTION(+:TEMP) UNROLL PARTIAL(10)
                      DO 120 I = 1,M
                          TEMP = TEMP + A(I,J)*X(IX)
                          IX = IX + INCX
  120                 CONTINUE
                  ELSE
                      !$OMP SIMD ALIGNED(Y:64,A) REDUCTION(+:TEMP) UNROLL PARTIAL(10)    
                      DO 130 I = 1,M
                          TEMP = TEMP + DCONJG(A(I,J))*X(IX)
                          IX = IX + INCX
  130                 CONTINUE
                  END IF
                  Y(JY) = Y(JY) + ALPHA*TEMP
                  JY = JY + INCY
  140         CONTINUE
          END IF
      END IF

END SUBROUTINE
