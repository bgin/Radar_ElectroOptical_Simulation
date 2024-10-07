      LOGICAL FUNCTION SBIGP( IP, DIM1, DIM2 )
*     .. Scalar Arguments ..
      INTEGER                IP, DIM1, DIM2
*     ..
*
*  Purpose
*  =======
*
*  SBIGP determines which of two alternative code sections in a GEMM-
*  Based Level 3 BLAS routine that will be the fastest for a particular
*  problem. If the problem is considered large enough SBIGP returns
*  .TRUE., otherwise .FALSE. is returned. The input parameter IP
*  specifies the calling routine and a break point for alternative code
*  sections. The input parameters DIM1 and DIM2 are matrix dimensions.
*  The returned value is a function of the input parameters and the
*  performance characteristics of the two alternative code sections.
*
*  In this simple implementation, the returned values are determined by
*  looking at only one of the two dimensions DIM1 and DIM2. It may be
*  rewarding to rewrite the logical expressions in SBIGP so that both
*  dimensions are involved. The returned values should effectively
*  reflect the performance characteristics of the underlying BLAS
*  routines.
*
*
*  Input
*  =====
*
*  IP     - INTEGER
*           On entry, IP specifies which routine and which alternative
*           code sections that the decision is intended for.
*           Unchanged on exit.
*
*  DIM1   - INTEGER.
*           On entry, DIM1 specifies the first dimension in the calling
*           sequence of the Level 3 routine specified by IP.
*           Unchanged on exit.
*
*  DIM2   - INTEGER.
*           On entry, DIM2 specifies the second dimension in the
*           calling sequence of the Level 3 routine specified by IP.
*           Unchanged on exit.
*
*
*  -- Written in December-1993.
*     GEMM-Based Level 3 BLAS.
*     Per Ling, Institute of Information Processing,
*     University of Umea, Sweden.
*
*
*     .. User specified parameters for SBIGP ..
      INTEGER            SIP41, SIP42, SIP81, SIP82, SIP83,
     $                   SIP91, SIP92, SIP93
      PARAMETER        ( SIP41 = 4, SIP42 = 3,
     $                   SIP81 = 4, SIP82 = 3, SIP83 = 4,
     $                   SIP91 = 4, SIP92 = 3, SIP93 = 4 )
*     ..
*     .. Executable Statements ..
      IF( IP.EQ.41 )THEN
         SBIGP = DIM1.GE.SIP41
      ELSE IF( IP.EQ.42 )THEN
         SBIGP = DIM2.GE.SIP42
      ELSE IF( IP.EQ.81 )THEN
         SBIGP = DIM2.GE.SIP81
      ELSE IF( IP.EQ.82 )THEN
         SBIGP = DIM2.GE.SIP82
      ELSE IF( IP.EQ.83 )THEN
         SBIGP = DIM1.GE.SIP83
      ELSE IF( IP.EQ.91 )THEN
         SBIGP = DIM2.GE.SIP91
      ELSE IF( IP.EQ.92 )THEN
         SBIGP = DIM2.GE.SIP92
      ELSE IF( IP.EQ.93 )THEN
         SBIGP = DIM1.GE.SIP93
      ELSE
         SBIGP = .FALSE.
      END IF
*
      RETURN
*
*     End of SBIGP.
*
      END
