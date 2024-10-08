      LOGICAL FUNCTION DBIGP( IP, DIM1, DIM2 )
*     .. Scalar Arguments ..
      INTEGER                IP, DIM1, DIM2
*     ..
*
*  Purpose
*  =======
*
*  DBIGP determines which of two alternative code sections in a GEMM-
*  Based Level 3 BLAS routine that will be the fastest for a particular
*  problem. If the problem is considered large enough DBIGP returns
*  .TRUE., otherwise .FALSE. is returned. The input parameter IP
*  specifies the calling routine and a break point for alternative code
*  sections. The input parameters DIM1 and DIM2 are matrix dimensions.
*  The returned value is a function of the input parameters and the
*  performance characteristics of the two alternative code sections.
*
*  In this simple implementation, the returned values are determined by
*  looking at only one of the two dimensions DIM1 and DIM2. It may be
*  rewarding to rewrite the logical expressions in DBIGP so that both
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
*     .. User specified parameters for DBIGP ..
      INTEGER            DIP41, DIP42, DIP81, DIP82, DIP83,
     $                   DIP91, DIP92, DIP93
      PARAMETER        ( DIP41 = 4, DIP42 = 3,
     $                   DIP81 = 4, DIP82 = 3, DIP83 = 4,
     $                   DIP91 = 4, DIP92 = 3, DIP93 = 4 )
*     ..
*     .. Executable Statements ..
      IF( IP.EQ.41 )THEN
         DBIGP = DIM1.GE.DIP41
      ELSE IF( IP.EQ.42 )THEN
         DBIGP = DIM2.GE.DIP42
      ELSE IF( IP.EQ.81 )THEN
         DBIGP = DIM2.GE.DIP81
      ELSE IF( IP.EQ.82 )THEN
         DBIGP = DIM2.GE.DIP82
      ELSE IF( IP.EQ.83 )THEN
         DBIGP = DIM1.GE.DIP83
      ELSE IF( IP.EQ.91 )THEN
         DBIGP = DIM2.GE.DIP91
      ELSE IF( IP.EQ.92 )THEN
         DBIGP = DIM2.GE.DIP92
      ELSE IF( IP.EQ.93 )THEN
         DBIGP = DIM1.GE.DIP93
      ELSE
         DBIGP = .FALSE.
      END IF
*
      RETURN
*
*     End of DBIGP.
*
      END
