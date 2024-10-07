      SUBROUTINE XERBLA( SRNAME, INFO )
*
*  -- LAPACK auxiliary routine --
*     Argonne National Laboratory
*     November 16, 1988
*
*     .. Scalar Arguments ..
      CHARACTER*6        SRNAME
      INTEGER            INFO
*     ..
*
*  Purpose
*  =======
*
*     XERBLA  is an error handler for the LAPACK routines.
*     It is called by an LAPACK routine if an input parameter has an
*     invalid value.  A message is printed and execution stops.
*
*     Installers may consider modifying the STOP statement in order to
*     call system-specific exception-handling facilities.
*
*  Parameters
*  ==========
*
*  SRNAME - CHARACTER*6.
*           On entry, SRNAME specifies the name of the routine which
*           called XERBLA.
*
*  INFO   - INTEGER.
*           On entry, INFO specifies the position of the invalid
*           parameter in the parameter-list of the calling routine.
*
*
      WRITE( *, FMT = 9999 )SRNAME, INFO
*
      STOP
*
 9999 FORMAT( ' ** On entry to ', A6, ' parameter number ', I2, ' had ',
     $      'an illegal value' )
*
*     End of XERBLA
*
      END
