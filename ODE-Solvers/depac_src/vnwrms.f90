!** VNWRMS
REAL(SP) PURE FUNCTION VNWRMS(N,V,W)
  !> Subsidiary to DEBDF
  !***
  ! **Library:**   SLATEC
  !***
  ! **Type:**      SINGLE PRECISION (VNWRMS-S, DVNRMS-D)
  !***
  ! **Author:**  (UNKNOWN)
  !***
  ! **Description:**
  !
  !   VNWRMS computes a weighted root-mean-square vector norm for the
  !   integrator package DEBDF.
  !
  !***
  ! **See also:**  DEBDF
  !***
  ! **Routines called:**  (NONE)

  !* REVISION HISTORY  (YYMMDD)
  !   800901  DATE WRITTEN
  !   890531  Changed all specific intrinsics to generic.  (WRB)
  !   890831  Modified array declarations.  (WRB)
  !   891214  Prologue converted to Version 4.0 format.  (BAB)
  !   900328  Added TYPE section.  (WRB)

  !
  !-----------------------------------------------------------------------
  ! THIS FUNCTION ROUTINE COMPUTES THE WEIGHTED ROOT-MEAN-SQUARE NORM
  ! OF THE VECTOR OF LENGTH N CONTAINED IN THE ARRAY V, WITH WEIGHTS
  ! CONTAINED IN THE ARRAY W OF LENGTH N..
  !   VNWRMS = SQRT( (1/N) * SUM( V(I)/W(I) )**2 )
  !-----------------------------------------------------------------------
  INTEGER, INTENT(IN) :: N
  REAL(SP), INTENT(IN) :: V(N), W(N)
  !* FIRST EXECUTABLE STATEMENT  VNWRMS
  VNWRMS = NORM2( V/W ) / SQRT(1._SP*N)
  !----------------------- END OF FUNCTION VNWRMS ------------------------
END FUNCTION VNWRMS