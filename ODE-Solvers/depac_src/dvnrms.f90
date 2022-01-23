!** DVNRMS
REAL(dp) PURE FUNCTION DVNRMS(N,V,W)
      !DIR$ ATTRIBUTES FORCEINLINE :: DVNRMS
     use mod_kinds, only : i4,dp
  !> Subsidiary to DDEBDF
  !***
  ! **Library:**   SLATEC
  !***
  ! **Type:**      DOUBLE PRECISION (VNWRMS-S, DVNRMS-D)
  !***
  ! **Author:**  (UNKNOWN)
  !***
  ! **Description:**
  !
  !   DVNRMS computes a weighted root-mean-square vector norm for the
  !   integrator package DDEBDF.
  !
  !***
  ! **See also:**  DDEBDF
  !***
  ! **Routines called:**  (NONE)

  !* REVISION HISTORY  (YYMMDD)
  !   820301  DATE WRITTEN
  !   890531  Changed all specific intrinsics to generic.  (WRB)
  !   890831  Modified array declarations.  (WRB)
  !   890911  Removed unnecessary intrinsics.  (WRB)
  !   891214  Prologue converted to Version 4.0 format.  (BAB)
  !   900328  Added TYPE section.  (WRB)

  INTEGER(i4), INTENT(IN) :: N
  REAL(dp), INTENT(IN) :: V(N), W(N)
  !* FIRST EXECUTABLE STATEMENT  DVNRMS
  DVNRMS = NORM2( V/W ) / SQRT(1._dp*N)
  !----------------------- END OF FUNCTION DVNRMS ------------------------
END FUNCTION DVNRMS
