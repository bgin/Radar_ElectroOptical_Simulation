!** HVNRM
REAL(SP) PURE FUNCTION HVNRM(V,Ncomp)
  !> Subsidiary to DEABM, DEBDF and DERKF
  !***
  ! **Library:**   SLATEC
  !***
  ! **Type:**      SINGLE PRECISION (HVNRM-S, DHVNRM-D)
  !***
  ! **Author:**  Watts, H. A., (SNLA)
  !***
  ! **Description:**
  !
  !     Compute the maximum norm of the vector V(*) of length NCOMP and
  !     return the result as HVNRM.
  !
  !***
  ! **See also:**  DEABM, DEBDF, DERKF
  !***
  ! **Routines called:**  (NONE)

  !* REVISION HISTORY  (YYMMDD)
  !   800501  DATE WRITTEN
  !   890531  Changed all specific intrinsics to generic.  (WRB)
  !   890831  Modified array declarations.  (WRB)
  !   891024  Changed routine name from VNORM to HVNRM.  (WRB)
  !   891214  Prologue converted to Version 4.0 format.  (BAB)
  !   900328  Added TYPE section.  (WRB)
  !   910722  Updated AUTHOR section.  (ALS)

  INTEGER, INTENT(IN) :: Ncomp
  REAL(SP), INTENT(IN) :: V(Ncomp)
  !* FIRST EXECUTABLE STATEMENT  HVNRM
  HVNRM = MAXVAL( ABS(V) )
  !
END FUNCTION HVNRM