!** SLVS
SUBROUTINE SLVS(Wm,Iwm,X)
  !> Subsidiary to DEBDF
  !***
  ! **Library:**   SLATEC
  !***
  ! **Type:**      SINGLE PRECISION (SLVS-S, DSLVS-D)
  !***
  ! **Author:**  Watts, H. A., (SNLA)
  !***
  ! **Description:**
  !
  !   SLVS solves the linear system in the iteration scheme for the
  !   integrator package DEBDF.
  !
  !***
  ! **See also:**  DEBDF
  !***
  ! **Routines called:**  SGBSL, SGESL
  !***
  ! COMMON BLOCKS    DEBDF1

  !* REVISION HISTORY  (YYMMDD)
  !   800901  DATE WRITTEN
  !   891214  Prologue converted to Version 4.0 format.  (BAB)
  !   900328  Added TYPE section.  (WRB)
  !   910722  Updated AUTHOR section.  (ALS)
  !   920422  Changed DIMENSION statement.  (WRB)
  USE DEBDF1, ONLY : el0_com, h_com, ier_com, miter_com, n_com
  USE lapack, ONLY : SGBTRS, SGETRS
  !
  INTEGER, INTENT(INOUT) :: Iwm(:)
  REAL(SP), INTENT(INOUT) :: Wm(:), X(n_com)
  !
  INTEGER :: i, meband, ml, mu, info
  REAL(SP) :: di, hl0, phl0, r
  !-----------------------------------------------------------------------
  ! THIS ROUTINE MANAGES THE SOLUTION OF THE LINEAR SYSTEM ARISING FROM
  ! A CHORD ITERATION.  IT IS CALLED BY STOD  IF MITER /= 0.
  ! IF MITER IS 1 OR 2, IT CALLS SGESL TO ACCOMPLISH THIS.
  ! IF MITER = 3 IT UPDATES THE COEFFICIENT H*EL0 IN THE DIAGONAL
  ! MATRIX, AND THEN COMPUTES THE SOLUTION.
  ! IF MITER IS 4 OR 5, IT CALLS SGBSL.
  ! COMMUNICATION WITH SLVS USES THE FOLLOWING VARIABLES..
  ! WM  = REAL WORK SPACE CONTAINING THE INVERSE DIAGONAL MATRIX IF MITER
  !       IS 3 AND THE LU DECOMPOSITION OF THE MATRIX OTHERWISE.
  !       STORAGE OF MATRIX ELEMENTS STARTS AT WM(3).
  !       WM ALSO CONTAINS THE FOLLOWING MATRIX-RELATED DATA..
  !       WM(1) = SQRT(UROUND) (NOT USED HERE),
  !       WM(2) = HL0, THE PREVIOUS VALUE OF H*EL0, USED IF MITER = 3.
  ! IWM = INTEGER WORK SPACE CONTAINING PIVOT INFORMATION, STARTING AT
  !       IWM(21), IF MITER IS 1, 2, 4, OR 5.  IWM ALSO CONTAINS THE
  !       BAND PARAMETERS ML = IWM(1) AND MU = IWM(2) IF MITER IS 4 OR 5.
  ! X   = THE RIGHT-HAND SIDE VECTOR ON INPUT, AND THE SOLUTION VECTOR
  !       ON OUTPUT, OF LENGTH N.
  ! TEM = VECTOR OF WORK SPACE OF LENGTH N, NOT USED IN THIS VERSION.
  ! IER = OUTPUT FLAG (IN COMMON).  IER = 0 IF NO TROUBLE OCCURRED.
  !       IER = -1 IF A SINGULAR MATRIX AROSE WITH MITER = 3.
  ! THIS ROUTINE ALSO USES THE COMMON VARIABLES EL0, H, MITER, AND N.
  !-----------------------------------------------------------------------
  !* FIRST EXECUTABLE STATEMENT  SLVS
  ier_com = 0
  SELECT CASE (miter_com)
    CASE (3)
      !
      phl0 = Wm(2)
      hl0 = h_com*el0_com
      Wm(2) = hl0
      IF( hl0/=phl0 ) THEN
        r = hl0/phl0
        DO i = 1, n_com
          di = 1._SP - r*(1._SP-1._SP/Wm(i+2))
          IF( ABS(di)==0._SP ) GOTO 100
          Wm(i+2) = 1._SP/di
        END DO
      END IF
      DO i = 1, n_com
        X(i) = Wm(i+2)*X(i)
      END DO
      RETURN
    CASE (4,5)
      !
      ml = Iwm(1)
      mu = Iwm(2)
      meband = 2*ml + mu + 1
      CALL SGBTRS('N',n_com,ml,mu,1,Wm(3:meband*n_com+2),meband,Iwm(21:n_com+20),&
        X,n_com,info)
      RETURN
    CASE DEFAULT
      CALL SGETRS('N',n_com,1,Wm(3:n_com**2+2),n_com,Iwm(21:n_com+20),X,n_com,info)
      RETURN
  END SELECT
  100  ier_com = -1
  !----------------------- END OF SUBROUTINE SLVS -----------------------
  RETURN
END SUBROUTINE SLVS