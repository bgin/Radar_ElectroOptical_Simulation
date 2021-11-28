!** DFEHL
SUBROUTINE DFEHL(DF,Neq,T,Y,H,Yp,F1,F2,F3,F4,F5,Ys)
  !> Subsidiary to DDERKF
  !***
  ! **Library:**   SLATEC
  !***
  ! **Type:**      DOUBLE PRECISION (DEFEHL-S, DFEHL-D)
  !***
  ! **Author:**  Watts, H. A., (SNLA)
  !***
  ! **Description:**
  !
  !     Fehlberg Fourth-Fifth Order Runge-Kutta Method
  !- *********************************************************************
  !
  !    DFEHL integrates a system of NEQ first order
  !    ordinary differential equations of the form
  !               DU/DX = DF(X,U)
  !    over one step when the vector Y(*) of initial values for U(*) and
  !    the vector YP(*) of initial derivatives, satisfying  YP = DF(T,Y),
  !    are given at the starting point X=T.
  !
  !    DFEHL advances the solution over the fixed step H and returns
  !    the fifth order (sixth order accurate locally) solution
  !    approximation at T+H in the array YS(*).
  !    F1,---,F5 are arrays of dimension NEQ which are needed
  !    for internal storage.
  !    The formulas have been grouped to control loss of significance.
  !    DFEHL should be called with an H not smaller than 13 units of
  !    roundoff in T so that the various independent arguments can be
  !    distinguished.
  !
  !    This subroutine has been written with all variables and statement
  !    numbers entirely compatible with DRKFS. For greater efficiency,
  !    the call to DFEHL can be replaced by the module beginning with
  !    line 222 and extending to the last line just before the return
  !    statement.
  !
  !- *********************************************************************
  !
  !***
  ! **See also:**  DDERKF
  !***
  ! **Routines called:**  (NONE)

  !* REVISION HISTORY  (YYMMDD)
  !   820301  DATE WRITTEN
  !   890831  Modified array declarations.  (WRB)
  !   891214  Prologue converted to Version 4.0 format.  (BAB)
  !   900328  Added TYPE section.  (WRB)
  !   910722  Updated AUTHOR section.  (ALS)

  !
  INTERFACE
    SUBROUTINE DF(X,U,Uprime)
      IMPORT DP
      REAL(DP), INTENT(IN) :: X
      REAL(DP), INTENT(IN) :: U(:)
      REAL(DP), INTENT(OUT) :: Uprime(:)
    END SUBROUTINE DF
  END INTERFACE
  INTEGER, INTENT(IN) :: Neq
  REAL(DP), INTENT(IN) :: H, T
  REAL(DP), INTENT(IN) :: Y(Neq), Yp(Neq)
  REAL(DP), INTENT(OUT) :: F1(Neq), F2(Neq), F3(Neq), F4(Neq), F5(Neq), Ys(Neq)
  !
  INTEGER :: k
  REAL(DP) :: ch
  !
  !* FIRST EXECUTABLE STATEMENT  DFEHL
  ch = H/4._DP
  DO k = 1, Neq
    Ys(k) = Y(k) + ch*Yp(k)
  END DO
  CALL DF(T+ch,Ys,F1)
  !
  ch = 3._DP*H/32._DP
  DO k = 1, Neq
    Ys(k) = Y(k) + ch*(Yp(k)+3._DP*F1(k))
  END DO
  CALL DF(T+3._DP*H/8._DP,Ys,F2)
  !
  ch = H/2197._DP
  DO k = 1, Neq
    Ys(k) = Y(k) + ch*(1932._DP*Yp(k)+(7296._DP*F2(k)-7200._DP*F1(k)))
  END DO
  CALL DF(T+12._DP*H/13._DP,Ys,F3)
  !
  ch = H/4104._DP
  DO k = 1, Neq
    Ys(k) = Y(k) + ch*((8341._DP*Yp(k)-845._DP*F3(k))+(29440._DP*F2(k)-32832._DP*F1(k)))
  END DO
  CALL DF(T+H,Ys,F4)
  !
  ch = H/20520._DP
  DO k = 1, Neq
    Ys(k) = Y(k) + ch*((-6080._DP*Yp(k)+(9295._DP*F3(k)-5643._DP*F4(k)))&
      +(41040._DP*F1(k)-28352._DP*F2(k)))
  END DO
  CALL DF(T+H/2._DP,Ys,F5)
  !
  !     COMPUTE APPROXIMATE SOLUTION AT T+H
  !
  ch = H/7618050._DP
  DO k = 1, Neq
    Ys(k) = Y(k)&
      + ch*((902880._DP*Yp(k)+(3855735._DP*F3(k)-1371249._DP*F4(k)))&
      +(3953664._DP*F2(k)+277020._DP*F5(k)))
  END DO
  !
END SUBROUTINE DFEHL