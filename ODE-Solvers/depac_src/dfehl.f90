!** DFEHL
SUBROUTINE DFEHL(DF,Neq,T,Y,H,Yp,F1,F2,F3,F4,F5,Ys)
     use mod_kinds, i4,dp
     use omp_lib
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
      IMPORT dp
      REAL(dp), INTENT(IN) :: X
      REAL(dp), INTENT(IN) :: U(:)
      REAL(dp), INTENT(OUT) :: Uprime(:)
    END SUBROUTINE DF
  END INTERFACE
  INTEGER(i4), INTENT(IN) :: Neq
  REAL(dp), INTENT(IN) :: H, T
  REAL(dp), INTENT(IN) :: Y(Neq), Yp(Neq)
  REAL(dp), INTENT(OUT) :: F1(Neq), F2(Neq), F3(Neq), F4(Neq), F5(Neq), Ys(Neq)
  !
  INTEGER(i4) :: k
  REAL(dp) :: ch
  !
  !* FIRST EXECUTABLE STATEMENT  DFEHL
  ch = H*0.25_dp
  !$OMP SIMD LINEAR(k:1) IF(Neq>=16)
  DO k = 1, Neq
    Ys(k) = Y(k) + ch*Yp(k)
  END DO
  CALL DF(T+ch,Ys,F1)
  !
  ch = 3._dp*H/32._dp
  !$OMP SIMD LINEAR(k:1) IF(Neq>=16)
  DO k = 1, Neq
    Ys(k) = Y(k) + ch*(Yp(k)+3._dp*F1(k))
  END DO
  CALL DF(T+3._dp*H*0.125_dp,Ys,F2)
  !
  ch = H/2197._dp
  !$OMP SIMD LINEAR(k:1) IF(Neq>=16)
  DO k = 1, Neq
    Ys(k) = Y(k) + ch*(1932._dp*Yp(k)+(7296._dp*F2(k)-7200._dp*F1(k)))
  END DO
  CALL DF(T+12._dp*H/13._dp,Ys,F3)
  !
  ch = H/4104._dp
  !$OMP SIMD LINEAR(k:1) IF(Neq>=16)
  DO k = 1, Neq
    Ys(k) = Y(k) + ch*((8341._dp*Yp(k)-845._dp*F3(k))+(29440._dp*F2(k)-32832._dp*F1(k)))
  END DO
  CALL DF(T+H,Ys,F4)
  !
  ch = H/20520._dp
  !$OMP SIMD LINEAR(k:1) IF(Neq>=16)
  DO k = 1, Neq
    Ys(k) = Y(k) + ch*((-6080._dp*Yp(k)+(9295._dp*F3(k)-5643._dp*F4(k)))&
      +(41040._dp*F1(k)-28352._dp*F2(k)))
  END DO
  CALL DF(T+H*0.5_dp,Ys,F5)
  !
  !     COMPUTE APPROXIMATE SOLUTION AT T+H
  !
  ch = H/7618050._dp
  !$OMP SIMD LINEAR(k:1) IF(Neq>=16)
  DO k = 1, Neq
    Ys(k) = Y(k)&
      + ch*((902880._dp*Yp(k)+(3855735._dp*F3(k)-1371249._dp*F4(k)))&
      +(3953664._dp*F2(k)+277020._dp*F5(k)))
  END DO
  !
END SUBROUTINE DFEHL
