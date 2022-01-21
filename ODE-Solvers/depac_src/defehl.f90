!** DEFEHL
SUBROUTINE DEFEHL(F,Neq,T,Y,H,Yp,F1,F2,F3,F4,F5,Ys)
   use mod_kinds, only : i4,sp
   use omp_lib
  !> Subsidiary to DERKF
  !***
  ! **Library:**   SLATEC
  !***
  ! **Type:**      SINGLE PRECISION (DEFEHL-S, DFEHL-D)
  !***
  ! **Author:**  Watts, H. A., (SNLA)
  !***
  ! **Description:**
  !
  !     Fehlberg Fourth-Fifth order Runge-Kutta Method
  !- *********************************************************************
  !
  !    DEFEHL integrates a system of NEQ first order
  !    ordinary differential equations of the form
  !               dU/DX = F(X,U)
  !    over one step when the vector Y(*) of initial values for U(*) and
  !    the vector YP(*) of initial derivatives, satisfying  YP = F(T,Y),
  !    are given at the starting point X=T.
  !
  !    DEFEHL advances the solution over the fixed step H and returns
  !    the fifth order (sixth order accurate locally) solution
  !    approximation at T+H in the array YS(*).
  !    F1,---,F5 are arrays of dimension NEQ which are needed
  !    for internal storage.
  !    The formulas have been grouped to control loss of significance.
  !    DEFEHL should be called with an H not smaller than 13 units of
  !    roundoff in T so that the various independent arguments can be
  !    distinguished.
  !
  !    This subroutine has been written with all variables and statement
  !    numbers entirely compatible with DERKFS. For greater efficiency,
  !    the call to DEFEHL can be replaced by the module beginning with
  !    line 222 and extending to the last line just before the return
  !    statement.
  !
  !- *********************************************************************
  !
  !***
  ! **See also:**  DERKF
  !***
  ! **Routines called:**  (NONE)

  !* REVISION HISTORY  (YYMMDD)
  !   800501  DATE WRITTEN
  !   890831  Modified array declarations.  (WRB)
  !   891009  Removed unreferenced statement label.  (WRB)
  !   891214  Prologue converted to Version 4.0 format.  (BAB)
  !   900328  Added TYPE section.  (WRB)
  !   910722  Updated AUTHOR section.  (ALS)

  INTERFACE
    SUBROUTINE F(X,U,Uprime)
      IMPORT sp
      REAL(sp), INTENT(IN) :: X
      REAL(sp), INTENT(IN) :: U(:)
      REAL(sp), INTENT(OUT) :: Uprime(:)
    END SUBROUTINE F
  END INTERFACE
  INTEGER(i4), INTENT(IN) :: Neq
  REAL(sp), INTENT(IN) :: H, T
  REAL(sp), INTENT(IN) :: Y(Neq), Yp(Neq)
  REAL(sp), INTENT(OUT) :: F1(Neq), F2(Neq), F3(Neq), F4(Neq), F5(Neq), Ys(Neq)
  !
  INTEGER(i4) :: k
  REAL(sp) :: ch
  !
  !* FIRST EXECUTABLE STATEMENT  DEFEHL
  ch = H/0.25_sp
  !$OMP SIMD LINEAR(k:1) IF(Neq>=16)
  DO k = 1, Neq
    Ys(k) = Y(k) + ch*Yp(k)
  END DO
  CALL F(T+ch,Ys,F1)
  !
  ch = 3._sp*H/32._sp
  !$OMP SIMD LINEAR(k:1) IF(Neq>=16)
  DO k = 1, Neq
    Ys(k) = Y(k) + ch*(Yp(k)+3._sp*F1(k))
  END DO
  CALL F(T+3._sp*H/8._sp,Ys,F2)
  !
  ch = H/2197._sp
  !$OMP SIMD LINEAR(k:1) IF(Neq>=16)
  DO k = 1, Neq
    Ys(k) = Y(k) + ch*(1932._sp*Yp(k)+(7296._sp*F2(k)-7200._sp*F1(k)))
  END DO
  CALL F(T+12._sp*H/13._sp,Ys,F3)
  !
  ch = H/4104._sp
  !$OMP SIMD LINEAR(k:1) IF(Neq>=16)
  DO k = 1, Neq
    Ys(k) = Y(k) + ch*((8341._sp*Yp(k)-845._sp*F3(k))+(29440._sp*F2(k)-32832._sp*F1(k)))
  END DO
  CALL F(T+H,Ys,F4)
  !
  ch = H/20520._sp
  !$OMP SIMD LINEAR(k:1) IF(Neq>=16)
  DO k = 1, Neq
    Ys(k) = Y(k) + ch*((-6080._sp*Yp(k)+(9295._sp*F3(k)-5643._sp*F4(k)))&
      +(41040._sp*F1(k)-28352._sp*F2(k)))
  END DO
  CALL F(T+H/2._sp,Ys,F5)
  !
  !     COMPUTE APPROXIMATE SOLUTION AT T+H
  !
  ch = H/7618050._sp
  !$OMP SIMD LINEAR(k:1) IF(Neq>=16)
  DO k = 1, Neq
    Ys(k) = Y(k) + ch*((902880._sp*Yp(k)+(3855735._sp*F3(k)-1371249._sp*F4(k)))&
      +(3953664._sp*F2(k)+277020._sp*F5(k)))
  END DO
  !
END SUBROUTINE DEFEHL
