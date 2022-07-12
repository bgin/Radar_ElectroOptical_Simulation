

module mol_solvers


!============================================================
! This module contains various subroutines adapted from
! the codes of William E. Schiesser.
! Reference:
!               https://www.lehigh.edu/~wes1/books/
! 
!
!===========================================================

  use mod_kinds, only : i4,sp,sp
  public
  implicit none

  !================================================================!
  !           Finite-Difference Scheme collection                  !
  !================================================================!

  
      SUBROUTINE DSS002(XL,XU,N,U,UX)
          !dir$ optimize:3
          !dir$ attributes forceinline :: DSS002
          !dir$ attributes optimization_parameter: "target_arch=skylake-avx512" :: DSS002
#if 0
C...
C...  SUBROUTINE DSS002 COMPUTES THE FIRST DERIVATIVE, U , OF A
C...                                                    X
C...  VARIABLE U OVER THE SPATIAL DOMAIN XL LE X LE XU
C...
C...  ARGUMENT LIST
C...
C...     XL      LOWER BOUNDARY VALUE OF X (INPUT)
C...
C...     XU      UPPER BOUNDARY VALUE OF X (INPUT)
C...
C...     N       NUMBER OF GRID POINTS IN THE X DOMAIN INCLUDING THE
C...             BOUNDARY POINTS (INPUT)
C...
C...     U       ONE-DIMENSIONAL ARRAY CONTAINING THE VALUES OF U AT
C...             THE N GRID POINT POINTS FOR WHICH THE DERIVATIVE IS
C...             TO BE COMPUTED (INPUT)
C...
C...     UX      ONE-DIMENSIONAL ARRAY CONTAINING THE NUMERICAL
C...             VALUES OF THE DERIVATIVES OF U AT THE N GRID POINTS
C...             (OUTPUT)
C...
C...  SUBROUTINE DSS002 COMPUTES THE FIRST DERIVATIVE, U , OF A
C...                                                    X
C...  VARIABLE U OVER THE SPATIAL DOMAIN XL LE X LE XU FROM THE
C...  CLASSICAL THREE-POINT, SECOND-ORDER FINITE DIFFERENCE APPROXI-
C...  TIONS
C...
C...                                       2
C...  U1  = (1/2DX)(-3U1 + 4U2 - U3) + O(DX ) (LEFT BOUNDARY,     (1)
C...    X                                         X = XL)
C...
C...                                   2
C...  UI  = (1/2DX)(UI+1 - UI-1) + O(DX ) (INTERIOR POINT,        (2)
C...    X                                   X NE XL, XU)
C...
C...                                          2
C...  UN  = (1/2DX)(3UN - 4UN-1 + UN-2) + O(DX ) (RIGHT BOUNDARY, (3)
C...    X                                            X = XU)
C...
C...  EQUATIONS (1) TO (3) APPLY OVER A GRID IN X WITH CORRESPONDING
C...  VALUES OF THE FUNCTION U(X) REPRESENTED AS
C...
C...   U1      U2       U3         UI        UN-2      UN-1    UN
C...
C...  X=XL  X=XL+DX  X=XL+2DX ... X=XI ... X=XU-2DX  X=XU-DX  X=XU
C...
C...  THE ORIGIN OF EQUATIONS (1) TO (3) IS OUTLINED BELOW.
C...
C...  CONSIDER THE FOLLOWING POLYNOMIAL IN X OF ARBITRARY ORDER
C...
C...                                     2             3
C...  U(X) = A0 + A1(X - X0) + A2(X - X0)  + A3(X - X0)  + ....   (4)
C...
C...  WE SEEK THE VALUES OF THE COEFFICIENTS A0, A1, A2, ... FOR A
C...  PARTICULAR FUNCTION U(X).  IF X = X0 IS SUBSTITUTED IN EQUATION
C...  (4), WE HAVE IMMEDIATELY A0 = U(X0).  NEXT, IF EQUATION (4) IS
C...  DIFFERENTIATED WITH RESPECT TO X,
C...
C...                                                   2
C...  DU(X)/DX = U (X) = A1 + 2A2(X - X0) + 3A3(X - X0)  + ...    (5)
C...              X
C...
C...  AGAIN, WITH X = X0, A1 = DU(X0)/DX = U (X0).  DIFFERENTIATION
C...                                        X
C...  OF EQUATION (5) IN TURN GIVES
C...
C...  D2U(X)/DX2 = U  (X) = 2A2 + 6A3(X - X0) + ...
C...                2X
C...
C...  AND FOR X = X0, A2 = U  (X0)/2F (2F = 1*2, I.E., 2 FACTORIAL).
C...                        2X
C...
C...  WE CAN CONTINUE THIS PROCESS OF DIFFERENTIATION FOLLOWED BY THE
C...  SUBSTITUTION X = X0 TO OBTAIN THE SUCCESSIVE COEFFICIENTS IN
C...  EQUATION (4), A3, A4, ...  FINALLY, SUBSTITUTION OF THESE CO-
C...  EFFICIENTS IN EQUATION (4) GIVES
C...
C...                                                 2
C...  U(X) = U(X0) + U (X0)(X - X0) + U  (X0)(X - X0)  +
C...                  X       1F       2X       2F
C...                                                              (6)
C...                                3                  4
C...                 U  (X0)(X - X0)  + U  (X0)(X - X0)  + ...
C...                  3X       3F        4X       4F
C...
C...  THE CORRESPONDENCE BETWEEN EQUATION (6) AND THE WELL-KNOWN
C...  TAYLOR SERIES SHOULD BE CLEAR.  THUS THE EXPANSION OF A
C...  FUNCTION, U(X), AROUND A NEIGHBORING POINT X0 IN TERMS OF U(X0)
C...  AND THE DERIVATIVES OF U(X) AT X = X0 IS EQUIVALENT TO APPROXI-
C...  MATING U(X) NEAR X0 BY A POLYNOMIAL.
C...
C...  EQUATION (6) IS THE STARTING POINT FOR THE DERIVATION OF THE
C...  CLASSICAL FINITE DIFFERENCE APPROXIMATIONS OF DERIVATIVES SUCH
C...  AS THE THREE-POINT FORMULAS OF EQUATIONS (1), (2) AND (3).  WE
C...  WILL NOW CONSIDER THE DERIVATION OF THESE THREE-POINT FORMULAS
C...  IN A STANDARD FORMAT WHICH CAN THEN BE EXTENDED TO HIGHER
C...  MULTI-POINT FORMULAS IN OTHER SUBROUTINES, E.G., FIVE-POINT
C...  FORMULAS IN SUBROUTINE DSS004.
C...
C...  THREE-POINT FORMULAS
C...
C...     (1)  LEFT END, POINT I = 1
C...
C...  IF EQUATION (6) IS WRITTEN AROUND THE POINT X = XL FOR X = XL +
C...  DX AND X = XL + 2DX, FOR WHICH THE CORRESPONDING VALUES OF U(X)
C...  ARE U1, U2 AND U3 (U1 AND U2 ARE SEPARATED WITH RESPECT TO X BY
C...  DISTANCE DX AS ARE U2 AND U3, I.E., WE ASSUME A UNIFORM GRID
C...  SPACING, DX, FOR INDEPENDENT VARIABLE X)
C...
C...                                2            3
C...  U2 = U1 + U1 ( DX) + U1  ( DX)  + U1  ( DX)  + ...          (7)
C...              X  1F      2X  2F       3X  3F
C...
C...                                2            3
C...  U3 = U1 + U1 (2DX) + U1  (2DX)  + U1  (2DX)  + ...          (8)
C...              X  1F      2X  2F       3X  3F
C...
C...  WE CAN NOW TAKE A LINEAR COMBINATION OF EQUATIONS (7) AND (8)
C...  BY FIRST MULTIPLYING EQUATION (7) BY A CONSTANT, A, AND EQUA-
C...  TION (8) BY CONSTANT B
C...
C...                                  2           3
C...  A(U2 = U1 + U1 ( DX) + U1  ( DX) + U1  ( DX) + ...)         (9)
C...                X  1F      2X  2F      3X  3F
C...
C...                                  2           3
C...  B(U3 = U1 + U1 (2DX) + U1  (2DX) + U1  (2DX) + ...)        (10)
C...                X  1F      2X  2F      3X  3F
C...
C...  CONSTANTS A AND B ARE THEN SELECTED SO THAT THE COEFFICIENTS OF
C...  THE U1  TERMS SUM TO ONE (SINCE WE ARE INTERESTED IN OBTAINING
C...        X
C...  A FINITE DIFFERENCE APPROXIMATION FOR THIS FIRST DERIVATIVE).
C...  ALSO, WE SELECT A AND B SO THAT THE COEFFICIENTS OF THE U1
C...                                                            2X
C...  TERMS SUM TO ZERO IN ORDER TO DROP OUT THE CONTRIBUTION OF THIS
C...  SECOND DERIVATIVE (THE BASIC IDEA IS TO DROP OUT AS MANY OF THE
C...  DERIVATIVES AS POSSIBLE IN THE TAYLOR SERIES BEYOND THE DERI-
C...  VATIVE OF INTEREST, IN THIS CASE U1 , IN ORDER TO PRODUCE A
C...                                     X
C...  FINITE DIFFERENCE APPROXIMATION FOR THE DERIVATIVE OF MAXIMUM
C...  ACCURACY).  IN THIS CASE WE HAVE ONLY TWO CONSTANTS, A AND B,
C...  TO SELECT SO WE CAN DROP OUT ONLY THE SECOND DERIVATIVE, U1  ,
C...                                                             2X
C...  IN THE TAYLOR SERIES (IN ADDITION TO RETAINING THE FIRST DERI-
C...  VATIVE).  THIS PROCEDURE LEADS TO TWO LINEAR ALGEBRAIC EQUA-
C...  TIONS IN THE TWO CONSTANTS
C...
C...  A + 2B = 1
C...
C...  A + 4B = 0
C...
C...  SOLUTION OF THESE EQUATIONS FOR A AND B GIVES
C...
C...  A = 2, B = -1/2
C...
C...  SOLUTION OF EQUATIONS (9) AND (10) FOR U1  WITH THESE VALUES OF
C...  A AND B GIVES EQUATION (1)               X
C...
C...                                      2
C...  U1 = (1/2DX)(-3U1 + 4U2 - U3) + O(DX )                      (1)
C...    X
C...               2
C...  THE TERM O(DX ) INDICATES A PRINCIPAL ERROR TERM DUE TO TRUNCA-
C...                                                2
C...  TION OF THE TAYLOR SERIES WHICH IS OF ORDER DX .  THIS TERM IN
C...                    2
C...  FACT EQUALS U1  DX /3F, WHICH IS EASILY OBTAINED IN DERIVING
C...                3X
C...  EQUATION (1).
C...
C...  THIS SAME BASIC PROCEDURE CAN NOW BE APPLIED TO THE DERIVATION
C...  OF EQUATIONS (2) AND (3).
C...
C...     (2)  INTERIOR POINT I
C...
C...                                    2           3
C...  A(UI-1 = UI + UI (-DX) + UI  (-DX) + UI  (-DX) + ...)
C...                  X  1F      2X  2F      3X  3F
C...
C...                                    2           3
C...  B(UI+1 = UI + UI ( DX) + UI  ( DX) + UI  ( DX) + ...)
C...                  X  1F      2X  2F      3X  3F
C...
C...  -A + B = 1
C...
C...   A + B = 0
C...
C...  A = -1/2, B = 1/2
C...                                   2
C...  UI  = (1/2DX)(UI+1 - UI-1) + O(DX )                         (2)
C...    X
C...
C...     (3)  RIGHT END, POINT I = N
C...
C...                                      2            3
C...  A(UN-2 = UN + UN (-2DX) + UN  (-2DX) + UN  (-2DX) + ...)
C...                  X   1F      2X   2F      3X   3F
C...
C...                                      2            3
C...  B(UN-1 = UN + UN ( -DX) + UN  ( -DX) + UN  ( -DX) + ...)
C...                  X   1F      2X   2F      3X   3F
C...
C...  -2A - B = 1
C...
C...   4A + B = 0
C...
C...   A = -2, B = 1/2
C...                                          2
C...  UN  = (1/2DX)(3UN - 4UN-1 + UN-2) + O(DX )                  (3)
C...    X
C...
C...  THE WEIGHTING COEFFICIENTS FOR EQUATIONS (1), (2) AND (3) CAN
C...  BE SUMMARIZED AS
C...
C...          -3   4  -1
C...
C...     1/2  -1   0   1
C...
C...           1  -4   3
C...
C...  WHICH ARE THE COEFFICIENTS REPORTED BY BICKLEY FOR N = 2, M =
C...  1, P = 0, 1, 2 (BICKLEY, W. G., FORMULAE FOR NUMERICAL DIFFER-
C...  ENTIATION, MATH. GAZ., VOL. 25, 1941).
C...
C...  EQUATIONS (1), (2) AND (3) CAN NOW BE PROGRAMMED TO GENERATE
C...  THE DERIVATIVE U (X) OF FUNCTION U(X) (ARGUMENTS U AND UX OF
C...                  X
C...  SUBROUTINE DSS002, RESPECTIVELY).
C...
C...  TYPE SELECTED REAL VARIABLES AS DOUBLE PRECISION
#endif
      DOUBLE PRECISION     DX,  R2FDX,      U,     UX,     XL,     XU
       !               DFLOAT
!C...
      DIMENSION U(N),UX(N)
!C...
!C...  COMPUTE THE SPATIAL INCREMENT
      DX=(XU-XL)/DFLOAT(N-1)
      R2FDX=1.D+00/(2.D+00*DX)
      NM1=N-1
!C...
!C...  EQUATION (1) (NOTE - THE RHS OF THE FINITE DIFFERENCE APPROXI-
!C...  TIONS, EQUATIONS (1), (2) AND (3) HAVE BEEN FORMATTED SO THAT
!C...  THE NUMERICAL WEIGHTING COEFFICIENTS CAN BE MORE EASILY ASSOCI-
!C...  ATED WITH THE BICKLEY MATRIX LISTED ABOVE)
      UX(1)=R2FDX*(-3.D+00*U(1)+4.D+00*U(2)-1.D+00*U(3)) 
      
!C...
!C...  EQUATION (2)
      !dir$ assume_aligned UX:64,U:64
      !dir$ vector aligned
      !dir$ vector vectorlength(8)
      !dir$ ivdep
      !dir$ vector always
      DO  I=2,NM1
          UX(I)=R2FDX*(-1.D+00*U(I-1)+0.D+00*U(I)+1.D+00*U(I+1))  
      END DO
!C...
!C...  EQUATION (3)
      UX(N)=R2FDX*(1.D+00*U(N-2)-4.D+00*U(N-1)+3.D+00*U(  N))
                
      
    END SUBROUTINE


    
   SUBROUTINE DSS004(XL,XU,N,U,UX)
          !dir$ optimize:3
          !dir$ attributes forceinline :: DSS004
          !dir$ attributes optimization_parameter: "target_arch=skylake-avx512" :: DSS004
#if 0
C...
C...  SUBROUTINE DSS004 COMPUTES THE FIRST DERIVATIVE, U , OF A
C...                                                    X
C...  VARIABLE U OVER THE SPATIAL DOMAIN XL LE X LE XU FROM CLASSICAL
C...  FIVE-POINT, FOURTH-ORDER FINITE DIFFERENCE APPROXIMATIONS
C...
C...  ARGUMENT LIST
C...
C...     XL      LOWER BOUNDARY VALUE OF X (INPUT)
C...
C...     XU      UPPER BOUNDARY VALUE OF X (INPUT)
C...
C...     N       NUMBER OF GRID POINTS IN THE X DOMAIN INCLUDING THE
C...             BOUNDARY POINTS (INPUT)
C...
C...     U       ONE-DIMENSIONAL ARRAY CONTAINING THE VALUES OF U AT
C...             THE N GRID POINT POINTS FOR WHICH THE DERIVATIVE IS
C...             TO BE COMPUTED (INPUT)
C...
C...     UX      ONE-DIMENSIONAL ARRAY CONTAINING THE NUMERICAL
C...             VALUES OF THE DERIVATIVES OF U AT THE N GRID POINTS
C...             (OUTPUT)
C...
C...  THE MATHEMATICAL DETAILS OF THE FOLLOWING TAYLOR SERIES (OR
C...  POLYNOMIALS) ARE GIVEN IN SUBROUTINE DSS002.
C...
C...  FIVE-POINT FORMULAS
C...
C...     (1)  LEFT END, POINT I = 1
C...
C...                                   2            3            4
C...  A(U2 = U1 + U1  ( DX) + U1  ( DX)  + U1  ( DX)  + U1  ( DX)
C...                X   1F      2X  2F       3X  3F       4X  4F
C...
C...                      5            6            7
C...           + U1  ( DX)  + U1  ( DX)  + U1  ( DX)  + ...)
C...               5X  5F       6X  6F       7X  7F
C...
C...                                   2            3            4
C...  B(U3 = U1 + U1  (2DX) + U1  (2DX)  + U1  (2DX)  + U1  (2DX)
C...                X   1F      2X  2F       3X  3F       4X  4F
C...
C...                      5            6            7
C...           + U1  (2DX)  + U1  (2DX)  + U1  (2DX)  + ...)
C...               5X  5F       6X  6F       7X  7F
C...
C...                                   2            3            4
C...  C(U4 = U1 + U1  (3DX) + U1  (3DX)  + U1  (3DX)  + U1  (3DX)
C...                X   1F      2X  2F       3X  3F       4X  4F
C...
C...                      5            6            7
C...           + U1  (3DX)  + U1  (3DX)  + U1  (3DX)  + ...)
C...               5X  5F       6X  6F       7X  7F
C...
C...                                   2            3            4
C...  D(U5 = U1 + U1  (4DX) + U1  (4DX)  + U1  (4DX)  + U1  (4DX)
C...                X   1F      2X  2F       3X  3F       4X  4F
C...
C...                      5            6            7
C...           + U1  (4DX)  + U1  (4DX)  + U1  (4DX)  + ...)
C...               5X  5F       6X  6F       7X  7F
C...
C...  CONSTANTS A, B, C AND D ARE SELECTED SO THAT THE COEFFICIENTS
C...  OF THE U1  TERMS SUM TO ONE AND THE COEFFICIENTS OF THE U1  ,
C...           X                                                2X
C...  U1   AND U1   TERMS SUM TO ZERO
C...    3X       4X
C...
C...  A +   2B +   3C +   4D = 1
C...
C...  A +   4B +   9C +  16D = 0
C...
C...  A +   8B +  27C +  64D = 0
C...
C...  A +  16B +  81C + 256D = 0
C...
C...  SIMULTANEOUS SOLUTION FOR A, B, C AND D FOLLOWED BY THE SOLU-
C...  TION OF THE PRECEDING TAYLOR SERIES, TRUNCATED AFTER THE U
C...                                                            4X
C...  TERMS, FOR U1  GIVES THE FOLLOWING FIVE-POINT APPROXIMATION
C...               X
C...                                                         4
C...  U1  = (1/12DX)(-25U1 + 48U2 - 36U3 + 16U4 - 3U5) + O(DX )   (1)
C...    X
C...
C...     (2)  INTERIOR POINT, I = 2
C...
C...                                   2            3            4
C...  A(U1 = U2 + U2  (-DX) + U2  (-DX)  + U2  (-DX)  + U2  (-DX)
C...                X   1F      2X  2F       3X  3F       4X  4F
C...
C...                      5            6            7
C...           + U2  (-DX)  + U2  (-DX)  + U2  (-DX)  + ...)
C...               5X  5F       6X  6F       7X  7F
C...
C...                                   2            3            4
C...  B(U3 = U2 + U2  ( DX) + U2  ( DX)  + U2  ( DX)  + U2  ( DX)
C...                X   1F      2X  2F       3X  3F       4X  4F
C...
C...                      5            6            7
C...           + U2  ( DX)  + U2  ( DX)  + U2  ( DX)  + ...)
C...               5X  5F       6X  6F       7X  7F
C...
C...                                   2            3            4
C...  C(U4 = U2 + U2  (2DX) + U2  (2DX)  + U2  (2DX)  + U2  (2DX)
C...                X   1F      2X  2F       3X  3F       4X  4F
C...
C...                      5            6            7
C...           + U2  (2DX)  + U2  (2DX)  + U2  (2DX)  + ...)
C...               5X  5F       6X  6F       7X  7F
C...
C...                                   2            3            4
C...  D(U5 = U2 + U2  (3DX) + U2  (3DX)  + U2  (3DX)  + U2  (3DX)
C...                X   1F      2X  2F       3X  3F       4X  4F
C...
C...                      5            6            7
C...           + U2  (3DX)  + U2  (3DX)  + U2  (3DX)  + ...)
C...               5X  5F       6X  6F       7X  7F
C...
C...  -A +   B +  2C +  3D = 1
C...
C...   A +   B +  4C +  9D = 0
C...
C...  -A +   B +  8C + 27D = 0
C...
C...   A +   B + 16C + 81D = 0
C...
C...  SIMULTANEOUS SOLUTION FOR A, B, C AND D FOLLOWED BY THE SOLU-
C...  TION OF THE PRECEDING TAYLOR SERIES, TRUNCATED AFTER THE U
C...                                                            4X
C...  TERMS, FOR U1  GIVES THE FOLLOWING FIVE-POINT APPROXIMATION
C...               X
C...                                                        4
C...  U2  = (1/12DX)(-3U1 - 10U2 + 18U3 -  6U4 +  U5) + O(DX )    (2)
C...    X
C...
C...     (3)  INTERIOR POINT I, I NE 2, N-1
C...
C...                                        2             3
C...  A(UI-2 = UI + UI  (-2DX)  + UI  (-2DX)  + UI  (-2DX)
C...                  X    1F       2X   2F       3X   3F
C...
C...                          4             5             6
C...              + UI  (-2DX)  + UI  (-2DX)  + UI  (-2DX)  + ...)
C...                  4X   4F       5X   5F       6X   6F
C...
C...                                        2             3
C...  B(UI-1 = UI + UI  ( -DX)  + UI  ( -DX)  + UI  ( -DX)
C...                  X    1F       2X   2F       3X   3F
C...
C...                          4             5             6
C...              + UI  ( -DX)  + UI  ( -DX)  + UI  ( -DX)  + ...)
C...                  4X   4F       5X   5F       6X   6F
C...
C...                                        2             3
C...  C(UI+1 = UI + UI  (  DX)  + UI  (  DX)  + UI  (  DX)
C...                  X    1F       2X   2F       3X   3F
C...
C...                          4             5             6
C...              + UI  (  DX)  + UI  (  DX)  + UI  (  DX)  + ...)
C...                  4X   4F       5X   5F       6X   6F
C...
C...                                        2             3
C...  D(UI+2 = UI + UI  ( 2DX)  + UI  ( 2DX)  + UI  ( 2DX)
C...                  X    1F       2X   2F       3X   3F
C...
C...                          4             5             6
C...              + UI  ( 2DX)  + UI  ( 2DX)  + UI  ( 2DX)  + ...)
C...                  4X   4F       5X   5F       6X   6F
C...
C...   -2A -   B +   C +  2D = 1
C...
C...    4A +   B +   C +  4D = 0
C...
C...   -8A -   B +   C +  8D = 0
C...
C...   16A +   B +   C + 16D = 0
C...
C...  SIMULTANEOUS SOLUTION FOR A, B, C AND D FOLLOWED BY THE SOLU-
C...  TION OF THE PRECEDING TAYLOR SERIES, TRUNCATED AFTER THE U
C...                                                            4X
C...  TERMS, FOR U1  GIVES THE FOLLOWING FIVE-POINT APPROXIMATION
C...               X
C...                                                          4
C...  UI  = (1/12DX)(UI-2 - 8UI-1 + 0UI + 8UI+1 - UI+2) + O(DX )  (3)
C...    X
C...
C...     (4)  INTERIOR POINT, I = N-1
C...
C...                                              2               3
C...  A(UN-4 = UN-1 + UN-1  (-3DX)  + UN-1  (-3DX)  + UN-1  (-3DX)
C...                      X    1F         2X   2F         3X   3F
C...
C...                       4               5               6
C...         + UN-1  (-3DX)  + UN-1  (-3DX)  + UN-1  (-3DX)  + ...
C...               4X   4F         5X   5F         6X   6F
C...
C...                                              2               3
C...  B(UN-3 = UN-1 + UN-1  (-2DX)  + UN-1  (-2DX)  + UN-1  (-2DX)
C...                      X    1F         2X   2F         3X   3F
C...
C...                       4               5               6
C...         + UN-1  (-2DX)  + UN-1  (-2DX)  + UN-1  (-2DX)  + ...
C...               4X   4F         5X   5F         6X   6F
C...
C...                                              2               3
C...  C(UN-2 = UN-1 + UN-1  ( -DX)  + UN-1  (- -X)  + UN-1  ( -DX)
C...                      X    1F         2X   2F         3X   3F
C...
C...                       4               5               6
C...         + UN-1  ( -DX)  + UN-1  ( -DX)  + UN-1  ( -DX)  + ...
C...               4X   4F         5X   5F         6X   6F
C...
C...                                              2               3
C...  D(UN   = UN-1 + UN-1  (  DX)  + UN-1  (  DX)  + UN-1  (  DX)
C...                      X    1F         2X   2F         3X   3F
C...
C...                       4               5               6
C...         + UN-1  (  DX)  + UN-1  (  DX)  + UN-1  (  DX)  + ...
C...               4X   4F         5X   5F         6X   6F
C...
C...  -3A -  2B -   C +   D = 1
C...
C...   9A +  4B +   C +   D = 0
C...
C... -27A -  8B -   C +   D = 0
C...
C...  81A + 16B +   C +   D = 0
C...
C...  SIMULTANEOUS SOLUTION FOR A, B, C AND D FOLLOWED BY THE SOLU-
C...  TION OF THE PRECEDING TAYLOR SERIES, TRUNCATED AFTER THE U
C...                                                            4X
C...  TERMS, FOR U1  GIVES THE FOLLOWING FIVE-POINT APPROXIMATION
C...               X
C...                                                                4
C...  UN-1  = (1/12DX)(-UN-4 + 6UN-3 - 18UN-2 + 10UN-1 + 3UN) + O(DX )
C...      X
C...                                                              (4)
C...
C...    (5)  RIGHT END, POINT I = N
C...
C...                                       2             3
C...  A(UN-4 = UN + UN (-4DX)  + UN  (-4DX)  + UN  (-4DX)
C...                  X   1F       2X   2F       3X   3F
C...
C...                         4             5             6
C...             + UN  (-4DX)  + UN  (-4DX)  + UN  (-4DX)  + ...)
C...                 4X   4F       5X   5F       6X   6F
C...
C...                                       2             3
C...  B(UN-3 = UN + UN (-3DX)  + UN  (-3DX)  + UN  (-3DX)
C...                  X   1F       2X   2F       3X   3F
C...
C...                         4             5             6
C...             + UN  (-3DX)  + UN  (-3DX)  + UN  (-3DX)  + ...)
C...                 4X   4F       5X   5F       6X   6F
C...
C...                                       2             3
C...  C(UN-2 = UN + UN (-2DX)  + UN  (-2DX)  + UN  (-2DX)
C...                  X   1F       2X   2F       3X   3F
C...
C...                         4             5             6
C...             + UN  (-2DX)  + UN  (-2DX)  + UN  (-2DX)  + ...)
C...                 4X   4F       5X   5F       6X   6F
C...
C...                                       2             3
C...  D(UN-1 = UN + UN ( -DX)  + UN  ( -DX)  + UN  ( -DX)
C...                  X   1F       2X   2F       3X   3F
C...
C...                         4             5             6
C...             + UN  ( -DX)  + UN  ( -DX)  + UN  ( -DX)  + ...)
C...                 4X   4F       5X   5F       6X   6F
C...
C...   -4A -  3B -  2C -   D = 1
C...
C...   16A +  9B +  4C +   D = 0
C...
C...  -64A - 27B -  8C -   D = 0
C...
C...  256A + 81B + 16C +   D = 0
C...
C...  SIMULTANEOUS SOLUTION FOR A, B, C AND D FOLLOWED BY THE SOLU-
C...  TION OF THE PRECEDING TAYLOR SERIES, TRUNCATED AFTER THE U
C...                                                            4X
C...  TERMS, FOR U1  GIVES THE FOLLOWING FIVE-POINT APPROXIMATION
C...               X
C...                                                                4
C...  UN  = (1/12DX)(3UN-4 - 16UN-3 + 36UN-2 - 48UN-1 + 25UN) + O(DX )
C...    X
C...                                                              (5)
C...
C...  THE WEIGHTING COEFFICIENTS FOR EQUATIONS (1) TO (5) CAN BE
C...  SUMMARIZED AS
C...
C...             -25   48  -36   16   -3
C...
C...              -3  -10   18   -6    1
C...
C...       1/12    1   -8    0    8   -1
C...
C...              -1    6  -18   10    3
C...
C...               3  -16   36  -48   25
C...
C...  WHICH ARE THE COEFFICIENTS REPORTED BY BICKLEY FOR N = 4, M =
C...  1, P = 0, 1, 2, 3, 4 (BICKLEY, W. G., FORMULAE FOR NUMERICAL
C...  DIFFERENTIATION, MATH. GAZ., VOL. 25, 1941.  NOTE - THE BICKLEY
C...  COEFFICIENTS HAVE BEEN DIVIDED BY A COMMON FACTOR OF TWO).
C...
C...  EQUATIONS (1) TO (5) CAN NOW BE PROGRAMMED TO GENERATE THE
C...  DERIVATIVE U (X) OF FUNCTION U(X) (ARGUMENTS U AND UX OF SUB-
C...              X
C...  ROUTINE DSS004 RESPECTIVELY).
C...
C...  TYPE SELECTED REAL VARIABLES AS DOUBLE PRECISION
#endif
      DOUBLE PRECISION     DX,R4FDX,U,UX,XL,XU
     !1                 DFLOAT

      DIMENSION U(N),UX(N)

!C...  COMPUTE THE SPATIAL INCREMENT
      DX=(XU-XL)/DFLOAT(N-1)
      R4FDX=1.D+00/(12.D+00*DX)
      NM2=N-2
!C...
!C...  EQUATION (1) (NOTE - THE RHS OF EQUATIONS (1), (2), (3), (4)
!C...  AND (5) HAVE BEEN FORMATTED SO THAT THE NUMERICAL WEIGHTING
!C...  COEFFICIENTS CAN BE MORE EASILY ASSOCIATED WITH THE BICKLEY
!C...  MATRIX ABOVE)
      UX(1)=R4FDX*(-25.D+00*U(1)+48.D+00*U(2)-36.D+00*U(3)+  &   
                    16.D+00*U(4)-3.D+00*U(5))
  
       
!C...
!C...  EQUATION (2)
      UX(  2)=R4FDX*            &
        ( -3.D+00      *U(  1)  &
          -10.D+00     *U(  2)  &
          +18.D+00     *U(  3)  &
          -6.D+00      *U(  4)  &
          +1.D+00      *U(  5))
!C...
!C...  EQUATION (3)
      !dir$ assume_aligned U:64,UX:64
      !dir$ vector aligned
      !dir$ ivdep
      !dir$ vector always
      DO  I=3,NM2
      UX(  I)=R4FDX*          &
      ( +1.D+00     *U(I-2)   &
       -8.D+00      *U(I-1)   &
       +0.D+00      *U(  I)   &
       +8.D+00      *U(I+1)   &
       -1.D+00      *U(I+2))
      END DO
!C...
!C...  EQUATION (4)
      UX(N-1)=R4FDX*          &
      ( -1.D+00      *U(N-4)  &
       +6.D+00      *U(N-3)   &
      -18.D+00      *U(N-2)   &
      +10.D+00      *U(N-1)   &
       +3.D+00      *U(N  ))
C...
C...  EQUATION (5)
      UX(  N)=R4FDX*
      ( +3.D+00      *U(N-4)  &
      -16.D+00      *U(N-3)   &
      +36.D+00      *U(N-2)   &
      -48.D+00      *U(N-1)   &
      +25.D+00      *U(N  ))
     
    END SUBROUTINE


    
    SUBROUTINE DSS012(XL,XU,N,U,UX,V)
          !dir$ optimize:3
          !dir$ attributes forceinline :: DSS012
          !dir$ attributes optimization_parameter: "target_arch=skylake-avx512" :: DSS012
#if 0
C...
C...  SUBROUTINE DSS012 IS AN APPLICATION OF FIRST-ORDER DIRECTIONAL
C...  DIFFERENCING IN THE NUMERICAL METHOD OF LINES.  IT IS INTENDED
C...  SPECIFICALLY FOR THE ANALYSIS OF CONVECTIVE SYSTEMS MODELLED BY
C...  FIRST-ORDER HYPERBOLIC PARTIAL DIFFERENTIAL EQUATIONS WITH THE
C...  SIMPLEST FORM
C...
C...                            U  + V*U  = 0                        (1)
C...                             T      X
C...
C...  THE FIRST FIVE PARAMETERS, XL, XU, N, U AND UX, ARE THE SAME
C...  AS FOR SUBROUTINES DSS002 TO DSS010 AS DEFINED IN THOSE ROUTINES.
C...  THE SIXTH PARAMETER, V, MUST BE PROVIDED TO DSS012 SO THAT THE
C...  DIRECTION OF FLOW IN EQUATION (1) CAN BE USED TO SELECT THE
C...  APPROPRIATE FINITE DIFFERENCE APPROXIMATION FOR THE FIRST-ORDER
C...  SPATIAL DERIVATIVE IN EQUATION (1), U .  THE CONVENTION FOR THE
C...  SIGN OF V IS                         X
C...
C...     FLOW LEFT TO RIGHT                 V GT 0
C...     (I.E., IN THE DIRECTION            (I.E., THE SIXTH ARGUMENT IS
C...     OF INCREASING X)                   POSITIVE IN CALLING DSS012)
C...
C...     FLOW RIGHT TO LEFT                 V LT 0
C...     (I.E., IN THE DIRECTION            (I.E., THE SIXTH ARGUMENT IS
C...     OF DECREASING X)                   NEGATIVE IN CALLING DSS012)
C...
C...  TYPE SELECTED REAL VARIABLES AS DOUBLE PRECISION
#endif
      DOUBLE PRECISION     DX,      U,     UX,      V,     XL,     XU
     !1                 DFLOAT
!C...
      DIMENSION U(N),UX(N)
!C!...
!C...  COMPUTE THE SPATIAL INCREMENT, THEN SELECT THE FINITE DIFFERENCE
!C...  APPROXIMATION DEPENDING ON THE SIGN OF V IN EQUATION (1).  THE
!C...  ORIGIN OF THE FINITE DIFFERENCE APPROXIMATIONS USED BELOW IS GIVEN
!C...  AT THE END OF SUBROUTINE DSS012.
      DX=(XU-XL)/DFLOAT(N-1)
      IF(V.LT.0.D+00)GO TO 10
!C...
!C...     (1)  FINITE DIFFERENCE APPROXIMATION FOR POSITIVE V
              UX(1)=(U(2)-U(1))/DX
              !dir$ assume_aligned UX:64,U:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector always
              DO 1 I=2,N
                   UX(I)=(U(I)-U(I-1))/DX
1             CONTINUE
              RETURN
!C...
!C...     (2)  FINITE DIFFERENCE APPROXIMATION FOR NEGATIVE V
10            NM1=N-1
              !dir$ assume_aligned UX:64,U:64
              !dir$ vector aligned
              !dir$ ivdep
              !dir$ vector always
              DO 2 I=1,NM1
                   UX(I)=(U(I+1)-U(I))/DX
2             CONTINUE
              UX(N)=(U(N)-U(N-1))/DX
              RETURN
#if 0
C...
C...  THE BACKWARD DIFFERENCES IN SECTION (1) ABOVE ARE BASED ON THE
C...  TAYLOR SERIES
C...
C...                                  2           3
C...  UI-1 = UI + UI (-DX) + UI  (-DX) + UI  (-DX) + ...
C...                X  1F      2X  2F      3X  3F
C...
C...                                          2
C...  IF THIS SERIES IS TRUNCATED AFTER THE DX  TERM AND THE RESULTING
C...  EQUATION SOLVED FOR U ,  WE OBTAIN IMMEDIATELY
C...                       X
C...
C...  UI  = (UI - UI-1)/DX + O(DX)
C...    X
C...
C...  WHICH IS THE FIRST-ORDER BACKWARD DIFFERENCE USED IN DO LOOP 1.
C...  THE DERIVATIVE U1  IS COMPUTED BY USING THE POINT TO THE RIGHT OF
C...                   X
C...  U1, I.E., U2, SINCE THIS IS THE ONLY POINT AVAILABLE IF FICTITIOUS
C...  POINTS TO THE LEFT OF U1 ARE TO BE AVOIDED.
C...
C...  THE FORWARD DIFFERENCES IN SECTION (2) ABOVE ARE BASED ON THE
C...  TAYLOR SERIES
C...
C...                                  2           3
C...  UI+1 = UI + UI ( DX) + UI  ( DX) + UI  ( DX) + ...
C...                X  1F      2X  2F      3X  3F
C...
C...                                          2
C...  IF THIS SERIES IS TRUNCATED AFTER THE DX  TERM AND THE RESULTING
C...  EQUATION SOLVED FOR U ,  WE OBTAIN IMMEDIATELY
C...                       X
C...
C...  UI  = (UI+1 - UI)/DX + O(DX)
C...    X
C...
C...  WHICH IS THE FIRST-ORDER FORWARD DIFFERENCE USED IN DO LOOP 2.
C...  THE DERIVATIVE UN  IS COMPUTED BY USING THE POINT TO THE LEFT OF
C...                   X
C...  UN (UN-1), SINCE THIS IS THE ONLY POINT AVAILABLE IF FICTITIOUS
C...  POINTS TO THE RIGHT OF UN ARE TO BE AVOIDED.
#endif

   END SUBROUTINE


   
   SUBROUTINE DSS018(XL,XU,N,U,UX,V)
          !dir$ optimize:3
          !dir$ attributes forceinline :: DSS018
          !dir$ attributes optimization_parameter: "target_arch=skylake-avx512" :: DSS018
#if 0
C...
C...  SUBROUTINE DSS018 IS AN APPLICATION OF THIRD-ORDER DIRECTIONAL
C...  DIFFERENCING IN THE NUMERICAL METHOD OF LINES.  IT IS INTENDED
C...  SPECIFICALLY FOR THE ANALYSIS OF CONVECTIVE SYSTEMS MODELLED BY
C...  FIRST-ORDER HYPERBOLIC PARTIAL DIFFERENTIAL EQUATIONS AS DIS-
C...  CUSSED IN SUBROUTINE DSS012.  THE COEFFICIENTS OF THE FINITE
C...  DIFFERENCE APPROXIMATIONS USED HEREIN ARE TAKEN FROM BICKLEY, W.
C...  G., FORMULAE FOR NUMERICAL DIFFERENTIATION, THE MATHEMATICAL
C...  GAZETTE, PP. 19-27, 1941, N = 3, M = 1, P = 0, 1, 2, 3.  THE
C...  IMPLEMENTATION IS THE **FOUR-POINT BIASED UPWIND FORMULA** OF
C...  M. B. CARVER AND H. W. HINDS, THE METHOD OF LINES AND THE
C...  ADVECTION EQUATION, SIMULATION, VOL. 31, NO. 2, PP. 59-69,
C...  AUGUST, 1978
C...
#endif
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(N),UX(N)
!C...
!C...  COMPUTE THE COMMON FACTOR FOR EACH FINITE DIFFERENCE APPROXIMATION
!C...  CONTAINING THE SPATIAL INCREMENT, THEN SELECT THE FINITE DIFFER-
!C...  ENCE APPROXIMATION DEPENDING ON THE SIGN OF V (SIXTH ARGUMENT).
      DX=(XU-XL)/DFLOAT(N-1)
      R3FDX=1.D+00/(6.D+00*DX)
      IF(V.LT.0.D+00)GO TO 10
!C...
!C...     (1)  FINITE DIFFERENCE APPROXIMATION FOR POSITIVE V
      UX(  1)=R3FDX*           &
      ( -11.D+00     *U(  1)   &
       +18.D+00     *U(  2)   &
        -9.D+00     *U(  3)   &
        +2.D+00     *U(  4))
      UX(  2)=R3FDX*
     (  -2.D+00     *U(  1)   &
        -3.D+00     *U(  2)   &
        +6.D+00     *U(  3)   &
        -1.D+00     *U(  4))
      NM1=N-1
      !dir$ asssume_aligned UX:64,U:64
      !dir$ vector aligned
      !dir$ ivdep
      !dir$ vector vectorlength(8)
      !dir$ vector always
      DO 1 I=3,NM1
      UX(  I)=R3FDX*           &
      (  +1.D+00     *U(I-2)   &
         -6.D+00     *U(I-1)   &
         +3.D+00     *U(I  )   &
         +2.D+00     *U(I+1))
1     CONTINUE
      UX(  N)=R3FDX*           &
      (  -2.D+00      *U(N-3)  &
         +9.D+00      *U(N-2)  &
         -18.D+00     *U(N-1)  &
         +11.D+00     *U(N  ))
      RETURN
!C...
!C...     (2)  FINITE DIFFERENCE APPROXIMATION FOR NEGATIVE V
10    UX(  1)=R3FDX*           &
     ( -11.D+00     *U(  1)    &
       +18.D+00     *U(  2)    &
        -9.D+00     *U(  3)    &
        +2.D+00     *U(  4))
      NM2=N-2
      !dir$ asssume_aligned UX:64,U:64
      !dir$ vector aligned
      !dir$ ivdep
      !dir$ vector vectorlength(8)
      !dir$ vector always
      DO 2 I=2,NM2
      UX(  I)=R3FDX*         &
     (  -2.D+00     *U(I-1)  &
        -3.D+00     *U(I  )  &
        +6.D+00     *U(I+1)  &
        -1.D+00     *U(I+2))
2     CONTINUE
      UX(N-1)=R3FDX*         &
     (  +1.D+00     *U(N-3)  &
        -6.D+00     *U(N-2)  &
        +3.D+00     *U(N-1)  &
        +2.D+00     *U(N  ))
      UX(  N)=R3FDX*         &
     (  -2.D+00     *U(N-3)  &
        +9.D+00     *U(N-2)  &
       -18.D+00     *U(N-1)  &
       +11.D+00     *U(N  ))
    
    END SUBROUTINE


   
     SUBROUTINE DSS020(XL,XU,N,U,UX,V)
          !dir$ optimize:3
          !dir$ attributes forceinline :: DSS020
          !dir$ attributes optimization_parameter: "target_arch=skylake-avx512" :: DSS020
#if 0
C...
C...  SUBROUTINE DSS020 IS AN APPLICATION OF FOURTH-ORDER DIRECTIONAL
C...  DIFFERENCING IN THE NUMERICAL METHOD OF LINES.  IT IS INTENDED
C...  SPECIFICALLY FOR THE ANALYSIS OF CONVECTIVE SYSTEMS MODELLED BY
C...  FIRST-ORDER HYPERBOLIC PARTIAL DIFFERENTIAL EQUATIONS AS DIS-
C...  CUSSED IN SUBROUTINE DSS012.  THE COEFFICIENTS OF THE FINITE
C...  DIFFERENCE APPROXIMATIONS USED HEREIN ARE TAKEN FROM BICKLEY, W.
C...  G., FORMULAE FOR NUMERICAL DIFFERENTIATION, THE MATHEMATICAL
C...  GAZETTE, PP. 19-27, 1941, N = 4, M = 1, P = 0, 1, 2, 3, 4.  THE
C...  IMPLEMENTATION IS THE **FIVE-POINT BIASED UPWIND FORMULA** OF
C...  M. B. CARVER AND H. W. HINDS, THE METHOD OF LINES AND THE
C...  ADVECTION EQUATION, SIMULATION, VOL. 31, NO. 2, PP. 59-69,
C...  AUGUST, 1978
C...
C...  TYPE SELECTED REAL VARIABLES AS DOUBLE PRECISION
#endif
      DOUBLE PRECISION      DX,  R4FDX,      U,     UX,      V,     XL
     !1                      XU, DFLOAT
!C...
      DIMENSION U(N),UX(N)
!C...
!C...  COMPUTE THE COMMON FACTOR FOR EACH FINITE DIFFERENCE APPROXIMATION
!C...  CONTAINING THE SPATIAL INCREMENT, THEN SELECT THE FINITE DIFFER-
!C...  ENCE APPROXIMATION DEPENDING ON THE SIGN OF V (SIXTH ARGUMENT).
      DX=(XU-XL)/DFLOAT(N-1)
      R4FDX=1.D+00/(12.D+00*DX)
      IF(V.LT.0.D+00)GO TO 10
!C...
!C...     (1)  FINITE DIFFERENCE APPROXIMATION FOR POSITIVE V
      UX(  1)=R4FDX*         &
      ( -25.D+00    *U(  1)  &
       +48.D+00     *U(  2)  &
       -36.D+00     *U(  3)  &
       +16.D+00     *U(  4)  &
        -3.D+00     *U(  5))
      UX(  2)=R4FDX*         &
      (  -3.D+00    *U(  1)  &
       -10.D+00     *U(  2)  &
       +18.D+00     *U(  3)  &
        -6.D+00     *U(  4)  &
        +1.D+00     *U(  5))
      UX(  3)=R4FDX*         &
      (  +1.D+00    *U(  1)  &
        -8.D+00     *U(  2)  &
        +0.D+00     *U(  3)  &
        +8.D+00     *U(  4)  &
        -1.D+00     *U(  5))
      NM1=N-1
      !dir$ asssume_aligned UX:64,U:64
      !dir$ vector aligned
      !dir$ ivdep
      !dir$ vector vectorlength(8)
      !dir$ vector always
      DO 1 I=4,NM1
      UX(  I)=R4FDX*          &
      (  -1.D+00     *U(I-3)  &
         +6.D+00     *U(I-2)  &
        -18.D+00     *U(I-1)  &
        +10.D+00     *U(I  )  &
         +3.D+00     *U(I+1))
1     CONTINUE
      UX(  N)=R4FDX*          &
     (  +3.D+00     *U(N-4)   &
       -16.D+00     *U(N-3)   &
       +36.D+00     *U(N-2)   &
       -48.D+00     *U(N-1)   &
       +25.D+00     *U(N  ))
      RETURN
C...
C...     (2)  FINITE DIFFERENCE APPROXIMATION FOR NEGATIVE V
10    UX(  1)=R4FDX*          &
     ( -25.D+00     *U(  1)   &
       +48.D+00     *U(  2)   &
       -36.D+00     *U(  3)   &
       +16.D+00     *U(  4)   &
        -3.D+00     *U(  5))
      NM3=N-3
      !dir$ asssume_aligned UX:64,U:64
      !dir$ vector aligned
      !dir$ ivdep
      !dir$ vector vectorlength(8)
      !dir$ vector always
      DO 2 I=2,NM3
      UX(  I)=R4FDX*         &
     (  -3.D+00     *U(I-1)  &
       -10.D+00     *U(I  )  &
       +18.D+00     *U(I+1)  &
        -6.D+00     *U(I+2)  &
        +1.D+00     *U(I+3))
2     CONTINUE
      UX(N-2)=R4FDX*         &
     (  +1.D+00     *U(N-4)  &
        -8.D+00     *U(N-3)  &
        +0.D+00     *U(N-2)  &
        +8.D+00     *U(N-1)  &
        -1.D+00     *U(N  ))
      UX(N-1)=R4FDX*         &
     (  -1.D+00     *U(N-4)  &
        +6.D+00     *U(N-3)  &
       -18.D+00     *U(N-2)  &
       +10.D+00     *U(N-1)  &
        +3.D+00     *U(N  ))
      UX(  N)=R4FDX*         &
     (  +3.D+00     *U(N-4)  &
       -16.D+00     *U(N-3)  &
       +36.D+00     *U(N-2)  &
       -48.D+00     *U(N-1)  &
       +25.D+00     *U(N  ))
     
    END SUBROUTINE


    
    SUBROUTINE DSS034(XL,XU,N1,N2,ND,U2D,UX2D,V)
          !dir$ optimize:3
          !dir$ attributes forceinline :: DSS034
          !dir$ attributes optimization_parameter: "target_arch=skylake-avx512" :: DSS034
#if 0
C...
C...  SUBROUTINE DSS034 COMPUTES A PARTIAL DERIVATIVE OVER A TWO-
C...  DIMENSIONAL DOMAIN USING EITHER FIVE-POINT CENTERED OR FIVE-
C...  POINT BIASED UPWIND APPROXIMATIONS.  IT IS INTENDED PRIMARILY
C...  FOR THE NUMERICAL METHOD OF LINES (NMOL) NUMERICAL INTEGRATION
C...  OF PARTIAL DIFFERENTIAL EQUATIONS (PDES) IN TWO DIMENSIONS.
C...  DSS034 IS RECOMMENDED PARTICULARLY FOR CONVECTIVE-DIFFUSION
C...  PROBLEMS WHICH REQUIRE COMBINATIONS OF BIASED UPWIND AND CENTERED
C...  APPROXIMATIONS FOR FIRST AND SECOND-ORDER SPATIAL DERIVATIVES IN
C...  PDES.
C...
C...  ARGUMENT LIST
C...
C...     XL        LOWER VALUE OF THE INDEPENDENT VARIABLE FOR WHICH
C...               THE PARTIAL DERIVATIVE IS TO BE COMPUTED (INPUT)
C...
C...     XU        UPPER VALUE OF THE INDEPENDENT VARIABLE FOR WHICH
C...               THE PARTIAL DERIVATIVE IS TO BE COMPUTED (INPUT)
C...
C...     N1        NUMBER OF GRID POINTS FOR THE FIRST INDEPENDENT
C...               VARIABLE (INPUT)
C...
C...     N2        NUMBER OF GRID POINTS FOR THE SECOND INDEPENDENT
C...               VARIABLE (INPUT)
C...
C...     ND        NUMBER OF THE INDEPENDENT VARIABLE FOR WHICH THE
C...               PARTIAL DERIVATIVE IS TO BE COMPUTED (INPUT)
C...
C...     U2D       TWO-DIMENSIONAL ARRAY CONTAINING THE DEPENDENT VARI-
C...               ABLE WHICH IS TO BE DIFFERENTIATED WITH RESPECT TO
C...               INDEPENDENT VARIABLE ND (INPUT)
C...
C...     UX2D      TWO-DIMENSIONAL ARRAY CONTAINING THE PARTIAL DERI-
C...               VATIVE OF THE DEPENDENT VARIABLE WITH RESPECT TO
C...               INDEPENDENT VARIABLE ND (OUTPUT)
C...
C...     V         VARIABLE TO SELECT EITHER THE FIVE-POINT CENTERED
C...               OR FIVE-POINT BIASED UPWIND APPROXIMATION FOR THE
C...               PARTIAL DERIVATIVE.  V EQ 0 CALLS THE FIVE-POINT
C...               CENTERED APPROXIMATION.  V NE 0 CALLS THE FIVE-POINT
C...               BIASED UPWIND APPROXIMATION (INPUT)
C...
C...  TYPE SELECTED REAL VARIABLES AS DOUBLE PRECISION
#endif
      DOUBLE PRECISION   UX1D,   UX2D,    U1D,    U2D,      V,     XL
     !1                     XU
!C...
!C...  THE FOLLOWING TWO-DIMENSIONAL ARRAYS CONTAIN THE DEPENDENT
!C...  VARIABLE (U2D) AND ITS PARTIAL DERIVATIVE (UX2D)
      DIMENSION   U2D(N1,N2), UX2D(N1,N2)
#if 0
C...
C...  THE FOLLOWING ONE-DIMENSIONAL ARRAYS CONTAIN THE DEPENDENT
C...  VARIABLE (U1D) AND ITS PARTIAL DERIVATIVE (UX1D).  IN EACH
C...  CASE, ONE OF THE INDEPENDENT VARIABLES IS CONSTANT AND THE
C...  OTHER INDEPENDENT VARIABLE VARIES OVER ITS TOTAL INTERVAL.
C...  THESE ARRAYS ARE USED FOR TEMPORARY STORAGE IN CALLING THE
C...  ONE-DIMENSIONAL ROUTINES DSS004 AND DSS020.
C...
C...  NOTE THAT THE ARRAYS HAVE ABSOLUTE DIMENSIONS AND MAY THERE-
C...  FORE HAVE TO BE INCREASED IN SIZE.  HOWEVER, WITH A SIZE
C...  OF 51, THE TWO-DIMENSIONAL PROBLEM COULD HAVE A GRID OF
C...  51 X 51 POINTS, THEREBY GENERATING AN APPROXIMATING ODE
C...  SYSTEM WITH A MULTIPLE OF 51 X 51 EQUATIONS, DEPENDING ON
C...  THE NUMBER OF SIMULTANEOUS PDES.  THIS IS A VERY LARGE ODE
C...  PROBLEM, AND THEREFORE THE FOLLOWING ABSOLUTE DIMENSIONING
C...  IS CONSIDERED ADEQUATE FOR MOST PROBLEMS.
#endif
      ! Changed to 1024x1024
      !DIMENSION      U1D(51),    UX1D(51)
      DOUBLE PRECISION, DIMENSION(1024) :: U1D
      DOUBLE PRECISION, DIMENSION(1024) :: UX1D
      !dir$ attributes align : 64 :: U1D
      !dir$ attributes align : 64 :: UX1D
!C...
!C...  GO TO STATEMENT 2 IF THE PARTIAL DERIVATIVE IS TO BE COMPUTED
!C...  WITH RESPECT TO THE SECOND INDEPENDENT VARIABLE
      IF(ND.EQ.2)GO TO 2
!C...
!C...  ******************************************************************
!C...
!C...  THE PARTIAL DERIVATIVE IS TO BE COMPUTED WITH RESPECT TO THE
!C...  FIRST INDEPENDENT VARIABLE DEFINED OVER AN INTERVAL CONSISTING
!C...  OF N1 GRID POINTS.  COMPUTE THE PARTIAL DERIVATIVE AT THE N1 X
!C...  N2 GRID POINTS VIA NESTED DO LOOPS 10, 11 AND 12
      DO 10 J=1,N2
!C...
!C...  TRANSFER THE DEPENDENT VARIABLE IN THE TWO-DIMENSIONAL ARRAY U2D
!C...  TO THE ONE-DIMENSIONAL ARRAY U1D SO THAT SUBROUTINES DSS004 AND
!C...  DSS020 CAN BE USED TO CALCULATE THE PARTIAL DERIVATIVE
      !dir$ assume_aligned U1D:64,U2D:64
      !dir$ vector aligned
      !dir$ unroll(16)
      !dir$ vector always
      DO 11 I=1,N1
            U1D(I)=U2D(I,J)
11    CONTINUE
!C...
!C...  IF V EQ 0, A FIVE-POINT CENTERED APPROXIMATION IS USED FOR THE
!C...  PARTIAL DERIVATIVE
      IF(V.EQ.0.D+00)CALL DSS004(XL,XU,N1,U1D,UX1D)
!C...
!C...  IF V NE 0, A FIVE-POINT BIASED UPWIND APPROXIMATION IS USED FOR
!C...  THE PARTIAL DERIVATIVE
      IF(V.NE.0.D+00)CALL DSS020(XL,XU,N1,U1D,UX1D,V)
!C...
!C...  RETURN THE PARTIAL DERIVATIVE IN THE ONE-DIMENSIONAL ARRAY UX1D
!C...  TO THE TWO-DIMENSIONAL ARRAY UX2D
      !dir$ assume_aligned UX2D:64,UX1D:64
      !dir$ vector aligned
      !dir$ unroll(16)
      !dir$ vector always
      DO 12 I=1,N1
            UX2D(I,J)=UX1D(I)
12    CONTINUE
!C...
!C...  THE PARTIAL DERIVATIVE AT A PARTICULAR VALUE OF THE SECOND INDE-
!C...  PENDENT VARIABLE HAS BEEN CALCULATED.  REPEAT THE CALCULATION FOR
!C...  THE NEXT VALUE OF THE SECOND INDEPENDENT VARIABLE
10    CONTINUE
!C...
!C...  THE PARTIAL DERIVATIVE HAS BEEN CALCULATED OVER THE ENTIRE N1 X
!C...  N2 GRID.  THEREFORE RETURN TO THE CALLING PROGRAM WITH THE PARTIAL
!C...  DERIVATIVE IN THE TWO-DIMENSIONAL ARRAY UX2D
      RETURN

!C...  THE PARTIAL DERIVATIVE IS TO BE COMPUTED WITH RESPECT TO THE
!C...  SECOND INDEPENDENT VARIABLE DEFINED OVER AN INTERVAL CONSISTING
!C...  OF N2 GRID POINTS.  COMPUTE THE PARTIAL DERIVATIVE AT THE N1 X
!C...  N2 GRID POINTS VIA NESTED DO LOOPS 20 AND 21
2     DO 20 I=1,N1
!C...
!C...  TRANSFER THE DEPENDENT VARIABLE IN THE TWO-DIMENSIONAL ARRAY U2D
!C...  TO THE ONE-DIMENSIONAL ARRAY U1D SO THAT SUBROUTINES DSS004 AND
!C...  DSS020 CAN BE USED TO CALCULATE THE PARTIAL DERIVATIVE
      !dir$ assume_aligned U1D:64,U2D:64
      !dir$ vector aligned
      !dir$ unroll(16)
      !dir$ vector always
      DO 21 J=1,N2
            U1D(J)=U2D(I,J)
21    CONTINUE
!C...
!C...  IF V EQ 0, A FIVE-POINT CENTERED APPROXIMATION IS USED FOR THE
!C!...  PARTIAL DERIVATIVE
      IF(V.EQ.0.D+00)CALL DSS004(XL,XU,N2,U1D,UX1D)
!C...
!C...  IF V NE 0, A FIVE-POINT BIASED UPWIND APPROXIMATION IS USED FOR
!C...  THE PARTIAL DERIVATIVE
      IF(V.NE.0.D+00)CALL DSS020(XL,XU,N2,U1D,UX1D,V)
!C...
!C...  RETURN THE PARTIAL DERIVATIVE IN THE ONE-DIMENSIONAL ARRAY UX1D
!C...  TO THE TWO-DIMENSIONAL ARRAY UX2D
      !dir$ assume_aligned UX2D:64,UX1D:64
      !dir$ vector aligned
      !dir$ unroll(16)
      !dir$ vector always
      DO 22 J=1,N2
             UX2D(I,J)=UX1D(J)
22    CONTINUE
!C...
!C...  THE PARTIAL DERIVATIVE AT A PARTICULAR VALUE OF THE FIRST INDE-
!C...  PENDENT VARIABLE HAS BEEN CALCULATED.  REPEAT THE CALCULATION FOR
!C...  THE NEXT VALUE OF THE FIRST INDEPENDENT VARIABLE
20    CONTINUE

     
   END SUBROUTINE


!=======================================================================!

   SUBROUTINE DDRIV2 (N,T,Y,F,TOUT,MSTATE,NROOT,EPS,EWT,MINT,WORK, &
        LENW,IWORK,LENIW,G)
#if 0
C***BEGIN PROLOGUE  DDRIV2
C***DATE WRITTEN   790601   (YYMMDD)
C***REVISION DATE  871105   (YYMMDD)
C***CATEGORY NO.  I1A2,I1A1B
C***KEYWORDS  ODE,STIFF,ORDINARY DIFFERENTIAL EQUATIONS,
C             INITIAL VALUE PROBLEMS,GEAR'S METHOD,
C             DOUBLE PRECISION
C***AUTHOR  KAHANER, D. K., NATIONAL BUREAU OF STANDARDS,
C           SUTHERLAND, C. D., LOS ALAMOS NATIONAL LABORATORY
C***PURPOSE  The function of DDRIV2 is to solve N ordinary differential
C            equations of the form dY(I)/dT = F(Y(I),T), given the
C            initial conditions Y(I) = YI.  The program has options to
C            allow the solution of both stiff and non-stiff differential
C            equations.  DDRIV2 uses double precision arithmetic.
C***DESCRIPTION
C    From the book "Numerical Methods and Software"
C       by D. Kahaner, C. Moler, S. Nash
C          Prentice Hall 1988
C  I.  ABSTRACT  .......................................................
C
C    The function of DDRIV2 is to solve N ordinary differential
C    equations of the form dY(I)/dT = F(Y(I),T), given the initial
C    conditions Y(I) = YI.  The program has options to allow the
C    solution of both stiff and non-stiff differential equations.
C    DDRIV2 is to be called once for each output point of T.
C
C  II.  PARAMETERS  ....................................................
C
C       (REMEMBER--To run DDRIV2 correctly in double precision, ALL
C       non-integer arguments in the call sequence, including
C       arrays, MUST be declared double precision.)
C
C    The user should use parameter names in the call sequence of DDRIV2
C    for those quantities whose value may be altered by DDRIV2.  The
C    parameters in the call sequence are:
C
C    N      = (Input) The number of differential equations.
C
C    T      = The independent variable.  On input for the first call, T
C             is the initial point.  On output, T is the point at which
C             the solution is given.
C
C    Y      = The vector of dependent variables.  Y is used as input on
C             the first call, to set the initial values.  On output, Y
C             is the computed solution vector.  This array Y is passed
C             in the call sequence of the user-provided routines F and
C             G.  Thus parameters required by F and G can be stored in
C             this array in components N+1 and above.  (Note: Changes
C             by the user to the first N components of this array will
C             take effect only after a restart, i.e., after setting
C             MSTATE to +1(-1).)
C
C    F      = A subroutine supplied by the user.  The name must be
C             declared EXTERNAL in the user's calling program.  This
C             subroutine is of the form:
C                   SUBROUTINE F (N, T, Y, YDOT)
C                   DOUBLE PRECISION Y(*), YDOT(*)
C                     .
C                     .
C                   YDOT(1) = ...
C                     .
C                     .
C                   YDOT(N) = ...
C                   END (Sample)
C             This computes YDOT = F(Y,T), the right hand side of the
C             differential equations.  Here Y is a vector of length at
C             least N.  The actual length of Y is determined by the
C             user's declaration in the program which calls DDRIV2.
C             Thus the dimensioning of Y in F, while required by FORTRAN
C             convention, does not actually allocate any storage.  When
C             this subroutine is called, the first N components of Y are
C             intermediate approximations to the solution components.
C             The user should not alter these values.  Here YDOT is a
C             vector of length N.  The user should only compute YDOT(I)
C             for I from 1 to N.  Normally a return from F passes
C             control back to  DDRIV2.  However, if the user would like
C             to abort the calculation, i.e., return control to the
C             program which calls DDRIV2, he should set N to zero.
C             DDRIV2 will signal this by returning a value of MSTATE
C             equal to +6(-6).  Altering the value of N in F has no
C             effect on the value of N in the call sequence of DDRIV2.
C
C    TOUT   = (Input) The point at which the solution is desired.
C
C    MSTATE = An integer describing the status of integration.  The user
C             must initialize MSTATE to +1 or -1.  If MSTATE is
C             positive, the routine will integrate past TOUT and
C             interpolate the solution.  This is the most efficient
C             mode.  If MSTATE is negative, the routine will adjust its
C             internal step to reach TOUT exactly (useful if a
C             singularity exists beyond TOUT.)  The meaning of the
C             magnitude of MSTATE:
C               1  (Input) Means the first call to the routine.  This
C                  value must be set by the user.  On all subsequent
C                  calls the value of MSTATE should be tested by the
C                  user.  Unless DDRIV2 is to be reinitialized, only the
C                  sign of MSTATE may be changed by the user.  (As a
C                  convenience to the user who may wish to put out the
C                  initial conditions, DDRIV2 can be called with
C                  MSTATE=+1(-1), and TOUT=T.  In this case the program
C                  will return with MSTATE unchanged, i.e.,
C                  MSTATE=+1(-1).)
C               2  (Output) Means a successful integration.  If a normal
C                  continuation is desired (i.e., a further integration
C                  in the same direction), simply advance TOUT and call
C                  again.  All other parameters are automatically set.
C               3  (Output)(Unsuccessful) Means the integrator has taken
C                  1000 steps without reaching TOUT.  The user can
C                  continue the integration by simply calling DDRIV2
C                  again.  Other than an error in problem setup, the
C                  most likely cause for this condition is trying to
C                  integrate a stiff set of equations with the non-stiff
C                  integrator option. (See description of MINT below.)
C               4  (Output)(Unsuccessful) Means too much accuracy has
C                  been requested.  EPS has been increased to a value
C                  the program estimates is appropriate.  The user can
C                  continue the integration by simply calling DDRIV2
C                  again.
C               5  (Output) A root was found at a point less than TOUT.
C                  The user can continue the integration toward TOUT by
C                  simply calling DDRIV2 again.
C               6  (Output)(Unsuccessful) N has been set to zero in
C                  SUBROUTINE F.
C               7  (Output)(Unsuccessful) N has been set to zero in
C                  FUNCTION G.  See description of G below.
C
C    NROOT  = (Input) The number of equations whose roots are desired.
C             If NROOT is zero, the root search is not active.  This
C             option is useful for obtaining output at points which are
C             not known in advance, but depend upon the solution, e.g.,
C             when some solution component takes on a specified value.
C             The root search is carried out using the user-written
C             function G (see description of G below.)  DDRIV2 attempts
C             to find the value of T at which one of the equations
C             changes sign.  DDRIV2 can find at most one root per
C             equation per internal integration step, and will then
C             return the solution either at TOUT or at a root, whichever
C             occurs first in the direction of integration.  The index
C             of the equation whose root is being reported is stored in
C             the sixth element of IWORK.
C             NOTE: NROOT is never altered by this program.
C
C    EPS    = On input, the requested relative accuracy in all solution
C             components.  EPS = 0 is allowed.  On output, the adjusted
C             relative accuracy if the input value was too small.  The
C             value of EPS should be set as large as is reasonable,
C             because the amount of work done by DDRIV2 increases as
C             EPS decreases.
C
C    EWT    = (Input) Problem zero, i.e., the smallest physically
C             meaningful value for the solution.  This is used inter-
C             nally to compute an array YWT(I) = MAX(ABS(Y(I)), EWT).
C             One step error estimates divided by YWT(I) are kept less
C             than EPS.  Setting EWT to zero provides pure relative
C             error control.  However, setting EWT smaller than
C             necessary can adversely affect the running time.
C
C    MINT   = (Input) The integration method flag.
C               MINT = 1  Means the Adams methods, and is used for
C                         non-stiff problems.
C               MINT = 2  Means the stiff methods of Gear (i.e., the
C                         backward differentiation formulas), and is
C                         used for stiff problems.
C               MINT = 3  Means the program dynamically selects the
C                         Adams methods when the problem is non-stiff
C                         and the Gear methods when the problem is
C                         stiff.
C             MINT may not be changed without restarting, i.e., setting
C             the magnitude of MSTATE to 1.
C
C    WORK
C    LENW   = (Input)
C             WORK is an array of LENW double precision words used
C             internally for temporary storage.  The user must allocate
C             space for this array in the calling program by a statement
C             such as
C                       DOUBLE PRECISION WORK(...)
C             The length of WORK should be at least
C               16*N + 2*NROOT + 204         if MINT is 1, or
C               N*N + 10*N + 2*NROOT + 204   if MINT is 2, or
C               N*N + 17*N + 2*NROOT + 204   if MINT is 3,
C             and LENW should be set to the value used.  The contents of
C             WORK should not be disturbed between calls to DDRIV2.
C
C    IWORK
C    LENIW  = (Input)
C             IWORK is an integer array of length LENIW used internally
C             for temporary storage.  The user must allocate space for
C             this array in the calling program by a statement such as
C                       INTEGER IWORK(...)
C             The length of IWORK should be at least
C               21      if MINT is 1, or
C               N+21    if MINT is 2 or 3,
C             and LENIW should be set to the value used.  The contents
C             of IWORK should not be disturbed between calls to DDRIV2.
C
C    G      = A double precision FORTRAN function supplied by the user
C             if NROOT is not 0.  In this case, the name must be
C             declared EXTERNAL in the user's calling program.  G is
C             repeatedly called with different values of IROOT to
C             obtain the value of each of the NROOT equations for which
C             a root is desired.  G is of the form:
C                   DOUBLE PRECISION FUNCTION G (N, T, Y, IROOT)
C                   DOUBLE PRECISION Y(*)
C                   GO TO (10, ...), IROOT
C              10   G = ...
C                     .
C                     .
C                   END (Sample)
C             Here, Y is a vector of length at least N, whose first N
C             components are the solution components at the point T.
C             The user should not alter these values.  The actual length
C             of Y is determined by the user's declaration in the
C             program which calls DDRIV2.  Thus the dimensioning of Y in
C             G, while required by FORTRAN convention, does not actually
C             allocate any storage.  Normally a return from G passes
C             control back to  DDRIV2.  However, if the user would like
C             to abort the calculation, i.e., return control to the
C             program which calls DDRIV2, he should set N to zero.
C             DDRIV2 will signal this by returning a value of MSTATE
C             equal to +7(-7).  In this case, the index of the equation
C             being evaluated is stored in the sixth element of IWORK.
C             Altering the value of N in G has no effect on the value of
C             N in the call sequence of DDRIV2.
C
C***LONG DESCRIPTION
C
C  III.  OTHER COMMUNICATION TO THE USER  ..............................
C
C    A. The solver communicates to the user through the parameters
C       above.  In addition it writes diagnostic messages through the
C       standard error handling program XERROR.  That program will
C       terminate the user's run if it detects a probable problem setup
C       error, e.g., insufficient storage allocated by the user for the
C       WORK array.  Messages are written on the standard error message
C       file.  At installations which have this error handling package
C       the user should determine the standard error handling file from
C       the local documentation.  Otherwise the short but serviceable
C       routine, XERROR, available with this package, can be used.  That
C       program writes on logical unit 6 to transmit messages.  A
C       complete description of XERROR is given in the Sandia
C       Laboratories report SAND78-1189 by R. E. Jones.
C
C    B. The first three elements of WORK and the first five elements of
C       IWORK will contain the following statistical data:
C         AVGH     The average step size used.
C         HUSED    The step size last used (successfully).
C         AVGORD   The average order used.
C         IMXERR   The index of the element of the solution vector that
C                  contributed most to the last error test.
C         NQUSED   The order last used (successfully).
C         NSTEP    The number of steps taken since last initialization.
C         NFE      The number of evaluations of the right hand side.
C         NJE      The number of evaluations of the Jacobian matrix.
C
C  IV.  REMARKS  .......................................................
C
C    A. On any return from DDRIV2 all information necessary to continue
C       the calculation is contained in the call sequence parameters,
C       including the work arrays.  Thus it is possible to suspend one
C       problem, integrate another, and then return to the first.
C
C    B. If this package is to be used in an overlay situation, the user
C       must declare in the primary overlay the variables in the call
C       sequence to DDRIV2.
C
C    C. When the routine G is not required, difficulties associated with
C       an unsatisfied external can be avoided by using the name of the
C       routine which calculates the right hand side of the differential
C       equations in place of G in the call sequence of DDRIV2.
C
C  V.  USAGE  ..........................................................
C
C               PROGRAM SAMPLE
C               EXTERNAL F
C               PARAMETER(MINT = 1, NROOT = 0, N = ...,
C              8          LENW = 16*N + 2*NROOT + 204, LENIW = 21)
C                                           N is the number of equations
C               DOUBLE PRECISION EPS, EWT, T, TOUT, WORK(LENW), Y(N)
C               INTEGER IWORK(LENIW)
C               OPEN(FILE='TAPE6', UNIT=6, STATUS='NEW')
C               T = 0.                           Initial point
C               DO 10 I = 1,N
C          10     Y(I) = ...                     Set initial conditions
C               TOUT = T
C               EWT = ...
C               MSTATE = 1
C               EPS = ...
C          20   CALL DDRIV2 (N, T, Y, F, TOUT, MSTATE, NROOT, EPS, EWT,
C              8             MINT, WORK, LENW, IWORK, LENIW, F)
C                                          Last argument is not the same
C                                          as F if rootfinding is used.
C               IF (MSTATE .GT. 2) STOP
C               WRITE(6, 100) TOUT, (Y(I), I=1,N)
C               TOUT = TOUT + 1.
C               IF (TOUT .LE. 10.) GO TO 20
C          100  FORMAT(...)
C               END (Sample)
C
C***REFERENCES  GEAR, C. W., "NUMERICAL INITIAL VALUE PROBLEMS IN
C                 ORDINARY DIFFERENTIAL EQUATIONS", PRENTICE-HALL, 1971.
C***ROUTINES CALLED  DDRIV3,XERROR
C***END PROLOGUE  DDRIV2
C    From the book "Numerical Methods and Software"
C       by D. Kahaner, C. Moler, S. Nash
C          Prentice Hall 1988
#endif
      EXTERNAL F, G
      DOUBLE PRECISION EPS, EWT, EWTCOM(1), G, HMAX, T, TOUT, &
          WORK(*), Y(*)
      INTEGER IWORK(*)
      CHARACTER MSG*81
      PARAMETER(IMPL = 0, MXSTEP = 1000)
!C***FIRST EXECUTABLE STATEMENT  DDRIV2
      IF (MINT .LT. 1 .OR. MINT .GT. 3) THEN
        WRITE(MSG, '(''DDRIV21FE Illegal input. Improper value for '', ''the integration method flag,'', I8)') MINT
        CALL XERROR(MSG(1:81), 81, 21, 2)
        RETURN
      END IF
      IF (MSTATE .GE. 0) THEN
        NSTATE = MSTATE
        NTASK = 1
      ELSE
        NSTATE = - MSTATE
        NTASK = 3
      END IF
      EWTCOM(1) = EWT
      IF (EWT .NE. 0.D0) THEN
        IERROR = 3
      ELSE
        IERROR = 2
      END IF
      IF (MINT .EQ. 1) THEN
        MITER = 0
        MXORD = 12
      ELSE IF (MINT .EQ. 2) THEN
        MITER = 2
        MXORD = 5
      ELSE IF (MINT .EQ. 3) THEN
        MITER = 2
        MXORD = 12
      END IF
      HMAX = 2.D0*ABS(TOUT - T)
      CALL DDRIV3 (N, T, Y, F, NSTATE, TOUT, NTASK, NROOT, EPS, EWTCOM,   &
                 IERROR, MINT, MITER, IMPL, ML, MU, MXORD, HMAX, WORK,  &
                  LENW, IWORK, LENIW, F, F, NDE, MXSTEP, G, F)
      IF (MSTATE .GE. 0) THEN
        MSTATE = NSTATE
      ELSE
        MSTATE = - NSTATE
      END IF
    END SUBROUTINE
    
      SUBROUTINE DDRIV3 (N,T,Y,F,NSTATE,TOUT,NTASK,NROOT,EPS,EWT,IERROR, &
        MINT,MITER,IMPL,ML,MU,MXORD,HMAX,WORK,LENW,IWORK,LENIW,JACOBN,  &
        FA,NDE,MXSTEP,G,USERS)
!C***BEGIN PROLOGUE  DDRIV3
!C   THIS PROLOGUE HAS BEEN OMITTED FOR REASONS OF SPACE
!C   FOR A COMPLETE COPY OF THIS SUBROUTINE CONTACT THE AUTHORS
!C   From the book "Numerical Methods and Software"
!C      by D. Kahaner, C. Moler, S. Nash
!C         Prentice Hall 1988
!C***END PROLOGUE  DDRIV3
      EXTERNAL F, JACOBN, FA, G, USERS
      DOUBLE PRECISION AE, BIG, EPS, EWT(*), G, GLAST, H, HMAX, HSIGN,  &
          NROUND, RE, D1MACH, SIZE, DNRM2, SUM, T, TLAST, TOUT, TROOT, &
          UROUND, WORK(*), Y(*)
      INTEGER IWORK(*)
      LOGICAL CONVRG
      CHARACTER MSG*205
      PARAMETER(NROUND = 20.D0)
      PARAMETER(IAVGH = 1, IHUSED = 2, IAVGRD = 3,                        &
               IEL = 4, IH = 160, IHMAX = 161, IHOLD = 162,              &
               IHSIGN = 163, IRC = 164, IRMAX = 165, IT = 166,           &
               ITOUT = 167, ITQ = 168, ITREND = 204, IYH = 205,          &
               INDMXR = 1, INQUSD = 2, INSTEP = 3, INFE = 4, INJE = 5,   &
               INROOT = 6, ICNVRG = 7, IJROOT = 8, IJTASK = 9,           &
               IMNTLD = 10, IMTRLD = 11, INQ = 12, INRTLD = 13,          &
               INDTRT = 14, INWAIT = 15, IMNT = 16, IMTRSV = 17,         &
               IMTR = 18, IMXRDS = 19, IMXORD = 20, INDPRT = 21,         &
               INDPVT = 22)
!C***FIRST EXECUTABLE STATEMENT  DDRIV3
      NPAR = N
      UROUND = D1MACH (4)
      IF (NROOT .NE. 0) THEN
        AE = D1MACH(1)
        RE = UROUND
      END IF
      IF (EPS .LT. 0.D0) THEN
        WRITE(MSG, '(''DDRIV36FE Illegal input.  EPS,'', D16.8, '', is negative.'')') EPS
        CALL XERROR(MSG(1:60), 60, 6, 2)
        RETURN
      END IF
      IF (N .LE. 0) THEN
        WRITE(MSG, '(''DDRIV37FE Illegal input.  Number of equations,'', I8, '', is not positive.'')') N
        CALL XERROR(MSG(1:72), 72, 7, 2)
        RETURN
      END IF
      IF (MXORD .LE. 0) THEN
        WRITE(MSG, '(''DDRIV314FE Illegal input.  Maximum order,'', I8, '', is not positive.'')') MXORD
      
        CALL XERROR(MSG(1:67), 67, 14, 2)
        RETURN
      END IF
      IF ((MINT .LT. 1 .OR. MINT .GT. 3) .OR. (MINT .EQ. 3 .AND.           &
       (MITER .EQ. 0 .OR. MITER .EQ. 3 .OR. IMPL .NE. 0))                 &
       .OR. (MITER .LT. 0 .OR. MITER .GT. 5) .OR.                         &
       (IMPL .NE. 0 .AND. IMPL .NE. 1 .AND. IMPL .NE. 2) .OR.             &
       ((IMPL .EQ. 1 .OR. IMPL .EQ. 2) .AND. MITER .EQ. 0) .OR.           &
       (IMPL .EQ. 2 .AND. MINT .EQ. 1) .OR.                               &
       (NSTATE .LT. 1 .OR. NSTATE .GT. 10)) THEN
        WRITE(MSG, '(''DDRIV39FE Illegal input.  Improper value for '',''NSTATE(MSTATE), MINT, MITER or IMPL.'')')
       
        CALL XERROR(MSG(1:81), 81, 9, 2)
        RETURN
      END IF
      IF (MITER .EQ. 0 .OR. MITER .EQ. 3) THEN
        LIWCHK = INDPVT - 1
      ELSE IF (MITER .EQ. 1 .OR. MITER .EQ. 2 .OR. MITER .EQ. 4 .OR. &
       MITER .EQ. 5) THEN
        LIWCHK = INDPVT + N - 1
      END IF
      IF (LENIW .LT. LIWCHK) THEN
        WRITE(MSG, '(''DDRIV310FE Illegal input.  Insufficient '',   &
       ''storage allocated for the IWORK array.  Based on the '')')
        WRITE(MSG(94:), '(''value of the input parameters involved, '', &
       ''the required storage is'', I8)') LIWCHK
        CALL XERROR(MSG(1:164), 164, 10, 2)
        RETURN
      END IF
!C                                                Allocate the WORK array
!C                                         IYH is the index of YH in WORK
      IF (MINT .EQ. 1 .OR. MINT .EQ. 3) THEN
        MAXORD = MIN(MXORD, 12)
      ELSE IF (MINT .EQ. 2) THEN
        MAXORD = MIN(MXORD, 5)
      END IF
      IDFDY = IYH + (MAXORD + 1)*N
!C                                             IDFDY is the index of DFDY
!C
      IF (MITER .EQ. 0 .OR. MITER .EQ. 3)  THEN
        IYWT = IDFDY
      ELSE IF (MITER .EQ. 1 .OR. MITER .EQ. 2)  THEN
        IYWT = IDFDY + N*N
      ELSE IF (MITER .EQ. 4 .OR. MITER .EQ. 5)  THEN
        IYWT = IDFDY + (2*ML + MU + 1)*N
      END IF
!C                                               IYWT is the index of YWT
      ISAVE1 = IYWT + N
!C                                           ISAVE1 is the index of SAVE1
      ISAVE2 = ISAVE1 + N
!C                                           ISAVE2 is the index of SAVE2
      IGNOW = ISAVE2 + N
!C                                             IGNOW is the index of GNOW
      ITROOT = IGNOW + NROOT
!C                                           ITROOT is the index of TROOT
      IFAC = ITROOT + NROOT
!C                                               IFAC is the index of FAC
      IF (MITER .EQ. 2 .OR. MITER .EQ. 5 .OR. MINT .EQ. 3) THEN
        IA = IFAC + N
      ELSE
        IA = IFAC
      END IF
!C                                                   IA is the index of A
      IF (IMPL .EQ. 0 .OR. MITER .EQ. 3) THEN
        LENCHK = IA - 1
      ELSE IF (IMPL .EQ. 1 .AND. (MITER .EQ. 1 .OR. MITER .EQ. 2)) THEN
        LENCHK = IA - 1 + N*N
      ELSE IF (IMPL .EQ. 1 .AND. (MITER .EQ. 4 .OR. MITER .EQ. 5)) THEN
        LENCHK = IA - 1 + (2*ML + MU + 1)*N
      ELSE IF (IMPL .EQ. 2 .AND. MITER .NE. 3) THEN
        LENCHK = IA - 1 + N
      END IF
      IF (LENW .LT. LENCHK) THEN
        WRITE(MSG, '(''DDRIV38FE Illegal input.  Insufficient '',    &
       ''storage allocated for the WORK array.  Based on the '')')
        WRITE(MSG(92:), '(''value of the input parameters involved, '', &
       ''the required storage is'', I8)') LENCHK
        CALL XERROR(MSG(1:162), 162, 8, 2)
        RETURN
      END IF
      IF (MITER .EQ. 0 .OR. MITER .EQ. 3) THEN
        MATDIM = 1
      ELSE IF (MITER .EQ. 1 .OR. MITER .EQ. 2) THEN
        MATDIM = N
      ELSE IF (MITER .EQ. 4 .OR. MITER .EQ. 5) THEN
        MATDIM = 2*ML + MU + 1
      END IF
      IF (IMPL .EQ. 0 .OR. IMPL .EQ. 1) THEN
        NDECOM = N
      ELSE IF (IMPL .EQ. 2) THEN
        NDECOM = NDE
      END IF
      IF (NSTATE .EQ. 1) THEN
!C                                                  Initialize parameters
        IF (MINT .EQ. 1 .OR. MINT .EQ. 3) THEN
          IWORK(IMXORD) = MIN(MXORD, 12)
        ELSE IF (MINT .EQ. 2) THEN
          IWORK(IMXORD) = MIN(MXORD, 5)
        END IF
        IWORK(IMXRDS) = MXORD
        IF (MINT .EQ. 1 .OR. MINT .EQ. 2) THEN
          IWORK(IMNT) = MINT
          IWORK(IMTR) = MITER
          IWORK(IMNTLD) = MINT
          IWORK(IMTRLD) = MITER
        ELSE IF (MINT .EQ. 3) THEN
          IWORK(IMNT) = 1
          IWORK(IMTR) = 0
          IWORK(IMNTLD) = IWORK(IMNT)
          IWORK(IMTRLD) = IWORK(IMTR)
          IWORK(IMTRSV) = MITER
        END IF
        WORK(IHMAX) = HMAX
        H = (TOUT - T)*(1.D0 - 4.D0*UROUND)
        H = SIGN(MIN(ABS(H), HMAX), H)
        WORK(IH) = H
        HSIGN = SIGN(1.D0, H)
        WORK(IHSIGN) = HSIGN
        IWORK(IJTASK) = 0
        WORK(IAVGH) = 0.D0
        WORK(IHUSED) =0.D0
        WORK(IAVGRD) = 0.D0
        IWORK(INDMXR) = 0
        IWORK(INQUSD) = 0
        IWORK(INSTEP) = 0
        IWORK(INFE) = 0
        IWORK(INJE) = 0
        IWORK(INROOT) = 0
        WORK(IT) = T
        IWORK(ICNVRG) = 0
        IWORK(INDPRT) = 0
!C                                                 Set initial conditions
        DO 30 I = 1,N
          JYH = I + IYH - 1
 30       WORK(JYH) = Y(I)
        IF (T .EQ. TOUT) RETURN
        GO TO 180
      END IF
!C                                             On a continuation, check
!C                                             that output points have
!C                                             been or will be overtaken.
      IF (IWORK(ICNVRG) .EQ. 1) THEN
        CONVRG = .TRUE.
      ELSE
        CONVRG = .FALSE.
      END IF
      T = WORK(IT)
      H = WORK(IH)
      HSIGN = WORK(IHSIGN)
      IF (IWORK(IJTASK) .EQ. 0) GO TO 180
!C
!C                                   IWORK(IJROOT) flags unreported
!C                                   roots, and is set to the value of
!C                                   NTASK when a root was last selected.
!C                                   It is set to zero when all roots
!C                                   have been reported.  IWORK(INROOT)
!C                                   contains the index and WORK(ITOUT)
!C                                   contains the value of the root last
!C                                   selected to be reported.
!C                                   IWORK(INRTLD) contains the value of
!C                                   NROOT and IWORK(INDTRT) contains
!C                                   the value of ITROOT when the array
!C                                   of roots was last calculated.
      IF (NROOT .NE. 0) THEN
        JROOT = IWORK(IJROOT)
        IF (JROOT .GT. 0) THEN
!C                                      TOUT has just been reported.
!C                                      If TROOT .LE. TOUT, report TROOT.
          IF (NSTATE .NE. 5) THEN
            IF (TOUT*HSIGN .GE. WORK(ITOUT)*HSIGN) THEN
              TROOT = WORK(ITOUT)
              CALL DDNTP(H, 0, N, IWORK(INQ), T, TROOT, WORK(IYH),  Y)
              T = TROOT
              NSTATE = 5
              GO TO 580
            END IF
!C                                         A root has just been reported.
!C                                         Select the next root.
          ELSE
            TROOT = T
            IROOT = 0
            DO 50 I = 1,IWORK(INRTLD)
              JTROOT = IWORK(INDTRT) + I - 1
              IF (WORK(JTROOT)*HSIGN .LE. TROOT*HSIGN) THEN
!C
!C                                              Check for multiple roots.
!C
                IF (WORK(JTROOT) .EQ. WORK(ITOUT) .AND. &
               I .GT. IWORK(INROOT)) THEN
                  IROOT = I
                  TROOT = WORK(JTROOT)
                  GO TO 60
                END IF
                IF (WORK(JTROOT)*HSIGN .GT. WORK(ITOUT)*HSIGN) THEN
                  IROOT = I
                  TROOT = WORK(JTROOT)
                END IF
              END IF
 50           CONTINUE
 60         IWORK(INROOT) = IROOT
            WORK(ITOUT) = TROOT
            IWORK(IJROOT) = NTASK
            IF (NTASK .EQ. 1) THEN
              IF (IROOT .EQ. 0) THEN
                IWORK(IJROOT) = 0
              ELSE
                IF (TOUT*HSIGN .GE. TROOT*HSIGN) THEN
                  CALL DDNTP(H, 0, N, IWORK(INQ), T, TROOT,WORK(IYH),Y)
                  NSTATE = 5
                  T = TROOT
                  GO TO 580
                END IF
              END IF
            ELSE IF (NTASK .EQ. 2 .OR. NTASK .EQ. 3) THEN
!C
!C                                     If there are no more roots, or the
!C                                     user has altered TOUT to be less
!C                                     than a root, set IJROOT to zero.
!C
              IF (IROOT .EQ. 0 .OR. (TOUT*HSIGN .LT. TROOT*HSIGN)) THEN
                IWORK(IJROOT) = 0
              ELSE
                CALL DDNTP(H, 0, N, IWORK(INQ), T, TROOT, WORK(IYH), Y)
                NSTATE = 5
                T = TROOT
                GO TO 580
              END IF
            END IF
          END IF
        END IF
      END IF
!C
      IF (NTASK .EQ. 1) THEN
        NSTATE = 2
        IF (T*HSIGN .GE. TOUT*HSIGN) THEN
          CALL DDNTP (H, 0, N, IWORK(INQ), T, TOUT, WORK(IYH),  Y)
          T = TOUT
          GO TO 580
        END IF
      ELSE IF (NTASK .EQ. 2) THEN
!C                                                      Check if TOUT has
!C                                                      been reset .LT. T
        IF (T*HSIGN .GT. TOUT*HSIGN) THEN
          WRITE(MSG, '(''DDRIV32WRN With NTASK='', I1, '' on input, '',  &
         ''T,'', D16.8, '', was beyond TOUT,'', D16.8, ''.  Solution'', &
         '' obtained by interpolation.'')') NTASK, T, TOUT
          CALL XERROR(MSG(1:124), 124, 2, 0)
          CALL DDNTP (H, 0, N, IWORK(INQ), T, TOUT, WORK(IYH),  Y)
          T = TOUT
          NSTATE = 2
          GO TO 580
        END IF
!C                                   Determine if TOUT has been overtaken
!C
        IF (ABS(TOUT - T).LE.NROUND*UROUND*MAX(ABS(T), ABS(TOUT))) THEN
          T = TOUT
          NSTATE = 2
          GO TO 560
        END IF
!C                                             If there are no more roots
!C                                             to report, report T.
        IF (NSTATE .EQ. 5) THEN
          NSTATE = 2
          GO TO 560
        END IF
        NSTATE = 2
!C                                                       See if TOUT will
!C                                                       be overtaken.
        IF ((T + H)*HSIGN .GT. TOUT*HSIGN) THEN
          H = TOUT - T
          IF ((T + H)*HSIGN .GT. TOUT*HSIGN) H = H*(1.D0 - 4.D0*UROUND)
          WORK(IH) = H
          IF (H .EQ. 0.D0) GO TO 670
          IWORK(IJTASK) = -1
        END IF
      ELSE IF (NTASK .EQ. 3) THEN
        NSTATE = 2
        IF (T*HSIGN .GT. TOUT*HSIGN) THEN
          WRITE(MSG, '(''DDRIV32WRN With NTASK='', I1, '' on input, '',   &
         ''T,'', D16.8, '', was beyond TOUT,'', D16.8, ''.  Solution'',  &
         '' obtained by interpolation.'')') NTASK, T, TOUT
          CALL XERROR(MSG(1:124), 124, 2, 0)
          CALL DDNTP (H, 0, N, IWORK(INQ), T, TOUT, WORK(IYH),  Y)
          T = TOUT
          GO TO 580
        END IF
        IF (ABS(TOUT - T).LE.NROUND*UROUND*MAX(ABS(T), ABS(TOUT))) THEN
          T = TOUT
          GO TO 560
        END IF
        IF ((T + H)*HSIGN .GT. TOUT*HSIGN) THEN
          H = TOUT - T
          IF ((T + H)*HSIGN .GT. TOUT*HSIGN) H = H*(1.D0 - 4.D0*UROUND)
          WORK(IH) = H
          IF (H .EQ. 0.D0) GO TO 670
          IWORK(IJTASK) = -1
        END IF
      END IF
!C                         Implement changes in MINT, MITER, and/or HMAX.
!C
      IF ((MINT .NE. IWORK(IMNTLD) .OR. MITER .NE. IWORK(IMTRLD)) .AND. &
       MINT .NE. 3 .AND. IWORK(IMNTLD) .NE. 3) IWORK(IJTASK) = -1
      IF (HMAX .NE. WORK(IHMAX)) THEN
        H = SIGN(MIN(ABS(H), HMAX), H)
        IF (H .NE. WORK(IH)) THEN
          IWORK(IJTASK) = -1
          WORK(IH) = H
        END IF
        WORK(IHMAX) = HMAX
      END IF

 180  NSTEPL = IWORK(INSTEP)
      DO 190 I = 1,N
        JYH = IYH + I - 1
 190    Y(I) = WORK(JYH)
      IF (NROOT .NE. 0) THEN
        DO 200 I = 1,NROOT
          JGNOW = IGNOW + I - 1
          WORK(JGNOW) = G (NPAR, T, Y, I)
          IF (NPAR .EQ. 0) THEN
            IWORK(INROOT) = I
            NSTATE = 7
            RETURN
          END IF
 200     CONTINUE
      END IF
      IF (IERROR .EQ. 1) THEN
        DO 230 I = 1,N
          JYWT = I + IYWT - 1
 230      WORK(JYWT) = 1.D0
        GO TO 410
      ELSE IF (IERROR .EQ. 5) THEN
        DO 250 I = 1,N
          JYWT = I + IYWT - 1
 250      WORK(JYWT) = EWT(I)
        GO TO 410
      END IF
!C                                       Reset YWT array.  Looping point.
 260  IF (IERROR .EQ. 2) THEN
        DO 280 I = 1,N
          IF (Y(I) .EQ. 0.D0) GO TO 290
          JYWT = I + IYWT - 1
 280      WORK(JYWT) = ABS(Y(I))
        GO TO 410
 290    IF (IWORK(IJTASK) .EQ. 0) THEN
          CALL F (NPAR, T, Y, WORK(ISAVE2))
          IF (NPAR .EQ. 0) THEN
            NSTATE = 6
            RETURN
          END IF
          IWORK(INFE) = IWORK(INFE) + 1
          IF (MITER .EQ. 3 .AND. IMPL .NE. 0) THEN
            IFLAG = 0
            CALL USERS(Y, WORK(IYH), WORK(IYWT), WORK(ISAVE1),      &
                      WORK(ISAVE2), T, H, WORK(IEL), IMPL, NPAR,   &
                      NDECOM, IFLAG)
            IF (NPAR .EQ. 0) THEN
              NSTATE = 10
              RETURN
            END IF
          ELSE IF (IMPL .EQ. 1) THEN
            IF (MITER .EQ. 1 .OR. MITER .EQ. 2) THEN
              CALL FA (NPAR, T, Y, WORK(IA), MATDIM, ML, MU, NDECOM)
              IF (NPAR .EQ. 0) THEN
                NSTATE = 9
                RETURN
              END IF
              CALL DGEFA (WORK(IA), MATDIM, N, IWORK(INDPVT), INFO)
              IF (INFO .NE. 0) GO TO 690
              CALL DGESL(WORK(IA),MATDIM,N,IWORK(INDPVT),WORK(ISAVE2),0)
            ELSE IF (MITER .EQ. 4 .OR. MITER .EQ. 5) THEN
              JAML = IA + ML
              CALL FA (NPAR, T, Y, WORK(JAML), MATDIM, ML, MU, NDECOM)
              IF (NPAR .EQ. 0) THEN
                NSTATE = 9
                RETURN
              END IF
              CALL DGBFA (WORK(IA),MATDIM,N,ML,MU,IWORK(INDPVT),INFO)
              IF (INFO .NE. 0) GO TO 690
              CALL DGBSL (WORK(IA), MATDIM, N, ML, MU, IWORK(INDPVT),  &
                         WORK(ISAVE2), 0)
            END IF
          ELSE IF (IMPL .EQ. 2) THEN
            CALL FA (NPAR, T, Y, WORK(IA), MATDIM, ML, MU, NDECOM)
            IF (NPAR .EQ. 0) THEN
              NSTATE = 9
              RETURN
            END IF
            DO 340 I = 1,NDECOM
              JA = I + IA - 1
              JSAVE2 = I + ISAVE2 - 1
              IF (WORK(JA) .EQ. 0.D0) GO TO 690
 340          WORK(JSAVE2) = WORK(JSAVE2)/WORK(JA)
          END IF
        END IF
        DO 360 J = I,N
          JYWT = J + IYWT - 1
          IF (Y(J) .NE. 0.D0) THEN
            WORK(JYWT) = ABS(Y(J))
          ELSE
            IF (IWORK(IJTASK) .EQ. 0) THEN
              JSAVE2 = J + ISAVE2 - 1
              WORK(JYWT) = ABS(H*WORK(JSAVE2))
            ELSE
              JHYP = J + IYH + N - 1
              WORK(JYWT) = ABS(WORK(JHYP))
            END IF
          END IF
          IF (WORK(JYWT) .EQ. 0.D0) WORK(JYWT) = UROUND
 360      CONTINUE
      ELSE IF (IERROR .EQ. 3) THEN
        DO 380 I = 1,N
          JYWT = I + IYWT - 1
 380      WORK(JYWT) = MAX(EWT(1), ABS(Y(I)))
      ELSE IF (IERROR .EQ. 4) THEN
        DO 400 I = 1,N
          JYWT = I + IYWT - 1
 400      WORK(JYWT) = MAX(EWT(I), ABS(Y(I)))
      END IF
!C
 410  DO 420 I = 1,N
        JYWT = I + IYWT - 1
        JSAVE2 = I + ISAVE2 - 1
 420    WORK(JSAVE2) = Y(I)/WORK(JYWT)
      SUM = DNRM2(N, WORK(ISAVE2), 1)/SQRT(DBLE(N))
      IF (EPS .LT. SUM*UROUND) THEN
        EPS = SUM*UROUND*(1.D0 + 10.D0*UROUND)
        WRITE(MSG, '(''DDRIV34REC At T,'', D16.8, '', the requested '',   &
       ''accuracy, EPS, was not obtainable with the machine '',          &
       ''precision.  EPS has been increased to'')') T
        WRITE(MSG(137:), '(D16.8)') EPS
        CALL XERROR(MSG(1:152), 152, 4, 1)
        NSTATE = 4
        GO TO 560
      END IF
      IF (ABS(H) .GE. UROUND*ABS(T)) THEN
        IWORK(INDPRT) = 0
      ELSE IF (IWORK(INDPRT) .EQ. 0) THEN
        WRITE(MSG, '(''DDRIV35WRN At T,'', D16.8, '', the step size,'',  &
       D16.8, '', is smaller than the roundoff level of T.  '')') T, H
        WRITE(MSG(109:), '(''This may occur if there is an abrupt '',  &
       ''change in the right hand side of the differential '',        &
       ''equations.'')')
        CALL XERROR(MSG(1:205), 205, 5, 0)
        IWORK(INDPRT) = 1
      END IF
      IF (NTASK.NE.2) THEN
        IF ((IWORK(INSTEP)-NSTEPL) .GT. MXSTEP) THEN
          WRITE(MSG, '(''DDRIV33WRN At T,'', D16.8, '', '', I8,
     8    '' steps have been taken without reaching TOUT,'', D16.8)')
     8    T, MXSTEP, TOUT
          CALL XERROR(MSG(1:103), 103, 3, 0)
          NSTATE = 3
          GO TO 560
        END IF
      END IF
C
C     CALL DDSTP (EPS, F, FA, HMAX, IMPL, JACOBN, MATDIM, MAXORD,
C    8            MINT, MITER, ML, MU, N, NDE, YWT, UROUND, USERS,
C    8            AVGH, AVGORD, H, HUSED, JTASK, MNTOLD, MTROLD,
C    8            NFE, NJE, NQUSED, NSTEP, T, Y, YH,  A, CONVRG,
C    8            DFDY, EL, FAC, HOLD, IPVT, JSTATE, NQ, NWAIT, RC,
C    8            RMAX, SAVE1, SAVE2, TQ, TREND, ISWFLG, MTRSV, MXRDSV)
C
      CALL DDSTP (EPS, F, FA, WORK(IHMAX), IMPL, JACOBN, MATDIM,
     8            IWORK(IMXORD), IWORK(IMNT), IWORK(IMTR), ML, MU, NPAR,
     8           NDECOM, WORK(IYWT), UROUND, USERS,  WORK(IAVGH),
     8           WORK(IAVGRD), WORK(IH), WORK(IHUSED), IWORK(IJTASK),
     8           IWORK(IMNTLD), IWORK(IMTRLD), IWORK(INFE), IWORK(INJE),
     8            IWORK(INQUSD), IWORK(INSTEP), WORK(IT), Y, WORK(IYH),
     8            WORK(IA), CONVRG, WORK(IDFDY), WORK(IEL), WORK(IFAC),
     8            WORK(IHOLD), IWORK(INDPVT), JSTATE, IWORK(INQ),
     8            IWORK(INWAIT), WORK(IRC), WORK(IRMAX), WORK(ISAVE1),
     8            WORK(ISAVE2), WORK(ITQ), WORK(ITREND), MINT,
     8            IWORK(IMTRSV), IWORK(IMXRDS))
      T = WORK(IT)
      H = WORK(IH)
      GO TO (470, 670, 680, 690, 690, 660, 660, 660, 660, 660), JSTATE
 470  IWORK(IJTASK) = 1
C                                 Determine if a root has been overtaken
      IF (NROOT .NE. 0) THEN
        IROOT = 0
        DO 500 I = 1,NROOT
          JTROOT = ITROOT + I - 1
          JGNOW = IGNOW + I - 1
          GLAST = WORK(JGNOW)
          WORK(JGNOW) = G (NPAR, T, Y, I)
          IF (NPAR .EQ. 0) THEN
            IWORK(INROOT) = I
            NSTATE = 7
            RETURN
          END IF
          IF (GLAST*WORK(JGNOW) .GT. 0.D0) THEN
            WORK(JTROOT) = T + H
          ELSE
            IF (WORK(JGNOW) .EQ. 0.D0) THEN
              WORK(JTROOT) = T
              IROOT = I
            ELSE
              IF (GLAST .EQ. 0.D0) THEN
                WORK(JTROOT) = T + H
              ELSE
                IF (ABS(WORK(IHUSED)) .GE. UROUND*ABS(T)) THEN
                  TLAST = T - WORK(IHUSED)
                  IROOT = I
                  TROOT = T
                  CALL DDZRO (AE, G, H, NPAR, IWORK(INQ), IROOT, RE, T,
     8                        WORK(IYH), UROUND,  TROOT, TLAST,
     8                        WORK(JGNOW), GLAST,  Y)
                  DO 480 J = 1,N
  480               Y(J) = WORK(IYH + J -1)
                  IF (NPAR .EQ. 0) THEN
                    IWORK(INROOT) = I
                    NSTATE = 7
                    RETURN
                  END IF
                  WORK(JTROOT) = TROOT
                ELSE
                  WORK(JTROOT) = T
                  IROOT = I
                END IF
              END IF
            END IF
          END IF
 500      CONTINUE
        IF (IROOT .EQ. 0) THEN
          IWORK(IJROOT) = 0
!C                                                  Select the first root
        ELSE
          IWORK(IJROOT) = NTASK
          IWORK(INRTLD) = NROOT
          IWORK(INDTRT) = ITROOT
          TROOT = T + H
          DO 510 I = 1,NROOT
            JTROOT = ITROOT + I - 1
            IF (WORK(JTROOT)*HSIGN .LT. TROOT*HSIGN) THEN
              TROOT = WORK(JTROOT)
              IROOT = I
            END IF
 510        CONTINUE
          IWORK(INROOT) = IROOT
          WORK(ITOUT) = TROOT
          IF (TROOT*HSIGN .LE. TOUT*HSIGN) THEN
            CALL DDNTP (H, 0, N, IWORK(INQ), T, TROOT, WORK(IYH),  Y)
            NSTATE = 5
            T = TROOT
            GO TO 580
          END IF
        END IF
      END IF
!C                               Test for NTASK condition to be satisfied
      NSTATE = 2
      IF (NTASK .EQ. 1) THEN
        IF (T*HSIGN .LT. TOUT*HSIGN) GO TO 260
        CALL DDNTP (H, 0, N, IWORK(INQ), T, TOUT, WORK(IYH),  Y)
        T = TOUT
        GO TO 580
!C                               TOUT is assumed to have been attained
!C                               exactly if T is within twenty roundoff
!C                               units of TOUT, relative to max(TOUT, T).
      ELSE IF (NTASK .EQ. 2) THEN
        IF (ABS(TOUT - T).LE.NROUND*UROUND*MAX(ABS(T), ABS(TOUT))) THEN
          T = TOUT
        ELSE
          IF ((T + H)*HSIGN .GT. TOUT*HSIGN) THEN
            H = TOUT - T
            IF ((T + H)*HSIGN.GT.TOUT*HSIGN) H = H*(1.D0 - 4.D0*UROUND)
            WORK(IH) = H
            IF (H .EQ. 0.D0) GO TO 670
            IWORK(IJTASK) = -1
          END IF
        END IF
      ELSE IF (NTASK .EQ. 3) THEN
        IF (ABS(TOUT - T).LE.NROUND*UROUND*MAX(ABS(T), ABS(TOUT))) THEN
          T = TOUT
        ELSE
          IF ((T + H)*HSIGN .GT. TOUT*HSIGN) THEN
            H = TOUT - T
            IF ((T + H)*HSIGN.GT.TOUT*HSIGN) H = H*(1.D0 - 4.D0*UROUND)
            WORK(IH) = H
            IF (H .EQ. 0.D0) GO TO 670
            IWORK(IJTASK) = -1
          END IF
          GO TO 260
        END IF
      END IF
!C                                      All returns are made through this
!C                                      section.  IMXERR is determined.
 560  DO 570 I = 1,N
        JYH = I + IYH - 1
 570    Y(I) = WORK(JYH)
 580  IF (CONVRG) THEN
        IWORK(ICNVRG) = 1
      ELSE
        IWORK(ICNVRG) = 0
      END IF
      IF (IWORK(IJTASK) .EQ. 0) RETURN
      BIG = 0.D0
      IMXERR = 1
      IWORK(INDMXR) = IMXERR
      DO  590 I = 1,N
!C                                            SIZE = ABS(ERROR(I)/YWT(I))
        JYWT = I + IYWT - 1
        JERROR = I + ISAVE1 - 1
        SIZE = ABS(WORK(JERROR)/WORK(JYWT))
        IF (BIG .LT. SIZE) THEN
          BIG = SIZE
          IMXERR = I
          IWORK(INDMXR) = IMXERR
        END IF
 590    CONTINUE
      RETURN
!C
 660  NSTATE = JSTATE
      RETURN
!C                                        Fatal errors are processed here
!C
 670  WRITE(MSG, '(''DDRIV311FE At T,'', D16.8, '', the attempted '',  &
       ''step size has gone to zero.  Often this occurs if the '',    &
       ''problem setup is incorrect.'')') T
      CALL XERROR(MSG(1:129), 129, 11, 2)
      RETURN
!C
 680  WRITE(MSG, '(''DDRIV312FE At T,'', D16.8, '', the step size has'',
     8  '' been reduced about 50 times without advancing the '')') T
      WRITE(MSG(103:), '(''solution.  Often this occurs if the '',
     8  ''problem setup is incorrect.'')')
      CALL XERROR(MSG(1:165), 165, 12, 2)
      RETURN
!C
 690  WRITE(MSG, '(''DDRIV313FE At T,'', D16.8, '', while solving'',  &
       '' A*YDOT = F, A is singular.'')') T
      CALL XERROR(MSG(1:74), 74, 13, 2)
      RETURN
   END SUBROUTINE
   
      SUBROUTINE DDNTP (H,K,N,NQ,T,TOUT,YH,Y)
!C***BEGIN PROLOGUE  DDNTP
!C   THIS PROLOGUE HAS BEEN OMITTED FOR REASONS OF SPACE
!C   FOR A COMPLETE COPY OF THIS SUBROUTINE CONTACT THE AUTHORS
!C    From the book "Numerical Methods and Software"
!C       by D. Kahaner, C. Moler, S. Nash
!C          Prentice Hall 1988
!C***END PROLOGUE  DDNTP
      DOUBLE PRECISION FACTOR, H, R, T, TOUT, Y(*), YH(N,*)
!C***FIRST EXECUTABLE STATEMENT  DDNTP
      IF (K .EQ. 0) THEN
        DO 10 I = 1,N
 10       Y(I) = YH(I,NQ+1)
        R = ((TOUT - T)/H)
        DO 20 JJ = 1,NQ
          J = NQ + 1 - JJ
          DO 20 I = 1,N
 20         Y(I) = YH(I,J) + R*Y(I)
      ELSE
        KUSED = MIN(K, NQ)
        FACTOR = 1.D0
        DO 40 KK = 1,KUSED
 40       FACTOR = FACTOR*DBLE(NQ+1-KK)
        DO 50 I = 1,N
 50       Y(I) = FACTOR*YH(I,NQ+1)
        DO 80 JJ = KUSED+1,NQ
          J = K + 1 + NQ - JJ
          FACTOR = 1.D0
          DO 60 KK = 1,KUSED
 60         FACTOR = FACTOR*DBLE(J-KK)
          DO 70 I = 1,N
 70         Y(I) = FACTOR*YH(I,J) + R*Y(I)
 80       CONTINUE
        DO 100 I = 1,N
 100      Y(I) = Y(I)*H**(-KUSED)
      END IF
   END SUBROUTINE
   
      SUBROUTINE DDZRO (AE,F,H,N,NQ,IROOT,RE,T,YH,UROUND,B,C,FB,FC,Y)
!C***BEGIN PROLOGUE  DDZRO
!C   THIS PROLOGUE HAS BEEN OMITTED FOR REASONS OF SPACE
!C   FOR A COMPLETE COPY OF THIS SUBROUTINE CONTACT THE AUTHORS
!C    From the book "Numerical Methods and Software"
!C       by D. Kahaner, C. Moler, S. Nash
!C          Prentice Hall 1988
!C***ROUTINES CALLED  DDNTP
!C***END PROLOGUE  DDZRO
      DOUBLE PRECISION A, ACBS, ACMB, AE, B, C, CMB, ER, F, FA, FB, FC, &
          H, P, Q, RE, RW, T, TOL, UROUND, Y(*), YH(N,*)
!C***FIRST EXECUTABLE STATEMENT  DDZRO
      ER = 4.D0*UROUND
      RW = MAX(RE, ER)
      IC = 0
      ACBS = ABS(B - C)
      A = C
      FA = FC
      KOUNT = 0
!C                                                    Perform interchange
 10   IF (ABS(FC) .LT. ABS(FB)) THEN
        A = B
        FA = FB
        B = C
        FB = FC
        C = A
        FC = FA
      END IF
      CMB = 0.5D0*(C - B)
      ACMB = ABS(CMB)
      TOL = RW*ABS(B) + AE
!C                                                Test stopping criterion
      IF (ACMB .LE. TOL) RETURN
      IF (KOUNT .GT. 50) RETURN
!C                                    Calculate new iterate implicitly as
!C                                    B + P/Q, where we arrange P .GE. 0.
!C                         The implicit form is used to prevent overflow.
      P = (B - A)*FB
      Q = FA - FB
      IF (P .LT. 0.D0) THEN
        P = -P
        Q = -Q
      END IF
!C                          Update A and check for satisfactory reduction
!C                          in the size of our bounding interval.
      A = B
      FA = FB
      IC = IC + 1
      IF (IC .GE. 4) THEN
        IF (8.D0*ACMB .GE. ACBS) THEN
!C                                                                 Bisect
          B = 0.5D0*(C + B)
          GO TO 20
        END IF
        IC = 0
      END IF
      ACBS = ACMB
!C                                            Test for too small a change
      IF (P .LE. ABS(Q)*TOL) THEN
!C                                                 Increment by tolerance
        B = B + SIGN(TOL, CMB)
!C                                               Root ought to be between
!C                                               B and (C + B)/2.
      ELSE IF (P .LT. CMB*Q) THEN
!C                                                            Interpolate
        B = B + P/Q
      ELSE
!C                                                                 Bisect
        B = 0.5D0*(C + B)
      END IF
!C                                             Have completed computation
!C                                             for new iterate B.
 20   CALL DDNTP (H, 0, N, NQ, T, B, YH,  Y)
      FB = F(N, B, Y, IROOT)
      IF (N .EQ. 0) RETURN
      IF (FB .EQ. 0.D0) RETURN
      KOUNT = KOUNT + 1
!C
!C             Decide whether next step is interpolation or extrapolation
!C
      IF (SIGN(1.0D0, FB) .EQ. SIGN(1.0D0, FC)) THEN
        C = A
        FC = FA
      END IF
      GO TO 10
    END SUBROUTINE DDZRO
    
    SUBROUTINE DDSTP (EPS,F,FA,HMAX,IMPL,JACOBN,MATDIM,MAXORD,MINT,      &
        MITER,ML,MU,N,NDE,YWT,UROUND,USERS,AVGH,AVGORD,H,HUSED,JTASK,   &
        MNTOLD,MTROLD,NFE,NJE,NQUSED,NSTEP,T,Y,YH,A,CONVRG,DFDY,EL,FAC, &
        HOLD,IPVT,JSTATE,NQ,NWAIT,RC,RMAX,SAVE1,SAVE2,TQ,TREND,ISWFLG,  &
        MTRSV,MXRDSV)
!C***BEGIN PROLOGUE  DDSTP
!!C   THIS PROLOGUE HAS BEEN OMITTED FOR REASONS OF SPACE
!C   FOR A COMPLETE COPY OF THIS SUBROUTINE CONTACT THE AUTHORS
!C    From the book "Numerical Methods and Software"
!C       by D. Kahaner, C. Moler, S. Nash
!C          Prentice Hall 1988
!C***ROUTINES CALLED  DDNTL,DDPST,DDCOR,DDPSC,DDSCL,DNRM2
!C***END PROLOGUE  DDSTP
      EXTERNAL F, JACOBN, FA, USERS
      DOUBLE PRECISION A(MATDIM,*), AVGH, AVGORD, BIAS1, BIAS2, BIAS3,  &
          BND, CTEST, D, DENOM, DFDY(MATDIM,*), D1, EL(13,12), EPS,    &
          ERDN, ERUP, ETEST, FAC(*), H, HMAX, HN, HOLD, HS, HUSED,     &
          NUMER, RC, RCTEST, RH, RH1, RH2, RH3, RMAX, RMFAIL, RMNORM,  &
          SAVE1(*), SAVE2(*), DNRM2, T, TOLD, TQ(3,12), TREND, TRSHLD, &
          UROUND, Y(*), YH(N,*), YWT(*), Y0NRM
      INTEGER IPVT(*)
      LOGICAL CONVRG, EVALFA, EVALJC, IER, SWITCH
      PARAMETER(BIAS1 = 1.3D0, BIAS2 = 1.2D0, BIAS3 = 1.4D0, MXFAIL = 3, &
               MXITER = 3, MXTRY = 50, RCTEST = .3D0, RMFAIL = 2.D0,     &
               RMNORM = 10.D0, TRSHLD = 1.D0)
      DATA IER /.FALSE./
!C***FIRST EXECUTABLE STATEMENT  DDSTP
      NSV = N
      BND = 0.D0
      SWITCH = .FALSE.
      NTRY = 0
      TOLD = T
      NFAIL = 0
      IF (JTASK .LE. 0) THEN
        CALL DDNTL (EPS, F, FA, HMAX, HOLD, IMPL, JTASK, MATDIM,            &
                   MAXORD, MINT, MITER, ML, MU, N, NDE, SAVE1, T,          &
                   UROUND, USERS, Y, YWT,  H, MNTOLD, MTROLD, NFE, RC,     &
                   YH,  A, CONVRG, EL, FAC, IER, IPVT, NQ, NWAIT, RH,      &
                   RMAX, SAVE2, TQ, TREND, ISWFLG, JSTATE)
        IF (N .EQ. 0) GO TO 440
        IF (H .EQ. 0.D0) GO TO 400
        IF (IER) GO TO 420
      END IF
 100  NTRY = NTRY + 1
      IF (NTRY .GT. MXTRY) GO TO 410
      T = T + H
      CALL DDPSC (1, N, NQ,  YH)
      EVALJC = ((ABS(RC - 1.D0) .GT. RCTEST) .AND. (MITER .NE. 0))
      EVALFA = .NOT. EVALJC
!C
 110  ITER = 0
      DO 115 I = 1,N
 115    Y(I) = YH(I,1)
      CALL F (N, T, Y, SAVE2)
      IF (N .EQ. 0) THEN
        JSTATE = 6
        GO TO 430
      END IF
      NFE = NFE + 1
      IF (EVALJC .OR. IER) THEN
        CALL DDPST (EL, F, FA, H, IMPL, JACOBN, MATDIM, MITER, ML,              &
                   MU, N, NDE, NQ, SAVE2, T, USERS, Y, YH, YWT, UROUND,       &
                   NFE, NJE,  A, DFDY, FAC, IER, IPVT, SAVE1, ISWFLG,          &
                   BND, JSTATE)
        IF (N .EQ. 0) GO TO 430
        IF (IER) GO TO 160
        CONVRG = .FALSE.
        RC = 1.D0
      END IF
      DO 125 I = 1,N
 125    SAVE1(I) = 0.D0
!C                      Up to MXITER corrector iterations are taken.
!C                      Convergence is tested by requiring the r.m.s.
!C                      norm of changes to be less than EPS.  The sum of
!C                      the corrections is accumulated in the vector
!C                      SAVE1(I).  It is approximately equal to the L-th
!C                      derivative of Y multiplied by
!C                      H**L/(factorial(L-1)*EL(L,NQ)), and is thus
!C                      proportional to the actual errors to the lowest
!C                      power of H present (H**L).  The YH array is not
!C                      altered in the correction loop.  The norm of the
!C                      iterate difference is stored in D.  If
!C                      ITER .GT. 0, an estimate of the convergence rate
!C                      constant is stored in TREND, and this is used in
!C                      the convergence test.
!C
 130  CALL DDCOR (DFDY, EL, FA, H, IMPL, IPVT, MATDIM, MITER, ML,        &
                 MU, N, NDE, NQ, T, USERS, Y, YH, YWT,  EVALFA, SAVE1,  &
                 SAVE2,  A, D, JSTATE)
        IF (N .EQ. 0) GO TO 430
      IF (ISWFLG .EQ. 3 .AND. MINT .EQ. 1) THEN
        IF (ITER .EQ. 0) THEN
          NUMER = DNRM2(N, SAVE1, 1)
          DO 132 I = 1,N
 132        DFDY(1,I) = SAVE1(I)
          Y0NRM = DNRM2(N, YH, 1)
        ELSE
          DENOM = NUMER
          DO 134 I = 1,N
 134        DFDY(1,I) = SAVE1(I) - DFDY(1,I)
          NUMER = DNRM2(N, DFDY, MATDIM)
          IF (EL(1,NQ)*NUMER .LE. 100.D0*UROUND*Y0NRM) THEN
            IF (RMAX .EQ. RMFAIL) THEN
              SWITCH = .TRUE.
              GO TO 170
            END IF
          END IF
          DO 136 I = 1,N
 136        DFDY(1,I) = SAVE1(I)
          IF (DENOM .NE. 0.D0)  &
         BND = MAX(BND, NUMER/(DENOM*ABS(H)*EL(1,NQ)))
        END IF
      END IF
      IF (ITER .GT. 0) TREND = MAX(.9D0*TREND, D/D1)
      D1 = D
      CTEST = MIN(2.D0*TREND, 1.D0)*D
      IF (CTEST .LE. EPS) GO TO 170
      ITER = ITER + 1
      IF (ITER .LT. MXITER) THEN
        DO 140 I = 1,N
 140      Y(I) = YH(I,1) + EL(1,NQ)*SAVE1(I)
        CALL F (N, T, Y, SAVE2)
        IF (N .EQ. 0) THEN
          JSTATE = 6
          GO TO 430
        END IF
        NFE = NFE + 1
        GO TO 130
      END IF
!C                     The corrector iteration failed to converge in
!C                     MXITER tries.  If partials are involved but are
!C                     not up to date, they are reevaluated for the next
!C                     try.  Otherwise the YH array is retracted to its
!C                     values before prediction, and H is reduced, if
!C                     possible.  If not, a no-convergence exit is taken.
      IF (CONVRG) THEN
        EVALJC = .TRUE.
        EVALFA = .FALSE.
        GO TO 110
      END IF
 160  T = TOLD
      CALL DDPSC (-1, N, NQ,  YH)
      NWAIT = NQ + 2
      IF (JTASK .NE. 0 .AND. JTASK .NE. 2) RMAX = RMFAIL
      IF (ITER .EQ. 0) THEN
        RH = .3D0
      ELSE
        RH = .9D0*(EPS/CTEST)**(.2D0)
      END IF
      IF (RH*H .EQ. 0.D0) GO TO 400
      CALL DDSCL (HMAX, N, NQ, RMAX,  H, RC, RH, YH)
      GO TO 100
!C                          The corrector has converged.  CONVRG is set
!C                          to .TRUE. if partial derivatives were used,
!C                          to indicate that they may need updating on
!C                          subsequent steps.  The error test is made.
 170  CONVRG = (MITER .NE. 0)
      DO 180 I = 1,NDE
 180    SAVE2(I) = SAVE1(I)/YWT(I)
      ETEST = DNRM2(NDE, SAVE2, 1)/(TQ(2,NQ)*SQRT(DBLE(NDE)))
!C
!C                           The error test failed.  NFAIL keeps track of
!C                           multiple failures.  Restore T and the YH
!C                           array to their previous values, and prepare
!C                           to try the step again.  Compute the optimum
!C                           step size for this or one lower order.
      IF (ETEST .GT. EPS) THEN
        T = TOLD
        CALL DDPSC (-1, N, NQ,  YH)
        NFAIL = NFAIL + 1
        IF (NFAIL .LT. MXFAIL) THEN
          IF (JTASK .NE. 0 .AND. JTASK .NE. 2) RMAX = RMFAIL
          RH2 = 1.D0/(BIAS2*(ETEST/EPS)**(1.D0/DBLE(NQ+1)))
          IF (NQ .GT. 1) THEN
            DO 190 I = 1,NDE
 190          SAVE2(I) = YH(I,NQ+1)/YWT(I)
            ERDN = DNRM2(NDE, SAVE2, 1)/(TQ(1,NQ)*SQRT(DBLE(NDE)))
            RH1 = 1.D0/MAX(1.D0, BIAS1*(ERDN/EPS)**(1.D0/DBLE(NQ)))
            IF (RH2 .LT. RH1) THEN
              NQ = NQ - 1
              RC = RC*EL(1,NQ)/EL(1,NQ+1)
              RH = RH1
            ELSE
              RH = RH2
            END IF
          ELSE
            RH = RH2
          END IF
          NWAIT = NQ + 2
          IF (RH*H .EQ. 0.D0) GO TO 400
          CALL DDSCL (HMAX, N, NQ, RMAX,  H, RC, RH, YH)
          GO TO 100
        END IF
!C                Control reaches this section if the error test has
!C                failed MXFAIL or more times.  It is assumed that the
!C                derivatives that have accumulated in the YH array have
!C                errors of the wrong order.  Hence the first derivative
!C                is recomputed, the order is set to 1, and the step is
!C                retried.
        NFAIL = 0
        JTASK = 2
        DO 215 I = 1,N
 215      Y(I) = YH(I,1)
        CALL DDNTL (EPS, F, FA, HMAX, HOLD, IMPL, JTASK, MATDIM,          &
                   MAXORD, MINT, MITER, ML, MU, N, NDE, SAVE1, T,        &
                   UROUND, USERS, Y, YWT,  H, MNTOLD, MTROLD, NFE, RC,   &
                   YH,  A, CONVRG, EL, FAC, IER, IPVT, NQ, NWAIT, RH,    &
                   RMAX, SAVE2, TQ, TREND, ISWFLG, JSTATE)
        RMAX = RMNORM
        IF (N .EQ. 0) GO TO 440
        IF (H .EQ. 0.D0) GO TO 400
        IF (IER) GO TO 420
        GO TO 100
      END IF
!C                          After a successful step, update the YH array.
      NSTEP = NSTEP + 1
      HUSED = H
      NQUSED = NQ
      AVGH = (DBLE(NSTEP-1)*AVGH + H)/DBLE(NSTEP)
      AVGORD = (DBLE(NSTEP-1)*AVGORD + DBLE(NQ))/DBLE(NSTEP)
      DO 230 J = 1,NQ+1
        DO 230 I = 1,N
 230      YH(I,J) = YH(I,J) + EL(J,NQ)*SAVE1(I)
      DO 235 I = 1,N
 235    Y(I) = YH(I,1)
!C                                          If ISWFLG is 3, consider
!C                                          changing integration methods.
!C
      IF (ISWFLG .EQ. 3) THEN
        IF (BND .NE. 0.D0) THEN
          IF (MINT .EQ. 1 .AND. NQ .LE. 5) THEN
            HN = ABS(H)/MAX(UROUND, (ETEST/EPS)**(1.D0/DBLE(NQ+1)))
            HN = MIN(HN, 1.D0/(2.D0*EL(1,NQ)*BND))
            HS = ABS(H)/MAX(UROUND,  &
           (ETEST/(EPS*EL(NQ+1,1)))**(1.D0/DBLE(NQ+1)))
            IF (HS .GT. 1.2D0*HN) THEN
              MINT = 2
              MNTOLD = MINT
              MITER = MTRSV
              MTROLD = MITER
              MAXORD = MIN(MXRDSV, 5)
              RC = 0.D0
              RMAX = RMNORM
              TREND = 1.D0
              CALL DDCST (MAXORD, MINT, ISWFLG, EL, TQ)
              NWAIT = NQ + 2
            END IF
          ELSE IF (MINT .EQ. 2) THEN
            HS = ABS(H)/MAX(UROUND, (ETEST/EPS)**(1.D0/DBLE(NQ+1)))
            HN = ABS(H)/MAX(UROUND, &
           (ETEST*EL(NQ+1,1)/EPS)**(1.D0/DBLE(NQ+1)))
            HN = MIN(HN, 1.D0/(2.D0*EL(1,NQ)*BND))
            IF (HN .GE. HS) THEN
              MINT = 1
              MNTOLD = MINT
              MITER = 0
              MTROLD = MITER
              MAXORD = MIN(MXRDSV, 12)
              RMAX = RMNORM
              TREND = 1.D0
              CONVRG = .FALSE.
              CALL DDCST (MAXORD, MINT, ISWFLG, EL, TQ)
              NWAIT = NQ + 2
            END IF
          END IF
        END IF
      END IF
      IF (SWITCH) THEN
        MINT = 2
        MNTOLD = MINT
        MITER = MTRSV
        MTROLD = MITER
        MAXORD = MIN(MXRDSV, 5)
        NQ = MIN(NQ, MAXORD)
        RC = 0.D0
        RMAX = RMNORM
        TREND = 1.D0
        CALL DDCST (MAXORD, MINT, ISWFLG, EL, TQ)
        NWAIT = NQ + 2
      END IF
!C                           Consider changing H if NWAIT = 1.  Otherwise
!C                           decrease NWAIT by 1.  If NWAIT is then 1 and
!C                           NQ.LT.MAXORD, then SAVE1 is saved for use in
!C                           a possible order increase on the next step.
!C
      IF (JTASK .EQ. 0 .OR. JTASK .EQ. 2) THEN
        RH = 1.D0/MAX(UROUND, BIAS2*(ETEST/EPS)**(1.D0/DBLE(NQ+1)))
        IF (RH.GT.TRSHLD) CALL DDSCL (HMAX, N, NQ, RMAX, H, RC, RH, YH)
      ELSE IF (NWAIT .GT. 1) THEN
        NWAIT = NWAIT - 1
        IF (NWAIT .EQ. 1 .AND. NQ .LT. MAXORD) THEN
          DO 250 I = 1,NDE
 250        YH(I,MAXORD+1) = SAVE1(I)
        END IF
!C             If a change in H is considered, an increase or decrease in
!C             order by one is considered also.  A change in H is made
!C             only if it is by a factor of at least TRSHLD.  Factors
!C             RH1, RH2, and RH3 are computed, by which H could be
!C             multiplied at order NQ - 1, order NQ, or order NQ + 1,
!C             respectively.  The largest of these is determined and the
!C             new order chosen accordingly.  If the order is to be
!C             increased, we compute one additional scaled derivative.
!C             If there is a change of order, reset NQ and the
!C             coefficients.  In any case H is reset according to RH and
!C             the YH array is rescaled.
      ELSE
        IF (NQ .EQ. 1) THEN
          RH1 = 0.D0
        ELSE
          DO 270 I = 1,NDE
 270        SAVE2(I) = YH(I,NQ+1)/YWT(I)
          ERDN = DNRM2(NDE, SAVE2, 1)/(TQ(1,NQ)*SQRT(DBLE(NDE)))
          RH1 = 1.D0/MAX(UROUND, BIAS1*(ERDN/EPS)**(1.D0/DBLE(NQ)))
        END IF
        RH2 = 1.D0/MAX(UROUND, BIAS2*(ETEST/EPS)**(1.D0/DBLE(NQ+1)))
        IF (NQ .EQ. MAXORD) THEN
          RH3 = 0.D0
        ELSE
          DO 290 I = 1,NDE
 290        SAVE2(I) = (SAVE1(I) - YH(I,MAXORD+1))/YWT(I)
          ERUP = DNRM2(NDE, SAVE2, 1)/(TQ(3,NQ)*SQRT(DBLE(NDE)))
          RH3 = 1.D0/MAX(UROUND, BIAS3*(ERUP/EPS)**(1.D0/DBLE(NQ+2)))
        END IF
        IF (RH1 .GT. RH2 .AND. RH1 .GE. RH3) THEN
          RH = RH1
          IF (RH .LE. TRSHLD) GO TO 380
          NQ = NQ - 1
          RC = RC*EL(1,NQ)/EL(1,NQ+1)
        ELSE IF (RH2 .GE. RH1 .AND. RH2 .GE. RH3) THEN
          RH = RH2
          IF (RH .LE. TRSHLD) GO TO 380
        ELSE
          RH = RH3
          IF (RH .LE. TRSHLD) GO TO 380
          DO 360 I = 1,N
 360        YH(I,NQ+2) = SAVE1(I)*EL(NQ+1,NQ)/DBLE(NQ+1)
          NQ = NQ + 1
          RC = RC*EL(1,NQ)/EL(1,NQ-1)
        END IF
        IF (ISWFLG .EQ. 3 .AND. MINT .EQ. 1) THEN
          IF (BND.NE.0.D0) RH = MIN(RH, 1.D0/(2.D0*EL(1,NQ)*BND*ABS(H)))
        END IF
        CALL DDSCL (HMAX, N, NQ, RMAX,  H, RC, RH, YH)
        RMAX = RMNORM
 380    NWAIT = NQ + 2
      END IF
!C               All returns are made through this section.  H is saved
!C               in HOLD to allow the caller to change H on the next step
      JSTATE = 1
      HOLD = H
      RETURN
!C
 400  JSTATE = 2
      HOLD = H
      DO 405 I = 1,N
 405    Y(I) = YH(I,1)
      RETURN
!C
 410  JSTATE = 3
      HOLD = H
      RETURN
!C
 420  JSTATE = 4
      HOLD = H
      RETURN
!C
 430  T = TOLD
      CALL DDPSC (-1, NSV, NQ,  YH)
      DO 435 I = 1,NSV
 435    Y(I) = YH(I,1)
 440  HOLD = H
      RETURN
   END SUBROUTINE
   
      SUBROUTINE DDNTL (EPS,F,FA,HMAX,HOLD,IMPL,JTASK,MATDIM,MAXORD,   &
     8   MINT,MITER,ML,MU,N,NDE,SAVE1,T,UROUND,USERS,Y,YWT,H,MNTOLD,   &
     8   MTROLD,NFE,RC,YH,A,CONVRG,EL,FAC,IER,IPVT,NQ,NWAIT,RH,RMAX,   &
     8   SAVE2,TQ,TREND,ISWFLG,JSTATE)
!C***BEGIN PROLOGUE  DDNTL
!C   THIS PROLOGUE HAS BEEN OMITTED FOR REASONS OF SPACE
!C   FOR A COMPLETE COPY OF THIS SUBROUTINE CONTACT THE AUTHORS
!C    From the book "Numerical Methods and Software"
!C       by D. Kahaner, C. Moler, S. Nash
!C          Prentice Hall 1988
!C***ROUTINES CALLED  DDCST,DDSCL,DGEFA,DGESL,DGBFA,DGBSL,DNRM2
!C***END PROLOGUE  DDNTL
      DOUBLE PRECISION A(MATDIM,*), EL(13,12), EPS, FAC(*), H, HMAX,      &
          HOLD, OLDL0, RC, RH, RMAX, RMINIT, SAVE1(*), SAVE2(*), SMAX,   &
          SMIN, DNRM2, SUM, SUM0, T, TQ(3,12), TREND, UROUND, Y(*),      &
          YH(N,*), YWT(*)
      INTEGER IPVT(*)
      LOGICAL CONVRG, IER
      PARAMETER(RMINIT = 10000.D0)
!C***FIRST EXECUTABLE STATEMENT  DDNTL
      IER = .FALSE.
      IF (JTASK .GE. 0) THEN
        IF (JTASK .EQ. 0) THEN
          CALL DDCST (MAXORD, MINT, ISWFLG,  EL, TQ)
          RMAX = RMINIT
        END IF
        RC = 0.D0
        CONVRG = .FALSE.
        TREND = 1.D0
        NQ = 1
        NWAIT = 3
        CALL F (N, T, Y, SAVE2)
        IF (N .EQ. 0) THEN
          JSTATE = 6
          RETURN
        END IF
        NFE = NFE + 1
        IF (IMPL .NE. 0) THEN
          IF (MITER .EQ. 3) THEN
            IFLAG = 0
            CALL USERS (Y, YH, YWT, SAVE1, SAVE2, T, H, EL, IMPL, N, &
                       NDE, IFLAG)
            IF (N .EQ. 0) THEN
              JSTATE = 10
              RETURN
            END IF
          ELSE IF (IMPL .EQ. 1) THEN
            IF (MITER .EQ. 1 .OR. MITER .EQ. 2) THEN
              CALL FA (N, T, Y, A, MATDIM, ML, MU, NDE)
              IF (N .EQ. 0) THEN
                JSTATE = 9
                RETURN
              END IF
              CALL DGEFA (A, MATDIM, N, IPVT, INFO)
              IF (INFO .NE. 0) THEN
                IER = .TRUE.
                RETURN
              END IF
              CALL DGESL (A, MATDIM, N, IPVT, SAVE2, 0)
            ELSE IF (MITER .EQ. 4 .OR. MITER .EQ. 5) THEN
              CALL FA (N, T, Y, A(ML+1,1), MATDIM, ML, MU, NDE)
              IF (N .EQ. 0) THEN
                JSTATE = 9
                RETURN
              END IF
              CALL DGBFA (A, MATDIM, N, ML, MU, IPVT, INFO)
              IF (INFO .NE. 0) THEN
                IER = .TRUE.
                RETURN
              END IF
              CALL DGBSL (A, MATDIM, N, ML, MU, IPVT, SAVE2, 0)
            END IF
          ELSE IF (IMPL .EQ. 2) THEN
            CALL FA (N, T, Y, A, MATDIM, ML, MU, NDE)
            IF (N .EQ. 0) THEN
              JSTATE = 9
              RETURN
            END IF
            DO 150 I = 1,NDE
              IF (A(I,1) .EQ. 0.D0) THEN
                IER = .TRUE.
                RETURN
              ELSE
                SAVE2(I) = SAVE2(I)/A(I,1)
              END IF
 150          CONTINUE
            DO 155 I = NDE+1,N
 155          A(I,1) = 0.D0
          END IF
        END IF
        DO 170 I = 1,NDE
 170      SAVE1(I) = SAVE2(I)/YWT(I)
        SUM = DNRM2(NDE, SAVE1, 1)
        SUM0 = 1.D0/MAX(1.D0, ABS(T))
        SMAX = MAX(SUM0, SUM)
        SMIN = MIN(SUM0, SUM)
        SUM = SMAX*SQRT(1.D0 + (SMIN/SMAX)**2)/SQRT(DBLE(NDE))
        H = SIGN(MIN(2.D0*EPS/SUM, ABS(H)), H)
        DO 180 I = 1,N
 180      YH(I,2) = H*SAVE2(I)
        IF (MITER .EQ. 2 .OR. MITER .EQ. 5 .OR. ISWFLG .EQ. 3) THEN
          DO 20 I = 1,N
 20         FAC(I) = SQRT(UROUND)
        END IF
      ELSE
        IF (MITER .NE. MTROLD) THEN
          MTROLD = MITER
          RC = 0.D0
          CONVRG = .FALSE.
        END IF
        IF (MINT .NE. MNTOLD) THEN
          MNTOLD = MINT
          OLDL0 = EL(1,NQ)
          CALL DDCST (MAXORD, MINT, ISWFLG,  EL, TQ)
          RC = RC*EL(1,NQ)/OLDL0
          NWAIT = NQ + 2
        END IF
        IF (H .NE. HOLD) THEN
          NWAIT = NQ + 2
          RH = H/HOLD
          CALL DDSCL (HMAX, N, NQ, RMAX,  HOLD, RC, RH, YH)
        END IF
      END IF
   END SUBROUTINE
   
      SUBROUTINE DDPST (EL,F,FA,H,IMPL,JACOBN,MATDIM,MITER,ML,MU,N,NDE,   &
        NQ,SAVE2,T,USERS,Y,YH,YWT,UROUND,NFE,NJE,A,DFDY,FAC,IER,IPVT,    &
        SAVE1,ISWFLG,BND,JSTATE)
!C***BEGIN PROLOGUE  DDPST
!C***REFER TO  DDRIV3
!C  Subroutine DDPST is called to reevaluate the partials.
!C  If MITER is 1, 2, 4, or 5, the matrix
!C  P = I - L(0)*H*Jacobian is stored in DFDY and subjected to LU
!C  decomposition, with the results also stored in DFDY.
!C***ROUTINES CALLED  DGEFA,DGBFA,DNRM2
!C***DATE WRITTEN   790601   (YYMMDD)
!C***REVISION DATE  870401   (YYMMDD)
!C***CATEGORY NO.  I1A2,I1A1B
!C***AUTHOR  KAHANER, D. K., NATIONAL BUREAU OF STANDARDS,
!C           SUTHERLAND, C. D., LOS ALAMOS NATIONAL LABORATORY
!C***END PROLOGUE  DDPST
      DOUBLE PRECISION A(MATDIM,*), BL, BND, BP, BR, BU, DFDY(MATDIM,*),  & 
          DFDYMX, DIFF, DY, EL(13,12), FAC(*), FACMAX, FACMIN, FACTOR,   &
          H, SAVE1(*), SAVE2(*), SCALE, DNRM2, T, UROUND, Y(*),          &
          YH(N,*), YJ, YS, YWT(*)
      INTEGER IPVT(*)
      LOGICAL IER
      PARAMETER(FACMAX = .5D0)
!C***FIRST EXECUTABLE STATEMENT  DDPST
      NJE = NJE + 1
      IER = .FALSE.
      IF (MITER .EQ. 1 .OR. MITER .EQ. 2) THEN
        IF (MITER .EQ. 1) THEN
          CALL JACOBN (N, T, Y, DFDY, MATDIM, ML, MU)
          IF (N .EQ. 0) THEN
            JSTATE = 8
            RETURN
          END IF
          IF (ISWFLG .EQ. 3) BND = DNRM2(N*N, DFDY, 1)
          FACTOR = -EL(1,NQ)*H
          DO 110 J = 1,N
            DO 110 I = 1,N
 110          DFDY(I,J) = FACTOR*DFDY(I,J)
        ELSE IF (MITER .EQ. 2) THEN
          BR = UROUND**(.875D0)
          BL = UROUND**(.75D0)
          BU = UROUND**(.25D0)
          BP = UROUND**(-.15D0)
          FACMIN = UROUND**(.78D0)
          DO 170 J = 1,N
            YS = MAX(ABS(YWT(J)), ABS(Y(J)))
 120        DY = FAC(J)*YS
            IF (DY .EQ. 0.D0) THEN
              IF (FAC(J) .LT. FACMAX) THEN
                FAC(J) = MIN(100.D0*FAC(J), FACMAX)
                GO TO 120
              ELSE
                DY = YS
              END IF
            END IF
            IF (NQ .EQ. 1) THEN
              DY = SIGN(DY, SAVE2(J))
            ELSE
              DY = SIGN(DY, YH(J,3))
            END IF
            DY = (Y(J) + DY) - Y(J)
            YJ = Y(J)
            Y(J) = Y(J) + DY
            CALL F (N, T, Y, SAVE1)
            IF (N .EQ. 0) THEN
              JSTATE = 6
              RETURN
            END IF
            Y(J) = YJ
            FACTOR = -EL(1,NQ)*H/DY
            DO 140 I = 1,N
 140          DFDY(I,J) = (SAVE1(I) - SAVE2(I))*FACTOR
!C                                                                 Step 1
            DIFF = ABS(SAVE2(1) - SAVE1(1))
            IMAX = 1
            DO 150 I = 2,N
              IF (ABS(SAVE2(I) - SAVE1(I)) .GT. DIFF) THEN
                IMAX = I
                DIFF = ABS(SAVE2(I) - SAVE1(I))
              END IF
 150          CONTINUE
!C                                                                 Step 2
            IF (MIN(ABS(SAVE2(IMAX)), ABS(SAVE1(IMAX))) .GT. 0.D0) THEN
              SCALE = MAX(ABS(SAVE2(IMAX)), ABS(SAVE1(IMAX)))
!C                                                                 Step 3
              IF (DIFF .GT. BU*SCALE) THEN
                FAC(J) = MAX(FACMIN, FAC(J)*.1D0)
              ELSE IF (BR*SCALE .LE. DIFF .AND. DIFF .LE. BL*SCALE) THEN
                FAC(J) = MIN(FAC(J)*10.D0, FACMAX)
!C                                                                 Step 4
              ELSE IF (DIFF .LT. BR*SCALE) THEN
                FAC(J) = MIN(BP*FAC(J), FACMAX)
              END IF
            END IF
 170        CONTINUE
          IF (ISWFLG .EQ. 3) BND = DNRM2(N*N, DFDY, 1)/(-EL(1,NQ)*H)
          NFE = NFE + N
        END IF
        IF (IMPL .EQ. 0) THEN
          DO 190 I = 1,N
 190        DFDY(I,I) = DFDY(I,I) + 1.D0
        ELSE IF (IMPL .EQ. 1) THEN
          CALL FA (N, T, Y, A, MATDIM, ML, MU, NDE)
          IF (N .EQ. 0) THEN
            JSTATE = 9
            RETURN
          END IF
          DO 210 J = 1,N
            DO 210 I = 1,N
 210          DFDY(I,J) = DFDY(I,J) + A(I,J)
        ELSE IF (IMPL .EQ. 2) THEN
          CALL FA (N, T, Y, A, MATDIM, ML, MU, NDE)
          IF (N .EQ. 0) THEN
            JSTATE = 9
            RETURN
          END IF
          DO 230 I = 1,NDE
 230        DFDY(I,I) = DFDY(I,I) + A(I,1)
        END IF
        CALL DGEFA (DFDY, MATDIM, N, IPVT, INFO)
        IF (INFO .NE. 0) IER = .TRUE.
      ELSE IF (MITER .EQ. 4 .OR. MITER .EQ. 5) THEN
        IF (MITER .EQ. 4) THEN
          CALL JACOBN (N, T, Y, DFDY(ML+1,1), MATDIM, ML, MU)
          IF (N .EQ. 0) THEN
            JSTATE = 8
            RETURN
          END IF
          FACTOR = -EL(1,NQ)*H
          MW = ML + MU + 1
          DO 260 J = 1,N
            I1 = MAX(ML+1, MW+1-J)
            I2 = MIN(MW+N-J, MW+ML)
            DO 260 I = I1,I2
 260          DFDY(I,J) = FACTOR*DFDY(I,J)
        ELSE IF (MITER .EQ. 5) THEN
          BR = UROUND**(.875D0)
          BL = UROUND**(.75D0)
          BU = UROUND**(.25D0)
          BP = UROUND**(-.15D0)
          FACMIN = UROUND**(.78D0)
          MW = ML + MU + 1
          J2 = MIN(MW, N)
          DO 340 J = 1,J2
            DO 290 K = J,N,MW
              YS = MAX(ABS(YWT(K)), ABS(Y(K)))
 280          DY = FAC(K)*YS
              IF (DY .EQ. 0.D0) THEN
                IF (FAC(K) .LT. FACMAX) THEN
                  FAC(K) = MIN(100.D0*FAC(K), FACMAX)
                  GO TO 280
                ELSE
                  DY = YS
                END IF
              END IF
              IF (NQ .EQ. 1) THEN
                DY = SIGN(DY, SAVE2(K))
              ELSE
                DY = SIGN(DY, YH(K,3))
              END IF
              DY = (Y(K) + DY) - Y(K)
              DFDY(MW,K) = Y(K)
 290          Y(K) = Y(K) + DY
            CALL F (N, T, Y, SAVE1)
            IF (N .EQ. 0) THEN
              JSTATE = 6
              RETURN
            END IF
            DO 330 K = J,N,MW
              Y(K) = DFDY(MW,K)
              YS = MAX(ABS(YWT(K)), ABS(Y(K)))
              DY = FAC(K)*YS
              IF (DY .EQ. 0.D0) DY = YS
              IF (NQ .EQ. 1) THEN
                DY = SIGN(DY, SAVE2(K))
              ELSE
                DY = SIGN(DY, YH(K,3))
              END IF
              DY = (Y(K) + DY) - Y(K)
              FACTOR = -EL(1,NQ)*H/DY
              I1 = MAX(ML+1, MW+1-K)
              I2 = MIN(MW+N-K, MW+ML)
              DO 300 I = I1,I2
                I3 = K + I - MW
 300            DFDY(I,K) = FACTOR*(SAVE1(I3) - SAVE2(I3))
!C                                                                 Step 1
              IMAX = MAX(1, K - MU)
              DIFF = ABS(SAVE2(IMAX) - SAVE1(IMAX))
              I1 = IMAX
              I2 = MIN(K + ML, N)
              DO 310 I = I1+1,I2
                IF (ABS(SAVE2(I) - SAVE1(I)) .GT. DIFF) THEN
                  IMAX = I
                  DIFF = ABS(SAVE2(I) - SAVE1(I))
                END IF
 310            CONTINUE
!C                                                                 Step 2
              IF (MIN(ABS(SAVE2(IMAX)), ABS(SAVE1(IMAX))) .GT.0.D0) THEN
                SCALE = MAX(ABS(SAVE2(IMAX)), ABS(SAVE1(IMAX)))
!C                                                                 Step 3
                IF (DIFF .GT. BU*SCALE) THEN
                  FAC(K) = MAX(FACMIN, FAC(K)*.1D0)
                ELSE IF (BR*SCALE .LE.DIFF .AND. DIFF .LE.BL*SCALE) THEN
                  FAC(K) = MIN(FAC(K)*10.D0, FACMAX)
!C                                                                 Step 4
                ELSE IF (DIFF .LT. BR*SCALE) THEN
                  FAC(K) = MIN(BP*FAC(K), FACMAX)
                END IF
              END IF
 330          CONTINUE
 340        CONTINUE
          NFE = NFE + J2
        END IF
        IF (ISWFLG .EQ. 3) THEN
          DFDYMX = 0.D0
          DO 345 J = 1,N
            I1 = MAX(ML+1, MW+1-J)
            I2 = MIN(MW+N-J, MW+ML)
            DO 345 I = I1,I2
 345          DFDYMX = MAX(DFDYMX, ABS(DFDY(I,J)))
          BND = 0.D0
          IF (DFDYMX .NE. 0.D0) THEN
            DO 350 J = 1,N
              I1 = MAX(ML+1, MW+1-J)
              I2 = MIN(MW+N-J, MW+ML)
              DO 350 I = I1,I2
 350            BND = BND + (DFDY(I,J)/DFDYMX)**2
            BND = DFDYMX*SQRT(BND)/(-EL(1,NQ)*H)
          END IF
        END IF
        IF (IMPL .EQ. 0) THEN
          DO 360 J = 1,N
 360        DFDY(MW,J) = DFDY(MW,J) + 1.D0
        ELSE IF (IMPL .EQ. 1) THEN
          CALL FA (N, T, Y, A(ML+1,1), MATDIM, ML, MU, NDE)
          IF (N .EQ. 0) THEN
            JSTATE = 9
            RETURN
          END IF
          DO 380 J = 1,N
            I1 = MAX(ML+1, MW+1-J)
            I2 = MIN(MW+N-J, MW+ML)
            DO 380 I = I1,I2
 380          DFDY(I,J) = DFDY(I,J) + A(I,J)
        ELSE IF (IMPL .EQ. 2) THEN
          CALL FA (N, T, Y, A, MATDIM, ML, MU, NDE)
          IF (N .EQ. 0) THEN
            JSTATE = 9
            RETURN
          END IF
          DO 400 J = 1,NDE
 400        DFDY(MW,J) =  DFDY(MW,J) + A(J,1)
        END IF
        CALL DGBFA (DFDY, MATDIM, N, ML, MU, IPVT, INFO)
        IF (INFO .NE. 0) IER = .TRUE.
      ELSE IF (MITER .EQ. 3) THEN
        IFLAG = 1
        CALL USERS (Y, YH(1,2), YWT, SAVE1, SAVE2, T, H, EL(1,NQ), IMPL,
     8              N, NDE, IFLAG)
        IF (N .EQ. 0) THEN
          JSTATE = 10
          RETURN
        END IF
      END IF
    END SUBROUTINE
    
      SUBROUTINE DDCOR (DFDY,EL,FA,H,IMPL,IPVT,MATDIM,MITER,ML,MU,N,  &
       NDE,NQ,T,USERS,Y,YH,YWT,EVALFA,SAVE1,SAVE2,A,D,JSTATE)
!C***BEGIN PROLOGUE  DDCOR
!C   THIS PROLOGUE HAS BEEN OMITTED FOR REASONS OF SPACE
!C   FOR A COMPLETE COPY OF THIS SUBROUTINE CONTACT THE AUTHORS
!C    From the book "Numerical Methods and Software"
!C       by D. Kahaner, C. Moler, S. Nash
!C          Prentice Hall 1988
!C***ROUTINES CALLED  DGESL,DGBSL,DNRM2
!C***END PROLOGUE  DDCOR
      DOUBLE PRECISION A(MATDIM,*), D, DFDY(MATDIM,*), EL(13,12), H,  &
           SAVE1(*), SAVE2(*), DNRM2, T, Y(*), YH(N,*), YWT(*)
      INTEGER IPVT(*)
      LOGICAL EVALFA
!C***FIRST EXECUTABLE STATEMENT  DDCOR
      IF (MITER .EQ. 0) THEN
        DO 100 I = 1,N
 100      SAVE1(I) = (H*SAVE2(I) - YH(I,2) - SAVE1(I))/YWT(I)
        D = DNRM2(N, SAVE1, 1)/SQRT(DBLE(N))
        DO 105 I = 1,N
 105      SAVE1(I) = H*SAVE2(I) - YH(I,2)
      ELSE IF (MITER .EQ. 1 .OR. MITER .EQ. 2) THEN
        IF (IMPL .EQ. 0) THEN
          DO 130 I = 1,N
 130        SAVE2(I) = H*SAVE2(I) - YH(I,2) - SAVE1(I)
        ELSE IF (IMPL .EQ. 1) THEN
          IF (EVALFA) THEN
            CALL FA (N, T, Y, A, MATDIM, ML, MU, NDE)
            IF (N .EQ. 0) THEN
              JSTATE = 9
              RETURN
            END IF
          ELSE
            EVALFA = .TRUE.
          END IF
          DO 150 I = 1,N
 150        SAVE2(I) = H*SAVE2(I)
          DO 160 J = 1,N
            DO 160 I = 1,N
 160          SAVE2(I) = SAVE2(I) - A(I,J)*(YH(J,2) + SAVE1(J))
        ELSE IF (IMPL .EQ. 2) THEN
          IF (EVALFA) THEN
            CALL FA (N, T, Y, A, MATDIM, ML, MU, NDE)
            IF (N .EQ. 0) THEN
              JSTATE = 9
              RETURN
            END IF
          ELSE
            EVALFA = .TRUE.
          END IF
          DO 180 I = 1,N
 180        SAVE2(I) = H*SAVE2(I) - A(I,1)*(YH(I,2) + SAVE1(I))
        END IF
        CALL DGESL (DFDY, MATDIM, N, IPVT, SAVE2, 0)
        DO 200 I = 1,N
          SAVE1(I) = SAVE1(I) + SAVE2(I)
 200      SAVE2(I) = SAVE2(I)/YWT(I)
        D = DNRM2(N, SAVE2, 1)/SQRT(DBLE(N))
      ELSE IF (MITER .EQ. 4 .OR. MITER .EQ. 5) THEN
        IF (IMPL .EQ. 0) THEN
          DO 230 I = 1,N
 230        SAVE2(I) = H*SAVE2(I) - YH(I,2) - SAVE1(I)
        ELSE IF (IMPL .EQ. 1) THEN
          IF (EVALFA) THEN
            CALL FA (N, T, Y, A(ML+1,1), MATDIM, ML, MU, NDE)
            IF (N .EQ. 0) THEN
              JSTATE = 9
              RETURN
            END IF
          ELSE
            EVALFA = .TRUE.
          END IF
          DO 250 I = 1,N
 250        SAVE2(I) = H*SAVE2(I)
          MW = ML + 1 + MU
          DO 260 J = 1,N
            I1 = MAX(ML+1, MW+1-J)
            I2 = MIN(MW+N-J, MW+ML)
            DO 260 I = I1,I2
              I3 = I + J - MW
 260          SAVE2(I3) = SAVE2(I3) - A(I,J)*(YH(J,2) + SAVE1(J))
        ELSE IF (IMPL .EQ. 2) THEN
          IF (EVALFA) THEN
            CALL FA (N, T, Y, A, MATDIM, ML, MU, NDE)
            IF (N .EQ. 0) THEN
              JSTATE = 9
              RETURN
            END IF
          ELSE
            EVALFA = .TRUE.
          END IF
          DO 280 I = 1,N
 280        SAVE2(I) = H*SAVE2(I) - A(I,1)*(YH(I,2) + SAVE1(I))
        END IF
        CALL DGBSL (DFDY, MATDIM, N, ML, MU, IPVT, SAVE2, 0)
        DO 300 I = 1,N
          SAVE1(I) = SAVE1(I) + SAVE2(I)
 300      SAVE2(I) = SAVE2(I)/YWT(I)
        D = DNRM2(N, SAVE2, 1)/SQRT(DBLE(N))
      ELSE IF (MITER .EQ. 3) THEN
        IFLAG = 2
        CALL USERS (Y, YH(1,2), YWT, SAVE1, SAVE2, T, H, EL(1,NQ), IMPL,
     8              N, NDE, IFLAG)
        IF (N .EQ. 0) THEN
          JSTATE = 10
          RETURN
        END IF
        DO 320 I = 1,N
          SAVE1(I) = SAVE1(I) + SAVE2(I)
 320      SAVE2(I) = SAVE2(I)/YWT(I)
        D = DNRM2(N, SAVE2, 1)/SQRT(DBLE(N))
      END IF
    END SUBROUTINE
    
      SUBROUTINE DDCST (MAXORD,MINT,ISWFLG,EL,TQ)
!C***BEGIN PROLOGUE  DDCST
!C   THIS PROLOGUE HAS BEEN OMITTED FOR REASONS OF SPACE
!C   FOR A COMPLETE COPY OF THIS SUBROUTINE CONTACT THE AUTHORS
!C    From the book "Numerical Methods and Software"
!C       by D. Kahaner, C. Moler, S. Nash
!C          Prentice Hall 1988
!C***ROUTINES CALLED  (NONE)
!C***END PROLOGUE  DDCST
      DOUBLE PRECISION EL(13,12), FACTRL(12), GAMMA(14), SUM, TQ(3,12)
!C***FIRST EXECUTABLE STATEMENT  DDCST
      FACTRL(1) = 1.D0
      DO 10 I = 2,MAXORD
 10     FACTRL(I) = DBLE(I)*FACTRL(I-1)
!C                                             COMPUTE ADAMS COEFFICIENTS
      IF (MINT .EQ. 1) THEN
        GAMMA(1) = 1.D0
        DO 40 I = 1,MAXORD+1
          SUM = 0.D0
          DO 30 J = 1,I
 30         SUM = SUM - GAMMA(J)/DBLE(I-J+2)
 40       GAMMA(I+1) = SUM
        EL(1,1) = 1.D0
        EL(2,1) = 1.D0
        EL(2,2) = 1.D0
        EL(3,2) = 1.D0
        DO 60 J = 3,MAXORD
          EL(2,J) = FACTRL(J-1)
          DO 50 I = 3,J
 50         EL(I,J) = DBLE(J-1)*EL(I,J-1) + EL(I-1,J-1)
 60       EL(J+1,J) = 1.D0
        DO 80 J = 2,MAXORD
          EL(1,J) = EL(1,J-1) + GAMMA(J)
          EL(2,J) = 1.D0
          DO 80 I = 3,J+1
 80         EL(I,J) = EL(I,J)/(DBLE(I-1)*FACTRL(J-1))
        DO 100 J = 1,MAXORD
          TQ(1,J) = -1.D0/(FACTRL(J)*GAMMA(J))
          TQ(2,J) = -1.D0/GAMMA(J+1)
 100      TQ(3,J) = -1.D0/GAMMA(J+2)
!C                                              COMPUTE GEAR COEFFICIENTS
      ELSE IF (MINT .EQ. 2) THEN
        EL(1,1) = 1.D0
        EL(2,1) = 1.D0
        DO 130 J = 2,MAXORD
          EL(1,J) = FACTRL(J)
          DO 120 I = 2,J
 120        EL(I,J) = DBLE(J)*EL(I,J-1) + EL(I-1,J-1)
 130      EL(J+1,J) = 1.D0
        SUM = 1.D0
        DO 150 J = 2,MAXORD
          SUM = SUM + 1.D0/DBLE(J)
          DO 150 I = 1,J+1
 150        EL(I,J) = EL(I,J)/(FACTRL(J)*SUM)
        DO 170 J = 1,MAXORD
          IF (J .GT. 1) TQ(1,J) = 1.D0/FACTRL(J-1)
          TQ(2,J) = DBLE(J+1)/EL(1,J)
 170      TQ(3,J) = DBLE(J+2)/EL(1,J)
      END IF
!C                          Compute constants used in the stiffness test.
!C                          These are the ratio of TQ(2,NQ) for the Gear
!C                          methods to those for the Adams methods.
      IF (ISWFLG .EQ. 3) THEN
        MXRD = MIN(MAXORD, 5)
        IF (MINT .EQ. 2) THEN
          GAMMA(1) = 1.D0
          DO 190 I = 1,MXRD
            SUM = 0.D0
            DO 180 J = 1,I
 180          SUM = SUM - GAMMA(J)/DBLE(I-J+2)
 190        GAMMA(I+1) = SUM
        END IF
        SUM = 1.D0
        DO 200 I = 2,MXRD
          SUM = SUM + 1.D0/DBLE(I)
 200      EL(1+I,1) = -DBLE(I+1)*SUM*GAMMA(I+1)
      END IF
    END SUBROUTINE
    
      SUBROUTINE DDSCL (HMAX,N,NQ,RMAX,H,RC,RH,YH)
!C***BEGIN PROLOGUE  DDSCL
!C   THIS PROLOGUE HAS BEEN OMITTED FOR REASONS OF SPACE
!C   FOR A COMPLETE COPY OF THIS SUBROUTINE CONTACT THE AUTHORS
!C    From the book "Numerical Methods and Software"
!C       by D. Kahaner, C. Moler, S. Nash
!C          Prentice Hall 1988
!C***END PROLOGUE  DDSCL
      DOUBLE PRECISION H, HMAX, RC, RH, RMAX, R1, YH(N,*)
!C***FIRST EXECUTABLE STATEMENT  DDSCL
      IF (H .LT. 1.D0) THEN
        RH = MIN(ABS(H)*RH, ABS(H)*RMAX, HMAX)/ABS(H)
      ELSE
        RH = MIN(RH, RMAX, HMAX/ABS(H))
      END IF
      R1 = 1.D0
      DO 10 J = 1,NQ
        R1 = R1*RH
        DO 10 I = 1,N
 10       YH(I,J+1) = YH(I,J+1)*R1
      H = H*RH
      RC = RC*RH
    END SUBROUTINE
    
    SUBROUTINE DDPSC (KSGN,N,NQ,YH)
      use omp_lib
!C***BEGIN PROLOGUE  DDPSC
!C   THIS PROLOGUE HAS BEEN OMITTED FOR REASONS OF SPACE
!C   FOR A COMPLETE COPY OF THIS SUBROUTINE CONTACT THE AUTHORS
!C    From the book "Numerical Methods and Software"
!C       by D. Kahaner, C. Moler, S. Nash
!C          Prentice Hall 1988
!!C***END PROLOGUE  DDPSC
      DOUBLE PRECISION YH(N,*)
!C***FIRST EXECUTABLE STATEMENT  DDPSC
      IF (KSGN .GT. 0) THEN
         !dir$ assume_aligned YH:64
        DO 10 J1 = 1,NQ
          DO 10 J2 = J1,NQ
             J = NQ - J2 + J1
             !dir$ vector aligned
             !dir$ ivdep
             !$omp simd reduction(+:YH)
            DO 10 I = 1,N
 10           YH(I,J) = YH(I,J) + YH(I,J+1)
      ELSE
               !dir$ assume_aligned YH:64
        DO 30 J1 = 1,NQ
          DO 30 J2 = J1,NQ
             J = NQ - J2 + J1
              !dir$ vector aligned
             !dir$ ivdep
             !$omp simd reduction(+:YH)
            DO 30 I = 1,N
 30           YH(I,J) = YH(I,J) - YH(I,J+1)
      END IF
    END SUBROUTINE
    
      subroutine f (n, t, y, yp)
      double precision alfa,t,y(*),yp(*)
      common /const/ alfa, impl, miter
      save
      data istart /0/
      if (istart.eq.0) then
        istart = 1
        n = 0
        return
      end if
      yp(1) = 1.d0 + alfa*(y(2) - y(1)) - y(1)*y(3)
      yp(2) = alfa*(y(1) - y(2)) - y(2)*y(3)
      if (impl.eq.0 .or. impl.eq.1) then
        yp(3) = 1.d0 - y(3)*(y(1) + y(2))
      else if (impl.eq.2) then
        yp(3) = y(1) + y(2) - y(3)
      end if
    end subroutine f
    
      SUBROUTINE DGBFA(ABD,LDA,N,ML,MU,IPVT,INFO)
!C***BEGIN PROLOGUE  DGBFA
!C   THIS PROLOGUE HAS BEEN OMITTED FOR REASONS OF SPACE
!C   FOR A COMPLETE COPY OF THIS SUBROUTINE CONTACT THE AUTHORS
!C   From the book "Numerical Methods and Software"
!C      by D. Kahaner, C. Moler, S. Nash
!C         Prentice Hall 1988
!C***END PROLOGUE  DGBFA
      INTEGER LDA,N,ML,MU,IPVT(1),INFO
      DOUBLE PRECISION ABD(LDA,1)
!C
      DOUBLE PRECISION T
      INTEGER I,IDAMAX,I0,J,JU,JZ,J0,J1,K,KP1,L,LM,M,MM,NM1
!C
!C***FIRST EXECUTABLE STATEMENT  DGBFA
      M = ML + MU + 1
      INFO = 0
!C
!C     ZERO INITIAL FILL-IN COLUMNS
!C
      J0 = MU + 2
      J1 = MIN0(N,M) - 1
      IF (J1 .LT. J0) GO TO 30
      DO 20 JZ = J0, J1
         I0 = M + 1 - JZ
         DO 10 I = I0, ML
            ABD(I,JZ) = 0.0D0
   10    CONTINUE
   20 CONTINUE
   30 CONTINUE
      JZ = J1
      JU = 0
!C
!C     GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING
!C
      NM1 = N - 1
      IF (NM1 .LT. 1) GO TO 130
      DO 120 K = 1, NM1
         KP1 = K + 1
!C
!C        ZERO NEXT FILL-IN COLUMN
!C
         JZ = JZ + 1
         IF (JZ .GT. N) GO TO 50
         IF (ML .LT. 1) GO TO 50
            DO 40 I = 1, ML
               ABD(I,JZ) = 0.0D0
   40       CONTINUE
   50    CONTINUE
!C
!C        FIND L = PIVOT INDEX
!C
         LM = MIN0(ML,N-K)
         L = IDAMAX(LM+1,ABD(M,K),1) + M - 1
         IPVT(K) = L + K - M
!C
!C        ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED
!C
         IF (ABD(L,K) .EQ. 0.0D0) GO TO 100
!C
!C           INTERCHANGE IF NECESSARY
!C
            IF (L .EQ. M) GO TO 60
               T = ABD(L,K)
               ABD(L,K) = ABD(M,K)
               ABD(M,K) = T
   60       CONTINUE
!C
!C           COMPUTE MULTIPLIERS
!C
            T = -1.0D0/ABD(M,K)
            CALL DSCAL(LM,T,ABD(M+1,K),1)
!C
!C           ROW ELIMINATION WITH COLUMN INDEXING
!C
            JU = MIN0(MAX0(JU,MU+IPVT(K)),N)
            MM = M
            IF (JU .LT. KP1) GO TO 90
            DO 80 J = KP1, JU
               L = L - 1
               MM = MM - 1
               T = ABD(L,J)
               IF (L .EQ. MM) GO TO 70
                  ABD(L,J) = ABD(MM,J)
                  ABD(MM,J) = T
   70          CONTINUE
               CALL DAXPY(LM,T,ABD(M+1,K),1,ABD(MM+1,J),1)
   80       CONTINUE
   90       CONTINUE
         GO TO 110
  100    CONTINUE
            INFO = K
  110    CONTINUE
  120 CONTINUE
  130 CONTINUE
      IPVT(N) = N
      IF (ABD(M,N) .EQ. 0.0D0) INFO = N
      RETURN
    END SUBROUTINE
    
      SUBROUTINE DGBSL(ABD,LDA,N,ML,MU,IPVT,B,JOB)
!C***BEGIN PROLOGUE  DGBSL
!C   THIS PROLOGUE HAS BEEN OMITTED FOR REASONS OF SPACE
!C   FOR A COMPLETE COPY OF THIS SUBROUTINE CONTACT THE AUTHORS
!C    From the book "Numerical Methods and Software"
!C       by D. Kahaner, C. Moler, S. Nash
!C          Prentice Hall 1988
!C***END PROLOGUE  DGBSL
      INTEGER LDA,N,ML,MU,IPVT(1),JOB
      DOUBLE PRECISION ABD(LDA,1),B(1)
!C
      DOUBLE PRECISION DDOT,T
      INTEGER K,KB,L,LA,LB,LM,M,NM1
!C***FIRST EXECUTABLE STATEMENT  DGBSL
      M = MU + ML + 1
      NM1 = N - 1
      IF (JOB .NE. 0) GO TO 50
!C
!C        JOB = 0 , SOLVE  A * X = B
!C        FIRST SOLVE L*Y = B
!C
         IF (ML .EQ. 0) GO TO 30
         IF (NM1 .LT. 1) GO TO 30
            DO 20 K = 1, NM1
               LM = MIN0(ML,N-K)
               L = IPVT(K)
               T = B(L)
               IF (L .EQ. K) GO TO 10
                  B(L) = B(K)
                  B(K) = T
   10          CONTINUE
               CALL DAXPY(LM,T,ABD(M+1,K),1,B(K+1),1)
   20       CONTINUE
   30    CONTINUE
!C
!C        NOW SOLVE  U*X = Y
!C
         DO 40 KB = 1, N
            K = N + 1 - KB
            B(K) = B(K)/ABD(M,K)
            LM = MIN0(K,M) - 1
            LA = M - LM
            LB = K - LM
            T = -B(K)
            CALL DAXPY(LM,T,ABD(LA,K),1,B(LB),1)
   40    CONTINUE
      GO TO 100
   50 CONTINUE
!C
!C        JOB = NONZERO, SOLVE  TRANS(A) * X = B
!C        FIRST SOLVE  TRANS(U)*Y = B
!C
         DO 60 K = 1, N
            LM = MIN0(K,M) - 1
            LA = M - LM
            LB = K - LM
            T = DDOT(LM,ABD(LA,K),1,B(LB),1)
            B(K) = (B(K) - T)/ABD(M,K)
   60    CONTINUE
!C
!C        NOW SOLVE TRANS(L)*X = Y
!C
         IF (ML .EQ. 0) GO TO 90
         IF (NM1 .LT. 1) GO TO 90
            DO 80 KB = 1, NM1
               K = N - KB
               LM = MIN0(ML,N-K)
               B(K) = B(K) + DDOT(LM,ABD(M+1,K),1,B(K+1),1)
               L = IPVT(K)
               IF (L .EQ. K) GO TO 70
                  T = B(L)
                  B(L) = B(K)
                  B(K) = T
   70          CONTINUE
   80       CONTINUE
   90    CONTINUE
  100 CONTINUE
      RETURN
    END SUBROUTINE
    

    SUBROUTINE DGEFS(A,LDA,N,V,ITASK,IND,WORK,IWORK,RCOND)
#if 0
C***BEGIN PROLOGUE  DGEFS
C***DATE WRITTEN   800326   (YYMMDD)
C***REVISION DATE  861211   (YYMMDD)
C***CATEGORY NO.  D2A1
C***AUTHOR  VOORHEES, E., (LANL)
C***PURPOSE  DGEFS solves a GENERAL double precision
C            NXN system of linear equations.
C***DESCRIPTION
C    From the book "Numerical Methods and Software"
C       by D. Kahaner, C. Moler, S. Nash
C          Prentice Hall 1988
C
C    Subroutine DGEFS solves a general NxN system of double
C    precision linear equations using LINPACK subroutines DGECO
C    and DGESL.  That is, if A is an NxN double precision matrix
C    and if X and B are double precision N-vectors, then DGEFS
C    solves the equation
C
C                          A*X=B.
C
C    The matrix A is first factored into upper and lower tri-
C    angular matrices U and L using partial pivoting.  These
C    factors and the pivoting information are used to find the
C    solution vector X.  An approximate condition number is
C    calculated to provide a rough estimate of the number of
C    digits of accuracy in the computed solution.
C
C    If the equation A*X=B is to be solved for more than one vector
C    B, the factoring of A does not need to be performed again and
C    the option to only solve (ITASK.GT.1) will be faster for
C    the succeeding solutions.  In this case, the contents of A,
C    LDA, N and IWORK must not have been altered by the user follow-
C    ing factorization (ITASK=1).  IND will not be changed by DGEFS
C    in this case.
C
C  Argument Description ***
C
C    A      DOUBLE PRECISION(LDA,N)
C             on entry, the doubly subscripted array with dimension
C               (LDA,N) which contains the coefficient matrix.
C             on return, an upper triangular matrix U and the
C               multipliers necessary to construct a matrix L
C               so that A=L*U.
C    LDA    INTEGER
C             the leading dimension of the array A.  LDA must be great-
C             er than or equal to N.  (terminal error message IND=-1)
C    N      INTEGER
C             the order of the matrix A.  The first N elements of
C             the array A are the elements of the first column of
C             the matrix A.  N must be greater than or equal to 1.
C             (terminal error message IND=-2)
C    V      DOUBLE PRECISION(N)
C             on entry, the singly subscripted array(vector) of di-
C               mension N which contains the right hand side B of a
C               system of simultaneous linear equations A*X=B.
C             on return, V contains the solution vector, X .
C    ITASK  INTEGER
C             If ITASK=1, the matrix A is factored and then the
C               linear equation is solved.
C             If ITASK .GT. 1, the equation is solved using the existing
C               factored matrix A and IWORK.
C             If ITASK .LT. 1, then terminal error message IND=-3 is
C               printed.
C    IND    INTEGER
C             GT. 0  IND is a rough estimate of the number of digits
C                     of accuracy in the solution, X.
C             LT. 0  see error message corresponding to IND below.
C    WORK   DOUBLE PRECISION(N)
C             a singly subscripted array of dimension at least N.
C    IWORK  INTEGER(N)
C             a singly subscripted array of dimension at least N.
C
C  Error Messages Printed ***
C
C    IND=-1  terminal   N is greater than LDA.
C    IND=-2  terminal   N is less than 1.
C    IND=-3  terminal   ITASK is less than 1.
C    IND=-4  terminal   The matrix A is computationally singular.
C                         A solution has not been computed.
C    IND=-10 warning    The solution has no apparent significance.
C                         The solution may be inaccurate or the matrix
C                         A may be poorly scaled.
C
C               Note-  The above terminal(*fatal*) error messages are
C                      designed to be handled by XERRWV in which
C                      LEVEL=1 (recoverable) and IFLAG=2 .  LEVEL=0
C                      for warning error messages from XERROR.  Unless
C                      the user provides otherwise, an error message
C                      will be printed followed by an abort.
C***REFERENCES  SUBROUTINE DGEFS WAS DEVELOPED BY GROUP C-3, LOS ALAMOS
C                 SCIENTIFIC LABORATORY, LOS ALAMOS, NM 87545.
C                 THE LINPACK SUBROUTINES USED BY DGEFS ARE DESCRIBED IN
C                 DETAIL IN THE *LINPACK USERS GUIDE* PUBLISHED BY
C                 THE SOCIETY FOR INDUSTRIAL AND APPLIED MATHEMATICS
C                 (SIAM) DATED 1979.
C***ROUTINES CALLED  D1MACH,DGECO,DGESL,XERROR,XERRWV
C***END PROLOGUE  DGEFS
C
#endif
      INTEGER LDA,N,ITASK,IND,IWORK(N)
      DOUBLE PRECISION A(LDA,N),V(N),WORK(N),D1MACH
      DOUBLE PRECISION RCOND
!C***FIRST EXECUTABLE STATEMENT  DGEFS
      IF (LDA.LT.N)  GO TO 101
      IF (N.LE.0)  GO TO 102
      IF (ITASK.LT.1) GO TO 103
      IF (ITASK.GT.1) GO TO 20
!C
!C     FACTOR MATRIX A INTO LU
      CALL DGECO(A,LDA,N,IWORK,RCOND,WORK)
!C
!C     CHECK FOR COMPUTATIONALLY SINGULAR MATRIX
      IF (RCOND.EQ.0.0D0)  GO TO 104
!C
!C     COMPUTE IND (ESTIMATE OF NO. OF SIGNIFICANT DIGITS)
      IND=-IDINT(DLOG10(D1MACH(4)/RCOND))
!C
!C     CHECK FOR IND GREATER THAN ZERO
      IF (IND.GT.0)  GO TO 20
      IND=-10
      CALL XERROR( 'DGEFS ERROR (IND=-10) -- SOLUTION MAY HAVE NO SIGNIF
     1ICANCE',58,-10,0)
!C
!C     SOLVE AFTER FACTORING
   20 CALL DGESL(A,LDA,N,IWORK,V,0)
      RETURN
!C
!C     IF LDA.LT.N, IND=-1, TERMINAL XERRWV MESSAGE
  101 IND=-1
      CALL XERRWV( 'DGEFS ERROR (IND=-1) -- LDA=I1 IS LESS THAN N=I2',
     148,-1,1,2,LDA,N,0,0,0)
      RETURN
!C
!C     IF N.LT.1, IND=-2, TERMINAL XERRWV MESSAGE
  102 IND=-2
      CALL XERRWV( 'DGEFS ERROR (IND=-2) -- N=I1 IS LESS THAN 1',
     143,-2,1,1,N,0,0,0,0)
      RETURN
!C
!C     IF ITASK.LT.1, IND=-3, TERMINAL XERRWV MESSAGE
  103 IND=-3
      CALL XERRWV( 'DGEFS ERROR (IND=-3) -- ITASK=I1 IS LESS THAN 1',
     147,-3,1,1,ITASK,0,0,0,0)
      RETURN
!C
!C     IF SINGULAR MATRIX, IND=-4, TERMINAL XERRWV MESSAGE
  104 IND=-4
      CALL XERRWV( 'DGEFS ERROR (IND=-4) -- SINGULAR MATRIX A - NO SOLUT
     1ION',55,-4,1,0,0,0,0,0,0)
      RETURN
!C
      END SUBROUTINE

      SUBROUTINE DGESL(A,LDA,N,IPVT,B,JOB)
!C***BEGIN PROLOGUE  DGESL
!C     THIS PROLOGUE HAS BEEN REMOVED FOR REASONS OF SPACE
!C     FOR A COMPLETE COPY OF THIS ROUTINE CONTACT THE AUTHORS
!C     From the book "Numerical Methods and Software"
!C          by  D. Kahaner, C. Moler, S. Nash
!C               Prentice Hall 1988
!C***ROUTINES CALLED  DAXPY,DDOT
!C***END PROLOGUE  DGESL
      INTEGER LDA,N,IPVT(1),JOB
      DOUBLE PRECISION A(LDA,1),B(1)
!C
      DOUBLE PRECISION DDOT,T
      INTEGER K,KB,L,NM1
!C***FIRST EXECUTABLE STATEMENT  DGESL
      NM1 = N - 1
      IF (JOB .NE. 0) GO TO 50
!C
!C        JOB = 0 , SOLVE  A * X = B
!C        FIRST SOLVE  L*Y = B
!C
         IF (NM1 .LT. 1) GO TO 30
         DO 20 K = 1, NM1
            L = IPVT(K)
            T = B(L)
            IF (L .EQ. K) GO TO 10
               B(L) = B(K)
               B(K) = T
   10       CONTINUE
            CALL DAXPY(N-K,T,A(K+1,K),1,B(K+1),1)
   20    CONTINUE
   30    CONTINUE
!C
!C        NOW SOLVE  U*X = Y
!C
         DO 40 KB = 1, N
            K = N + 1 - KB
            B(K) = B(K)/A(K,K)
            T = -B(K)
            CALL DAXPY(K-1,T,A(1,K),1,B(1),1)
   40    CONTINUE
      GO TO 100
   50 CONTINUE
!C
!C        JOB = NONZERO, SOLVE  TRANS(A) * X = B
!C        FIRST SOLVE  TRANS(U)*Y = B
!C
         DO 60 K = 1, N
            T = DDOT(K-1,A(1,K),1,B(1),1)
            B(K) = (B(K) - T)/A(K,K)
   60    CONTINUE
!C
!C        NOW SOLVE TRANS(L)*X = Y
!C
         IF (NM1 .LT. 1) GO TO 90
         DO 80 KB = 1, NM1
            K = N - KB
            B(K) = B(K) + DDOT(N-K,A(K+1,K),1,B(K+1),1)
            L = IPVT(K)
            IF (L .EQ. K) GO TO 70
               T = B(L)
               B(L) = B(K)
               B(K) = T
   70       CONTINUE
   80    CONTINUE
   90    CONTINUE
  100 CONTINUE
      RETURN
      END

      SUBROUTINE DGECO(A,LDA,N,IPVT,RCOND,Z)
!C***BEGIN PROLOGUE  DGECO
!C     THIS PROLOGUE HAS BEEN REMOVED FOR REASONS OF SPACE
!C     FOR A COMPLETE COPY OF THIS ROUTINE CONTACT THE AUTHORS
!C     From the book "Numerical Methods and Software"
!C          by  D. Kahaner, C. Moler, S. Nash
!C               Prentice Hall 1988
!C***ROUTINES CALLED  DASUM,DAXPY,DDOT,DGEFA,DSCAL
!C***END PROLOGUE  DGECO
      INTEGER LDA,N,IPVT(1)
      DOUBLE PRECISION A(LDA,1),Z(1)
      DOUBLE PRECISION RCOND
!C
      DOUBLE PRECISION DDOT,EK,T,WK,WKM
      DOUBLE PRECISION ANORM,S,DASUM,SM,YNORM
      INTEGER INFO,J,K,KB,KP1,L
!C
!C     COMPUTE 1-NORM OF A
!C
!C***FIRST EXECUTABLE STATEMENT  DGECO
      ANORM = 0.0D0
      DO 10 J = 1, N
         ANORM = DMAX1(ANORM,DASUM(N,A(1,J),1))
   10 CONTINUE
!C
!C     FACTOR
!C
      CALL DGEFA(A,LDA,N,IPVT,INFO)
!C
!C     RCOND = 1/(NORM(A)*(ESTIMATE OF NORM(INVERSE(A)))) .
!C     ESTIMATE = NORM(Z)/NORM(Y) WHERE  A*Z = Y  AND  TRANS(A)*Y = E .
!C     TRANS(A)  IS THE TRANSPOSE OF A .  THE COMPONENTS OF  E  ARE
!C     CHOSEN TO CAUSE MAXIMUM LOCAL GROWTH IN THE ELEMENTS OF W  WHERE
!C     TRANS(U)*W = E .  THE VECTORS ARE FREQUENTLY RESCALED TO AVOID
!C     OVERFLOW.
!C
!C     SOLVE TRANS(U)*W = E
!C
      EK = 1.0D0
      DO 20 J = 1, N
         Z(J) = 0.0D0
   20 CONTINUE
      DO 100 K = 1, N
         IF (Z(K) .NE. 0.0D0) EK = DSIGN(EK,-Z(K))
         IF (DABS(EK-Z(K)) .LE. DABS(A(K,K))) GO TO 30
            S = DABS(A(K,K))/DABS(EK-Z(K))
            CALL DSCAL(N,S,Z,1)
            EK = S*EK
   30    CONTINUE
         WK = EK - Z(K)
         WKM = -EK - Z(K)
         S = DABS(WK)
         SM = DABS(WKM)
         IF (A(K,K) .EQ. 0.0D0) GO TO 40
            WK = WK/A(K,K)
            WKM = WKM/A(K,K)
         GO TO 50
   40    CONTINUE
            WK = 1.0D0
            WKM = 1.0D0
   50    CONTINUE
         KP1 = K + 1
         IF (KP1 .GT. N) GO TO 90
            DO 60 J = KP1, N
               SM = SM + DABS(Z(J)+WKM*A(K,J))
               Z(J) = Z(J) + WK*A(K,J)
               S = S + DABS(Z(J))
   60       CONTINUE
            IF (S .GE. SM) GO TO 80
               T = WKM - WK
               WK = WKM
               DO 70 J = KP1, N
                  Z(J) = Z(J) + T*A(K,J)
   70          CONTINUE
   80       CONTINUE
   90    CONTINUE
         Z(K) = WK
  100 CONTINUE
      S = 1.0D0/DASUM(N,Z,1)
      CALL DSCAL(N,S,Z,1)
!C
!C     SOLVE TRANS(L)*Y = W
!C
      DO 120 KB = 1, N
         K = N + 1 - KB
         IF (K .LT. N) Z(K) = Z(K) + DDOT(N-K,A(K+1,K),1,Z(K+1),1)
         IF (DABS(Z(K)) .LE. 1.0D0) GO TO 110
            S = 1.0D0/DABS(Z(K))
            CALL DSCAL(N,S,Z,1)
  110    CONTINUE
         L = IPVT(K)
         T = Z(L)
         Z(L) = Z(K)
         Z(K) = T
  120 CONTINUE
      S = 1.0D0/DASUM(N,Z,1)
      CALL DSCAL(N,S,Z,1)
!C
      YNORM = 1.0D0
!C
!C     SOLVE L*V = Y
!C
      DO 140 K = 1, N
         L = IPVT(K)
         T = Z(L)
         Z(L) = Z(K)
         Z(K) = T
         IF (K .LT. N) CALL DAXPY(N-K,T,A(K+1,K),1,Z(K+1),1)
         IF (DABS(Z(K)) .LE. 1.0D0) GO TO 130
            S = 1.0D0/DABS(Z(K))
            CALL DSCAL(N,S,Z,1)
            YNORM = S*YNORM
  130    CONTINUE
  140 CONTINUE
      S = 1.0D0/DASUM(N,Z,1)
      CALL DSCAL(N,S,Z,1)
      YNORM = S*YNORM
!C
!C     SOLVE  U*Z = V
!C
      DO 160 KB = 1, N
         K = N + 1 - KB
         IF (DABS(Z(K)) .LE. DABS(A(K,K))) GO TO 150
            S = DABS(A(K,K))/DABS(Z(K))
            CALL DSCAL(N,S,Z,1)
            YNORM = S*YNORM
  150    CONTINUE
         IF (A(K,K) .NE. 0.0D0) Z(K) = Z(K)/A(K,K)
         IF (A(K,K) .EQ. 0.0D0) Z(K) = 1.0D0
         T = -Z(K)
         CALL DAXPY(K-1,T,A(1,K),1,Z(1),1)
  160 CONTINUE
!C     MAKE ZNORM = 1.0
      S = 1.0D0/DASUM(N,Z,1)
      CALL DSCAL(N,S,Z,1)
      YNORM = S*YNORM
!C
      IF (ANORM .NE. 0.0D0) RCOND = YNORM/ANORM
      IF (ANORM .EQ. 0.0D0) RCOND = 0.0D0
      RETURN
      END SUBROUTINE

      SUBROUTINE DGEFA(A,LDA,N,IPVT,INFO)
C***BEGIN PROLOGUE  DGEFA
C     THIS PROLOGUE HAS BEEN REMOVED FOR REASONS OF SPACE
C     FOR A COMPLETE COPY OF THIS ROUTINE CONTACT THE AUTHORS
C     From the book "Numerical Methods and Software"
C          by  D. Kahaner, C. Moler, S. Nash
C               Prentice Hall 1988
C***ROUTINES CALLED  DAXPY,DSCAL,IDAMAX
C***END PROLOGUE  DGEFA
      INTEGER LDA,N,IPVT(1),INFO
      DOUBLE PRECISION A(LDA,1)
C
      DOUBLE PRECISION T
      INTEGER IDAMAX,J,K,KP1,L,NM1
C
C     GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING
C
C***FIRST EXECUTABLE STATEMENT  DGEFA
      INFO = 0
      NM1 = N - 1
      IF (NM1 .LT. 1) GO TO 70
      DO 60 K = 1, NM1
         KP1 = K + 1
C
C        FIND L = PIVOT INDEX
C
         L = IDAMAX(N-K+1,A(K,K),1) + K - 1
         IPVT(K) = L
C
C        ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED
C
         IF (A(L,K) .EQ. 0.0D0) GO TO 40
C
C           INTERCHANGE IF NECESSARY
C
            IF (L .EQ. K) GO TO 10
               T = A(L,K)
               A(L,K) = A(K,K)
               A(K,K) = T
   10       CONTINUE
C
C           COMPUTE MULTIPLIERS
C
            T = -1.0D0/A(K,K)
            CALL DSCAL(N-K,T,A(K+1,K),1)
C
C           ROW ELIMINATION WITH COLUMN INDEXING
C
            DO 30 J = KP1, N
               T = A(L,J)
               IF (L .EQ. K) GO TO 20
                  A(L,J) = A(K,J)
                  A(K,J) = T
   20          CONTINUE
               CALL DAXPY(N-K,T,A(K+1,K),1,A(K+1,J),1)
   30       CONTINUE
         GO TO 50
   40    CONTINUE
            INFO = K
   50    CONTINUE
   60 CONTINUE
   70 CONTINUE
      IPVT(N) = N
      IF (A(N,N) .EQ. 0.0D0) INFO = N
      RETURN
      END



  





end module mol_solvers
