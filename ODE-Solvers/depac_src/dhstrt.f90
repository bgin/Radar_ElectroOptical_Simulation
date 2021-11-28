!** DHSTRT
SUBROUTINE DHSTRT(DF,Neq,A,B,Y,Yprime,Etol,Morder,Small,Big,Spy,Pv,Yp,Sf,H)
  !> Subsidiary to DDEABM, DDEBDF and DDERKF
  !***
  ! **Library:**   SLATEC
  !***
  ! **Type:**      DOUBLE PRECISION (HSTART-S, DHSTRT-D)
  !***
  ! **Author:**  Watts, H. A., (SNLA)
  !***
  ! **Description:**
  !
  !   DHSTRT computes a starting step size to be used in solving initial
  !   value problems in ordinary differential equations.
  !
  !- *********************************************************************
  !  ABSTRACT
  !
  !     Subroutine DHSTRT computes a starting step size to be used by an
  !     initial value method in solving ordinary differential equations.
  !     It is based on an estimate of the local Lipschitz constant for the
  !     differential equation   (lower bound on a norm of the Jacobian) ,
  !     a bound on the differential equation  (first derivative), and
  !     a bound on the partial derivative of the equation with respect to
  !     the independent variable.
  !     (all approximated near the initial point A)
  !
  !     Subroutine DHSTRT uses a function subprogram DHVNRM for computing
  !     a vector norm. The maximum norm is presently utilized though it
  !     can easily be replaced by any other vector norm. It is presumed
  !     that any replacement norm routine would be carefully coded to
  !     prevent unnecessary underflows or overflows from occurring, and
  !     also, would not alter the vector or number of components.
  !
  !- *********************************************************************
  !  On input you must provide the following
  !
  !      DF -- This is a subroutine of the form
  !                               DF(X,U,UPRIME,RPAR,IPAR)
  !             which defines the system of first order differential
  !             equations to be solved. For the given values of X and the
  !             vector  U(*)=(U(1),U(2),...,U(NEQ)), the subroutine must
  !             evaluate the NEQ components of the system of differential
  !             equations  DU/DX=DF(X,U)  and store the derivatives in the
  !             array UPRIME(*), that is,  UPRIME(I) = * DU(I)/DX *  for
  !             equations I=1,...,NEQ.
  !
  !             Subroutine DF must not alter X or U(*). You must declare
  !             the name DF in an external statement in your program that
  !             calls DHSTRT. You must dimension U and UPRIME in DF.
  !
  !             RPAR and IPAR are DOUBLE PRECISION and INTEGER parameter
  !             arrays which you can use for communication between your
  !             program and subroutine DF. They are not used or altered by
  !             DHSTRT. If you do not need RPAR or IPAR, ignore these
  !             parameters by treating them as dummy arguments. If you do
  !             choose to use them, dimension them in your program and in
  !             DF as arrays of appropriate length.
  !
  !      NEQ -- This is the number of (first order) differential equations
  !             to be integrated.
  !
  !      A -- This is the initial point of integration.
  !
  !      B -- This is a value of the independent variable used to define
  !             the direction of integration. A reasonable choice is to
  !             set  B  to the first point at which a solution is desired.
  !             You can also use  B, if necessary, to restrict the length
  !             of the first integration step because the algorithm will
  !             not compute a starting step length which is bigger than
  !             ABS(B-A), unless  B  has been chosen too close to  A.
  !             (it is presumed that DHSTRT has been called with  B
  !             different from  A  on the machine being used. Also see the
  !             discussion about the parameter  SMALL.)
  !
  !      Y(*) -- This is the vector of initial values of the NEQ solution
  !             components at the initial point  A.
  !
  !      YPRIME(*) -- This is the vector of derivatives of the NEQ
  !             solution components at the initial point  A.
  !             (defined by the differential equations in subroutine DF)
  !
  !      ETOL -- This is the vector of error tolerances corresponding to
  !             the NEQ solution components. It is assumed that all
  !             elements are positive. Following the first integration
  !             step, the tolerances are expected to be used by the
  !             integrator in an error test which roughly requires that
  !                        ABS(LOCAL ERROR)  <=  ETOL
  !             for each vector component.
  !
  !      MORDER -- This is the order of the formula which will be used by
  !             the initial value method for taking the first integration
  !             step.
  !
  !      SMALL -- This is a small positive machine dependent constant
  !             which is used for protecting against computations with
  !             numbers which are too small relative to the precision of
  !             floating point arithmetic.  SMALL  should be set to
  !             (approximately) the smallest positive DOUBLE PRECISION
  !             number such that  (1.+SMALL) > 1.  on the machine being
  !             used. The quantity  SMALL**(3/8)  is used in computing
  !             increments of variables for approximating derivatives by
  !             differences.  Also the algorithm will not compute a
  !             starting step length which is smaller than
  !             100*SMALL*ABS(A).
  !
  !      BIG -- This is a large positive machine dependent constant which
  !             is used for preventing machine overflows. A reasonable
  !             choice is to set big to (approximately) the square root of
  !             the largest DOUBLE PRECISION number which can be held in
  !             the machine.
  !
  !      SPY(*),PV(*),YP(*),SF(*) -- These are DOUBLE PRECISION work
  !             arrays of length NEQ which provide the routine with needed
  !             storage space.
  !
  !      RPAR,IPAR -- These are parameter arrays, of DOUBLE PRECISION and
  !             INTEGER type, respectively, which can be used for
  !             communication between your program and the DF subroutine.
  !             They are not used or altered by DHSTRT.
  !
  !- *********************************************************************
  !  On Output  (after the return from DHSTRT),
  !
  !      H -- is an appropriate starting step size to be attempted by the
  !             differential equation method.
  !
  !           All parameters in the call list remain unchanged except for
  !           the working arrays SPY(*),PV(*),YP(*), and SF(*).
  !
  !- *********************************************************************
  !
  !***
  ! **See also:**  DDEABM, DDEBDF, DDERKF
  !***
  ! **Routines called:**  DHVNRM

  !* REVISION HISTORY  (YYMMDD)
  !   820301  DATE WRITTEN
  !   890531  Changed all specific intrinsics to generic.  (WRB)
  !   890831  Modified array declarations.  (WRB)
  !   890911  Removed unnecessary intrinsics.  (WRB)
  !   891024  Changed references from DVNORM to DHVNRM.  (WRB)
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
  INTEGER, INTENT(IN) :: Morder, Neq
  REAL(DP), INTENT(IN) :: A, B, Big, Small
  REAL(DP), INTENT(OUT) :: H
  REAL(DP), INTENT(IN) :: Etol(Neq), Y(Neq), Yprime(Neq)
  REAL(DP), INTENT(OUT) :: Pv(Neq), Sf(Neq), Spy(Neq), Yp(Neq)
  !
  INTEGER :: j, k, lk
  REAL(DP) :: absdx, da, delf, dely, dfdub, dfdxb, dx, dy, fbnd, relper, srydpb, &
    tolexp, tolmin, tolp, tolsum, ydpb
  !
  !     ..................................................................
  !
  !     BEGIN BLOCK PERMITTING ...EXITS TO 160
  !* FIRST EXECUTABLE STATEMENT  DHSTRT
  dx = B - A
  absdx = ABS(dx)
  relper = Small**0.375_DP
  !
  !        ...............................................................
  !
  !             COMPUTE AN APPROXIMATE BOUND (DFDXB) ON THE PARTIAL
  !             DERIVATIVE OF THE EQUATION WITH RESPECT TO THE
  !             INDEPENDENT VARIABLE. PROTECT AGAINST AN OVERFLOW.
  !             ALSO COMPUTE A BOUND (FBND) ON THE FIRST DERIVATIVE
  !             LOCALLY.
  !
  da = SIGN(MAX(MIN(relper*ABS(A),absdx),100._DP*Small*ABS(A)),dx)
  IF( da==0._DP ) da = relper*dx
  CALL DF(A+da,Y,Sf)
  DO j = 1, Neq
    Yp(j) = Sf(j) - Yprime(j)
  END DO
  delf = DHVNRM(Yp,Neq)
  dfdxb = Big
  IF( delf<Big*ABS(da) ) dfdxb = delf/ABS(da)
  fbnd = DHVNRM(Sf,Neq)
  !
  !        ...............................................................
  !
  !             COMPUTE AN ESTIMATE (DFDUB) OF THE LOCAL LIPSCHITZ
  !             CONSTANT FOR THE SYSTEM OF DIFFERENTIAL EQUATIONS. THIS
  !             ALSO REPRESENTS AN ESTIMATE OF THE NORM OF THE JACOBIAN
  !             LOCALLY.  THREE ITERATIONS (TWO WHEN NEQ=1) ARE USED TO
  !             ESTIMATE THE LIPSCHITZ CONSTANT BY NUMERICAL DIFFERENCES.
  !             THE FIRST PERTURBATION VECTOR IS BASED ON THE INITIAL
  !             DERIVATIVES AND DIRECTION OF INTEGRATION. THE SECOND
  !             PERTURBATION VECTOR IS FORMED USING ANOTHER EVALUATION OF
  !             THE DIFFERENTIAL EQUATION.  THE THIRD PERTURBATION VECTOR
  !             IS FORMED USING PERTURBATIONS BASED ONLY ON THE INITIAL
  !             VALUES. COMPONENTS THAT ARE ZERO ARE ALWAYS CHANGED TO
  !             NON-ZERO VALUES (EXCEPT ON THE FIRST ITERATION). WHEN
  !             INFORMATION IS AVAILABLE, CARE IS TAKEN TO ENSURE THAT
  !             COMPONENTS OF THE PERTURBATION VECTOR HAVE SIGNS WHICH ARE
  !             CONSISTENT WITH THE SLOPES OF LOCAL SOLUTION CURVES.
  !             ALSO CHOOSE THE LARGEST BOUND (FBND) FOR THE FIRST
  !             DERIVATIVE.
  !
  !                               PERTURBATION VECTOR SIZE IS HELD
  !                               CONSTANT FOR ALL ITERATIONS. COMPUTE
  !                               THIS CHANGE FROM THE
  !                                       SIZE OF THE VECTOR OF INITIAL
  !                                       VALUES.
  dely = relper*DHVNRM(Y,Neq)
  IF( dely==0._DP ) dely = relper
  dely = SIGN(dely,dx)
  delf = DHVNRM(Yprime,Neq)
  fbnd = MAX(fbnd,delf)
  IF( delf==0._DP ) THEN
    !           CANNOT HAVE A NULL PERTURBATION VECTOR
    DO j = 1, Neq
      Spy(j) = 0._DP
      Yp(j) = 1._DP
    END DO
    delf = DHVNRM(Yp,Neq)
  ELSE
    !           USE INITIAL DERIVATIVES FOR FIRST PERTURBATION
    DO j = 1, Neq
      Spy(j) = Yprime(j)
      Yp(j) = Yprime(j)
    END DO
  END IF
  !
  dfdub = 0._DP
  lk = MIN(Neq+1,3)
  DO k = 1, lk
    !           DEFINE PERTURBED VECTOR OF INITIAL VALUES
    DO j = 1, Neq
      Pv(j) = Y(j) + dely*(Yp(j)/delf)
    END DO
    IF( k==2 ) THEN
      !              USE A SHIFTED VALUE OF THE INDEPENDENT VARIABLE
      !                                    IN COMPUTING ONE ESTIMATE
      CALL DF(A+da,Pv,Yp)
      DO j = 1, Neq
        Pv(j) = Yp(j) - Sf(j)
      END DO
    ELSE
      !              EVALUATE DERIVATIVES ASSOCIATED WITH PERTURBED
      !              VECTOR  AND  COMPUTE CORRESPONDING DIFFERENCES
      CALL DF(A,Pv,Yp)
      DO j = 1, Neq
        Pv(j) = Yp(j) - Yprime(j)
      END DO
    END IF
    !           CHOOSE LARGEST BOUNDS ON THE FIRST DERIVATIVE
    !                          AND A LOCAL LIPSCHITZ CONSTANT
    fbnd = MAX(fbnd,DHVNRM(Yp,Neq))
    delf = DHVNRM(Pv,Neq)
    !        ...EXIT
    IF( delf>=Big*ABS(dely) ) EXIT
    dfdub = MAX(dfdub,delf/ABS(dely))
    !     ......EXIT
    IF( k==lk ) GOTO 100
    !           CHOOSE NEXT PERTURBATION VECTOR
    IF( delf==0._DP ) delf = 1._DP
    DO j = 1, Neq
      IF( k==2 ) THEN
        dy = Y(j)
        IF( dy==0._DP ) dy = dely/relper
      ELSE
        dy = ABS(Pv(j))
        IF( dy==0._DP ) dy = delf
      END IF
      IF( Spy(j)==0._DP ) Spy(j) = Yp(j)
      IF( Spy(j)/=0._DP ) dy = SIGN(dy,Spy(j))
      Yp(j) = dy
    END DO
    delf = DHVNRM(Yp,Neq)
  END DO
  !
  !        PROTECT AGAINST AN OVERFLOW
  dfdub = Big
  !
  !     ..................................................................
  !
  !          COMPUTE A BOUND (YDPB) ON THE NORM OF THE SECOND DERIVATIVE
  !
  100  ydpb = dfdxb + dfdub*fbnd
  !
  !     ..................................................................
  !
  !          DEFINE THE TOLERANCE PARAMETER UPON WHICH THE STARTING STEP
  !          SIZE IS TO BE BASED.  A VALUE IN THE MIDDLE OF THE ERROR
  !          TOLERANCE RANGE IS SELECTED.
  !
  tolmin = Big
  tolsum = 0._DP
  DO k = 1, Neq
    tolexp = LOG10(Etol(k))
    tolmin = MIN(tolmin,tolexp)
    tolsum = tolsum + tolexp
  END DO
  tolp = 10._DP**(0.5_DP*(tolsum/Neq+tolmin)/(Morder+1))
  !
  !     ..................................................................
  !
  !          COMPUTE A STARTING STEP SIZE BASED ON THE ABOVE FIRST AND
  !          SECOND DERIVATIVE INFORMATION
  !
  !                            RESTRICT THE STEP LENGTH TO BE NOT BIGGER
  !                            THAN ABS(B-A).   (UNLESS  B  IS TOO CLOSE
  !                            TO  A)
  H = absdx
  !
  IF( ydpb==0._DP .AND. fbnd==0._DP ) THEN
    !
    !        BOTH FIRST DERIVATIVE TERM (FBND) AND SECOND
    !                     DERIVATIVE TERM (YDPB) ARE ZERO
    IF( tolp<1._DP ) H = absdx*tolp
    !
  ELSEIF( ydpb/=0._DP ) THEN
    !
    !        SECOND DERIVATIVE TERM (YDPB) IS NON-ZERO
    srydpb = SQRT(0.5_DP*ydpb)
    IF( tolp<srydpb*absdx ) H = tolp/srydpb
  ELSE
    !
    !        ONLY SECOND DERIVATIVE TERM (YDPB) IS ZERO
    IF( tolp<fbnd*absdx ) H = tolp/fbnd
  END IF
  !
  !     FURTHER RESTRICT THE STEP LENGTH TO BE NOT
  !                               BIGGER THAN  1/DFDUB
  IF( H*dfdub>1._DP ) H = 1._DP/dfdub
  !
  !     FINALLY, RESTRICT THE STEP LENGTH TO BE NOT
  !     SMALLER THAN  100*SMALL*ABS(A).  HOWEVER, IF
  !     A=0. AND THE COMPUTED H UNDERFLOWED TO ZERO,
  !     THE ALGORITHM RETURNS  SMALL*ABS(B)  FOR THE
  !                                     STEP LENGTH.
  H = MAX(H,100._DP*Small*ABS(A))
  IF( H==0._DP ) H = Small*ABS(B)
  !
  !     NOW SET DIRECTION OF INTEGRATION
  H = SIGN(H,dx)
  !
END SUBROUTINE DHSTRT