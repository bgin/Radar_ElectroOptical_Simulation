!** HSTART
SUBROUTINE HSTART(F,Neq,A,B,Y,Yprime,Etol,Morder,Small,Big,Spy,Pv,Yp,Sf,H)
  !> Subsidiary to DEABM, DEBDF and DERKF
  !***
  ! **Library:**   SLATEC
  !***
  ! **Type:**      SINGLE PRECISION (HSTART-S, DHSTRT-D)
  !***
  ! **Author:**  Watts, H. A., (SNLA)
  !***
  ! **Description:**
  !
  !   HSTART computes a starting step size to be used in solving initial
  !   value problems in ordinary differential equations.
  !- *********************************************************************
  !  Abstract
  !
  !     Subroutine HSTART computes a starting step size to be used by an
  !     initial value method in solving ordinary differential equations.
  !     It is based on an estimate of the local Lipschitz constant for the
  !     differential equation (lower bound on a norm of the Jacobian),
  !     a bound on the differential equation (first derivative), and
  !     a bound on the partial derivative of the equation with respect to
  !     the independent variable.
  !     (All approximated near the initial point A.)
  !
  !     Subroutine HSTART uses a function subprogram HVNRM for computing
  !     a vector norm.  The maximum norm is presently utilized though it
  !     can easily be replaced by any other vector norm.  It is presumed
  !     that any replacement norm routine would be carefully coded to
  !     prevent unnecessary underflows or overflows from occurring, and
  !     also, would not alter the vector or number of components.
  !
  !- *********************************************************************
  !  On Input you must provide the following
  !
  !      F -- This is a subroutine of the form
  !                               F(X,U,UPRIME,RPAR,IPAR)
  !             which defines the system of first order differential
  !             equations to be solved.  For the given values of X and the
  !             vector  U(*)=(U(1),U(2),...,U(NEQ)), the subroutine must
  !             evaluate the NEQ components of the system of differential
  !             equations  dU/DX=F(X,U)  and store the derivatives in the
  !             array UPRIME(*), that is,  UPRIME(I) = * dU(I)/DX *  for
  !             equations I=1,...,NEQ.
  !
  !             Subroutine F must not alter X or U(*).  You must declare
  !             the name F in an EXTERNAL statement in your program that
  !             calls HSTART.  You must dimension U and UPRIME in F.
  !
  !             RPAR and IPAR are real and integer parameter arrays which
  !             you can use for communication between your program and
  !             subroutine F.  They are not used or altered by HSTART.  If
  !             you do not need RPAR or IPAR, ignore these parameters by
  !             treating them as dummy arguments.  If you do choose to use
  !             them, dimension them in your program and in F as arrays
  !             of appropriate length.
  !
  !      NEQ -- This is the number of (first order) differential equations
  !             to be integrated.
  !
  !      A -- This is the initial point of integration.
  !
  !      B -- This is a value of the independent variable used to define
  !             the direction of integration.  A reasonable choice is to
  !             set  B  to the first point at which a solution is desired.
  !             You can also use  B, if necessary, to restrict the length
  !             of the first integration step because the algorithm will
  !             not compute a starting step length which is bigger than
  !             ABS(B-A), unless  B  has been chosen too close to  A.
  !             (It is presumed that HSTART has been called with  B
  !             different from  A  on the machine being used.  Also see
  !             the discussion about the parameter  SMALL.)
  !
  !      Y(*) -- This is the vector of initial values of the NEQ solution
  !             components at the initial point  A.
  !
  !      YPRIME(*) -- This is the vector of derivatives of the NEQ
  !             solution components at the initial point  A.
  !             (defined by the differential equations in subroutine F)
  !
  !      ETOL -- This is the vector of error tolerances corresponding to
  !             the NEQ solution components.  It is assumed that all
  !             elements are positive.  Following the first integration
  !             step, the tolerances are expected to be used by the
  !             integrator in an error test which roughly requires that
  !                        ABS(local error) <= ETOL
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
  !             (approximately) the smallest positive real number such
  !             that  (1.+SMALL) > 1.  on the machine being used. the
  !             quantity  SMALL**(3/8)  is used in computing increments of
  !             variables for approximating derivatives by differences.
  !             also the algorithm will not compute a starting step length
  !             which is smaller than  100*SMALL*ABS(A).
  !
  !      BIG -- This is a large positive machine dependent constant which
  !             is used for preventing machine overflows.  A reasonable
  !             choice is to set big to (approximately) the square root of
  !             the largest real number which can be held in the machine.
  !
  !      SPY(*),PV(*),YP(*),SF(*) -- These are real work arrays of length
  !             NEQ which provide the routine with needed storage space.
  !
  !      RPAR,IPAR -- These are parameter arrays, of real and integer
  !             type, respectively, which can be used for communication
  !             between your program and the F subroutine.  They are not
  !             used or altered by HSTART.
  !
  !- *********************************************************************
  !  On Output  (after the return from HSTART),
  !
  !      H -- Is an appropriate starting step size to be attempted by the
  !             differential equation method.
  !
  !           All parameters in the call list remain unchanged except for
  !           the working arrays SPY(*),PV(*),YP(*) and SF(*).
  !
  !- *********************************************************************
  !
  !***
  ! **See also:**  DEABM, DEBDF, DERKF
  !***
  ! **Routines called:**  HVNRM

  !* REVISION HISTORY  (YYMMDD)
  !   800501  DATE WRITTEN
  !   890531  Changed all specific intrinsics to generic.  (WRB)
  !   891024  Changed references from VNORM to HVNRM.  (WRB)
  !   891024  REVISION DATE from Version 3.2
  !   891214  Prologue converted to Version 4.0 format.  (BAB)
  !   900328  Added TYPE section.  (WRB)
  !   910722  Updated AUTHOR section.  (ALS)

  INTERFACE
    SUBROUTINE F(X,U,Uprime)
      IMPORT SP
      REAL(SP), INTENT(IN) :: X
      REAL(SP), INTENT(IN) :: U(:)
      REAL(SP), INTENT(OUT) :: Uprime(:)
    END SUBROUTINE F
  END INTERFACE
  INTEGER, INTENT(IN) :: Morder, Neq
  REAL(SP), INTENT(IN) :: A, B, Big, Small
  REAL(SP), INTENT(OUT) :: H
  REAL(SP), INTENT(IN) :: Etol(Neq), Y(Neq), Yprime(Neq)
  REAL(SP), INTENT(OUT) :: Pv(Neq), Sf(Neq), Spy(Neq), Yp(Neq)
  !
  REAL(SP) :: absdx, da, delf, delx, delxb, dely, dfdub, dfdxb, dx, dy, fbnd, power, &
    relper, srydpb, wtj, ydpb, ynorm, ypnorm
  INTEGER :: icase, j, k, lk
  !
  !.......................................................................
  !
  !* FIRST EXECUTABLE STATEMENT  HSTART
  dx = B - A
  absdx = ABS(dx)
  relper = Small**0.375_SP
  ynorm = HVNRM(Y,Neq)
  !
  !.......................................................................
  !
  !     COMPUTE A WEIGHTED APPROXIMATE BOUND (DFDXB) ON THE PARTIAL
  !     DERIVATIVE OF THE EQUATION WITH RESPECT TO THE
  !     INDEPENDENT VARIABLE. PROTECT AGAINST AN OVERFLOW. ALSO
  !     COMPUTE A WEIGHTED BOUND (FBND) ON THE FIRST DERIVATIVE LOCALLY.
  !
  da = SIGN(MAX(MIN(relper*ABS(A),absdx),100._SP*Small*ABS(A)),dx)
  IF( da==0. ) da = relper*dx
  CALL F(A+da,Y,Sf)
  !
  IF( Morder==1 ) THEN
    !
    DO j = 1, Neq
      Spy(j) = Sf(j)/Etol(j)
      Yp(j) = Yprime(j)/Etol(j)
      Pv(j) = Spy(j) - Yp(j)
    END DO
  ELSE
    power = 2._SP/(Morder+1)
    DO j = 1, Neq
      wtj = Etol(j)**power
      Spy(j) = Sf(j)/wtj
      Yp(j) = Yprime(j)/wtj
      Pv(j) = Spy(j) - Yp(j)
    END DO
  END IF
  !
  delf = HVNRM(Pv,Neq)
  dfdxb = Big
  IF( delf<Big*ABS(da) ) dfdxb = delf/ABS(da)
  ypnorm = HVNRM(Yp,Neq)
  fbnd = MAX(HVNRM(Spy,Neq),ypnorm)
  !
  !.......................................................................
  !
  !     COMPUTE AN ESTIMATE (DFDUB) OF THE LOCAL LIPSCHITZ CONSTANT FOR
  !     THE SYSTEM OF DIFFERENTIAL EQUATIONS. THIS ALSO REPRESENTS AN
  !     ESTIMATE OF THE NORM OF THE JACOBIAN LOCALLY.
  !     THREE ITERATIONS (TWO WHEN NEQ=1) ARE USED TO ESTIMATE THE
  !     LIPSCHITZ CONSTANT BY NUMERICAL DIFFERENCES. THE FIRST
  !     PERTURBATION VECTOR IS BASED ON THE INITIAL DERIVATIVES AND
  !     DIRECTION OF INTEGRATION. THE SECOND PERTURBATION VECTOR IS
  !     FORMED USING ANOTHER EVALUATION OF THE DIFFERENTIAL EQUATION.
  !     THE THIRD PERTURBATION VECTOR IS FORMED USING PERTURBATIONS BASED
  !     ONLY ON THE INITIAL VALUES. COMPONENTS THAT ARE ZERO ARE ALWAYS
  !     CHANGED TO NON-ZERO VALUES (EXCEPT ON THE FIRST ITERATION). WHEN
  !     INFORMATION IS AVAILABLE, CARE IS TAKEN TO ENSURE THAT COMPONENTS
  !     OF THE PERTURBATION VECTOR HAVE SIGNS WHICH ARE CONSISTENT WITH
  !     THE SLOPES OF LOCAL SOLUTION CURVES.
  !     ALSO CHOOSE THE LARGEST BOUND (FBND) FOR THE FIRST DERIVATIVE.
  !     NO ATTEMPT IS MADE TO KEEP THE PERTURBATION VECTOR SIZE CONSTANT.
  !
  IF( ypnorm==0. ) THEN
    !                       CANNOT HAVE A NULL PERTURBATION VECTOR
    icase = 2
    DO j = 1, Neq
      Spy(j) = Yprime(j)
      Yp(j) = Etol(j)
    END DO
  ELSE
    !                       USE INITIAL DERIVATIVES FOR FIRST PERTURBATION
    icase = 1
    DO j = 1, Neq
      Spy(j) = Yprime(j)
      Yp(j) = Yprime(j)
    END DO
  END IF
  !
  dfdub = 0._SP
  lk = MIN(Neq+1,3)
  DO k = 1, lk
    !                       SET YPNORM AND DELX
    ypnorm = HVNRM(Yp,Neq)
    IF( icase==1 .OR. icase==3 ) THEN
      !                       TRY TO ENFORCE MEANINGFUL PERTURBATION VALUES
      delx = dx
      IF( ABS(delx)*ypnorm<relper*ynorm ) THEN
        delxb = Big
        IF( relper*ynorm<Big*ypnorm ) delxb = relper*ynorm/ypnorm
        delx = SIGN(delxb,dx)
      END IF
      DO j = 1, Neq
        IF( ABS(delx*Yp(j))>Etol(j) ) delx = SIGN(Etol(j)/Yp(j),dx)
      END DO
    ELSE
      delx = SIGN(1._SP,dx)
    END IF
    !                       DEFINE PERTURBED VECTOR OF INITIAL VALUES
    DO j = 1, Neq
      Pv(j) = Y(j) + delx*Yp(j)
    END DO
    IF( k==2 ) THEN
      !                       USE A SHIFTED VALUE OF THE INDEPENDENT VARIABLE
      !                                             IN COMPUTING ONE ESTIMATE
      CALL F(A+da,Pv,Yp)
      DO j = 1, Neq
        Pv(j) = Yp(j) - Sf(j)
      END DO
    ELSE
      !                       EVALUATE DERIVATIVES ASSOCIATED WITH PERTURBED
      !                       VECTOR  AND  COMPUTE CORRESPONDING DIFFERENCES
      CALL F(A,Pv,Yp)
      DO j = 1, Neq
        Pv(j) = Yp(j) - Yprime(j)
      END DO
    END IF
    !                       CHOOSE LARGEST BOUND ON THE WEIGHTED FIRST
    !                                                   DERIVATIVE
    IF( Morder==1 ) THEN
      DO j = 1, Neq
        Yp(j) = Yp(j)/Etol(j)
      END DO
    ELSE
      DO j = 1, Neq
        Yp(j) = Yp(j)/Etol(j)**power
      END DO
    END IF
    fbnd = MAX(fbnd,HVNRM(Yp,Neq))
    !                       COMPUTE BOUND ON A LOCAL LIPSCHITZ CONSTANT
    delf = HVNRM(Pv,Neq)
    IF( delf/=0. ) THEN
      dely = ABS(delx)*ypnorm
      IF( delf>=Big*dely ) EXIT
      dfdub = MAX(dfdub,delf/dely)
    END IF
    !
    IF( k==lk ) GOTO 100
    !                       CHOOSE NEXT PERTURBATION VECTOR
    DO j = 1, Neq
      IF( k==lk-1 ) THEN
        icase = 4
        dy = MAX(relper*ABS(Y(j)),Etol(j))
      ELSE
        icase = 3
        dy = ABS(Pv(j))
        IF( dy==0. ) dy = MAX(delf,Etol(j))
      END IF
      IF( Spy(j)==0. ) Spy(j) = Yp(j)
      IF( Spy(j)/=0. ) dy = SIGN(dy,Spy(j))
      Yp(j) = dy
    END DO
  END DO
  !
  !                       PROTECT AGAINST AN OVERFLOW
  dfdub = Big
  !
  !.......................................................................
  !
  !     COMPUTE A BOUND (YDPB) ON THE NORM OF THE SECOND DERIVATIVE
  !
  100  ydpb = dfdxb + dfdub*fbnd
  !
  !.......................................................................
  !
  !     COMPUTE A STARTING STEP SIZE BASED ON THE ABOVE FIRST AND SECOND
  !     DERIVATIVE INFORMATION
  !
  !                       RESTRICT THE STEP LENGTH TO BE NOT BIGGER THAN
  !                       ABS(B-A).   (UNLESS  B  IS TOO CLOSE TO  A)
  H = absdx
  !
  IF( ydpb/=0. .OR. fbnd/=0. ) THEN
    !
    IF( ydpb/=0. ) THEN
      !
      !                       SECOND DERIVATIVE TERM (YDPB) IS NON-ZERO
      srydpb = SQRT(0.5_SP*ydpb)
      IF( 1.0<srydpb*absdx ) H = 1._SP/srydpb
    ELSE
      !
      !                       ONLY SECOND DERIVATIVE TERM (YDPB) IS ZERO
      IF( 1.0<fbnd*absdx ) H = 1._SP/fbnd
    END IF
    !
    !                       BOTH FIRST DERIVATIVE TERM (FBND) AND SECOND
    !                                    DERIVATIVE TERM (YDPB) ARE ZERO
  END IF
  !
  !                       FURTHER RESTRICT THE STEP LENGTH TO BE NOT
  !                                                 BIGGER THAN  1/DFDUB
  IF( H*dfdub>1. ) H = 1._SP/dfdub
  !
  !                       FINALLY, RESTRICT THE STEP LENGTH TO BE NOT
  !                       SMALLER THAN  100*SMALL*ABS(A).  HOWEVER, IF
  !                       A=0. AND THE COMPUTED H UNDERFLOWED TO ZERO,
  !                       THE ALGORITHM RETURNS  SMALL*ABS(B)  FOR THE
  !                                                       STEP LENGTH.
  H = MAX(H,100._SP*Small*ABS(A))
  IF( H==0. ) H = Small*ABS(B)
  !
  !                       NOW SET DIRECTION OF INTEGRATION
  H = SIGN(H,dx)
  !
END SUBROUTINE HSTART