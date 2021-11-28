!** STEPS
SUBROUTINE STEPS(F,Neqn,Y,X,H,Eps,Wt,Start,Hold,K,Kold,Crash,Phi,P,Yp,Psi,&
    Alpha,Beta,Sig,V,W,G,Phase1,Ns,Nornd,Ksteps,Twou,Fouru,&
    Xold,Kprev,Ivc,Iv,Kgi,Gi)
  !> Integrate a system of first order ordinary differential
  !            equations one step.
  !***
  ! **Library:**   SLATEC (DEPAC)
  !***
  ! **Category:**  I1A1B
  !***
  ! **Type:**      SINGLE PRECISION (STEPS-S, DSTEPS-D)
  !***
  ! **Keywords:**  ADAMS METHOD, DEPAC, INITIAL VALUE PROBLEMS, ODE,
  !             ORDINARY DIFFERENTIAL EQUATIONS, PREDICTOR-CORRECTOR
  !***
  ! **Author:**  Shampine, L. F., (SNLA)
  !           Gordon, M. K., (SNLA)
  !             MODIFIED BY H.A. WATTS
  !***
  ! **Description:**
  !
  !   Written by L. F. Shampine and M. K. Gordon
  !
  !   Abstract
  !
  !   Subroutine  STEPS  is normally used indirectly through subroutine
  !   DEABM .  Because  DEABM  suffices for most problems and is much
  !   easier to use, using it should be considered before using  STEPS
  !   alone.
  !
  !   Subroutine STEPS integrates a system of  NEQN  first order ordinary
  !   differential equations one step, normally from X to X+H, using a
  !   modified divided difference form of the Adams Pece formulas.  Local
  !   extrapolation is used to improve absolute stability and accuracy.
  !   The code adjusts its order and step size to control the local error
  !   per unit step in a generalized sense.  Special devices are included
  !   to control roundoff error and to detect when the user is requesting
  !   too much accuracy.
  !
  !   This code is completely explained and documented in the text,
  !   Computer Solution of Ordinary Differential Equations, The Initial
  !   Value Problem  by L. F. Shampine and M. K. Gordon.
  !   Further details on use of this code are available in "Solving
  !   Ordinary Differential Equations with ODE, STEP, and INTRP",
  !   by L. F. Shampine and M. K. Gordon, SLA-73-1060.
  !
  !
  !   The parameters represent --
  !      F -- subroutine to evaluate derivatives
  !      NEQN -- number of equations to be integrated
  !      Y(*) -- solution vector at X
  !      X -- independent variable
  !      H -- appropriate step size for next step.  Normally determined by
  !           code
  !      EPS -- local error tolerance
  !      WT(*) -- vector of weights for error criterion
  !      START -- logical variable set .TRUE. for first step,  .FALSE.
  !           otherwise
  !      HOLD -- step size used for last successful step
  !      K -- appropriate order for next step (determined by code)
  !      KOLD -- order used for last successful step
  !      CRASH -- logical variable set .TRUE. when no step can be taken,
  !           .FALSE. otherwise.
  !      YP(*) -- derivative of solution vector at  X  after successful
  !           step
  !      KSTEPS -- counter on attempted steps
  !      TWOU -- 2.*U where U is machine unit roundoff quantity
  !      FOURU -- 4.*U where U is machine unit roundoff quantity
  !      RPAR,IPAR -- parameter arrays which you may choose to use
  !            for communication between your program and subroutine F.
  !            They are not altered or used by STEPS.
  !   The variables X,XOLD,KOLD,KGI and IVC and the arrays Y,PHI,ALPHA,G,
  !   W,P,IV and GI are required for the interpolation subroutine SINTRP.
  !   The remaining variables and arrays are included in the call list
  !   only to eliminate local retention of variables between calls.
  !
  !   Input to STEPS
  !
  !      First call --
  !
  !   The user must provide storage in his calling program for all arrays
  !   in the call list, namely
  !
  !     DIMENSION Y(NEQN),WT(NEQN),PHI(NEQN,16),P(NEQN),YP(NEQN),PSI(12),
  !    1  ALPHA(12),BETA(12),SIG(13),V(12),W(12),G(13),GI(11),IV(10),
  !    2  RPAR(*),IPAR(*)
  !
  !    **Note**
  !
  !   The user must also declare  START,  CRASH,  PHASE1  and  NORND
  !   logical variables and  F  an EXTERNAL subroutine, supply the
  !   subroutine  F(X,Y,YP)  to evaluate
  !      DY(I)/DX = YP(I) = F(X,Y(1),Y(2),...,Y(NEQN))
  !   and initialize only the following parameters.
  !      NEQN -- number of equations to be integrated
  !      Y(*) -- vector of initial values of dependent variables
  !      X -- initial value of the independent variable
  !      H -- nominal step size indicating direction of integration
  !           and maximum size of step.  Must be variable
  !      EPS -- local error tolerance per step.  Must be variable
  !      WT(*) -- vector of non-zero weights for error criterion
  !      START -- .TRUE.
  !      YP(*) -- vector of initial derivative values
  !      KSTEPS -- set KSTEPS to zero
  !      TWOU -- 2.*U where U is machine unit roundoff quantity
  !      FOURU -- 4.*U where U is machine unit roundoff quantity
  !   Define U to be the machine unit roundoff quantity by calling
  !   the function routine  R1MACH,  U = eps_sp, or by
  !   computing U so that U is the smallest positive number such
  !   that 1.0+U > 1.0.
  !
  !   STEPS  requires that the L2 norm of the vector with components
  !   LOCAL ERROR(L)/WT(L)  be less than  EPS  for a successful step.  The
  !   array  WT  allows the user to specify an error test appropriate
  !   for his problem.  For example,
  !      WT(L) = 1.0  specifies absolute error,
  !            = ABS(Y(L))  error relative to the most recent value of the
  !                 L-th component of the solution,
  !            = ABS(YP(L))  error relative to the most recent value of
  !                 the L-th component of the derivative,
  !            = MAX(WT(L),ABS(Y(L)))  error relative to the largest
  !                 magnitude of L-th component obtained so far,
  !            = ABS(Y(L))*RELERR/EPS + ABSERR/EPS  specifies a mixed
  !                 relative-absolute test where  RELERR  is relative
  !                 error,  ABSERR  is absolute error and  EPS =
  !                 MAX(RELERR,ABSERR) .
  !
  !      Subsequent calls --
  !
  !   Subroutine  STEPS  is designed so that all information needed to
  !   continue the integration, including the step size  H  and the order
  !   K, is returned with each step.  With the exception of the step
  !   size, the error tolerance, and the weights, none of the parameters
  !   should be altered.  The array  WT  must be updated after each step
  !   to maintain relative error tests like those above.  Normally the
  !   integration is continued just beyond the desired endpoint and the
  !   solution interpolated there with subroutine  SINTRP .  If it is
  !   impossible to integrate beyond the endpoint, the step size may be
  !   reduced to hit the endpoint since the code will not take a step
  !   larger than the  H  input.  Changing the direction of integration,
  !   i.e., the sign of  H, requires the user set  START = .TRUE. before
  !   calling  STEPS  again.  This is the only situation in which  START
  !   should be altered.
  !
  !   Output from STEPS
  !
  !      Successful Step --
  !
  !   The subroutine returns after each successful step with  START  and
  !   CRASH  set .FALSE. .  X  represents the independent variable
  !   advanced one step of length  HOLD  from its value on input and  Y
  !   the solution vector at the new value of  X .  All other parameters
  !   represent information corresponding to the new  X  needed to
  !   continue the integration.
  !
  !      Unsuccessful Step --
  !
  !   When the error tolerance is too small for the machine precision,
  !   the subroutine returns without taking a step and  CRASH = .TRUE. .
  !   An appropriate step size and error tolerance for continuing are
  !   estimated and all other information is restored as upon input
  !   before returning.  To continue with the larger tolerance, the user
  !   just calls the code again.  A restart is neither required nor
  !   desirable.
  !
  !***
  ! **References:**  L. F. Shampine and M. K. Gordon, Solving ordinary
  !                 differential equations with ODE, STEP, and INTRP,
  !                 Report SLA-73-1060, Sandia Laboratories, 1973.
  !***
  ! **Routines called:**  HSTART, R1MACH

  !* REVISION HISTORY  (YYMMDD)
  !   740101  DATE WRITTEN
  !   890531  Changed all specific intrinsics to generic.  (WRB)
  !   890831  Modified array declarations.  (WRB)
  !   890831  REVISION DATE from Version 3.2
  !   891214  Prologue converted to Version 4.0 format.  (BAB)
  !   920501  Reformatted the REFERENCES section.  (WRB)
  USE service, ONLY : eps_sp, huge_sp
  !
  INTERFACE
    SUBROUTINE F(X,U,Uprime)
      IMPORT SP
      REAL(SP), INTENT(IN) :: X
      REAL(SP), INTENT(IN) :: U(:)
      REAL(SP), INTENT(OUT) :: Uprime(:)
    END SUBROUTINE F
  END INTERFACE
  INTEGER, INTENT(IN) :: Neqn
  INTEGER, INTENT(INOUT) :: Ivc, K, Kgi, Kold, Kprev, Ksteps, Ns
  INTEGER, INTENT(INOUT) :: Iv(10)
  REAL(SP), INTENT(IN) :: Fouru, Twou
  REAL(SP), INTENT(INOUT) :: Eps, H, Hold, X, Xold
  REAL(SP), INTENT(IN) :: Wt(Neqn)
  REAL(SP), INTENT(INOUT) :: Alpha(12), Beta(12), G(13), Gi(11), P(Neqn), &
    Phi(Neqn,16), Psi(12), Sig(13), V(12), W(12), Y(Neqn), Yp(Neqn)
  LOGICAL, INTENT(INOUT) :: Start, Crash, Phase1, Nornd
  !
  INTEGER :: i, ifail, im1, ip1, iq, j, jv, km1, km2, knew, kp1, kp2, l, limit1, &
    limit2, nsm2, nsp1, nsp2
  REAL(SP) :: absh, big, erk, erkm1, erkm2, erkp1, err, hnew, p5eps, r, reali, realns, &
    rho, round, tau, temp1, temp2, temp3, temp4, temp5, temp6, u
  !
  REAL(SP), PARAMETER :: two(13) = [ 2._SP, 4._SP, 8._SP, 16._SP, 32._SP, 64._SP, &
    128._SP, 256._SP, 512._SP, 1024._SP, 2048._SP, 4096._SP, 8192._SP ]
  REAL(SP), PARAMETER :: gstr(13) = [ 0.500_SP, 0.0833_SP, 0.0417_SP, 0.0264_SP, &
    0.0188_SP, 0.0143_SP, 0.0114_SP, 0.00936_SP, 0.00789_SP, 0.00679_SP, 0.00592_SP, &
    0.00524_SP, 0.00468_SP ]
  !
  !
  !       ***     BEGIN BLOCK 0     ***
  !   CHECK IF STEP SIZE OR ERROR TOLERANCE IS TOO SMALL FOR MACHINE
  !   PRECISION.  IF FIRST STEP, INITIALIZE PHI ARRAY AND ESTIMATE A
  !   STARTING STEP SIZE.
  !                   ***
  !
  !   IF STEP SIZE IS TOO SMALL, DETERMINE AN ACCEPTABLE ONE
  !
  !* FIRST EXECUTABLE STATEMENT  STEPS
  Crash = .TRUE.
  IF( ABS(H)>=Fouru*ABS(X) ) THEN
    p5eps = 0.5_SP*Eps
    !
    !   IF ERROR TOLERANCE IS TOO SMALL, INCREASE IT TO AN ACCEPTABLE VALUE
    !
    round = 0._SP
    DO l = 1, Neqn
      round = round + (Y(l)/Wt(l))**2
    END DO
    round = Twou*SQRT(round)
    IF( p5eps>=round ) THEN
      Crash = .FALSE.
      G(1) = 1._SP
      G(2) = 0.5
      Sig(1) = 1._SP
      IF( Start ) THEN
        !
        !   INITIALIZE.  COMPUTE APPROPRIATE STEP SIZE FOR FIRST STEP
        !
        !     CALL F(X,Y,YP,RPAR,IPAR)
        !     SUM = 0.0
        DO l = 1, Neqn
          Phi(l,1) = Yp(l)
          Phi(l,2) = 0._SP
        END DO
        !20     SUM = SUM + (YP(L)/WT(L))**2
        !     SUM = SQRT(SUM)
        !     ABSH = ABS(H)
        !     IF(EPS < 16.0*SUM*H*H) ABSH = 0.25*SQRT(EPS/SUM)
        !     H = SIGN(MAX(ABSH,FOURU*ABS(X)),H)
        !
        u = eps_sp
        big = SQRT(huge_sp)
        CALL HSTART(F,Neqn,X,X+H,Y,Yp,Wt,1,u,big,Phi(1,3),Phi(1,4),Phi(1,5),&
          Phi(1,6),H)
        !
        Hold = 0._SP
        K = 1
        Kold = 0
        Kprev = 0
        Start = .FALSE.
        Phase1 = .TRUE.
        Nornd = .TRUE.
        IF( p5eps<=100._SP*round ) THEN
          Nornd = .FALSE.
          DO l = 1, Neqn
            Phi(l,15) = 0._SP
          END DO
        END IF
      END IF
      ifail = 0
    ELSE
      Eps = 2._SP*round*(1._SP+Fouru)
      RETURN
    END IF
  ELSE
    H = SIGN(Fouru*ABS(X),H)
    RETURN
  END IF
  !       ***     END BLOCK 0     ***
  !
  !       ***     BEGIN BLOCK 1     ***
  !   COMPUTE COEFFICIENTS OF FORMULAS FOR THIS STEP.  AVOID COMPUTING
  !   THOSE QUANTITIES NOT CHANGED WHEN STEP SIZE IS NOT CHANGED.
  !                   ***
  !
  100  kp1 = K + 1
  kp2 = K + 2
  km1 = K - 1
  km2 = K - 2
  !
  !   NS IS THE NUMBER OF STEPS TAKEN WITH SIZE H, INCLUDING THE CURRENT
  !   ONE.  WHEN K<NS, NO COEFFICIENTS CHANGE
  !
  IF( H/=Hold ) Ns = 0
  IF( Ns<=Kold ) Ns = Ns + 1
  nsp1 = Ns + 1
  IF( K>=Ns ) THEN
    !
    !   COMPUTE THOSE COMPONENTS OF ALPHA(*),BETA(*),PSI(*),SIG(*) WHICH
    !   ARE CHANGED
    !
    Beta(Ns) = 1._SP
    realns = Ns
    Alpha(Ns) = 1._SP/realns
    temp1 = H*realns
    Sig(nsp1) = 1._SP
    IF( K>=nsp1 ) THEN
      DO i = nsp1, K
        im1 = i - 1
        temp2 = Psi(im1)
        Psi(im1) = temp1
        Beta(i) = Beta(im1)*Psi(im1)/temp2
        temp1 = temp2 + H
        Alpha(i) = H/temp1
        reali = i
        Sig(i+1) = reali*Alpha(i)*Sig(i)
      END DO
    END IF
    Psi(K) = temp1
    !
    !   COMPUTE COEFFICIENTS G(*)
    !
    !   INITIALIZE V(*) AND SET W(*).
    !
    IF( Ns>1 ) THEN
      !
      !   IF ORDER WAS RAISED, UPDATE DIAGONAL PART OF V(*)
      !
      IF( K>Kprev ) THEN
        IF( Ivc==0 ) THEN
          jv = 1
          temp4 = K*kp1
          V(K) = 1._SP/temp4
          W(K) = V(K)
          IF( K==2 ) THEN
            Kgi = 1
            Gi(1) = W(2)
          END IF
        ELSE
          jv = kp1 - Iv(Ivc)
          Ivc = Ivc - 1
        END IF
        nsm2 = Ns - 2
        IF( nsm2>=jv ) THEN
          DO j = jv, nsm2
            i = K - j
            V(i) = V(i) - Alpha(j+1)*V(i+1)
            W(i) = V(i)
          END DO
          IF( i==2 ) THEN
            Kgi = Ns - 1
            Gi(Kgi) = W(2)
          END IF
        END IF
      END IF
      !
      !   UPDATE V(*) AND SET W(*)
      !
      limit1 = kp1 - Ns
      temp5 = Alpha(Ns)
      DO iq = 1, limit1
        V(iq) = V(iq) - temp5*V(iq+1)
        W(iq) = V(iq)
      END DO
      G(nsp1) = W(1)
      IF( limit1/=1 ) THEN
        Kgi = Ns
        Gi(Kgi) = W(2)
      END IF
      W(limit1+1) = V(limit1+1)
      IF( K<Kold ) THEN
        Ivc = Ivc + 1
        Iv(Ivc) = limit1 + 2
      END IF
    ELSE
      DO iq = 1, K
        temp3 = iq*(iq+1)
        V(iq) = 1._SP/temp3
        W(iq) = V(iq)
      END DO
      Ivc = 0
      Kgi = 0
      IF( K/=1 ) THEN
        Kgi = 1
        Gi(1) = W(2)
      END IF
    END IF
    !
    !   COMPUTE THE G(*) IN THE WORK VECTOR W(*)
    !
    nsp2 = Ns + 2
    Kprev = K
    IF( kp1>=nsp2 ) THEN
      DO i = nsp2, kp1
        limit2 = kp2 - i
        temp6 = Alpha(i-1)
        DO iq = 1, limit2
          W(iq) = W(iq) - temp6*W(iq+1)
        END DO
        G(i) = W(1)
      END DO
    END IF
  END IF
  !       ***     END BLOCK 1     ***
  !
  !       ***     BEGIN BLOCK 2     ***
  !   PREDICT A SOLUTION P(*), EVALUATE DERIVATIVES USING PREDICTED
  !   SOLUTION, ESTIMATE LOCAL ERROR AT ORDER K AND ERRORS AT ORDERS K,
  !   K-1, K-2 AS IF CONSTANT STEP SIZE WERE USED.
  !                   ***
  !
  !   INCREMENT COUNTER ON ATTEMPTED STEPS
  !
  Ksteps = Ksteps + 1
  !
  !   CHANGE PHI TO PHI STAR
  !
  IF( K>=nsp1 ) THEN
    DO i = nsp1, K
      temp1 = Beta(i)
      DO l = 1, Neqn
        Phi(l,i) = temp1*Phi(l,i)
      END DO
    END DO
  END IF
  !
  !   PREDICT SOLUTION AND DIFFERENCES
  !
  DO l = 1, Neqn
    Phi(l,kp2) = Phi(l,kp1)
    Phi(l,kp1) = 0._SP
    P(l) = 0._SP
  END DO
  DO j = 1, K
    i = kp1 - j
    ip1 = i + 1
    temp2 = G(i)
    DO l = 1, Neqn
      P(l) = P(l) + temp2*Phi(l,i)
      Phi(l,i) = Phi(l,i) + Phi(l,ip1)
    END DO
  END DO
  IF( Nornd ) THEN
    DO l = 1, Neqn
      P(l) = Y(l) + H*P(l)
    END DO
  ELSE
    DO l = 1, Neqn
      tau = H*P(l) - Phi(l,15)
      P(l) = Y(l) + tau
      Phi(l,16) = (P(l)-Y(l)) - tau
    END DO
  END IF
  Xold = X
  X = X + H
  absh = ABS(H)
  CALL F(X,P,Yp)
  !
  !   ESTIMATE ERRORS AT ORDERS K,K-1,K-2
  !
  erkm2 = 0._SP
  erkm1 = 0._SP
  erk = 0._SP
  DO l = 1, Neqn
    temp3 = 1._SP/Wt(l)
    temp4 = Yp(l) - Phi(l,1)
    IF( km2<0 ) GOTO 150
    IF( km2/=0 ) erkm2 = erkm2 + ((Phi(l,km1)+temp4)*temp3)**2
    erkm1 = erkm1 + ((Phi(l,K)+temp4)*temp3)**2
    150  erk = erk + (temp4*temp3)**2
  END DO
  IF( km2<0 ) GOTO 200
  IF( km2/=0 ) erkm2 = absh*Sig(km1)*gstr(km2)*SQRT(erkm2)
  erkm1 = absh*Sig(K)*gstr(km1)*SQRT(erkm1)
  200  temp5 = absh*SQRT(erk)
  err = temp5*(G(K)-G(kp1))
  erk = temp5*Sig(kp1)*gstr(K)
  knew = K
  !
  !   TEST IF ORDER SHOULD BE LOWERED
  !
  IF( km2<0 ) THEN
  ELSEIF( km2==0 ) THEN
    IF( erkm1<=0.5_SP*erk ) knew = km1
  ELSE
    IF( MAX(erkm1,erkm2)<=erk ) knew = km1
  END IF
  !
  !   TEST IF STEP SUCCESSFUL
  !
  IF( err<=Eps ) THEN
    !       ***     END BLOCK 3     ***
    !
    !       ***     BEGIN BLOCK 4     ***
    !   THE STEP IS SUCCESSFUL.  CORRECT THE PREDICTED SOLUTION, EVALUATE
    !   THE DERIVATIVES USING THE CORRECTED SOLUTION AND UPDATE THE
    !   DIFFERENCES.  DETERMINE BEST ORDER AND STEP SIZE FOR NEXT STEP.
    !                   ***
    Kold = K
    Hold = H
    !
    !   CORRECT AND EVALUATE
    !
    temp1 = H*G(kp1)
    IF( Nornd ) THEN
      DO l = 1, Neqn
        temp3 = Y(l)
        Y(l) = P(l) + temp1*(Yp(l)-Phi(l,1))
        P(l) = temp3
      END DO
    ELSE
      DO l = 1, Neqn
        temp3 = Y(l)
        rho = temp1*(Yp(l)-Phi(l,1)) - Phi(l,16)
        Y(l) = P(l) + rho
        Phi(l,15) = (Y(l)-P(l)) - rho
        P(l) = temp3
      END DO
    END IF
    CALL F(X,Y,Yp)
    !
    !   UPDATE DIFFERENCES FOR NEXT STEP
    !
    DO l = 1, Neqn
      Phi(l,kp1) = Yp(l) - Phi(l,1)
      Phi(l,kp2) = Phi(l,kp1) - Phi(l,kp2)
    END DO
    DO i = 1, K
      DO l = 1, Neqn
        Phi(l,i) = Phi(l,i) + Phi(l,kp1)
      END DO
    END DO
    !
    !   ESTIMATE ERROR AT ORDER K+1 UNLESS:
    !     IN FIRST PHASE WHEN ALWAYS RAISE ORDER,
    !     ALREADY DECIDED TO LOWER ORDER,
    !     STEP SIZE NOT CONSTANT SO ESTIMATE UNRELIABLE
    !
    erkp1 = 0._SP
    IF( knew==km1 .OR. K==12 ) Phase1 = .FALSE.
    IF( .NOT. (Phase1) ) THEN
      IF( knew==km1 ) GOTO 300
      IF( kp1>Ns ) GOTO 400
      DO l = 1, Neqn
        erkp1 = erkp1 + (Phi(l,kp2)/Wt(l))**2
      END DO
      erkp1 = absh*gstr(kp1)*SQRT(erkp1)
      !
      !   USING ESTIMATED ERROR AT ORDER K+1, DETERMINE APPROPRIATE ORDER
      !   FOR NEXT STEP
      !
      IF( K>1 ) THEN
        IF( erkm1<=MIN(erk,erkp1) ) GOTO 300
        IF( erkp1>=erk .OR. K==12 ) GOTO 400
      ELSEIF( erkp1>=0.5_SP*erk ) THEN
        GOTO 400
      END IF
    END IF
    !
    !   HERE ERKP1 < ERK < MAX(ERKM1,ERKM2) ELSE ORDER WOULD HAVE
    !   BEEN LOWERED IN BLOCK 2.  THUS ORDER IS TO BE RAISED
    !
    !   RAISE ORDER
    !
    K = kp1
    erk = erkp1
    GOTO 400
  ELSE
    !       ***     END BLOCK 2     ***
    !
    !       ***     BEGIN BLOCK 3     ***
    !   THE STEP IS UNSUCCESSFUL.  RESTORE  X, PHI(*,*), PSI(*) .
    !   IF THIRD CONSECUTIVE FAILURE, SET ORDER TO ONE.  IF STEP FAILS MORE
    !   THAN THREE TIMES, CONSIDER AN OPTIMAL STEP SIZE.  DOUBLE ERROR
    !   TOLERANCE AND RETURN IF ESTIMATED STEP SIZE IS TOO SMALL FOR MACHINE
    !   PRECISION.
    !                   ***
    !
    !   RESTORE X, PHI(*,*) AND PSI(*)
    !
    Phase1 = .FALSE.
    X = Xold
    DO i = 1, K
      temp1 = 1._SP/Beta(i)
      ip1 = i + 1
      DO l = 1, Neqn
        Phi(l,i) = temp1*(Phi(l,i)-Phi(l,ip1))
      END DO
    END DO
    IF( K>=2 ) THEN
      DO i = 2, K
        Psi(i-1) = Psi(i) - H
      END DO
    END IF
    !
    !   ON THIRD FAILURE, SET ORDER TO ONE.  THEREAFTER, USE OPTIMAL STEP
    !   SIZE
    !
    ifail = ifail + 1
    temp2 = 0.5
    IF( ifail<3 ) GOTO 250
    IF( ifail/=3 ) THEN
      IF( p5eps<0.25_SP*erk ) temp2 = SQRT(p5eps/erk)
    END IF
    knew = 1
    250  H = temp2*H
    K = knew
    Ns = 0
    IF( ABS(H)>=Fouru*ABS(X) ) GOTO 100
    Crash = .TRUE.
    H = SIGN(Fouru*ABS(X),H)
    Eps = Eps + Eps
    RETURN
  END IF
  !
  !   LOWER ORDER
  !
  300  K = km1
  erk = erkm1
  !
  !   WITH NEW ORDER DETERMINE APPROPRIATE STEP SIZE FOR NEXT STEP
  !
  400  hnew = H + H
  IF( .NOT. (Phase1) ) THEN
    IF( p5eps<erk*two(K+1) ) THEN
      hnew = H
      IF( p5eps<erk ) THEN
        temp2 = K + 1
        r = (p5eps/erk)**(1._SP/temp2)
        hnew = absh*MAX(0.5_SP,MIN(0.9_SP,r))
        hnew = SIGN(MAX(hnew,Fouru*ABS(X)),H)
      END IF
    END IF
  END IF
  H = hnew
  !       ***     END BLOCK 4     ***
END SUBROUTINE STEPS