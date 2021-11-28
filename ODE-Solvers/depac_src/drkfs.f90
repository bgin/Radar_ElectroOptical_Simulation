!** DRKFS
SUBROUTINE DRKFS(DF,Neq,T,Y,Tout,Info,Rtol,Atol,Idid,H,Tolfac,Yp,F1,F2,F3,&
    F4,F5,Ys,Told,Dtsign,U26,Rer,Init,Ksteps,Kop,Iquit,Stiff,Nonstf,Ntstep,Nstifs)
  !> Subsidiary to DDERKF
  !***
  ! **Library:**   SLATEC
  !***
  ! **Type:**      DOUBLE PRECISION (DERKFS-S, DRKFS-D)
  !***
  ! **Author:**  Watts, H. A., (SNLA)
  !***
  ! **Description:**
  !
  !     Fehlberg Fourth-Fifth Order Runge-Kutta Method
  !- *********************************************************************
  !
  !     DRKFS integrates a system of first order ordinary differential
  !     equations as described in the comments for DDERKF .
  !
  !     The arrays YP,F1,F2,F3,F4,F5,and YS  (of length at least NEQ)
  !     appear in the call list for variable dimensioning purposes.
  !
  !     The variables H,TOLFAC,TOLD,DTSIGN,U26,RER,INIT,KSTEPS,KOP,IQUIT,
  !     STIFF,NONSTF,NTSTEP, and NSTIFS are used internally by the code
  !     and appear in the call list to eliminate local retention of
  !     variables between calls. Accordingly, these variables and the
  !     array YP should not be altered.
  !     Items of possible interest are
  !         H  - An appropriate step size to be used for the next step
  !         TOLFAC - Factor of change in the tolerances
  !         YP - Derivative of solution vector at T
  !         KSTEPS - Counter on the number of steps attempted
  !
  !- *********************************************************************
  !
  !***
  ! **See also:**  DDERKF
  !***
  ! **Routines called:**  D1MACH, DFEHL, DHSTRT, DHVNRM, XERMSG

  !* REVISION HISTORY  (YYMMDD)
  !   820301  DATE WRITTEN
  !   890531  Changed all specific intrinsics to generic.  (WRB)
  !   890831  Modified array declarations.  (WRB)
  !   891024  Changed references from DVNORM to DHVNRM.  (WRB)
  !   891214  Prologue converted to Version 4.0 format.  (BAB)
  !   900328  Added TYPE section.  (WRB)
  !   900510  Convert XERRWV calls to XERMSG calls, change GOTOs to IF-THEN-ELSEs.  (RWC)
  !   910722  Updated AUTHOR section.  (ALS)
  USE service, ONLY : eps_dp, huge_dp
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
  INTEGER, INTENT(INOUT) :: Init, Iquit, Kop, Ksteps, Nstifs, Ntstep
  INTEGER, INTENT(OUT) :: Idid
  INTEGER, INTENT(INOUT) :: Info(15)
  REAL(DP), INTENT(IN) :: Tout
  REAL(DP), INTENT(INOUT) :: Dtsign, H, Rer, T, Told, U26
  REAL(DP), INTENT(OUT) :: Tolfac
  REAL(DP), INTENT(INOUT) :: Atol(:), Rtol(:), Y(Neq)
  REAL(DP), INTENT(OUT) :: F1(Neq), F2(Neq), F3(Neq), F4(Neq), F5(Neq), &
    Yp(Neq), Ys(Neq)
  LOGICAL, INTENT(INOUT) :: Stiff, Nonstf
  !
  INTEGER :: k, ktol, natolp, nrtolp
  REAL(DP) :: a, big, dt, dy, ee, eeoet, es, estiff, esttol, et, hmin, s, &
    tol, u, ute, yavg
  LOGICAL :: hfaild, output
  CHARACTER(8) :: xern1
  CHARACTER(16) :: xern3, xern4
  !
  !     ..................................................................
  !
  !       A FIFTH ORDER METHOD WILL GENERALLY NOT BE CAPABLE OF DELIVERING
  !       ACCURACIES NEAR LIMITING PRECISION ON COMPUTERS WITH LONG
  !       WORDLENGTHS. TO PROTECT AGAINST LIMITING PRECISION DIFFICULTIES
  !       ARISING FROM UNREASONABLE ACCURACY REQUESTS, AN APPROPRIATE
  !       TOLERANCE THRESHOLD REMIN IS ASSIGNED FOR THIS METHOD. THIS
  !       VALUE SHOULD NOT BE CHANGED ACROSS DIFFERENT MACHINES.
  !
  REAL(DP), PARAMETER :: remin = 1.E-12_DP
  !
  !     ..................................................................
  !
  !       THE EXPENSE OF SOLVING THE PROBLEM IS MONITORED BY COUNTING THE
  !       NUMBER OF  STEPS ATTEMPTED. WHEN THIS EXCEEDS  MXSTEP, THE
  !       COUNTER IS RESET TO ZERO AND THE USER IS INFORMED ABOUT POSSIBLE
  !       EXCESSIVE WORK.
  !
  INTEGER, PARAMETER :: mxstep = 500
  !
  !     ..................................................................
  !
  !       INEFFICIENCY CAUSED BY TOO FREQUENT OUTPUT IS MONITORED BY
  !       COUNTING THE NUMBER OF STEP SIZES WHICH ARE SEVERELY SHORTENED
  !       DUE SOLELY TO THE CHOICE OF OUTPUT POINTS. WHEN THE NUMBER OF
  !       ABUSES EXCEED MXKOP, THE COUNTER IS RESET TO ZERO AND THE USER
  !       IS INFORMED ABOUT POSSIBLE MISUSE OF THE CODE.
  !
  INTEGER, PARAMETER :: mxkop = 100
  !
  !     ..................................................................
  !
  !* FIRST EXECUTABLE STATEMENT  DRKFS
  IF( Info(1)==0 ) THEN
    !
    ! ON THE FIRST CALL, PERFORM INITIALIZATION --
    !        DEFINE THE MACHINE UNIT ROUNDOFF QUANTITY  U  BY CALLING THE
    !        FUNCTION ROUTINE  D1MACH. THE USER MUST MAKE SURE THAT THE
    !        VALUES SET IN D1MACH ARE RELEVANT TO THE COMPUTER BEING USED.
    !
    u = eps_dp
    !  -- SET ASSOCIATED MACHINE DEPENDENT PARAMETERS
    U26 = 26._DP*u
    Rer = 2._DP*u + remin
    !  -- SET TERMINATION FLAG
    Iquit = 0
    !  -- SET INITIALIZATION INDICATOR
    Init = 0
    !  -- SET COUNTER FOR IMPACT OF OUTPUT POINTS
    Kop = 0
    !  -- SET COUNTER FOR ATTEMPTED STEPS
    Ksteps = 0
    !  -- SET INDICATORS FOR STIFFNESS DETECTION
    Stiff = .FALSE.
    Nonstf = .FALSE.
    !  -- SET STEP COUNTERS FOR STIFFNESS DETECTION
    Ntstep = 0
    Nstifs = 0
    !  -- RESET INFO(1) FOR SUBSEQUENT CALLS
    Info(1) = 1
  END IF
  !
  !.......................................................................
  !
  !        CHECK VALIDITY OF INPUT PARAMETERS ON EACH ENTRY
  !
  IF( Info(1)/=0 .AND. Info(1)/=1 ) THEN
    WRITE (xern1,'(I8)') Info(1)
    ERROR STOP 'DRKFS : IN DDERKF, INFO(1) MUST BE SET TO 0 FOR THE START OF&
      & A NEW PROBLEM, AND MUST BE SET TO 1 FOLLOWING AN INTERRUPTED TASK.&
      & YOU ARE ATTEMPTING TO CONTINUE THE INTEGRATION&
      & ILLEGALLY BY CALLING THE CODE WITH  INFO(1) /= 0 OR 1'
    Idid = -33
  END IF
  !
  IF( Info(2)/=0 .AND. Info(2)/=1 ) THEN
    WRITE (xern1,'(I8)') Info(2)
    ERROR STOP 'DRKFS : IN DDERKF, INFO(2) MUST BE 0 OR 1 INDICATING&
      & SCALAR AND VECTOR ERROR TOLERANCES, RESPECTIVELY.'
    Idid = -33
  END IF
  !
  IF( Info(3)/=0 .AND. Info(3)/=1 ) THEN
    WRITE (xern1,'(I8)') Info(3)
    ERROR STOP 'DRKFS : IN DDERKF, INFO(3) MUST BE 0 OR 1 INDICATING&
      & THE INTERVAL OR INTERMEDIATE-OUTPUT MODE OF INTEGRATION, RESPECTIVELY.'
    Idid = -33
  END IF
  !
  IF( Neq<1 ) THEN
    WRITE (xern1,'(I8)') Neq
    ERROR STOP 'DRKFS : IN DDERKF, THE NUMBER OF EQUATIONS NEQ MUST&
      & BE A POSITIVE INTEGER.'
    Idid = -33
  END IF
  !
  nrtolp = 0
  natolp = 0
  DO k = 1, Neq
    IF( nrtolp==0 .AND. Rtol(k)<0._DP ) THEN
      WRITE (xern1,'(I8)') k
      WRITE (xern3,'(1PE15.6)') Rtol(k)
      ERROR STOP 'DRKFS : IN DDERKF, THE RELATIVE ERROR TOLERANCES RTOL MUST BE&
        & NON-NEGATIVE. IN THE CASE OF VECTOR ERROR TOLERANCES, NO FURTHER CHECKING&
        & OF RTOL COMPONENTS IS DONE.'
      Idid = -33
      nrtolp = 1
    END IF
    !
    IF( natolp==0 .AND. Atol(k)<0._DP ) THEN
      WRITE (xern1,'(I8)') k
      WRITE (xern3,'(1PE15.6)') Atol(k)
      ERROR STOP 'DRKFS : IN DDERKF, THE ABSOLUTE ERROR TOLERANCES ATOL MUST BE&
        & NON-NEGATIVE. IN THE CASE OF VECTOR ERROR TOLERANCES, NO FURTHER CHECKING&
        & OF ATOL COMPONENTS IS DONE.'
      Idid = -33
      natolp = 1
    END IF
    !
    IF( Info(2)==0 ) EXIT
    IF( natolp>0 .AND. nrtolp>0 ) EXIT
  END DO
  !
  !
  !     CHECK SOME CONTINUATION POSSIBILITIES
  !
  IF( Init/=0 ) THEN
    IF( T==Tout ) THEN
      WRITE (xern3,'(1PE15.6)') T
      ERROR STOP 'DRKFS : IN DDERKF, YOU HAVE CALLED THE CODE WITH&
        & T = TOUT. THIS IS NOT ALLOWED ON CONTINUATION CALLS.'
      Idid = -33
    END IF
    !
    IF( T/=Told ) THEN
      WRITE (xern3,'(1PE15.6)') Told
      WRITE (xern4,'(1PE15.6)') T
      ERROR STOP 'DRKFS : IN DDERKF, YOU HAVE CHANGED THE VALUE OF T. &
        &THIS IS NOT ALLOWED ON CONTINUATION CALLS.'
      Idid = -33
    END IF
    !
    IF( Init/=1 ) THEN
      IF( Dtsign*(Tout-T)<0._DP ) THEN
        WRITE (xern3,'(1PE15.6)') Tout
        ERROR STOP 'DRKFS : INVALID VALUE OF TOUT. YOU ARE ATTEMPTING TO CHANGE&
          & THE DIRECTION OF INTEGRATION. THIS IS NOT ALLOWED WITHOUT RESTARTING.'
        Idid = -33
      END IF
    END IF
  END IF
  !
  !     INVALID INPUT DETECTED
  !
  IF( Idid==(-33) ) THEN
    IF( Iquit/=(-33) ) THEN
      Iquit = -33
      GOTO 200
    ELSE
      ERROR STOP 'DRKFS : IN DDERKF, INVALID INPUT WAS DETECTED ON&
        & SUCCESSIVE ENTRIES.  IT IS IMPOSSIBLE TO PROCEED BECAUSE YOU HAVE NOT&
        & CORRECTED THE PROBLEM, SO EXECUTION IS BEING TERMINATED.'
      RETURN
    END IF
  END IF
  !
  !           ............................................................
  !
  !                RTOL = ATOL = 0. IS ALLOWED AS VALID INPUT AND
  !                INTERPRETED AS ASKING FOR THE MOST ACCURATE SOLUTION
  !                POSSIBLE. IN THIS CASE, THE RELATIVE ERROR TOLERANCE
  !                RTOL IS RESET TO THE SMALLEST VALUE RER WHICH IS LIKELY
  !                TO BE REASONABLE FOR THIS METHOD AND MACHINE.
  !
  DO k = 1, Neq
    IF( Rtol(k)+Atol(k)<=0._DP ) THEN
      Rtol(k) = Rer
      Idid = -2
    END IF
    !           ...EXIT
    IF( Info(2)==0 ) EXIT
  END DO
  !
  IF( Idid/=(-2) ) THEN
    !
    !  BRANCH ON STATUS OF INITIALIZATION INDICATOR
    !   INIT=0 MEANS INITIAL DERIVATIVES AND
    !   STARTING STEP SIZE
    !          NOT YET COMPUTED
    !   INIT=1 MEANS STARTING STEP SIZE NOT YET
    !   COMPUTED INIT=2 MEANS NO FURTHER
    !   INITIALIZATION REQUIRED
    !
    IF( Init==0 ) THEN
      !
      !  ................................................
      !
      !       MORE INITIALIZATION --
      !  -- EVALUATE INITIAL
      !  DERIVATIVES
      !
      Init = 1
      a = T
      CALL DF(a,Y,Yp)
      IF( T==Tout ) THEN
        !
        !     INTERVAL MODE
        Idid = 2
        T = Tout
        Told = T
        !     .....................EXIT
        RETURN
      END IF
      !                    ......EXIT
    ELSEIF( Init/=1 ) THEN
      GOTO 100
      !                 .........EXIT
    END IF
    !
    !                    -- SET SIGN OF INTEGRATION DIRECTION  AND
    !                    -- ESTIMATE STARTING STEP SIZE
    !
    Init = 2
    Dtsign = SIGN(1._DP,Tout-T)
    u = eps_dp
    big = SQRT(huge_dp)
    ute = u**0.375_DP
    dy = ute*DHVNRM(Y,Neq)
    IF( dy==0._DP ) dy = ute
    ktol = 1
    DO k = 1, Neq
      IF( Info(2)==1 ) ktol = k
      tol = Rtol(ktol)*ABS(Y(k)) + Atol(ktol)
      IF( tol==0._DP ) tol = dy*Rtol(ktol)
      F1(k) = tol
    END DO
    !
    CALL DHSTRT(DF,Neq,T,Tout,Y,Yp,F1,4,u,big,F2,F3,F4,F5,H)
  ELSE
    !
    !  RTOL=ATOL=0 ON INPUT, SO RTOL WAS CHANGED TO A SMALL POSITIVE VALUE
    Tolfac = 1._DP
    GOTO 200
  END IF
  !
  !......................................................
  !
  ! SET STEP SIZE FOR INTEGRATION IN THE DIRECTION
  ! FROM T TO TOUT AND SET OUTPUT POINT INDICATOR
  !
  100  dt = Tout - T
  H = SIGN(H,dt)
  output = .FALSE.
  !
  !                 TEST TO SEE IF DDERKF IS BEING SEVERELY IMPACTED BY
  !                 TOO MANY OUTPUT POINTS
  !
  IF( ABS(H)>=2._DP*ABS(dt) ) Kop = Kop + 1
  IF( Kop<=mxkop ) THEN
    !
    IF( ABS(dt)>U26*ABS(T) ) THEN
      DO
        !  BEGIN BLOCK PERMITTING ...EXITS TO 490
        !
        !     *********************************************
        !     *********************************************
        !    STEP BY STEP INTEGRATION
        !
        !  BEGIN BLOCK PERMITTING ...EXITS TO 480
        hfaild = .FALSE.
        !
        !     TO PROTECT AGAINST IMPOSSIBLE ACCURACY
        !     REQUESTS, COMPUTE A TOLERANCE FACTOR
        !     BASED ON THE REQUESTED ERROR TOLERANCE
        !     AND A LEVEL OF ACCURACY ACHIEVABLE AT
        !     LIMITING PRECISION
        !
        Tolfac = 0._DP
        ktol = 1
        DO k = 1, Neq
          IF( Info(2)==1 ) ktol = k
          et = Rtol(ktol)*ABS(Y(k)) + Atol(ktol)
          IF( et>0._DP ) THEN
            Tolfac = MAX(Tolfac,ABS(Y(k))*(Rer/et))
          ELSE
            Tolfac = MAX(Tolfac,Rer/Rtol(ktol))
          END IF
        END DO
        IF( Tolfac<=1._DP ) THEN
          !
          !     SET SMALLEST ALLOWABLE STEP SIZE
          !
          hmin = U26*ABS(T)
          !
          !     ADJUST STEP SIZE IF NECESSARY TO HIT
          !     THE OUTPUT POINT -- LOOK AHEAD TWO
          !     STEPS TO AVOID DRASTIC CHANGES IN THE
          !     STEP SIZE AND THUS LESSEN THE IMPACT OF
          !     OUTPUT POINTS ON THE CODE.  STRETCH THE
          !     STEP SIZE BY, AT MOST, AN AMOUNT EQUAL
          !     TO THE SAFETY FACTOR OF 9/10.
          !
          dt = Tout - T
          IF( ABS(dt)<2._DP*ABS(H) ) THEN
            IF( ABS(dt)>ABS(H)/0.9_DP ) THEN
              !
              H = 0.5_DP*dt
            ELSE
              !
              !           THE NEXT STEP, IF SUCCESSFUL,
              !           WILL COMPLETE THE INTEGRATION TO
              !           THE OUTPUT POINT
              !
              output = .TRUE.
              H = dt
            END IF
          END IF
          !
          !
          !  ***************************************
          !       CORE INTEGRATOR FOR TAKING A
          !       SINGLE STEP
          !  ***************************************
          !       TO AVOID PROBLEMS WITH ZERO
          !       CROSSINGS, RELATIVE ERROR IS
          !       MEASURED USING THE AVERAGE OF THE
          !       MAGNITUDES OF THE SOLUTION AT THE
          !       BEGINNING AND END OF A STEP.
          !       THE ERROR ESTIMATE FORMULA HAS
          !       BEEN GROUPED TO CONTROL LOSS OF
          !       SIGNIFICANCE.
          !       LOCAL ERROR ESTIMATES FOR A FIRST
          !       ORDER METHOD USING THE SAME
          !       STEP SIZE AS THE FEHLBERG METHOD
          !       ARE CALCULATED AS PART OF THE
          !       TEST FOR STIFFNESS.
          !       TO DISTINGUISH THE VARIOUS
          !       ARGUMENTS, H IS NOT PERMITTED
          !       TO BECOME SMALLER THAN 26 UNITS OF
          !       ROUNDOFF IN T.  PRACTICAL LIMITS
          !       ON THE CHANGE IN THE STEP SIZE ARE
          !       ENFORCED TO SMOOTH THE STEP SIZE
          !       SELECTION PROCESS AND TO AVOID
          !       EXCESSIVE CHATTERING ON PROBLEMS
          !       HAVING DISCONTINUITIES.  TO
          !       PREVENT UNNECESSARY FAILURES, THE
          !       CODE USES 9/10 THE STEP SIZE
          !       IT ESTIMATES WILL SUCCEED.
          !       AFTER A STEP FAILURE, THE STEP
          !       SIZE IS NOT ALLOWED TO INCREASE
          !       FOR THE NEXT ATTEMPTED STEP. THIS
          !       MAKES THE CODE MORE EFFICIENT ON
          !       PROBLEMS HAVING DISCONTINUITIES
          !       AND MORE EFFECTIVE IN GENERAL
          !       SINCE LOCAL EXTRAPOLATION IS BEING
          !       USED AND EXTRA CAUTION SEEMS
          !       WARRANTED.
          !  .......................................
          !
          !       MONITOR NUMBER OF STEPS ATTEMPTED
          !
          DO WHILE( Ksteps<=mxstep )
            !
            !     ADVANCE AN APPROXIMATE SOLUTION OVER
            !     ONE STEP OF LENGTH H
            !
            CALL DFEHL(DF,Neq,T,Y,H,Yp,F1,F2,F3,F4,F5,Ys)
            Ksteps = Ksteps + 1
            !
            !     ....................................
            !
            !          COMPUTE AND TEST ALLOWABLE
            !          TOLERANCES VERSUS LOCAL ERROR
            !          ESTIMATES.  NOTE THAT RELATIVE
            !          ERROR IS MEASURED WITH RESPECT
            !          TO THE AVERAGE OF THE
            !          MAGNITUDES OF THE SOLUTION AT
            !          THE BEGINNING AND END OF THE
            !          STEP.  LOCAL ERROR ESTIMATES
            !          FOR A SPECIAL FIRST ORDER
            !          METHOD ARE CALCULATED ONLY WHEN
            !          THE STIFFNESS DETECTION IS
            !          TURNED ON.
            !
            eeoet = 0._DP
            estiff = 0._DP
            ktol = 1
            DO k = 1, Neq
              yavg = 0.5_DP*(ABS(Y(k))+ABS(Ys(k)))
              IF( Info(2)==1 ) ktol = k
              et = Rtol(ktol)*yavg + Atol(ktol)
              IF( et>0._DP ) THEN
                !
                ee = ABS((-2090._DP*Yp(k)+(21970._DP*F3(k)-15048._DP*F4(k)))&
                  +(22528._DP*F2(k)-27360._DP*F5(k)))
                IF( .NOT. (Stiff .OR. Nonstf) ) THEN
                  es = ABS&
                    (H*(0.055455_DP*Yp(k)-0.035493_DP*F1(k)-0.036571_DP*F2&
                    (k)+0.023107_DP*F3(k)-0.009515_DP*F4(k)+0.003017_DP*F5(k)))
                  estiff = MAX(estiff,es/et)
                END IF
                eeoet = MAX(eeoet,ee/et)
              ELSE
                !
                !           PURE RELATIVE ERROR INAPPROPRIATE WHEN SOLUTION
                !                    VANISHES
                Idid = -3
                !              ...........................EXIT
                GOTO 200
              END IF
            END DO
            !
            esttol = ABS(H)*eeoet/752400._DP
            !
            !  ...EXIT
            IF( esttol<=1._DP ) THEN
              !
              !  .......................................
              !
              !  SUCCESSFUL STEP
              !                    STORE SOLUTION AT T+H
              !                    AND EVALUATE
              !                    DERIVATIVES THERE
              !
              T = T + H
              DO k = 1, Neq
                Y(k) = Ys(k)
              END DO
              a = T
              CALL DF(a,Y,Yp)
              !
              !  CHOOSE NEXT STEP SIZE
              !  THE INCREASE IS LIMITED TO A FACTOR OF
              !  5 IF STEP FAILURE HAS JUST OCCURRED,
              !  NEXT
              !     STEP SIZE IS NOT ALLOWED TO INCREASE
              !
              s = 5._DP
              IF( esttol>1.889568D-4 ) s = 0.9_DP/esttol**0.2_DP
              IF( hfaild ) s = MIN(s,1._DP)
              H = SIGN(MAX(s*ABS(H),hmin),H)
              !
              !  .......................................
              !
              !       CHECK FOR STIFFNESS (IF NOT
              !       ALREADY DETECTED)
              !
              !       IN A SEQUENCE OF 50 SUCCESSFUL
              !       STEPS BY THE FEHLBERG METHOD, 25
              !       SUCCESSFUL STEPS BY THE FIRST
              !       ORDER METHOD INDICATES STIFFNESS
              !       AND TURNS THE TEST OFF. IF 26
              !       FAILURES BY THE FIRST ORDER METHOD
              !       OCCUR, THE TEST IS TURNED OFF
              !       UNTIL THIS SEQUENCE OF 50 STEPS BY
              !       THE FEHLBERG METHOD IS COMPLETED.
              !
              !  ...EXIT
              IF( .NOT. (Stiff) ) THEN
                Ntstep = MOD(Ntstep+1,50)
                IF( Ntstep==1 ) Nonstf = .FALSE.
                !  ...EXIT
                IF( .NOT. (Nonstf) ) THEN
                  IF( estiff<=1._DP ) THEN
                    !
                    !     SUCCESSFUL STEP WITH FIRST ORDER
                    !     METHOD
                    Nstifs = Nstifs + 1
                    !     TURN TEST OFF AFTER 25 INDICATIONS
                    !     OF STIFFNESS
                    IF( Nstifs==25 ) Stiff = .TRUE.
                    !
                    !  UNSUCCESSFUL STEP WITH FIRST ORDER
                    !  METHOD
                  ELSEIF( Ntstep-Nstifs>25 ) THEN
                    !               TURN STIFFNESS DETECTION OFF FOR THIS BLOCK OF
                    !                    FIFTY STEPS
                    Nonstf = .TRUE.
                    !     RESET STIFF STEP COUNTER
                    Nstifs = 0
                  END IF
                END IF
              END IF
              !
              !  ******************************************
              !    END OF CORE INTEGRATOR
              !  ******************************************
              !
              !
              !       SHOULD WE TAKE ANOTHER STEP
              !
              !  ......EXIT
              IF( output ) GOTO 150
              IF( Info(3)==0 ) GOTO 120
              !
              !     *********************************************
              !     *********************************************
              !
              !    INTEGRATION SUCCESSFULLY COMPLETED
              !
              !                ONE-STEP MODE
              Idid = 1
              Told = T
              !     .....................EXIT
              RETURN
              !
              !        ....................................
              !
              !             UNSUCCESSFUL STEP
              !
            ELSEIF( ABS(H)>hmin ) THEN
              !
              !        REDUCE THE STEP SIZE, TRY AGAIN
              !        THE DECREASE IS LIMITED TO A FACTOR
              !        OF 1/10
              !
              hfaild = .TRUE.
              output = .FALSE.
              s = 0.1_DP
              IF( esttol<59049._DP ) s = 0.9_DP/esttol**0.2_DP
              H = SIGN(MAX(s*ABS(H),hmin),H)
            ELSE
              !
              !  REQUESTED ERROR UNATTAINABLE AT SMALLEST
              !  ALLOWABLE STEP SIZE
              Tolfac = 1.69_DP*esttol
              Idid = -2
              !              ........................EXIT
              GOTO 200
            END IF
          END DO
          !
          !           A SIGNIFICANT AMOUNT OF WORK HAS
          !           BEEN EXPENDED
          Idid = -1
          Ksteps = 0
          !              ........................EXIT
          IF( Stiff ) THEN
            !
            !           PROBLEM APPEARS TO BE STIFF
            Idid = -4
            Stiff = .FALSE.
            Nonstf = .FALSE.
            Ntstep = 0
            !              ........................EXIT
            Nstifs = 0
          END IF
          GOTO 200
        ELSE
          !
          !     REQUESTED ERROR UNATTAINABLE DUE TO LIMITED
          !  PRECISION AVAILABLE
          Tolfac = 2._DP*Tolfac
          Idid = -2
          !              .....................EXIT
          GOTO 200
        END IF
        120 CONTINUE
      END DO
    ELSE
      !
      !  IF TOO CLOSE TO OUTPUT POINT,EXTRAPOLATE AND
      !  RETURN
      !
      DO k = 1, Neq
        Y(k) = Y(k) + dt*Yp(k)
      END DO
      a = Tout
      CALL DF(a,Y,Yp)
      Ksteps = Ksteps + 1
    END IF
    !
    !                    INTERVAL MODE
    150  Idid = 2
    T = Tout
    Told = T
    !     ...............EXIT
    RETURN
  ELSE
    !
    !                    UNNECESSARY FREQUENCY OF OUTPUT IS RESTRICTING
    !                   THE STEP SIZE CHOICE
    Idid = -5
    Kop = 0
  END IF
  !
  !        INTEGRATION TASK INTERRUPTED
  !
  200  Info(1) = -1
  Told = T
  !     ...EXIT
  IF( Idid==(-2) ) THEN
    !
    !        THE ERROR TOLERANCES ARE INCREASED TO VALUES
    !                WHICH ARE APPROPRIATE FOR CONTINUING
    Rtol(1) = Tolfac*Rtol(1)
    Atol(1) = Tolfac*Atol(1)
    !     ...EXIT
    IF( Info(2)/=0 ) THEN
      DO k = 2, Neq
        Rtol(k) = Tolfac*Rtol(k)
        Atol(k) = Tolfac*Atol(k)
      END DO
    END IF
  END IF
  !
  RETURN
END SUBROUTINE DRKFS