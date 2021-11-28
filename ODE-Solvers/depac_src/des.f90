!** DES
SUBROUTINE DES(F,Neq,T,Y,Tout,Info,Rtol,Atol,Idid,Ypout,Yp,Yy,Wt,P,Phi,&
    Alpha,Beta,Psi,V,W,Sig,G,Gi,H,Eps,X,Xold,Hold,Told,Delsgn,&
    Tstop,Twou,Fouru,Start,Phase1,Nornd,Stiff,Intout,Ns,Kord,&
    Kold,Init,Ksteps,Kle4,Iquit,Kprev,Ivc,Iv,Kgi)
  !> Subsidiary to DEABM
  !***
  ! **Library:**   SLATEC
  !***
  ! **Type:**      SINGLE PRECISION (DES-S, DDES-D)
  !***
  ! **Author:**  Watts, H. A., (SNLA)
  !***
  ! **Description:**
  !
  !   DEABM  merely allocates storage for  DES  to relieve the user of the
  !   inconvenience of a long call list.  Consequently  DES  is used as
  !   described in the comments for  DEABM .
  !
  !***
  ! **See also:**  DEABM
  !***
  ! **Routines called:**  R1MACH, SINTRP, STEPS, XERMSG

  !* REVISION HISTORY  (YYMMDD)
  !   800501  DATE WRITTEN
  !   890531  Changed all specific intrinsics to generic.  (WRB)
  !   890831  Modified array declarations.  (WRB)
  !   891214  Prologue converted to Version 4.0 format.  (BAB)
  !   900328  Added TYPE section.  (WRB)
  !   900510  Convert XERRWV calls to XERMSG calls, replace GOTOs with IF-THEN-ELSEs.  (RWC)
  !   910722  Updated AUTHOR section.  (ALS)
  USE service, ONLY : eps_sp
  !
  INTERFACE
    SUBROUTINE F(X,U,Uprime)
      IMPORT SP
      REAL(SP), INTENT(IN) :: X
      REAL(SP), INTENT(IN) :: U(:)
      REAL(SP), INTENT(OUT) :: Uprime(:)
    END SUBROUTINE F
  END INTERFACE
  INTEGER, INTENT(IN) :: Neq
  INTEGER, INTENT(INOUT) :: Idid, Init, Iquit, Ivc, Kgi, Kle4, Kold, Kord, &
    Kprev, Ksteps, Ns
  INTEGER, INTENT(INOUT) :: Iv(10), Info(15)
  REAL(SP), INTENT(IN) :: Tout, Tstop
  REAL(SP), INTENT(INOUT) :: Delsgn, Eps, Fouru, H, Hold, Twou, T, Told, X, Xold
  REAL(SP), INTENT(INOUT) :: Alpha(12), Atol(:), Beta(12), G(13), Gi(11), P(Neq), &
    Phi(Neq,16), Psi(12), Rtol(:), Sig(13), V(12), W(12), Wt(Neq), Y(Neq), Yp(Neq), &
    Ypout(Neq), Yy(Neq)
  LOGICAL, INTENT(INOUT) :: Stiff, Start, Phase1, Nornd, Intout
  !
  INTEGER :: k, l, ltol, natolp, nrtolp
  REAL(SP) :: a, absdel, del, dt, ha, u
  LOGICAL :: crash
  CHARACTER(8) :: xern1
  CHARACTER(16) :: xern3, xern4
  !
  !.......................................................................
  !
  !  THE EXPENSE OF SOLVING THE PROBLEM IS MONITORED BY COUNTING THE
  !  NUMBER OF  STEPS ATTEMPTED. WHEN THIS EXCEEDS  MAXNUM, THE COUNTER
  !  IS RESET TO ZERO AND THE USER IS INFORMED ABOUT POSSIBLE EXCESSIVE
  !  WORK.
  !
  INTEGER, PARAMETER :: maxnum = 500
  !
  !.......................................................................
  !
  !* FIRST EXECUTABLE STATEMENT  DES
  IF( Info(1)==0 ) THEN
    !
    ! ON THE FIRST CALL, PERFORM INITIALIZATION --
    !        DEFINE THE MACHINE UNIT ROUNDOFF QUANTITY  U  BY CALLING THE
    !        FUNCTION ROUTINE  R1MACH. THE USER MUST MAKE SURE THAT THE
    !        VALUES SET IN R1MACH ARE RELEVANT TO THE COMPUTER BEING USED.
    !
    u = eps_sp
    !                       -- SET ASSOCIATED MACHINE DEPENDENT PARAMETERS
    Twou = 2._SP*u
    Fouru = 4._SP*u
    !                       -- SET TERMINATION FLAG
    Iquit = 0
    !                       -- SET INITIALIZATION INDICATOR
    Init = 0
    !                       -- SET COUNTER FOR ATTEMPTED STEPS
    Ksteps = 0
    !                       -- SET INDICATOR FOR INTERMEDIATE-OUTPUT
    Intout = .FALSE.
    !                       -- SET INDICATOR FOR STIFFNESS DETECTION
    Stiff = .FALSE.
    !                       -- SET STEP COUNTER FOR STIFFNESS DETECTION
    Kle4 = 0
    !                       -- SET INDICATORS FOR STEPS CODE
    Start = .TRUE.
    Phase1 = .TRUE.
    Nornd = .TRUE.
    !                       -- RESET INFO(1) FOR SUBSEQUENT CALLS
    Info(1) = 1
  END IF
  !
  !.......................................................................
  !
  !      CHECK VALIDITY OF INPUT PARAMETERS ON EACH ENTRY
  !
  IF( Info(1)/=0 .AND. Info(1)/=1 ) THEN
    WRITE (xern1,'(I8)') Info(1)
    ERROR STOP 'DES : IN DEABM, INFO(1) MUST BE SET TO 0 FOR THE&
      & START OF A NEW PROBLEM, AND MUST BE SET TO 1 FOLLOWING AN INTERRUPTED TASK.&
      & YOU ARE ATTEMPTING TO CONTINUE THE INTEGRATION ILLEGALLY'
    Idid = -33
  END IF
  !
  IF( Info(2)/=0 .AND. Info(2)/=1 ) THEN
    WRITE (xern1,'(I8)') Info(2)
    ERROR STOP 'DES : IN DEABM, INFO(2) MUST BE 0 OR 1 INDICATING&
      & SCALAR AND VECTOR ERROR TOLERANCES, RESPECTIVELY.'
    Idid = -33
  END IF
  !
  IF( Info(3)/=0 .AND. Info(3)/=1 ) THEN
    WRITE (xern1,'(I8)') Info(3)
    ERROR STOP 'DES : IN DEABM, INFO(3) MUST BE 0 OR 1 INDICATING&
      & THE INTERVAL OR INTERMEDIATE-OUTPUT MODE OF INTEGRATION, RESPECTIVELY.'
    Idid = -33
  END IF
  !
  IF( Info(4)/=0 .AND. Info(4)/=1 ) THEN
    WRITE (xern1,'(I8)') Info(4)
    ERROR STOP 'DES : IN DEABM, INFO(4) MUST BE 0 OR 1 INDICATING&
      & WHETHER OR NOT THE INTEGRATION INTERVAL IS TO BE RESTRICTED BY A POINT TSTOP.'
    Idid = -33
  END IF
  !
  IF( Neq<1 ) THEN
    WRITE (xern1,'(I8)') Neq
    ERROR STOP 'DES : IN DEABM, THE NUMBER OF EQUATIONS NEQ MUST BE A POSITIVE INTEGER.'
    Idid = -33
  END IF
  !
  nrtolp = 0
  natolp = 0
  DO k = 1, Neq
    IF( nrtolp==0 .AND. Rtol(k)<0. ) THEN
      WRITE (xern1,'(I8)') k
      WRITE (xern3,'(1PE15.6)') Rtol(k)
      ERROR STOP 'DES : IN DEABM, THE RELATIVE ERROR TOLERANCES RTOL&
        & MUST BE NON-NEGATIVE. IN THE CASE OF VECTOR ERROR TOLERANCES, NO FURTHER&
        & CHECKING OF RTOL COMPONENTS IS DONE.'
      Idid = -33
      nrtolp = 1
    END IF
    !
    IF( natolp==0 .AND. Atol(k)<0. ) THEN
      WRITE (xern1,'(I8)') k
      WRITE (xern3,'(1PE15.6)') Atol(k)
      ERROR STOP 'DES : IN DEABM, THE ABSOLUTE ERROR TOLERANCES ATOL&
        & MUST BE NON-NEGATIVE. IN THE CASE OF VECTOR ERROR TOLERANCES, NO FURTHER&
        & CHECKING OF ATOL COMPONENTS IS DONE.'
      Idid = -33
      natolp = 1
    END IF
    !
    IF( Info(2)==0 ) EXIT
    IF( natolp>0 .AND. nrtolp>0 ) EXIT
  END DO
  !
  IF( Info(4)==1 ) THEN
    IF( SIGN(1._SP,Tout-T)/=SIGN(1._SP,Tstop-T) .OR. ABS(Tout-T)>ABS(Tstop-T) ) THEN
      WRITE (xern3,'(1PE15.6)') Tout
      WRITE (xern4,'(1PE15.6)') Tstop
      ERROR STOP 'DES : IN DEABM, YOU HAVE CALLED THE CODE WITH THAT VALUE OF TOUT &
        & BUT YOU HAVE ALSO TOLD THE CODE (INFO(4) = 1) NOT TO&
        & INTEGRATE PAST THE POINT TSTOP THESE INSTRUCTIONS CONFLICT.'
      Idid = -33
    END IF
  END IF
  !
  !     CHECK SOME CONTINUATION POSSIBILITIES
  !
  IF( Init/=0 ) THEN
    IF( T==Tout ) THEN
      WRITE (xern3,'(1PE15.6)') T
      ERROR STOP 'DES : IN DEABM, YOU HAVE CALLED THE CODE WITH  T = TOUT&
        & THIS IS NOT ALLOWED ON CONTINUATION CALLS.'
      Idid = -33
    END IF
    !
    IF( T/=Told ) THEN
      WRITE (xern3,'(1PE15.6)') Told
      WRITE (xern4,'(1PE15.6)') T
      ERROR STOP 'DES : IN DEABM, YOU HAVE CHANGED THE VALUE OF T  THIS IS NOT&
        & ALLOWED ON CONTINUATION CALLS.'
      Idid = -33
    END IF
    !
    IF( Init/=1 ) THEN
      IF( Delsgn*(Tout-T)<0. ) THEN
        WRITE (xern3,'(1PE15.6)') Tout
        ERROR STOP 'DES : IN DEABM, BY CALLING THE CODE WITH THAT VALUE OF TOUT &
          & YOU ARE ATTEMPTING TO CHANGE THE DIRECTION OF INTEGRATION.&
          & THIS IS NOT ALLOWED WITHOUT RESTARTING.'
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
      Info(1) = -1
    ELSE
      ERROR STOP 'DES : IN DEABM, INVALID INPUT WAS DETECTED ON&
        & SUCCESSIVE ENTRIES.  IT IS IMPOSSIBLE TO PROCEED BECAUSE YOU HAVE NOT&
        & CORRECTED THE PROBLEM, SO EXECUTION IS BEING TERMINATED.'
    END IF
    RETURN
  END IF
  !
  !.......................................................................
  !
  !     RTOL = ATOL = 0. IS ALLOWED AS VALID INPUT AND INTERPRETED AS
  !     ASKING FOR THE MOST ACCURATE SOLUTION POSSIBLE. IN THIS CASE,
  !     THE RELATIVE ERROR TOLERANCE RTOL IS RESET TO THE SMALLEST VALUE
  !     FOURU WHICH IS LIKELY TO BE REASONABLE FOR THIS METHOD AND MACHINE
  !
  DO k = 1, Neq
    IF( Rtol(k)+Atol(k)<=0. ) THEN
      Rtol(k) = Fouru
      Idid = -2
    END IF
    IF( Info(2)==0 ) EXIT
  END DO
  !
  IF( Idid/=(-2) ) THEN
    !
    !     BRANCH ON STATUS OF INITIALIZATION INDICATOR
    !            INIT=0 MEANS INITIAL DERIVATIVES AND NOMINAL STEP SIZE
    !                   AND DIRECTION NOT YET SET
    !            INIT=1 MEANS NOMINAL STEP SIZE AND DIRECTION NOT YET SET
    !            INIT=2 MEANS NO FURTHER INITIALIZATION REQUIRED
    !
    IF( Init==0 ) THEN
      !
      !.......................................................................
      !
      !     MORE INITIALIZATION --
      !                         -- EVALUATE INITIAL DERIVATIVES
      !
      Init = 1
      a = T
      CALL F(a,Y,Yp)
      IF( T==Tout ) THEN
        Idid = 2
        DO l = 1, Neq
          Ypout(l) = Yp(l)
        END DO
        Told = T
        RETURN
      END IF
    ELSEIF( Init/=1 ) THEN
      GOTO 100
    END IF
    !
    !                         -- SET INDEPENDENT AND DEPENDENT VARIABLES
    !                                              X AND YY(*) FOR STEPS
    !                         -- SET SIGN OF INTEGRATION DIRECTION
    !                         -- INITIALIZE THE STEP SIZE
    !
    Init = 2
    X = T
    DO l = 1, Neq
      Yy(l) = Y(l)
    END DO
    Delsgn = SIGN(1._SP,Tout-T)
    H = SIGN(MAX(Fouru*ABS(X),ABS(Tout-X)),Tout-X)
  ELSE
    !                       RTOL=ATOL=0 ON INPUT, SO RTOL IS CHANGED TO A
    !                                                SMALL POSITIVE VALUE
    Info(1) = -1
    RETURN
  END IF
  !
  !.......................................................................
  !
  !   ON EACH CALL SET INFORMATION WHICH DETERMINES THE ALLOWED INTERVAL
  !   OF INTEGRATION BEFORE RETURNING WITH AN ANSWER AT TOUT
  !
  100  del = Tout - T
  absdel = ABS(del)
  !
  !.......................................................................
  !
  !   IF ALREADY PAST OUTPUT POINT, INTERPOLATE AND RETURN
  !
  DO WHILE( ABS(X-T)<absdel )
    !
    !   IF CANNOT GO PAST TSTOP AND SUFFICIENTLY CLOSE,
    !   EXTRAPOLATE AND RETURN
    !
    IF( Info(4)==1 ) THEN
      IF( ABS(Tstop-X)<Fouru*ABS(X) ) THEN
        dt = Tout - X
        DO l = 1, Neq
          Y(l) = Yy(l) + dt*Yp(l)
        END DO
        CALL F(Tout,Y,Ypout)
        Idid = 3
        T = Tout
        Told = T
        RETURN
      END IF
    END IF
    !
    IF( .NOT. (Info(3)==0 .OR. .NOT. Intout) ) THEN
      !
      !   INTERMEDIATE-OUTPUT MODE
      !
      Idid = 1
      DO l = 1, Neq
        Y(l) = Yy(l)
        Ypout(l) = Yp(l)
      END DO
      T = X
      Told = T
      Intout = .FALSE.
      RETURN
      !
      !.......................................................................
      !
      !     MONITOR NUMBER OF STEPS ATTEMPTED
      !
    ELSEIF( Ksteps<=maxnum ) THEN
      !
      !.......................................................................
      !
      !   LIMIT STEP SIZE, SET WEIGHT VECTOR AND TAKE A STEP
      !
      ha = ABS(H)
      IF( Info(4)==1 ) ha = MIN(ha,ABS(Tstop-X))
      H = SIGN(ha,H)
      Eps = 1._SP
      ltol = 1
      DO l = 1, Neq
        IF( Info(2)==1 ) ltol = l
        Wt(l) = Rtol(ltol)*ABS(Yy(l)) + Atol(ltol)
        IF( Wt(l)<=0._SP ) GOTO 120
      END DO
      !
      CALL STEPS(F,Neq,Yy,X,H,Eps,Wt,Start,Hold,Kord,Kold,crash,Phi,P,Yp,&
        Psi,Alpha,Beta,Sig,V,W,G,Phase1,Ns,Nornd,Ksteps,Twou,Fouru,&
        Xold,Kprev,Ivc,Iv,Kgi,Gi)
      !
      !.......................................................................
      !
      IF( .NOT. crash ) THEN
        !
        !   (STIFFNESS TEST) COUNT NUMBER OF CONSECUTIVE STEPS TAKEN WITH THE
        !   ORDER OF THE METHOD BEING LESS OR EQUAL TO FOUR
        !
        Kle4 = Kle4 + 1
        IF( Kold>4 ) Kle4 = 0
        IF( Kle4>=50 ) Stiff = .TRUE.
        Intout = .TRUE.
        CYCLE
      ELSE
        !
        !                       TOLERANCES TOO SMALL
        Idid = -2
        Rtol(1) = Eps*Rtol(1)
        Atol(1) = Eps*Atol(1)
        IF( Info(2)/=0 ) THEN
          DO l = 2, Neq
            Rtol(l) = Eps*Rtol(l)
            Atol(l) = Eps*Atol(l)
          END DO
        END IF
        GOTO 200
      END IF
      !
      !                       RELATIVE ERROR CRITERION INAPPROPRIATE
      120  Idid = -3
      DO l = 1, Neq
        Y(l) = Yy(l)
        Ypout(l) = Yp(l)
      END DO
      T = X
      Told = T
      Info(1) = -1
      Intout = .FALSE.
      RETURN
    ELSE
      !
      !                       A SIGNIFICANT AMOUNT OF WORK HAS BEEN EXPENDED
      Idid = -1
      Ksteps = 0
      IF( Stiff ) THEN
        !
        !                       PROBLEM APPEARS TO BE STIFF
        Idid = -4
        Stiff = .FALSE.
        Kle4 = 0
      END IF
      !
      DO l = 1, Neq
        Y(l) = Yy(l)
        Ypout(l) = Yp(l)
      END DO
      T = X
      Told = T
      Info(1) = -1
      Intout = .FALSE.
      RETURN
    END IF
  END DO
  CALL SINTRP(X,Yy,Tout,Y,Ypout,Neq,Kold,Phi,Ivc,Iv,Kgi,Gi,Alpha,G,W,Xold,P)
  Idid = 3
  IF( X==Tout ) THEN
    Idid = 2
    Intout = .FALSE.
  END IF
  T = Tout
  Told = T
  RETURN
  200 CONTINUE
  DO l = 1, Neq
    Y(l) = Yy(l)
    Ypout(l) = Yp(l)
  END DO
  T = X
  Told = T
  Info(1) = -1
  Intout = .FALSE.
  !
  RETURN
END SUBROUTINE DES