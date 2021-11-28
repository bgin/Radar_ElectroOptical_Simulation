!** DLSOD
SUBROUTINE DLSOD(DF,Neq,T,Y,Tout,Rtol,Atol,Idid,Ypout,Yh,Yh1,Ewt,Savf,&
    Acor,Wm,Iwm,DJAC,Intout,Tstop,Tolfac,Delsgn)
  !> Subsidiary to DDEBDF
  !***
  ! **Library:**   SLATEC
  !***
  ! **Type:**      DOUBLE PRECISION (LSOD-S, DLSOD-D)
  !***
  ! **Author:**  (UNKNOWN)
  !***
  ! **Description:**
  !
  !   DDEBDF  merely allocates storage for  DLSOD  to relieve the user of
  !   the inconvenience of a long call list.  Consequently  DLSOD  is used
  !   as described in the comments for  DDEBDF .
  !
  !***
  ! **See also:**  DDEBDF
  !***
  ! **Routines called:**  D1MACH, DHSTRT, DINTYD, DSTOD, DVNRMS, XERMSG
  !***
  ! COMMON BLOCKS    DDEBD1

  !* REVISION HISTORY  (YYMMDD)
  !   820301  DATE WRITTEN
  !   890531  Changed all specific intrinsics to generic.  (WRB)
  !   890831  Modified array declarations.  (WRB)
  !   891214  Prologue converted to Version 4.0 format.  (BAB)
  !   900328  Added TYPE section.  (WRB)
  !   900510  Convert XERRWV calls to XERMSG calls.  (RWC)
  USE DDEBD1, ONLY : told_com, h_com, hmin_com, hmxi_com, tn_com, uround_com, &
    iquit_com, init_com, ksteps_com, ibegin_com, itol_com, iinteg_com, itstop_com, &
    ijac_com, iband_com, jstart_com, kflag_com, meth_com, miter_com, maxord_com, &
    n_com, nq_com, nst_com, nfe_com, nje_com
  USE service, ONLY : eps_dp, huge_dp
  !
  INTERFACE
    SUBROUTINE DF(X,U,Uprime)
      IMPORT DP
      REAL(DP), INTENT(IN) :: X
      REAL(DP), INTENT(IN) :: U(:)
      REAL(DP), INTENT(OUT) :: Uprime(:)
    END SUBROUTINE DF
    PURE SUBROUTINE DJAC(X,U,Pd,Nrowpd)
      IMPORT DP
      INTEGER, INTENT(IN) :: Nrowpd
      REAL(DP), INTENT(IN) :: X
      REAL(DP), INTENT(IN) :: U(:)
      REAL(DP), INTENT(OUT) :: Pd(:,:)
    END SUBROUTINE DJAC
  END INTERFACE
  INTEGER, INTENT(IN) :: Neq
  INTEGER, INTENT(OUT) :: Idid
  INTEGER, INTENT(INOUT) :: Iwm(:)
  REAL(DP), INTENT(IN) :: Tout, Tstop
  REAL(DP), INTENT(INOUT) :: Delsgn, T
  REAL(DP), INTENT(OUT) :: Tolfac
  REAL(DP), INTENT(INOUT) :: Acor(Neq), Atol(:), Ewt(Neq), Rtol(:), Savf(Neq), &
    Y(Neq), Yh(Neq,6), Yh1(6*Neq), Ypout(Neq), Wm(:)
  LOGICAL, INTENT(INOUT) :: Intout
  !
  INTEGER :: intflg, k, l, ltol, natolp, nrtolp
  REAL(DP) :: absdel, big, del, dt, ha, tol

  CHARACTER(8) :: xern1
  CHARACTER(16) :: xern3, xern4
  !
  !     ..................................................................
  !
  !       THE EXPENSE OF SOLVING THE PROBLEM IS MONITORED BY COUNTING THE
  !       NUMBER OF  STEPS ATTEMPTED. WHEN THIS EXCEEDS  MAXNUM, THE
  !       COUNTER IS RESET TO ZERO AND THE USER IS INFORMED ABOUT POSSIBLE
  !       EXCESSIVE WORK.
  INTEGER, PARAMETER :: maxnum = 500
  !
  !     ..................................................................
  !
  !* FIRST EXECUTABLE STATEMENT  DLSOD
  IF( ibegin_com==0 ) THEN
    !
    !        ON THE FIRST CALL, PERFORM INITIALIZATION --
    !        DEFINE THE MACHINE UNIT ROUNDOFF QUANTITY  U  BY CALLING THE
    !        FUNCTION ROUTINE D1MACH. THE USER MUST MAKE SURE THAT THE
    !        VALUES SET IN D1MACH ARE RELEVANT TO THE COMPUTER BEING USED.
    !
    uround_com = eps_dp
    !                          -- SET ASSOCIATED MACHINE DEPENDENT PARAMETER
    Wm(1) = SQRT(uround_com)
    !                          -- SET TERMINATION FLAG
    iquit_com = 0
    !                          -- SET INITIALIZATION INDICATOR
    init_com = 0
    !                          -- SET COUNTER FOR ATTEMPTED STEPS
    ksteps_com = 0
    !                          -- SET INDICATOR FOR INTERMEDIATE-OUTPUT
    Intout = .FALSE.
    !                          -- SET START INDICATOR FOR DSTOD CODE
    jstart_com = 0
    !                          -- SET BDF METHOD INDICATOR
    meth_com = 2
    !                          -- SET MAXIMUM ORDER FOR BDF METHOD
    maxord_com = 5
    !                          -- SET ITERATION MATRIX INDICATOR
    !
    IF( ijac_com==0 .AND. iband_com==0 ) miter_com = 2
    IF( ijac_com==1 .AND. iband_com==0 ) miter_com = 1
    IF( ijac_com==0 .AND. iband_com==1 ) miter_com = 5
    IF( ijac_com==1 .AND. iband_com==1 ) miter_com = 4
    !
    !                          -- SET OTHER NECESSARY ITEMS IN COMMON BLOCK
    n_com = Neq
    nst_com = 0
    nje_com = 0
    hmxi_com = 0._DP
    nq_com = 1
    h_com = 1._DP
    !                          -- RESET IBEGIN FOR SUBSEQUENT CALLS
    ibegin_com = 1
  END IF
  !
  !     ..................................................................
  !
  !      CHECK VALIDITY OF INPUT PARAMETERS ON EACH ENTRY
  !
  IF( Neq<1 ) THEN
    WRITE (xern1,'(I8)') Neq
    ERROR STOP 'DLSOD : IN DDEBDF, THE NUMBER OF EQUATIONS MUST BE A POSITIVE INTEGER.'
    Idid = -33
  END IF
  !
  nrtolp = 0
  natolp = 0
  DO k = 1, Neq
    IF( nrtolp<=0 ) THEN
      IF( Rtol(k)<0. ) THEN
        WRITE (xern1,'(I8)') k
        WRITE (xern3,'(1PE15.6)') Rtol(k)
        ERROR STOP 'DLSOD : IN DDEBDF, THE RELATIVE ERROR TOLERANCES&
          & MUST BE NON-NEGATIVE. IN THE CASE OF VECTOR ERROR TOLERANCES,&
          & NO FURTHER CHECKING OF RTOL COMPONENTS IS DONE.'
        Idid = -33
        IF( natolp>0 ) EXIT
        nrtolp = 1
      ELSEIF( natolp>0 ) THEN
        GOTO 50
      END IF
    END IF
    !
    IF( Atol(k)<0. ) THEN
      WRITE (xern1,'(I8)') k
      WRITE (xern3,'(1PE15.6)') Atol(k)
      ERROR STOP 'DLSOD : IN DDEBDF, THE ABSOLUTE ERROR TOLERANCES&
        & MUST BE NON-NEGATIVE. IN THE CASE OF VECTOR ERROR TOLERANCES,&
        & NO FURTHER CHECKING OF ATOL COMPONENTS IS DONE.'
      Idid = -33
      IF( nrtolp>0 ) EXIT
      natolp = 1
    END IF
    50  IF( itol_com==0 ) EXIT
  END DO
  !
  IF( itstop_com==1 ) THEN
    IF( SIGN(1._DP,Tout-T)/=SIGN(1._DP,Tstop-T) .OR. ABS(Tout-T)>ABS(Tstop-T) ) THEN
      WRITE (xern3,'(1PE15.6)') Tout
      WRITE (xern4,'(1PE15.6)') Tstop
      ERROR STOP 'DLSOD : IN DDEBDF, YOU HAVE CALLED THE CODE WITH THAT VALUE OF&
        & TOUT, BUT YOU HAVE ALSO TOLD THE CODE NOT TO INTEGRATE PAST THE POINT&
        & TSTOP BY SETTING INFO(4) = 1. THESE INSTRUCTIONS CONFLICT.'
      Idid = -33
    END IF
  END IF
  !
  !        CHECK SOME CONTINUATION POSSIBILITIES
  !
  IF( init_com/=0 ) THEN
    IF( T==Tout ) THEN
      WRITE (xern3,'(1PE15.6)') T
      ERROR STOP 'DLSOD : IN DDEBDF, YOU HAVE CALLED THE CODE WITH T = TOUT&
        & THIS IS NOT ALLOWED ON CONTINUATION CALLS.'
      Idid = -33
    END IF
    !
    IF( T/=told_com ) THEN
      WRITE (xern3,'(1PE15.6)') told_com
      WRITE (xern4,'(1PE15.6)') T
      ERROR STOP 'DLSOD : IN DDEBDF, YOU HAVE CHANGED THE VALUE OF T, &
        &THIS IS NOT ALLOWED ON CONTINUATION CALLS.'
      Idid = -33
    END IF
    !
    IF( init_com/=1 ) THEN
      IF( Delsgn*(Tout-T)<0._DP ) THEN
        WRITE (xern3,'(1PE15.6)') Tout
        ERROR STOP 'DLSOD : IN DDEBDF, BY CALLING THE CODE WITH THAT VALUE OFTOUT&
          & YOU ARE ATTEMPTING TO CHANGE THE DIRECTION OF INTEGRATION.&
          & THIS IS NOT ALLOWED WITHOUT RESTARTING.'
        Idid = -33
      END IF
    END IF
  END IF
  !
  IF( Idid==(-33) ) THEN
    IF( iquit_com/=(-33) ) THEN
      !                       INVALID INPUT DETECTED
      iquit_com = -33
      ibegin_com = -1
    ELSE
      ERROR STOP 'DLSOD : IN DDEBDF, INVALID INPUT WAS DETECTED ON&
        & SUCCESSIVE ENTRIES.  IT IS IMPOSSIBLE TO PROCEED BECAUSE YOU HAVE NOT&
        & CORRECTED THE PROBLEM, SO EXECUTION IS BEING TERMINATED.'
    END IF
    RETURN
  END IF
  !
  !        ...............................................................
  !
  !             RTOL = ATOL = 0. IS ALLOWED AS VALID INPUT AND INTERPRETED
  !             AS ASKING FOR THE MOST ACCURATE SOLUTION POSSIBLE. IN THIS
  !             CASE, THE RELATIVE ERROR TOLERANCE RTOL IS RESET TO THE
  !             SMALLEST VALUE 100*U WHICH IS LIKELY TO BE REASONABLE FOR
  !             THIS METHOD AND MACHINE
  !
  DO k = 1, Neq
    IF( Rtol(k)+Atol(k)<=0._DP ) THEN
      Rtol(k) = 100._DP*uround_com
      Idid = -2
    END IF
    !     ...EXIT
    IF( itol_com==0 ) EXIT
  END DO
  !
  IF( Idid/=(-2) ) THEN
    !        BEGIN BLOCK PERMITTING ...EXITS TO 450
    !           BEGIN BLOCK PERMITTING ...EXITS TO 430
    !              BEGIN BLOCK PERMITTING ...EXITS TO 260
    !                 BEGIN BLOCK PERMITTING ...EXITS TO 230
    !
    !                    BRANCH ON STATUS OF INITIALIZATION INDICATOR
    !                           INIT=0 MEANS INITIAL DERIVATIVES AND
    !                           NOMINAL STEP SIZE
    !                                  AND DIRECTION NOT YET SET
    !                           INIT=1 MEANS NOMINAL STEP SIZE AND
    !                           DIRECTION NOT YET SET INIT=2 MEANS NO
    !                           FURTHER INITIALIZATION REQUIRED
    !
    IF( init_com==0 ) THEN
      !
      !................................................
      !
      ! MORE INITIALIZATION --
      !  -- EVALUATE INITIAL DERIVATIVES
      !
      init_com = 1
      CALL DF(T,Y,Yh(:,2))
      nfe_com = 1
      !                 ...EXIT
      IF( T==Tout ) THEN
        Idid = 2
        DO l = 1, Neq
          Ypout(l) = Yh(l,2)
        END DO
        told_com = T
        !        ............EXIT
        RETURN
      END IF
      !                 ......EXIT
    ELSEIF( init_com/=1 ) THEN
      GOTO 100
      !              .........EXIT
    END IF
    !
    !                 -- COMPUTE INITIAL STEP SIZE
    !                 -- SAVE SIGN OF INTEGRATION DIRECTION
    !                 -- SET INDEPENDENT AND DEPENDENT VARIABLES
    !                                      X AND YH(*) FOR DSTOD
    !
    ltol = 1
    DO l = 1, Neq
      IF( itol_com==1 ) ltol = l
      tol = Rtol(ltol)*ABS(Y(l)) + Atol(ltol)
      IF( tol==0._DP ) GOTO 200
      Ewt(l) = tol
    END DO
    !
    big = SQRT(huge_dp)
    CALL DHSTRT(DF,Neq,T,Tout,Y,Yh(1,2),Ewt,1,uround_com,big,Yh(1,3),Yh(1,4),Yh(1,5),&
      Yh(1,6),h_com)
    !
    Delsgn = SIGN(1._DP,Tout-T)
    tn_com = T
    DO l = 1, Neq
      Yh(l,1) = Y(l)
      Yh(l,2) = h_com*Yh(l,2)
    END DO
    init_com = 2
  ELSE
    !        RTOL=ATOL=0 ON INPUT, SO RTOL IS CHANGED TO A
    !                                 SMALL POSITIVE VALUE
    ibegin_com = -1
    RETURN
  END IF
  !
  !              ......................................................
  !
  !                 ON EACH CALL SET INFORMATION WHICH DETERMINES THE
  !                 ALLOWED INTERVAL OF INTEGRATION BEFORE RETURNING
  !                 WITH AN ANSWER AT TOUT
  !
  100  del = Tout - T
  absdel = ABS(del)
  !
  !              ......................................................
  !
  !                 IF ALREADY PAST OUTPUT POINT, INTERPOLATE AND
  !                 RETURN
  !
  !                 BEGIN BLOCK PERMITTING ...EXITS TO 400
  !                    BEGIN BLOCK PERMITTING ...EXITS TO 380
  DO WHILE( ABS(tn_com-T)<absdel )
    !
    !                       IF CANNOT GO PAST TSTOP AND SUFFICIENTLY
    !                       CLOSE, EXTRAPOLATE AND RETURN
    !
    IF( itstop_com==1 ) THEN
      IF( ABS(Tstop-tn_com)<100._DP*uround_com*ABS(tn_com) ) THEN
        dt = Tout - tn_com
        DO l = 1, Neq
          Y(l) = Yh(l,1) + (dt/h_com)*Yh(l,2)
        END DO
        CALL DF(Tout,Y,Ypout)
        nfe_com = nfe_com + 1
        Idid = 3
        T = Tout
        told_com = T
        !        ..................EXIT
        RETURN
      END IF
    END IF
    !
    IF( .NOT. (iinteg_com==0 .OR. .NOT. Intout) ) THEN
      !
      !                          INTERMEDIATE-OUTPUT MODE
      !
      Idid = 1
      GOTO 300
      !
      !                       .............................................
      !
      !                            MONITOR NUMBER OF STEPS ATTEMPTED
      !
    ELSEIF( ksteps_com<=maxnum ) THEN
      !
      !                          ..........................................
      !
      !                             LIMIT STEP SIZE AND SET WEIGHT VECTOR
      !
      hmin_com = 100._DP*uround_com*ABS(tn_com)
      ha = MAX(ABS(h_com),hmin_com)
      IF( itstop_com==1 ) ha = MIN(ha,ABS(Tstop-tn_com))
      h_com = SIGN(ha,h_com)
      ltol = 1
      DO l = 1, Neq
        IF( itol_com==1 ) ltol = l
        Ewt(l) = Rtol(ltol)*ABS(Yh(l,1)) + Atol(ltol)
        !                    .........EXIT
        IF( Ewt(l)<=0._DP ) GOTO 200
      END DO
      Tolfac = uround_com*DVNRMS(Neq,Yh,Ewt)
      !                 .........EXIT
      IF( Tolfac<=1._DP ) THEN
        !
        !                 ...................................................
        !
        !                      TAKE A STEP
        !
        CALL DSTOD(Neq,Y,Yh,Neq,Yh1,Ewt,Savf,Acor,Wm,Iwm,DF,DJAC)
        !
        jstart_com = -2
        Intout = .TRUE.
        IF( kflag_com/=0 ) THEN
          !
          !              ......................................................
          !
          IF( kflag_com==-1 ) THEN
            !
            !                 REPEATED ERROR TEST FAILURES
            Idid = -7
            ibegin_com = -1
          ELSE
            !
            !                 REPEATED CORRECTOR CONVERGENCE FAILURES
            Idid = -6
            ibegin_com = -1
          END IF
          GOTO 300
        END IF
      ELSE
        !
        !                          TOLERANCES TOO SMALL
        Idid = -2
        Tolfac = 2._DP*Tolfac
        Rtol(1) = Tolfac*Rtol(1)
        Atol(1) = Tolfac*Atol(1)
        IF( itol_com/=0 ) THEN
          DO l = 2, Neq
            Rtol(l) = Tolfac*Rtol(l)
            Atol(l) = Tolfac*Atol(l)
          END DO
        END IF
        ibegin_com = -1
        !           ............EXIT
        GOTO 300
      END IF
    ELSE
      !
      !                          A SIGNIFICANT AMOUNT OF WORK HAS BEEN
      !                          EXPENDED
      Idid = -1
      ksteps_com = 0
      ibegin_com = -1
      GOTO 300
    END IF
  END DO
  CALL DINTYD(Tout,0,Yh,Neq,Y,intflg)
  CALL DINTYD(Tout,1,Yh,Neq,Ypout,intflg)
  Idid = 3
  IF( tn_com==Tout ) THEN
    Idid = 2
    Intout = .FALSE.
  END IF
  T = Tout
  told_com = T
  !        ..................EXIT
  RETURN
  !
  !                    RELATIVE ERROR CRITERION INAPPROPRIATE
  200  Idid = -3
  !           .........EXIT
  ibegin_com = -1
  !
  !           .........................................................
  !
  !                                  STORE VALUES BEFORE RETURNING TO
  !                                  DDEBDF
  300 CONTINUE
  DO l = 1, Neq
    Y(l) = Yh(l,1)
    Ypout(l) = Yh(l,2)/h_com
  END DO
  T = tn_com
  told_com = T
  Intout = .FALSE.
  !
  RETURN
END SUBROUTINE DLSOD