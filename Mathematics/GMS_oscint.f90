


MODULE Oscillating_Integration
! Code converted using TO_F90 by Alan Miller
! Date: 2002-05-18  Time: 18:20:57

!     ALGORITHM 639 COLLECTED ALGORITHMS FROM ACM.
!     ALGORITHM APPEARED IN ACM-TRANS. MATH. SOFTWARE, VOL.12, NO. 1,
!     MAR., 1986, P. 24.
use mod_kinds, only : i4, dp
IMPLICIT NONE



CONTAINS


! **************** BEGIN SUBROUTINE OSCINT ****************

SUBROUTINE oscint(azero, period, rfirst, eps, nquad, ndim1, ndim2, gauss,  &
                  hfun, gper, savper, weight, abscis, qlist, result, istate)


! N.B. Argument WORK has been removed.

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!      THIS ROUTINE PROVIDES AN APPROXIMATION **RESULT** TO THE INTEGRAL OF
!  AN OVERALL INTEGRAND **HFUN(X)*GPER(X)** OVER A SEMI-INFINITE INTERVAL
!  (AZERO,INFINITY).  THE OVERALL INTEGRAND FUNCTION SHOULD BE ULTIMATELY
!  OSCILLATING IN SIGN AND PERIODIC WITH USER-PROVIDED **PERIOD**.

!      THE ROUTINE CONSIDERS A SEQUENCE OF INTERVALS.  ALL THE INTERVALS,
!  EXCEPT THE FIRST, ARE OF LENGTH 0.5*PERIOD.   IT USES A QUADRATURE RULE TO
!  APPROXIMATE THE INTEGRAL OVER EACH INTERVAL.  THESE APPROXIMATIONS ARE
!  STORED IN QLIST(J),  J = 1,2,3,...  .  THE VALUES OF QLIST ULTIMATELY
!  OSCILLATE IN SIGN.  THEN THE ROUTINE USES A SERIES ACCELERATION
!  TECHNIQUE BASED ON THE EULER TRANSFORMATION TO SUM THIS SEQUENCE.


! INPUT PARAMETERS:

!   AZERO         LOWER LIMIT OF INTEGRATION

!   PERIOD        LEAST POSITIVE PERIOD OR ULTIMATE PERIOD OF
!                 INTEGRAND FUNCTION

!   RFIRST        RIGHT-HAND ENDPOINT OF FIRST INTERVAL. SUGGESTED VALUE
!                 FOR STRAIGHTFORWARD PROBLEMS IS AZERO.
!                 SEE NOTE 1 BELOW

!   EPS           THE REQUESTED ACCURACY

!   NQUAD         INTEGER(i4). NQ = ABS(NQUAD) IS THE NUMBER OFABSCISSAS TO
!                 BE USED BY THE QUADRATURE RULE IN EACH INTERVAL.
!                     NQUAD > 1    TRAPEZOIDAL RULE IS USED
!                     NQUAD < 0    RULE SPECIFIED BY GAUSS BELOW IS USED

!   NDIM1,NDIM2   DIMENSIONS OF THE OUTPUT ARRAY **WORK**.
!                 NDIM1 = 10 IMPLIES NMAX = 100
!                 NDIM1 .NE. 10 IMPLIES NMAX = MIN(100,NDIM1)
!                 NMAX IS A PHYSICAL LIMIT.  IF CALCULATION IS NOT
!                 COMPLETE AFTER NMAX INTERVALS HAVE BEEN CONSIDERED
!                 IT IS THEN ABANDONED. (SEE NOTE 5 BELOW)
!                 NDIM2  SUGGESTED VALUE IS 15.  NORMALLY SHOULD EXCEED 4

!   GAUSS         NAME OF A SUBROUTINE WHICH PROVIDES WEIGHTS
!                 AND ABSCISSAS.  WHEN NQUAD > 0 THIS IS NOT CALLED.
!                 SEE NOTE 2 BELOW FOR USE WHEN NQUAD < 0.

!   HFUN          NAME OF A FUNCTION SUBROUTINE.  THIS IS ONE FACTOR OF
!                 THE INTEGRAND FUNCTION.  THE OTHER FACTOR IS GPER BELOW

!   GPER          NAME OF A FUNCTION SUBROUTINE.  THIS IS THE
!                 CO-FACTOR OF HFUN IN THE INTEGRAND FUNCTION.
!                 GPER MUST BE PERIODIC WITH PERIOD COINCIDING WITH THE
!                 INPUT PARAMETER, PERIOD ABOVE. (SEE NOTE 3 BELOW)


! OUTPUT PARAMETERS:

!   WORK(NDIM1,NDIM2)    HOLDS COMPLETED FINITE AVERAGE TABLE

!   SAVPER( )         ARRAY TO SAVE FUNCTION VALUES OF GPER.
!                     DIMENSION NOT LESS THAN 2*ABS(NQUAD)


!   WEIGHT( )         ARRAY TO HOLD WEIGHTS FOR GAUSSIAN QUADRATURE
!                     DIMENSION NOT LESS THAN MAX(1,-NQUAD)

!   ABSCIS( )         ARRAY TO HOLD ABSCISSAS FOR GAUSSIAN QUADRATURE
!                     DIMENSION NOT LESS THAN MAX(1,-NQUAD)

!   QLIST(100)        ARRAY TO HOLD THE RESULT OF EACH QUADRATURE

!   RESULT            OVERALL RESULT OF THE INTEGRATION

!   ISTATE(6)         VECTOR OF INTEGER(i4)S GIVES STATUS OF RESULT

!     ISTATE(1)       AN INDICATOR WARNING ABOUT SUSPICIOUS RESULTS -
!                     ZERO:      THE RUN WAS APPARENTLY SUCCESSFUL.
!                     POSITIVE:  THE RUN WAS APPARENTLY SUCCESSFUL, BUT
!                                THERE ARE UNSATISFACTORY FEATURES OF
!                                POSSIBLE INTEREST TO THE SOPHISTICATED USER
!                     NEGATIVE:  UNSUCCESSFUL RUN - DISREGARD THE RESULT
!                     SEE NOTES BELOW FOR COMPLETE SPECIFICATION
!     ISTATE(2)       LSIGCH - INDICATES LAST INTERVAL IN WHICH THE SIGN
!                                OF THE INTEGRAL COINCIDED WITH THAT OF
!                                THE INTEGRAL OVER THE PREVIOUS INTERVAL
!                                SEE NOTE 4 BELOW.
!     ISTATE(3)       NOW  - THE NUMBER OF FINITE INTEGRALS, QLIST(Q),
!                                EVALUATED IN THE CALCULATION.
!     ISTATE(4)       NCOL - THE COLUMN OF THE FINITE AVERAGE TABLE,
!                                (WORK), ON WHICH THE  RESULT IS BASED.
!     ISTATE(5)       NROW - THE ROW OF THE FINITE AVERAGE TABLE,
!                                (WORK), ON WHICH THE  RESULT IS BASED.
!     ISTATE(6)       NCOUNT - THE NUMBER OF CALLS TO FUNCTION HFUN.



!  NOTE 1

!  RFIRST
!     THIS ALLOWS THE USER TO LOCATE HIS SUBDIVISION.  FOR CAUTIOUS RUNNING,
!  ARRANGE RFIRST TO COINCIDE WITH AN "ULTIMATE ZERO".  FOR SLIGHTLY MORE
!  ADVENTUROUS BUT LESS RELIABLE RUNNING, ARRANGE RFIRST TO COINCIDE WITH AN
!  "ULTIMATE" PEAK.  OTHERWISE, SET RFIRST < AZERO, IN WHICH CASE, OSCINT USES
!  AZERO INSTEAD OF  RFIRST.


!  NOTE 2

!  GAUSS
!     THIS IS THE NAME OF A USER-PROVIDED SUBROUTINE OF THE FORM
!          GAUSS(ITYPE, A, B, C, D, N, WEIGHT(N), ABSCIS(N), IFAIL)
!  IT IS CALLED ONLY WHEN NQUAD IS NEGATIVE WITH N = -NQUAD.  IT RETURNS A SET
!  OF WEIGHTS AND ABSCISSAS, SUITABLE FOR INTEGRATION OVER THE INTERVAL (-1,1).

!     THE FIRST FIVE INPUT PARAMETERS OF GAUSS SHOULD BE IGNORED.
!  IFAIL MAY BE USED FOR A WARNING MESSAGE.  IF GAUSS RETURNS
!  IFAIL .NE. 0, OSCINT ABORTS, SETTING ISTATE(1) = -4000.

!     THE USER WHO HAS THE NAG LIBRARY AVAILABLE MAY SET GAUSS = D01BCF.


!  NOTE 3

!  HFUN AND GPER
!     ONE MAY ALWAYS USE HFUN FOR THE INTEGRAND FUNCTION, F(X), AND CODE GPER
!  TO RETURN THE VALUE 1.0D0.  HOWEVER, WHEN F(X) HAS A PERIODIC FACTOR, J(X),
!  REPETITIVE EVALUATION OF J(X) IN EACH INTERVAL MAY BE AVOIDED BY SETTING
!  GPER = J(X) AND HFUN = H(X).   THE ROUTINE CHECKS THAT GPER IS INDEED
!  PERIODIC BY MAKING SOME EVALUATIONS IN THE THIRD, FOURTH, AND FIFTH
!  INTERVALS.  IF IT FINDS GPER IS NOT PERIODIC, IT TERMINATES WITH
!  ISTATE(1) = -5000.
!  IF PERIOD <= 10**-5, THE ROUTINE TERMINATES WITH ISTATE(1) = -3000.


!  NOTE 4

!  TERMINATION
!     IN THIS NOTE, THE "NORMAL SIGN CHANGE PATTERN" IS ONE IN WHICH
!  SUCCESSIVE VALUES OF QLIST(Q) OSCILLATE IN SIGN.  THE "GRACE PERIOD" IS
!  Q <= 10 WHEN THE NORMAL SIGN CHANGE PATTERN IS NOT INSISTED ON.
!  LSIGCH IS THE HIGHEST VALUE OF Q FOR WHICH QLIST(Q)*QLIST(Q-1) IS POSITIVE.
!  TERMINATION COMES ABOUT AFTER CALCULATING QLIST(NOW), WHEN EITHER

!  (A)  AN APPROXIMATION OF SUFFICIENT ACCURACY IS CURRENTLY AVAIL-
!       ABLE  (THE ROUTINE SETS ISTATE(1) = MAX(0,4-(NOW-LSIGCH))
!  OR

!  (B)  THE NORMAL SIGN CHANGE PATTERN IS VIOLATED AFTER THE GRACE
!       PERIOD.   (THE ROUTINE SETS ISTATE(1) = -200)
!   OR

!  (C)  THE USER SET LIMIT, NMAX, OF INTERVALS HAVE BEEN CALCULATED
!       I.E.  NOW = NMAX.  (THE ROUTINE SETS ISTATE(1) = -100)

!     THE ROUTINE CHECKS (A), THEN (B), AND THEN (C).  TERMINATION
!  UNDER (A) MAY OCCUR WITHOUT ANY NORMAL SIGN CHANGE PATTERN
!  EMERGING.  THIS MAY BE DUE TO MISUSE OF THE ROUTINE, BUT THE
!  PROBLEM IS SMALL ENOUGH TO BE CORRECTLY HANDLED.  IN THIS CASE,
!  ISTATE(1) = MAX(0,4-(NOW-LSIGCH)) MAY BE A SMALL POSITIVE  INTEGER(i4).


!  NOTE 5

!  VARIABLE DIMENSIONS AND STORAGE ECONOMY
!     THE PROGRAM WHICH CALLS OSCINT HAS TO PROVIDE NUMERICAL VALUES OF THE
!  INPUT PARAMETERS NQUAD, NDIM1 AND NDIM2.  IT MUST ALSO INCLUDE A DIMENSION
!  STATEMENT IN WHICH THE FIRST FIVE AND THE LAST OUTPUT PARAMETERS OF OSCINT
!  ARE DIMENSIONED.  NOTE THAT THE DIMENSIONS OF WEIGHT AND ABSCIS ARE BOTH AT
!  LEAST 1 WHEN NQUAD IS POSITIVE AND AT LEAST -NQUAD OTHERWISE.  HOWEVER, THE
!  DIMENSION OF SAVPER IS AT LEAST 2*ABS(NQUAD) REGARDLESS OF THE SIGN OF NQUAD.

!     GENERALLY, THESE VARIABLE DIMENSION STATEMENTS ALLOW ECONOMIC USE OF
!  STORAGE.  THERE IS ONE FURTHER STORAGE SAVING FEATURE.
!  WHEN NDIM1 = 10, THE ROUTINE CARRIES OUT THE SAME CALCULATION AS IT WOULD
!  IF NDIM1 = 100.  HOWEVER, INSTEAD OF USING A WORK ARRAY OF DIMENSION
!  (100,NDIM2), IT USES A WORK ARRAY OF DIMENSION (10,NDIM2) AND OVERWRITES
!  IT, AS AND WHEN NECESSARY, AS THE CALCULATION PROCEEDS.
!  IN THIS CASE, OSCINT BEHAVES AS IF NDIM2 WERE REPLACED BY  MIN(20,NDIM2).

!     SOME OBVIOUS ERRORS IN DIMENSIONING, SUCH AS NEGATIVE DIMENSIONS,
!  CAUSE THE ROUTINE TO TERMINATE WITH
!         ISTATE(1) = -6000
!  HOWEVER, INADEQUATE DIMENSIONING IN THE CALLING PROGRAM MAY GO
!  UNDETECTED AND MAY LEAD TO RANDOM OR CHAOTIC OUTPUT.


!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

REAL (dp), INTENT(IN)   :: azero
REAL (dp), INTENT(IN)   :: period
REAL (dp), INTENT(IN)   :: rfirst
REAL (dp), INTENT(IN)   :: eps
INTEGER(i4), INTENT(IN)     :: nquad
INTEGER(i4), INTENT(IN)     :: ndim1
INTEGER(i4), INTENT(IN)     :: ndim2
REAL (dp), INTENT(OUT)  :: savper(:)
REAL (dp), INTENT(OUT)  :: weight(:)
REAL (dp), INTENT(OUT)  :: abscis(:)
REAL (dp), INTENT(OUT)  :: qlist(100)
REAL (dp), INTENT(OUT)  :: result
INTEGER(i4), INTENT(OUT)    :: istate(6)

! EXTERNAL gauss, hfun, gper

INTERFACE
  SUBROUTINE GAUSS(ITYPE, A, B, C, D, N, WEIGHT, ABSCIS, IFAIL)
    IMPLICIT NONE
    INTEGER(i4), PARAMETER  :: dp = SELECTED_REAL_KIND(12, 60)
    INTEGER(i4), INTENT(IN)     :: itype
    REAL (dp), INTENT(IN)   :: a, b, c, d
    INTEGER(i4), INTENT(IN)     :: n
    REAL (dp), INTENT(OUT)  :: weight(:), abscis(:)
    INTEGER(i4), INTENT(OUT)    :: ifail
  END SUBROUTINE GAUSS

  FUNCTION hfun(x) RESULT(fn_val)
    IMPLICIT NONE
    INTEGER(i4), PARAMETER  :: dp = SELECTED_REAL_KIND(12, 60)
    REAL (dp), INTENT(IN)  :: x
    REAL (dp)              :: fn_val
  END FUNCTION hfun

  FUNCTION gper(x) RESULT(fn_val)
    IMPLICIT NONE
    INTEGER(i4), PARAMETER  :: dp = SELECTED_REAL_KIND(12, 60)
    REAL (dp), INTENT(IN)  :: x
    REAL (dp)              :: fn_val
  END FUNCTION gper
END INTERFACE

REAL (dp)  :: curr, hasper, prev, s, wmin, work(ndim1,ndim2)
INTEGER(i4)    :: i, ii, isub, j, jp, k, mj, mj1, mj2, mjk, mjm2, nmax, nml,  &
              now, nowjp, nround, nrow

hasper = .5D0 * period
wmin = 1.0D0
istate(1:6) = 0
IF (nquad == 0 .OR. nquad == 1 .OR. ndim1 < 1 .OR. ndim2 < 1) istate(1) = -6000
IF (period < 10.0D-5) istate(1) = -5000
IF (istate(1) >= 0) THEN
  IF (ndim1 == 10) THEN
    nmax = 100
    
  ELSE
    nmax = MIN(100,ndim1)
  END IF
  
  jp = 0
  s = 0.0D0
  
!      LOOP TO CONSTRUCT TABLE
  
  20 IF (jp == nmax) istate(1) = -100
  IF (istate(1) == 0) THEN
    
    k = 0
    jp = jp + 1
    j = MIN(jp,ndim2)
    IF (ndim1 == 10) j = MIN(j,20)
    30 IF (k < j) THEN
      k = k + 1
      IF (ndim1 == 10) THEN
        mj = MOD(jp,10)
        mjm2 = MOD(jp-2,10)
        mj1 = MOD(jp-k+1,10)
        mj2 = MOD(jp-k+2,10)
        mjk = MOD(jp-k,10)
        IF (mj == 0) mj = 10
        IF (mjm2 == 0) mjm2 = 10
        IF (mj1 == 0) mj1 = 10
        IF (mj2 == 0) mj2 = 10
        IF (mjk == 0) mjk = 10
        
      ELSE
        mj = jp
        mjm2 = jp - 2
        mj1 = jp - k + 1
        mj2 = jp - k + 2
        mjk = jp - k
      END IF
      
      IF (k == 1) THEN
        work(mj,k) = qrule(jp-1, azero, hasper, rfirst, nquad, ndim1, ndim2, &
                           gauss, hfun, gper, savper, weight, abscis, qlist, &
                           istate)
        
      ELSE
        work(mj1,k) = (work(mj1,k-1) + work(mj2,k-1)) / 2.0D0
        IF (ABS(work(mj1,k)) < wmin) THEN
          wmin = ABS(work(mj1,k))
          now = k
          nrow = mj1
          nowjp = jp
        END IF
        
        curr = ABS(work(mj1,k))
        prev = ABS(work(mjk,k))
        IF (jp /= k) THEN
          IF (curr < eps .AND. prev < eps) THEN
            now = k
            nrow = mj1
            nowjp = jp
            GO TO 40
            
          END IF
          
        END IF
        
      END IF
      
      GO TO 30
      
    END IF
    
    GO TO 20
  END IF
  
  40 DO  i = 1, nowjp - now
    s = s + qlist(i)
  END DO
  IF (ndim1 == 10) THEN
    nround = now - 10
    IF (nround <= 0) nround = 0
    IF (nround > 0) THEN
      
      DO  i = nowjp - now + 1, nowjp - 10
        s = s + qlist(i)
      END DO
      isub = nowjp - 9
      70 IF (isub > 10) isub = isub - 10
      IF (isub > 10) GO TO 70
      DO  j = 1, nround
        s = s + work(isub,j) / 2.0D0
      END DO
      DO  i = nrow, nrow + nround - 1
        ii = i
        IF (i > 10) ii = i - 10
        s = s - work(ii,nround+1)
      END DO
    END IF
  END IF
  
  DO  j = nround + 1, now - 1
    s = s + work(nrow,j) / 2.0D0
  END DO
  s = s + work(nrow,now)
  result = s
  istate(3) = jp
  istate(4) = now
  istate(5) = nrow
  nml = istate(3) - istate(2)
  IF (nml < 4 .AND. istate(1) == 0) istate(1) = MAX(0,4-nml)
END IF
RETURN
END SUBROUTINE oscint

! ******** END OF OSCINT *********



! *********** BEGIN FUNCTION QRULE ************

FUNCTION qrule(j, azero, hasper, rfirst, nquad, ndim1, ndim2, gauss, hfun,  &
               gper, savper, weight, abscis, qlist, istate) RESULT(fn_val)

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!          THIS ROUTINE EVALUATES THE INTEGRAL OF THE FUNCTION,
!     HFUN(X)*GPER(X) OVER THE INTERVAL (A,B) WHERE:

!          GENERALLY (J>0)     A = PREVIOUS VALUE OF B
!                              B = A + HASPER
!                    (J=0)     A = AZERO
!                              B = RFIRST WHEN RFIRST > AZERO
!                              B = AZERO + HASPER WHEN RFIRST <= AZERO

!      INPUT PARAMETERS:

!               J                  DEFINES WHICH TERM, QLIST(J), OF
!                                  THE SERIES IS BEING EVALUATED

!               HASPER             HALF THE PERIOD


!      OTHER INPUT AND OUTPUT PARAMETERS ARE IDENTICAL TO THOSE IN
!      OSCINT (DESCRIBED ABOVE).

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

INTEGER(i4), INTENT(IN)     :: j
REAL (dp), INTENT(IN)   :: azero
REAL (dp), INTENT(IN)   :: hasper
REAL (dp), INTENT(IN)   :: rfirst
INTEGER(i4), INTENT(IN)     :: nquad
INTEGER(i4), INTENT(IN)     :: ndim1
INTEGER(i4), INTENT(IN)     :: ndim2
REAL (dp), INTENT(OUT)  :: savper(:)
REAL (dp), INTENT(OUT)  :: weight(:)
REAL (dp), INTENT(OUT)  :: abscis(:)
REAL (dp), INTENT(OUT)  :: qlist(100)
INTEGER(i4), INTENT(OUT)    :: istate(6)
REAL (dp)               :: fn_val

! SAVE b, tendpt
! EXTERNAL gauss, hfun, gper

INTERFACE
  SUBROUTINE GAUSS(ITYPE, A, B, C, D, N, WEIGHT, ABSCIS, IFAIL)
    IMPLICIT NONE
    INTEGER(i4), INTENT(IN)     :: itype
    REAL (dp), INTENT(IN)   :: a, b, c, d
    INTEGER(i4), INTENT(IN)     :: n
    REAL (dp), INTENT(OUT)  :: weight(:), abscis(:)
    INTEGER(i4), INTENT(OUT)    :: ifail
  END SUBROUTINE GAUSS

  FUNCTION hfun(x) RESULT(fn_val)
    IMPLICIT NONE
    REAL (dp), INTENT(IN)  :: x
    REAL (dp)              :: fn_val
  END FUNCTION hfun

  FUNCTION gper(x) RESULT(fn_val)
    IMPLICIT NONE
    REAL (dp), INTENT(IN)  :: x
    REAL (dp)              :: fn_val
  END FUNCTION gper
END INTERFACE

INTEGER(i4)    :: i, ielm, ifail, INDEX, itype, npts
REAL (dp)  :: a, aa, bb, cc, dd, diff, fun, gmax, xi, y, wsum, wt
REAL (dp), SAVE  :: b, tendpt

npts = ABS(nquad)
istate = 0

INDEX = 0
IF (j == 0) THEN
  qlist(1:100) = 0.0_dp

!        CALL GAUSS IF NQUAD < 0
  IF (nquad < 0) THEN
    itype = 0
    aa = -1.0D0
    bb = 1.0D0
    cc = 0.0D0
    dd = 0.0D0
    ifail = 0
    CALL gauss(itype, aa, bb, cc, dd, npts, weight, abscis, ifail)
    IF (ifail /= 0) THEN
      istate(1) = -4000
      GO TO 30
      
    END IF
    
  END IF
  
END IF

fn_val = 0.0D0

!     SET INTERVAL ENDPOINTS, A AND B
IF (j /= 0) THEN
  a = b
  
ELSE
  a = azero
END IF

IF (rfirst <= azero .OR. j /= 0) THEN
  b = a + hasper
  
ELSE
  b = rfirst
END IF

!     LOOP ON ABSCISSAS FOR INTERVAL #J - STARTS TO CALCULATE QRULE
DO  i = 1, npts
  xi = i
  
!        CALCULATE ABSCISSA Y
  IF (nquad < 0) THEN
    y = (b-a) * abscis(i) / 2.0D0 + (b+a) / 2.0D0
    wt = weight(i)
    
  ELSE
    y = ((xi-1)*b + a*(npts-i)) / (npts-1)
    wt = 1.0D0
  END IF
  
!     IELM IS LOCATION IN SAVPER FOR GPER FUNCTION VALUES
!     J=0: IGNORES.  J=1,2: WHERE TO PUT VALUE.  J>2: WHERE TO GET VALUE FROM

  ielm = npts * MOD(j-1,2) + i
  
  IF (j <= 0 .OR. i /= 1 .OR. nquad <= 0) THEN
    IF (j == 0) THEN
      fun = hfun(y) * gper(y)
      
    ELSE IF (j < 3) THEN
      savper(ielm) = gper(y)

!           CHECK FOR CONSTANT FUNCTION
      IF (savper(ielm) == savper(1)) INDEX = INDEX + 1
      IF (ABS(savper(ielm)) > ABS(savper(ielm-1))) THEN
        gmax = ABS(savper(ielm))
      END IF
      
      fun = hfun(y) * savper(ielm)
      
    ELSE IF (j < 5) THEN

!           CHECK THAT GPER IS PERIODIC WITH PERIOD=PERIOD
      diff = ABS(savper(ielm) - gper(y))
      IF (diff < gmax*1.0D-5) THEN
        fun = hfun(y) * savper(ielm)
        
      ELSE
        istate(1) = -3000
      END IF
      
    ELSE
      IF (INDEX == 2*npts) THEN
        fun = hfun(y)
        
      ELSE
        fun = hfun(y) * savper(ielm)
      END IF
      
    END IF
    
    istate(6) = istate(6) + 1
  END IF
  
  IF (nquad > 0) THEN
    IF (i == 1) THEN
      IF (j == 0) fun = fun / 2.0D0
      IF (j > 0) fun = tendpt
    END IF
    
    IF (i == npts) THEN
      fun = fun / 2.0D0
      tendpt = fun
    END IF
    
    fn_val = fn_val + fun
    
  ELSE
    fn_val = fn_val + wt * fun
  END IF
  
END DO

!     LOOP ON ABSCISSA FOR INTERVAL #J ENDS
IF (INDEX == 2*npts .AND. savper(1) /= 1) THEN
  fn_val = fn_val * savper(1)
END IF

IF (nquad > 0) wsum = npts - 1
IF (nquad < 0) wsum = 2.0D0
fn_val = fn_val * (b-a) / wsum
qlist(j+1) = fn_val

!  N.B. This point can be reached with J = 0
IF (j > 0) THEN
  IF (fn_val*qlist(j) > 0) istate(2) = j
END IF
IF (istate(2) > 9) istate(1) = -200

30 RETURN
END FUNCTION qrule

END MODULE Oscillating_Integration




! **************** BEGIN Driver Program ****************


MODULE oscsub

use mod_kinds, only : i4, dp
IMPLICIT NONE


! COMMON /oscsub/ pi, halfpi, ebase, alpha, const6, nix(10), numfun, nb

REAL (dp), SAVE  :: alpha, const6, ebase, halfpi, pi
INTEGER(i4), SAVE    :: nb, nix(10), numfun

END MODULE oscsub


#if 0
PROGRAM drive1
 
! Code converted using TO_F90 by Alan Miller
! Date: 2002-05-18  Time: 18:21:02
 
!   FILE CONTAINS MAIN, EXACT, HFUN, GPER, G5AND9, RJBESL AND DGAMMA.
!   THE FINAL TWO ARE HIGH QUALITY SOFTWARE WRITTEN BY W.J.CODY.
!   THE FIRST FOUR ARE A MEDIUM QUALITY DRIVER AND SUBROUTINES
!   TO RUN THE SUBMITTED SUBROUTINE OSCINT.
!   THESE FOUR SHARE A LABELLED COMMON LIST.

!      THIS PROGRAM TESTS THE SUBROUTINE, OSCINT, FOR FOUR GIVEN FUNCTIONS
!   (SPECIFIED BY NUMFUN).  THE RESULTS ARE COMPARED TO THE EXACT INTEGRALS
!   FOUND USING ANALYTIC FORMULAS IN THE SUBROUTINE EXACT.

!   CENTRAL LOOP IS ON NUMFUN = 1,4.  THIS IS INSIDE A HAND CODED LOOP,
!   NTIME  = 1,4.  WHEN  NTIME = 1, THE RESULTS ONLY ARE PRINTED.
!   WHEN NTIME = 2, SOME OF THE ABSCISSAS AND FUNCTION VALUES ARE PRINTED,
!   ALONG WITH THE FINITE AVERAGE TABLE.
!   WHEN NTIME = 3 AND 4, MINOR ALTERATIONS ARE MADE IN THE INPUT PARAMETERS
!   WHICH ILLUSTRATE SOME FEATURES OF THE METHOD AND HOW THINGS CAN GO WRONG
!   AND WHETHER OR NOT THE ROUTINE GIVES ANY WARNING.

USE Oscillating_Integration
USE oscsub
IMPLICIT NONE

CHARACTER (LEN=12)  :: gname
CHARACTER (LEN=52)  :: notes(4,4), com1(4,2), com2(4,2), com3(4,2), hname, m52

! EXTERNAL hfun, gper, g5and9

INTERFACE
  SUBROUTINE g5and9(ITYPE, A, B, C, D, N, WEIGHT, ABSCIS, IFAIL)
    IMPLICIT NONE
    INTEGER(i4), PARAMETER  :: dp = SELECTED_REAL_KIND(12, 60)
    INTEGER(i4), INTENT(IN)     :: itype
    REAL (dp), INTENT(IN)   :: a, b, c, d
    INTEGER(i4), INTENT(IN)     :: n
    REAL (dp), INTENT(OUT)  :: weight(n), abscis(n)
    INTEGER(i4), INTENT(OUT)    :: ifail
  END SUBROUTINE g5and9

  FUNCTION hfun(x) RESULT(fn_val)
    IMPLICIT NONE
    INTEGER(i4), PARAMETER  :: dp = SELECTED_REAL_KIND(12, 60)
    REAL (dp), INTENT(IN)  :: x
    REAL (dp)              :: fn_val
  END FUNCTION hfun

  FUNCTION gper(x) RESULT(fn_val)
    IMPLICIT NONE
    INTEGER(i4), PARAMETER  :: dp = SELECTED_REAL_KIND(12, 60)
    REAL (dp), INTENT(IN)  :: x
    REAL (dp)              :: fn_val
  END FUNCTION gper

  FUNCTION exact() RESULT(fn_val)
    USE oscsub
    IMPLICIT NONE
    INTEGER(i4), PARAMETER  :: dp = SELECTED_REAL_KIND(12, 60)
    REAL (dp)  :: fn_val
  END FUNCTION exact
END INTERFACE

REAL (dp)  :: azero, cin, eps, period, qlist(100), result, rfirst
REAL (dp)  :: savper(128), weight(18), abscis(18)
INTEGER(i4)    :: i, j, istate(6), k, ndim1, ndim2, nquad, nrun, ncol,  &
              nptop, nterms, ntime, ntm2

! COMMON /oscsub/ pi, halfpi, ebase, alpha, const6, nix(10), numfun, nb

!     SET UP THE NOTES. THESE APPEAR BEFORE EACH RUN.
notes(1,1) = ' FIRST TIME. BRIEF OUTPUT. NOTE EPS AND DIFFERENCE  '
notes(1,2) = ' SECOND TIME. SAME AS FIRST BUT WITH DETAILED OUTPUT'
notes(1,3) = ' NOW NDIM1 = 10. SPECIAL VALUE FOR SPACE ECONOMY.   '
notes(1,4) = ' PERIOD WRONG. IT SHOULD BE TWOPI = 0.628...+01     '
DO  k = 1, 4
  notes(2:4,k) = notes(1,k)
END DO

notes(2,3) = ' NOW NQUAD = -9 FOR 9 POINT GAUSS QUADRATURE RULE   '
notes(3,3) = ' NOW NDIM1 = 5, TO ILLUSTRATE NO CONVERGENCE.       '
notes(4,3) = ' NOW NDIM2 = 11, TO ILLUSTRATE VARIANT BEHAVIOR     '

!     SET UP THE COMMENTS. THESE APPEAR AFTER EACH RUN.
m52 = ' MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM '
com3(1:4, 1:2) = m52

com1(1,2) = ' NOTE ISTATE(1), ISTATE(6).  NEGATIVE PERIOD NOTICED'
com2(1,2) = ' BEFORE ANY SERIOUS COMPUTATION.                    '
com1(2,2) = ' WRONG PERIOD NOTICED QUICKLY SINCE GPER = COS(X)   '
com2(2,2) = ' WAS FOUND NOT TO HAVE CLAIMED PERIOD.              '
com1(3,2) = ' THIS EASY CALCULATION ENDED DURING THE SIGN CHANGE '
com2(3,2) = ' GRACE PERIOD. (YOU CANT LOOSE EM ALL.)             '
com1(4,2) = ' SINCE GPER = 1, WRONG PERIOD WAS NOT NOTICED, BUT  '
com2(4,2) = ' SIGN CHANGE PATTERN ULTIMATELY LED TO TERMINATION  '
com3(4,2) = ' AFTER THE GRACE PERIOD.    MMMMMMMMMMMMMMMMMMMMMM  '

com1(1,1) = ' NOTE ONLY SMALL ARRAY NEEDED. THE READER MAY       '
com2(1,1) = ' IDENTIFY THIS SUBSET OF THE PREVIOUS NUMFUN=1 TABLE'
com1(2,1) = ' THIS QUADRATURE RULE INADEQUATE. NOTE THE STEADY   '
com2(2,1) = ' CONVERGENCE OF THE TABLE ELEMENTS BELOW TO A RESULT'
com3(2,1) = ' WHICH IS IN FACT WRONG. ROUTINE CANNOT KNOW THIS.  '
com1(3,1) = ' BECAUSE NDIM1 = 5, BUT THE WORK ARRAY HAS DIMENSION'
com2(3,1) = ' 100, THE ELEMENTS APPEAR IN UNFAMILIAR LOCATIONS.  '
com1(4,1) = ' THE PREVIOUSLY CHOSEN ELEMENT (16,14) UNREACHABLE. '
com2(4,1) = ' EQUALLY GOOD, BUT MARGINALLY MORE EXPENSIVE        '
com3(4,1) = ' ELEMENT (25,11) IS CHOSEN INSTEAD.     MMMMMMMMMMM '

!      CONSTANTS, ETC.USED IN SUBROUTINES.
ebase = EXP(1.0D0)
pi = 4.0D0 * ATAN(1.0D0)
halfpi = pi / 2.0D0
cin = 1.0D0 + SQRT(2.0D0)
const6 = 1.0D0 / LOG(cin)
alpha = 0.0D0
nb = 2

ntime = 1
!   HAND CODED LOOP ON NTIME = 1,2,3,4  STARTS HERE.

!   NOW, SET THE CODE-CHECK OUTPUT PARAMETERS.
!   SET VALUES OF NIX TO PRINT VARIOUS CODE CHECKS
!       NIX(1) > 0    CAUSES VALUE RETURNED FROM XINT TO BE PRINTED
!                     IN THE MAIN ROUTINE FOR THE FIRST N CALLS
!       NIX(2) > 0    CAUSES FIRST NIX(2) ROWS OF THE FORWARD AVERAGE
!                     TABLE TO BE PRINTED.
!       NIX(3) > 0    CAUSES OUTPUT FROM FUN TO BE PRINTED FOR THE
!                     FIRST N CALLS

50 nix(1) = 0
nix(3) = 0
IF (ntime > 1) nix(2) = 40


DO  numfun = 1, 4
  
  nrun = 10 * ntime + numfun
  IF (ntime == 2) nix(1) = 12
  IF (ntime == 2) nix(3) = 12
  
!      FIRST, CLEAR VARIOUS WORK SPACES.  THIS IS UNNECESSARY FOR RUNNING,
!      BUT IS USEFUL FOR TIDY OUTPUT.
  
  qlist(1:100) = 0.0D0
  result = 0.0D0
  
!     SECOND, SET THE INPUT PARAMETERS FOR OSCINT.
  
!     AZERO IS THE LOWER LIMIT OF INTEGRATION
  azero = 0.0D0
  
  period = 2.0D0 * pi
  IF (numfun == 6) period = pi
  
!     RFIRST IS THE RIGHT-HAND ENDPOINT OF THE FIRST INTERVAL
  rfirst = 0.0D0
  
!     NQUAD REPRESENTS THE TYPE OF QUADRATURE RULE USED
!           NQUAD = -5    5 PT GAUSS RULE
!           NQUAD = -9    9 PT GAUSS RULE
!           NQUAD = N,N>0 (N ODD) N POINT TRAPEZOIDAL RULE(N-1 PANELS)
  nquad = -9
  IF (numfun == 2) nquad = 64
  
!     SET EPSILON, THE REQUESTED TOLERANCE
  eps = 1.0D-13
  
!     NDIM1 AND NDIM2 ARE THE DIMENSIONS OF THE WORK TABLE.  THEY MUST BE
!     <= THE DIMENSIONS GIVEN TO WORK IN THE DIMENSION STATEMENT
  ndim1 = 100
  ndim2 = 14
  
!     WHEN NTIME = 1  ONLY PRINCIPALRESULTS
!     WHEN NTIME = 2  FULL FINITE AVERAGE TABLE AND  OTHER CODE CHECK.
!     WHEN NTIME = 3 OR 4 INPUT CHANGES (MISTAKES,ETC.)
  
  IF (ntime == 3) THEN
    IF (numfun == 1) ndim1 = 10
    IF (numfun == 2) nquad = -9
    IF (numfun == 3) ndim1 = 5
    IF (numfun == 4) ndim2 = 11
  END IF
  
  IF (ntime >= 4) THEN
    IF (numfun == 1) period = -pi
    IF (numfun == 2) period = pi
    IF (numfun == 3) period = pi
    IF (numfun == 4) period = pi
  END IF
  
!      NOW, PRINT OUT THE INPUT PARAMETERS BEFORE THE CALL TO OSCINT.
  
  gname = ' 1.0D0      '
  IF (numfun == 2) gname = ' COS(X)     '
  IF (numfun == 6) gname = ' C4(X) -3/8 '
  IF (numfun == 1) hname = 'SIN(X)/X'
  IF (numfun == 2) hname = '1.0D0/(1+X**2)'
  IF (numfun == 3) hname = 'EXP(-X*SINH(PI/2)*J1(X)'
  IF (numfun == 4) hname =  &
                   'CONST(1 - EXP(X)) * J0(X) / X.  (QUADPACK, PAGE 118)'
  IF (numfun == 5) hname = '(1 + 100*EXP(-X**2/PI**2))*J1(X)'
  IF (numfun == 6) hname = '(COS**4(X) - 3/8)/(1 + X**2)'
  
  WRITE(*, 5400)
  WRITE(*, 5000) numfun, gname, hname
  WRITE(*, 5100) notes(numfun,ntime), m52
  WRITE(*, 5200) azero, period, rfirst, eps, nquad
  WRITE(*, 5300) 'G5AND9', ndim1, ndim2
  WRITE(*, 5400)
  IF (nix(1) > 0 .OR. nix(3) > 0) WRITE(*, 5500)
  
  
  IF (nrun /= 31) THEN
    CALL oscint(azero, period, rfirst, eps, nquad, ndim1, ndim2, g5and9,  &
                hfun, gper, savper, weight, abscis, qlist, result, istate)
  ELSE
    
    CALL oscint(azero, period, rfirst, eps, nquad, ndim1, ndim2, g5and9,  &
                hfun, gper, savper, weight, abscis, qlist, result, istate)
  END IF
  
!      NOW, PRINT OUT THE RESULTS.
  
  WRITE(*, 5600)
  WRITE(*, 5700) result, exact(), exact() - result
  WRITE(*, 5800) (istate(i),i=1,5)
  IF (ntime <= 2) WRITE(*, 5900) istate(6)
  ntm2 = ntime - 2
  IF (ntime >= 3) WRITE(*, 6000) com1(numfun,ntm2),  &
                  com2(numfun,ntm2), istate(6), com3(numfun,ntm2)
  WRITE(*, 5000) numfun, gname, hname
  WRITE(*, 5400)
  
  IF (nix(2) > 0) THEN
    nterms = istate(3)
    ncol = istate(4)
    nptop = MIN(nterms,nix(2))
    IF (nrun /= 31) THEN
      
      IF (nrun == 33) nptop = 25
    ELSE
      
      nptop = MIN(10,nix(2))
      nptop = MIN(nterms,nptop)
    END IF
    
    WRITE(*, 5000) numfun, gname, hname
    WRITE(*, 5400)
  END IF
  
  WRITE(*, 6300)
END DO

ntime = ntime + 1
IF (ntime < 5) GO TO 50

5000 FORMAT (/' NUMFUN =  ',i1, '     F(X) = G(X).H(X)     G(X) = ',  &
             a12, '  H(X) = ', a52)
5100 FORMAT (/' INPUT PARAMETERS FOR OSCINT-', t40, a52/ t40, a52/)
5200 FORMAT (t5, 'AZERO', t22, 'PERIOD', t40, 'RFIRST', t58, 'EPS', t73, 'NQUAD'/ &
             g13.6, t19, g12.6, t37, g12.6, t55, g12.6, t73, i3/)
5300 FORMAT (/t5, '(GAUSS)          (HFUN)            (GPER)            NDIM1', &
             '          NDIM2'/ t5, a6, t59, i3, t73, i3/)
5400 FORMAT ('---------------------------------------------------------',  &
             '-----------------------')
5500 FORMAT (/'    CODE CHECK OUTPUT FROM HFUN, GPER, QRULE'/)
5600 FORMAT (/' PRINCIPAL OUTPUT PARAMETERS-'//)
5700 FORMAT (t6, 'RESULT', t32, 'EXACT', t54, 'DIFFERENCE'/   &
             g17.8, t28, g16.8, t52, g16.8//)
5800 FORMAT (t5, 'ISTATE(1)     ISTATE(2)     ISTATE(3)     ISTATE(4)',  &
             '       ISTATE(5)'/  &
             t5, '(IFAIL)       (LSIGCH)        (N)       (COLUMN NO.)', &
             '      (ROW NO.)'/   &
             i10, t22, i3, t35, i3, t48, i3, t62, i4//)

5900 FORMAT (t5, 'ISTATE(6)'/ ' NO. OF FUNCTION VALUES'/ t5, i4//)
6000 FORMAT (t5, 'ISTATE(6)', t40, a52/ ' NO. OF FUNCTION VALUES', t40, a52/  &
             t5, i4, t40, a52//)
6100 FORMAT (i4, 10g12.4/)
6200 FORMAT (i4, 14g9.2/)
6300 FORMAT (///)
END PROGRAM drive1

#endif

FUNCTION exact() RESULT(fn_val)

!      EXACT IS THE VALUE OF THE INTEGRAL OF F(X) OVER (0,INFINITY).
!      HERE, OF COURSE, F(X) = H(X).G(X) GIVEN BY HFUN AND GPER BELOW.

USE oscsub
IMPLICIT NONE


REAL (dp)  :: fn_val

! COMMON /oscsub/ pi, halfpi, ebase, alpha, const6, nix(10), numfun, nb

fn_val = 0.0D0
IF (numfun == 1) fn_val = halfpi
IF (numfun == 2) fn_val = pi / (2.0D0*ebase)
IF (numfun == 3) THEN
  fn_val = EXP(-(1.0D0 + alpha)*halfpi) / COSH(halfpi)
END IF

IF (numfun == 4) fn_val = 1.0D0
IF (numfun == 6) THEN
  fn_val = pi / 16.0D0 * (ebase**(-4) + 4.0D0*ebase**(-2))
END IF

RETURN
END FUNCTION exact



FUNCTION hfun(x) RESULT(fn_val)

USE oscsub
IMPLICIT NONE


REAL (dp), INTENT(IN)  :: x
REAL (dp)              :: fn_val

! EXTERNAL rjbesl

! COMMON /oscsub/ pi, halfpi, ebase, alpha, const6, nix(10), numfun, nb
REAL (dp)  :: b(100), f1, sinchu, u
INTEGER(i4)    :: ncalc

!      ALPHA - FRACTIONAL PART OF ORDER FOR WHICH BESSEL FUNCTION IS TO
!              BE CALCULATED, 0 <= ALPHA < 1
!      NB      NUMBER OF BESSEL FUNCTIONS IN SEQUENCE.

!CCC      IF (NUMFUN .EQ. 1) F(X) = SIN(X)/X
!CCC      IF (NUMFUN .EQ. 2) F(X) = COS(X)/(1+X**2)
!CCC      IF (NUMFUN .EQ. 3) F(X) = EXP(-X*SINH(PI/2)*J1(X)
!CCC      IF (NUMFUN .EQ. 4) F(X) = QUADPACK 5.2.3. PAGE118
!CCC      IF (NUMFUN .EQ. 5) F(X) = (1 + 100*EXP(-X**2/PI**2))*J1(X)
!CCC      IF (NUMFUN .EQ. 6) F(X) = (COS**4(X) - 3/8)/(1 + X**2)

!CCC      WHEN NUMFUN IS 1,3,4 OR 5, H(X) = F(X).
!CCC      WHEN NUMFUN IS 2 OR 6,     H(X) = 1/(1 + X**2)

IF (numfun == 1) THEN
  fn_val = 1.0D0 - x ** 2 / 6.0D0 + x ** 4 / 120.0D0
  IF (ABS(x) > 10.0D-4) fn_val = SIN(x) / x
ELSE
  
  IF (numfun == 2) fn_val = 1.0D0 / (1.0D0 + x**2)
  IF (numfun < 6) THEN
    IF (numfun >= 3) CALL rjbesl(x, alpha, nb, b, ncalc)
    IF (numfun == 5) fn_val = 1.0D0 + 100.0_dp*EXP(-(x/pi)**2) * b(2)
    
    IF (numfun == 3) THEN
      u = halfpi
      sinchu = (ebase**u - ebase**(-u)) / 2.0D0
      fn_val = ebase ** (-x*sinchu) * b(2)
    END IF
    
    IF (numfun == 4) THEN
      IF (x < 10.0D-8) f1 = 1.0D0
      IF (x >= 10.0D-8) f1 = (1.0D0 - EXP(-x)) / x
      fn_val = f1 * const6 * b(1)
    END IF
  END IF
  
  IF (numfun == 6) fn_val = 1.0D0 / (1.0D0 + x**2)
END IF

IF (nix(3) > 0) THEN
  nix(3) = nix(3) - 1
  WRITE(*, 5000) x, fn_val
END IF

RETURN

5000 FORMAT ('   X = ', g22.16,'   HFUN = ', g22.16)
END FUNCTION hfun




FUNCTION gper(x) RESULT(fn_val)

USE oscsub
IMPLICIT NONE


REAL (dp), INTENT(IN)  :: x
REAL (dp)              :: fn_val

! COMMON /oscsub/ pi, halfpi, ebase, alpha, const6, nix(10), numfun, nb

fn_val = 1.0D0
IF (numfun == 2) fn_val = COS(x)
IF (numfun == 6) fn_val = COS(x) ** 4 - 0.375_dp
IF (nix(3) /= 0) WRITE(*, 5000) x, fn_val
RETURN

5000 FORMAT ('   X = ', g22.16, '   GPER = ', g22.16)
END FUNCTION gper



SUBROUTINE g5and9(itype, a, b, c, d, nquad, weight, abscis, ierr)

!   THIS IS AN EXTRACT FROM A QUADRATURE ROUTINE CONSTRUCTED ONLY FOR
!   USE IN A DRIVER WHICH ILLUSTRATES OSCINT.  A NAG LIBRARY
!   SUBSCRIBER MAY REPLACE THIS BY D01BCF FOR GENERAL USE.

IMPLICIT NONE


INTEGER(i4), INTENT(IN)     :: itype
REAL (dp), INTENT(IN)   :: a
REAL (dp), INTENT(IN)   :: b
REAL (dp), INTENT(IN)   :: c
REAL (dp), INTENT(IN)   :: d
INTEGER(i4), INTENT(IN)     :: nquad
REAL (dp), INTENT(OUT)  :: weight(nquad)
REAL (dp), INTENT(OUT)  :: abscis(nquad)
INTEGER(i4), INTENT(OUT)    :: ierr

INTEGER(i4)    :: i

!      THE FOLLOWING DATA ARE WEIGHTS AND ABSCISSAS OF THE FIVE POINT
!      AND THE NINE POINT GAUSS-LEGENDRE QUADRATURE RULES RESPECTIVELY.
!      NORMALISED TO THE INTERVAL (-1,1).
REAL (dp), PARAMETER  :: wt5(5)  = (/   &
           .2369268850561891_dp, .4786286704993665_dp, .5688888888888889_dp,  &
           .4786286704993665_dp, .2369268850561891_dp /)
REAL (dp), PARAMETER  :: absc5(5) = (/    &
          -.9061798459386640_dp, -.5384693101056831_dp, 0.0_dp,  &
           .5384693101056830_dp, .9061798459386640_dp /)
REAL (dp), PARAMETER  :: wt9(9) = (/     &
           .0812743883615745_dp, .1806481606948574_dp, .2606106964029355_dp, &
           .3123470770400029_dp, .3302393550012598_dp, .3123470770400029_dp, &
           .2606106964029356_dp, .1806481606948575_dp, .8127438836157467D-01 /)
REAL (dp), PARAMETER  :: absc9(9) = (/    &
          -.9681602395076261_dp, -.8360311073266358_dp, -.6133714327005904_dp, &
          -.3242534234038090_dp, 0.0_dp, .3242534234038087_dp,    &
           .6133714327005902_dp, .8360311073266357_dp, .9681602395076260_dp /)

ierr = 59

IF (nquad == 5) THEN
  ierr = 0
  DO  i = 1, 5
    abscis(i) = absc5(i)
    weight(i) = wt5(i)
  END DO
END IF

IF (nquad == 9) THEN
  ierr = 0
  DO  i = 1, 9
    abscis(i) = absc9(i)
    weight(i) = wt9(i)
  END DO
END IF

RETURN
END SUBROUTINE g5and9



SUBROUTINE rjbesl(x, alpha, nb, b, ncalc)
!-----------------------------------------------------------

!  THIS ROUTINE CALCULATES BESSEL FUNCTIONS J SUB(N+ALPHA) (X)
!  FOR NON-NEGATIVE ARGUMENT X, AND NON-NEGATIVE ORDER N+ALPHA.


! EXPLANATION OF VARIABLES IN THE CALLING SEQUENCE

! X     - WORKING PRECISION NON-NEGATIVE REAL ARGUMENT FOR WHICH
!         J'S ARE TO BE CALCULATED.
! ALPHA - WORKING PRECISION FRACTIONAL PART OF ORDER FOR WHICH
!         J'S OR EXPONENTIALLY SCALED J'S (J*EXP(X)) ARE
!         TO BE CALCULATED.  0 .LE. ALPHA < 1.0.
! NB    - INTEGER(i4) NUMBER OF FUNCTIONS TO BE CALCULATED, NB > 0.
!         THE FIRST FUNCTION CALCULATED IS OF ORDER ALPHA, AND THE
!         LAST IS OF ORDER (NB - 1 + ALPHA).
! B     - WORKING PRECISION OUTPUT VECTOR OF LENGTH NB.  IF THE ROUTINE
!         TERMINATES NORMALLY (NCALC=NB), THE VECTOR B CONTAINS THE
!         FUNCTIONS J/ALPHA/(X) THROUGH J/NB-1+ALPHA/(X), OR THE
!         CORRESPONDING EXPONENTIALLY SCALED FUNCTIONS.
! NCALC - INTEGER(i4) OUTPUT VARIABLE INDICATING POSSIBLE ERRORS.
!         BEFORE USING THE VECTOR B, THE USER SHOULD CHECK THAT
!         NCALC=NB, I.E., ALL ORDERS HAVE BEEN CALCULATED TO
!         THE DESIRED ACCURACY.  SEE ERROR RETURNS BELOW.


! EXPLANATION OF MACHINE-DEPENDENT CONSTANTS

!   IN THE FOLLOWING DISCUSSION, THE DESIRED DECIMAL SIGNIFICANCE
!   IS DENOTED BY  NSIG = IFIX(ALOG10(2)*NBIT+1), WHERE NBIT IS
!   THE NUMBER OF BITS IN THE MANTISSA OF A WORKING PRECISION
!   VARIABLE.  (LOWER VALUES OF NSIG RESULT IN DECREASED ACCURACY
!   WHILE HIGHER VALUES INCREASE CPU TIME WITHOUT INCREASING
!   ACCURACY.) RELATIVE TRUNCATION ERROR IS THEN LIMITED TO
!   T = 0.5*10**(-NSIG).  THE FOLLOWING CONSTANTS ARE USED IN
!   THIS PROGRAM:

! ENTEN  - 10.0 ** K, WHERE K IS THE LARGEST INTEGER(i4) SUCH THAT
!          ENTEN IS MACHINE-REPRESENTABLE IN WORKING PRECISION.
! ENSIG  - 10.0 ** NSIG.
! RTNSIG - 10.0 ** (-K) FOR THE SMALLEST INTEGER(i4) K SUCH THAT
!          K .GE. NSIG/4.
! ENMTEN - THE SMALLEST ABS(X) SUCH THAT X/4 DOES NOT UNDERFLOW.
! XLARGE - UPPER LIMIT ON THE MAGNITUDE OF X.  BEAR IN MIND
!          THAT IF ABS(X)=N, THEN AT LEAST N ITERATIONS OF THE
!          BACKWARD RECURSION WILL BE EXECUTED.

!     APPROXIMATE VALUES FOR SOME IMPORTANT MACHINES ARE:

!         IBM/195    CDC/7600  UNIVAC/1108   VAX 11/780 (UNIX)
!          (D.P.)  (S.P.,RNDG)    (D.P.)     (S.P.)     (D.P.)

! NSIG      16         14          18           8        17
! ENTEN   1.0D75     1.0E322     1.0D307     1.0E38    1.0D38
! ENSIG   1.0D16     1.0E14      1.0D18      1.0E8     1.0D17
! RTNSIG  1.0D-4     1.0E-4      1.0D-5      1.0E-2    1.0D-4
! ENMTEN  2.2D-78    1.0E-290    1.2D-308    1.2E-37   1.2D-37
! XLARGE  1.0D4      1.0E4       1.0D4       1.0E4     1.0D4


! ERROR RETURNS

!  IN CASE OF AN ERROR,  NCALC .NE. NB,  AND NOT ALL J'S ARE
!  CALCULATED TO THE DESIRED ACCURACY.

!  NCALC < 0:  AN ARGUMENT IS OUT OF RANGE. FOR EXAMPLE,
!     NB .LE. 0, ALPHA < 0 OR > 1, OR X IS TOO LARGE.
!     IN THIS CASE, THE B-VECTOR IS NOT CALCULATED, AND NCALC IS
!     SET TO  MIN0(NB,0)-1  SO THAT NCALC .NE. NB.

!  NB > NCALC > 0: NOT ALL REQUESTED FUNCTION VALUES COULD
!     BE CALCULATED ACCURATELY.  THIS USUALLY OCCURS BECAUSE NB IS
!     MUCH LARGER THAN ABS(X).  IN THIS CASE, B(N) IS CALCULATED
!     TO THE DESIRED ACCURACY FOR  N .LE. NCALC,  BUT PRECISION
!     IS LOST FOR NCALC < N .LE. NB.  IF B(N) DOES NOT VANISH
!     FOR  N > NCALC  (BECAUSE IT IS TOO SMALL TO BE REPRESENTED),
!     AND  B(N)/B(NCALC) = 10**(-K), THEN ONLY THE FIRST NSIG-K
!     SIGNIFICANT FIGURES OF B(N) CAN BE TRUSTED.

! OTHER SUBPROGRAMS REQUIRED (SINGLE PRECISION VERSION)

!     ABS,AINT,AMAX1,COS,GAMMA,SIN,SQRT,FLOAT,IFIX,MIN0

! OTHER SUBPROGRAMS REQUIRED (DOUBLE PRECISION VERSION)

!     DABS,DBLE,DCOS,DINT,DGAMMA,DMAX1,DSIN,DSQRT,FLOAT,
!          IFIX,MIN0,SNGL


! ACKNOWLEDGEMENT

!  THIS PROGRAM IS BASED ON A PROGRAM WRITTEN BY DAVID J. SOOKNE
!  THAT COMPUTES VALUES OF THE BESSEL FUNCTIONS J OR I OF REAL
!  ARGUMENT AND INTEGER(i4) ORDER.  MODIFICATIONS INCLUDE THE
!  RESTRICTION OF THE COMPUTATION TO THE J BESSEL FUNCTION OF
!  NON-NEGATIVE REAL ARGUMENT, THE EXTENSION OF THE COMPUTATION TO
!  ARBITRARY POSITIVE ORDER, AND THE ELIMINATION OF MOST UNDERFLOW.


!      MODIFIED BY: W. J. CODY
!                   ARGONNE NATIONAL LABORATORY

!      LATEST MODIFICATION: AUGUST 4, 1982

!-----------------------------------------------------------

!IMPLICIT NONE
use mod_kinds, only : i4, dp

REAL (dp), INTENT(IN)   :: x
REAL (dp), INTENT(IN)   :: alpha
INTEGER(i4), INTENT(IN)     :: nb
REAL (dp), INTENT(OUT)  :: b(nb)
INTEGER(i4), INTENT(OUT)    :: ncalc

INTEGER(i4)    :: i, j, k, l, m, magx, n, nbmx, nend, nstart
REAL (dp)  :: alpem, alp2em, capp, capq, dgamma,  &
    eighth, em, en, enmten, ensig, enten, fact, four, gnu, half,  &
    halfx, one, p, pi2, plast, pold, psave, psavel, rtnsig, s,  &
    sum, t, t1, tempa, tempb, tempc, test, three, tover, two,  &
    twofiv, twopi1, twopi2, xc, xin, xk, xlarge, xm, vcos, vsin, z, zero
DIMENSION  fact(25)
!-----------------------------------------------------------
!  MATHEMATICAL CONSTANTS

!   PI2    - 2 / PI
!   TWOPI1 - FIRST FEW SIGNIFICANT DIGITS OF 2 * PI
!   TWOPI2 - (2*PI - TWOPI) TO WORKING PRECISION, I.E.,
!            TWOPI1 + TWOPI2 = 2 * PI TO EXTRA PRECISION
!-----------------------------------------------------------
DATA pi2, twopi1, twopi2 /0.636619772367581343075535_dp, 6.28125_dp,  &
    1.935307179586476925286767D-3/
DATA zero, eighth, half, one /0.0_dp, 0.125_dp, 0.5_dp, 1.0_dp/
DATA two, three, four, twofiv /2.0_dp, 3.0_dp, 4.0_dp, 25.0_dp/
!-----------------------------------------------------------
!  MACHINE DEPENDENT PARAMETERS
!-----------------------------------------------------------
DATA enten, ensig, rtnsig /1.0D38, 1.0D17, 1.0D-4/
DATA enmten, xlarge /1.2D-37, 1.0D4/
!---------------------------------------------------------------------
!     FACTORIAL(N)
!---------------------------------------------------------------------
DATA fact /1.0D0, 1.0D0, 2.0D0, 6.0D0, 24.0D0, 1.2D2, 7.2D2,  &
    5.04D3, 4.032D4, 3.6288D5, 3.6288D6, 3.99168D7, 4.790016D8,  &
    6.2270208D9, 8.71782912D10, 1.307674368D12, 2.0922789888D13,  &
    3.55687428096D14, 6.402373705728D15, 1.21645100408832D17,  &
    2.43290200817664D18, 5.109094217170944D19,  &
    1.12400072777760768D21, 2.585201673888497664D22, 6.2044840173323943936D23/
!---------------------------------------------------------------------
magx = x
IF (.NOT.(nb > 0 .AND. x >= zero .AND. x <= xlarge .AND. alpha >= zero  &
          .AND. alpha < one)) THEN
!-----------------------------------------------------------
! ERROR RETURN -- X,NB,OR ALPHA IS OUT OF RANGE
!-----------------------------------------------------------
  ncalc = MIN(nb,0) - 1
ELSE
!-----------------------------------------------------------
! INITIALIZE RESULT ARRAY TO ZERO
!-----------------------------------------------------------
  ncalc = nb
  DO  i = 1, nb
    b(i) = zero
  END DO
!-----------------------------------------------------------
! BRANCH TO USE 2-TERM ASCENDING SERIES FOR SMALL X,
! AND ASYMPTOTIC FORM FOR LARGE X WHEN NB IS NOT TOO LARGE
!-----------------------------------------------------------
  IF (x >= rtnsig) THEN
    IF ((x > twofiv).AND.(nb <= magx+1)) GO TO 170
!-----------------------------------------------------------
! USE RECURRENCE TO GENERATE RESULTS.
! FIRST INITIALIZE THE CALCULATION OF P*S
!-----------------------------------------------------------
    nbmx = nb - magx
    n = magx + 1
    en = 2*(n + alpha)
    plast = one
    p = en / x
!-----------------------------------------------------------
! CALCULATE GENERAL SIGNIFICANCE TEST
!-----------------------------------------------------------
    test = ensig + ensig
    IF (nbmx >= 3) THEN
!-----------------------------------------------------------
! CALCULATE P*S UNTIL N = NB-1.  CHECK FOR POSSIBLE OVERFLOW.
!-----------------------------------------------------------
      tover = enten / ensig
      nstart = magx + 2
      nend = nb - 1
      en = nstart + nstart - two + (alpha + alpha)
      DO  n = nstart, nend
        en = en + two
        pold = plast
        plast = p
        p = en * plast / x - pold
        IF (p > tover) GO TO 40
      END DO
      n = nend
      en = 2*(n + alpha)
!-----------------------------------------------------------
! CALCULATE SPECIAL SIGNIFICANCE TEST FOR NBMX.GT.2.
!-----------------------------------------------------------
      test = MAX(test,DSQRT(plast*ensig)*DSQRT(p+p))
    END IF
!-----------------------------------------------------------
! CALCULATE P*S UNTIL SIGNIFICANCE TEST PASSES
!-----------------------------------------------------------
    30 n = n + 1
    en = en + two
    pold = plast
    plast = p
    p = en * plast / x - pold
    IF (p < test) GO TO 30
    GO TO 80
!-----------------------------------------------------------
! TO AVOID OVERFLOW, DIVIDE P*S BY TOVER.  CALCULATE P*S
! UNTIL ABS(P) > 1.
!-----------------------------------------------------------
    40     tover = enten
    p = p / tover
    plast = plast / tover
    psave = p
    psavel = plast
    nstart = n + 1
    50     n = n + 1
    en = en + two
    pold = plast
    plast = p
    p = en * plast / x - pold
    IF (p <= one) GO TO 50
    tempb = en / x
!-----------------------------------------------------------
! CALCULATE BACKWARD TEST, AND FIND NCALC, THE HIGHEST N
! SUCH THAT THE TEST IS PASSED.
!-----------------------------------------------------------
    test = pold * plast * (half-half/(tempb*tempb)) / ensig
    p = plast * tover
    n = n - 1
    en = en - two
    nend = MIN0(nb,n)
    DO  l = nstart, nend
      ncalc = l
      pold = psavel
      psavel = psave
      psave = en * psavel / x - pold
      IF (psave*psavel > test) GO TO 70
    END DO
    ncalc = nend + 1
    70 ncalc = ncalc - 1
!-----------------------------------------------------------
! INITIALIZE THE BACKWARD RECURSION AND THE NORMALIZATION SUM
!-----------------------------------------------------------
    80 n = n + 1
    en = en + two
    tempb = zero
    tempa = one / p
    m = 2 * n - 4 * (n/2)
    sum = zero
    em = n/2
    alpem = (em-one) + alpha
    alp2em = (em+em) + alpha
    IF (m /= 0) sum = tempa * alpem * alp2em / em
    nend = n - nb
    IF (nend > 0) THEN
!     IF (NEND) 140, 120, 100
!-----------------------------------------------------------
! RECUR BACKWARD VIA DIFFERENCE EQUATION, CALCULATING (BUT
! NOT STORING) B(N), UNTIL N = NB.
!-----------------------------------------------------------
      DO  l = 1, nend
        n = n - 1
        en = en - two
        tempc = tempb
        tempb = tempa
        tempa = (en*tempb) / x - tempc
        m = 2 - m
        IF (m /= 0) THEN
          em = em - one
          alp2em = (em+em) + alpha
          IF (n == 1) GO TO 100
          alpem = (em-one) + alpha
          IF (alpem == zero) alpem = one
          sum = (sum+tempa*alp2em) * alpem / em
        END IF
      END DO
    END IF
!-----------------------------------------------------------
! STORE B(NB)
!-----------------------------------------------------------
    100     b(n) = tempa
    IF (nend < 0) THEN
      nend = -nend
    ELSE
      
      IF (nb <= 1) THEN
        alp2em = alpha
        IF ((alpha+one) == one) alp2em = one
        sum = sum + b(1) * alp2em
        GO TO 130
      END IF
!-----------------------------------------------------------
! CALCULATE AND STORE B(NB-1)
!-----------------------------------------------------------
      n = n - 1
      en = en - two
      b(n) = (en*tempa) / x - tempb
      IF (n == 1) GO TO 120
      m = 2 - m
      IF (m /= 0) THEN
        em = em - one
        alp2em = (em+em) + alpha
        alpem = (em-one) + alpha
        IF (alpem == zero) alpem = one
        sum = (sum+b(n)*alp2em) * alpem / em
      END IF
    END IF
!     GO TO 160
!-----------------------------------------------------------
! N.LT.NB, SO STORE B(N) AND SET HIGHER ORDERS TO ZERO
!-----------------------------------------------------------
! 140 B(N) = TEMPA
!     NEND = -NEND
!     DO 150 L=1,NEND
!       B(N+L) = ZERO
! 150 CONTINUE
    nend = n - 2
    IF (nend /= 0) THEN
!-----------------------------------------------------------
! CALCULATE VIA DIFFERENCE EQUATION AND STORE B(N),
! UNTIL N = 2
!-----------------------------------------------------------
      DO  l = 1, nend
        n = n - 1
        en = en - two
        b(n) = (en*b(n+1)) / x - b(n+2)
        m = 2 - m
        IF (m /= 0) THEN
          em = em - one
          alp2em = (em+em) + alpha
          alpem = (em-one) + alpha
          IF (alpem == zero) alpem = one
          sum = (sum + b(n)*alp2em) * alpem / em
        END IF
      END DO
    END IF
!-----------------------------------------------------------
! CALCULATE B(1)
!-----------------------------------------------------------
    b(1) = two * (alpha+one) * b(2) / x - b(3)
    120 em = em - one
    alp2em = (em+em) + alpha
    IF (alp2em == zero) alp2em = one
    sum = sum + b(1) * alp2em
!-----------------------------------------------------------
! NORMALIZE.  DIVIDE ALL B(N) BY SUM.
!-----------------------------------------------------------
    130 IF ((alpha+one) /= one) sum = sum * dgamma(alpha) * (x*half) ** (-alpha)
    tempa = enmten
    IF (sum > one) tempa = tempa * sum
    DO  n = 1, nb
      IF (ABS(b(n)) < tempa) b(n) = zero
      b(n) = b(n) / sum
    END DO
    GO TO 210
  END IF
!-----------------------------------------------------------
! TWO-TERM ASCENDING SERIES FOR SMALL X
!-----------------------------------------------------------
  tempa = one
  alpem = one + alpha
  halfx = zero
  IF (x > enmten) halfx = half * x
  IF (alpha /= zero) tempa = halfx ** alpha / (alpha* dgamma(alpha))
  tempb = zero
  IF ((x+one) > one) tempb = -halfx * halfx
  b(1) = tempa + tempa * tempb / alpem
  IF (x /= zero .AND. b(1) == zero) ncalc = 0
  IF (nb == 1) GO TO 210
  IF (x <= zero) THEN
    DO  n = 2, nb
      b(n) = zero
    END DO
    GO TO 210
  END IF
!-----------------------------------------------------------
! CALCULATE HIGHER ORDER FUNCTIONS
!-----------------------------------------------------------
  tempc = halfx
  tover = (enmten+enmten) / x
  IF (tempb /= zero) tover = enmten / tempb
  DO  n = 2, nb
    tempa = tempa / alpem
    alpem = alpem + one
    tempa = tempa * tempc
    IF (tempa <= tover*alpem) tempa = zero
    b(n) = tempa + tempa * tempb / alpem
    IF ((b(n) == zero).AND.(ncalc > n)) ncalc = n - 1
  END DO
  GO TO 210
!-----------------------------------------------------------
! ASYMPTOTIC SERIES FOR X > 21.0
!-----------------------------------------------------------
  170   xc = SQRT(pi2/x)
  xin = (eighth/x) ** 2
  m = 11
  IF (x >= 35.0D0) m = 8
  IF (x >= 130.0D0) m = 4
  xm = four * m
!-----------------------------------------------------------
! REDUCTION OF ARGUMENT FOR SIN AND COS ROUTINES
!-----------------------------------------------------------
  t = INT(x/(twopi1+twopi2)+half)
  z = ((x-t*twopi1) - t*twopi2) - (alpha+half) / pi2
  vsin = SIN(z)
  vcos = COS(z)
  gnu = alpha + alpha
  DO  i = 1, 2
    s = ((xm-one) - gnu) * ((xm-one) + gnu) * xin * half
    t = (gnu - (xm-three)) * (gnu + (xm-three))
    capp = s * t / fact(2*m+1)
    t1 = (gnu - (xm+one)) * (gnu + (xm+one))
    capq = s * t1 / fact(2*m+2)
    xk = xm
    k = m + m
    t1 = t
    DO  j = 2, m
      xk = xk - four
      s = ((xk-one) - gnu) * ((xk-one) + gnu)
      t = (gnu - (xk-three)) * (gnu + (xk-three))
      capp = (capp + one/fact(k-1)) * s * t * xin
      capq = (capq + one/fact(k)) * s * t1 * xin
      k = k - 2
      t1 = t
    END DO
    capp = capp + one
    capq = (capq+one) * (gnu*gnu - one) * (eighth/x)
    b(i) = xc * (capp*vcos - capq*vsin)
    IF (nb == 1) GO TO 210
    t = vsin
    vsin = -vcos
    vcos = t
    gnu = gnu + two
  END DO
!-----------------------------------------------------------
! IF  NB > 2, COMPUTE J(X,ORDER+I)  I = 2, NB-1
!-----------------------------------------------------------
  IF (nb > 2) THEN
    gnu = alpha + alpha + two
    
    DO  j = 3, nb
      b(j) = gnu * b(j-1) / x - b(j-2)
      gnu = gnu + two
    END DO
  END IF
END IF
!-----------------------------------------------------------
! EXIT
!-----------------------------------------------------------
210 RETURN
! ---------- LAST CARD OF RJBESL ----------
END SUBROUTINE rjbesl



FUNCTION dgamma(x) RESULT(fn_val)
!----------------------------------------------------------------------

! THIS ROUTINE CALCULATES THE GAMMA FUNCTION FOR A REAL ARGUMENT X.
!     COMPUTATION IS BASED ON AN ALGORITHM OUTLINED IN W. J. CODY,
!     'AN OVERVIEW OF SOFTWARE DEVELOPMENT FOR SPECIAL FUNCTIONS',
!     LECTURE NOTES IN MATHEMATICS, 506, NUMERICAL ANALYSIS DUNDEE,
!     1975, G. A. WATSON (ED.), SPRINGER VERLAG, BERLIN, 1976.  THE
!     PROGRAM USES RATIONAL FUNCTIONS THAT APPROXIMATE THE GAMMA
!     FUNCTION TO AT LEAST 20 SIGNIFICANT DECIMAL DIGITS.  COEFFICIENTS
!     FOR THE APPROXIMATION OVER THE INTERVAL (1,2) ARE UNPUBLISHED.
!     THOSE FOR THE APPROXIMATION FOR X .GE. 12 ARE FROM HART, ET. AL.,
!     COMPUTER APPROXIMATIONS, WILEY AND SONS, NEW YORK, 1968.  LOWER
!     ORDER APPROXIMATIONS CAN BE SUBSTITUTED FOR THESE ON MACHINES
!     WITH LESS PRECISE ARITHMETIC.


!*******************************************************************
!*******************************************************************

! EXPLANATION OF MACHINE-DEPENDENT CONSTANTS

! EPS    - THE SMALLEST POSITIVE FLOATING-POINT NUMBER SUCH THAT
!          1.0 + EPS > 1.0
! XBIG   - THE LARGEST ARGUMENT FOR WHICH GAMMA(X) IS REPRESENTABLE
!          IN THE MACHINE, I.E., THE SOLUTION TO THE EQUATION
!                  GAMMA(XBIG) = XINF.
! XMININ - THE SMALLEST POSITIVE FLOATING-POINT NUMBER SUCH THAT
!          1/XMININ IS MACHINE REPRESENTABLE.
! XINF   - THE LARGEST MACHINE REPRESENTABLE FLOATING-POINT NUMBER.

!     APPROXIMATE VALUES FOR SOME IMPORTANT MACHINES ARE:

!         IBM/195    CDC/7600  UNIVAC/1108   VAX 11/780 (UNIX)
!          (D.P.)  (S.P.,RNDG)    (D.P.)     (S.P.)     (D.P.)

! EPS     2.221D-16  3.553E-15   1.735D-18   5.961E-08  1.388D-17
! XBIG    57.574     177.802     171.489     34.844     34.844
! XMININ  1.382D-76  3.132E-294  1.113D-308  5.883E-39  5.883D-39
! XINF    7.23D+75   1.26E+322   8.98D+307   1.70E+38   1.70D+38

!*******************************************************************
!*******************************************************************


! ERROR RETURNS

!  THE PROGRAM RETURNS THE VALUE XINF FOR SINGULARITIES OR
!     WHEN OVERFLOW WOULD OCCUR.  THE COMPUTATION IS BELIEVED
!     TO BE FREE OF UNDERFLOW AND OVERFLOW.

! OTHER SUBPROGRAMS REQUIRED (SINGLE PRECISION VERSION)

!     ALOG,EXP,FLOAT,IFIX,SIN

! OTHER SUBPROGRAMS REQUIRED (DOUBLE PRECISION VERSION)

!     DBLE,DEXP,DLOG,DSIN,FLOAT,IFIX,SNGL

!  AUTHOR: W. J. CODY
!          APPLIED MATHEMATICS DIVISION
!          ARGONNE NATIONAL LABORATORY
!          ARGONNE, IL 60439

!  LATEST MODIFICATION: MAY 18, 1982

!----------------------------------------------------------------------

!IMPLICIT NONE
use mod_kinds, only : i4, dp

REAL (dp), INTENT(IN)  :: x
REAL (dp)              :: fn_val

REAL (dp)  :: fact, res, sum, xden, xnum, y, y1, ysq, z
INTEGER(i4)    :: i, j, n
LOGICAL    :: parity
!----------------------------------------------------------------------
!  MATHEMATICAL CONSTANTS
!----------------------------------------------------------------------
REAL (dp), PARAMETER  :: one = 1.0_dp, half = 0.5_dp, twelve = 12.0_dp,   &
                         zero = 0.0_dp
REAL (dp), PARAMETER  :: sqrtpi = 0.9189385332046727417803297_dp
REAL (dp), PARAMETER  :: pi = 3.1415926535897932384626434_dp
!----------------------------------------------------------------------
!  MACHINE DEPENDENT PARAMETERS
!----------------------------------------------------------------------
REAL (dp), PARAMETER  :: xbig = 34.844D0, xminin = 5.883D-39, eps = EPSILON(one)
REAL (dp), PARAMETER  :: xinf = 1.7014D38
!----------------------------------------------------------------------
!  NUMERATOR AND DENOMINATOR COEFFICIENTS FOR RATIONAL MINIMAX
!     APPROXIMATION OVER (1,2).
!----------------------------------------------------------------------
REAL (dp), PARAMETER  :: p(8) = (/ -1.71618513886549492533811D+0,  &
     2.47656508055759199108314D+1, -3.79804256470945635097577D+2,  &
     6.29331155312818442661052D+2,  8.66966202790413211295064D+2,  &
    -3.14512729688483675254357D+4, -3.61444134186911729807069D+4,  &
     6.64561438202405440627855D+4 /)
REAL (dp), PARAMETER  :: q(8) = (/ -3.08402300119738975254353D+1,  &
     3.15350626979604161529144D+2, -1.01515636749021914166146D+3,  &
    -3.10777167157231109440444D+3,  2.25381184209801510330112D+4,  &
     4.75584627752788110767815D+3, -1.34659959864969306392456D+5,  &
    -1.15132259675553483497211D+5 /)
!----------------------------------------------------------------------
!  COEFFICIENTS FOR MINIMAX APPROXIMATION OVER (12, INF).
!----------------------------------------------------------------------
REAL (dp), PARAMETER  :: c(7) = (/ -1.910444077728D-03, 8.4171387781295D-04,  &
    -5.952379913043012D-04, 7.93650793500350248D-04, -2.777777777777681622553D-03, &
     8.333333333333333331554247D-02, 5.7083835261D-03 /)
!----------------------------------------------------------------------
parity = .false.
fact = one
n = 0
y = x
IF (y <= zero) THEN
!----------------------------------------------------------------------
!  ARGUMENT IS NEGATIVE
!----------------------------------------------------------------------
  y = -x
  j = y
  res = y - j
  IF (res == zero) GO TO 40
  IF (j /= (j/2)*2) parity = .true.
  fact = -pi / SIN(pi*res)
  y = y + one
END IF
!----------------------------------------------------------------------
!  ARGUMENT IS POSITIVE
!----------------------------------------------------------------------
IF (y >= eps) THEN
  IF (y < twelve) THEN
    y1 = y
    IF (y < one) THEN
!----------------------------------------------------------------------
!  0.0 < ARGUMENT < 1.0
!----------------------------------------------------------------------
      z = y
      y = y + one
    ELSE
!----------------------------------------------------------------------
!  1.0 < ARGUMENT < 12.0, REDUCE ARGUMENT IF NECESSARY
!----------------------------------------------------------------------
      n = y - 1
      y = y - n
      z = y - one
    END IF
!----------------------------------------------------------------------
!  EVALUATE APPROXIMATION FOR 1.0 < ARGUMENT < 2.0
!----------------------------------------------------------------------
    xnum = zero
    xden = one
    DO  i = 1, 8
      xnum = (xnum+p(i)) * z
      xden = xden * z + q(i)
    END DO
    res = xnum / xden + one
    IF (y == y1) GO TO 50
    IF (y1 <= y) THEN
!----------------------------------------------------------------------
!  ADJUST RESULT FOR CASE  0.0 < ARGUMENT < 1.0
!----------------------------------------------------------------------
      res = res / y1
      GO TO 50
    END IF
!----------------------------------------------------------------------
!  ADJUST RESULT FOR CASE  2.0 < ARGUMENT < 12.0
!----------------------------------------------------------------------
    DO  i = 1, n
      res = res * y
      y = y + one
    END DO
    GO TO 50
  END IF
!----------------------------------------------------------------------
!  EVALUATE FOR ARGUMENT >= 12.0,
!----------------------------------------------------------------------
  IF (y > xbig) GO TO 40
  ysq = y * y
  sum = c(7)
  DO  i = 1, 6
    sum = sum / ysq + c(i)
  END DO
  sum = sum / y - y + sqrtpi
  sum = sum + (y-half) * LOG(y)
  res = EXP(sum)
  GO TO 50
END IF
!----------------------------------------------------------------------
!  ARGUMENT < EPS
!----------------------------------------------------------------------
IF (y >= xminin) THEN
  res = one / y
  GO TO 50
END IF
!----------------------------------------------------------------------
!  RETURN FOR SINGULARITIES, EXTREME ARGUMENTS, ETC.
!----------------------------------------------------------------------
40 fn_val = xinf
GO TO 60
!----------------------------------------------------------------------
!  FINAL ADJUSTMENTS AND RETURN
!----------------------------------------------------------------------
50 IF (parity) res = -res
IF (fact /= one) res = fact / res
fn_val = res
60 RETURN
! ---------- LAST CARD OF GAMMA ----------
END FUNCTION dgamma
