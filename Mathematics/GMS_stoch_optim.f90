MODULE common_dincom
use mod_kinds, only : i4,dp
IMPLICIT NONE

! COMMON /dincom/ x(100,20), h(20), dx(20), vmvt(20,19), eps(20),  &
!     vmcor(20), vcor(20), xrmin(100), xrmax(100), xopt(100), fopt,  &
!     ie(20), isvt(20,19), kgen, ktim, ndim, ntraj, ntrajr, isegbr,  &
!     inkpbr, kpbr0, ncf, ifep, inhp
REAL (dp), SAVE  :: x(100,20), h(20), dx(20), vmvt(20,19), eps(20),  &
                    vmcor(20), vcor(20), xrmin(100), xrmax(100), xopt(100), &
                    fopt
INTEGER(kind=i4), SAVE    :: ie(20), isvt(20,19), kgen, ktim, ndim, ntraj, ntrajr,  &
                    isegbr, inkpbr, kpbr0, ncf, ifep, inhp

END MODULE common_dincom



MODULE stoch_optim

!      ALGORITHM 667, COLLECTED ALGORITHMS FROM ACM.

!      THIS WORK PUBLISHED IN TRANSACTIONS ON MATHEMATICAL SOFTWARE,
!      VOL. 14, NO. 4, PP. 366-388.

! Code converted using TO_F90 by Alan Miller
! Date: 2001-06-17  Time: 00:43:18

! Latest revision - 20 June 2001
! Alan Miller: amiller @ bigpond.net.au
! http://users.bigpond.net.au/amiller/
use mod_kinds, only : i4,dp
IMPLICIT NONE


! COMMON /scale/ dist(10,10,20), bias(10,20), gragra(10,10,20),  &
!                gra(10,20), ngra(20), lsca, idsca, nx, nord
REAL (dp), SAVE  :: dist(10,10,20), bias(10,20), gragra(10,10,20), gra(10,20)
INTEGER(kind=i4), SAVE    :: ngra(20), lsca, idsca, nx, nord

! Interfaces to user-supplied functions & subroutines
INTERFACE
  FUNCTION FUNCT(n, x) RESULT(fn_val)
    IMPORT :: i4,dp
    IMPLICIT NONE
    INTEGER(kind=i4), INTENT(IN)    :: n
    REAL (dp), INTENT(IN)  :: x(:)
    REAL (dp)              :: fn_val
  END FUNCTION FUNCT

  SUBROUTINE PTSEG(N, XPFMIN, FPFMIN, FPFMAX, KP, NFEV)   
    IMPORT :: i4,dp
    IMPLICIT NONE
    INTEGER(kind=i4), INTENT(IN)    :: n, kp, nfev
    REAL (dp), INTENT(IN)  :: xpfmin(:), fpfmin, fpfmax
  END SUBROUTINE PTSEG

  SUBROUTINE PTRIAL(N, XOPT, FOPT, FTFMIN, FTFMAX, FTFOPT,  &
                    ISTOP, ISTOPT, NFEV, KP, IPRINT)
    IMPORT :: i4,dp
    IMPLICIT NONE
    INTEGER(kind=i4), INTENT(IN)    :: n, kp, nfev, istop, istopt, iprint
    REAL (dp), INTENT(IN)  :: xopt(:), fopt, ftfmin, ftfmax, ftfopt
  END SUBROUTINE PTRIAL

  SUBROUTINE ptksuc(ksuc)
    IMPLICIT NONE
    INTEGER(kind=i4), INTENT(IN)  :: ksuc
  END SUBROUTINE ptksuc
END INTERFACE


CONTAINS


SUBROUTINE sigma1(n,x0,nsuc,iprint,xmin,fmin,nfev,iout)
      !dir$ attribute code_align : 32 :: sigma1
      !dir$ optimize : 3
      !dir$ attribute optimization_parameter: target_arch=AVX :: sigma1

!  SIGMA1  IS A 'DRIVER' SUBROUTINE WHICH SIMPLY CALLS THE PRINCIPAL SUBROUTINE
!  SIGMA  AFTER HAVING ASSIGNED DEFAULT VALUES TO A NUMBER OF INPUT PARAMETERS
!  OF  SIGMA , AND HAS THEREFORE A CONSIDERABLY LOWER NUMBER OF INPUT
!  PARAMETERS.
!  IT CAN BE USED AS A SIMPLE EXAMPLE OF HOW TO CALL  SIGMA , BUT ALSO AS AN
!  EASY-TO-USE DRIVER FOR THE AVERAGE USER, WHICH MAY FIND IT EASIER TO CALL
!  SIGMA1  INSTEAD OF  SIGMA , THUS AVOIDING THE TROUBLE OF ASSIGNING A VALUE
!  TO ALL THE INPUT PARAMETERS OS  SIGMA .  ALL THE PARAMETERS IN THE
!  DEFINITION OF  SIGMA1  HAVE THE SAME MEANING AS IN  SIGMA .

!  THE USER OF  SIGMA1  MUST ONLY GIVE VALUES TO THE INPUT PARAMETERS
!      N, X0, NSUC, IPRINT
!  AND OBTAINS ON OUTPUT THE SAME OUTPUT PARAMETERS OF  SIGMA
!      XMIN, FMIN, NFEV, IOUT

!  WE RECALL HERE THE MEANING OF THE ABOVE PARAMETERS

!  N       IS THE PROBLEM DIMENSION (NUMBER OF VARIABLES)
!  X0      IS AN  N-VECTOR CONTAINING THE INITIAL VALUES OF THE X-VARIABLES
!  NSUC    IS THE REQUESTED NUMBER OF SUCCESSFUL TRIALS AFTER WHICH THE
!          COMPUTATION IS STOPPED.
!  IPRINT  IS AN INDEX USED TO CONTROL THE AMOUNT OF PRINTED OUTPUT BY
!          BY CONTROLLING THE CALLS TO THE USER-SUPPLIED OUTPUT SUBROUTINES
!          PTSEG  (END-OF-SEGMENT OUTPUT),  PTRIAL  (END-OF-TRIAL OUTPUT),
!          AND  PTKSUC (END-OF-TRIAL OUTPUT RELATED TO THE COUNT OF SUCCESSFUL
!          TRIALS), WHICH ARE DESCRIBED BELOW.
!          IPRINT<0   NO CALL TO THE OUTPUT SUBROUTINES
!          IPRINT.EQ.0   CALL ONLY  PTRIAL  AND  PTKSUC
!          IPRINT>0   CALL ALL OUTPUT SUBROUTINES.
!  XMIN    IS AN N-VECTOR CONTAINING THE COORDINATES OF THE POINT (OR POSSIBLY
!          ONE OF THE POINTS) WHERE THE FINAL VALUE  FMIN  OF  FOPT  WAS FOUND.
!  FMIN    IS THE FINAL VALUE OF THE BEST CURRENT MINIMUM FUNCTION VALUE  FOPT.
!  NFEV    IS THE TOTAL NUMBER OF FUNCTION EVALUATION (INCLUDING
!          THOSE USED FOR THE COMPUTATION OF DERIVATIVES, AND FOR
!          THE REJECTED TIME-INTEGRATION STEPS).
!  IOUT    IS THE INDICATOR OF THE STOPPING CONDITIONS, AS FOLLOWS
!          IF  IOUT = -99  A FATAL ERROR WAS DETECTED WHEN PERFORMING SOME
!             PRELIMINARY CHECKING OF THE INPUT DATA, AND THE ALGORITHM WAS
!             NOT EVEN STARTED
!          OTHERWISE THE ALGORITHM WAS STARTED, AND THE MEANING OF
!          IOUT  IS AS FOLLOWS
!          IF  IOUT.EQ.0  NO TRIAL HAD A UNIFORM STOP.
!          IF  IOUT.NE.0  THE SIGN AND THE MAGNITUDE OF  IOUT  HAVE
!          THE FOLLOWING MEANING
!          A) THE SIGN OF  IOUT  INDICATES IF THE BEST UNIFORM STOP
!          (I.E. THE ONE IN WHICH  FTFOPT  WAS OBTAINED)
!          IS TO BE CONSIDERED SUCCESSFUL (IOUT>0) OR UNSUCCESSFUL
!          B) THE MAGNITUDE OF  IOUT  INDICATES WHICH ONE OF THE NUMERICAL
!          EQUALITY CRITERIA WAS SATISFIED IN THE BEST UNIFORM STOP
!          IABS(IOUT).EQ.1  RELATIVE DIFFERENCE CRITERION
!          IABS(IOUT).EQ.2  ABSOLUTE DIFFERENCE CRITERION
!          IABS(IOUT).EQ.3  BOTH CRITERIA
!          (IOUT  IS THE FINAL VALUE OF THE INTERNAL PARAMETER  ISTOPT
!          (AN OUTPUT INDICATOR OF THE USER-SUPPLIED SUBROUTINE PTRIAL )).
!          SUCCESS IS CLAIMED BY THE ALGORITHM IF  IOUT > 0,
!          I.E. IF AT LEAST ONE OF THE TRIALS STOPPED SUCCESSFULLY
!          (I.E. WITH A POSITIVE VALUE OF THE TRIAL STOP INDICATOR
!          ISTOP ), AND THE SUCCESSFUL TRIALS (AS COUNTED BY  KSUC )
!          ARE CURRENTLY VALID.

INTEGER(kind=i4), INTENT(IN)     :: n
REAL (dp), INTENT(IN)   :: x0(:)
INTEGER(kind=i4), INTENT(IN)     :: nsuc
INTEGER(kind=i4), INTENT(IN)     :: iprint
REAL (dp), INTENT(OUT)  :: xmin(:)
REAL (dp), INTENT(OUT)  :: fmin
INTEGER(kind=i4), INTENT(OUT)    :: nfev
INTEGER(kind=i4), INTENT(OUT)    :: iout

REAL (dp)  :: dx, eps, h, tolabs, tolrel
REAL (dp)  :: vrmax = 1.0E+4_dp, vrmin = -1.0E-4_dp

REAL (dp)  :: xrmin(104), xrmax(104)
!dir$ attribute align : 64 :: xrmin
!dir$ attribute align : 64 :: xrmax
INTEGER(kind=i4)    :: inhp, inkpbr, inpmax, irand, isegbr, ix, kpasca, kpbr0,  &
              npmin, npmax, ntraj, ntrial, ntrild = 50

h = 1.d-10
eps = 1.d0
dx = 1.d-9
irand = 0
ntraj = 0
isegbr = 0
inkpbr = 0
kpbr0 = 0
npmin = 10
npmax = 100
inpmax = 50
ntrial = MAX(ntrild, 5*nsuc)
tolrel = 1.d-3
tolabs = 1.d-6
kpasca = 10
IF (n > 5) kpasca = 300
inhp = 1
DO  ix = 1, n
  xrmin(ix) = vrmin
  xrmax(ix) = vrmax
END DO

CALL sigma(n,x0,h,eps,dx,ntraj,isegbr,kpbr0,inkpbr,npmin,npmax,  &
           inpmax,nsuc,ntrial,tolrel,tolabs,xrmin,xrmax,kpasca,irand,  &
           inhp,iprint,xmin,fmin,nfev,iout)

RETURN
END SUBROUTINE sigma1



SUBROUTINE sigma(n,x0,h,eps,dx,ntraj,isegbr,kpbr0,inkpbr,npmin,npmax0,  &
                 inpmax,nsuc,ntrial,tolrel,tolabs,xrmin,xrmax,kpasca,  &
                 irand,inhp,iprint,xmin,fmin,nfev,iout)
      !dir$ attribute code_align : 32 :: sigma1
      !dir$ optimize : 3
      !dir$ attribute optimization_parameter: target_arch=AVX :: sigma1



!  THE SUBROUTINE  SIGMA  IS THE PRINCIPAL SUBROUTINE OF THE PACKAGE
!  SIGMA, WHICH ATTEMPTS TO FIND A GLOBAL MINIMIZER OF A REAL VALUED
!  FUNCTION  F(X) = F(X1,...,XN)  OF  N  REAL VARIABLES  X1,...,XN.
!  THE ALGORITHM AND THE PACKAGE ARE DESCRIBED IN DETAIL IN THE TWO
!  PAPERS PUBLISHED IN THE SAME ISSUE OF THE A.C.M. TRANSACTIONS ON
!  MATHEMATICAL SOFTWARE, BOTH BY
!     F. ALUFFI-PENTINI, V. PARISI, F. ZIRILLI.
!  (1)  A GLOBAL MINIMIZATION ALGORITHM USING STOCHASTIC DIFFERENTIAL EQUATIONS
!  (2)  ALGORITHM SIGMA, A STOCHASTIC-INTEGRATION GLOBAL MINIMIZATION ALGORITHM.
!  THE SOFTWARE IMPLEMENTATION AND ITS USAGE ARE DESCRIBED IN (2).

!  METHOD

!  A GLOBAL MINIMIZER OF  F(X)  IS SOUGHT BY MONITORING THE VALUES OF  F
!  ALONG TRAJECTORIES GENERATED BY A SUITABLE (STOCHASTIC) DISCRETIZATION
!  OF A FIRST-ORDER STOCHASTIC DIFFERENTIAL EQUATION INSPIRED BY
!  STATISTICAL MECHANICS. STARTING FROM AN INITIAL POINT  X0 ,
!  X  IS UPDATED BY THE (STOCHASTIC) DISCRETIZATION STEP
!       X = X + DX1 + DX2
!  WHERE  DX1 = - H * GAM           (FIRST HALF-STEP)
!         DX2 = EPS * SQRT(H) * U   (SECOND HALF-STEP)
!  AND  H  IS THE TIME-INTEGRATION STEPLENGTH,
!  GAM/N  IS COMPUTED AS A FINITE-DIFFERENCE APPROXIMATION TO THE
!     DIRECTIONAL DERIVATIVE OF  F  ALONG AN ISOTROPICALLY RANDOM DIRECTION,
!  EPS  IS A POSITIVE 'NOISE' COEFFICIENT, AND
!  U  IS A RANDOM SAMPLE FROM AN N-DIMENSIONAL GAUSSIAN DISTRIBUTION.
!  WE CONSIDER THE SIMULTANEOUS EVOLUTION OF A GIVEN FIXED NUMBER
!  NTRAJ  OF TRAJECTORIES DURING AN OBSERVATION PERIOD IN WHICH FOR
!  EACH TRAJECTORY  EPS  IS FIXED WHILE  H  AND THE SPATIAL DISCRETI-
!  ZATION INCREMENT  DX  FOR COMPUTING  GAM  ARE AUTOMATICALLY
!  ADJUSTED BY THE ALGORITHM.
!  AFTER EVERY OBSERVATION PERIOD ONE OF THE TRAJECTORIES IS DISCARDED,
!  ALL OTHER TRAJECTORIES CONTINUE UNPERTURBED, AND ONE OF THEM IS SE-
!  LECTED FOR BRANCHING, I.E. GENERATING A SECOND PERTURBED CONTI-
!  NUATION, WITH DIFFERENT STARTING  EPS  AND  DX  (AND THE SAME
!  'PAST HISTORY' OF THE FIRST).
!  THE SET OF SIMULTANEOUS TRAJECTORIES IS CONSIDERED A SINGLE TRIAL,
!  AND THE COMPLETE ALGORITHM IS A SET OF REPEATED TRIALS.
!  A TRIAL IS STOPPED, AT THE END OF AN OBSERVATION PERIOD, AND AFTER
!  HAVING DISCARDED THE WORST TRAJECTORY, IF ALL THE FINAL VALUES OF
!  F  FOR THE REMAINING TRAJECTORIES ARE EQUAL (WITHIN NUMERICAL TOLERANCES,
!  AND POSSIBLY AT DIFFERENT POINTS  X) TO THEIR MINIMUM
!  VALUE  FTFMIN ('UNIFORM STOP AT THE LEVEL  FTFMIN ').
!  A UNIFORM STOP IS CONSIDERED SUCCESSFUL ONLY IF THE FINAL VALUE
!  FTFMIN  IS (NUMERICALLY) EQUAL TO THE CURRENT BEST MINIMUM  FOPT
!  FOUND SO FAR FROM ALGORITHM START.
!  A TRIAL IS ALSO ANYWAY STOPPED (UNSUCCESSFULLY) IF A GIVEN MAXIMUM
!  NUMBER  npmax0  OF OBSERVATION PERIODS HAS ELAPSED.
!  TRIALS ARE REPEATED WITH DIFFERENT OPERATING CONDITIONS (INITIAL
!  POINT, MAX TRIAL LENGTH  npmax0 , SEED OF NOISE GENERATOR, POLICY
!  FOR CHOOSING THE STARTING  EPS  FOR THE PERTURBED CONTINUATION,
!  AND TRIAL-START VALUE OF  EPS ).
!  THE OUTCOMES OF ALL THE COMPLETED TRIALS ARE SUMMARIZED BY
!  FTFOPT (THE BEST CURRENT MIMNIMUM VALUE FOUND SO FAR FOR
!  FTFMIN ) AND BY THE CURRENT COUNT  KSUC  OF SUCCESSFUL TRIALS
!  (WHICH IS ZERO AT ALGORITHM START, AND IS UPDATED AT EVERY
!  SUCCESSFULLY-STOPPING TRIALS).
!  THE ALGORITHM IS STOPPED, AT THE END OF A TRIAL, IF A REQUESTED
!  COUNT  NSUC  OF SUCCESSFUL TRIALS HAS BEEN REACHED,
!  OR ANYWAY IF A GIVEN MAXIMUM NUMBER  NTRIAL  OF TRIALS HAS BEEN REACHED.
!  SUCCESS IS CLAIMED IF THE CURRENT COUNT  KSUC  OF SUCCESSFUL TRIALS
!  IS AT LEAST ONE, AND IF SUCH TRIALS ARE CURRENTLY VALID.

!  CALL STATEMENT

!  THE CALL STATEMENT IS
!     CALL SIGMA ( N, X0, H, EPS, DX,
!                  NTRAJ, ISEGBR, KPBR0, INKPBR,
!                  NPMIN, NPMAX0, INPMAX,
!                  NSUC, NTRIAL, TOLREL, TOLABS, XRMIN, XRMAX,
!                  KPASCA, IRAND, INHP, IPRINT,
!                  XMIN, FMIN, NFEV, IOUT )

!  CALL PARAMETERS

!  INPUT PARAMETERS ARE THOSE IN LINES 1,3,4,5 OF THE CALL STATEMENT,
!  INPUT-OUTPUT PARAMETERS ARE THOSE IN LINE 2,
!  OUTPUT PARAMETERS ARE THOSE IN LINE 6.
!  NOTE THAT A NUMBER OF OTHER (INTERNAL) PARAMETERS CAN BE OBTAINED
!  BY MEANS OF THE USER-SUPPLIED OUTPUT SUBROUTINES  PTSEG, PTRIAL,
!  AND  PTKSUC, WHICH ARE DESCRIBED BELOW.

!  DESCRIPTION OF THE CALL PARAMETERS

!  N       IS THE PROBLEM DIMENSION (NUMBER OF VARIABLES)
!  X0      IS AN  N-VECTOR CONTAINING THE INITIAL VALUES OF THE X-VARIABLES
!  H       IS THE INITIAL VALUE OF THE TIME-INTEGRATION STEPLENGTH.
!  EPS     IS THE INITIAL VALUE OF THE NOISE COEFFICIENT.
!  DX      IS THE INITIAL VALUE OF THE MAGNITUDE OF THE DISCRETIZATION
!          INCREMENT FOR COMPUTING THE FINITE-DIFFERENCE DERIVATIVES.
!  NTRAJ   IS THE NUMBER OF SIMULTANEOUS TRAJECTORIES.
!          (NOTE HOWEVER THAT IF THE INPUT VALUE IS ZERO,  NTRAJ  IS
!          SET TO A DEFAULT VALUE (NTRAJ = 7), AND IF THE INPUT VALUE
!          IS OTHERWISE OUTSIDE THE INTERVAL (3,20) NTRAJ IS SET TO
!          THE NEAREST EXTREME VALUE).
!  ISEGBR, KPBR0, INKPBR  DETERMINE, AT THE END OF AN OBSERVATION
!          PERIOD, WHICH ONE OF THE SIMULTANEOUS TRAJECTORIES
!          IS TO BE BRANCHED, AS FOLLOWS.
!          BRANCHING IS NORMALLY PERFORMED ON THE TRAJECTORY WHICH
!          OCCUPIES THE PLACE  ISEGBR  IN THE TRAJECTORY SELECTION ORDERING,
!          EXCEPT AT (THE END OF) EXCEPTIONAL OBSERVATION
!          PERIODS, WHERE THE FIRST TRAJECTORY IN THE ORDERING IS
!          BRANCHED. EXCEPTIONAL BRANCHING OCCURS AT THE OBSERVATION
!          PERIODS NUMBERED  KP = KPBR0 + J*INKPBR, (J = 1,2,3,...).
!          THEREFORE  ISEGBR  SELECTS THE LEVEL (IN THE ORDERING) AT
!          WHICH NORMAL BRANCHING OCCURS, WHILE  KPBR0  AND  INKPBR
!          SELECT THE FIRST OCCURRENCE AND THE REPETITION FREQUENCY
!          OF THE EXCEPTIONAL OBSERVATION PERIODS.
!          (NOTE HOWEVER THAT IF ONE OF THE INPUT VALUES IS ZERO,
!          THE CORRESPONDING VARIABLE IS SET TO A DEFAULT VALUE
!          ISEGBR = INT((1+NTRAJ)/2), INKPBR = 10, KPBR0 = 3.
!          IF THE INPUT VALUE FOR  ISEGBR  IS OTHERWISE OUTSIDE THE
!          INTERVAL  (1,NTRAJ),  ISEGBR  IS SET TO THE NEAREST
!          EXTREME VALUE, AND IF KPBR0  HAS A VALUE NOT INSIDE THE
!          INTERVAL  (1,INKPBR), IT IS ASSIGNED THE SAME VALUE MODULO  INKPBR).
!  NPMIN   IS THE MINIMUM DURATION OF A TRIAL, I.E. THE MINIMUM
!          NUMBER OF OBSERVATION PERIODS THAT SHOULD ELAPSE BEFORE
!          STARTING TO CHECK THE TRIAL STOPPING CRITERIA.
!  NPMAX0  IS THE MAXIMUM DURATION OF THE FIRST TRIAL, I.E. THE
!          VALUE, FOR THE FIRST TRIAL, OF MAXIMUM ACCEPTABLE
!          NUMBER  npmax0  OF OBSERVATION PERIODS IN A TRIAL.
!  INPMAX  IS THE INCREMENT FOR  npmax0 , WHEN  npmax0  IS VARIED
!          FROM ONE TRIAL TO THE FOLLOWING ONE.
!  NSUC    IS THE REQUESTED NUMBER OF SUCCESSFUL TRIALS
!          AFTER WHICH THE COMPUTATION IS STOPPED.
!  TOLREL  AND  TOLABS  ARE THE RELATIVE AND ABSOLUTE TOLERANCES
!          FOR STOPPING A SINGLE TRIAL.
!  XRMIN,  XRMAX  ARE N-VECTORS DEFINING THE ADMISIBLE REGION FOR THE
!          X-VALUES, WITHIN WHICH THE FUNCTION VALUES CAN BE SAFELY COMPUTED.
!  KPASCA  IS THE MINIMUM NUMBER OF TRAJECTORY SEGMENTS THAT SHOULD
!          ELAPSE BEFORE THE RESCALING PROCEDURES ARE ACTIVATED.
!  IRAND   IS A CONTROL INDEX FOR THE INITIALIZATION OF THE RANDOM
!          NUMBER GENERATOR.
!          IRAND > 0    THE GENERATOR IS INITIALIZED, BEFORE STARTING
!                       THE TRIAL  KT, WITH SEED  IRAND+KT-1
!          IRAND <= 0   THE GENERATOR IS INITIALIZED (WITH SEED 0)
!                       ONLY AT THE FIRST CALL OF SIGMA
!  INHP    IS A CONTROL INDEX FOR SELECTING THE NUMBER  NHP  OF TIME-
!          INTEGRATION STEPS FOR OBSERVATION PERIOD  KP (DURATION OF
!          TRIAL  KP) AS FOLLOWS (LOG IS BASE 2)
!          INHP=1  NHP = 1 + INT(LOG(KP))      ('SHORT' DURATION)
!          INHP=2  NHP = INT(SQRT(KP))         ('MEDIUM' DURATION)
!          INHP=3  NHP = KP                    ('LONG' DURATION)
!  IPRINT  IS AN INDEX USED TO CONTROL THE AMOUNT OF PRINTED OUTPUT BY
!          CONTROLLING THE CALLS TO THE USER-SUPPLIED OUTPUT SUBROUTINES
!          PTSEG  (END-OF-SEGMENT OUTPUT),  PTRIAL  (END-OF-TRIAL OUTPUT),
!          AND  PTKSUC (END-OF-TRIAL OUTPUT RELATED TO THE COUNT OF SUCCESSFUL
!          TRIALS), WHICH ARE DESCRIBED BELOW.
!          IPRINT < 0   NO CALL TO THE OUTPUT SUBROUTINES
!          IPRINT.EQ.0   CALL ONLY  PTRIAL  AND  PTKSUC
!          IPRINT > 0   CALL ALL OUTPUT SUBROUTINES.
!  XMIN    IS AN N-VECTOR CONTAINING THE COORDINATES OF THE POINT
!          (OR POSSIBLY ONE OF THE POINTS) WHERE THE FINAL VALUE  FMIN
!          OF  FOPT  WAS FOUND.
!  FMIN    IS THE FINAL VALUE OF THE BEST CURRENT MINIMUM FUNCTION VALUE  FOPT.
!  NFEV    IS THE TOTAL NUMBER OF FUNCTION EVALUATION (INCLUDING
!          THOSE USED FOR THE COMPUTATION OF DERIVATIVES, AND FOR
!          THE REJECTED TIME-INTEGRATION STEPS).
!  IOUT    IS THE INDICATOR OF THE STOPPING CONDITIONS, AS FOLLOWS
!          IF  IOUT = -99  A FATAL ERROR WAS DETECTED WHEN PERFORMING SOME
!             PRELIMINARY CHECKING OF THE INPUT DATA, AND THE ALGORITHM WAS NOT
!             EVEN STARTED
!          OTHERWISE THE ALGORITHM WAS STARTED, AND THE MEANING OF
!          IOUT  IS AS FOLLOWS
!          IF  IOUT.EQ.0  NO TRIAL HAD A UNIFORM STOP.
!          IF  IOUT.NE.0  THE SIGN AND THE MAGNITUDE OF  IOUT  HAVE
!          THE FOLLOWING MEANING
!          A) THE SIGN OF  IOUT  INDICATES IF THE BEST UNIFORM STOP
!             (I.E. THE ONE IN WHICH  FTFOPT  WAS OBTAINED)
!             IS TO BE CONSIDERED SUCCESSFUL (IOUT>0) OR UNSUCCESSFUL
!          B) THE MAGNITUDE OF  IOUT  INDICATES WHICH ONE OF THE NUMERICAL
!             EQUALITY CRITERIA WAS SATISFIED IN THE BEST UNIFORM STOP
!          IABS(IOUT).EQ.1  RELATIVE DIFFERENCE CRITERION
!          IABS(IOUT).EQ.2  ABSOLUTE DIFFERENCE CRITERION
!          IABS(IOUT).EQ.3  BOTH CRITERIA
!          (IOUT  IS THE FINAL VALUE OF THE INTERNAL PARAMETER  ISTOPT
!          (AN OUTPUT INDICATOR OF THE USER-SUPPLIED SUBROUTINE PTRIAL )).
!          SUCCESS IS CLAIMED BY THE ALGORITHM IF  IOUT > 0,
!          I.E. IF AT LEAST ONE OF THE TRIALS STOPPED SUCCESSFULLY
!          (I.E. WITH A POSITIVE VALUE OF THE TRIAL STOP INDICATOR ISTOP ),
!          AND THE SUCCESSFUL TRIALS (AS COUNTED BY  KSUC )
!          ARE CURRENTLY VALID.

!  USER-SUPPLIED SUPROGRAMS

!  THE USER MUST PROVIDE THE FUNCTION  FUNCT  TO COMPUTE  F(X),
!  AND THE THREE OUTPUT SUBROUTINE  PTSEG, PTRIAL, PTKSUC .
!  THE CALLS TO THE OUTPUT SUBROUTINES ARE CONTROLLED BY  IPRINT
!  (INPUT PARAMETER TO  SIGMA).
!  A USER NOT INTERESTED IN USING ANY ONE OF THE OUTPUT SUBROUTINES
!  CAN SIMPLY PUT  IPRINT = -1.
!  SAMPLE VERSIONS OF THE OUTPUT SUBROUTINES ARE PROVIDED
!  WITH THE PACKAGE FOR THE BENEFIT OF THE AVERAGE USER.
!  IN THE FOLLOWING DESCRIPTION ALL NON-INTEGER(kind=i4) ARGUMENTS ARE
!  REAL (dp) (INTEGER(kind=i4) ARGUMENTS ARE INDICATED BY MEANS OF THE
!  FORTRAN IMPLICIT TYPE DEFINITION CONVENTION).

!     THE FUNCTION  FUNCT

!     FUNCT  MUST RETURN AS ITS VALUE THE VALUE AT  X  OF THE FUNCTION
!     TO BE MINIMIZED
!     THE DEFINITION STATEMENT IS
!        REAL (dp) FUNCTION  FUNCT (N, X)
!     WHERE
!     N   IS THE (INPUT) DIMENSION OF THE PROBLEM.
!     X   IS THE (INPUT) N-VECTOR CONTAINING THE COORDINATES OF THE
!         POINT  X  WHERE THE FUNCTION IS TO BE COMPUTED.

!     THE SUBROUTINE  PTSEG

!     PTSEG  IS CALLED (IF  IPRINT > 0) AT THE END OF EVERY OBSERVATION
!     PERIOD.
!     THE DEFINITION STATEMENT IS
!         SUBROUTINE  PTSEG ( N, XPFMIN, FPFMIN, FPFMAX, KP, NFEV, IPRINT )
!     WHERE
!     N   IS THE (INPUT) DIMENSION OF THE PROBLEM
!     FPFMIN, FPFMAX  ARE RESPECTIVELY THE MINIMUM AND THE MAXIMUM AMONG
!        THE VALUES OF  F(X) OBTAINED AT THE FINAL POINTS OF THE TRAJECTORY
!        SEGMENTS OF THE (JUST ELAPSED) OBSERVATION PERIOD  KP.
!     XPFMIN   IS AN N-VECTOR CONTAINING THE COORDINATES OF THE (FINAL) POINT
!        (OR POSSIBLY ONE OF THE POINTS) WHERE FPFMIN WAS OBTAINED.
!     KP   IS THE TOTAL NUMBER OF ELAPSED OBSERVATION PERIODS IN THE
!        CURRENT TRIAL.
!     NFEV   IS THE TOTAL NUMBER OF FUNCTION EVALUATIONS PERFORMED FROM
!        ALGORITHM START.

!     THE SUBROUTINE  PTRIAL

!     PTRIAL  IS CALLED (IF IPRINT >= 0) AT THE END OF EVERY TRIAL.
!     THE DEFINITION STATEMENT IS
!         SUBROUTINE PTRIAL ( N, XOPT, FOPT,
!                             FTFMIN, FTFMAX, FTFOPT,
!                             ISTOP, ISTOPT, NFEV, KP, IPRINT )
!     WHERE
!     N   IS THE (INPUT) DIMENSION OF THE PROBLEM.
!     XOPT   IS AN N-VECTOR CONTINING THE COORDINATES OF THE
!        POINT (OR POSSIBLY ONE OF THE POINTS) WHERE THE CURRENT
!        MINIMUM  FOPT  WAS OBTAINED.
!     FOPT   IS THE CURRENT BEST MINIMUM VALUE FOUND FOR  F  FROM ALGORITHM
!        START ( FOPT  IS UPDATED WHENEVER A FUNCTION VALUE IS COMPUTED).
!     FTFMIN, FTFMAX   ARE RESPECTIVELY THE MINIMUM AND THE MAXIMUM
!        AMONG THE VALUES OF  F(X)  OBTAINED AT THE FINAL POINTS OF
!        THE LAST TRAJECTORY SEGMENTS OF THE CURRENT TRIAL.
!     FTFOPT   IS THE CURRENT MINIMUM VALUE OF  FTFMIN  AMONG THE TRIALS WHICH
!        DID NOT STOP FOR REACHING THE MAXIMUM ALLOWED NUMBER OF SEGMENTS
!        (STOPPING INDICATOR  ISTOP = 0, SEE BELOW).  FTFOPT  IS USED BY
!        SIGMA  TO COMPUTE  KSUC  (INPUT PARAMETER TO THE OUTPUT SUBROUTINE
!        PTKSUK , SEE BELOW).
!     KP   IS THE TOTAL NUMBER OF ELAPSED OBSERVATION PERIODS IN HE CURRENT
!        TTRIAL.
!     NFEV   IS THE TOTAL NUMBER OF FUNCTION EVALUATIONS PERFORMED FROM
!        ALGORITHM START.
!     ISTOP   IS THE INDICATOR OF THE STOPPING CONDITION OF THE TRIAL,
!        AS FOLLOWS
!        ISTOP = 0
!           THE MAXIMUM NUMBER  NPMAX0  OF OBSERVATION PERIODS HAS BEEN REACHED.
!        ISTOP.NE.0
!           ALL THE END-OF-SEGMENT VALUES OF  F(X) , (EXCEPT FOR THE JUST
!           DISCARDED SEGMENT) ARE CLOSE ENOUGH TO THEIR COMMON MINIMUM VALUE
!           FPFMIN , WITH RESPECT TO AN ABSOLUTE OR RELATIVE DIFFERENCE
!           CRITERION, TO BE CONSIDERED NUMERICALLY EQUAL.
!           THE ABSOLUTE VALUE AND THE SIGN OF  ISTOP  HAVE THE
!           FOLLOWING MEANING.
!              THE ABSOLUTE VALUE INDICATES WHICH DIFFERENCE
!                 CRITERION WAS SATISFIED
!                 1   RELATIVE DIFFERENCE CRITERION SATISFIED
!                 2   ABSOLUTE DIFFERENCE CRITERION SATISFIED
!                 3   BOTH CRITERIA SATISFIED
!              THE SIGN OF  ISTOP  INDICATES THE RELATIONSHIP BETWEEN THE
!                 END-OF-TRIAL VALUE  FPFMIN  AND THE CURRENT BEST MINIMUM
!                 VALUE  FOPT  (WHICH IS UPDATED WHENEVER A FUNCTION VALUE
!                 IS COMPUTED
!                 ISTOP > 0
!                    FPFMIN  IS NUMERICALLY EQUAL (W.R.T. AT LEAST
!                    ONE OF THE ABOVE  DIFFERENCE CRITERIA) TO  FOPT
!                 ISTOP < 0
!                    FPFMIN  IS NOT EVEN NUMERICALLY EQUAL TO  FOPT
!                    (AND THEREFORE CANNOT BE CONSIDERED AS AN ACCEPTABLE
!                    GLOBAL MINIMUM).
!     ISTOPT   IS THE VALUE OF THE TRIAL STOPPING INDICATOR  ISTOP
!        CORRESPONDING TO THE (CURRENT OR PAST) TRIAL WHERE  FTFOPT  WAS
!        OBTAINED, WITH THE SIGN WHICH IS UPDATED ACCORDING TO THE COMPARISON
!        BETWEEN  FTFOPT  AND THE PRESENT VALUE OF FOPT , AS DESCRIBED ABOVE.
!        THE FINAL VALUE OF  ISTOP IS RETURNED BY  SIGMA  AS THE VALUE
!        OF THE OUTPUT INDICATOR IOUT OF THE ALGORITHM STOPPING CONDITIONS
!        (WHENEVER THE ALGORITHM WAS STARTED, IOUT.NE.-99, SEE ABOVE).

!     THE SUBROUTINE  PTKSUC

!     PTKSUC  IS CALLED ONLY AT THE END OF EVERY SUCCESSFUL TRIAL SUCH THAT
!     AN INCREMENT OCCURRED IN THE VALUE OF THE CURRENT MAXIMUM  MSUC  AMONG
!     ALL THE VALUES OF  KSUC  FROM ALGORITHM START.
!     A CALL TO  PTKSUC  THEREFORE PROVIDES THE USER WITH THE OPERATIONALLY
!     INTERESTING INFORMATION THAT ALGORITHM STOP WOULD HAVE TAKEN PLACE,
!     IF  NSUC  (INPUT PARAMETER TO  SIGMA ) HAD BEEN GIVEN A LOWER VALUE,
!     EQUAL TO THE CURRENT  KSUC .
!     PTKSUC  IS CALLED ONLY IF  IPRINT>=0  AND  KSUC<NSUC  .
!     THE DEFINITION STATEMENT IS
!         SUBROUTINE  PTKSUC ( KSUC )
!     WHERE  KSUC  IS THE CURRENT COUNT OF SUCCESSFUL TRIALS
!     (1 <= KSUC <= NSUC)

USE common_dincom, hc => h, dxc => dx, epsco => eps, xrmic => xrmin,   &
                   xrmac => xrmax, foptc => fopt, ntrajc => ntraj,  &
                   isegbc => isegbr, inkpbc => inkpbr, kpbr0c => kpbr0, &
                   ifepc => ifep, inhpc => inhp

INTEGER(kind=i4), INTENT(IN)        :: n
REAL (dp), INTENT(IN)      :: x0(:)
REAL (dp), INTENT(IN OUT)  :: h
REAL (dp), INTENT(IN OUT)  :: eps
REAL (dp), INTENT(IN OUT)  :: dx
INTEGER(kind=i4), INTENT(IN OUT)    :: ntraj
INTEGER(kind=i4), INTENT(IN OUT)    :: isegbr
INTEGER(kind=i4), INTENT(IN OUT)    :: kpbr0
INTEGER(kind=i4), INTENT(IN OUT)    :: inkpbr
INTEGER(kind=i4), INTENT(IN)        :: npmin
INTEGER(kind=i4), INTENT(IN)        :: npmax0
INTEGER(kind=i4), INTENT(IN)        :: inpmax
INTEGER(kind=i4), INTENT(IN)        :: nsuc
INTEGER(kind=i4), INTENT(IN OUT)    :: ntrial
REAL (dp), INTENT(IN)      :: tolrel
REAL (dp), INTENT(IN OUT)  :: tolabs
REAL (dp), INTENT(IN OUT)  :: xrmin(:)
REAL (dp), INTENT(IN OUT)  :: xrmax(:)
INTEGER(kind=i4), INTENT(IN)        :: kpasca
INTEGER(kind=i4), INTENT(IN OUT)    :: irand
INTEGER(kind=i4), INTENT(IN OUT)    :: inhp
INTEGER(kind=i4), INTENT(IN)        :: iprint
REAL (dp), INTENT(IN OUT)  :: xmin(:)
REAL (dp), INTENT(OUT)     :: fmin
INTEGER(kind=i4), INTENT(OUT)       :: nfev
INTEGER(kind=i4), INTENT(OUT)       :: iout

REAL (dp)  :: epsc, f, fopt, ftfmax, ftfmin, ftfopt

!  DATA FOR THE VARIATION OF NOISE COEFFICIENT

REAL (dp), SAVE  :: epsr = 1.d4, epsap = 10.d0, epsag = 1.d3, epsmax = 1.d15

INTEGER(kind=i4)  :: ic, iccom, icts, ifep, is, istop, istopt, kp, ksuc, npmax, ntes

ifep = 1

!  INITIALIZE COMMON AREA  /DINCOM/

CALL init(n,x0,h,eps,dx,irand,f,ntraj,isegbr,inkpbr,kpbr0,inhp,  &
          ifep,xrmin,xrmax,iout)

!  CHECK PARAMETER VALUES

IF (npmin <= 0 .OR. npmax0 < 0 .OR. inpmax <= 0 .OR. nsuc <= 0 .OR.   &
    ntrial <= 0) iout = -99
IF (iout == -99) RETURN

!  INITIALIZE VARIABLES

epsc = eps
npmax = npmax0
istopt = 0
istop = 0
iccom = (ntrial+ntrial+4) / 5
nfev = 0
ntes = nsuc
icts = nsuc - 1
ftfopt = f

!  START SERIES OF TRIALS

DO  ic = 1, ntrial

!  SET INITIALIZATION INDEX FOR NOISE GENERATOR

  is = 0
  IF (irand > 0) is = irand + ic - 1

!  INITIALIZE TRIAL

  IF (ic > 1 .AND. ic <= iccom) CALL reinit(x0,epsc,is,f,ifep)
  IF (ic > iccom) CALL reinit(xmin,epsc,is,fopt,ifep)

  ftfmin = f
  ftfmax = f
  nfev = nfev + 1

!  PRINT INITIAL CONDITIONS OF TRIAL

  IF (iprint > 0) CALL ptseg(n,x0,ftfmin,ftfmax,0,nfev)


!  DEACTIVATE SCALING

  IF (kpasca > npmax .OR. n <= 1) CALL nosca()

!  INITIALIZE COMMON AREA  /SCALE/

  IF (kpasca <= npmax .AND. n > 1) CALL inisca(n,ntraj)

!  PERFORM A TRIAL

  CALL trial(n,npmin,npmax,kpasca,tolrel,tolabs,iprint,xmin,  &
             ftfmin,ftfmax,nfev,kp,istop)


!     EVALUATE PAST TRIAL AND PREPARE NEXT TRIAL


!  SET TRIAL DURATION

  IF (istop == 0) npmax = npmax + inpmax

!  RESET CURRENT NUMBER OF SUCCESSES REQUIRED BEFORE STOPPING
!  COMPUTE INDICATOR OF TRIAL STOPPING CONDITIONS
!  UPDATE BEST CURRENT VALUES OF TRIAL STOPPING INDICATOR AND
!  OF FUNCTION  F(X)  AT TRIAL STOP

  IF (.NOT.((ftfmin > ftfopt .OR. istop == 0) .AND. istopt /= 0)) THEN
    IF (itolch(ftfopt,ftfmin,tolrel,tolabs) == 0) ntes = nsuc

    ftfopt = ftfmin
    istopt = istop
  END IF
  istopt = ABS(istopt)
  CALL rclopt(n,xmin,fopt)
  IF (itolch(ftfopt,fopt,tolrel,tolabs) == 0) istopt = -istopt
  IF (itolch(ftfmin,fopt,tolrel,tolabs) == 0) istop = -istop

!  END-OF-TRIAL PRINT OUT

  IF (iprint >= 0) CALL ptrial(n,xopt,fopt,ftfmin,ftfmax,ftfopt,  &
                               istop,istopt,nfev,kp,iprint)

!  UPDATE INITIAL VALUE OF NOISE COEFFICIENT FOR NEXT TRIAL

  IF (istop == 0) epsc = epsc / epsr
  IF (istop > 0) epsc = epsc * epsag
  IF (istop < 0 .AND. ic <= iccom) epsc = epsc * epsap
  IF (istop < 0 .AND. ic > iccom) epsc = epsc / epsr

!  UPDATE OPERATING CONDITIONS FOR SELECTING (IN THE NEXT TRIAL)
!  THE STARTING VALUE OF THE NOISE COEFFICIENT OF THE NEW TRAJECTORY
!  AFTER BRANCHING

  IF (istop == 0) ifep = 1
  IF (istop /= 0) ifep = 2

!  UPDATE, PRINT, AND CHECK TRIAL STOPPING CONDITIONS

  IF (istop > 0 .AND. istopt > 0) ntes = ntes - 1
  IF (ntes > 0 .AND. istopt > 0 .AND. istop > 0 .AND. ntes <= icts  &
         .AND. iprint >= 0) THEN
    ksuc = nsuc - icts
    CALL ptksuc(ksuc)
    icts = ntes - 1
  END IF
  IF (ntes <= 0 .AND. istopt > 0 .AND. istop > 0) GO TO 20

!  CONSTRAIN NOISE COEFFICIENT WITHIN BOUNDS

  epsc = MIN(epsc,epsmax)
  IF (epsc <= 0.d0) epsc = 1.d0
END DO
!  END OF SERIES OF TRIALS

!  INDICATOR OF STOPPING CONDITIONS

20 iout = istopt
fmin = fopt

RETURN

END SUBROUTINE sigma



SUBROUTINE init(nx,x0,h0,eps0,dx0,irand,f,n1,n2,n3,n4,inh,ife,xri,xra,it)
      !dir$ attribute forceinline :: init
      !dir$ attribute code_align : 32 :: init
      !dir$ optimize : 3
      !dir$ attribute optimization_parameter: target_arch=AVX :: init
!  INIT  PERFORMS THE INITIALIZATION OF THE FIRST TRIAL.
!  THE PART OF THE INITIALIZATION WHICH IS COMMON ALSO TO THE TRIALS
!  FOLLOWING THE FIRST ONE IS PERFORMED BY CALLING THE SUBROUTINE REINIT.

USE common_dincom

INTEGER(kind=i4), INTENT(IN)      :: nx
REAL (dp), INTENT(IN)    :: x0(:)
REAL (dp), INTENT(IN)    :: h0
REAL (dp), INTENT(IN)    :: eps0
REAL (dp), INTENT(IN)    :: dx0
INTEGER(kind=i4), INTENT(IN OUT)  :: irand
REAL (dp), INTENT(OUT)   :: f
INTEGER(kind=i4), INTENT(IN OUT)  :: n1
INTEGER(kind=i4), INTENT(IN OUT)  :: n2
INTEGER(kind=i4), INTENT(IN OUT)  :: n3
INTEGER(kind=i4), INTENT(IN OUT)  :: n4
INTEGER(kind=i4), INTENT(IN)      :: inh
INTEGER(kind=i4), INTENT(IN OUT)  :: ife
REAL (dp), INTENT(IN)    :: xri(:)
REAL (dp), INTENT(IN)    :: xra(:)
INTEGER(kind=i4), INTENT(OUT)     :: it

INTEGER(kind=i4), SAVE  :: npmax = 100, ntrajm = 20, ntraj0 = 7, inkpb0 = 10, &
                  kpbr00 = 3
INTEGER(kind=i4)        :: id, ix

! CHECK PARAMETER VALUES

it = 0
IF (nx > npmax .OR. nx < 1 .OR. h0 <= 0.d0 .OR. eps0 <= 0.d0 .OR. dx0  &
     <= 0.d0) it = -99
IF (it == -99) RETURN

! INITIALIZE SOME VARIABLES

inhp = inh
DO  ix = 1, nx
  xrmin(ix) = xri(ix)
  xrmax(ix) = xra(ix)
END DO
CALL nosca()
ntraj = n1
IF (ntraj == 0) ntraj = ntraj0
ntraj = MIN(ntraj,ntrajm)
ntraj = MAX(ntraj,3)
n1 = ntraj
isegbr = n2
IF (isegbr == 0) isegbr = (1+ntraj) / 2
isegbr = MIN(isegbr,ntraj)
isegbr = MAX(isegbr,1)
n2 = isegbr
inkpbr = n3
IF (inkpbr == 0) inkpbr = inkpb0
n3 = inkpbr
kpbr0 = n4
IF (kpbr0 == 0) kpbr0 = kpbr00
kpbr0 = MOD(kpbr0,inkpbr)
n4 = kpbr0
ndim = nx
ntrajr = ntraj - 1
ncf = 1
f = funct(nx,x0)
CALL stoopt(nx,x0,f)
DO  id = 1, ntraj
  h(id) = h0
  dx(id) = dx0
END DO

! INITIALIZE REMAINING VARIABLES

CALL reinit(x0,eps0,irand,f,ife)

RETURN
END SUBROUTINE init



SUBROUTINE reinit(x0,eps0,irand,f,ife)
      !dir$ attribute forceinline :: reinit
      !dir$ attribute code_align : 32 :: reinit
      !dir$ optimize : 3
      !dir$ attribute optimization_parameter: target_arch=AVX :: reinit

! N.B. Argument NX has been removed.

!  REINIT  PERFORMS THE INITIALIZATION OF ALL TRIALS FOLLOWING THE
!  FIRST TRIAL, AND PART OF THE INITIALIZATION OF THE FIRST TRIAL.

USE common_dincom

REAL (dp), INTENT(IN)    :: x0(:)
REAL (dp), INTENT(IN)    :: eps0
INTEGER(kind=i4), INTENT(IN OUT)  :: irand
REAL (dp), INTENT(IN)    :: f
INTEGER(kind=i4), INTENT(IN)      :: ife

REAL (dp)        :: g
REAL (dp), SAVE  :: epsv = 1.0_dp
INTEGER(kind=i4)          :: ic, id, it

! INITIALIZE RANDOM NOISE GENERATOR

g = chaos(irand)

ifep = ife
kgen = 0
ktim = 0
DO  id = 1, ntraj
  DO  ic = 1, ndim
    x(ic,id) = x0(ic)
  END DO
  ie(id) = 0
  DO  it = 1, ntrajr
    isvt(id,it) = 1
    vmvt(id,it) = f
  END DO
  eps(id) = eps0 * epsv ** (isegbr-id)
  vmcor(id) = f
  vcor(id) = f
END DO

RETURN
END SUBROUTINE reinit



SUBROUTINE trial(n,npmin,npmax,kpasca,tolrel,tolabs,iprint,xmin,  &
                 fmin,fmax,nfev,nr,istop)
      !dir$ attribute forceinline :: trial
      !dir$ attribute code_align : 32 :: trial
      !dir$ optimize : 3
      !dir$ attribute optimization_parameter: target_arch=AVX :: trial

!  THE SUBROUTINE  TRIAL  GENERATES A TRIAL, I.E. A SET OF COMPLETE
!  SIMULTANEOUS TRAJECTORIES BY REPEATEDLY PERFORMING
!     A CALL TO THE SUBROUTINE  GENEVA  WHICH GENERATES THE SET OF
!        SIMULTANEOUS TRAJECTORY SEGMENTS CORRESPONDING TO THE CURRENT
!        OBSERVATION PERIOD, AND PERFORMS THE TRAJECTORY SELECTION
!     A (POSSIBLE) CALL TO THE SUBROUTINE  PTSEG  WHICH PERFORMS
!        END-OF-SEGMENT OUTPUT
!     A CHECK OF THE TRIAL STOPPING CRITERIA (WITH THE AID OF THE
!        FUNCTION  ITOLCH
!     A DECISION ABOUT ACTIVATING OR DEACTIVATING THE SCALING OF
!        THE VARIABLES (ACTIONS PERFORMED BY CALLING THE SUBROUTINES
!        ACTSCA  AND  NOSCA).

INTEGER(kind=i4), INTENT(IN)        :: n
INTEGER(kind=i4), INTENT(IN)        :: npmin
INTEGER(kind=i4), INTENT(IN)        :: npmax
INTEGER(kind=i4), INTENT(IN)        :: kpasca
REAL (dp), INTENT(IN)      :: tolrel
REAL (dp), INTENT(IN OUT)  :: tolabs
INTEGER(kind=i4), INTENT(IN)        :: iprint
REAL (dp), INTENT(OUT)     :: xmin(:)
REAL (dp), INTENT(OUT)     :: fmin
REAL (dp), INTENT(IN OUT)  :: fmax
INTEGER(kind=i4), INTENT(IN OUT)    :: nfev
INTEGER(kind=i4), INTENT(OUT)       :: nr
INTEGER(kind=i4), INTENT(OUT)       :: istop

INTEGER(kind=i4), SAVE  :: irnf = 7
INTEGER(kind=i4)        :: ir

DO  ir = 1, npmax

! ACTIVATE SCALING

  IF (ir >= kpasca .AND. ir > n*irnf) CALL actsca()
  nr = ir

! GENERATE AND EVALUATE THE SIMULTANEOUS TRAJECTORY SEGMENTS PERIOD

  CALL geneva(n,xmin,fmin,fmax,nfev)

! PRINT RESULTS OF OBSERVATION PERIOD

  IF (iprint > 0) CALL ptseg(n,xmin,fmin,fmax,ir,nfev)

! CHECK TRIAL STOPPING CONDITIONS

  IF (ir >= npmin) THEN
    istop = itolch(fmax,fmin,tolrel,tolabs)
    IF (istop /= 0) EXIT
  END IF

END DO

CALL nosca()

RETURN
END SUBROUTINE trial



SUBROUTINE geneva(nx,xmin,fmin,fmax,ncef)
      !dir$ attribute forceinline :: geneva
      !dir$ attribute code_align : 32 :: geneva
      !dir$ optimize : 3
      !dir$ attribute optimization_parameter: target_arch=AVX :: geneva

!  GENEVA  PERFORMS THE GENERATION AND THE FINAL PROCESSING AND
!  EVALUATION OF THE SET OF TRAJECTORY SEGMENTS CORRESPONDING TO
!  THE CURRENT OBSERVATION PERIOD.
!  GENEVA  UPDATES THE SCALING ARRAYS  DIST  AND  BIAS  BY CALLING
!             THE SUBROUTINES  SEGSCA  AND  UPDSCA
!          GENERATES THE TRAJECTORY SEGMENTS BY CALLING THE SUB-
!             ROUTINE  PERIOD
!          DETERMINES SOME END-OF-SEGMENT RESULTS  (FPFMIN, FPFMAX,
!             XPFMIN)  USING THE RESCALING CAPABILITIES OF THE SUB-
!             ROUTINES  SEGSCA  AND  VARSCA.

USE common_dincom

INTEGER(kind=i4), INTENT(IN)     :: nx
REAL (dp), INTENT(OUT)  :: xmin(:)
REAL (dp), INTENT(OUT)  :: fmin
REAL (dp), INTENT(OUT)  :: fmax
INTEGER(kind=i4), INTENT(OUT)    :: ncef

REAL (dp)  :: fm
INTEGER(kind=i4)    :: ic, id, ifm

! UPDATE SCALING DATA

DO  id = 1, ntraj
  CALL segsca(id)
  CALL updsca(nx,x(1:,id))
END DO

! GENERATE THE SIMULTANEOUS TRAJECTORY SEGMENTS

CALL period()

! EXTRACT AND RESCALE SOME FINAL VALUES

fm = vcor(1)
fmax = vcor(1)
ifm = 1
DO  id = 2, ntraj
  fmax = MAX(fmax,vcor(id))
  IF (vcor(id) < fm) THEN
    fm = vcor(id)
    ifm = id
  END IF
END DO
DO  ic = 1, ndim
  xmin(ic) = x(ic,ifm)
END DO
fmin = fm
ncef = ncf
CALL segsca(ifm)
CALL varsca(nx,xmin)

RETURN
END SUBROUTINE geneva



SUBROUTINE period()
      !dir$ attribute forceinline :: period
      !dir$ attribute code_align : 32 :: period
      !dir$ optimize : 3
      !dir$ attribute optimization_parameter: target_arch=AVX :: period

!  PERIOD  IS CALLED BY SUBROUTINE  GENEVA  TO PERFORM THE GENERATION
!  OF THE TRAJECTORY SEGMENTS.
!  PERIOD  - COMPUTES THE DURATION OF THE OBSERVATION PERIOD, I.E. THE
!              NUMBER  NHP  OF ACCEPTED INTEGRATION STEPS IN A PERIOD
!          - COMPUTES ALL THE SEGMENT STEPS BY REPEATEDLY CALLING THE
!              SUBROUTINE  STEP
!          - PERFORMS THE SEGMENT SELECTION BY CALLING THE SUBROUTINE BRASI

USE common_dincom

INTEGER(kind=i4)  :: ik, nkgen

! DETERMINE DURATION OF OBSERVATION PERIOD
! (NUMBER OF TIME INTEGRATION STEPS)

kgen = kgen + 1
nkgen = 1
IF (inhp == 1) nkgen = LOG(kgen+.5D0)/LOG(2.d0) + 1
IF (inhp == 2) nkgen = SQRT(kgen+.5D0)
IF (inhp == 3) nkgen = kgen

! PERFORM THE REQUIRED NUMBER OF INTEGRATION STEPS
! (FOR ALL TRAJECTORIES)

DO  ik = 1, nkgen

! PERFORM A SINGLE INTEGRATION STEP
! (FOR ALL TRAJECTORIES)

  CALL step(ik)
END DO

! MANAGE THE TRAJECTORY BRANCHING PROCESS

CALL brasi()
RETURN
END SUBROUTINE period



SUBROUTINE brasi()
      !dir$ attribute forceinline :: brasi
      !dir$ attribute code_align : 32 :: brasi
      !dir$ optimize : 3
      !dir$ attribute optimization_parameter: target_arch=AVX :: brasi

!  BRASI  PERFORMS THE SELECTION PROCESS FOR THE TRAJECTORIES
!  BRASI  - UPDATES THE DATA ABOUT THE PAST TRAJECTORIES
!         - ASKS FOR THE TRAJECTORY-SELECTION ORDERING BY CALLING THE
!              SUBROUTINE  ORDER
!         - DISCARDS ONE OF THE TRAJECTORIES
!         - PERFORMS BRANCHING ON ONE OF THE REMAINING TRAJECTORIES
!         - MOVES THE DATA OF THE UNPERTURBED CONTINUATION
!              TO THE POSITION OF THE PERTURBED CONTINUATION
!         - CALLS THE SUBROUTINE  COMPAS  TO EXAMINE DATA ABOUT PAST
!              HISTORY OF THE TRAJECTORIES AND DISCARD IRRILEVANT DATA

USE common_dincom

REAL (dp), SAVE  :: dfac = 1000.0_dp, dlepmx = 25.0_dp, efac = 0.5_dp,  &
                    dlfacl = 0.301029995663981194_dp, epsmax = 1.0E+15_dp, &
                    facg = 10.0_dp
INTEGER(kind=i4)  :: ic, id, im, ip, it, iu

! UPDATE PAST HISTORY DATA

DO  id = 1, ntraj
  vmvt(id,ntrajr) = vmcor(id)
  isvt(id,ntrajr) = id
END DO

! OBTAIN TRAJECTORY-SELECTION ORDERING

CALL order(ip,im,iu)

! DECIDE WHICH TRAJECTORY IS TO BE BRANCHED

IF (MOD(kgen,inkpbr) == kpbr0) im = ip

! PERFORM BRANCHING

DO  ic = 1, ndim
  x(ic,iu) = x(ic,im)
END DO
h(iu) = h(im)
ie(iu) = ie(im)
DO  it = 1, ntrajr
  isvt(iu,it) = isvt(im,it)
  vmvt(iu,it) = vmvt(im,it)
END DO
eps(iu) = eps(im)
dx(iu) = dx(im)
vcor(iu) = vcor(im)
DO  id = 1, ntraj
  vmcor(id) = vcor(id)
END DO

! UPDATE PAST-HISTORY-DATA MATRICES

CALL compas()

! UPDATE SCALING DATA

CALL movsca(iu,im)

! UPDATE NOISE COEFFICIENT VALUES

IF (eps(iu) > 0.d0) THEN
  IF (ifep == 2) eps(iu) = eps(iu) * facg ** (chaos(0)-efac)
  IF (ifep == 1) eps(iu) = facg ** (  &
      MIN(dlepmx,LOG10(eps(iu))+(chaos(-1)-efac)*dlfacl))
  eps(iu) = MIN(eps(iu),epsmax)
END IF

! UPDATE MAGNITUDE OF SPATIAL DISCRETIZATION INCREMENT

dx(iu) = dx(iu) * dfac ** chaos(0)
RETURN
END SUBROUTINE brasi



SUBROUTINE order(ip,im,iu)
      !dir$ attribute forceinline :: order
      !dir$ attribute code_align : 32 :: order
      !dir$ optimize : 3
      !dir$ attribute optimization_parameter: target_arch=AVX :: order

!  ORDER  COMPARES THE TRAJECTORIES FROM THE POINT OV VIEW OF PAST
!  HISTORY (BY CALLING THE FUNCTION  IPREC ) AND FROM THE POINT OF VIEW
!  OF THEIR CURRENT VALUE OF  EPS  (BY CALLING THE FUNCTION  IPRECE)
!  AND PROVIDES THE TRAJECTORY ORDERING NEEDED FOR SELECTING THE TRAJECTORY
!  WHICH IS TO BE BRANCHED.

USE common_dincom

INTEGER(kind=i4), INTENT(OUT)  :: ip
INTEGER(kind=i4), INTENT(OUT)  :: im
INTEGER(kind=i4), INTENT(OUT)  :: iu

INTEGER(kind=i4)        :: i, i1, iord(20), ir, j, k1, k2, km
INTEGER(kind=i4), SAVE  :: kp = 0

ir = 0

! ASSIGN INITIAL ORDERING

10 DO  i = 1, ntraj
  iord(i) = i
END DO

! SORT TRAJECTORIES ...

30 ir = ir + 1

DO  i = 1, ntrajr
  i1 = i + 1
  DO  j = i1, ntraj
    k1 = iord(i)
    k2 = iord(j)

! ... ACCORDING TO PAST HISTORY ...

    IF (ir /= 2) kp = iprec(k1,k2)
    IF (kp == 0 .AND. ir == 1) GO TO 10

! ... OR ACCORDING TO NOISE LEVEL

    IF (ir == 2) kp = iprece(k1,k2)
    IF (kp /= 0) THEN
      km = k1 + k2 - kp
      iord(i) = kp
      iord(j) = km
    END IF
  END DO
END DO
IF (ir == 2) GO TO 30

! RETURN THE INDICES OF THE SEGMENTS WHICH IN THE ORDERING
! OCCUPY THE FIRST, THE LAST, AND A SUITABLE MEDIUM LEVEL POSITION

ip = iord(1)
iu = iord(ntraj)
im = iord(isegbr)

RETURN
END SUBROUTINE order



FUNCTION iprec(id1,id2) RESULT(ival)
      !dir$ attribute forceinline :: iprec
      !dir$ attribute code_align : 32 :: iprec
      !dir$ optimize : 3
      !dir$ attribute optimization_parameter: target_arch=AVX :: iprec

!  IPREC  DETERMINES THE PRECEDENCE RELATION BETWEEN TWO TRAJECTORIES
!  BASED ON THE PAST HISTORY DATA

USE common_dincom

INTEGER(kind=i4), INTENT(IN)  :: id1
INTEGER(kind=i4), INTENT(IN)  :: id2
INTEGER(kind=i4)              :: ival

REAL (dp)  :: vm1, vm2
INTEGER(kind=i4)    :: iit, it

vm1 = vmvt(id1,ntrajr)
vm2 = vmvt(id2,ntrajr)
DO  iit = 1, ntrajr
  it = 1 + ntrajr - iit
  IF (isvt(id1,it) == isvt(id2,it)) EXIT
  vm1 = MIN(vm1,vmvt(id1,it))
  vm2 = MIN(vm2,vmvt(id2,it))
END DO

ival = 0
IF (vm2 < vm1) ival = id2
IF (vm2 > vm1) ival = id1

RETURN
END FUNCTION iprec



FUNCTION iprece(id1,id2) RESULT(ival)
      !dir$ attribute forceinline :: iprece
      !dir$ attribute code_align : 32 :: iprece
      !dir$ optimize : 3
      !dir$ attribute optimization_parameter: target_arch=AVX :: iprece

!  IPRECE  DETERMINES THE PRECEDENCE RELATION BETWEEN TWO TRAJECTORIES
!  BASED ON THEIR CURRENT VALUE OF  EPS

USE common_dincom

INTEGER(kind=i4), INTENT(IN)  :: id1, id2
INTEGER(kind=i4)              :: ival

ival = 0
IF (kgen <= isegbr*inkpbr) THEN
  IF (eps(id2) < eps(id1)) ival = id1
  IF (eps(id2) > eps(id1)) ival = id2
  RETURN
END IF

IF (eps(id2) < eps(id1)) ival = id2
IF (eps(id2) > eps(id1)) ival = id1

RETURN
END FUNCTION iprece



SUBROUTINE compas()
      !dir$ attribute forceinline :: compas
      !dir$ attribute code_align : 32 :: compas
      !dir$ optimize : 3
      !dir$ attribute optimization_parameter: target_arch=AVX :: compas

!  COMPAS  TAKES CARE OF THE STORAGE OF PAST HISTORY DATA, DISCARDING
!  ALL DATA NOT NEEDED BY THE ONLY USER OF SUCH DATA, THE FUNCTION IPREC .

USE common_dincom

INTEGER(kind=i4)  :: id, idd, it, it1, itc, itm, ncn, ncv

it1 = 1
loop20:  &
DO  it = 2, ntrajr
  it1 = it - 1
  DO  id = 1, ntraj
    IF (isvt(id,it) /= isvt(id,it1)) CYCLE loop20
  END DO
  GO TO 100
END DO loop20
it1 = 1
ncv = 0
DO  id = 1, ntraj
  DO  idd = 1, ntraj
    IF (isvt(id,it1) == isvt(idd,it1)) ncv = ncv + 1
  END DO
END DO
DO  it = 2, ntrajr
  it1 = it - 1
  ncn = 0
  DO  id = 1, ntraj
    DO  idd = 1, ntraj
      IF (isvt(id,it) == isvt(idd,it)) ncn = ncn + 1
    END DO
  END DO
  IF (ncn == ncv) GO TO 100
  ncv = ncn
END DO
DO  id = 1, ntraj
  IF (isvt(1,1) /= isvt(id,1)) GO TO 100
END DO
it = 2
GO TO 140

100 it = it1 + 1
DO  id = 1, ntraj
  vmvt(id,it) = MIN(vmvt(id,it),vmvt(id,it1))
END DO

140 DO  itc = it, ntrajr
  itm = itc - 1
  DO  id = 1, ntraj
    vmvt(id,itm) = vmvt(id,itc)
    isvt(id,itm) = isvt(id,itc)
  END DO
END DO

RETURN
END SUBROUTINE compas



SUBROUTINE step(ik)
      !dir$ attribute forceinline :: step
      !dir$ attribute code_align : 32 :: step
      !dir$ optimize : 3
      !dir$ attribute optimization_parameter: target_arch=AVX :: step

!  STEP  PERFORMS A SINGLE TIME-INTEGRATION STEP FOR EACH ONE OF THE
!  SIMULTANEOUS TRAJECTORIES BY REPEATEDLY CALLING THE SUBROUTINE  SSTEP

USE common_dincom

INTEGER(kind=i4), INTENT(IN)  :: ik

REAL (dp)  :: f
REAL (dp)  :: xid(104), hid, epsid, dxid
!d
INTEGER(kind=i4)    :: id, ieid, ix, ka

ktim = ktim + 1

! LOOP ON THE SIMULTANEOUS TRAJECTORY SEGMENTS
DO  id = 1, ntraj

! INFORM THE SCALING SUBPROGRAMS (VIA THE COMMON AREA /SCALE/)
! THAT SCALING OPERATIONS ARE TO BE PERFORMED ON SEGMENT  ID .
  CALL segsca(id)
  f = vcor(id)

! PERFORM A TIME-INTEGRATION STEP ON SEGMENT  ID .
  ka = ktim
  nx = ndim
  DO  ix = 1, nx
    xid(ix) = x(ix,id)
  END DO
  hid = h(id)
  epsid = eps(id)
  dxid = dx(id)
  ieid = ie(id)

  CALL sstep(ka,nx,xid,hid,epsid,dxid,ieid,f)

  DO  ix = 1, nx
    x(ix,id) = xid(ix)
  END DO
  h(id) = hid
  eps(id) = epsid
  dx(id) = dxid
  ie(id) = ieid
  vcor(id) = f
  vmcor(id) = MIN(vmcor(id),f)
  IF (ik == 1) vmcor(id) = f
  IF (ktim == 1) ie(id) = 0
END DO

RETURN
END SUBROUTINE step



SUBROUTINE sstep(ktim,ndim,x,h,eps,dx,ie,f)
      !dir$ attribute forceinline :: sstep
      !dir$ attribute code_align : 32 :: sstep
      !dir$ optimize : 3
      !dir$ attribute optimization_parameter: target_arch=AVX :: sstep

!  THE BASIC TIME-INTEGRATION STEP FOR A GIVEN TRAJECTORY IS PERFORMED
!  BY THE SUBROUTINE  STEP  WHICH
!   - CALLS THE FUNCTION  FUNCT0  TO COMPUTE THE VALUE OF  F
!   - CALLS THE SUBROUTINE  UNITRV  TO COMPUTE THE RANDOM DIRECTION
!        ALONG WHICH THE DIRECTIONAL DERIVATIVE  GAM/N  IS TO BE COMPUTED
!   - CALLS THE SUBROUTINE  DERFOR (OR DERCEN ) TO COMPUTE THE FORWARD-
!        (OR CENTRAL-) DIFFERENCES DIRECTIONAL DERIVATIVE  GAM/N
!   - CALLS THE SUBROUTINE  NEWH  TO ACCEPT OR REJECT THE FIRST HALF-
!        STEP AND OBTAIN AN UPDATED VALUE FOR  H
!   - CALLS THE SUBROUTINE  CUMSCA  TO UPDATE THE CUMULATED SCALING DATA
!   - UPDATES THE SPATIAL DISCRETIZATION INCREMENT  DX  BASED ON THE
!        RESULTS OF CALLING THE FUNCTION  ITOLCH
!   - CALLS THE SUBROUTINE  GAUSRV  TO PERFORM THE SECOND HALF-STEP.


INTEGER(kind=i4), INTENT(IN OUT)    :: ktim
INTEGER(kind=i4), INTENT(IN)        :: ndim
REAL (dp), INTENT(IN OUT)  :: x(:)
REAL (dp), INTENT(IN OUT)  :: h
REAL (dp), INTENT(IN)      :: eps
REAL (dp), INTENT(IN OUT)  :: dx
INTEGER(kind=i4), INTENT(IN OUT)    :: ie
REAL (dp), INTENT(IN OUT)  :: f

REAL (dp)  :: dfdx, dfdxv
REAL (dp)  :: epsr, fs, fv, fvs, hs
REAL (dp)  :: w(104), xp(104)
!dir$ attribute align : 64 :: w
!dir$ attribute align : 64 :: xp
REAL (dp), SAVE  :: rdx = 1.0E-4_dp, dxmin = 1.0E-35_dp, dxmax = 1000.0_dp, &
                    hr = 0.1_dp, hmins = 1.0E-30_dp, stf = 100.0_dp,  &
                    rcd = 2.0_dp, tolri = 1.0E-5_dp, tolra = 1.0E-11_dp, &
                    tolabs = 0.0_dp
INTEGER(kind=i4)    :: ic, iec

iec = 0
fv = f

! TAKE A RANDOM DIRECTION FOR THE DIRECTIONAL DERIVATIVE
10 CALL unitrv(ndim,w)

! COMPUTE FORWARD-DIFFERENCE DERIVATIVE
CALL derfor(ndim,x,fv,dx,w,dfdx)

! TRY THE FIRST HALF-STEP
DO  ic = 1, ndim
  xp(ic) = x(ic) - h * w(ic) * dfdx * ndim
END DO
hs = h
f = funct0(ndim,xp)
fvs = fv + dx * ABS(dfdx)

! UPDATE STEPLENGTH  H  AND ACCEPT OR REJECT THE HALF-STEP
CALL newh(ktim,fvs,f,h,ie,iec)
IF (iec > 0) THEN
  ie = ie - 1
  iec = iec - 1
  h = hs
  dfdxv = dfdx

! COMPUTE CENTRAL-DIFFERENCES DERIVATIVE
  CALL dercen(ndim,x,fv,dx,w,dfdx)

! TRY AGAIN THE FIRST HALF-STEP
  DO  ic = 1, ndim
    xp(ic) = x(ic) - h * w(ic) * dfdx * ndim
  END DO
  f = funct0(ndim,xp)
  fvs = fv + dx * ABS(dfdxv-dfdx)


! UPDATE STEPLENGTH  H  AND ACCEPT OR REJECT THE HALF-STEP
  CALL newh(ktim,fvs,f,h,ie,iec)

! UPDATE CUMULATED PAST SCALING DATA
  IF (iec >= 1) CALL cumsca(ndim,w,dfdx)
  IF (iec >= 1) GO TO 10
  dx = dx * rdx
END IF

! FIRST HALF-STEP ACCEPTED

! UPDATE CUMULATED PAST SCALING DATA
CALL cumsca(ndim,w,dfdx)
fs = fv + dx * ABS(dfdx)
IF (itolch(fs,fv,tolri,tolabs) == 0) dx = dx / rcd
IF (itolch(fs,fv,tolra,tolabs) > 0) dx = dx * rcd
epsr = SQRT(h) * eps

! TAKE A SAMPLE INCREMENT OF THE WIENER PROCESS
CALL gausrv(ndim,w)

! TRY THE SECOND HALF-STEP
DO  ic = 1, ndim
  xp(ic) = xp(ic) + epsr * w(ic)
END DO
f = funct0(ndim,xp)

! ACCEPT OR REJECT THE COMPLETE STEP
IF (f-fv > eps*eps*stf) THEN
  h = h * hr
  ie = ie + 1
  IF (h > hmins) GO TO 10
END IF

! STEP ACCEPTED
DO  ic = 1, ndim
  x(ic) = xp(ic)
END DO
dx = MIN(dx,dxmax)
dx = MAX(dx,dxmin)

RETURN
END SUBROUTINE sstep



SUBROUTINE newh(k,fv,f,h,ie,iec)
      !dir$ attribute forceinline :: newh
      !dir$ attribute code_align : 32 :: newh
      !dir$ optimize : 3
      !dir$ attribute optimization_parameter: target_arch=AVX :: newh

!  NEWH  IS CALLED BY THE SUBROUTINE  SSTEP  TO DECIDE WHETHER TO ACCEPT
!  OR REJECT THE FIRST HALF-STEP, AND TO PROVIDE AN UPDATED VALUE FOR  H

INTEGER(kind=i4), INTENT(IN)        :: k
REAL (dp), INTENT(IN)      :: fv
REAL (dp), INTENT(IN)      :: f
REAL (dp), INTENT(IN OUT)  :: h
INTEGER(kind=i4), INTENT(IN OUT)    :: ie
INTEGER(kind=i4), INTENT(IN OUT)    :: iec

REAL (dp), PARAMETER  :: fa(4) = (/ 1.0_dp, 1.1_dp, 2.0_dp, 10.0_dp /),  &
                         fr(3) = (/ 1.05_dp, 2.0_dp, 10.0_dp /),  &
                         hmax = 1.0E+25_dp, hmin = 1.0E-30_dp
REAL (dp)      :: r
INTEGER(kind=i4), SAVE  :: iecmax = 50
INTEGER(kind=i4)        :: ic

IF (fv < f) GO TO 20

! STEP ACCEPTED, POSSIBLY INCREASE THE STEPLENGTH  H

r = fa(1)
IF (ie*2 < k) r = fa(2)
IF (ie*3 < k) r = fa(3)
IF (ie == 0 .AND. k > 1) r = fa(4)
h = h * r

10 iec = 0
GO TO 30

20 ie = ie + 1
iec = iec + 1
IF (iec > iecmax) GO TO 10

! STEP REJECTED, DECREASE  H

ic = MIN(3,iec)
r = fr(ic)
h = h / r

30 h = MIN(h,hmax)
h = MAX(h,hmin)

RETURN
END SUBROUTINE newh



SUBROUTINE derfor(ndim,x,f,dx,w,dfdx)
      !dir$ attribute forceinline :: derfor
      !dir$ attribute code_align : 32 :: derfor
      !dir$ optimize : 3
      !dir$ attribute optimization_parameter: target_arch=AVX :: derfor
!  DERFOR  COMPUTED THE FORWARD FINITE-DIFFERNCES DIRECTIONAL
!  DERIVATIVES (CALLING  FUNCT0 )

INTEGER(kind=i4), INTENT(IN)        :: ndim
REAL (dp), INTENT(IN)      :: x(:)
REAL (dp), INTENT(IN OUT)  :: f
REAL (dp), INTENT(IN OUT)  :: dx
REAL (dp), INTENT(IN)      :: w(:)
REAL (dp), INTENT(OUT)     :: dfdx

REAL (dp)        :: fn, s, xx(ndim)
REAL (dp), SAVE  :: dxff = 1.0E+6_dp, dxf = 10.0_dp, dxmax = 1.0E+6_dp
INTEGER(kind=i4)          :: ic

10 s = 0.d0
DO  ic = 1, ndim
  xx(ic) = x(ic) + w(ic) * dx
  s = s + (xx(ic)-x(ic)) ** 2
END DO
IF (s <= 0.d0) THEN
  dx = dx * dxff
  GO TO 10
END IF
fn = funct0(ndim,xx)
dfdx = (fn-f) / dx
IF (dx > dxmax) RETURN
IF (ABS(dfdx) > 1.d0) RETURN
IF (dfdx**2 <= 0.d0) THEN
  dx = dx * dxf
  GO TO 10
END IF

RETURN
END SUBROUTINE derfor



SUBROUTINE dercen(ndim,x,f,dx,w,dfdx)
      !dir$ attribute forceinline :: dercen
      !dir$ attribute code_align : 32 :: dercen
      !dir$ optimize : 3
      !dir$ attribute optimization_parameter: target_arch=AVX :: dercen

!  DERFOR  COMPUTED THE CENTRAL FINITE-DIFFERNCES DIRECTIONAL
!  DERIVATIVES (CALLING  FUNCT0 )

INTEGER(kind=i4), INTENT(IN)        :: ndim
REAL (dp), INTENT(IN)      :: x(:)
REAL (dp), INTENT(IN)      :: f
REAL (dp), INTENT(IN)      :: dx
REAL (dp), INTENT(IN)      :: w(:)
REAL (dp), INTENT(IN OUT)  :: dfdx

REAL (dp)  :: fn, fr, xx(ndim)

xx(1:ndim) = x(1:ndim) - w(1:ndim) * dx
fr = funct0(ndim,xx)
fn = f + dfdx * dx
dfdx = (fn-fr) / (2.d0*dx)

RETURN
END SUBROUTINE dercen



SUBROUTINE rclopt(n,xo,fo)
      !dir$ attribute forceinline :: rclopt
      !dir$ attribute code_align : 32 :: rclopt
      !dir$ optimize : 3
      !dir$ attribute optimization_parameter: target_arch=AVX :: rclopt

!  RCLOPT  RECALLS THE CURRENT BEST MINIMUM VALUE  FOPT  FOUND SO FAR
!  FROM ALGORITHM START, AND THE POINT  XOPT  (OR POSSIBLY ONE OF THE
!  POINTS) WHERE  FOPT  WAS OBTAINED

USE common_dincom

INTEGER(kind=i4), INTENT(IN)     :: n
REAL (dp), INTENT(OUT)  :: xo(:)
REAL (dp), INTENT(OUT)  :: fo

xo(1:n) = xopt(1:n)
fo = fopt

RETURN
END SUBROUTINE rclopt



SUBROUTINE stoopt(n,xo,fo)
      !dir$ attribute forceinline :: stoopt
      !dir$ attribute code_align : 32 :: stoopt
      !dir$ optimize : 3
      !dir$ attribute optimization_parameter: target_arch=AVX :: stoopt

!  STOOPT  STORES THE CURRENT BEST MINIMUM VALUE  FOPT  FOUND SO FAR
!  FROM ALGORITHM START, AND THE POINT  XOPT  (OR POSSIBLY ONE OF THE
!  POINTS) WHERE  FOPT  WAS OBTAINED

USE common_dincom

INTEGER(kind=i4), INTENT(IN)    :: n
REAL (dp), INTENT(IN)  :: xo(:)
REAL (dp), INTENT(IN)  :: fo

xopt(1:n) = xo(1:n)
fopt = fo

RETURN
END SUBROUTINE stoopt



FUNCTION funct0(n,xx) RESULT(fn_val)
      !dir$ attribute forceinline :: funct0
      !dir$ attribute code_align : 32 :: funct0
      !dir$ optimize : 3
      !dir$ attribute optimization_parameter: target_arch=AVX :: funct0

!  FUNCT0  IS CALLED  WHENEVER THE VALUE OF THE FUNCTION  F  IS REQUIRED
!  IN THE NUMERICAL INTEGRATION PROCESS.
!  THE FUNCTION  FUNCT0
!   - RESCALES THE VARIABLES BY CALLING THE SUBROUTINE  VARSCA
!   - CALLS THE SUBROUTINE  RANGE  TO TAKE CARE OF THE CASES WHERE THE
!      CURRENT POINT  X  IS OUTSIDE A GIVEN ADMISSIBLE REGION
!   - CALLS THE USER-SUPPLIED FUNCTION  FUNCT  TO ACTUALLY COMPUTE THE
!      VALUE OF  F
!   - POSSIBLY CALLS THE SUBROUTINE  STOOPT  TO UPDATE THE CURRENT BEST
!        MINIMUM FUNCTION VALUE  FOPT  AND THE CORRESPONDING MINIMIZER  XOPT

USE common_dincom

INTEGER(kind=i4), INTENT(IN)    :: n
REAL (dp), INTENT(IN)  :: xx(:)
REAL (dp)              :: fn_val

REAL (dp) :: f, r, xs(n)

xs(1:n) = xx(1:n)

! DESCALE  X-VARIABLES
CALL varsca(n,xs)

! CONSTRAIN THE  X-VARIABLES WITHIN BOUNDS
CALL range(n,xs,xrmin,xrmax,r)

! COMPUTE THE FUNCTION VALUE...
f = funct(n,xs) + r

! ... AND POSSIBLY UPDATE THE BEST CURRENT MINIMUM
IF (f < fopt) CALL stoopt(n,xs,f)
fn_val = f
ncf = ncf + 1

RETURN
END FUNCTION funct0



SUBROUTINE range(n,xs,xrmin,xrmax,r)
      !dir$ attribute forceinline :: range
      !dir$ attribute code_align : 32 :: range
      !dir$ optimize : 3
      !dir$ attribute optimization_parameter: target_arch=AVX :: range

!  RANGE  IS CALLED BY THE FUNCTION  FUNCT0  TO TAKE CARE OF THE CASES
!  WHERE THE CURRENT POINT  X  IS OUTSIDE A GIVEN ADMISSIBLE REGION

INTEGER(kind=i4), INTENT(IN)        :: n
REAL (dp), INTENT(IN OUT)  :: xs(:)
REAL (dp), INTENT(IN)      :: xrmin(:)
REAL (dp), INTENT(IN)      :: xrmax(:)
REAL (dp), INTENT(OUT)     :: r

REAL (dp) :: a, b, c, d, rr, xc
INTEGER(kind=i4)   :: i
REAL (dp), PARAMETER  :: rmax = 1.0E+35_dp, dlrmax = 80.5904782547915990_dp

r = 0.d0
DO  i = 1, n
  a = xrmax(i)
  c = xrmin(i)
  xc = xs(i)
  IF (xc > a) THEN
    b = a + a - c
    rr = rmax
    IF (xc < b) rr = EXP((xc-a)*dlrmax/(b-a)) - 1.d0
    r = r + rr
    xs(i) = xrmax(i)
  ELSE
    IF (xc < c) THEN
      d = c + c - a
      rr = rmax
      IF (xc > d) rr = EXP((c-xc)*dlrmax/(c-d)) - 1.d0
      r = r + rr
      xs(i) = xrmin(i)
    END IF
  END IF
END DO

RETURN
END SUBROUTINE range



FUNCTION itolch(fmax,fmin,tolrel,tolabs) RESULT(ival)
      !dir$ attribute forceinline :: itolch
      !dir$ attribute code_align : 32 :: itolch
      !dir$ optimize : 3
      !dir$ attribute optimization_parameter: target_arch=AVX :: itolch

!  ITOLCH  DETERMINES  WHETHER TWO QUANTITIES ARE TO BE CONSIDERED NU-
!  MERICALLY EQUAL WITH RESPECT TO AN ABSOLUTE (OR RELATIVE) DIFFERENCE
!  CRITERION, WITHIN GIVEN TOLERANCES

REAL (dp), INTENT(IN)  :: fmax, fmin, tolrel, tolabs
INTEGER(kind=i4)                :: ival

ival = 0

! CHECK RELATIVE DIFFERENCE AGAINST  TOLREL
IF (ABS(fmax-fmin) <= tolrel*(ABS(fmin)+ABS(fmax))/2.d0) ival = ival + 1

! CHECK ABSOLUTE DIFFERENCE AGAINST  TOLABS
IF (fmax-fmin <= tolabs) ival = ival + 2

RETURN
END FUNCTION itolch



SUBROUTINE inisca(n,nd)
      !dir$ attribute forceinline :: inisca
      !dir$ attribute code_align : 32 :: inisca
      !dir$ optimize : 3
      !dir$ attribute optimization_parameter: target_arch=AVX :: inisca
     
!  INISCA  INITIALIZES THE COOMON AREA  /SCALE/  FOR THE SCALING DATA

INTEGER(kind=i4), INTENT(IN)  :: n
INTEGER(kind=i4), INTENT(IN)  :: nd

INTEGER(kind=i4), SAVE  :: nxmsca = 10
INTEGER(kind=i4)        :: id, ix, iy

lsca = -1
IF (n > nxmsca .OR. n == 1) RETURN
lsca = 0
nx = n
nord = nd
idsca = 1

DO  id = 1, nord
  DO  ix = 1, nx
    DO  iy = 1, nx
      dist(ix,iy,id) = 0.d0
      gragra(ix,iy,id) = 0.d0
    END DO
    dist(ix,ix,id) = 1.d0
    bias(ix,id) = 0.d0
    gra(ix,id) = 0.d0
  END DO
  ngra(id) = 0
END DO

RETURN
END SUBROUTINE inisca



SUBROUTINE nosca()
      !dir$ attribute forceinline :: nosca
      !dir$ attribute code_align : 32 :: nosca
      !dir$ optimize : 3
      
!  NOSCA  DEACTIVATES THE SCALING

lsca = -1

RETURN
END SUBROUTINE nosca



SUBROUTINE segsca(id)
      !dir$ attribute forceinline :: segsca
      !dir$ attribute code_align : 32 :: segsca
      !dir$ optimize : 3
!  SEGSCA  SELECTS THE TRAJECTORY WHICH MUST BE RESCALED

INTEGER(kind=i4), INTENT(IN)  :: id

idsca = id

RETURN
END SUBROUTINE segsca



SUBROUTINE varsca(n,x)
      !dir$ attribute forceinline :: varsca
      !dir$ attribute code_align : 32 :: varsca
      !dir$ optimize : 3
      !dir$ attribute optimization_parameter: target_arch=skylake_avx512 :: varsca
      use omp_lib
!  VARSCA  COMPUTES THE RESCALED VARIABLE  AX + B

INTEGER(kind=i4), INTENT(IN)        :: n
REAL (dp), INTENT(IN OUT)  :: x(:)

REAL (dp)  :: xb(n)
!dir$ attribute align : 64 :: xb
INTEGER(kind=i4)    :: i

IF (lsca <= 0) RETURN
!dir$ assume_aligned x:64
!$omp simd simdlen(8) linear(i:1)
DO  i = 1, n
  xb(i) = bias(i,idsca) + DOT_PRODUCT( dist(i,1:n,idsca), x(1:n) )
END DO
x(1:n) = xb(1:n)

RETURN
END SUBROUTINE varsca



SUBROUTINE cumsca(n,w,dfdx)
      !dir$ attribute forceinline :: cumsca
      !dir$ attribute code_align : 32 :: cumsca
      !dir$ optimize : 3
      !dir$ attribute optimization_parameter: target_arch=skylake_avx512 :: cumsca
      use omp_lib
!  CUMSCA  STORES CUMULATED STATISTICAL DATA ON THE ILL-CONDITIONING OF
!  F(AX+B) W.R.T. X

INTEGER(kind=i4), INTENT(IN)    :: n
REAL (dp), INTENT(IN)  :: w(:)
REAL (dp), INTENT(IN)  :: dfdx

REAL (dp), SAVE  :: dfdxma = 1.0E+8_dp
INTEGER(kind=i4)          :: i, j

IF (lsca <= 0) RETURN
IF (ABS(dfdx) > dfdxma) RETURN
!dir$ assume_aligned w:64
DO  i = 1, n
  !$omp simd simdlen(8) reduction(+:gragra)
  DO  j = 1, n
    gragra(i,j,idsca) = gragra(i,j,idsca) + dfdx * w(i) * dfdx * w(j)
  END DO
  gra(i,idsca) = gra(i,idsca) + dfdx * w(i)
END DO
ngra(idsca) = ngra(idsca) + 1

RETURN
END SUBROUTINE cumsca



SUBROUTINE actsca()
      !dir$ attribute forceinline :: actsca
      !dir$ attribute code_align : 32 :: actsca
      !dir$ optimize : 3
!  ACTSCA  ACTIVATES THE RESCALING

IF (lsca == 0) lsca = 1

RETURN
END SUBROUTINE actsca



SUBROUTINE movsca(iu,im)
      !dir$ attribute forceinline :: movsca
      !dir$ attribute code_align : 32 :: movsca
      !dir$ optimize : 3
      !dir$ attribute optimization_parameter: target_arch=skylake_avx512 :: movsca
      use omp_lib
!  MOVSCA  MOVES THE SCALING DATA OF THE UNPERTURBED CONTINUATION TO THE
!  POSITION OF THE PERTURBED CONTINUATION

INTEGER(kind=i4), INTENT(IN)  :: iu
INTEGER(kind=i4), INTENT(IN)  :: im

INTEGER(kind=i4)  :: i, j

IF (lsca < 0) RETURN
DO  i = 1, nx
  !$omp simd simdlen(8)
  DO  j = 1, nx
    dist(i,j,iu) = dist(i,j,im)
    gragra(i,j,iu) = gragra(i,j,im)
  END DO
  bias(i,iu) = bias(i,im)
  gra(i,iu) = gra(i,im)
END DO
ngra(iu) = ngra(im)

RETURN
END SUBROUTINE movsca



SUBROUTINE updsca(n,x)
      !dir$ attribute forceinline :: updsca
      !dir$ attribute code_align : 32 :: updsca
      !dir$ optimize : 3
      !dir$ attribute optimization_parameter: target_arch=skylake_avx512 :: updsca
      use omp_lib
!  UPDSCA  UPDATES THE SCALING MATRIX  A  AND THE BIAS VECTOR  B BY
!  CALLING  EIGSCA  AND  VARSCA

INTEGER(kind=i4), INTENT(IN)    :: n
REAL (dp), INTENT(IN)  :: x(:)

REAL (dp)  :: agrai, ala1, alpha = 0.3_dp, amcor, biast(10)
REAL (dp)  :: cd, cor(10,16), distt(10,16), sn
!dir$ attribute align : 64 :: cor
!dir$ attribute align : 64 :: distt
INTEGER(kind=i4)    :: i, id, j, k

IF (lsca <= 0) RETURN
id = idsca
IF (ngra(id) >= 2*nx*nx) THEN
  agrai = 1.d0 / ngra(id)
  amcor = 0.d0
  !dir$ assume_aligned cor:64
  !dir$ assume_aligned gragra:64
  !dir$ assume_aligned gra:64
  DO  i = 1, nx
    !$omp simd simdlen(8)
    DO  j = 1, nx
      cor(i,j) = agrai * gragra(i,j,id) - agrai * gra(i,id) * agrai * gra(j,id)
      amcor = MAX(amcor, ABS(cor(i,j)))
    END DO
  END DO
  IF (amcor <= 0.d0) GO TO 110
  !dir$ assume_aligned cor:64
  DO  i = 1, nx
    !$omp simd simdlen(8) reduction(/:cor)
    DO  j = 1, nx
      cor(i,j) = cor(i,j) / amcor
    END DO
  END DO
  ala1 = eigsca(cor)
  cd = ala1 * (1.d0+alpha)
  DO  i = 1, nx
    cor(i,i) = cor(i,i) - cd
    biast(i) = x(i)
  END DO
  CALL varsca(nx,biast)
  sn = 0.d0
  !dir$ assume_aligned distt:64
  !dir$ assume_aligned cor:64
  DO  i = 1, nx
    DO  j = 1, nx
      distt(i,j) = 0.d0
      !$omp simd simdlen(8) reduction(-:dist)
      DO  k = 1, nx
        distt(i,j) = distt(i,j) - dist(i,k,id) * cor(k,j)
      END DO
      sn = sn + distt(i,j) ** 2
    END DO
  END DO
  sn = 1.d0 / SQRT(sn/DBLE(nx))
  !dir$ assume_aligned bias:64
  !dir$ assume_aligned dist:64
  !dir$ assume_aligned distt:64
  DO  i = 1, nx
    bias(i,id) = biast(i)
    !$omp simd simdlen(8) reduction(*:dist) reduction(-:bias)
    DO  j = 1, n
      dist(i,j,id) = sn * distt(i,j)
      bias(i,id) = bias(i,id) - dist(i,j,id) * x(j)
      gragra(i,j,id) = 0.d0
    END DO
    gra(i,id) = 0.d0
  END DO
  ngra(id) = 0
END IF

110 RETURN
END SUBROUTINE updsca



FUNCTION eigsca(cor) RESULT(fn_val)
      !dir$ attribute forceinline :: eigsca
      !dir$ attribute code_align : 32 :: eigsca
      !dir$ optimize : 3
      !dir$ attribute optimization_parameter: target_arch=skylake_avx512 :: eigsca
      use omp_lib
!  EIGSCA COMPUTES THE LARGEST EIGENVALUE OF A MATRIX USED FOR RESCA-
!  LING, STARTING FROM A RANDOMLY-CHOSEN ESTIMATE (OBTAINED BY CALLING
!  THE SUBROUTINE  UNITRV ) OF THE CORRESPONDING EIGENVECTOR

REAL (dp), INTENT(IN)  :: cor(10,16)
REAL (dp)              :: fn_val

REAL (dp)  :: ala1, ala1i, ala1o
REAL (dp)  :: prec = 0.001_dp, sww, w(16), ww(16)
!dir$ assume_aligned w:64
!dir$ assume_aligned ww:64
INTEGER(kind=i4)    :: ir, ix, jx, nrmin = 10, nrmax = 100

CALL unitrv(nx,w)
ala1 = 0.d0
!dir$ assume_aligned ww:64
!dir$ assume_aligned cor:64
!dir$ assume_aligned w:64
DO  ir = 1, nrmax
  ala1o = ala1
  sww = 0.d0
  DO  ix = 1, nx
    ww(ix) = 0.d0
    !$omp simd simdlen(8) reduction(+:ww)
    DO  jx = 1, nx
      ww(ix) = ww(ix) + cor(ix,jx) * w(jx)
    END DO
    sww = sww + ww(ix) ** 2
  END DO
  ala1 = SQRT(sww)
  ala1i = 1.d0 / ala1
  IF (ir >= nrmin .AND. ala1*prec > ABS(ala1-ala1o)) EXIT
  w(1:nx) = ww(1:nx) * ala1i
END DO
fn_val = ala1

RETURN
END FUNCTION eigsca



FUNCTION chaos(iniz) RESULT(fn_val)
      !dir$ attribute forceinline :: chaos
      !dir$ attribute code_align : 32 :: chaos
      !dir$ optimize : 3
      !dir$ attribute optimization_parameter: target_arch=AVX :: chaos

!  CHAOS  GENERATES A RANDOM SAMPLE FROM ONE OUT OF FOUR POSSIBLE
! PROBABILITY DISTRIBUTIONS USING RANDOM NUMBERS UNIFORMLY
! DISTRIBUTED IN (0,1) GENERATED BY THE FUNCTION  UNIFRD

!  INIZ  IS AN INPUT PARAMETER AS FOLLOWS

!      INIZ>0          INITIALIZATION WITH SEED  INIZ.
!      INIZ=0          STANDARD GAUSSIAN DISTRIBUTION.
!      INIZ=-1         CAUCHY DISTRIBUTION.
!      INIZ=-2         UNIFORM DISTRIBUTION IN (-1,+1).
!      OTHERWISE       UNIFORM DISTRIBUTION IN ( 0,+1).

INTEGER(kind=i4), INTENT(IN)  :: iniz
REAL (dp)            :: fn_val

REAL (dp) :: a, b
REAL (dp), PARAMETER  :: pai = 3.14159265358979324_dp

IF (iniz > 0) THEN

! INITIALIZATION.

  fn_val = unifrd(iniz)
  RETURN
END IF

a = unifrd(0)
IF (iniz == 0) THEN
  b = unifrd(0)

! GAUSSIAN RANDOM NUMBER BY POLAR METHOD

  fn_val = SQRT(-2.d0*LOG(a)) * COS(pai*b)

  RETURN
END IF

! UNIFORM RANDOM NUMBER IN (0,1)

fn_val = a

! CAUCHY RANDOM NUMBER BY INVERSE TRANSFORMATION

IF (iniz == -1) fn_val = TAN(pai*a)

! UNIFORM RANDOM NUMBER IN (-1,+1)

IF (iniz == -2) fn_val = 2.d0 * a - 1.d0

RETURN
END FUNCTION chaos



FUNCTION unifrd(iniz) RESULT(fn_val)
      !dir$ attribute forceinline :: unifrd
      !dir$ attribute code_align : 32 :: unifrd
      !dir$ optimize : 3
      !dir$ attribute optimization_parameter: target_arch=AVX :: unifrd

! UNIFRD GENERATES THE RANDOM NUMBERS UNIFORMLY DISTRIBUTED IN (0,1)
! EXPLOITING THOSE GENERATED BY  ALKNUT  WITH A FURTHER RANDOMIZATION
! IF THE INPUT PARAMETER INIZ  IS NOT 0
! THE RANDOM NUMBER GENERATOR IS INITIALIZED

INTEGER(kind=i4), INTENT(IN)  :: iniz
REAL (dp)            :: fn_val

REAL (dp), SAVE  :: x(61)
REAL (dp)        :: x0, p
INTEGER(kind=i4)          :: i0, k

INTEGER(kind=i4), SAVE    :: nrem = 61, nrip = 100, irem = 0
REAL (dp), SAVE  :: a = -1.5_dp, b = 5.5_dp, c = -2.0_dp, finv = 3.5E-5_dp, &
                    p0 = 3.0_dp, p1 = 1.0_dp, p2 = 3.0_dp, r1 = 0.25_dp,  &
                    r2 = 0.75_dp

IF (iniz == 0 .AND. irem /= 0) THEN

  i0 = irem
  x0 = x(i0)

! NONLINEARIZATION OF X0 TO AVOID LONG-DISTANCE LINEAR RELATIONSHIP.

  IF (x0 >= finv) x0 = MOD(1.d0/x0, 1.d0)

! UPDATE A COMPONENT OF THE VECTOR X ...

  CALL alknut(nrem,x,irem)

! ... AND FURTHER RANDOMIZE

  fn_val = MOD(x0+x(i0), 1.d0)

  RETURN
END IF

! INITIALIZATION OF THE RANDOM NUMBER GENERATOR

p = p0 - 1.d0 / DBLE(ABS(iniz) + 100.0)
DO  k = 1, nrem
  p = c + p * (b+p*a)
  x(k) = r1 + (r2-r1) * (p-p1) / (p2-p1)
END DO

irem = 0

DO  k = 1, nrip
  CALL alknut(nrem,x,irem)
END DO

fn_val = x(1)

RETURN
END FUNCTION unifrd



SUBROUTINE alknut(nrem,x,irem)
      !dir$ attribute forceinline :: alknut
      !dir$ attribute code_align : 32 :: alknut
      !dir$ optimize : 3
     

! UPDATES THE COMPONENT IREM OF THE NREM-VECTOR X WITH A RANDOM NUMBER
! UNIFORMLY DISTRIBUTED IN (0,1) BY MEANS OF THE ALGORITHM OF MITCHELL-MOORE,
! MODIFIED AS SUGGESTED BY BRENT, QUOTED IN D.E.KNUTH, THE ART OF COMPUTER
! PROGRAMMING, SECOND EDITION, SECOND VOLUME, SEMINUMERICAL ALGORITHMS,
! ADDISON-WESLEY PUB. CO., READING (1981), PP. 26-28.

INTEGER(kind=i4), INTENT(IN)        :: nrem
REAL (dp), INTENT(IN OUT)  :: x(:)
INTEGER(kind=i4), INTENT(IN OUT)    :: irem

INTEGER(kind=i4), SAVE  :: n1 = 24, n2 = 55
INTEGER(kind=i4), SAVE  :: i1, i2

IF (irem == 0) THEN
  irem = nrem
  i1 = nrem - n1
  i2 = nrem - n2
END IF

x(irem) = MOD(x(i1)+x(i2), 1.d0)

irem = 1 + MOD(irem,nrem)
i1 = 1 + MOD(i1,nrem)
i2 = 1 + MOD(i2,nrem)

RETURN
END SUBROUTINE alknut



SUBROUTINE gausrv(n,w)
      !dir$ attribute forceinline :: gausrv
      !dir$ attribute code_align : 32 :: gausrv
      !dir$ optimize : 3
      !dir$ attribute optimization_parameter: target_arch=AVX :: gausrv
! GENERATES A RANDOM VECTOR SAMPLE FROM AN N-DIMENSIONAL NORMAL DISTRIBUTION

INTEGER(kind=i4), INTENT(IN)     :: n
REAL (dp), INTENT(OUT)  :: w(:)

REAL (dp)  :: x, y, r
INTEGER(kind=i4)    :: i, ii, nn

nn = (n+1) / 2

DO  i = 1, nn
  ii = 1 + n - i

  10 x = chaos(-2)
  y = chaos(-2)
  r = x * x + y * y

  IF (r > 1.d0) GO TO 10

  r = SQRT(-2.d0*LOG(r)/r)

  w(i) = x * r
  w(ii) = y * r

END DO

RETURN
END SUBROUTINE gausrv



SUBROUTINE unitrv(n,w)
      !dir$ attribute forceinline :: unitrv
      !dir$ attribute code_align : 32 :: unitrv
      !dir$ optimize : 3
      !dir$ attribute optimization_parameter: target_arch=AVX :: unitrv
! GENERATES A RANDOM VECTOR UNIFORMLY DISTRIBUTED ON THE UNIT SPHERE.

INTEGER(kind=i4), INTENT(IN)        :: n
REAL (dp), INTENT(IN OUT)  :: w(:)

REAL (dp) :: ww

CALL gausrv(n,w)
ww = SUM( w(1:n)**2 )
ww = 1.d0 / SQRT(ww)
w(1:n) = ww * w(1:n)

RETURN
END SUBROUTINE unitrv

END MODULE stoch_optim

#if 0

MODULE common_tun
IMPLICIT NONE
INTEGER, SAVE  :: nprob
END MODULE common_tun



PROGRAM Test677

! (ALGORITHM SIGMA)
! MAIN PROGRAM (TEST VERSION)
! (CALL  SIGMA  VIA THE DRIVER  SIGMA1 )

USE stoch_optim
USE common_tun
IMPLICIT NONE

! COMMON AREA TO PASS TEST-PROBLEM NUMBER  NPROB
! TO THE FUNCTION  FUNCT  WHICH WILL COMPUTE
! THE FUNCTION VALUES OF TEST-PROBLEM  NPROB
! BY CALLING THE TEST-PROBLEM COLLECTION SUBROUTINE  GLOMTF

! COMMON /tun/ nprob

!  X0     INITIAL POINT
!  XMIN   FINAL ESTIMATE OF GLOBAL MINIMUM
!  XMINGL, XMAXGL  MUST BE DIMENSIONED HERE IN ORDER TO CALL
!         THE PRE-EXISTING SUBROUTINE  GLOMIP.
REAL (dp)  :: x0(100), xmin(100), xmingl(100), xmaxgl(100)

INTERFACE
  SUBROUTINE glomip(nprob, n, x0, xmin, xmax)
    USE toms667, ONLY: dp
    IMPLICIT NONE
    INTEGER, INTENT(IN)     :: nprob
    INTEGER, INTENT(OUT)    :: n
    REAL (dp), INTENT(OUT)  :: x0(:)
    REAL (dp), INTENT(OUT)  :: xmin(:)
    REAL (dp), INTENT(OUT)  :: xmax(:)
  END SUBROUTINE glomip
END INTERFACE

INTEGER    :: iout, iprint, n, nfev, nsuc
REAL (dp)  :: fmin

! INPUT PROBLEM NUMBER
10 WRITE (6,5000, ADVANCE='NO')
READ (5,5100) nprob
WRITE (6,5200) nprob

! TERMINATE OR CONTINUE
IF (nprob > 0 .AND. nprob < 38) THEN

! CALL  GLOMIP  TO GET PROBLEM DIMENSION  N  AND INITIAL POINT  X0
! NOTE THAT  GLOMIP  RETURNS ALSO THE BOUNDARIES  XMINGL ,  XMAXGL
! OF THE OBSERVATION REGION (NOT NEEDED HERE)
  CALL glomip(nprob,n,x0,xmingl,xmaxgl)

! SET  NSUC  SO AS TO HAVE GOOD CHANCES, WITHOUT PROHIBITIVE
! COMPUTATIONAL EFFORT
  nsuc = 5

! SET  IPRINT  SO AS TO HAVE A MODERATE OUTPUT
  iprint = 0

! CALL DRIVER SUBROUTINE  SIGMA1
  CALL sigma1(n,x0,nsuc,iprint,xmin,fmin,nfev,iout)

! GO TO THE NEXT PROBLEM
  GO TO 10
END IF

! END OF TEST PROBLEMS
WRITE (6,5300)

STOP
5000 FORMAT (///' INPUT PROBLEM NUMBER (1 TO 37, 0 = STOP): ')
5100 FORMAT (i2)
5200 FORMAT (//' PROBLEM NUMBER = ',i2//)
5300 FORMAT (/' END OF TEST PROBLEMS '/)
END PROGRAM Test677



FUNCTION funct(n,x) RESULT(fn_val)

! COMPUTES THE FUNCTION VALUES OF TEST-PROBLEM  NPROB
! BY CALLING THE SUBROUTINE  GLOMTF .

USE common_tun
USE mod_kinds, ONLY: dp
IMPLICIT NONE

INTEGER, INTENT(IN)    :: n
REAL (dp), INTENT(IN)  :: x(:)
REAL (dp)              :: fn_val

INTERFACE
  SUBROUTINE glomtf(nprob, n, x, funz)
    USE mod_kinds, ONLY: dp
    IMPLICIT NONE
    INTEGER, INTENT(IN)     :: nprob
    INTEGER, INTENT(IN)     :: n
    REAL (dp), INTENT(IN)   :: x(:)
    REAL (dp), INTENT(OUT)  :: funz
  END SUBROUTINE glomtf
END INTERFACE

REAL (dp)  :: f

! COMMON /tun/ nprob

CALL glomtf(nprob,n,x,f)
fn_val = f

RETURN
END FUNCTION funct




SUBROUTINE glomtf(nprob, n, x, funz)

!  THE SUBROUTINE  GLOMTF  PROVIDES THE CODING OF 37 REAL-VALUED
!  FUNCTIONS OF  N  REAL VARIABLES, TO BE USED, TOGETHER WITH THE
!  SUBROUTINE  GLOMIP , TO DEFINE 37 TEST PROBLEMS FOR GLOBAL
!  MINIMIZATION SOFTWARE.

!  THE SUBROUTINE  GLOMTF  RETURNS IN  FUNZ  THE FUNCTION VALUE
!  AT THE POINT  X = (X1,X2,...,XN)  FOR THE FUNCTION DEFINED BY
!  PROBLEM NUMBER  NPROB .

!  CALLING STATEMENT

!     CALL GLOMTF (NPROB, N, X, FUNZ)

!  DESCRIPTION OF THE CALL PARAMETERS
!     (THE FORTRAN IMPLICIT TYPE DEFINITION FOR INTEGERS IS USED.
!     ALL NON-INTEGER PARAMETERS ARE DOUBLE-PRECISION).

!  NPROB   IS THE (INPUT) TEST-PROBLEM NUMBER

!  N   IS THE (INPUT) DIMENSION OF THE PROBLEM

!  X   IS AN (INPUT) N-VECTOR CONTAINING THE INDEPENDENT VARIABLES

!  FUNZ   IS THE (OUTPUT) VALUE AT  X  OF THE FUNCTION DEFINED BY
!     PROBLEM NUMBER  NPROB .

USE mod_kinds, ONLY: dp
IMPLICIT NONE

INTEGER, INTENT(IN)     :: nprob
INTEGER, INTENT(IN)     :: n
REAL (dp), INTENT(IN)   :: x(:)
REAL (dp), INTENT(OUT)  :: funz

REAL (dp)  :: xx, di, yy, beta, s1, s2, a, b, r2, r4, r8, u, v, uu, vv, s
REAL (dp)  :: rg, rp, h, pund, range

REAL (dp)  :: y(10)

REAL (dp)  :: pi = 3.14159265358979323846D0, p1a = 0.1D0, p4a = 0.1D0,  &
              p5a = 0.1D0, p6a = 0.1D0
REAL (dp)  :: p9a = -1.4251284283197609708D0, p9b = -0.80032110047197312466D0
REAL (dp)  :: p17a = 1.275D0

REAL (dp)  :: p20a(10,4) = RESHAPE( (/  &
    4.d0, 1.d0, 8.d0, 6.d0, 3.d0, 2.d0, 5.d0, 8.d0, 6.d0, 7.d0,  &
    4.d0, 1.d0, 8.d0, 6.d0, 7.d0, 9.d0, 5.d0, 1.d0, 2.d0, 3.6D0, &
    4.d0, 1.d0, 8.d0, 6.d0, 3.d0, 2.d0, 3.d0, 8.d0, 6.d0, 7.d0,  &
    4.d0, 1.d0, 8.d0, 6.d0, 7.d0, 9.d0, 3.d0, 1.d0, 2.d0, 3.6D0 /), (/ 10, 4 /) )
REAL (dp)  :: p20b(10) = (/  &
    0.1D0, 0.2D0, 0.2D0, 0.4D0, 0.4D0, 0.6D0, 0.3D0, 0.7D0, 0.5D0, 0.5D0 /)
REAL (dp)  :: p21a(4,3) = RESHAPE( (/  3.d0,  0.1D0, 3.d0, 0.1D0,  &
                                      10.d0, 10.d0, 10.d0, 10.d0,  &
                                      30.d0, 35.d0, 30.d0, 35.d0 /), (/ 4, 3 /) )
REAL (dp)  :: p21b(4,3) = RESHAPE( (/ 0.3689D0, 0.4699D0, 0.1091D0, 0.03815D0, &
                                      0.1170D0, 0.4387D0, 0.8732D0, 0.5743D0, &
                                      0.2673D0, 0.7470D0, 0.5547D0, 0.8828D0 /), &
                                      (/ 4, 3 /) )
REAL (dp)  :: p21c(4) = (/ 1.0d0, 1.2D0, 3.d0, 3.2D0 /)
REAL (dp)  :: p21d =  -69.d0
REAL (dp)  :: p22a(4,6) = RESHAPE( (/ 10.d0, 0.05D0, 3.d0, 17.d0,  &
                                       3.d0, 10.d0, 3.5D0, 8.d0,   &
                                      17.d0, 17.d0, 1.7D0, 0.05D0, &
                                      3.5D0, 0.1D0, 10.d0, 10.d0,  &
                                      1.7D0, 8.d0,  17.d0, 0.1D0,  &
                                      8.d0, 14.d0,  8.d0, 14.d0 /), (/ 4, 6 /) )
REAL (dp)  :: p22b(4,6) = RESHAPE( (/ 0.1312D0, 0.2329D0, 0.2348D0, 0.4047D0, &
                                      0.1696D0, 0.4135D0, 0.1451D0, 0.8828D0, &
                                      0.5569D0, 0.8307D0, 0.3522D0, 0.8732D0, &
                                      0.0124D0, 0.3736D0, 0.2883D0, 0.5743D0, &
                                      0.8283D0, 0.1004D0, 0.3047D0, 0.1091D0, &
                                      0.5886D0, 0.9991D0, 0.6650D0, 0.0381D0 /), &
                                      (/ 4, 6 /) )
REAL (dp)  :: p22c(4) = (/ 1.d0, 1.2D0, 3.d0, 3.2D0 /)
REAL (dp)  :: p22d = -69.d0
REAL (dp)  :: p36a = 100.0_dp, p36b = 1.0_dp, p36c = 10.0_dp, p36d = 0.98_dp
REAL (dp)  :: p37a = 10.0_dp, p37b = 1.0_dp, p37c = 10.0_dp, p37d = 0.98_dp

INTEGER    :: i, j, m

SELECT CASE (nprob)
  CASE (1)
    !    1  A FOURTH ORDER POLYNOMIAL  (N = 1)

    funz = ((0.25D0*x(1)*x(1) - 0.5D0)*x(1) + p1a) * x(1)

  CASE (2)
    !    2  GOLDSTEIN SIXTH ORDER POLYNOMIAL  (N = 1)

    xx = x(1) * x(1)
    funz = ((xx-15.d0)*xx+27.d0) * xx + 250.d0

  CASE (3)
    !    3  ONE-DIMENSIONAL PENALIZED SHUBERT FUNCTION  (N = 1)

    funz = 0.d0
    DO  i = 1, 5
      di = i
      funz = funz + di * COS((di+1.d0)*x(1)+di)
    END DO
    IF (ABS(x(1)) > 10.d0) funz = funz + penfun(x(1),10.d0,100.d0,2)

  CASE (4)
    !    4  A FOURTH ORDER POLYNOMIAL IN TWO VARIABLES  (N = 2)

    xx = x(1) * x(1)
    yy = x(2) * x(2)
    funz = 0.25D0 * xx * xx - 0.5D0 * xx + p4a * x(1) + 0.5D0 * yy

  CASE (5)
    !    5  A FUNCTION WITH A SINGLE ROW OF LOCAL MINIMA  (N = 2)

    funz = 0.5D0 * (p5a*x(1)*x(1) + 1.d0 - COS(2.d0*x(1))) + x(2)*x(2)

  CASE (6)
    !    6  SIX-HUMP CAMEL FUNCTION  (N = 2)

    xx = x(1) * x(1)
    yy = x(2) * x(2)
    funz = ((xx/3.d0 - (2.d0 + p6a))*xx + 4.d0)*xx + x(1)*x(2) + 4.d0*(yy-1.d0)*yy

  CASE (7)
    !    7  TWO-DIMENSIONAL PENALIZED SHUBERT FUNCTION, BETA = 0  (N = 2)

    beta = 0.d0
    GO TO 110

  CASE (8)
    !    8  TWO-DIMENSIONAL PENALIZED SHUBERT FUNCTION, BETA = 1/2  (N = 2)

    beta = 0.5D0
    GO TO 110

  CASE (9)
    !    9  TWO-DIMENSIONAL PENALIZED SHUBERT FUNCTION, BETA = 1  (N = 2)

    beta = 1.d0
    GO TO 110

  CASE (10)
    !   10  A FUNCTION WITH THREE ILL-CONDITIONED MINIMA, A=10  (N = 2)

    a = 1.d1
    b = 1.d-1
    GO TO 190

  CASE (11)
    !   11  A FUNCTION WITH THREE ILL-CONDITIONED MINIMA, A=10**2  (N = 2)

    a = 1.d2
    b = 1.d-2
    GO TO 190

  CASE (12)
    !   12  A FUNCTION WITH THREE ILL-CONDITIONED MINIMA, A=10**3  (N = 2)

    a = 1.d3
    b = 1.d-3
    GO TO 190

  CASE (13)
    !   13  A FUNCTION WITH THREE ILL-CONDITIONED MINIMA, A=10**4  (N = 2)

    a = 1.d4
    b = 1.d-4
    GO TO 190

  CASE (14)
    !   14  A FUNCTION WITH THREE ILL-CONDITIONED MINIMA, A=10**5  (N = 2)

    a = 1.d5
    b = 1.d-5
    GO TO 190

  CASE (15)
    !   15  A FUNCTION WITH THREE ILL-CONDITIONED MINIMA, A=10**6  (N = 2)

    a = 1.d6
    b = 1.d-6
    GO TO 190

  CASE (16)
    !   16  GOLDSTEIN-PRICE FUNCTION  (N = 2)

    u = x(1) + x(2) + 1.d0
    v = 2.d0 * x(1) - 3.d0 * x(2)
    uu = u * u
    vv = v * v
    funz = (1.d0+uu*(36.d0-20.d0*u+3.d0*uu)) * (30.d0+vv*(18.d0-16.d0*v+3.d0*vv))

  CASE (17)
    !   17  PENALIZED BRANIN FUNCTION  (N = 2)

    funz = (x(2)-p17a*(x(1)/pi)**2+(5.d0/pi)*x(1)-6.d0) ** 2 + 10.d0 *  &
           (1.d0-1.d0/(8.d0*pi)) * COS(x(1)) + 10.d0
    IF (ABS(x(1)-2.5D0) > 7.5D0) funz = funz + penfun(x(1)-2.5D0,7.5D0,100.d0,2)
    IF (ABS(x(2)-7.5D0) > 7.5D0) funz = funz + penfun(x(2)-7.5D0,7.5D0,100.d0,2)

  CASE (18)
    !   18  PENALIZED SHEKEL FUNCTION, M = 5  (N = 4)

    m = 5
    GO TO 250

  CASE (19)
    !   19  PENALIZED SHEKEL FUNCTION, M = 7  (N = 4)

    m = 7
    GO TO 250

  CASE (20)
    !   20  PENALIZED SHEKEL FUNCTION, M = 10  (N = 4)

    m = 10
    GO TO 250

  CASE (21)
    !   21  PENALIZED THREE-DIMENSIONAL HARTMAN FUNCTION  (N = 3)

    m = 4
    funz = 0.d0
    DO  i = 1, m
      s = 0.d0
      DO  j = 1, n
        s = s - p21a(i,j) * (x(j)-p21b(i,j)) ** 2
      END DO
      IF (s >= p21d) funz = funz - p21c(i) * EXP(s)
    END DO
    GO TO 350

  CASE (22)
    !   22  PENALIZED SIX-DIMENSIONAL HARTMAN FUNCTION  (N = 6)

    m = 4
    funz = 0.d0
    DO  i = 1, m
      s = 0.d0
      DO  j = 1, n
        s = s - p22a(i,j) * (x(j)-p22b(i,j)) ** 2
      END DO
      IF (s >= p22d) funz = funz - p22c(i) * EXP(s)
    END DO
    GO TO 350

  CASE (23:25)
    !   23  PENALIZED LEVY-MONTALVO FUNCTION, TYPE 1  (N = 2)

    !   24  PENALIZED LEVY-MONTALVO FUNCTION, TYPE 1  (N = 3)

    !   25  PENALIZED LEVY-MONTALVO FUNCTION, TYPE 1  (N = 4)

    y(1:n) = 1.d0 + 0.25D0 * (x(1:n) - 1.d0)
    GO TO 450

  CASE (26:28)
    !   26  PENALIZED LEVY-MONTALVO FUNCTION, TYPE 2  (N = 5)

    !   27  PENALIZED LEVY-MONTALVO FUNCTION, TYPE 2  (N = 8)

    !   28  PENALIZED LEVY-MONTALVO FUNCTION, TYPE 2  (N = 10)

    y(1:n) = x(1:n)
    GO TO 450

  CASE (29:31)
    !   29  PENALIZED LEVY-MONTALVO FUNCTION, TYPE 3, RANGE = 10   (N = 2)

    !   30  PENALIZED LEVY-MONTALVO FUNCTION, TYPE 3, RANGE = 10   (N = 3)

    !   31  PENALIZED LEVY-MONTALVO FUNCTION, TYPE 3, RANGE = 10   (N = 4)

    range = 10.d0
    GO TO 530

  CASE (32:34)
    !   32  PENALIZED LEVY-MONTALVO FUNCTION, TYPE 3, RANGE = 5   (N = 5)

    !   33  PENALIZED LEVY-MONTALVO FUNCTION, TYPE 3, RANGE = 5   (N = 6)

    !   34  PENALIZED LEVY-MONTALVO FUNCTION, TYPE 3, RANGE = 5  (N = 7)

    range = 5.d0
    GO TO 530

  CASE (35)
    !   35  A FUNCTION WITH A SINGLE CUSP-SHAPED MINIMUM  (N = 5)

    funz = 0.d0
    DO  i = 1, n
      funz = funz + i * x(i) * x(i)
    END DO
    funz = SQRT(SQRT(funz))

  CASE (36)
    !   36  A FUNCTION WITH A SMALL-ATTRACTION-REGION GLOBAL MINIMUM (N = 2)

    rg = p36a
    rp = p36b
    h = p36c
    pund = p36d
    GO TO 610

  CASE (37)
    !   37  A FUNCTION WITH A SMALL-ATTRACTION-REGION GLOBAL MINIMUM (N = 5)

    rg = p37a
    rp = p37b
    h = p37c
    pund = p37d
    GO TO 610

END SELECT
RETURN

110 funz = ((x(1)-p9a)**2 + (x(2)-p9b)**2) * beta
s1 = 0.d0
s2 = 0.d0
DO  i = 1, 5
  di = i
  s1 = s1 + di * COS((di+1.d0)*x(1) + di)
  s2 = s2 + di * COS((di+1.d0)*x(2) + di)
END DO
funz = funz + s1 * s2
IF (ABS(x(1)) > 10.d0) funz = funz + penfun(x(1),10.d0,100.d0,2)
IF (ABS(x(2)) > 10.d0) funz = funz + penfun(x(2),10.d0,100.d0,2)
RETURN

190 xx = x(1) * x(1)
yy = x(2) * x(2)
r2 = xx + yy
r4 = r2 * r2
r8 = r4 * r4
funz = a * xx + yy - r4 + b * r8
RETURN

250 funz = 0.d0
DO  i = 1, m
  s = p20b(i)
  DO  j = 1, n
    s = s + (x(j)-p20a(i,j)) ** 2
  END DO
  funz = funz - 1.d0 / s
END DO
DO  i = 1, n
  IF (ABS(x(i)-5.d0) > 5.d0) funz = funz + penfun(x(i)-5.d0,5.d0,100.d0,2)
END DO
RETURN

350 DO  i = 1, n
  IF (ABS(x(i)-0.5D0) > 0.5D0) funz = funz + penfun(x(i)-0.5D0,0.5D0,100.d0,2)
END DO
RETURN

450 funz = 10.d0 * SIN(pi*y(1)) ** 2 + (y(n)-1.d0) ** 2
DO  i = 2, n
  funz = funz + (y(i-1)-1.d0) ** 2 * (1.d0 + 10.d0*SIN(pi*y(i))**2)
END DO
funz = funz * pi / n
range = 10.d0
GO TO 550

530 funz = SIN(3.d0*pi*x(1))**2 + (x(n) - 1.d0)**2 * (1.d0 + SIN(2.d0*pi*x(n))**2)
DO  i = 2, n
  funz = funz + (x(i-1)-1.d0)**2 * (1.d0 + SIN(3.d0*pi*x(i))**2)
END DO
funz = funz * 0.1D0

550 DO  i = 1, n
  IF (ABS(x(i)) > range) funz = funz + penfun(x(i),range,100.d0,4)
END DO
RETURN

610 s = SUM( x(2:n)**2 )
funz = s + x(1) * x(1)
s = s + (x(1)-rg) ** 2
IF (s < rp*rp*pund) funz = funz - (rg*rg+h) * EXP(-s/(rp*rp-s))
RETURN

CONTAINS


FUNCTION penfun(var, ran, fact, iexp) RESULT(fn_val)
!     PENALIZATION FUNCTION

REAL (dp), INTENT(IN)  :: var, ran, fact
INTEGER, INTENT(IN)    :: iexp
REAL (dp)              :: fn_val

fn_val = fact * (ABS(var) - ran) ** iexp
RETURN
END FUNCTION penfun

END SUBROUTINE glomtf



SUBROUTINE glomip(nprob, n, x0, xmin, xmax)

!  THE SUBROUTINE  GLOMIP  PROVIDES THE CODING FOR THE NUMBER OF VARIABLES,
!  THE INITIAL POINT, AND THE OBSERVATION REGION TO BE USED, TOGETHER WITH
!  THE 37 TEST FUNCTIONS GIVEN BY SUBROUTINE  GLOMTF , TO DEFINE 37 TEST
!  PROBLEMS FOR GLOBAL MINIMIZATION SOFTWARE.

!  THE SUBROUTINE  GLOMIP  RETURNS IN  N , X0 , AND  XMIN ,
!  XMAX  THE NUMBER OF VARIABLES, THE INITIAL POINT, AND THE
!  BOUNDARIES OF THE OBSERVATION REGION.

!  CALLING STATMENT
!     CALL GLOMIP (NPROB, N, X0, XMIN, XMAX)

!  DESCRIPTION OF THE CALL PARAMETERS
!     (THE FORTRAN IMPLICIT TYPE DEFINITION FOR INTEGERS IS USED.
!     ALL NON-INTEGER PARAMETERS ARE DOUBLE-PRECISION).

!  NPROB   IS THE (INPUT) NUMBER OF THE TEST PROBLEM TO BE CONSIDERED
!  N       IS THE (OUTPUT) NUMBER OF VARIABLES (DIMENSION) OF THE PROBLEM
!  XMIN , XMAX   ARE THE (OUTPUT) N-VECTORS CONTAINING THE LEFT AND RIGHT
!          BOUNDARIES OF THE OBSERVATION REGION DEFINED BY THE POINTS
!          X = (X1,...,XN) SUCH THAT XMIN(I) <= X(I) <= XMAX(I), I = 1,...,N

USE mod_kinds, ONLY: dp
IMPLICIT NONE

INTEGER, INTENT(IN)     :: nprob
INTEGER, INTENT(OUT)    :: n
REAL (dp), INTENT(OUT)  :: x0(:)
REAL (dp), INTENT(OUT)  :: xmin(:)
REAL (dp), INTENT(OUT)  :: xmax(:)

REAL (dp) :: v0, vmin, vmax
INTEGER   :: i

SELECT CASE (nprob)

  CASE (1)
    !    1  A FOURTH-ORDER POLYNOMIAL  (N = 1)

    n = 1
    v0 = 1.0D0
    vmin = -10.d0
    vmax = 10.d0
    GO TO 430

  CASE (2)
    !    2  GOLDSTEIN SIXTH ORDER POLYNOMIAL  (N = 1)

    n = 1
    v0 = 0.d0
    vmin = -4.d0
    vmax = 4.d0
    GO TO 430

  CASE (3)
    !    3  ONE-DIMENSIONAL PENALIZED SHUBERT FUNCTION  (N = 1)

    n = 1
    v0 = 0.d0
    vmin = -10.d0
    vmax = 10.d0
    GO TO 430

  CASE (4)
    !    4  A FOURTH ORDER POLYNOMIAL IN TWO VARIABLES  (N = 2)

    n = 2
    x0(1) = 1.d0
    x0(2) = 0.d0
    vmin = -10.d0
    vmax = 10.d0
    GO TO 410

  CASE (5)
    !    5  A FUNCTION WITH A SINGLE ROW OF LOCAL MINIMA  (N = 2)

    n = 2
    x0(1) = -3.d0
    x0(2) = 0.d0
    xmin(1) = -15.d0
    xmax(1) = 25.d0
    xmin(2) = -5.d0
    xmax(2) = 15.d0
    RETURN

  CASE (6)
    !    6  SIX-HUMP CAMEL FUNCTION  (N = 2)

    n = 2
    v0 = 0.d0
    xmin(1) = -3.d0
    xmax(1) = 3.d0
    xmin(2) = -2.d0
    xmax(2) = 2.d0
    GO TO 450

  CASE (7:9)
    !    7  TWO-DIMENSIONAL PENALIZED SHUBERT FUNCTION, BETA = 0  (N = 2)

    !    8  TWO-DIMENSIONAL PENALIZED SHUBERT FUNCTION, BETA = 1/2  (N = 2)

    !    9  TWO-DIMENSIONAL PENALIZED SHUBERT FUNCTION, BETA = 1  (N = 2)

    n = 2
    v0 = 0.d0
    vmin = -10.d0
    vmax = 10.d0
    GO TO 430

  CASE (10:15)
    !   10  A FUNCTION WITH THREE ILL-CONDITIONED MINIMA, A=10  (N = 2)

    !   11  A FUNCTION WITH THREE ILL-CONDITIONED MINIMA, A=10**2  (N = 2)

    !   12  A FUNCTION WITH THREE ILL-CONDITIONED MINIMA, A=10**3  (N = 2)

    !   13  A FUNCTION WITH THREE ILL-CONDITIONED MINIMA, A=10**4  (N = 2)

    !   14  A FUNCTION WITH THREE ILL-CONDITIONED MINIMA, A=10**5  (N = 2)

    !   15  A FUNCTION WITH THREE ILL-CONDITIONED MINIMA, A=10**6  (N = 2)

    n = 2
    x0(1) = 0.d0
    x0(2) = 0.d0
    xmin(1) = -10.d0
    xmax(1) = 10.d0
    xmin(2) = -100.d0
    xmax(2) = 100.d0
    RETURN

  CASE (16)
    !   16  GOLDSTEIN-PRICE FUNCTION  (N = 2)

    n = 2
    v0 = 1.d0
    vmin = -2.d0
    vmax = 2.d0
    GO TO 430

  CASE (17)
    !   17  PENALIZED BRANIN FUNCTION  (N = 2)

    n = 2
    x0(1) = 2.5D0
    x0(2) = 7.5D0
    xmin(1) = -5.d0
    xmax(1) = 10.d0
    xmin(2) = 0.d0
    xmax(2) = 15.d0
    RETURN

  CASE (18:20)
    !   18  PENALIZED SHEKEL FUNCTION, M = 5  (N = 4)

    !   19  PENALIZED SHEKEL FUNCTION, M = 7  (N = 4)

    !   20  PENALIZED SHEKEL FUNCTION, M = 10  (N = 4)

    n = 4
    v0 = 9.d0
    vmin = 0.d0
    vmax = 10.d0
    GO TO 430

  CASE (21)
    !   21  PENALIZED THREE-DIMENSIONAL HARTMAN FUNCTION  (N = 3)

    n = 3
    GO TO 230

  CASE (22)
    !   22  PENALIZED SIX-DIMENSIONAL HARTMAN FUNCTION  (N = 6)

    n = 6
    GO TO 230

  CASE (23)
    !   23  PENALIZED LEVY-MONTALVO FUNCTION, TYPE 1  (N = 2)

    n = 2
    GO TO 330

  CASE (24)
    !   24  PENALIZED LEVY-MONTALVO FUNCTION, TYPE 1  (N = 3)

    n = 3
    GO TO 330

  CASE (25)
    !   25  PENALIZED LEVY-MONTALVO FUNCTION, TYPE 1  (N = 4)

    n = 4
    GO TO 330

  CASE (26)
    !   26  PENALIZED LEVY-MONTALVO FUNCTION, TYPE 2  (N = 5)

    n = 5
    GO TO 330

  CASE (27)
    !   27  PENALIZED LEVY-MONTALVO FUNCTION, TYPE 2  (N = 8)

    n = 8
    GO TO 330

  CASE (28)
    !   28  PENALIZED LEVY-MONTALVO FUNCTION, TYPE 2  (N = 10)

    n = 10
    GO TO 330

  CASE (29)
    !   29  PENALIZED LEVY-MONTALVO FUNCTION, TYPE 3, RANGE = 10   (N = 2)

    n = 2
    GO TO 330

  CASE (30)
    !   30  PENALIZED LEVY-MONTALVO FUNCTION, TYPE 3, RANGE = 10   (N = 3)

    n = 3
    GO TO 330

  CASE (31)
    !   31  PENALIZED LEVY-MONTALVO FUNCTION, TYPE 3, RANGE = 10   (N = 4)

    n = 4
    GO TO 330

  CASE (32)
    !   32  PENALIZED LEVY-MONTALVO FUNCTION, TYPE 3, RANGE = 5   (N = 5)

    n = 5
    GO TO 370

  CASE (33)
    !   33  PENALIZED LEVY-MONTALVO FUNCTION, TYPE 3, RANGE = 5   (N = 6)

    n = 6
    GO TO 370

  CASE (34)
    !   34  PENALIZED LEVY-MONTALVO FUNCTION, TYPE 3, RANGE = 5  (N = 7)

    n = 7
    GO TO 370

  CASE (35)
    !   35  A FUNCTION WITH A SINGLE CUSP-SHAPED MINIMUM  (N = 5)

    n = 5
    v0 = 1000.d0
    vmin = -20000.d0
    vmax = 10000.d0
    GO TO 430

  CASE (36)
    !   36  A FUNCTION WITH A SMALL-ATTRACTION-REGION GLOBAL MINIMUM (N = 2)

    n = 2
    x0(1) = 0.d0
    x0(2) = 100.d0
    vmin = -1000.d0
    vmax = 1000.d0
    GO TO 410

  CASE (37)
    !   37  A FUNCTION WITH A SMALL-ATTRACTION-REGION GLOBAL MINIMUM (N = 5)

    n = 5
    x0(1) = 0.d0
    x0(2) = 0.d0
    x0(3) = 0.d0
    x0(4) = 0.d0
    x0(5) = 10.d0
    vmin = -100.d0
    vmax = 100.d0
    GO TO 410

END SELECT

230 v0 = 0.5D0
vmin = 0.d0
vmax = 1.d0
GO TO 430

330 v0 = 0.d0
vmin = -10.d0
vmax = 10.d0
GO TO 430

370 v0 = 0.d0
vmin = -5.d0
vmax = 5.d0
GO TO 430

410 DO  i = 1, n
  xmin(i) = vmin
  xmax(i) = vmax
END DO
RETURN

430 DO  i = 1, n
  xmin(i) = vmin
  xmax(i) = vmax
END DO

450 x0(1:n) = v0

RETURN
END SUBROUTINE glomip



SUBROUTINE ptseg(n,xpfmin,fpfmin,fpfmax,kp,nfev)

!  SAMPLE VERSION OF THE OUTPUT SUBROUTINE  PTSEG
!  (PERFORMS END-OF-SEGMENT OUTPUT)
!  WHICH MUST BY SUPPLIED BY THE USER AND WHICH IS DESCRIBED IN
!  DETAIL WITHIN THE SUBROUTINE  SIGMA.
use mod_kinds, only : dp
IMPLICIT NONE


INTEGER, INTENT(IN)    :: n
REAL (dp), INTENT(IN)  :: xpfmin(:)
REAL (dp), INTENT(IN)  :: fpfmin
REAL (dp), INTENT(IN)  :: fpfmax
INTEGER, INTENT(IN)    :: kp
INTEGER, INTENT(IN)    :: nfev

WRITE (6,5000) kp, nfev, fpfmin, fpfmax
WRITE (6,5100) xpfmin

RETURN

5000 FORMAT (' OBSERVATION PERIOD  KP= ', i4,  &
             ',   FUNCTION EVALUATIONS  NFEV= ', i7/  &
             '  FINAL BEST, WORST FUNCT. VALUES  FPFMIN= ', g13.5,  &
             ',  FPFMAX= ', g13.5)
5100 FORMAT ('  BEST FINAL POINT  XPFMIN'/(' ', 6G13.5))
END SUBROUTINE ptseg



SUBROUTINE ptrial(n,xopt,fopt,ftfmin,ftfmax,ftfopt,istop,istopt,nfev,kp,iprint)

!  SAMPLE VERSION OF THE OUTPUT SUBROUTINE  PTRIAL
!  (PERFORMS END-OF-TRIAL OUTPUT)
!  WHICH MUST BY SUPPLIED BY THE USER AND WHICH IS DESCRIBED IN
!  DETAIL WITHIN THE SUBROUTINE  SIGMA.
use mod_kinds, only : dp
IMPLICIT NONE


INTEGER, INTENT(IN)    :: n
REAL (dp), INTENT(IN)  :: xopt(:)
REAL (dp), INTENT(IN)  :: fopt
REAL (dp), INTENT(IN)  :: ftfmin
REAL (dp), INTENT(IN)  :: ftfmax
REAL (dp), INTENT(IN)  :: ftfopt
INTEGER, INTENT(IN)    :: istop
INTEGER, INTENT(IN)    :: istopt
INTEGER, INTENT(IN)    :: nfev
INTEGER, INTENT(IN)    :: kp
INTEGER, INTENT(IN)    :: iprint

WRITE (6,5000)

IF (iprint == 0) WRITE (6,5100) kp, nfev, ftfmin, ftfmax
WRITE (6,5200) istop, istopt, ftfopt, fopt
WRITE (6,5300) xopt(1:n)

RETURN
5000 FORMAT (//' END OF A TRIAL ')
5100 FORMAT ('  OBSERVATION PERIOD  KP= ', i4,  &
             ',   FUNCTION EVALUATIONS  NFEV= ', i7/  &
             '  FINAL BEST, WORST FUNCT. VALUES  FTFMIN= ', g13.5,  &
             ',  FTFMAX= ', g13.5)
5200 FORMAT (/'  TRIAL STOP INDICATOR  ISTOP= ', i2,  &
             ',   PAST STOPS INDICATOR  ISTOPT= ', i2/  &
             '  END-OF-TRIAL BEST FUNCTION VALUE   FTFOPT= ', g14.6/  &
             '  BEST CURRENT MINIMUM FUNCTION VALUE  FOPT= ', g14.6)
5300 FORMAT ('  BEST CURRENT MINIMIZER   XOPT'/(' ', 6G13.5))
END SUBROUTINE ptrial



SUBROUTINE ptksuc(ksuc)

!  SAMPLE VERSION OF THE OUTPUT SUBROUTINE  PTKSUC
!  (PERFORMS END-OF-TRIAL OUTPUT RELATED TO THE COUNT OF
!    SUCCESSFUL TRIALS)
!  WHICH MUST BY SUPPLIED BY THE USER AND WHICH IS DESCRIBED IN
!  DETAIL WITHIN THE SUBROUTINE  SIGMA.

IMPLICIT NONE

INTEGER, INTENT(IN)    :: ksuc

WRITE (6,5000) ksuc, ksuc

RETURN
5000 FORMAT (// ' THE CURRENT COUNT  KSUC  OF SUCCESSFUL TRIALS HAS ',  &
             'REACHED FOR THE FIRST'/  &
             '  TIME THE VALUE ', i2,  &
             ' (IF THE REQUESTED COUNT  NSUC  OF SUCCESSFUL TRIALS '/  &
             '  HAD BEEN GIVEN THE VALUE ', i2, ' THE ALGORITHM WOULD ',  &
             'HAVE STOPPED HERE)'//)
END SUBROUTINE ptksuc

#endif
