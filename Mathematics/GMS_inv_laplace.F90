MODULE invert_Laplace

!    ALGORITHM 796, COLLECTED ALGORITHMS FROM ACM.
!    THIS WORK PUBLISHED IN TRANSACTIONS ON MATHEMATICAL SOFTWARE,
!    VOL. 25,NO. 3, September, 1999, P.  306--315.
use mod_kinds, only : i4,dp
IMPLICIT NONE
!INTEGER(kind=i4), PARAMETER  :: dp = SELECTED_REAL_KIND(12, 60)

!     .. Common blocks ..
! COMMON /param/first, ma, mp
! COMMON /param1/gamma, pi

INTEGER(kind=i4), SAVE    :: ma, mp
LOGICAL, SAVE    :: first
REAL (dp), SAVE  :: pi, gamma


CONTAINS


SUBROUTINE invltf(tol, valt, fz, sigma0, ssbar, nmax, fzinv, error,  &
                  ifzeval, ifail)
      !dir$ attribute code_align : 32 :: invltf
      !dir$ optimize : 3
      !dir$ attribute optimization_parameter: target_arch=AVX :: invltf

! N.B. Argument WORK has been removed.
 
! Code converted using TO_F90 by Alan Miller
! Date: 2000-02-12  Time: 17:34:40

! *********************************************************************

!  PURPOSE:
!  =======
!  THIS ROUTINE COMPUTES AN APPROXIMATE VALUE OF AN INVERSE LAPLACE TRANSFORM,
!       BY MEANS OF AN EXPANSION IN SINE AND COSINE FOURIER SERIES.
!       THE METHOD WHICH THIS SOFTWARE IS BASED ON UTILIZES THE Q-D ALGORITHM
!       TO ACCELERATE THE CONVERGENCE OF THE SERIES.   THE SUMMATION OF
!       SUCCESSIVE TERMS IS TRUNCATED WHEN THE ESTIMATED TRUNCATION ERROR
!       IS LESS OR EQUAL THAN AN INPUT PROVIDED TOLERANCE. THE DISCRETIZATION
!       ERROR IS MADE LESS OR EQUAL THAN THIS TOLERANCE BY SUITABLY CHOOSING
!       SOME METHOD'S PARAMETERS.


! THE CALLING SEQUENCE IS
! =======================

!  CALL INVLTF(TOL, VALT, FZ, SIGMA0, SSBAR, NMAX, FZINV, ERROR, IFZEVAL, IFAIL)


! INPUT PARAMETERS
! ================


! TOL: REAL (dp).
!                       ON ENTRY TOL CONTAINS THE RELATIVE REQUIRED ACCURACY.

! VALT: REAL (dp).
!                       ON ENTRY, VALT CONTAINS A POSITIVE VALUE OF T
!                       FOR WHICH THE INVERSE LAPLACE TRANSFORM IS REQUIRED.
!                       VALT  HAS TO BE GREATER THAN ZERO.

! FZ:  COMPLEX*16 (REAL (dp) COMPLEX).
!                       NAME OF THE FUNCTION SUBPROGRAMS FOR THE COMPLEX
!                       VALUED LAPLACE TRANSFORM TO BE INVERTED.
!                       ITS SPECIFICATION IS:
!                       COMPLEX*16 FUNCTION FZ(Z)
!                       COMPLEX*16 Z

!                       Z: COMPLEX*16. ON ENTRY, Z MUST SPECIFY THE POINT AT
!                       WHICH THE LAPLACE TRANSFORM FUNCTION VALUE IS REQUIRED.

!                       FZ MUST BE DECLARED AS EXTERNAL IN THE PROGRAM
!                       FROM WHICH INVLTF IS CALLED.

! SIGMA0: REAL (dp).
!                       ON ENTRY, SIGMA0 CONTAINS THE VALUE OF THE ABSCISSA
!                       OF CONVERGENCE OF THE LAPLACE TRANSFORM FUNCTION TO
!                       BE INVERTED OR AN UPPER BOUND OF THIS.
!                       IT IS RECOMMENDED THAT A CORRECT VALUE TO SIGMA0 OR A
!                       CORRECT UPPER BOUND TO SIGMA0 IS PROVIDED.
!                       IF AN INCORRECT VALUE IS USED THE ROUTINE APPEARS TO
!                       WORK WELL BUT CONVERGES TO COMPLETELY WRONG RESULTS.
!                       THERE IS NO WAY IN WHICH THE ROUNTINE CAN DETECT THIS.

! SSBAR:  REAL (dp).
!                       ON ENTRY, IT SPECIFIES THE VALUE OF THE PARAMETER SS
!                       (> 2) TO BE USED IN CALCULATING THE PARAMETER D*T.
!                       TO OBTAIN DEFAULT OPTION (SS=4.1d0) ONE
!                       MAY SET   SSBAR = 0.

! NMAX: INTEGER(kind=i4).
!                       ON ENTRY, NMAX SPECIFIES THE MAXIMUM
!                       NUMBER OF FZ EVALUATIONS ALLOWED.


! OUTPUT PARAMETERS
! =================

! FZINV: REAL (dp).
!                       ON EXIT, FZINV CONTAINS  THE APPROXIMATION
!                       OF THE INVERSE LAPLACE TRANSFORM  AT THE POINT VALT.

! ERROR: REAL (dp) ARRAY OF DIMENSION 3.
!                      ON EXIT, ERROR(1) CONTAINS AN ESTIMATE OF THE RELATIVE ERROR
!                      WHICH SHOULD BE AN  UPPER BOUND FOR

!                      ABS(TRUEVALUE AT VALT - FZINV)/ABS(TRUEVALUE AT VALT)

!                      ON EXIT, ERROR(2) CONTAINS AN ESTIMATE OF THE ABSOLUTE ERROR
!                      WHICH SHOULD BE AN UPPER BOUND FOR

!                      ABS(TRUEVALUE AT VALT - FZINV)

!                      ON EXIT, ERROR(3) CONTAINS AN ESTIMATE OF THE TRUNCATION ERROR
!                      MADE IN  CALCULATING FZINV.

! IFZEVAL: INTEGER(kind=i4).
!                      ON EXIT, IFZEVAL CONTAINS THE
!                      NUMBER OF EVALUATIONS OF FZ USED TO OBTAIN FZINV.

! IFAIL: INTEGER(kind=i4).
!                      ON EXIT, A DIAGNOSTIC.
!                      = 0   ALL INPUTS WITHIN LIMITS.
!                            ALGORITHM APPERENTLY WORKED PROPERLY.
!                      =  1  TOL IS EQUAL OR GREATER THAN 1.
!                      =  2  VALT NOT POSITIVE.
!                      = -1  ACCURACY NOT REACHED AFTER NMAX FUNCTION EVALUATIONS.
!                      = -2  ALGORITHM DID NOT APPEAR TO BE CONVERGING, POSSIBLY
!                            DUE TO AN UNSUITABLE VALUE OF PARAMETER SSBAR.

!  WORK : COMPLEX (dp)  ARRAY OF DIMENSION(2,0:2*NMAX). WORKSPACE AREA.


! SUBROUTINES OR FUNCTIONS NEEDED

!   FZ      : USER PROVIDED FUNCTION
!   QDACC   : Q-D ALGORITHM
!   BACKCF  : COMPUTATION OF THE CONTINUED FRACTION
!   D1MACH  : PROVIDES MACHINE EPSILON

! ***************************************************************************

!     .. Scalar Arguments ..

REAL (dp), INTENT(OUT)  :: tol
REAL (dp), INTENT(IN)   :: valt
REAL (dp), INTENT(IN)   :: sigma0
REAL (dp), INTENT(IN)   :: ssbar
INTEGER(kind=i4), INTENT(IN)     :: nmax
REAL (dp), INTENT(OUT)  :: fzinv
REAL (dp), INTENT(OUT)  :: error(3)
INTEGER(kind=i4), INTENT(OUT)    :: ifzeval
INTEGER(kind=i4), INTENT(OUT)    :: ifail

INTERFACE
  FUNCTION fz(z) RESULT(fn_val)
    import :: dp
    IMPLICIT NONE
    !INTEGER(kind=i4), PARAMETER        :: dp = SELECTED_REAL_KIND(12, 60)
    COMPLEX (dp), INTENT(IN)  :: z
    COMPLEX (dp)              :: fn_val
  END FUNCTION fz
END INTERFACE

REAL (dp) :: ss

!     ..
!     .. Array Arguments ..
COMPLEX (dp) :: work(2, 0:2*nmax)
!dir$ attribute align : 64 :: work

!     .. Function Arguments ..
! COMPLEX (dp) fz
! EXTERNAL fz
!     ..
!     .. Local Scalars ..
REAL (dp) :: alpha, dt, err1, err2, fa3, part2, part0, part1, relp, t,  &
             tol1, toll
REAL (dp), parameter :: ln10 = 0.9999999999999999502037_dp
REAL (dp), parameter :: piv  = 3.1415926535897932384626_dp
REAL (dp), parameter :: hpi  = 1.5707963267948966192313_dp
REAL (dp), parameter :: qpi  = 0.7853981633974483096157_dp
!     ..
!     .. External Subroutines ..
! EXTERNAL qdacc,backcf,d1mach
!     ..
!     .. Intrinsic Functions ..
! INTRINSIC ABS,DATAN,DLOG
!     ..

error(1) = 0.0_dp
error(2) = 0.0_dp
error(3) = 0.0_dp

ss = ssbar + 4.1_dp
! ****************************************************************************

!  INPUT DATA CHECK

! ****************************************************************************
ifail = 0

IF (tol >= 1) THEN
  ifail = 1
  RETURN
  
END IF
IF (valt < 0) THEN
  ifail = 2
  RETURN
  
END IF


! ************************************************************************
!  SETTING THE PARAMETERS : ALPHA, TOL1, RELP
!  THE PRODUCT D*T IS COMPUTED IN SUCH A WAY THAT
!  THE ERROR IS LESS THAN TOL
! ************************************************************************

relp = EPSILON(0.0_dp)
!     RELP = X02AJF()
!     RELP  =0.222044d-15
!     FOR AN IBM 6000 RISC STATION MOD. 32H
IF (tol < relp) tol = relp
tol1 = tol/10._dp
alpha = 1.4_dp

dt = -LOG(tol1*hpi)/ (ss - ln10)

dt = dt*alpha
gamma = dt/valt + sigma0
t = ss*0.5_dp*valt
ifzeval = 0
pi = piv/t
first = .true.
ma = 2
mp = ma - 1

! ***********************************************************************

!    CALL TO THE Q-D ALGORITHM

! ***********************************************************************

10 IF (ifail /= 0) RETURN
CALL qdacc(fz, ifzeval, ifail, work, part0, part1, part2)
IF (ifail /= 0) RETURN
CALL backcf(valt, work, part2)


!**********************************************************************

!     FZINV IS THE APPROXIMATED VALUE OF THE INVERSE LAPLACE TRANSFORM

! *********************************************************************
fa3 = EXP(dt)/t
fzinv = EXP(sigma0*valt)*fa3*part2


! *********************************************************************

!    COMPUTATION OF THE TRUNCATION ERROR

! ERR1 IS THE DIFFERENCE BETWEEN THE LAST APPROXIMATION AND THE PREVIOUS ONE,
! WHILE ERR2 IS THE DIFFERENCE BETWEEN THE LAST APPROXIMATION
! AND THE LAST-THIRD

! ***************************************************************************

err1 = fa3*ABS(part2 - part1)
err2 = fa3*ABS(part2 - part0)
error(2) = tol1*ABS(fa3*part2)
toll = tol1*ABS(fa3*part2) + relp*ABS(fa3*part2)
IF (toll == 0.0_dp) toll = relp

! ****************************************************************************

!               STOPPING CRITERION

!     THE ALGORITHM STOPS WHEN BOTH ERR1 AND ERR2 ARE LESS THAN RELP
!                         OR
!     THE NUMBER OF FUNCTION EVALUATIONS, OF COURSE, IS >= NMAX

! *****************************************************************************


IF (ifzeval < nmax) THEN
  IF (err1 /= 0.0_dp .AND. err2 /= 0.0_dp) THEN
    IF (err1 > relp .AND. err2 > relp) THEN
      IF (err1 > toll .OR. err2 > toll) GO TO 10
    END IF
    
  ELSE IF (err1 == 0.0_dp .AND. err2 /= 0.0_dp) THEN
    IF (err2 > relp .OR. err2 > toll) THEN
      GO TO 10
      
    ELSE IF (err2 == 0.0_dp .AND. err1 /= 0.0_dp) THEN
      IF (err1 > relp .OR. err1 > toll) GO TO 10
    END IF
    
  END IF
  
ELSE
  IF (err1 /= 0.0_dp .AND. err2 /= 0.0_dp) THEN
    IF (err1 > relp .AND. err2 > relp) THEN
      IF (err1 > toll .OR. err2 > toll) THEN
        ifail = - 1
      END IF
      
    END IF
    
  ELSE IF (err1 == 0.0_dp .AND. err2 /= 0.0_dp) THEN
    IF (err2 > relp .OR. err2 > toll) THEN
      ifail = -1
      
    ELSE IF (err2 == 0.0_dp .AND. err1 /= 0.0_dp) THEN
      IF (err1 > relp .OR. err1 > toll) THEN
        ifail = -1
      END IF
      
    END IF
    
  END IF
  
END IF

! ****************************************************************************

!      ESTIMATION OF THE ABSOLUTE ERROR, TRUNCATION ERROR AND THE SUM OF THEM

! ****************************************************************************

error(2) = error(2) + (err1+err2)
IF (part2 /= 0.0_dp) THEN
  error(3) = (err1 + err2) / ABS(fa3*part2)
  
ELSE
  error(3) = err1 + err2
END IF

error(1) = tol1 + error(3)
RETURN

END SUBROUTINE invltf


! ****************************************************************

SUBROUTINE qdacc(fz, ifv, ifail, work, part0, part1, part2)
      !dir$ attribute forceinline :: qdacc
      !dir$ attribute code_align : 32 :: qdacc
      !dir$ optimize : 3
      !dir$ attribute optimization_parameter: target_arch=skylake_avx512 :: qdacc

! N.B. Argument NMAX has been removed.

! ****************************************************************
!    PURPOSE:
!    =======
!    THIS SUBROUTINE IMPLEMENTS A COLUMN-VERSION OF THE Q-D ALGORITHM.
!    THE QD SCHEME IS BUILT UP PROCEEDING FROM LEFT TO RIGHT ALONG THE
!    DIAGONAL DIRECTION. THE ELEMENTS OF EACH DIAGONALS ARE COMPUTED
!    PROCEEDING BOTTOM-UP IN THE TABLE. THE LAST ELEMENT COMPUTED IN
!    EACH DIAGONAL IS THE COEFFICIENT OF THE CONTINUED FRACTION.

!    DESCRIPTION OF PARAMETERS:
!    =========================

!    INPUT PARAMETERS:

!   FZ:  COMPLEX*16 (REAL (dp) COMPLEX) FUNCTION, SUPPLIED BY THE USER.
!        FZ MUST RETURN THE VALUE OF THE LAPLACE TRANSFORM FUNCTION TO
!        BE INVERTED, AT A GIVEN POINT. ITS SPECIFICATION IS:
!                  COMPLEX*16 FUNCTION FZ(Z)
!                  COMPLEX*16 Z

!                  Z: COMPLEX*16. ON ENTRY, Z MUST SPECIFY THE POINT AT
!                     WHICH THE LAPLACE TRANSFORM FUNCTION VALUE IS REQUIRED.

!        FZ MUST BE DECLARED AS EXTERNAL IN THE PROGRAM FROM WHICH INVLTF IS
!        CALLED.

!   IFV  : INTEGER(kind=i4). ON EXIT, IFV CONTAINS THE
!          NUMBER OF EVALUATIONS OF FZ USED TO OBTAIN PART2.

!   IFAIL: INTEGER(kind=i4). ON EXIT, IFAIL CONTAINS POSSIBLE ERRORS DETECTED

!   WORK : COMPLEX (dp) ARRAY OF DIMENSION(2,0:2*NMAX).  WORKSPACE AREAS.

!   PART0: REAL (dp). ON EXIT PART0 CONTAINS THE APPROXIMATION
!          OF THE LAST-THIRD ACCELERATED TERM.

!   PART1: REAL (dp).  ON EXIT PART1 CONTAINS THE APPROXIMATION
!          OF THE LAST-SECOND ACCELERATED TERM.

!   PART2: REAL (dp). ON EXIT PART2 CONTAINS THE APPROXIMATION
!          OF THE LAST ACCELERATED.
! ****************************************************************
!     .. Scalar Arguments ..

INTEGER(kind=i4), INTENT(IN OUT)       :: ifv
INTEGER(kind=i4), INTENT(OUT)          :: ifail
COMPLEX (dp), INTENT(IN OUT)  :: work(:, 0:)
REAL (dp), INTENT(OUT)        :: part0
REAL (dp), INTENT(IN OUT)     :: part1
REAL (dp), INTENT(IN)         :: part2

INTERFACE
  FUNCTION fz(z) RESULT(fn_val)
    import :: dp
    IMPLICIT NONE
 !   INTEGER(kind=i4), PARAMETER        :: dp = SELECTED_REAL_KIND(12, 60)
    COMPLEX (dp), INTENT(IN)  :: z
    COMPLEX (dp)              :: fn_val
  END FUNCTION fz
END INTERFACE

!     ..
!     .. Function Arguments ..
! COMPLEX (dp) fz
! EXTERNAL fz
!     ..
!     .. Local Scalars ..
COMPLEX (dp)        :: aux1, aux2, aux3, h
INTEGER(kind=i4)             :: i, j, k, mm, ul
LOGICAL             :: riv
COMPLEX (dp), SAVE  :: mx
INTEGER(kind=i4), SAVE       :: ibegin
!     ..
!     .. Intrinsic Functions ..
! INTRINSIC ABS, DCMPLX, EXP, dreal, CEXP, SQRT
!     ..

! **********************************************************
!   INITIALIZATION STEP
! **********************************************************
ibegin = 2*mp
mm = 2*ma
IF (first) THEN
  h = fz(CMPLX(gamma, 0.0_dp, KIND=dp)) / (2.0_dp,0.0_dp)
  ifv = ifv + 1
  mx = h
  work(2,0) = h

! **************************************************************
!   INITIALIZE THE WORKSPACE AREA: THE ARRAYS Q and D OF
!   THE COLUMN VERSION OF THE QD ALGORITHM
!   THE VECTOR Q HAS BEEN STORED IN THE FIRST ROW OF THE ARRAY WORK
!   THE VECTOR D HAS BEEN STORED IN THE SECOND ROW OF THE ARRAY WORK
! ***************************************************************
  !dir$ assume_aligned work:64
  DO  k = 0,1
    work(1,k) = mx
    mx = fz(CMPLX(gamma, pi*(k+1), KIND=dp))
    ifv = ifv + 1
    work(1,k) = mx / work(1,k)
    work(2,k+1) =  work(1,k)
  END DO
  aux1 = work(1,1)
  work(1,1) =  work(1,1) - work(1,0)
  work(2,2) = - work(1,1)
  work(1,0) = aux1
  work(2,1) = -work(2,1)
  work(1,2) = 0.0_dp
END IF

ul = mm - 1
!     DO 15 K = 3, UL
!       WORK(1,K) = 0.0D0
!15    CONTINUE

! *****************************************************************
! COMPUTATION OF THE COEFFICIENTS NEEDED IN THE QD ALGORITHM
! *****************************************************************
ifail = 0
i = ibegin

25 riv = .true.
aux1 = work(1,0)
work(1,0) = mx
mx = fz(CMPLX(gamma, pi*(i+1), KIND=dp))
ifv = ifv + 1
work(1,0) = mx/work(1,0)
aux2 = work(1,1)
work(1,1) = work(1,0) - aux1
j = 2

30 IF (j == i)  work(1,j) = 0.0_dp
aux3 = work(1,j)
IF (riv) THEN
  IF (aux2 /= (0.0_dp,0.0_dp)) THEN
    work(1,j) = aux1*work(1,j-1)/aux2
  ELSE
    ifail = -2
  END IF
  
ELSE
  work(1,j) = work(1,j-1) - aux2 + aux1
END IF

aux1 = aux2
aux2 = aux3
riv = .NOT. riv
j = j + 1
IF(j <= i .AND. ifail == 0) GO TO 30
work(2,i+1) = -work(1,i)
i = i + 1
IF (i <= ul .AND. ifail == 0 ) GO TO 25
IF (.NOT.first) THEN
  ibegin = ibegin + 1
END IF
IF (first) THEN
  part0 = 0.0_dp
  part1 = 0.0_dp
  
ELSE
  part0 = part1
  part1 = part2
END IF
! ***************************************************************************
! ************* END OF THE QD ALGORITHM  *************************************
! ***************************************************************************
RETURN
END SUBROUTINE qdacc


! *****************************************************************************

SUBROUTINE backcf(tv, work, part2)
      !dir$ attribute forceinline :: backcf
      !dir$ attribute code_align : 32 :: backcf
      !dir$ optimize : 3
      !dir$ attribute optimization_parameter: target_arch=skylake_avx512 :: backcf

! N.B. Argument NMAX has been removed.

! *****************************************************************************
!  COMPUTATION OF THE  CONTINUED FRACTION BY THE  BACKWARD FORMULA
! *****************************************************************************

REAL (dp), INTENT(IN)     :: tv
COMPLEX (dp), INTENT(IN)  :: work(:,0:)
REAL (dp), INTENT(OUT)    :: part2

INTEGER(kind=i4)      :: mm, i
COMPLEX (dp) :: cc, z, h2m, r2m

mm = 2*ma
z = EXP(CMPLX(0.0_dp, pi*tv, KIND=dp))
h2m = ((work(2,mm-1) - work(2,mm))*z + (1.0_dp,0.d0))/ (2.0_dp,0.0_dp)
r2m = SQRT((work(2,mm)*z/ (h2m*h2m)) + (1.0_dp,0.0_dp))
r2m = (1.0_dp,0.0_dp) - r2m
r2m = -h2m*r2m
cc = r2m*z
!dir$ assume_aligned work:64
DO  i = mm - 1,1,-1
  cc = (work(2,i)*z)/ ((1.d0,0.d0) + cc)
END DO
cc = work(2,0)/ ((1.d0,0.d0) + cc)
part2 = REAL(cc, KIND=dp)
mp = ma
ma = ma + 1
first = .false.

RETURN
END SUBROUTINE backcf


END MODULE invert_Laplace

!============================================
!   Driver implementation added
!============================================
#if 0


!   DRIVER PROGRAM FOR TESTING SOFTWARE INVLTF  IMPLEMENTING
 
! Code converted using TO_F90 by Alan Miller
! Date: 2000-02-12  Time: 17:34:52

!   FOURIER BASED METHOD FOR THE NUMERICAL INVERSION

!                  OF LAPLACE TRANSFORMS


!                     JANUARY , 1998


!   AUTHORS: D'AMORE LUISA, GIULIANO LACCETTI, ALMERICO MURLI



!   REFERENCES
!   ==========

!   D'AMORE L., LACCETTI G., MURLI A.,  -    "ALGORITHM XXX: A FORTRAN
!      SOFTWARE PACKAGE FOR THE NUMERICAL INVERSION OF THE
!      LAPLACE TRANSFORM BASE ON FOURIER SERIES' METHOD"


PROGRAM main

!   ***********************************************************
!   DRIVER  PROGRAM TO TEST  THE ROUTINE INVLTF
!   FOR THE INVERSION OF A LAPLACE TRANSFORM FUNCTION.
!   THIS VERSION USES BOTH REAL AND COMPLEX DOUBLE PRECISION OPERATIONS.
!   THE MAIN PROGRAM ALLOWS THE INVERSION OF A SET OF LAPLACE TRANSFORM
!   FUNCTIONS WHICH CAN BE ADDRESSED  BY A NATURAL NUMBER  BETWEEN 1 AND 34.
!   FOR THE COMPLETE LIST SEE THE TABLE IN THE COMPANION PAPER.

!   THIS IS A SELF-CONTAINED DRIVER FOR THE INVLTF ROUTINE
!   COMPRISING A MAIN PROGRAM AND SUBPROGRAMS QDACC, BACKCF,
!   AND THE LAPLACE TRANSFORM PAIRS FZ AND FEX

!   THIS DRIVER ALLOWS TO OBTAIN UP TO 50 VALUES OF THE
!   INVERSE LAPLACE FUNCTION

!   ************************************************************

USE invert_Laplace
USE common_nf
IMPLICIT NONE

!     .. Parameters ..
INTEGER, PARAMETER  :: nmax = 550, fmax = 50, lastfn = 34
!     ..
!     .. Local Scalars ..
REAL (dp)           :: exf, sigma0, tol, ssbar
INTEGER             :: i, nt
CHARACTER (LEN=77)  :: aa, bb
LOGICAL             :: STOP
!     ..
!     .. Local Arrays ..
REAL (dp)    :: difabs(fmax), difrel(fmax), fzinv(fmax),  &
                tarray(fmax), error(3,fmax), ERR(3)
INTEGER      :: ifail(fmax), ifzeval(fmax)
!     ..
!     .. External Functions ..
! COMPLEX (dp) fz
! REAL (dp) :: fex
! EXTERNAL fz, fex

INTERFACE
  FUNCTION fz(z) RESULT(fn_val)
    IMPLICIT NONE
    INTEGER, PARAMETER        :: dp = SELECTED_REAL_KIND(12, 60)
    COMPLEX (dp), INTENT(IN)  :: z
    COMPLEX (dp)              :: fn_val
  END FUNCTION fz

  FUNCTION fex(x) RESULT(fn_val)
    IMPLICIT NONE
    INTEGER, PARAMETER     :: dp = SELECTED_REAL_KIND(12, 60)
    REAL (dp), INTENT(IN)  :: x
    REAL (dp)              :: fn_val
  END FUNCTION fex
END INTERFACE

!     ..
!     .. External Subroutines ..
! EXTERNAL invltf
!     ..
!     .. Intrinsic Functions ..
! INTRINSIC ABS
!     ..
!     .. Common blocks ..
! COMMON /nf/nfun
!     ..
!     *****************************************************************
!                   SET UP THE OUTPUT
!     *****************************************************************


! Loop through all the tests

DO  nfun = 1,lastfn
  
  
! Skip the tests that require the use of NAG library routines
  
! Commented out lines need to be reintroduced in the routines
! FZ and FEX after labels 9, 36, 37, 38
  
  IF (nfun == 9 .OR. nfun == 36 .OR. nfun == 37 .OR. nfun == 38) CYCLE
  aa = '*****************************************************************************'
  bb = '<><><><><><><><><><><><><><><><><><><><><><><><><><><><><>'
  WRITE (*,FMT=9000) aa, aa, aa, aa
  WRITE (*,FMT=9090)
!     READ (*,FMT=*) NFUN
  WRITE (*,FMT=9050) nfun
  WRITE (*,FMT=9110)
!     READ (*,FMT=*) SIGMA0
  sigma0 = 0.0
  WRITE (*,FMT=9120) sigma0
  IF (nfun == 18) sigma0 = 3.0
  IF (nfun == 23) sigma0 = 0.25D0
  IF (nfun == 29) sigma0 = 2.d0
  WRITE (*,FMT=9080) bb, bb, bb, bb

!     *****************************************************************
!          NOW, SET INPUT PARAMETERS FOR INVLTF
!     *****************************************************************
  
!      NT IS THE NUMBER OF T-VALUES USED HERE FOR THE TEST  FUNCTIONS.
!      THE MAXIMUM ALLOWED IS THE DIMENSION OF THE ARRAY TVAL
!     *****************************************************************
  
!     IN THIS TEST PROGRAM THE INVERSE FUNCTION IS REQUESTED IN THE
!     FOLLOWING VALUES OF T:
!     T=1,20, STEP=0.5 AND T=20,65 STEP=5
!     REMARK:
!     ======
!     FOR T SMALL, THAT IS FOR  T=1,20, STEP=0.5, ALL THE RESULTS ARE
!     QUITE ACCURATE, WHILE FOR T LARGE, THAT IS FOR T=20,65 STEP=5,
!     IN SOME CASES THE RESULTS COULDN'T BE ACCURATE. IN SUCH CASES
!     THE AUTHORS SUGGEST TO USE AN ASYMPTOTIC INVERSION METHOD.
!     *****************************************************************
  
  nt = 44
  DO  i = 1,39
    tarray(i) = 1.d0 + (i-1)*0.5D0
  END DO
  tarray(40) = 30.
  DO  i = 41,44
    tarray(i) = 30.d0 + 5.d0*(i-40)
  END DO
  tol = .1D-5
  ssbar = 0.d0
  WRITE (*,FMT=9010) aa, aa
  WRITE (*,FMT=9030) aa, aa
  WRITE (*,FMT=9020) tol
  
! ************************************************************
!                    CALL OF THE ROUTINE INVLTF
! ************************************************************
  
  DO  i=1,nt
    CALL invltf(tol, tarray(i), fz, sigma0, ssbar, nmax, fzinv(i), ERR,  &
                ifzeval(i), ifail(i))
    error(1,i) = ERR(1)
    error(2,i) = ERR(2)
    error(3,i) = ERR(3)
  END DO
  
  
  WRITE (*,FMT=9040) aa, aa
  
  DO  i = 1,nt
    STOP = .false.
    IF (ifail(i) /= 0 ) THEN
      WRITE (*,FMT=9100) i, ifail(i)
      STOP =.true.
    END IF
    
    
    IF (.NOT. STOP) THEN
      
      
      exf = fex(tarray(i))
      difabs(i) = ABS(exf - fzinv(i))
      IF (exf /= 0.d0) THEN
        difrel(i) = difabs(i)/ABS(exf)
        
      ELSE
        difrel(i) = difabs(i)
      END IF
      
      exf = fex(tarray(i))
      IF (ifail(i) /= -2) THEN
        WRITE (*,FMT=9070) tarray(i), fzinv(i), exf, error(1,i), difrel(i), &
                           error(3,i), error(2,i), difabs(i), ifzeval(i),  &
                           ifail(i)
        
      ELSE
        WRITE (*,FMT=9060) tarray(i), ifail(i)
      END IF
    END IF
    
  END DO
END DO

STOP

9000 FORMAT (' ', a45, a45//  &
             t16, '           SUBROUTINE INVLTF             '/  &
             t16, 'NUMERICAL INVERSION OF A LAPLACE TRANSFORM:'/  &
             t16, ' THIS VERSION USES BOTH REAL AND COMPLEX'/  &
             t16, ' REAL (dp) OPERATIONS'// ' ', a45, a45/)
9010 FORMAT (/a45, a45// t17, '        OUTPUT'/)
9020 FORMAT (/' TOLL --> ', e15.7)
9030 FORMAT (' T      : POINT AT WHICH THE INVERSE TRANSFORM IS',  &
             ' COMPUTED;'//  &
             ' FEX    : EXACT VALUE OF THE INVERSE TRANSFORM;'//   &
             ' FCAL   : COMPUTED VALUE OF THE INVERSE TRANSFORM;'//  &
             ' ESTREL : ESTIMATED RELATIVE ERROR;'//   &
             ' RELERR : ACTUAL RELATIVE ERROR;'//   &
             ' ESTABS : ESTIMATED ABSOLUTE ERROR ;'//   &
             ' ABSERR : ACTUAL ABSOLUTE ERROR;'//   &
             ' N      : # OF FUNCTION EVALUATIONS;'//   &
             ' IFAIL  : = 0 NO INPUT ERRORS; SUCCESSFUL RUN',  &
             '    (ACCURACY REACHED AND IFZEVAL<NMAX),'/   &
             t11, '= 1 TOL >= 1,'/   &
             t11, '= 2 VALT LESS THAN ZERO,'/  &
             t11, '= -1 ACCURACY NOT REACHED AND IFZEVAL > NMAX;,'/  &
             t11, '= -2 THE CHOICE FOR SSBAR  MAY BE NOT OPTIMAL;'/  &
             t11, '        IN SUCH A CASE THE USER MAY SLIGHTLY'/  &
             t11, '        CHANGE THE DEFAULT VALUE;'//a45, a45//)
9040 FORMAT (/ a86/  &
             '   T         FCAL         FEX         ESTREL',  &
             '   RELERR  TRUNERR  ESTABS   ABSERR   N  IFAIL'/ a100//)
9050 FORMAT (/ ' TEST FUNCTION ----->  ', i2/)
9060 FORMAT (f5.1, t96, i2)
9070 FORMAT (f5.1, ' ', e14.8, ' ', e14.8, ' ', e8.3, ' ', e8.3, ' ', e8.3, &
             ' ', e8.3, ' ', e8.3,' ', i3, '   ', i2)
9080 FORMAT (//' ',a45,a45//   &
             '     THE T-VALUES AT WHICH THE INVERSE IS REQUIRED ARE T=1,20', &
             ' STEP=0.5 AND T=20,100 STEP=10.'// ' ', a45, a45)
9090 FORMAT (' TEST  FUNCTION : ')
9100 FORMAT (/ ' ERROR DETECTED , I = ', i3, ' IFAIL=', i3)
9110 FORMAT (/ ' ABSCISSA OF CONVERGENCE  ---> '/)
9120 FORMAT (/ ' ABSCISSA OF CONVERGENCE  ---> ', f5.1/)

END PROGRAM main





#endif
