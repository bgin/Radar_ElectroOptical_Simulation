MODULE Legendre_funcs

! ======================================================================
! NIST Guide to Available Math Software.
! Fullsource for module XDLEGF from package CMLIB.
! Retrieved from ARNO on Mon Nov  9 18:15:47 1998.
! Converted to F90 `style' by Alan.Miller @ vic.cmis.csiro.au
! http://www.ozemail.com.au/~milleraj

! Latest revision - 10 November 1998
! ======================================================================
use mod_kinds, only : i4,dp
IMPLICIT NONE


! The following code replaces COMMON blocks XDBLK1 & XDBLK2.
! COMMON block XDBLK3 was unnecessary!

INTEGER, SAVE   :: nbitsf
REAL (dp), SAVE :: radix0, radixl, rad2l, dlg10r ! N.B. radix renamed radix0
INTEGER, SAVE   :: l1, l2, kmax                  ! N.B. l changed to l1


CONTAINS


SUBROUTINE xdlegf(dnu1, nudiff, mu1, mu2, theta, id, pqa, ipqa)
!*** BEGIN PROLOGUE   XDLEGF
!*** DATE WRITTEN   820728   (YYMMDD)
!*** REVISION DATE  871020   (YYMMDD)
!*** CATEGORY NO.  C3a2,C9
!*** KEYWORDS  LEGENDRE FUNCTIONS
!*** AUTHOR  SMITH, JOHN M. (NBS AND GEORGE MASON UNIVERSITY)
!*** PURPOSE  TO COMPUTE THE VALUES OF LEGENDRE FUNCTIONS
!*** DESCRIPTION

!   XDLEGF: Extended-range Double-precision Legendre Functions

! A feature of the XDLEGF subroutine for Legendre functions is the use of
! extended-range arithmetic, a software extension of ordinary floating-point
! arithmetic that greatly increases the exponent range of the representable
! numbers.  This avoids the need for scaling the solutions to lie within the
! exponent range of the most restrictive manufacturer's hardware.
! The increased exponent range is achieved by allocating an integer storage
! location together with each floating-point storage location.

! The interpretation of the pair (X,I) where X is floating-point and I is
! integer is X*(IR**I) where IR is the internal radix of the computer
! arithmetic.

!   This subroutine computes one of the following vectors:

! 1. Legendre function of the first kind of negative order, either
!    a. P(-MU1,NU,X), P(-MU1-1,NU,X), ..., P(-MU2,NU,X) or
!    b. P(-MU,NU1,X), P(-MU,NU1+1,X), ..., P(-MU,NU2,X)
! 2. Legendre function of the second kind, either
!    a. Q(MU1,NU,X), Q(MU1+1,NU,X), ..., Q(MU2,NU,X) or
!    b. Q(MU,NU1,X), Q(MU,NU1+1,X), ..., Q(MU,NU2,X)
! 3. Legendre function of the first kind of positive order, either
!    a. P(MU1,NU,X), P(MU1+1,NU,X), ..., P(MU2,NU,X) or
!    b. P(MU,NU1,X), P(MU,NU1+1,X), ..., P(MU,NU2,X)
! 4. Normalized Legendre polynomials, either
!    a. PN(MU1,NU,X), PN(MU1+1,NU,X), ..., PN(MU2,NU,X) or
!    b. PN(MU,NU1,X), PN(MU,NU1+1,X), ..., PN(MU,NU2,X)

! where X = COS(THETA).

! The input values to XDLEGF are DNU1, NUDIFF, MU1, MU2, THETA and ID.
! These must satisfy

!    DNU1 is REAL (dp) and greater than or equal to -0.5;
!    NUDIFF is INTEGER and non-negative;
!    MU1 is INTEGER and non-negative;
!    MU2 is INTEGER and greater than or equal to MU1;
!    THETA is REAL (dp) and in the half-open interval (0,PI/2];
!    ID is INTEGER and equal to 1, 2, 3 or 4;

! and  additionally either NUDIFF = 0 or MU2 = MU1.

! If ID=1 and NUDIFF=0, a vector of type 1a above is computed
! with NU=DNU1.

! If ID=1 and MU1=MU2, a vector of type 1b above is computed
! with NU1=DNU1, NU2=DNU1+NUDIFF and MU=MU1.

! If ID=2 and NUDIFF=0, a vector of type 2a above is computed
! with NU=DNU1.

! If ID=2 and MU1=MU2, a vector of type 2b above is computed
! with NU1=DNU1, NU2=DNU1+NUDIFF and MU=MU1.

! If ID=3 and NUDIFF=0, a vector of type 3a above is computed
! with NU=DNU1.

! If ID=3 and MU1=MU2, a vector of type 3b above is computed
! with NU1=DNU1, NU2=DNU1+NUDIFF and MU=MU1.

! If ID=4 and NUDIFF=0, a vector of type 4a above is computed
! with NU=DNU1.

! If ID=4 and MU1=MU2, a vector of type 4b above is computed
! with NU1=DNU1, NU2=DNU1+NUDIFF and MU=MU1.

! In each case the vector of computed Legendre function values is returned in
! the extended-range vector (PQA(I),IPQA(I)).  The length of this vector is
! either MU2-MU1+1 or NUDIFF+1.

! Where possible, XDLEGF returns IPQA(I) as zero.  In this case the value of
! the Legendre function is contained entirely in PQA(I), so it can be used in
! subsequent computations without further consideration of extended-range
! arithmetic.  If IPQA(I) is nonzero, then the value of the Legendre function
! is not representable in floating-point because of underflow or overflow.
! The program that calls XDLEGF must test IPQA(I) to ensure correct usage.

!*** REFERENCES  OLVER AND SMITH,J.COMPUT.PHYSICS,51(1983),N0.3,502-518.
!*** ROUTINES CALLED  XERROR, XDPMU, XDPMUP, XDPNRM, XDPQNU, XDQMU, XDQNU
!                     XDRED, XDSET
!*** END PROLOGUE  XDLEGF

REAL (dp), INTENT(IN)   :: dnu1
INTEGER(i4), INTENT(IN)     :: nudiff
INTEGER(i4), INTENT(IN OUT) :: mu1
INTEGER(i4), INTENT(IN OUT) :: mu2
REAL (dp), INTENT(IN)   :: theta
INTEGER(i4), INTENT(IN)     :: id
REAL (dp), INTENT(OUT)  :: pqa(:)
INTEGER(i4), INTENT(OUT)    :: ipqa(:)

! Local variables

REAL (dp) :: dnu2, sx, x, pi2
INTEGER(i4)   :: i, l

!*** FIRST EXECUTABLE STATEMENT   XDLEGF
CALL xdset (0, 0, 0.0_dp, 0)
pi2 = 2._dp*ATAN(1._dp)

!        ZERO OUTPUT ARRAYS

l = (mu2-mu1) + nudiff + 1
DO i=1,l
  pqa(i) = 0.
  ipqa(i) = 0
END DO

!        CHECK FOR VALID INPUT VALUES

IF(nudiff < 0) GO TO 400
IF(dnu1 < -.5_dp) GO TO 400
IF(mu2 < mu1) GO TO 400
IF(mu1 < 0) GO TO 400
IF(theta <= 0._dp .OR. theta > pi2) GO TO 420
IF(id < 1 .OR. id > 4) GO TO 400
IF((mu1 /= mu2).AND.(nudiff > 0)) GO TO 400

!      IF DNU1 IS NOT AN INTEGER, NORMALIZED P(MU,DNU,X) CANNOT BE CALCULATED.
!      IF DNU1 IS AN INTEGER AND MU1 > DNU2 THEN ALL VALUES OF P(+MU,DNU,X)
!      AND NORMALIZED P(MU,NU,X) WILL BE ZERO.

dnu2 = dnu1 + nudiff
IF(id == 3 .AND. MOD(dnu1,1._dp) /= 0.) GO TO 295
IF(id == 4 .AND. MOD(dnu1,1._dp) /= 0.) GO TO 400
IF((id == 3 .OR. id == 4) .AND. DBLE(mu1) > dnu2) RETURN

295 x = COS(theta)
sx = 1._dp/SIN(theta)
IF(id == 2) GO TO 300
IF(mu2-mu1 <= 0) GO TO 360

!        FIXED NU, VARIABLE MU
!        CALL XDPMU TO CALCULATE P(-MU1,NU,X),....,P(-MU2,NU,X)

CALL xdpmu(dnu1, dnu2, mu1, mu2, theta, x, sx, id, pqa, ipqa)
GO TO 380

300 IF(mu2 == mu1) GO TO 320

!        FIXED NU, VARIABLE MU
!        CALL XDQMU TO CALCULATE Q(MU1,NU,X),....,Q(MU2,NU,X)

CALL xdqmu(dnu1, dnu2, mu1, mu2, theta, x, sx, id, pqa, ipqa)
GO TO 390

!        FIXED MU, VARIABLE NU
!        CALL XDQNU TO CALCULATE Q(MU,DNU1,X),....,Q(MU,DNU2,X)

320 CALL xdqnu(dnu1, dnu2, mu1, theta, x, sx, id, pqa, ipqa)
GO TO 390

!        FIXED MU, VARIABLE NU
!        CALL XDPQNU TO CALCULATE P(-MU,DNU1,X),....,P(-MU,DNU2,X)

360 CALL xdpqnu(dnu1, dnu2, mu1, theta, id, pqa, ipqa)

!        IF ID = 3, TRANSFORM P(-MU,NU,X) VECTOR INTO P(MU,NU,X) VECTOR.

380 IF(id == 3) CALL xdpmup(dnu1, dnu2, mu1, mu2, pqa, ipqa)

!        IF ID = 4, TRANSFORM P(-MU,NU,X) VECTOR INTO
!        NORMALIZED P(MU,NU,X) VECTOR.

IF(id == 4) CALL xdpnrm(dnu1, dnu2, mu1, mu2, pqa, ipqa)

!        PLACE RESULTS IN REDUCED FORM IF POSSIBLE AND RETURN TO MAIN PROGRAM.

390 DO i=1,l
  CALL xdred(pqa(i), ipqa(i))
END DO
RETURN

!        *****     ERROR TERMINATION     *****

400 CALL xerror('XDLEGF: DNU1,NUDIFF,MU1,MU2, or ID not valid', 44, 1, 1)
GO TO 430
420 CALL xerror('XDLEGF: THETA out of range', 26, 2, 1)

430 RETURN
END SUBROUTINE xdlegf


SUBROUTINE xdpmup(nu1, nu2, mu1, mu2, pqa, ipqa)
!*** BEGIN PROLOGUE  XDPMUP
!*** REFER TO  XDLEGF
!*** ROUTINES CALLED  XDADJ
!*** DATE WRITTEN   820728   (YYMMDD)
!*** REVISION DATE  871119   (YYMMDD)
!*** CATEGORY NO.  C3a2,C9
!*** KEYWORDS  LEGENDRE FUNCTIONS
!*** AUTHOR  SMITH, JOHN M. (NBS AND GEORGE MASON UNIVERSITY)
!*** PURPOSE  TO COMPUTE THE VALUES OF LEGENDRE FUNCTIONS FOR XDLEGF.
!        SUBROUTINE XDPMUP TRANSFORMS AN ARRAY OF LEGENDRE FUNCTIONS
!        OF THE FIRST KIND OF NEGATIVE ORDER STORED IN ARRAY
!        PQA INTO LEGENDRE FUNCTIONS OF THE FIRST KIND OF
!        POSITIVE ORDER  STORED IN ARRAY PQA. THE ORIGINAL
!        ARRAY IS DESTROYED.
!*** REFERENCES  OLVER AND SMITH,J.COMPUT.PHYSICS,51(1983),N0.3,502-518.
!*** END PROLOGUE  XDPMUP

REAL (dp), INTENT(IN)   :: nu1
REAL (dp), INTENT(IN)   :: nu2
INTEGER(i4), INTENT(IN)     :: mu1
INTEGER(i4), INTENT(IN)     :: mu2
REAL (dp), INTENT(OUT)  :: pqa(:)
INTEGER(i4), INTENT(OUT)    :: ipqa(:)

! Local variables

REAL (dp) :: dmu, nu, prod
INTEGER(i4)   :: i, iprod, j, k, l, mu, n

!*** FIRST EXECUTABLE STATEMENT   XDPMUP
nu = nu1
mu = mu1
dmu = DBLE(mu)
n = INT(nu2-nu1+.1) + (mu2-mu1) + 1
j = 1
IF(MOD(REAL(nu), 1.) /= 0.) GO TO 210
200 IF(dmu < nu+1._dp) GO TO 210
pqa(j) = 0.
ipqa(j) = 0
j = j + 1
IF(j > n) RETURN
!        INCREMENT EITHER MU OR NU AS APPROPRIATE.
IF(nu2-nu1 > .5_dp) nu = nu + 1._dp
IF(mu2 > mu1) mu = mu + 1
GO TO 200

!        TRANSFORM P(-MU,NU,X) TO P(MU,NU,X) USING
!        P(MU,NU,X)=(NU-MU+1)*(NU-MU+2)*...*(NU+MU)*P(-MU,NU,X)*(-1)**MU

210 prod = 1._dp
iprod = 0
k = 2*mu
IF(k == 0) GO TO 222
DO l=1,k
  prod = prod*(dmu-nu-DBLE(l))
  CALL xdadj(prod, iprod)
END DO

222 DO i=j,n
  IF(mu == 0) GO TO 225
  pqa(i) = pqa(i)*prod*DBLE((-1)**mu)
  ipqa(i) = ipqa(i) + iprod
  CALL xdadj(pqa(i), ipqa(i))
  225 IF(nu2-nu1 > .5_dp) GO TO 230
  prod = (dmu-nu)*prod*(-dmu-nu-1._dp)
  CALL xdadj(prod, iprod)
  mu = mu + 1
  dmu = dmu + 1._dp
  CYCLE
  230 prod = prod*(-dmu-nu-1._dp)/(dmu-nu-1._dp)
  CALL xdadj(prod, iprod)
  nu = nu + 1._dp
END DO

RETURN
END SUBROUTINE xdpmup


SUBROUTINE xdset(irad, nradpl, dzero, nbits)
!*** BEGIN PROLOGUE  XDSET
!*** DATE WRITTEN   820712   (YYMMDD)
!*** REVISION DATE  871110   (YYMMDD)
!*** CATEGORY NO.  A3d
!*** KEYWORDS  EXTENDED-RANGE DOUBLE-PRECISION ARITHMETIC
!*** AUTHOR  LOZIER, DANIEL W. (NATIONAL BUREAU OF STANDARDS)
!           SMITH, JOHN M. (NBS AND GEORGE MASON UNIVERSITY)
!*** PURPOSE  TO PROVIDE DOUBLE-PRECISION FLOATING-POINT ARITHMETIC
!            WITH AN EXTENDED EXPONENT RANGE
!*** DESCRIPTION

!   SUBROUTINE  XDSET  MUST BE CALLED PRIOR TO CALLING ANY OTHER
! EXTENDED-RANGE SUBROUTINE. IT CALCULATES AND STORES SEVERAL
! MACHINE-DEPENDENT CONSTANTS IN COMMON BLOCKS. THE USER MUST
! SUPPLY FOUR CONSTANTS THAT PERTAIN TO HIS PARTICULAR COMPUTER.
! THE CONSTANTS ARE

!          IRAD = THE INTERNAL BASE OF DOUBLE-PRECISION
!                 ARITHMETIC IN THE COMPUTER.
!        NRADPL = THE NUMBER OF RADIX PLACES CARRIED IN
!                 THE DOUBLE-PRECISION REPRESENTATION.
!         DZERO = THE SMALLEST OF 1/DMIN, DMAX, DMAXLN WHERE
!                 DMIN = THE SMALLEST POSITIVE DOUBLE-PRECISION NUMBER OR AN
!                 UPPER BOUND TO THIS NUMBER,
!                 DMAX = THE LARGEST DOUBLE-PRECISION NUMBER OR A LOWER BOUND
!                 TO THIS NUMBER,
!                 DMAXLN = THE LARGEST DOUBLE-PRECISION NUMBER SUCH THAT
!                 LOG10(DMAXLN) CAN BE COMPUTED BY THE FORTRAN SYSTEM (ON MOST
!                 SYSTEMS DMAXLN = DMAX).
!         NBITS = THE NUMBER OF BITS (EXCLUSIVE OF SIGN) IN AN INTEGER
!                 COMPUTER WORD.

! ALTERNATIVELY, ANY OR ALL OF THE CONSTANTS CAN BE GIVEN THE VALUE 0
! (0.0_dp FOR DZERO). IF A CONSTANT IS ZERO, XDSET TRIES TO ASSIGN AN
! APPROPRIATE VALUE BY CALLING I1MACH.
! (SEE P.A.FOX, A.D.HALL, N.L.SCHRYER, ALGORITHM 528 FRAMEWORK FOR A PORTABLE
! LIBRARY, ACM TRANSACTIONS ON MATH SOFTWARE, V.4, NO.2, JUNE 1978, 177-188).

! THIS IS THE SETTING-UP SUBROUTINE FOR A PACKAGE OF SUBROUTINES THAT
! FACILITATE THE USE OF EXTENDED-RANGE ARITHMETIC.  EXTENDED-RANGE ARITHMETIC
! ON A PARTICULAR COMPUTER IS DEFINED ON THE SET OF NUMBERS OF THE FORM

!               (X,IX) = X*RADIX**IX

! WHERE X IS A DOUBLE-PRECISION NUMBER CALLED THE PRINCIPAL PART,
! IX IS AN INTEGER CALLED THE AUXILIARY INDEX, AND RADIX IS THE INTERNAL BASE
! OF THE DOUBLE-PRECISION ARITHMETIC.   OBVIOUSLY, EACH REAL NUMBER IS
! REPRESENTABLE WITHOUT ERROR BY MORE THAN ONE EXTENDED-RANGE FORM.
! CONVERSIONS BETWEEN  DIFFERENT FORMS ARE ESSENTIAL IN CARRYING OUT ARITHMETIC
! OPERATIONS.  WITH THE CHOICE OF RADIX WE HAVE MADE, AND THE SUBROUTINES WE
! HAVE WRITTEN, THESE CONVERSIONS ARE PERFORMED WITHOUT ERROR (AT LEAST ON
! MOST COMPUTERS).
! (SEE SMITH, J.M., OLVER, F.W.J., AND LOZIER, D.W., EXTENDED-RANGE ARITHMETIC
! AND NORMALIZED LEGENDRE POLYNOMIALS, ACM TRANSACTIONS ON MATHEMATICAL
! SOFTWARE, MARCH 1981).

! AN EXTENDED-RANGE NUMBER  (X,IX)  IS SAID TO BE IN ADJUSTED FORM IF
! X AND IX ARE ZERO OR

!           RADIX**(-L) <= ABS(X) < RADIX**L

! IS SATISFIED, WHERE L IS A COMPUTER-DEPENDENT INTEGER DEFINED IN THIS
! SUBROUTINE.  TWO EXTENDED-RANGE NUMBERS IN ADJUSTED FORM CAN BE ADDED,
! SUBTRACTED, MULTIPLIED OR DIVIDED (IF THE DIVISOR IS NONZERO) WITHOUT CAUSING
! OVERFLOW OR UNDERFLOW IN THE PRINCIPAL PART OF THE RESULT.   WITH PROPER USE
! OF THE EXTENDED-RANGE SUBROUTINES, THE ONLY OVERFLOW THAT CAN OCCUR IS
! INTEGER OVERFLOW IN THE AUXILIARY INDEX.  IF THIS IS DETECTED, THE SOFTWARE
! CALLS XERROR (A GENERAL ERROR-HANDLING FORTRAN SUBROUTINE PACKAGE).

!   MULTIPLICATION AND DIVISION IS PERFORMED BY SETTING

!                 (X,IX)*(Y,IY) = (X*Y,IX+IY)
! OR
!                 (X,IX)/(Y,IY) = (X/Y,IX-IY).

! PRE-ADJUSTMENT OF THE OPERANDS IS ESSENTIAL TO AVOID OVERFLOW OR UNDERFLOW
! OF THE PRINCIPAL PART.  SUBROUTINE XDADJ (SEE BELOW) MAY BE CALLED TO
! TRANSFORM ANY EXTENDED-RANGE NUMBER INTO ADJUSTED FORM.

! ADDITION AND SUBTRACTION REQUIRE THE USE OF SUBROUTINE XDADD (SEE BELOW).
! THE INPUT OPERANDS NEED NOT BE IN ADJUSTED FORM.
! HOWEVER, THE RESULT OF ADDITION OR SUBTRACTION IS RETURNED IN ADJUSTED FORM.
! THUS, FOR EXAMPLE, IF (X,IX),(Y,IY), (U,IU), AND (V,IV) ARE IN ADJUSTED FORM,
! THEN
!                 (X,IX)*(Y,IY) + (U,IU)*(V,IV)

! CAN BE COMPUTED AND STORED IN ADJUSTED FORM WITH NO EXPLICIT CALLS TO XDADJ.

! WHEN AN EXTENDED-RANGE NUMBER IS TO BE PRINTED, IT MUST BE CONVERTED TO AN
! EXTENDED-RANGE FORM WITH DECIMAL RADIX.  SUBROUTINE XDCON IS PROVIDED FOR
! THIS PURPOSE.

!   THE SUBROUTINES CONTAINED IN THIS PACKAGE ARE

!     SUBROUTINE XDADD
! USAGE
!                  CALL XDADD(X,IX,Y,IY,Z,IZ)
! DESCRIPTION
!                  FORMS THE EXTENDED-RANGE SUM  (Z,IZ) = (X,IX) + (Y,IY).
!                  (Z,IZ) IS ADJUSTED BEFORE RETURNING.
!                  THE INPUT OPERANDS NEED NOT BE IN ADJUSTED FORM, BUT THEIR
!                  PRINCIPAL PARTS MUST SATISFY
!                  RADIX**(-2L)<=ABS(X)<=RADIX**(2L),
!                  RADIX**(-2L)<=ABS(Y)<=RADIX**(2L).

!     SUBROUTINE XDADJ
! USAGE
!                  CALL XDADJ(X,IX)
! DESCRIPTION
!                  TRANSFORMS (X,IX) SO THAT
!                  RADIX**(-L) <= ABS(X) < RADIX**L.
!                  ON MOST COMPUTERS THIS TRANSFORMATION DOES
!                  NOT CHANGE THE MANTISSA OF X PROVIDED RADIX IS
!                  THE NUMBER BASE OF DOUBLE-PRECISION ARITHMETIC.

!     SUBROUTINE XDC210
! USAGE
!                  CALL XDC210(K,Z,J)
! DESCRIPTION
!                  GIVEN K THIS SUBROUTINE COMPUTES J AND Z SUCH THAT
!                  RADIX**K = Z*10**J, WHERE Z IS IN THE RANGE 1/10 <= Z < 1.
!                  THE VALUE OF Z WILL BE ACCURATE TO FULL DOUBLE-PRECISION
!                  PROVIDED THE NUMBER OF DECIMAL PLACES IN THE LARGEST
!                  INTEGER PLUS THE NUMBER OF DECIMAL PLACES CARRIED IN
!                  DOUBLE-PRECISION DOES NOT EXCEED 60.  XDC210 IS CALLED BY
!                  SUBROUTINE XDCON WHEN NECESSARY.  THE USER SHOULD NEVER
!                  NEED TO CALL XDC210 DIRECTLY.

!     SUBROUTINE XDCON
! USAGE
!                  CALL XDCON(X,IX)
! DESCRIPTION
!                  CONVERTS (X,IX) = X*RADIX**IX TO DECIMAL FORM IN PREPARATION
!                  FOR PRINTING, SO THAT (X,IX) = X*10**IX
!                  WHERE 0.1 <= ABS(X) < 1 IS RETURNED, EXCEPT THAT IF
!                  (ABS(X),IX) IS BETWEEN RADIX**(-2L) AND RADIX**(2L)
!                  THEN THE REDUCED FORM WITH IX = 0 IS RETURNED.

!     SUBROUTINE XDRED
! USAGE
!                  CALL XDRED(X,IX)
! DESCRIPTION
!                  IF
!                  RADIX**(-2L) <= (ABS(X),IX) <= RADIX**(2L)
!                  THEN XDRED TRANSFORMS (X,IX) SO THAT IX=0.
!                  IF (X,IX) IS OUTSIDE THE ABOVE RANGE,
!                  THEN XDRED TAKES NO ACTION.
!                  THIS SUBROUTINE IS USEFUL IF THE RESULTS OF EXTENDED-RANGE
!                  CALCULATIONS ARE TO BE USED IN SUBSEQUENT ORDINARY
!                  DOUBLE-PRECISION CALCULATIONS.

!*** REFERENCES  (SEE DESCRIPTION ABOVE)
!*** ROUTINES CALLED  I1MACH, XERROR
!*** COMMON BLOCKS    XDBLK1, XDBLK2, XDBLK3
!*** END PROLOGUE  XDSET

INTEGER(i4), INTENT(IN)    :: irad
INTEGER(i4), INTENT(IN)    :: nradpl
REAL (dp), INTENT(IN)  :: dzero
INTEGER(i4), INTENT(IN)    :: nbits

REAL (dp) ::  dzerox

INTEGER(i4), SAVE :: nlg102, lg102(21)
INTEGER(i4), SAVE :: iflag = 0

INTEGER(i4) :: i, ic, ii, imaxex, iminex, iradx, it, j, k, kk,  &
           lgtemp(20), log2r, nb, nbitsx, np1, nrdplc

! LOG102 CONTAINS THE FIRST 60 DIGITS OF LOG10(2) FOR USE IN
! CONVERSION OF EXTENDED-RANGE NUMBERS TO BASE 10.

INTEGER(i4), PARAMETER :: log102(20) = (/301, 029, 995, 663, 981, 195, 213, 738, &
                                     894, 724, 493, 026, 768, 189, 881, 462, &
                                     108, 541, 310, 428 /)

! FOLLOWING CODING PREVENTS XDSET FROM BEING EXECUTED MORE THAN ONCE.
! THIS IS IMPORTANT BECAUSE SOME SUBROUTINES (SUCH AS XDNRMP AND XDLEGF)
! CALL XDSET TO MAKE SURE EXTENDED-RANGE ARITHMETIC HAS BEEN INITIALIZED.
! THE USER MAY WANT TO PRE-EMPT THIS CALL, FOR EXAMPLE WHEN I1MACH IS NOT
! AVAILABLE.  SEE CODING BELOW.

!*** FIRST EXECUTABLE STATEMENT  XDSET
IF (iflag /= 0) RETURN
iflag = 1
iradx = irad
nrdplc = nradpl
dzerox = dzero
iminex = 0
imaxex = 0
nbitsx = nbits

! FOLLOWING 6 STATEMENTS SHOULD BE DELETED IF I1MACH NOT AVAILABLE
! OR NOT CONFIGURED TO RETURN THE CORRECT MACHINE-DEPENDENT VALUES.

IF (iradx == 0) iradx = i1mach (10)
IF (nrdplc == 0) nrdplc = i1mach (14)
IF (dzerox == 0.0_dp) iminex = i1mach (15)
IF (dzerox == 0.0_dp) imaxex = i1mach (16)
IF (nbitsx == 0) nbitsx = i1mach (8)
IF (iradx == 2) GO TO 10
IF (iradx == 4) GO TO 10
IF (iradx == 8) GO TO 10
IF (iradx == 16) GO TO 10
CALL xerror('ERR IN XDSET...IMPROPER VALUE OF IRAD', 37, 1, 1)
GO TO 100

10 log2r=0
IF (iradx == 2) log2r = 1
IF (iradx == 4) log2r = 2
IF (iradx == 8) log2r = 3
IF (iradx == 16) log2r = 4
nbitsf = log2r*nrdplc
radix0 = iradx
dlg10r = LOG10(radix0)
IF (dzerox /= 0.0_dp) GO TO 14
l1 = MIN ((1-iminex)/2, (imaxex-1)/2)
GO TO 16
14 l1 = 0.5_dp*LOG10(dzerox)/dlg10r

! radix0**(2*L) SHOULD NOT OVERFLOW, BUT REDUCE L BY 1 FOR FURTHER PROTECTION.
l1 = l1 - 1
16 l2 = 2*l1
IF (l1 >= 4) GO TO 20
CALL xerror('ERR IN XDSET...IMPROPER VALUE OF DZERO', 38, 2, 1)
GO TO 100
20 radixl = radix0**l1
rad2l = radixl**2

! IT IS NECESSARY TO RESTRICT NBITS (OR NBITSX) TO BE LESS THAN SOME UPPER
! LIMIT BECAUSE OF BINARY-TO-DECIMAL CONVERSION. SUCH CONVERSION IS DONE BY
! XDC210 AND REQUIRES A CONSTANT THAT IS STORED TO SOME FIXED PRECISION.
! THE CONSTANT THAT IS STORED (LOG102 IN THIS ROUTINE) PROVIDES FOR CONVERSIONS
! TO BE ACCURATE TO THE LAST DECIMAL DIGIT WHEN THE INTEGER WORD LENGTH DOES
! NOT EXCEED 63.  A LOWER LIMIT OF 15 BITS IS IMPOSED BECAUSE THE SOFTWARE IS
! DESIGNED TO RUN ON COMPUTERS WITH INTEGER WORD LENGTH OF AT LEAST 16 BITS.

IF (15 <= nbitsx .AND. nbitsx <= 63) GO TO 30
CALL xerror('ERR IN XDSET...IMPROPER VALUE OF NBITS', 38, 3, 1)
GO TO 100

30 kmax = 2**(nbitsx-1) - l2
nb = (nbitsx-1)/2
IF (1 <= nrdplc*log2r .AND. nrdplc*log2r <= 120) GO TO 40
CALL xerror('ERR IN XDSET...IMPROPER VALUE OF NRADPL', 39, 4, 1)
GO TO 100

40 nlg102 = nrdplc*log2r/nb + 3
np1 = nlg102 + 1

! AFTER COMPLETION OF THE FOLLOWING LOOP, IC CONTAINS THE INTEGER PART AND
! LGTEMP CONTAINS THE FRACTIONAL PART OF LOG10(IRADX) IN RADIX 1000.
ic = 0
DO ii=1,20
  i = 21 - ii
  it = log2r*log102(i) + ic
  ic = it/1000
  lgtemp(i) = MOD(it,1000)
END DO

! AFTER COMPLETION OF THE FOLLOWING LOOP, LG102 CONTAINS LOG10(IRADX) IN RADIX
! MLG102.  THE RADIX POINT IS BETWEEN LG102(1) AND LG102(2).
lg102(1) = ic
DO i=2,np1
  lg102(i) = 0
  DO j=1,nb
    ic = 0
    DO kk=1,20
      k = 21 - kk
      it = 2*lgtemp(k) + ic
      ic = it/1000
      lgtemp(k) = MOD(it, 1000)
    END DO
    lg102(i) = 2*lg102(i) + ic
  END DO
END DO

! CHECK SPECIAL CONDITIONS REQUIRED BY SUBROUTINES...
IF (nrdplc < l1) GO TO 90
CALL xerror('ERR IN XDSET...NRADPL >= L', 26, 5, 1)
GO TO 100
90 IF (6*l1 <= kmax) GO TO 100
CALL xerror('ERR IN XDSET...6*L > KMAX', 25, 6, 1)
GO TO 100

100 RETURN
END SUBROUTINE xdset


SUBROUTINE xdpmu(nu1, nu2, mu1, mu2, theta, x, sx, id, pqa, ipqa)
!*** BEGIN PROLOGUE  XDPMU
!*** REFER TO  XDLEGF
!*** ROUTINES CALLED  XDADD, XDADJ, XDPQNU
!*** DATE WRITTEN   820728   (YYMMDD)
!*** REVISION DATE  871119   (YYMMDD)
!*** CATEGORY NO.  C3a2,C9
!*** KEYWORDS  LEGENDRE FUNCTIONS
!*** AUTHOR  SMITH, JOHN M. (NBS AND GEORGE MASON UNIVERSITY)
!*** PURPOSE  TO COMPUTE THE VALUES OF LEGENDRE FUNCTIONS FOR XDLEGF.
! METHOD: BACKWARD MU-WISE RECURRENCE FOR P(-MU,NU,X) FOR FIXED NU TO
!         OBTAIN P(-MU2,NU1,X), P(-(MU2-1),NU1,X),...., P(-MU1,NU1,X)
!         AND STORE IN ASCENDING MU ORDER.
!*** REFERENCES  OLVER AND SMITH, J.COMPUT.PHYSICS, 51(1983), N0.3, 502-518.
!*** END PROLOGUE  XDPMU

REAL (dp), INTENT(IN)      :: nu1
REAL (dp), INTENT(IN)      :: nu2
INTEGER(i4), INTENT(IN)        :: mu1
INTEGER(i4), INTENT(IN OUT)    :: mu2
REAL (dp), INTENT(IN)      :: theta
REAL (dp), INTENT(IN)      :: x
REAL (dp), INTENT(IN)      :: sx
INTEGER(i4), INTENT(IN)        :: id
REAL (dp), INTENT(IN OUT)  :: pqa(:)
INTEGER(i4), INTENT(IN OUT)    :: ipqa(:)

! Local variables

REAL (dp) :: p0, x1, x2
INTEGER(i4)   :: ip0, j, mu, n

!        CALL XDPQNU TO OBTAIN P(-MU2,NU,X)

!*** FIRST EXECUTABLE STATEMENT   XDPMU
CALL xdpqnu(nu1, nu2, mu2, theta, id, pqa, ipqa)
p0 = pqa(1)
ip0 = ipqa(1)
mu = mu2 - 1

!        CALL XDPQNU TO OBTAIN P(-MU2-1,NU,X)

CALL xdpqnu(nu1, nu2, mu, theta, id, pqa, ipqa)
n = mu2 - mu1 + 1
pqa(n) = p0
ipqa(n) = ip0
IF(n == 1) GO TO 300
pqa(n-1) = pqa(1)
ipqa(n-1) = ipqa(1)
IF(n == 2) GO TO 300
j=n-2

!        BACKWARD RECURRENCE IN MU TO OBTAIN
!              P(-MU2,NU1,X),P(-(MU2-1),NU1,X),....P(-MU1,NU1,X)
!              USING
!              (NU-MU)*(NU+MU+1.)*P(-(MU+1),NU,X)=
!                2.*MU*X*SQRT((1./(1.-X**2))*P(-MU,NU,X)-P(-(MU-1),NU,X)

290 x1 = 2._dp*DBLE(mu)*x*sx*pqa(j+1)
x2 = -(nu1-DBLE(mu))*(nu1+DBLE(mu)+1._dp)*pqa(j+2)
CALL xdadd(x1, ipqa(j+1), x2, ipqa(j+2), pqa(j), ipqa(j))
CALL xdadj(pqa(j), ipqa(j))
IF(j == 1) GO TO 300
j = j - 1
mu = mu - 1
GO TO 290

300 RETURN
END SUBROUTINE xdpmu


SUBROUTINE xdqmu(nu1, nu2, mu1, mu2, theta, x, sx, id, pqa, ipqa)
!*** BEGIN PROLOGUE  XDQMU
!*** REFER TO  XDLEGF
!*** ROUTINES CALLED  XDADD, XDADJ, XDPQNU
!*** DATE WRITTEN   820728   (YYMMDD)
!*** REVISION DATE  871119   (YYMMDD)
!*** CATEGORY NO.  C3a2,C9
!*** KEYWORDS  LEGENDRE FUNCTIONS
!*** AUTHOR  SMITH, JOHN M. (NBS AND GEORGE MASON UNIVERSITY)
!*** PURPOSE  TO COMPUTE THE VALUES OF LEGENDRE FUNCTIONS FOR XDLEGF.
! METHOD: FORWARD MU-WISE RECURRENCE FOR Q(MU,NU,X) FOR FIXED NU TO
!         OBTAIN  Q(MU1,NU,X),Q(MU1+1,NU,X),....,Q(MU2,NU,X)
!*** REFERENCES  OLVER AND SMITH,J.COMPUT.PHYSICS,51(1983),N0.3,502-518.
!*** END PROLOGUE  XDQMU

REAL (dp), INTENT(IN)      :: nu1
REAL (dp), INTENT(IN)      :: nu2
INTEGER(i4), INTENT(IN)        :: mu1
INTEGER(i4), INTENT(IN)        :: mu2
REAL (dp), INTENT(IN)      :: theta
REAL (dp), INTENT(IN)      :: x
REAL (dp), INTENT(IN)      :: sx
INTEGER(i4), INTENT(IN)        :: id
REAL (dp), INTENT(IN OUT)  :: pqa(:)
INTEGER(i4), INTENT(IN OUT)    :: ipqa(:)

! Local variables

REAL (dp) :: dmu, nu, pq, pq1, pq2, x1, x2
INTEGER(i4)   :: ipq, ipq1, ipq2, k, mu

!*** FIRST EXECUTABLE STATEMENT   XDQMU
mu = 0

!        CALL XDPQNU TO OBTAIN Q(0.,NU1,X)

CALL xdpqnu(nu1, nu2, mu, theta, id, pqa, ipqa)
pq2 = pqa(1)
ipq2 = ipqa(1)
mu=1

!        CALL XDPQNU TO OBTAIN Q(1.,NU1,X)

CALL xdpqnu(nu1, nu2, mu, theta, id, pqa, ipqa)
nu = nu1
k = 0
mu = 1
dmu = 1._dp
pq1 = pqa(1)
ipq1 = ipqa(1)
IF(mu1 > 0) GO TO 310
k = k + 1
pqa(k) = pq2
ipqa(k) = ipq2
IF(mu2 < 1) GO TO 330
310 IF(mu1 > 1) GO TO 320
k = k + 1
pqa(k) = pq1
ipqa(k) = ipq1
IF(mu2 <= 1) GO TO 330

!        FORWARD RECURRENCE IN MU TO OBTAIN
!                  Q(MU1,NU,X),Q(MU1+1,NU,X),....,Q(MU2,NU,X) USING
!             Q(MU+1,NU,X)=-2.*MU*X*SQRT(1./(1.-X**2))*Q(MU,NU,X)
!                               -(NU+MU)*(NU-MU+1.)*Q(MU-1,NU,X)

320 x1 = -2._dp*dmu*x*sx*pq1
x2 = (nu+dmu)*(nu-dmu+1._dp)*pq2
CALL xdadd(x1, ipq1, -x2, ipq2, pq, ipq)
CALL xdadj(pq, ipq)
pq2 = pq1
ipq2 = ipq1
pq1 = pq
ipq1 = ipq
mu = mu + 1
dmu = dmu + 1._dp
IF(mu < mu1) GO TO 320
k = k+1
pqa(k) = pq
ipqa(k) = ipq
IF(mu2 > mu) GO TO 320

330 RETURN
END SUBROUTINE xdqmu


SUBROUTINE xdpnrm(nu1, nu2, mu1, mu2, pqa, ipqa)
!*** BEGIN PROLOGUE  XDPNRM
!*** REFER TO  XDLEGF
!*** ROUTINES CALLED  XDADJ
!*** DATE WRITTEN   820728   (YYMMDD)
!*** REVISION DATE  871119   (YYMMDD)
!*** CATEGORY NO.  C3a2,C9
!*** KEYWORDS  LEGENDRE FUNCTIONS
!*** AUTHOR  SMITH, JOHN M. (NBS AND GEORGE MASON UNIVERSITY)
!*** PURPOSE  TO COMPUTE THE VALUES OF LEGENDRE FUNCTIONS FOR XDLEGF.
!        SUBROUTINE XDPNRM TRANSFORMS AN ARRAY OF LEGENDRE
!        FUNCTIONS OF THE FIRST KIND OF NEGATIVE ORDER STORED
!        IN ARRAY PQA INTO NORMALIZED LEGENDRE POLYNOMIALS STORED
!        IN ARRAY PQA. THE ORIGINAL ARRAY IS DESTROYED.
!*** REFERENCES  OLVER AND SMITH,J.COMPUT.PHYSICS,51(1983),N0.3,502-518.
!*** END PROLOGUE  XDPNRM

REAL (dp), INTENT(IN)   :: nu1
REAL (dp), INTENT(IN)   :: nu2
INTEGER(i4), INTENT(IN)     :: mu1
INTEGER(i4), INTENT(IN)     :: mu2
REAL (dp), INTENT(OUT)  :: pqa(:)
INTEGER(i4), INTENT(OUT)    :: ipqa(:)

! Local variables

REAL (dp) :: c1, dmu, nu, prod
INTEGER(i4)   :: i, iprod, j, k, l, mu

!*** FIRST EXECUTABLE STATEMENT   XDPNRM
l = (mu2-mu1) + (nu2-nu1+1.5_dp)
mu = mu1
dmu = DBLE(mu1)
nu = nu1

!         IF MU > NU, NORM P =0.

j = 1
500 IF(dmu <= nu) GO TO 505
pqa(j) = 0.
ipqa(j) = 0
j = j + 1
IF(j > l) RETURN

!        INCREMENT EITHER MU OR NU AS APPROPRIATE.

IF(mu2 > mu1) dmu = dmu + 1._dp
IF(nu2-nu1 > .5_dp) nu = nu + 1._dp
GO TO 500

!         TRANSFORM P(-MU,NU,X) INTO NORMALIZED P(MU,NU,X) USING
!              NORM P(MU,NU,X)=
!                 SQRT((NU+.5)*FACTORIAL(NU+MU)/FACTORIAL(NU-MU))
!                             *P(-MU,NU,X)

505 prod = 1._dp
iprod = 0
k = 2*mu
IF(k <= 0) GO TO 520
DO i=1,k
  prod = prod*SQRT(nu+dmu+1._dp-DBLE(i))
  CALL xdadj(prod, iprod)
END DO
520 DO i=j,l
  c1 = prod*SQRT(nu+.5_dp)
  pqa(i) = pqa(i)*c1
  ipqa(i) = ipqa(i) + iprod
  CALL xdadj(pqa(i), ipqa(i))
  IF(nu2-nu1 > .5_dp) GO TO 530
  IF(dmu >= nu) GO TO 525
  prod = SQRT(nu + dmu + 1._dp)*prod
  IF(nu > dmu) prod = prod*SQRT(nu-dmu)
  CALL xdadj(prod, iprod)
  mu = mu + 1
  dmu = dmu + 1._dp
  CYCLE

  525 prod = 0.
  iprod = 0
  mu = mu + 1
  dmu = dmu + 1._dp
  CYCLE

  530 prod = SQRT(nu + dmu + 1._dp)*prod
  IF(nu /= dmu-1._dp) prod = prod/SQRT(nu-dmu+1._dp)
  CALL xdadj(prod, iprod)
  nu = nu + 1._dp
END DO

RETURN
END SUBROUTINE xdpnrm


SUBROUTINE xdqnu(nu1, nu2, mu1, theta, x, sx, id, pqa, ipqa)
!*** BEGIN PROLOGUE  XDQNU
!*** REFER TO  XDLEGF
!*** ROUTINES CALLED  XDADD, XDADJ, XDPQNU
!*** DATE WRITTEN   820728   (YYMMDD)
!*** REVISION DATE  871119   (YYMMDD)
!*** CATEGORY NO.  C3a2,C9
!*** KEYWORDS  LEGENDRE FUNCTIONS
!*** AUTHOR  SMITH, JOHN M. (NBS AND GEORGE MASON UNIVERSITY)
!*** PURPOSE  TO COMPUTE THE VALUES OF LEGENDRE FUNCTIONS FOR XDLEGF.
!  METHOD: BACKWARD NU-WISE RECURRENCE FOR Q(MU,NU,X) FOR FIXED MU TO
!          OBTAIN Q(MU1,NU1,X),Q(MU1,NU1+1,X),....,Q(MU1,NU2,X)
!*** REFERENCES  OLVER AND SMITH,J.COMPUT.PHYSICS,51(1983),N0.3,502-518.
!*** END PROLOGUE  XDQNU

REAL (dp), INTENT(IN)      :: nu1
REAL (dp), INTENT(IN)      :: nu2
INTEGER(i4), INTENT(IN)        :: mu1
REAL (dp), INTENT(IN)      :: theta
REAL (dp), INTENT(IN)      :: x
REAL (dp), INTENT(IN)      :: sx
INTEGER(i4), INTENT(IN)        :: id
REAL (dp), INTENT(IN OUT)  :: pqa(:)
INTEGER(i4), INTENT(IN OUT)    :: ipqa(:)

! Local variables

REAL (dp) :: dmu, nu, pq, pq1, pq2, x1, x2
REAL (dp) :: pql1, pql2
INTEGER(i4)   :: ipq, ipql1, ipql2, ipq1, ipq2, k, mu

!*** FIRST EXECUTABLE STATEMENT   XDQNU
k = 0
pq2 = 0.0_dp
ipq2 = 0
pql2 = 0.0_dp
ipql2 = 0
IF(mu1 == 1) GO TO 290
mu=0

!        CALL XDPQNU TO OBTAIN Q(0.,NU2,X) AND Q(0.,NU2-1,X)

CALL xdpqnu(nu1, nu2, mu, theta, id, pqa, ipqa)
IF(mu1 == 0) RETURN
k = (nu2-nu1+1.5_dp)
pq2 = pqa(k)
ipq2 = ipqa(k)
pql2 = pqa(k-1)
ipql2 = ipqa(k-1)
290 mu = 1

!        CALL XDPQNU TO OBTAIN Q(1.,NU2,X) AND Q(1.,NU2-1,X)

CALL xdpqnu(nu1, nu2, mu, theta, id, pqa, ipqa)
IF(mu1 == 1) RETURN
nu = nu2
pq1 = pqa(k)
ipq1 = ipqa(k)
pql1 = pqa(k-1)
ipql1 = ipqa(k-1)
300 mu = 1
dmu = 1._dp

!        FORWARD RECURRENCE IN MU TO OBTAIN Q(MU1,NU2,X) AND
!              Q(MU1,NU2-1,X) USING
!              Q(MU+1,NU,X)=-2.*MU*X*SQRT(1./(1.-X**2))*Q(MU,NU,X)
!                   -(NU+MU)*(NU-MU+1.)*Q(MU-1,NU,X)

!              FIRST FOR NU=NU2

320 x1 = -2._dp*dmu*x*sx*pq1
x2 = (nu+dmu)*(nu-dmu+1._dp)*pq2
CALL xdadd(x1, ipq1, -x2, ipq2, pq, ipq)
CALL xdadj(pq, ipq)
pq2 = pq1
ipq2 = ipq1
pq1 = pq
ipq1 = ipq
mu = mu + 1
dmu = dmu + 1._dp
IF(mu < mu1) GO TO 320
pqa(k) = pq
ipqa(k) = ipq
IF(k == 1) RETURN
IF(nu < nu2) GO TO 340

!              THEN FOR NU=NU2-1

nu = nu - 1._dp
pq2 = pql2
ipq2 = ipql2
pq1 = pql1
ipq1 = ipql1
k = k - 1
GO TO 300

!         BACKWARD RECURRENCE IN NU TO OBTAIN
!              Q(MU1,NU1,X),Q(MU1,NU1+1,X),....,Q(MU1,NU2,X)
!              USING
!              (NU-MU+1.)*Q(MU,NU+1,X)=
!                       (2.*NU+1.)*X*Q(MU,NU,X)-(NU+MU)*Q(MU,NU-1,X)

340 pq1 = pqa(k)
ipq1 = ipqa(k)
pq2 = pqa(k+1)
ipq2 = ipqa(k+1)
350 IF(nu <= nu1) RETURN
k = k - 1
x1 = (2._dp*nu + 1._dp)*x*pq1/(nu+dmu)
x2 = -(nu-dmu+1._dp)*pq2/(nu+dmu)
CALL xdadd(x1, ipq1, x2, ipq2, pq, ipq)
CALL xdadj(pq, ipq)
pq2 = pq1
ipq2 = ipq1
pq1 = pq
ipq1 = ipq
pqa(k) = pq
ipqa(k) = ipq
nu = nu - 1._dp
GO TO 350
END SUBROUTINE xdqnu


FUNCTION i1mach(i) RESULT(ival)
!*** BEGIN PROLOGUE  I1MACH
!*** DATE WRITTEN   750101   (YYMMDD)
!*** REVISION DATE  910131   (YYMMDD)
!*** CATEGORY NO.  R1
!*** KEYWORDS  MACHINE CONSTANTS
!*** AUTHOR  FOX, P. A., (BELL LABS)
!           HALL, A. D., (BELL LABS)
!           SCHRYER, N. L., (BELL LABS)
!*** PURPOSE  Returns integer machine dependent constants
!*** DESCRIPTION

!     This is the CMLIB version of I1MACH, the integer machine constants
!     subroutine originally developed for the PORT library.

!     I1MACH can be used to obtain machine-dependent parameters for the
!     local machine environment.  It is a function subroutine with one
!     (input) argument, and can be called as follows, for example

!          K = I1MACH(I)

!     where I=1,...,16.   The (output) value of K above is determined by the
!     (input) value of I.   The results for various values of I are discussed
!     below.

!  I/O unit numbers.
!    I1MACH( 1) = the standard input unit.
!    I1MACH( 2) = the standard output unit.
!    I1MACH( 3) = the standard punch unit.
!    I1MACH( 4) = the standard error message unit.

!  Words.
!    I1MACH( 5) = the number of bits per integer storage unit.
!    I1MACH( 6) = the number of characters per integer storage unit.

!  Integers.
!    assume integers are represented in the S-digit, base-A form

!               sign ( X(S-1)*A**(S-1) + ... + X(1)*A + X(0) )

!               where 0 <= X(I) < A for I=0,...,S-1.
!    I1MACH( 7) = A, the base.
!    I1MACH( 8) = S, the number of base-A digits.
!    I1MACH( 9) = A**S - 1, the largest magnitude.

!  Floating-Point Numbers.
!    Assume floating-point numbers are represented in the T-digit,
!    base-B form
!               sign (B**E)*( (X(1)/B) + ... + (X(T)/B**T) )

!               where 0 <= X(I) < B for I=1,...,T,
!               0 < X(1), and EMIN <= E <= EMAX.
!    I1MACH(10) = B, the base.

!  Single-Precision
!    I1MACH(11) = T, the number of base-B digits.
!    I1MACH(12) = EMIN, the smallest exponent E.
!    I1MACH(13) = EMAX, the largest exponent E.

!  Double-Precision
!    I1MACH(14) = T, the number of base-B digits.
!    I1MACH(15) = EMIN, the smallest exponent E.
!    I1MACH(16) = EMAX, the largest exponent E.

!*** REFERENCES  FOX P.A., HALL A.D., SCHRYER N.L.,*FRAMEWORK FOR A
!                 PORTABLE LIBRARY*, ACM TRANSACTIONS ON MATHEMATICAL
!                 SOFTWARE, VOL. 4, NO. 2, JUNE 1978, PP. 177-188.
!*** ROUTINES CALLED  (NONE)
!*** END PROLOGUE  I1MACH


INTEGER(i4), INTENT(IN)  :: i
INTEGER(i4)              :: ival

!*** FIRST EXECUTABLE STATEMENT  I1MACH
IF (i < 1 .OR. i > 16) CALL xerror ( 'I1MACH -- I OUT OF BOUNDS', 25, 1, 2)

SELECT CASE (i)
  CASE (1)
    ival = 5
  CASE (2)
    ival = 6
  CASE (3)
    ival = 6
  CASE (4)
    ival = 0
  CASE (5)
    ival = BIT_SIZE(i)
  CASE (6)
    ival = 4
  CASE (7)
    ival = RADIX(i)
  CASE (8)
    ival = DIGITS(i)
  CASE (9)
    ival = HUGE(i)
  CASE (10)
    ival = RADIX(1.0)
  CASE (11)
    ival = DIGITS(1.0)
  CASE (12)
    ival = MINEXPONENT(1.0)
  CASE (13)
    ival = MAXEXPONENT(1.0)
  CASE (14)
    ival = DIGITS(1.0_dp)
  CASE (15)
    ival = MINEXPONENT(1.0_dp)
  CASE (16)
    ival = MAXEXPONENT(1.0_dp)
END SELECT

RETURN
END FUNCTION i1mach


SUBROUTINE xdpqnu(nu1, nu2, mu, theta, id, pqa, ipqa)
!*** BEGIN PROLOGUE  XDPQNU
!*** REFER TO  XDLEGF
!*** ROUTINES CALLED  XDADD, XDADJ, XDPSI
!*** COMMON BLOCKS    XDBLK1
!*** DATE WRITTEN   820728   (YYMMDD)
!*** REVISION DATE  871119   (YYMMDD)
!*** CATEGORY NO.  C3A2,C9
!*** KEYWORDS  LEGENDRE FUNCTIONS
!*** AUTHOR  SMITH, JOHN M. (NBS AND GEORGE MASON UNIVERSITY)
!*** PURPOSE  TO COMPUTE THE VALUES OF LEGENDRE FUNCTIONS FOR XDLEGF.
!        SUBROUTINE XDPQNU CALCULATES INITIAL VALUES OF P OR Q
!        USING POWER SERIES.  THEN XDPQNU PERFORMS FORWARD NU-WISE
!        RECURRENCE TO OBTAIN P(-MU,NU,X), Q(0,NU,X), OR Q(1,NU,X).
!        THE FORWARD NU-WISE RECURRENCE IS STABLE FOR P FOR ALL
!        VALUES OF MU, AND IS STABLE FOR Q FOR MU=0 OR 1.
!*** REFERENCES  OLVER AND SMITH,J.COMPUT.PHYSICS,51(1983),N0.3,502-518.
!*** END PROLOGUE  XDPQNU

REAL (dp), INTENT(IN)   :: nu1
REAL (dp), INTENT(IN)   :: nu2
INTEGER(i4), INTENT(IN OUT) :: mu
REAL (dp), INTENT(IN)   :: theta
INTEGER(i4), INTENT(IN)     :: id
REAL (dp), INTENT(OUT)  :: pqa(:)
INTEGER(i4), INTENT(OUT)    :: ipqa(:)

! Local variables

REAL (dp) :: a, nu, pq, r, w, x, x1, x2, xs, y, z
REAL (dp) :: di, dmu, pq1, pq2, factmu, flok
INTEGER(i4)   :: i, ia, if, ipq, ipq1, ipq2, ipsik, ipsix, ixs, ix1, j, j0, k

!    J0, IPSIK, AND IPSIX ARE INITIALIZED IN THIS SUBROUTINE.
!    J0 IS THE NUMBER OF TERMS USED IN SERIES EXPANSION IN SUBROUTINE XDPQNU.
!    IPSIK, IPSIX ARE VALUES OF K AND X RESPECTIVELY
!    USED IN THE CALCULATION OF THE XDPSI FUNCTION.

!*** FIRST EXECUTABLE STATEMENT   XDPQNU
j0 = nbitsf
ipsik = 1 + (nbitsf/10)
ipsix = 5*ipsik
ipq = 0
!        FIND NU IN INTERVAL [-.5,.5) IF ID=2  ( CALCULATION OF Q )
nu = MOD(nu1, 1._dp)
IF(nu >= .5_dp) nu = nu - 1._dp
!        FIND NU IN INTERVAL (-1.5,-.5] IF ID=1,3, OR 4  ( CALCULATION OF P )
IF(id /= 2 .AND. nu > -.5_dp) nu = nu - 1._dp
!        CALCULATE MU FACTORIAL
k = mu
dmu = DBLE(mu)
IF(mu <= 0) GO TO 60
factmu = 1._dp
IF = 0
DO i=1,k
  factmu = factmu*DBLE(i)
  CALL xdadj(factmu, IF)
END DO
60 IF(k == 0) factmu = 1._dp
IF(k == 0) IF = 0

!        X=COS(THETA)
!        Y=SIN(THETA/2)**2=(1-X)/2=.5-.5*X
!        R=TAN(THETA/2)=SQRT((1-X)/(1+X)

x = COS(theta)
y = SIN(theta/2._dp)**2
r = TAN(theta/2._dp)

!        USE ASCENDING SERIES TO CALCULATE TWO VALUES OF P OR Q
!        FOR USE AS STARTING VALUES IN RECURRENCE RELATION.

pq2 = 0.0_dp
DO j=1,2
  ipq1 = 0
  IF(id == 2) GO TO 80
  
!        SERIES FOR P ( ID = 1, 3, OR 4 )
!        P(-MU,NU,X)=1./FACTORIAL(MU)*SQRT(((1.-X)/(1.+X))**MU)
!                *SUM(FROM 0 TO J0-1)A(J)*(.5-.5*X)**J
  
  ipq = 0
  pq = 1._dp
  a = 1._dp
  ia=0
  DO i=2,j0
    di = DBLE(i)
    a = a*y*(di-2._dp-nu)*(di-1._dp+nu)/((di-1._dp+dmu)*(di-1._dp))
    CALL xdadj(a, ia)
    IF(a == 0._dp) EXIT
    CALL xdadd(pq, ipq, a, ia, pq, ipq)
  END DO

  IF(mu <= 0) GO TO 90
  x2 = r
  x1 = pq
  k = mu
  DO i=1,k
    x1=x1*x2
    CALL xdadj(x1, ipq)
  END DO
  pq = x1/factmu
  ipq = ipq - IF
  CALL xdadj(pq, ipq)
  GO TO 90
  
!        Z=-LN(R)=.5*LN((1+X)/(1-X))
  
  80 z = -LOG(r)
  w = xdpsi(nu+1._dp, ipsik, ipsix)
  xs = 1._dp/SIN(theta)
  
!        SERIES SUMMATION FOR Q ( ID = 2 )
!        Q(0,NU,X) = SUM(FROM 0 TO J0-1) ((.5*LN((1+X)/(1-X))
!    + XDPSI(J+1,IPSIK,IPSIX)-XDPSI(NU+1,IPSIK,IPSIX)))*A(J)*(.5-.5*X)**J
  
!        Q(1,NU,X)=-SQRT(1./(1.-X**2))+SQRT((1-X)/(1+X))
!             *SUM(FROM 0 T0 J0-1)(-NU*(NU+1)/2*LN((1+X)/(1-X))
!                 +(J-NU)*(J+NU+1)/(2*(J+1))+NU*(NU+1)*
!     (XDPSI(NU+1,IPSIK,IPSIX)-XDPSI(J+1,IPSIK,IPSIX))*A(J)*(.5-.5*X)**J
  
!        NOTE, IN THIS LOOP K=J+1
  
  pq = 0._dp
  ipq = 0
  ia = 0
  a = 1._dp
  DO k=1,j0
    flok = DBLE(k)
    IF(k == 1) GO TO 81
    a = a*y*(flok-2._dp-nu)*(flok-1._dp+nu)/((flok-1._dp+dmu)*(flok-1._dp))
    CALL xdadj(a, ia)

    81 IF(mu >= 1) GO TO 83
    x1 = (xdpsi(flok, ipsik, ipsix) - w + z)*a
    ix1 = ia
    CALL xdadd(pq, ipq, x1, ix1, pq, ipq)
    CYCLE
    83 x1 = (nu*(nu+1._dp)*(z-w+xdpsi(flok, ipsik, ipsix)) + (nu-flok+1._dp)  &
            *(nu+flok)/(2._dp*k))*a
    ix1 = ia
    CALL xdadd(pq, ipq, x1, ix1, pq, ipq)
  END DO
  IF(mu >= 1) pq = -r*pq
  ixs = 0
  IF(mu >= 1) CALL xdadd(pq, ipq, -xs, ixs, pq, ipq)
  IF(j == 2) mu = -mu
  IF(j == 2) dmu = -dmu
  90 IF(j == 1) pq2 = pq
  IF(j == 1) ipq2 = ipq
  nu = nu + 1._dp
END DO
k = 0
IF(nu-1.5_dp < nu1) GO TO 120
k = k + 1
pqa(k) = pq2
ipqa(k) = ipq2
IF(nu > nu2+.5_dp) RETURN
120 pq1 = pq
ipq1 = ipq
IF(nu < nu1+.5_dp) GO TO 130
k = k + 1
pqa(k) = pq
ipqa(k) = ipq
IF(nu > nu2+.5_dp) RETURN

!        FORWARD NU-WISE RECURRENCE FOR F(MU,NU,X) FOR FIXED MU
!        USING
!        (NU+MU+1)*F(MU,NU,X)=(2.*NU+1)*F(MU,NU,X)-(NU-MU)*F(MU,NU-1,X)
!        WHERE F(MU,NU,X) MAY BE P(-MU,NU,X) OR IF MU IS REPLACED
!        BY -MU THEN F(MU,NU,X) MAY BE Q(MU,NU,X).
!        NOTE, IN THIS LOOP, NU=NU+1

130 x1 = (2._dp*nu-1._dp)/(nu+dmu)*x*pq1
x2 = (nu-1._dp-dmu)/(nu+dmu)*pq2
CALL xdadd(x1, ipq1, -x2, ipq2, pq, ipq)
CALL xdadj(pq,ipq)
nu = nu + 1._dp
pq2 = pq1
ipq2 = ipq1
GO TO 120

END SUBROUTINE xdpqnu


FUNCTION xdpsi(a, ipsik, ipsix) RESULT(fn_val)
!*** BEGIN PROLOGUE  XDPSI
!*** REFER TO  XDLEGF
!*** ROUTINES CALLED  (NONE)
!*** DATE WRITTEN   820728   (YYMMDD)
!*** REVISION DATE  871119   (YYMMDD)
!*** CATEGORY NO.  C7c
!*** KEYWORDS  PSI FUNCTION
!*** AUTHOR  SMITH, JOHN M. (NBS AND GEORGE MASON UNIVERSITY)
!*** PURPOSE  TO COMPUTE VALUES OF THE PSI FUNCTION FOR XDLEGF.
!        FUNCTION PSI(A,IPSIK,IPSIX) RETURNS THE VALUE OF THE DIGAMMA
!        ( OR PSI ) FUNCTION OF THE ARGUMENT A TO THE CALLING ROUTINE.
!*** REFERENCES  OLVER AND SMITH,J.COMPUT.PHYSICS,51(1983),N0.3,502-518.
!*** END PROLOGUE  XDPSI

REAL (dp), INTENT(IN)  :: a
INTEGER(i4), INTENT(IN)    :: ipsik
INTEGER(i4), INTENT(IN)    :: ipsix
REAL (dp)              :: fn_val

! Local variables

REAL (dp) :: b, c
INTEGER(i4)   :: i, k, k1, m, n

!        CNUM(I) AND CDENOM(I) ARE THE ( REDUCED ) NUMERATOR AND
!        2*I*DENOMINATOR RESPECTIVELY OF THE 2*I TH BERNOULLI NUMBER.

REAL (dp), PARAMETER :: cnum(12) = (/ 1._dp, -1._dp, 1._dp, -1._dp, 1._dp,  &
                                   -691._dp,  1._dp, -3617._dp, 43867._dp,  &
                                   -174611._dp, 77683._dp, -236364091._dp /)
REAL (dp), PARAMETER :: cdenom(12) = (/ 12._dp, 120._dp, 252._dp, 240._dp,  &
                                       132._dp, 32760._dp, 12._dp, 8160._dp, &
                                     14364._dp, 6600._dp, 276._dp, 65520._dp /)

!*** FIRST EXECUTABLE STATEMENT   XDPSI
n = MAX(0, ipsix-INT(a))
b = DBLE(n) + a
k1 = ipsik - 1

!        SERIES EXPANSION FOR A > IPSIX USING IPSIK-1 TERMS.

c = 0._dp
DO i=1,k1
  k = ipsik - i
  c = (c + cnum(k)/cdenom(k))/b**2
END DO
fn_val = LOG(b) - (c+.5_dp/b)
IF(n == 0) GO TO 20
b=0.

!        RECURRENCE FOR A <= IPSIX.

DO m=1,n
  b = b + 1._dp/(DBLE(n-m) + a)
END DO
fn_val = fn_val - b

20 RETURN
END FUNCTION xdpsi


SUBROUTINE xerror(messg, nmessg, nerr, level)
!*** BEGIN PROLOGUE  XERROR
!*** DATE WRITTEN   790801   (YYMMDD)
!*** REVISION DATE  820801   (YYMMDD)
!*** CATEGORY NO.  R3C
!*** KEYWORDS  ERROR,XERROR PACKAGE
!*** AUTHOR  JONES, R. E., (SNLA)
!*** PURPOSE  Processes an error (diagnostic) message.
!*** DESCRIPTION
!     Abstract
!        XERROR processes a diagnostic message, in a manner
!        determined by the value of LEVEL and the current value
!        of the library error control flag, KONTRL.
!        (See subroutine XSETF for details.)

!     Description of Parameters
!      --Input--
!        MESSG - the Hollerith message to be processed, containing
!                no more than 72 characters.
!        NMESSG- the actual number of characters in MESSG.
!        NERR  - the error number associated with this message.
!                NERR must not be zero.
!        LEVEL - error category.
!                =2 means this is an unconditionally fatal error.
!                =1 means this is a recoverable error.  (I.e., it is
!                   non-fatal if XSETF has been appropriately called.)
!                =0 means this is a warning message only.
!                =-1 means this is a warning message which is to be printed at
!                   most once, regardless of how many times this call is
!                   executed.

!     Examples
!        CALL XERROR('SMOOTH -- NUM WAS ZERO.',23,1,2)
!        CALL XERROR('INTEG  -- LESS THAN FULL ACCURACY ACHIEVED.', 43,2,1)
!        CALL XERROR('ROOTER -- ACTUAL ZERO OF F FOUND BEFORE INTERVAL &
!                    & FULLY COLLAPSED.',65,3,0)
!        CALL XERROR('EXP    -- UNDERFLOWS BEING SET TO ZERO.',39,1,-1)

!     Latest revision ---  19 MAR 1980
!     Written by Ron Jones, with SLATEC Common Math Library Subcommittee
!*** REFERENCES  JONES R.E., KAHANER D.K., "XERROR, THE SLATEC ERROR-HANDLING
!                PACKAGE", SAND82-0800, SANDIA LABORATORIES, 1982.
!*** ROUTINES CALLED  XERRWV
!*** END PROLOGUE  XERROR


CHARACTER (LEN=*), INTENT(IN)  :: messg
INTEGER(i4), INTENT(IN)            :: nmessg
INTEGER(i4), INTENT(IN)            :: nerr
INTEGER(i4), INTENT(IN)            :: level

!*** FIRST EXECUTABLE STATEMENT  XERROR
CALL xerrwv(messg, nmessg, nerr, level, 0, 0, 0, 0, 0., 0.)
RETURN
END SUBROUTINE xerror


SUBROUTINE xdadd(x, ix, y, iy, z, iz)
!*** BEGIN PROLOGUE  XDADD
!*** DATE WRITTEN   820712   (YYMMDD)
!*** REVISION DATE  831027   (YYMMDD)
!*** CATEGORY NO.  A3d
!*** KEYWORDS  EXTENDED-RANGE DOUBLE-PRECISION ARITHMETIC
!*** AUTHOR  LOZIER, DANIEL W. (NATIONAL BUREAU OF STANDARDS)
!           SMITH, JOHN M. (NBS AND GEORGE MASON UNIVERSITY)
!*** PURPOSE  TO PROVIDE DOUBLE-PRECISION FLOATING-POINT ARITHMETIC
!            WITH AN EXTENDED EXPONENT RANGE
!*** DESCRIPTION
!     REAL (dp) X, Y, Z
!     INTEGER IX, IY, IZ

!                  FORMS THE EXTENDED-RANGE SUM  (Z,IZ) = (X,IX) + (Y,IY).
!                  (Z,IZ) IS ADJUSTED BEFORE RETURNING.
!                  THE INPUT OPERANDS NEED NOT BE IN ADJUSTED FORM, BUT THEIR
!                  PRINCIPAL PARTS MUST SATISFY
!                  RADIX**(-2L) <= ABS(X) <= RADIX**(2L),
!                  RADIX**(-2L) <= ABS(Y) <= RADIX**(2L).

!*** REFERENCES  (PROGRAM LISTING FOR XDSET)
!*** ROUTINES CALLED  XDADJ
!*** COMMON BLOCKS    XDBLK2
!*** END PROLOGUE  XDADD

REAL (dp), INTENT(IN)   :: x
INTEGER(i4), INTENT(IN)     :: ix
REAL (dp), INTENT(IN)   :: y
INTEGER(i4), INTENT(IN)     :: iy
REAL (dp), INTENT(OUT)  :: z
INTEGER(i4), INTENT(OUT)    :: iz

! Local variables

INTEGER(i4)   :: i, is, i1, i2, j
REAL (dp) :: s, t

!   THE CONDITIONS IMPOSED ON L1 AND KMAX BY THIS SUBROUTINE ARE

!     (1) 1 < L1 <= 0.5_dp*LOGR(0.5_dp*DZERO)

!     (2) NRADPL < L1 <= KMAX/6

!     (3) KMAX <= (2**NBITS - 4*L1 - 1)/2

! THESE CONDITIONS MUST BE MET BY APPROPRIATE CODING IN SUBROUTINE XDSET.

!*** FIRST EXECUTABLE STATEMENT  XDADD
IF (x /= 0.0_dp) GO TO 10
z = y
iz = iy
GO TO 220
10 IF (y /= 0.0_dp) GO TO 20
z = x
iz = ix
GO TO 220

20 IF (ix >= 0 .AND. iy >= 0) GO TO 40
IF (ix < 0 .AND. iy < 0) GO TO 40
IF (ABS(ix) <= 6*l1 .AND. ABS(iy) <= 6*l1) GO TO 40
IF (ix >= 0) GO TO 30
z = y
iz = iy
GO TO 220

30 z = x
iz = ix
GO TO 220
40 i = ix - iy
IF (i < 0) THEN
  GO TO 80
ELSE IF (i > 0) THEN
  GO TO 90
END IF

IF (ABS(x) > 1.0_dp .AND. ABS(y) > 1.0_dp) GO TO 60
IF (ABS(x) < 1.0_dp .AND. ABS(y) < 1.0_dp) GO TO 70
z = x + y
iz = ix
GO TO 220

60 s = x/radixl
t = y/radixl
z = s + t
iz = ix + l1
GO TO 220

70 s = x*radixl
t = y*radixl
z = s + t
iz = ix - l1
GO TO 220
80 s = y
is = iy
t = x
GO TO 100
90 s = x
is = ix
t = y

! AT THIS POINT, THE ONE OF (X,IX) OR (Y,IY) THAT HAS THE LARGER AUXILIARY
! INDEX IS STORED IN (S,IS).   THE PRINCIPAL PART OF THE OTHER INPUT IS STORED
! IN T.

100 i1 = ABS(i)/l1
i2 = MOD(ABS(i), l1)
IF (ABS(t) >= radixl) GO TO 130
IF (ABS(t) >= 1.0_dp) GO TO 120
IF (radixl*ABS(t) >= 1.0_dp) GO TO 110
j = i1 + 1
t = t*radix0**(l1-i2)
GO TO 140
110 j = i1
t = t*radix0**(-i2)
GO TO 140
120 j = i1 - 1
IF (j < 0) GO TO 110
t = t*radix0**(-i2)/radixl
GO TO 140
130 j = i1 - 2
IF (j < 0) GO TO 120
t = t*radix0**(-i2)/rad2l

! AT THIS POINT, SOME OR ALL OF THE DIFFERENCE IN THE AUXILIARY INDICES HAS
! BEEN USED TO EFFECT A LEFT SHIFT OF T.   THE SHIFTED VALUE OF T SATISFIES

!       radix0**(-2*L1) <= ABS(T) <= 1.0_dp

! AND, IF J=0, NO FURTHER SHIFTING REMAINS TO BE DONE.

140 IF (j == 0) GO TO 190
IF (ABS(s) >= radixl .OR. j > 3) GO TO 150
IF (ABS(s) >= 1.0_dp) THEN
  IF (j < 0) GO TO 180
  GO TO 150
END IF

IF (radixl*ABS(s) >= 1.0_dp) THEN
  SELECT CASE (j)
    CASE (:-1)
      GO TO 180
    CASE (0)
      GO TO 170
    CASE (1:)
      GO TO 150
  END SELECT
END IF

SELECT CASE (j)
  CASE (:-1)
    GO TO 180
  CASE (0)
    GO TO 170
  CASE (1:)
    GO TO 160
END SELECT

150 z = s
iz = is
GO TO 220

160 s = s*radixl
170 s = s*radixl
180 s = s*radixl

! AT THIS POINT, THE REMAINING DIFFERENCE IN THE AUXILIARY INDICES HAS BEEN
! USED TO EFFECT A RIGHT SHIFT OF S.  IF THE SHIFTED VALUE OF S WOULD HAVE
! EXCEEDED radix0**L, THEN (S,IS) IS RETURNED AS THE VALUE OF THE SUM.

190 IF (ABS(s) > 1.0_dp .AND. ABS(t) > 1.0_dp) GO TO 200
IF (ABS(s) < 1.0_dp .AND. ABS(t) < 1.0_dp) GO TO 210
z = s + t
iz = is - j*l1
GO TO 220

200 s = s/radixl
t = t/radixl
z = s + t
iz = is - j*l1 + l1
GO TO 220
210 s = s*radixl
t = t*radixl
z = s + t
iz = is - j*l1 - l1
220 CALL xdadj(z, iz)

RETURN
END SUBROUTINE xdadd


SUBROUTINE xerrwv(messg, nmessg, nerr, level, ni, i1, i2, nr, r1, r2)
!*** BEGIN PROLOGUE  XERRWV
!*** DATE WRITTEN   800319   (YYMMDD)
!*** REVISION DATE  820801   (YYMMDD)
!*** CATEGORY NO.  R3C
!*** KEYWORDS  ERROR,XERROR PACKAGE
!*** AUTHOR  JONES, R. E., (SNLA)
!*** PURPOSE  Processes error message allowing 2 integer and two real
!            values to be included in the message.
!*** DESCRIPTION
!     Abstract
!        XERRWV processes a diagnostic message, in a manner
!        determined by the value of LEVEL and the current value
!        of the library error control flag, KONTRL.
!        (See subroutine XSETF for details.)
!        In addition, up to two integer values and two real
!        values may be printed along with the message.

!     Description of Parameters
!      --Input--
!        MESSG - the Hollerith message to be processed.
!        NMESSG- the actual number of characters in MESSG.
!        NERR  - the error number associated with this message.
!                NERR must not be zero.
!        LEVEL - error category.
!                =2 means this is an unconditionally fatal error.
!                =1 means this is a recoverable error.  (I.e., it is
!                   non-fatal if XSETF has been appropriately called.)
!                =0 means this is a warning message only.
!                =-1 means this is a warning message which is to be
!                   printed at most once, regardless of how many
!                   times this call is executed.
!        NI    - number of integer values to be printed. (0 to 2)
!        I1    - first integer value.
!        I2    - second integer value.
!        NR    - number of real values to be printed. (0 to 2)
!        R1    - first real value.
!        R2    - second real value.

!     Examples
!        CALL XERRWV('SMOOTH -- NUM (=I1) WAS ZERO.',29,1,2,
!    1   1,NUM,0,0,0.,0.)
!        CALL XERRWV('QUADXY -- REQUESTED ERROR (R1) LESS THAN MINIMUM (
!    1R2).,54,77,1,0,0,0,2,ERRREQ,ERRMIN)

!     Latest revision ---  19 MAR 1980
!     Written by Ron Jones, with SLATEC Common Math Library Subcommittee
!*** REFERENCES  JONES R.E., KAHANER D.K., "XERROR, THE SLATEC ERROR-HANDLING
!                PACKAGE", SAND82-0800, SANDIA LABORATORIES, 1982.
!*** ROUTINES CALLED  FDUMP,I1MACH,J4SAVE,XERABT,XERCTL,XERPRT,XERSAV,
!                    XGETUA
!*** END PROLOGUE  XERRWV

CHARACTER (LEN=*), INTENT(IN)  :: messg
INTEGER(i4), INTENT(IN)            :: nmessg
INTEGER(i4), INTENT(IN)            :: nerr
INTEGER(i4), INTENT(IN)            :: level
INTEGER(i4), INTENT(IN)            :: ni
INTEGER(i4), INTENT(IN)            :: i1
INTEGER(i4), INTENT(IN)            :: i2
INTEGER(i4), INTENT(IN)            :: nr
REAL, INTENT(IN)               :: r1
REAL, INTENT(IN)               :: r2

CHARACTER (LEN=37) :: FORM
INTEGER(i4)            :: i, ifatal, isizef, isizei, iunit, kount, kunit,  &
                      lerr, llevel, lun(5), lkntrl, maxmes, mkntrl, nunit

!     GET FLAGS
!*** FIRST EXECUTABLE STATEMENT  XERRWV
lkntrl = j4save(2, 0, .false.)
maxmes = j4save(4, 0, .false.)
!     CHECK FOR VALID INPUT
IF (nmessg > 0 .AND. nerr /= 0 .AND. level >= -1 .AND. level <= 2) GO TO 10
IF (lkntrl > 0) CALL xerprt('FATAL ERROR IN...')
CALL xerprt('XERROR -- INVALID INPUT')
IF (lkntrl > 0) CALL fdump()
IF (lkntrl > 0) CALL xerprt('JOB ABORT DUE TO FATAL ERROR.')
IF (lkntrl > 0) CALL xersav(' ', 0, 0, 0, ifatal)
RETURN

!     RECORD MESSAGE
10 ifatal = j4save(1, nerr, .true.)
CALL xersav(messg, nmessg, nerr, level, kount)
!     RESET TO ORIGINAL VALUES
lerr = nerr
llevel = level
lkntrl = MAX(-2, MIN(2, lkntrl))
mkntrl = ABS(lkntrl)
!     DECIDE WHETHER TO PRINT MESSAGE
IF (llevel < 2 .AND. lkntrl == 0) GO TO 100
IF ( (llevel == -1 .AND. kount > MIN(1,maxmes))  &
     .OR. (llevel == 0 .AND. kount > maxmes)  &
     .OR. (llevel == 1 .AND. kount > maxmes .AND. mkntrl == 1)  &
     .OR. (llevel == 2 .AND. kount > MAX(1,maxmes)) ) GO TO 100
IF (lkntrl <= 0) GO TO 20
CALL xerprt(' ')
!           INTRODUCTION
IF (llevel == (-1)) CALL xerprt  &
    ('WARNING MESSAGE...THIS MESSAGE WILL ONLY BE PRINTED ONCE.')
IF (llevel == 0) CALL xerprt('WARNING IN...')
IF (llevel == 1) CALL xerprt ('RECOVERABLE ERROR IN...')
IF (llevel == 2) CALL xerprt('FATAL ERROR IN...')

!        MESSAGE
20 CALL xerprt(messg)
CALL xgetua(lun, nunit)
isizei = LOG10(REAL(i1mach(9))) + 1.0
isizef = LOG10(REAL(i1mach(10))**i1mach(11)) + 1.0
DO kunit=1,nunit
  iunit = lun(kunit)
  IF (iunit == 0) iunit = i1mach(4)
  DO i=1,MIN(ni,2)
    WRITE (FORM, 21) i, isizei
    21 FORMAT ('(       IN ABOVE MESSAGE, I', i1, '=,I', i2, ')   ')
    IF (i == 1) WRITE (iunit,FORM) i1
    IF (i == 2) WRITE (iunit,FORM) i2
  END DO
  DO i=1,MIN(nr,2)
    WRITE (FORM,23) i, isizef+10, isizef
    23 FORMAT ('(       IN ABOVE MESSAGE, R', i1, '=,E', i2, '.', i2, ')')
    IF (i == 1) WRITE (iunit,FORM) r1
    IF (i == 2) WRITE (iunit,FORM) r2
  END DO
  IF (lkntrl <= 0) CYCLE
!              ERROR NUMBER
  WRITE (iunit,30) lerr
  30 FORMAT (' error NUMBER = ',i10)
END DO
!        TRACE-BACK
IF (lkntrl > 0) CALL fdump()

100 ifatal = 0
IF (llevel == 2 .OR. (llevel == 1 .AND. mkntrl == 2)) ifatal = 1
!     QUIT HERE IF MESSAGE IS NOT FATAL
IF (ifatal <= 0) RETURN
IF (lkntrl <= 0 .OR. kount > MAX(1,maxmes)) GO TO 120
!        PRINT REASON FOR ABORT
IF (llevel == 1) CALL xerprt ('JOB ABORT DUE TO UNRECOVERED ERROR.')
IF (llevel == 2) CALL xerprt ('JOB ABORT DUE TO FATAL ERROR.')
!        PRINT ERROR SUMMARY
CALL xersav(' ', -1, 0, 0, ifatal)

!     ABORT
120 RETURN
END SUBROUTINE xerrwv


SUBROUTINE fdump()
!*** BEGIN PROLOGUE  FDUMP
!*** DATE WRITTEN   790801   (YYMMDD)
!*** REVISION DATE  820801   (YYMMDD)
!*** CATEGORY NO.  Z
!*** KEYWORDS  ERROR,XERROR PACKAGE
!*** AUTHOR  JONES, R. E., (SNLA)
!*** PURPOSE  Symbolic dump (should be locally written).
!*** DESCRIPTION
!        *** Note*** Machine Dependent Routine
!        FDUMP is intended to be replaced by a locally written
!        version which produces a symbolic dump.  Failing this,
!        it should be replaced by a version which prints the
!        subprogram nesting list.  Note that this dump must be
!        printed on each of up to five files, as indicated by the
!        XGETUA routine.  See XSETUA and XGETUA for details.

!     Written by Ron Jones, with SLATEC Common Math Library Subcommittee
!     Latest revision ---  23 May 1979
!*** ROUTINES CALLED  (NONE)
!*** END PROLOGUE  FDUMP
!*** FIRST EXECUTABLE STATEMENT  FDUMP
RETURN
END SUBROUTINE fdump


FUNCTION j4save(iwhich, ivalue, iset) RESULT(ival)
!*** BEGIN PROLOGUE  J4SAVE
!*** REFER TO  XERROR
!     Abstract
!        J4SAVE saves and recalls several global variables needed
!        by the library error handling routines.

!     Description of Parameters
!      --Input--
!        IWHICH - Index of item desired.
!                 = 1 Refers to current error number.
!                 = 2 Refers to current error control flag.
!                 = 3 Refers to current unit number to which error
!                     messages are to be sent.  (0 means use standard.)
!                 = 4 Refers to the maximum number of times any
!                     message is to be printed (as set by XERMAX).
!                 = 5 Refers to the total number of units to which
!                     each error message is to be written.
!                 = 6 Refers to the 2nd unit for error messages
!                 = 7 Refers to the 3rd unit for error messages
!                 = 8 Refers to the 4th unit for error messages
!                 = 9 Refers to the 5th unit for error messages
!        IVALUE - The value to be set for the IWHICH-th parameter,
!                 if ISET is .TRUE. .
!        ISET   - If ISET=.TRUE., the IWHICH-th parameter will BE
!                 given the value, IVALUE.  If ISET=.FALSE., the
!                 IWHICH-th parameter will be unchanged, and IVALUE
!                 is a dummy parameter.
!      --Output--
!        The (old) value of the IWHICH-th parameter will be returned
!        in the function value, J4SAVE.

!    Written by Ron Jones, with SLATEC Common Math Library Subcommittee
!    Adapted from Bell Laboratories PORT Library Error Handler
!    Latest revision ---  23 MAY 1979
!*** REFERENCES  JONES R.E., KAHANER D.K., "XERROR, THE SLATEC ERROR-HANDLING
!                PACKAGE", SAND82-0800, SANDIA LABORATORIES, 1982.
!*** ROUTINES CALLED  (NONE)
!*** END PROLOGUE  J4SAVE

INTEGER(i4), INTENT(IN)  :: iwhich
INTEGER(i4), INTENT(IN)  :: ivalue
LOGICAL, INTENT(IN)  :: iset
INTEGER(i4)              :: ival

INTEGER(i4), SAVE :: iparam(9) = (/ 0, 2, 0, 10, 1, 0, 0, 0, 0 /)

!*** FIRST EXECUTABLE STATEMENT  J4SAVE
ival = iparam(iwhich)
IF (iset) iparam(iwhich) = ivalue

RETURN
END FUNCTION j4save


SUBROUTINE xersav(messg, nmessg, nerr, level, icount)
!*** BEGIN PROLOGUE  XERSAV
!*** DATE WRITTEN   800319   (YYMMDD)
!*** REVISION DATE  820801   (YYMMDD)
!*** CATEGORY NO.  Z
!*** KEYWORDS  ERROR,XERROR PACKAGE
!*** AUTHOR  JONES, R. E., (SNLA)
!*** PURPOSE  Records that an error occurred.
!*** DESCRIPTION
!     Abstract
!        Record that this error occurred.

!     Description of Parameters
!     --Input--
!       MESSG, NMESSG, NERR, LEVEL are as in XERROR, except that when NMESSG=0
!       the tables will be dumped and cleared, and when NMESSG is less than
!       zero the tables will be dumped and not cleared.
!     --Output--
!       ICOUNT will be the number of times this message has been seen, or zero
!       if the table has overflowed and does not contain this message
!       specifically.  When NMESSG=0, ICOUNT will not be altered.

!     Written by Ron Jones, with SLATEC Common Math Library Subcommittee
!     Latest revision ---  19 Mar 1980
!*** REFERENCES  JONES R.E., KAHANER D.K., "XERROR, THE SLATEC ERROR-HANDLING
!                PACKAGE", SAND82-0800, SANDIA LABORATORIES, 1982.
!*** ROUTINES CALLED  I1MACH,S88FMT,XGETUA
!*** END PROLOGUE  XERSAV

CHARACTER (LEN=*), INTENT(IN)  :: messg
INTEGER(i4), INTENT(IN)            :: nmessg
INTEGER(i4), INTENT(IN)            :: nerr
INTEGER(i4), INTENT(IN)            :: level
INTEGER(i4), INTENT(OUT)           :: icount

! Local variables

INTEGER :: i, ii, iunit, kunit, lun(5), nunit

CHARACTER (LEN=20), SAVE :: mestab(10), mes
INTEGER, SAVE            :: nertab(10), levtab(10)

!     NEXT TWO DATA STATEMENTS ARE NECESSARY TO PROVIDE A BLANK
!     ERROR TABLE INITIALLY

INTEGER, SAVE :: kount(10) = (/ (0,i=1,10) /), kountx = 0

!*** FIRST EXECUTABLE STATEMENT  XERSAV
IF (nmessg > 0) GO TO 80
!     DUMP THE TABLE
IF (kount(1) == 0) RETURN
!        PRINT TO EACH UNIT
CALL xgetua(lun, nunit)
DO kunit=1,nunit
  iunit = lun(kunit)
  IF (iunit == 0) iunit = i1mach(4)
!           PRINT TABLE HEADER
  WRITE (iunit,10)
  10 FORMAT (/'          error message summary'/  &
      ' message start             nerr     level     count')
!           PRINT BODY OF TABLE
  DO i=1,10
    IF (kount(i) == 0) EXIT
    WRITE (iunit,15) mestab(i), nertab(i), levtab(i), kount(i)
    15 FORMAT (' ', a20, 3I10)
  END DO

!           PRINT NUMBER OF OTHER ERRORS
  IF (kountx /= 0) WRITE (iunit, 40) kountx
  40 FORMAT (/' OTHER errors NOT individually tabulated = ', i10)
  WRITE (iunit, 50)
  50 FORMAT (' ')
END DO
IF (nmessg < 0) RETURN
!        CLEAR THE ERROR TABLES
kount(1:10) = 0
kountx = 0
RETURN

!     PROCESS A MESSAGE...
!     SEARCH FOR THIS MESSG, OR ELSE AN EMPTY SLOT FOR THIS MESSG,
!     OR ELSE DETERMINE THAT THE ERROR TABLE IS FULL.
80 mes = messg
DO i=1,10
  ii = i
  IF (kount(i) == 0) GO TO 110
  IF (mes /= mestab(i)) CYCLE
  IF (nerr /= nertab(i)) CYCLE
  IF (level /= levtab(i)) CYCLE
  GO TO 100
END DO
!     THREE POSSIBLE CASES...
!     TABLE IS FULL
kountx = kountx+1
icount = 1
RETURN
!     MESSAGE FOUND IN TABLE
100 kount(ii) = kount(ii) + 1
icount = kount(ii)
RETURN
!     EMPTY SLOT FOUND FOR NEW MESSAGE
110 mestab(ii) = mes
nertab(ii) = nerr
levtab(ii) = level
kount(ii)  = 1
icount = 1

RETURN
END SUBROUTINE xersav


SUBROUTINE xerprt(messg)
!*** BEGIN PROLOGUE  XERPRT
!*** DATE WRITTEN   790801   (YYMMDD)
!*** REVISION DATE  820801   (YYMMDD)
!*** CATEGORY NO.  Z
!*** KEYWORDS  ERROR,XERROR PACKAGE
!*** AUTHOR  JONES, R. E., (SNLA)
!*** PURPOSE  Prints error messages.
!*** DESCRIPTION
!     Abstract
!        Print the Hollerith message in MESSG, of length NMESSG,
!        on each file indicated by XGETUA.
!     Latest revision ---  19 MAR 1980
!*** REFERENCES  JONES R.E., KAHANER D.K., "XERROR, THE SLATEC ERROR-
!                 HANDLING PACKAGE", SAND82-0800, SANDIA LABORATORIES,
!                 1982.
!*** ROUTINES CALLED  I1MACH,S88FMT,XGETUA
!*** END PROLOGUE  XERPRT

! N.B. Argument NMESSG has been removed.

CHARACTER (LEN=*), INTENT(IN)  :: messg

! Local variables

INTEGER :: ichar, iunit, kunit, last, lenmes, lun(5), nunit

!     OBTAIN UNIT NUMBERS AND WRITE LINE TO EACH UNIT
!*** FIRST EXECUTABLE STATEMENT  XERPRT
CALL xgetua(lun, nunit)
lenmes = LEN_TRIM(messg)
DO kunit=1,nunit
  iunit = lun(kunit)
  IF (iunit == 0) iunit = i1mach(4)
  DO ICHAR=1,lenmes,72
    last = MIN(ICHAR+71 , lenmes)
    WRITE (iunit, '(1X,A)') messg(ICHAR:last)
  END DO
END DO

RETURN
END SUBROUTINE xerprt


SUBROUTINE xdadj(x, ix)
!*** BEGIN PROLOGUE  XDADJ
!*** DATE WRITTEN   820712   (YYMMDD)
!*** REVISION DATE  831027   (YYMMDD)
!*** CATEGORY NO.  A3d
!*** KEYWORDS  EXTENDED-RANGE DOUBLE-PRECISION ARITHMETIC
!*** AUTHOR  LOZIER, DANIEL W. (NATIONAL BUREAU OF STANDARDS)
!           SMITH, JOHN M. (NBS AND GEORGE MASON UNIVERSITY)
!*** PURPOSE  TO PROVIDE DOUBLE-PRECISION FLOATING-POINT ARITHMETIC
!            WITH AN EXTENDED EXPONENT RANGE
!*** DESCRIPTION
!     REAL (dp) X
!     INTEGER IX

!                  TRANSFORMS (X,IX) SO THAT
!                  RADIX**(-L) <= ABS(X) < RADIX**L.
!                  ON MOST COMPUTERS THIS TRANSFORMATION DOES
!                  NOT CHANGE THE MANTISSA OF X PROVIDED RADIX IS
!                  THE NUMBER BASE OF DOUBLE-PRECISION ARITHMETIC.

!*** REFERENCES  (PROGRAM LISTING FOR XDSET)
!*** ROUTINES CALLED  XERROR
!*** COMMON BLOCKS    XDBLK2
!*** END PROLOGUE  XDADJ

REAL (dp), INTENT(OUT)  :: x
INTEGER(i4), INTENT(OUT)    :: ix

!   THE CONDITION IMPOSED ON L AND KMAX BY THIS SUBROUTINE IS
!     2*L <= KMAX

! THIS CONDITION MUST BE MET BY APPROPRIATE CODING IN SUBROUTINE XDSET.

!*** FIRST EXECUTABLE STATEMENT  XDADJ
IF (x == 0.0_dp) GO TO 50
IF (ABS(x) >= 1.0_dp) GO TO 20
IF (radixl*ABS(x) >= 1.0_dp) GO TO 60
x = x*rad2l
IF (ix < 0) GO TO 10
ix = ix - l2
GO TO 70

10 IF (ix < -kmax+l2) GO TO 40
ix = ix - l2
GO TO 70

20 IF (ABS(x) < radixl) GO TO 60
x = x/rad2l
IF (ix > 0) GO TO 30
ix = ix + l2
GO TO 70

30 IF (ix > kmax-l2) GO TO 40
ix = ix + l2
GO TO 70

40 CALL xerror('Err in XDADJ...overflow in auxiliary index', 42, 1, 1)
RETURN

50 ix = 0
60 IF (ABS(ix) > kmax) GO TO 40

70 RETURN
END SUBROUTINE xdadj


SUBROUTINE xgetua(iunita, n)
!*** BEGIN PROLOGUE  XGETUA
!*** DATE WRITTEN   790801   (YYMMDD)
!*** REVISION DATE  820801   (YYMMDD)
!*** CATEGORY NO.  R3C
!*** KEYWORDS  ERROR,XERROR PACKAGE
!*** AUTHOR  JONES, R. E., (SNLA)
!*** PURPOSE  Returns unit number(s) to which error messages are being
!            sent.
!*** DESCRIPTION
!     Abstract
!        XGETUA may be called to determine the unit number or numbers
!        to which error messages are being sent.
!        These unit numbers may have been set by a call to XSETUN,
!        or a call to XSETUA, or may be a default value.

!     Description of Parameters
!      --Output--
!        IUNIT - an array of one to five unit numbers, depending on the value
!                of N.  A value of zero refers to the default unit, as defined
!                by the I1MACH machine constant routine.
!                Only IUNIT(1),...,IUNIT(N) are defined by XGETUA.  The values
!                of IUNIT(N+1),..., IUNIT(5) are not defined (for N < 5) or
!                altered in any way by XGETUA.
!        N     - the number of units to which copies of the error messages are
!                being sent.   N will be in the range from 1 to 5.

!     Latest revision ---  19 MAR 1980
!     Written by Ron Jones, with SLATEC Common Math Library Subcommittee
!*** REFERENCES  JONES R.E., KAHANER D.K., "XERROR, THE SLATEC ERROR-HANDLING
!                PACKAGE", SAND82-0800, SANDIA LABORATORIES, 1982.
!*** ROUTINES CALLED  J4SAVE
!*** END PROLOGUE  XGETUA


INTEGER(i4), INTENT(OUT)  :: iunita(:)
INTEGER(i4), INTENT(OUT)  :: n

! Local variables

INTEGER :: i, index

!*** FIRST EXECUTABLE STATEMENT  XGETUA
n = j4save(5, 0, .false.)
DO i=1,n
  INDEX = i + 4
  IF (i == 1) INDEX = 3
  iunita(i) = j4save(INDEX, 0, .false.)
END DO

RETURN
END SUBROUTINE xgetua


SUBROUTINE xdred(x, ix)
!*** BEGIN PROLOGUE  XDRED
!*** DATE WRITTEN   820712   (YYMMDD)
!*** REVISION DATE  831027   (YYMMDD)
!*** CATEGORY NO.  A3d
!*** KEYWORDS  EXTENDED-RANGE DOUBLE-PRECISION ARITHMETIC
!*** AUTHOR  LOZIER, DANIEL W. (NATIONAL BUREAU OF STANDARDS)
!           SMITH, JOHN M. (NBS AND GEORGE MASON UNIVERSITY)
!*** PURPOSE  TO PROVIDE DOUBLE-PRECISION FLOATING-POINT ARITHMETIC
!            WITH AN EXTENDED EXPONENT RANGE
!*** DESCRIPTION
!     REAL (dp) X
!     INTEGER IX

!          IF
!          RADIX**(-2L) <= (ABS(X),IX) <= RADIX**(2L)
!          THEN XDRED TRANSFORMS (X,IX) SO THAT IX=0.
!          IF (X,IX) IS OUTSIDE THE ABOVE RANGE, THEN XDRED TAKES NO ACTION.
!          THIS SUBROUTINE IS USEFUL IF THE RESULTS OF EXTENDED-RANGE
!          CALCULATIONS ARE TO BE USED IN SUBSEQUENT ORDINARY
!          DOUBLE-PRECISION CALCULATIONS.

!*** REFERENCES  (PROGRAM LISTING FOR XDSET)
!*** ROUTINES CALLED  (NONE)
!*** COMMON BLOCKS    XDBLK2
!*** END PROLOGUE  XDRED

REAL (dp), INTENT(OUT)  :: x
INTEGER(i4), INTENT(OUT)    :: ix

! Local variables

REAL (dp) :: xa
INTEGER   :: i, ixa, ixa1, ixa2

!*** FIRST EXECUTABLE STATEMENT  XDRED
IF (x == 0.0_dp) GO TO 90
xa = ABS(x)
IF (ix == 0) GO TO 70
ixa = ABS(ix)
ixa1 = ixa/l2
ixa2 = MOD(ixa, l2)
IF (ix > 0) GO TO 40

DO
  IF (xa > 1.0_dp) EXIT
  xa = xa*rad2l
  ixa1 = ixa1 + 1
END DO
xa = xa/radix0**ixa2
IF (ixa1 == 0) GO TO 70
DO i=1,ixa1
  IF (xa < 1.0_dp) GO TO 100
  xa = xa/rad2l
END DO
GO TO 70

40 IF (xa < 1.0_dp) GO TO 50
xa = xa/rad2l
ixa1 = ixa1 + 1
GO TO 40

50 xa = xa*radix0**ixa2
DO i=1,ixa1
  IF (xa > 1.0_dp) GO TO 100
  xa = xa*rad2l
END DO

70 IF (xa > rad2l) GO TO 100
IF (xa > 1.0_dp) GO TO 80
IF (rad2l*xa < 1.0_dp) GO TO 100
80 x = SIGN(xa, x)
90 ix = 0

100 RETURN
END SUBROUTINE xdred

END MODULE Legendre_funcs
