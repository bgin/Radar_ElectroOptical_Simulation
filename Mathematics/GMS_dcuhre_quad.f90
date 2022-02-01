MODULE dcuhre_quadrature
  use mod_kinds, only : i4, dp
IMPLICIT NONE

!INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(15, 60)

! This version, which uses the ELF90 subset of Fortran 90, produced by
! Alan Miller:  alan @ users.bigpond.net.au
! URL: www.bigpond.net.au/amiller/

! Latest revision - 29 August 1997

!#if 0


! This is a summary of the routines in this file. 
!C
!C     DTEST1   Simple test driver for DCUHRE.
!C              Sample output from a SUN 3/50 is included.
!C     DCUHRE   Main Integrator. DCUHRE calls DCHHRE and DADHRE.
!C     DCHHRE   Checks the input to DCUHRE.
!C     DADHRE   The adaptive integration routine. 
!C              DADHRE calls DTRHRE, DINHRE and DRLHRE.
!C     DTRHRE   Maintaines the heap of subregions.
!C     DINHRE   Computes weights and abscissas of the integration
!C              rule. DINHRE calls D132RE, D112RE, D09HRE and D07HRE.
!C     D132RE   Computes weights and abscissas for a 2-dimensional
!C              rule of degree 13.
!C     D113RE   Computes weights and abscissas for a 3-dimensional 
!C              rule of degree 11.
!C     D09HRE   Computes weights and abscissas for a degree 9 rule.
!C     D07HRE   Computes weights and abscissas for a degree 7 rule.
!C     DRLHRE   Computes estimates of integral and error over
!C              subregions.
!C     DFSHRE   Computes fully symmetric sums of function evaluations.
!C
!C   DTEST1 is a simple test driver for DCUHRE.
!C
!C   Output produced on a SUN 3/50.
!c
!C       DCUHRE TEST RESULTS
!C
!C    FTEST CALLS = 3549, IFAIL =  0
!C   N   ESTIMATED ERROR    INTEGRAL
!C   1       0.000000       0.138508
!C   2       0.000000       0.063695
!C   3       0.000009       0.058618
!C   4       0.000000       0.054070
!C   5       0.000000       0.050056
!C   6       0.000000       0.046546
!C
!      PROGRAM DTEST1
!      EXTERNAL FTEST
!      integer(kind=4) ::   KEY, N, NF, NDIM, MINCLS, MAXCLS, IFAIL, NEVAL, NW
!      PARAMETER (NDIM = 5, NW = 5000, NF = NDIM+1)
!      real(kind=8) ::  A(NDIM), B(NDIM), WRKSTR(NW)
!      real(kind=8) ::  ABSEST(NF), FINEST(NF), ABSREQ, RELREQ
!      DO 10 N = 1,NDIM
!         A(N) = 0
!         B(N) = 1
!   10 CONTINUE
!      MINCLS = 0
!      MAXCLS = 10000
!!      KEY = 0
!      ABSREQ = 0
!      RELREQ = 1E-3
!      CALL DCUHRE(NDIM, NF, A, B, MINCLS, MAXCLS, FTEST, ABSREQ, RELREQ,
!     * KEY, NW, 0, FINEST, ABSEST, NEVAL, IFAIL, WRKSTR)
!      PRINT 9999, NEVAL, IFAIL
! 9999 FORMAT (8X, 'DCUHRE TEST RESULTS', //'     FTEST CALLS = ', I4,
!     * ', IFAIL = ', I2, /'    N   ESTIMATED ERROR    INTEGRAL')
!      DO 20 N = 1,NF
!         PRINT 9998, N, ABSEST(N), FINEST(N)
! 9998    FORMAT (3X, I2, 2F15.6)
!   20 CONTINUE
!      END
!      SUBROUTINE FTEST(NDIM, Z, NFUN, F)
!      integer(kind=4) ::   N, NDIM, NFUN
!      real(kind=8) ::  Z(NDIM), F(NFUN), SUM
!      SUM = 0
!      DO 10 N = 1,NDIM
!         SUM = SUM + N*Z(N)**2
!   10 CONTINUE
!      F(1) = EXP(-SUM/2)
!      DO 20 N = 1,NDIM
!         F(N+1) = Z(N)*F(1)
!   20 CONTINUE
!      END




!#endif

CONTAINS


SUBROUTINE dcuhre(ndim, numfun, a, b, minpts, maxpts, funsub, epsabs,  &
     epsrel, key, restar, result, abserr, neval, ifail, nsub)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: DCUHRE
      !DIR$ OPTIMIZE : 3
      !DIR$ CODE_ALIGN : 32 :: DCUHRE
#endif
!***BEGIN PROLOGUE DCUHRE
!***DATE WRITTEN   900116   (YYMMDD)
!***REVISION DATE  900116   (YYMMDD)
!***CATEGORY NO. H2B1A1
!***AUTHOR
!        Jarle Berntsen, The Computing Centre,
!        University of Bergen, Thormohlens gt. 55,
!        N-5008 Bergen, Norway
!        Phone..  47-5-544055
!        Email..  jarle@eik.ii.uib.no
!        Terje O. Espelid, Department of Informatics,
!        University of Bergen, Thormohlens gt. 55,
!        N-5008 Bergen, Norway
!        Phone..  47-5-544180
!        Email..  terje@eik.ii.uib.no
!        Alan Genz, Computer Science Department, Washington State
!        University, Pullman, WA 99163-2752, USA
!        Email..  acg@eecs.wsu.edu
!***KEYWORDS automatic multidimensional integrator,
!        n-dimensional hyper-rectangles,
!        general purpose, global adaptive
!***PURPOSE  The routine calculates an approximation to a given
!            vector of definite integrals

!      B(1) B(2)     B(NDIM)
!     I    I    ... I       (F ,F ,...,F      ) DX(NDIM)...DX(2)DX(1),
!      A(1) A(2)     A(NDIM)  1  2      NUMFUN

!       where F = F (X ,X ,...,X    ), I = 1,2,...,NUMFUN.
!              I   I  1  2      NDIM

!        hopefully satisfying for each component of I the following
!        claim for accuracy:
!        ABS(I(K)-RESULT(K)).LE.MAX(EPSABS,EPSREL*ABS(I(K)))
!***DESCRIPTION Computation of integrals over hyper-rectangular regions.
!        DCUHRE is a driver for the integration routine
!        DADHRE, which repeatedly subdivides the region
!        of integration and estimates the integrals and the
!        errors over the subregions with greatest
!        estimated errors until the error request
!        is met or MAXPTS function evaluations have been used.

!        For NDIM = 2 the default integration rule is of
!        degree 13 and uses 65 evaluation points.
!        For NDIM = 3 the default integration rule is of
!        degree 11 and uses 127 evaluation points.
!        For NDIM greater then 3 the default integration rule
!        is of degree 9 and uses NUM evaluation points where
!          NUM = 1 + 4*2*NDIM + 2*NDIM*(NDIM-1) + 4*NDIM*(NDIM-1) +
!                4*NDIM*(NDIM-1)*(NDIM-2)/3 + 2**NDIM
!        The degree 9 rule may also be applied for NDIM = 2
!        and NDIM = 3.
!        A rule of degree 7 is available in all dimensions.
!        The number of evaluation
!        points used by the degree 7 rule is
!          NUM = 1 + 3*2*NDIM + 2*NDIM*(NDIM-1) + 2**NDIM

!        When DCUHRE computes estimates to a vector of
!        integrals, all components of the vector are given
!        the same treatment. That is, I(F ) and I(F ) for
!                                        J         K
!        J not equal to K, are estimated with the same
!        subdivision of the region of integration.
!        For integrals with enough similarity, we may save
!        time by applying DCUHRE to all integrands in one call.
!        For integrals that varies continuously as functions of
!        some parameter, the estimates produced by DCUHRE will
!        also vary continuously when the same subdivision is
!        applied to all components. This will generally not be
!        the case when the different components are given
!        separate treatment.

!        On the other hand this feature should be used with
!        caution when the different components of the integrals
!        require clearly different subdivisions.

!   ON ENTRY

!   NDIM   Integer.
!          Number of variables. 1 < NDIM <=  15.
!   NUMFUN Integer.
!          Number of components of the integral.
!   A      Real array of dimension NDIM.
!          Lower limits of integration.
!   B      Real array of dimension NDIM.
!          Upper limits of integration.
!   MINPTS Integer.
!          Minimum number of function evaluations.
!   MAXPTS Integer.
!          Maximum number of function evaluations.
!          The number of function evaluations over each subregion is NUM.
!          If (KEY = 0 or KEY = 1) and (NDIM = 2) Then
!            NUM = 65
!          Elseif (KEY = 0 or KEY = 2) and (NDIM = 3) Then
!            NUM = 127
!          Elseif (KEY = 0 and NDIM > 3) or (KEY = 3) Then
!            NUM = 1 + 4*2*NDIM + 2*NDIM*(NDIM-1) + 4*NDIM*(NDIM-1) +
!                  4*NDIM*(NDIM-1)*(NDIM-2)/3 + 2**NDIM
!          Elseif (KEY = 4) Then
!            NUM = 1 + 3*2*NDIM + 2*NDIM*(NDIM-1) + 2**NDIM
!          MAXPTS >= 3*NUM and MAXPTS >= MINPTS
!          For 3 < NDIM < 13 the minimum values for MAXPTS are:
!           NDIM =    4   5   6    7    8    9    10   11    12
!          KEY = 3:  459 819 1359 2151 3315 5067 7815 12351 20235
!          KEY = 4:  195 309  483  765 1251 2133 3795  7005 13299
!   FUNSUB Externally declared subroutine for computing all components of the
!          integrand at the given evaluation point.
!          It must have parameters (NDIM,X,NUMFUN,FUNVLS)
!          Input parameters:
!            NDIM   Integer that defines the dimension of the
!                   integral.
!            X      Real array of dimension NDIM
!                   that defines the evaluation point.
!            NUMFUN Integer that defines the number of
!                   components of I.
!          Output parameter:
!            FUNVLS Real array of dimension NUMFUN
!                   that defines NUMFUN components of the integrand.

!   EPSABS Real.
!          Requested absolute error.
!   EPSREL Real.
!          Requested relative error.
!   KEY    Integer.
!          Key to selected local integration rule.
!          KEY = 0 is the default value.
!                For NDIM = 2 the degree 13 rule is selected.
!                For NDIM = 3 the degree 11 rule is selected.
!                For NDIM > 3 the degree  9 rule is selected.
!          KEY = 1 gives the user the 2 dimensional degree 13
!                integration rule that uses 65 evaluation points.
!          KEY = 2 gives the user the 3 dimensional degree 11
!                integration rule that uses 127 evaluation points.
!          KEY = 3 gives the user the degree 9 integration rule.
!          KEY = 4 gives the user the degree 7 integration rule.
!                This is the recommended rule for problems that
!                require great adaptivity.
!   RESTAR Integer.
!          If RESTAR = 0, this is the first attempt to compute
!          the integral.
!          If RESTAR = 1, then we restart a previous attempt.
!          In this case the only parameters for DCUHRE that may
!          be changed (with respect to the previous call of DCUHRE)
!          are MINPTS, MAXPTS, EPSABS, EPSREL and RESTAR.
!   NSUB   Integer.
!          If RESTAR = 1, then NSUB contains the number of subregions from
!          the previous call,

! ON RETURN

!   RESULT Real array of dimension NUMFUN.
!          Approximations to all components of the integral.
!   ABSERR Real array of dimension NUMFUN.
!          Estimates of absolute errors.
!   NEVAL  Integer.
!          Number of function evaluations used by DCUHRE.
!   IFAIL  Integer.
!          IFAIL = 0 for normal exit, when ABSERR(K) <=  EPSABS or
!            ABSERR(K) <=  ABS(RESULT(K))*EPSREL with MAXPTS or less
!            function evaluations for all values of K,
!            1 <= K <= NUMFUN .
!          IFAIL = 1 if MAXPTS was too small for DCUHRE
!            to obtain the required accuracy. In this case DCUHRE
!            returns values of RESULT with estimated absolute
!            errors ABSERR.
!          IFAIL = 2 if KEY is less than 0 or KEY greater than 4.
!          IFAIL = 3 if NDIM is less than 2 or NDIM greater than 15.
!          IFAIL = 4 if KEY = 1 and NDIM not equal to 2.
!          IFAIL = 5 if KEY = 2 and NDIM not equal to 3.
!          IFAIL = 6 if NUMFUN is less than 1.
!          IFAIL = 7 if volume of region of integration is zero.
!          IFAIL = 8 if MAXPTS is less than 3*NUM.
!          IFAIL = 9 if MAXPTS is less than MINPTS.
!          IFAIL = 10 if EPSABS < 0 and EPSREL < 0.
!          IFAIL = 11 if NW is too small.
!          IFAIL = 12 if unlegal RESTAR.
!   NSUB   Integer.
!          Contains the number of subregions, in case needed for a restart.

!***LONG DESCRIPTION

!   The information for each subregion is contained in:
!   VALUES, ERRORS, CENTRS, HWIDTS, GREATE, DIR, OLDRES and WORK.
!   VALUES contains the estimated values of the integrals.
!   ERRORS contains the estimated errors.
!   CENTRS contains the centers of the subregions.
!   HWIDTS contains the half widths of the subregions.
!   GREATE contains the greatest estimated error for each subregion.
!   DIR    contains the directions for further subdivision.
!   OLDRES is used as a work array in DADHRE.

!   The data structures for the subregions are in DADHRE organized
!   as a heap, and the size of GREATE(I) defines the position of
!   region I in the heap. The heap is maintained by the program DTRHRE.

!   The subroutine DADHRE is written for efficient execution on shared
!   memory parallel computer. On a computer with NPROC processors we will
!   in each subdivision step divide MDIV regions, where MDIV is
!   chosen such that MOD(2*MDIV,NPROC) = 0, in totally 2*MDIV new regions.
!   Each processor will then compute estimates of the integrals and errors
!   over 2*MDIV/NPROC subregions in each subdivision step.
!   The subroutine for estimating the integral and the error over
!   each subregion, DRLHRE, uses WORK2 as a work array.
!   We must make sure that each processor writes its results to
!   separate parts of the memory, and therefore the sizes of WORK and
!   WORK2 are functions of MDIV.
!   In order to achieve parallel processing of subregions, compiler
!   directives should be placed in front of the DO 200
!   loop in DADHRE on machines like Alliant and CRAY.

!***REFERENCES
!   J.Berntsen, T.O.Espelid and A.Genz, An Adaptive Algorithm
!   for the Approximate Calculation of Multiple Integrals,
!   To be published.

!   J.Berntsen, T.O.Espelid and A.Genz, DCUHRE: An Adaptive
!   Multidimensional Integration Routine for a Vector of
!   Integrals, To be published.

!***ROUTINES CALLED DCHHRE,DADHRE
!***END PROLOGUE DCUHRE

!   Global variables.

INTEGER, INTENT(IN)     :: ndim, numfun, minpts, maxpts, key, restar
INTEGER, INTENT(IN OUT) :: nsub
INTEGER, INTENT(OUT)    :: neval, ifail
REAL (dp), INTENT(IN)   :: a(:), b(:), epsabs, epsrel
REAL (dp), INTENT(OUT)  :: result(:), abserr(:)

! Dimensions of arguments:
! a(ndim), b(ndim), result(numfun), abserr(numfun), work(nw)

INTERFACE
   SUBROUTINE funsub(ndim, z, nfun, f)
    import :: dp
    IMPLICIT NONE
    !INTEGER, PARAMETER     :: dp = SELECTED_REAL_KIND(15, 60)
    INTEGER, INTENT(IN)    :: ndim, nfun
    REAL (dp), INTENT(IN)  :: z(:)
    REAL (dp), INTENT(OUT) :: f(:)
  END SUBROUTINE funsub
END INTERFACE

!   Local variables.

!   MDIV   Integer.
!          MDIV is the number of subregions that are divided in
!          each subdivision step in DADHRE.
!          MDIV is chosen default to 1.
!          For efficient execution on parallel computers
!          with NPROC processors MDIV should be set equal to
!          the smallest integer such that MOD(2*MDIV,NPROC) = 0.
!   MAXDIM Integer.
!          The maximum allowed value of NDIM.
!   MAXWT  Integer. The maximum number of weights used by the
!          integration rule.
!   WTLENG Integer.
!          The number of generators used by the selected rule.
!   MAXSUB Integer.
!          The maximum allowed number of subdivisions
!          for the given values of KEY, NDIM and MAXPTS.
!   MINSUB Integer.
!          The minimum allowed number of subregions for the given
!          values of MINPTS, KEY and NDIM.
!   WRKSUB Integer.
!          The maximum allowed number of subregions as a function
!          of NW, NUMFUN, NDIM and MDIV. This determines the length
!          of the main work arrays.
!   NUM    Integer. The number of integrand evaluations needed
!          over each subregion.

INTEGER            :: wtleng, maxsub, minsub
INTEGER            :: num, keyf
INTEGER, PARAMETER :: mdiv=1, maxdim=15

!***FIRST EXECUTABLE STATEMENT DCUHRE

!   Compute NUM, WTLENG, MAXSUB and MINSUB, and check the input parameters.

CALL dchhre(maxdim, ndim, numfun, a, b, minpts, maxpts, epsabs,  &
            epsrel, key, restar, num, maxsub, minsub, keyf, ifail, wtleng)

IF (ifail /= 0) THEN
  GO TO 999
END IF

!   Compute the size of the temporary work space needed in DADHRE.

CALL dadhre(ndim, numfun, mdiv, a, b, minsub, maxsub, funsub, epsabs,  &
            epsrel, keyf, restar, num, wtleng, result, abserr,  &
            neval, nsub, ifail)
999 RETURN

!***END DCUHRE

END SUBROUTINE dcuhre


SUBROUTINE dchhre(maxdim, ndim, numfun, a, b, minpts, maxpts, epsabs,  &
                  epsrel, key, restar, num, maxsub, minsub, keyf, ifail, &
                  wtleng)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: DCHHRE
      !DIR$ OPTIMIZE : 3
      !DIR$ CODE_ALIGN : 32 :: DCHHRE
#endif
  
!***BEGIN PROLOGUE DCHHRE
!***PURPOSE  DCHHRE checks the validity of the
!            input parameters to DCUHRE.
!***DESCRIPTION
!            DCHHRE computes NUM, MAXSUB, MINSUB, KEYF, WTLENG and
!            IFAIL as functions of the input parameters to DCUHRE,
!            and checks the validity of the input parameters to DCUHRE.

!   ON ENTRY

!     MAXDIM Integer.
!            The maximum allowed number of dimensions.
!     NDIM   Integer.
!            Number of variables. 1 < NDIM <= MAXDIM.
!     NUMFUN Integer.
!            Number of components of the integral.
!     A      Real array of dimension NDIM.
!            Lower limits of integration.
!     B      Real array of dimension NDIM.
!            Upper limits of integration.
!     MINPTS Integer.
!            Minimum number of function evaluations.
!     MAXPTS Integer.
!            Maximum number of function evaluations.
!            The number of function evaluations over each subregion
!            is NUM.
!            If (KEY = 0 or KEY = 1) and (NDIM = 2) Then
!              NUM = 65
!            Elseif (KEY = 0 or KEY = 2) and (NDIM = 3) Then
!              NUM = 127
!            Elseif (KEY = 0 and NDIM > 3) or (KEY = 3) Then
!              NUM = 1 + 4*2*NDIM + 2*NDIM*(NDIM-1) + 4*NDIM*(NDIM-1) +
!                    4*NDIM*(NDIM-1)*(NDIM-2)/3 + 2**NDIM
!            Elseif (KEY = 4) Then
!              NUM = 1 + 3*2*NDIM + 2*NDIM*(NDIM-1) + 2**NDIM
!            MAXPTS >= 3*NUM and MAXPTS >= MINPTS
!     EPSABS Real.
!            Requested absolute error.
!     EPSREL Real.
!            Requested relative error.
!     KEY    Integer.
!            Key to selected local integration rule.
!            KEY = 0 is the default value.
!                  For NDIM = 2 the degree 13 rule is selected.
!                  For NDIM = 3 the degree 11 rule is selected.
!                  For NDIM > 3 the degree  9 rule is selected.
!            KEY = 1 gives the user the 2 dimensional degree 13
!                  integration rule that uses 65 evaluation points.
!            KEY = 2 gives the user the 3 dimensional degree 11
!                  integration rule that uses 127 evaluation points.
!            KEY = 3 gives the user the degree 9 integration rule.
!            KEY = 4 gives the user the degree 7 integration rule.
!                  This is the recommended rule for problems that
!                  require great adaptivity.
!     RESTAR Integer.
!            If RESTAR = 0, this is the first attempt to compute
!            the integral.
!            If RESTAR = 1, then we restart a previous attempt.

!   ON RETURN

!     NUM    Integer.
!            The number of function evaluations over each subregion.
!     MAXSUB Integer.
!            The maximum allowed number of subregions for the
!            given values of MAXPTS, KEY and NDIM.
!     MINSUB Integer.
!            The minimum allowed number of subregions for the given
!            values of MINPTS, KEY and NDIM.
!     IFAIL  Integer.
!            IFAIL = 0 for normal exit.
!            IFAIL = 2 if KEY is less than 0 or KEY greater than 4.
!            IFAIL = 3 if NDIM is less than 2 or NDIM greater than MAXDIM.
!            IFAIL = 4 if KEY = 1 and NDIM not equal to 2.
!            IFAIL = 5 if KEY = 2 and NDIM not equal to 3.
!            IFAIL = 6 if NUMFUN less than 1.
!            IFAIL = 7 if volume of region of integration is zero.
!            IFAIL = 8 if MAXPTS is less than 3*NUM.
!            IFAIL = 9 if MAXPTS is less than MINPTS.
!            IFAIL = 10 if EPSABS < 0 and EPSREL < 0.
!            IFAIL = 11 if NW is too small.
!            IFAIL = 12 if unlegal RESTAR.
!     KEYF   Integer.
!            Key to selected integration rule.
!     WTLENG Integer.
!            The number of generators of the chosen integration rule.

!***ROUTINES CALLED-NONE
!***END PROLOGUE DCHHRE

!   Global variables.

INTEGER, INTENT(IN)   :: maxdim, ndim, numfun, minpts, maxpts, key, restar
INTEGER, INTENT(OUT)  :: num, minsub, maxsub, keyf, ifail, wtleng
REAL (dp), INTENT(IN) :: a(:), b(:), epsabs, epsrel

!   Local variables.

INTEGER :: j

!***FIRST EXECUTABLE STATEMENT DCHHRE

ifail = 0

!   Check on legal KEY.

IF (key < 0 .OR. key > 4) THEN
  ifail = 2
  GO TO 999
END IF

!   Check on legal NDIM.

IF (ndim < 2 .OR. ndim > maxdim) THEN
  ifail = 3
  GO TO 999
END IF

!   For KEY = 1, NDIM must be equal to 2.

IF (key == 1 .AND. ndim /= 2) THEN
  ifail = 4
  GO TO 999
END IF

!   For KEY = 2, NDIM must be equal to 3.

IF (key == 2 .AND. ndim /= 3) THEN
  ifail = 5
  GO TO 999
END IF

!   For KEY = 0, we point at the selected integration rule.

IF (key == 0) THEN
  IF (ndim == 2) THEN
    keyf = 1
  ELSE IF (ndim == 3) THEN
    keyf = 2
  ELSE
    keyf = 3
  END IF
ELSE
  keyf = key
END IF

!   Compute NUM and WTLENG as a function of KEYF and NDIM.

IF (keyf == 1) THEN
  num = 65
  wtleng = 14
ELSE IF (keyf == 2) THEN
  num = 127
  wtleng = 13
ELSE IF (keyf == 3) THEN
  num = 1 + 4*2*ndim + 2*ndim* (ndim-1) + 4*ndim* (ndim-1) +  &
            4*ndim* (ndim-1)* (ndim-2)/3 + 2**ndim
  wtleng = 9
  IF (ndim == 2) wtleng = 8
ELSE IF (keyf == 4) THEN
  num = 1 + 3*2*ndim + 2*ndim* (ndim-1) + 2**ndim
  wtleng = 6
END IF

!   Compute MAXSUB.

maxsub = (maxpts-num)/ (2*num) + 1

!   Compute MINSUB.

minsub = (minpts-num)/ (2*num) + 1
IF (MOD(minpts-num, 2*num) /= 0) THEN
  minsub = minsub + 1
END IF
minsub = MAX(2,minsub)

!   Check on positive NUMFUN.

IF (numfun < 1) THEN
  ifail = 6
  GO TO 999
END IF

!   Check on legal upper and lower limits of integration.

DO j = 1, ndim
  IF (a(j)-b(j) == 0) THEN
    ifail = 7
    GO TO 999
  END IF
END DO

!   Check on MAXPTS < 3*NUM.

IF (maxpts < 3*num) THEN
  ifail = 8
  GO TO 999
END IF

!   Check on MAXPTS >= MINPTS.

IF (maxpts < minpts) THEN
  ifail = 9
  GO TO 999
END IF

!   Check on legal accuracy requests.

IF (epsabs < 0 .AND. epsrel < 0) THEN
  ifail = 10
  GO TO 999
END IF

!    Check on legal RESTAR.

IF (restar /= 0 .AND. restar /= 1) THEN
  ifail = 12
  GO TO 999
END IF
999 RETURN

!***END DCHHRE

END SUBROUTINE dchhre


SUBROUTINE dadhre(ndim, numfun, mdiv, a, b, minsub, maxsub, funsub, epsabs,   &
                  epsrel, key, restar, num, wtleng, result, abserr,     &
                  neval, nsub, ifail)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: DADHRE
      !DIR$ OPTIMIZE : 3
      !DIR$ CODE_ALIGN : 32 :: DADHRE
#endif
  
!***BEGIN PROLOGUE DADHRE
!***KEYWORDS automatic multidimensional integrator,
!            n-dimensional hyper-rectangles,
!            general purpose, global adaptive
!***PURPOSE  The routine calculates an approximation to a given
!            vector of definite integrals, I, over a hyper-rectangular
!            region hopefully satisfying for each component of I the
!            following claim for accuracy:
!            ABS(I(K)-RESULT(K)).LE.MAX(EPSABS,EPSREL*ABS(I(K)))
!***DESCRIPTION Computation of integrals over hyper-rectangular
!            regions.
!            DADHRE repeatedly subdivides the region
!            of integration and estimates the integrals and the
!            errors over the subregions with  greatest
!            estimated errors until the error request
!            is met or MAXSUB subregions are stored.
!            The regions are divided in two equally sized parts along
!            the direction with greatest absolute fourth divided
!            difference.

!   ON ENTRY

!     NDIM   Integer.
!            Number of variables. 1 < NDIM <= MAXDIM.
!     NUMFUN Integer.
!            Number of components of the integral.
!     MDIV   Integer.
!            Defines the number of new subregions that are divided
!            in each subdivision step.
!     A      Real array of dimension NDIM.
!            Lower limits of integration.
!     B      Real array of dimension NDIM.
!            Upper limits of integration.
!     MINSUB Integer.
!            The computations proceed until there are at least
!            MINSUB subregions in the data structure.
!     MAXSUB Integer.
!            The computations proceed until there are at most
!            MAXSUB subregions in the data structure.

!     FUNSUB Externally declared subroutine for computing
!            all components of the integrand in the given
!            evaluation point.
!            It must have parameters (NDIM,X,NUMFUN,FUNVLS)
!            Input parameters:
!              NDIM   Integer that defines the dimension of the
!                     integral.
!              X      Real array of dimension NDIM
!                     that defines the evaluation point.
!              NUMFUN Integer that defines the number of
!                     components of I.
!            Output parameter:
!              FUNVLS Real array of dimension NUMFUN
!                     that defines NUMFUN components of the integrand.

!     EPSABS Real.
!            Requested absolute error.
!     EPSREL Real.
!            Requested relative error.
!     KEY    Integer.
!            Key to selected local integration rule.
!            KEY = 0 is the default value.
!                  For NDIM = 2 the degree 13 rule is selected.
!                  For NDIM = 3 the degree 11 rule is selected.
!                  For NDIM > 3 the degree  9 rule is selected.
!            KEY = 1 gives the user the 2 dimensional degree 13
!                  integration rule that uses 65 evaluation points.
!            KEY = 2 gives the user the 3 dimensional degree 11
!                  integration rule that uses 127 evaluation points.
!            KEY = 3 gives the user the degree 9 integration rule.
!            KEY = 4 gives the user the degree 7 integration rule.
!                  This is the recommended rule for problems that
!                  require great adaptivity.
!     RESTAR Integer.
!            If RESTAR = 0, this is the first attempt to compute
!            the integral.
!            If RESTAR = 1, then we restart a previous attempt.
!            (In this case the output parameters from DADHRE
!            must not be changed since the last
!            exit from DADHRE.)
!     NUM    Integer.
!            The number of function evaluations over each subregion.
!     WTLENG Integer.
!            The number of weights in the basic integration rule.
!     NSUB   Integer.
!            If RESTAR = 1, then NSUB must specify the number
!            of subregions stored in the previous call to DADHRE.

!   ON RETURN

!     RESULT Real array of dimension NUMFUN.
!            Approximations to all components of the integral.
!     ABSERR Real array of dimension NUMFUN.
!            Estimates of absolute accuracies.
!     NEVAL  Integer.
!            Number of function evaluations used by DADHRE.
!     NSUB   Integer.
!            Number of stored subregions.
!     IFAIL  Integer.
!            IFAIL = 0 for normal exit, when ABSERR(K) <=  EPSABS or
!              ABSERR(K) <=  ABS(RESULT(K))*EPSREL with MAXSUB or less
!              subregions processed for all values of K,
!              1 <=  K <=  NUMFUN.
!            IFAIL = 1 if MAXSUB was too small for DADHRE
!              to obtain the required accuracy. In this case DADHRE
!              returns values of RESULT with estimated absolute
!              accuracies ABSERR.
!
! The following occupied workspace provided by DCUHRE in the Fortran 77 code:
!
!     VALUES Real array of dimension (NUMFUN,MAXSUB).
!            Used to store estimated values of the integrals
!            over the subregions.
!     ERRORS Real array of dimension (NUMFUN,MAXSUB).
!            Used to store the corresponding estimated errors.
!     CENTRS Real array of dimension (NDIM,MAXSUB).
!            Used to store the centers of the stored subregions.
!     HWIDTS Real array of dimension (NDIM,MAXSUB).
!            Used to store the half widths of the stored subregions.
!     GREATE Real array of dimension MAXSUB.
!            Used to store the greatest estimated errors in
!            all subregions.
!     DIR    Real array of dimension MAXSUB.
!            DIR is used to store the directions for
!            further subdivision.
!     OLDRES Real array of dimension (NUMFUN,MDIV).
!            Used to store old estimates of the integrals over the
!            subregions.
!     WORK   Real array of dimension LENW.
!            Used  in DRLHRE and DTRHRE.
!     G      Real array of dimension (NDIM,WTLENG,2*MDIV).
!            The fully symmetric sum generators for the rules.
!            G(1,J,1),...,G(NDIM,J,1) are the generators for the
!            points associated with the Jth weights.
!            When MDIV subregions are divided in 2*MDIV
!            subregions, the subregions may be processed on different
!            processors and we must make a copy of the generators
!            for each processor.
!     W      Real array of dimension (5,WTLENG).
!            The weights for the basic and null rules.
!            W(1,1), ..., W(1,WTLENG) are weights for the basic rule.
!            W(I,1), ..., W(I,WTLENG) , for I > 1 are null rule weights.
!     RULPTS Real array of dimension WTLENG.
!            Work array used in DINHRE.
!     CENTER Real array of dimension NDIM.
!            Work array used in DTRHRE.
!     HWIDTH Real array of dimension NDIM.
!            Work array used in DTRHRE.
!     X      Real array of dimension (NDIM,2*MDIV).
!            Work array used in DRLHRE.
!     SCALES Real array of dimension (3,WTLENG).
!            Work array used by DINHRE and DRLHRE.
!     NORMS  Real array of dimension (3,WTLENG).
!            Work array used by DINHRE and DRLHRE.

!***REFERENCES

!   P. van Dooren and L. de Ridder, Algorithm 6, An adaptive algorithm
!   for numerical integration over an n-dimensional cube, J.Comput.Appl.
!   Math. 2(1976)207-217.

!   A.C.Genz and A.A.Malik, Algorithm 019. Remarks on algorithm 006:
!   An adaptive algorithm for numerical integration over an
!   N-dimensional rectangular region, J.Comput.Appl.Math. 6(1980)295-302.

!***ROUTINES CALLED DTRHRE, DINHRE, DRLHRE
!***END PROLOGUE DADHRE

!   Global variables.

INTEGER, INTENT(IN)     :: num, ndim, numfun, mdiv, minsub, maxsub, key,  &
                           restar, wtleng
INTEGER, INTENT(IN OUT) :: nsub
INTEGER, INTENT(OUT)    :: neval, ifail
REAL (dp), INTENT(IN)   :: a(:), b(:), epsabs, epsrel
REAL (dp), INTENT(OUT)  :: result(:), abserr(:)

INTERFACE
   SUBROUTINE funsub(ndim, z, nfun, f)
    import :: dp
    IMPLICIT NONE
    !INTEGER, PARAMETER     :: dp = SELECTED_REAL_KIND(15, 60)
    INTEGER, INTENT(IN)    :: ndim, nfun
    REAL (dp), INTENT(IN)  :: z(:)
    REAL (dp), INTENT(OUT) :: f(:)
  END SUBROUTINE funsub
END INTERFACE

!   Local variables.

!   INTSGN is used to get correct sign on the integral.
!   SBRGNS is the number of stored subregions.
!   NDIV   The number of subregions to be divided in each main step.
!   POINTR Pointer to the position in the data structure where
!          the new subregions are to be stored.
!   DIRECT Direction of subdivision.
!   ERRCOF Heuristic error coeff. defined in DINHRE and used by DRLHRE
!          and DADHRE.

INTEGER   :: i, j, k
INTEGER   :: intsgn, sbrgns
INTEGER   :: ndiv, pointr, DIRECT, INDEX
REAL (dp) :: oldcen, est1, est2, errcof(6)

! The following automatic arrays were previously part of the workspace
! passed from DCUHRE.

REAL (dp) :: values(numfun,maxsub), errors(numfun,maxsub), &
             centrs(ndim,maxsub), hwidts(ndim,maxsub), greate(maxsub),  &
             dir(maxsub), oldres(numfun,mdiv), g(ndim,wtleng,2*mdiv),   &
             w(5,wtleng), center(ndim), hwidth(ndim), scales(3,wtleng), &
             norms(3,wtleng)

!***FIRST EXECUTABLE STATEMENT DADHRE

!   Get the correct sign on the integral.

intsgn = 1
DO j = 1, ndim
  IF (b(j) < a(j)) THEN
    intsgn = - intsgn
  END IF
END DO

!   Call DINHRE to compute the weights and abscissas of
!   the function evaluation points.

CALL dinhre(ndim, key, wtleng, w, g(:,:,1), errcof, scales, norms)

!   If RESTAR = 1, then this is a restart run.

IF (restar == 1) THEN
  sbrgns = nsub
  GO TO 110
END IF

!   Initialize the SBRGNS, CENTRS and HWIDTS.

sbrgns = 1
DO j = 1, ndim
  centrs(j,1) = (a(j)+b(j))/2
  hwidts(j,1) = ABS(b(j)-a(j))/2
END DO

!   Initialize RESULT, ABSERR and NEVAL.

DO j = 1, numfun
  result(j) = 0
  abserr(j) = 0
END DO
neval = 0

!   Apply DRLHRE over the whole region.

CALL drlhre(ndim, centrs(:,1), hwidts(:,1), wtleng, g(:,:,1), w, errcof,  &
            numfun, funsub, scales, norms, values(:,1), errors(:,1), dir(1))
neval = neval + num

!   Add the computed values to RESULT and ABSERR.

DO j = 1, numfun
  result(j) = result(j) + values(j,1)
END DO
DO j = 1, numfun
  abserr(j) = abserr(j) + errors(j,1)
END DO

!   Store results in heap.

INDEX = 1
CALL dtrhre(2, ndim, numfun, INDEX, values, errors, centrs, hwidts,  &
            greate, center, hwidth, dir)

!***End initialisation.

!***Begin loop while the error is too great,
!   and SBRGNS+1 is less than MAXSUB.

110 IF (sbrgns+1 <= maxsub) THEN
  
!   If we are allowed to divide further,
!   prepare to apply basic rule over each half of the
!   NDIV subregions with greatest errors.
!   If MAXSUB is great enough, NDIV = MDIV
  
  IF (mdiv > 1) THEN
    ndiv = maxsub - sbrgns
    ndiv = MIN(ndiv, mdiv, sbrgns)
  ELSE
    ndiv = 1
  END IF
  
!   Divide the NDIV subregions in two halves, and compute
!   integral and error over each half.
  
  DO i = 1, ndiv
    pointr = sbrgns + ndiv + 1 - i
    
!   Adjust RESULT and ABSERR.
    
    DO j = 1, numfun
      result(j) = result(j) - values(j,1)
      abserr(j) = abserr(j) - errors(j,1)
    END DO
    
!   Compute first half region.
    
    DO j = 1, ndim
      centrs(j,pointr) = centrs(j,1)
      hwidts(j,pointr) = hwidts(j,1)
    END DO
    DIRECT = dir(1)
    dir(pointr) = DIRECT
    hwidts(DIRECT,pointr) = hwidts(DIRECT,1)/2
    oldcen = centrs(DIRECT,1)
    centrs(DIRECT,pointr) = oldcen - hwidts(DIRECT,pointr)
    
!   Save the computed values of the integrals.
    
    DO j = 1, numfun
      oldres(j,ndiv-i+1) = values(j,1)
    END DO
    
!   Adjust the heap.
    
    CALL dtrhre(1, ndim, numfun, sbrgns, values, errors, centrs,  &
                hwidts, greate, center, hwidth, dir)
    
!   Compute second half region.
    
    DO j = 1, ndim
      centrs(j,pointr-1) = centrs(j,pointr)
      hwidts(j,pointr-1) = hwidts(j,pointr)
    END DO
    centrs(DIRECT,pointr-1) = oldcen + hwidts(DIRECT,pointr)
    hwidts(DIRECT,pointr-1) = hwidts(DIRECT,pointr)
    dir(pointr-1) = DIRECT
  END DO
  
!   Make copies of the generators for each processor.
  
  DO i = 2, 2*ndiv
    DO j = 1, ndim
      DO k = 1, wtleng
        g(j,k,i) = g(j,k,1)
      END DO
    END DO
  END DO
  
!   Apply basic rule.
  
  DO i = 1, 2*ndiv
    INDEX = sbrgns + i
    CALL drlhre(ndim, centrs(:,INDEX), hwidts(:,INDEX), wtleng, g(:,:,i), w, &
                errcof, numfun, funsub, scales, norms, values(:,INDEX),   &
                errors(:,INDEX), dir(INDEX))
  END DO
  neval = neval + 2*ndiv*num
  
!   Add new contributions to RESULT.
  
  DO i = 1, 2*ndiv
    DO j = 1, numfun
      result(j) = result(j) + values(j,sbrgns+i)
    END DO
  END DO
  
!   Check consistency of results and if necessary adjust
!   the estimated errors.
  
  DO i = 1, ndiv
    greate(sbrgns+2*i-1) = 0
    greate(sbrgns+2*i) = 0
    DO j = 1, numfun
      est1 = ABS(oldres(j,i)- (values(j,sbrgns+2*i-1)+values(j,sbrgns+2*i)))
      est2 = errors(j, sbrgns+2*i-1) + errors(j, sbrgns+2*i)
      IF (est2 > 0) THEN
        errors(j,sbrgns+2*i-1) = errors(j,sbrgns+2*i-1)*(1+errcof(5)*est1/est2)
        errors(j,sbrgns+2*i) = errors(j,sbrgns+2*i)*(1+errcof(5)*est1/est2)
      END IF
      errors(j,sbrgns+2*i-1) = errors(j,sbrgns+2*i-1) +errcof(6)*est1
      errors(j,sbrgns+2*i) = errors(j,sbrgns+2*i) +errcof(6)*est1
      IF (errors(j,sbrgns+2*i-1) > greate(sbrgns+2*i-1)) THEN
        greate(sbrgns+2*i-1) = errors(j,sbrgns+2*i-1)
      END IF
      IF (errors(j,sbrgns+2*i) > greate(sbrgns+2*i)) THEN
        greate(sbrgns+2*i) = errors(j,sbrgns+2*i)
      END IF
      abserr(j) = abserr(j) + errors(j,sbrgns+2*i-1) + errors(j,sbrgns+2*i)
    END DO
  END DO
  
!   Store results in heap.
  
  DO i = 1, 2*ndiv
    INDEX = sbrgns + i
    CALL dtrhre(2, ndim, numfun, INDEX, values, errors, centrs,  &
                hwidts, greate, center, hwidth, dir)
  END DO
  sbrgns = sbrgns + 2*ndiv
  
!   Check for termination.
  
  IF (sbrgns < minsub) THEN
    GO TO 110
  END IF
  DO j = 1, numfun
    IF (abserr(j) > epsrel*ABS(result(j)) .AND. abserr(j) > epsabs) THEN
      GO TO 110
    END IF
  END DO
  ifail = 0
  GO TO 499
  
!   Else we did not succeed with the
!   given value of MAXSUB.
  
ELSE
  ifail = 1
END IF

!   Compute more accurate values of RESULT and ABSERR.

499 DO j = 1, numfun
  result(j) = 0
  abserr(j) = 0
END DO
DO i = 1, sbrgns
  DO j = 1, numfun
    result(j) = result(j) + values(j,i)
    abserr(j) = abserr(j) + errors(j,i)
  END DO
END DO

!   Compute correct sign on the integral.

DO j = 1, numfun
  result(j) = result(j)*intsgn
END DO
nsub = sbrgns
RETURN

!***END DADHRE

END SUBROUTINE dadhre


SUBROUTINE dinhre(ndim, key, wtleng, w, g, errcof, scales, norms)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: DINHRE
      !DIR$ OPTIMIZE : 3
      !DIR$ CODE_ALIGN : 32 :: DINHRE
#endif
!***BEGIN PROLOGUE DINHRE
!***PURPOSE DINHRE computes abscissas and weights of the integration
!            rule and the null rules to be used in error estimation.
!            These are computed as functions of NDIM and KEY.
!***DESCRIPTION DINHRE will for given values of NDIM and KEY compute or
!            select the correct values of the abscissas and
!            corresponding weights for different
!            integration rules and null rules and assign them to G and W.
!            The heuristic error coefficients ERRCOF
!            will be computed as a function of KEY.
!            Scaling factors SCALES and normalization factors NORMS
!            used in the error estimation are computed.


!   ON ENTRY

!     NDIM   Integer.
!            Number of variables.
!     KEY    Integer.
!            Key to selected local integration rule.
!     WTLENG Integer.
!            The number of weights in each of the rules.

!   ON RETURN

!     W      Real array of dimension (5,WTLENG).
!            The weights for the basic and null rules.
!            W(1,1), ...,W(1,WTLENG) are weights for the basic rule.
!            W(I,1), ...,W(I,WTLENG), for I > 1 are null rule weights.
!     G      Real array of dimension (NDIM,WTLENG).
!            The fully symmetric sum generators for the rules.
!            G(1,J),...,G(NDIM,J) are the generators for the points
!            associated with the the Jth weights.
!     ERRCOF Real array of dimension 6.
!            Heuristic error coefficients that are used in the
!            error estimation in BASRUL.
!            It is assumed that the error is computed using:
!             IF (N1*ERRCOF(1) < N2 and N2*ERRCOF(2) < N3)
!               THEN ERROR = ERRCOF(3)*N1
!               ELSE ERROR = ERRCOF(4)*MAX(N1, N2, N3)
!             ERROR = ERROR + EP*(ERRCOF(5)*ERROR/(ES+ERROR)+ERRCOF(6))
!            where N1-N3 are the null rules, EP is the error for
!            the parent
!            subregion and ES is the error for the sibling subregion.
!     SCALES Real array of dimension (3,WTLENG).
!            Scaling factors used to construct new null rules,
!            N1, N2 and N3,
!            based on a linear combination of two successive null rules
!            in the sequence of null rules.
!     NORMS  Real array of dimension (3,WTLENG).
!            2**NDIM/(1-norm of the null rule constructed by each of
!            the scaling factors.)

!***ROUTINES CALLED  D132RE, D113RE, D07HRE, D09HRE
!***END PROLOGUE DINHRE

!   Global variables.

INTEGER, INTENT(IN)    :: ndim, key, wtleng
REAL (dp), INTENT(OUT) :: g(:,:), w(:,:), errcof(:)
REAL (dp), INTENT(OUT) :: scales(:,:)
REAL (dp), INTENT(OUT) :: norms(:,:)

! Dimensions:
! g(ndim,wtleng), w(5,wtleng), errcof(6), scales(3,wtleng), norms(3,wtleng)

!   Local variables.

INTEGER   :: i, j, k
REAL (dp) :: we(14), rulpts(wtleng)

!***FIRST EXECUTABLE STATEMENT DINHRE

!   Compute W, G and ERRCOF.

IF (key == 1) THEN
  CALL d132re(w, g, errcof, rulpts)
ELSE IF (key == 2) THEN
  CALL d113re(w, g, errcof, rulpts)
ELSE IF (key == 3) THEN
  CALL d09hre(ndim, wtleng, w, g, errcof, rulpts)
ELSE IF (key == 4) THEN
  CALL d07hre(ndim, wtleng, w, g, errcof, rulpts)
END IF

!   Compute SCALES and NORMS.

DO k = 1, 3
  DO i = 1, wtleng
    IF (w(k+1,i) /= 0) THEN
      scales(k,i) = - w(k+2,i)/w(k+1,i)
    ELSE
      scales(k,i) = 100
    END IF
    DO j = 1, wtleng
      we(j) = w(k+2,j) + scales(k,i)*w(k+1,j)
    END DO
    norms(k,i) = 0
    DO j = 1, wtleng
      norms(k,i) = norms(k,i) + rulpts(j)*ABS(we(j))
    END DO
    norms(k,i) = 2**ndim/norms(k,i)
  END DO
END DO
RETURN

!***END DINHRE

END SUBROUTINE dinhre


SUBROUTINE d132re(w, g, errcof, rulpts)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: D132RE
      !DIR$ OPTIMIZE : 3
      !DIR$ CODE_ALIGN : 32 :: D132RE
#endif
  
!***BEGIN PROLOGUE D132RE
!***AUTHOR   Jarle Berntsen, EDB-senteret,
!            University of Bergen, Thormohlens gt. 55,
!            N-5008 Bergen, NORWAY
!***PURPOSE D132RE computes abscissas and weights of a 2 dimensional
!            integration rule of degree 13.
!            Two null rules of degree 11, one null rule of degree 9
!            and one null rule of degree 7 to be used in error
!            estimation are also computed.
! ***DESCRIPTION D132RE will select the correct values of the abscissas
!            and corresponding weights for different
!            integration rules and null rules and assign them to
!            G and W. The heuristic error coefficients ERRCOF
!            will also be assigned.


!   ON RETURN

!     W      Real array of dimension (5,WTLENG).
!            The weights for the basic and null rules.
!            W(1,1),...,W(1,WTLENG) are weights for the basic rule.
!            W(I,1),...,W(I,WTLENG), for I > 1 are null rule weights.
!     G      Real array of dimension (NDIM,WTLENG).
!            The fully symmetric sum generators for the rules.
!            G(1,J),...,G(NDIM,J) are the generators for the points
!            associated with the the Jth weights.
!     ERRCOF Real array of dimension 6.
!            Heuristic error coefficients that are used in the
!            error estimation in BASRUL.
!     RULPTS Real array of dimension WTLENG.
!            The number of points produced by each generator.
!***REFERENCES S.Eriksen,
!              Thesis of the degree cand.scient, Dept. of Informatics,
!              Univ. of Bergen, Norway, 1984.

!***ROUTINES CALLED-NONE
!***END PROLOGUE D132RE

!   Global variables

REAL (dp), INTENT(OUT) :: w(:,:), g(:,:), errcof(:)
REAL (dp), INTENT(OUT) :: rulpts(:)

! Dimensions:
! w(5,wtleng), g(2,wtleng), errcof(6), rulpts(wtleng)

!   Local variables.

INTEGER   :: i, j
REAL (dp), PARAMETER :: dim2g(16) = (/ 0.2517129343453109D+00,  &
    0.7013933644534266D+00, 0.9590960631619962D+00, 0.9956010478552127D+00, &
    0.5000000000000000D+00, 0.1594544658297559D+00, 0.3808991135940188D+00, &
    0.6582769255267192D+00, 0.8761473165029315D+00, 0.9982431840531980D+00, &
    0.9790222658168462D+00, 0.6492284325645389D+00, 0.8727421201131239D+00, &
    0.3582614645881228D+00, 0.5666666666666666D+00, 0.2077777777777778D+00 /)
REAL (dp), PARAMETER :: dim2w(14,5) = RESHAPE( (/   &
0.3379692360134460D-01,  &
0.9508589607597761D-01, 0.1176006468056962D+00,  &
0.2657774586326950D-01, 0.1701441770200640D-01,  &
0.0000000000000000D+00, 0.1626593098637410D-01,  &
0.1344892658526199D+00, 0.1328032165460149D+00,  &
0.5637474769991870D-01, 0.3908279081310500D-02,  &
0.3012798777432150D-01, 0.1030873234689166D+00, 0.6250000000000000D-01, &

0.3213775489050763D+00,  &
- .1767341636743844D+00, 0.7347600537466072D-01,  &
- .3638022004364754D-01, 0.2125297922098712D-01,  &
0.1460984204026913D+00, 0.1747613286152099D-01,  &
0.1444954045641582D+00, 0.1307687976001325D-03,  &
0.5380992313941161D-03, 0.1042259576889814D-03,  &
- .1401152865045733D-02, 0.8041788181514763D-02, - .1420416552759383D+00, &

0.3372900883288987D+00,  &
- .1644903060344491D+00, 0.7707849911634622D-01,  &
- .3804478358506310D-01, 0.2223559940380806D-01,  &
0.1480693879765931D+00, 0.4467143702185814D-05,  &
0.1508944767074130D+00, 0.3647200107516215D-04,  &
0.5777198999013880D-03, 0.1041757313688177D-03,  &
- .1452822267047819D-02, 0.8338339968783705D-02, - .1472796329231960D+00, &

- .8264123822525677D+00,  &
0.3065838614094360D+00, 0.2389292538329435D-02,  &
- .1343024157997222D+00, 0.8833366840533900D-01,  &
0.0000000000000000D+00, 0.9786283074168292D-03,  &
- .1319227889147519D+00, 0.7990012200150630D-02,  &
0.3391747079760626D-02, 0.2294915718283264D-02,  &
- .1358584986119197D-01, 0.4025866859057809D-01, 0.3760268580063992D-02, &

0.6539094339575232D+00,  &
- .2041614154424632D+00, - .1746981515794990D+00,  &
0.3937939671417803D-01, 0.6974520545933992D-02,  &
0.0000000000000000D+00, 0.6667702171778258D-02,  &
0.5512960621544304D-01, 0.5443846381278607D-01,  &
0.2310903863953934D-01, 0.1506937747477189D-01,  &
- .6057021648901890D-01, 0.4225737654686337D-01, 0.2561989142123099D-01 /), &
   (/ 14, 5 /))

!***FIRST EXECUTABLE STATEMENT D132RE

!   Assign values to W.

DO i = 1, 14
  DO j = 1, 5
    w(j,i) = dim2w(i,j)
  END DO
END DO

!   Assign values to G.

DO i = 1, 2
  DO j = 1, 14
    g(i,j) = 0.d0
  END DO
END DO
g(1,2) = dim2g(1)
g(1,3) = dim2g(2)
g(1,4) = dim2g(3)
g(1,5) = dim2g(4)
g(1,6) = dim2g(5)
g(1,7) = dim2g(6)
g(2,7) = g(1,7)
g(1,8) = dim2g(7)
g(2,8) = g(1,8)
g(1,9) = dim2g(8)
g(2,9) = g(1,9)
g(1,10) = dim2g(9)
g(2,10) = g(1,10)
g(1,11) = dim2g(10)
g(2,11) = g(1,11)
g(1,12) = dim2g(11)
g(2,12) = dim2g(12)
g(1,13) = dim2g(13)
g(2,13) = dim2g(14)
g(1,14) = dim2g(15)
g(2,14) = dim2g(16)

!   Assign values to RULPTS.

rulpts(1) = 1
DO i = 2, 11
  rulpts(i) = 4
END DO
rulpts(12) = 8
rulpts(13) = 8
rulpts(14) = 8

!   Assign values to ERRCOF.

errcof(1) = 10
errcof(2) = 10
errcof(3) = 1.
errcof(4) = 5.
errcof(5) = 0.5
errcof(6) = 0.25

!***END D132RE

RETURN
END SUBROUTINE d132re


SUBROUTINE d113re(w, g, errcof, rulpts)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: D113RE
      !DIR$ OPTIMIZE : 3
      !DIR$ CODE_ALIGN : 32 :: D113RE
#endif
  
!***BEGIN PROLOGUE D113RE
!***AUTHOR   Jarle Berntsen, EDB-senteret,
!            University of Bergen, Thormohlens gt. 55,
!            N-5008 Bergen, NORWAY
!***PURPOSE D113RE computes abscissas and weights of a 3 dimensional
!            integration rule of degree 11.
!            Two null rules of degree 9, one null rule of degree 7
!            and one null rule of degree 5 to be used in error
!            estimation are also computed.
!***DESCRIPTION D113RE will select the correct values of the abscissas
!            and corresponding weights for different
!            integration rules and null rules and assign them to G
!            and W.
!            The heuristic error coefficients ERRCOF
!            will also be computed.


!   ON RETURN

!     W      Real array of dimension (5,WTLENG).
!            The weights for the basic and null rules.
!            W(1,1),...,W(1,WTLENG) are weights for the basic rule.
!            W(I,1),...,W(I,WTLENG), for I > 1 are null rule weights.
!     G      Real array of dimension (NDIM,WTLENG).
!            The fully symmetric sum generators for the rules.
!            G(1,J),...,G(NDIM,J) are the generators for the points
!            associated with the the Jth weights.
!     ERRCOF Real array of dimension 6.
!            Heuristic error coefficients that are used in the
!            error estimation in BASRUL.
!     RULPTS Real array of dimension WTLENG.
!            The number of points used by each generator.

!***REFERENCES  J.Berntsen, Cautious adaptive numerical integration
!               over the 3-cube, Reports in Informatics 17, Dept. of
!               Inf.,Univ. of Bergen, Norway, 1985.
!               J.Berntsen and T.O.Espelid, On the construction of
!               higher degree three-dimensional embedded integration
!               rules, SIAM J. Numer. Anal.,Vol. 25,No. 1, pp.222-234,
!               1988.
!***ROUTINES CALLED-NONE
!***END PROLOGUE D113RE

!   Global variables.

REAL (dp), INTENT(OUT) :: w(:, :), g(:,:), errcof(:)
REAL (dp), INTENT(OUT) :: rulpts(:)

! Dimensions:
! w(5, wtleng), g(3,wtleng), errcof(6), rulpts(wtleng)

!   Local variables.

INTEGER   :: i
REAL (dp) :: dim3g(14) = (/ 0.1900000000000000D+00, 0.5000000000000000D+00,  &
    0.7500000000000000D+00, 0.8000000000000000D+00, 0.9949999999999999D+00,  &
    0.9987344998351400D+00, 0.7793703685672423D+00, 0.9999698993088767D+00,  &
    0.7902637224771788D+00, 0.4403396687650737D+00, 0.4378478459006862D+00,  &
    0.9549373822794593D+00, 0.9661093133630748D+00, 0.4577105877763134D+00 /)

REAL (dp), PARAMETER :: dim3w(13,5) = RESHAPE( (/  &
0.7923078151105734D-02,  &
0.6797177392788080D-01, 0.1086986538805825D-02,  &
0.1838633662212829D+00, 0.3362119777829031D-01,  &
0.1013751123334062D-01, 0.1687648683985235D-02,  &
0.1346468564512807D+00, 0.1750145884600386D-02,  &
0.7752336383837454D-01, 0.2461864902770251D+00,  &
0.6797944868483039D-01, 0.1419962823300713D-01,  &

0.1715006248224684D+01,  &
- .3755893815889209D+00, 0.1488632145140549D+00,  &
- .2497046640620823D+00, 0.1792501419135204D+00,  &
0.3446126758973890D-02, - .5140483185555825D-02,  &
0.6536017839876425D-02, - .6513454939229700D-03,  &
- .6304672433547204D-02, 0.1266959399788263D-01,  &
- .5454241018647931D-02, 0.4826995274768427D-02,  &

0.1936014978949526D+01,  &
- .3673449403754268D+00, 0.2929778657898176D-01,  &
- .1151883520260315D+00, 0.5086658220872218D-01,  &
0.4453911087786469D-01, - .2287828257125900D-01,  &
0.2908926216345833D-01, - .2898884350669207D-02,  &
- .2805963413307495D-01, 0.5638741361145884D-01,  &
- .2427469611942451D-01, 0.2148307034182882D-01,  &

0.5170828195605760D+00,  &
0.1445269144914044D-01, - .3601489663995932D+00,  &
0.3628307003418485D+00, 0.7148802650872729D-02,  &
- .9222852896022966D-01, 0.1719339732471725D-01,  &
- .1021416537460350D+00, - .7504397861080493D-02,  &
0.1648362537726711D-01, 0.5234610158469334D-01,  &
0.1445432331613066D-01, 0.3019236275367777D-02,  &

0.2054404503818520D+01,  &
0.1377759988490120D-01, - .5768062917904410D+00,  &
0.3726835047700328D-01, 0.6814878939777219D-02,  &
0.5723169733851849D-01, - .4493018743811285D-01,  &
0.2729236573866348D-01, 0.3547473950556990D-03,  &
0.1571366799739551D-01, 0.4990099219278567D-01,  &
0.1377915552666770D-01, 0.2878206423099872D-02 /), (/ 13, 5 /))

!***FIRST EXECUTABLE STATEMENT D113RE

!   Assign values to W.

DO i = 1, 13
  w(1:5,i) = dim3w(i,1:5)
END DO

!   Assign values to G.

g = 0._dp
g(1,2) = dim3g(1)
g(1,3) = dim3g(2)
g(1,4) = dim3g(3)
g(1,5) = dim3g(4)
g(1,6) = dim3g(5)
g(1,7) = dim3g(6)
g(2,7) = g(1,7)
g(1,8) = dim3g(7)
g(2,8) = g(1,8)
g(1,9) = dim3g(8)
g(2,9) = g(1,9)
g(3,9) = g(1,9)
g(1,10) = dim3g(9)
g(2,10) = g(1,10)
g(3,10) = g(1,10)
g(1,11) = dim3g(10)
g(2,11) = g(1,11)
g(3,11) = g(1,11)
g(1,12) = dim3g(12)
g(2,12) = dim3g(11)
g(3,12) = g(2,12)
g(1,13) = dim3g(13)
g(2,13) = g(1,13)
g(3,13) = dim3g(14)

!   Assign values to RULPTS.

rulpts(1) = 1
rulpts(2) = 6
rulpts(3) = 6
rulpts(4) = 6
rulpts(5) = 6
rulpts(6) = 6
rulpts(7) = 12
rulpts(8) = 12
rulpts(9) = 8
rulpts(10) = 8
rulpts(11) = 8
rulpts(12) = 24
rulpts(13) = 24

!   Assign values to ERRCOF.

errcof(1) = 4
errcof(2) = 4.
errcof(3) = 0.5
errcof(4) = 3.
errcof(5) = 0.5
errcof(6) = 0.25

!***END D113RE

RETURN
END SUBROUTINE d113re


SUBROUTINE d09hre(ndim, wtleng, w, g, errcof, rulpts)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: D09HRE
      !DIR$ OPTIMIZE : 3
      !DIR$ CODE_ALIGN : 32 :: D09HRE
#endif
!***BEGIN PROLOGUE D09HRE
!***KEYWORDS basic integration rule, degree 9
!***PURPOSE  To initialize a degree 9 basic rule and null rules.
!***AUTHOR   Alan Genz, Computer Science Department, Washington
!            State University, Pullman, WA 99163-1210 USA
!***LAST MODIFICATION 88-05-20
!***DESCRIPTION  D09HRE initializes a degree 9 integration rule,
!            two degree 7 null rules, one degree 5 null rule and one
!            degree 3 null rule for the hypercube [-1,1]**NDIM.

!   ON ENTRY

!   NDIM   Integer.
!          Number of variables.
!   WTLENG Integer.
!          The number of weights in each of the rules.

!   ON RETURN
!   W      Real array of dimension (5,WTLENG).
!          The weights for the basic and null rules.
!          W(1,1),...,W(1,WTLENG) are weights for the basic rule.
!          W(I,1),...,W(I,WTLENG), for I > 1 are null rule weights.
!   G      Real array of dimension (NDIM, WTLENG).
!          The fully symmetric sum generators for the rules.
!          G(1, J), ..., G(NDIM, J) are the are the generators for the
!          points associated with the Jth weights.
!   ERRCOF Real array of dimension 6.
!          Heuristic error coefficients that are used in the
!          error estimation in BASRUL.
!   RULPTS Real array of dimension WTLENG.
!          A work array.

!***REFERENCES A. Genz and A. Malik,
!             "An Imbedded Family of Fully Symmetric Numerical
!              Integration Rules",
!              SIAM J Numer. Anal. 20 (1983), pp. 580-588.
!***ROUTINES CALLED-NONE
!***END PROLOGUE D09HRE

!   Global variables

INTEGER, INTENT(IN)    :: ndim, wtleng
REAL (dp), INTENT(OUT) :: w(:,:), g(:,:), errcof(:)
REAL (dp), INTENT(OUT) :: rulpts(:)

! Dimensions:
! w(5,wtleng), g(ndim,wtleng), errcof(6), rulpts(wtleng)

!   Local Variables

REAL (dp) :: ratio, lam0, lam1, lam2, lam3, lamp, twondm
INTEGER :: i, j

!***FIRST EXECUTABLE STATEMENT D09HRE


!     Initialize generators, weights and RULPTS

DO j = 1, wtleng
  DO i = 1, ndim
    g(i,j) = 0
  END DO
  DO i = 1, 5
    w(i,j) = 0
  END DO
  rulpts(j) = 2*ndim
END DO
twondm = 2**ndim
rulpts(wtleng) = twondm
IF (ndim > 2) rulpts(8) = (4*ndim* (ndim-1)* (ndim-2))/3
rulpts(7) = 4*ndim* (ndim-1)
rulpts(6) = 2*ndim* (ndim-1)
rulpts(1) = 1

!     Compute squared generator parameters

lam0 = 0.4707
lam1 = 4/ (15-5/lam0)
ratio = (1-lam1/lam0)/27
lam2 = (5-7*lam1-35*ratio)/ (7-35*lam1/3-35*ratio/lam0)
ratio = ratio* (1-lam2/lam0)/3
lam3 = (7-9* (lam2+lam1)+63*lam2*lam1/5-63*ratio)/  &
       (9-63* (lam2+lam1)/5+21*lam2*lam1-63*ratio/lam0)
lamp = 0.0625

!     Compute degree 9 rule weights

w(1,wtleng) = 1/ (3*lam0)**4/twondm
IF (ndim > 2) w(1,8) = (1-1/ (3*lam0)) / (6*lam1)**3
w(1,7) = (1-7* (lam0+lam1)/5+7*lam0*lam1/3) /  &
         (84*lam1*lam2* (lam2-lam0)* (lam2-lam1))
w(1,6) = (1-7* (lam0+lam2)/5+7*lam0*lam2/3) /  &
         (84*lam1*lam1* (lam1-lam0)* (lam1-lam2)) - w(1,7)*lam2/lam1  &
         - 2* (ndim-2)*w(1,8)
w(1,4) = (1-9* ((lam0+lam1+lam2) /7 - (lam0*lam1+lam0*lam2+  &
         lam1*lam2)/5)-3*lam0*lam1*lam2)/  &
         (18*lam3* (lam3-lam0)* (lam3-lam1)* (lam3-lam2))
w(1,3) = (1-9* ((lam0+lam1+lam3)/7- (lam0*lam1+lam0*lam3+  &
         lam1*lam3)/5)-3*lam0*lam1*lam3)/  &
         (18*lam2* (lam2-lam0)* (lam2-lam1)* (lam2-lam3)) -2* (ndim-1)*w(1,7)
w(1,2) = (1-9* ((lam0+lam2+lam3)/7- (lam0*lam2+lam0*lam3+  &
         lam2*lam3)/5)-3*lam0*lam2*lam3) /  &
         (18*lam1* (lam1-lam0)* (lam1-lam2)* (lam1-lam3)) -  &
         2* (ndim-1)* (w(1,7)+w(1,6)+ (ndim-2)*w(1,8))

!     Compute weights for 2 degree 7, 1 degree 5 and 1 degree 3 rules

w(2,wtleng) = 1/ (108*lam0**4)/twondm
IF (ndim > 2) w(2,8) = (1-27*twondm*w(2,9)*lam0**3)/ (6*lam1)**3
w(2,7) = (1-5*lam1/3-15*twondm*w(2,wtleng)*lam0**2* (lam0-lam1))/  &
         (60*lam1*lam2* (lam2-lam1))
w(2,6) = (1-9* (8*lam1*lam2*w(2,7)+twondm*w(2,wtleng)*lam0**2))/  &
         (36*lam1*lam1) - 2*w(2,8)* (ndim-2)
w(2,4) = (1-7* ((lam1+lam2)/5-lam1*lam2/3+twondm*w(2,wtleng)*lam0*   &
         (lam0-lam1)* (lam0-lam2)))/(14*lam3* (lam3-lam1)* (lam3-lam2))
w(2,3) = (1-7* ((lam1+lam3)/5-lam1*lam3/3+twondm*w(2,wtleng)*lam0*   &
         (lam0-lam1)* (lam0-lam3)))/ (14*lam2* (lam2-lam1)* (lam2-lam3)) &
         - 2* (ndim-1)*w(2,7)
w(2,2) = (1-7* ((lam2+lam3)/5-lam2*lam3/3+twondm*w(2,wtleng)*lam0* &
         (lam0-lam2)* (lam0-lam3)))/ (14*lam1* (lam1-lam2)* (lam1-lam3)) -  &
         2* (ndim-1)* (w(2,7)+w(2,6)+ (ndim-2)*w(2,8))
w(3,wtleng) = 5/ (324*lam0**4)/twondm
IF (ndim > 2) w(3,8) = (1-27*twondm*w(3,9)*lam0**3)/ (6*lam1)**3
w(3,7) = (1-5*lam1/3-15*twondm*w(3,wtleng)*lam0**2* (lam0-lam1))/  &
         (60*lam1*lam2* (lam2-lam1))
w(3,6) = (1-9* (8*lam1*lam2*w(3,7)+twondm*w(3,wtleng)*lam0**2))/  &
         (36*lam1*lam1) - 2*w(3,8)* (ndim-2)
w(3,5) = (1-7* ((lam1+lam2)/5-lam1*lam2/3+twondm*w(3,wtleng)*lam0*  &
         (lam0-lam1)* (lam0-lam2)))/(14*lamp* (lamp-lam1)* (lamp-lam2))
w(3,3) = (1-7* ((lam1+lamp)/5-lam1*lamp/3+twondm*w(3,wtleng)*lam0*  &
         (lam0-lam1)* (lam0-lamp)))/  &
         (14*lam2* (lam2-lam1)* (lam2-lamp)) - 2* (ndim-1)*w(3,7)
w(3,2) = (1-7* ((lam2+lamp)/5-lam2*lamp/3+twondm*w(3,wtleng)*lam0*  &
         (lam0-lam2)* (lam0-lamp)))/ (14*lam1* (lam1-lam2)* (lam1-lamp)) -  &
         2* (ndim-1)* (w(3,7)+w(3,6)+ (ndim-2)*w(3,8))
w(4,wtleng) = 2/ (81*lam0**4)/twondm
IF (ndim > 2) w(4,8) = (2-27*twondm*w(4,9)*lam0**3)/ (6*lam1)**3
w(4,7) = (2-15*lam1/9-15*twondm*w(4,wtleng)*lam0* (lam0-lam1))/  &
         (60*lam1*lam2* (lam2-lam1))
w(4,6) = (1-9* (8*lam1*lam2*w(4,7)+twondm*w(4,wtleng)*lam0**2))/  &
         (36*lam1*lam1) - 2*w(4,8)* (ndim-2)
w(4,4) = (2-7* ((lam1+lam2)/5-lam1*lam2/3+twondm*w(4,wtleng)*lam0*  &
         (lam0-lam1)* (lam0-lam2)))/(14*lam3* (lam3-lam1)* (lam3-lam2))
w(4,3) = (2-7* ((lam1+lam3)/5-lam1*lam3/3+twondm*w(4,wtleng)*lam0*  &
         (lam0-lam1)* (lam0-lam3)))/  &
         (14*lam2* (lam2-lam1)* (lam2-lam3)) - 2* (ndim-1)*w(4,7)
w(4,2) = (2-7* ((lam2+lam3)/5-lam2*lam3/3+twondm*w(4,wtleng)*lam0*  &
         (lam0-lam2)* (lam0-lam3)))/ (14*lam1* (lam1-lam2)* (lam1-lam3)) -  &
         2* (ndim-1)* (w(4,7)+w(4,6)+ (ndim-2)*w(4,8))
w(5,2) = 1/ (6*lam1)

!     Set generator values

lam0 = SQRT(lam0)
lam1 = SQRT(lam1)
lam2 = SQRT(lam2)
lam3 = SQRT(lam3)
lamp = SQRT(lamp)
DO i = 1, ndim
  g(i,wtleng) = lam0
END DO
IF (ndim > 2) THEN
  g(1,8) = lam1
  g(2,8) = lam1
  g(3,8) = lam1
END IF
g(1,7) = lam1
g(2,7) = lam2
g(1,6) = lam1
g(2,6) = lam1
g(1,5) = lamp
g(1,4) = lam3
g(1,3) = lam2
g(1,2) = lam1

!     Compute final weight values.
!     The null rule weights are computed from differences between
!     the degree 9 rule weights and lower degree rule weights.

w(1,1) = twondm
DO j = 2, 5
  DO i = 2, wtleng
    w(j,i) = w(j,i) - w(1,i)
    w(j,1) = w(j,1) - rulpts(i)*w(j,i)
  END DO
END DO
DO i = 2, wtleng
  w(1,i) = twondm*w(1,i)
  w(1,1) = w(1,1) - rulpts(i)*w(1,i)
END DO

!     Set error coefficients

errcof(1) = 5
errcof(2) = 5
errcof(3) = 1.
errcof(4) = 5
errcof(5) = 0.5
errcof(6) = 0.25

!***END D09HRE

RETURN
END SUBROUTINE d09hre


SUBROUTINE d07hre(ndim, wtleng, w, g, errcof, rulpts)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: D07HRE
      !DIR$ OPTIMIZE : 3
      !DIR$ CODE_ALIGN : 32 :: D07HRE
#endif
!***BEGIN PROLOGUE D07HRE
!***KEYWORDS basic integration rule, degree 7
!***PURPOSE  To initialize a degree 7 basic rule, and null rules.
!***AUTHOR   Alan Genz, Computer Science Department, Washington
!            State University, Pullman, WA 99163-1210 USA
!***LAST MODIFICATION 88-05-31
!***DESCRIPTION  D07HRE initializes a degree 7 integration rule,
!            two degree 5 null rules, one degree 3 null rule and one
!            degree 1 null rule for the hypercube [-1,1]**NDIM.

!   ON ENTRY

!   NDIM   Integer.
!          Number of variables.
!   WTLENG Integer.
!          The number of weights in each of the rules.
!          WTLENG MUST be set equal to 6.

!   ON RETURN
!   W      Real array of dimension (5,WTLENG).
!          The weights for the basic and null rules.
!          W(1,1),...,W(1,WTLENG) are weights for the basic rule.
!          W(I,1),...,W(I,WTLENG), for I > 1 are null rule weights.
!   G      Real array of dimension (NDIM, WTLENG).
!          The fully symmetric sum generators for the rules.
!          G(1, J), ..., G(NDIM, J) are the are the generators for the
!          points associated with the Jth weights.
!   ERRCOF Real array of dimension 6.
!          Heuristic error coefficients that are used in the
!          error estimation in BASRUL.
!   RULPTS Real array of dimension WTLENG.
!          A work array.

!***REFERENCES A. Genz and A. Malik,
!             "An Imbedded Family of Fully Symmetric Numerical
!              Integration Rules",
!              SIAM J Numer. Anal. 20 (1983), pp. 580-588.
!***ROUTINES CALLED-NONE
!***END PROLOGUE D07HRE

!   Global variables

INTEGER, INTENT(IN)    :: ndim, wtleng
REAL (dp), INTENT(OUT) :: w(:,:), g(:,:), errcof(:)
REAL (dp), INTENT(OUT) :: rulpts(:)

! Dimensions:
! w(5,wtleng), g(ndim,wtleng), errcof(6), rulpts(wtleng)

!   Local Variables

REAL (dp) :: ratio, lam0, lam1, lam2, lamp, twondm
INTEGER   :: i, j

!***FIRST EXECUTABLE STATEMENT D07HRE


!     Initialize generators, weights and RULPTS

DO j = 1, wtleng
  DO i = 1, ndim
    g(i,j) = 0
  END DO
  DO i = 1, 5
    w(i,j) = 0
  END DO
  rulpts(j) = 2*ndim
END DO
twondm = 2**ndim
rulpts(wtleng) = twondm
rulpts(wtleng-1) = 2*ndim* (ndim-1)
rulpts(1) = 1

!     Compute squared generator parameters

lam0 = 0.4707
lamp = 0.5625
lam1 = 4/ (15-5/lam0)
ratio = (1-lam1/lam0)/27
lam2 = (5-7*lam1-35*ratio)/ (7-35*lam1/3-35*ratio/lam0)

!     Compute degree 7 rule weights

w(1,6) = 1/ (3*lam0)**3/twondm
w(1,5) = (1-5*lam0/3)/ (60* (lam1-lam0)*lam1**2)
w(1,3) = (1-5*lam2/3-5*twondm*w(1,6)*lam0* (lam0-lam2))/  &
         (10*lam1* (lam1-lam2)) - 2* (ndim-1)*w(1,5)
w(1,2) = (1-5*lam1/3-5*twondm*w(1,6)*lam0* (lam0-lam1))/(10*lam2* (lam2-lam1))

!     Compute weights for 2 degree 5, 1 degree 3 and 1 degree 1 rules

w(2,6) = 1/ (36*lam0**3)/twondm
w(2,5) = (1-9*twondm*w(2,6)*lam0**2)/ (36*lam1**2)
w(2,3) = (1-5*lam2/3-5*twondm*w(2,6)*lam0* (lam0-lam2))/  &
         (10*lam1* (lam1-lam2)) - 2* (ndim-1)*w(2,5)
w(2,2) = (1-5*lam1/3-5*twondm*w(2,6)*lam0* (lam0-lam1))/(10*lam2* (lam2-lam1))
w(3,6) = 5/ (108*lam0**3)/twondm
w(3,5) = (1-9*twondm*w(3,6)*lam0**2)/ (36*lam1**2)
w(3,3) = (1-5*lamp/3-5*twondm*w(3,6)*lam0* (lam0-lamp))/  &
         (10*lam1* (lam1-lamp)) - 2* (ndim-1)*w(3,5)
w(3,4) = (1-5*lam1/3-5*twondm*w(3,6)*lam0* (lam0-lam1))/(10*lamp* (lamp-lam1))
w(4,6) = 1/ (54*lam0**3)/twondm
w(4,5) = (1-18*twondm*w(4,6)*lam0**2)/ (72*lam1**2)
w(4,3) = (1-10*lam2/3-10*twondm*w(4,6)*lam0* (lam0-lam2))/  &
         (20*lam1* (lam1-lam2)) - 2* (ndim-1)*w(4,5)
w(4,2) = (1-10*lam1/3-10*twondm*w(4,6)*lam0* (lam0-lam1))/ (20*lam2* (lam2-lam1))

!     Set generator values

lam0 = SQRT(lam0)
lam1 = SQRT(lam1)
lam2 = SQRT(lam2)
lamp = SQRT(lamp)
DO i = 1, ndim
  g(i,wtleng) = lam0
END DO
g(1,wtleng-1) = lam1
g(2,wtleng-1) = lam1
g(1,wtleng-4) = lam2
g(1,wtleng-3) = lam1
g(1,wtleng-2) = lamp

!     Compute final weight values.
!     The null rule weights are computed from differences between
!     the degree 7 rule weights and lower degree rule weights.

w(1,1) = twondm
DO j = 2, 5
  DO i = 2, wtleng
    w(j,i) = w(j,i) - w(1,i)
    w(j,1) = w(j,1) - rulpts(i)*w(j,i)
  END DO
END DO
DO i = 2, wtleng
  w(1,i) = twondm*w(1,i)
  w(1,1) = w(1,1) - rulpts(i)*w(1,i)
END DO

!     Set error coefficients

errcof(1) = 5.
errcof(2) = 5.
errcof(3) = 1.
errcof(4) = 5.
errcof(5) = 0.5
errcof(6) = 0.25

!***END D07HRE

RETURN
END SUBROUTINE d07hre


SUBROUTINE drlhre(ndim, center, hwidth, wtleng, g, w, errcof, numfun,  &
     funsub, scales, norms, basval, rgnerr, DIRECT)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: DRLHRE
      !DIR$ OPTIMIZE : 3
      !DIR$ CODE_ALIGN : 32 :: DRLHRE
#endif 
!***BEGIN PROLOGUE DRLHRE
!***KEYWORDS basic numerical integration rule
!***PURPOSE  To compute basic integration rule values.
!***AUTHOR   Alan Genz,  Computer Science Department, Washington
!            State University, Pullman, WA 99163-1210 USA
!***LAST MODIFICATION 90-02-06
!***DESCRIPTION DRLHRE computes basic integration rule values for a
!            vector of integrands over a hyper-rectangular region.
!            These are estimates for the integrals. DRLHRE also computes
!            estimates for the errors and determines the coordinate axis
!            where the fourth difference for the integrands is largest.

!   ON ENTRY

!   NDIM   Integer.
!          Number of variables.
!   CENTER Real array of dimension NDIM.
!          The coordinates for the center of the region.
!   HWIDTH Real Array of dimension NDIM.
!          HWIDTH(I) is half of the width of dimension I of the region.
!   WTLENG Integer.
!          The number of weights in the basic integration rule.
!   G      Real array of dimension (NDIM,WTLENG).
!          The fully symmetric sum generators for the rules.
!          G(1,J), ..., G(NDIM,J) are the are the generators for the
!          points associated with the Jth weights.
!   W      Real array of dimension (5,WTLENG).
!          The weights for the basic and null rules.
!          W(1,1),...,W(1,WTLENG) are weights for the basic rule.
!          W(I,1),...,W(I,WTLENG), for I > 1 are null rule weights.
!   ERRCOF Real array of dimension 6.
!          The error coefficients for the rules.
!          It is assumed that the error is computed using:
!           IF (N1*ERRCOF(1) < N2 and N2*ERRCOF(2) < N3)
!             THEN ERROR = ERRCOF(3)*N1
!             ELSE ERROR = ERRCOF(4)*MAX(N1, N2, N3)
!           ERROR = ERROR + EP*(ERRCOF(5)*ERROR/(ES+ERROR)+ERRCOF(6))
!          where N1-N4 are the null rules, EP is the error
!          for the parent
!          subregion and ES is the error for the sibling subregion.
!   NUMFUN Integer.
!          Number of components for the vector integrand.
!   FUNSUB Externally declared subroutine.
!          For computing the components of the integrand at a point X.
!          It must have parameters (NDIM,X,NUMFUN,FUNVLS).
!           Input Parameters:
!            X      Real array of dimension NDIM.
!                   Defines the evaluation point.
!            NDIM   Integer.
!                   Number of variables for the integrand.
!            NUMFUN Integer.
!                   Number of components for the vector integrand.
!           Output Parameters:
!            FUNVLS Real array of dimension NUMFUN.
!                   The components of the integrand at the point X.
!   SCALES Real array of dimension (3,WTLENG).
!          Scaling factors used to construct new null rules based
!          on a linear combination of two successive null rules
!          in the sequence of null rules.
!   NORMS  Real array of dimension (3,WTLENG).
!          2**NDIM/(1-norm of the null rule constructed by each of the
!          scaling factors.)

!   ON RETURN

!   BASVAL Real array of dimension NUMFUN.
!          The values for the basic rule for each component
!          of the integrand.
!   RGNERR Real array of dimension NUMFUN.
!          The error estimates for each component of the integrand.
!   DIRECT Real.
!          The coordinate axis where the fourth difference of the
!          integrand values is largest.

!***REFERENCES
!   A.C.Genz and A.A.Malik, An adaptive algorithm for numerical
!   integration over an N-dimensional rectangular region,
!   J.Comp.Appl.Math., 6:295-302, 1980.

!   T.O.Espelid, Integration Rules, Null Rules and Error
!   Estimation, Reports in Informatics 33, Dept. of Informatics,
!   Univ. of Bergen, 1988.

!***ROUTINES CALLED: DFSHRE, FUNSUB

!***END PROLOGUE DRLHRE

!   Global variables.

INTEGER, INTENT(IN)       :: wtleng, numfun, ndim
REAL (dp), INTENT(IN)     :: center(:), hwidth(:), w(:,:), errcof(:),  &
                             scales(:,:), norms(:,:)
REAL (dp), INTENT(IN OUT) :: g(:,:)
REAL (dp), INTENT(OUT)    :: basval(:), rgnerr(:), DIRECT

! Dimensions:
! center(ndim), x(ndim), hwidth(ndim), basval(numfun), rgnerr(numfun)
! null(numfun,8), w(5,wtleng), g(ndim,wtleng), errcof(6), scales(3,wtleng)
! norms(3,wtleng)

INTERFACE
   SUBROUTINE funsub(ndim, z, nfun, f)
    import :: dp
    IMPLICIT NONE
    !INTEGER, PARAMETER     :: dp = SELECTED_REAL_KIND(15, 60)
    INTEGER, INTENT(IN)    :: ndim, nfun
    REAL (dp), INTENT(IN)  :: z(:)
    REAL (dp), INTENT(OUT) :: f(:)
  END SUBROUTINE funsub
END INTERFACE

!   Local variables.

REAL (dp) :: rgnvol, difsum, difmax, frthdf, null(numfun,8), x(ndim)
INTEGER   :: i, j, k, divaxn
REAL (dp) :: search, ratio

!***FIRST EXECUTABLE STATEMENT DRLHRE


!       Compute volume of subregion, initialize DIVAXN and rule sums;
!       compute fourth differences and new DIVAXN (RGNERR is used
!       for a work array here). The integrand values used for the
!       fourth divided differences are accumulated in rule arrays.

rgnvol = 1
divaxn = 1
DO i = 1, ndim
  rgnvol = rgnvol*hwidth(i)
  x(i) = center(i)
  IF (hwidth(i) > hwidth(divaxn)) divaxn = i
END DO
CALL funsub(ndim, x, numfun, rgnerr)
DO j = 1, numfun
  basval(j) = w(1,1)*rgnerr(j)
  DO k = 1, 4
    null(j,k) = w(k+1,1)*rgnerr(j)
  END DO
END DO
difmax = 0
ratio = (g(1,3)/g(1,2))**2
DO i = 1, ndim
  x(i) = center(i) - hwidth(i)*g(1,2)
  CALL funsub(ndim, x, numfun, null(:,5))
  x(i) = center(i) + hwidth(i)*g(1,2)
  CALL funsub(ndim, x, numfun, null(:,6))
  x(i) = center(i) - hwidth(i)*g(1,3)
  CALL funsub(ndim, x, numfun, null(:,7))
  x(i) = center(i) + hwidth(i)*g(1,3)
  CALL funsub(ndim, x, numfun, null(:,8))
  x(i) = center(i)
  difsum = 0
  DO j = 1, numfun
    frthdf = 2* (1-ratio)*rgnerr(j) - (null(j,7)+null(j,8)) +  &
             ratio* (null(j,5)+null(j,6))
    
!           Ignore differences below roundoff
    
    IF (rgnerr(j)+frthdf/4 /= rgnerr(j)) difsum = difsum + ABS(frthdf)
    DO k = 1, 4
      null(j,k) = null(j,k) + w(k+1,2)*(null(j,5)+null(j,6)) +  &
      w(k+1,3)* (null(j,7)+null(j,8))
    END DO
    basval(j) = basval(j) + w(1,2)* (null(j,5)+null(j,6)) +  &
    w(1,3)* (null(j,7)+null(j,8))
  END DO
  IF (difsum > difmax) THEN
    difmax = difsum
    divaxn = i
  END IF
END DO
DIRECT = divaxn

!    Finish computing the rule values.

DO i = 4, wtleng
  CALL dfshre(ndim, center, hwidth, x, g(:,i), numfun, funsub, rgnerr,  &
              null(:,5))
  DO j = 1, numfun
    basval(j) = basval(j) + w(1,i)*rgnerr(j)
    DO k = 1, 4
      null(j,k) = null(j,k) + w(k+1,i)*rgnerr(j)
    END DO
  END DO
END DO

!    Compute errors.

DO j = 1, numfun
  
!    We search for the null rule, in the linear space spanned by two
!    successive null rules in our sequence, which gives the greatest
!    error estimate among all normalized (1-norm) null rules in this
!    space.
  
  DO i = 1, 3
    search = 0
    DO k = 1, wtleng
      search = MAX(search, ABS(null(j,i+1)+scales(i,k)*null(j,i))*norms(i,k))
    END DO
    null(j,i) = search
  END DO
  IF (errcof(1)*null(j,1) <= null(j,2) .AND.  &
      errcof(2)*null(j,2) <= null(j,3)) THEN
    rgnerr(j) = errcof(3)*null(j,1)
  ELSE
    rgnerr(j) = errcof(4)*MAX(null(j,1), null(j,2), null(j,3))
  END IF
  rgnerr(j) = rgnvol*rgnerr(j)
  basval(j) = rgnvol*basval(j)
END DO

!***END DRLHRE

RETURN
END SUBROUTINE drlhre


SUBROUTINE dfshre(ndim, center, hwidth, x, g, numfun, funsub, fulsms, funvls)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: DFSHRE
      !DIR$ OPTIMIZE : 3
      !DIR$ CODE_ALIGN : 32 :: DRSHRE
#endif
!***BEGIN PROLOGUE DFSHRE
!***KEYWORDS fully symmetric sum
!***PURPOSE  To compute fully symmetric basic rule sums
!***AUTHOR   Alan Genz, Computer Science Department, Washington
!            State University, Pullman, WA 99163-1210 USA
!***LAST MODIFICATION 88-04-08
!***DESCRIPTION DFSHRE computes a fully symmetric sum for a vector
!            of integrand values over a hyper-rectangular region.
!            The sum is fully symmetric with respect to the center of
!            the region and is taken over all sign changes and
!            permutations of the generators for the sum.

!   ON ENTRY

!   NDIM   Integer.
!          Number of variables.
!   CENTER Real array of dimension NDIM.
!          The coordinates for the center of the region.
!   HWIDTH Real Array of dimension NDIM.
!          HWIDTH(I) is half of the width of dimension I of the region.
!   X      Real Array of dimension NDIM.
!          A work array.
!   G      Real Array of dimension NDIM.
!          The generators for the fully symmetric sum. These MUST BE
!          non-negative and non-increasing.
!   NUMFUN Integer.
!          Number of components for the vector integrand.
!   FUNSUB Externally declared subroutine.
!          For computing the components of the integrand at a point X.
!          It must have parameters (NDIM, X, NUMFUN, FUNVLS).
!           Input Parameters:
!            X      Real array of dimension NDIM.
!                   Defines the evaluation point.
!            NDIM   Integer.
!                   Number of variables for the integrand.
!            NUMFUN Integer.
!                   Number of components for the vector integrand.
!           Output Parameters:
!            FUNVLS Real array of dimension NUMFUN.
!                   The components of the integrand at the point X.
!   ON RETURN

!   FULSMS Real array of dimension NUMFUN.
!          The values for the fully symmetric sums for each component
!          of the integrand.
!   FUNVLS Real array of dimension NUMFUN.
!          A work array.

!***ROUTINES CALLED: FUNSUB

!***END PROLOGUE DFSHRE

!   Global variables.

INTEGER, INTENT(IN)       :: ndim, numfun
REAL (dp), INTENT(IN)     :: center(:), hwidth(:)
REAL (dp), INTENT(IN OUT) :: x(:), g(:)
REAL (dp), INTENT(OUT)    :: fulsms(:), funvls(:)

INTERFACE
   SUBROUTINE funsub(ndim, z, nfun, f)
    import :: dp
    IMPLICIT NONE
    !INTEGER, PARAMETER     :: dp = SELECTED_REAL_KIND(15, 60)
    INTEGER, INTENT(IN)    :: ndim, nfun
    REAL (dp), INTENT(IN)  :: z(:)
    REAL (dp), INTENT(OUT) :: f(:)
  END SUBROUTINE funsub
END INTERFACE

!   Local variables.

INTEGER   :: ixchng, lxchng, i, j, l
REAL (dp) :: gl, gi

!***FIRST EXECUTABLE STATEMENT DFSHRE

fulsms(1:numfun) = 0

!     Compute centrally symmetric sum for permutation of G

20 DO i = 1, ndim
  x(i) = center(i) + g(i)*hwidth(i)
END DO
40 CALL funsub(ndim, x, numfun, funvls)
DO j = 1, numfun
  fulsms(j) = fulsms(j) + funvls(j)
END DO
DO i = 1, ndim
  g(i) = - g(i)
  x(i) = center(i) + g(i)*hwidth(i)
  IF (g(i) < 0) GO TO 40
END DO

!       Find next distinct permutation of G and loop back for next sum.
!       Permutations are generated in reverse lexicographic order.

DO i = 2, ndim
  IF (g(i-1) > g(i)) THEN
    gi = g(i)
    ixchng = i - 1
    DO l = 1, (i-1)/2
      gl = g(l)
      g(l) = g(i-l)
      g(i-l) = gl
      IF (gl <= gi) ixchng = ixchng - 1
      IF (g(l) > gi) lxchng = l
    END DO
    IF (g(ixchng) <= gi) ixchng = lxchng
    g(i) = g(ixchng)
    g(ixchng) = gi
    GO TO 20
  END IF
END DO

!     Restore original order to generators

DO i = 1, ndim/2
  gi = g(i)
  g(i) = g(ndim-i+1)
  g(ndim-i+1) = gi
END DO

!***END DFSHRE

RETURN
END SUBROUTINE dfshre


SUBROUTINE dtrhre(dvflag, ndim, numfun, sbrgns, values, errors, centrs,  &
     hwidts, greate, center, hwidth, dir)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: DTRHRE
      !DIR$ OPTIMIZE : 3
      !DIR$ CODE_ALIGN : 32 :: DTRHRE
#endif
!***BEGIN PROLOGUE DTRHRE
!***PURPOSE DTRHRE maintains a heap of subregions.
!***DESCRIPTION DTRHRE maintains a heap of subregions.
!            The subregions are ordered according to the size
!            of the greatest error estimates of each subregion(GREATE).

!   PARAMETERS

!     DVFLAG Integer.
!            If DVFLAG = 1, we remove the subregion with
!            greatest error from the heap.
!            If DVFLAG = 2, we insert a new subregion in the heap.
!     NDIM   Integer.
!            Number of variables.
!     NUMFUN Integer.
!            Number of components of the integral.
!     SBRGNS Integer.
!            Number of subregions in the heap.
!     VALUES Real array of dimension (NUMFUN,SBRGNS).
!            Used to store estimated values of the integrals
!            over the subregions.
!     ERRORS Real array of dimension (NUMFUN,SBRGNS).
!            Used to store the corresponding estimated errors.
!     CENTRS Real array of dimension (NDIM,SBRGNS).
!            Used to store the center limits of the stored subregions.
!     HWIDTS Real array of dimension (NDIM,SBRGNS).
!            Used to store the hwidth limits of the stored subregions.
!     GREATE Real array of dimension SBRGNS.
!            Used to store the greatest estimated errors in all subregions.
!     CENTER Real array of dimension NDIM.
!            Used as intermediate storage for the center of the subregion.
!     HWIDTH Real array of dimension NDIM.
!            Used as intermediate storage for the half width of the subregion.
!     DIR    Integer array of dimension SBRGNS.
!            DIR is used to store the directions for further subdivision.

!***ROUTINES CALLED-NONE
!***END PROLOGUE DTRHRE

!   Global variables.

INTEGER, INTENT(IN)       :: dvflag, ndim, numfun
INTEGER, INTENT(IN OUT)   :: sbrgns
REAL (dp), INTENT(IN OUT) :: values(:,:), errors(:,:)
REAL (dp), INTENT(IN OUT) :: centrs(:,:)
REAL (dp), INTENT(IN OUT) :: hwidts(:,:)
REAL (dp), INTENT(IN OUT) :: greate(:)
REAL (dp), INTENT(IN OUT) :: center(:), hwidth(:)
REAL (dp), INTENT(IN OUT) :: dir(:)

! Dimensions:
! values(numfun,*), errors(numfun,*), centrs(ndim,*), hwidts(ndim,*)
! center(ndim), hwidth(ndim)

!   Local variables.

!   GREAT  is used as intermediate storage for the greatest error of a
!          subregion.
!   DIRECT is used as intermediate storage for the direction of further
!          subdivision.
!   SUBRGN Position of child/parent subregion in the heap.
!   SUBTMP Position of parent/child subregion in the heap.

INTEGER   :: j, subrgn, subtmp
REAL (dp) :: great, DIRECT, error(numfun), value(numfun)

!***FIRST EXECUTABLE STATEMENT DTRHRE

!   Save values to be stored in their correct place in the heap.

great = greate(sbrgns)
DIRECT = dir(sbrgns)
DO j = 1, numfun
  error(j) = errors(j,sbrgns)
  value(j) = values(j,sbrgns)
END DO
DO j = 1, ndim
  center(j) = centrs(j,sbrgns)
  hwidth(j) = hwidts(j,sbrgns)
END DO

!    If DVFLAG = 1, we will remove the region
!    with greatest estimated error from the heap.

IF (dvflag == 1) THEN
  sbrgns = sbrgns - 1
  subrgn = 1
  20 subtmp = 2*subrgn
  IF (subtmp <= sbrgns) THEN
    IF (subtmp /= sbrgns) THEN
      
!   Find max. of left and right child.
      
      IF (greate(subtmp) < greate(subtmp+1)) THEN
        subtmp = subtmp + 1
      END IF
    END IF
    
!   Compare max.child with parent.
!   If parent is max., then done.
    
    IF (great < greate(subtmp)) THEN
      
!   Move the values at position subtmp up the heap.
      
      greate(subrgn) = greate(subtmp)
      DO j = 1, numfun
        errors(j,subrgn) = errors(j,subtmp)
        values(j,subrgn) = values(j,subtmp)
      END DO
      dir(subrgn) = dir(subtmp)
      DO j = 1, ndim
        centrs(j,subrgn) = centrs(j,subtmp)
        hwidts(j,subrgn) = hwidts(j,subtmp)
      END DO
      subrgn = subtmp
      GO TO 20
    END IF
  END IF
ELSE IF (dvflag == 2) THEN
  
!   If DVFLAG = 2, then insert new region in the heap.
  
  subrgn = sbrgns
  40 subtmp = subrgn/2
  IF (subtmp >= 1) THEN
    
!   Compare max.child with parent.
!   If parent is max, then done.
    
    IF (great > greate(subtmp)) THEN
      
!   Move the values at position subtmp down the heap.
      
      greate(subrgn) = greate(subtmp)
      DO j = 1, numfun
        errors(j,subrgn) = errors(j,subtmp)
        values(j,subrgn) = values(j,subtmp)
      END DO
      dir(subrgn) = dir(subtmp)
      DO j = 1, ndim
        centrs(j,subrgn) = centrs(j,subtmp)
        hwidts(j,subrgn) = hwidts(j,subtmp)
      END DO
      subrgn = subtmp
      GO TO 40
    END IF
  END IF
END IF

!    Insert the saved values in their correct places.

IF (sbrgns > 0) THEN
  greate(subrgn) = great
  DO j = 1, numfun
    errors(j,subrgn) = error(j)
    values(j,subrgn) = value(j)
  END DO
  dir(subrgn) = DIRECT
  DO j = 1, ndim
    centrs(j,subrgn) = center(j)
    hwidts(j,subrgn) = hwidth(j)
  END DO
END IF

!***END DTRHRE

RETURN
END SUBROUTINE dtrhre


#if 0
       !=================================================================
         Real-world example of DCUHRE usage (taken from ADDA software)
       !=================================================================
          
            Authors: P. C. Chaumet and A. Rahmani
c     Date: 03/10/2009

c     Purpose: This routine compute the integration of the Green Tensor
c     (field susceptibility tensor) of free space to improve the
c     convergence rate of the discrete dipole approximation method.

c     Reference: if you use this routine in your research, please
c     reference, as appropriate: P. C. Chaumet, A. Sentenac and
c     A. Rahmani "Coupled dipole method for scatterers with large
c     permittivity", Phys. Rev. E 70(3), 036606-6 (2004).

c     license: GNU GPL

      subroutine propaespacelibreintadda(Rij,k0a,arretecube,relreq,
     $                                   result)
      implicit none
      integer i,j
c     definition of the position of the dipole, observation, wavenumber
c     ,wavelength, spacing lattice
      real(kind=8) ::  k0a,arretecubem
      real(kind=8) ::  x,y,z,arretecube,k0,xx0,yy0,zz0
      real(kind=8) ::  Rij(3),result(12)
c     The structure of the result is the following:
c     Re(G11),Re(G12),Re(G13),Re(G22),Re(G23),Re(G33),Im(G11),...,Im(G33)

c     Variables needs for the integration
      integer  KEY, N, NF, NDIM, MINCLS, MAXCLS, IFAIL, NEVAL, NW
      parameter (nw=4000000,ndim=3,nf=12)
      real(kind=8) ::  A(NDIM), B(NDIM), WRKSTR(NW)
      real(kind=8) ::   ABSEST(NF), ABSREQ, RELREQ,err
      
      real(kind=8) ::  Id(3,3),Rab,Rvect(3)

      external fonctionigtadda

      common/k0xyz/k0,x,y,z,xx0,yy0,zz0

      x=Rij(1)
      y=Rij(2)
      z=Rij(3)
      k0=k0a
      arretecubem=arretecube*0.1d0

c     We perform the integration of the tensor
c     definition for the integration
      MINCLS = 1000
      MAXCLS = 1000000
      KEY = 0
      ABSREQ = 0.d0
      
      A(1)=-arretecube/2.d0
      A(2)=-arretecube/2.d0
      A(3)=-arretecube/2.d0
      B(1)=+arretecube/2.d0
      B(2)=+arretecube/2.d0
      B(3)=+arretecube/2.d0
      
      xx0=1.d0
      yy0=1.d0
      zz0=1.d0
      if (dabs(z).le.arretecubem) then
         zz0=0.d0
      endif
      if (dabs(x).le.arretecubem) then
         xx0=0.d0
      endif
      if (dabs(y).le.arretecubem) then
         yy0=0.d0
      endif

      call  DCUHRE(NDIM,NF,A,B, MINCLS, MAXCLS, fonctionigtadda,
     $      ABSREQ,RELREQ,KEY,NW,0,result,ABSEST,NEVAL,IFAIL, WRKSTR) 
      
      do N = 1,NF
         result(N)=result(N)/arretecube/arretecube/arretecube
      enddo

      if (ifail.ne.0) then
         write(*,*) 'IFAIL in IGT routine',IFAIL
      endif

      end
c*************************************************************
      subroutine fonctionigtadda(ndim,zz,nfun,f)
      implicit none
      integer n,ndim,nfun
      real(kind=8) ::  zz(ndim),f(nfun)
      
      integer i,j
      real(kind=8) ::  x,y,z,x0,y0,z0,k0,Id(3,3),Rab,Rtenseur(3,3)
     $     ,Rvect(3),xx0,yy0,zz0
      double complex propaesplibre(3,3),const1,const2
      common/k0xyz/k0,x,y,z,xx0,yy0,zz0

      x0=zz(1)
      y0=zz(2)
      z0=zz(3)

      Rab=0.d0
      Rvect(1)=(x-x0)
      Rvect(2)=(y-y0)
      Rvect(3)=(z-z0)

      do i=1,3
         do j=1,3
            Id(i,j)=0.d0
            if (i.eq.j) Id(i,i)=1.d0
            Rtenseur(i,j)=Rvect(i)*Rvect(j)
         enddo
         Rab=Rab+Rvect(i)*Rvect(i)
      enddo
      Rab=dsqrt(Rab)

c     normalise pour avoir le vecteur unitaire
      do i=1,3
         do j=1,3
            Rtenseur(i,j)=Rtenseur(i,j)/(Rab*Rab)
         enddo
      enddo
    
      const1=(Rab*(1.d0,0.d0))**(-3.d0)-(0.d0,1.d0)*k0*(Rab**(-2.d0))
      const2=k0*k0/Rab*(1.d0,0.d0)
      do i=1,3
         do j=1,3
            propaesplibre(i,j)=((3.d0*Rtenseur(i,j)-Id(i,j))*const1+
     *           (Id(i,j)-Rtenseur(i,j))*const2)*
     *           cdexp((0.d0,1.d0)*k0*Rab)
         enddo
      enddo

      f(1)=dreal(propaesplibre(1,1))
      f(2)=dreal(propaesplibre(1,2))*xx0*yy0
      f(3)=dreal(propaesplibre(1,3))*xx0*zz0
      f(4)=dreal(propaesplibre(2,2))
      f(5)=dreal(propaesplibre(2,3))*yy0*zz0
      f(6)=dreal(propaesplibre(3,3))

      f(7)=dimag(propaesplibre(1,1))
      f(8)=dimag(propaesplibre(1,2))*xx0*yy0
      f(9)=dimag(propaesplibre(1,3))*xx0*zz0
      f(10)=dimag(propaesplibre(2,2))
      f(11)=dimag(propaesplibre(2,3))*yy0*zz0
      f(12)=dimag(propaesplibre(3,3))

      end

      !=========================================================================
          Second example of real world usage, i.e. magnetic field computation
      !=========================================================================
      
      https://github.com/ORNL-Fusion/LIBSTELL/blob/master/Sources/Modules/virtual_casing_mod.f90
      
      ! Volume Integral
      
      SUBROUTINE bfield_volint_adapt_dbl(x,y,z,bx,by,bz,istat)
      IMPLICIT NONE
      ! INPUT VARIABLES
      real(kind=8) :: , INTENT(in)  :: x, y, z
      real(kind=8) :: , INTENT(out) :: bx, by, bz
      integer(kind=4) ::  , INTENT(inout) :: istat
      ! LOCAL VARIABLES
      LOGICAL            :: adapt_rerun
      integer(kind=4) ::  (KIND=8), PARAMETER :: ndim_nag = 3 ! theta,zeta,s
      integer(kind=4) ::  (KIND=8), PARAMETER :: nfun_nag = 3 ! Bx, By, Bz
      integer(kind=4) ::  (KIND=8), PARAMETER :: lenwrk_nag = IWRK
      integer(kind=4) ::  (KIND=8) :: maxcls_nag,mincls_nag, subs, restar, wrklen, rulcls, wrksbs, n, m, funcls
      real(kind=8) ::  :: absreq_nag, relreq_nag
      real(kind=8) ::  :: wrkstr_nag(lenwrk_nag)
      real(kind=8) ::  :: a_nag(ndim_nag), b_nag(ndim_nag), &
                          finest_nag(nfun_nag), absest_nag(nfun_nag)
      real(kind=8) :: , ALLOCATABLE :: vrtwrk(:)

#ifdef NAG
      EXTERNAL :: D01EAF

#else
      EXTERNAL :: dcuhre

#endif
      ! BEGIN SUBROUTINE
      IF (adapt_tol < 0) THEN
         ! Not implmented
         CALL bfield_volint_dbl(x,y,z,bx,by,bz)
         RETURN
      END IF
      a_nag(1:3) = zero
      b_nag(1:2) = one*pi2
      b_nag(3)   = one
      mincls_nag = MIN_CLS
      maxcls_nag = IWRK
      absreq_nag = adapt_tol       ! Talk to Stuart about proper values
      relreq_nag = adapt_rel ! Talk to Stuart about proper values
      finest_nag = zero
      absest_nag = zero
      x_nag      = x
      y_nag      = y
      z_nag      = z
      adapt_rerun = .true.
      subs = 1
      restar = 0
      DO WHILE (adapt_rerun)

#ifdef NAG
         CALL D01EAF(ndim_nag,a_nag,b_nag,mincls_nag,maxcls_nag,nfun_nag,funsub_nag_b3d,absreq_nag,&
                   relreq_nag,lenwrk_nag,wrkstr_nag,finest_nag,absest_nag,istat)
         IF (istat == 1 .or. istat == 3) THEN
            maxcls_nag = maxcls_nag*10
            mincls_nag = -1
            restar = 1
            WRITE(6,*) '!!!!!  WARNING Could not reach desired tollerance  !!!!!'
            WRITE(6,*) '  BX = ',finest_nag(1),' +/- ',absest_nag(1)
            WRITE(6,*) '  BY = ',finest_nag(2),' +/- ',absest_nag(2)
            WRITE(6,*) '  BZ = ',finest_nag(3),' +/- ',absest_nag(3)
            WRITE(6,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
         ELSE IF (istat < 0) THEN
            bx = zero
            by = zero
            bz = zero
            adapt_rerun=.false.
         ELSE
            bx = finest_nag(1)
            by = finest_nag(2)
            bz = finest_nag(3)
            adapt_rerun=.false.
         END IF

#else
         IF (.not.ALLOCATED(vrtwrk)) THEN
            wrklen = ((IWRK-ndim_nag)/(2*ndim_nag) + 1)*(2*ndim_nag+2*nfun_nag+2) + 17*nfun_nag + 256
            ALLOCATE(vrtwrk(wrklen),STAT=istat)
            IF (istat .ne. 0) THEN
               WRITE(6,*) ' ALLOCATION ERROR IN: bfield_volint_adapt_dbl'
               WRITE(6,*) '   VARIABLE: VRTWRK, SIZE: ',wrklen
               WRITE(6,*) '   ISTAT: ',istat
               RETURN
            END IF
         END IF
         CALL dcuhre(ndim_nag,nfun_nag,a_nag,b_nag,mincls_nag,maxcls_nag,funsub_nag_b3d,absreq_nag,&
                     relreq_nag,0,wrklen,restar,finest_nag,absest_nag,funcls,istat,vrtwrk)
         !DEALLOCATE(vrtwrk)
         IF (istat == 1) THEN
            maxcls_nag = maxcls_nag*10
            mincls_nag = funcls
            restar = 1
         ELSE IF (istat > 1) THEN
            bx = zero
            by = zero
            bz = zero
            adapt_rerun=.false.
            DEALLOCATE(vrtwrk)
         ELSE
            bx = finest_nag(1)
            by = finest_nag(2)
            bz = finest_nag(3)
            adapt_rerun=.false.
            DEALLOCATE(vrtwrk)
         END IF

#endif
      END DO
      nlastcall=mincls_nag
      RETURN
      ! END SUBROUTINE
      END SUBROUTINE bfield_volint_adapt_dbl
      
      !======================================  
      !         Vector integrand
      !======================================

       SUBROUTINE funsub_nag_b3d(ndim, vec, nfun, f)
      IMPLICIT NONE
      ! INPUT VARIABLES
      integer(kind=4) ::  , INTENT(in) :: ndim, nfun
      real(kind=8) :: , INTENT(in) :: vec(ndim)
      real(kind=8) :: , INTENT(out) :: f(nfun)
      ! LOCAL VARIABLES
      integer(kind=4) ::   :: ier
      real(kind=8) ::  :: bn, ax, ay, az , xs, ys, zs, gf, gf3
      ! BEGIN SUBROUTINE

      xs = zero; ys = zero; zs = zero
      ax = zero; ay = zero; az = zero
      CALL EZspline_interp(x3d_spl,vec(1),vec(2),vec(3),xs,ier)
      CALL EZspline_interp(y3d_spl,vec(1),vec(2),vec(3),ys,ier)
      CALL EZspline_interp(z3d_spl,vec(1),vec(2),vec(3),zs,ier)
      CALL EZspline_interp(jx3d_spl,vec(1),vec(2),vec(3),ax,ier)
      CALL EZspline_interp(jy3d_spl,vec(1),vec(2),vec(3),ay,ier)
      CALL EZspline_interp(jz3d_spl,vec(1),vec(2),vec(3),az,ier)

      gf   = one/DSQRT((x_nag-xs)*(x_nag-xs)+(y_nag-ys)*(y_nag-ys)+(z_nag-zs)*(z_nag-zs))
      gf3  = gf*gf*gf
      f(1) = norm_3d*(ay*(z_nag-zs)-az*(y_nag-ys))*gf3
      f(2) = norm_3d*(az*(x_nag-xs)-ax*(z_nag-zs))*gf3
      f(3) = norm_3d*(ax*(y_nag-ys)-ay*(x_nag-xs))*gf3
      !WRITE(427,*) xs,ys,zs
      RETURN
      ! END SUBROUTINE
      END SUBROUTINE funsub_nag_b3d
!=======================================================================
!https://github.com/mrachh/boxcodes3d-paper-examples/blob/master/src/Helmholtz/h3dtab_brute.f
!=======================================================================

        subroutine mksurhelm3dp(x0,y0,z0,ix,iy,iz,zk0,norder0,
     1     value,ifail)
c
c       This subroutine is the wrapper for computing
c       the integrals  
c     \int_{[-1,1]^3} e^{ikr}/r *
c          P_{ix-1}(x)*P_{iy-1}(y)*P_{iz-1}(z) dx dy dz \, ,
c        
c       where P_{n}(x) are legendre polynomials
c       and r = \sqrt{(x-x_{0})^2 + (y-y_{0})^2 + (z-z_{0})^2} 
c
c       Be careful, there are a few global variables
c
c       The kernel can be changed by appropriately changing
c       the function "fhelmgreen3d"
c
c       Input parameters:
c          x0,y0,z0 - coordinates of target location
c          ix,iy,iz - polynomial order in x,y, and z variables
c          norder - order of box code generation. 
c                   Must be greater than max(ix,iy,iz)
c       
c       Output parameters:
c          value - value of integral
c          ifail - ifail = 0 for successful computation of integral
c                     check other routines for error code otherwise
c          
c
c
c
c

      implicit none
      external fhelm3dp
      integer key, n, nf, ndim, mincls, maxcls, ifail, neval, nw
      parameter (ndim = 3, nw = 4000000, nf = 2)
      real *8 a(ndim), b(ndim)
      real *8, allocatable :: wrkstr(:)
      real *8 absest(nf), finest(nf), absreq, relreq
      real *8 xtarg,ytarg,ztarg
      real *8 x0, y0, z0
      complex *16 value, zk, zk0
      complex *16 im
      data im / (0.0d0,1.0d0) /
      integer ix,iy,iz,ixpol,iypol,izpol,norder,norder0
      common /cbh3dtab_brute/ xtarg,ytarg,ztarg,ixpol,iypol,izpol,
     1     norder,zk
c$omp threadprivate(/cbh3dtab_brute/)      

      xtarg = x0
      ytarg = y0
      ztarg = z0

      allocate(wrkstr(nw))

      ixpol = ix
      iypol = iy
      izpol = iz

      norder = norder0
      zk = zk0


      do 10 n = 1,ndim
         a(n) = -1.0d0
         b(n) =  1.0d0
   10 continue
      mincls = 0
      maxcls = 4000000
      key = 0
      absreq = 1d-12
      relreq = 1d-12
      ifail = 0

      call dcuhre(ndim, nf, a, b, mincls, maxcls, fhelm3dp, 
     1      absreq, relreq, key, nw, 0, finest, absest, neval,
     2      ifail, wrkstr)


      value = finest(1) + im*finest(2)

      return
      end


      subroutine fhelm3dp(ndim, z, nfun, f)
      implicit none
      integer ndim, nfun
      real *8 z(ndim), f(nfun), rx, ry, rz
      real *8 rr,reps,dgreen
      real *8 xtarg,ytarg,ztarg
      integer ixpol,iypol,izpol,norder
      real *8 polsx(norder),polsy(norder),polsz(norder)
      real *8 h2
      complex *16 im, zf, zk
      data im / (0.0d0,1.0d0) /
      common /cbh3dtab_brute/ xtarg,ytarg,ztarg,ixpol,iypol,izpol,
     1     norder,zk
c$omp threadprivate(/cbh3dtab_brute/)      
      
      
      rx = z(1) - xtarg
      ry = z(2) - ytarg
      rz = z(3) - ztarg

      call legepols(z(1),norder-1,polsx)
      call legepols(z(2),norder-1,polsy)
      call legepols(z(3),norder-1,polsz)



      rr = dsqrt(rx*rx + ry*ry + rz*rz)

      zf = exp(im*zk*rr)*polsx(ixpol)*polsy(iypol)*polsz(izpol)/rr
      f(1) = dreal(zf)
      f(2) = dimag(zf)

      return
      end

#endif

END MODULE dcuhre_quadrature
