#if 0
*********************************************************
* Orignal BAYESPACK source code (author: Alan Genz, minor 
* adaptions by Bjoern Bornkamp)
*********************************************************
*
* 
*   BAYESPACK : 
*   A collection of numerical integration software for Bayesian analysis
* 
*   The main subroutine is BANINT, with simple driver BAYSNT. The only 
*   subroutines not in this file called by BANINT are ADBAYS, RNRTMX,
*   HRMSYM and RANRTH.
*
*
*
#endif
      SUBROUTINE BANINT( M, RELREQ, MAXVLS, RS, MN,                   &
                        PROBLM, MU, C, PRU, NUMTRN, METHOD,          &
                        NRMCON, MEANS, ERRORS, COVRNC, INFORM )
                 use mod_kinds, only : i4,dp

#if 0
*   
*        Bayesian Analysis Integration Subroutine
*
*     Input:
*      M - integer number of variables, 1 <= M <= 20.
*      RELREQ - real requested relative accuracy.
*      MAXVLS - integer limit on the number of posterior function 
*                 values allowed.
*      RS - integer restart parameter; if RS = 0, then it is assumed that
*             this is the first call of BANINT and, if NUMTRN > 0, the
*              transformation parameters are determined.
*      USRLGP - name of external real function for computation log
*                 of posterior. It must input parameter X, a real M-vector
*                 that defines the evaluation point.
*      MN - integer length final MEANS vector, MN >= M.
*      USRMNS - name of external subroutine for computation 
*                   of posterior means. This is used when MN > M
*                   and the user requires additional MEANS.
*               It must have parameters ( X, FUN )
*               Input parameter:
*                 X - real M-vector, the evaluation point.
*               Output parameter:
*                 FUN - real (MN-M)-vector that gives integrand values at X
*                 (excluding posterior) for computation of additional means.
*      PROBLM - character string for name of problem.
*      MU - real M-vector of starting values for posterior mode 
*                 optimization.
*      C - real M*(M+1)/2-vector, lower triangular standardizing
*               transformation array, stored by rows, and used for input
*               only when RS > 0.
*      PRU - integer output unit number.
*               If PRU = 0, no output is produced.
*               If PRU > 0, some transformation information, MEANS, final 
*                   errors and standard deviations are output to unit PU.
*               If PRU < 0, modal covariance Cholesky factor, modal
*                   covariance matrix, transformation information, MEANS, 
*                   errors, final standard deviations and final 
*                   covariance matrix are output to unit |PRU|.
*      NUMTRN - integer used to control types of transformations used. 
*               If NUMTRN = 0, no transformation is used.
*               If 0 < NUMTRN < 9, a standardizing transformation, with 
*                  (for METHOD < 20) multivariate Student-t(NUMTRN) model.
*               If NUMTRN = 10, a standardizing transformation, with
*                  (for METHOD < 20) multivariate normal model.
*               If NUMTRN = 20, a scaled standardizing transformation, with
*                  (for METHOD < 20) split-t. 
*      METHOD - integer method type:
*               If METHOD = 0, a Monte-Carlo importance sampling method.
*               If METHOD = 1, a global lattice rule method.
*               If METHOD = 10, a subregion adaptive method.
*               If METHOD = 20, a mixed spherical-radial method.
*               If METHOD = 30, a Gauss-Hermite product rules method.
*               If METHOD = 31, a Modified Gauss-Hermite rules method.
*               If METHOD = 32, a stochastic radial-spherical method.
*
*    Output:
*      RS - integer restart parameter; RS = 1 on exit.
*      MU - real M-vector for computed posterior mode.
*      C - real M*(M+1)/2-vector, lower triangular standardizing
*               transformation array, stored by rows; when RS = 0 and
*               NUMTRN > 0, this is the Cholesky factor of the modal 
*               covariance matrix..
*      NRMCON - real normalizing constant.
*      MEANS - real MN-vector of means.
*      ERRORS - real MN-vector of approximate errors in means.
*      COVRNC - real lower left posterior covariance matrix stored by
*                    rows as a long vector of length M*(M+1)/2
*      INFORM - integer completion information paramter:
*               If INFORM = 0, then integration was completed with
*                    estimated relative accuracy less than RELREQ.
*               If INFORM = 1, then integration could not completed with
*                    estimated relative accuracy less than RELREQ.
*
* 
#endif
      EXTERNAL USRLGP, USRMNS, POSTSB
      CHARACTER*(*) PROBLM
      REAL(kind=dp) RELREQ, USRLGP, MU(*), C(*)
      REAL(kind=dp) NRMCON, MEANS(*), ERRORS(*), COVRNC(*)
      INTEGER(kind=i4) M, MN, MAXVLS, RS, PU, PRU, NUMTRN, METHOD, INFORM
      CHARACTER DNSTYP*4, FRMATA*50, FRMATB*70, FRMATC*70
      INTEGER(kind=i4) MX, NF, NFMX, NUNRML, OLDMTH, OLDTRN
      PARAMETER ( MX = 20, NFMX = MX*(MX+3)/2, NUNRML = 30 )
      INTEGER(kind=i4) NU(2,MX), I, J, IJ, IR, CLTOTL, NCLS, INFRMP
      REAL(kind=dp) COVTMP( MX*(MX+1)/2 ), DELTA(2,MX)
      REAL(kind=dp) ESTERR(0:NFMX), FINEST(0:NFMX), ZERO, SQTWPI
      REAL(kind=dp) LGPSMX, DETERM, NULIM, NUCON
      PARAMETER ( ZERO = 0, SQTWPI =  2.5066282746310005D0 )
      SAVE OLDMTH, OLDTRN
      DATA OLDMTH, OLDTRN / 2*-1 /
      PU = ABS(PRU)
      IF ( RS .EQ. 0 .OR. OLDMTH.NE.METHOD .OR. OLDTRN.NE.NUMTRN ) THEN
         CALL POSTNT( M, METHOD, NUMTRN, C, MU, DELTA, NU,                  &
             LGPSMX, INFRMP, USRLGP, USRMNS )
      ELSE
         CALL POSTNT( -M, METHOD, NUMTRN, C, MU, DELTA, NU,                 &
             LGPSMX, INFRMP, USRLGP, USRMNS )
      END IF
      IF ( PU .GT. 0 ) THEN
         IF ( RS.EQ.0 .OR. OLDMTH.NE.METHOD .OR. OLDTRN.NE.NUMTRN ) THEN
            CALL INTPR("BANINT integration results", 26, PU, 0) 
            NUCON = 1
            NULIM = NUMTRN
            IF ( 0.LT.NUMTRN .AND. NUMTRN.LT.10 .AND. METHOD .LT. 20 )      &
     &          NUCON = SQRT( ( NULIM + M )/NULIM ) 
            DETERM = 1/NUCON**M
            DO I = 1,M
               DETERM = DETERM*SQTWPI*C(I*(I+1)/2)
            END DO
            CALL INTPR("Optimization Results", 20, PU, 0)
            CALL DBLEPR("Laplace Approx. for Constant is", 31,              &
                DETERM/EXP(LGPSMX),1)
            CALL DBLEPR("Log Posterior Max", 17, -LGPSMX, 1)
            CALL DBLEPR("Mode Vector", 11, MU, M)
            DO I = 1, M*(M+1)/2
               COVRNC(I) = C(I)
            END DO
            CALL DBLEPR("Modal Covariance Matrix Cholesky Factor", 39,     &
                COVRNC, M*(M+1)/2)
            CALL CHOLPD( M, COVRNC )
            CALL DBLEPR("Modal Covariance Matrix",23, COVRNC, M*(M+1)/2)
            CALL INTPR("Integration Results", 19, PU, 0)
            IF ( METHOD .LT. 20 ) THEN
               CALL INTPR("Transformation Types", 20, PU, 0)
               DO I = 1, M
                  DO J = 1, 2
                     CALL INTPR(DNSTYP(NU(J,I),DELTA(J,I),NUNRML), 4,       &
                         PU, 0)
                     END DO
                  END DO
            END IF
            IF ( NUMTRN .EQ. 20 ) THEN
               CALL DBLEPR("Delta Scale Factors", 19, DELTA, 2*M)
            END IF
         END IF
      END IF
!*     
!*     Initialize Variables for Integration Subroutine
!*     
      NF = 1 + MN  + M*(M+1)/2
      IF ( OLDMTH .EQ. METHOD ) THEN
         IR = RS
      ELSE
         IR = 0
         OLDMTH = METHOD
      END IF
!*     
!*     Initialize R random number generator
!*     
      CALL RNDSTART()
!*     
!*     Call Integration Subroutine
!*     
      CALL INTGRT( PU, METHOD, M, NF, MAXVLS, POSTSB, RELREQ,               &
          IR, FINEST, ESTERR, NCLS, INFORM )
      CALL RNDEND()
      IF ( NCLS .GT. 0 .AND. INFORM .LT. 2 ) THEN
!*     
!*     Compute covariance matrix and produce output
!*     
         NRMCON = FINEST(0)
         IF ( NRMCON  .GT. 0 ) THEN
            IJ = 0
            DO I = 1,M
               MEANS(I)   = FINEST(I)/NRMCON
               ERRORS(I)  = ESTERR(I)/NRMCON
               DO J = 1,I
                  IJ = IJ + 1
                  COVRNC(IJ) = FINEST(M+IJ)/NRMCON - MEANS(I)*MEANS(J)
               END DO
            END DO
            IF ( PU .GT. 0 ) THEN
               CALL DBLEPR("Normalization Constant", 22,                   &
                   NRMCON/EXP(LGPSMX), 1)
               CALL DBLEPR("Error Estimate", 14, ESTERR(0)/EXP(LGPSMX),    &
                   1)
               CALL DBLEPR("Means", 5, MEANS, M)
               CALL DBLEPR("Error Estimates", 15, ERRORS, M)
               CALL DBLEPR("Covariance Matrix", 17, COVRNC, M*(M+1)/2)
            END IF
            IF ( MN .GT. M ) THEN
               DO I = M+1, MN
                  MEANS(I) = FINEST( M*(M+1)/2 + I )/NRMCON
                  ERRORS(I) = ESTERR( M*(M+1)/2 + I )/NRMCON
               END DO
               IF ( PU .GT. 0 ) THEN
                  CALL INTPR("Additional Means and Errors", 27, PU, 0)
                  DO I = M+1,MN
                     CALL DBLEPR("Mean", 4, MEANS(I), 1)
                     CALL DBLEPR("Error Est.", 10, ERRORS(I), 1)
                  END DO
               END IF
            END IF
            INFORM = 0
            IF( ESTERR(0) .GT. RELREQ*NRMCON ) INFORM = 1
            DO I = 1, MN
               IF( ERRORS(I) .GT. RELREQ*ABS( MEANS(I) ) ) INFORM = 1
            END DO
            RS = 1
         ELSE
            INFORM = 1
            IF ( PU .GT. 0 ) THEN
               CALL INTPR("WARNING: Apprx. Normalizing Constant <= 0 !",    &
                   44, PU, 0)
            END IF
         END IF
         NRMCON = NRMCON
         MAXVLS = NCLS
         RELREQ = ESTERR(0)
      END IF
      END
     
      SUBROUTINE INTGRT( PU, METHOD, M, NF, MAXCLS, POSTSB, RELREQ,        &
                       IR, FINEST, ESTERR, NCLS, INFORM )
                 use mod_kinds, only : i4,dp
#if 0                       
*   
*     Integration Method Calling Subroutine
*
*     Input:
*      PU - Integer flag to control prints to standard output. 
*               If PU = 0, no output is produced.
*               If PU > 0, name of method is output to unit PU.
*      METHOD - Integer method type:
*               If METHOD = 0, a Monte-Carlo importance sampling method.
*               If METHOD = 1, a global lattice rule method.
*               If METHOD = 10, a subregion adaptive method.
*               If METHOD = 20, a mixed spherical-radial method.
*               If METHOD = 30, a Gauss-Hermite product rules method.
*               If METHOD = 31, a Modified Gauss-Hermite rules method.
*               If METHOD = 32, a stochastic radial-spherical method.
*      M - Integer number of variables.
*      RELREQ - Real requested relative accuracy.
*      MAXVLS - Integer limit on the number of posterior function 
*                 values allowed.
*
#endif
      EXTERNAL POSTSB
      INTEGER(kind=i4) PU, METHOD, M, NF, MAXCLS, IR, NCLS, INFORM
      REAL(kind=dp) RELREQ, ESTERR(0:*), FINEST(0:*)
      INTEGER(kind=i4) MX, I, MINCLS, NW, KEY
      PARAMETER ( MX = 20, NW = 5000*MX**2 )
      REAL(kind=dp) AB, A(MX), B(MX), WRKSTR(NW)
      SAVE A, B, WRKSTR
      IF ( PU .GT. 0 ) THEN
         IF ( METHOD .EQ. 0 ) THEN
            CALL INTPR("Monte Carlo Integration", 23, PU, 0)
         ELSE IF ( METHOD .EQ. 1 ) THEN
            CALL INTPR("Lattice Rule Integration", 24, PU, 0)
         ELSE IF ( METHOD .EQ. 10 ) THEN
            CALL INTPR("Subregion Adaptive Integration", 30, PU, 0)
         ELSE IF ( METHOD .EQ. 20 ) THEN
            CALL INTPR("Mixed Spherical-Radial Integration",34,PU, 0)
         ELSE IF ( METHOD .EQ. 30 ) THEN
            CALL INTPR("Gauss-Hermite Integration", 25, PU, 0)
         ELSE IF ( METHOD .EQ. 31 ) THEN
            CALL INTPR("Modified Gauss-Hermite Integration",34,PU,0)
         ELSE IF ( METHOD .EQ. 32 ) THEN
            CALL INTPR("Stochastic Radial-Spherical Integ.",34,PU,0)
         ELSE IF ( METHOD .EQ. 33 ) THEN
            CALL INTPR("Adaptive Radial-Spherical Integr.", 33,PU,0)
         ELSE IF ( METHOD .EQ. 34 ) THEN
            CALL INTPR("Monte Carlo Radial-Spherical Int.", 33,PU,0)
         ELSE IF ( METHOD .EQ. 35 ) THEN
            CALL INTPR("Lattice Rule  Radial-Spherical Int.", 35,PU,0)
         ELSE  
            CALL INTPR("Not implemented method selected", 31, PU, 0)
         END IF
      END IF
      IF ( MAXCLS .GT. 0 ) THEN
         IF ( IR .EQ. 0 .AND. METHOD .LT. 20 ) THEN
            DO I = 1, M
               A(I) = 0
               B(I) = 1
            END DO
         END IF
         AB = 0
         MINCLS = 0
         IF ( METHOD .EQ. 0 ) THEN
            CALL VCRUDE( M, NF, A, B, MAXCLS, POSTSB, AB, RELREQ,         &
                IR, FINEST, ESTERR, NCLS, INFORM, WRKSTR )
         ELSE IF ( METHOD .EQ. 1 ) THEN
            CALL VKROBV( M, NF, A, B, MINCLS, MAXCLS, POSTSB, AB,         &
                RELREQ, IR, FINEST, ESTERR, NCLS, INFORM, WRKSTR )
         ELSE IF ( METHOD .EQ. 10 ) THEN
            KEY = 2
            CALL ADBAYS( M, NF, A, B, MINCLS, MAXCLS, POSTSB, AB,RELREQ,  &
                KEY, NW, IR, FINEST, ESTERR, NCLS, INFORM, WRKSTR )
         ELSE IF ( METHOD .EQ. 20 ) THEN
            CALL RNRTMX( M, NF, MAXCLS, POSTSB, AB, RELREQ,               &
                IR, FINEST, ESTERR, NCLS, INFORM, NW, WRKSTR )
            IF ( INFORM .EQ. 2 ) INFORM = 1
         ELSE IF ( METHOD .EQ. 30 ) THEN
            CALL HERMIT( M, NF, MINCLS, MAXCLS, POSTSB, AB, RELREQ,       &
                IR, FINEST, ESTERR, NCLS, INFORM, WRKSTR )
         ELSE IF ( METHOD .EQ. 31 ) THEN
            CALL HRMSYM( M, NF, MINCLS, MAXCLS, POSTSB, AB, RELREQ,       &
                IR, FINEST, ESTERR, NCLS, INFORM, WRKSTR )
         ELSE IF ( METHOD .EQ. 32 ) THEN
            KEY = 2
            CALL RANRTH( M, NF, MAXCLS, POSTSB, AB, RELREQ, IR,           &
                KEY, FINEST, ESTERR, NCLS, INFORM, WRKSTR )
         ELSE IF ( METHOD .EQ. 33 ) THEN
            KEY = 0
            CALL ADBYSR( M, NF, MINCLS, MAXCLS, POSTSB, AB, RELREQ, IR,   &
                KEY, FINEST, ESTERR, NCLS, INFORM, NW, WRKSTR )
         ELSE IF ( METHOD .EQ. 34 ) THEN
            CALL VCRDSR( M, NF, MAXCLS, POSTSB, AB, RELREQ,               &
                IR, FINEST, ESTERR, NCLS, INFORM, WRKSTR )
         ELSE IF ( METHOD .EQ. 35 ) THEN
            CALL VKRBVR( M, NF, MINCLS, MAXCLS, POSTSB, AB,               &
                RELREQ, IR, FINEST, ESTERR, NCLS, INFORM, WRKSTR )
         ELSE
            NCLS = 0
            FINEST(0) = 0
            ESTERR(0) = 1
         END IF
      END IF
      END



      CHARACTER*4 FUNCTION DNSTYP( NU, DELTA, NUNRML )
!*
!*     Produces string for name of transformation
!*
      INTEGER(kind=i4) NU, NUNRML, NT
      REAL(kind=dp) DELTA
      IF ( NU .LE. 0 ) THEN
         IF ( DELTA .EQ. 1 ) THEN
            DNSTYP = 'None'
         ELSE
            DNSTYP = 'Scld'
         END IF
      ELSE IF ( NU .LT. NUNRML ) THEN
         NT = MOD( NU, 10 )
         IF ( NU .EQ. NT ) THEN
            DNSTYP = ' T-'//CHAR( ICHAR('0') + NT )
         ELSE
            DNSTYP = 'T-'//CHAR(ICHAR('0')+NU/10)//CHAR(ICHAR('0')+NT)
         END IF
      ELSE
         DNSTYP = 'Nrml'
      ENDIF
      END
!*
!*
!*
      SUBROUTINE MOMNTS( M, X, LGSMFC, NF, FUN )
          use mod_kinds, only : i4,dp
          use omp_lib
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: MOMNTS
           !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: MOMNTS
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: MOMNTS
#endif  
!*
!*     For computing function values for moments.
!*
      INTEGER(kind=i4) M, NF, I, J, IJ
      REAL(kind=dp) X(*), FUN(0:*), LGSMFC
      IF ( LGSMFC .GT. -100 ) THEN
         FUN(0) = EXP(LGSMFC)
         IJ = M
         DO I = 1,M
            FUN(I) = FUN(0)*X(I)
            !$OMP SIMD REDUCTION(:*FUN) ALIGNED(FUN,X:64)
            DO J = 1,I
               FUN(IJ+J) = FUN(I)*X(J)
            END DO
            IJ = IJ + I
         END DO
!*
!*     Call for additional means.
!*
         IF ( NF .GT. IJ+1 ) THEN
            CALL MEANSB( X, FUN(IJ+1) )
            DO I = IJ+1, NF-1
               FUN(I) = FUN(0)*FUN(I)
            END DO
         END IF
      ELSE
         DO I = 0, NF - 1
            FUN(I) = 0
         END DO
      ENDIF
      END
!*
!*
!*
      SUBROUTINE POSTSB( M, Z, NFUN, FUN )
            use mod_kinds, only : i4,dp
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: POSTSB
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: POSTSB
#endif  
      EXTERNAL USRPST, USRMNS
!*
!*     To compute posterior and moments at transformed Z.
!*
      REAL(kind=dp) Z(*), FUN(*)
      INTEGER(kind=i4) N, NFUN, I
      REAL(kind=dp) BZLPST
      PARAMETER ( N = 20 )
      REAL(kind=dp) LGPMXO, CO(*),        MUO(*), DELTO(2,*)
      REAL(kind=dp) LGPMXN, C(N*(N+1)/2), MU(N),  DELTA(2,N)
      INTEGER(kind=i4) INFORM, M, METHOD, NUMTRN, NMTRNO, NU(2,N), NUO(2,*)
      REAL(kind=dp) LGSMFC, LGJACB, X(N)
      SAVE C, MU, LGPMXN, NUMTRN, DELTA, NU
!*
!*     Compute transformed Z and posterior function values
!*
      CALL COVTRN( M, C, MU, NUMTRN, DELTA, NU, LGJACB, Z, X )       
      LGSMFC = LGPMXN + LGJACB + BZLPST(X)
      CALL MOMNTS( M, X, LGSMFC, NFUN, FUN )
      RETURN
!*
!*    Entry point for intialization and communcation:
!*      a) Initialize mode and modal covariance arrays and scale factor.
!*      b) Compute transformation parameters.
!*
      ENTRY POSTNT( M, METHOD, NMTRNO, CO, MUO, DELTO, NUO,                &
                   LGPMXO, INFORM, USRPST, USRMNS )
      CALL COMMUN(  M, METHOD, NMTRNO, CO, MUO, DELTO, NUO, LGPMXO,        &
              USRPST, INFORM, NUMTRN, C,  MU,  DELTA, NU,  LGPMXN )
      IF ( M .GT. 0 ) CALL BZLPNT( USRPST, USRMNS )
      END
      REAL(kind=dp) FUNCTION BZLPST(X)
!*
!*     To compute log of posterior density function
!*
      REAL(kind=dp) X(*), USRFNC
      BZLPST = USRFNC(X)
      END
!*
!*
!*
      SUBROUTINE COMMUN( MI, METHOD, NMTRNO, CO,MUO, DELTO,NUO, LGPMXO,    &
                    LGPOST, INFORM, NMTRNN, CN,MUN, DELTN,NUN, LGPMXN )
               use mod_kinds, only : i4,dp
               use omp_lib
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: COMMUN
           !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: COMMUN
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: COMMUN
#endif                 
!*
!*     For determination of transformation information and
!*     communication of transformation information to integrand.
!*
      EXTERNAL LGPOST
      REAL(kind=dp) LGPMXO, CO(*), MUO(*), DELTO(2,*), LGPOST
      REAL(kind=dp) LGPMXN, CN(*), MUN(*), DELTN(2,*)
      INTEGER(kind=i4) INFORM, I,M,MI, METHOD, NMTRNN, NMTRNO, NUO(2,*), NUN(2,*)
      INFORM = 0
      M = ABS(MI)
      !$OMP SIMD aligned(MUN,MUO:64)
      DO I = 1,M
         MUN(I) = MUO(I)
      END DO
      IF ( MI .GT. 0 ) THEN
         NMTRNN = NMTRNO
         IF ( NMTRNO .GT. 0 ) THEN 
            CALL MODCOV( M, CN, MUN, LGPMXN, LGPOST, INFORM )
!*
!*     Compute Cholesky factor for covariance matrix.
!*
            CALL CHOLSK( M, CN )
            IF ( INFORM .GT. 0 ) NMTRNN = MAX( NMTRNN, 10 )
         END IF
         CALL TRANSF( M, CN, MUN, DELTN, NUN, LGPOST, METHOD, NMTRNN )
         !$OMP SIMD ALIGNED(NUO,NUN,DELTO,DELTN,MUO,MUN:64)
         DO I = 1,M
            NUO(1,I) = NUN(1,I)
            NUO(2,I) = NUN(2,I)
            DELTO(1,I) = DELTN(1,I)
            DELTO(2,I) = DELTN(2,I)
            MUO(I) = MUN(I)
         END DO
         !$OMP SIMD ALIGNED(CO,CN:64)
         DO I = 1, M*(M+1)/2
            CO(I) = CN(I)
         END DO
         LGPMXO = LGPMXN
      ELSE
         !$OMP SIMD ALIGNED(CO,CN:64)
         DO I = 1, M*(M+1)/2
            CN(I) = CO(I)
         END DO
         LGPMXN = LGPMXO
      END IF
      END
!*
!*
!*
      SUBROUTINE TRANSF( M, C, MU, DELTA, NU, LGPOST, METHOD, NUMTRN )
                use mod_kinds, only : i4,dp
#if 0
*
*     Computes split transformation parameters DELTAS and NU.
*
*     Input:
*      M - Integer number of variables.
*      C - Real lower triangular standardizing transfomration matrix 
*               stored by rows as a long vector of length M*(M+1)/2.
*      MU - Real M-vector of modes.
*      LGPOST - name of externally declared real function to compute
*               log of posterior at M-vector X.
*      METHOD - Integer method type.
*      NUMTRN - Integer used to control types of transformations used. 
*
*     Output: 
*      DELTA - Real 2xM array of split transformation scale parameters.
*      NU - Integer 2xM array of split transformation types.
*
#endif
      EXTERNAL LGPOST
      INTEGER(kind=i4) M, NT, NU(2,*), METHOD, NUMTRN
      REAL(kind=dp) C(*), MU(*), DELTA(2,*), LGPOST
      INTEGER(kind=i4) I, K, N, NUNRML, NL, NLT, NULIM
      PARAMETER ( NUNRML = 30, NL = 20, NLT = 2*NL, NULIM = 10 )
      REAL(kind=dp) FUN,FUNE,FUNM,FUNP,FUNMX, ALPHA, TCON, ONE, LJ
      REAL(kind=dp) DELT, DELTM, DELTP, X(NL), Z(NL), DLT(2,NL)
      PARAMETER ( ONE = 1, ALPHA = 1.5811388D0 )
      INTEGER(kind=i4) NUT(2,NL)
      DATA NUT, DLT / NLT*0, NLT*1D0 /
!*
!*     Set up default DELTA and NU for unsplit transformations
!*
      IF( NUMTRN .GT. 0 ) TCON = SQRT( DBLE( NUMTRN + M )/NUMTRN )
      DO I = 1, M
         IF ( METHOD .LT. 20 ) THEN
            IF ( NUMTRN .LE. 0 ) THEN
               NT = 0
            ELSE IF ( NUMTRN .LT. 10 ) THEN
               NT = NUMTRN + I - 1
               DO K = I*(I-1)/2 + 1, I*(I+1)/2
                  C(K) = TCON*C(K)
               END DO
            ELSE
               NT = NUNRML
            END IF
         ELSE IF ( METHOD .LT. 30 ) THEN
            NT = 0
         ELSE
            NT = -NUNRML
         END IF
         DO K = 1, 2
               NU(K,I) = NT
            DELTA(K,I) = 1
         END DO
      END DO
      IF ( NUMTRN .EQ. 20 ) THEN
!*
!*     Determine split transformation DELTA and NU.
!*
         DO I = 1, M
            Z(I) = 0
         END DO
         TCON = -2*LOG( 1 + ALPHA**2/3 )
         FUNMX = LGPOST(MU)
         DO I = 1, M
            DO K = -1, 1, 2
!*      
!*     Compute approximate DELT using bisection 
!*     
               DELTM = 0
               FUNM = -TCON
               DELT = 1
!*
!*     First find interval ( DELTM, DELT ) with sign change
!*
 10            Z(I) = K*ALPHA*DELT
               CALL COVTRN( M, C, MU, 0, DLT, NUT, LJ, Z, X )
               FUN = LGPOST(X) - FUNMX - TCON
               IF ( FUN .GT. 0 ) THEN
                  FUNM = FUN
                  DELTM = DELT
                  DELT = 2*DELT
                  GO TO 10
               END IF
!*     
!*     Refine interval with modified secant method
!*     
 20            DELTP = DELT - FUN*( DELT - DELTM )/( FUN - FUNM )
               IF ( ABS( DELT - DELTP ) .GT. DELTP/1000 ) THEN
                  Z(I) = K*ALPHA*DELTP
                  CALL COVTRN( M, C, MU, 0, DLT, NUT, LJ, Z, X )
                  FUNP = LGPOST(X) - FUNMX - TCON
                  IF ( FUNP*FUN .LT. 0 ) THEN
                     DELTM = DELT
                     FUNM = FUN
                  ELSE
                     FUNM = FUNM*FUN/( FUN + FUNP )
                  END IF
                  DELT = DELTP
                  FUN = FUNP
                  GO TO 20
               END IF
               DELT = DELTP
               IF ( METHOD .LT. 20 ) THEN
!*     
!*     Given DELT, compute approximate NU. 
!*     
                  Z(I) =   K*DELT
                  CALL COVTRN( M, C, MU, 0, DLT, NUT, LJ, Z, X )
                  FUN = LGPOST(X) - FUNMX
                  Z(I) = 2*K*DELT
                  CALL COVTRN( M, C, MU, 0, DLT, NUT, LJ, Z, X )
                  FUNP = LGPOST(X) - FUNMX
                  NT = 1
                  FUNM = MAX( ABS( 2*LOG( 1 +   ONE )/2 + FUN  ),           &  
                             ABS( 2*LOG( 1 + 4*ONE )/2 + FUNP ) )
                  DO N = 2, NULIM
                     FUNE = MAX(ABS( (N+1)*LOG( 1 + ONE/N )/2 + FUN  ),     &
                               ABS( (N+1)*LOG( 1+4*ONE/N )/2 + FUNP ) )
                     IF ( FUNE .LT. FUNM ) THEN
                        FUNM = FUNE
                        NT = N
                     END IF
                  END DO
                  IF ( NT .GE. NULIM ) NT =  NUNRML
                  NU( ( K + 3 )/2, I ) = NT
               END IF
               DELTA( ( K + 3 )/2, I ) = DELT
            END DO
            Z(I) = 0
         END DO
      END IF
      END
!*
!*
!*
      SUBROUTINE COVTRN( M, C, MU, NUMTRN, DELTA, NU, LGJACB, Z, X )
               use mod_kinds, only : i4,dp
               use omp_lib
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: COVTRN
           !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: COVTRN
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: COVTRN
#endif          
!*
!*     Computes transformed X vector from input vector Z, standardizing 
!*      transformation matrix C, MU, and split transformation parameters
!*      NU and DELTA. Also computes log of Jacobian for transformation.
!*
      INTEGER(kind=i4) M, NU(2, *), I, J, IJ, JJ, NUMTRN
      REAL(kind=dp) C(*), MU(*), DELTA(2, *), X(*), Z(*)
      REAL(kind=dp) TRAN, LGJACB, DY, Y, UP, UJ
      LGJACB = 0
      !$OMP SIMD ALIGNED(X,MU:64)
      DO I = 1,M
         X(I) = MU(I)
      END DO
      JJ = 0 
      UP = 1
      DO J = 1, M
         JJ = JJ + J
         UJ = TRAN( Z(J), NU(1,J), DELTA(1,J), DY )
         LGJACB = LGJACB + LOG( UP*DY*C(JJ) ) 
         Y = UJ*UP 
!*
!*     Bjoern: Shouldn't there be a method < 30 in the if below?
!*     catch it in the R code for now
!*
         IF ( 0 .LT. NUMTRN .AND. NUMTRN .LT. 10 )                    &
             UP = UP*SQRT( 1 + ( UJ**2 - 1 )/( NUMTRN + J ) )
         IJ = JJ
         !$OMP SIMD ALIGNED(X,C:64)
         DO I = J,M
            X(I) = X(I) + C(IJ)*Y
            IJ = IJ + I
         END DO
      END DO
      END
!*
!*
!*
      REAL(kind=dp) FUNCTION TRAN( Z, NU, DELTA, DT )
         use mod_kinds, only : i4,dp
              
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: TRAN
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: TRAN
#endif          
!*
!*     Transforms Z using selected split inverse distribution; 
!*      also computes Jacobean factor DT
!*
      INTEGER(kind=i4) NU(2), NT, NUNRML, IS
      REAL(kind=dp) Z, T, DT, DELTA(2), DLT
      REAL(kind=dp) PHINV, STDINV, STDJAC, RP
      PARAMETER ( NUNRML = 30, RP = 2.506628274631001_dp)
      IF ( NU(1) .GT. 0 .AND. NU(2) .GT. 0 ) THEN
         IF ( Z .LT. 5D-1 ) THEN 
            IS = 1
         ELSE
            IS = 2
         ENDIF
      ELSE
         IF ( Z .LT. 0 ) THEN 
            IS = 1
         ELSE
            IS = 2
         ENDIF
      END IF
      NT = NU(IS)
      DLT = DELTA(IS)
      IF ( NT .EQ. 0 ) THEN
!*
!*       No Transformation
!*
          T = Z
         DT = 1
      ELSE IF ( 0 .LT. NT .AND. NT .LT. NUNRML ) THEN
!*
!*       Student t Distribution
!*
          T = STDINV( NT, Z )
         DT = STDJAC( NT, T )
      ELSE
!*
!*       Normal Distribution
!*
         IF ( NT .GT. 0 ) THEN
            T = PHINV( Z )
         ELSE
            T = Z
         END IF
         DT = RP*EXP(T*T/2)
      END IF
      IF ( DELTA(IS) .NE. 1 ) THEN
         TRAN = T*( 1 + DLT*ABS(T) )/( 1 + ABS(T) )
         DT =  DT*( 1 + DLT*ABS(T)*( 2 + ABS(T) ) )/( 1 + ABS(T) )**2 
c         TRAN = T*DLT
c          DT = DT*DLT
      ELSE
         TRAN = T
      END IF
      END
      
      REAL(kind=dp) FUNCTION STUDNT( NU, T )
         use mod_kinds, only : i4,dp
            
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: STUDNT
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: STUDNT
#endif          
!*
!*     Student t Distribution Function
!*
!*                       T
!*         STUDNT = C   I  ( 1 + y*y/NU )**( -(NU+1)/2 ) dy
!*                   NU -INF
!*
      INTEGER(kind=i4) NU, J
      REAL(kind=dp) T, CSSTHE, SNTHE, POLYN, TT, TS, RN, PI
      PARAMETER ( PI = 3.141592653589793_dp )
      IF ( NU .EQ. 1 ) THEN
         STUDNT = ( 1 + 2*ATAN(T)/PI )/2
      ELSE IF ( NU .EQ. 2) THEN
         STUDNT = ( 1 + T/SQRT( 2 + T*T ))/2
      ELSE 
         TT = T*T
         CSSTHE = 1/( 1 + TT/NU )
         POLYN = 1
         DO J = NU-2, 2, -2
            POLYN = 1 + ( J - 1 )*CSSTHE*POLYN/J
         END DO
         IF ( MOD( NU, 2 ) .EQ. 1 ) THEN
            RN = NU
            TS = T/SQRT(RN)
            STUDNT = ( 1 + 2*( ATAN(TS) + TS*CSSTHE*POLYN )/PI )/2
         ELSE
            SNTHE = T/SQRT( NU + TT )
            STUDNT = ( 1 + SNTHE*POLYN )/2
         END IF
      ENDIF
      END
      
      REAL(kind=dp) FUNCTION STDINV( N, Z )
         use mod_kinds, only : i4,dp
              
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: STDINV
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: STDINV
#endif          
#if 0      
*
*     Inverse Student t Distribution Function
*
*                     STDINV
*           Z = C    I      (1 + y*y/N)**(-(N+1)/2) dy
*                N  - INF
*
*      Reference: G.W. Hill, Comm. ACM Algorithm 395
*                 Comm. ACM 13 (1970), pp. 619-620.
*
*      Conversions to double precision and other modifications by
*                 Alan Genz, 1993-4.
*
#endif
      INTEGER(kind=i4) N
      REAL(kind=dp) Z, P, PHINV, A, B, C, D, X, Y, PI, TWO
      REAL(kind=dp) STUDNT, STDJAC
      PARAMETER ( PI = 3.141592653589793_dp, TWO = 2  )
      IF ( 0 .LT. Z .AND. Z .LT. 1 ) THEN
         IF ( N .EQ. 1 ) THEN
            STDINV = TAN( PI*( 2*Z - 1 )/2 )
         ELSE IF ( N .EQ. 2) THEN
            STDINV = ( 2*Z - 1 )/SQRT( 2*Z*( 1 - Z ) )
         ELSE 
            IF ( 2*Z .GE. 1 ) THEN 
               P = 2*( 1 - Z )
            ELSE
               P = 2*Z
            END IF
            A = 1/( N - 0.5 )
            B = 48/( A*A )
            C = ( ( 20700*A/B - 98 )*A - 16 )*A + 96.36
            D = ( ( 94.5/( B + C ) - 3 )/B + 1 )*SQRT( A*PI/2 )*N
            X = D*P
            Y = X**( TWO/N )
            IF ( Y .GT. A + 0.05 ) THEN
               X = PHINV( P/2 )
               Y = X*X
               IF ( N .LT. 5 ) C = C + 3*( N - 4.5 )*( 10*X + 6 )/100
               C = ( ( (D*X - 100)*X/20 - 7 )*X - 2 )*X + B + C
               Y = ( ( ( ( (4*Y+63)*Y/10+36 )*Y+94.5 )/C-Y-3 )/B + 1 )*X
               Y = A*Y*Y
               IF ( Y .GT. 0.002 ) THEN
                  Y = EXP(Y) - 1
               ELSE
                  Y = Y*( 1 + Y/2 )
               ENDIF
            ELSE
               Y = ( ( 1/( ( (N+6)/(N*Y) - 0.089*D - 0.822 )*(3*N+6) )     &
                   + 0.5/(N+4) )*Y - 1 )*(N+1)/(N+2) + 1/Y
            END IF
            STDINV = SQRT(N*Y)
            IF ( 2*Z .LT. 1 ) STDINV = -STDINV
!*
!*     Use two Newton corrections to the single precision result
!*
            STDINV = STDINV + STDJAC(N,STDINV)*( Z - STUDNT(N,STDINV) )
            STDINV = STDINV + STDJAC(N,STDINV)*( Z - STUDNT(N,STDINV) )
         END IF
      ELSE
!*
!*     Use cutoff values for Z near 0 or 1.
!*
         STDINV = SQRT( N/( 2D-16*SQRT( 2*PI*N ) )**( TWO/N ) )
         IF ( 2*Z .LT. 1 ) STDINV = -STDINV
      END IF
      END
      
      REAL(kind=dp) FUNCTION STDJAC( NU, T )
         use mod_kinds, only : i4,dp
             
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: STDJAC
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: STDJAC
#endif          
!*
!*     Student t Distribution Transformation Jacobean
!*
!*          T            STDINV(NU,T)
!*         I  f(y) dy = I   f(STDINV(NU,Z) STDJAC(NU,STDINV(NU,Z)) dZ
!*         -INF          0
!*
      INTEGER(kind=i4) NU, NUOLD, J
      REAL(kind=dp) CONST, PI, T
      PARAMETER ( PI = 3.141592653589793_dp )
      SAVE NUOLD, CONST
      DATA NUOLD/ 0 /
      IF ( NU .EQ. 1 ) THEN
         STDJAC = PI*( 1 + T*T )
      ELSE IF ( NU .EQ. 2 ) THEN 
         STDJAC = SQRT( 2 + T*T )**3
      ELSE 
         IF ( NU .NE. NUOLD ) THEN
            NUOLD = NU
            CONST = 2
            IF ( MOD( NU, 2 ) .EQ. 1 ) CONST = PI
            DO J = NU-2, 1, -2
               CONST = J*CONST/(J+1)
            END DO
         END IF
         STDJAC = CONST*SQRT( NU*( 1 + T*T/NU )**(NU+1) )
      END IF
      END
      
      REAL(kind=dp) FUNCTION PHI(Z)
         use mod_kinds, only : i4,dp
              
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: PHI
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: PHI
#endif          

#if 0      
*
*	Normal distribution probabilities accurate to 1.e-15.
*	Z = no. of standard deviations from the mean.
*
*       Based upon algorithm 5666 for the error function, from:
*       Hart, J.F. et al, 'Computer Approximations', Wiley 1968
*
*       Programmer: Alan Miller
*
*	Latest revision - 30 March 1986
*
#endif
      REAL(kind=dp) P0, P1, P2, P3, P4, P5, P6,                 &
          Q0, Q1, Q2, Q3, Q4, Q5, Q6, Q7,                      &
          Z, P, EXPNTL, CUTOFF, ROOTPI, ZABS
      PARAMETER(  P0 = 220.2068679123761D0,                  &
     	          P1 = 221.2135961699311D0,                  &
                  P2 = 112.0792914978709D0,                  &
     	          P3 = 33.91286607838300D0,                  &
                  P4 = 6.373962203531650D0,                  &
     	          P5 = .7003830644436881D0,                  &
                  P6 = .03526249659989109D0 )
      PARAMETER(  Q0 = 440.4137358247522D0,                  &
     	          Q1 = 793.8265125199484D0,                  &
                  Q2 = 637.3336333788311D0,                  &
     	          Q3 = 296.5642487796737D0,                  &
                  Q4 = 86.78073220294608D0,                  &
     	          Q5 = 16.06417757920695D0,                  &
                  Q6 = 1.755667163182642D0,                  &
                  Q7 = .08838834764831844D0 )
      PARAMETER(  ROOTPI = 2.506628274631001D0 )
      PARAMETER(  CUTOFF = 7.071067811865475D0 )
     
      ZABS = ABS(Z)
!*     
!*     |Z| > 37
!*     
      IF ( ZABS .GT. 37 ) THEN
         P = 0
      ELSE
!*     
!*     |Z| <= 37
!*     
         EXPNTL = EXP(-ZABS**2/2)
!*     
!*     |Z| < CUTOFF = 10/SQRT(2)
!*     
         IF ( ZABS .LT. CUTOFF ) THEN
            P = EXPNTL*((((((P6*ZABS + P5)*ZABS + P4)*ZABS + P3)*ZABS   &
               + P2)*ZABS + P1)*ZABS + P0)/(((((((Q7*ZABS + Q6)*ZABS   &
               + Q5)*ZABS + Q4)*ZABS + Q3)*ZABS + Q2)*ZABS + Q1)*ZABS  &
               + Q0)
!*
!*     |Z| >= CUTOFF.
!*     
         ELSE
            P = EXPNTL/(ZABS + 1/(ZABS + 2/(ZABS + 3/(ZABS + 4/  &
                (ZABS + 0.650000000000000_dp)))))/ROOTPI
         END IF
      END IF
      IF (Z .GT. 0) P = 1 - P
      PHI = P
      END
      
      REAL(kind=dp) FUNCTION PHINV(P)
       use mod_kinds, only : i4,dp
              
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: PHINV
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: PHINV
#endif         
#if 0
*
*     ALGORITHM AS241  APPL. STATIST. (1988) VOL. 37, NO. 3
*     
*     Produces the normal deviate Z corresponding to a given lower
*     tail area of P.
*     
*     The hash sums below are the sums of the mantissas of the
*     coefficients.   They are included for use in checking
*     transcription.
*  
#endif   
      REAL(kind=dp) SPLIT1, SPLIT2, CONST1, CONST2,                        &
          A0, A1, A2, A3, A4, A5, A6, A7, B1, B2, B3, B4, B5, B6, B7,      &
          C0, C1, C2, C3, C4, C5, C6, C7, D1, D2, D3, D4, D5, D6, D7,      &
          E0, E1, E2, E3, E4, E5, E6, E7, F1, F2, F3, F4, F5, F6, F7,      &
          P, Q, R
      PARAMETER (SPLIT1 = 0.425D0, SPLIT2 = 5,                             &
          CONST1 = 0.180625D0, CONST2 = 1.6D0)
!*     
!*     Coefficients for P close to 0.5
!*     
      PARAMETER (  A0 = 3.3871328727963666080D0,                        &
     		   A1 = 1.3314166789178437745D+2,                       &
     		   A2 = 1.9715909503065514427D+3,                       &
     		   A3 = 1.3731693765509461125D+4,                       &
     		   A4 = 4.5921953931549871457D+4,                       &
     		   A5 = 6.7265770927008700853D+4,                       &
     		   A6 = 3.3430575583588128105D+4,                       &
     		   A7 = 2.5090809287301226727D+3,                       &
     		   B1 = 4.2313330701600911252D+1,                       &
     		   B2 = 6.8718700749205790830D+2,                       &
     		   B3 = 5.3941960214247511077D+3,                       &
     		   B4 = 2.1213794301586595867D+4,                       &
     		   B5 = 3.9307895800092710610D+4,                       &
     		   B6 = 2.8729085735721942674D+4,                       &
     		   B7 = 5.2264952788528545610D+3)
!*     HASH SUM AB      55.88319 28806 14901 4439
!*     
!*     Coefficients for P not close to 0, 0.5 or 1.
!*     
      PARAMETER (  C0 = 1.42343711074968357734D0,                       &
                  C1 = 4.63033784615654529590D0,                       &
     		   C2 = 5.76949722146069140550D0,                       &
     		   C3 = 3.64784832476320460504D0,                       &
     		   C4 = 1.27045825245236838258D0,                       &
     		   C5 = 2.41780725177450611770D-1,                      &
     		   C6 = 2.27238449892691845833D-2,                      &
     		   C7 = 7.74545014278341407640D-4,                      &
     		   D1 = 2.05319162663775882187D0,                       &
     		   D2 = 1.67638483018380384940D0,                       &
     		   D3 = 6.89767334985100004550D-1,                      &
     		   D4 = 1.48103976427480074590D-1,                      &
     		   D5 = 1.51986665636164571966D-2,                      &
     		   D6 = 5.47593808499534494600D-4,                      &
     		   D7 = 1.05075007164441684324D-9)
!*	HASH SUM CD    49.33206 50330 16102 89036
!*
!*	Coefficients for P near 0 or 1.
!*
      PARAMETER (  E0 = 6.65790464350110377720D0,                       &
     		   E1 = 5.46378491116411436990D0,                       &
     		   E2 = 1.78482653991729133580D0,                       &
     		   E3 = 2.96560571828504891230D-1,                      &
     		   E4 = 2.65321895265761230930D-2,                      &
     		   E5 = 1.24266094738807843860D-3,                      &
     		   E6 = 2.71155556874348757815D-5,                      &
     		   E7 = 2.01033439929228813265D-7,                      &
     		   F1 = 5.99832206555887937690D-1,                      &
     		   F2 = 1.36929880922735805310D-1,                      &
     		   F3 = 1.48753612908506148525D-2,                      &
     		   F4 = 7.86869131145613259100D-4,                      &
     		   F5 = 1.84631831751005468180D-5,                      &
     		   F6 = 1.42151175831644588870D-7,                      &
     		   F7 = 2.04426310338993978564D-15)
!*     HASH SUM EF      47.52583 31754 92896 71629
!*     
      Q = ( 2*P - 1 )/2
      IF ( ABS(Q) .LE. SPLIT1 ) THEN
         R = CONST1 - Q*Q
         PHINV = Q*(((((((A7*R + A6)*R + A5)*R + A4)*R + A3)           &
             *R + A2)*R + A1)*R + A0) /                                &
             (((((((B7*R + B6)*R + B5)*R + B4)*R + B3)                 &
             *R + B2)*R + B1)*R + 1)
      ELSE
         R = MIN( P, 1 - P )
         IF ( R .GT. 0 ) THEN
            R = SQRT(-LOG(R))
            IF ( R .LE. SPLIT2 ) THEN
               R = R - CONST2
               PHINV = (((((((C7*R + C6)*R + C5)*R + C4)*R + C3)      &
                   *R + C2)*R + C1)*R + C0) /                         &
                   (((((((D7*R + D6)*R + D5)*R + D4)*R + D3)          &
                   *R + D2)*R + D1)*R + 1)
            ELSE
               R = R - SPLIT2
               PHINV = (((((((E7*R + E6)*R + E5)*R + E4)*R + E3)      &
                   *R + E2)*R + E1)*R + E0) /                        &
                   (((((((F7*R + F6)*R + F5)*R + F4)*R + F3)         &
                   *R + F2)*R + F1)*R + 1)
            END IF
         ELSE
            PHINV = 8.5
         END IF
         IF (Q .LT. 0) PHINV = -PHINV
      END IF
      END
!*
!*
!*
      SUBROUTINE MODCOV( M, COVAR, X, LGPTMX, LGPOST, INFORM ) 
                 use mod_kinds, only : i4,dp
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: MODCOV
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: MODCOV
#endif                      
!*
!*     To determine modal covariance matrix for LGPOST
!*
      EXTERNAL LGPOST
      INTEGER(kind=i4) I, J, M, INFORM
      REAL(kind=dp) X(M), COVAR(M*(M+1)/2), LGPOST, LGPTMX
      LGPTMX = -LGPOST(X)
      INFORM = 0
      IF ( INFORM .EQ. 0 ) THEN
         CALL COVCLC( M, X, LGPOST, COVAR ) 
      ELSE
         DO I = 1,M
            DO J = I*(I-1)/2+1, I*(I+1)/2-1
               COVAR(J) = 0
            END DO
            COVAR(I*(I+1)/2) = 1
         END DO
      END IF
      END

      SUBROUTINE COVCLC(N, X, F, CV )
       use mod_kinds, only : i4,dp
       use omp_lib
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: COVCLC
           !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: COVCLC
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: COVCLC
#endif                
!*
!*     To compute modal covariance matrix for log posterior at mode X.
!*
      EXTERNAL F
      INTEGER(kind=i4) I, J, IJ, N
      REAL(kind=dp) X(*), CV(*), F, H, HI, HJ
      PARAMETER ( H = 1D-4 )
      REAL(kind=dp) FUN, XI, XJ, FPP, FMP, FIP, FMM, FPM, FIM
      FUN = F(X)
      IJ = 0
      DO I = 1,N
         XI = X(I)
         HI = MAX( ABS(XI)*H, H )
         !$OMP SIMD ALIGNED(X,F,CV:64)
         DO J = 1,I-1
            IJ = IJ + 1
            XJ = X(J)
            HJ = MAX( ABS(XJ)*H, H )
            X(I) = XI - HI
            X(J) = XJ - HJ
            FMM = F(X)
            X(J) = XJ + HJ
            FMP = F(X)
            X(I) = XI + HI
            FPP = F(X)
            X(J) = XJ - HJ
            FPM = F(X)
            CV(IJ) = -( FPP-FMP-FPM+FMM )/( 4*HI*HJ )
            X(I) = XI - HI/2
            X(J) = XJ - HJ/2
            FMM = F(X)
            X(J) = XJ + HJ/2
            FMP = F(X)
            X(I) = XI + HI/2
            FPP = F(X)
            X(J) = XJ - HJ/2
            FPM = F(X)
            CV(IJ) = ( -4*( FPP-FMP-FPM+FMM )/( HI*HJ ) - CV(IJ) )/3
            X(J) = XJ
         END DO
         IJ = IJ + 1
         X(I) = XI - HI
         FIM = F(X)
         X(I) = XI + HI
         FIP = F(X)
         CV(IJ) = -( FIP + FIM - 2*FUN )/HI**2
         X(I) = XI - HI/2
         FIM = F(X)
         X(I) = XI + HI/2
         FIP = F(X)
         CV(IJ) = ( -16*( FIP + FIM - 2*FUN )/HI**2 - CV(IJ) )/3
         X(I) = XI
      END DO
      CALL SYMINV( N, CV )
      END

      SUBROUTINE SYMINV( N, LOWINV )
       use mod_kinds, only : i4,dp
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: SYMINV
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: SYMINV
#endif                
!*
!*     Computes lower symmetric inverse and determinant in situ
!*
      INTEGER(kind=i4) N
      REAL(kind=dp) LOWINV(*)
      CALL CHOLSK(N, LOWINV)
      CALL CHOLNV(N, LOWINV)
      CALL CHOLPI(N, LOWINV)
      END
      
      SUBROUTINE CHOLSK( N, CHOFAC )
       use mod_kinds, only : i4,dp
       use omp_lib
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: CHOLSK
           !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: CHOLSK
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: CHOLSK
#endif                
!*
!*     Computes Choleski factor in situ
!*
      INTEGER(kind=i4) I, II, J, JJ, K, N
      REAL(kind=dp) CHOFAC(*), T, S, ZERO
      PARAMETER ( ZERO = 0 )
      JJ = 0
      DO J = 1,N
         II = JJ
         DO I = J,N
            S = CHOFAC(II+J)
            !$OMP SIMD REDUCTION(-:S) ALIGNED(CHOFAC:64)
            DO K = 1,J-1
               S = S - CHOFAC(II+K)*CHOFAC(JJ+K)
            END DO
            IF ( I .EQ. J ) THEN
               T = SQRT( MAX( S, ZERO ) )
               CHOFAC(II+J) = T
            ELSE IF ( T .GT. 0 ) THEN
               CHOFAC(II+J) = S/T
            ELSE
               CHOFAC(II+J) = 0
            ENDIF
            II = II + I
         END DO
         JJ = JJ + J
      END DO
      END
      SUBROUTINE CHOLNV(N, CHOINV)
       use mod_kinds, only : i4,dp
       use omp_lib
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: CHOLNV
           !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: CHOLNV
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: CHOLNV
#endif        
!*
!*     Inverts a lower triangular matrix in situ
!*
      INTEGER(kind=i4) I, II, J, JJ, K, KK, N
      REAL(kind=dp) CHOINV(*), T
      REAL(kind=dp) S
      II = 0
      DO I = 1,N
         T = 1/CHOINV(II+I)
         JJ = 0
         DO J = 1,I-1
            S = 0
            JJ = JJ + J
            KK = JJ
            !$OMP SIMD REDUCTION(+:S) ALIGNED(CHOINV:64)
            DO K = J,I-1
               S = S + CHOINV(II+K)*CHOINV(KK)
               KK = KK + K
            END DO
            CHOINV(II+J) = -S*T
         END DO
         II = II + I
         CHOINV(II) = T
      END DO
      END
      SUBROUTINE CHOLPD(N, CHOPRD)
          use mod_kinds, only : i4,dp
          use omp_lib
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: CHOLPD
           !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: CHOLPD
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: CHOLPD
#endif  
!*
!*     Multiplies Choleski factors in situ
!*
      INTEGER(kind=i4) I, II, J, K, KK, N, NN
      REAL(kind=dp) CHOPRD(*)
      REAL(kind=dp) S
      NN = (N*(N+1))/2
      KK = NN
      DO K = N,1,-1
         KK = KK - K
         II = NN
         DO I = N,K,-1
            II = II - I
            S = 0
            !$OMP SIMD REDUCTION(+:S) ALIGNED(CHOPRD:64)
            DO J = 1,K
               S = S + CHOPRD(II+J)*CHOPRD(KK+J)
            END DO
            CHOPRD(II+K) = S
         END DO
      END DO
      END
      
      SUBROUTINE CHOLPI(N, CHOPDI)
          use mod_kinds, only : i4,dp
          use omp_lib
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: CHOLPI
           !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: CHOLPI
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: CHOLPI
#endif  
!*
!*     Multiplies Choleski inverse factors in situ
!*
      INTEGER(kind=i4) I, II, J, JJ, K, KK, N
      REAL(kind=dp) CHOPDI(*)
      REAL(kind=dp) S
      II = 0
      DO I = 1,N
         DO J = 1,I
            S = 0
            JJ = II + I
            KK = II + J
            !$OMP SIMD REDUCTION(+:S) ALIGNED(CHOPDI:64)
            DO K = I,N
               S = S + CHOPDI(KK)*CHOPDI(JJ)
               JJ = JJ + K
               KK = KK + K
            END DO
            CHOPDI(II+J) = S
         END DO
         II = II + I
      END DO
      END
!*
!*
!*
      REAL(kind=dp) FUNCTION ADONE(A, B, F, TOL, IPTS)
          use mod_kinds, only : i4,dp
      
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: ADONE
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ADONE
#endif  
!*
!*     One Dimensional Adaptive Integration Routine
!*
      EXTERNAL F
      REAL(kind=dp) A, B, F, TOL
      INTEGER(kind=i4) NL, I, IM, IP, IPTS
      PARAMETER ( NL = 500 )
      REAL(kind=dp) WI(NL), EI(NL), AI(NL), FI(NL), FIN, ERR, KRONRD
      IP = 1
      WI(1) = (B-A)/2
      AI(1) = A
      IM = 1
      IPTS = 0
 10   FI(IP) = KRONRD( AI(IP), WI(IP), F, EI(IP) )
      FIN = 0
      ERR = 0
      IPTS = IPTS + 15
      DO I = 1, IM
         IF ( EI(I) .GT. EI(IP) ) IP = I
         FIN = FIN + FI(I)
         ERR = ERR + EI(I)
      END DO
      IF ( ERR .GT. TOL*ABS(FIN) .AND. IM .LT. NL ) THEN
         IM = IM + 1
         AI(IM) = AI(IP) + WI(IP)
         WI(IM) = WI(IP)/2
         WI(IP) = WI(IM)
         FI(IM) = KRONRD( AI(IM), WI(IM), F, EI(IM) )
         IPTS = IPTS + 15
         GO TO 10
      ENDIF
      ADONE = FIN
      END
!*
!*
      REAL(kind=dp) FUNCTION KRONRD(A, HFLGTH, F, ABSERR)
          use mod_kinds, only : i4,dp
   
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: KRONRD
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: KRONRD
#endif  
!*
!*     Kronrod Rule
!*
      EXTERNAL F
      REAL(kind=dp) A, ABSERR, F, FUNSUM, HFLGTH, RESLTG, RESLTK
      INTEGER(kind=i4) J, N
      PARAMETER (N = 7)

      REAL(kind=dp) WG(0:(N+1)/2), WGK(0:N), XGK(0:N) 
      SAVE WG, WGK, XGK
#if defined(_OPENMP)
!$OMP THREADPRIVATE(WG,WGK,XGK)
#endif
#if 0
*
*           THE ABSCISSAE AND WEIGHTS ARE GIVEN FOR THE INTERVAL (-1,1)
*           BECAUSE OF SYMMETRY ONLY THE POSITIVE ABSCISSE AND THEIR 
*           CORRESPONDING WEIGHTS ARE GIVEN.
*
*           XGK    - ABSCISSAE OF THE 15-POINT KRONROD RULE 
*                    XGK(2), XGK(4), ...  ABSCISSAE OF THE 7-POINT
*                    GAUSS RULE
*                    XGK(1), XGK(3), ...  ABSCISSAE WHICH ARE OPTIMALLY
*                    ADDED TO THE 7-POINT GAUSS RULE
*
*           WGK    - WEIGHTS OF THE 15-POINT KRONROD RULE
*
*           WG     - WEIGHTS OF THE 7-POINT GAUSS RULE
*
#endif
      DATA WG(0) /0.417959183673469387755102040816327D0/
      DATA WG(1) /0.129484966168869693270611432679082D0/
      DATA WG(2) /0.279705391489276667901467771423780D0/
      DATA WG(3) /0.381830050505118944950369775488975D0/

      DATA XGK(0) /0.000000000000000000000000000000000D0/
      DATA XGK(1) /0.991455371120812639206854697526329D0/
      DATA XGK(2) /0.949107912342758524526189684047851D0/
      DATA XGK(3) /0.864864423359769072789712788640926D0/
      DATA XGK(4) /0.741531185599394439863864773280788D0/
      DATA XGK(5) /0.586087235467691130294144838258730D0/
      DATA XGK(6) /0.405845151377397166906606412076961D0/
      DATA XGK(7) /0.207784955007898467600689403773245D0/

      DATA WGK(0) /0.209482141084727828012999174891714D0/
      DATA WGK(1) /0.022935322010529224963732008058970D0/
      DATA WGK(2) /0.063092092629978553290700663189204D0/
      DATA WGK(3) /0.104790010322250183839876322541518D0/
      DATA WGK(4) /0.140653259715525918745189590510238D0/
      DATA WGK(5) /0.169004726639267902826583426598550D0/
      DATA WGK(6) /0.190350578064785409913256402421014D0/
      DATA WGK(7) /0.204432940075298892414161999234649D0/
#if 0
*
*
*           LIST OF MAJOR VARIABLES
*           -----------------------
*
*           A        - LEFT ENDPOINT OF THE INTERVAL
*           HFLGTH   - HALF-LENGTH OF THE INTERVAL
*           RESLTG   - RESULT OF THE 7-POINT GAUSS FORMULA
*           RESLTK   - RESULT OF THE 15-POINT KRONROD FORMULA
*
*
*
*           COMPUTE THE 15-POINT KRONROD APPROXIMATION TO
*           THE INTEGRAL, AND ESTIMATE THE ABSOLUTE ERROR.
*
#endif
      RESLTG = F(A+HFLGTH)
      RESLTK = RESLTG*WGK(0)
      RESLTG = RESLTG*WG(0)
      DO J = 1,N
         FUNSUM = F(A+HFLGTH*(1+XGK(J))) + F(A+HFLGTH*(1-XGK(J)))
         RESLTK = RESLTK + WGK(J)*FUNSUM
         IF( MOD(J,2) .EQ. 0 ) RESLTG = RESLTG + WG(J/2)*FUNSUM
      END DO
      KRONRD = RESLTK*HFLGTH
      ABSERR = ABS((RESLTK-RESLTG)*HFLGTH)
      END
!*
!*
!*
      SUBROUTINE VCRUDE( M, NF, A, B, MAXPTS, FUNCTN, ABSREQ, RELREQ,      &
                       IR, FINEST, ESTERR, NCLS, INFORM, WORK )
                   use mod_kinds, only : i4,dp
   
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: VCRUDE
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: VCRUDE
#endif                         
#if 0
*
*    Crude Monte Carlo Integration Subroutine Driver
*
#endif
      EXTERNAL FUNCTN
      INTEGER(kind=i4) M, NF, MAXPTS, IR, NCLS, INFORM, I
      REAL(kind=dp)  A(*), B(*), FINEST(*), ESTERR(*), WORK(*),            &
          ABSREQ, RELREQ, WEIGHT, DIFFER 

      IF ( IR .NE. 0 ) THEN
         DO I = 1, NF
            WORK(I) = FINEST(I)
            WORK(NF+I) = ESTERR(I) 
         END DO
      END IF
      CALL VCRUDR( M, NF, A, B, MAXPTS, FUNCTN, FINEST, ESTERR, NCLS,      &
                  WORK(2*NF+1), WORK(3*NF+1), WORK(4*NF+1) ) 
      INFORM = 0
      DO I = 1, NF
         ESTERR(I) = SQRT( ESTERR(I) )
         IF ( IR .NE. 0 ) THEN
            IF ( WORK(NF+I) .GT. 0 ) THEN 
               WEIGHT = 1/( 1 + ( ESTERR(I)/WORK(NF+I) )**2 )
            ELSE IF ( ESTERR(I) .GT. 0 ) THEN
               WEIGHT = 0
            ELSE
               WEIGHT = 1 
            END IF
            DIFFER = WEIGHT*( FINEST(I) - WORK(I) )
            FINEST(I) = WORK(I) + DIFFER
            ESTERR(I) = SQRT(WEIGHT)*ESTERR(I)
         END IF
         IF ( ESTERR(I) .GT. MAX( ABSREQ, RELREQ*ABS(FINEST(I)) ) )         &
             INFORM = 1
      END DO
      END

      SUBROUTINE VCRUDR( M, NF, A, B, MAXPTS, FUNCTN,                       &
                        FINEST, ESTERR, NCLS, FUNS, FUNTMP, X )
             use mod_kinds, only : i4,dp
             use omp_lib
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: VCRUDR
           !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: VCRUDR
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: VCRUDR
#endif  
!*
!*    Crude Monte Carlo Integration Subroutine 
!*
      EXTERNAL FUNCTN
      INTEGER(kind=i4) M, N, NF, MAXPTS, NCLS, INFORM, I, K
      REAL(kind=dp)     A(*), B(*), FINEST(*), ESTERR(*), FUNTMP(*), FUNS(*), X(*)
      REAL(kind=dp) UNIRAN, FINVAL, VOLUME, FINDIF
      VOLUME = 1
      DO K = 1,M
         VOLUME = VOLUME*( B(K) - A(K) )
      END DO
      DO I = 1,NF
         FINEST(I) = 0
         ESTERR(I) = 0
      END DO
!*
!*     Uses simple antithetic variates
!*
      NCLS = MAXPTS/2
      DO N = 1,NCLS
         !$OMP SIMD ALIGNED(X,A,B,UNIRAN:64)
         DO K = 1,M
            X(K) = A(K) + ( B(K) - A(K) )*UNIRAN()
         END DO
         CALL FUNCTN(M, X, NF, FUNS)
         !$OMP SIMD ALIGNED(X,A,B:64)
         DO K = 1,M
            X(K) = A(K) + B(K) - X(K)
          END DO 
         CALL FUNCTN(M, X, NF, FUNTMP)
         DO I = 1,NF
            FINVAL = VOLUME*( FUNS(I) + FUNTMP(I) )/2
            FINDIF = ( FINVAL - FINEST(I) )/N
            ESTERR(I) = ( N - 2 )*ESTERR(I)/N + FINDIF**2 
            FINEST(I) = FINEST(I) + FINDIF
         END DO
      END DO
      NCLS = 2*NCLS
      END

      SUBROUTINE VKROBV( NDIM, NF, A, B, MINVLS, MAXVLS, FUNSUB,                    &
          ABSEPS, RELEPS, IR, FINEST, ABSERR, INTVLS, INFORM, WORK )
             use mod_kinds, only : i4,dp
            
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: VKROBV
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: VKROBV
#endif  
#if 0
*
*  Automatic Multidimensional Integration Subroutine
*               
*         AUTHOR: Alan Genz
*                 Department of Mathematics
*                 Washington State University
*                 Pulman, WA 99164-3113
*
*         Last Change: 5/15/94
*
*  VKROBV computes an approximation to the integral
*
*    B(1) B(2)  B(NDIM)
*   I    I ... I         F(X) F(X) ... F(X)   dx(NDIM)...dx(2)dx(1)
*    A(1) A(2)  A(NDIM)   1    2        NF
*
*
*  VKROBV uses randomized Korobov rules. The primary references are
*  "Randomization of Number Theoretic Methods for Multiple Integration"
*   R. Cranley and T.N.L. Patterson, SIAM J Numer Anal, 13, pp. 904-14,
*  and 
*   "Optimal Parameters for Multidimensional Integration", 
*    P. Keast, SIAM J Numer Anal, 10, pp. 831-838.
*   
***************  Parameters for KROBOV  *******************************
****** Input parameters
*  NDIM    Number of variables, must exceed 1, but not exceed 40.
*  A       Real array of lower integration limits.
*  B       Real array of upper integration limits.
*  MINVLS  Integer minimum number of function evaluations allowed.
*          MINVLS must not exceed MAXVLS.
*  MAXVLS  Integer maximum number of function evaluations allowed.
*  NF      Integer number of functions to be integrated 
*  FUNSUB  EXTERNALly declared user defined subroutine for integrands.
*          It must have parameters (NDIM,Z,NF,F), where Z and F are real 
*          arrays with respective lengths NDIM and NF.
*  ABSEPS  Required absolute accuracy.
*  RELEPS  Required relative accuracy.
*  IR      Integer restart parameter. If IR < > 0 then the routine
*          assumes a previous call of VKROBV has been made with 
*          the same integrand and continues that calculation.
*  WORK    Real array of working storage, with length at least 2*NF 
****** Output parameters
*  INTVLS  Actual number of function evaluations used by VKROBV.
*  ABSERR  Real array of length NF; 
*            the estimated absolute accuracys for FINEST.
*  FINEST  Real array of length NF; 
*            the estimated values of the integrals.
*  INFORM   IFAIL = 0 for normal exit, when for each I
*                     ABSERR(I) <= MAX(ABSEPS, RELEPS*ABS(FINEST(I)))
*                  and 
*                     INTVLS <= MAXCLS.
*          INFORM = 1 If MAXVLS was too small for KROBOV to obtain the
c                  required accuracy. Then KROBOV returns values for
c                  FINEST with estimated absolute accuracies ABSERR.
************************************************************************
#endif
      EXTERNAL FUNSUB
      REAL(kind=dp) A(*), B(*), ABSERR(*), FINEST(*), WORK(*)
      INTEGER(kind=i4) IR, PLIM, NLIM, NF, NDIM, MINVLS, MAXVLS, NP,    &
          INFORM, NSAMP, I, J, INTVLS, MINSMP, SAMPLS
      REAL(kind=dp) ABSEPS, RELEPS, FINDIF
      PARAMETER ( PLIM = 18, NLIM = 40, MINSMP = 8 )
      INTEGER(kind=i4) C(PLIM,NLIM), P(PLIM), V(NLIM)
      REAL(kind=dp) VK(NLIM), ONE, X(NLIM), ALPHA(NLIM)
      PARAMETER ( ONE = 1 )
      SAVE P, C, NSAMP, NP
      DATA P( 1),(C( 1,I), I = 1,39) /  173,                            &
         73,   34,   57,    9,   12,    2,   16,   30,   30,   42,     &
         70,   86,    2,   53,   53,   30,   30,    5,   42,   42,     &
         70,   42,   53,   42,   42,   53,   42,   53,   53,    2,     &
         86,    2,    2,    2,    2,    2,    2,    2,    2/
      DATA P( 2),(C( 2,I), I = 1,39) /  263,                            &
        111,  106,   51,   36,   48,  110,    2,    2,    2,    2,     &
         70,   70,   48,    2,    2,   70,  124,  124,   70,   48,     &
         48,   48,   48,  108,   65,   48,   48,   70,    2,   20,     &
          2,    2,    2,    2,    2,    2,    2,    2,    2/
      DATA P( 3),(C( 3,I), I = 1,39) /  397,
        163,  168,  164,  133,   23,   64,    2,    2,  106,   80,     &
         80,  126,   16,   16,   16,   16,   16,   16,  107,   80,     &
          2,    2,    2,   32,   32,   32,   31,   64,   31,   31,     &
          4,    4,    4,  126,   16,   16,   16,   16,   16/
      DATA P( 4),(C( 4,I), I = 1,39) /  593,                            &
        229,   40,  268,  240,   31,  119,   71,  296,  130,  199,     &
        149,  149,  149,  149,  149,   31,  130,  149,  149,   79,     &
        119,  119,   31,   82,  130,  122,  122,  122,  122,    2,     &
        130,  130,  130,  130,    2,    2,   82,   82,    2/
      DATA P( 5),(C( 5,I), I = 1,39) /  887,                            &
        192,  424,   55,  221,  179,  242,  242,    2,    2,   11,     &
         11,   11,  394,  394,  439,  394,  394,  394,  394,  439,     &
        394,  394,  394,  101,  378,  394,  394,  394,  394,  394,     &
        202,  279,  394,  279,    2,    2,    2,    2,    2/
      DATA P( 6),(C( 6,I), I = 1,39) / 1327,                            &
       513,  195,  599,  661,  443,  632,  251,  603,  663,    2,     &
        425,  425,  603,  425,  425,  525,  412,  412,  412,  412,     &
        412,   82,   82,   82,  603,  580,  580,  444,   82,   82,     &
        276,  601,  276,  276,  276,  276,  112,  112,  112/
      DATA P( 7),(C( 7,I), I = 1,39) / 1997,                            &
        839,  146,  860,  183,  121,   11,   11,  793,  998,    2,     &
          2,  110,  110,  236,  110,  236,  147,  147,  110,  190,     &
       147,  147,  147,  147,  147,  147,  236,  110,  110,  147,     &
        110,  110,  632,  147,  147,  148,    2,  147,  147/
      DATA P( 8),(C( 8,I), I = 1,39) / 2999,                            &
       1148, 1406, 1192, 1094, 1290,  632,  341,  785,  393, 1499,     &
          2,  798,  808,  798,  918,  393,  924,  924,    2,    2,     &
          2, 1499,    2, 1016,  798,  798,  798,  808,  270, 1344,     &
        798,  798,  798,  798,  798,  798,  798,    2,    2/
      DATA P( 9),(C( 9,I), I = 1,39) / 4493,                            &
       1360,  383,  842, 2157,   30,  959,    3,  717, 1107,    2,     &
          2,    2,  836,  836, 1134,  836,  836,  426,  898,  898,     &
         65,  836,  836,  836,  836,  216,  104,  300,  836, 1022,     &
       1022, 1022, 1022, 1420, 1478, 1478, 1478,  283, 2246/
      DATA P(10),(C(10,I), I = 1,39) / 6737,                            &
       2602, 2818, 3240, 2528, 2260, 3141, 2857, 1484, 2113, 2265,     &
          2,    2, 2207, 2207, 2207,  542,  132,  934,  378,  378,     &
       2099,  934,  225,  225,  225,  169,  378, 2257, 2257, 2257,     &
       2257,  934, 2576,  934,  934,  934,  934,  934, 2257/
      DATA P(11),(C(11,I), I = 1,39) / 10111,                           &
       3071, 3114, 1170, 3432, 2726, 1098, 3371,  185,    4, 3143,     &
       5055,    2,    2,    2,    2,    2,    2,  334, 1254, 4146,     &
        617, 1879,    2,    2, 1146,  475, 4725,    2,    2,  475,     &
        475,  475,  475,  475,  638,  638,  638,    2, 3107/
      DATA P(12),(C(12,I), I = 1,39) / 15161,                           &
       6280, 6716, 7191, 2574, 3970,  687, 2990, 4054, 7092, 6207,     &
       3821,    2, 3821, 3821, 3821, 3821, 3821, 3821,  896, 2077,     &
       3536, 3104, 3320, 4562, 3320, 2900, 3956, 3956, 2954, 3784,     &
       3956, 2210, 3784, 1547, 1547, 1547, 1547, 1547, 1547/
      DATA P(13),(C(13,I), I = 1,39) / 22751,                           &
       9438,10221, 6766, 6496, 9448,  629, 2936, 4929, 6234, 9048,     &
       6339, 6339,    2,    2,    2, 6725, 6725, 9300, 9300, 6725,     &
       6725, 9300,10128, 6990,  783, 6990,  435,  435,  435,  435,     &
        435, 6720, 8772, 6720, 6720, 6720, 8772, 2335, 2335/
      DATA P(14), (C(14,I), I=1,39)/  34127,                            &
        7002,  6450,  5206, 11453,  4186,  7777,  5806, 14858, 10620,  &
        5497,   938,  8883, 10669,  2444,   876, 13293,  3725,   213,  &
        3348,  4998,     2,     2,     2,     2,     2,     2,     2,  &
           2,     2,     2,     2,     2,     2,     2,  9777,     2,  &
           2,     2,     2/
      DATA P(15), (C(15,I), I=1,39)/  51193,                            &
        6964, 20979,  9862, 15012, 13588,   204,  4339, 20078,   551,  &
         957,  9428,  8919,  8919,  8919,  8919, 17428,  2075, 17428,  &
        1315,  9428, 12635, 17428, 17428, 17428, 23061, 21994, 21994,  &
      21994,  8406,  8392,  8392,  8392,  8392,  8392,  8392, 23956,  &
       23061, 15077,  1027/
      DATA P(16), (C(16,I), I=1,39)/  76801,                            &
       26053, 12197, 25526, 25526,  1023, 20586, 26053, 26053,  1688,  &
        2924,  6925,  6925, 20629, 27112, 22711,  5298,  5298, 22711,  &
       22711,  2993,  1871,  8093,  8093,  1871,   822, 15206, 15206,  &
        6326,  6326, 15206,  6326,  6326,  6326,  6326, 24628, 23450,  &
        6326,  6326,  6326/
      DATA P(17), (C(17,I), I=1,39)/ 115183,                            &
       15990, 14064, 18027,  9380, 17512, 14711, 12807,  5241,  8528,  &
        8528, 12412,  9204,  9198,  9198,  9198,  9198,  9198,  7539,  &
        9198,  9198,  9198, 18397, 17518, 17518, 18397,  7564,  9198,  &
        9198,  9198,  9198, 11737, 11737, 11737, 11737, 11737,  7564,  &
        7564,  7564,   580/
      DATA P(18), (C(18,I), I=1,39)/ 259151,                            &
        9841,  4842,  1518,  5362,  3899,  4842,   186,   186,   186,  &
        1518,  6513,  4842,  6226,  6226,  4842,  3523,  3523,  3398,  &
        3398,  3398,  3398,  3398,  3398,  4842,  4842,  6513,   120,  &
         120,   120,  5341,  5341,  4931,   186,   186,   186,   186,  &
        1518,   186,   186/
      INTVLS = 0
      IF ( IR .EQ. 0 ) THEN
         NSAMP = MINSMP 
         DO I = 1,PLIM
            NP = I
            IF ( MINVLS .LT. NSAMP*P(I) ) GO TO 10
         END DO
         NSAMP = MAX( MINSMP, MINVLS/P(NP) )
      ENDIF      
 10   VK(1) = ONE/P(NP)
      V(1) = 1
      DO I = 2,NDIM
         V(I) = MOD( C(NP,NDIM-1)*V(I-1), P(NP) )
         VK(I) = V(I)*VK(1)
      END DO
      SAMPLS = NSAMP/2
      DO J = 1,NF
         FINEST(J) = 0
         ABSERR(J) = 0
      END DO
      DO I = 1,SAMPLS
         CALL VKROSM( NDIM, A, B, P(NP), VK, NF, FUNSUB,   &
            WORK, WORK(NF+1), X, ALPHA )
         DO J = 1,NF
            FINDIF = ( WORK(J) - FINEST(J) )/I
            FINEST(J) = FINEST(J) + FINDIF
            ABSERR(J) = ( I - 2 )*ABSERR(J)/I + FINDIF**2 
         END DO
      END DO
      INTVLS = INTVLS + 2*SAMPLS*P(NP)
      INFORM = 0
      DO J = 1,NF
         ABSERR(J) = SQRT( ABSERR(J) )
         IF ( INFORM .EQ. 0 )THEN 
            IF ( ABSERR(J) .GT. MAX(ABS(FINEST(J))*RELEPS,ABSEPS) ) THEN
               INFORM = 1
               IF ( NP .LT. PLIM ) THEN
                  NP = NP + 1
               ELSE
                  NSAMP = MAX( MINSMP, &
                              MIN( 3*NSAMP/2, (MAXVLS-INTVLS)/P(NP) ) ) 
               END IF
            END IF
         END IF
      END DO
      IF ( INFORM .EQ. 1 .AND. INTVLS+NSAMP*P(NP) .LE. MAXVLS ) GO TO 10
      END

      SUBROUTINE VKROSM( NDIM, A, B, NPTS, VK, NF, FUNSUB,                  &
                        SUMFUN, FUNVAL, ALPHA, X )
             use mod_kinds, only : i4,dp
             use omp_lib
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: VKROSM
           !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: VKROSM
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: VKROSM
#endif                        
      EXTERNAL FUNSUB
      INTEGER(kind=i4) NDIM, NPTS, K, J, I, NF
      REAL(kind=dp) VK(*), ONE, VOLUME, UNIRAN, XT 
      PARAMETER ( ONE = 1 )
      REAL(kind=dp) A(*), B(*), ALPHA(*), X(*), SUMFUN(*), FUNVAL(*)
      VOLUME = 1
      DO J = 1,NDIM
         ALPHA(J) = UNIRAN()
         VOLUME = VOLUME*( B(J) - A(J) )
      END DO
      DO I = 1,NF
         SUMFUN(I) = 0
      END DO
      DO K = 1,NPTS
         !$OMP SIMD ALIGNED(ALPHA,VK,A,B:64)
         DO J = 1,NDIM
            XT = ABS( 2*MOD( ALPHA(J) + VK(J)*K, ONE ) - 1 )
            X(J) = A(J) + XT*( B(J) - A(J) )
         END DO
         CALL FUNSUB( NDIM, X, NF, FUNVAL )
         !$OMP SIMD REDUCTION(+:SUMFUN) ALIGNED(SUMFUN,FUNVAL:64)
         DO I = 1, NF
            SUMFUN(I) = SUMFUN(I) + FUNVAL(I)/2
         END DO
         !$OMP SIMD ALIGNED(X,B,A:64)
         DO J = 1,NDIM
            X(J) = B(J) + A(J) - X(J)
         END DO
         CALL FUNSUB( NDIM, X, NF, FUNVAL )
         !$OMP SIMD REDUCTION(+:SUMFUN) ALIGNED(SUMFUN,FUNVAL:64)
         DO I = 1, NF
            SUMFUN(I) = SUMFUN(I) + FUNVAL(I)/2
         END DO
      END DO
      !$OMP SIMD ALIGNED(SUNFUN:64)
      DO I = 1, NF
         SUMFUN(I) = VOLUME*SUMFUN(I)/NPTS
      END DO
      END
!*
!!*    This file contains ADBYSR and supporting functions and subroutines.
!*
      SUBROUTINE ADBYSR( M, NF, MNVALS, MXVALS, F, EPSABS, EPSREL, RS,          &
                        KEY, VALUE, ERROR, INTVLS, INFORM, NW, WK )
             use mod_kinds, only : i4,dp
             
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: ADBYSR
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ADBYSR
#endif 
#if 0                                
****BEGIN PROLOGUE ADBYSR
****AUTHOR
*            Alan Genz, Department of Mathematics, Washington State
*            University, Pullman, WA 99164-3113, USA
*              Email: alangenz@wsu.edu
*
****KEYWORDS automatic multidimensional integrator,
*            n-dimensional region ( -infin, infin )^n, Gaussian weight
****PURPOSE  The routine calculates an approximation to a given
*            vector of definite integrals
*
*
*      infin     infin 
*     I    ...  I     w(X)( F ,F ,...,F   ) DX(M)...DX(2)DX(1),
*     -infin    -infin       1  2      NF
*
*       where F = F ( X ,X ,...,X  ), I = 1,2,...,NF,
*              I   I   1  2      M
*
*       w(X) = (2PI)^(-M/2)EXP(-( X(1)**2 + ... + X(M)**2 )/2),
*
*            hopefully satisfying for K = 1, 2, ... NF
*            ABS( I(K)-VALUE(K) ) .LE. MAX( EPSABS, EPSREL*ABS(I(K)) )
****DESCRIPTION Computation of integrals over infinite regions with
*               Gaussian weight function.
*
*   ON ENTRY
*
*     M  Integer number of variables, M > 1.
*     NF Integer number of components of the integral.
*     MXVALS Integer maximum number of F calls.
*            When RS > 0, this is the maximum number of new F calls.
*     F Externally declared subroutine for computing all components 
*            of the integrand at the given evaluation point.
*            It must have parameters ( M, X, NF, FUNS )
*            Input parameters:
*              M   Integer number of variables.
*              X   Real array of length M, the evaluation point.
*              NF Integer number of components for I.
*            Output parameter:
*              FUNS Real array of length NF, components of the integrand
*               evaluated at the point X.
*     EPSABS Real requested absolute accuracy.
*     EPSREL Real requested relative accuracy.
*     RS Integer.
*            If RS = 0, this is the first attempt to compute the integral(s).
*            If RS = 1, then a previous calculation is continued. In 
*              this case, the only parameters that may be changed (with 
*              respect to the previous call of the subroutine) are  
*              MXVALS, EPSABS, EPSREL and KEY.
*     KEY    Integer.
*            Key to selected local integration rule.
*            KEY = 0 gives the user a default rule
*            KEY = 1 gives the user a degree 7 integration rule.
*                  This is the recommended general purpose default rule.
*            KEY = 2 gives the user a degree 9 integration rule.
*                  This rule is recommended for oscillatory problems.
*            KEY = 3 gives the user a degree 5 integration rule.
*     WK   Real work array
*
*   ON RETURN
*
*     VALUE Real array of length NF of approximations to the 
*            components of the integral.
*     ERROR Real array of length NF of estimates of absolute accuracies.
*     INTVLS Integer number of F calls used.
*            When RS > 0, this is the number of new F calls.
*     INFORM  Integer.
*            INFORM = 0 for normal exit, when 
*              ERROR(K) <= MAX( EPSABS, ABS( VALUE(K) )EPSREL ) for
*              0 < K <= NF, with <= MXVALS function values. 
*            INFORM = 1 if MXVALS was too small to obtain the required 
*              accuracy. In this case values of VALUE are returned
*              with estimated absolute accuracies ERROR.
*
****END PROLOGUE ABBYSR
*
*   Global variables.
*
#endif
      EXTERNAL F
      INTEGER(kind=i4) M, NF, NW, MNVALS, MXVALS, RS, KEY
      INTEGER(kind=i4) INTVLS, INFORM
      REAL(kind=dp) EPSABS, EPSREL
      REAL(kind=dp) VALUE(*), ERROR(*), WK(*)
      CALL ADPTRS( M, NF, MNVALS, MXVALS, F, EPSABS, EPSREL, KEY,             &
                  NW, RS, VALUE, ERROR, INTVLS, INFORM, WK )
!*
!****END ADBYSR
!*
      END
      
      SUBROUTINE ADPTRS( NDIM, NUMFUN, MINPTS, MAXPTS, FUNSUB, EPSABS,        &
          EPSREL, KEY, NW, RESTAR, RESULT, ABSERR, NEVAL, IFAIL, WORK )  
             use mod_kinds, only : i4,dp
            
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: ADPTRS
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ADPTRS
#endif    
#if 0                 
****BEGIN PROLOGUE ADPTRS
****AUTHOR
*            Alan Genz, Department of Mathematics, Washington State
*            University, Pullman, WA 99164-3113, USA
*              Email: genz@gauss.math.wsu.edu
****KEYWORDS automatic multidimensional integrator,
*            n-dimensional hyper-rectangles,
*            general purpose, global adaptive
****PURPOSE  The routine calculates an approximation to a given
*            vector of definite integrals
*
*      1    1        1
*     I    I    ... I  (F ,F ,...,F      ) DX(NDIM)...DX(2)DX(1),
*      0    0        0   1  2      NUMFUN
*
*       where F = F (X ,X ,...,X    ), I = 1,2,...,NUMFUN.
*              I   I  1  2      NDIM
*
*            hopefully satisfying for each component of I the following
*            claim for accuracy:
*            ABS( I(K)-RESULT(K) ) .LE. MAX( EPSABS, EPSREL*ABS(I(K)) )
****DESCRIPTION Computation of integrals over hyper-rectangular
*            regions.
*            ADPTRS is a driver for the integration routine
*            ADBASR, which repeatedly subdivides the region
*            of integration and estimates the integrals and the
*            errors over the subregions with greatest
*            estimated errors until the error request
*            is met or MAXPTS function evaluations have been used.
*
*   ON ENTRY
*
*     NDIM   Integer.
*            Number of variables. 0 < NDIM <=  20.
*     NUMFUN Integer.
*            Number of components of the integral.
*     MINPTS Integer.
*            Minimum number of function evaluations.
*     MAXPTS Integer.
*            Maximum number of function evaluations.
*            The number of function values for each subregion is NUM.
*            If NDIM = 1 Then NUM = 15
*            ElseIf KEY = 0 Then 
*                  if NDIM < 12 then NUM = 1 + 2*NDIM*(NDIM+3) + 2**NDIM
*                             else NUM = 1 + 2*NDIM*(NDIM+4)
*               Elseif KEY = 1 Then NUM = 1 + 2*NDIM*(NDIM+3) + 2**NDIM
*               Elseif KEY = 2 Then NUM = 1 + 4*NDIM + 6*NDIM*NDIM 
*                                 + 4*NDIM*(NDIM-1)*(NDIM-2)/3 + 2**NDIM
*               Else NUM = 1 + 2*NDIM*(NDIM+4).
*            You must have MAXPTS >= NUM and MAXPTS >= MINPTS.
*     FUNSUB Externally declared subroutine for computing
*            all components of the integrand at the given
*            evaluation point.
*            It must have parameters (NDIM,X,NUMFUN,FUNVLS)
*            Input parameters:
*              NDIM   Integer that defines the dimension of the
*                     integral.
*              X      Real array of dimension NDIM
*                     that defines the evaluation point.
*              NUMFUN Integer that defines the number of
*                     components of I.
*            Output parameter:
*              FUNVLS Real array of dimension NUMFUN
*                     that defines NUMFUN components of the integrand.
*
*     EPSABS Real.
*            Requested absolute accuracy.
*     EPSREL Real.
*            Requested relative accuracy.
*     KEY    Integer.
*            Key to selected local integration rule.
*            KEY = 0 gives the user a default rule
*            KEY = 1 gives the user a degree 7 integration rule.
*                  This is the recommended general purpose rule.
*            KEY = 2 gives the user a degree 9 integration rule.
*                  This rule is recommended for oscillatory problems.
*            KEY = 3 gives the user a degree 5 integration rule.
*     NW     Integer.
*            Defines the length of the working array WORK.
*            Let MAXSUB denote the maximum allowed number of subregions
*            for the given values of MAXPTS, KEY and NDIM.
*            With MAXSUB = (MAXPTS-NUM)/(2*NUM) + 1, you must have 
*             NW >= MAXSUB*( 2*NDIM + 2*NUMFUN + 2 ) + 7*NUMFUN + NDIM.
*     RESTAR Integer.
*            If RESTAR = 0, this is the first attempt to compute
*            the integral.
*            If RESTAR = 1, then we restart a previous attempt.
*            In this case the only parameters for ADPTRS that may
*            be changed (with respect to the previous call of ADPTRS)
*            are MINPTS, MAXPTS, EPSABS, EPSREL, KEY and RESTAR.
*
*   ON RETURN
*
*     RESULT Real array of dimension NUMFUN.
*            Approximations to all components of the integral.
*     ABSERR Real array of dimension NUMFUN.
*            Estimates of absolute accuracies.
*     NEVAL  Integer.
*            Number of function evaluations used by ADPTRS.
*     IFAIL  Integer.
*            IFAIL = 0 for normal exit, when 
*              ABSERR(K) <= MAX( EPSABS, ABS(RESULT(K))*EPSREL ) for
*              all K, 0 < K <= NUMFUN, with <= MAXPTS function values. 
*            IFAIL = 1 if MAXPTS was too small to obtain the required 
*              accuracy. In this case values of RESULT are returned
*              with estimated absolute accuracies ABSERR.
*            IFAIL = 2 if KEY is less than 0 or KEY > 3.
*            IFAIL = 3 if NDIM is less than 2 or NDIM > 20.
*            IFAIL = 4 if NUMFUN is less than 1.
*            IFAIL = 6 if MAXPTS is less than NUM.
*            IFAIL = 7 if MAXPTS is less than MINPTS.
*            IFAIL = 8 if EPSABS < 0 and EPSREL < 0.
*            IFAIL = 9 if NW is too small.
*            IFAIL = 10 if RESTAR < 0 or RESTAR > 1.
*     WORK   Real array of dimension NW, used as working storage.
*            Let WRKSUB = ( NW - NDIM - 6*NUMFUN )/( 2*NDIM+2*NUMFUN+2 )
*            WORK(1),...,WORK(NUMFUN*MAXSUB) contain
*              the estimated components of the integrals over the
*              subregions.
*            WORK(NUMFUN*WRKSUB+1),...,WORK(2*NUMFUN*MAXSUB) contain
*              the estimated errors over the subregions.
*            WORK(2*NUMFUN*WRKSUB+1),...,WORK(2*NUMFUN*WRKSUB+NDIM*
*              MAXSUB) contain the centers of the subregions.
*            WORK(2*NUMFUN*WRKSUB+NDIM*WRKSUB+1),...,WORK((2*NUMFUN+
*              NDIM)*WRKSUB+NDIM*MAXSUB) contain subregion half widths.
*            WORK(2*NUMFUN*WRKSUB+2*NDIM*WRKSUB+1),...,WORK(2*NUMFUN*
*              WRKSUB+2*NDIM*WRKSUB+MAXSUB) contain the greatest errors
*              in each subregion.
*            WORK((2*NUMFUN+2*NDIM+1)*WRKSUB+1),...,WORK((2*NUMFUN+
*              2*NDIM+1)*WRKSUB+MAXSUB) contain the heap pointers
*              for the subregions.
*
****ROUTINES CALLED BSCHRS, ADBASR
****END PROLOGUE ADPTRS
*
*   Global variables.
*
#endif

      EXTERNAL FUNSUB
      INTEGER(kind=i4) NDIM, NUMFUN, MINPTS, MAXPTS, KEY, NW, RESTAR
      INTEGER(kind=i4) NEVAL, IFAIL
      REAL(kind=dp) EPSABS, EPSREL
      REAL(kind=dp) RESULT(NUMFUN), ABSERR(NUMFUN), WORK(NW)
#if 0
*
*   Local variables.
*
*   MAXDIM Integer.
*          The maximum allowed value of NDIM.
*   MAXSUB Integer.
*          The maximum allowed number of subdivisions
*          for the given values of KEY, NDIM and MAXPTS.
*   MINSUB Integer.
*          The minimum allowed number of subregions for the given
*          values of MINPTS, KEY and NDIM.
*   WRKSUB Integer.
*          The maximum allowed number of subregions as a function 
*          of NW, NUMFUN, NDIM and NPROC. This determines the length
*          of the main work arrays.
*   NUM    Integer. The number of integrand evaluations needed
*          over each subregion.
*
#endif
      INTEGER(kind=i4) MAXDIM, MAXSUB, MINSUB, NUM, NSUB, NEWPTS, NEWCLS, TOTCLS       
      PARAMETER ( MAXDIM = 20 )
      REAL(kind=dp) A(MAXDIM), B(MAXDIM)
      INTEGER(kind=i4) WRKSUB, I, J, JL, I1,I2,I3,I4,I5,I6,I7,I8, CLTOTL, RS 
      REAL(kind=dp) EPOWER, CLINIT
      PARAMETER ( EPOWER = 0.5 )
      SAVE NSUB, CLTOTL, CLINIT
#if defined(_OPENMP)
!$OMP THREADPRIVATE(NSUB,CLTOTL,CLINIT)
#endif
!*
!****FIRST EXECUTABLE STATEMENT ADPTRS
!*
!*   Compute NUM, WTLENG, MAXSUB and MINSUB,
!*   and check the input parameters.
!*
!*
!*   On restart runs the number of subregions from the
!*   previous call is assigned to NSUB.
!*
      IF ( RESTAR .EQ. 0 ) THEN
         NSUB = 1
         CLTOTL = 0
         DO I = 1,NUMFUN
            WORK( NW - NUMFUN + I ) = 0
         END DO
         JL = 3
         NEWPTS = MAXPTS/( 2**JL - 1 )
         CLINIT = NEWPTS 
         RS = 0
      ELSE
         NEWPTS = MAXPTS
         JL = 1
         RS = 1
      END IF
      DO I = 1, NDIM
         A(I) = 0
         B(I) = 1
      END DO
      TOTCLS = 0
!*
!*   Split up the work space.
!*
      WRKSUB = ( NW - NDIM - 6*NUMFUN )/( 2*NDIM + 2*NUMFUN + 2 )
      I1 = 1
      I2 = I1 + WRKSUB*NUMFUN
      I3 = I2 + WRKSUB*NUMFUN
      I4 = I3 + WRKSUB*NDIM
      I5 = I4 + WRKSUB*NDIM
      I6 = I5 + WRKSUB
      I7 = I6 + WRKSUB
      I8 = I7 + NDIM
      DO J = 1, JL
         CALL BASCHC( MAXDIM, NDIM, NUMFUN, A,B, MINPTS, NEWPTS, EPSABS,         &
             EPSREL, KEY, NW, RS, NUM, NSUB, MAXSUB,MINSUB, IFAIL )
         IF ( IFAIL .EQ. 0 ) THEN
            CALL ADBASR( NDIM, NUMFUN, A,B, MINSUB, MAXSUB, FUNSUB,              &
                EPSABS, EPSREL, KEY, RS, NUM, RESULT, ABSERR,                    &
                NEWCLS, NSUB, IFAIL, WORK(I1), WORK(I2), WORK(I3),               &
                WORK(I4), WORK(I5), WORK(I6), WORK(I7), WORK(I8) )
            TOTCLS = TOTCLS + NEWCLS
            CLTOTL = CLTOTL + NEWCLS
            IF ( CLTOTL .GT. NEWCLS ) THEN
               DO I = 1,NUMFUN
                  ABSERR(I) = ABS( RESULT(I) - WORK( NW - NUMFUN + I ) )         &
                           + ABSERR(I)*( CLINIT/CLTOTL )**EPOWER
               END DO
            END IF
            DO I = 1,NUMFUN
               WORK( NW - NUMFUN + I ) = RESULT(I)
            END DO
         END IF
         NEWPTS = 2*NEWPTS
         RS = 1
      END DO
      NEVAL = TOTCLS
!*
!****END ADPTRS
!*
      END
      
      SUBROUTINE ADBASR( NDIM,NUMFUN,A,B, MINSUB,MAXSUB, FUNSUB, EPSABS,         &
          EPSREL, KEY, RESTAR, NUM, RESULT, ABSERR, NEVAL, NSUB, IFAIL,         &
          VALUES, ERRORS, CENTRS, HWIDTS, GREATE, PONTRS, X, WORK )
            use mod_kinds, only : i4,dp
            
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: ADBASR
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ADBASR
#endif      
#if 0        
****BEGIN PROLOGUE ADBASR
****KEYWORDS automatic multidimensional integrator,
*            n-dimensional hyper-rectangles,
*            general purpose, global adaptive
****PURPOSE  The routine calculates an approximation to a given
*            vector of definite integrals, I, over a hyper-rectangular
*            region hopefully satisfying for each component of I the
*            following claim for accuracy:
*            ABS(I(K)-RESULT(K)).LE.MAX(EPSABS,EPSREL*ABS(I(K)))
****DESCRIPTION Computation of integrals over hyper-rectangular
*            regions.
*            ADBASR repeatedly subdivides the region
*            of integration and estimates the integrals and the
*            errors over the subregions with  greatest
*            estimated errors until the error request
*            is met or MAXSUB subregions are stored.
*            The regions are devided in two equally sized parts along
*            the direction with greatest absolute fourth divided
*            difference.
*
*   ON ENTRY
*
*     NDIM   Integer.
*            Number of variables. 1 < NDIM <= MAXDIM.
*     NUMFUN Integer.
*            Number of components of the integral.
*     A      Real array of dimension NDIM.
*            Lower limits of integration.
*     B      Real array of dimension NDIM.
*            Upper limits of integration.
*     MINSUB Integer.
*            The computations proceed until there are at least
*            MINSUB subregions in the data structure.
*     MAXSUB Integer.
*            The computations proceed until there are at most
*            MAXSUB subregions in the data structure.
*
*     FUNSUB Externally declared subroutine for computing
*            all components of the integrand in the given
*            evaluation point.
*            It must have parameters (NDIM,X,NUMFUN,FUNVLS)
*            Input parameters:
*              NDIM   Integer that defines the dimension of the
*                     integral.
*              X      Real array of dimension NDIM
*                     that defines the evaluation point.
*              NUMFUN Integer that defines the number of
*                     components of I.
*            Output parameter:
*              FUNVLS Real array of dimension NUMFUN
*                     that defines NUMFUN components of the integrand.
*
*     EPSABS Real.
*            Requested absolute accuracy.
*     EPSREL Real.
*            Requested relative accuracy.
*     KEY    Integer.
*            Key to selected local integration rule.
*     RESTAR Integer.
*            If RESTAR = 0, this is the first attempt to compute
*            the integral.
*            If RESTAR = 1, then we restart a previous attempt.
*              (In this case the output parameters must not be changed 
*               since the last exit.)
*     NUM    Integer.
*            The number of function evaluations over each subregion.
*     NSUB   Integer.
*            If RESTAR = 1, then NSUB must specify the number
*              of subregions stored in the previous call to ADBASR.
*
*   ON RETURN
*
*     RESULT Real array of dimension NUMFUN.
*            Approximations to all components of the integral.
*     ABSERR Real array of dimension NUMFUN.
*            Estimates of absolute accuracies.
*     NEVAL  Integer.
*            Number of function evaluations used.
*     NSUB   Integer.
*            Number of stored subregions.
*     IFAIL  Integer.
*            IFAIL = 0 for normal exit, when 
*              ABSERR(K) <= MAX( EPSABS, ABS(RESULT(K))*EPSREL ) for all
*              K, 1 <= K <= NUMFUN, with <= MAXSUB subregions processed. 
*            IFAIL = 1 if MAXSUB was too small to obtain the required 
*              accuracy. In this case values of RESULT with estimated
*              absolute accuracies ABSERR are returned.
*     VALUES Real array of dimension (NUMFUN,*).
*            Used to store estimated values of the integrals
*            over the subregions.
*     ERRORS Real array of dimension (NUMFUN,*).
*            Used to store the corresponding estimated errors.
*     CENTRS Real array of dimension (NDIM,*).
*            Used to store the centers of the stored subregions.
*     HWIDTS Real array of dimension (NDIM,*).
*            Used to store the half widths of the stored subregions.
*     GREATE Real array of dimension (*).
*            Used to store the greatest estimated errors in
*            all subregions.
*     PONTRS Real array of dimension (*).
*            PONTRS is used to store heap pointers.
*     WORK   Real work array of length at least 5*NUMFUN
*            Used  in BSRLSR and SDFFRS.
*     X      Real array of length NDIM.
*            Work array used in BSRLSR.
*
****REFERENCES
*
*   P. van Dooren and L. de Ridder, Algorithm 6, An adaptive algorithm
*   for numerical integration over an n-dimensional cube, J.Comput.Appl.
*   Math. 2(1976)207-217.
*
*   A.C. Genz and A.A. Malik, Algorithm 019. Remarks on algorithm 006:
*   An adaptive algorithm for numerical integration over an
*   N-dimensional rectangular region,J.Comput.Appl.Math. 6(1980)295-302.
*
****  ROUTINES CALLED TRESTR, BSRLSR, SDFFRS
****  END PROLOGUE ADBASR
*
*     Global variables.
*
#endif
      EXTERNAL FUNSUB
      INTEGER(kind=i4) NDIM, NUMFUN, MINSUB, MAXSUB, KEY, RESTAR
      INTEGER(kind=i4) NUM, NEVAL, NSUB, IFAIL
      REAL(kind=dp) A(NDIM),B(NDIM), EPSABS, EPSREL
      REAL(kind=dp) RESULT(NUMFUN), ABSERR(NUMFUN)
      REAL(kind=dp) VALUES(NUMFUN,*), ERRORS(NUMFUN,*)
      REAL(kind=dp) CENTRS(NDIM,*), HWIDTS(NDIM,*)
      REAL(kind=dp) GREATE(*), PONTRS(*)
      REAL(kind=dp) WORK(*), X(*)
#if 0
*
*     Local variables.
*
*   INTSGN is used to get correct sign on the integral.
*   SBRGNS is the number of stored subregions.
*   POINTR Pointer to the position in the datastructure where
*          the new subregions are to be stored.
*
#endif
      INTEGER(kind=i4) I, J, SBRGNS, POINTR, DIRECT, WTLENG 
      INTEGER(kind=i4) MAXDIM, MAXWTS, NUMNUL
      PARAMETER ( MAXWTS = 9, NUMNUL = 5, MAXDIM = 20)
      REAL(kind=dp) G(MAXWTS*MAXDIM), W(MAXWTS*NUMNUL)
#if 0
*
****  FIRST PROCESSING STATEMENT for ADBASR
*
*     Call BSINIT to compute the weights and abscissas of
*     the function evaluation points.
*
#endif
      CALL BSINIT( NDIM, KEY, WTLENG, G, W )
!*
!*     Get the correct sign on the integral.
!*
      NEVAL = 0
      SBRGNS = NSUB
      IF ( RESTAR .EQ. 0 ) THEN

!*     
!*     Initialize the SBRGNS, CENTRS and HWIDTS.
!*
         DO J = 1, NDIM
            CENTRS(J,1) = ( A(J) + B(J) )/2
            HWIDTS(J,1) = ( B(J) - A(J) )/2
         END DO
!*     
!*     Apply BSRLSR over the whole region.
!*     
         CALL BSRLSR( NDIM, CENTRS, HWIDTS, WTLENG, G, W,                     &
             NUMFUN, FUNSUB, X, WORK, VALUES, ERRORS, GREATE )
         NEVAL = NEVAL + NUM
!*     
!*     Store results in heap.
!*     
         CALL TRESTR( SBRGNS, SBRGNS, PONTRS, GREATE )
!*     
!****  End initialisation.
!*     
      END IF
!*     
!*     Check for termination.
!*     
 10   IFAIL = 0
      DO J = 1,NUMFUN
         RESULT(J) = 0
         ABSERR(J) = 0
         DO I = 1,SBRGNS
            RESULT(J) = RESULT(J) + VALUES(J,I)
            ABSERR(J) = ABSERR(J) + ERRORS(J,I)
         END DO
         IF ( ABSERR(J) .GT. MAX( EPSABS, EPSREL*ABS(RESULT(J)) ) )           &
                IFAIL = 1
      END DO
!*
!****  Begin loop while the error is too great,
!*     and SBRGNS + 1 is less than MAXSUB.
!*     
      IF ( ( IFAIL .NE. 0 .AND. SBRGNS+1 .LE. MAXSUB )                        &
                          .OR.  SBRGNS  .LT. MINSUB )   THEN
!*     
!*     If we are allowed to divide further,
!*     prepare to apply basic rule over each half of the
!*     subregion with greatest error.
!*     
         POINTR = PONTRS(1)
         CALL SDFFRS( NDIM, CENTRS(1,POINTR), HWIDTS(1,POINTR),               &
             NUMFUN, FUNSUB, X, WORK, DIRECT )
!*     
!*     Divide the subregion in two halves. 
!*     
         HWIDTS(DIRECT,POINTR) = HWIDTS(DIRECT,POINTR)/2
         SBRGNS = SBRGNS + 1
         DO J = 1,NDIM
            CENTRS(J,SBRGNS) = CENTRS(J,POINTR)
            HWIDTS(J,SBRGNS) = HWIDTS(J,POINTR)
         END DO
!*
!*     Compute integral and error over first half and store results.
!*
         CENTRS(DIRECT,POINTR) = CENTRS(DIRECT,POINTR)                       &
                              - HWIDTS(DIRECT,POINTR)
         CALL BSRLSR( NDIM, CENTRS(1,POINTR), HWIDTS(1,POINTR),              &
             WTLENG, G, W, NUMFUN, FUNSUB, X, WORK,                          &
             VALUES(1,POINTR), ERRORS(1,POINTR), GREATE(POINTR) )
         CALL TRESTR( POINTR, SBRGNS-1, PONTRS, GREATE )
!*
!*     Compute integral and error over second half and store results.
!*
         CENTRS(DIRECT,SBRGNS) = CENTRS(DIRECT,SBRGNS)                       &
                              + HWIDTS(DIRECT,SBRGNS)
         CALL BSRLSR( NDIM, CENTRS(1,SBRGNS), HWIDTS(1,SBRGNS),              &
            WTLENG, G, W, NUMFUN, FUNSUB, X, WORK,                           &
             VALUES(1,SBRGNS), ERRORS(1,SBRGNS), GREATE(SBRGNS) )
         CALL TRESTR( SBRGNS,  SBRGNS,  PONTRS, GREATE )
         NEVAL = NEVAL + 2*NUM
         GO TO 10
      END IF
      NSUB = SBRGNS
!*
!****END ADBASR
!*
      END
      
      SUBROUTINE SDFFRS( N, CENTER, HWIDTH, NF, FUNSUB,                      &
                        X, WORK, DIVAXN )
                 use mod_kinds, only : i4,dp
            
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: SDFFRS
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: SDFFRS
#endif    
!*
!*     Compute differences and subdivision axis
!*
      EXTERNAL FUNSUB
      INTEGER(kind=i4) I, J, K, N, NF, DIVAXN, DCM, DCR
      REAL(kind=dp) CENTER(*), HWIDTH(*), X(*), WORK(NF,0:2)
      REAL(kind=dp) DIFFER, DIFMAX, DIFSUM
      PARAMETER ( DCM = 4, DCR = 4**DCM )
      INTEGER(kind=i4) DC(0:DCM)
      SAVE DC
      DATA DC / 70, -56, 28, -8, 1 /
      DIVAXN = 1
      DO I = 1, N
         IF ( HWIDTH(I) .GT. HWIDTH(DIVAXN) ) DIVAXN = I
         X(I) = CENTER(I)
      END DO
      DIFMAX = 0
      CALL FUNTRN( FUNSUB, N, CENTER, NF, WORK(1,0) ) 
      DO I = 1, N
         DO J = 1, NF
            WORK(J,1) = DC(0)*WORK(J,0)
         END DO
         DO K = -DCM, DCM
            IF ( K .NE. 0 ) THEN
               X(I) = CENTER(I) + K*HWIDTH(I)/( DCM + 1 )
               CALL FUNTRN( FUNSUB, N, X, NF, WORK(1,2) ) 
               DO J = 1, NF
                  WORK(J,1) = WORK(J,1) + DC(ABS(K))*WORK(J,2) 
               END DO
            END IF
         END DO
         X(I) = CENTER(I)
         DIFSUM = 0
         DO J = 1, NF
           DIFFER = ABS( WORK(J,1) )
!*
!*     Ignore differences below roundoff
!*     
            IF ( ABS(WORK(J,0)) + DIFFER/DCR .GT. ABS(WORK(J,0)) )   &
                DIFSUM = DIFSUM + DIFFER
         END DO
         DIFSUM = SQRT(HWIDTH(I))*DIFSUM
         IF ( DIFSUM .GT. DIFMAX ) THEN
            DIFMAX = DIFSUM
            DIVAXN = I
         END IF
      END DO
      END
      
      SUBROUTINE BSRLSR(NDIM, CENTER, HWIDTH, WTLENG, G, W,          &
          NUMFUN, FUNSUB, X, NULL, BASVAL, RGNERR, GREAT)
          use mod_kinds, only : i4,dp
          use omp_lib  
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: BSRLSR
           !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: BSRLSR
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: BSRLRS
#endif    
#if 0
****BEGIN PROLOGUE BSRLSR
****KEYWORDS basic numerical integration rule
****PURPOSE  To compute basic integration rule values.
****AUTHOR   
*            Alan Genz, Department of Mathematics, Washington State
*            University, Pullman, WA 99163-3113, USA
*              Email: genz@gauss.math.wsu.edu
****LAST MODIFICATION 93-08-20
****DESCRIPTION BSRLSR computes basic integration rule values for a
*            vector of integrands over a hyper-rectangular region.
*            These are estimates for the integrals. 
*
*   ON ENTRY
*
*   NDIM   Integer.
*          Number of variables.
*   CENTER Real array of dimension NDIM.
*          The coordinates for the center of the region.
*   HWIDTH Real Array of dimension NDIM.
*          HWIDTH(I) is half of the width of dimension I of the region.
*   WTLENG Integer.
*          The number of weights in the basic integration rule.
*   G      Real array of dimension (NDIM,WTLENG).
*          The fully symmetric sum generators for the rules.
*          G(1,J), ..., G(NDIM,J) are the are the generators for the
*          points associated with the Jth weights.
*   W      Real array of dimension (5,WTLENG).
*          The weights for the basic and null rules.
*          W(1,1),...,W(1,WTLENG) are weights for the basic rule.
*          W(I,1),...,W(I,WTLENG), for I > 1 are null rule weights.
*   NUMFUN Integer.
*          Number of components for the vector integrand.
*   FUNSUB Externally declared subroutine.
*          For computing the components of the integrand at a point X.
*          It must have parameters (NDIM,X,NUMFUN,FUNVLS).
*           Input Parameters:
*            X      Real array of dimension NDIM.
*                   Defines the evaluation point.
*            NDIM   Integer.
*                   Number of variables for the integrand.
*            NUMFUN Integer.
*                   Number of components for the vector integrand.
*           Output Parameters:
*            FUNVLS Real array of dimension NUMFUN.
*                   The components of the integrand at the point X.
*   X      Real Array of dimension NDIM.
*          A work array.
*   NULL   Real array of dimension (NUMFUN, 5)
*          A work array.
*
*   ON RETURN
*
*   BASVAL Real array of dimension NUMFUN.
*          The values for the basic rule for each component
*          of the integrand.
*   RGNERR Real array of dimension NUMFUN.
*          The error estimates for each component of the integrand.
*   GREAT  Real maximum error for RGNERR.
*
****ROUTINES CALLED: FLSMSR, FUNSUB, TWONRM
*
****END PROLOGUE BSRLSR
*
*   Global variables.
*
#endif
      EXTERNAL FUNSUB
      INTEGER(kind=i4) WTLENG, NUMFUN, NDIM, NUMNUL
      PARAMETER ( NUMNUL = 4 )
      REAL(kind=dp) GREAT, CENTER(*), X(*), HWIDTH(*), BASVAL(*),           &
          RGNERR(*), NULL(NUMFUN,*), W(5,*), G(NDIM,*)
!*
!*   Local variables.
!*
      REAL(kind=dp) RGNVOL, RGNCMP, RGNCPT, TWONRM
      INTEGER(kind=i4) I,J,K
!*
!****FIRST EXECUTABLE STATEMENT BSRLSR
!*
!*
      RGNVOL = 1
      DO I = 1,NDIM
         RGNVOL = RGNVOL*HWIDTH(I)
      END DO
      DO J = 1,NUMFUN
         BASVAL(J) = 0
         DO K = 1,NUMNUL
            NULL(J,K) = 0
         END DO
      END DO
!*     
!*    Finish computing the rule values.
!*
      DO I = 1,WTLENG
         CALL FLSMSR( NDIM, CENTER, HWIDTH, X, G(1,I), NUMFUN, FUNSUB,      &
                     RGNERR, NULL(1,5) )
         DO J = 1,NUMFUN
            BASVAL(J) = BASVAL(J) + W(1,I)*RGNERR(J)
            !$OMP SIMD REDUCTION(+:NULL) ALIGNED(NULL,W,RGNERR:64)
            DO K = 1,NUMNUL
               NULL(J,K) = NULL(J,K) + W(K+1,I)*RGNERR(J)
            END DO
         END DO
      END DO
!*
!*    Compute errors.
!*
      GREAT = 0
      DO J = 1,NUMFUN
         BASVAL(J) = RGNVOL*BASVAL(J)
         RGNERR(J) = TWONRM( NULL(J,1), NULL(J,2) )
         RGNCMP    = TWONRM( NULL(J,2), NULL(J,3) )
         RGNCPT    = TWONRM( NULL(J,3), NULL(J,4) )
         IF ( 4*RGNERR(J) .LT. RGNCMP .AND. 2*RGNCMP .LT. RGNCPT )          &
             RGNERR(J) = RGNERR(J)/2 
         IF ( 2*RGNERR(J) .GT. RGNCMP )                                     &
             RGNERR(J) = MAX( RGNERR(J), RGNCMP ) 
         RGNERR(J) = RGNVOL*RGNERR(J)*( 1 + ABS( BASVAL(J) ) )
         GREAT = GREAT + RGNERR(J)
      END DO
!*
!****END BSRLSR
!*
      END
      
      SUBROUTINE FLSMSR(NDIM,CENTER,HWIDTH,X,G,NUMFUN,FUNSUB,FULSMS,        &
                       FUNVLS)
                use mod_kinds, only : i4,dp
            
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: FLSMSR
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: FLSMSR
#endif    
#if 0   
****BEGIN PROLOGUE FLSMSR
****KEYWORDS fully symmetric sum
****PURPOSE  To compute fully symmetric basic rule sums
****AUTHOR   
*            Alan Genz, Department of Mathematics, Washington State
*            University, Pullman, WA 99163-3113, USA
*              Email: genz@gauss.math.wsu.edu
****LAST MODIFICATION 88-04-08
****DESCRIPTION FLSMSR computes a fully symmetric sum for a vector
*            of integrand values over a hyper-rectangular region.
*            The sum is fully symmetric with respect to the center of
*            the region and is taken over all sign changes and
*            permutations of the generators for the sum.
*
*   ON ENTRY
*
*   NDIM   Integer.
*          Number of variables.
*   CENTER Real array of dimension NDIM.
*          The coordinates for the center of the region.
*   HWIDTH Real Array of dimension NDIM.
*          HWIDTH(I) is half of the width of dimension I of the region.
*   X      Real Array of dimension NDIM.
*          A work array.
*   G      Real Array of dimension NDIM.
*          The generators for the fully symmetric sum. These MUST BE
*          non-negative and non-increasing.
*   NUMFUN Integer.
*          Number of components for the vector integrand.
*   FUNSUB Externally declared subroutine.
*          For computing the components of the integrand at a point X.
*          It must have parameters (NDIM, X, NUMFUN, FUNVLS).
*           Input Parameters:
*            X      Real array of dimension NDIM.
*                   Defines the evaluation point.
*            NDIM   Integer.
*                   Number of variables for the integrand.
*            NUMFUN Integer.
*                   Number of components for the vector integrand.
*           Output Parameters:
*            FUNVLS Real array of dimension NUMFUN.
*                   The components of the integrand at the point X.
*   ON RETURN
*
*   FULSMS Real array of dimension NUMFUN.
*          The values for the fully symmetric sums for each component
*          of the integrand.
*   FUNVLS Real array of dimension NUMFUN.
*          A work array.
*
****ROUTINES CALLED: FUNSUB
*
****END PROLOGUE FLSMSR
*
*   Global variables.
*
#endif
      EXTERNAL FUNSUB
      INTEGER(kind=i4) NDIM,NUMFUN
      REAL(kind=dp) CENTER(NDIM),HWIDTH(NDIM),X(NDIM),G(NDIM),               &
                      FULSMS(NUMFUN),FUNVLS(NUMFUN)
!*
!*   Local variables.
!*
      INTEGER(kind=i4) IXCHNG,LXCHNG,I,J,L
      REAL(kind=dp) GL,GI
!*
!****FIRST EXECUTABLE STATEMENT FLSMSR
!*
      DO J = 1, NUMFUN
         FULSMS(J) = 0
      END DO
!*     
!*     Compute centrally symmetric sum for permutation of G
!*     
 10   DO I = 1, NDIM
         X(I) = CENTER(I) + G(I)*HWIDTH(I)
      END DO
 20   CALL FUNTRN( FUNSUB, NDIM, X, NUMFUN, FUNVLS )
      DO J = 1,NUMFUN
         FULSMS(J) = FULSMS(J) + FUNVLS(J)
      END DO
      DO I = 1, NDIM
         G(I) = - G(I)
         X(I) = CENTER(I) + G(I)*HWIDTH(I)
         IF ( G(I) .LT. 0 ) GO TO 20
      END DO
!*     
!*     Find next distinct permuation of G and loop back for next sum.
!*     Permutations are generated in reverse lexicographic order.
!*     
      DO I = 2, NDIM
         IF ( G(I-1) .GT. G(I) ) THEN
            GI = G(I)
            IXCHNG = I - 1
            DO L = 1, (I-1)/2
               GL = G(L)
               G(L) = G(I-L)
               G(I-L) = GL
               IF ( GL .LE. GI ) IXCHNG = IXCHNG - 1
               IF ( G(L).GT. GI ) LXCHNG = L
            END DO
            IF ( G(IXCHNG) .LE. GI ) IXCHNG = LXCHNG
            G(I) = G(IXCHNG)
            G(IXCHNG) = GI
            GO TO 10
         END IF
      END DO
!*     
!*     Restore original order to generators
!*
      DO I = 1,NDIM/2
          GI = G(I)
          G(I) = G(NDIM-I+1)
          G(NDIM-I+1) = GI
       END DO
!*
!****END FLSMSR
!*
      END
      
      SUBROUTINE FUNTRN( FUNSUB, N, X, NF, FS )
                use mod_kinds, only : i4,dp
            
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: FUNTRUN
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: FUNTRUN
#endif          
      EXTERNAL FUNSUB
      REAL(kind=dp) X(*), FS(*), TWOPI, TWO
      INTEGER(kind=i4) I, N, NF, MXN, P, MN, TWIMN 
      PARAMETER ( MXN = 20, TWOPI = 6.28318530717958647688D0, TWO = 2 )
      REAL(kind=dp) Y(MXN), R, TCHINV, YT, YO, YN, YS
      R = TCHINV( N, X(N) )
      MN = MOD( N, 2 )
      P = N/2
      YO = 1
      DO I = P-1, 1, -1
         TWIMN = 2*I + MN
         YN = YO*X(I)**( TWO/TWIMN )
         YS = R*SQRT( YO - YN )
         YT = TWOPI*X(P+MN+I)
         Y(TWIMN+2) = YS*SIN(YT)
         Y(TWIMN+1) = YS*COS(YT)
         YO = YN
      END DO
      YS = R*SQRT(YO)
      IF ( MN .EQ. 1 ) THEN
         Y(1) = YS*( 2*X(1) - 1 )
         YS = 2*SQRT( X(1)*( 1 - X(1) ) )*YS
      END IF
      YT = TWOPI*X(P+MN)
      Y(2+MN) = YS*SIN(YT)
      Y(1+MN) = YS*COS(YT)
      CALL FUNSUB( N, Y, NF, FS )
      END
      
      REAL(kind=dp) FUNCTION TCHINV( N, P )
             use mod_kinds, only : i4,dp
            
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: TCHINV
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: TCHINV
#endif              
!*     
!*                  TCHINV  
!*     P =  1 - K  I     exp(-t*t/2) t**(N-1) dt, for N > 1.
!*               N  0
!*     
      INTEGER(kind=i4) I, N, NO
      REAL(kind=dp) P, PO, TWO, R, RO, RP, KN, PHINV, TCHNVC, TO
      PARAMETER ( RP = .79788456080286535588D0, TWO = 2 )
!*                 RP = SQRT(2/PI)
      SAVE NO, KN, PO, RO
#if defined (_OPENMP)
!$OMP THREADPRIVATE(NO,KN,PO,RO)
#endif
      DATA NO, PO / 0, TWO /
      IF ( P .EQ. PO .AND. N .EQ. NO ) THEN
         R = RO
      ELSE
         IF ( N .LE. 1 ) THEN
            R = -PHINV( P/2 )
         ELSE IF ( P .LT. 1 ) THEN
            IF ( N .EQ. 2 ) THEN            
               R = SQRT( -2*LOG(P) )
            ELSE
               IF ( N .NE. NO ) THEN
                  NO = N            
                  KN = 1
                  DO I = N-2, 2, -2
                     KN = KN/I
                  END DO
                  IF ( MOD( N, 2 ) .EQ. 1 ) THEN
                     KN = KN*RP
                  END IF
               END IF
               IF ( N .GE. -5*LOG(1-P)/4 ) THEN
                  R = TWO/( 9*N )
                  R = N*( -PHINV(P)*SQRT(R) + 1 - R )**3
                  IF ( R .GT. 2*N+6 ) THEN
                     R = 2*LOG( KN/P ) + ( N - 2 )*LOG(R)
                  END IF
               ELSE
                  R = ( (1-P)*N/KN )**( TWO/N )
               END IF
               R = SQRT(R)
               RO = R
               R = TCHNVC( KN, N, P, R )
               IF ( ABS(R-RO) .GT. 1D-6 ) THEN 
                  RO = R
                  R = TCHNVC( KN, N, P, R )
                  IF ( ABS(R-RO) .GT. 1D-6 ) R = TCHNVC( KN, N, P, R )
               END IF
            END IF
         ELSE
            R = 0
         END IF
         RO = R
         PO = P
      END IF
      TCHINV = R
      END

      REAL(kind=dp) FUNCTION TCHNVC( KN, N, P, R )
               use mod_kinds, only : i4,dp
            
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: TCHNVC
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: TCHNVC
#endif                
!*     
!*     Third order correction to R for TCHINV
!*     
      INTEGER(kind=i4) N, I
      REAL(kind=dp) P, R, KN, DF, RR, RP, PF, PHI
      PARAMETER ( RP = 0.79788456080286535588D0 )
      RR = R*R
      PF = 1
      DO I = N - 2, 2, -2
         PF = 1 + RR*PF/I
      END DO
      IF ( MOD( N, 2 ) .EQ. 0 ) THEN
         DF = ( P             - EXP( LOG(      PF ) - RR/2 ) )
      ELSE
         DF = ( P - 2*PHI(-R) - EXP( LOG( RP*R*PF ) - RR/2 ) )
      ENDIF
      TCHNVC = R - DF/( KN*EXP((N-1)*LOG(R)-RR/2) + DF*( R-(N-1)/R )/2 )
      END
!*
      SUBROUTINE VCRDSR( M, NF, MAXPTS, FUNCTN, ABSREQ, RELREQ,                &
                        IR, FINEST, ESTERR, NCLS, INFORM, WORK )
                 use mod_kinds, only : i4,dp
            
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: VCRDSR
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: VCRDSR
#endif         
!*
!*    Crude Monte Carlo Integration Subroutine Driver
!*
      EXTERNAL FUNCTN
      INTEGER(kind=i4) M, NF, MAXPTS, IR, NCLS, INFORM, I
      REAL(kind=dp)  FINEST(*), ESTERR(*), WORK(*),                            &
          ABSREQ, RELREQ, WEIGHT, DIFFER 
!*
      IF ( IR .NE. 0 ) THEN
         DO I = 1, NF
            WORK(I) = FINEST(I)
            WORK(NF+I) = ESTERR(I) 
         END DO
      END IF
      CALL VCRDRS( M, NF, MAXPTS, FUNCTN, FINEST, ESTERR, NCLS,               &
                  WORK(2*NF+1), WORK(3*NF+1), WORK(4*NF+1) ) 
      INFORM = 0
      DO I = 1, NF
         ESTERR(I) = SQRT( ESTERR(I) )
         IF ( IR .NE. 0 ) THEN
            IF ( WORK(NF+I) .GT. 0 ) THEN 
               WEIGHT = 1/( 1 + ( ESTERR(I)/WORK(NF+I) )**2 )
            ELSE IF ( ESTERR(I) .GT. 0 ) THEN
               WEIGHT = 0
            ELSE
               WEIGHT = 1 
            END IF
            DIFFER = WEIGHT*( FINEST(I) - WORK(I) )
            FINEST(I) = WORK(I) + DIFFER
            ESTERR(I) = SQRT(WEIGHT)*ESTERR(I)
         END IF
         IF ( ESTERR(I) .GT. MAX( ABSREQ, RELREQ*ABS(FINEST(I)) ) )            &
             INFORM = 1
      END DO
      END

      SUBROUTINE VCRDRS( M, NF, MAXPTS, FUNCTN,                                &
                        FINEST, ESTERR, NCLS, FUNS, FUNTMP, X )
                      use mod_kinds, only : i4,dp
            
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: VCRDRS
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: VCRDRS
#endif         
!*
!*    Crude Monte Carlo Integration Subroutine 
!*
      EXTERNAL FUNCTN
      INTEGER(kind=i4) M, N, NF, MAXPTS, NCLS, INFORM, I, K
      REAL(kind=dp)                                                            &
         FINEST(*), ESTERR(*), FUNTMP(*), FUNS(*), X(*)
      REAL(kind=dp) UNIRAN, FINVAL, FINDIF
      DO I = 1,NF
         FINEST(I) = 0
         ESTERR(I) = 0
      END DO
!*
!*     Uses simple antithetic variates
!*
      NCLS = MAXPTS/2
      DO N = 1,NCLS
         DO K = 1,M
            X(K) = UNIRAN()
         END DO
         CALL FUNTRN( FUNCTN, M, X, NF, FUNS )
         DO K = 1,M
            X(K) = 1 - X(K)
          END DO 
         CALL FUNTRN( FUNCTN, M, X, NF, FUNTMP )
         DO I = 1,NF
            FINVAL = ( FUNS(I) + FUNTMP(I) )/2
            FINDIF = ( FINVAL - FINEST(I) )/N
            ESTERR(I) = ( N - 2 )*ESTERR(I)/N + FINDIF**2 
            FINEST(I) = FINEST(I) + FINDIF
         END DO
      END DO
      NCLS = 2*NCLS
      END

      SUBROUTINE VKRBVR( NDIM, NF, MINVLS, MAXVLS, FUNSUB,                  &
          ABSEPS, RELEPS, IR, FINEST, ABSERR, INTVLS, INFORM, WORK )
                  use mod_kinds, only : i4,dp
            
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: VKRBVR
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: VKRBVR
#endif         
#if 0 
*
*  Automatic Multidimensional Integration Subroutine
*               
*         AUTHOR: Alan Genz
*                 Department of Mathematics
*                 Washington State University
*                 Pulman, WA 99164-3113
*
*         Last Change: 3/15/97
*
*  VKROBV computes an approximation to the integral
*
*    1    1     1
*   I    I ... I         F(X) F(X) ... F(X)   dx(NDIM)...dx(2)dx(1)
*    0    0     0            1    2        NF
*
*
*  VKROBV uses randomized Korobov rules. The primary references are
*  "Randomization of Number Theoretic Methods for Multiple Integration"
*   R. Cranley and T.N.L. Patterson, SIAM J Numer Anal, 13, pp. 904-14,
*  and 
*   "Optimal Parameters for Multidimensional Integration", 
*    P. Keast, SIAM J Numer Anal, 10, pp. 831-838.
*   
***************  Parameters for KROBOV  *******************************
****** Input parameters
*  NDIM    Number of variables, must exceed 1, but not exceed 40.
*  MINVLS  Integer minimum number of function evaluations allowed.
*          MINVLS must not exceed MAXVLS.
*  MAXVLS  Integer maximum number of function evaluations allowed.
*  NF      Integer number of functions to be integrated 
*  FUNSUB  EXTERNALly declared user defined subroutine for integrands.
*          It must have parameters (NDIM,Z,NF,F), where Z and F are real 
*          arrays with respective lengths NDIM and NF.
*  ABSEPS  Required absolute accuracy.
*  RELEPS  Required relative accuracy.
*  IR      Integer restart parameter. If IR < > 0 then the routine
*          assumes a previous call of VKROBV has been made with 
*          the same integrand and continues that calculation.
*  WORK    Real array of working storage, with length at least 2*NF 
****** Output parameters
*  INTVLS  Actual number of function evaluations used by VKROBV.
*  ABSERR  Real array of length NF; 
*            the estimated absolute accuracys for FINEST.
*  FINEST  Real array of length NF; 
*            the estimated values of the integrals.
*  INFORM   IFAIL = 0 for normal exit, when for each I
*                     ABSERR(I) <= MAX(ABSEPS, RELEPS*ABS(FINEST(I)))
*                  and 
*                     INTVLS <= MAXCLS.
*          INFORM = 1 If MAXVLS was too small for KROBOV to obtain the
c                  required accuracy. Then KROBOV returns values for
c                  FINEST with estimated absolute accuracies ABSERR.
************************************************************************
#endif
      EXTERNAL FUNSUB
      REAL(kind=dp) ABSERR(*), FINEST(*), WORK(*)
      INTEGER(kind=i4) IR, PLIM, NLIM, NF, NDIM, MINVLS, MAXVLS, NP,        &     
          INFORM, NSAMP, I, J, INTVLS, MINSMP, SAMPLS
      REAL(kind=dp) ABSEPS, RELEPS, FINDIF
      PARAMETER ( PLIM = 18, NLIM = 40, MINSMP = 8 )
      INTEGER(kind=i4) C(PLIM,NLIM), P(PLIM), V(NLIM)
      REAL(kind=dp) VK(NLIM), ONE, X(NLIM), ALPHA(NLIM)
      PARAMETER ( ONE = 1 )
      SAVE P, C, NSAMP, NP
#if defined(_OPENMP)
!$OMP THREADPRIVATE(P,C,NSAMP,NP)
#endif
      DATA P( 1),(C( 1,I), I = 1,39) /  173,                                &
         73,   34,   57,    9,   12,    2,   16,   30,   30,   42,         &
         70,   86,    2,   53,   53,   30,   30,    5,   42,   42,         &
         70,   42,   53,   42,   42,   53,   42,   53,   53,    2,         &
         86,    2,    2,    2,    2,    2,    2,    2,    2/
      DATA P( 2),(C( 2,I), I = 1,39) /  263,                                &
        111,  106,   51,   36,   48,  110,    2,    2,    2,    2,         &
         70,   70,   48,    2,    2,   70,  124,  124,   70,   48,         &
         48,   48,   48,  108,   65,   48,   48,   70,    2,   20,         &
          2,    2,    2,    2,    2,    2,    2,    2,    2/
      DATA P( 3),(C( 3,I), I = 1,39) /  397,                                &
        163,  168,  164,  133,   23,   64,    2,    2,  106,   80,         &
         80,  126,   16,   16,   16,   16,   16,   16,  107,   80,         &
          2,    2,    2,   32,   32,   32,   31,   64,   31,   31,         &
          4,    4,    4,  126,   16,   16,   16,   16,   16/
      DATA P( 4),(C( 4,I), I = 1,39) /  593,                                &
        229,   40,  268,  240,   31,  119,   71,  296,  130,  199,         &
        149,  149,  149,  149,  149,   31,  130,  149,  149,   79,         &
        119,  119,   31,   82,  130,  122,  122,  122,  122,    2,         &
        130,  130,  130,  130,    2,    2,   82,   82,    2/
      DATA P( 5),(C( 5,I), I = 1,39) /  887,                                &
        192,  424,   55,  221,  179,  242,  242,    2,    2,   11,         &
         11,   11,  394,  394,  439,  394,  394,  394,  394,  439,         &
        394,  394,  394,  101,  378,  394,  394,  394,  394,  394,         &
        202,  279,  394,  279,    2,    2,    2,    2,    2/
      DATA P( 6),(C( 6,I), I = 1,39) / 1327,                                &
        513,  195,  599,  661,  443,  632,  251,  603,  663,    2,         &
        425,  425,  603,  425,  425,  525,  412,  412,  412,  412,         &
        412,   82,   82,   82,  603,  580,  580,  444,   82,   82,         &
        276,  601,  276,  276,  276,  276,  112,  112,  112/
      DATA P( 7),(C( 7,I), I = 1,39) / 1997,                                &
        839,  146,  860,  183,  121,   11,   11,  793,  998,    2,         &
          2,  110,  110,  236,  110,  236,  147,  147,  110,  190,         &
        147,  147,  147,  147,  147,  147,  236,  110,  110,  147,         &
        110,  110,  632,  147,  147,  148,    2,  147,  147/
      DATA P( 8),(C( 8,I), I = 1,39) / 2999,                                &
       1148, 1406, 1192, 1094, 1290,  632,  341,  785,  393, 1499,         &
          2,  798,  808,  798,  918,  393,  924,  924,    2,    2,         &
          2, 1499,    2, 1016,  798,  798,  798,  808,  270, 1344,         &
        798,  798,  798,  798,  798,  798,  798,    2,    2/
      DATA P( 9),(C( 9,I), I = 1,39) / 4493,                                &
       1360,  383,  842, 2157,   30,  959,    3,  717, 1107,    2,         &
          2,    2,  836,  836, 1134,  836,  836,  426,  898,  898,         &
         65,  836,  836,  836,  836,  216,  104,  300,  836, 1022,         &
       1022, 1022, 1022, 1420, 1478, 1478, 1478,  283, 2246/
      DATA P(10),(C(10,I), I = 1,39) / 6737,                                &
       2602, 2818, 3240, 2528, 2260, 3141, 2857, 1484, 2113, 2265,         &
          2,    2, 2207, 2207, 2207,  542,  132,  934,  378,  378,         &
       2099,  934,  225,  225,  225,  169,  378, 2257, 2257, 2257,         &
       2257,  934, 2576,  934,  934,  934,  934,  934, 2257/
      DATA P(11),(C(11,I), I = 1,39) / 10111,                               &
       3071, 3114, 1170, 3432, 2726, 1098, 3371,  185,    4, 3143,         &
       5055,    2,    2,    2,    2,    2,    2,  334, 1254, 4146,         &
        617, 1879,    2,    2, 1146,  475, 4725,    2,    2,  475,         &
        475,  475,  475,  475,  638,  638,  638,    2, 3107/
      DATA P(12),(C(12,I), I = 1,39) / 15161,                               &
       6280, 6716, 7191, 2574, 3970,  687, 2990, 4054, 7092, 6207,         &
       3821,    2, 3821, 3821, 3821, 3821, 3821, 3821,  896, 2077,         &
       3536, 3104, 3320, 4562, 3320, 2900, 3956, 3956, 2954, 3784,         &
       3956, 2210, 3784, 1547, 1547, 1547, 1547, 1547, 1547/
      DATA P(13),(C(13,I), I = 1,39) / 22751,                               &
       9438,10221, 6766, 6496, 9448,  629, 2936, 4929, 6234, 9048,         &
       6339, 6339,    2,    2,    2, 6725, 6725, 9300, 9300, 6725,         &
       6725, 9300,10128, 6990,  783, 6990,  435,  435,  435,  435,         &
        435, 6720, 8772, 6720, 6720, 6720, 8772, 2335, 2335/
      DATA P(14), (C(14,I), I=1,39)/  34127,                                &
        7002,  6450,  5206, 11453,  4186,  7777,  5806, 14858, 10620,      &
        5497,   938,  8883, 10669,  2444,   876, 13293,  3725,   213,      &
        3348,  4998,     2,     2,     2,     2,     2,     2,     2,      &
           2,     2,     2,     2,     2,     2,     2,  9777,     2,      &
           2,     2,     2/
      DATA P(15), (C(15,I), I=1,39)/  51193,                                &
        6964, 20979,  9862, 15012, 13588,   204,  4339, 20078,   551,      &
         957,  9428,  8919,  8919,  8919,  8919, 17428,  2075, 17428,      &
        1315,  9428, 12635, 17428, 17428, 17428, 23061, 21994, 21994,      &
       21994,  8406,  8392,  8392,  8392,  8392,  8392,  8392, 23956,      &
       23061, 15077,  1027/
      DATA P(16), (C(16,I), I=1,39)/  76801,                                &
       26053, 12197, 25526, 25526,  1023, 20586, 26053, 26053,  1688,      &
        2924,  6925,  6925, 20629, 27112, 22711,  5298,  5298, 22711,      &
       22711,  2993,  1871,  8093,  8093,  1871,   822, 15206, 15206,      &
        6326,  6326, 15206,  6326,  6326,  6326,  6326, 24628, 23450,      &
        6326,  6326,  6326/
      DATA P(17), (C(17,I), I=1,39)/ 115183,                                &
       15990, 14064, 18027,  9380, 17512, 14711, 12807,  5241,  8528,      &
        8528, 12412,  9204,  9198,  9198,  9198,  9198,  9198,  7539,      &
        9198,  9198,  9198, 18397, 17518, 17518, 18397,  7564,  9198,      &
        9198,  9198,  9198, 11737, 11737, 11737, 11737, 11737,  7564,      &
        7564,  7564,   580/
      DATA P(18), (C(18,I), I=1,39)/ 259151,                                &
        9841,  4842,  1518,  5362,  3899,  4842,   186,   186,   186,      &
        1518,  6513,  4842,  6226,  6226,  4842,  3523,  3523,  3398,      &
        3398,  3398,  3398,  3398,  3398,  4842,  4842,  6513,   120,      &
         120,   120,  5341,  5341,  4931,   186,   186,   186,   186,      &
        1518,   186,   186/
      INTVLS = 0
      IF ( IR .EQ. 0 ) THEN
         NSAMP = MINSMP 
         DO I = 1,PLIM
            NP = I
            IF ( MINVLS .LT. NSAMP*P(I) ) GO TO 10
         END DO
         NSAMP = MAX( MINSMP, MINVLS/P(NP) )
      ENDIF      
 10   VK(1) = ONE/P(NP)
      V(1) = 1
      DO I = 2,NDIM
         V(I) = MOD( C(NP,NDIM-1)*V(I-1), P(NP) )
         VK(I) = V(I)*VK(1)
      END DO
      SAMPLS = NSAMP/2
      DO J = 1,NF
         FINEST(J) = 0
         ABSERR(J) = 0
      END DO
      DO I = 1,SAMPLS
         CALL VKRSMR( NDIM, P(NP), VK, NF, FUNSUB,                        &
             WORK, WORK(NF+1), X, ALPHA )
         DO J = 1,NF
            FINDIF = ( WORK(J) - FINEST(J) )/I
            FINEST(J) = FINEST(J) + FINDIF
            ABSERR(J) = ( I - 2 )*ABSERR(J)/I + FINDIF**2 
         END DO
      END DO
      INTVLS = INTVLS + 2*SAMPLS*P(NP)
      INFORM = 0
      DO J = 1,NF
         ABSERR(J) = SQRT( ABSERR(J) )
         IF ( INFORM .EQ. 0 )THEN 
            IF ( ABSERR(J) .GT. MAX(ABS(FINEST(J))*RELEPS,ABSEPS) ) THEN
               INFORM = 1
               IF ( NP .LT. PLIM ) THEN
                  NP = NP + 1
               ELSE
                  NSAMP = MAX( MINSMP,                                    &
                              MIN( 3*NSAMP/2, (MAXVLS-INTVLS)/P(NP) ) ) 
               END IF
            END IF
         END IF
      END DO
      IF ( INFORM .EQ. 1 .AND. INTVLS+NSAMP*P(NP) .LE. MAXVLS ) GO TO 10
      END

      SUBROUTINE VKRSMR( NDIM, NPTS, VK, NF, FUNSUB,                       &
                        SUMFUN, FUNVAL, ALPHA, X )
                 use mod_kinds, only : i4,dp
            
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: VKRSMR
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: VKRSMR
#endif                                 
      EXTERNAL FUNSUB
      INTEGER(kind=i4) NDIM, NPTS, K, J, I, NF
      REAL(kind=dp) VK(*), ONE, UNIRAN, XT 
      PARAMETER ( ONE = 1 )
      REAL(kind=dp) ALPHA(*), X(*), SUMFUN(*), FUNVAL(*)
      DO J = 1,NDIM
         ALPHA(J) = UNIRAN()
      END DO
      DO I = 1,NF
         SUMFUN(I) = 0
      END DO
      DO K = 1,NPTS
         DO J = 1,NDIM
            X(J) = MOD( ALPHA(J) + VK(J)*K, ONE )
         END DO
         CALL FUNTRN( FUNSUB, NDIM, X, NF, FUNVAL )
         DO I = 1, NF
            SUMFUN(I) = SUMFUN(I) + FUNVAL(I)/2
         END DO
         DO J = 1,NDIM
            X(J) = 1 - X(J)
         END DO
         CALL FUNTRN( FUNSUB, NDIM, X, NF, FUNVAL )
         DO I = 1, NF
            SUMFUN(I) = SUMFUN(I) + FUNVAL(I)/2
         END DO
      END DO
      DO I = 1, NF
         SUMFUN(I) = SUMFUN(I)/NPTS
      END DO
      END

