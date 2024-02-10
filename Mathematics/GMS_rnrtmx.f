#if 0
*********************************************************
* Orignal BAYESPACK source code (author: Alan Genz)
*********************************************************
*
*    This file contains RNRTMX and supporting function and subroutines.
*    It is self-contained except for calls to a uniform random number
*    generator UNIRAN, a Normal random number generator NORRAN and a
*    Normal distribution function PHI.
*
#endif
      SUBROUTINE RNRTMX( M, NF, MXVALS, F, EPSABS, EPSREL, RESTAR, &
                        RESULT, ABSERR, FNVALS, INFORM, NW, WK )  
          use mode_kinds, only : i4,dp
#if 0                            
****BEGIN PROLOGUE RNRTMX
****AUTHOR
*            Alan Genz 
*            Department of Mathematics 
*            Washington State University 
*            Pullman, WA 99164-3113, USA
*            Email: alangenz@wsu.edu
****KEYWORDS adaptive multidimensional integrator,
*            n-dimensional region (-infin, +infin)^n 
****PURPOSE  The routine calculates an approximation to a given
*            vector of definite integrals
*
*
*      infin     infin 
*     I    ...  I       (F ,F ,...,F      ) DX(M)...DX(2)DX(1),
*     -infin    -infin    1  2      NF
*
*        where F = F (X ,X ,...,X    ), I = 1,2,...,NF,
*              I   I  1  2      M
*
*        hopefully satisfying for each component of I 
*
*            ABS( I(K) - RESULT(K) ) .LE. MAX( EPSABS, EPSREL*ABS(I(K)) )
*
****DESCRIPTION Computation of integrals over infinite regions.
*
*   ON ENTRY
*
*     M      Integer, number of variables, must be >= 1. 
*     NF     Integer, number of components of the integral.
*     MXVALS Integer, maximum number of function evaluations.
*     F      Externally declared subroutine for computing
*            all components of the integrand at the given
*            evaluation point.
*            It must have parameters ( M, X, NF, F )
*            Input parameters:
*              M  Integer that defines the dimension of the integral.
*              X  Real array of length M, the evaluation point.
*              NF Integer, number of components of I.
*            Output parameter:
*              F Real array of length NF, components of the integrand.
*
*     EPSABS Real, requested absolute accuracy.
*     EPSREL Real, requested relative accuracy.
*     RESTAR Integer.
*            If RESTAR = 0, this is the first attempt to compute
*              the integral(s).
*            If RESTAR = 1, then a previous calculation is restarted. In 
*              this case, the only parameters that may be changed (with 
*              respect to the previous call of the subroutine) are  
*              MXVALS, EPSABS, EPSREL and RESTAR.
*
*     NW     Integer, length of array WK, NW must be >= 6*NF + M*(M+3).
*            It is recommended that NW be >= 60*NF + M*(M+3).
*     WK     Real array of working storage.
*
*   ON RETURN
*
*     RESULT Real array of approximations components of the integral.
*     ABSERR Real array of absolute accuracy estimates. 
*     FNVALS  Integer, number of function evaluations used.
*     INFORM Integer.
*              INFORM = 0 for normal exit, when 
*                ABSERR(K) <= MAX( EPSABS, ABS(RESULT(K))*EPSREL ) for
*                all K, 0 < K <= NF, with <= MXVALS function values. 
*              INFORM = 1 if MXVALS was too small to obtain the required 
*                accuracy. In this case values of RESULT are returned
*                with estimated accuracies ABSERR.
*              INFORM = 2 if MXVALS was too small to obtain the required 
*                accuracy, and the efficiency of the computation was 
*                limited by the size of NW. In this case values of 
*                RESULT are returned with estimated accuracies ABSERR.
*              INFORM = 3 if M < 1, or NW or MXVALS too small.
*                All values of RESULT and ABSERR are all set = 1.
*     WK     Real array of length NW of working storage, containing 
*            information that will be used in subsequent calls 
*            of the subroutine (with RESTAR = 1). 
*
****ROUTINES CALLED 
****END PROLOGUE RNRTMX
*
*   Global variables.
*
#endif
      EXTERNAL F
      INTEGER(kind=i4) :: M, NF, MXVALS, FNVALS, INFORM, RESTAR, NW
      REAL(kind=dp) :: EPSABS, EPSREL
      REAL(kind=dp) RESULT(*), ABSERR(*), WK(*)
!*
!*   Local variables.
!*
      REAL(kind=dp) :: ESPH
      INTEGER(kind=i4) :: DGMN, NK, SS, SUBS, SBMX, I, I1,I2,I3,I4,I5,I6,I7, MNVALS
      NK =   MIN(  7, 3 + 20/M )
      SS =   MIN( 10, 5 + 20/M )
      DGMN = MIN(  3, 1 + 20/M )
      IF ( M .LE. 20 ) THEN
         MNVALS = SS*( 4*NK + 3 ) *( M + 1 )
      ELSE
         MNVALS = SS*( 4*NK + 3 + M )
      END IF
!*
!*     Determine work array lengths and call main subroutine.
!*
      SBMX = ( NW - M*( M + 3 ) - 6*NF )/( 2*NF + 5 )
      I1 = 1 + M*( M + 3 ) + 6*NF
      I2 = I1 + SBMX*NF
      I3 = I2 + SBMX*NF
      I4 = I3 + SBMX
      I5 = I4 + SBMX
      I6 = I5 + SBMX
      I7 = I6 + SBMX
      IF ( SBMX .LE. 0 .OR. M .LT. 1 .OR. MXVALS .LT. 3*( M + 1 ) ) THEN
         DO I = 1,NF
            ABSERR(I) = 1
            RESULT(I) = 1
         END DO
         INFORM = 3
         FNVALS = 0
      ELSE IF ( SBMX .LT. 2 .OR. MXVALS .LT. MNVALS ) THEN
         CALL TAILNT( 0D0, M, NF, F, MXVALS/( M + 1 ), RESTAR,              &
                     RESULT, ABSERR, ESPH, FNVALS, WK, WK(M*M+M+1) ) 
         INFORM = 0
         DO I = 1,NF
            IF ( ABSERR(I) .GT.                                             &
                MAX( EPSABS, EPSREL*ABS( RESULT(I) ) ) ) INFORM = 2
            WK(I1-1+I) = RESULT(I)
            WK(I2-1+I) = ABSERR(I)
         END DO
         WK(I3) = 0
         WK(I6) = ESPH
         WK(I7) = ESPH
         SUBS = 1
      ELSE
         CALL ADONRS( M, NF, F, MXVALS, EPSABS, EPSREL, DGMN, NK, SS,       &
              RESTAR, RESULT, ABSERR, FNVALS, INFORM, WK, SUBS, SBMX,       &
              WK(I1), WK(I2), WK(I3), WK(I4), WK(I5), WK(I6), WK(I7) )
      END IF
!*
!****END RNRTMX
!*
      END
      SUBROUTINE ADONRS( M, NF, F, MXVALS, AB, RL, DGMN, NK, SS,            &
                         RESTAR, RESULT, ABSERR, FNVALS, INFORM, WK,        &
                         SUBS, SBMX, S, E, A, B, DGS, ERAD, ESPH ) 
              use mod_kinds, only : i4,dp 
#if 0  
*
*     Adaptive Mixed Stochastic Spherical Radial Rule, for
*
*          INF                     
*         I   r**(M-1) I G(rZ) dZ dr.
*          0            U         
*
*     U is the surface of unit M-sphere, Z is an M-vector and
*     G is an NF-vector valued function.
*       In this subroutine, F is a subroutine with calling sequence:
*               CALL F( M, X, NF, G ).
* 
*
#endif
      EXTERNAL F
      INTEGER(kind=i4) :: M, NF, MXVALS, RESTAR, SBMX, FNVALS, INFORM 
      INTEGER(kind=i4) NS(0:6), I, SUBS, IP, J, DR, NK, DGMN, DEG, DGMX
      INTEGER(kind=i4) :: SS, SN, TLVALS, I1, I2, I3, I4
      REAL(kind=dp) AB, RL, RESULT(*), ABSERR(*), WK(*)
      REAL(kind=dp) S(NF,*), E(NF,*), A(*), B(*), DGS(*)
      REAL(kind=dp) ERAD(*), ESPH(*), ERRMX, ERRNX, RF, ONE, RHO
      PARAMETER ( DGMX = 6, RF = 10, ONE = 1 ) 
      RHO = MAX( ONE, SQRT( M - ONE ) )
      DR = 2*NK + 1
      NS(0) = DR*SS
      NS(1) = DR*SS*2
      NS(2) = DR*SS*( M + 1 )
      NS(3) = DR*SS*2*( M + 1 )      
      IF ( M .EQ. 1 ) THEN
         DO I = 1, DGMX
            NS(I) = 2*DR
         END DO
      END IF
      IF ( M .EQ. 2 ) NS(4) = DR*SS*6
      IF ( M .GE. 3 ) NS(4) = DR*SS*( M + 4 )*( M + 1 )/2
      IF ( M .EQ. 2 ) NS(5) = DR*SS*6
      IF ( M .EQ. 3 ) NS(5) = DR*SS*14
      IF ( M .GT. 3 ) NS(5) = DR*SS*( M + 2 )*( M + 1 )
      IF ( M .EQ. 2 ) NS(6) = DR*SS*12
      IF ( M .EQ. 3 ) NS(6) = DR*SS*38
      IF ( M .EQ. 4 ) NS(6) = DR*SS*70
      IF ( M .EQ. 5 ) NS(6) = DR*SS*122
      IF ( M .GT. 5 ) NS(6) = DR*SS*( M*M +8*M + 6 )*( M + 1 )/3 
      I1 = 1 + M*( M + 1 )
      I2 = I1 + M
      I3 = I2 + NF
      I4 = I3 + NF
      FNVALS = 0
      IF ( RESTAR .EQ. 0 .OR. SUBS .EQ. 0 ) THEN
         A(1) = 3*RHO
         CALL TAILNT( A, M, NF, F, SS, 0, S,E, ESPH, FNVALS, WK,WK(I1) ) 
         ERAD(1) = ESPH(1) 
         DGS(1) = DGMN
         A(2) = 0
         B(2) = A(1)
         CALL RNSPMX( A(2), B(2), NK, DGMN, SS, M, NF, F, 0,                &
                      S(1,2), E(1,2), ERAD(2), ESPH(2),                     &
                      WK, WK(I1), WK(I2), WK(I3), WK(I4) ) 
         DGS(2) = DGMN 
         FNVALS = FNVALS + NS(DGMN)
         SUBS = 2
      END IF
 10   INFORM = 0
      DO I = 1,NF
         RESULT(I) = S(I,1)
         ABSERR(I) = E(I,1)
      END DO
      ERRNX = 0
      IP = 1
      DO J = 2,SUBS
         DO I = 1,NF
            RESULT(I) = RESULT(I) + S(I,J)
            ABSERR(I) = ABSERR(I) + E(I,J) + ERAD(J)
         END DO
         IF ( RF*ERAD(J) + ESPH(J) .GE. RF*ERAD(IP) + ESPH(IP) ) THEN
            ERRNX = ESPH(IP)
            ERRMX = ESPH(J)
            IP = J
         END IF
         ERRNX = MAX ( ERRNX, RF*ERAD(IP) )
      END DO
      DO I = 1,NF
         IF ( ABSERR(I) .GT. MAX( AB, RL*ABS( RESULT(I) ) ) ) INFORM = 1
      END DO
      IF ( INFORM .EQ. 1 ) THEN
         DEG = DGS(IP)
         IF ( RF*ERAD(IP) .GE. ESPH(IP) ) THEN
            IF ( SUBS .LT. SBMX ) THEN
               IF ( IP .EQ. 1 .AND. FNVALS+2*NS(DGMN) .LT. MXVALS ) THEN
!*
!*     Move tail further out
!*
                  SUBS = SUBS + 1
                  IP = SUBS
                  A(IP) = A(1)
                  A(1) = A(1) + RHO
                  CALL TAILNT( A,M,NF,F,SS,0,S,E,ESPH,TLVALS,WK,WK(I1) )
                  FNVALS = FNVALS + TLVALS
                  ERAD(1) = ESPH(1) 
                  B(IP) = A(1)
                  CALL RNSPMX( A(IP), B(IP), NK, DEG, SS, M, NF, F, 0,      &
                               S(1,IP), E(1,IP), ERAD(IP), ESPH(IP),        &
                               WK, WK(I1), WK(I2), WK(I3), WK(I4) ) 
                  DGS(IP) = DGMN
                  FNVALS = FNVALS + NS(DGMN)
                  GO TO 10
               ELSE IF ( FNVALS + 2*NS(DEG) .LE. MXVALS ) THEN
!*     
!*     Split largest error subinterval into two parts.
!*     
                  SUBS = SUBS + 1
                  B(SUBS) = B(IP)
                  B(IP) = ( A(IP) + B(IP) )/2
                  A(SUBS) = B(IP)
                  DO IP = IP, SUBS, SUBS - IP
                     CALL RNSPMX( A(IP),B(IP), NK, DEG, SS, M, NF, F, 0,    &
                                  S(1,IP), E(1,IP), ERAD(IP), ESPH(IP),     &
                                  WK, WK(I1), WK(I2), WK(I3), WK(I4) ) 
                  END DO
                  DGS(SUBS) = DEG
                  FNVALS = FNVALS + 2*NS(DEG)
                  GO TO 10
               END IF
            ELSE
               INFORM = 2
            END IF
         ELSE 
!*     
!*     Increase degree of spherical rule or add more spherical samples.
!*     
            DEG = MIN( DEG + 1, DGMX )
            DO DEG = DEG, DGMN, -1
               IF ( FNVALS + NS(DEG) .LE. MXVALS ) THEN
                  SN = 1
                  IF ( DEG .EQ. DGS(IP) ) THEN
                     SN = MAX( SN, INT( 3*( (ERRMX/ERRNX)**2 - 1 )/2 ) )
                     SN = MIN( 10, SN, ( MXVALS - FNVALS )/NS(DEG) )
                  END IF
                  CALL RNSPMX( A(IP),B(IP), NK, DEG, SN*SS, M, NF, F, 1,    &
                               S(1,IP), E(1,IP), ERAD(IP), ESPH(IP),        &
                               WK, WK(I1), WK(I2), WK(I3), WK(I4) ) 
                  FNVALS = FNVALS + SN*NS(DEG)
                  DGS(IP) = DEG
                  GO TO 10
               END IF
            END DO
         END IF
      END IF
      END
!*
!*
      SUBROUTINE RNSPMX( A,B, NK, DEG, NS, M, NF, F, RS, VALUES,ERRORS,     &
                         ERRRAD, ERRSPH, V, X, FUNS, INTV, WK )
                 use mode_kinds, only : i4,dp
#if 0              
*
*     Mixed Stochastic Spherical Radial Rule, for
*
*             B                  
*         I  I r**(M-1) G(rZ) dr dZ .
*          U  A                   
*
*     U is the surface of unit M-sphere, Z is an M-vector, 
*     A and B are limits for the radial integrals (0 <= A < B) and 
*     G is an NF-vector valued function.
*       In this subroutine, F is a subroutine with calling sequence:
*               CALL F( M, X, NF, G ).
*     NK is a radial rule degree parameter: a 2*NK+1 point Kronrod rule 
*       rule is used for the inner integrals.
*     DEG is a rule degree parameter: a degree 2*DEG-1 stochastic 
*       spherical rule is used with NS sample points.
*     Outputs VALUES and ERRORS are NF-vectors. Output ERRRAD and 
*       ERRSPH are respective radial and spherical error estimates.
*     Work vectors V, X, FUNS, INTV and WK must have lengths at 
*      least M*(M+1), M, NF, NF and 4*NF+M, respectively.
* 
#endif
      EXTERNAL F
      INTEGER(kind=i4) :: NK, DEG, NS, M, NF, RS, MOLD, I, J, K, L, N, NW
      REAL(kind=dp) A, B, VALUES(*), ERRORS(*), ERRRAD, ERRSPH
      REAL(kind=dp) V( M, * ), FUNS(*), INTV(*), X(*), WK(*) 
      REAL(kind=dp) :: WEIGHT, VOLUME, PI, DIFFER, ERRRDI, ERRKRN
      PARAMETER ( PI = 3.14159265358979323844_dp )
      SAVE MOLD, VOLUME
      DATA MOLD/ 0 /
      IF ( M .EQ. 1 ) THEN
         DO I = 1, NF
            VALUES(I) = 0
            ERRORS(I) = 0
         END DO
         ERRRAD = 0
         DO J = -1, 1, 2
            X(1) = J
            CALL KRNRDR( A, B, NK, M, X, NF, F, FUNS, ERRKRN, WK )
            ERRRAD = ERRRAD + ERRKRN
            DO I = 1, NF
               VALUES(I) = VALUES(I) + FUNS(I) 
               ERRORS(I) = ERRORS(I) + WK(I) 
            END DO
         END DO
         ERRSPH = 0
         RETURN
      END IF
      IF ( MOLD .NE. M ) THEN
!*
!*     On first call compute content of N-sphere surface 
!*
         MOLD = M
         VOLUME = 2*PI
         DO I = M-2, 1, -1 
            IF ( MOD( I, 2 ) .EQ. 0 ) THEN
               VOLUME = PI*VOLUME/( M - I )
            ELSE
               VOLUME = 2*VOLUME
            END IF
         END DO
      END IF
      NW = 2*NF + M
      DO I = 1, NF
         IF ( RS .NE. 0 ) THEN
            WK(NW+I) = VALUES(I)
            WK(NW+NF+I) = ERRORS(I)
         END IF
         VALUES(I) = 0
         ERRORS(I) = 0
      END DO
      ERRRAD = 0
!*     
!*     Compute integrand average
!*     
      DO L = 1, NS
         CALL RNSPRL( A,B, NK,DEG, M,NF,F, ERRRDI, V,X,FUNS, INTV, WK )
         ERRRAD = ERRRAD + ( VOLUME*ERRRDI - ERRRAD )/L
         DO I = 1,NF
            DIFFER = ( VOLUME*INTV(I) - VALUES(I) )/L
            VALUES(I) = VALUES(I) + DIFFER
            ERRORS(I) = ( L - 2 )*ERRORS(I)/L + DIFFER**2 
         END DO
      END DO
      ERRSPH = 0
      DO I = 1, NF
         ERRORS(I) = SQRT( ERRORS(I) )
         IF ( RS .NE. 0 ) THEN
            IF ( WK(NW+NF+I) .GT. 0 ) THEN
               WEIGHT = 1/( 1 + ( ERRORS(I)/WK(NW+NF+I) )**2 )
            ELSE IF ( ERRORS(I) .GT. 0 ) THEN
               WEIGHT = 0
            ELSE 
               WEIGHT = 1
            END IF
            VALUES(I) = WK(NW+I) + WEIGHT*( VALUES(I) - WK(NW+I) )
            ERRORS(I) = SQRT(WEIGHT)*ERRORS(I)
         END IF
         ERRSPH = MAX( ERRSPH, ERRORS(I) )
      END DO
      END
      
      SUBROUTINE RNSPRL( A, B, NK, DEG, M, NF, F, ERRRAD, V, X,             &
                         RESR, INTV, WK )
                 use mod_kinds, only : i4,dp
#if 0                         
*
*     Spherical Radial Rule, for
*
*             B                  
*         I  I r**(M-1) G(rZ) dr dZ .
*          U  A                   
*
*     U is the surface of unit M-sphere, Z is an M-vector, 
*     A and B are limits for the radial integrals (0 <= A < B) and 
*     G is an NF-vector valued function.
*       In this subroutine, F is a subroutine with calling sequence:
*               CALL F( M, X, NF, G ).
*     NK is a radial rule degree parameter: a 2*NK+1 point Kronrod rule 
*       rule is used for the inner integrals.
*     DEG is a rule degree parameter with 0 <= DEG <= 6: a degree DEG 
*       stochastic spherical rule is used with NS sample points.
*     Output INTV is an NF-vector. 
*     Output ERRRAD is a radial error estimate.
*     Work vectors V, X, RESR and WK must have lengths at 
*      least M*(M+1), M, NF and 4*NF+M, respectively.
* 
#endif
      EXTERNAL F
      INTEGER(kind=i4) :: NK, DEG, NS, M, NF, RS, I, IS, J, K, L, N, NV
      REAL(kind=dp) A, B, ERRRAD, V( M,* ), RESR(*), INTV(*), X(*)
      REAL(kind=dp) WK(*), ERRKRN, MP, WV, WM, WC, WT, RM, RC, RT
      REAL(kind=dp) :: XN, NORRAN
!*
!*     Determine Weights
!*
      MP = M + 1
      IF ( DEG .EQ. 0 ) THEN
         WV = 1
      ELSE IF ( DEG .EQ. 1 ) THEN
         WV = 0.5D0
      ELSE IF ( DEG .EQ. 2 ) THEN
         WV = 1/MP
      ELSE IF ( DEG .EQ. 3 ) THEN
         WV = 1/( 2*MP )
      ELSE IF ( DEG .EQ. 4 ) THEN
         WM = 4*( M - 1 )**2/( M*( M + 2 )*MP**2 )
         RM = SQRT( 2*( MP - 2 )/M )
      ELSE IF ( DEG .EQ. 5 ) THEN
         WV =   ( 7 - M )*M /( 2*( M + 2 )*MP**2 )
         WM = 2*( M - 1 )**2/( M*( M + 2 )*MP**2 )
         RM = SQRT( 2*( MP - 2 )/M )
         IF ( M .EQ. 2 ) WV = WV + WM
         IF ( M .EQ. 3 ) WM = 2*WM
      ELSE
         WV =   1/( M*MP**3*( M + 2 )*( M + 4 ) )
         WM = 4*( M - 1)**3*( 4 - M )*WV
         WC = 27*( M - 2 )**3*WV/2
         WT = ( 10*M - 6 )**3*WV/36
         WV = M**3*( 1800 - 793*M + 9*M*M )*WV/36
         RM = SQRT( 2*( MP - 2 )/M )
         RC = SQRT( 3*( MP - 3 )/M )
         RT = SQRT( ( 10*MP - 16 )/M )
         IF ( M .EQ. 2 ) WV = WV + WM
         IF ( M .EQ. 2 ) WT = 2*WT
         IF ( M .EQ. 3 ) WV = WV + WC
         IF ( M .EQ. 3 ) WM = 2*WM
         IF ( M .EQ. 4 ) WM = WC
         IF ( M .EQ. 5 ) WC = 2*WC
      ENDIF
      ERRRAD = 0
      NV = 0
!*     
!*     Compute integrand average
!*     
      DO I = 1,NF
         INTV(I) = 0
      END DO
      IF ( DEG .LE. 1 ) THEN
         XN = 0
         DO I = 1, M
            V(I,1) = NORRAN()
            XN = XN + V(I,1)**2
         END DO
         XN = SQRT(XN)
         DO I = 1, M
            V(I,1) = V(I,1)/XN
         END DO
      ELSE
         CALL RANSMP( M, V, X )
      END IF
      DO IS = -1, 1, 2
         IF ( DEG .EQ. 1 .OR. DEG .EQ. 0 .AND. IS .EQ. 1 ) THEN
            DO I = 1, M
               X(I) = IS*V(I,1)
            END DO
            CALL KRNRDR(  A, B, NK, M, X, NF, F, RESR, ERRKRN, WK )
            NV = NV + 1
            ERRRAD = ERRRAD + ( ERRKRN - ERRRAD )/NV
            DO I = 1, NF
               INTV(I) = INTV(I)+ WV*RESR(I)
            END DO
         END IF 
         IF ( ( DEG .EQ. 2 .AND. IS .EQ. 1 ) .OR. DEG .GE. 3 ) THEN
            IF ( DEG .EQ. 4 ) THEN
               IF ( IS .EQ. -1 ) THEN 
                  WV = 1/( 2*MP ) - WM*( M + 2*( M - 3 )/RM**3 )/4 
               ELSE 
                  WV = WV + WM*( M - 3 )/RM**3 
                  IF ( M .EQ. 2 ) WV = 1/( 2*MP )
               END IF
            END IF
            DO K = 1, M+1
               DO I = 1, M
                  X(I) = IS*V(I,K)
               END DO
               CALL KRNRDR( A, B, NK, M, X, NF,F, RESR, ERRKRN, WK )
               NV = NV + 1
               ERRRAD = ERRRAD + ( ERRKRN - ERRRAD )/NV
               DO I = 1, NF
                  INTV(I) = INTV(I) + WV*RESR(I) 
               END DO
            END DO
         END IF
         IF ( ( DEG .GT. 4 .OR. DEG .EQ. 4 .AND. IS .EQ. 1 ) .AND.          &
              (  M  .GT. 3 .OR.  M  .EQ. 3 .AND. IS .EQ. 1 ) ) THEN     
            DO K = 1, M
               DO J = K+1, M+1
                  DO I = 1, M
                     X(I) = IS*( V(I,K) + V(I,J) )/RM
                  END DO
                  CALL KRNRDR( A, B, NK, M, X, NF, F, RESR, ERRKRN, WK )
                  NV = NV + 1
                  ERRRAD = ERRRAD + ( ERRKRN - ERRRAD )/NV
                  DO I = 1, NF
                     INTV(I) = INTV(I) + WM*RESR(I)
                  END DO
               END DO
            END DO
         END IF
         IF ( DEG .GT. 5 ) THEN
            IF ( M .GT. 5 .OR. M .EQ. 5 .AND. IS .EQ. 1 ) THEN
               DO K = 1, M-1
                  DO J = K+1, M
                     DO N = J+1, M+1
                        DO I = 1,M
                           X(I) = IS*( V(I,K) + V(I,J) + V(I,N) )/RC
                        END DO
                        CALL KRNRDR( A,B, NK,M,X,NF,F, RESR,ERRKRN, WK )
                        NV = NV + 1
                        ERRRAD = ERRRAD + ( ERRKRN - ERRRAD )/NV
                        DO I = 1, NF
                           INTV(I) = INTV(I) + WC*RESR(I)
                        END DO
                     END DO
                  END DO
               END DO
            END IF
            IF ( M .GT. 2 .OR. IS .EQ. 1 ) THEN
               DO K = 1, M
                  DO J = K+1, M+1
                     DO I = 1, M
                        X(I) = IS*( 3*V(I,K) + V(I,J) )/RT
                     END DO
                     CALL KRNRDR( A,B, NK, M,X, NF,F, RESR, ERRKRN, WK )
                     NV = NV + 1
                     ERRRAD = ERRRAD + ( ERRKRN - ERRRAD )/NV
                     DO I = 1, NF
                        INTV(I) = INTV(I) + WT*RESR(I)
                     END DO
                     DO I = 1, M
                        X(I) = IS*( V(I,K) + 3*V(I,J) )/RT
                     END DO
                     CALL KRNRDR( A,B, NK, M,X, NF,F, RESR, ERRKRN, WK )
                     NV = NV + 1
                     ERRRAD = ERRRAD + ( ERRKRN - ERRRAD )/NV
                     DO I = 1, NF
                        INTV(I) = INTV(I) + WT*RESR(I)
                     END DO
                  END DO
               END DO
            END IF
         END IF
      END DO
!*
!*
      END
      
      SUBROUTINE KRNRDR( A, B, NK, M, X, NF, F, RESULT, ERROR, WK )
                 use mod_kinds, only : i4,dp
#if 0
*
*     Kronrod Rule, with 2*NK + 1 points for
*
*          B
*         I  |r|**(M-1) G(r*X) dr
*          A
*
*     Input X is an N-vector; G is an NF-vector valued function.
*      In this subroutine, F is a subroutine with calling sequence:
*               CALL F( M, X, NF, G ).
*     Outputs RESULT and ERROR are NF-vectors.
*     Work vector WK must have length at least 2*NF+M.
* 
#endif
      EXTERNAL F
      REAL(kind=dp) A,B, C,H, X(*), RESULT(*), ERROR, WK(*), R
      INTEGER(kind=i4) :: I, K, L, M, NF, NK, MXR
#if 0
*
*           THE ABSCISSAE AND WEIGHTS ARE GIVEN FOR THE INTERVAL (-1,1)
*           BECAUSE OF SYMMETRY ONLY THE POSITIVE ABSCISSE AND THEIR 
*           CORRESPONDING WEIGHTS ARE GIVEN.
*
*           XGK    - ABSCISSAE OF THE (2*NK+1)-POINT KRONROD RULE 
*                    XGK(2), XGK(4), ...  ABSCISSAE OF THE NK-POINT
*                    GAUSS RULE
*                    XGK(1), XGK(3), ...  ABSCISSAE WHICH ARE OPTIMALLY
*                    ADDED TO THE NK-POINT GAUSS RULE
*
*           WGK    - WEIGHTS OF THE (2*NK+1)-POINT KRONROD RULE
*
*           WG     - WEIGHTS OF THE NK-POINT GAUSS RULE
*
*
#endif
      PARAMETER ( MXR = 7 )
      REAL(kind=dp) WG(0:MXR,MXR), WGK(0:MXR,MXR), XGK(0:MXR,MXR) 
      SAVE WG, WGK, XGK
#if defined (_OPENMP)
      !$OMP THREADPRIVATE(WG,WGK,XGK)
#endif
!*  Gauss-Legendre 1-point extension
      DATA  WG(0,1) / 0.20000000000000000000000000000000E+01_dp /
      DATA ( WG(I,1), I = 1,1,2 ) / 1*0D0 /
      DATA XGK(1,1) / 0.77459666924148337703585307995648E+00_dp /
      DATA XGK(0,1) / 0.00000000000000000000000000000000E+00_dp /
      DATA WGK(1,1) / 0.55555555555555555555555555555556E+00_dp /
      DATA WGK(0,1) / 0.88888888888888888888888888888889E+00_dp /
!*  Gauss-Legendre 2-point extension
      DATA  WG(2,2) / 0.10000000000000000000000000000000E+01_dp /
      DATA ( WG(I,2), I = 1,1,2 ) / 1*0D0 /
      DATA XGK(1,2) / 0.92582009977255146156656677658400E+00_dp /
      DATA XGK(2,2) / 0.57735026918962576450914878050196E+00_dp /
      DATA XGK(0,2) / 0.00000000000000000000000000000000E+00_dp /
      DATA WGK(1,2) / 0.19797979797979797979797979797980E+00_dp /
      DATA WGK(2,2) / 0.49090909090909090909090909090909E+00_dp /
      DATA WGK(0,2) / 0.62222222222222222222222222222222E+00_dp /
!*  Gauss-Legendre 3-point extension
      DATA  WG(2,3) / 0.55555555555555555555555555555556E+00_dp /
      DATA  WG(0,3) / 0.88888888888888888888888888888889E+00_dp /
      DATA ( WG(I,3), I = 1,3,2 ) / 2*0D0 /
      DATA XGK(1,3) / 0.96049126870802028342350709262908E+00_dp /
      DATA XGK(2,3) / 0.77459666924148337703585307995648E+00_dp /
      DATA XGK(3,3) / 0.43424374934680255800207150284463E+00_dp /
      DATA XGK(0,3) / 0.00000000000000000000000000000000E+00_dp /
      DATA WGK(1,3) / 0.10465622602646726519382385719207E+00_dp /
      DATA WGK(2,3) / 0.26848808986833344072856928066671E+00_dp /
      DATA WGK(3,3) / 0.40139741477596222290505181861843E+00_dp /
      DATA WGK(0,3) / 0.45091653865847414234511008704557E+00_dp /
!*  Gauss-Legendre 4-point extension
      DATA  WG(2,4) / 0.34785484513745385737306394922200E+00_dp /
      DATA  WG(4,4) / 0.65214515486254614262693605077800E+00_dp /
      DATA ( WG(I,4), I = 1,3,2 ) / 2*0D0 /
      DATA XGK(1,4) / 0.97656025073757311153450535936992E+00_dp /
      DATA XGK(2,4) / 0.86113631159405257522394648889281E+00_dp /
      DATA XGK(3,4) / 0.64028621749630998240468902315749E+00_dp /
      DATA XGK(4,4) / 0.33998104358485626480266575910324E+00_dp /
      DATA XGK(0,4) / 0.00000000000000000000000000000000E+00_dp /
      DATA WGK(1,4) / 0.62977373665473014765492488552819E-01_dp /
      DATA WGK(2,4) / 0.17005360533572272680273885329621E+00_dp /
      DATA WGK(3,4) / 0.26679834045228444803277062841786E+00_dp /
      DATA WGK(4,4) / 0.32694918960145162955845946561732E+00_dp /
      DATA WGK(0,4) / 0.34644298189013636168107712823160E+00_dp /
!*  Gauss-Legendre 5-point extension
      DATA  WG(2,5) / 0.23692688505618908751426404071992E+00_dp /
      DATA  WG(4,5) / 0.47862867049936646804129151483564E+00_dp /
      DATA  WG(0,5) / 0.56888888888888888888888888888889E+00_dp /
      DATA ( WG(I,5), I = 1,5,2 ) / 3*0D0 /
      DATA XGK(1,5) / 0.98408536009484246449617293463614E+00_dp /
      DATA XGK(2,5) / 0.90617984593866399279762687829939E+00_dp /
      DATA XGK(3,5) / 0.75416672657084922044081716694612E+00_dp /
      DATA XGK(4,5) / 0.53846931010568309103631442070021E+00_dp /
      DATA XGK(5,5) / 0.27963041316178319341346652274898E+00_dp /
      DATA XGK(0,5) / 0.00000000000000000000000000000000E+00_dp /
      DATA WGK(1,5) / 0.42582036751081832864509450847670E-01_dp /
      DATA WGK(2,5) / 0.11523331662247339402462684588057E+00_dp /
      DATA WGK(3,5) / 0.18680079655649265746780002687849E+00_dp /
      DATA WGK(4,5) / 0.24104033922864758669994261122326E+00_dp /
      DATA WGK(5,5) / 0.27284980191255892234099326448446E+00_dp /
      DATA WGK(0,5) / 0.28298741785749121320425560137111E+00_dp /
!*  Gauss-Legendre 6-point extension
      DATA  WG(2,6) / 0.17132449237917034504029614217273E+00_dp /
      DATA  WG(4,6) / 0.36076157304813860756983351383772E+00_dp /
      DATA  WG(6,6) / 0.46791393457269104738987034398955E+00_dp /
      DATA ( WG(I,6), I = 1,5,2 ) / 3*0D0 /
      DATA XGK(1,6) / 0.98870320261267885750464595171218E+00_dp /
      DATA XGK(2,6) / 0.93246951420315202781230155449399E+00_dp /
      DATA XGK(3,6) / 0.82137334086502794004564983424395E+00_dp /
      DATA XGK(4,6) / 0.66120938646626451366139959501991E+00_dp /
      DATA XGK(5,6) / 0.46311821247530461215675836401918E+00_dp /
      DATA XGK(6,6) / 0.23861918608319690863050172168071E+00_dp /
      DATA XGK(0,6) / 0.00000000000000000000000000000000E+00_dp /
      DATA WGK(1,6) / 0.30396154119819768851964544676028E-01_dp /
      DATA WGK(2,6) / 0.83694440446906626132845603482411E-01_dp /
      DATA WGK(3,6) / 0.13732060463444692308714987253378E+00_dp /
      DATA WGK(4,6) / 0.18107199432313761518699209331551E+00_dp /
      DATA WGK(5,6) / 0.21320965227196227916289416351689E+00_dp /
      DATA WGK(6,6) / 0.23377086411699440662283572598900E+00_dp /
      DATA WGK(0,6) / 0.24107258017346476191063599297276E+00_dp /
!*  Gauss-Legendre 7-point extension
      DATA  WG(2,7) / 0.12948496616886969327061143267908E+00_dp /
      DATA  WG(4,7) / 0.27970539148927666790146777142378E+00_dp /
      DATA  WG(6,7) / 0.38183005050511894495036977548898E+00_dp /
      DATA  WG(0,7) / 0.41795918367346938775510204081633E+00_dp /
      DATA ( WG(I,7), I = 1,7,2 ) / 4*0D0 /
      DATA XGK(1,7) / 0.99145537112081263920685469752633E+00_dp /
      DATA XGK(2,7) / 0.94910791234275852452618968404785E+00_dp /
      DATA XGK(3,7) / 0.86486442335976907278971278864093E+00_dp /
      DATA XGK(4,7) / 0.74153118559939443986386477328079E+00_dp /
      DATA XGK(5,7) / 0.58608723546769113029414483825873E+00_dp /
      DATA XGK(6,7) / 0.40584515137739716690660641207696E+00_dp /
      DATA XGK(7,7) / 0.20778495500789846760068940377324E+00_dp /
      DATA XGK(0,7) / 0.00000000000000000000000000000000E+00_dp /
      DATA WGK(1,7) / 0.22935322010529224963732008058970E-01_dp /
      DATA WGK(2,7) / 0.63092092629978553290700663189205E-01_dp /
      DATA WGK(3,7) / 0.10479001032225018383987632254152E+00_dp /
      DATA WGK(4,7) / 0.14065325971552591874518959051024E+00_dp /
      DATA WGK(5,7) / 0.16900472663926790282658342659855E+00_dp /
      DATA WGK(6,7) / 0.19035057806478540991325640242101E+00_dp /
      DATA WGK(7,7) / 0.20443294007529889241416199923465E+00_dp /
      DATA WGK(0,7) / 0.20948214108472782801299917489171E+00_dp /
!*
!*           COMPUTE THE (2*NK+1)-POINT KRONROD APPROXIMATION TO
!*           THE INTEGRAL, AND ESTIMATE THE ABSOLUTE ERROR.
!*
      H = ( B - A )/2
      C = ( A + B )/2
      DO I = 1, NF
         RESULT(I) = 0
         WK(NF+I) =  0
      END DO
      DO I = 1, M
         WK(2*NF+I) = C*X(I)
      END DO
      CALL F( M, WK(2*NF+1), NF, WK ) 
      CALL RSCALE( M, C, NF, WK )
      DO I = 1, NF
         RESULT(I) = RESULT(I) + WGK(0,NK)*WK(I)
      END DO
      IF ( MOD( NK, 2 ) .EQ. 1 ) THEN
         DO I = 1, NF
            WK(NF+I) = WK(NF+I) + WG(0,NK)*WK(I)
         END DO
      END IF
      DO K = 1, NK
         DO L = -1, 1, 2
            R = C + L*H*XGK(K,NK) 
            DO I = 1, M
               WK(2*NF+I) = R*X(I)
            END DO
            CALL F( M, WK(2*NF+1), NF, WK ) 
            CALL RSCALE( M, R, NF, WK )
            DO I = 1, NF
               RESULT(I) = RESULT(I) + WGK(K,NK)*WK(I)
            END DO
            IF ( MOD( K, 2 ) .EQ. 0 ) THEN
               DO I = 1, NF
                  WK(NF+I) = WK(NF+I) + WG(K,NK)*WK(I)
               END DO
            END IF
         END DO
      END DO
      ERROR = 0
      DO I = 1, NF
         RESULT(I) = H*RESULT(I)
         WK(I) = ABS( RESULT(I) - H*WK(NF+I) )/NK 
         ERROR = MAX( ERROR, WK(I) )
      END DO
      END
!*
      SUBROUTINE RSCALE( M, R, NF, F )
      INTEGER(kind=i4) I, M, NF
      REAL(kind=dp) R, F(*), RL, FL, FI
      RL = ( M - 1 )*LOG( ABS( R ) )
      DO I = 1, NF
         FI = F(I)
         IF ( ABS( FI ) .GT. 0 ) THEN
            FL = LOG( ABS( FI ) ) 
            F(I) = EXP( RL + FL )
            IF ( FI .LT. 0 ) F(I) = -F(I)
         END IF
      END DO
      END

      SUBROUTINE TAILNT( CUTOFF, M, NF, F, CUTSMP, RS,                      &
                         RESULT, ERROR, ERRMX, TLVALS, V, WK )
                 use mod_kinds, only : i4,dp
#if 0                 
*
*     Tail integral estimator; for integrals
*
*           INF
*          I      r**(M-1) I  G(rZ) dZ dr ,
*           CUTOFF          UM
*                              
*     assuming tail is approximately multivariate Student-t with MU
*     degrees of freedom. UM is the surface of unit M-sphere, Z is 
*     an M-vector and G is an NF-vector valued function.
*      In this subroutine, F is a subroutine with calling sequence:
*               CALL F( M, X, NF, G ).
*     Input CUTSMP is the number of radial samples.
*     Outputs RESULT and ERROR are NF-vectors. Output ERRMX is the
*      is the maximum of ERROR.
*     Work vectors V and WK must have lengths at least M*( M + 1 ) 
*      and 2*NF+M, respectively.
* 
#endif
      EXTERNAL F
      INTEGER(kind=i4) :: I,J,K, M,ML, NU, MOLD, CUTSMP, RS, NF, MP, SAMPLS, TLVALS
      REAL(kind=dp) CUTOFF, RESULT(*), ERROR(*), ERRMX, V(M,*), WK(*)
      REAL(kind=dp) :: DIFFER, PI, SQPI, LSQTPI, R, RC, M0, CON, FI
      REAL(kind=dp) :: TWO, CUTOLD, CTFDRN, FDTAIL, CTCHRN, LCTAIL, WT
      PARAMETER (  ML = 20, TWO = 2, PI  = 3.14159265358979323844_dp )
      PARAMETER ( SQPI = 1.77245385090551602729_dp )
      PARAMETER ( LSQTPI = 0.91893853320467274177_dp )
      SAVE MOLD, CUTOLD, NU, MP, M0
      DATA MOLD, NU, CUTOLD / 0, 3, -1D0 /
      IF ( MOLD .NE. M .OR. CUTOFF .NE. CUTOLD ) THEN
!*
!*     On first call compute the constant 
!*
         MP = M + 1
         MOLD = M
         CON = 1
         IF ( M .LE. ML ) THEN
            DO I = M-2, 0, -2 
               CON = CON*NU/( I + NU )
            END DO
            IF ( MOD( M, 2 ) .EQ. 1 ) THEN
               CON = CON*SQRT( NU/TWO )
               DO I = NU-2, 1, -2
                  CON = I*CON/( I + 1 )
               END DO
               IF ( MOD( NU, 2 ) .EQ. 0 ) THEN
                  CON = 2*CON/SQPI
               ELSE
                  CON = CON*SQPI
               END IF
            END IF 
            CON = LOG( CON*FDTAIL( M, NU, CUTOFF )/MP ) 
         ELSE
            CON = LCTAIL( M, CUTOFF ) - LOG( DBLE(MP) )   
         END IF
         M0 = M*LSQTPI + CON 
      END IF
      DO I = 1,NF
         IF ( RS .NE. 0 ) THEN
            WK(M+2*NF+I) = RESULT(I)
            WK(M+3*NF+I) = ERROR(I)
         END IF
         RESULT(I) = 0
         ERROR(I) = 0
      END DO
      SAMPLS = MAX( 3, CUTSMP )
      DO K = 1, SAMPLS
         IF ( M .LE. ML ) THEN
            R = CTFDRN( M, NU, CUTOFF )
            RC = M0 + ( NU + M )*LOG( 1 + R*R/NU )/2 
         ELSE
            R = CTCHRN( M,     CUTOFF )
            RC = M0 + R*R/2 
         END IF
         CALL RANSMP( M, V, WK )
!*     
!*     Compute integrand average
!*     
         DO I = 1,NF
            WK(M+I) = 0
         END DO
         DO J = 1, MP
            DO I = 1, M
               WK(I) = R*V(I,J)
            END DO
            CALL F( M, WK, NF, WK(M+NF+1) )
            DO I = 1, NF
               WK(M+I) = WK(M+I) + WK(M+NF+I) 
            END DO
         END DO
         DO I = 1,NF
            FI = WK(M+I)
            IF ( ABS( FI ) .GT. 0 ) THEN
               WK(M+I) =  EXP( RC + LOG( ABS( FI ) ) )
               IF ( FI .LT. 0 ) WK(M+I) = - WK(M+I)      
            END IF
            DIFFER = ( WK(M+I) - RESULT(I) )/K
            RESULT(I) = RESULT(I) + DIFFER
            ERROR(I) = ( K - 2 )*ERROR(I)/K + DIFFER*DIFFER 
         END DO
      END DO
      TLVALS = SAMPLS*( M + 1 )
      ERRMX = 0
      DO I = 1,NF
         ERROR(I) = SQRT( ERROR(I) )
         IF ( RS .NE. 0 ) THEN
            IF ( WK(M+3*NF+I) .GT. 0 ) THEN
               WT = 1/( 1 + ( ERROR(I)/WK(M+3*NF+I) )**2 )  
            ELSE IF ( ERROR(I) .GT. 0 ) THEN
               WT = 0
            ELSE
               WT = 1
            END IF
            RESULT(I) = WK(M+2*NF+I) + WT*( RESULT(I) - WK(M+2*NF+I) )
            ERROR(I)= SQRT(WT)*ERROR(I)
         END IF
         ERRMX = MAX ( ERRMX, ERROR(I) )
      END DO
      END
!*
!*
      SUBROUTINE RANSMP( M, V, X )
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: RANSMP
           !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: RANSMP
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: RANSMP
#endif
          use mod_kinds, only : i4,dp
!*
!*     Determine random M-simplex with vertices V and work vector X.
!*
      INTEGER(kind=i4) :: I, J, K, M
      REAL(kind=dp) V( M, * ), X(*), AL, BT, RV, MP, NORRAN
      MP = M + 1
!*
!*     Determine standard unit simplex centered at origin
!*
#if defined (__INTEL_COMPILER)
      !dir$ assume_aligned V:64
#endif
      DO I = 1, M
         DO J = 1, I-1
            V(I,J) = 0
         END DO
         RV = SQRT( MP/( ( M - I + 1 )*M*( M - I + 2 ) ) )
         V(I,I) = ( M - I + 1 )*RV
         DO J = I+1, M+1
            V(I,J) = -RV
         END DO
      END DO
!*     
!*     Replace V with (random orthogonal matrix)*V
!*    
#if defined (__INTEL_COMPILER)
      !dir$ assume_aligned X:64
      !dir$ assume_aligned V:64
#endif 
      DO K = M-1, 1, -1
         AL = 0
         DO I = K, M
            X(I) = NORRAN()
            AL = AL + X(I)**2
         END DO
         AL = -SQRT(AL)
         BT = 1/( AL*( AL + X(K) ) )
         X(K) = X(K) + AL
         DO J = K, M+1
            AL = 0
            DO I = K, M
               AL = AL + X(I)*V(I,J)
            END DO
            AL = BT*AL
            DO I = K, M
               V(I,J) = V(I,J) - X(I)*AL
            END DO
         END DO
      END DO
      END
!*
      REAL(kind=dp) FUNCTION CTFDRN( M, NU, CUT )
            use mod_kinds, only : i4,dp
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: CTFDRN
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: CTFDRN
#endif            
#if 0
*
*     Uses ratio of uniforms to generate random numbers with density
*
*      ( 1 + t**2/NU )**( -(NU+M)/2 )t**(M-1), for M > 0, t > CUT >= 0.
*
*  
#endif   
      REAL(kind=dp) :: A, B, T, U, R, RR, RN, RP, CUT, OLDCUT, UNIRAN
      INTEGER(kind=i4) :: M, NU, OLDM, OLDNU
      SAVE OLDM, OLDNU, OLDCUT, A, B
      DATA OLDM, OLDNU, OLDCUT/ 2*0, -1D0 /
      IF ( M .NE. OLDM .OR. NU .NE. OLDNU .OR. CUT .NE. OLDCUT ) THEN
         OLDM = M
         OLDNU = NU
         OLDCUT = CUT
         RN = NU
         T = MAX( ( M + 1 )*RN/( NU - 1 ), CUT**2 )
         A = EXP( ( M + 1 )*LOG(T)/4 - ( M + NU )*LOG( 1 + T/NU )/4 )
         IF ( M .EQ. 1 .AND. CUT .LE. 0 ) THEN
            B = 1
         ELSE
            T = MAX( ( M - 1 )*RN/( NU + 1 ), CUT**2 )
            B = EXP( ( M - 1 )*LOG(T)/4 - ( M + NU )*LOG( 1 + T/NU )/4 )
         END IF
      END IF
 10   U = B*UNIRAN()
      R = CUT + ( A/U - CUT )*UNIRAN() 
      RR = R*R
      RP = 1 + RR/NU
      IF ( 4*LOG(U) + (NU+1)*LOG(RP) .GT. (M-1)*LOG( RR/RP ) ) GO TO 10
      CTFDRN = R
      END

      REAL(kind=dp) FUNCTION FDTAIL( M, NU, R )
           use mod_kinds, only : i4,dp
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: FDTAIL
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: FDTAIL
#endif           
!*     
!*                   INF  
!*     FDTAIL =  K  I  ( 1 + t**2/NU )**(-(NU+M)/2 ) t**(M-1) dt, for M > 0.
!*                M  R
!*     
      INTEGER(kind=i4) I, M, NU
      REAL(kind=dp) R, RR, RT, PI, PF, STTAIL, TCON
      PARAMETER ( PI = 3.141592653589793_dp)
      IF ( R .GT. 0 ) THEN
         IF ( M .LE. 1 ) THEN
            FDTAIL = 2*STTAIL( NU, R )
         ELSE IF ( M .EQ. 2 ) THEN
            FDTAIL = 1/SQRT( 1 + R*R/NU )**NU
         ELSE 
            RR = R*R/NU
            RT = RR/( 1 + RR )
            PF = 1
            DO I = M - 2, 2, -2
               PF = 1 + PF*RT*( NU + I - 2 )/I
            END DO
            PF = PF*SQRT( RT/RR )**NU
            IF ( MOD( M, 2 ) .EQ. 0 ) THEN
               FDTAIL = PF
            ELSE
               TCON = 1
               IF ( MOD( NU, 2 ) .EQ. 0 ) THEN 
                  TCON = TCON/2
               ELSE
                  TCON = TCON/PI
               END IF
               DO I = NU-2, 1, -2
                  TCON = ( I + 1 )*TCON/I
               END DO
               FDTAIL = 2*( STTAIL( NU, R ) + TCON*SQRT(RT)*PF )
            ENDIF
         ENDIF
      ELSE
         FDTAIL = 1
      ENDIF
      END
      
      REAL(kind=dp) FUNCTION STTAIL( NU, T )
           use mod_kinds, only : i4,dp
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: STTAIL
           !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: STTAIL
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: STTAIL
#endif

!*
!*     Student t Tail Distribution Function
!*
!*                       INF
!*         STTAIL = C   I  ( 1 + y*y/NU )**( -(NU+1)/2 ) dy
!*                   NU  T
!*
      INTEGER(kind=i4) NU, J
      REAL(kind=dp) :: T, CSSTHE, SNTHE, POLYN, TT, TS, RN, PI
      PARAMETER ( PI = 3.141592653589793_dp )
      IF ( NU .EQ. 1 ) THEN
         STTAIL = ( 1 - 2*ATAN(T)/PI )/2
      ELSE IF ( NU .EQ. 2 ) THEN
         STTAIL = ( 1 - T/SQRT( 2 + T*T ))/2
      ELSE 
         TT = T*T
         CSSTHE = 1/( 1 + TT/NU )
         POLYN = 1
         DO J = NU - 2, 2, -2
            POLYN = 1 + ( J - 1 )*CSSTHE*POLYN/J
         END DO
         IF ( MOD( NU, 2 ) .EQ. 1 ) THEN
            RN = NU
            TS = T/SQRT(RN)
            STTAIL = ( 1 - 2*( ATAN(TS) + TS*CSSTHE*POLYN )/PI )/2
         ELSE
            SNTHE = T/SQRT( NU + TT )
            STTAIL = ( 1 - SNTHE*POLYN )/2
         END IF
      ENDIF
      END

      REAL(kind=dp) FUNCTION CTCHRN( M, CUT )
           use mod_kinds, only : i4,dp
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: CTCHRN
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: CTCHRN
#endif           
!*
!*     Uses ratio of uniforms to generate random numbers with density
!*
!*            exp(-t*t/2)t**(M-1), for M > 0, t > CUT >= 0.
!*
!*     
      REAL(kind=dp) A, B, T, U, R, RR, CUT, OLDCUT, UNIRAN
      INTEGER(kind=i4) M, OLDM
      SAVE OLDM, OLDCUT, A, B
#if defined (_OPENMP)
      !$OMP THREADPRIVATE(OLDM,OLDCUT,A,B)
#endif
      DATA OLDM, OLDCUT/ 0, -1.0_dp /
      IF ( M .NE. OLDM .OR. CUT .NE. OLDCUT ) THEN
         OLDM = M
         OLDCUT = CUT
            T = MAX( DBLE( M + 1 ), CUT**2 )
            A = EXP( ( M + 1 )*LOG(T)/4 - T/4 )
         IF ( M .EQ. 1 .AND. CUT .LE. 0 ) THEN
            B = 1
         ELSE
            T = MAX( DBLE( M - 1 ) , CUT**2 )
            B = EXP( ( M - 1 )*LOG(T)/4 - T/4 )
         END IF
      END IF
 10   U = B*UNIRAN()
      R = CUT + ( A/U - CUT )*UNIRAN() 
      RR = R*R
      IF ( 4*LOG(U) + RR .GT. ( M - 1 )*LOG(RR) ) GO TO 10
      CTCHRN = R
      END
!*
      REAL(kind=dp) FUNCTION LCTAIL( M, R )
           use mod_kinds, only : i4,dp
#if defined (__INTEL_COMPILER)
           !DIR$ ATTRIBUTES INLINE :: LCTAIL
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: LCTAIL
#endif
!*     
!*                        INF  
!*     LCTAIL =  LOG( K  I  exp(-t*t/2) t**(M-1) dt ), for M > 1 and R > 0.
!*                     M  R
!*
!*       with K  = 1/( Gamma(M/2) 2^(M/2-1) ).
!*             M
!*     
      INTEGER(kind=i4) I, M
      REAL(kind=dp) R, RR, RP, RL, PF, PHI
      PARAMETER ( RL = -.22579135264472743236_dp )
      PARAMETER ( RP = 0.79788456080286535588_dp )
      IF ( R .GT. 0 ) THEN
         RR = R*R
         PF = 1
         DO I = M - 2, 2, -2
            PF = 1 + RR*PF/I
         END DO
         IF ( MOD( M, 2 ) .EQ. 0 ) THEN
            LCTAIL = - RR/2 + LOG(PF)
         ELSE IF ( R .GT. 37 ) THEN
            LCTAIL = RL - RR/2 + LOG( R*PF )
         ELSE
            LCTAIL = LOG( RP*EXP(-RR/2)*R*PF + 2*PHI(-R) )
         ENDIF
      ELSE
         LCTAIL = 0
      ENDIF
      END
