

      SUBROUTINE DSTSLV (NA,NC,N,A,C,U,IDIM)
!C
!C     *****PARAMETERS:
      INTEGER NA,NC,N,IDIM(N)
      DOUBLE PRECISION A(NA,N),U(NA,N),C(NC,N)
!C
!C     *****LOCAL VARIABLES:
      INTEGER K,KK,DK,KM1,L,LL,DL,LDL,I,IA,J,NSYS,DM,IBLK, &
             NT,IPVT(4),IBM1,IC,JJ,IDM,IDK,IB
      DOUBLE PRECISION T(4,4),P(4),COND,WORK(4),S,EPS,EPSP1
!C
!C     *****SUBROUTINES CALLED:
!C     LINEQ
!C
!C     ------------------------------------------------------------------
!C
!C     ******PURPOSE:
!C     THIS SUBROUTINE SOLVES THE REAL MATRIX EQUATION
!C      T
!C     A *X*A - X = C, WHERE C IS SYMMETRIC, A IS IN UPPER
!C     REAL SCHUR FORM.
!C
!C     *****PARAMETER DESCRIPTION:
!C     ON INPUT:
!C        NA,NC            ROW DIMENSIONS OF THE ARRAYS CONTAINING A
!C                         (AND U), AND C, RESPECTIVELY, AS DECLARED IN
!C                         THE CALLING PROGRAM DIMENSION STATEMENT;
!C
!C        N                ORDER OF THE MATRICES A AND C;
!C
!C        A                N X N (REAL) MATRIX IN UPPER SCHUR FORM;
!C
!C        C                N X N (REAL) SYMMETRIC MATRIX.
!C
!C     ON OUTPUT:
!C
!C        C                N X N (REAL) MATRIX CONTAINING THE
!C                         SOLUTION;
!C
!C        U                N X N (REAL) SCRATCH ARRAY;
!C
!C        IDIM             INTEGER WORK VECTOR OF LENGTH N.
!C
!C     *****HISTORY:
!C     WRITTEN BY J.A.K. CARRIG (ELEC. SYS. LAB., M.I.T., RM. 35-427,
!C     CAMBRIDGE, MA 02139, PH.: (617) - 253-7263, AUGUST 1978.
!C     MOST RECENT VERSION: AUG. 29, 1978.
!C
!C     ------------------------------------------------------------------
!C
!C     EPS IS AN INTERNALLY GENERATED MACHINE DEPENDENT PARAMETER
!C     SPECIFYING THE RELATIVE PRECISION OF FLOATING POINT ARITHMETIC.
!C     FOR EXAMPLE, EPS = 16.0D0**(-13) FOR DOUBLE PRECISION ARITHMETIC
!C     ON IBM S360/S370.
!C
      EPS=1.0D0
    1 EPS=EPS/2.0D0
      EPSP1=EPS+1.0D0
      IF(EPSP1.GT.1.0D0) GO TO 1
      EPS=2.0D0*EPS
!C
      DO 5 J=1,N
        DO 5 I=1,N
           U(I,J)=0.0D0
    5 CONTINUE
      NT = 4
      L = 1
      IBLK=0
   10 DL = 1
      IBLK=IBLK+1
      IF(L.EQ.N) GO TO 20
      S=DABS(A(L,L))+DABS(A(L+1,L+1))
      IF(DABS(A(L+1,L)).GT.EPS*S) DL=2
      IDIM(IBLK)=DL
   20 LL = L+DL-1
      K = L
   30 KM1 = K-1
      DK = 1
      IF(K.EQ.N) GO TO 35
      S=DABS(A(K,K))+DABS(A(K+1,K+1))
      IF(DABS(A(K+1,K)).GT.EPS*S) DK=2
   35 KK= K+DK-1
      IF(K.EQ.1) GO TO 45
      IF(L.NE.1) GO TO 37
      DO 36 J=L,LL
         DO 36 I=K,KK
            U(I,J)= 0.0D0
   36 CONTINUE
   37 CONTINUE
      DO 38 J=L,LL
         DO 38 I=K,KK
            DO 38 IA=1,KM1
               U(I,J)=U(I,J)+A(IA,I)*C(IA,J)
   38 CONTINUE
      DO 39 J=L,LL
         DO 39 I=K,KK
            DO 39 IB=L,LL
               C(I,J)=C(I,J)-U(I,IB)*A(IB,J)
   39 CONTINUE
      IF(IBLK.EQ.1) GO TO 45
      IBM1=IBLK-1
      JJ=1
      DO 44 IC=1,IBM1
         DM=IDIM(IC)
         IDM=DM+JJ-1
         IDK=DK+K-1
         IF(IC.NE.IBM1) GO TO 42
         DO 41 J=JJ,IDM
            DO 41 I=K,KK
               DO 41 IA=K,IDK
                  U(I,J)=U(I,J)+A(IA,I)*C(IA,J)
  41    CONTINUE
  42    CONTINUE
        DO 43 J=L,LL
           DO 43 I=K,KK
              DO 43 IA=JJ,IDM
                 C(I,J)=C(I,J)-U(I,IA)*A(IA,J)
   43   CONTINUE
        JJ=JJ+DM
   44 CONTINUE
   45 IF(DL.EQ.2) GO TO 60
      IF(DK.EQ.2) GO TO 50
      T(1,1) = A(K,K)*A(L,L)-1.0D0
      IF (T(1,1).EQ.0.0D0) RETURN
      C(K,L) = C(K,L)/T(1,1)
      GO TO 90
   50 T(1,1) = A(K,K)*A(L,L)-1.0D0
      T(1,2) = A(KK,K)*A(L,L)
      T(2,1) = A(K,KK)*A(L,L)
      T(2,2) = A(KK,KK)*A(L,L)-1.0D0
      P(1) = C(K,L)
      P(2) = C(KK,L)
      NSYS = 2
      CALL LINEQ (NT,NSYS,T,P,COND,IPVT,WORK)
      C(K,L) = P(1)
      C(KK,L) = P(2)
      GO TO 90
   60 IF(DK.EQ.2) GO TO 70
      T(1,1) = A(K,K)*A(L,L)-1.0D0
      T(1,2) = A(LL,L)*A(K,K)
      T(2,1) = A(L,LL)*A(K,K)
      T(2,2) = A(K,K)*A(LL,LL)-1.0D0
      P(1) = C(K,L)
      P(2) = C(K,LL)
      NSYS= 2
      CALL LINEQ (NT,NSYS,T,P,COND,IPVT,WORK)
      C(K,L) = P(1)
      C(K,LL) = P(2)
      GO TO 90
   70 IF(K.NE.L) GO TO 80
      T(1,1) = A(L,L)*A(L,L)-1.0D0
      T(1,2) = 2.0D0*A(LL,L)*A(L,L)
      T(1,3) = A(LL,L)*A(LL,L)
      T(2,1) = A(L,L)*A(L,LL)
      T(2,2) = A(L,L)*A(LL,LL)+A(L,LL)*A(LL,L)-1.0D0
      T(2,3) = A(LL,LL)*A(LL,L)
      T(3,1) = A(L,LL)*A(L,LL)
      T(3,2) = 2.0D0*A(L,LL)*A(LL,LL)
      T(3,3) = A(LL,LL)*A(LL,LL)-1.0D0
      P(1) = C(L,L)
      P(2) =  C(LL,L)
      P(3) = C(LL,LL)
      NSYS = 3
      CALL LINEQ (NT,NSYS,T,P,COND,IPVT,WORK)
      C(LL,L) = P(2)
      C(L,L) = P(1)
      C(L,LL) = P(2)
      C(LL,LL) = P(3)
      GO TO 90
   80 T(1,1) = A(K,K)*A(L,L)-1.0D0
      T(1,2) = A(KK,K)*A(L,L)
      T(1,3) = A(LL,L)*A(K,K)
      T(1,4) = A(KK,K)*A(LL,L)
      T(2,1) = A(L,L)*A(K,KK)
      T(2,2) = A(KK,KK)*A(L,L)-1.0D0
      T(2,3) = A(K,KK)*A(LL,L)
      T(2,4) = A(KK,KK)*A(LL,L)
      T(3,1) = A(K,K)*A(L,LL)
      T(3,2) = A(KK,K)*A(L,LL)
      T(3,3) = A(K,K)*A(LL,LL)-1.0D0
      T(3,4) = A(LL,LL)*A(KK,K)
      T(4,1) = A(K,KK)*A(L,LL)
      T(4,2) = A(L,LL)*A(KK,KK)
      T(4,3) = A(K,KK)*A(LL,LL)
      T(4,4) = A(KK,KK)*A(LL,LL)-1.0D0
      P(1) = C(K,L)
      P(2) = C(KK,L)
      P(3) = C(K,LL)
      P(4) = C(KK,LL)
      NSYS = 4
      CALL LINEQ (NT,NSYS,T,P,COND,IPVT,WORK)
      C(K,L) = P(1)
      C(KK,L) = P(2)
      C(K,LL) = P(3)
      C(KK,LL) = P(4)
   90 K= K + DK
      IF(K.LE.N) GO TO 30
      LDL = L + DL
      IF(LDL.GT.N) RETURN
      DO 100 J=LDL,N
         DO 100 I=L,LL
            C(I,J) = C(J,I)
  100 CONTINUE
      L = LDL
      GO TO 10

END SUBROUTINE


      SUBROUTINE LINEQ (NA,N,A,B,COND,IPVT,WORK)
C
C     *****PARAMETERS:
      INTEGER NA,N,IPVT(N)
      DOUBLE PRECISION A(NA,N),B(N),COND,WORK(N)
C
C     *****SUBROUTINES CALLED:
C     DGECOM,DGESLM
C
C     ------------------------------------------------------------------
C
C     *****PURPOSE:
C     THIS SUBROUTINE SOLVES THE SYSTEM OF LINEAR EQUATIONS
C                               A*X = B
C     WHERE B IS AN N-VECTOR.  SUBROUTINES DGECOM (LU-FACTORIZATION)
C     AND DGESLM (FORWARD ELIMINATION AND BACK SUBSTITUTION) ARE
C     EMPLOYED.  AN ESTIMATE OF THE CONDITION OF A IS RETURNED.
C     SHOULD A BE SINGULAR TO WORKING ACCURACY, COND IS SET TO 1.0D+20.
C
C     *****PARAMETER DESCRIPTION:
C
C     ON INPUT:
C
C        NA               ROW DIMENSION OF THE ARRAY CONTAINING A AS
C                         DECLARED IN THE CALLING PROGRAM DIMENSION
C                         STATEMENT;
C
C        N                ORDER OF THE MATRIX A;
C
C        A                N X N COEFFICIENT MATRIX;
C
C        B                RIGHT HAND SIDE VECTOR OF LENGTH N;
C
C     ON OUTPUT:
C
C        B                SOLUTION VECTOR  X = (A-INVERSE)*B;
C
C        COND             AN ESTIMATE OF THE CONDITION OF A.
C                         =1/RCOND  WHERE RCOND IS THE INVERSE
C                         OF THE CONDITION ESTIMATE (SEE THE LINPACK
!C                         USER'S GUIDE FOR DETAILS);
C
C        IPVT             PIVOT VECTOR OF LENGTH N (SEE DGECOM
C                         DOCUMENTATION);
C
C        WORK             A REAL SCRATCH VECTOR OF LENGTH N.
C
C     *****APPLICATIONS AND USAGE RESTRICTIONS:
C     THE VALUE OF COND SHOULD ALWAYS BE CHECKED BY THE CALLING
C     PROGRAM.  SHOULD A BE NEAR-SINGULAR (OR SINGULAR TO WORKING
C     ACCURACY) THE DATA SHOULD BE INVESTIGATED FOR POSSIBLE
C     ERRORS.  IF THERE ARE NONE AND THE PROBLEM IS APPARENTLY
C     WELL-POSED AND/OR MEANINGFUL, SINGULAR VALUE ANALYSIS MAY
C     THEN BE A MORE RELIABLE SOLUTION TECHNIQUE (E.G., EISPACK
C     SUBROUTINES  SVD  OR  MINFIT).
C
C     *****ALGORITHM NOTES:
C     THE CONTENTS OF A ARE MODIFIED BY THIS SUBROUTINE.  SHOULD THE
C     ORIGINAL COEFFICIENTS OF A BE NEEDED SUBSEQUENTLY, THE
C     CONTENTS OF A SHOULD BE SAVED PRIOR TO THE CALL TO LINEQ.
C
C     *****HISTORY:
!C     WRITTEN BY ALAN J. LAUB (DEP'T. OF ELEC. ENGRG. - SYSTEMS,
C     UNIVERSITY OF SOUTHERN CALIFORNIA, LOS ANGELES, CA 90007,
C     PH.: (213)-743-5535), MAY 1980.
C     MOST RECENT VERSION:  MAY 6, 1980.
C
C     ------------------------------------------------------------------
C
      CALL DGECOM (A,NA,N,IPVT,COND,WORK)
      IF ((1.0D0 + COND) .GT. 1.0D0) GO TO 20
      COND = 1.0D+20
      RETURN
   20 CONTINUE
      COND = 1.0D0/COND
      CALL DGESLM (A,NA,N,IPVT,B)
      RETURN
      END

      SUBROUTINE DGECOM (A,LDA,N,IPVT,RCOND,Z)
      INTEGER LDA,N,IPVT(N)
      DOUBLE PRECISION A(LDA,N),Z(N)
      DOUBLE PRECISION RCOND
C
C     DGECOM FACTORS A DOUBLE PRECISION MATRIX BY GAUSSIAN ELIMINATION
C     AND ESTIMATES THE CONDITION OF THE MATRIX.
C
C     IF RCOND IS NOT NEEDED, DGEFAM IS SLIGHTLY FASTER.
C     TO SOLVE  A*X = B , FOLLOW DGECOM BY DGESLM.
C     TO COMPUTE  INVERSE(A)*C , FOLLOW DGECOM BY DGESLM.
C
C     ON ENTRY:
C
C        A       DOUBLE PRECISION(LDA,N)
C                THE MATRIX TO BE FACTORED.
C
C        LDA     INTEGER
C                THE LEADING DIMENSION OF THE ARRAY A AS DECLARED
C                IN THE MAIN CALLING PROGRAM.
C
C        N       INTEGER
C                THE ORDER OF THE MATRIX A.
C
C     ON RETURN:
C
C        A       AN UPPER TRIANGULAR MATRIX AND THE MULTIPLIERS
C                WHICH WERE USED TO OBTAIN IT.
C                THE FACTORIZATION CAN BE WRITTEN  A = L*U  WHERE
C                L IS A PRODUCT OF PERMUTATION AND UNIT LOWER
C                TRIANGULAR MATRICES AND U IS UPPER TRIANGULAR.
C
C        IPVT    INTEGER(N)
C                AN INTEGER VECTOR OF PIVOT INDICES.
C                   IPVT(K) = THE INDEX OF THE K-TH PIVOT ROW
C                THE DETERMINANT OF A CAN BE OBTAINED ON OUTPUT BY
C                   DET(A) = S*A(1,1)*A(2,2)* ... *A(N,N)
C                WHERE S = (-1)**(NUMBER OF TIMES IPVT(K) .NE. K)
C                BUT THIS RESULT SHOULD BE USED WITH CAUTION AS IT
C                MAY EASILY UNDERFLOW OR OVERFLOW.
C
C        RCOND   DOUBLE PRECISION
C                AN ESTIMATE OF THE RECIPROCAL CONDITION OF A.
C                FOR THE SYSTEM  A*X = B , RELATIVE PERTURBATIONS
C                IN A AND B OF SIZE EPSILON MAY CAUSE RELATIVE
C                PERTURBATIONS IN X OF SIZE  EPSILON/RCOND.
C                IF RCOND IS SO SMALL THAT THE LOGICAL EXPRESSION
C                             1.0D0 + RCOND  .EQ.  1.0D0
C                IS TRUE, THEN A MAY BE SINGULAR TO WORKING
C                PRECISION.  IN PARTICULAR, RCOND IS ZERO IF
C                EXACT SINGULARITY IS DETECTED OR THE ESTIMATE
C                UNDERFLOWS.
C
C        Z       DOUBLE PRECISION(N)
C                A WORK VECTOR WHOSE CONTENTS ARE USUALLY UNIMPORTANT.
C                IF A IS CLOSE TO A SINGULAR MATRIX, THEN Z IS AN
C                APPROXIMATE NULL VECTOR IN THE SENSE THAT
C                       NORM(A*Z) = RCOND*NORM(A)*NORM(Z).
C
C     THIS VERSION IS ADAPTED FROM THE LINPACK SUBROUTINE DGECO BY
C     REPLACEMENT OF CALLS TO THE BLAS WITH IN-LINE CODE.  MODIFICATION
C     DONE BY ALAN J. LAUB, UNIVERSITY OF SOUTHERN CALIFORNIA,
C     APRIL 1980.
C
C     SUBROUTINES AND FUNCTIONS CALLED:
C
C     DGEFAM
C
C     FORTRAN FUNCTIONS CALLED:
C
      DOUBLE PRECISION DABS,DSIGN
C
C     INTERNAL VARIABLES:
C
      INTEGER I,INFO,J,K,KB,KM1,KP1,L
      DOUBLE PRECISION ANORM,EK,S,SM,T,WK,WKM,YNORM
C
C     COMPUTE 1-NORM OF A
C
      ANORM = 0.0D0
      DO 20 J=1,N
         T = 0.0D0
         DO 10 I=1,N
            T = T+DABS(A(I,J))
   10    CONTINUE
         IF (T .GT. ANORM) ANORM = T
   20 CONTINUE
C
C     FACTOR
C
      CALL DGEFAM(A,LDA,N,IPVT,INFO)
C
C     RCOND = 1/(NORM(A)*(ESTIMATE OF NORM(INVERSE(A)))).
C     ESTIMATE = NORM(Z)/NORM(Y)  WHERE  A*Z = Y AND  TRANS(A)*Y = E.
C     TRANS(A)  IS THE TRANSPOSE OF A.  THE COMPONENTS OF E ARE
C     CHOSEN TO CAUSE MAXIMUM LOCAL GROWTH IN THE ELEMENTS OF W WHERE
C     TRANS(U)*W = E .  THE VECTORS ARE FREQUENTLY RESCALED TO AVOID
C     OVERFLOW.
C
C     SOLVE  TRANS(U)*W = E
C
      EK = 1.0D0
      DO 30 I=1,N
         Z(I) = 0.0D0
   30 CONTINUE
      DO 120 K=1,N
         IF (Z(K) .NE. 0.0D0) EK = DSIGN(EK,-Z(K))
         IF (DABS(EK-Z(K)) .LE. DABS(A(K,K))) GO TO 50
         S = DABS(A(K,K))/DABS(EK-Z(K))
         DO 40 I=1,N
            Z(I) = S*Z(I)
   40    CONTINUE
         EK = S*EK
   50    CONTINUE
         WK = EK-Z(K)
         WKM = -EK-Z(K)
         S = DABS(WK)
         SM = DABS(WKM)
         IF (A(K,K) .EQ. 0.0D0) GO TO 60
         WK = WK/A(K,K)
         WKM = WKM/A(K,K)
         GO TO 70
   60    CONTINUE
         WK = 1.0D0
         WKM = 1.0D0
   70    CONTINUE
         KP1 = K+1
         IF (KP1 .GT. N) GO TO 110
         DO 80 J=KP1,N
            SM = SM+DABS(Z(J)+WKM*A(K,J))
            Z(J) = Z(J)+WK*A(K,J)
            S = S+DABS(Z(J))
   80    CONTINUE
         IF (S .GE. SM) GO TO 100
         T = WKM-WK
         WK = WKM
         DO 90 J=KP1,N
            Z(J) = Z(J)+T*A(K,J)
   90    CONTINUE
  100    CONTINUE
  110    CONTINUE
         Z(K) = WK
  120 CONTINUE
      T = 0.0D0
      DO 130 I=1,N
         T = T+DABS(Z(I))
  130 CONTINUE
      S = 1.0D0/T
      DO 140 I=1,N
         Z(I) = S*Z(I)
  140 CONTINUE
C
C     SOLVE  TRANS(L)*Y = W
C
      DO 190 KB=1,N
         K = N+1-KB
         IF (K .EQ. N) GO TO 160
         KP1 = K+1
         T = 0.0D0
         DO 150 I=KP1,N
            T = T+A(I,K)*Z(I)
  150    CONTINUE
         Z(K) = Z(K)-T
  160    CONTINUE
         IF (DABS(Z(K)) .LE. 1.0D0) GO TO 180
         S = 1.0D0/DABS(Z(K))
         DO 170 I=1,N
            Z(I) = S*Z(I)
  170    CONTINUE
  180    CONTINUE
         L = IPVT(K)
         T = Z(L)
         Z(L) = Z(K)
         Z(K) = T
  190 CONTINUE
      T = 0.0D0
      DO 200 I=1,N
         T = T+DABS(Z(I))
  200 CONTINUE
      S = 1.0D0/T
      DO 210 I=1,N
         Z(I) = S*Z(I)
  210 CONTINUE
C
      YNORM = 1.0D0
C
C     SOLVE  L*V = Y
C
      DO 260 K=1,N
         L = IPVT(K)
         T = Z(L)
         Z(L) = Z(K)
         Z(K) = T
         IF (K .EQ. N) GO TO 230
         KP1 = K+1
         DO 220 I=KP1,N
            Z(I) = Z(I)-T*A(I,K)
  220    CONTINUE
  230    CONTINUE
         IF (DABS(Z(K)) .LE. 1.0D0) GO TO 250
         S = 1.0D0/DABS(Z(K))
         DO 240 I=1,N
            Z(I) = S*Z(I)
  240    CONTINUE
         YNORM = S*YNORM
  250    CONTINUE
  260 CONTINUE
      T = 0.0D0
      DO 270 I=1,N
         T = T+DABS(Z(I))
  270 CONTINUE
      S = 1.0D0/T
      DO 280 I=1,N
         Z(I) = S*Z(I)
  280 CONTINUE
      YNORM = S*YNORM
C
C     SOLVE  U*Z = V
C
      DO 330 KB=1,N
         K = N+1-KB
         IF (DABS(Z(K)) .LE. DABS(A(K,K))) GO TO 300
         S = DABS(A(K,K))/DABS(Z(K))
         DO 290 I=1,N
            Z(I) = S*Z(I)
  290    CONTINUE
         YNORM = S*YNORM
  300    CONTINUE
         IF (A(K,K) .NE. 0.0D0) Z(K) = Z(K)/A(K,K)
         IF (A(K,K) .EQ. 0.0D0) Z(K) = 1.0D0
         T = -Z(K)
         IF (K .EQ. 1) GO TO 320
         KM1 = K-1
         DO 310 I=1,KM1
            Z(I) = Z(I)+T*A(I,K)
  310    CONTINUE
  320    CONTINUE
  330 CONTINUE
C
C     MAKE ZNORM = 1.0D0
C
      T = 0.0D0
      DO 340 I=1,N
         T = T+DABS(Z(I))
  340 CONTINUE
      S = 1.0D0/T
      DO 350 I=1,N
         Z(I) = S*Z(I)
  350 CONTINUE
      YNORM = S*YNORM
C
C     DETERMINE RCOND
C
      IF (ANORM .NE. 0.0D0) RCOND = YNORM/ANORM
      IF (ANORM .EQ. 0.0D0) RCOND = 0.0D0
      RETURN
      END


      SUBROUTINE DGEFAM (A,LDA,N,IPVT,INFO)
      INTEGER LDA,N,IPVT(N),INFO
      DOUBLE PRECISION A(LDA,N)
C
C     DGEFAM FACTORS A DOUBLE PRECISION REAL MATRIX BY
C     GAUSSIAN ELIMINATION.
C
C     DGEFAM IS USUALLY CALLED BY DGECOM, BUT IT CAN BE CALLED
C     DIRECTLY WITH A MODEST SAVING IN TIME IF RCOND IS NOT
C     NEEDED.
C
C     ON ENTRY:
C
C        A       DOUBLE PRECISION(LDA,N)
C                THE MATRIX TO BE FACTORED.
C
C        LDA     INTEGER
C                THE LEADING DIMENSION OF THE ARRAY A AS DECLARED
C                IN THE MAIN CALLING PROGRAM.
C
C        N       INTEGER
C                THE ORDER OF THE MATRIX A.
C
C     ON RETURN:
C
C        A       AN UPPER TRIANGULAR MATRIX AND THE MULTIPLIERS
C                WHICH WERE USED TO OBTAIN IT.
C                THE FACTORIZATION CAN BE WRITTEN  A = L * U WHERE
C                L IS A PRODUCT OF PERMUTATION AND UNIT LOWER
C                TRIANGULAR MATRICES AND U IS UPPER TRIANGULAR.
C
C        IPVT    INTEGER(N)
C                AN INTEGER VECTOR OF PIVOT INDICES.
C                   IPVT(K) = THE INDEX OF THE K-TH PIVOT ROW
C                THE DETERMINANT OF A CAN BE OBTAINED ON OUTPUT BY
C                   DET(A) = S*A(1,1)*A(2,2)* ... *A(N,N)
C                WHERE S = (-1)**(NUMBER OF TIMES IPVT(K) .NE. K)
C                BUT THIS RESULT SHOULD BE USED WITH CAUTION AS IT
C                MAY EASILY UNDERFLOW OR OVERFLOW.
C
C        INFO    INTEGER
C                = 0  NORMAL VALUE.
C                = K  IF  U(K,K) .EQ. 0.0D0.  THIS IS NOT AN ERROR
C                     CONDITION FOR THIS SUBROUTINE, BUT IT DOES
C                     INDICATE THAT DGESLM WILL DIVIDE BY ZERO IF
C                     CALLED.
C                     USE RCOND IN DGECOM FOR A RELIABLE INDICATION OF
C                     SINGULARITY.
C
C     THIS VERSION IS ADAPTED FROM THE LINPACK SUBROUTINE DGEFA BY
C     REPLACEMENT OF CALLS TO THE BLAS WITH IN-LINE CODE.  MODIFICATION
C     DONE BY ALAN J. LAUB, UNIVERSITY OF SOUTHERN CALIFORNIA,
C     APRIL 1980.
C
C     FORTRAN FUNCTIONS CALLED:
C
      DOUBLE PRECISION DABS
C
C     INTERNAL VARIABLES:
C
      INTEGER I,J,K,KP1,L,NM1
      DOUBLE PRECISION T
C
C     GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING
C
      IPVT(N) = 1
      INFO = 0
      NM1 = N-1
      IF (NM1 .LT. 1) GO TO 100
      DO 90 K=1,NM1
         KP1 = K+1
C
C        FIND L = PIVOT INDEX
C
         L = K
         DO 10 I=KP1,N
            IF (DABS(A(I,K)) .GT. DABS(A(L,K))) L = I
   10    CONTINUE
         IPVT(K) = L
C
C        ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED
C
         IF (A(L,K) .EQ. 0.0D0) GO TO 70
C
C        INTERCHANGE IF NECESSARY
C
         IF (L .EQ. K) GO TO 20
         T = A(L,K)
         A(L,K) = A(K,K)
         A(K,K) = T
   20    CONTINUE
C
C        COMPUTE MULTIPLIERS
C
         T = 1.0D0/A(K,K)
         DO 30 I=KP1,N
            A(I,K) = T*A(I,K)
   30    CONTINUE
C
C        ROW ELIMINATION WITH COLUMN INDEXING
C
         DO 60 J=KP1,N
            T = A(L,J)
            IF (L .EQ. K) GO TO 40
            A(L,J) = A(K,J)
            A(K,J) = T
   40       CONTINUE
            DO 50 I=KP1,N
               A(I,J) = A(I,J)-T*A(I,K)
   50       CONTINUE
   60    CONTINUE
         GO TO 80
   70    CONTINUE
         INFO = K
   80    CONTINUE
   90 CONTINUE
  100 CONTINUE
      IPVT(N) = N
      IF (A(N,N) .EQ. 0.0D0) INFO = N
      RETURN
      END


      SUBROUTINE DGESLM (A,LDA,N,IPVT,B)
      INTEGER LDA,N,IPVT(N)
      DOUBLE PRECISION A(LDA,N),B(N)
C
C     DGESLM SOLVES THE (REAL) LINEAR SYSTEM
C            A * X = B
C     USING THE FACTORS COMPUTED BY DGECOM OR DGEFAM.
C
C     ON ENTRY:
C
C        A       DOUBLE PRECISION(LDA,N)
C                THE OUTPUT FROM DGECOM OR DGEFAM.
C
C        LDA     INTEGER
C                THE LEADING DIMENSION OF THE ARRAY A AS DECLARED
C                IN THE MAIN CALLING PROGRAM.
C
C        N       INTEGER
C                THE ORDER OF THE MATRIX A.
C
C        IPVT    INTEGER(N)
C                THE PIVOT VECTOR FROM DGECOM OR DGEFAM.
C
C        B       DOUBLE PRECISION(N)
C                THE RIGHT HAND SIDE VECTOR.
C
C     ON RETURN:
C
C        B       THE SOLUTION VECTOR X.
C
C     ERROR CONDITION:
C
C        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS A
C        ZERO ON THE DIAGONAL.  TECHNICALLY THIS INDICATES SINGULARITY
C        BUT IT IS OFTEN CAUSED BY IMPROPER ARGUMENTS OR IMPROPER
C        SETTING OF LDA.  IT WILL NOT OCCUR IF THE SUBROUTINES ARE
C        CALLED CORRECTLY AND IF DGECOM HAS SET RCOND .GT. 0.0
C        OR DGEFAM HAS SET INFO .EQ. 0.
C
C     TO COMPUTE  INVERSE(A) * B  WHERE B IS A MATRIX WITH M COLUMNS
C             CALL DGECOM (A,LDA,N,IPVT,RCOND,Z)
C             IF (RCOND IS TOO SMALL) GO TO ...
C             DO 10 J=1,M
C                CALL DGESLM (A,LDA,N,IPVT,B(1,J))
C          10 CONTINUE
C
C     THIS VERSION IS ADAPTED FROM THE LINPACK SUBROUTINE DGESL BY
C     REPLACEMENT OF CALLS TO THE BLAS WITH IN-LINE CODE.  MODIFICA-
C     TION DONE BY ALAN J. LAUB, UNIVERSITY OF SOUTHERN CALIFORNIA,
C     APRIL 1980.
C
C     FORTRAN FUNCTIONS CALLED:
C
C     NONE
C
C     INTERNAL VARIABLES:
C
      INTEGER I,K,KB,KM1,KP1,L,NM1
      DOUBLE PRECISION T
C
      IF (N .EQ. 1) GO TO 60
      NM1 = N-1
C
C     FIRST SOLVE L*Y = B
C
      DO 30 K=1,NM1
         KP1 = K+1
         L = IPVT(K)
         T = B(L)
         IF (L .EQ. K) GO TO 10
         B(L) = B(K)
         B(K) = T
   10    CONTINUE
         DO 20 I=KP1,N
            B(I) = B(I)-T*A(I,K)
   20    CONTINUE
   30 CONTINUE
C
C     NOW SOLVE  U*X = Y
C
      DO 50 KB=1,NM1
         K = N+1-KB
         KM1 = K-1
         B(K) = B(K)/A(K,K)
         T = -B(K)
         DO 40 I=1,KM1
            B(I) = B(I)+T*A(I,K)
   40    CONTINUE
   50 CONTINUE
   60 CONTINUE
      B(1) = B(1)/A(1,1)
      RETURN
      END
