!C     ALGORITHM 413 COLLECTED ALGORITHMS FROM ACM.
!C     ALGORITHM APPEARED IN COMM. ACM, VOL. 14, NO. 10,
!C     P. 669.
      SUBROUTINE ENTCRE ( CFUN, ZETA, RCIRC, EPREQ, EPMACH, NMAX, NCODE, &
                 EPEST, NTCOF, TCOF, WORK, NTAB, SINTAB )
            use mod_kinds, only : i4,sp
#if 0     
C
C ** EVALUATION OF NORMALIZED TAYLOR COEFFICIENTS **
C **         OF A REAL(kind=sp) ANALYTIC FUNCTION          **
C
C     **  GENERAL PURPOSE  **
C THIS ROUTINE EVALUATES A SET OF NORMALIZED TAYLOR COEFFICIENTS
C TCOF(J+1) = (RCIRC**J) * (J-TH DERIVATIVE OF CFUN(Z) AT Z=ZETA)
C DIVIDED BY FACTORIAL(J) ... J = 0,1,2,3...NMAX-1.
C TO A UNIFORM ABSOLUTE ACCURACY **EPEST** USING FUNCTION
C VALUES OF CFUN(Z) AT POINTS IN THE COMPLEX(kind=sp) PLANE LYING ON
C THE CIRCLE OF RADIUS **RCIRC** WITH CENTER AT Z = ZETA.
C THIS ROUTINE IS A SPECIAL VERSION OF ENTCAF FOR USE WHEN
C ZETA IS REAL(kind=sp) AND ALSO CFUN(Z) IS REAL(kind=sp) WHEN Z IS REAL(kind=sp).
C
C     ** THEORETICAL RESTRICTIONS **
C RCIRC MUST BE SMALLER THAN THE RADIUS OF CONVERGENCE OF
C THE TAYLOR SERIES.  THE PROBLEM HAS TO BE REFORMULATED
C SHOULD CFUN(Z) HAPPEN TO BE AN ODD FUNCTION
C OF  (Z - ZETA)  , THAT IS IF THE RELATION
C ** -CFUN(-(Z-ZETA))=CFUN(Z-ZETA) **  IS AN IDENTITY.
C
C     ** REQUIREMENTS FOR CALLING PROGRAM **
C CALLING PROGRAM MUST CONTAIN CONTROL STATEMENTS DESCRIBED
C NOTES (3) AND (4) BELOW.  IT MUST ALSO ASSIGN VALUES TO
C INPUT PARAMETERS.  THE ROUTINE REQUIRES TWO SUBPROGRAMS,
C HFCOF  (LISTED AFTER ENTCRE) AND CFUN (SEE NOTE (4) BELOW).
C
C     **INPUT PARAMETERS**
C (1) CFUN   NAME OF COMPLEX(kind=sp) FUNCTION SUBPROGRAM.
C (2) ZETA   REAL(kind=sp) POINT ABOUT WHICH TAYLOR EXPANSION IS REQUIRED
C (3) RCIRC  RADIUS (REAL(kind=sp))
C (4) EPREQ  THE ABSOLUTE ACCURACY (REAL(kind=sp)) TO WHICH THE
C            NORMALIZED TAYLOR COEFFICENTS, TCOF(J), ARE REQUIRED
C (5) EPMACH THE MACHINE ACCURACY PARAMETER (REAL(kind=sp))
C            (OR AN UPPER BOUND ON THE RELATIVE ACCURACY OF
C            QUANTITES LIKELY TO BE ENCOUNTERED).
C (6) NMAX   PHYSICAL UPPER LIMIT ON THE SIZE AND LENGTH
C            OF THE CALCULATION.  THE MAXIMUM NUMBER OF
C            COEFFICIENTS CALCULATED WILL BE THAT POWER OF TWO
C            LESS THAN OR EQUAL TO NMAX.  NMAX IS ASSUMED TO
C            BE AT LEAST 8.  (SEE NOTE(3) BELOW).
C (7) NCODE  .GE.0  THE ROUTINE WILL DO AS WELL AS IT CAN.
C            .LT.0  THE ROUTINE WILL ABORT AT AN EARLY STAGE
C            IF THE REQUIRED ACCURACY CANNOT BE ATTAINED
C            BECAUSE OF ROUND OFF ERROR.
C (12) NTAB  IN NORMAL RUNNING, NTAB SHOULD BE SET TO ZERO
C            BEFORE THE FIRST CALL TO ENTCRE, BUT LEFT ALONE
C            AFTER THAT.  (FOR MORE SOPHISTICATED USE, SEE
C            OUTPUT PARAMETERS (12) AND (13) AND NOTE(2) BELOW)
C
C     ** OUTPUT PARAMETERS **
C  (1),(2),(3),(4),(5),(6) IDENTICAL WITH INPUT VALUES.
C (7)  NCODE  RESULT STATUS INDICATOR.
C             TAKES ONE OF FIVE VALUES AS FOLLOWS,
C             = +1. CONVERGED NORMALLY.
C             = -1. DID NOT CONVERGE. NO ROUND OFF ERROR
C             TROUBLE.
C             = +2. CONVERGED, BUT WITH A HIGHER TOLERANCE
C             SET BY THE ROUND OFF LEVEL. (EPEST.GT.EPREQ)
C             = -2. DID NOT CONVERGE IN SPITE OF HIGHER
C             TOLERANCE SET BY ROUND OFF LEVEL.
C             =  0. RUN WAS ABORTED BECAUSE EPREQ IS
C             UNATTAINABLE DUE TO ROUND OFF LEVEL AND INPUT
C             NCODE IS NEGATIVE.
C (8)  EPEST  ESTIMATE OF ACTUAL UNIFORM ABSOLUTE ACCURACY
C             IN ALL TCOF.  EXCEPT, IF NCODE.EQ.0  ESTIMATE
C             OF ROUND OFF LEVEL.
C (9)  NTCOF  NUMBER OF NONTRIVIAL VALUES OF TCOF ACTUALLY
C             CALCULATED.  THEY ARE BASED ON NTCOF/2+2 CALLS
C             OF CFUN (THREE CALLS WERE FOR PURELY
C             REAL(kind=sp) ARGUMENT).
C (10) TCOF   REAL(kind=sp) DIMENSION (DIM).  APPROXIMATIONS TO THE
C             NORMALIZED TAYLOR COEFFICIENTS, EXCEPT WHEN
C             OUTPUT NCODE = 0. (SEE NOTE(3) BELOW)
C (11) WORK   INTERNAL WORKING AREA OF REAL(kind=sp) DIMENSION (DIM)
C             (SEE NOTE(3) BELOW.) CONTENTS IS IDENTICAL WITH
C             THAT OF TCOF.
C (12) NTAB   NUMBER OF VALUES OF SINTAB AVAILABLE
C             (SEE NOTE (2) BELOW).
C (13) SINTAB REAL(kind=sp) DIMENSION (DIM/4). (SEE NOTES (2) AND (3)
C             BELOW.) SINTAB(J+1) = SIN(PI*J/2*NTAB) ,
C             J = 0,1,2,...NTAB-1.
C             (A QUARTER CYCLE) OTHER LOCATIONS ARE EMPTY.
C
C     ** NOTES ON INPUT/OUTPUT PARAMETERS **
C  NOTE(1)**  NCODE IS USED BOTH AS INPUT AND OUTPUT PARAMETER.
C NORMALLY IT RETAINS THE VALUE +1 AND NEED NOT BE RESET
C BETWEEN NORMAL RUNS.
C  NOTE(2)**  THE APPEARANCE OF NTAB AND SINTAB IN THE
C CALLING SEQUENCE ALLOWS THE USER TO MAKE USE OF - OR TO
C PRECOMPUTE - THESE NUMBERS IN ANOTHER PART OF THE PROGRAM
C SHOULD HE SO DESIRE.  NTAB MUST BE A POWER OF TWO OR 0.
C  NOTE(3)**  THE APPEARANCE OF NMAX,TCOF,WORK AND SINTAB IN
C THE CALLING SEQUENCE ALLOWS THE SCOPE OF THE SUBPROGRAM AND
C THE AMOUNT OF STORAGE TO BE ASSIGNED BY THE CALLING
C PROGRAM, WHICH SHOULD CONTAIN A CONTROL STATEMENT TO THE
C FOLLOWING EFFECT -
C  REAL(kind=sp) TCOF(DIM), WORK(DIM), SINTAB(DIM/4)
C WHERE DIM IS NORMALLY A POWER OF TWO.  NMAX IS NORMALLY
C EQUAL TO DIM,  BUT MAY BE LESS THAN DIM.
C  NOTE(4)**  CFUN(Z)  IS A USER PROVIDED COMPLEX(kind=sp) VALUED
C FUNCTION SUBPROGRAM WITH A COMPLEX(kind=sp) VALUED ARGUMENT.  THE
C CALLING PROGRAM MUST CONTAIN CONTROL STATEMENTS AS FOLLOWS -
C  EXTERNAL CFUN     COMPLEX(kind=sp) CFUN
C
C     ** BOOKKEEPING PARAMETERS FOR STAGE ONE **
C  NCONV   1  CONVERGENCE ACHIEVED.
C         -1  NO CONVERGENCE ACHIEVED.
C  NROUND  1  NO ROUND OFF TROUBLE OBSERVED.
C          2  ROUND OFF TROUBLE OBSERVED.
C  NABORT  0  UPDATE TOLERANCE AND CONTINUE ON APPEARANCE OF
C             OF ROUND OFF TROUBLE.
C          1  TERMINATE WHEN ROUND OFF TROUBLE OBSERVED.
C  EXACT   THE EXACT VALUE OF TCOF(1) WHICH IS CFUN(ZETA).
C  SAFETY  THIS IS A SAFETY FACTOR BY WHICH THE ROUTINE AVOIDS
C          THE ROUND OFF LEVEL.  IT IS SET TO 10.0 AND
C          APPEARS ONLY IN THE COMBINATION (SAFETY*EPMACH).
C          TO ALTER THIS FACTOR, OR TO REMOVE THE ROUND OFF
C          ERROR GUARD COMPLETELY, THE USER NEED ONLY ADJUST
C          THE INPUT PARAMETER EPMACH APPROPRIATELY.
C
C     ** QUANTITIES CALCULATED IN STAGE THREE(A) **
C  THIS IS THE FIRST PART OF ITERATION NUMBER NTCOF. PRESENTLY
C AVAILABLE ARE -
C SINTAB(J+1) = SIN(PI*J/2*NTAB) , J = 0,1,2,...NTAB-1.
C WE REQUIRE THE SEQUENCE SIN(PI*J/2*(NTCOF/4)),
C J = 1,3,5,...(NTCOF/4-1).
C IF (NTCOF.LE.4*NTAB) THESE NUMBERS ARE ALREADY AVAILABLE IN
C THE SINTAB TABLE SPACED AT AN INTERVAL 2*NSPACE = 8*NTAB/NTCOF.
C OTHERWISE, NTCOF = 8*NTAB AND THE SINTAB TABLE IS UPDATED.
C THIS INVOLVES REARRANGING THE NTAB VALUES AVAILABLE,
C CALCULATING AND STORING NTAB NEW VALUES AND UPDATING
C NTAB TO 2*NTAB.
C
C     ** QUANTITIES CALCULATED IN STAGE THREE(B) **
C  ITERATIONS ARE NUMBERED 8,16,32,...AT THE END OF ITERATION
C NTCOF, THE NTCOF/2 + 1 COMPLEX(kind=sp) FUNCTION VALUES AT
C ABSCISSAS REGULARLY SPACED ON UPPER HALF OF CIRCLE ARE
C STORED IN THE TCOF VECTOR AS FOLLOWS.
C  TCOF(J+1)  =  REAL(kind=sp) PART OF CFUN(Z(J)) J=0,1,2,...NTCOF/2.
C  TCOF(NTCOF-J+1) = IMAGINARY PART OF CFUN(Z(J))
C                    J=1,2,...(NTCOF/2-1).
C WHERE
C  Z(J) = ZETA + RCIRC*CEXP(2*PI*EYE*J/NTCOF)
C THIS INVOLVES A REARRANGEMENT OF THE NTCOF/4 + 1 FUNCTION
C VALUES AVAILABLE AT THE START OF THE ITERATION AND THE
C CALCULATION OF A FURTHER NTCOF/4 FUNCTION VALUES.  IN
C ADDITION FMAX AND APPROX ARE CALCULATED.  THESE ARE
C  FMAX   MAXIMUM MODULUS OF THE FUNCTION VALUES SO FAR
C         ENCOUNTERED.
C  APPROX AN APPROXIMATION TO TCOF(1)
C         BASED ON THESE FUNCTION VALUES.
C
C     ** QUANTITIES CALCULATED AT STAGE THREE(C) **
C  ERROR1  CURRENT VALUE OF THE ERROR = ABS(APPROX-EXACT).
C  ERROR2,ERROR3,ERROR4  VALUES OF ERROR AT END OF THREE
C          PREVIOUS ITERATIONS.
C  EPMACH  MACHINE ACCURACY PARAMETER. (INPUT PARAMETER)
C  EPREQ   REQUIRED ACCURACY. (INPUT PARAMETER)
C  EPRO    HIGHEST ACCURACY REASONABLY ATTAINABLE IN VIEW OF
C          THE SIZE OF THE FUNCTION VALUES SO FAR ENCOUNTERED.
C          (=10.0*EPMACH*FMAX)
C  EPCOF   CURRENTLY REQUIRED ACCURACY (=AMAX1(EPREQ,EPRO)).
C  EPEST   ESTIMATE OF CURRENT ACCURACY. (THE MAXIMUM OF EPRO
C          AND A FUNCTION OF ERRORS 1,2,3 AND 4) (OUTPUT PARAMETER)
C
C ** CONVERGENCE AND TERMINATION CHECKS IN STAGE THREE(C) **
C (1)  USES FMAX TO RAISE EPCOF ABOVE ROUND OFF LEVEL.  IF
C  THIS IS NECESSARY AND THE INPUT VALUE OF NCODE IS NEGATIVE,
C  IT TERMINATES SETTING NCODE = 0.
C (2)  USES APPROX TO EVALUATE CONVERGENCE OF TCOF(1) TOWARDS
C  EXACT.  IT MAY ASSIGN CONVERGENCE AND GO TO STAGE FOUR(A)
C  SETTING NCODE = +1 OR +2.
C (3)  USES NMAX TO CHECK PHYSICAL LIMIT.  IF THIS HAS BEEN
C  REACHED, IT GOES TO STAGE FOUR(A) SETTING NCODE = -1 OR -2.
C (4)  OTHERWISE CONTINUES NEXT ITERATION BY GOING TO STAGE THREE
C
C  ** CALCULATION OF FIRST NTCOF TAYLOR COEFFICIENTS IN
C  **  STAGE FOUR(A)
C A VERSION OF THE FAST FOURIER TRANSFORM USING A WORK ARRAY
C IS USED.  THE ARRAY **WORK** IS USED ONLY DURING THIS STAGE.
C THE WORK ARRAY ALLOWS THE PERMUTING OF INDICES ASSOCIATED
C WITH IN-PLACE FFTS TO BE SUPPRESSED.  THE FFT CALCULATES
C THE NECCESSARY SUMMATIONS EXCEPT FOR DIVIDING BY NTCOF.
C
C ** SETTING OF REMAINING TAYLOR COEFFICIENTS IN STAGE FOUR(B) **
C  THE CONVERGENCE CRITERION ALLOWS US TO INFER THAT THE
C NORMALIZED TAYLOR COEFFICIENTS OF ORDER GREATER THAN NTCOF
C ARE ZERO TO ACCURACY  EPEST. THEY ARE EVALUATED AS BEING
C EXACTLY ZERO.
C REAL(kind=sp) VARIABLE R IS USED TO PUT VALUE OF CABS(FVAL) AND USE IT IN
C AMAX1 FUNCTION CALL, SINCE FUNCTION CALL WITHIN A FUNCTION CALL
C IS NOT ALLOWED IN CDC FORTRAN.
#endif
      COMPLEX(kind=sp) CFUN
      REAL(kind=sp) ZETA,RCIRC,EPREQ,EPMACH,EPEST
      INTEGER(kind=i4) NMAX,NCODE,NTCOF,NTAB
      REAL(kind=sp) TCOF (*), WORK (*), SINTAB (*)
      INTEGER(kind=i4) NABORT,NCONV,NDISP,NDOLIM,NPREV,NROUND,NSPACE
      INTEGER(kind=i4) J,JCONJ,JCOS,JFROM,JRCONJ,JREFL,JSIN,JTO
      REAL(kind=sp) APPROX,COSDIF,EPCOF,EPMIN,EPRO,EP32,EP42
      REAL(kind=sp) ERROR1,ERROR2,ERROR3,ERROR4,EXACT,FMAX,FVALIM
      REAL(kind=sp) FVALRE,RCOS,RSIN,SAFETY,SCALE,SUPPER,TWOPI
      REAL(kind=sp) R
      COMPLEX(kind=sp) FVAL,ZVAL
      COMPLEX(kind=sp) CMPLX
!C ***   STAGE ONE   ***
!C ---------------------
!C INITIALISE BOOKKEEPING PARAMETERS AND EXACT VALUE OF TCOF(1).
      NROUND = 1
      NABORT = 0
      IF (NCODE.LT.0) NABORT = 1
      EPCOF = EPREQ
      SAFETY = 10.0
      ZVAL = CMPLX(ZETA,0.0)
      FVAL = CFUN(ZVAL)
      FVALRE = REAL(FVAL,kind=sp)
      EXACT = FVALRE
!C ***   STAGE TWO   ***
!C ---------------------
!C FIRST THREE ITERATIONS ( THOSE WITH NTCOF = 1,2,4 ).
      ZVAL = CMPLX(ZETA+RCIRC,0.0)
      FVAL = CFUN(ZVAL)
      FVALRE = REAL(FVAL,kind=sp)
      APPROX = FVALRE
      FMAX = ABS(FVALRE)
      TCOF(1) = FVALRE
      ERROR3 = ABS(APPROX-EXACT)
      ZVAL = CMPLX(ZETA-RCIRC,0.0)
      FVAL = CFUN(ZVAL)
      FVALRE = REAL(FVAL,kind=sp)
      APPROX = 0.5*(APPROX+FVALRE)
      FMAX = AMAX1(FMAX,ABS(FVALRE))
      TCOF(3) = FVALRE
      ERROR2 = ABS(APPROX-EXACT)
      ZVAL = CMPLX(ZETA,RCIRC)
      FVAL = CFUN(ZVAL)
      FVALRE = REAL(FVAL,kind=sp)
      FVALIM = AIMAG(FVAL)
      APPROX = 0.5*(APPROX+FVALRE)
       R=CABS(FVAL)
       FMAX=AMAX1(FMAX,R)
      TCOF(2) = FVALRE
      TCOF(4) = FVALIM
      ERROR1 = ABS(APPROX-EXACT)
      NTCOF = 4
      EPRO = FMAX*SAFETY*EPMACH
      IF (EPRO.LT.EPCOF) GO TO 300
        EPCOF = EPRO
        NROUND = 2
        IF (NABORT.EQ.0) GO TO 300
        NCODE = 0
        EPEST = EPRO
        GO TO 470
!C ***   STAGE THREE   ***
!C -----------------------
!C COMMENCE ITERATION NUMBER NTCOF.
  300 CONTINUE
      NPREV = NTCOF
      NTCOF = 2*NTCOF
!C ***   STAGE THREE(A)   ***
!C --------------------------
!C UPDATE SINTAB TABLE IF NECESSARY.
      IF (4*NTAB.GE.NTCOF) GO TO 340
      IF (NTAB.GE.2) GO TO 310
        SINTAB(1) = 0.0
        SINTAB(2) = SQRT(0.5)
        NTAB = 2
        GO TO 340
  310 CONTINUE
      NDOLIM = NTAB-1
      DO 320 J = 1,NDOLIM
        JFROM = NTAB-J
        JTO = 2*JFROM
        SINTAB(JTO+1) = SINTAB(JFROM+1)
  320 CONTINUE
      NTAB = 2*NTAB
      TWOPI = 8.0*ATAN(1.0)
      COSDIF = COS(TWOPI/FLOAT(4*NTAB))
      NDOLIM = NTAB-3
      DO 330 J = 1,NDOLIM,2
        SINTAB(J+1) = (0.5*SINTAB(J)+0.5*SINTAB(J+2))/COSDIF
  330 CONTINUE
      SINTAB(NTAB) = COSDIF
  340 CONTINUE
!C ***   STAGE THREE(B)   ***
!C --------------------------
!C UPDATE LIST OF FUNCTION VALUES IN TCOF,
!C CALCULATE FMAX AND APPROX.
      NDOLIM = NPREV-1
      DO 350 J = 1,NDOLIM
        JFROM = NPREV-J
        JTO = 2*JFROM
        TCOF(JTO+1) = TCOF(JFROM+1)
  350 CONTINUE
      SUPPER = 0.0
      NDOLIM = (NPREV/2)-1
      NSPACE = (4*NTAB)/NTCOF
      DO 360 J = 1,NDOLIM,2
        JSIN = J*NSPACE
        JCOS = NTAB-JSIN
        RSIN = RCIRC*SINTAB(JSIN+1)
        RCOS = RCIRC*SINTAB(JCOS+1)
        JCONJ = NTCOF-J
        ZVAL = CMPLX(ZETA+RCOS,RSIN)
        FVAL = CFUN(ZVAL)
        FVALRE = REAL(FVAL,kind=sp)
        FVALIM = AIMAG(FVAL)
        SUPPER = SUPPER+FVALRE
       R=CABS(FVAL)
       FMAX=AMAX1(FMAX,R)
        TCOF(J+1) = FVALRE
        TCOF(JCONJ+1) = FVALIM
        JREFL = NPREV-J
        JRCONJ = NTCOF-JREFL
        ZVAL = CMPLX(ZETA-RCOS,RSIN)
        FVAL = CFUN(ZVAL)
        FVALRE = REAL(FVAL,kind=sp)
        FVALIM = AIMAG(FVAL)
        SUPPER = SUPPER+FVALRE
       R=CABS(FVAL)
       FMAX=AMAX1(FMAX,R)
        TCOF(JREFL+1) = FVALRE
        TCOF(JRCONJ+1) = FVALIM
  360 CONTINUE
      APPROX = 0.5*APPROX+SUPPER/FLOAT(NPREV)
!C ***   STAGE THREE(C)   ***
!C --------------------------
!C CONVERGENCE AND TERMINATION CHECK.
      ERROR4 = ERROR3
      ERROR3 = ERROR2
      ERROR2 = ERROR1
      ERROR1 = ABS(APPROX-EXACT)
      EPRO = FMAX*SAFETY*EPMACH
      IF (EPRO.LT.EPCOF) GO TO 370
        EPCOF = EPRO
        NROUND = 2
        IF (NABORT.EQ.0) GO TO 370
        NCODE = 0
        EPEST = EPRO
        GO TO 470
  370 CONTINUE
      ERROR4 = AMAX1(ERROR4,EPRO)
      ERROR3 = AMAX1(ERROR3,EPRO)
      EP42 = ERROR2*((ERROR2/ERROR4)**(4.0/3.0))
      EP32 = ERROR2*((ERROR2/ERROR3)**2)
      EPMIN = AMIN1(ERROR2,EP32,EP42)
      EPEST = AMAX1(ERROR1,EPMIN,EPRO)
      IF (EPEST.GT.EPCOF) GO TO 380
        NCONV = 1
        GO TO 400
  380 CONTINUE
      IF (2*NTCOF.LE.NMAX) GO TO 300
        NCONV = -1
!C ***   STAGE FOUR(A)   ***
!C -------------------------
!C CALCULATION OF FIRST NTCOF TAYLOR COEFFICIENTS USING F.F.T.
  400 CONTINUE
      NCODE = NCONV*NROUND
      NDISP = NTCOF
  410 CONTINUE
      NDISP = NDISP/2
      CALL HFCOF (NTCOF,NDISP,TCOF,WORK,NTAB,SINTAB)
      IF (NDISP.GT.1) GO TO 430
      DO 420 J = 1,NTCOF
        TCOF(J) = WORK(J)
  420 CONTINUE
      GO TO 440
  430 CONTINUE
      NDISP = NDISP/2
      CALL HFCOF (NTCOF,NDISP,WORK,TCOF,NTAB,SINTAB)
      IF (NDISP.GT.1) GO TO 410
  440 CONTINUE
      SCALE = 1.0/FLOAT(NTCOF)
      DO 450 J = 1,NTCOF
        TCOF(J) = TCOF(J)*SCALE
        WORK(J) = TCOF(J)
  450 CONTINUE
!C ***   STAGE FOUR(B)   ***
!C -------------------------
!C SETTING OF REMAINING TAYLOR COEFFICIENTS.
      IF (NTCOF.GE.NMAX) GO TO 470
      NDOLIM = NTCOF+1
      DO 460 J = NDOLIM,NMAX
        TCOF(J) = 0.0
        WORK(J) = 0.0
  460 CONTINUE
  470 CONTINUE
      RETURN
!C END OF ENTCRE
      END
      
      SUBROUTINE HFCOF ( NTCOF, NDISP, TCOF, WORK, NTAB, SINTAB )
#if 0      
C
C ** HERMITIAN FOURIER COEFFICIENTS **
C
C     **  GENERAL PURPOSE  **
C THIS ROUTINE DOES ONE PASS OF A FAST FOURIER TRANSFORM.
C THE INDEXING IS ARRANGED SO THAT THE COEFFICIENTS ARE IN
C ORDER AT THE END OF THE LAST PASS.  THIS INDEXING REQUIRES
C THE USE OF SEPARATE ARRAYS FOR INPUT AND OUTPUT OF THE
C PARTIAL RESULTS.  THIS ROUTINE IS CALLED ONCE FOR EACH PASS.
C
C     **  INPUT PARAMETERS  **
C  (1)  NTCOF    NUMBER OF COEFFICIENTS TO BE PROCESSED.
C  (2)  NDISP    MAXIMUM VALUE OF DISPLACEMENT INDEX.
C  (3)  TCOF     (REAL(kind=sp)) INPUT ARRAY.
C  (5)  NTAB     NUMBER OF ENTRIES IN SINTAB.
C  (6)  SINTAB   (REAL(kind=sp)) TABLE OF VALUES OF SINE.
C                SINTAB(J+1)=SIN(PI*J/2*NTAB), J=0,1,2...NTAB-1
C
C     **  OUTPUT PARAMETERS  **
C  (4)  WORK     (REAL(kind=sp)) OUTPUT ARRAY.
C
C     **  INDEXING OF ARRAYS  **
C THE TWO POINT FOURIER TRANSFORM IS APPLIED TO THE POINTS
C OF TCOF WITH INDICES
C     JDISP*NPREV+JREPL  AND  JDISP*NPREV+JREPL+NHALF
C THE RESULTS ARE MODIFIED BY THE APPROPRIATE TWIDDLE FACTOR
C AND STORED IN WORK WITH INDICES
C     JDISP*NNEXT+JREPL  AND  JDISP*NNEXT+JREPL+NPREV
C  WHERE
C     NDISP      PRODUCT OF REMAINING FACTORS.
C     NPREV      PRODUCT OF PREVIOUS FACTORS.
C     NNEXT      PRODUCT OF PREVIOUS AND CURRENT FACTORS.
C     NHALF      PRODUCT OF PREVIOUS AND REMAINING FACTORS.
C     JREPL      REPLICATION INDEX = 1,2,...NPREV.
C     JDISP      HERMITIAN SYMMETRY IN THIS INDEX RESULTS IN
C                THREE CASES.
C                1)  INITIAL POINT - JDISP=0. INPUT POINTS
C                ARE PURELY REAL(kind=sp) AND OUTPUT POINTS ARE
C                PURELY REAL(kind=sp).
C                2)  MIDDLE POINT - JDISP=NDISP/2 - NOT
C                ALWAYS PRESENT.  INPUT POINTS ARE COMPLEX(kind=sp) AND
C                OUTPUT POINTS ARE PURELY REAL(kind=sp).
C                3)  INTERMEDIATE POINTS - JDISP=1,2,..(NDISP/2-1)
C                - NOT ALWAYS PRESENT.  INPUT POINTS ARE
C                COMPLEX(kind=sp) AND OUTPUT POINTS ARE COMPLEX(kind=sp).
C
C ON INPUT, THE HERMITIAN SYMMETRY IS IN A BLOCK OF LENGTH
C 2*NDISP, I.E. THE POINT CONJUGATE TO JDISP IS 2*NDISP-JDISP.
C ON OUTPUT, THE HERMITIAN SYMMETRY IS IN A BLOCK OF LENGTH
C NDISP, I.E. THE POINT CONJUGATE TO JDISP IS NDISP-JDISP.
C A HERMITIAN SYMMETRIC BLOCK HAS REAL(kind=sp) PARTS AT THE FRONT
C IMAGINARY PARTS (WHEN THEY EXIST) AT THE CONJUGATE
C POSITIONS AT THE BACK.
C
C THE TWIDDLE FACTOR CEXP(-PI*EYE*J/NDISP), J=1,2,..(NDISP/2-1)
C IS OBTAINED AS SEPARATE REAL(kind=sp) AND IMAGINARY PARTS FROM
C THE SINTAB TABLE.  THE IMAGINARY PART SIN(PI*J/NDISP) IS
C FOUND AT A SPACING OF NSPACE=2*NTAB/NDISP IN SINTAB.
C THE REAL(kind=sp) PART IS FOUND AT A CONJUGATE POSITION IN THE TABLE.
C
#endif
      INTEGER(kind=i4) NTCOF,NDISP,NTAB
      REAL(kind=sp) TCOF (*), WORK (*), SINTAB (*)
      REAL(kind=sp) CS,IS,IU,I0,I1,RS,RU,R0,R1,SN
      INTEGER(kind=i4) JCONJ,JCOS,JDISP,JREPL,JSIN,JT,JTC,JW,JWC,KT0,KT1
      INTEGER(kind=i4) KT2,KT3,KW0,KW1,KW2,KW3,NHALF,NMIDL,NNEXT,NPREV,NSPACE
      NHALF = NTCOF/2
      NPREV = NTCOF/(2*NDISP)
      NNEXT = NTCOF/NDISP
      NMIDL = (NDISP-1)/2
      NSPACE = (2*NTAB)/NDISP
!C INITIAL POINTS OF BLOCKS.
      DO 100 JREPL = 1,NPREV
        KT0 = JREPL
        KT1 = KT0+NHALF
        KW0 = JREPL
        KW1 = KW0+NPREV
        R0 = TCOF(KT0)
        R1 = TCOF(KT1)
        WORK(KW0) = R0+R1
        WORK(KW1) = R0-R1
  100 CONTINUE
!C INTERMEDIATE POINTS OF BLOCKS.
      IF (NMIDL.LT.1) GO TO 400
      DO 300 JDISP = 1,NMIDL
        JCONJ = NDISP-JDISP
        JSIN = JDISP*NSPACE
        JCOS = NTAB-JSIN
        SN = SINTAB(JSIN+1)
        CS = SINTAB(JCOS+1)
        JT = JDISP*NPREV
        JTC = JCONJ*NPREV
        JW = JDISP*NNEXT
        JWC = JCONJ*NNEXT
        DO 200 JREPL = 1,NPREV
          KT0 = JT+JREPL
          KT1 = KT0+NHALF
          KT2 = JTC+JREPL
          KT3 = KT2+NHALF
          KW0 = JW+JREPL
          KW1 = KW0+NPREV
          KW2 = JWC+JREPL
          KW3 = KW2+NPREV
          R0 = TCOF(KT0)
          I0 = TCOF(KT3)
          R1 = TCOF(KT2)
          I1 = -TCOF(KT1)
          RS = R0+R1
          IS = I0+I1
          RU = R0-R1
          IU = I0-I1
          WORK(KW0) = RS
          WORK(KW2) = IS
          WORK(KW1) = RU*CS+IU*SN
          WORK(KW3) = IU*CS-RU*SN
  200   CONTINUE
  300 CONTINUE
  400 CONTINUE
!C MIDDLE POINTS OF BLOCKS.
      IF (NDISP.LE.1) GO TO 600
      JT = (NDISP/2)*NPREV
      JW = (NDISP/2)*NNEXT
      DO 500 JREPL = 1,NPREV
        KT0 = JT+JREPL
        KT1 = KT0+NHALF
        KW0 = JW+JREPL
        KW1 = KW0+NPREV
        R0 = TCOF(KT0)
        I0 = TCOF(KT1)
        WORK(KW0) = 2.0*R0
        WORK(KW1) = 2.0*I0
  500 CONTINUE
  600 CONTINUE
      RETURN
!C END OF HFCOF
      END
      
      SUBROUTINE ENTCAF ( CFUN, ZETA, RCIRC, EPREQ, EPMACH, NMAX, NCODE, &
      EPEST, NTCOF, TCOF, WORK, NTAB, EXPTAB )
#if 0      
C
C ** EVALUATION OF NORMALIZED TAYLOR COEFFICIENTS **
C **           OF AN ANALYTIC FUNCTION            **
C
C     **  GENERAL PURPOSE  **
C THIS ROUTINE EVALUATES A SET OF NORMALIZED TAYLOR COEFFICIENTS
C TCOF(J+1) = (RCIRC**J) * (J-TH DERIVATIVE OF CFUN(Z) AT Z=ZETA)
C DIVIDED BY FACTORIAL(J) ... J = 0,1,2,3...NMAX-1.
C TO A UNIFORM ABSOLUTE ACCURACY **EPEST** USING FUNCTION
C VALUES OF CFUN(Z) AT POINTS IN THE COMPLEX(kind=sp) PLANE LYING ON
C THE CIRCLE OF RADIUS **RCIRC** WITH CENTER AT Z = ZETA.
C
C     ** THEORETICAL RESTRICTIONS **
C RCIRC MUST BE SMALLER THAN THE RADIUS OF CONVERGENCE OF
C THE TAYLOR SERIES.  THE PROBLEM HAS TO BE REFORMULATED
C SHOULD CFUN(Z) HAPPEN TO BE AN ODD FUNCTION OF (Z - ZETA),
C THAT IS IF THE RELATION **-CFUN(-(Z-ZETA))=CFUN(Z-ZETA)**
C IS AN IDENTITY.
C
C     ** REQUIREMENTS FOR CALLING PROGRAM **
C CALLING PROGRAM MUST CONTAIN CONTROL STATEMENTS DESCRIBED
C IN NOTES (3) AND (4) BELOW.  IT MUST ALSO ASSIGN VALUES TO
C INPUT PARAMETERS.  THE ROUTINE REQUIRES TWO SUBPROGRAMS,
C CFCOF (LISTED AFTER ENTCAF) AND CFUN (SEE NOTE(4) BELOW).
C
C     **INPUT PARAMETERS**
C  (1) CFUN   NAME OF COMPLEX(kind=sp) FUNCTION SUBPROGRAM.
C  (2) ZETA   COMPLEX(kind=sp) POINT ABOUT WHICH TAYLOR EXPANSION
C             IS REQUIRED.
C  (3) RCIRC  RADIUS (REAL(kind=sp))
C  (4) EPREQ  THE ABSOLUTE ACCURACY (REAL(kind=sp)) TO WHICH THE
C             NORMALIZED TAYLOR COEFFICIENTS, TCOF(J), ARE REQUIRED
C  (5) EPMACH THE MACHINE ACCURACY PARAMETER (REAL(kind=sp)) (OR AN
C             UPPER BOUND ON THE RELATIVE ACCURACY OF
C             QUANTITIES LIKELY TO BE ENCOUNTERED).
C  (6) NMAX   PHYSICAL UPPER LIMIT ON THE SIZE AND LENGTH OF
C             THE CALCULATION.  THE MAXIMUM NUMBER OF
C             COEFFICIENTS CALCULATED WILL BE THAT POWER OF
C             TWO LESS THAN OR EQUAL TO NMAX.  NMAX IS
C             ASSUMED TO BE AT LEAST 4. (SEE NOTE(3) BELOW.)
C  (7) NCODE  .GE.0 THE ROUTINE WILL DO AS WELL AS IT CAN.
C             .LT.0 THE ROUTINE WILL ABORT AT AN EARLY
C             STAGE IF THE REQUIRED ACCURACY CANNOT BE
C             ATTAINED BECAUSE OF ROUND OFF ERROR.
C (12) NTAB   IN NORMAL RUNNING, NTAB SHOULD BE SET TO ZERO
C             BEFORE THE FIRST CALL TO ENTCAF, BUT LEFT ALONE
C             AFTER THAT. (FOR MORE SOPHISTICATED USE, SEE
C             OUTPUT PARAMETERS (12) AND (13) AND NOTE(2)
C             BELOW.)
C
C     ** OUTPUT PARAMETERS **
C  (1),(2),(3),(4),(5),(6) IDENTICAL WITH INPUT VALUES.
C  (7) NCODE  RESULT STATUS INDICATOR.  TAKES ONE OF FIVE
C             VALUES AS FOLLOWS,
C             =+1. CONVERGED NORMALLY.
C             =-1. DID NOT CONVERGE. NO ROUND OFF ERROR TROUBLE
C             =+2. CONVERGED, BUT WITH A HIGHER TOLERANCE SET
C             BY THE ROUND OFF LEVEL. (EPEST.GT.EPREQ)
C             =-2. DID NOT CONVERGE IN SPITE OF HIGHER
C             TOLERANCE SET BY ROUND OFF LEVEL.
C             = 0. RUN WAS ABORTED BECAUSE EPREQ IS
C             UNATTAINABLE DUE TO ROUND OFF LEVEL AND INPUT
C             NCODE IS NEGATIVE.
C  (8) EPEST  ESTIMATE OF ACTUAL UNIFORM ABSOLUTE ACCURACY
C             IN ALL TCOF. EXCEPT IF NCODE.EQ.0 ESTIMATE OF
C             ROUND OFF LEVEL.
C  (9) NTCOF  NUMBER OF NONTRIVIAL VALUES OF TCOF ACTUALLY
C             CALCULATED.  THEY ARE BASED ON NTCOF+1 CALLS
C             OF CFUN.
C (10) TCOF   COMPLEX(kind=sp) DIMENSION (DIM).  APPROXIMATIONS TO
C             THE NORMALIZED TAYLOR COEFFICIENTS, EXCEPT WHEN
C             OUTPUT NCODE = 0. (SEE NOTE(3) BELOW.)
C (11) WORK   INTERNAL WORKING AREA OF COMPLEX(kind=sp) DIMENSION (DIM).
C             (SEE NOTE(3) BELOW). CONTENTS IS IDENTICAL
C             WITH THAT OF TCOF.
C (12) EXPTAB COMPLEX(kind=sp) DIMENSION (DIM/2). (SEE NOTES (2) AND
C             (3) BELOW.)  EXPTAB(J+1) = CEXP(PI*EYE*J/NTAB)
C             J = 0,1,2,...,NTAB-1.   (A HALF CYCLE)
C             OTHER LOCATIONS ARE EMPTY.
C
C     ** NOTES ON INPUT/OUTPUT PARAMETERS **
C NOTE(1)** NCODE IS USED BOTH AS INPUT AND OUTPUT PARAMETER.
C  NORMALLY IT RETAINS THE VALUE +1 AND NEED NOT BE RESET
C  BETWEEN NORMAL RUNS.
C NOTE(2)** THE APPEARANCE OF NTAB AND EXPTAB IN THE CALLING
C  SEQUENCE ALLOWS THE USER TO MAKE USE OF - OR TO PRECOMPUTE -
C  THESE NUMBERS IN ANOTHER PART OF THE PROGRAM SHOULD HE
C  SO DESIRE.  NTAB MUST BE A POWER OF TWO OR 0.
C NOTE(3)** THE APPEARANCE OF NMAX, TCOF, WORK, AND EXPTAB
C  IN THE CALLING SEQUENCE ALLOWS THE SCOPE OF THE SUBPROGRAM
C  AND THE AMOUNT OF STORAGE TO BE ASSIGNED BY THE CALLING
C  PROGRAM, WHICH SHOULD CONTAIN A CONTROL STATEMENT TO THE
C  FOLLOWING EFFECT
C  COMPLEX(kind=sp) TCOF(DIM), WORK(DIM), EXPTAB(DIM/2)
C  WHERE DIM IS NORMALLY A POWER OF TWO.  NMAX IS NORMALLY
C  EQUAL TO DIM, BUT MAY BE LESS THAN DIM.
C NOTE(4)** CFUN(Z) IS A USER PROVIDED COMPLEX(kind=sp) VALUED
C  FUNCTION SUBPROGRAM WITH A COMPLEX(kind=sp) VALUED ARGUMENT.  THE
C  CALLING PROGRAM MUST CONTAIN CONTROL STATEMENTS AS FOLLOWS
C  EXTERNAL CFUN
C  COMPLEX(kind=sp) CFUN
C
C     ** BOOKKEEPING PARAMETERS FOR STAGE ONE **
C  NCONV   1  CONVERGENCE ACHIEVED.
C         -1  NO CONVERGENCE ACHIEVED.
C  NROUND  1  NO ROUND OFF TROUBLE OBSERVED.
C          2  ROUND OFF TROUBLE OBSERVED.
C  NABORT  0  UPDATE TOLERANCE AND CONTINUE ON APPEARANCE OF
C             ROUND OFF TROUBLE.
C          1  TERMINATE WHEN ROUND OFF TROUBLE OBSERVED.
C  EXACT   THE EXACT VALUE OF TCOF(1) WHICH IS CFUN(ZETA).
C  SAFETY  THIS IS A SAFETY FACTOR BY WHICH THE ROUTINE AVOIDS
C      THE ROUND OFF LEVEL.  IT IS SET TO 10.0 AND APPEARS
C      ONLY IN THE COMBINATION (SAFETY*EPMACH).  TO ALTER THIS
C      FACTOR, OR TO REMOVE THE ROUND OFF ERROR GUARD
C      COMPLETELY, THE USER NEED ONLY ADJUST THE INPUT
C      PARAMETER EPMACH APPROPRIATELY.
C
C     ** QUANTITIES CALCULATED IN STAGE THREE(A) **
C  THIS IS THE FIRST PART OF ITERATION NUMBER NTCOF. PRESENTLY
C  AVAILABLE ARE  EXPTAB(J+1) = CEXP(PI*EYE*J/NTAB),
C  J = 0,1,2,...NTAB-1.
C  WE REQUIRE THE SEQUENCE  CEXP(PI*EYE*J/NTCOF/2)),
C  J= 1,3,5,...(NTCOF/2-1).
C  IF (NTCOF.LE.2*NTAB) THESE NUMBERS ARE ALREADY AVAILABLE
C  IN THE EXPTAB TABLE SPACED AT AN INTERVAL  2*NSPACE = 4*NTAB/NTCOF.
C  OTHERWISE, NTCOF = 4*NTAB AND THE EXPTAB TABLE IS UPDATED.
C  THIS INVOLVES REARRANGING THE NTAB VALUES AVAILABLE,
C  CALCULATING AND STORING NTAB NEW VALUES AND UPDATING
C  NTAB TO 2*NTAB.
C
C     ** QUANTITIES CALCULATED IN STAGE THREE(B) **
C  ITERATIONS ARE NUMBERED 4,8,16,... AT THE END OF
C  ITERATION NUMBER NTCOF, THE NTCOF COMPLEX(kind=sp) FUNCTION
C  VALUES AT ABCISSAS REGULARLY SPACED ON CIRCLE ARE STORED
C  IN THE TCOF VECTOR AS FOLLOWS
C   TCOF(J+1) = CFUN(Z(J))  J=0,1,2,...,NTCOF-1
C  WHERE
C   Z(J) = ZETA + RCIRC*CEXP(2*PI*EYE*J/NTCOF)
C  THIS INVLOVES A REARRANGEMENT OF THE NTCOF/2 FUNCTION
C  VALUES AVAILABLE AT THE START OF THE ITERATION AND THE
C  CALCULATION OF A FURTHER NTCOF/2 FUNCTION VALUES.  IN
C  ADDITION FMAX AND APPROX ARE CALCULATED.  THESE ARE
C   FMAX   MAXIMUM MODULUS OF THE FUNCTION VALUES SO FAR
C          ENCOUNTERED.
C   APPROX AN APPROXIMATION TO TCOF(1) BASED ON THESE
C          FUNCTION VALUES.
C
C     ** QUANTITIES CALCULATED AT STAGE THREE(C) **
C ERROR1 CURRENT VALUE OF THE ERROR = CABS(APPROX-EXACT).
C ERROR2, ERROR3, ERROR4 VALUES OF ERROR AT END OF THREE
C        PREVIOUS ITERATIONS.
C EPMACH MACHINE ACCURACY PARAMETER. (INPUT PARAMETER)
C EPREQ  REQUIRED ACCURACY. (INPUT PARAMETER)
C EPRO   HIGHEST  ACCURACY REASONABLY ATTAINABLE IN VIEW OF
C        THE SIZE OF THE FUNCTION VALUES SO FAR ENCOUNTERED.
C        (=10.0*EPMACH*FMAX)
C EPCOF  CURRENTLY REQUIRED ACCURACY (=AMAX1(EPREQ,EPRO)).
C EPEST  ESTIMATE OF CURRENT ACCURACY. (THE MAXIMUM OF EPRO AND
C        A FUNCTION OF ERRORS 1,2,3 AND 4. (OUTPUT PARAMETER)
C
C     ** CONVERGENCE AND TERMINATION CHECKS IN STAGE THREE(C) **
C (1) USES FMAX TO RAISE EPCOF ABOVE ROUND OFF LEVEL.
C  IF THIS NECESSARY AND THE INPUT VALUE OF NCODE IS NEGATIVE,
C  IT TERMINATES SETTING NCODE=0.
C (2) USES APPROX TO EVALUATE CONVERGENCE OF TCOF(1) TOWARDS
C  EXACT.  IT MAY ASSIGN CONVERGENCE AND GO TO STAGE FOUR(A)
C  SETTING NCODE=+1 OR +2. (CONVERGENCE IS NOT CHECKED FOR
C  FOUR OR FEWER POINTS).
C (3) USES NMAX TO CHECK PHYSICAL LIMIT.  IF THIS HAS BEEN
C  REACHED, IT GOES TO STAGE FOUR(A) SETTING NCODE=-1 OR -2.
C (4) OTHERWISE CONTINUES NEXT ITERATION BY GOING TO STAGE
C  THREE.
C
C **CALCULATION OF FIRST NTCOF TAYLOR COEFFICIENTS IN STAGE FOUR(A)
C  A VERSION OF THE FAST FOURIER TRANSFORM USING A WORK ARRAY
C IS USED.  THE ARRAY **WORK** IS USED ONLY DURING THIS STAGE.
C THE WORK ARRAY ALLOWS THE PERMUTING OF INDICES ASSOCIATED
C WITH IN-PLACE FFTS TO BE SUPPRESSED.  THE FFT CALCULATES
C THE NECCESSARY SUMMATIONS EXCEPT FOR DIVIDING BY NTCOF.
C
C **SETTING OF REMAINING TAYLOR COEFFICIENTS IN STAGE FOUR(B)
C  THE CONVERGENCE CRITERION ALLOWS US TO INFER THAT THE
C NORMALIZED TAYLOR COEFFICIENTS OF ORDER GREATER THAN NTCOF
C ARE ZERO TO ACCURACY EPEST.
C THEY ARE EVALUATED AS BEING EXACTLY ZERO.
C REAL(kind=sp) VARIABLE R IS USED TO PUT VALUE OF CABS(FVAL) AND USE IT IN
C AMAX1 FUNCTION CALL, SINCE FUNCTION CALL WITHIN A FUNCTION CALL
C IS NOT ALLOWED IN CDC FORTRAN.
#endif
      COMPLEX(kind=sp) CFUN
      COMPLEX(kind=sp) ZETA
      REAL(kind=sp) RCIRC,EPREQ,EPMACH,EPEST
      INTEGER(kind=i4) NMAX,NCODE,NTCOF,NTAB
      COMPLEX(kind=sp) TCOF (*), WORK (*), EXPTAB (*)
      INTEGER(kind=i4) NABORT,NCONV,NDISP,NDOLIM,NPREV,NROUND,NSPACE
      REAL(kind=sp) COSDIF,EPCOF,EPMIN,EPRO,EP32,EP42,ERROR1,ERROR2
      REAL(kind=sp) ERROR3,ERROR4,FMAX,SAFETY,SCALE,TWOPI
       REAL(kind=sp) R
      COMPLEX(kind=sp) APPROX,EXACT,FVAL,REXP,SUM,ZVAL
      INTEGER(kind=i4) J,JCONJ,JFROM,JTAB,JTO
      COMPLEX(kind=sp) CMPLX,CONJG
!C ***   STAGE ONE   ***
!C ---------------------
!C INITIALISE BOOKKEEPING PARAMETERS AND EXACT VALUE OF TCOF(1).
      NROUND = 1
      NABORT = 0
      IF (NCODE.LT.0) NABORT = 1
      EPCOF = EPREQ
      SAFETY = 10.0
      ZVAL = ZETA
      FVAL = CFUN(ZVAL)
      EXACT = FVAL
!C ***   STAGE TWO   ***
!C ---------------------
!C FIRST TWO ITERATIONS ( THOSE WITH NTCOF = 1,2 ).
      ERROR3 = 0.0
      ZVAL = ZETA+CMPLX(RCIRC,0.0)
      FVAL = CFUN(ZVAL)
      APPROX = FVAL
      FMAX = CABS(FVAL)
      TCOF(1) = FVAL
      ERROR2 = CABS(APPROX-EXACT)
      ZVAL = ZETA-CMPLX(RCIRC,0.0)
      FVAL = CFUN(ZVAL)
      APPROX = 0.5*(APPROX+FVAL)
       R=CABS(FVAL)
       FMAX=AMAX1(FMAX,R)
      TCOF(2) = FVAL
      ERROR1 = CABS(APPROX-EXACT)
      NTCOF = 2
!C ***   STAGE THREE   ***
!C -----------------------
!C COMMENCE ITERATION NUMBER NTCOF.
  300 CONTINUE
      NPREV = NTCOF
      NTCOF = 2*NTCOF
!C ***   STAGE THREE(A)   ***
!C --------------------------
!C UPDATE EXPTAB TABLE IF NECESSARY.
      IF (2*NTAB.GE.NTCOF) GO TO 340
      IF (NTAB.GE.2) GO TO 310
        EXPTAB(1) = (1.0,0.0)
        EXPTAB(2) = (0.0,1.0)
        NTAB = 2
        GO TO 340
  310 CONTINUE
      NDOLIM = NTAB-1
      DO 320 J = 1,NDOLIM
        JFROM = NTAB-J
        JTO = 2*JFROM
        EXPTAB(JTO+1) = EXPTAB(JFROM+1)
  320 CONTINUE
      NTAB = 2*NTAB
      TWOPI = 8.0*ATAN(1.0)
      COSDIF = COS(TWOPI/FLOAT(2*NTAB))
      NDOLIM = NTAB-3
      DO 330 J = 1,NDOLIM,2
        EXPTAB(J+1) = (0.5*EXPTAB(J)+0.5*EXPTAB(J+2))/COSDIF
  330 CONTINUE
      EXPTAB(NTAB) = (0.5*EXPTAB(NTAB-1)-(0.5,0.0))/COSDIF
  340 CONTINUE
!C ***   STAGE THREE(B)   ***
!C --------------------------
!C UPDATE LIST OF FUNCTION VALUES IN TCOF,
!C CALCULATE FMAX AND APPROX.
      NDOLIM = NPREV-1
      DO 350 J = 1,NDOLIM
        JFROM = NPREV-J
        JTO = 2*JFROM
        TCOF(JTO+1) = TCOF(JFROM+1)
  350 CONTINUE
      SUM = (0.0,0.0)
      NSPACE = (2*NTAB)/NTCOF
      DO 360 J = 1,NDOLIM,2
        JTAB = J*NSPACE
        REXP = RCIRC*EXPTAB(JTAB+1)
        ZVAL = ZETA+REXP
        FVAL = CFUN(ZVAL)
        SUM = SUM+FVAL
       R=CABS(FVAL)
       FMAX=AMAX1(FMAX,R)
        TCOF(J+1) = FVAL
        JCONJ = NTCOF-J
        ZVAL = ZETA+CONJG(REXP)
        FVAL = CFUN(ZVAL)
        SUM = SUM+FVAL
       R=CABS(FVAL)
       FMAX=AMAX1(FMAX,R)
        TCOF(JCONJ+1) = FVAL
  360 CONTINUE
      APPROX = 0.5*APPROX+SUM/FLOAT(NTCOF)
!C ***   STAGE THREE(C)   ***
!C --------------------------
!C CONVERGENCE AND TERMINATION CHECK.
      ERROR4 = ERROR3
      ERROR3 = ERROR2
      ERROR2 = ERROR1
      ERROR1 = CABS(APPROX-EXACT)
      EPRO = FMAX*SAFETY*EPMACH
      IF (EPRO.LT.EPCOF) GO TO 370
        EPCOF = EPRO
        NROUND = 2
        IF (NABORT.EQ.0) GO TO 370
        NCODE = 0
        EPEST = EPRO
        GO TO 470
  370 CONTINUE
      IF (NTCOF.LE.4) GO TO 380
      ERROR4 = AMAX1(ERROR4,EPRO)
      ERROR3 = AMAX1(ERROR3,EPRO)
      EP42 = ERROR2*((ERROR2/ERROR4)**(4.0/3.0))
      EP32 = ERROR2*((ERROR2/ERROR3)**2)
      EPMIN = AMIN1(ERROR2,EP32,EP42)
      EPEST = AMAX1(ERROR1,EPMIN,EPRO)
      IF (EPEST.GT.EPCOF) GO TO 380
        NCONV = 1
        GO TO 400
  380 CONTINUE
      IF (2*NTCOF.LE.NMAX) GO TO 300
        NCONV = -1
!C ***   STAGE FOUR(A)   ***
!C -------------------------
!C CALCULATION OF FIRST NTCOF TAYLOR COEFFICIENTS USING F.F.T.
  400 CONTINUE
      NCODE = NCONV*NROUND
      NDISP = NTCOF
  410 CONTINUE
      NDISP = NDISP/2
      CALL CFCOF (NTCOF,NDISP,TCOF,WORK,NTAB,EXPTAB)
      IF (NDISP.GT.1) GO TO 430
      DO 420 J = 1,NTCOF
        TCOF(J) = WORK(J)
  420 CONTINUE
      GO TO 440
  430 CONTINUE
      NDISP = NDISP/2
      CALL CFCOF (NTCOF,NDISP,WORK,TCOF,NTAB,EXPTAB)
      IF (NDISP.GT.1) GO TO 410
  440 CONTINUE
      SCALE = 1.0/FLOAT(NTCOF)
      DO 450 J = 1,NTCOF
        TCOF(J) = TCOF(J)*SCALE
        WORK(J) = TCOF(J)
  450 CONTINUE
!C ***   STAGE FOUR(B)   ***
!C -------------------------
!C SETTING OF REMAINING TAYLOR COEFFICIENTS.
      IF (NTCOF.GE.NMAX) GO TO 470
      NDOLIM = NTCOF+1
      DO 460 J = NDOLIM,NMAX
        TCOF(J) = (0.0,0.0)
        WORK(J) = (0.0,0.0)
  460 CONTINUE
  470 CONTINUE
      RETURN
!C END OF ENTCAF
      END
      
      SUBROUTINE CFCOF ( NTCOF, NDISP, TCOF, WORK, NTAB, EXPTAB )
#if 0      
C
C ** COMPLEX(kind=sp) FOURIER COEFFICIENTS **
C     **  GENERAL PURPOSE  **
C THIS ROUTINE DOES ONE PASS OF A FAST FOURIER TRANSFORM.
C THE INDEXING IS ARRANGED SO THAT THE COEFFICIENTS ARE IN
C ORDER AT THE END OF THE LAST PASS.  THIS INDEXING REQUIRES
C THE USE OF SEPARATE ARRAYS FOR INPUT AND OUTPUT OF THE
C PARTIAL RESULTS.  THIS ROUTINE IS CALLED ONCE FOR
C EACH PASS.
C
C     **  INPUT PARAMETERS  **
C  (1)  NTCOF    NUMBER OF COEFFICIENTS TO BE PROCESSED.
C  (2)  NDISP    MAXIMUM VALUE OF DISPLACEMENT INDEX.
C  (3)  TCOF     (COMPLEX(kind=sp)) INPUT ARRAY.
C  (5)  NTAB     NUMBER OF ENTRIES IN EXPTAB.
C  (6)  EXPTAB   (COMPLEX(kind=sp)) TABLE OF VALUES OF COMPLEX(kind=sp) EXPONENTIAL.
C                EXPTAB(J+1) = CEXP(PI*EYE*J/NTAB),
C                J = 0,1,2,...,NTAB-1.
C
C     **  OUTPUT PARAMETERS  **
C  (4)  WORK     (COMPLEX(kind=sp)) OUTPUT ARRAY.
C
C     **  INDEXING OF ARRAYS  **
C THE TWO POINT FOURIER TRANSFORM IS APPLIED TO THE POINTS
C OF TCOF WITH INDICES
C  (JDISP-1)*NPREV+JREPL  AND  (JDISP-1)*NPREV+JREPL+NHALF.
C THE RESULTS ARE MODIFIED BY THE APPROPRIATE TWIDDLE FACTOR
C AND STORED IN WORK WITH INDICES
C  (JDISP-1)*NNEXT+JREPL  AND  (JDISP-1)*NNEXT+JREPL+NPREV
C WHERE
C     NDISP      PRODUCT OF REMAINING FACTORS.
C     NPREV      PRODUCT OF PREVIOUS FACTORS.
C     NNEXT      PRODUCT OF PREVIOUS AND CURRENT FACTORS.
C     NHALF      PRODUCT OF PREVIOUS AND REMAINING FACTORS.
C     JDISP      DISPLACEMENT INDEX = 1,2,...NDISP.
C     JREPL      REPLICATION INDEX = 1,2,...NPREV.
C
C THE TWIDDLE FACTOR CEXP(-PI*EYE*J/NDISP), J=0,1,99NDISP-1
C IS OBTAINED BY TAKING THE CONJUGATE OF ELEMENTS SPACED
C EVERY NSPACE=NTAB/NDISP OF EXPTAB.
#endif
      INTEGER(kind=i4) NTCOF,NDISP,NTAB
      COMPLEX(kind=sp) TCOF (*), WORK (*), EXPTAB (*)
      COMPLEX(kind=sp) CONJG
      COMPLEX(kind=sp) ROT,Z0,Z1
      INTEGER(kind=i4) J,JDISP,JREPL,JTAB,JT,JW
      INTEGER(kind=i4) KT0,KT1,KW0,KW1,NHALF,NNEXT,NPREV,NSPACE
      NHALF = NTCOF/2
      NPREV = NTCOF/(2*NDISP)
      NNEXT = NTCOF/NDISP
      NSPACE = NTAB/NDISP
      DO 200 JDISP = 1,NDISP
        J = JDISP-1
        JTAB = J*NSPACE
        ROT = CONJG(EXPTAB(JTAB+1))
        JT = J*NPREV
        JW = J*NNEXT
        DO 100 JREPL = 1,NPREV
          KT0 = JT+JREPL
          KT1 = KT0+NHALF
          KW0 = JW+JREPL
          KW1 = KW0+NPREV
          Z0 = TCOF(KT0)
          Z1 = TCOF(KT1)
          WORK(KW0) = Z0+Z1
          WORK(KW1) = (Z0-Z1)*ROT
  100   CONTINUE
  200 CONTINUE
      RETURN
!C END OF CFCOF
      END