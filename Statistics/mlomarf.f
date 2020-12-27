      SUBROUTINE  MLOMARF( ZS,N,ID,C,LAG,NS0,KSW,K,ZMEAN,ZVARI,NF,NS,MS,
cxx     *                     AIC,MP,AICP,MF,AICF,A,E,LK0,LKE )
     *                     AIC,MP,AICP,MF,AICF,A,E,LK0,LKE,M )
C
      INCLUDE 'timsac_f.h'
C
cc      PROGRAM  MLOMAR                                                   
C.......................................................................
C.....PLANNED BY H.AKAIKE...............................................
C.....DESIGNED BY H.AKAIKE AND G.KITAGAWA...............................
C.....PROGRAMMED BY G.KITAGAWA AND F.TADA...............................
C.....ADDRESS: THE INSTITUTE OF STATISTICAL MATHEMATICS, 4-6-7 MINAMI-AZ
C..............MINATO-KU, TOKYO 106, JAPAN..............................
C.....DATE OF THE LATEST REVISION:  MAR. 6,1979.........................
C.......................................................................
C.....THIS PROGRAM WAS ORIGINALLY PUBLISHED IN "TIMSAC-78", BY H.AKAIKE,
C.....G.KITAGAWA, E.ARAHATA AND F.TADA, COMPUTER SCIENCE MONOGRAPHS, NO.
C.....THE INSTITUTE OF STATISTICAL MATHEMATICS, TOKYO, 1979.............
C.......................................................................
C     TIMSAC 78.3.3.                                                    
C     _                     __                 _            __          
C     MINIMUM AIC METHOD OF LOCALLY STATIONARY MULTIVARIATE AR MODEL FIT
C                                                                       
C     THIS PROGRAM LOCALLY FITS MULTI-VARIATE AUTOREGRESSIVE MODELS TO  
C     NON-STATIONARY TIME SERIES BY THE MINIMUM AIC PROCEDURE USING THE 
C     HOUSEHOLDER TRANSFORMATION.                                       
C                                                                       
C     BY THIS PROCEDURE, THE DATA OF LENGTH N ARE DIVIDED INTO J LOCALLY
C     STATIONARY SPANS                                                  
C                                                                       
C                <-- N1 --> <-- N2 --> <-- N3 -->          <-- NJ -->   
C               !----------!----------!----------!--------!----------!  
C                <-----------------------  N  ---------------------->   
C                                                                       
C     WHERE NI (I=1,...,J) DENOTES THE NUMBER OF BASIC SPANS, EACH OF   
C     LENGTH NS, WHICH CONSTITUTE THE I-TH LOCALLY STATIONARY SPAN.     
C     AT EACH LOCAL SPAN, THE PROCESS IS REPRESENTED BY A STATIONARY    
C     AUTOREGRESSIVE MODEL.                                             
C                                                                       
C                                                                       
C       --------------------------------------------------------------- 
C       REFERENCE:                                                      
C          G.KITAGAWA AND H.AKAIKE(1978), "A PROCEDURE FOR THE MODELING 
C          OF NON-STATIONARY TIME SERIES.",  ANN. INST. STATIST. MATH., 
C          30,B,351-363.                                                
C       --------------------------------------------------------------- 
C       THE FOLLOWING SUBROUTINES ARE DIRECTLY CALLED BY THIS PROGRAM:  
C             MRDATA                                                    
C             MNONST                                                    
C       --------------------------------------------------------------- 
C       INPUTS REQUIRED;                                                
C          MT:    INPUT DEVICE FOR ORIGINAL DATA (MT=5: CARD READER).   
C          LAG:   UPPER LIMIT OF THE ORDER OF AR-MODEL, MUST BE LESS THA
C                 OR EQUAL TO 50.                                       
C          NS:    LENGTH OF BASIC LOCAL SPAN.                           
C          KSW:   =0  CONSTANT VECTOR IS NOT INCLUDED AS A REGRESSOR    
C                 =1  CONSTANT VECTOR IS INCLUDED AS THE FIRST REGRESSOR
C                                                                       
C            -- THE FOLLOWING INPUTS ARE REQUESTED BY SUBROUTINE MRDATA 
C          TITLE: SPECIFICATION OF DATA                                 
C          N:     DATA LENGTH, MUST BE LESS THAN OR EQUAL TO 1000.      
C          ID:    DIMENSION OF DATA,  MUST BE LESS THAN 6               
C                       < ID*(LAG+1)+KSW MUST BE LESS THAN 101 >        
C          IFM:   INPUT FORMAT                                          
C          FORM:  INPUT DATA FORMAT SPECIFICATION STATEMENT.            
C                 -- EXAMPLE --     (8F10.5)                            
C          C(J):  CALIBRATION CONSTANT FOR CHANNEL J (J=1,ID)           
C          Z(I,J): ORIGINAL DATA                                        
C            -----------------------------------------------------------
C                                                                       
cc      !DEC$ ATTRIBUTES DLLEXPORT :: MLOMARF
C
cxx      IMPLICIT  REAL * 8  ( A-H , O-Z )                                 
CC      REAL * 4  Z                                                       
cc      DIMENSION  Z(1500,10)                                             
cc      DIMENSION  X(200,100) , U(100,100) , D(200)                       
cc      DIMENSION  A(5,5,50) , B(5,5,50) , E(5,5)                         
cxx      DIMENSION  ZS(N,ID), Z(N,ID), C(ID)
cxx      DIMENSION  ZMEAN(ID), ZVARI(ID)
cxx      DIMENSION  A(ID,ID,LAG,K) , B(ID,ID,LAG) , E(ID,ID,K)             
cxx      DIMENSION  NF(K), NS(K), MS(K), MP(K), MF(K)
cxx      DIMENSION  AIC(K), AICP(K), AICF(K)
cxx      DIMENSION  LK0(K), LKE(K)
cxxcx      DIMENSION  X(N,((LAG+1)*ID+KSW)*2)
cxx      DIMENSION  X(((LAG+1)*ID+KSW)*4,((LAG+1)*ID+KSW)*2)
cxx      DIMENSION  U(((LAG+1)*ID+KSW)*2,((LAG+1)*ID+KSW)*2)
      INTEGER :: N, ID, LAG, NS0, KSW, K, NF(K), NS(K), MS(K), MP(K),
     1           MF(K), LK0(K), LKE(K), M
      REAL(8) :: ZS(N,ID), C(ID), ZMEAN(ID), ZVARI(ID), AIC(K), AICP(K),
     1           AICF(K), A(ID,ID,LAG,K), E(ID,ID,K)
      REAL(8) :: Z(N,ID), B(ID,ID,LAG),
     1           X(((LAG+1)*ID+KSW)*4,((LAG+1)*ID+KSW)*2),
     2           U(((LAG+1)*ID+KSW)*2,((LAG+1)*ID+KSW)*2)
C
cc      CHARACTER(100)  IFLNAM,OFLNAM
cc      CALL FLNAM2( IFLNAM,OFLNAM,NFL )
cc      IF ( NFL.EQ.0 ) GO TO 999
cc      IF ( NFL.EQ.2 ) THEN
cc         OPEN( 6,FILE=OFLNAM,ERR=900,IOSTAT=IVAR )
cc      ELSE
cc         CALL SETWND
cc      END IF
C                                                                       
C       PARAMETERS:                                                     
C          MJ:    ABSOLUTE DIMENSION FOR SUBROUTINE CALL                
C          MJ1:   ABSOLUTE DIMENSION FOR SUBROUTINE CALL                
C          MJ2:   ABSOLUTE DIMENSION FOR SUBROUTINE CALL                
C          MJ3:   ABSOLUTE DIMENSION FOR SUBROUTINE CALL                
C                                                                       
cc      MJ = 1500                                                         
cc      MJ1 = 200                                                         
cc      MJ2 = 100                                                         
cc      MJ3 = 5                                                           
      MJ2 = ((LAG+1)*ID+KSW)*2
      MJ1 = MJ2*2
      MJ3 = ID
C
      NF(1:K) = 0
      NS(1:K) = 0
      MS(1:K) = 0
      AIC(1:K) = 0.0D0
      MP(1:K) = 0
      AICP(1:K) = 0.0D0
      MF(1:K) = 0
      AICF(1:K) = 0.0D0
      A(1:ID,1:ID,1:LAG,1:K) = 0.0D0
      E(1:ID,1:ID,1:K) = 0.0D0
      LK0(1:K) = 0
      LKE(1:K) = 0
      X(1:MJ1,1:MJ2) = 0.0D0
      U(1:MJ2,1:MJ2) = 0.0D0
C
CC      READ( 5,1 )     MT                                                
cc      MT = 5
cc      OPEN( MT,FILE=IFLNAM,ERR=910,IOSTAT=IVAR,STATUS='OLD' )
cc      READ( 5,1 )     LAG , NS , KSW                                    
C                                                                       
      NS(1) = NS0
cc      WRITE( 6,2 )                                                      
cc      WRITE( 6,4 )                                                      
cc      WRITE( 6,3 )     LAG , NS , MT                                    
C                                                                       
cc      CALL  MRDATA( MT,MJ,Z,N,ID )                                      
      CALL MRDATA( ZS,Z,N,ID,C,ZMEAN,ZVARI )
cc      CLOSE( MT )
C                                                                       
      L = 0                                                             
      KD = LAG * ID + KSW                                               
      MX = KD * 2                                                       
C                                                                       
C                                                                       
      IF = 0
      M = 0
      NF(1) = 0
  111 CONTINUE                                                          
cxx      M = M+1
      LK = L + LAG                                                      
      LK1 = LK + 1                                                      
      IF( LK1 .GE. N )     GO TO 300                                    
      M = M+1
cc      IF( N-LK1 .LE. NS )     NS = N - LK                               
cc      IF( N-LK1-NS .LT. MX )     NS = N - LK                            
      IF( M. NE. 1 )  THEN
         AICF(M) = AICF(M-1)
         NS(M) = NS(M-1)
         LK0(M) = LK0(M-1)
      END IF
      IF( N-LK1 .LE. NS(M) )     NS(M) = N - LK
      IF( N-LK1-NS(M) .LT. MX )     NS(M) = N - LK
C                                                                       
cc      CALL  MNONST( Z,X,U,D,KSW,LAG,L,NS,ID,IF,MJ,MJ1,MJ2,MJ3,A,B,E,MF, 
cc     *              AIC )                                               
      CALL MNONST( Z,X,U,KSW,LAG,L,NNF,NF(M),NS(M),ID,IF,N,MJ1,MJ2,MJ3,
     * A(1,1,1,M),B,E(1,1,M),MS(M),AIC(M),MP(M),AICP(M),MF(M),AICF(M) )
C                                                                      
cc      L = L + NS                                                        
cc      IF( IF .EQ. 2 )     LK0 = LK1                                     
      L = L + NS(M)
      IF( IF .EQ. 2 )     LK0(M) = LK1
C                                                                       
cc      LKE = LK + NS                                                     
      LKE(M) = LK + NS(M)
cc      WRITE( 6,13 )                                                     
cc      WRITE( 6,16 )                                                     
cc      WRITE( 6,14 )     LK0 , LKE                                       
cc      DO 10  I=1,MF                                                     
cc      WRITE( 6,16 )                                                     
cc      DO 10  II=1,ID                                                    
cc      WRITE( 6,15 )     (A(II,JJ,I),JJ=1,ID)                            
cc      IF( II .EQ. 1 )     WRITE( 6,17 )   I                             
cc      IF( II .NE. 1 )     WRITE( 6,21 )                                 
cc   10 CONTINUE                                                          
cc      WRITE( 6,16 )                                                     
cc      WRITE( 6,19 )     MF , AIC                                        
cc      WRITE( 6,16 )                                                     
cc      WRITE( 6,12 )                                                     
cc      DO 20  I=1,ID                                                     
cc      WRITE( 6,11 )     (E(I,J),J=1,ID)                                 
cc   20 WRITE( 6,17 )     I                                               
cc      WRITE( 6,16 )                                                     
cc      WRITE( 6,18 )                                                     
C                                                                       
      GO TO 111                                                         
  300 CONTINUE                                                          
cc      GO TO 999
C                                                                       
cc  900 CONTINUE
cc      WRITE(6,600) IVAR,OFLNAM
cc  600 FORMAT(/,' !!! Output_Data_File OPEN ERROR ',I8,//,5X,100A)
cc      GO TO 999
C
cc  910 CONTINUE
cc      IF ( NFL.EQ.2 ) CLOSE( 6 )
cc#ifdef __linux__
ccC	reopen #6 as stdout
cc      IF ( NFL.EQ.2 ) OPEN(6, FILE='/dev/fd/1')
cc#endif
ccC /* __linux__ */
cc      WRITE(6,610) IVAR,IFLNAM
cc  610 FORMAT(/,' !!! Input_Data_File OPEN ERROR ',I8,//,5X,100A)
C
cc  999 CONTINUE
cc      close(3)
      RETURN
cxx    1 FORMAT( 16I5 )                                                    
cxx    2 FORMAT( ///1H ,'PROGRAM TIMSAC 78.3.3',/,'   LOCALLY STATIONARY MU
cxx     1LTI-VARIATE AUTOREGRESSIVE MODEL FITTING;',//,'  < BASIC AUTOREGRE
cxx     2SSIVE MODEL >' )                                                  
cxx    3 FORMAT( ///1H ,'  FITTING UP TO THE ORDER  K =',I3,'  IS TRIED',/,
cxx     1'   BASIC LOCAL SPAN LENGTH  NS =',I4,/,'   ORIGINAL DATA INPUT DE
cxx     2VICE  MT =',I3 )                                                  
cxx    4 FORMAT( //1H ,10X,'Z(N) = A1*Z(N-1) + A2*Z(N-2) + ... + AK*Z(N-K) 
cxx     1+ W(N)',/,1H ,'  WHERE',/,11X,'K:     ORDER OF THE MODEL',/,11X,  
cxx     2'W(N):  INNOVATION' )                                             
cxx   11 FORMAT( 1H ,18X,5D15.5 )                                          
cxx   12 FORMAT( 1H ,10X,1H.,6X,'INNOVATION VARIANCE MATRIX',53X,1H. )     
cxx   13 FORMAT( 1H ,//11X,35(1H.),'  CURRENT MODEL  ',35(1H.) )           
cxx   14 FORMAT( 1H ,10X,1H.,6X,'M',7X,'AM(I,J)',30X,'DATA  Z(K,.); K=',I5,
cxx     11H,,I5,7X,1H. )                                                   
cxx   15 FORMAT( 1H ,18X,5F15.8 )                                          
cxx   16 FORMAT( 1H ,10X,1H.,85X,1H. )                                     
cxx   17 FORMAT( 1H+,10X,1H.,I7,78X,1H. )                                  
cxx   18 FORMAT( 1H ,10X,87(1H.) )                                         
cxx   19 FORMAT( 1H ,10X,1H.,6X,'ORDER =',I5,67X,1H.,/,11X,1H.,6X,'AIC =', 
cxx     1 F15.3,59X,1H. )                                                  
cxx   21 FORMAT( 1H+,10X,1H.,85X,1H. )                                     
      END                                                               
cc      SUBROUTINE  MNONST( Z,X,U,D,KSW,LAG,N0,NS,ID,IF,MJ,MJ1,MJ2,MJ3,   
cc     1                    A,B,E,MF,AICF )                               
      SUBROUTINE  MNONST( Z,X,U,KSW,LAG,N0,NNF,NF,NS,ID,IF,MJ,MJ1,MJ2,
     *                    MJ3,A,B,E,MS,AICFS,MP,AICP,MF,AICF )
C                                                                       
C     IN THIS SUBROUTINE THE FOLLOWING TWO MODELS ARE COMPARED AND      
C     THE MODEL WITH THE SMALLER AIC IS ACCEPTED AS THE CURRENT MODEL.  
C                                                                       
C       MOVING MODEL:      SUCCESSION OF TWO AR-MODELS INDEPENDENTLY FIT
C                          TO THE FORMER AND PRESENT BLOCK OF DATA      
C          NF:    DATA LENGTH OF THE PRECEDING STATIONARY BLOCK         
C          NS:    DATA LENGTH OF NEW BLOCK (A BASIC LOCAL SPAN)         
C          AR(MF,SDF):  MAICE AR-MODEL WITH THE ORDER MF AND INNOVATION 
C                       VARIANCE SDF FITTED TO THE PRECEDING STATIONARY 
C                       BLOCK                                           
C          AR(MS,SDS):  MAICE AR-MODEL (ORDER MS AND INNOVATION VARIANCE
C                       SDS) FITTED TO THE NEWLY OBTAINED DATA          
C                                                                       
C                                AR(MF,SDF)      AR(MS,SDS)             
C                             !---------------!---------------!         
C                              <---- NF -----> <----- NS ---->          
C                                                                       
C       CONSTANT MODEL:    AR MODEL FITTED TO THE POOLED DATA OF THE FOR
C                          AN PRESENT BLOCK                             
C          NP:    DATA LENGTH OF POOLED DATA                            
C          AR(MP,SDP):  MAICE AR-MODEL FITTED TO THE POOLED DATA        
C                       (ORDER=MP, INNOVATION VARIANCE=SDP)             
C                                                                       
C                                       AR(MP,SDP)                      
C                             !-------------------------------!         
C                              <----------- NP -------------->          
C       ----------------------------------------------------------------
C       THE FOLLOWING SUBROUTINES ARE DIRECTLY CALLED BY THIS SUBROUTINE
C             COPY                                                      
C             HUSHLD                                                    
C             MARFIT                                                    
C             MREDCT                                                    
C       ----------------------------------------------------------------
C                                                                       
C       INPUTS:                                                         
C          Z:      ORIGINAL DATA; Z(K,I) (K=1,N) REPRESENTS THE RECORD O
C                  THE I-TH CHANNEL                                     
C          X:      WORKING AREA                                         
C          U:      WORKING AREA                                         
C          D:      WORKING AREA                                         
C          KSW:    =0   CONSTANT VECTOR IS NOT INCLUDED AS A REGRESSOR  
C                  =1   CONSTANT VECTOR IS INCLUDED AS THE FIRST REGRESS
C          LAG:    UPPER LIMIT OF THE ORDER OF AR MODEL                 
C          N0:     INDEX OF THE END POINT OF THE FORMER SPAN            
C          NS:     LENGTH OF BASIC LOCAL SPAN                           
C          ID:     DIMENSION OF DATA                                    
C          IF:                                                          
C          MJ:     ABSOLUTE DIMENSION OF Z IN THE MAIN PROGRAM          
C          MJ1:    ABSOLUTE DIMENSION OF X IN THE MAIN PROGRAM          
C          MJ2:    ABSOLUTE DIMENSION OF U IN THE MAIN PROGRAM          
C          MJ3:    ABSOLUTE DIMENSION OF A IN THE MAIN PROGRAM          
C          B:      WORKING AREA                                         
C                                                                       
C       OUTPUTS:                                                        
C          A:      AR COEFFICIENT MATRICES OF THE CURRENT MODEL         
C          MF:     ORDER OF THE CURRENT MODEL                           
C          AICF:   AIC OF THE CURRENT MODEL                             
C                                                                       
C                                                                       
cxx      IMPLICIT  REAL * 8  ( A-H , O-Z )                                 
CC      REAL * 4  Z                                                       
cc      DIMENSION  X(MJ1,1) , U(MJ2,1) , D(1)                             
cx      DIMENSION  Z(MJ,1)                                                
cxx      DIMENSION  Z(MJ,ID)                                                
cxx      DIMENSION  X(MJ1,MJ2) , U(MJ2,MJ2)
cxx      DIMENSION  A(ID,ID,LAG) , B(ID,ID,LAG) , E(ID,ID)                 
cxx      DIMENSION  AI(ID,ID,LAG), BI(ID,ID,LAG), EI(ID,ID) 
cc      DIMENSION  Y(100,100)                                             
cc      DIMENSION  C(10) , EX(10)                                         
cxx      DIMENSION  C(ID) , EX(ID)
cxx      DIMENSION  AIC(LAG+1,ID), SD(LAG+1,ID), DIC(LAG+1,ID)
cxx      DIMENSION  AICM(ID), SDM(ID), M(ID)
cxx      DIMENSION  JNDF(MJ2,ID), AF(MJ2,ID), NPR(ID), AAIC(ID)
      INTEGER :: KSW, LAG, N0, NNF, NF, NS, ID, IF, MJ, MJ1, MJ2,
     1           MJ3, MS, MP, MF
      REAL(8) :: Z(MJ,ID), X(MJ1,MJ2), U(MJ2,MJ2), A(ID,ID,LAG),
     1           B(ID,ID,LAG), E(ID,ID), AICFS, AICP, AICF
      INTEGER :: M(ID), JNDF(MJ2,ID), NPR(ID)
      REAL(8) :: AI(ID,ID,LAG), BI(ID,ID,LAG), EI(ID,ID), C(ID), EX(ID),
     1           AIC(LAG+1,ID), SD(LAG+1,ID), DIC(LAG+1,ID), AICM(ID),
     2           SDM(ID), AF(MJ2,ID), AAIC(ID), AICS
C                                                                       
C                                                                       
cc      MJ4 = 50                                                          
      MJ4 = LAG
      IPR = 0
cx      IFG = 0
C                                                                       
      K1 = LAG + 1                                                      
      KD1 = K1*ID + KSW                                                 
      KD2 = KD1 * 2                                                     
C                                                                       
C       HOUSEHOLDER'S REDUCTION                                         
C                                                                       
cc      CALL  MREDCT( Z,D,NS,N0,LAG,ID,MJ,MJ1,KSW,X )                     
      CALL  MREDCT( Z,NS,N0,LAG,ID,MJ,MJ1,KSW,X )
C                                                                       
C                                                                       
C       AR-MODEL FITTING BY THE MINIMUM AIC PROCEDURE                   
C                                                                       
cc      CALL  MARFIT( X,Y,D,NS,ID,LAG,KSW,MJ1,MJ3,MJ4,MJ2,0,IPR,B,E,EX,C, 
cc     *             MS,AICS )                                            
cxx      CALL  MARFIT( X,NS,ID,LAG,KSW,MJ1,MJ3,MJ4,MJ2,0,IPR,AIC,SD,DIC,
      CALL  MARFIT( X,NS,ID,LAG,KSW,MJ1,MJ3,MJ4,KD1,0,IPR,AIC,SD,DIC,
cx     *AICM,SDM,M,BI,EI,B,E,EX,C,MS,AICS,JNDF,AF,NPR,AAIC,IFG,LU )
     *AICM,SDM,M,BI,EI,B,E,EX,C,MS,AICS,JNDF,AF,NPR,AAIC )
C                                                                       
      IF( IF .NE. 0 )     GO TO 10                                      
C                                                                       
      CALL  COPY( X,KD1,0,0,MJ1,MJ2,U )                                 
C                                                                       
cc      WRITE( 6,5 )     NS , MS , AICS                                   
      GO TO 20                                                          
C                                                                       
C                                                                       
cc   10 AIC = AICF + AICS                                                 
   10 AICFS = AICF + AICS                                               
C                                                                       
cc      WRITE( 6,4 )                                                      
cc      WRITE( 6,6 )     NF , NS , MS , AIC                               
      NF = NNF
C                                                                       
      CALL  COPY( X,KD1,0,KD2,MJ1,MJ1,X )                               
      CALL  COPY( U,KD1,0,KD1,MJ2,MJ1,X )                               
C                                                                       
C       ---  HOUSEHOLDER TRANSFORMATION  ---                            
C                                                                       
cc      CALL  HUSHLD( X,D,MJ1,KD2,KD1 )                                   
      CALL  HUSHLD( X,MJ1,KD2,KD1 )                                   
C                                                                       
C                                                                       
C                                                                       
C       ---  AR-MODEL FITTING FOR POOLED DATA  ---                      
C                                                                       
      NP = NNF + NS                                                     
cc      CALL  MARFIT( X,Y,D,NP,ID,LAG,KSW,MJ1,MJ3,MJ4,MJ2,0,IPR,A,E,EX,C, 
cc     *              MP,AICP )                                           
cxx      CALL  MARFIT( X,NP,ID,LAG,KSW,MJ1,MJ3,MJ4,MJ2,0,IPR,AIC,SD,DIC, 
      CALL  MARFIT( X,NP,ID,LAG,KSW,MJ1,MJ3,MJ4,KD1,0,IPR,AIC,SD,DIC, 
cx     *AICM,SDM,M,AI,EI,A,E,EX,C,MP,AICP,JNDF,AF,NPR,AAIC,IFG,LU )
     *AICM,SDM,M,AI,EI,A,E,EX,C,MP,AICP,JNDF,AF,NPR,AAIC )
C                                                                       
cc      WRITE( 6,7 )     NP , MP , AICP                                   
C                                                                       
cc      IF( AIC .GE. AICP )     GO TO 40                                  
      IF( AICFS .GE. AICP )     GO TO 40                                
C                                                                       
cc      WRITE( 6,8 )                                                      
C                                                                       
      CALL  COPY( X,KD1,KD2,0,MJ1,MJ2,U )                               
C                                                                       
   20 CONTINUE                                                          
      IF = 2                                                            
      NNF = NS                                                          
      MF = MS                                                           
      AICF = AICS                                                       
C                                                                       
cxx      DO 30  II=1,MF
cxx      DO 30  J=1,ID
      DO 32  II=1,MF                                                    
      DO 31  J=1,ID                                                     
      DO 30  I=1,ID                                                     
cxx   30 A(I,J,II) = B(I,J,II) 
      A(I,J,II) = B(I,J,II)                                            
   30 CONTINUE
   31 CONTINUE
   32 CONTINUE
      GO TO 50                                                          
C                                                                       
C                                                                       
C                                                                       
   40 CONTINUE                                                          
      IF = 1                                                            
      CALL  COPY( X,KD1,0,0,MJ1,MJ2,U )                                 
C                                                                       
cc      WRITE( 6,9 )                                                      
      NNF = NNF + NS                                                    
      MF = MP                                                           
      AICF = AICP                                                       
C                                                                       
C                                                                       
   50 CONTINUE                                                          
C                                                                       
C                                                                       
      RETURN                                                            
C                                                                       
cxx  600 FORMAT( 1H ,'N =',I5,5X,'ID =',I5,5X,'K =',I5,5X,'M =',I5,5X,     
cxx     U  'MT =',I5,5X,'DATA FORMAT =',10A4 )                             
cxx  601 FORMAT( 1H ,'-----  ORIGINAL DATA  -----' )                       
cxx  602 FORMAT( 1H ,9X,'I',10X,'MEAN',7X,'VARIANCE' )                     
cxx  610 FORMAT( 1H ,10D13.5 )                                             
cxx  620 FORMAT( 1H ,I10,2D15.7 )                                          
cxx    2 FORMAT( 20A4 )                                                    
cxx    3 FORMAT( 8F10.0 )                                                  
cxx    4 FORMAT( //1H ,'---  THE FOLLOWING TWO MODELS ARE COMPARED  ---' ) 
cxx    5 FORMAT( //1H ,'INITIAL LOCAL MODEL:   NS =',I5,5X,'MS =',I3,5X,   
cxx     1'AIC =',F16.3 )                                                   
cxx    6 FORMAT( 1H ,'MOVING MODEL:     (NF =',I5,', NS =',I4,')',5X,      
cxx     1 'MS =',I3,5X,'AIC =',F16.3 )                                     
cxx    7 FORMAT( 1H ,'CONSTANT MODEL:   (NP =',I5,')',15X,'MP =',I3,5X,'AIC
cxx     1 =',F16.3 )                                                       
cxx    8 FORMAT( //1H ,37(1H*),/,1H ,'*****',27X,'*****',/,1H ,'*****     N
cxx     1EW MODEL ADOPTED     *****',/,1H ,'*****',27X,'*****',/,1H ,37(1H*
cxx     2) )                                                               
cxx    9 FORMAT( 1H ,'*****  CONSTANT MODEL ADOPTED  *****' )              
cxx   19 FORMAT( 1H ,'*****',27X,'*****' )                                 
cxx   21 FORMAT( 1H ,37(1H*) )                                             
cxx   22 FORMAT( 1H ,// )                                                  
cxx   24 FORMAT( 1H ,'LK1 =',I5,5X,'M =',I5,/,1H ,130(1H*) )               
C                                                                       
      END                                                               
