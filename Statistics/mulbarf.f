      SUBROUTINE MULBARF( ZS,N,ID,C,LAG,ZMEAN,ZVARI,SD,AIC,DIC,IMIN,
     *                    AICM,SDMIN,BW1,BW2,A,B,G,H,E,AICB )
C
      INCLUDE 'timsac_f.h'
C
cc      PROGRAM  MULBAR                                                   
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
C     TIMSAC 78.2.2.                                                    
C     ___          _                  __                                
C     MULTIVARIATE BAYESIAN METHOD OF AR MODEL FITTING                  
C                                                                       
C     THIS PROGRAM DETERMINES MULTI-VARIATE AUTOREGRESSIVE MODELS BY A  
C     BAYESIAN PROCEDURE.  THE BASIC LEAST SQUARES ESTIMATES OF THE PARA
C     ARE OBTAINED BY THE HOUSEHOLDER TRANSFORMATION.                   
C                                                                       
C     THE STATISTIC AIC IS DEFINED BY                                   
C                                                                       
C            AIC  =  N * LOG( DET(SD) ) + 2 * (NUMBER OF PARAMETERS)    
C                                                                       
C       WHERE                                                           
C           N:    NUMBER OF DATA,                                       
C           SD:   ESTIMATE OF INNOVATION VARIANCE MATRIX                
C           DET:  DETERMINANT,                                          
C           K:    NUMBER OF FREE PARAMETERS.                            
C                                                                       
C     BAYESIAN WEIGHT OF THE M-TH ORDER MODEL IS DEFINED BY             
C         W(M)  = CONST * C(M) / (M+1)                                  
C     WHERE                                                             
C         CONST = NORMALIZING CONSTANT                                  
C         C(M)  = EXP( -0.5*AIC(M) ).                                   
C     THE BAYESIAN ESTIMATES OF PARTIAL AUTOREGRESSION COEFFICIENT MATRI
C     OF FORWARD AND BACKWARD MODELS ARE OBTAINED BY (M=1,...,LAG)      
C         G(M)  = G(M)*D(M)                                             
C         H(M)  = H(M)*D(M),                                            
C     WHERE THE ORIGINAL G(M) AND H(M) ARE THE (CONDITIONAL) MAXIMUM    
C     LIKELIHOOD ESTIMATES OF THE HIGHEST ORDER COEFFICIENT MATRICES OF 
C     FORWARD AND BACKWARD AR MODELS OF ORDER M AND D(M) IS DEFINED BY  
C         D(M)  = W(M) + ... + W(LAG).                                  
C                                                                       
C     THE EQUIVALENT NUMBER OF PARAMETERS FOR THE BAYESIAN MODEL IS     
C     DEFINED BY                                                        
C         EK = (D(1)**2 + ... + D(LAG)**2)*ID + ID*(ID+1)/2             
C     WHERE ID DENOTES DIMENSION OF THE PROCESS.                        
C                                                                       
C                                                                       
C       --------------------------------------------------------------- 
C       REFERENCES:                                                     
C          H.AKAIKE(1978), "A BAYESIAN EXTENSION OF THE MINIMUM AIC     
C          PROCEDURE OF AUTOREGRESSIVE MODEL FITTING.",  RESEARCH MEMO. 
C          NO. 126, THE INSTITUTE OF STATISTICAL MATHEMATICS; TOKYO.    
C                                                                       
C          G.KITAGAWA AND H.AKAIKE(1978), "A PROCEDURE FOR THE MODELING 
C          OF NON-STATIONARY TIME SERIES.",  ANN. INST. STATIST. MATH., 
C          30,B,351-363.                                                
C       --------------------------------------------------------------- 
C       THE FOLLOWING SUBROUTINES ARE DIRECTLY CALLED BY THIS PROGRAM:  
C             MRDATA                                                    
C             MREDCT                                                    
C             MARFIT                                                    
C       ----------------------------------------------------------------
C       INPUTS REQUIRED:                                                
C           MT:    INPUT DEVICE FOR ORIGINAL DATA (MT=5; CARD READER)   
C           LAG:   UPPER LIMIT OF AR-ORDER,  MUST BE LESS THAN 31       
C                                                                       
C-----  THE FOLLOWING INPUTS ARE REQUIRED AT SUBROUTINE MRDATA  -----   
C                                                                       
C           TITLE: SPECIFICATION OF DATA                                
C           N:     DATA LENGTH,  MUST BE LESS THAN OR EQUAL TO 1000     
C           ID:    DIMENSION OF VECTOR,  MUST BE LESS THAN 11           
C                       < ID * LAG  MUST BE LESS THAN 101 >             
C           IFM:   CONTROL FOR INPUT                                    
C           FORM:  INPUT DATA FORMAT SPECIFICATION STATEMENT            
C                  -- FOR EXAMPLE --     (8F10.5)                       
C           C(I):  CALIBRATION OF CHANNEL I (I=1,ID)                    
C           Z:     ORIGINAL DATA; Z(K,I) (K=1,N) REPRESENTS THE I-TH CHA
C                  RECORD                                               
C                                                                       
cc      !DEC$ ATTRIBUTES DLLEXPORT :: MULBARF
C
cxx      IMPLICIT  REAL * 8  ( A-H , O-Z )                                 
CC      REAL * 4  Z                                                       
cc      DIMENSION  Z(1500,10)                                             
cc      DIMENSION  X(200,100) , D(200)                                    
cc      DIMENSION  A(10,10,30) , B(10,10,30) , G(10,10,30) , H(10,10,30)  
cc      DIMENSION  E(10,10)                                               
cxx      DIMENSION  Z(N,ID), ZS(N,ID), C(ID)
cxx      DIMENSION  ZMEAN(ID), ZVARI(ID)
cxx      DIMENSION  X((LAG+1)*ID*2,(LAG+1)*ID)
cxx      DIMENSION  A(ID,ID,LAG), B(ID,ID,LAG), G(ID,ID,LAG), H(ID,ID,LAG)
cxx      DIMENSION  E(ID,ID)
C
cxx      DIMENSION  SD(LAG+1), AIC(LAG+1), DIC(LAG+1)
cxx      DIMENSION  BW1(LAG+1), BW2(LAG)
      INTEGER :: N, ID, LAG, IMIN
      REAL(8) :: ZS(N,ID), C(ID), ZMEAN(ID), ZVARI(ID), SD(LAG+1),
     1           AIC(LAG+1), DIC(LAG+1), AICM, SDMIN, BW1(LAG+1),
     2           BW2(LAG), A(ID,ID,LAG), B(ID,ID,LAG), G(ID,ID,LAG),
     3           H(ID,ID,LAG), E(ID,ID), AICB
      REAL(8) :: Z(N,ID), X((LAG+1)*ID*2,(LAG+1)*ID), EK
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
cc      MJ = 1500                                                         
cc      MJ1 = 200                                                         
cc      MJ2 = 10                                                          
      MJ = N
      MJ1 = (LAG+1)*ID*2
      MJ2 = ID
      IPR = 1                                                           
      IPR = 3                                                           
      IPR = 2                                                           
CC      READ( 5,1 )     MT                                                
cc      MT = 5
cc      OPEN( MT,FILE=IFLNAM,ERR=910,IOSTAT=IVAR,STATUS='OLD' )
cc      READ( 5,1 )     LAG                                               
cc      WRITE( 6,3 )                                                      
cc      WRITE( 6,4 )                                                      
cc      WRITE( 6,5 )     LAG , MT                                         
C                                                                       
C     --  ORIGINAL DATA LOADING AND MEANS DELETION  --                  
C                                                                       
cc      CALL  MRDATA( MT,MJ,Z,N,ID )                                      
      CALL MRDATA( ZS,Z,N,ID,C,ZMEAN,ZVARI )
cc      CLOSE( MT )
      N0 = 0                                                            
      NMK = N - LAG                                                     
      KSW = 0                                                           
C                                                                       
C     --  HOUSEHOLDER REDUCTION  --                                     
C                                                                       
cc      CALL  MREDCT( Z,D,NMK,N0,LAG,ID,MJ,MJ1,KSW,X )                    
      X(1:MJ1,1:(LAG+1)*ID) = 0.0D0
      CALL  MREDCT( Z,NMK,N0,LAG,ID,MJ,MJ1,KSW,X )                    
C                                                                       
C     --  AR-MODEL FITTING (BAYESIAN PROCEDURE)  --                     
C                                                                       
cc      CALL  MBYSAR( X,D,NMK,LAG,ID,KSW,IPR,MJ1,MJ2,A,B,G,H,E,AIC,EK )   
cxx      CALL  MBYSAR( X,NMK,LAG,ID,KSW,IPR,MJ1,MJ2,SD,AIC,DIC,
      CALL  MBYSAR( X,NMK,LAG,ID,KSW,MJ1,MJ2,SD,AIC,DIC,
     *              AICM,SDMIN,IMIN,BW1,BW2,A,B,G,H,E,AICB,EK )
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
      RETURN
cxx    1 FORMAT( 16I5 )                                                    
cxx    3 FORMAT( 1H ,'PROGRAM TIMSAC 78.2.2',/,'   EXPONENTIALLY WEIGHTED B
cxx     1AYESIAN AUTOREGRESSIVE MODEL FITTING;  MULTI-VARIATE CASE' )      
cxx    4 FORMAT( 1H ,'  < AUTOREGRESSIVE MODEL >',/,1H ,10X,'Z(I) = A(1)*Z(
cxx     1I-1) + A(2)*Z(I-2) + ... + A(II)*Z(I-II) + ... + A(M)*Z(I-M) + W(I
cxx     2)',/,'   WHERE',/,11X,'M:     ORDER OF THE MODEL',/,11X,'W(I):  ID
cxx     3-DIMENSIONAL GAUSSIAN WHITE NOISE WITH MEAN 0 AND VARIANCE',
cxx     4' MATRIX E(M).' )                                                          
cxx    5 FORMAT( 1H ,I4,'-TH ORDER BAYESIAN MODEL IS FITTED',/,1H ,2X,'ORIG
cxx     1INAL DATA INPUT DEVICE  MT =',I4 )                                
      E N D                                                             
