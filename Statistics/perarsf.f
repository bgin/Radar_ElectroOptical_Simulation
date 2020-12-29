      SUBROUTINE PERARSF( ZS,N,IP,LAG,KSW,ZMEAN,SUM,NPR,JNDF,AF,AICF,
     *                    B,E,C,EX,LMAX)
C
      INCLUDE 'timsac_f.h'
C
cc      PROGRAM  PERARS                                                   
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
C     TIMSAC 78.2.3.                                                    
C     ___      _   _                _                                   
C     PERIODIC AUTOREGRESSION FOR A SCALAR TIME SERIES                  
C                                                                       
C       THIS IS THE PROGRAM FOR THE FITTING OF PERIODIC AUTOREGRESSIVE M
C       BY THE METHOD OF LEAST SQUARES REALIZED THROUGH HOUSEHOLDER     
C       TRANSFORMATION.  THE OUTPUTS ARE THE ESTIMATES OF THE REGRESSION
C       COEFFICIENTS AND INNOVATION VARIANCE OF THE PERIODIC AR-MODEL FO
C       EACH INSTANT.                                                   
C                                                                       
C       PERIOD I           1              2                             
C                    !-----------!  !-----------!                  !----
C            Z(II)   +--+--+--+--+--+--+--+--+--+   . . . . . .    +--+-
C            Y(I,J)  +--+--+--+--+  +--+--+--+--+                  +--+-
C       INSTANT J    1  2       IP  1  2       IP                  1  2 
C                                                                       
C       WHERE                                                           
C          IP:     NUMBER OF INSTANTS IN ONE PERIOD                     
C          ND:     NUMBER OF PERIODS                                    
C          Y(I,J) = Z(IP*(I-1)+J)                                       
C                                                                       
C       THE STATISTIC AIC IS DEFINED BY                                 
C                                                                       
C               AIC  =  N * LOG( SD )  +  2 * ( NUMBER OF PARAMETERS )  
C       WHERE                                                           
C             N:    DATA LENGTH,                                        
C             SD:   ESTIMATE OF THE INNOVATION VARIANCE.                
C                                                                       
C                                                                       
C       --------------------------------------------------------------- 
C       REFERRENCES:                                                    
C          R.H.JONES AND W.M.BRELSFORD(1967), "TIME SERIES WITH PERIODIC
C          STRUCTURE.",  BIOMETRIKA,54,403-408.                         
C                                                                       
C          M.PAGANO(1978), "ON PERIODIC AND MULTIPLE AUTOREGRESSIONS."  
C          ANN. STATIST., 6, 1310-1317.                                 
C       --------------------------------------------------------------- 
C       THE FOLLOWING SUBROUTINES ARE DIRECTLY CALLED BY THIS PROGRAM.  
C            REDATA                                                     
C            PERREG                                                     
C            MREDCT                                                     
C            MARFIT                                                     
C            PRINT4                                                     
C       --------------------------------------------------------------- 
C       INPUTS REQUIRED:                                                
C             MT:    INPUT DEVICE SPECIFICATION (MT=5 : CARD READER)    
C             IP:    NUMBER OF OBSERVATIONS WITHIN A PERIOD             
C             LAG:   MAXIMUM LAG OF PERIODS                             
C                                                                       
C       --  THE FOLLOWING INPUTS ARE REQUESTED BY SUBROUTINE REDATA  -- 
C                                                                       
C             TITLE: TITLE OF DATA                                      
C             N:     DATA LENGTH, MUST BE LESS THAN OR EQUAL TO 10000   
C             DFORM: INPUT DATA FORMAT SPECIFICATION STATEMENT          
C                    -- EXAMPLE --    (8F10.5)                          
C             (Z(I),I=1,N):  ORIGINAL DATA                              
C       --------------------------------------------------------------- 
C                                                                       
cc      !DEC$ ATTRIBUTES DLLEXPORT :: PERARSF
C
cxx      IMPLICIT  REAL * 8 ( A-H,O-Z )                                    
CC      REAL  * 4   Z , Y                                                 
cc      REAL * 4   TITLE(20)                                              
cc      DIMENSION  Z(5000) , Y(200,24)                                    
cc      DIMENSION  X(200,150) , D(300)
cc      DIMENSION  U(150,150) , B(24,24,5) , E(24,24)                     
cc      DIMENSION  C(24)                                                  
cc      DIMENSION  EX(24)                                                 
cxx      DIMENSION  ZS(N), Z(N), Y(N/IP,IP)                                    
cxx      DIMENSION  X(((LAG+1)*IP+KSW)*2,(LAG+1)*IP+KSW)
cxx      DIMENSION  B(IP,IP,LAG) , E(IP,IP), BI(IP,IP,LAG) , EI(IP,IP)                     
cxx      DIMENSION  C(IP)
cxx      DIMENSION  EX(IP)
cxx      DIMENSION  AIC(LAG+1,IP), SD(LAG+1,IP), DIC(LAG+1,IP)
cxx      DIMENSION  AICM(IP), SDM(IP), IM(IP)
cxx      DIMENSION  JNDF((LAG+1)*IP+KSW,IP), AF((LAG+1)*IP+KSW,IP)
cxx      DIMENSION  NPR(IP), AICF(IP)
C
      INTEGER :: N, IP, LAG, KSW, NPR(IP), JNDF((LAG+1)*IP+KSW,IP),
     1           LMAX
      REAL(8) :: ZS(N), ZMEAN, SUM, AF((LAG+1)*IP+KSW,IP), AICF(IP),
     1           B(IP,IP,LAG), E(IP,IP), C(IP), EX(IP)
      INTEGER :: IM(IP)
      REAL(8) :: X(((LAG+1)*IP+KSW)*2,(LAG+1)*IP+KSW), Y(N/IP,IP), 
     1           Z(N), BI(IP,IP,LAG), EI(IP,IP), AIC(LAG+1,IP),
     2           SD(LAG+1,IP), DIC(LAG+1,IP), AICM(IP), SDM(IP),
     3           AICS
C
cc      CHARACTER(100) IFLNAM,OFLNAM
cc      CALL FLNAM2( IFLNAM,OFLNAM,NFL )
cc      IF ( NFL.EQ.0 ) GO TO 999
cc      IF ( NFL.EQ.2 ) THEN
cc         OPEN( 6,FILE=OFLNAM,ERR=900,IOSTAT=IVAR )
cc      ELSE
cc         CALL SETWND
cc      END IF
C                                                                       
C
C          PARAMETERS:                                                  
C                                                                       
cc      MJ = 200                                                          
cc      MJ1 = 200                                                         
cc      MJ2 = 24                                                          
cc      MJ3 = 5                                                           
cc      MJ4 = 150                                                         
      MJ = N/IP
      MJ2 = IP
      MJ3 = LAG
      MJ4 = (LAG+1)*IP+KSW
      MJ1 = MJ4*2
      ISW = 1                                                           
      IPR = 2                                                           
C                                                                       
CC      READ( 5,1 )     MT                                                
cc      MT = 5
cc      OPEN( MT,FILE=IFLNAM,ERR=910,IOSTAT=IVAR,STATUS='OLD' )
cc      READ( 5,1 )     IP , LAG , KSW                                    
cc      WRITE( 6,2 )                                                      
cc      WRITE( 6,3 )                                                      
cc      WRITE( 6,4 )   IP , LAG , KSW , MT                                
cc      WRITE( 6,5 )                                                      
C                                                                       
C          ORIGINAL DATA LOADING                                        
C                                                                       
cc      CALL  REDATA( Z,N,MT,TITLE )                                      
      CALL  REDATA( ZS,Z,N,ZMEAN,SUM )                                      
cc      CLOSE( MT )
C                                                                       
C          DATA MATRIX SET UP                                           
C                                                                       
      CALL  PERREG( Z,N,IP,MJ,Y,ND )                                    
      NMK = ND - LAG                                                    
      N0 = 0                                                            
      ID = IP                                                           
C                                                                       
C          REDUCTION TO AN UPPER TRIANGULAR FORM                        
C                                                                       
cc      CALL  MREDCT( Y,D,NMK,N0,LAG,ID,MJ,MJ1,KSW,X )
      X(1:MJ1,1:MJ4) = 0.0D0
      CALL  MREDCT( Y,NMK,N0,LAG,ID,MJ,MJ1,KSW,X )                    
C                                                                       
C          INSTANTANEOUS RESPONSE MODEL FITTING                         
C                                                                       
cc      CALL  MARFIT( X,U,D,NMK,IP,LAG,KSW,MJ1,MJ2,MJ3,MJ4,ISW,IPR,B,E,EX,
cc     *              C,LMAX,AIC )                                        
cx      IFG = 0
      CALL MARFIT( X,NMK,IP,LAG,KSW,MJ1,MJ2,MJ3,MJ4,ISW,IPR,AIC,SD,
cx     *DIC,AICM,SDM,IM,BI,EI,B,E,EX,C,LMAX,AICS,JNDF,AF,NPR,AICF,IFG,LU )
     *DIC,AICM,SDM,IM,BI,EI,B,E,EX,C,LMAX,AICS,JNDF,AF,NPR,AICF )
C   
C                                                                       
C          REGRESSION MODEL PRINT OUT                                   
C                                                                       
cc      CALL  PRINT4( B,E,C,EX,ID,LMAX,MJ2 )                              
cc      GO TO 999
C                                                                       
cc  900 CONTINUE
cc      WRITE(6,600) IVAR,OFLNAM
cc  600 FORMAT(/,' !!! Output_Data_File OPEN ERROR ',I8,/,5X,100A)
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
cc  610 FORMAT(/,' !!! Input_Data_File OPEN ERROR ',I8,/,5X,100A)
C
cc  999 CONTINUE
      RETURN
C                                                                       
cxx    1 FORMAT( 16I5 )                                                    
cxx    2 FORMAT( 1H ,'PROGRAM  TIMSAC 78.2.3.',/,1H ,'  PERIODIC AUTOREGRES
cxx     1SIVE MODELS FITTING BY THE METHOD OF LEAST SQUARES (SCALAR CASE)')
cxx    3 FORMAT( 1H ,'  <PERIODIC AUTOREGRESSIVE MODEL (J=1,...,IP) >',/,  
cxx     1 1H ,8X,'Y(I,J) = C(J) + A(1,J,0)*Y(I,1) + ... + A(J-1,J,0)*',
cxx     2'Y(I,J-1) + A(1,J,1)*Y(I-1,1) + ... + A(IP,J,1)*Y(I-1,IP)',
cxx     3' + ... + E(I,J)',/,   1H ,'  WHERE',/,
cxx     4  1H ,7X,'IP:       NUMBER OF INSTANTS IN ONE PERIOD',/,          
cxx     5  1H ,7X,'E(I,J):   GAUSSIAN WHITE NOISE' )                       
cxx    4 FORMAT( 1H ,2X,'IP =',I3,5X,'LAG =',I3,5X,'KSW =',I3,/,3X,'ORIGINA
cxx     *L DATA INPUT DEVICE   MT =',I3 )                                  
cxx    5 FORMAT( 1H ,'  *****  WHEN KSW IS SET TO 0, THE CONSTANT TERM C(J)
cxx     * IS EXCLUDED.  *****' )                                           
      E N D                                                             
      SUBROUTINE  PERREG( Z,N,IP,MJ,Y,ND )                              
C                                                                       
C     THIS SUBROUTINE PREPARES DATA MATRIX Y FOR THE FITTING OF A PERIOD
C     AUTOREGRESSIVE MODEL FROM THE DATA VECTOR Z OF CONSECUTIVE OBSERVA
C     EACH COLUMN OF Y IS COMPOSED OF THE OBSERVATIONS AT THE SAME INSTA
C     WITHIN A PERIOD.                                                  
C                                                                       
C       INPUTS:                                                         
C          Z:     ORIGINAL DATA VECTOR                                  
C          N:     LENGTH OF ORIGINAL DATA                               
C          IP:    SPAN OF ONE PERIOD                                    
C          MJ:    ABSOLUTE DIMENSION OF Y                               
C                                                                       
C       OUTPUTS:                                                        
C          Y:     REARRANGED DATA MATRIX                                
C          ND:    NUMBER OF ROWS OF Y                                   
C                                                                        
CC      DIMENSION  Z(1) , Y(MJ,1)                                         
cx      REAL * 8  Z(1) , Y(MJ,1)
cxx      REAL * 8  Z(N) , Y(MJ,IP)
      INTEGER :: N, IP, MJ, ND
      REAL(8) :: Z(N), Y(MJ,IP)
      ND = N / IP                                                       
cxx      DO 10  I=1,ND
      DO 20  I=1,ND
      DO 10  J=1,IP                                                     
      II = (I-1)*IP + J                                                 
cxx   10 Y(I,J) = Z(II)
      Y(I,J) = Z(II)
   10 CONTINUE
   20 CONTINUE
      RETURN                                                            
C                                                                       
      E N D                                                             
