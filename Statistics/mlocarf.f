      SUBROUTINE MLOCARF( ZS,N,LAG,NS0,KSW,NML,ZMEAN,SUM,A,MF,SDF,LK0,
     *                    LK2,SXX,NNF,NNS,MS,SDMS,AICS,MP,SDMP,AICP )
C
      INCLUDE 'timsac_f.h'
C
cc      PROGRAM  MLOCAR                                                   
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
C     TIMSAC 78.3.1.                                                    
C     _                     ___                __                       
C     MINIMUM AIC METHOD OF LOCALLY STATIONARY AR MODEL FITTING; SCALAR 
C                                                                       
C     THIS PROGRAM LOCALLY FITS AUTOREGRESSIVE MODELS TO NON-STATIONARY 
C     SERIES BY MINIMUM AIC PROCEDURE.                                  
C                                                                       
C     BY THIS PROCEDURE, THE DATA OF LENGTH N ARE DIVIDED INTO J LOCALLY
C     STATIONARY SPANS                                                  
C                                                                       
C                <-- N1 --> <-- N2 --> <-- N3 -->          <-- NJ -->   
C               !----------!----------!----------! ...... !----------!  
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
C             REDATA                                                    
C             NONSTA                                                    
C             PRINTA                                                    
C             NRASPE                                                    
C       --------------------------------------------------------------- 
C        INPUTS REQUIRED;                                               
C             MT:       INPUT DEVICE FOR ORIGINAL DATA (MT=5 : CARD READ
C             LAG:      UPPER LIMIT OF THE ORDER OF AR-MODEL, MUST BE LE
C                       OR EQUAL TO 50.                                 
C             NS:       LENGTH OF BASIC LOCAL SPAN                      
C             KSW:      =0  CONSTANT VECTOR IS NOT INCLUDED AS A REGRESS
C                       =1  CONSTANT VECTOR IS INCLUDED AS THE FIRST REG
C                                                                       
C               -- THE FOLLOWING INPUTS ARE REQUESTED BY SUBROUTINE REDA
C             TITLE:    SPECIFICATION OF DATA                           
C             N:        DATA LENGTH, MUST BE LESS THAN OR EQUAL TO 10000
C             DFORM:    INPUT DATA SPECIFICATION STATEMENT.             
C                       -- EXAMPLE  --     (8F10.5)                     
C             (Z(I),I=1,N):  ORIGINAL DATA                              
C               --------------------------------------------------------
C
cc      !DEC$ ATTRIBUTES DLLEXPORT :: MLOCARF
C
cxx      IMPLICIT  REAL * 8  ( A-H , O-Z )                                 
CC      REAL * 4   Z(10000) , TITLE(20)                                   
cc      REAL * 4   TITLE(20)
cc      DIMENSION  X(200,51)                                              
cc      DIMENSION  D(200) , A(50)                                         
cc      DIMENSION  U(51,51)                                               
CC      DIMENSION  TTL(2)                                                 
CC      DATA  TTL / 8H  CURREN,8HT MODEL  /                               
cxx      DIMENSION  ZS(N), Z(N)
cxx      DIMENSION  X(N,LAG+KSW+1)
cxx      DIMENSION  U(LAG+KSW+1,LAG+KSW+1), AA(LAG+KSW), A(LAG+KSW,NML)
cc      REAL*4  TTL(4)                                                 
cc      DATA  TTL / 4H  CU,4HRREN,4HT MO,4HDEL  /                               
C
cxx      DIMENSION  MF(NML), SDF(NML), LK0(NML), LK2(NML)
cxx      DIMENSION  NNF(NML), NNS(NML)
cxx      DIMENSION  MS(NML), SDMS(NML), AICS(NML)
cxx      DIMENSION  MP(NML), SDMP(NML), AICP(NML)
cxx      DIMENSION  SXX(121,NML)
      INTEGER :: N, LAG, NS0, KSW, NML, MF(NML), LK0(NML), LK2(NML),
     1           NNF(NML), NNS(NML), MS(NML), MP(NML)
      REAL(8) :: ZS(N), ZMEAN, SUM, A(LAG+KSW,NML), SDF(NML), 
     1           SXX(121,NML), SDMS(NML), AICS(NML), SDMP(NML),
     2           AICP(NML)
      REAL(8) :: Z(N), X(N,LAG+KSW+1), U(LAG+KSW+1,LAG+KSW+1),
     1           AA(LAG+KSW), B
C                                                                       
C          EXTERNAL SUBROUTINE DECLARATION                              
C                                                                       
      EXTERNAL  SETX1                                                   
C                                                                       
C     PARAMETERS:                                                       
C          ISW:   =0  TO PRODUCE THE MAICE MODEL ONLY (OUTPUTS SUPRRESSE
C                 =1  TO PRODUCE THE MAICE MODEL ONLY                   
C                 =2  TO PRODUCE ALL AR-MODELS (UP TO THE ORDER K)      
C          MJ1:   ABSOLUTE DIMENSION FOR SUBROUTINE CALL                
C          MJ2:   ABSOLUTE DIMENSION FOR SUBROUTINE CALL                
C
cc      CHARACTER(100) IFLNAM,OFLNAM
cc      CALL FLNAM2( IFLNAM,OFLNAM,NFL )
cc      IF (NFL.EQ.0) GO TO 999
cc      IF (NFL.EQ.2) THEN
cc         OPEN( 6,FILE=OFLNAM,ERR=900,IOSTAT=IVAR )
cc      ELSE
cc         CALL SETWND
cc      END IF
C                                                                       
C
cc      MJ1 = 200                                                         
cc      MJ2 = 51                                                          
      MJ1 = N
      MJ2 = LAG+1                                                          
      ISW = 0
C
      MF(1:NML) = 0
      A(1:(LAG+KSW),1:NML) = 0.0d0
      SXX(1:121,1:NML) = 0.0d0 
C                                                                       
CC      READ( 5,1 )     MT                                                
cc      MT = 5
cc      OPEN( MT,FILE=IFLNAM,ERR=910,IOSTAT=IVAR,STATUS='OLD' )
cc      READ( 5,1 )     LAG , NS , KSW                                    
C
cc      WRITE( 6,3 )                                                      
cc      IF( KSW .EQ. 1 )     WRITE( 6,5 )                                 
cc      IF( KSW .NE. 1 )     WRITE( 6,4 )                                 
cc      WRITE( 6,6 )                                                      
cc      WRITE( 6,2 )    LAG , NS , MT                                     
C
C          ---------------------------------------                      
C          ORIGINAL DATA LOADING AND MEAN DELETION                      
C          ---------------------------------------                      
C
cc      CALL  REDATA( Z,N,MT,TITLE )
      CALL  REDATA( ZS,Z,N,ZMEAN,SUM )
cc      CLOSE( MT )
C
      L  = 0                                                            
      K  = LAG + KSW                                                    
      MX = K * 2                                                        
      IF = 0                                                            
      NJ = 0
      NF = 0
      NS = NS0
C                                                                       
  100 CONTINUE                                                          
C                                                                       
      LK  = L + K                                                       
      LK1 = LK + 1                                                      
      IF( LK1     .GE. N  )   GO TO 200                                 
      IF( N-LK1   .LT. NS )   NS = N - LK                               
      IF( N-LK1-NS.LT. MX )   NS = N - LK                               
C                                                                       
C          -----------------------------------                          
C          LOCALLY STATIONARY AR-MODEL FITTING                          
C          -----------------------------------                          
C                                                                       
cc      CALL  NONSTA( SETX1,Z,X,U,D,LAG,L,NS,K,IF,ISW,TITLE,MJ1,MJ2,A,MF, 
cc     1SDF )                                                             
      NJ = NJ+1
      IF ( NJ .GT. 1 )  MF(NJ) = MF(NJ-1)
      IF ( NJ .GT. 1 )  SDF(NJ) = SDF(NJ-1)
      CALL  NONSTA( SETX1,Z,X,U,LAG,L,NF,NS,K,IF,ISW,MJ1,MJ2,AA,MF(NJ),
     *SDF(NJ),NNF(NJ),NNS(NJ),MS(NJ),SDMS(NJ),AICS(NJ),MP(NJ),SDMP(NJ),
     *AICP(NJ) )
C                                                                       
      L = L + NS                                                        
cc      IF( IF .EQ. 2 )     LK0 = LK1                                     
      IF( IF .EQ. 2 )     LK0(NJ) = LK1                                     
      IF( IF .NE. 2 )     LK0(NJ) = LK0(NJ-1)
C
C          -----------------------                                      
C          PRINT OUT CURRENT MODEL                                      
C          -----------------------                                      
C
cc      LK2 = LK + NS                                                     
cc      CALL  PRINTA( A,SDF,MF,TTL,4,TITLE,LK0,LK2 )                      
      LK2(NJ) = LK + NS
C                                                                       
C                                                                       
C          ----------------                                             
C          SPECTRUM DISPLAY                                             
C          ----------------                                             
C                                                                       
cc      CALL  NRASPE( SDF,A,B,MF,0,121,TITLE )                            
      CALL  NRASPE( SDF(NJ),AA,B,MF(NJ),0,120,SXX(1,NJ) )
      DO 110 I = 1,MF(NJ)
cxx  110 A(I,NJ) = AA(I)
      A(I,NJ) = AA(I)
  110 CONTINUE
C                                                                       
      GO TO 100                                                         
C                                                                       
  200 CONTINUE                                                          
cc      GO TO 999
C
cc  900 CONTINUE
cc      WRITE(6,600) IVAR,OFLNAM
cc      GO TO 999
C
cc  910 CONTINUE
cc      IF (NFL.EQ.2) CLOSE( 6 )
cc#ifdef __linux__
ccC	reopen #6 as stdout
cc      IF (NFL.EQ.2) OPEN(6, FILE='/dev/fd/1')
cc#endif
ccC /* __linux__ */
cc      WRITE(6,610) IVAR,IFLNAM
C
cxx  600 FORMAT(/,' !!! Output_Data_File OPEN ERROR ',I8,//5X,100A)
cxx  610 FORMAT(/,' !!! Input_Data_File OPEN ERROR ',I8,//5X,100A)
C
cc  999 CONTINUE
      RETURN
C                                                                       
cxx    1 FORMAT( 16I5 )                                                    
cxx    2 FORMAT( ///1H ,'  FITTING UP TO THE ORDER  K =',I3,'  IS TRIED',/,
cxx     1'   BASIC LOCAL SPAN  NS =',I4,/,'   ORIGINAL DATA INPUT DEVICE  M
cxx     2T =',I3 )                                                         
cxx    3 FORMAT( //  ' PROGRAM TIMSAC 78.3.1',/'   LOCALLY STATIONARY AUTOR
cxx     1EGRESSIVE MODEL FITTING;   SCALAR CASE',//,'   < BASIC AUTOREGRESS
cxx     2IVE MODEL >' )                                                    
cxx    4 FORMAT( 1H ,10X,'Z(I) = A(1)*Z(I-1) + A(2)*Z(I-2) + ... + A(M)*Z(I
cxx     1-M) + E(I)' )                                                     
cxx    5 FORMAT( 1H ,10X,'Z(I) = A(1) + A(2)*Z(I-1) + ... + A(M+1)*Z(I-M) +
cxx     1 E(I)' )                                                          
cxx    6 FORMAT( 1H ,2X,'WHERE',/,11X,'M:     ORDER OF THE MODEL',/,11X,'E(
cxx     1I):  GAUSSIAN WHITE NOISE WITH MEAN 0  AND  VARIANCE SD(M).' )    
C                                                                       
      END                                                               
cc      SUBROUTINE  NONSTA( SETX,Z,X,U,D,LAG,N0,NS,K,IF,ISW,TITLE,MJ1,MJ2,
cc     1A,MF,SDF )                                                        
      SUBROUTINE  NONSTA( SETX,Z,X,U,LAG,N0,NF,NS,K,IF,ISW,MJ1,MJ2,A,MF,
     *                    SDF,NNF,NNS,MS,SDMS,AICS,MP,SDMP,AICP )
C                                                                       
C     IN THIS SUBROUTINE THE FOLLOWING TWO MODELS ARE COMPARED AND      
C     THE MODEL WITH LESS AIC IS ACCEPTED AS THE CURRENT MODEL.         
C                                                                       
C       MOVING MODEL:     SUCCESSION OF TWO-MODELS INDEPENDENTLY FITTED 
C                         TO THE DIVIDED DATA                           
C              NF:     DATA LENGTH OF THE PRECEDING STATIONARY BLOCK    
C              NS:     DATA LENGTH OF NEW BLOCK (= BASIC LOCAL SPAN )   
C              AR(MF,SDF):   MAICE AR-MODEL WITH THE ORDER MF AND INNOVA
C                            VARIANCE SDF FITTED TO THE PRECEDING STATIO
C                            BLOCK                                      
C              AR(MS,SDS):   MAICE AR-MODEL (ORDER MS AND INNOVATION VAR
C                            SDS) FITTED TO THE NEWLY OBTAINED DATA     
C              AICS = NF*LOG(SDF) + NS*LOG(SDS) + 2*(MF+MS+2)           
C                                                                       
C                                AR(MF,SDF)       AR(MS,SDS)            
C                             !---------------!----------------!        
C                              <---- NF -----> <----- NS ----->         
C                                                                       
C                                                                       
C       CONSTANT MODEL:   AR MODEL FITTED TO THE POOLED DATA            
C              NP:     DATA LENGTH OF POOLED DATA  (=NF+NS)             
C              AR(MP,SDP):   MAICE AR-MODEL FITTED TO THE POOLED DATA   
C                            (ORDER = MP, INNOVATION VARIANCE = SDP)    
C              AICP = NP*LOG(SDP) + 2*(MP+1)                            
C                                                                       
C                                        AR(MP,SDP)                     
C                             !--------------------------------!        
C                              <------------ NP -------------->         
C                                                                       
C                                                                       
C       ----------------------------------------------------------------
C       THE FOLLOWING SUBROUTINES ARE DIRECTLY CALLED BY THIS SUBROUTINE
C             COPY                                                      
C             ARMFIT                                                    
C             HUSHLD                                                    
C             REDUCT                                                    
C       ----------------------------------------------------------------
C                                                                       
C       INPUTS:                                                         
C          SETX:   EXTERNAL SUBROUTINE DESIGNATION                      
C          Z:      ORIGINAL DATA VECTOR                                 
C          X:      WORKING AREA                                         
C          U:      WORKING AREA                                         
C          D:      WORKING AREA                                         
C          LAG:    UPPER LIMIT OF THE ORDER OF AR-MODEL                 
C          NS:     LENGTH OF BASIC LOCAL SPAN                           
C          IF:     =0   FIT INITIAL AR-MODEL AND STORE                  
C                  >0   UPDATE THE CURRENT MODEL                        
C          ISW:    =0  TO PRODUCE THE MAICE MODEL ONLY (OUTPUTS SUPPRESS
C                  =1  TO PRODUCE THE MAICE MODEL ONLY                  
C                  =2  TO PRODUCE ALL AR-MODELS (UP TO THE ORDER K)     
C          TITLE:  TITLE OF DATA                                        
C          MJ1:    ABSOLUTE DIMENSION OF X IN THE MAIN PROGRAM          
C          MJ2:    ABSOLUTE DIMENSION OF U IN THE MAIN PROGRAM          
C                                                                       
C       OUTPUTS:                                                        
C          A:      AR-COEFFICIENTS OF THE CURRENT MODEL                 
C          MF:     ORDER OF THE CURRENT MODEL                           
C          SDF:    INNOVATION VARIANCE OF THE CURRENT MODEL             
C          IF:     =1   MODEL UNSWITCHED                                
C                  =2   MODEL SWITCHED                                  
C                                                                       
cxx      IMPLICIT  REAL * 8  ( A-H , O-Z )                                 
CC      REAL * 4  Z(1) , TITLE(1)                                         
cc      REAL * 4   TITLE(1)
cc      DIMENSION  X(MJ1,1) , U(MJ2,1) , A(1)                      
cc      DIMENSION  B(50)                                                  
cx      DIMENSION  Z(1)
cxx      DIMENSION  Z(MJ1)
cxx      DIMENSION  X(MJ1,K+1) , U(K+1,K+1) , A(K)                      
cxx      DIMENSION  B(K)                                                  
cxx      DIMENSION  SDS(K+1), AS(K+1), DICS(K+1)
cxx      DIMENSION  SDP(K+1), AP(K+1), DICP(K+1)
      INTEGER :: LAG, N0, NF, NS, K, IF, ISW, MJ1, MJ2, MF, NNF,
     1           NNS, MS, MP
      REAL(8) :: Z(MJ1), X(MJ1,K+1), U(K+1,K+1), A(K), SDF, SDMS,
     1           AICS, SDMP, AICP
      REAL(8) :: B(K), SDS(K+1), AS(K+1), DICS(K+1), SDP(K+1),
     1           AP(K+1), DICP(K+1), AICMS, AICMP
      EXTERNAL  SETX
C                                                                       
      K1 = K + 1                                                        
      K2 = K1*2                                                         
      NNF = 0
      NNS = 0
cx      IFG = 0
C                                                                     +-
C       ---  DATA LOADING AND HOUSEHOLDER TRANSFORMATION  ---         ! 
C                                                                     +-
cc      CALL  REDUCT( SETX,Z,D,NS,N0,K,MJ1,LAG,X )                        
      CALL  REDUCT( SETX,Z,NS,N0,K,MJ1,LAG,X )                        
C                                                                     +-
C       ---  AR-MODEL FITTING TO NEW SET OF DATA  ---                 ! 
C                                                                     +-
cc      CALL  ARMFIT( X,K,LAG,NS,ISW,TITLE,MJ1,B,SDS,MS )                 
cx      CALL  ARMFIT( X,K,LAG,NS,ISW,MJ1,B,MS,SDS,AS,DICS,SDMS,AICMS,
cx     *              IFG,LU )
      CALL  ARMFIT( X,K,LAG,NS,ISW,MJ1,B,MS,SDS,AS,DICS,SDMS,AICMS)
C                                                                NO  +--
C                                                              +-----!  
C                                                              !     +--
      IF( IF .NE. 0 )     GO TO 10                                      
C                                                              !        
C                                                              !      +-
C       ---  MAKE A COPY OF X ON U  ---                        !      ! 
C                                                              !      +-
      CALL  COPY( X,K1,0,0,MJ1,MJ2,U )                                  
C                                                              !        
C       ---  AIC FOR INITIAL LOCAL MODEL  ---                  !        
C                                                              !        
cc      AICS = NS*DLOG(SDS) + 2.D0*(MS+1)                                 
      AICS = NS*DLOG(SDMS) + 2.D0*(MS+1)                                 
C                                                              !    ( GO
cc      WRITE( 6,5 )     NS , MS , SDS , AICS                             
      NNS = NS
      GO TO 20                                                          
C                                                              !        
C                                                              +--------
C       ---  AIC FOR MOVING MODEL  ---                                  
C                                                                       
cc   10 AICS = NF*DLOG(SDF) + NS*DLOG(SDS) + 2.D0*(MF+MS+2)               
   10 AICS = NF*DLOG(SDF) + NS*DLOG(SDMS) + 2.D0*(MF+MS+2)               
C                                                                       
cc      WRITE( 6,4 )                                                      
cc      WRITE( 6,6 )     NF , NS , MS , SDS , AICS                        
      NNF = NF
      NNS = NS
C                                                                       
C          -------------------------------                              
C          AR-MODEL FITTING TO POOLED DATA                              
C          -------------------------------                            +-
C                                                                     ! 
C       ---  MAKE COPIES OF U AND X  ---                              ! 
C                                                                     +-
      CALL  COPY( X,K1,0,K2,MJ1,MJ1,X )                                 
      CALL  COPY( U,K1,0,K1,MJ2,MJ1,X )                                 
C                                                                     +-
C       HOUSEHOLDER TRANSFORMATION  ---                               ! 
C                                                                     +-
cc      CALL  HUSHLD( X,D,MJ1,K2,K1 )                                     
      CALL  HUSHLD( X,MJ1,K2,K1 )                                     
C                                                                       
C       ---  AR MODEL FITTING TO POOLED DATA  ---                       
C                                                                       
      NP = NF + NS                                                      
cc      CALL  ARMFIT( X,K,LAG,NP,ISW,TITLE,MJ1,A,SDP,MP )                 
cx      CALL  ARMFIT( X,K,LAG,NP,ISW,MJ1,A,MP,SDP,AP,DICP,SDMP,AICMP,
cx     *              IFG,LU )
      CALL  ARMFIT( X,K,LAG,NP,ISW,MJ1,A,MP,SDP,AP,DICP,SDMP,AICMP)
C                                                                       
C       ---  AIC FOR CONSTANT MODEL  ---                                
C                                                                       
cc     AICP = NP*DLOG(SDP) + 2.D0*(MP+1)                                 
cc      WRITE( 6,7 )     NP , MP , SDP , AICP                             
      AICP = NP*DLOG(SDMP) + 2.D0*(MP+1)                                 
C                                                                       
C          --------------------                              YES  +-----
C          COMPARISON OF MODELS                             +-----!AICS 
C          --------------------                             !     +-----
C                                                           !           
      IF( AICS .GE. AICP )     GO TO  40                                
C                                                           !           
C          ------------------------                                     
C          SPECTRUM CHANGE DETECTED                                     
C          ------------------------                                     
C                                                           !           
cc      WRITE( 6,8 )                                                      
C                                                           !         +-
C       ---  MAKE A COPY OF X2 ON U  ---                    !         !X
C                                                           !         +-
      CALL  COPY( X,K1,K2,0,MJ1,MJ2,U )                                 
C                                                           !           
   20 IF = 2                                                            
      NF = NS                                                           
      MF = MS                                                           
C                                                           !           
      DO 30  I=1,MF                                                     
cxx   30 A(I) = B(I)
      A(I) = B(I)
   30 CONTINUE                                                       
C                                                           !           
cc      SDF = SDS                                                         
      SDF = SDMS                                                         
      GO TO 50                                                          
C                                                           +-----------
   40 IF = 1                                                            
C                                                                       
C          ------------------                                   (DATA PO
C          SPECTRUM UNCHANGED                                           
C          ------------------                                           
C                                                                   +---
C       ---  MAKE A COPY OF X ON U  ---                             ! X-
C                                                                   +---
C                                                                       
      CALL  COPY( X,K1,0,0,MJ1,MJ2,U )                                  
C                                                                       
cc      WRITE( 6,9 )                                                      
cc      SDF = SDP                                                         
      SDF = SDMP                                                         
      MF = MP                                                           
      NF = NF + NS                                                      
C                                                                       
   50 CONTINUE                                                          
C
      RETURN                                                            
C
cxx    4 FORMAT( //1H ,'---  THE FOLLOWING TWO MODELS ARE COMPARED  ---' ) 
cxx    5 FORMAT( //1H ,'INITIAL LOCAL MODEL:    NS =',I5,5X,'MS =',I3,5X,  
cxx     1  'SDS =',D16.8,5X,'AICS =',F16.3 )                               
cxx    6 FORMAT( 1H ,'MOVING MODEL:      (NF =',I5,', NS =',I4,1H),5X,'MS =
cxx     2',I3,5X,'SDS =',D16.8,5X,'AICS =',F16.3 )                         
cxx    7 FORMAT( 1H ,'CONSTANT MODEL:    (NP =',I5,1H),15X,'MP =',I3,5X,'SD
cxx     3P =',D16.8,5X,'AICP =',F16.3 )                                    
cxx    8 FORMAT( //1H ,37(1H*),/,1H ,'*****',27X,'*****',/,1H ,'*****     N
cxx     1EW MODEL ADOPTED     *****',/,1H ,'*****',27X,'*****',/,1H ,37(1H*
cxx     2) )                                                               
cxx    9 FORMAT( 1H ,'*****  CONSTANT MODEL ADOPTED  *****' )              
cxx  700 FORMAT( 1H ,'-----  X  -----' )                                   
cxx  620 FORMAT( 1H ,10D13.5 )                                             
cxx  600 FORMAT( 1H ,'N =',I5,5X,'K =',I5,5X,'M =',I5,5X,'MT =',I5,5X,     
cxx     * 'DATA FORMAT =',15A4 )                                           
cxx  601 FORMAT( 1H ,'-----  ORIGINAL DATA  -----',/,(1X,10D13.5) )        
C                                                                       
      END
