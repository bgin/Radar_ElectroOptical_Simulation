      SUBROUTINE  MULMARF( ZS,N,ID,C,LAG,ZMEAN,ZVARI,SD1,AIC1,DIC1,IM,
cx     * AICM,SDM,NPR,JNDF,AF,EX,AIC,EI,BI,E,B,LMAX,AICS,TMP,IER )
     * AICM,SDM,NPR,JNDF,AF,EX,AIC,EI,BI,E,B,LMAX,AICS )
C
      INCLUDE 'timsac_f.h'
C
cc      PROGRAM  MULMAR                                                   
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
C     TIMSAC 78.2.1.                                                    
C     ___                  _                     __                     
C     MULTIVARIATE CASE OF MINIMUM AIC METHOD OF AR MODEL FITTING.      
C                                                                       
C     THIS PROGRAM FITS A MULTI-VARIATE AUTOREGRESSIVE MODEL BY THE MINI
C     AIC PROCEDURE.  ONLY THE POSSIBILITIES OF ZERO COEFFICIENTS AT THE
C     BEGINNING AND END OF THE MODEL ARE CONSIDERED. THE LEAST SQUARES E
C     OF THE PARAMETERS ARE OBTAINED BY THE HOUSEHOLDER TRANSFORMATION. 
C     AIC IS DEFINED BY                                                 
C                                                                       
C            AIC  =  N * LOG( DET(SD) ) + 2 * (NUMBER OF PARAMETERS)    
C                                                                       
C       WHERE                                                           
C           N:    NUMBER OF DATA,                                       
C           SD:   ESTIMATE OF INNOVATION VARIANCE MATRIX                
C           DET:  DETERMINANT,                                          
C           K:    NUMBER OF FREE PARAMETERS.                            
C                                                                       
C                                                                       
C       --------------------------------------------------------------- 
C       THE FOLLOWING SUBROUTINES ARE DIRECTLY CALLED BY THIS PROGRAM.  
C           MRDATA                                                      
C           MREDCT                                                      
C           MARFIT                                                      
C       --------------------------------------------------------------- 
C       REFERENCE:                                                      
C          G.KITAGAWA AND H.AKAIKE(1978), "A PROCEDURE FOR THE MODELING 
C          OF NON-STATIONARY TIME SERIES.",  ANN. INST. STATIST. MATH., 
C          30,B,351-363.                                                
C       --------------------------------------------------------------- 
C       INPUTS REQUIRED:                                                
C           MT:    INPUT DEVICE FOR ORIGINAL DATA (MT=5; CARD READER)   
C           LAG:   UPPER LIMIT OF AR-ORDER,  MUST BE LESS THAN 31       
C                                                                       
C          .....  FOLLOWING INPUTS ARE REQUIRED AT SUBROUTINE MREDCT  ..
C           TITLE: SPECIFICATION OF DATA                                
C           N:     DATA LENGTH,  MUST BE LESS THAN OR EQUAL TO 1000     
C           ID:    DIMENSION OF DATA,  MUST BE LESS THAN 11             
C                       < ID*(M+1) MUST BE LESS THAN 101 >              
C           IFM:   CONTROL FOR INPUT                                    
C           FORM:  INPUT DATA FORMAT SPECIFICATION STATEMENT            
C                  -- FOR EXAMPLE --     (8F10.5)                       
C          C(I):  CALIBRATION OF CHANNEL I (I=1,ID)                     
C           Z:    ORIGINAL DATA; Z(K,I) (K=1,N) REPRESENTS THE I-TH CHAN
C                 RECORD                                                
C                                                                       
cc      !DEC$ ATTRIBUTES DLLEXPORT :: MULMARF
C
cxx      IMPLICIT  REAL * 8  ( A-H , O-Z )                                 
CC      REAL * 4  Z                                                       
cc      DIMENSION  Z(1500,5)                                              
cc      DIMENSION  X(200,100) , D(200)                                    
cc      DIMENSION  Y(100,100) , B(10,10,30) , E(10,10)                    
cc      DIMENSION  C(10) , EX(10)                                         
cxx      DIMENSION  Z(N,ID), ZS(N,ID), C(ID)
cxx      DIMENSION  ZMEAN(ID), ZVARI(ID)
cxx      DIMENSION  X((LAG+1)*ID*2,(LAG+1)*ID)
cxx      DIMENSION  B(ID,ID,LAG) , E(ID,ID), BI(ID,ID,LAG) , EI(ID,ID)
cxx      DIMENSION  EX(ID), CV(ID)
cxx      DIMENSION  SD1(LAG+1,ID), AIC1(LAG+1,ID), DIC1(LAG+1,ID)
cxx      DIMENSION  AICM(ID), SDM(ID), IM(ID)
cxx      DIMENSION  JNDF((LAG+1)*ID,ID), AF((LAG+1)*ID,ID)
cxx      DIMENSION  NPR(ID), AIC(ID)
      INTEGER :: N, ID, LAG, IM(ID), NPR(ID), JNDF((LAG+1)*ID,ID), LMAX
      REAL(8) :: ZS(N,ID), C(ID), ZMEAN(ID), ZVARI(ID), SD1(LAG+1,ID),
     1           AIC1(LAG+1,ID), DIC1(LAG+1,ID), AICM(ID), SDM(ID),
     2           AF((LAG+1)*ID,ID), EX(ID), AIC(ID), EI(ID,ID),
     3           BI(ID,ID,LAG), E(ID,ID), B(ID,ID,LAG), AICS
      REAL(8) :: Z(N,ID), X((LAG+1)*ID*2,(LAG+1)*ID), CV(ID)
cx      INTEGER*1  TMP(1)
cx      CHARACTER  CNAME*80
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
C           MJ:    ABSOLUTE DIMENSION FOR SUBROUTINE CALL               
C           MJ1:   ABSOLUTE DIMENSION FOR SUBROUTINE CALL, SHOULD BE LAR
C                  THAN ID*(M+1)                                        
C           MJ2:   ABSOLUTE DIMENSION FOR SUBROUTINE CALL               
C           MJ3:   ABSOLUTE DIMENSION FOR SUBROUTINE CALL               
C           MJ4:   ABSOLUTE DIMENSION FOR SUBROUTINE CALL               
C           IPR:   PRINT OUT CONTROL                                    
C                                                                       
cc      MJ = 1500                                                         
cc      MJ1 = 200                                                         
cc      MJ2 = 10                                                          
cc      MJ3 = 30                                                          
cc      MJ4 = 100                                                         
      MJ = N
      MJ1 = (LAG+1)*ID*2
      MJ2 = ID
      MJ3 = LAG
      MJ4 = (LAG+1)*ID
      KSW = 0                                                           
      IPR = 3                                                           
CC      READ( 5,1 )     MT                                                
cc      MT = 5
cc      OPEN( MT,FILE=IFLNAM,ERR=910,IOSTAT=IVAR,STATUS='OLD' )
cc      READ( 5,1 )     LAG                                               
cc      WRITE( 6,3 )                                                      
cc      WRITE( 6,4 )     LAG , MT                                         
C                                                                       
cx      IER=0
cx      LU=3
cx      DO 100 I = 1,80
cx         CNAME(I:I) = ' '
cx  100 CONTINUE
cx      I = 1
cx      IFG = 1
cx      DO WHILE( (IFG.EQ.1) .AND. (I.LE.80) )
cx	   IF ( TMP(I).NE.ICHAR(' ') ) THEN
cx            CNAME(I:I) = CHAR(TMP(I))
cx            I = I+1
cx         ELSE
cx            IFG = 0
cx         END IF
cx      END DO
cx      IF ( I.GT.1 ) THEN
cx         IFG = 1
cx         OPEN (LU,FILE=CNAME,IOSTAT=IVAR)
cx         IF (IVAR .NE. 0) THEN
cxcx            WRITE(*,*) ' ***  mulmar temp FILE OPEN ERROR :',CNAME,IVAR
cx            IER=IVAR
cx            IFG=0
cx         END IF
cx      END IF

C
C     --  ORIGINAL DATA LOADING AND MEANS DELETION  --                  
C                                                                       
cc      CALL  MRDATA( MT,MJ,Z,N,ID )                                      
      CALL MRDATA( ZS,Z,N,ID,C,ZMEAN,ZVARI )
cc      CLOSE( MT )
      N0 = 0                                                            
      NMK = N - LAG                                                     
C                                                                       
C     --  HOUSEHOLDER REDUCTION  --                                     
C                                                                       
cc      CALL  MREDCT( Z,D,NMK,N0,LAG,ID,MJ,MJ1,KSW,X )
      X(1:MJ1,1:MJ4) = 0.0D0
      CALL  MREDCT( Z,NMK,N0,LAG,ID,MJ,MJ1,KSW,X )                    
C                                                                       
C     --  AR-MODEL FITTING (MAICE PROCEDURE)  --                        
C                                                                       
cc      CALL  MARFIT( X,Y,D,NMK,ID,LAG,KSW,MJ1,MJ2,MJ3,MJ4,0,IPR,B,E,EX,C,
cc     *              LMAX,AIC )                                          
      CALL MARFIT( X,NMK,ID,LAG,KSW,MJ1,MJ2,MJ3,MJ4,0,IPR,AIC1,SD1,DIC1,
cx     * AICM,SDM,IM,BI,EI,B,E,EX,CV,LMAX,AICS,JNDF,AF,NPR,AIC,IFG,LU )
     * AICM,SDM,IM,BI,EI,B,E,EX,CV,LMAX,AICS,JNDF,AF,NPR,AIC )
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
cc      CLOSE( LU )
cx      IF( IFG.NE.0 ) CLOSE( LU )
      RETURN
cxx    1 FORMAT( 16I5 )                                                    
cxx    3 FORMAT( ' PROGRAM TIMSAC 78.2.1',/'   MULTI-VARIATE AUTOREGRESSIVE
cxx     * MODEL FITTING  ;  LEAST SQUARES METHOD BY HOUSEHOLDER TRANSFORMAT
cxx     2ION',/,'   < AUTOREGRESSIVE MODEL >',/,1H ,10X,'Z(I) = A(1)*Z(I-1)
cxx     3 + A(2)*Z(I-2) + ... + A(II)*Z(I-II) + ... + A(M)*Z(I-M) + W(I)',/
cxx     4,'   WHERE',/,11X,'M:     ORDER OF THE MODEL',/,11X,'W(I):  ID-DIM
cxx     5ENSIONAL GAUSSIAN WHITE NOISE WITH MEAN 0 AND VARIANCE MATRIX E(M)
cxx     6.' )                                                              
cxx    4 FORMAT( 1H ,'FITTING UP TO THE ORDER  K =',I3,'   IS TRIED',/,' OR
cxx     1IGINAL DATA INPUT DEVICE   MT =',I3 )                             
      E N D                                                             
