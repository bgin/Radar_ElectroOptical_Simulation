C
C     FFTE: A FAST FOURIER TRANSFORM PACKAGE
C
C     (C) COPYRIGHT SOFTWARE, 2000-2004, 2008-2011, 2020
C         ALL RIGHTS RESERVED
C                BY
C         DAISUKE TAKAHASHI
C         CENTER FOR COMPUTATIONAL SCIENCES
C         UNIVERSITY OF TSUKUBA
C         1-1-1 TENNODAI, TSUKUBA, IBARAKI 305-8577, JAPAN
C         E-MAIL: daisuke@cs.tsukuba.ac.jp
C
C
C     1-D COMPLEX FFT ROUTINE (FOR VECTOR MACHINES)
C
C     FORTRAN77 SOURCE PROGRAM
C
C     CALL ZFFT1D(A,N,IOPT,B)
C
C     A(N) IS COMPLEX INPUT/OUTPUT VECTOR (COMPLEX*16)
C     B(N*2) IS WORK/COEFFICIENT VECTOR (COMPLEX*16)
C     N IS THE LENGTH OF THE TRANSFORMS (INTEGER*4)
C       -----------------------------------
C         N = (2**IP) * (3**IQ) * (5**IR)
C       -----------------------------------
C     IOPT = 0 FOR INITIALIZING THE COEFFICIENTS (INTEGER*4)
C          = -1 FOR FORWARD TRANSFORM
C          = +1 FOR INVERSE TRANSFORM
C
C     WRITTEN BY DAISUKE TAKAHASHI
C
      SUBROUTINE ZFFT1D(A,N,IOPT,B)
         !DIR$ OPTIMIZE:3
       !DIR$ CODE_ALIGN : 32 :: ZFFT1D
       !dir$ attributes optimization_parameter: "target_arch=skylake-avx512" :: ZFFT1D
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'param.h'
      COMPLEX*16 A(*),B(*)
      COMPLEX*16 WX(NDA2),WY(NDA2)
      DIMENSION LNX(3),LNY(3)
      SAVE WX,WY
C
      IF (IOPT .EQ. 1) THEN
           !DIR$ ASSUME_ALIGNED A:64
      !DIR$ VECTOR ALIGNED
      !DIR$ VECTOR VECTORLENGTH(8)
      !DIR$ VECTOR ALWAYS   
        DO 10 I=1,N
          A(I)=DCONJG(A(I))
   10   CONTINUE
      END IF
C
      CALL GETNXNY(N,NX,NY)
C
      IF (IOPT .EQ. 0) THEN
        CALL SETTBL(WX,NX)
        CALL SETTBL(WY,NY)
        CALL SETTBL2(B(N+1),NX,NY)
        RETURN
      END IF
C
      CALL FACTOR(NX,LNX)
      CALL FACTOR(NY,LNY)
C
      CALL MFFT235A(A,B,WY,NX,NY,LNY)
      CALL ZTRANSMUL(A,B,B(N+1),NX,NY)
      CALL MFFT235B(B,A,WX,NY,NX,LNX)
C
      IF (IOPT .EQ. 1) THEN
        DN=1.0D0/DBLE(N)

           !DIR$ ASSUME_ALIGNED A:64
      !DIR$ VECTOR ALIGNED
      !DIR$ VECTOR VECTORLENGTH(8)
      !DIR$ VECTOR ALWAYS   
        DO 20 I=1,N
          A(I)=DCONJG(A(I))*DN
   20   CONTINUE
      END IF
      RETURN
      END
      SUBROUTINE ZTRANSMUL(A,B,W,NX,NY)
       !DIR$ OPTIMIZE:3
       !DIR$ ATTRIBUTES FORCEINLINE :: ZTRANSMUL
       !DIR$ CODE_ALIGN : 32 :: ZFFT1D
       !dir$ attributes optimization_parameter: "target_arch=skylake-avx512" :: ZTRANSMUL
      IMPLICIT REAL*8 (A-H,O-Z)
      COMPLEX*16 A(*),B(*),W(*)
      DIMENSION LNX(3),LNY(3)
C
      CALL FACTOR(NX,LNX)
      CALL FACTOR(NY,LNY)
C
      IF (NX .EQ. 1 .OR. NY .EQ. 1) THEN
      !DIR$ ASSUME_ALIGNED A:64,B:64,W:64
      !DIR$ VECTOR ALIGNED
      !DIR$ VECTOR VECTORLENGTH(8)
      !DIR$ UNROLL(8)
      !DIR$ VECTOR ALWAYS   
        DO 10 I=1,NX*NY
          B(I)=A(I)*W(I)
   10   CONTINUE
        RETURN
      END IF
C
      IF (LNX(1)+LNY(1) .LE. 1) THEN
        CALL ZTRANSMULA(A,B,W,NX,NY)
      ELSE
        CALL ZTRANSMULB(A,B,W,NX,NY)
      END IF
      RETURN
      END
      SUBROUTINE ZTRANSMULA(A,B,W,NX,NY)
        !DIR$ OPTIMIZE:3
       !DIR$ ATTRIBUTES FORCEINLINE :: ZTRANSMULA
       !DIR$ CODE_ALIGN : 32 :: ZFFT1D
       !dir$ attributes optimization_parameter: "target_arch=skylake-avx512" :: ZTRANSMULA
      IMPLICIT REAL*8 (A-H,O-Z)
      COMPLEX*16 A(NX,*),B(NY,*),W(NX,*)
C
      DO 20 I=1,NX
      !DIR$ ASSUME_ALIGNED A:64,B:64,W:64
      !DIR$ VECTOR ALIGNED
      !DIR$ VECTOR VECTORLENGTH(8)
      !DIR$ UNROLL(8)
      !DIR$ VECTOR ALWAYS   
        DO 10 J=1,NY
          B(J,I)=A(I,J)*W(I,J)
   10   CONTINUE
   20 CONTINUE
      RETURN
      END
      SUBROUTINE ZTRANSMULB(A,B,W,NX,NY)
        !DIR$ OPTIMIZE:3
       !DIR$ ATTRIBUTES FORCEINLINE :: ZTRANSMULB
       !DIR$ CODE_ALIGN : 32 :: ZFFT1D
       !dir$ attributes optimization_parameter: "target_arch=skylake-avx512" :: ZTRANSMULB
      IMPLICIT REAL*8 (A-H,O-Z)
      COMPLEX*16 A(NX,*),B(NY,*),W(NX,*)
C
      IF (NY .GE. NX) THEN
        DO 20 I=0,NX-1
      !DIR$ ASSUME_ALIGNED A:64,B:64,W:64
      !DIR$ IVDEP
      !DIR$ VECTOR ALIGNED
      !DIR$ VECTOR VECTORLENGTH(8)
      !DIR$ UNROLL(8)
      !DIR$ VECTOR ALWAYS   
          DO 10 J=1,NX-I
            B(J,I+J)=A(I+J,J)*W(I+J,J)
   10     CONTINUE
   20   CONTINUE
        DO 40 I=1,NY-NX
            !DIR$ ASSUME_ALIGNED A:64,B:64,W:64
      !DIR$ IVDEP
      !DIR$ VECTOR ALIGNED
      !DIR$ VECTOR VECTORLENGTH(8)
      !DIR$ UNROLL(8)
      !DIR$ VECTOR ALWAYS   
          DO 30 J=1,NX
            B(I+J,J)=A(J,I+J)*W(J,I+J)
   30     CONTINUE
   40   CONTINUE
        DO 60 I=NY-NX+1,NY-1
            !DIR$ ASSUME_ALIGNED A:64,B:64,W:64
      !DIR$ IVDEP
      !DIR$ VECTOR ALIGNED
      !DIR$ VECTOR VECTORLENGTH(8)
      !DIR$ UNROLL(8)
      !DIR$ VECTOR ALWAYS   
          DO 50 J=1,NY-I
            B(I+J,J)=A(J,I+J)*W(J,I+J)
   50     CONTINUE
   60   CONTINUE
      ELSE
        DO 80 I=0,NY-1
            !DIR$ ASSUME_ALIGNED A:64,B:64,W:64
      !DIR$ IVDEP
      !DIR$ VECTOR ALIGNED
      !DIR$ VECTOR VECTORLENGTH(8)
      !DIR$ UNROLL(8)
      !DIR$ VECTOR ALWAYS   
          DO 70 J=1,NY-I
            B(I+J,J)=A(J,I+J)*W(J,I+J)
   70     CONTINUE
   80   CONTINUE
        DO 100 I=1,NX-NY
            !DIR$ ASSUME_ALIGNED A:64,B:64,W:64
      !DIR$ IVDEP
      !DIR$ VECTOR ALIGNED
      !DIR$ VECTOR VECTORLENGTH(8)
      !DIR$ UNROLL(8)
      !DIR$ VECTOR ALWAYS   
          DO 90 J=1,NY
            B(J,I+J)=A(I+J,J)*W(I+J,J)
   90     CONTINUE
  100   CONTINUE
        DO 120 I=NX-NY+1,NX-1
             !DIR$ ASSUME_ALIGNED A:64,B:64,W:64
      !DIR$ IVDEP
      !DIR$ VECTOR ALIGNED
      !DIR$ VECTOR VECTORLENGTH(8)
      !DIR$ UNROLL(8)
      !DIR$ VECTOR ALWAYS   
          DO 110 J=1,NX-I
            B(J,I+J)=A(I+J,J)*W(I+J,J)
  110     CONTINUE
  120   CONTINUE
      END IF
      RETURN
      END
