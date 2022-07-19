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
C     3-D REAL-TO-COMPLEX FFT ROUTINE
C
C     FORTRAN77 SOURCE PROGRAM
C
C     CALL DZFFT3D(A,NX,NY,NZ,IOPT,B)
C
C     A(NX,NY,NZ) IS REAL INPUT VECTOR (REAL*8)
C     A(NX/2+1,NY,NZ) IS COMPLEX OUTPUT VECTOR (COMPLEX*16)
C     B(NX/2+1,NY,NZ) IS WORK VECTOR (COMPLEX*16)
C     NX IS THE LENGTH OF THE TRANSFORMS IN THE X-DIRECTION (INTEGER*4)
C     NY IS THE LENGTH OF THE TRANSFORMS IN THE Y-DIRECTION (INTEGER*4)
C     NZ IS THE LENGTH OF THE TRANSFORMS IN THE Z-DIRECTION (INTEGER*4)
C       ------------------------------------
C         NX = (2**IP) * (3**IQ) * (5**IR)
C         NY = (2**JP) * (3**JQ) * (5**JR)
C         NZ = (2**KP) * (3**KQ) * (5**KR)
C       ------------------------------------
C     IOPT = 0 FOR INITIALIZING THE COEFFICIENTS (INTEGER*4)
C          = -1 FOR FORWARD TRANSFORM
C
C     WRITTEN BY DAISUKE TAKAHASHI
C
      SUBROUTINE DZFFT3D(A,NX,NY,NZ,IOPT,B)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'param.h'
      COMPLEX*16 A(*),B(*)
      COMPLEX*16 C((NDA3+NP)*NBLK),D(NDA3)
      COMPLEX*16 WX(NDA3),WY(NDA3),WZ(NDA3)
      DIMENSION LNX(3),LNY(3),LNZ(3)
      SAVE WX,WY,WZ
C
      IF (IOPT .EQ. 0) THEN
        CALL SETTBL(WX,NX)
        CALL SETTBL(WY,NY)
        CALL SETTBL(WZ,NZ)
        RETURN
      END IF
C
      CALL FACTOR(NX,LNX)
      CALL FACTOR(NY,LNY)
      CALL FACTOR(NZ,LNZ)
C
!$OMP PARALLEL PRIVATE(C,D)
      CALL DZFFT3D0(A,A,B,C,C,C,D,WX,WY,WZ,NX,NY,NZ,LNX,LNY,LNZ)
!$OMP END PARALLEL
      RETURN
      END
      SUBROUTINE DZFFT3D0(DA,A,B,CX,CY,CZ,D,WX,WY,WZ,NX,NY,NZ,
     1                    LNX,LNY,LNZ)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'param.h'
      COMPLEX*16 A(NX/2+1,NY,*),B(NX/2+1,NY,*)
      COMPLEX*16 CX(*),CY(NY+NP,*),CZ(NZ+NP,*),D(*)
      COMPLEX*16 WX(*),WY(*),WZ(*)
      DIMENSION DA(NX,NY,*)
      DIMENSION LNX(*),LNY(*),LNZ(*)
C
      IF (MOD(NY,2) .EQ. 0) THEN
!$OMP DO PRIVATE(I,II,J)
        DO 100 K=1,NZ
          DO 30 J=1,NY,2
            DO 10 I=1,NX
              CX(I)=DCMPLX(DA(I,J,K),DA(I,J+1,K))
   10       CONTINUE
            CALL FFT235(CX,D,WX,NX,LNX)
            B(1,J,K)=DBLE(CX(1))
            B(1,J+1,K)=DIMAG(CX(1))
!DIR$ VECTOR ALIGNED
            DO 20 I=2,NX/2+1
              B(I,J,K)=0.5D0*(CX(I)+DCONJG(CX(NX-I+2)))
              B(I,J+1,K)=(0.0D0,-0.5D0)*(CX(I)-DCONJG(CX(NX-I+2)))
   20       CONTINUE
   30     CONTINUE
          DO 90 II=1,NX/2+1,NBLK
            DO 50 I=II,MIN0(II+NBLK-1,NX/2+1)
!DIR$ VECTOR ALIGNED
              DO 40 J=1,NY
                CY(J,I-II+1)=B(I,J,K)
   40         CONTINUE
   50       CONTINUE
            DO 60 I=II,MIN0(II+NBLK-1,NX/2+1)
              CALL FFT235(CY(1,I-II+1),D,WY,NY,LNY)
   60       CONTINUE
            DO 80 J=1,NY
!DIR$ VECTOR ALIGNED
              DO 70 I=II,MIN0(II+NBLK-1,NX/2+1)
                B(I,J,K)=CY(J,I-II+1)
   70         CONTINUE
   80       CONTINUE
   90     CONTINUE
  100   CONTINUE
      ELSE
!$OMP DO PRIVATE(I,II,J)
        DO 220 K=1,NZ
          DO 130 J=1,NY-1,2
            DO 110 I=1,NX
              CX(I)=DCMPLX(DA(I,J,K),DA(I,J+1,K))
  110       CONTINUE
            CALL FFT235(CX,D,WX,NX,LNX)
            B(1,J,K)=DBLE(CX(1))
            B(1,J+1,K)=DIMAG(CX(1))
!DIR$ VECTOR ALIGNED
            DO 120 I=2,NX/2+1
              B(I,J,K)=0.5D0*(CX(I)+DCONJG(CX(NX-I+2)))
              B(I,J+1,K)=(0.0D0,-0.5D0)*(CX(I)-DCONJG(CX(NX-I+2)))
  120       CONTINUE
  130     CONTINUE
          DO 140 I=1,NX
            CX(I)=DCMPLX(DA(I,NY,K),0.0D0)
  140     CONTINUE
          CALL FFT235(CX,D,WX,NX,LNX)
!DIR$ VECTOR ALIGNED
          DO 150 I=1,NX/2+1
            B(I,NY,K)=CX(I)
  150     CONTINUE
          DO 210 II=1,NX/2+1,NBLK
            DO 170 I=II,MIN0(II+NBLK-1,NX/2+1)
!DIR$ VECTOR ALIGNED
              DO 160 J=1,NY
                CY(J,I-II+1)=B(I,J,K)
  160         CONTINUE
  170       CONTINUE
            DO 180 I=II,MIN0(II+NBLK-1,NX/2+1)
              CALL FFT235(CY(1,I-II+1),D,WY,NY,LNY)
  180       CONTINUE
            DO 200 J=1,NY
!DIR$ VECTOR ALIGNED
              DO 190 I=II,MIN0(II+NBLK-1,NX/2+1)
                B(I,J,K)=CY(J,I-II+1)
  190         CONTINUE
  200       CONTINUE
  210     CONTINUE
  220   CONTINUE
      END IF
!$OMP DO PRIVATE(I,II,K)
      DO 290 J=1,NY
        DO 280 II=1,NX/2+1,NBLK
          DO 240 I=II,MIN0(II+NBLK-1,NX/2+1)
!DIR$ VECTOR ALIGNED
            DO 230 K=1,NZ
              CZ(K,I-II+1)=B(I,J,K)
  230       CONTINUE
  240     CONTINUE
          DO 250 I=II,MIN0(II+NBLK-1,NX/2+1)
            CALL FFT235(CZ(1,I-II+1),D,WZ,NZ,LNZ)
  250     CONTINUE
          DO 270 K=1,NZ
!DIR$ VECTOR ALIGNED
            DO 260 I=II,MIN0(II+NBLK-1,NX/2+1)
              A(I,J,K)=CZ(K,I-II+1)
  260       CONTINUE
  270     CONTINUE
  280   CONTINUE
  290 CONTINUE
      RETURN
      END
