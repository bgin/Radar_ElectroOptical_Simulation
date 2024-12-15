
 
!C     NUMERICAL METHODS: FORTRAN Programs, (c) John H. Mathews 1994
!C     To accompany the text:
!C     NUMERICAL METHODS for Mathematics, Science and Engineering, 2nd Ed, 1992
!C     Prentice Hall, Englewood Cliffs, New Jersey, 07632, U.S.A.
!C     This free software is complements of the author.
!C
!C     Algorithm 6.1 (Differentiation Using Limits).
!C     Section 6.1, Approximating the Derivative, Page 326

      SUBROUTINE DLIMIT(F,D,DX,E,X,H,M,Tol)
      PARAMETER(Max=100)
      INTEGER M,N
      REAL D,DX,E,H,R,X
      DIMENSION D(0:Max),DX(0:Max),E(0:Max),R(0:Max)
      CHARACTER ANS*1
      EXTERNAL F
      Small=1E-9
      H = 1
      DX(0) = H
      D(0) = 0.5*(F(X+H)-F(X-H))/H
      DO N=1,2
        H = H/2
        DX(N) = H
        D(N) = 0.5*(F(X+H)-F(X-H))/H
        E(N) = ABS(D(N)-D(N-1))
        R(N) = 2*E(N)/(ABS(D(N))+ABS(D(N-1))+Small)
      ENDDO
      N = 1
      WHILE ((E(N).GT.E(N+1) .OR. R(N).GT.Tol).AND.(N.LT.Max))
        H = H/2
        DX(N+2) = H
        D(N+2) = 0.5*(F(X+H)-F(X-H))/H
        E(N+2) = ABS(D(N+2)-D(N+1))
        R(N+2) = 2*E(N+2)/(ABS(D(N+2))+ABS(D(N+1))+Small)
        N = N+1
      REPEAT
      M=N
      RETURN
      END

      SUBROUTINE XLIMIT(F,D,DX,E,X,H,M,Tol)
!C     This subroutine uses labeled DO loop(s).
      PARAMETER(Max=100)
      INTEGER M,N
      REAL D,DX,E,H,R,X
      DIMENSION D(0:Max),DX(0:Max),E(0:Max),R(0:Max)
      CHARACTER ANS*1
      EXTERNAL F
      Small=1E-9
      H = 1
      DX(0) = H
      D(0) = 0.5*(F(X+H)-F(X-H))/H
      DO 10 N=1,2
        H = H/2
        DX(N) = H
        D(N) = 0.5*(F(X+H)-F(X-H))/H
        E(N) = ABS(D(N)-D(N-1))
        R(N) = 2*E(N)/(ABS(D(N))+ABS(D(N-1))+Small)
10    CONTINUE
      N = 1
20    IF ((E(N).GT.E(N+1) .OR. R(N).GT.Tol).AND.(N.LT.Max)) THEN
        H = H/2
        DX(N+2) = H
        D(N+2) = 0.5*(F(X+H)-F(X-H))/H
        E(N+2) = ABS(D(N+2)-D(N+1))
        R(N+2) = 2*E(N+2)/(ABS(D(N+2))+ABS(D(N+1))+Small)
        N = N+1
        GOTO 20
      ENDIF
      M=N
      RETURN
      END


      SUBROUTINE EXTRAP(F,D,DX,X,H,Error,N,Tol,Delta)
      PARAMETER(MaxN=15)
      INTEGER J,K,N
      REAL D,DX,Error,H,RelErr,Small,X
      DIMENSION D(0:MaxN,0:MaxN),DX(0:MaxN)
      EXTERNAL F
      Small=1E-7
      H=1
      DX(0) = H
      J=1
      Error=1
      RelErr=1
      D(0,0)=0.5*(F(X+H)-F(X-H))/H
      WHILE ( RelErr.GT.Tol .AND. Error.GT.Delta .AND. J.LT.16)
        H=H/2
        DX(J) = H
        D(J,0)=0.5*(F(X+H)-F(X-H))/H
        DO K=1,J
          D(J,K)=D(J,K-1)+(D(J,K-1)-D(J-1,K-1))/(4**K-1)
        ENDDO
        Error=ABS(D(J,J)-D(J-1,J-1))
        RelErr=2*Error/(ABS(D(J,J))+ABS(D(J-1,J-1))+Small)
        N=J
        J=J+1
      REPEAT
      RETURN
      END

      SUBROUTINE XEXTRAP(F,D,DX,X,H,Error,N,Tol,Delta)
!C     This subroutine uses simulated WHILE loop(s).
      PARAMETER(MaxN=15)
      INTEGER J,K,N
      REAL D,DX,Error,H,RelErr,Small,X
      DIMENSION D(0:MaxN,0:MaxN),DX(0:MaxN)
      EXTERNAL F
      Small=1E-7
      H=1
      DX(0) = H
      J=1
      Error=1
      RelErr=1
      D(0,0)=0.5*(F(X+H)-F(X-H))/H
10    IF ( RelErr.GT.Tol .AND. Error.GT.Delta .AND. J.LT.16) THEN
      H=H/2
      DX(J) = H
      D(J,0)=0.5*(F(X+H)-F(X-H))/H
      DO 20 K=1,J
        D(J,K)=D(J,K-1)+(D(J,K-1)-D(J-1,K-1))/(4**K-1)
20    CONTINUE
        Error=ABS(D(J,J)-D(J-1,J-1))
        RelErr=2*Error/(ABS(D(J,J))+ABS(D(J-1,J-1))+Small)
        N=J
        J=J+1
        GOTO 10
      ENDIF
      RETURN
      END


      SUBROUTINE DIFPOLY(A,X,Y,N,Df)
      PARAMETER(MaxN=15)
      INTEGER J,K,N
      REAL A,Df,Prod,T,X,Y
      DIMENSION A(0:MaxN),X(0:MaxN),Y(0:MaxN)
      DO K=0,N
        A(K)=Y(K)
      ENDDO
      DO J=1,N
        DO K=N,J,-1
          A(K)=(A(K)-A(K-1))/(X(K)-X(K-J))
        ENDDO
      ENDDO
      T=X(0)
      Df=A(1)
      Prod=1
      DO K=2,N
        Prod=Prod*(T-X(K-1))
        Df=Df+Prod*A(K)
      ENDDO
      RETURN
      END

      SUBROUTINE XDIFPOLY(A,X,Y,N,Df)
!C     This subroutine uses labeled DO loop(s).
      PARAMETER(MaxN=15)
      INTEGER J,K,N
      REAL A,Df,Prod,T,X,Y
      DIMENSION A(0:MaxN),X(0:MaxN),Y(0:MaxN)
      DO 10 K=0,N
        A(K)=Y(K)
10    CONTINUE
      DO 30 J=1,N
      DO 20 K=N,J,-1
        A(K)=(A(K)-A(K-1))/(X(K)-X(K-J))
20    CONTINUE
30    CONTINUE
      T=X(0)
      Df=A(1)
      Prod=1
      DO 40 K=2,N
        Prod=Prod*(T-X(K-1))
        Df=Df+Prod*A(K)
40    CONTINUE
      RETURN
      END
      

