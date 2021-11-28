!** DPJAC
SUBROUTINE DPJAC(Neq,Y,Yh,Nyh,Ewt,Ftem,Savf,Wm,Iwm,DF,DJAC)
  !> Subsidiary to DDEBDF
  !***
  ! **Library:**   SLATEC
  !***
  ! **Type:**      DOUBLE PRECISION (PJAC-S, DPJAC-D)
  !***
  ! **Author:**  Watts, H. A., (SNLA)
  !***
  ! **Description:**
  !
  !   DPJAC sets up the iteration matrix (involving the Jacobian) for the
  !   integration package DDEBDF.
  !
  !***
  ! **See also:**  DDEBDF
  !***
  ! **Routines called:**  DGBFA, DGEFA, DVNRMS
  !***
  ! COMMON BLOCKS    DDEBD1

  !* REVISION HISTORY  (YYMMDD)
  !   820301  DATE WRITTEN
  !   890531  Changed all specific intrinsics to generic.  (WRB)
  !   890911  Removed unnecessary intrinsics.  (WRB)
  !   891214  Prologue converted to Version 4.0 format.  (BAB)
  !   900328  Added TYPE section.  (WRB)
  !   910722  Updated AUTHOR section.  (ALS)
  !   920422  Changed DIMENSION statement.  (WRB)
  USE DDEBD1, ONLY : el0_com, h_com, tn_com, uround_com, ier_com, miter_com, n_com, &
    nfe_com, nje_com
  USE linpack, ONLY : DGBFA, DGEFA
  !
  INTERFACE
    SUBROUTINE DF(X,U,Uprime)
      IMPORT DP
      REAL(DP), INTENT(IN) :: X
      REAL(DP), INTENT(IN) :: U(:)
      REAL(DP), INTENT(OUT) :: Uprime(:)
    END SUBROUTINE DF
    PURE SUBROUTINE DJAC(X,U,Pd,Nrowpd)
      IMPORT DP
      INTEGER, INTENT(IN) :: Nrowpd
      REAL(DP), INTENT(IN) :: X
      REAL(DP), INTENT(IN) :: U(:)
      REAL(DP), INTENT(OUT) :: Pd(:,:)
    END SUBROUTINE DJAC
  END INTERFACE
  INTEGER, INTENT(IN) :: Neq, Nyh
  INTEGER, INTENT(INOUT) :: Iwm(:)
  REAL(DP), INTENT(IN) :: Yh(Nyh,n_com), Ewt(n_com), Savf(n_com)
  REAL(DP), INTENT(INOUT) :: Y(Neq), Wm(:)
  REAL(DP), INTENT(OUT) :: Ftem(n_com)
  !
  INTEGER :: i, i1, i2, ii, j, j1, jj, mba, mband, meb1, meband, ml, ml3, mu
  REAL(DP) :: con, di, fac, hl0, r, r0, srur, yi, yj, yjj
  REAL(DP), ALLOCATABLE :: pd(:,:)
  !     ------------------------------------------------------------------
  !      DPJAC IS CALLED BY DSTOD  TO COMPUTE AND PROCESS THE MATRIX
  !      P = I - H*EL(1)*J, WHERE J IS AN APPROXIMATION TO THE JACOBIAN.
  !      HERE J IS COMPUTED BY THE USER-SUPPLIED ROUTINE DJAC IF
  !      MITER = 1 OR 4, OR BY FINITE DIFFERENCING IF MITER = 2, 3, OR 5.
  !      IF MITER = 3, A DIAGONAL APPROXIMATION TO J IS USED.
  !      J IS STORED IN WM AND REPLACED BY P.  IF MITER /= 3, P IS THEN
  !      SUBJECTED TO LU DECOMPOSITION IN PREPARATION FOR LATER SOLUTION
  !      OF LINEAR SYSTEMS WITH P AS COEFFICIENT MATRIX. THIS IS DONE
  !      BY DGEFA IF MITER = 1 OR 2, AND BY DGBFA IF MITER = 4 OR 5.
  !
  !      IN ADDITION TO VARIABLES DESCRIBED PREVIOUSLY, COMMUNICATION
  !      WITH DPJAC USES THE FOLLOWING..
  !      Y    = ARRAY CONTAINING PREDICTED VALUES ON ENTRY.
  !      FTEM = WORK ARRAY OF LENGTH N (ACOR IN DSTOD ).
  !      SAVF = ARRAY CONTAINING DF EVALUATED AT PREDICTED Y.
  !      WM   = DOUBLE PRECISION WORK SPACE FOR MATRICES.  ON OUTPUT IT
  !      CONTAINS THE
  !             INVERSE DIAGONAL MATRIX IF MITER = 3 AND THE LU
  !             DECOMPOSITION OF P IF MITER IS 1, 2, 4, OR 5.
  !             STORAGE OF MATRIX ELEMENTS STARTS AT WM(3).
  !             WM ALSO CONTAINS THE FOLLOWING MATRIX-RELATED DATA..
  !             WM(1) = SQRT(UROUND), USED IN NUMERICAL JACOBIAN
  !             INCREMENTS.  WM(2) = H*EL0, SAVED FOR LATER USE IF MITER =
  !             3.
  !      IWM  = INTEGER WORK SPACE CONTAINING PIVOT INFORMATION, STARTING
  !             AT IWM(21), IF MITER IS 1, 2, 4, OR 5.  IWM ALSO CONTAINS
  !             THE BAND PARAMETERS ML = IWM(1) AND MU = IWM(2) IF MITER
  !             IS 4 OR 5.
  !      EL0  = EL(1) (INPUT).
  !      IER  = OUTPUT ERROR FLAG,  = 0 IF NO TROUBLE, /= 0 IF
  !             P MATRIX FOUND TO BE SINGULAR.
  !      THIS ROUTINE ALSO USES THE COMMON VARIABLES EL0, H, TN, UROUND,
  !      MITER, N, NFE, AND NJE.
  !-----------------------------------------------------------------------
  !     BEGIN BLOCK PERMITTING ...EXITS TO 240
  !        BEGIN BLOCK PERMITTING ...EXITS TO 220
  !           BEGIN BLOCK PERMITTING ...EXITS TO 130
  !              BEGIN BLOCK PERMITTING ...EXITS TO 70
  !* FIRST EXECUTABLE STATEMENT  DPJAC
  nje_com = nje_com + 1
  hl0 = h_com*el0_com
  SELECT CASE (miter_com)
    CASE (2)
      !                 IF MITER = 2, MAKE N CALLS TO DF TO APPROXIMATE J.
      !                 --------------------
      fac = DVNRMS(n_com,Savf,Ewt)
      r0 = 1000._DP*ABS(h_com)*uround_com*n_com*fac
      IF( r0==0._DP ) r0 = 1._DP
      srur = Wm(1)
      j1 = 2
      DO j = 1, n_com
        yj = Y(j)
        r = MAX(srur*ABS(yj),r0*Ewt(j))
        Y(j) = Y(j) + r
        fac = -hl0/r
        CALL DF(tn_com,Y,Ftem)
        DO i = 1, n_com
          Wm(i+j1) = (Ftem(i)-Savf(i))*fac
        END DO
        Y(j) = yj
        j1 = j1 + n_com
      END DO
      nfe_com = nfe_com + n_com
    CASE (3)
      ! IF MITER = 3, CONSTRUCT A DIAGONAL APPROXIMATION TO J AND P. ---------
      Wm(2) = hl0
      ier_com = 0
      r = el0_com*0.1_DP
      DO i = 1, n_com
        Y(i) = Y(i) + r*(h_com*Savf(i)-Yh(i,2))
      END DO
      CALL DF(tn_com,Y,Wm(3:Neq+2))
      nfe_com = nfe_com + 1
      DO i = 1, n_com
        r0 = h_com*Savf(i) - Yh(i,2)
        di = 0.1_DP*r0 - h_com*(Wm(i+2)-Savf(i))
        Wm(i+2) = 1._DP
        IF( ABS(r0)>=uround_com*Ewt(i) ) THEN
          !           .........EXIT
          IF( ABS(di)==0._DP ) GOTO 100
          Wm(i+2) = 0.1_DP*r0/di
        END IF
      END DO
      !     .........EXIT
      RETURN
    CASE (4)
      !           IF MITER = 4, CALL DJAC AND MULTIPLY BY SCALAR.
      !           -----------------------
      ml = Iwm(1)
      mu = Iwm(2)
      ml3 = 3
      mband = ml + mu + 1
      meband = 2*ml + mu + 1
      ALLOCATE( pd(meband,n_com) )
      pd = 0._DP
      CALL DJAC(tn_com,Y,pd,meband)
      con = -hl0
      DO j = 1, n_com
        DO i = 1, meband
          Wm( 2+(j-1)*meband+i ) = pd(i,j)*con
        END DO
      END DO
      GOTO 200
    CASE (5)
      !           IF MITER = 5, MAKE MBAND CALLS TO DF TO APPROXIMATE J.
      !           ----------------
      ml = Iwm(1)
      mu = Iwm(2)
      mband = ml + mu + 1
      mba = MIN(mband,n_com)
      meband = mband + ml
      meb1 = meband - 1
      srur = Wm(1)
      fac = DVNRMS(n_com,Savf,Ewt)
      r0 = 1000._DP*ABS(h_com)*uround_com*n_com*fac
      IF( r0==0._DP ) r0 = 1._DP
      DO j = 1, mba
        DO i = j, n_com, mband
          yi = Y(i)
          r = MAX(srur*ABS(yi),r0*Ewt(i))
          Y(i) = Y(i) + r
        END DO
        CALL DF(tn_com,Y,Ftem)
        DO jj = j, n_com, mband
          Y(jj) = Yh(jj,1)
          yjj = Y(jj)
          r = MAX(srur*ABS(yjj),r0*Ewt(jj))
          fac = -hl0/r
          i1 = MAX(jj-mu,1)
          i2 = MIN(jj+ml,n_com)
          ii = jj*meb1 - ml + 2
          DO i = i1, i2
            Wm(ii+i) = (Ftem(i)-Savf(i))*fac
          END DO
        END DO
      END DO
      nfe_com = nfe_com + mba
      GOTO 200
    CASE DEFAULT
      !                 IF MITER = 1, CALL DJAC AND MULTIPLY BY SCALAR.
      !                 -----------------------
      ALLOCATE( pd(n_com,n_com) )
      pd = 0._DP
      CALL DJAC(tn_com,Y,pd,n_com)
      con = -hl0
      DO j = 1, n_com
        DO i = 1, n_com
          Wm( 2+(j-1)*n_com+i ) = pd(i,j)*con
        END DO
      END DO
  END SELECT
  !              ADD IDENTITY MATRIX.
  !              -------------------------------------------------
  j = 3
  DO i = 1, n_com
    Wm(j) = Wm(j) + 1._DP
    j = j + (n_com+1)
  END DO
  !              DO LU DECOMPOSITION ON P.
  !              --------------------------------------------
  CALL DGEFA(Wm(3:n_com**2+2),n_com,n_com,Iwm(21:n_com+20),ier_com)
  !     .........EXIT
  RETURN
  100  ier_com = -1
  !     ......EXIT
  RETURN
  !        ADD IDENTITY MATRIX.
  !        -------------------------------------------------
  200  ii = mband + 2
  DO i = 1, n_com
    Wm(ii) = Wm(ii) + 1._DP
    ii = ii + meband
  END DO
  !        DO LU DECOMPOSITION OF P.
  !        --------------------------------------------
  CALL DGBFA(Wm(3:meband*n_com+2),meband,n_com,ml,mu,Iwm(21:n_com+20),ier_com)
  !----------------------- END OF SUBROUTINE DPJAC -----------------------
  RETURN
END SUBROUTINE DPJAC