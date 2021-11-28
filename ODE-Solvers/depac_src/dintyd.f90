!** DINTYD
PURE SUBROUTINE DINTYD(T,K,Yh,Nyh,Dky,Iflag)
  !> Subsidiary to DDEBDF
  !***
  ! **Library:**   SLATEC
  !***
  ! **Type:**      DOUBLE PRECISION (INTYD-S, DINTYD-D)
  !***
  ! **Author:**  Watts, H. A., (SNLA)
  !***
  ! **Description:**
  !
  !   DINTYD approximates the solution and derivatives at T by polynomial
  !   interpolation. Must be used in conjunction with the integrator
  !   package DDEBDF.
  ! ----------------------------------------------------------------------
  ! DINTYD computes interpolated values of the K-th derivative of the
  ! dependent variable vector Y, and stores it in DKY.
  ! This routine is called by DDEBDF with K = 0,1 and T = TOUT, but may
  ! also be called by the user for any K up to the current order.
  ! (see detailed instructions in LSODE usage documentation.)
  ! ----------------------------------------------------------------------
  ! The computed values in DKY are gotten by interpolation using the
  ! Nordsieck history array YH.  This array corresponds uniquely to a
  ! vector-valued polynomial of degree NQCUR or less, and DKY is set
  ! to the K-th derivative of this polynomial at T.
  ! The formula for DKY is..
  !              Q
  !  DKY(I)  =  Sum  C(J,K) * (T - TN)**(J-K) * H**(-J) * YH(I,J+1)
  !             J=K
  ! where  C(J,K) = J*(J-1)*...*(J-K+1), Q = NQCUR, TN = TCUR, H = HCUR.
  ! The quantities  NQ = NQCUR, L = NQ+1, N = NEQ, TN, and H are
  ! communicated by common.  The above sum is done in reverse order.
  ! IFLAG is returned negative if either K or T is out of bounds.
  ! ----------------------------------------------------------------------
  !
  !***
  ! **See also:**  DDEBDF
  !***
  ! **Routines called:**  (NONE)
  !***
  ! COMMON BLOCKS    DDEBD1

  !* REVISION HISTORY  (YYMMDD)
  !   820301  DATE WRITTEN
  !   890911  Removed unnecessary intrinsics.  (WRB)
  !   891214  Prologue converted to Version 4.0 format.  (BAB)
  !   900328  Added TYPE section.  (WRB)
  !   910722  Updated AUTHOR section.  (ALS)
  USE DDEBD1, ONLY : h_com, hu_com, tn_com, uround_com, l_com, n_com, nq_com
  !
  INTEGER, INTENT(IN) :: K, Nyh
  INTEGER, INTENT(OUT) :: Iflag
  REAL(DP), INTENT(IN) :: T
  REAL(DP), INTENT(IN) :: Yh(Nyh,nq_com+1)
  REAL(DP), INTENT(OUT) :: Dky(Nyh)
  !
  INTEGER :: i, ic, j, jb, jb2, jj, jj1, jp1
  REAL(DP) :: c, r, s, tp
  !
  !     BEGIN BLOCK PERMITTING ...EXITS TO 130
  !* FIRST EXECUTABLE STATEMENT  DINTYD
  Iflag = 0
  IF( K<0 .OR. K>nq_com ) THEN
    !
    Iflag = -1
  ELSE
    tp = tn_com - hu_com*(1.0_DP+100._DP*uround_com)
    IF( (T-tp)*(T-tn_com)<=0._DP ) THEN
      !
      s = (T-tn_com)/h_com
      ic = 1
      IF( K/=0 ) THEN
        jj1 = l_com - K
        DO jj = jj1, nq_com
          ic = ic*jj
        END DO
      END IF
      c = ic
      DO i = 1, n_com
        Dky(i) = c*Yh(i,l_com)
      END DO
      IF( K/=nq_com ) THEN
        jb2 = nq_com - K
        DO jb = 1, jb2
          j = nq_com - jb
          jp1 = j + 1
          ic = 1
          IF( K/=0 ) THEN
            jj1 = jp1 - K
            DO jj = jj1, j
              ic = ic*jj
            END DO
          END IF
          c = ic
          DO i = 1, n_com
            Dky(i) = c*Yh(i,jp1) + s*Dky(i)
          END DO
        END DO
        !     .........EXIT
        IF( K==0 ) RETURN
      END IF
      r = h_com**(-K)
      DO i = 1, n_com
        Dky(i) = r*Dky(i)
      END DO
    ELSE
      !     .........EXIT
      Iflag = -2
    END IF
  END IF
  !----------------------- END OF SUBROUTINE DINTYD -----------------------
  RETURN
END SUBROUTINE DINTYD