

module integrators


!======================================================================================!
! Various collected SLATEC integrator subroutines.                                     !
! Reference:                                                                           !
!              https://github.com/MehdiChinoune/SLATEC/tree/master/src/diff_integ      !
!======================================================================================!

    use mod_kinds, only : i4,sp,dp
    implicit none
    public
    
     REAL(sp), PARAMETER, PRIVATE    :: log10_radix_sp = LOG10( REAL( RADIX(1._sp), sp ) )
     REAL(sp), PARAMETER, PRIVATE    :: eps_sp         = EPSILON(1._sp)
     INTEGER(i4), PARAMETER, PRIVATE :: digits_sp      = DIGITS(1._sp)
     REAL(dp), PARAMETER, PRIVATE    :: log10_radix_dp = LOG10( REAL( RADIX(1._dp), DP ) )
     REAL(dp), PARAMETER, PRIVATE    :: eps_dp         = EPSILON(1._dp)
     INTEGER(i4), PARAMETER, PRIVATE :: digits_dp      = DIGITS(1._dp)
    contains

    !** QNC79
PURE SUBROUTINE QNC79(FUN,A,B,Err,Ans,Ierr,K)
     
  !> Integrate a function using a 7-point adaptive Newton-Cotes quadrature rule.
  !***
  ! **Library:**   SLATEC
  !***
  ! **Category:**  H2A1A1
  !***
  ! **Type:**      SINGLE PRECISION (QNC79-S, DQNC79-D)
  !***
  ! **Keywords:**  ADAPTIVE QUADRATURE, INTEGRATION, NEWTON-COTES
  !***
  ! **Author:**  Kahaner, D. K., (NBS)
  !           Jones, R. E., (SNLA)
  !***
  ! **Description:**
  !
  !     Abstract
  !       QNC79 is a general purpose program for evaluation of
  !       one dimensional integrals of user defined functions.
  !       QNC79 will pick its own points for evaluation of the
  !       integrand and these will vary from problem to problem.
  !       Thus, QNC79 is not designed to integrate over data sets.
  !       Moderately smooth integrands will be integrated efficiently
  !       and reliably.  For problems with strong singularities,
  !       oscillations etc., the user may wish to use more sophis-
  !       ticated routines such as those in QUADPACK.  One measure
  !       of the reliability of QNC79 is the output parameter K,
  !       giving the number of integrand evaluations that were needed.
  !
  !     Description of Arguments
  !
  !     --Input--
  !       FUN  - name of external function to be integrated.  This name
  !              must be in an EXTERNAL statement in your calling
  !              program.  You must write a Fortran function to evaluate
  !              FUN.  This should be of the form
  !                    REAL FUNCTION FUN (X)
  !              C
  !              C     X can vary from A to B
  !              C     FUN(X) should be finite for all X on interval.
  !              C
  !                    FUN = ...
  !                    RETURN
  !                    END
  !       A    - lower limit of integration
  !       B    - upper limit of integration (may be less than A)
  !       ERR  - is a requested error tolerance.  Normally, pick a value
  !              0 < ERR < 1.0E-3.
  !
  !     --Output--
  !       ANS  - computed value of the integral.  Hopefully, ANS is
  !              accurate to within ERR * integral of ABS(FUN(X)).
  !       IERR - a status code
  !            - Normal codes
  !               1  ANS most likely meets requested error tolerance.
  !              -1  A equals B, or A and B are too nearly equal to
  !                  allow normal integration.  ANS is set to zero.
  !            - Abnormal code
  !               2  ANS probably does not meet requested error tolerance.
  !       K    - the number of function evaluations actually used to do
  !              the integration.  A value of K > 1000 indicates a
  !              difficult problem; other programs may be more efficient.
  !              QNC79 will gracefully give up if K exceeds 2000.
  !
  !***
  ! **References:**  (NONE)
  !***
  ! **Routines called:**  I1MACH, R1MACH, XERMSG

  !* REVISION HISTORY  (YYMMDD)
  !   790601  DATE WRITTEN
  !   890531  Changed all specific intrinsics to generic.  (WRB)
  !   890531  REVISION DATE from Version 3.2
  !   891214  Prologue converted to Version 4.0 format.  (BAB)
  !   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
  !   920218  Code and prologue polished.  (WRB)
  !USE service, ONLY : log10_radix_sp, eps_sp, digits_sp
  !     .. Function Arguments ..
  INTERFACE
    REAL(sp) PURE FUNCTION FUN(X)
      IMPORT SP
      REAL(sp), INTENT(IN) :: X
    END FUNCTION FUN
  END INTERFACE
  !     .. Scalar Arguments ..
  INTEGER(i4), INTENT(OUT) :: Ierr, K
  REAL(sp), INTENT(IN) :: A, B, Err
  REAL(sp), INTENT(OUT) :: Ans
  !     .. Local Scalars ..
  REAL(sp) :: ae, area, bank, blocal, c, ce, ee, ef, eps, q13, q7, q7l, test, tol, vr
  INTEGER(i4) :: i, l, lmn, lmx, nib
  !     .. Local Arrays ..
  REAL(sp) :: aa(40), f(13), f1(40), f2(40), f3(40), f4(40), f5(40), f6(40), &
    f7(40), hh(40), q7r(40), vl(40)
  INTEGER(i4) :: lr(40)
  !     .. Intrinsic Functions ..
  INTRINSIC ABS, LOG, MAX, MIN, SIGN, SQRT
  !     .. Data statements ..
  REAL(sp), PARAMETER    :: log10_radix_sp = LOG10( REAL( RADIX(1._sp), SP ) )
  REAL(sp), PARAMETER    :: eps_sp         = EPSILON(1._sp)
  INTEGER(i4), PARAMETER :: digits_sp      = DIGITS(1._sp)
  INTEGER(i4), PARAMETER :: nbits = INT( log10_radix_sp*digits_sp/0.30102000_sp ), &
    nlmx = MIN(40,(nbits*4)/5)
  REAL(sp), PARAMETER :: sq2 = SQRT(2._sp), w1 = 41._sp/140._sp, w2 = 216._sp/140._sp, &
    w3 = 27._sp/140._sp, w4 = 272._sp/140._sp
  INTEGER(i4), PARAMETER :: kml = 7, kmx = 2000, nlmn = 2
  !* FIRST EXECUTABLE STATEMENT  QNC79
  Ans = 0._sp
  Ierr = 1
  ce = 0._sp
  IF( A==B ) GOTO 400
  lmx = nlmx
  lmn = nlmn
  IF( B/=0._sp ) THEN
    IF( SIGN(1._sp,B)*A>0._sp ) THEN
      c = ABS(1._sp-A/B)
      IF( c<=0.1_sp ) THEN
        IF( c<=0._sp ) GOTO 400
        nib = INT( 0.5_sp - LOG(c)/LOG(2._sp) )
        lmx = MIN(nlmx,nbits-nib-4)
        IF( lmx<2 ) GOTO 400
        lmn = MIN(lmn,lmx)
      END IF
    END IF
  END IF
  tol = MAX(ABS(Err),2._sp**(5-nbits))
  IF( Err==0._sp ) tol = SQRT(eps_sp)
  eps = tol
  hh(1) = (B-A)/12._sp
  aa(1) = A
  lr(1) = 1
  DO i = 1, 11, 2
    f(i) = FUN(A+(i-1)*hh(1))
  END DO
  blocal = B
  f(13) = FUN(blocal)
  K = 7
  l = 1
  area = 0._sp
  q7 = 0._sp
  ef = 256._sp/255._sp
  bank = 0._sp
  !
  !     Compute refined estimates, estimate the error, etc.
  !
  100 CONTINUE
  DO i = 2, 12, 2
    f(i) = FUN(aa(l)+(i-1)*hh(l))
  END DO
  K = K + 6
  !
  !     Compute left and right half estimates
  !
  q7l = hh(l)*((w1*(f(1)+f(7))+w2*(f(2)+f(6)))+(w3*(f(3)+f(5))+w4*f(4)))
  q7r(l) = hh(l)&
    *((w1*(f(7)+f(13))+w2*(f(8)+f(12)))+(w3*(f(9)+f(11))+w4*f(10)))
  !
  !     Update estimate of integral of absolute value
  !
  area = area + (ABS(q7l)+ABS(q7r(l))-ABS(q7))
  !
  !     Do not bother to test convergence before minimum refinement level
  !
  IF( l>=lmn ) THEN
    !
    !     Estimate the error in new value for whole interval, Q13
    !
    q13 = q7l + q7r(l)
    ee = ABS(q7-q13)*ef
    !
    !     Compute nominal allowed error
    !
    ae = eps*area
    !
    !     Borrow from bank account, but not too much
    !
    test = MIN(ae+0.8_sp*bank,10._sp*ae)
    !
    !     Don't ask for excessive accuracy
    !
    test = MAX(test,tol*ABS(q13),0.00003_sp*tol*area)
    !
    !     Now, did this interval pass or not?
    !
    IF( ee<=test ) THEN
      !
      !     On good intervals accumulate the theoretical estimate
      !
      ce = ce + (q7-q13)/255._sp
    ELSE
      !
      !     Consider the left half of next deeper level
      !
      IF( K>kmx ) lmx = MIN(kml,lmx)
      IF( l<lmx ) GOTO 200
      !
      !     Have hit maximum refinement level -- penalize the cumulative error
      !
      ce = ce + (q7-q13)
    END IF
    !
    !     Update the bank account.  Don't go into debt.
    !
    bank = bank + (ae-ee)
    IF( bank<0._sp ) bank = 0._sp
    !
    !     Did we just finish a left half or a right half?
    !
    IF( lr(l)<=0 ) THEN
      !
      !     Proceed to right half at this level
      !
      vl(l) = q13
      GOTO 300
    ELSE
      !
      !     Left and right halves are done, so go back up a level
      !
      vr = q13
      DO WHILE( l>1 )
        IF( l<=17 ) ef = ef*sq2
        eps = eps*2._sp
        l = l - 1
        IF( lr(l)<=0 ) THEN
          vl(l) = vl(l+1) + vr
          GOTO 300
        ELSE
          vr = vl(l+1) + vr
        END IF
      END DO
      !
      !     Exit
      !
      Ans = vr
      IF( ABS(ce)>2._sp*tol*area ) THEN
        Ierr = 2
        ERROR STOP 'QNC79 : ANS is probably insufficiently accurate.'
      END IF
      RETURN
    END IF
  END IF
  200  l = l + 1
  eps = eps*0.5_sp
  IF( l<=17 ) ef = ef/sq2
  hh(l) = hh(l-1)*0.5_sp
  lr(l) = -1
  aa(l) = aa(l-1)
  q7 = q7l
  f1(l) = f(7)
  f2(l) = f(8)
  f3(l) = f(9)
  f4(l) = f(10)
  f5(l) = f(11)
  f6(l) = f(12)
  f7(l) = f(13)
  f(13) = f(7)
  f(11) = f(6)
  f(9) = f(5)
  f(7) = f(4)
  f(5) = f(3)
  f(3) = f(2)
  GOTO 100
  300  q7 = q7r(l-1)
  lr(l) = 1
  aa(l) = aa(l) + 12._sp*hh(l)
  f(1) = f1(l)
  f(3) = f2(l)
  f(5) = f3(l)
  f(7) = f4(l)
  f(9) = f5(l)
  f(11) = f6(l)
  f(13) = f7(l)
  GOTO 100
  400  Ierr = -1
  ! 'QNC79 : A and B are too nearly equal to allow normal integration.&
    ! & ANS is set to zero and IERR to -1.'
  !
  RETURN
END SUBROUTINE QNC79

!** DQNC79
PURE SUBROUTINE DQNC79(FUN,A,B,Err,Ans,Ierr,K)
  !> Integrate a function using a 7-point adaptive Newton-Cotes quadrature rule.
  !***
  ! **Library:**   SLATEC
  !***
  ! **Category:**  H2A1A1
  !***
  ! **Type:**      DOUBLE PRECISION (QNC79-S, DQNC79-D)
  !***
  ! **Keywords:**  ADAPTIVE QUADRATURE, INTEGRATION, NEWTON-COTES
  !***
  ! **Author:**  Kahaner, D. K., (NBS)
  !           Jones, R. E., (SNLA)
  !***
  ! **Description:**
  !
  !     Abstract  *** a DOUBLE PRECISION routine ***
  !       DQNC79 is a general purpose program for evaluation of
  !       one dimensional integrals of user defined functions.
  !       DQNC79 will pick its own points for evaluation of the
  !       integrand and these will vary from problem to problem.
  !       Thus, DQNC79 is not designed to integrate over data sets.
  !       Moderately smooth integrands will be integrated efficiently
  !       and reliably.  For problems with strong singularities,
  !       oscillations etc., the user may wish to use more sophis-
  !       ticated routines such as those in QUADPACK.  One measure
  !       of the reliability of DQNC79 is the output parameter K,
  !       giving the number of integrand evaluations that were needed.
  !
  !     Description of Arguments
  !
  !     --Input--* FUN, A, B, ERR are DOUBLE PRECISION *
  !       FUN  - name of external function to be integrated.  This name
  !              must be in an EXTERNAL statement in your calling
  !              program.  You must write a Fortran function to evaluate
  !              FUN.  This should be of the form
  !                    DOUBLE PRECISION FUNCTION FUN (X)
  !              C
  !              C     X can vary from A to B
  !              C     FUN(X) should be finite for all X on interval.
  !              C
  !                    FUN = ...
  !                    RETURN
  !                    END
  !       A    - lower limit of integration
  !       B    - upper limit of integration (may be less than A)
  !       ERR  - is a requested error tolerance.  Normally, pick a value
  !              0 < ERR < 1.0D-8.
  !
  !     --Output--
  !       ANS  - computed value of the integral.  Hopefully, ANS is
  !              accurate to within ERR * integral of ABS(FUN(X)).
  !       IERR - a status code
  !            - Normal codes
  !               1  ANS most likely meets requested error tolerance.
  !              -1  A equals B, or A and B are too nearly equal to
  !                  allow normal integration.  ANS is set to zero.
  !            - Abnormal code
  !               2  ANS probably does not meet requested error tolerance.
  !       K    - the number of function evaluations actually used to do
  !              the integration.  A value of K > 1000 indicates a
  !              difficult problem; other programs may be more efficient.
  !              DQNC79 will gracefully give up if K exceeds 2000.
  !
  !***
  ! **References:**  (NONE)
  !***
  ! **Routines called:**  D1MACH, I1MACH, XERMSG

  !* REVISION HISTORY  (YYMMDD)
  !   790601  DATE WRITTEN
  !   890531  Changed all specific intrinsics to generic.  (WRB)
  !   890911  Removed unnecessary intrinsics.  (WRB)
  !   890911  REVISION DATE from Version 3.2
  !   891214  Prologue converted to Version 4.0 format.  (BAB)
  !   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
  !   920218  Code redone to parallel QNC79.  (WRB)
  !   930120  Increase array size 80->99, and KMX 2000->5000 for SUN -r8 wordlength.  (RWC)
  !USE service, ONLY : log10_radix_dp, eps_dp, digits_dp
  !     .. Function Arguments ..
  INTERFACE
    REAL(dp) PURE FUNCTION FUN(X)
      IMPORT DP
      REAL(dp), INTENT(IN) :: X
    END FUNCTION FUN
  END INTERFACE
  !     .. Scalar Arguments ..
  INTEGER(i4), INTENT(OUT) :: Ierr, K
  REAL(dp), INTENT(IN) :: A, B, Err
  REAL(dp), INTENT(OUT) :: Ans
  !     .. Local Scalars ..
  REAL(dp) :: ae, area, bank, blocal, c, ce, ee, ef, eps, q13, q7, q7l, test, tol, vr
  INTEGER(i4) :: i, l, lmn, lmx, nib
  !     .. Local Arrays ..
  REAL(dp) :: aa(99), f(13), f1(99), f2(99), f3(99), f4(99), f5(99), f6(99), &
    f7(99), hh(99), q7r(99), vl(99)
  INTEGER(i4) :: lr(99)
  !     .. Intrinsic Functions ..
  INTRINSIC ABS, LOG, MAX, MIN, SIGN, SQRT
  !     .. Data statements ..
  INTEGER(i4), PARAMETER :: nbits = INT( log10_radix_dp*digits_dp/0.30102000_dp ), &
    nlmx = MIN(99,(nbits*4)/5)
  REAL(dp), PARAMETER :: sq2 = SQRT(2._dp), w1 = 41._dp/140._dp, w2 = 216._dp/140._dp, &
    w3 = 27._dp/140._dp, w4 = 272._dp/140._dp
  INTEGER(i4), PARAMETER :: kml = 7, kmx = 5000, nlmn = 2
  !* FIRST EXECUTABLE STATEMENT  DQNC79
  Ans = 0._dp
  Ierr = 1
  ce = 0._dp
  IF( A==B ) GOTO 400
  lmx = nlmx
  lmn = nlmn
  IF( B/=0._dp ) THEN
    IF( SIGN(1._dp,B)*A>0._dp ) THEN
      c = ABS(1._dp-A/B)
      IF( c<=0.1_dp ) THEN
        IF( c<=0._dp ) GOTO 400
        nib = INT( 0.5_dp - LOG(c)/LOG(2._dp) )
        lmx = MIN(nlmx,nbits-nib-4)
        IF( lmx<2 ) GOTO 400
        lmn = MIN(lmn,lmx)
      END IF
    END IF
  END IF
  tol = MAX(ABS(Err),2._dp**(5-nbits))
  IF( Err==0._dp ) tol = SQRT(eps_dp)
  eps = tol
  hh(1) = (B-A)/12._dp
  aa(1) = A
  lr(1) = 1
  DO i = 1, 11, 2
    f(i) = FUN(A+(i-1)*hh(1))
  END DO
  blocal = B
  f(13) = FUN(blocal)
  K = 7
  l = 1
  area = 0._dp
  q7 = 0._dp
  ef = 256._dp/255._dp
  bank = 0._dp
  !
  !     Compute refined estimates, estimate the error, etc.
  !
  100 CONTINUE
  DO i = 2, 12, 2
    f(i) = FUN(aa(l)+(i-1)*hh(l))
  END DO
  K = K + 6
  !
  !     Compute left and right half estimates
  !
  q7l = hh(l)*((w1*(f(1)+f(7))+w2*(f(2)+f(6)))+(w3*(f(3)+f(5))+w4*f(4)))
  q7r(l) = hh(l)&
    *((w1*(f(7)+f(13))+w2*(f(8)+f(12)))+(w3*(f(9)+f(11))+w4*f(10)))
  !
  !     Update estimate of integral of absolute value
  !
  area = area + (ABS(q7l)+ABS(q7r(l))-ABS(q7))
  !
  !     Do not bother to test convergence before minimum refinement level
  !
  IF( l>=lmn ) THEN
    !
    !     Estimate the error in new value for whole interval, Q13
    !
    q13 = q7l + q7r(l)
    ee = ABS(q7-q13)*ef
    !
    !     Compute nominal allowed error
    !
    ae = eps*area
    !
    !     Borrow from bank account, but not too much
    !
    test = MIN(ae+0.8_dp*bank,10._dp*ae)
    !
    !     Don't ask for excessive accuracy
    !
    test = MAX(test,tol*ABS(q13),0.00003_dp*tol*area)
    !
    !     Now, did this interval pass or not?
    !
    IF( ee<=test ) THEN
      !
      !     On good intervals accumulate the theoretical estimate
      !
      ce = ce + (q7-q13)/255._dp
    ELSE
      !
      !     Consider the left half of next deeper level
      !
      IF( K>kmx ) lmx = MIN(kml,lmx)
      IF( l<lmx ) GOTO 200
      !
      !     Have hit maximum refinement level -- penalize the cumulative error
      !
      ce = ce + (q7-q13)
    END IF
    !
    !     Update the bank account.  Don't go into debt.
    !
    bank = bank + (ae-ee)
    IF( bank<0._dp ) bank = 0._dp
    !
    !     Did we just finish a left half or a right half?
    !
    IF( lr(l)<=0 ) THEN
      !
      !     Proceed to right half at this level
      !
      vl(l) = q13
      GOTO 300
    ELSE
      !
      !     Left and right halves are done, so go back up a level
      !
      vr = q13
      DO WHILE( l>1 )
        IF( l<=17 ) ef = ef*sq2
        eps = eps*2._dp
        l = l - 1
        IF( lr(l)<=0 ) THEN
          vl(l) = vl(l+1) + vr
          GOTO 300
        ELSE
          vr = vl(l+1) + vr
        END IF
      END DO
      !
      !     Exit
      !
      Ans = vr
      IF( ABS(ce)>2._dp*tol*area ) THEN
        Ierr = 2
        ERROR STOP 'DQNC79 : ANS is probably insufficiently accurate.'
      END IF
      RETURN
    END IF
  END IF
  200  l = l + 1
  eps = eps*0.5_dp
  IF( l<=17 ) ef = ef/sq2
  hh(l) = hh(l-1)*0.5_dp
  lr(l) = -1
  aa(l) = aa(l-1)
  q7 = q7l
  f1(l) = f(7)
  f2(l) = f(8)
  f3(l) = f(9)
  f4(l) = f(10)
  f5(l) = f(11)
  f6(l) = f(12)
  f7(l) = f(13)
  f(13) = f(7)
  f(11) = f(6)
  f(9) = f(5)
  f(7) = f(4)
  f(5) = f(3)
  f(3) = f(2)
  GOTO 100
  300  q7 = q7r(l-1)
  lr(l) = 1
  aa(l) = aa(l) + 12._dp*hh(l)
  f(1) = f1(l)
  f(3) = f2(l)
  f(5) = f3(l)
  f(7) = f4(l)
  f(9) = f5(l)
  f(11) = f6(l)
  f(13) = f7(l)
  GOTO 100
  400  Ierr = -1
  ! 'DQNC79 : A and B are too nearly equal to allow normal integration. &
    ! ANS is set to zero and IERR to -1.'
  RETURN
END SUBROUTINE DQNC79

PURE SUBROUTINE GAUS8(FUN,A,B,Err,Ans,Ierr)
  !> Integrate a real function of one variable over a finite interval using an
  !  adaptive 8-point Legendre-Gauss algorithm.
  !  Intended primarily for high accuracy integration or integration of smooth functions.
  !***
  ! **Library:**   SLATEC
  !***
  ! **Category:**  H2A1A1
  !***
  ! **Type:**      SINGLE PRECISION (GAUS8-S, DGAUS8-D)
  !***
  ! **Keywords:**  ADAPTIVE QUADRATURE, AUTOMATIC INTEGRATOR,
  !             GAUSS QUADRATURE, NUMERICAL INTEGRATION
  !***
  ! **Author:**  Jones, R. E., (SNLA)
  !***
  ! **Description:**
  !
  !     Abstract
  !        GAUS8 integrates real functions of one variable over finite
  !        intervals using an adaptive 8-point Legendre-Gauss algorithm.
  !        GAUS8 is intended primarily for high accuracy integration
  !        or integration of smooth functions.
  !
  !     Description of Arguments
  !
  !        Input--
  !        FUN - name of external function to be integrated.  This name
  !              must be in an EXTERNAL statement in the calling program.
  !              FUN must be a REAL function of one REAL argument.  The
  !              value of the argument to FUN is the variable of
  !              integration which ranges from A to B.
  !        A   - lower limit of integration
  !        B   - upper limit of integration (may be less than A)
  !        ERR - is a requested pseudorelative error tolerance.  Normally
  !              pick a value of ABS(ERR) so that STOL < ABS(ERR) <=
  !              1.0E-3 where STOL is the single precision unit roundoff
  !              eps_sp.  ANS will normally have no more error than
  !              ABS(ERR) times the integral of the absolute value of
  !              FUN(X).  Usually, smaller values for ERR yield more
  !              accuracy and require more function evaluations.
  !
  !              A negative value for ERR causes an estimate of the
  !              absolute error in ANS to be returned in ERR.  Note that
  !              ERR must be a variable (not a constant) in this case.
  !              Note also that the user must reset the value of ERR
  !              before making any more calls that use the variable ERR.
  !
  !        Output--
  !        ERR - will be an estimate of the absolute error in ANS if the
  !              input value of ERR was negative.  (ERR is unchanged if
  !              the input value of ERR was non-negative.)  The estimated
  !              error is solely for information to the user and should
  !              not be used as a correction to the computed integral.
  !        ANS - computed value of integral
  !        IERR- a status code
  !            --Normal codes
  !               1 ANS most likely meets requested error tolerance, or A=B.
  !              -1 A and B are too nearly equal to allow normal integration.
  !                 ANS is set to zero.
  !            --Abnormal code
  !               2 ANS probably does not meet requested error tolerance.
  !
  !***
  ! **References:**  (NONE)
  !***
  ! **Routines called:**  I1MACH, R1MACH, XERMSG

  !* REVISION HISTORY  (YYMMDD)
  !   810223  DATE WRITTEN
  !   890531  Changed all specific intrinsics to generic.  (WRB)
  !   890531  REVISION DATE from Version 3.2
  !   891214  Prologue converted to Version 4.0 format.  (BAB)
  !   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
  !   900326  Removed duplicate information from DESCRIPTIONsection.  (WRB)
  !USE service, ONLY : log10_radix_sp, eps_sp, digits_sp
  !
  INTERFACE
    REAL(sp) PURE FUNCTION FUN(X)
      IMPORT SP
      REAL(sp), INTENT(IN) :: X
    END FUNCTION FUN
  END INTERFACE
  INTEGER(i4), INTENT(OUT) :: Ierr
  REAL(sp), INTENT(IN) :: A, B
  REAL(sp), INTENT(INOUT) :: Err
  REAL(sp), INTENT(OUT) :: Ans
  !
  INTEGER(i4) :: k, l, lmn, lmx, lr(30), mxl, nbits, nib, nlmx
  REAL(sp) :: ae, anib, area, c, ce, ee, ef, eps, est, gl, glr, tol, vr
  REAL(sp) :: aa(30), gr(30), hh(30), vl(30)
  REAL(sp), PARAMETER :: x1 = 1.83434642495649805E-01_sp, x2 = 5.25532409916328986E-01_sp, &
    x3 =7.96666477413626740E-01_sp, x4 = 9.60289856497536232E-01_sp
  REAL(sp), PARAMETER ::  w1 =3.62683783378361983E-01_sp, w2 = 3.13706645877887287E-01_sp, &
    w3 = 2.22381034453374471E-01_sp, w4 = 1.01228536290376259E-01_sp
  REAL(sp), PARAMETER :: sq2 = 1.41421356_sp
  INTEGER(i4), PARAMETER :: nlmn = 1, kmx = 5000, kml = 6
  !* FIRST EXECUTABLE STATEMENT  GAUS8
  !
  !     Initialize
  !
  k = digits_sp
  anib = log10_radix_sp*k/0.30102000_sp
  nbits = INT( anib )
  nlmx = MIN(30,(nbits*5)/8)
  Ans = 0._sp
  Ierr = 1
  ce = 0._sp
  IF( A==B ) THEN
    IF( Err<0._sp ) Err = ce
    RETURN
  ELSE
    lmx = nlmx
    lmn = nlmn
    IF( B/=0._sp ) THEN
      IF( SIGN(1._sp,B)*A>0._sp ) THEN
        c = ABS(1._sp-A/B)
        IF( c<=0.1_sp ) THEN
          IF( c<=0._sp ) THEN
            IF( Err<0._sp ) Err = ce
            RETURN
          ELSE
            anib = 0.5_sp - LOG(c)/0.69314718_sp
            nib = INT( anib )
            lmx = MIN(nlmx,nbits-nib-7)
            IF( lmx<1 ) THEN
              Ierr = -1
              ! 'GAUS8 : A and B are too nearly equal to allow normal integration.&
                ! & ANS is set to zero and IERR to -1.'
              IF( Err<0._sp ) Err = ce
              RETURN
            ELSE
              lmn = MIN(lmn,lmx)
            END IF
          END IF
        END IF
      END IF
    END IF
    tol = MAX(ABS(Err),2._sp**(5-nbits))/2._sp
    IF( Err==0._sp ) tol = SQRT(eps_sp)
    eps = tol
    hh(1) = (B-A)/4._sp
    aa(1) = A
    lr(1) = 1
    l = 1
    est = G8(aa(l)+2._sp*hh(l),2._sp*hh(l))
    k = 8
    area = ABS(est)
    ef = 0.5_sp
    mxl = 0
  END IF
  100 CONTINUE
  DO
    !
    !     Compute refined estimates, estimate the error, etc.
    !
    gl = G8(aa(l)+hh(l),hh(l))
    gr(l) = G8(aa(l)+3._sp*hh(l),hh(l))
    k = k + 16
    area = area + (ABS(gl)+ABS(gr(l))-ABS(est))
    !     IF(L < LMN) GO TO 11
    glr = gl + gr(l)
    ee = ABS(est-glr)*ef
    ae = MAX(eps*area,tol*ABS(glr))
    IF( ee<=ae ) EXIT
    !
    !     Consider the left half of this level
    !
    IF( k>kmx ) lmx = kml
    IF( l>=lmx ) THEN
      mxl = 1
      EXIT
    ELSE
      l = l + 1
      eps = eps*0.5_sp
      ef = ef/sq2
      hh(l) = hh(l-1)*0.5_sp
      lr(l) = -1
      aa(l) = aa(l-1)
      est = gl
    END IF
  END DO
  ce = ce + (est-glr)
  IF( lr(l)<=0 ) THEN
    !
    !     Proceed to right half at this level
    !
    vl(l) = glr
  ELSE
    !
    !     Return one level
    !
    vr = glr
    DO WHILE( l>1 )
      l = l - 1
      eps = eps*2._sp
      ef = ef*sq2
      IF( lr(l)<=0 ) THEN
        vl(l) = vl(l+1) + vr
        GOTO 200
      ELSE
        vr = vl(l+1) + vr
      END IF
    END DO
    !
    !     Exit
    !
    Ans = vr
    IF( (mxl/=0) .AND. (ABS(ce)>2._sp*tol*area) ) THEN
      Ierr = 2
      ERROR STOP 'GAUS8 : ANS is probably insufficiently accurate.'
    END IF
    IF( Err<0._sp ) Err = ce
    RETURN
  END IF
  200  est = gr(l-1)
  lr(l) = 1
  aa(l) = aa(l) + 4._sp*hh(l)
  GOTO 100
  !
  RETURN
CONTAINS
  REAL(sp) ELEMENTAL FUNCTION G8(x,h)
    REAL(sp), INTENT(IN) :: x, h
    G8 = h*((w1*(FUN(x-x1*h)+FUN(x+x1*h))+w2*(FUN(x-x2*h)+FUN(x+x2*h)))&
      +(w3*(FUN(x-x3*h)+FUN(x+x3*h))+w4*(FUN(x-x4*h)+FUN(x+x4*h))))
  END FUNCTION G8
END SUBROUTINE GAUS8


PURE SUBROUTINE DGAUS8(FUN,A,B,Err,Ans,Ierr)
  !> Integrate a real function of one variable over a finite interval using an
  !  adaptive 8-point Legendre-Gauss algorithm.
  !  Intended primarily for high accuracy integration or integration of smooth functions.
  !***
  ! **Library:**   SLATEC
  !***
  ! **Category:**  H2A1A1
  !***
  ! **Type:**      DOUBLE PRECISION (GAUS8-S, DGAUS8-D)
  !***
  ! **Keywords:**  ADAPTIVE QUADRATURE, AUTOMATIC INTEGRATOR,
  !             GAUSS QUADRATURE, NUMERICAL INTEGRATION
  !***
  ! **Author:**  Jones, R. E., (SNLA)
  !***
  ! **Description:**
  !
  !     Abstract  *** a DOUBLE PRECISION routine ***
  !        DGAUS8 integrates real functions of one variable over finite
  !        intervals using an adaptive 8-point Legendre-Gauss algorithm.
  !        DGAUS8 is intended primarily for high accuracy integration
  !        or integration of smooth functions.
  !
  !        The maximum number of significant digits obtainable in ANS
  !        is the smaller of 18 and the number of digits carried in
  !        double precision arithmetic.
  !
  !     Description of Arguments
  !
  !        Input--* FUN, A, B, ERR are DOUBLE PRECISION *
  !        FUN - name of external function to be integrated.  This name
  !              must be in an EXTERNAL statement in the calling program.
  !              FUN must be a DOUBLE PRECISION function of one DOUBLE
  !              PRECISION argument.  The value of the argument to FUN
  !              is the variable of integration which ranges from A to B.
  !        A   - lower limit of integration
  !        B   - upper limit of integration (may be less than A)
  !        ERR - is a requested pseudorelative error tolerance.  Normally
  !              pick a value of ABS(ERR) so that DTOL < ABS(ERR) <=
  !              1.0D-3 where DTOL is the larger of 1.0D-18 and the
  !              double precision unit roundoff eps_dp.  ANS will
  !              normally have no more error than ABS(ERR) times the
  !              integral of the absolute value of FUN(X).  Usually,
  !              smaller values of ERR yield more accuracy and require
  !              more function evaluations.
  !
  !              A negative value for ERR causes an estimate of the
  !              absolute error in ANS to be returned in ERR.  Note that
  !              ERR must be a variable (not a constant) in this case.
  !              Note also that the user must reset the value of ERR
  !              before making any more calls that use the variable ERR.
  !
  !        Output--* ERR,ANS are double precision *
  !        ERR - will be an estimate of the absolute error in ANS if the
  !              input value of ERR was negative.  (ERR is unchanged if
  !              the input value of ERR was non-negative.)  The estimated
  !              error is solely for information to the user and should
  !              not be used as a correction to the computed integral.
  !        ANS - computed value of integral
  !        IERR- a status code
  !            --Normal codes
  !               1 ANS most likely meets requested error tolerance,
  !                 or A=B.
  !              -1 A and B are too nearly equal to allow normal
  !                 integration.  ANS is set to zero.
  !            --Abnormal code
  !               2 ANS probably does not meet requested error tolerance.
  !
  !***
  ! **References:**  (NONE)
  !***
  ! **Routines called:**  D1MACH, I1MACH, XERMSG

  !* REVISION HISTORY  (YYMMDD)
  !   810223  DATE WRITTEN
  !   890531  Changed all specific intrinsics to generic.  (WRB)
  !   890911  Removed unnecessary intrinsics.  (WRB)
  !   890911  REVISION DATE from Version 3.2
  !   891214  Prologue converted to Version 4.0 format.  (BAB)
  !   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
  !   900326  Removed duplicate information from DESCRIPTIONsection.  (WRB)
  !USE service, ONLY : log10_radix_dp, eps_dp, digits_dp
  !
  INTERFACE
    REAL(dp) PURE FUNCTION FUN(X)
      IMPORT dp
      REAL(dp), INTENT(IN) :: X
    END FUNCTION FUN
  END INTERFACE
  INTEGER(i4), INTENT(OUT) :: Ierr
  REAL(dp), INTENT(IN) :: A, B
  REAL(dp), INTENT(INOUT) :: Err
  REAL(dp), INTENT(OUT) :: Ans
  !
  INTEGER(i4) :: k, l, lmn, lmx, lr(60), mxl, nbits, nib, nlmx
  REAL(dp) :: ae, anib, area, c, ce, ee, ef, eps, est, gl, glr, tol, vr
  REAL(dp) :: aa(60), gr(60), hh(60), vl(60)
  REAL(dp), PARAMETER :: x1 = 1.83434642495649805E-01_dp, x2 = 5.25532409916328986E-01_dp, &
    x3 =7.96666477413626740E-01_dp , x4 = 9.60289856497536232E-01_dp
  REAL(dp), PARAMETER :: w1 = 3.62683783378361983E-01_dp, w2 = 3.13706645877887287E-01_dp, &
    w3 = 2.22381034453374471E-01_dp, w4 = 1.01228536290376259E-01_dp
  REAL(dp), PARAMETER :: sq2 = 1.41421356_dp
  INTEGER(i4), PARAMETER :: nlmn = 1, kmx = 5000, kml = 6
  !* FIRST EXECUTABLE STATEMENT  DGAUS8
  !
  !     Initialize
  !
  k = digits_dp
  anib = log10_radix_dp*k/0.30102000_dp
  nbits = INT( anib )
  nlmx = MIN(60,(nbits*5)/8)
  Ans = 0._dp
  Ierr = 1
  ce = 0._dp
  IF( A==B ) THEN
    IF( Err<0._DP ) Err = ce
    RETURN
  ELSE
    lmx = nlmx
    lmn = nlmn
    IF( B/=0._dp) THEN
      IF( SIGN(1._dp,B)*A>0._dp ) THEN
        c = ABS(1._dp-A/B)
        IF( c<=0.1_dp ) THEN
          IF( c<=0._dp ) THEN
            IF( Err<0._dp ) Err = ce
            RETURN
          ELSE
            anib = 0.5_dp - LOG(c)/0.69314718_dp
            nib = INT( anib )
            lmx = MIN(nlmx,nbits-nib-7)
            IF( lmx<1 ) THEN
              Ierr = -1
              ! 'DGAUS8 : A and B are too nearly equal to allow normal integration.&
                ! & ANS is set to zero and IERR to -1.',1,-1)
              IF( Err<0._dp ) Err = ce
              RETURN
            ELSE
              lmn = MIN(lmn,lmx)
            END IF
          END IF
        END IF
      END IF
    END IF
    tol = MAX(ABS(Err),2._dp**(5-nbits))/2._dp
    IF( Err==0._dp ) tol = SQRT(eps_dp)
    eps = tol
    hh(1) = (B-A)/4._dp
    aa(1) = A
    lr(1) = 1
    l = 1
    est = G8(aa(l)+2._dp*hh(l),2._dp*hh(l))
    k = 8
    area = ABS(est)
    ef = 0.5_dp
    mxl = 0
  END IF
  100 CONTINUE
  DO
    !
    !     Compute refined estimates, estimate the error, etc.
    !
    gl = G8(aa(l)+hh(l),hh(l))
    gr(l) = G8(aa(l)+3._dp*hh(l),hh(l))
    k = k + 16
    area = area + (ABS(gl)+ABS(gr(l))-ABS(est))
    !     IF(L .LT .LMN) GO TO 11
    glr = gl + gr(l)
    ee = ABS(est-glr)*ef
    ae = MAX(eps*area,tol*ABS(glr))
    IF( ee<=ae ) EXIT
    !
    !     Consider the left half of this level
    !
    IF( k>kmx ) lmx = kml
    IF( l>=lmx ) THEN
      mxl = 1
      EXIT
    ELSE
      l = l + 1
      eps = eps*0.5_dp
      ef = ef/sq2
      hh(l) = hh(l-1)*0.5_dp
      lr(l) = -1
      aa(l) = aa(l-1)
      est = gl
    END IF
  END DO
  ce = ce + (est-glr)
  IF( lr(l)<=0 ) THEN
    !
    !     Proceed to right half at this level
    !
    vl(l) = glr
  ELSE
    !
    !     Return one level
    !
    vr = glr
    DO WHILE( l>1 )
      l = l - 1
      eps = eps*2._dp
      ef = ef*sq2
      IF( lr(l)<=0 ) THEN
        vl(l) = vl(l+1) + vr
        GOTO 200
      ELSE
        vr = vl(l+1) + vr
      END IF
    END DO
    !
    !     Exit
    !
    Ans = vr
    IF( (mxl/=0) .AND. (ABS(ce)>2._dp*tol*area) ) THEN
      Ierr = 2
      ERROR STOP 'DGAUS8 : ANS is probably insufficiently accurate.'
    END IF
    IF( Err<0._dp ) Err = ce
    RETURN
  END IF
  200  est = gr(l-1)
  lr(l) = 1
  aa(l) = aa(l) + 4._dp*hh(l)
  GOTO 100
  !
  RETURN
CONTAINS
  REAL(dp) ELEMENTAL FUNCTION G8(x,h)
    REAL(dp), INTENT(IN) :: x, h
    G8 = h*((w1*(FUN(x-x1*h)+FUN(x+x1*h))+w2*(FUN(x-x2*h)+FUN(x+x2*h)))&
      +(w3*(FUN(x-x3*h)+FUN(x+x3*h))+w4*(FUN(x-x4*h)+FUN(x+x4*h))))
  END FUNCTION G8
END SUBROUTINE DGAUS8


PURE SUBROUTINE AVINT(X,Y,N,Xlo,Xup,Ans,Ierr)
  !> Integrate a function tabulated at arbitrarily spaced abscissas using
  !  overlapping parabolas.
  !***
  ! **Library:**   SLATEC
  !***
  ! **Category:**  H2A1B2
  !***
  ! **Type:**      SINGLE PRECISION (AVINT-S, DAVINT-D)
  !***
  ! **Keywords:**  INTEGRATION, QUADRATURE, TABULATED DATA
  !***
  ! **Author:**  Jones, R. E., (SNLA)
  !***
  ! **Description:**
  !
  !     Abstract
  !         AVINT integrates a function tabulated at arbitrarily spaced
  !         abscissas.  The limits of integration need not coincide
  !         with the tabulated abscissas.
  !
  !         A method of overlapping parabolas fitted to the data is used
  !         provided that there are at least 3 abscissas between the
  !         limits of integration.  AVINT also handles two special cases.
  !         If the limits of integration are equal, AVINT returns a result
  !         of zero regardless of the number of tabulated values.
  !         If there are only two function values, AVINT uses the
  !         trapezoid rule.
  !
  !     Description of Parameters
  !         The user must dimension all arrays appearing in the call list
  !              X(N), Y(N).
  !
  !         Input--
  !         X    - real array of abscissas, which must be in increasing
  !                order.
  !         Y    - real array of functional values. i.e., Y(I)=FUNC(X(I)).
  !         N    - the integer number of function values supplied.
  !                N >= 2 unless XLO = XUP.
  !         XLO  - real lower limit of integration.
  !         XUP  - real upper limit of integration.
  !                Must have XLO <= XUP.
  !
  !         Output--
  !         ANS  - computed approximate value of integral
  !         IERR - a status code
  !              --normal code
  !                =1 means the requested integration was performed.
  !              --abnormal codes
  !                =2 means XUP was less than XLO.
  !                =3 means the number of X(I) between XLO and XUP
  !                   (inclusive) was less than 3 and neither of the two
  !                   special cases described in the Abstract occurred.
  !                   No integration was performed.
  !                =4 means the restriction X(I+1) > X(I) was violated.
  !                =5 means the number N of function values was < 2.
  !                ANS is set to zero if IERR=2,3,4,or 5.
  !
  !     AVINT is documented completely in SC-M-69-335
  !     Original program from "Numerical Integration" by Davis & Rabinowitz.
  !     Adaptation and modifications for Sandia Mathematical Program
  !     Library by Rondall E. Jones.
  !
  !***
  ! **References:**  R. E. Jones, Approximate integrator of functions
  !                 tabulated at arbitrarily spaced abscissas,
  !                 Report SC-M-69-335, Sandia Laboratories, 1969.
  !***
  ! **Routines called:**  XERMSG

  !* REVISION HISTORY  (YYMMDD)
  !   690901  DATE WRITTEN
  !   890831  Modified array declarations.  (WRB)
  !   890831  REVISION DATE from Version 3.2
  !   891214  Prologue converted to Version 4.0 format.  (BAB)
  !   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
  !   900326  Removed duplicate information from DESCRIPTIONsection.  (WRB)
  !   920501  Reformatted the REFERENCES section.  (WRB)

  INTEGER(i4), INTENT(IN) :: N
  INTEGER(i4), INTENT(OUT) :: Ierr
  REAL(sp), INTENT(IN) :: Xlo, Xup, X(N), Y(N)
  REAL(sp), INTENT(OUT) :: Ans
  !
  INTEGER(i4) :: i, inlft, inrt, istart, istop
  REAL(sp) :: fl, fr, slope
  REAL(dp) :: r3, rp5, summ, syl, syl2, syl3, syu, syu2, syu3, &
    x1, x2, x3, x12, x13, x23, term1, term2, term3, a, b, c, ca, cb, cc
  !* FIRST EXECUTABLE STATEMENT  AVINT
  Ierr = 1
  Ans = 0._SP
  IF( Xlo<Xup ) THEN
    IF( N<2 ) THEN
      Ierr = 5
      ERROR STOP 'AVINT : LESS THAN TWO FUNCTION VALUES WERE SUPPLIED.'
      RETURN
    ELSE
      DO i = 2, N
        IF( X(i)<=X(i-1) ) GOTO 200
        IF( X(i)>Xup ) EXIT
      END DO
      IF( N>=3 ) THEN
        IF( X(N-2)<Xlo ) GOTO 100
        IF( X(3)>Xup ) GOTO 100
        i = 1
        DO WHILE( X(i)<Xlo )
          i = i + 1
        END DO
        inlft = i
        i = N
        DO WHILE( X(i)>Xup )
          i = i - 1
        END DO
        inrt = i
        IF( (inrt-inlft)<2 ) GOTO 100
        istart = inlft
        IF( inlft==1 ) istart = 2
        istop = inrt
        IF( inrt==N ) istop = N - 1
        !
        r3 = 3._sp
        rp5 = 0.5_sp
        summ = 0._sp
        syl = Xlo
        syl2 = syl*syl
        syl3 = syl2*syl
        !
        DO i = istart, istop
          x1 = X(i-1)
          x2 = X(i)
          x3 = X(i+1)
          x12 = x1 - x2
          x13 = x1 - x3
          x23 = x2 - x3
          term1 = REAL( Y(i-1), DP )/(x12*x13)
          term2 = -REAL( Y(i), DP )/(x12*x23)
          term3 = REAL( Y(i+1), DP )/(x13*x23)
          a = term1 + term2 + term3
          b = -(x2+x3)*term1 - (x1+x3)*term2 - (x1+x2)*term3
          c = x2*x3*term1 + x1*x3*term2 + x1*x2*term3
          IF( i<=istart ) THEN
            ca = a
            cb = b
            cc = c
          ELSE
            ca = 0.5_sp*(a+ca)
            cb = 0.5_sp*(b+cb)
            cc = 0.5_sp*(c+cc)
          END IF
          syu = x2
          syu2 = syu*syu
          syu3 = syu2*syu
          summ = summ + ca*(syu3-syl3)/r3 + cb*rp5*(syu2-syl2) + cc*(syu-syl)
          ca = a
          cb = b
          cc = c
          syl = syu
          syl2 = syu2
          syl3 = syu3
        END DO
        syu = Xup
        Ans = REAL( summ + ca*(syu**3-syl3)/r3 + cb*rp5*(syu**2-syl2) + cc*(syu-syl) , sp )
      ELSE
        !
        !     SPECIAL N=2 CASE
        slope = (Y(2)-Y(1))/(X(2)-X(1))
        fl = Y(1) + slope*(Xlo-X(1))
        fr = Y(2) + slope*(Xup-X(2))
        Ans = 0.5_sp*(fl+fr)*(Xup-Xlo)
        RETURN
      END IF
    END IF
  ELSEIF( Xlo/=Xup ) THEN
    Ierr = 2
    ERROR STOP 'AVINT : THE UPPER LIMIT OF INTEGRATION WAS NOT GREATER THAN &
      &THE LOWER LIMIT.'
    RETURN
  END IF
  RETURN
  100  Ierr = 3
  ERROR STOP 'AVINT : THERE WERE LESS THAN THREE FUNCTION VALUES BETWEEN THE &
    &LIMITS OF INTEGRATION.'
  RETURN
  200  Ierr = 4
  ERROR STOP 'AVINT : THE ABSCISSAS WERE NOT STRICTLY INCREASING. &
    &MUST HAVE X(I-1) < X(I) FOR ALL I.'
  !
  RETURN
END SUBROUTINE AVINT


!** DAVINT
PURE SUBROUTINE DAVINT(X,Y,N,Xlo,Xup,Ans,Ierr)
  !> Integrate a function tabulated at arbitrarily spaced abscissas using
  !  overlapping parabolas.
  !***
  ! **Library:**   SLATEC
  !***
  ! **Category:**  H2A1B2
  !***
  ! **Type:**      DOUBLE PRECISION (AVINT-S, DAVINT-D)
  !***
  ! **Keywords:**  INTEGRATION, QUADRATURE, TABULATED DATA
  !***
  ! **Author:**  Jones, R. E., (SNLA)
  !***
  ! **Description:**
  !
  !     Abstract
  !         DAVINT integrates a function tabulated at arbitrarily spaced
  !         abscissas.  The limits of integration need not coincide
  !         with the tabulated abscissas.
  !
  !         A method of overlapping parabolas fitted to the data is used
  !         provided that there are at least 3 abscissas between the
  !         limits of integration.  DAVINT also handles two special cases.
  !         If the limits of integration are equal, DAVINT returns a
  !         result of zero regardless of the number of tabulated values.
  !         If there are only two function values, DAVINT uses the
  !         trapezoid rule.
  !
  !     Description of Parameters
  !         The user must dimension all arrays appearing in the call list
  !              X(N), Y(N)
  !
  !         Input--
  !      X    - DOUBLE PRECISION array of abscissas, which must be in
  !             increasing order.
  !      Y    - DOUBLE PRECISION array of function values. i.e.,
  !                Y(I)=FUNC(X(I))
  !      N    - The integer number of function values supplied.
  !                N >= 2 unless XLO = XUP.
  !      XLO  - DOUBLE PRECISION lower limit of integration
  !      XUP  - DOUBLE PRECISION upper limit of integration.  Must have
  !              XLO<=XUP
  !
  !         Output--
  !      ANS  - Double Precision computed approximate value of integral
  !      IERR - A status code
  !           --Normal Code
  !                =1 Means the requested integration was performed.
  !           --Abnormal Codes
  !                =2 Means XUP was less than XLO.
  !                =3 Means the number of X(I) between XLO and XUP
  !                   (inclusive) was less than 3 and neither of the two
  !                   special cases described in the abstract occurred.
  !                   No integration was performed.
  !                =4 Means the restriction X(I+1)>X(I) was violated.
  !                =5 Means the number N of function values was < 2.
  !                   ANS is set to zero if IERR=2,3,4,or 5.
  !
  !    DAVINT is documented completely in SC-M-69-335
  !    Original program from *Numerical Integration* by Davis & Rabinowitz
  !    Adaptation and modifications by Rondall E Jones.
  !
  !***
  ! **References:**  R. E. Jones, Approximate integrator of functions
  !                 tabulated at arbitrarily spaced abscissas,
  !                 Report SC-M-69-335, Sandia Laboratories, 1969.
  !***
  ! **Routines called:**  XERMSG

  !* REVISION HISTORY  (YYMMDD)
  !   690901  DATE WRITTEN
  !   890831  Modified array declarations.  (WRB)
  !   890831  REVISION DATE from Version 3.2
  !   891214  Prologue converted to Version 4.0 format.  (BAB)
  !   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
  !   920501  Reformatted the REFERENCES section.  (WRB)

  INTEGER(i4), INTENT(IN) :: N
  INTEGER(i4), INTENT(OUT) :: Ierr
  REAL(dp), INTENT(IN) :: Xlo, Xup, X(N), Y(N)
  REAL(dp), INTENT(OUT) :: Ans
  !
  INTEGER(i4) :: i, inlft, inrt, istart, istop
  REAL(dp) :: a, b, c, ca, cb, cc, fl, fr, r3, rp5, slope, summ, syl, syl2, syl3, &
    syu, syu2, syu3, term1, term2, term3, x1, x12, x13, x2, x23, x3
  !     BEGIN BLOCK PERMITTING ...EXITS TO 190
  !        BEGIN BLOCK PERMITTING ...EXITS TO 180
  !* FIRST EXECUTABLE STATEMENT  DAVINT
  Ierr = 1
  Ans = 0._dp
  IF( Xlo>Xup ) THEN
    Ierr = 2
    !     ......EXIT
    ERROR STOP 'DAVINT : THE UPPER LIMIT OF INTEGRATION WAS NOT GREATER THAN &
      &THE LOWER LIMIT.'
  ELSEIF( Xlo/=Xup ) THEN
    IF( N>=2 ) THEN
      DO i = 2, N
        !        ............EXIT
        IF( X(i)<=X(i-1) ) GOTO 50
        !                 ...EXIT
        IF( X(i)>Xup ) EXIT
      END DO
      IF( N<3 ) THEN
        !
        !                    SPECIAL N=2 CASE
        slope = (Y(2)-Y(1))/(X(2)-X(1))
        fl = Y(1) + slope*(Xlo-X(1))
        fr = Y(2) + slope*(Xup-X(2))
        Ans = 0.5_DP*(fl+fr)*(Xup-Xlo)
        !     ...............EXIT
        RETURN
      ELSEIF( X(N-2)<Xlo ) THEN
        Ierr = 3
        ERROR STOP 'DAVINT : THERE WERE LESS THAN THREE FUNCTION VALUES BETWEEN &
          &THE LIMITS OF INTEGRATION.'
        !     ...............EXIT
        RETURN
      ELSEIF( X(3)<=Xup ) THEN
        i = 1
        DO WHILE( X(i)<Xlo )
          i = i + 1
        END DO
        inlft = i
        i = N
        DO WHILE( X(i)>Xup )
          i = i - 1
        END DO
        inrt = i
        IF( (inrt-inlft)>=2 ) THEN
          istart = inlft
          IF( inlft==1 ) istart = 2
          istop = inrt
          IF( inrt==N ) istop = N - 1
          !
          r3 = 3._dp
          rp5 = 0.5_dp
          summ = 0._dp
          syl = Xlo
          syl2 = syl*syl
          syl3 = syl2*syl
          !
          DO i = istart, istop
            x1 = X(i-1)
            x2 = X(i)
            x3 = X(i+1)
            x12 = x1 - x2
            x13 = x1 - x3
            x23 = x2 - x3
            term1 = Y(i-1)/(x12*x13)
            term2 = -Y(i)/(x12*x23)
            term3 = Y(i+1)/(x13*x23)
            a = term1 + term2 + term3
            b = -(x2+x3)*term1 - (x1+x3)*term2 - (x1+x2)*term3
            c = x2*x3*term1 + x1*x3*term2 + x1*x2*term3
            IF( i>istart ) THEN
              ca = 0.5_dp*(a+ca)
              cb = 0.5_dp*(b+cb)
              cc = 0.5_dp*(c+cc)
            ELSE
              ca = a
              cb = b
              cc = c
            END IF
            syu = x2
            syu2 = syu*syu
            syu3 = syu2*syu
            summ = summ + ca*(syu3-syl3)/r3 + cb*rp5*(syu2-syl2) + cc*(syu-syl)
            ca = a
            cb = b
            cc = c
            syl = syu
            syl2 = syu2
            syl3 = syu3
          END DO
          syu = Xup
          Ans = summ + ca*(syu**3-syl3)/r3 + cb*rp5*(syu**2-syl2) + cc*(syu-syl)
        ELSE
          Ierr = 3
          !     ...............EXIT
          ERROR STOP 'DAVINT : THERE WERE LESS THAN THREE FUNCTION VALUES BETWEEN &
            &THE LIMITS OF INTEGRATION.'
        END IF
        RETURN
      ELSE
        Ierr = 3
        ERROR STOP 'DAVINT : THERE WERE LESS THAN THREE FUNCTION VALUES BETWEEN &
          &THE LIMITS OF INTEGRATION.'
        !     ...............EXIT
        RETURN
      END IF
    ELSE
      Ierr = 5
      ERROR STOP 'DAVINT : LESS THAN TWO FUNCTION VALUES WERE SUPPLIED.'
      !     ...............EXIT
      RETURN
    END IF
    50  Ierr = 4
    ERROR STOP 'DAVINT : THE ABSCISSAS WERE NOT STRICTLY INCREASING. &
      &MUST HAVE X(I-1) < X(I) FOR ALL I.'
  END IF
  !
  RETURN
END SUBROUTINE DAVINT

end module integrators
