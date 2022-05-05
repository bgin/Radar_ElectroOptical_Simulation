MODULE global_minimum

! This is translated from the code of:
! Dr. Tibor Csendes
! Dept. of Applied Informatics, Jozsef Attila University
! H-6701 Szeged, Pf. 652, Hungary

! Phone: +36 62 544 305

! Fax: +36 62 420 292

! E-mail: csendes@inf.u-szeged.hu

! URL: http://www.inf.u-szeged.hu/~csendes/

use mod_kinds, only : i4,dp
IMPLICIT NONE
!INTEGER(kind=i4), PARAMETER, PRIVATE :: dp = SELECTED_REAL_KIND(12, 60)


CONTAINS

!   ROUTINE NAME - GLOBAL
 
! Code converted using TO_F90 by Alan Miller
! Date: 1999-12-27  Time: 13:16:39

!-----------------------------------------------------------------------

!   LATEST REVISION - OKTOBER 23, 1986
!   Latest revision of Fortran 90 version - 30 January 2000

!   PURPOSE  - GLOBAL MINIMUM OF FUNCTION OF N VARIABLES
!              USING A LOCAL SEARCH METHOD

!   USAGE  - CALL GLOBAL (AMIN, AMAX, NPARM, M, N100, NG0, IPR,
!                         NSIG, X0, NC, F0)

!   ARGUMENTS
!  AMIN - VECTOR OF LENGTH NPARM CONTAINING THE LOWER BOUNDS OF THE
!         PARAMETERS, SO X(I) IS SEARCHED IN THE INTERVAL (AMIN(I), AMAX(I)).
!         (INPUT)
!  AMAX - VECTOR OF LENGTH NPARM CONTAINING THE UPPER BOUNDS OF THE
!         PARAMETERS. (INPUT)
! NPARM - NUMBER OF PARAMETERS, 1 <= NPARM <= 20. (INPUT)
!     M - NUMBER OF RESIDUAL FUNCTIONS, WHEN THE OBJECTIVE FUNCTION IS OF THE
!         FORM F1**2 + F2**2 +...+ FM**2, 1 <=M <= 100. (INPUT)
!         N.B. M is NOT used for this purpose!
!              It is passed to the user's routine FUNCT as a parameter.
!  N100 - NUMBER OF SAMPLE POINTS TO BE DRAWN UNIFORMLY IN ONE CYCLE,
!         20 <= N100 <= 10000. THE SUGGESTED VALUE IS 100*NPARM. (INPUT)
!   NG0 - NUMBER OF BEST POINTS SELECTED FROM THE ACTUAL SAMPLE, 1 <= NG0 <= 20.
!         THE SUGGESTED VALUE IS TWICE THE EXPECTED NUMBER OF LOCAL MINIMA.
!         (INPUT)
!   IPR - FORTRAN DATA SET REFERENCE NUMBER WHERE THE PRINTED OUTPUT BE SENT.
!         (INPUT)
!  NSIG - CONVERGENCE CRITERION, THE ACCURACY REQUIRED IN THE PARAMETER
!         ESTIMATES.  THIS CONVERGENCE CRITERION IS SATISFIED IF ON TWO
!         SUCCESSIVE ITERATIONS THE PARAMETER ESTIMATES AGREE, COMPONENT BY
!         COMPONENT, TO NSIG DIGITS.  THE SUGGESTED VALUE IS 6. (INPUT)
!    X0 - OUTPUT NPARM BY 20 MATRIX CONTAINING NC (UP TO 20) LOCAL MINIMIZERS
!         FOUND.
!    NC - NUMBER OF DIFFERENT LOCAL MINIMIZERS FOUND. (OUTPUT)
!    F0 - OUTPUT VECTOR OF NC (UP TO 20) OBJECTIVE FUNCTION VALUES, F0(I)
!         BELONGS TO THE PARAMETERS X0(1,I), X0(2,I),..., X0(NPARM,I).

!   REQUIRED ROUTINES - URDMN, FUN, LOCAL

!-----------------------------------------------------------------------

SUBROUTINE global(amin, amax, nparm, m, n100, ng0, ipr, nsig, x0, nc, f0)
      !dir$ attributes code_align : 32 :: global
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=skylake_avx512 :: global
      use omp_lib
REAL (dp), INTENT(IN)    :: amin(:)
REAL (dp), INTENT(IN)    :: amax(:)
INTEGER(kind=i4), INTENT(IN)      :: nparm
INTEGER(kind=i4), INTENT(IN)      :: m
INTEGER(kind=i4), INTENT(IN OUT)  :: n100
INTEGER(kind=i4), INTENT(IN OUT)  :: ng0
INTEGER(kind=i4), INTENT(IN)      :: ipr
INTEGER(kind=i4), INTENT(IN)      :: nsig
REAL (dp), INTENT(OUT)   :: x0(:,:)
INTEGER(kind=i4), INTENT(OUT)     :: nc
REAL (dp), INTENT(OUT)   :: f0(:)

REAL (dp) :: x(nparm,104), x1(nparm,24), xcl(nparm,104), r(nparm), w(nparm)
!dir$ attribute align : 64 :: x
!dir$ attribute align : 64 :: x1
!dir$ attribute align : 64 :: xcl
INTEGER(kind=i4)   :: ic(104), ic1(24)
!dir$ attribute align : 64 :: ic
!dir$ attribute align : 64 :: ic1
INTEGER(kind=i4)   :: i, i1, icc, icj, ig, ii, iii, im, in1, inum, inum1, inum2, it, &
             iv, j, jj, l1, maxfn, n, n0, n1, ncp, nfe, nfe1, ng, ng10, nm, &
             nn100, ns
REAL (dp) :: f(104), f1(24), fcl(104), y(nparm), mmin(nparm), mmax(nparm)
!dir$ attribute align : 64 :: f
!dir$ attribute align : 64 :: f1
!dir$ attribute align : 64 :: fcl
REAL (dp) :: a, alfa, b, b1, bb, fc, ff, fm, relcon
REAL (dp), PARAMETER :: zero = 0.0_dp, one = 1.0_dp, two = 2.0_dp, ten = 10._dp

DO  i=1,nparm
  mmin(i) = amin(i)
  mmax(i) = amax(i)
  IF (mmin(i) == mmax(i)) GO TO 460
END DO
b1 = one/nparm

IF (ng0 < 1) ng0 = 1
IF (ng0 > 20) ng0 = 20
IF (n100 < 20) n100 = 20
IF (n100 > 10000) n100 = 10000
IF (n100 >= 100) GO TO 10

nn100 = n100
n = 1
GO TO 15

10 nn100 = 100
n = n100/100
n100 = n*100

15 ng10 = 100
!dir$ loop_count(100)
!dir$ code_align : 32
!$omp simd simdlen(8) linear(i:1)
DO  i=1,ng10
  f(i) = HUGE(one)
  ic(i) = 0
END DO
DO  i=1,nparm
  mmax(i) = (mmax(i) - mmin(i))/two
  mmin(i) = mmin(i) + mmax(i)
END DO
alfa = 0.01_dp
nfe = 0
ng = 0
ns = 0
nc = 0
ncp = 1
n0 = 0
n1 = 0
im = 1
ig = 0
fm = HUGE(one)
maxfn = 500*nparm
relcon = ten**(-nsig)

!         SAMPLING
20 n0 = n0 + n100
nm = n0 - 1
ng = ng + ng0
ns = ns + 1
IF (ns*ng0 > 100) GO TO 465
b = (one - alfa**(one/REAL(nm)))**b1
bb = 0.1*b
DO  i1=1,n
  DO  j=1,nn100
     CALL RANDOM_NUMBER( r )
    !dir$ loop_count min(1),max(20),avg(10)
    DO  i=1,nparm
      y(i) = two*r(i) - one
    END DO
    CALL fun(y, fc, nparm, m, mmin, mmax)
    IF (fc >= fm) CYCLE
    f(im) = fc
    !dir$ loop_count min(1),max(20),avg(10)
    !dir$ assume_aligned x:64
    !$omp simd simdlen(8) linear(i:1)
    DO  i=1,nparm
      x(i,im) = y(i)
    END DO
    IF (im <= ng .AND. ic(im) > 0) ig = ig - 1
    ic(im) = 0
    im = 1
    fm = f(1)
    DO  i=2,ng10
      IF (f(i) < fm) CYCLE
      im = i
      fm = f(i)
    END DO
  END DO
END DO

nfe = nfe + n100
WRITE(ipr,901) n100
WRITE(*,901) n100
901 FORMAT(/' ', i5, ' FUNCTION EVALUATIONS USED FOR SAMPLING')

!        SORTING
inum = ng10 - 1
DO  i=1,inum
  im = i
  fm = f(i)
  inum1 = i + 1
  DO  j=inum1,ng10
    IF (f(j) >= fm) CYCLE
    im = j
    fm = f(j)
  END DO
  IF (im <= i) CYCLE
  a = fm
   !dir$ loop_count min(1),max(20),avg(10)
  DO  j=1,nparm
    y(j) = x(j,im)
  END DO
  IF (i > ng .OR. im <= ng) GO TO 55
  IF (ic(ng) == 0 .AND. ic(im) > 0) ig = ig+1
  IF (ic(ng) > 0 .AND. ic(im) == 0) ig = ig-1
  55 icc = ic(im)
  inum1 = im-i
  !dir$ assume_aligned f:64
  !dir$ assume_aligned ic:64
  DO  j=1,inum1
    inum2 = im-j
    f(inum2+1) = f(inum2)
    ic(inum2+1) = ic(inum2)
    !dir4 assume_aligned x:64
    !dir$ loop_count min(1),max(20),avg(10)
    !dir$ ivdep
    !$omp simd simdlen(8) linear(jj:1)
    DO  jj=1,nparm
      x(jj,inum2+1) = x(jj,inum2)
    END DO
  END DO
  f(i) = a
  !dir$ loop_count min(1),max(20),avg(10)
  !dir$ ivdep
  !$omp simd simdlen(8) linear(j:1)
  DO  j=1,nparm
    x(j,i) = y(j)
  END DO
  ic(i) = icc
END DO
IF (nc <= 0) GO TO 200

!     CLUSTERING TO X*
DO  iii=1,nc
  i = 1
  in1 = i
  fcl(i) = f0(iii)
  !dir$ loop_count min(1),max(20),avg(10)
  !dir$ assume_aligned xcl:64
  !dir$ assume_aligned x0:64
  !dir$ ivdep
  !$omp simd simdlen(8) linear(j:1)
  DO  j=1,nparm
    xcl(j,i) = x0(j,iii)
  END DO
  DO  j=1,ng
    IF (ic(j) /= iii) CYCLE
    in1 = in1+1
  !dir$ loop_count min(1),max(20),avg(10)
  !dir$ assume_aligned xcl:64
  !dir$ assume_aligned x0:64
  !dir$ ivdep
  !$omp simd simdlen(8) linear(ii:1)
    DO  ii=1,nparm
      xcl(ii,in1) = x(ii,j)
    END DO
  END DO
  95 DO  j=1,ng
    IF (ic(j) /= 0) CYCLE
    IF (fcl(i) >= f(j)) CYCLE
    !dir$ loop_count min(1),max(20),avg(10)
    DO  l1=1,nparm
      w(l1) = ABS(xcl(l1,i)-x(l1,j))
    END DO
    a = zero
    !dir$ loop_count min(1),max(20),avg(10)
    DO  l1=1,nparm
      IF (w(l1) > a) a = w(l1)
    END DO
    IF (a >= b) CYCLE
    WRITE(ipr,902) iii
    WRITE(*,902) iii
  902 FORMAT(' SAMPLE POINT ADDED TO THE CLUSTER NO. ', i2)
    !dir$ loop_count min(1),max(20),avg(10)
    DO  ii=1,nparm
      w(ii) = x(ii,j)*mmax(ii) + mmin(ii)
    END DO
    WRITE(ipr,903) f(j), w(1:nparm)
    WRITE (*,903) f(j), w(1:nparm)
    903 FORMAT(' ', g14.8/ ('    ', 5(g14.8, ' ')))
    ig = ig+1
    IF (ig >= ng) GO TO 395
    in1 = in1+1
    fcl(in1) = f(j)
  !dir$ loop_count min(1),max(20),avg(10)
  !dir$ assume_aligned xcl:64
  !dir$ assume_aligned x:64
  !dir$ ivdep
  !$omp simd simdlen(8) linear(ii:1)
    DO  ii=1,nparm
      xcl(ii,in1) = x(ii,j)
    END DO
    ic(j) = iii
  END DO
  i = i+1
  IF (i <= in1) GO TO 95
END DO
IF (n1 <= 0) GO TO 200

!     CLUSTERING TO X1
DO  iii=1,n1
  i = 1
  in1 = i
  fcl(i) = f1(iii)
   !dir$ loop_count min(1),max(20),avg(10)
    DO  j=1,nparm
        xcl(j,i) = x1(j,iii)
    END DO
  155 DO  j=1,ng
    IF (ic(j) /= 0) CYCLE
    IF (fcl(i) >= f(j)) CYCLE
     !dir$ loop_count min(1),max(20),avg(10)
    DO  l1=1,nparm
      w(l1) = ABS(xcl(l1,i)-x(l1,j))
    END DO
    a = zero
    DO  l1=1,nparm
      IF (w(l1) > a) a=w(l1)
    END DO
    IF (a >= b) CYCLE
    WRITE(ipr,902) ic1(iii)
    WRITE(*,902) ic1(iii)
     !dir$ loop_count min(1),max(20),avg(10)
    DO  ii=1,nparm
      w(ii) = x(ii,j)*mmax(ii) + mmin(ii)
    END DO
    WRITE(ipr,903) f(j), w(1:nparm)
    WRITE(*,903) f(j), w(1:nparm)
    ig = ig+1
    IF (ig >= ng) GO TO 395
    in1 = in1+1
    fcl(in1) = f(j)
     !dir$ loop_count min(1),max(20),avg(10)
    DO  ii=1,nparm
      xcl(ii,in1) = x(ii,j)
    END DO
    ic(j) = ic1(iii)
  END DO
  i = i+1
  IF (i <= in1) GO TO 155
END DO

!     LOCAL SEARCH
200 it = 0
DO  i1=1,ng
   IF (ic(i1) /= 0) CYCLE
    !dir$ loop_count min(1),max(20),avg(10)
  DO  i=1,nparm
    y(i) = x(i,i1)
  END DO
  ff = f(i1)
  CALL local(m, nparm, relcon, maxfn, y, ff, nfe1, mmin, mmax)
  IF (nc <= 0) GO TO 290
  DO  iv=1,nc
      !dir$ loop_count min(1),max(20),avg(10)
    DO  l1=1,nparm
      w(l1) = ABS(x0(l1,iv) - y(l1))
    END DO
    a = zero
    DO  l1=1,nparm
      IF (w(l1) > a) a = w(l1)
    END DO
    IF (a < bb) GO TO 255
  END DO
  GO TO 290

!       NEW SEED-POINT
  255 n1 = n1 + 1
  WRITE(ipr,905) iv, nfe1
  WRITE(*,905) iv,nfe1
  905 FORMAT(' NEW SEED POINT ADDED TO THE CLUSTER NO. ', i2, ', NFEV=', i5)
   !dir$ loop_count min(1),max(20),avg(10)
  DO  ii=1,nparm
    w(ii) = x(ii,i1)*mmax(ii) + mmin(ii)
  END DO
  WRITE(ipr,903) ff, w(1:nparm)
  WRITE(*,903) ff, w(1:nparm)
  IF (ff >= f0(iv)) GO TO 280
  WRITE(ipr,906) iv, f0(iv), ff
  WRITE(*,906) iv,f0(iv),ff
  906 FORMAT(' *** IMPROVEMENT ON THE LOCAL MINIMUM NO. ',  &
             i2, ':', g14.8, ' FOR ', g14.8)
  w(1:nparm) = y(1:nparm)*mmax(1:nparm) + mmin(1:nparm)
  WRITE(ipr,903) ff, w(1:nparm)
  WRITE(*,903) ff, w(1:nparm)
  f0(iv) = ff
   !dir$ loop_count min(1),max(20),avg(10)
  DO  ii=1,nparm
    x0(ii,iv) = y(ii)
  END DO
  280 IF (n1 > 20) GO TO 470
   !dir$ loop_count min(1),max(20),avg(10)
  DO  ii=1,nparm
    x1(ii,n1) = x(ii,i1)
    xcl(ii,1) = x(ii,i1)
  END DO
  f1(n1) = f(i1)
  fcl(1) = f(i1)
  ic1(n1) = iv
  icj = iv
  GO TO 305

!     NEW LOCAL MINIMUM
  290 nc = nc+1
  ncp = ncp+1
  WRITE(ipr,907) nc, ff, nfe1
  WRITE(*,907) nc, ff, nfe1
  907 FORMAT(' *** THE LOCAL MINIMUM NO. ', i2, ': ', g14.8, ', NFEV=', i5)
   !dir$ loop_count min(1),max(20),avg(10)
  DO  ii=1,nparm
    w(ii) = y(ii)*mmax(ii) + mmin(ii)
  END DO
  WRITE(ipr,903) ff, w(1:nparm)
  WRITE(*,903) ff, w(1:nparm)
   !dir$ loop_count min(1),max(20),avg(10)
  DO  ii=1,nparm
    x0(ii,nc) = y(ii)
    xcl(ii,1) = y(ii)
  END DO
  fcl(1) = ff
  f0(nc) = ff
  IF (nc >= 20) GO TO 475
  it = 1
  icj = nc

!    CLUSTERING TO THE NEW POINT
  305 nfe = nfe + nfe1
  ic(i1) = icj
  ig = ig + 1
  IF (ig >= ng) EXIT
  i = 1
  in1 = i
  310 DO  j=1,ng
    IF (ic(j) /= 0) CYCLE
    IF (fcl(i) >= f(j)) CYCLE
     !dir$ loop_count min(1),max(20),avg(10)
    DO  l1=1,nparm
      w(l1) = ABS(xcl(l1,i) - x(l1,j))
    END DO
    a = zero
    DO  l1=1,nparm
      IF (w(l1) > a) a = w(l1)
    END DO
    IF (a >= b) CYCLE
    in1 = in1 + 1
    !dir$ loop_count min(1),max(20),avg(10)
    !dir$ assume_aligned xcl:64
    !dir$ assume_aligned x:64
    DO  ii=1,nparm
      xcl(ii,in1) = x(ii,j)
    END DO
    fcl(in1) = f(j)
    ic(j) = icj
    WRITE(ipr,902) icj
    WRITE(*,902) icj
    !dir$ loop_count min(1),max(20),avg(10)
    !dir$ assume_aligned x:64
    DO  ii=1,nparm
      w(ii) = x(ii,j)*mmax(ii) + mmin(ii)
    END DO
    WRITE(ipr,903) f(j), w(1:nparm)
    WRITE(*,903) f(j), w(1:nparm)
    ig = ig+1
    IF (ig >= ng) EXIT
  END DO
  i = i+1
  IF (i < in1) GO TO 310
END DO
IF (it /= 0) GO TO 20

!      PRINT RESULTS
395 WRITE(ipr,908)
WRITE(*,908)
908 FORMAT(///' LOCAL MINIMA FOUND:'//)
IF (nc <= 1) GO TO 430
inum = nc - 1
DO  i=1,inum
  im = i
  fm = f0(i)
  inum1 = i + 1
  DO  j=inum1,nc
    IF (f0(j) >= fm) CYCLE
    im = j
    fm = f0(j)
  END DO
  IF (im <= i) CYCLE
  a = fm
  y(1:nparm) = x0(1:nparm,im)
  inum1 = im-i
  DO  j=1,inum1
    inum2 = im - j
    f0(inum2+1) = f0(inum2)
    !dir$ loop_count min(1),max(20),avg(10)
    !dir$ assume_aligned x0:64
    
    DO  jj=1,nparm
      x0(jj,inum2+1) = x0(jj,inum2)
    END DO
  END DO
  f0(i) = a
  x0(1:nparm,i) = y(1:nparm)
END DO
430 IF (nc <= 0) GO TO 445
!dir$ assume_aligned x0:64
DO  i=1,nc
  x0(1:nparm,i) = x0(1:nparm,i)*mmax(1:nparm) + mmin(1:nparm)
  WRITE(ipr,903) f0(i), x0(1:nparm,i)
  WRITE(*,903) f0(i), x0(1:nparm,i)
END DO

445 WRITE(ipr,911) nfe
WRITE(*,911) nfe
911 FORMAT(//' NORMAL TERMINATION AFTER ', i5, ' FUNCTION EVALUATIONS'//)
RETURN

460 WRITE(ipr,914)
WRITE(*,914)
914 FORMAT(' ***   DATA ERROR')
STOP

465 WRITE(ipr,915)
WRITE(*,915)
915 FORMAT(' ***   TOO MANY SAMPLES')
GO TO 395

470 WRITE(ipr,916)
WRITE(*,916)
916 FORMAT(' ***   TOO MANY NEW SEED POINTS')
GO TO 395

475 WRITE(ipr,917)
WRITE(*,917)
917 FORMAT(' ***   TOO MANY CLUSTERS')
GO TO 395
END SUBROUTINE global



SUBROUTINE fun(r, f, nparm, m, mmin, mmax)
      !dir$ attributes code_align : 32 :: fun
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=skylake_avx512 :: fun
REAL (dp), INTENT(IN)   :: r(:)
REAL (dp), INTENT(OUT)  :: f
INTEGER(kind=i4), INTENT(IN)     :: nparm
INTEGER(kind=i4), INTENT(IN)     :: m
REAL (dp), INTENT(IN)   :: mmin(:)
REAL (dp), INTENT(IN)   :: mmax(:)

INTERFACE
  SUBROUTINE funct(x, f, nparm, m)
    IMPLICIT NONE
    !INTEGER(kind=i4), PARAMETER      :: dp = SELECTED_REAL_KIND(12, 60)
    import :: dp
    import :: i4
    REAL (dp), INTENT(IN)   :: x(:)
    REAL (dp), INTENT(OUT)  :: f
    INTEGER(kind=i4), INTENT(IN)     :: nparm, m
  END SUBROUTINE funct
END INTERFACE

REAL (dp) :: x(nparm)

! N.B. mmin = mid-point between lower & upper limits
!      mmax = (upper - lower limits) / 2

x(1:nparm) = mmax(1:nparm)*r(1:nparm) + mmin(1:nparm)
CALL funct(x, f, nparm, m)

RETURN
END SUBROUTINE fun



!   ROUTINE NAME - LOCAL
 
! Code converted using TO_F90 by Alan Miller
! Date: 1999-12-27  Time: 13:16:43

!-----------------------------------------------------------------------

!   LATEST REVISION - JULY 31, 1986

!   PURPOSE  - MINIMUM OF A FUNCTION OF N VARIABLES USING
!       A QUASI-NEWTON METHOD

!   USAGE  - CALL LOCAL (M, N, EPS, MAXFN, X, F, NFEV, mmin, mmax)

!   ARGUMENTS  M - THE NUMBER OF RESIDUAL FUNCTIONS (INPUT)
!       NOT USED IN THIS ROUTINE.
!   N - THE NUMBER OF PARAMETERS (I.E., THE LENGTH OF X) (INPUT)
!   EPS - CONVERGENCE CRITERION. (INPUT).  THE ACCURACY REQUIRED IN THE
!       PARAMETER ESTIMATES.
!       THIS CONVERGENCE CONDITION IS SATISFIED IF ON TWO SUCCESSIVE
!       ITERATIONS, THE PARAMETER ESTIMATES (I.E.,X(I), I=1,...,N) DIFFERS,
!       COMPONENT BY COMPONENT, BY AT MOST EPS.
!   MAXFN - MAXIMUM NUMBER OF FUNCTION EVALUATIONS (I.E.,
!       CALLS TO SUBROUTINE FUN) ALLOWED. (INPUT)
!   X - VECTOR OF LENGTH N CONTAINING PARAMETER VALUES.
!     ON INPUT, X MUST CONTAIN THE INITIAL PARAMETER ESTIMATES.
!     ON OUTPUT, X CONTAINS THE FINAL PARAMETER
!       ESTIMATES AS DETERMINED BY LOCAL.
!   F - A SCALAR CONTAINING THE VALUE OF THE FUNCTION
!       AT THE FINAL PARAMETER ESTIMATES. (OUTPUT)
!   NFEV - THE NUMBER OF FUNCTION EVALUATIONS (OUTPUT)
!   W - A VECTOR OF LENGTH 3*N USED AS WORKING SPACE.
!   mmin    - A VECTOR OF LENGTH N CONTAINING THE LOWER
!              BOUNDS OF THE PARAMETERS, SO X(I) IS
!              SEARCHED IN THE INTERVAL (mmin(I),mmax(I)).
!              (INPUT)
!   mmax    - A VECTOR OF LENGTH N CONTAINING THE UPPER
!              BOUNDS OF THE PARAMETERS. (INPUT)

!   REQUIRED ROUTINES - UPDATE, FUN

!   FUN   - A USER SUPPLIED SUBROUTINE WHICH CALCULATES THE FUNCTION F FOR
!           GIVEN PARAMETER VALUES X(1),X(2),...,X(N).
!           THE CALLING SEQUENCE HAS THE FOLLOWING FORM
!              CALL FUN(X, F, N, M, mmin, mmax)
!           WHERE X IS A VEKTOR OF LENGTH N.
!           FUN MUST APPEAR IN AN EXTERNAL STATEMENT IN THE CALLING PROGRAM.
!           FUN MUST NOT ALTER THE VALUES OF X(I), I=1,...,N OR N.

!-----------------------------------------------------------------------

SUBROUTINE local (m, n, eps, maxfn, x, f, nfev, mmin, mmax)
      !dir$ attributes code_align : 32 :: local
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=skylake_avx512 :: local
! N.B. Argument W has been removed.

!       SPECIFICATIONS FOR ARGUMENTS

INTEGER(kind=i4), INTENT(IN)        :: m
INTEGER(kind=i4), INTENT(IN)        :: n
REAL (dp), INTENT(IN)      :: eps
INTEGER(kind=i4), INTENT(IN)        :: maxfn
REAL (dp), INTENT(IN OUT)  :: x(:)
REAL (dp), INTENT(OUT)     :: f
INTEGER(kind=i4), INTENT(OUT)       :: nfev
REAL (dp), INTENT(IN)      :: mmin(:)
REAL (dp), INTENT(IN)      :: mmax(:)


!       SPECIFICATIONS FOR LOCAL VARIABLES
REAL (dp) :: w(3*n)
!dir$ attribute align : 64 :: w
INTEGER(kind=i4)   :: ig, igg, im1, is, idiff, ir, ij, i, iopt, j, nm1, jj, jp1, l, &
             kj, k, link, itn, ii, jnt, np1, jb, nj, ier
REAL (dp) :: hh, hjj, v, df, relx, gs0, diff, aeps, alpha, ff, tot, f1, f2, &
     z, gys, dgs, sig, zz, hhh, ghh, g(n), h(n*n)
!dir$ attribute align : 64 :: h
REAL (dp), PARAMETER :: reps = 1.1921E-07_dp, ax = 0.1_dp, zero = 0.0_dp,  &
                        one = 1.0_dp, half = 0.5_dp, seven = 7.0_dp,  &
                        five = 5.0_dp, twelve = 12.0_dp, p1 = 0.1_dp

!       INITIALIZATION
!       FIRST EXECUTABLE STATEMENT
iopt = 0

!   IOPT - OPTIONS SELECTOR. (INPUT)
!     IOPT = 0 CAUSES LOCAL TO INITIALIZE THE
!       HESSIAN MATRIX H TO THE IDENTITY MATRIX.
!     IOPT = 1 INDICATES THAT H HAS BEEN INITIALIZED
!       BY THE USER TO A POSITIVE DEFINITE MATRIX.
!     IOPT = 2 CAUSES LOCAL TO COMPUTE THE DIAGONAL
!       VALUES OF THE HESSIAN MATRIX AND SET H TO
!       A DIAGONAL MATRIX CONTAINING THESE VALUES.
!     IOPT = 3 CAUSES LOCAL TO COMPUTE AN ESTIMATE
!       OF THE HESSIAN IN H.
ier = 0
hh = SQRT(reps)
ig = n
igg = n+n
is = igg
idiff = 1
ir = n
w(1) = -one
w(2) = zero
w(3) = zero

!       EVALUATE FUNCTION AT STARTING POINT
g(1:n) = x(1:n)
CALL fun (g, f, n, m, mmin, mmax)
nfev = 1
IF (iopt == 1) GO TO 45

!       SET OFF-DIAGONAL ELEMENTS OF H TO 0.0
IF (n == 1) GO TO 25
ij = 2
!dir$ loop_count min(1),max(20),avg(10)
!dir$ assume_aligned h:64
DO  i=2,n
  DO  j=2,i
    h(ij) = zero
    ij = ij + 1
  END DO
  ij = ij + 1
END DO
IF (iopt /= 0) GO TO 25

!       SET DIAGONAL ELEMENTS OF H TO ONE
ij = 0
!dir$ loop_count min(1),max(20),avg(10)
!dir$ assume_aligned h:64
DO  i=1,n
  ij = ij+i
  h(ij) = one
END DO
GO TO 80

!       GET DIAGONAL ELEMENTS OF HESSIAN
25 im1 = 1
nm1 = 1
np1 = n+1
!dir$ loop_count min(1),max(21),avg(10)
DO  i=2,np1
  hhh = hh*MAX(ABS(x(im1)), ax)
  g(im1) = x(im1) + hhh
  CALL fun (g, f2, n, m, mmin, mmax)
  g(im1) = g(im1) + hhh
  CALL fun (g, ff, n, m, mmin, mmax)
  h(nm1) = (ff-f2+f-f2)/(hhh*hhh)
  g(im1) = x(im1)
  im1 = i
  nm1 = i+nm1
END DO
nfev = nfev+n+n
IF (iopt /= 3 .OR. n == 1) GO TO 45

!       GET THE REST OF THE HESSIAN
jj = 1
ii = 2
!dir$ loop_count min(1),max(20),avg(10)
DO  i=2,n
  ghh = hh*MAX(ABS(x(i)), ax)
  g(i) = x(i)+ghh
  CALL fun (g, f2, n, m, mmin, mmax)
  DO  j=1,jj
    hhh = hh*MAX(ABS(x(j)), ax)
    g(j) = x(j) + hhh
    CALL fun (g, ff, n, m, mmin, mmax)
    g(i) = x(i)
    CALL fun (g, f1, n, m, mmin, mmax)
!     H(II) = (FF-F1-F2+F)*SQREPS
    h(ii) = (ff-f1-f2+f)/(hhh*ghh)
    ii = ii+1
    g(j) = x(j)
  END DO
  jj = jj+1
  ii = ii+1
END DO
nfev = nfev + ((n*n-n)/2)

!       FACTOR H TO L*D*L-TRANSPOSE
45 ir = n
IF (n > 1) GO TO 50
IF (h(1) > zero) GO TO 80
h(1) = zero
ir = 0
GO TO 75
50 nm1 = n-1
jj = 0
DO  j=1,n
  jp1 = j+1
  jj = jj+j
  hjj = h(jj)
  IF (hjj > zero) GO TO 55
  h(jj) = zero
  ir = ir-1
  CYCLE
  55 IF (j == n) CYCLE
  ij = jj
  l = 0
  DO  i=jp1,n
    l = l+1
    ij = ij+i-1
    v = h(ij)/hjj
    kj = ij
    DO  k=i,n
      h(kj+l) = h(kj+l) - h(kj)*v
      kj = kj + k
    END DO
    h(ij) = v
  END DO
END DO
75 IF (ir /= n) THEN
  ier = 129
  GO TO 9000
END IF
80 itn = 0
df = -one

!       EVALUATE GRADIENT W(IG+I),I=1,...,N
85 link = 1
GO TO 260

!       BEGIN ITERATION LOOP
90 IF (nfev >= maxfn) GO TO 225
itn = itn+1

DO  i=1,n
  w(i) = -w(ig+i)
END DO

!       DETERMINE SEARCH DIRECTION W
!         BY SOLVING H*W = -G WHERE
!         H = L*D*L-TRANSPOSE
IF (ir < n) GO TO 125
!       N .EQ. 1
g(1) = w(1)
IF (n > 1) GO TO 100
w(1) = w(1)/h(1)
GO TO 125
!       N > 1
100 ii = 1

!       SOLVE L*W = -G
DO  i=2,n
  ij = ii
  ii = ii+i
  v = w(i)
  im1 = i-1
  DO  j=1,im1
    ij = ij+1
    v = v - h(ij)*w(j)
  END DO
  g(i) = v
  w(i) = v
END DO

!       SOLVE (D*LT)*Z = W WHERE
!                                    LT = L-TRANSPOSE
w(n) = w(n)/h(ii)
jj = ii
nm1 = n-1
DO  nj=1,nm1
!       J = N-1,N-2,...,1
  j = n-nj
  jp1 = j+1
  jj = jj-jp1
  v = w(j)/h(jj)
  ij = jj
  DO  i=jp1,n
    ij = ij+i-1
    v = v - h(ij)*w(i)
  END DO
  w(j) = v
END DO

!       DETERMINE STEP LENGTH ALPHA
125 relx = zero
gs0 = zero
DO  i=1,n
  w(is+i) = w(i)
  diff = ABS(w(i)) / MAX(ABS(x(i)),ax)
  relx = MAX(relx, diff)
  gs0 = gs0 + w(ig+i)*w(i)
END DO
IF (relx == zero) GO TO 230
aeps = eps/relx
ier = 130
IF (gs0 >= zero) GO TO 230
IF (df == zero) GO TO 230
ier = 0
alpha = (-df-df)/gs0
IF (alpha <= zero) alpha = one
alpha = MIN(alpha,one)
IF (idiff == 2) alpha = MAX(p1,alpha)
ff = f
tot = zero
jnt = 0

!       SEARCH ALONG X + ALPHA*W
135 IF (nfev >= maxfn) GO TO 225
DO  i=1,n
  w(i) = x(i) + alpha*w(is+i)
END DO
CALL fun (w, f1, n, m, mmin, mmax)
nfev = nfev+1
IF (f1 >= f) GO TO 165
f2 = f
tot = tot + alpha
145 ier = 0
f = f1
x(1:n) = w(1:n)
IF (jnt-1 < 0) THEN
  GO TO   155
ELSE IF (jnt-1 == 0) THEN
  GO TO 185
ELSE
  GO TO 190
END IF

155 IF (nfev >= maxfn) GO TO 225
DO  i=1,n
  w(i) = x(i) + alpha*w(is+i)
END DO
CALL fun (w, f1, n, m, mmin, mmax)
nfev = nfev+1
IF (f1 >= f) GO TO 190
IF (f1+f2 >= f+f .AND. seven*f1+five*f2 > twelve*f) jnt = 2
tot = tot + alpha
alpha = alpha + alpha
GO TO 145

165 IF (f == ff .AND. idiff == 2 .AND. relx > eps) ier = 130
IF (alpha < aeps) GO TO 230
IF (nfev >= maxfn) GO TO 225
alpha = half*alpha
DO  i=1,n
  w(i) = x(i) + alpha*w(is+i)
END DO
CALL fun (w, f2, n, m, mmin, mmax)
nfev = nfev + 1
IF (f2 >= f) GO TO 180
tot = tot + alpha
ier = 0
f = f2
x(1:n) = w(1:n)
GO TO 185

180 z = p1
IF (f1+f > f2+f2) z = one + half*(f-f1)/(f+f1-f2-f2)
z = MAX(p1,z)
alpha = z*alpha
jnt = 1
GO TO 135

185 IF (tot < aeps) GO TO 230
190 alpha = tot

!       SAVE OLD GRADIENT
DO  i=1,n
  w(i) = w(ig+i)
END DO

!       EVALUATE GRADIENT W(IG+I), I=1,...,N
link = 2
GO TO 260
200 IF (nfev >= maxfn) GO TO 225
gys = zero
DO  i=1,n
  gys = gys + w(ig+i)*w(is+i)
  w(igg+i) = w(i)
END DO
df = ff-f
dgs = gys-gs0
IF (dgs <= zero) GO TO 90
IF (dgs + alpha*gs0 > zero) GO TO 215

!       UPDATE HESSIAN H USING
!         COMPLEMENTARY DFP FORMULA
sig = one/gs0
ir = -ir
CALL update (h, n, w, sig, g, ir, 0, zero)
DO  i=1,n
  g(i) = w(ig+i) - w(igg+i)
END DO
sig = one/(alpha*dgs)
ir = -ir
CALL update (h, n, g, sig, w, ir, 0, zero)
GO TO 90

!       UPDATE HESSIAN USING DFP FORMULA
215 zz = alpha/(dgs - alpha*gs0)
sig = -zz
CALL update (h, n, w, sig, g, ir, 0, reps)
z = dgs*zz - one
DO  i=1,n
  g(i) = w(ig+i) + z*w(igg+i)
END DO
sig = one/(zz*dgs*dgs)
CALL update (h, n, g, sig, w, ir, 0, zero)
GO TO 90

!       MAXFN FUNCTION EVALUATIONS
225 GO TO 235
230 IF (idiff == 2) GO TO 235

!       CHANGE TO CENTRAL DIFFERENCES
idiff = 2
GO TO 85
235 IF (relx > eps .AND. ier == 0) GO TO 85

!       COMPUTE H = L*D*L-TRANSPOSE AND OUTPUT
IF (n == 1) GO TO 9000
np1 = n+1
nm1 = n-1
jj = (n*(np1))/2
DO  jb=1,nm1
  jp1 = np1-jb
  jj = jj-jp1
  hjj = h(jj)
  ij = jj
  l = 0
  DO  i=jp1,n
    l = l+1
    ij = ij+i-1
    v = h(ij)*hjj
    kj = ij
    DO  k=i,n
      h(kj+l) = h(kj+l) + h(kj)*v
      kj = kj + k
    END DO
    h(ij) = v
  END DO
  hjj = h(jj)
END DO
GO TO 9000

!        EVALUATE GRADIENT
260 IF (idiff == 2) GO TO 270

!       FORWARD DIFFERENCES
!         GRADIENT = W(IG+I), I=1,...,N
!dir$ loop_count min(1),max(20),avg(10)
DO  i=1,n
  z = hh*MAX(ABS(x(i)),ax)
  zz = x(i)
  x(i) = zz + z
  CALL fun (x, f1, n, m, mmin, mmax)
  w(ig+i) = (f1-f)/z
  x(i) = zz
END DO
nfev = nfev+n
SELECT CASE ( link )
  CASE (    1)
    GO TO 90
  CASE (    2)
    GO TO 200
END SELECT

!       CENTRAL DIFFERENCES
!         GRADIENT = W(IG+I), I=1,...,N
!dir$ loop_count min(1),max(20),avg(10)
270 DO  i=1,n
  z = hh*MAX(ABS(x(i)), ax)
  zz = x(i)
  x(i) = zz + z
  CALL fun (x, f1, n, m, mmin, mmax)
  x(i) = zz-z
  CALL fun (x, f2, n, m, mmin, mmax)
  w(ig+i) = (f1-f2)/(z+z)
  x(i) = zz
END DO
nfev = nfev+n+n
SELECT CASE ( link )
  CASE (    1)
    GO TO 90
  CASE (    2)
    GO TO 200
END SELECT

!       RETURN
9000 RETURN
END SUBROUTINE local



!   ROUTINE NAME - UPDATE

!-----------------------------------------------------------------------

!   LATEST REVISION     - JULY 31, 1986
!     (CHANGES IN COMMENTS)

!   PURPOSE  - NUCLEUS CALLED ONLY BY ROUTINE LOCAL

!   REQD. ROUTINES - NONE REQUIRED

!-----------------------------------------------------------------------

SUBROUTINE update (a, n, z, sig, w, ir, mk, eps)
  !       SPECIFICATIONS FOR ARGUMENTS
      !dir$ attributes code_align : 32 :: update
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=skylake_avx512 :: update

REAL (dp), INTENT(OUT)     :: a(:)
INTEGER(kind=i4), INTENT(IN)        :: n
REAL (dp), INTENT(IN OUT)  :: z(:)
REAL (dp), INTENT(IN)      :: sig
REAL (dp), INTENT(IN OUT)  :: w(:)
INTEGER(kind=i4), INTENT(OUT)       :: ir
INTEGER(kind=i4), INTENT(IN)        :: mk
REAL (dp), INTENT(IN)      :: eps


!       SPECIFICATIONS FOR LOCAL VARIABLES
INTEGER(kind=i4)   :: j, jj, ij, jp1, i, ii, mm
REAL (dp) :: ti, v, tim, al, r, b, gm, y
REAL (dp), PARAMETER :: zero = 0.0_dp, one = 1.0_dp, four = 4.0_dp

!       UPDATE FACTORS GIVEN IN A
!         SIG*Z*Z-TRANSPOSE IS ADDED
!       FIRST EXECUTABLE STATEMENT
IF (n > 1) GO TO 5

!       N .EQ. 1
a(1) = a(1) + sig*z(1)*z(1)
ir = 1
IF (a(1) > zero) GO TO 9005
a(1) = zero
ir = 0
GO TO 9005

!       N > 1
5 IF (sig > zero) GO TO 65
IF (sig == zero .OR. ir == 0) GO TO 9005
ti = one/sig
jj = 0
IF (mk == 0) GO TO 15

!       L*W = Z ON INPUT
DO  j=1,n
  jj = jj+j
  IF (a(jj) /= zero) ti = ti + (w(j)*w(j))/a(jj)
END DO
GO TO 40

!       SOLVE L*W = Z
15 w(1:n) = z(1:n)
DO  j=1,n
  jj = jj+j
  v = w(j)
  IF (a(jj) > zero) GO TO 25
  w(j) = zero
  CYCLE
  25 ti = ti+(v*v)/a(jj)
  IF (j == n) CYCLE
  ij = jj
  jp1 = j+1
  DO  i=jp1,n
    ij = ij+i-1
    w(i) = w(i) - v*a(ij)
  END DO
END DO

!        SET TI, TIM AND W
40 IF (ir <= 0) GO TO 45
IF (ti > zero) GO TO 50
IF (mk-1 > 0) THEN
  GO TO  55
ELSE
  GO TO  65
END IF

45 ti = zero
ir = -ir-1
GO TO 55

50 ti = eps/sig
IF (eps == zero) ir = ir-1
55 tim = ti
ii = jj
i = n
DO  j=1,n
  IF (a(ii) /= zero) tim = ti - (w(i)*w(i))/a(ii)
  w(i) = ti
  ti = tim
  ii = ii-i
  i = i-1
END DO
mm = 1
GO TO 70

65 mm = 0
tim = one/sig
70 jj = 0

!       UPDATE A
DO  j=1,n
  jj = jj+j
  ij = jj
  jp1 = j+1

!       UPDATE A(J,J)
  v = z(j)
  IF (a(jj) > zero) GO TO 85

!       A(J,J) .EQ. ZERO
  IF (ir > 0 .OR. sig < zero .OR. v == zero) GO TO 80
  ir = 1-ir
  a(jj) = (v*v)/tim
  IF (j == n) GO TO 9005
  DO  i=jp1,n
    ij = ij+i-1
    a(ij) = z(i)/v
  END DO
  GO TO 9005
  80 ti = tim
  CYCLE

!       A(J,J) .GT. ZERO
  85 al = v/a(jj)
  ti = w(j)
  IF (mm == 0) ti = tim + v*al
  r = ti/tim
  a(jj) = r*a(jj)
  IF (r == zero) EXIT
  IF (j == n) EXIT

!       UPDATE REMAINDER OF COLUMN J
  b = al/ti
  IF (r > four) GO TO 95
  DO  i=jp1,n
    ij = ij+i-1
    z(i) = z(i) - v*a(ij)
    a(ij) = a(ij) + b*z(i)
  END DO
  GO TO 105
  95 gm = tim/ti
  DO  i=jp1,n
    ij = ij+i-1
    y = a(ij)
    a(ij) = b*z(i) + y*gm
    z(i) = z(i) - v*y
  END DO
  105  tim = ti
END DO
IF (ir < 0) ir = -ir

9005 RETURN
END SUBROUTINE update

END MODULE global_minimum


#if 0

=============Usage Example=================

!   PROGRAM NAME    - FIT
 
! Code converted using TO_F90 by Alan Miller
! Date: 1999-12-27  Time: 13:16:33


! N.B. The files INPUT & OUTPUT follow at the end of the code.

!-----------------------------------------------------------------------

!   LATEST REVISION - DECEMBER 27, 1986

!   PURPOSE     - PARAMETER ESTIMATION OF RESPIRATORY MODELS
!                 BY A GLOBAL OPTIMIZATION ALGORITHM

!   REQUIRED ROUTINES   - GLOBAL, LOCAL, URDMN, FUNCT, FUN

!-----------------------------------------------------------------------

MODULE fit_common
IMPLICIT NONE
INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(12, 60)

! COMMON /fu/ om(50), rezz(50), irel, zre(50), zim(50)

REAL (dp), SAVE :: om(50), rezz(50), zre(50), zim(50)
INTEGER, SAVE   :: irel

END MODULE fit_common



PROGRAM fit
USE fit_common
USE global_minimum
IMPLICIT NONE

REAL (dp)             :: x0(15,20), f00(20), MIN(18), MAX(18),   &
                         df, do, f0, o0, o1, tpi
CHARACTER (LEN=80)    :: label
INTEGER               :: i, in, ipr, k, m, nc, ng, nparm, npt, nsampl, nsig
REAL (dp), PARAMETER  :: zero = 0.0_dp, one = 1.0_dp, onep5 = 1.5_dp,  &
                         eight = 8.0_dp, tausend = 1000._dp
INTEGER, ALLOCATABLE  :: seed(:)

INTERFACE
  SUBROUTINE ladder(x, f, f0, df, o0, DO, npt, ipr, np)
    USE fit_common
    IMPLICIT NONE
    REAL (dp), INTENT(IN)      :: x(:)
    REAL (dp), INTENT(IN)      :: f
    REAL (dp), INTENT(IN)      :: f0
    REAL (dp), INTENT(IN)      :: df
    REAL (dp), INTENT(IN)      :: o0
    REAL (dp), INTENT(IN OUT)  :: DO
    INTEGER, INTENT(IN OUT)    :: npt
    INTEGER, INTENT(IN OUT)    :: ipr
    INTEGER, INTENT(IN OUT)    :: np
  END SUBROUTINE ladder
END INTERFACE

tpi = eight*ATAN(one)
in = 5
ipr = 6
OPEN(5, FILE='INPUT')
OPEN(6, FILE='OUTPUT')
READ(in, 901) label
901 FORMAT(A)
WRITE(ipr, 902) label
WRITE(*, 902) label
902 FORMAT(' ', A/)
nparm = 5
READ(in, *) MIN(1), MAX(1), MIN(2), MAX(2), MIN(3), MAX(3), MIN(4), MAX(4), &
            MIN(5), MAX(5)
920 FORMAT(' ', f9.4, ' ', f9.4)
WRITE(ipr, 920) MIN(1), MAX(1), MIN(2), MAX(2), MIN(3), MAX(3),  &
                MIN(4), MAX(4), MIN(5), MAX(5)
WRITE(*, 920) MIN(1), MAX(1), MIN(2), MAX(2), MIN(3), MAX(3),  &
              MIN(4), MAX(4), MIN(5), MAX(5)
WRITE(ipr, 905)
WRITE(*, 905)
905 FORMAT(// ' RUN PARAMETERS'/)
READ(in, *) irel, nsampl, ng, nsig
WRITE(ipr, 922) irel, nsampl, ng, nsig
WRITE(*, 922) irel, nsampl, ng, nsig
922 FORMAT(' ', i2, ' ', i4, ' ', i2, ' ', i1 /)
i = 1
WRITE(ipr, 906)
WRITE(*, 906)
906 FORMAT(' SAMPLE'/)

3 READ(in, *, END=6, ERR=9) om(i), zre(i), zim(i)
WRITE(ipr, 925) om(i), zre(i), zim(i)
WRITE(*, 925) om(i), zre(i), zim(i)
925 FORMAT(' ', f6.3, ' ', f7.2, ' ', f8.2)
i = i + 1
GO TO 3

6 m = i - 1
WRITE(ipr, 926)
WRITE(*, 926)
926 FORMAT(//)
o0 = tausend
o1 = zero
DO = (om(2) - om(1))*tpi
DO  i=1,m
  om(i) = om(i)*tpi
  IF (om(i) < o0) o0 = om(i)
  IF (om(i) > o1) o1 = om(i)
  IF (i <= 1) CYCLE
  IF (om(i) - om(i-1) < DO) DO = om(i) - om(i-1)
END DO

! Set the random number seed

CALL RANDOM_SEED(size=k)
ALLOCATE (seed(k))
WRITE(*, '(1x, a, i4, a)') 'Enter ', k, ' integers as random number seeds: '
READ(*, *) seed
CALL RANDOM_SEED(put=seed)
WRITE(ipr, '(a / (7i11) )') ' Random number seed(s): ', seed
WRITE(ipr, * )

f0 = o0/tpi
df = DO/tpi
npt = INT((o1-o0)/DO + onep5)
DO  i=1,m
  rezz(i) = SQRT(zre(i)*zre(i) + zim(i)*zim(i))
END DO
CALL global(MIN, MAX, nparm, m, nsampl, ng, ipr, nsig, x0, nc, f00)
DO  i=1,nc
  CALL ladder(x0(1:,i), f00(i), f0, df, o0, DO, npt, ipr, nparm)
END DO

9 STOP
END PROGRAM fit
    

SUBROUTINE funct(x, value, nparm, m)

USE fit_common
IMPLICIT NONE

REAL (dp), INTENT(IN)   :: x(:)
REAL (dp), INTENT(OUT)  :: value
INTEGER, INTENT(IN)     :: nparm
INTEGER, INTENT(IN)     :: m

REAL (dp) :: f(100), zimi, zrei
INTEGER   :: i, j, kk, mm

DO  kk=1,m
  zrei = x(1) + (x(2)/(om(kk)**x(3)))
  zimi = om(kk)*x(4) - (x(5)/(om(kk)**x(3)))
  zrei = zre(kk) - zrei
  zimi = zim(kk) - zimi
  j = kk*2
  i = j-1
  IF (irel /= 0) GO TO 100
  f(i) = zrei/rezz(kk)
  f(j) = zimi/rezz(kk)
  CYCLE
  100 f(i) = zrei
  f(j) = zimi
END DO

mm = m+m
value = SUM( f(1:mm)**2 )
value = SQRT(value/m)
RETURN
END SUBROUTINE funct


SUBROUTINE ladder(x, f, f0, df, o0, DO, npt, ipr, np)

USE fit_common
IMPLICIT NONE

REAL (dp), INTENT(IN)      :: x(:)
REAL (dp), INTENT(IN)      :: f
REAL (dp), INTENT(IN)      :: f0
REAL (dp), INTENT(IN)      :: df
REAL (dp), INTENT(IN)      :: o0
REAL (dp), INTENT(IN OUT)  :: DO
INTEGER, INTENT(IN OUT)    :: npt
INTEGER, INTENT(IN OUT)    :: ipr
INTEGER, INTENT(IN OUT)    :: np

REAL (dp) :: zbr(50), zbi(50), fok, oj, tr1, tr2, tr3, tr4, tr5, tr6, tr7
INTEGER   :: i, kk
REAL (dp), PARAMETER :: zero = 0.0_dp, one = 1.0_dp, eight = 8.0_dp,  &
                        ts = 360._dp

oj = o0 - DO
fok = ts/(eight*ATAN(one))
WRITE(ipr, 901) f, x(1:np)
901 FORMAT(/////' ', g14.8, 3(/'    ', 5(g14.8, ' ')))
WRITE(ipr, 903)
903 FORMAT(//'  FREQ    REAL       IMAG       ABS       PHASE    DRE ',  &
             '   DIM   DABS    FREQ'/)
DO  kk=1,npt
  oj = oj + DO
  zbr(kk) = x(1) + (x(2)/(oj**x(3)))
  zbi(kk) = oj*x(4) - (x(5)/(oj**x(3)))
END DO
oj = f0 - df
DO  i=1,npt
  oj = oj + df
  tr1 = zbr(i)
  tr2 = zbi(i)
  tr3 = SQRT(tr1*tr1 + tr2*tr2)
  tr4 = ATAN2(tr2, tr1)*fok
  IF (zre(i) /= zero) tr5 = (zre(i)-tr1)/ABS(zre(i))
  IF (zre(i) == zero) tr5 = zre(i)-tr1
  IF (zim(i) /= zero) tr6 = (zim(i)-tr2)/ABS(zim(i))
  IF (zim(i) == zero) tr6 = zim(i)-tr2
  tr7 = SQRT(tr5*tr5 + tr6*tr6)
  WRITE(ipr, 904) oj, tr1, tr2, tr3, tr4, tr5, tr6, tr7, oj
  904 FORMAT(' ', f5.2, ' ', 3(' ', g10.4), 4(' ', f6.2), '   ', f5.2)
END DO
RETURN
END SUBROUTINE ladder


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! The file: INPUT

 ez itt a cimke helye
   0.0000   1.0000
   0.0000   1.0000
   0.0000   2.0000
   0.0000   1.0000
   0.0000   1.0000
 0  100  2  3
  0.025  5.00   -5.00
  0.050  3.00   -2.00
  0.075  2.00   -1.00
  0.100  1.50   -0.50
  0.125  1.20   -0.20
  0.150  1.10   -0.10

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! An output file follows:

  ez itt a cimke helye

    0.0000    1.0000
    0.0000    1.0000
    0.0000    2.0000
    0.0000    1.0000
    0.0000    1.0000


 RUN PARAMETERS

  0  100  2 3

 SAMPLE

  0.025    5.00    -5.00
  0.050    3.00    -2.00
  0.075    2.00    -1.00
  0.100    1.50    -0.50
  0.125    1.20    -0.20
  0.150    1.10    -0.10



 Random number seed(s): 
      87632     389707         32     643890      32987    4365908     243987
        689      43897     654987


   100 FUNCTION EVALUATIONS USED FOR SAMPLING
 *** THE LOCAL MINIMUM NO.  1: 0.67609502E-01, NFEV=  200
 0.67609502E-01
    0.51391532     0.55396685      1.1950652     0.56849758     0.53076047     
    
 SAMPLE POINT ADDED TO THE CLUSTER NO.  1
 0.21660555    
    0.45845671     0.51020943      1.3761022     0.59273621     0.57636474     
    

   100 FUNCTION EVALUATIONS USED FOR SAMPLING
 NEW SEED POINT ADDED TO THE CLUSTER NO.  1, NFEV=  163
 0.67609502E-01
    0.10385758     0.85375662      1.0035828     0.90094354     0.75231245     
    
 *** IMPROVEMENT ON THE LOCAL MINIMUM NO.  1:0.67609502E-01 FOR 0.67609502E-01
 0.67609502E-01
    0.51391543     0.55396676      1.1950652     0.56849753     0.53076042     
    
 SAMPLE POINT ADDED TO THE CLUSTER NO.  1
 0.16335736    
    0.14014181     0.93676996     0.78321739     0.85749589     0.73506345     
    
 NEW SEED POINT ADDED TO THE CLUSTER NO.  1, NFEV=  184
 0.67609502E-01
    0.71011390     0.62601696      1.0769925     0.95468350     0.69247156     
    
 *** IMPROVEMENT ON THE LOCAL MINIMUM NO.  1:0.67609502E-01 FOR 0.67609502E-01
 0.67609502E-01
    0.51391535     0.55396682      1.1950652     0.56849757     0.53076046     
    





 LOCAL MINIMA FOUND:


 0.67609502E-01
    0.51391535     0.55396682      1.1950652     0.56849757     0.53076046     
    



 NORMAL TERMINATION AFTER   747 FUNCTION EVALUATIONS








 0.67609502E-01
    0.51391535     0.55396682      1.1950652     0.56849757     0.53076046     
    


  FREQ    REAL       IMAG       ABS       PHASE    DRE    DIM   DABS    FREQ

  0.03   5.574     -4.759      7.329     -40.49  -0.11  -0.05   0.12    0.03
  0.05   2.724     -1.939      3.344     -35.44   0.09  -0.03   0.10    0.05
  0.08   1.875     -1.036      2.143     -28.93   0.06   0.04   0.07    0.08
  0.10   1.479     -.5677      1.584     -21.00   0.01   0.14   0.14    0.10
  0.13   1.253     -.2619      1.280     -11.80  -0.04   0.31   0.31    0.13
  0.15   1.109     -.3390E-01  1.109      -1.75  -0.01  -0.66   0.66    0.15


#endif

#if 0
  PROGRAM testfunktionen
! Testing the global optimization program of Tibor Csendes using
! the 30 test functions from:
!   http://solon.mat.univie.ac.at/~vpk/math/funcs.html

! ** This is not complete yet  **
! It takes a long while to program all 30 functions + data + starting values

USE global_minimum
IMPLICIT NONE
INTEGER, PARAMETER    :: dp = SELECTED_REAL_KIND(12, 60)

INTEGER, ALLOCATABLE  :: seed(:)       ! For the random number generator

INTEGER, PARAMETER    :: ipr = 9

REAL (dp)             :: amin(30), amax(30), x0(30,20), f00(20)
INTEGER               :: i, k, nparm, m, nsampl, ng, nsig, nc
CHARACTER (LEN=30)    :: heading

OPEN(UNIT=ipr, FILE='testfunc.out')

! Set the random number seed

CALL RANDOM_SEED(size=k)
ALLOCATE (seed(k))
WRITE(*, '(1x, a, i4, a)') 'Enter ', k, ' integers as random number seeds: '
READ(*, *) seed
CALL RANDOM_SEED(put=seed)
WRITE(ipr, '(a / (7i11) )') ' Random number seed(s): ', seed
WRITE(ipr, * )

DO m = 1, 30
  CALL start_values()
  WRITE(ipr, '(/" ", a)') 'Test problem: ' // heading
  nsampl = MIN(100*nparm, 1000)
  ng = 5
  nsig = 6
  CALL global(amin, amax, nparm, m, nsampl, ng, ipr, nsig, x0, nc, f00)
  DO i = 1, nc
    WRITE(ipr, '(a, g13.5)') ' Function value = ', f00(i)
    WRITE(ipr, '(a / (6g13.5))') ' Parameter values = ', x0(1:nparm, i)
  END DO
  WRITE(ipr, '(//)')
END DO

STOP


CONTAINS


SUBROUTINE start_values()

SELECT CASE ( m )
  CASE ( 1 )
    heading = 'Sphere'
    nparm = 3
    amin(1:nparm) = -5.12_dp
    amax(1:nparm) =  5.12_dp
  CASE ( 2 )
    heading = 'Rosenbrock'
    nparm = 10
    amin(1:nparm) = -2.048_dp
    amax(1:nparm) =  2.048_dp
  CASE ( 3 )
    heading = 'Step function'
    nparm = 5
    amin(1:nparm) = -5.12_dp
    amax(1:nparm) =  5.12_dp
  CASE ( 4 )
    heading = 'Hyper-Ellipsoid '
    nparm = 30
    amin(1:nparm) = -1.0_dp
    amax(1:nparm) =  1.0_dp
  CASE ( 5 )
    heading = 'Neumaier nr.3'
    nparm = 30
    amin(1:nparm) = -nparm**2
    amax(1:nparm) =  nparm**2
  CASE ( 6 )
    heading = 'Schaffer nr.2'
    nparm = 2
    amin(1:nparm) = -100._dp
    amax(1:nparm) =  100._dp
  CASE ( 7 )
    heading = 'Shekel'
    nparm = 10
    amin(1:nparm) =   0.0_dp
    amax(1:nparm) =  10.0_dp
  CASE ( 8 )
    heading = 'Hartman'
    nparm = 6
    amin(1:nparm) =  0.0_dp
    amax(1:nparm) =  1.0_dp
  CASE ( 9 )
    heading = 'Goldstein-Price'
    nparm = 2
    amin(1:nparm) =  0.0_dp
    amax(1:nparm) =  1.0_dp
  CASE ( 10 )
    heading = 'Branin'
    nparm = 2
    amin(1) = -5.0_dp
    amax(1) = 10.0_dp
    amin(2) =  0.0_dp
    amax(2) = 15.0_dp
  CASE ( 11 )
    heading = 'Six-hump camel back'
    nparm = 2
    amin(1:nparm) =  0.0_dp
    amax(1:nparm) =  1.0_dp
  CASE ( 12 )
    heading = 'Shubert'
    nparm = 2
    amin(1:nparm) = -10._dp
    amax(1:nparm) =  10._dp
  CASE ( 13 )
    heading = 'Shekels Foxholes'
    nparm = 2
    amin(1:nparm) = -65.536_dp
    amax(1:nparm) =  65.536_dp
  CASE ( 14 )
    heading = 'Modifizierte Shekels Foxholes'
    nparm = 10
    amin(1:nparm) =  0._dp
    amax(1:nparm) = 10._dp
  CASE ( 15 )
    heading = 'Coranas Parabel'
    nparm = 4
    amin(1:nparm) = -1000._dp
    amax(1:nparm) =  1000._dp
  CASE ( 16 )
    heading = 'Griewanke'
    nparm = 10
    amin(1:nparm) = -400._dp
    amax(1:nparm) =  400._dp
  CASE ( 17 )
    heading = 'Zimmermann'
    nparm = 2
    amin(1:nparm) =   0._dp
    amax(1:nparm) = 100._dp
  CASE ( 18 )
    heading = 'Katsuuras'
    nparm = 10
    amin(1:nparm) =    0._dp
    amax(1:nparm) = 1000._dp
  CASE ( 19 )
    heading = 'Rastringins'
    nparm = 20
    amin(1:nparm) = -600._dp
    amax(1:nparm) =  600._dp
  CASE ( 20 )
    heading = 'Ackleys'
    nparm = 30
    amin(1:nparm) = -30._dp
    amax(1:nparm) =  30._dp
  CASE ( 21 )
    heading = 'Schaffer nr.1'
    nparm = 2
    amin(1:nparm) = -100._dp
    amax(1:nparm) =  100._dp
  CASE ( 22 )
    heading = 'Easom'
    nparm = 2
    amin(1:nparm) = -100._dp
    amax(1:nparm) =  100._dp
  CASE ( 23 )
    heading = 'Bohachevsky nr.1'
    STOP
  CASE ( 24 )
    heading = 'Bohachevsky nr.2'
  CASE ( 25 )
    heading = 'Colville'
  CASE ( 26 )
    heading = 'Neumaier nr.2'
  CASE ( 27 )
    heading = 'Modifizierte Langerman'
  CASE ( 28 )
    heading = 'Epistatic Michalewicz'
  CASE ( 29 )
    heading = 'Odd Square'
  CASE ( 30 )
    heading = 'Chebychev-Polynom'
END SELECT

RETURN
END SUBROUTINE start_values

END PROGRAM testfunktionen



SUBROUTINE funct(x, value, nparm, m)

IMPLICIT NONE
INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(12, 60)

REAL (dp), INTENT(IN)   :: x(:)
REAL (dp), INTENT(OUT)  :: value
INTEGER, INTENT(IN)     :: nparm
INTEGER, INTENT(IN)     :: m          ! this is the function number

INTEGER   :: i, j
REAL (dp) :: temp, z
REAL (dp), PARAMETER  :: zero = 0.0_dp, one = 1.0_dp,  &
                         pi = 3.141592653589793846_dp

! Data for Shekel test function
REAL (dp), PARAMETER  :: c(10) = (/ 0.1_dp, 0.2_dp, 0.2_dp, 0.4_dp, 0.4_dp, &
                                    0.6_dp, 0.3_dp, 0.7_dp, 0.5_dp, 0.5_dp /)
REAL (dp), PARAMETER  :: a(10,4) = RESHAPE(  &
  (/ 4._dp, 1._dp, 8._dp, 6._dp, 3._dp, 2._dp, 5._dp, 8._dp, 6._dp, 7._dp, &
     4._dp, 1._dp, 8._dp, 6._dp, 7._dp, 9._dp, 5._dp, 1._dp, 2._dp, 3.6_dp, &
     4._dp, 1._dp, 8._dp, 6._dp, 3._dp, 2._dp, 3._dp, 8._dp, 6._dp, 7._dp, &
     4._dp, 1._dp, 8._dp, 6._dp, 7._dp, 9._dp, 3._dp, 1._dp, 2._dp, 3.6_dp /), &
     (/ 10, 4 /) )

! Data for Hartman test function
REAL (dp), PARAMETER  :: ha(6,4) = RESHAPE(  &
  (/ 10._dp,   3._dp, 17._dp, 3.5_dp, 1.7_dp, 8._dp,  &
     0.05_dp, 10._dp, 17._dp, 0.1_dp, 8._dp, 14._dp,  &
     3._dp,  3.5_dp, 1.7_dp,  10._dp, 17._dp, 8._dp,  &
     17._dp,  8._dp, 0.05_dp, 10._dp, 0.1_dp, 14._dp /), (/ 6, 4 /) )
REAL (dp), PARAMETER  :: hc(4) = (/ 1.0_dp, 1.2_dp, 3.0_dp, 3.2_dp /)
REAL (dp), PARAMETER  :: hp(6,4) = RESHAPE(  &
  (/ 0.1312_dp, 0.1696_dp, 0.5569_dp, 0.0124_dp, 0.8283_dp, 0.5886_dp,  &
     0.2329_dp, 0.4135_dp, 0.8307_dp, 0.3736_dp, 0.1004_dp, 0.9991_dp,  &
     0.2348_dp, 0.1451_dp, 0.3522_dp, 0.2883_dp, 0.3047_dp, 0.6650_dp,  &
     0.4047_dp, 0.8828_dp, 0.8732_dp, 0.5743_dp, 0.1091_dp, 0.0381_dp /), &
     (/ 6, 4 /) )

! Data for Shekels Foxholes
REAL (dp), PARAMETER  :: sh(25,2) = RESHAPE(  &
  (/ -32._dp, -16._dp, 0._dp, 16._dp, 32._dp,  &
     -32._dp, -16._dp, 0._dp, 16._dp, 32._dp,  &
     -32._dp, -16._dp, 0._dp, 16._dp, 32._dp,  &
     -32._dp, -16._dp, 0._dp, 16._dp, 32._dp,  &
     -32._dp, -16._dp, 0._dp, 16._dp, 32._dp,  &
     -32._dp, -32._dp, -32._dp, -32._dp, -32._dp,  &
     -16._dp, -16._dp, -16._dp, -16._dp, -16._dp,  &
       0._dp,   0._dp,   0._dp,   0._dp,   0._dp,  &
     +16._dp, +16._dp, +16._dp, +16._dp, +16._dp,  &
     +32._dp, +32._dp, +32._dp, +32._dp, +32._dp /), (/ 25, 2 /) )

! Data for Coranas Parabel
REAL (dp), PARAMETER  :: d(4) = (/ 1._dp, 1000._dp, 10._dp, 100._dp /)

SELECT CASE (m )
  CASE ( 1 )                 ! Sphere
    value = SUM( x(1:nparm)**2 )

  CASE ( 2 )                 ! Rosenbrock
    value = zero
    DO i = 1, nparm-1
      value = value + 100._dp*(x(i+1)**2 - x(i))**2 + (one - x(i))**2
    END DO

  CASE ( 3 )                 ! Step function
    value = SUM( FLOOR(x(1:nparm)) )

  CASE ( 4 )                 ! Hyper-Ellipsoid
    value = zero
    DO i = 1, nparm
      value = value + (i*x(i))**2
    END DO

  CASE ( 5 )                 ! Neumaier nr.3
    value = SUM( (x(1:nparm) - one)**2 )
    DO i = 2, nparm
      value = value - x(i)*x(i-1)
    END DO

  CASE ( 6 )                 ! Schaffer nr.2
    temp = x(1)**2 + x(2)**2
    value = temp**0.25 * (2500._dp*temp**0.2_dp + one)

  CASE ( 7 )                 ! Shekel
    value = zero
    DO i = 1, nparm
      value = value - one / (c(i) + SUM( (x(1:4) - a(i,1:4))**2 ))
    END DO

  CASE ( 8 )                 ! Hartman (n = 6)
    value = zero
    DO i = 1, 4
      value = value - hc(i) *   &
              EXP(-DOT_PRODUCT( ha(1:nparm,i), (x(1:nparm)-hp(1:nparm,i))**2 ))
    END DO

  CASE ( 9 )                 ! Goldstein-Price
    value = (one + (x(1)+x(2)+one)**2 *  &
                   (19-14*x(1)+3*x(1)**2-14*x(2)+6*x(1)*x(2)+3*x(2)**2)) *  &
            (30 + (2*x(1)-3*x(2))**2 *  &
                  (18-32*x(1)+12*x(1)**2+48*x(2)-36*x(1)*x(2)+27*x(2)**2))

  CASE ( 10 )                ! Branin
    value = (x(2) - 1.25*(x(1)/pi)**2 + 5*x(1)/pi - 6)**2 +  &
            10*(one - 0.125_dp/pi)*COS(x(1)) + 10._dp

  CASE ( 11 )                ! Six-hump camel back
    value = (4 - 2.1_dp*x(1)**2 + x(1)**4/3)*x(1)**2 + x(1)*x(2) +  &
            4*(x(2)**2 - one)*x(2)**2

  CASE ( 12 )                ! Shubert
    value = one
    DO i = 1, 2
      temp = zero
      DO j = 1, 5
        temp = temp + j*COS((j+1)*x(i) + j)
      END DO
      value = value * temp
    END DO

  CASE ( 13 )                ! Shekels Foxholes
    temp = zero
    DO i = 1, 25
      temp = temp + one / (i + (x(1)-sh(i,1))**6 + (x(2)-sh(i,2))**6)
    END DO
    value = one / (0.002_dp + temp)

  CASE ( 14 )                ! Modifizierte Shekels Foxholes
                             ! Using same pattern for A-matrix as for F7,
                             ! and 3 repeats of the c-vector.
    value = zero
    DO i = 1, 30
      temp = c( MOD(i-1,10) + 1 )
      DO j = 1, nparm
        temp = temp + (x(j) - a(j, MOD(i-1,4) + 1))**2
      END DO
      value = value - one / temp
    END DO

  CASE ( 15 )                ! Coranas Parabel
    value = zero
    DO i = 1, nparm
      z = FLOOR( ABS(5*x(i)) + 0.49999_dp )
      z = SIGN(z, x(i))*0.2_dp
      IF ( ABS(x(i)-z) < 0.05_dp ) THEN
        value = value + 0.15_dp * (z - SIGN(0.05_dp, z))**2 * d(i)
      ELSE
        value = value + d(i)*x(i)**2
      END IF
    END DO

  CASE ( 16 )                ! Griewanke
    value = one + SUM( x(1:nparm)**2 ) / 4000.
    temp = one
    DO i = 1, nparm
      temp = temp * COS(x(i) / SQRT(DBLE(i)))
    END DO
    value = value - temp

  CASE ( 17 )                ! Zimmermann
    value = MAX( h1(x), SIGN( p(h2(x)), h2(x) ), SIGN( p(h3(x)), h3(x) ),  &
                 SIGN( p(-x(1)), x(1) ), SIGN( p(-x(2)), x(2) ) )

  CASE ( 18 )                ! Katsuuras
    value = one
    DO i = 1, nparm
      temp = zero
      DO j = 1, 32
        temp = temp + abstoint(x(i), j)
      END DO
      value = value * (one + i*temp)
    END DO

  CASE ( 19 )                ! Rastringins
    value = 10*nparm
    DO i = 1, nparm
      value = value + x(i)**2 - 10*COS(2*pi*x(i))
    END DO

  CASE ( 20 )                ! Ackleys
    value = -20*EXP( -0.02_dp*SQRT( SUM( x(1:nparm)**2 ) / nparm ) ) -  &
            EXP( SUM( COS(2*pi*x(1:nparm)) ) / nparm ) + 20._dp

  CASE ( 21 )                ! Schaffer nr.1
    temp = x(1)**2 + x(2)**2
    value = 0.5 + ( SIN(SQRT(temp))**2 - 0.5 ) / (one + 0.001_dp*temp)**2

  CASE ( 22 )                ! Easom
    value = -COS(x(1)) * COS(x(2)) * EXP( -(x(1)-pi)**2 - (x(2)-pi)**2 )

  CASE ( 23 )                ! Bohachevsky nr.1

  CASE ( 24 )                ! Bohachevsky nr.2

  CASE ( 25 )                ! Colville

  CASE ( 26 )                ! Neumaier nr.2

  CASE ( 27 )                ! Modifizierte Langerman

  CASE ( 28 )                ! Epistatic Michalewicz

  CASE ( 29 )                ! Odd Square

  CASE ( 30 )                ! Chebychev-Polynom
END SELECT

RETURN


CONTAINS


FUNCTION h1(x) RESULT(fn_val)
! Function required for Zimmermann's Problem (F17)

REAL (dp), INTENT(IN)  :: x(:)
REAL (dp)              :: fn_val

fn_val = 9._dp - x(1) - x(2)

RETURN
END FUNCTION h1



FUNCTION h2(x) RESULT(fn_val)
! Function required for Zimmermann's Problem (F17)

REAL (dp), INTENT(IN)  :: x(:)
REAL (dp)              :: fn_val

fn_val = (x(1) - 3._dp)**2 + (x(2) - 2._dp)**2 - 16._dp

RETURN
END FUNCTION h2



FUNCTION h3(x) RESULT(fn_val)
! Function required for Zimmermann's Problem (F17)

REAL (dp), INTENT(IN)  :: x(:)
REAL (dp)              :: fn_val

fn_val = x(1)*x(2) - 14._dp

RETURN
END FUNCTION h3



FUNCTION p(delta) RESULT(fn_val)
! Function required for Zimmermann's Problem (F17)

REAL (dp), INTENT(IN)  :: delta
REAL (dp)              :: fn_val

fn_val = 100*(one + delta)

RETURN
END FUNCTION p



FUNCTION abstoint(x, j) RESULT(fn_val)

REAL (dp), INTENT(IN)  :: x
INTEGER, INTENT(IN)    :: j
REAL (dp)              :: fn_val

INTEGER, PARAMETER  :: large = HUGE(1)

fn_val = INT( MIN( SCALE(x, j), DBLE(large) ) )
fn_val = SCALE(fn_val, -j)

RETURN
END FUNCTION abstoint

END SUBROUTINE funct

#endif
