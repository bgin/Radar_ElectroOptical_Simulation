MODULE cobyla2

! The Fortran 77 version of this code was by Michael Powell
! (M.J.D.Powell @ damtp.cam.ac.uk)

! This Fortran 90 version by Alan Miller
! This version is in a subset of F90 and is compatible with version 3.0a
! of Lahey's free ELF90 compiler.
! Alan.Miller @ vic.cmis.csiro.au
! Latest revision - 28 June 2000
! Slightly adapted to better suit 'Missile Simulation' project
! Mainly ifort and OMP directives were added
! Bernard Gingold 08-05-2022, beniekg@gmail.com
use mod_kinds, only : i4,dp
IMPLICIT NONE


CONTAINS

!------------------------------------------------------------------------

SUBROUTINE cobyla (n, m, x, rhobeg, rhoend, iprint, maxfun)
      !dir$ attributes code_align : 32 :: cobyla
      !dir$ optimize : 32
      
INTEGER(i4), INTENT(IN)        :: n
INTEGER(i4), INTENT(IN)        :: m
REAL (dp), INTENT(IN OUT)  :: x(:)
REAL (dp), INTENT(IN OUT)  :: rhobeg
REAL (dp), INTENT(IN OUT)  :: rhoend
INTEGER(i4), INTENT(IN)        :: iprint
INTEGER(i4), INTENT(IN OUT)    :: maxfun

!  This subroutine minimizes an objective function F(X) subject to M
!  inequality constraints on X, where X is a vector of variables that has
!  N components.  The algorithm employs linear approximations to the
!  objective and constraint functions, the approximations being formed by
!  linear interpolation at N+1 points in the space of the variables.
!  We regard these interpolation points as vertices of a simplex.  The
!  parameter RHO controls the size of the simplex and it is reduced
!  automatically from RHOBEG to RHOEND.  For each RHO the subroutine tries
!  to achieve a good vector of variables for the current size, and then
!  RHO is reduced until the value RHOEND is reached.  Therefore RHOBEG and
!  RHOEND should be set to reasonable initial changes to and the required
!  accuracy in the variables respectively, but this accuracy should be
!  viewed as a subject for experimentation because it is not guaranteed.
!  The subroutine has an advantage over many of its competitors, however,
!  which is that it treats each constraint individually when calculating
!  a change to the variables, instead of lumping the constraints together
!  into a single penalty function.  The name of the subroutine is derived
!  from the phrase Constrained Optimization BY Linear Approximations.

!  The user must set the values of N, M, RHOBEG and RHOEND, and must
!  provide an initial vector of variables in X.  Further, the value of
!  IPRINT should be set to 0, 1, 2 or 3, which controls the amount of
!  printing during the calculation. Specifically, there is no output if
!  IPRINT=0 and there is output only at the end of the calculation if
!  IPRINT=1.  Otherwise each new value of RHO and SIGMA is printed.
!  Further, the vector of variables and some function information are
!  given either when RHO is reduced or when each new value of F(X) is
!  computed in the cases IPRINT=2 or IPRINT=3 respectively. Here SIGMA
!  is a penalty parameter, it being assumed that a change to X is an
!  improvement if it reduces the merit function
!             F(X)+SIGMA*MAX(0.0, - C1(X), - C2(X),..., - CM(X)),
!  where C1,C2,...,CM denote the constraint functions that should become
!  nonnegative eventually, at least to the precision of RHOEND. In the
!  printed output the displayed term that is multiplied by SIGMA is
!  called MAXCV, which stands for 'MAXimum Constraint Violation'.  The
!  argument MAXFUN is an integer variable that must be set by the user to a
!  limit on the number of calls of CALCFC, the purpose of this routine being
!  given below.  The value of MAXFUN will be altered to the number of calls
!  of CALCFC that are made.

!  In order to define the objective and constraint functions, we require
!  a subroutine that has the name and arguments
!             SUBROUTINE CALCFC (N,M,X,F,CON)
!             DIMENSION X(:),CON(:)  .
!  The values of N and M are fixed and have been defined already, while
!  X is now the current vector of variables. The subroutine should return
!  the objective and constraint functions at X in F and CON(1),CON(2),
!  ...,CON(M).  Note that we are trying to adjust X so that F(X) is as
!  small as possible subject to the constraint functions being nonnegative.

!  N.B. If the starting value for any x(i) is set to zero, that value will
!       not be changed.   This can be a useful feature in comparing
!       nested models.   If all the x(i)'s are set to zero, an error message
!       will result.

! Local variable

INTEGER(i4) :: mpp

mpp = m + 2
CALL cobylb (n, m, mpp, x, rhobeg, rhoend, iprint, maxfun)

RETURN
END SUBROUTINE cobyla


!------------------------------------------------------------------------------

SUBROUTINE cobylb (n, m, mpp, x, rhobeg, rhoend, iprint, maxfun)
      !dir$ attributes code_align : 32 :: cobylb
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=skylake_avx512 :: cobylb
      use omp_lib
! N.B. Arguments CON, SIM, SIMI, DATMAT, A, VSIG, VETA, SIGBAR, DX, W & IACT
!   have been removed.

INTEGER(i4), INTENT(IN)        :: n
INTEGER(i4), INTENT(IN)        :: m
INTEGER(i4), INTENT(IN)        :: mpp
REAL (dp), INTENT(IN OUT)  :: x(:)
REAL (dp), INTENT(IN)      :: rhobeg
REAL (dp), INTENT(IN)      :: rhoend
INTEGER(i4), INTENT(IN)        :: iprint
INTEGER(i4), INTENT(OUT)       :: maxfun

INTERFACE
  SUBROUTINE calcfc (n, m, x, f, con)
    import :: i4,dp
    IMPLICIT NONE
    INTEGER(i4), INTENT(IN)    :: n, m
    REAL (dp), INTENT(IN)  :: x(:)
    REAL (dp), INTENT(OUT) :: f
    REAL (dp), INTENT(OUT) :: con(:)
  END SUBROUTINE calcfc
END INTERFACE

!  Set the initial values of some parameters. The last column of SIM holds
!  the optimal vertex of the current simplex, and the preceding N columns
!  hold the displacements from the optimal vertex to the other vertices.
!  Further, SIMI holds the inverse of the matrix that is contained in the
!  first N columns of SIM.

! Local variables

REAL (dp) :: con(mpp), sim(n,n+1), simi(n,n), datmat(mpp,n+1), a(n,m+1),      &
             vsig(n), veta(n), sigbar(n), dx(n), w(n)
!dir$ attribute align : 64 :: con
!dir$ attribute align : 64 :: sim
!dir$ attribute align : 64 :: simi
!dir$ attribute align : 64 :: datmat
!dir$ attribute align : 64 :: a
!dir$ attribute align : 64 :: vsig
!dir$ attribute align : 64 :: veta
!dir$ attribute align : 64 :: sigbar
!dir$ attribute align : 64 :: dx
!dir$ attribute align : 64 :: w
REAL (dp) :: alpha, barmu, beta, cmin, cmax, cvmaxm, cvmaxp, delta, denom,    &
             dxsign, edgmax, error, f, gamma, pareta, parmu, parsig, phi,     &
             phimin, prerec, prerem, ratio, resmax, resnew, rho, temp, tempa, &
             total, trured, vmnew, vmold, weta, wsig
INTEGER(i4)   :: i, ibrnch, iflag, ifull, iptem, iptemp, j, jdrop, k, l, mp,  &
             nbest, nfvals, np

iptem = MIN(n,5)
iptemp = iptem + 1
np = n + 1
mp = m + 1
alpha = 0.25_dp
beta = 2.1_dp
gamma = 0.5_dp
delta = 1.1_dp
rho = rhobeg
parmu = 0.0_dp
IF (iprint >= 2) WRITE(*, 10) rho
10 FORMAT (/'   The initial value of RHO is', G13.6,   &
           '  and PARMU is set to zero.')
nfvals = 0
temp = 1.0_dp/rho
!dir$ assume_aligned sim:64
!dir$ assume_aligned simi:64
DO i=1,n
  sim(i,np) = x(i)
  !$omp simd simdlen(8) linear(j:1)
  DO j=1,n
    sim(i,j) = 0.0_dp
    simi(i,j) = 0.0_dp
  END DO
  sim(i,i) = rho
  simi(i,i) = temp
END DO
jdrop = np
ibrnch = 0

!  Make the next call of the user-supplied subroutine CALCFC. These
!  instructions are also used for calling CALCFC during the iterations of
!  the algorithm.

40 IF (nfvals >= maxfun .AND. nfvals > 0) THEN
  IF (iprint >= 1) WRITE(*, 50)
  50 FORMAT (/'   Return from subroutine COBYLA because the ',  &
             'MAXFUN limit has been reached.')
  GO TO 600
END IF
nfvals = nfvals + 1
CALL calcfc (n, m, x, f, con)
resmax = 0.0_dp
IF (m > 0) THEN
  DO k=1,m
    resmax = MAX(resmax, - con(k))
  END DO
END IF
IF (nfvals == iprint-1 .OR. iprint == 3) THEN
  WRITE(*, 70) nfvals, f, resmax, x(1:iptem)
  70 FORMAT (/'   NFVALS = ', i5, '   F = ', G13.6, '    MAXCV = ',  &
             G13.6/ ('   X = ', 5G14.6))
  IF (iptem < n) WRITE(*, 80) x(iptemp:n)
  80 FORMAT (G19.6, G15.6)
END IF
con(mp) = f
con(mpp) = resmax
IF (ibrnch == 1) GO TO 440

!  Set the recently calculated function values in a column of DATMAT. This
!  array has a column for each vertex of the current simplex, the entries of
!  each column being the values of the constraint functions (if any)
!  followed by the objective function and the greatest constraint violation
!  at the vertex.

DO k=1,mpp
  datmat(k,jdrop) = con(k)
END DO
IF (nfvals > np) GO TO 130

!  Exchange the new vertex of the initial simplex with the optimal vertex if
!  necessary. Then, if the initial simplex is not complete, pick its next
!  vertex and calculate the function values there.

IF (jdrop <= n) THEN
  IF (datmat(mp,np) <= f) THEN
    x(jdrop) = sim(jdrop,np)
  ELSE
    sim(jdrop,np) = x(jdrop)
    !$omp simd simdlen(8) linear(k:1)
    DO k=1,mpp
      datmat(k,jdrop) = datmat(k,np)
      datmat(k,np) = con(k)
    END DO
    DO k=1,jdrop
      sim(jdrop,k) = -rho
      temp = -SUM( simi(k:jdrop, k) )
      simi(jdrop,k) = temp
    END DO
  END IF
END IF
IF (nfvals <= n) THEN
  jdrop = nfvals
  x(jdrop) = x(jdrop) + rho
  GO TO 40
END IF
130 ibrnch = 1

!  Identify the optimal vertex of the current simplex.

140 phimin = datmat(mp,np) + parmu*datmat(mpp,np)
nbest = np
DO j=1,n
  temp = datmat(mp,j) + parmu*datmat(mpp,j)
  IF (temp < phimin) THEN
    nbest = j
    phimin = temp
  ELSE IF (temp == phimin .AND. parmu == 0.0_dp) THEN
    IF (datmat(mpp,j) < datmat(mpp,nbest)) nbest = j
  END IF
END DO

!  Switch the best vertex into pole position if it is not there already,
!  and also update SIM, SIMI and DATMAT.

IF (nbest <= n) THEN
  DO i=1,mpp
    temp = datmat(i,np)
    datmat(i,np) = datmat(i,nbest)
    datmat(i,nbest) = temp
  END DO
  DO i=1,n
    temp = sim(i,nbest)
    sim(i,nbest) = 0.0_dp
    sim(i,np) = sim(i,np) + temp
    tempa = 0.0_dp
    !dir$ assume_aligned sim:64
    !dir$ assume_aligned simi:64
    !$omp simd simdlen(8) private(temp)
    DO k=1,n
      sim(i,k) = sim(i,k) - temp
      tempa = tempa - simi(k,i)
    END DO
    simi(nbest,i) = tempa
  END DO
END IF

!  Make an error return if SIGI is a poor approximation to the inverse of
!  the leading N by N submatrix of SIG.

error = 0.0_dp
DO i=1,n
  !dir$ assume_aligned simi:64
  !dir$ assume_aligned sim:64
  !$omp simd simdlen(8) reduction(+:temp) private(error)
  DO j=1,n
    temp = 0.0_dp
    IF (i == j) temp = temp - 1.0_dp
    temp = temp + DOT_PRODUCT( simi(i,1:n), sim(1:n,j) )
    error = MAX(error, ABS(temp))
  END DO
END DO
IF (error > 0.1_dp) THEN
  IF (iprint >= 1) WRITE(*, 210)
  210 FORMAT (/'   Return from subroutine COBYLA because ',  &
              'rounding errors are becoming damaging.')
  GO TO 600
END IF

!  Calculate the coefficients of the linear approximations to the objective
!  and constraint functions, placing minus the objective function gradient
!  after the constraint gradients in the array A. The vector W is used for
!  working space.
!dir$ assume_aligned con:64
!dir$ assume_aligned w:64
!dir$ assume_aligned datmat:64
!dir$ assume_aligned simi:64
!dir$ assume_aligned a:64
DO k=1,mp
  con(k) = -datmat(k,np)
  !$omp simd simdlen(8)
  DO j=1,n
    w(j) = datmat(k,j) + con(k)
  END DO
  !$omp simd simdlen(8) private(temp)
  DO i=1,n
    temp = DOT_PRODUCT( w(1:n), simi(1:n,i) )
    IF (k == mp) temp = -temp
    a(i,k) = temp
  END DO
END DO

!  Calculate the values of sigma and eta, and set IFLAG = 0 if the current
!  simplex is not acceptable.

iflag = 1
parsig = alpha*rho
pareta = beta*rho
!dir$ assume_aligned simi:64
!dir$ assume_aligned sim:64
!dir$ assume_aligned vsig:64
!dir$ assume_aligned veta:64
!$omp simd simdlen(8) private(wsig,weta)
DO j=1,n
  wsig = SUM( simi(j,1:n)**2 )
  weta = SUM( sim(1:n,j)**2 )
  vsig(j) = 1.0_dp/SQRT(wsig)
  veta(j) = SQRT(weta)
  IF (vsig(j) < parsig .OR. veta(j) > pareta) iflag = 0
END DO

!  If a new vertex is needed to improve acceptability, then decide which
!  vertex to drop from the simplex.

IF (ibrnch == 1 .OR. iflag == 1) GO TO 370
jdrop = 0
temp = pareta
DO j=1,n
  IF (veta(j) > temp) THEN
    jdrop = j
    temp = veta(j)
  END IF
END DO
IF (jdrop == 0) THEN
  DO j=1,n
    IF (vsig(j) < temp) THEN
      jdrop = j
      temp = vsig(j)
    END IF
  END DO
END IF

!  Calculate the step to the new vertex and its sign.

temp = gamma*rho*vsig(jdrop)
dx(1:n) = temp*simi(jdrop,1:n)
cvmaxp = 0.0_dp
cvmaxm = 0.0_dp
!dir$ assume_aligned a:64
!dir$ assume_aligned dx:64
!dir$ assume_aligned datmat:64
!$omp simd simdlen(8) private(total,temp,cvmaxp,cvmaxm)
DO k=1,mp
  total = DOT_PRODUCT( a(1:n,k), dx(1:n) )
  IF (k < mp) THEN
    temp = datmat(k,np)
    cvmaxp = MAX(cvmaxp, -total - temp)
    cvmaxm = MAX(cvmaxm, total - temp)
  END IF
END DO
dxsign = 1.0_dp
IF (parmu*(cvmaxp - cvmaxm) > total + total) dxsign = -1.0_dp

!  Update the elements of SIM and SIMI, and set the next X.

temp = 0.0_dp
!dir$ assume_aligned dx:64
!dir$ assume_aligned sim:64
!dir$ assume_aligned simi:64
!$omp simd simdlen(8) reduction(+:temp)
DO i=1,n
  dx(i) = dxsign*dx(i)
  sim(i,jdrop) = dx(i)
  temp = temp + simi(jdrop,i)*dx(i)
END DO
simi(jdrop,1:n) = simi(jdrop,1:n) / temp
!dir$ assume_aligned simi:64
!dir$ assume_aligned dx:64
!dir$ assume_aligned x:64
!dir$ assume_aligned sim:64
!$omp simd simdlen(8) reduction(-:simi)
DO j=1,n
  IF (j /= jdrop) THEN
    temp = DOT_PRODUCT( simi(j,1:n), dx(1:n) )
    simi(j,1:n) = simi(j,1:n) - temp*simi(jdrop,1:n)
  END IF
  x(j) = sim(j,np) + dx(j)
END DO
GO TO 40

!  Calculate DX = x(*)-x(0).
!  Branch if the length of DX is less than 0.5*RHO.

370 CALL trstlp (n, m, a, con, rho, dx, ifull)
IF (ifull == 0) THEN
  temp = SUM( dx(1:n)**2 )
  IF (temp < 0.25_dp*rho*rho) THEN
    ibrnch = 1
    GO TO 550
  END IF
END IF

!  Predict the change to F and the new maximum constraint violation if the
!  variables are altered from x(0) to x(0) + DX.

resnew = 0.0_dp
con(mp) = 0.0_dp
DO k=1,mp
  total = con(k) - DOT_PRODUCT( a(1:n,k), dx(1:n) )
  IF (k < mp) resnew = MAX(resnew, total)
END DO

!  Increase PARMU if necessary and branch back if this change alters the
!  optimal vertex. Otherwise PREREM and PREREC will be set to the predicted
!  reductions in the merit function and the maximum constraint violation
!  respectively.

barmu = 0.0_dp
prerec = datmat(mpp,np) - resnew
IF (prerec > 0.0_dp) barmu = total/prerec
IF (parmu < 1.5_dp*barmu) THEN
  parmu = 2.0_dp*barmu
  IF (iprint >= 2) WRITE(*, 410) parmu
  410 FORMAT (/'   Increase in PARMU to', G13.6)
  phi = datmat(mp,np) + parmu*datmat(mpp,np)
  DO j=1,n
    temp = datmat(mp,j) + parmu*datmat(mpp,j)
    IF (temp < phi) GO TO 140
    IF (temp == phi .AND. parmu == 0.0) THEN
      IF (datmat(mpp,j) < datmat(mpp,np)) GO TO 140
    END IF
  END DO
END IF
prerem = parmu*prerec - total

!  Calculate the constraint and objective functions at x(*).
!  Then find the actual reduction in the merit function.

x(1:n) = sim(1:n,np) + dx(1:n)
ibrnch = 1
GO TO 40

440 vmold = datmat(mp,np) + parmu*datmat(mpp,np)
vmnew = f + parmu*resmax
trured = vmold - vmnew
IF (parmu == 0.0_dp .AND. f == datmat(mp,np)) THEN
  prerem = prerec
  trured = datmat(mpp,np) - resmax
END IF

!  Begin the operations that decide whether x(*) should replace one of the
!  vertices of the current simplex, the change being mandatory if TRURED is
!  positive. Firstly, JDROP is set to the index of the vertex that is to be
!  replaced.

ratio = 0.0_dp
IF (trured <= 0.0) ratio = 1.0
jdrop = 0
DO j=1,n
  temp = DOT_PRODUCT( simi(j,1:n), dx(1:n) )
  temp = ABS(temp)
  IF (temp > ratio) THEN
    jdrop = j
    ratio = temp
  END IF
  sigbar(j) = temp*vsig(j)
END DO

!  Calculate the value of ell.

edgmax = delta*rho
l = 0
DO j=1,n
  IF (sigbar(j) >= parsig .OR. sigbar(j) >= vsig(j)) THEN
    temp = veta(j)
    IF (trured > 0.0_dp) THEN
      temp = SUM( (dx(1:n) - sim(1:n,j))**2 )
      temp = SQRT(temp)
    END IF
    IF (temp > edgmax) THEN
      l = j
      edgmax = temp
    END IF
  END IF
END DO
IF (l > 0) jdrop = l
IF (jdrop == 0) GO TO 550

!  Revise the simplex by updating the elements of SIM, SIMI and DATMAT.

temp = 0.0_dp
!dir$ assume_aligned sim:64
!dir$ assume_aligned dx:64
!dir$ assume_aligned simi
!$omp simd simdlen(8) reduction(+:temp)
DO i=1,n
  sim(i,jdrop) = dx(i)
  temp = temp + simi(jdrop,i)*dx(i)
END DO
simi(jdrop,1:n) = simi(jdrop,1:n) / temp
DO j=1,n
  IF (j /= jdrop) THEN
    temp = DOT_PRODUCT( simi(j,1:n), dx(1:n) )
    simi(j,1:n) = simi(j,1:n) - temp*simi(jdrop,1:n)
  END IF
END DO
datmat(1:mpp,jdrop) = con(1:mpp)

!  Branch back for further iterations with the current RHO.

IF (trured > 0.0_dp .AND. trured >= 0.1_dp*prerem) GO TO 140
550 IF (iflag == 0) THEN
  ibrnch = 0
  GO TO 140
END IF

!  Otherwise reduce RHO if it is not at its least value and reset PARMU.

IF (rho > rhoend) THEN
  rho = 0.5_dp*rho
  IF (rho <= 1.5_dp*rhoend) rho = rhoend
  IF (parmu > 0.0_dp) THEN
    denom = 0.0_dp
    DO k=1,mp
      cmin = datmat(k,np)
      cmax = cmin
      DO i=1,n
        cmin = MIN(cmin, datmat(k,i))
        cmax = MAX(cmax, datmat(k,i))
      END DO
      IF (k <= m .AND. cmin < 0.5_dp*cmax) THEN
        temp = MAX(cmax,0.0_dp) - cmin
        IF (denom <= 0.0_dp) THEN
          denom = temp
        ELSE
          denom = MIN(denom,temp)
        END IF
      END IF
    END DO
    IF (denom == 0.0_dp) THEN
      parmu = 0.0_dp
    ELSE IF (cmax - cmin < parmu*denom) THEN
      parmu = (cmax - cmin)/denom
    END IF
  END IF
  IF (iprint >= 2) WRITE(*, 580) rho,parmu
  580 FORMAT (/'   Reduction in RHO to ', G13.6, '  and PARMU = ', G13.6)
  IF (iprint == 2) THEN
    WRITE(*, 70) nfvals, datmat(mp,np), datmat(mpp,np), sim(1:iptem,np)
    IF (iptem < n) WRITE(*, 80) x(iptemp:n)
  END IF
  GO TO 140
END IF

!  Return the best calculated values of the variables.

IF (iprint >= 1) WRITE(*, 590)
590 FORMAT (/'   Normal return from subroutine COBYLA')
IF (ifull == 1) GO TO 620

600 x(1:n) = sim(1:n,np)
f = datmat(mp,np)
resmax = datmat(mpp,np)
620 IF (iprint >= 1) THEN
  WRITE(*, 70) nfvals, f, resmax, x(1:iptem)
  IF (iptem < n) WRITE(*, 80) x(iptemp:n)
END IF
maxfun = nfvals

RETURN
END SUBROUTINE cobylb
!------------------------------------------------------------------------------

SUBROUTINE trstlp (n, m, a, b, rho, dx, ifull)
      !dir$ attributes code_align : 32 :: trstlp
      !dir$ optimize : 3
      !dir$ attributes optimization_parameter:TARGET_ARCH=skylake_avx512 :: trstlp
      use omp_lib
! N.B. Arguments Z, ZDOTA, VMULTC, SDIRN, DXNEW, VMULTD & IACT have been removed.

INTEGER(i4), INTENT(IN)     :: n
INTEGER(i4), INTENT(IN)     :: m
REAL (dp), INTENT(IN)   :: a(:,:)
REAL (dp), INTENT(IN)   :: b(:)
REAL (dp), INTENT(IN)   :: rho
REAL (dp), INTENT(OUT)  :: dx(:)
INTEGER(i4), INTENT(OUT)    :: ifull

!  This subroutine calculates an N-component vector DX by applying the
!  following two stages. In the first stage, DX is set to the shortest
!  vector that minimizes the greatest violation of the constraints
!    A(1,K)*DX(1)+A(2,K)*DX(2)+...+A(N,K)*DX(N) .GE. B(K), K = 2,3,...,M,
!  subject to the Euclidean length of DX being at most RHO. If its length is
!  strictly less than RHO, then we use the resultant freedom in DX to
!  minimize the objective function
!           -A(1,M+1)*DX(1) - A(2,M+1)*DX(2) - ... - A(N,M+1)*DX(N)
!  subject to no increase in any greatest constraint violation. This
!  notation allows the gradient of the objective function to be regarded as
!  the gradient of a constraint. Therefore the two stages are distinguished
!  by MCON .EQ. M and MCON .GT. M respectively. It is possible that a
!  degeneracy may prevent DX from attaining the target length RHO. Then the
!  value IFULL = 0 would be set, but usually IFULL = 1 on return.

!  In general NACT is the number of constraints in the active set and
!  IACT(1),...,IACT(NACT) are their indices, while the remainder of IACT
!  contains a permutation of the remaining constraint indices.  Further, Z
!  is an orthogonal matrix whose first NACT columns can be regarded as the
!  result of Gram-Schmidt applied to the active constraint gradients.  For
!  J = 1,2,...,NACT, the number ZDOTA(J) is the scalar product of the J-th
!  column of Z with the gradient of the J-th active constraint.  DX is the
!  current vector of variables and here the residuals of the active
!  constraints should be zero. Further, the active constraints have
!  nonnegative Lagrange multipliers that are held at the beginning of
!  VMULTC. The remainder of this vector holds the residuals of the inactive
!  constraints at DX, the ordering of the components of VMULTC being in
!  agreement with the permutation of the indices of the constraints that is
!  in IACT. All these residuals are nonnegative, which is achieved by the
!  shift RESMAX that makes the least residual zero.

!  Initialize Z and some other variables. The value of RESMAX will be
!  appropriate to DX = 0, while ICON will be the index of a most violated
!  constraint if RESMAX is positive. Usually during the first stage the
!  vector SDIRN gives a search direction that reduces all the active
!  constraint violations by one simultaneously.

! Local variables

REAL (dp) :: z(n,n), zdota(m+1), vmultc(m+1), sdirn(n), dxnew(n), vmultd(m+1)
!dir$ attribute align : 64 :: z
!dir$ attribute align : 64 :: zdota
!dir$ attribute align : 64 :: vmultc
!dir$ attribute align : 64 :: sdirn
!dir$ attribute align : 64 :: dxnew
!dir$ attribute align : 64 :: vmultd
REAL (dp) :: acca, accb, alpha, beta, dd, optnew, optold, ratio, resmax,   &
             resold, sd, sp, spabs, ss, step, stpful, sumabs, temp, tempa, &
             tot, total, vsave, zdotv, zdotw, zdvabs, zdwabs
INTEGER(i4)   :: i, iact(m+1), icon, icount, isave, k, kk, kl, kp, kw, mcon,   &
             nact, nactx

ifull = 1
mcon = m
nact = 0
resmax = 0.0_dp
DO i=1,n
  z(i,1:n) = 0.0_dp
  z(i,i) = 1.0_dp
  dx(i) = 0.0_dp
END DO
IF (m >= 1) THEN
  DO k=1,m
    IF (b(k) > resmax) THEN
      resmax = b(k)
      icon = k
    END IF
  END DO
  !dir$ assume_aligned iact:64
  !dir$ assume_aligned vmultc:64
  !dir$ assume_aligned b:64
  !$omp simd simdlen(8) linear(k:1)
  DO k=1,m
    iact(k) = k
    vmultc(k) = resmax - b(k)
  END DO
END IF
IF (resmax == 0.0_dp) GO TO 480
sdirn(1:n) = 0.0_dp

!  End the current stage of the calculation if 3 consecutive iterations
!  have either failed to reduce the best calculated value of the objective
!  function or to increase the number of active constraints since the best
!  value was calculated. This strategy prevents cycling, but there is a
!  remote possibility that it will cause premature termination.

60 optold = 0.0_dp
icount = 0
70 IF (mcon == m) THEN
  optnew = resmax
ELSE
  optnew = - DOT_PRODUCT( dx(1:n), a(1:n,mcon) )
END IF
IF (icount == 0 .OR. optnew < optold) THEN
  optold = optnew
  nactx = nact
  icount = 3
ELSE IF (nact > nactx) THEN
  nactx = nact
  icount = 3
ELSE
  icount = icount - 1
  IF (icount == 0) GO TO 490
END IF

!  If ICON exceeds NACT, then we add the constraint with index IACT(ICON) to
!  the active set. Apply Givens rotations so that the last N-NACT-1 columns
!  of Z are orthogonal to the gradient of the new constraint, a scalar
!  product being set to zero if its nonzero value could be due to computer
!  rounding errors. The array DXNEW is used for working space.

IF (icon <= nact) GO TO 260
kk = iact(icon)
dxnew(1:n) = a(1:n,kk)
tot = 0.0_dp
k = n
100 IF (k > nact) THEN
  sp = 0.0_dp
  spabs = 0.0_dp
  !dir$ assume_aligned z:64
  !dir$ assume_aligned dxnew:64
  !$omp simd simdlen(8) reduction(+:sp) reduction(+:spabs) private(temp)
  DO i=1,n
    temp = z(i,k)*dxnew(i)
    sp = sp + temp
    spabs = spabs + ABS(temp)
  END DO
  acca = spabs + 0.1_dp*ABS(sp)
  accb = spabs + 0.2_dp*ABS(sp)
  IF (spabs >= acca .OR. acca >= accb) sp = 0.0_dp
  IF (tot == 0.0_dp) THEN
    tot = sp
  ELSE
    kp = k + 1
    temp = SQRT(sp*sp + tot*tot)
    alpha = sp/temp
    beta = tot/temp
    tot = temp
    !dir$ assume_aligned z:64
    !$omp simd simdlen(8) private(temp)
    DO i=1,n
      temp = alpha*z(i,k) + beta*z(i,kp)
      z(i,kp) = alpha*z(i,kp) - beta*z(i,k)
      z(i,k) = temp
    END DO
  END IF
  k = k - 1
  GO TO 100
END IF

!  Add the new constraint if this can be done without a deletion from the
!  active set.

IF (tot /= 0.0_dp) THEN
  nact = nact + 1
  zdota(nact) = tot
  vmultc(icon) = vmultc(nact)
  vmultc(nact) = 0.0_dp
  GO TO 210
END IF

!  The next instruction is reached if a deletion has to be made from the
!  active set in order to make room for the new active constraint, because
!  the new constraint gradient is a linear combination of the gradients of
!  the old active constraints.  Set the elements of VMULTD to the multipliers
!  of the linear combination.  Further, set IOUT to the index of the
!  constraint to be deleted, but branch if no suitable index can be found.

ratio = -1.0_dp
k = nact
130 zdotv = 0.0_dp
zdvabs = 0.0_dp
!dir$ assume_aligned z:64
!dir$ assume_aligned dxnew:64
!$omp simd simdlen(8) reduction(+:zdotv) reduction(+:zdvabs) private(temp)
DO i=1,n
  temp = z(i,k)*dxnew(i)
  zdotv = zdotv + temp
  zdvabs = zdvabs + ABS(temp)
END DO
acca = zdvabs + 0.1_dp*ABS(zdotv)
accb = zdvabs + 0.2_dp*ABS(zdotv)
IF (zdvabs < acca .AND. acca < accb) THEN
  temp = zdotv/zdota(k)
  IF (temp > 0.0_dp .AND. iact(k) <= m) THEN
    tempa = vmultc(k)/temp
    IF (ratio < 0.0_dp .OR. tempa < ratio) THEN
      ratio = tempa
    END IF
  END IF
  IF (k >= 2) THEN
    kw = iact(k)
    dxnew(1:n) = dxnew(1:n) - temp*a(1:n,kw)
  END IF
  vmultd(k) = temp
ELSE
  vmultd(k) = 0.0_dp
END IF
k = k - 1
IF (k > 0) GO TO 130
IF (ratio < 0.0_dp) GO TO 490

!  Revise the Lagrange multipliers and reorder the active constraints so
!  that the one to be replaced is at the end of the list. Also calculate the
!  new value of ZDOTA(NACT) and branch if it is not acceptable.

DO k=1,nact
  vmultc(k) = MAX(0.0_dp,vmultc(k) - ratio*vmultd(k))
END DO
IF (icon < nact) THEN
  isave = iact(icon)
  vsave = vmultc(icon)
  k = icon
  170 kp = k + 1
  kw = iact(kp)
  sp = DOT_PRODUCT( z(1:n,k), a(1:n,kw) )
  temp = SQRT(sp*sp + zdota(kp)**2)
  alpha = zdota(kp)/temp
  beta = sp/temp
  zdota(kp) = alpha*zdota(k)
  zdota(k) = temp
  !dir$ assume_aligned z:64
  !$omp simd simdlen(8) private(temp)
  DO i=1,n
    temp = alpha*z(i,kp) + beta*z(i,k)
    z(i,kp) = alpha*z(i,k) - beta*z(i,kp)
    z(i,k) = temp
  END DO
  iact(k) = kw
  vmultc(k) = vmultc(kp)
  k = kp
  IF (k < nact) GO TO 170
  iact(k) = isave
  vmultc(k) = vsave
END IF
temp = DOT_PRODUCT( z(1:n,nact), a(1:n,kk) )
IF (temp == 0.0_dp) GO TO 490
zdota(nact) = temp
vmultc(icon) = 0.0_dp
vmultc(nact) = ratio

!  Update IACT and ensure that the objective function continues to be
!  treated as the last active constraint when MCON>M.

210 iact(icon) = iact(nact)
iact(nact) = kk
IF (mcon > m .AND. kk /= mcon) THEN
  k = nact - 1
  sp = DOT_PRODUCT( z(1:n,k), a(1:n,kk) )
  temp = SQRT(sp*sp + zdota(nact)**2)
  alpha = zdota(nact)/temp
  beta = sp/temp
  zdota(nact) = alpha*zdota(k)
  zdota(k) = temp
  !dir$ assume_aligned z:64
  !$omp simd simdlen(8) private(temp)
  DO i=1,n
    temp = alpha*z(i,nact) + beta*z(i,k)
    z(i,nact) = alpha*z(i,k) - beta*z(i,nact)
    z(i,k) = temp
  END DO
  iact(nact) = iact(k)
  iact(k) = kk
  temp = vmultc(k)
  vmultc(k) = vmultc(nact)
  vmultc(nact) = temp
END IF

!  If stage one is in progress, then set SDIRN to the direction of the next
!  change to the current vector of variables.

IF (mcon > m) GO TO 320
kk = iact(nact)
temp = DOT_PRODUCT( sdirn(1:n), a(1:n,kk) )
temp = temp - 1.0_dp
temp = temp/zdota(nact)
sdirn(1:n) = sdirn(1:n) - temp*z(1:n,nact)
GO TO 340

!  Delete the constraint that has the index IACT(ICON) from the active set.

260 IF (icon < nact) THEN
  isave = iact(icon)
  vsave = vmultc(icon)
  k = icon
  DO
    kp = k + 1
    kk = iact(kp)
    sp = DOT_PRODUCT( z(1:n,k), a(1:n,kk) )
    temp = SQRT(sp*sp + zdota(kp)**2)
    alpha = zdota(kp)/temp
    beta = sp/temp
    zdota(kp) = alpha*zdota(k)
    zdota(k) = temp
     !dir$ assume_aligned z:64
     !$omp simd simdlen(8) private(temp)
    DO i=1,n
      temp = alpha*z(i,kp) + beta*z(i,k)
      z(i,kp) = alpha*z(i,k) - beta*z(i,kp)
      z(i,k) = temp
    END DO
    iact(k) = kk
    vmultc(k) = vmultc(kp)
    k = kp
    IF (k >= nact) EXIT
  END DO
  iact(k) = isave
  vmultc(k) = vsave
END IF
nact = nact - 1

!  If stage one is in progress, then set SDIRN to the direction of the next
!  change to the current vector of variables.

IF (mcon > m) GO TO 320
temp = DOT_PRODUCT( sdirn(1:n), z(1:n,nact+1) )
sdirn(1:n) = sdirn(1:n) - temp*z(1:n,nact+1)
GO TO 340

!  Pick the next search direction of stage two.

320 temp = 1.0_dp/zdota(nact)
sdirn(1:n) = temp*z(1:n,nact)

!  Calculate the step to the boundary of the trust region or take the step
!  that reduces RESMAX to zero. The two statements below that include the
!  factor 1.0E-6 prevent some harmless underflows that occurred in a test
!  calculation. Further, we skip the step if it could be zero within a
!  reasonable tolerance for computer rounding errors.

340 dd = rho*rho
sd = 0.0_dp
ss = 0.0_dp
!dir$ assume_aligned dx:64
!dir$ assume_aligned sdirn:64
!$omp simd simdlen(8) private(dd) reduction(+:sd) reduction(+:ss)
DO i=1,n
  IF (ABS(dx(i)) >= 1.0E-6_dp*rho) dd = dd - dx(i)**2
  sd = sd + dx(i)*sdirn(i)
  ss = ss + sdirn(i)**2
END DO
IF (dd <= 0.0_dp) GO TO 490
temp = SQRT(ss*dd)
IF (ABS(sd) >= 1.0E-6_dp*temp) temp = SQRT(ss*dd + sd*sd)
stpful = dd/(temp + sd)
step = stpful
IF (mcon == m) THEN
  acca = step + 0.1_dp*resmax
  accb = step + 0.2_dp*resmax
  IF (step >= acca .OR. acca >= accb) GO TO 480
  step = MIN(step,resmax)
END IF

!  Set DXNEW to the new variables if STEP is the steplength, and reduce
!  RESMAX to the corresponding maximum residual if stage one is being done.
!  Because DXNEW will be changed during the calculation of some Lagrange
!  multipliers, it will be restored to the following value later.

dxnew(1:n) = dx(1:n) + step*sdirn(1:n)
IF (mcon == m) THEN
  resold = resmax
  resmax = 0.0_dp
  DO k=1,nact
    kk = iact(k)
    temp = b(kk) - DOT_PRODUCT( a(1:n,kk), dxnew(1:n) )
    resmax = MAX(resmax,temp)
  END DO
END IF

!  Set VMULTD to the VMULTC vector that would occur if DX became DXNEW. A
!  device is included to force VMULTD(K) = 0.0 if deviations from this value
!  can be attributed to computer rounding errors. First calculate the new
!  Lagrange multipliers.

k = nact
390 zdotw = 0.0_dp
zdwabs = 0.0_dp
!dir$ assume_aligned z:64
!dir$ assume_aligned dxnew:64
!$omp simd simdlen(8) private(temp) reduction(+:zdotw) reduction(+:zdwabs)
DO i=1,n
  temp = z(i,k)*dxnew(i)
  zdotw = zdotw + temp
  zdwabs = zdwabs + ABS(temp)
END DO
acca = zdwabs + 0.1_dp*ABS(zdotw)
accb = zdwabs + 0.2_dp*ABS(zdotw)
IF (zdwabs >= acca .OR. acca >= accb) zdotw = 0.0_dp
vmultd(k) = zdotw / zdota(k)
IF (k >= 2) THEN
  kk = iact(k)
  dxnew(1:n) = dxnew(1:n) - vmultd(k)*a(1:n,kk)
  k = k - 1
  GO TO 390
END IF
IF (mcon > m) vmultd(nact) = MAX(0.0_dp,vmultd(nact))

!  Complete VMULTC by finding the new constraint residuals.

dxnew(1:n) = dx(1:n) + step*sdirn(1:n)
IF (mcon > nact) THEN
  kl = nact + 1
  DO k=kl,mcon
    kk = iact(k)
    total = resmax - b(kk)
    sumabs = resmax + ABS(b(kk))
    !dir$ assume_aligned a:64
    !dir$ assume_aligned dxnew:64
    !$omp simd simdlen(8) private(temp) reduction(+:total) reduction(+:sumabs)
    DO i=1,n
      temp = a(i,kk)*dxnew(i)
      total = total + temp
      sumabs = sumabs + ABS(temp)
    END DO
    acca = sumabs + 0.1*ABS(total)
    accb = sumabs + 0.2*ABS(total)
    IF (sumabs >= acca .OR. acca >= accb) total = 0.0
    vmultd(k) = total
  END DO
END IF

!  Calculate the fraction of the step from DX to DXNEW that will be taken.

ratio = 1.0_dp
icon = 0
DO k=1,mcon
  IF (vmultd(k) < 0.0_dp) THEN
    temp = vmultc(k)/(vmultc(k) - vmultd(k))
    IF (temp < ratio) THEN
      ratio = temp
      icon = k
    END IF
  END IF
END DO

!  Update DX, VMULTC and RESMAX.

temp = 1.0_dp - ratio
dx(1:n) = temp*dx(1:n) + ratio*dxnew(1:n)
!dir$ assume_aligned vmultc:64
!dir$ assume_aligned vmultd:64
!$omp simd simdlen(8)
DO k=1,mcon
  vmultc(k) = MAX(0.0_dp,temp*vmultc(k) + ratio*vmultd(k))
END DO
IF (mcon == m) resmax = resold + ratio*(resmax - resold)

!  If the full step is not acceptable then begin another iteration.
!  Otherwise switch to stage two or end the calculation.

IF (icon > 0) GO TO 70
IF (step == stpful) GO TO 500
480 mcon = m + 1
icon = mcon
iact(mcon) = mcon
vmultc(mcon) = 0.0_dp
GO TO 60

!  We employ any freedom that may be available to reduce the objective
!  function before returning a DX whose length is less than RHO.

490 IF (mcon == m) GO TO 480
ifull = 0

500 RETURN
END SUBROUTINE trstlp

END MODULE cobyla2



!MODULE common_nprob
!IMPLICIT NONE

!INTEGER(i4), SAVE, PUBLIC :: nprob

!END MODULE common_nprob


!------------------------------------------------------------------------------
!  Main program of test problems in Report DAMTP 1992/NA5.
!------------------------------------------------------------------------------
#if 0
PROGRAM test_cobyla
USE common_nprob
USE cobyla2
IMPLICIT NONE

INTEGER(i4), PARAMETER :: nn = 10

REAL (dp) :: rhobeg, rhoend, temp, tempa, tempb, tempc, tempd, x(nn),  &
             xopt(nn)
INTEGER(i4)   :: i, icase, iprint, m, maxfun, n

INTERFACE
  SUBROUTINE calcfc (n, m, x, f, con)
    IMPLICIT NONE
    INTEGER(i4), PARAMETER :: dp = SELECTED_REAL_KIND(14, 60)
    INTEGER(i4), INTENT(IN)    :: n, m
    REAL (dp), INTENT(IN)  :: x(:)
    REAL (dp), INTENT(OUT) :: f
    REAL (dp), INTENT(OUT) :: con(:)
  END SUBROUTINE calcfc
END INTERFACE

DO nprob=1,10
  IF (nprob == 1) THEN

!  Minimization of a simple quadratic function of two variables.

    WRITE(*, 10)
    10 FORMAT (/'       Output from test problem 1 (Simple quadratic)')
    n = 2
    m = 0
    xopt(1) = -1.0_dp
    xopt(2) = 0.0_dp
  ELSE IF (nprob == 2) THEN

!  Easy two dimensional minimization in unit circle.

    WRITE(*, 20)
    20 FORMAT (/'       Output from test problem 2 (2D unit circle calculation)')
    n = 2
    m = 1
    xopt(1) = SQRT(0.5_dp)
    xopt(2) = -xopt(1)
  ELSE IF (nprob == 3) THEN

!  Easy three dimensional minimization in ellipsoid.

    WRITE(*, 30)
    30 FORMAT (/'       Output from test problem 3 (3D ellipsoid calculation)')
    n = 3
    m = 1
    xopt(1) = 1.0_dp/SQRT(3.0_dp)
    xopt(2) = 1.0_dp/SQRT(6.0_dp)
    xopt(3) = -1.0_dp/3.0_dp
  ELSE IF (nprob == 4) THEN

!  Weak version of Rosenbrock's problem.

    WRITE(*, 40)
    40 FORMAT (/'       Output from test problem 4 (Weak Rosenbrock)')
    n = 2
    m = 0
    xopt(1) = -1.0_dp
    xopt(2) = 1.0_dp
  ELSE IF (nprob == 5) THEN

!  Intermediate version of Rosenbrock's problem.

    WRITE(*, 50)
    50 FORMAT (/'       Output from test problem 5 (Intermediate Rosenbrock)')
    n = 2
    m = 0
    xopt(1) = -1.0_dp
    xopt(2) = 1.0_dp
  ELSE IF (nprob == 6) THEN

!  This problem is taken from Fletcher's book Practical Methods of
!  Optimization and has the equation number (9.1.15).

    WRITE(*, 60)
    60 FORMAT (/'       Output from test problem 6 (Equation ',  &
               '(9.1.15) in Fletcher)')
    n = 2
    m = 2
    xopt(1) = SQRT(0.5_dp)
    xopt(2) = xopt(1)
  ELSE IF (nprob == 7) THEN

!  This problem is taken from Fletcher's book Practical Methods of
!  Optimization and has the equation number (14.4.2).

    WRITE(*, 70)
    70 FORMAT (/'       Output from test problem 7 (Equation ',  &
               '(14.4.2) in Fletcher)')
    n = 3
    m = 3
    xopt(1) = 0.0_dp
    xopt(2) = -3.0_dp
    xopt(3) = -3.0_dp
  ELSE IF (nprob == 8) THEN

!  This problem is taken from page 66 of Hock and Schittkowski's book Test
!  Examples for Nonlinear Programming Codes. It is their test problem Number
!  43, and has the name Rosen-Suzuki.

    WRITE(*, 80)
    80 FORMAT (/'       Output from test problem 8 (Rosen-Suzuki)')
    n=4
    m=3
    xopt(1) = 0.0_dp
    xopt(2) = 1.0_dp
    xopt(3) = 2.0_dp
    xopt(4) = -1.0_dp
  ELSE IF (nprob == 9) THEN

!  This problem is taken from page 111 of Hock and Schittkowski's
!  book Test Examples for Nonlinear Programming Codes. It is their
!  test problem Number 100.

    WRITE(*, 90)
    90 FORMAT (/'       Output from test problem 9 (Hock and Schittkowski 100)')
    n = 7
    m = 4
    xopt(1) = 2.330499_dp
    xopt(2) = 1.951372_dp
    xopt(3) = -0.4775414_dp
    xopt(4) = 4.365726_dp
    xopt(5) = -0.624487_dp
    xopt(6) = 1.038131_dp
    xopt(7) = 1.594227_dp
  ELSE IF (nprob == 10) THEN

!  This problem is taken from page 415 of Luenberger's book Applied
!  Nonlinear Programming. It is to maximize the area of a hexagon of
!  unit diameter.

    WRITE(*, 100)
    100 FORMAT (/'       Output from test problem 10 (Hexagon area)')
    n = 9
    m = 14
  END IF

  DO icase = 1,2
    x(1:n) = 1.0_dp
    rhobeg = 0.5_dp
    rhoend = 1.d-6
    IF (icase == 2) rhoend = 1.d-8
    iprint = 1
    maxfun = 3500
    CALL cobyla (n, m, x, rhobeg, rhoend, iprint, maxfun)
    IF (nprob == 10) THEN
      tempa = x(1) + x(3) + x(5) + x(7)
      tempb = x(2) + x(4) + x(6) + x(8)
      tempc = 0.5_dp/SQRT(tempa*tempa + tempb*tempb)
      tempd = tempc*SQRT(3.0_dp)
      xopt(1) = tempd*tempa + tempc*tempb
      xopt(2) = tempd*tempb - tempc*tempa
      xopt(3) = tempd*tempa - tempc*tempb
      xopt(4) = tempd*tempb + tempc*tempa
      DO i=1,4
        xopt(i+4) = xopt(i)
      END DO
    END IF
    temp = 0.0_dp
    DO i=1,n
      temp = temp + (x(i) - xopt(i))**2
    END DO
    WRITE(*, 150) SQRT(temp)
    150 FORMAT (/'     Least squares error in variables = ', G16.6)
  END DO
  WRITE(*, 170)
  170 FORMAT ('  ----------------------------------------------',  &
              '--------------------')
END DO

STOP
END PROGRAM test_cobyla


!------------------------------------------------------------------------------

SUBROUTINE calcfc (n, m, x, f, con)
USE common_nprob
IMPLICIT NONE

INTEGER(i4), PARAMETER :: dp = SELECTED_REAL_KIND(14, 60)

INTEGER(i4), INTENT(IN)     :: n
INTEGER(i4), INTENT(IN)     :: m
REAL (dp), INTENT(IN)   :: x(:)
REAL (dp), INTENT(OUT)  :: f
REAL (dp), INTENT(OUT)  :: con(:)

IF (nprob == 1) THEN

!  Test problem 1 (Simple quadratic)

  f = 10.0_dp*(x(1) + 1.0_dp)**2 + x(n)**2
ELSE IF (nprob == 2) THEN

!    Test problem 2 (2D unit circle calculation)

  f = x(1)*x(2)
  con(m) = 1.0_dp - x(1)**2 - x(n)**2
ELSE IF (nprob == 3) THEN

!  Test problem 3 (3D ellipsoid calculation)

  f = x(1)*x(2)*x(3)
  con(1) = 1.0_dp - x(1)**2 - 2.0_dp*x(2)**2 - 3.0_dp*x(n)**2
ELSE IF (nprob == 4) THEN

!  Test problem 4 (Weak Rosenbrock)

  f = (x(1)**2 - x(2))**2 + (1.0_dp + x(1))**2
ELSE IF (nprob == 5) THEN

!  Test problem 5 (Intermediate Rosenbrock)

  f = 10.0_dp*(x(1)**2 - x(n))**2 + (1.0_dp + x(1))**2
ELSE IF (nprob == 6) THEN

!  Test problem 6 (Equation (9.1.15) in Fletcher's book)

  f = - x(1) - x(2)
  con(1) = x(2) - x(1)**2
  con(2) = 1.0_dp - x(1)**2 - x(2)**2
ELSE IF (nprob == 7) THEN

!  Test problem 7 (Equation (14.4.2) in Fletcher's book)

  f = x(3)
  con(1) = 5.0_dp*x(1) - x(2) + x(3)
  con(2) = x(3) - x(1)**2 - x(2)**2 - 4.0_dp*x(2)
  con(m) = x(3) - 5.0_dp*x(1) - x(2)
ELSE IF (nprob == 8) THEN

!  Test problem 8 (Rosen-Suzuki)

  f = x(1)**2 + x(2)**2 + 2.0*x(3)**2 + x(4)**2 - 5.0_dp*x(1) - 5.0_dp*x(2)  &
      - 21.0_dp*x(3) + 7.0_dp*x(4)
  con(1) = 8.0_dp - x(1)**2 - x(2)**2 - x(3)**2 - x(4)**2 - x(1) + x(2)  &
           - x(3) + x(4)
  con(2) = 10._dp - x(1)**2 - 2._dp*x(2)**2 - x(3)**2 - 2._dp*x(4)**2 + x(1) + x(4)
  con(m) = 5.0_dp - 2.0*x(1)**2 - x(2)**2 - x(3)**2 - 2.0_dp*x(1) + x(2) + x(4)
ELSE IF (nprob == 9) THEN

!  Test problem 9 (Hock and Schittkowski 100)

  f = (x(1) - 10._dp)**2 + 5._dp*(x(2) - 12._dp)**2 + x(3)**4 + 3._dp*(x(4)  &
       - 11._dp)**2 + 10._dp*x(5)**6 + 7._dp*x(6)**2 + x(7)**4 - 4._dp*x(6)*x(7) &
       - 10._dp*x(6) - 8._dp*x(7)
  con(1) = 127._dp - 2._dp*x(1)**2 - 3._dp*x(2)**4 - x(3) - 4._dp*x(4)**2   &
           - 5._dp*x(5)
  con(2) = 282._dp - 7._dp*x(1) - 3._dp*x(2) - 10._dp*x(3)**2 - x(4) + x(5)
  con(3) = 196._dp - 23._dp*x(1) - x(2)**2 - 6._dp*x(6)**2 + 8._dp*x(7)
  con(4) = - 4._dp*x(1)**2 - x(2)**2 + 3._dp*x(1)*x(2) - 2._dp*x(3)**2 - 5._dp*x(6)  &
           + 11._dp*x(7)
ELSE IF (nprob == 10) THEN

!  Test problem 10 (Hexagon area)

  f = - 0.5_dp*(x(1)*x(4) - x(2)*x(3) + x(3)*x(n) - x(5)*x(n) + x(5)*x(8) &
                - x(6)*x(7))
  con(1) = 1.0_dp - x(3)**2 - x(4)**2
  con(2) = 1.0_dp - x(n)**2
  con(3) = 1.0_dp - x(5)**2 - x(6)**2
  con(4) = 1.0_dp - x(1)**2 - (x(2) - x(n))**2
  con(5) = 1.0_dp - (x(1) - x(5))**2 - (x(2) - x(6))**2
  con(6) = 1.0_dp - (x(1) - x(7))**2 - (x(2) - x(8))**2
  con(7) = 1.0_dp - (x(3) - x(5))**2 - (x(4) - x(6))**2
  con(8) = 1.0_dp - (x(3) - x(7))**2 - (x(4) - x(8))**2
  con(9) = 1.0_dp - x(7)**2 - (x(8) - x(n))**2
  con(10) = x(1)*x(4) - x(2)*x(3)
  con(11) = x(3)*x(n)
  con(12) = - x(5)*x(n)
  con(13) = x(5)*x(8) - x(6)*x(7)
  con(m) = x(n)
END IF

RETURN
END SUBROUTINE calcfc
#endif


! The following output was obtained using Lahey's ELF90 compiler
!
!
!   Output from test problem 1 (Simple quadratic)
!
!  Normal return from subroutine COBYLA
!
!  NFVALS =    87   F =  0.118975E-10    MAXCV =  0.000000
!  X =   -1.00000      0.262770E-05
!
!    Least squares error in variables =     0.272104E-05
!
!  Normal return from subroutine COBYLA
!
!  NFVALS =   116   F =  0.258086E-14    MAXCV =  0.000000
!  X =   -1.00000      0.403932E-07
!
!    Least squares error in variables =     0.415516E-07
! ------------------------------------------------------------------
!
!   Output from test problem 2 (2D unit circle calculation)
!
!  Normal return from subroutine COBYLA
!
!  NFVALS =    66   F = -0.500000        MAXCV =  0.199977E-11
!  X =   0.707106     -0.707108
!
!    Least squares error in variables =     0.131782E-05
!
!  Normal return from subroutine COBYLA
!
!  NFVALS =    82   F = -0.500000        MAXCV =  0.305826E-15
!  X =   0.707107     -0.707107
!
!    Least squares error in variables =     0.348402E-08
! ------------------------------------------------------------------
!
!   Output from test problem 3 (3D ellipsoid calculation)
!
!  Normal return from subroutine COBYLA
!
!  NFVALS =    94   F = -0.785674E-01    MAXCV =  0.593595E-11
!  X =   0.577351      0.408247     -0.333334
!
!    Least squares error in variables =     0.111077E-05
!
!  Normal return from subroutine COBYLA
!
!  NFVALS =   115   F = -0.785674E-01    MAXCV =  0.333853E-15
!  X =   0.577350      0.408248     -0.333333
!
!    Least squares error in variables =     0.693448E-08
! ------------------------------------------------------------------
!
!   Output from test problem 4 (Weak Rosenbrock)
!
!  Normal return from subroutine COBYLA
!
!  NFVALS =   269   F =  0.703758E-10    MAXCV =  0.000000
!  X =  -0.999992      0.999983
!
!    Least squares error in variables =     0.188469E-04
!
!  Normal return from subroutine COBYLA
!
!  NFVALS =   387   F =  0.799591E-14    MAXCV =  0.000000
!  X =   -1.00000       1.00000
!
!    Least squares error in variables =     0.203843E-06
! ------------------------------------------------------------------
!
!   Output from test problem 5 (Intermediate Rosenbrock)
!
!  Normal return from subroutine COBYLA
!
!  NFVALS =  2174   F =  0.839835E-08    MAXCV =  0.000000
!  X =  -0.999908      0.999816
!
!    Least squares error in variables =     0.205844E-03
!
!  Normal return from subroutine COBYLA
!
!  NFVALS =  3377   F =  0.948763E-12    MAXCV =  0.000000
!  X =  -0.999999      0.999998
!
!    Least squares error in variables =     0.218951E-05
! ------------------------------------------------------------------
!
!   Output from test problem 6 (Equation (9.1.15) in Fletcher)
!
!  Normal return from subroutine COBYLA
!
!  NFVALS =    63   F =  -1.41421        MAXCV =  0.199977E-11
!  X =   0.707106      0.707107
!
!    Least squares error in variables =     0.588093E-06
!
!  Normal return from subroutine COBYLA
!
!  NFVALS =    79   F =  -1.41421        MAXCV =  0.138669E-15
!  X =   0.707107      0.707107
!
!    Least squares error in variables =     0.139562E-08
! ------------------------------------------------------------------
!
!   Output from test problem 7 (Equation (14.4.2) in Fletcher)
!
!  Normal return from subroutine COBYLA
!
!  NFVALS =    40   F =  -3.00000        MAXCV =  0.000000
!  X =  -0.644464E-20  -3.00000      -3.00000
!
!    Least squares error in variables =     0.299666E-08
!
!  Normal return from subroutine COBYLA
!
!  NFVALS =    47   F =  -3.00000        MAXCV =  0.424972E-08
!  X =  -0.651433E-20  -3.00000      -3.00000
!
!    Least squares error in variables =     0.200334E-08
! ------------------------------------------------------------------
!
!   Output from test problem 8 (Rosen-Suzuki)
!
!  Normal return from subroutine COBYLA
!
!  NFVALS =   120   F =  -44.0000        MAXCV =  0.444367E-11
!  X =   0.607440E-06  0.999999       2.00000      -1.00000
!
!    Least squares error in variables =     0.152955E-05
!
!  Normal return from subroutine COBYLA
!
!  NFVALS =   150   F =  -44.0000        MAXCV =  0.000000
!  X =   0.193525E-07   1.00000       2.00000      -1.00000
!
!    Least squares error in variables =     0.501895E-07
! ------------------------------------------------------------------
!
!   Output from test problem 9 (Hock and Schittkowski 100)
!
!  Normal return from subroutine COBYLA
!
!  NFVALS =   422   F =   680.630        MAXCV =  0.595093E-11
!  X =    2.33050       1.95137     -0.477538       4.36573     -0.624488
!    1.03813        1.59423
!
!    Least squares error in variables =     0.416981E-05
!
!  Normal return from subroutine COBYLA
!
!  NFVALS =   476   F =   680.630        MAXCV =  0.813238E-14
!  X =    2.33050       1.95137     -0.477541       4.36573     -0.624487
!    1.03813        1.59423
!
!    Least squares error in variables =     0.809366E-06
! ------------------------------------------------------------------
!
!   Output from test problem 10 (Hexagon area)
!
!  Normal return from subroutine COBYLA
!
!  NFVALS =   243   F = -0.866025        MAXCV =  0.971976E-23
!  X =   0.687780      0.725919     -0.284774      0.958595      0.687780
!   0.725919      -0.284774
!   0.958595       0.141321E-22
!
!    Least squares error in variables =      1.44346
!
!  Normal return from subroutine COBYLA
!
!  NFVALS =   285   F = -0.866025        MAXCV =  0.220618E-15
!  X =   0.687780      0.725919     -0.284774      0.958595      0.687780
!   0.725919      -0.284774
!   0.958595      -0.814519E-24
!
!    Least squares error in variables =      1.44346
! ------------------------------------------------------------------
