MODULE mod_arma_estimation

  use mod_kinds, only : i4,sp
  use omp_lib

IMPLICIT NONE

! Code converted using TO_F90 by Alan Miller
! Date: 2000-12-27  Time: 21:46:40
! Slightly modified and adapted to needs of GMS project
! by Bernard Gingold on 29-11-2020 3:57PM +00200

!  ALGORITHM AS 154  APPL. STATIST. (1980) VOL.29, P.311
!  ALGORITHM AS 182  APPL. STATIST. (1982) VOL.31, NO.2

  !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
   
integer(kind=i4), parameter, public :: MOD_ARMA_ESTIMATION_MAJOR = 1
  
integer(kind=i4), parameter, public :: MOD_ARMA_ESTIMATION_MINOR = 1
   
integer(kind=i4), parameter, public :: MOD_ARMA_ESTIMATION_MICRO = 1
   
integer(kind=i4), parameter, public :: MOD_ARMA_ESTIMATION_FULLVER = 1000*MOD_ARMA_ESTIMATION_MAJOR + &
                                                             100*MOD_ARMA_ESTIMATION_MINOR  + &
                                                             10*MOD_ARMA_ESTIMATION_MICRO
   
character(*),  parameter, public :: MOD_ARMA_ESTIMATION_CREATE_DATE = "29-11-2018 15:37 +00200 (SUN 29 NOV  2020 GMT+2) "
   
    ! Module build date ( should be set after successful compilation)
character(*),  parameter, public :: MOD_ARMA_ESTIMATION_BUILD_DATE = __DATE__ ":" __TIME__
   
    ! Module author info
character(*),  parameter, public :: MOD_ARMA_ESTIMATION_AUTHOR = "ALGORITHM AS 154, ALGORITHM AS 182 (converted to F90 by Alan Miller), modified by Bernard Gingold, beniekg@gmail.com "
   
    ! Module short description
character(*),  parameter, public :: MOD_ARMA_ESTIMATION_DESCRIPT = "ALGORITHM AS 154  APPL. STATIST. (1980) VOL.29, P.311, ALGORITHM AS 182  APPL. STATIST. (1982) VOL.31, NO.2"
    

REAL(sp), PARAMETER, PRIVATE  :: zero = 0.0_sp, one = 1.0_sp, two = 2.0_sp


CONTAINS

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
SUBROUTINE starma(ip,iq,ir,np,phi,theta,a,p,v,thetab,xnext,xrow,  &
     rbar,nrbar,ifault) !GCC$ ATTRIBUTES aligned(32) :: starma !GCC$ ATTRIBUTES inline :: starma !GCC$ ATTRIBUTES hot :: starma
#elif defined __INTEL_COMPILER
SUBROUTINE starma(ip,iq,ir,np,phi,theta,a,p,v,thetab,xnext,xrow,  &
     rbar,nrbar,ifault)
  !DIR$ CODE_ALIGN : 32 :: starma
  !DIR$ OPTIMIZE : 3 
  !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: starma
#endif

!  INVOKING THIS SUBROUTINE SETS THE VALUES OF V AND PHI, AND
!  OBTAINS THE INITIAL VALUES OF A AND P.
!  THIS ROUTINE IS NOT SUITABLE FOR USE WITH AN AR(1) PROCESS.
!  IN THIS CASE THE FOLLOWING INSTRUCTIONS SHOULD BE USED FOR INITIALISATION.
!  V(1) = 1.0
!  A(1) = 0.0
!  P(1) = 1.0 / (1.0 - PHI(1) * PHI(1))

INTEGER(i4), INTENT(IN)   :: ip
INTEGER(i4), INTENT(IN)   :: iq
INTEGER(i4), INTENT(IN)   :: ir
INTEGER(i4), INTENT(IN)   :: np
REAL(sp), ALLOCATABLE, INTENT(OUT)     :: phi(:)

REAL(sp), ALLOCATABLE, INTENT(IN)      :: theta(:)

REAL(sp), ALLOCATABLE, INTENT(OUT)     :: a(:)

REAL(sp), ALLOCATABLE, INTENT(OUT)     :: p(:)

REAL(sp), ALLOCATABLE, INTENT(OUT)     :: v(:)

REAL(sp), ALLOCATABLE, INTENT(OUT)     :: thetab(:)

REAL(sp), ALLOCATABLE, INTENT(OUT)     :: xnext(:)

REAL(sp), ALLOCATABLE, INTENT(OUT)     :: xrow(:)

REAL(sp), ALLOCATABLE, INTENT(OUT)     :: rbar(:)

INTEGER(i4), INTENT(IN)   :: nrbar
INTEGER(i4), INTENT(OUT)  :: ifault

REAL(sp)     :: vj, phii, phij, ssqerr, recres, ynext
INTEGER(i4)  :: i, ifail, ind, ind1, ind2, indi, indj, indn, irank, j, k, npr, npr1

!        CHECK FOR FAILURE INDICATION.

ifault = 0
IF (ip < 0) ifault = 1
IF (iq < 0) ifault = ifault + 2
IF (ip == 0 .AND. iq == 0) ifault = 4
k = iq + 1
IF (k < ip) k = ip
IF (ir /= k) ifault = 5
IF (np /= ir*(ir+1)/2) ifault = 6
IF (nrbar /= np*(np-1)/2) ifault = 7
IF (ir == 1) ifault = 8
IF (ifault /= 0) RETURN

!        NOW SET A(0), V AND PHI.

!$OMP SIMD ALIGNED(a:64) ALIGNED(v:64) \
!$OMP& ALIGNED(phi:64) ALIGNED(theta:64) LINEAR(i:1)
DO  i = 2, ir
  a(i) = zero
  IF (i > ip) phi(i) = zero
  v(i) = zero
  IF (i <= iq+1) v(i) = theta(i-1)
END DO

a(1) = zero
IF (ip == 0) phi(1) = zero
v(1) = one
ind = ir
DO  j = 2, ir
  vj = v(j)
  DO  i = j, ir
    ind = ind + 1
    v(ind) = v(i) * vj
  END DO
END DO

!        NOW FIND P(0).

IF (ip /= 0) THEN
  
!   THE SET OF EQUATIONS S * VEC(P(0)) = VEC(V) IS SOLVED FOR VEC(P(0)).
!   S IS GENERATED ROW BY ROW IN THE ARRAY XNEXT.
!   THE ORDER OF ELEMENTS IN P IS CHANGED, SO AS TO
!   BRING MORE LEADING ZEROS INTO THE ROWS OF S,
!   HENCE ACHIEVING A REDUCTION OF COMPUTING TIME.
  
  irank = 0
  ssqerr = zero
  rbar(1:nrbar) = zero

!$OMP SIMD ALIGNED(p:64) ALIGNED(thetab:64) ALIGNED(xnext:64) \
!$OMP& LINEAR(i:1)
#endif
  DO  i = 1, np
    p(i) = zero
    thetab(i) = zero
    xnext(i) = zero
 END DO

  ind = 0
  ind1 = 0
  npr = np - ir
  npr1 = npr + 1
  indj = npr1
  ind2 = npr
  DO  j = 1, ir
    phij = phi(j)
    xnext(indj) = zero
    indj = indj + 1
    indi = npr1 + j
    DO  i = j, ir
      ind = ind + 1
      ynext = v(ind)
      phii = phi(i)
      IF (j /= ir) THEN
        xnext(indj) = -phii
        IF (i /= ir) THEN
          xnext(indi) = xnext(indi) - phij
          ind1 = ind1 + 1
          xnext(ind1) = -one
        END IF
      END IF
      xnext(npr1) = -phii * phij
      ind2 = ind2 + 1
      IF (ind2 > np) ind2 = 1
      xnext(ind2) = xnext(ind2) + one
      CALL inclu2(np,one,xnext,xrow,ynext,p,rbar,thetab,  &
                  ssqerr,recres,irank,ifail)
      
!        NO NEED TO CHECK IFAIL AS WEIGHT = 1.0
      
      xnext(ind2) = zero
      IF (i /= ir) THEN
        xnext(indi) = zero
        indi = indi + 1
        xnext(ind1) = zero
      END IF
    END DO
  END DO
  CALL regres(np,nrbar,rbar,thetab,p)
  
!        NOW RE-ORDER P.
  
  ind = npr

!$OMP SIMD ALIGNED(p:64) ALIGNED(xnext:64) LINEAR(i:1)
  DO  i = 1, ir
    ind = ind + 1
    xnext(i) = p(ind)
 END DO

  ind = np
  ind1 = npr
  DO  i = 1, npr
    p(ind) = p(ind1)
    ind = ind - 1
    ind1 = ind1 - 1
  END DO
  p(1:ir) = xnext(1:ir)

  RETURN
END IF

!        P(0) IS OBTAINED BY BACKSUBSTITUTION FOR A MOVING AVERAGE PROCESS.

indn = np + 1
ind = np + 1
DO  i = 1, ir
  DO  j = 1, i
    ind = ind - 1
    p(ind) = v(ind)
    IF (j /= 1) THEN
      indn = indn - 1
      p(ind) = p(ind) + p(indn)
    END IF
  END DO
END DO

RETURN
END SUBROUTINE starma


#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
SUBROUTINE karma(ip,iq,ir,phi,theta,a,p,v,n,w,resid,sumlog,ssq,  &
     iupd,delta,e,nit) !GCC$ ATTRIBUTES aligned(32) :: karma !GCC$ ATTRIBUTES inline :: karma !GCC$ ATTRIBUTES hot :: karma
#elif defined __INTEL_COMPILER
SUBROUTINE karma(ip,iq,ir,phi,theta,a,p,v,n,w,resid,sumlog,ssq,  &
     iupd,delta,e,nit)
  !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: karma
  !DIR$ ATTRIBUTES INLINE :: karma
#endif

! N.B. Argument NP has been removed.

!   ALGORITHM AS 154.1  APPL. STATIST. (1980) VOL.29, P.311

!   INVOKING THIS SUBROUTINE UPDATES A, P, SUMLOG AND SSQ BY
!   INCLUSION OF DATA VALUES W(1) TO W(N).  THE CORRESPONDING
!   VALUES OF RESID ARE ALSO OBTAINED.
!   WHEN FT IS LESS THAN (1 + DELTA), QUICK RECURSIONS ARE USED.

INTEGER(i4), INTENT(IN)      :: ip
INTEGER(i4), INTENT(IN)      :: iq
INTEGER(i4), INTENT(IN)      :: ir
REAL(sp), INTENT(IN)         :: phi(:)
#if defined __INTEL_COMPILER || defined __ICC
!DIR$  ASSUME_ALIGNED phi:64
#endif
REAL(sp), INTENT(IN)         :: theta(:)
#if defined __INTEL_COMPILER || defined __ICC
!DIR$  ASSUME_ALIGNED theta:64
#endif
REAL(sp), INTENT(IN OUT)     :: a(:)
#if defined __INTEL_COMPILER || defined __ICC
!DIR$  ASSUME_ALIGNED a:64
#endif
REAL(sp), INTENT(IN OUT)     :: p(:)
#if defined __INTEL_COMPILER || defined __ICC
!DIR$  ASSUME_ALIGNED p:64
#endif
REAL(sp), INTENT(IN)         :: v(:)
#if defined __INTEL_COMPILER || defined __ICC
!DIR$  ASSUME_ALIGNED v:64
#endif
INTEGER(i4), INTENT(IN)      :: n
REAL(sp), INTENT(IN)         :: w(:)
#if defined __INTEL_COMPILER || defined __ICC
!DIR$  ASSUME_ALIGNED w:64
#endif
REAL(sp), INTENT(OUT)        :: resid(:)
#if defined __INTEL_COMPILER || defined __ICC
!DIR$  ASSUME_ALIGNED resid:64
#endif
REAL(sp), INTENT(IN OUT)     :: sumlog
REAL(sp), INTENT(IN OUT)     :: ssq
INTEGER(i4), INTENT(IN)      :: iupd
REAL(sp), INTENT(IN)         :: delta
REAL(sp), INTENT(OUT)        :: e(:)
#if defined __INTEL_COMPILER || defined __ICC
!DIR$  ASSUME_ALIGNED e:64
#endif
INTEGER(i4), INTENT(IN OUT)  :: nit

REAL(sp)     :: wnext, a1, dt, et, ft, ut, g
INTEGER(i4)  :: i, ii, ind, inde, indn, indw, ir1, j, l

ir1 = ir - 1
e(1:ir) = zero
inde = 1

!        FOR NON-ZERO VALUES OF NIT, PERFORM QUICK RECURSIONS.

IF (nit == 0) THEN
  DO  i = 1, n
    wnext = w(i)
    
!        PREDICTION.
    
    IF (iupd /= 1 .OR. i /= 1) THEN
      
!        HERE DT = FT - 1.0
      
      dt = zero
      IF (ir /= 1) dt = p(ir+1)
      IF (dt < delta) GO TO 100
      a1 = a(1)
      IF (ir /= 1) THEN
        DO  j = 1, ir1
          a(j) = a(j+1)
        END DO
      END IF
      a(ir) = zero
      IF (ip /= 0) THEN
#if defined __INTEL_COMPILER || defined __ICC
!DIR$      VECTOR ALIGNED
!DIR$      SIMD VECTORLENGTHFOR(REAL(KIND=4))
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
!$OMP SIMD ALIGNED(a:64,phi:64)
#endif        
        DO  j = 1, ip
          a(j) = a(j) + phi(j) * a1
       END DO
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
!$OMP END SIMD
#endif       
      END IF
      ind = 0
      indn = ir
      DO  l = 1, ir
        DO  j = l, ir
          ind = ind + 1
          p(ind) = v(ind)
          IF (j /= ir) THEN
            indn = indn + 1
            p(ind) = p(ind) + p(indn)
          END IF
        END DO
      END DO
    END IF
    
!        UPDATING.
    
    ft = p(1)
    ut = wnext - a(1)
    IF (ir /= 1) THEN
      ind = ir
      DO  j = 2, ir
        g = p(j) / ft
        a(j) = a(j) + g * ut
        DO  l = j, ir
          ind = ind + 1
          p(ind) = p(ind) - g * p(l)
        END DO
      END DO
    END IF
    a(1) = wnext
    p(1:ir) = zero
    resid(i) = ut / sqrt(ft)
    e(inde) = resid(i)
    inde = inde + 1
    IF (inde > iq) inde = 1
    ssq = ssq + ut * ut / ft
    sumlog = sumlog + log(ft)
  END DO
  nit = n
  RETURN
END IF

!        QUICK RECURSIONS

i = 1
100 nit = i - 1
DO  ii = i, n
  et = w(ii)
  indw = ii
  IF (ip /= 0) THEN
#if defined __INTEL_COMPILER || defined __ICC
!DIR$      VECTOR ALIGNED
!DIR$      SIMD REDUCTION(-:et)
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
!$OMP SIMD REDUCTION(-:et) ALIGNED(w:64,phi:64)
#endif   
    DO  j = 1, ip
      indw = indw - 1
      IF (indw < 1) EXIT
      et = et - phi(j) * w(indw)
   END DO
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
!$OMP END SIMD
#endif     
  END IF
  IF (iq /= 0) THEN
#if defined __INTEL_COMPILER || defined __ICC
!DIR$      VECTOR ALIGNED
!DIR$      SIMD REDUCTION(-:et)
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
!$OMP SIMD REDUCTION(-:et) ALIGNED(theta:64,e:64)
#endif       
    DO  j = 1, iq
      inde = inde - 1
      IF (inde == 0) inde = iq
      et = et - theta(j) * e(inde)
   END DO
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
!$OMP END SIMD
#endif     
  END IF
  e(inde) = et
  resid(ii) = et
  ssq = ssq + et * et
  inde = inde + 1
  IF (inde > iq) inde = 1
END DO

RETURN
END SUBROUTINE karma


#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
SUBROUTINE kalfor(m,ip,ir,np,phi,a,p,v) !GCC$ ATTRIBUTES HOT :: kalfor  !GCC$ ATTRIBUTES ALIGNED(32) :: kalfor
#elif defined __INTEL_COMPILER || defined __ICC
SUBROUTINE kalfor(m,ip,ir,np,phi,a,p,v)
    !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: kalfor
    !DIR$ OPTIMIZE : 3
    !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: kalfor
#endif

! N.B. Argument WORK has been removed.

!  ALGORITHM AS 154.2  APPL. STATIST. (1980) VOL.29, P.311

!  INVOKING THIS SUBROUTINE OBTAINS PREDICTIONS OF A AND P, M STEPS AHEAD.

INTEGER(i4), INTENT(IN)      :: m
INTEGER(i4), INTENT(IN)      :: ip
INTEGER(i4), INTENT(IN)      :: ir
INTEGER(i4), INTENT(IN OUT)  :: np
REAL(sp), ALLOCATABLE, INTENT(IN)         :: phi(:)

REAL(sp), ALLOCATABLE, INTENT(IN OUT)     :: a(:)

REAL(sp), ALLOCATABLE, INTENT(IN OUT)     :: p(:)

REAL(sp), ALLOCATABLE, INTENT(IN)         :: v(:)

REAL(sp)     :: dt, a1, phii, phij, phijdt, t0
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
REAL(sp), dimension(ir) :: work !GCC$ ATTRIBUTES ALIGNED(64) ::  work
#elif defined __INTEL_COMPILER || defined __ICC
!DIR$ ATTRIBUTES ALIGN : 64 :: work
#endif
INTEGER(sp)  :: i, ind, ind1, ir1, j, l
t0 = 0.0_sp
ir1 = ir - 1
DO  l = 1, m
  
!        PREDICT A.
  
  a1 = a(1)
  IF (ir /= 1) THEN


    DO  i = 1, ir1
         t0 = a(i+1)
         a(i) = t0
    END DO
       
  END IF
  a(ir) = zero

  IF (ip /= 0) THEN

!$OMP SIMD REDUCTION(+:a) ALIGNED(a:64) \
!$OMP& ALIGNED(phi:64) LINEAR(j:1)
    DO  j = 1, ip
      a(j) = a(j) + phi(j) * a1
   END DO
 
  END IF
  
!        PREDICT P.
  
  work(1:ir) = p(1:ir)
  ind = 0
  ind1 = ir
  dt = p(1)
  DO  j = 1, ir
    phij = phi(j)
    phijdt = phij * dt
    DO  i = j, ir
      ind = ind + 1
      phii = phi(i)
      p(ind) = v(ind) + phii * phijdt
      IF (j < ir) p(ind) = p(ind) + work(j+1) * phii
      IF (i /= ir) THEN
        ind1 = ind1 + 1
        p(ind) = p(ind) + work(i+1) * phij + p(ind1)
      END IF
    END DO
  END DO
END DO

RETURN
END SUBROUTINE kalfor


#if defined (__GFORTRAN__) && !defined (__INTEL_COMPILER)
SUBROUTINE inclu2(np,weight,xnext,xrow,ynext,d,rbar,thetab,  &
                  ssqerr,recres,irank,ifault) !GCC$ ATTRIBUTES inline :: inclu2  !GCC$ ATTRIBUTES ALIGNED(32) :: inclu2
#elif defined (__INTEL_COMPILER) || defined (__ICC)
SUBROUTINE inclu2(np,weight,xnext,xrow,ynext,d,rbar,thetab,  &
                  ssqerr,recres,irank,ifault) 
      !DIR$ ATTRIBUTES FORCEINLINE :: inclu2
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: inclu2
      !DIR$ OPTIMIZE : 3
      !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: inclu2
#endif
! N.B. Argument NRBAR has been removed.

!   ALGORITHM AS 154.3  APPL. STATIST. (1980) VOL.29, P.311

!   FORTRAN VERSION OF REVISED VERSION OF ALGORITHM AS 75.1
!   APPL. STATIST. (1974) VOL.23, P.448
!   SEE REMARK AS R17 APPL. STATIST. (1976) VOL.25, P.323

INTEGER, INTENT(IN)   :: np
REAL, INTENT(IN)      :: weight
REAL, ALLOCATable, INTENT(IN)      :: xnext(:)
REAL, ALLOCATABLE, INTENT(OUT)     :: xrow(:)
REAL, INTENT(IN)      :: ynext
REAL, ALLOCATABLE, INTENT(IN OUT)  :: d(:)
REAL, ALLOCATABLE, INTENT(IN OUT)  :: rbar(:)
REAL, ALLOCATABLE, INTENT(IN OUT)  :: thetab(:)
REAL, INTENT(OUT)     :: ssqerr
REAL, INTENT(OUT)     :: recres
INTEGER, INTENT(OUT)  :: irank
INTEGER, INTENT(OUT)  :: ifault

REAL     :: wt, y, di, dpi, xi, xk, cbar, sbar, rbthis
INTEGER  :: i, i1, ithisr, k

!   INVOKING THIS SUBROUTINE UPDATES D, RBAR, THETAB, SSQERR
!   AND IRANK BY THE INCLUSION OF XNEXT AND YNEXT WITH A
!   SPECIFIED WEIGHT. THE VALUES OF XNEXT, YNEXT AND WEIGHT WILL
!   BE CONSERVED.  THE CORRESPONDING VALUE OF RECRES IS CALCULATED.

y = ynext
wt = weight
xrow(1:np) = xnext(1:np)
recres = zero
ifault = 1
IF (wt <= zero) RETURN
ifault = 0

ithisr = 0
DO  i = 1, np
  IF (xrow(i) == zero) THEN
    ithisr = ithisr + np - i
  ELSE
    xi = xrow(i)
    di = d(i)
    dpi = di + wt * xi * xi
    d(i) = dpi
    cbar = di / dpi
    sbar = wt * xi / dpi
    wt = cbar * wt
    IF (i /= np) THEN
      i1 = i + 1
   !$OMP SIMD ALIGNED(xrow:64) ALIGNED(rbar:64) LINEAR(k:1)
      DO  k = i1, np
        ithisr = ithisr + 1
        xk = xrow(k)
        rbthis = rbar(ithisr)
        xrow(k) = xk - xi * rbthis
        rbar(ithisr) = cbar * rbthis + sbar * xk
      END DO
    END IF
    xk = y
    y = xk - xi * thetab(i)
    thetab(i) = cbar * thetab(i) + sbar * xk
    IF (di == zero) GO TO 40
  END IF
END DO
ssqerr = ssqerr + wt * y * y
recres = y * sqrt(wt)
RETURN

40 irank = irank + 1
RETURN
END SUBROUTINE inclu2


#if defined (__GFORTRAN__) && !defined (__INTEL_COMPILER)
SUBROUTINE regres(np,nrbar,rbar,thetab,beta)  !GCC$ ATTRIBUTES inline :: regress  !GCC$ ATTRIBUTES ALIGNED(32) :: regress
#elif defined (__INTEL_COMPILER) || defined (__ICC)
SUBROUTINE regres(np,nrbar,rbar,thetab,beta)
      !DIR$ ATTRIBUTES FORCEINLINE :: regress
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: regress
      !DIR$ OPTIMIZE : 3
      !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: regress
#endif

!   ALGORITHM AS 154.4  APPL. STATIST. (1980) VOL.29, P.311

!   REVISED VERSION OF ALGORITHM AS 75.4
!   APPL. STATIST. (1974) VOL.23, P.448
!   INVOKING THIS SUBROUTINE OBTAINS BETA BY BACKSUBSTITUTION
!   IN THE TRIANGULAR SYSTEM RBAR AND THETAB.

INTEGER, INTENT(IN)  :: np
INTEGER, INTENT(IN)  :: nrbar
REAL, ALLOCATABLE, INTENT(IN)     :: rbar(:)
REAL, ALLOCATABLE, INTENT(IN)     :: thetab(:)
REAL, ALLOCATABLE, INTENT(OUT)    :: beta(:)

REAL     :: bi
INTEGER  :: i, i1, im, ithisr, j, jm

ithisr = nrbar
im = np
DO  i = 1, np
  bi = thetab(im)
  IF (im /= np) THEN
    i1 = i - 1
    jm = np
!$OMP SIMD REDUCTION(-:bi) ALIGNED(rbar:64) ALIGNED(beta:64) \
!$OMP& LINEAR(j:1)
    DO  j = 1, i1
      bi = bi - rbar(ithisr) * beta(jm)
      ithisr = ithisr - 1
      jm = jm - 1
    END DO
  END IF
  beta(im) = bi
  im = im - 1
END DO

RETURN
END SUBROUTINE regres


#if defined (__GFORTRAN__) && !defined (__INTEL_COMPILER)
SUBROUTINE forkal(ip, iq, ir, np, ird, irz, id, il, n, nrbar, phi, theta,  &
                  delta, w, y, amse, a, p, v, resid, e, xnext, xrow,  &
                  rbar, thetab, store, ifault) !GCC$ ATTRIBUTES hot :: forkal  !GCC$ ATTRIBUTES ALIGNED(32) :: forkal
#elif defined (__INTEL_COMPILER) || defined (__ICC)
SUBROUTINE forkal(ip, iq, ir, np, ird, irz, id, il, n, nrbar, phi, theta,  &
                  delta, w, y, amse, a, p, v, resid, e, xnext, xrow,  &
                  rbar, thetab, store, ifault) 
     !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: forkal
      !DIR$ OPTIMIZE : 3
      !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: forkal
#endif
 
!  ALGORITHM AS 182  APPL. STATIST. (1982) VOL.31, NO.2

!  Finite sample prediction from ARIMA processes.

INTEGER, INTENT(IN)   :: ip
INTEGER, INTENT(IN)   :: iq
INTEGER, INTENT(IN)   :: ir
INTEGER, INTENT(IN)   :: np
INTEGER, INTENT(IN)   :: ird
INTEGER, INTENT(IN)   :: irz
INTEGER, INTENT(IN)   :: id
INTEGER, INTENT(IN)   :: il
INTEGER, INTENT(IN)   :: n
INTEGER, INTENT(IN)   :: nrbar
REAL, ALLOCATABLE, INTENT(OUT)     :: phi(:)
REAL, ALLOCATABLE, INTENT(IN)      :: theta(:)
REAL, ALLOCATABLE, INTENT(IN)      :: delta(:)
REAL, ALLOCATABLE, INTENT(IN OUT)  :: w(:)
REAL, ALLOCATABLE, INTENT(OUT)     :: y(:)
REAL, ALLOCATABLE, INTENT(OUT)     :: amse(:)
REAL, ALLOCATABLE, INTENT(OUT)     :: a(:)
REAL, ALLOCATABLE, INTENT(OUT)     :: p(:)
REAL, ALLOCATABLE, INTENT(OUT)     :: v(:)
REAL, ALLOCATABLE, INTENT(OUT)     :: resid(:)
REAL, ALLOCATABLE, INTENT(OUT)     :: e(:)
REAL, ALLOCATABLE, INTENT(OUT)     :: xnext(:)
REAL, ALLOCATABLE, INTENT(OUT)     :: xrow(:)
REAL, ALLOCATABLE, INTENT(OUT)     :: rbar(:)
REAL, ALLOCATABLE, INTENT(OUT)     :: thetab(:)
REAL, ALLOCATABLE, INTENT(OUT)     :: store(:)
INTEGER, INTENT(OUT)  :: ifault

!     Invoking this routine will calculate the finite sample predictions
!     and their conditional mean square errors for any ARIMA process.

INTEGER  :: i, i45, ibc, id1, id2r, id2r1, id2r2, idd1, idd2, iddr, idrr1, &
            idk, iid, ind, ind1, ind2, iq1, ir1, ir2, iri, iri1, irj, iupd, &
            j, j1, jj, jkl, jkl1, jklj, jrj, jrk, k, k1, kk, kk1, kkk, l, &
            lk, lk1, ll, lli, nit, nj, nt
REAL     :: a1, aa, ams, del, dt, phii, phij, phijdt, sigma, ssq, sumlog

!     Check for input faults.

ifault = 0
IF (ip < 0) ifault = 1
IF (iq < 0) ifault = ifault + 2
IF (ip * ip + iq * iq == 0) ifault = 4
k = iq + 1
IF (k < ip) k = ip
IF (ir /= k) ifault = 5
IF (np /= ir * (ir + 1) / 2) ifault = 6
IF (nrbar /= np * (np - 1) / 2) ifault = 7
IF (id < 0) ifault = 8
IF (ird /= ir + id) ifault = 9
IF (irz /= ird * (ird + 1) / 2) ifault = 10
IF (il < 1) ifault = 11
IF (ifault /= 0) RETURN

!     Calculate initial conditions for Kalman filter

a(1) = zero
v(1) = one
IF (np == 1) GO TO 130
v(2:np) = zero
IF (iq == 0) GO TO 130
iq1 = iq + 1
!$OMP SIMD ALIGNED(v:64) ALIGNED(theta:64) \
!$OMP& LINEAR(i:1)
DO  i = 2, iq1
  v(i) = theta(i-1)
END DO
DO  j = 1, iq
  ll = j * (2*ir + 1 - j) / 2
  DO  i = j, iq
    lli = ll + i
    v(lli) = theta(i) * theta(j)
  END DO
END DO

!     Find initial likelihood conditions.
!     IFAULT not tested on exit from STARMA as all possible errors
!     have been checked above.

130 IF (ir == 1) p(1) = one / (one - phi(1) * phi(1))
IF (ir /= 1) CALL starma(ip, iq, ir, np, phi, theta, a, p, v,  &
                         thetab, xnext, xrow, rbar, nrbar, ifault)

!     Calculate data transformations

nt = n - id
IF (id == 0) GO TO 170
!$OMP SIMD ALIGNED(store:64) ALIGNED(w:64) LINEAR(j:1)
DO  j = 1, id
  nj = n - j
  store(j) = w(nj)
END DO
DO  i = 1, nt
  aa = zero
 !$OMP SIMD REDUCTION(-:aa) ALIGNED(delta:64) \
 !$OMP& ALIGNED(w:64) LINEAR(k:1)
  DO  k = 1, id
    idk = id + i - k
    aa = aa - delta(k) * w(idk)
  END DO
  iid = i + id
  w(i) = w(iid) + aa
END DO

!     Evaluate likelihood to obtain final KF conditions

170 sumlog = zero
ssq = zero
iupd = 1
del = - one
nit = 0
CALL karma(ip, iq, ir, phi, theta, a, p, v, nt, w, resid,  &
           sumlog, ssq, iupd, del, e, nit)

!     Calculate M.L.E. of sigma squared

sigma = zero
!$OMP SIMD REDUCTION(+:sigma) ALIGNED(resid:64) \
!$OMP& LINEAR(j:1)
DO  j = 1, nt
  sigma = sigma + resid(j)*resid(j)
END DO
sigma = sigma / nt

!     Reset the initial A and P when differencing occurs

IF (id == 0) GO TO 250
!$OMP SIMD ALIGNED(xrow:64) ALIGNED(p:64) LINEAR(i:1)
DO  i = 1, np
  xrow(i) = p(i)
END DO
!$OMP SIMD ALIGNED(p:64) LINEAR(j:1)
DO  i = 1, irz
  p(i) = zero
END DO
ind = 0
DO  j = 1, ir
  k = (j-1) * (id + ir + 1) - (j-1) * j / 2
  DO  i = j, ir
    ind = ind + 1
    k = k + 1
    p(k) = xrow(ind)
  END DO
END DO
!$OMP SIMD ALIGNED(a:64) ALIGNED(p:64) ALIGNED(store:64) LINEAR(j:1)
DO  j = 1, id
  irj = ir + j
  a(irj) = store(j)
END DO

!     Set up constants

250 ir2 = ir + 1
ir1 = ir - 1
id1 = id - 1
id2r = 2 * ird
id2r1 = id2r - 1
idd1 = 2 * id + 1
idd2 = idd1 + 1
i45 = id2r + 1
idrr1 = ird + 1
iddr = 2 * id + ir
jkl = ir * (iddr + 1) / 2
jkl1 = jkl + 1
id2r2 = id2r + 2
ibc = ir * (i45 - ir) / 2
DO  l = 1, il
  
!     Predict A
  
  a1 = a(1)
  IF (ir == 1) GO TO 310
  DO  i = 1, ir1
    a(i) = a(i+1)
  END DO
  310 a(ir) = zero
  IF (ip == 0) GO TO 330
  !$OMP SIMD REDUCTION(+:a) ALIGNED(a:64) \
  !$OMP& ALIGNED(phi:64) LINEAR(j:1)
  DO  j = 1, ip
    a(j) = a(j) + phi(j) * a1
  END DO
  330 IF (id == 0) GO TO 360
  !$OMP SIMD REDUCTION(+:a1) ALIGNED(delta:64) \
  !$OMP& ALIGNED(a:64) LINEAR(j:1)
  DO  j = 1, id
    irj = ir + j
    a1 = a1 + delta(j) * a(irj)
  END DO
  IF (id < 2) GO TO 360
  DO  i = 1, id1
    iri1 = ird - i
    a(iri1 + 1) = a(iri1)
  END DO
  360 a(ir2) = a1
  
!     Predict P
  
  IF (id == 0) GO TO 480
  DO  i = 1, id
    store(i) = zero
  !$OMP SIMD REDUCTION(+:store) ALIGNED(store:64) \
  !$OMP& ALIGNED(delta:64) ALIGNED(p:64) LINEAR(j:1)
    DO  j = 1, id
      ll = MAX(i,j)
      k = MIN(i,j)
      jj = jkl + (ll - k) + 1 + (k-1) * (idd2 - k) / 2
      store(i) = store(i) + delta(j) * p(jj)
    END DO
  END DO

  DO  j = 1, id1
    jj = id - j
    lk = (jj-1) * (idd2 - jj) / 2 + jkl
    lk1 = jj * (idd1 - jj) / 2 + jkl
    DO  i = 1, j
      lk = lk + 1
      lk1 = lk1 + 1
      p(lk1) = p(lk)
    END DO
  END DO
  DO  j = 1, id1
    jklj = jkl1 + j
    irj = ir + j
    p(jklj) = store(j) + p(irj)
  END DO

  p(jkl1) = p(1)
  !$OMP SIMD REDUCTION(+:p) ALIGNED(p:64) \
  !$OMP& ALIGNED(delta:64) ALIGNED(store:64) LINEAR(i:1)
  DO  i = 1, id
    iri = ir + i
    p(jkl1) = p(jkl1) + delta(i) * (store(i) + two * p(iri))
  END DO
  DO  i = 1, id
    iri = ir + i
    store(i) = p(iri)
  END DO
  DO  j = 1, ir
    kk1 = j * (id2r1 - j) / 2 + ir
    k1 = (j-1) * (id2r - j) / 2 + ir
    DO  i = 1, id
      kk = kk1 + i
      k = k1 + i
      p(k) = phi(j) * store(i)
      IF (j /= ir) p(k) = p(k) + p(kk)
    END DO
  END DO
  
  DO  j = 1, ir
    store(j) = zero
    kkk = j * (i45 - j) / 2 - id
 !$OMP SIMD REDUCTION(+:store) ALIGNED(store:64) \
  !$OMP& ALIGNED(delta:64) ALIGNED(p:64) LINEAR(i:1)   
    DO  i = 1, id
      kkk = kkk + 1
      store(j) = store(j) + delta(i) * p(kkk)
    END DO
  END DO
  IF (id == 1) GO TO 460
  DO  j = 1, ir
    k = j * idrr1 - j * (j+1) / 2 + 1
    DO  i = 1, id1
      k = k - 1
      p(k) = p(k-1)
    END DO
  END DO
  460 DO  j = 1, ir
    k = (j-1) * (id2r - j) / 2 + ir + 1
    p(k) = store(j) + phi(j) * p(1)
    IF (j < ir) p(k) = p(k) + p(j+1)
  END DO
  480 store(1:ir) = p(1:ir)
  
  ind = 0
  dt = p(1)
  DO  j = 1, ir
    phij = phi(j)
    phijdt = phij * dt
    ind2 = (j-1) * (id2r2 - j) / 2
    ind1 = j * (i45 - j) / 2
    DO  i = j, ir
      ind = ind + 1
      ind2 = ind2 + 1
      phii = phi(i)
      p(ind2) = v(ind) + phii * phijdt
      IF (j < ir) p(ind2) = p(ind2) + store(j+1) * phii
      IF (i == ir) CYCLE
      ind1 = ind1 + 1
      p(ind2) = p(ind2) + store(i+1) * phij + p(ind1)
    END DO
  END DO
  
!     Predict Y
  
  y(l) = a(1)
  IF (id == 0) GO TO 520
  !$OMP SIMD REDUCTION(+:y) ALIGNED(a:64) \
  !$OMP& ALIGNED(y:64) LINEAR(j:1)
  DO  j = 1, id
    irj = ir + j
    y(l) = y(l) + a(irj) * delta(j)
  END DO
  
!     Calculate M.S.E. of Y
  
  520 ams = p(1)
  DO  j = 1, id
    jrj = ibc + (j-1) * (idd2 - j) / 2
    irj = ir + j
    ams = ams + two * delta(j) * p(irj) + p(jrj+1) * delta(j)**2
  END DO
  DO  j = 1, id1
    j1 = j + 1
    jrk = ibc + 1 + (j-1) * (idd2 - j) / 2
 !$OMP SIMD REDUCTION(+:ams) ALIGNED(delta:64) \
  !$OMP& ALIGNED(p:64) LINEAR(j:1)   
    DO  i = j1, id
      jrk = jrk + 1
      ams = ams + two * delta(i) * delta(j) * p(jrk)
    END DO
  END DO
  amse(l) = ams * sigma
END DO

RETURN
END SUBROUTINE forkal

END MODULE GMS_mod_arma_estimation
