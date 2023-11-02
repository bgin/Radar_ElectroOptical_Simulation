MODULE FFT_chirp_transform
! Module for the Fast Fourier transform of series of arbitrary length,
! using the CHIRP-Z transform.
! Adapted from Applied Statistics algorithms AS 117 and AS 83.
! Monro, D.M. & Branch, J.L. (1977) `The Chirp discrete Fourier transform of
! general length', Appl. Statist., 26 (3), 351-361.

! Translated to be compatible with Lahey's ELF90 compiler by Alan Miller
! amiller@bigpond.net.au    http://users.bigpond.net.au/amiller/
! Latest revision - 3 December 2002

IMPLICIT NONE
PRIVATE
PUBLIC  :: chrft, chfor, chrev, fastf, fastg, setwt

CONTAINS


#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
  SUBROUTINE chrft(x, y, wr, wi, isize, lwork, itype, ifault) !GCC$ ATTRIBUTES HOT :: chrft !GCC$ ATTRIBUTES aligned(32) :: chrft
#elif defined __ICC || defined __INTEL_COMPILER
  SUBROUTINE chrft(x, y, wr, wi, isize, lwork, itype, ifault)
    !DIR$ ATTRIBUTES ALIGN : 32 :: chrft  
#endif
  use omp_lib

!  Algorithm AS 117.1 Appl. Statist. (1977) vol.26, no.3

!  Code for complex discrete Fourier transform of a sequence of general
!  length by the Chirp-Z transform

REAL(kind=4), INTENT(IN OUT) :: x(:), y(:)
REAL(kind=4), INTENT(IN)     :: wr(:), wi(:)
INTEGER(kind=4), INTENT(IN)  :: isize, lwork, itype
INTEGER(kind=4), INTENT(OUT) :: ifault
#if defined __ICC || defined __INTEL_COMPILER
!DIR$ ASSUME_ALIGNED x:64,y:64
!DIR$ ASSUME_ALIGNED wr:64,wi:64
#endif
INTEGER(kind=4)         :: ii, k, np1, nn
REAL(kind=4), PARAMETER :: zero = 0.0_4, one = 1.0_4
REAL(kind=4)            :: xwi, z, gc

!       Check that ISIZE and LWORK are valid

ifault = 0
IF (isize < 3) ifault = 1
ii = 4
DO k = 3, 20
  ii = ii * 2
  IF (ii == lwork) GO TO 3
  IF (ii > lwork) GO TO 2
END DO

2 ifault = ifault + 2
3 IF (lwork < 2 * isize) ifault = ifault + 4
IF (itype == 0) ifault = ifault + 8
IF (ifault /= 0) RETURN

!       Multiply by the Chirp function in the time domain

CALL chrp(x, y, one, isize, itype)
np1 = isize + 1
#if defined __ICC || defined __INTEL_COMPILER
!DIR$ VECTOR ALIGNED
!DIR$ VECTOR ALWAYS
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
!$OMP SIMD ALIGNED(x:64,y:64)
#endif
DO nn = np1, lwork
  x(nn) = zero
  y(nn) = zero
END DO

!       Fourier transform the chirped series

CALL fastf(x, y, lwork, 1)

!       Convolve by frequency domain multiplication with (wr, wi)
!$OMP PARALLEL DO PRIVATE(nn,xwi,z) SHARED(wi,x,wr,y)  SCHEDULE(STATIC)  IF(nn>=2048)
DO nn = 1, lwork
  xwi = wi(nn)
  IF (itype < 0) xwi = -xwi
  z = x(nn) * wr(nn) - y(nn) * xwi
  y(nn) = x(nn) * xwi + y(nn) * wr(nn)
  x(nn) = z
END DO
!$OMP END PARALLEL 

!       Inverse Fourier transform

CALL fastf(x, y, lwork, -1)

!       Multiply by Chirp function & gain correction

gc = lwork
IF (itype > 0) gc = gc/isize
CALL chrp(x, y, gc, isize, itype)

RETURN
END SUBROUTINE chrft


#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
SUBROUTINE chfor(xray, wr, wi, jsize, lwork, mwork, ifault) !GCC$ ATTRIBUTES HOT :: chfor !GCC$ ATTRIBUTES aligned(32) :: chfor
#elif defined __ICC || defined __INTEL_COMPILER
  SUBROUTINE chfor(xray, wr, wi, jsize, lwork, mwork, ifault)
    !DIR$ ATTRIBUTES ALIGN : 32 :: chfor
#endif

   use omp_lib
!       Algorithm AS 117.2 Appl. Statist. (1977) vol.26, no.3

!       Forward Chirp DFT for real even length input sequence

REAL(kind=4), INTENT(IN)     :: wr(:), wi(:)
REAL(kind=4), INTENT(IN OUT) :: xray(:)
#if defined __ICC || defined __INTEL_COMPILER
!DIR$ ASSUME_ALIGNED wr:64,wi:64
!DIR$ ASSUME_ALIGNED xray:64
#endif
INTEGER(kind=4), INTENT(IN)  :: jsize, lwork, mwork
INTEGER(kind=4), INTENT(OUT) :: ifault

INTEGER(kind=4)         :: ii, k, nn, ll, i, m, nnn, mm, j, jj
REAL(kind=4), PARAMETER :: zero = 0.0_4, quart = 0.25_4, half = 0.5_4, one = 1.0_4,  &
                   one5 = 1.5_4, two = 2.0_4, four = 4.0_4
REAL(kind=4)            :: z, bcos, bsin, un, vn, save1, an, bn, cn, dn

!       Check for valid JSIZE, LWORK & MWORK

ifault = 0
ii = 8
DO k = 4, 21
  ii = ii * 2
  IF (ii == lwork) GO TO 3
  IF (ii > lwork) GO TO 2
END DO
2 ifault = 2
3 IF (lwork < 2 * jsize) ifault = ifault + 4
IF (lwork /= 2 * mwork) ifault = ifault + 16
nn = jsize / 2
IF (2 * nn /= jsize) ifault = ifault + 32
IF (nn < 3) ifault = ifault + 1
IF (ifault /= 0) RETURN
ll = mwork

!       Split XRAY into even and odd sequences of length n/2 placing
!       the even terms in the bottom half of XRAY as dummy real terms
!       and odd terms in the top half as dummy imaginary terms.

DO i = 1, nn
  ii = 2 * i
  m = ll + i
  xray(m) = xray(ii)
  xray(i) = xray(ii-1)
END DO

!       Perform forward Chirp DFT on even and odd sequences.

CALL chrft(xray, xray(ll+1:), wr, wi, nn, ll, 1, ifault)

!       Reorder the results according to output format.
!       First, the unique real terms.

z = half * (xray(1) + xray(ll+1))
xray(nn+1) = half * (xray(1) - xray(ll+1))
xray(1) = z

!       If nn is even, terms at nn/2 + 1 and 3*nn/2 can be calculated

nnn = nn / 2
IF (nn /= 2 * nnn) GO TO 5
m = nnn + 1
xray(m) = half * xray(m)
mm = m + ll
m = m + nn
xray(m) = -half * xray(mm)
GO TO 6
5 nnn = nnn + 1

!       Set up trig functions for calculations of remaining non-unique terms

6 z = four * ATAN(one) / nn
bcos = -two * (SIN(z/two) ** 2)
bsin = SIN(z)
un = one
vn = zero

!       Calculate & place remaining terms in correct locations.
!!$OMP PARALLEL DO PRIVATE(i,z,vn,save1,un,vn,ii,m,mm,an,bn,cn,dn,j,jj) SHARED(xray,bcos,acos,quart,nn,ll)  SCHEDULE(STATIC)  IF(i>=2048)
!!$OMP PARALLEL DO SCHEDULE(STATIC) IF(i>=2048)
DO i = 2, nnn
  z = un * bcos + un + vn * bsin
  vn = vn * bcos + vn - un * bsin
  save1 = one5 - half * (z * z + vn * vn)
  un = z * save1
  vn = vn * save1
  ii = nn + 2 - i
  m = ll + i
  mm = ll + ii
  an = quart * (xray(i) + xray(ii))
  bn = quart * (xray(m) - xray(mm))
  cn = quart * (xray(m) + xray(mm))
  dn = quart * (xray(i) - xray(ii))
  xray(i) = an + un * cn + vn * dn
  xray(ii) = two * an - xray(i)
  j = nn + i
  jj = nn + ii
  xray(j) = bn - un * dn + vn * cn
  xray(jj) = xray(j) - two * bn
END DO

RETURN
END SUBROUTINE chfor


#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
SUBROUTINE chrev(xray, wr, wi, jsize, lwork, mwork, ifault) !GCC$ ATTRIBUTES hot :: chrev !GCC$ ATTRIBUTES aligned(32) :: chrev
#elif defined __ICC || defined __INTEL_COMPILER
  SUBROUTINE chrev(xray, wr, wi, jsize, lwork, mwork, ifault)
    !DIR$ ATTRIBUTES ALIGN : 32 :: chrev
#endif
     use omp_lib
!       Algorithm AS 117.3 Appl. Statist. (1977) vol.26, no.3

!       Inverse Chirp DFT to give real even length sequence

REAL(kind=4), INTENT(IN OUT) :: xray(:)
REAL(kind=4), INTENT(IN)     :: wr(:), wi(:)
#if defined __ICC || defined __INTEL_COMPILER
!DIR$ ASSUME_ALIGNED xray:64,wr:64,wi:64
#endif
INTEGER(kind=4), INTENT(IN)  :: jsize, lwork, mwork
INTEGER(kind=4), INTENT(OUT) :: ifault

INTEGER(kind=4)        :: ii, k, nn, ll, nnn, m, mm, i, j, jj
REAL(kind=4), PARAMETER :: zero = 0.0_4, half = 0.5_4, one = 1.0_4, one5 = 1.5_4, two = 2.0_4,  &
                   four = 4.0_4
REAL(kind=4)            :: z, bcos, bsin, un, vn, save1, an, bn, pn, qn, cn, dn

!       Check for valid JSIZE, LWORK & MWORK

ifault = 0
ii = 8
DO k = 4, 21
  ii = ii * 2
  IF (ii == lwork) GO TO 3
  IF (ii > lwork) GO TO 2
END DO
2 ifault = 2
3 IF (lwork < 2 * jsize) ifault = ifault + 4
IF (lwork /= 2 * mwork) ifault = ifault + 16
nn = jsize / 2
IF (2 * nn /= jsize) ifault = ifault + 32
IF (nn < 3) ifault = ifault + 1
IF (ifault /= 0) RETURN
ll = mwork

!       Reorder the spectrum; first the unique terms.

z = xray(1) + xray(nn+1)
xray(ll+1) = xray(1) - xray(nn+1)
xray(1) = z

!       If nn is even then terms nn/2 + 1 and 3*nn/2 + 1 must be reordered.

nnn = nn / 2
IF (nn /= 2 * nnn) GO TO 4
m = nnn + 1
xray(m) = two * xray(m)
mm = m + ll
m = m + nn
xray(mm) = -two * xray(m)
GO TO 5
4 nnn = nnn + 1

!       Set up trig functions for manipulation of remaining terms.

5 z = four * ATAN(one) / nn
bcos = -two * (SIN(z/two) ** 2)
bsin = SIN(z)
un = one
vn = zero

!       Perform manipulation and reordering of remaining non-unique terms.
!!$OMP PARALLEL DO PRIVATE(i,z,vn,save1,un,vn,ii,m,mm,an,bn,cn,dn,j,jj) SHARED(xray,bcos,acos,quart,nn,ll,one5,half,pn,qn)  SCHEDULE(STATIC)  IF(nn>2048)
!!$OMP PARALLEL DO SCHEDULE(STATIC) IF(i>=2048)
DO i = 2, nnn
  z = un * bcos + un + vn * bsin
  vn = vn * bcos + vn - un * bsin
  save1 = one5 - half * (z * z + vn * vn)
  un = z * save1
  vn = vn * save1
  ii = nn + 2 - i
  j = nn + i
  jj = nn + ii
  an = xray(i) + xray(ii)
  bn = xray(j) - xray(jj)
  pn = xray(i) - xray(ii)
  qn = xray(j) + xray(jj)
  cn = un * pn + vn * qn
  dn = vn * pn - un * qn
  xray(i) = an + dn
  xray(ii) = an - dn
  m = ll + i
  mm = ll + ii
  xray(m) = cn + bn
  xray(mm) = cn - bn
END DO

!       Do inverse Chirp DFT to give even and odd sequences.

CALL chrft(xray, xray(ll+1:), wr, wi, nn, ll, -1, ifault)

!       Interlace the results to produce the required output sequence.

nnn = nn + 1
ii = jsize + 2
!!$OMP PARALLEL DO PRIVATE(i,m,mm,j,jj) SHARED(xray,ll)  SCHEDULE(STATIC)  IF(i>=2048)
DO i = 1, nn
  j = nnn - i
  jj = ll + j
  m = ii - 2 * i
  mm = m - 1
  xray(m) = xray(jj)
  xray(mm) = xray(j)
END DO

RETURN
END SUBROUTINE chrev


#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
SUBROUTINE setwt(wr, wi, ksize, kwork, ifault) !GCC$ ATTRIBUTES HOT :: setwt !GCC$ ATTRIBUTES aligned(32) :: setwt
#elif defined __ICC || defined __INTEL_COMPILER
  SUBROUTINE setwt(wr, wi, ksize, kwork, ifault)
    !DIR$ ATTRIBUTES ALIGN : 32 :: setwt
#endif
    use omp_lib
!       Algorithm AS 117.4 Appl. Statist. (1977) vol.26, no.3

!       Subroutine to set up Fourier transformed Chirp function for
!       use by subroutine CHRFT.

REAL(kind=4), INTENT(OUT)    :: wr(:), wi(:)
#if defined __ICC || defined __INTEL_COMPILER
!DIR$ ASSUME_ALIGNED wr:64,wi:64
#endif
INTEGER(kind=4), INTENT(IN)  :: ksize, kwork
INTEGER(kind=4), INTENT(OUT) :: ifault

INTEGER(kind=4)        :: ii, k, nn, ll
REAL(kind=4), PARAMETER :: zero = 0.0_4, one = 1.0_4, four = 4.0_4
REAL(kind=4)            :: tc, z

!       Check that KSIZE & KWORK are valid

ifault = 0
IF (ksize < 3) ifault = 1
ii = 4
DO k = 3, 20
  ii = ii * 2
  IF (ii == kwork) GO TO 3
  IF (ii > kwork) GO TO 2
END DO
2 ifault = 2
3 IF (kwork < 2 * ksize) ifault = ifault + 4
IF (ifault /= 0) RETURN
tc = four * ATAN(one) / ksize

!       Set up bottom segment of Chirp function
!$OMP PARARLLEL DO PRIVATE(nn,z) SHARED(wr,wi) SCHEDULE(STATIC) IF(nn>=2048)
DO nn = 1, ksize
  z = nn - 1
  z = z * z * tc
  wr(nn) = COS(z)
  wi(nn) = SIN(z)
END DO

!       Clear the rest

DO nn = ksize+1, kwork
  wr(nn) = zero
  wi(nn) = zero
END DO

!       Copy to the top segment

DO nn = kwork-ksize+2, kwork
  ll = kwork - nn + 2
  wr(nn) = wr(ll)
  wi(nn) = wi(ll)
END DO

!       Fourier transform the Chirp function

CALL fastf(wr, wi, kwork, 1)

RETURN
END SUBROUTINE setwt

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
SUBROUTINE chrp(x, y, gc, isize, itype)             !GCC$ ATTRIBUTES HOT :: chrp !GCC$ ATTRIBUTES aligned(32) :: chrp !GCC$ ATTRIBUTES inline :: chrp
#elif defined __ICC || defined __INTEL_COMPILER
  SUBROUTINE chrp(x, y, gc, isize, itype)
    !DIR$ ATTRIBUTES INLINE :: chrp
    !DIR$ ATTRIBUTES ALIGN : 32 :: chrp
#endif
   use omp_lib
!       Algorithm AS 117.5 Appl. Statist. (1977) vol.26, no.3

!       Subroutine to multiply time series by Chirp function

REAL(kind=4), INTENT(IN OUT) :: x(:), y(:)
#if defined __ICC || defined __INTEL_COMPILER
!DIR$ ASSUME_ALIGNED x:64,y:64
#endif
REAL(kind=4), INTENT(IN)     :: gc
INTEGER(kind=4), INTENT(IN)  :: isize, itype

INTEGER(kind=4)         :: nn
REAL(kind=4), PARAMETER :: one = 1.0_4, four = 4.0_4
REAL(kind=4)            :: tc, z, xwr, xwi

tc = four * ATAN(one) / isize
!!$OMP PARALLEL DO SHEDULE(STATIC) IF(nn>=2048)
DO nn = 1, isize
  z = nn - 1
  z = z * z * tc
  xwr = COS(z)
  xwi = -SIN(z)
  IF (itype < 0) xwi = -xwi
  z = x(nn) * xwr - y(nn) * xwi
  y(nn) = (x(nn) * xwi + y(nn) * xwr) * gc
  x(nn) = z * gc
END DO

RETURN
END SUBROUTINE chrp

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
SUBROUTINE fastf(xreal, ximag, isize, itype)  !GCC$ ATTRIBUTES HOT :: fastf !GCC$ ATTRIBUTES aligned(32) :: fastf 
#elif defined __ICC || defined __INTEL_COMPILER
  SUBROUTINE fastf(xreal, ximag, isize, itype)
    !DIR$ ATTRIBUTES ALIGN : 32 :: fastf
   
#endif
!       Algorithm AS 83.1 Appl. Statist. (1975) vol.24, no.1

!       Radix 4 complex discrete fast Fourier transform with
!       unscrambling of the transformed arrays.

REAL(kind=4), INTENT(IN OUT)  :: xreal(:), ximag(:)
INTEGER(kind=4), INTENT(IN)   :: isize, itype

INTEGER(kind=4) :: ii, k

!       Check for valid transform size.

ii = 4
DO k = 2, 20
  IF (ii == isize) GO TO 4
  IF (ii > isize) EXIT
  ii = ii * 2
END DO

!       If this point is reached a size error has occurred.

RETURN

!       Call FASTG to perform the transform.

4 CALL fastg(xreal, ximag, isize, itype)

!       Call SCRAM to unscramble the results.

CALL scram(xreal, ximag, k)

RETURN
END SUBROUTINE fastf


#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
SUBROUTINE fastg(xreal, ximag, n, itype) !GCC$ ATTRIBUTES HOT :: fastg !GCC$ ATTRIBUTES aligned(32) :: fastg !GCC$ ATTRIBUTES INLINE :: fastg
#elif defined __ICC || defined __INTEL_COMPILER
  SUBROUTINE fastg(xreal, ximag, n, itype)
    !DIR$ ATTRIBUTES ALIGN : 32 :: fastg
    !DIR$ ATTRIBUTES INLINE :: fastg
#endif
    use omp_lib

!       Algorithm AS 83.2 Appl. Statist. (1975) vol.24, no.1

!       Radix 4 complex discrete fast Fourier transform without unscrambling,
!       suitable for convolutions or other applications which do not require
!       unscrambling.   Called by subroutine FASTF which also does the
!       unscrambling.

  REAL(kind=4), INTENT(IN OUT) :: xreal(:), ximag(:)
#if defined __ICC || defined __INTEL_COMPILER
  !DIR$ ASSUME_ALIGNED xreal:64,ximag:64
#endif
INTEGER(kind=4), INTENT(IN)  :: n, itype

INTEGER(kind=4)         :: ifaca, k, ifcab, litla, i0, i1, i2, i3
REAL(kind=4), PARAMETER :: zero = 0.0_4, half = 0.5_4, one = 1.0_4, one5 = 1.5_4, two = 2.0_4, &
                   four = 4.0_4
REAL(kind=4)            :: pi, z, bcos, bsin, cw1, sw1, xs0, xs1, ys0, ys1, xs2, xs3, &
                   ys2, ys3, x1, y1, x2, y2, x3, y3, tempr, cw2, sw2, cw3, sw3

pi = four * ATAN(one)
ifaca = n / 4
IF (itype == 0) RETURN
IF (itype > 0) GO TO 5

!       ITYPE < 0 indicates inverse transform required.
!       Calculate conjugate.

ximag(1:n) = -ximag(1:n)

!       Following code is executed for IFACA = N/4, N/16, N/64, ...
!       until IFACA <= 1.

5 ifcab = ifaca * 4
z = pi / ifcab
bcos = -two * SIN(z)**2
bsin = SIN(two * z)
cw1 = one
sw1 = zero
!!$OMP PARALLEL DO SCHEDULE(STATIC) IF(litla>=512)
DO litla = 1, ifaca
  DO i0 = litla, n, ifcab
    i1 = i0 + ifaca
    i2 = i1 + ifaca
    i3 = i2 + ifaca
    xs0 = xreal(i0) + xreal(i2)
    xs1 = xreal(i0) - xreal(i2)
    ys0 = ximag(i0) + ximag(i2)
    ys1 = ximag(i0) - ximag(i2)
    xs2 = xreal(i1) + xreal(i3)
    xs3 = xreal(i1) - xreal(i3)
    ys2 = ximag(i1) + ximag(i3)
    ys3 = ximag(i1) - ximag(i3)
    xreal(i0) = xs0 + xs2
    ximag(i0) = ys0 + ys2
    x1 = xs1 + ys3
    y1 = ys1 - xs3
    x2 = xs0 - xs2
    y2 = ys0 - ys2
    x3 = xs1 - ys3
    y3 = ys1 + xs3
    IF (litla == 1) THEN
      xreal(i2) = x1
      ximag(i2) = y1
      xreal(i1) = x2
      ximag(i1) = y2
      xreal(i3) = x3
      ximag(i3) = y3
    ELSE
      xreal(i2) = x1 * cw1 + y1 * sw1
      ximag(i2) = y1 * cw1 - x1 * sw1
      xreal(i1) = x2 * cw2 + y2 * sw2
      ximag(i1) = y2 * cw2 - x2 * sw2
      xreal(i3) = x3 * cw3 + y3 * sw3
      ximag(i3) = y3 * cw3 - x3 * sw3
    END IF
 END DO

  
!       Calculate a new set of twiddle factors.
  
  IF (litla < ifaca) THEN
    z = cw1 * bcos - sw1 * bsin + cw1
    sw1 = bcos * sw1 + bsin * cw1 + sw1
    tempr = one5 - half * (z * z + sw1 * sw1)
    cw1 = z * tempr
    sw1 = sw1 * tempr
    cw2 = cw1 * cw1 - sw1 * sw1
    sw2 = two * cw1 * sw1
    cw3 = cw1 * cw2 - sw1 * sw2
    sw3 = cw1 * sw2 + cw2 * sw1
  END IF
END DO
!!$OMP END PARALLEL
IF (ifaca <= 1) GO TO 14

!       Set up the transform split for the next stage.

ifaca = ifaca / 4
IF (ifaca > 0) GO TO 5

!       Radix 2 calculation, if needed.

IF (ifaca < 0) RETURN
!!$OMP PARALLEL DO SCHEDULE(STATIC) IF(k>=1024)
DO k = 1, n, 2
  tempr = xreal(k) + xreal(k+1)
  xreal(k+1) = xreal(k) - xreal(k+1)
  xreal(k) = tempr
  tempr = ximag(k) + ximag(k+1)
  ximag(k+1) = ximag(k) - ximag(k+1)
  ximag(k) = tempr
END DO
14 IF (itype < 0) THEN
  
!       Inverse transform; conjugate the result.
  
  ximag(1:n) = -ximag(1:n)
  RETURN
END IF

!       Forward transform

z = one / n
!!$OMP PARALLEL DO SCHEDULE(STATIC) IF(k>=2048)
DO k = 1, n
  xreal(k) = xreal(k) * z
  ximag(k) = ximag(k) * z
END DO

RETURN
END SUBROUTINE fastg

!Seems to be not used (kind of auxiliary subroutine)

SUBROUTINE scram(xreal, ximag, ipow)

!       Algorithm AS 83.3 Appl. Statist. (1975) vol.24, no.1

!       Subroutine for unscrambling FFT data.

REAL, INTENT(IN OUT) :: xreal(:), ximag(:)
INTEGER, INTENT(IN)  :: ipow

INTEGER :: ii, itop, i, k, l0, j1, j2, j3, j4, j5, j6, j7, j8, j9,  &
           j10, j11, j12, j13, j14, j15, j16, j17, j18, j19, j20, l(19)
REAL    :: tempr

ii = 1
itop = 2 ** (ipow - 1)
i = 20 - ipow
l(1:i) = ii
l0 = ii
i = i + 1
DO k = i, 19
  ii = ii * 2
  l(k) = ii
END DO

ii = 0
DO j1 = 1, l(1), l0
  DO j2 = j1, l(2), l(3)
    DO j3 = j2, l(3), l(2)
      DO j4 = j3, l(4), l(3)
        DO j5 = j4, l(5), l(4)
          DO j6 = j5, l(6), l(5)
            DO j7 = j6, l(7), l(6)
              DO j8 = j7, l(8), l(7)
                DO j9 = j8, l(9), l(8)
                  DO j10 = j9, l(10), l(9)
                    DO j11 = j10, l(11), l(10)
                      DO j12 = j11, l(12), l(11)
                        DO j13 = j12, l(13), l(12)
                          DO j14 = j13, l(14), l(13)
                            DO j15 = j14, l(15), l(14)
                              DO j16 = j15, l(16), l(15)
                                DO j17 = j16, l(17), l(16)
                                  DO j18 = j17, l(18), l(17)
                                    DO j19 = j18, l(19), l(18)
                                      j20 = j19
                                      DO i = 1, 2
                                        ii = ii + 1
                                        IF (ii < j20) THEN
                                          
!       J20 is the bit-reverse of II pairwise interchange.
                                          
                                          tempr = xreal(ii)
                                          xreal(ii) = xreal(j20)
                                          xreal(j20) = tempr
                                          tempr = ximag(ii)
                                          ximag(ii) = ximag(j20)
                                          ximag(j20) = tempr
                                        END IF
                                        j20 = j20 + itop
                                      END DO
                                    END DO
                                  END DO
                                END DO
                              END DO
                            END DO
                          END DO
                        END DO
                      END DO
                    END DO
                  END DO
                END DO
              END DO
            END DO
          END DO
        END DO
      END DO
    END DO
  END DO
END DO

RETURN
END SUBROUTINE scram


END MODULE FFT_chirp_transform
