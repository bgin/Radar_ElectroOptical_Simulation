! MODULE: quad
! AUTHOR: Jouni Makitalo
! Modified by Bernard Gingold , beniekg@gmail.com
! DESCRIPTION:
! Quadrature routines for integartion over triangles and tetrahedra (Gauss-Legendre).
! Also routines for Simpson quadrature over interval and rectangle.
MODULE gauss_quadrature
  USE mod_kinds, only : i4, dp
  USE gmsh_mesh

  IMPLICIT NONE

  REAL (KIND=dp), DIMENSION(4), PARAMETER :: qw4 = (/-0.56250000_dp,&
       0.52083333_dp,0.52083333_dp,0.52083333_dp/)

  REAL (KIND=dp), DIMENSION(4,3), PARAMETER :: quadFactors4 = RESHAPE((/&
       1.0_dp/3.0_dp,&
       0.6_dp,&
       0.2_dp,&
       0.2_dp,&

       1.0_dp/3.0_dp,&
       0.2_dp,&
       0.6_dp,&
       0.2_dp,&

       1.0_dp/3.0_dp,&
       0.2_dp,&
       0.2_dp,&
       0.6_dp/), (/4,3/))

  REAL (KIND=dp), DIMENSION(7), PARAMETER :: qw7 = (/0.22500000_dp,&
       0.13239415_dp, 0.13239415_dp, 0.13239415_dp, 0.12593918_dp,&
       0.12593918_dp, 0.12593918_dp/)

  REAL (KIND=dp), DIMENSION(7,3), PARAMETER :: quadFactors7 = RESHAPE((/&
       0.33333333_dp,&
       0.05971587_dp,&
       0.47014206_dp,&
       0.47014206_dp,&
       0.79742698_dp,&
       0.10128650_dp,&
       0.10128650_dp,&

       0.33333333_dp,&
       0.47014206_dp,&
       0.05971587_dp,&
       0.47014206_dp,&
       0.10128650_dp,&
       0.79742698_dp,&
       0.10128650_dp,&

       0.33333333_dp,&
       0.47014206_dp,&
       0.47014206_dp,&
       0.05971587_dp,&
       0.10128650_dp,&
       0.10128650_dp,&
       0.79742698_dp&
       /), (/7,3/))

  REAL (KIND=dp), DIMENSION(6), PARAMETER :: qw6 = (/&
       0.109951743655322_dp,&
       0.109951743655322_dp,&
       0.109951743655322_dp,&
       0.223381589678011_dp,&
       0.223381589678011_dp,&
       0.223381589678011_dp/)

  REAL (KIND=dp), DIMENSION(6,3), PARAMETER :: quadFactors6 = RESHAPE((/&
       0.816847572980459_dp,&
       0.091576213509771_dp,&
       0.091576213509771_dp,&
       0.108103018168070_dp,&
       0.445948490915965_dp,&
       0.445948490915965_dp,&

       0.091576213509771_dp,&
       0.816847572980459_dp,&
       0.091576213509771_dp,&
       0.445948490915965_dp,&
       0.108103018168070_dp,&
       0.445948490915965_dp,&

       0.091576213509771_dp,&
       0.091576213509771_dp,&
       0.816847572980459_dp,&
       0.445948490915965_dp,&
       0.445948490915965_dp,&
       0.108103018168070_dp/),(/6,3/))

  REAL (KIND=dp), DIMENSION(1), PARAMETER :: qw1 = 1.0_dp

  REAL (KIND=dp), DIMENSION(1,3), PARAMETER :: quadFactors1 = RESHAPE((/&
       0.333333333333333_dp,&
       0.333333333333333_dp,&
       0.333333333333333_dp/),(/1,3/))

  REAL (KIND=dp), DIMENSION(3), PARAMETER :: qw3 = (/&
       0.333333333333333_dp,&
       0.333333333333333_dp,&
       0.333333333333333_dp/)

  REAL (KIND=dp), DIMENSION(3,3), PARAMETER :: quadFactors3 = RESHAPE((/&
       0.666666666666667_dp,&
       0.166666666666667_dp,&
       0.166666666666667_dp,&

       0.166666666666667_dp,&
       0.666666666666667_dp,&
       0.166666666666667_dp,&

       0.166666666666667_dp,&
       0.166666666666667_dp,&
       0.666666666666667_dp/),(/3,3/))

  REAL (KIND=dp), DIMENSION(13), PARAMETER :: qw13 = (/&
       -0.149570044467670_dp,&
       0.175615257433204_dp,&
       0.175615257433204_dp,&
       0.175615257433204_dp,&
       0.053347235608839_dp,&
       0.053347235608839_dp,&
       0.053347235608839_dp,&
       0.077113760890257_dp,&
       0.077113760890257_dp,&
       0.077113760890257_dp,&
       0.077113760890257_dp,&
       0.077113760890257_dp,&
       0.077113760890257_dp/)

  REAL (KIND=dp), DIMENSION(13,3), PARAMETER :: quadFactors13 = RESHAPE((/&
       0.333333333333333,&
       0.479308067841923,&
       0.260345966079038,&
       0.260345966079038,&
       0.869739794195568,&
       0.065130102902216,&
       0.065130102902216,&
       0.638444188569809,&
       0.638444188569809,&
       0.312865496004875,&
       0.312865496004875,&
       0.048690315425316,&
       0.048690315425316,&

       0.333333333333333,&
       0.260345966079038,&
       0.260345966079038,&
       0.479308067841923,&
       0.065130102902216,&
       0.065130102902216,&
       0.869739794195568,&
       0.048690315425316,&
       0.312865496004875,&
       0.048690315425316,&
       0.638444188569809,&
       0.312865496004875,&
       0.638444188569809,&

       0.333333333333333,&
       0.260345966079038,&
       0.479308067841923,&
       0.260345966079038,&
       0.065130102902216,&
       0.869739794195568,&
       0.065130102902216,&
       0.312865496004875,&
       0.048690315425316,&
       0.638444188569809,&
       0.048690315425316,&
       0.638444188569809,&
       0.312865496004875/),(/13,3/))

  REAL (KIND=dp), DIMENSION(1), PARAMETER :: volQw1 = (/1.0_dp/)

  REAL (KIND=dp), DIMENSION(1,4), PARAMETER :: volQuadFactors1 = RESHAPE((/&
       0.25_dp, 0.25_dp, 0.25_dp, 0.25_dp/), (/1,4/))

  REAL (KIND=dp), DIMENSION(4), PARAMETER :: volQw4 = (/&
       0.25_dp,&
       0.25_dp,&
       0.25_dp,&
       0.25_dp/)

  REAL (KIND=dp), DIMENSION(4,4), PARAMETER :: volQuadFactors4 = RESHAPE((/&
       0.585410196624969_dp,&
       0.138196601125011_dp,&
       0.138196601125011_dp,&
       0.138196601125011_dp,&

       0.138196601125011_dp,&
       0.585410196624969_dp,&
       0.138196601125011_dp,&
       0.138196601125011_dp,&

       0.138196601125011_dp,&
       0.138196601125011_dp,&
       0.585410196624969_dp,&
       0.138196601125011_dp,&

       0.138196601125011_dp,&
       0.138196601125011_dp,&
       0.138196601125011_dp,&
       0.585410196624969_dp/),(/4,4/))

  REAL (KIND=dp), DIMENSION(11), PARAMETER :: volQw11 = (/&
       -0.013155555555556_dp,&

       0.007622222222222_dp,&
       0.007622222222222_dp,&
       0.007622222222222_dp,&
       0.007622222222222_dp,&

       0.024888888888889_dp,&
       0.024888888888889_dp,&
       0.024888888888889_dp,&
       0.024888888888889_dp,&
       0.024888888888889_dp,&
       0.024888888888889_dp/)

  REAL (KIND=dp), DIMENSION(11,4), PARAMETER :: volQuadFactors11 = RESHAPE((/&
       0.250000000000000_dp,&
       0.785714285714286_dp,&
       0.071428571428571_dp,&
       0.071428571428571_dp,&
       0.071428571428571_dp,&
       0.399403576166799_dp,&
       0.100596423833201_dp,&
       0.399403576166799_dp,&
       0.100596423833201_dp,&
       0.399403576166799_dp,&
       0.100596423833201_dp,&

       0.250000000000000_dp,&
       0.071428571428571_dp,&
       0.785714285714286_dp,&
       0.071428571428571_dp,&
       0.071428571428571_dp,&
       0.399403576166799_dp,&
       0.100596423833201_dp,&
       0.100596423833201_dp,&
       0.399403576166799_dp,&
       0.100596423833201_dp,&
       0.399403576166799_dp,&

       0.250000000000000_dp,&
       0.071428571428571_dp,&
       0.071428571428571_dp,&
       0.785714285714286_dp,&
       0.071428571428571_dp,&
       0.100596423833201_dp,&
       0.399403576166799_dp,&
       0.399403576166799_dp,&
       0.100596423833201_dp,&
       0.100596423833201_dp,&
       0.399403576166799_dp,&

       0.250000000000000_dp,&
       0.071428571428571_dp,&
       0.071428571428571_dp,&
       0.071428571428571_dp,&
       0.785714285714286_dp,&
       0.100596423833201_dp,&
       0.399403576166799_dp,&
       0.100596423833201_dp,&
       0.399403576166799_dp,&
       0.399403576166799_dp,&
       0.100596423833201_dp/),(/11,4/))

  TYPE quad_data
     CHARACTER (LEN=256) :: description
     INTEGER :: num_nodes
     REAL (KIND=dp), DIMENSION(:), ALLOCATABLE :: weights
     !dir$ attributes align : 64 :: weights
     REAL (KIND=dp), DIMENSION(:,:), ALLOCATABLE :: nodes
     !dir$ attributes align : 64 :: nodes
  END type quad_data

CONTAINS
  FUNCTION tri_quad_data(name) RESULT(qd)
    CHARACTER (LEN=*) :: name
    TYPE(quad_data) :: qd

    IF(name=='tri_gl1') THEN
       qd%description = '1-point Gauss-Legendre'
       qd%num_nodes = 1
       ALLOCATE(qd%weights(qd%num_nodes), qd%nodes(qd%num_nodes,3))
       qd%weights = qw1
       qd%nodes = quadFactors1
    ELSE IF(name=='tri_gl3') THEN
       qd%description = '3-point Gauss-Legendre'
       qd%num_nodes = 3
       ALLOCATE(qd%weights(qd%num_nodes), qd%nodes(qd%num_nodes,3))
       qd%weights = qw3
       qd%nodes = quadFactors3
    ELSE IF(name=='tri_gl4') THEN
       qd%description = '4-point Gauss-Legendre'
       qd%num_nodes = 4
       ALLOCATE(qd%weights(qd%num_nodes), qd%nodes(qd%num_nodes,3))
       qd%weights = qw4
       qd%nodes = quadFactors4
    ELSE IF(name=='tri_gl7') THEN
       qd%description = '7-point Gauss-Legendre'
       qd%num_nodes = 7
       ALLOCATE(qd%weights(qd%num_nodes), qd%nodes(qd%num_nodes,3))
       qd%weights = qw7
       qd%nodes = quadFactors7
    ELSE IF(name=='tri_gl13') THEN
       qd%description = '13-point Gauss-Legendre'
       qd%num_nodes = 13
       ALLOCATE(qd%weights(qd%num_nodes), qd%nodes(qd%num_nodes,3))
       qd%weights = qw13
       qd%nodes = quadFactors13
    ELSE
       WRITE(*,*) 'Unrecognized quadrature type ', name, '!'
       STOP
    END IF
  END FUNCTION tri_quad_data

  FUNCTION tetra_quad_data(name) RESULT(qd)
    CHARACTER (LEN=*) :: name
    TYPE(quad_data) :: qd

    IF(name=='tetra_gl1') THEN
       qd%description = '1-point Gauss-Legendre'
       qd%num_nodes = 1
       ALLOCATE(qd%weights(qd%num_nodes), qd%nodes(qd%num_nodes,4))
       qd%weights = volQw1
       qd%nodes = volQuadFactors1
    ELSE IF(name=='tetra_gl4') THEN
       qd%description = '4-point Gauss-Legendre'
       qd%num_nodes = 4
       ALLOCATE(qd%weights(qd%num_nodes), qd%nodes(qd%num_nodes,4))
       qd%weights = volQw4
       qd%nodes = volQuadFactors4
    ELSE IF(name=='tetra_gl11') THEN
       qd%description = '11-point Gauss-Legendre'
       qd%num_nodes = 11
       ALLOCATE(qd%weights(qd%num_nodes), qd%nodes(qd%num_nodes,4))
       qd%weights = volQw11
       qd%nodes = volQuadFactors11
    ELSE
       WRITE(*,*) 'Unrecognized quadrature type ', name, '!'
       STOP
    END IF
  END FUNCTION tetra_quad_data

  SUBROUTINE delete_quad_data(qd)
    TYPE(quad_data), INTENT(INOUT) :: qd

    DEALLOCATE(qd%weights)
    DEALLOCATE(qd%nodes)
  END SUBROUTINE delete_quad_data

  FUNCTION quad_tri_points(qd, faceind, mesh) RESULT(res)
    TYPE(quad_data), INTENT(IN) :: qd
    INTEGER, INTENT(IN) :: faceind
    TYPE(mesh_container), INTENT(IN) :: mesh
    REAL (KIND=dp), DIMENSION(3,qd%num_nodes) :: res
    REAL (KIND=dp), DIMENSION(4) :: p1, p2, p3
    INTEGER :: n

    p1 = mesh%nodes(mesh%faces(faceind)%node_indices(1))%p
    p2 = mesh%nodes(mesh%faces(faceind)%node_indices(2))%p
    p3 = mesh%nodes(mesh%faces(faceind)%node_indices(3))%p

    DO n=1,qd%num_nodes
       res(:,n) = qd%nodes(n,3)*p1 + qd%nodes(n,1)*p2 + qd%nodes(n,2)*p3
    END DO
  END FUNCTION quad_tri_points

  FUNCTION quad_tetra_points(qd, solidind, mesh) RESULT(res)
    TYPE(quad_data), INTENT(IN) :: qd
    INTEGER, INTENT(IN) :: solidind
    TYPE(mesh_container), INTENT(IN) :: mesh
    REAL (KIND=dp), DIMENSION(3,qd%num_nodes) :: res
    REAL (KIND=dp), DIMENSION(4) :: p1, p2, p3, p4
    INTEGER :: n

    p1 = mesh%nodes(mesh%solids(solidind)%node_indices(1))%p
    p2 = mesh%nodes(mesh%solids(solidind)%node_indices(2))%p
    p3 = mesh%nodes(mesh%solids(solidind)%node_indices(3))%p
    p4 = mesh%nodes(mesh%solids(solidind)%node_indices(4))%p

    DO n=1,qd%num_nodes
       res(:,n) = qd%nodes(n,1)*p1 + qd%nodes(n,2)*p2 + qd%nodes(n,3)*p3 + qd%nodes(n,4)*p4
    END DO
  END FUNCTION quad_tetra_points

  ! n is the number of subintervals -> number of nodes is n+1.
  ! n must be even.
  SUBROUTINE get_simpsons_weights(a, b, n, weights)
    REAL (KIND=dp), INTENT(IN) :: a, b
    INTEGER, INTENT(IN) :: n
    REAL (KIND=dp), DIMENSION(n+1), INTENT(OUT) :: weights
    REAL (KIND=dp) :: h

    IF(MOD(n,2)/=0) THEN
       WRITE(*,*) "Number of subintervals for Simpson's rule must be even!"
       STOP
    END IF

    h = (b-a)/n

    weights((/1,n+1/)) = h/3.0_dp
    weights(3:(n-1):2) = 2.0_dp*h/3.0_dp
    weights(2:n:2) = 4.0_dp*h/3.0_dp
  END SUBROUTINE get_simpsons_weights

  ! n is the number of subintervals -> number of nodes is n+1.
  ! n must be even.
  SUBROUTINE get_simpsons_points(a, b, n, points)
    REAL (KIND=dp), INTENT(IN) :: a, b
    INTEGER, INTENT(IN) :: n
    REAL (KIND=dp), DIMENSION(n+1), INTENT(OUT) :: points
    REAL (KIND=dp) :: h
    INTEGER :: i

    IF(MOD(n,2)/=0) THEN
       WRITE(*,*) "Number of subintervals for Simpson's rule must be even!"
       STOP
    END IF

    h = (b-a)/n

    points(1:(n+1)) = a + h*(/(i,i=0,n)/)
  END SUBROUTINE get_simpsons_points

  RECURSIVE FUNCTION asqz_aux(f, a, b, eps, s, fa, fb, fc, level, maxDepth) RESULT(res)
    COMPLEX (KIND=dp), EXTERNAL :: f
    REAL (KIND=dp), INTENT(IN) :: a, b, eps
    COMPLEX (KIND=dp), INTENT(IN) :: s, fa, fb, fc
    INTEGER, INTENT(IN) :: level, maxDepth

    COMPLEX (KIND=dp) :: res, fd, fe, sleft, sright, s2
    REAL (KIND=dp) :: c, h, d, e

    c = (a + b)/2
    h = b - a

    d = (a + c)/2
    e = (c + b)/2

    fd = f(d)
    fe = f(e)

    sleft = (h/12)*(fa + 4*fd + fc)
    sright = (h/12)*(fc + 4*fe + fb)

    s2 = sleft + sright

    !IF(bottom<=0 .OR. (ABS(s2 - s)<=15*eps .AND. bottom<4) ) THEN
    IF(level>=maxDepth .OR. (ABS(s2 - s)<=15*eps*ABS(s2) .AND. level>1) ) THEN
       res = s2 + (s2 - s)/15
    ELSE
       res = asqz_aux(f, a, c, eps/2, sleft, fa, fc, fd, level+1, maxDepth) +&
            asqz_aux(f, c, b, eps/2, sright, fc, fb, fe, level+1, maxDepth)
    END IF
  END FUNCTION asqz_aux

  ! Adaptive Simpson's method based on listing at
  ! http://en.wikipedia.org/wiki/Adaptive_Simpson%27s_method#C
  FUNCTION asqz(f, a, b, eps, maxDepth) RESULT(res)
    COMPLEX (KIND=dp), EXTERNAL :: f
    REAL (KIND=dp), INTENT(IN) :: a, b, eps
    INTEGER, INTENT(IN) :: maxDepth

    COMPLEX (KIND=dp) :: fa, fb, fc, s, res
    REAL (KIND=dp) :: c, h

    c = (a + b)/2
    h = b - a

    fa = f(a)
    fb = f(b)
    fc = f(c)

    s = (h/6)*(fa + 4*fc + fb)

    res = asqz_aux(f, a, b, eps, s, fa, fb, fc, 0, maxDepth)
  END FUNCTION asqz

  ! Integrates f(x,y) over [x1,x2]x[y1,y2].
  SUBROUTINE asqz2(f, x1, x2, y1, y2, eps, maxDepth, res)
    COMPLEX (KIND=dp), EXTERNAL :: f
    REAL (KIND=dp), INTENT(IN) :: x1, x2, y1, y2, eps
    INTEGER, INTENT(IN) :: maxDepth
    COMPLEX (KIND=dp), INTENT(INOUT) :: res

    ! Auxiliary variable to make f(x,y) to appear a
    ! function of single variable for two nested
    ! integration routines.
    REAL (KIND=dp) :: gy

    res = asqz(fnested, y1, y2, eps, maxDepth)

  CONTAINS
    ! Evaluates f with y fixed to global value gy.
    FUNCTION fproxy(x) RESULT(z)
      REAL (KIND=dp), INTENT(IN) :: x
      COMPLEX (KIND=dp) :: z

      z = f(x,gy)
    END FUNCTION fproxy

    FUNCTION fnested(y) RESULT(z)
      REAL (KIND=dp), INTENT(IN) :: y
      COMPLEX (KIND=dp) :: z
      
      gy = y

      z = asqz(fproxy, x1, x2, eps, maxDepth)
    END FUNCTION fnested

  END SUBROUTINE asqz2

END MODULE gauss_quadrature
