! Generic math subroutines
module math_mod

    implicit none
    real,parameter :: pi = 3.14159265358979323846264338327950288419716939937510 ! No, this is not at all excessive
    real,parameter :: pi2 = pi*0.5
    real,parameter :: inf = huge(0.) ! Reason #403929 why Fortran is the best: huge() is an intrinsic function
    
contains


function isinf(x) result(is)
  ! Checks if x is infinite

  implicit none

  real,intent(in) :: x

  logical :: is

  ! Check for infinity
  if (x >= inf .or. x <= -inf) then
    is = .true.
  else
    is = .false.
  end if

end function isinf


function plane_normal(p1, p2, p3) result(n)
  ! Computes the normal vector to a plane defined by 3 points

  implicit none

  real,dimension(3),intent(in) :: p1, p2, p3
  real,dimension(3) :: a, b, n

  a = p2 - p1
  b = p3 - p1
  n = cross(a,b)
  n = n/norm2(n)

end function plane_normal


function reflect_point(A, B, C, D, P) result(P_refl)
  ! Somehow reflects a point P

  implicit none

  real,intent(in) :: A, B, C, D
  real,dimension(3),intent(in) :: P(3)
  real,dimension(3) :: P_refl

  real :: mult

  mult = 2.0*(A*P(1) + B*P(2) + C*P(3) + D)/(A**2 + B**2 + C**2)

  P_refl(1) = P(1) - mult*A
  P_refl(2) = P(2) - mult*B
  P_refl(3) = P(3) - mult*C

end function reflect_point


function dist(a, b) result(c)
  ! Calculates the cartesian distance between 2 points

    implicit none

    real,dimension(3),intent(in) :: a, b
    real :: c

    c = norm2(a-b)

end function dist


function cross(a, b) result(c)
  ! Calculates the cross-product of two 3-element vectors

    implicit none

    real,dimension(3),intent(in) :: a, b

    real,dimension(3) :: c

    c(1) = a(2)*b(3) - a(3)*b(2)
    c(2) = a(3)*b(1) - a(1)*b(3)
    c(3) = a(1)*b(2) - a(2)*b(1)

end function cross


function inner(a, b) result(c)
  ! Calculates the 3D Euclidean inner product

  implicit none
  real,dimension(3),intent(in) :: a, b
  real :: c

  c = a(1)*b(1)+a(2)*b(2)+a(3)*b(3)

end function inner


function inner2(a, b) result(c)
  ! Calculates the 2D Euclidean inner product

  implicit none
  real,dimension(2),intent(in) :: a, b
  real :: c

  c = a(1)*b(1)+a(2)*b(2)

end function inner2


function outer(a, b) result(c)
  ! Calculates the outer product of two vectors

  implicit none

  real,dimension(3),intent(in) :: a, b
  real,dimension(3,3) :: c

  integer :: i

  do i=1,3
    c(:,i) = a(:)*b(i)
  end do

end function


function det3(a) result(c)
  ! Calculates the determinant of a 3x3 matrix

  implicit none

  real,dimension(3,3) :: a
  real :: c

  c = a(1,1)*(a(2,2)*a(3,3)-a(2,3)*a(3,2))
  c = c - a(1,2)*(a(2,1)*a(3,3)-a(2,3)*a(3,1))
  c = c + a(1,3)*(a(2,1)*a(3,2)-a(3,1)*a(2,2))

end function det3


function rot_x(v, theta) result(v_rot)
  ! Rotates v about the x axis by theta (in radians)

    implicit none

    real,dimension(3),intent(in) :: v
    real,intent(in) :: theta
    real,dimension(3) :: v_rot

    real,dimension(3,3) :: rm(3,3) = 0.

    rm(1,1) = 1.
    rm(2,2) = cos(theta)
    rm(2,3) = -sin(theta)
    rm(3,2) = sin(theta)
    rm(3,3) = cos(theta)

    v_rot = matmul(rm, v)

end function rot_x


function rot_y(v, theta) result(v_rot)
  ! Rotates v about the y axis by theta (in radians)

    implicit none

    real,dimension(3),intent(in) :: v
    real,intent(in) :: theta
    real,dimension(3) :: v_rot

    real,dimension(3,3) :: rm(3,3) = 0.

    rm(1,1) = cos(theta)
    rm(1,3) = sin(theta)
    rm(2,2) = 1.0
    rm(3,1) = -sin(theta)
    rm(3,3) = cos(theta)

    v_rot = matmul(rm, v)

end function rot_y


function rot_z(v, theta) result(v_rot)
  ! Rotates v about the z axis by theta (in radians)

    implicit none

    real,dimension(3),intent(in) :: v
    real,intent(in) :: theta
    real,dimension(3) :: v_rot

    real,dimension(3,3) :: rm(3,3) = 0.

    rm(1,1) = cos(theta)
    rm(1,2) = -sin(theta)
    rm(2,1) = sin(theta)
    rm(2,2) = cos(theta)
    rm(3,3) = 1.0

    v_rot = matmul(rm, v)

end function rot_z


function parallel_matmul(A, B) result(C)
  ! Multiplies A by B (both 2-dimensional arrays) in parallel

  implicit none
  
  real,dimension(:,:),allocatable,intent(in) :: A, B
  
  real,dimension(:,:),allocatable :: C

  integer,dimension(:),allocatable :: A_shape, B_shape
  integer :: i, j

  ! Check shape
  A_shape = shape(A)
  B_shape = shape(B)
  if (size(A_shape) /= 2) then
    write(*,*) "!!! Function 'parallel_matmul' cannot be used with a non-2-dimensional array. A is not 2D."
    stop
  end if
  if (size(B_shape) /= 2) then
    write(*,*) "!!! Function 'parallel_matmul' cannot be used with a non-2-dimensional array. B is not 2D."
    stop
  end if
  if (A_shape(2) /= B_shape(1)) then
    write(*,*) "!!! Dimension mismatch in 'parallel_matmul'. A is ", A_shape(1), 'x', A_shape(2), " while B is", &
                B_shape(1), 'x', B_shape(2), "."
    stop
  end if

  ! Allocate C
  allocate(C(A_shape(1), B_shape(2)), source=0.)

  ! Multiply
  !$OMP parallel do private(j)
  do i=1,A_shape(1)
    do j=1,B_shape(2)
      C(i,j) = sum(A(i,:)*B(:,j))
    end do
  end do
  
end function parallel_matmul

end module math_mod