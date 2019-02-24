!------------------------------------------------------------------------------
module m_constants
    use mod_kinds, only : sp
!------------------------------------------------------------------------------
!
! physical constants
!
real(sp) :: sqrtg   ! square root of grav
real(sp) :: gsq     ! square of grav
real(sp) ::  nu      ! kinematic viscosity of water
!
real(sp) :: d_water ! density of water
real(sp) :: d_air   ! density of air
!
real(sp) :: trshdep ! treshold depth (=DEPMIN as given by SWAN)
!
! mathematical constants
!
real(sp) :: pih    ! pi/2
real(sp) :: dera   ! conversion from degrees to radians
real(sp) :: rade   ! conversion from radians to degrees
real(sp) :: expmin ! min argument for exp. function to avoid underflow
real(sp) :: expmax ! max argument for exp. function to avoid overflow
real(sp) :: sqrt2  ! square root of 2 ~ 1.41
!
contains
!
!------------------------------------------------------------------------------
subroutine init_constants
!------------------------------------------------------------------------------
!
use SWCOMM3
!
pih  = 0.5_sp*PI
dera = PI/180._sp
rade = 180._sp/PI
!
expmin = -20._sp
expmax =  20._sp
!
!  physical constants
!
sqrtg   = sqrt(GRAV)
gsq     = GRAV*GRAV
nu      = 1.e-6
d_air   = PWIND(16)
d_water = PWIND(17)
!
trshdep = DEPMIN
!
end subroutine
!
end module
