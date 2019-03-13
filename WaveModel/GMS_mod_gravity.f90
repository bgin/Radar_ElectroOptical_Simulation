module mod_gravity

!
! wavy - A spectral ocean wave modeling and development framework
! Copyright (c) 2017, Wavebit Scientific LLC
! All rights reserved.
!
! Licensed under the BSD-3 clause license. See LICENSE for details.
! Modified by Bernard Gingold (beniekg@gmail.com)
!
    use mod_kinds, only : dp
    implicit none

    private
    public :: gravityClairaut
    public :: grav_constant

    real(kind=dp), parameter :: grav_constants = 6.67408e-11_dp

    contains

    pure elemental real(kind=dp) &
                   function gravityClairaut(latitude) result(grav)
      !! Returns the gravitational acceleration at the Earth's surface as function
      !! of latitude, based on Clairaut's formula.
      !!
          real(kind=dp), intent(in) :: latitude
          ! Locals
          real(kind=dp) :: grav, sum_lat
          ! Exec code .....
          sum_lat = latitude+latitude
          grav = 9.780327_dp*(1.0026454_dp   &
                            - 0.0026512_dp*cos(sum_lat) &
                            + 0.0000058_dp*cos(sum_lat)**2)
    end function gravityClairaut

end module mod_gravity
