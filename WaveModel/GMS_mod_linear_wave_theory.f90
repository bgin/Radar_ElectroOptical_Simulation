module mod_linear_wave_theory

!
! wavy - A spectral ocean wave modeling and development framework
! Copyright (c) 2017, Wavebit Scientific LLC
! All rights reserved.
!
! Licensed under the BSD-3 clause license. See LICENSE for details.
! Modified by Bernard Gingold (beniekg@gmail.com) on 14/03/2019
!===============================================================================
    use mod_kinds, only : i4, dp

    implicit none

    private
    public :: elevation
    public :: pressure
    public :: horizontalAcceleration
    public :: horizontalVelocity
    public :: verticalAcceleration
    public :: verticalVelocity

    contains

    !-------------------------------------------------------------------------------
!DIR$   ATTRIBUTES INLINE :: eleveation
    pure elemental real(kind=dp) function elevation(x,t,a,k,omega)

    !! Returns the elevation [m] of a sinusoid wave given its amplitude [m],
    !! wavenumber [rad/m], and frequency [Hz].
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: elevation
          real(kind=dp),intent(in) :: x
             !! Horizontal space [m]
          real(kind=dp),intent(in) :: t
             !! Time [s]
          real(kind=dp),intent(in) :: a
             !! Wave amplitude [m]
          real(kind=dp),intent(in) :: k
             !! Wavenumber [rad/m]
          real(kind=dp),intent(in) :: omega
          !! Angular frequency [rad]

            elevation = a*sin(k*x-omega*t)

    end function elevation
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
!DIR$   ATTRIBUTES INLINE :: pressure
    pure elemental real(kind=dp) function pressure(x,z,t,a,k,omega,rho,grav)

        !! Returns the pressure [Pa] at depth z (negative downward) for a sinusoid
        !! wave given its amplitude [m], wavenumber [rad/m], and frequency [Hz].
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: pressure
          real(kind=dp),intent(in) :: x
            !! Horizontal space [m]
          real(kind=dp),intent(in) :: z
            !! Vertical displacement [m] from the surface, negative downward
          real(kind=dp),intent(in) :: t
            !! Time [s]
          real(kind=dp),intent(in) :: a
            !! Wave amplitude [m]
          real(kind=dp),intent(in) :: k
            !! Wavenumber [rad/m]
          real(kind=dp),intent(in) :: omega
            !! Angular frequency [rad]
          real(kind=dp),intent(in) :: rho
            !! Water density [kg/m^3]
          real(kind=dp),intent(in) :: grav
            !! Gravitational acceleration [m/s^2]

            pressure = -rho*grav*(elevation(x,t,a,k,omega)-z)

    end function pressure
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
!DIR$   ATTRIBUTES INLINE :: horizontalAcceleration
    pure elemental real(kind=dp) function horizontalAcceleration(x,z,t,a,k,omega)

        !! Returns the horizontal acceleration of a water particle under a sinusoid wave,
        !! given its amplitude, wavenumber, and frequency.
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: horizontalAcceleration
          real(kind=dp),intent(in) :: x
            !! Horizontal space [m]
          real(kind=dp),intent(in) :: z
            !! Vertical space, negative downward [m]
          real(kind=dp),intent(in) :: t
            !! Time [s]
          real(kind=dp),intent(in) :: a
            !! Wave amplitude [m]
          real(kind=dp),intent(in) :: k
             !! Wavenumber [rad/m]
          real(kind=dp),intent(in) :: omega
             !! Angular frequency [rad]

                 horizontalAcceleration = -a*omega**2*cos(k*x-omega*t)*exp(k*z)

    end function horizontalAcceleration
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
!DIR$   ATTRIBUTES INLINE :: horizontalVelocity
    pure elemental real(kind=dp) function horizontalVelocity(x,z,t,a,k,omega)

        !! Returns the horizontal velocity of a water particle under a sinusoid wave,
        !! given its amplitude, wavenumber, and frequency.
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: horizontalVelocity
          real(kind=dp),intent(in) :: x
            !! Horizontal space [m]
          real(kind=dp),intent(in) :: z
            !! Vertical space, negative downward [m]
          real(kind=dp),intent(in) :: t
            !! Time [s]
          real(kind=dp),intent(in) :: a
            !! Wave amplitude [m]
          real(kind=dp),intent(in) :: k
            !! Wavenumber [rad/m]
          real(kind=dp),intent(in) :: omega
                !! Angular frequency [rad]

            horizontalVelocity = a*omega*sin(k*x-omega*t)*exp(k*z)

    end function horizontalVelocity
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
!DIR$   ATTRIBUTES INLINE :: verticalAcceleration
    pure elemental real(kind=dp)&
         function verticalAcceleration(x,z,t,a,k,omega)

        !! Returns the vertical acceleration of a water particle under a sinusoid wave,
        !! given its amplitude, wavenumber, and frequency.
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: verticalAcceleration
          real(kind=dp),intent(in) :: x
            !! Horizontal space [m]
          real(kind=dp),intent(in) :: z
            !! Vertical space, negative downward [m]
          real(kind=dp),intent(in) :: t
            !! Time [s]
          real(kind=dp),intent(in) :: a
            !! Wave amplitude [m]
          real(kind=dp),intent(in) :: k
            !! Wavenumber [rad/m]
          real(kind=dp),intent(in) :: omega
            !! Angular frequency [rad]

            verticalAcceleration = -a*omega**2*sin(k*x-omega*t)*exp(k*z)

endfunction verticalAcceleration
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
!DIR$   ATTRIBUTES INLINE :: verticalVelocity
    pure elemental real(kind=dp) function verticalVelocity(x,z,t,a,k,omega)

        !! Returns the vertical velocity of a water particle under a sinusoid wave,
        !! given its amplitude, wavenumber, and frequency.
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: verticalVelocity
          real(kind=dp),intent(in) :: x
            !! Horizontal space [m]
          real(kind=dp),intent(in) :: z
            !! Vertical space, negative downward [m]
          real(kind=dp),intent(in) :: t
            !! Time [s]
          real(kind=dp),intent(in) :: a
            !! Wave amplitude [m]
          real(kind=dp),intent(in) :: k
            !! Wavenumber [rad/m]
          real(kind=dp),intent(in) :: omega
                !! Angular frequency [rad]

            verticalVelocity = -a*omega*cos(k*x-omega*t)*exp(k*z)

    end function verticalVelocity

end module mod_linear_wave_theory
