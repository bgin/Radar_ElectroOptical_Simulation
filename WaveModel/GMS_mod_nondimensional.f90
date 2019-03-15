module mod_nondimensional

!
! wavy - A spectral ocean wave modeling and development framework
! Copyright (c) 2017, Wavebit Scientific LLC
! All rights reserved.
!
! Licensed under the BSD-3 clause license. See LICENSE for details.
! Modified by Bernard Gingold (beniekg@gmail.com) on 14/03/2019
!
    use mod_kinds, only : int32_t, dp
    implicit none

    private

    public :: nondimensionalDepth
    public :: nondimensionalEnergy
    public :: nondimensionalFetch
    public :: nondimensionalFrequency
    public :: nondimensionalRoughness_S1974
    public :: nondimensionalRoughness_H1986
    public :: nondimensionalTime
    public :: waveAge

    contains

    !-------------------------------------------------------------------------------
!DIR$   ATTRIBUTES INLINE :: waveAge
    pure elemental real(kind=dp) function waveAge(cp,u)
        !! Returns wave age, the ratio of phase speed and friction velocity or wind
        !! speed, depending on the caller's definition of wave age.
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: waveAge
          real(kind=dp),intent(in) :: cp
            !! Phase speed [m/s]
          real(kind=dp),intent(in) :: u
            !! Friction velocity or wind speed [m/s]
            waveAge = cp / u
    end function waveAge
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
!DIR$ ATTRIBUTES INLINE :: nondimensionalDepth
    pure elemental real(kind=dp) function nondimensionalDepth(wspd,depth,grav)
        !! Returns nondimensional depth based on input wind speed [m/s], mean water
        !! depth [m], and gravitational acceleration [m/s^2].
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: nondimensionalDepth
          real(kind=dp),intent(in) :: wspd
            !! Wind speed at reference height [m/s]
          real(kind=dp),intent(in) :: depth
            !! Mean water depth [m]
          real(kind=dp),intent(in) :: grav
            !! Gravitational acceleration [m/s^2]
            nondimensionalDepth = grav*depth/wspd**2
    end function nondimensionalDepth
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
!DIR$   ATTRIBUTES INLINE :: nondimensionalEnergy
    pure elemental real(kind=dp) function nondimensionalEnergy(wspd,sigma,&
        grav)
        !! Returns nondimensional energy based on input wind speed, RMS of wave
        !! variance, and gravitational acceleration.
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: nondimensionalEnergy
          real(kind=dp),intent(in) :: wspd
            !! Wind speed at reference height [m/s]
          real(kind=dp),intent(in) :: sigma
            !! Root mean square of wave variance
          real(kind=dp),intent(in) :: grav
             !! Gravitational acceleration [m/s^2]
                 nondimensionalEnergy = sigma**2*grav**2/wspd**4
    end function nondimensionalEnergy
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
!DIR$   ATTRIBUTES INLINE :: nondimensionalFetch
    pure elemental real(kind=dp) function nondimensionalFetch(wspd,fetch,grav)
  !! Returns nondimensional energy based on input wind speed, RMS of wave
  !! variance, and gravitational acceleration.
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: nondimensionalFetch
  real(kind=dp),intent(in) :: wspd
    !! Wind speed at reference height [m/s]
  real(kind=dp),intent(in) :: fetch
    !! Fetch [m]
  real(kind=dp),intent(in) :: grav
    !! Gravitational acceleration [m/s^2]
            nondimensionalFetch = grav*fetch/wspd**2
    end function nondimensionalFetch
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
!DIR$   ATTRIBUTES INLINE :: nondimensionalFrequency
    pure elemental real(kind=dp) function nondimensionalFrequency(wspd,fpeak,&
                                                 grav)
        !! Returns nondimensional frequency based on input wind speed, peak frequency,
        !! and gravitational acceleration.
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: nondimensionalFrequency
          real(kind=dp),intent(in) :: wspd
            !! Wind speed at reference height [m/s]
          real(kind=dp),intent(in) :: fpeak
            !! Peak frequency [Hz]
          real(kind=dp),intent(in) :: grav
            !! Gravitational acceleration [m/s^2]
                nondimensionalFrequency = fpeak*wspd/grav
    end function nondimensionalFrequency
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
!DIR$   ATTRIBUTES INLINE :: nondimensionalRoughness_S1974
    pure elemental real(kind=dp) function nondimensionalRoughness_S1974(z0,&
            ustar,grav)
             !! Returns the aerodynamic roughness length scaled by friction velocity
             !! squared and gravitational acceleration, after Stewart (1974).
!DIR$   ATTRIBUTES CODE_ALIGN:32 ::  nondimensionalRoughness_S1974

          real(kind=dp),intent(in) :: z0
             !! Roughness length [m]
          real(kind=dp),intent(in) :: ustar
             !! Friction velocity [m/s]
          real(kind=dp),intent(in) :: grav
             !! Gravitational acceleration [m/s^2]
                nondimensionalRoughness_S1974 = grav*z0/ustar**2
    end function nondimensionalRoughness_S1974
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
!DIR$   ATTRIBUTES INLINE :: nondimensionalRoughness_H1986
    pure elemental real(kind=dp) function nondimensionalRoughness_H1986(z0,hs)
        !! Returns the aerodynamic roughness length scaled by significant wave height,
        !! after Huang (1986).
         !!
 !DIR$  ATTRIBUTES CODE_ALIGN:32 :: nondimensionalRoughness_H1986
          real(kind=realkind),intent(in) :: z0
         !! Roughness length [m]
          real(kind=realkind),intent(in) :: hs
         !! Significant wave height [m]
                nondimensionalRoughness_H1986 = z0 / hs
    end function nondimensionalRoughness_H1986
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
!DIR$   ATTRIBUTES INLINE :: nondimensionalTime
    pure elemental real(kind=dp) function nondimensionalTime(wspd,time,grav)
        !! Returns nondimensional time (duration) based on input wind speed, duration,
        !! and gravitational acceleration.
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: nondimensionalTie
          real(kind=dp),intent(in) :: wspd
            !! Wind speed at reference height [m/s]
          real(kind=dp),intent(in) :: time
            !! Time [s]
          real(kind=dp),intent(in) :: grav
            !! Gravitational acceleration [m/s^2]
            nondimensionalTime = grav*time/wspd
    end function nondimensionalTime
!-------------------------------------------------------------------------------

end module mod_nondimensional
