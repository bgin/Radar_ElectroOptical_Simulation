module GMS_mod_spectral_shapes

!
! wavy - A spectral ocean wave modeling and development framework
! Copyright (c) 2017, Wavebit Scientific LLC
! All rights reserved.
!
! Licensed under the BSD-3 clause license. See LICENSE for details.
! Modified by Bernard Gingold (beniekg@gmail.com) on 15/03/2019

    use mod_kinds,          only : i4, dp
    use GMS_mod_constants,      only : pi2_const
    use GMS_mod_nondimensional, only : nondimensionalFetch,nondimensionalFrequency
    implicit none

    private

    public :: donelanHamiltonHui
    public :: donelanHamiltonHuiDirectionalSpreading
    public :: donelanHamiltonHuiDirectionalSpectrum
    public :: jonswap
    public :: jonswapPeakFrequency
    public :: piersonMoskowitz
    public :: piersonMoskowitzPeakFrequency
    public :: phillips

    contains

!DIR$   ATTRIBUTES INLINE :: donelanHamiltonHui
    pure elemental function real(kind=dp) function donelanHamiltonHui(f,fpeak,wspd,grav)  &
            result(spec)
  !! The omnidirectional spectrum function based on the laboratory and field
  !! measurements by Donelan, Hamilton, and Hui (1985).
  !!
  !! References:
  !!
  !! Donelan, M. A., J. Hamilton, and W. H. Hui, 1985. Directional
  !! spectra of wind-generated waves. Phil. Trans. Royal Soc. London A.,
  !! 315, 509-562.
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: donelanHamiltonHui
          real(kind=dp), intent(in) :: f
          real(kind=dp), intent(in) :: fpeak
          real(kind=dp), intent(in) :: wspd
          real(kind=dp), intent(in) :: grav
          ! LOcals
          real(kind=dp), automatic :: beta
          real(kind=dp), automatic :: gamma
          real(kind=dp), automatic :: nu
          real(kind=dp), automatic :: omega
          real(kind=dp), automatic :: sigma
          real(kind=dp) :: spec
          ! EXec code .....
          omega = pi2_const*f
          nu = nondimensionalFRequency(wspd,fpeak,grav)
          if(nu >= 0.159_dp) then
                gamma = 6.489_dp+6.0_dp*log(nu)
                if(gamma <0.0_dp) gamma = 0.0_dp
          else
                gamma = 1.7_dp
          endif
          sigma = 0.08_dp+1.29e-3_dp*nu**(-3)
          r     = exp(-0.5_dp*((f-fpeak)/(sigma*fpeak))**2)
          beta  = 0.0165_dp*nu**0.55_dp
          spec  = pi2_const*beta*grav**2/omega**4/fpeak*exp(-(fpeak/f)**4)*gamma**r
    end function donelanHamiltonHui

!DIR$   ATTRIBUTES INLINE :: donelanHamiltonHuiDirectionalSpreading
    pure elemental function donelanHamiltonHuiDirectionalSpreading(f, &
                        wspd,fpeak,theta,theat_mean)    result(spreading)
  !! Directional spreading function based on the laboratory and field
  !! measurements by Donelan, Hamilton, and Hui (1985). Includes the
  !! high-frequency form for beta_s found by Banner (1990).
  !!
  !! References:
  !!
  !! Donelan, M. A., J. Hamilton, and W. H. Hui, 1985. Directional
  !! spectra of wind-generated waves. *Phil. Trans. Royal Soc. London A.*,
  !! **315**, 509-562.
  !!
  !! Banner, M. L., 1990. Equilibrium spectra of wind waves. *J. Phys.
  !! Oceanogr.*, **20**, 966-984.
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: donelanHamiltonHuiDirectionalSpreading
          real(kind=dp), intent(in) :: f
          real(kind=dp), intent(in) :: wspd
          real(kind=dp), intent(in) :: fpeak
          real(kind=dp), intent(in) :: theta
          real(kind=dp), intent(in) :: theta_mean
          ! Locals
          real(kind=dp), automatic :: frel
          real(kind=dp), automatic :: nu
          real(kind=dp), automatic :: beta
          real(kind=dp) :: spreading
          ! Exec code ....
          frel = f/fpeak
          if(frel < 0.56_dp) then
                beta = 1.24_dp
          else if(frel >= 0.56_dp .and. frel <= 0.95_dp) then
                beta = 2.61_dp*frel**1.3_dp
          else if(frel >= 0.95_dp .and. frel <= 1.6_dp) then
                beta = 2.28_dp*frel**(-1.3_dp)
          else
                beta =  10**(-0.4_dp+0.8393_dp*exp(-0.567_dp*log(frel*frel)))
          end if
          spreading = 0.5_dp*beta/cosh(beta*(theta-theta_mean))**2

   end function donelanHamiltonHuiDirectionalSpreading

   subroutine donelanHamiltonHuiDirectionalSpectrum(f,theta,spec,nf,ntheta,wspd,fpeak, &
                theta_mean,grav)
  !! Returns directional frequency spectrum based on the laboratory and field
  !! measurements by Donelan, Hamilton, and Hui (1985). Includes the high
  !! frequency form for beta_s found by Banner (1990). This function invokes the
  !!  DHH omnidirectional spectrum and the directional spreading functions to
  !! compute directional frequency spectrum:
  !!
  !! $$
  !!     F(f,\theta) = F'(f) * D(f,\theta)
  !! $$
  !!
  !! References:
  !!
  !! Donelan, M. A., J. Hamilton, and W. H. Hui, 1985. Directional
  !! spectra of wind-generated waves. Phil. Trans. Royal Soc. London A.,
  !! 315, 509-562.
  !!
  !! Banner, M. L., 1990. Equilibrium spectra of wind waves. J. Phys. Oceanogr.,
  !! 20, 966-984.
!DIR$   ATTRIBUTES CODE_ALIGN:32 ::  donelanHamiltonHuiDirectionalSpectrum
!DIR$   ASSUME_ALIGNED f:64
          real(kind=dp), dimension(nf),        intent(in)  :: f
!DIR$   ASSUME_ALIGNED theta:64
          real(kind=dp), dimension(ntheta),    intent(in)  :: theta
!DIR$   ASSUME_ALIGNED  spec:64
          real(kind=dp), dimension(nf,ntheta), intent(out) :: spec
          integer(kind=i4),               intent(in)  :: nf
          integer(kind=i4),               intent(in)  :: ntheta
          real(kind=dp),                       intent(in)  :: wspd
          real(kind=dp),                       intent(in)  :: fpeak
          real(kind=dp),                       intent(in)  :: theta_mean
          real(kind=dp),                       intent(in)  :: grav
          ! Locals
          integer(kind=i4) :: ndir
          ! EXec code ....
          do concurrent(ndir=1:ntheta)
                spec(:,ndir) = donelanHamiltonHui(f,fpeak,wspd,grav) &
                        *donelanHamiltonHuiDirectionalSpreading(f,wspd,fpeak,theta(ndir), &
                            theta_mean)
          end do

   end subroutine donelanHamiltonHuiDirectionalSpectrum

!DIR$ ATTRIBUTES INLINE :: jonswap
    pure elemental function jonswap(f,wspd,fetch,grav) result(spec)
  !! Computes the JONSWAP equilibrium spectrum (Hasselmann et al. 1973) based on
  !!  input wind speed at the height of 10 m and fetch.
  !!
  !! References:
  !!
  !! Hasselmann, K. et al., 1973. Measurements of wind-wave growth and swell
  !! decay during the Joint North Sea Wave Project (JONSWAP). Dtsch. Hydrogh.
  !! Z., Suppl. A, 8, 12, 95pp.
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: jonswap
          real(kind=dp),    intent(in) :: f
          real(kind=dp),    intent(in) :: wspd
          real(kind=dp),    intent(in) :: fetch
          real(kind=dp),    intet(in)  :: grav
          ! Locals
          real(kind=dp),    automatic :: alpha
          real(kind=dp),    automatic :: sigma
          real(kind=dp),    automatic :: r
          real(kind=dp),    automatic :: fpeak
          real(kind=dp),    automatic :: omega
          real(kind=dp),    parameter, automatic :: beta = 1.25_dp
          real(kind=dp),    parameter, automatic :: gamma = 3.3_dp
          real(kind=dp) :: spec
          ! EXec code ....
          spec = 0.0_dp
          omega = pi2_const*f
          alpha = 0.076_dp*nondimensionalFetch(wspd,fetch,grav)**(-0.22_dp)
          fpeak = jonswapPeakFrequency(wspd,fetch,grav)
          if(f > fpeak) then
               sigma = 0.09_dp
          else
               sigma = 0.07_dp
          end if
          r = exp(-0.5_dp*((f-fpeak)/(sigma*fpeak))**2)
          spec = pi2_const*alpha*grav**2/omega**5*exp(beta*(fpeak/f)**4)*gamma**r

    end function jonswap

!DIR$   ATTRIBUTES INLINE :: jonswapPeakFrequency
    pure elemental function jonswapPeakFrequency(wspd,fetch,grav) result(fpeak)
  !! Computes the JONSWAP equilibrium peak frequency [Hz] on the input
  !! based on the 10-m wind speed and fetch [km] (Hasselmann et al., 1973).
  !!
  !! References:
  !!
  !! Hasselmann, K. et al., 1973. Measurements of wind-wave growth and swell
  !! decay during the Joint North Sea Wave Project (JONSWAP). Dtsch. Hydrogh.
  !! Z., Suppl. A, 8, 12, 95pp.
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: jonswapPeakFrequency
          real(kind=dp),    intent(in) :: wspd
          real(kind=dp),    intent(in) :: fetch
          real(kind=dp),    intent(in) :: grav
          ! LOcals
          real(kind=dp) :: fpeak
          real(kind=dp), parameter, automatic :: a = 3.5_dp
          real(kind=dp), parameter, automatic :: b = 0.3333333333333333333333333_dp
          ! EXec code
          fpeak = a*(grav**2/(wspd*fetch))**b

    end function jonswapPeakFrequency

!DIR$   ATTRIBUTES INLINE :: phillips
    pure elemental function phillips(f,fpeak,grav) result(spec)
  !! Computes the Phillips (1958) equilibrium spectrum based on the input
  !! peak frequency [Hz].
  !!
  !! References:
  !!
  !! Phillips, O.M., 1958. The equilibrium range in the spectrum of
  !! wind-generated waves. J. Fluid Mech., 4, 426–434.
  !! doi:10.1017/S0022112058000550.
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: phillips
          real(kind=dp),    intent(in) :: f
          real(kind=dp),    intent(in) :: fpeak
          real(kind=dp),    intent(in) :: grav
          ! Locals
          real(kind=dp), parameter, automatic :: alpha = 8.13e-3_dp
          real(kind=dp) :: spec
          ! Exec code ....
          if(f < fpeak) then
                spec = 0.0_dp
          else
                spec = alpha*grav**2*(pi2_const*f)**(-5)
          end if

    end function phillips

!DIR$   ATTRIBUTES INLINE :: piersonMoskovitz
    pure elemental function piersonMoskovitz(f,wspd,grav) result(spec)
  !! Computes the Pierson-Moskowitz (1964) equilibrium spectrum based on input
  !! wind speed at the height of 10 m.
  !!
  !! References:
  !!
  !! Pierson Jr., W. J., and L. Moskowitz (1964), A proposed spectral form for
  !! fully developed wind seas based on the similarity theory of S. A.
  !! Kitaigorodskii, J. Geophys. Res., 69(24), 5181–5190,
  !! doi:10.1029/JZ069i024p05181.
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: piersonMoskovitz
          real(kind=dp),    intent(in) :: f
          real(kind=dp),    intent(in) :: wspd
          real(kind=dp),    intent(in) :: grav
          ! Locals
          real(kind=dp), automatic ::  omega
          real(kind=dp), automatic ::  fpeak
          real(kind=dp) :: spec
          real(kind=dp), parameter, automatic :: alpha = 8.1e-3_dp
          real(kind=dp), parameter, automatic :: beta = -1.25_dp
          ! Exec code ....
          omega = pi2_const*f
          fpeak = piersonMoskovitzPeakFrequency(wspd,grav)
          spec  = pi2_const*alpha*grav**2/omega**5*exp(beta*(fpeak/f)**4)

    end function piersonMoskovitz

!DIR$   ATTRIBUTES INLINE :: piersonMoskovitzPeakFrequency
    pure elemental function piersonMoskovitzPeakFrequency(wspd,grav) result(fpeak)
  !! Computes the Pierson-Moskowitz (1964) peak frequency based on input wind
  !! speed at the height of 10 m.
  !!
  !! References:
  !!
  !! Pierson Jr., W. J., and L. Moskowitz (1964), A proposed spectral form for
  !! fully developed wind seas based on the similarity theory of S. A.
  !! Kitaigorodskii, J. Geophys. Res., 69(24), 5181–5190,
  !! doi:10.1029/JZ069i024p05181.
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: piersonMoskovitzPeakFrequency
          real(kind=dp),    intent(in) :: wspd
          real(kind=dp),    intent(in) :: grav
          ! LOcals
          real(kind=dp) :: fpeak
          real(kind=dp), parameter, automatic :: const = 0.1325_dp
          ! EXec code ...
          fpeak = 0.0_dp
          fpeak = const*grav/max(wspd,1.0e-2_dp)

    end function piersonMoskovitzPeakFrequency


end module GMS_mod_spectral_shapes
