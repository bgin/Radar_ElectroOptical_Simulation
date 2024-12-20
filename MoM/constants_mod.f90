module constants_mod
!!==============================================================================
! This module defines various mathematical constants
!  
!  
! Last edited: November 27th 2020.
!!==============================================================================

  !!==============!!
  ! Use statements !
  !================!============================================================
  use working_precision, only: wp

  implicit none

  
  !!=================================!!
  ! Public types/procedures/constants !
  !===================================!=========================================
  
  ! Constants
  complex(wp), parameter, public :: I_IMAG = cmplx(0._wp, 1._wp)
  complex(wp), parameter, public :: ZERO_CMPLX = cmplx(0._wp, 0._wp)
  real(wp)   , parameter, public :: PERMEABILITY_VACUUM = 1.25663706212e-6_wp
  real(wp)   , parameter, public :: PERMITIVITY_VACUUM = 8.8541878128e-12_wp
  real(wp)   , parameter, public :: LIGHTSPEED_VACUUM = 299792458._wp
  real(wp)   , parameter, public :: PI = 4._wp*atan(1._wp)
  real(wp)   , parameter, public :: PI4_INV = 1._wp/(4._wp*PI)
  real(wp)   , parameter, public :: ZERO = 0._wp
  real(wp)   , parameter, public :: UNITY = 1._wp
  
  !!==================================!!
  ! Private types/procedures/constants !
  !====================================!========================================
  private

  !!------------------------!!
  ! Derived type definitions !
  !--------------------------!--------------------------------------------------

  
  !=======!=========================!==========================================!
contains  ! /\/\/\/\/\/\/\/\/\/\/\/\!/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\!
  !=======!=========================!==========================================!

  
  !!=================!!
  ! Public procedures !
  !===================!=========================================================


  !!------------!!
  ! Constructors !
  !--------------!--------------------------------------------------------------


  !!==================================!!
  ! RWG_basis_type internal procedures !
  !====================================!========================================
  !!------------!!
  ! Initialisers !
  !--------------!--------------------------------------------------------------
  
end module constants_mod
