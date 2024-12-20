module math_funcs_mod
!!==============================================================================
! This module defines various mathematical functions
!  
!  
! Last edited: November 9th 2020.
!!==============================================================================

  !!==============!!
  ! Use statements !
  !================!============================================================
  use working_precision, only: wp
  use constants_mod    , only: I_IMAG
  use constants_mod    , only: ZERO
  use is_close_mod     , only: is_close

  implicit none

  
  !!=================================!!
  ! Public types/procedures/constants !
  !===================================!=========================================
  public :: cross_prod_3D
  public :: cross_prod_3D_cmplx
  public :: standard_basis
  public :: scale_with_1
  public :: first_component
  public :: plane_wave
  
  !!==================================!!
  ! Private types/procedures/constants !
  !====================================!========================================
  private

  !!------------------------!!
  ! Derived type definitions !
  !--------------------------!--------------------------------------------------


  !!---------!!
  ! Main type !
  !-----------!-----------------------------------------------------------------
  
  !=======!=========================!==========================================!
contains  ! /\/\/\/\/\/\/\/\/\/\/\/\!/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\!
  !=======!=========================!==========================================!

  
  !!=================!!
  ! Public procedures !
  !===================!=========================================================

  function cross_prod_3D(a, b) result(return_value)
    ! Computes the cross product a x b, for a, b in R^3
    real(wp), dimension(3) :: a
    real(wp), dimension(3) :: b
    real(wp), dimension(3) :: return_value
    return_value(1) = a(2)*b(3) - a(3)*b(2)
    return_value(2) = a(3)*b(1) - a(1)*b(3)
    return_value(3) = a(1)*b(2) - a(2)*b(1)
  end function cross_prod_3D
  
  !!----------------------------------------------------------------------------

  function cross_prod_3D_cmplx(a, b) result(return_value)
    ! Computes the cross product a x b, for a, b in R^3
    complex(wp), dimension(3) :: a
    complex(wp), dimension(3) :: b
    complex(wp), dimension(3) :: return_value
    return_value(1) = a(2)*b(3) - a(3)*b(2)
    return_value(2) = a(3)*b(1) - a(1)*b(3)
    return_value(3) = a(1)*b(2) - a(2)*b(1)
  end function cross_prod_3D_cmplx
  
  !-----------------------------------------------------------------------------
  
  function standard_basis(vector) result(return_value)
    real(wp), dimension(:), intent(in)     :: vector
    complex(wp), dimension(:), allocatable :: return_value
    integer                             :: i
    allocate(return_value(size(vector)))
    do i = 1, size(vector)
      return_value(:) = cmplx(1._wp, 0._wp)
   end do
  end function standard_basis

  !!----------------------------------------------------------------------------

  function scale_with_1(vector) result(return_value)
    real(wp), dimension(:), intent(in)     :: vector
    complex(wp), dimension(:), allocatable :: return_value
    integer                                :: i
    allocate(return_value(size(vector)))
    do i = 1, size(vector)
       return_value(i) = cmplx(vector(i), 0._wp)
    end do
  end function scale_with_1
    
  !!----------------------------------------------------------------------------

  function first_component(vector) result(return_value)
    real(wp), dimension(:), intent(in)     :: vector
    complex(wp), dimension(:), allocatable :: return_value
    integer                                :: i
    allocate(return_value(size(vector)))
    return_value(1) = cmplx(vector(1), 0._wp)
    do i = 1, size(vector)
       if (i /= 1) then
          return_value(i) = cmplx(vector(i), 0._wp)
       end if
    end do
  end function first_component
 
  !!----------------------------------------------------------------------------

  function plane_wave(&
       r                , &
       wavenumber       , &
       amplitude        , &
       direction        , &
       angular_frequency, &
       time)              &
       result(return_value)
    ! Direction must be normalised to unity
    real(wp), dimension(:)   , intent(in)  :: r ! position-vector
    complex(wp)              , intent(in)  :: wavenumber
    complex(wp), dimension(:), intent(in)  :: amplitude
    real(wp)   , dimension(:), intent(in)  :: direction
    real(wp)   , optional    , intent(in)  :: angular_frequency
    real(wp)   , optional    , intent(in)  :: time
    complex(wp), dimension(:), allocatable :: return_value
    ! Variables for internal use -----------------------------------------------
    complex(wp)                            :: exponential
    complex(wp)                            :: kr
    real(wp)                               :: wt
    integer                                :: spatial_dim
    integer                                :: i
    
!!$    print *, 'r:', r
    spatial_dim = size(r)
    allocate(return_value(spatial_dim))

    kr = wavenumber*dot_product(r, direction)
    if (is_close(wavenumber%im, ZERO)) then
       exponential = cmplx(cos(kr%re), sin(kr%re))
    else
       exponential = exp(I_IMAG*kr)
    end if
    if (present(angular_frequency) .and. present(time)) then
       wt = angular_frequency*time
       exponential = exponential*cmplx(cos(wt), -sin(wt))
    end if
    do i = 1, spatial_dim
       return_value(i) = amplitude(i)*exponential
    end do
  end function plane_wave

  !--------------!--------------------------------------------------------------
end module math_funcs_mod
