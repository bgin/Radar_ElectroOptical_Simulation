module is_close_mod
!!==============================================================================
! This module defines the is_close funcion useful for comparing real numbers.
!
!
! Last edited: November 25th 2020.
!!==============================================================================

  !!==============!!
  ! Use statements !
  !================!============================================================
  use working_precision, only: wp
  use ieee_arithmetic  , only: ieee_is_nan
  use ieee_arithmetic  , only: ieee_is_finite
  implicit none


  !!=================================!!
  ! Public types/procedures/constants !
  !===================================!=========================================
  public :: is_close
   

  !!=====================!!
  ! Procedure definitions !
  !=======================!=====================================================

  !=======!=========================!==========================================!
contains  ! /\/\/\/\/\/\/\/\/\/\/\/\!/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\!
  !=======!=========================!==========================================!


  elemental function is_close(actual, desired, rtol, atol, equal_nan)&
       result(return_value)
    ! The following code is snipped from:
    !  https://scivision.github.io/fortran2018-examples/sourcefile/
    !    assert.f90.html
    !!--------------------------------------------------------------------------
    ! inputs
    ! ------
    ! actual: value "measured"
    ! desired: value "wanted"
    ! rtol: relative tolerance
    ! atol: absolute tolerance
    ! equal_nan: consider NaN to be equal?
    !
    !  rtol overrides atol when both are specified
    !
    ! https://www.python.org/dev/peps/pep-0485/#proposed-implementation
    ! https://github.com/PythonCHB/close_pep/blob/master/is_close.py
    
    real(wp), intent(in)           :: actual
    real(wp), intent(in)           :: desired
    real(wp), intent(in), optional :: rtol
    real(wp), intent(in), optional :: atol
    logical , intent(in), optional :: equal_nan
    logical                        :: return_value
    ! Variables for internal use -----------------------------------------------
    real(wp)                       :: r
    real(wp)                       :: a
    logical                        :: n
    real(wp)                       :: zero_margin
    real(wp)                       :: adjustment

    ! this is appropriate INSTEAD OF merge(), since non present values aren't
    ! defined.
    r = 1e-5_wp
    a = 1.e4_wp*epsilon(0._wp) ! 1.e3
    n = .false.
    if (present(rtol)) r = rtol
    if (present(atol)) a = atol
    if (present(equal_nan)) n = equal_nan
    
    zero_margin = 0.1_wp
    adjustment = 0._wp
    
    !--- Check if desired and actual is close to zero. Added by E. S. Oyre
!!$    if ((abs(desired) < zero_margin) .and. (abs(actual) < zero_margin)) then
!!$       adjustment = zero_margin
!!$    end if
    !--- sanity check
    if ((r < 0._wp).or.(a < 0._wp)) error stop
    !--- simplest case
    return_value = (actual == desired) 
    if (return_value) return
    !--- equal nan
    return_value = n.and.(ieee_is_nan(actual).and.ieee_is_nan(desired))
    if (return_value) return
    !--- Inf /= -Inf, unequal NaN
    if (.not.ieee_is_finite(actual) .or. .not.ieee_is_finite(desired)) return
    !--- floating point closeness check
!!$    return_value = abs(actual-desired) <= max(r * max(abs(actual + adjustment),&
!!$         abs(desired + adjustment)), a)
    return_value = abs(actual-desired) <= max(r * max(abs(actual),&
         abs(desired)), a)
    !!--------------------------------------------------------------------------
    
  end function is_close



!!------------------------------------------------------------------------------
end module is_close_mod



