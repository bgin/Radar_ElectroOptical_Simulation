! fplot_axis.f90

submodule (fplot_core) fplot_axis
contains
! ******************************************************************************
! X_AXIS MEMBERS
! ------------------------------------------------------------------------------
    module function xa_get_id(this) result(x)
        class(x_axis), intent(in) :: this
        character(len = :), allocatable :: x
        x = this%m_id
    end function

! ******************************************************************************
! Y_AXIS MEMBERS
! ------------------------------------------------------------------------------
    module function ya_get_id(this) result(x)
        class(y_axis), intent(in) :: this
        character(len = :), allocatable :: x
        x = this%m_id
    end function

! ******************************************************************************
! Y2_AXIS MEMBERS
! ------------------------------------------------------------------------------
    module function y2a_get_id(this) result(x)
        class(y2_axis), intent(in) :: this
        character(len = :), allocatable :: x
        x = this%m_id
    end function

! ******************************************************************************
! Z_AXIS MEMBERS
! ------------------------------------------------------------------------------
    module function za_get_id(this) result(x)
        class(z_axis), intent(in) :: this
        character(len = :), allocatable :: x
        x = this%m_id
    end function

end submodule