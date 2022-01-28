! fplot_plot_data_colored.f90

submodule (fplot_core) fplot_plot_data_colored
contains
! ------------------------------------------------------------------------------
    pure module function pdc_get_line_color(this) result(x)
        class(plot_data_colored), intent(in) :: this
        type(color) :: x
        if (this%m_useAutoColor) then
            x = color_list(this%get_color_index())
        else
            x = this%m_color
        end if
    end function

! --------------------
    module subroutine pdc_set_line_color(this, x)
        class(plot_data_colored), intent(inout) :: this
        type(color), intent(in) :: x
        this%m_color = x
        this%m_useAutoColor = .false.
    end subroutine

! ------------------------------------------------------------------------------
    pure module function pdc_get_color_index(this) result(x)
        class(plot_data_colored), intent(in) :: this
        integer(int32) :: x
        x = this%m_colorIndex
    end function

! --------------------
    module subroutine pdc_set_color_index(this, x)
        class(plot_data_colored), intent(inout) :: this
        integer(int32), intent(in) :: x
        this%m_colorIndex = x
    end subroutine

! ------------------------------------------------------------------------------
end submodule
