! fplot_plot_data.f90

submodule (fplot_core) fplot_plot_data
contains
! ------------------------------------------------------------------------------
    !> @brief Gets the name to associate with this data set.
    !!
    !! @param[in] this The plot_data object.
    !! @return The name.
    pure module function pd_get_name(this) result(txt)
        class(plot_data), intent(in) :: this
        character(len = :), allocatable :: txt
        txt = trim(this%m_name)
    end function

! --------------------
    !> @brief Sets the name to associate with this data set.
    !!
    !! @param[in,out] this The plot_data object.
    !! @param[in] txt The name.
    module subroutine pd_set_name(this, txt)
        class(plot_data), intent(inout) :: this
        character(len = *), intent(in) :: txt
        integer(int32) :: n
        n = min(len(txt), PLOTDATA_MAX_NAME_LENGTH)
        this%m_name = ""
        if (n /= 0) then
            this%m_name(1:n) = txt(1:n)
        end if
    end subroutine

end submodule