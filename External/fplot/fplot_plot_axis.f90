! fplot_plot_axis.f90

submodule (fplot_core) fplot_plot_axis
contains
! ------------------------------------------------------------------------------
    module function pa_get_title(this) result(txt)
        class(plot_axis), intent(in) :: this
        character(len = :), allocatable :: txt
        integer(int32) :: n
        n = len_trim(this%m_title)
        allocate(character(len = n) :: txt)
        txt = trim(this%m_title)
    end function

! --------------------
    module subroutine pa_set_title(this, txt)
        ! Arguments
        class(plot_axis), intent(inout) :: this
        character(len = *), intent(in) :: txt

        ! Local Variables
        integer(int32) :: n

        ! Process
        n = min(len_trim(txt), PLOTDATA_MAX_NAME_LENGTH)
        this%m_title = ""
        if (n /= 0) then
            this%m_title(1:n) = txt(1:n)
            this%m_hasTitle = .true.
        else
            this%m_hasTitle = .false.
        end if
    end subroutine

! ------------------------------------------------------------------------------
    pure module function pa_has_title(this) result(x)
        class(plot_axis), intent(in) :: this
        logical :: x
        x = this%m_hasTitle
    end function

! ------------------------------------------------------------------------------
    pure module function pa_get_autoscale(this) result(x)
        class(plot_axis), intent(in) :: this
        logical :: x
        x = this%m_autoscale
    end function

! --------------------
    module subroutine pa_set_autoscale(this, x)
        class(plot_axis), intent(inout) :: this
        logical, intent(in) :: x
        this%m_autoscale = x
    end subroutine

! ------------------------------------------------------------------------------
    pure module function pa_get_axis_limits(this) result(x)
        class(plot_axis), intent(in) :: this
        real(real64), dimension(2) :: x
        x(1) = minval(this%m_limits)
        x(2) = maxval(this%m_limits)
    end function

! --------------------
    module subroutine pa_set_axis_limits(this, lower, upper)
        class(plot_axis), intent(inout) :: this
        real(real64), intent(in) :: lower, upper
        this%m_limits(1) = lower
        this%m_limits(2) = upper
    end subroutine

! ------------------------------------------------------------------------------
    pure module function pa_get_log_scale(this) result(x)
        class(plot_axis), intent(in) :: this
        logical :: x
        x = this%m_logScale
    end function

! --------------------
    module subroutine pa_set_log_scale(this, x)
        class(plot_axis), intent(inout) :: this
        logical, intent(in) :: x
        this%m_logScale = x
    end subroutine

! ------------------------------------------------------------------------------
    module function pa_get_cmd_string(this) result(txt)
        ! Arguments
        class(plot_axis), intent(in) :: this
        character(len = :), allocatable :: txt

        ! Local Variables
        type(string_builder) :: str
        character(len = :), allocatable :: axis
        real(real64) :: lim(2)

        ! Process
        axis = this%get_id_string()
        lim = this%get_limits()
        call str%initialize()

        ! Axis Limits
        if (this%get_autoscale()) then
            call str%append("set ")
            call str%append(axis)
            call str%append("range [*:*]")
        else
            call str%append("set ")
            call str%append(axis)
            call str%append("range [")
            call str%append(to_string(lim(1)))
            call str%append(":")
            call str%append(to_string(lim(2)))
            call str%append("]")
        end if

        ! Titles
        call str%append(new_line('a'))
        if (this%is_title_defined()) then
            call str%append("set ")
            call str%append(axis)
            call str%append("label ")
            call str%append('"')
            call str%append(this%get_title())
            call str%append('"')
        else
            call str%append("set ")
            call str%append(axis)
            call str%append("label ")
            call str%append('""')
        end if

        ! Scaling
        call str%append(new_line('a'))
        if (this%get_is_log_scaled()) then
            call str%append("set log ")
            call str%append(axis)
        else
            call str%append("unset log ")
            call str%append(axis)
        end if

        ! Zero Axis
        if (this%get_zero_axis()) then
            call str%append(new_line('a'))
            call str%append("set ")
            call str%append(this%get_id_string())
            call str%append("zeroaxis linestyle -1 linewidth ")
            call str%append(to_string(this%get_zero_axis_line_width()))
        end if

        ! Output
        txt = str%to_string()
    end function

! ------------------------------------------------------------------------------
    pure module function pa_get_zero_axis(this) result(x)
        class(plot_axis), intent(in) :: this
        logical :: x
        x = this%m_zeroAxis
    end function

! --------------------
    module subroutine pa_set_zero_axis(this, x)
        class(plot_axis), intent(inout) :: this
        logical, intent(in) :: x
        this%m_zeroAxis = x
    end subroutine

! ------------------------------------------------------------------------------
    pure module function pa_get_zero_axis_width(this) result(x)
        class(plot_axis), intent(in) :: this
        real(real32) :: x
        x = this%m_axisWidth
    end function

! --------------------
    module subroutine pa_set_zero_axis_width(this, x)
        class(plot_axis), intent(inout) :: this
        real(real32), intent(in) :: x
        this%m_axisWidth = x
    end subroutine

end submodule