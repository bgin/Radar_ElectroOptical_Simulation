! fplot_scatter_plot_data.f90

submodule (fplot_core) fplot_scatter_plot_data
contains
! ------------------------------------------------------------------------------
    module function spd_get_cmd(this) result(x)
        ! Arguments
        class(scatter_plot_data), intent(in) :: this
        character(len = :), allocatable :: x

        ! Local Variables
        type(string_builder) :: str
        integer(int32) :: n
        type(color) :: clr

        ! Initialization
        call str%initialize()

        ! Title
        n = len_trim(this%get_name())
        if (n > 0) then
            call str%append(' "-" title "')
            call str%append(this%get_name())
            call str%append('"')
        else
            call str%append(' "-" notitle')
        end if

        ! Lines or points?
        if (this%get_draw_line() .and. this%get_draw_markers()) then
            call str%append(" with linespoints")
        else if (.not.this%get_draw_line() .and. this%get_draw_markers()) then
            call str%append(" with points")
        else
            call str%append(" with lines")
        end if

        ! Line Width
        call str%append(" lw ")
        call str%append(to_string(this%get_line_width()))

        ! Line Color
        clr = this%get_line_color()
        call str%append(' lc rgb "#')
        call str%append(clr%to_hex_string())
        call str%append('"')

        ! Define other properties specific to the lines and points
        if (this%get_draw_line()) then
            call str%append(" lt ")
            call str%append(to_string(this%get_line_style()))
            if (this%get_line_style() /= LINE_SOLID) then
                call str%append(" dashtype ")
                call str%append(to_string(this%get_line_style()))
            end if
        end if
        if (this%get_draw_markers()) then
            call str%append(" pi ")
            call str%append(to_string(this%get_marker_frequency()))
            call str%append(" pt ")
            call str%append(to_string(this%get_marker_style()))
            call str%append(" ps ")
            call str%append(to_string(this%get_marker_scaling()))
        end if

        ! Define the axes structure
        call str%append(" ")
        call str%append(this%get_axes_string())

        ! End
        x = str%to_string()
    end function

! ------------------------------------------------------------------------------
    pure module function spd_get_line_width(this) result(x)
        class(scatter_plot_data), intent(in) :: this
        real(real32) :: x
        x = this%m_lineWidth
    end function

! --------------------
    module subroutine spd_set_line_width(this, x)
        class(scatter_plot_data), intent(inout) :: this
        real(real32), intent(in) :: x
        this%m_lineWidth = x
    end subroutine

! ------------------------------------------------------------------------------
    pure module function spd_get_line_style(this) result(x)
        class(scatter_plot_data), intent(in) :: this
        integer(int32) :: x
        x = this%m_lineStyle
    end function

! --------------------
    module subroutine spd_set_line_style(this, x)
        class(scatter_plot_data), intent(inout) :: this
        integer(int32), intent(in) :: x
        if (x == LINE_DASHED .or. &
            x == LINE_DASH_DOTTED .or. &
            x == LINE_DASH_DOT_DOT .or. &
            x == LINE_DOTTED .or. &
            x == LINE_SOLID) then
            ! Only reset the line style if it is a valid type.
            this%m_lineStyle = x
        end if
    end subroutine

! ------------------------------------------------------------------------------
    pure module function spd_get_draw_line(this) result(x)
        class(scatter_plot_data), intent(in) :: this
        logical :: x
        x = this%m_drawLine
    end function

! --------------------
    module subroutine spd_set_draw_line(this, x)
        class(scatter_plot_data), intent(inout) :: this
        logical, intent(in) :: x
        this%m_drawLine = x
    end subroutine

! ------------------------------------------------------------------------------
    pure module function spd_get_draw_markers(this) result(x)
        class(scatter_plot_data), intent(in) :: this
        logical :: x
        x = this%m_drawMarkers
    end function

! --------------------
    module subroutine spd_set_draw_markers(this, x)
        class(scatter_plot_data), intent(inout) :: this
        logical, intent(in) :: x
        this%m_drawMarkers = x
    end subroutine

! ------------------------------------------------------------------------------
    pure module function spd_get_marker_style(this) result(x)
        class(scatter_plot_data), intent(in) :: this
        integer(int32) :: x
        x = this%m_markerType
    end function

! --------------------
    module subroutine spd_set_marker_style(this, x)
        class(scatter_plot_data), intent(inout) :: this
        integer(int32), intent(in) :: x
        if (x == MARKER_ASTERISK .or. &
            x == MARKER_EMPTY_CIRCLE .or. &
            x == MARKER_EMPTY_NABLA .or. &
            x == MARKER_EMPTY_RHOMBUS .or. &
            x == MARKER_EMPTY_SQUARE .or. &
            x == MARKER_EMPTY_TRIANGLE .or. &
            x == MARKER_FILLED_CIRCLE .or. &
            x == MARKER_FILLED_NABLA .or. &
            x == MARKER_FILLED_RHOMBUS .or. &
            x == MARKER_FILLED_SQUARE .or. &
            x == MARKER_FILLED_TRIANGLE .or. &
            x == MARKER_PLUS .or. &
            x == MARKER_X) then

            ! Only alter the value if the marker is a known type
            this%m_markerType = x
        end if
    end subroutine

! ------------------------------------------------------------------------------
    pure module function spd_get_marker_scaling(this) result(x)
        class(scatter_plot_data), intent(in) :: this
        real(real32) :: x
        x = this%m_markerSize
    end function

! --------------------
    module subroutine spd_set_marker_scaling(this, x)
        class(scatter_plot_data), intent(inout) :: this
        real(real32), intent(in) :: x
        this%m_markerSize = x
    end subroutine

! ------------------------------------------------------------------------------
    pure module function spd_get_marker_frequency(this) result(x)
        class(scatter_plot_data), intent(in) :: this
        integer(int32) :: x
        x = this%m_markerFrequency
    end function

! --------------------
    module subroutine spd_set_marker_frequency(this, x)
        class(scatter_plot_data), intent(inout) :: this
        integer(int32), intent(in) :: x
        this%m_markerFrequency = x
    end subroutine

! ------------------------------------------------------------------------------
    pure module function spd_get_simplify_data(this) result(x)
        class(scatter_plot_data), intent(in) :: this
        logical :: x
        x = this%m_simplifyData
    end function

! --------------------
    module subroutine spd_set_simplify_data(this, x)
        class(scatter_plot_data), intent(inout) :: this
        logical, intent(in) :: x
        this%m_simplifyData = x
    end subroutine

! ------------------------------------------------------------------------------
    pure module function spd_get_simplify_factor(this) result(x)
        class(scatter_plot_data), intent(in) :: this
        real(real64) :: x
        x = this%m_simplifyFactor
    end function

! --------------------
    module subroutine spd_set_simplify_factor(this, x)
        class(scatter_plot_data), intent(inout) :: this
        real(real64), intent(in) :: x
        this%m_simplifyFactor = x
    end subroutine

! ------------------------------------------------------------------------------
end submodule
