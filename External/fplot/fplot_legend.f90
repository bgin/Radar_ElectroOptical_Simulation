! fplot_legend.f90

submodule (fplot_core) fplot_legend
contains
! ------------------------------------------------------------------------------
    pure module function leg_get_inside(this) result(x)
        class(legend), intent(in) :: this
        logical :: x
        x = this%m_inside
    end function

! ---------------------
    module subroutine leg_set_inside(this, x)
        class(legend), intent(inout) :: this
        logical, intent(in) :: x
        this%m_inside = x
    end subroutine

! ------------------------------------------------------------------------------
    pure module function leg_get_box(this) result(x)
        class(legend), intent(in) :: this
        logical :: x
        x = this%m_box
    end function

! ---------------------
    module subroutine leg_set_box(this, x)
        class(legend), intent(inout) :: this
        logical, intent(in) :: x
        this%m_box = x
    end subroutine

! ------------------------------------------------------------------------------
    module function leg_get_horz_pos(this) result(x)
        class(legend), intent(in) :: this
        character(len = :), allocatable :: x
        integer(int32) :: n
        n = len_trim(this%m_horzPosition)
        allocate(character(len = n) :: x)
        x = trim(this%m_horzPosition)
    end function

! ---------------------
    module subroutine leg_set_horz_pos(this, x)
        class(legend), intent(inout) :: this
        character(len = *), intent(in) :: x
        this%m_horzPosition = x
        if (x /= LEGEND_LEFT .and. x /= LEGEND_RIGHT .and. x /= LEGEND_CENTER) &
            this%m_horzPosition = LEGEND_RIGHT
    end subroutine

! ------------------------------------------------------------------------------
    module function leg_get_vert_pos(this) result(x)
        class(legend), intent(in) :: this
        character(len = :), allocatable :: x
        integer(int32) :: n
        n = len_trim(this%m_vertPosition)
        allocate(character(len = n) :: x)
        x = trim(this%m_vertPosition)
    end function

! ---------------------
    module subroutine leg_set_vert_pos(this, x)
        class(legend), intent(inout) :: this
        character(len = *), intent(in) :: x
        this%m_vertPosition = x
        if (x /= LEGEND_TOP .and. x /= LEGEND_CENTER .and. x /= LEGEND_BOTTOM) &
            this%m_vertPosition = LEGEND_TOP
    end subroutine

! ------------------------------------------------------------------------------
    pure module function leg_get_visible(this) result(x)
        class(legend), intent(in) :: this
        logical :: x
        x = this%m_show
    end function

! ---------------------
    module subroutine leg_set_visible(this, x)
        class(legend), intent(inout) :: this
        logical, intent(in) :: x
        this%m_show = x
    end subroutine

! ------------------------------------------------------------------------------
    module function leg_get_command_txt(this) result(txt)
        ! Arguments
        class(legend), intent(in) :: this
        character(len = :), allocatable :: txt

        ! Local Variables
        type(string_builder) :: str

        ! Process
        call str%initialize()

        ! Visible?
        if (.not.this%get_is_visible()) then
            txt = "set key off"
            return
        end if

        ! Inside vs Outside & Position
        if (this%get_draw_inside_axes()) then
            call str%append("set key inside")
        else
            call str%append("set key outside")
        end if
        call str%append(" ")
        call str%append(this%get_vertical_position())
        call str%append(" ")
        call str%append(this%get_horizontal_position())

        ! Border
        call str%append(new_line('a'))
        if (this%get_draw_border()) then
            call str%append("set key box opaque")
        else
            call str%append("set key nobox")
        end if

        ! End
        txt = str%to_string()
    end function

end submodule
