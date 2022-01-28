! fplot_plot_2d.f90

submodule (fplot_core) fplot_plot_2d
contains
! ------------------------------------------------------------------------------
    module subroutine p2d_clean_up(this)
        type(plot_2d), intent(inout) :: this
        call this%free_resources()
        if (associated(this%m_xAxis)) then
            deallocate(this%m_xAxis)
            nullify(this%m_xAxis)
        end if
        if (associated(this%m_yAxis)) then
            deallocate(this%m_yAxis)
            nullify(this%m_yAxis)
        end if
        if (associated(this%m_y2Axis)) then
            deallocate(this%m_y2Axis)
            nullify(this%m_y2Axis)
        end if
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine p2d_init(this, term, fname, err)
        ! Arguments
        class(plot_2d), intent(inout) :: this
        integer(int32), intent(in), optional :: term
        character(len = *), intent(in), optional :: fname
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: flag
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Initialize the base class
        call plt_init(this, term, fname, errmgr)
        if (errmgr%has_error_occurred()) return

        ! Process
        flag = 0
        if (.not.associated(this%m_xAxis)) then
            allocate(this%m_xAxis, stat = flag)
        end if
        if (flag == 0 .and. .not.associated(this%m_yAxis)) then
            allocate(this%m_yAxis, stat = flag)
        end if
        if (flag == 0 .and. .not.associated(this%m_y2Axis)) then
            allocate(this%m_y2Axis, stat = flag)
        end if

        ! Error Checking
        if (flag /= 0) then
            call errmgr%report_error("p2d_init", &
                "Insufficient memory available.", PLOT_OUT_OF_MEMORY_ERROR)
            return
        end if
    end subroutine

! ------------------------------------------------------------------------------
    module function p2d_get_cmd(this) result(x)
        ! Arguments
        class(plot_2d), intent(in) :: this
        character(len = :), allocatable :: x

        ! Local Variables
        type(string_builder) :: str
        integer(int32) :: i, n
        class(plot_data), pointer :: ptr
        class(plot_axis), pointer :: axis, xAxis, yAxis
        type(legend), pointer :: leg
        class(plot_label), pointer :: lbl

        ! Initialization
        call str%initialize()

        ! Grid
        if (this%get_show_gridlines()) then
            call str%append(new_line('a'))
            call str%append("set grid")
        end if

        ! Title
        n = len_trim(this%get_title())
        if (n > 0) then
            call str%append(new_line('a'))
            call str%append('set title "')
            call str%append(this%get_title())
            call str%append('"')
        end if

        ! Axes
        call str%append(new_line('a'))
        xAxis => this%get_x_axis()
        if (associated(xAxis)) call str%append(xAxis%get_command_string())

        call str%append(new_line('a'))
        yAxis => this%get_y_axis()
        if (associated(yAxis)) call str%append(yAxis%get_command_string())

        ! Secondary Axes
        if (this%get_use_y2_axis()) then
            call str%append(new_line('a'))
            axis => this%get_y2_axis()
            if (associated(axis)) then
                call str%append(axis%get_command_string())
                call str%append(new_line('a'))
                call str%append("set y2tics")
                call str%append(new_line('a'))
                call str%append("set ytics nomirror")
            end if
        end if

        ! Tic Marks
        if (.not.this%get_tics_inward()) then
            call str%append(new_line('a'))
            call str%append("set tics out")
        end if
        if ((xAxis%get_zero_axis() .or. yAxis%get_zero_axis()) .and. &
                .not.this%get_use_y2_axis()) then
            ! Set tics to the axis only if there is a zero axis, and no
            ! secondary y axis
            call str%append(new_line('a'))
            call str%append("set tics axis")
        end if


        ! Border
        call str%append(new_line('a'))
        call str%append("set border back")

        if (this%get_draw_border()) then
            n = 31
        else
            n = 0
            if (.not.xAxis%get_zero_axis()) n = n + 1
            if (.not.yAxis%get_zero_axis()) n = n + 2

            call str%append(new_line('a'))
            call str%append("set xtics nomirror")
            call str%append(new_line('a'))
            call str%append("set ytics nomirror")

            if (this%get_use_y2_axis()) then
                n = n + 8
            end if
        end if

        call str%append(new_line('a'))
        if (n > 0) then
            call str%append("set border ")
            call str%append(to_string(n))
        else
            call str%append("unset border")
        end if

        ! Legend
        call str%append(new_line('a'))
        leg => this%get_legend()
        if (associated(leg)) call str%append(leg%get_command_string())

        ! Labels
        do i = 1, this%get_label_count()
            lbl => this%get_label(i)
            if (.not.associated(lbl)) cycle
            call str%append(new_line('a'))
            call str%append(lbl%get_command_string())
        end do

        ! Define the plot function and data formatting commands
        n = this%get_count()
        call str%append(new_line('a'))
        call str%append("plot ")
        do i = 1, n
            ptr => this%get(i)
            if (.not.associated(ptr)) cycle
            call str%append(ptr%get_command_string())
            if (i /= n) call str%append(", ")
        end do

        ! Define the data to plot
        do i = 1, n
            ptr => this%get(i)
            if (.not.associated(ptr)) cycle
            call str%append(new_line('a'))
            call str%append(ptr%get_data_string())
            call str%append("e")
            ! if (i /= n) then
            !     call str%append("e")
            ! end if
        end do

        ! End
        x = str%to_string()
    end function

! ------------------------------------------------------------------------------
    module function p2d_get_x_axis(this) result(ptr)
        class(plot_2d), intent(in) :: this
        class(plot_axis), pointer :: ptr
        ptr => this%m_xAxis
    end function

! ------------------------------------------------------------------------------
    module function p2d_get_y_axis(this) result(ptr)
        class(plot_2d), intent(in) :: this
        class(plot_axis), pointer :: ptr
        ptr => this%m_yAxis
    end function

! ------------------------------------------------------------------------------
    module function p2d_get_y2_axis(this) result(ptr)
        class(plot_2d), intent(in) :: this
        class(plot_axis), pointer :: ptr
        ptr => this%m_y2Axis
    end function

! ------------------------------------------------------------------------------
    pure module function p2d_get_use_y2(this) result(x)
        class(plot_2d), intent(in) :: this
        logical :: x
        x = this%m_useY2
    end function

! --------------------
    module subroutine p2d_set_use_y2(this, x)
        class(plot_2d), intent(inout) :: this
        logical, intent(in) :: x
        this%m_useY2 = x
    end subroutine

end submodule
