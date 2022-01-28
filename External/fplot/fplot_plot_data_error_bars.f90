! fplot_plot_data_error_bars.f90

submodule (fplot_core) fplot_plot_data_error_bars
contains
! ------------------------------------------------------------------------------
    module function pde_get_cmd(this) result(cmd)
        ! Arguments
        class(plot_data_error_bars), intent(in) :: this
        character(len = :), allocatable :: cmd

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

        ! Color
        clr = this%get_line_color()
        call str%append(' lc rgb "#')
        call str%append(clr%to_hex_string())
        call str%append('"')

        ! Error Bars
        if (this%get_plot_x_error_bars() .and. this%get_plot_y_error_bars()) then
            if (this%get_use_error_box()) then
                call str%append(" w boxxyerr")
            else
                call str%append(" w xyerr")
            end if
        else if (this%get_plot_x_error_bars() .and. .not.this%get_plot_y_error_bars()) then
            call str%append(" w xerr")
        else if (.not.this%get_plot_x_error_bars() .and. this%get_plot_y_error_bars()) then
            call str%append(" w yerr")
        end if

        ! Output
        cmd = str%to_string()
    end function

! ------------------------------------------------------------------------------
    module function pde_get_data_cmd(this) result(cmd)
        ! Arguments
        class(plot_data_error_bars), intent(in) :: this
        character(len = :), allocatable :: cmd

        ! Local Variables
        type(string_builder) :: str
        integer(int32) :: i, n
        character :: delimiter, nl

        ! Initialization
        call str%initialize()
        delimiter = achar(9) ! tab delimiter
        nl = new_line(nl)
        n = this%get_count()

        ! Process
        if (this%get_plot_x_error_bars() .and. this%get_plot_y_error_bars()) then
            do i = 1, n
                call str%append(to_string(this%m_data(i, 1)))
                call str%append(delimiter)
                call str%append(to_string(this%m_data(i, 2)))
                call str%append(delimiter)
                call str%append(to_string(this%m_data(i, 3)))
                call str%append(delimiter)
                call str%append(to_string(this%m_data(i, 4)))
                call str%append(nl)
            end do
        else
            do i = 1, n
                call str%append(to_string(this%m_data(i, 1)))
                call str%append(delimiter)
                call str%append(to_string(this%m_data(i, 2)))
                call str%append(delimiter)
                call str%append(to_string(this%m_data(i, 3)))
                call str%append(nl)
            end do
        end if

        ! End
        cmd = str%to_string()
    end function

! ------------------------------------------------------------------------------
    module subroutine pde_define_x_err(this, x, y, xerr, err)
        ! Arguments
        class(plot_data_error_bars), intent(inout) :: this
        real(real64), intent(in), dimension(:) :: x, y, xerr
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: i, n, flag
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        n = size(x)
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Checking
        if (size(y) /= n .or. size(xerr) /= n) then
            call errmgr%report_error("pde_define_x_err", &
                "Input arrays must be the same size.", &
                PLOT_ARRAY_SIZE_MISMATCH_ERROR)
            return
        end if

        ! Process
        this%m_xBars = .false.
        this%m_yBars = .false.
        if (allocated(this%m_data)) deallocate(this%m_data)
        allocate(this%m_data(n, 3), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("pde_define_x_err", &
                "Insufficient memory available.", PLOT_OUT_OF_MEMORY_ERROR)
            return
        end if
        do i = 1, n
            this%m_data(i, 1) = x(i)
            this%m_data(i, 2) = y(i)
            this%m_data(i, 3) = xerr(i)
        end do
        this%m_xBars = .true.
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine pde_define_y_err(this, x, y, yerr, err)
        ! Arguments
        class(plot_data_error_bars), intent(inout) :: this
        real(real64), intent(in), dimension(:) :: x, y, yerr
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: i, n, flag
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        n = size(x)
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Checking
        if (size(y) /= n .or. size(yerr) /= n) then
            call errmgr%report_error("pde_define_y_err", &
                "Input arrays must be the same size.", &
                PLOT_ARRAY_SIZE_MISMATCH_ERROR)
            return
        end if

        ! Process
        this%m_xBars = .false.
        this%m_yBars = .false.
        if (allocated(this%m_data)) deallocate(this%m_data)
        allocate(this%m_data(n, 3), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("pde_define_y_err", &
                "Insufficient memory available.", PLOT_OUT_OF_MEMORY_ERROR)
            return
        end if
        do i = 1, n
            this%m_data(i, 1) = x(i)
            this%m_data(i, 2) = y(i)
            this%m_data(i, 3) = yerr(i)
        end do
        this%m_yBars = .true.
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine pde_define_xy_err(this, x, y, xerr, yerr, err)
        ! Arguments
        class(plot_data_error_bars), intent(inout) :: this
        real(real64), intent(in), dimension(:) :: x, y, xerr, yerr
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: i, n, flag
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        n = size(x)
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Checking
        if (size(y) /= n .or. size(xerr) /= n .or. size(yerr) /= n) then
            call errmgr%report_error("pde_define_xy_err", &
                "Input arrays must be the same size.", &
                PLOT_ARRAY_SIZE_MISMATCH_ERROR)
            return
        end if

        ! Process
        this%m_xBars = .false.
        this%m_yBars = .false.
        if (allocated(this%m_data)) deallocate(this%m_data)
        allocate(this%m_data(n, 4), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("pde_define_xy_err", &
                "Insufficient memory available.", PLOT_OUT_OF_MEMORY_ERROR)
            return
        end if
        do i = 1, n
            this%m_data(i, 1) = x(i)
            this%m_data(i, 2) = y(i)
            this%m_data(i, 3) = xerr(i)
            this%m_data(i, 4) = yerr(i)
        end do
        this%m_xBars = .true.
        this%m_yBars = .true.
    end subroutine

! ------------------------------------------------------------------------------
    pure module function pde_get_plot_x_err(this) result(x)
        class(plot_data_error_bars), intent(in) :: this
        logical :: x
        x = this%m_xBars
    end function

! ------------------------------------------------------------------------------
    pure module function pde_get_plot_y_err(this) result(x)
        class(plot_data_error_bars), intent(in) :: this
        logical :: x
        x = this%m_yBars
    end function
! ------------------------------------------------------------------------------
    pure module function pde_get_count(this) result(x)
        class(plot_data_error_bars), intent(in) :: this
        integer(int32) :: x
        if (allocated(this%m_data)) then
            x = size(this%m_data, 1)
        else
            x = 0
        end if
    end function

! ------------------------------------------------------------------------------
    pure module function pde_get_box(this) result(x)
        class(plot_data_error_bars), intent(in) :: this
        logical :: x
        x = this%m_box
    end function

! --------------------
    module subroutine pde_set_box(this, x)
        class(plot_data_error_bars), intent(inout) :: this
        logical, intent(in) :: x
        this%m_box = x
    end subroutine

! ------------------------------------------------------------------------------
end submodule
