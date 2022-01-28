! fplot_multiplot.f90

submodule (fplot_core) fplot_multiplot
contains
! ------------------------------------------------------------------------------
    module function mp_get_command(this) result(x)
        ! Arguments
        class(multiplot), intent(in) :: this
        character(len = :), allocatable :: x

        ! Local Variables
        type(string_builder) :: str
        integer(int32) :: i, j, m, n
        class(plot), pointer :: ptr

        ! Initialization
        call str%initialize()
        m = this%get_row_count()
        n = this%get_column_count()

        ! Set up the multiplot
        call str%append("set multiplot layout ")
        call str%append(to_string(m))
        call str%append(",")
        call str%append(to_string(n))
        call str%append(" columnsfirst")
        if (this%is_title_defined()) then
            call str%append(" title ")
            call str%append('"')
            call str%append(this%get_title())
            call str%append('"')
        end if
        call str%append(new_line('a'))

        ! Write commands for each plot object
        do j = 1, n
            do i = 1, m
                ptr => this%get(i, j)
                call str%append(new_line('a'))
                call str%append(ptr%get_command_string())
            end do
        end do

        ! Close out the multiplot
        call str%append(new_line('a'))
        call str%append("unset multiplot")

        ! Get the string
        x = str%to_string()
    end function

! ------------------------------------------------------------------------------
    module subroutine mp_init(this, m, n, term, err)
        ! Arguments
        class(multiplot), intent(inout) :: this
        integer(int32), intent(in) :: m, n
        integer(int32), intent(in), optional :: term
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: flag, t, i
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        type(wxt_terminal), pointer :: wxt
        type(windows_terminal), pointer :: win
        type(qt_terminal), pointer :: qt
        type(png_terminal), pointer :: png
        type(latex_terminal), pointer :: latex

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        if (present(term)) then
            t = term
        else
            t = GNUPLOT_TERMINAL_WXT
        end if

        ! Process
        call this%m_plots%clear()
        this%m_rows = m
        this%m_cols = n
        flag = 0

        ! Populate the list with a dummy variable at the outset.  This allows
        ! the list to be appropriately sized so the user may use the "set"
        ! subroutine appropriately
        do i = 1, m * n
            call this%m_plots%push(i)
        end do

        ! Define the terminal
        if (associated(this%m_terminal)) deallocate(this%m_terminal)
        select case (t)
        case (GNUPLOT_TERMINAL_PNG)
            allocate(png, stat = flag)
            this%m_terminal => png
        case (GNUPLOT_TERMINAL_QT)
            allocate(qt, stat = flag)
            this%m_terminal => qt
        case (GNUPLOT_TERMINAL_WIN32)
            allocate(win, stat = flag)
            this%m_terminal => win
        case (GNUPLOT_TERMINAL_LATEX)
            allocate(latex, stat = flag)
            this%m_terminal => latex
        case default ! WXT is the default
            allocate(wxt, stat = flag)
            this%m_terminal => wxt
        end select

        ! Error Checking
        if (flag /= 0) then
            call errmgr%report_error("mp_init", &
                "Insufficient memory available.", PLOT_OUT_OF_MEMORY_ERROR)
            return
        end if
    end subroutine
    
! ------------------------------------------------------------------------------
    module subroutine mp_clean(this)
        type(multiplot), intent(inout) :: this
        if (associated(this%m_terminal)) deallocate(this%m_terminal)
        nullify(this%m_terminal)
    end subroutine

! ------------------------------------------------------------------------------
    pure module function mp_get_rows(this) result(x)
        class(multiplot), intent(in) :: this
        integer(int32) :: x
        x = this%m_rows
    end function

! --------------------
    pure module function mp_get_cols(this) result(x)
        class(multiplot), intent(in) :: this
        integer(int32) :: x
        x = this%m_cols
    end function

! --------------------
    pure module function mp_get_count(this) result(x)
        class(multiplot), intent(in) :: this
        integer(int32) :: x
        x = this%m_plots%get_count()
    end function
    
! ------------------------------------------------------------------------------
    module function mp_get_title(this) result(x)
        class(multiplot), intent(in) :: this
        character(len = :), allocatable :: x
        x = this%m_title
    end function

! --------------------
    module subroutine mp_set_title(this, x)
        ! Arguments
        class(multiplot), intent(inout) :: this
        character(len = *), intent(in) :: x

        ! Local Variables
        integer(int32) :: n

        ! Process
        n = min(len(x), PLOTDATA_MAX_NAME_LENGTH)
        this%m_title = ""
        if (n /= 0) then
            this%m_title(1:n) = x(1:n)
            this%m_hasTitle = .true.
        else
            this%m_hasTitle = .false.
        end if
    end subroutine
    
! ------------------------------------------------------------------------------
    module subroutine mp_draw(this, persist, err)
        ! Arguments
        class(multiplot), intent(in) :: this
        logical, intent(in), optional :: persist
        class(errors), intent(inout), optional, target :: err

        ! Parameters
        character(len = *), parameter :: fname = "temp_gnuplot_file.plt"

        ! Local Variables
        logical :: p
        integer(int32) :: fid, flag
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        class(terminal), pointer :: term

        ! Initialization
        if (present(persist)) then
            p = persist
        else
            p = .true.
        end if
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        term => this%get_terminal()

        ! Open the file for writing, and write the contents to file
        open(newunit = fid, file = fname, iostat = flag)
        if (flag > 0) then
            write(errmsg, "(AI0A)") &
                "The file could not be opened/created.  Error code ", flag, &
                " was encountered."
            call errmgr%report_error("mp_draw", trim(errmsg), &
                PLOT_GNUPLOT_FILE_ERROR)
            return
        end if
        write(fid, '(A)') term%get_command_string()
        write(fid, '(A)') new_line('a')
        write(fid, '(A)') this%get_command_string()
        close(fid)

        ! Launch GNUPLOT
        if (p) then
            call execute_command_line("gnuplot --persist " // fname)
        else
            call execute_command_line("gnuplot " // fname)
        end if

        ! Clean up by deleting the file
        open(newunit = fid, file = fname)
        close(fid, status = "delete")
    end subroutine

! ------------------------------------------------------------------------------
    module function mp_get(this, i, j) result(x)
        ! Arguments
        class(multiplot), intent(in) :: this
        integer(int32), intent(in) :: i, j
        class(plot), pointer :: x

        ! Local Variables
        class(*), pointer :: item
        integer(int32) :: ind

        ! Process
        ind = this%m_rows * (j - 1) + i
        item => this%m_plots%get(ind)
        select type (item)
        class is (plot)
            x => item
        class default
            nullify(x)
        end select
    end function

! --------------------
    module subroutine mp_set(this, i, j, x)
        ! Arguments
        class(multiplot), intent(inout) :: this
        integer(int32), intent(in) :: i, j
        class(plot), intent(in) :: x

        ! Local Variables
        integer(int32) :: ind

        ! Process
        ind = this%m_rows * (j - 1) + i
        call this%m_plots%set(ind, x)
    end subroutine
    
! ------------------------------------------------------------------------------
    pure module function mp_has_title(this) result(x)
        class(multiplot), intent(in) :: this
        logical :: x
        x = this%m_hasTitle
    end function

! ------------------------------------------------------------------------------
    module function mp_get_term(this) result(x)
        class(multiplot), intent(in) :: this
        class(terminal), pointer :: x
        x => this%m_terminal
    end function

! ------------------------------------------------------------------------------
    module subroutine mp_save(this, fname, err)
        ! Arguments
        class(multiplot), intent(in) :: this
        character(len = *), intent(in) :: fname
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: fid, flag
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        class(terminal), pointer :: term

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        term => this%get_terminal()

        ! Open the file for writing, and write the contents to file
        open(newunit = fid, file = fname, iostat = flag)
        if (flag > 0) then
            write(errmsg, "(AI0A)") &
                "The file could not be opened/created.  Error code ", flag, &
                " was encountered."
            call errmgr%report_error("mp_save", trim(errmsg), &
                PLOT_GNUPLOT_FILE_ERROR)
            return
        end if
        write(fid, '(A)') term%get_command_string()
        write(fid, '(A)') new_line('a')
        write(fid, '(A)') this%get_command_string()
        close(fid)
    end subroutine

! ------------------------------------------------------------------------------
    module function mp_get_font(this) result(x)
        class(multiplot), intent(in) :: this
        character(len = :), allocatable :: x
        class(terminal), pointer :: term
        term => this%get_terminal()
        x = term%get_font_name()
    end function

! --------------------
    module subroutine mp_set_font(this, x)
        class(multiplot), intent(inout) :: this
        character(len = *), intent(in) :: x
        class(terminal), pointer :: term
        term => this%get_terminal()
        call term%set_font_name(x)
    end subroutine

! ------------------------------------------------------------------------------
    module function mp_get_font_size(this) result(x)
        class(multiplot), intent(in) :: this
        integer(int32) :: x
        class(terminal), pointer :: term
        term => this%get_terminal()
        x = term%get_font_size()
    end function

! --------------------
    module subroutine mp_set_font_size(this, x)
        class(multiplot), intent(inout) :: this
        integer(int32), intent(in) :: x
        class(terminal), pointer :: term
        term => this%get_terminal()
        call term%set_font_size(x)
    end subroutine

! ------------------------------------------------------------------------------
end submodule
