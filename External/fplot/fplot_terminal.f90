! fplot_terminal.f90

submodule (fplot_core) fplot_terminal
contains
! ------------------------------------------------------------------------------
    pure module function term_get_window_width(this) result(x)
        class(terminal), intent(in) :: this
        integer :: x
        x = this%m_windowWidth
    end function

! --------------------
    module subroutine term_set_window_width(this, x)
        class(terminal), intent(inout) :: this
        integer, intent(in) :: x
        if (x == 0) then
            this%m_windowWidth = GNUPLOT_DEFAULT_WINDOW_WIDTH
        else
            this%m_windowWidth = abs(x)
        end if
    end subroutine

! ------------------------------------------------------------------------------
    pure module function term_get_window_height(this) result(x)
        class(terminal), intent(in) :: this
        integer :: x
        x = this%m_windowHeight
    end function

! --------------------
    module subroutine term_set_window_height(this, x)
        class(terminal), intent(inout) :: this
        integer, intent(in) :: x
        if (x == 0) then
            this%m_windowHeight = GNUPLOT_DEFAULT_WINDOW_HEIGHT
        else
            this%m_windowHeight = abs(x)
        end if
    end subroutine

! ------------------------------------------------------------------------------
    pure module function term_get_plot_window_number(this) result(x)
        class(terminal), intent(in) :: this
        integer(int32) :: x
        x = this%m_termID
    end function

! --------------------
    module subroutine term_set_plot_window_number(this, x)
        class(terminal), intent(inout) :: this
        integer(int32), intent(in) :: x
        this%m_termID = x
    end subroutine

! ------------------------------------------------------------------------------
    module function term_get_title(this) result(str)
        class(terminal), intent(in) :: this
        character(len = :), allocatable :: str
        integer(int32) :: n
        n = len_trim(str)
        allocate(character(len = n) :: str)
        str = trim(this%m_title)
    end function

! --------------------
    module subroutine term_set_title(this, txt)
        class(terminal), intent(inout) :: this
        character(len = *), intent(in) :: txt
        integer(int32) :: n
        n = min(len_trim(txt), GNUPLOT_MAX_LABEL_LENGTH)
        this%m_title = ""
        if (n /= 0) then
            this%m_title(1:n) = txt(1:n)
            this%m_hasTitle = .true.
        else
            this%m_hasTitle = .false.
        end if
    end subroutine

! ------------------------------------------------------------------------------
    module function term_get_font_name(this) result(name)
        class(terminal), intent(in) :: this
        character(len = :), allocatable :: name
        integer(int32) :: n
        n = len_trim(this%m_fontName)
        allocate(character(len = n) :: name)
        name = trim(this%m_fontName)
    end function

! --------------------
    module subroutine term_set_font_name(this, name)
        class(terminal), intent(inout) :: this
        character(len = *), intent(in) :: name
        integer(int32) :: n
        n = min(len_trim(name), GNUPLOT_MAX_LABEL_LENGTH)
        this%m_fontName = ""
        if (n == 0) then
            this%m_fontName = GNUPLOT_DEFAULT_FONTNAME
        else
            this%m_fontName(1:n) = name(1:n)
        end if
    end subroutine

! ------------------------------------------------------------------------------
    pure module function term_get_font_size(this) result(sz)
        class(terminal), intent(in) :: this
        integer(int32) :: sz
        sz = this%m_fontSize
    end function

! --------------------
    module subroutine term_set_font_size(this, sz)
        class(terminal), intent(inout) :: this
        integer(int32), intent(in) :: sz
        if (sz == 0) then
            this%m_fontSize = GNUPLOT_DEFAULT_FONT_SIZE
        else
            this%m_fontSize = abs(sz)
        end if
    end subroutine

! ------------------------------------------------------------------------------
    module function term_get_command_string(this) result(x)
        ! Arguments
        class(terminal), intent(in) :: this
        character(len = :), allocatable :: x

        ! Local Variables
        type(string_builder) :: str

        ! Process
        call str%initialize()
        call str%append("set term ")
        call str%append(this%get_id_string())
        call str%append(" enhanced ")
        call str%append(to_string(this%get_plot_window_number()))
        call str%append(" font ")
        call str%append('"')
        call str%append(this%get_font_name())
        call str%append(',')
        call str%append(to_string(this%get_font_size()))
        call str%append('"')
        call str%append(" size ")
        call str%append(to_string(this%get_window_width()))
        call str%append(",")
        call str%append(to_string(this%get_window_height()))
        if (this%m_hasTitle) then
            call str%append(' title "')
            call str%append(this%get_title())
            call str%append('"')
        end if
        x = str%to_string()
    end function

! ------------------------------------------------------------------------------
end submodule
