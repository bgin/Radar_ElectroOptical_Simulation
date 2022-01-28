! fplot_label.f90

submodule (fplot_core) fplot_label
contains
! ------------------------------------------------------------------------------
    module function lbl_get_cmd(this) result(x)
        ! Arguments
        class(plot_label), intent(in) :: this
        character(len = :), allocatable :: x

        ! Local Variables
        type(string_builder) :: str
        real(real32) :: pt(3)

        ! Initialization
        call str%initialize()
        pt = this%get_position()

        ! If visible, draw the label
        if (this%get_is_visible()) then
            call str%append('set label "')
            call str%append(this%get_text())
            call str%append('"')

            call str%append(" at ")
            call str%append(to_string(pt(1)))
            call str%append(",")
            call str%append(to_string(pt(2)))
            call str%append(",")
            call str%append(to_string(pt(3)))

            call str%append(" rotate by ")
            call str%append(to_string(this%get_angle()))

            x = str%to_string()
        end if
    end function

! ------------------------------------------------------------------------------
    pure module function lbl_get_is_visible(this) result(x)
        class(plot_label), intent(in) :: this
        logical :: x
        x = this%m_visible
    end function

! --------------------
    module subroutine lbl_set_is_visible(this, x)
        class(plot_label), intent(inout) :: this
        logical, intent(in) :: x
        this%m_visible = x
    end subroutine
    
! ------------------------------------------------------------------------------
    pure module function lbl_get_position(this) result(x)
        class(plot_label), intent(in) :: this
        real(real32), dimension(3) :: x
        x = this%m_position
    end function

! --------------------
    module subroutine lbl_set_position(this, x)
        class(plot_label), intent(inout) :: this
        real(real32), intent(in), dimension(3) :: x
        this%m_position = x
    end subroutine

! ------------------------------------------------------------------------------
    pure module function lbl_get_angle(this) result(x)
        class(plot_label), intent(in) :: this
        real(real32) :: x
        x = this%m_angle
    end function

! --------------------
    module subroutine lbl_set_angle(this, x)
        class(plot_label), intent(inout) :: this
        real(real32), intent(in) :: x
        this%m_angle = x
    end subroutine
    
! ------------------------------------------------------------------------------
    module function lbl_get_txt(this) result(x)
        class(plot_label), intent(in) :: this
        character(len = :), allocatable :: x
        x = trim(this%m_text)
    end function

! --------------------
    module subroutine lbl_set_txt(this, x)
        class(plot_label), intent(inout) :: this
        character(len = *), intent(in) :: x
        integer(int32) :: n
        n = min(len(x), PLOTDATA_MAX_NAME_LENGTH)
        this%m_text = ""
        this%m_text(1:n) = x(1:n)
    end subroutine

! ------------------------------------------------------------------------------
end submodule
