! fplot_colors.f90

submodule (fplot_core) fplot_colors
contains
! ------------------------------------------------------------------------------
    pure module function clr_to_hex_string(this) result(txt)
        ! Arguments
        class(color), intent(in) :: this
        character(6) :: txt

        ! Local Variables
        integer(int32) :: r, g, b, clr

        ! Clip each color if necessary
        if (this%red < 0) then
            r = 0
        else if (this%red > 255) then
            r = 255
        else
            r = this%red
        end if

        if (this%green < 0) then
            g = 0
        else if (this%green > 255) then
            g = 255
        else
            g = this%green
        end if

        if (this%blue < 0) then
            b = 0
        else if (this%blue > 255) then
            b = 255
        else
            b = this%blue
        end if

        ! Build the color information
        clr = ishft(r, 16) + ishft(g, 8) + b

        ! Convert the integer to a hexadecimal string
        write(txt, '(Z6.6)') clr
    end function

! ------------------------------------------------------------------------------
    module subroutine clr_copy_from(this, clr)
        class(color), intent(inout) :: this
        class(color), intent(in) :: clr
        this%red = clr%red
        this%green = clr%green
        this%blue = clr%blue
    end subroutine

end submodule