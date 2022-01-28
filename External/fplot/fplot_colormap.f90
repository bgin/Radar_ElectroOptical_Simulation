! fplot_colormap.f90

submodule (fplot_core) fplot_colormap
contains
! ******************************************************************************
! COLORMAP MEMBERS
! ------------------------------------------------------------------------------
    module function cm_get_cmd(this) result(x)
        ! Arguments
        class(colormap), intent(in) :: this
        character(len = :), allocatable :: x

        ! Local Variables
        type(string_builder) :: str

        ! Initialization
        call str%initialize()

        ! Process
        call str%append("set palette defined (")
        call str%append(this%get_color_string())
        call str%append(")")

        ! End
        x = str%to_string()
    end function

! ******************************************************************************
! RAINBOW_COLORMAP MEMBERS
! ------------------------------------------------------------------------------
    module function rcm_get_clr(this) result(x)
        class(rainbow_colormap), intent(in) :: this
        character(len = :), allocatable :: x
        x = '0 "dark-blue", 1 "blue", 2 "cyan", 3 "green", 4 "yellow", ' // &
            '5 "orange", 6 "red", 7 "dark-red"'
    end function

! ******************************************************************************
! HOT_COLORMAP MEMBERS
! ------------------------------------------------------------------------------
    module function hcm_get_clr(this) result(x)
        class(hot_colormap), intent(in) :: this
        character(len = :), allocatable :: x
        x = '0 "black", 1 "red", 2 "orange", 3 "yellow", 4 "white"'
    end function

! ******************************************************************************
! COOL_COLORMAP MEMBERS
! ------------------------------------------------------------------------------
    module function ccm_get_clr(this) result(x)
        class(cool_colormap), intent(in) :: this
        character(len = :), allocatable :: x
        x = '0 "blue", 1 "turquoise", 2 "light-green"'
    end function

! ------------------------------------------------------------------------------
end submodule
