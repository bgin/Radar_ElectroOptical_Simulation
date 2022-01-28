! fplot_surface_plot.f90

submodule (fplot_core) fplot_surface_plot
contains
! ------------------------------------------------------------------------------
    module subroutine surf_clean_up(this)
        type(surface_plot), intent(inout) :: this
        if (associated(this%m_colormap)) then
            deallocate(this%m_colormap)
            nullify(this%m_colormap)
        end if

        ! No need to call the base class finalization routine as the compiler
        ! takes care of that for us.
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine surf_init(this, term, fname, err)
        ! Arguments
        class(surface_plot), intent(inout) :: this
        integer(int32), intent(in), optional :: term
        character(len = *), intent(in), optional :: fname
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        type(legend), pointer :: lgnd

        ! Initialize the base class
        call this%plot_3d%initialize(term, fname, err)

        ! Do not display the legend
        lgnd => this%get_legend()
        call lgnd%set_is_visible(.false.)

        ! Nullify the colormap
        nullify(this%m_colormap)
    end subroutine

! ------------------------------------------------------------------------------
    pure module function surf_get_show_hidden(this) result(x)
        class(surface_plot), intent(in) :: this
        logical :: x
        x = this%m_showHidden
    end function

! ------------------------------------------------------------------------------
    module subroutine surf_set_show_hidden(this, x)
        class(surface_plot), intent(inout) :: this
        logical, intent(in) :: x
        this%m_showHidden = x
    end subroutine

! ------------------------------------------------------------------------------
    module function surf_get_cmd(this) result(x)
        ! Arguments
        class(surface_plot), intent(in) :: this
        character(len = :), allocatable :: x

        ! Local Variables
        type(string_builder) :: str
        class(colormap), pointer :: clr

        ! Initialization
        call str%initialize()

        ! Hidden Stuff
        if (this%get_show_hidden()) then
            call str%append("unset hidden3d")
        else
            call str%append("set hidden3d")
        end if

        ! Define the colormap
        clr => this%get_colormap()
        if (associated(clr)) then
            call str%append(new_line('a'))
            call str%append(clr%get_command_string())
        end if

        ! Allow for smoothing interpolation
        if (this%get_allow_smoothing()) then
            call str%append(new_line('a'))
            call str%append("set pm3d interpolate 0,0")
        end if

        ! Draw a contour plot as well?
        if (this%get_show_contours()) then
            call str%append(new_line('a'))
            call str%append("set contour")
        end if

        ! Show colorbar
        if (.not.this%get_show_colorbar()) then
            call str%append(new_line('a'))
            call str%append("unset colorbox")
        end if

        ! Lighting
        if (this%get_use_lighting()) then
            call str%append(new_line('a'))
            call str%append("set pm3d lighting primary ")
            call str%append(to_string(this%get_light_intensity()))
            call str%append(" specular ")
            call str%append(to_string(this%get_specular_intensity()))
        end if

        ! Translucent
        if (this%get_transparency() < 1.0 .and. this%get_transparency() > 0.0) then
            call str%append(new_line('a'))
            call str%append("set style fill transparent solid ")
            call str%append(to_string(this%get_transparency()))
        end if

        ! Call the base class to define the rest of the plot commands
        call str%append(new_line('a'))
        call str%append(this%plot_3d%get_command_string())

        ! Output
        x = str%to_string()
    end function

! ------------------------------------------------------------------------------
    module function surf_get_colormap(this) result(x)
        class(surface_plot), intent(in) :: this
        class(colormap), pointer :: x
        x => this%m_colormap
    end function

! --------------------
    module subroutine surf_set_colormap(this, x, err)
        ! Arguments
        class(surface_plot), intent(inout) :: this
        class(colormap), intent(in) :: x
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

        ! Process
        if (associated(this%m_colormap)) deallocate(this%m_colormap)
        allocate(this%m_colormap, stat = flag, source = x)
        if (flag /= 0) then
            call errmgr%report_error("surf_set_colormap", &
                "Insufficient memory available.", PLOT_OUT_OF_MEMORY_ERROR)
            return
        end if
    end subroutine

! ------------------------------------------------------------------------------
    pure module function surf_get_smooth(this) result(x)
        class(surface_plot), intent(in) :: this
        logical :: x
        x = this%m_smooth
    end function

! --------------------
    module subroutine surf_set_smooth(this, x)
        class(surface_plot), intent(inout) :: this
        logical, intent(in) :: x
        this%m_smooth = x
    end subroutine

! ------------------------------------------------------------------------------
    pure module function surf_get_show_contours(this) result(x)
        class(surface_plot), intent(in) :: this
        logical :: x
        x = this%m_contour
    end function

! --------------------
    module subroutine surf_set_show_contours(this, x)
        class(surface_plot), intent(inout) :: this
        logical, intent(in) :: x
        this%m_contour = x
    end subroutine

! ------------------------------------------------------------------------------
    pure module function surf_get_show_colorbar(this) result(x)
        class(surface_plot), intent(in) :: this
        logical :: x
        x = this%m_showColorbar
    end function

! --------------------
    module subroutine surf_set_show_colorbar(this, x)
        class(surface_plot), intent(inout) :: this
        logical, intent(in) :: x
        this%m_showColorbar = x
    end subroutine

! ------------------------------------------------------------------------------
    pure module function surf_get_use_lighting(this) result(x)
        class(surface_plot), intent(in) :: this
        logical :: x
        x = this%m_useLighting
    end function

! --------------------
    module subroutine surf_set_use_lighting(this, x)
        class(surface_plot), intent(inout) :: this
        logical, intent(in) :: x
        this%m_useLighting = x
    end subroutine

! ------------------------------------------------------------------------------
    pure module function surf_get_light_intensity(this) result(x)
        class(surface_plot), intent(in) :: this
        real(real32) :: x
        x = this%m_lightIntensity
    end function

! --------------------
    module subroutine surf_set_light_intensity(this, x)
        class(surface_plot), intent(inout) :: this
        real(real32), intent(in) :: x
        if (x < 0.0) then
            this%m_lightIntensity = 0.0
        else if (x > 1.0) then
            this%m_lightIntensity = 1.0
        else
            this%m_lightIntensity = x
        end if
    end subroutine

! ------------------------------------------------------------------------------
    pure module function surf_get_specular_intensity(this) result(x)
        class(surface_plot), intent(in) :: this
        real(real32) :: x
        x = this%m_specular
    end function

! --------------------
    module subroutine surf_set_specular_intensity(this, x)
        class(surface_plot), intent(inout) :: this
        real(real32), intent(in) :: x
        if (x < 0.0) then
            this%m_specular = 0.0
        else if (x > 1.0) then
            this%m_specular = 1.0
        else
            this%m_specular = x
        end if
    end subroutine

! ------------------------------------------------------------------------------
    pure module function surf_get_transparency(this) result(x)
        class(surface_plot), intent(in) :: this
        real(real32) :: x
        x = this%m_transparency
    end function

! --------------------
    module subroutine surf_set_transparency(this, x)
        class(surface_plot), intent(inout) :: this
        real(real32), intent(in) :: x
        if (x > 1.0) then
            this%m_transparency = 1.0
        else if (x <= 0.0) then
            this%m_transparency = 0.1
        else
            this%m_transparency = x
        end if
    end subroutine

end submodule