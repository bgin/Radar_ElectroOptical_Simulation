! fplot_plot_data_2d.f90

submodule (fplot_core) fplot_plot_data_2d
    use fplot_simplify
contains
! ------------------------------------------------------------------------------
    module function pd2d_get_axes_cmd(this) result(x)
        ! Arguments
        class(plot_data_2d), intent(in) :: this
        character(len = :), allocatable :: x

        ! Define which axes the data is to be plotted against
        if (this%get_draw_against_y2()) then
            x = "axes x1y2"
        else
            x = "axes x1y1"
        end if
    end function

! ------------------------------------------------------------------------------
    module function pd2d_get_data_cmd(this) result(x)
        ! Arguments
        class(plot_data_2d), intent(in) :: this
        character(len = :), allocatable :: x

        ! Local Variables
        type(string_builder) :: str
        integer(int32) :: i
        character :: delimiter, nl
        real(real64), allocatable, dimension(:) :: xv, yv
        real(real64), allocatable, dimension(:,:) :: pts
        real(real64) :: tol, maxy, miny, eps

        ! Initialization
        call str%initialize()
        delimiter = achar(9) ! tab delimiter
        nl = new_line(nl)

        ! Process
        xv = this%get_x_data()
        yv = this%get_y_data()
        if (this%get_simplify_data()) then
            maxy = maxval(yv)
            miny = minval(yv)
            tol = abs(this%get_simplification_factor() * (maxy - miny))
            eps = 10.0d0 * epsilon(eps)
            if (tol < eps) tol = eps
            pts = simplify_polyline(xv, yv, tol)
            do i = 1, size(pts, 1)
                call str%append(to_string(pts(i,1)))
                call str%append(delimiter)
                call str%append(to_string(pts(i,2)))
                call str%append(nl)
            end do
        else
            do i = 1, size(xv)
                call str%append(to_string(xv(i)))
                call str%append(delimiter)
                call str%append(to_string(yv(i)))
                call str%append(nl)
            end do
        end if

        ! End
        x = str%to_string()
    end function

! ------------------------------------------------------------------------------
    pure module function pd2d_get_data_count(this) result(x)
        class(plot_data_2d), intent(in) :: this
        integer(int32) :: x
        if (allocated(this%m_data)) then
            x = size(this%m_data, 1)
        else
            x = 0
        end if
    end function

! ------------------------------------------------------------------------------
    pure module function pd2d_get_x_data(this, index) result(x)
        class(plot_data_2d), intent(in) :: this
        integer(int32), intent(in) :: index
        real(real64) :: x
        if (allocated(this%m_data)) then
            x = this%m_data(index, 1)
        else
            x = 0.0d0
        end if
    end function

! --------------------
    module subroutine pd2d_set_x_data(this, index, x)
        class(plot_data_2d), intent(inout) :: this
        integer(int32), intent(in) :: index
        real(real64), intent(in) :: x
        if (allocated(this%m_data)) then
            this%m_data(index, 1) = x
        end if
    end subroutine

! ------------------------------------------------------------------------------
    pure module function pd2d_get_y_data(this, index) result(x)
        class(plot_data_2d), intent(in) :: this
        integer(int32), intent(in) :: index
        real(real64) :: x
        if (allocated(this%m_data)) then
            x = this%m_data(index, 2)
        else
            x = 0.0d0
        end if
    end function

! --------------------
    module subroutine pd2d_set_y_data(this, index, x)
        class(plot_data_2d), intent(inout) :: this
        integer(int32), intent(in) :: index
        real(real64), intent(in) :: x
        if (allocated(this%m_data)) then
            this%m_data(index, 2) = x
        end if
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine pd2d_set_data_1(this, x, y, err)
        ! Arguments
        class(plot_data_2d), intent(inout) :: this
        real(real64), intent(in), dimension(:) :: x, y
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

        ! Input Check
        if (size(y) /= n) then
            call errmgr%report_error("pd2d_set_data_1", &
                "The input arrays are not the same size.", &
                PLOT_ARRAY_SIZE_MISMATCH_ERROR)
            return
        end if

        ! Process
        if (allocated(this%m_data)) deallocate(this%m_data)
        allocate(this%m_data(n, 2), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("pd2d_set_data_1", &
                "Insufficient memory available.", PLOT_OUT_OF_MEMORY_ERROR)
            return
        end if
        do concurrent (i = 1:n)
            this%m_data(i, 1) = x(i)
            this%m_data(i, 2) = y(i)
        end do
    end subroutine

! ------------------------------------------------------------------------------
    pure module function pd2d_get_draw_against_y2(this) result(x)
        class(plot_data_2d), intent(in) :: this
        logical :: x
        x = this%m_useY2
    end function

! --------------------
    module subroutine pd2d_set_draw_against_y2(this, x)
        class(plot_data_2d), intent(inout) :: this
        logical, intent(in) :: x
        this%m_useY2 = x
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine pd2d_set_data_2(this, y, err)
        ! Arguments
        class(plot_data_2d), intent(inout) :: this
        real(real64), intent(in), dimension(:) :: y
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: i, n, flag
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        n = size(y)
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Process
        if (allocated(this%m_data)) deallocate(this%m_data)
        allocate(this%m_data(n, 2), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("pd2d_set_data_2", &
                "Insufficient memory available.", PLOT_OUT_OF_MEMORY_ERROR)
            return
        end if
        do concurrent (i = 1:n)
            this%m_data(i, 1) = real(i, real64)
            this%m_data(i, 2) = y(i)
        end do
    end subroutine

! ------------------------------------------------------------------------------
    module function pd2d_get_x_array(this) result(x)
        ! Arguments
        class(plot_data_2d), intent(in) :: this
        real(real64), allocatable, dimension(:) :: x

        ! Process
        if (allocated(this%m_data)) then
            x = this%m_data(:,1)
        end if
    end function

! ------------------------------------------------------------------------------
    module function pd2d_get_y_array(this) result(x)
        ! Arguments
        class(plot_data_2d), intent(in) :: this
        real(real64), allocatable, dimension(:) :: x

        ! Process
        if (allocated(this%m_data)) then
            x = this%m_data(:,2)
        end if
    end function

end submodule