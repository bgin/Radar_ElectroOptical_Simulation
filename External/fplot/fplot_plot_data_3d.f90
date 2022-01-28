! fplot_plot_data_3d.f90

submodule (fplot_core) fplot_plot_data_3d
    use fplot_simplify
contains
! ------------------------------------------------------------------------------
    pure module function pd3d_get_data_count(this) result(x)
        class(plot_data_3d), intent(in) :: this
        integer(int32) :: x
        if (allocated(this%m_data)) then
            x = size(this%m_data, 1)
        else
            x = 0
        end if
    end function

! ------------------------------------------------------------------------------
    pure module function pd3d_get_x_data(this, index) result(x)
        class(plot_data_3d), intent(in) :: this
        integer(int32), intent(in) :: index
        real(real64) :: x
        if (allocated(this%m_data)) then
            x = this%m_data(index, 1)
        else
            x = 0.0d0
        end if
    end function

! --------------------
    module subroutine pd3d_set_x_data(this, index, x)
        class(plot_data_3d), intent(inout) :: this
        integer(int32), intent(in) :: index
        real(real64), intent(in) :: x
        if (allocated(this%m_data)) then
            this%m_data(index, 1) = x
        end if
    end subroutine

! ------------------------------------------------------------------------------
    pure module function pd3d_get_y_data(this, index) result(x)
        class(plot_data_3d), intent(in) :: this
        integer(int32), intent(in) :: index
        real(real64) :: x
        if (allocated(this%m_data)) then
            x = this%m_data(index, 2)
        else
            x = 0.0d0
        end if
    end function

! --------------------
    module subroutine pd3d_set_y_data(this, index, x)
        class(plot_data_3d), intent(inout) :: this
        integer(int32), intent(in) :: index
        real(real64), intent(in) :: x
        if (allocated(this%m_data)) then
            this%m_data(index, 2) = x
        end if
    end subroutine

! ------------------------------------------------------------------------------
    pure module function pd3d_get_z_data(this, index) result(x)
        class(plot_data_3d), intent(in) :: this
        integer(int32), intent(in) :: index
        real(real64) :: x
        if (allocated(this%m_data)) then
            x = this%m_data(index, 3)
        else
            x = 0.0d0
        end if
    end function

! --------------------
    module subroutine pd3d_set_z_data(this, index, x)
        class(plot_data_3d), intent(inout) :: this
        integer(int32), intent(in) :: index
        real(real64), intent(in) :: x
        if (allocated(this%m_data)) then
            this%m_data(index, 3) = x
        end if
    end subroutine

! ------------------------------------------------------------------------------
    module function pd3d_get_axes_cmd(this) result(x)
        ! Arguments
        class(plot_data_3d), intent(in) :: this
        character(len = :), allocatable :: x

        ! Output
        x = ""
    end function

! ------------------------------------------------------------------------------
    module function pd3d_get_data_cmd(this) result(x)
        ! Arguments
        class(plot_data_3d), intent(in) :: this
        character(len = :), allocatable :: x

        ! Local Variables
        type(string_builder) :: str
        integer(int32) :: i
        character :: delimiter, nl
        real(real64), allocatable, dimension(:) :: xv, yv, zv
        real(real64), allocatable, dimension(:,:) :: pts
        real(real64) :: tol, maxz, minz, eps

        ! Initialization
        call str%initialize()
        delimiter = achar(9) ! tab delimiter
        nl = new_line(nl)

        ! Process
        xv = this%get_x_data()
        yv = this%get_y_data()
        zv = this%get_z_data()
        if (this%get_simplify_data()) then
            maxz = maxval(zv)
            minz = minval(zv)
            tol = abs(this%get_simplification_factor() * (maxy - miny))
            eps = 10.0d0 * epsilon(eps)
            if (tol < eps) tol = eps
            pts = simplify_polyline(xv, yv, zv, tol)
            do i = 1, size(pts, 1)
                call str%append(to_string(pts(i,1)))
                call str%append(delimiter)
                call str%append(to_string(pts(i,2)))
                call str%append(delimiter)
                call str%append(to_string(pts(i,3)))
                call str%append(nl)
            end do
        else
            do i = 1, size(xv)
                call str%append(to_string(xv(i)))
                call str%append(delimiter)
                call str%append(to_string(yv(i)))
                call str%append(delimiter)
                call str%append(to_string(zv(i)))
                call str%append(nl)
            end do
        end if

        ! End
        x = str%to_string()
    end function

! ------------------------------------------------------------------------------
    module subroutine pd3d_set_data_1(this, x, y, z, err)
        ! Arguments
        class(plot_data_3d), intent(inout) :: this
        real(real64), intent(in), dimension(:) :: x, y, z
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
        if (size(y) /= n .or. size(z) /= n) then
            call errmgr%report_error("pd3d_set_data_1", &
                "The input arrays are not the same size.", &
                PLOT_ARRAY_SIZE_MISMATCH_ERROR)
            return
        end if

        ! Process
        if (allocated(this%m_data)) deallocate(this%m_data)
        allocate(this%m_data(n, 3), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("pd3d_set_data_1", &
                "Insufficient memory available.", PLOT_OUT_OF_MEMORY_ERROR)
            return
        end if
        do concurrent (i = 1:n)
            this%m_data(i, 1) = x(i)
            this%m_data(i, 2) = y(i)
            this%m_data(i, 3) = z(i)
        end do
    end subroutine

! ------------------------------------------------------------------------------
    module function pd3d_get_x_array(this) result(x)
        ! Arguments
        class(plot_data_3d), intent(in) :: this
        real(real64), allocatable, dimension(:) :: x

        ! Process
        if (allocated(this%m_data)) then
            x = this%m_data(:,1)
        end if
    end function

! ------------------------------------------------------------------------------
    module function pd3d_get_y_array(this) result(x)
        ! Arguments
        class(plot_data_3d), intent(in) :: this
        real(real64), allocatable, dimension(:) :: x

        ! Process
        if (allocated(this%m_data)) then
            x = this%m_data(:,2)
        end if
    end function

! ------------------------------------------------------------------------------
    module function pd3d_get_z_array(this) result(x)
        ! Arguments
        class(plot_data_3d), intent(in) :: this
        real(real64), allocatable, dimension(:) :: x

        ! Process
        if (allocated(this%m_data)) then
            x = this%m_data(:,3)
        end if
    end function

! ------------------------------------------------------------------------------
end submodule