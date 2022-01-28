! fplot_core_routines.f90

submodule (fplot_core) fplot_core_routines
contains
! ------------------------------------------------------------------------------
    pure module function linspace(start, finish, npts) result(x)
        ! Arguments
        real(real64), intent(in) :: start, finish
        integer(int32), intent(in) :: npts
        real(real64), dimension(npts) :: x

        ! Local Variables
        integer(int32) :: i
        real(real64) :: dx

        ! Process
        dx = (finish - start) / (npts - 1.0d0)
        x(1) = start
        do i = 2, npts
            x(i) = x(i-1) + dx
        end do
    end function

! ------------------------------------------------------------------------------
    pure module function logspace(start, finish, npts) result(x)
        ! Arguments
        real(real64), intent(in) :: start, finish
        integer(int32), intent(in) :: npts
        real(real64), dimension(npts) :: x

        ! Local Variables
        integer(int32) :: i
        real(real64) :: dx, exponent

        ! Process
        dx = (finish - start) / (npts - 1.0d0)
        exponent = start
        do i = 1, npts
            x(i) = 1.0d1**exponent
            exponent = exponent + dx
        end do
    end function

! ------------------------------------------------------------------------------
    pure module function meshgrid(x, y) result(xy)
        ! Arguments
        real(real64), intent(in), dimension(:) :: x, y
        real(real64), dimension(size(y), size(x), 2) :: xy

        ! Local Variables
        integer(int32) :: i, nx, ny

        ! Process
        nx = size(x)
        ny = size(y)
        do i = 1, nx
            xy(:,i,1) = x
        end do
        do i = 1, ny
            xy(i,:,2) = y
        end do
    end function

end submodule
