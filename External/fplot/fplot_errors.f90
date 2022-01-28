! fplot_errors.f90

!> @brief \b plot_errors
!!
!! @par Purpose
!! Provides error codes for plot routines.
module fplot_errors
    use, intrinsic :: iso_fortran_env, only : int32
    implicit none

    !> @brief Occurs if there is insufficient memory available for the
    !! requested operation.
    integer(int32), parameter :: PLOT_OUT_OF_MEMORY_ERROR = 1000
    !> @brief Occurs if an invalid input is provided.
    integer(int32), parameter :: PLOT_INVALID_INPUT_ERROR = 1001
    !> @brief Occurs if an attempt is made to perform an invalid operation.
    integer(int32), parameter :: PLOT_INVALID_OPERATION_ERROR = 1002
    !> @brief Occurs if there is an array size mismatch error.
    integer(int32), parameter :: PLOT_ARRAY_SIZE_MISMATCH_ERROR = 1003
    !> @brief Occurs if there is a GNUPLOT file error.
    integer(int32), parameter :: PLOT_GNUPLOT_FILE_ERROR = 1004
end module
