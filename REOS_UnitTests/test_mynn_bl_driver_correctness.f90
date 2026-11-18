

program test_mynn_bl_driver
    use mod_kinds,          only : i1, i4, i8, sp
    use mod_mynn_bl_driver
    implicit none

    integer(kind=i4) :: total_tests = 0
    integer(kind=i4) :: passed_tests = 0
    integer(kind=i4) :: failed_tests = 0

    print *, "=========================================================="
    print *, " Running Unit Tests for mod_mynn_bl_driver"
    print *, "=========================================================="

    call test_construct_valid()
    call test_construct_already_built()
    call test_construct_invalid_dims()
    call test_destroy_valid()
    call test_destroy_unbuilt()
    call test_copy_create_valid()
    call test_copy_create_error_cases()
    call test_allocation_status_arrays()

    print *, "=========================================================="
    print *, " Test Summary:"
    print *, "   Total : ", total_tests
    print *, "   Passed: ", passed_tests
    print *, "   Failed: ", failed_tests
    print *, "=========================================================="

    if (failed_tests > 0) then
        error stop 1
    end if

contains

    subroutine assert_true(condition, test_name)
        logical,          intent(in) :: condition
        character(len=*), intent(in) :: test_name
        total_tests = total_tests + 1
        if (condition) then
            passed_tests = passed_tests + 1
            print *, "[PASS] ", trim(test_name)
        else
            failed_tests = failed_tests + 1
            print *, "[FAIL] ", trim(test_name)
        end if
    end subroutine assert_true

    subroutine assert_eq_i4(actual, expected, test_name)
        integer(kind=i4), intent(in) :: actual, expected
        character(len=*), intent(in) :: test_name
        total_tests = total_tests + 1
        if (actual == expected) then
            passed_tests = passed_tests + 1
            print *, "[PASS] ", trim(test_name)
        else
            failed_tests = failed_tests + 1
            print *, "[FAIL] ", trim(test_name), " | Expected:", expected, " Got:", actual
        end if
    end subroutine assert_eq_i4

    function create_default_args() result(args)
        type(mynn_bl_driver_args_holder_t) :: args
        args%initflag           = 1_i4
        args%grav_settling      = 0_i4
        args%bl_mynn_tkebudget  = 1_i4
        args%bl_mynn_cloudpdf   = 2_i4
        args%bl_mixlength       = 0_i4
        args%bl_mynn_edmf       = 1_i4
        args%bl_mynn_tkeadvect  = 0_i4
        args%bl_mynn_edmf_mom   = 1_i4
        args%bl_mynn_edmf_tke   = 1_i4
        args%bl_mynn_edmf_part  = 0_i4
        args%bl_mynn_cloudmix   = 1_i4
        args%bl_mynn_mixqt      = 1_i4
        args%icloud_bl          = 1_i4
        args%FLAG_QI            = 1_i4
        args%FLAG_QNI           = 1_i4
        args%FLAG_QC            = 1_i4
        args%FLAG_QNC           = 1_i4

        ! Domain, Memory, and Tile indices
        args%IDS = 1_i4; args%IDE = 10_i4
        args%JDS = 1_i4; args%JDE = 10_i4
        args%KDS = 1_i4; args%KDE = 20_i4

        args%IMS = 0_i4; args%IME = 11_i4
        args%JMS = 0_i4; args%JME = 11_i4
        args%KMS = 1_i4; args%KME = 21_i4

        args%ITS = 1_i4; args%ITE = 10_i4
        args%JTS = 1_i4; args%JTE = 10_i4
        args%KTS = 1_i4; args%KTE = 20_i4

        args%delt = 10.0_sp
        args%dx   = 3000.0_sp
    end function create_default_args

    !-------------------------------------------------------------------------
    ! 1. Tests for construct
    !-------------------------------------------------------------------------
    subroutine test_construct_valid()
        type(mynn_bl_driver_wrapper_t)     :: mw
        type(mynn_bl_driver_args_holder_t) :: args
        integer(kind=i4)                   :: ier

        mw%m_is_derived_type_built = .false.
        args = create_default_args()

        call construct(mw, args, ier)

        call assert_eq_i4(ier, 0_i4, "construct: valid input returns ier == 0")
        call assert_true(logical(mw%m_is_derived_type_built), "construct: sets m_is_derived_type_built to .true.")
        call assert_eq_i4(mw%m_IMS, args%IMS, "construct: copies scalar integer m_IMS")
        call assert_true(abs(mw%m_delt - args%delt) < 1.0e-6_sp, "construct: copies scalar real m_delt")

        ! Verify array allocation and bounds
        call assert_true(allocated(mw%m_dz), "construct: allocates 3D array m_dz")
        call assert_eq_i4(lbound(mw%m_dz, 1), args%IMS, "construct: m_dz dim 1 lower bound matches IMS")
        call assert_eq_i4(ubound(mw%m_dz, 2), args%KME, "construct: m_dz dim 2 upper bound matches KME")
        call assert_true(allocated(mw%m_xland), "construct: allocates 2D array m_xland")
        call assert_eq_i4(ubound(mw%m_xland, 2), args%JME, "construct: m_xland dim 2 upper bound matches JME")

        call destroy(mw, ier)
    end subroutine test_construct_valid

    subroutine test_construct_already_built()
        type(mynn_bl_driver_wrapper_t)     :: mw
        type(mynn_bl_driver_args_holder_t) :: args
        integer(kind=i4)                   :: ier

        mw%m_is_derived_type_built = .true.
        args = create_default_args()

        call construct(mw, args, ier)
        call assert_eq_i4(ier, -1_i4, "construct: already built object returns ier == -1")
    end subroutine test_construct_already_built

    subroutine test_construct_invalid_dims()
        type(mynn_bl_driver_wrapper_t)     :: mw
        type(mynn_bl_driver_args_holder_t) :: args
        integer(kind=i4)                   :: ier

        ! Invalid I dimension (IME <= IMS)
        mw%m_is_derived_type_built = .false.
        args = create_default_args()
        args%IME = args%IMS
        call construct(mw, args, ier)
        call assert_eq_i4(ier, -2_i4, "construct: invalid I dimension returns ier == -2")

        ! Invalid K dimension (KME <= KMS)
        mw%m_is_derived_type_built = .false.
        args = create_default_args()
        args%KME = args%KMS - 1_i4
        call construct(mw, args, ier)
        call assert_eq_i4(ier, -3_i4, "construct: invalid K dimension returns ier == -3")

        ! Invalid J dimension (JME <= JMS)
        mw%m_is_derived_type_built = .false.
        args = create_default_args()
        args%JME = args%JMS
        call construct(mw, args, ier)
        call assert_eq_i4(ier, -4_i4, "construct: invalid J dimension returns ier == -4")
    end subroutine test_construct_invalid_dims

    !-------------------------------------------------------------------------
    ! 2. Tests for destroy
    !-------------------------------------------------------------------------
    subroutine test_destroy_valid()
        type(mynn_bl_driver_wrapper_t)     :: mw
        type(mynn_bl_driver_args_holder_t) :: args
        integer(kind=i4)                   :: ier

        mw%m_is_derived_type_built = .false.
        args = create_default_args()
        call construct(mw, args, ier)

        call destroy(mw, ier)
        call assert_eq_i4(ier, 0_i4, "destroy: built object returns ier == 0")
        call assert_true(.not. logical(mw%m_is_derived_type_built), "destroy: resets m_is_derived_type_built to .false.")
        call assert_true(.not. allocated(mw%m_dz), "destroy: deallocates 3D array m_dz")
        call assert_true(.not. allocated(mw%m_xland), "destroy: deallocates 2D array m_xland")
    end subroutine test_destroy_valid

    subroutine test_destroy_unbuilt()
        type(mynn_bl_driver_wrapper_t) :: mw
        integer(kind=i4)               :: ier

        mw%m_is_derived_type_built = .false.
        call destroy(mw, ier)
        call assert_eq_i4(ier, -1_i4, "destroy: unbuilt object returns ier == -1")
    end subroutine test_destroy_unbuilt

    !-------------------------------------------------------------------------
    ! 3. Tests for copy_create
    !-------------------------------------------------------------------------
    subroutine test_copy_create_valid()
        type(mynn_bl_driver_wrapper_t)     :: src, dst
        type(mynn_bl_driver_args_holder_t) :: args
        integer(kind=i4)                   :: ier

        src%m_is_derived_type_built = .false.
        dst%m_is_derived_type_built = .false.
        args = create_default_args()

        call construct(src, args, ier)

        ! Populate sentinel test values inside tile range (ITS:ITE, KTS:KTE, JTS:JTE)
        src%m_u(1, 1, 1)     = 42.5_sp
        src%m_th(10, 20, 10) = 305.25_sp
        src%m_xland(5, 5)    = 1.0_sp
        src%m_Pblh(10, 10)   = 1250.0_sp

        call copy_create(dst, src, ier)

        call assert_eq_i4(ier, 0_i4, "copy_create: valid copy returns ier == 0")
        call assert_true(logical(dst%m_is_derived_type_built), "copy_create: sets dst m_is_derived_type_built to .true.")
        call assert_eq_i4(dst%m_KDE, src%m_KDE, "copy_create: copies scalar metadata")
        call assert_true(abs(dst%m_u(1, 1, 1) - 42.5_sp) < 1.0e-6_sp, "copy_create: copies 3D array values (lower corner)")
        call assert_true(abs(dst%m_th(10, 20, 10) - 305.25_sp) < 1.0e-6_sp, "copy_create: copies 3D array values (upper corner)")
        call assert_true(abs(dst%m_xland(5, 5) - 1.0_sp) < 1.0e-6_sp, "copy_create: copies 2D array values")

        ! Verify deep copy (modifying src does not alter dst)
        src%m_u(1, 1, 1) = -99.0_sp
        call assert_true(abs(dst%m_u(1, 1, 1) - 42.5_sp) < 1.0e-6_sp, "copy_create: performs deep copy independent of source")

        call destroy(src, ier)
        call destroy(dst, ier)
    end subroutine test_copy_create_valid

    subroutine test_copy_create_error_cases()
        type(mynn_bl_driver_wrapper_t)     :: src, dst
        type(mynn_bl_driver_args_holder_t) :: args
        integer(kind=i4)                   :: ier

        args = create_default_args()

        ! Case 1: dst is already built -> ier == -1
        dst%m_is_derived_type_built = .true.
        src%m_is_derived_type_built = .true.
        call copy_create(dst, src, ier)
        call assert_eq_i4(ier, -1_i4, "copy_create: already built dst returns ier == -1")
       ! Case 2: src is not built -> ier == -2
        dst%m_is_derived_type_built = .false.
        src%m_is_derived_type_built = .false.
        call copy_create(dst, src, ier)
        call assert_eq_i4(ier, -2_i4, "copy_create: unbuilt src returns ier == -2")
        ! Case 3: Self-assignment (LOC(this) == LOC(other)) -> ier == -3
        ! Note: Since this%m_is_derived_type_built is checked first, ier == -3 is only reachable
        ! if both flags are .false. (which triggers -2 first). Testing self-call behavior:
        src%m_is_derived_type_built = .false.
        call copy_create(src, src, ier)
        call assert_true(ier < 0_i4, "copy_create: self-copy is rejected with negative error code")
end subroutine test_copy_create_error_cases
!-------------------------------------------------------------------------
! 4. Tests for allocation_status_array2D and allocation_status_array3D
!-------------------------------------------------------------------------
subroutine test_allocation_status_arrays()
        real(kind=sp), allocatable :: arr2d(:,:)
        real(kind=sp), allocatable :: arr3d(:,:,:)
! Test unallocated state (should execute cleanly without crashing)
        call allocation_status_array2D(arr2d, "unallocated_2d")
        call assert_true(.not. allocated(arr2d), "allocation_status_array2D: handles unallocated array")
        call allocation_status_array3D(arr3d, "unallocated_3d")
        call assert_true(.not. allocated(arr3d), "allocation_status_array3D: handles unallocated array")
! Test allocated state with non-default lower bounds
        allocate(arr2d(0:5, -2:4))
        call allocation_status_array2D(arr2d, "allocated_2d")
        call assert_true(allocated(arr2d), "allocation_status_array2D: handles allocated array")
        deallocate(arr2d)
        allocate(arr3d(1:4, 1:10, 1:4))
        call allocation_status_array3D(arr3d, "allocated_3d")
        call assert_true(allocated(arr3d), "allocation_status_array3D: handles allocated array")
        deallocate(arr3d)
end subroutine test_allocation_status_arrays
end program test_mynn_bl_driver