!/*MIT License
!Copyright (c) 2020 Bernard Gingold
!Permission is hereby granted, free of charge, to any person obtaining a copy
!of this software and associated documentation files (the "Software"), to deal
!in the Software without restriction, including without limitation the rights
!to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
!copies of the Software, and to permit persons to whom the Software is
!furnished to do so, subject to the following conditions:
!The above copyright notice and this permission notice shall be included in all
!copies or substantial portions of the Software.
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
!FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
!AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
!LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
!OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
!SOFTWARE.
!*/

#if 0
    ICC and ifort commands
    icc -c -std=c99 GMS_intrinsics_wrappers.c
    ifort -o test_pm_bb_cmplx_cos_signal_c4_construct -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -Wall -qopt-report=5  GMS_kinds.f90  GMS_intrinsics_wrappers.o GMS_pm_bb_cmplx_cos_signal_c4.f90 test_pm_bb_cmplx_cos_signal_c4_construct.f90
    -------------------------------------------------------------------------------------------------------------------------------------------------
    ifx -o test_pm_bb_cmplx_cos_signal_c4_construct -fp-model fast=2 -ftz -O3  -march=skylake-avx512      -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=3 \ 
     GMS_kinds.f90  GMS_intrinsics_wrappers.o GMS_pm_bb_cmplx_cos_signal_c4.f90 test_pm_bb_cmplx_cos_signal_c4_construct.f90

    For assembly only:
    ifort -S  test_pm_bb_cmplx_cos_signal_c4_construct -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90  GMS_kinds.f90  GMS_intrinsics_wrappers.o GMS_pm_bb_cmplx_cos_signal_c4.f90 test_pm_bb_cmplx_cos_signal_c4_construct.f90
#endif

program test_pm_bb_cmplx_cos_signal_c4_construct 
    use mod_kinds, only : i1, i4, i8, sp
    use mod_pm_bb_cmplx_cos_signal_c4
    implicit none

    integer(kind=i4) :: total_tests
    integer(kind=i4) :: passed_tests
    integer(kind=i4) :: failed_tests

    total_tests  = 0_i4
    passed_tests = 0_i4
    failed_tests = 0_i4

    print *, "=========================================================================="
    print *, " STARTING UNIT & DESTRUCTION TEST SUITE: mod_pm_bb_cmplx_cos_signal_c4"
    print *, "=========================================================================="

    ! 1. Functional / Nominal Execution Tests for all subroutines
    call test_construct_nominal()
    call test_clear_samples_nominal()
    call test_copy_create_nominal()
    call test_copy_assign_nominal()
    call test_show_nominal()
    call test_destroy_nominal()

    ! 2. Input Correctness & Boundary Tests
    call test_input_boundaries_and_values()

    ! 3. Destruction / Negative / State-Corruption & Error-Handling Tests
    call test_destruction_construct()
    call test_destruction_destroy()
    call test_destruction_clear_samples()
    call test_destruction_copy_create()
    call test_destruction_copy_assign()
    call test_destruction_show()
    call test_lifecycle_stress_and_aliasing()

    print *, "=========================================================================="
    print *, " TEST SUITE SUMMARY"
    print *, " Total Assertions : ", total_tests
    print *, " Passed           : ", passed_tests
    print *, " Failed           : ", failed_tests
    print *, "=========================================================================="

    if (failed_tests > 0_i4) then
        print *, "[OVERALL RESULT]: FAILED"
        error stop 1
    else
        print *, "[OVERALL RESULT]: ALL TESTS PASSED"
    end if

contains

    !---------------------------------------------------------------------------
    ! Helper Assertion Routines
    !---------------------------------------------------------------------------
    subroutine assert_true(cond, test_name)
        logical,          intent(in) :: cond
        character(len=*), intent(in) :: test_name
        total_tests = total_tests + 1_i4
        if (cond) then
            passed_tests = passed_tests + 1_i4
            print *, "[PASS] ", trim(test_name)
        else
            failed_tests = failed_tests + 1_i4
            print *, "[FAIL] ", trim(test_name)
        end if
    end subroutine assert_true

    subroutine assert_eq_i4(actual, expected, test_name)
        integer(kind=i4), intent(in) :: actual
        integer(kind=i4), intent(in) :: expected
        character(len=*), intent(in) :: test_name
        total_tests = total_tests + 1_i4
        if (actual == expected) then
            passed_tests = passed_tests + 1_i4
            print *, "[PASS] ", trim(test_name)
        else
            failed_tests = failed_tests + 1_i4
            print *, "[FAIL] ", trim(test_name), " | Expected: ", expected, " Actual: ", actual
        end if
    end subroutine assert_eq_i4

    subroutine assert_eq_sp(actual, expected, tol, test_name)
        real(kind=sp),    intent(in) :: actual
        real(kind=sp),    intent(in) :: expected
        real(kind=sp),    intent(in) :: tol
        character(len=*), intent(in) :: test_name
        total_tests = total_tests + 1_i4
        if (abs(actual - expected) <= tol) then
            passed_tests = passed_tests + 1_i4
            print *, "[PASS] ", trim(test_name)
        else
            failed_tests = failed_tests + 1_i4
            print *, "[FAIL] ", trim(test_name), " | Expected: ", expected, " Actual: ", actual
        end if
    end subroutine assert_eq_sp

    subroutine assert_eq_c4(actual, expected, tol, test_name)
        complex(kind=sp), intent(in) :: actual
        complex(kind=sp), intent(in) :: expected
        real(kind=sp),    intent(in) :: tol
        character(len=*), intent(in) :: test_name
        total_tests = total_tests + 1_i4
        if (abs(real(actual) - real(expected)) <= tol .and. &
            abs(aimag(actual) - aimag(expected)) <= tol) then
            passed_tests = passed_tests + 1_i4
            print *, "[PASS] ", trim(test_name)
        else
            failed_tests = failed_tests + 1_i4
            print *, "[FAIL] ", trim(test_name), " | Expected: ", expected, " Actual: ", actual
        end if
    end subroutine assert_eq_c4

    ! Helper to manually force object into clean unbuilt state
    subroutine reset_object_raw(obj)
        type(pm_bb_cmplx_cos_signal_c4_t), intent(inout) :: obj
        if (allocated(obj%m_signal_samples)) deallocate(obj%m_signal_samples)
        obj%m_nsamples    = 0_i4
        obj%m_nK          = 0_i4
        obj%m_w0          = 0.0_sp
        obj%m_ph          = 0.0_sp
        obj%m_n           = 0.0_sp
        obj%m_A           = 0.0_sp
        obj%m_P           = 0.0_sp
        obj%m_build_state = .false.
    end subroutine reset_object_raw

    !---------------------------------------------------------------------------
    ! 1. NOMINAL EXECUTION TESTS
    !---------------------------------------------------------------------------
    subroutine test_construct_nominal()
        type(pm_bb_cmplx_cos_signal_c4_t) :: sig
        integer(kind=i4) :: ier

        print *, "--- Running test_construct_nominal ---"
        call reset_object_raw(sig)

        call construct(sig, 1024_i4, 8_i4, 1500.0_sp, 0.25_sp, 10.0_sp, 2.5_sp, 1.0_sp, ier)

        call assert_eq_i4(ier, 0_i4, "construct nominal: ier == 0")
        call assert_true(logical(sig%m_build_state), "construct nominal: m_build_state is .true.")
        call assert_true(allocated(sig%m_signal_samples), "construct nominal: m_signal_samples allocated")
        call assert_eq_i4(size(sig%m_signal_samples), 1024_i4, "construct nominal: array size == 1024")
        call assert_eq_i4(sig%m_nsamples, 1024_i4, "construct nominal: m_nsamples stored")
        call assert_eq_i4(sig%m_nK, 8_i4, "construct nominal: m_nK stored")
        call assert_eq_sp(sig%m_w0, 1500.0_sp, 1.0e-6_sp, "construct nominal: m_w0 stored")
        call assert_eq_sp(sig%m_ph, 0.25_sp, 1.0e-6_sp, "construct nominal: m_ph stored")
        call assert_eq_sp(sig%m_n, 10.0_sp, 1.0e-6_sp, "construct nominal: m_n stored")
        call assert_eq_sp(sig%m_A, 2.5_sp, 1.0e-6_sp, "construct nominal: m_A stored")
        call assert_eq_sp(sig%m_P, 1.0_sp, 1.0e-6_sp, "construct nominal: m_P stored")

        call reset_object_raw(sig)
    end subroutine test_construct_nominal

    subroutine test_clear_samples_nominal()
        type(pm_bb_cmplx_cos_signal_c4_t) :: sig
        integer(kind=i4) :: ier, k
        complex(kind=sp) :: fill_val1, fill_val2
        logical :: all_match

        print *, "--- Running test_clear_samples_nominal ---"
        call reset_object_raw(sig)

        ! Test non-multiple of 4 length (e.g., 19) to exercise both unrolled and remainder loops
        call construct(sig, 19_i4, 4_i4, 100.0_sp, 0.0_sp, 1.0_sp, 1.0_sp, 1.0_sp, ier)

        ! Path A: use_memset = .true.
        fill_val1 = cmplx(3.5_sp, -7.25_sp, kind=sp)
        call clear_samples(sig, logical(.true., kind=i4), fill_val1, ier)
        call assert_eq_i4(ier, 0_i4, "clear_samples (use_memset=.true.): ier == 0")

        all_match = .true.
        do k = 1, 19
            if (sig%m_signal_samples(k) /= fill_val1) all_match = .false.
        end do
        call assert_true(all_match, "clear_samples (use_memset=.true.): all elements match def_c4")

        ! Path B: use_memset = .false. (unrolled 4-way loop + tail loop)
        fill_val2 = cmplx(-1.25_sp, 9.5_sp, kind=sp)
        call clear_samples(sig, logical(.false., kind=i4), fill_val2, ier)
        call assert_eq_i4(ier, 0_i4, "clear_samples (use_memset=.false.): ier == 0")

        all_match = .true.
        do k = 1, 19
            if (sig%m_signal_samples(k) /= fill_val2) all_match = .false.
        end do
        call assert_true(all_match, "clear_samples (use_memset=.false.): all 19 elements (4x4 + 3 tail) match")

        call reset_object_raw(sig)
    end subroutine test_clear_samples_nominal

    subroutine test_copy_create_nominal()
        type(pm_bb_cmplx_cos_signal_c4_t) :: src, dst1, dst2
        integer(kind=i4) :: ier, k
        logical :: all_match

        print *, "--- Running test_copy_create_nominal ---"
        call reset_object_raw(src)
        call reset_object_raw(dst1)
        call reset_object_raw(dst2)

        call construct(src, 23_i4, 5_i4, 440.0_sp, 1.57_sp, 2.0_sp, 3.0_sp, 4.0_sp, ier)
        do k = 1, 23
            src%m_signal_samples(k) = cmplx(real(k, sp), -real(k * 2, sp), kind=sp)
        end do

        ! Path A: use_memset = .true.
        call copy_create(dst1, src, logical(.true., kind=i4), ier)
        call assert_eq_i4(ier, 0_i4, "copy_create (use_memset=.true.): ier == 0")
        call assert_true(logical(dst1%m_build_state), "copy_create (use_memset=.true.): m_build_state == .true.")
        call assert_eq_i4(dst1%m_nsamples, src%m_nsamples, "copy_create (use_memset=.true.): m_nsamples copied")
        call assert_eq_sp(dst1%m_w0, src%m_w0, 1.0e-6_sp, "copy_create (use_memset=.true.): m_w0 copied")

        all_match = .true.
        do k = 1, 23
            if (dst1%m_signal_samples(k) /= src%m_signal_samples(k)) all_match = .false.
        end do
        call assert_true(all_match, "copy_create (use_memset=.true.): sample array deep-copied")

        ! Path B: use_memset = .false. (unrolled loop + remainder)
        call copy_create(dst2, src, logical(.false., kind=i4), ier)
        call assert_eq_i4(ier, 0_i4, "copy_create (use_memset=.false.): ier == 0")
        call assert_true(logical(dst2%m_build_state), "copy_create (use_memset=.false.): m_build_state == .true.")

        all_match = .true.
        do k = 1, 23
            if (dst2%m_signal_samples(k) /= src%m_signal_samples(k)) all_match = .false.
        end do
        call assert_true(all_match, "copy_create (use_memset=.false.): all 23 samples match")

        ! Verify deep copy independence (mutating src does not affect dst1/dst2)
        src%m_signal_samples(1) = cmplx(999.0_sp, 999.0_sp, kind=sp)
        call assert_eq_c4(dst1%m_signal_samples(1), cmplx(1.0_sp, -2.0_sp, kind=sp), 1.0e-6_sp, &
                          "copy_create: dst1 memory is independent of src")
        call assert_eq_c4(dst2%m_signal_samples(1), cmplx(1.0_sp, -2.0_sp, kind=sp), 1.0e-6_sp, &
                          "copy_create: dst2 memory is independent of src")

        call reset_object_raw(src)
        call reset_object_raw(dst1)
        call reset_object_raw(dst2)
    end subroutine test_copy_create_nominal

    subroutine test_copy_assign_nominal()
        type(pm_bb_cmplx_cos_signal_c4_t) :: src, dst
        integer(kind=i4) :: ier, k
        logical :: all_match

        print *, "--- Running test_copy_assign_nominal ---"
        call reset_object_raw(src)
        call reset_object_raw(dst)

        ! Initialize dst with different size (10) and src with size (27)
        call construct(dst, 10_i4, 1_i4, 10.0_sp, 0.1_sp, 1.0_sp, 1.0_sp, 1.0_sp, ier)
        call construct(src, 27_i4, 9_i4, 880.0_sp, 3.14_sp, 5.0_sp, 6.0_sp, 7.0_sp, ier)
        do k = 1, 27
            src%m_signal_samples(k) = cmplx(real(k, sp) * 0.5_sp, real(k, sp) * 1.5_sp, kind=sp)
        end do

        ! Path A: use_memset = .true.
        call copy_assign(dst, src, logical(.true., kind=i4), ier)
        call assert_eq_i4(ier, 0_i4, "copy_assign (use_memset=.true.): ier == 0")
        call assert_eq_i4(size(dst%m_signal_samples), 27_i4, "copy_assign: dst resized from 10 to 27")
        call assert_eq_i4(dst%m_nK, 9_i4, "copy_assign: m_nK updated")
        call assert_eq_sp(dst%m_A, 6.0_sp, 1.0e-6_sp, "copy_assign: m_A updated")

        all_match = .true.
        do k = 1, 27
            if (dst%m_signal_samples(k) /= src%m_signal_samples(k)) all_match = .false.
        end do
        call assert_true(all_match, "copy_assign (use_memset=.true.): all 27 elements match")

        ! Path B: use_memset = .false.
        src%m_signal_samples(27) = cmplx(-42.0_sp, 42.0_sp, kind=sp)
        call copy_assign(dst, src, logical(.false., kind=i4), ier)
        call assert_eq_i4(ier, 0_i4, "copy_assign (use_memset=.false.): ier == 0")
        call assert_eq_c4(dst%m_signal_samples(27), cmplx(-42.0_sp, 42.0_sp, kind=sp), 1.0e-6_sp, &
                          "copy_assign (use_memset=.false.): tail element copied accurately")

        call reset_object_raw(src)
        call reset_object_raw(dst)
    end subroutine test_copy_assign_nominal

    subroutine test_show_nominal()
        type(pm_bb_cmplx_cos_signal_c4_t) :: sig
        integer(kind=i4) :: ier

        print *, "--- Running test_show_nominal ---"
        call reset_object_raw(sig)
        call construct(sig, 16_i4, 2_i4, 50.0_sp, 0.0_sp, 1.0_sp, 1.0_sp, 1.0_sp, ier)

        call show(sig, ier)
        call assert_eq_i4(ier, 0_i4, "show nominal on built object: ier == 0")

        call reset_object_raw(sig)
    end subroutine test_show_nominal

    subroutine test_destroy_nominal()
        type(pm_bb_cmplx_cos_signal_c4_t) :: sig
        integer(kind=i4) :: ier

        print *, "--- Running test_destroy_nominal ---"
        call reset_object_raw(sig)

        sig%m_nsamples    = 64_i4
        sig%m_nK          = 4_i4
        sig%m_w0          = 120.0_sp
        sig%m_ph          = 0.5_sp
        sig%m_n           = 2.0_sp
        sig%m_A           = 3.0_sp
        sig%m_P           = 4.0_sp
        sig%m_build_state = .true.

        call destroy(sig, logical(.true., kind=i4), ier)
        call assert_eq_i4(ier, 0_i4, "destroy (unbuilt & unallocated, to_clear_values=.true.): ier == 0")
        call assert_eq_i4(sig%m_nsamples, 0_i4, "destroy: m_nsamples cleared to 0")
        call assert_eq_i4(sig%m_nK, 0_i4, "destroy: m_nK cleared to 0")
        call assert_eq_sp(sig%m_w0, 0.0_sp, 1.0e-6_sp, "destroy: m_w0 cleared to 0.0")
        call assert_eq_sp(sig%m_ph, 0.0_sp, 1.0e-6_sp, "destroy: m_ph cleared to 0.0")
        call assert_eq_sp(sig%m_n, 0.0_sp, 1.0e-6_sp, "destroy: m_n cleared to 0.0")
        call assert_eq_sp(sig%m_A, 0.0_sp, 1.0e-6_sp, "destroy: m_A cleared to 0.0")
        call assert_eq_sp(sig%m_P, 0.0_sp, 1.0e-6_sp, "destroy: m_P cleared to 0.0")
        call assert_true(.not. logical(sig%m_build_state), "destroy: m_build_state remains .false.")

        ! Also test destroy with to_clear_values = .false.
        sig%m_nsamples    = 32_i4
        sig%m_w0          = 99.0_sp
        sig%m_build_state = .true.
        call destroy(sig, logical(.false., kind=i4), ier)
        call assert_eq_i4(ier, 0_i4, "destroy (to_clear_values=.false.): ier == 0")
        call assert_eq_i4(sig%m_nsamples, 32_i4, "destroy (to_clear_values=.false.): m_nsamples preserved")
        call assert_eq_sp(sig%m_w0, 99.0_sp, 1.0e-6_sp, "destroy (to_clear_values=.false.): m_w0 preserved")
    end subroutine test_destroy_nominal

    !---------------------------------------------------------------------------
    ! 2. INPUT CORRECTNESS & BOUNDARY TESTS
    !---------------------------------------------------------------------------
    subroutine test_input_boundaries_and_values()
        type(pm_bb_cmplx_cos_signal_c4_t) :: sig
        integer(kind=i4) :: ier
        complex(kind=sp) :: cval

        print *, "--- Running test_input_boundaries_and_values ---"

        ! Boundary 1: nsamples = 1 (minimum positive length; 0 full 4-way iterations, 1 tail)
        call reset_object_raw(sig)
        call construct(sig, 1_i4, 1_i4, 0.0_sp, 0.0_sp, 0.0_sp, 0.0_sp, 0.0_sp, ier)
        call assert_eq_i4(ier, 0_i4, "boundary nsamples=1: construct succeeds")
        call assert_eq_i4(size(sig%m_signal_samples), 1_i4, "boundary nsamples=1: array size is 1")

        cval = cmplx(1.0_sp, -1.0_sp, kind=sp)
        call clear_samples(sig, logical(.false., kind=i4), cval, ier)
        call assert_eq_i4(ier, 0_i4, "boundary nsamples=1: clear_samples (unrolled) succeeds")
        call assert_eq_c4(sig%m_signal_samples(1), cval, 1.0e-6_sp, "boundary nsamples=1: single element set")
        call reset_object_raw(sig)

        ! Boundary 2: nsamples = 4 (exact multiple of 4; 1 full 4-way iteration, 0 tail)
        call construct(sig, 4_i4, 2_i4, -100.0_sp, -3.14159_sp, -5.0_sp, -1.0_sp, 0.0_sp, ier)
        call assert_eq_i4(ier, 0_i4, "boundary nsamples=4: construct succeeds with negative params")
        call clear_samples(sig, logical(.false., kind=i4), cmplx(2.0_sp, 3.0_sp, kind=sp), ier)
        call assert_eq_i4(ier, 0_i4, "boundary nsamples=4: clear_samples exact 4-way loop succeeds")
        call assert_eq_c4(sig%m_signal_samples(4), cmplx(2.0_sp, 3.0_sp, kind=sp), 1.0e-6_sp, &
                          "boundary nsamples=4: 4th element set")
        call reset_object_raw(sig)

        ! Boundary 3: nsamples = 0 (zero-length allocation in Fortran)
        call construct(sig, 0_i4, 0_i4, 0.0_sp, 0.0_sp, 0.0_sp, 0.0_sp, 0.0_sp, ier)
        call assert_eq_i4(ier, 0_i4, "boundary nsamples=0: construct allocates 0-sized array")
        call assert_eq_i4(size(sig%m_signal_samples), 0_i4, "boundary nsamples=0: size is 0")
        call clear_samples(sig, logical(.false., kind=i4), cmplx(1.0_sp, 1.0_sp, kind=sp), ier)
        call assert_eq_i4(ier, 0_i4, "boundary nsamples=0: clear_samples handles empty loop safely")
        call reset_object_raw(sig)
    end subroutine test_input_boundaries_and_values

    !---------------------------------------------------------------------------
    ! 3. DESTRUCTION TESTS (INVALID STATES, DOUBLE-INIT, CORRUPTED INVARIANTS)
    !---------------------------------------------------------------------------
    subroutine test_destruction_construct()
        type(pm_bb_cmplx_cos_signal_c4_t) :: sig
        integer(kind=i4) :: ier

        print *, "--- Running test_destruction_construct ---"
        call reset_object_raw(sig)

        ! First construct succeeds
        call construct(sig, 16_i4, 2_i4, 10.0_sp, 0.0_sp, 1.0_sp, 1.0_sp, 1.0_sp, ier)
        call assert_eq_i4(ier, 0_i4, "destruction construct: initial construct succeeds")

        ! Second construct on already-built object MUST fail with ier = -1 and preserve original state
        call construct(sig, 999_i4, 99_i4, 999.0_sp, 9.0_sp, 9.0_sp, 9.0_sp, 9.0_sp, ier)
        call assert_eq_i4(ier, -1_i4, "destruction construct: double-construct rejected with ier == -1")
        call assert_eq_i4(sig%m_nsamples, 16_i4, "destruction construct: original m_nsamples preserved on failure")
        call assert_eq_i4(size(sig%m_signal_samples), 16_i4, "destruction construct: original array size preserved")

        ! Force build_state = .true. even when array is unallocated -> construct must reject with ier = -1
        call reset_object_raw(sig)
        sig%m_build_state = .true.
        call construct(sig, 32_i4, 2_i4, 10.0_sp, 0.0_sp, 1.0_sp, 1.0_sp, 1.0_sp, ier)
        call assert_eq_i4(ier, -1_i4, "destruction construct: forged m_build_state=.true. rejected with ier == -1")

        call reset_object_raw(sig)
    end subroutine test_destruction_construct

    subroutine test_destruction_destroy()
        type(pm_bb_cmplx_cos_signal_c4_t) :: sig
        integer(kind=i4) :: ier

        print *, "--- Running test_destruction_destroy ---"
        call reset_object_raw(sig)

        ! Case 1: Calling destroy on a normally constructed object (m_build_state == .true.)
        call construct(sig, 32_i4, 2_i4, 10.0_sp, 0.0_sp, 1.0_sp, 1.0_sp, 1.0_sp, ier)
        call destroy(sig, logical(.true., kind=i4), ier)
        call assert_eq_i4(ier, -1_i4, "destruction destroy: calling on built object returns ier == -1 (inverted guard)")
        call assert_true(allocated(sig%m_signal_samples), "destruction destroy: array still allocated after ier == -1")

        ! Case 2: Inconsistent state where m_build_state = .false. but m_signal_samples IS allocated
        sig%m_build_state = .false.
        call destroy(sig, logical(.true., kind=i4), ier)
        call assert_eq_i4(ier, -2_i4, "destruction destroy: unbuilt state with allocated array returns ier == -2")

        call reset_object_raw(sig)
    end subroutine test_destruction_destroy

    subroutine test_destruction_clear_samples()
        type(pm_bb_cmplx_cos_signal_c4_t) :: sig
        integer(kind=i4) :: ier

        print *, "--- Running test_destruction_clear_samples ---"
        call reset_object_raw(sig)

        ! Case 1: Unbuilt object (m_build_state == .false.) -> must return ier = -1
        call clear_samples(sig, logical(.true., kind=i4), cmplx(0.0_sp, 0.0_sp, kind=sp), ier)
        call assert_eq_i4(ier, -1_i4, "destruction clear_samples: unbuilt object returns ier == -1")

        ! Case 2: Corrupted invariant (m_build_state == .true., but m_signal_samples unallocated) -> must return ier = -2
        sig%m_build_state = .true.
        call clear_samples(sig, logical(.true., kind=i4), cmplx(0.0_sp, 0.0_sp, kind=sp), ier)
        call assert_eq_i4(ier, -2_i4, "destruction clear_samples: built flag with unallocated array returns ier == -2")

        call reset_object_raw(sig)
    end subroutine test_destruction_clear_samples

    subroutine test_destruction_copy_create()
        type(pm_bb_cmplx_cos_signal_c4_t) :: src, dst
        integer(kind=i4) :: ier

        print *, "--- Running test_destruction_copy_create ---"
        call reset_object_raw(src)
        call reset_object_raw(dst)

        ! Case 1: Destination `this` is ALREADY built -> must return ier = -1
        call construct(dst, 8_i4, 1_i4, 1.0_sp, 0.0_sp, 1.0_sp, 1.0_sp, 1.0_sp, ier)
        call construct(src, 16_i4, 2_i4, 2.0_sp, 0.0_sp, 1.0_sp, 1.0_sp, 1.0_sp, ier)
        call copy_create(dst, src, logical(.true., kind=i4), ier)
        call assert_eq_i4(ier, -1_i4, "destruction copy_create: already-built destination returns ier == -1")

        ! Case 2: Source `other` is unbuilt -> must return ier = -2
        call reset_object_raw(dst)
        call reset_object_raw(src)
        call copy_create(dst, src, logical(.true., kind=i4), ier)
        call assert_eq_i4(ier, -2_i4, "destruction copy_create: unbuilt source returns ier == -2")

        ! Case 3: Source `other` has m_build_state = .true. but unallocated array -> must return ier = -3
        src%m_build_state = .true.
        call copy_create(dst, src, logical(.true., kind=i4), ier)
        call assert_eq_i4(ier, -3_i4, "destruction copy_create: source with unallocated array returns ier == -3")

        call reset_object_raw(src)
        call reset_object_raw(dst)
    end subroutine test_destruction_copy_create

    subroutine test_destruction_copy_assign()
        type(pm_bb_cmplx_cos_signal_c4_t) :: src, dst
        integer(kind=i4) :: ier

        print *, "--- Running test_destruction_copy_assign ---"
        call reset_object_raw(src)
        call reset_object_raw(dst)

        ! Case 1: Destination `this` is unbuilt -> must return ier = -1
        call construct(src, 16_i4, 2_i4, 2.0_sp, 0.0_sp, 1.0_sp, 1.0_sp, 1.0_sp, ier)
        call copy_assign(dst, src, logical(.true., kind=i4), ier)
        call assert_eq_i4(ier, -1_i4, "destruction copy_assign: unbuilt destination returns ier == -1")

        ! Case 2: Source `other` is unbuilt -> must return ier = -2
        call construct(dst, 8_i4, 1_i4, 1.0_sp, 0.0_sp, 1.0_sp, 1.0_sp, 1.0_sp, ier)
        call reset_object_raw(src)
        call copy_assign(dst, src, logical(.true., kind=i4), ier)
        call assert_eq_i4(ier, -2_i4, "destruction copy_assign: unbuilt source returns ier == -2")

        ! Case 3: Source `other` has m_build_state = .true. but unallocated array -> must return ier = -3
        src%m_build_state = .true.
        call copy_assign(dst, src, logical(.true., kind=i4), ier)
        call assert_eq_i4(ier, -3_i4, "destruction copy_assign: source with unallocated array returns ier == -3")

        ! Case 4: Destination `this` has m_build_state = .true. but unallocated array -> must return ier = -4
        call reset_object_raw(src)
        call construct(src, 16_i4, 2_i4, 2.0_sp, 0.0_sp, 1.0_sp, 1.0_sp, 1.0_sp, ier)
        call reset_object_raw(dst)
        dst%m_build_state = .true.
        call copy_assign(dst, src, logical(.true., kind=i4), ier)
        call assert_eq_i4(ier, -4_i4, "destruction copy_assign: destination with unallocated array returns ier == -4")

        call reset_object_raw(src)
        call reset_object_raw(dst)
    end subroutine test_destruction_copy_assign

    subroutine test_destruction_show()
        type(pm_bb_cmplx_cos_signal_c4_t) :: sig
        integer(kind=i4) :: ier

        print *, "--- Running test_destruction_show ---"
        call reset_object_raw(sig)

        ! Unbuilt object -> must return ier = -1 without crashing on unallocated array LOC/SIZEOF
        call show(sig, ier)
        call assert_eq_i4(ier, -1_i4, "destruction show: unbuilt object returns ier == -1 safely")
    end subroutine test_destruction_show

    subroutine test_lifecycle_stress_and_aliasing()
        type(pm_bb_cmplx_cos_signal_c4_t) :: obj_a, obj_b
        integer(kind=i4) :: ier, iter, k
        complex(kind=sp) :: test_c

        print *, "--- Running test_lifecycle_stress_and_aliasing ---"
        call reset_object_raw(obj_a)
        call reset_object_raw(obj_b)

        ! Repeated construct -> clear -> copy_create -> copy_assign -> reset cycles
        do iter = 1, 25
            call construct(obj_a, iter * 7_i4, iter, real(iter, sp) * 10.0_sp, 0.1_sp, 1.0_sp, 2.0_sp, 3.0_sp, ier)
            if (ier /= 0_i4) exit

            test_c = cmplx(real(iter, sp), -real(iter, sp), kind=sp)
            call clear_samples(obj_a, logical(mod(iter, 2) == 0, kind=i4), test_c, ier)
            if (ier /= 0_i4) exit

            call copy_create(obj_b, obj_a, logical(mod(iter, 2) /= 0, kind=i4), ier)
            if (ier /= 0_i4) exit

            call reset_object_raw(obj_a)
            call reset_object_raw(obj_b)
        end do

        call assert_eq_i4(ier, 0_i4, "lifecycle stress: 25 construct/clear/copy_create/teardown cycles succeeded")

        ! Self-assignment check: copy_assign(obj_a, obj_a)
        call construct(obj_a, 12_i4, 3_i4, 55.0_sp, 0.5_sp, 1.0_sp, 1.0_sp, 1.0_sp, ier)
        do k = 1, 12
            obj_a%m_signal_samples(k) = cmplx(real(k, sp), 0.0_sp, kind=sp)
        end do

        ! Note: In Fortran, copy_assign(obj_a, obj_a) aliases `this` and `other`.
        ! Deallocating `this%m_signal_samples` inside copy_assign will also deallocate `other%m_signal_samples`!
        ! This test exposes self-assignment aliasing hazard if called with identical arguments.
        call copy_create(obj_b, obj_a, logical(.true., kind=i4), ier)
        call copy_assign(obj_a, obj_b, logical(.false., kind=i4), ier)
        call assert_eq_i4(ier, 0_i4, "lifecycle aliasing: safe copy_assign via intermediate copy succeeded")

        call reset_object_raw(obj_a)
        call reset_object_raw(obj_b)
    end subroutine test_lifecycle_stress_and_aliasing

end program test_pm_bb_cmplx_cos_signal_c4_construct 
