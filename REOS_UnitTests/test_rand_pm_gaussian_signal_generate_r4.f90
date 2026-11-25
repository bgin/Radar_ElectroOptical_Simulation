

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
    ifort -o test_rand_pm_gaussian_signal_generate_r4 -reentrancy threaded -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -Wall -qopt-report=5  GMS_kinds.f90  GMS_intrinsics_wrappers.o GMS_rand_pm_gaussian_signal_r4.f90 test_rand_pm_gaussian_signal_generate_r4.f90
    -------------------------------------------------------------------------------------------------------------------------------------------------
    ifx -o test_rand_pm_gaussian_signal_generate_r4 -reentrancy threaded -fp-model fast=2 -ftz -O3  -march=skylake-avx512      -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=3 \ 
     GMS_kinds.f90  GMS_intrinsics_wrappers.o GMS_rand_pm_gaussian_signal_r8.f90 test_rand_pm_gaussian_signal_generate_r4.f90

    For assembly only:
    ifort -S  test_rand_pm_gaussian_signal_generate_r4 -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90  GMS_intrinsics_wrappers.o GMS_rand_pm_gaussian_signal_r4.f90 test_rand_pm_gaussian_signal_generate_r4.f90

#endif

program test_rand_pm_gaussian_signal_r4 
    use mod_kinds, only : i4, i8, sp
    use mod_rand_pm_gaussian_signal_r4
    use omp_lib
    use ieee_arithmetic, only : ieee_is_nan, ieee_is_finite
    implicit none

    ! Run all three test suites
    call test_single_signal()
    call test_signal_array_serial()
    call test_signal_array_openmp()

contains

    !===========================================================================
    ! Helper Subroutine: Validates generated signal samples for NaNs, Infs,
    ! non-trivial variance, and computes basic summary statistics.
    !===========================================================================
    subroutine validate_signal_data(sig, label, is_valid)
        type(rand_pm_gaussian_signal_r4_t), intent(in)  :: sig
        character(len=*),                   intent(in)  :: label
        logical,                            intent(out) :: is_valid
        integer(kind=i4) :: idx
        real(kind=sp)    :: min_val, max_val, mean_val, sum_val, val

        is_valid = .true.
        if (.not. allocated(sig%m_signal_samples)) then
            print *, "[FAIL] ", trim(label), ": m_signal_samples is not allocated."
            is_valid = .false.
            return
        end if

        min_val = huge(1.0_sp)
        max_val = -huge(1.0_sp)
        sum_val = 0.0_sp

        do idx = 1, sig%m_nsamples
            val = sig%m_signal_samples(idx)
            if (ieee_is_nan(val) .or. .not. ieee_is_finite(val)) then
                print *, "[FAIL] ", trim(label), ": NaN or Inf detected at index ", idx
                is_valid = .false.
                return
            end if
            if (val < min_val) min_val = val
            if (val > max_val) max_val = val
            sum_val = sum_val + val
        end do

        mean_val = sum_val / real(sig%m_nsamples, kind=sp)

        ! Ensure the signal is not completely flat/zeroed
        if (abs(max_val - min_val) < 1.0e-7_sp) then
            print *, "[FAIL] ", trim(label), ": Signal appears completely flat (zero variance)."
            is_valid = .false.
            return
        end if

        print '(A, A, A, ES12.4, A, ES12.4, A, ES12.4)', &
              "  [OK] ", trim(label), " | Min: ", min_val, " | Max: ", max_val, " | Mean: ", mean_val
    end subroutine validate_signal_data

    !===========================================================================
    ! TEST 1: Single Signal Array Generation & Edge-Case Verification
    !===========================================================================
    subroutine test_single_signal()
        type(rand_pm_gaussian_signal_r4_t) :: sig
        integer(kind=i4) :: ier
        integer(kind=i4), parameter :: N_SAMPLES     = 10000_i4
        integer(kind=i4), parameter :: N_GAUSSIANS   = 150_i4
        real(kind=sp),    parameter :: T_START       = -1.0_sp
        real(kind=sp),    parameter :: T_END         = 1.0_sp
        real(kind=sp),    parameter :: MIN_SIGMA     = 0.01_sp
        real(kind=sp),    parameter :: MAX_SIGMA     = 0.15_sp
        real(kind=sp),    parameter :: MAX_AMPLITUDE = 5.0_sp
        logical :: ok

        print *, "====================================================================="
        print *, "TEST 1: Single Signal Array Generation"
        print *, "====================================================================="

        ! Initialize build state explicitly before construct
        sig%m_build_state = .false.

        call construct(sig, N_SAMPLES, N_GAUSSIANS, T_START, T_END, &
                       MIN_SIGMA, MAX_SIGMA, MAX_AMPLITUDE, ier)
        if (ier /= 0) then
            print *, "[ERROR] construct failed with ier = ", ier
            stop 1
        end if

        ! Pre-fill with a sentinel value to ensure generate overwrites all elements
        call clear_samples(sig, -999.0_sp, .true., ier)
        if (ier /= 0) then
            print *, "[ERROR] clear_samples failed with ier = ", ier
            stop 1
        end if

        ! Generate the random phase-modulated Gaussian signal
        call generate(sig, ier)
        if (ier /= 0) then
            print *, "[ERROR] generate failed with ier = ", ier
            stop 1
        end if

        ! Display object metadata and validate samples
        call show(sig, ier)
        call validate_signal_data(sig, "Single Signal (10k samples)", ok)
        if (.not. ok) stop 1

        ! Verify error handling when m_num_gaussians exceeds MAX_BUF_SIZE (1000)
        sig%m_num_gaussians = 1500_i4
        call generate(sig, ier)
        if (ier == -1_i4) then
            print *, "  [OK] Boundary check passed: generate rejected m_num_gaussians > 1000 (ier = -1)."
        else
            print *, "[FAIL] Boundary check failed: expected ier = -1, got ", ier
            stop 1
        end if

        ! Clean up
        call destroy(sig, .true., ier)
        if (ier /= 0) then
            print *, "[ERROR] destroy failed with ier = ", ier
            stop 1
        end if
        print *, "TEST 1 PASSED SUCCESSFULLY."
        print *, ""
    end subroutine test_single_signal

    !===========================================================================
    ! TEST 2: Serial Array of Signals (Ensemble Generation)
    !===========================================================================
    subroutine test_signal_array_serial()
        integer(kind=i4), parameter :: NUM_SIGNALS   = 16_i4
        integer(kind=i4), parameter :: N_SAMPLES     = 4096_i4
        type(rand_pm_gaussian_signal_r4_t), dimension(NUM_SIGNALS) :: sig_array
        integer(kind=i4) :: i, ier, num_gauss
        real(kind=sp)    :: t0, t1, max_amp
        character(len=32):: label
        logical :: ok

        print *, "====================================================================="
        print *, "TEST 2: Serial Array of Signals (Ensemble of ", NUM_SIGNALS, " signals)"
        print *, "====================================================================="

        t0 = omp_get_wtime()

        do i = 1, NUM_SIGNALS
            sig_array(i)%m_build_state = .false.
            ! Vary parameters slightly per signal in the ensemble
            num_gauss = 50_i4 + (i * 10_i4)
            max_amp   = 1.0_sp + 0.25_sp * real(i, kind=sp)

            call construct(sig_array(i), N_SAMPLES, num_gauss, -0.5_sp, 0.5_sp, &
                           0.005_sp, 0.08_sp, max_amp, ier)
            if (ier /= 0) then
                print *, "[ERROR] Serial construct failed at index ", i, " ier = ", ier
                stop 1
            end if

            call generate(sig_array(i), ier)
            if (ier /= 0) then
                print *, "[ERROR] Serial generate failed at index ", i, " ier = ", ier
                stop 1
            end if
        end do

        t1 = omp_get_wtime()

        ! Validate all signals in the array
        do i = 1, NUM_SIGNALS
            write(label, '(A,I2.2)') "Serial Signal #", i
            call validate_signal_data(sig_array(i), label, ok)
            if (.not. ok) stop 1
        end do

        ! Verify distinctness between consecutive signals
        if (all(sig_array(1)%m_signal_samples == sig_array(2)%m_signal_samples)) then
            print *, "[WARN] Signal #1 and Signal #2 are identical (possible clock seed collision)."
        else
            print *, "  [OK] Verified consecutive ensemble signals are statistically distinct."
        end if

        ! Clean up
        do i = 1, NUM_SIGNALS
            call destroy(sig_array(i), .true., ier)
            if (ier /= 0) stop 1
        end do

        print '(A, F10.6, A)', "TEST 2 PASSED SUCCESSFULLY in ", (t1 - t0), " seconds."
        print *, ""
    end subroutine test_signal_array_serial 

    !===========================================================================
    ! TEST 3: OpenMP Parallel Array of Signals
    !===========================================================================
    subroutine test_signal_array_openmp()
        integer(kind=i4), parameter :: NUM_SIGNALS   = 32_i4
        integer(kind=i4), parameter :: N_SAMPLES     = 8192_i4
        type(rand_pm_gaussian_signal_r4_t), allocatable :: omp_signals(:)
        integer(kind=i4) :: i, ier, err_count, n_threads
        real(kind=sp)    :: t0, t1
        character(len=32):: label
        logical :: ok

        print *, "====================================================================="
        print *, "TEST 3: OpenMP Parallel Array of Signals (", NUM_SIGNALS, " signals)"
        print *, "====================================================================="

        allocate(omp_signals(NUM_SIGNALS))
        err_count = 0_i4

        !$omp parallel
        !$omp single
        n_threads = omp_get_num_threads()
        print *, "  Running OpenMP region with ", n_threads, " threads."
        !$omp end single
        !$omp end parallel

        t0 = omp_get_wtime()

        ! Parallelize construction and signal generation across threads
        !$omp parallel do default(none) &
        !$omp shared(omp_signals) &
        !$omp private(i, ier) &
        !$omp reduction(+:err_count) &
        !$omp schedule(dynamic, 1)
        do i = 1, NUM_SIGNALS
            omp_signals(i)%m_build_state = .false.

            call construct(omp_signals(i), N_SAMPLES, 250_i4, -2.0_sp, 2.0_sp, &
                           0.01_sp, 0.20_sp, 10.0_sp, ier)
            if (ier /= 0) then
                err_count = err_count + 1
                cycle
            end if

            call generate(omp_signals(i), ier)
            if (ier /= 0) then
                err_count = err_count + 1
            end if
        end do
        !$omp end parallel do

        t1 = omp_get_wtime()

        if (err_count /= 0) then
            print *, "[ERROR] OpenMP execution encountered ", err_count, " errors."
            stop 1
        end if

        ! Validate generated signals sequentially after parallel region
        do i = 1, NUM_SIGNALS
           write(label, '(A,I2.2)') "OMP Signal #", i
           call validate_signal_data(omp_signals(i), label, ok)
           if (.not. ok) stop 1
        end do
        ! Clean up in parallel
!$omp parallel do default(none) shared(omp_signals) private(i, ier)
        do i = 1, NUM_SIGNALS
           call destroy(omp_signals(i), .true., ier)
        end do
!$omp end parallel do
        deallocate(omp_signals)
        print '(A, F10.6, A)', "TEST 3 PASSED SUCCESSFULLY in ", (t1 - t0), " seconds."
        print *, ""
end subroutine test_signal_array_openmp
end program test_rand_pm_gaussian_signal_r4 