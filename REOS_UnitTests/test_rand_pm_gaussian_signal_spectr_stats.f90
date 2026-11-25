
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
    ifort -o test_rand_pm_gaussian_signal_generate_spectr_states -reentrancy threaded -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -Wall -qopt-report=5  GMS_kinds.f90  GMS_intrinsics_wrappers.o GMS_rand_pm_gaussian_signal_r8.f90 test_rand_pm_gaussian_signal_generate_spectr_stats.f90
    -------------------------------------------------------------------------------------------------------------------------------------------------
    ifx -o test_rand_pm_gaussian_signal_generate_spectr_stats -reentrancy threaded -fp-model fast=2 -ftz -O3  -march=skylake-avx512      -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=3 \ 
     GMS_kinds.f90  GMS_intrinsics_wrappers.o GMS_rand_pm_gaussian_signal_r8.f90 test_rand_pm_gaussian_signal_generate_spectr_stats.f90

    For assembly only:
    ifort -S test_rand_pm_gaussian_signal_generate_spectr_stats -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90  GMS_intrinsics_wrappers.o GMS_rand_pm_gaussian_signal_r8.f90 test_rand_pm_gaussian_signal_generate_spectr_stats.f90

#endif


program verify_spectral_stats
    use mod_kinds, only : i4, i8, dp
    use mod_rand_pm_gaussian_signal
    implicit none

    real(kind=dp), parameter :: PI     = 3.1415926535897932384626433832795_dp
    real(kind=dp), parameter :: TWO_PI = 2.0_dp * PI

    ! Must be a power of 2 for the built-in Radix-2 FFT
    integer(kind=i4), parameter :: N_SAMPLES     = 8192_i4
    integer(kind=i4), parameter :: N_GAUSSIANS   = 400_i4
    real(kind=dp),    parameter :: T_START       = 0.0_dp
    real(kind=dp),    parameter :: T_END         = 1.0_dp
    real(kind=dp),    parameter :: MIN_SIGMA     = 0.005_dp
    real(kind=dp),    parameter :: MAX_SIGMA     = 0.05_dp
    real(kind=dp),    parameter :: MAX_AMPLITUDE = 12.0_dp

    type(rand_pm_gaussian_signal_r8_t) :: sig
    real(kind=dp), allocatable :: freq(:), psd(:), wrapped_phase(:)
    real(kind=dp) :: dt, fs, parseval_rel_err
    integer(kind=i4) :: ier

    print *, "====================================================================="
    print *, "SPECTRAL (FFT PSD) & PHASE DISTRIBUTION VERIFICATION"
    print *, "====================================================================="

    ! 1. Construct and generate signal
    sig%m_build_state = .false.
    call construct(sig, N_SAMPLES, N_GAUSSIANS, T_START, T_END, &
                   MIN_SIGMA, MAX_SIGMA, MAX_AMPLITUDE, ier)
    if (ier /= 0) stop "Error in construct"

    call generate(sig, ier)
    if (ier /= 0) stop "Error in generate"

    dt = (sig%m_tend - sig%m_tstart) / real(sig%m_nsamples - 1_i4, kind=dp)
    fs = 1.0_dp / dt

    print '(A, I8)',       "  Samples (N)          : ", sig%m_nsamples
    print '(A, ES12.4, A)',"  Sampling Rate (Fs)   : ", fs, " Hz"
    print '(A, ES12.4, A)',"  Freq Resolution (df) : ", fs / real(N_SAMPLES, kind=dp), " Hz"
    print *, "---------------------------------------------------------------------"

    ! 2. Compute and verify FFT Power Spectral Density
    allocate(freq(N_SAMPLES/2 + 1), psd(N_SAMPLES/2 + 1))
    call analyze_fft_psd(sig%m_signal_samples, N_SAMPLES, fs, freq, psd, parseval_rel_err)

    ! 3. Analyze Wrapped Phase Distribution [-pi, pi)
    allocate(wrapped_phase(N_SAMPLES))
    call analyze_phase_distribution(sig%m_signal_samples, N_SAMPLES, wrapped_phase)

    ! 4. Export data to CSV for external plotting
    call export_verification_csv(sig, freq, psd, wrapped_phase, dt)

    ! 5. Clean up
    deallocate(freq, psd, wrapped_phase)
    call destroy(sig, .true., ier)

    print *, "====================================================================="
    print *, "VERIFICATION COMPLETE. CSV files written: psd_output.csv, phase_dist_output.csv"
    print *, "====================================================================="

contains

    !===========================================================================
    ! In-Place Iterative Radix-2 Cooley-Tukey FFT (Decimation-In-Time)
    !===========================================================================
    subroutine fft_radix2(x, n)
        integer(kind=i4), intent(in)    :: n
        complex(kind=dp), intent(inout) :: x(n)
        integer(kind=i4) :: i, j, k, m, half_m, step
        complex(kind=dp) :: temp, w, wm
        real(kind=dp)    :: theta

        ! Bit-reversal permutation
        j = 1_i4
        do i = 1_i4, n
            if (i < j) then
                temp = x(j)
                x(j) = x(i)
                x(i) = temp
            end if
            m = n / 2_i4
            do while (m >= 1_i4 .and. j > m)
                j = j - m
                m = m / 2_i4
            end do
            j = j + m
        end do

        ! Butterfly stages
        step = 2_i4
        do while (step <= n)
            half_m = step / 2_i4
            theta  = -TWO_PI / real(step, kind=dp)
            wm     = cmplx(cos(theta), sin(theta), kind=dp)
            do k = 1_i4, n, step
                w = (1.0_dp, 0.0_dp)
                do j = 0_i4, half_m - 1_i4
                    temp              = w * x(k + j + half_m)
                    x(k + j + half_m) = x(k + j) - temp
                    x(k + j)          = x(k + j) + temp
                    w                 = w * wm
                end do
            end do
            step = step * 2_i4
        end do
    end subroutine fft_radix2

    !===========================================================================
    ! Computes One-Sided PSD, Parseval Energy Check, Peak Freq, and 99% Bandwidth
    !===========================================================================
    subroutine analyze_fft_psd(samples, n, fs, freq, psd, parseval_rel_err)
        integer(kind=i4), intent(in)  :: n
        real(kind=dp),    intent(in)  :: samples(n), fs
        real(kind=dp),    intent(out) :: freq(n/2 + 1), psd(n/2 + 1), parseval_rel_err

        complex(kind=dp), allocatable :: X(:)
        real(kind=dp) :: df, time_energy, freq_energy, cum_energy, total_psd
        real(kind=dp) :: peak_psd, peak_freq, bw_99
        integer(kind=i4) :: k, n_onesided

        if (iand(n, n - 1_i4) /= 0_i4) then
            stop "[ERROR] N_SAMPLES must be a power of 2 for Radix-2 FFT."
        end if

        n_onesided = n / 2_i4 + 1_i4
        df = fs / real(n, kind=dp)

        allocate(X(n))
        X = cmplx(samples, 0.0_dp, kind=dp)
        call fft_radix2(X, n)

        ! Compute one-sided PSD: |X[k]|^2 / (Fs * N), doubled for non-DC/Nyquist bins
        do k = 1_i4, n_onesided
            freq(k) = real(k - 1_i4, kind=dp) * df
            psd(k)  = (real(X(k), kind=dp)**2 + aimag(X(k))**2) / (fs * real(n, kind=dp))
            if (k > 1_i4 .and. k < n_onesided) then
                psd(k) = 2.0_dp * psd(k)
            end if
        end do

        ! Parseval's Theorem check: sum(x^2)*dt == sum(PSD)*df
        time_energy = sum(samples**2) * (1.0_dp / fs)
        freq_energy = sum(psd) * df
        parseval_rel_err = abs(time_energy - freq_energy) / max(time_energy, 1.0e-30_dp)

        ! Find Peak PSD and 99% Occupied Bandwidth
        peak_psd  = -1.0_dp
        peak_freq = 0.0_dp
        do k = 1_i4, n_onesided
            if (psd(k) > peak_psd) then
                peak_psd  = psd(k)
                peak_freq = freq(k)
            end if
        end do

        total_psd  = sum(psd)
        cum_energy = 0.0_dp
        bw_99      = freq(n_onesided)
        do k = 1_i4, n_onesided
            cum_energy = cum_energy + psd(k)
            if (cum_energy >= 0.99_dp * total_psd) then
                bw_99 = freq(k)
                exit
            end if
        end do

        print *, "[1] FFT POWER SPECTRAL DENSITY (PSD) METRICS:"
        print '(A, ES14.6)',    "    Time-Domain Energy      : ", time_energy
        print '(A, ES14.6)',    "    Freq-Domain Energy      : ", freq_energy
        print '(A, ES14.6)',    "    Parseval Relative Error : ", parseval_rel_err
        if (parseval_rel_err > 1.0e-12_dp) then
            print *, "    [FAIL] Parseval energy conservation check failed!"
            stop 1
        else
            print *, "    [PASS] Parseval energy conservation verified (< 1e-12)."
        end if
        print '(A, ES14.6, A)', "    Peak PSD Frequency      : ", peak_freq, " Hz"
        print '(A, ES14.6, A)', "    99% Occupied Bandwidth  : ", bw_99, " Hz"
        print *, "---------------------------------------------------------------------"

        deallocate(X)
    end subroutine analyze_fft_psd

    !===========================================================================
    ! Wraps Phase to [-pi, pi), Computes Circular Stats & Chi-Square Uniformity
    !===========================================================================
    subroutine analyze_phase_distribution(raw_phase, n, wrapped)
        integer(kind=i4), intent(in)  :: n
        real(kind=dp),    intent(in)  :: raw_phase(n)
        real(kind=dp),    intent(out) :: wrapped(n)

        integer(kind=i4), parameter :: N_BINS = 16_i4
        integer(kind=i4) :: hist(N_BINS)
        integer(kind=i4) :: i, bin_idx
        real(kind=dp)    :: mean_w, var_w, std_w, expected_count, chi_sq
        real(kind=dp)    :: sum_cos, sum_sin, r_resultant, circ_var, bin_width

        hist    = 0_i4
        sum_cos = 0.0_dp
        sum_sin = 0.0_dp
        bin_width = TWO_PI / real(N_BINS, kind=dp)

        ! Wrap raw phase into [-pi, pi) using modulo
        do i = 1_i4, n
            wrapped(i) = modulo(raw_phase(i) + PI, TWO_PI) - PI
            sum_cos = sum_cos + cos(wrapped(i))
            sum_sin = sum_sin + sin(wrapped(i))

            bin_idx = int((wrapped(i) + PI) / bin_width, kind=i4) + 1_i4
            if (bin_idx < 1_i4)      bin_idx = 1_i4
            if (bin_idx > N_BINS)    bin_idx = N_BINS
            hist(bin_idx) = hist(bin_idx) + 1_i4
        end do

        ! Linear statistics on [-pi, pi) (Theoretical Uniform: mean = 0, std = pi/sqrt(3) ~ 1.8138)
        mean_w = sum(wrapped) / real(n, kind=dp)
        var_w  = sum((wrapped - mean_w)**2) / real(n - 1_i4, kind=dp)
        std_w  = sqrt(var_w)

        ! Circular statistics (R -> 0 for uniform circular phase, R -> 1 for concentrated phase)
        r_resultant = hypot(sum_cos, sum_sin) / real(n, kind=dp)
        circ_var    = 1.0_dp - r_resultant

        ! Chi-square goodness-of-fit against uniform distribution across N_BINS
        expected_count = real(n, kind=dp) / real(N_BINS, kind=dp)
        chi_sq = 0.0_dp
        do i = 1_i4, N_BINS
            chi_sq = chi_sq + ((real(hist(i), kind=dp) - expected_count)**2) / expected_count
        end do

        print *, "[2] WRAPPED PHASE DISTRIBUTION [-pi, pi) METRICS:"
        print '(A, F10.5, A)', "    Wrapped Mean            : ", mean_w, " rad (Uniform ideal: 0.00000)"
        print '(A, F10.5, A)', "    Wrapped Std Dev         : ", std_w,  " rad (Uniform ideal: 1.81380)"
        print '(A, F10.5)',    "    Circular Resultant (R)  : ", r_resultant
        print '(A, F10.5)',    "    Circular Variance (1-R) : ", circ_var
        print '(A, F10.3, A, I2)', "    Chi-Square Statistic    : ", chi_sq, " | DOF: ", N_BINS - 1
        print *, "    16-Bin Phase Histogram Counts ([-pi .. +pi)):"
        print '(4X, 8I7)', hist(1:8)
        print '(4X, 8I7)', hist(9:16)
end subroutine analyze_phase_distribution
!===========================================================================
! Exports PSD and Phase Time-Series to CSV Files
!===========================================================================
subroutine export_verification_csv(sig, freq, psd, wrapped_phase, dt)
    type(rand_pm_gaussian_signal_r8_t), intent(in) :: sig
    real(kind=dp), intent(in) :: freq(:), psd(:), wrapped_phase(:), dt
    integer(kind=i4) :: i, unit_psd, unit_ph
    real(kind=dp)    :: t
    open(newunit=unit_psd, file="psd_output.csv", status="replace", action="write")
    write(unit_psd, '(A)') "Frequency_Hz,PSD_OneSided"
    do i = 1, size(freq)
       write(unit_psd, '(ES20.12, ",", ES20.12)') freq(i), psd(i)
    end do
    close(unit_psd)
    open(newunit=unit_ph, file="phase_dist_output.csv", status="replace", action="write")
    write(unit_ph, '(A)') "Sample_Index,Time_s,Raw_Phase_rad,Wrapped_Phase_rad"
    do i = 1, sig%m_nsamples
       t = sig%m_tstart + real(i - 1_i4, kind=dp) * dt
       write(unit_ph, '(I8, ",", ES20.12, ",", ES20.12, ",", ES20.12)') &
                i, t, sig%m_signal_samples(i), wrapped_phase(i)
    end do
    close(unit_ph)
    end subroutine export_verification_csv
end program verify_spectral_stats