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

module mod_test_rand_pm_gaussian_signal_construct 

    use mod_kinds,                 only : i4, dp 
    use iso_c_binding,             only : c_int, c_long_long 
    use mod_rand_pm_gaussian_signal

    implicit none 
    integer(kind=c_long_long),       parameter :: RDTSC_LATENCY = 18 ! for Skylake arch.
    integer(kind=c_long_long),       parameter :: ZERO          = 0_c_long_long 
       

#if 0
    ICC and ifort commands
    icc -c -std=c99 GMS_intrinsics_wrappers.c
    ifort -o test_rand_pm_gaussian_signal -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -Wall -qopt-report=5  GMS_kinds.f90  GMS_intrinsics_wrappers.o GMS_rand_pm_gaussian_signal_r8.f90 test_rand_pm_gaussian_signal_r8.f90
    -------------------------------------------------------------------------------------------------------------------------------------------------
    ifx -o test_rand_pm_gaussian_signal -fp-model fast=2 -ftz -O3  -march=skylake-avx512      -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=3 \ 
     GMS_kinds.f90  GMS_intrinsics_wrappers.o GMS_rand_pm_gaussian_signal_r8.f90 test_rand_pm_gaussian_signal_r8.f90

    For assembly only:
    ifort -S  test_avx_cvec4_add -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90  GMS_intrinsics_wrappers.o GMS_rand_pm_gaussian_signal_r8.f90 test_rand_pm_gaussian_signal_r8.f90
#endif

contains 


subroutine unit_test_construct()
    use iso_c_binding, only : c_int 
    use IFPORT
#if 0
              interface
                  function raise(sig) bind(C,name="raise")
                           use iso_c_binding, only : c_int 
                           integer(c_int) :: raise 
                           integer(c_int), value :: sig 
                  end function raise 
              end interface
#endif

    interface 
        function rdtsc_wrap() bind(C,name="rdtsc_wrap")
                            use iso_c_binding, only : c_long_long 
                            integer(c_long_long) :: rdtsc_wrap 
        end function rdtsc_wrap 
    end interface
    character(len=64),           parameter     :: header = "[UNIT-TEST] #1: construct -- START]"
    character(len=64),           parameter     :: footer = "[UNIT-TEST] #1: construct -- END]"
    integer(kind=c_long_long),   automatic     :: start,end 
    integer(kind=c_long_long),   automatic     :: start_c,end_c 
    integer(kind=c_long_long),   automatic     :: tsc_elapsed 
    integer(kind=i4),            automatic     :: ier 
    type(rand_pm_gaussian_signal_r8_t), automatic :: test_signal 
    integer(kind=i4),            parameter     :: num_samples   = 1024_i4 
    integer(kind=i4),             parameter     :: num_gaussians = 64_i4
    real(kind=dp), automatic :: tstart,tend,min_sigma,max_sigma,max_amplitude 
    tstart    = 0.0_dp
    tend      = 10.0_dp
    min_sigma = 0.05_dp
    max_sigma = 0.5_dp
    max_amplitude = 2.54_dp
    print*, header 
    print*, "[UNIT-TEST]: [1] -- subroutine construct: rand_pm_gaussian_signal_r8_t, STARTED."
    start = rdtsc_wrap()
    call construct(test_signal,num_samples,num_gaussians,tstart,tend,min_sigma,max_sigma,max_amplitude,ier)
    end   = rdtsc_wrap()
    start_c     = start-RDTSC_LATENCY
    end_c       = end-RDTSC_LATENCY
    tsc_elapsed = end_c-start_c 
    if(tsc_elapsed<ZERO) then
        print*,"[INVALID-TSC]=", tsc_elapsed
    else 
        print*, "[WARNING]: Crude timing measurement!!"
        print*,"[TSC]=", tsc_elapsed 
    end if
    if(ier .ne. 0) then 
       if(ier .eq. -9999) print*, "[UNIT-TEST]: [1] subroutine construct: execution did not process past prologue!!"
       if(ier .eq. -1)    print*, "[UNIT-TEST]: [1] subroutine construct: derived type already created!!"
    else if(ier .eq. 0) then 
       print*, "[UNIT-TEST]: [1] subroutine construct: execution did reached end!!"
       print*, "ier=", ier
    end if 
    print*, "[UNIT-TEST]: [1] -- subroutine construct: rand_pm_gaussian_signal_r8_t, ENDED."
    print*, footer 
end subroutine unit_test_construct 

end module mod_test_rand_pm_gaussian_signal_construct 

program main 
    use mod_test_rand_pm_gaussian_signal_construct 
    call unit_test_construct()
end program 