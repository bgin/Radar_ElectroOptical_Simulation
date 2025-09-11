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

module mod_test_trapezoid_waveform_copy


       use mod_kinds,                 only : i4, i8, sp 
       use iso_c_binding,             only : c_int, c_long_long 
       use mod_trapezoid_waveform

       implicit none 
       integer(kind=c_long_long),       parameter :: RDTSC_LATENCY = 18 ! for Skylake arch.
       integer(kind=c_long_long),       parameter :: ZERO          = 0_c_long_long 
       

#if 0
    ICC and ifort commands
    icc -c -std=c99 GMS_intrinsics_wrappers.c
    ifort -o test_trapez_waveform_copy -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90  GMS_intrinsics_wrappers.o GMS_trapezoid_waveform.f90 test_trapez_waveform_copy.f90
    -------------------------------------------------------------------------------------------------------------------------------------------------
    ifx -o test_trapez_waveform_copy -fp-model fast=2 -ftz -O3  -march=skylake-avx512      -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=3 \ 
    GMS_kinds.f90 GMS_intrinsics_wrappers.o GMS_kinds.f90  GMS_intrinsics_wrappers.o GMS_trapezoid_waveform.f90 test_trapez_waveform_copy.f90

    For assembly only:
    ifort -S   -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90  GMS_intrinsics_wrappers.o GMS_trapezoid_waveform.f90 test_trapez_waveform_copy.f90
#endif

      contains 

subroutine unit_test_trapezoid_waveform_copy()
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
               character(len=128),          automatic :: filename 
               integer(c_int),              parameter :: SIGTRAP = 5 
               character(len=64),           parameter  :: header = "[TEST #4: copy -- START]"
               character(len=64),           parameter  :: footer = "[TEST #4: copy -- END]"
               integer(kind=c_long_long),   automatic  :: start,end 
               integer(kind=c_long_long),   automatic  :: start_c,end_c 
               integer(kind=c_long_long),   automatic  :: tsc_elapsed 
               type(trapezoid_waveform_t),  automatic  :: waveform_1__ 
               type(trapezoid_waveform_t),  automatic  :: waveform_2__ 
               print*, header 
               !start  = rdtsc_wrap()
               call construct(waveform_1__,1024,32,32,32,32,32)
               !end         = rdtsc_wrap()
               !start_c     = start-RDTSC_LATENCY
               !end_c       = end-RDTSC_LATENCY
               !tsc_elapsed = end_c-start_c 
               !if(tsc_elapsed<ZERO) then
               !  print*,"[INVALID-TSC]=", tsc_elapsed
               !else 
               !  print*, "[WARNING]: Crude timing measurement!!"
                ! print*,"[TSC]=", tsc_elapsed 
               !end if 
               print*,"[TEST #2: Dumping object state.]"
               print*, "n_samples__  = ", waveform_1__.n_samples__ 
               print*, "n_waves__    = ", waveform_1__.n_waves__ 
               print*, "n_param_a__  = ", waveform_1__.n_param_a__ 
               print*, "n_param_l__  = ", waveform_1__.n_param_l__ 
               print*, "n_param_c__  = ", waveform_1__.n_param_c__ 
               print*, "n_param_m__  = ", waveform_1__.n_param_m__ 
               print*, "build_state__= ", waveform_1__.build_state__
               call check_allocation_stats_real1D(waveform_1__.tw_samples__)
               !start  = rdtsc_wrap()
               call copy(waveform_2__,waveform_1__)
               ! end         = rdtsc_wrap()
               !start_c     = start-RDTSC_LATENCY
               !end_c       = end-RDTSC_LATENCY
               !tsc_elapsed = end_c-start_c 
               !if(tsc_elapsed<ZERO) then
              !   print*,"[INVALID-TSC]=", tsc_elapsed
               !else 
               !  print*, "[WARNING]: Crude timing measurement!!"
               !  print*,"[TSC]=", tsc_elapsed 
              ! end if 
               call check_allocation_stats_real1D(waveform_2__.tw_samples__)
               print*, waveform_1__.tw_samples__ 
               print*, "n_samples__  = ", waveform_2__.n_samples__ 
               print*, "n_waves__    = ", waveform_2__.n_waves__ 
               print*, "n_param_a__  = ", waveform_2__.n_param_a__ 
               print*, "n_param_l__  = ", waveform_2__.n_param_l__ 
               print*, "n_param_c__  = ", waveform_2__.n_param_c__ 
               print*, "n_param_m__  = ", waveform_2__.n_param_m__ 
               print*, "build_state__= ", waveform_2__.build_state__
               print*, footer 
               contains 

               subroutine check_allocation_stats_real1D(array)
                       implicit none
                       real(kind=sp), allocatable, dimension(:), intent(in) :: array 
                       ! Locals
                       integer(kind=i8), automatic :: vaddr 
                       integer(kind=i8), automatic :: size 
                       integer(kind=i4), automatic :: lo_bound
                       integer(kind=i4), automatic :: hi_bound 
                       logical(kind=i1), automatic :: is_allocated 
                       vaddr        = LOC(array)
                       size         = SIZEOF(array)
                       lo_bound     = LBOUND(array,DIM=1)
                       hi_bound     = UBOUND(array,DIM=1)
                       is_allocated = allocated(array)
                       print*,"======================INFO======================================"
                       print*, "vaddr=",vaddr,"size=",size,"allocated=",is_allocated
                       print*, "lo_bound=",lo_bound,"hi_bound=",hi_bound 
                       print*,"======================INFO======================================"
              end subroutine check_allocation_stats_real1D
end subroutine unit_test_trapezoid_waveform_copy


end module mod_test_trapezoid_waveform_copy


program main 
    use mod_test_trapezoid_waveform_copy
    call unit_test_trapezoid_waveform_copy()
end program main 