
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

module mod_test_func_f267 

       use mod_kinds,        only : i1, i4, i8, sp, dp 
       use iso_c_binding,    only : c_int, c_long_long 
       use atmos_refraction, only : rho_to_a_f267_r4, rho_to_a_f267_r8
       implicit none
       
       character(len=64),               parameter :: row1          =  "20,0,101.325,633,1.000271800,1.000271799,0.1"
       integer(kind=c_long_long),       parameter :: RDTSC_LATENCY = 18 ! for Skylake arch.
       integer(kind=c_long_long),       parameter :: ZERO          = 0_c_long_long 

#if 0
    ICC and ifort commands
    icc -c -std=c99 GMS_intrinsics_wrappers.c
    ifort -o unit_test_func_f267 -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90 GMS_atmos_refraction.f90 GMS_intrinsics_wrappers.o test_func_f267.f90
    -------------------------------------------------------------------------------------------------------------------------------------------------
    For assembly only:
    ifort -S unit_test_func_f267 -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90 GMS_atmos_refraction.f90 GMS_intrinsics_wrappers.o test_func_f267.f90
#endif


      contains 

subroutine unit_test_rho_to_a_f267_r4()
           use iso_c_binding, only : c_int, c_long_long 
           use IFPORT 
           implicit none 
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
              character(len=50),         parameter  :: header = "[TEST #9: rho_to_a_f267_r4 -- START]"
              character(len=48),         parameter  :: footer = "[TEST #9: rho_to_a_f267_r4 -- END]"
              integer(kind=c_long_long), automatic  :: start, end 
              integer(kind=c_long_long), automatic  :: start_c, end_c,tsc_elapsed 
              integer(c_int),            parameter :: SIGTRAP = 5 
              real(kind=sp),             automatic  :: dndh
              real(kind=sp),             automatic  :: R 
              print*, header 
              dndh    = -0.00000004_sp 
              R       = 0.0_sp 
#if 0
              raise(SIGTRAP)
#endif
              start   = rdtsc_wrap()
              R       = rho_to_a_f267_r4(dndh)
              end     = rdtsc_wrap()
              start_c = start-RDTSC_LATENCY
              end_c   = end-RDTSC_LATENCY
              tsc_elapsed = end_c-start_c 
              print*,"[Input]: dndh=",dndh 
              print*,"[Output]: R=",R 
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              print*, footer 
end subroutine unit_test_rho_to_a_f267_r4

subroutine unit_test_rho_to_a_f267_r8()
           use iso_c_binding, only : c_int, c_long_long 
           use IFPORT 
           implicit none 
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
              character(len=50),         parameter  :: header = "[TEST #10: rho_to_a_f267_r8 -- START]"
              character(len=48),         parameter  :: footer = "[TEST #10: rho_to_a_f267_r8 -- END]"
              integer(kind=c_long_long), automatic  :: start, end 
              integer(kind=c_long_long), automatic  :: start_c, end_c,tsc_elapsed 
              integer(c_int),            parameter :: SIGTRAP = 5 
              real(kind=dp),             automatic  :: dndh
              real(kind=dp),             automatic  :: R 
              print*, header 
              dndh    = -0.00000004_dp 
              R       = 0.0_dp 
#if 0
              raise(SIGTRAP)
#endif
              start   = rdtsc_wrap()
              R       = rho_to_a_f267_r8(dndh)
              end     = rdtsc_wrap()
              start_c = start-RDTSC_LATENCY
              end_c   = end-RDTSC_LATENCY
              tsc_elapsed = end_c-start_c 
              print*,"[Input]: dndh=",dndh 
              print*,"[Output]: R=",R 
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              print*, footer 
end subroutine unit_test_rho_to_a_f267_r8
 

end module mod_test_func_f267 




program main 
    use mod_test_func_f267
    implicit none 
    call unit_test_rho_to_a_f267_r4()
    call unit_test_rho_to_a_f267_r8()
end program main 