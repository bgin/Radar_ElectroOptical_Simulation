
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

module mod_test_func_f351 


        use mod_kinds,        only : i1, i4, i8, sp, dp 
        use iso_c_binding,    only : c_int, c_long_long 
        use atmos_refraction, only : refraction_angle_n90_f351_r4, refraction_angle_n90_f351_r8
        implicit none 

        integer(kind=c_long_long),       parameter :: RDTSC_LATENCY = 18 ! for Skylake arch.
        integer(kind=c_long_long),       parameter :: ZERO          = 0_c_long_long 

#if 0
    ICC and ifort commands
    icc -c -std=c99 GMS_intrinsics_wrappers.c
    ifort -o unit_test_func_f351 -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90 GMS_atmos_refraction.f90 GMS_intrinsics_wrappers.o test_func_f351.f90
    -------------------------------------------------------------------------------------------------------------------------------------------------
    For assembly only:
    ifort -S unit_test_func_f351 -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90 GMS_atmos_refraction.f90 GMS_intrinsics_wrappers.o test_func_f351.f90
#endif


        contains 


subroutine unit_test_refraction_angle_n90_f351_r4()
           use iso_c_binding, only : c_int, c_long_long 
           use IFPORT 
           use , intrinsic           :: IEEE_ARITHMETIC
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
              character(len=50),         parameter  :: header = "[TEST #13: refraction_angle_n90_f351_r4 -- START]"
              character(len=48),         parameter  :: footer = "[TEST #13: refraction_angle_n90_f351_r4 -- END]"
              integer(kind=c_long_long), automatic  :: start, end 
              integer(kind=c_long_long), automatic  :: start_c, end_c,tsc_elapsed
              real(kind=sp),             automatic  :: dn0 
              real(kind=sp),             automatic  :: beta 
              real(kind=sp),             automatic  :: z0 
              real(kind=sp),             automatic  :: alpha 
#if 0
              integer(c_int),            parameter :: SIGTRAP = 5 
#endif 
              print*,  header
              alpha = IEEE_VALUE(1.0_sp,IEEE_QUIET_NAN) 
              dn0   = 0.000240_sp 
              beta  = 0.10_sp 
              z0    = 1.5533_sp !89 (deg)
#if 0
              raise(SIGTRAP)
#endif
              start   = rdtsc_wrap()
              alpha   = refraction_angle_n90_f351_r4(dn0,beta,z0)
              end     = rdtsc_wrap()
              start_c = start-RDTSC_LATENCY
              end_c   = end-RDTSC_LATENCY
              tsc_elapsed  = end_c-start_c
              print*,"[Input]: dn0=",dn0,"beta=",beta,"z0=",z0 
              print*,"[Output]: alpha=",alpha 
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              print*, footer 
end subroutine unit_test_refraction_angle_n90_f351_r4 

subroutine unit_test_refraction_angle_n90_f351_r8()
           use iso_c_binding, only : c_int, c_long_long 
           use IFPORT 
           use , intrinsic        :: IEEE_ARITHMETIC
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
              character(len=50),         parameter  :: header = "[TEST #14: refraction_angle_n90_f351_r8 -- START]"
              character(len=48),         parameter  :: footer = "[TEST #14: refraction_angle_n90_f351_r8 -- END]"
              integer(kind=c_long_long), automatic  :: start, end 
              integer(kind=c_long_long), automatic  :: start_c, end_c,tsc_elapsed
              real(kind=dp),             automatic  :: dn0 
              real(kind=dp),             automatic  :: beta 
              real(kind=dp),             automatic  :: z0 
              real(kind=dp),             automatic  :: alpha 
#if 0
              integer(c_int),            parameter :: SIGTRAP = 5 
#endif 
              print*,  header
              alpha = IEEE_VALUE(1.0_dp,IEEE_QUIET_NAN) 
              dn0   = 0.000240_dp 
              beta  = 0.10_dp 
              z0    = 1.5533_dp !89 (deg)
#if 0
              raise(SIGTRAP)
#endif
              start   = rdtsc_wrap()
              alpha   = refraction_angle_n90_f351_r8(dn0,beta,z0)
              end     = rdtsc_wrap()
              start_c = start-RDTSC_LATENCY
              end_c   = end-RDTSC_LATENCY
              tsc_elapsed  = end_c-start_c
              print*,"[Input]: dn0=",dn0,"beta=",beta,"z0=",z0 
              print*,"[Output]: alpha=",alpha 
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              print*, footer 
end subroutine unit_test_refraction_angle_n90_f351_r8 



end module mod_test_func_f351 



program main 
   use mod_test_func_f351 

   call unit_test_refraction_angle_n90_f351_r4()
   call unit_test_refraction_angle_n90_f351_r8()
end program main 