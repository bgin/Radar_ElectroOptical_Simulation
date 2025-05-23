
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

module mod_test_func_f96


       use mod_kinds,        only : i1, i4, i8, sp, dp 
       use iso_c_binding,    only : c_int, c_long_long 
       use atmos_refraction, only : analytic_sol_phase_to_geo_path_case_2_f96_r4, &
                                    analytic_sol_phase_to_geo_path_case_2_f96_r8
       implicit none 
       

#if 0
        deln0 = 3.285*10e-4
        beta  = 0.126 km
        H1    = 60km
        H2    = 310km 
        delnM = 4.04*10e-3
        beta_a= 0.0035km
        a     = 6370
#endif

#if 0
    ICC and ifort commands
    icc -c -std=c99 GMS_intrinsics_wrappers.c
    ifort -o unit_test_func_f96 -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90 GMS_atmos_refraction.f90 GMS_intrinsics_wrappers.o test_func_f96.f90
    -------------------------------------------------------------------------------------------------------------------------------------------------
    For assembly only:
    ifort -S unit_test_func_f96 -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90 GMS_atmos_refraction.f90 GMS_intrinsics_wrappers.o test_func_f96.f90
#endif


        contains 


subroutine unit_test_analytic_sol_phase_to_geo_path_case_2_f96_r4()
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
             
              real(kind=sp),dimension(1024), automatic :: delLf 
              real(kind=sp),dimension(1024), automatic :: z0_rad
              character(len=80),         parameter  :: header = "[TEST #18: analytic_sol_phase_to_geo_path_case_2_f96_r4 -- START]"
              character(len=80),         parameter  :: footer = "[TEST #18: analytic_sol_phase_to_geo_path_case_2_f96_r4 -- END]"
              real(kind=sp),             automatic  :: deln0 
              real(kind=sp),             automatic  :: z0 
              real(kind=sp),             automatic  :: beta 
              real(kind=sp),             automatic  :: Hc 
              !real(kind=sp),             automatic  :: delLf 
              real(kind=sp),             parameter  :: increment = 0.0009765625_sp
              real(kind=sp),             parameter  :: cutoff    = 1.5707963267_sp  
              integer(kind=i4),          automatic  :: i__ 
#if 0
              integer(c_int),            parameter :: SIGTRAP = 5 
#endif 
              print*,  header
              deln0 = 3.285*10.e-4_sp 
              z0    = 1.5000000000_sp 
              beta  = 0.126_sp 
              Hc    = 3.0_sp 
#if 0
              raise(SIGTRAP)
#endif   
              i__ = 1
              do 
                 z0    = z0+increment
                 if(abs(z0-cutoff)<=increment) exit  
                 delLf(i__)  = analytic_sol_phase_to_geo_path_case_2_f96_r4(deln0,z0,beta,Hc)
                 z0_rad(i__) = z0 
                 i__ = i__+1
              end do 
              print*, "[delLf]= ",delLf
              print*, "[z0_rad]=", z0_rad  
              print*, footer           
end subroutine unit_test_analytic_sol_phase_to_geo_path_case_2_f96_r4


subroutine unit_test_analytic_sol_phase_to_geo_path_case_2_f96_r8()
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
             
              real(kind=dp), dimension(1024), automatic :: delLf 
              real(kind=dp), dimension(1024), automatic :: z0_rad 
              character(len=80),         parameter  :: header = "[TEST #19: analytic_sol_phase_to_geo_path_case_2_f96_r8 -- START]"
              character(len=80),         parameter  :: footer = "[TEST #19: analytic_sol_phase_to_geo_path_case_2_f96_r8 -- END]"
              real(kind=dp),             automatic  :: deln0 
              real(kind=dp),             automatic  :: z0 
              real(kind=dp),             automatic  :: beta 
              real(kind=dp),             automatic  :: Hc 
              real(kind=sp),             parameter  :: increment = 0.0009765625_dp
              real(kind=sp),             parameter  :: cutoff    = 1.5707963267_dp  
              integer(kind=i4),          automatic  :: i__ 
#if 0
              integer(c_int),            parameter :: SIGTRAP = 5 
#endif 
              print*,  header
              deln0 = 3.285_dp*10.0e-4_dp 
              z0    = 1.50_dp 
              beta  = 0.126_dp 
              Hc    = 3.0_dp 
#if 0
              raise(SIGTRAP)
#endif   
              i__ = 1
              do 
                 z0    = z0+increment
                 if(abs(z0-cutoff)<=increment) exit  
                 delLf(i__)  = analytic_sol_phase_to_geo_path_case_2_f96_r8(deln0,z0,beta,Hc)
                 z0_rad(i__) = z0 
                 i__ = i__+1
              end do 
              print*, "[delLf]=",  delLf
              print*, "[z0_rad]", z0_rad   
              print*, footer           
end subroutine unit_test_analytic_sol_phase_to_geo_path_case_2_f96_r8



end module mod_test_func_f96 


program main 
   use mod_test_func_f96 
   call unit_test_analytic_sol_phase_to_geo_path_case_2_f96_r4()
   call unit_test_analytic_sol_phase_to_geo_path_case_2_f96_r8()
end program main 