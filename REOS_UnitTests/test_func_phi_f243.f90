
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

module mod_test_func_phi_f243

        use mod_kinds, only : i1, i4, i8, sp, dp 
        use iso_c_binding, only : c_int, c_long_long 
        use atmos_refraction, only : n_refract_phi_f243_r4, n_refract_phi_f243_r8 

        character(len=64),               parameter :: row1          =  "20,0,101.325,633,1.000271800,1.000271799,0.1"
        integer(kind=c_long_long),       parameter :: RDTSC_LATENCY = 18 ! for Skylake arch.
        integer(kind=c_long_long),       parameter :: ZERO          = 0_c_long_long 
#if 0
    ICC and ifort commands
    icc -c -std=c99 GMS_intrinsics_wrappers.c
    ifort -o unit_test_func_phi_f243 -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 
     -fopenmp -qopenmp -fpp -falign-functions=32 GMS_kinds.f90 GMS_atmos_refraction.f90 GMS_intrinsics_wrappers.o test_func_phi_f243.f90
#endif
        
    contains 

subroutine unit_test_n_refract_phi_f243_r4()
             use iso_c_binding, only : c_int, c_long_long 
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
              integer(kind=c_long_long), automatic :: start, end 
              integer(kind=c_long_long), automatic :: start_c, end_c,tsc_cyc 
              real(kind=sp),      automatic :: n 
              real(kind=sp),      automatic :: n0 
              real(kind=sp),      automatic :: z 
              real(kind=sp),      automatic :: z0 
              real(kind=sp),      automatic :: r 
              real(kind=sp),      automatic :: R0  
              real(kind=sp),      automatic :: phi 
              real(kind=sp),      automatic :: phi0 
              real(kind=sp),      automatic :: n_over_tht 
#if 0
              integer(c_int),     parameter :: SIGTRAP = 5 
#endif
              character(len=50),  parameter :: header = "[TEST #3: n_refract_phi_f243_r4 -- START]"
              character(len=48),  parameter :: footer = "[TEST #3: n_refract_phi_f243_r4 -- END]"
              ! Exec code ...
                          
              print*, header
              n           = 1.000271800_sp 
              n0          = 1.000282756_sp 
              z           = 0.5_sp 
              z0          = 0.42_sp 
              r           = 25000.0_sp
              R0          = 15000.0_sp
              phi         = 0.47_sp 
              phi0        = 0.25_sp 
              start       = rdtsc_wrap()
              start_c     = start-RDTSC_LATENCY
              n_over_tht  = n_refract_phi_f243_r4(n,n0,z,z0,r,R0,phi,phi0)
              end         = rdtsc_wrap()
              end_c       = end-RDTSC_LATENCY
              tsc_cyc     = end_c-start_c 
              print*,"[Input]:  n=",n,"n0=",n0,"z=",z,"z0=",z0,"r=",r,"R0=",R0,"phi=",phi,"phi0=",phi0  
              print*,"[Output]: n_over_tht=",n_over_tht 
              print*, "[WARNING]: Crude timing measurement!!"
              if(tsc_cyc<=ZERO) then
                 print*, "[INVALID-TSC]=", tsc_cyc
              else
                 print*, "[TSC]=", tsc_cyc 
              end if 
              print*, footer
end subroutine unit_test_n_refract_phi_f243_r4

subroutine unit_test_n_refract_phi_f243_r8()
             use iso_c_binding, only : c_int, c_long_long 
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
              integer(kind=c_long_long), automatic :: start, end 
              integer(kind=c_long_long), automatic :: start_c, end_c,tsc_cyc 
              real(kind=dp),      automatic :: n 
              real(kind=dp),      automatic :: n0 
              real(kind=dp),      automatic :: z 
              real(kind=dp),      automatic :: z0 
              real(kind=dp),      automatic :: r 
              real(kind=dp),      automatic :: R0  
              real(kind=dp),      automatic :: phi 
              real(kind=dp),      automatic :: phi0 
              real(kind=dp),      automatic :: n_over_tht 
#if 0
              integer(c_int),     parameter :: SIGTRAP = 5 
#endif
              character(len=50),  parameter :: header = "[TEST #4: n_refract_phi_f243_r8 -- START]"
              character(len=48),  parameter :: footer = "[TEST #4: n_refract_phi_f243_r8 -- END]"
              ! Exec code ...
                          
              print*, header
              n           = 1.000271800_dp 
              n0          = 1.000282756_dp 
              z           = 0.5_dp 
              z0          = 0.42_dp 
              r           = 25000.0_dp
              R0          = 15000.0_dp
              phi         = 0.47_dp 
              phi0        = 0.25_dp 
              start       = rdtsc_wrap()
              start_c     = start-RDTSC_LATENCY
              n_over_tht  = n_refract_phi_f243_r8(n,n0,z,z0,r,R0,phi,phi0)
              end         = rdtsc_wrap()
              end_c       = end-RDTSC_LATENCY
              tsc_cyc     = end_c-start_c 
              print*,"[Input]:  n=",n,"n0=",n0,"z=",z,"z0=",z0,"r=",r,"R0=",R0,"phi=",phi,"phi0=",phi0  
              print*,"[Output]: n_over_tht=",n_over_tht 
              print*, "[WARNING]: Crude timing measurement!!"
              if(tsc_cyc<=ZERO) then
                 print*, "[INVALID-TSC]=", tsc_cyc
              else
                 print*, "[TSC]=", tsc_cyc 
              end if 
              print*, footer
end subroutine unit_test_n_refract_phi_f243_r8



end module mod_test_func_phi_f243


program main 
   use mod_test_func_phi_f243
   write(*,*), "Using precomputed n-Cidor values."
   write(*,'(A60)'), row1 
   call unit_test_n_refract_phi_f243_r4()
   call unit_test_n_refract_phi_f243_r8()
end program main 