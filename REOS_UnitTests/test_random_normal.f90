
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

module mod_test_random_normal

       use mod_kinds,                 only : i1, i4, i8, sp, dp 
       use iso_c_binding,             only : c_int, c_long_long 
       use rand_scalar_distributions, only : random_normal, random_normal_clamped

       implicit none 

        integer(kind=c_long_long),       parameter :: RDTSC_LATENCY = 18 ! for Skylake arch.
        integer(kind=c_long_long),       parameter :: ZERO          = 0_c_long_long 
        integer(kind=i4),                parameter :: buf_len       = 5399_i4 

#if 0
    ICC and ifort commands
    icc -c -std=c99 GMS_intrinsics_wrappers.c
    ifort -o unit_test_random_normal -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90 GMS_rand_distributions.f90 GMS_intrinsics_wrappers.o test_random_normal.f90
    -------------------------------------------------------------------------------------------------------------------------------------------------
    For assembly only:
    ifort -S unit_test_random_normal -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90 GMS_rand_distributions.f90 GMS_intrinsics_wrappers.o test_random_normal.f90
#endif


        contains 

subroutine unit_test_random_normal()
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
              character(len=60),         parameter  :: header = "[TEST #1: random_normal -- START]"
              character(len=60),         parameter  :: footer = "[TEST #1: random_normal -- END]"
              integer(kind=c_long_long), automatic  :: start, end 
              integer(kind=c_long_long), automatic  :: start_c, end_c,tsc_elapsed
              real(kind=sp), automatic :: rnum 
#if 0
              integer(c_int),            parameter :: SIGTRAP = 5 
#endif 
              print*,  header
              rnum        = IEEE_VALUE(1.0_sp,IEEE_QUIET_NAN)
              start       = rdtsc_wrap()
              rnum        = random_normal()
              end         = rdtsc_wrap()
              start_c     = start-RDTSC_LATENCY
              end_c       = end-RDTSC_LATENCY
              tsc_elapsed = end_c-start_c 
              print*,"[INPUT]: -- None"
              print*,"[OUTPUT]: rnum=", rnum 
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              print*, footer 
end subroutine unit_test_random_normal

subroutine unit_test_random_normal_looped()
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
              character(len=60),         parameter  :: header = "[TEST #2: random_normal, (buffer-fill) -- START]"
              character(len=60),         parameter  :: footer = "[TEST #2: random_normal, (buffer-fill) -- END]  "
              
              character(len=40),         parameter  :: OUTFILE  = "unit_test_random_normal_looped_output.dat" 
              real(kind=sp), dimension(0:buf_len), automatic :: rand_buf 
              !dir$ attributes align : 64 :: rand_buf 
              character(len=256),        automatic  :: emsg 
              integer(kind=c_long_long), automatic  :: start, end 
              integer(kind=c_long_long), automatic  :: start_c, end_c,tsc_elapsed
              integer(kind=i4),          automatic  :: ioerr 
              integer(kind=i4),          automatic  :: i__ 
              integer(kind=i4),          parameter  :: IOUNIT = 102
#if 0
              integer(c_int),            parameter :: SIGTRAP = 5 
              integer(c_int),            automatic :: ret_val
#endif 
#if 0
              ret_val = raise(SIGTRAP)
              if(ret_val/=0_c_int) print*, "raise returned=",ret_val
#endif
              print*, header 
              
end subroutine unit_test_random_normal_looped


end module mod_test_random_normal 


program main 
   use mod_test_random_normal
   call unit_test_random_normal()
end program main 