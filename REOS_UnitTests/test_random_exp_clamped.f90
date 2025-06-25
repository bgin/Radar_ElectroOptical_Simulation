
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

module mod_test_random_exp_clamped

       use mod_kinds,                 only : i1, i4, i8, sp, dp 
       use iso_c_binding,             only : c_int, c_long_long 
       use rand_scalar_distributions, only : random_exponential_clamped 

       implicit none 

        integer(kind=c_long_long),       parameter :: RDTSC_LATENCY = 18 ! for Skylake arch.
        integer(kind=c_long_long),       parameter :: ZERO          = 0_c_long_long 
        integer(kind=i4),                parameter :: buf_len       = 5399_i4 

#if 0
    ICC and ifort commands
    icc -c -std=c99 GMS_intrinsics_wrappers.c
    ifort -o unit_test_random_exp_clamped -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90 GMS_rand_distributions.f90 GMS_intrinsics_wrappers.o test_random_exp_clamped.f90
    -------------------------------------------------------------------------------------------------------------------------------------------------
    For assembly only:
    ifort -S unit_test_random_exp_clamped -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90 GMS_rand_distributions.f90 GMS_intrinsics_wrappers.o test_random_exp_clamped.f90
#endif


        contains 

subroutine unit_test_random_exp_clamped()
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
              character(len=80),         parameter  :: header = "[TEST #1: random_exponential_clamped -- START]"
              character(len=80),         parameter  :: footer = "[TEST #1: random_exponential_clamped -- END]"
              integer(kind=c_long_long), automatic  :: start, end 
              integer(kind=c_long_long), automatic  :: start_c, end_c,tsc_elapsed
              real(kind=sp), automatic :: rnum 
#if 0
              integer(c_int),            parameter :: SIGTRAP = 5 
#endif 
              print*,  header
              rnum        = IEEE_VALUE(1.0_sp,IEEE_QUIET_NAN)
              start       = rdtsc_wrap()
              rnum        = random_exponential_clamped()
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
end subroutine unit_test_random_exp_clamped

subroutine unit_test_random_exp_clamped_looped()
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
              character(len=80),         parameter  :: header = "[TEST #2: random_exponential_clamped, (buffer-fill) -- START]"
              character(len=80),         parameter  :: footer = "[TEST #2: random_exponential_clamped, (buffer-fill) -- END]  "
              
              character(len=80),         parameter  :: OUTFILE  = "unit_test_random_exp_clamped_looped_output.dat" 
              integer(kind=i4),          parameter  :: IOUNIT = 102
              real(kind=sp), dimension(0:buf_len), automatic :: rand_buf 
              !dir$ attributes align : 64 :: rand_buf 
              character(len=256),        automatic  :: emsg 
              integer(kind=c_long_long), automatic  :: start, end 
              integer(kind=c_long_long), automatic  :: start_c, end_c,tsc_elapsed
              integer(kind=i4),          automatic  :: ioerr 
              integer(kind=i4),          automatic  :: i__ 
              logical(kind=i1),          automatic  :: ioflag 
              
#if 0
              integer(c_int),            parameter :: SIGTRAP = 5 
              integer(c_int),            automatic :: ret_val
#endif 
#if 0
              ret_val = raise(SIGTRAP)
              if(ret_val/=0_c_int) print*, "raise returned=",ret_val
#endif
              print*, header 
              start       = rdtsc_wrap()
              do i__ = 0, buf_len, 8 
                 rand_buf(i__+0) = random_exponential_clamped()
                 rand_buf(i__+1) = random_exponential_clamped()
                 rand_buf(i__+2) = random_exponential_clamped()
                 rand_buf(i__+3) = random_exponential_clamped()
                 rand_buf(i__+4) = random_exponential_clamped()
                 rand_buf(i__+5) = random_exponential_clamped()
                 rand_buf(i__+6) = random_exponential_clamped()
                 rand_buf(i__+7) = random_exponential_clamped()
              end do 
              end         = rdtsc_wrap()
              start_c     = start-RDTSC_LATENCY
              end_c       = end-RDTSC_LATENCY
              tsc_elapsed = end_c-start_c 
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
#if 1
              ioerr = 0
              open(UNIT=IOUNIT,FILE=OUTFILE,IOMSG=emsg,ACCESS="SEQUENTIAL",STATUS="NEW")
              ioflag = (ioerr==0)
              if(.not.ioflag) then 
                 print*, "[ERROR] -- OPEN failed an error message is as follows:"
                 print*, emsg 
                 return 
              else 
                 write(IOUNIT,'(A80)') "[OUTPUT-START]: random_exponential_clamped (buffer-fill)."
                 do i__=0, buf_len 
                    write(IOUNIT,'(A6,I5,F22.15)') "Index:", i__,rand_buf(i__)
                 end do 
                 write(IOUNIT,'(A80)') "[OUTPUT-END]: random_exponential_clamped (buffer-fill)."
              end if  
              close(IOUNIT,STATUS='KEEP')
#endif
              print*, footer
end subroutine unit_test_random_exp_clamped_looped


end module mod_test_random_exp_clamped


program main 
   use mod_test_random_exp_clamped
   call unit_test_random_exp_clamped()
   call unit_test_random_exp_clamped_looped()
end program main 