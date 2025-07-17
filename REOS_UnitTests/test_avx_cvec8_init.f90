
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

module mod_test_avx_cvec8


       use mod_kinds,                 only : i1, i4, sp 
       use iso_c_binding,             only : c_int, c_long_long 
       use mod_vectypes,              only : YMM8r4_t
       use avx_cvec8
       implicit none 

       integer(kind=c_long_long),       parameter :: RDTSC_LATENCY = 18 ! for Skylake arch.
       integer(kind=c_long_long),       parameter :: ZERO          = 0_c_long_long 
       

#if 0
    ICC and ifort commands
    icc -c -std=c99 GMS_intrinsics_wrappers.c
    ! CRASH (ICE) of ifort!!
    ifort -o test_avx_cvec8_init -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90 GMS_vectypes.f90 GMS_constants.f90 GMS_vecconsts.f90 GMS_avx_cvec8.f90 GMS_intrinsics_wrappers.o test_avx_cvec8_init.f90
    -------------------------------------------------------------------------------------------------------------------------------------------------
    ifx -o test_avx_cvec8_init -fp-model fast=2 -ftz -O3   -march=skylake-avx512 \
    -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90 GMS_vectypes.f90 GMS_constants.f90 GMS_vecconsts.f90 GMS_avx_cvec8.f90 GMS_intrinsics_wrappers.o test_avx_cvec8_init.f90
    For assembly only:
    ifort -S test_avx_cvec8_init -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90 GMS_vectypes.f90 GMS_constants.f90 GMS_vecconsts.f90 GMS_avx_cvec8.f90 GMS_intrinsics_wrappers.o test_avx_cvec8_init.f90
#endif

      contains 


subroutine unit_test_avx_cvec8_default_init()
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
              character(len=60),         parameter  :: header = "[TEST #1:  default_init -- START]"
              character(len=60),         parameter  :: footer = "[TEST #1:  default_init -- END]  "
              type(YMM8c4),              automatic  :: IQ1
              integer(kind=i4),          automatic  :: start,end 
              integer(kind=i4),          automatic  :: start_c,end_c 
              integer(kind=i4),          automatic  ::  tsc_elapsed 
#if 0
              integer(c_int),            parameter :: SIGTRAP = 5 
              integer(c_int),            automatic :: ret_val
#endif 
#if 0
              ret_val = raise(SIGTRAP)
              if(ret_val/=0_c_int) print*, "raise returned=",ret_val
#endif
              print*, header 
              start    = rdtsc_wrap()
              IQ1      = default_init()
              end      = rdtsc_wrap()
              start_c  = start-RDTSC_LATENCY
              end_c    = end-RDTSC_LATENCY
              tsc_elapsed = end_c-start_c 
              print*,"[OUTPUT:] re:",IQ1.re, "real-components."
              print*,"[OUTPUT:] im:",IQ1.im, "imag-components."
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              print*, footer
end subroutine unit_test_avx_cvec8_default_init


subroutine unit_test_avx_cvec8_array_init()
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
              character(len=60),            parameter  :: header = "[TEST #2:  array_init -- START]"
              character(len=60),            parameter  :: footer = "[TEST #2:  array_init -- END]  "
              type(YMM8c4),                 automatic  :: IQ1
              !dir$ attributes align : 32 :: IQ1
              real(kind=sp), dimension(0:7),automatic  :: xre 
              real(kind=sp), dimension(0:7),automatic  :: xim
              !dir$ attributes align : 32 :: xre 
              !dir$ attributes align : 32 :: xim 
              integer(kind=i4),             automatic  :: start,end 
              integer(kind=i4),             automatic  :: start_c,end_c 
              integer(kind=i4),             automatic  :: tsc_elapsed 
#if 0
              integer(c_int),            parameter :: SIGTRAP = 5 
              integer(c_int),            automatic :: ret_val
#endif 
#if 0
              ret_val = raise(SIGTRAP)
              if(ret_val/=0_c_int) print*, "raise returned=",ret_val
#endif
              print*, header 
              call random_init(.false.,.false.)
              call random_number(xre)
              call random_number(xim)
              start    = rdtsc_wrap()
              IQ1      = array_init(xre,xim)
              end      = rdtsc_wrap()
              start_c  = start-RDTSC_LATENCY
              end_c    = end-RDTSC_LATENCY
              tsc_elapsed = end_c-start_c 
              print*,"[OUTPUT:] re:",IQ1.re, "real-components."
              print*,"[OUTPUT:] im:",IQ1.im, "imag-components."
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              print*, footer
end subroutine unit_test_avx_cvec8_array_init


subroutine unit_test_avx_cvec8_complex1_init()
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
              character(len=60),            parameter  :: header = "[TEST #3:  complex1_init -- START]"
              character(len=60),            parameter  :: footer = "[TEST #3:  complex1_init -- END]  "
              type(YMM8c4),                 automatic  :: IQ1
              !dir$ attributes align : 32 :: IQ1
              complex(kind=sp),             automatic  :: xc 
              real(kind=sp),                automatic  :: xr,xi 
              integer(kind=i4),             automatic  :: start,end 
              integer(kind=i4),             automatic  :: start_c,end_c 
              integer(kind=i4),             automatic  :: tsc_elapsed 
#if 0
              integer(c_int),            parameter :: SIGTRAP = 5 
              integer(c_int),            automatic :: ret_val
#endif 
#if 0
              ret_val = raise(SIGTRAP)
              if(ret_val/=0_c_int) print*, "raise returned=",ret_val
#endif
              print*, header 
              call random_init(.false.,.false.)
              call random_number(xr)
              call random_number(xi)
              xc       = cmplx(xr,xi,kind=sp)
              start    = rdtsc_wrap()
              IQ1      = complex1_init(xc)
              end      = rdtsc_wrap()
              start_c  = start-RDTSC_LATENCY
              end_c    = end-RDTSC_LATENCY
              tsc_elapsed = end_c-start_c 
              print*,"[OUTPUT:] re:",IQ1.re, "real-components."
              print*,"[OUTPUT:] im:",IQ1.im, "imag-components."
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              print*, footer
end subroutine unit_test_avx_cvec8_complex1_init


subroutine unit_test_avx_cvec8_complex2x8_init()
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
              character(len=60),            parameter  :: header = "[TEST #4:  complex2x8_init -- START]"
              character(len=60),            parameter  :: footer = "[TEST #4:  complex2x8_init -- END]  "
              type(YMM8c4),                 automatic  :: IQ1
              !dir$ attributes align : 32:: IQ1
              complex(kind=sp), dimension(0:7), automatic :: xc 
              !dir$ attributes align : 32 :: xc 
              real(kind=sp),                automatic  :: xr0,xi0
              real(kind=sp),                automatic  :: xr1,xi1 
              real(kind=sp),                automatic  :: xr2,xi2 
              real(kind=sp),                automatic  :: xr3,xi3  
              real(kind=sp),                automatic  :: xr4,xi4
              real(kind=sp),                automatic  :: xr5,xi5 
              real(kind=sp),                automatic  :: xr6,xi6 
              real(kind=sp),                automatic  :: xr7,xi7  
              integer(kind=i4),             automatic  :: start,end 
              integer(kind=i4),             automatic  :: start_c,end_c 
              integer(kind=i4),             automatic  :: tsc_elapsed 
#if 0
              integer(c_int),            parameter :: SIGTRAP = 5 
              integer(c_int),            automatic :: ret_val
#endif 
#if 0
              ret_val = raise(SIGTRAP)
              if(ret_val/=0_c_int) print*, "raise returned=",ret_val
#endif
              print*, header 
              call random_init(.false.,.false.)
              call random_number(xr0)
              call random_number(xi0)
              call random_number(xr1)
              call random_number(xi1)
              xc(0)    = cmplx(xr0,xi0,kind=sp)
              xc(1)    = cmplx(xr1,xi1,kind=sp)
              call random_number(xr2)
              call random_number(xi2)
              call random_number(xr3)
              call random_number(xi3)
              xc(2)    = cmplx(xr2,xi2,kind=sp)
              xc(3)    = cmplx(xr3,xi3,kind=sp)
              call random_number(xr4)
              call random_number(xi4)
              call random_number(xr5)
              call random_number(xi5)
              xc(4)    = cmplx(xr4,xi4,kind=sp)
              xc(5)    = cmplx(xr5,xi5,kind=sp)
              call random_number(xr6)
              call random_number(xi6)
              call random_number(xr7)
              call random_number(xi7)
              xc(6)    = cmplx(xr6,xi6,kind=sp)
              xc(7)    = cmplx(xr7,xi7,kind=sp)
              start    = rdtsc_wrap()
              IQ1      = complex2x8_init(xc)
              end      = rdtsc_wrap()
              start_c  = start-RDTSC_LATENCY
              end_c    = end-RDTSC_LATENCY
              tsc_elapsed = end_c-start_c 
              print*,"[OUTPUT:] re:",IQ1.re, "real-components."
              print*,"[OUTPUT:] im:",IQ1.im, "imag-components."
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              print*, footer
end subroutine unit_test_avx_cvec8_complex2x8_init


subroutine unit_test_avx_cvec8_ymm8r42x_init()
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
              character(len=60),            parameter  :: header = "[TEST #5:  ymm8r42x_init -- START]"
              character(len=60),            parameter  :: footer = "[TEST #5:  ymm8r42x_init -- END]  "
              type(YMM8c4),               automatic  :: IQ1
              !dir$ attributes align : 32 :: IQ1
              type(YMM8r4_t),               automatic  :: xre 
              !dir$ attributes align : 32 :: xre
              type(YMM8r4_t),               automatic  :: xim 
              !dir$ attributes align : 32 :: xim  
              integer(kind=i4),             automatic  :: start,end 
              integer(kind=i4),             automatic  :: start_c,end_c 
              integer(kind=i4),             automatic  :: tsc_elapsed 
#if 0
              integer(c_int),            parameter :: SIGTRAP = 5 
              integer(c_int),            automatic :: ret_val
#endif 
#if 0
              ret_val = raise(SIGTRAP)
              if(ret_val/=0_c_int) print*, "raise returned=",ret_val
#endif
              print*, header 
              call random_init(.false.,.false.)
              call random_number(xre.v)
              call random_number(xim.v)
              start    = rdtsc_wrap()
              IQ1      = ymm8r42x_init(xre,xim)
              end      = rdtsc_wrap()
              start_c  = start-RDTSC_LATENCY
              end_c    = end-RDTSC_LATENCY
              tsc_elapsed = end_c-start_c 
              print*,"[OUTPUT:] re:",IQ1.re, "real-components."
              print*,"[OUTPUT:] im:",IQ1.im, "imag-components."
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              print*, footer
end subroutine unit_test_avx_cvec8_ymm8r42x_init


subroutine unit_test_avx_cvec8_ymm8r41x_init()
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
              character(len=60),            parameter  :: header = "[TEST #6:  ymm8r41x_init -- START]"
              character(len=60),            parameter  :: footer = "[TEST #6:  ymm8r41x_init -- END]  "
              type(YMM8c4),               automatic  :: IQ1
              !dir$ attributes align : 32 :: IQ1
              type(YMM8r4_t),               automatic  :: xre 
              !dir$ attributes align : 32 :: xre
              integer(kind=i4),             automatic  :: start,end 
              integer(kind=i4),             automatic  :: start_c,end_c 
              integer(kind=i4),             automatic  :: tsc_elapsed 
#if 0
              integer(c_int),            parameter :: SIGTRAP = 5 
              integer(c_int),            automatic :: ret_val
#endif 
#if 0
              ret_val = raise(SIGTRAP)
              if(ret_val/=0_c_int) print*, "raise returned=",ret_val
#endif
              print*, header 
              call random_init(.false.,.false.)
              call random_number(xre.v)
              start    = rdtsc_wrap()
              IQ1      = ymm8r41x_init(xre)
              end      = rdtsc_wrap()
              start_c  = start-RDTSC_LATENCY
              end_c    = end-RDTSC_LATENCY
              tsc_elapsed = end_c-start_c 
              print*,"[OUTPUT:] re:",IQ1.re, "real-components."
              print*,"[OUTPUT:] im:",IQ1.im, "imag-components."
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              print*, footer
end subroutine unit_test_avx_cvec8_ymm8r41x_init


subroutine unit_test_avx_cvec8_r41x_init()
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
              character(len=60),            parameter  :: header = "[TEST #7:  r41x_init -- START]"
              character(len=60),            parameter  :: footer = "[TEST #7:  r41x_init -- END]  "
              type(YMM8c4),               automatic  :: IQ1
              !dir$ attributes align : 32 :: IQ1
              real(kind=sp),                automatic  :: xre 
              integer(kind=i4),             automatic  :: start,end 
              integer(kind=i4),             automatic  :: start_c,end_c 
              integer(kind=i4),             automatic  :: tsc_elapsed 
#if 0
              integer(c_int),            parameter :: SIGTRAP = 5 
              integer(c_int),            automatic :: ret_val
#endif 
#if 0
              ret_val = raise(SIGTRAP)
              if(ret_val/=0_c_int) print*, "raise returned=",ret_val
#endif
              print*, header 
              call random_init(.false.,.false.)
              call random_number(xre)
              start    = rdtsc_wrap()
              IQ1      = r41x_init(xre)
              end      = rdtsc_wrap()
              start_c  = start-RDTSC_LATENCY
              end_c    = end-RDTSC_LATENCY
              tsc_elapsed = end_c-start_c 
              print*,"[OUTPUT:] re:",IQ1.re, "real-components."
              print*,"[OUTPUT:] im:",IQ1.im, "imag-components."
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              print*, footer
end subroutine unit_test_avx_cvec8_r41x_init


subroutine unit_test_avx_cvec8_copy_init()
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
              character(len=60),            parameter  :: header = "[TEST #8:  copy_init -- START]"
              character(len=60),            parameter  :: footer = "[TEST #8:  copy_init -- END]  "
              type(YMM8c4),               automatic  :: IQ1
              !dir$ attributes align : 32 :: IQ1
              type(YMM8c4),               automatic  :: IQ1c
              !dir$ attributes align : 32 :: IQ1c
              type(YMM8r4_t),               automatic  :: xre 
              !dir$ attributes align : 32 :: xre
              type(YMM8r4_t),               automatic  :: xim 
              !dir$ attributes align : 32 :: xim  
              integer(kind=i4),             automatic  :: start,end 
              integer(kind=i4),             automatic  :: start_c,end_c 
              integer(kind=i4),             automatic  :: tsc_elapsed 
#if 0
              integer(c_int),            parameter :: SIGTRAP = 5 
              integer(c_int),            automatic :: ret_val
#endif 
#if 0
              ret_val = raise(SIGTRAP)
              if(ret_val/=0_c_int) print*, "raise returned=",ret_val
#endif
              print*, header 
              call random_init(.false.,.false.)
              call random_number(xre.v)
              call random_number(xim.v)
              IQ1      = ymm8r42x_init(xre,xim)
              start    = rdtsc_wrap()
              IQ1c     = copy_init(IQ1)
              end      = rdtsc_wrap()
              start_c  = start-RDTSC_LATENCY
              end_c    = end-RDTSC_LATENCY
              tsc_elapsed = end_c-start_c 
              print*,"[INPUT:]  re:",IQ1.re,  "real-components."
              print*,"[INPUT:]  im:",IQ1.im,  "imag-components."
              print*,"[OUTPUT:] re:",IQ1c.re, "real-components."
              print*,"[OUTPUT:] im:",IQ1c.im, "imag-components."
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              print*, footer
end subroutine unit_test_avx_cvec8_copy_init


end module mod_test_avx_cvec8

program main 
    use mod_test_avx_cvec8
    call unit_test_avx_cvec8_default_init()
    call unit_test_avx_cvec8_array_init()
    call unit_test_avx_cvec8_complex1_init()
    call unit_test_avx_cvec8_complex2x8_init()
    call unit_test_avx_cvec8_ymm8r42x_init()
    call unit_test_avx_cvec8_ymm8r41x_init()
    call unit_test_avx_cvec8_r41x_init()
    call unit_test_avx_cvec8_copy_init()
end program main 