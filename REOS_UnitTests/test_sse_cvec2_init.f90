
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

module mod_test_sse_cvec2


       use mod_kinds,                 only : i1, i4, dp 
       use iso_c_binding,             only : c_int, c_long_long 
       use mod_vectypes,              only : XMM2r8_t
       use sse_cvec2
       implicit none 

       integer(kind=c_long_long),       parameter :: RDTSC_LATENCY = 18 ! for Skylake arch.
       integer(kind=c_long_long),       parameter :: ZERO          = 0_c_long_long 
       

#if 0
    ICC and ifort commands
    icc -c -std=c99 GMS_intrinsics_wrappers.c
    ifort -o test_sse_cvec2_init -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90 GMS_vectypes.f90 GMS_constants.f90 GMS_vecconsts.f90 GMS_sse_cvec2.f90 GMS_intrinsics_wrappers.o test_sse_cvec2_init.f90
    -------------------------------------------------------------------------------------------------------------------------------------------------
    For assembly only:
    ifort -S unit_test_random_poisson -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90 GMS_vectypes.f90 GMS_constants.f90 GMS_vecconsts.f90 GMS_sse_cvec2.f90 GMS_intrinsics_wrappers.o test_sse_cvec2_init.f90
#endif

      contains 


subroutine unit_test_sse_cvec2_default_init()
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
              type(XMM2c8_t),            automatic  :: IQ1
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
end subroutine unit_test_sse_cvec2_default_init


subroutine unit_test_sse_cvec2_array_init()
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
              type(XMM2c8_t),               automatic  :: IQ1
              !dir$ attributes align : 16 :: IQ1
              real(kind=dp), dimension(0:1),automatic  :: xre 
              real(kind=dp), dimension(0:1),automatic  :: xim
              !dir$ attributes align : 16 :: xre 
              !dir$ attributes align : 16 :: xim 
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
end subroutine unit_test_sse_cvec2_array_init


subroutine unit_test_sse_cvec2_complex1_init()
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
              type(XMM2c8_t),               automatic  :: IQ1
              !dir$ attributes align : 16 :: IQ1
              complex(kind=dp),             automatic  :: xc 
              real(kind=dp),                automatic  :: xr,xi 
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
              xc       = cmplx(xr,xi,kind=dp)
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
end subroutine unit_test_sse_cvec2_complex1_init


subroutine unit_test_sse_cvec2_complex2x4_init()
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
              character(len=60),            parameter  :: header = "[TEST #4:  complex2x4_init -- START]"
              character(len=60),            parameter  :: footer = "[TEST #4:  complex2x4_init -- END]  "
              type(XMM2c8_t),               automatic  :: IQ1
              !dir$ attributes align : 16 :: IQ1
              complex(kind=dp), dimension(0:1), automatic :: xc 
              !dir$ attributes align : 16 :: xc 
              real(kind=dp),                automatic  :: xr0,xi0
              real(kind=dp),                automatic  :: xr1,xi1  
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
              xc(0)    = cmplx(xr0,xi0,kind=dp)
              xc(1)    = cmplx(xr1,xi1,kind=dp)
              start    = rdtsc_wrap()
              IQ1      = complex2x4_init(xc)
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
end subroutine unit_test_sse_cvec2_complex2x4_init


subroutine unit_test_sse_cvec2_xmm2r82x_init()
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
              character(len=60),            parameter  :: header = "[TEST #5:  xmm2r82x_init -- START]"
              character(len=60),            parameter  :: footer = "[TEST #5:  xmm2r82x_init -- END]  "
              type(XMM2c8_t),               automatic  :: IQ1
              !dir$ attributes align : 16 :: IQ1
              type(XMM2r8_t),               automatic  :: xre 
              !dir$ attributes align : 16 :: xre
              type(XMM2r8_t),               automatic  :: xim 
              !dir$ attributes align : 16 :: xim  
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
              IQ1      = xmm2r82x_init(xre,xim)
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
end subroutine unit_test_sse_cvec2_xmm2r82x_init


subroutine unit_test_sse_cvec2_xmm2r81x_init()
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
              character(len=60),            parameter  :: header = "[TEST #6:  xmm2r81x_init -- START]"
              character(len=60),            parameter  :: footer = "[TEST #6:  xmm2r81x_init -- END]  "
              type(XMM2c8_t),               automatic  :: IQ1
              !dir$ attributes align : 16 :: IQ1
              type(XMM2r8_t),               automatic  :: xre 
              !dir$ attributes align : 16 :: xre
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
              IQ1      = xmm2r81x_init(xre)
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
end subroutine unit_test_sse_cvec2_xmm2r81x_init


subroutine unit_test_sse_cvec2_r81x_init()
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
              character(len=60),            parameter  :: header = "[TEST #7:  r81x_init -- START]"
              character(len=60),            parameter  :: footer = "[TEST #7:  r81x_init -- END]  "
              type(XMM2c8_t),               automatic  :: IQ1
              !dir$ attributes align : 16 :: IQ1
              real(kind=dp),                automatic  :: xre 
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
              IQ1      = r81x_init(xre)
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
end subroutine unit_test_sse_cvec2_r81x_init


subroutine unit_test_sse_cvec2_copy_init()
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
              type(XMM2c8_t),               automatic  :: IQ1
              !dir$ attributes align : 16 :: IQ1
              type(XMM2c8_t),               automatic  :: IQ1c
              !dir$ attributes align : 16 :: IQ1c
              type(XMM2r8_t),               automatic  :: xre 
              !dir$ attributes align : 16 :: xre
              type(XMM2r8_t),               automatic  :: xim 
              !dir$ attributes align : 16 :: xim  
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
              IQ1      = xmm2r82x_init(xre,xim)
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
end subroutine unit_test_sse_cvec2_copy_init


end module mod_test_sse_cvec2

program main 
    use mod_test_sse_cvec2
    call unit_test_sse_cvec2_default_init()
    call unit_test_sse_cvec2_array_init()
    call unit_test_sse_cvec2_complex1_init()
    call unit_test_sse_cvec2_complex2x4_init()
    call unit_test_sse_cvec2_xmm2r82x_init()
    call unit_test_sse_cvec2_xmm2r81x_init()
    call unit_test_sse_cvec2_r81x_init()
    call unit_test_sse_cvec2_copy_init()
end program main 