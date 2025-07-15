
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

module mod_test_eos_sensor_types 


       use mod_kinds,         only : i1, i4, i8, sp, dp 
       use iso_c_binding,     only : c_int, c_long_long 
       use eos_sensor_types 
       implicit none 

       integer(kind=c_long_long),       parameter :: RDTSC_LATENCY = 18 ! for Skylake arch.
       integer(kind=c_long_long),       parameter :: ZERO          = 0_c_long_long 


#if 0
    ICC and ifort commands
    icc -c -std=c99 GMS_intrinsics_wrappers.c
    ifort -o test_eos_sensor_types -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90 GMS_eos_sensor_types.f90 GMS_intrinsics_wrappers.o test_eos_sensor_types.f90
    -------------------------------------------------------------------------------------------------------------------------------------------------
    ifx -o test_sse_cvec2_add -fp-model fast=2 -ftz -O3  -march=skylake-avx512      -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=3 \ 
    GMS_kinds.f90 GMS_eos_sensor_types.f90 GMS_intrinsics_wrappers.o est_eos_sensor_types.f90

    For assembly only:
    ifort -S  test_sse_cvec2_add -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90 GMS_eos_sensor_types.f90 GMS_intrinsics_wrappers.o test_eos_sensor_types.f90
#endif


      contains 

subroutine unit_test_param_gamma_r4_t 
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
              character(len=80),                  parameter  :: header = "[TEST #1:  alloc/dealloc: param_gamma_r4_t -- START]"
              character(len=80),                  parameter  :: footer = "[TEST #1:  alloc/dealloc: param_gamma_r4_t -- END]" 
              type(param_gamma_r4_t),             automatic  :: m_param_gamma  
              integer(kind=c_long_long),          automatic  :: start,end 
              integer(kind=c_long_long),          automatic  :: start_c,end_c 
              integer(kind=c_long_long),          automatic  :: tsc_elapsed 
              integer(kind=i4),                   automatic  :: n
              
#if 0
              integer(c_int),            parameter :: SIGTRAP = 5 
              integer(c_int),            automatic :: ret_val
#endif 

              print*, header
#if 0
              ret_val = raise(SIGTRAP)
              if(ret_val/=0_c_int) print*, "raise returned=",ret_val
#endif

              n = set_random_size()
              start = rdtsc_wrap()
              allocate(m_param_gamma.phi(n))
              allocate(m_param_gamma.gamma(n))
              end   = rdtsc_wrap()
              call check_allocation_stats(m_param_gamma.phi)
              call check_allocation_stats(m_param_gamma.gamma)
              start_c     = start-RDTSC_LATENCY
              end_c       = end-RDTSC_LATENCY
              tsc_elapsed = end_c-start_c
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              if(allocated(m_param_gamma.phi)) deallocate(m_param_gamma.phi)
              if(allocated(m_param_gamma.gamma)) deallocate(m_param_gamma.gamma)
              call check_allocation_stats(m_param_gamma.phi)
              call check_allocation_stats(m_param_gamma.gamma)
              print*, footer 
              contains 

              function set_random_size() result(rval)
                       implicit none 
                       integer(kind=i4), automatic :: rnum 
                       integer(kind=i4), parameter :: lo = 512
                       integer(kind=i4)            :: rval 
                       rnum = irand()
                       if(rnum<lo) then 
                          rnum=lo
                          rval=rnum
                       end if 
                       rval=rnum 
               end function set_random_size

              subroutine check_allocation_stats(array)
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
              end subroutine check_allocation_stats

end subroutine unit_test_param_gamma_r4_t

subroutine unit_test_param_gamma_r8_t 
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
              character(len=80),                  parameter  :: header = "[TEST #2:  alloc/dealloc: param_gamma_r8_t -- START]"
              character(len=80),                  parameter  :: footer = "[TEST #2:  alloc/dealloc: param_gamma_r8_t -- END]" 
              type(param_gamma_r8_t),             automatic  :: m_param_gamma  
              integer(kind=c_long_long),          automatic  :: start,end 
              integer(kind=c_long_long),          automatic  :: start_c,end_c 
              integer(kind=c_long_long),          automatic  :: tsc_elapsed 
              integer(kind=i4),                   automatic  :: n
              
#if 0
              integer(c_int),            parameter :: SIGTRAP = 5 
              integer(c_int),            automatic :: ret_val
#endif 

              print*, header
#if 0
              ret_val = raise(SIGTRAP)
              if(ret_val/=0_c_int) print*, "raise returned=",ret_val
#endif

              n = set_random_size()
              start = rdtsc_wrap()
              allocate(m_param_gamma.phi(n))
              allocate(m_param_gamma.gamma(n))
              end   = rdtsc_wrap()
              call check_allocation_stats(m_param_gamma.phi)
              call check_allocation_stats(m_param_gamma.gamma)
              start_c     = start-RDTSC_LATENCY
              end_c       = end-RDTSC_LATENCY
              tsc_elapsed = end_c-start_c
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              if(allocated(m_param_gamma.phi)) deallocate(m_param_gamma.phi)
              if(allocated(m_param_gamma.gamma)) deallocate(m_param_gamma.gamma)
              call check_allocation_stats(m_param_gamma.phi)
              call check_allocation_stats(m_param_gamma.gamma)
              print*, footer 
              contains 

              function set_random_size() result(rval)
                       implicit none 
                       integer(kind=i4), automatic :: rnum 
                       integer(kind=i4), parameter :: lo = 512
                       integer(kind=i4)            :: rval 
                       rnum = irand()
                       if(rnum<lo) then 
                          rnum=lo
                          rval=rnum
                       end if 
                       rval=rnum 
               end function set_random_size

              subroutine check_allocation_stats(array)
                       implicit none
                       real(kind=dp), allocatable, dimension(:), intent(in) :: array 
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
              end subroutine check_allocation_stats

end subroutine unit_test_param_gamma_r8_t


subroutine unit_test_SN_r4_t 
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
              character(len=80),                  parameter  :: header = "[TEST #3:  alloc/dealloc: SN_r4_t -- START]"
              character(len=80),                  parameter  :: footer = "[TEST #3:  alloc/dealloc: SN_r4_t -- END]" 
              type(SN_r4_t),                      automatic  :: m_SN  
              integer(kind=c_long_long),          automatic  :: start,end 
              integer(kind=c_long_long),          automatic  :: start_c,end_c 
              integer(kind=c_long_long),          automatic  :: tsc_elapsed 
              integer(kind=i4),                   automatic  :: n
              
#if 0
              integer(c_int),            parameter :: SIGTRAP = 5 
              integer(c_int),            automatic :: ret_val
#endif 

              print*, header
#if 0
              ret_val = raise(SIGTRAP)
              if(ret_val/=0_c_int) print*, "raise returned=",ret_val
#endif

              n = set_random_size()
              start = rdtsc_wrap()
              allocate(m_SN.phi(n))
              allocate(m_SN.gamma(n))
              allocate(m_SN.sn(n))
              end   = rdtsc_wrap()
              call check_allocation_stats(m_SN.phi)
              call check_allocation_stats(m_SN.gamma)
              call check_allocation_stats(m_SN.sn)
              start_c     = start-RDTSC_LATENCY
              end_c       = end-RDTSC_LATENCY
              tsc_elapsed = end_c-start_c
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              if(allocated(m_SN.phi)) deallocate(m_SN.phi)
              if(allocated(m_SN.gamma)) deallocate(m_SN.gamma)
              if(allocated(m_SN.sn)) deallocate(m_SN.sn)
              call check_allocation_stats(m_SN.phi)
              call check_allocation_stats(m_SN.gamma)
              call check_allocation_stats(m_Sn.sn)
              print*, footer 
              contains 

              function set_random_size() result(rval)
                       implicit none 
                       integer(kind=i4), automatic :: rnum 
                       integer(kind=i4), parameter :: lo = 512
                       integer(kind=i4)            :: rval 
                       rnum = irand()
                       if(rnum<lo) then 
                          rnum=lo
                          rval=rnum
                       end if 
                       rval=rnum 
               end function set_random_size

              subroutine check_allocation_stats(array)
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
              end subroutine check_allocation_stats

end subroutine unit_test_SN_r4_t


subroutine unit_test_SN_r8_t 
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
              character(len=80),                  parameter  :: header = "[TEST #4:  alloc/dealloc: SN_r8_t -- START]"
              character(len=80),                  parameter  :: footer = "[TEST #4:  alloc/dealloc: SN_r8_t -- END]" 
              type(SN_r8_t),                      automatic  :: m_SN  
              integer(kind=c_long_long),          automatic  :: start,end 
              integer(kind=c_long_long),          automatic  :: start_c,end_c 
              integer(kind=c_long_long),          automatic  :: tsc_elapsed 
              integer(kind=i4),                   automatic  :: n
              
#if 0
              integer(c_int),            parameter :: SIGTRAP = 5 
              integer(c_int),            automatic :: ret_val
#endif 

              print*, header
#if 0
              ret_val = raise(SIGTRAP)
              if(ret_val/=0_c_int) print*, "raise returned=",ret_val
#endif

              n = set_random_size()
              start = rdtsc_wrap()
              allocate(m_SN.phi(n))
              allocate(m_SN.gamma(n))
              allocate(m_SN.sn(n))
              end   = rdtsc_wrap()
              call check_allocation_stats(m_SN.phi)
              call check_allocation_stats(m_SN.gamma)
              call check_allocation_stats(m_SN.sn)
              start_c     = start-RDTSC_LATENCY
              end_c       = end-RDTSC_LATENCY
              tsc_elapsed = end_c-start_c
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              if(allocated(m_SN.phi)) deallocate(m_SN.phi)
              if(allocated(m_SN.gamma)) deallocate(m_SN.gamma)
              if(allocated(m_SN.sn)) deallocate(m_SN.sn)
              call check_allocation_stats(m_SN.phi)
              call check_allocation_stats(m_SN.gamma)
              call check_allocation_stats(m_SN.sn)
              print*, footer 
              contains 

              function set_random_size() result(rval)
                       implicit none 
                       integer(kind=i4), automatic :: rnum 
                       integer(kind=i4), parameter :: lo = 512
                       integer(kind=i4)            :: rval 
                       rnum = irand()
                       if(rnum<lo) then 
                          rnum=lo
                          rval=rnum
                       end if 
                       rval=rnum 
               end function set_random_size

              subroutine check_allocation_stats(array)
                       implicit none
                       real(kind=dp), allocatable, dimension(:), intent(in) :: array 
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
              end subroutine check_allocation_stats

end subroutine unit_test_SN_r8_t


subroutine unit_test_SM_r4_t 
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
              character(len=80),                  parameter  :: header = "[TEST #5:  alloc/dealloc: SM_r4_t -- START]"
              character(len=80),                  parameter  :: footer = "[TEST #5:  alloc/dealloc: SM_r4_t -- END]" 
              type(SM_r4_t),                      automatic  :: m_SM  
              integer(kind=c_long_long),          automatic  :: start,end 
              integer(kind=c_long_long),          automatic  :: start_c,end_c 
              integer(kind=c_long_long),          automatic  :: tsc_elapsed 
              integer(kind=i4),                   automatic  :: n
              
#if 0
              integer(c_int),            parameter :: SIGTRAP = 5 
              integer(c_int),            automatic :: ret_val
#endif 

              print*, header
#if 0
              ret_val = raise(SIGTRAP)
              if(ret_val/=0_c_int) print*, "raise returned=",ret_val
#endif

              n = set_random_size()
              start = rdtsc_wrap()
              allocate(m_SM.phi(n))
              allocate(m_SM.gamma(n))
              allocate(m_SM.sm(n))
              end   = rdtsc_wrap()
              call check_allocation_stats(m_SM.phi)
              call check_allocation_stats(m_SM.gamma)
              call check_allocation_stats(m_SM.sm)
              start_c     = start-RDTSC_LATENCY
              end_c       = end-RDTSC_LATENCY
              tsc_elapsed = end_c-start_c
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              if(allocated(m_SM.phi)) deallocate(m_SM.phi)
              if(allocated(m_SM.gamma)) deallocate(m_SM.gamma)
              if(allocated(m_SM.sm)) deallocate(m_SM.sm)
              call check_allocation_stats(m_SM.phi)
              call check_allocation_stats(m_SM.gamma)
              call check_allocation_stats(m_SM.sm)
              print*, footer 
              contains 

              function set_random_size() result(rval)
                       implicit none 
                       integer(kind=i4), automatic :: rnum 
                       integer(kind=i4), parameter :: lo = 512
                       integer(kind=i4)            :: rval 
                       rnum = irand()
                       if(rnum<lo) then 
                          rnum=lo
                          rval=rnum
                       end if 
                       rval=rnum 
               end function set_random_size

              subroutine check_allocation_stats(array)
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
              end subroutine check_allocation_stats

end subroutine unit_test_SM_r4_t


subroutine unit_test_SM_r8_t 
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
              character(len=80),                  parameter  :: header = "[TEST #6:  alloc/dealloc: SM_r8_t -- START]"
              character(len=80),                  parameter  :: footer = "[TEST #6:  alloc/dealloc: SM_r8_t -- END]" 
              type(SM_r8_t),                      automatic  :: m_SM  
              integer(kind=c_long_long),          automatic  :: start,end 
              integer(kind=c_long_long),          automatic  :: start_c,end_c 
              integer(kind=c_long_long),          automatic  :: tsc_elapsed 
              integer(kind=i4),                   automatic  :: n
              
#if 0
              integer(c_int),            parameter :: SIGTRAP = 5 
              integer(c_int),            automatic :: ret_val
#endif 

              print*, header
#if 0
              ret_val = raise(SIGTRAP)
              if(ret_val/=0_c_int) print*, "raise returned=",ret_val
#endif

              n = set_random_size()
              start = rdtsc_wrap()
              allocate(m_SM.phi(n))
              allocate(m_SM.gamma(n))
              allocate(m_SM.sm(n))
              end   = rdtsc_wrap()
              call check_allocation_stats(m_SM.phi)
              call check_allocation_stats(m_SM.gamma)
              call check_allocation_stats(m_SM.sm)
              start_c     = start-RDTSC_LATENCY
              end_c       = end-RDTSC_LATENCY
              tsc_elapsed = end_c-start_c
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              if(allocated(m_SM.phi)) deallocate(m_SM.phi)
              if(allocated(m_SM.gamma)) deallocate(m_SM.gamma)
              if(allocated(m_SM.sm)) deallocate(m_SM.sm)
              call check_allocation_stats(m_SM.phi)
              call check_allocation_stats(m_SM.gamma)
              call check_allocation_stats(m_SM.sm)
              print*, footer 
              contains 

              function set_random_size() result(rval)
                       implicit none 
                       integer(kind=i4), automatic :: rnum 
                       integer(kind=i4), parameter :: lo = 512
                       integer(kind=i4)            :: rval 
                       rnum = irand()
                       if(rnum<lo) then 
                          rnum=lo
                          rval=rnum
                       end if 
                       rval=rnum 
               end function set_random_size

              subroutine check_allocation_stats(array)
                       implicit none
                       real(kind=dp), allocatable, dimension(:), intent(in) :: array 
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
              end subroutine check_allocation_stats

end subroutine unit_test_SM_r8_t


subroutine unit_test_ratio_FH_r4_t 
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
              character(len=80),                  parameter  :: header = "[TEST #7:  alloc/dealloc: ratio_FH_r4_t -- START]"
              character(len=80),                  parameter  :: footer = "[TEST #7:  alloc/dealloc: ratio_FH_r4_t -- END]" 
              type(ratio_FH_r4_t),                automatic  :: m_ratio_FH  
              integer(kind=c_long_long),          automatic  :: start,end 
              integer(kind=c_long_long),          automatic  :: start_c,end_c 
              integer(kind=c_long_long),          automatic  :: tsc_elapsed 
              integer(kind=i4),                   automatic  :: n
              
#if 0
              integer(c_int),            parameter :: SIGTRAP = 5 
              integer(c_int),            automatic :: ret_val
#endif 

              print*, header
#if 0
              ret_val = raise(SIGTRAP)
              if(ret_val/=0_c_int) print*, "raise returned=",ret_val
#endif

              n = set_random_size()
              start = rdtsc_wrap()
              allocate(m_ratio_FH.phi(n))
              allocate(m_ratio_FH.psi(n))
              allocate(m_ratio_FH.fh(n))
              end   = rdtsc_wrap()
              call check_allocation_stats(m_ratio_FH.phi)
              call check_allocation_stats(m_ratio_FH.psi)
              call check_allocation_stats(m_ratio_FH.fh)
              start_c     = start-RDTSC_LATENCY
              end_c       = end-RDTSC_LATENCY
              tsc_elapsed = end_c-start_c
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              if(allocated(m_ratio_FH.phi)) deallocate(m_ratio_FH.phi)
              if(allocated(m_ratio_FH.psi)) deallocate(m_ratio_FH.psi)
              if(allocated(m_ratio_FH.fh)) deallocate(m_ratio_FH.fh)
              call check_allocation_stats(m_ratio_FH.phi)
              call check_allocation_stats(m_ratio_FH.psi)
              call check_allocation_stats(m_ratio_FH.fh)
              print*, footer 
              contains 

              function set_random_size() result(rval)
                       implicit none 
                       integer(kind=i4), automatic :: rnum 
                       integer(kind=i4), parameter :: lo = 512
                       integer(kind=i4)            :: rval 
                       rnum = irand()
                       if(rnum<lo) then 
                          rnum=lo
                          rval=rnum
                       end if 
                       rval=rnum 
               end function set_random_size

              subroutine check_allocation_stats(array)
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
              end subroutine check_allocation_stats

end subroutine unit_test_ratio_FH_r4_t


subroutine unit_test_ratio_FH_r8_t 
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
              character(len=80),                  parameter  :: header = "[TEST #8:  alloc/dealloc: ratio_FH_r8_t -- START]"
              character(len=80),                  parameter  :: footer = "[TEST #8:  alloc/dealloc: ratio_FH_r8_t -- END]" 
              type(ratio_FH_r8_t),                automatic  :: m_ratio_FH  
              integer(kind=c_long_long),          automatic  :: start,end 
              integer(kind=c_long_long),          automatic  :: start_c,end_c 
              integer(kind=c_long_long),          automatic  :: tsc_elapsed 
              integer(kind=i4),                   automatic  :: n
              
#if 0
              integer(c_int),            parameter :: SIGTRAP = 5 
              integer(c_int),            automatic :: ret_val
#endif 

              print*, header
#if 0
              ret_val = raise(SIGTRAP)
              if(ret_val/=0_c_int) print*, "raise returned=",ret_val
#endif

              n = set_random_size()
              start = rdtsc_wrap()
              allocate(m_ratio_FH.phi(n))
              allocate(m_ratio_FH.psi(n))
              allocate(m_ratio_FH.fh(n))
              end   = rdtsc_wrap()
              call check_allocation_stats(m_ratio_FH.phi)
              call check_allocation_stats(m_ratio_FH.psi)
              call check_allocation_stats(m_ratio_FH.fh)
              start_c     = start-RDTSC_LATENCY
              end_c       = end-RDTSC_LATENCY
              tsc_elapsed = end_c-start_c
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              if(allocated(m_ratio_FH.phi)) deallocate(m_ratio_FH.phi)
              if(allocated(m_ratio_FH.psi)) deallocate(m_ratio_FH.psi)
              if(allocated(m_ratio_FH.fh)) deallocate(m_ratio_FH.fh)
              call check_allocation_stats(m_ratio_FH.phi)
              call check_allocation_stats(m_ratio_FH.psi)
              call check_allocation_stats(m_ratio_FH.fh)
              print*, footer 
              contains 

              function set_random_size() result(rval)
                       implicit none 
                       integer(kind=i4), automatic :: rnum 
                       integer(kind=i4), parameter :: lo = 512
                       integer(kind=i4)            :: rval 
                       rnum = irand()
                       if(rnum<lo) then 
                          rnum=lo
                          rval=rnum
                       end if 
                       rval=rnum 
               end function set_random_size

              subroutine check_allocation_stats(array)
                       implicit none
                       real(kind=dp), allocatable, dimension(:), intent(in) :: array 
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
              end subroutine check_allocation_stats

end subroutine unit_test_ratio_FH_r8_t


subroutine unit_test_scan_mirror_ang_r4_t 
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
              character(len=80),                  parameter  :: header = "[TEST #9:  alloc/dealloc: scan_mirror_ang_r4_t  -- START]"
              character(len=80),                  parameter  :: footer = "[TEST #9:  alloc/dealloc: scan_mirror_ang_r4_t  -- END]" 
              type(scan_mirror_ang_r4_t),         automatic  :: m_scan_mirror_ang 
              integer(kind=c_long_long),          automatic  :: start,end 
              integer(kind=c_long_long),          automatic  :: start_c,end_c 
              integer(kind=c_long_long),          automatic  :: tsc_elapsed 
              integer(kind=i4),                   automatic  :: n
              
#if 0
              integer(c_int),            parameter :: SIGTRAP = 5 
              integer(c_int),            automatic :: ret_val
#endif 

              print*, header
#if 0
              ret_val = raise(SIGTRAP)
              if(ret_val/=0_c_int) print*, "raise returned=",ret_val
#endif

              n = set_random_size()
              start = rdtsc_wrap()
              allocate(m_scan_mirror_ang.gam0(n))
              allocate(m_scan_mirror_ang.phi(n))
              allocate(m_scan_mirror_ang.psi(n))
              allocate(m_scan_mirror_ang.gamma(n))
              end   = rdtsc_wrap()
              call check_allocation_stats(m_scan_mirror_ang.gam0)
              call check_allocation_stats(m_scan_mirror_ang.phi)
              call check_allocation_stats(m_scan_mirror_ang.psi)
              call check_allocation_stats(m_scan_mirror_ang.gamma)
              start_c     = start-RDTSC_LATENCY
              end_c       = end-RDTSC_LATENCY
              tsc_elapsed = end_c-start_c
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              if(allocated(m_scan_mirror_ang.gam0)) deallocate(m_scan_mirror_ang.gam0)
              if(allocated(m_scan_mirror_ang.phi)) deallocate(m_scan_mirror_ang.phi)
              if(allocated(m_scan_mirror_ang.psi)) deallocate(m_scan_mirror_ang.psi)
              if(allocated(m_scan_mirror_ang.gamma)) deallocate(m_scan_mirror_ang.gamma)
              call check_allocation_stats(m_scan_mirror_ang.gam0)
              call check_allocation_stats(m_scan_mirror_ang.phi)
              call check_allocation_stats(m_scan_mirror_ang.psi)
              call check_allocation_stats(m_scan_mirror_ang.gamma)
              print*, footer 
              contains 

              function set_random_size() result(rval)
                       implicit none 
                       integer(kind=i4), automatic :: rnum 
                       integer(kind=i4), parameter :: lo = 512
                       integer(kind=i4)            :: rval 
                       rnum = irand()
                       if(rnum<lo) then 
                          rnum=lo
                          rval=rnum
                       end if 
                       rval=rnum 
               end function set_random_size

              subroutine check_allocation_stats(array)
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
              end subroutine check_allocation_stats

end subroutine unit_test_scan_mirror_ang_r4_t 


subroutine unit_test_scan_mirror_ang_r8_t 
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
              character(len=80),                  parameter  :: header = "[TEST #10:  alloc/dealloc: scan_mirror_ang_r8_t  -- START]"
              character(len=80),                  parameter  :: footer = "[TEST #10:  alloc/dealloc: scan_mirror_ang_r8_t  -- END]" 
              type(scan_mirror_ang_r8_t),         automatic  :: m_scan_mirror_ang 
              integer(kind=c_long_long),          automatic  :: start,end 
              integer(kind=c_long_long),          automatic  :: start_c,end_c 
              integer(kind=c_long_long),          automatic  :: tsc_elapsed 
              integer(kind=i4),                   automatic  :: n
              
#if 0
              integer(c_int),            parameter :: SIGTRAP = 5 
              integer(c_int),            automatic :: ret_val
#endif 

              print*, header
#if 0
              ret_val = raise(SIGTRAP)
              if(ret_val/=0_c_int) print*, "raise returned=",ret_val
#endif

              n = set_random_size()
              start = rdtsc_wrap()
              allocate(m_scan_mirror_ang.gam0(n))
              allocate(m_scan_mirror_ang.phi(n))
              allocate(m_scan_mirror_ang.psi(n))
              allocate(m_scan_mirror_ang.gamma(n))
              end   = rdtsc_wrap()
              call check_allocation_stats(m_scan_mirror_ang.gam0)
              call check_allocation_stats(m_scan_mirror_ang.phi)
              call check_allocation_stats(m_scan_mirror_ang.psi)
              call check_allocation_stats(m_scan_mirror_ang.gamma)
              start_c     = start-RDTSC_LATENCY
              end_c       = end-RDTSC_LATENCY
              tsc_elapsed = end_c-start_c
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              if(allocated(m_scan_mirror_ang.gam0)) deallocate(m_scan_mirror_ang.gam0)
              if(allocated(m_scan_mirror_ang.phi)) deallocate(m_scan_mirror_ang.phi)
              if(allocated(m_scan_mirror_ang.psi)) deallocate(m_scan_mirror_ang.psi)
              if(allocated(m_scan_mirror_ang.gamma)) deallocate(m_scan_mirror_ang.gamma)
              call check_allocation_stats(m_scan_mirror_ang.gam0)
              call check_allocation_stats(m_scan_mirror_ang.phi)
              call check_allocation_stats(m_scan_mirror_ang.psi)
              call check_allocation_stats(m_scan_mirror_ang.gamma)
              print*, footer 
              contains 

              function set_random_size() result(rval)
                       implicit none 
                       integer(kind=i4), automatic :: rnum 
                       integer(kind=i4), parameter :: lo = 512
                       integer(kind=i4)            :: rval 
                       rnum = irand()
                       if(rnum<lo) then 
                          rnum=lo
                          rval=rnum
                       end if 
                       rval=rnum 
               end function set_random_size

              subroutine check_allocation_stats(array)
                       implicit none
                       real(kind=dp), allocatable, dimension(:), intent(in) :: array 
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
              end subroutine check_allocation_stats

end subroutine unit_test_scan_mirror_ang_r8_t 


subroutine unit_test_Dmax_r4_t 
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
              character(len=80),                  parameter  :: header = "[TEST #11:  alloc/dealloc: Dmax_r4_t  -- START]"
              character(len=80),                  parameter  :: footer = "[TEST #11:  alloc/dealloc: Dmax_r4_t  -- END]" 
              type(Dmax_r4_t),                    automatic  :: m_Dmax 
              integer(kind=c_long_long),          automatic  :: start,end 
              integer(kind=c_long_long),          automatic  :: start_c,end_c 
              integer(kind=c_long_long),          automatic  :: tsc_elapsed 
              integer(kind=i4),                   automatic  :: n
              
#if 0
              integer(c_int),            parameter :: SIGTRAP = 5 
              integer(c_int),            automatic :: ret_val
#endif 

              print*, header
#if 0
              ret_val = raise(SIGTRAP)
              if(ret_val/=0_c_int) print*, "raise returned=",ret_val
#endif

              n = set_random_size()
              start = rdtsc_wrap()
              allocate(m_Dmax.h(n))
              allocate(m_Dmax.delta(n))
              allocate(m_Dmax.gamma(n))
              allocate(m_Dmax.dmax(n))
              end   = rdtsc_wrap()
              call check_allocation_stats(m_Dmax.h)
              call check_allocation_stats(m_Dmax.delta)
              call check_allocation_stats(m_Dmax.gamma)
              call check_allocation_stats(m_Dmax.dmax)
              start_c     = start-RDTSC_LATENCY
              end_c       = end-RDTSC_LATENCY
              tsc_elapsed = end_c-start_c
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              if(allocated(m_Dmax.h)) deallocate(m_Dmax.h)
              if(allocated(m_Dmax.delta)) deallocate(m_Dmax.delta)
              if(allocated(m_Dmax.gamma)) deallocate(m_Dmax.gamma)
              if(allocated(m_Dmax.dmax)) deallocate(m_Dmax.dmax)
              call check_allocation_stats(m_Dmax.h)
              call check_allocation_stats(m_Dmax.delta)
              call check_allocation_stats(m_Dmax.gamma)
              call check_allocation_stats(m_Dmax.dmax)
              print*, footer 
              contains 

              function set_random_size() result(rval)
                       implicit none 
                       integer(kind=i4), automatic :: rnum 
                       integer(kind=i4), parameter :: lo = 512
                       integer(kind=i4)            :: rval 
                       rnum = irand()
                       if(rnum<lo) then 
                          rnum=lo
                          rval=rnum
                       end if 
                       rval=rnum 
               end function set_random_size

              subroutine check_allocation_stats(array)
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
              end subroutine check_allocation_stats

end subroutine unit_test_Dmax_r4_t 


subroutine unit_test_Dmax_r8_t 
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
              character(len=80),                  parameter  :: header = "[TEST #12:  alloc/dealloc: Dmax_r8_t  -- START]"
              character(len=80),                  parameter  :: footer = "[TEST #12:  alloc/dealloc: Dmax_r8_t  -- END]" 
              type(Dmax_r8_t),                    automatic  :: m_Dmax 
              integer(kind=c_long_long),          automatic  :: start,end 
              integer(kind=c_long_long),          automatic  :: start_c,end_c 
              integer(kind=c_long_long),          automatic  :: tsc_elapsed 
              integer(kind=i4),                   automatic  :: n
              
#if 0
              integer(c_int),            parameter :: SIGTRAP = 5 
              integer(c_int),            automatic :: ret_val
#endif 

              print*, header
#if 0
              ret_val = raise(SIGTRAP)
              if(ret_val/=0_c_int) print*, "raise returned=",ret_val
#endif

              n = set_random_size()
              start = rdtsc_wrap()
              allocate(m_Dmax.h(n))
              allocate(m_Dmax.delta(n))
              allocate(m_Dmax.gamma(n))
              allocate(m_Dmax.dmax(n))
              end   = rdtsc_wrap()
              call check_allocation_stats(m_Dmax.h)
              call check_allocation_stats(m_Dmax.delta)
              call check_allocation_stats(m_Dmax.gamma)
              call check_allocation_stats(m_Dmax.dmax)
              start_c     = start-RDTSC_LATENCY
              end_c       = end-RDTSC_LATENCY
              tsc_elapsed = end_c-start_c
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              if(allocated(m_Dmax.h)) deallocate(m_Dmax.h)
              if(allocated(m_Dmax.delta)) deallocate(m_Dmax.delta)
              if(allocated(m_Dmax.gamma)) deallocate(m_Dmax.gamma)
              if(allocated(m_Dmax.dmax)) deallocate(m_Dmax.dmax)
              call check_allocation_stats(m_Dmax.h)
              call check_allocation_stats(m_Dmax.delta)
              call check_allocation_stats(m_Dmax.gamma)
              call check_allocation_stats(m_Dmax.dmax)
              print*, footer 
              contains 

              function set_random_size() result(rval)
                       implicit none 
                       integer(kind=i4), automatic :: rnum 
                       integer(kind=i4), parameter :: lo = 512
                       integer(kind=i4)            :: rval 
                       rnum = irand()
                       if(rnum<lo) then 
                          rnum=lo
                          rval=rnum
                       end if 
                       rval=rnum 
               end function set_random_size

              subroutine check_allocation_stats(array)
                       implicit none
                       real(kind=dp), allocatable, dimension(:), intent(in) :: array 
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
              end subroutine check_allocation_stats

end subroutine unit_test_Dmax_r8_t 


subroutine unit_test_Dmin_r4_t 
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
              character(len=80),                  parameter  :: header = "[TEST #13:  alloc/dealloc: Dmin_r4_t  -- START]"
              character(len=80),                  parameter  :: footer = "[TEST #13:  alloc/dealloc: Dmin_r4_t  -- END]" 
              type(Dmax_r4_t),                    automatic  :: m_Dmin
              integer(kind=c_long_long),          automatic  :: start,end 
              integer(kind=c_long_long),          automatic  :: start_c,end_c 
              integer(kind=c_long_long),          automatic  :: tsc_elapsed 
              integer(kind=i4),                   automatic  :: n
              
#if 0
              integer(c_int),            parameter :: SIGTRAP = 5 
              integer(c_int),            automatic :: ret_val
#endif 

              print*, header
#if 0
              ret_val = raise(SIGTRAP)
              if(ret_val/=0_c_int) print*, "raise returned=",ret_val
#endif

              n = set_random_size()
              start = rdtsc_wrap()
              allocate(m_Dmin.h(n))
              allocate(m_Dmin.delta(n))
              allocate(m_Dmin.dmin(n))
              end   = rdtsc_wrap()
              call check_allocation_stats(m_Dmin.h)
              call check_allocation_stats(m_Dmin.delta)
              call check_allocation_stats(m_Dmin.dmin)
              start_c     = start-RDTSC_LATENCY
              end_c       = end-RDTSC_LATENCY
              tsc_elapsed = end_c-start_c
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              if(allocated(m_Dmin.h)) deallocate(m_Dmin.h)
              if(allocated(m_Dmin.delta)) deallocate(m_Dmin.delta)
              if(allocated(m_Dmin.dmin)) deallocate(m_Dmin.dmin)
              call check_allocation_stats(m_Dmin.h)
              call check_allocation_stats(m_Dmin.delta)
              call check_allocation_stats(m_Dmin.dmin)
              print*, footer 
              contains 

              function set_random_size() result(rval)
                       implicit none 
                       integer(kind=i4), automatic :: rnum 
                       integer(kind=i4), parameter :: lo = 512
                       integer(kind=i4)            :: rval 
                       rnum = irand()
                       if(rnum<lo) then 
                          rnum=lo
                          rval=rnum
                       end if 
                       rval=rnum 
               end function set_random_size

              subroutine check_allocation_stats(array)
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
              end subroutine check_allocation_stats

end subroutine unit_test_Dmin_r4_t 


subroutine unit_test_Dmin_r8_t 
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
              character(len=80),                  parameter  :: header = "[TEST #14:  alloc/dealloc: Dmin_r8_t  -- START]"
              character(len=80),                  parameter  :: footer = "[TEST #14:  alloc/dealloc: Dmin_r8_t  -- END]" 
              type(Dmax_r8_t),                    automatic  :: m_Dmin
              integer(kind=c_long_long),          automatic  :: start,end 
              integer(kind=c_long_long),          automatic  :: start_c,end_c 
              integer(kind=c_long_long),          automatic  :: tsc_elapsed 
              integer(kind=i4),                   automatic  :: n
              
#if 0
              integer(c_int),            parameter :: SIGTRAP = 5 
              integer(c_int),            automatic :: ret_val
#endif 

              print*, header
#if 0
              ret_val = raise(SIGTRAP)
              if(ret_val/=0_c_int) print*, "raise returned=",ret_val
#endif

              n = set_random_size()
              start = rdtsc_wrap()
              allocate(m_Dmin.h(n))
              allocate(m_Dmin.delta(n))
              allocate(m_Dmin.dmin(n))
              end   = rdtsc_wrap()
              call check_allocation_stats(m_Dmin.h)
              call check_allocation_stats(m_Dmin.delta)
              call check_allocation_stats(m_Dmin.dmin)
              start_c     = start-RDTSC_LATENCY
              end_c       = end-RDTSC_LATENCY
              tsc_elapsed = end_c-start_c
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              if(allocated(m_Dmin.h)) deallocate(m_Dmin.h)
              if(allocated(m_Dmin.delta)) deallocate(m_Dmin.delta)
              if(allocated(m_Dmin.dmin)) deallocate(m_Dmin.dmin)
              call check_allocation_stats(m_Dmin.h)
              call check_allocation_stats(m_Dmin.delta)
              call check_allocation_stats(m_Dmin.dmin)
              print*, footer 
              contains 

              function set_random_size() result(rval)
                       implicit none 
                       integer(kind=i4), automatic :: rnum 
                       integer(kind=i4), parameter :: lo = 512
                       integer(kind=i4)            :: rval 
                       rnum = irand()
                       if(rnum<lo) then 
                          rnum=lo
                          rval=rnum
                       end if 
                       rval=rnum 
               end function set_random_size

              subroutine check_allocation_stats(array)
                       implicit none
                       real(kind=dp), allocatable, dimension(:), intent(in) :: array 
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
              end subroutine check_allocation_stats

end subroutine unit_test_Dmin_r8_t



end module mod_test_eos_sensor_types  


program main 
   use mod_test_eos_sensor_types
   call unit_test_param_gamma_r4_t()
   call unit_test_param_gamma_r8_t()
   call unit_test_SN_r4_t()
   call unit_test_SN_r8_t()
   call unit_test_SM_r4_t()
   call unit_test_SM_r8_t()
   call unit_test_ratio_FH_r4_t()
   call unit_test_ratio_FH_r8_t()
   call unit_test_scan_mirror_ang_r4_t()
   call unit_test_scan_mirror_ang_r8_t()
   call unit_test_Dmax_r4_t()
   call unit_test_Dmax_r8_t()
end program main 