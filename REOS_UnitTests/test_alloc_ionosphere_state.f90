
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

module mod_test_alloc_ionosphere_state

       use mod_kinds, only : i1, i4, i8, sp, dp 
       use atmos_refraction 

       character(len=1),        parameter, private :: dcol = ":"

#if 0
     ICC and Ifort compiler arguments:
     icc -c -std=c99 GMS_intrinsics_wrappers.c
     ifort -o unit_test_alloc_ionosphere_state -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 
     -fopenmp -qopenmp -fpp -falign-functions=32 GMS_kinds.f90 GMS_atmos_refraction.f90 GMS_intrinsics_wrappers.o test_alloc_ionosphere_state.f90

     
#endif

        contains 

        subroutine unit_test_alloc_ionosphere_state_r4()
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
              type(ionosphere_state_r4_t),       automatic :: iono_state 
              integer(kind=i4), dimension(0:10), automatic :: sparams 
              character(len=128),                automatic :: filename 
#if 0
              integer(c_int),                    parameter :: SIGTRAP = 5 
#endif
              character(len=10)                             :: t
              character(len=8)                              :: d
              character(len=64),                  parameter :: header = "[TEST #17: alloc_ionosphere_state_r4 -- START]"
              character(len=64),                  parameter :: footer = "[TEST #17: alloc_ionosphere_state_r4 -- END]"
              integer(c_long_long),               automatic :: start, end 
              integer(kind=i4),                   automatic :: i__  
              call DATE_AND_TIME(date=d,time=t)
              filename = __FILE__ 
              lstart   = __LINE__ 
              print*, d , dcol , t , filename , lstart
              print*,  header   
              
              do i__=0, 11
                 sparams(i__) = set_random_size() 
              end do 
              print*, "================================ ALLOCATION =================================" 
#if 0
              print*, raise(SIGTRAP)
#endif 
              start = rdtsc_wrap()
              call alloc_ionosphere_state_r4(iono_state,sparams)
              end   = rdtsc_wrap()
              print*, "alloc_ionosphere_state_r4:"
              print*, "TSC-start=",start
              print*, "TSC-end  =",end
              print*, "TSC-delta=",end-start
              call check_allocation_stats(iono_state.elec_dens)
              call check_allocation_stats(iono_state.neut_tmp)
              call check_allocation_stats(iono_state.ion_tmp)
              call check_allocation_stats(iono_state.elec_tmp)
              call check_allocation_stats(iono_state.O_ion_d)
              call check_allocation_stats(iono_state.H_ion_d)
              call check_allocation_stats(iono_state.He_ion_d)
              call check_allocation_stats(iono_state.O2_ion_d)
              call check_allocation_stats(iono_state.NO_ion_d)
              call check_allocation_stats(iono_state.ion_dens)
              call check_allocation_stats(iono_state.N_ion_d)
              print*, "================================ DEALLOCATION ==============================="
#if 0
              print*, raise(SIGTRAP)
#endif 
              start = rdtsc_wrap()
              call dealloc_ionosphere_state_r4(iono_state)
              end   = rdtsc_wrap()
              print*, "dealloc_ionosphere_state_r4:"
              print*, "TSC-start=",start
              print*, "TSC-end  =",end
              print*, "TSC-delta=",end-start
              call check_allocation_stats(iono_state.elec_dens)
              call check_allocation_stats(iono_state.neut_tmp)
              call check_allocation_stats(iono_state.ion_tmp)
              call check_allocation_stats(iono_state.elec_tmp)
              call check_allocation_stats(iono_state.O_ion_d)
              call check_allocation_stats(iono_state.H_ion_d)
              call check_allocation_stats(iono_state.He_ion_d)
              call check_allocation_stats(iono_state.O2_ion_d)
              call check_allocation_stats(iono_state.NO_ion_d)
              call check_allocation_stats(iono_state.ion_dens)
              call check_allocation_stats(iono_state.N_ion_d)
              call DATE_AND_TIME(date=d,time=t)
              lend = __LINE__
              print*, d , dcol , t , filename , lend  
              print*,  footer
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
        end subroutine unit_test_alloc_ionosphere_state_r4

        subroutine unit_test_alloc_ionosphere_state_r8()
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
              type(ionosphere_state_r8_t),       automatic :: iono_state 
              integer(kind=i4), dimension(0:10), automatic :: sparams 
              character(len=128),                automatic :: filename 
#if 0
              integer(c_int),                    parameter :: SIGTRAP = 5 
#endif
              character(len=10)                             :: t
              character(len=8)                              :: d
              character(len=64),                  parameter :: header = "[TEST #18: alloc_ionosphere_state_r8 -- START]"
              character(len=64),                  parameter :: footer = "[TEST #18: alloc_ionosphere_state_r8 -- END]"
              integer(c_long_long),               automatic :: start, end 
              integer(kind=i4),                   automatic :: i__  
              call DATE_AND_TIME(date=d,time=t)
              filename = __FILE__ 
              lstart   = __LINE__ 
              print*, d , dcol , t , filename , lstart
              print*,  header   
              
              do i__=0, 11
                 sparams(i__) = set_random_size() 
              end do 
              print*, "================================ ALLOCATION =================================" 
#if 0
              print*, raise(SIGTRAP)
#endif 
              start = rdtsc_wrap()
              call alloc_ionosphere_state_r8(iono_state,sparams)
              end   = rdtsc_wrap()
              print*, "alloc_ionosphere_state_r8:"
              print*, "TSC-start=",start
              print*, "TSC-end  =",end
              print*, "TSC-delta=",end-start
              call check_allocation_stats(iono_state.elec_dens)
              call check_allocation_stats(iono_state.neut_tmp)
              call check_allocation_stats(iono_state.ion_tmp)
              call check_allocation_stats(iono_state.elec_tmp)
              call check_allocation_stats(iono_state.O_ion_d)
              call check_allocation_stats(iono_state.H_ion_d)
              call check_allocation_stats(iono_state.He_ion_d)
              call check_allocation_stats(iono_state.O2_ion_d)
              call check_allocation_stats(iono_state.NO_ion_d)
              call check_allocation_stats(iono_state.ion_dens)
              call check_allocation_stats(iono_state.N_ion_d)
              print*, "================================ DEALLOCATION ==============================="
#if 0
              print*, raise(SIGTRAP)
#endif 
              start = rdtsc_wrap()
              call dealloc_ionosphere_state_r8(iono_state)
              end   = rdtsc_wrap()
              print*, "dealloc_ionosphere_state_r8:"
              print*, "TSC-start=",start
              print*, "TSC-end  =",end
              print*, "TSC-delta=",end-start
              call check_allocation_stats(iono_state.elec_dens)
              call check_allocation_stats(iono_state.neut_tmp)
              call check_allocation_stats(iono_state.ion_tmp)
              call check_allocation_stats(iono_state.elec_tmp)
              call check_allocation_stats(iono_state.O_ion_d)
              call check_allocation_stats(iono_state.H_ion_d)
              call check_allocation_stats(iono_state.He_ion_d)
              call check_allocation_stats(iono_state.O2_ion_d)
              call check_allocation_stats(iono_state.NO_ion_d)
              call check_allocation_stats(iono_state.ion_dens)
              call check_allocation_stats(iono_state.N_ion_d)
              call DATE_AND_TIME(date=d,time=t)
              lend = __LINE__
              print*, d , dcol , t , filename , lend  
              print*,  footer
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
        end subroutine unit_test_alloc_ionosphere_state_r8

end module mod_test_alloc_ionosphere_state


program main 
   use mod_test_alloc_ionosphere_state
   call unit_test_alloc_ionosphere_state_r4()
   call unit_test_alloc_ionosphere_state_r8()
end program main 