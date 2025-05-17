
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

module mod_test_alloc_atmos_refraction

       use mod_kinds, only : i1, i4, i8, sp, dp 
       use atmos_refraction 

       character(len=1),        parameter, private :: dcol = ":"

#if 0
 Ifort command line: 
 ifort -o unit_test_alloc_atmos_refraction -fp-model fast=2 -ftz -O3 -ggdb -ipo -march=skylake-avx512 -fopenmp -qopenmp -fpp 
 -falign-functions=32 GMS_kinds.f90 GMS_atmos_refraction.f90 test_alloc_atmos_refraction.f90
#endif


       contains 

       subroutine unit_test_alloc_atmos_refraction_state_r4_omp()
                use iso_c_binding, only : c_int 
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
              type(atmos_refraction_state_r4_t),    automatic :: atmos_refract_state 
              type(atmos_refraction_size_params_t), automatic :: size_params 
              character(len=128),                   automatic :: filename 
              integer(c_int),                       parameter :: SIGTRAP = 5 
              character(len=10)                               :: t
              character(len=8)                                :: d
              character(len=64),                    parameter  :: header = "[TEST #15: alloc_atmos_refraction_state_r4_omp -- START]"
              character(len=64),                    parameter  :: footer = "[TEST #15: alloc_atmos_refraction_state_r4_omp -- END]"
              call DATE_AND_TIME(date=d,time=t)
              filename = __FILE__ 
              lstart   = __LINE__ 
              print*, d , dcol , t , filename , lstart
              print*,  header
              size_params = atmos_refraction_size_params_t(set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size())
#if 0
              print*, raise(SIGTRAP)
#endif        
print*, "================================ ALLOCATION ================================="      
              call alloc_atmos_refraction_state_r4_omp(atmos_refract_state,size_params)

              call check_allocation_stats(atmos_refract_state.data_nidx)
              call check_allocation_stats(atmos_refract_state.data_n0idx)
              call check_allocation_stats(atmos_refract_state.data_z)
              call check_allocation_stats(atmos_refract_state.data_r)
              call check_allocation_stats(atmos_refract_state.data_R0)
              call check_allocation_stats(atmos_refract_state.data_phi)
              call check_allocation_stats(atmos_refract_state.data_phi0)
              call check_allocation_stats(atmos_refract_state.data_ntht)
              call check_allocation_stats(atmos_refract_state.data_nphi)
              call check_allocation_stats(atmos_refract_state.data_dndr)
              call check_allocation_stats(atmos_refract_state.data_rho)
              call check_allocation_stats(atmos_refract_state.data_dn0)
              call check_allocation_stats(atmos_refract_state.data_beta)
              call check_allocation_stats(atmos_refract_state.data_navgh)
              call check_allocation_stats(atmos_refract_state.data_H)
              call check_allocation_stats(atmos_refract_state.data_f345)
              call check_allocation_stats(atmos_refract_state.data_f351)
              call check_allocation_stats(atmos_refract_state.data_f352)
              call check_allocation_stats(atmos_refract_state.data_nh)
              call check_allocation_stats(atmos_refract_state.data_f41)
              call check_allocation_stats(atmos_refract_state.data_f)
              call check_allocation_stats(atmos_refract_state.data_d)
              call check_allocation_stats(atmos_refract_state.data_Nmf)
              call check_allocation_stats(atmos_refract_state.data_f412)
              call check_allocation_stats(atmos_refract_state.data_f413)
              call check_allocation_stats(atmos_refract_state.data_D1)
              call check_allocation_stats(atmos_refract_state.data_f415)
              call check_allocation_stats(atmos_refract_state.data_f425)
              call check_allocation_stats(atmos_refract_state.data_f428)
              call check_allocation_stats(atmos_refract_state.data_f429)
              call check_allocation_stats(atmos_refract_state.data_H1)
              call check_allocation_stats(atmos_refract_state.data_H2)
              call check_allocation_stats(atmos_refract_state.data_f430)
              call check_allocation_stats(atmos_refract_state.data_H3)
              call check_allocation_stats(atmos_refract_state.data_deln0)
              call check_allocation_stats(atmos_refract_state.data_f431)
              call check_allocation_stats(atmos_refract_state.data_f438)
              call check_allocation_stats(atmos_refract_state.data_f442)
              call check_allocation_stats(atmos_refract_state.data_f445)
              call check_allocation_stats(atmos_refract_state.data_g)
              call check_allocation_stats(atmos_refract_state.data_f450)
              call check_allocation_stats(atmos_refract_state.data_f451)
              call check_allocation_stats(atmos_refract_state.data_Hc0)
              call check_allocation_stats(atmos_refract_state.data_delnA)
              call check_allocation_stats(atmos_refract_state.data_na)
              call check_allocation_stats(atmos_refract_state.data_nc)
              call check_allocation_stats(atmos_refract_state.data_f53)
              call check_allocation_stats(atmos_refract_state.data_f517)
              call check_allocation_stats(atmos_refract_state.data_tht)
              call check_allocation_stats(atmos_refract_state.data_f531)
              call check_allocation_stats(atmos_refract_state.data_thtc)
              call check_allocation_stats(atmos_refract_state.data_f534)
              call check_allocation_stats(atmos_refract_state.data_Rc)
              call check_allocation_stats(atmos_refract_state.data_f535)
              call check_allocation_stats(atmos_refract_state.data_f538)
              call check_allocation_stats(atmos_refract_state.data_f539)
              call check_allocation_stats(atmos_refract_state.data_f541)
              call check_allocation_stats(atmos_refract_state.data_H0)
              call check_allocation_stats(atmos_refract_state.data_Hc)
              call check_allocation_stats(atmos_refract_state.data_f543)
              call check_allocation_stats(atmos_refract_state.data_H10)
              call check_allocation_stats(atmos_refract_state.data_f554)
              call check_allocation_stats(atmos_refract_state.data_H20)
              call check_allocation_stats(atmos_refract_state.data_f572)
              call check_allocation_stats(atmos_refract_state.data_f576)
              call check_allocation_stats(atmos_refract_state.data_f577)
              call check_allocation_stats(atmos_refract_state.data_f578)
              call check_allocation_stats(atmos_refract_state.data_f579)
              call check_allocation_stats(atmos_refract_state.data_f590)
              call check_allocation_stats(atmos_refract_state.data_R2)
              call check_allocation_stats(atmos_refract_state.data_f591)
              call check_allocation_stats(atmos_refract_state.data_f593)
              call check_allocation_stats(atmos_refract_state.data_f595)
              call check_allocation_stats(atmos_refract_state.data_HB)
              call check_allocation_stats(atmos_refract_state.data_f62)
              call check_allocation_stats(atmos_refract_state.data_HC2)
              call check_allocation_stats(atmos_refract_state.data_f66)
              call check_allocation_stats(atmos_refract_state.data_f61)
              call check_allocation_stats(atmos_refract_state.data_f61b)
              call check_allocation_stats(atmos_refract_state.data_Bf61b)
              call check_allocation_stats(atmos_refract_state.data_f619)
              call check_allocation_stats(atmos_refract_state.data_Lc)
              call check_allocation_stats(atmos_refract_state.data_Lb)
              call check_allocation_stats(atmos_refract_state.data_gamma)
              call check_allocation_stats(atmos_refract_state.data_f618)
              call check_allocation_stats(atmos_refract_state.data_f620)
              call check_allocation_stats(atmos_refract_state.data_f621)
              call check_allocation_stats(atmos_refract_state.data_Lh)
              call check_allocation_stats(atmos_refract_state.data_f622)
              call check_allocation_stats(atmos_refract_state.data_f623)
              call check_allocation_stats(atmos_refract_state.data_f625)
              call check_allocation_stats(atmos_refract_state.data_f627)
              call check_allocation_stats(atmos_refract_state.data_Hh)
              call check_allocation_stats(atmos_refract_state.data_f72)
              call check_allocation_stats(atmos_refract_state.data_delnb)
              call check_allocation_stats(atmos_refract_state.data_delnc)
              call check_allocation_stats(atmos_refract_state.data_delnh)
              call check_allocation_stats(atmos_refract_state.data_f74)
              call check_allocation_stats(atmos_refract_state.data_f75)
              call check_allocation_stats(atmos_refract_state.data_f714)
              call check_allocation_stats(atmos_refract_state.data_1f714)
              call check_allocation_stats(atmos_refract_state.data_f739)
              call check_allocation_stats(atmos_refract_state.data_f741)
              call check_allocation_stats(atmos_refract_state.data_f743)
              call check_allocation_stats(atmos_refract_state.data_f744)
              call check_allocation_stats(atmos_refract_state.data_f747)
              call check_allocation_stats(atmos_refract_state.data_f96)
              call check_allocation_stats(atmos_refract_state.data_f910)
              call check_allocation_stats(atmos_refract_state.data_f2f96)
              call check_allocation_stats(atmos_refract_state.data_f914)
              call check_allocation_stats(atmos_refract_state.data_L)
              call check_allocation_stats(atmos_refract_state.data_f924)
              print*, "===================== DEALLOCATION ==========================="
#if 0
              print*, raise(SIGTRAP)
#endif 
              call dealloc_atmos_refraction_state_r4_omp(atmos_refract_state)

              call check_allocation_stats(atmos_refract_state.data_nidx)
              call check_allocation_stats(atmos_refract_state.data_n0idx)
              call check_allocation_stats(atmos_refract_state.data_z)
              call check_allocation_stats(atmos_refract_state.data_r)
              call check_allocation_stats(atmos_refract_state.data_R0)
              call check_allocation_stats(atmos_refract_state.data_phi)
              call check_allocation_stats(atmos_refract_state.data_phi0)
              call check_allocation_stats(atmos_refract_state.data_ntht)
              call check_allocation_stats(atmos_refract_state.data_nphi)
              call check_allocation_stats(atmos_refract_state.data_dndr)
              call check_allocation_stats(atmos_refract_state.data_rho)
              call check_allocation_stats(atmos_refract_state.data_dn0)
              call check_allocation_stats(atmos_refract_state.data_beta)
              call check_allocation_stats(atmos_refract_state.data_navgh)
              call check_allocation_stats(atmos_refract_state.data_H)
              call check_allocation_stats(atmos_refract_state.data_f345)
              call check_allocation_stats(atmos_refract_state.data_f351)
              call check_allocation_stats(atmos_refract_state.data_f352)
              call check_allocation_stats(atmos_refract_state.data_nh)
              call check_allocation_stats(atmos_refract_state.data_f41)
              call check_allocation_stats(atmos_refract_state.data_f)
              call check_allocation_stats(atmos_refract_state.data_d)
              call check_allocation_stats(atmos_refract_state.data_Nmf)
              call check_allocation_stats(atmos_refract_state.data_f412)
              call check_allocation_stats(atmos_refract_state.data_f413)
              call check_allocation_stats(atmos_refract_state.data_D1)
              call check_allocation_stats(atmos_refract_state.data_f415)
              call check_allocation_stats(atmos_refract_state.data_f425)
              call check_allocation_stats(atmos_refract_state.data_f428)
              call check_allocation_stats(atmos_refract_state.data_f429)
              call check_allocation_stats(atmos_refract_state.data_H1)
              call check_allocation_stats(atmos_refract_state.data_H2)
              call check_allocation_stats(atmos_refract_state.data_f430)
              call check_allocation_stats(atmos_refract_state.data_H3)
              call check_allocation_stats(atmos_refract_state.data_deln0)
              call check_allocation_stats(atmos_refract_state.data_f431)
              call check_allocation_stats(atmos_refract_state.data_f438)
              call check_allocation_stats(atmos_refract_state.data_f442)
              call check_allocation_stats(atmos_refract_state.data_f445)
              call check_allocation_stats(atmos_refract_state.data_g)
              call check_allocation_stats(atmos_refract_state.data_f450)
              call check_allocation_stats(atmos_refract_state.data_f451)
              call check_allocation_stats(atmos_refract_state.data_Hc0)
              call check_allocation_stats(atmos_refract_state.data_delnA)
              call check_allocation_stats(atmos_refract_state.data_na)
              call check_allocation_stats(atmos_refract_state.data_nc)
              call check_allocation_stats(atmos_refract_state.data_f53)
              call check_allocation_stats(atmos_refract_state.data_f517)
              call check_allocation_stats(atmos_refract_state.data_tht)
              call check_allocation_stats(atmos_refract_state.data_f531)
              call check_allocation_stats(atmos_refract_state.data_thtc)
              call check_allocation_stats(atmos_refract_state.data_f534)
              call check_allocation_stats(atmos_refract_state.data_Rc)
              call check_allocation_stats(atmos_refract_state.data_f535)
              call check_allocation_stats(atmos_refract_state.data_f538)
              call check_allocation_stats(atmos_refract_state.data_f539)
              call check_allocation_stats(atmos_refract_state.data_f541)
              call check_allocation_stats(atmos_refract_state.data_H0)
              call check_allocation_stats(atmos_refract_state.data_Hc)
              call check_allocation_stats(atmos_refract_state.data_f543)
              call check_allocation_stats(atmos_refract_state.data_H10)
              call check_allocation_stats(atmos_refract_state.data_f554)
              call check_allocation_stats(atmos_refract_state.data_H20)
              call check_allocation_stats(atmos_refract_state.data_f572)
              call check_allocation_stats(atmos_refract_state.data_f576)
              call check_allocation_stats(atmos_refract_state.data_f577)
              call check_allocation_stats(atmos_refract_state.data_f578)
              call check_allocation_stats(atmos_refract_state.data_f579)
              call check_allocation_stats(atmos_refract_state.data_f590)
              call check_allocation_stats(atmos_refract_state.data_R2)
              call check_allocation_stats(atmos_refract_state.data_f591)
              call check_allocation_stats(atmos_refract_state.data_f593)
              call check_allocation_stats(atmos_refract_state.data_f595)
              call check_allocation_stats(atmos_refract_state.data_HB)
              call check_allocation_stats(atmos_refract_state.data_f62)
              call check_allocation_stats(atmos_refract_state.data_HC2)
              call check_allocation_stats(atmos_refract_state.data_f66)
              call check_allocation_stats(atmos_refract_state.data_f61)
              call check_allocation_stats(atmos_refract_state.data_f61b)
              call check_allocation_stats(atmos_refract_state.data_Bf61b)
              call check_allocation_stats(atmos_refract_state.data_f619)
              call check_allocation_stats(atmos_refract_state.data_Lc)
              call check_allocation_stats(atmos_refract_state.data_Lb)
              call check_allocation_stats(atmos_refract_state.data_gamma)
              call check_allocation_stats(atmos_refract_state.data_f618)
              call check_allocation_stats(atmos_refract_state.data_f620)
              call check_allocation_stats(atmos_refract_state.data_f621)
              call check_allocation_stats(atmos_refract_state.data_Lh)
              call check_allocation_stats(atmos_refract_state.data_f622)
              call check_allocation_stats(atmos_refract_state.data_f623)
              call check_allocation_stats(atmos_refract_state.data_f625)
              call check_allocation_stats(atmos_refract_state.data_f627)
              call check_allocation_stats(atmos_refract_state.data_Hh)
              call check_allocation_stats(atmos_refract_state.data_f72)
              call check_allocation_stats(atmos_refract_state.data_delnb)
              call check_allocation_stats(atmos_refract_state.data_delnc)
              call check_allocation_stats(atmos_refract_state.data_delnh)
              call check_allocation_stats(atmos_refract_state.data_f74)
              call check_allocation_stats(atmos_refract_state.data_f75)
              call check_allocation_stats(atmos_refract_state.data_f714)
              call check_allocation_stats(atmos_refract_state.data_1f714)
              call check_allocation_stats(atmos_refract_state.data_f739)
              call check_allocation_stats(atmos_refract_state.data_f741)
              call check_allocation_stats(atmos_refract_state.data_f743)
              call check_allocation_stats(atmos_refract_state.data_f744)
              call check_allocation_stats(atmos_refract_state.data_f747)
              call check_allocation_stats(atmos_refract_state.data_f96)
              call check_allocation_stats(atmos_refract_state.data_f910)
              call check_allocation_stats(atmos_refract_state.data_f2f96)
              call check_allocation_stats(atmos_refract_state.data_f914)
              call check_allocation_stats(atmos_refract_state.data_L)
              call check_allocation_stats(atmos_refract_state.data_f924)
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
end subroutine unit_test_alloc_atmos_refraction_state_r4_omp


 subroutine unit_test_alloc_atmos_refraction_state_r8_omp()
                use iso_c_binding, only : c_int 
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
              type(atmos_refraction_state_r8_t),    automatic :: atmos_refract_state 
              type(atmos_refraction_size_params_t), automatic :: size_params 
              character(len=128),                   automatic :: filename 
              integer(c_int),                       parameter :: SIGTRAP = 5 
              character(len=10)                               :: t
              character(len=8)                                :: d
              character(len=64),                    parameter  :: header = "[TEST #16: alloc_atmos_refraction_state_r8_omp -- START]"
              character(len=64),                    parameter  :: footer = "[TEST #16: alloc_atmos_refraction_state_r8_omp -- END]"
              call DATE_AND_TIME(date=d,time=t)
              filename = __FILE__ 
              lstart   = __LINE__ 
              print*, d , dcol , t , filename , lstart
              print*,  header
              size_params = atmos_refraction_size_params_t(set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size(),set_random_size(), &
                                                           set_random_size())
#if 0
              print*, raise(SIGTRAP)
#endif        
print*, "================================ ALLOCATION ================================="      
              call alloc_atmos_refraction_state_r8_omp(atmos_refract_state,size_params)

              call check_allocation_stats(atmos_refract_state.data_nidx)
              call check_allocation_stats(atmos_refract_state.data_n0idx)
              call check_allocation_stats(atmos_refract_state.data_z)
              call check_allocation_stats(atmos_refract_state.data_r)
              call check_allocation_stats(atmos_refract_state.data_R0)
              call check_allocation_stats(atmos_refract_state.data_phi)
              call check_allocation_stats(atmos_refract_state.data_phi0)
              call check_allocation_stats(atmos_refract_state.data_ntht)
              call check_allocation_stats(atmos_refract_state.data_nphi)
              call check_allocation_stats(atmos_refract_state.data_dndr)
              call check_allocation_stats(atmos_refract_state.data_rho)
              call check_allocation_stats(atmos_refract_state.data_dn0)
              call check_allocation_stats(atmos_refract_state.data_beta)
              call check_allocation_stats(atmos_refract_state.data_navgh)
              call check_allocation_stats(atmos_refract_state.data_H)
              call check_allocation_stats(atmos_refract_state.data_f345)
              call check_allocation_stats(atmos_refract_state.data_f351)
              call check_allocation_stats(atmos_refract_state.data_f352)
              call check_allocation_stats(atmos_refract_state.data_nh)
              call check_allocation_stats(atmos_refract_state.data_f41)
              call check_allocation_stats(atmos_refract_state.data_f)
              call check_allocation_stats(atmos_refract_state.data_d)
              call check_allocation_stats(atmos_refract_state.data_Nmf)
              call check_allocation_stats(atmos_refract_state.data_f412)
              call check_allocation_stats(atmos_refract_state.data_f413)
              call check_allocation_stats(atmos_refract_state.data_D1)
              call check_allocation_stats(atmos_refract_state.data_f415)
              call check_allocation_stats(atmos_refract_state.data_f425)
              call check_allocation_stats(atmos_refract_state.data_f428)
              call check_allocation_stats(atmos_refract_state.data_f429)
              call check_allocation_stats(atmos_refract_state.data_H1)
              call check_allocation_stats(atmos_refract_state.data_H2)
              call check_allocation_stats(atmos_refract_state.data_f430)
              call check_allocation_stats(atmos_refract_state.data_H3)
              call check_allocation_stats(atmos_refract_state.data_deln0)
              call check_allocation_stats(atmos_refract_state.data_f431)
              call check_allocation_stats(atmos_refract_state.data_f438)
              call check_allocation_stats(atmos_refract_state.data_f442)
              call check_allocation_stats(atmos_refract_state.data_f445)
              call check_allocation_stats(atmos_refract_state.data_g)
              call check_allocation_stats(atmos_refract_state.data_f450)
              call check_allocation_stats(atmos_refract_state.data_f451)
              call check_allocation_stats(atmos_refract_state.data_Hc0)
              call check_allocation_stats(atmos_refract_state.data_delnA)
              call check_allocation_stats(atmos_refract_state.data_na)
              call check_allocation_stats(atmos_refract_state.data_nc)
              call check_allocation_stats(atmos_refract_state.data_f53)
              call check_allocation_stats(atmos_refract_state.data_f517)
              call check_allocation_stats(atmos_refract_state.data_tht)
              call check_allocation_stats(atmos_refract_state.data_f531)
              call check_allocation_stats(atmos_refract_state.data_thtc)
              call check_allocation_stats(atmos_refract_state.data_f534)
              call check_allocation_stats(atmos_refract_state.data_Rc)
              call check_allocation_stats(atmos_refract_state.data_f535)
              call check_allocation_stats(atmos_refract_state.data_f538)
              call check_allocation_stats(atmos_refract_state.data_f539)
              call check_allocation_stats(atmos_refract_state.data_f541)
              call check_allocation_stats(atmos_refract_state.data_H0)
              call check_allocation_stats(atmos_refract_state.data_Hc)
              call check_allocation_stats(atmos_refract_state.data_f543)
              call check_allocation_stats(atmos_refract_state.data_H10)
              call check_allocation_stats(atmos_refract_state.data_f554)
              call check_allocation_stats(atmos_refract_state.data_H20)
              call check_allocation_stats(atmos_refract_state.data_f572)
              call check_allocation_stats(atmos_refract_state.data_f576)
              call check_allocation_stats(atmos_refract_state.data_f577)
              call check_allocation_stats(atmos_refract_state.data_f578)
              call check_allocation_stats(atmos_refract_state.data_f579)
              call check_allocation_stats(atmos_refract_state.data_f590)
              call check_allocation_stats(atmos_refract_state.data_R2)
              call check_allocation_stats(atmos_refract_state.data_f591)
              call check_allocation_stats(atmos_refract_state.data_f593)
              call check_allocation_stats(atmos_refract_state.data_f595)
              call check_allocation_stats(atmos_refract_state.data_HB)
              call check_allocation_stats(atmos_refract_state.data_f62)
              call check_allocation_stats(atmos_refract_state.data_HC2)
              call check_allocation_stats(atmos_refract_state.data_f66)
              call check_allocation_stats(atmos_refract_state.data_f61)
              call check_allocation_stats(atmos_refract_state.data_f61b)
              call check_allocation_stats(atmos_refract_state.data_Bf61b)
              call check_allocation_stats(atmos_refract_state.data_f619)
              call check_allocation_stats(atmos_refract_state.data_Lc)
              call check_allocation_stats(atmos_refract_state.data_Lb)
              call check_allocation_stats(atmos_refract_state.data_gamma)
              call check_allocation_stats(atmos_refract_state.data_f618)
              call check_allocation_stats(atmos_refract_state.data_f620)
              call check_allocation_stats(atmos_refract_state.data_f621)
              call check_allocation_stats(atmos_refract_state.data_Lh)
              call check_allocation_stats(atmos_refract_state.data_f622)
              call check_allocation_stats(atmos_refract_state.data_f623)
              call check_allocation_stats(atmos_refract_state.data_f625)
              call check_allocation_stats(atmos_refract_state.data_f627)
              call check_allocation_stats(atmos_refract_state.data_Hh)
              call check_allocation_stats(atmos_refract_state.data_f72)
              call check_allocation_stats(atmos_refract_state.data_delnb)
              call check_allocation_stats(atmos_refract_state.data_delnc)
              call check_allocation_stats(atmos_refract_state.data_delnh)
              call check_allocation_stats(atmos_refract_state.data_f74)
              call check_allocation_stats(atmos_refract_state.data_f75)
              call check_allocation_stats(atmos_refract_state.data_f714)
              call check_allocation_stats(atmos_refract_state.data_1f714)
              call check_allocation_stats(atmos_refract_state.data_f739)
              call check_allocation_stats(atmos_refract_state.data_f741)
              call check_allocation_stats(atmos_refract_state.data_f743)
              call check_allocation_stats(atmos_refract_state.data_f744)
              call check_allocation_stats(atmos_refract_state.data_f747)
              call check_allocation_stats(atmos_refract_state.data_f96)
              call check_allocation_stats(atmos_refract_state.data_f910)
              call check_allocation_stats(atmos_refract_state.data_f2f96)
              call check_allocation_stats(atmos_refract_state.data_f914)
              call check_allocation_stats(atmos_refract_state.data_L)
              call check_allocation_stats(atmos_refract_state.data_f924)
              print*, "===================== DEALLOCATION ==========================="
#if 0
              print*, raise(SIGTRAP)
#endif 
              call dealloc_atmos_refraction_state_r8_omp(atmos_refract_state)

              call check_allocation_stats(atmos_refract_state.data_nidx)
              call check_allocation_stats(atmos_refract_state.data_n0idx)
              call check_allocation_stats(atmos_refract_state.data_z)
              call check_allocation_stats(atmos_refract_state.data_r)
              call check_allocation_stats(atmos_refract_state.data_R0)
              call check_allocation_stats(atmos_refract_state.data_phi)
              call check_allocation_stats(atmos_refract_state.data_phi0)
              call check_allocation_stats(atmos_refract_state.data_ntht)
              call check_allocation_stats(atmos_refract_state.data_nphi)
              call check_allocation_stats(atmos_refract_state.data_dndr)
              call check_allocation_stats(atmos_refract_state.data_rho)
              call check_allocation_stats(atmos_refract_state.data_dn0)
              call check_allocation_stats(atmos_refract_state.data_beta)
              call check_allocation_stats(atmos_refract_state.data_navgh)
              call check_allocation_stats(atmos_refract_state.data_H)
              call check_allocation_stats(atmos_refract_state.data_f345)
              call check_allocation_stats(atmos_refract_state.data_f351)
              call check_allocation_stats(atmos_refract_state.data_f352)
              call check_allocation_stats(atmos_refract_state.data_nh)
              call check_allocation_stats(atmos_refract_state.data_f41)
              call check_allocation_stats(atmos_refract_state.data_f)
              call check_allocation_stats(atmos_refract_state.data_d)
              call check_allocation_stats(atmos_refract_state.data_Nmf)
              call check_allocation_stats(atmos_refract_state.data_f412)
              call check_allocation_stats(atmos_refract_state.data_f413)
              call check_allocation_stats(atmos_refract_state.data_D1)
              call check_allocation_stats(atmos_refract_state.data_f415)
              call check_allocation_stats(atmos_refract_state.data_f425)
              call check_allocation_stats(atmos_refract_state.data_f428)
              call check_allocation_stats(atmos_refract_state.data_f429)
              call check_allocation_stats(atmos_refract_state.data_H1)
              call check_allocation_stats(atmos_refract_state.data_H2)
              call check_allocation_stats(atmos_refract_state.data_f430)
              call check_allocation_stats(atmos_refract_state.data_H3)
              call check_allocation_stats(atmos_refract_state.data_deln0)
              call check_allocation_stats(atmos_refract_state.data_f431)
              call check_allocation_stats(atmos_refract_state.data_f438)
              call check_allocation_stats(atmos_refract_state.data_f442)
              call check_allocation_stats(atmos_refract_state.data_f445)
              call check_allocation_stats(atmos_refract_state.data_g)
              call check_allocation_stats(atmos_refract_state.data_f450)
              call check_allocation_stats(atmos_refract_state.data_f451)
              call check_allocation_stats(atmos_refract_state.data_Hc0)
              call check_allocation_stats(atmos_refract_state.data_delnA)
              call check_allocation_stats(atmos_refract_state.data_na)
              call check_allocation_stats(atmos_refract_state.data_nc)
              call check_allocation_stats(atmos_refract_state.data_f53)
              call check_allocation_stats(atmos_refract_state.data_f517)
              call check_allocation_stats(atmos_refract_state.data_tht)
              call check_allocation_stats(atmos_refract_state.data_f531)
              call check_allocation_stats(atmos_refract_state.data_thtc)
              call check_allocation_stats(atmos_refract_state.data_f534)
              call check_allocation_stats(atmos_refract_state.data_Rc)
              call check_allocation_stats(atmos_refract_state.data_f535)
              call check_allocation_stats(atmos_refract_state.data_f538)
              call check_allocation_stats(atmos_refract_state.data_f539)
              call check_allocation_stats(atmos_refract_state.data_f541)
              call check_allocation_stats(atmos_refract_state.data_H0)
              call check_allocation_stats(atmos_refract_state.data_Hc)
              call check_allocation_stats(atmos_refract_state.data_f543)
              call check_allocation_stats(atmos_refract_state.data_H10)
              call check_allocation_stats(atmos_refract_state.data_f554)
              call check_allocation_stats(atmos_refract_state.data_H20)
              call check_allocation_stats(atmos_refract_state.data_f572)
              call check_allocation_stats(atmos_refract_state.data_f576)
              call check_allocation_stats(atmos_refract_state.data_f577)
              call check_allocation_stats(atmos_refract_state.data_f578)
              call check_allocation_stats(atmos_refract_state.data_f579)
              call check_allocation_stats(atmos_refract_state.data_f590)
              call check_allocation_stats(atmos_refract_state.data_R2)
              call check_allocation_stats(atmos_refract_state.data_f591)
              call check_allocation_stats(atmos_refract_state.data_f593)
              call check_allocation_stats(atmos_refract_state.data_f595)
              call check_allocation_stats(atmos_refract_state.data_HB)
              call check_allocation_stats(atmos_refract_state.data_f62)
              call check_allocation_stats(atmos_refract_state.data_HC2)
              call check_allocation_stats(atmos_refract_state.data_f66)
              call check_allocation_stats(atmos_refract_state.data_f61)
              call check_allocation_stats(atmos_refract_state.data_f61b)
              call check_allocation_stats(atmos_refract_state.data_Bf61b)
              call check_allocation_stats(atmos_refract_state.data_f619)
              call check_allocation_stats(atmos_refract_state.data_Lc)
              call check_allocation_stats(atmos_refract_state.data_Lb)
              call check_allocation_stats(atmos_refract_state.data_gamma)
              call check_allocation_stats(atmos_refract_state.data_f618)
              call check_allocation_stats(atmos_refract_state.data_f620)
              call check_allocation_stats(atmos_refract_state.data_f621)
              call check_allocation_stats(atmos_refract_state.data_Lh)
              call check_allocation_stats(atmos_refract_state.data_f622)
              call check_allocation_stats(atmos_refract_state.data_f623)
              call check_allocation_stats(atmos_refract_state.data_f625)
              call check_allocation_stats(atmos_refract_state.data_f627)
              call check_allocation_stats(atmos_refract_state.data_Hh)
              call check_allocation_stats(atmos_refract_state.data_f72)
              call check_allocation_stats(atmos_refract_state.data_delnb)
              call check_allocation_stats(atmos_refract_state.data_delnc)
              call check_allocation_stats(atmos_refract_state.data_delnh)
              call check_allocation_stats(atmos_refract_state.data_f74)
              call check_allocation_stats(atmos_refract_state.data_f75)
              call check_allocation_stats(atmos_refract_state.data_f714)
              call check_allocation_stats(atmos_refract_state.data_1f714)
              call check_allocation_stats(atmos_refract_state.data_f739)
              call check_allocation_stats(atmos_refract_state.data_f741)
              call check_allocation_stats(atmos_refract_state.data_f743)
              call check_allocation_stats(atmos_refract_state.data_f744)
              call check_allocation_stats(atmos_refract_state.data_f747)
              call check_allocation_stats(atmos_refract_state.data_f96)
              call check_allocation_stats(atmos_refract_state.data_f910)
              call check_allocation_stats(atmos_refract_state.data_f2f96)
              call check_allocation_stats(atmos_refract_state.data_f914)
              call check_allocation_stats(atmos_refract_state.data_L)
              call check_allocation_stats(atmos_refract_state.data_f924)
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
end subroutine unit_test_alloc_atmos_refraction_state_r8_omp


end module mod_test_alloc_atmos_refraction


program main 
   use mod_test_alloc_atmos_refraction 
   !call unit_test_alloc_atmos_refraction_state_r4_omp()
   call unit_test_alloc_atmos_refraction_state_r8_omp()
end program main 