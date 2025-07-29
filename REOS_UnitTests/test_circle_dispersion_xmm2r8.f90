
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

module mod_test_circle_dispersion_xmm2r8


       use mod_kinds,                 only : i1, i4, sp 
       use iso_c_binding,             only : c_int, c_long_long 
       use mod_vectypes,              only : XMM2r8_t, Mask2_t
       use eos_sensor_sse
       implicit none 

       integer(kind=c_long_long),       parameter :: RDTSC_LATENCY = 18 ! for Skylake arch.
       integer(kind=c_long_long),       parameter :: ZERO          = 0_c_long_long 
       

#if 0
    ICC and ifort commands
    icc -c -std=c99 GMS_intrinsics_wrappers.c
    ifort -o test_circle_dispersion_xmm2r8 -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
    -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90 GMS_vectypes.f90 GMS_eos_sensor_sse.f90 GMS_intrinsics_wrappers.o test_circle_dispersion_xmm2r8.f90
    -------------------------------------------------------------------------------------------------------------------------------------------------
    ifx -o test_circle_dispersion_xmm2r8 -fp-model fast=2 -ftz -O3  -march=skylake-avx512 \     
    -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=3  GMS_kinds.f90 GMS_vectypes.f90 GMS_eos_sensor_sse.f90 GMS_intrinsics_wrappers.o test_circle_dispersion_xmm2r8.f90

    For assembly only:
    ifort -S   -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90 GMS_vectypes.f90 GMS_eos_sensor_sse.f90 GMS_intrinsics_wrappers.o test_circle_dispersion_xmm2r8.f90
#endif

      contains 


subroutine unit_test_circle_dispersion_xmm2r8()
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
              character(len=128),        automatic  :: emsg 
              character(len=60),         parameter  :: header = "[TEST #1:  circle_dispersion_xmm2r8 -- START]"
              character(len=60),         parameter  :: footer = "[TEST #1:  circle_dispersion_xmm2r8 -- END]  "
              character(len=40),         parameter  :: OUTFILE = "OUTPUT_circle_dispersion_xmm2r8.dat"
              
              
              type(XMM2r8_t), allocatable, dimension(:) :: alpha
              type(XMM2r8_t), allocatable, dimension(:,:) :: rho
              !dir$ attributes align : 16 :: alpha
              !dir$ attributes align : 16 :: rho 
              type(XMM2r8_t),            automatic ::  rand_l2
              type(XMM2r8_t),            automatic ::  rand_l1
              type(XMM2r8_t),            automatic ::  rand_alpha
              type(XMM2r8_t),            automatic ::  rand_O
              type(XMM2r8_t),            parameter ::  d      = XMM2r8_t(50.0_dp)
              type(XMM2r8_t),            parameter ::  ref100 = XMM2r8_t(100.0_dp)
              type(XMM2r8_t),            parameter ::  lo_bound = XMM2r8_t(0.1_dp)
              type(XMM2r8_t),            parameter ::  vzero   = XMM2r8_t(0.0_dp)
              !dir$ attributes align : 16 :: rand_l2
              !dir$ attributes align : 16 :: rand_alpha
              !dir$ attributes align : 16 :: rand_O
              !dir$ attributes align : 16 :: d 
              !dir$ attributes align : 16 :: ref100 
              !dir$ attributes align : 16 :: vzero 
              !dir$ attributes align : 16 :: lo_bound 
              type(Mask2_t),             automatic  :: m0_eq_zero,m1_eq_zero,m2_eq_zero
              integer(kind=c_long_long), automatic  :: start,end 
              integer(kind=c_long_long), automatic  :: start_c,end_c 
              integer(kind=c_long_long), automatic  :: tsc_elapsed 
              integer(kind=i4),          automatic  :: nvecs 
              integer(kind=i4),          automatic  :: i__,j__ 
              integer(kind=i4),          parameter  :: iounit = 102 
              integer(kind=i4),          automatic  :: ioerr
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
              nvecs = set_random_size()
              allocate(alpha(nvecs))
              allocate(rho(5,nvecs))
                    
              call random_init(.false.,.false.)
              m0_eq_zero.m = .false.
              m1_eq_zero.m = .false. 
              m2_eq_zero.m = .false. 
              call random_number(rand_l2.v)
              m0_eq_zero.m = (rand_l2.v==vzero.v)
              where(m0_eq_zero.m) 
                      rand_l2.v = lo_bound.v 
              else where 
                      rand_l2.v = rand_l2.v*ref100.v
              end where 

              m2_eq_zero.m = (rand_l1.v==vzero.v)
              where(m2_eq_zero.m) 
                      rand_l1.v = lo_bound.v 
              else where 
                      rand_l1.v = rand_l1.v*ref100.v
              end where 

              call random_number(rand_O.v)
              m1_eq_zero.m = (rand_O.v==vzero.v)
              where(m1_eq_zero.m) 
                       rand_O.v = lo_bound.v 
              end where 

              do i__=1, nvecs 
                 call random_number(rand_alpha.v)
                 alpha(i__) = rand_alpha
              end do 

              do j__=0,4
                 start = rdtsc_wrap()
                 call circle_dispersion_dispatch_xmm2r8(d,rand_l1,rand_l2,alpha,rand_O,.true.,rho(j__,:),nvecs,j__)
                 end         = rdtsc_wrap()
                 start_c     = start-RDTSC_LATENCY
                 end_c       = end-RDTSC_LATENCY
                 tsc_elapsed = end_c-start_c 
                 if(tsc_elapsed<ZERO) then
                     print*,"[INVALID-TSC]=", tsc_elapsed
                 else 
                     print*, "[WARNING]: Crude timing measurement!!"
                     print*,"[TSC]=",tsc_elapsed, "for case: #", j__ 
                 end if 
              end do 
              
#if 1 
              ioerr = 0
              open(UNIT=IOUNIT,FILE=OUTFILE,IOMSG=emsg,ACCESS="SEQUENTIAL",STATUS="NEW")
              ioflag = (ioerr==0)
              if(.not.ioflag) then 
                 print*, "[ERROR] -- OPEN failed an error message is as follows:"
                 print*, emsg 
                 return 
              else 
                 write(IOUNIT,'(A60)') "[OUTPUT-START]: circle_dispersion_xmm2r8 -- field"
                 do j__=0,4
                     write(IOUNIT,'(T14,A3,T36,A3)') "v0","v1"
                     do i__=1,nvecs                                                                                                                                           
                         write(IOUNIT,'(4F22.15)') rho(j__,i__).v(0),rho(j__,i__).v(1)
                     end do 
                 end do 
                 write(IOUNIT,'(A60)') "[OUTPUT-END]:   circle_dispersion_xmm2r8 -- field"
              end if  
              close(IOUNIT,STATUS='KEEP')
#endif             
             
              print*, footer                
              contains 

              function set_random_size() result(rval)
                       implicit none 
                       integer(kind=i4), automatic :: rnum 
                       integer(kind=i4), parameter :: lo = 1024
                       integer(kind=i4)            :: rval 
                       rnum = irand(0)
                       if(rnum<lo) then 
                          rnum=lo
                          rval=rnum
                       end if 
                       rval=rnum 
               end function set_random_size
               
end subroutine unit_test_circle_dispersion_xmm2r8





end module mod_test_circle_dispersion_xmm2r8 



program main 
   use mod_test_circle_dispersion_xmm2r8 
   call unit_test_circle_dispersion_xmm2r8()
end program main 