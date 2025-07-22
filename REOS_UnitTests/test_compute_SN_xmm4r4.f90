
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

module mod_test_compute_SN_xmm4r4


       use mod_kinds,                 only : i1, i4, sp 
       use iso_c_binding,             only : c_int, c_long_long 
       use mod_vectypes,              only : XMM2r8_t, XMM4r4_t
       use eos_sensor_sse
       implicit none 

       integer(kind=c_long_long),       parameter :: RDTSC_LATENCY = 18 ! for Skylake arch.
       integer(kind=c_long_long),       parameter :: ZERO          = 0_c_long_long 
       

#if 0
    ICC and ifort commands
    icc -c -std=c99 GMS_intrinsics_wrappers.c
    ifort -o test_compute_SN_xmm4r4 -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
    -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90 GMS_vectypes.f90 GMS_eos_sensor_sse.f90 GMS_intrinsics_wrappers.o test_compute_SN_xmm4r4.f90
    -------------------------------------------------------------------------------------------------------------------------------------------------
    ifx -o test_compute_SN_xmm4r4 -fp-model fast=2 -ftz -O3  -march=skylake-avx512 \     
    -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=3  GMS_kinds.f90 GMS_vectypes.f90 GMS_eos_sensor_sse.f90 GMS_intrinsics_wrappers.o test_compute_SN_xmm4r4.f90

    For assembly only:
    ifort -S   -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90 GMS_vectypes.f90 GMS_eos_sensor_sse.f90 GMS_intrinsics_wrappers.o test_compute_SN_xmm4r4.f90
#endif

      contains 


subroutine unit_test_compute_SN_xmm4r4()
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
              character(len=60),         parameter  :: header = "[TEST #1:  compute_SN_xmm4r4 -- START]"
              character(len=60),         parameter  :: footer = "[TEST #1:  compute_SN_xmm4r4 -- END]  "
              character(len=40),         parameter  :: OUTFILE = "OUTPUT_compute_SN_xmm4r4.dat"
              
              type(XMM4r4_t), allocatable, dimension(:) :: phi 
              type(XMM4r4_t), allocatable, dimension(:) :: gamma 
              type(XMM4r4_t), allocatable, dimension(:) :: SN 
              !dir$ attributes align : 16 :: phi 
              !dir$ attributes align : 16 :: gamma
              type(XMM4r4_t),            parameter ::  R = XMM4r4_t([8.5_sp,4.25_sp,1.5_sp,10.0_sp])
              type(XMM4r4_t),            automatic ::  rand_R 
              type(XMM4r4_t),            automatic ::  rand_phi 
              type(XMM4r4_t),            automatic ::  rand_gamma 
              !dir$ attributes align : 16 :: R 
              !dir$ attributes align : 16 :: rand_R 
              !dir$ attributes align : 16 :: rand_phi 
              !dir$ attributes align : 16 :: rand_gamma 
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
              allocate(phi(nvecs))
              allocate(gamma(nvecs))
              allocate(SN(nvecs))
                           
              call random_init(.false.,.false.)
              do i__=1, nvecs 
                 call random_number(rand_phi.v)
                 phi(i__)  = rand_phi 
                 call random_number(rand_gamma.v)
                 gamma(i__) = rand_gamma
              end do 

              do j__=0,4
                 start = rdtsc_wrap()
                 call compute_SN_dispatch_xmm4r4(R,phi,gamma,SN,nvecs,j__)
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
                 write(IOUNIT,'(A60)') "[OUTPUT-START]: compute_SN_xmm4r4 -- field"
                 write(IOUNIT,'(T14,A3,T36,A3,T57,A3,T79,A3)') "v0","v1","v2","v3"
                 do i__=1,nvecs                                                                                                                                           
                    write(IOUNIT,'(4F22.15)') SN(i__).v(0),SN(i__).v(1),SN(i__).v(2),SN(i__).v(3)
                 end do 
                 write(IOUNIT,'(A60)') "[OUTPUT-END]:   compute_SN_xmm4r4 -- field"
              end if  
              close(IOUNIT,STATUS='KEEP')
#endif             
              if(allocated(SN))     deallocate(SN)
              if(allocated(gamma))  deallocate(gamma)
              if(allocated(phi))    deallocate(phi)
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
end subroutine unit_test_compute_SN_xmm4r4





end module mod_test_compute_SN_xmm4r4 



program main 
   use mod_test_compute_SN_xmm4r4 
   call unit_test_compute_SN_xmm4r4()
end program main 