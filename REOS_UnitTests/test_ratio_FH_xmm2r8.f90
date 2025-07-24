
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

module mod_test_ratio_FH_xmm2r8


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
    ifort -o test_ratio_FH_xmm2r8 -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
    -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90 GMS_vectypes.f90 GMS_eos_sensor_sse.f90 GMS_intrinsics_wrappers.o test_ratio_FH_xmm2r8.f90
    -------------------------------------------------------------------------------------------------------------------------------------------------
    ifx -o test_ratio_FH_xmm2r8 -fp-model fast=2 -ftz -O3  -march=skylake-avx512 \     
    -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=3  GMS_kinds.f90 GMS_vectypes.f90 GMS_eos_sensor_sse.f90 GMS_intrinsics_wrappers.o test_ratio_FH_xmm2r8.f90

    For assembly only:
    ifort -S   -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90 GMS_vectypes.f90 GMS_eos_sensor_sse.f90 GMS_intrinsics_wrappers.o test_ratio_FH_xmm2r8.f90
#endif

      contains 


subroutine unit_test_ratio_FH_xmm2r8()
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
              character(len=60),         parameter  :: header = "[TEST #1:  ratio_FH_xmm2r8 -- START]"
              character(len=60),         parameter  :: footer = "[TEST #1:  ratio_FH_xmm2r8 -- END]  "
              character(len=40),         parameter  :: OUTFILE = "OUTPUT_ratio_FH_xmm2r8.dat"
              
              type(XMM2r8_t), allocatable, dimension(:) :: phi 
              type(XMM2r8_t), allocatable, dimension(:) :: psi 
              type(XMM2r8_t), allocatable, dimension(:,:) :: FH 
              !dir$ attributes align : 16 :: phi 
              !dir$ attributes align : 16 :: psi 
              !dir$ attributes align : 16 :: FH
              type(XMM2r8_t),            automatic ::  rand_phi 
              type(XMM2r8_t),            automatic ::  rand_psi 
              !dir$ attributes align : 16 :: rand_phi 
              !dir$ attributes align : 16 :: rand_psi
              real(kind=dp),             automatic  :: rnum,tmp  
              integer(kind=i4),          automatic  :: nvecs 
              integer(kind=i4),          automatic  :: i__,j__ 
              integer(kind=i4),          parameter  :: iounit = 102 
              integer(kind=i4),          automatic  :: ioerr
              logical(kind=i1),          automatic  :: ioflag 
              integer(kind=c_long_long), automatic  :: start,end 
              integer(kind=c_long_long), automatic  :: start_c,end_c 
              integer(kind=c_long_long), automatic  :: tsc_elapsed 
              
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
              allocate(psi(nvecs))
              allocate(FH(5,nvecs))
             
             
              call random_init(.false.,.false.)
              do i__=1, nvecs 
                 call random_number(rand_phi.v)
                 phi(i__)  = rand_phi 
                
                 call random_number(rand_psi.v)
                 psi(i__) = rand_psi
                 
              end do 

              do j__=0,4
                 start = rdtsc_wrap()
                 call ratio_FH_dispatch_xmm2r8(psi,phi,FH(j__,:),nvecs,j__)
                 end         = rdtsc_wrap()
                 start_c     = start-RDTSC_LATENCY
                 end_c       = end-RDTSC_LATENCY
                 tsc_elapsed = end_c-start_c 
                 if(tsc_elapsed<ZERO) then
                     print*,"[INVALID-TSC]=", tsc_elapsed
                 else 
                     print*, "[WARNING]: Crude timing measurement!!"
                     print*,"[TSC]=",tsc_elapsed, "for case: #",j__ 
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
                 write(IOUNIT,'(A60)') "[OUTPUT-START]: ratio_FH_xmm2r8 -- field"
                 do j__=0,4
                    write(IOUNIT,'(T14,A3,T36,A3)') "v0","v1"
                    do i__=1,nvecs                                                                                                                                           
                       write(IOUNIT,'(2F22.15)') FH(j__,i__).v(0),FH(j__,i__).v(1)
                    end do 
                 end do 
                 write(IOUNIT,'(A60)') "[OUTPUT-END]:   ratio_FH_xmm2r8 -- field"
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
end subroutine unit_test_ratio_FH_xmm2r8 





end module mod_test_ratio_FH_xmm2r8



program main 
   use mod_test_ratio_FH_xmm2r8
   call unit_test_ratio_FH_xmm2r8()
end program main 