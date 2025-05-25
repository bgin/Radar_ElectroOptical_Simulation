
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

module mod_test_func_f924_omp 
       

       use mod_kinds,        only : i1, i4, i8, sp, dp 
       use iso_c_binding,    only : c_int, c_long_long 
       use atmos_refraction, only : emitter_height_delta_atmos_refraction_f924_r4, &
                                    emitter_height_delta_atmos_refraction_f924_r8
       implicit none 
       integer(kind=c_long_long),       parameter :: RDTSC_LATENCY     = 18 ! for Skylake arch.
       integer(kind=c_long_long),       parameter :: ZERO              = 0_c_long_long 
       !real(kind=sp),                   parameter :: sec_to_rad_inc    = 0.000004848_sp 
       real(kind=sp),                   parameter :: min_to_rad_step   = 0.000581776_sp
       real(kind=sp),                   parameter :: z0_to_del_step    = 0.000004040111111111111111111111_sp
       real(kind=sp),                   parameter :: min_to_rad_step_r8   = 0.000581776_dp
       real(kind=sp),                   parameter :: z0_to_del_step_r8    = 0.000004040111111111111111111111_dp
       !real(kind=sp),                   parameter :: C314159265358979323846264338328 = &
       !                                                       3.14159265358979323846264338328_sp
       integer(kind=i4),                parameter :: buf_len          = 5399_i4 
#if 0
    ICC and ifort commands
    icc -c -std=c99 GMS_intrinsics_wrappers.c
    ifort -o unit_test_func_f924_omp -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90 GMS_atmos_refraction.f90 GMS_intrinsics_wrappers.o test_func_f924_omp.f90
    -------------------------------------------------------------------------------------------------------------------------------------------------
    For assembly only:
    ifort -S unit_test_func_f924_omp -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90 GMS_atmos_refraction.f90 GMS_intrinsics_wrappers.o test_func_f924_omp.f90
#endif

#if 0
Ifort causes a nan value due to weird optimization bug
    (gdb) x/4i $pc
    => 0x4a23a9 <atmos_refraction::emitter_height_delta_atmos_refraction_f924_r4+233>:	vsqrtss xmm16,xmm16,xmm16 <------------- HERE xmm16 is a negative value.
=> 0x4a23af <atmos_refraction::emitter_height_delta_atmos_refraction_f924_r4+239>:	vmulss xmm20,xmm19,xmm16
   0x4a23b5 <atmos_refraction::emitter_height_delta_atmos_refraction_f924_r4+245>:	vsubss xmm0,xmm20,xmm21
   0x4a23bb <atmos_refraction::emitter_height_delta_atmos_refraction_f924_r4+251>:	add    rsp,0x20
   0x4a23bf <atmos_refraction::emitter_height_delta_atmos_refraction_f924_r4+255>:	pop    rbx
(gdb) p $xmm16
$16 = ( v8_bfloat16 = (0, -nan(0x40), 0, 0, 0, 0, 0, 0), v8_half = (0, -nan(0x3c0), 0, 0, 0, 0, 0, 0), v4_float = (-nan(0x400000), 0, 0, 0), v2_double = (2.1199235294506578e-314, 0), v16_int8 = (0, 0, -64, -1, 0, <repeats 12 times>), v8_int16 = (0, -64, 0, 0, 0, 0, 0, 0), v4_int32 = (-4194304, 0, 0, 0), v2_int64 = (4290772992, 0), uint128 = 4290772992 )
(gdb) 
#endif

      contains 


subroutine test_omp_emitter_height_delta_atmos_refraction_f924_r4()
           use iso_c_binding, only : c_int, c_long_long 
           use IFPORT 
           use omp_lib 
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
              real(kind=sp),dimension(0:buf_len), automatic :: buf_z0 
              real(kind=sp),dimension(0:buf_len), automatic :: buf_del 
              real(kind=sp),dimension(0:buf_len), automatic :: buf_L 
              real(kind=sp),dimension(0:buf_len), automatic :: buf_dHc
              !dir$ attributes align : 64 :: buf_z0
              !dir$ attributes align : 64 :: buf_del 
              !dir$ attributes align : 64 :: buf_L
              !dir$ attributes align : 64 :: buf_dHc 
              character(len=256),        automatic  :: emsg 
              character(len=40),         parameter  :: OUTFILE  = "test_func_omp_f924_output.dat" 
              character(len=80),         parameter  :: header   = "[TEST #20: emitter_height_delta_atmos_refraction_f924_r4 -- START]"
              character(len=80),         parameter  :: footer   = "[TEST #20: emitter_height_delta_atmos_refraction_f924_r4 -- END]"
              integer(kind=c_long_long), automatic  :: start, end 
              integer(kind=c_long_long), automatic  :: start_c, end_c,tsc_elapsed
              integer(kind=i4),          automatic  :: j__, i__ 
              integer(kind=i4),          automatic  :: ioerr 
              integer(kind=i4),          parameter  :: IOUNIT = 102
              real(kind=sp),             automatic  :: z0,z0_sum,z0_step
              real(kind=sp),             automatic  :: del,del_sum,del_step  
              real(kind=sp),             automatic  :: L,L_sum,L_step  
              real(kind=sp),             automatic  :: t0,t1
              real(kind=sp),             automatic  :: t2,t3 
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
              z0_step  = min_to_rad_step 
              del_step = z0_to_del_step  
              L_step   = 0.1_sp ! meter
              z0_sum   = 0.0_sp 
              del_sum  = 0.0_sp 
              L_sum    = 0.0_sp 
              start    = rdtsc_wrap()
!$omp simd linear(j__:1) aligned(buf_del,buf_L,buf_z0:64) reduction(+:z0_sum) reduction(+:del_sum) reduction(+:L_sum)
              do j__=0, buf_len 
                 z0_sum       = z0_sum+z0_step 
                 buf_z0(j__) = z0_sum 
                 del_sum      = del_sum+del_step 
                 buf_del(j__)= del_sum 
                 L_sum        = L_sum+L_step 
                 buf_L(j__)  = L_sum 
              end do 
              
              !print*,buf_z0 

!!$omp simd linear(i__:1) aligned(buf_del,buf_L,buf_dHc,buf_z0:64)  private(t0,t1,t2,t3)
              do i__=0, buf_len-3, i__+4 
                 t0            = buf_del(i__+0)
                 t1            = buf_z0(i__+0)
                 t2            = buf_L(i__+0)
                 t3            = emitter_height_delta_atmos_refraction_f924_r4(t0,t1,t2)
                 buf_dHc(i__+0) = t3 
                 t0            = buf_del(i__+1)
                 t1            = buf_z0(i__+1)
                 t2            = buf_L(i__+1)
                 t3            = emitter_height_delta_atmos_refraction_f924_r4(t0,t1,t2)
                 buf_dHc(i__+1) = t3 
                 t0            = buf_del(i__+2)
                 t1            = buf_z0(i__+2)
                 t2            = buf_L(i__+2)
                 t3            = emitter_height_delta_atmos_refraction_f924_r4(t0,t1,t2)
                 buf_dHc(i__+2) = t3
                 t0            = buf_del(i__+3)
                 t1            = buf_z0(i__+3)
                 t2            = buf_L(i__+3)
                 t3            = emitter_height_delta_atmos_refraction_f924_r4(t0,t1,t2)
                 buf_dHc(i__+3) = t3  
              end do 
              !print*, buf_dHc

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
                 write(IOUNIT,'(A64)') "[OUTPUT-START]: emitter_height_delta_atmos_refraction_f924_r4"
                 do i__=0, buf_len 
                    write(IOUNIT,'(A6,I5,F22.15)') "Index:", i__,buf_dHc(i__)
                 end do 
                 write(IOUNIT,'(A64)') "[OUTPUT-END]: emitter_height_delta_atmos_refraction_f924_r4"
              end if  
              close(IOUNIT,STATUS='KEEP')
#endif
              print*, "[SANITY-CHECK]:"
              call intern_test_func_f921()
              call intern_test_func_f924()
              print*, footer

              contains 

              subroutine intern_test_func_f921()
                   use atmos_refraction, only : emitter_known_height_f921_r4
                   real(kind=sp), automatic :: Hc 
                   real(kind=sp), parameter :: L  = 500.0_sp 
                   real(kind=sp), parameter :: z0 = 1.5707963_sp !2679489661923132169164_sp
                   Hc = emitter_known_height_f921_r4(L,z0)
                   print*,"Hc=", Hc 
              end subroutine intern_test_func_f921
              subroutine intern_test_func_f924()
                   use atmos_refraction, only : emitter_height_delta_atmos_refraction_f924_r4
                   real(kind=sp) :: L   = 500.0_sp 
                   real(kind=sp) :: del = 0.6544985_sp 
                   real(kind=sp) :: z0  = 0.51_sp 
                   real(kind=sp) :: delHc = 0.0_sp 
                   delHc = emitter_height_delta_atmos_refraction_f924_r4(del,z0,L)
                   print*,"delHc=", delHc
              end subroutine intern_test_func_f924
end subroutine test_omp_emitter_height_delta_atmos_refraction_f924_r4

subroutine test_omp_emitter_height_delta_atmos_refraction_f924_r8()
           use iso_c_binding, only : c_int, c_long_long 
           use IFPORT 
           use omp_lib 
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
              real(kind=dp),dimension(0:buf_len), automatic :: buf_z0 
              real(kind=dp),dimension(0:buf_len), automatic :: buf_del 
              real(kind=dp),dimension(0:buf_len), automatic :: buf_L 
              real(kind=dp),dimension(0:buf_len), automatic :: buf_dHc
              !dir$ attributes align : 64 :: buf_z0
              !dir$ attributes align : 64 :: buf_del 
              !dir$ attributes align : 64 :: buf_L
              !dir$ attributes align : 64 :: buf_dHc 
              character(len=256),        automatic  :: emsg 
              character(len=40),         parameter  :: OUTFILE  = "test_func_omp_f924_output2.dat" 
              character(len=80),         parameter  :: header   = "[TEST #21: emitter_height_delta_atmos_refraction_f924_r8 -- START]"
              character(len=80),         parameter  :: footer   = "[TEST #21: emitter_height_delta_atmos_refraction_f924_r8 -- END]"
              integer(kind=c_long_long), automatic  :: start, end 
              integer(kind=c_long_long), automatic  :: start_c, end_c,tsc_elapsed
              integer(kind=i4),          automatic  :: j__, i__ 
              integer(kind=i4),          automatic  :: ioerr 
              integer(kind=i4),          parameter  :: IOUNIT = 102
              real(kind=dp),             automatic  :: z0,z0_sum,z0_step
              real(kind=dp),             automatic  :: del,del_sum,del_step  
              real(kind=dp),             automatic  :: L,L_sum,L_step  
              real(kind=dp),             automatic  :: t0,t1
              real(kind=dp),             automatic  :: t2,t3 
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
              z0_step  = min_to_rad_step_r8
              del_step = z0_to_del_step_r8 
              L_step   = 0.1_dp ! meter
              z0_sum   = 0.0_dp 
              del_sum  = 0.0_dp 
              L_sum    = 0.0_dp 
              start    = rdtsc_wrap()
!$omp simd linear(j__:1) aligned(buf_del,buf_L,buf_z0:64) reduction(+:z0_sum) reduction(+:del_sum) reduction(+:L_sum)
              do j__=0, buf_len
                 z0_sum       = z0_sum+z0_step 
                 buf_z0(j__) = z0_sum 
                 del_sum      = del_sum+del_step 
                 buf_del(j__)= del_sum 
                 L_sum        = L_sum+L_step 
                 buf_L(j__)  = L_sum 
              end do 
              
              !print*,buf_z0 

!!$omp simd linear(i__:1) aligned(buf_del,buf_L,buf_dHc,buf_z0:64)  private(t0,t1,t2,t3)
              do i__=0, buf_len-3, i__+4 
                 t0            = buf_del(i__+0)
                 t1            = buf_z0(i__+0)
                 t2            = buf_L(i__+0)
                 t3            = emitter_height_delta_atmos_refraction_f924_r8(t0,t1,t2)
                 buf_dHc(i__+0) = t3 
                 t0            = buf_del(i__+1)
                 t1            = buf_z0(i__+1)
                 t2            = buf_L(i__+1)
                 t3            = emitter_height_delta_atmos_refraction_f924_r8(t0,t1,t2)
                 buf_dHc(i__+1) = t3 
                 t0            = buf_del(i__+2)
                 t1            = buf_z0(i__+2)
                 t2            = buf_L(i__+2)
                 t3            = emitter_height_delta_atmos_refraction_f924_r8(t0,t1,t2)
                 buf_dHc(i__+2) = t3
                 t0            = buf_del(i__+3)
                 t1            = buf_z0(i__+3)
                 t2            = buf_L(i__+3)
                 t3            = emitter_height_delta_atmos_refraction_f924_r8(t0,t1,t2)
                 buf_dHc(i__+3) = t3  
              end do 
              !print*, buf_dHc

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
                 write(IOUNIT,'(A64)') "[OUTPUT-START]: emitter_height_delta_atmos_refraction_f924_r4"
                 do i__=0, buf_len 
                    write(IOUNIT,'(A6,I5,F22.15)') "Index:", i__,buf_dHc(i__)
                 end do 
                 write(IOUNIT,'(A64)') "[OUTPUT-END]: emitter_height_delta_atmos_refraction_f924_r4"
              end if  
              close(IOUNIT,STATUS='KEEP')
#endif
              print*, footer 
end subroutine test_omp_emitter_height_delta_atmos_refraction_f924_r8


end module mod_test_func_f924_omp 





program main 
   use mod_test_func_f924_omp 
   call test_omp_emitter_height_delta_atmos_refraction_f924_r4()
   call test_omp_emitter_height_delta_atmos_refraction_f924_r8()
end program main 