


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

module mod_test_cadd_vec_zmm16r4_cv_sv



       use mod_kinds,                 only : i1, i4, sp 
       use iso_c_binding,             only : c_int, c_long_long 
       use caddv_zmm16r4
       implicit none 

       integer(kind=c_long_long),       parameter :: RDTSC_LATENCY = 18 ! for Skylake arch.
       integer(kind=c_long_long),       parameter :: ZERO          = 0_c_long_long 
       

#if 0
    ICC and ifort commands
    icc -c -std=c99 GMS_intrinsics_wrappers.c
    ifort -o test_cadd_vec_zmm16r4_cv_sv -fp-model fast=2 -ftz -O3 -ggdb -qopt-zmm-usage=high -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90 GMS_vectypes.f90  GMS_cadd_vec_zmm16r4.f90 GMS_intrinsics_wrappers.o test_cadd_vec_zmm16r4_cv_sv.f90
    -------------------------------------------------------------------------------------------------------------------------------------------------
    ifx -o test_cadd_vec_zmm16r4_cv_sv -fp-model fast=2 -ftz -O3 -qopt-zmm-usage=high -march=skylake-avx512   \
    -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=3 GMS_kinds.f90 GMS_vectypes.f90  GMS_cadd_vec_zmm16r4.f90 GMS_intrinsics_wrappers.o test_cadd_vec_zmm16r4_cv_sv.f90

    For assembly only:
    ifort -S  test_cadd_vec_zmm16r4_cv_sv -fp-model fast=2 -ftz -O3 -ggdb -qopt-zmm-usage=high -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90 GMS_vectypes.f90  GMS_cadd_vec_zmm16r4.f90 GMS_intrinsics_wrappers.o test_cadd_vec_zmm16r4_cv_sv.f90
#endif

      contains 




subroutine unit_test_caddv_kernel_v512_cv_sv_32x16_ps()
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
              character(len=256),        automatic  :: emsg 
              character(len=80),         parameter  :: header  = "[TEST #1: caddv_kernel_v512_cv_sv_32x16_ps -- START]"
              character(len=80),         parameter  :: footer  = "[TEST #1: caddv_kernel_v512_cv_sv_32x16_ps -- END]"
              character(len=80),         parameter  :: OUTFILE = "OUTPUT_caddv_kernel_v512_cv_sv_32x16_ps.dat"
              real(kind=sp), dimension(:), allocatable :: xre 
              real(kind=sp), dimension(:), allocatable :: yre 
              real(kind=sp), dimension(:), allocatable :: zre 
            
              !dir$ attributes align : 64 :: xre 
              !dir$ attributes align : 64 :: yre 
              !dir$ attributes align : 64 :: zre 
              
              integer(kind=i4),          automatic  :: nvecs 
              integer(kind=i4),          automatic  :: i__,j__
              integer(kind=i4),          parameter  :: iounit = 102 
              integer(kind=i4),          automatic  :: ioerr
              integer(kind=i4),          automatic  :: start,end 
              integer(kind=i4),          automatic  :: start_c,end_c 
              integer(kind=i4),          automatic  :: tsc_elapsed 
              real(kind=sp),             automatic  :: rxr,ryr  
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
              allocate(xre(nvecs))
              allocate(yre(nvecs))
              allocate(zre(nvecs))
             
              rxr = 0.0_sp
              ryr = 0.0_sp 
              call random_init(.false.,.false.)
              do i__=1,iand(nvecs-1,inot(3)) 
                 call random_number(rxr)
                 xre(i__+0) = rxr 
                 call random_number(ryr)
                 yre(i__+0) = ryr 
                 call random_number(rxr)
                 xre(i__+1) = rxr 
                 call random_number(ryr)
                 yre(i__+1) = ryr 
                 call random_number(rxr)
                 xre(i__+2) = rxr 
                 call random_number(ryr)
                 yre(i__+2) = ryr 
                 call random_number(rxr)
                 xre(i__+3) = rxr 
                 call random_number(ryr)
                 yre(i__+3) = ryr 
              end do  
              do j__=i__,nvecs 
                 call random_number(rxr)
                 xre(j__) = rxr 
                 call random_number(ryr)
                 yre(j__) = ryr 
             end do  
              
              start       = rdtsc_wrap()
              call caddv_kernel_v512_cv_sv_32x16_ps(xre,yre,zre,nvecs)             
              end         = rdtsc_wrap()
              start_c     = start-RDTSC_LATENCY
              end_c       = end-RDTSC_LATENCY
              tsc_elapsed = end_c-start_c 
#if 1 
              ioerr = 0
              open(UNIT=IOUNIT,FILE=OUTFILE,IOMSG=emsg,ACCESS="SEQUENTIAL",STATUS="NEW")
              ioflag = (ioerr==0)
              if(.not.ioflag) then 
                 print*, "[ERROR] -- OPEN failed an error message is as follows:"
                 print*, emsg 
                 return 
              else 
                 write(IOUNIT,'(A80)') "[OUTPUT-START]: caddv_kernel_v512_cv_sv_32x16_ps"
                 write(IOUNIT,'(T14,A3)') "re"
                 do i__=1,nvecs  
                    write(IOUNIT,'(F22.15)') zre(i__)
                end do 
                 write(IOUNIT,'(A80)') "[OUTPUT-END]: caddv_kernel_v512_cv_sv_32x16_ps"
              end if  
              close(IOUNIT,STATUS='KEEP')
#endif 
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              if(allocated(xre)) deallocate(xre)
              if(allocated(yre)) deallocate(yre)
              if(allocated(zre)) deallocate(zre)
              print*, footer
        contains 
              function set_random_size() result(rval)
                       implicit none 
                       integer(kind=i4), automatic :: rnum 
                       integer(kind=i4), parameter :: lo = 1024
                       integer(kind=i4)            :: rval 
                       rnum = irand()
                       if(rnum<lo) then 
                          rnum=lo
                          rval=rnum
                       end if 
                       rval=rnum 
               end function set_random_size
end subroutine unit_test_caddv_kernel_v512_cv_sv_32x16_ps


subroutine unit_test_caddv_kernel_v512_cv_sv_16x16_ps()
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
              character(len=256),        automatic  :: emsg 
              character(len=80),         parameter  :: header  = "[TEST #2: caddv_kernel_v512_cv_sv_16x16_ps -- START]"
              character(len=80),         parameter  :: footer  = "[TEST #2: caddv_kernel_v512_cv_sv_16x16_ps -- END]"
              character(len=80),         parameter  :: OUTFILE = "OUTPUT_caddv_kernel_v512_cv_sv_16x16_ps.dat"
              real(kind=sp), dimension(:), allocatable :: xre 
              real(kind=sp), dimension(:), allocatable :: yre 
              real(kind=sp), dimension(:), allocatable :: zre 
              
              !dir$ attributes align : 64 :: xre 
              !dir$ attributes align : 64 :: yre 
              !dir$ attributes align : 64 :: zre 
           
              integer(kind=i4),          automatic  :: nvecs 
              integer(kind=i4),          automatic  :: i__,j__
              integer(kind=i4),          parameter  :: iounit = 102 
              integer(kind=i4),          automatic  :: ioerr
              integer(kind=i4),          automatic  :: start,end 
              integer(kind=i4),          automatic  :: start_c,end_c 
              integer(kind=i4),          automatic  :: tsc_elapsed 
              real(kind=sp),             automatic  :: rxr,ryr 
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
              allocate(xre(nvecs))
              allocate(yre(nvecs))
              allocate(zre(nvecs))
              
              rxr = 0.0_sp
              ryr = 0.0_sp 
              call random_init(.false.,.false.)
              do i__=1,iand(nvecs-1,inot(3)) 
                 call random_number(rxr)
                 xre(i__+0) = rxr 
                 call random_number(ryr)
                 yre(i__+0) = ryr 
                 call random_number(rxr)
                 xre(i__+1) = rxr 
                 call random_number(ryr)
                 yre(i__+1) = ryr 
                 call random_number(rxr)
                 xre(i__+2) = rxr 
                 call random_number(ryr)
                 yre(i__+2) = ryr 
                 call random_number(rxr)
                 xre(i__+3) = rxr 
                 call random_number(ryr)
                 yre(i__+3) = ryr 
              end do  
              do j__=i__,nvecs 
                 call random_number(rxr)
                 xre(j__) = rxr 
                 call random_number(ryr)
                 yre(j__) = ryr 
             end do  
              
              start       = rdtsc_wrap()
              call caddv_kernel_v512_cv_sv_16x16_ps(xre,yre,zre,nvecs)             
              end         = rdtsc_wrap()
              start_c     = start-RDTSC_LATENCY
              end_c       = end-RDTSC_LATENCY
              tsc_elapsed = end_c-start_c 
#if 1 
              ioerr = 0
              open(UNIT=IOUNIT,FILE=OUTFILE,IOMSG=emsg,ACCESS="SEQUENTIAL",STATUS="NEW")
              ioflag = (ioerr==0)
              if(.not.ioflag) then 
                 print*, "[ERROR] -- OPEN failed an error message is as follows:"
                 print*, emsg 
                 return 
              else 
                 write(IOUNIT,'(A80)') "[OUTPUT-START]: caddv_kernel_v512_cv_sv_16x16_ps"
                 write(IOUNIT,'(T14,A3)') "re","im"
                 do i__=1,nvecs  
                    write(IOUNIT,'(F22.15)') zre(i__)
                end do 
                 write(IOUNIT,'(A80)') "[OUTPUT-END]: caddv_kernel_v512_cv_sv_16x16_ps"
              end if  
              close(IOUNIT,STATUS='KEEP')
#endif 
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              if(allocated(xre)) deallocate(xre)
              if(allocated(yre)) deallocate(yre)
              if(allocated(zre)) deallocate(zre)
              print*, footer
        contains 
              function set_random_size() result(rval)
                       implicit none 
                       integer(kind=i4), automatic :: rnum 
                       integer(kind=i4), parameter :: lo = 1024
                       integer(kind=i4)            :: rval 
                       rnum = irand()
                       if(rnum<lo) then 
                          rnum=lo
                          rval=rnum
                       end if 
                       rval=rnum 
               end function set_random_size
end subroutine unit_test_caddv_kernel_v512_cv_sv_16x16_ps


subroutine unit_test_caddv_kernel_v512_cv_sv_8x16_ps()
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
              character(len=256),        automatic  :: emsg 
              character(len=80),         parameter  :: header  = "[TEST #3: caddv_kernel_v512_cv_sv_8x16_ps -- START]"
              character(len=80),         parameter  :: footer  = "[TEST #3: caddv_kernel_v512_cv_sv_8x16_ps -- END]"
              character(len=80),         parameter  :: OUTFILE = "OUTPUT_caddv_kernel_v512_cv_sv_8x16_ps.dat"
              real(kind=sp), dimension(:), allocatable :: xre 
              real(kind=sp), dimension(:), allocatable :: yre 
              real(kind=sp), dimension(:), allocatable :: zre 
             
              !dir$ attributes align : 64 :: xre 
              !dir$ attributes align : 64 :: yre 
              !dir$ attributes align : 64 :: zre 
              
              integer(kind=i4),          automatic  :: nvecs 
              integer(kind=i4),          automatic  :: i__,j__
              integer(kind=i4),          parameter  :: iounit = 102 
              integer(kind=i4),          automatic  :: ioerr
              integer(kind=i4),          automatic  :: start,end 
              integer(kind=i4),          automatic  :: start_c,end_c 
              integer(kind=i4),          automatic  :: tsc_elapsed 
              real(kind=sp),             automatic  :: rxr,ryr  
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
              allocate(xre(nvecs))
              allocate(yre(nvecs))
              allocate(zre(nvecs))
              rxr = 0.0_sp
              ryr = 0.0_sp 
              call random_init(.false.,.false.)
              do i__=1,iand(nvecs-1,inot(3)) 
                 call random_number(rxr)
                 xre(i__+0) = rxr 
                 call random_number(ryr)
                 yre(i__+0) = ryr 
                 call random_number(rxr)
                 xre(i__+1) = rxr 
                 call random_number(ryr)
                 yre(i__+1) = ryr 
                 call random_number(rxr)
                 xre(i__+2) = rxr 
                 call random_number(ryr)
                 yre(i__+2) = ryr 
                 call random_number(rxr)
                 xre(i__+3) = rxr 
                 call random_number(ryr)
                 yre(i__+3) = ryr 
              end do  
              do j__=i__,nvecs 
                 call random_number(rxr)
                 xre(j__) = rxr 
                 call random_number(ryr)
                 yre(j__) = ryr 
             end do  
              
              start       = rdtsc_wrap()
              call caddv_kernel_v512_cv_sv_8x16_ps(xre,yre,zre,nvecs)             
              end         = rdtsc_wrap()
              start_c     = start-RDTSC_LATENCY
              end_c       = end-RDTSC_LATENCY
              tsc_elapsed = end_c-start_c 
#if 1 
              ioerr = 0
              open(UNIT=IOUNIT,FILE=OUTFILE,IOMSG=emsg,ACCESS="SEQUENTIAL",STATUS="NEW")
              ioflag = (ioerr==0)
              if(.not.ioflag) then 
                 print*, "[ERROR] -- OPEN failed an error message is as follows:"
                 print*, emsg 
                 return 
              else 
                 write(IOUNIT,'(A80)') "[OUTPUT-START]: caddv_kernel_v512_cv_sv_8x16_ps"
                 write(IOUNIT,'(T14,A3)') "re"
                 do i__=1,nvecs  
                    write(IOUNIT,'(F22.15)') zre(i__)
                end do 
                 write(IOUNIT,'(A80)') "[OUTPUT-END]: caddv_kernel_v512_cv_sv_8x16_ps"
              end if  
              close(IOUNIT,STATUS='KEEP')
#endif 
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              if(allocated(xre)) deallocate(xre)
              if(allocated(yre)) deallocate(yre)
              if(allocated(zre)) deallocate(zre)
              print*, footer
        contains 
              function set_random_size() result(rval)
                       implicit none 
                       integer(kind=i4), automatic :: rnum 
                       integer(kind=i4), parameter :: lo = 1024
                       integer(kind=i4)            :: rval 
                       rnum = irand()
                       if(rnum<lo) then 
                          rnum=lo
                          rval=rnum
                       end if 
                       rval=rnum 
               end function set_random_size
end subroutine unit_test_caddv_kernel_v512_cv_sv_8x16_ps


subroutine unit_test_caddv_kernel_v512_cv_sv_4x16_ps()
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
              character(len=256),        automatic  :: emsg 
              character(len=80),         parameter  :: header  = "[TEST #4: caddv_kernel_v512_cv_sv_4x16_ps -- START]"
              character(len=80),         parameter  :: footer  = "[TEST #4: caddv_kernel_v512_cv_sv_4x16_ps -- END]"
              character(len=80),         parameter  :: OUTFILE = "OUTPUT_caddv_kernel_v512_cv_sv_4x16_ps.dat"
              real(kind=sp), dimension(:), allocatable :: xre 
              real(kind=sp), dimension(:), allocatable :: yre 
              real(kind=sp), dimension(:), allocatable :: zre 
             
              !dir$ attributes align : 64 :: xre 
              !dir$ attributes align : 64 :: yre 
              !dir$ attributes align : 64 :: zre 
              
              integer(kind=i4),          automatic  :: nvecs 
              integer(kind=i4),          automatic  :: i__,j__
              integer(kind=i4),          parameter  :: iounit = 102 
              integer(kind=i4),          automatic  :: ioerr
              integer(kind=i4),          automatic  :: start,end 
              integer(kind=i4),          automatic  :: start_c,end_c 
              integer(kind=i4),          automatic  :: tsc_elapsed 
              real(kind=sp),             automatic  :: rxr,ryr  
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
              allocate(xre(nvecs))
              allocate(yre(nvecs))
              allocate(zre(nvecs))
              rxr = 0.0_sp
              ryr = 0.0_sp 
              call random_init(.false.,.false.)
              do i__=1,iand(nvecs-1,inot(3)) 
                 call random_number(rxr)
                 xre(i__+0) = rxr 
                 call random_number(ryr)
                 yre(i__+0) = ryr 
                 call random_number(rxr)
                 xre(i__+1) = rxr 
                 call random_number(ryr)
                 yre(i__+1) = ryr 
                 call random_number(rxr)
                 xre(i__+2) = rxr 
                 call random_number(ryr)
                 yre(i__+2) = ryr 
                 call random_number(rxr)
                 xre(i__+3) = rxr 
                 call random_number(ryr)
                 yre(i__+3) = ryr 
              end do  
              do j__=i__,nvecs 
                 call random_number(rxr)
                 xre(j__) = rxr 
                 call random_number(ryr)
                 yre(j__) = ryr 
             end do  
              
              start       = rdtsc_wrap()
              call caddv_kernel_v512_cv_sv_4x16_ps(xre,yre,zre,nvecs)             
              end         = rdtsc_wrap()
              start_c     = start-RDTSC_LATENCY
              end_c       = end-RDTSC_LATENCY
              tsc_elapsed = end_c-start_c 
#if 1 
              ioerr = 0
              open(UNIT=IOUNIT,FILE=OUTFILE,IOMSG=emsg,ACCESS="SEQUENTIAL",STATUS="NEW")
              ioflag = (ioerr==0)
              if(.not.ioflag) then 
                 print*, "[ERROR] -- OPEN failed an error message is as follows:"
                 print*, emsg 
                 return 
              else 
                 write(IOUNIT,'(A80)') "[OUTPUT-START]: caddv_kernel_v512_cv_sv_4x16_ps"
                 write(IOUNIT,'(T14,A3)') "re"
                 do i__=1,nvecs  
                    write(IOUNIT,'(F22.15)') zre(i__)
                end do 
                 write(IOUNIT,'(A80)') "[OUTPUT-END]: caddv_kernel_v512_cv_sv_4x16_ps"
              end if  
              close(IOUNIT,STATUS='KEEP')
#endif 
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              if(allocated(xre)) deallocate(xre)
              if(allocated(yre)) deallocate(yre)
              if(allocated(zre)) deallocate(zre)
              print*, footer
        contains 
              function set_random_size() result(rval)
                       implicit none 
                       integer(kind=i4), automatic :: rnum 
                       integer(kind=i4), parameter :: lo = 1024
                       integer(kind=i4)            :: rval 
                       rnum = irand()
                       if(rnum<lo) then 
                          rnum=lo
                          rval=rnum
                       end if 
                       rval=rnum 
               end function set_random_size
end subroutine unit_test_caddv_kernel_v512_cv_sv_4x16_ps


end module mod_test_cadd_vec_zmm16r4_cv_sv  


program main 
   use mod_test_cadd_vec_zmm16r4_cv_sv
   call unit_test_caddv_kernel_v512_cv_sv_32x16_ps()
   call unit_test_caddv_kernel_v512_cv_sv_16x16_ps()
   call unit_test_caddv_kernel_v512_cv_sv_8x16_ps()
   call unit_test_caddv_kernel_v512_cv_sv_4x16_ps()
end program main 