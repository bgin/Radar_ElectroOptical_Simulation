


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

module mod_test_cpow_vec_zmm16r4  



       use mod_kinds,                 only : i1, i4, sp 
       use iso_c_binding,             only : c_int, c_long_long 
       use cpow_vec_zmm16r4
       implicit none 

       integer(kind=c_long_long),       parameter :: RDTSC_LATENCY = 18 ! for Skylake arch.
       integer(kind=c_long_long),       parameter :: ZERO          = 0_c_long_long 

#if !defined(DUMP_ARRAYS_TO_FILE)
#define DUMP_ARRAYS_TO_FILE 0  
#endif   

#if 0
    ICC and ifort commands
    icc -c -std=c99 GMS_intrinsics_wrappers.c
    ifort -o test_cpow_vec_zmm16r4 -fp-model fast=2 -ftz -O3 -ggdb -qopt-zmm-usage=high -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90 GMS_vectypes.f90  GMS_cpow_vec_zmm16r4.f90 GMS_intrinsics_wrappers.o test_cpow_vec_zmm16r4.f90
    -------------------------------------------------------------------------------------------------------------------------------------------------
    ifx -o test_cpow_vec_zmm16r4 -fp-model fast=2 -ftz -O3 -qopt-zmm-usage=high -march=skylake-avx512   \
    -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=3 GMS_kinds.f90 GMS_vectypes.f90  GMS_cexp_vec_zmm16r4.f90 GMS_intrinsics_wrappers.o test_cpow_vec_zmm16r4.f90

    For assembly only:
    ifort -S  test_cpow_vec_zmm16r4 -fp-model fast=2 -ftz -O3 -ggdb -qopt-zmm-usage=high -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90 GMS_vectypes.f90  GMS_cadd_vec_zmm16r4.f90 GMS_intrinsics_wrappers.o test_cpow_vec_zmm16r4.f90
#endif

      contains 




subroutine unit_test_cpowv_v512_32x16_ps()
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
              character(len=80),         parameter  :: header  = "[TEST #1: cpowv_v512_32x16_ps -- START]"
              character(len=80),         parameter  :: footer  = "[TEST #1: cpowv_v512_32x16_ps -- END]"
              character(len=80),         parameter  :: OUTFILE = "OUTPUT_cpowv_v512_32x16_ps.dat"
              real(kind=sp), dimension(:), allocatable :: xre 
              real(kind=sp), dimension(:), allocatable :: xim 
              real(kind=sp), dimension(:), allocatable :: vn 
              real(kind=sp), dimension(:), allocatable :: zre 
              real(kind=sp), dimension(:), allocatable :: zim 
              !dir$ attributes align : 64 :: xre 
              !dir$ attributes align : 64 :: xim
              !dir$ attributes align : 64 :: vn 
              !dir$ attributes align : 64 :: zre 
              !dir$ attributes align : 64 :: zim 
              integer(kind=i4),          automatic  :: nvecs 
              integer(kind=i4),          automatic  :: i__,j__
              integer(kind=i4),          parameter  :: iounit = 102 
              integer(kind=i4),          automatic  :: ioerr
              integer(kind=i4),          automatic  :: start,end 
              integer(kind=i4),          automatic  :: start_c,end_c 
              integer(kind=i4),          automatic  :: tsc_elapsed 
              real(kind=sp),             automatic  :: rxr,rxi,rvn  
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
              allocate(xim(nvecs))
              allocate(vn(nvecs))
              allocate(zre(nvecs))
              allocate(zim(nvecs))
              rxr = 0.0_sp
              rxi = 0.0_sp 
              rvn = 0.0_sp 
              call random_init(.false.,.false.)
              do i__=1,iand(nvecs-1,inot(3)) 
                 call random_number(rxr)
                 xre(i__+0) = rxr 
                 call random_number(rxi)
                 xim(i__+0) = rxi 
                 call random_number(rvn)
                 vn(i__+0)  = rvn 
                 call random_number(rxr)
                 xre(i__+1) = rxr 
                 call random_number(rxi)
                 xim(i__+1) = rxi 
                 call random_number(rvn)
                 vn(i__+1)  = rvn 
                 call random_number(rxr)
                 xre(i__+2) = rxr 
                 call random_number(rxi)
                 xim(i__+2) = rxi 
                 call random_number(rvn)
                 vn(i__+2)  = rvn 
                 call random_number(rxr)
                 xre(i__+3) = rxr 
                 call random_number(rxi)
                 xim(i__+3) = rxi 
                 call random_number(rvn)
                 vn(i__+3)  = rvn 
              end do  
              do j__=i__,nvecs 
                 call random_number(rxr)
                 xre(j__) = rxr 
                 call random_number(rxi)
                 xim(j__) = rxi 
                 call random_number(rvn)
                 vn(j__)  = rvn 
              end do  
              
              start       = rdtsc_wrap()
              call cpowv_v512_32x16_ps(xre,xim,vn,zre,zim,nvecs)             
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
                 write(IOUNIT,'(A80)') "[OUTPUT-START]: cpowv_v512_32x16_ps"
                 write(IOUNIT,'(T14,A3,T36,A3)') "re","im"
                 do i__=1,nvecs  
                    write(IOUNIT,'(2F22.15)') zre(i__),zim(i__)
                end do 
                 write(IOUNIT,'(A80)') "[OUTPUT-END]: cpowv_v512_32x16_ps"
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
              if(allocated(xim)) deallocate(xim)
              if(allocated(vn))  deallocate(vn)
              if(allocated(zre)) deallocate(zre)
              if(allocated(zim)) deallocate(zim)
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
end subroutine unit_test_cpowv_v512_32x16_ps


subroutine unit_test_cpowv_v512_16x16_ps()
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
              character(len=80),         parameter  :: header  = "[TEST #2: cpowv_v512_16x16_ps -- START]"
              character(len=80),         parameter  :: footer  = "[TEST #2: cpowv_v512_16x16_ps -- END]"
              character(len=80),         parameter  :: OUTFILE = "OUTPUT_cpowv_v512_16x16_ps.dat"
              real(kind=sp), dimension(:), allocatable :: xre 
              real(kind=sp), dimension(:), allocatable :: xim 
              real(kind=sp), dimension(:), allocatable :: vn 
              real(kind=sp), dimension(:), allocatable :: zre 
              real(kind=sp), dimension(:), allocatable :: zim 
              !dir$ attributes align : 64 :: xre 
              !dir$ attributes align : 64 :: xim
              !dir$ attributes align : 64 :: vn 
              !dir$ attributes align : 64 :: zre 
              !dir$ attributes align : 64 :: zim 
              integer(kind=i4),          automatic  :: nvecs 
              integer(kind=i4),          automatic  :: i__,j__
              integer(kind=i4),          parameter  :: iounit = 102 
              integer(kind=i4),          automatic  :: ioerr
              integer(kind=i4),          automatic  :: start,end 
              integer(kind=i4),          automatic  :: start_c,end_c 
              integer(kind=i4),          automatic  :: tsc_elapsed 
              real(kind=sp),             automatic  :: rxr,rxi,rvn 
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
              allocate(xim(nvecs))
              allocate(vn(nvecs))
              allocate(zre(nvecs))
              allocate(zim(nvecs))
              rxr = 0.0_sp
              rxi = 0.0_sp 
              rvn = 0.0_sp 
              call random_init(.false.,.false.)
              do i__=1,iand(nvecs-1,inot(3)) 
                 call random_number(rxr)
                 xre(i__+0) = rxr 
                 call random_number(rxi)
                 xim(i__+0) = rxi 
                 call random_number(rvn)
                 vn(i__+0)  = rvn 
                 call random_number(rxr)
                 xre(i__+1) = rxr 
                 call random_number(rxi)
                 xim(i__+1) = rxi 
                 call random_number(rvn)
                 vn(i__+1)  = rvn
                 call random_number(rxr)
                 xre(i__+2) = rxr 
                 call random_number(rxi)
                 xim(i__+2) = rxi 
                 call random_number(rvn)
                 vn(i__+2)  = rvn
                 call random_number(rxr)
                 xre(i__+3) = rxr 
                 call random_number(rxi)
                 xim(i__+3) = rxi 
                 call random_number(rvn)
                 vn(i__+3)  = rvn
              end do  
              do j__=i__,nvecs 
                 call random_number(rxr)
                 xre(j__) = rxr 
                 call random_number(rxi)
                 xim(j__) = rxi 
                 call random_number(rvn)
                 vn(j__)  = rvn
              end do  
              
              start       = rdtsc_wrap()
              call cpowv_v512_16x16_ps(xre,xim,vn,zre,zim,nvecs)             
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
                 write(IOUNIT,'(A80)') "[OUTPUT-START]: cpowv_v512_16x16_ps"
                 write(IOUNIT,'(T14,A3,T36,A3)') "re","im"
                 do i__=1,nvecs  
                    write(IOUNIT,'(2F22.15)') zre(i__),zim(i__)
                end do 
                 write(IOUNIT,'(A80)') "[OUTPUT-END]: cpowv_v512_16x16_ps"
              end if  
              close(IOUNIT,STATUS='KEEP')
#endif 
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
             
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
end subroutine unit_test_cpowv_v512_16x16_ps


subroutine unit_test_cpowv_v512_8x16_ps()
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
              character(len=80),         parameter  :: header  = "[TEST #3: cpowv_v512_8x16_ps -- START]"
              character(len=80),         parameter  :: footer  = "[TEST #3: cpowv_v512_8x16_ps -- END]"
              character(len=80),         parameter  :: OUTFILE = "OUTPUT_cpowv_v512_8x16_ps.dat"
              real(kind=sp), dimension(:), allocatable :: xre 
              real(kind=sp), dimension(:), allocatable :: xim 
              real(kind=sp), dimension(:), allocatable :: vn 
              real(kind=sp), dimension(:), allocatable :: zre 
              real(kind=sp), dimension(:), allocatable :: zim 
              !dir$ attributes align : 64 :: xre 
              !dir$ attributes align : 64 :: xim
              !dir$ attributes align : 64 :: vn 
              !dir$ attributes align : 64 :: zre 
              !dir$ attributes align : 64 :: zim 
              integer(kind=i4),          automatic  :: nvecs 
              integer(kind=i4),          automatic  :: i__,j__
              integer(kind=i4),          parameter  :: iounit = 102 
              integer(kind=i4),          automatic  :: ioerr
              integer(kind=i4),          automatic  :: start,end 
              integer(kind=i4),          automatic  :: start_c,end_c 
              integer(kind=i4),          automatic  :: tsc_elapsed 
              real(kind=sp),             automatic  :: rxr,rxi,rvn   
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
              allocate(xim(nvecs))
              allocate(vn(nvecs))
              allocate(zre(nvecs))
              allocate(zim(nvecs))
              rxr = 0.0_sp
              rxi = 0.0_sp 
              call random_init(.false.,.false.)
              do i__=1,iand(nvecs-1,inot(3)) 
                 call random_number(rxr)
                 xre(i__+0) = rxr 
                 call random_number(rxi)
                 xim(i__+0) = rxi 
                 call random_number(rvn)
                 vn(i__+0)  = rvn 
                 call random_number(rxr)
                 xre(i__+1) = rxr 
                 call random_number(rxi)
                 xim(i__+1) = rxi 
                 call random_number(rvn)
                 vn(i__+1)  = rvn 
                 call random_number(rxr)
                 xre(i__+2) = rxr 
                 call random_number(rxi)
                 xim(i__+2) = rxi 
                 call random_number(rvn)
                 vn(i__+2)  = rvn 
                 call random_number(rxr)
                 xre(i__+3) = rxr 
                 call random_number(rxi)
                 xim(i__+3) = rxi 
                 call random_number(rvn)
                 vn(i__+3)  = rvn 
              end do  
              do j__=i__,nvecs 
                 call random_number(rxr)
                 xre(j__) = rxr 
                 call random_number(rxi)
                 xim(j__) = rxi 
                 call random_number(rvn)
                 vn(j__)  = rvn 
              end do  
              
              start       = rdtsc_wrap()
              call cpowv_v512_8x16_ps(xre,xim,vn,zre,zim,nvecs)             
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
                 write(IOUNIT,'(A80)') "[OUTPUT-START]: cpowv_v512_8x16_ps"
                 write(IOUNIT,'(T14,A3,T36,A3)') "re","im"
                 do i__=1,nvecs  
                    write(IOUNIT,'(2F22.15)') zre(i__),zim(i__)
                end do 
                 write(IOUNIT,'(A80)') "[OUTPUT-END]: cpowv_v512_8x16_ps"
              end if  
              close(IOUNIT,STATUS='KEEP')
#endif 
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 

              !if(allocated(xre)) deallocate(xre)
              !if(allocated(xim)) deallocate(xim)
              !if(allocated(zre)) deallocate(zre)
              !if(allocated(zim)) deallocate(zim)
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
end subroutine unit_test_cpowv_v512_8x16_ps


subroutine unit_test_cpowv_v512_4x16_ps()
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
              character(len=80),         parameter  :: header  = "[TEST #4: cpowv_v512_4x16_ps -- START]"
              character(len=80),         parameter  :: footer  = "[TEST #4: cpowv_v512_4x16_ps -- END]"
              character(len=80),         parameter  :: OUTFILE = "OUTPUT_cpowv_v512_4x16_ps.dat"
              real(kind=sp), dimension(:), allocatable :: xre 
              real(kind=sp), dimension(:), allocatable :: xim 
              real(kind=sp), dimension(:), allocatable :: vn 
              real(kind=sp), dimension(:), allocatable :: zre 
              real(kind=sp), dimension(:), allocatable :: zim 
              !dir$ attributes align : 64 :: xre 
              !dir$ attributes align : 64 :: xim
              !dir$ attributes align : 64 :: vn 
              !dir$ attributes align : 64 :: zre 
              !dir$ attributes align : 64 :: zim 
              integer(kind=i4),          automatic  :: nvecs 
              integer(kind=i4),          automatic  :: i__,j__
              integer(kind=i4),          parameter  :: iounit = 102 
              integer(kind=i4),          automatic  :: ioerr
              integer(kind=i4),          automatic  :: start,end 
              integer(kind=i4),          automatic  :: start_c,end_c 
              integer(kind=i4),          automatic  :: tsc_elapsed 
              real(kind=sp),             automatic  :: rxr,rxi,rvn 
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
              allocate(xim(nvecs))
              allocate(vn(nvecs))
              allocate(zre(nvecs))
              allocate(zim(nvecs))
              rxr = 0.0_sp
              rxi = 0.0_sp 
              rvn = 0.0_sp 
              call random_init(.false.,.false.)
              do i__=1,iand(nvecs-1,inot(3)) 
                 call random_number(rxr)
                 xre(i__+0) = rxr 
                 call random_number(rxi)
                 xim(i__+0) = rxi 
                 call random_number(rvn)
                 vn(i__+0)  = rvn 
                 call random_number(rxr)
                 xre(i__+1) = rxr 
                 call random_number(rxi)
                 xim(i__+1) = rxi 
                 call random_number(rvn)
                 vn(i__+1)  = rvn 
                 call random_number(rxr)
                 xre(i__+2) = rxr 
                 call random_number(rxi)
                 xim(i__+2) = rxi 
                 call random_number(rvn)
                 vn(i__+2)  = rvn 
                 call random_number(rxr)
                 xre(i__+3) = rxr 
                 call random_number(rxi)
                 xim(i__+3) = rxi 
                 call random_number(rvn)
                 vn(i__+3)  = rvn 
              end do  
              do j__=i__,nvecs 
                 call random_number(rxr)
                 xre(j__) = rxr 
                 call random_number(rxi)
                 xim(j__) = rxi 
                 call random_number(rvn)
                 vn(j__)  = rvn 
              end do  
              
              start       = rdtsc_wrap()
              call cpowv_v512_4x16_ps(xre,xim,vn,zre,zim,nvecs)             
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
                 write(IOUNIT,'(A80)') "[OUTPUT-START]: cpowv_v512_4x16_ps"
                 write(IOUNIT,'(T14,A3,T36,A3)') "re","im"
                 do i__=1,nvecs  
                    write(IOUNIT,'(2F22.15)') zre(i__),zim(i__)
                end do 
                 write(IOUNIT,'(A80)') "[OUTPUT-END]: cpowv_v512_4x16_ps"
              end if  
              close(IOUNIT,STATUS='KEEP')
#endif 
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              !if(allocated(xre)) deallocate(xre)
              !if(allocated(xim)) deallocate(xim)
              !if(allocated(zre)) deallocate(zre)
              !if(allocated(zim)) deallocate(zim)
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
end subroutine unit_test_cpowv_v512_4x16_ps

#if 0
subroutine unit_test_cpowv_v512_XXx16_ps_omp()
           use iso_c_binding, only : c_int, c_long_long 
           use IFPORT 
           use omp_lib 
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
              character(len=80),         parameter  :: header  = "[TEST #1: cpowv_v512_XXx16_ps_omp -- START]"
              character(len=80),         parameter  :: footer  = "[TEST #1: cpowv_v512_XXx16_ps_omp -- END]"
#if (DUMP_ARRAYS_TO_FILE) == 1
              character(len=80),         parameter  :: OUTFILE = "OUTPUT_cpowv_v512_XXx16_ps_omp.dat"
#endif 
              real(kind=sp), dimension(:,:), allocatable :: xre 
              real(kind=sp), dimension(:,:), allocatable :: xim 
              real(kind=sp), dimension(:,:), allocatable :: vn 
              real(kind=sp), dimension(:,:), allocatable :: zre 
              real(kind=sp), dimension(:,:), allocatable :: zim 
              !dir$ attributes align : 64 :: xre 
              !dir$ attributes align : 64 :: xim
              !dir$ attributes align : 64 :: vn 
              !dir$ attributes align : 64 :: zre 
              !dir$ attributes align : 64 :: zim 
              integer(kind=c_long_long),          automatic  :: start,end 
              integer(kind=c_long_long),          automatic  :: start_c,end_c 
              integer(kind=c_long_long),          automatic  :: tsc_elapsed 
              integer(kind=i4),          automatic  :: n_j
              integer(kind=i4),          automatic  :: n_i 
              integer(kind=i4),          automatic  :: i__,j__ 
              integer(kind=i4),          parameter  :: iounit = 102 
              real(kind=sp),             parameter  :: scale  = 6.0_sp 
              integer(kind=i4),          automatic  :: ioerr
              integer(kind=i4),          automatic  :: which 
              real(kind=sp),             automatic  :: rxr,rxi,rvn  
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
              n_i = set_random_size()
              n_j = set_random_1st_dim()
              allocate(xre(n_i,n_j))
              allocate(xim(n_i,n_j))
              allocate(vn(n_i,n_j))
              allocate(zre(n_i,n_j))
              allocate(zim(n_i,n_j))
              rxr = 0.0_sp
              rxi = 0.0_sp 
              rvn = 0.0_sp 
              call random_init(.true.,.true.)
              start       = rdtsc_wrap()
!$omp parallel default(none)  shared(n_j,n_i,xre,xim,zre,zim) firstprivate(rxr,rxi) private(j__,i__) num_threads(6)
!$omp do schedule(static)
              do j__=1,n_j 
                   do i__=1,n_i 
                      call random_number(rxr)
                      xre(i__+0,j__) = rxr 
                      call random_number(rxi)
                      xim(i__+0,j__) = rxi 
                      call random_number(rvn)
                      vn(i__+0,j__)  = rvn 
                      call random_number(rxr)
                      xre(i__+1,j__) = rxr 
                      call random_number(rxi)
                      xim(i__+1,j__) = rxi 
                      call random_number(rvn)
                      vn(i__+1,j__)  = rvn 
                      call random_number(rxr)
                      xre(i__+2,j__) = rxr 
                      call random_number(rxi)
                      xim(i__+2,j__) = rxi 
                      call random_number(rvn)
                      vn(i__+2,j__)  = rvn 
                      call random_number(rxr)
                      xre(i__+3,j__) = rxr 
                      call random_number(rxi)
                      xim(i__+3,j__) = rxi 
                      call random_number(rvn)
                      vn(i__+3,j__)  = rvn 
                   end do  
              end do 
!$omp end do nowait
!$omp do schedule(static)
             do j__=1,n_j 
                call cpowv_v512_32x16_ps(xre(:,j__),xim(:,j__),vn(:,j__),zre(:,j__),zim(:,j__),n_i)  
             end do 
!$omp end do
!$omp end parallel 
    
              end         = rdtsc_wrap()
              start_c     = start-RDTSC_LATENCY
              end_c       = end-RDTSC_LATENCY
              tsc_elapsed = end_c-start_c 
#if (DUMP_ARRAYS_TO_FILE) == 1
              ioerr = 0
              open(UNIT=IOUNIT,FILE=OUTFILE,IOMSG=emsg,ACCESS="SEQUENTIAL",STATUS="NEW")
              ioflag = (ioerr==0)
              if(.not.ioflag) then 
                 print*, "[ERROR] -- OPEN failed an error message is as follows:"
                 print*, emsg 
                 return 
              else 
                 write(IOUNIT,'(A80)') "[OUTPUT-START]: cpowv_v512_32x16_ps"
                 write(IOUNIT,'(T14,A3,T36,A3)') "re","im"
                 do j__=1,n_j 
                    do i__=1,n_i 
                        write(IOUNIT,'(2F22.15)') zre(i__,j__),zim(i__,j__)
                    end do 
                 end do 
                 write(IOUNIT,'(A80)') "[OUTPUT-END]: cpowv_v512_32x16_ps"
              end if  
              close(IOUNIT,STATUS='KEEP')
#endif 
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 

              !if(allocated(xre)) deallocate(xre)
              !if(allocated(xim)) deallocate(xim)
              !if(allocated(zre)) deallocate(zre)
              !if(allocated(zim)) deallocate(zim)
              print*, footer
        contains 
              function set_random_size() result(rval)
                       implicit none 
                       integer(kind=i4), automatic :: rnum 
                       integer(kind=i4), parameter :: lo = 4096
                       integer(kind=i4)            :: rval 
                       rnum = irand()
                       if(rnum<lo) then 
                          rnum=lo
                          rval=rnum
                       end if 
                       rval=rnum 
               end function set_random_size
               function set_random_1st_dim() result(rval)
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
               end function set_random_1st_dim
end subroutine unit_test_cpowv_v512_32x16_ps_omp 
#endif

end module mod_test_cpow_vec_zmm16r4  


program main 
   use mod_test_cpow_vec_zmm16r4
   call unit_test_cpowv_v512_32x16_ps()
   call unit_test_cpowv_v512_16x16_ps()
   call unit_test_cpowv_v512_8x16_ps()
   call unit_test_cpowv_v512_4x16_ps()
   !call unit_test_cpowv_v512_32x16_ps_omp()
end program main 