
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

module mod_test_avx_cvec4_mul



       use mod_kinds,                 only : i1, i4, dp 
       use iso_c_binding,             only : c_int, c_long_long 
       use mod_vectypes,              only : YMM4r8_t
       use avx_cvec4
       implicit none 

       integer(kind=c_long_long),       parameter :: RDTSC_LATENCY = 18 ! for Skylake arch.
       integer(kind=c_long_long),       parameter :: ZERO          = 0_c_long_long 
       

#if 0
    ICC and ifort commands
    icc -c -std=c99 GMS_intrinsics_wrappers.c
    ifort -o test_avx_cvec4_mul -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90 GMS_vectypes.f90 GMS_constants.f90 GMS_vecconsts.f90 GMS_sse_cvec2.f90 GMS_intrinsics_wrappers.o test_avx_cvec4_mul.f90
    -------------------------------------------------------------------------------------------------------------------------------------------------
    ifx -o test_avx_cvec4_mul -fp-model fast=2 -ftz -O3  -march=skylake-avx512      -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=3 \ 
    GMS_kinds.f90 GMS_vectypes.f90 GMS_constants.f90 GMS_vecconsts.f90 GMS_sse_cvec2.f90 GMS_intrinsics_wrappers.o test_avx_cvec4_mul.f90

    For assembly only:
    ifort -S  test_avx_cvec4_mul -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90 GMS_vectypes.f90 GMS_constants.f90 GMS_vecconsts.f90 GMS_sse_cvec2.f90 GMS_intrinsics_wrappers.o test_avx_cvec4_mul.f90
#endif

      contains 

subroutine unit_test_ymm4c8_mul_ymm4c8()
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
              character(len=60),         parameter  :: header = "[TEST #1:  ymm4c8_mul_ymm4c8 -- START]"
              character(len=60),         parameter  :: footer = "[TEST #1:  ymm4c8_mul_ymm4c8 -- END]  "
              type(YMM4c8),              automatic  :: IQ1,IQ2,IQ3 
              !dir$ attributes align : 32 :: IQ1 
              !dir$ attributes align : 32 :: IQ2 
              !dir$ attributes align : 32 :: IQ3
              type(YMM4r8_t),            automatic ::  xq1r,xq1i 
              type(YMM4r8_t),            automatic ::  xq2r,xq2i 
              !dir$ attributes align : 32 :: xq1r
              !dir$ attributes align : 32 :: xq1i
              !dir$ attributes align : 32 :: xq2r
              !dir$ attributes align : 32 :: xq2i
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
              call random_init(.false.,.false.)
              call random_number(xq1r.v)
              call random_number(xq1i.v)
              call random_number(xq2r.v)
              call random_number(xq2i.v)
              IQ1         = YMM4c8(xq1r.v,xq1i.v)
              IQ2         = YMM4c8(xq2r.v,xq2i.v)
              start       = rdtsc_wrap()
              IQ3         = IQ1*IQ2 
              end         = rdtsc_wrap()
              start_c     = start-RDTSC_LATENCY
              end_c       = end-RDTSC_LATENCY
              tsc_elapsed = end_c-start_c 
              print*,"[INPUT]:   IQ1.re=",IQ1.re
              print*,"[INPUT]:   IQ1.im=",IQ1.im
              print*,"[INPUT]:   IQ2.re=",IQ2.re
              print*,"[INPUT]:   IQ2.im=",IQ2.im
              print*,"[OUTPUT]:  IQ3.re=",IQ3.re
              print*,"[OUTPUT]:  IQ3.im=",IQ3.im
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              print*, footer
end subroutine unit_test_ymm4c8_mul_ymm4c8


subroutine unit_test_ymm4c8_mul_ymm4c8_field()
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
              character(len=80),         parameter  :: header  = "[TEST #2:  ymm4c8_mul_ymm4c8 (field) -- START]"
              character(len=80),         parameter  :: footer  = "[TEST #2:  ymm4c8_mul_ymm4c8 (field) -- END]  "
              character(len=40),         parameter  :: OUTFILE = "OUTPUT_ymm4c8_mul_ymm4c8_field.dat"
              type(YMM4c8),dimension(:),allocatable :: IQ1
              type(YMM4c8),dimension(:),allocatable :: IQ2 
              type(YMM4c8),dimension(:),allocatable :: IQ3 
              !dir$ attributes align : 32 :: IQ1 
              !dir$ attributes align : 32 :: IQ2 
              !dir$ attributes align : 32 :: IQ3
              integer(kind=i4),          automatic  :: nvecs 
              integer(kind=i4),          automatic  :: i__
              integer(kind=i4),          parameter  :: iounit = 102 
              integer(kind=i4),          automatic  :: ioerr
              integer(kind=i4),          automatic  :: start,end 
              integer(kind=i4),          automatic  :: start_c,end_c 
              integer(kind=i4),          automatic  :: tsc_elapsed 
              real(kind=dp),             automatic  :: x0r,x0i,x1r,x1i
              real(kind=dp),             automatic  :: x2r,x2i,x3r,x3i   
              real(kind=dp),             automatic  :: y0r,y0i,y1r,y1i 
              real(kind=dp),             automatic  :: y2r,y2i,y3r,y3i
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
              allocate(IQ1(nvecs))
              allocate(IQ2(nvecs))
              allocate(IQ3(nvecs))
              call random_init(.false.,.false.)
              do i__=1,nvecs 
                    call random_number(x0r)
                    call random_number(x1r)
                    IQ1(i__).re(0) = x0r 
                    IQ1(i__).re(1) = x1r 
                    call random_number(y0r)
                    call random_number(y1r)
                    IQ2(i__).re(0) = y0r 
                    IQ2(i__).re(1) = y1r 
                    call random_number(x0i)
                    call random_number(x1i)
                    IQ1(i__).im(0) = x0i 
                    IQ1(i__).im(1) = x1i
                    call random_number(y0i)
                    call random_number(y1i)
                    IQ2(i__).im(0) = y0i 
                    IQ2(i__).im(1) = y1i 
                    !******************
                    call random_number(x2r)
                    call random_number(x3r)
                    IQ1(i__).re(2) = x2r 
                    IQ1(i__).re(3) = x3r 
                    call random_number(y2r)
                    call random_number(y3r)
                    IQ2(i__).re(2) = y2r 
                    IQ2(i__).re(3) = y3r 
                    call random_number(x2i)
                    call random_number(x3i)
                    IQ1(i__).im(2) = x2i 
                    IQ1(i__).im(3) = x3i
                    call random_number(y2i)
                    call random_number(y3i)
                    IQ2(i__).im(2) = y2i 
                    IQ2(i__).im(3) = y3i 
              end do   
              !print*,IQ1 
              !print*,IQ2  
              start       = rdtsc_wrap()
!$omp simd linear(i__:1) aligned(IQ1,IQ2,IQ3:32)
              do i__ = 1,nvecs 
                 IQ3(i__)   = IQ1(i__)*IQ2(i__)
              end do  
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
                 write(IOUNIT,'(A60)') "[OUTPUT-START]: ymm4c8_mul_ymm4c8 -- field"
                 write(IOUNIT,'(T14,A3,T36,A3,T57,A3,T79,A3,T101,A3,T121,A3,T142,A3,T163,A3)') "re0","im0","re1","im1","re2","im2","re3","im3"
                 do i__=1,nvecs  
                    
                       write(IOUNIT,'(8F22.15)') IQ3(i__).re(0),IQ3(i__).im(0),IQ3(i__).re(1),IQ3(i__).im(1), &
                                                 IQ3(i__).re(2),IQ3(i__).im(2),IQ3(i__).re(3),IQ3(i__).im(3)
                 end do 
                 write(IOUNIT,'(A60)') "[OUTPUT-END]:   ymm4c8_mul_ymm4c8 -- field"
              end if  
              close(IOUNIT,STATUS='KEEP')
#endif 
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              deallocate(IQ1)
              deallocate(IQ2)
              deallocate(IQ3)
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
end subroutine unit_test_ymm4c8_mul_ymm4c8_field

subroutine unit_test_ymm4c8_mul_c1()
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
              character(len=60),         parameter  :: header = "[TEST #4:  ymm4c8_mul_c1 -- START]"
              character(len=60),         parameter  :: footer = "[TEST #4:  ymm4c8_mul_c1 -- END]  "
              type(YMM4c8),            automatic  :: IQ1,IQ2
              !dir$ attributes align : 32 :: IQ1 
              !dir$ attributes align : 32 :: IQ2 
              type(YMM4r8_t),            automatic  :: xq1r,xq1i 
              !dir$ attributes align : 32 :: xq1r
              !dir$ attributes align : 32 :: xq1i
              complex(kind=dp),          automatic  :: c1
              real(kind=dp),             automatic  :: cr,ci 
              integer(kind=i4),          automatic  :: start,end 
              integer(kind=i4),          automatic  :: start_c,end_c 
              integer(kind=i4),          automatic  :: tsc_elapsed 
             
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
              call random_number(cr)
              call random_number(ci)
              c1          = cmplx(cr,ci,kind=dp)
              call random_number(xq1r.v)
              call random_number(xq1i.v)
              IQ1         = YMM4c8(xq1r.v,xq1i.v)
              start       = rdtsc_wrap()
              IQ2         = IQ1*c1 
              end         = rdtsc_wrap()
              start_c     = start-RDTSC_LATENCY
              end_c       = end-RDTSC_LATENCY
              tsc_elapsed = end_c-start_c 
              print*,"[INPUT]:   IQ1.re=",IQ1.re
              print*,"[INPUT]:   IQ1.im=",IQ1.im
              print*,"[INPUT]:   c1.re=",real(c1)
              print*,"[INPUT]:   c1.im=",aimag(c1)
              print*,"[OUTPUT]:  IQ2.re=",IQ2.re
              print*,"[OUTPUT]:  IQ2.im=",IQ2.im
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              print*, footer
end subroutine unit_test_ymm4c8_mul_c1 


subroutine unit_test_ymm4c8_mul_c1_field()
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
              character(len=60),         parameter  :: OUTFILE = "OUTPUT_ymm4c8_mul_c1-field.dat"
              character(len=60),         parameter  :: header = "[TEST #5:  ymm4c8_mul_c1 (field) -- START]"
              character(len=60),         parameter  :: footer = "[TEST #5:  ymm4c8_mul_c1 (field) -- END]  "
              type(YMM4c8), dimension(:), allocatable :: IQ1
              type(YMM4c8), dimension(:), allocatable :: IQ2
              !dir$ attributes align : 32 :: IQ1 
              !dir$ attributes align : 32 :: IQ2 
              type(YMM4r8_t),            automatic  :: xq1r,xq1i 
              !dir$ attributes align : 32 :: xq1r
              !dir$ attributes align : 32 :: xq1i
              complex(kind=dp), dimension(:), allocatable :: c1
              !dir$ attributes align : 8 :: c1 
              
              real(kind=dp),             automatic  :: cr,ci 
              integer(kind=i4),          automatic  :: i__ 
              integer(kind=i4),          automatic  :: nvecs 
              integer(kind=i4),          parameter  :: iounit = 102 
              integer(kind=i4),          automatic  :: ioerr
              integer(kind=i4),          automatic  :: start,end 
              integer(kind=i4),          automatic  :: start_c,end_c 
              integer(kind=i4),          automatic  :: tsc_elapsed 
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
              call random_init(.false.,.false.)
              nvecs       = set_random_size()
              allocate(IQ1(nvecs))
              allocate(IQ2(nvecs))
              allocate(c1(nvecs))
              do i__=1,nvecs 
                 call random_number(cr)
                 call random_number(ci)
                 c1(i__) = cmplx(cr,ci,kind=dp)
                 call random_number(xq1r.v)
                 call random_number(xq1i.v)
                 IQ1(i__) = ymm4r82x_init(xq1r,xq1i)
              end do 
              start       = rdtsc_wrap()
!$omp         simd linear(i__:1) aligned(IQ1,IQ2:32) aligned(c1:8)
CADD_LOOP:    do i__=1,nvecs 
                  IQ2(i__) = IQ1(i__)*c1(i__)
              end do CADD_LOOP
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
                 write(IOUNIT,'(A60)') "[OUTPUT-START]: ymm4c8_mul_c1 -- field"
                 write(IOUNIT,'(T14,A3,T36,A3,T57,A3,T79,A3,T101,A3,T121,A3,T142,A3,T163,A3)') "re0","im0","re1","im1","re2","im2","re3","im3"
                 do i__=1,nvecs  
                    
                       write(IOUNIT,'(8F22.15)') IQ2(i__).re(0),IQ2(i__).im(0),IQ2(i__).re(1),IQ2(i__).im(1), &
                                                 IQ2(i__).re(2),IQ2(i__).im(2),IQ2(i__).re(3),IQ2(i__).im(3) 
                 end do 
                 write(IOUNIT,'(A60)') "[OUTPUT-END]:   ymm4c8_mul_c1 -- field"
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
              if(allocated(IQ1)) deallocate(IQ1)
              if(allocated(IQ2)) deallocate(IQ2)
              if(allocated(c1))  deallocate(c1)
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
end subroutine unit_test_ymm4c8_mul_c1_field


subroutine unit_test_ymm4c8_mul_ymm4r8_field()
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
              character(len=60),         parameter  :: OUTFILE = "OUTPUT_ymm4c8_mul_ymm4r8-field.dat"
              character(len=60),         parameter  :: header = "[TEST #6:  ymm4c8_mul_ymm4r8 (field) -- START]"
              character(len=60),         parameter  :: footer = "[TEST #6:  ymm4c8_mul_ymm4r8 (field) -- END]  "
              type(YMM4c8), dimension(:), allocatable :: IQ1
              type(YMM4c8), dimension(:), allocatable :: IQ2
              type(YMM4r8_t), dimension(:), allocatable :: Q 
              !dir$ attributes align : 32 :: IQ1 
              !dir$ attributes align : 32 :: IQ2 
              !dir$ attributes align : 32 :: Q 
              !type(YMM4r8_t),            automatic  :: q1r,q1i 
              !!dir$ attributes align : 32 :: xq1r
              !!dir$ attributes align : 32 :: xq1i
              type(YMM4r8_t),            automatic  :: qr
              !dir$ attributes align : 32 :: qr
              real(kind=dp),             automatic  :: qr0
              real(kind=dp),             automatic  :: qr1 
              real(kind=dp),             automatic  :: qr2 
              real(kind=dp),             automatic  :: qr3  
              real(kind=dp),             automatic  :: q1r0,q1r1
              real(kind=dp),             automatic  :: q1i0,q1i1 
              real(kind=dp),             automatic  :: q2r0,q2r1
              real(kind=dp),             automatic  :: q2i0,q2i1 
              integer(kind=i4),          automatic  :: i__ 
              integer(kind=i4),          automatic  :: nvecs 
              integer(kind=i4),          parameter  :: iounit = 102 
              integer(kind=i4),          automatic  :: ioerr
              integer(kind=i4),          automatic  :: start,end 
              integer(kind=i4),          automatic  :: start_c,end_c 
              integer(kind=i4),          automatic  :: tsc_elapsed 
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
              call random_init(.false.,.false.)
              nvecs       = set_random_size()
              allocate(IQ1(nvecs))
              allocate(IQ2(nvecs))
              allocate(Q(nvecs))
INIT_LOOP:    do i__=1,nvecs 
                 call random_number(qr0)
                 call random_number(qr1)
                 call random_number(qr2)
                 call random_number(qr3)
                 Q(i__) = YMM4r8_t([qr0,qr1,qr2,qr3]) 
                 call random_number(q1r0)
                 call random_number(q1r1)
                 call random_number(q1i0)
                 call random_number(q1i1)
                 call random_number(q2r0)
                 call random_number(q2r1)
                 call random_number(q2i0)
                 call random_number(q2i1)
                 IQ1(i__) = YMM4c8([q1r0,q1r1,q2r0,q2r1],[q1i0,q1i1,q2i0,q2i1])
              end do INIT_LOOP
              start       = rdtsc_wrap()
!$omp         simd linear(i__:1) aligned(qr,IQ1,IQ2:32) 
CADD_LOOP:    do i__=1,nvecs 
                 IQ2(i__) = IQ1(i__)*Q(i__)
              end do CADD_LOOP
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
                 write(IOUNIT,'(A60)') "[OUTPUT-START]: ymm4c8_mul_ymm4r8 -- field"
                 write(IOUNIT,'(T14,A3,T36,A3,T57,A3,T79,A3,T101,A3,T121,A3,T142,A3,T163,A3)') "re0","im0","re1","im1","re2","im2","re3","im3"
                 do i__=1,nvecs  
                    
                       write(IOUNIT,'(8F22.15)') IQ2(i__).re(0),IQ2(i__).im(0),IQ2(i__).re(1),IQ2(i__).im(1), &
                                                 IQ2(i__).re(2),IQ2(i__).im(2),IQ2(i__).re(3),IQ2(i__).im(3) 
                 end do 
                 write(IOUNIT,'(A60)') "[OUTPUT-END]:   ymm4c8_mul_ymm4r8 -- field"
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
              if(allocated(IQ1)) deallocate(IQ1)
              if(allocated(IQ2)) deallocate(IQ2)
              if(allocated(Q))  deallocate(Q)
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
end subroutine unit_test_ymm4c8_mul_ymm4r8_field



end module mod_test_avx_cvec4_mul


program main 
   use mod_test_avx_cvec4_mul
   call unit_test_ymm4c8_mul_ymm4c8()
   call unit_test_ymm4c8_mul_ymm4c8_field()
   call unit_test_ymm4c8_mul_c1()
   call unit_test_ymm4c8_mul_c1_field()
   call unit_test_ymm4c8_mul_ymm4r8_field()
end program main 