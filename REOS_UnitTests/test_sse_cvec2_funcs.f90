

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

module mod_test_sse_cvec2_funcs 



       use mod_kinds,                 only : i1, i4, dp 
       use iso_c_binding,             only : c_int, c_long_long 
       use mod_vectypes,              only : XMM2r8_t
       use sse_cvec2
       implicit none 

       integer(kind=c_long_long),       parameter :: RDTSC_LATENCY = 18 ! for Skylake arch.
       integer(kind=c_long_long),       parameter :: ZERO          = 0_c_long_long 
       

#if 0
    ICC and ifort commands
    icc -c -std=c99 GMS_intrinsics_wrappers.c
    ifort -o test_sse_cvec2_funcs -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90 GMS_vectypes.f90 GMS_constants.f90 GMS_vecconsts.f90 GMS_sse_cvec2.f90 GMS_intrinsics_wrappers.o test_sse_cvec2_funcs.f90
    -------------------------------------------------------------------------------------------------------------------------------------------------
    ifx -o test_sse_cvec2_funcs -fp-model fast=2 -ftz -O3  -march=skylake-avx512   \
    -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=3  GMS_kinds.f90 GMS_vectypes.f90 GMS_constants.f90 GMS_vecconsts.f90 GMS_sse_cvec2.f90 GMS_intrinsics_wrappers.o test_sse_cvec2_funcs.f90

    For assembly only:
    ifort -S  test_sse_cvec2_funcs -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90 GMS_vectypes.f90 GMS_constants.f90 GMS_vecconsts.f90 GMS_sse_cvec2.f90 GMS_intrinsics_wrappers.o test_sse_cvec2_funcs.f90
#endif

      contains 




subroutine unit_test_xmm2c8_polar_field()
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
              character(len=80),         parameter  :: header  = "[TEST #1:  xmm2c8_polar (field) -- START]"
              character(len=80),         parameter  :: footer  = "[TEST #1:  xmm2c8_polar (field) -- END]  "
              character(len=40),         parameter  :: OUTFILE = "OUTPUT_xmm2c8_polar_field.dat"
              type(XMM2c8_t),dimension(:),allocatable :: IQ
              type(XMM2r8_t),dimension(:),allocatable :: rho
              type(XMM2r8_t),dimension(:),allocatable :: theta  
              !dir$ attributes align : 16 :: IQ
              !dir$ attributes align : 16 :: rho 
              !dir$ attributes align : 16 :: theta 
              integer(kind=i4),          automatic  :: nvecs 
              integer(kind=i4),          automatic  :: i__
              integer(kind=i4),          parameter  :: iounit = 102 
              integer(kind=i4),          automatic  :: ioerr
              integer(kind=i4),          automatic  :: start,end 
              integer(kind=i4),          automatic  :: start_c,end_c 
              integer(kind=i4),          automatic  :: tsc_elapsed 
              real(kind=dp),             automatic  :: x0r,x1r 
              real(kind=dp),             automatic  :: y0r,y1r 
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
              allocate(IQ(nvecs))
              allocate(rho(nvecs))
              allocate(theta(nvecs))
              call random_init(.false.,.false.)
              do i__=1,nvecs 
                    call random_number(x0r)
                    call random_number(x1r)
                    rho(i__)   = XMM2r8_t([x0r,x1r])
                    call random_number(y0r)
                    call random_number(y1r)
                    theta(i__) = XMM2r8_t([y0r,y1r]) 
              end do   
               
              start       = rdtsc_wrap()
!$omp simd linear(i__:1) aligned(IQ,rho,theta:16)
              do i__ = 1,nvecs 
                 IQ(i__)   = polar(rho(i__),theta(i__))
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
                 write(IOUNIT,'(A64)') "[OUTPUT-START]: xmm2c8_polar -- field"
                 write(IOUNIT,'(T14,A3,T36,A3,T57,A3,T79,A3)') "re0","im0","re1","im1"
                 do i__=1,nvecs  
                    
                       write(IOUNIT,'(4F22.15)') IQ(i__).re(0),IQ(i__).im(0),IQ(i__).re(1),IQ(i__).im(1)
                                         
                 end do 
                 write(IOUNIT,'(A64)') "[OUTPUT-END]:   xmm2c8_polar -- field"
              end if  
              close(IOUNIT,STATUS='KEEP')
#endif 
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              deallocate(IQ)
              deallocate(rho)
              deallocate(theta)
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
end subroutine unit_test_xmm2c8_polar_field


subroutine unit_test_xmm2c8_carg_field()
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
              character(len=80),         parameter  :: header  = "[TEST #2:  xmm2c8_carg (field) -- START]"
              character(len=80),         parameter  :: footer  = "[TEST #2:  xmm2c8_carg (field) -- END]  "
              character(len=40),         parameter  :: OUTFILE = "OUTPUT_xmm2c8_carg_field.dat"
              type(XMM2c8_t),dimension(:),allocatable :: IQ
              type(XMM2r8_t),dimension(:),allocatable :: arg 
              
              !dir$ attributes align : 16 :: IQ
              !dir$ attributes align : 16 :: arg
              
              integer(kind=i4),          automatic  :: nvecs 
              integer(kind=i4),          automatic  :: i__
              integer(kind=i4),          parameter  :: iounit = 102 
              integer(kind=i4),          automatic  :: ioerr
              integer(kind=i4),          automatic  :: start,end 
              integer(kind=i4),          automatic  :: start_c,end_c 
              integer(kind=i4),          automatic  :: tsc_elapsed 
              real(kind=dp),             automatic  :: x1r,x0r  
              real(kind=dp),             automatic  :: y1r,y0r 
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
              allocate(IQ(nvecs))
              allocate(arg(nvecs))
             
              call random_init(.false.,.false.)
              do i__=1,nvecs 
                    call random_number(x0r)
                    call random_number(x1r)
                    call random_number(y0r)
                    call random_number(y1r)
                    IQ(i__)  = XMM2c8_t([x0r,x1r],[y0r,y1r])
              end do   
               
              start       = rdtsc_wrap()
!$omp simd linear(i__:1) aligned(arg,IQ:16)
              do i__ = 1,nvecs 
                 arg(i__)   = carg_xmm2c8(IQ(i__))
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
                 write(IOUNIT,'(A64)') "[OUTPUT-START]: xmm2c8_carg -- field"
                 write(IOUNIT,'(T14,A3,T36,A3)') "re0","re1"
                 do i__=1,nvecs  
                    
                       write(IOUNIT,'(2F22.15)') arg(i__).v(0),arg(i__).v(1)
                                         
                 end do 
                 write(IOUNIT,'(A64)') "[OUTPUT-END]:   xmm2c8_carg -- field"
              end if  
              close(IOUNIT,STATUS='KEEP')
#endif 
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              deallocate(IQ)
              deallocate(arg)
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
end subroutine unit_test_xmm2c8_carg_field


subroutine unit_test_xmm2c8_csin_field()
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
              character(len=80),         parameter  :: header  = "[TEST #3:  xmm2c8_csin (field) -- START]"
              character(len=80),         parameter  :: footer  = "[TEST #3:  xmm2c8_csin (field) -- END]  "
              character(len=40),         parameter  :: OUTFILE = "OUTPUT_xmm2c8_csin_field.dat"
              type(XMM2c8_t),dimension(:),allocatable :: arg
              type(XMM2c8_t),dimension(:),allocatable :: csin  
              
              !dir$ attributes align : 16 :: csin
              !dir$ attributes align : 16 :: arg
              
              integer(kind=i4),          automatic  :: nvecs 
              integer(kind=i4),          automatic  :: i__
              integer(kind=i4),          parameter  :: iounit = 102 
              integer(kind=i4),          automatic  :: ioerr
              integer(kind=i4),          automatic  :: start,end 
              integer(kind=i4),          automatic  :: start_c,end_c 
              integer(kind=i4),          automatic  :: tsc_elapsed 
              real(kind=dp),             automatic  :: x0r,x0i 
              real(kind=dp),             automatic  :: x1r,x1i 
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
              allocate(arg(nvecs))
              allocate(csin(nvecs))
             
              call random_init(.false.,.false.)
              do i__=1,nvecs 
                    call random_number(x0r)
                    call random_number(x0i)
                    call random_number(x1r)
                    call random_number(x1i)
                    arg(i__)  = XMM2c8_t([x0r,x0i],[x1r,x1i])
              end do   
               
              start       = rdtsc_wrap()
!$omp simd linear(i__:1) aligned(arg,csin:16)
              do i__ = 1,nvecs 
                 csin(i__)   = csin_xmm2c8(arg(i__))
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
                 write(IOUNIT,'(A64)') "[OUTPUT-START]: xmm2c8_sin -- field"
                 write(IOUNIT,'(T14,A3,T36,A3,T57,A3,T79,A3)') "re0","im0","re1","im1"
                 do i__=1,nvecs  
                    
                       write(IOUNIT,'(4F22.15)') csin(i__).re(0),csin(i__).im(0),csin(i__).re(1),csin(i__).im(1)
                                         
                 end do 
                 write(IOUNIT,'(A64)') "[OUTPUT-END]:   xmm2c8_csin -- field"
              end if  
              close(IOUNIT,STATUS='KEEP')
#endif 
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              deallocate(csin)
              deallocate(arg)
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
end subroutine unit_test_xmm2c8_csin_field


subroutine unit_test_xmm2c8_csinh_field()
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
              character(len=80),         parameter  :: header  = "[TEST #4:  xmm2c8_csinh (field) -- START]"
              character(len=80),         parameter  :: footer  = "[TEST #4:  xmm2c8_csinh (field) -- END]  "
              character(len=40),         parameter  :: OUTFILE = "OUTPUT_xmm2c8_csinh_field.dat"
              type(XMM2c8_t),dimension(:),allocatable :: arg
              type(XMM2c8_t),dimension(:),allocatable :: csinh  
              
              !dir$ attributes align : 16 :: csinh
              !dir$ attributes align : 16 :: arg
              
              integer(kind=i4),          automatic  :: nvecs 
              integer(kind=i4),          automatic  :: i__
              integer(kind=i4),          parameter  :: iounit = 102 
              integer(kind=i4),          automatic  :: ioerr
              integer(kind=i4),          automatic  :: start,end 
              integer(kind=i4),          automatic  :: start_c,end_c 
              integer(kind=i4),          automatic  :: tsc_elapsed 
              real(kind=dp),             automatic  :: x0r,x0i 
              real(kind=dp),             automatic  :: x1r,x1i 
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
              allocate(arg(nvecs))
              allocate(csinh(nvecs))
             
              call random_init(.false.,.false.)
              do i__=1,nvecs 
                    call random_number(x0r)
                    call random_number(x0i)
                    call random_number(x1r)
                    call random_number(x1i)
                    arg(i__)  = XMM2c8_t([x0r,x0i],[x1r,x1i])
              end do   
               
              start       = rdtsc_wrap()
!$omp simd linear(i__:1) aligned(arg,csinh:16)
              do i__ = 1,nvecs 
                 csinh(i__)   = csinh_xmm2c8(arg(i__))
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
                 write(IOUNIT,'(A64)') "[OUTPUT-START]: xmm2c8_csinh -- field"
                 write(IOUNIT,'(T14,A3,T36,A3,T57,A3,T79,A3)') "re0","im0","re1","im1"
                 do i__=1,nvecs  
                    
                       write(IOUNIT,'(4F22.15)') csinh(i__).re(0),csinh(i__).im(0),csinh(i__).re(1),csinh(i__).im(1)
                                         
                 end do 
                 write(IOUNIT,'(A64)') "[OUTPUT-END]:   xmm2c8_csinh -- field"
              end if  
              close(IOUNIT,STATUS='KEEP')
#endif 
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              deallocate(csinh)
              deallocate(arg)
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
end subroutine unit_test_xmm2c8_csinh_field


subroutine unit_test_xmm2c8_ccos_field()
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
              character(len=80),         parameter  :: header  = "[TEST #5:  xmm2c8_ccos (field) -- START]"
              character(len=80),         parameter  :: footer  = "[TEST #5:  xmm2c8_ccos (field) -- END]  "
              character(len=40),         parameter  :: OUTFILE = "OUTPUT_xmm2c8_ccos_field.dat"
              type(XMM2c8_t),dimension(:),allocatable :: arg
              type(XMM2c8_t),dimension(:),allocatable :: ccos  
              
              !dir$ attributes align : 16 :: ccos
              !dir$ attributes align : 16 :: arg
              
              integer(kind=i4),          automatic  :: nvecs 
              integer(kind=i4),          automatic  :: i__
              integer(kind=i4),          parameter  :: iounit = 102 
              integer(kind=i4),          automatic  :: ioerr
              integer(kind=i4),          automatic  :: start,end 
              integer(kind=i4),          automatic  :: start_c,end_c 
              integer(kind=i4),          automatic  :: tsc_elapsed 
              real(kind=dp),             automatic  :: x0r,x0i 
              real(kind=dp),             automatic  :: x1r,x1i 
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
              allocate(arg(nvecs))
              allocate(ccos(nvecs))
             
              call random_init(.false.,.false.)
              do i__=1,nvecs 
                    call random_number(x0r)
                    call random_number(x0i)
                    call random_number(x1r)
                    call random_number(x1i)
                    arg(i__)  = XMM2c8_t([x0r,x0i],[x1r,x1i])
              end do   
               
              start       = rdtsc_wrap()
!$omp simd linear(i__:1) aligned(arg,ccos:16)
              do i__ = 1,nvecs 
                 ccos(i__)   = ccos_xmm2c8(arg(i__))
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
                 write(IOUNIT,'(A64)') "[OUTPUT-START]: xmm2c8_ccos -- field"
                 write(IOUNIT,'(T14,A3,T36,A3,T57,A3,T79,A3)') "re0","im0","re1","im1"
                 do i__=1,nvecs  
                    
                       write(IOUNIT,'(4F22.15)') ccos(i__).re(0),ccos(i__).im(0),ccos(i__).re(1),ccos(i__).im(1)
                                         
                 end do 
                 write(IOUNIT,'(A64)') "[OUTPUT-END]:   xmm2c8_ccos -- field"
              end if  
              close(IOUNIT,STATUS='KEEP')
#endif 
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              deallocate(ccos)
              deallocate(arg)
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
end subroutine unit_test_xmm2c8_ccos_field


subroutine unit_test_xmm2c8_ccosh_field()
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
              character(len=80),         parameter  :: header  = "[TEST #6:  xmm2c8_ccosh (field) -- START]"
              character(len=80),         parameter  :: footer  = "[TEST #6:  xmm2c8_ccosh (field) -- END]  "
              character(len=40),         parameter  :: OUTFILE = "OUTPUT_xmm2c8_ccosh_field.dat"
              type(XMM2c8_t),dimension(:),allocatable :: arg
              type(XMM2c8_t),dimension(:),allocatable :: ccosh  
              
              !dir$ attributes align : 16 :: ccosh
              !dir$ attributes align : 16 :: arg
              
              integer(kind=i4),          automatic  :: nvecs 
              integer(kind=i4),          automatic  :: i__
              integer(kind=i4),          parameter  :: iounit = 102 
              integer(kind=i4),          automatic  :: ioerr
              integer(kind=i4),          automatic  :: start,end 
              integer(kind=i4),          automatic  :: start_c,end_c 
              integer(kind=i4),          automatic  :: tsc_elapsed 
              real(kind=dp),             automatic  :: x0r,x0i 
              real(kind=dp),             automatic  :: x1r,x1i 
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
              allocate(arg(nvecs))
              allocate(ccosh(nvecs))
             
              call random_init(.false.,.false.)
              do i__=1,nvecs 
                    call random_number(x0r)
                    call random_number(x0i)
                    call random_number(x1r)
                    call random_number(x1i)
                    arg(i__)  = XMM2c8_t([x0r,x0i],[x1r,x1i])
              end do   
               
              start       = rdtsc_wrap()
!$omp simd linear(i__:1) aligned(arg,ccosh:16)
              do i__ = 1,nvecs 
                 ccosh(i__)   = ccosh_xmm2c8(arg(i__))
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
                 write(IOUNIT,'(A64)') "[OUTPUT-START]: xmm2c8_ccosh -- field"
                 write(IOUNIT,'(T14,A3,T36,A3,T57,A3,T79,A3)') "re0","im0","re1","im1"
                 do i__=1,nvecs  
                    
                       write(IOUNIT,'(4F22.15)') ccosh(i__).re(0),ccosh(i__).im(0),ccosh(i__).re(1),ccosh(i__).im(1)
                                         
                 end do 
                 write(IOUNIT,'(A64)') "[OUTPUT-END]:   xmm2c8_ccosh -- field"
              end if  
              close(IOUNIT,STATUS='KEEP')
#endif 
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              deallocate(ccosh)
              deallocate(arg)
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
end subroutine unit_test_xmm2c8_ccosh_field


subroutine unit_test_xmm2c8_cexp_field()
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
              character(len=80),         parameter  :: header  = "[TEST #7:  xmm2c8_cexp (field) -- START]"
              character(len=80),         parameter  :: footer  = "[TEST #7:  xmm2c8_cexp (field) -- END]  "
              character(len=40),         parameter  :: OUTFILE = "OUTPUT_xmm2c8_cexp_field.dat"
              type(XMM2c8_t),dimension(:),allocatable :: arg
              type(XMM2c8_t),dimension(:),allocatable :: cexp  
              
              !dir$ attributes align : 16 :: cexp
              !dir$ attributes align : 16 :: arg
              
              integer(kind=i4),          automatic  :: nvecs 
              integer(kind=i4),          automatic  :: i__
              integer(kind=i4),          parameter  :: iounit = 102 
              integer(kind=i4),          automatic  :: ioerr
              integer(kind=i4),          automatic  :: start,end 
              integer(kind=i4),          automatic  :: start_c,end_c 
              integer(kind=i4),          automatic  :: tsc_elapsed 
              real(kind=dp),             automatic  :: x0r,x0i 
              real(kind=dp),             automatic  :: x1r,x1i 
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
              allocate(arg(nvecs))
              allocate(cexp(nvecs))
             
              call random_init(.false.,.false.)
              do i__=1,nvecs 
                    call random_number(x0r)
                    call random_number(x0i)
                    call random_number(x1r)
                    call random_number(x1i)
                    arg(i__)  = XMM2c8_t([x0r,x0i],[x1r,x1i])
              end do   
               
              start       = rdtsc_wrap()
!$omp simd linear(i__:1) aligned(arg,cexp:16)
              do i__ = 1,nvecs 
                 cexp(i__)   = cexp_xmm2c8(arg(i__))
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
                 write(IOUNIT,'(A64)') "[OUTPUT-START]: xmm2c8_cexp -- field"
                 write(IOUNIT,'(T14,A3,T36,A3,T57,A3,T79,A3)') "re0","im0","re1","im1"
                 do i__=1,nvecs  
                    
                       write(IOUNIT,'(4F22.15)') cexp(i__).re(0),cexp(i__).im(0),cexp(i__).re(1),cexp(i__).im(1)
                                         
                 end do 
                 write(IOUNIT,'(A64)') "[OUTPUT-END]:   xmm2c8_exp -- field"
              end if  
              close(IOUNIT,STATUS='KEEP')
#endif 
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              deallocate(cexp)
              deallocate(arg)
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
end subroutine unit_test_xmm2c8_cexp_field


subroutine unit_test_xmm2c8_ctan_field()
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
              character(len=80),         parameter  :: header  = "[TEST #8:  xmm2c8_ctan (field) -- START]"
              character(len=80),         parameter  :: footer  = "[TEST #8:  xmm2c8_ctan (field) -- END]  "
              character(len=40),         parameter  :: OUTFILE = "OUTPUT_xmm2c8_ctan_field.dat"
              type(XMM2c8_t),dimension(:),allocatable :: arg
              type(XMM2c8_t),dimension(:),allocatable :: ctan  
              
              !dir$ attributes align : 16 :: ctan
              !dir$ attributes align : 16 :: arg
              
              integer(kind=i4),          automatic  :: nvecs 
              integer(kind=i4),          automatic  :: i__
              integer(kind=i4),          parameter  :: iounit = 102 
              integer(kind=i4),          automatic  :: ioerr
              integer(kind=i4),          automatic  :: start,end 
              integer(kind=i4),          automatic  :: start_c,end_c 
              integer(kind=i4),          automatic  :: tsc_elapsed 
              real(kind=dp),             automatic  :: x0r,x0i 
              real(kind=dp),             automatic  :: x1r,x1i 
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
              allocate(arg(nvecs))
              allocate(ctan(nvecs))
             
              call random_init(.false.,.false.)
              do i__=1,nvecs 
                    call random_number(x0r)
                    call random_number(x0i)
                    call random_number(x1r)
                    call random_number(x1i)
                    arg(i__)  = XMM2c8_t([x0r,x0i],[x1r,x1i])
              end do   
               
              start       = rdtsc_wrap()
!$omp simd linear(i__:1) aligned(arg,ctan:16)
              do i__ = 1,nvecs 
                 ctan(i__)   = ctan_xmm2c8(arg(i__))
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
                 write(IOUNIT,'(A64)') "[OUTPUT-START]: xmm2c8_ctan -- field"
                 write(IOUNIT,'(T14,A3,T36,A3,T57,A3,T79,A3)') "re0","im0","re1","im1"
                 do i__=1,nvecs  
                    
                       write(IOUNIT,'(4F22.15)') ctan(i__).re(0),ctan(i__).im(0),ctan(i__).re(1),ctan(i__).im(1)
                                         
                 end do 
                 write(IOUNIT,'(A64)') "[OUTPUT-END]:   xmm2c8_ctan -- field"
              end if  
              close(IOUNIT,STATUS='KEEP')
#endif 
              if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
              else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*,"[TSC]=", tsc_elapsed 
              end if 
              deallocate(ctan)
              deallocate(arg)
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
end subroutine unit_test_xmm2c8_ctan_field


end module mod_test_sse_cvec2_funcs


program main 
   use mod_test_sse_cvec2_funcs
   call unit_test_xmm2c8_polar_field()
   call unit_test_xmm2c8_carg_field()
   call unit_test_xmm2c8_csin_field()
   call unit_test_xmm2c8_csinh_field()
   call unit_test_xmm2c8_ccos_field()
   call unit_test_xmm2c8_ccosh_field()
   call unit_test_xmm2c8_cexp_field()
   call unit_test_xmm2c8_ctan_field()

end program main 