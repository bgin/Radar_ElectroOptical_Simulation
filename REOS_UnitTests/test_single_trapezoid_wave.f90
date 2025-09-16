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

module mod_test_single_trapezoid_waveform 


       use mod_kinds,                 only : i4, i8, sp 
       use iso_c_binding,             only : c_int, c_long_long 
       use mod_trapezoid_waveform

       implicit none 
       integer(kind=c_long_long),       parameter :: RDTSC_LATENCY = 18 ! for Skylake arch.
       integer(kind=c_long_long),       parameter :: ZERO          = 0_c_long_long 
       

#if 0
    ICC and ifort commands
    icc -c -std=c99 GMS_intrinsics_wrappers.c
    ifort -o test_single_trapezoid_wave -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90  GMS_intrinsics_wrappers.o GMS_trapezoid_waveform.f90 test_single_trapezoid_wave.f90
    -------------------------------------------------------------------------------------------------------------------------------------------------
    ifx -o test_single_trapezoid_wave -fp-model fast=2 -ftz -O3  -march=skylake-avx512      -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=3 \ 
    GMS_kinds.f90 GMS_intrinsics_wrappers.o GMS_kinds.f90  GMS_intrinsics_wrappers.o GMS_trapezoid_waveform.f90 test_single_trapezoid_wave.f90

    For assembly only:
    ifort -S   -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90  GMS_intrinsics_wrappers.o GMS_trapezoid_waveform.f90 test_single_trapezoid_wave.f90
#endif


      contains 

      subroutine unit_test_single_trapezoid_waveform()
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

               interface 
                   function rdtsc_wrap() bind(C,name="rdtsc_wrap")
                            use iso_c_binding, only : c_long_long 
                            integer(c_long_long) :: rdtsc_wrap 
                    end function rdtsc_wrap 
               end interface
               character(len=256),          automatic  :: emsg
               character(len=128),          automatic  :: filename 
               character(len=80),           parameter  :: OUTFILE = "single_trapezoidal_wave_test_1_"//__DATE__//":"//__TIME__//".txt"
               integer(c_int),              parameter  :: SIGTRAP = 5 
               character(len=64),           parameter  :: header = "[TEST #5: single_trapezoid_wave -- START]"
               character(len=64),           parameter  :: footer = "[TEST #5: single_trapezoid_wave -- END]"
#if 0
               integer(kind=c_long_long),   automatic  :: start,end 
               integer(kind=c_long_long),   automatic  :: start_c,end_c 
               integer(kind=c_long_long),   automatic  :: tsc_elapsed 
#endif 
               integer(kind=i4),            parameter       :: iounit = 102 
               type(trapezoid_waveform_t),  automatic       :: waveform_1__ 
               real(kind=sp),               automatic       :: a 
               real(kind=sp),               automatic       :: m 
               real(kind=sp),               automatic       :: l 
               real(kind=sp),               automatic       :: c 
               integer(kind=i4),            automatic       :: filerr
               integer(kind=i4),            automatic       :: i__ 
               logical(kind=i1),            automatic       :: ioflag
               a = 10.0_sp 
               m = 5.0_sp 
               l = 5.0_sp 
               c = 2.0_sp 

               call construct(waveform_1__,1024,32,32,32,32,32)
               call single_trapezoid_wave(waveform_1__.tw_samples__,waveform_1__.n_samples__, &
                                          a,l,c,m)
#if 1
              filerr = 0
              open(UNIT=IOUNIT,FILE=OUTFILE,IOMSG=emsg,ACCESS="SEQUENTIAL",STATUS="NEW")
              ioflag = (filerr==0)
              if(.not.ioflag) then 
                 print*, "[ERROR] -- OPEN failed an error message is as follows:"
                 print*, emsg 
                 return 
              else 
                 !write(IOUNIT,'(A80)') "[OUTPUT-START]: modulate_add_noise_AM_broadband_signal"
                 !write(IOUNIT,'(T14,A3,T36,A3)') "re","im"
                 do i__=0,waveform_1__.n_samples__-1
                    write(IOUNIT,'(1F22.15)') waveform_1__.tw_samples__(i__)
                end do 
                 !write(IOUNIT,'(A80)') "[OUTPUT-END]: modulate_add_noise_AM_broadband_signal"
              end if 
              close(IOUNIT,STATUS='KEEP')
#endif  
              call destroy(waveform_1__,.true.)
      end subroutine unit_test_single_trapezoid_waveform


end module mod_test_single_trapezoid_waveform 


program main 
   use mod_test_single_trapezoid_waveform
   call unit_test_single_trapezoid_waveform()
end program main 