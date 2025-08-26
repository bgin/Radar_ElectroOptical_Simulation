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

module mod_test_AM_broadband_signal_modulation 


       use mod_kinds,                 only : i4, i8, sp 
       use iso_c_binding,             only : c_int, c_long_long 
       use mod_AM_broadband_signal

       implicit none 
       integer(kind=c_long_long),       parameter :: RDTSC_LATENCY = 18 ! for Skylake arch.
       integer(kind=c_long_long),       parameter :: ZERO          = 0_c_long_long 
       

#if 0
    ICC and ifort commands
    icc -c -std=c99 GMS_intrinsics_wrappers.c
    ifort -o test_AM_broadband_signal_modulation -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90  GMS_intrinsics_wrappers.o GMS_AM_broadband_signal.f90 test_AM_broadband_signal_modulate.f90
    -------------------------------------------------------------------------------------------------------------------------------------------------
    ifx -o test_AM_broadband_signal_modulation -fp-model fast=2 -ftz -O3  -march=skylake-avx512      -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=3 \ 
    GMS_kinds.f90 GMS_intrinsics_wrappers.o GMS_AM_broadband_signal.f90 test_AM_broadband_signal_modulate.f90

    For assembly only:
    ifort -S   -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90 GMS_intrinsics_wrappers.o GMS_AM_broadband_signal.f90 test_AM_broadband_signal_modulate.f90
#endif

      contains 


subroutine unit_test_AM_broadband_signal_modulation()
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
               character(len=128),          automatic :: filename 
#if 0
               integer(c_int),              parameter :: SIGTRAP = 5 
#endif
               character(len=64),           parameter  :: header = "[TEST #1: modulate_AM_broadband_signal -- START]"
               character(len=64),           parameter  :: footer = "[TEST #1: modulate_AM_broadband_signal -- END]"
               integer(kind=c_long_long),   automatic  :: start,end 
               integer(kind=c_long_long),   automatic  :: start_c,end_c 
               integer(kind=c_long_long),   automatic  :: tsc_elapsed 
               type(AM_broadband_signal_t), automatic  :: baseband_sig
               complex(kind=sp),            automatic  :: A0
               complex(kind=sp),            automatic  :: Ac 
               integer(kind=i4),            automatic  :: i__ 
               real(kind=sp),               automatic  :: rand_r 
               integer(kind=i4),            automatic  :: trunc_r 
               integer(kind=i4),            automatic  :: ioerr !0 == success, -1,-2,... equal failure
#if 0
               baseband_sig.m_signal_name    = "Amplitude_Modulated_wideband_signal"
               baseband_sig.m_envelope_type  = "Trapezoidal"
               baseband_sig.m_distro_omega   = "Gaussian"
               baseband_sig.m_distro_theta   = "Uniform"
               baseband_sig.m_code_type      = "1,-1"
               baseband_sig.m_id             = 1 
               baseband_sig.m_interval_1     = 0
               baseband_sig.m_interval_2     = 0
               baseband_sig.m_interval_3     = 0
               baseband_sig.m_baude_rate     = 64 
               baseband_sig.m_Ns             = 0 
               baseband_sig.m_Ne             = 63 
               baseband_sig.m_Ts             = 0
               baseband_sig.m_Te             = 255 
               baseband_sig.m_nfreqs         = 0
               baseband_sig.m_nfreqe         = 64 
               baseband_sig.m_nomegs         = 0
               baseband_sig.m_nomege         = 63 
               baseband_sig.m_nthets         =  0
               baseband_sig.m_nthete         = 1023
               baseband_sig.m_sym_dep        = .false. 
               baseband_sig.m_split_carrier  = .true. 
               baseband_sig.m_split_envelope = .true. 
               baseband_sig.m_ft_process     = .true.
               baseband_sig.m_order          = 1
               baseband_sig.m_A0             = cmplx(1.0_sp,0.0_sp)
               baseband_sig.m_fc             = 3.0e+9_sp 
#endif 
               print*, header 
               A0 = cmplx(1.0_sp,1.0_sp)
               Ac = cmplx(1.5_sp,1.25_sp)
               start       = rdtsc_wrap()
               call create_AM_broadband_signal(baseband_sig,"AM_wideband_signal", &
                                               "Trapezoidal","Gaussian","Uniform","1,-1",1,0,0,0, &
                                               64,0,63,0,255,0,63,0,63,0,1023,.false.,.false.,.true.,.true.,    &
                                               1,A0,Ac,3.4589e+9_sp)
               end         = rdtsc_wrap()
               start_c     = start-RDTSC_LATENCY
               end_c       = end-RDTSC_LATENCY
               tsc_elapsed = end_c-start_c 
               if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
               else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*, "create_AM_broadband_signal"
                 print*,"[TSC]=", tsc_elapsed 
               end if 
               baseband_sig.m_invT = 1.0_sp/real(baseband_sig.m_Te-baseband_sig.m_Ts)
               call random_seed()
               do i__=1,baseband_sig.m_baude_rate
                  call random_number(rand_r)
                  trunc_r = floor(rand_r*2.0_sp)
                  if(trunc_r .eq. 1) then
                     baseband_sig.m_code_seq(i__) = 1.0_sp
                  else if(trunc_r .eq. 0) then 
                     baseband_sig.m_code_seq(i__) = -1.0_sp
                  end if 
               end do 
               print*, baseband_sig.m_code_seq
               ioerr = 1
               print*,"[UNIT-TEST #1] -- Calling: modulate_AM_broadband_signal."
               start   = rdtsc_wrap()
               call modulate_AM_broadband_signal(baseband_sig.m_envelope_type,     &
                                                 baseband_sig.m_baude_rate,        &
                                                 baseband_sig.m_Ns,                &
                                                 baseband_sig.m_Ne,                &
                                                 baseband_sig.m_Ts,                &
                                                 baseband_sig.m_Te,                &
                                                 baseband_sig.m_A0,                &
                                                 baseband_sig.m_Ac,                &
                                                 baseband_sig.m_invT,              &
                                                 baseband_sig.m_fc,                &
                                                 baseband_sig.m_code_seq,          &
                                                 baseband_sig.m_carrier,           &
                                                 baseband_sig.m_complex_env,       &
                                                 baseband_sig.m_signal,            &
                                                 baseband_sig.m_samples,           &
                                                 baseband_sig.m_carrier_i,         &
                                                 baseband_sig.m_carrier_q,         &
                                                 baseband_sig.m_signal_i,          &
                                                 baseband_sig.m_signal_q,          &
                                                 baseband_sig.m_complex_env_i,     &
                                                 baseband_sig.m_complex_env_q,     &
                                                 ioerr)
               end         = rdtsc_wrap()
               start_c     = start-RDTSC_LATENCY
               end_c       = end-RDTSC_LATENCY
               tsc_elapsed = end_c-start_c 
               if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
               else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*, "modulate_AM_broadband_signal"
                 print*,"[TSC]=", tsc_elapsed 
               end if 
               call destroy_AM_broadband_signal(baseband_sig,.true.)
               print*, footer 
end subroutine unit_test_AM_broadband_signal_modulation



end module mod_test_AM_broadband_signal_modulation


program main 
   use mod_test_AM_broadband_signal_modulation
   call unit_test_AM_broadband_signal_modulation()
end program main 