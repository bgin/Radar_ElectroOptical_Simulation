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

module mod_test_AM_broadband_signal_creation 


       use mod_kinds,                 only : i4, i8, sp 
       use iso_c_binding,             only : c_int, c_long_long 
       use mod_AM_broadband_signal

       implicit none 
       integer(kind=c_long_long),       parameter :: RDTSC_LATENCY = 18 ! for Skylake arch.
       integer(kind=c_long_long),       parameter :: ZERO          = 0_c_long_long 
       

#if 0
    ICC and ifort commands
    icc -c -std=c99 GMS_intrinsics_wrappers.c
    ifort -o test_AM_broadband_signal_creation -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90  GMS_intrinsics_wrappers.o GMS_AM_broadband_signal.f90 test_AM_broadband_signal_creation.f90
    -------------------------------------------------------------------------------------------------------------------------------------------------
    ifx -o test_AM_broadband_signal_creation -fp-model fast=2 -ftz -O3  -march=skylake-avx512      -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=3 \ 
    GMS_kinds.f90 GMS_intrinsics_wrappers.o GMS_AM_broadband_signal.f90 test_AM_broadband_signal_creation.f90

    For assembly only:
    ifort -S  test_avx_cvec4_add -fp-model fast=2 -ftz -O3 -ggdb  -march=skylake-avx512 \
     -fopenmp -qopenmp -fpp -falign-functions=32 -qopt-report=5 GMS_kinds.f90 GMS_intrinsics_wrappers.o GMS_AM_broadband_signal.f90 test_AM_broadband_signal_creation.f90
#endif

      contains 
  

subroutine unit_test_AM_broadband_signal_create()
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
               integer(c_int),              parameter :: SIGTRAP = 5 
               character(len=64),           parameter  :: header = "[TEST #1: create_AM_broadband_signal -- START]"
               character(len=64),           parameter  :: footer = "[TEST #1: create_AM_broadband_signal -- END]"
               integer(kind=c_long_long),   automatic  :: start,end 
               integer(kind=c_long_long),   automatic  :: start_c,end_c 
               integer(kind=c_long_long),   automatic  :: tsc_elapsed 
               type(AM_broadband_signal_t), automatic  :: baseband_sig
               complex(kind=sp),            automatic  :: A0
#if 0
               baseband_sig.m_signal_name    = "Amplitude_Modulated_wideband_signal"
               baseband_sig.m_envelope_type  = "Sine-squared"
               baseband_sig.m_distro_omega   = "Gaussian"
               baseband_sig.m_distro_theta   = "Uniform"
               baseband_sig.m_code_type      = "1,-1"
               baseband_sig.m_id             = 1 
               baseband_sig.m_interval_1     = 0
               baseband_sig.m_interval_2     = 0
               baseband_sig.m_interval_3     = 0
               baseband_sig.m_baude_rate     = 16 
               baseband_sig.m_Ns             = 0 
               baseband_sig.m_Ne             = 15 
               baseband_sig.m_Ts             = 0
               baseband_sig.m_Te             = 128 
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
               baseband_sig.m_A0             = cmplx(1.0_sp,0.0_sp)
               baseband_sig.m_fc             = 3.0e+9_sp 
#endif 
               print*, header 
               A0 = cmplx(1.0_sp,0.0_sp)
               start       = rdtsc_wrap()
               call create_AM_broadband_signal(baseband_sig,"AM_wideband_signal", &
                                               "Sine-squared","Gaussian","Uniform","1,-1",1,0,0,0, &
                                               16,0,15,0,128,0,32,0,64,0,1023,.false.,.false.,.true.,.true.,    &
                                               A0,3.0e+9_sp)
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
               print*,"[TEST #1: Dumping object state.]"
               print*, "m_signal_name=",    baseband_sig.m_signal_name
               print*, "m_envelope_type=",  baseband_sig.m_envelope_type 
               print*, "m_distro_omega=",   baseband_sig.m_distro_omega
               print*, "m_distro_theta=",   baseband_sig.m_distro_theta
               print*, "m_code_type=",      baseband_sig.m_code_type 
               print*, "m_id=",             baseband_sig.m_id 
               print*, "m_interval_1=",     baseband_sig.m_interval_1  
               print*, "m_interval_2=",     baseband_sig.m_interval_2 
               print*, "m_interval_3=",     baseband_sig.m_interval_3
               print*, "m_baude_rate=",     baseband_sig.m_baude_rate
               print*, "m_Ns=",             baseband_sig.m_Ns 
               print*, "m_Ne=",             baseband_sig.m_Ne 
               print*, "m_Ts=",             baseband_sig.m_Ts 
               print*, "m_Te=",             baseband_sig.m_Te 
               print*, "m_nfreqs=",         baseband_sig.m_nfreqs
               print*, "m_nfreqe=",         baseband_sig.m_nfreqe 
               print*, "m_num_samples=",    baseband_sig.m_num_samples 
               print*, "m_nomegs=",         baseband_sig.m_nomegs 
               print*, "m_nomege=",         baseband_sig.m_nomege 
               print*, "m_nthets=",         baseband_sig.m_nthets 
               print*, "m_nthete",          baseband_sig.m_nthete         
               print*, "m_sym_dep=",        baseband_sig.m_sym_dep        
               print*, "m_ft_process=",     baseband_sig.m_ft_process
               print*, "m_A0=",             baseband_sig.m_A0                                
               print*, "m_invT=",           baseband_sig.m_invT           
               print*, "m_fc=",             baseband_sig.m_fc             
               print*, "m_fs=",             baseband_sig.m_fs           
               print*, "m_sig_width=",      baseband_sig.m_sig_width     
               print*, "m_sig_energy=",     baseband_sig.m_sig_energy    
               print*, "m_snr=",            baseband_sig.m_snr           ! signal-to-noise ratio
               print*, "m_Ps=",             baseband_sig.m_Ps ! SEP (symbol error probability)
               print*, "m_creation_state=", baseband_sig.m_creation_state      
               print*, footer     
     
        contains 

               subroutine check_allocation_stats_real1D(array)
                       implicit none
                       real(kind=sp), allocatable, dimension(:), intent(in) :: array 
                       ! Locals
                       integer(kind=i8), automatic :: vaddr 
                       integer(kind=i8), automatic :: size 
                       integer(kind=i4), automatic :: lo_bound
                       integer(kind=i4), automatic :: hi_bound 
                       logical(kind=i1), automatic :: is_allocated 
                       vaddr        = LOC(array)
                       size         = SIZEOF(array)
                       lo_bound     = LBOUND(array,DIM=1)
                       hi_bound     = UBOUND(array,DIM=1)
                       is_allocated = allocated(array)
                       print*,"======================INFO======================================"
                       print*, "vaddr=",vaddr,"size=",size,"allocated=",is_allocated
                       print*, "lo_bound=",lo_bound,"hi_bound=",hi_bound 
                       print*,"======================INFO======================================"
              end subroutine check_allocation_stats_real1D
              subroutine check_allocation_stats_real2D(array)
                       implicit none
                       real(kind=sp), allocatable, dimension(:,:), intent(in) :: array 
                       ! Locals
                       integer(kind=i8), automatic :: vaddr 
                       integer(kind=i8), automatic :: size 
                       integer(kind=i4), dimension(2), automatic :: lo_bound
                       integer(kind=i4), dimension(2), automatic :: hi_bound 
                       logical(kind=i1), automatic :: is_allocated 
                       vaddr        = LOC(array)
                       size         = SIZEOF(array)
                       lo_bound     = LBOUND(array)
                       hi_bound     = UBOUND(array)
                       is_allocated = allocated(array)
                       print*,"======================INFO======================================"
                       print*, "vaddr=",vaddr,"size=",size,"allocated=",is_allocated
                       print*, "lo_bound=",lo_bound,"hi_bound=",hi_bound 
                       print*,"======================INFO======================================"
              end subroutine check_allocation_stats_real2D
              subroutine check_allocation_stats_complex1D(array)
                       implicit none
                       complex(kind=sp), allocatable, dimension(:), intent(in) :: array 
                       ! Locals
                       integer(kind=i8), automatic :: vaddr 
                       integer(kind=i8), automatic :: size 
                       integer(kind=i4), automatic :: lo_bound
                       integer(kind=i4), automatic :: hi_bound 
                       logical(kind=i1), automatic :: is_allocated 
                       vaddr        = LOC(array)
                       size         = SIZEOF(array)
                       lo_bound     = LBOUND(array,DIM=1)
                       hi_bound     = UBOUND(array,DIM=1)
                       is_allocated = allocated(array)
                       print*,"======================INFO======================================"
                       print*, "vaddr=",vaddr,"size=",size,"allocated=",is_allocated
                       print*, "lo_bound=",lo_bound,"hi_bound=",hi_bound 
                       print*,"======================INFO======================================"
              end subroutine check_allocation_stats_complex1D
              subroutine check_allocation_stats_complex2D(array)
                       implicit none
                       complex(kind=sp), allocatable, dimension(:,:), intent(in) :: array 
                       ! Locals
                       integer(kind=i8), automatic :: vaddr 
                       integer(kind=i8), automatic :: size 
                       integer(kind=i4), dimension(2),automatic :: lo_bound
                       integer(kind=i4), dimension(2),automatic :: hi_bound 
                       logical(kind=i1), automatic :: is_allocated 
                       vaddr        = LOC(array)
                       size         = SIZEOF(array)
                       lo_bound     = LBOUND(array)
                       hi_bound     = UBOUND(array)
                       is_allocated = allocated(array)
                       print*,"======================INFO======================================"
                       print*, "vaddr=",vaddr,"size=",size,"allocated=",is_allocated
                       print*, "lo_bound=",lo_bound,"hi_bound=",hi_bound 
                       print*,"======================INFO======================================"
              end subroutine check_allocation_stats_complex2D
end subroutine unit_test_AM_broadband_signal_create


subroutine unit_test_AM_broadband_signal_destroy()
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
               integer(c_int),              parameter :: SIGTRAP = 5 
               character(len=64),           parameter  :: header = "[TEST #2: destroy_AM_broadband_signal -- START]"
               character(len=64),           parameter  :: footer = "[TEST #2: destroy_AM_broadband_signal -- END]"
               integer(kind=c_long_long),   automatic  :: start,end 
               integer(kind=c_long_long),   automatic  :: start_c,end_c 
               integer(kind=c_long_long),   automatic  :: tsc_elapsed 
               type(AM_broadband_signal_t), automatic  :: baseband_sig
               complex(kind=sp),            automatic  :: A0
#if 0
               baseband_sig.m_signal_name    = "Amplitude_Modulated_wideband_signal"
               baseband_sig.m_envelope_type  = "Sine-squared"
               baseband_sig.m_distro_omega   = "Gaussian"
               baseband_sig.m_distro_theta   = "Uniform"
               baseband_sig.m_code_type      = "1,-1"
               baseband_sig.m_id             = 1 
               baseband_sig.m_interval_1     = 0
               baseband_sig.m_interval_2     = 0
               baseband_sig.m_interval_3     = 0
               baseband_sig.m_baude_rate     = 16 
               baseband_sig.m_Ns             = 0 
               baseband_sig.m_Ne             = 15 
               baseband_sig.m_Ts             = 0
               baseband_sig.m_Te             = 128 
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
               baseband_sig.m_A0             = cmplx(1.0_sp,0.0_sp)
               baseband_sig.m_fc             = 3.0e+9_sp 
#endif 
               print*, header 
               A0 = cmplx(1.0_sp,0.0_sp)
               start       = rdtsc_wrap()
               call create_AM_broadband_signal(baseband_sig,"AM_wideband_signal", &
                                               "Sine-squared","Gaussian","Uniform","1,-1",1,0,0,0, &
                                               16,0,15,0,128,0,32,0,64,0,1023,.false.,.false.,.true.,.true.,    &
                                               A0,3.0e+9_sp)
               end         = rdtsc_wrap()
               start_c     = start-RDTSC_LATENCY
               end_c       = end-RDTSC_LATENCY
               tsc_elapsed = end_c-start_c 
               if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
               else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*, "[UNIT-TEST]: create_AM_broadband_signal -- TIMING"
                 print*,"[TSC]=", tsc_elapsed 
               end if 
               start      = rdtsc_wrap()
               call destroy_AM_broadband_signal(baseband_sig,.true.)
               end         = rdtsc_wrap()
               start_c     = start-RDTSC_LATENCY
               end_c       = end-RDTSC_LATENCY
               tsc_elapsed = end_c-start_c 
               if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
               else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*, "[UNIT-TEST]: destroy_AM_broadband_signal -- TIMING"
                 print*,"[TSC]=", tsc_elapsed 
               end if 
                print*,"[TEST #2: Dumping object state.]"
               print*, "m_signal_name=",    baseband_sig.m_signal_name
               print*, "m_envelope_type=",  baseband_sig.m_envelope_type 
               print*, "m_distro_omega=",   baseband_sig.m_distro_omega
               print*, "m_distro_theta=",   baseband_sig.m_distro_theta
               print*, "m_code_type=",      baseband_sig.m_code_type 
               print*, "m_id=",             baseband_sig.m_id 
               print*, "m_interval_1=",     baseband_sig.m_interval_1  
               print*, "m_interval_2=",     baseband_sig.m_interval_2 
               print*, "m_interval_3=",     baseband_sig.m_interval_3
               print*, "m_baude_rate=",     baseband_sig.m_baude_rate
               print*, "m_Ns=",             baseband_sig.m_Ns 
               print*, "m_Ne=",             baseband_sig.m_Ne 
               print*, "m_Ts=",             baseband_sig.m_Ts 
               print*, "m_Te=",             baseband_sig.m_Te 
               print*, "m_nfreqs=",         baseband_sig.m_nfreqs
               print*, "m_nfreqe=",         baseband_sig.m_nfreqe 
               print*, "m_num_samples=",    baseband_sig.m_num_samples 
               print*, "m_nomegs=",         baseband_sig.m_nomegs 
               print*, "m_nomege=",         baseband_sig.m_nomege 
               print*, "m_nthets=",         baseband_sig.m_nthets 
               print*, "m_nthete",          baseband_sig.m_nthete         
               print*, "m_sym_dep=",        baseband_sig.m_sym_dep        
               print*, "m_ft_process=",     baseband_sig.m_ft_process
               print*, "m_A0=",             baseband_sig.m_A0                                
               print*, "m_invT=",           baseband_sig.m_invT           
               print*, "m_fc=",             baseband_sig.m_fc             
               print*, "m_fs=",             baseband_sig.m_fs           
               print*, "m_sig_width=",      baseband_sig.m_sig_width     
               print*, "m_sig_energy=",     baseband_sig.m_sig_energy    
               print*, "m_snr=",            baseband_sig.m_snr           ! signal-to-noise ratio
               print*, "m_Ps=",             baseband_sig.m_Ps ! SEP (symbol error probability)
               print*, "m_creation_state=", baseband_sig.m_creation_state      
               print*, footer     
end subroutine unit_test_AM_broadband_signal_destroy



subroutine unit_test_AM_broadband_signal_clear()
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
               integer(c_int),              parameter :: SIGTRAP = 5 
               character(len=64),           parameter  :: header = "[TEST #3: clear_AM_broadband_signal -- START]"
               character(len=64),           parameter  :: footer = "[TEST #3: clear_AM_broadband_signal -- END]"
               integer(kind=c_long_long),   automatic  :: start,end 
               integer(kind=c_long_long),   automatic  :: start_c,end_c 
               integer(kind=c_long_long),   automatic  :: tsc_elapsed 
               type(AM_broadband_signal_t), automatic  :: baseband_sig
               complex(kind=sp),            automatic  :: A0
#if 0
               baseband_sig.m_signal_name    = "Amplitude_Modulated_wideband_signal"
               baseband_sig.m_envelope_type  = "Sine-squared"
               baseband_sig.m_distro_omega   = "Gaussian"
               baseband_sig.m_distro_theta   = "Uniform"
               baseband_sig.m_code_type      = "1,-1"
               baseband_sig.m_id             = 1 
               baseband_sig.m_interval_1     = 0
               baseband_sig.m_interval_2     = 0
               baseband_sig.m_interval_3     = 0
               baseband_sig.m_baude_rate     = 16 
               baseband_sig.m_Ns             = 0 
               baseband_sig.m_Ne             = 15 
               baseband_sig.m_Ts             = 0
               baseband_sig.m_Te             = 128 
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
               baseband_sig.m_A0             = cmplx(1.0_sp,0.0_sp)
               baseband_sig.m_fc             = 3.0e+9_sp 
#endif 
               print*, header 
               A0 = cmplx(1.0_sp,0.0_sp)
               start       = rdtsc_wrap()
               call create_AM_broadband_signal(baseband_sig,"AM_wideband_signal", &
                                               "Sine-squared","Gaussian","Uniform","1,-1",1,0,0,0, &
                                               16,0,15,0,128,0,32,0,64,0,1023,.false.,.false.,.true.,.true.,    &
                                               A0,3.0e+9_sp)
               end         = rdtsc_wrap()
               start_c     = start-RDTSC_LATENCY
               end_c       = end-RDTSC_LATENCY
               tsc_elapsed = end_c-start_c 
               if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
               else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*, "[UNIT-TEST]: create_AM_broadband_signal -- TIMING"
                 print*,"[TSC]=", tsc_elapsed 
               end if 
               start      = rdtsc_wrap()
               print*, "m_carrier -- status:", allocated(baseband_sig.m_carrier)
               print*, "m_signal -- status: ", allocated(baseband_sig.m_signal)
               call clear_AM_broadband_signal(baseband_sig,cmplx(0.0_sp,0.0_sp),0.0_sp,.false.)
               end         = rdtsc_wrap()
               start_c     = start-RDTSC_LATENCY
               end_c       = end-RDTSC_LATENCY
               tsc_elapsed = end_c-start_c 
               if(tsc_elapsed<ZERO) then
                 print*,"[INVALID-TSC]=", tsc_elapsed
               else 
                 print*, "[WARNING]: Crude timing measurement!!"
                 print*, "[UNIT-TEST]: clear_AM_broadband_signal -- TIMING"
                 print*,"[TSC]=", tsc_elapsed 
               end if 
                print*,"[TEST #3: Dumping object state.]"
               print*, "m_signal_name=",    baseband_sig.m_signal_name
               print*, "m_envelope_type=",  baseband_sig.m_envelope_type 
               print*, "m_distro_omega=",   baseband_sig.m_distro_omega
               print*, "m_distro_theta=",   baseband_sig.m_distro_theta
               print*, "m_code_type=",      baseband_sig.m_code_type 
               print*, "m_id=",             baseband_sig.m_id 
               print*, "m_interval_1=",     baseband_sig.m_interval_1  
               print*, "m_interval_2=",     baseband_sig.m_interval_2 
               print*, "m_interval_3=",     baseband_sig.m_interval_3
               print*, "m_baude_rate=",     baseband_sig.m_baude_rate
               print*, "m_Ns=",             baseband_sig.m_Ns 
               print*, "m_Ne=",             baseband_sig.m_Ne 
               print*, "m_Ts=",             baseband_sig.m_Ts 
               print*, "m_Te=",             baseband_sig.m_Te 
               print*, "m_nfreqs=",         baseband_sig.m_nfreqs
               print*, "m_nfreqe=",         baseband_sig.m_nfreqe 
               print*, "m_num_samples=",    baseband_sig.m_num_samples 
               print*, "m_nomegs=",         baseband_sig.m_nomegs 
               print*, "m_nomege=",         baseband_sig.m_nomege 
               print*, "m_nthets=",         baseband_sig.m_nthets 
               print*, "m_nthete",          baseband_sig.m_nthete         
               print*, "m_sym_dep=",        baseband_sig.m_sym_dep        
               print*, "m_ft_process=",     baseband_sig.m_ft_process
               print*, "m_A0=",             baseband_sig.m_A0                                
               print*, "m_invT=",           baseband_sig.m_invT           
               print*, "m_fc=",             baseband_sig.m_fc             
               print*, "m_fs=",             baseband_sig.m_fs           
               print*, "m_sig_width=",      baseband_sig.m_sig_width     
               print*, "m_sig_energy=",     baseband_sig.m_sig_energy    
               print*, "m_snr=",            baseband_sig.m_snr           ! signal-to-noise ratio
               print*, "m_Ps=",             baseband_sig.m_Ps ! SEP (symbol error probability)
               print*, "m_creation_state=", baseband_sig.m_creation_state      
               print*, footer     
end subroutine unit_test_AM_broadband_signal_clear




end module mod_test_AM_broadband_signal_creation 


program main 
    use mod_test_AM_broadband_signal_creation
    !call unit_test_AM_broadband_signal_create()
    !call unit_test_AM_broadband_signal_destroy()
    call unit_test_AM_broadband_signal_clear()
end program main 