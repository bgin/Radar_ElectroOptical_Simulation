
#include "GMS_config.fpp"

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

module mod_AM_broadband_signal

!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name: mod_AM_broadband_signal
 !                         
 !          
 !          Purpose:
 !                       Amplitude-modulated broadband signal  
 !                        
 !          History:
 !                        Date: 05-08-2025
 !                        Time: 10:34AM GMT+2
 !                        
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  
 !                      Bernard Gingold
 !                 
 !          References:
 !         
 !                     Макаров С.Б., Цикин И.А, "Передача дискретных сообщений по радиоканалам с ограниченной полосой пропускания"
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
!==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.

    use mod_kinds,     only : i1,i4,i8,sp
    use omp_lib
    implicit none
    public
   
    
    
      ! Major version
     integer(kind=i4),  parameter :: AM_BROADBAND_SIGNAL_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: AM_BROADBAND_SIGNAL_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: AM_BROADBAND_SIGNAL_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: AM_BROADBAND_SIGNAL_FULLVER =   &
            1000*AM_BROADBAND_SIGNAL_MAJOR+100*AM_BROADBAND_SIGNAL_MINOR+10*AM_BROADBAND_SIGNAL_MICRO
     ! Module creation date
     character(*),        parameter :: AM_BROADBAND_SIGNAL_CREATE_DATE = "05-08-2025 10:38AM +00200 (TUE 05 AUG 2025 GMT+2)"
     ! Module build date
     character(*),        parameter :: AM_BROADBAND_SIGNAL_BUILD_DATE  = __DATE__ 

     character(*),        parameter :: AM_BROADBAND_SIGNAL_BUILD_TIME  =  __TIME__
     ! Module author info
     character(*),        parameter :: AM_BROADBAND_SIGNAL_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: AM_BROADBAND_SIGNAL_SYNOPSIS    = "Amplitude-modulated broadband signal"

     character(*),        parameter :: AM_BROADBAND_SIGNAL_REFERENCE   = "Макаров С.Б., Цикин И.А, Передача дискретных сообщений по радиоканалам с ограниченной полосой пропускания"

!===========================================================================================================================================================================!

!#define AM_BROADBAND_SIGNAL_SAFE_ALLOC(mem,size) if(allocated(mem)) deallocate(mem); allocate(mem size)
!#define AM_BROADBAND_SIGNAL_SAFE_DEALLOC(mem)    if(allocated(mem)) deallocate(mem)

#if !defined(AM_BROADBAND_SIGNAL_USE_EXPLICIT_DEALLOCATION)
#define AM_BROADBAND_SIGNAL_USE_EXPLICIT_DEALLOCATION 1 
#endif 

! omega-theta,theta-omega order 
#if !defined(AM_BROADBAND_SIGNAL_OMEGA_THETA_ORDER)
#define AM_BROADBAND_SIGNAL_OMEGA_THETA_ORDER 1
#endif 

#if !defined(AM_BROADBAND_SIGNAL_USE_MKL_FFT)
#define AM_BROADBAND_SIGNAL_USE_MKL_FFT 0
#endif 

#if !defined(AM_BROADBAND_SIGNAL_EXEC_TRACE)
#define AM_BROADBAND_SIGNAL_EXEC_TRACE 1
#endif 

     ! Helper derived type for holding random-distribution parameters
     type, public :: rand_distro_params_t 
           SEQUENCE
           real(kind=sp)    :: random_gamma_s_r
           real(kind=sp)    :: random_gamma_s_i
           logical(kind=i4) :: random_gamma_first 
           real(kind=sp)    :: random_gamma1_s_r
           real(kind=sp)    :: random_gamma1_s_i
           logical(kind=i4) :: random_gamma1_first 
           real(kind=sp)    :: random_gamma2_s_r
           real(kind=sp)    :: random_gamma2_s_i
           logical(kind=i4) :: random_gamma2_first
           integer(kind=i4) :: rand_chisq_ndf 
           logical(kind=i4) :: rand_chisq_first 
           real(kind=sp)    :: random_weibull_a_r 
           real(kind=sp)    :: random_weibull_a_i 
           real(kind=sp)    :: random_beta_aa_r 
           real(kind=sp)    :: random_beta_aa_i
           real(kind=sp)    :: random_beta_bb_r 
           real(kind=sp)    :: random_beta_bb_i 
           logical(kind=i4) :: random_beta_first 
           real(kind=sp)    :: rand_poisson_mu_r 
           real(kind=sp)    :: rand_poisson_mu_i
           logical(kind=i4) :: rand_poisson_first
           real(kind=sp)    :: rand_von_Misses_k_r 
           real(kind=sp)    :: rand_von_Misses_k_i 
           logical(kind=i4) :: rand_von_Misses_first 

     end type rand_distro_params_t 

     type, public :: AM_broadband_signal_t 

           character(len=32) :: m_signal_name    ! signal/waveform name plain name
           character(len=32) :: m_envelope_type !trapzeoidal,sine, sine_squared
           character(len=32) :: m_distro_omega   ! statistical distribution type for ambiguity simulation (omega)
           character(len=32) :: m_distro_theta   ! statistical distribution type for ambiguity simulation (theta)
           character(len=4)  :: m_code_type     ! either [0,1], or [-1,1]
           integer(kind=i4)  :: m_id            ! This signal ID for pulse train scenario 
           integer(kind=i4)  :: m_interval_1    ! for trapezoidal envelope only: interval 0<=t<=s
           integer(kind=i4)  :: m_interval_2    ! for trapezoidal envelope only: interval s<t<T-s
           integer(kind=i4)  :: m_interval_3    ! for trapezoidal envelope only: interval T-s<=t<=T
           integer(kind=i4)  :: m_baude_rate    ! number of baude bit changes i.e. -1,1,or 0,1 usually the same as m_Ne value
           integer(kind=i4)  :: m_Ns            ! number of narrowband signals (start value)
           integer(kind=i4)  :: m_Ne            ! number of narrowband signal (end value)
           integer(kind=i4)  :: m_Ts             ! symbol length (period) (start value)
           integer(kind=i4)  :: m_Te             ! symbol length (period) (end value)
           integer(kind=i4)  :: m_nfreqs        ! number of frequencies (Spectral analysis) -- start value
           integer(kind=i4)  :: m_nfreqe        ! number of frequencies (Spectral analysis) -- end value 
                                                                                                     !N-1
           integer(kind=i4)  :: m_num_samples   ! number of accumulated samples per symbol i.e. y(t)=Sum a(t-kT)*dr
                                                                                                     !k=0
           
           integer(kind=i4)  :: m_nomegs         ! number of doppler frequency shifts (start value)
           integer(kind=i4)  :: m_nomege         ! number of doppler frequency shifts (end value)
           integer(kind=i4)  :: m_nthets         ! number of time delays (for return signal) (start value)
           integer(kind=i4)  :: m_nthete         ! number of time delays (for return signal) (end value)
           logical(kind=i4)  :: m_sym_dep       ! previous-next symbol dependency (page: 50, formula: 2.7)  
           logical(kind=i4)  :: m_split_carrier ! split into real/imag parts
           logical(kind=i4)  :: m_split_envelope ! split into real/imag parts
           logical(kind=i4)  :: m_ft_process     ! Fourier-Transform processing: true for MKL, false for Quadpack numerical integration
           integer(kind=i4)  :: m_order          ! 1 for: (omega,theta), 2 for: (theta,omega)
           complex(kind=sp)  :: m_A0            ! complex amplitude value   
           complex(kind=sp)  :: m_Ac            ! complex carrier amplitude                       
           real(kind=sp)     :: m_invT          ! inverse of symbol length
           real(kind=sp)     :: m_fc            ! carrier frequency 
           real(kind=sp)     :: m_fs            ! sampling frequency 
           real(kind=sp)     :: m_sig_width     ! signal width 
           real(kind=sp)     :: m_sig_energy    ! signal energy 
           complex(kind=sp)  :: m_snr           ! signal-to-noise ratio
           real(kind=sp)     :: m_Ps            ! SEP (symbol error probability)
           logical(kind=i4)  :: m_creation_state   ! is signal derived type creation accomplished
           real(kind=sp),    dimension(:),   allocatable :: m_code_seq ![0,1 or -1,1] 
           complex(kind=sp), dimension(:),   allocatable :: m_carrier 
           complex(kind=sp), dimension(:),   allocatable :: m_complex_env
           complex(kind=sp), dimension(:),   allocatable :: m_signal      ! whole signal
           complex(kind=sp), dimension(:),   allocatable :: m_env_spec    ! complex envelope spectrum  
           complex(kind=sp), dimension(:,:), allocatable :: m_samples     ! signal samples  
           complex(kind=sp), dimension(:,:), allocatable :: m_env_correl  ! complex envelope correlation function for: (omega,theta)
           complex(kind=sp), dimension(:,:), allocatable :: m_ambiguity   ! ambiguity function for: (omega,theta)
           real(kind=sp),    dimension(:,:), allocatable :: m_abs_ambiguity ! absolute value of ambiguity function
           
#if defined(__INTEL_COMPILER) || defined(__ICC)
           !dir$ attributes align : 64 :: m_code_seq
           !dir$ attributes align : 64 :: m_carrier 
           !dir$ attributes align : 64 :: m_complex_env 
           !dir$ attributes align : 64 :: m_signal 
           !dir$ attributes align : 64 :: m_env_spec 
           !dir$ attributes align : 64 :: m_env_correl 
           !dir4 attributes align : 64 :: m_ambiguity 
           !dir$ attributes align : 64 :: m_abs_ambiguity
#endif 

           real(kind=sp),    dimension(:),   allocatable :: m_carrier_i
           real(kind=sp),    dimension(:),   allocatable :: m_carrier_q 
           real(kind=sp),    dimension(:),   allocatable :: m_complex_env_i
           real(kind=sp),    dimension(:),   allocatable :: m_complex_env_q
           real(kind=sp),    dimension(:),   allocatable :: m_signal_i
           real(kind=sp),    dimension(:),   allocatable :: m_signal_q
           real(kind=sp),    dimension(:,:), allocatable :: m_env_correl_i 
           real(kind=sp),    dimension(:,:), allocatable :: m_env_correl_q
#if defined(__INTEL_COMPILER) || defined(__ICC)
           !dir$ attributes align : 64 :: m_carrier_i 
           !dir$ attributes align : 64 :: m_carrier_q
           !dir$ attributes align : 64 :: m_complex_env_i
           !dir$ attributes align : 64 :: m_complex_env_q
           !dir$ attributes align : 64 :: m_signal_i
           !dir$ attributes align : 64 :: m_signal_q
           !dir$ attributes align : 64 :: m_env_correl_i 
           !dir$ attributes align : 64 :: m_env_correl_q 
#endif 

           
     end type AM_broadband_signal_t


     contains 


     subroutine create_AM_broadband_signal(AM_signal,sig_name,envelope_type,distro_omega,distro_theta,          &
                                           code_type,id,interval_1,interval_2,interval_3, baude_rate,           &
                                           Ns,Ne,Ts,Te,nfreqs,nfreqe,nomegs,nomege,nthets,nthete,               &
                                           sym_dep,split_carrier,split_envelope,ft_process,order,A0,Ac,fc)  
          implicit none   
#if (AM_BROADBAND_SIGNAL_EXEC_TRACE) == 1
          interface 
                   function rdtsc_wrap() bind(C,name="rdtsc_wrap")
                            use iso_c_binding, only : c_long_long 
                            integer(c_long_long) :: rdtsc_wrap 
                    end function rdtsc_wrap 
               end interface
#endif           
                                        
          
#if (AM_BROADBAND_SIGNAL_EXEC_TRACE) == 1
          character(*), parameter :: sub_name = "create_AM_broadband_signal"
#endif 
          type(AM_broadband_signal_t),        intent(out)          :: AM_signal 
          character(len=*),                   intent(in)           :: sig_name 
          character(len=*),                   intent(in)           :: envelope_type 
          character(len=*),                   intent(in)           :: distro_omega 
          character(len=*),                   intent(in)           :: distro_theta 
          character(len=*),                   intent(in)           :: code_type 
          integer(kind=i4),                   intent(in)           :: id 
          integer(kind=i4),                   intent(in), optional :: interval_1
          integer(kind=i4),                   intent(in), optional :: interval_2 
          integer(kind=i4),                   intent(in), optional :: interval_3 
          integer(kind=i4),                   intent(in)           :: baude_rate  
          integer(kind=i4),                   intent(in)           :: Ns 
          integer(kind=i4),                   intent(in)           :: Ne 
          integer(kind=i4),                   intent(in)           :: Ts 
          integer(kind=i4),                   intent(in)           :: Te 
          integer(kind=i4),                   intent(in)           :: nfreqs 
          integer(kind=i4),                   intent(in)           :: nfreqe 
          integer(kind=i4),                   intent(in)           :: nomegs
          integer(kind=i4),                   intent(in)           :: nomege   
          integer(kind=i4),                   intent(in)           :: nthets
          integer(kind=i4),                   intent(in)           :: nthete   
          logical(kind=i4),                   intent(in)           :: sym_dep 
          logical(kind=i4),                   intent(in)           :: split_carrier 
          logical(kind=i4),                   intent(in)           :: split_envelope 
          logical(kind=i4),                   intent(in)           :: ft_process 
          integer(kind=i4),                   intent(in)           :: order 
          complex(kind=sp),                   intent(in)           :: A0 
          complex(kind=sp),                   intent(in)           :: Ac 
          real(kind=sp),                      intent(in)           :: fc 
          logical(kind=i1), automatic :: trpz_env   
#if (AM_BROADBAND_SIGNAL_EXEC_TRACE) == 1
          print*, "[EXECUTION-TRACE: START]"
          print*, "File:     ", __FILE__
          print*, "Procedure:", sub_name 
          print*, "LOC:      ", __LINE__ 
          print*, "TSC-Start:", rdtsc_wrap()
#endif 
         

          if(AM_signal.m_creation_state .eq. .true.) return 
          AM_signal.m_signal_name   = sig_name 
          AM_signal.m_envelope_type = envelope_type  
          AM_signal.m_distro_omega  = distro_omega 
          AM_signal.m_distro_theta  = distro_theta 
          AM_signal.m_code_type     = code_type 
          AM_signal.m_id            = id 
          trpz_env                  = AM_signal.m_envelope_type .eq. "trapezoidal"
          if(trpz_env  .and. present(interval_1)) AM_signal.m_interval_1 = interval_1 
          if(trpz_env  .and. present(interval_2)) AM_signal.m_interval_2 = interval_2 
          if(trpz_env  .and. present(interval_3)) AM_signal.m_interval_3 = interval_3 
          AM_signal.m_baude_rate    = baude_rate  
          AM_signal.m_Ns            = Ns 
          AM_signal.m_Ne            = Ne  
          AM_signal.m_Ts            = Ts 
          AM_signal.m_Te            = Te 
          AM_signal.m_nfreqs        = nfreqs 
          AM_signal.m_nfreqe        = nfreqe 
          AM_signal.m_num_samples   = (AM_signal.m_Te-AM_signal.m_Ts)* &
                                      (AM_signal.m_Ne-AM_signal.m_Ns) 
          AM_signal.m_nomegs        = nomegs  
          AM_signal.m_nomege        = nomege 
          AM_signal.m_nthets        = nthets 
          AM_signal.m_nthete        = nthete  
          AM_signal.m_sym_dep       = sym_dep 
          AM_signal.m_split_carrier = split_carrier 
          AM_signal.m_split_envelope=split_envelope 
          AM_signal.m_ft_process    = ft_process 
          AM_signal.m_order         = order 
          AM_signal.m_A0            = A0 ! Complex amplitude value
          Am_signal.m_Ac            = Ac 
          AM_signal.m_fc            = fc 
          AM_signal.m_fs            = 1.0_sp/real(AM_signal.m_num_samples,kind=sp) 
          AM_signal.m_sig_width     = 0.0_sp 
          AM_signal.m_sig_energy    = 0.0_sp 
          AM_signal.m_snr           = cmplx(0.0_sp,0.0_sp)
          AM_signal.m_Ps            = 0.0_sp 
                      
          allocate(AM_signal.m_code_seq(AM_signal.m_baude_rate))
          allocate(AM_signal.m_carrier(AM_signal.m_Ts:AM_signal.m_Te))
          allocate(AM_signal.m_complex_env(AM_signal.m_Ts:AM_signal.m_Te))
          allocate(AM_signal.m_signal(AM_signal.m_Ts:AM_signal.m_Te))
          allocate(AM_signal.m_env_spec(AM_signal.m_nfreqs:AM_signal.m_nfreqe))
          allocate(AM_signal.m_samples(AM_signal.m_Ns:AM_signal.m_Ne,AM_signal.m_Ts:AM_signal.m_Te))
          select case (AM_signal.m_order)
              case (1)
                  allocate(AM_signal.m_env_correl(AM_signal.m_nomegs:AM_signal.m_nomege,AM_signal.m_nthets:AM_signal.m_nthete))
                  allocate(AM_signal.m_ambiguity(AM_signal.m_nomegs:AM_signal.m_nomege,AM_signal.m_nthets:AM_signal.m_nthete))
                  allocate(AM_signal.m_abs_ambiguity(AM_signal.m_nomegs:AM_signal.m_nomege,AM_signal.m_nthets:AM_signal.m_nthete))
              case (2)
                  allocate(AM_signal.m_env_correl(AM_signal.m_nthets:AM_signal.m_nthete,AM_signal.m_nomegs:AM_signal.m_nomege))
                  allocate(AM_signal.m_ambiguity(AM_signal.m_nthets:AM_signal.m_nthete,AM_signal.m_nomegs:AM_signal.m_nomege))
                  allocate(AM_signal.m_abs_ambiguity(AM_signal.m_nthets:AM_signal.m_nthete,AM_signal.m_nomegs:AM_signal.m_nomege))
              case default 
                  STOP "[create_AM_broadband_signal] -- INVALID SWITCH ARGUMENT!!"
          end select 

          allocate(AM_signal.m_carrier_i(AM_signal.m_Ts:AM_signal.m_Te))
          allocate(AM_signal.m_carrier_q(AM_signal.m_Ts:AM_signal.m_Te))
          allocate(AM_signal.m_complex_env_i(AM_signal.m_Ts:AM_signal.m_Te))
          allocate(AM_signal.m_complex_env_q(AM_signal.m_Ts:AM_signal.m_Te))
          allocate(AM_signal.m_signal_i(AM_signal.m_Ts:AM_signal.m_Te))
          allocate(AM_signal.m_signal_q(AM_signal.m_Ts:AM_signal.m_Te))
          select case (AM_signal.m_order)
             case (1)
                allocate(AM_signal.m_env_correl_i(AM_signal.m_nomegs:AM_signal.m_nomege,AM_signal.m_nthets:AM_signal.m_nthete))
                allocate(AM_signal.m_env_correl_q(AM_signal.m_nomegs:AM_signal.m_nomege,AM_signal.m_nthets:AM_signal.m_nthete))
             case (2)
                allocate(AM_signal.m_env_correl_i(AM_signal.m_nthets:AM_signal.m_nthete,AM_signal.m_nomegs:AM_signal.m_nomege))
                allocate(AM_signal.m_env_correl_q(AM_signal.m_nthets:AM_signal.m_nthete,AM_signal.m_nomegs:AM_signal.m_nomege))
             case default
                STOP "[create_AM_broadband_signal] -- INVALID SWITCH ARGUMENT!!"
          end select 
          AM_signal.m_creation_state = .true. 
#if (AM_BROADBAND_SIGNAL_EXEC_TRACE) == 1
          print*, "[EXECUTION-TRACE: END]"
          print*, "TSC-End:", rdtsc_wrap()
#endif
     end subroutine create_AM_broadband_signal

#if (AM_BROADBAND_SIGNAL_USE_EXPLICIT_DEALLOCATION) == 1

     subroutine destroy_AM_broadband_signal(AM_signal,clear_values)
          implicit none 
#if (AM_BROADBAND_SIGNAL_EXEC_TRACE) == 1
          interface 
                   function rdtsc_wrap() bind(C,name="rdtsc_wrap")
                            use iso_c_binding, only : c_long_long 
                            integer(c_long_long) :: rdtsc_wrap 
                    end function rdtsc_wrap 
               end interface
#endif 
          
#if (AM_BROADBAND_SIGNAL_EXEC_TRACE) == 1
          character(*), parameter :: sub_name = "destroy_AM_broadband_signal"
#endif 
          type(AM_broadband_signal_t),        intent(inout)        :: AM_signal
          logical(kind=i4),                   intent(in)           :: clear_values 

#if (AM_BROADBAND_SIGNAL_EXEC_TRACE) == 1
          print*, "[EXECUTION-TRACE: START]"
          print*, "File:     ", __FILE__
          print*, "Procedure:", sub_name 
          print*, "LOC:      ", __LINE__ 
          print*, "TSC-Start:", rdtsc_wrap()
#endif          
          if(AM_signal.m_creation_state .eq. .false.) return 
          if(allocated(AM_signal.m_code_seq))     deallocate(AM_signal.m_code_seq) 
          if(allocated(AM_signal.m_carrier))      deallocate(AM_signal.m_carrier)    
          if(allocated(AM_signal.m_complex_env))  deallocate(AM_signal.m_complex_env)
          if(allocated(AM_signal.m_signal))       deallocate(AM_signal.m_signal)     
          if(allocated(AM_signal.m_env_spec))     deallocate(AM_signal.m_env_spec)   
          if(allocated(AM_signal.m_samples))      deallocate(AM_signal.m_samples)    
          if(allocated(AM_signal.m_env_correl))   deallocate(AM_signal.m_env_correl) 
          if(allocated(AM_signal.m_ambiguity))    deallocate(AM_signal.m_ambiguity)  
          if(allocated(AM_signal.m_abs_ambiguity))deallocate(AM_signal.m_abs_ambiguity)    
          if(allocated(AM_signal.m_carrier_i))    deallocate(AM_signal.m_carrier_i)    
          if(allocated(AM_signal.m_carrier_q))    deallocate(AM_signal.m_carrier_q)    
          if(allocated(AM_signal.m_complex_env_i))deallocate(AM_signal.m_complex_env_i)
          if(allocated(AM_signal.m_complex_env_q))deallocate(AM_signal.m_complex_env_q)
          if(allocated(AM_signal.m_signal_i))     deallocate(AM_signal.m_signal_i)     
          if(allocated(AM_signal.m_signal_q))     deallocate(AM_signal.m_signal_q)     
          if(allocated(AM_signal.m_env_correl_i)) deallocate(AM_signal.m_env_correl_i) 
          if(allocated(AM_signal.m_env_correl_q)) deallocate(AM_signal.m_env_correl_q) 
         
          if(clear_values .eq. .true.) then 
             AM_signal.m_signal_name   = ""
             AM_signal.m_envelope_type = ""
             AM_signal.m_distro_omega  = ""
             AM_signal.m_distro_theta  = ""
             AM_signal.m_code_type     = ""
             AM_signal.m_id            = 0 
             AM_signal.m_interval_1    = 0
             AM_signal.m_interval_2    = 0
             AM_signal.m_interval_3    = 0
             AM_signal.m_baude_rate    = 0
             AM_signal.m_Ts            = 0
             AM_signal.m_Te            = 0
             AM_signal.m_Ns            = 0
             AM_signal.m_Ne            = 0
             AM_signal.m_nfreqs        = 0
             AM_signal.m_nfreqe        = 0
             AM_signal.m_num_samples   = 0
             AM_signal.m_nomegs        = 0
             AM_signal.m_nomege        = 0
             AM_signal.m_nthets        = 0
             AM_signal.m_nthete        = 0
             AM_signal.m_sym_dep       = .false. 
             AM_signal.m_ft_process    = .false. 
             AM_signal.m_order         = 0
             AM_signal.m_A0            = cmplx(0.0_sp,0.0_sp)
             AM_signal.m_Ac            = cmplx(0.0_sp,0.0_sp)
             AM_signal.m_invT          = 0.0_sp 
             AM_signal.m_fc            = 0.0_sp 
             AM_signal.m_fs            = 0.0_sp 
             AM_signal.m_sig_width     = -1.0_sp 
             AM_signal.m_sig_energy    = -1.0_sp 
             AM_signal.m_snr           = 0.0_sp 
             AM_signal.m_Ps            = -1.0_sp 
          end if 
          AM_signal.m_creation_state   = .false.
#if (AM_BROADBAND_SIGNAL_EXEC_TRACE) == 1
          print*, "[EXECUTION-TRACE: END]"
          print*, "TSC-End:", rdtsc_wrap()
#endif          
     end subroutine destroy_AM_broadband_signal
#endif 


     subroutine clear_AM_broadband_signal(AM_signal,carray_fill,rarray_fill,use_memset)
          implicit none 
#if (AM_BROADBAND_SIGNAL_EXEC_TRACE) == 1
          interface 
                   function rdtsc_wrap() bind(C,name="rdtsc_wrap")
                            use iso_c_binding, only : c_long_long 
                            integer(c_long_long) :: rdtsc_wrap 
                    end function rdtsc_wrap 
               end interface
#endif 
          
#if (AM_BROADBAND_SIGNAL_EXEC_TRACE) == 1
          character(*), parameter :: sub_name = "clear_AM_broadband_signal"
#endif 
          type(AM_broadband_signal_t),        intent(inout)        :: AM_signal
          complex(kind=sp),                   intent(in)           :: carray_fill 
          real(kind=sp),                      intent(in)           :: rarray_fill 
          logical(kind=i4),                   intent(in)           :: use_memset
          integer(kind=i4), automatic :: i__,j__,k__ 

#if (AM_BROADBAND_SIGNAL_EXEC_TRACE) == 1
          print*, "[EXECUTION-TRACE: START]"
          print*, "File:     ", __FILE__
          print*, "Procedure:", sub_name 
          print*, "LOC:      ", __LINE__ 
          print*, "TSC-Start:", rdtsc_wrap()
#endif 
          if(AM_signal.m_creation_state .eq. .false.) return 
          if(use_memset .eq. .true.) then 
             AM_signal.m_code_seq      = rarray_fill
             AM_signal.m_carrier       = carray_fill
             AM_signal.m_complex_env   = carray_fill
             AM_signal.m_signal        = carray_fill
             AM_signal.m_env_spec      = carray_fill
             AM_signal.m_samples       = carray_fill
             AM_signal.m_env_correl    = carray_fill    
             AM_signal.m_ambiguity     = carray_fill     
             AM_signal.m_abs_ambiguity = rarray_fill
             AM_signal.m_carrier_i     = rarray_fill     
             AM_signal.m_carrier_q     = rarray_fill     
             AM_signal.m_complex_env_i = rarray_fill 
             AM_signal.m_complex_env_q = rarray_fill 
             AM_signal.m_signal_i      = rarray_fill      
             AM_signal.m_signal_q      = rarray_fill     
             AM_signal.m_env_correl_i  = rarray_fill  
             AM_signal.m_env_correl_q  = rarray_fill  

          else 
                do i__=1,AM_signal.m_baude_rate 
                   AM_signal.m_code_seq(i__) = rarray_fill
                end do 

                do i__= AM_signal.m_Ts,AM_signal.m_Te 
                   AM_signal.m_carrier(i__)       = carray_fill
                   AM_signal.m_complex_env(i__)   = carray_fill
                   AM_signal.m_signal(i__)        = carray_fill
                   AM_signal.m_carrier_i(i__)     = rarray_fill
                   AM_signal.m_carrier_q(i__)     = rarray_fill
                   AM_signal.m_complex_env_i(i__) = rarray_fill
                   AM_signal.m_complex_env_q(i__) = rarray_fill
                   AM_signal.m_signal_i(i__)      = rarray_fill
                   AM_signal.m_signal_q(i__)      = rarray_fill

                   do j__= AM_signal.m_Ns,iand(AM_signal.m_Ne-1,inot(3)), 4
                      AM_signal.m_samples(j__+0,i__) = carray_fill
                      AM_signal.m_samples(j__+1,i__) = carray_fill
                      AM_signal.m_samples(j__+2,i__) = carray_fill
                      AM_signal.m_samples(j__+3,i__) = carray_fill
                   end do 
                   do k__=j__,AM_signal.m_Ne 
                      AM_signal.m_samples(k__,i__)   = carray_fill
                   end do 
                end do 
                
                do i__=AM_signal.m_nfreqs,AM_signal.m_nfreqe 
                    AM_signal.m_env_spec(i__) = carray_fill
                end do 
          select case (AM_signal.m_order)  
               case (1) 
                    do i__= AM_signal.m_nomegs,AM_signal.m_nomege
                           do j__ = AM_signal.m_nthets,iand(AM_signal.m_nthete-1,inot(3)), 4
                                  AM_signal.m_env_correl(j__+0,i__)    = carray_fill
                                  AM_signal.m_ambiguity(j__+0,i__)     = carray_fill
                                  AM_signal.m_abs_ambiguity(j__+0,i__) = rarray_fill

                                  AM_signal.m_env_correl_i(j__+0,i__)  = rarray_fill 
                                  AM_signal.m_env_correl_q(j__+0,i__)  = rarray_fill
 
                                  AM_signal.m_env_correl(j__+1,i__)    = carray_fill
                                  AM_signal.m_ambiguity(j__+1,i__)     = carray_fill
                                  AM_signal.m_abs_ambiguity(j__+1,i__) = rarray_fill

                                  AM_signal.m_env_correl_i(j__+1,i__)  = rarray_fill 
                                  AM_signal.m_env_correl_q(j__+1,i__)  = rarray_fill
 
                                  AM_signal.m_env_correl(j__+2,i__)    = carray_fill
                                  AM_signal.m_ambiguity(j__+2,i__)     = carray_fill
                                  AM_signal.m_abs_ambiguity(j__+2,i__) = rarray_fill

                                  AM_signal.m_env_correl_i(j__+2,i__)  = rarray_fill 
                                  AM_signal.m_env_correl_q(j__+2,i__)  = rarray_fill

                                  AM_signal.m_env_correl(j__+3,i__)    = carray_fill
                                  AM_signal.m_ambiguity(j__+3,i__)     = carray_fill
                                  AM_signal.m_abs_ambiguity(j__+3,i__) = rarray_fill

                                  AM_signal.m_env_correl_i(j__+3,i__)  = rarray_fill 
                                  AM_signal.m_env_correl_q(j__+3,i__)  = rarray_fill
 
                           end do 
                           do k__=j__,AM_signal.m_nthete 
                                  AM_signal.m_env_correl(k__,i__)    = carray_fill
                                  AM_signal.m_ambiguity(k__,i__)     = carray_fill
                                  AM_signal.m_abs_ambiguity(k__,i__) = rarray_fill

                                  AM_signal.m_env_correl_i(k__,i__)  = rarray_fill 
                                  AM_signal.m_env_correl_q(k__,i__)  = rarray_fill
 
                           end do 
                     end do 
               case (2)
                     do i__= AM_signal.m_nthets,AM_signal.m_nthete
                           do j__ = AM_signal.m_nomegs,iand(AM_signal.m_nomege-1,inot(3)), 4
                                  AM_signal.m_env_correl(j__+0,i__)    = carray_fill
                                  AM_signal.m_ambiguity(j__+0,i__)     = carray_fill
                                  AM_signal.m_abs_ambiguity(j__+0,i__) = rarray_fill

                                  AM_signal.m_env_correl_i(j__+0,i__)  = rarray_fill 
                                  AM_signal.m_env_correl_q(j__+0,i__)  = rarray_fill
 
                                  AM_signal.m_env_correl(j__+1,i__)    = carray_fill
                                  AM_signal.m_ambiguity(j__+1,i__)     = carray_fill
                                  AM_signal.m_abs_ambiguity(j__+1,i__) = rarray_fill

                                  AM_signal.m_env_correl_i(j__+1,i__)  = rarray_fill 
                                  AM_signal.m_env_correl_q(j__+1,i__)  = rarray_fill
 
                                  AM_signal.m_env_correl(j__+2,i__)    = carray_fill
                                  AM_signal.m_ambiguity(j__+2,i__)     = carray_fill
                                  AM_signal.m_abs_ambiguity(j__+2,i__) = rarray_fill

                                  AM_signal.m_env_correl_i(j__+2,i__)  = rarray_fill 
                                  AM_signal.m_env_correl_q(j__+2,i__)  = rarray_fill

                                  AM_signal.m_env_correl(j__+3,i__)    = carray_fill
                                  AM_signal.m_ambiguity(j__+3,i__)     = carray_fill
                                  AM_signal.m_abs_ambiguity(j__+3,i__) = rarray_fill

                                  AM_signal.m_env_correl_i(j__+3,i__)  = rarray_fill 
                                  AM_signal.m_env_correl_q(j__+3,i__)  = rarray_fill
 
                           end do 
                           do k__=j__,AM_signal.m_nomege 
                                  AM_signal.m_env_correl(k__,i__)    = carray_fill
                                  AM_signal.m_ambiguity(k__,i__)     = carray_fill
                                  AM_signal.m_abs_ambiguity(k__,i__) = rarray_fill

                                  AM_signal.m_env_correl_i(k__,i__)  = rarray_fill 
                                  AM_signal.m_env_correl_q(k__,i__)  = rarray_fill
 
                           end do 
                     end do 
           end select 
            
          end if 
#if (AM_BROADBAND_SIGNAL_EXEC_TRACE) == 1
          print*, "[EXECUTION-TRACE: END]"
          print*, "TSC-End:", rdtsc_wrap()
#endif          
     end subroutine clear_AM_broadband_signal


     subroutine copy_create_AM_broadband_signal(this,other)
          implicit none
#if (AM_BROADBAND_SIGNAL_EXEC_TRACE) == 1
          interface 
                   function rdtsc_wrap() bind(C,name="rdtsc_wrap")
                            use iso_c_binding, only : c_long_long 
                            integer(c_long_long) :: rdtsc_wrap 
                    end function rdtsc_wrap 
               end interface
#endif 
           
#if (AM_BROADBAND_SIGNAL_EXEC_TRACE) == 1
          character(*), parameter :: sub_name = "copy_create_AM_broadband_signal"
#endif 
          type(AM_broadband_signal_t),        intent(out)        :: this
          type(AM_broadband_signal_t),        intent(in)         :: other 
          integer(kind=i4), automatic :: i__,j__,k__ 

#if (AM_BROADBAND_SIGNAL_EXEC_TRACE) == 1
          print*, "[EXECUTION-TRACE: START]"
          print*, "File:     ", __FILE__
          print*, "Procedure:", sub_name 
          print*, "LOC:      ", __LINE__ 
          print*, "TSC-Start:", rdtsc_wrap()
#endif
             if(this.m_creation_state  .eq. .true. .or. &
                other.m_creation_state .eq. .false.) return 
             if(LOC(this) .eq. LOC(other)) return 
             this.m_signal_name   =    other.m_signal_name
             this.m_envelope_type =    other.m_envelope_type
             this.m_distro_omega  =    other.m_distro_omega
             this.m_distro_theta  =    other.m_distro_theta
             this.m_code_type     =    other.m_code_type
             this.m_id            =    other.m_id  
             this.m_interval_1    =    other.m_interval_1
             this.m_interval_2    =    other.m_interval_2  
             this.m_interval_3    =    other.m_interval_3
             this.m_baude_rate    =    other.m_baude_rate
             this.m_Ts            =    other.m_Ts 
             this.m_Te            =    other.m_Te 
             this.m_Ns            =    other.m_Ns 
             this.m_Ne            =    other.m_Ne 
             this.m_nfreqs        =    other.m_nfreqs
             this.m_nfreqe        =    other.m_nfreqe 
             this.m_num_samples   =    other.m_num_samples
             this.m_nomegs        =    other.m_nomegs 
             this.m_nomege        =    other.m_nomege 
             this.m_nthets        =    other.m_nthets 
             this.m_nthete        =    other.m_nthete 
             this.m_sym_dep       =    other.m_sym_dep  
             this.m_ft_process    =    other.m_ft_process 
             this.m_order         =    other.m_order 
             this.m_A0            =    other.m_A0
             this.m_Ac            =    other.m_Ac 
             this.m_invT          =    other.m_invT 
             this.m_fc            =    other.m_fc  
             this.m_fs            =    other.m_fs  
             this.m_sig_width     =    other.m_sig_width 
             this.m_sig_energy    =    other.m_sig_energy 
             this.m_snr           =    other.m_snr  
             this.m_Ps            =    other.m_Ps 
             
             allocate(this.m_code_seq(this.m_baude_rate))
             allocate(this.m_carrier(this.m_Ts:this.m_Te))
             allocate(this.m_complex_env(this.m_Ts:this.m_Te))
             allocate(this.m_signal(this.m_Ts:this.m_Te))
             allocate(this.m_env_spec(this.m_nfreqs:this.m_nfreqe))
             allocate(this.m_samples(this.m_Ns:this.m_Ne,this.m_Ts:this.m_Te))
             select case (this.m_order)
              case (1)
                    allocate(this.m_env_correl(this.m_nomegs:this.m_nomege,this.m_nthets:this.m_nthete))
                    allocate(this.m_ambiguity(this.m_nomegs:this.m_nomege,this.m_nthets:this.m_nthete))
                    allocate(this.m_abs_ambiguity(this.m_nomegs:this.m_nomege,this.m_nthets:this.m_nthete))
              case (2)
                    allocate(this.m_env_correl(this.m_nthets:this.m_nthete,this.m_nomegs:this.m_nomege))
                    allocate(this.m_ambiguity(this.m_nthets:this.m_nthete,this.m_nomegs:this.m_nomege))
                    allocate(this.m_abs_ambiguity(this.m_nthets:this.m_nthete,this.m_nomegs:this.m_nomege))
              case default 
                    STOP "[create_AM_broadband_signal] -- INVALID SWITCH ARGUMENT!!"
             end select 

             allocate(this.m_carrier_i(this.m_Ts:this.m_Te))
             allocate(this.m_carrier_q(this.m_Ts:this.m_Te))
             allocate(this.m_complex_env_i(this.m_Ts:this.m_Te))
             allocate(this.m_complex_env_q(this.m_Ts:this.m_Te))
             allocate(this.m_signal_i(this.m_Ts:this.m_Te))
             allocate(this.m_signal_q(this.m_Ts:this.m_Te))
             select case (this.m_order)
                case (1)
                     allocate(this.m_env_correl_i(this.m_nomegs:this.m_nomege,this.m_nthets:this.m_nthete))
                     allocate(this.m_env_correl_q(this.m_nomegs:this.m_nomege,this.m_nthets:this.m_nthete))
                case (2)
                     allocate(this.m_env_correl_i(this.m_nthets:this.m_nthete,this.m_nomegs:this.m_nomege))
                     allocate(this.m_env_correl_q(this.m_nthets:this.m_nthete,this.m_nomegs:this.m_nomege))
                case default
                     STOP "[create_AM_broadband_signal] -- INVALID SWITCH ARGUMENT!!"
             end select 

                do i__=1,this.m_baude_rate 
                   this.m_code_seq(i__) = other.m_code_seq(i__)
                end do 

                do i__= this.m_Ts,this.m_Te 
                   this.m_carrier(i__)       = other.m_carrier(i__)
                   this.m_complex_env(i__)   = other.m_complex_env(i__)
                   this.m_signal(i__)        = other.m_signal(i__)
                   this.m_carrier_i(i__)     = other.m_carrier_i(i__)
                   this.m_carrier_q(i__)     = other.m_carrier_q(i__)
                   this.m_complex_env_i(i__) = other.m_complex_env_i(i__)
                   this.m_complex_env_q(i__) = other.m_complex_env_q(i__)
                   this.m_signal_i(i__)      = other.m_signal_i(i__)
                   this.m_signal_q(i__)      = other.m_signal_q(i__)

                   do j__= this.m_Ns,iand(this.m_Ne-1,inot(3)), 4
                      this.m_samples(j__+0,i__) = other.m_samples(j__+0,i__)
                      this.m_samples(j__+1,i__) = other.m_samples(j__+1,i__)
                      this.m_samples(j__+2,i__) = other.m_samples(j__+2,i__)
                      this.m_samples(j__+3,i__) = other.m_samples(j__+3,i__)
                   end do 
                   do k__=j__,this.m_Ne 
                      this.m_samples(k__,i__)   = other.m_samples(k__,i__)
                   end do 
                end do 
                
                do i__=this.m_nfreqs,this.m_nfreqe 
                    this.m_env_spec(i__) = other.m_env_spec(i__)
                end do 
           select case (this.m_order)  
               case (1) 
                    do i__= this.m_nomegs,this.m_nomege
                           do j__ = this.m_nthets,iand(this.m_nthete-1,inot(3)), 4
                                  this.m_env_correl(j__+0,i__)    = other.m_env_correl(j__+0,i__) 
                                  this.m_ambiguity(j__+0,i__)     = other.m_ambiguity(j__+0,i__)
                                  this.m_abs_ambiguity(j__+0,i__) = other.m_abs_ambiguity(j__+0,i__)

                                  this.m_env_correl_i(j__+0,i__)  = other.m_env_correl_i(j__+0,i__)
                                  this.m_env_correl_q(j__+0,i__)  = other.m_env_correl_q(j__+0,i__)
 
                                  this.m_env_correl(j__+1,i__)    = other.m_env_correl(j__+1,i__)
                                  this.m_ambiguity(j__+1,i__)     = other.m_ambiguity(j__+1,i__)
                                  this.m_abs_ambiguity(j__+1,i__) = other.m_abs_ambiguity(j__+1,i__)

                                  this.m_env_correl_i(j__+1,i__)  = other.m_env_correl_i(j__+1,i__)
                                  this.m_env_correl_q(j__+1,i__)  = other.m_env_correl_q(j__+1,i__)
                                  
                                  this.m_env_correl(j__+2,i__)    = other.m_env_correl(j__+2,i__)
                                  this.m_ambiguity(j__+2,i__)     = other.m_ambiguity(j__+2,i__)
                                  this.m_abs_ambiguity(j__+2,i__) = other.m_abs_ambiguity(j__+2,i__)

                                  this.m_env_correl_i(j__+2,i__)  = other.m_env_correl_i(j__+2,i__)
                                  this.m_env_correl_q(j__+2,i__)  = other.m_env_correl_q(j__+2,i__)

                                  this.m_env_correl(j__+3,i__)    = other.m_env_correl(j__+3,i__)
                                  this.m_ambiguity(j__+3,i__)     = other.m_ambiguity(j__+3,i__)
                                  this.m_abs_ambiguity(j__+3,i__) = other.m_abs_ambiguity(j__+3,i__)

                                  this.m_env_correl_i(j__+3,i__)  = other.m_env_correl_i(j__+3,i__)
                                  this.m_env_correl_q(j__+3,i__)  = other.m_env_correl_q(j__+3,i__)
 
                           end do 
                           do k__=j__,this.m_nthete 
                                  this.m_env_correl(k__,i__)    = other.m_env_correl(k__,i__)
                                  this.m_ambiguity(k__,i__)     = other.m_ambiguity(k__,i__)
                                  this.m_abs_ambiguity(k__,i__) = other.m_abs_ambiguity(k__,i__)

                                  this.m_env_correl_i(k__,i__)  = other.m_env_correl_i(k__,i__)
                                  this.m_env_correl_q(k__,i__)  = other.m_env_correl_q(k__,i__)
 
                           end do 
                     end do 
               case (2)
                     do i__= this.m_nthets,this.m_nthete
                           do j__ = this.m_nomegs,iand(this.m_nomege-1,inot(3)), 4
                                  this.m_env_correl(j__+0,i__)    = other.m_env_correl(j__+0,i__) 
                                  this.m_ambiguity(j__+0,i__)     = other.m_ambiguity(j__+0,i__)
                                  this.m_abs_ambiguity(j__+0,i__) = other.m_abs_ambiguity(j__+0,i__)

                                  this.m_env_correl_i(j__+0,i__)  = other.m_env_correl_i(j__+0,i__)
                                  this.m_env_correl_q(j__+0,i__)  = other.m_env_correl_q(j__+0,i__)
 
                                  this.m_env_correl(j__+1,i__)    = other.m_env_correl(j__+1,i__)
                                  this.m_ambiguity(j__+1,i__)     = other.m_ambiguity(j__+1,i__)
                                  this.m_abs_ambiguity(j__+1,i__) = other.m_abs_ambiguity(j__+1,i__)

                                  this.m_env_correl_i(j__+1,i__)  = other.m_env_correl_i(j__+1,i__)
                                  this.m_env_correl_q(j__+1,i__)  = other.m_env_correl_q(j__+1,i__)
                                  
                                  this.m_env_correl(j__+2,i__)    = other.m_env_correl(j__+2,i__)
                                  this.m_ambiguity(j__+2,i__)     = other.m_ambiguity(j__+2,i__)
                                  this.m_abs_ambiguity(j__+2,i__) = other.m_abs_ambiguity(j__+2,i__)

                                  this.m_env_correl_i(j__+2,i__)  = other.m_env_correl_i(j__+2,i__)
                                  this.m_env_correl_q(j__+2,i__)  = other.m_env_correl_q(j__+2,i__)

                                  this.m_env_correl(j__+3,i__)    = other.m_env_correl(j__+3,i__)
                                  this.m_ambiguity(j__+3,i__)     = other.m_ambiguity(j__+3,i__)
                                  this.m_abs_ambiguity(j__+3,i__) = other.m_abs_ambiguity(j__+3,i__)

                                  this.m_env_correl_i(j__+3,i__)  = other.m_env_correl_i(j__+3,i__)
                                  this.m_env_correl_q(j__+3,i__)  = other.m_env_correl_q(j__+3,i__)
 
                           end do 
                           do k__=j__,this.m_nomege 
                                  this.m_env_correl(k__,i__)    = other.m_env_correl(k__,i__)
                                  this.m_ambiguity(k__,i__)     = other.m_ambiguity(k__,i__)
                                  this.m_abs_ambiguity(k__,i__) = other.m_abs_ambiguity(k__,i__)

                                  this.m_env_correl_i(k__,i__)  = other.m_env_correl_i(k__,i__)
                                  this.m_env_correl_q(k__,i__)  = other.m_env_correl_q(k__,i__)
 
                           end do 
                     end do 
           end select 
           this.m_creation_state = other.m_creation_state
#if (AM_BROADBAND_SIGNAL_EXEC_TRACE) == 1
          print*, "[EXECUTION-TRACE: END]"
          print*, "TSC-End:", rdtsc_wrap()
#endif
     end subroutine copy_create_AM_broadband_signal

     
     ! The destination i.e. this-object is in existing (created) state!!.
     subroutine copy_assign_AM_broadband_signal(this,other)
          implicit none
#if (AM_BROADBAND_SIGNAL_EXEC_TRACE) == 1
          interface 
                   function rdtsc_wrap() bind(C,name="rdtsc_wrap")
                            use iso_c_binding, only : c_long_long 
                            integer(c_long_long) :: rdtsc_wrap 
                    end function rdtsc_wrap 
               end interface
#endif 
           
#if (AM_BROADBAND_SIGNAL_EXEC_TRACE) == 1
          character(*), parameter :: sub_name = "copy_create_AM_broadband_signal"
#endif 
          type(AM_broadband_signal_t),        intent(inout)        :: this
          type(AM_broadband_signal_t),        intent(in)         :: other 
          integer(kind=i4), automatic :: i__,j__,k__ 

#if (AM_BROADBAND_SIGNAL_EXEC_TRACE) == 1
          print*, "[EXECUTION-TRACE: START]"
          print*, "File:     ", __FILE__
          print*, "Procedure:", sub_name 
          print*, "LOC:      ", __LINE__ 
          print*, "TSC-Start:", rdtsc_wrap()
#endif
             if(this.m_creation_state  .eq. .false. .or. &
                other.m_creation_state .eq. .false.) return 
             if(LOC(this) .eq. LOC(other)) return 
             ! deallocate current (existing) state 
             if(allocated(this.m_code_seq))     deallocate(this.m_code_seq) 
             if(allocated(this.m_carrier))      deallocate(this.m_carrier)    
             if(allocated(this.m_complex_env))  deallocate(this.m_complex_env)
             if(allocated(this.m_signal))       deallocate(this.m_signal)     
             if(allocated(this.m_env_spec))     deallocate(this.m_env_spec)   
             if(allocated(this.m_samples))      deallocate(this.m_samples)    
             if(allocated(this.m_env_correl))   deallocate(this.m_env_correl) 
             if(allocated(this.m_ambiguity))    deallocate(this.m_ambiguity)  
             if(allocated(this.m_abs_ambiguity))deallocate(this.m_abs_ambiguity)    
             if(allocated(this.m_carrier_i))    deallocate(this.m_carrier_i)    
             if(allocated(this.m_carrier_q))    deallocate(this.m_carrier_q)    
             if(allocated(this.m_complex_env_i))deallocate(this.m_complex_env_i)
             if(allocated(this.m_complex_env_q))deallocate(this.m_complex_env_q)
             if(allocated(this.m_signal_i))     deallocate(this.m_signal_i)     
             if(allocated(this.m_signal_q))     deallocate(this.m_signal_q)     
             if(allocated(this.m_env_correl_i)) deallocate(this.m_env_correl_i) 
             if(allocated(this.m_env_correl_q)) deallocate(this.m_env_correl_q)

             this.m_signal_name   =    other.m_signal_name
             this.m_envelope_type =    other.m_envelope_type
             this.m_distro_omega  =    other.m_distro_omega
             this.m_distro_theta  =    other.m_distro_theta
             this.m_code_type     =    other.m_code_type
             this.m_id            =    other.m_id  
             this.m_interval_1    =    other.m_interval_1
             this.m_interval_2    =    other.m_interval_2  
             this.m_interval_3    =    other.m_interval_3
             this.m_baude_rate    =    other.m_baude_rate
             this.m_Ts            =    other.m_Ts 
             this.m_Te            =    other.m_Te 
             this.m_Ns            =    other.m_Ns 
             this.m_Ne            =    other.m_Ne 
             this.m_nfreqs        =    other.m_nfreqs
             this.m_nfreqe        =    other.m_nfreqe 
             this.m_num_samples   =    other.m_num_samples
             this.m_nomegs        =    other.m_nomegs 
             this.m_nomege        =    other.m_nomege 
             this.m_nthets        =    other.m_nthets 
             this.m_nthete        =    other.m_nthete 
             this.m_sym_dep       =    other.m_sym_dep  
             this.m_ft_process    =    other.m_ft_process 
             this.m_order         =    other.m_order 
             this.m_A0            =    other.m_A0
             this.m_Ac            =    other.m_Ac 
             this.m_invT          =    other.m_invT 
             this.m_fc            =    other.m_fc  
             this.m_fs            =    other.m_fs  
             this.m_sig_width     =    other.m_sig_width 
             this.m_sig_energy    =    other.m_sig_energy 
             this.m_snr           =    other.m_snr  
             this.m_Ps            =    other.m_Ps 

             allocate(this.m_code_seq(this.m_baude_rate))
             allocate(this.m_carrier(this.m_Ts:this.m_Te))
             allocate(this.m_complex_env(this.m_Ts:this.m_Te))
             allocate(this.m_signal(this.m_Ts:this.m_Te))
             allocate(this.m_env_spec(this.m_nfreqs:this.m_nfreqe))
             allocate(this.m_samples(this.m_Ns:this.m_Ne,this.m_Ts:this.m_Te))
             select case (this.m_order)
              case (1)
                    allocate(this.m_env_correl(this.m_nomegs:this.m_nomege,this.m_nthets:this.m_nthete))
                    allocate(this.m_ambiguity(this.m_nomegs:this.m_nomege,this.m_nthets:this.m_nthete))
                    allocate(this.m_abs_ambiguity(this.m_nomegs:this.m_nomege,this.m_nthets:this.m_nthete))
              case (2)
                    allocate(this.m_env_correl(this.m_nthets:this.m_nthete,this.m_nomegs:this.m_nomege))
                    allocate(this.m_ambiguity(this.m_nthets:this.m_nthete,this.m_nomegs:this.m_nomege))
                    allocate(this.m_abs_ambiguity(this.m_nthets:this.m_nthete,this.m_nomegs:this.m_nomege))
              case default 
                    STOP "[create_AM_broadband_signal] -- INVALID SWITCH ARGUMENT!!"
             end select 

             allocate(this.m_carrier_i(this.m_Ts:this.m_Te))
             allocate(this.m_carrier_q(this.m_Ts:this.m_Te))
             allocate(this.m_complex_env_i(this.m_Ts:this.m_Te))
             allocate(this.m_complex_env_q(this.m_Ts:this.m_Te))
             allocate(this.m_signal_i(this.m_Ts:this.m_Te))
             allocate(this.m_signal_q(this.m_Ts:this.m_Te))
             select case (this.m_order)
                case (1)
                     allocate(this.m_env_correl_i(this.m_nomegs:this.m_nomege,this.m_nthets:this.m_nthete))
                     allocate(this.m_env_correl_q(this.m_nomegs:this.m_nomege,this.m_nthets:this.m_nthete))
                case (2)
                     allocate(this.m_env_correl_i(this.m_nthets:this.m_nthete,this.m_nomegs:this.m_nomege))
                     allocate(this.m_env_correl_q(this.m_nthets:this.m_nthete,this.m_nomegs:this.m_nomege))
                case default
                     STOP "[create_AM_broadband_signal] -- INVALID SWITCH ARGUMENT!!"
             end select 

                do i__=1,this.m_baude_rate 
                   this.m_code_seq(i__) = other.m_code_seq(i__)
                end do 

                do i__= this.m_Ts,this.m_Te 
                   this.m_carrier(i__)       = other.m_carrier(i__)
                   this.m_complex_env(i__)   = other.m_complex_env(i__)
                   this.m_signal(i__)        = other.m_signal(i__)
                   this.m_carrier_i(i__)     = other.m_carrier_i(i__)
                   this.m_carrier_q(i__)     = other.m_carrier_q(i__)
                   this.m_complex_env_i(i__) = other.m_complex_env_i(i__)
                   this.m_complex_env_q(i__) = other.m_complex_env_q(i__)
                   this.m_signal_i(i__)      = other.m_signal_i(i__)
                   this.m_signal_q(i__)      = other.m_signal_q(i__)

                   do j__= this.m_Ns,iand(this.m_Ne-1,inot(3)), 4
                      this.m_samples(j__+0,i__) = other.m_samples(j__+0,i__)
                      this.m_samples(j__+1,i__) = other.m_samples(j__+1,i__)
                      this.m_samples(j__+2,i__) = other.m_samples(j__+2,i__)
                      this.m_samples(j__+3,i__) = other.m_samples(j__+3,i__)
                   end do 
                   do k__=j__,this.m_Ne 
                      this.m_samples(k__,i__)   = other.m_samples(k__,i__)
                   end do 
                end do 
                
                do i__=this.m_nfreqs,this.m_nfreqe 
                    this.m_env_spec(i__) = other.m_env_spec(i__)
                end do 
           select case (this.m_order)  
               case (1) 
                    do i__= this.m_nomegs,this.m_nomege
                           do j__ = this.m_nthets,iand(this.m_nthete-1,inot(3)), 4
                                  this.m_env_correl(j__+0,i__)    = other.m_env_correl(j__+0,i__) 
                                  this.m_ambiguity(j__+0,i__)     = other.m_ambiguity(j__+0,i__)
                                  this.m_abs_ambiguity(j__+0,i__) = other.m_abs_ambiguity(j__+0,i__)

                                  this.m_env_correl_i(j__+0,i__)  = other.m_env_correl_i(j__+0,i__)
                                  this.m_env_correl_q(j__+0,i__)  = other.m_env_correl_q(j__+0,i__)
 
                                  this.m_env_correl(j__+1,i__)    = other.m_env_correl(j__+1,i__)
                                  this.m_ambiguity(j__+1,i__)     = other.m_ambiguity(j__+1,i__)
                                  this.m_abs_ambiguity(j__+1,i__) = other.m_abs_ambiguity(j__+1,i__)

                                  this.m_env_correl_i(j__+1,i__)  = other.m_env_correl_i(j__+1,i__)
                                  this.m_env_correl_q(j__+1,i__)  = other.m_env_correl_q(j__+1,i__)
                                  
                                  this.m_env_correl(j__+2,i__)    = other.m_env_correl(j__+2,i__)
                                  this.m_ambiguity(j__+2,i__)     = other.m_ambiguity(j__+2,i__)
                                  this.m_abs_ambiguity(j__+2,i__) = other.m_abs_ambiguity(j__+2,i__)

                                  this.m_env_correl_i(j__+2,i__)  = other.m_env_correl_i(j__+2,i__)
                                  this.m_env_correl_q(j__+2,i__)  = other.m_env_correl_q(j__+2,i__)

                                  this.m_env_correl(j__+3,i__)    = other.m_env_correl(j__+3,i__)
                                  this.m_ambiguity(j__+3,i__)     = other.m_ambiguity(j__+3,i__)
                                  this.m_abs_ambiguity(j__+3,i__) = other.m_abs_ambiguity(j__+3,i__)

                                  this.m_env_correl_i(j__+3,i__)  = other.m_env_correl_i(j__+3,i__)
                                  this.m_env_correl_q(j__+3,i__)  = other.m_env_correl_q(j__+3,i__)
 
                           end do 
                           do k__=j__,this.m_nthete 
                                  this.m_env_correl(k__,i__)    = other.m_env_correl(k__,i__)
                                  this.m_ambiguity(k__,i__)     = other.m_ambiguity(k__,i__)
                                  this.m_abs_ambiguity(k__,i__) = other.m_abs_ambiguity(k__,i__)

                                  this.m_env_correl_i(k__,i__)  = other.m_env_correl_i(k__,i__)
                                  this.m_env_correl_q(k__,i__)  = other.m_env_correl_q(k__,i__)
 
                           end do 
                     end do 
               case (2)
                     do i__= this.m_nthets,this.m_nthete
                           do j__ = this.m_nomegs,iand(this.m_nomege-1,inot(3)), 4
                                  this.m_env_correl(j__+0,i__)    = other.m_env_correl(j__+0,i__) 
                                  this.m_ambiguity(j__+0,i__)     = other.m_ambiguity(j__+0,i__)
                                  this.m_abs_ambiguity(j__+0,i__) = other.m_abs_ambiguity(j__+0,i__)

                                  this.m_env_correl_i(j__+0,i__)  = other.m_env_correl_i(j__+0,i__)
                                  this.m_env_correl_q(j__+0,i__)  = other.m_env_correl_q(j__+0,i__)
 
                                  this.m_env_correl(j__+1,i__)    = other.m_env_correl(j__+1,i__)
                                  this.m_ambiguity(j__+1,i__)     = other.m_ambiguity(j__+1,i__)
                                  this.m_abs_ambiguity(j__+1,i__) = other.m_abs_ambiguity(j__+1,i__)

                                  this.m_env_correl_i(j__+1,i__)  = other.m_env_correl_i(j__+1,i__)
                                  this.m_env_correl_q(j__+1,i__)  = other.m_env_correl_q(j__+1,i__)
                                  
                                  this.m_env_correl(j__+2,i__)    = other.m_env_correl(j__+2,i__)
                                  this.m_ambiguity(j__+2,i__)     = other.m_ambiguity(j__+2,i__)
                                  this.m_abs_ambiguity(j__+2,i__) = other.m_abs_ambiguity(j__+2,i__)

                                  this.m_env_correl_i(j__+2,i__)  = other.m_env_correl_i(j__+2,i__)
                                  this.m_env_correl_q(j__+2,i__)  = other.m_env_correl_q(j__+2,i__)

                                  this.m_env_correl(j__+3,i__)    = other.m_env_correl(j__+3,i__)
                                  this.m_ambiguity(j__+3,i__)     = other.m_ambiguity(j__+3,i__)
                                  this.m_abs_ambiguity(j__+3,i__) = other.m_abs_ambiguity(j__+3,i__)

                                  this.m_env_correl_i(j__+3,i__)  = other.m_env_correl_i(j__+3,i__)
                                  this.m_env_correl_q(j__+3,i__)  = other.m_env_correl_q(j__+3,i__)
 
                           end do 
                           do k__=j__,this.m_nomege 
                                  this.m_env_correl(k__,i__)    = other.m_env_correl(k__,i__)
                                  this.m_ambiguity(k__,i__)     = other.m_ambiguity(k__,i__)
                                  this.m_abs_ambiguity(k__,i__) = other.m_abs_ambiguity(k__,i__)

                                  this.m_env_correl_i(k__,i__)  = other.m_env_correl_i(k__,i__)
                                  this.m_env_correl_q(k__,i__)  = other.m_env_correl_q(k__,i__)
 
                           end do 
                     end do 
           end select 
           this.m_creation_state = other.m_creation_state  
#if (AM_BROADBAND_SIGNAL_EXEC_TRACE) == 1
          print*, "[EXECUTION-TRACE: END]"
          print*, "TSC-End:", rdtsc_wrap()
#endif
     end subroutine copy_assign_AM_broadband_signal

     subroutine check_allocation_stats_real1D(array,name)
                       implicit none
                       real(kind=sp), allocatable, dimension(:), intent(in) :: array 
                       character(len=*),                         intent(in) :: name 
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
                       print*, "======================INFO==========================="
                       print*, "Array member name:", name 
                       print*, "vaddr=",vaddr
                       print*, "size=",size
                       print*, "allocated=",is_allocated
                       print*, "lo_bound=",lo_bound
                       print*, "hi_bound=",hi_bound 
                       print*,"======================INFO==========================="
      end subroutine check_allocation_stats_real1D

      subroutine check_allocation_stats_real2D(array,name)
                       implicit none
                       real(kind=sp), allocatable, dimension(:,:), intent(in) :: array 
                       character(len=*),                           intent(in) :: name 
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
                       print*,"======================INFO======================"
                       print*, "Array member name:", name 
                       print*, "vaddr=",vaddr
                       print*, "size=",size
                       print*, "allocated=",is_allocated
                       print*, "lo_bound=",lo_bound
                       print*, "hi_bound=",hi_bound 
                       print*,"======================INFO======================================"
     end subroutine check_allocation_stats_real2D

     subroutine check_allocation_stats_complex1D(array,name)
                       implicit none
                       complex(kind=sp), allocatable, dimension(:), intent(in) :: array 
                       character(len=*),                            intent(in) :: name 
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
                       print*,"======================INFO==================="
                       print*, "Array member name:", name 
                       print*, "vaddr=",vaddr
                       print*, "size=",size
                       print*, "allocated=",is_allocated
                       print*, "lo_bound=",lo_bound
                       print*, "hi_bound=",hi_bound 
                       print*,"======================INFO==================="
     end subroutine check_allocation_stats_complex1D

     subroutine check_allocation_stats_complex2D(array,name)
                       implicit none
                       complex(kind=sp), allocatable, dimension(:,:), intent(in) :: array 
                       character(len=*),                              intent(in) :: name 
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
                       print*,"======================INFO======================"
                       print*, "Array member name:", name 
                       print*, "vaddr=",vaddr
                       print*, "size=",size
                       print*, "allocated=",is_allocated
                       print*, "lo_bound=",lo_bound
                       print*, "hi_bound=",hi_bound 
                       print*,"======================INFO======================"
     end subroutine check_allocation_stats_complex2D

     
     !  show allocation status
     subroutine show_AM_broadband_signal_state(AM_signal)
          implicit none
#if (AM_BROADBAND_SIGNAL_EXEC_TRACE) == 1
          interface 
                   function rdtsc_wrap() bind(C,name="rdtsc_wrap")
                            use iso_c_binding, only : c_long_long 
                            integer(c_long_long) :: rdtsc_wrap 
                    end function rdtsc_wrap 
               end interface
#endif 
          
#if (AM_BROADBAND_SIGNAL_EXEC_TRACE) == 1
         character(*), parameter :: sub_name = "show_AM_broadband_signal_state"
#endif 
         type(AM_broadband_signal_t),        intent(in)        :: AM_signal
         integer(kind=i8), automatic :: obj_vaddr
         integer(kind=i8), automatic :: obj_size 

#if (AM_BROADBAND_SIGNAL_EXEC_TRACE) == 1
          print*, "[EXECUTION-TRACE: START]"
          print*, "File:     ", __FILE__
          print*, "Procedure:", sub_name 
          print*, "LOC:      ", __LINE__ 
          print*, "TSC-Start:", rdtsc_wrap()
#endif
         obj_vaddr = LOC(AM_signal)
         obj_size  = SIZEOF(AM_signal)
         print*, "[AM_broadband_signal_t] -- INFO:  array members"
         print*, "AM_signal vaddr:        ", obj_vaddr 
         print*, "AM_signal size (bytes): ", obj_size 
         call check_allocation_stats_real1D(AM_signal.m_code_seq,       "m_code_seq")
         call check_allocation_stats_complex1D(AM_signal.m_carrier,     "m_carrier")
         call check_allocation_stats_complex1D(AM_signal.m_complex_env, "m_complex_env")
         call check_allocation_stats_complex1D(AM_signal.m_env_spec,    "m_env_spec")
         call check_allocation_stats_complex1D(AM_signal.m_signal,      "m_signal")
         call check_allocation_stats_complex2D(AM_signal.m_samples,     "m_samples")
         call check_allocation_stats_complex2D(AM_signal.m_env_correl,  "m_env_correl")
         call check_allocation_stats_complex2D(AM_signal.m_ambiguity,   "m_ambiguity")
         call check_allocation_stats_real2D(AM_signal.m_abs_ambiguity,  "m_abs_ambiguity")
         call check_allocation_stats_real1D(AM_signal.m_carrier_i,      "m_carrier_i")
         call check_allocation_stats_real1D(AM_signal.m_carrier_q,      "m_carrier_q")
         call check_allocation_stats_real1D(AM_signal.m_complex_env_i,  "m_complex_env_i")
         call check_allocation_stats_real1D(AM_signal.m_complex_env_q,  "m_complex_env_q")
         call check_allocation_stats_real1D(AM_signal.m_signal_i,       "m_signal_i")
         call check_allocation_stats_real1D(AM_signal.m_signal_q,       "m_signal_q")
         call check_allocation_stats_real2D(AM_signal.m_env_correl_i,   "m_env_correl_i")
         call check_allocation_stats_real2D(AM_signal.m_env_correl_q,   "m_env_correl_q")
         print*,"[AM_broadband_signal_t] -- INFO: scalar members"
         print*, "m_signal_name=",    AM_signal.m_signal_name
         print*, "m_envelope_type=",  AM_signal.m_envelope_type 
         print*, "m_distro_omega=",   AM_signal.m_distro_omega
         print*, "m_distro_theta=",   AM_signal.m_distro_theta
         print*, "m_code_type=",      AM_signal.m_code_type 
         print*, "m_id=",             AM_signal.m_id 
         print*, "m_interval_1=",     AM_signal.m_interval_1  
         print*, "m_interval_2=",     AM_signal.m_interval_2 
         print*, "m_interval_3=",     AM_signal.m_interval_3
         print*, "m_baude_rate=",     AM_signal.m_baude_rate
         print*, "m_Ns=",             AM_signal.m_Ns 
         print*, "m_Ne=",             AM_signal.m_Ne 
         print*, "m_Ts=",             AM_signal.m_Ts 
         print*, "m_Te=",             AM_signal.m_Te 
         print*, "m_nfreqs=",         AM_signal.m_nfreqs
         print*, "m_nfreqe=",         AM_signal.m_nfreqe 
         print*, "m_num_samples=",    AM_signal.m_num_samples 
         print*, "m_nomegs=",         AM_signal.m_nomegs 
         print*, "m_nomege=",         AM_signal.m_nomege 
         print*, "m_nthets=",         AM_signal.m_nthets 
         print*, "m_nthete",          AM_signal.m_nthete         
         print*, "m_sym_dep=",        AM_signal.m_sym_dep        
         print*, "m_ft_process=",     AM_signal.m_ft_process
         print*, "m_order=",          AM_signal.m_order 
         print*, "m_A0=",             AM_signal.m_A0  
         print*, "m_Ac=",             AM_signal.m_Ac                               
         print*, "m_invT=",           AM_signal.m_invT           
         print*, "m_fc=",             AM_signal.m_fc             
         print*, "m_fs=",             AM_signal.m_fs           
         print*, "m_sig_width=",      AM_signal.m_sig_width     
         print*, "m_sig_energy=",     AM_signal.m_sig_energy    
         print*, "m_snr=",            AM_signal.m_snr           ! signal-to-noise ratio
         print*, "m_Ps=",             AM_signal.m_Ps ! SEP (symbol error probability)
         print*, "m_creation_state=", AM_signal.m_creation_state
#if (AM_BROADBAND_SIGNAL_EXEC_TRACE) == 1
          print*, "[EXECUTION-TRACE: END]"
          print*, "TSC-End:", rdtsc_wrap()
#endif
     end subroutine show_AM_broadband_signal_state

     ! Start of computational subroutines
     ! Signal derived type is completely decomposed
     subroutine modulate_AM_broadband_signal(envelope_type,               &
                                             baude_rate,                  &
                                             Ns,                          &
                                             Ne,                          &
                                             Ts,                          &
                                             Te,                          &
                                             A0,                          &
                                             Ac,                          &
                                             invT,                        &
                                             fc,                          &
                                             code_seq,                    &
                                             carrier,                     &
                                             complex_env,                 &
                                             signal,                      &
                                             samples,                     &
                                             carrier_i,                   &
                                             carrier_q,                   &
                                             signal_i,                    &
                                             signal_q,                    &
                                             complex_env_i,               &
                                             complex_env_q,               &
                                             ioerr)
          use omp_lib
          implicit none
#if (AM_BROADBAND_SIGNAL_EXEC_TRACE) == 1
          interface 
                   function rdtsc_wrap() bind(C,name="rdtsc_wrap")
                            use iso_c_binding, only : c_long_long 
                            integer(c_long_long) :: rdtsc_wrap 
                    end function rdtsc_wrap 
               end interface
#endif 
           
#if (AM_BROADBAND_SIGNAL_EXEC_TRACE) == 1
          character(*), parameter :: sub_name = "modulate_AM_broadband_signal"
#endif 
          character(len=*),                         intent(in), value :: envelope_type 
          integer(kind=i4),                         intent(in), value :: baude_rate 
          integer(kind=i4),                         intent(in), value :: Ns 
          integer(kind=i4),                         intent(in), value :: Ne 
          integer(kind=i4),                         intent(in), value :: Ts 
          integer(kind=i4),                         intent(in), value :: Te 
          complex(kind=sp),                         intent(in), value :: A0 
          complex(kind=sp),                         intent(in), value :: Ac ! carrier amplitude 
          real(kind=sp),                            intent(in), value :: invT 
          real(kind=sp),                            intent(in), value :: fc 
          real(kind=sp),    dimension(1:baude_rate),intent(in)        :: code_seq 
          complex(kind=sp), dimension(Ts:Te),       intent(out)       :: carrier 
          complex(kind=sp), dimension(Ts:Te),       intent(out)       :: complex_env 
          complex(kind=sp), dimension(Ts:Te),       intent(out)       :: signal 
          complex(kind=sp), dimension(Ns:Ne,Ts:Te), intent(out)       :: samples 
          real(kind=sp),    dimension(Ts:Te),       intent(out)       :: carrier_i 
          real(kind=sp),    dimension(Ts:Te),       intent(out)       :: carrier_q 
          real(kind=sp),    dimension(Ts:Te),       intent(out)       :: signal_i 
          real(kind=sp),    dimension(Ts:Te),       intent(out)       :: signal_q 
          real(kind=sp),    dimension(Ts:Te),       intent(out)       :: complex_env_i 
          real(kind=sp),    dimension(Ts:Te),       intent(out)       :: complex_env_q 
          integer(kind=i4),                         intent(out)       :: ioerr 
          ! Locals
          
          complex(kind=sp), automatic :: c_amp 
          complex(kind=sp), automatic :: yc_t 
          complex(kind=sp), automatic :: ctmp 
          complex(kind=sp), automatic :: sig_sum 
          complex(kind=sp), parameter :: j = cmplx(0.0_sp,1.0_sp)
          real(kind=sp),    parameter :: C6283185307179586476925286766559 = &
                                               6.283185307179586476925286766559_sp
          real(kind=sp),    parameter :: C314159265358979323846264338328 = &
                                               3.14159265358979323846264338328_sp
          real(kind=sp),    automatic :: r_phase
          real(kind=sp),    automatic :: rand_r 
          real(kind=sp),    automatic :: r_t 
          real(kind=sp),    automatic :: r_k 
          real(kind=sp),    automatic :: Tp_i 
          real(kind=sp),    automatic :: Tp_j 
          real(kind=sp),    automatic :: fs_i
          real(kind=sp),    automatic :: fs_j
          real(kind=sp),    automatic :: h_spread
          real(kind=sp),    automatic :: v_spread
          real(kind=sp),    automatic :: r_Ts
          real(kind=sp),    automatic :: r_Te 
          real(kind=sp),    automatic :: t0
          real(kind=sp),    automatic :: t1
          real(kind=sp),    automatic :: t2 
          real(kind=sp),    automatic :: t3 
          real(kind=sp),    automatic :: arg 
          real(kind=sp),    automatic :: r_seq 
          integer(kind=i4), automatic :: trunc_r
          integer(kind=i4), automatic :: i__,j__,k__ 
          ! Exec code 
#if (AM_BROADBAND_SIGNAL_EXEC_TRACE) == 1
          print*, "File:     ", __FILE__
          print*, "Procedure:", sub_name 
          print*, "LOC:      ", __LINE__ 
          print*, "TSC-Start:", rdtsc_wrap()
          
#endif 
           
          select case (envelope_type)
               case ("Trapezoidal")
                  fs_i     = 0.0_sp 
                  fs_j     = 0.0_sp 
                  Tp_i     = 1.0_sp/real(2.0_sp*(Te-Ts),kind=sp)
                  Tp_j     = 1.0_sp/real(2.0_sp*(Ne-Ns),kind=sp)
                  r_Ts     = real(Ts,kind=sp)
                  r_Te     = real(Te,kind=sp)
                  h_spread = (r_Te-r_Ts)/8.0_sp   
                  v_spread = real(A0)
                  
                  t0       = C314159265358979323846264338328/(r_Te-r_Ts)
                  r_phase  = 0.0_sp 
                  call random_seed()
                  call random_number(rand_r)
                  trunc_r = floor(2.0_sp*rand_r)
                  if(trunc_r .eq. 1)  then 
                     r_phase = rand_r*C314159265358979323846264338328
                  else if(trunc_r .eq. 0) then 
                    r_phase = -C314159265358979323846264338328*rand_r
                  end if 
                  !print*, "random-phase=", r_phase 
!dir$ assume_aligned code_seq:64
!dir$ assume_aligned carrier:64
!dir$ assume_aligned complex_env:64
!dir$ assume_aligned signal:64
!dir$ assume_aligned samples:64
!dir$ assume_aligned carrier_i:64
!dir$ assume_aligned carrier_q:64
!dir$ assume_aligned signal_i:64
!dir$ assume_aligned signal_q:64
!dir$ assume_aligned complex_env_i:64
!dir$ assume_aligned complex_env_q:64
                  do i__ = Ts,Te 
                     r_t          = real(i__,kind=sp)
                     fs_i         = r_t*Tp_i 
                     carrier(i__) = Ac*exp(j*C6283185307179586476925286766559*fc*fs_i+r_phase)
                     sig_sum      = cmplx(0.0_sp,0.0_sp)
                     c_amp        = cmplx(0.0_sp,0.0_sp)
!$omp simd linear(j__:1) aligned(code_seq,samples:64) reduction(+:sig_sum) 
                     do j__ = Ns,Ne 
                        r_k              = real(j__,kind=sp)
                        c_amp            = A0*0.318309886183790671537767526745_sp
                        fs_j             = r_k*Tp_j
                        r_seq            = code_seq(j__)
                        arg              = t0*(fs_i-fs_j)+h_spread
                        t1               = asin(sin(arg))
                        t2               = acos(cos(arg))
                        t3               = (t1+t2)-v_spread*0.5_sp+v_spread 
                        yc_t             = c_amp*t3 
                        ctmp             = yc_t*r_seq
                        sig_sum          = sig_sum+ctmp
                        samples(j__,i__) = ctmp
                     end do 
                     complex_env(i__)      = sig_sum
                     signal(i__)           = sig_sum*carrier(i__)
                     carrier_i(i__)        = real(carrier(i__))
                     carrier_q(i__)        = aimag(carrier(i__))
                     signal_i(i__)         = real(signal(i__))
                     signal_q(i__)         = aimag(signal(i__))
                     complex_env_i(i__)    = real(complex_env(i__))
                     complex_env_q(i__)    = aimag(complex_env(i__))
                  end do 
               case ("Sine_squared")
                     fs_i     = 0.0_sp 
                     fs_j     = 0.0_sp 
                     Tp_i     = 1.0_sp/real(2.0_sp*(Te-Ts),kind=sp)
                     Tp_j     = 1.0_sp/real(2.0_sp*(Ne-Ns),kind=sp)
                     r_phase  = 0.0_sp 
                     call random_seed()
                     call random_number(rand_r)
                     trunc_r = floor(2.0_sp*rand_r)
                     if(trunc_r .eq. 1)  then 
                        r_phase = rand_r*C314159265358979323846264338328
                     else if(trunc_r .eq. 0) then 
                        r_phase = -C314159265358979323846264338328*rand_r
                     end if 
                     print*,"random_phase=", r_phase 
!dir$ assume_aligned code_seq:64
!dir$ assume_aligned carrier:64
!dir$ assume_aligned complex_env:64
!dir$ assume_aligned signal:64
!dir$ assume_aligned samples:64
!dir$ assume_aligned carrier_i:64
!dir$ assume_aligned carrier_q:64
!dir$ assume_aligned signal_i:64
!dir$ assume_aligned signal_q:64
!dir$ assume_aligned complex_env_i:64
!dir$ assume_aligned complex_env_q:64
                   do i__ = Ts,Te 
                      r_t          = real(i__,kind=sp)
                      fs_i         = r_t*Tp_i 
                      carrier(i__) = Ac*exp(j*C6283185307179586476925286766559*fc*fs_i+r_phase)
                      sig_sum      = cmplx(0.0_sp,0.0_sp)
!$omp simd linear(j__:1) aligned(code_seq,samples:64) reduction(+:sig_sum)
                      do j__ = Ns,Ne 
                         r_k              = real(j__,kind=sp)
                         fs_j             = r_k*Tp_j
                         r_seq            = code_seq(j__)
                         arg              = C6283185307179586476925286766559*fs_i-fs_j 
                         t0               = 1.0_sp-cos(arg)
                         yc_t             = A0*0.5_sp*t0 
                         ctmp             = yc_t*r_seq
                         sig_sum          = sig_sum+ctmp
                         samples(j__,i__) = ctmp
                      end do 
                       complex_env(i__)      = sig_sum
                       signal(i__)           = sig_sum*carrier(i__)
                       carrier_i(i__)        = real(carrier(i__))
                       carrier_q(i__)        = aimag(carrier(i__))
                       signal_i(i__)         = real(signal(i__))
                       signal_q(i__)         = aimag(signal(i__))
                       complex_env_i(i__)    = real(complex_env(i__))
                       complex_env_q(i__)    = aimag(complex_env(i__))
                   end do    
               case ("Sine")
                     fs_i     = 0.0_sp 
                     fs_j     = 0.0_sp 
                     Tp_i     = 1.0_sp/real(2.0_sp*(Te-Ts),kind=sp)
                     Tp_j     = 1.0_sp/real(2.0_sp*(Ne-Ns),kind=sp)
                     r_phase  = 0.0_sp 
                     call random_seed()
                     call random_number(rand_r)
                     trunc_r = floor(2.0_sp*rand_r)
                     if(trunc_r .eq. 1)  then 
                        r_phase = rand_r*C314159265358979323846264338328
                     else if(trunc_r .eq. 0) then 
                        r_phase = -C314159265358979323846264338328*rand_r
                     end if 
                     print*,"random_phase=", r_phase 
!dir$ assume_aligned code_seq:64
!dir$ assume_aligned carrier:64
!dir$ assume_aligned complex_env:64
!dir$ assume_aligned signal:64
!dir$ assume_aligned samples:64
!dir$ assume_aligned carrier_i:64
!dir$ assume_aligned carrier_q:64
!dir$ assume_aligned signal_i:64
!dir$ assume_aligned signal_q:64
!dir$ assume_aligned complex_env_i:64
!dir$ assume_aligned complex_env_q:64
                   do i__ = Ts,Te 
                      r_t          = real(i__,kind=sp)
                      fs_i         = r_t*Tp_i
                      carrier(i__) = Ac*exp(j*C6283185307179586476925286766559*fc*fs_i+r_phase)
                      sig_sum      = cmplx(0.0_sp,0.0_sp)
!$omp simd linear(j__:1) aligned(code_seq,samples:64) reduction(+:sig_sum)
                      do j__ = Ns,Ne 
                         r_k              = real(j__,kind=sp)
                         fs_j             = r_k*Tp_j 
                         r_seq            = code_seq(j__)
                         arg              = C314159265358979323846264338328*fs_i-fs_j 
                         t0               = sin(arg)
                         yc_t             = A0*t0 
                         ctmp             = yc_t*r_seq
                         sig_sum          = sig_sum+ctmp
                         samples(j__,i__) = ctmp
                      end do 
                       complex_env(i__)      = sig_sum
                       signal(i__)           = sig_sum*carrier(i__)
                       carrier_i(i__)        = real(carrier(i__))
                       carrier_q(i__)        = aimag(carrier(i__))
                       signal_i(i__)         = real(signal(i__))
                       signal_q(i__)         = aimag(signal(i__))
                       complex_env_i(i__)    = real(complex_env(i__))
                       complex_env_q(i__)    = aimag(complex_env(i__))
                   end do 
               case default 
                   print*, "Invalid switch variable: ", envelope_type 
                   ioerr = -1
                   return 
            end select 
            ioerr = 0    
#if (AM_BROADBAND_SIGNAL_EXEC_TRACE) == 1
          print*, "[EXECUTION-TRACE: END]"
          print*, "TSC-End:", rdtsc_wrap()
#endif              
     end subroutine modulate_AM_broadband_signal


     ! Start of computational subroutines
     ! Signal derived type is completely decomposed
     subroutine modulate_add_noise_AM_broadband_signal(envelope_type,               &
                                                       baude_rate,                  &
                                                       Ns,                          &
                                                       Ne,                          &
                                                       Ts,                          &
                                                       Te,                          &
                                                       A0,                          &
                                                       Ac,                          &
                                                       invT,                        &
                                                       fc,                          &
                                                       scale_r,                     &
                                                       scale_i,                     &
                                                       rd_params,                   &
                                                       which_distro,                &
                                                       code_seq,                    &
                                                       carrier,                     &
                                                       complex_env,                 &
                                                       signal,                      &
                                                       samples,                     &
                                                       carrier_i,                   &
                                                       carrier_q,                   &
                                                       signal_i,                    &
                                                       signal_q,                    &
                                                       complex_env_i,               &
                                                       complex_env_q,               &
                                                       ioerr)
          use omp_lib
          use rand_scalar_distributions
          implicit none
#if (AM_BROADBAND_SIGNAL_EXEC_TRACE) == 1
          interface 
                   function rdtsc_wrap() bind(C,name="rdtsc_wrap")
                            use iso_c_binding, only : c_long_long 
                            integer(c_long_long) :: rdtsc_wrap 
                    end function rdtsc_wrap 
               end interface
#endif 
           
#if (AM_BROADBAND_SIGNAL_EXEC_TRACE) == 1
          character(*), parameter :: sub_name = "modulate_add_noise_AM_broadband_signal"
#endif 
          character(len=*),                         intent(in), value :: envelope_type 
          integer(kind=i4),                         intent(in), value :: baude_rate 
          integer(kind=i4),                         intent(in), value :: Ns 
          integer(kind=i4),                         intent(in), value :: Ne 
          integer(kind=i4),                         intent(in), value :: Ts 
          integer(kind=i4),                         intent(in), value :: Te 
          complex(kind=sp),                         intent(in)        :: A0 
          complex(kind=sp),                         intent(inout)     :: Ac ! carrier amplitude 
          real(kind=sp),                            intent(in), value :: invT 
          real(kind=sp),                            intent(in), value :: fc 
          real(kind=sp),                            intent(in), value :: scale_r 
          real(kind=sp),                            intent(in), value :: scale_i
          type(rand_distro_params_t),               intent(in)        :: rd_params 
          character(len=*),                         intent(in)        :: which_distro 
          real(kind=sp),    dimension(1:baude_rate),intent(in)        :: code_seq 
          complex(kind=sp), dimension(Ts:Te),       intent(out)       :: carrier 
          complex(kind=sp), dimension(Ts:Te),       intent(out)       :: complex_env 
          complex(kind=sp), dimension(Ts:Te),       intent(out)       :: signal 
          complex(kind=sp), dimension(Ns:Ne,Ts:Te), intent(out)       :: samples 
          real(kind=sp),    dimension(Ts:Te),       intent(out)       :: carrier_i 
          real(kind=sp),    dimension(Ts:Te),       intent(out)       :: carrier_q 
          real(kind=sp),    dimension(Ts:Te),       intent(out)       :: signal_i 
          real(kind=sp),    dimension(Ts:Te),       intent(out)       :: signal_q 
          real(kind=sp),    dimension(Ts:Te),       intent(out)       :: complex_env_i 
          real(kind=sp),    dimension(Ts:Te),       intent(out)       :: complex_env_q 
          integer(kind=i4),                         intent(out)       :: ioerr 
          ! Locals
          
          complex(kind=sp), automatic :: c_amp 
          complex(kind=sp), automatic :: A0_cpy
          complex(kind=sp), automatic :: yc_t 
          complex(kind=sp), automatic :: ctmp 
          complex(kind=sp), automatic :: sig_sum 
          complex(kind=sp), automatic :: c_scale 
          complex(kind=sp), automatic :: c_rand_i 
          complex(kind=sp), automatic :: c_rand_j 
          complex(kind=sp), parameter :: j = cmplx(0.0_sp,1.0_sp)
          real(kind=sp),    parameter :: C6283185307179586476925286766559 = &
                                               6.283185307179586476925286766559_sp
          real(kind=sp),    parameter :: C314159265358979323846264338328 = &
                                               3.14159265358979323846264338328_sp
          real(kind=sp),    automatic :: r_phase
          real(kind=sp),    automatic :: rand_r 
          real(kind=sp),    automatic :: t_i_re 
          real(kind=sp),    automatic :: t_i_im 
          real(kind=sp),    automatic :: t_j_re
          real(kind=sp),    automatic :: t_j_im
          real(kind=sp),    automatic :: r_t 
          real(kind=sp),    automatic :: r_k 
          real(kind=sp),    automatic :: fs_i 
          real(kind=sp),    automatic :: fs_j 
          real(kind=sp),    automatic :: Tp_i 
          real(kind=sp),    automatic :: Tp_j 
          real(kind=sp),    automatic :: h_spread
          real(kind=sp),    automatic :: v_spread
          real(kind=sp),    automatic :: r_Ts
          real(kind=sp),    automatic :: r_Te 
          real(kind=sp),    automatic :: t0
          real(kind=sp),    automatic :: t1
          real(kind=sp),    automatic :: t2 
          real(kind=sp),    automatic :: t3 
          real(kind=sp),    automatic :: arg 
          real(kind=sp),    automatic :: r_seq 
          integer(kind=i4), automatic :: trunc_r
          integer(kind=i4), automatic :: i__,j__,k__ 
          ! Exec code 
#if (AM_BROADBAND_SIGNAL_EXEC_TRACE) == 1
          print*, "File:     ", __FILE__
          print*, "Procedure:", sub_name 
          print*, "LOC:      ", __LINE__ 
          print*, "TSC-Start:", rdtsc_wrap()
          
#endif 
           
          select case (envelope_type)
               case ("Trapezoidal")
#if 0
%equation
a = 10  %Amplitude
m = 5   %Time Period
l = 5   %Horizontal Spread
c = 2   %Vertical Spread
x = 0:.1:10 %Sample Points
Trapezoidal_Wave = a/pi*(asin(sin((pi/m)*x+l))+acos(cos((pi/m)*x+l)))-a/2+c;
#endif
                  fs_i     = 0.0_sp 
                  fs_j     = 0.0_sp 
                  Tp_i     = 1.0_sp/real(2.0_sp*(Te-Ts),kind=sp)
                  Tp_j     = 1.0_sp/real(2.0_sp*(Ne-Ns),kind=sp)
                  r_Ts     = real(Ts,kind=sp)
                  r_Te     = real(Te,kind=sp)
                  h_spread = r_Te-r_Ts   
                  v_spread = real(A0)
                  t0       = C314159265358979323846264338328/(r_Te-r_Ts)
                  r_phase  = 0.0_sp 
                  call random_seed()
                  call random_number(rand_r)
                  trunc_r = floor(2.0_sp*rand_r)
                  if(trunc_r .eq. 1)  then 
                     r_phase = rand_r*C314159265358979323846264338328
                  else if(trunc_r .eq. 0) then 
                    r_phase = -C314159265358979323846264338328*rand_r
                  end if 
                  print*, "random-phase=", r_phase 
                  if(which_distro .eq. "random_normal") then 
                     c_scale = cmplx(scale_r,scale_i)
                     !c_amp   = A0/C314159265358979323846264338328
!dir$ assume_aligned code_seq:64
!dir$ assume_aligned carrier:64
!dir$ assume_aligned complex_env:64
!dir$ assume_aligned signal:64
!dir$ assume_aligned samples:64
!dir$ assume_aligned carrier_i:64
!dir$ assume_aligned carrier_q:64
!dir$ assume_aligned signal_i:64
!dir$ assume_aligned signal_q:64
!dir$ assume_aligned complex_env_i:64
!dir$ assume_aligned complex_env_q:64
                     do i__ = Ts,Te 
                         t_i_re       = random_normal_clamped()
                         t_i_im       = random_normal_clamped()
                         c_rand_i     = cmplx(t_i_re,t_i_im)
                         Ac           = Ac+c_rand_i*c_scale 
                         r_t          = real(i__,kind=sp)
                         fs_i         = r_t*Tp_i 
                         carrier(i__) = Ac*exp(j*C6283185307179586476925286766559*fc*fs_i+r_phase)
                         sig_sum      = cmplx(0.0_sp,0.0_sp)
                         c_amp        = cmplx(0.0_sp,0.0_sp)
                         do j__ = Ns,Ne 
                            t_j_re           = random_normal_clamped()
                            c_amp            = A0/C314159265358979323846264338328
                            t_j_im           = random_normal_clamped()
                            c_rand_j         = cmplx(t_j_re,t_j_im)
                            r_k              = real(j__,kind=sp)
                            fs_j             = r_k*Tp_j 
                            r_seq            = code_seq(j__)
                            arg              = t0*(fs_i-fs_j)+h_spread
                            t1               = asin(sin(arg))
                            t2               = acos(cos(arg))
                            t3               = (t1+t2)-v_spread*0.5_sp+v_spread 
                            c_amp            = c_amp+c_rand_j*c_scale 
                            yc_t             = c_amp*t3 
                            ctmp             = yc_t*r_seq
                            sig_sum          = sig_sum+ctmp
                            samples(j__,i__) = ctmp
                         end do 
                         complex_env(i__)      = sig_sum
                         signal(i__)           = sig_sum*carrier(i__)
                         carrier_i(i__)        = real(carrier(i__))
                         carrier_q(i__)        = aimag(carrier(i__))
                         signal_i(i__)         = real(signal(i__))
                         signal_q(i__)         = aimag(signal(i__))
                         complex_env_i(i__)    = real(complex_env(i__))
                         complex_env_q(i__)    = aimag(complex_env(i__))
                     end do 
                  else if(which_distro .eq. "rand_exponential_clamped") then 
                     c_scale = cmplx(scale_r,scale_i)
                     
!dir$ assume_aligned code_seq:64
!dir$ assume_aligned carrier:64
!dir$ assume_aligned complex_env:64
!dir$ assume_aligned signal:64
!dir$ assume_aligned samples:64
!dir$ assume_aligned carrier_i:64
!dir$ assume_aligned carrier_q:64
!dir$ assume_aligned signal_i:64
!dir$ assume_aligned signal_q:64
!dir$ assume_aligned complex_env_i:64
!dir$ assume_aligned complex_env_q:64
                     do i__ = Ts,Te 
                         t_i_re       = random_exponential_clamped()
                         t_i_im       = random_exponential_clamped()
                         c_rand_i     = cmplx(t_i_re,t_i_im)
                         Ac           = Ac+c_rand_i*c_scale 
                         r_t          = real(i__,kind=sp)
                         fs_i         = r_t*Tp_i 
                         carrier(i__) = Ac*exp(j*C6283185307179586476925286766559*fc*fs_i+r_phase)
                         sig_sum      = cmplx(0.0_sp,0.0_sp)
                         c_amp        = cmplx(0.0_sp,0.0_sp)
                         do j__ = Ns,Ne 
                            t_j_re           = random_exponential_clamped()
                            c_amp            = A0/C314159265358979323846264338328
                            t_j_im           = random_exponential_clamped()
                            c_rand_j         = cmplx(t_j_re,t_j_im)
                            r_k              = real(j__,kind=sp)
                            fs_j             = r_k*Tp_j
                            r_seq            = code_seq(j__)
                            arg              = t0*(fs_i-fs_j)+h_spread
                            t1               = asin(sin(arg))
                            t2               = acos(cos(arg))
                            t3               = (t1+t2)-v_spread*0.5_sp+v_spread 
                            c_amp            = c_amp+c_rand_j*c_scale 
                            yc_t             = c_amp*t3 
                            ctmp             = yc_t*r_seq
                            sig_sum          = sig_sum+ctmp
                            samples(j__,i__) = ctmp
                         end do 
                         complex_env(i__)      = sig_sum
                         signal(i__)           = sig_sum*carrier(i__)
                         carrier_i(i__)        = real(carrier(i__))
                         carrier_q(i__)        = aimag(carrier(i__))
                         signal_i(i__)         = real(signal(i__))
                         signal_q(i__)         = aimag(signal(i__))
                         complex_env_i(i__)    = real(complex_env(i__))
                         complex_env_q(i__)    = aimag(complex_env(i__))
                     end do 
                  else if (which_distro .eq. "rand_weibull_clamped") then 
                         c_scale = cmplx(scale_r,scale_i)
                        
!dir$ assume_aligned code_seq:64
!dir$ assume_aligned carrier:64
!dir$ assume_aligned complex_env:64
!dir$ assume_aligned signal:64
!dir$ assume_aligned samples:64
!dir$ assume_aligned carrier_i:64
!dir$ assume_aligned carrier_q:64
!dir$ assume_aligned signal_i:64
!dir$ assume_aligned signal_q:64
!dir$ assume_aligned complex_env_i:64
!dir$ assume_aligned complex_env_q:64
                     do i__ = Ts,Te 
                         t_i_re       = random_Weibull_clamped(rd_params.random_weibull_a_r)
                         t_i_im       = random_Weibull_clamped(rd_params.random_weibull_a_i)
                         c_rand_i     = cmplx(t_i_re,t_i_im)
                         Ac           = Ac+c_rand_i*c_scale 
                         r_t          = real(i__,kind=sp)
                         fs_i         = r_t*Tp_i 
                         carrier(i__) = Ac*exp(j*C6283185307179586476925286766559*fc*fs_i+r_phase)
                         sig_sum      = cmplx(0.0_sp,0.0_sp)
                         c_amp        = cmplx(0.0_sp,0.0_sp)
                         do j__ = Ns,Ne 
                            t_j_re           = random_Weibull_clamped(rd_params.random_weibull_a_r)
                            c_amp            = A0/C314159265358979323846264338328
                            t_j_im           = random_Weibull_clamped(rd_params.random_weibull_a_i)
                            c_rand_j         = cmplx(t_j_re,t_j_im)
                            r_k              = real(j__,kind=sp)
                            fs_j             = r_k*Tp_j 
                            r_seq            = code_seq(j__)
                            arg              = t0*(fs_i-fs_j)+h_spread
                            t1               = asin(sin(arg))
                            t2               = acos(cos(arg))
                            t3               = (t1+t2)-v_spread*0.5_sp+v_spread 
                            c_amp            = c_amp+c_rand_j*c_scale 
                            yc_t             = c_amp*t3 
                            ctmp             = yc_t*r_seq
                            sig_sum          = sig_sum+ctmp
                            samples(j__,i__) = ctmp
                         end do 
                         complex_env(i__)      = sig_sum
                         signal(i__)           = sig_sum*carrier(i__)
                         carrier_i(i__)        = real(carrier(i__))
                         carrier_q(i__)        = aimag(carrier(i__))
                         signal_i(i__)         = real(signal(i__))
                         signal_q(i__)         = aimag(signal(i__))
                         complex_env_i(i__)    = real(complex_env(i__))
                         complex_env_q(i__)    = aimag(complex_env(i__))
                     end do 
                  else if (which_distro .eq. "random_beta_clamped") then 
                          c_scale = cmplx(scale_r,scale_i)
                          
!dir$ assume_aligned code_seq:64
!dir$ assume_aligned carrier:64
!dir$ assume_aligned complex_env:64
!dir$ assume_aligned signal:64
!dir$ assume_aligned samples:64
!dir$ assume_aligned carrier_i:64
!dir$ assume_aligned carrier_q:64
!dir$ assume_aligned signal_i:64
!dir$ assume_aligned signal_q:64
!dir$ assume_aligned complex_env_i:64
!dir$ assume_aligned complex_env_q:64
                     do i__ = Ts,Te 
                         t_i_re       = random_beta_clamped(rd_params.random_beta_aa_r, &
                                                            rd_params.random_beta_aa_i, &
                                                            rd_params.random_beta_first)
                         t_i_im       = random_beta_clamped(rd_params.random_beta_bb_r, &
                                                            rd_params.random_beta_bb_i, &
                                                            rd_params.random_beta_first)
                         c_rand_i     = cmplx(t_i_re,t_i_im)
                         Ac           = Ac+c_rand_i*c_scale 
                         r_t          = real(i__,kind=sp)
                         fs_i         = r_t*Tp_i 
                         carrier(i__) = Ac*exp(j*C6283185307179586476925286766559*fc*fs_i+r_phase)
                         sig_sum      = cmplx(0.0_sp,0.0_sp)
                         c_amp        = cmplx(0.0_sp,0.0_sp)
                         do j__ = Ns,Ne 
                            t_j_re           = random_beta_clamped(rd_params.random_beta_aa_r, &
                                                                   rd_params.random_beta_aa_i, &
                                                                   rd_params.random_beta_first)
                            c_amp            = A0/C314159265358979323846264338328
                            t_j_im           = random_beta_clamped(rd_params.random_beta_bb_r, &
                                                                   rd_params.random_beta_bb_i, &
                                                                   rd_params.random_beta_first)
                            c_rand_j         = cmplx(t_j_re,t_j_im)
                            r_k              = real(j__,kind=sp)
                            fs_j             = r_k*Tp_j
                            r_seq            = code_seq(j__)
                            arg              = t0*(fs_i-fs_j)+h_spread
                            t1               = asin(sin(arg))
                            t2               = acos(cos(arg))
                            t3               = (t1+t2)-v_spread*0.5_sp+v_spread 
                            c_amp            = c_amp+c_rand_j*c_scale 
                            yc_t             = c_amp*t3 
                            ctmp             = yc_t*r_seq
                            sig_sum          = sig_sum+ctmp
                            samples(j__,i__) = ctmp
                         end do 
                         complex_env(i__)      = sig_sum
                         signal(i__)           = sig_sum*carrier(i__)
                         carrier_i(i__)        = real(carrier(i__))
                         carrier_q(i__)        = aimag(carrier(i__))
                         signal_i(i__)         = real(signal(i__))
                         signal_q(i__)         = aimag(signal(i__))
                         complex_env_i(i__)    = real(complex_env(i__))
                         complex_env_q(i__)    = aimag(complex_env(i__))
                     end do 
                  end if 
               case ("Sine_squared")
                     fs_i     = 0.0_sp 
                     fs_j     = 0.0_sp 
                     Tp_i     = 1.0_sp/real(2.0_sp*(Te-Ts),kind=sp)
                     Tp_j     = 1.0_sp/real(2.0_sp*(Ne-Ns),kind=sp)
                     r_phase  = 0.0_sp 
                     call random_seed()
                     call random_number(rand_r)
                     trunc_r = floor(2.0_sp*rand_r)
                     if(trunc_r .eq. 1)  then 
                        r_phase = rand_r*C314159265358979323846264338328
                     else if(trunc_r .eq. 0) then 
                        r_phase = -C314159265358979323846264338328*rand_r
                     end if 
                     print*,"random_phase=", r_phase 
                     if(which_distro .eq. "random_normal") then 
                        c_scale = cmplx(scale_r,scale_i)
!dir$ assume_aligned code_seq:64
!dir$ assume_aligned carrier:64
!dir$ assume_aligned complex_env:64
!dir$ assume_aligned signal:64
!dir$ assume_aligned samples:64
!dir$ assume_aligned carrier_i:64
!dir$ assume_aligned carrier_q:64
!dir$ assume_aligned signal_i:64
!dir$ assume_aligned signal_q:64
!dir$ assume_aligned complex_env_i:64
!dir$ assume_aligned complex_env_q:64
                        do i__ = Ts,Te 
                           t_i_re       = random_normal_clamped()
                           t_i_im       = random_normal_clamped()
                           c_rand_i     = cmplx(t_i_re,t_i_im)
                           Ac           = Ac+c_rand_i*c_scale
                           r_t          = real(i__,kind=sp)
                           fs_i         = r_t*Tp_i 
                           carrier(i__) = Ac*exp(j*C6283185307179586476925286766559*fc*fs_i+r_phase)
                           sig_sum      = cmplx(0.0_sp,0.0_sp)
                           A0_cpy       = A0
                           do j__ = Ns,Ne 
                              t_j_re           = random_normal_clamped()
                              t_j_im           = random_normal_clamped()
                              c_rand_j         = cmplx(t_j_re,t_j_im)
                              r_k              = real(j__,kind=sp)
                              fs_j             = r_k*Tp_j
                              r_seq            = code_seq(j__)
                              arg              = C6283185307179586476925286766559*fs_i*fs_j
                              A0_cpy           = A0_cpy+c_rand_j*c_scale 
                              t0               = 1.0_sp-cos(arg)
                              yc_t             = A0_cpy*0.5_sp*t0 
                              ctmp             = yc_t*r_seq
                              sig_sum          = sig_sum+ctmp
                              samples(j__,i__) = ctmp
                           end do 
                           complex_env(i__)      = sig_sum
                           signal(i__)           = sig_sum*carrier(i__)
                           carrier_i(i__)        = real(carrier(i__))
                           carrier_q(i__)        = aimag(carrier(i__))
                           signal_i(i__)         = real(signal(i__))
                           signal_q(i__)         = aimag(signal(i__))
                           complex_env_i(i__)    = real(complex_env(i__))
                           complex_env_q(i__)    = aimag(complex_env(i__))
                       end do  
                     else if (which_distro .eq. "rand_exponential_clamped") then 
                        c_scale = cmplx(scale_r,scale_i)
!dir$ assume_aligned code_seq:64
!dir$ assume_aligned carrier:64
!dir$ assume_aligned complex_env:64
!dir$ assume_aligned signal:64
!dir$ assume_aligned samples:64
!dir$ assume_aligned carrier_i:64
!dir$ assume_aligned carrier_q:64
!dir$ assume_aligned signal_i:64
!dir$ assume_aligned signal_q:64
!dir$ assume_aligned complex_env_i:64
!dir$ assume_aligned complex_env_q:64
                        do i__ = Ts,Te 
                           t_i_re       = random_exponential_clamped()
                           t_i_im       = random_exponential_clamped()
                           c_rand_i     = cmplx(t_i_re,t_i_im)
                           Ac           = Ac+c_rand_i*c_scale
                           r_t          = real(i__,kind=sp)
                           fs_i         = r_t*Tp_i 
                           carrier(i__) = Ac*exp(j*C6283185307179586476925286766559*fc*fs_i+r_phase)
                           sig_sum      = cmplx(0.0_sp,0.0_sp)
                           A0_cpy       = A0 
                           do j__ = Ns,Ne 
                              t_j_re           = random_exponential_clamped()
                              t_j_im           = random_exponential_clamped()
                              c_rand_j         = cmplx(t_j_re,t_j_im)
                              r_k              = real(j__,kind=sp)
                              fs_j             = r_k*Tp_j 
                              r_seq            = code_seq(j__)
                              arg              = C6283185307179586476925286766559*fs_i-fs_j 
                              A0_cpy           = A0_cpy+c_rand_j*c_scale 
                              t0               = 1.0_sp-cos(arg)
                              yc_t             = A0_cpy*0.5_sp*t0 
                              ctmp             = yc_t*r_seq
                              sig_sum          = sig_sum+ctmp
                              samples(j__,i__) = ctmp
                           end do 
                           complex_env(i__)      = sig_sum
                           signal(i__)           = sig_sum*carrier(i__)
                           carrier_i(i__)        = real(carrier(i__))
                           carrier_q(i__)        = aimag(carrier(i__))
                           signal_i(i__)         = real(signal(i__))
                           signal_q(i__)         = aimag(signal(i__))
                           complex_env_i(i__)    = real(complex_env(i__))
                           complex_env_q(i__)    = aimag(complex_env(i__))
                       end do  
                     else if (which_distro .eq. "rand_weibull_clamped") then 
                        c_scale = cmplx(scale_r,scale_i)
!dir$ assume_aligned code_seq:64
!dir$ assume_aligned carrier:64
!dir$ assume_aligned complex_env:64
!dir$ assume_aligned signal:64
!dir$ assume_aligned samples:64
!dir$ assume_aligned carrier_i:64
!dir$ assume_aligned carrier_q:64
!dir$ assume_aligned signal_i:64
!dir$ assume_aligned signal_q:64
!dir$ assume_aligned complex_env_i:64
!dir$ assume_aligned complex_env_q:64
                        do i__ = Ts,Te 
                           t_i_re       = random_Weibull_clamped(rd_params.random_weibull_a_r)
                           t_i_im       = random_Weibull_clamped(rd_params.random_weibull_a_i)
                           c_rand_i     = cmplx(t_i_re,t_i_im)
                           Ac           = Ac+c_rand_i*c_scale
                           r_t          = real(i__,kind=sp)
                           fs_i         = r_t*Tp_i 
                           carrier(i__) = Ac*exp(j*C6283185307179586476925286766559*fc*fs_i+r_phase)
                           sig_sum      = cmplx(0.0_sp,0.0_sp)
                           A0_cpy       = A0 
                           do j__ = Ns,Ne 
                              t_j_re           = random_Weibull_clamped(rd_params.random_weibull_a_r)
                              t_j_im           = random_Weibull_clamped(rd_params.random_weibull_a_i)
                              c_rand_j         = cmplx(t_j_re,t_j_im)
                              r_k              = real(j__,kind=sp)
                              fs_j             = r_k*Tp_j 
                              r_seq            = code_seq(j__)
                              arg              = C6283185307179586476925286766559*fs_i-fs_j 
                              A0_cpy           = A0_cpy+c_rand_j*c_scale 
                              t0               = 1.0_sp-cos(arg)
                              yc_t             = A0_cpy*0.5_sp*t0 
                              ctmp             = yc_t*r_seq
                              sig_sum          = sig_sum+ctmp
                              samples(j__,i__) = ctmp
                           end do 
                           complex_env(i__)      = sig_sum
                           signal(i__)           = sig_sum*carrier(i__)
                           carrier_i(i__)        = real(carrier(i__))
                           carrier_q(i__)        = aimag(carrier(i__))
                           signal_i(i__)         = real(signal(i__))
                           signal_q(i__)         = aimag(signal(i__))
                           complex_env_i(i__)    = real(complex_env(i__))
                           complex_env_q(i__)    = aimag(complex_env(i__))
                       end do  
                     else if (which_distro .eq. "random_beta_clamped") then 
                         c_scale = cmplx(scale_r,scale_i)
!dir$ assume_aligned code_seq:64
!dir$ assume_aligned carrier:64
!dir$ assume_aligned complex_env:64
!dir$ assume_aligned signal:64
!dir$ assume_aligned samples:64
!dir$ assume_aligned carrier_i:64
!dir$ assume_aligned carrier_q:64
!dir$ assume_aligned signal_i:64
!dir$ assume_aligned signal_q:64
!dir$ assume_aligned complex_env_i:64
!dir$ assume_aligned complex_env_q:64
                        do i__ = Ts,Te 
                           t_i_re       = random_beta_clamped(rd_params.random_beta_aa_r, &
                                                              rd_params.random_beta_bb_r, &
                                                              rd_params.random_beta_first)
                           t_i_im       = random_beta_clamped(rd_params.random_beta_aa_i, &
                                                              rd_params.random_beta_bb_i, &
                                                              rd_params.random_beta_first)
                           c_rand_i     = cmplx(t_i_re,t_i_im)
                           Ac           = Ac+c_rand_i*c_scale
                           r_t          = real(i__,kind=sp)
                           fs_i         = r_t*Tp_i 
                           carrier(i__) = Ac*exp(j*C6283185307179586476925286766559*fc*fs_i+r_phase)
                           sig_sum      = cmplx(0.0_sp,0.0_sp)
                           A0_cpy       = A0 
                           do j__ = Ns,Ne 
                              t_j_re           = random_beta_clamped(rd_params.random_beta_aa_r, &
                                                                     rd_params.random_beta_bb_r, &
                                                                     rd_params.random_beta_first)
                              t_j_im           = random_beta_clamped(rd_params.random_beta_aa_i, &
                                                              rd_params.random_beta_bb_i, &
                                                              rd_params.random_beta_first)
                              c_rand_j         = cmplx(t_j_re,t_j_im)
                              r_k              = real(j__,kind=sp)
                              fs_j             = r_k*Tp_j 
                              r_seq            = code_seq(j__)
                              arg              = C6283185307179586476925286766559*fs_i-fs_j 
                              A0_cpy           = A0_cpy+c_rand_j*c_scale 
                              t0               = 1.0_sp-cos(arg)
                              yc_t             = A0_cpy*0.5_sp*t0 
                              ctmp             = yc_t*r_seq
                              sig_sum          = sig_sum+ctmp
                              samples(j__,i__) = ctmp
                           end do 
                           complex_env(i__)      = sig_sum
                           signal(i__)           = sig_sum*carrier(i__)
                           carrier_i(i__)        = real(carrier(i__))
                           carrier_q(i__)        = aimag(carrier(i__))
                           signal_i(i__)         = real(signal(i__))
                           signal_q(i__)         = aimag(signal(i__))
                           complex_env_i(i__)    = real(complex_env(i__))
                           complex_env_q(i__)    = aimag(complex_env(i__))
                       end do  
                     end if
               case ("Sine")
                     fs_i     = 0.0_sp 
                     fs_j     = 0.0_sp 
                     Tp_i     = 1.0_sp/real(2.0_sp*(Te-Ts),kind=sp)
                     Tp_j     = 1.0_sp/real(2.0_sp*(Ne-Ns),kind=sp)
                     r_phase  = 0.0_sp 
                     call random_seed()
                     call random_number(rand_r)
                     trunc_r = floor(2.0_sp*rand_r)
                     if(trunc_r .eq. 1)  then 
                        r_phase = rand_r*C314159265358979323846264338328
                     else if(trunc_r .eq. 0) then 
                        r_phase = -C314159265358979323846264338328*rand_r
                     end if 
                     print*,"random_phase=", r_phase 
                     if(which_distro .eq. "random_normal") then 
                        c_scale = cmplx(scale_r,scale_i)
!dir$ assume_aligned code_seq:64
!dir$ assume_aligned carrier:64
!dir$ assume_aligned complex_env:64
!dir$ assume_aligned signal:64
!dir$ assume_aligned samples:64
!dir$ assume_aligned carrier_i:64
!dir$ assume_aligned carrier_q:64
!dir$ assume_aligned signal_i:64
!dir$ assume_aligned signal_q:64
!dir$ assume_aligned complex_env_i:64
!dir$ assume_aligned complex_env_q:64
                        do i__ = Ts,Te 
                           t_i_re       = random_normal_clamped()
                           t_i_im       = random_normal_clamped()
                           c_rand_i     = cmplx(t_i_re,t_i_im)
                           Ac           = Ac+c_rand_i*c_scale
                           r_t          = real(i__,kind=sp)
                           fs_i         = r_t*Tp_i 
                           carrier(i__) = Ac*exp(j*C6283185307179586476925286766559*fc*fs_i+r_phase)
                           sig_sum      = cmplx(0.0_sp,0.0_sp)
                           A0_cpy       = A0 
                           do j__ = Ns,Ne 
                              t_j_re           = random_normal_clamped()
                              t_j_im           = random_normal_clamped()
                              c_rand_j         = cmplx(t_j_re,t_j_im)
                              r_k              = real(j__,kind=sp)
                              fs_j             = r_k*Tp_j 
                              r_seq            = code_seq(j__)
                              arg              = C314159265358979323846264338328*fs_i-fs_j 
                              A0_cpy           = A0_cpy+c_rand_j*c_scale 
                              t0               = sin(arg)
                              yc_t             = A0_cpy*t0 
                              ctmp             = yc_t*r_seq
                              sig_sum          = sig_sum+ctmp
                              samples(j__,i__) = ctmp
                           end do 
                           complex_env(i__)      = sig_sum
                           signal(i__)           = sig_sum*carrier(i__)
                           carrier_i(i__)        = real(carrier(i__))
                           carrier_q(i__)        = aimag(carrier(i__))
                           signal_i(i__)         = real(signal(i__))
                           signal_q(i__)         = aimag(signal(i__))
                           complex_env_i(i__)    = real(complex_env(i__))
                           complex_env_q(i__)    = aimag(complex_env(i__))
                        end do 
                     else if (which_distro .eq. "rand_exponential_clamped") then 
                              c_scale = cmplx(scale_r,scale_i)
!dir$ assume_aligned code_seq:64
!dir$ assume_aligned carrier:64
!dir$ assume_aligned complex_env:64
!dir$ assume_aligned signal:64
!dir$ assume_aligned samples:64
!dir$ assume_aligned carrier_i:64
!dir$ assume_aligned carrier_q:64
!dir$ assume_aligned signal_i:64
!dir$ assume_aligned signal_q:64
!dir$ assume_aligned complex_env_i:64
!dir$ assume_aligned complex_env_q:64
                              do i__ = Ts,Te 
                                 t_i_re       = random_exponential_clamped()
                                 t_i_im       = random_exponential_clamped()
                                 c_rand_i     = cmplx(t_i_re,t_i_im)
                                 Ac           = Ac+c_rand_i*c_scale
                                 r_t          = real(i__,kind=sp)
                                 fs_i         = r_t*Tp_i 
                                 carrier(i__) = Ac*exp(j*C6283185307179586476925286766559*fc*fs_i+r_phase)
                                 sig_sum      = cmplx(0.0_sp,0.0_sp)
                                 A0_cpy       = A0 
                                 do j__ = Ns,Ne 
                                    t_j_re           = random_exponential_clamped()
                                    t_j_im           = random_exponential_clamped()
                                    c_rand_j         = cmplx(t_j_re,t_j_im)
                                    r_k              = real(j__,kind=sp)
                                    fs_j             = r_k*Tp_j 
                                    r_seq            = code_seq(j__)
                                    arg              = C314159265358979323846264338328*fs_i-fs_j 
                                    A0_cpy           = A0_cpy+c_rand_j*c_scale 
                                    t0               = sin(arg)
                                    yc_t             = A0_cpy*t0 
                                    ctmp             = yc_t*r_seq
                                    sig_sum          = sig_sum+ctmp
                                    samples(j__,i__) = ctmp
                                 end do 
                                 complex_env(i__)      = sig_sum
                                 signal(i__)           = sig_sum*carrier(i__)
                                 carrier_i(i__)        = real(carrier(i__))
                                 carrier_q(i__)        = aimag(carrier(i__))
                                 signal_i(i__)         = real(signal(i__))
                                 signal_q(i__)         = aimag(signal(i__))
                                 complex_env_i(i__)    = real(complex_env(i__))
                                 complex_env_q(i__)    = aimag(complex_env(i__))
                           end do     
                     else if (which_distro .eq. "rand_weibull_clamped") then 
                              c_scale = cmplx(scale_r,scale_i)
!dir$ assume_aligned code_seq:64
!dir$ assume_aligned carrier:64
!dir$ assume_aligned complex_env:64
!dir$ assume_aligned signal:64
!dir$ assume_aligned samples:64
!dir$ assume_aligned carrier_i:64
!dir$ assume_aligned carrier_q:64
!dir$ assume_aligned signal_i:64
!dir$ assume_aligned signal_q:64
!dir$ assume_aligned complex_env_i:64
!dir$ assume_aligned complex_env_q:64
                              do i__ = Ts,Te 
                                 t_i_re       = random_Weibull_clamped(rd_params.random_weibull_a_r)
                                 t_i_im       = random_Weibull_clamped(rd_params.random_weibull_a_i)
                                 c_rand_i     = cmplx(t_i_re,t_i_im)
                                 Ac           = Ac+c_rand_i*c_scale
                                 r_t          = real(i__,kind=sp)
                                 fs_i         = r_t*Tp_i 
                                 carrier(i__) = Ac*exp(j*C6283185307179586476925286766559*fc*fs_i+r_phase)
                                 sig_sum      = cmplx(0.0_sp,0.0_sp)
                                 A0_cpy       = A0 
                                 do j__ = Ns,Ne 
                                    t_j_re           = random_Weibull_clamped(rd_params.random_weibull_a_r)
                                    t_j_im           = random_Weibull_clamped(rd_params.random_weibull_a_i)
                                    c_rand_j         = cmplx(t_j_re,t_j_im)
                                    r_k              = real(j__,kind=sp)
                                    fs_j             = r_k*Tp_j 
                                    r_seq            = code_seq(j__)
                                    arg              = C314159265358979323846264338328*fs_i-fs_j 
                                    A0_cpy           = A0_cpy+c_rand_j*c_scale 
                                    t0               = sin(arg)
                                    yc_t             = A0_cpy*t0 
                                    ctmp             = yc_t*r_seq
                                    sig_sum          = sig_sum+ctmp
                                    samples(j__,i__) = ctmp
                                 end do 
                                 complex_env(i__)      = sig_sum
                                 signal(i__)           = sig_sum*carrier(i__)
                                 carrier_i(i__)        = real(carrier(i__))
                                 carrier_q(i__)        = aimag(carrier(i__))
                                 signal_i(i__)         = real(signal(i__))
                                 signal_q(i__)         = aimag(signal(i__))
                                 complex_env_i(i__)    = real(complex_env(i__))
                                 complex_env_q(i__)    = aimag(complex_env(i__))
                           end do     
                     else if (which_distro .eq. "rand_beta_clamped") then 
                              c_scale = cmplx(scale_r,scale_i)
!dir$ assume_aligned code_seq:64
!dir$ assume_aligned carrier:64
!dir$ assume_aligned complex_env:64
!dir$ assume_aligned signal:64
!dir$ assume_aligned samples:64
!dir$ assume_aligned carrier_i:64
!dir$ assume_aligned carrier_q:64
!dir$ assume_aligned signal_i:64
!dir$ assume_aligned signal_q:64
!dir$ assume_aligned complex_env_i:64
!dir$ assume_aligned complex_env_q:64
                              do i__ = Ts,Te 
                                 t_i_re       = random_beta_clamped(rd_params.random_beta_aa_r,  &
                                                                    rd_params.random_beta_bb_r,  &
                                                                    rd_params.random_beta_first)
                                 t_i_im       = random_beta_clamped(rd_params.random_beta_aa_i,  &
                                                                    rd_params.random_beta_bb_i,  &
                                                                    rd_params.random_beta_first)
                                 c_rand_i     = cmplx(t_i_re,t_i_im)
                                 Ac           = Ac+c_rand_i*c_scale
                                 r_t          = real(i__,kind=sp)
                                 fs_i         = r_t*Tp_i 
                                 carrier(i__) = Ac*exp(j*C6283185307179586476925286766559*fc*fs_i+r_phase)
                                 sig_sum      = cmplx(0.0_sp,0.0_sp)
                                 A0_cpy       = A0 
                                 do j__ = Ns,Ne 
                                    t_j_re           = random_beta_clamped(rd_params.random_beta_aa_r,  &
                                                                           rd_params.random_beta_bb_r,  &
                                                                           rd_params.random_beta_first)
                                    t_j_im           = random_beta_clamped(rd_params.random_beta_aa_i,  &
                                                                           rd_params.random_beta_bb_i,  &
                                                                           rd_params.random_beta_first)
                                    c_rand_j         = cmplx(t_j_re,t_j_im)
                                    r_k              = real(j__,kind=sp)
                                    fs_j             = r_k*Tp_j 
                                    r_seq            = code_seq(j__)
                                    arg              = C314159265358979323846264338328*fs_i-fs_j 
                                    A0_cpy           = A0_cpy+c_rand_j*c_scale 
                                    t0               = sin(arg)
                                    yc_t             = A0_cpy*t0 
                                    ctmp             = yc_t*r_seq
                                    sig_sum          = sig_sum+ctmp
                                    samples(j__,i__) = ctmp
                                 end do 
                                 complex_env(i__)      = sig_sum
                                 signal(i__)           = sig_sum*carrier(i__)
                                 carrier_i(i__)        = real(carrier(i__))
                                 carrier_q(i__)        = aimag(carrier(i__))
                                 signal_i(i__)         = real(signal(i__))
                                 signal_q(i__)         = aimag(signal(i__))
                                 complex_env_i(i__)    = real(complex_env(i__))
                                 complex_env_q(i__)    = aimag(complex_env(i__))
                           end do     
                     end if 
               case default 
                   print*, "Invalid switch variable: ", envelope_type 
                   ioerr = -1
                   return 
            end select 
            ioerr = 0    
#if (AM_BROADBAND_SIGNAL_EXEC_TRACE) == 1
          print*, "[EXECUTION-TRACE: END]"
          print*, "TSC-End:", rdtsc_wrap()
#endif              
     end subroutine modulate_add_noise_AM_broadband_signal





end module mod_AM_broadband_signal 