
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

    use mod_kinds,     only : i4,sp
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

#if !defined(AM_BROADBAND_SIGNAL_SPLIT_COMPLEX_DATA)
#define AM_BROADBAND_SIGNAL_SPLIT_COMPLEX_DATA 1 
#endif 

#if !defined(AM_BROADBAND_SIGNAL_USE_MKL_FFT)
#define AM_BROADBAND_SIGNAL_USE_MKL_FFT 0
#endif 

     type, public :: AM_broadband_signal_t 

           character(len=64) :: m_signal_name    ! signal/waveform name plain name
           character(len=32) :: m_envelope_type !trapzeoidal,sine, sine_squared
           character(len=32) :: m_distro_omega   ! statistical distribution type for ambiguity simulation (omega)
           character(len=32) :: m_distro_theta   ! statistical distribution type for ambiguity simulation (theta)
           character(len=4)  :: m_code_type     ! either [0,1], or [-1,1]
           integer(kind=i4)  :: m_id            ! This signal ID for pulse train scenario 
           integer(kind=i4)  :: m_interval_1    ! for trapezoidal envelope only: interval 0<=t<=s
           integer(kind=i4)  :: m_interval_2    ! for trapezoidal envelope only: interval s<t<T-s
           integer(kind=i4)  :: m_interval_3    ! for trapezoidal envelope only: interval T-s<=t<=T
           integer(kind=i4)  :: m_num_symbols   ! number of symbols transmitted  = num_samples*T  
           integer(kind=i4)  :: m_N             ! number of narrowband signals
           integer(kind=i4)  :: m_T             ! symbol length (period) 
           integer(kind=i4)  :: m_nfreqs        ! number of frequencies (Spectral analysis)
                                                                                                     !N-1
           integer(kind=i4)  :: m_num_samples   ! number of accumulated samples per symbol i.e. y(t)=Sum a(t-kT)*dr
                                                                                                     !k=0
           
           integer(kind=i4)  :: m_nomega         ! number of doppler frequency shifts 
           integer(kind=i4)  :: m_ntheta         ! number of time delays (for return signal)
           logical(kind=i4)  :: m_sym_dep       ! previous-next symbol dependency (page: 50, formula: 2.7)  
           logical(kind=i4)  :: m_ft_process     ! Fourier-Transform processing: true for MKL, false for Quadpack numerical integration
           complex(kind=sp)  :: m_A0            ! complex amplitude value                          
           real(kind=sp)     :: m_invT          ! inverse of symbol length
           real(kind=sp)     :: m_fc            ! carrier frequency 
           real(kind=sp)     :: m_fs            ! sampling frequency 
           real(kind=sp)     :: m_sig_width     ! signal width 
           real(kind=sp)     :: m_sig_energy    ! signal energy 
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
#if (AM_BROADBAND_SIGNAL_SPLIT_COMPLEX_DATA) == 1
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
#endif 
           
     end type AM_broadband_signal_t


     contains 


     subroutine init_AM_broadband_signal(AM_signal,sig_name,envelope_type,distro_omega,distro_theta,          &
                                         code_type,id,interval_1,interval_2,interval_3, num_symbols,          &
                                         N,T,nfreqs,nomega,ntheta,sym_dep,ft_process,A0,fc)              
                                        
          implicit none 
          character(*), parameter :: sub_name = "init_AM_broadband_signal"
          type(AM_broadband_signal_t),        intent(inout)        :: AM_signal 
          character(len=64),                  intent(in)           :: sig_name 
          character(len=32),                  intent(in)           :: envelope_type 
          character(len=32),                  intent(in)           :: distro_omega 
          character(len=32),                  intent(in)           :: distro_theta 
          character(len=4),                   intent(in)           :: code_type 
          integer(kind=i4),                   intent(in)           :: id 
          integer(kind=i4),                   intent(in), optional :: interval_1
          integer(kind=i4),                   intent(in), optional :: interval_2 
          integer(kind=i4),                   intent(in), optional :: interval_3 
          integer(kind=i4),                   intent(in)           :: num_symbols 
          integer(kind=i4),                   intent(in)           :: num_samples 
          integer(kind=i4),                   intent(in)           :: N 
          integer(kind=i4),                   intent(in)           :: T 
          integer(kind=i4),                   intent(in)           :: nfreqs 
          integer(kind=i4),                   intent(in)           :: nomega 
          integer(kind=i4),                   intent(in)           :: ntheta 
          logical(kind=i4)                    intent(in)           :: sym_dep 
          logical(kind=i4)                    intent(in)           :: split_carrier 
          logical(kind=i4)                    intent(in)           :: split_envelope 
          logical(kind=i4)                    intent(in)           :: ft_process 
          complex(kind=sp)                    intent(in)           :: A0 
          real(kind=sp),                      intent(in)           :: fc 
          

          logical(kind=i1), automatic :: trpz_env   

          if(AM_signal.m_creation_state .eq. .true.) return 
          AM_signal.m_signal_name   = sig_name 
          AM_signal.m_envelope_type = envelope_type  
          AM_signal.m_distro_omega  = distro_omega 
          AM_signal.m_distro_theta  = distro_theta 
          AM_signal.m_code_type     = code_type 
          AM_signal.m_id            = id 
          trpz_env                  = AM_signal.envelope_type .eq. "trapezoidal"
          if(trpz_env  .and. present(interval_1)) AM_signal.m_interval_1 = interval_1 
          if(trpz_env  .and. present(interval_2)) AM_signal.m_intercal_2 = interval_2 
          if(trpz_env  .and. present(interval_3)) AM_signal.m_interval_3 = interval_3 
          AM_signal.num_symbols     = num_symbols 
          AM_signal.m_N             = N 
          AM_signal.m_T             = T 
          AM_signal.num_samples     = AM_signal.m_T*AM_signal.m_N 
          AM_signal.m_nomega        = nomega 
          AM_signal.m_ntheta        = ntheta 
          AM_signal.m_sym_dep       = sym_dep 
          AM_signal.m_split_carrier = split_carrier 
          AM_signal.m_split_envelope=split_envelope 
          AM_signal.m_ft_process    = ft_process 
          AM_signal.m_A0            = A0 
          AM_signal.m_fc            = fc 
          AM_signal.m_fs            = 1.0_sp/real(AM_signal_num_samples,kind=sp) 
          AM_signal.m_sig_width     = 0.0_sp 
          AM_signal.m_sig_energy    = 0.0_sp 

          if(allocated(AM_signal.m_code_seq))      deallocate(AM_signal.m_code_seq);   allocate(AM_signal.m_code_seq(AM_signal.m_num_symbols))
          if(allocated(AM_signal.m_carrier))       deallocate(AM_signal.m_carrier);    allocate(AM_signal.m_carrier(AM_signal.m_T))
          if(allocated(AM_signal.m_complex_env))   deallocate(AM_signal.m_complex_env);allocate(AM_signal.m_complex_env(AM_signal.m_T))
          if(allocated(AM_signal.m_signal))        deallocate(AM_signal.m_signal);     allocate(AM_signal.m_signal(AM_signal.m_T))
          if(allocated(AM_signal.m_env_spec))      deallocate(AM_signal.m_env_spec);   allocate(AM_signal.m_env_spec(AM_signal.m_nfreqs))
          if(allocated(AM_signal.m_samples))       deallocate(AM_signal.m_samples);    allocate(AM_signal.m_samples(AM_signal.m_T,AM_signal.m_N))
          if(allocated(AM_signal.m_env_correl))    deallocate(AM_signal.m_env_correl); allocate(AM_signal.m_env_correl(AM_signal.m_nomega,AM_signal.m_ntheta))
          if(allocated(AM_signal.m_ambiguity))     deallocate(AM_signal.m_ambiguity);  allocate(AM_signal.m_ambiguity(AM_signal.m_nomega,AM_signal.m_ntheta))
          if(allocated(AM_signal.m_abs_ambiguity)) deallocate(AM_signal.m_carrier);    allocate(AM_signal.m_abs_ambiguity(AM_signal.m_nomega,AM_signal.m_ntheta))
#if (AM_BROADBAND_SIGNAL_SPLIT_COMPLEX_DATA) == 1
          if(allocated(AM_signal.m_carrier_i))     deallocate(AM_signal.m_carrier_i);    allocate(AM_signal.m_carrier_i(AM_signal.m_T))
          if(allocated(AM_signal.m_carrier_q))     deallocate(AM_signal.m_carrier_q);    allocate(AM_signal.m_carrier_q(AM_signal.m_T))
          if(allocated(AM_signal.m_complex_env_i)) deallocate(AM_signal.m_complex_env_i);allocate(AM_signal.m_complex_env_i(AM_signal.m_T))
          if(allocated(AM_signal.m_complex_env_r)) deallocate(AM_signal.m_complex_env_r);allocate(AM_signal.m_complex_env_r(AM_signal.m_T))
          if(allocated(AM_signal.m_signal_i))      deallocate(AM_signal.m_signal_i);     allocate(AM_signal.m_signal_i(AM_signal.m_T))
          if(allocated(AM_signal.m_signal_q))      deallocate(AM_signal.m_signal_q);     allocate(AM_signal.m_signal_q(AM_signal.m_T))
          if(allocated(AM_signal.m_env_correl_i))  deallocate(AM_signal.m_env_correl_i); allocate(AM_signal.m_env_correl_i(AM_signal.m_nomega,AM_signal_m_ntheta))
          if(allocated(AM_signal.m_env_correl_r))  deallocate(AM_signal.m_env_correl_r); allocate(AM_signal.m_env_correl_r(AM_signal.m_nomega,AM_signal_m_ntheta))
#endif
          AM_signal.m_creation_state = .true. 
     end subroutine init_AM_broadband_signal




end module mod_AM_broadband_signal 