
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

module mod_AM_radar_signal

!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name: mod_AM_radar_waveform
 !                         
 !          
 !          Purpose:
 !                       Amplitude-modulated wideband Radar signal  
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
     integer(kind=i4),  parameter :: AM_RADAR_SIGNAL_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: AM_RADAR_SIGNAL_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: AM_RADAR_SIGNAL_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: AM_RADAR_SIGNAL_FULLVER =   &
            1000*AM_RADAR_SIGNAL_MAJOR+100*AM_RADAR_SIGNAL_MINOR+10*AM_RADAR_SIGNAL_MICRO
     ! Module creation date
     character(*),        parameter :: AM_RADAR_SIGNAL_CREATE_DATE = "05-08-2025 10:38AM +00200 (TUE 05 AUG 2025 GMT+2)"
     ! Module build date
     character(*),        parameter :: AM_RADAR_SIGNAL_BUILD_DATE  = __DATE__ 

     character(*),        parameter :: AM_RADAR_SIGNAL_BUILD_TIME  =  __TIME__
     ! Module author info
     character(*),        parameter :: AM_RADAR_SIGNAL_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: AM_RADAR_SIGNAL_SYNOPSIS    = "Amplitude-modulated wideband Radar signal"

     character(*),        parameter :: AM_RADAR_SIGNAL_REFERENCE   = "Макаров С.Б., Цикин И.А, Передача дискретных сообщений по радиоканалам с ограниченной полосой пропускания"

     type, public :: AM_radar_signal_t 

           character(len=64) :: m_signal_name    ! signal/waveform name plain name
           character(len=32) :: m_envelope_type !trapzeoidal,sine, sine_squared
           character(len=4)  :: m_code_type     ! either [0,1], or [-1,1]
           integer(kind=i4)  :: m_id            ! This signal ID for pulse train scenario 
           integer(kind=i4)  :: m_interval_1    ! for trapezoidal envelope only: interval 0<=t<=s
           integer(kind=i4)  :: m_interval_2    ! for trapezoidal envelope only: interval s<t<T-s
           integer(kind=i4)  :: m_interval_3    ! for trapezoidal envelope only: interval T-s<=t<=T
           integer(kind=i4)  :: m_num_symbols   ! number of symbols transmitted  = num_samples*T  
                                                                                                     !N-1
           integer(kind=i4)  :: m_num_samples   ! number of accumulated samples per symbol i.e. y(t)=Sum a(t-kT)*dr
                                                                                                     !k=0
           integer(kind=i4)  :: m_T             ! symbol length (period) 
           logical(kind=i4)  :: m_sym_dep       ! previous-next symbol dependency (page: 50, formula: 2.7)  
           logical(kind=i4)  :: m_split_carrier  ! split complex carrier into real part only  
           logical(kind=i4)  :: m_split_envelope ! split complex envelope into real part  
           complex(kind=sp)  :: m_A0            ! complex amplitude value                          
           real(kind=sp)     :: m_invT          ! inverse of symbol length
           real(kind=sp)     :: m_fc            ! carrier frequency 
           real(kind=sp)     :: m_fs            ! sampling frequency 
           real(kind=sp)     :: m_sig_width     ! signal width 
           real(kind=sp),    dimension(:), allocatable :: m_code_seq ![0,1 or -1,1] 
           complex(kind=sp), dimension(:), allocatable :: m_carrier 
           complex(kind=sp), dimension(:), allocatable :: m_complex_env
           complex(kind=sp), dimension(:), allocatable :: m_signal
           real(kind=sp),    dimension(:), allocatable :: m_carrier_re 
           real(kind=sp),    dimension(:), allocatable :: m_carrier_im 
           real(kind=sp),    dimension(:), allocatable :: m_complex_env_re 
           real(kind=sp),    dimension(:), allocatable :: m_complex_env_im 
           real(kind=sp),    dimension(:), allocatable :: m_signal_re 
           real(kind=sp),    dimension(:), allocatable :: m_signal_im

     end type AM_radar_signal_t




end module mod_AM_radar_signal 