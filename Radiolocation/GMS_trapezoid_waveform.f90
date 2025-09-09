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

module mod_trapezoid_waveform

!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name: mod_trapezoid_waveform
 !                         
 !          
 !          Purpose:
 !                       TRapezoid waveform generating functions
 !                        
 !          History:
 !                        Date: 09-09-2025
 !                        Time: 09:22AM GMT+2
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
 !                     Макаров С.Б., Цикин И.А, Передача дискретных сообщений по радиоканалам с ограниченной полосой пропускания
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
     integer(kind=i4),  parameter :: TRAPEZOID_WAVEFORM_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: TRAPEZOID_WAVEFORM_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: TRAPEZOID_WAVEFORM_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: TRAPEZOID_WAVEFORM_FULLVER =   &
            1000*TRAPEZOID_WAVEFORM_MAJOR+100*TRAPEZOID_WAVEFORM_MINOR+10*TRAPEZOID_WAVEFORM_MICRO
     ! Module creation date
     character(*),        parameter :: TRAPEZOID_WAVEFORM_CREATE_DATE = "09-09-2025 09:22AM +00200 (TUE 09 SEP 2025 GMT+2)"
     ! Module build date
     character(*),        parameter :: TRAPEZOID_WAVEFORM_BUILD_DATE  = __DATE__ 

     character(*),        parameter :: TRAPEZOID_WAVEFORM_BUILD_TIME  =  __TIME__
     ! Module author info
     character(*),        parameter :: TRAPEZOID_WAVEFORM_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: TRAPEZOID_WAVEFORM_SYNOPSIS    = "Trapezoid waveform generating functions."

     character(*),        parameter :: TRAPEZOID_WAVEFORM_REFERENCE   = "Макаров С.Б., Цикин И.А, Передача дискретных сообщений по радиоканалам с ограниченной полосой пропускания"

!===========================================================================================================================================================================!

!#define TRAPEZOID_WAVEFORM_SAFE_ALLOC(mem,size) if(allocated(mem)) deallocate(mem); allocate(mem size)
!#define TRAPEZOID_WAVEFORM_SAFE_DEALLOC(mem)    if(allocated(mem)) deallocate(mem)

      ! Static arrays dimensioning
      integer(kind=i4), parameter, private :: MAX_WAVES  = 256
      integer(kind=i4), parameter, private :: MAX_PARAMS = 256

      ! Helper derived type for holding random-distribution parameters
     type, public :: tw_pdf_params_t 
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

     end type tw_pdf_params_t 

      type, public :: trapezoid_waveform_t 
            
            integer(kind=i4)                         :: n_samples 
            integer(kind=i4)                         :: n_waves 
            integer(kind=i4)                         :: n_param_a 
            integer(kind=i4)                         :: n_param_l
            integer(kind=i4)                         :: n_param_c 
            integer(kind=i4)                         :: n_param_m 
            real(kind=sp), dimension(MAX_WAVES)      :: trapezoid_waves 
            real(kind=sp), dimension(MAX_PARAMS)     :: params_a 
            real(kind=sp), dimension(MAX_PARAMS)     :: params_l 
            real(kind=sp), dimension(MAX_PARAMS)     :: params_c 
            real(kind=sp), dimension(MAX_PARAMS)     :: params_m 
            type(tw_pdf_params_t)                    :: pdf_params 
            real(kind=sp), dimension(:), allocatable :: samples
#if defined(__INTEL_COMPILER) || defined(__ICC)
            !dir$ attribute align : 64 :: samples   
#endif          
      end type trapezoid_waveform_t




end module mod_trapezoid_waveform