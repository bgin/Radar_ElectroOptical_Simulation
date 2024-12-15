

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


module waveform_analysis_types








!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name: 
 !                         waveform_analysis_types
 !          
 !          Purpose:
 !                       Waveform analysis derived types describing the fundamentals
 !                       of radar signal waveform analysis.
 !                       Derived types only (computational routines are placed in different module).
 !                        
 !          History:
 !                        Date: 15-12-2024
 !                        Time: 09:55PM GMT+2
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
 !                          Digital Communication over Fading Channels a Unified Approach, Marvin K. Simon, Mohamed Slim-Alouini
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
!==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.

    use mod_kinds,     only : i4,sp,dp
    


    public
    implicit none
    
    
      ! Major version
     integer(kind=i4),  parameter :: WAVEFORM_ANALYSIS_TYPES_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: WAVEFORM_ANALYSIS_TYPES_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: WAVEFORM_ANALYSIS_TYPES_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: WAVEFORM_ANALYSIS_TYPES_FULLVER =   &
            1000*WAVEFORM_ANALYSIS_TYPES_MAJOR+100*WAVEFORM_ANALYSIS_TYPES_MINOR+10*WAVEFORM_ANALYSIS_TYPES_MICRO
     ! Module creation date
     character(*),        parameter :: WAVEFORM_ANALYSIS_TYPES_CREATE_DATE = "15-12-2024 09:57AM +00200 (SUN 15 DEC 2024 GMT+2)"
     ! Module build date
     character(*),        parameter :: WAVEFORM_ANALYSIS_TYPES_BUILD_DATE  = __DATE__ " " __TIME__
     ! Module author info
     character(*),        parameter :: WAVEFORM_ANALYSIS_TYPES_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: WAVEFORM_ANALYSIS_TYPES_SYNOPSIS  = "Radar waveform analysis derived types." 


     complex(kind=sp), parameter :: j = (0.0_sp,1.0_sp)
     complex(kind=sp), parameter :: nj= (0.0_sp,-1.0_sp)

     ! Signal Fourier Transform (continous) single precision.
     type, public :: signal_ft_cont_c4_t
           
           logical(kind=i4), dimension(15)             :: alloc_stat ! arrays allocation status (is_allocated output)      
           character(len=64) :: integrator_name                 ! either direct integration by ?qawf, or by tabulated data integrator
           character(len=12) :: transform_type                  ! direct or inverse transform  
         
           integer(kind=i4)  :: n                               ! number of sample signal values
           real(kind=sp),    dimension(:), allocatable :: a     ! lower limit of integration
           real(kind=sp),    dimension(:), allocatable :: b     ! upper limit of integration
           complex(kind=sp), dimension(:), allocatable :: cexp  ! complex exponential
           complex(kind=sp), dimension(:), allocatable :: ft_dir! direct transform
           complex(kind=sp), dimension(:), allocatable :: ft_inv! inverse transform
           real(kind=sp),    dimension(:), allocatable :: s_re  ! signal's real part
           real(kind=sp),    dimension(:), allocatable :: s_im  ! signal's imaginary part
           real(kind=sp),    dimension(:), allocatable :: freqs ! frequencies for direct integration
           real(kind=sp),    dimension(:), allocatable :: abscs ! abscissas for direct integration
           real(kind=sp),    dimension(:), allocatable :: del_re! delta (real-part), between the tabulated andf ?qawf integrators
           real(kind=sp),    dimension(:), allocatable :: del_im! delta (imag-part), between the tabulated and ?qawf integrators
           ! For Quadpack integrators (cached out info)
           ! The outer dimension size is 2 (real,imag) parts
           real(kind=sp),    dimension(:,:), allocatable :: abserr! !! estimate of the modulus of the absolute error,
                                                                  !! which should equal or exceed `abs(i-result)`
           integer(kind=i4), dimension(:,:), allocatable :: neval ! !! number of integrand evaluations
           integer(kind=i4), dimension(:,:), allocatable :: ier   ! !! * ier = 0 normal and reliable termination of the
                                                                  !!   routine. it is assumed that the requested
                                                                  !!   accuracy has been achieved.
                                                                  !! * ier>0 abnormal termination of the routine.
                                                                  !!   the estimates for integral and error are
                                                                  !!   less reliable. it is assumed that the
                                                                  !!   requested accuracy has not been achieved.
           integer(kind=i4), dimension(:,:), allocatable :: lst   ! !! on return, lst indicates the number of cycles
                                                                  !! actually needed for the integration.
                                                                  !! if `omega = 0`, then lst is set to 1.
     end type signal_ft_cont_c4_t

      ! Signal Fourier Transform (continous) double precision.
     type, public :: signal_ft_cont_c8_t
           
           logical(kind=i4), dimension(15)             :: alloc_stat ! arrays allocation status (is_allocated output)      
           character(len=64) :: integrator_name                 ! either direct integration by ?qawf, or by tabulated data integrator
           character(len=12) :: transform_type                  ! direct or inverse transform     
           integer(kind=i4)  :: n                               ! number of sample signal values
           real(kind=sp),    dimension(:), allocatable :: a     ! lower limit of integration
           real(kind=sp),    dimension(:), allocatable :: b     ! upper limit of integration
           complex(kind=dp), dimension(:), allocatable :: cexp  ! complex exponential
           complex(kind=dp), dimension(:), allocatable :: ft_dir! direct transform
           complex(kind=dp), dimension(:), allocatable :: ft_inv! inverse transform
           real(kind=dp),    dimension(:), allocatable :: s_re  ! signal's real part
           real(kind=dp),    dimension(:), allocatable :: s_im  ! signal's imaginary part
           real(kind=dp),    dimension(:), allocatable :: freqs ! frequencies for direct integration
           real(kind=dp),    dimension(:), allocatable :: abscs ! abscissas for direct integration
           real(kind=dp),    dimension(:), allocatable :: del_re! delta (real-part), between the tabulated andf ?qawf integrators
           real(kind=dp),    dimension(:), allocatable :: del_im! delta (imag-part), between the tabulated and ?qawf integrators
           ! For Quadpack integrators (cached out info)
           ! The outer dimension size is 2 (real,imag) parts
           real(kind=dp),    dimension(:,:), allocatable :: abserr! !! estimate of the modulus of the absolute error,
                                                                  !! which should equal or exceed `abs(i-result)`
           integer(kind=i4), dimension(:,:), allocatable :: neval ! !! number of integrand evaluations
           integer(kind=i4), dimension(:,:), allocatable :: ier   ! !! * ier = 0 normal and reliable termination of the
                                                                  !!   routine. it is assumed that the requested
                                                                  !!   accuracy has been achieved.
                                                                  !! * ier>0 abnormal termination of the routine.
                                                                  !!   the estimates for integral and error are
                                                                  !!   less reliable. it is assumed that the
                                                                  !!   requested accuracy has not been achieved.
           integer(kind=i4), dimension(:,:), allocatable :: lst   ! !! on return, lst indicates the number of cycles
                                                                  !! actually needed for the integration.
                                                                  !! if `omega = 0`, then lst is set to 1.
     end type signal_ft_cont_c8_t

end module waveform_analysis_types