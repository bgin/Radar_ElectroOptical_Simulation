
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

module sph_complex_types


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         sph_complex_types
 !          
 !          Purpose:
 !                       Complex Spherical Harmonics up to degree (l=10) derived types
 !                        
 !          History:
 !                        Date: 19-10-2024
 !                        Time: 02:57 GMT+2
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
 !                      https://en.wikipedia.org/wiki/Table_of_spherical_harmonics
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
!==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.

    use mod_kinds,    only : i4
    use sse_cvec2
    use sse_cvec4
    use avx_cvec4
    use avx_cvec8
    use avx512_cvec8
    use avx512_cvec16
    use mod_vectypes, only : XMM2r8_t, XMM4r4_t, YMM4r8_t, YMM8r4_t, ZMM8r8_t, ZMM16r4_t
    
    public
    implicit none
    
    
      ! Major version
     integer(kind=i4),  parameter :: SPH_COMPLEX_TYPES_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: SPH_COMPLEX_TYPES_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: SPH_COMPLEX_TYPES_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: SPH_COMPLEX_TYPES_FULLVER =   &
            1000*SPH_COMPLEX_TYPES_MAJOR+100*SPH_COMPLEX_TYPES_MINOR+10*SPH_COMPLEX_TYPES_MICRO
     ! Module creation date
     character(*),        parameter :: SPH_COMPLEX_TYPES_CREATE_DATE = "19-10-2024 02:57AM +00200 (SAT 19 OCT 2024 GMT+2)"
     ! Module build date
     character(*),        parameter :: SPH_COMPLEX_TYPES_BUILD_DATE  = __DATE__ " " __TIME__
     ! Module author info
     character(*),        parameter :: SPH_COMPLEX_TYPES_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: SPH_COMPLEX_TYPES_SYNOPSIS  = "Complex Spherical Harmonics up to degree (l=10)." 
     
     
     ! Data types
     
     
     type, public :: csph_harmonic_deg1_4_v128pd_t
           type(XMM2c8),   dimension(:), allocatable :: yxx
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)  
           !dir$ attributes align : 16 :: yxx
#endif
           type(XMM2c8),   dimension(:), allocatable :: c
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)  
           !dir$ attributes align : 16 :: c
#endif
           type(XMM2r8_t), dimension(:), allocatable :: z
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)  
           !dir$ attributes align : 16 :: z
#endif
           type(XMM2r8_t), dimension(:), allocatable :: r
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)  
           !dir$ attributes align : 16 :: r
#endif
           integer(i4)                               :: nz   
           integer(i4)                               :: nr
           integer(i4)                               :: deg ! degree
     end type csph_harmonic_deg1_4_v128pd_t
     
     type, public :: csph_harmonic_deg5_10_v128pd_t
           type(XMM2r8_t),  dimension(:), allocatable :: theta
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)  
           !dir$ attributes align : 16 :: theta
#endif
           type(XMM2r8_t),  dimension(:), allocatable :: phi
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)  
           !dir$ attributes align : 16 :: phi
#endif
           type(XMM2c8),    dimension(:), allocatable :: yxx
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)  
           !dir$ attributes align : 16 :: yxx
#endif
           integer(i4)                                :: deg
           integer(i4)                                :: ntheta
           integer(i4)                                :: nphi
     end type csph_harmonic_deg5_10_v128pd_pdt
     
     
     type, public :: csph_harmonic_deg1_4_v128ps_t
           type(XMM4c4),   dimension(:), allocatable :: yxx
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)  
           !dir$ attributes align : 16 :: yxx
#endif
           type(XMM4c4),   dimension(:), allocatable :: c
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)  
           !dir$ attributes align : 16 :: c
#endif
           type(XMM4r4_t), dimension(:), allocatable :: z
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)             
           !dir$ attributes align : 16 :: z
#endif
           type(XMM4r4_t), dimension(:), allocatable :: r
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)  
           !dir$ attributes align : 16 :: r
#endif
           integer(i4)                               :: nz   
           integer(i4)                               :: nr
           integer(i4)                               :: deg ! degree
     end type csph_harmonic_deg1_4_v128ps_t
     
     type, public :: csph_harmonic_deg5_10_v128ps_t
           type(XMM4r4_t),  dimension(:), allocatable :: theta
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)  
           !dir$ attributes align : 16 :: theta
#endif
           type(XMM4r4_t),  dimension(:), allocatable :: phi
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)  
           !dir$ attributes align : 16 :: phi
#endif
           type(XMM4c4),    dimension(:), allocatable :: yxx
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)  
           !dir$ attributes align : 16 :: yxx
#endif
           integer(i4)                                :: deg
           integer(i4)                                :: ntheta
           integer(i4)                                :: nphi
     end type csph_harmonic_deg5_10_v128ps_t
     
     
     type, public :: csph_harmonic_deg1_4_v256pd_t
           type(YMM4c8),   dimension(:), allocatable :: yxx
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)  
           !dir$ attributes align : 32 :: yxx
#endif
           type(YMM4c8),   dimension(:), allocatable :: c
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)  
           !dir$ attributes align : 32 :: c
#endif
           type(YMM4r8_t), dimension(:), allocatable :: z
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)  
           !dir$ attributes align : 32 :: z
#endif
           type(YMM4r8_t), dimension(:), allocatable :: r
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)  
           !dir$ attributes align : 32 :: r
#endif
           integer(i4)                               :: nz   
           integer(i4)                               :: nr
           integer(i4)                               :: deg ! degree
     end type csph_harmonic_deg1_4_v256pd_t
     
     type, public :: csph_harmonic_deg5_10_v256pd_t
           type(YMM4r8_t),  dimension(:), allocatable :: theta
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)  
           !dir$ attributes align : 32 :: theta
#endif
           type(YMM4r8_t),  dimension(:), allocatable :: phi
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)  
           !dir$ attributes align : 32 :: phi
#endif
           type(YMM4c8),    dimension(:), allocatable :: yxx
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)  
           !dir$ attributes align : 32 :: yxx
#endif
           integer(i4)                                :: deg
           integer(i4)                                :: ntheta
           integer(i4)                                :: nphi
     end type csph_harmonic_deg5_10_v256pd_t
     
     
    type, public :: csph_harmonic_deg1_4_v256ps_t
           type(YMM8c4),   dimension(:), allocatable :: yxx
#if defined(__INTEL_COMPILER) && !defined(__GNUC__) 
           !dir$ attributes align  : 32  :: yxx
#endif
           type(YMM8c4),   dimension(:), allocatable :: c
#if defined(__INTEL_COMPILER) && !defined(__GNUC__) 
           !dir$ attributes align  : 32  :: c
#endif
           type(YMM8r4_t), dimension(:), allocatable :: z
#if defined(__INTEL_COMPILER) && !defined(__GNUC__) 
           !dir$ attributes align  : 32  :: z
#endif
           type(YMM8r4_t), dimension(:), allocatable :: r
#if defined(__INTEL_COMPILER) && !defined(__GNUC__) 
           !dir$ attributes align  : 32  :: r
#endif
           integer(i4)                               :: nz   
           integer(i4)                               :: nr
           integer(i4)                               :: deg ! degree
     end type csph_harmonic_deg1_4_v256ps_t
     
     type, public :: csph_harmonic_deg5_10_v256ps_t
           type(YMM8r4_t),  dimension(:), allocatable :: theta
#if defined(__INTEL_COMPILER) && !defined(__GNUC__) 
           !dir$ attributes align  : 32  :: theta
#endif
           type(YMM8r4_t),  dimension(:), allocatable :: phi
#if defined(__INTEL_COMPILER) && !defined(__GNUC__) 
           !dir$ attributes align  : 32  :: phi
#endif
           type(YMM8c4),    dimension(:), allocatable :: yxx
#if defined(__INTEL_COMPILER) && !defined(__GNUC__) 
           !dir$ attributes align  : 32  :: yxx
#endif
           integer(i4)                                :: deg
           integer(i4)                                :: ntheta
           integer(i4)                                :: nphi
     end type csph_harmonic_deg5_10_v256ps_t
     
     
     type, public :: csph_harmonic_deg1_4_v512pd_t
           type(ZMM8c8),   dimension(:), allocatable :: yxx
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)  
           !dir$ attributes align : 64 :: yxx
#endif
           type(ZMM8c8),   dimension(:), allocatable :: c
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)  
           !dir$ attributes align : 64 :: c
#endif
           type(ZMM8r8_t), dimension(:), allocatable :: z
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)  
           !dir$ attributes align : 64 :: z
#endif
           type(ZMM8r8_t), dimension(:), allocatable :: r
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)  
           !dir$ attributes align : 64 :: r
#endif
           integer(i4)                               :: nz   
           integer(i4)                               :: nr
           integer(i4)                               :: deg ! degree
     end type csph_harmonic_deg1_4_v512pd_t
     
     type, public :: csph_harmonic_deg5_10_v512pd_t
           type(ZMM8r8_t),  dimension(:), allocatable :: theta
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)  
           !dir$ attributes align : 64 :: theta
#endif
           type(ZMM8r8_t),  dimension(:), allocatable :: phi
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)  
           !dir$ attributes align : 64 :: phi
#endif
           type(ZMM8c8),    dimension(:), allocatable :: yxx
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)  
           !dir$ attributes align : 64 :: yxx
#endif
           integer(i4)                                :: deg
           integer(i4)                                :: ntheta
           integer(i4)                                :: nphi
     end type csph_harmonic_deg5_10_v512pd_t
     
     
     type, public :: csph_harmonic_deg1_4_v512ps_t
           type(ZMM16c4),   dimension(:), allocatable :: yxx
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)  
           !dir$ attributes align  : 64  :: yxx
#endif          
           type(ZMM16c4),   dimension(:), allocatable :: c
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)             
           !dir$ attributes align  : 64  :: c
#endif
           type(ZMM16r4_t), dimension(:), allocatable :: z
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)             
           !dir$ attributes align  : 64  :: z
#endif
           type(ZMM16r4_t), dimension(:), allocatable :: r
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)             
           !dir$ attributes align  : 64  :: r
#endif
           integer(i4)                               :: nz   
           integer(i4)                               :: nr
           integer(i4)                               :: deg ! degree
     end type csph_harmonic_deg1_4_v512ps_t
     
     type, public :: csph_harmonic_deg5_10_v512ps_t
           type(ZMM16r4_t),  dimension(:), allocatable :: theta
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)  
           !dir$ attributes align  : 64  :: theta
#endif
           type(ZMM16r4_t),  dimension(:), allocatable :: phi
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)             
           !dir$ attributes align  : 64  :: phi
#endif
           type(ZMM16c4),    dimension(:), allocatable :: yxx
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)  
           !dir$ attributes align  : 64  :: yxx
#endif
           integer(i4)                                :: deg
           integer(i4)                                :: ntheta
           integer(i4)                                :: nphi
     end type csph_harmonic_deg5_10_v512ps_t
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
end module sph_complex_types
