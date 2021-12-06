

module avx_rotations


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'avx_rotations'
 !          
 !          Purpose:
 !                      Explicit vectorization of various rotation kernels (AVX)
 !          History:
 !                        
 !                        Date: 06-12-2021
 !                        Time: 15:56 GMT+2
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:
 !                 
 !                   Bernard Gingold
 !         
 !         
 !         
 !          
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !=================================================================================
     ! Tab:5 col - Type and etc.. definitions
     ! Tab:10,11 col - Type , function and subroutine code blocks.
     use mod_kinds,    only : i4,sp,dp
     use mod_vectypes, only : YMM8r4_t,YMM4r8_t
     implicit none
     public

         ! Major version
    integer(kind=i4), parameter, public :: MOD_AVX_ROTATIONS_MAJOR = 1
    
    ! Minor version
    integer(kind=i4), parameter, public :: MOD_AVX_ROTATIONS_MINOR = 0
    
    ! Micro version
    integer(kind=i4), parameter, public :: MOD_AVX_ROTATIONS_MICRO = 0
    
    ! Module full version
    integer(kind=i4), parameter, public :: MOD_AVX_ROTATIONS_FULLVER = 1000*MOD_AVX_ROTATIONS_MAJOR+100*MOD_AVX_ROTATIONS_MINOR+ &
                                             *MOD_AVX_ROTATIONS_MICRO
    
    ! Module creation date
    character(*),       parameter, public :: MOD_AVX_ROTATIONS_CREATE_DATE = "06-12-2021 15:57 +00200 (MON 06 DEC 2021 GMT+2)"
    
    ! Module build date
    character(*),       parameter, public :: MOD_AVX_ROTATIONS_BUILD_DATE = __DATE__":"__TIME__
    
    ! Module author info
    character(*),       parameter, public :: MOD_AVX_ROTATIONS_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com "
    
    ! Module short synopsis
    character(*),       parameter, public :: MOD_AVX_ROTATIONS_SYNOPSIS = "AVX optimized rotation computation subroutines"

    type(YMM8r4_t), parameter, private v8_0  = YMM8r4_t(0.0_sp)
    type(YMM8r4_t), parameter, private v8_1  = YMM8r4_t(1.0_sp)
    type(YMM8r4_t), parameter, private v8_2  = YMM8r4_t(2.0_sp)
    type(YMM8r4_t), parameter, private v8_n2 = YMM8r4_t(-2.0_sp)



















end module avx_rotations
