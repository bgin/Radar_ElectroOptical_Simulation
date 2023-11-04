

module mueller_types_avx


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mueller_types_avx'
 !          
 !          Purpose:
 !                     SIMD-friendly derived types which describes the basic
 !                     Mueller calculus data types.
 !                     
 !
 !          History:
 !                        
 !                        Date: 04-11-2023
 !                        Time: 13:25 GMT+2
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:
 !                 
 !                      Bernard Gingold
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
     use mod_kinds,         only : i4
     use mod_vectypes,      only : YMM8r4_t, YMM4r8_t
     use mod_avx_cvec8,  only : YMM8c4
     use mod_avx_cvec4,  only : YMM4c8

     implicit none
     public

    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59

    ! Major version
    integer(kind=i4), parameter, public :: MUELLER_TYPES_AVX_MAJOR = 1
    
    ! Minor version
    integer(kind=i4), parameter, public :: MUELLER_TYPES_AVX_MINOR = 0
    
    ! Micro version
    integer(kind=i4), parameter, public :: MUELLER_TYPES_AVX_MICRO = 0
    
    ! Module full version
    integer(kind=i4), parameter, public :: MUELLER_TYPES_AVX_FULLVER = 1000*MUELLER_TYPES_AVX_MAJOR+100*MUELLER_TYPES_AVX_MINOR+ &
                                             10*MUELLER_TYPES_AVX_MICRO
    
    ! Module creation date
    character(*),       parameter, public :: MUELLER_TYPES_AVX_CREATE_DATE = "04-11-2023 10:31 +00200 (SAT 04 NOV 2023 GMT+2)"
    
    ! Module build date
    character(*),       parameter, public :: MUELLER_TYPES_AVX_BUILD_DATE = __DATE__ ":"__TIME__
    
    ! Module author info
    character(*),       parameter, public :: MUELLER_TYPES_AVX_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com "
    
    ! Module short synopsis
    character(*),       parameter, public :: MUELLER_TYPES_AVX_SYNOPSIS = " Mueller calculus SIMD (AVX) data types"


    !==============================================================
    !    Jones Vector based on two 8-tuple SIMD complex types.
    !==============================================================

    type, public :: JVec2x8c4
       sequence
       type(YMM8c4) :: p
       type(YMM8c4) :: s
    end type JVec2x8c4


    !==============================================================
    !    Jones Vector based on two 4-tuple SIMD complex types.
    !==============================================================

    type, public :: JVec2x4c8
       sequence
       type(YMM4c8) :: p
       type(YMM4c8) :: s
    end type JVec2x4c8


    !==============================================================
    !    Jones Matrix based on four 8-tuple SIMD complex types.
    !==============================================================

    type, public :: JMat4x8c4
       sequence
       type(YMM8c4) :: j0
       type(YMM8c4) :: j1
       type(YMM8c4) :: j2
       type(YMM8c4) :: j3
    end type JMat4x8c4


    !==============================================================
    !    Jones Matrix based on four 4-tuple SIMD complex types.
    !==============================================================

    type, public :: JMat4x4c8
       sequence
       type(YMM4c8) :: j0
       type(YMM4c8) :: j1
       type(YMM4c8) :: j2
       type(YMM4c8) :: j3
    end type JMat4x4c8
    

    !==============================================================
    !  Stokes Vector based on four 8-tuple SIMD real types.
    !==============================================================

    type, public :: SVec4x8v8
       sequence
       type(YMM8r4_t) :: s0
       type(YMM8r4_t) :: s1
       type(YMM8r4_t) :: s2
       type(YMM8r4_t) :: s3
    end type SVec4x8v18


    !==============================================================
    !  Stokes Vector based on four 4-tuple SIMD real types.
    !==============================================================

    type, public :: SVec4x4v8
       sequence
       type(YMM4r8_t) :: s0
       type(YMM4r8_t) :: s1
       type(YMM4r8_t) :: s2
       type(YMM4r8_t) :: s3
    end type SVec4x4v8


    !=============================================================
    !  Mueller Matrix based on 16 8-tuple SIMD real types.
    !=============================================================

    type, public :: MMat16x8v4
       sequence
       type(YMM8r4_t), dimension(16) :: mmat
    end type MMat16x8v4


    !=============================================================
    !  Mueller Matrix based on 16 4-tuple SIMD real types.
    !=============================================================

    type, public :: MMat16x4v8
       sequence
       type(YMM4r8_t), dimension(16) :: mmat
    end type MMat16x4v8
    
    

end module mueller_types_avx
