

module mueller_types_avx512


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mueller_types_avx512'
 !          
 !          Purpose:
 !                     SIMD-friendly derived types which describes the basic
 !                     Mueller calculus data types.
 !                     
 !
 !          History:
 !                        
 !                        Date: 10-01-2022
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
     use mod_vectypes,      only : ZMM16r4_t, ZMM8r8_t
     use mod_avx512_cvec16, only : ZMM16c4
     use mod_avx512_cvec8,  only : ZMM8c8

     implicit none
     public

    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59

    ! Major version
    integer(kind=i4), parameter, public :: MUELLER_TYPES_AVX512_MAJOR = 1
    
    ! Minor version
    integer(kind=i4), parameter, public :: MUELLER_TYPES_AVX512_MINOR = 0
    
    ! Micro version
    integer(kind=i4), parameter, public :: MUELLER_TYPES_AVX512_MICRO = 0
    
    ! Module full version
    integer(kind=i4), parameter, public :: MUELLER_TYPES_AVX512_FULLVER = 1000*MUELLER_TYPES_AVX512_MAJOR+100*MUELLER_TYPES_AVX512_MINOR+ &
                                             10*MUELLER_TYPES_AVX512_MICRO
    
    ! Module creation date
    character(*),       parameter, public :: MUELLER_TYPES_AVX512_CREATE_DATE = "10-01-2022 10:31 +00200 (MON 10 JAN 2022 GMT+2)"
    
    ! Module build date
    character(*),       parameter, public :: MUELLER_TYPES_AVX512_BUILD_DATE = __DATE__ ":"__TIME__
    
    ! Module author info
    character(*),       parameter, public :: MUELLER_TYPES_AVX512_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com "
    
    ! Module short synopsis
    character(*),       parameter, public :: MUELLER_TYPES_AVX512_SYNOPSIS = " Mueller calculus SIMD data types"


    !==============================================================
    !    Jones Vector based on two 16-tuple SIMD complex types.
    !==============================================================

    type, public :: JVec2x16c16
       sequence
       type(ZMM16c4) :: p
       type(ZMM16c4) :: s
    end type JVec2x16c16


    !==============================================================
    !    Jones Vector based on two 8-tuple SIMD complex types.
    !==============================================================

    type, public :: JVec2x8c8
       sequence
       type(ZMM8c8) :: p
       type(ZMM8c8) :: s
    end type JVec2x8c8


    !==============================================================
    !    Jones Matrix based on four 16-tuple SIMD complex types.
    !==============================================================

    type, public :: JMat4x16c16
       sequence
       type(ZMM16c4) :: j0
       type(ZMM16c4) :: j1
       type(ZMM16c4) :: j2
       type(ZMM16c4) :: j3
    end type JMat4x16c16


    !==============================================================
    !    Jones Matrix based on four 8-tuple SIMD complex types.
    !==============================================================

    type, public :: JMat4x8c8
       sequence
       type(ZMM8c8) :: j0
       type(ZMM8c8) :: j1
       type(ZMM8c8) :: j2
       type(ZMM8c8) :: j3
    end type JMat4x8c8
    

    !==============================================================
    !  Stokes Vector based on four 16-tuple SIMD real types.
    !==============================================================

    type, public :: SVec4x16v16
       sequence
       type(ZMM16r4_t) :: s0
       type(ZMM16r4_t) :: s1
       type(ZMM16r4_t) :: s2
       type(ZMM16r4_t) :: s3
    end type SVec4x16v16


    !==============================================================
    !  Stokes Vector based on four 8-tuple SIMD real types.
    !==============================================================

    type, public :: SVec4x8v8
       sequence
       type(ZMM8r8_t) :: s0
       type(ZMM8r8_t) :: s1
       type(ZMM8r8_t) :: s2
       type(ZMM8r8_t) :: s3
    end type SVec4x8v8


    !=============================================================
    !  Mueller Matrix based on 16 16-tuple SIMD real types.
    !=============================================================

    type, public :: MMat16x16v16
       sequence
       type(ZMM16r4_t), dimension(16) :: mmat
    end type MMat16x16v16


    !=============================================================
    !  Mueller Matrix based on 16 8-tuple SIMD real types.
    !=============================================================

    type, public :: MMat16x8v8
       sequence
       type(ZMM8r8_t), dimension(16) :: mmat
    end type MMat16x8v8
    
    

end module mueller_types_avx512
