

module simd_rotation_kernels


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'simd_rotation_kernels'
 !          
 !          Purpose:
 !                      Explicit vectorization of various rotation kernels (AVX/AVX2/AVX512)
 !          History:
 !                        
 !                        Date: 28-11-2021
 !                        Time: 17:556 GMT+2
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
     use mod_kinds,   only : i4,sp,dp
     use mod_vectypes
     use omp_lib

     implicit none
     
     public


       ! Major version
    integer(kind=i4), parameter, public :: MOD_SIMD_ROTATION_KERNELS_MAJOR = 1
    
    ! Minor version
    integer(kind=i4), parameter, public :: MOD_SIMD_ROTATION_KERNELS_MINOR = 0
    
    ! Micro version
    integer(kind=i4), parameter, public :: MOD_SIMD_ROTATION_KERNELS_MICRO = 0
    
    ! Module full version
    integer(kind=i4), parameter, public :: MOD_SIMD_ROTATION_KERNELS_FULLVER = 1000*MOD_SIMD_ROTATION_KERNELS_MAJOR+100*MOD_SIMD_ROTATION_KERNELS_MINOR+ &
                                             *MOD_SIMD_ROTATION_KERNELS_MICRO
    
    ! Module creation date
    character(*),       parameter, public :: MOD_SIMD_ROTATION_KERNELS_CREATE_DATE = "21-11-2021 13:27 +00200 (SUN 21 NOV 2018 GMT+2)"
    
    ! Module build date
    character(*),       parameter, public :: MOD_SIMD_ROTATION_KERNELS_BUILD_DATE = __DATE__":"__TIME__
    
    ! Module author info
    character(*),       parameter, public :: MOD_SIMD_ROTATION_KERNELS_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com "
    
    ! Module short synopsis
    character(*),       parameter, public :: MOD_SIMD_ROTATION_KERNELS_SYNOPSIS = "SIMD optimized rotation computation subrotines"


    contains

#if defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
    subroutine q4x16_to_rmat9x16_zmm16r4(qx,qy,qz,qw, &
                                           row1,row2,row3,row4,row5, &
                                           row6,row7,row8,row9) !GCC$ ATTRIBUTES hot :: q4x16_to_rmat9x16_zmm16r4 !GCC$ ATTRIBUTES inline :: q4x16_to_rmat9x16_zmm16r4
#elif defined(__INTEL_COMPILER) || defined(__ICC)
    subroutine q4x16_to_rmat9x16_zmm16r4(qx,qy,qz,qw, &
                                           row1,row2,row3,row4,row5, &
                                           row6,row7,row8,row9)
        !DIR$ ATTRIBUTES INLINE :: q4x16_to_rmat9x16_zmm16r4
        !DIR$ ATTRIBUTES VECTOR :: q4x16_to_rmat9x16_zmm16r4
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: q4x16_to_rmat9x16_zmm16r4
#endif
        type(ZMM16r4_t), intent(in) :: qx
        type(ZMM16r4_t), intent(in) :: qy
        type(ZMM16r4_t), intent(in) :: qz
        type(ZMM16r4_t), intent(in) :: qw
        type(ZMM16r4_t), intent(out) :: row1
        type(ZMM16r4_t), intent(out) :: row2
        type(ZMM16r4_t), intent(out) :: row3
        type(ZMM16r4_t), intent(out) :: row4
        type(ZMM16r4_t), intent(out) :: row5
        type(ZMM16r4_t), intent(out) :: row6
        type(ZMM16r4_t), intent(out) :: row7
        type(ZMM16r4_t), intent(out) :: row8
        type(ZMM16r4_t), intent(out) :: row9
        !Locals
        type(ZMM16r4_t), automatic :: t1,t2,t3,t4,t5, &
             t6,t7,t8,t9,t10
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ATTRIBUTES ALIGN : 64 :: t1,t2,t3,t4,t5
        !DIR$ ATTRIBUTES ALIGN : 64 :: t6,t7,t8,t9,t10
#endif
        ! Executable code
        t0.v = qx.v*qx.v
        t1.v = qw.v+qx.v+qy.v
        t2.v = t1.v*t1.v
        t3.v = t0.v-t2.v
        t4.v = qy.v*qy.v
        row1.v = v16_2.v*t4.v+t3.v
        t5.v = qy.v-qz.v
        t8.v = qx.v*qw.v
        row2.v = v16_2.v*(t5.v-t8.v)
        t7.v = qw.v*qy.v
        t10.v = qx.v*qz.v
        row3.v = v16_2.v*(t7.v+t10.v)
        row4.v = v16_2.v*(t5.v+t8.v)
        row5.v = v16.2*(qz.v*qz.v)+t3.v
        t9.v = qx.v*qy.v
        row6.v = v16_2.v*(t6.v-t9.v)
        row7.v = v16_2.v*(t7.v-t8.v)
        row8.v = v16_2.v*(t6.v+t9.v)
        row9.v = v16_2.v*(qw.v*qw.v)+t3.v
    end subroutine 

    










end module simd_rotation_kernels



  
