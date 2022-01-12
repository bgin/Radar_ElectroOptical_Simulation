

module mueller_calculus_avx512


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mueller_calculus_avx512'
 !          
 !          Purpose:
  !                       Mueller calculus based mainly on
  !                       // Based on: R.A. Chipman, "Polarimetry" chapter in Handbook of Optics Volume 2
 !                        // (McGraw-Hill, New York, 1995).
 !                     
 !
 !          History:
 !                        
 !                        Date: 12-01-2022
 !                        Time: 15:11 GMT+2
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
     use mod_kinds,         only : i4, sp, dp
     use mod_vectypes,      only : ZMM16r4_t, ZMM8r8_t
     use mod_avx512_cvec16, only : ZMM16c4
     use mod_avx512_cvec8,  only : ZMM8c8
     use mueller_types_avx512
     implicit none
     public

    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59

    ! Major version
    integer(kind=i4), parameter, public :: MUELLER_CALCULUS_AVX512_MAJOR = 1
    
    ! Minor version
    integer(kind=i4), parameter, public :: MUELLER_CALCULUS_AVX512_MINOR = 0
    
    ! Micro version
    integer(kind=i4), parameter, public :: MUELLER_CALCULUS_AVX512_MICRO = 0
    
    ! Module full version
    integer(kind=i4), parameter, public :: MUELLER_CALCULUS_AVX512_FULLVER = 1000*MUELLER_CALCULUS_AVX512_MAJOR+100*MUELLER_CALCULUS_AVX512_MINOR+ &
                                             10*MUELLER_CALCULUS_AVX512_MICRO
    
    ! Module creation date
    character(*),       parameter, public :: MUELLER_TYPES_AVX512_CREATE_DATE = "12-01-2022 15:16 +00200 (WED 12 JAN 2022 GMT+2)"
    
    ! Module build date
    character(*),       parameter, public :: MUELLER_TYPES_AVX512_BUILD_DATE = __DATE__ ":"__TIME__
    
    ! Module author info
    character(*),       parameter, public :: MUELLER_TYPES_AVX512_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com "
    
    ! Module short synopsis
    character(*),       parameter, public :: MUELLER_TYPES_AVX512_SYNOPSIS = " Mueller calculus implementation"

  contains

    !=============================================================
    !
    ! /*
    !    Jones-Vector components -- packed single-precision
    !    16-tuple vectors of complex numbers [deinterleaved]
    !  */
    !==============================================================

    pure function JVec2x16c16_set_1() result(cvec)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x16c16_set_1
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x16c16_set_1
      !DIR$ ATTRIBUTES VECTOR :: JVec2x16c16_set_1
#endif
       type(JVec2x16c16) :: cvec
      ! EXEC CODE ....
       cvec.p = ZMM16c4()
       cvec.s = ZMM16c4()
    end function JVec2x16c16_set_1
    

    pure function JVec2x16c16_set_2(c1,c2) result(cvec)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x16c16_set_2
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x16c16_set_2
      !DIR$ ATTRIBUTES VECTOR :: JVec2x16c16_set_2
#endif
        complex(kind=sp),    intent(in) :: c1
        complex(kind=sp),    intent(in) :: c2
        type(JVec2x16c16) :: cvec
        !Exec code ....
        cvec.p = complex1x_init(c1)
        cvec.s = complex1x_init(c2)
    end function JVec2x16c16_set_2

   
    pure function JVec2x16c16_set_3(c1,c2) result(cvec)
#if defined(__INTEL_COMPILER) || defined(__ICC)
      !DIR$ ATTRIBUTES INLINE :: JVec2x16c16_set_3
      !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: JVec2x16c16_set_3
      !DIR$ ATTRIBUTES VECTOR :: JVec2x16c16_set_3
#endif
        complex(kind=sp), dimension(0:15), intent(in) :: c1
        complex(kind=sp), dimension(0:15), intent(in) :: c2
#if defined(__INTEL_COMPILER) || defined(__ICC)
        !DIR$ ASSUME_ALIGNED c1:64
        !DIR$ ASSUME_ALIGNED c2:64
#endif
        type(JVec2x16c16) :: cvec
        ! Exec code ....
        cvec.p = complex2x16_init(c1)
        cvec.s = complex2x16_init(c2)
    end function JVec2x16c16_set_3


      
    
    







end module mueller_calculus_avx512
