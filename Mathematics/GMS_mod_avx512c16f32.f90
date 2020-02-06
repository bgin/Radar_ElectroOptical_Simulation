
#include "Config.fpp"

module  mod_avx512c16f32


 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_avx512c16f32
 !          
 !          Purpose:
 !                      This module contains a decomposed to real and imaginary
 !                      parts a complex vector of 16 elements (complex numbers)
 !                      This representation nicely fits into 2 AVX-512 ZMMx
 !                      registers.
 !          History:
 !                        Date: 03-02-2020
 !                        Time: 11:57 GMT+2
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
 !                 
 !          References:
 !         
 !                          Own implementation (ported from similar C++ version)
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.

     use mod_kinds, only : int1,int4,sp
     use mod_vectypes, only : ZMM16r4_t
     use, intrinsic :: ISO_C_BINDING
     implicit none
     !=====================================================59
     !  File and module information:
     !  version,creation and build date, author,description
     !=====================================================59

     ! Major version
     integer(kind=int4), parameter :: MOD_AVX512C16F32_MAJOR = 1
     ! Minor version
     integer(kind=int4), parameter :: MOD_AVX512C16F32_MINOR = 0
     ! Micro version
     integer(kind=int4), parameter :: MOD_AVX512C16F32_MICRO = 0
     ! Full version
     integer(kind=int4), parameter :: MOD_AVX512C16F32_FULLVER = &
          1000*MOD_AVX512C16F32_MAJOR+100*MOD_AVX512C16F32_MINOR+10*MOD_AVX512C16F32_MICRO
     ! Module creation date
     character(*),       parameter :: MOD_AVX512C16F32_CREATION_DATE = "03-02-2020 11:57 +00200 (MON 03 FEB 2020 GMT+2)"
     ! Module build date
     character(*),       parameter :: MOD_AVX512C16F32_BUILD_DATE    = __DATE__ " " __TIME__
     ! Module author info
     character(*),       parameter :: MOD_AVX512C16F32_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),       parameter :: MOD_AVX512C16F32_SYNOPSIS      = "Packed complex vector of 16 elements (single precision)"

     type, public :: AVX512c16f32_t
        public
        sequence
        real(kind=sp), dimension(0:15) :: re
        real(kind=sp), dimension(0:15) :: im
     end type AVX512c16f32_t

     interface operator (+)
         module procedure c16_add_c16
         module procedure c16_add_c1
         module procedure c16_add_v16
         module procedure c16_add_s1
         module procedure c1_add_c16
         module procedure v16_add_c16
         module procedure s1_add_c16
     end interface operator (+)

     interface operator (-)
         module procedure c16_sub_c16
         module procedure c16_sub_c1
         module procedure c16_sub_v16
         module procedure c16_sub_s1
         module procedure c1_sub_c16
         module procedure v16_sub_c16
         module procedure s1_sub_c16
     end interface operator (-)

     interface operator (*)
         module procedure c16_mul_c16
         module procedure c16_mul_c1
         module procedure c16_mul_v16
         module procedure c16_mul_s1
         module procedure c1_mul_c16
         module procedure v16_mul_c16
         module procedure s1_mul_c16
     end interface operator (*)

     interface operator (/)
         module procedure c16_div_c16
         module procedure c16_div_c1
         module procedure c16_div_v16
         module procedure c16_div_s1
         module procedure c1_div_c16
         module procedure v16_div_c16
         module procedure s1_div_c16
     end interface operator (/)

#if (USE_INTRINSIC_VECTOR_COMPARE) == 1

      interface operator (==)
         module procedure c16_eq_c16
         module procedure c16_eq_c1
       
         module procedure c1_eq_c16
        
      end interface operator (==)

      interface operator (/=)
         module procedure c16_neq_c16
         module procedure c16_neq_c1
       
         module procedure c2_neq_c16
      
      end interface operator (/=)

      interface operator (>)
         module procedure c16_gt_c16
         module procedure c16_gt_c1
        
         module procedure c1_gt_c16
     
      end interface operator (>)

      interface operator (<)
         module procedure c16_lt_c16
         module procedure c16_lt_c1
        
         module procedure c1_lt_c16
        
      end interface operator (<)

      interface operator (>=)
         module procedure c16_ge_c16
         module procedure c16_ge_c1
       
         module procedure c1_ge_c16
        
      end interface operator (>=)

      interface operator (<=)
         module procedure c16_le_c16
         module procedure c16_le_c1
        
         module procedure c1_le_c16
        
      end interface operator (<=)
#else
     interface operator (==)
         module procedure c16_eq_c16
         module procedure c16_eq_c1
       
         module procedure c1_eq_c16
        
      end interface operator (==)

      interface operator (/=)
         module procedure c16_neq_c16
         module procedure c16_neq_c1
       
         module procedure c1_neq_c16
      
      end interface operator (/=)

      interface operator (>)
         module procedure c16_gt_c16
         module procedure c16_gt_c2
        
         module procedure c2_gt_c16
     
      end interface operator (>)

      interface operator (<)
         module procedure c16_lt_c16
         module procedure c16_lt_c1
        
         module procedure c1_lt_c16
        
      end interface operator (<)

      interface operator (>=)
         module procedure c16_ge_c16
         module procedure c16_ge_c1
       
         module procedure c1_ge_c16
        
      end interface operator (>=)

      interface operator (<=)
         module procedure c16_le_c16
         module procedure c16_le_c1
        
         module procedure c1_le_c16
        
      end interface operator (<=)

#endif

    contains

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER     
      pure function default_init() result(iq) !GCC$ ATTRIBUTES cold :: default_init !GCC$ ATTRIBUTES vectorcall :: default_init !GCC$ ATTRIBUTES inline :: default_init
#elif defined __ICC || defined __INTEL_COMPILER
        !DIR$ ATTRIBUTES INLINE :: default_init
      pure function default_init() result(iq)
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: default_init
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: default_init
      
#endif
        use mod_vecconsts, only : v16_n0
#if defined __INTEL_COMPILER 
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(AVX512c16f32_t) :: iq
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        type(AVX512c16f32_t) :: iq !GCC$ ATTRIBUTES aligned(64) :: iq
#endif
        iq.re = v16_n0
        iq.im = v16_n0
      end function default_init

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      pure function array_init(re,im) result(iq) !GCC$ ATTRIBUTES cold :: array_init !GCC$ ATTRIBUTES vectorcall :: array_init !GCC$ ATTRIBUTES inline :: array_init
#elif defined __ICC || defined __INTEL_COMPILER
        !DIR$ ATTRIBUTES INLINE :: array_init
      pure function array_init(re,im) result(iq)
         !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: array_init
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: array_init
#endif
        real(kind=sp), dimension(0:15), intent(in) :: re
        real(kind=sp), dimension(0:15), intent(in) :: im
#if defined __INTEL_COMPILER 
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(AVX512c16f32_t) :: iq
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        type(AVX512c16f32_t) :: iq !GCC$ ATTRIBUTES aligned(64) :: iq
#endif
        iq.re = re
        iq.im = im
      end function array_init

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      pure function complex1_init(c) result(iq) !GCC$ ATTRIBUTES cold :: complex1_init !GCC$ ATTRIBUTES vectorcall :: complex1_init !GCC$ ATTRIBUTES inline :: complex1_init
#elif defined __ICC || defined __INTEL_COMPILER
        !DIR$ ATTRIBUTES INLINE :: complex1_init
      pure function complex1_init(c) result(iq)
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: complex1_init
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: complex_init
#endif
        complex(kind=sp),     intent(in) :: c
#if defined __INTEL_COMPILER 
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(AVX512c16f32_t) :: iq
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        type(AVX512c16f32_t) :: iq !GCC$ ATTRIBUTES aligned(64) :: iq
#endif
        iq.re = real(c,kind=sp)
        iq.im = aimag(c,kind=sp)
      end function complex1_init

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      pure function complex2x16_init(c) result(iq) !GCC$ ATTRIBUTES cold :: complex2x16_init !GCC$ ATTRIBUTES vectorcall :: complex2x16_init !GCC$ ATTRIBUTES inline :: complex2x16_init
#elif defined __ICC || defined __INTEL_COMPILER
        !DIR$ ATTRIBUTES INLINE :: complex2x16_init
      pure function complex2x16_init(c) result(iq)
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: complex2x16_init
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: complex2x16_init
#endif
        complex(kind=sp), dimension(0:15),  intent(in) :: c
#if defined __INTEL_COMPILER 
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(AVX512c16f32_t) :: iq
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        type(AVX512c16f32_t) :: iq !GCC$ ATTRIBUTES aligned(64) :: iq
#endif
        iq.re = real(c,kind=sp)
        iq.im = aimag(c,kind=sp)
      end function complex2x16_init

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      pure function zmm16r42x_init(v1,v2) result(iq) !GCC$ ATTRIBUTES cold :: zmm16r42x_init !GCC$ ATTRIBUTES vectorcall :: zmm16r42x_init !GCC$ ATTRIBUTES inline :: zmm16r42x_init
#elif defined __ICC || defined __INTEL_COMPILER
        !DIR$ ATTRIBUTES INLINE :: zmm16r42x_init
      pure function zmm16r42x_init(v1,v2) result(iq)
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: zmm16r42x_init
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: zmm16r42x_init
#endif
        type(ZMM16r4_t),    intent(in) :: v1
        type(ZMM16r4_t),    intent(in) :: v2
#if defined __INTEL_COMPILER 
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(AVX512c16f32_t) :: iq
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        type(AVX512c16f32_t) :: iq !GCC$ ATTRIBUTES aligned(64) :: iq
#endif
        iq.re = v1
        iq.im = v2
      end function zmm16r42x_init

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      pure function zmm16r41x_init(v) result(iq) !GCC$ ATTRIBUTES cold :: zmm16r41x_init !GCC$ ATTRIBUTES vectorcall :: zmm16r41x_init !GCC$ ATTRIBUTES inline :: zmm16r41x_init
#elif defined __ICC || defined __INTEL_COMPILER
        !DIR$ ATTRIBUTES INLINE :: zmm16r41x_init
      pure function zmm16r41x_init(v) result(iq)
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: zmm16r41x_init
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: zmm16r41x_init
#endif
        type(ZMM16r4_t),    intent(in) :: v
#if defined __INTEL_COMPILER 
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(AVX512c16f32_t) :: iq
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        type(AVX512c16f32_t) :: iq !GCC$ ATTRIBUTES aligned(64) :: iq
#endif
        iq.re = v
        iq.im = v16_n0
      end function zmm16r41x_init

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      pure function r41x_init(s) result(iq) !GCC$ ATTRIBUTES cold :: r41x_init !GCC$ ATTRIBUTES inline :: r41x_init
#elif defined __ICC || defined __INTEL_COMPILER
      pure function r41x_init(s) result(iq)
        !DIR$ ATTRIBUTES INLINE :: r41x_init
        !DIR$M ATTRIBUTES CODE_ALIGN : 16 :: r41x_init
#endif
        real(kind=sp),    intent(in) :: s
#if defined __INTEL_COMPILER 
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(AVX512c16f32_t) :: iq
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        type(AVX512c16f32_t) :: iq !GCC$ ATTRIBUTES aligned(64) :: iq
#endif
        iq.re = s
        iq.im = v16_n0
      end function r41x_init

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      pure function copy_init(rhs) result(iq) !GCC$ ATTRIBUTES cold :: copy_init !GCC$ ATTRIBUTES vectorcall :: copy_init !GCC$ ATTRIBUTES inline :: copy_init
#elif defined __ICC || defined __INTEL_COMPILER
        !DIR$ ATTRIBUTES INLINE :: copy_init
      pure function copy_init(rhs) result(iq)
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: copy_init
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: copy_init
#endif
        type(AVX512c16f32_t),    intent(in) :: rhs
#if defined __INTEL_COMPILER 
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(AVX512c16f32_t) :: iq
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        type(AVX512c16f32_t) :: iq !GCC$ ATTRIBUTES aligned(64) :: iq
#endif
         iq = rhs
      end function copy_init

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      pure function c16_add_c16(x,y) result(iq) !GCC$ ATTRIBUTES hot :: c16_add_c16 !GCC$ ATTRIBUTES vectorcall :: c16_add_c16 !GCC$ ATTRIBUTES inline :: c16_add_c16
#elif defined __ICC || defined __INTEL_COMPILER
        !DIR$ ATTRIBUTES INLINE :: c16_add_c16
      pure function c16_add_c16(x,y) result(iq)
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_add_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c16_add_c16
#endif
        type(AVX512c16f32_t),    intent(in) :: x
        type(AVX512c16f32_t),    intent(in) :: y
#if defined __INTEL_COMPILER 
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(AVX512c16f32_t) :: iq
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        type(AVX512c16f32_t) :: iq !GCC$ ATTRIBUTES aligned(64) :: iq
#endif
        iq.re = x.re+y.re
        iq.im = x.im+y.im
      end function c16_add_c16

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      pure function c16_add_c1(x,y) result(iq) !GCC$ ATTRIBUTES hot :: c16_add_c1 !GCC$ ATTRIBUTES vectorcall :: c16_add_c1 !GCC$ ATTRIBUTES inline :: c16_add_c1
#elif defined __ICC || defined __INTEL_COMPILER
        !DIR$ ATTRIBUTES INLINE :: c16_add_c1
      pure function c16_add_c1(x,y) result(iq)
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_add_c1
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c16_add_c1
#endif
        type(AVX512c16f32_t),    intent(in) :: x
        complex(kind=sp),        intent(in) :: y
#if defined __INTEL_COMPILER 
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(AVX512c16f32_t) :: iq
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        type(AVX512c16f32_t) :: iq !GCC$ ATTRIBUTES aligned(64) :: iq
#endif
        iq.re = x.re+real(y,kind=sp)
        iq.im = x.im+aimag(y,kind=sp)
      end function c16_add_c1

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      pure function c16_add_v16(x,y) result(iq) !GCC$ ATTRIBUTES hot :: c16_add_v16 !GCC$ ATTRIBUTES vectorcall :: c16_add_v16 !GCC$ ATTRIBUTES inline :: c16_add_v16
#elif defined __ICC || defined __INTEL_COMPILER
         !DIR$ ATTRIBUTES INLINE :: c16_add_v16
      pure function c16_add_v16(x,y) result(iq)
         !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_add_v16
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c16_add_v16
#endif
        use mod_vecconsts, only : v16_n0
        type(AVX512c16f32_t),    intent(in) :: x
        type(ZMM16r4_t),         intent(in) :: y
#if defined __INTEL_COMPILER 
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(AVX512c16f32_t) :: iq
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        type(AVX512c16f32_t) :: iq !GCC$ ATTRIBUTES aligned(64) :: iq
#endif
        iq.re = x.re+y.v
        iq.im = v16_n0
      end function c16_add_v16

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      pure function c16_add_s1(x,y) result(iq) !GCC$ ATTRIBUTES hot :: c16_add_s1 !GCC$ ATTRIBUTES vectorcall :: c16_add_s1 !GCC$ ATTRIBUTES inline :: c16_add_s1
#elif defined __ICC || defined __INTEL_COMPILER
        !DIR$ ATTRIBUTES INLINE :: c16_add_s1
      pure function c16_add_s1(x,y) result(iq)
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_add_s1
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c16_add_s1
#endif
        use mod_veccconts, only : v16_n0
        type(AVX512c16f32_t),    intent(in) :: x
        real(kind=sp),           intent(in) :: y
#if defined __INTEL_COMPILER 
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(AVX512c16f32_t) :: iq
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        type(AVX512c16f32_t) :: iq !GCC$ ATTRIBUTES aligned(64) :: iq
#endif
        iq.re = x.re+y
        iq.im = v16_n0
      end function c16_add_s1

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      pure function c1_add_c16(x,y) result(iq) !GCC$ ATTRIBUTES hot :: c1_add_c16 !GCC$ ATTRIBUTES vectorcall :: c1_add_c16 !GCC$ ATTRIBUTES inline :: c1_add_c16
#elif defined __ICC || defined __INTEL_COMPILER
      !DIR$ ATTRIBUTES INLINE :: c1_add_c16
      pure function c1_add_c16(x,y) result(iq)
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c1_add_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c1_add_c16
#endif
        complex(kind=sp),      intent(in) :: x
        type(AVX512c16f32_t),  intent(in) :: y
#if defined __INTEL_COMPILER 
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(AVX512c16f32_t) :: iq
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        type(AVX512c16f32_t) :: iq !GCC$ ATTRIBUTES aligned(64) :: iq
#endif
        iq.re = real(x,kind=sp)+y.re
        iq.im = aimag(x,kind=sp)+y.im
      end function c1_add_c16

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      pure function v16_add_c16(x,y) result(iq) !GCC$ ATTRIBUTES hot :: v16_add_c16 !GCC$ ATTRIBUTES vectorcall :: v16_add_c16 !GCC$ ATTRIBUTES inline :: v16_add_c16
#elif defined __ICC || defined __INTEL_COMPILER
        !DIR$ ATTRIBUTES INLINE :: v16_add_c16
      pure function v16_add_c16(x,y) result(iq)
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: v16_add_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: v16_add_c16
#endif
        use mod_vecconsts, only : v16_n0
        type(ZMM16r4_t),      intent(in) :: x
        type(AVX512c16f32_t), intent(in) :: y
#if defined __INTEL_COMPILER 
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(AVX512c16f32_t) :: iq
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        type(AVX512c16f32_t) :: iq !GCC$ ATTRIBUTES aligned(64) :: iq
#endif
        iq.re = x.v+y.re
        iq.im = v16_n0
      end function v16_add_c16

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      pure function s1_add_c16(x,y) result(iq) !GCC$ ATTRIBUTES hot :: s1_add_c16 !GCC$ ATTRIBUTES vectorcall :: s1_add_c16 !GCC$ ATTRIBUTES inline :: s1_add_c16
#elif defined __ICC || defined __INTEL_COMPILER
        !DIR$ ATTRIBUTES INLINE :: s1_add_c16
      pure function s1_add_c16(x,y) result(iq)
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: s1_add_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: s1_add_c16
#endif
         use mod_vecconsts, only : v16_n0
        real(kind=sp),         intent(in) :: x
        type(AVX512c16f32_t),  intent(in) :: y
#if defined __INTEL_COMPILER 
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(AVX512c16f32_t) :: iq
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        type(AVX512c16f32_t) :: iq !GCC$ ATTRIBUTES aligned(64) :: iq
#endif
        iq.re = s1
        iq.im = v16_n0
      end function s1_add_c16

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      pure function c16_sub_c16(x,y) result(iq) !GCC$ ATTRIBUTES hot :: c16_sub_c16 !GCC$ ATTRIBUTES vectorcall :: c16_sub_c16 !GCC$ ATTRIBUTES inline :: c16_sub_c16
#elif defined __ICC || defined __INTEL_COMPILER
        !DIR$ ATTRIBUTES INLINE :: c16_sub_c16
      pure function c16_sub_c16(x,y) result(iq)
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_sub_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c16_sub_c16
#endif
        type(AVX512c16f32_t),     intent(in) :: x
        type(AVX512c16f32_t),     intent(in) :: y
#if defined __INTEL_COMPILER 
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(AVX512c16f32_t) :: iq
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        type(AVX512c16f32_t) :: iq !GCC$ ATTRIBUTES aligned(64) :: iq
#endif
        iq.re = x.re-y.re
        iq.im = x.im-y.im
      end function c16_sub_c16
        
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER        
      pure function c16_sub_c1(x,y) result(iq) !GCC$ ATTRIBUTES hot :: c16_sub_c1 !GCC$ ATTRIBUTES vectorcall :: c16_sub_c1 !GCC$ ATTRIBUTES inline :: c16_sub_c1
#elif defined __ICC || defined __INTEL_COMPILER
         !DIR$ ATTRIBUTES INLINE :: c16_sub_c1
      pure function c16_sub_c1(x,y) result(iq)
         !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_sub_c1
         !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c16_sub_c1
#endif
        type(AVX512c16f32_t),    intent(in) :: x
        complex(kind=sp),        intent(in) :: y
#if defined __INTEL_COMPILER 
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(AVX512c16f32_t) :: iq
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        type(AVX512c16f32_t) :: iq !GCC$ ATTRIBUTES aligned(64) :: iq
#endif
        iq.re = x.re-real(y,kind=sp)
        iq.im = x.im-aimag(y,kind=sp)
      end function c16_sub_c1

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      pure function c16_sub_v16(x,y) result(iq) !GCC$ ATTRIBUTES hot :: c16_sub_v16 !GCC$ ATTRIBUTES vectorcall :: c16_sub_v16 !GCC$ ATTRIBUTES inline :: c16_sub_v16
#elif defined __ICC || defined __INTEL_COMPILER
        !DIR$ ATTRIBUTES INLINE :: c16_sub_v16
      pure function c16_sub_v16(x,y) result(iq)
            use mod_vecconsts, only : v16_n0
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_sub_v16
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c16_sub_v16
#endif
        type(AVX512c16f32_t),     intent(in) :: x
        type(ZMM16r4_t),          intent(in) :: y
#if defined __INTEL_COMPILER 
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(AVX512c16f32_t) :: iq
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        type(AVX512c16f32_t) :: iq !GCC$ ATTRIBUTES aligned(64) :: iq
#endif
        iq.re = x.re-y.v
        iq.im = v16_n0
      end function c16_sub_v16

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      pure function c16_sub_s1(x,y) result(iq) !GCC$ ATTRIBUTES hot :: c16_sub_s1 !GCC$ ATTRIBUTES inline :: c16_sub_s1
#elif defined __ICC || defined __INTEL_COMPILER
        !DIR$ ATTRIBUTES INLINE :: c16_sub_s1
      pure function c16_sub_s1(x,y) result(iq)
        use mod_vecconsts, only : v16_n0
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_sub_s1
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c16_sub_s1
#endif
        type(AVX512c16f32_t),    intent(in) :: x
        real(kind=sp),           intent(in) :: y
#if defined __INTEL_COMPILER 
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(AVX512c16f32_t) :: iq
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        type(AVX512c16f32_t) :: iq !GCC$ ATTRIBUTES aligned(64) :: iq
#endif
        iq.re = x.re-s1
        iq.im = v16_n0
      end function c16_sub_s1

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      pure function c1_sub_c16(x,y) result(iq) !GCC$ ATTRIBUTES hot :: c1_sub_c16 !GCC$ ATTRIBUTES vectorcall :: c1_sub_c16 !GCC$ ATTRIBUTES inline :: c1_sub_c16
#elif defined __ICC || defined __INTEL_COMPILER
        !DIR$ ATTRIBUTES INLINE :: c1_sub_c16
      pure function c1_sub_c16(x,y) result(iq)
         !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c1_sub_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c1_sub_c16  
#endif
        complex(kind=sp),     intent(in) :: x
        type(AVX512c16f32_t), intent(in) :: y
#if defined __INTEL_COMPILER 
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(AVX512c16f32_t) :: iq
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        type(AVX512c16f32_t) :: iq !GCC$ ATTRIBUTES aligned(64) :: iq
#endif
        iq.re = real(x,kind=sp)-y.re
        iq.im = aimag(x,kind=sp)-y.im
      end function c1_sub_c16

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      pure function v16_sub_c16(x,y) result(iq) !GCC$ ATTRIBUTES hot :: v16_sub_c16 !GCC$ ATTRIBUTES vectorcall :: v16_sub_c16 !GCC$ ATTRIBUTES inline :: v16_sub_c16
#elif defined __ICC || defined __INTEL_COMPILER
        !DIR$ ATTRIBUTES INLINE :: v16_sub_c16
        pure function v16_sub_c16(x,y) result(iq)
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: v16_sub_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: v16_sub_c16
#endif
          use mod_vecconsts, only : v16_n0
          type(ZMM16r4_t),      intent(in) :: x
          type(AVX512c16f32_t), intent(in) :: y
#if defined __INTEL_COMPILER 
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(AVX512c16f32_t) :: iq
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        type(AVX512c16f32_t) :: iq !GCC$ ATTRIBUTES aligned(64) :: iq
#endif
        iq.re = x.v-y.re
        iq.im = v16_n0
      end function v16_sub_c16

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      pure function s1_sub_c16(x,y) result(iq) !GCC$ ATTRIBUTES hot :: s1_sub_c16 !GCC$ ATTRIBUTES vectorcall :: s1_sub_c16 !GCC$ ATTRIBUTES inline :: s1_sub_c16
#elif defined __ICC || defined __INTEL_COMPILER
        !DIR$ ATTRIBUTES INLINE :: s1_sub_c16
      pure function s1_sub_c16(x,y) result(iq)
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: s1_sub_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: s1_sub_c16
        
#endif
         use mod_vecconsts, only : v16_n0
        real(kind=sp),           intent(in) :: x
        type(AVX512c16f32_t),    intent(in) :: y
#if defined __INTEL_COMPILER 
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(AVX512c16f32_t) :: iq
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        type(AVX512c16f32_t) :: iq !GCC$ ATTRIBUTES aligned(64) :: iq
#endif
        iq.re = x-y.re
        iq.im = v16_n0
      end function s1_sub_c16

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      pure function c16_mul_c16(x,y) result(iq) !GCC$ ATTRIBUTES hot :: c16_mul_c16 !GCC$ ATTRIBUTES vectorcall :: c16_mul_c16 !GCC$ ATTRIBUTES inline :: c16_mul_c16
#elif defined __ICC || defined __INTEL_COMPILER
        !DIR$ ATTRIBUTES INLINE :: c16_mul_c16
      pure function c16_mul_c16(x,y) result(uq)
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_sub_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c16_sub_c16
#endif
        type(AVX512c16f32_t),   intent(in) :: x
        type(AVX512c16f32_t),   intent(in) :: y
#if defined __INTEL_COMPILER 
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(AVX512c16f32_t) :: iq
        !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0,zmm1,zmm2,zmm3
        type(ZMM16r4_t), automatic :: zmm0,zmm1,zmm2,zmm3
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        type(AVX512c16f32_t) :: iq !GCC$ ATTRIBUTES aligned(64) :: iq
        type(ZMM16r4_t), automatic :: zmm0 !GCC$ ATTRIBUTES aligned(64) :: zmm0
        type(ZMM16r4_t), automatic :: zmm1 !GCC$ ATTRIBUTES aligned(64) :: zmm1
        type(ZMM16r4_t), automatic :: zmm2 !GCC$ ATTRIBUTES aligned(64) :: zmm2
        type(ZMM16r4_t), automatic :: zmm3 !GCC$ ATTRIBUTES aligned(64) :: zmm3
#endif
       zmm0.v = x.re*y.re
       zmm1.v = x.im*y.im
       iq.re  = zmm0.v+zmm1.v
       zmm2.v = x.im*y.re
       zmm3.v = x.re*y.im
       iq.im  = zmm2.v-zmm3.v
     end function c16_mul_c16
    
        
      
        
      
end module mod_avx512c16f32
