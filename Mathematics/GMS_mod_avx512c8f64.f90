

module mod_avx512c8f64

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_avx512c8f64
 !          
 !          Purpose:
 !                      This module contains a decomposed to real and imaginary
 !                      parts a complex vector of 8 elements (complex numbers)
 !                      This representation nicely fits into 2 AVX-512 ZMMx
 !                      registers.
 !          History:
 !                        Date: 03-11-2019
 !                        Time: 11:17 GMT+2
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

     use module_kinds, only : int1,int4,dp
     use mod_vectypes, only : ZMM8c8_t,ZMM8r8_t
     use,  intrinsic :: ISO_C_BINDING
     implicit none
     !=====================================================59
     !  File and module information:
     !  version,creation and build date, author,description
     !=====================================================59

     ! Major version
     integer(kind=int4),  parameter, public :: MOD_AVX512C8F64_MAJOR = 1
     ! Minor version
     integer(kind=int4),  parameter, public :: MOD_AVX512C8F64_MINOR = 0
     ! Micro version
     integer(kind=int4),  parameter, public :: MOD_AVX512C8F64_MICRO = 0
     ! Full version
     integer(kind=int4),  parameter, public :: MOD_AVX512C8F64_FULLVER =   &
            1000*MOD_AVX512C8F64_MAJOR+100*MOD_AVX512C8F64_MINOR+10*MOD_AVX512C8F64
     ! Module creation date
     character(*),        parameter, public :: MOD_AVX512C8F64_CREATE_DATE = "03-11-2019 15:54 +00200 (SUN 03 NOV 2019 GMT+2)"
     ! Module build date
     character(*),        parameter, public :: MOD_AVX512C8F64_BUILD_DATE  = "00-00-0000 00:00"
     ! Module author info
     character(*),        parameter, public :: MOD_AVX512C8F64_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter, public :: MOD_AVX512C8F64_SYNOPSIS    = "Packed complex vector of 8 elements (complex numbers)"

     ! public operators

     public :: operator(+)
     public :: operator(-)
     public :: operator(*)
     public :: operator(/)
     public :: operator(==)
     public :: operator(/=)
     public :: operator(>)
     public :: operator(<)
     public :: operator(>=)
     public :: operator(<=)
     public :: operator .conjugate.

     type, public :: AVX512c8f64_t
        !
        sequence
        type(ZMM8c8_t) :: IQvec8
     end type AVX512C8f64_t  
       
        
        
        
      
        
    

     interface operator (+)
         module procedure c8_add_c8
         module procedure c8_add_c2
         module procedure c8_add_v8
         module procedure c8_add_s1
         module procedure c2_add_c8
         module procedure v8_add_c8
         module procedure s1_add_c8
     end interface operator (+)

     interface operator (-)
         module procedure c8_sub_c8
         module procedure c8_sub_c2
         module procedure c8_sub_v8
         module procedure c8_sub_s1
         module procedure c2_sub_c8
         module procedure v8_sub_c8
         module procedure s1_sub_c8
      end interface operator (-)
      
     interface operator (*)
         module procedure c8_mul_c8
         module procedure c8_mul_c2
         module procedure c8_mul_v8
         module procedure c8_mul_s1
         module procedure c2_mul_c8
         module procedure v8_mul_c8
         module procedure s1_mul_c8
      end interface operator (*)

      interface operator (/)
         module procedure c8_div_c8
         module procedure c8_div_c2
         module procedure c8_div_v8
         module procedure c8_div_s1
         module procedure c2_div_c8
         module procedure v8_div_c8
         module procedure s1_div_c8
      end interface operator (/)

      interface operator (==)
         module procedure c8_eq_c8
         module procedure c8_eq_c2
         module procedure c8_eq_v8
         module procedure c2_eq_c8
         module procedure v8_eq_c8
      end interface operator (==)

      interface operator (/=)
         module procedure c8_neq_c8
         module procedure c8_neq_c2
         module procedure c8_neq_v8
         module procedure c2_neq_c8
         module procedure v8_neq_c8
      end interface operator (/=)

      interface operator (>)
         module procedure c8_gt_c8
         module procedure c8_gt_c2
         module procedure c8_gt_v8
         module procedure c2_gt_c8
         module procedure v8_gt_c8 
      end interface operator (>)

      interface operator (<)
         module procedure c8_lt_c8
         module procedure c8_lt_c2
         module procedure c8_lt_v8
         module procedure c2_lt_c8
         module procedure v8_lt_c8
      end interface operator (<)

      interface operator (>=)
         module procedure c8_ge_c8
         module procedure c8_ge_c2
         module procedure c8_ge_v8
         module procedure c2_ge_c8
         module procedure v8_ge_c8
      end interface operator (>=)

      interface operator (<=)
         module procedure c8_le_c8
         module procedure c8_le_c2
         module procedure c8_le_v8
         module procedure c2_le_c8
         module procedure v8_le_c8
      end interface operator (<=)

      contains
!DIR$ ATTRIBUTES INLINE :: default_init
     pure   function default_init() result(iq)
          !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: default_init
          !DIR$ ATTRIBUTES VECTOR :: default_init 
          !DIR$ ATTRIBUTES ALIGN : 64 :: iq
          type(AVX512c8f64_t) :: iq
          ! Exec code
          iq.IQvec8.re = 0.0_dp
          iq.IQvec8.im = 0.0_dp
        end function default_init

!DIR$ ATTRIBUTES INLINE :: array_init
     pure   function array_init(re,im) result(iq)
          !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: array_init
          !DIR$ ATTRIBUTES VECTOR :: array_init
          real(kind=dp), dimension(0:7), intent(in) :: re
          real(kind=dp), dimension(0:7), intent(in) :: im
          !DIR$ ATTRIBUTES ALIGN : 64 :: iq
          type(AVX512c8f64_t) :: iq
          ! EXec code ....
          iq.IQvec8.re = re
          iq.IQvec8.im = im
       end function array_init

!DIR$ ATTRIBUTES INLINE :: complex1_init
     pure   function complex1_init(c) result(iq)
          !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: complex1_init
          complex(kind=dp), intent(in) :: c
          !DIR$ ATTRIBUTES ALIGN : 64 :: iq
          type(AVX512c8f64_t) :: iq
          ! Exec code ...
          iq.IQvec8.re = real(c,kind=dp)
          iq.IQvec8.im = aimag(c,kind=dp)
     end function complex1_init

!DIR$ ATTRIBUTES INLINE :: complex2x8_init
     pure   function complex2x8_init(c) result(iq)
         !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: complex2x8_init
         complex(kind=dp), dimension(0:7), intent(in) :: c
         !DIR$ ATTRIBUTES ALIGN : 64 :: iq
         type(AVX512c8f64_t) :: iq
         ! Exec code ....
         iq.IQvec8.re = real(c,kind=dp)
         iq.IQvec8.im = aimag(c,kind=dp)
     end function complex2x8_init

!DIR$ ATTRIBUTES INLINE :: zmm8r82x_init       
     pure  function zmm8r82x_init(v1,v2) result(iq)
         !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: zmm8r82x_init
         !DIR$ ATTRIBUTES VECTOR :: zmm8r82x_init
         type(ZMM8r8_t),  intent(in) :: v1
         type(ZMM8r8_t),  intent(in) :: v2
         !DIR$ ATTRIBUTES ALIGN : 64 :: iq
         type(AVX512c8f64_t) :: iq
         ! EXec code ....
         iq.IQvec8.re = v1.v
         iq.IQvec8.im = v2.v
     end function zmm8r82x_init

!DIR$ ATTRIBUTES INLINE :: zmm8r81x_init
     pure function zmm8r81x_init(v1) result(iq)
         !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: zmm8r81x_init
         !DIR$ ATTRIBUTES VECTOR :: zmm8r81x_init
         type(ZMM8r8_t),  intent(in) :: v1
         !DIR$ ATTRIBUTES ALIGN : 64 :: iq
         type(AVX512c8f64_t) :: iq
         ! Exec code ....
         iq.IQvec8.re = v1.v
         iq.IQvec8.im = 0.0_dp
     end function zmm8r81x_init

!DIR$ ATTRIBUTES INLINE :: r81x_init
     pure function r81x_init(s) result(iq)
         !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: r81x_init
         !DIR$ ATTRIBUTES VECTOR :: r81x_init
         real(kind=dp),  intent(in) :: s
         !DIR$ ATTRIBUTES ALIGN : 64 :: iq
         type(AVX512c8f64_t) :: iq
         ! EXec code ....
         iq.IQvec8.re = s
         iq.IQvec8.im = 0.0_dp
     end function r81x_init

!DIR$ ATTRIBUTES INLINE ::  copy_init
     pure function copy_init(rhs) result(iq)
         !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: copy_init
         !DIR$ ATTRIBUTES VECTOR :: copy_init
         type(AVX512c8f64_t),  intent(in) :: rhs
         !DIR$ ATTRIBUTES ALIGN : 64 :: iq
         type(AVX512c8f64_t) :: iq
         ! EXec code ...
         iq.IQvec8 = rhs.IQvec8
     end function copy_init

!DIR$ ATTRIBUTES INLINE :: polar
     pure elemental function polar(rho,theta) result (iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: polar
       !DIR$ ATTRIBUTES VECTOR :: polar
       type(ZMM8r8_t), intent(in) :: rho
       type(ZMM8r8_t), intent(in) :: theta
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(AVX512c8f64_t) :: iq
       ! EXec code ....
       iq.IQvec8.re = rho.v*cos(theta.v)
       iq.IQvec8.im = rho.v*sin(theta.v) 
     end function polar
!DIR$ ATTRIBUTES INLINE :: carg_zmm8c8
     pure elemental function carg_zmm8c8(c8) result(arg)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: carg_zmm8c8
       !DIR$ ATTRIBUTES VECTOR :: carg_zmm8c8
       type(AVX512c8f64_t),   intent(in) :: c8
       !DIR$ ATTRIBUTES ALIGN : 64 :: arg
       type(ZMM8r8_t) :: arg
       ! EXec code ....
       arg.v  = atan2(c8.IQvec8.im,c8.IQvec8.re)
       
     end function carg_zmm8c8

!DIR$ ATTRIBUTES INLINE :: carg_zmm8r8     
     pure elemental function carg_zmm8r8(re,im) result(arg)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: carg_zmm8r8
       !DIR$ ATTRIBUTES VECTOR :: carg_zmm8r8
       type(ZMM8r8_t),  intent(in) :: re
       type(ZMM8r8_t),  intent(in) :: im
       !DIR$ ATTRIBUTES ALIGN : 64 :: arg
       type(ZMM8r8_t) :: arg
       ! EXec code ....
       arg.v = atan2(im.v,re.v)
     end function carg_zmm8r8

!DIR$ ATTRIBUTES INLINE :: csin_zmm8c8
     pure elemental function csin_zmm8c8(c8) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: csin_zmm8c8
       !DIR$ ATTRIBUTES VECTOR :: csin_zmm8c8
       type(AVX512c8f64_t),  intent(in) :: c8
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(AVX512c8f64_t) :: iq
       !DIR$ ATTRIBUTES ALIGN : 64 :: tre,tim
       type(ZMM8r8_t) :: tre
       type(ZMM8r8_t) :: tim
       ! Exec code ....
       tre = c8.IQvec8.re
       tim = c8.IQvec8.im
       iq.IQvec8.re = sin(tre.v)*cosh(tim.v)
       iq.IQvec8.im = cos(tre.v)*sinh(tim.v)
     end function csin_zmm8c8

!DIR$ ATTRIBUTES INLINE :: csin_zmm8r8
     pure elemental function csin_zmm8r8(re,im) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: csin_zmm8r8
       !DIR$ ATTRIBUTES VECTOR :: csin_zmm8r8
       type(ZMM8r8_t),   intent(in) :: re
       type(ZMM8r8_t),   intent(in) :: im
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(AVX512c8f64_t) :: iq
       ! Exec code ...
       iq.IQvec8.re = sin(re.v)*cosh(im.v)
       iq.IQvec8.im = cos(re.v)*sinh(im.v)
     end function csin_zmm8r8

!DIR$ ATTRIBUTES INLINE :: csinh_zmm8c8
     pure elemental function csinh_zmm8c8(c8) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: csinh_zmm8c8
       !DIR$ ATTRIBUTES VECTOR :: csinh_zmm8c8
       type(AVX512c8f64_t),   intent(in) :: c8
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(AVX512c8f64_t) :: iq
       !DIR$ ATTRIBUTES ALIGN : 64 :: tre,tim
       type(ZMM8r8_t) :: tre
       type(ZMM8r8_t) :: tim
       ! EXec code ....
       tre = c8.IQvec8.re
       tim = c8.IQvec8.im
       iq.IQvec8.re = sinh(tre.v)*cos(tim.v)
       iq.IQvec8.im = cosh(tre.v)*sin(tim.v)
     end function csinh_zmm8c8

!DIR$ ATTRIBUTES INLINE :: csinh_zmm8r8
     pure elemental function csinh_zmm8r8(re,im) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: csinh_zmm8r8
       !DIR$ ATTRIBUTES VECTOR :: csinh_zmm8r8
       type(ZMM8r8_t), intent(in) :: re
       type(ZMM8r8_t), intent(in) :: im
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(AVX512c8f64_t) :: iq
       ! EXec code ....
       iq.IQvec8.re = sinh(re.v)*cos(im.v)
       iq.IQvec8.im = cosh(re.v)*sin(im.v
     end function csinh_zmm8r8

!DIR$ ATTRIBUTES INLINE :: ccos_zmm8c8
     pure elemental function ccos_zmm8c8(c8) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ccos_zmm8c8
       !DIR$ ATTRIBUTES VECTOR :: ccos_zmm8c8
       type(AVX512c8f64_t),   intent(in) :: c8
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(AVX512c8f64_t) :: iq
       !DIR$ ATTRIBUTES ALIGN : 64 :: tre,tim
       type(ZMM8r8_t) :: tre
       type(ZMM8r8_t) :: tim
       ! EXec code ....
       tre = c8.IQvec8.re
       tim = c8.IQvec8.im
       iq.IQvec8.re = cos(tre.v)*cosh(tim.v)
       iq.IQvec8.im = sin(tre.v)*sinh(tim.v)
     end function ccos_zmm8c8

!DIR$ ATTRIBUTES INLINE :: ccos_zmm8r8
     pure elemental function ccos_zmm8r8(re,im) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ccos_zmm8r8
       !DIR$ ATTRIBUTES VECTOR :: ccos_zmm8r8
       type(ZMM8r8_t), intent(in) :: re
       type(ZMM8r8_t), intent(in) :: im
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(AVX512c8f64_t) :: iq
       ! EXec code .....
       iq.IQvec8.re = cos(re.v)*cosh(im.v)
       iq.IQvec8.im = sin(re.v)*sinh(im.v)
     end function ccos_zmm8r8  
end module mod_avx512c8f64
