

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
     use mod_vectypes, only : ZMM8r8_t
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
     character(*),        parameter, public :: MOD_AVX512C8F64_BUILD_DATE  = __DATE__ " " __TIME__
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
        real(kind=dp), dimension(0:7) :: re
        real(kind=dp), dimension(0:7) :: im
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
          !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: default_init
          !DIR$ ATTRIBUTES VECTOR :: default_init 
          !DIR$ ATTRIBUTES ALIGN : 64 :: iq
          type(AVX512c8f64_t) :: iq
          ! Exec code
          iq.re = 0.0_dp
          iq.im = 0.0_dp
        end function default_init

!DIR$ ATTRIBUTES INLINE :: array_init
     pure   function array_init(re,im) result(iq)
          !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: array_init
          !DIR$ ATTRIBUTES VECTOR :: array_init
          real(kind=dp), dimension(0:7), intent(in) :: re
          real(kind=dp), dimension(0:7), intent(in) :: im
          !DIR$ ATTRIBUTES ALIGN : 64 :: iq
          type(AVX512c8f64_t) :: iq
          ! EXec code ....
          iq.re = re
          iq.im = im
       end function array_init

!DIR$ ATTRIBUTES INLINE :: complex1_init
     pure   function complex1_init(c) result(iq)
          !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: complex1_init
          complex(kind=dp), intent(in) :: c
          !DIR$ ATTRIBUTES ALIGN : 64 :: iq
          type(AVX512c8f64_t) :: iq
          ! Exec code ...
          iq.re = real(c,kind=dp)
          iq.im = aimag(c,kind=dp)
     end function complex1_init

!DIR$ ATTRIBUTES INLINE :: complex2x8_init
     pure   function complex2x8_init(c) result(iq)
         !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: complex2x8_init
         complex(kind=dp), dimension(0:7), intent(in) :: c
         !DIR$ ATTRIBUTES ALIGN : 64 :: iq
         type(AVX512c8f64_t) :: iq
         ! Exec code ....
         iq.re = real(c,kind=dp)
         iq.im = aimag(c,kind=dp)
     end function complex2x8_init

!DIR$ ATTRIBUTES INLINE :: zmm8r82x_init       
     pure  function zmm8r82x_init(v1,v2) result(iq)
         !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: zmm8r82x_init
         !DIR$ ATTRIBUTES VECTOR :: zmm8r82x_init
         type(ZMM8r8_t),  intent(in) :: v1
         type(ZMM8r8_t),  intent(in) :: v2
         !DIR$ ATTRIBUTES ALIGN : 64 :: iq
         type(AVX512c8f64_t) :: iq
         ! EXec code ....
         iq.re = v1.v
         iq.im = v2.v
     end function zmm8r82x_init

!DIR$ ATTRIBUTES INLINE :: zmm8r81x_init
     pure function zmm8r81x_init(v1) result(iq)
         !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: zmm8r81x_init
         !DIR$ ATTRIBUTES VECTOR :: zmm8r81x_init
         type(ZMM8r8_t),  intent(in) :: v1
         !DIR$ ATTRIBUTES ALIGN : 64 :: iq
         type(AVX512c8f64_t) :: iq
         ! Exec code ....
         iq.re = v1.v
         iq.im = 0.0_dp
     end function zmm8r81x_init

!DIR$ ATTRIBUTES INLINE :: r81x_init
     pure function r81x_init(s) result(iq)
         !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: r81x_init
         !DIR$ ATTRIBUTES VECTOR :: r81x_init
         real(kind=dp),  intent(in) :: s
         !DIR$ ATTRIBUTES ALIGN : 64 :: iq
         type(AVX512c8f64_t) :: iq
         ! EXec code ....
         iq.re = s
         iq.im = 0.0_dp
     end function r81x_init

!DIR$ ATTRIBUTES INLINE ::  copy_init
     pure function copy_init(rhs) result(iq)
         !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: copy_init
         !DIR$ ATTRIBUTES VECTOR :: copy_init
         type(AVX512c8f64_t),  intent(in) :: rhs
         !DIR$ ATTRIBUTES ALIGN : 64 :: iq
         type(AVX512c8f64_t) :: iq
         ! EXec code ...
         iq = rhs
     end function copy_init

!DIR$ ATTRIBUTES INLINE :: c8_add_c8
     pure elemental function c8_add_c8(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_add_c8
       !DIR$ ATTRIBUTES VECTOR :: c8_add_c8
       type(AVX512c8f64_t),  intent(in) :: lhs
       type(AVX512c8f64_t),  intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(AVX512c8f64_t) :: iq
       ! EXec code ....
       iq.re = lhs.re+rhs.re
       iq.im = lhs.im+rhs.im
     end function c8_add_c8

!DIR$ ATTRIBUTES INLINE :: c8_add_c2
     pure elemental function c8_add_c2(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_add_c2
       !DIR$ ATTRIBUTES VECTOR :: c8_add_c2
       type(AVX512c8f64_t),  intent(in) :: lhs
       complex(kind=dp),     intent(in), value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(AVX512c8f64_t) :: iq
       ! EXec code ....
       iq.re = lhs.re+real(rhs,kind=dp)
       iq.im = lhs.im+aimag(rhs,kind=dp)
     end function c8_add_c2

!DIR$ ATTRIBUTES INLINE :: c8_add_v8
     pure elemental function c8_add_v8(lhs,rhs) result(iq)
       use mod_vecconsts, only : v8_n0
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_add_v8
       !DIR$ ATTRIBUTES VECTOR :: c8_add_v8
       type(AVX512c8f64_t),   intent(in) :: lhs
       type(ZMM8r8_t),        intent(in),value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(AVX512c8f64_t) :: iq
       !Exec code....
       iq.re = lhs.re+rhs.v
       iq.im = v8_n0
     end function c8_add_v8

!DIR$ ATTRIBUTES INLINE :: c8_add_s1
     pure elemental function c8_add_s1(lhs,rhs) result(iq)
       use mod_vecconsts, only : v8_n0
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_add_s1
       !DIR$ ATTRIBUTES VECTOR :: c8_add_s1
       type(AVX512c8f64_t),   intent(in) :: lhs
       real(kind=dp),         intent(in), value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(AVX512c8f64_t) :: iq
       iq.re = lhs.re+rhs
       iq.im = v8_n0
     end function c8_add_s1

!DIR$ ATTRIBUTES INLINE :: c2_add_c8     
     pure elemental function c2_add_c8(lhs,rhs) result(iq)
       !
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c2_add_c8
       !DIR$ ATTRIBUTES VECTOR :: c2_add_c8
       complex(kind=dp),     intent(in) :: lhs
       type(AVX512c8f64_t),  intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(AVX512c8f64_t) :: iq
       iq.re = real(lhs,kind=dp)+rhs.re
       iq.im = aimag(lhs,kind=dp)+rhs.im
     end function c2_add_c8

!DIR$ ATTRIBUTES INLINE v8_add_c8
     pure elemental function v8_add_c8(lhs,rhs) result(iq)
       use mod_vecconsts, only : v8_n0
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: v8_add_c8
       !DIR$ ATTRIBUTES VECTOR :: v8_add_c8
       type(ZMM8r8_t),      intent(in) :: lhs
       type(AVX512c8f64_t), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(AVX512c8f64_t) :: iq
       iq.re = lhs.v+rhs.re
       iq.im = v8_n0.v
     end function v8_add_c8

!DIR$ ATTRIBUTES INLINE :: s1_add_c8
     pure elemental function s1_add_c8(lhs,rhs) result(iq)
       use mod_vecconsts, only : v8_n0
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: s1_add_c8
       !DIR$ ATTRIBUTES VECTOR :: s1_add_c8
       real(kind=dp),       intent(in) :: lhs
       type(AVX512c8f64_t), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(AVX512c8f64_t) :: iq
       iq.m_re = lhs+rhs.m_re
       iq.m_im = v8_n0.v
     end function s1_add_c8

!DIR$ ATTRIBUTES INLINE :: c8_sub_c8
     pure elemental function c8_add_c8(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_sub_c8
       !DIR$ ATTRIBUTES VECTOR :: c8_sub_c8
       type(AVX512c8f64_t),    intent(in) :: lhs
       type(AVX512c8f64_t),    intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(AVX512c8f64_t) :: iq
       iq.re = lhs.re-rhs.re
       iq.im = lhs.im-rhs.im
     end function c8_add_c8

!DIR$ ATTRIBUTES INLINE :: c8_sub_c2
     pure elemental function c8_sub_c2(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_sub_c2
       !DIR$ ATTRIBUTES VECTOR :: c8_sub_c2
       type(AVX512c8f64_t),     intent(in) :: lhs
       complex(kind=dp),        intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(AVX512c8f64_t) :: iq
       iq.re = lhs.re-real(rhs,kind=dp)
       iq.im = lhs.im-aimag(rhs,kind=dp)
     end function c8_sub_c2

!DIR$ ATTRIBUTES INLINE :: c8_sub_v8
     pure elemental function c8_sub_v8(lhs,rhs) result(iq)
       use mod_vecconsts, only v8_n0
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_sub_v8
       !DIR$ ATTRIBUTES VECTOR :: c8_sub_v8
       type(AVX512c8f64_t),    intent(in) :: lhs
       type(ZMM8r8_t),         intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(AVX512c8f64_t) :: iq
       iq.re = lhs.re-rhs.v
       iq.im = v8_n0.v
     end function c8_sub_v8

!DIR$ ATTRIBUTES INLINE :: c8_sub_s1
     pure elemental function c8_sub_s1(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_sub_s1
       !DIR$ ATTRTIBUTES VECTOR :: c8_sub_s1
       type(AVX512c8f64_t),     intent(in) :: lhs
       real(kind=dp),           intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(AVX512c8f64_t) :: iq
       iq.re = lhs.re-rhs
       iq.im = 0.0_dp
     end function c8_sub_s1

!DIR$ ATTRIBUTES INLINE :: c2_sub_c8
     pure elemental function c2_sub_c8(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c2_sub_c8
       !DIR$ ATTRIBUTES VECTOR :: c2_sub_c8
       complex(kind=dp),      intent(in) :: lhs
       type(AVX512c8f64_t),   intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(AVX512c8f64_t) :: iq
       iq.re = real(lhs,kind=dp)-rhs.re
       iq.im = aimag(lhs,kind=dp)-rhs.im
     end function c2_sub_c8

!DIR$ ATTRIBUTES INLINE :: v8_sub_c8
     pure elemental function v8_sub_c8(lhs,rhs) result(iq)
       use mod_vecconsts, only : v8_n0
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: v8_sub_c8
       !DIR$ ATTRIBUTES VECTOR :: v8_sub_c8
       type(ZMM8r8_t),      intent(in) :: lhs
       type(AVX512c8f64_t), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(AVX512c8f64_t) :: iq
       iq.re = lhs.v-rhs.re
       iq.im = v8_n0.v
     end function v8_sub_c8

!DIR$ ATTRIBUTES INLINE :: s1_sub_c8
     pure elemental function s1_sub_c8(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: s1_sub_c8
       !DIR$ ATTRIBUTES VECTOR :: s1_sub_c8
       real(kind=dp),       intent(in) :: lhs
       type(AVX512c8f64_t), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(AVX512c8f64_t) :: iq
       iq.re = lhs-rhs.re
       iq.im = 0.0_dp
     end function s1_sub_c8

!DIR$ ATTRIBUTES INLINE :: c8_mul_c8
     pure elemental function c8_mul_c8(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c8_mul_c8
       !DIR$ ATTRIBUTES VECTOR :: c8_mul_c8
       type(AVX512c8f64_t),    intent(in) :: lhs
       type(AVX512c8f64_t),    intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(AVX512c8f64_t) :: iq
       !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0,zmm1,zmm2,zmm3
       type(ZMM8r8_t) :: zmm0,zmm41,zmm2,zmm3
       zmm0.v = lhs.re*rhs.re
       zmm1.v = lhs.im*rhs.im
       iq.re  = zmm0.v-zmm1.v
       zmm2.v = lhs.im*rhs.re
       zmm3.v = lhs.re*rhs.im
       iq.im  = zmm2.v-zmm3.v
     end function c8_mul_c8

!DIR$ ATTRIBUTES INLINE :: c8_mul_c2
     pure elemental function c8_mul_c2(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c8_mul_c2
       !DIR$ ATTRIBUTES VECTOR :: c8_mul_c2
       type(AVX512c8f64_t),     intent(in) :: lhs
       complex(kind=dp),        intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(AVX512c8f64_t) :: iq
       !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0,zmm1,zmm2,zmm3
       type(ZMM8r8_t) :: zmm0,zmm1,zmm2,zmm3
       zmm0.v = lhs.re*real(rhs,kind=dp)
       zmm1.v = lhs.im*aimag(rhs,kind=dp)
       iq.re  = zmm0.v-zmm1.v
       zmm2.v = lhs.im*real(rhs,kind=dp)
       zmm3.v = lhs.re*aimag(rhs,kind=dp)
       iq.im  = zmm2.v-zmm3.v
     end function c8_mul_c2

!DIR$ ATTRIBUTES INLINE :: c8_mul_v8
     pure elemental function c8_mul_v8(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_mul_v8
       !DIR$ ATTRIBUTES VECTOR :: c8_mul_v8
       type(AVX512c8f64_t),    intent(in) :: lhs
       type(ZMM8r8_t),         intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(AVX512c8f64_t) :: iq
       iq.re = lhs.re*rhs.v
       iq.im = lhs.im*rhs.v
     end function c8_mul_v8

!DIR$ ATTRIBUTES INLINE :: c8_mul_s1
     pure elemental function c8_mul_s1(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_mul_s1
       !DIR$ ATTRIBUTES VECTOR :: c8_mul_s1
       type(AVX512c8f64_t),    intent(in) :: lhs
       real(kind=dp),          intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(AVX512c8f64_t) :: iq
       iq.re = lhs.re*rhs
       iq.im = lhs.im*rhs
     end function c8_mul_s1

!DIR$ ATTRIBUTES INLINE :: c2_mul_c8
     pure elemental function c2_mul_c8(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c2_mul_c8
       !DIR$ ATTRIBUTES VECTOR :: c2_mul_c8
       complex(kind=dp),        intent(in) :: lhs
       type(AVX512c8f64_t),     intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(AVX512c8f64_t) :: iq
       !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0,zmm1,zmm2,zmm3
       type(ZMM8r8_t) :: zmm0,zmm1,zmm2,zmm3
       zmm0.v = real(lhs,kind=dp)*rhs.re
       zmm1.v = aimag(lhs,kind=dp)*rhs.im
       iq.re  = zmm0.v-zmm1.v
       zmm2.v = real(lhs,kind=dp)*rhs.im
       zmm3.v = aimag(lhs,kind=dp)*rhs.re
       iq.im  = zmm2.v-zmm3.v
     end function c2_mul_c8


     
!DIR$ ATTRIBUTES INLINE :: polar
     pure elemental function polar(rho,theta) result (iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: polar
       !DIR$ ATTRIBUTES VECTOR :: polar
       type(ZMM8r8_t), intent(in) :: rho
       type(ZMM8r8_t), intent(in) :: theta
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(AVX512c8f64_t) :: iq
       ! EXec code ....
       iq.re = rho.v*cos(theta.v)
       iq.im = rho.v*sin(theta.v) 
     end function polar
!DIR$ ATTRIBUTES INLINE :: carg_zmm8c8
     pure elemental function carg_zmm8c8(c8) result(arg)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: carg_zmm8c8
       !DIR$ ATTRIBUTES VECTOR :: carg_zmm8c8
       type(AVX512c8f64_t),   intent(in) :: c8
       !DIR$ ATTRIBUTES ALIGN : 64 :: arg
       type(ZMM8r8_t) :: arg
       ! EXec code ....
       arg.v  = atan2(c8.im,c8.re)
       
     end function carg_zmm8c8

!DIR$ ATTRIBUTES INLINE :: carg_2xzmm8r8     
     pure elemental function carg_2xzmm8r8(re,im) result(arg)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: carg_2xzmm8r8
       !DIR$ ATTRIBUTES VECTOR :: carg_2xzmm8r8
       type(ZMM8r8_t),  intent(in) :: re
       type(ZMM8r8_t),  intent(in) :: im
       !DIR$ ATTRIBUTES ALIGN : 64 :: arg
       type(ZMM8r8_t) :: arg
       ! EXec code ....
       arg.v = atan2(im.v,re.v)
     end function carg_2xzmm8r8

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
       tre = c8.re
       tim = c8.im
       iq.re = sin(tre.v)*cosh(tim.v)
       iq.im = cos(tre.v)*sinh(tim.v)
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
       iq.re = sin(re.v)*cosh(im.v)
       iq.im = cos(re.v)*sinh(im.v)
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
       tre = c8.re
       tim = c8.im
       iq.re = sinh(tre.v)*cos(tim.v)
       iq.im = cosh(tre.v)*sin(tim.v)
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
       iq.re = sinh(re.v)*cos(im.v)
       iq.im = cosh(re.v)*sin(im.v
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
       tre = c8.re
       tim = c8.im
       iq.re = cos(tre.v)*cosh(tim.v)
       iq.im = sin(tre.v)*sinh(tim.v)
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
       iq.re = cos(re.v)*cosh(im.v)
       iq.im = sin(re.v)*sinh(im.v)
     end function ccos_zmm8r8

!DIR$ ATTRIBUTES INLINE :: ccosh_zmm8c8
     pure elemental function ccosh_zmm8c8(c8) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ccosh_zmm8c8
       !DIR$ ATTRIBUTES VECTOR :: ccosh_zmm8c8
       type(AVX512c8f64_t),    intent(in) :: c8
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(AVX512c8f64_t) :: iq
       !DIR$ ATTRIBUTES ALIGN : 64 :: tre,tim
       type(ZMM8r8_t) :: tre
       type(ZMM8r8_t) :: tim
       ! EXec code ....
       tre = c8.re
       tim = c8.im
       iq.re = cosh(tre.v)*cos(tim.v)
       iq.im = sinh(tre.v)*sin(tim.v)
     end function ccosh_zmm8c8

!DIR$ ATTRIBUTES INLINE :: ccosh_zmm8r8
     pure elemental function ccosh_zmm8r8(re,im) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ccosh_zmm8r8
       !DIR$ ATTRIBUTES VECTOR :: ccosh_zmm8r8
       type(ZMM8r8_t),   intent(in) :: re
       type(ZMM8r8_t),   intent(in) :: im
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(AVX512c8f64_t) :: iq
       ! EXec code ....
       iq.re = cosh(re.v)*cos(im.v)
       iq.im = sinh(re.v)*sin(im.v)
     end function ccosh_zmm8r8

!DIR$ ATTRIBUTES INLINE :: cexp_zmm8c8
     pure elemental function cexp_zmm8c8(c8) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cexp_zmm8c8
       !DIR$ ATTRIBUTES VECTOR :: cexp_zmm8c8
       type(AVX512c8f64_t),  intent(in) :: c8
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(AVX512c8f64_t) :: iq
       !DIR$ ATTRIBUTES ALIGN : 64 :: tre,tim
       type(ZMM8r8_t) :: tre
       type(ZMM8r8_t) :: tim
       ! Exec code ....
       tre = c8.re
       tim = c8.im
       iq.re = exp(tre.v)*cos(tim.v)
       iq.im = exp(tre.v)*sin(tim.v)
     end function cexp_zmm8c8

!DIR$ ATTRIBUTES INLINE :: cexp_zmm8r8
     pure elemental function cexp_zmm8r8(re,im) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cexp_zmm8r8
       !DIR$ ATTRIBUTES VECTOR :: cexp_zmm8r8
       type(ZMM8r8_t),  intent(in) :: re
       type(ZMM8r8_t),  intent(in) :: im
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(AVX512c8f64_t) :: iq
       ! Exec code ....
       iq.re = exp(re.v)*cos(im.v)
       iq.im = exp(re.v)*sin(im.v)
     end function cexp_zmm8r8

!DIR$ ATTRIBUTES INLINE :: cabs_zmm8c8
     pure elemental function cabs_zmm8c8(c8) result(val)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cabs_zmm8c8
       !DIR$ ATTRIBUTES VECTOR :: cabs_zmm8c8
       type(AVX512c8f64_t), intent(in) :: c8
       !DIR$ ATTRIBUTES ALIGN : 64 :: val
       type(ZMM8r8_t) :: val
       !DIR$ ATTRIBUTES ALIGN : 64 :: tre,tim
       type(ZMM8r8_t) :: tre,tim
       ! Exec code ...
       tre = c8.re
       tim = c8.im
       val.v = sqrt(tre.v*tre.v+tim.v*tim.v)
     end function cabs_zmm8c8

!DIR$ ATTRIBUTES INLINE :: cabs_2xzmm8r8
     pure elemental function cabs_2xzmm8r8(re,im) result(val)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cabs_2xzmm8r8
       !DIR$ ATTRIBUTES VECTOR :: cabs_2xzmm8r8
       type(ZMM8r8_t),  intent(in) :: re
       type(ZMM8r8_t),  intent(in) :: im
       !DIR$ ATTRIBUTES ALIGN : 64 :: val
       type(ZMM8r8_t) :: val
       ! EXec code ....
       val.v = sqrt(re.v*re.v+im.v*im.v)
     end function cabs_2xzmm8r8

!DIR$ ATTRIBUTES INLINE :: cpow_zmm8c8
     pure elemental function cpow_zmm8c8(c8,n) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cpow_zmm8c8
       !DIR$ ATTRIBUTES VECTOR :: cpow_zmm8c8
       type(AVX512c8f64_t), intent(in) :: c8
       real(kind=dp),       intent(in) :: n
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(AVX512c8f64_t) :: iq
       !DIR$ ATTRIBUTES ALIGN : 64 :: tre,tim
       type(ZMM8r8_t) :: tre,tim
       !DIR$ ATTRIBUTES ALIGN : 64 :: r,theta,pow,trig
       type(ZMM8r8_t) :: r,theta,pow,trig
       !EXec code ....
       tre = c8.re
       tim = c8.im
       r.v = sqrt(tre.v*tre.v+tim.v*tim.v)
       pow.v   = r.v**n
       theta.v = atan(tim.v/tre.v)
       ! 
       trig.v  = theta.v*n
       iq.re = pow.v*cos(trig.v)
       iq.im = pow.v*sin(trig.v)
     end function cpow_zmm8c8

!DIR$ ATTRIBUTES INLINE :: cpow_2xzmm8r8
     pure elemental function cpow_2xzmm8r8(re,im,n) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cpow_2xzmm8r8
       !DIR$ ATTRIBUTES VECTOR :: cpow_2xzmm8r8
       type(ZMM8r8_t),   intent(in) :: re
       type(ZMM8r8_t),   intent(in) :: im
       real(kind=dp),    intent(in) :: n
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(AVX512c8f64_t) :: iq
       !DIR$ ATTRIBUTES ALIGN : 64 :: tre,tim
       type(ZMM8r8_t) :: tre,tim
       !DIR$ ATTRIBUTES ALIGN : 64 :: r,theta,pow,trig
       type(ZMM8r8_t) :: r,theta,pow,trig
       !EXec code ....
       r.v = sqrt(re.v*re.v+im.v*im.v)
       pow.v   = r.v**n
       theta.v = atan(im.v/re.v)
       !
       trig.v  = theta.v*n
       iq.re = pow.v*cos(trig.v)
       iq.im = pow.v*sin(trig.v)
     end function cpow_2xzmm8r8

!DIR$ ATTRIBUTES INLINE :: clog_zmm8c8    
     pure elemental function clog_zmm8c8(c8) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: clog_zmm8c8
       !DIR$ ATTRIBUTES VECTOR :: clog_zmm8c8
       type(AVX512c8f64_t),   intent(in) :: c8
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(AVX512c8f64_t) :: iq
       !DIR$ ATTRIBUTES ALIGN : 64 :: t0
       type(ZMM8r8_t) :: t0
       !
       ! EXec code ....
       t0 = cabs_zmm8c8(c8)
       iq.re = log(t0.v)
       iq.im = carg_zmm8c8(c8)
      end function clog_zmm8c8 
    
!DIR$ ATTRIBUTES INLINE :: clog_2xzmm8r8
      pure elemental function clog_2xzmm8r8(re,im) result(iq)
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: clog_2xzmm8r8
        !DIR$ ATTRIBUTES VECTOR :: clog_2xzmm8r8
        type(ZMM8r8_t),  intent(in), value :: re
        type(ZMM8r8_t),  intent(in), value :: im
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(AVX512c8f64_t) :: iq
        !DIR$ ATTRIBUTES ALIGN : 64 :: t0
        type(ZMM8r8_t) :: t0
        ! EXec code ....
        t0 = cabs_2xzmm8r8(re,im)
        iq.re = log(t0.v)
        iq.im = carg_2xzmm8r8(re,im)
      end function clog_2xzmm8r8

!DIR$ ATTRIBUTES INLINE :: csqrt_zmm8c8
      pure elemental function csqrt_zmm8c8(c8) result(iq)
        use mod_vecconsts, only : v8_1over2
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: csqrt_zmm8c8
        !DIR$ ATTRIBUTES VECTOR :: csqrt_zmm8c8
        type(AVX512c8f64_t),   intent(in) :: c8
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(AVX512c8f64_t) :: iq
        !DIR$ ATTRIBUTES ALIGN : 64 :: t0,t1,t2
        type(ZMM8r8_t) :: t0
        type(ZMM8r8_t) :: t1
        type(ZMM8r8_t) :: t2
        ! Exec code ....
        t0 = cabs_zmm8c8(c8)
        t1.v = v8_1over2.v*(t0.v+c8.re)
        iq.re = sqrt(t1.v)
        t2.v = v8_1over2.v*(t0.v-c8.re)
        iq.im = sqrt(t2.v)
      end function csqrt_zmm8c8   
     

!DIR$ ATTRIBUTES INLINE :: csqrt_2xzmm8r8
      pure elemental function csqrt_2xzmm8r8(re,im) result(iq)
        use mod_vecconsts, only : v8_1over2
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: csqrt_2xzmm8r8
        !DIR$ ATTRIBUTES VECTOR :: csqrt_2xzmm8r8
        type(ZMM8r8_t),  intent(in), value :: re
        type(ZMM8r8_t),  intent(in), value :: im
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(AVX512c8f64_t) :: iq
        !DIR$ ATTRIBUTES ALIGN : 64 :: t0,t1,t2
        type(ZMM8r8_t) :: t0
        type(ZMM8r8_t) :: t1
        type(ZMM8r8_t) :: t2
        ! Exec code ....
        t0 = cabs_2xzmm8r8(re,im)
        t1.v = v8_1over2*(t0.v+re.v)
        iq.re = sqrt(t1.v)
        t2.v = v8_1over2*(t0.v-re.v)
      end function csqrt_2xzmm8r8

!DIR$ ATTRIBUTES INLINE :: select_zmm8c8
      pure elemental function select_zmm8c8(lhs,rhs,mask) result(iq)
        use, intrinsic :: ISO_C_BINDING
        use mod_avx512_bindings, only : v8f64, v8f64_mask_blend_pd
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: select_zmm8c8
        !DIR$ ATTRIBUTES VECTOR :: select_zmm8c8
        type(AVX512c8f64_t),  intent(in) :: lhs
        type(AVX512c8f64_t),  intent(in) :: ths
        integer(c_char),      intent(in) :: mask
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(AVX512c8f64_t) :: iq
        !DIR$ ATTRIBUTES ALIGN : 64 :: lre,lim,rre,rim
        type(v8f64) :: lre,lim,rre,rim,tre,tim
        ! EXec code ....
        lre.zmm = lhs.re
        rre.zmm = rhs.re
        tre = v8f64_mask_blend_pd(mask,lre,rre)
        iq.re = tre.zmm
        lim.zmm = lhs.im
        rim.zmm = rhs.im
        tim = v8f64_mask_blend_pd(mask,lim,rim)
        iq.im = tim.zmm
      end function select_zmm8c8
       
       

!DIR$ ATTRIBUTES INLINE :: permute_zmm8c8
      pure elemental function permute_zmm8c8(c8,k,imm) result(iq)
        use, intrinsic :: ISO_C_BINDING
        use mod_avx512_bindings, only : v8f64, v8f64_mask_permute_pd
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: permute_zmm8c8
        !DIR$ ATTIRBUTES VECTOR :: permute_zmm8c8
        type(AVX512c8f64_t), intent(in) :: c8
        integer(c_char),     intent(in) :: k
        integer(c_int),      intent(in) :: imm
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(AVX512c8f64_t) :: iq
        !DIR$ ATTRIBUTES ALIGN : 64 :: tre,tim,rre,rim
        type(v8f64) :: tre,tim,rre,rim
        ! EXec code ....
        tre.zmm = c8.re
        tim.zmm = c8.im
        rre = v8f64_mask_permute_pd(tre,k,tim,imm)
        iq.re = rre.zmm
        rim = v8f64_mask_permute_pd(tim,k,tre,imm)
        iq.im = rim.zmm
      end function permute_zmm8c8

!DIR$ ATTRIBUTES INLINE :: expand_zmm8c8
      pure elemental function maskz_expand_zmm8c8(c8,k) result(iq)
        use, intrinsic :: ISO_C_BINDING
        use mod_avx512_bindings, only : v8f64, v8f64_maskz_expand_pd
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: maskz_expand_zmm8c8
        !DIR$ ATTRIBUTES VECTOR :: maskz_expand_zmm8c8
        type(AVX512c8f64_t),  intent(in) :: c8
        integer(c_char),      intent(in) :: k
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(AVX512c8f64_t) :: iq
        !DIR$ ATTRIBUTES ALIGN : 64 :: tre,tim,rre,rim
        type(v8f64) :: tre,tim,rre,rim
        ! EXec code ....
        tre.zmm = c8.re
        rre = v8f64_maskz_expand_pd(k,tre)
        iq.re = rre.zmm
        tim.zmm = c8.im
        rim = v8f64_maskz_expand_pd(k,tim)
        iq.im = rim.zmm
      end function maskz_expand_zmm8c8

!DIR$ ATTRIBUTES INLINE :: cdiv_smith
      pure elemental function cdiv_smith(lhs,rhs) result(iq)
#if  (USE_INTRINSIC_VECTOR_COMPARE) == 1
        use, intrinsic :: ISO_C_BINDING
        use mod_avx512_bindings, only : v8f64, v8f64_cmp_pd_mask
#endif
        use mod_vecconsts, only : v8_n0
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cdiv_smith
        !DIR$ ATTRIBUTES VECTOR :: cdiv_smith
        type(AVX512c8f64_t),  intent(in) :: lhs
        type(AVX512c8f64_t),  intent(in) :: rhs
        ! DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(AVX512c8f64_t) :: iq
        !DIR$ ATTRIBUTES ALIGN : 64 :: ratio,denom
        type(ZMM8r8_t) :: ratio,denom
        !DIR$ ATTRIBUTES ALIGN : 32 :: bres
        logical(kind=int4), dimension(0:7) :: bres
#if   (USE_INTRINSIC_VECTOR_COMPARE) == 1
        !DIR$ ATTRIBUTES ALIGN : 64 :: tre,tim
        type(v8f64) :: tre,tim
        integer(c_char), automatic :: mask_gte
        integer(c_char), parameter :: all_ones = Z'FF'
#endif
        ! EXec code ....
        ratio = v8_n0
        denom = v8_n0
#if   (USE_INTRINSIC_VECTOR_COMPARE) == 1
        tre.zmm = abs(rhs.re)
        tim.zmm = abs(rhs.im)
        mask_gte = v8f64_cmp_pd_mask(tre,tim,29);
#else
        bres = abs(rhs.re) >= abs(rhs.im)
#endif
        re_part = v8_n0
        im_part = v8_n0
#if   (USE_INTRINSIC_VECTOR_COMPARE) == 1
        if(mask_gte == all_ones) then
#elif
        if(all(bres)) then
#endif
           ratio.v   = rhs.im/rhs.re
           denom.v   = rhs.re+(ratio.v*rhs.im)
           iq.re     = (lhs.re+lhs.im*ratio.v)/denom.v
           iq.im     = (lhs.im-lhs.re*ratio.v)/denom.v
        else
           ratio.v   = rhs.re/rhs.im
           denom.v   = rhs.im+ratio.v*rhs.re
           iq.re     = (lhs.re*ratio.v+lhs.im)/denom.v
           iq.im     = (lhs.im*ratio.v-lhs.re)/denom.v
        end if
      end function cdiv_smith

      
     
end module mod_avx512c8f64
