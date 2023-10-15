

module sse_cvec4

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'sse_cvec4'
 !          
 !          Purpose:
 !                      This module contains a decomposed to real and imaginary
 !                      parts a complex vector of 4 elements (complex numbers)
 !                      This representation nicely fits into 2 SSE XMMx
 !                      registers.
 !          History:
 !                        Date: 15-10-2023
 !                        Time: 16:47 GMT+2
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

     use mod_kinds, only : i1,i4,sp
     use mod_vectypes, only : XMM4r4_t
     use,  intrinsic :: ISO_C_BINDING
     implicit none
     public
     !=====================================================59
     !  File and module information:
     !  version,creation and build date, author,description
     !=====================================================59

     ! Major version
     integer(kind=i4),  parameter :: SSE_CVEC4_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: SSE_CVEC4_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: SSE_CVEC4_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: SSE_CVEC4_FULLVER =   &
            1000*SSE_CVEC4_MAJOR+100*SSE_CVEC4_MINOR+10*SSE_CVEC4_MICRO
     ! Module creation date
     character(*),        parameter :: SSE_CVEC4_CREATE_DATE = "15-10-2023 16:48 +00200 (SUN 15 OCT 2022 GMT+2)"
     ! Module build date
     character(*),        parameter :: SSE_CVEC4_BUILD_DATE  = __DATE__ " " __TIME__
     ! Module author info
     character(*),        parameter :: SSE_CVEC4_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: SSE_CVEC4_SYNOPSIS    = "Packed complex vector of 4 elements (complex numbers)"

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
    

     type, public :: XMM4c4_t
        ! 
        sequence
        real(kind=sp), dimension(0:3) :: re
        real(kind=sp), dimension(0:3) :: im
     end type XMM4c4_t
       
        
        
        
      
        
    

     interface operator (+)
         module procedure xmm4c4_add_xmm4c4
         module procedure xmm4c4_add_c1
         module procedure xmm4c4_add_xmm4r4
         module procedure xmm4c4_add_s1
         module procedure c1_add_xmm4c4
         module procedure xmm4r4_add_xmm4c4
         module procedure s1_add_xmm4c4
     end interface operator (+)

     interface operator (-)
         module procedure xmm4c4_sub_xmm4c4
         module procedure xmm4c4_sub_c1
         module procedure xmm4c4_sub_xmm4r4
         module procedure xmm4c4_sub_s1
         module procedure c1_sub_xmm4c4
         module procedure xmm4r4_sub_xmm4c4
         module procedure s1_sub_xmm4c4
      end interface operator (-)
      
     interface operator (*)
         module procedure xmm4c4_mul_xmm4c4
         module procedure xmm4c4_mul_c1
         module procedure xmm4c4_mul_xmm4r4
         module procedure xmm4c4_mul_s1
         module procedure c1_mul_xmm4c4
         module procedure xmm4r4_mul_xmm4c4
         module procedure s1_mul_xmm4c4
      end interface operator (*)

      interface operator (/)
         module procedure xmm4c4_div_xmm4c4
         module procedure xmm4c4_div_c1
         module procedure xmm4c4_div_xmm4r4
         module procedure xmm4c4_div_s1
         module procedure c1_div_xmm4c4
         module procedure xmm4r4_div_xmm4c4
         module procedure s1_div_xmm4c4
      end interface operator (/)
!#if 0      
!#if (USE_INTRINSIC_VECTOR_COMPARE) == 1
!!      interface operator (==)
!         module procedure c8_eq_c8
!         module procedure c8_eq_c1
       
!         module procedure c1_eq_c8
        
!      end interface operator (==)

!      interface operator (/=)
!         module procedure c8_neq_c8
!         module procedure c8_neq_c1
       
!         module procedure c1_neq_c8
      
!      end interface operator (/=)

!      interface operator (>)
!         module procedure c8_gt_c8
!         module procedure c8_gt_c1
        
!         module procedure c1_gt_c8
     
!      end interface operator (>)

!      interface operator (<)
!         module procedure c8_lt_c8
!         module procedure c8_lt_c1
        
!         module procedure c1_lt_c8
        
!      end interface operator (<)

!      interface operator (>=)
!         module procedure c8_ge_c8
!         module procedure c8_ge_c1
       
!         module procedure c1_ge_c8
        
!      end interface operator (>=)

!      interface operator (<=)
!         module procedure c8_le_c8
!         module procedure c8_le_c1
        
!         module procedure c1_le_c8
        
!      end interface operator (<=)
!#else
!#endif
        interface operator (==)
         module procedure xmm4c4_eq_xmm4c4
         module procedure xmm4c4_eq_c1
        
         module procedure c1_eq_xmm4c4
        
      end interface operator (==)

      interface operator (/=)
         module procedure xmm4c4_neq_xmm4c4
         module procedure xmm4c4_neq_c1
       
         module procedure c1_neq_xmm4c4
       
      end interface operator (/=)

      interface operator (>)
         module procedure xmm4c4_gt_xmm4c4
         module procedure xmm4c4_gt_c1
        
         module procedure c1_gt_xmm4c4
        
      end interface operator (>)

      interface operator (<)
         module procedure xmm4c4_lt_xmm4c4
         module procedure xmm4c4_lt_c1
        
         module procedure c1_lt_xmm4c4
       
      end interface operator (<)

      interface operator (>=)
         module procedure xmm4c4_ge_xmm4c4
         module procedure xmm4c4_ge_c1
        
         module procedure c1_ge_xmm4c4
        
      end interface operator (>=)

      interface operator (<=)
         module procedure xmm4c4_le_xmm4c4
         module procedure xmm4c4_le_c1
        
         module procedure c1_le_xmm4c4
        
      end interface operator (<=)


      
      contains
!DIR$ ATTRIBUTES INLINE :: default_init
     pure   function default_init() result(iq)
          !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: default_init
          !DIR$ ATTRIBUTES VECTOR :: default_init 
          !DIR$ ATTRIBUTES ALIGN : 16 :: iq
          type(XMM4c4_t) :: iq
          ! Exec code
          iq.re = 0.0_sp
          iq.im = 0.0_sp
        end function default_init

!DIR$ ATTRIBUTES INLINE :: array_init
     pure   function array_init(re,im) result(iq)
          !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: array_init
          !DIR$ ATTRIBUTES VECTOR :: array_init
          real(kind=sp), dimension(0:1), intent(in) :: re
          real(kind=sp), dimension(0:1), intent(in) :: im
          !DIR$ ATTRIBUTES ALIGN : 16 :: iq
          type(XMM4c4_t) :: iq
          ! EXec code ....
          iq.re = re
          iq.im = im
       end function array_init

!DIR$ ATTRIBUTES INLINE :: complex1_init
     pure   function complex1_init(c) result(iq)
          !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: complex1_init
          complex(kind=sp), intent(in) :: c
          !DIR$ ATTRIBUTES ALIGN : 16 :: iq
          type(XMM4c4_t) :: iq
          ! Exec code ...
          iq.re = real(c,kind=sp)
          iq.im = aimag(c,kind=sp)
     end function complex1_init

!DIR$ ATTRIBUTES INLINE :: complex2x4_init
     pure   function complex2x4_init(c) result(iq)
         !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: complex2x4_init
         complex(kind=sp), dimension(0:3), intent(in) :: c
         !DIR$ ATTRIBUTES ALIGN : 16 :: iq
         type(XMM4c4_t) :: iq
         ! Exec code ....
         iq.re = real(c,kind=sp)
         iq.im = aimag(c,kind=sp)
     end function complex2x4_init

!DIR$ ATTRIBUTES INLINE :: xmm4r42x_init       
     pure  function xmm4r42x_init(v1,v2) result(iq)
         !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: xmm4r42x_init
         !DIR$ ATTRIBUTES VECTOR :: xmm4r42x_init
         type(XMM4r4_t),  intent(in) :: v1
         type(XMM4r4_t),  intent(in) :: v2
         !DIR$ ATTRIBUTES ALIGN : 16 :: iq
         type(XMM4c4_t) :: iq
         ! EXec code ....
         iq.re = v1.v
         iq.im = v2.v
     end function xmm4r42x_init

!DIR$ ATTRIBUTES INLINE :: xmm4r41x_init
     pure function xmm4r41x_init(v1) result(iq)
         !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: xmm4r41x_init
         !DIR$ ATTRIBUTES VECTOR :: xmm4r41x_init
         type(XMM4r4_t),  intent(in) :: v1
         !DIR$ ATTRIBUTES ALIGN : 16 :: iq
         type(XMM4c4_t) :: iq
         ! Exec code ....
         iq.re = v1.v
         iq.im = 0.0_sp
     end function xmm4r41x_init

!DIR$ ATTRIBUTES INLINE :: r41x_init
     pure function r41x_init(s) result(iq)
         !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: r41x_init
         !DIR$ ATTRIBUTES VECTOR :: r41x_init
         real(kind=sp),  intent(in) :: s
         !DIR$ ATTRIBUTES ALIGN : 16 :: iq
         type(XMM4c4_t) :: iq
         ! EXec code ....
         iq.re = s
         iq.im = 0.0_sp
     end function r41x_init

!DIR$ ATTRIBUTES INLINE ::  copy_init
     pure function copy_init(rhs) result(iq)
         !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: copy_init
         !DIR$ ATTRIBUTES VECTOR :: copy_init
         type(XMM4c4_t),  intent(in) :: rhs
         !DIR$ ATTRIBUTES ALIGN : 16 :: iq
         type(XMM4c4_t) :: iq
         ! EXec code ...
         iq = rhs
     end function copy_init

!DIR$ ATTRIBUTES INLINE :: xmm4c4_add_xmm4c4
     pure function xmm4c4_add_xmm4c4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: xmm4c4_add_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: xmm4c4_add_xmm4c4
       type(XMM4c4_t),  intent(in) :: lhs
       type(XMM4c4_t),  intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       ! EXec code ....
       iq.re = lhs.re+rhs.re
       iq.im = lhs.im+rhs.im
     end function xmm4c4_add_xmm4c4

!DIR$ ATTRIBUTES INLINE :: xmm4c4_add_c1
     pure function xmm4c4_add_c1(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: xmm4c4_add_c1
       !DIR$ ATTRIBUTES VECTOR :: xmm4c4_add_c1
       type(XMM4c4_t),  intent(in) :: lhs
       complex(kind=sp),     intent(in), value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       ! EXec code ....
       iq.re = lhs.re+real(rhs,kind=sp)
       iq.im = lhs.im+aimag(rhs,kind=sp)
     end function xmm4c4_add_c1

!DIR$ ATTRIBUTES INLINE :: xmm4c4_add_xmm4r4
     pure function xmm4c4_add_xmm4r4(lhs,rhs) result(iq)
      
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: xmm4c4_add_xmm4r4
       !DIR$ ATTRIBUTES VECTOR :: xmm4c4_add_xmm4r4
       type(XMM4c4_t),   intent(in) :: lhs
       type(XMM4r4_t),        intent(in),value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       !Exec code....
       iq.re = lhs.re+rhs.v
       iq.im = 0.0_sp
     end function xmm4c4_add_xmm4r4

!DIR$ ATTRIBUTES INLINE :: xmm4c4_add_s1
     pure function xmm4c4_add_s1(lhs,rhs) result(iq)
      
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: xmm4c4_add_s1
       !DIR$ ATTRIBUTES VECTOR :: xmm4c4_add_s1
       type(XMM4c4_t),   intent(in) :: lhs
       real(kind=sp),         intent(in), value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       iq.re = lhs.re+rhs
       iq.im = 0.0_sp
     end function xmm4c4_add_s1

!DIR$ ATTRIBUTES INLINE :: c1_add_xmm4c4     
     pure function c1_add_xmm4c4(lhs,rhs) result(iq)
       !
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c1_add_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: c1_add_xmm4c4
       complex(kind=sp),     intent(in) :: lhs
       type(XMM4c4_t),  intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       iq.re = real(lhs,kind=sp)+rhs.re
       iq.im = aimag(lhs,kind=sp)+rhs.im
     end function c1_add_xmm4c4

!DIR$ ATTRIBUTES INLINE xmm4r4_add_xmm4c4
     pure function xmm4r4_add_xmm4c4(lhs,rhs) result(iq)
      
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  xmm4r4_add_xmm4c4
       !DIR$ ATTRIBUTES VECTOR ::  xmm4r4_add_xmm4c4
       type(XMM4r4_t),      intent(in) :: lhs
       type(XMM4c4_t), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       iq.re = lhs.v+rhs.re
       iq.im = 0.0_sp
     end function  xmm4r4_add_xmm4c4

!DIR$ ATTRIBUTES INLINE :: s1_add_xmm4c4
     pure function s1_add_xmm4c4(lhs,rhs) result(iq)
       
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: s1_add_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: s1_add_xmm4c4
       real(kind=sp),       intent(in) :: lhs
       type(XMM4c4_t), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       iq.m_re = lhs+rhs.m_re
       iq.m_im = 0.0_sp
     end function s1_add_xmm4c4

!DIR$ ATTRIBUTES INLINE :: xmm4c4_sub_xmm4c4
     pure function xmm4c4_sub_xmm4c4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  xmm4c4_sub_xmm4c4
       !DIR$ ATTRIBUTES VECTOR ::  xmm4c4_sub_xmm4c4
       type(XMM4c4_t),    intent(in) :: lhs
       type(XMM4c4_t),    intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       iq.re = lhs.re-rhs.re
       iq.im = lhs.im-rhs.im
     end function  xmm4c4_sub_xmm4c4

!DIR$ ATTRIBUTES INLINE :: xmm4c4_sub_c1
     pure function xmm4c4_sub_c1(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: xmm4c4_sub_c1
       !DIR$ ATTRIBUTES VECTOR :: xmm4c4_sub_c1
       type(XMM4c4_t),     intent(in) :: lhs
       complex(kind=sp),        intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       iq.re = lhs.re-real(rhs,kind=sp)
       iq.im = lhs.im-aimag(rhs,kind=sp)
     end function xmm4c4_sub_c1

!DIR$ ATTRIBUTES INLINE :: xmm4c4_sub_xmm4r4
     pure function xmm4c4_sub_xmm4r4(lhs,rhs) result(iq)
      
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: xmm4c4_sub_xmm4r4
       !DIR$ ATTRIBUTES VECTOR :: xmm4c4_sub_xmm4r4
       type(XMM4c4_t),    intent(in) :: lhs
       type(XMM4r4_t),         intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       iq.re = lhs.re-rhs.v
       iq.im = 0.0_sp
     end function xmm4c4_sub_xmm4r4

!DIR$ ATTRIBUTES INLINE :: xmm4c4_sub_s1
     pure function xmm4c4_sub_s1(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: xmm4c4_sub_s1
       !DIR$ ATTRTIBUTES VECTOR :: xmm4c4_sub_s1
       type(XMM4c4_t),     intent(in) :: lhs
       real(kind=sp),           intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       iq.re = lhs.re-rhs
       iq.im = 0.0_sp
     end function xmm4c4_sub_s1

!DIR$ ATTRIBUTES INLINE :: c1_sub_xmm4c4
     pure function c1_sub_xmm4c4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c1_sub_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: c1_sub_xmm4c4
       complex(kind=sp),      intent(in) :: lhs
       type(XMM4c4_t),   intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       iq.re = real(lhs,kind=sp)-rhs.re
       iq.im = aimag(lhs,kind=sp)-rhs.im
     end function c1_sub_xmm4c4

!DIR$ ATTRIBUTES INLINE :: xmm4r4_sub_xmm4c4
     pure function xmm4r4_sub_xmm4c4(lhs,rhs) result(iq)
     
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: xmm4r4_sub_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: xmm4r4_sub_xmm4c4
       type(XMM4r4_t),      intent(in) :: lhs
       type(XMM4c4_t), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       iq.re = lhs.v-rhs.re
       iq.im = 0.0_sp
     end function xmm4r4_sub_xmm4c4

!DIR$ ATTRIBUTES INLINE :: s1_sub_xmm4c4
     pure function  s1_sub_xmm4c4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  s1_sub_xmm4c4
       !DIR$ ATTRIBUTES VECTOR ::  s1_sub_xmm4c4
       real(kind=sp),       intent(in) :: lhs
       type(XMM4c4_t), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       iq.re = lhs-rhs.re
       iq.im = 0.0_sp
     end function  s1_sub_xmm4c4

!DIR$ ATTRIBUTES INLINE :: xmm4c4_mul_xmm4c4
     pure function xmm4c4_mul_xmm4c4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: xmm4c4_mul_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: xmm4c4_mul_xmm4c4
       type(XMM4c4_t),    intent(in) :: lhs
       type(XMM4c4_t),    intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       !DIR$ ATTRIBUTES ALIGN : 16 :: xmm0,xmm1,xmm2,xmm3
       type(XMM4r4_t) :: xmm0,xmm1,xmm2,xmm3
       xmm0.v = lhs.re*rhs.re
       xmm1.v = lhs.im*rhs.im
       iq.re  = xmm0.v-xmm1.v
       xmm2.v = lhs.im*rhs.re
       xmm3.v = lhs.re*rhs.im
       iq.im  = xmm2.v-xmm3.v
     end function xmm4c4_mul_xmm4c4

!DIR$ ATTRIBUTES INLINE :: xmm4c4_mul_c1
     pure function  xmm4c4_mul_c1(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  xmm4c4_mul_c1
       !DIR$ ATTRIBUTES VECTOR ::  xmm4c4_mul_c1
       type(XMM4c4_t),     intent(in) :: lhs
       complex(kind=sp),        intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       !DIR$ ATTRIBUTES ALIGN : 16 :: xmm0,xmm1,xmm2,xmm3
       type(XMM4r4_t) :: xmm0,xmm1,xmm2,xmm3
       xmm0.v = lhs.re*real(rhs,kind=sp)
       xmm1.v = lhs.im*aimag(rhs,kind=sp)
       iq.re  = xmm0.v-xmm1.v
       xmm2.v = lhs.im*real(rhs,kind=sp)
       xmm3.v = lhs.re*aimag(rhs,kind=sp)
       iq.im  = xmm2.v-xmm3.v
     end function  xmm4c4_mul_c1

!DIR$ ATTRIBUTES INLINE :: xmm4c4_mul_xmm4r4
     pure function xmm4c4_mul_xmm4r4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: xmm4c4_mul_xmm4r4
       !DIR$ ATTRIBUTES VECTOR :: xmm4c4_mul_xmm4r4
       type(XMM4c4_t),    intent(in) :: lhs
       type(XMM4r4_t),         intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       iq.re = lhs.re*rhs.v
       iq.im = lhs.im*rhs.v
     end function xmm4c4_mul_xmm4r4

!DIR$ ATTRIBUTES INLINE :: xmm4c4_mul_s1
     pure function  xmm4c4_mul_s1(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  xmm4c4_mul_s1
       !DIR$ ATTRIBUTES VECTOR ::  xmm4c4_mul_s1
       type(XMM4c4_t),    intent(in) :: lhs
       real(kind=sp),   intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       iq.re = lhs.re*rhs
       iq.im = lhs.im*rhs
     end function  xmm4c4_mul_s1

!DIR$ ATTRIBUTES INLINE :: c1_mul_xmm4c4
     pure function c1_mul_xmm4c4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c1_mul_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: c1_mul_xmm4c4
       complex(kind=sp),        intent(in) :: lhs
       type(XMM4c4_t),     intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       !DIR$ ATTRIBUTES ALIGN : 16 :: xmm0,xmm1,xmm2,xmm3
       type(XMM4r4_t) :: xmm0,xmm1,xmm2,xmm3
       xmm0.v = real(lhs,kind=sp)*rhs.re
       xmm1.v = aimag(lhs,kind=sp)*rhs.im
       iq.re  = xmm0.v+xmm1.v
       xmm2.v = real(lhs,kind=sp)*rhs.im
       xmm3.v = aimag(lhs,kind=sp)*rhs.re
       iq.im  = xmm2.v-xmm3.v
     end function c1_mul_xmm4c4

!DIR$ ATTRIBUTES INLINE :: xmm4r4_mul_xmm4c4
     pure function  xmm4r4_mul_xmm4c4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  xmm4r4_mul_xmm4c4
       !DIR$ ATTRIBUTES VECTOR ::  xmm4r4_mul_xmm4c4
       type(XMM4r4_t),        intent(in) :: lhs
       type(XMM4c4_t),   intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 ::  xmm4r4_mul_xmm4c4
       type(XMM4c4_t) :: iq
       iq.re = lhs.v*rhs.re
       iq.im = lhs.v*rhs.im
     end function  xmm4r4_mul_xmm4c4

!DIR$ ATTRIBUTES INLINE :: s1_mul_xmm4c4
     pure function s1_mul_xmm4c4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: s1_mul_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: s1_mul_xmm4c4
       real(kind=sp),       intent(in) :: lhs
       type(XMM4c4_t), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       iq.re = lhs*rhs.re
       iq.im = lhs*rhs.im
     end function s1_mul_xmm4c4

!DIR$ ATTRIBUTES INLINE :: xmm4c4_div_xmm4c4    
     pure function xmm4c4_div_xmm4c4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  xmm4c4_div_xmm4c4   
       !DIR$ ATTRIBUTES VECTOR ::  xmm4c4_div_xmm4c4   
       type(XMM4c4_t),    intent(in) :: lhs
       type(XMM4c4_t),    intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       !DIR$ ATTRIBUTES ALIGN : 16 :: xmm0,xmm1,xmm2,xmm3,den
       type(XMM4r4_t), automatic :: xmm0,xmm1,xmm2,xmm3,den
#if (USE_SAFE_COMPLEX_DIVISION) == 1
       iq = cdiv_smith(lhs,rhs)
#else
       xmm0.v = lhs.re*rhs.re
       xmm1.v = lhs.im*rhs.im
       xmm2.v = lhs.im*rhs.re
       xmm3.v = lhs.re*rhs.im
       den.v  = (rhs.re*rhs.re)+(rhs.im*rhs.im)
       iq.re  = (xmm0.v+xmm1.v)/den.v
       iq.im  = (xmm2.v-xmm3.v)/den.v
#endif
     end function  xmm4c4_div_xmm4c4   

!DIR$ ATTRIBUTES INLINE :: xmm4c4_div_c1
     pure function xmm4c4_div_c1(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: xmm4c4_div_c1
       !DIR$ ATTRIBUTES VECTOR :: xmm4c4_div_c1
       type(XMM4c4_t),   intent(in) :: lhs
       complex(kind=sp),      intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       !DIR$ ATTRIBUTES ALIGN : 16 :: xmm0,xmm1,xmm2,xmm3,den
       type(XMM4r4_t), automatic :: xmm0,xmm1,xmm2,xmm3,den
       xmm0.v = lhs.re*real(rhs,kind=sp)
       xmm1.v = lhs.im*aimag(rhs,kind=sp)
       xmm2.v = lhs.im*real(rhs,kind=sp)
       xmm3.v = lhs.re*aimag(rhs,kind=sp)
       den.v  = (real(rhs,kind=sp)*real(rhs,kind=sp))+ &
            (aimag(rhs,kind=sp)*aimag(rhs,kind=sp))
       iq.re = (xmm0.v+xmm1.v)/den.v
       iq.im = (xmm2.v-xmm3.v)/den.v
     end function xmm4c4_div_c1

!DIR$ ATTRIBUTES INLINE :: xmm4c4_div_v2
     pure function xmm4c4_div_v2(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: xmm4c4_div_v2
       !DIR$ ATTRIBUTES VECTOR :: xmm4c4_div_v2
       type(XMM4c4_t),  intent(in) :: lhs
       type(XMM4r4_t),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       iq.re = lhs.re/rhs.v
       iq.im = lhs.im/rhs.v
     end function xmm4c4_div_v2

!DIR$ ATTRIBUTES INLINE :: xmm4c4_div_s1
     pure  function xmm4c4_div_s1(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: xmm4c4_div_s1
       !DIR$ ATTRIBUTES VECTOR :: xmm4c4_div_s1
       type(XMM4c4_t),     intent(in) :: lhs
       real(kind=sp),           intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       iq.re = lhs.re/rhs
       iq.im = lhs.im/rhs
     end function xmm4c4_div_s1

!DIR$ ATTRIBUTES INLINE :: c1_div_xmm4c4
     pure function c1_div_xmm4c4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c1_div_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: c1_div_xmm4c4
       complex(kind=sp),       intent(in) :: lhs
       type(XMM4c4_t),      intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       !DIR$ ATTRIBUTES ALIGN : 16 :: xmm0,xmm1,xmm2,xmm3,den
       type(XMM4c4_t), automatic :: xmm0,xmm1,xmm2,xmm3,den
       real(kind=sp), automatic :: r,i
       r = real(lhs,kind=sp)
       i = aimag(lhs,kind=sp)
       xmm0.v = r*rhs.re
       xmm1.v = i*rhs.im
       xmm2.v = i*rhs.re
       xmm3.v = r*rhs.im
       den.v  = (rhs.re*rhs.re)+(rhs.im*rhs.im)
       iq.re  = (xmm0.v+xmm1.v)/den.v
       iq.im  = (xmm2.v-xmm3.v)/den.v
     end function c1_div_xmm4c4

!DIR$ ATTRIBUTES INLINE :: v2_div_xmm4c4
     pure  function v2_div_xmm4c4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: v2_div_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: v2_div_xmm4c4
       type(XMM4r4_t),      intent(in) :: lhs
       type(XMM4c4_t), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       !DIR$ ATTRIBUTES ALIGN : 16 :: t0
       type(XMM4c4_t) :: t0
       t0.re = lhs.v
       t0.im = 0.0_sp
       iq    = t0/rhs
    end function v2_div_xmm4c4

!DIR$ ATTRIBUTES INLINE :: s1_div_xmm4c4
     pure  function s1_div_xmm4c4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: s1_div_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: s1_div_xmm4c4
       real(kind=sp),       intent(in) :: lhs
       type(XMM4c4_t), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       !DIR$ ATTRIBUTES ALIGN : 16 :: t0
       type(XMM4c4_t) :: t0
       t0.re = lhs
       t0.im = 0.0_sp
       iq    = t0/rhs
     end function s1_div_xmm4c4

!DIR$ ATTRIBUTES INLINE :: conjugate
     pure  function conjugate(x) result(iq)
      
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: conjugate
       !DIR$ ATTRIBUTES VECTOR :: conjugate
       type(XMM4c4_t),  intent(in) :: x
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       iq.re = 0.0_sp-x.re
       iq.im = x.im
     end function conjugate
     





     





!DIR$ ATTRIBUTES INLINE :: xmm4c4_eq_xmm4c4
     pure function xmm4c4_eq_xmm4c4(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: xmm4c4_eq_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: xmm4c4_eq_xmm4c4
       type(XMM4c4_t),       intent(in) :: lhs
       type(XMM4c4_t),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: mre,mim
       logical(kind=i4), dimension(0:3) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re == rhs.re
       bres(0) = all(mre)
       mim = lhs.im == rhs.im
       bres(1) = all(mim)
     end function xmm4c4_eq_xmm4c4

!DIR$ ATTRIBUTES INLINE :: xmm4c4_eq_c1
     pure function xmm4c4_eq_c1(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: xmm4c4_eq_c1
       !DIR$ ATTRIBUTES VECTOR :: xmm4c4_eq_c1
       type(XMM4c4_t),    intent(in) :: lhs
       complex(kind=sp),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: mre,mim
       logical(kind=i4), dimension(0:3) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re == real(rhs,kind=sp)
       bres(0) = all(mre)
       mim = lhs.im == aimag(rhs,kind=sp)
       bres(1) = all(mim)
     end function xmm4c4_eq_c1

!DIR$ ATTRIBUTES INLINE :: c1_eq_xmm4c4
     pure function c1_eq_xmm4c4(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c1_eq_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: c1_eq_xmm4c4
       complex(kind=sp),        intent(in) :: lhs
       type(XMM4c4_t),     intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: mre,mim
       logical(kind=i4), dimension(0:3) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = real(lhs,kind=sp)  == rhs.re
       bres(0) = all(mre)
       mim = aimag(lhs,kind=sp) == rhs.im
       bres(1) = all(mim)
     end function c1_eq_xmm4c4

!DIR$ ATTRIBUTES INLINE :: xmm4c4_neq_xmm4c4
     pure function xmm4c4_neq_xmm4c4(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: xmm4c4_neq_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: xmm4c4_neq_xmm4c4
       type(XMM4c4_t),       intent(in) :: lhs
       type(XMM4c4_t),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: mre,mim
       logical(kind=i4), dimension(0:3) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re /= rhs.re
       bres(0) = all(mre)
       mim = lhs.im /= rhs.im
       bres(1) = all(mim)
     end function xmm4c4_neq_xmm4c4

!DIR$ ATTRIBUTES INLINE :: xmm4c4_neq_c1
     pure function xmm4c4_neq_c1(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: xmm4c4_neq_c1
       !DIR$ ATTRIBUTES VECTOR :: xmm4c4_neq_c1
       type(XMM4c4_t),    intent(in) :: lhs
       complex(kind=sp),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: mre,mim
       logical(kind=i4), dimension(0:3) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re /= real(rhs,kind=sp)
       bres(0) = all(mre)
       mim = lhs.im /= aimag(rhs,kind=sp)
       bres(1) = all(mim)
     end function xmm4c4_neq_c1

!DIR$ ATTRIBUTES INLINE :: c1_neq_xmm4c4
     pure function c1_neq_xmm4c4(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c1_neq_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: c1_neq_xmm4c4
       complex(kind=sp),        intent(in) :: lhs
       type(XMM4c4_t),     intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: mre,mim
       logical(kind=i4), dimension(0:3) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = real(lhs,kind=sp)  /= rhs.re
       bres(0) = all(mre)
       mim = aimag(lhs,kind=sp) /= rhs.im
       bres(1) = all(mim)
     end function c1_neq_xmm4c4

!DIR$ ATTRIBUTES INLINE :: xmm4c4_gt_xmm4c4
     pure function xmm4c4_gt_xmm4c4(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: xmm4c4_gt_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: xmm4c4_gt_xmm4c4
       type(XMM4c4_t),       intent(in) :: lhs
       type(XMM4c4_t),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: mre,mim
       logical(kind=i4), dimension(0:3) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re > rhs.re
       bres(0) = all(mre)
       mim = lhs.im > rhs.im
       bres(1) = all(mim)
     end function xmm4c4_gt_xmm4c4

!DIR$ ATTRIBUTES INLINE :: xmm4c4_gt_c1
     pure function xmm4c4_gt_c1(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: xmm4c4_gt_c1
       !DIR$ ATTRIBUTES VECTOR :: xmm4c4_gt_c1
       type(XMM4c4_t),    intent(in) :: lhs
       complex(kind=sp),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: mre,mim
       logical(kind=i4), dimension(0:3) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re > real(rhs,kind=sp)
       bres(0) = all(mre)
       mim = lhs.im > aimag(rhs,kind=sp)
       bres(1) = all(mim)
     end function xmm4c4_gt_c1

!DIR$ ATTRIBUTES INLINE :: c1_gt_xmm4c4
     pure function c1_gt_xmm4c4(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c1_gt_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: c1_gt_xmm4c4
       complex(kind=sp),        intent(in) :: lhs
       type(XMM4c4_t),     intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: mre,mim
       logical(kind=i4), dimension(0:3) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = real(lhs,kind=sp)  > rhs.re
       bres(0) = all(mre)
       mim = aimag(lhs,kind=sp) > rhs.im
       bres(1) = all(mim)
     end function c1_gt_xmm4c4
     
!DIR$ ATTRIBUTES INLINE :: xmm4c4_lt_xmm4c4
     pure function xmm4c4_lt_xmm4c4(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: xmm4c4_lt_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: xmm4c4_lt_xmm4c4
       type(XMM4c4_t),       intent(in) :: lhs
       type(XMM4c4_t),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: mre,mim
       logical(kind=i4), dimension(0:3) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re < rhs.re
       bres(0) = all(mre)
       mim = lhs.im < rhs.im
       bres(1) = all(mim)
     end function xmm4c4_lt_xmm4c4

!DIR$ ATTRIBUTES INLINE :: xmm4c4_lt_c1
     pure function xmm4c4_lt_c1(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: xmm4c4_lt_c1
       !DIR$ ATTRIBUTES VECTOR :: xmm4c4_lt_c1
       type(XMM4c4_t),    intent(in) :: lhs
       complex(kind=sp),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: mre,mim
       logical(kind=i4), dimension(0:3) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re < real(rhs,kind=sp)
       bres(0) = all(mre)
       mim = lhs.im < aimag(rhs,kind=sp)
       bres(1) = all(mim)
     end function xmm4c4_lt_c1

!DIR$ ATTRIBUTES INLINE :: c1_lt_xmm4c4
     pure function c1_lt_xmm4c4(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c1_lt_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: c1_lt_xmm4c4
       complex(kind=sp),        intent(in) :: lhs
       type(XMM4c4_t),     intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: mre,mim
       logical(kind=i4), dimension(0:3) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = real(lhs,kind=sp) < rhs.re
       bres(0) = all(mre)
       mim = aimag(lhs,kind=sp) < rhs.im
       bres(1) = all(mim)
     end function c1_lt_xmm4c4

!DIR$ ATTRIBUTES INLINE :: xmm4c4_ge_xmm4c4
     pure function xmm4c4_ge_xmm4c4(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: xmm4c4_ge_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: xmm4c4_ge_xmm4c4
       type(XMM4c4_t),       intent(in) :: lhs
       type(XMM4c4_t),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: mre,mim
       logical(kind=i4), dimension(0:3) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re >= rhs.re
       bres(0) = all(mre)
       mim = lhs.im >= rhs.im
       bres(1) = all(mim)
     end function xmm4c4_ge_xmm4c4

!DIR$ ATTRIBUTES INLINE :: xmm4c4_ge_c1
     pure function xmm4c4_ge_c1(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: xmm4c4_ge_c1
       !DIR$ ATTRIBUTES VECTOR :: xmm4c4_ge_c1
       type(XMM4c4_t),    intent(in) :: lhs
       complex(kind=sp),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: mre,mim
       logical(kind=i4), dimension(0:3) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re >= real(rhs,kind=sp)
       bres(0) = all(mre)
       mim = lhs.im >= aimag(rhs,kind=sp)
       bres(1) = all(mim)
     end function xmm4c4_ge_c1

!DIR$ ATTRIBUTES INLINE :: c1_ge_xmm4c4
     pure function c1_ge_xmm4c4(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c1_ge_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: c1_ge_xmm4c4
       complex(kind=sp),        intent(in) :: lhs
       type(XMM4c4_t),     intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: mre,mim
       logical(kind=i4), dimension(0:3) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = real(lhs,kind=sp) >= rhs.re
       bres(0) = all(mre)
       mim = aimag(lhs,kind=sp) >= rhs.im
       bres(1) = all(mim)
     end function c1_ge_xmm4c4
     
!DIR$ ATTRIBUTES INLINE :: xmm4c4_le_xmm4c4
     pure function xmm4c4_le_xmm4c4(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: xmm4c4_le_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: xmm4c4_le_xmm4c4
       type(XMM4c4_t),       intent(in) :: lhs
       type(XMM4c4_t),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: mre,mim
       logical(kind=i4), dimension(0:3) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re <= rhs.re
       bres(0) = all(mre)
       mim = lhs.im <= rhs.im
       bres(1) = all(mim)
     end function xmm4c4_le_xmm4c4

!DIR$ ATTRIBUTES INLINE :: xmm4c4_le_c1
     pure function xmm4c4_le_c1(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: xmm4c4_le_c1
       !DIR$ ATTRIBUTES VECTOR :: xmm4c4_le_c1
       type(XMM4c4_t),    intent(in) :: lhs
       complex(kind=sp),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: mre,mim
       logical(kind=i4), dimension(0:3) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re <= real(rhs,kind=sp)
       bres(0) = all(mre)
       mim = lhs.im <= aimag(rhs,kind=sp)
       bres(1) = all(mim)
     end function xmm4c4_le_c1

!DIR$ ATTRIBUTES INLINE :: c1_le_xmm4c4
     pure function c1_le_xmm4c4(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c1_le_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: c1_le_c8
       complex(kind=sp),        intent(in) :: lhs
       type(XMM4c4_t),     intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: mre,mim
       logical(kind=i4), dimension(0:3) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = real(lhs,kind=sp) <= rhs.re
       bres(0) = all(mre)
       mim = aimag(lhs,kind=sp) <= rhs.im
       bres(1) = all(mim)
     end function c1_le_xmm4c4
     
     

!DIR$ ATTRIBUTES INLINE :: polar
     pure function polar(rho,theta) result (iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: polar
       !DIR$ ATTRIBUTES VECTOR :: polar
       type(XMM4r4_t), intent(in) :: rho
       type(XMM4r4_t), intent(in) :: theta
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       ! EXec code ....
       iq.re = rho.v*cos(theta.v)
       iq.im = rho.v*sin(theta.v) 
     end function polar
     
!DIR$ ATTRIBUTES INLINE :: carg_xmm4c4
     pure function carg_xmm4c4(c) result(arg)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: carg_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: carg_xmm4c4
       type(XMM4c4_t),   intent(in) :: c
       !DIR$ ATTRIBUTES ALIGN : 16 :: arg
       type(XMM4r4_t) :: arg
       ! EXec code ....
       arg.v  = atan2(c.im,c.re)
       
     end function carg_xmm4c4

!DIR$ ATTRIBUTES INLINE :: carg_2xmm4r4     
     pure function carg_2xmm4r4(re,im) result(arg)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: carg_2xmm4r4
       !DIR$ ATTRIBUTES VECTOR :: carg_2xmm4r4
       type(XMM4r4_t),  intent(in) :: re
       type(XMM4r4_t),  intent(in) :: im
       !DIR$ ATTRIBUTES ALIGN : 16 :: arg
       type(XMM4r4_t) :: arg
       ! EXec code ....
       arg.v = atan2(im.v,re.v)
     end function carg_2xmm4r4

!DIR$ ATTRIBUTES INLINE :: csin_xmm4c4
     pure function csin_xmm4c4(c) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: csin_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: csin_xmm4c4
       type(XMM4c4_t),  intent(in) :: c8
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       !DIR$ ATTRIBUTES ALIGN : 16 :: tre,tim
       type(XMM4r4_t) :: tre
       type(XMM4r4_t) :: tim
       ! Exec code ....
       tre = c.re
       tim = c.im
       iq.re = sin(tre.v)*cosh(tim.v)
       iq.im = cos(tre.v)*sinh(tim.v)
     end function csin_xmm4c4

!DIR$ ATTRIBUTES INLINE :: csin_xmm4r8
     pure function csin_xmm4r4(re,im) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: csin_xmm4r4
       !DIR$ ATTRIBUTES VECTOR :: csin_xmm8r8
       type(XMM4r4_t),   intent(in) :: re
       type(XMM4r4_t),   intent(in) :: im
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       ! Exec code ...
       iq.re = sin(re.v)*cosh(im.v)
       iq.im = cos(re.v)*sinh(im.v)
     end function csin_xmm4r4

!DIR$ ATTRIBUTES INLINE :: csinh_xmm4c4
     pure function csinh_xmm4c4(c) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: csinh_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: csinh_xmm4c4
       type(XMM4c4_t),   intent(in) :: c
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       !DIR$ ATTRIBUTES ALIGN : 16 :: tre,tim
       type(XMM4r4_t) :: tre
       type(XMM4r4_t) :: tim
       ! EXec code ....
       tre = c.re
       tim = c.im
       iq.re = sinh(tre.v)*cos(tim.v)
       iq.im = cosh(tre.v)*sin(tim.v)
     end function csinh_xmm4c4

!DIR$ ATTRIBUTES INLINE :: csinh_xmm4r4
     pure function csinh_xmm4r4(re,im) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: csinh_xmm4r4
       !DIR$ ATTRIBUTES VECTOR :: csinh_xmm8r8
       type(XMM4r4_t), intent(in) :: re
       type(XMM4r4_t), intent(in) :: im
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       ! EXec code ....
       iq.re = sinh(re.v)*cos(im.v)
       iq.im = cosh(re.v)*sin(im.v
     end function csinh_xmm4r4

!DIR$ ATTRIBUTES INLINE :: ccos_xmm4c4
     pure function ccos_xmm4c4(c) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ccos_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: ccos_xmm4c4
       type(XMM4c4_t),   intent(in) :: c8
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       !DIR$ ATTRIBUTES ALIGN : 16 :: tre,tim
       type(XMM4r4_t) :: tre
       type(XMM4r4_t) :: tim
       ! EXec code ....
       tre = c.re
       tim = c.im
       iq.re = cos(tre.v)*cosh(tim.v)
       iq.im = sin(tre.v)*sinh(tim.v)
     end function ccos_xmm4c4

!DIR$ ATTRIBUTES INLINE :: ccos_xmm4r4
     pure function ccos_xmm4r4(re,im) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ccos_xmm4r4
       !DIR$ ATTRIBUTES VECTOR :: ccos_xmm4r4
       type(XMM4r4_t), intent(in) :: re
       type(XMM4r4_t), intent(in) :: im
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       ! EXec code .....
       iq.re = cos(re.v)*cosh(im.v)
       iq.im = sin(re.v)*sinh(im.v)
     end function ccos_xmm4r4

!DIR$ ATTRIBUTES INLINE :: ccosh_xmm4c4
     pure function ccosh_xmm4c4(c) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ccosh_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: ccosh_xmm4c4
       type(XMM4c4_t),    intent(in) :: c
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       !DIR$ ATTRIBUTES ALIGN : 16 :: tre,tim
       type(XMM4r4_t) :: tre
       type(XMM4r4_t) :: tim
       ! EXec code ....
       tre = c.re
       tim = c.im
       iq.re = cosh(tre.v)*cos(tim.v)
       iq.im = sinh(tre.v)*sin(tim.v)
     end function ccosh_xmm4c4

!DIR$ ATTRIBUTES INLINE :: ccosh_xmm4r4
     pure function ccosh_xmm4r4(re,im) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ccosh_xmm4r4
       !DIR$ ATTRIBUTES VECTOR :: ccosh_xmm4r4
       type(XMM4r4_t),   intent(in) :: re
       type(XMM4r4_t),   intent(in) :: im
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       ! EXec code ....
       iq.re = cosh(re.v)*cos(im.v)
       iq.im = sinh(re.v)*sin(im.v)
     end function ccosh_xmm4r4

!DIR$ ATTRIBUTES INLINE :: cexp_xmm4c4
     pure function cexp_xmm4c4(c) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cexp_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: cexp_xmm4c4
       type(XMM4c4_t),  intent(in) :: c8
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       !DIR$ ATTRIBUTES ALIGN : 16 :: tre,tim
       type(XMM4r4_t) :: tre
       type(XMM4r4_t) :: tim
       ! Exec code ....
       tre = c.re
       tim = c.im
       iq.re = exp(tre.v)*cos(tim.v)
       iq.im = exp(tre.v)*sin(tim.v)
     end function cexp_xmm4c4

!DIR$ ATTRIBUTES INLINE :: ctan_xmm4c4
     pure function ctan_xmm4c4(x) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ctan_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: ctan_xmm4c4
       type(XMM4c4_t),  intent(in) :: x
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       iq = csin_xmm4c4(x)/ccos_xmm4c4(x)
     end function ctan_xmm4c4

!DIR$ ATTRIBUTES INLINE :: ctanh_xmm4c4
     pure function ctanh_xmm4c4(x) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ctanh_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: ctanh_xmm4c4
       type(XMM4c4_t),  intent(in) :: x
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       iq = csinh_xmm4c4(x)/ccosh_xmm4c4(x)
     end function ctanh_xmm4c4
     
!DIR$ ATTRIBUTES INLINE :: cexp_xmm4r4
     pure function cexp_xmm4r4(re,im) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cexp_xmm4r4
       !DIR$ ATTRIBUTES VECTOR :: cexp_xmm4r4
       type(XMM4r4_t),  intent(in) :: re
       type(XMM4r4_t),  intent(in) :: im
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       ! Exec code ....
       iq.re = exp(re.v)*cos(im.v)
       iq.im = exp(re.v)*sin(im.v)
     end function cexp_xmm4r4

!DIR$ ATTRIBUTES INLINE :: cabs_xmm4c4
     pure function cabs_xmm4c4(c) result(val)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cabs_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: cabs_xmm4c4
       type(XMM4c4_t), intent(in) :: c
       !DIR$ ATTRIBUTES ALIGN : 16 :: val
       type(XMM4r4_t) :: val
       !DIR$ ATTRIBUTES ALIGN : 16 :: tre,tim
       type(XMM4r4_t) :: tre,tim
       ! Exec code ...
       tre = c.re
       tim = c.im
       val.v = sqrt(tre.v*tre.v+tim.v*tim.v)
     end function cabs_xmm4c4

!DIR$ ATTRIBUTES INLINE :: cabs_2xmm4r4
     pure function cabs_2xmm8r8(re,im) result(val)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cabs_2xmm4r4
       !DIR$ ATTRIBUTES VECTOR :: cabs_2xmm4r4
       type(XMM4r4_t),  intent(in) :: re
       type(XMM4r4_t),  intent(in) :: im
       !DIR$ ATTRIBUTES ALIGN : 16 :: val
       type(XMM4r4_t) :: val
       ! EXec code ....
       val.v = sqrt(re.v*re.v+im.v*im.v)
     end function cabs_2xmm4r4

!DIR$ ATTRIBUTES INLINE :: cpow_xmm4c4
     pure function cpow_xmm4c4(c8,n) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cpow_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: cpow_xmm4c4
       type(XMM4c4_t), intent(in) :: c8
       real(kind=sp),       intent(in) :: n
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       !DIR$ ATTRIBUTES ALIGN : 16 :: tre,tim
       type(XMM4r4_t) :: tre,tim
       !DIR$ ATTRIBUTES ALIGN : 16 :: r,theta,pow,trig
       type(XMM4r4_t) :: r,theta,pow,trig
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
     end function cpow_xmm4c4

!DIR$ ATTRIBUTES INLINE :: cpow_2xmmtr8
     pure function cpow_2xmm4r4(re,im,n) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cpow_2xmm4r4
       !DIR$ ATTRIBUTES VECTOR :: cpow_2xmm4r4
       type(XMM4r4_t),   intent(in) :: re
       type(XMM4r4_t),   intent(in) :: im
       real(kind=sp),    intent(in) :: n
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       !DIR$ ATTRIBUTES ALIGN : 16 :: tre,tim
       type(XMM4r4_t) :: tre,tim
       !DIR$ ATTRIBUTES ALIGN : 16 :: r,theta,pow,trig
       type(XMM4r4_t) :: r,theta,pow,trig
       !EXec code ....
       r.v = sqrt(re.v*re.v+im.v*im.v)
       pow.v   = r.v**n
       theta.v = atan(im.v/re.v)
       !
       trig.v  = theta.v*n
       iq.re = pow.v*cos(trig.v)
       iq.im = pow.v*sin(trig.v)
     end function cpow_2xmm4r4

!DIR$ ATTRIBUTES INLINE :: clog_xmm4c4    
     pure function clog_xmm4c4(c) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: clog_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: clog_xmm4c4
       type(XMM4c4_t),   intent(in) :: c
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       !DIR$ ATTRIBUTES ALIGN : 16 :: t0
       type(XMM4r4_t) :: t0
       !
       ! EXec code ....
       t0 = cabs_xmm4c4(c)
       iq.re = log(t0.v)
       iq.im = carg_xmm4c4(c)
      end function clog_xmm4c4 
    
!DIR$ ATTRIBUTES INLINE :: clog_2xxmm4r4
      pure function clog_2xmm4r4(re,im) result(iq)
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: clog_2xmm4r4
        !DIR$ ATTRIBUTES VECTOR :: clog_2xmm4r4
        type(XMM4r4_t),  intent(in), value :: re
        type(XMM4r4_t),  intent(in), value :: im
        !DIR$ ATTRIBUTES ALIGN : 16 :: iq
        type(XMM4c4_t) :: iq
        !DIR$ ATTRIBUTES ALIGN : 16 :: t0
        type(XMM4r4_t) :: t0
        ! EXec code ....
        t0 = cabs_2xmm4r4(re,im)
        iq.re = log(t0.v)
        iq.im = carg_2xmm4r4(re,im)
      end function clog_2xmm4r4

!DIR$ ATTRIBUTES INLINE :: csqrt_xmm4c4
      pure function csqrt_xmm4c4(c) result(iq)
        
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: csqrt_xmm4c4
        !DIR$ ATTRIBUTES VECTOR :: csqrt_xmm4c4
        type(XMM4c4_t),   intent(in) :: c
        !DIR$ ATTRIBUTES ALIGN : 16 :: iq
        type(XMM4c4_t) :: iq
        !DIR$ ATTRIBUTES ALIGN : 16 :: t0,t1,t2
        type(XMM4r4_t) :: t0
        type(XMM4r4_t) :: t1
        type(XMM4r4_t) :: t2
        ! Exec code ....
        t0 = cabs_xmm4c4(c)
        t1.v = 0.5_sp*(t0.v+c.re)
        iq.re = sqrt(t1.v)
        t2.v = 0.5_sp*(t0.v-c.re)
        iq.im = sqrt(t2.v)
      end function csqrt_xmm4c4   
     

!DIR$ ATTRIBUTES INLINE :: csqrt_2xxmm4r4
      pure function csqrt_2xmm4r4(re,im) result(iq)
        
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: csqrt_2xmm4r4
        !DIR$ ATTRIBUTES VECTOR :: csqrt_2xmm4r4
        type(XMM4r4_t),  intent(in), value :: re
        type(XMM4r4_t),  intent(in), value :: im
        !DIR$ ATTRIBUTES ALIGN : 16 :: iq
        type(XMM4c4_t) :: iq
        !DIR$ ATTRIBUTES ALIGN : 16 :: t0,t1,t2
        type(XMM4r4_t) :: t0
        type(XMM4r4_t) :: t1
        type(XMM4r4_t) :: t2
        ! Exec code ....
        t0 = cabs_2xmm4r4(re,im)
        t1.v = 0.5_sp*(t0.v+re.v)
        iq.re = sqrt(t1.v)
        t2.v = 0.5_sp*(t0.v-re.v)
      end function csqrt_2xmm4r4

!!DIR$ ATTRIBUTES INLINE :: select_xmm4c4
!      pure function select_xmm4c4(lhs,rhs,mask) result(iq)
!        use, intrinsic :: ISO_C_BINDING
!        use mod_avx512_bindings, only : v8f64, v8f64_mask_blend_pd
!        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: select_xmm4c4
!        !DIR$ ATTRIBUTES VECTOR :: select_xmm4c4
!        type(XMM4c4_t),  intent(in) :: lhs
!        type(XMM4c4_t),  intent(in) :: ths
!        integer(c_char),      intent(in) :: mask
!        !DIR$ ATTRIBUTES ALIGN : 16 :: iq
!        type(XMM4c4_t) :: iq
!        !DIR$ ATTRIBUTES ALIGN : 16 :: lre,lim,rre,rim
!        type(v8f64) :: lre,lim,rre,rim,tre,tim
!        ! EXec code ....
!        lre.xmm = lhs.re
!        rre.xmm = rhs.re
!        tre = v8f64_mask_blend_pd(mask,lre,rre)
!        iq.re = tre.xmm
!        lim.xmm = lhs.im
!        rim.xmm = rhs.im
!        tim = v8f64_mask_blend_pd(mask,lim,rim)
!        iq.im = tim.xmm
!      end function select_xmm4c4
       
       

!!DIR$ ATTRIBUTES INLINE :: permute_xmm4c4
!      pure function permute_xmm4c4(c8,k,imm) result(iq)
!        use, intrinsic :: ISO_C_BINDING
!        use mod_avx512_bindings, only : v8f64, v8f64_mask_permute_pd
!        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: permute_xmm4c4
!        !DIR$ ATTIRBUTES VECTOR :: permute_xmm4c4
!        type(XMM4c4_t), intent(in) :: c8
!        integer(c_char),     intent(in) :: k
!        integer(c_int),      intent(in) :: imm
!        !DIR$ ATTRIBUTES ALIGN : 16 :: iq
!        type(XMM4c4_t) :: iq
!        !DIR$ ATTRIBUTES ALIGN : 16 :: tre,tim,rre,rim
!        type(v8f64) :: tre,tim,rre,rim
!        ! EXec code ....
!        tre.xmm = c8.re
!        tim.xmm = c8.im
!        rre = v8f64_mask_permute_pd(tre,k,tim,imm)
!        iq.re = rre.xmm
!        rim = v8f64_mask_permute_pd(tim,k,tre,imm)
 !       iq.im = rim.xmm
!      end function permute_xmm4c4

!!DIR$ ATTRIBUTES INLINE :: expand_xmm4c4
!      pure function maskz_expand_xmm4c4(c8,k) result(iq)
!        use, intrinsic :: ISO_C_BINDING
!        use mod_avx512_bindings, only : v8f64, v8f64_maskz_expand_pd
 !       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: maskz_expand_xmm4c4
!        !DIR$ ATTRIBUTES VECTOR :: maskz_expand_xmm4c4
!        type(XMM4c4_t),  intent(in) :: c8
!        integer(c_char),      intent(in) :: k
!        !DIR$ ATTRIBUTES ALIGN : 16 :: iq
!        type(XMM4c4_t) :: iq
!        !DIR$ ATTRIBUTES ALIGN : 16 :: tre,tim,rre,rim
!        type(v8f64) :: tre,tim,rre,rim
!        ! EXec code ....
!        tre.xmm = c8.re
!        rre = v8f64_maskz_expand_pd(k,tre)
!        iq.re = rre.xmm
!        tim.xmm = c8.im
!        rim = v8f64_maskz_expand_pd(k,tim)
!        iq.im = rim.xmm
!      end function maskz_expand_xmm4c4

!DIR$ ATTRIBUTES INLINE :: cdiv_smith
      pure function cdiv_smith(lhs,rhs) result(iq)
!#if  (USE_INTRINSIC_VECTOR_COMPARE) == 1
!        use, intrinsic :: ISO_C_BINDING
!        use mod_avx512_bindings, only : v8f64, v8f64_cmp_pd_mask
!#endif
       
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cdiv_smith
        !DIR$ ATTRIBUTES VECTOR :: cdiv_smith
        type(XMM4c4_t),  intent(in) :: lhs
        type(XMM4c4_t),  intent(in) :: rhs
        ! DIR$ ATTRIBUTES ALIGN : 16 :: iq
        type(XMM4c4_t) :: iq
        !DIR$ ATTRIBUTES ALIGN : 16 :: ratio,denom
        type(XMM4r4_t) :: ratio,denom
        !DIR$ ATTRIBUTES ALIGN : 16 :: bres
        logical(kind=i4), dimension(0:3) :: bres
!#if   (USE_INTRINSIC_VECTOR_COMPARE) == 1
!        !DIR$ ATTRIBUTES ALIGN : 16 :: tre,tim
!        type(v8f64) :: tre,tim
!        integer(c_char), automatic :: mask_gte
!        integer(c_char), parameter :: all_ones = Z'FF'
!#endif
        ! EXec code ....
        ratio = 0.0_sp
        denom = 0.0_sp
!#if   (USE_INTRINSIC_VECTOR_COMPARE) == 1
!        tre.xmm = abs(rhs.re)
!        tim.xmm = abs(rhs.im)
!        mask_gte = v8f64_cmp_pd_mask(tre,tim,29);
!#else
        bres = abs(rhs.re) >= abs(rhs.im)
!#endif
        re_part = 0.0_sp
        im_part = 0.0_sp
!#if   (USE_INTRINSIC_VECTOR_COMPARE) == 1
!        if(mask_gte == all_ones) then
!#elif
        if(all(bres)) then
!#endif
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

      
     
end module sse_cvec4
