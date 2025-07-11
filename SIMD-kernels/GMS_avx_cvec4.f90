

module avx_cvec4

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'avx_cvec4'
 !          
 !          Purpose:
 !                      This module contains a decomposed to real and imaginary
 !                      parts a complex vector of 4 elements (complex numbers)
 !                      This representation nicely fits into 2 AVX/AVX2 YMMx
 !                      registers.
 !          History:
 !                        Date: 17-01-2022
 !                        Time: 15:04 GMT+2
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

     use mod_kinds, only : i1,i4,dp
     use mod_vectypes, only : YMM4r8_t
     use,  intrinsic :: ISO_C_BINDING
     implicit none
     public
     !=====================================================59
     !  File and module information:
     !  version,creation and build date, author,description
     !=====================================================59

     ! Major version
     integer(kind=i4),  parameter :: AVX_CVEC4_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: AVX_CVEC4_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: AVX_CVEC4_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: AVX_CVEC4_FULLVER =   &
            1000*AVX_CVEC4_MAJOR+100*AVX_CVEC4_MINOR+10*AVX_CVEC4_MICRO
     ! Module creation date
     character(*),        parameter :: AVX_CVEC4_CREATE_DATE = "17-01-2022 15:04 +00200 (MON 17 JAN 2022 GMT+2)"
     ! Module build date
     character(*),        parameter :: AVX_CVEC4_BUILD_DATE  = __DATE__ 
     
     character(*),        parameter :: AVX_CVEC4_BUILD_TIME  = __TIME__
     ! Module author info
     character(*),        parameter :: AVX_CVEC4_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: AVX_CVEC4_SYNOPSIS    = "Packed complex vector of 4 elements (complex numbers)"

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
    

     type, public :: YMM4c8
        !
        sequence
        real(kind=dp), dimension(0:3) :: re
        real(kind=dp), dimension(0:3) :: im
     end type YMM4c8
       
        
        
        
      
        
    

     interface operator (+)
         module procedure ymm4c8_add_ymm4c8
         module procedure ymm4c8_add_c1
         module procedure ymm4c8_add_ymm4r8
         module procedure ymm4c8_add_s1
         module procedure c1_add_ymm4c8
         module procedure ymm4r8_add_ymm4c8
         module procedure s1_add_ymm4c8
     end interface operator (+)

     interface operator (-)
         module procedure ymm4c8_sub_ymm4c8
         module procedure ymm4c8_sub_c1
         module procedure ymm4c8_sub_ymm4r8
         module procedure ymm4c8_sub_s1
         module procedure c1_sub_ymm4c8
         module procedure ymm4r8_sub_ymm4c8
         module procedure s1_sub_ymm4c8
      end interface operator (-)
      
     interface operator (*)
         module procedure ymm4c8_mul_ymm4c8
         module procedure ymm4c8_mul_c1
         module procedure ymm4c8_mul_ymm4r8
         module procedure ymm4c8_mul_s1
         module procedure c1_mul_ymm4c8
         module procedure ymm4r8_mul_ymm4c8
         module procedure s1_mul_ymm4c8
      end interface operator (*)

      interface operator (/)
         module procedure ymm4c8_div_ymm4c8
         module procedure ymm4c8_div_c1
         module procedure ymm4c8_div_ymm4r8
         module procedure ymm4c8_div_s1
         module procedure c1_div_ymm4c8
         module procedure ymm4r8_div_ymm4c8
         module procedure s1_div_ymm4c8
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
         module procedure ymm4c8_eq_ymm4c8
         module procedure ymm4c8_eq_c1
        
         module procedure c1_eq_ymm4c8
        
      end interface operator (==)

      interface operator (/=)
         module procedure ymm4c8_neq_ymm4c8
         module procedure ymm4c8_neq_c1
       
         module procedure c1_neq_ymm4c8
       
      end interface operator (/=)

      interface operator (>)
         module procedure ymm4c8_gt_ymm4c8
         module procedure ymm4c8_gt_c1
        
         module procedure c1_gt_ymm4c8
        
      end interface operator (>)

      interface operator (<)
         module procedure ymm4c8_lt_ymm4c8
         module procedure ymm4c8_lt_c1
        
         module procedure c1_lt_ymm4c8
       
      end interface operator (<)

      interface operator (>=)
         module procedure ymm4c8_ge_ymm4c8
         module procedure ymm4c8_ge_c1
        
         module procedure c1_ge_ymm4c8
        
      end interface operator (>=)

      interface operator (<=)
         module procedure ymm4c8_le_ymm4c8
         module procedure ymm4c8_le_c1
        
         module procedure c1_le_ymm4c8
        
      end interface operator (<=)


      
      contains
!DIR$ ATTRIBUTES INLINE :: default_init
     pure   function default_init() result(iq)
          
          !DIR$ ATTRIBUTES VECTOR :: default_init 
          !DIR$ ATTRIBUTES ALIGN : 32 :: iq
          
          type(YMM4c8) :: iq
          ! Exec code
          iq.re = 0.0_dp
          iq.im = 0.0_dp
        end function default_init

!DIR$ ATTRIBUTES INLINE :: array_init
     pure   function array_init(re,im) result(iq)
         
          !DIR$ ATTRIBUTES VECTOR :: array_init
          real(kind=dp), dimension(0:3), intent(in) :: re
          real(kind=dp), dimension(0:3), intent(in) :: im
          !DIR$ ATTRIBUTES ALIGN : 32 :: iq
          type(YMM4c8) :: iq
          ! EXec code ....
          iq.re = re
          iq.im = im
       end function array_init

!DIR$ ATTRIBUTES INLINE :: complex1_init
     pure   function complex1_init(c) result(iq)
        
          complex(kind=dp), intent(in) :: c
          !DIR$ ATTRIBUTES ALIGN : 32 :: iq
          type(YMM4c8) :: iq
          ! Exec code ...
          iq.re = real(c)
          iq.im = aimag(c)
     end function complex1_init

!DIR$ ATTRIBUTES INLINE :: complex2x4_init
     pure   function complex2x4_init(c) result(iq)
        
         complex(kind=dp), dimension(0:3), intent(in) :: c
         !DIR$ ATTRIBUTES ALIGN : 32 :: iq
         type(YMM4c8) :: iq
         ! Exec code ....
         iq.re = real(c)
         iq.im = aimag(c)
     end function complex2x4_init

!DIR$ ATTRIBUTES INLINE :: ymm4r82x_init       
     pure  function ymm4r82x_init(v1,v2) result(iq)
        
         !DIR$ ATTRIBUTES VECTOR :: ymm4r82x_init
         type(YMM4r8_t),  intent(in) :: v1
         type(YMM4r8_t),  intent(in) :: v2
         !DIR$ ATTRIBUTES ALIGN : 32 :: iq
         type(YMM4c8) :: iq
         ! EXec code ....
         iq.re = v1.v
         iq.im = v2.v
     end function ymm4r82x_init

!DIR$ ATTRIBUTES INLINE :: ymm4r81x_init
     pure function ymm4r81x_init(v1) result(iq)
        
         !DIR$ ATTRIBUTES VECTOR :: ymm4r81x_init
         type(YMM4r8_t),  intent(in) :: v1
         !DIR$ ATTRIBUTES ALIGN : 32 :: iq
         type(YMM4c8) :: iq
         !DIR$ ATTRIBUTES ALIGN : 32 :: C00
         type(YMM4r8_t), parameter :: C00 = YMM4r8_t(0.0_dp)
         ! Exec code ....
         iq.re = v1.v
         iq.im = C00.v
     end function ymm4r81x_init

!DIR$ ATTRIBUTES INLINE :: r81x_init
     pure function r81x_init(s) result(iq)
         
         !DIR$ ATTRIBUTES VECTOR :: r81x_init
         real(kind=dp),  intent(in) :: s
         !DIR$ ATTRIBUTES ALIGN : 32 :: iq
         type(YMM4c8) :: iq
         !DIR$ ATTRIBUTES ALIGN : 32 :: C00
         type(YMM4r8_t), parameter :: C00 = YMM4r8_t(0.0_dp)
         ! EXec code ....
         iq.re = s
         iq.im = C00.v
     end function r81x_init

!DIR$ ATTRIBUTES INLINE ::  copy_init
     pure function copy_init(rhs) result(iq)
         
         !DIR$ ATTRIBUTES VECTOR :: copy_init
         type(YMM4c8),  intent(in) :: rhs
         !DIR$ ATTRIBUTES ALIGN : 32 :: iq
         type(YMM4c8) :: iq
         ! EXec code ...
         iq = rhs
     end function copy_init

!DIR$ ATTRIBUTES INLINE :: ymm4c8_add_ymm4c8
     pure function ymm4c8_add_ymm4c8(lhs,rhs) result(iq)
      
       !DIR$ ATTRIBUTES VECTOR :: ymm4c8_add_ymm4c8
       type(YMM4c8),  intent(in) :: lhs
       type(YMM4c8),  intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       ! EXec code ....
       iq.re = lhs.re+rhs.re
       iq.im = lhs.im+rhs.im
     end function ymm4c8_add_ymm4c8

!DIR$ ATTRIBUTES INLINE :: ymm4c8_add_c1
     pure function ymm4c8_add_c1(lhs,rhs) result(iq)
       
       !DIR$ ATTRIBUTES VECTOR :: ymm4c8_add_c1
       type(YMM4c8),  intent(in) :: lhs
       complex(kind=dp),     intent(in), value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       ! EXec code ....
       iq.re = lhs.re+real(rhs)
       iq.im = lhs.im+aimag(rhs)
     end function ymm4c8_add_c1

!DIR$ ATTRIBUTES INLINE :: ymm4c8_add_ymm4r8
     pure function ymm4c8_add_ymm4r8(lhs,rhs) result(iq)
       
      
       !DIR$ ATTRIBUTES VECTOR :: ymm4c8_add_ymm4r8
       type(YMM4c8),   intent(in) :: lhs
       type(YMM4r8_t),        intent(in),value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: C00
       type(YMM4r8_t), parameter :: C00 = YMM4r8_t(0.0_dp)
       !Exec code....
       iq.re = lhs.re+rhs.v
       iq.im = lhs.im
     end function ymm4c8_add_ymm4r8

!DIR$ ATTRIBUTES INLINE :: ymm4c8_add_s1
     pure function ymm4c8_add_s1(lhs,rhs) result(iq)
      
      
       !DIR$ ATTRIBUTES VECTOR :: ymm4c8_add_s1
       type(YMM4c8),   intent(in) :: lhs
       real(kind=dp),         intent(in), value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       iq.re = lhs.re+rhs
       iq.im = lhs.im 
     end function ymm4c8_add_s1

!DIR$ ATTRIBUTES INLINE :: c1_add_ymm4c8     
     pure function c1_add_ymm4c8(lhs,rhs) result(iq)
       !
     
       !DIR$ ATTRIBUTES VECTOR :: c1_add_ymm4c8
       complex(kind=dp),     intent(in) :: lhs
       type(YMM4c8),  intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       iq.re = real(lhs)+rhs.re
       iq.im = aimag(lhs)+rhs.im
     end function c1_add_ymm4c8

!DIR$ ATTRIBUTES INLINE :: ymm4r8_add_ymm4c8
     pure function ymm4r8_add_ymm4c8(lhs,rhs) result(iq)
       
     
       !DIR$ ATTRIBUTES VECTOR ::  ymm4r8_add_ymm4c8
       type(YMM4r8_t),      intent(in) :: lhs
       type(YMM4c8), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: C00
       type(YMM4r8_t), parameter :: C00 = YMM4r8_t(0.0_dp)
       iq.re = lhs.v+rhs.re
       iq.im = rhs.im 
     end function  ymm4r8_add_ymm4c8

!DIR$ ATTRIBUTES INLINE :: s1_add_ymm4c8
     pure function s1_add_ymm4c8(lhs,rhs) result(iq)
       
       
       !DIR$ ATTRIBUTES VECTOR :: s1_add_ymm4c8
       real(kind=dp),       intent(in) :: lhs
       type(YMM4c8), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       iq.re = lhs+rhs.re
       iq.im = rhs.im 
     end function s1_add_ymm4c8

!DIR$ ATTRIBUTES INLINE :: ymm4c8_sub_ymm4c8
     pure function ymm4c8_sub_ymm4c8(lhs,rhs) result(iq)
      
       !DIR$ ATTRIBUTES VECTOR ::  ymm4c8_sub_ymm4c8
       type(YMM4c8),    intent(in) :: lhs
       type(YMM4c8),    intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       iq.re = lhs.re-rhs.re
       iq.im = lhs.im-rhs.im
     end function  ymm4c8_sub_ymm4c8

!DIR$ ATTRIBUTES INLINE :: ymm4c8_sub_c1
     pure function ymm4c8_sub_c1(lhs,rhs) result(iq)
      
       !DIR$ ATTRIBUTES VECTOR :: ymm4c8_sub_c1
       type(YMM4c8),     intent(in) :: lhs
       complex(kind=dp),        intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       iq.re = lhs.re-real(rhs)
       iq.im = lhs.im-aimag(rhs)
     end function ymm4c8_sub_c1

!DIR$ ATTRIBUTES INLINE :: ymm4c8_sub_ymm4r8
     pure function ymm4c8_sub_ymm4r8(lhs,rhs) result(iq)
       
      
       !DIR$ ATTRIBUTES VECTOR :: ymm4c8_sub_ymm4r8
       type(YMM4c8),    intent(in) :: lhs
       type(YMM4r8_t),         intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: C00
       type(YMM4r8_t), parameter :: C00 = YMM4r8_t(0.0_dp)
       iq.re = lhs.re-rhs.v
       iq.im = lhs.im 
     end function ymm4c8_sub_ymm4r8

!DIR$ ATTRIBUTES INLINE :: ymm4c8_sub_s1
     pure function ymm4c8_sub_s1(lhs,rhs) result(iq)
       
       !DIR$ ATTRTIBUTES VECTOR :: ymm4c8_sub_s1
       type(YMM4c8),     intent(in) :: lhs
       real(kind=dp),           intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: C00
       type(YMM4r8_t), parameter :: C00 = YMM4r8_t(0.0_dp)
       iq.re = lhs.re-rhs
       iq.im = lhs.im 
     end function ymm4c8_sub_s1

!DIR$ ATTRIBUTES INLINE :: c1_sub_ymm4c8
     pure function c1_sub_ymm4c8(lhs,rhs) result(iq)
      
       !DIR$ ATTRIBUTES VECTOR :: c1_sub_ymm4c8
       complex(kind=dp),      intent(in) :: lhs
       type(YMM4c8),   intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       iq.re = real(lhs)-rhs.re
       iq.im = aimag(lhs)-rhs.im
     end function c1_sub_ymm4c8

!DIR$ ATTRIBUTES INLINE :: ymm4r8_sub_ymm4c8
     pure function ymm4r8_sub_ymm4c8(lhs,rhs) result(iq)
       
      
       !DIR$ ATTRIBUTES VECTOR :: ymm4r8_sub_ymm4c8
       type(YMM4r8_t),      intent(in) :: lhs
       type(YMM4c8), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: C00
       type(YMM4r8_t), parameter :: C00 = YMM4r8_t(0.0_dp)
       iq.re = lhs.v-rhs.re
       iq.im = rhs.im 
     end function ymm4r8_sub_ymm4c8

!DIR$ ATTRIBUTES INLINE :: s1_sub_ymm4c8
     pure function  s1_sub_ymm4c8(lhs,rhs) result(iq)
     
       !DIR$ ATTRIBUTES VECTOR ::  s1_sub_ymm4c8
       real(kind=dp),       intent(in) :: lhs
       type(YMM4c8), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: C00
       type(YMM4r8_t), parameter :: C00 = YMM4r8_t(0.0_dp)
       iq.re = lhs-rhs.re
       iq.im = rhs.im 
     end function  s1_sub_ymm4c8

!DIR$ ATTRIBUTES INLINE :: ymm4c8_mul_ymm4c8
     pure function ymm4c8_mul_ymm4c8(lhs,rhs) result(iq)
      
       !DIR$ ATTRIBUTES VECTOR :: ymm4c8_mul_ymm4c8
       type(YMM4c8),    intent(in) :: lhs
       type(YMM4c8),    intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: ymm0,ymm1,ymm2,ymm3
       type(YMM4r8_t) :: ymm0,ymm1,ymm2,ymm3
       ymm0.v = lhs.re*rhs.re
       ymm1.v = lhs.im*rhs.im
       iq.re  = ymm0.v+ymm1.v
       ymm2.v = lhs.im*rhs.re
       ymm3.v = lhs.re*rhs.im
       iq.im  = ymm2.v-ymm3.v
     end function ymm4c8_mul_ymm4c8

!DIR$ ATTRIBUTES INLINE :: ymm4c8_mul_c1
     pure function  ymm4c8_mul_c1(lhs,rhs) result(iq)
      
       !DIR$ ATTRIBUTES VECTOR ::  ymm4c8_mul_c1
       type(YMM4c8),     intent(in) :: lhs
       complex(kind=dp),        intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: ymm0,ymm1,ymm2,ymm3
       type(YMM4r8_t) :: ymm0,ymm1,ymm2,ymm3
       ymm0.v = lhs.re*real(rhs)
       ymm1.v = lhs.im*aimag(rhs)
       iq.re  = ymm0.v+ymm1.v
       ymm2.v = lhs.im*real(rhs)
       ymm3.v = lhs.re*aimag(rhs)
       iq.im  = ymm2.v-ymm3.v
     end function  ymm4c8_mul_c1

!DIR$ ATTRIBUTES INLINE :: ymm4c8_mul_ymm4r8
     pure function ymm4c8_mul_ymm4r8(lhs,rhs) result(iq)
     
       !DIR$ ATTRIBUTES VECTOR :: ymm4c8_mul_ymm4r8
       type(YMM4c8),    intent(in) :: lhs
       type(YMM4r8_t),         intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       iq.re = lhs.re*rhs.v
       iq.im = lhs.im*rhs.v
     end function ymm4c8_mul_ymm4r8

!DIR$ ATTRIBUTES INLINE :: ymm4c8_mul_s1
     pure function  ymm4c8_mul_s1(lhs,rhs) result(iq)
      
       !DIR$ ATTRIBUTES VECTOR ::  ymm4c8_mul_s1
       type(YMM4c8),    intent(in) :: lhs
       real(kind=dp),   intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       iq.re = lhs.re*rhs
       iq.im = lhs.im*rhs
     end function  ymm4c8_mul_s1

!DIR$ ATTRIBUTES INLINE :: c1_mul_ymm4c8
     pure function c1_mul_ymm4c8(lhs,rhs) result(iq)
     
       !DIR$ ATTRIBUTES VECTOR :: c1_mul_ymm4c8
       complex(kind=dp),        intent(in) :: lhs
       type(YMM4c8),     intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: ymm0,ymm1,ymm2,ymm3
       type(YMM4r8_t) :: ymm0,ymm1,ymm2,ymm3
       ymm0.v = real(lhs)*rhs.re
       ymm1.v = aimag(lhs)*rhs.im
       iq.re  = ymm0.v+ymm1.v
       ymm2.v = real(lhs)*rhs.im
       ymm3.v = aimag(lhs)*rhs.re
       iq.im  = ymm2.v-ymm3.v
     end function c1_mul_ymm4c8

!DIR$ ATTRIBUTES INLINE :: ymm4r8_mul_ymm4c8
     pure function  ymm4r8_mul_ymm4c8(lhs,rhs) result(iq)
      
       !DIR$ ATTRIBUTES VECTOR ::  ymm4r8_mul_ymm4c8
       type(YMM4r8_t),        intent(in) :: lhs
       type(YMM4c8),   intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 ::  iq 
       type(YMM4c8) :: iq
       iq.re = lhs.v*rhs.re
       iq.im = lhs.v*rhs.im
     end function  ymm4r8_mul_ymm4c8

!DIR$ ATTRIBUTES INLINE :: s1_mul_ymm4c8
     pure function s1_mul_ymm4c8(lhs,rhs) result(iq)
       
       !DIR$ ATTRIBUTES VECTOR :: s1_mul_ymm4c8
       real(kind=dp),       intent(in) :: lhs
       type(YMM4c8), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       iq.re = lhs*rhs.re
       iq.im = lhs*rhs.im
     end function s1_mul_ymm4c8

!DIR$ ATTRIBUTES INLINE :: ymm4c8_div_ymm4c8    
     pure function ymm4c8_div_ymm4c8(lhs,rhs) result(iq)
     
       !DIR$ ATTRIBUTES VECTOR ::  ymm4c8_div_ymm4c8   
       type(YMM4c8),    intent(in) :: lhs
       type(YMM4c8),    intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: ymm0,ymm1,ymm2,ymm3,den
       type(YMM4r8_t), automatic :: ymm0,ymm1,ymm2,ymm3,den
#if (USE_SAFE_COMPLEX_DIVISION) == 1
       iq = cdiv_smith(lhs,rhs)
#else
       ymm0.v = lhs.re*rhs.re
       ymm1.v = lhs.im*rhs.im
       ymm2.v = lhs.im*rhs.re
       ymm3.v = lhs.re*rhs.im
       den.v  = (rhs.re*rhs.re)+(rhs.im*rhs.im)
       iq.re  = (ymm0.v+ymm1.v)/den.v
       iq.im  = (ymm2.v-ymm3.v)/den.v
#endif
     end function  ymm4c8_div_ymm4c8   

!DIR$ ATTRIBUTES INLINE :: ymm4c8_div_c1
     pure function ymm4c8_div_c1(lhs,rhs) result(iq)
      
       !DIR$ ATTRIBUTES VECTOR :: ymm4c8_div_c1
       type(YMM4c8),   intent(in) :: lhs
       complex(kind=dp),      intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: ymm0,ymm1,ymm2,ymm3,den
       type(YMM4r8_t), automatic :: ymm0,ymm1,ymm2,ymm3,den
       ymm0.v = lhs.re*real(rhs)
       ymm1.v = lhs.im*aimag(rhs)
       ymm2.v = lhs.im*real(rhs)
       ymm3.v = lhs.re*aimag(rhs)
       den.v  = (real(rhs)*real(rhs))+ &
            (aimag(rhs)*aimag(rhs))
       iq.re = (ymm0.v+ymm1.v)/den.v
       iq.im = (ymm2.v-ymm3.v)/den.v
     end function ymm4c8_div_c1

!DIR$ ATTRIBUTES INLINE :: ymm4c8_div_ymm4r8
     pure function ymm4c8_div_ymm4r8(lhs,rhs) result(iq)
     
       !DIR$ ATTRIBUTES VECTOR :: ymm4c8_div_ymm4r8
       type(YMM4c8),  intent(in) :: lhs
       type(YMM4r8_t),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       iq.re = lhs.re/rhs.v
       iq.im = lhs.im/rhs.v
     end function ymm4c8_div_ymm4r8

!DIR$ ATTRIBUTES INLINE :: ymm4c8_div_s1
     pure  function ymm4c8_div_s1(lhs,rhs) result(iq)
      
       !DIR$ ATTRIBUTES VECTOR :: ymm4c8_div_s1
       type(YMM4c8),     intent(in) :: lhs
       real(kind=dp),           intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       iq.re = lhs.re/rhs
       iq.im = lhs.im/rhs
     end function ymm4c8_div_s1

!DIR$ ATTRIBUTES INLINE :: c1_div_ymm4c8
     pure function c1_div_ymm4c8(lhs,rhs) result(iq)
      
       !DIR$ ATTRIBUTES VECTOR :: c1_div_ymm4c8
       complex(kind=dp),       intent(in) :: lhs
       type(YMM4c8),      intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: ymm0,ymm1,ymm2,ymm3,den
       type(YMM4r8_t), automatic :: ymm0,ymm1,ymm2,ymm3,den
       real(kind=dp), automatic :: r,i
       r = real(lhs)
       i = aimag(lhs)
       ymm0.v = r*rhs.re
       ymm1.v = i*rhs.im
       ymm2.v = i*rhs.re
       ymm3.v = r*rhs.im
       den.v  = (rhs.re*rhs.re)+(rhs.im*rhs.im)
       iq.re  = (ymm0.v+ymm1.v)/den.v
       iq.im  = (ymm2.v-ymm3.v)/den.v
     end function c1_div_ymm4c8

!DIR$ ATTRIBUTES INLINE :: ymm4r8_div_ymm4c8
     pure  function ymm4r8_div_ymm4c8(lhs,rhs) result(iq)
       
       !DIR$ ATTRIBUTES VECTOR :: ymm4r8_div_ymm4c8
       type(YMM4r8_t),      intent(in) :: lhs
       type(YMM4c8), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: C00
       type(YMM4r8_t), parameter :: C00 = YMM4r8_t(0.0_dp)
       !DIR$ ATTRIBUTES ALIGN : 32 :: t0
       type(YMM4c8), automatic :: t0
       t0.re = lhs.v
       t0.im = C00.v
       iq    = t0/rhs
     end function ymm4r8_div_ymm4c8

!DIR$ ATTRIBUTES INLINE :: s1_div_ymm4c8
     pure  function s1_div_ymm4c8(lhs,rhs) result(iq)
      
       !DIR$ ATTRIBUTES VECTOR :: s1_div_ymm4c8
       real(kind=dp),       intent(in) :: lhs
       type(YMM4c8), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: C00
       type(YMM4r8_t), parameter :: C00 = YMM4r8_t(0.0_dp)
       !DIR$ ATTRIBUTES ALIGN : 32 :: t0
       type(YMM4c8), automatic :: t0
       t0.re = lhs
       t0.im = C00.v
       iq    = t0/rhs
     end function s1_div_ymm4c8

!DIR$ ATTRIBUTES INLINE :: conjugate
     pure  function conjugate(x) result(iq)
       
     
       !DIR$ ATTRIBUTES VECTOR :: conjugate
       type(YMM4c8),  intent(in) :: x
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: C00
       type(YMM4r8_t), parameter :: C00 = YMM4r8_t(0.0_dp)
       iq.re = C00.v-x.re
       iq.im = x.im
     end function conjugate
  

!DIR$ ATTRIBUTES INLINE :: ymm4c8_eq_ymm4c8
     pure function ymm4c8_eq_ymm4c8(lhs,rhs) result(bres)
       
       !DIR$ ATTRIBUTES VECTOR :: ymm4c8_eq_ymm4c8
       type(YMM4c8),       intent(in) :: lhs
       type(YMM4c8),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:3) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re == rhs.re
       bres(0) = all(mre)
       mim = lhs.im == rhs.im
       bres(1) = all(mim)
     end function ymm4c8_eq_ymm4c8

!DIR$ ATTRIBUTES INLINE :: ymm4c8_eq_c1
     pure function ymm4c8_eq_c1(lhs,rhs) result(bres)
      
       !DIR$ ATTRIBUTES VECTOR :: ymm4c8_eq_c1
       type(YMM4c8),    intent(in) :: lhs
       complex(kind=dp),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:3) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re == real(rhs)
       bres(0) = all(mre)
       mim = lhs.im == aimag(rhs)
       bres(1) = all(mim)
     end function ymm4c8_eq_c1

!DIR$ ATTRIBUTES INLINE :: c1_eq_ymm4c8
     pure function c1_eq_ymm4c8(lhs,rhs) result(bres)
     
       !DIR$ ATTRIBUTES VECTOR :: c1_eq_ymm4c8
       complex(kind=dp),        intent(in) :: lhs
       type(YMM4c8),     intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:3) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = real(lhs)  == rhs.re
       bres(0) = all(mre)
       mim = aimag(lhs) == rhs.im
       bres(1) = all(mim)
     end function c1_eq_ymm4c8

!DIR$ ATTRIBUTES INLINE :: ymm4c8_neq_ymm4c8
     pure function ymm4c8_neq_ymm4c8(lhs,rhs) result(bres)
      
       !DIR$ ATTRIBUTES VECTOR :: ymm4c8_neq_ymm4c8
       type(YMM4c8),       intent(in) :: lhs
       type(YMM4c8),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:3) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re /= rhs.re
       bres(0) = all(mre)
       mim = lhs.im /= rhs.im
       bres(1) = all(mim)
     end function ymm4c8_neq_ymm4c8

!DIR$ ATTRIBUTES INLINE :: ymm4c8_neq_c1
     pure function ymm4c8_neq_c1(lhs,rhs) result(bres)
      
       !DIR$ ATTRIBUTES VECTOR :: ymm4c8_neq_c1
       type(YMM4c8),    intent(in) :: lhs
       complex(kind=dp),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:3) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re /= real(rhs)
       bres(0) = all(mre)
       mim = lhs.im /= aimag(rhs)
       bres(1) = all(mim)
     end function ymm4c8_neq_c1

!DIR$ ATTRIBUTES INLINE :: c1_neq_ymm4c8
     pure function c1_neq_ymm4c8(lhs,rhs) result(bres)
      
       !DIR$ ATTRIBUTES VECTOR :: c1_neq_ymm4c8
       complex(kind=dp),        intent(in) :: lhs
       type(YMM4c8),     intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:3) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = (real(lhs) /= rhs.re)
       bres(0) = all(mre)
       mim = (aimag(lhs) /= rhs.im)
       bres(1) = all(mim)
     end function c1_neq_ymm4c8

!DIR$ ATTRIBUTES INLINE :: ymm4c8_gt_ymm4c8
     pure function ymm4c8_gt_ymm4c8(lhs,rhs) result(bres)
     
       !DIR$ ATTRIBUTES VECTOR :: ymm4c8_gt_ymm4c8
       type(YMM4c8),       intent(in) :: lhs
       type(YMM4c8),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:3) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re > rhs.re
       bres(0) = all(mre)
       mim = lhs.im > rhs.im
       bres(1) = all(mim)
     end function ymm4c8_gt_ymm4c8

!DIR$ ATTRIBUTES INLINE :: ymm4c8_gt_c1
     pure function ymm4c8_gt_c1(lhs,rhs) result(bres)
      
       !DIR$ ATTRIBUTES VECTOR :: ymm4c8_gt_c1
       type(YMM4c8),    intent(in) :: lhs
       complex(kind=dp),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:3) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re > real(rhs)
       bres(0) = all(mre)
       mim = lhs.im > aimag(rhs)
       bres(1) = all(mim)
     end function ymm4c8_gt_c1

!DIR$ ATTRIBUTES INLINE :: c1_gt_ymm4c8
     pure function c1_gt_ymm4c8(lhs,rhs) result(bres)
       
       !DIR$ ATTRIBUTES VECTOR :: c1_gt_ymm4c8
       complex(kind=dp),        intent(in) :: lhs
       type(YMM4c8),     intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:3) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = real(lhs)  > rhs.re
       bres(0) = all(mre)
       mim = aimag(lhs) > rhs.im
       bres(1) = all(mim)
     end function c1_gt_ymm4c8
     
!DIR$ ATTRIBUTES INLINE :: ymm4c8_lt_ymm4c8
     pure function ymm4c8_lt_ymm4c8(lhs,rhs) result(bres)
       
       !DIR$ ATTRIBUTES VECTOR :: ymm4c8_lt_ymm4c8
       type(YMM4c8),       intent(in) :: lhs
       type(YMM4c8),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i1), dimension(0:3) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re < rhs.re
       bres(0) = all(mre)
       mim = lhs.im < rhs.im
       bres(1) = all(mim)
     end function ymm4c8_lt_ymm4c8

!DIR$ ATTRIBUTES INLINE :: ymm4c8_lt_c1
     pure function ymm4c8_lt_c1(lhs,rhs) result(bres)
       
       !DIR$ ATTRIBUTES VECTOR :: ymm4c8_lt_c1
       type(YMM4c8),    intent(in) :: lhs
       complex(kind=dp),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:3) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re < real(rhs)
       bres(0) = all(mre)
       mim = lhs.im < aimag(rhs)
       bres(1) = all(mim)
     end function ymm4c8_lt_c1

!DIR$ ATTRIBUTES INLINE :: c1_lt_ymm4c8
     pure function c1_lt_ymm4c8(lhs,rhs) result(bres)
      
       !DIR$ ATTRIBUTES VECTOR :: c1_lt_ymm4c8
       complex(kind=dp),        intent(in) :: lhs
       type(YMM4c8),     intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:3) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = real(lhs) < rhs.re
       bres(0) = all(mre)
       mim = aimag(lhs) < rhs.im
       bres(1) = all(mim)
     end function c1_lt_ymm4c8

!DIR$ ATTRIBUTES INLINE :: ymm4c8_ge_ymm4c8
     pure function ymm4c8_ge_ymm4c8(lhs,rhs) result(bres)
       
       !DIR$ ATTRIBUTES VECTOR :: ymm4c8_ge_ymm4c8
       type(YMM4c8),       intent(in) :: lhs
       type(YMM4c8),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:3) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re >= rhs.re
       bres(0) = all(mre)
       mim = lhs.im >= rhs.im
       bres(1) = all(mim)
     end function ymm4c8_ge_ymm4c8

!DIR$ ATTRIBUTES INLINE :: ymm4c8_ge_c1
     pure function ymm4c8_ge_c1(lhs,rhs) result(bres)
       
       !DIR$ ATTRIBUTES VECTOR :: ymm4c8_ge_c1
       type(YMM4c8),    intent(in) :: lhs
       complex(kind=dp),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:3) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re >= real(rhs)
       bres(0) = all(mre)
       mim = lhs.im >= aimag(rhs)
       bres(1) = all(mim)
     end function ymm4c8_ge_c1

!DIR$ ATTRIBUTES INLINE :: c1_ge_ymm4c8
     pure function c1_ge_ymm4c8(lhs,rhs) result(bres)
       
       !DIR$ ATTRIBUTES VECTOR :: c1_ge_ymm4c8
       complex(kind=dp),        intent(in) :: lhs
       type(YMM4c8),     intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:3) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = real(lhs) >= rhs.re
       bres(0) = all(mre)
       mim = aimag(lhs) >= rhs.im
       bres(1) = all(mim)
     end function c1_ge_ymm4c8
     
!DIR$ ATTRIBUTES INLINE :: ymm4c8_le_ymm4c8
     pure function ymm4c8_le_ymm4c8(lhs,rhs) result(bres)
     
       !DIR$ ATTRIBUTES VECTOR :: ymm4c8_le_ymm4c8
       type(YMM4c8),       intent(in) :: lhs
       type(YMM4c8),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:3) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re <= rhs.re
       bres(0) = all(mre)
       mim = lhs.im <= rhs.im
       bres(1) = all(mim)
     end function ymm4c8_le_ymm4c8

!DIR$ ATTRIBUTES INLINE :: ymm4c8_le_c1
     pure function ymm4c8_le_c1(lhs,rhs) result(bres)
     
       !DIR$ ATTRIBUTES VECTOR :: ymm4c8_le_c1
       type(YMM4c8),    intent(in) :: lhs
       complex(kind=dp),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:3) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re <= real(rhs)
       bres(0) = all(mre)
       mim = lhs.im <= aimag(rhs)
       bres(1) = all(mim)
     end function ymm4c8_le_c1

!DIR$ ATTRIBUTES INLINE :: c1_le_ymm4c8
     pure function c1_le_ymm4c8(lhs,rhs) result(bres)
      
       !DIR$ ATTRIBUTES VECTOR :: c1_le_ymm4c8
       complex(kind=dp),        intent(in) :: lhs
       type(YMM4c8),     intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:3) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = real(lhs) <= rhs.re
       bres(0) = all(mre)
       mim = aimag(lhs) <= rhs.im
       bres(1) = all(mim)
     end function c1_le_ymm4c8
     
     

!DIR$ ATTRIBUTES INLINE :: polar
     pure function polar(rho,theta) result (iq)
    
       !DIR$ ATTRIBUTES VECTOR :: polar
       type(YMM4r8_t), intent(in) :: rho
       type(YMM4r8_t), intent(in) :: theta
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       ! EXec code ....
       iq.re = rho.v*cos(theta.v)
       iq.im = rho.v*sin(theta.v) 
     end function polar

!DIR$ ATTRIBUTES INLINE :: carg_ymm4c8
     pure function carg_ymm4c8(c8) result(arg)
      
       !DIR$ ATTRIBUTES VECTOR :: carg_ymm4c8
       type(YMM4c8),   intent(in) :: c8
       !DIR$ ATTRIBUTES ALIGN : 32 :: arg
       type(YMM4r8_t) :: arg
       ! EXec code ....
       arg.v  = atan2(c8.im,c8.re)
       
     end function carg_ymm4c8

!DIR$ ATTRIBUTES INLINE :: carg_2xymm4r8     
     pure function carg_2xymm4r8(re,im) result(arg)
     
       !DIR$ ATTRIBUTES VECTOR :: carg_2xymm4r8
       type(YMM4r8_t),  intent(in) :: re
       type(YMM4r8_t),  intent(in) :: im
       !DIR$ ATTRIBUTES ALIGN : 32 :: arg
       type(YMM4r8_t) :: arg
       ! EXec code ....
       arg.v = atan2(im.v,re.v)
     end function carg_2xymm4r8

!DIR$ ATTRIBUTES INLINE :: csin_ymm4c8
     pure function csin_ymm4c8(c8) result(iq)
     
       !DIR$ ATTRIBUTES VECTOR :: csin_ymm4c8
       type(YMM4c8),  intent(in) :: c8
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: tre,tim
       type(YMM4r8_t) :: tre
       type(YMM4r8_t) :: tim
       ! Exec code ....
       tre.v = c8.re
       tim.v = c8.im
       iq.re = sin(tre.v)*cosh(tim.v)
       iq.im = cos(tre.v)*sinh(tim.v)
     end function csin_ymm4c8

!DIR$ ATTRIBUTES INLINE :: csin_2xymm4r8
     pure function csin_2xymm4r8(re,im) result(iq)
    
       !DIR$ ATTRIBUTES VECTOR :: csin_2xymm4r8
       type(YMM4r8_t),   intent(in) :: re
       type(YMM4r8_t),   intent(in) :: im
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       ! Exec code ...
       iq.re = sin(re.v)*cosh(im.v)
       iq.im = cos(re.v)*sinh(im.v)
     end function csin_2xymm4r8

!DIR$ ATTRIBUTES INLINE :: csinh_ymm4c8
     pure function csinh_ymm4c8(c8) result(iq)
      
       !DIR$ ATTRIBUTES VECTOR :: csinh_ymm4c8
       type(YMM4c8),   intent(in) :: c8
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: tre,tim
       type(YMM4r8_t) :: tre
       type(YMM4r8_t) :: tim
       ! EXec code ....
       tre.v = c8.re
       tim.v = c8.im
       iq.re = sinh(tre.v)*cos(tim.v)
       iq.im = cosh(tre.v)*sin(tim.v)
     end function csinh_ymm4c8

!DIR$ ATTRIBUTES INLINE :: csinh_ymm4r8
     pure function csinh_ymm4r8(re,im) result(iq)
     
       !DIR$ ATTRIBUTES VECTOR :: csinh_ymm4r8
       type(YMM4r8_t), intent(in) :: re
       type(YMM4r8_t), intent(in) :: im
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       ! EXec code ....
       iq.re = sinh(re.v)*cos(im.v)
       iq.im = cosh(re.v)*sin(im.v)
     end function csinh_ymm4r8

!DIR$ ATTRIBUTES INLINE :: ccos_ymm4c8
     pure function ccos_ymm4c8(c8) result(iq)
       
       !DIR$ ATTRIBUTES VECTOR :: ccos_ymm4c8
       type(YMM4c8),   intent(in) :: c8
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: tre,tim
       type(YMM4r8_t) :: tre
       type(YMM4r8_t) :: tim
       ! EXec code ....
       tre.v = c8.re
       tim.v = c8.im
       iq.re = cos(tre.v)*cosh(tim.v)
       iq.im = sin(tre.v)*sinh(tim.v)
     end function ccos_ymm4c8

!DIR$ ATTRIBUTES INLINE :: ccos_ymm4r8
     pure function ccos_ymm4r8(re,im) result(iq)
      
       !DIR$ ATTRIBUTES VECTOR :: ccos_ymm4r8
       type(YMM4r8_t), intent(in) :: re
       type(YMM4r8_t), intent(in) :: im
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       ! EXec code .....
       iq.re = cos(re.v)*cosh(im.v)
       iq.im = sin(re.v)*sinh(im.v)
     end function ccos_ymm4r8

!DIR$ ATTRIBUTES INLINE :: ccosh_ymm4c8
     pure function ccosh_ymm4c8(c8) result(iq)
       
       !DIR$ ATTRIBUTES VECTOR :: ccosh_ymm4c8
       type(YMM4c8),    intent(in) :: c8
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: tre,tim
       type(YMM4r8_t) :: tre
       type(YMM4r8_t) :: tim
       ! EXec code ....
       tre.v = c8.re
       tim.v = c8.im
       iq.re = cosh(tre.v)*cos(tim.v)
       iq.im = sinh(tre.v)*sin(tim.v)
     end function ccosh_ymm4c8

!DIR$ ATTRIBUTES INLINE :: ccosh_2xymm4r8
     pure function ccosh_2xymm4r8(re,im) result(iq)
       
       !DIR$ ATTRIBUTES VECTOR :: ccosh_2xymm4r8
       type(YMM4r8_t),   intent(in) :: re
       type(YMM4r8_t),   intent(in) :: im
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       ! EXec code ....
       iq.re = cosh(re.v)*cos(im.v)
       iq.im = sinh(re.v)*sin(im.v)
     end function ccosh_2xymm4r8

!DIR$ ATTRIBUTES INLINE :: cexp_ymm4c8
     pure function cexp_ymm4c8(c8) result(iq)
      
       !DIR$ ATTRIBUTES VECTOR :: cexp_ymm4c8
       type(YMM4c8),  intent(in) :: c8
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: tre,tim
       type(YMM4r8_t) :: tre
       type(YMM4r8_t) :: tim
       ! Exec code ....
       tre.v = c8.re
       tim.v = c8.im
       iq.re = exp(tre.v)*cos(tim.v)
       iq.im = exp(tre.v)*sin(tim.v)
     end function cexp_ymm4c8

!DIR$ ATTRIBUTES INLINE :: ctan_ymm4c8
     pure function ctan_ymm4c8(x) result(iq)
     
       !DIR$ ATTRIBUTES VECTOR :: ctan_ymm4c8
       type(YMM4c8),  intent(in) :: x
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       iq = csin_ymm4c8(x)/ccos_ymm4c8(x)
     end function ctan_ymm4c8

!DIR$ ATTRIBUTES INLINE :: ctanh_ymm4c8
     pure function ctanh_ymm4c8(x) result(iq)
      
       !DIR$ ATTRIBUTES VECTOR :: ctanh_ymm4c8
       type(YMM4c8),  intent(in) :: x
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       iq = csinh_ymm4c8(x)/ccosh_ymm4c8(x)
     end function ctanh_ymm4c8
     
!DIR$ ATTRIBUTES INLINE :: cexp_2xymm4r8
     pure function cexp_2xymm4r8(re,im) result(iq)
     
       !DIR$ ATTRIBUTES VECTOR :: cexp_2xymm4r8
       type(YMM4r8_t),  intent(in) :: re
       type(YMM4r8_t),  intent(in) :: im
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       ! Exec code ....
       iq.re = exp(re.v)*cos(im.v)
       iq.im = exp(re.v)*sin(im.v)
     end function cexp_2xymm4r8

!DIR$ ATTRIBUTES INLINE :: cabs_ymm4c8
     pure function cabs_ymm4c8(c8) result(val)
       
       !DIR$ ATTRIBUTES VECTOR :: cabs_ymm4c8
       type(YMM4c8), intent(in) :: c8
       !DIR$ ATTRIBUTES ALIGN : 32 :: val
       type(YMM4r8_t) :: val
       !DIR$ ATTRIBUTES ALIGN : 32 :: tre,tim
       type(YMM4r8_t) :: tre,tim
       ! Exec code ...
       tre.v = c8.re
       tim.v = c8.im
       val.v = sqrt(tre.v*tre.v+tim.v*tim.v)
     end function cabs_ymm4c8

!DIR$ ATTRIBUTES INLINE :: cabs_2xymm4r8
     pure function cabs_2xymm4r8(re,im) result(val)
      
       !DIR$ ATTRIBUTES VECTOR :: cabs_2xymm4r8
       type(YMM4r8_t),  intent(in) :: re
       type(YMM4r8_t),  intent(in) :: im
       !DIR$ ATTRIBUTES ALIGN : 32 :: val
       type(YMM4r8_t) :: val
       ! EXec code ....
       val.v = sqrt(re.v*re.v+im.v*im.v)
     end function cabs_2xymm4r8

!DIR$ ATTRIBUTES INLINE :: cpow_ymm4c8
     pure function cpow_ymm4c8(c8,n) result(iq)
      
       !DIR$ ATTRIBUTES VECTOR :: cpow_ymm4c8
       type(YMM4c8), intent(in) :: c8
       real(kind=dp),       intent(in) :: n
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: tre,tim
       type(YMM4r8_t) :: tre,tim
       !DIR$ ATTRIBUTES ALIGN : 32 :: r,theta,pow,trig
       type(YMM4r8_t) :: r,theta,pow,trig
       !EXec code ....
       tre.v = c8.re
       tim.v = c8.im
       r.v = sqrt(tre.v*tre.v+tim.v*tim.v)
       pow.v   = r.v**n
       theta.v = atan(tim.v/tre.v)
       ! 
       trig.v  = theta.v*n
       iq.re = pow.v*cos(trig.v)
       iq.im = pow.v*sin(trig.v)
     end function cpow_ymm4c8

!DIR$ ATTRIBUTES INLINE :: cpow_2xymmtr8
     pure function cpow_2xymm4r8(re,im,n) result(iq)
      
       !DIR$ ATTRIBUTES VECTOR :: cpow_2xymm4r8
       type(YMM4r8_t),   intent(in) :: re
       type(YMM4r8_t),   intent(in) :: im
       real(kind=dp),    intent(in) :: n
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: tre,tim
       type(YMM4r8_t) :: tre,tim
       !DIR$ ATTRIBUTES ALIGN : 32 :: r,theta,pow,trig
       type(YMM4r8_t) :: r,theta,pow,trig
       !EXec code ....
       r.v = sqrt(re.v*re.v+im.v*im.v)
       pow.v   = r.v**n
       theta.v = atan(im.v/re.v)
       !
       trig.v  = theta.v*n
       iq.re = pow.v*cos(trig.v)
       iq.im = pow.v*sin(trig.v)
     end function cpow_2xymm4r8

!DIR$ ATTRIBUTES INLINE :: clog_ymm4c8    
     pure function clog_ymm4c8(c8) result(iq)
      
       !DIR$ ATTRIBUTES VECTOR :: clog_ymm4c8
       type(YMM4c8),   intent(in) :: c8
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM4c8) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: t0
       type(YMM4r8_t) :: t0
       !
       ! EXec code ....
       t0    = cabs_ymm4c8(c8)
       iq.re = log(t0.v)
       t0    = carg_ymm4c8(c8)
       iq.im = t0.v 
      end function clog_ymm4c8 
    
!DIR$ ATTRIBUTES INLINE :: clog_2xymm4r8
      pure function clog_2xymm4r8(re,im) result(iq)
        
        !DIR$ ATTRIBUTES VECTOR :: clog_2xymm4r8
        type(YMM4r8_t),  intent(in), value :: re
        type(YMM4r8_t),  intent(in), value :: im
        !DIR$ ATTRIBUTES ALIGN : 32 :: iq
        type(YMM4c8) :: iq
        !DIR$ ATTRIBUTES ALIGN : 32 :: t0
        type(YMM4r8_t) :: t0
        ! EXec code ....
        t0    = cabs_2xymm4r8(re,im)
        iq.re = log(t0.v)
        t0    = carg_2xymm4r8(re,im)  
        iq.im = t0.v 
      end function clog_2xymm4r8

!DIR$ ATTRIBUTES INLINE :: csqrt_ymm4c8
      pure function csqrt_ymm4c8(c8) result(iq)
               
        !DIR$ ATTRIBUTES VECTOR :: csqrt_ymm4c8
        type(YMM4c8),   intent(in) :: c8
        !DIR$ ATTRIBUTES ALIGN : 32 :: iq
        type(YMM4c8) :: iq
        !DIR$ ATTRIBUTES ALIGN : 32 :: C05
        type(YMM4r8_t), parameter :: C05 = YMM4r8_t(0.5_dp)
        !DIR$ ATTRIBUTES ALIGN : 32 :: t0,t1,t2
        type(YMM4r8_t) :: t0
        type(YMM4r8_t) :: t1
        type(YMM4r8_t) :: t2
        ! Exec code ....
        t0 = cabs_ymm4c8(c8)
        t1.v = C05.v*(t0.v+c8.re)
        iq.re = sqrt(t1.v)
        t2.v = C05.v*(t0.v-c8.re)
        iq.im = sqrt(t2.v)
      end function csqrt_ymm4c8   
     

!DIR$ ATTRIBUTES INLINE :: csqrt_2xymm4r8
      pure function csqrt_2xymm4r8(re,im) result(iq)
       
        
        !DIR$ ATTRIBUTES VECTOR :: csqrt_2xymm4r8
        type(YMM4r8_t),  intent(in), value :: re
        type(YMM4r8_t),  intent(in), value :: im
        !DIR$ ATTRIBUTES ALIGN : 32 :: iq
        type(YMM4c8) :: iq
        !DIR$ ATTRIBUTES ALIGN : 32 :: C05
        type(YMM4r8_t), parameter :: C05 = YMM4r8_t(0.5_dp)
        !DIR$ ATTRIBUTES ALIGN : 32 :: t0,t1,t2
        type(YMM4r8_t) :: t0
        type(YMM4r8_t) :: t1
        type(YMM4r8_t) :: t2
        ! Exec code ....
        t0 = cabs_2xymm4r8(re,im)
        t1.v = C05.v*(t0.v+re.v)
        iq.re = sqrt(t1.v)
        t2.v = C05.v*(t0.v-re.v)
      end function csqrt_2xymm4r8

!!DIR$ ATTRIBUTES INLINE :: select_ymm4c8
!      pure function select_ymm4c8(lhs,rhs,mask) result(iq)
!        use, intrinsic :: ISO_C_BINDING
!        use mod_avx512_bindings, only : v8f64, v8f64_mask_blend_pd
!        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: select_ymm4c8
!        !DIR$ ATTRIBUTES VECTOR :: select_ymm4c8
!        type(YMM4c8),  intent(in) :: lhs
!        type(YMM4c8),  intent(in) :: ths
!        integer(c_char),      intent(in) :: mask
!        !DIR$ ATTRIBUTES ALIGN : 32 :: iq
!        type(YMM4c8) :: iq
!        !DIR$ ATTRIBUTES ALIGN : 32 :: lre,lim,rre,rim
!        type(v8f64) :: lre,lim,rre,rim,tre,tim
!        ! EXec code ....
!        lre.ymm = lhs.re
!        rre.ymm = rhs.re
!        tre = v8f64_mask_blend_pd(mask,lre,rre)
!        iq.re = tre.ymm
!        lim.ymm = lhs.im
!        rim.ymm = rhs.im
!        tim = v8f64_mask_blend_pd(mask,lim,rim)
!        iq.im = tim.ymm
!      end function select_ymm4c8
       
       

!!DIR$ ATTRIBUTES INLINE :: permute_ymm4c8
!      pure function permute_ymm4c8(c8,k,imm) result(iq)
!        use, intrinsic :: ISO_C_BINDING
!        use mod_avx512_bindings, only : v8f64, v8f64_mask_permute_pd
!        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: permute_ymm4c8
!        !DIR$ ATTIRBUTES VECTOR :: permute_ymm4c8
!        type(YMM4c8), intent(in) :: c8
!        integer(c_char),     intent(in) :: k
!        integer(c_int),      intent(in) :: imm
!        !DIR$ ATTRIBUTES ALIGN : 32 :: iq
!        type(YMM4c8) :: iq
!        !DIR$ ATTRIBUTES ALIGN : 32 :: tre,tim,rre,rim
!        type(v8f64) :: tre,tim,rre,rim
!        ! EXec code ....
!        tre.ymm = c8.re
!        tim.ymm = c8.im
!        rre = v8f64_mask_permute_pd(tre,k,tim,imm)
!        iq.re = rre.ymm
!        rim = v8f64_mask_permute_pd(tim,k,tre,imm)
 !       iq.im = rim.ymm
!      end function permute_ymm4c8

!!DIR$ ATTRIBUTES INLINE :: expand_ymm4c8
!      pure function maskz_expand_ymm4c8(c8,k) result(iq)
!        use, intrinsic :: ISO_C_BINDING
!        use mod_avx512_bindings, only : v8f64, v8f64_maskz_expand_pd
 !       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: maskz_expand_ymm4c8
!        !DIR$ ATTRIBUTES VECTOR :: maskz_expand_ymm4c8
!        type(YMM4c8),  intent(in) :: c8
!        integer(c_char),      intent(in) :: k
!        !DIR$ ATTRIBUTES ALIGN : 32 :: iq
!        type(YMM4c8) :: iq
!        !DIR$ ATTRIBUTES ALIGN : 32 :: tre,tim,rre,rim
!        type(v8f64) :: tre,tim,rre,rim
!        ! EXec code ....
!        tre.ymm = c8.re
!        rre = v8f64_maskz_expand_pd(k,tre)
!        iq.re = rre.ymm
!        tim.ymm = c8.im
!        rim = v8f64_maskz_expand_pd(k,tim)
!        iq.im = rim.ymm
!      end function maskz_expand_ymm4c8

!DIR$ ATTRIBUTES INLINE :: cdiv_smith
      pure function cdiv_smith(lhs,rhs) result(iq)
           
        !DIR$ ATTRIBUTES VECTOR :: cdiv_smith
        type(YMM4c8),  intent(in) :: lhs
        type(YMM4c8),  intent(in) :: rhs
        ! DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(YMM4c8) :: iq
        !DIR$ ATTRIBUTES ALIGN : 32 :: ratio,denom
        type(YMM4r8_t) :: ratio,denom
        !DIR$ ATTRIBUTES ALIGN : 32 :: bres
        logical(kind=i1), dimension(0:3) :: bres

        ! EXec code ....
        ratio.v = 0.0_dp
        denom.v = 0.0_dp
        bres = abs(rhs.re) >= abs(rhs.im)
        where(bres) 
           ratio.v   = rhs.im/rhs.re
           denom.v   = rhs.re+(ratio.v*rhs.im)
           iq.re     = (lhs.re+lhs.im*ratio.v)/denom.v
           iq.im     = (lhs.im-lhs.re*ratio.v)/denom.v
        else where 
           ratio.v   = rhs.re/rhs.im
           denom.v   = rhs.im+ratio.v*rhs.re
           iq.re     = (lhs.re*ratio.v+lhs.im)/denom.v
           iq.im     = (lhs.im*ratio.v-lhs.re)/denom.v
        end where 
      end function cdiv_smith

      
     
end module avx_cvec4
