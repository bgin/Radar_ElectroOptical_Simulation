

module avx_cvec8_v2

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'avx_cvec8_v2'
 !          
 !          Purpose:
 !                      This module contains a decomposed to real and imaginary
 !                      parts a complex vector of 8 elements (complex numbers)
 !                      This representation nicely fits into 2 AVX/AVX2 YMMx
 !                      registers.
 !                      Explictly vectorized.
 !          History:
 !                        Date: 20-12-2021
 !                        Time: 15:54 GMT+2
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
     use mod_vectypes, only : YMM8r4_t
     use,  intrinsic :: ISO_C_BINDING
     implicit none
     public
     !=====================================================59
     !  File and module information:
     !  version,creation and build date, author,description
     !=====================================================59

     ! Major version
     integer(kind=i4),  parameter :: AVX_CVEC8_V2_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: AVX_CVEC8_V2_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: AVX_CVEC8_V2_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: AVX_CVEC8_V2_FULLVER =   &
            1000*AVX_CVEC8_V2_MAJOR+100*AVX_CVEC8_V2_MINOR+10*AVX_CVEC8_V2_MICRO
     ! Module creation date
     character(*),        parameter :: AVX_CVEC8_CREATE_DATE = "19-10-2023 13:48 +00200 (THR 19 OCT 2023 GMT+2)"
     ! Module build date
     character(*),        parameter :: AVX_CVEC8_BUILD_DATE  = __DATE__ " " __TIME__
     ! Module author info
     character(*),        parameter :: AVX_CVEC8_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: AVX_CVEC8_SYNOPSIS    = "Packed complex vector of 8 elements (complex numbers)"

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
    

     type, public :: YMM8c4
        ! 
        sequence
        real(kind=sp), dimension(0:7) :: re
        real(kind=sp), dimension(0:7) :: im
     end type YMM8c4
       
        
        
        
      
        
    

     interface operator (+)
         module procedure ymm8c4_add_ymm8c4
         module procedure ymm8c4_add_c1
         module procedure ymm8c4_add_ymm8r4
         module procedure ymm8c4_add_s1
         module procedure c1_add_ymm8c4
         module procedure ymm8r4_add_ymm8c4
         module procedure s1_add_ymm8c4
     end interface operator (+)

     interface operator (-)
         module procedure ymm8c4_sub_ymm8c4
         module procedure ymm8c4_sub_c1
         module procedure ymm8c4_sub_ymm8r4
         module procedure ymm8c4_sub_s1
         module procedure c1_sub_ymm8c4
         module procedure ymm8r4_sub_ymm8c4
         module procedure s1_sub_ymm8c4
      end interface operator (-)
      
     interface operator (*)
         module procedure ymm8c4_mul_ymm8c4
         module procedure ymm8c4_mul_c1
         module procedure ymm8c4_mul_ymm8r4
         module procedure ymm8c4_mul_s1
         module procedure c1_mul_ymm8c4
         module procedure ymm8r4_mul_ymm8c4
         module procedure s1_mul_ymm8c4
      end interface operator (*)

      interface operator (/)
         module procedure ymm8c4_div_ymm8c4
         module procedure ymm8c4_div_c1
         module procedure ymm8c4_div_ymm8r4
         module procedure ymm8c4_div_s1
         module procedure c1_div_ymm8c4
         module procedure ymm8r4_div_ymm8c4
         module procedure s1_div_ymm8c4
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
         module procedure ymm8c4_eq_ymm8c4
         module procedure ymm8c4_eq_c1
        
         module procedure c1_eq_ymm8c4
        
      end interface operator (==)

      interface operator (/=)
         module procedure ymm8c4_neq_ymm8c4
         module procedure ymm8c4_neq_c1
       
         module procedure c1_neq_ymm8c4
       
      end interface operator (/=)

      interface operator (>)
         module procedure ymm8c4_gt_ymm8c4
         module procedure ymm8c4_gt_c1
        
         module procedure c1_gt_ymm8c4
        
      end interface operator (>)

      interface operator (<)
         module procedure ymm8c4_lt_ymm8c4
         module procedure ymm8c4_lt_c1
        
         module procedure c1_lt_ymm8c4
       
      end interface operator (<)

      interface operator (>=)
         module procedure ymm8c4_ge_ymm8c4
         module procedure ymm8c4_ge_c1
        
         module procedure c1_ge_ymm8c4
        
      end interface operator (>=)

      interface operator (<=)
         module procedure ymm8c4_le_ymm8c4
         module procedure ymm8c4_le_c1
        
         module procedure c1_le_ymm8c4
        
      end interface operator (<=)


      
      contains
!DIR$ ATTRIBUTES INLINE :: default_init
     pure   function default_init() result(iq)
          !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: default_init
          !DIR$ ATTRIBUTES VECTOR :: default_init 
          !DIR$ ATTRIBUTES ALIGN : 32 :: iq
          type(YMM8c4) :: iq
          !DIR$ ATTRIBUTES ALIGN : 32 :: C00
          type(YMM8r4_t), parameter :: C00 = YMM8r4_t(0.0_sp)
          integer(kind=i4) :: i
          !dir$ loop_count(8)
          !dir$ vector aligned
          !dir$ vector vectorlength(4)
          !dir$ vector always
          do i=0,7
             iq.re(i) = C00.v(i)
             iq.im(i) = C00.v(i)
          end do
        end function default_init

!DIR$ ATTRIBUTES INLINE :: array_init
     pure   function array_init(re,im) result(iq)
          !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: array_init
          !DIR$ ATTRIBUTES VECTOR :: array_init
          real(kind=sp), dimension(0:7), intent(in) :: re
          real(kind=sp), dimension(0:7), intent(in) :: im
          !DIR$ ATTRIBUTES ALIGN : 32 :: iq
          type(YMM8c4) :: iq
          integer(kind=i4) :: i
          !dir$ loop_count(8)
          !dir$ vector aligned
          !dir$ vector vectorlength(4)
          !dir$ vector always
          do i=0,7
             iq.re(i) = re(i)
             iq.im(i) = im(i)
          end do
       end function array_init

!DIR$ ATTRIBUTES INLINE :: complex1_init
     pure   function complex1_init(c) result(iq)
          !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: complex1_init
          complex(kind=sp), intent(in), value :: c
          !DIR$ ATTRIBUTES ALIGN : 32 :: iq
          type(YMM8c4) :: iq
          real(kind=sp), automatic :: cr,ci
          integer(kind=i4) :: i
          cr = real(c,kind=sp)
          ci = aimag(c,kind=sp)
          !dir$ loop_count(8)
          !dir$ vector aligned
          !dir$ vector vectorlength(4)
          !dir$ vector always
          do i=0,7
             iq.re(i) = cr 
             iq.im(i) = ci
          end do 
     end function complex1_init

!DIR$ ATTRIBUTES INLINE :: complex2x8_init
     pure   function complex2x8_init(c) result(iq)
         !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: complex2x8_init
         complex(kind=sp), dimension(0:7), intent(in) :: c
         !DIR$ ATTRIBUTES ALIGN : 32 :: iq
         type(YMM8c4) :: iq
         integer(kind=i4) :: i
         !dir$ loop_count(8)
         !dir$ vector aligned
         !dir$ vector vectorlength(4)
         !dir$ vector always
         do i=0,7
            iq.re(i) = real(c(i),kind=sp)
            iq.im(i) = aimag(c(i),kind=sp)
         end do
     end function complex2x8_init

!DIR$ ATTRIBUTES INLINE :: ymm8r42x_init       
     pure  function ymm8r42x_init(v1,v2) result(iq)
         !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ymm8r42x_init
         !DIR$ ATTRIBUTES VECTOR :: ymm8r42x_init
         type(YMM8r4_t),  intent(in), value :: v1
         type(YMM8r4_t),  intent(in), value :: v2
         !DIR$ ATTRIBUTES ALIGN : 32 :: iq
         type(YMM8c4) :: iq
         integer(kind=i4) :: i
         !dir$ loop_count(8)
         !dir$ vector aligned
         !dir$ vector vectorlength(4)
         !dir$ vector always
         do i=0,7
            iq.re(i) = v1.v(i)
            iq.im(i) = v2.v(i)
         end do
     end function YMM8r42x_init

!DIR$ ATTRIBUTES INLINE :: ymm8r41x_init
     pure function ymm8r41x_init(v1) result(iq)
         !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ymm8r41x_init
         !DIR$ ATTRIBUTES VECTOR :: ymm8r41x_init
         type(YMM8r4_t),  intent(in), value :: v1
         !DIR$ ATTRIBUTES ALIGN : 32 :: iq
         type(YMM8c4) :: iq
         !DIR$ ATTRIBUTES ALIGN : 32 :: C00
         type(YMM8r4_t), parameter :: C00 = YMM8r4_t(0.0_sp)
         integer(kind=i4) :: i
         !dir$ loop_count(8)
         !dir$ vector aligned
         !dir$ vector vectorlength(4)
         !dir$ vector always
         do i=0,7
            iq.re(i) = v1.v(i)
            iq.im(i) = C00.v(i)
         end do
     end function ymm8r41x_init

!DIR$ ATTRIBUTES INLINE :: r41x_init
     pure function r41x_init(s) result(iq)
         !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: r41x_init
         !DIR$ ATTRIBUTES VECTOR :: r41x_init
         real(kind=sp),  intent(in), value :: s
         !DIR$ ATTRIBUTES ALIGN : 32 :: iq
         type(YMM8c4) :: iq
         !DIR$ ATTRIBUTES ALIGN : 32 :: C00
         type(YMM8r4_t), parameter :: C00 = YMM8r4_t(0.0_sp)
         integer(kind=i4) :: i
         !dir$ loop_count(8)
         !dir$ vector aligned
         !dir$ vector vectorlength(4)
         !dir$ vector always
         do i=0,7
            iq.re(i) = s
            iq.im(i) = C00.v(i)
         end do
     end function r41x_init

!DIR$ ATTRIBUTES INLINE ::  copy_init
     pure function copy_init(rhs) result(iq)
         !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: copy_init
         !DIR$ ATTRIBUTES VECTOR :: copy_init
         type(YMM8c4),  intent(in) :: rhs
         !DIR$ ATTRIBUTES ALIGN : 32 :: iq
         type(YMM8c4) :: iq
         integer(kind=i4) :: i
         !dir$ loop_count(8)
         !dir$ vector aligned
         !dir$ vector vectorlength(4)
         !dir$ vector always
         do i=0,7
            iq.re(i) = rhs.re(i)
            iq.im(i) = rhs.im(i)
         end do
     end function copy_init

!DIR$ ATTRIBUTES INLINE :: ymm8c4_add_ymm8c4
     pure function ymm8c4_add_ymm8c4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ymm8c4_add_ymm8c4
       !DIR$ ATTRIBUTES VECTOR :: ymm8c4_add_ymm8c4
       type(YMM8c4),  intent(in) :: lhs
       type(YMM8c4),  intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
       integer(kind=i4) :: i
         !dir$ loop_count(8)
         !dir$ vector aligned
         !dir$ vector vectorlength(4)
         !dir$ vector always
       do i=0,7
          iq.re(i) = lhs.re(i)+rhs.re(i)
          iq.im(i) = lhs.im(i)+rhs.im(i)
       end do
     end function ymm8c4_add_ymm8c4

!DIR$ ATTRIBUTES INLINE :: ymm8c4_add_c1
     pure function ymm8c4_add_c1(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ymm8c4_add_c1
       !DIR$ ATTRIBUTES VECTOR :: ymm8c4_add_c1
       type(YMM8c4),    intent(in) :: lhs
       complex(kind=sp),intent(in), value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
       real(kind=sp), automatic :: cr,ci
       integer(kind=i4) :: i
       cr = real(rhs,kind=dp)
       ci = aimag(rhs,kind=dp)
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,7
          iq.re(i) = lhs.re(i)+cr
          iq.im(i) = lhs.im(i)+ci
       end do
     end function ymm8c4_add_c1

!DIR$ ATTRIBUTES INLINE :: ymm8c4_add_ymm8r4
     pure function ymm8c4_add_ymm8r4(lhs,rhs) result(iq)
      
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ymm8c4_add_ymm8r4
       !DIR$ ATTRIBUTES VECTOR :: ymm8c4_add_ymm8r4
       type(YMM8c4),   intent(in) :: lhs
       type(YMM8r4_t),        intent(in),value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: C00
       type(YMM8r4_t), parameter :: C00 = YMM8r4_t(0.0_sp)
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,7
          iq.re(i) = lhs.re(i)+rhs.v(i)
          iq.im(i) = C00.v(i)
       end do
     end function c8_add_v8

!DIR$ ATTRIBUTES INLINE :: ymm8c4_add_s1
     pure function ymm8c4_add_s1(lhs,rhs) result(iq)
      
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ymm8c4_add_s1
       !DIR$ ATTRIBUTES VECTOR :: ymm8c4_add_s1
       type(YMM8c4),   intent(in) :: lhs
       real(kind=sp),  intent(in), value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: C00
       type(YMM8r4_t), parameter :: C00 = YMM8r4_t(0.0_sp)
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,7
          iq.re(i) = lhs.re(i)+rhs
          iq.im(i) = C00.v(i)
       end do
     end function ymm8c4_add_s1

!DIR$ ATTRIBUTES INLINE :: c1_add_ymm8c4     
     pure function c1_add_ymm8c4(lhs,rhs) result(iq)
       !
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c1_add_ymm8c4
       !DIR$ ATTRIBUTES VECTOR :: c1_add_ymm8c4
       complex(kind=sp),intent(in), value :: lhs
       type(YMM8c4),    intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
       real(kind=sp), automatic :: cr,ci
       integer(kind=i4) :: i
       cr = real(lhs,kind=dp)
       ci = aimag(lhs,kind=dp)
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,7
          iq.re(i) = cr+rhs.re(i)
          iq.im(i) = ci+rhs.im(i)
       end do
     end function c1_add_ymm8c4

!DIR$ ATTRIBUTES INLINE ymm8r4_add_ymm8c4
     pure function ymm8r4_add_ymm8c4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  ymm8r4_add_ymm8c4
       !DIR$ ATTRIBUTES VECTOR ::  ymm8r4_add_ymm8c4
       type(YMM8r4_t),intent(in), value :: lhs
       type(YMM8c4), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: C00
       type(YMM8r4_t), parameter :: C00 = YMM8r4_t(0.0_sp)
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,7
          iq.re(i) = lhs.v(i)+rhs.re(i)
          iq.im(i) = C00.v(i)
       end do
     end function  ymm8r4_add_ymm8c4

!DIR$ ATTRIBUTES INLINE :: s1_add_ymm8c4
     pure function s1_add_ymm8c4(lhs,rhs) result(iq)
       
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: s1_add_ymm8c4
       !DIR$ ATTRIBUTES VECTOR :: s1_add_ymm8c4
     pure function s1_add_ymm8c4(lhs,rhs) result(iq)
       real(kind=sp),intent(in) :: lhs
       type(YMM8c4), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: C00
       type(YMM8r4_t), parameter :: C00 = YMM8r4_t(0.0_sp)
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,7
          iq.m_re(i) = lhs+rhs.re(i)
          iq.m_im(i) = C00.v(i)
       end do
     end function s1_add_ymm8c4

!DIR$ ATTRIBUTES INLINE :: ymm8c4_sub_ymm8c4
     pure function ymm8c4_sub_ymm8c4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  ymm8c4_sub_ymm8c4
       !DIR$ ATTRIBUTES VECTOR ::  ymm8c4_sub_ymm8c4
       type(YMM8c4),    intent(in) :: lhs
       type(YMM8c4),    intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,7
          iq.re(i) = lhs.re(i)-rhs.re(i)
          iq.im(i) = lhs.im(i)-rhs.im(i)
       end do
     end function  ymm8c4_sub_ymm8c4

!DIR$ ATTRIBUTES INLINE :: ymm8c4_sub_c1
     pure function ymm8c4_sub_c1(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ymm8c4_sub_c1
       !DIR$ ATTRIBUTES VECTOR :: ymm8c4_sub_c1
       type(YMM8c4),     intent(in) :: lhs
       complex(kind=sp),intent(in), value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
       real(kind=sp), automatic :: cr,ci
       integer(kind=i4) :: i
       cr = real(rhs,kind=sp)
       ci = aimag(rhs,kind=sp)
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,7
          iq.re(i) = lhs.re(i)-cr
          iq.im(i) = lhs.im(i)-ci
       end do
     end function ymm8c4_sub_c1

!DIR$ ATTRIBUTES INLINE :: ymm8c4_sub_ymm8r4
     pure function ymm8c4_sub_ymm8r4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ymm8c4_sub_ymm8r4
       !DIR$ ATTRIBUTES VECTOR :: ymm8c4_sub_ymm8r4
       type(YMM8c4),    intent(in) :: lhs
       type(YMM8r4_t),  intent(in), value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: C00
       type(YMM8r4_t), parameter :: C00 = YMM8r4_t(0.0_sp)
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,7
          iq.re(i) = lhs.re(i)-rhs.v(i)
          iq.im(i) = C00.v(i)
       end do
     end function ymm8c4_sub_ymm8r4

!DIR$ ATTRIBUTES INLINE :: ymm8c4_sub_s1
     pure function ymm8c4_sub_s1(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ymm8c4_sub_s1
       !DIR$ ATTRTIBUTES VECTOR :: ymm8c4_sub_s1
       type(YMM8c4),     intent(in) :: lhs
       real(kind=sp),    intent(in), value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: C00
       type(YMM8r4_t), parameter :: C00 = YMM8r4_t(0.0_sp)
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,7
          iq.re(i) = lhs.re(i)-rhs
          iq.im(i) = C00.v(i)
       end do
     end function ymm8c4_sub_s1

!DIR$ ATTRIBUTES INLINE :: c1_sub_ymm8c4
     pure function c1_sub_ymm8c4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c1_sub_ymm8c4
       !DIR$ ATTRIBUTES VECTOR :: c1_sub_ymm8c4
       complex(kind=sp),intent(in), value :: lhs
       type(YMM8c4),   intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
       real(kind=sp), automatic :: cr,ci
       integer(kind=i4) :: i
       cr = real(lhs,kind=sp)
       ci = aimag(lhs,kind=sp)
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,7
          iq.re(i) = cr-rhs.re(i)
          iq.im(i) = ci-rhs.im(i)
       end do
     end function c1_sub_ymm8c4

!DIR$ ATTRIBUTES INLINE :: ymm8r4_sub_ymm8c4
     pure function ymm8r4_sub_ymm8c4(lhs,rhs) result(iq)
      
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ymm8r4_sub_ymm8c4
       !DIR$ ATTRIBUTES VECTOR :: ymm8r4_sub_ymm8c4
       type(YMM8r4_t),      intent(in) :: lhs
       type(YMM8c4), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: C00
       type(YMM8r4_t), parameter :: C00 = YMM8r4_t(0.0_sp)
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,7
          iq.re(i) = lhs.v(i)-rhs.re(i)
          iq.im(i) = C00.v(i)
       end do
     end function ymm8r4_sub_ymm8c4

!DIR$ ATTRIBUTES INLINE :: s1_sub_ymm8c4
     pure function  s1_sub_ymm8c4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  s1_sub_ymm8c4
       !DIR$ ATTRIBUTES VECTOR ::  s1_sub_ymm8c4
       real(kind=sp),       intent(in) :: lhs
       type(YMM8c4), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: C00
       type(YMM8r4_t), parameter :: C00 = YMM8r4_t(0.0_sp)
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,7
          iq.re(i) = lhs-rhs.re(i)
          iq.im(i) = C00.v(i)
       end do
     end function  s1_sub_ymm8c4

!DIR$ ATTRIBUTES INLINE :: ymm8c4_mul_ymm8c4
     pure function ymm8c4_mul_ymm8c4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ymm8c4_mul_ymm8c4
       !DIR$ ATTRIBUTES VECTOR :: ymm8c4_mul_ymm8c4
       type(YMM8c4),    intent(in) :: lhs
       type(YMM8c4),    intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: ymm0,ymm1,ymm2,ymm3
       type(YMM8r4_t) :: ymm0,ymm1,ymm2,ymm3
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,7
          ymm0.v(i) = lhs.re(i)*rhs.re(i)
          ymm1.v(i) = lhs.im(i)*rhs.im(i)
          iq.re(i)  = ymm0.v(i)-ymm1.v(i)
          ymm2.v(i) = lhs.im(i)*rhs.re(i)
          ymm3.v(i) = lhs.re(i)*rhs.im(i)
          iq.im(i)  = ymm2.v(i)-ymm3.v(i)
       end do
     end function ymm8c4_mul_ymm8c4

!DIR$ ATTRIBUTES INLINE :: ymm8c4_mul_c1
     pure function  ymm8c4_mul_c1(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  ymm8c4_mul_c1
       !DIR$ ATTRIBUTES VECTOR ::  ymm8c4_mul_c1
       type(YMM8c4),     intent(in) :: lhs
       complex(kind=sp), intent(in), value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: ymm0,ymm1,ymm2,ymm3
       type(YMM8r4_t) :: ymm0,ymm1,ymm2,ymm3
       real(kind=sp), automatic :: cr,ci
       integer(kind=i4) :: i
       cr = real(rhs,kind=dp)
       ci = aimag(rhs,kind=dp)
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,7
          ymm0.v(i) = lhs.re(i)*cr
          ymm1.v(i) = lhs.im(i)*ci
          iq.re(i)  = ymm0.v(i)-ymm1.v(i)
          ymm2.v(i) = lhs.im(i)*cr
          ymm3.v(i) = lhs.re(i)*ci
          iq.im(i)  = ymm2.v(i)-ymm3.v(i)
       end do
     end function  ymm8c4_mul_c1

!DIR$ ATTRIBUTES INLINE :: ymm8c4_mul_ymm8r4
     pure function ymm8c4_mul_ymm8r4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ymm8c4_mul_ymm8r4
       !DIR$ ATTRIBUTES VECTOR :: ymm8c4_mul_ymm8r4
       type(YMM8c4),    intent(in) :: lhs
       type(YMM8r4_t),         intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,7
          iq.re(i) = lhs.re(i)*rhs.v(i)
          iq.im(i) = lhs.im(i)*rhs.v(i)
       end do
     end function ymm8c4_mul_ymm8r4

!DIR$ ATTRIBUTES INLINE :: ymm8c4_mul_s1
     pure function  ymm8c4_mul_s1(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  ymm8c4_mul_s1
       !DIR$ ATTRIBUTES VECTOR ::  ymm8c4_mul_s1
       type(YMM8c4),    intent(in) :: lhs
       real(kind=sp),   intent(in), :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,7
          iq.re(i) = lhs.re(i)*rhs
          iq.im(i) = lhs.im(i)*rhs
       end do
     end function  ymm8c4_mul_s1

!DIR$ ATTRIBUTES INLINE :: c1_mul_ymm8c4
     pure function c1_mul_ymm8c4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c1_mul_ymm8c4
       !DIR$ ATTRIBUTES VECTOR :: c1_mul_ymm8c4
       complex(kind=sp), intent(in), value :: lhs
       type(YMM8c4),     intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: ymm0,ymm1,ymm2,ymm3
       type(YMM8r4_t) :: ymm0,ymm1,ymm2,ymm3
       real(kind=sp), automatic :: cr,ci
       integer(kind=i4) :: i
       cr = real(lhs,kind=sp)
       ci = aimag(lhs,kind=sp)
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,7
           ymm0.v(i) = cr*rhs.re(i)
           ymm1.v(i) = ci*rhs.im(i)
           iq.re(i)  = ymm0.v(i)+ymm1.v(i)
           ymm2.v(i) = cr*rhs.im(i)
           ymm3.v(i) = ci*rhs.re(i)
           iq.im(i)  = ymm2.v(i)-ymm3.v(i)
       end do
     end function c1_mul_ymm8c4

!DIR$ ATTRIBUTES INLINE :: ymm8r4_mul_ymm8c4
     pure function  ymm8r4_mul_ymm8c4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  ymm8r4_mul_ymm8c4
       !DIR$ ATTRIBUTES VECTOR ::  ymm8r4_mul_ymm8c4
       type(YMM8r4_t), intent(in), value :: lhs
       type(YMM8c4),   intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 ::  ymm8r4_mul_ymm8c4
       type(YMM8c4) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,7
          iq.re(i) = lhs.v(i)*rhs.re(i)
          iq.im(i) = lhs.v(i)*rhs.im(i)
       end do
     end function  ymm8r4_mul_ymm8c4

!DIR$ ATTRIBUTES INLINE :: s1_mul_ymm8c4
     pure function s1_mul_ymm8c4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: s1_mul_ymm8c4
       !DIR$ ATTRIBUTES VECTOR :: s1_mul_ymm8c4
       real(kind=sp),intent(in), value :: lhs
       type(YMM8c4), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,7
          iq.re(i) = lhs*rhs.re(i)
          iq.im(i) = lhs*rhs.im(i)
       end do
     end function s1_mul_ymm8c4

!DIR$ ATTRIBUTES INLINE :: ymm8c4_div_ymm8c4    
     pure function ymm8c4_div_ymm8c4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  ymm8c4_div_ymm8c4   
       !DIR$ ATTRIBUTES VECTOR ::  ymm8c4_div_ymm8c4   
       type(YMM8c4),    intent(in) :: lhs
       type(YMM8c4),    intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(YMM8c4) :: iq
       !DIR$ ATTRIBUTES ALIGN : 64 :: ymm0,ymm1,ymm2,ymm3,den
       type(YMM8r4_t), automatic :: ymm0,ymm1,ymm2,ymm3,den
       integer(kind=i4) :: i
#if (USE_SAFE_COMPLEX_DIVISION) == 1
       iq = cdiv_smith(lhs,rhs)
#else
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,7
          ymm0.v(i) = lhs.re(i)*rhs.re(i)
          ymm1.v(i) = lhs.im(i)*rhs.im(i)
          ymm2.v(i) = lhs.im(i)*rhs.re(i)
          ymm3.v(i) = lhs.re(i)*rhs.im(i)
          den.v(i)  = (rhs.re(i)*rhs.re(i))+ &
                      (rhs.im(i)*rhs.im(i))
          iq.re(i)  = (ymm0.v(i)+ymm1.v(i))/den.v(i)
          iq.im(i)  = (ymm2.v(i)-ymm3.v(i))/den.v(i)
       end do
#endif
     end function  ymm8c4_div_ymm8c4   

!DIR$ ATTRIBUTES INLINE :: ymm8c4_div_c1
     pure function ymm8c4_div_c1(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ymm8c4_div_c1
       !DIR$ ATTRIBUTES VECTOR :: ymm8c4_div_c1
       type(YMM8c4),   intent(in) :: lhs
       complex(kind=sp),intent(in), value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
       !DIR$ ATTRIBUTES ALIGN : 64 :: ymm0,ymm1,ymm2,ymm3,den
       type(YMM8c4), automatic :: ymm0,ymm1,ymm2,ymm3,den
       real(kind=sp), automatic :: cr,ci
       integer(kind=i4) :: i
       cr = real(rhs,kind=sp)
       ci = aimag(rhs,kind=sp)
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,7
          ymm0.v(i) = lhs.re(i)*cr
          ymm1.v(i) = lhs.im(i)*ci
          ymm2.v(i) = lhs.im(i)*cr
          ymm3.v(i) = lhs.re(i)*ci
          den.v(i)  = (cr*cr)+(ci*ci)
          iq.re(i) = (ymm0.v(i)+ymm1.v(i))/den.v(i)
          iq.im(i) = (ymm2.v(i)-ymm3.v(i))/den.v(i)
       end do
     end function ymm8c4_div_c1

!DIR$ ATTRIBUTES INLINE :: ymm8c4_div_ymm8r4
     pure function ymm8c4_div_ymm8r4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ymm8c4_div_ymm8r4
       !DIR$ ATTRIBUTES VECTOR :: ymm8c4_div_ymm8r4
       type(YMM8c4),  intent(in) :: lhs
       type(YMM8r8_t),intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,7
          iq.re(i) = lhs.re(i)/rhs.v(i)
          iq.im(i) = lhs.im(i)/rhs.v(i)
       end do
     end function ymm8c4_div_ymm8r4

!DIR$ ATTRIBUTES INLINE :: ymm8c4_div_s1
     pure function ymm8c4_div_s1(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ymm8c4_div_s1
       !DIR$ ATTRIBUTES VECTOR :: ymm8c4_div_s1
       type(YMM8c4),  intent(in) :: lhs
       real(kind=sp), intent(in), value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,7
          iq.re = lhs.re(i)/rhs
          iq.im = lhs.im(i)/rhs
       end do
     end function ymm8c4_div_s1

!DIR$ ATTRIBUTES INLINE :: c1_div_ymm8c4
     pure function c1_div_ymm8c4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c1_div_ymm8c4
       !DIR$ ATTRIBUTES VECTOR :: c1_div_ymm8c4
       complex(kind=sp), intent(in), value :: lhs
       type(YMM8c4),      intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: ymm0,ymm1,ymm2,ymm3,den
       type(YMM8c4), automatic :: ymm0,ymm1,ymm2,ymm3,den
       real(kind=sp), automatic :: cr,ci
       integer(kind=i4) :: i
       cr = real(lhs,kind=sp)
       ci = aimag(lhs,kind=sp)
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,7
           ymm0.v(i) = cr*rhs.re(i)
           ymm1.v(i) = ci*rhs.im(i)
           ymm2.v(i) = ci*rhs.re(i)
           ymm3.v(i) = cr*rhs.im(i)
           den.v(i)  = (rhs.re(i)*rhs.re(i))+ &
                       (rhs.im(i)*rhs.im(i))
           iq.re(i)  = (ymm0.v(i)+ymm1.v(i))/den.v(i)
           iq.im(i)  = (ymm2.v(i)-ymm3.v(i))/den.v(i)
       end do
     end function c1_div_ymm8c4

!DIR$ ATTRIBUTES INLINE :: ymm8r4_div_ymm8c4
     pure function ymm8r4_div_ymm8c4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ymm8r4_div_ymm8c4
       !DIR$ ATTRIBUTES VECTOR :: ymm8r4_div_ymm8c4
       type(YMM8r4_t),intent(in) :: lhs
       type(YMM8c4), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: C00
       type(YMM8r4_t), parameter :: C00 = YMM8r4_t(0.0_sp)
       !DIR$ ATTRIBUTES ALIGN : 32 :: t0
       type(YMM8c4), automatic :: t0
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,7
          t0.re(i) = lhs.v(i) 
          t0.im(i) = C00.v(i)
       end do
       iq = t0/rhs
     end function ymm8r4_div_ymm8c4

!DIR$ ATTRIBUTES INLINE :: s1_div_ymm8c4
     pure function s1_div_ymm8c4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: s1_div_ymm8c4
       !DIR$ ATTRIBUTES VECTOR :: s1_div_ymm8c4
       real(kind=sp),       intent(in) :: lhs
       type(YMM8c4), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: C00
       type(YMM8r4_t), parameter :: C00 = YMM8r4_t(0.0_sp)
       !DIR$ ATTRIBUTES ALIGN : 32 :: t0
       type(YMM8c4), automatic :: t0
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,7
          t0.re(i) = lhs
          t0.im(i) = C00.v(i)
       end do
       iq = t0/rhs
     end function s1_div_ymm8c4

!DIR$ ATTRIBUTES INLINE :: conjugate
     pure function conjugate(x) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: conjugate
       !DIR$ ATTRIBUTES VECTOR :: conjugate
       type(YMM8c4),  intent(in) :: x
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: C00
       type(YMM8r4_t), parameter :: C00 = YMM8r4_t(0.0_sp)
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,7
          iq.re(i) = C00.v(i)-x.re(i)
          iq.im(i) = x.im(i)
       end do
     end function conjugate
     

     
!DIR$ ATTRIBUTES INLINE :: ymm8c4_eq_ymm8c4
     pure function ymm8c4_eq_ymm8c4(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ymm8c4_eq_ymm8c4
       !DIR$ ATTRIBUTES VECTOR :: ymm8c4_eq_ymm8c4
       type(YMM8c4),       intent(in) :: lhs
       type(YMM8c4),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:7) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re == rhs.re
       bres(0) = all(mre)
       mim = lhs.im == rhs.im
       bres(1) = all(mim)
     end function ymm8c4_eq_ymm8c4

!DIR$ ATTRIBUTES INLINE :: ymm8c4_eq_c1
     pure function ymm8c4_eq_c1(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  ymm8c4_eq_c1
       !DIR$ ATTRIBUTES VECTOR ::  ymm8c4_eq_c1
       type(YMM8c4),    intent(in) :: lhs
       complex(kind=sp),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:7) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re == real(rhs,kind=dp)
       bres(0) = all(mre)
       mim = lhs.im == aimag(rhs,kind=dp)
       bres(1) = all(mim)
     end function  ymm8c4_eq_c1

!DIR$ ATTRIBUTES INLINE :: c1_eq_ymm8c4
     pure function  c1_eq_ymm8c4(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  c1_eq_ymm8c4
       !DIR$ ATTRIBUTES VECTOR ::  c1_eq_ymm8c4
       complex(kind=sp),        intent(in) :: lhs
       type(YMM8c4),     intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:7) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = real(lhs,kind=dp)  == rhs.re
       bres(0) = all(mre)
       mim = aimag(lhs,kind=dp) == rhs.im
       bres(1) = all(mim)
     end function  c1_eq_ymm8c4

!DIR$ ATTRIBUTES INLINE :: ymm8c4_neq_ymm8c4
     pure function ymm8c4_neq_ymm8c4(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: ymm8c4_neq_ymm8c4
       !DIR$ ATTRIBUTES VECTOR :: ymm8c4_neq_ymm8c4
       type(YMM8c4),       intent(in) :: lhs
       type(YMM8c4),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=int4), dimension(0:7) :: mre,mim
       logical(kind=int1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re /= rhs.re
       bres(0) = all(mre)
       mim = lhs.im /= rhs.im
       bres(1) = all(mim)
     end function ymm8c4_neq_ymm8c4

!DIR$ ATTRIBUTES INLINE :: ymm8c4_neq_c1
     pure function ymm8c4_neq_c1(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ymm8c4_neq_c1
       !DIR$ ATTRIBUTES VECTOR :: ymm8c4_neq_c1
       type(YMM8c4),    intent(in) :: lhs
       complex(kind=sp),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:7) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re /= real(rhs,kind=sp)
       bres(0) = all(mre)
       mim = lhs.im /= aimag(rhs,kind=sp)
       bres(1) = all(mim)
     end function ymm8c4_neq_c1

!DIR$ ATTRIBUTES INLINE :: c1_neq_ymm8c4
     pure function c1_neq_ymm8c4(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c1_neq_ymm8c4
       !DIR$ ATTRIBUTES VECTOR :: c1_neq_ymm8c4
       complex(kind=sp),        intent(in) :: lhs
       type(YMM8c4),     intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:7) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = real(lhs,kind=sp)  /= rhs.re
       bres(0) = all(mre)
       mim = aimag(lhs,kind=sp) /= rhs.im
       bres(1) = all(mim)
     end function c1_neq_ymm8c4

!DIR$ ATTRIBUTES INLINE :: ymm8c4_gt_ymm8c4
     pure function  ymm8c4_gt_ymm8c4(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  ymm8c4_gt_ymm8c4
       !DIR$ ATTRIBUTES VECTOR ::  ymm8c4_gt_ymm8c4
       type(YMM8c4),       intent(in) :: lhs
       type(YMM8c4),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:7) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re > rhs.re
       bres(0) = all(mre)
       mim = lhs.im > rhs.im
       bres(1) = all(mim)
     end function  ymm8c4_gt_ymm8c4

!DIR$ ATTRIBUTES INLINE :: ymm8c4_gt_c1
     pure function ymm8c4_gt_c1(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: ymm8c4_gt_c1
       !DIR$ ATTRIBUTES VECTOR :: ymm8c4_gt_c1
       type(YMM8c4),    intent(in) :: lhs
       complex(kind=sp),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:7) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re > real(rhs,kind=sp)
       bres(0) = all(mre)
       mim = lhs.im > aimag(rhs,kind=sp)
       bres(1) = all(mim)
     end function ymm8c4_gt_c1

!DIR$ ATTRIBUTES INLINE :: c1_gt_ymm8c4
     pure function  c1_gt_ymm8c4(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  c1_gt_ymm8c4
       !DIR$ ATTRIBUTES VECTOR ::  c1_gt_ymm8c4
       complex(kind=sp),        intent(in) :: lhs
       type(YMM8c4),     intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:7) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = real(lhs,kind=dp)  > rhs.re
       bres(0) = all(mre)
       mim = aimag(lhs,kind=dp) > rhs.im
       bres(1) = all(mim)
     end function  c1_gt_ymm8c4
     
!DIR$ ATTRIBUTES INLINE :: ymm8c4_lt_ymm8c4
     pure function ymm8c4_lt_ymm8c4(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ymm8c4_lt_ymm8c4
       !DIR$ ATTRIBUTES VECTOR :: ymm8c4_lt_ymm8c4
       type(YMM8c4),       intent(in) :: lhs
       type(YMM8c4),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:7) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re < rhs.re
       bres(0) = all(mre)
       mim = lhs.im < rhs.im
       bres(1) = all(mim)
     end function ymm8c4_lt_ymm8c4

!DIR$ ATTRIBUTES INLINE :: ymm8c4_lt_c1
     pure function ymm8c4_lt_c1(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ymm8c4_lt_c1
       !DIR$ ATTRIBUTES VECTOR :: ymm8c4_lt_c1
       type(YMM8c4),    intent(in) :: lhs
       complex(kind=sp),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:7) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re < real(rhs,kind=sp)
       bres(0) = all(mre)
       mim = lhs.im < aimag(rhs,kind=sp)
       bres(1) = all(mim)
     end function ymm8c4_lt_c1

!DIR$ ATTRIBUTES INLINE :: c1_lt_ymm8c4
     pure function c1_lt_ymm8c4(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c1_lt_ymm8c4
       !DIR$ ATTRIBUTES VECTOR :: c1_lt_ymm8c4
       complex(kind=sp),        intent(in) :: lhs
       type(YMM8c4),     intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:7) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = real(lhs,kind=sp) < rhs.re
       bres(0) = all(mre)
       mim = aimag(lhs,kind=sp) < rhs.im
       bres(1) = all(mim)
     end function c1_lt_ymm8c4
     
!DIR$ ATTRIBUTES INLINE :: ymm8c4_ge_ymm8c4
     pure function ymm8c4_ge_ymm8c4(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ymm8c4_ge_ymm8c4
       !DIR$ ATTRIBUTES VECTOR :: ymm8c4_ge_ymm8c4
       type(YMM8c4),       intent(in) :: lhs
       type(YMM8c4),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:7) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re >= rhs.re
       bres(0) = all(mre)
       mim = lhs.im >= rhs.im
       bres(1) = all(mim)
     end function ymm8c4_ge_ymm8c4

!DIR$ ATTRIBUTES INLINE :: ymm8c4_ge_c1
     pure function ymm8c4_ge_c1(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ymm8c4_ge_c1
       !DIR$ ATTRIBUTES VECTOR :: ymm8c4_ge_c1
       type(YMM8c4),    intent(in) :: lhs
       complex(kind=sp),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:7) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re >= real(rhs,kind=sp)
       bres(0) = all(mre)
       mim = lhs.im >= aimag(rhs,kind=sp)
       bres(1) = all(mim)
     end function ymm8c4_ge_c1

!DIR$ ATTRIBUTES INLINE :: c1_ge_ymm8c4
     pure function c1_ge_ymm8c4(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c1_ge_ymm8c4
       !DIR$ ATTRIBUTES VECTOR :: c1_ge_ymm8c4
       complex(kind=sp),        intent(in) :: lhs
       type(YMM8c4),     intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:7) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = real(lhs,kind=sp) >= rhs.re
       bres(0) = all(mre)
       mim = aimag(lhs,kind=sp) >= rhs.im
       bres(1) = all(mim)
     end function c1_ge_ymm8c4
     
!DIR$ ATTRIBUTES INLINE :: ymm8c4_le_ymm8c4
     pure function ymm8c4_le_ymm8c4(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ymm8c4_le_ymm8c4
       !DIR$ ATTRIBUTES VECTOR :: ymm8c4_le_ymm8c4
       type(YMM8c4),       intent(in) :: lhs
       type(YMM8c4),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:7) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re <= rhs.re
       bres(0) = all(mre)
       mim = lhs.im <= rhs.im
       bres(1) = all(mim)
     end function ymm8c4_le_ymm8c4

!DIR$ ATTRIBUTES INLINE :: ymm8c4_le_c1
     pure function c8_le_c1(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: ymm8c4_le_c1
       !DIR$ ATTRIBUTES VECTOR :: ymm8c4_le_c1
       type(YMM8c4),    intent(in) :: lhs
       complex(kind=sp),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:7) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re <= real(rhs,kind=dp)
       bres(0) = all(mre)
       mim = lhs.im <= aimag(rhs,kind=dp)
       bres(1) = all(mim)
     end function ymm8c4_le_c1

!DIR$ ATTRIBUTES INLINE :: c1_le_ymm8c4
     pure function c1_le_ymm8c4(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c1_le_ymm8c4
       !DIR$ ATTRIBUTES VECTOR :: c1_le_ymm8c4
       complex(kind=sp),        intent(in) :: lhs
       type(YMM8c4),     intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:7) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = real(lhs,kind=sp) <= rhs.re
       bres(0) = all(mre)
       mim = aimag(lhs,kind=sp) <= rhs.im
       bres(1) = all(mim)
     end function c1_le_ymm8c4
     
     
!#endif
!DIR$ ATTRIBUTES INLINE :: polar
     pure function polar(rho,theta) result (iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: polar
       !DIR$ ATTRIBUTES VECTOR :: polar
       type(YMM8r4_t), intent(in) :: rho
       type(YMM8r4_t), intent(in) :: theta
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
        integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,7
          iq.re(i) = rho.v(i)*cos(theta.v(i))
          iq.im(i) = rho.v(i)*sin(theta.v(i)) 
       end do
     end function polar
     
!DIR$ ATTRIBUTES INLINE :: carg_ymm8c4
     pure function carg_ymm8c4(c4) result(arg)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: carg_ymm8c4
       !DIR$ ATTRIBUTES VECTOR :: carg_ymm8c4
       type(YMM8c4),   intent(in) :: c4
       !DIR$ ATTRIBUTES ALIGN : 32 :: arg
       type(YMM8r4_t) :: arg
        integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,7
          arg.v(i)  = atan2(c4.im(i),c4.re(i))
       end do
     end function carg_ymm8c4
     
!DIR$ ATTRIBUTES INLINE :: carg_ymm8c4     
     pure function cmag_ymm8c4(c4) result(mag)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cmag_ymm8c4
       !DIR$ ATTRIBUTES VECTOR :: cmag_ymm8c4
       type(YMM8c4),   intent(in) :: c4
       !DIR$ ATTRIBUTES ALIGN : 32 :: mag
       type(YMM8r4_t) :: mag
       type(YMM8r4_t), automatic :: tre,tim
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,7
          tre.v(i) = c4.re(i)
          tim.v(i) = c4.im(i)
          mag.v(i) = sqrt(tre.v(i)*tre.v(i)+ &
                          tim.v(i)*tim.v(i))
       end do
     end function cmag_ymm8c4

!DIR$ ATTRIBUTES INLINE :: carg_2xymm8r4     
     pure function carg_2xymm8r4(re,im) result(arg)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: carg_2xymm8r8
       !DIR$ ATTRIBUTES VECTOR :: carg_2xymm8r8
       type(YMM8r4_t),  intent(in) :: re
       type(YMM8r4_t),  intent(in) :: im
       !DIR$ ATTRIBUTES ALIGN : 32 :: arg
       type(YMM8r4_t) :: arg
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,7
          arg.v(i) = atan2(im.v(i),re.v(i))
       end do
     end function carg_2xymm8r4

!DIR$ ATTRIBUTES INLINE :: csin_ymm8c4
     pure function csin_ymm8c4(c8) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: csin_ymm8c4
       !DIR$ ATTRIBUTES VECTOR :: csin_ymm8c4
       type(YMM8c4),  intent(in) :: c8
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: tre,tim
       type(YMM8r4_t) :: tre
       type(YMM8r4_t) :: tim
        integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,7
          tre.v(i) = c8.re(i)
          tim.v(i) = c8.im(i)
          iq.re(i) = sin(tre.v(i))*cosh(tim.v(i))
          iq.im(i) = cos(tre.v(i))*sinh(tim.v(i))
       end do
     end function csin_ymm8c4

!DIR$ ATTRIBUTES INLINE :: csin_2xymm8r4
     pure function csin_2xymm8r4(re,im) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: csin_2xymm8r4
       !DIR$ ATTRIBUTES VECTOR :: csin_2xymm8r4
       type(YMM8r4_t),   intent(in) :: re
       type(YMM8r4_t),   intent(in) :: im
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(YMM8c4) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,7
          iq.re(i) = sin(re.v(i))*cosh(im.v(i))
          iq.im(i) = cos(re.v(i))*sinh(im.v(0))
       end do
     end function csin_2xymm8r4

!DIR$ ATTRIBUTES INLINE :: csinh_ymm8c4
     pure function csinh_ymm8c4(c8) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: csinh_ymm8c4
       !DIR$ ATTRIBUTES VECTOR :: csinh_ymm8c4
       type(YMM8c4),   intent(in) :: c8
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: tre,tim
       type(YMM8r4_t) :: tre
       type(YMM8r4_t) :: tim
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,7
          tre.v(i) = c8.re(i)
          tim.v(i) = c8.im(i)
          iq.re(i) = sinh(tre.v(i))*cos(tim.v(i))
          iq.im(i) = cosh(tre.v(i))*sin(tim.v(i))
       end do
     end function csinh_ymm8c4

!DIR$ ATTRIBUTES INLINE :: csinh_2xymm8r4
     pure function csinh_2xymm8r4(re,im) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: csinh_2xymm8r4
       !DIR$ ATTRIBUTES VECTOR :: csinh_2xymm8r4
       type(YMM8r4_t), intent(in) :: re
       type(YMM8r4_t), intent(in) :: im
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
        do i=0,7
           iq.re(i) = sinh(re.v(i))*cos(im.v(i))
           iq.im(i) = cosh(re.v(i))*sin(im.v(i))
        end do
     end function csinh_2xymm8r4

!DIR$ ATTRIBUTES INLINE :: ccos_ymm8c4
     pure function ccos_ymm8c4(c8) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ccos_ymm8c4
       !DIR$ ATTRIBUTES VECTOR :: ccos_ymm8c4
       type(YMM8c4),   intent(in) :: c8
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: tre,tim
       type(YMM8r4_t) :: tre
       type(YMM8r4_t) :: tim
        integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
        do i=0,7
           tre.v(i) = c8.re(i)
           tim.v(i) = c8.im(i)
           iq.re = cos(tre.v(i))*cosh(tim.v(i))
           iq.im = sin(tre.v(i))*sinh(tim.v(i))
        end do
     end function ccos_ymm8c4

!DIR$ ATTRIBUTES INLINE :: ccos_2xymm8r4
     pure function ccos_2xymm8r4(re,im) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ccos_2xymm8r4
       !DIR$ ATTRIBUTES VECTOR :: ccos_2xymm8r4
       type(YMM8r4_t), intent(in) :: re
       type(YMM8r4_t), intent(in) :: im
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
        do i=0,7
           iq.re(i) = cos(re.v(i))*cosh(im.v(i))
           iq.im(i) = sin(re.v(i))*sinh(im.v(i))
        end do
     end function ccos_2xymm8r4

!DIR$ ATTRIBUTES INLINE :: ccosh_ymm8c4
     pure function ccosh_ymm8c4(c8) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ccosh_ymm8c4
       !DIR$ ATTRIBUTES VECTOR :: ccosh_ymm8c4
       type(YMM8c4),    intent(in) :: c8
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: tre,tim
       type(YMM8r4_t) :: tre
       type(YMM8r4_t) :: tim
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
        do i=0,7
           tre.v(i) = c8.re(i)
           tim.v(i) = c8.im(i)
           iq.re(i) = cosh(tre.v(i))*cos(tim.v(i))
           iq.im(i) = sinh(tre.v(i))*sin(tim.v(i))
        end do
     end function ccosh_ymm8c4

!DIR$ ATTRIBUTES INLINE :: ccosh_2xymm8r4
     pure function ccosh_2xymm8r4(re,im) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ccosh_2xymm8r4
       !DIR$ ATTRIBUTES VECTOR :: ccosh_2xymm8r4
       type(YMM8r4_t),   intent(in) :: re
       type(YMM8r4_t),   intent(in) :: im
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
        do i=0,7
           iq.re(i) = cosh(re.v(i))*cos(im.v(i))
           iq.im(i) = sinh(re.v(i))*sin(im.v(i))
        end do
     end function ccosh_2xymm8r4

!DIR$ ATTRIBUTES INLINE :: cexp_ymm8c4
     pure function cexp_ymm8c4(c8) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cexp_ymm8c4
       !DIR$ ATTRIBUTES VECTOR :: cexp_ymm8c4
       type(YMM8c4),  intent(in) :: c8
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: tre,tim
       type(YMM8r4_t) :: tre
       type(YMM8r4_t) :: tim
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
        do i=0,7
           tre.v(i) = c8.re(i)
           tim.v(i) = c8.im(i)
           iq.re(i) = exp(tre.v(i))*cos(tim.v(i))
           iq.im(i) = exp(tre.v(i))*sin(tim.v(i))
        end do
     end function cexp_ymm8c4

!DIR$ ATTRIBUTES INLINE :: ctan_ymm8c4
     pure function ctan_ymm8c4(x) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ctan_ymm8c4
       !DIR$ ATTRIBUTES VECTOR :: ctan_ymm8c4
       type(YMM8c4),  intent(in) :: x
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
       iq = csin_ymm8c4(x)/ccos_ymm8c4(x)
     end function ctan_ymm8c4

!DIR$ ATTRIBUTES INLINE :: ctanh_ymm8c4
     pure function ctanh_ymm8c4(x) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ctanh_ymm8c4
       !DIR$ ATTRIBUTES VECTOR :: ctanh_ymm8c4
       type(YMM8c4),  intent(in) :: x
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
       iq = csinh_ymm8c4(x)/ccosh_ymm8c4(x)
     end function ctanh_ymm8c4
     
!DIR$ ATTRIBUTES INLINE :: cexp_2xymm8r4
     pure function cexp_2xymm8r4(re,im) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cexp_2xymm8r4
       !DIR$ ATTRIBUTES VECTOR :: cexp_2xymm8r4
       type(YMM8r4_t),  intent(in) :: re
       type(YMM8r4_t),  intent(in) :: im
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(YMM8c4) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
        do i=0,7
           iq.re(i) = exp(re.v(i))*cos(im.v(i))
           iq.im(i) = exp(re.v(i))*sin(im.v(i))
        end do
     end function cexp_2xymm8r4

!DIR$ ATTRIBUTES INLINE :: cabs_ymm8c4
     pure function  cabs_ymm8c4(c8) result(val)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  cabs_ymm8c4
       !DIR$ ATTRIBUTES VECTOR ::  cabs_ymm8c4
       type(YMM8c4), intent(in) :: c8
       !DIR$ ATTRIBUTES ALIGN : 32 :: val
       type(YMM8r4_t) :: val
       !DIR$ ATTRIBUTES ALIGN : 32 :: tre,tim
       type(YMM8r4_t) :: tre,tim
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
        do i=0,7
           tre.v(i) = c8.re(i)
           tim.v(i) = c8.im(i)
           val.v(i) = sqrt(tre.v(i)*tre.v(i)+ &
                          tim.v(i)*tim.v(i))
        end do
     end function  cabs_ymm8c4

!DIR$ ATTRIBUTES INLINE :: cabs_2xymm8r4
     pure function cabs_2xymm8r4(re,im) result(val)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cabs_2xymm8r4
       !DIR$ ATTRIBUTES VECTOR :: cabs_2xymm8r4
       type(YMM8r4_t),  intent(in) :: re
       type(YMM8r4_t),  intent(in) :: im
       !DIR$ ATTRIBUTES ALIGN : 32 :: val
       type(YMM8r4_t) :: val
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
        do i=0,7
           val.v(i) = sqrt(re.v(i)*re.v(i)+im.v(i)*im.v(i))
        end do
     end function cabs_2xymm8r4

!DIR$ ATTRIBUTES INLINE :: cpow_ymm8c4
     pure function cpow_ymm8c4(c8,n) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cpow_ymm8c4
       !DIR$ ATTRIBUTES VECTOR :: cpow_ymm8c4
       type(YMM8c4), intent(in) :: c8
       real(kind=sp),       intent(in) :: n
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: tre,tim
       type(YMM8r4_t) :: tre,tim
       !DIR$ ATTRIBUTES ALIGN : 32 :: r,theta,pow,trig
       type(YMM8r4_t) :: r,theta,pow,trig
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
        do i=0,7
           tre.v(i) = c8.re(i)
           tim.v(i) = c8.im(i)
           r.v(i) = sqrt(tre.v(i)*tre.v(i)+
                         tim.v(i)*tim.v(i))
           pow.v(i)   = r.v(i)**n
           theta.v(i) = atan(tim.v(i)/tre.v(i))
       ! 
           trig.v(i)  = theta.v(i)*n
           iq.re(i) = pow.v(i)*cos(trig.v(i))
           iq.im(i) = pow.v(i)*sin(trig.v(i))
        end do
     end function cpow_ymm8c4

!DIR$ ATTRIBUTES INLINE :: cpow_2xymm8r4
     pure function cpow_2xymm8r4(re,im,n) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cpow_2xymm8r4
       !DIR$ ATTRIBUTES VECTOR :: cpow_2xymm8r4
       type(YMM8r4_t),   intent(in) :: re
       type(YMM8r4_t),   intent(in) :: im
       real(kind=sp),    intent(in) :: n
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: tre,tim
       type(YMM8r4_t) :: tre,tim
       !DIR$ ATTRIBUTES ALIGN : 32 :: r,theta,pow,trig
       type(YMM8r4_t) :: r,theta,pow,trig
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
        do i=0,7
           r.v(i) = sqrt(re.v(i)*re.v(i)+im.v(i)*im.v(i))
           pow.v(i)   = r.v(i)**n
           theta.v(i) = atan(im.v(i)/re.v(i))
           trig.v(i)  = theta.v(i)*n
           iq.re(i) = pow.v(i)*cos(trig.v(i))
           iq.im(i) = pow.v(i)*sin(trig.v(i))
        end do
     end function cpow_2xymm8r4

!DIR$ ATTRIBUTES INLINE :: clog_ymm8c4    
     pure function clog_ymm8c4(c8) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: clog_ymm8c4    
       !DIR$ ATTRIBUTES VECTOR :: clog_ymm8c4    
       type(YMM8c4),   intent(in) :: c8
       !DIR$ ATTRIBUTES ALIGN : 32 :: iq
       type(YMM8c4) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: t0
       type(YMM8r4_t) :: t0
       
       t0 = cabs_ymm8c4(c8)
       iq.re = log(t0.v)
       iq.im = carg_ymm8c4(c8)
      end function clog_ymm8c4    
    
!DIR$ ATTRIBUTES INLINE :: clog_2xymm8r4
      pure function clog_2xymm8r4(re,im) result(iq)
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: clog_2xymm8r4
        !DIR$ ATTRIBUTES VECTOR :: clog_2xymm8r4
        type(YMM8r4_t),  intent(in), value :: re
        type(YMM8r4_t),  intent(in), value :: im
        !DIR$ ATTRIBUTES ALIGN : 32 :: iq
        type(YMM8c4) :: iq
        !DIR$ ATTRIBUTES ALIGN : 32 :: t0
        type(YMM8r4_t) :: t0
        ! EXec code ....
        t0 = cabs_2xymm8r4(re,im)
        iq.re = log(t0.v)
        iq.im = carg_2xymm8r4(re,im)
      end function clog_2xymm8r4

!DIR$ ATTRIBUTES INLINE :: csqrt_ymm8c4
      pure function csqrt_ymm8c4(c8) result(iq)
        use mod_vecconsts, only : v8_1over2
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: csqrt_ymm8c4
        !DIR$ ATTRIBUTES VECTOR :: csqrt_ymm8c4
        type(YMM8c4),   intent(in) :: c8
        !DIR$ ATTRIBUTES ALIGN : 32 :: iq
        type(YMM8c4) :: iq
        !DIR$ ATTRIBUTES ALIGN : 32 :: t0,t1,t2
        type(YMM8r4_t) :: t0
        type(YMM8r4_t) :: t1
        type(YMM8r4_t) :: t2
        ! Exec code ....
        t0 = cabs_ymm8c4(c8)
        t1.v = v8_1over2.v*(t0.v+c8.re)
        iq.re = sqrt(t1.v)
        t2.v = v8_1over2.v*(t0.v-c8.re)
        iq.im = sqrt(t2.v)
      end function csqrt_ymm8c4
     

!DIR$ ATTRIBUTES INLINE :: csqrt_2xymm8r4
      pure function csqrt_2xymm8r4(re,im) result(iq)
        use mod_vecconsts, only : v8_1over2
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: csqrt_2xymm8r4
        !DIR$ ATTRIBUTES VECTOR :: csqrt_2xymm8r4
        type(YMM8r4_t),  intent(in), value :: re
        type(YMM8r4_t),  intent(in), value :: im
        !DIR$ ATTRIBUTES ALIGN : 32 :: iq
        type(YMM8c4) :: iq
        !DIR$ ATTRIBUTES ALIGN : 32 :: t0,t1,t2
        type(YMM8r4_t) :: t0
        type(YMM8r4_t) :: t1
        type(YMM8r4_t) :: t2
        ! Exec code ....
        t0 = cabs_2xymm8r4(re,im)
        t1.v = v8_1over2*(t0.v+re.v)
        iq.re = sqrt(t1.v)
        t2.v = v8_1over2*(t0.v-re.v)
      end function csqrt_2xymm8r4



!DIR$ ATTRIBUTES INLINE :: cdiv_smith
      pure function cdiv_smith(lhs,rhs) result(iq)
!#if  (USE_INTRINSIC_VECTOR_COMPARE) == 1
!        use, intrinsic :: ISO_C_BINDING
!        use mod_avx512_bindings, only : v8f64, v8f64_cmp_pd_mask
!#endif
        use mod_vecconsts, only : v8_n0
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cdiv_smith
        !DIR$ ATTRIBUTES VECTOR :: cdiv_smith
        type(YMM8c4),  intent(in) :: lhs
        type(YMM8c4),  intent(in) :: rhs
        ! DIR$ ATTRIBUTES ALIGN : 32 :: iq
        type(YMM8c4) :: iq
        !DIR$ ATTRIBUTES ALIGN : 32 :: C00
        type(YMM8r4_t), parameter :: C00 = YMM8r4_t(0.0_sp)
        !DIR$ ATTRIBUTES ALIGN : 32 :: ratio,denom
        type(YMM8r4_t) :: ratio,denom
        !DIR$ ATTRIBUTES ALIGN : 32 :: bres
        logical(kind=i1), dimension(0:7) :: bres
        integer(kind=i4) :: i
     
        
!#if   (USE_INTRINSIC_VECTOR_COMPARE) == 1
!        !DIR$ ATTRIBUTES ALIGN : 32 :: tre,tim
!        type(v8f64) :: tre,tim
!        integer(c_char), automatic :: mask_gte
!        integer(c_char), parameter :: all_ones = Z'FF'
!#endif
        ! EXec code ....
        ratio = C00.v
        denom = C00.v
!#if   (USE_INTRINSIC_VECTOR_COMPARE) == 1
!        tre.ymm = abs(rhs.re)
!        tim.ymm = abs(rhs.im)
!        mask_gte = v8f64_cmp_pd_mask(tre,tim,29);
!#else
        bres = abs(rhs.re) >= abs(rhs.im)
!#endif
        re_part = C00.v
        im_part = C00.v
!#if   (USE_INTRINSIC_VECTOR_COMPARE) == 1
!        if(mask_gte == all_ones) then
!#elif
        if(all(bres)) then
!#endif
           !dir$ loop_count(8)
           !dir$ vector aligned
           !dir$ vector vectorlength(4)
           !dir$ vector always
           do i=0,7
              ratio.v(i)   = rhs.im(i)/rhs.re(i)
              denom.v(i)   = rhs.re(i)+(ratio.v(i)*rhs.im(i))
              iq.re(i)     = (lhs.re(i)+lhs.im(i)*ratio.v(i))/denom.v(i)
              iq.im(i)     = (lhs.im(i)-lhs.re(i)*ratio.v(i))/denom.v(i)
           end do
        else
           !dir$ loop_count(8)
           !dir$ vector aligned
           !dir$ vector vectorlength(4)
           !dir$ vector always
           do i=0,7
              ratio.v(i)   = rhs.re(i)/rhs.im(i)
              denom.v(i)   = rhs.im(i)+ratio.v(i)*rhs.re(i)
              iq.re(i)     = (lhs.re(i)*ratio.v(i)+lhs.im(i))/denom.v(i)
              iq.im(i)     = (lhs.im(i)*ratio.v(i)-lhs.re(i))/denom.v(i)
           end do
        end if
      end function cdiv_smith

      
     
end module avx_cvec8
