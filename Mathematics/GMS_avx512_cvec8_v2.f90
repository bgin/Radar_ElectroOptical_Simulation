

module avx512_cvec8_v2

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'avx512_cvec8_v2'
 !          
 !          Purpose:
 !                      This module contains a decomposed to real and imaginary
 !                      parts a complex vector of 8 elements (complex numbers)
 !                      This representation nicely fits into 2 AVX-512 ZMMx
 !                      registers.
 !                      Explicitly vectorized.
 !          History:
 !                        Date: 21-10-2023
 !                        Time: 16:57 GMT+2
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
     use mod_vectypes, only : ZMM8r8_t
     use,  intrinsic :: ISO_C_BINDING
     implicit none
     public
     !=====================================================59
     !  File and module information:
     !  version,creation and build date, author,description
     !=====================================================59

     ! Major version
     integer(kind=i4),  parameter :: AVX512_CVEC8_V2_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: AVX512_CVEC8_V2_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: AVX512_CVEC8_V2_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: AVX512_CVEC8_V2_FULLVER =   &
            1000*AVX512_CVEC8_V2_MAJOR+100*AVX512_CVEC8_V2_MINOR+10*AVX512_CVEC8_V2_MICRO
     ! Module creation date
     character(*),        parameter :: AVX512_CVEC8_V2_CREATE_DATE = "21-10-2023 16:57 +00200 (SAT 21 OCT 2023 GMT+2)"
     ! Module build date
     character(*),        parameter :: AVX512_CVEC8_V2_BUILD_DATE  = __DATE__ " " __TIME__
     ! Module author info
     character(*),        parameter :: AVX512_CVEC8_V2_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: AVX512_CVEC8_V2_SYNOPSIS    = "Packed complex vector of 8 elements (complex numbers)"

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
    

     type, public :: ZMM8c8
        !
        sequence
        real(kind=dp), dimension(0:7) :: re
        real(kind=dp), dimension(0:7) :: im
     end type ZMM8c8
       
        
        
        
      
        
    

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
      
#if (USE_INTRINSIC_VECTOR_COMPARE) == 1
      interface operator (==)
         module procedure c8_eq_c8
         module procedure c8_eq_c2
       
         module procedure c2_eq_c8
        
      end interface operator (==)

      interface operator (/=)
         module procedure c8_neq_c8
         module procedure c8_neq_c2
       
         module procedure c2_neq_c8
      
      end interface operator (/=)

      interface operator (>)
         module procedure c8_gt_c8
         module procedure c8_gt_c2
        
         module procedure c2_gt_c8
     
      end interface operator (>)

      interface operator (<)
         module procedure c8_lt_c8
         module procedure c8_lt_c2
        
         module procedure c2_lt_c8
        
      end interface operator (<)

      interface operator (>=)
         module procedure c8_ge_c8
         module procedure c8_ge_c2
       
         module procedure c2_ge_c8
        
      end interface operator (>=)

      interface operator (<=)
         module procedure c8_le_c8
         module procedure c8_le_c2
        
         module procedure c2_le_c8
        
      end interface operator (<=)
#else
        interface operator (==)
         module procedure c8_eq_c8
         module procedure c8_eq_c2
        
         module procedure c2_eq_c8
        
      end interface operator (==)

      interface operator (/=)
         module procedure c8_neq_c8
         module procedure c8_neq_c2
       
         module procedure c2_neq_c8
       
      end interface operator (/=)

      interface operator (>)
         module procedure c8_gt_c8
         module procedure c8_gt_c2
        
         module procedure c2_gt_c8
        
      end interface operator (>)

      interface operator (<)
         module procedure c8_lt_c8
         module procedure c8_lt_c2
        
         module procedure c2_lt_c8
       
      end interface operator (<)

      interface operator (>=)
         module procedure c8_ge_c8
         module procedure c8_ge_c2
        
         module procedure c2_ge_c8
        
      end interface operator (>=)

      interface operator (<=)
         module procedure c8_le_c8
         module procedure c8_le_c2
        
         module procedure c2_le_c8
        
      end interface operator (<=)
#endif

      
      contains
!DIR$ ATTRIBUTES INLINE :: default_init
     pure   function default_init() result(iq)
          !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: default_init
          !DIR$ ATTRIBUTES VECTOR :: default_init 
          !DIR$ ATTRIBUTES ALIGN : 64 :: iq
          use mod_vecconsts, only : v8_n0
          type(ZMM8c8) :: iq
          integer(kind=i4) :: i
          !dir$ loop_count(8)
          !dir$ vector aligned
          !dir$ vector vectorlength(8)
          !dir$ vector always
          do i=0,7
             iq.re(i) = v8_n0.v(i)
             iq.im(i) = v8_n0.v(i)
          end do
        end function default_init

!DIR$ ATTRIBUTES INLINE :: array_init
     pure   function array_init(re,im) result(iq)
          !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: array_init
          !DIR$ ATTRIBUTES VECTOR :: array_init
          real(kind=dp), dimension(0:7), intent(in) :: re
          real(kind=dp), dimension(0:7), intent(in) :: im
          !DIR$ ATTRIBUTES ALIGN : 64 :: iq
          type(ZMM8c8) :: iq
          integer(kind=i4) :: i
          !dir$ loop_count(8)
          !dir$ vector aligned
          !dir$ vector vectorlength(8)
          !dir$ vector always
          do i=0,7
             iq.re(i) = re(i)
             iq.im(i) = im(i)
          end do
       end function array_init

!DIR$ ATTRIBUTES INLINE :: complex1_init
     pure   function complex1_init(c) result(iq)
          !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: complex1_init
          complex(kind=dp), intent(in), value :: c
          !DIR$ ATTRIBUTES ALIGN : 64 :: iq
          type(ZMM8c8) :: iq
          real(kind=sp), automatic :: cr,ci
          integer(kind=i4) :: i
          cr = real(c,kind=dp)
          ci = aimag(c,kind=dp)
          !dir$ loop_count(8)
          !dir$ vector aligned
          !dir$ vector vectorlength(8)
          !dir$ vector always
          do i=0,7
             iq.re = cr
             iq.im = ci
          end do
     end function complex1_init

!DIR$ ATTRIBUTES INLINE :: complex2x8_init
     pure   function complex2x8_init(c) result(iq)
         !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: complex2x8_init
         complex(kind=dp), dimension(0:7), intent(in) :: c
         !DIR$ ATTRIBUTES ALIGN : 64 :: iq
         type(ZMM8c8) :: iq
         integer(kind=i4) :: i
         !dir$ loop_count(8)
         !dir$ vector aligned
         !dir$ vector vectorlength(8)
         !dir$ vector always
         do i=0,7
            iq.re(i) = real(c(i),kind=dp)
            iq.im(i) = aimag(c(i),kind=dp)
         end do
     end function complex2x8_init

!DIR$ ATTRIBUTES INLINE :: zmm8r82x_init       
     pure  function zmm8r82x_init(v1,v2) result(iq)
         !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: zmm8r82x_init
         !DIR$ ATTRIBUTES VECTOR :: zmm8r82x_init
         type(ZMM8r8_t),  intent(in), value :: v1
         type(ZMM8r8_t),  intent(in), value :: v2
         !DIR$ ATTRIBUTES ALIGN : 64 :: iq
         type(ZMM8c8) :: iq
         integer(kind=i4) :: i
          !dir$ loop_count(8)
          !dir$ vector aligned
          !dir$ vector vectorlength(8)
          !dir$ vector always
         do i=0,7
            iq.re(i) = v1.v(i)
            iq.im(i) = v2.v(i)
         end do
     end function zmm8r82x_init

!DIR$ ATTRIBUTES INLINE :: zmm8r81x_init
     pure function zmm8r81x_init(v1) result(iq)
         !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: zmm8r81x_init
         !DIR$ ATTRIBUTES VECTOR :: zmm8r81x_init
         type(ZMM8r8_t),  intent(in), value :: v1
         !DIR$ ATTRIBUTES ALIGN : 64 :: iq
         type(ZMM8c8) :: iq
         integer(kind=i4) :: i
          !dir$ loop_count(8)
          !dir$ vector aligned
          !dir$ vector vectorlength(8)
          !dir$ vector always
         do i=0,7
            iq.re(i) = v1.v(i)
            iq.im(i) = 0.0_dp
         end do
     end function zmm8r81x_init

!DIR$ ATTRIBUTES INLINE :: r81x_init
     pure function r81x_init(s) result(iq)
         !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: r81x_init
         !DIR$ ATTRIBUTES VECTOR :: r81x_init
         real(kind=dp),  intent(in) , value :: s
         !DIR$ ATTRIBUTES ALIGN : 64 :: iq
         type(ZMM8c8) :: iq
         integer(kind=i4) :: i
          !dir$ loop_count(8)
          !dir$ vector aligned
          !dir$ vector vectorlength(8)
          !dir$ vector always
          do i=0,7
             iq.re(i) = s
             iq.im(i) = 0.0_dp
          end do
     end function r81x_init

!DIR$ ATTRIBUTES INLINE ::  copy_init
     pure function copy_init(rhs) result(iq)
         !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: copy_init
         !DIR$ ATTRIBUTES VECTOR :: copy_init
         type(ZMM8c8),  intent(in) :: rhs
         !DIR$ ATTRIBUTES ALIGN : 64 :: iq
         type(ZMM8c8) :: iq
         integer(kind=i4) :: i
          !dir$ loop_count(8)
          !dir$ vector aligned
          !dir$ vector vectorlength(8)
          !dir$ vector always
          do i=0,7
             iq.re(i) = rhs.re(i)
             iq.im(i) = rhs.im(i)
          end do
     end function copy_init

!DIR$ ATTRIBUTES INLINE :: c8_add_c8
     pure elemental function c8_add_c8(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_add_c8
       !DIR$ ATTRIBUTES VECTOR :: c8_add_c8
       type(ZMM8c8),  intent(in) :: lhs
       type(ZMM8c8),  intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          iq.re(i) = lhs.re(i)+rhs.re(i)
          iq.im(i) = lhs.im(i)+rhs.im(i)
       end do
     end function c8_add_c8

!DIR$ ATTRIBUTES INLINE :: c8_add_c2
     pure elemental function c8_add_c2(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_add_c2
       !DIR$ ATTRIBUTES VECTOR :: c8_add_c2
       type(ZMM8c8),  intent(in) :: lhs
       complex(kind=dp),intent(in), value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       real(kind=sp), automatic :: cr,ci
       integer(kind=i4) :: i
       cr = real(rhs,kind=dp)
       ci = aimag(rhs,kind=dp)
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          iq.re(i) = lhs.re(i)+cr
          iq.im(i) = lhs.im(i)+ci
       end do
    end function c8_add_c2

!DIR$ ATTRIBUTES INLINE :: c8_add_v8
     pure elemental function c8_add_v8(lhs,rhs) result(iq)
       use mod_vecconsts, only : v8_n0
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_add_v8
       !DIR$ ATTRIBUTES VECTOR :: c8_add_v8
       type(ZMM8c8),   intent(in) :: lhs
       type(ZMM8r8_t), intent(in),value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
        integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          iq.re(i) = lhs.re(i)+rhs.v(i)
          iq.im(i) = v8_n0.v(i)
       end do
     end function c8_add_v8

!DIR$ ATTRIBUTES INLINE :: c8_add_s1
     pure elemental function c8_add_s1(lhs,rhs) result(iq)
       use mod_vecconsts, only : v8_n0
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_add_s1
       !DIR$ ATTRIBUTES VECTOR :: c8_add_s1
       type(ZMM8c8),   intent(in) :: lhs
       real(kind=dp),  intent(in), value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          iq.re(i) = lhs.re(i)+rhs
          iq.im(i) = v8_n0.v(i)
       end do
     end function c8_add_s1

!DIR$ ATTRIBUTES INLINE :: c2_add_c8     
     pure elemental function c2_add_c8(lhs,rhs) result(iq)
       !
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c2_add_c8
       !DIR$ ATTRIBUTES VECTOR :: c2_add_c8
       complex(kind=dp), intent(in), value :: lhs
       type(ZMM8c8),  intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       real(kind=dp), automatic :: cr,ci
       integer(kind=i4) :: i
       cr = real(lhs,kind=dp)
       ci = aimag(lhs,kind=dp)
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          iq.re(i) = cr+rhs.re(i)
          iq.im(i) = ci+rhs.im(i)
       end do
     end function c2_add_c8

!DIR$ ATTRIBUTES INLINE v8_add_c8
     pure elemental function v8_add_c8(lhs,rhs) result(iq)
       use mod_vecconsts, only : v8_n0
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: v8_add_c8
       !DIR$ ATTRIBUTES VECTOR :: v8_add_c8
       type(ZMM8r8_t),intent(in), value :: lhs
       type(ZMM8c8), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          iq.re(i) = lhs.v(i)+rhs.re(i)
          iq.im(i) = v8_n0.v(i)
       end do
     end function v8_add_c8

!DIR$ ATTRIBUTES INLINE :: s1_add_c8
     pure elemental function s1_add_c8(lhs,rhs) result(iq)
       use mod_vecconsts, only : v8_n0
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: s1_add_c8
       !DIR$ ATTRIBUTES VECTOR :: s1_add_c8
       real(kind=dp), intent(in), value :: lhs
       type(ZMM8c8), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          iq.re(i) = lhs+rhs.re(i)
          iq.im(i) = v8_n0.v(i)
       end do
     end function s1_add_c8

!DIR$ ATTRIBUTES INLINE :: c8_sub_c8
     pure elemental function c8_sub_c8(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_sub_c8
       !DIR$ ATTRIBUTES VECTOR :: c8_sub_c8
       type(ZMM8c8),    intent(in) :: lhs
       type(ZMM8c8),    intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          iq.re(i) = lhs.re(i)-rhs.re(i)
          iq.im(i) = lhs.im(i)-rhs.im(i)
       end do
     end function c8_sub_c8

!DIR$ ATTRIBUTES INLINE :: c8_sub_c2
     pure elemental function c8_sub_c2(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_sub_c2
       !DIR$ ATTRIBUTES VECTOR :: c8_sub_c2
       type(ZMM8c8),     intent(in) :: lhs
       complex(kind=dp), intent(in), value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       real(kind=dp), automatic :: cr,ci
       integer(kind=i4) :: i
       cr = real(rhs,kind=dp)
       ci = aimag(rhs,kind=dp)
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          iq.re(i) = lhs.re(i)-cr
          iq.im(i) = lhs.im(i)-ci
       end do
     end function c8_sub_c2

!DIR$ ATTRIBUTES INLINE :: c8_sub_v8
     pure elemental function c8_sub_v8(lhs,rhs) result(iq)
       use mod_vecconsts, only v8_n0
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_sub_v8
       !DIR$ ATTRIBUTES VECTOR :: c8_sub_v8
       type(ZMM8c8),    intent(in) :: lhs
       type(ZMM8r8_t),  intent(in), value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          iq.re(i) = lhs.re(i)-rhs.v(i)
          iq.im(i) = v8_n0.v(i)
       end do
     end function c8_sub_v8

!DIR$ ATTRIBUTES INLINE :: c8_sub_s1
     pure elemental function c8_sub_s1(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_sub_s1
       !DIR$ ATTRTIBUTES VECTOR :: c8_sub_s1
       type(ZMM8c8),     intent(in) :: lhs
       real(kind=dp),    intent(in), value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          iq.re(i) = lhs.re(i)-rhs
          iq.im(i) = 0.0_dp
       end do
     end function c8_sub_s1

!DIR$ ATTRIBUTES INLINE :: c2_sub_c8
     pure elemental function c2_sub_c8(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c2_sub_c8
       !DIR$ ATTRIBUTES VECTOR :: c2_sub_c8
       complex(kind=dp),      intent(in) :: lhs
       type(ZMM8c8),   intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       real(kind=dp), automatic :: cr,ci
       integer(kind=i4) :: i
       cr = real(lhs,kind=dp)
       ci = aimag(lhs,kind=dp)
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          iq.re = cr-rhs.re(i)
          iq.im = ci-rhs.im(i)
       end do
     end function c2_sub_c8

!DIR$ ATTRIBUTES INLINE :: v8_sub_c8
     pure elemental function v8_sub_c8(lhs,rhs) result(iq)
       use mod_vecconsts, only : v8_n0
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: v8_sub_c8
       !DIR$ ATTRIBUTES VECTOR :: v8_sub_c8
       type(ZMM8r8_t), intent(in), value :: lhs
       type(ZMM8c8), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          iq.re(i) = lhs.v(i)-rhs.re(i)
          iq.im(i) = v8_n0.v(i)
       end do
     end function v8_sub_c8

!DIR$ ATTRIBUTES INLINE :: s1_sub_c8
     pure elemental function s1_sub_c8(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: s1_sub_c8
       !DIR$ ATTRIBUTES VECTOR :: s1_sub_c8
       real(kind=dp),intent(in), value :: lhs
       type(ZMM8c8), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          iq.re(i) = lhs-rhs.re(i)
          iq.im(i) = 0.0_dp
       end do
     end function s1_sub_c8

!DIR$ ATTRIBUTES INLINE :: c8_mul_c8
     pure elemental function c8_mul_c8(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c8_mul_c8
       !DIR$ ATTRIBUTES VECTOR :: c8_mul_c8
       type(ZMM8c8),    intent(in) :: lhs
       type(ZMM8c8),    intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0,zmm1,zmm2,zmm3
       type(ZMM8r8_t) :: zmm0,zmm41,zmm2,zmm3
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          zmm0.v(i) = lhs.re(i)*rhs.re(i)
          zmm1.v(i) = lhs.im(i)*rhs.im(i)
          iq.re(i)  = zmm0.v(i)-zmm1.v(i)
          zmm2.v(i) = lhs.im(i)*rhs.re(i)
          zmm3.v(i) = lhs.re(i)*rhs.im(i)
          iq.im(i)  = zmm2.v(i)-zmm3.v(i)
       end do
     end function c8_mul_c8

!DIR$ ATTRIBUTES INLINE :: c8_mul_c2
     pure elemental function c8_mul_c2(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c8_mul_c2
       !DIR$ ATTRIBUTES VECTOR :: c8_mul_c2
       type(ZMM8c8),     intent(in) :: lhs
       complex(kind=dp), intent(in), value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0,zmm1,zmm2,zmm3
       type(ZMM8r8_t) :: zmm0,zmm1,zmm2,zmm3
       real(kind=dp), automatic :: cr,ci
       integer(kind=i4) :: i
       cr = real(rhs,kind=dp)
       ci = aimag(rhs,kind=dp)
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          zmm0.v(i) = lhs.re(i)*cr
          zmm1.v(i) = lhs.im(i)*ci
          iq.re(i)  = zmm0.v(i)-zmm1.v(i)
          zmm2.v(i) = lhs.im(i)*cr
          zmm3.v(i) = lhs.re(i)*ci
          iq.im(i)  = zmm2.v(i)-zmm3.v(i)
       end do
     end function c8_mul_c2

!DIR$ ATTRIBUTES INLINE :: c8_mul_v8
     pure elemental function c8_mul_v8(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_mul_v8
       !DIR$ ATTRIBUTES VECTOR :: c8_mul_v8
       type(ZMM8c8),    intent(in) :: lhs
       type(ZMM8r8_t),  intent(in), value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          iq.re(i) = lhs.re(i)*rhs.v(i)
          iq.im(i) = lhs.im(i)*rhs.v(i)
       end do
     end function c8_mul_v8

!DIR$ ATTRIBUTES INLINE :: c8_mul_s1
     pure elemental function c8_mul_s1(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_mul_s1
       !DIR$ ATTRIBUTES VECTOR :: c8_mul_s1
       type(ZMM8c8),    intent(in) :: lhs
       real(kind=dp),   intent(in), value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          iq.re(i) = lhs.re(i)*rhs
          iq.im(i) = lhs.im(i)*rhs
       end do
     end function c8_mul_s1

!DIR$ ATTRIBUTES INLINE :: c2_mul_c8
     pure elemental function c2_mul_c8(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c2_mul_c8
       !DIR$ ATTRIBUTES VECTOR :: c2_mul_c8
       complex(kind=dp),intent(in), value :: lhs
       type(ZMM8c8),     intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0,zmm1,zmm2,zmm3
       type(ZMM8r8_t) :: zmm0,zmm1,zmm2,zmm3
       real(kind=sp), automatic :: cr,ci
       integer(kind=i4) :: i
       cr = real(lhs,kind=dp)
       ci = aimag(lhs,kind=dp)
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          zmm0.v(i) = cr*rhs.re(i)
          zmm1.v(i) = ci*rhs.im(i)
          iq.re(i)  = zmm0.v(i)+zmm1.v(i)
          zmm2.v(i) = cr*rhs.im(i)
          zmm3.v(i) = ci*rhs.re(i)
          iq.im(i)  = zmm2.v(i)-zmm3.v(i)
        end do
     end function c2_mul_c8

!DIR$ ATTRIBUTES INLINE :: v8_mul_c8
     pure elemental function v8_mul_c8(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: v8_mul_c8
       !DIR$ ATTRIBUTES VECTOR :: v8_mul_c8
       type(ZMM8r8_t), intent(in), value :: lhs
       type(ZMM8c8),   intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: v8_mul_c8
       type(ZMM8c8) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          iq.re(i) = lhs.v(i)*rhs.re(i)
          iq.im(i) = lhs.v(i)*rhs.im(i)
       end do
     end function v8_mul_c8

!DIR$ ATTRIBUTES INLINE :: s1_mul_c8
     pure elemental function s1_mul_c8(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: s1_mul_c8
       !DIR$ ATTRIBUTES VECTOR :: s1_mul_c8
       real(kind=dp),intent(in), value :: lhs
       type(ZMM8c8), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          iq.re(i) = lhs*rhs.re(i)
          iq.im(i) = lhs*rhs.im(i)
       end do
     end function s1_mul_c8

!DIR$ ATTRIBUTES INLINE :: c8_div_c8     
     pure elemental function c8_div_c8(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c8_div_c8
       !DIR$ ATTRIBUTES VECTOR :: c8_div_c8
       type(ZMM8c8),    intent(in) :: lhs
       type(ZMM8c8),    intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0,zmm1,zmm2,zmm3,den
       type(ZMM8r8_t), automatic :: zmm0,zmm1,zmm2,zmm3,den
       integer(kind=i4) :: i
#if (USE_SAFE_COMPLEX_DIVISION) == 1
       iq = cdiv_smith(lhs,rhs)
#else
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          zmm0.v(i) = lhs.re(i)*rhs.re(i)
          zmm1.v(i) = lhs.im(i)*rhs.im(i)
          zmm2.v(i) = lhs.im(i)*rhs.re(i)
          zmm3.v(i) = lhs.re(i)*rhs.im(i)
          den.v(i)  = (rhs.re(i)*rhs.re(i))+ &
                      (rhs.im(i)*rhs.im(i))
          iq.re(i)  = (zmm0.v(i)+zmm1.v(i))/den.v(i)
          iq.im(i)  = (zmm2.v(i)-zmm3.v(i))/den.v(i)
       end do
#endif
     end function c8_div_c8

!DIR$ ATTRIBUTES INLINE :: c8_div_c2
     pure elemental function c8_div_c2(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c8_div_c2
       !DIR$ ATTRIBUTES VECTOR :: c8_div_c2
       type(ZMM8c8),   intent(in) :: lhs
       complex(kind=dp),intent(in), value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0,zmm1,zmm2,zmm3,den
       type(ZMM8c8), automatic :: zmm0,zmm1,zmm2,zmm3,den
       real(kind=dp), automatic :: cr,ci,t0
       integer(kind=i4) :: i
       cr = real(rhs,kind=dp)
       ci = aimag(rhs,kind=dp)
       t0 = (cr*cr)+(ci*ci)
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          zmm0.v(i) = lhs.re(i)*cr
          zmm1.v(i) = lhs.im(i)*ci
          zmm2.v(i) = lhs.im(i)*cr
          zmm3.v(i) = lhs.re(i)*ci
          den.v(i)  = t0
          iq.re(i) = (zmm0.v(i)+zmm1.v(i))/den.v(i)
          iq.im(i) = (zmm2.v(i)-zmm3.v(i))/den.v(i)
       end do
     end function c8_div_c2

!DIR$ ATTRIBUTES INLINE :: c8_div_v8
     pure elemental function c8_div_v8(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_div_v8
       !DIR$ ATTRIBUTES VECTOR :: c8_div_v8
       type(ZMM8c8),  intent(in) :: lhs
       type(ZMM8r8_t),intent(in), value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          iq.re(i) = lhs.re(i)/rhs.v(i)
          iq.im(i) = lhs.im(i)/rhs.v(i)
       end do
     end function c8_div_v8

!DIR$ ATTRIBUTES INLINE :: c8_div_s1
     pure elemental function c8_div_s1(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_div_s1
       !DIR$ ATTRIBUTES VECTOR :: c8_div_s1
       type(ZMM8c8),     intent(in) :: lhs
       real(kind=dp),    intent(in), value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          iq.re(i) = lhs.re(i)/rhs
          iq.im(i) = lhs.im(i)/rhs
       end do
     end function c8_div_s1

!DIR$ ATTRIBUTES INLINE :: c2_div_c8
     pure elemental function c2_div_c8(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c2_div_c8
       !DIR$ ATTRIBUTES VECTOR :: c2_div_c8
       complex(kind=dp),intent(in), value :: lhs
       type(ZMM8c8),    intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0,zmm1,zmm2,zmm3,den
       type(ZMM8c8), automatic :: zmm0,zmm1,zmm2,zmm3,den
       real(kind=dp), automatic :: cr,ci
       integer(kind=i4) :: i
       cr = real(lhs,kind=dp)
       ci = aimag(lhs,kind=dp)
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          zmm0.v(i) = cr*rhs.re(i)
          zmm1.v(i) = ci*rhs.im(i)
          zmm2.v(i) = ci*rhs.re(i)
          zmm3.v(i) = cr*rhs.im(i)
          den.v(i)  = (rhs.re(i)*rhs.re(i))+ &
                      (rhs.im(i)*rhs.im(i))
          iq.re(i)  = (zmm0.v(i)+zmm1.v(i))/den.v(i)
          iq.im(i)  = (zmm2.v(i)-zmm3.v(i))/den.v(i)
       end do
     end function c2_div_c8

!DIR$ ATTRIBUTES INLINE :: v8_div_c8
     pure elemental function v8_div_c8(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: v8_div_c8
       !DIR$ ATTRIBUTES VECTOR :: v8_div_c8
       type(ZMM8r8_t),intent(in), value :: lhs
       type(ZMM8c8), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: C00
       type(ZMM8r8_t), parameter :: C00 = ZMM8r8_t(0.0_dp)
       type(ZMM8c8), automatic :: t0
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          t0.re(i) = lhs.v(i)
          t0.im(i) = C00.v(i)
       end do 
       iq = t0/rhs
     end function v8_div_c8

!DIR$ ATTRIBUTES INLINE :: s1_div_c8
     pure elemental function s1_div_c8(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: s1_div_c8
       !DIR$ ATTRIBUTES VECTOR :: s1_div_c8
       real(kind=dp),intent(in), value :: lhs
       type(ZMM8c8), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       !DIR$ ATTRIBUTES ALIGN : 32 :: C00
       type(ZMM8r8_t), parameter :: C00 = ZMM8r8_t(0.0_dp)
       type(ZMM8c8), automatic :: t0
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          t0.re(i) = lhs
          t0.im(i) = C00.v(i)
       end do
       iq = t0/rhs
     end function s1_div_c8

!DIR$ ATTRIBUTES INLINE :: conjugate
     pure elemental function conjugate(x) result(iq)
       use mod_vecconsts, only : v8_n0
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: conjugate
       !DIR$ ATTRIBUTES VECTOR :: conjugate
       type(ZMM8c8),  intent(in) :: x
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          iq.re(i) = v8_n0.v(i)-x.re(i)
          iq.im(i) = x.im(i)
       end do
     end function conjugate
     
#if (USE_INTRINSIC_VECTOR_COMPARE) == 0
     
!DIR$ ATTRIBUTES INLINE :: c8_eq_c8     
     pure elemental function c8_eq_c8(lhs,rhs) result(mmask8)
       use mod_avx512_bindings, only : v8f64, v8f64_cmp_pd_mask
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_eq_c8
       !DIR$ ATTRIBUTES VECTOR :: c8_eq_c8
       type(ZMM8c8),   intent(in) :: lhs
       type(ZMM8c8),   intent(in) :: rhs
       integer(c_char), dimension(0:1) :: mmask8
       !DIR$ ATTRIBUTES ALIGN : 64 :: lre,lim,rre,rim
       type(v8f64), automatic :: lre,lim,rre,rim
       mmask8 = 0
       lre.zmm = lhs.re
       rre.zmm = rhs.re
       mmask8(0) = v8f64_cmp_pd_mask(lre,rre,0)
       lim.zmm = lhs.im
       rim.zmm = rhs.im
       mmask8(1) = v8f64_cmp_pd_mask(lim,rim,0)
     end function c8_eq_c8

!DIR$ ATTRIBUTES INLINE :: c8_eq_c2
     pure elemental function c8_eq_c2(lhs,rhs) result(mmask8)
       use mod_avx512_bindings, only : v8f64, v8f64_cmp_pd_mask
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_eq_c2
       !DIR$ ATTRIBUTES VECTOR :: c8_eq_c2
       type(ZMM8c8),  intent(in) :: lhs
       complex(kind=dp),     intent(in) :: rhs
       integer(c_char), dimension(0:1) :: mmask8
       !DIR$ ATTRIBUTES ALIGN : 64 :: lre,lim,rre,rim
       type(v8f64), automatic :: lre,lim,rre,rim
       mmask8 = 0
       lre.zmm = lhs.re
       rre.zmm = real(rhs,kind=dp)
       mmask8(0) = v8f64_cmp_pd_mask(lre,rre,0)
       lim.zmm = lhs.im
       rim.zmm = aimag(rhs,kind=dp)
       mmask8(1) = v8f64_cmp_pd_mask(lim,rim,0)
     end function c8_eq_c2

!DIR$ ATTRIBUTES INLINE :: c2_eq_c8
     pure elemental function c2_eq_c8(lhs,rhs) result(mmask8)
       use mod_avx512_bindings, only : v8f64, v8f64_cmp_pd_mask
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c2_eq_c8
       !DIR$ ATTRIBUTES VECTOR :: c2_eq_c8
       complex(kind=dp),     intent(in) :: lhs
       type(ZMM8c8),  intent(in) :: rhs
       integer(c_char), dimension(0:1) :: mmask8
       !DIR$ ATTRIBUTES ALIGN : 64 :: lre,lim,rre,rim
       type(v8f64), automatic :: lre,lim,rre,rim
       mmask8 = 0
       lre.zmm = real(lhs,kind=dp)
       rre.zmm = rhs.re
       mmask8(0) = v8f64_cmp_pd_mask(lre,rre,0)
       lim.zmm = aimag(lhs,kind=dp)
       rim.zmm = rhs.im
       mmask8(1) = v8f64_cmp_pd_mask(lim,rim,0)
     end function c2_eq_c8

!DIR$ ATTRIBUTES INLINE :: c8_neq_c8     
     pure elemental function c8_neq_c8(lhs,rhs) result(mmask8)
       use mod_avx512_bindings, only : v8f64, v8f64_cmp_pd_mask
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_neq_c8
       !DIR$ ATTRIBUTES VECTOR :: c8_neq_c8
       type(ZMM8c8),   intent(in) :: lhs
       type(ZMM8c8),   intent(in) :: rhs
       integer(c_char), dimension(0:1) :: mmask8
       !DIR$ ATTRIBUTES ALIGN : 64 :: lre,lim,rre,rim
       type(v8f64), automatic :: lre,lim,rre,rim
       mmask8 = 0
       lre.zmm = lhs.re
       rre.zmm = rhs.re
       mmask8(0) = v8f64_cmp_pd_mask(lre,rre,12)
       lim.zmm = lhs.im
       rim.zmm = rhs.im
       mmask8(1) = v8f64_cmp_pd_mask(lim,rim,12)
     end function c8_neq_c8

!DIR$ ATTRIBUTES INLINE :: c8_neq_c2
     pure elemental function c8_neq_c2(lhs,rhs) result(mmask8)
       use mod_avx512_bindings, only : v8f64, v8f64_cmp_pd_mask
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_neq_c2
       !DIR$ ATTRIBUTES VECTOR :: c8_neq_c2
       type(ZMM8c8),  intent(in) :: lhs
       complex(kind=dp),     intent(in) :: rhs
       integer(c_char), dimension(0:1) :: mmask8
       !DIR$ ATTRIBUTES ALIGN : 64 :: lre,lim,rre,rim
       type(v8f64), automatic :: lre,lim,rre,rim
       mmask8 = 0
       lre.zmm = lhs.re
       rre.zmm = real(rhs,kind=dp)
       mmask8(0) = v8f64_cmp_pd_mask(lre,rre,12)
       lim.zmm = lhs.im
       rim.zmm = aimag(rhs,kind=dp)
       mmask8(1) = v8f64_cmp_pd_mask(lim,rim,12)
     end function c8_neq_c2

!DIR$ ATTRIBUTES INLINE :: c2_neq_c8
     pure elemental function c2_neq_c8(lhs,rhs) result(mmask8)
       use mod_avx512_bindings, only : v8f64, v8f64_cmp_pd_mask
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c2_neq_c8
       !DIR$ ATTRIBUTES VECTOR :: c2_neq_c8
       complex(kind=dp),     intent(in) :: lhs
       type(ZMM8c8),  intent(in) :: rhs
       integer(c_char), dimension(0:1) :: mmask8
       !DIR$ ATTRIBUTES ALIGN : 64 :: lre,lim,rre,rim
       type(v8f64), automatic :: lre,lim,rre,rim
       mmask8 = 0
       lre.zmm = real(lhs,kind=dp)
       rre.zmm = rhs.re
       mmask8(0) = v8f64_cmp_pd_mask(lre,rre,12)
       lim.zmm = aimag(lhs,kind=dp)
       rim.zmm = rhs.im
       mmask8(1) = v8f64_cmp_pd_mask(lim,rim,12)
     end function c2_neq_c8
     
!DIR$ ATTRIBUTES INLINE :: c8_gt_c8     
     pure elemental function c8_gt_c8(lhs,rhs) result(mmask8)
       use mod_avx512_bindings, only : v8f64, v8f64_cmp_pd_mask
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_gt_c8
       !DIR$ ATTRIBUTES VECTOR :: c8_gt_c8
       type(ZMM8c8),   intent(in) :: lhs
       type(ZMM8c8),   intent(in) :: rhs
       integer(c_char), dimension(0:1) :: mmask8
       !DIR$ ATTRIBUTES ALIGN : 64 :: lre,lim,rre,rim
       type(v8f64), automatic :: lre,lim,rre,rim
       mmask8 = 0
       lre.zmm = lhs.re
       rre.zmm = rhs.re
       mmask8(0) = v8f64_cmp_pd_mask(lre,rre,30)
       lim.zmm = lhs.im
       rim.zmm = rhs.im
       mmask8(1) = v8f64_cmp_pd_mask(lim,rim,30)
     end function c8_gt_c8

!DIR$ ATTRIBUTES INLINE :: c8_gt_c2
     pure elemental function c8_gt_c2(lhs,rhs) result(mmask8)
       use mod_avx512_bindings, only : v8f64, v8f64_cmp_pd_mask
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_gt_c2
       !DIR$ ATTRIBUTES VECTOR :: c8_gt_c2
       type(ZMM8c8),  intent(in) :: lhs
       complex(kind=dp),     intent(in) :: rhs
       integer(c_char), dimension(0:1) :: mmask8
       !DIR$ ATTRIBUTES ALIGN : 64 :: lre,lim,rre,rim
       type(v8f64), automatic :: lre,lim,rre,rim
       mmask8 = 0
       lre.zmm = lhs.re
       rre.zmm = real(rhs,kind=dp)
       mmask8(0) = v8f64_cmp_pd_mask(lre,rre,30)
       lim.zmm = lhs.im
       rim.zmm = aimag(rhs,kind=dp)
       mmask8(1) = v8f64_cmp_pd_mask(lim,rim,30)
     end function c8_gt_c2

!DIR$ ATTRIBUTES INLINE :: c2_gt_c8
     pure elemental function c2_eq_c8(lhs,rhs) result(mmask8)
       use mod_avx512_bindings, only : v8f64, v8f64_cmp_pd_mask
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c2_gt_c8
       !DIR$ ATTRIBUTES VECTOR :: c2_gt_c8
       complex(kind=dp),     intent(in) :: lhs
       type(ZMM8c8),  intent(in) :: rhs
       integer(c_char), dimension(0:1) :: mmask8
       !DIR$ ATTRIBUTES ALIGN : 64 :: lre,lim,rre,rim
       type(v8f64), automatic :: lre,lim,rre,rim
       mmask8 = 0
       lre.zmm = real(lhs,kind=dp)
       rre.zmm = rhs.re
       mmask8(0) = v8f64_cmp_pd_mask(lre,rre,30)
       lim.zmm = aimag(lhs,kind=dp)
       rim.zmm = rhs.im
       mmask8(1) = v8f64_cmp_pd_mask(lim,rim,30)
     end function c2_gt_c8
     
!DIR$ ATTRIBUTES INLINE :: c8_lt_c8     
     pure elemental function c8_lt_c8(lhs,rhs) result(mmask8)
       use mod_avx512_bindings, only : v8f64, v8f64_cmp_pd_mask
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_lt_c8
       !DIR$ ATTRIBUTES VECTOR :: c8_eq_c8
       type(ZMM8c8),   intent(in) :: lhs
       type(ZMM8c8),   intent(in) :: rhs
       integer(c_char), dimension(0:1) :: mmask8
       !DIR$ ATTRIBUTES ALIGN : 64 :: lre,lim,rre,rim
       type(v8f64), automatic :: lre,lim,rre,rim
       mmask8 = 0
       lre.zmm = lhs.re
       rre.zmm = rhs.re
       mmask8(0) = v8f64_cmp_pd_mask(lre,rre,17)
       lim.zmm = lhs.im
       rim.zmm = rhs.im
       mmask8(1) = v8f64_cmp_pd_mask(lim,rim,17)
     end function c8_lt_c8

!DIR$ ATTRIBUTES INLINE :: c8_lt_c2
     pure elemental function c8_lt_c2(lhs,rhs) result(mmask8)
       use mod_avx512_bindings, only : v8f64, v8f64_cmp_pd_mask
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_lt_c2
       !DIR$ ATTRIBUTES VECTOR :: c8_lt_c2
       type(ZMM8c8),  intent(in) :: lhs
       complex(kind=dp),     intent(in) :: rhs
       integer(c_char), dimension(0:1) :: mmask8
       !DIR$ ATTRIBUTES ALIGN : 64 :: lre,lim,rre,rim
       type(v8f64), automatic :: lre,lim,rre,rim
       mmask8 = 0
       lre.zmm = lhs.re
       rre.zmm = real(rhs,kind=dp)
       mmask8(0) = v8f64_cmp_pd_mask(lre,rre,17)
       lim.zmm = lhs.im
       rim.zmm = aimag(rhs,kind=dp)
       mmask8(1) = v8f64_cmp_pd_mask(lim,rim,17)
     end function c8_lt_c2

!DIR$ ATTRIBUTES INLINE :: c2_lt_c8
     pure elemental function c2_lt_c8(lhs,rhs) result(mmask8)
       use mod_avx512_bindings, only : v8f64, v8f64_cmp_pd_mask
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c2_lt_c8
       !DIR$ ATTRIBUTES VECTOR :: c2_lt_c8
       complex(kind=dp),     intent(in) :: lhs
       type(ZMM8c8),  intent(in) :: rhs
       integer(c_char), dimension(0:1) :: mmask8
       !DIR$ ATTRIBUTES ALIGN : 64 :: lre,lim,rre,rim
       type(v8f64), automatic :: lre,lim,rre,rim
       mmask8 = 0
       lre.zmm = real(lhs,kind=dp)
       rre.zmm = rhs.re
       mmask8(0) = v8f64_cmp_pd_mask(lre,rre,0)
       lim.zmm = aimag(lhs,kind=dp)
       rim.zmm = rhs.im
       mmask8(1) = v8f64_cmp_pd_mask(lim,rim,0)
     end function c2_lt_c8

!DIR$ ATTRIBUTES INLINE :: c8_ge_c8     
     pure elemental function c8_ge_c8(lhs,rhs) result(mmask8)
       use mod_avx512_bindings, only : v8f64, v8f64_cmp_pd_mask
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_ge_c8
       !DIR$ ATTRIBUTES VECTOR :: c8_ge_c8
       type(ZMM8c8),   intent(in) :: lhs
       type(ZMM8c8),   intent(in) :: rhs
       integer(c_char), dimension(0:1) :: mmask8
       !DIR$ ATTRIBUTES ALIGN : 64 :: lre,lim,rre,rim
       type(v8f64), automatic :: lre,lim,rre,rim
       mmask8 = 0
       lre.zmm = lhs.re
       rre.zmm = rhs.re
       mmask8(0) = v8f64_cmp_pd_mask(lre,rre,29)
       lim.zmm = lhs.im
       rim.zmm = rhs.im
       mmask8(1) = v8f64_cmp_pd_mask(lim,rim,29)
     end function c8_ge_c8

!DIR$ ATTRIBUTES INLINE :: c8_ge_c2
     pure elemental function c8_ge_c2(lhs,rhs) result(mmask8)
       use mod_avx512_bindings, only : v8f64, v8f64_cmp_pd_mask
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_ge_c2
       !DIR$ ATTRIBUTES VECTOR :: c8_ge_c2
       type(ZMM8c8),  intent(in) :: lhs
       complex(kind=dp),     intent(in) :: rhs
       integer(c_char), dimension(0:1) :: mmask8
       !DIR$ ATTRIBUTES ALIGN : 64 :: lre,lim,rre,rim
       type(v8f64), automatic :: lre,lim,rre,rim
       mmask8 = 0
       lre.zmm = lhs.re
       rre.zmm = real(rhs,kind=dp)
       mmask8(0) = v8f64_cmp_pd_mask(lre,rre,29)
       lim.zmm = lhs.im
       rim.zmm = aimag(rhs,kind=dp)
       mmask8(1) = v8f64_cmp_pd_mask(lim,rim,29)
     end function c8_ge_c2

!DIR$ ATTRIBUTES INLINE :: c2_ge_c8
     pure elemental function c2_ge_c8(lhs,rhs) result(mmask8)
       use mod_avx512_bindings, only : v8f64, v8f64_cmp_pd_mask
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c2_ge_c8
       !DIR$ ATTRIBUTES VECTOR :: c2_ge_c8
       complex(kind=dp),     intent(in) :: lhs
       type(ZMM8c8),  intent(in) :: rhs
       integer(c_char), dimension(0:1) :: mmask8
       !DIR$ ATTRIBUTES ALIGN : 64 :: lre,lim,rre,rim
       type(v8f64), automatic :: lre,lim,rre,rim
       mmask8 = 0
       lre.zmm = real(lhs,kind=dp)
       rre.zmm = rhs.re
       mmask8(0) = v8f64_cmp_pd_mask(lre,rre,29)
       lim.zmm = aimag(lhs,kind=dp)
       rim.zmm = rhs.im
       mmask8(1) = v8f64_cmp_pd_mask(lim,rim,29)
     end function c2_ge_c8

!DIR$ ATTRIBUTES INLINE :: c8_le_c8     
     pure elemental function c8_le_c8(lhs,rhs) result(mmask8)
       use mod_avx512_bindings, only : v8f64, v8f64_cmp_pd_mask
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_le_c8
       !DIR$ ATTRIBUTES VECTOR :: c8_le_c8
       type(ZMM8c8),   intent(in) :: lhs
       type(ZMM8c8),   intent(in) :: rhs
       integer(c_char), dimension(0:1) :: mmask8
       !DIR$ ATTRIBUTES ALIGN : 64 :: lre,lim,rre,rim
       type(v8f64), automatic :: lre,lim,rre,rim
       mmask8 = 0
       lre.zmm = lhs.re
       rre.zmm = rhs.re
       mmask8(0) = v8f64_cmp_pd_mask(lre,rre,18)
       lim.zmm = lhs.im
       rim.zmm = rhs.im
       mmask8(1) = v8f64_cmp_pd_mask(lim,rim,18)
     end function c8_le_c8

!DIR$ ATTRIBUTES INLINE :: c8_le_c2
     pure elemental function c8_le_c2(lhs,rhs) result(mmask8)
       use mod_avx512_bindings, only : v8f64, v8f64_cmp_pd_mask
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_le_c2
       !DIR$ ATTRIBUTES VECTOR :: c8_le_c2
       type(ZMM8c8),  intent(in) :: lhs
       complex(kind=dp),     intent(in) :: rhs
       integer(c_char), dimension(0:1) :: mmask8
       !DIR$ ATTRIBUTES ALIGN : 64 :: lre,lim,rre,rim
       type(v8f64), automatic :: lre,lim,rre,rim
       mmask8 = 0
       lre.zmm = lhs.re
       rre.zmm = real(rhs,kind=dp)
       mmask8(0) = v8f64_cmp_pd_mask(lre,rre,18)
       lim.zmm = lhs.im
       rim.zmm = aimag(rhs,kind=dp)
       mmask8(1) = v8f64_cmp_pd_mask(lim,rim,18)
     end function c8_le_c2

!DIR$ ATTRIBUTES INLINE :: c2_le_c8
     pure elemental function c2_le_c8(lhs,rhs) result(mmask8)
       use mod_avx512_bindings, only : v8f64, v8f64_cmp_pd_mask
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c2_le_c8
       !DIR$ ATTRIBUTES VECTOR :: c2_le_c8
       complex(kind=dp),     intent(in) :: lhs
       type(ZMM8c8),  intent(in) :: rhs
       integer(c_char), dimension(0:1) :: mmask8
       !DIR$ ATTRIBUTES ALIGN : 64 :: lre,lim,rre,rim
       type(v8f64), automatic :: lre,lim,rre,rim
       mmask8 = 0
       lre.zmm = real(lhs,kind=dp)
       rre.zmm = rhs.re
       mmask8(0) = v8f64_cmp_pd_mask(lre,rre,0)
       lim.zmm = aimag(lhs,kind=dp)
       rim.zmm = rhs.im
       mmask8(1) = v8f64_cmp_pd_mask(lim,rim,0)
     end function c2_le_c8     
#else
!DIR$ ATTRIBUTES INLINE :: c8_eq_c8
     pure elemental function c8_eq_c8(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_eq_c8
       !DIR$ ATTRIBUTES VECTOR :: c8_eq_c8
       type(ZMM8c8),       intent(in) :: lhs
       type(ZMM8c8),       intent(in) :: rhs
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
     end function c8_eq_c8

!DIR$ ATTRIBUTES INLINE :: c8_eq_c2
     pure elemental function c8_eq_c2(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_eq_c2
       !DIR$ ATTRIBUTES VECTOR :: c8_eq_c2
       type(ZMM8c8),    intent(in) :: lhs
       complex(kind=dp),intent(in), value :: rhs
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
     end function c8_eq_c2

!DIR$ ATTRIBUTES INLINE :: c2_eq_c8
     pure elemental function c2_eq_c8(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c2_eq_c8
       !DIR$ ATTRIBUTES VECTOR :: c2_eq_c8
       complex(kind=dp), intent(in), value :: lhs
       type(ZMM8c8),     intent(in) :: rhs
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
     end function c2_eq_c8

!DIR$ ATTRIBUTES INLINE :: c8_neq_c8
     pure elemental function c8_neq_c8(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_neq_c8
       !DIR$ ATTRIBUTES VECTOR :: c8_neq_c8
       type(ZMM8c8),       intent(in) :: lhs
       type(ZMM8c8),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:7) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re /= rhs.re
       bres(0) = all(mre)
       mim = lhs.im /= rhs.im
       bres(1) = all(mim)
     end function c8_neq_c8

!DIR$ ATTRIBUTES INLINE :: c8_neq_c2
     pure elemental function c8_neq_c2(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_neq_c2
       !DIR$ ATTRIBUTES VECTOR :: c8_neq_c2
       type(ZMM8c8),    intent(in) :: lhs
       complex(kind=dp),intent(in), value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:7) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re /= real(rhs,kind=dp)
       bres(0) = all(mre)
       mim = lhs.im /= aimag(rhs,kind=dp)
       bres(1) = all(mim)
     end function c8_neq_c2

!DIR$ ATTRIBUTES INLINE :: c2_neq_c8
     pure elemental function c2_neq_c8(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c2_neq_c8
       !DIR$ ATTRIBUTES VECTOR :: c2_neq_c8
       complex(kind=dp), intent(in), value :: lhs
       type(ZMM8c8),     intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:7) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = real(lhs,kind=dp)  /= rhs.re
       bres(0) = all(mre)
       mim = aimag(lhs,kind=dp) /= rhs.im
       bres(1) = all(mim)
     end function c2_neq_c8

!DIR$ ATTRIBUTES INLINE :: c8_gt_c8
     pure elemental function c8_gt_c8(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_gt_c8
       !DIR$ ATTRIBUTES VECTOR :: c8_gt_c8
       type(ZMM8c8),       intent(in) :: lhs
       type(ZMM8c8),       intent(in) :: rhs
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
     end function c8_gt_c8

!DIR$ ATTRIBUTES INLINE :: c8_gt_c2
     pure elemental function c8_gt_c2(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_gt_c2
       !DIR$ ATTRIBUTES VECTOR :: c8_gt_c2
       type(ZMM8c8),    intent(in) :: lhs
       complex(kind=dp),intent(in), value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:7) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re > real(rhs,kind=dp)
       bres(0) = all(mre)
       mim = lhs.im > aimag(rhs,kind=dp)
       bres(1) = all(mim)
     end function c8_gt_c2

!DIR$ ATTRIBUTES INLINE :: c2_gt_c8
     pure elemental function c2_gt_c8(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c2_gt_c8
       !DIR$ ATTRIBUTES VECTOR :: c2_gt_c8
       complex(kind=dp), intent(in), value :: lhs
       type(ZMM8c8),     intent(in) :: rhs
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
     end function c2_gt_c8
     
!DIR$ ATTRIBUTES INLINE :: c8_lt_c8
     pure elemental function c8_lt_c8(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_lt_c8
       !DIR$ ATTRIBUTES VECTOR :: c8_lt_c8
       type(ZMM8c8),       intent(in) :: lhs
       type(ZMM8c8),       intent(in) :: rhs
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
     end function c8_lt_c8

!DIR$ ATTRIBUTES INLINE :: c8_lt_c2
     pure elemental function c8_lt_c2(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_lt_c2
       !DIR$ ATTRIBUTES VECTOR :: c8_lt_c2
       type(ZMM8c8),    intent(in) :: lhs
       complex(kind=dp),intent(in), value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:7) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re < real(rhs,kind=dp)
       bres(0) = all(mre)
       mim = lhs.im < aimag(rhs,kind=dp)
       bres(1) = all(mim)
     end function c8_lt_c2

!DIR$ ATTRIBUTES INLINE :: c2_lt_c8
     pure elemental function c2_lt_c8(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c2_lt_c8
       !DIR$ ATTRIBUTES VECTOR :: c2_lt_c8
       complex(kind=dp), intent(in), value :: lhs
       type(ZMM8c8),     intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:7) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = real(lhs,kind=dp) < rhs.re
       bres(0) = all(mre)
       mim = aimag(lhs,kind=dp) < rhs.im
       bres(1) = all(mim)
     end function c2_lt_c8

!DIR$ ATTRIBUTES INLINE :: c8_ge_c8
     pure elemental function c8_ge_c8(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_ge_c8
       !DIR$ ATTRIBUTES VECTOR :: c8_ge_c8
       type(ZMM8c8),       intent(in) :: lhs
       type(ZMM8c8),       intent(in) :: rhs
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
     end function c8_ge_c8

!DIR$ ATTRIBUTES INLINE :: c8_ge_c2
     pure elemental function c8_ge_c2(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_ge_c2
       !DIR$ ATTRIBUTES VECTOR :: c8_ge_c2
       type(ZMM8c8),    intent(in) :: lhs
       complex(kind=dp),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:7) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = lhs.re >= real(rhs,kind=dp)
       bres(0) = all(mre)
       mim = lhs.im >= aimag(rhs,kind=dp)
       bres(1) = all(mim)
     end function c8_ge_c2

!DIR$ ATTRIBUTES INLINE :: c2_ge_c8
     pure elemental function c2_ge_c8(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c2_ge_c8
       !DIR$ ATTRIBUTES VECTOR :: c2_ge_c8
       complex(kind=dp), intent(in), value :: lhs
       type(ZMM8c8),     intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:7) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = real(lhs,kind=dp) >= rhs.re
       bres(0) = all(mre)
       mim = aimag(lhs,kind=dp) >= rhs.im
       bres(1) = all(mim)
     end function c2_ge_c8
     
!DIR$ ATTRIBUTES INLINE :: c8_le_c8
     pure elemental function c8_le_c8(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_le_c8
       !DIR$ ATTRIBUTES VECTOR :: c8_le_c8
       type(ZMM8c8),       intent(in) :: lhs
       type(ZMM8c8),       intent(in) :: rhs
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
     end function c8_le_c8

!DIR$ ATTRIBUTES INLINE :: c8_le_c2
     pure elemental function c8_le_c2(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c8_le_c2
       !DIR$ ATTRIBUTES VECTOR :: c8_le_c2
       type(ZMM8c8),    intent(in) :: lhs
       complex(kind=dp),intent(in), value :: rhs
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
     end function c8_le_c2

!DIR$ ATTRIBUTES INLINE :: c2_le_c8
     pure elemental function c2_le_c8(lhs,rhs) result(bres)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c2_le_c8
       !DIR$ ATTRIBUTES VECTOR :: c2_le_c8
       complex(kind=dp), intent(in), value :: lhs
       type(ZMM8c8),     intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 32 :: mre,mim
       logical(kind=i4), dimension(0:7) :: mre,mim
       logical(kind=i1), dimension(0:1) :: bres
       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = real(lhs,kind=dp) <= rhs.re
       bres(0) = all(mre)
       mim = aimag(lhs,kind=dp) <= rhs.im
       bres(1) = all(mim)
     end function c2_le_c8
     
     
#endif
!DIR$ ATTRIBUTES INLINE :: polar
     pure elemental function polar(rho,theta) result (iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: polar
       !DIR$ ATTRIBUTES VECTOR :: polar
       type(ZMM8r8_t), intent(in) :: rho
       type(ZMM8r8_t), intent(in) :: theta
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          iq.re(i) = rho.v(i)*cos(theta.v(i))
          iq.im(i) = rho.v(i)*sin(theta.v(i)) 
       end do
     end function polar
     

       pure function cnorm(x) result(cn) 
#elif defined __ICC || defined __INTEL_COMPILER
           !DIR$ ATTRIBUTES INLINE :: cnorm
           !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: cnorm
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cnorm
#endif
            type(ZMM8c8),  intent(in) :: x
            type(ZMM8r8_t) :: cn
            integer(kind=i4) :: i
            !dir$ loop_count(8)
            !dir$ vector aligned
            !dir$ vector vectorlength(8)
            !dir$ vector always
            do i=0,7
               cn.v(i) = sqrt(x.re(i)*x.re(i)+ &
                              x.im(i)*x.im(i))
            end do
       end function cnorm
       
!DIR$ ATTRIBUTES INLINE :: carg_zmm8c8
     pure elemental function carg_zmm8c8(c8) result(arg)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: carg_zmm8c8
       !DIR$ ATTRIBUTES VECTOR :: carg_zmm8c8
       type(ZMM8c8),   intent(in) :: c8
       !DIR$ ATTRIBUTES ALIGN : 64 :: arg
       type(ZMM8r8_t) :: arg
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
           arg.v(i)  = atan2(c8.im(i),c8.re(i))
       end do
     end function carg_zmm8c8

!DIR$ ATTRIBUTES INLINE :: carg_2xzmm8r8     
     pure elemental function carg_2xzmm8r8(re,im) result(arg)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: carg_2xzmm8r8
       !DIR$ ATTRIBUTES VECTOR :: carg_2xzmm8r8
       type(ZMM8r8_t),  intent(in) :: re
       type(ZMM8r8_t),  intent(in) :: im
       !DIR$ ATTRIBUTES ALIGN : 64 :: arg
       type(ZMM8r8_t) :: arg
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
           arg.v(i) = atan2(im.v(i),re.v(i))
       end do
     end function carg_2xzmm8r8

!DIR$ ATTRIBUTES INLINE :: csin_zmm8c8
     pure elemental function csin_zmm8c8(c8) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: csin_zmm8c8
       !DIR$ ATTRIBUTES VECTOR :: csin_zmm8c8
       type(ZMM8c8),  intent(in) :: c8
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       !DIR$ ATTRIBUTES ALIGN : 64 :: tre,tim
       type(ZMM8r8_t) :: tre
       type(ZMM8r8_t) :: tim
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          tre.v(i) = c8.re(i)
          tim.v(i) = c8.im(i)
          iq.re(i) = sin(tre.v(i))*cosh(tim.v(i))
          iq.im(i) = cos(tre.v(i))*sinh(tim.v(i))
     end function csin_zmm8c8

!DIR$ ATTRIBUTES INLINE :: csin_zmm8r8
     pure elemental function csin_zmm8r8(re,im) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: csin_zmm8r8
       !DIR$ ATTRIBUTES VECTOR :: csin_zmm8r8
       type(ZMM8r8_t),   intent(in) :: re
       type(ZMM8r8_t),   intent(in) :: im
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          iq.re(i) = sin(re.v(i))*cosh(im.v(i))
          iq.im(i) = cos(re.v(i))*sinh(im.v(i))
       end do
     end function csin_zmm8r8

!DIR$ ATTRIBUTES INLINE :: csinh_zmm8c8
     pure elemental function csinh_zmm8c8(c8) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: csinh_zmm8c8
       !DIR$ ATTRIBUTES VECTOR :: csinh_zmm8c8
       type(ZMM8c8),   intent(in) :: c8
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       !DIR$ ATTRIBUTES ALIGN : 64 :: tre,tim
       type(ZMM8r8_t) :: tre
       type(ZMM8r8_t) :: tim
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          tre.v(i) = c8.re(i)
          tim.v(i) = c8.im(i)
          iq.re(i) = sinh(tre.v(i))*cos(tim.v(i))
          iq.im(i) = cosh(tre.v(i))*sin(tim.v(i))
       end do
     end function csinh_zmm8c8

!DIR$ ATTRIBUTES INLINE :: csinh_zmm8r8
     pure elemental function csinh_zmm8r8(re,im) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: csinh_zmm8r8
       !DIR$ ATTRIBUTES VECTOR :: csinh_zmm8r8
       type(ZMM8r8_t), intent(in) :: re
       type(ZMM8r8_t), intent(in) :: im
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          iq.re(i) = sinh(re.v(i))*cos(im.v(i))
          iq.im(i) = cosh(re.v(i))*sin(im.v(i))
       end do
     end function csinh_zmm8r8

!DIR$ ATTRIBUTES INLINE :: ccos_zmm8c8
     pure elemental function ccos_zmm8c8(c8) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ccos_zmm8c8
       !DIR$ ATTRIBUTES VECTOR :: ccos_zmm8c8
       type(ZMM8c8),   intent(in) :: c8
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       !DIR$ ATTRIBUTES ALIGN : 64 :: tre,tim
       type(ZMM8r8_t) :: tre
       type(ZMM8r8_t) :: tim
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          tre.v(i) = c8.re(i)
          tim.v(i) = c8.im(i)
          iq.re(i) = cos(tre.v(i))*cosh(tim.v(i))
          iq.im(i) = sin(tre.v(i))*sinh(tim.v(i))
       end do
    end function ccos_zmm8c8

!DIR$ ATTRIBUTES INLINE :: ccos_zmm8r8
     pure elemental function ccos_zmm8r8(re,im) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ccos_zmm8r8
       !DIR$ ATTRIBUTES VECTOR :: ccos_zmm8r8
       type(ZMM8r8_t), intent(in), value :: re
       type(ZMM8r8_t), intent(in), value :: im
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          iq.re(i) = cos(re.v(i))*cosh(im.v(i))
          iq.im(i) = sin(re.v(i))*sinh(im.v(i))
       end do
     end function ccos_zmm8r8

!DIR$ ATTRIBUTES INLINE :: ccosh_zmm8c8
     pure elemental function ccosh_zmm8c8(c8) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ccosh_zmm8c8
       !DIR$ ATTRIBUTES VECTOR :: ccosh_zmm8c8
       type(ZMM8c8),    intent(in) :: c8
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       !DIR$ ATTRIBUTES ALIGN : 64 :: tre,tim
       type(ZMM8r8_t) :: tre
       type(ZMM8r8_t) :: tim
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          tre.v(i) = c8.re(i)
          tim.v(i) = c8.im(i)
          iq.re(i) = cosh(tre.v(i))*cos(tim.v(i))
          iq.im(i) = sinh(tre.v(i))*sin(tim.v(i))
       end do
     end function ccosh_zmm8c8

!DIR$ ATTRIBUTES INLINE :: ccosh_zmm8r8
     pure elemental function ccosh_zmm8r8(re,im) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ccosh_zmm8r8
       !DIR$ ATTRIBUTES VECTOR :: ccosh_zmm8r8
       type(ZMM8r8_t),   intent(in), value :: re
       type(ZMM8r8_t),   intent(in), value :: im
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          iq.re(i) = cosh(re.v(i))*cos(im.v(i))
          iq.im(i) = sinh(re.v(i))*sin(im.v(i))
       end do
     end function ccosh_zmm8r8

!DIR$ ATTRIBUTES INLINE :: cexp_zmm8c8
     pure elemental function cexp_zmm8c8(c8) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cexp_zmm8c8
       !DIR$ ATTRIBUTES VECTOR :: cexp_zmm8c8
       type(ZMM8c8),  intent(in) :: c8
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       !DIR$ ATTRIBUTES ALIGN : 64 :: tre,tim
       type(ZMM8r8_t) :: tre
       type(ZMM8r8_t) :: tim
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          tre.v(i) = c8.re(i)
          tim.v(i) = c8.im(i)
          iq.re(i) = exp(tre.v(i))*cos(tim.v(i))
          iq.im(i) = exp(tre.v(i))*sin(tim.v(i))
       end do
     end function cexp_zmm8c8

!DIR$ ATTRIBUTES INLINE :: ctan_zmm8c8
     pure elemental function ctan_zmm8c8(x) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ctan_zmm8c8
       !DIR$ ATTRIBUTES VECTOR :: ctan_zmm8c8
       type(ZMM8c8),  intent(in) :: x
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       iq = csin_zmm8c8(x)/ccos_zmm8c8(x)
     end function ctan_zmm8c8

!DIR$ ATTRIBUTES INLINE :: ctanh_zmm8c8
     pure elemental function ctanh_zmm8c8(x) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ctanh_zmm8c8
       !DIR$ ATTRIBUTES VECTOR :: ctanh_zmm8c8
       type(ZMM8c8),  intent(in) :: x
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       iq = csinh_zmm8c8(x)/ccosh_zmm8c8(x)
     end function ctanh_zmm8c8
     
!DIR$ ATTRIBUTES INLINE :: cexp_zmm8r8
     pure elemental function cexp_zmm8r8(re,im) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cexp_zmm8r8
       !DIR$ ATTRIBUTES VECTOR :: cexp_zmm8r8
       type(ZMM8r8_t),  intent(in), value :: re
       type(ZMM8r8_t),  intent(in), value :: im
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          iq.re(i) = exp(re.v(i))*cos(im.v(i))
          iq.im(i) = exp(re.v(i))*sin(im.v(i))
       end do
     end function cexp_zmm8r8

!DIR$ ATTRIBUTES INLINE :: cabs_zmm8c8
     pure elemental function cabs_zmm8c8(c8) result(val)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cabs_zmm8c8
       !DIR$ ATTRIBUTES VECTOR :: cabs_zmm8c8
       type(ZMM8c8), intent(in) :: c8
       !DIR$ ATTRIBUTES ALIGN : 64 :: val
       type(ZMM8r8_t) :: val
       !DIR$ ATTRIBUTES ALIGN : 64 :: tre,tim
       type(ZMM8r8_t) :: tre,tim
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          tre.v(i) = c8.re(i)
          tim.v(i) = c8.im(i)
          val.v(i) = sqrt(tre.v(i)*tre.v(i)+ &
                       tim.v(i)*tim.v(i))
       end do
     end function cabs_zmm8c8

!DIR$ ATTRIBUTES INLINE :: cabs_2xzmm8r8
     pure elemental function cabs_2xzmm8r8(re,im) result(val)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cabs_2xzmm8r8
       !DIR$ ATTRIBUTES VECTOR :: cabs_2xzmm8r8
       type(ZMM8r8_t),  intent(in), value :: re
       type(ZMM8r8_t),  intent(in), value :: im
       !DIR$ ATTRIBUTES ALIGN : 64 :: val
       type(ZMM8r8_t) :: val
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          val.v(i) = sqrt(re.v(i)*re.v(i)+ &
                          im.v(i)*im.v(i))
       end do
     end function cabs_2xzmm8r8

!DIR$ ATTRIBUTES INLINE :: cpow_zmm8c8
     pure elemental function cpow_zmm8c8(c8,n) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cpow_zmm8c8
       !DIR$ ATTRIBUTES VECTOR :: cpow_zmm8c8
       type(ZMM8c8), intent(in) :: c8
       real(kind=dp),       intent(in) :: n
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       !DIR$ ATTRIBUTES ALIGN : 64 :: tre,tim
       type(ZMM8r8_t) :: tre,tim
       !DIR$ ATTRIBUTES ALIGN : 64 :: r,theta,pow,trig
       type(ZMM8r8_t) :: r,theta,pow,trig
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          tre.v(i) = c8.re(i)
          tim.v(i) = c8.im(i)
          r.v(i) = sqrt(tre.v(i)*tre.v(i)+ &
                       tim.v(i)*tim.v(i))
          pow.v(i)   = r.v(i)**n
          theta.v(i) = atan(tim.v(i)/tre.v(i))
          trig.v(i)  = theta.v(i)*n
          iq.re(i)   = pow.v(i)*cos(trig.v(i))
          iq.im(i)   = pow.v(i)*sin(trig.v(i))
       end do
     end function cpow_zmm8c8

!DIR$ ATTRIBUTES INLINE :: cpow_2xzmm8r8
     pure elemental function cpow_2xzmm8r8(re,im,n) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cpow_2xzmm8r8
       !DIR$ ATTRIBUTES VECTOR :: cpow_2xzmm8r8
       type(ZMM8r8_t),   intent(in), value :: re
       type(ZMM8r8_t),   intent(in), value :: im
       real(kind=dp),    intent(in) :: n
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       !DIR$ ATTRIBUTES ALIGN : 64 :: tre,tim
       type(ZMM8r8_t) :: tre,tim
       !DIR$ ATTRIBUTES ALIGN : 64 :: r,theta,pow,trig
       type(ZMM8r8_t) :: r,theta,pow,trig
       integer(kind=i4) :: i
       !dir$ loop_count(8)
       !dir$ vector aligned
       !dir$ vector vectorlength(8)
       !dir$ vector always
       do i=0,7
          r.v(i)     = sqrt(re.v(i)*re.v(i)+ &
                        im.v(i)*im.v(i))
          pow.v(i)   = r.v(i)**n
          theta.v(i) = atan(im.v(i)/re.v(i))
          trig.v(i)  = theta.v(i)*n
          iq.re(i)   = pow.v(i)*cos(trig.v(i))
          iq.im(i)   = pow.v(i)*sin(trig.v(i))
       end do
     end function cpow_2xzmm8r8

!DIR$ ATTRIBUTES INLINE :: clog_zmm8c8    
     pure elemental function clog_zmm8c8(c8) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: clog_zmm8c8
       !DIR$ ATTRIBUTES VECTOR :: clog_zmm8c8
       type(ZMM8c8),   intent(in) :: c8
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM8c8) :: iq
       !DIR$ ATTRIBUTES ALIGN : 64 :: t0
       type(ZMM8r8_t) :: t0
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
        type(ZMM8c8) :: iq
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
        type(ZMM8c8),   intent(in) :: c8
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM8c8) :: iq
        !DIR$ ATTRIBUTES ALIGN : 64 :: t0,t1,t2
        type(ZMM8r8_t) :: t0
        type(ZMM8r8_t) :: t1
        type(ZMM8r8_t) :: t2
        integer(kind=i4) :: i
        t0 = cabs_zmm8c8(c8)
        !dir$ loop_count(8)
        !dir$ vector aligned
        !dir$ vector vectorlength(8)
        !dir$ vector always
        do i=0,7
           t1.v(i)  = v8_1over2.v(i)*(t0.v(i)+c8.re(i))
           iq.re(i) = sqrt(t1.v(i))
           t2.v(i)  = v8_1over2.v(i)*(t0.v(i)-c8.re(i))
           iq.im(i) = sqrt(t2.v(i))
        end do
      end function csqrt_zmm8c8   
     

!DIR$ ATTRIBUTES INLINE :: csqrt_2xzmm8r8
      pure elemental function csqrt_2xzmm8r8(re,im) result(iq)
        use mod_vecconsts, only : v8_1over2
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: csqrt_2xzmm8r8
        !DIR$ ATTRIBUTES VECTOR :: csqrt_2xzmm8r8
        type(ZMM8r8_t),  intent(in), value :: re
        type(ZMM8r8_t),  intent(in), value :: im
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM8c8) :: iq
        !DIR$ ATTRIBUTES ALIGN : 64 :: t0,t1,t2
        type(ZMM8r8_t) :: t0
        type(ZMM8r8_t) :: t1
        type(ZMM8r8_t) :: t2
        integer(kind=i4) :: i
        t0 = cabs_2xzmm8r8(re,im)
        !dir$ loop_count(8)
        !dir$ vector aligned
        !dir$ vector vectorlength(8)
        !dir$ vector always
        do i=0,7
           t1.v(i)  = v8_1over2.v(i)*(t0.v(i)+re.v(i))
           iq.re(i) = sqrt(t1.v(i))
           t2.v(i)  = v8_1over2.v(i)*(t0.v(i)-re.v(i))
        end do
      end function csqrt_2xzmm8r8

!DIR$ ATTRIBUTES INLINE :: select_zmm8c8
      pure elemental function select_zmm8c8(lhs,rhs,mask) result(iq)
        use, intrinsic :: ISO_C_BINDING
        use mod_avx512_bindings, only : v8f64, v8f64_mask_blend_pd
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: select_zmm8c8
        !DIR$ ATTRIBUTES VECTOR :: select_zmm8c8
        type(ZMM8c8),  intent(in) :: lhs
        type(ZMM8c8),  intent(in) :: ths
        integer(c_char),      intent(in) :: mask
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM8c8) :: iq
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
        type(ZMM8c8), intent(in) :: c8
        integer(c_char),     intent(in) :: k
        integer(c_int),      intent(in) :: imm
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM8c8) :: iq
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
        type(ZMM8c8),  intent(in) :: c8
        integer(c_char),      intent(in) :: k
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM8c8) :: iq
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
        type(ZMM8c8),  intent(in) :: lhs
        type(ZMM8c8),  intent(in) :: rhs
        ! DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM8c8) :: iq
        !DIR$ ATTRIBUTES ALIGN : 64 :: ratio,denom
        type(ZMM8r8_t) :: ratio,denom
        !DIR$ ATTRIBUTES ALIGN : 32 :: bres
        logical(kind=i4), dimension(0:7) :: bres
        integer(kind=i4) :: i
#if   (USE_INTRINSIC_VECTOR_COMPARE) == 1
        !DIR$ ATTRIBUTES ALIGN : 64 :: tre,tim
        type(v8f64) :: tre,tim
        integer(c_char), automatic :: mask_gte
        integer(c_char), parameter :: all_ones = Z'FF'
#endif
        ! EXec code ....
        ratio.v = v8_n0.v
        denom.v = v8_n0.v
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
           
          !dir$ loop_count(8)
          !dir$ vector aligned
          !dir$ vector vectorlength(8)
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
          !dir$ vector vectorlength(8)
          !dir$ vector always
          do i=0,7
             ratio.v(i)   = rhs.re(i)/rhs.im(i)
             denom.v(i)   = rhs.im(i)+ratio.v(i)*rhs.re(i)
             iq.re(i)     = (lhs.re(i)*ratio.v(i)+lhs.im(i))/denom.v(i)
             iq.im(i)     = (lhs.im(i)*ratio.v(i)-lhs.re(i))/denom.v(i)
        end if
     end function cdiv_smith

      
     
end module avx512_cvec8_v2
