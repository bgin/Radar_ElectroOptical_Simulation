
#include "GMS_config.fpp"

module  avx512_cvec16_v2


 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'avx512_cvec16_v2'
 !          
 !          Purpose:
 !                      This module contains a decomposed to real and imaginary
 !                      parts a complex vector of 16 elements (complex numbers)
 !                      This representation nicely fits into 2 AVX-512 ZMMx
 !                      registers.
 !                      This implementation is based on explicit vector programming
 !                      concept.
 !          History:
 !                        Date: 28-07-2022
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

     use mod_kinds, only : i1,i4,sp
     use mod_vectypes, only : ZMM16r4_t
     use, intrinsic :: ISO_C_BINDING
     implicit none
     !=====================================================59
     !  File and module information:
     !  version,creation and build date, author,description
     !=====================================================59

     ! Major version
     integer(kind=i4), parameter :: MOD_AVX512_CVEC16_V2_MAJOR = 1
     ! Minor version
     integer(kind=i4), parameter :: MOD_AVX512_CVEC16_V2_MINOR = 0
     ! Micro version
     integer(kind=i4), parameter :: MOD_AVX512_CVEC16_V2_MICRO = 1
     ! Full version
     integer(kind=i4), parameter :: MOD_AVX512_CVEC16_V2_FULLVER = &
          1000*MOD_AVX512_CVEC16_V2_MAJOR+100*MOD_AVX512_CVEC16_V2_MINOR+10*MOD_AVX512_CVEC16_V2_MICRO
     ! Module creation date
     character(*),       parameter :: MOD_AVX512_CVEC16_V2_CREATION_DATE = "28-07-2022 11:57 +00200 (THR 28 JUL 2022 GMT+2)"
     ! Module build date
     character(*),       parameter :: MOD_AVX512_CVEC16_V2_BUILD_DATE    = __DATE__ " " __TIME__
     ! Module author info
     character(*),       parameter :: MOD_AVX512_CVEC16_V2_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),       parameter :: MOD_AVX512_CVEC16_V2_SYNOPSIS      = "Explicitly vectorized complex container of 16 elements (single precision)"

     type, public :: ZMM16c4
        
        sequence
        real(kind=sp), dimension(0:15) :: re
        real(kind=sp), dimension(0:15) :: im
     end type ZMM16c4
     
     interface assignment (=)
         module procedure c16_assign_c16
         module procedure c16_assign_v16
     end interface assignment (=)

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

#if 0

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
#endif
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



    contains


       
      pure function default_init() result(iq)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: default_init
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: default_init
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: default_init
        use omp_lib
        use mod_vecconsts, only : v16_n0
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4) :: iq
    
        integer(kind=i4) :: i
        !dir$ assume_aligned iq:64
        !dir$ loop_count(15)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !$omp simd
        do i=0, 15 
            iq.re(i) = 0.0_sp
            iq.im(i) = 0.0_sp
        end do
      end function default_init



        
      pure function array_init(re,im) result(iq)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: array_init
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: array_init
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: array_init
        real(kind=sp), dimension(0:15), intent(in) :: re
        real(kind=sp), dimension(0:15), intent(in) :: im
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4) :: iq
        integer(kind=i4) :: i
        !dir$ assume_aligned re:64
        !dir$ assume_aligned im:64
        !dir$ loop_count(15)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           iq.re(i) = re(i)
           iq.im(i) = im(i)
        end do
      end function array_init



      
      pure function complex1_init(c) result(iq)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: complex1_init
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: complex1_init
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: complex_init

        complex(kind=sp),     intent(in) :: c
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4) :: iq
        real(kind=sp), automatic :: cr,ci
        integer(kind=i4) :: i
        cr = real(c,kind=sp)
        ci = aimag(c,kind=sp)
        !dir$ loop_count(15)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           iq.re = cr
           iq.im = ci
        end do
      end function complex1_init


      pure function complex2x16_init(c) result(iq)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: complex2x16_init
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: complex2x16_init
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: complex2x16_init
        complex(kind=sp), dimension(0:15),  intent(in) :: c
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4) :: iq
        integer(kind=i4) :: i
        !dir$ assume_aligned c:64
        !dir$ loop_count(15)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
 
           iq.re = real(c(i),kind=sp)
           iq.im = aimag(c(i),kind=sp)
        end do
      end function complex2x16_init


        
      pure function zmm16r42x_init(v1,v2) result(iq)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: zmm16r42x_init
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: zmm16r42x_init
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: zmm16r42x_init
        type(ZMM16r4_t),    intent(in) :: v1
        type(ZMM16r4_t),    intent(in) :: v2
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4) :: iq
        integer(kind=i4) :: i
        !dir$ loop_count(15)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           iq.re(i) = v1.v(i)
           iq.im(i) = v2.v(i)
        end do
      end function zmm16r42x_init
      
 
      pure function zmm16r41x_init(v) result(iq)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: zmm16r41x_init
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: zmm16r41x_init
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: zmm16r41x_init
        type(ZMM16r4_t),    intent(in) :: v
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4) :: iq
        integer(kind=i4) :: i
        !dir$ loop_count(15)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           iq.re(i) = v.v(i)
           iq.im = v16_n0.v(i)
        end do
      end function zmm16r41x_init


      pure function r41x_init(s) result(iq)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: r41x_init
        !DIR$M ATTRIBUTES CODE_ALIGN : 16 :: r41x_init
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: r41x_init
        real(kind=sp),    intent(in) :: s
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4) :: iq
        integer(kind=i4) :: i
        !dir$ loop_count(15)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           iq.re(i) = s
           iq.im(i) = v16_n0.v(i)
        end do
      end function r41x_init



      
      pure function copy_init(rhs) result(iq)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: copy_init
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: copy_init
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: copy_init

        type(ZMM16c4),    intent(in) :: rhs
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4) :: iq
        integer(kind=i4) :: i
        !dir$ loop_count(15)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           iq.re(i) = rhs.re(i)
           iq.im(i) = rhs.im(i)
        end do
      end function copy_init
      
      
      pure function c16_assign_c16(x) result(iq)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: c16_assign_c16
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_assign_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c16_assign_c16
        type(ZMM16c4),    intent(in) :: x
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4) :: iq
        integer(kind=i4) :: i
        !dir$ loop_count(15)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           iq.re(i) = x.re(i)
           iq.im(i) = x.im(i)
        end do
      end function c16_assign_c16
      
      
      pure function c16_assign_v16(x) result(iq)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: c16_assign_v16
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_assign_v16
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c16_assign_v16
        use mod_vecconsts, only : v16_0
        type(ZMM16r4_t),    intent(in) :: x
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4) :: iq
        integer(kind=i4) :: i
        !dir$ loop_count(15)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           iq.re(i) = x.v(i)
           iq.im(i) = v16_0.v
        end do
      end function c16_assign_v16
      
      pure function c16_add_c16(x,y) result(iq)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: c16_add_c16
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_add_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c16_add_c16
        type(ZMM16c4),    intent(in) :: x
        type(ZMM16c4),    intent(in) :: y
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4) :: iq
        integer(kind=i4) :: i
        !dir$ loop_count(15)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           iq.re(i) = x.re(i)+y.re(i)
           iq.im(i) = x.im(i)+y.im(i)
        end do
      end function c16_add_c16


      
      pure function c16_add_c1(x,y) result(iq)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: c16_add_c1
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_add_c1
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c16_add_c1
        type(ZMM16c4),    intent(in) :: x
        complex(kind=sp), intent(in) :: y
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4) :: iq
        real(kind=sp), automatic :: cr,ci
        integer(kind=i4) :: i
        cr = real(y,kind=sp)
        ci = aimag(y,kind=sp)
        !dir$ loop_count(15)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           iq.re(i) = x.re(i)+cr
           iq.im(i) = x.im(i)+ci
        end do
      end function c16_add_c1


        
      pure function c16_add_v16(x,y) result(iq)
         !DIR$ OPTIMIZE:3
         !DIR$ ATTRIBUTES INLINE :: c16_add_v16
         !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_add_v16
         !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c16_add_v16
        use mod_vecconsts, only : v16_n0
        type(ZMM16c4),    intent(in) :: x
        type(ZMM16r4_t),  intent(in) :: y
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4) :: iq
        integer(kind=i4) :: i
        !dir$ loop_count(15)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           iq.re(i) = x.re(i)+y.v(i)
           iq.im(i) = v16_n0.v(i)
        end do
      end function c16_add_v16


        
      pure function c16_add_s1(x,y) result(iq)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: c16_add_s1
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_add_s1
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c16_add_s1

        use mod_veccconts, only : v16_n0
        type(ZMM16c4),    intent(in) :: x
        real(kind=sp),           intent(in) :: y
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4) :: iq
        integer(kind=i4) :: i
        !dir$ loop_count(15)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           iq.re(i) = x.re(i)+y
           iq.im(i0 = v16_n0.v(i)
        end do
      end function c16_add_s1


     
      pure function c1_add_c16(x,y) result(iq)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: c1_add_c16
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c1_add_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c1_add_c16
        complex(kind=sp),      intent(in) :: x
        type(ZMM16c4),  intent(in) :: y
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4) :: iq
        real(kind=sp), automatic :: cr,ci
        integer(kind=i4) :: i
        cr = real(x,kind=sp)
        ci = aimag(x,kind=sp)
         !dir$ loop_count(15)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           iq.re(i) = cr+y.re(i)
           iq.im(i) = ci+y.im(i)
        end do
      end function c1_add_c16


      
      pure function v16_add_c16(x,y) result(iq)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: v16_add_c16
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: v16_add_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: v16_add_c16
        use mod_vecconsts, only : v16_n0
        type(ZMM16r4_t),      intent(in) :: x
        type(ZMM16c4), intent(in) :: y
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4) :: iq
        integer(kind=i4) :: i
        !dir$ loop_count(16)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           iq.re(i) = x.v(i)+y.re(i)
           iq.im(i) = v16_n0.v(i)
        end do
      end function v16_add_c16


      
      pure function s1_add_c16(x,y) result(iq)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: s1_add_c16
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: s1_add_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: s1_add_c16
        use mod_vecconsts, only : v16_n0
        real(kind=sp),         intent(in) :: x
        type(ZMM16c4),  intent(in) :: y
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4) :: iq
        integer(kind=i4) :: i
        !dir$ loop_count(16)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           iq.re(i) = x
           iq.im(i) = v16_n0.v(i)
        end do
      end function s1_add_c16


       
      pure function c16_sub_c16(x,y) result(iq)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: c16_sub_c16
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_sub_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c16_sub_c16
        type(ZMM16c4),     intent(in) :: x
        type(ZMM16c4),     intent(in) :: y
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4) :: iq
        integer(kind=i4) :: i
        !dir$ loop_count(16)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           iq.re(i) = x.re(i)-y.re(i)
           iq.im(i) = x.im(i)-y.im(i)
        end do
      end function c16_sub_c16
        

         
      pure function c16_sub_c1(x,y) result(iq)
         !DIR$ OPTIMIZE:3
         !DIR$ ATTRIBUTES INLINE :: c16_sub_c1
         !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_sub_c1
         !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c16_sub_c1
        type(ZMM16c4),           intent(in) :: x
        complex(kind=sp),        intent(in) :: y
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4) :: iq
        real(kind=sp), automatic :: cr,ci
        integer(kind=i4) :: i
        cr = real(y,kind=sp)
        ci = aimag(y,kind=sp)
        !dir$ loop_count(16)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           iq.re(i) = x.re(i)-cr
           iq.im(i) = x.im(i)-ci
        end do
      end function c16_sub_c1


       
      pure function c16_sub_v16(x,y) result(iq)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: c16_sub_v16
         !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_sub_v16
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c16_sub_v16
        use mod_vecconsts, only : v16_n0
        type(ZMM16c4),     intent(in) :: x
        type(ZMM16r4_t),   intent(in) :: y
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4) :: iq
        integer(kind=i4) :: i
        !dir$ loop_count(16)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           iq.re(i) = x.re(i)-y.v(i)
           iq.im(i0 = v16_n0.v(i)
        end do
      end function c16_sub_v16


       
      pure function c16_sub_s1(x,y) result(iq)
        !DIR$V OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: c16_sub_s1
         !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_sub_s1
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c16_sub_s1
        use mod_vecconsts, only : v16_n0
        type(ZMM16c4),    intent(in) :: x
        real(kind=sp),    intent(in) :: y
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4) :: iq
        integer(kind=i4) :: i
        !dir$ loop_count(16)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           iq.re(i) = x.re(i)-y
           iq.im(i) = v16_n0.v(i)
        end do
      end function c16_sub_s1


       
      pure function c1_sub_c16(x,y) result(iq)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: c1_sub_c16
         !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c1_sub_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c1_sub_c16  
        complex(kind=sp),     intent(in) :: x
        type(ZMM16c4),        intent(in) :: y
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4) :: iq
        real(kind=sp), automatic :: cr,ci
        integer(kind=i4) :: i
        cr = real(x,kind=sp)
        ci = aimag(x,kind=sp)
        !dir$ loop_count(16)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           iq.re(i) = cr-y.re(i)
           iq.im(i) = ci-y.im(i)
        end do
      end function c1_sub_c16


      
      pure function v16_sub_c16(x,y) result(iq)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: v16_sub_c16
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: v16_sub_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: v16_sub_c16
        use mod_vecconsts, only : v16_n0
        type(ZMM16r4_t),      intent(in) :: x
        type(ZMM16c4), intent(in) :: y
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4) :: iq
        integer(kind=i4) :: i
        !dir$ loop_count(16)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           iq.re(i) = x.v(i)-y.re(i)
           iq.im(i) = v16_n0.v(i)
        end do
      end function v16_sub_c16


        
      pure function s1_sub_c16(x,y) result(iq)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: s1_sub_c16
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: s1_sub_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: s1_sub_c16
         use mod_vecconsts, only : v16_n0
        real(kind=sp),           intent(in) :: x
        type(ZMM16c4),    intent(in) :: y
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4) :: iq
        integer(kind=i4) :: i
        !dir$ loop_count(16)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           iq.re(i) = x-y.re(i)
           iq.im(i) = v16_n0.v(i)
        end do
      end function s1_sub_c16


       
      pure function c16_mul_c16(x,y) result(iq)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: c16_mul_c16
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_mul_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c16_mul_c16
        type(ZMM16c4),   intent(in) :: x
        type(ZMM16c4),   intent(in) :: y
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4) :: iq
        !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0,zmm1,zmm2,zmm3
        type(ZMM16r4_t), automatic :: zmm0,zmm1,zmm2,zmm3
        integer(kind=i4) :: i
        !dir$ loop_count(16)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           zmm0.v(i) = x.re(i)*y.re(i)
           zmm1.v(i) = x.im(i)*y.im(i)
           iq.re(i)  = zmm0.v(i)+zmm1.v(i)
           zmm2.v(i) = x.im(i)*y.re(i)
           zmm3.v(i) = x.re(i)*y.im(i)
           iq.im(i)  = zmm2.v(i)-zmm3.v(i)
        end do
     end function c16_mul_c16
    

      
     pure function c16_mul_c1(x,y) result(iq)
       !DIR$ OPTIMIZE:3
       !DIR$ ATTRIBUTES INLINE :: c16_mul_c1
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_mul_c1
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c16_mul_c1
       type(ZMM16c4),     intent(in) :: x
       complex(kind=sp),         intent(in) :: y
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM16c4) :: iq
       !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0,zmm1,zmm2,zmm3
       type(ZMM16r4_t), automatic :: zmm0,zmm1,zmm2,zmm3
       real(kind=sp), automatic :: cr,ci
       integer(kind=i4) :: i
       cr = real(y,kind=sp)
       ci = aimag(y,kind=sp)
        !dir$ loop_count(16)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
       do i=0, 15
          zmm0.v(i) = x.re(i)*cr
          zmm1.v(i) = x.im(i)*ci
          iq.re(i)  = zmm0.v(i)+zmm1.v(i)
          zmm2.v(i) = x.im(i)*cr
          zmm3.v(i) = x.re(i)*ci
          iq.im(i)  = zmm2.v(i)-zmm3.v(i)
       end do
     end function c16_mul_c1


      
     pure function c16_mul_v16(x,y) result(iq)
       !DIR$ OPTIMIZE:3
       !DIR$ ATTRIBUTES INLINE :: c16_mul_v16
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_mul_v16
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c16_mul_v16
       type(ZMM16c4),     intent(in) :: x
       type(ZMM16r4_t),          intent(in) :: y
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM16c4) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(16)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0, 15
          iq.re(i) = x.re(i)*y.v(i)
          iq.im(i) = x.im(i)*y.v(i)
       end do
     end function c16_mul_v16
       

       
     pure function c16_mul_s1(x,y) result(iq)
       !DIR$ OPTIMIZE:3
       !DIR$ ATTRIBUTES INLINE :: c16_mul_s1
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_mul_s1
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c16_mul_s1
       type(ZMM16c4), intent(in) :: x
       real(kind=sp), intent(in) :: y
       !DIR$ ATTRIBUTES ALIGN : 64 :: iq
       type(ZMM16c4) :: iq
       integer(kind=i4) :: i4
       !dir$ loop_count(16)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0, 15
          iq.re(i) = x.re(i)*y
          iq.im(i) = x.im(i)*y
       end do
      end function c16_mul_s1


       
      pure function c1_mul_c16(x,y) result(iq)
         !DIR$ OPTIMIZE:3
         !DIR$ ATTRIBUTES INLINE :: c1_mul_c16
         !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c1_mul_c16
         !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c1_mul_c16
        complex(kind=sp),     intent(in) :: x
        type(ZMM16c4), intent(in) :: y
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4) :: iq
        !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0,zmm1,zmm2,zmm3
        type(ZMM16r4_t), automatic :: zmm0,zmm1,zmm2,zmm3
        real(kind=sp), automatic :: cr,ci
        integer(kind=i4) :: i
        cr = real(x,kind=sp)
        ci = aimag(x,kind=sp)
        !dir$ loop_count(16)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           zmm0.v(i) = cr*y.re(i)
           zmm1.v(i) = ci*y.im(i)
           iq.re(i)  = zmm0.v(i)+zmm1.v(i)
           zmm2.v(i) = cr*y.im(i)
           zmm3.v(i) = ci*y.re(i)
           iq.im(i)  = zmm2.v(i)-zmm3.v(i)
        end do
     end function c1_mul_c16


      
     pure function v16_mul_c16(x,y) result(iq)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: v16_mul_c16
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: v16_mul_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: v16_mul_c16
        type(ZMM16r4_t),       intent(in) :: x
        type(ZMM16c4),  intent(in) :: y
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4) :: iq
        integer(kind=i4) :: i
        !dir$ loop_count(16)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           iq.re(i) = x.v(i)*y.re(i)
           iq.im(i) = x.v(i)*y.im(i)
        end do
      end function v16_mul_c16


        
      pure function s1_mul_c16(x,y) result(iq)
          !DIR$ OPTIMIZE:3
          !DIR$ ATTRIBUTES INLINE :: s1_mul_c16
          !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: s1_mul_c16
          !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: s1_mul_c16
          real(kind=sp),        intent(in) :: x
          type(ZMM16c4), intent(in) :: y
          !DIR$ ATTRIBUTES ALIGN : 64 :: iq
          type(ZMM16c4) :: iq
          integer(kind=i4) :: i
         !dir$ loop_count(16)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
          do i=0, 15
             iq.re(i) = x*y.re(i)
             iq.im(i) = x*y.im(i)
          end do
      end function s1_mul_c16


        
      pure function c16_div_c16(x,y) result(iq)
         !DIR$ OPTIMIZE:3
         !DIR$ ATTRIBUTES INLINE :: c16_div_c16
         !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_div_c16
         !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c16_div_c16
        type(ZMM16c4),    intent(in) :: x
        type(ZMM16c4),    intent(in) :: y
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4) :: iq
        !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0,zmm1,zmm2,zmm3,den
        type(ZMM16r4_t), automatic :: zmm0,zmm1,zmm2,zmm3,den
        integer(kind=i4) :: i
#if (USE_SAFE_COMPLEX_DIVISION) == 1
        iq = cdiv_smith(x,y)
#else
        !dir$ loop_count(16)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           zmm0.v(i) = x.re(i)*y.re(i)
           zmm1.v(i) = x.im(i)*y.im(i)
           zmm2.v(i) = x.im(i)*y.re(i)
           zmm3.v(i) = x.re(i)*y.im(i)
           den.v(i)  = (y.re(i)*y.re(i))+(y.im(i)*y.im(i))
           iq.re(i)  = (zmm0.v(i)+zmm1.v(i))/den.v(i)
           iq.im(i)  = (zmm2.v(i)-zmm3.v(i))/den.v(i)
        end do
#endif        
     end function c16_div_c16


       
     pure function c16_div_c1(x,y) result(iq)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: c16_div_c1
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_div_c1
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c16_div_c1

        type(ZMM16c4),    intent(in) :: x
        complex(kind=sp),        intent(in) :: y

        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4) :: iq
        !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0,zmm1,zmm2,zmm3,den
        type(ZMM16r4_t), automatic :: zmm0,zmm1,zmm2,zmm3,den
        real(kind=sp), automatic :: cr,ci,t0
        integer(kind=i4) :: i
        cr = real(y,kind=sp)
        ci = aimag(y,kind=sp)
        t0 = cr*cr+ci*ci
         !dir$ loop_count(16)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           zmm0.v(i) = x.re(i)*cr
           zmm1.v(i) = x.im(i)*ci
           zmm2.v(i) = x.im(i)*cr
           zmm3.v(i) = x.re(i)*ci
           den.v(i)  = t0
           iq.re(i)  = (zmm0.v(i)+zmm1.v(i))/den.v(i)
           iq.im(i)  = (zmm2.v(i)-zmm3.v(i))/den.v(i)
       end do
     end function c16_div_c1


      
     pure function c16_div_v16(x,y) result(iq)
       !DIR$ OPTIMIZE:3
       !DIR$ ATTRIBUTES INLINE :: c16_div_v16
       !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_div_v16
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c16_div_v16
       
       type(ZMM16c4),     intent(in) :: x
       type(ZMM16r4_t),   intent(in) :: y
       type(ZMM16c4) :: iq
       integer(kind=i4) :: i
        !dir$ loop_count(16)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
       do i=0, 15
          iq.re(i) = x.re(i)/y.v(i)
          iq.im(i) = x.im(i)/y.v(i)
       end do
      end function c16_div_v16


        
      pure function c16_div_s1(x,y) result(iq)
         !DIR$ OPTIMIZE:3
         !DIR$ ATTRIBUTES INLINE :: c16_div_s1
         !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_div_s1
         !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: c16_div_s1

        type(ZMM16c4),      intent(in) :: x
        real(kind=sp),             intent(in) :: y
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4) :: iq
        integer(kind=i4) :: i
        !dir$ loop_count(16)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           iq.re(i) = x.re(i)/y
           iq.im = x.im(i)/y
        end do
      end function c16_div_s1


        
      pure function c1_div_c16(x,y) result(iq)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: c1_div_c16
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c1_div_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c1_div_c16
        complex(kind=sp),      intent(in) :: x
        type(AVX512c16f32_t),  intent(in) :: y
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4) :: iq
        !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0,zmm1,zmm2,zmm3,den
        type(ZMM16r4_t), automatic :: zmm0,zmm1,zmm2,zmm3,den
        real(kind=sp), automatic :: r,i
        integer(kind=i4) :: i
        r = real(x,kind=sp)
        i = aimag(x,kind=sp)
        !dir$ loop_count(16)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           zmm0.v(i) = r*y.re(i)
           zmm1.v(i) = i*y.im(i)
           zmm2.v(i) = i*y.re(i)
           zmm3.v(i) = r*y.im(i)
           den.v(i)  = (y.re(i)*y.re(i))+(y.im(i)*y.im(i))
           iq.re(i)  = (zmm0.v(i)+zmm1.v(i))/den.v(i)
           iq.im(i)  = (zmm2.v(i)-zmm3.v(i))/den.v(i)
        end do
      end function c1_div_c16


     
      pure function v16_div_c16(x,y) result(iq)
         !DIR$ OPTIMIZE:3
         !DIR$ ATTRIBUTES INLINE :: v16_div_c16
         !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: v16_div_c16
         !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: v16_div_c16
        type(ZMM16r4_t),       intent(in) :: x
        type(ZMM16c4),  intent(in) :: y
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4) :: iq
        integer(kind=i4) :: i
        !dir$ loop_count(16)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           iq.re(i) = x/y.re(i)
           iq.im(i) = x/y.im(i)
        end do
      end function v16_div_c16
        

      
      pure function s1_div_c16(x,y) result(iq)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: s1_div_c16
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: s1_div_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: s1_div_c16
        real(kind=sp),         intent(in) :: x
        type(ZMM16c4),  intent(in) :: y
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4) :: iq
        integer(kind=i4) :: i
          !dir$ loop_count(16)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           iq.re(i) = x/y.re(i)
           iq.im(i) = x/y.im(i)
        end do
      end function s1_div_c16


        
      pure function conjugate(x) result(iq)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: conjugate
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: conjugate
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: conjugate
        use mod_vecconsts, only : v16_n0
        type(ZMM16c4),    intent(in) :: x
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4) :: iq
        integer(kind=i4) :: i
           !dir$ loop_count(16)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           iq.re(i) = v16_n0.v(i)-x.re(i)
           iq.im(i) = x.im(i)
        end do
      end function conjugate
        
#if 0

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      pure function c16_eq_c16(x,y) result(mmask16) !GCC$ ATTRIBUTES hot :: c16_eq_c16 !GCC$ ATTRIBUTES vectorcall :: c16_eq_c16 !GCC$ ATTRIBUTES inline :: c16_eq_c16
#elif defined __ICC || defined __INTEL_COMPILER
      pure function c16_eq_c16(x,y) result(mmask16)
         !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_eq_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::c16_eq_c16
        use mod_avx512_bindings, only : v16f32, v16f32_cmp_ps_mask
#endif
        type(ZMM16c4),      intent(in) :: x
        type(ZMM16c4),      intent(in) :: y
        integer(c_short), dimension(0:1) :: mmask16
#if defined __INTEL_COMPILER 
        
        !DIR$ ATTRIBUTES ALIGN : 64 :: lre,lim,rre,rim
        type(v16f32), automatic :: lre,lim,rre,rim
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        
        type(v16f32), automatic :: lre !GCC$ ATTRIBUTES aligned(64) :: lre
        type(v16f32), automatic :: lim !GCC$ ATTRIBUTES aligned(64) :: lim
        type(v16f32), automatic :: rre !GCC$ ATTRIBUTES aligned(64) :: rre
        type(v16f32), automatic :: rim !GCC$ ATTRIBUTES aligned(64) :: rim
        
#endif
        mmask16 = 0
        lre.zmm = x.re
        rre.zmm = y.re
        mmask16(0) = v16f32_cmp_ps_mask(lre,rre,0)
        lim.zmm = x.im
        rim.zmm = y.im
        mmask16(1) = v16f32_cmp_ps_mask(lim,rim,0)
      end function c16_eq_c16

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      pure function c16_eq_c1(x,y) result(mmask16) !GCC$ ATTRIBUTES hot :: c16_eq_c1 !GCC$ ATTRIBUTES vectorcall :: c16_eq_c1 !GCC$ ATTRIBUTES inline :: c16_eq_c1
#elif defined __ICC || defined __INTEL_COMPILER
        !DIR$ ATTRIBUTES INLINE :: c16_eq_c1
      pure function c16_eq_c1(x,y) result(mmask16)
         !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_eq_c1
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c16_eq_c1
#endif
        use mod_avx512_bindings, only : v16f32, v16f32_cmp_ps_mask
        type(ZMM16c4),     intent(in) :: x
        complex(kind=sp),         intent(in) :: y
        integer(c_short), dimension(0:1) :: mmask16
#if defined __INTEL_COMPILER 
        
        !DIR$ ATTRIBUTES ALIGN : 64 :: lre,lim,rre,rim
        type(v16f32), automatic :: lre,lim,rre,rim
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        
        type(v16f32), automatic :: lre !GCC$ ATTRIBUTES aligned(64) :: lre
        type(v16f32), automatic :: lim !GCC$ ATTRIBUTES aligned(64) :: lim
        type(v16f32), automatic :: rre !GCC$ ATTRIBUTES aligned(64) :: rre
        type(v16f32), automatic :: rim !GCC$ ATTRIBUTES aligned(64) :: rim
        
#endif
        mmask16 = 0
        lre.zmm = x.re
        rre.zmm = real(y,kind=sp)
        mmask16(0) = v16f32_cmp_ps_mask(lre,rre,0)
        lim.zmm = x.im
        rim.zmm = aimag(y,kind=sp)
        mmask16(1) = v16f32_cmp_ps_mask(lim,rim,0)
      end function c16_eq_c1
        
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      pure function c1_eq_c16(x,y) result(mmask16) !GCC$ ATTRIBUTES hot :: c1_eq_c16 !GCC$ ATTRIBUTES vectorcall :: c1_eq_c16 !GCC$ ATTRIBUTES inline :: c1_eq_c16
#elif defined __ICC || defined __INTEL_COMPILER
         !DIR$ ATTRIBUTES INLINE :: c1_eq_c16
      pure function c1_eq_c16(x,y) result(mmask16)
          !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c1_eq_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c1_eq_c16
#endif
        use mod_avx512_bindings, only : v16f32, v16f32_cmp_ps_mask
        complex(kind=sp),     intent(in) :: x
        type(ZMM16c4), intent(in) :: y
        integer(c_short), dimension(0:1) :: mmask16
#if defined __INTEL_COMPILER 
        
        !DIR$ ATTRIBUTES ALIGN : 64 :: lre,lim,rre,rim
        type(v16f32), automatic :: lre,lim,rre,rim
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        
        type(v16f32), automatic :: lre !GCC$ ATTRIBUTES aligned(64) :: lre
        type(v16f32), automatic :: lim !GCC$ ATTRIBUTES aligned(64) :: lim
        type(v16f32), automatic :: rre !GCC$ ATTRIBUTES aligned(64) :: rre
        type(v16f32), automatic :: rim !GCC$ ATTRIBUTES aligned(64) :: rim
        
#endif
        mmask16 = 0
        lre.zmm = real(x,kind=sp)
        rre.zmm = y.re
        mmask16(0) = v16f32_cmp_ps_mask(lre,rre,0)
        lim.zmm = aimag(x,kind=sp)
        rim.zmm = y.im
        mmask16(1) = v16f32_cmp_ps_mask(lim,rim,0)
      end function c1_eq_c16

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      pure function c16_neq_c16(x,y) result(mmask16) !GCC$ ATTRIBUTES hot :: c16_neq_c16 !GCC$ ATTRIBUTES vectorcall :: c16_neq_c16 !GCC$ ATTRIBUTES inline :: c16_neq_c16
#elif defined __ICC || defined __INTEL_COMPILER
      pure function c16_neq_c16(x,y) result(mmask16)
         !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_neq_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c16_neq_c16
        use mod_avx512_bindings, only : v16f32, v16f32_cmp_ps_mask
#endif
        type(ZMM16c4),      intent(in) :: x
        type(ZMM16c4),      intent(in) :: y
        integer(c_short), dimension(0:1) :: mmask16
#if defined __INTEL_COMPILER 
        
        !DIR$ ATTRIBUTES ALIGN : 64 :: lre,lim,rre,rim
        type(v16f32), automatic :: lre,lim,rre,rim
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        
        type(v16f32), automatic :: lre !GCC$ ATTRIBUTES aligned(64) :: lre
        type(v16f32), automatic :: lim !GCC$ ATTRIBUTES aligned(64) :: lim
        type(v16f32), automatic :: rre !GCC$ ATTRIBUTES aligned(64) :: rre
        type(v16f32), automatic :: rim !GCC$ ATTRIBUTES aligned(64) :: rim
        
#endif
        mmask16 = 0
        lre.zmm = x.re
        rre.zmm = y.re
        mmask16(0) = v16f32_cmp_ps_mask(lre,rre,12)
        lim.zmm = x.im
        rim.zmm = y.im
        mmask16(1) = v16f32_cmp_ps_mask(lim,rim,12)
      end function c16_neq_c16

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      pure function c16_neq_c1(x,y) result(mmask16) !GCC$ ATTRIBUTES hot :: c16_neq_c1 !GCC$ ATTRIBUTES vectorcall :: c16_neq_c1 !GCC$ ATTRIBUTES inline :: c16_neq_c1
#elif defined __ICC || defined __INTEL_COMPILER
        !DIR$ ATTRIBUTES INLINE :: c16_neq_c1
      pure function c16_neq_c1(x,y) result(mmask16)
         !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_neq_c1
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c16_neq_c1
#endif
        use mod_avx512_bindings, only : v16f32, v16f32_cmp_ps_mask
        type(ZMM16c4),     intent(in) :: x
        complex(kind=sp),         intent(in) :: y
        integer(c_short), dimension(0:1) :: mmask16
#if defined __INTEL_COMPILER 
        
        !DIR$ ATTRIBUTES ALIGN : 64 :: lre,lim,rre,rim
        type(v16f32), automatic :: lre,lim,rre,rim
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        
        type(v16f32), automatic :: lre !GCC$ ATTRIBUTES aligned(64) :: lre
        type(v16f32), automatic :: lim !GCC$ ATTRIBUTES aligned(64) :: lim
        type(v16f32), automatic :: rre !GCC$ ATTRIBUTES aligned(64) :: rre
        type(v16f32), automatic :: rim !GCC$ ATTRIBUTES aligned(64) :: rim
        
#endif
        mmask16 = 0
        lre.zmm = x.re
        rre.zmm = real(y,kind=sp)
        mmask16(0) = v16f32_cmp_ps_mask(lre,rre,12)
        lim.zmm = x.im
        rim.zmm = aimag(y,kind=sp)
        mmask16(1) = v16f32_cmp_ps_mask(lim,rim,12)
      end function c16_neq_c1
        
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      pure function c1_neq_c16(x,y) result(mmask16) !GCC$ ATTRIBUTES hot :: c1_neq_c16 !GCC$ ATTRIBUTES vectorcall :: c1_neq_c16 !GCC$ ATTRIBUTES inline :: c1_neq_c16
#elif defined __ICC || defined __INTEL_COMPILER
         !DIR$ ATTRIBUTES INLINE :: c1_neq_c16
      pure function c1_neq_c16(x,y) result(mmask16)
          !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c1_neq_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c1_neq_c16
#endif
        use mod_avx512_bindings, only : v16f32, v16f32_cmp_ps_mask
        complex(kind=sp),     intent(in) :: x
        type(ZMM16c4), intent(in) :: y
        integer(c_short), dimension(0:1) :: mmask16
#if defined __INTEL_COMPILER 
        
        !DIR$ ATTRIBUTES ALIGN : 64 :: lre,lim,rre,rim
        type(v16f32), automatic :: lre,lim,rre,rim
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        
        type(v16f32), automatic :: lre !GCC$ ATTRIBUTES aligned(64) :: lre
        type(v16f32), automatic :: lim !GCC$ ATTRIBUTES aligned(64) :: lim
        type(v16f32), automatic :: rre !GCC$ ATTRIBUTES aligned(64) :: rre
        type(v16f32), automatic :: rim !GCC$ ATTRIBUTES aligned(64) :: rim
        
#endif
        mmask16 = 0
        lre.zmm = real(x,kind=sp)
        rre.zmm = y.re
        mmask16(0) = v16f32_cmp_ps_mask(lre,rre,12)
        lim.zmm = aimag(x,kind=sp)
        rim.zmm = y.im
        mmask16(1) = v16f32_cmp_ps_mask(lim,rim,12)
      end function c1_neq_c16

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      pure function c16_gt_c16(x,y) result(mmask16) !GCC$ ATTRIBUTES hot :: c16_gt_c16 !GCC$ ATTRIBUTES vectorcall :: c16_gt_c16 !GCC$ ATTRIBUTES inline :: c16_gt_c16
#elif defined __ICC || defined __INTEL_COMPILER
      pure function c16_gt_c16(x,y) result(mmask16)
         !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_gt_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c16_gt_c16
        use mod_avx512_bindings, only : v16f32, v16f32_cmp_ps_mask
#endif
        type(ZMM16c4),      intent(in) :: x
        type(ZMM16c4),      intent(in) :: y
        integer(c_short), dimension(0:1) :: mmask16
#if defined __INTEL_COMPILER 
        
        !DIR$ ATTRIBUTES ALIGN : 64 :: lre,lim,rre,rim
        type(v16f32), automatic :: lre,lim,rre,rim
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        
        type(v16f32), automatic :: lre !GCC$ ATTRIBUTES aligned(64) :: lre
        type(v16f32), automatic :: lim !GCC$ ATTRIBUTES aligned(64) :: lim
        type(v16f32), automatic :: rre !GCC$ ATTRIBUTES aligned(64) :: rre
        type(v16f32), automatic :: rim !GCC$ ATTRIBUTES aligned(64) :: rim
        
#endif
        mmask16 = 0
        lre.zmm = x.re
        rre.zmm = y.re
        mmask16(0) = v16f32_cmp_ps_mask(lre,rre,30)
        lim.zmm = x.im
        rim.zmm = y.im
        mmask16(1) = v16f32_cmp_ps_mask(lim,rim,30)
      end function c16_gt_c16

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      pure function c16_gt_c1(x,y) result(mmask16) !GCC$ ATTRIBUTES hot :: c16_gt_c1 !GCC$ ATTRIBUTES vectorcall :: c16_gt_c1 !GCC$ ATTRIBUTES inline :: c16_gt_c1
#elif defined __ICC || defined __INTEL_COMPILER
        !DIR$ ATTRIBUTES INLINE :: c16_gt_c1
      pure function c16_gt_c1(x,y) result(mmask16)
         !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_gt_c1
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c16_gt_c1
#endif
        use mod_avx512_bindings, only : v16f32, v16f32_cmp_ps_mask
        type(ZMM16c4),     intent(in) :: x
        complex(kind=sp),         intent(in) :: y
        integer(c_short), dimension(0:1) :: mmask16
#if defined __INTEL_COMPILER 
        
        !DIR$ ATTRIBUTES ALIGN : 64 :: lre,lim,rre,rim
        type(v16f32), automatic :: lre,lim,rre,rim
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        
        type(v16f32), automatic :: lre !GCC$ ATTRIBUTES aligned(64) :: lre
        type(v16f32), automatic :: lim !GCC$ ATTRIBUTES aligned(64) :: lim
        type(v16f32), automatic :: rre !GCC$ ATTRIBUTES aligned(64) :: rre
        type(v16f32), automatic :: rim !GCC$ ATTRIBUTES aligned(64) :: rim
        
#endif
        mmask16 = 0
        lre.zmm = x.re
        rre.zmm = real(y,kind=sp)
        mmask16(0) = v16f32_cmp_ps_mask(lre,rre,30)
        lim.zmm = x.im
        rim.zmm = aimag(y,kind=sp)
        mmask16(1) = v16f32_cmp_ps_mask(lim,rim,30)
      end function c16_gt_c1
        
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      pure function c1_gt_c16(x,y) result(mmask16) !GCC$ ATTRIBUTES hot :: c1_gt_c16 !GCC$ ATTRIBUTES vectorcall :: c1_gt_c16 !GCC$ ATTRIBUTES inline :: c1_gt_c16
#elif defined __ICC || defined __INTEL_COMPILER
         !DIR$ ATTRIBUTES INLINE :: c1_gt_c16
      pure function c1_gt_c16(x,y) result(mmask16)
          !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c1_gt_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c1_gt_c16
#endif
        use mod_avx512_bindings, only : v16f32, v16f32_cmp_ps_mask
        complex(kind=sp),     intent(in) :: x
        type(ZMM16c4), intent(in) :: y
        integer(c_short), dimension(0:1) :: mmask16
#if defined __INTEL_COMPILER 
        
        !DIR$ ATTRIBUTES ALIGN : 64 :: lre,lim,rre,rim
        type(v16f32), automatic :: lre,lim,rre,rim
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        
        type(v16f32), automatic :: lre !GCC$ ATTRIBUTES aligned(64) :: lre
        type(v16f32), automatic :: lim !GCC$ ATTRIBUTES aligned(64) :: lim
        type(v16f32), automatic :: rre !GCC$ ATTRIBUTES aligned(64) :: rre
        type(v16f32), automatic :: rim !GCC$ ATTRIBUTES aligned(64) :: rim
        
#endif
        mmask16 = 0
        lre.zmm = real(x,kind=sp)
        rre.zmm = y.re
        mmask16(0) = v16f32_cmp_ps_mask(lre,rre,30)
        lim.zmm = aimag(x,kind=sp)
        rim.zmm = y.im
        mmask16(1) = v16f32_cmp_ps_mask(lim,rim,30)
      end function c1_gt_c16
      
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      pure function c16_lt_c16(x,y) result(mmask16) !GCC$ ATTRIBUTES hot :: c16_lt_c16 !GCC$ ATTRIBUTES vectorcall :: c16_lt_c16 !GCC$ ATTRIBUTES inline :: c16_lt_c16
#elif defined __ICC || defined __INTEL_COMPILER
      pure function c16_lt_c16(x,y) result(mmask16)
         !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_lt_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c16_lt_c16
        use mod_avx512_bindings, only : v16f32, v16f32_cmp_ps_mask
#endif
        type(ZMM16c4),      intent(in) :: x
        type(ZMM16c4),      intent(in) :: y
        integer(c_short), dimension(0:1) :: mmask16
#if defined __INTEL_COMPILER 
        
        !DIR$ ATTRIBUTES ALIGN : 64 :: lre,lim,rre,rim
        type(v16f32), automatic :: lre,lim,rre,rim
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        
        type(v16f32), automatic :: lre !GCC$ ATTRIBUTES aligned(64) :: lre
        type(v16f32), automatic :: lim !GCC$ ATTRIBUTES aligned(64) :: lim
        type(v16f32), automatic :: rre !GCC$ ATTRIBUTES aligned(64) :: rre
        type(v16f32), automatic :: rim !GCC$ ATTRIBUTES aligned(64) :: rim
        
#endif
        mmask16 = 0
        lre.zmm = x.re
        rre.zmm = y.re
        mmask16(0) = v16f32_cmp_ps_mask(lre,rre,17)
        lim.zmm = x.im
        rim.zmm = y.im
        mmask16(1) = v16f32_cmp_ps_mask(lim,rim,17)
      end function c16_lt_c16

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      pure function c16_lt_c1(x,y) result(mmask16) !GCC$ ATTRIBUTES hot :: c16_lt_c1 !GCC$ ATTRIBUTES vectorcall :: c16_lt_c1 !GCC$ ATTRIBUTES inline :: c16_lt_c1
#elif defined __ICC || defined __INTEL_COMPILER
        !DIR$ ATTRIBUTES INLINE :: c16_lt_c1
      pure function c16_lt_c1(x,y) result(mmask16)
         !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_lt_c1
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c16_lt_c1
#endif
        use mod_avx512_bindings, only : v16f32, v16f32_cmp_ps_mask
        type(ZMM16c4),     intent(in) :: x
        complex(kind=sp),         intent(in) :: y
        integer(c_short), dimension(0:1) :: mmask16
#if defined __INTEL_COMPILER 
        
        !DIR$ ATTRIBUTES ALIGN : 64 :: lre,lim,rre,rim
        type(v16f32), automatic :: lre,lim,rre,rim
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        
        type(v16f32), automatic :: lre !GCC$ ATTRIBUTES aligned(64) :: lre
        type(v16f32), automatic :: lim !GCC$ ATTRIBUTES aligned(64) :: lim
        type(v16f32), automatic :: rre !GCC$ ATTRIBUTES aligned(64) :: rre
        type(v16f32), automatic :: rim !GCC$ ATTRIBUTES aligned(64) :: rim
        
#endif
        mmask16 = 0
        lre.zmm = x.re
        rre.zmm = real(y,kind=sp)
        mmask16(0) = v16f32_cmp_ps_mask(lre,rre,17)
        lim.zmm = x.im
        rim.zmm = aimag(y,kind=sp)
        mmask16(1) = v16f32_cmp_ps_mask(lim,rim,17)
      end function c16_lt_c1
        
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      pure function c1_lt_c16(x,y) result(mmask16) !GCC$ ATTRIBUTES hot :: c1_lt_c16 !GCC$ ATTRIBUTES vectorcall :: c1_lt_c16 !GCC$ ATTRIBUTES inline :: c1_lt_c16
#elif defined __ICC || defined __INTEL_COMPILER
         !DIR$ ATTRIBUTES INLINE :: c1_eq_c16
      pure function c1_lt_c16(x,y) result(mmask16)
          !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c1_lt_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c1_lt_c16
#endif
        use mod_avx512_bindings, only : v16f32, v16f32_cmp_ps_mask
        complex(kind=sp),     intent(in) :: x
        type(ZMM16c4), intent(in) :: y
        integer(c_short), dimension(0:1) :: mmask16
#if defined __INTEL_COMPILER 
        
        !DIR$ ATTRIBUTES ALIGN : 64 :: lre,lim,rre,rim
        type(v16f32), automatic :: lre,lim,rre,rim
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        
        type(v16f32), automatic :: lre !GCC$ ATTRIBUTES aligned(64) :: lre
        type(v16f32), automatic :: lim !GCC$ ATTRIBUTES aligned(64) :: lim
        type(v16f32), automatic :: rre !GCC$ ATTRIBUTES aligned(64) :: rre
        type(v16f32), automatic :: rim !GCC$ ATTRIBUTES aligned(64) :: rim
        
#endif
        mmask16 = 0
        lre.zmm = real(x,kind=sp)
        rre.zmm = y.re
        mmask16(0) = v16f32_cmp_ps_mask(lre,rre,17)
        lim.zmm = aimag(x,kind=sp)
        rim.zmm = y.im
        mmask16(1) = v16f32_cmp_ps_mask(lim,rim,17)
      end function c1_lt_c16

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      pure function c16_ge_c16(x,y) result(mmask16) !GCC$ ATTRIBUTES hot :: c16_ge_c16 !GCC$ ATTRIBUTES vectorcall :: c16_ge_c16 !GCC$ ATTRIBUTES inline :: c16_ge_c16
#elif defined __ICC || defined __INTEL_COMPILER
      pure function c16_ge_c16(x,y) result(mmask16)
         !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_ge_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c16_ge_c16
        use mod_avx512_bindings, only : v16f32, v16f32_cmp_ps_mask
#endif
        type(ZMM16c4),      intent(in) :: x
        type(ZMM16c4),      intent(in) :: y
        integer(c_short), dimension(0:1) :: mmask16
#if defined __INTEL_COMPILER 
        
        !DIR$ ATTRIBUTES ALIGN : 64 :: lre,lim,rre,rim
        type(v16f32), automatic :: lre,lim,rre,rim
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        
        type(v16f32), automatic :: lre !GCC$ ATTRIBUTES aligned(64) :: lre
        type(v16f32), automatic :: lim !GCC$ ATTRIBUTES aligned(64) :: lim
        type(v16f32), automatic :: rre !GCC$ ATTRIBUTES aligned(64) :: rre
        type(v16f32), automatic :: rim !GCC$ ATTRIBUTES aligned(64) :: rim
        
#endif
        mmask16 = 0
        lre.zmm = x.re
        rre.zmm = y.re
        mmask16(0) = v16f32_cmp_ps_mask(lre,rre,29)
        lim.zmm = x.im
        rim.zmm = y.im
        mmask16(1) = v16f32_cmp_ps_mask(lim,rim,29)
      end function c16_ge_c16

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      pure function c16_ge_c1(x,y) result(mmask16) !GCC$ ATTRIBUTES hot :: c16_ge_c1 !GCC$ ATTRIBUTES vectorcall :: c16_ge_c1 !GCC$ ATTRIBUTES inline :: c16_ge_c1
#elif defined __ICC || defined __INTEL_COMPILER
        !DIR$ ATTRIBUTES INLINE :: c16_ge_c1
      pure function c16_ge_c1(x,y) result(mmask16)
         !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_ge_c1
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c16_ge_c1
#endif
        use mod_avx512_bindings, only : v16f32, v16f32_cmp_ps_mask
        type(ZMM16c4),     intent(in) :: x
        complex(kind=sp),         intent(in) :: y
        integer(c_short), dimension(0:1) :: mmask16
#if defined __INTEL_COMPILER 
        
        !DIR$ ATTRIBUTES ALIGN : 64 :: lre,lim,rre,rim
        type(v16f32), automatic :: lre,lim,rre,rim
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        
        type(v16f32), automatic :: lre !GCC$ ATTRIBUTES aligned(64) :: lre
        type(v16f32), automatic :: lim !GCC$ ATTRIBUTES aligned(64) :: lim
        type(v16f32), automatic :: rre !GCC$ ATTRIBUTES aligned(64) :: rre
        type(v16f32), automatic :: rim !GCC$ ATTRIBUTES aligned(64) :: rim
        
#endif
        mmask16 = 0
        lre.zmm = x.re
        rre.zmm = real(y,kind=sp)
        mmask16(0) = v16f32_cmp_ps_mask(lre,rre,29)
        lim.zmm = x.im
        rim.zmm = aimag(y,kind=sp)
        mmask16(1) = v16f32_cmp_ps_mask(lim,rim,29)
      end function c16_ge_c1
        
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      pure function c1_ge_c16(x,y) result(mmask16) !GCC$ ATTRIBUTES hot :: c1_ge_c16 !GCC$ ATTRIBUTES vectorcall :: c1_ge_c16 !GCC$ ATTRIBUTES inline :: c1_ge_c16
#elif defined __ICC || defined __INTEL_COMPILER
         !DIR$ ATTRIBUTES INLINE :: c1_ge_c16
      pure function c1_ge_c16(x,y) result(mmask16)
          !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c1_ge_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c1_ge_c16
#endif
        use mod_avx512_bindings, only : v16f32, v16f32_cmp_ps_mask
        complex(kind=sp),     intent(in) :: x
        type(ZMM16c4), intent(in) :: y
        integer(c_short), dimension(0:1) :: mmask16
#if defined __INTEL_COMPILER 
        
        !DIR$ ATTRIBUTES ALIGN : 64 :: lre,lim,rre,rim
        type(v16f32), automatic :: lre,lim,rre,rim
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        
        type(v16f32), automatic :: lre !GCC$ ATTRIBUTES aligned(64) :: lre
        type(v16f32), automatic :: lim !GCC$ ATTRIBUTES aligned(64) :: lim
        type(v16f32), automatic :: rre !GCC$ ATTRIBUTES aligned(64) :: rre
        type(v16f32), automatic :: rim !GCC$ ATTRIBUTES aligned(64) :: rim
        
#endif
        mmask16 = 0
        lre.zmm = real(x,kind=sp)
        rre.zmm = y.re
        mmask16(0) = v16f32_cmp_ps_mask(lre,rre,29)
        lim.zmm = aimag(x,kind=sp)
        rim.zmm = y.im
        mmask16(1) = v16f32_cmp_ps_mask(lim,rim,29)
      end function c1_ge_c16
      
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      pure function c16_le_c16(x,y) result(mmask16) !GCC$ ATTRIBUTES hot :: c16_le_c16 !GCC$ ATTRIBUTES vectorcall :: c16_le_c16 !GCC$ ATTRIBUTES inline :: c16_le_c16
#elif defined __ICC || defined __INTEL_COMPILER
      pure function c16_le_c16(x,y) result(mmask16)
         !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_le_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c16_le_c16
        use mod_avx512_bindings, only : v16f32, v16f32_cmp_ps_mask
#endif
        type(ZMM16c4),      intent(in) :: x
        type(ZMM16c4),      intent(in) :: y
        integer(c_short), dimension(0:1) :: mmask16
#if defined __INTEL_COMPILER 
        
        !DIR$ ATTRIBUTES ALIGN : 64 :: lre,lim,rre,rim
        type(v16f32), automatic :: lre,lim,rre,rim
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        
        type(v16f32), automatic :: lre !GCC$ ATTRIBUTES aligned(64) :: lre
        type(v16f32), automatic :: lim !GCC$ ATTRIBUTES aligned(64) :: lim
        type(v16f32), automatic :: rre !GCC$ ATTRIBUTES aligned(64) :: rre
        type(v16f32), automatic :: rim !GCC$ ATTRIBUTES aligned(64) :: rim
        
#endif
        mmask16 = 0
        lre.zmm = x.re
        rre.zmm = y.re
        mmask16(0) = v16f32_cmp_ps_mask(lre,rre,18)
        lim.zmm = x.im
        rim.zmm = y.im
        mmask16(1) = v16f32_cmp_ps_mask(lim,rim,18)
      end function c16_le_c16

#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      pure function c16_le_c1(x,y) result(mmask16) !GCC$ ATTRIBUTES hot :: c16_le_c1 !GCC$ ATTRIBUTES vectorcall :: c16_le_c1 !GCC$ ATTRIBUTES inline :: c16_le_c1
#elif defined __ICC || defined __INTEL_COMPILER
        !DIR$ ATTRIBUTES INLINE :: c16_le_c1
      pure function c16_le_c1(x,y) result(mmask16)
         !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_le_c1
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c16_le_c1
#endif
        use mod_avx512_bindings, only : v16f32, v16f32_cmp_ps_mask
        type(ZMM16c4),     intent(in) :: x
        complex(kind=sp),         intent(in) :: y
        integer(c_short), dimension(0:1) :: mmask16
#if defined __INTEL_COMPILER 
        
        !DIR$ ATTRIBUTES ALIGN : 64 :: lre,lim,rre,rim
        type(v16f32), automatic :: lre,lim,rre,rim
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        
        type(v16f32), automatic :: lre !GCC$ ATTRIBUTES aligned(64) :: lre
        type(v16f32), automatic :: lim !GCC$ ATTRIBUTES aligned(64) :: lim
        type(v16f32), automatic :: rre !GCC$ ATTRIBUTES aligned(64) :: rre
        type(v16f32), automatic :: rim !GCC$ ATTRIBUTES aligned(64) :: rim
        
#endif
        mmask16 = 0
        lre.zmm = x.re
        rre.zmm = real(y,kind=sp)
        mmask16(0) = v16f32_cmp_ps_mask(lre,rre,18)
        lim.zmm = x.im
        rim.zmm = aimag(y,kind=sp)
        mmask16(1) = v16f32_cmp_ps_mask(lim,rim,18)
      end function c16_le_c1
        
#if defined __GFORTRAN__ && !defined __INTEL_COMPILER
      pure function c1_le_c16(x,y) result(mmask16) !GCC$ ATTRIBUTES hot :: c1_le_c16 !GCC$ ATTRIBUTES vectorcall :: c1_le_c16 !GCC$ ATTRIBUTES inline :: c1_le_c16
#elif defined __ICC || defined __INTEL_COMPILER
         !DIR$ ATTRIBUTES INLINE :: c1_le_c16
      pure function c1_le_c16(x,y) result(mmask16)
          !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c1_le_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c1_le_c16
#endif
        use mod_avx512_bindings, only : v16f32, v16f32_cmp_ps_mask
        complex(kind=sp),     intent(in) :: x
        type(ZMM16c4), intent(in) :: y
        integer(c_short), dimension(0:1) :: mmask16
#if defined __INTEL_COMPILER 
        
        !DIR$ ATTRIBUTES ALIGN : 64 :: lre,lim,rre,rim
        type(v16f32), automatic :: lre,lim,rre,rim
#elif defined __GFORTRAN__ && !defined __INTEL_COMPILER
        
        type(v16f32), automatic :: lre !GCC$ ATTRIBUTES aligned(64) :: lre
        type(v16f32), automatic :: lim !GCC$ ATTRIBUTES aligned(64) :: lim
        type(v16f32), automatic :: rre !GCC$ ATTRIBUTES aligned(64) :: rre
        type(v16f32), automatic :: rim !GCC$ ATTRIBUTES aligned(64) :: rim
        
#endif
        mmask16 = 0
        lre.zmm = real(x,kind=sp)
        rre.zmm = y.re
        mmask16(0) = v16f32_cmp_ps_mask(lre,rre,18)
        lim.zmm = aimag(x,kind=sp)
        rim.zmm = y.im
        mmask16(1) = v16f32_cmp_ps_mask(lim,rim,18)
      end function c1_le_c16
      

#endif ! if 0


      
      pure function c16_eq_c16(x,y) result(bres)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: c16_eq_c16
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_eq_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c16_eq_c16
        type(ZMM16c4),   intent(in) :: x
        type(ZMM16c4),   intent(in) :: y
        !DIR$ ATTRIBUTES ALIGN : 64 :: mre,mim
        logical(kind=int4), dimension(0:15) :: mre,mim
        logical(kind=int1), dimension(0:1) :: bres
        mre = .false.
        mre = (x.re == y.re)
        mim = .false.
        mim = (x.im == y.im)
        bres = .false.
        bres(0) = all(mre)
        bres(1) = all(mim)
      end function c16_eq_c16


      
      pure function c16_eq_c1(x,y) result(bres)
        !DIR$ OPTIMIZE:3
         !DIR$ ATTRIBUTES INLINE :: c16_eq_c1
         !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_eq_c1
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c16_eq_c1  
        type(ZMM16c4),      intent(in) :: x
        complex(kind=sp),          intent(in) :: y
        !DIR$ ATTRIBUTES ALIGN : 64 :: mre,mim
        logical(kind=int4), dimension(0:15) :: mre,mim
        logical(kind=int1), dimension(0:1) :: bres
        mre  = .false.
        mim  = .false.
        bres = .false.
        mre = x.re == real(y,kind=sp)
        bres(0) = all(mre)
        mim = x.im == aimag(y,kind=sp)
        bres(1) = all(mim)
      end function c16_eq_c1


       
      pure function c1_eq_c16(x,y) result(bres)
         !DIR$ OPTIMIZE:3
         !DIR$ ATTRIBUTES INLINE :: c1_eq_c16
          !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c1_eq_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c1_eq_c16
        complex(kind=sp),      intent(in) :: x
        type(ZMM16c4),  intent(in) :: y
        !DIR$ ATTRIBUTES ALIGN : 64 :: mre,mim
        logical(kind=int4), dimension(0:15) :: mre,mim
        mre  = .false.
        mim  = .false.
        bres = .false.
        mre = real(x,kind=sp)  == y.re
        bres(0) = all(mre)
        mim = aimag(x,kind=sp) == y.im
        bres(1) = all(mim)
     end function c1_eq_c16


       
     pure function c16_eq_c16(x,y) result(bres)
        !DIR$ OPTIMIZE:3
         !DIR$ ATTRIBUTES INLINE :: c16_neq_c16
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_neq_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c16_neq_c16
        type(ZMM16c4),   intent(in) :: x
        type(ZMM16c4),   intent(in) :: y
        !DIR$ ATTRIBUTES ALIGN : 64 :: mre,mim
        logical(kind=int4), dimension(0:15) :: mre,mim
        logical(kind=int1), dimension(0:1) :: bres
        mre = .false.
        mre = (x.re /= y.re)
        mim = .false.
        mim = (x.im /= y.im)
        bres = .false.
        bres(0) = all(mre)
        bres(1) = all(mim)
      end function c16_neq_c16


       
      pure function c16_eq_c1(x,y) result(bres)
         !DIR$ OPTIMIZE:3
         !DIR$ ATTRIBUTES INLINE :: c16_neq_c1
          !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_neq_c1
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c16_neq_c1  

        type(ZMM16c4),      intent(in) :: x
        complex(kind=sp),          intent(in) :: y
        !DIR$ ATTRIBUTES ALIGN : 64 :: mre,mim
        logical(kind=int4), dimension(0:15) :: mre,mim
        logical(kind=int1), dimension(0:1) :: bres
        mre  = .false.
        mim  = .false.
        bres = .false.
        mre = (x.re /= real(y,kind=sp))
        bres(0) = all(mre)
        mim = (x.im /= aimag(y,kind=sp))
        bres(1) = all(mim)
      end function c16_neq_c1


      
      pure function c1_eq_c16(x,y) result(bres)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: c1_neq_c16
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c1_neq_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c1_neq_c16

        complex(kind=sp),      intent(in) :: x
        type(ZMM16c4),  intent(in) :: y

        !DIR$ ATTRIBUTES ALIGN : 64 :: mre,mim
        logical(kind=int4), dimension(0:15) :: mre,mim

       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = (real(x,kind=sp)  /= y.re)
       bres(0) = all(mre)
       mim = (aimag(x,kind=sp) /= y.im)
       bres(1) = all(mim)
     end function c1_neq_c16



       
      pure function c16_gt_c16(x,y) result(bres)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: c16_gt_c16
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_gt_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c16_gt_c16

        type(ZMM16c4),   intent(in) :: x
        type(ZMM16c4),   intent(in) :: y

        !DIR$ ATTRIBUTES ALIGN : 64 :: mre,mim
        logical(kind=int4), dimension(0:15) :: mre,mim

        logical(kind=int1), dimension(0:1) :: bres
        mre = .false.
        mre = (x.re > y.re)
        mim = .false.
        mim = (x.im > y.im)
        bres = .false.
        bres(0) = all(mre)
        bres(1) = all(mim)
      end function c16_gt_c16

       
      pure function c16_gt_c1(x,y) result(bres)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: c16_gt_c1
          !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_gt_c1
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c16_gt_c1  

        type(ZMM16c4),      intent(in) :: x
        complex(kind=sp),          intent(in) :: y

        !DIR$ ATTRIBUTES ALIGN : 64 :: mre,mim
        logical(kind=int4), dimension(0:15) :: mre,mim

        logical(kind=int1), dimension(0:1) :: bres
        mre  = .false.
        mim  = .false.
        bres = .false.
        mre = (x.re > real(y,kind=sp))
        bres(0) = all(mre)
        mim = (x.im > aimag(y,kind=sp))
        bres(1) = all(mim)
      end function c16_gt_c1

       
      pure function c1_gt_c16(x,y) result(bres)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: c1_gt_c16
          !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c1_gt_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c1_gt_c16

        complex(kind=sp),      intent(in) :: x
        type(ZMM16c4),  intent(in) :: y

        !DIR$ ATTRIBUTES ALIGN : 64 :: mre,mim
        logical(kind=int4), dimension(0:15) :: mre,mim

       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = (real(x,kind=sp) > y.re)
       bres(0) = all(mre)
       mim = (aimag(x,kind=sp) > y.im)
       bres(1) = all(mim)
     end function c1_gt_c16

        
      pure function c16_lt_c16(x,y) result(bres)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: c16_lt_c16
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_lt_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c16_lt_c16

        type(ZMM16c4),   intent(in) :: x
        type(ZMM16c4),   intent(in) :: y

        !DIR$ ATTRIBUTES ALIGN : 64 :: mre,mim
        logical(kind=int4), dimension(0:15) :: mre,mim

        logical(kind=int1), dimension(0:1) :: bres
        mre = .false.
        mre = (x.re < y.re)
        mim = .false.
        mim = (x.im < y.im)
        bres = .false.
        bres(0) = all(mre)
        bres(1) = all(mim)
      end function c16_lt_c16

        
      pure function c16_lt_c1(x,y) result(bres)
         !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: c16_lt_c1
          !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_lt_c1
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c16_lt_c1  

        type(ZMM16c4),      intent(in) :: x
        complex(kind=sp),          intent(in) :: y

        !DIR$ ATTRIBUTES ALIGN : 64 :: mre,mim
        logical(kind=int4), dimension(0:15) :: mre,mim

        logical(kind=int1), dimension(0:1) :: bres
        mre  = .false.
        mim  = .false.
        bres = .false.
        mre = (x.re < real(y,kind=sp))
        bres(0) = all(mre)
        mim = (x.im < aimag(y,kind=sp))
        bres(1) = all(mim)
      end function c16_lt_c1

       
      pure function c1_lt_c16(x,y) result(bres)
         !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: c1_lt_c16
          !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c1_lt_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c1_lt_c16

        complex(kind=sp),      intent(in) :: x
        type(ZMM16c4),  intent(in) :: y

        !DIR$ ATTRIBUTES ALIGN : 64 :: mre,mim
        logical(kind=int4), dimension(0:15) :: mre,mim

       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = (real(x,kind=sp) < y.re)
       bres(0) = all(mre)
       mim = (aimag(x,kind=sp) < y.im)
       bres(1) = all(mim)
     end function c1_lt_c16


       
      pure function c16_ge_c16(x,y) result(bres)
         !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: c16_ge_c16
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_ge_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c16_ge_c16

        type(ZMM16c4),   intent(in) :: x
        type(ZMM16c4),   intent(in) :: y

        !DIR$ ATTRIBUTES ALIGN : 64 :: mre,mim
        logical(kind=int4), dimension(0:15) :: mre,mim

        logical(kind=int1), dimension(0:1) :: bres
        mre = .false.
        mre = (x.re >= y.re)
        mim = .false.
        mim = (x.im >= y.im)
        bres = .false.
        bres(0) = all(mre)
        bres(1) = all(mim)
      end function c16_ge_c16

        
      pure function c16_ge_c1(x,y) result(bres)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: c16_ge_c1
          !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_ge_c1
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c16_ge_c1  

        type(ZMM16c4),      intent(in) :: x
        complex(kind=sp),          intent(in) :: y

        !DIR$ ATTRIBUTES ALIGN : 64 :: mre,mim
        logical(kind=int4), dimension(0:15) :: mre,mim

        logical(kind=int1), dimension(0:1) :: bres
        mre  = .false.
        mim  = .false.
        bres = .false.
        mre = (x.re >= real(y,kind=sp))
        bres(0) = all(mre)
        mim = (x.im >= aimag(y,kind=sp))
        bres(1) = all(mim)
      end function c16_ge_c1

        
      pure function c1_eq_c16(x,y) result(bres)
         !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: c1_ge_c16
          !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c1_ge_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c1_ge_c16

        complex(kind=sp),      intent(in) :: x
        type(ZMM16c4),  intent(in) :: y

        !DIR$ ATTRIBUTES ALIGN : 64 :: mre,mim
        logical(kind=int4), dimension(0:15) :: mre,mim

       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = (real(x,kind=sp)  >= y.re)
       bres(0) = all(mre)
       mim = (aimag(x,kind=sp) >= y.im)
       bres(1) = all(mim)
     end function c1_eq_c16

       
      pure function c16_le_c16(x,y) result(bres)
         !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: c16_le_c16
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_le_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c16_le_c16

        type(ZMM16c4),   intent(in) :: x
        type(ZMM16c4),   intent(in) :: y

        !DIR$ ATTRIBUTES ALIGN : 64 :: mre,mim
        logical(kind=int4), dimension(0:15) :: mre,mim

        logical(kind=int1), dimension(0:1) :: bres
        mre = .false.
        mre = (x.re <= y.re)
        mim = .false.
        mim = (x.im <= y.im)
        bres = .false.
        bres(0) = all(mre)
        bres(1) = all(mim)
      end function c16_le_c16


       
      pure function c16_le_c1(x,y) result(bres)
        !DIR$ OPTIMIZE:3
          !DIR$ ATTRIBUTES INLINE :: c16_le_c1
          !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c16_le_c1
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c16_le_c1  
        type(ZMM16c4),      intent(in) :: x
        complex(kind=sp),          intent(in) :: y
        !DIR$ ATTRIBUTES ALIGN : 64 :: mre,mim
        logical(kind=int4), dimension(0:15) :: mre,mim
        logical(kind=int1), dimension(0:1) :: bres
        mre  = .false.
        mim  = .false.
        bres = .false.
        mre = (x.re <= real(y,kind=sp))
        bres(0) = all(mre)
        mim = (x.im <= aimag(y,kind=sp))
        bres(1) = all(mim)
      end function c16_le_c1


     
      pure function c1_le_c16(x,y) result(bres)
        !DIR$ OPTIMIZE:3
           !DIR$ ATTRIBUTES INLINE :: c1_le_c16
          !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: c1_le_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c1_le_c16

        complex(kind=sp),      intent(in) :: x
        type(ZMM16c4),  intent(in) :: y

        !DIR$ ATTRIBUTES ALIGN : 64 :: mre,mim
        logical(kind=int4), dimension(0:15) :: mre,mim

       mre  = .false.
       mim  = .false.
       bres = .false.
       mre = (real(x,kind=sp)  <= y.re)
       bres(0) = all(mre)
       mim = (aimag(x,kind=sp) <= y.im)
       bres(1) = all(mim)
     end function c1_le_c16




        
      



      
       pure function polar(rho,theta) result(iq)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: polar
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: polar
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: polar
        type(ZMM16r4_t),     intent(in) :: rho
        type(ZMM16r4_t),     intent(in) :: theta
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4) :: iq
        integer(kind=i4) :: i
         !dir$ loop_count(16)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           iq.re(i) = rho*cos(theta.v(i))
           iq.im(i) = rho*sin(theta.v(i))
        end do
      end function polar

        
      pure function carg_c16(x) result(arg)
         !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: carg_c16
          !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: carg_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: carg_c16
        type(ZMM16c4),  intent(in) :: x
        !DIR$ ATTRIBUTES ALIGN : 64 :: arg
        type(ZMM16r4_t) :: arg
        integer(kind=i4) :: i
          !dir$ loop_count(16)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           arg.v(i) = atan2(x.re(i),x.im(i))
        end do
      end function carg_c16

       
        pure function carg_2xv16(re,im) result(arg)
         !DIR$ OPTIMIZE:3
         !DIR$ ATTRIBUTES INLINE :: carg_2xv16
         !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: carg_2xv16
          !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: carg_2xv16

          type(ZMM16r4_t),     intent(in) :: re
          type(ZMM16r4_t),     intent(in) :: im

          !DIR$ ATTRIBUTES ALIGN : 64 :: arg
          type(ZMM16r4_t) :: arg
          integer(kind=i4) :: i
        !dir$ loop_count(16)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
          do i=0, 15
             arg.v(i) = atan2(re.v(i),im.v(i))
          end do
      end function carg_2xv16
      
 
       pure function cnorm(x) result(cn)
           !DIR$ OPTIMIZE:3
           !DIR$ ATTRIBUTES INLINE :: cnorm
           !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: cnorm
           !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: cnorm

            type(ZMM16c4),  intent(in) :: x
            type(ZMM16r4_t) :: cn
            cn.v = sqrt(x.re*x.re+x.im*x.im)
       end function cnorm

        
        pure function csin_c16(x) result(iq)
          !DIR$ OPTIMIZE:3
          !DIR$ ATTRIBUTES INLINE :: csin_c16
           !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: csin_c16
          !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: csin_c16

          type(ZMM16c4),    intent(in) :: x

          !DIR$ ATTRIBUTES ALIGN : 64 :: iq
          type(ZMM16c4)  :: iq
          integer(kind=i4) :: i
           !dir$ loop_count(16)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
          do i=0, 15
             iq.re(i) = sin(x.re(i))*cosh(x.im(i))
             iq.im(i) = cos(x.re(i))*sinh(x.im(i))
          end do
      end function csin_c16


        
      pure function csin_2xv16(re,im) result(iq)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: csin_2xv16
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: csin_2xv16
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: csin_2xv16

        type(ZMM16r4_t),    intent(in) :: re
        type(ZMM16r4_t),    intent(in) :: im

        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4)  :: iq
        integer(kind=i4) :: i
             !dir$ loop_count(16)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           iq.re(i) = sin(re.v(i))*cosh(im.v(i))
           iq.im(i) = cos(re.v(i))*sinh(im.v(i))
        end do
      end function csin_2xv16


        
      pure function csinh_c16(x) result(iq)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: csinh_c16 
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: csinh_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: csinh_c16
        type(ZMM16c4),    intent(in) :: x
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4)  :: iq
        integer(kind=i4) :: i
              !dir$ loop_count(16)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           iq.re(i) = sinh(x.re(i))*cos(x.im(i))
           iq.im(i) = cosh(x.re(i))*sin(x.im(i))
        end do
      end function csinh_c16


        
      pure function csinh_2xv16(re,im) result(iq)
          !DIR$ OPTIMIZE:3
          !DIR$ ATTRIBUTES INLINE :: csinh_2xv16
          !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: csinh_2xv16
          !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: csinh_2xv16
          type(ZMM16r4_t),     intent(in) :: re
          type(ZMM16r4_t),     intent(in) :: im
          !DIR$ ATTRIBUTES ALIGN : 64 :: iq
          type(ZMM16c4)  :: iq
          integer(kind=i4) :: i
                 !dir$ loop_count(16)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
          do i=0, 15
             iq.re(i) = sinh(re.v(i))*cos(im.v(i))
             iq.im(i) = cosh(re.v(i))*sin(im.v(i))
          end do
      end function csinh_2xv16


       
      pure function ccos_c16(x) result(iq)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: ccos_c16
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: ccos_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ccos_c16
        type(ZMM16c4),   intent(in) :: x
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4)  :: iq
        integer(kind=i4) :: i
        !dir$ loop_count(16)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           iq.re(i) = cos(x.re(i))*cosh(x.im(i))
           iq.im(i) = sin(x.re(i))*sinh(x.im(i))
        end do
      end function ccos_c16


        
      pure function ccos_2xv16(re,im) result(iq)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: ccos_2xv16
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: ccos_2xv16
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ccos_2xv16
        type(ZMM16r4_t),    intent(in) :: re
        type(ZMM16r4_t),    intent(in) :: im
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4)  :: iq
        integer(kind=i4) :: i
        !dir$ loop_count(16)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           iq.re(i) = cos(re.v(i))*cosh(im.v(i))
           iq.im(i) = sin(re.v(i))*sinh(im.v(i))
        end do
      end function ccos_2xv16


       
     pure function ccosh_c16(x) result(iq)
         !DIR$ OPTIMIZE:3
         !DIR$ ATTRIBUTES INLINE :: ccosh_c16
         !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: ccosh_c16
         !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ccosh_c16
         type(ZMM16c4),     intent(in) :: x
         !DIR$ ATTRIBUTES ALIGN : 64 :: iq
         type(ZMM16c4)  :: iq
         integer(kind=i4) :: i
          !dir$ loop_count(16)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
         do i=0, 15
            iq.re(i) = cosh(x.re(i))*cos(x.im(i))
            iq.im(i) = sinh(x.re(i))*sin(x.im(i))
         end do
      end function ccosh_c16


        
      pure function ccos_2xv16(re,im) result(iq)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: ccosh_2xv16
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: ccosh_2xv16
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ccosh_2xv16
        type(ZMM16r4_t),    intent(in) :: re
        type(ZMM16r4_t),    intent(in) :: im
        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4)  :: iq
        integer(kind=i4) :: i
            !dir$ loop_count(16)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           iq.re(i) = cosh(re.v(i))*cos(im.v(i))
           iq.im(i) = sinh(re.v(i))*sin(im.v(i))
        end do
      end function ccos_2xv16


        
      pure function cexp_c16(x) result(iq)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: cexp_c16
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: cexp_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cexp_c16

         type(ZMM16c4),   intent(in) :: x

         !DIR$ ATTRIBUTES ALIGN : 64 :: iq
         type(ZMM16c4)  :: iq
         type(ZMM16r4_t), automatic :: t0
         integer(kind=i4) :: i4
        !dir$ loop_count(16)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
         do i=0, 15
            t0.v(i)  = exp(x.re(i))
            iq.re(i) = t0.v(i)*cos(x.im(i))
            iq.im(i) = t0.v(i)*sin(x.im(i))
         end do
      end function cexp_c16
        

       
      pure function ctan_c16(x) result(iq)
        !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE :: ctan_c16
              !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: ctan_c16
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ctan_c16
        type(ZMM16c4),    intent(in) :: x

        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4)  :: iq
        iq = csin_c16(x)/ccos_c16(x)
      end function ctan_c16

      
      pure function ctan_2xv16(re,im) result(iq)
          !DIR$ OPTIMIZE:3
        !DIR$ ATTRIBUTES INLINE ::  ctan_2xv16
        !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: ctan_2xv16
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ctan_2xv16

        type(ZMM16r4_t),     intent(in) :: re
        type(ZMM16r4_t),     intent(in) :: im

        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4)  :: iq

        iq = csin_2xv16(re,im)/ccos_2xv16(re,im)
      end function ctan_2xv16
        
        
          
        pure function ctanh_c16(x) result(iq)
          !DIR$ OPTIMIZE:3
          !DIR$ ATTRIBUTES INLINE :: ctanh_c16
             !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: ctanh_c16
          !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ctanh_c16

          type(ZMM16c4),     intent(in) :: x

        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4)  :: iq

        iq = csinh_c16(x)/ccosh_c16(x)
      end function ctanh_c16
        

      pure function cexp_2xv16(re,im) result(iq)
           !DIR$ OPTIMIZE:3
          !DIR$ ATTRIBUTES INLINE :: cexp_2xv16
          !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: cexp_2xv16
          !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cexp_2xv16

        type(ZMM16r4_t),     intent(in) :: re
        type(ZMM16r4_t),     intent(in) :: im

        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4)  :: iq
        type(ZMM16r4_t), automatic :: t0
        integer(kind=i4) :: i
        !dir$ loop_count(16)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           t0.v(i)  = exp(re.v(i))
           iq.re(i) = t0.v(i)*cos(im.v(i))
           iq.im(i) = t0.v(i)*sin(im.v(i))
        end do
      end function cexp_2xv16
        

        
     pure function cabs_c16(x) result(val)
          !DIR$ OPTIMIZE:3
          !DIR$ ATTRIBUTES INLINE :: cabs_c16
         !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: cabs_c16
          !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: cabs_c16
          type(ZMM16c4),  intent(in) :: x
        !DIR$ ATTRIBUTES ALIGN : 64 :: val
        type(ZMM16r4_t)  :: val
          val.v = sqrt(x.re*x.re+x.im*x.im)
      end function cabs_c16


        
        pure function cabs_2xv16(re,im) result(val)
          !DIR$ OPTIMIZE:3
          !DIR$ ATTRIBUTES INLINE :: cabs_2xv16
          !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: cabs_2xv16
          !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: cabs_2xv16

          type(ZMM16r4_t),    intent(in) :: re
          type(ZMM16r4_t),    intent(in) :: im

          !DIR$ ATTRIBUTES ALIGN : 64 :: val
          type(ZMM16r4_t)  :: val

          val.v = sqrt(re*re+im*im)
      end function cabs_2xv16


        
      pure function cpow_c16(x,n) result(iq)
         !DIR$ OPTIMIZE:3
         !DIR$ ATTRIBUTES INLINE :: cpow_c16
         !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: cpow_c16
         !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cpow_c16

         type(ZMM16c4),    intent(in) :: x
         real(kind=sp),           intent(in) :: n
         !DIR$ ATTRIBUTES ALIGN : 64 :: iq
         type(ZMM16c4)  :: iq
         !DIR$ ATTRIBUTES ALIGN : 64 :: r,theta,pow,trig
         type(ZMM16r4_t), automatic :: r,theta,pow,trig
         integer(kind=i4) :: i
         r.v = sqrt(x.re*x.re+x.im*x.im)
          !dir$ loop_count(16)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
         do i=0, 15
            pow.v(i)    = r.v(i)**n
            theta.v(i)  = atan(x.im(i)/x.re(i))
            trig.v(i)   = theta.v(i)**n
            iq.re(i)    = pow.v(i)*cos(trig.v(i))
            iq.im(i)    = pow.v(i)*sin(trig.v(i))
         end do
      end function cpow_c16


          
     pure function cpow_2xv16(re,im,n) result(iq)
          !DIR$ OPTIMIZE:3
          !DIR$ ATTRIBUTES INLINE :: cpow_2xv16
             !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: cpow_2xv16
          !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cpow_2xv16

          type(ZMM16r4_t),   intent(in) :: re
          type(ZMM16r4_t),   intent(in) :: im
          real(kind=sp),     intent(in) :: n

          !DIR$ ATTRIBUTES ALIGN : 64 :: iq
          type(ZMM16c4)  :: iq
          !DIR$ ATTRIBUTES ALIGN : 64 :: r,theta,pow,trig
          type(ZMM16r4_t), automatic :: r,theta,pow,trig
          integer(kind=i4) :: i
          r.v = sqrt(re*re+im*im)
        !dir$ loop_count(16)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
          do i=0, 15
             pow.v(i)  = r.v(i)**n
             theta.v(i)= atan(im.v(i)/re.v(i))
             trig.v(i) = theta.v(i)**n
             iq.re(i)  = pow.v(i)*cos(trig.v(i))
             iq.im(i)  = pow.v(i)*sin(trig.v(i))
          end do
      end function cpow_2xv16
        

       
    pure function clog_c16(x) result(iq)
          !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: clog_c16
          !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: clog_c16
          !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: clog_c16

          type(ZMM16c4),  intent(in) :: x

        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4)  :: iq
        !DIR$ ATTRIBUTES ALIGN : 64 :: t0
        type(ZMM16r4_t), automatic :: t0

       t0 = cabs_c16(x)
       iq.re = log(t0.v)
       iq.im = carg_c16(x)
     end function clog_c16


       
       pure function clog_2xv16(re,im) result(iq)
          !DIR$ OPTIMIZE:3
          !DIR$ ATTRIBUTES INLINE :: clog_2xv16
          !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: clog_2xv16
          !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: clog_2xv16

         type(ZMM16r4_t),  intent(in)  :: re
         type(ZMM16r4_t),  intent(in)  :: im

        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4)  :: iq
        !DIR$ ATTRIBUTES ALIGN : 64 :: t0
        type(ZMM16r4_t), automatic :: t0
        integer(kind=i4) :: i
        t0 = cabs_2xv16(re,im)
        !dir$ loop_count(16)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           iq.re(i) = log(t0.v(i))
        end do
        iq.im = carg_2xv16(re,im)
      end function clog_2xv16


      
        pure function csqrt_c16(x) result(iq)
            !DIR$ OPTIMIZE:3
            !DIR$ ATTRIBUTES INLINE :: csqrt_c16
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: csqrt_c16
            !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: csqrt_c16

          use mod_vecconsts, only : v16_1over2
          type(ZMM16c4),     intent(in) :: x
          !DIR$ ATTRIBUTES ALIGN : 64 :: iq
          type(ZMM16c4)  :: iq
          !DIR$ ATTRIBUTES ALIGN : 64 :: t0,t1,t2
          type(ZMM16r4_t), automatic :: t0,t1,t2
          integer(kind=i4) :: i4
          t0 = cabs_c16(x)
          !dir$ loop_count(16)
          !dir$ vector aligned
          !dir$ vector vectorlength(4)
          !dir$ vector always
          do i=0, 15
             t1.v(i)  = v16_1over2.v(i)*(t0.v(i)+c16.re(i))
             iq.re(i) = sqrt(t1.v(i))
             t2.v(i)  = v16_1over2.v(i)*(t0.v(i)-c16.re(i))
             iq.im(i) = sqrt(t2.v(i))
          end do
      end function csqrt_c16


        !DIR$ ATTRIBUTES INLINE :: csqrt_2xv16
        pure function csqrt_2xv16(re,im) result(iq)
            !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: csqrt_2xv16
          !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: csqrt_2xv16

          use mod_vecconsts, only : v16_1over2
          type(ZMM16r4_t),  intent(in) :: re
          type(ZMM16r4_t),  intent(in) :: im

        !DIR$ ATTRIBUTES ALIGN : 64 :: iq
        type(ZMM16c4)  :: iq
        !DIR$ ATTRIBUTES ALIGN : 64 :: t0,t1,t2
        type(ZMM16r4_t), automatic :: t0,t1,t2
        integer(kind=i4) :: i
        t0 = cabs_2xv16(re,im)
        !dir$ loop_count(16)
        !dir$ vector aligned
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=0, 15
           t1.v(i)  = v16_1over2.v(i)*(t0.v(i)+re.v(i))
           iq.re(i) = sqrt(t1.v(i))
           t2.v(i)  = v8_1over2.v(i)*(t0.v(i)-re.v(i))
        end do
      end function csqrt_2xv16


      
      pure function cdiv_smith(x,y) result(iq)
          !DIR$ OPTIMIZE:3
          !DIR$ ATTRIBUTES INLINE :: cdiv_smith
          !DIR$ ATTRIBUTES VECTOR:PROCESSOR(skylake_avx512) :: cdiv_smith
          !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cdiv_smith

#if   0
        use, intrinsic :: ISO_C_BINDING
        use mod_avx512_bindings, only : v16f32, v16f32_cmp_ps_mask
#endif
          type(ZMM16c4),   intent(in) :: x
          type(ZMM16c4),   intent(in) :: y

          !DIR$ ATTRIBUTES ALIGN : 64 :: iq
          type(ZMM16c4)  :: iq
          !DIR$ ATTRIBUTES ALIGN : 64 :: ratio,denom
          type(ZMM16r4_t), automatic :: ratio,denom
          logical(kind=int4), dimension(0:15) :: bres
          integer(kind=i4) :: i
#if  0
        !DIR$ ATTRIBUTES ALIGN : 64 :: tre,tim
        type(v16f32) :: tre,tim
        integer(c_short), automatic :: mask_gte
        integer(c_short), parameter :: all_ones = Z'FFFF'
#endif
#if   0
        tre.zmm = abs(y.re)
        tim.zmm = abs(y.im)
        mask_gte = v16f32_cmp_ps_mask(tre,tim,29);
#endif
         bres = abs(y.re) >= abs(y.im)

         re_part = v16_n0
         im_part = v16_n0
#if   0
        if(mask_gte == all_ones) then
#endif
         if(all(bres)) then
            !dir$ loop_count(16)
            !dir$ vector aligned
            !dir$ vector vectorlength(4)
            !dir$ vector always
            do i=0, 15
               ratio.v(i)   = y.im(i)/y.re(i)
               denom.v(i)   = y.re(i)+(ratio.v(i)*y.im(i))
               iq.re(i)     = (x.re(i)+x.im(i)*ratio.v(i))/denom.v(i)
               iq.im(i)     = (x.im(i)-x.re(i)*ratio.v(i))/denom.v(i)
            end do
        else
            !dir$ loop_count(16)
            !dir$ vector aligned
            !dir$ vector vectorlength(4)
            !dir$ vector always
            do i=0, 15
               ratio.v(i)   = y.re(i)/y.im(i)
               denom.v(i)   = y.im(i)+ratio.v(i)*y.re(i)
               iq.re(i)     = (x.re(i)*ratio.v(i)+x.im(i))/denom.v(i)
               iq.im(i)     = (x.im(i)*ratio.v(i)-x.re(i))/denom.v(i)
            end do
        end if       
      end function cdiv_smith
        
      
end module avx512_cvec16
