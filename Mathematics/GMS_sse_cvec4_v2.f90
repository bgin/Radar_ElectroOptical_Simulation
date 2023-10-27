



!/*MIT License
!Copyright (c) 2020 Bernard Gingold
!Permission is hereby granted, free of charge, to any person obtaining a copy
!of this software and associated documentation files (the "Software"), to deal
!in the Software without restriction, including without limitation the rights
!to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
!copies of the Software, and to permit persons to whom the Software is
!furnished to do so, subject to the following conditions:
!The above copyright notice and this permission notice shall be included in all
!copies or substantial portions of the Software.
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
!FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
!AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
!LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
!OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
!SOFTWARE.
!*/

module sse_cvec4_v2

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'sse_cvec4_v2'
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
     integer(kind=i4),  parameter :: SSE_CVEC4_V2_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: SSE_CVEC4_V2_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: SSE_CVEC4_V2_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: SSE_CVEC4_V2_FULLVER =   &
            1000*SSE_CVEC4_V2_MAJOR+100*SSE_CVEC4_V2_MINOR+10*SSE_CVEC4_V2_MICRO
     ! Module creation date
     character(*),        parameter :: SSE_CVEC4_V2_CREATE_DATE = "27-10-2023 09:33 +00200 (FRI 23 OCT 2023 GMT+2)"
     ! Module build date
     character(*),        parameter :: SSE_CVEC4_V2_BUILD_DATE  = __DATE__ " " __TIME__
     ! Module author info
     character(*),        parameter :: SSE_CVEC4_V2_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: SSE_CVEC4_V2_SYNOPSIS    = "Packed complex vector of 4 elements (complex numbers) -- explicitly vectorized."

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
          integer(kind=i4) :: i
          !dir$ loop_count(4)
          !dir$ vector aligned
          !dir$ vector vectorlength(4)
          !dir$ vector always
          do i=0,3
             iq.re(i) = 0.0_sp
             iq.im(i) = 0.0_sp
          end do
        end function default_init

!DIR$ ATTRIBUTES INLINE :: array_init
     pure   function array_init(re,im) result(iq)
          !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: array_init
          !DIR$ ATTRIBUTES VECTOR :: array_init
          real(kind=sp), dimension(0:3), intent(in) :: re
          real(kind=sp), dimension(0:3), intent(in) :: im
          !DIR$ ATTRIBUTES ALIGN : 16 :: iq
          type(XMM4c4_t) :: iq
          integer(kind=i4) :: i
          !dir$ loop_count(4)
          !dir$ vector aligned
          !dir$ vector vectorlength(4)
          !dir$ vector always
          do i=0,3
             iq.re(i) = re(i)
             iq.im(i) = im(i)
          end do
       end function array_init

!DIR$ ATTRIBUTES INLINE :: complex1_init
     pure   function complex1_init(c) result(iq)
          !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: complex1_init
          complex(kind=sp), intent(in), value :: c
          !DIR$ ATTRIBUTES ALIGN : 16 :: iq
          type(XMM4c4_t) :: iq
          real(kind=sp), automatic :: cr,ci
          integer(kind=i4) :: i
          cr = real(c,kind=sp)
          ci = aimag(c,kind=sp)
          !dir$ loop_count(4)
          !dir$ vector aligned
          !dir$ vector vectorlength(4)
          !dir$ vector always
          do i=0,3
             iq.re(i) = cr
             iq.im(i) = ci
          end do
     end function complex1_init

!DIR$ ATTRIBUTES INLINE :: complex2x4_init
     pure   function complex2x4_init(c) result(iq)
         !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: complex2x4_init
         complex(kind=sp), dimension(0:3), intent(in) :: c
         !DIR$ ATTRIBUTES ALIGN : 16 :: iq
         type(XMM4c4_t) :: iq
         integer(kind=i4) :: i
         !dir$ loop_count(4)
         !dir$ vector aligned
         !dir$ vector vectorlength(4)
         !dir$ vector always
         do i=0,3
            iq.re(i) = real(c(i),kind=sp)
            iq.im(i) = aimag(c(i),kind=sp)
         end do
     end function complex2x4_init

!DIR$ ATTRIBUTES INLINE :: xmm4r42x_init       
     pure  function xmm4r42x_init(v1,v2) result(iq)
         !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: xmm4r42x_init
         !DIR$ ATTRIBUTES VECTOR :: xmm4r42x_init
         type(XMM4r4_t),  intent(in) :: v1
         type(XMM4r4_t),  intent(in) :: v2
         !DIR$ ATTRIBUTES ALIGN : 16 :: iq
         type(XMM4c4_t) :: iq
         integer(kind=i4) :: i
         !dir$ loop_count(4)
         !dir$ vector aligned
         !dir$ vector vectorlength(4)
         !dir$ vector always
         do i=0,3
            iq.re(i) = v1.v(i)
            iq.im(i) = v2.v(i)
         end do
     end function xmm4r42x_init

!DIR$ ATTRIBUTES INLINE :: xmm4r41x_init
     pure function xmm4r41x_init(v1) result(iq)
         !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: xmm4r41x_init
         !DIR$ ATTRIBUTES VECTOR :: xmm4r41x_init
         type(XMM4r4_t),  intent(in) :: v1
         !DIR$ ATTRIBUTES ALIGN : 16 :: iq
         type(XMM4c4_t) :: iq
          integer(kind=i4) :: i
         !dir$ loop_count(4)
         !dir$ vector aligned
         !dir$ vector vectorlength(4)
         !dir$ vector always
         do i=0,3
            iq.re(i) = v1.v(i)
            iq.im(i) = 0.0_sp
         end do
     end function xmm4r41x_init

!DIR$ ATTRIBUTES INLINE :: r41x_init
     pure function r41x_init(s) result(iq)
         !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: r41x_init
         !DIR$ ATTRIBUTES VECTOR :: r41x_init
         real(kind=sp),  intent(in), value :: s
         !DIR$ ATTRIBUTES ALIGN : 16 :: iq
         type(XMM4c4_t) :: iq
          integer(kind=i4) :: i
         !dir$ loop_count(4)
         !dir$ vector aligned
         !dir$ vector vectorlength(4)
         !dir$ vector always
         do i=0,3
            iq.re(i) = s
            iq.im(i) = 0.0_sp
         end do
     end function r41x_init

!DIR$ ATTRIBUTES INLINE ::  copy_init
     pure function copy_init(rhs) result(iq)
         !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: copy_init
         !DIR$ ATTRIBUTES VECTOR :: copy_init
         type(XMM4c4_t),  intent(in) :: rhs
         !DIR$ ATTRIBUTES ALIGN : 16 :: iq
         type(XMM4c4_t) :: iq
         integer(kind=i4) :: i
         !dir$ loop_count(4)
         !dir$ vector aligned
         !dir$ vector vectorlength(4)
         !dir$ vector always
          do i=0,3
             iq.re(i) = rhs.re(i)
             iq.im(i) = rhs.im(i)
          end do
     end function copy_init

!DIR$ ATTRIBUTES INLINE :: xmm4c4_add_xmm4c4
     pure function xmm4c4_add_xmm4c4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: xmm4c4_add_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: xmm4c4_add_xmm4c4
       type(XMM4c4_t),  intent(in) :: lhs
       type(XMM4c4_t),  intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          iq.re(i) = lhs.re(i)+rhs.re(i)
          iq.im(i) = lhs.im(i)+rhs.im(i)
       end do
     end function xmm4c4_add_xmm4c4

!DIR$ ATTRIBUTES INLINE :: xmm4c4_add_c1
     pure function xmm4c4_add_c1(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: xmm4c4_add_c1
       !DIR$ ATTRIBUTES VECTOR :: xmm4c4_add_c1
       type(XMM4c4_t),  intent(in) :: lhs
       complex(kind=sp),intent(in), value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       real(kind=sp), automatic :: cr,ci
       integer(kind=i4) :: i
       cr  = real(rhs,kind=sp)
       ci  = aimag(rhs,kind=sp)
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          iq.re(i) = lhs.re(i)+cr
          iq.im(i) = lhs.im(i)+ci
       end do
    end function xmm4c4_add_c1

!DIR$ ATTRIBUTES INLINE :: xmm4c4_add_xmm4r4
     pure function xmm4c4_add_xmm4r4(lhs,rhs) result(iq)
      
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: xmm4c4_add_xmm4r4
       !DIR$ ATTRIBUTES VECTOR :: xmm4c4_add_xmm4r4
       type(XMM4c4_t),   intent(in) :: lhs
       type(XMM4r4_t),        intent(in),value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          iq.re(i) = lhs.re(i)+rhs.v(i)
          iq.im(i) = 0.0_sp
       end do
     end function xmm4c4_add_xmm4r4

!DIR$ ATTRIBUTES INLINE :: xmm4c4_add_s1
     pure function xmm4c4_add_s1(lhs,rhs) result(iq)
      
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: xmm4c4_add_s1
       !DIR$ ATTRIBUTES VECTOR :: xmm4c4_add_s1
       type(XMM4c4_t),   intent(in) :: lhs
       real(kind=sp),    intent(in), value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          iq.re(i) = lhs.re(i)+rhs
          iq.im(i) = 0.0_sp
       end do
     end function xmm4c4_add_s1

!DIR$ ATTRIBUTES INLINE :: c1_add_xmm4c4     
     pure function c1_add_xmm4c4(lhs,rhs) result(iq)
       ! 
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c1_add_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: c1_add_xmm4c4
       complex(kind=sp),intent(in), value :: lhs
       type(XMM4c4_t),  intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       real(kind=sp), automatic :: cr,ci
       integer(kind=i4) :: i
       cr = real(lhs,kind=sp)
       ci = aimag(lhs,kind=sp)
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          iq.re(i) = cr+rhs.re(i)
          iq.im(i) = ci+rhs.im(i)
       end do
     end function c1_add_xmm4c4

!DIR$ ATTRIBUTES INLINE xmm4r4_add_xmm4c4
     pure function xmm4r4_add_xmm4c4(lhs,rhs) result(iq)
      
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  xmm4r4_add_xmm4c4
       !DIR$ ATTRIBUTES VECTOR ::  xmm4r4_add_xmm4c4
       type(XMM4r4_t),      intent(in) :: lhs
       type(XMM4c4_t), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          iq.re(i) = lhs.v(i)+rhs.re(i)
          iq.im(i) = 0.0_sp
       end do
     end function  xmm4r4_add_xmm4c4

!DIR$ ATTRIBUTES INLINE :: s1_add_xmm4c4
     pure function s1_add_xmm4c4(lhs,rhs) result(iq)
       
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: s1_add_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: s1_add_xmm4c4
       real(kind=sp),  intent(in), value :: lhs
       type(XMM4c4_t), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          iq.re(i) = lhs+rhs.re(i)
          iq.im(i) = 0.0_sp
       end do
     end function s1_add_xmm4c4

!DIR$ ATTRIBUTES INLINE :: xmm4c4_sub_xmm4c4
     pure function xmm4c4_sub_xmm4c4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  xmm4c4_sub_xmm4c4
       !DIR$ ATTRIBUTES VECTOR ::  xmm4c4_sub_xmm4c4
       type(XMM4c4_t),    intent(in) :: lhs
       type(XMM4c4_t),    intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          iq.re(i) = lhs.re(i)-rhs.re(i)
          iq.im(i) = lhs.im(i)-rhs.im(i)
       end do
     end function  xmm4c4_sub_xmm4c4

!DIR$ ATTRIBUTES INLINE :: xmm4c4_sub_c1
     pure function xmm4c4_sub_c1(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: xmm4c4_sub_c1
       !DIR$ ATTRIBUTES VECTOR :: xmm4c4_sub_c1
       type(XMM4c4_t),     intent(in) :: lhs
       complex(kind=sp),   intent(in), value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       real(kind=sp), automatic :: cr,ci
       integer(kind=i4) :: i
       cr = real(rhs,kind=sp)
       ci = aimag(rhs,kind=sp)
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          iq.re(i) = lhs.re(i)-cr
          iq.im(i) = lhs.im(i)-ci
       end do
     end function xmm4c4_sub_c1

!DIR$ ATTRIBUTES INLINE :: xmm4c4_sub_xmm4r4
     pure function xmm4c4_sub_xmm4r4(lhs,rhs) result(iq)
      
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: xmm4c4_sub_xmm4r4
       !DIR$ ATTRIBUTES VECTOR :: xmm4c4_sub_xmm4r4
       type(XMM4c4_t), intent(in) :: lhs
       type(XMM4r4_t), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          iq.re(i) = lhs.re(i)-rhs.v(i)
          iq.im(i) = 0.0_sp
       end do
    end function xmm4c4_sub_xmm4r4

!DIR$ ATTRIBUTES INLINE :: xmm4c4_sub_s1
     pure function xmm4c4_sub_s1(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: xmm4c4_sub_s1
       !DIR$ ATTRTIBUTES VECTOR :: xmm4c4_sub_s1
       type(XMM4c4_t), intent(in) :: lhs
       real(kind=sp),  intent(in), value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          iq.re(i) = lhs.re(i)-rhs
          iq.im(i) = 0.0_sp
       end do
     end function xmm4c4_sub_s1

!DIR$ ATTRIBUTES INLINE :: c1_sub_xmm4c4
     pure function c1_sub_xmm4c4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c1_sub_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: c1_sub_xmm4c4
       complex(kind=sp), intent(in), value :: lhs
       type(XMM4c4_t),   intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       real(kind=sp), automatic :: cr,ci
       integer(kind=i4) :: i
       cr = real(lhs,kind=sp)
       ci = aimag(lhs,kind=sp)
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          iq.re = cr-rhs.re(i)
          iq.im = ci-rhs.im(i)
       end do
     end function c1_sub_xmm4c4

!DIR$ ATTRIBUTES INLINE :: xmm4r4_sub_xmm4c4
     pure function xmm4r4_sub_xmm4c4(lhs,rhs) result(iq)
     
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: xmm4r4_sub_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: xmm4r4_sub_xmm4c4
       type(XMM4r4_t), intent(in) :: lhs
       type(XMM4c4_t), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          iq.re(i) = lhs.v(i)-rhs.re(i)
          iq.im(i) = 0.0_sp
       end do
     end function xmm4r4_sub_xmm4c4

!DIR$ ATTRIBUTES INLINE :: s1_sub_xmm4c4
     pure function  s1_sub_xmm4c4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  s1_sub_xmm4c4
       !DIR$ ATTRIBUTES VECTOR ::  s1_sub_xmm4c4
       real(kind=sp),  intent(in), value :: lhs
       type(XMM4c4_t), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          iq.re(i) = lhs-rhs.re(i)
          iq.im(i) = 0.0_sp
       end do
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
       integer(kind=i4) :: i
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          xmm0.v(i) = lhs.re(i)*rhs.re(i)
          xmm1.v(i) = lhs.im(i)*rhs.im(i)
          iq.re(i)  = xmm0.v(i)-xmm1.v(i)
          xmm2.v(i) = lhs.im(i)*rhs.re(i)
          xmm3.v(i) = lhs.re(i)*rhs.im(i)
          iq.im(i)  = xmm2.v(i)-xmm3.v(i)
       end do
     end function xmm4c4_mul_xmm4c4

!DIR$ ATTRIBUTES INLINE :: xmm4c4_mul_c1
     pure function  xmm4c4_mul_c1(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  xmm4c4_mul_c1
       !DIR$ ATTRIBUTES VECTOR ::  xmm4c4_mul_c1
       type(XMM4c4_t),     intent(in) :: lhs
       complex(kind=sp),   intent(in), value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       !DIR$ ATTRIBUTES ALIGN : 16 :: xmm0,xmm1,xmm2,xmm3
       type(XMM4r4_t) :: xmm0,xmm1,xmm2,xmm3
       real(kind=sp), automatic :: cr,ci
       integer(kind=i4) :: i
       cr = real(rhs,kind=sp)
       ci = aimag(rhs,kind=sp)
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          xmm0.v(i) = lhs.re(i)*cr
          xmm1.v(i) = lhs.im(i)*ci
          iq.re(i)  = xmm0.v(i)-xmm1.v(i)
          xmm2.v(i) = lhs.im(i)*cr
          xmm3.v(i) = lhs.re(i)*ci
          iq.im(i)  = xmm2.v(i)-xmm3.v(i)
       end do
     end function  xmm4c4_mul_c1

!DIR$ ATTRIBUTES INLINE :: xmm4c4_mul_xmm4r4
     pure function xmm4c4_mul_xmm4r4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: xmm4c4_mul_xmm4r4
       !DIR$ ATTRIBUTES VECTOR :: xmm4c4_mul_xmm4r4
       type(XMM4c4_t),    intent(in) :: lhs
       type(XMM4r4_t),    intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          iq.re(i) = lhs.re(i)*rhs.v(i)
          iq.im(i) = lhs.im(i)*rhs.v(i)
       end do
     end function xmm4c4_mul_xmm4r4

!DIR$ ATTRIBUTES INLINE :: xmm4c4_mul_s1
     pure function  xmm4c4_mul_s1(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  xmm4c4_mul_s1
       !DIR$ ATTRIBUTES VECTOR ::  xmm4c4_mul_s1
       type(XMM4c4_t),    intent(in) :: lhs
       real(kind=sp),     intent(in), value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          iq.re(i) = lhs.re(i)*rhs
          iq.im(i) = lhs.im(i)*rhs
       end do
     end function  xmm4c4_mul_s1

!DIR$ ATTRIBUTES INLINE :: c1_mul_xmm4c4
     pure function c1_mul_xmm4c4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c1_mul_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: c1_mul_xmm4c4
       complex(kind=sp),   intent(in), value :: lhs
       type(XMM4c4_t),     intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       !DIR$ ATTRIBUTES ALIGN : 16 :: xmm0,xmm1,xmm2,xmm3
       type(XMM4r4_t) :: xmm0,xmm1,xmm2,xmm3
       real(kind=sp), automatic :: cr,ci
       integer(kind=i4) :: i
       cr = real(lhs,kind=sp)
       ci = aimag(lhs,kind=sp)
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
           xmm0.v(i) = cr*rhs.re(i)
           xmm1.v(i) = ci*rhs.im(i)
           iq.re(i)  = xmm0.v(i)+xmm1.v(i)
           xmm2.v(i) = cr*rhs.im(i)
           xmm3.v(i) = ci*rhs.re(i)
           iq.im(i)  = xmm2.v(i)-xmm3.v(i)
       end do
     end function c1_mul_xmm4c4

!DIR$ ATTRIBUTES INLINE :: xmm4r4_mul_xmm4c4
     pure function  xmm4r4_mul_xmm4c4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 ::  xmm4r4_mul_xmm4c4
       !DIR$ ATTRIBUTES VECTOR ::  xmm4r4_mul_xmm4c4
       type(XMM4r4_t),        intent(in) :: lhs
       type(XMM4c4_t),   intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 ::  xmm4r4_mul_xmm4c4
       type(XMM4c4_t) :: iq
        integer(kind=i4) :: i
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          iq.re(i) = lhs.v(i)*rhs.re(i)
          iq.im(i) = lhs.v(i)*rhs.im(i)
       end do
     end function  xmm4r4_mul_xmm4c4

!DIR$ ATTRIBUTES INLINE :: s1_mul_xmm4c4
     pure function s1_mul_xmm4c4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: s1_mul_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: s1_mul_xmm4c4
       real(kind=sp),  intent(in), value :: lhs
       type(XMM4c4_t), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          iq.re(i) = lhs*rhs.re(i)
          iq.im(i) = lhs*rhs.im(i)
       end do
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
       integer(kind=i4) :: i
#if (USE_SAFE_COMPLEX_DIVISION) == 1
       iq = cdiv_smith(lhs,rhs)
#else
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          xmm0.v(i) = lhs.re(i)*rhs.re(i)
          xmm1.v(i) = lhs.im(i)*rhs.im(i)
          xmm2.v(i) = lhs.im(i)*rhs.re(i)
          xmm3.v(i) = lhs.re(i)*rhs.im(i)
          den.v(i)  = (rhs.re(i)*rhs.re(i))+ &
                      (rhs.im(i)*rhs.im(i))
          iq.re(i)  = (xmm0.v(i)+xmm1.v(i))/den.v(i)
          iq.im(i)  = (xmm2.v(i)-xmm3.v(i))/den.v(i)
       end do
#endif
     end function  xmm4c4_div_xmm4c4   

!DIR$ ATTRIBUTES INLINE :: xmm4c4_div_c1
     pure function xmm4c4_div_c1(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: xmm4c4_div_c1
       !DIR$ ATTRIBUTES VECTOR :: xmm4c4_div_c1
       type(XMM4c4_t),   intent(in) :: lhs
       complex(kind=sp), intent(in), value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       !DIR$ ATTRIBUTES ALIGN : 16 :: xmm0,xmm1,xmm2,xmm3,den
       type(XMM4r4_t), automatic :: xmm0,xmm1,xmm2,xmm3,den
       real(kind=sp), automatic :: cr,ci
       integer(kind=i4) :: i
       cr = real(rhs,kind=sp)
       ci = aimag(rhs,kind=sp)
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          xmm0.v(i) = lhs.re(i)*cr
          xmm1.v(i) = lhs.im(i)*ci
          xmm2.v(i) = lhs.im(i)*cr
          xmm3.v(i) = lhs.re(i)*ci
          den.v(i)  = (cr*cr)+(ci*ci) 
          iq.re(i) = (xmm0.v(i)+xmm1.v(i))/den.v(i)
          iq.im(i) = (xmm2.v(i)-xmm3.v(i))/den.v(i)
       end do
     end function xmm4c4_div_c1

!DIR$ ATTRIBUTES INLINE :: xmm4c4_div_v2
     pure function xmm4c4_div_v2(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: xmm4c4_div_v2
       !DIR$ ATTRIBUTES VECTOR :: xmm4c4_div_v2
       type(XMM4c4_t),  intent(in) :: lhs
       type(XMM4r4_t),       intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
           iq.re(i) = lhs.re(i)/rhs.v(i)
           iq.im(i) = lhs.im(i)/rhs.v(i)
       end do
     end function xmm4c4_div_v2

!DIR$ ATTRIBUTES INLINE :: xmm4c4_div_s1
     pure  function xmm4c4_div_s1(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: xmm4c4_div_s1
       !DIR$ ATTRIBUTES VECTOR :: xmm4c4_div_s1
       type(XMM4c4_t),     intent(in) :: lhs
       real(kind=sp),      intent(in), value :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          iq.re(i) = lhs.re(i)/rhs
          iq.im(i) = lhs.im(i)/rhs
       end do
     end function xmm4c4_div_s1

!DIR$ ATTRIBUTES INLINE :: c1_div_xmm4c4
     pure function c1_div_xmm4c4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: c1_div_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: c1_div_xmm4c4
       complex(kind=sp), intent(in), value :: lhs
       type(XMM4c4_t),   intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       !DIR$ ATTRIBUTES ALIGN : 16 :: xmm0,xmm1,xmm2,xmm3,den
       type(XMM4c4_t), automatic :: xmm0,xmm1,xmm2,xmm3,den
       real(kind=sp), automatic :: cr,ci
       integer(kind=i4) :: i
       cr = real(lhs,kind=sp)
       ci = aimag(lhs,kind=sp)
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          xmm0.v(i) = cr*rhs.re(i)
          xmm1.v(i) = ci*rhs.im(i)
          xmm2.v(i) = ci*rhs.re(i)
          xmm3.v(i) = cr*rhs.im(i)
          den.v(i)  = (rhs.re(i)*rhs.re(i))+ &
                      (rhs.im(i)*rhs.im(i))
          iq.re(i)  = (xmm0.v(i)+xmm1.v(i))/den.v(i)
          iq.im(i)  = (xmm2.v(i)-xmm3.v(i))/den.v(i)
       end do
     end function c1_div_xmm4c4

!DIR$ ATTRIBUTES INLINE :: v2_div_xmm4c4
     pure  function v2_div_xmm4c4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: v2_div_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: v2_div_xmm4c4
       type(XMM4r4_t), intent(in) :: lhs
       type(XMM4c4_t), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       !DIR$ ATTRIBUTES ALIGN : 16 :: t0
       type(XMM4c4_t) :: t0
       integer(kind=i4) :: i
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          t0.re(i) = lhs.v(i)
          t0.im(i) = 0.0_sp
       end do
       iq    = t0/rhs
    end function v2_div_xmm4c4

!DIR$ ATTRIBUTES INLINE :: s1_div_xmm4c4
     pure  function s1_div_xmm4c4(lhs,rhs) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: s1_div_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: s1_div_xmm4c4
       real(kind=sp),  intent(in), value :: lhs
       type(XMM4c4_t), intent(in) :: rhs
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       !DIR$ ATTRIBUTES ALIGN : 16 :: t0
       type(XMM4c4_t) :: t0
       integer(kind=i4) :: i
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          t0.re(i) = lhs
          t0.im(i) = 0.0_sp
       end do
       iq    = t0/rhs
     end function s1_div_xmm4c4

!DIR$ ATTRIBUTES INLINE :: conjugate
     pure  function conjugate(x) result(iq)
      
       !DIR$ ATTRIBUTES CODE_ALIGN : 16 :: conjugate
       !DIR$ ATTRIBUTES VECTOR :: conjugate
       type(XMM4c4_t),  intent(in) :: x
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          iq.re(i) = 0.0_sp-x.re(i)
          iq.im(i) = x.im(i)
       end do
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
       complex(kind=sp),  intent(in), value :: rhs
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
       complex(kind=sp),   intent(in), value :: lhs
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
       complex(kind=sp),  intent(in), value :: rhs
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
       complex(kind=sp),   intent(in), value :: lhs
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
       complex(kind=sp),  intent(in), value :: rhs
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
       complex(kind=sp),   intent(in), value :: lhs
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
       complex(kind=sp),  intent(in), value :: rhs
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
       complex(kind=sp),   intent(in), value :: lhs
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
       complex(kind=sp),  intent(in), value :: rhs
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
       complex(kind=sp),   intent(in), value :: lhs
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
       complex(kind=sp),  intent(in), value :: rhs
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
       complex(kind=sp),   intent(in), value :: lhs
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
       integer(kind=i4) :: i
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          iq.re(i) = rho.v(i)*cos(theta.v(i))
          iq.im(i) = rho.v(i)*sin(theta.v(i)) 
       end do
     end function polar
     
!DIR$ ATTRIBUTES INLINE :: carg_xmm4c4
     pure function carg_xmm4c4(c) result(arg)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: carg_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: carg_xmm4c4
       type(XMM4c4_t),   intent(in) :: c
       !DIR$ ATTRIBUTES ALIGN : 16 :: arg
       type(XMM4r4_t) :: arg
       integer(kind=i4) :: i
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          arg.v(i)  = atan2(c.im(i),c.re(i))
       end do
     end function carg_xmm4c4

!DIR$ ATTRIBUTES INLINE :: carg_2xmm4r4     
     pure function carg_2xmm4r4(re,im) result(arg)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: carg_2xmm4r4
       !DIR$ ATTRIBUTES VECTOR :: carg_2xmm4r4
       type(XMM4r4_t),  intent(in) :: re
       type(XMM4r4_t),  intent(in) :: im
       !DIR$ ATTRIBUTES ALIGN : 16 :: arg
       type(XMM4r4_t) :: arg
       integer(kind=i4) :: i
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          arg.v(i) = atan2(im.v(i),re.v(i))
       end do
     end function carg_2xmm4r4

!DIR$ ATTRIBUTES INLINE :: csin_xmm4c4
     pure function csin_xmm4c4(c) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: csin_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: csin_xmm4c4
       type(XMM4c4_t),  intent(in) :: c
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       !DIR$ ATTRIBUTES ALIGN : 16 :: tre,tim
       type(XMM4r4_t) :: tre
       type(XMM4r4_t) :: tim
       integer(kind=i4) :: i
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          tre.v(i) = c.re(i)
          tim.v(i) = c.im(i)
          iq.re(i) = sin(tre.v(i))*cosh(tim.v(i))
          iq.im(i) = cos(tre.v(i))*sinh(tim.v(i))
       end do
    end function csin_xmm4c4

!DIR$ ATTRIBUTES INLINE :: csin_xmm4r8
     pure function csin_xmm4r4(re,im) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: csin_xmm4r4
       !DIR$ ATTRIBUTES VECTOR :: csin_xmm8r8
       type(XMM4r4_t),   intent(in) :: re
       type(XMM4r4_t),   intent(in) :: im
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          iq.re(i) = sin(re.v(i))*cosh(im.v(i))
          iq.im(i) = cos(re.v(i))*sinh(im.v(i))
       end do
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
       integer(kind=i4) :: i
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          tre.v(i) = c.re(i)
          tim.v(i) = c.im(i)
          iq.re(i) = sinh(tre.v(i))*cos(tim.v(i))
          iq.im(i) = cosh(tre.v(i))*sin(tim.v(i))
       end do
     end function csinh_xmm4c4

!DIR$ ATTRIBUTES INLINE :: csinh_xmm4r4
     pure function csinh_xmm4r4(re,im) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: csinh_xmm4r4
       !DIR$ ATTRIBUTES VECTOR :: csinh_xmm8r8
       type(XMM4r4_t), intent(in) :: re
       type(XMM4r4_t), intent(in) :: im
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          iq.re(i) = sinh(re.v(i))*cos(im.v(i))
          iq.im(i) = cosh(re.v(i))*sin(im.v(i))
       end do
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
       integer(kind=i4) :: i
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          tre.v(i) = c.re(i)
          tim.v(i) = c.im(i)
          iq.re(i) = cos(tre.v(i))*cosh(tim.v(i))
          iq.im(i) = sin(tre.v(i))*sinh(tim.v(i))
     end function ccos_xmm4c4

!DIR$ ATTRIBUTES INLINE :: ccos_xmm4r4
     pure function ccos_xmm4r4(re,im) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ccos_xmm4r4
       !DIR$ ATTRIBUTES VECTOR :: ccos_xmm4r4
       type(XMM4r4_t), intent(in) :: re
       type(XMM4r4_t), intent(in) :: im
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          iq.re(i) = cos(re.v(i))*cosh(im.v(i))
          iq.im(i) = sin(re.v(i))*sinh(im.v(i))
       end do
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
       integer(kind=i4) :: i
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          tre.v(i) = c.re(i)
          tim.v(i) = c.im(i)
          iq.re(i) = cosh(tre.v(i))*cos(tim.v(i))
          iq.im(i) = sinh(tre.v(i))*sin(tim.v(i))
       end do
     end function ccosh_xmm4c4

!DIR$ ATTRIBUTES INLINE :: ccosh_xmm4r4
     pure function ccosh_xmm4r4(re,im) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ccosh_xmm4r4
       !DIR$ ATTRIBUTES VECTOR :: ccosh_xmm4r4
       type(XMM4r4_t),   intent(in) :: re
       type(XMM4r4_t),   intent(in) :: im
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       integer(kind=i4) :: i
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          iq.re(i) = cosh(re.v(i))*cos(im.v(i))
          iq.im(i) = sinh(re.v(i))*sin(im.v(i))
       end do
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
       integer(kind=i4) :: i
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          tre.v(i) = c.re(i)
          tim.v(i) = c.im(i)
          iq.re(i) = exp(tre.v(i))*cos(tim.v(i))
          iq.im(i) = exp(tre.v(i))*sin(tim.v(i))
       end do
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
       integer(kind=i4) :: i
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          iq.re(i) = exp(re.v(i))*cos(im.v(i))
          iq.im(i) = exp(re.v(i))*sin(im.v(i))
       end do
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
     pure function cpow_xmm4c4(c,n) result(iq)
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cpow_xmm4c4
       !DIR$ ATTRIBUTES VECTOR :: cpow_xmm4c4
       type(XMM4c4_t), intent(in) :: c
       real(kind=sp),       intent(in) :: n
       !DIR$ ATTRIBUTES ALIGN : 16 :: iq
       type(XMM4c4_t) :: iq
       !DIR$ ATTRIBUTES ALIGN : 16 :: tre,tim
       type(XMM4r4_t) :: tre,tim
       !DIR$ ATTRIBUTES ALIGN : 16 :: r,theta,pow,trig
       type(XMM4r4_t) :: r,theta,pow,trig
         integer(kind=i4) :: i
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          tre.v(i) = c.re(i)
          tim.v(i) = c.im(i)
          r.v(i) = sqrt(tre.v(i)*tre.v(i)+ &
                        tim.v(i)*tim.v(i))
          pow.v(i)   = r.v(i)**n
          theta.v(i) = atan(tim.v(i)/tre.v(i))
          trig.v(i)  = theta.v(i)*n
          iq.re(i)   = pow.v(i)*cos(trig.v(i))
          iq.im(i)   = pow.v(i)*sin(trig.v(i))
       end do
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
       integer(kind=i4) :: i
       !dir$ loop_count(4)
       !dir$ vector aligned
       !dir$ vector vectorlength(4)
       !dir$ vector always
       do i=0,3
          r.v(i) = sqrt(re.v(i)*re.v(i)+ &
                        im.v(i)*im.v(i))
          pow.v(i)   = r.v(i)**n
          theta.v(i) = atan(im.v(i)/re.v(i))
          trig.v(i)  = theta.v(i)*n
          iq.re(i)   = pow.v(i)*cos(trig.v(i))
          iq.im(i)   = pow.v(i)*sin(trig.v(i))
       end do
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
        integer(kind=i4) :: i
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
        
!#if   (USE_INTRINSIC_VECTOR_COMPARE) == 1
!        if(mask_gte == all_ones) then
!#elif
        if(all(bres)) then
!#endif
           
           !dir$ loop_count(4)
           !dir$ vector aligned
           !dir$ vector vectorlength(4)
           !dir$ vector always
           do i=0,3
               ratio.v(i)   = rhs.im(i)/rhs.re(i)
               denom.v(i)   = rhs.re(i)+(ratio.v(i)*rhs.im(i))
               iq.re(i)     = (lhs.re(i)+lhs.im(i)*ratio.v(i))/denom.v(i)
               iq.im(i)     = (lhs.im(i)-lhs.re(i)*ratio.v(i))/denom.v(i)
           end do
        else
           !dir$ loop_count(4)
           !dir$ vector aligned
           !dir$ vector vectorlength(4)
           !dir$ vector always
           do i=0,3
              ratio.v(i)   = rhs.re(i)/rhs.im(i)
              denom.v(i)   = rhs.im(i)+ratio.v(i)*rhs.re(i)
              iq.re(i)     = (lhs.re(i)*ratio.v(i)+lhs.im(i))/denom.v(i)
              iq.im(i)     = (lhs.im(i)*ratio.v(i)-lhs.re(i))/denom.v(i)
           end do
        end if
      end function cdiv_smith

      
     
end module sse_cvec4_v2
