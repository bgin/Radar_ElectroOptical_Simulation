
#include "GMS_config.fpp"

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

module rcs_planar_zmm16r4



!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         rcs_planar_zmm16r4
 !          
 !          Purpose:
 !                        Various characteristics of analytically derived Radar
 !                        Cross Section of planar objects  
 !                        Based  on George T. Ruck, Donald E. Barrick , William D. Stuart , 
 !                        - "Radar Cross Section Handbook 1 and 2" (1970, Kluwer Academic Plenum Publishers) 
 !                        This module contains only explicitly vectorized (SIMD)
 !                        
 !          History:
 !                        Date: 01-09-2024
 !                        Time: 07:07 GMT+2
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
 !                      George T. Ruck, Donald E. Barrick , William D. Stuart
 !                      Radar Cross Section Handbook 1 and 2" (1970, Kluwer Academic Plenum Publishers)     
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
!==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.

    use mod_kinds,    only : i4,sp
    use mod_vectypes, only : ZMM16r4_t
    use avx512_cvec16_v2

    public
    implicit none
    
     ! Major version
     integer(kind=i4),  parameter :: RCS_PLANAR_ZMM16R4_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: RCS_PLANAR_ZMM16R4_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: RCS_PLANAR_ZMM16R4_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: RCS_PLANAR_ZMM16R4_FULLVER =   &
            1000*RCS_PLANAR_ZMM16R4_MAJOR+100*RCS_PLANAR_ZMM16R4_MINOR+10*RCS_PLANAR_ZMM16R4_MICRO
     ! Module creation date
     character(*),        parameter :: RCS_PLANAR_ZMM16R4_CREATE_DATE = "01-09-2024 07:10 +00200 (SUN 01 SEP 2024 GMT+2)"
     ! Module build date
     character(*),        parameter :: RCS_PLANAR_ZMM16R4_BUILD_DATE  = __DATE__ " " __TIME__
     ! Module author info
     character(*),        parameter :: RCS_PLANAR_ZMM16R4_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: RCS_PLANAR_ZMM16R4_SYNOPSIS    = "Analytical Cylindrical objects RCS characteristics and models explicitly vectorized (SIMD)."
    
#ifndef __RCS_PLANAR_PF_CACHE_HINT__
#define __RCS_PLANAR_PF_CACHE_HINT__ 1
#endif 
    

        !/*
        !               Complex impedances.
        !               Formula 7.1-6
        !           */


      pure function zi_f716_zmm16r4(tht,mu,eps) result(z)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: zi_f716_zmm16r4
            !dir$ attributes forceinline :: zi_f716_zmm16r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: zi_f716_zmm16r4
            use mod_vecconsts, only : v16_0,v16_1
            type(ZMM16r4_t),   intent(in) :: tht
            type(ZMM16c4),     intent(in) :: mu
            type(ZMM16c4),     intent(in) :: eps
            type(ZMM16c4) :: z
            ! Locals
            type(ZMM16r4_t), automatic :: cost
            type(ZMM16r4_t), automatic :: invc
            type(ZMM16r4_t), automatic :: wrkc
            type(ZMM16c4),   automatic :: div
            type(ZMM16c4),   automatic :: csq
            !dir$ attributes align : 64 :: cost
            !dir$ attributes align : 64 :: invc
            !dir$ attributes align : 64 :: wrkc
            !dir$ attributes align : 64 :: div
            !dir$ attributes align : 64 :: csq
            cost.v = cos(tht.v)
            wrkc.v = v16_0.v
            div    = mu/eps       
            invc.v = v16_1.v/cost.v
            csq    = csqrt_c16(div)
            z.re   = invc.v*csq.re
            z.im   = invc.v*csq.im
      end function zi_f716_zmm16r4
      
      
       !/*
       !                   Equivalent complex impedances.
       !                   Formula 7.1-4
       !             */
      
      pure function R_f714_zmm16r4(tht1,mu1,eps1,tht2,mu2,eps2) result(R)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: R_f714_zmm16r4
            !dir$ attributes forceinline :: R_f714_zmm16r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: R_f714_zmm16r4
            type(ZMM16r4_t),    intent(in) :: tht1
            type(ZMM16c4),      intent(in) :: mu1
            type(ZMM16c4),      intent(in) :: eps1
            type(ZMM16r4_t),    intent(in) :: tht2
            type(ZMM16c4),      intent(in) :: mu2
            type(ZMM16c4),      intent(in) :: eps2
            type(ZMM16c4)  :: R
            ! Locals
            type(ZMM16c4), automatic :: z1
            type(ZMM16c4), automatic :: z2
            type(ZMM16c4), automatic :: t0
            type(ZMM16c4), automatic :: t1 
            !dir$ attributes align : 64 :: z1
            !dir$ attributes align : 64 :: z2
            !dir$ attributes align : 64 :: t0
            !dir$ attributes align : 64 :: t1
            z1    = zi_f716_zmm16r4(tht1,mu1,eps1)
            z2    = zi_f716_zmm16r4(tht2,mu2,eps2)
            t0    = z1-z2
            t1    = z1+z2
            t0.re = -t0.re
            t0.im = -t0.im
            R     = t0/t1 
      end function R_f714_zmm16r4
      
      
      !            /*
      !                  Transmission coefficient components.
      !                  Formula 7.1-5
      !              */
      
       pure function T_f715_zmm16r4(tht1,mu1,eps1,tht2,mu2,eps2) result(T)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: T_f715_zmm16r4
            !dir$ attributes forceinline :: T_f715_zmm16r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: T_f715_zmm16r4
            type(ZMM16r4_t),      intent(in) :: tht1
            type(ZMM16c4),        intent(in) :: mu1
            type(ZMM16c4),        intent(in) :: eps1
            type(ZMM16r4_t),      intent(in) :: tht2
            type(ZMM16c4),        intent(in) :: mu2
            type(ZMM16c4),        intent(in) :: eps2
            type(ZMM16c4) :: T
            ! Locals
            type(ZMM16r4_t), parameter :: C20 = ZMM16r4_t(2.0_sp)
            type(ZMM16c4), automatic   :: z1
            type(ZMM16c4), automatic   :: z2
            type(ZMM16c4), automatic   :: t0
            type(ZMM16c4), automatic   :: t1
             !dir$ attributes align : 64 :: C20
             !dir$ attributes align : 64 :: z1
             !dir$ attributes align : 64 :: z2
             !dir$ attributes align : 64 :: t0
             !dir$ attributes align : 64 :: t1
            z2 = zi_f716_zmm16r4(tht2,mu2,eps2)
            z1 = zi_f716_zmm16r4(tht1,mu1,eps1)
            t0 = z2*C20
            t1 = z1+z2
            R  = t0/t1
       end function T_f715_zmm16r4
       
       ! /*
       !                 Reflection coefficient special cases:
       !                 1) k1<k2, eps1,eps2 (real), mu1 = m2 = mu0
       !                 Formula 7.1-17
       !            */
       
       pure function R_f7117_zmm16r4(tht,eps1,eps2) result(R)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: R_f7117_zmm16r4
            !dir$ attributes forceinline :: R_f7117_zmm16r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: R_f7117_zmm16r4
            type(ZMM16r4_t),          intent(in) :: tht
            type(ZMM16r4_t),          intent(in) :: eps1
            type(ZMM16r4_t),          intent(in) :: eps2
            type(ZMM16r4_t)  :: R
            ! Locals
            type(ZMM16r4_t), automatic :: e1e2
            type(ZMM16r4_t), automatic :: sqr1
            type(ZMM16r4_t), automatic :: sqr2
            type(ZMM16r4_t), automatic :: num
            type(ZMM16r4_t), automatic :: den
            type(ZMM16r4_t), automatic :: cost
            type(ZMM16r4_t), automatic :: sint
            type(ZMM16r4_t), automatic :: x0
            type(ZMM16r4_t), automatic :: x1
            !dir$ attributes align : 64 :: e1e2
            !dir$ attributes align : 64 :: sqr1
            !dir$ attributes align : 64 :: sqr2
            !dir$ attributes align : 64 :: num
            !dir$ attributes align : 64 :: den
            !dir$ attributes align : 64 :: cost
            !dir$ attributes align : 64 :: sint
            !dir$ attributes align : 64 :: x0
            !dir$ attributes align : 64 :: x1
            type(ZMM16r4_t), parameter :: C1 = ZMM16r4_t(1.0_sp)
#if (GMS_EXPLICIT_VECTORIZE) == 1
             integer(kind=i4) :: j
             !dir$ loop_count(16)
             !dir$ vector aligned
             !dir$ vector vectorlength(4)
             !dir$ vector always
             do j=0,15
                e1e2.v(j) = eps1.v(j)/eps2.v(j)
                cost.v(j) = cos(tht.v(j))
                sqr1.v(j) = sqrt(e1e2.v(j))
                sint.v(j) = sin(tht.v(j))
                x0.v(j)   = sqr1.v(j)*cost.v(j)
                x1.v(j)   = C1.v(j)-e1e2.v(j)*(sint.v(j)*sint.v(j))
                sqr2.v(j) = sqrt(x1.v(j))
                num.v(j)  = x0.v(j)-x1.v(j)
                den.v(j)  = x0.v(j)+x1.v(j)
                R.v(j)    = num.v(j)/den.v(j)      
             end do
#else
                e1e2.v = eps1.v/eps2.v
                cost.v = cos(tht.v)
                sqr1.v = sqrt(e1e2.v)
                sint.v = sin(tht.v)
                x0.v   = sqr1.v*cost.v
                x1.v   = C1.v-e1e2.v*(sint.v*sint.v)
                sqr2.v = sqrt(x1.v)
                num.v  = x0.v-x1.v
                den.v  = x0.v+x1.v
                R.v    = num.v/den.v      
#endif
       end function R_f7117_zmm16r4





end module rcs_planar_zmm16r4
