
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

module sph_complex_xmm2r8


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         dph_complex_xmm2r8
 !          
 !          Purpose:
 !                       Complex Spherical Harmonics up to degree (l=10).
 !                        
 !          History:
 !                        Date: 13-10-2024
 !                        Time: 04:45 GMT+2
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
 !          References:
 !         
 !                      https://en.wikipedia.org/wiki/Table_of_dpherical_harmonics
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
!==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.

    use mod_kinds,    only : i4,dp
    use sse_cvec2
    use mod_vectypes, only : XMM2r8_t,Mask2_t
    
    public
    implicit none
    
    
      ! Major version
     integer(kind=i4),  parameter :: SPH_COMPLEX_XMM2R8_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: SPH_COMPLEX_XMM2R8_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: SPH_COMPLEX_XMM2R8_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: SPH_COMPLEX_XMM2R8_FULLVER =   &
            1000*SPH_COMPLEX_XMM2R8_MAJOR+100*SPH_COMPLEX_XMM2R8_MINOR+10*SPH_COMPLEX_XMM2R8_MICRO
     ! Module creation date
     character(*),        parameter :: SPH_COMPLEX_XMM2R8_CREATE_DATE = "13-10-2024 09:15AM +00200 (SUN 13 OCT 2024 GMT+2)"
     ! Module build date
     character(*),        parameter :: SPH_COMPLEX_XMM2R8_BUILD_DATE  = __DATE__ " " __TIME__
     ! Module author info
     character(*),        parameter :: SPH_COMPLEX_XMM2R8_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: SPH_COMPLEX_XMM2R8_SYNOPSIS    = "Complex Spherical Harmonics up to degree (l=5)." 
     
     type(XMM2c8), parameter, public :: I = XMM2c8(0.0_dp,1.0_dp)
     ! Data types
     
    
     
     contains
     
     ! Degree l=0
     
     pure function SphcY0_0_v128b_pd() result(y0)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY0_0_v128b_pd
            !dir$ attributes forceinline :: SphcY0_0_v128b_pd
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY0_0_v128b_pd
#endif
            type(XMM2r8_t) :: y0
            y0 = XMM2r8_t(0.28209479177387814347403972578_dp)
     end function SphcY0_0_v128b_pd
     
     ! Degree l=1
     
     pure function SphcY1_inv1_v128b_pd(c,r) result(y1)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY1_inv1_v128b_pd
            !dir$ attributes forceinline :: SphcY1_inv1_v128b_pd
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY1_inv1_v128b_pd
#endif     
            type(XMM2c8),    intent(in) :: c
            type(XMM2r8_t),  intent(in) :: r
            type(XMM2c8) :: y1
            type(XMM2r8_t),  parameter :: C0345494149471335479265244646032 = &
                                   XMM2r8_t(0.345494149471335479265244646032_dp)
            type(XMM2c8),    automatic :: tmp
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)             
            !dir$ attributes align : 16 :: C0345494149471335479265244646032 
            !dir$ attributes align : 16 :: tmp
#endif
            tmp = c/r
            y0  = tmp*C0345494149471335479265244646032
     end function SphcY1_inv1_v128b_pd
     
     pure function SphcY1_0_v128b_pd(z,r) result(y0)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY1_0_v128b_pd
            !dir$ attributes forceinline :: SphcY1_0_v128b_pd
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY1_0_v128b_pd
#endif          
            type(XMM2r8_t),   intent(in) :: z
            type(XMM2r8_t),   intent(in) :: r
            type(XMM2r8_t) :: y0
            type(XMM2r8_t),   parameter :: C1534990061919732732719327437334 = &
                                  XMM2r8_t(1.534990061919732732719327437334_dp)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)                                   
            !dir$ attributes align : 16 :: C1534990061919732732719327437334
#endif            
            y0.v = C1534990061919732732719327437334.v*(z.v/r.v)
     end function SphcY1_0_v128b_pd
     
     pure function SphcY1_1_v128b_pd(c,r) result(y1)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY1_1_v128b_pd
            !dir$ attributes forceinline :: SphcY1_1_v128b_pd
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY1_1_v128b_pd
#endif     
            type(XMM2c8),    intent(in) :: c
            type(XMM2r8_t),  intent(in) :: r
            type(XMM2c8) :: y1
            type(XMM2r8_t),  parameter :: C0345494149471335479265244646032 = &
                                   XMM2r8_t(-0.345494149471335479265244646032_dp)
            type(XMM2c8),    automatic :: tmp
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)             
            !dir$ attributes align : 16 :: C0345494149471335479265244646032
            !dir$ attributes align : 16 :: tmp
#endif            
            tmp = c/r
            y1  = tmp*C0345494149471335479265244646032
     end function SphcY1_1_v128b_pd
     
     ! Degree l=2
     
     pure function SphcY2_inv2_v128b_pd(c,r) result(y2)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY2_inv2_v128b_pd
            !dir$ attributes forceinline :: SphcY2_inv2_v128b_pd
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY2_inv2_v128b_pd
#endif     
            type(XMM2c8),    intent(in) :: c
            type(XMM2r8_t),  intent(in) :: r
            type(XMM2c8) :: y2
            type(XMM2r8_t),  parameter :: C0386274202023189580342192735311 = &
                                    XMM2r8_t(0.386274202023189580342192735311_dp)
            type(XMM2c8),    automatic :: tmp1
            type(XMM2c8),    automatic :: csqr
            type(XMM2r8_t),  automatic :: tmp2
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)             
            !dir$ attributes align : 16 :: C0386274202023189580342192735311
            !dir$ attributes align : 16 :: tmp
            !dir$ attributes align : 16 :: csqr
#endif
            tmp2.v = r.v*r.v
            csqr   = c*c
            tmp1   = csqr/tmp2
            y2     = tmp1*C0386274202023189580342192735311
     end function SphcY2_inv2_v128b_pd


     pure function SphcY2_inv1_v128b_pd(c,z,r) result(y2)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY2_inv1_v128b_pd
            !dir$ attributes forceinline :: SphcY2_inv1_v128b_pd
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY2_inv1_v128b_pd
#endif     
            type(XMM2c8),    intent(in) :: c
            type(XMM2r8_t),  intent(in) :: z  
            type(XMM2r8_t),  intent(in) :: r
            type(XMM2c8)  :: y2
            type(XMM2r8_t),  parameter :: C0772548404046379160684385470623 = &
                                    XMM2r8_t(0.772548404046379160684385470623_dp)
            type(XMM2c8),    automatic :: tmp
            type(XMM2r8_t),  automatic :: rsqr
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)             
            !dir$ attributes align : 16 :: C0772548404046379160684385470623
            !dir$ attributes align : 16 :: tmp
            !dir$ attributes align : 16 :: rsqr
#endif
            rsqr.v = r.v*r.v
            tmp    = c*z
            tmp    = tmp/rsqr
            y2     = tmp*C0772548404046379160684385470623           
     end function SphcY2_inv1_v128b_pd
     
     pure function SphcY2_0_v128b_pd(z,r) result(y2)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY2_0_v128b_pd
            !dir$ attributes forceinline :: SphcY2_0_v128b_pd
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY2_0_v128b_pd
#endif       
            type(XMM2r8_t),   intent(in) :: z
            type(XMM2r8_t),   intent(in) :: r
            type(XMM2r8_t)  :: y2
            type(XMM2r8_t),   parameter :: C0630783130505040012061787380591 = 
                                   XMM2r8_t(0.630783130505040012061787380591_dp)
            type(XMM2r8_t),   parameter :: C3 = XMM2r8_t(3.0_dp)
            type(XMM2r8_t),   automatic :: zsqr
            type(XMM2r8_t),   automatic :: rsqr
            type(XMM2r8_t),   automatic :: diff
            type(XMM2r8_t),   automatic :: rat 
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)       
            !dir$ attributes align : 16 :: C0630783130505040012061787380591
            !dir$ attributes align : 16 :: C3
            !dir$ attributes align : 16 :: zsqr
            !dir$ attributes align : 16 :: rsqr
            !dir$ attributes align : 16 :: diff
            !dir$ attributes align : 16 :: rat
#endif
            zsqr.v = z.v*z.v
            rsqr.v = r.v*r.v
            diff.v = (C3.v*zsqr.v)-rsqr.v
            rat.v  = diff.v/rsqr.v
            y2.v   = C0630783130505040012061787380591.v*rat.v
     end function SphcY2_0_v128b_pd
     
     pure function SphcY2_1_v128b_pd(c,z,r) result(y2)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY2_1_v128b_pd
            !dir$ attributes forceinline :: SphcY2_1_v128b_pd
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY2_1_v128b_pd
#endif     
            type(XMM2c8),    intent(in) :: c
            type(XMM2r8_t),  intent(in) :: z  
            type(XMM2r8_t),  intent(in) :: r
            type(XMM2c8)  :: y2
            type(XMM2r8_t),  parameter :: C0772548404046379160684385470623 = &
                                    XMM2r8_t(-0.772548404046379160684385470623_dp)
            type(XMM2c8),    automatic :: tmp
            type(XMM2r8_t),  automatic :: rsqr
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)             
            !dir$ attributes align : 16 :: C0772548404046379160684385470623
            !dir$ attributes align : 16 :: tmp
            !dir$ attributes align : 16 :: rsqr
#endif  
            rsqr.v = r.v*r.v
            tmp    = c*z
            tmp    = tmp/rsqr
            y2     = tmp*C0772548404046379160684385470623     
     end function SphcY2_1_v128b_pd
     
     pure function SphcY2_2_v128b_pd(c,r) result(y2)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY2_2_v128b_pd
            !dir$ attributes forceinline :: SphcY2_2_v128b_pd
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY2_2_v128b_pd
#endif     
            type(XMM2c8),    intent(in) :: c
            type(XMM2r8_t),  intent(in) :: r
            type(XMM2c8) :: y2  
            y2 = Y2_inv2_v128b_pd(c,r)     
     end function SphcY2_2_v128b_pd

     
     ! Degree l=3

     pure function SphcY3_inv1_v128b_pd(c,z,r) result(y3)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)       
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY3_inv1_v128b_pd
            !dir$ attributes forceinline :: SphcY3_inv1_v128b_pd
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY3_inv1_v128b_pd
#endif
            type(XMM2c8),    intent(in) :: c
            type(XMM2r8_t),  intent(in) :: z
            type(XMM2r8_t),  intent(in) :: r
            type(XMM2c8)  ::               y3
            type(XMM2r8_t),  parameter  :: C032318018411415065300739416333 = &
                                    XMM2r8_t(0.32318018411415065300739416333_dp)
            type(XMM2r8_t),  parameter  :: C5 = XMM2r8_t(5.0_dp)
            type(XMM2c8),    automatic :: ct1
            type(XMM2c8),    automatic :: ct2
            type(XMM2r8_t),  automatic :: zz
            type(XMM2r8_t),  automatic :: rr
            type(XMM2r8_t),  automatic :: rrr
            type(XMM2r8_t),  automatic :: t0
            
            !dir$ attributes align : 16 :: C032318018411415065300739416333
            !dir$ attributes align : 16 :: C5
            !dir$ attributes align : 16 :: ct1
            !dir$ attributes align : 16 :: ct2
            !dir$ attributes align : 16 :: zz
            !dir$ attributes align : 16 :: rr
            !dir$ attributes align : 16 :: rrr
            !dir$ attributes align : 16 :: t0
           
            zz.v = z.v*z.v
            rr.v = r.v*r.v
            ct0.v= C5.v*zz.v-rr.v
            rrr.v= rr.v*r.v
            ct1  = c*t0
            ct2  = ct1/rrr
            y3   = ct2*C032318018411415065300739416333
     end function SphcY3_inv1_v128b_pd
     
     pure function SphcY3_0_v128b_pd(z,r) result(y3)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)  
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY3_0_v128b_pd
            !dir$ attributes forceinline :: SphcY3_0_v128b_pd
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY3_0_v128b_pd
#endif
            type(XMM2r8_t),  intent(in) :: z
            type(XMM2r8_t),  intent(in) :: r
            type(XMM2r8_t)  ::             y3
            type(XMM2r8_t),  parameter :: C0373176332590115391414395913199 = &
                                   XMM2r8_t(0.373176332590115391414395913199_dp)
            type(XMM2r8_t),  parameter :: C5 = XMM2r8_t(0.5_dp)
            type(XMM2r8_t),  parameter :: C3 = XMM2r8_t(3.0_dp)
            type(XMM2r8_t),  automatic :: zzz
            type(XMM2r8_t),  automatic :: rr
            type(XMM2r8_t),  automatic :: rrr
            type(XMM2r8_t),  automatic :: t0
            type(XMM2r8_t),  automatic :: t1
            type(XMM2r8_t),  automatic :: t2
            !dir$ attributes align : 16 :: C0373176332590115391414395913199
            !dir$ attributes align : 16 :: C5
            !dir$ attributes align : 16 :: C3
            !dir$ attributes align : 16 :: zzz
            !dir$ attributes align : 16 :: rr
            !dir$ attributes align : 16 :: rrr
            !dir$ attributes align : 16 :: t0
            !dir$ attributes align : 16 :: t1 
            !dir$ attributes align : 16 :: t2
            zzz.v = z.v*z.v*z.v
            rr.v  = r.v*r.v
            rrr.v = rr.v*r.v
            t0.v  = C5.v*zzz.v
            t1.v  = C3.v*z.v*rr.v
            t2.v  = t0.v-t1.v
            y3.v  = C0373176332590115391414395913199.v*(t2.v/rrr.v)
     end function SphcY3_0_v128b_pd
     
     pure function SphcY3_1_v128b_pd(c,z,r) result(y3)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)  
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY3_1_v128b_pd
            !dir$ attributes forceinline :: SphcY3_1_v128b_pd
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY3_1_v128b_pd
#endif
            type(XMM2c8),    intent(in) :: c
            type(XMM2r8_t),  intent(in) :: z
            type(XMM2r8_t),  intent(in) :: r
            type(XMM2c8)  ::               y3
            type(XMM2r8_t),  parameter  :: C032318018411415065300739416333 = &
                                    XMM2r8_t(-0.32318018411415065300739416333_dp)
            type(XMM2r8_t),  parameter  :: C5 = XMM2r8_t(5.0_dp)
            type(XMM2c8),    automatic :: ct1
            type(XMM2c8),    automatic :: ct2
            type(XMM2r8_t),  automatic :: zz
            type(XMM2r8_t),  automatic :: rr
            type(XMM2r8_t),  automatic :: rrr
            type(XMM2r8_t),  automatic :: t0
            
            !dir$ attributes align : 16 :: C032318018411415065300739416333
            !dir$ attributes align : 16 :: C5
            !dir$ attributes align : 16 :: ct1
            !dir$ attributes align : 16 :: ct2
            !dir$ attributes align : 16 :: zz
            !dir$ attributes align : 16 :: rr
            !dir$ attributes align : 16 :: rrr
            !dir$ attributes align : 16 :: t0
           
            zz.v = z.v*z.v
            rr.v = r.v*r.v
            ct0.v= C5.v*zz.v-rr.v
            rrr.v= rr.v*r.v
            ct1  = c*t0
            ct2  = ct1/rrr
            y3   = ct2*C032318018411415065300739416333
     end function SphcY3_1_v128b_pd
     
     pure function SphcY3_2_v128b_pd(c,z,r) result(y32)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)       
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY3_2_v128b_pd
            !dir$ attributes forceinline :: SphcY3_2_v128b_pd
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY3_2_v128b_pd
#endif
            type(XMM2c8),    intent(in) :: c
            type(XMM2r8_t),  intent(in) :: z
            type(XMM2r8_t),  intent(in) :: r
            type(XMM2c8)  ::               y32
            type(XMM2r8_t),  parameter :: C10219854764332823633961144917 = &
                                   XMM2r8_t(1.0219854764332823633961144917)
            type(XMM2c8),    automatic :: csqr
            type(XMM2c8),    automatic :: ct0
            type(XMM2c8),    automatic :: ct1
            type(XMM2r8_t),  automatic :: rrr
            !dir$ attributes align : 16 :: C10219854764332823633961144917
            !dir$ attributes align : 16 :: csqr
            !dir$ attributes align : 16 :: ct0
            !dir$ attributes align : 16 :: ct1
            !dir$ attributes align : 16 :: rrr
            rrr.v = r.v*r.v*r.v
            csqr  = c*c
            ct0   = csqr*z
            ct1   = ct0/rrr
            y32   = ct1*C10219854764332823633961144917
     end function SphcY3_2_v128v_pd

     pure function SphcY3_3_v128b_pd(c,r) result(y33)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)     
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY3_3_v128b_pd
            !dir$ attributes forceinline :: SphcY3_3_v128b_pd
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY3_3_v128b_pd
#endif
            type(XMM2c8),    intent(in) :: c
            type(XMM2r8_t),  intent(in) :: r
            type(XMM2c8)  ::               y33
            type(XMM2r8_t),  parameter :: C0417223823632784089724427015737 =  &
                                  XMM2r8_t(0.417223823632784089724427015737_dp)
            type(XMM2c8),    automatic :: pow3c
            type(XMM2c8),    automatic :: rat
            type(XMM2r8_t),  automatic :: rrr
            !dir$ attributes align : 16 :: C0417223823632784089724427015737
            !dir$ attributes align : 16 :: pow3c
            !dir$ attributes align : 16 :: rat
            rrr.v = r.v*r.v*r.v
            pow3c = cpow_xmm4c4(c,3.0_dp)
            rat   = pow3c/rrr
            y33   = rat*C0417223823632784089724427015737
     end function SphcY3_3_v128b_pd
     
     
     ! Degree l=4
     pure function SphcY4_inv4_v128b_pd(c,r) result(y4)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)         
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY4_inv4_v128b_pd
            !dir$ attributes forceinline :: SphcY4_inv4_v128b_pd
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY4_inv4_v128b_pd
#endif
            type(XMM2c8),    intent(in) :: c
            type(XMM2r8_t),  intent(in) :: r
            type(XMM2c8)  ::               y4
            type(XMM2r8_t),  parameter :: C0442532692444982632759207708156 = &
                                  XMMr4_t(0.442532692444982632759207708156_dp)
            type(XMM2c8),    automatic :: powc4
            type(XMM2c8),    automatic :: rat
            type(XMM2r8_t),  automatic :: powr4
            !dir$ attributes align : 16 :: C0442532692444982632759207708156
            !dir$ attributes align : 16 :: powc4
            !dir$ attributes align : 16 :: rat
            !dir$ attributes align : 16 :: powr4
            pow4r = r.v*r.v*r.v*r.v
            powc4 = cpow_xmm4c4(c,4.0_dp)
            rat   = powc4/powr4
            y4    = rat*C0442532692444982632759207708156
     end function SphcY4_inv4_v128b_pd
     
     pure function SphcY4_inv3_v128b_pd(c,z,r) result(y43)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)          
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY4_inv3_v128b_pd
            !dir$ attributes forceinline :: SphcY4_inv3_v128b_pd
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY4_inv3_v128b_pd   
#endif       
            type(XMM2c8),    intent(in) :: c
            type(XMM2r8_t),  intent(in) :: z
            type(XMM2r8_t),  intent(in) :: r
            type(XMM2c8)  ::               y43
            type(XMM2r8_t),  parameter  :: C125167147089835226917328104721 = &
                                 XMM2r8_t(1.25167147089835226917328104721_dp)
            type(XMM2c8),    automatic :: powc3
            type(XMM2c8),    automatic :: ct0
            type(XMM2c8),    automatic :: rat
            type(XMM2r8_t),  automatic :: powr4
            !dir$ attributes align : 16 :: C125167147089835226917328104721
            !dir$ attributes align : 16 :: powc3
            !dir$ attributes align : 16 :: ct0
            !dir$ attributes align : 16 :: rat
            !dir$ attributes align : 16 :: powr4
            powr4.v = r.v*r.v*r.v*r.v
            powc3   = cpow_xmm4c4(c,3.0_dp)
            ct0     = powc3*z
            rat     = ct0/powr4
            y43     = rat*C125167147089835226917328104721
     end function SphcY4_inv3_v128b_pd
     
     pure function SphcY4_inv2_v128b_pd(c,z,r) result(y42)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)          
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY4_inv2_v128b_pd
            !dir$ attributes forceinline :: SphcY4_inv2_v128b_pd
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY4_inv2_v128b_pd   
#endif            
            type(XMM2c8),    intent(in) :: c
            type(XMM2r8_t),  intent(in) :: z
            type(XMM2r8_t),  intent(in) :: r
            type(XMM2c8)  ::               y42
            type(XMM2r8_t),  parameter :: C0334523271778644583976056194856 = &
                                XMM2r8_t(0.334523271778644583976056194856_dp)
            type(XMM2r8_t),  parameter :: C7 = XMM2r8_t(7.0_dp)
            type(XMM2c8),    automatic :: powc2
            type(XMM2c8),    automatic :: ct0
            type(XMM2c8),    automatic :: rat
            type(XMM2r8_t),  automatic :: zz
            type(XMM2r8_t),  automatic :: rr
            type(XMM2r8_t),  automatic :: powr4
            type(XMM2r8_t),  automatic :: t0
            type(XMM2r8_t),  automatic :: t1
            !dir$ attributes align : 16 :: C0334523271778644583976056194856
            !dir$ attributes align : 16 :: C7
            !dir$ attributes align : 16 :: powc2
            !dir$ attributes align : 16 :: ct0
            !dir$ attributes align : 16 :: rat
            !dir$ attributes align : 16 :: zz
            !dir$ attributes align : 16 :: rr
            !dir$ attributes align : 16 :: t0
            !dir$ attributes align : 16 :: t1
            rr.v    = r.v*r.v
            powr4.v = rr.v*rr.v
            zz.v    = z.v*z.v
            powc2   = c*c
            t0      = C7.v*zz.v-rr.v
            ct0     = powc2*t0
            rat     = ct0/powr4
            y42     = rat*C0334523271778644583976056194856 
     end function SphcY4_inv2_v128b_pd
     
     pure function SphcY4_inv1_v128b_pd(c,z,r) result(y41)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)          
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY4_inv1_v128b_pd
            !dir$ attributes forceinline :: SphcY4_inv1_v128b_pd
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY4_inv1_v128b_pd   
#endif            
            type(XMM2c8),    intent(in) :: c
            type(XMM2r8_t),  intent(in) :: z
            type(XMM2r8_t),  intent(in) :: r
            type(XMM2c8)  ::               y41
            type(XMM2r8_t),  parameter :: C0473087347878780009046340535444 = &
                                XMM2r8_t(0.473087347878780009046340535444_dp)
            type(XMM2r8_t),  parameter :: C7 = XMM2r8_t(7.0_dp)
            type(XMM2r8_t),  parameter :: C3 = XMM2r8_t(3.0_dp)
            type(XMM2c8),    automatic :: ct0
            type(XMM2c8),    automatic :: rat
            type(XMM2r8_t),  automatic :: zzz
            type(XMM2r8_t),  automatic :: rr
            type(XMM2r8_t),  automatic :: powr4
            type(XMM2r8_t),  automatic :: t0
            type(XMM2r8_t),  automatic :: t1
            type(XMM2r8_t),  automatic :: t2
            !dir$ attributes align : 16 :: C0473087347878780009046340535444
            !dir$ attributes align : 16 :: C7
            !dir$ attributes align : 16 :: C2
            !dir$ attributes align : 16 :: ct0
            !dir$ attributes align : 16 :: rat
            !dir$ attributes align : 16 :: zzz
            !dir$ attributes align : 16 :: rr
            !dir$ attributes align : 16 :: t0
            !dir$ attributes align : 16 :: t1
            !dir$ attributes align : 16 :: t2
            zzz.v    = z.v*z.v*z.v
            rr.v     = r.v*r.v
            t0.v     = C3.v*z.v*rr.v
            powr4.v  = rr.v*rr.v
            t1.v     = C7.v*zzz.v
            t2.v     = t1.v-t0.v
            ct0      = c*t2.v
            rat      = ct0/powr4
            y41      = rat*C0473087347878780009046340535444
    end function SphcY4_inv1_v128b_pd 
    
    pure function SphcY4_0_v128b_pd(z,r) result(y40)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)          
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY4_0_v128b_pd
            !dir$ attributes forceinline :: SphcY4_0_v128b_pd
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY4_0_v128b_pd   
#endif           
            type(XMM2r8_t),  intent(in) :: z
            type(XMM2r8_t),  intent(in) :: r
            type(XMM2r8_t)              :: y41   
            type(XMM2r8_t),  parameter  :: C0105785546915204303802764897168 = &
                                XMM2r8_t(0.105785546915204303802764897168_dp)
            type(XMM2r8_t),  parameter  :: C35 = XMM2r8_t(35.0_dp)
            type(XMM2r8_t),  parameter  :: C30 = XMM2r8_t(30.0_dp)
            type(XMM2r8_t),  parameter  :: C3  = XMM2r8_t(3.0_dp)
            type(XMM2r8_t),  automatic  :: powr4
            type(XMM2r8_t),  automatic  :: powz4
            type(XMM2r8_t),  automatic  :: zz
            type(XMM2r8_t),  automatic  :: rr 
            type(XMM2r8_t),  automatic  :: t0
            type(XMM2r8_t),  automatic  :: t1
            type(XMM2r8_t),  automatic  :: rat
            !dir$ attributes align : 16 :: C0105785546915204303802764897168
            !dir$ attributes align : 16 :: C35
            !dir$ attributes align : 16 :: C30
            !dir$ attributes align : 16 :: C3
            !dir$ attributes align : 16 :: powr4
            !dir$ attributes align : 16 :: powzr
            !dir$ attributes align : 16 :: zz
            !dir$ attributes align : 16 :: rr
            !dir$ attributes align : 16 :: t0
            !dir$ attributes align : 16 :: t1
            !dir$ attributes align : 16 :: rat
            zz.v    = z.v*z.v
            rr.v    = r.v*r.v
            powr4.v = rr.v*rr.v
            powz4.v = zz.v*zz.v
            t0.v    = C35.v*z-C30.v*zz.v*rr.v
            t1.v    = C3.v*powr4.v
            rat.v   = (t0.v+t1.v)/powr4.v
            y40.v   = rat.v*C0105785546915204303802764897168.v
    end function SphcY4_0_v128b_pd
    
    pure function SphycY4_1_v128b_pd(c,z,r) result(y41)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)          
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY4_1_v128b_pd
            !dir$ attributes forceinline :: SphcY4_1_v128b_pd
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY4_1_v128b_pd   
#endif            
            type(XMM2c8),    intent(in) :: c
            type(XMM2r8_t),  intent(in) :: z
            type(XMM2r8_t),  intent(in) :: r
            type(XMM2c8)  ::               y41
            type(XMM2r8_t),  parameter :: C0473087347878780009046340535444 = &
                                XMM2r8_t(-0.473087347878780009046340535444_dp)
            type(XMM2r8_t),  parameter :: C7 = XMM2r8_t(7.0_dp)
            type(XMM2r8_t),  parameter :: C3 = XMM2r8_t(3.0_dp)
            type(XMM2c8),    automatic :: ct0
            type(XMM2c8),    automatic :: rat
            type(XMM2r8_t),  automatic :: zzz
            type(XMM2r8_t),  automatic :: rr
            type(XMM2r8_t),  automatic :: powr4
            type(XMM2r8_t),  automatic :: t0
            type(XMM2r8_t),  automatic :: t1
            type(XMM2r8_t),  automatic :: t2
            !dir$ attributes align : 16 :: C0473087347878780009046340535444
            !dir$ attributes align : 16 :: C7
            !dir$ attributes align : 16 :: C2
            !dir$ attributes align : 16 :: ct0
            !dir$ attributes align : 16 :: rat
            !dir$ attributes align : 16 :: zzz
            !dir$ attributes align : 16 :: rr
            !dir$ attributes align : 16 :: t0
            !dir$ attributes align : 16 :: t1
            !dir$ attributes align : 16 :: t2
            zzz.v    = z.v*z.v*z.v
            rr.v     = r.v*r.v
            t0.v     = C3.v*z.v*rr.v
            powr4.v  = rr.v*rr.v
            t1.v     = C7.v*zzz.v
            t2.v     = t1.v-t0.v
            ct0      = c*t2.v
            rat      = ct0/powr4
            y41      = rat*C0473087347878780009046340535444
    end function SphycY4_1_v128b_pd
    
    pure function SphcY4_2_v128b_pd(c,z,r) result(y42)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)          
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY4_2_v128b_pd
            !dir$ attributes forceinline :: SphcY4_2_v128b_pd
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY4_2_v128b_pd   
#endif            
            type(XMM2c8),    intent(in) :: c
            type(XMM2r8_t),  intent(in) :: z
            type(XMM2r8_t),  intent(in) :: r
            type(XMM2c8)  ::               y42
            y42 = SphycY4_inv2_v128b_pd(c,z,r)
     end function SphcY4_2_v128b_pd
     
     pure function SphcY4_3_v128b_pd(c,z,r) result(y43)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)          
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY4_3_v128b_pd
            !dir$ attributes forceinline :: SphcY4_3_v128b_pd
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY4_3_v128b_pd   
#endif       
            type(XMM2c8),    intent(in) :: c
            type(XMM2r8_t),  intent(in) :: z
            type(XMM2r8_t),  intent(in) :: r
            type(XMM2c8)  ::               y43
            type(XMM2r8_t),  parameter  :: C125167147089835226917328104721 = &
                                 XMM2r8_t(-1.25167147089835226917328104721_dp)
            type(XMM2c8),    automatic :: powc3
            type(XMM2c8),    automatic :: ct0
            type(XMM2c8),    automatic :: rat
            type(XMM2r8_t),  automatic :: powr4
            !dir$ attributes align : 16 :: C125167147089835226917328104721
            !dir$ attributes align : 16 :: powc3
            !dir$ attributes align : 16 :: ct0
            !dir$ attributes align : 16 :: rat
            !dir$ attributes align : 16 :: powr4
            powr4.v = r.v*r.v*r.v*r.v
            powc3   = cpow_xmm4c4(c,3.0_dp)
            ct0     = powc3*z
            rat     = ct0/powr4
            y43     = rat*C125167147089835226917328104721
     end function SphcY4_3_v128b_pd
     
     pure function SphcY4_4_v128b_pd(c,r) result(y44)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)         
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY4_4_v128b_pd
            !dir$ attributes forceinline :: SphcY4_4_v128b_pd
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY4_4_v128b_pd
#endif
            type(XMM2c8),    intent(in) :: c
            type(XMM2r8_t),  intent(in) :: r
            type(XMM2c8)  ::               y44 
            y44 = SphcY4_inv4_v128b_pd(c,r)    
     end function SphcY4_4_v128b_pd
     
     
     ! Degree l=5
     
     pure function SphcY5_inv5_v128b_pd(tht,phi) result(y5i5)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)          
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY5_inv5_v128b_pd
            !dir$ attributes forceinline :: SphcY5_inv5_v128b_pd
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY5_inv5_v128b_pd  
#endif       
            type(XMM2r8_t),   intent(in) :: tht
            type(XMM2r8_t),   intent(in) :: phi
            type(XMM2c8)                 :: y5i5
            type(XMM2r8_t),   parameter  :: C0464132203440858160657998605534 = &
                                                  XMM2r8_t(0.464132203440858160657998605534_dp)
            type(XMM2r8_t),   parameter  :: C5  = XMM2r8_t(-5.0_dp)
            type(XMM2c8),     automatic  :: carg
            type(XMM2c8),     automatic  :: cexp
            type(XMM2c8),     automatic  :: ct0
            type(XMM2r8_t),   automatic  :: sint
            type(XMM2r8_t),   automatic  :: psin2
            type(XMM2r8_t),   automatic  :: psin5
            !dir$ attributes align : 16 :: C0464132203440858160657998605534
            !dir$ attributes align : 16 :: C5
            !dir$ attributes align : 16 :: carg
            !dir$ attributes align : 16 :: ct0
            !dir$ attributes align : 16 :: cexp
            !dir$ attributes align : 16 :: sint
            !dir$ attributes align : 16 :: psin2
            !dir$ attributes align : 16 :: psin5
            ct0    = I*C5
            carg   = ct0*phi
            sint.v = sin(tht.v)
            psin2.v= sint.v*sint.v
            cexp   = cexp_xmm2c8(carg)
            psin5.v= psin2.v*psin2.v*sint.v
            ct0    = cexp*C0464132203440858160657998605534
            y5i5   = ct0*psin5
     end function SphcY5_inv5_v128b_pd
     
      pure function SphcY5_inv4_v128b_pd(tht,phi) result(y5i4)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)          
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY5_inv4_v128b_pd
            !dir$ attributes forceinline :: SphcY5_inv4_v128b_pd
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY5_inv4_v128b_pd  
#endif       
            type(XMM2r8_t),   intent(in) :: tht
            type(XMM2r8_t),   intent(in) :: phi
            type(XMM2c8)                 :: y5i4
            type(XMM2r8_t),   parameter  :: C1467714898305751163052026147529 = &
                                                 XMM2r8_t(1.467714898305751163052026147529_dp)
            type(XMM2r8_t),   parameter  :: C4 = XMM2r8_t(-4.0_dp)       
            type(XMM2c8),     automatic  :: carg
            type(XMM2c8),     automatic  :: cexp
            type(XMM2c8),     automatic  :: ct0
            type(XMM2r8_t),   automatic  :: sint
            type(XMM2r8_t),   automatic  :: cost
            type(XMM2r8_t),   automatic  :: psin4
            !dir$ attributes align : 16 ::  C1467714898305751163052026147529
            !dir$ attributes align : 16 ::  C4
            !dir$ attributes align : 16 ::  carg
            !dir$ attributes align : 16 ::  cexp
            !dir$ attributes align : 16 ::  ct0
            !dir$ attributes align : 16 ::  sint
            !dir$ attributes align : 16 ::  cost
            !dir$ attributes align : 16 ::  psin4
            ct0    =  I*C4
            sint.v =  sin(tht.v)
            carg   =  ct0*phi
            cost.v =  cos(tht.v)
            psin4.v=  sint.v*sint.v*sint.v*sint.v
            cexp   =  cexp_xmm2c8(carg)
            psin4.v=  psin4.v*cost.v
            ct0    =  cexp*psin4
            y5i4   =  ct0*C1467714898305751163052026147529    
      end function SphcY5_inv4_v128b_pd
      
      pure function SphcY5_inv3_v128b_pd(tht,phi) result(y5i3)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)          
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY5_inv3_v128b_pd
            !dir$ attributes forceinline :: SphcY5_inv3_v128b_pd
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY5_inv3_v128b_pd  
#endif       
            type(XMM2r8_t),   intent(in) :: tht
            type(XMM2r8_t),   intent(in) :: phi
            type(XMM2c8)                 :: y5i3
            type(XMM2r8_t),   parameter  :: C0345943719146840213165966420433 = &
                                              XMM2r8_t(0.345943719146840213165966420433_dp)
            type(XMM2r8_t),   parameter  :: C9 = XMM2r8_t(9.0_dp)
            type(XMM2r8_t),   parameter  :: C1 = XMM2r8_t(1.0_dp)
            type(XMM2r8_t),   parameter  :: C3 = XMM2r8_t(-3.0_dp)
            type(XMM2c8),     automatic  :: carg
            type(XMM2c8),     automatic  :: cexp
            type(XMM2c8),     automatic  :: ct0
            type(XMM2r8_t),   automatic  :: sint
            type(XMM2r8_t),   automatic  :: psin3
            type(XMM2r8_t),   automatic  :: cost
            type(XMM2r8_t),   automatic  :: t0
            !dir$ attributes align : 16 :: C0345943719146840213165966420433
            !dir$ attributes align : 16 :: C9
            !dir$ attributes align : 16 :: C1
            !dir$ attributes align : 16 :: carg
            !dir$ attributes align : 16 :: cexp
            !dir$ attributes align : 16 :: ct0
            !dir$ attributes align : 16 :: sint
            !dir$ attributes align : 16 :: psin3
            !dir$ attributes align : 16 :: cost
            ct0    = I*C3
            carg   = ct0*phi
            sint.v = sin(tht.v)
            cexp   = cexp_xmm2c8(carg)
            cost.v = cos(tht.v)
            t0.v   = (C9.v*cost.v*cost.v)-C1.v
            psin3.v= sint.v*sint.v*sint.v
            ct0    = cexp*C0345943719146840213165966420433
            t0.v   = t0.v*psin3.v
            y5i3   = ct0*t0
      end function SphcY5_inv3_v128_pd
      
      pure function SphcY5_inv2_v128b_pd(tht,phi) result(y5i2)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)          
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY5_inv2_v128b_pd
            !dir$ attributes forceinline :: SphcY5_inv2_v128b_pd
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY5_inv2_v128b_pd  
#endif       
            type(XMM2r8_t),   intent(in) :: tht
            type(XMM2r8_t),   intent(in) :: phi
            type(XMM2c8)                 :: y5i2
            type(XMM2r8_t),   parameter  :: C1694771183260899275815691555511 = 
                                            XMM2r8_t(1.694771183260899275815691555511_dp)
            type(XMM2r8_t),   parameter  :: C2 = XMM2r8_t(-2.0_dp)
            type(XMM2r8_t),   parameter  :: C3 = XMM2r8_t(3.0_dp) 
            type(XMM2c8),     automatic  :: carg
            type(XMM2c8),     automatic  :: cexp
            type(XMM2c8),     automatic  :: ct0
            type(XMM2r8_t),   automatic  :: sint
            type(XMM2r8_t),   automatic  :: psin
            type(XMM2r8_t),   automatic  :: cost
            type(XMM2r8_t),   automatic  :: pcos3
            type(XMM2r8_t),   automatic  :: t0
             !dir$ attributes align : 16 :: C1694771183260899275815691555511
             !dir$ attributes align : 16 :: C2
             !dir$ attributes align : 16 :: C3
             !dir$ attributes align : 16 :: carg
             !dir$ attributes align : 16 :: cexp
             !dir$ attributes align : 16 :: ct0
             !dir$ attributes align : 16 :: sint
             !dir$ attributes align : 16 :: psin
             !dir$ attributes align : 16 :: cost
             !dir$ attributes align : 16 :: pcos3
             !dir$ attributes align : 16 :: t0
            ct0    = I*C2
            cost.v = cos(tht.v)
            carg   = ct0*phi
            pcos3.v= cost.v*cost.v*cost.v
            t0.v   = C3.v*pcos3.v-cost.v
            cexp   = cexp_xmm2c8(carg)
            sint.v = sin(tht.v)
            t0.v   = sint.v*sint.v*t0.v
            ct0    = cexp*C1694771183260899275815691555511
            y5i2   = ct0*t0
      end function SphcY5_inv2_v128b_pd
     
      pure function SphcY5_inv1_v128b_pd(theta,phi) result(y5i1)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)          
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY5_inv1_v128b_pd
            !dir$ attributes forceinline :: SphcY5_inv1_v128b_pd
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY5_inv1_v128b_pd  
#endif       
            type(XMM2r8_t),   intent(in) :: theta
            type(XMM2r8_t),   intent(in) :: phi
            type(XMM2c8)                 :: y5i1
            type(XMM2r8_t),   parameter  :: C0320281648576215127537161432847 = 
                                              XMM2r8_t(0.320281648576215127537161432847_dp)
            type(XMM2r8_t),   parameter  :: C21 = XMM2r8_t(21.0_dp)
            type(XMM2r8_t),   parameter  :: C14 = XMM2r8_t(14.0_dp)
            type(XMM2r8_t),   parameter  :: C1  = XMM2r8_t(1.0_dp)
            type(XMM2r8_t),   parameter  :: CN1 = XMM2r8_t(-1.0_dp)
            type(XMM2c8),     automatic  :: carg
            type(XMM2c8),     automatic  :: cexp
            type(XMM2c8),     automatic  :: ct0
            type(XMM2r8_t),   automatic  :: sint
            type(XMM2r8_t),   automatic  :: cost
            type(XMM2r8_t),   automatic  :: pcos4
            type(XMM2r8_t),   automatic  :: t0
            type(XMM2r8_t),   automatic  :: t1
            !dir$ attributes align : 16 :: C0320281648576215127537161432847
            !dir$ attributes align : 16 :: C14
            !dir$ attributes align : 16 :: C21
            !dir$ attributes align : 16 :: C1
            !dir$ attributes align : 16 :: carg
            !dir$ attributes align : 16 :: cexp
            !dir$ attributes align : 16 :: ct0
            !dir$ attributes align : 16 :: sint
            !dir$ attributes align : 16 :: cost
            !dir$ attributes align : 16 :: pcos4
            !dir$ attributes align : 16 :: t0
            !dir$ attributes align : 16 :: t1
            !dir$ attributes align : 16 :: t2
            ct0    = I*CN1
            sint.v = sin(tht.v)
            carg   = ct0*phi
            cost.v = cos(tht.v)
            cexp   = cexp_xmm2c8(carg)
            t0.v   = (C14*cost.v*cost.v)-C1.V
            t1.v   = C21.v*cost.v*cost.v*cost.v*cost.v
            t2.v   = sint.v*(t0.v-t1.v)
            ct0    = cexp*C0320281648576215127537161432847
            y5i1   = ct0*t2
      end function SphcY5_inv1_v128b_pd
      
      pure function SphcY5_0_v128b_pd(theta,phi) result(y50)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)          
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY5_0_v128b_pd
            !dir$ attributes forceinline :: SphcY5_0_v128b_pd
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY5_0_v128b_pd  
#endif       
            type(XMM2r8_t),   intent(in) :: theta
            type(XMM2r8_t),   intent(in) :: phi
            type(XMM2c8)                 :: y50
            type(XMM2r8_t),   parameter  :: C011695032245342359643971519209 = &
                                                   XMM2r8_t(0.11695032245342359643971519209_dp)
            type(XMM2r8_t),   parameter  :: C63 =  XMM2r8_t(63.0_dp)
            type(XMM2r8_t),   parameter  :: C70 =  XMM2r8_t(70.0_dp)
            type(XMM2r8_t),   parameter  :: C15 =  XMM2r8_t(15.0_dp)
            type(XMM2r8_t),   automatic  :: cost
            type(XMM2r8_t),   automatic  :: costp2
            type(XMM2r8_t),   automatic  :: t0
            type(XMM2r8_t),   automatic  :: t1
            type(XMM2r8_t),   automatic  :: t2
            type(XMM2r8_t),   automatic  :: t3
            !dir$ attributes align : 16 :: C011695032245342359643971519209
            !dir$ attributes align : 16 :: C63
            !dir$ attributes align : 16 :: C70
            !dir$ attributes align : 16 :: C15
            !dir$ attributes align : 16 :: cost
            !dir$ attributes align : 16 :: costp2
            !dir$ attributes align : 16 :: t0
            !dir$ attributes align : 16 :: t1
            !dir$ attributes align : 16 :: t2
            !dir$ attributes align : 16 :: t3
            cost.v  = cos(theta.v)
            t0.v    = C15.v*cost.v
            costp2.v= cost.v*cost.v
            t1.v    = C70.v*(costp2.v*cost.v)
            t2.v    = C63.v*(costp2.v*costp2.v*cost.v)
            t3.v    = t2.v-t1.v+t0.v
            y50.v   = C011695032245342359643971519209.v*t3.v
      end function SphcY5_0_v128b_pd
      
      pure function SphcY5_1_v128b_pd(theta,phi) result(y51)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)          
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY5_1_v128b_pd
            !dir$ attributes forceinline :: SphcY5_1_v128b_pd
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY5_1_v128b_pd  
#endif       
            type(XMM2r8_t),   intent(in) :: theta
            type(XMM2r8_t),   intent(in) :: phi
            type(XMM2c8)                 :: y5i1
            type(XMM2r8_t),   parameter  :: C0320281648576215127537161432847 = 
                                              XMM2r8_t(0.320281648576215127537161432847_dp)
            type(XMM2r8_t),   parameter  :: C21 = XMM2r8_t(21.0_dp)
            type(XMM2r8_t),   parameter  :: C14 = XMM2r8_t(14.0_dp)
            type(XMM2r8_t),   parameter  :: C1  = XMM2r8_t(1.0_dp)
            type(XMM2r8_t),   parameter  :: CN1 = XMM2r8_t(-1.0_dp)
            type(XMM2c8),     automatic  :: carg
            type(XMM2c8),     automatic  :: cexp
            type(XMM2c8),     automatic  :: ct0
            type(XMM2r8_t),   automatic  :: sint
            type(XMM2r8_t),   automatic  :: cost
            type(XMM2r8_t),   automatic  :: pcos4
            type(XMM2r8_t),   automatic  :: t0
            type(XMM2r8_t),   automatic  :: t1
            !dir$ attributes align : 16 :: C0320281648576215127537161432847
            !dir$ attributes align : 16 :: C14
            !dir$ attributes align : 16 :: C21
            !dir$ attributes align : 16 :: C1
            !dir$ attributes align : 16 :: carg
            !dir$ attributes align : 16 :: cexp
            !dir$ attributes align : 16 :: ct0
            !dir$ attributes align : 16 :: sint
            !dir$ attributes align : 16 :: cost
            !dir$ attributes align : 16 :: pcos4
            !dir$ attributes align : 16 :: t0
            !dir$ attributes align : 16 :: t1
            !dir$ attributes align : 16 :: t2
            ct0    = I
            sint.v = sin(tht.v)
            carg   = ct0*phi
            cost.v = cos(tht.v)
            cexp   = cexp_xmm2c8(carg)
            t0.v   = (C14*cost.v*cost.v)-C1.V
            t1.v   = C21.v*cost.v*cost.v*cost.v*cost.v
            t2.v   = sint.v*(t0.v-t1.v)
            ct0    = cexp*C0320281648576215127537161432847
            y51   = ct0*t2
      end function SphcY5_1_v128b_pd
      
      pure function SphcY5_2_v128b_pd(tht,phi) result(y52)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)          
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY5_2_v128b_pd
            !dir$ attributes forceinline :: SphcY5_2_v128b_pd
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY5_2_v128b_pd  
#endif       
            type(XMM2r8_t),   intent(in) :: tht
            type(XMM2r8_t),   intent(in) :: phi
            type(XMM2c8)                 :: y5i2
            type(XMM2r8_t),   parameter  :: C1694771183260899275815691555511 = 
                                            XMM2r8_t(1.694771183260899275815691555511_dp)
            type(XMM2r8_t),   parameter  :: C2 = XMM2r8_t(-2.0_dp)
            type(XMM2r8_t),   parameter  :: C3 = XMM2r8_t(3.0_dp) 
            type(XMM2c8),     automatic  :: carg
            type(XMM2c8),     automatic  :: cexp
            type(XMM2c8),     automatic  :: ct0
            type(XMM2r8_t),   automatic  :: sint
            type(XMM2r8_t),   automatic  :: psin
            type(XMM2r8_t),   automatic  :: cost
            type(XMM2r8_t),   automatic  :: pcos3
            type(XMM2r8_t),   automatic  :: t0
             !dir$ attributes align : 16 :: C1694771183260899275815691555511
             !dir$ attributes align : 16 :: C2
             !dir$ attributes align : 16 :: C3
             !dir$ attributes align : 16 :: carg
             !dir$ attributes align : 16 :: cexp
             !dir$ attributes align : 16 :: ct0
             !dir$ attributes align : 16 :: sint
             !dir$ attributes align : 16 :: psin
             !dir$ attributes align : 16 :: cost
             !dir$ attributes align : 16 :: pcos3
             !dir$ attributes align : 16 :: t0
            ct0    = I
            cost.v = cos(tht.v)
            carg   = ct0*phi
            pcos3.v= cost.v*cost.v*cost.v
            t0.v   = C3.v*pcos3.v-cost.v
            cexp   = cexp_xmm2c8(carg)
            sint.v = sin(tht.v)
            t0.v   = sint.v*sint.v*t0.v
            ct0    = cexp*C1694771183260899275815691555511
            y52   = ct0*t0
      end function SphcY5_2_v128b_pd
     
      pure function SphcY5_3_v128b_pd(tht,phi) result(y53)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)          
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY5_i3_v128b_pd
            !dir$ attributes forceinline :: SphcY5_i3_v128b_pd
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY5_i3_v128b_pd  
#endif       
            type(XMM2r8_t),   intent(in) :: tht
            type(XMM2r8_t),   intent(in) :: phi
            type(XMM2c8)                 :: y53
            type(XMM2r8_t),   parameter  :: C0345943719146840213165966420433 = &
                                              XMM2r8_t(0.345943719146840213165966420433_dp)
            type(XMM2r8_t),   parameter  :: C9 = XMM2r8_t(9.0_dp)
            type(XMM2r8_t),   parameter  :: C1 = XMM2r8_t(1.0_dp)
            type(XMM2r8_t),   parameter  :: C3 = XMM2r8_t(-3.0_dp)
            type(XMM2c8),     automatic  :: carg
            type(XMM2c8),     automatic  :: cexp
            type(XMM2c8),     automatic  :: ct0
            type(XMM2r8_t),   automatic  :: sint
            type(XMM2r8_t),   automatic  :: psin3
            type(XMM2r8_t),   automatic  :: cost
            type(XMM2r8_t),   automatic  :: t0
            !dir$ attributes align : 16 :: C0345943719146840213165966420433
            !dir$ attributes align : 16 :: C9
            !dir$ attributes align : 16 :: C1
            !dir$ attributes align : 16 :: carg
            !dir$ attributes align : 16 :: cexp
            !dir$ attributes align : 16 :: ct0
            !dir$ attributes align : 16 :: sint
            !dir$ attributes align : 16 :: psin3
            !dir$ attributes align : 16 :: cost
            ct0    = I
            carg   = ct0*phi
            sint.v = sin(tht.v)
            cexp   = cexp_xmm2c8(carg)
            cost.v = cos(tht.v)
            t0.v   = (C9.v*cost.v*cost.v)-C1.v
            psin3.v= sint.v*sint.v*sint.v
            ct0    = cexp*C0345943719146840213165966420433
            t0.v   = t0.v*psin3.v
            y53   = ct0*t0
      end function SphcY5_3_v128_pd
      
      pure function SphcY5_4_v128b_pd(tht,phi) result(y54)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)          
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY5_4_v128b_pd
            !dir$ attributes forceinline :: SphcY5_4_v128b_pd
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY5_4_v128b_pd  
#endif       
            type(XMM2r8_t),   intent(in) :: tht
            type(XMM2r8_t),   intent(in) :: phi
            type(XMM2c8)                 :: y5i4
            type(XMM2r8_t),   parameter  :: C1467714898305751163052026147529 = &
                                                 XMM2r8_t(1.467714898305751163052026147529_dp)
            type(XMM2r8_t),   parameter  :: C4 = XMM2r8_t(-4.0_dp)       
            type(XMM2c8),     automatic  :: carg
            type(XMM2c8),     automatic  :: cexp
            type(XMM2c8),     automatic  :: ct0
            type(XMM2r8_t),   automatic  :: sint
            type(XMM2r8_t),   automatic  :: cost
            type(XMM2r8_t),   automatic  :: psin4
            !dir$ attributes align : 16 ::  C1467714898305751163052026147529
            !dir$ attributes align : 16 ::  C4
            !dir$ attributes align : 16 ::  carg
            !dir$ attributes align : 16 ::  cexp
            !dir$ attributes align : 16 ::  ct0
            !dir$ attributes align : 16 ::  sint
            !dir$ attributes align : 16 ::  cost
            !dir$ attributes align : 16 ::  psin4
            ct0    =  I
            sint.v =  sin(tht.v)
            carg   =  ct0*phi
            cost.v =  cos(tht.v)
            psin4.v=  sint.v*sint.v*sint.v*sint.v
            cexp   =  cexp_xmm2c8(carg)
            psin4.v=  psin4.v*cost.v
            ct0    =  cexp*psin4
            y54   =  ct0*C1467714898305751163052026147529    
      end function SphcY5_4_v128b_pd
      
      pure function SphcY5_5_v128b_pd(tht,phi) result(y55)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)          
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY5_5_v128b_pd
            !dir$ attributes forceinline :: SphcY5_5_v128b_pd
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY5_5_v128b_pd  
#endif       
            type(XMM2r8_t),   intent(in) :: tht
            type(XMM2r8_t),   intent(in) :: phi
            type(XMM2c8)                 :: y55
            type(XMM2r8_t),   parameter  :: C0464132203440858160657998605534 = &
                                                  XMM2r8_t(0.464132203440858160657998605534_dp)
            type(XMM2c8),     automatic  :: carg
            type(XMM2c8),     automatic  :: cexp
            type(XMM2c8),     automatic  :: ct0
            type(XMM2r8_t),   automatic  :: sint
            type(XMM2r8_t),   automatic  :: psin2
            type(XMM2r8_t),   automatic  :: psin5
            !dir$ attributes align : 16 :: C0464132203440858160657998605534
            !dir$ attributes align : 16 :: C5
            !dir$ attributes align : 16 :: carg
            !dir$ attributes align : 16 :: ct0
            !dir$ attributes align : 16 :: cexp
            !dir$ attributes align : 16 :: sint
            !dir$ attributes align : 16 :: psin2
            !dir$ attributes align : 16 :: psin5
            ct0    = I
            carg   = ct0*phi
            sint.v = sin(tht.v)
            psin2.v= sint.v*sint.v
            cexp   = cexp_xmm2c8(carg)
            psin5.v= psin2.v*psin2.v*sint.v
            ct0    = cexp*C0464132203440858160657998605534
            y55   = ct0*psin5
     end function SphcY5_5_v128b_pd
     
      ! Degree l=6
      
     pure function SphcY6_inv6_v128b_pd(tht,phi) result(y6i6)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)          
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY6_inv6_v128b_pd
            !dir$ attributes forceinline :: SphcY6_inv6_v128b_pd
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY6_inv6_v128b_pd  
#endif       
            type(XMM2r8_t),   intent(in) :: tht
            type(XMM2r8_t),   intent(in) :: phi
            type(XMM2c8)                 :: y6i6
            type(XMM2r8_t),   parameter  :: C0483084113580066226972961123235 = &
                                                 XMM2r8_t(0.483084113580066226972961123235_dp)
            type(XMM2r8_t),   parameter  :: CN6  = XMM2r8_t(-6.0_dp)
            type(XMM2c8),     automatic  :: carg
            type(XMM2c8),     automatic  :: cexp
            type(XMM2c8),     automatic  :: ct0
            type(XMM2r8_t),   automatic  :: sint
            type(XMM2r8_t),   automatic  :: sintp2
            type(XMM2r8_t),   automatic  :: sintp6
            !dir$ attributes align : 16 :: C0483084113580066226972961123235 
            !dir$ attributes align : 16 :: CN6
            !dir$ attributes align : 16 :: carg
            !dir$ attributes align : 16 :: cexp
            !dir$ attributes align : 16 :: ct0
            !dir$ attributes align : 16 :: sint
            !dir$ attributes align : 16 :: sintp2
            !dir$ attributes align : 16 :: sintp6
            ct0     = I*CN6
            carg    = ct0*phi
            sint.v  = sin(tht.v)
            sintp2.v= sint.v*sint.v
            cexp    = cexp_xmm2c8(carg)
            sintp6.v= sintp2.v*sintp2.v*sintp2.v
            ct0     = cexp*C0483084113580066226972961123235 
            y6i6    = ct0*sintp6
     end function SphcY6_inv6_v128b_pd
     
     
     
      
      
end module sph_complex_xmm2r8        
           
       
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     





















































































