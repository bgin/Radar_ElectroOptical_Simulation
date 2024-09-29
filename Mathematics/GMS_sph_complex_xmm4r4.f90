
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

module sph_complex_xmm4r4


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         sph_complex_xmm4r4
 !          
 !          Purpose:
 !                       Complex Spherical Harmonics up to degree (l=10).
 !                        
 !          History:
 !                        Date: 22-09-2024
 !                        Time: 09:13 GMT+2
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
 !                      https://en.wikipedia.org/wiki/Table_of_spherical_harmonics
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
!==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.

    use mod_kinds,    only : i4,sp
    use sse_cvec4
    use mod_vectypes, only : XMM4r4_t,Mask4_t
    
    public
    implicit none
    
    
      ! Major version
     integer(kind=i4),  parameter :: SPH_COMPLEX_XMM4R4_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: SPH_COMPLEX_XMM4R4_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: SPH_COMPLEX_XMM4R4_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: SPH_COMPLEX_XMM4R4_FULLVER =   &
            1000*SPH_COMPLEX_XMM4R4_MAJOR+100*SPH_COMPLEX_XMM4R4_MINOR+10*SPH_COMPLEX_XMM4R4_MICRO
     ! Module creation date
     character(*),        parameter :: SPH_COMPLEX_XMM4R4_CREATE_DATE = "22-09-2024 09:15AM +00200 (SUN 22 SEP 2024 GMT+2)"
     ! Module build date
     character(*),        parameter :: SPH_COMPLEX_XMM4R4_BUILD_DATE  = __DATE__ " " __TIME__
     ! Module author info
     character(*),        parameter :: SPH_COMPLEX_XMM4R4_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: SPH_COMPLEX_XMM4R4_SYNOPSIS  = "Complex Spherical Harmonics up to degree (l=10)." 
     
     contains
     
     ! Degree l=0
     
     pure function SphcY0_0_v128b_ps() result(y0)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY0_0_v128b_ps
            !dir$ attributes forceinline :: SphcY0_0_v128b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY0_0_v128b_ps
#endif
            type(XMM4r4_t) :: y0
            y0 = XMM4r4_t(0.28209479177387814347403972578_sp)
     end function SphcY0_0_v128b_ps
     
     ! Degree l=1
     
     pure function SphcY1_inv1_v128b_ps(c,r) result(y1)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY1_inv1_v128b_ps
            !dir$ attributes forceinline :: SphcY1_inv1_v128b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY1_inv1_v128b_ps
#endif     
            type(XMM4c4),    intent(in) :: c
            type(XMM4r4_t),  intent(in) :: r
            type(XMM4c4) :: y1
            type(XMM4r4_t),  parameter :: C0345494149471335479265244646032 = &
                                   XMM4r4_t(0.345494149471335479265244646032_sp)
            type(XMM4c4),    automatic :: tmp
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)             
            !dir$ attributes align : 16 :: C0345494149471335479265244646032 
            !dir$ attributes align : 16 :: tmp
#endif
            tmp = c/r
            y0  = tmp*C0345494149471335479265244646032
     end function SphcY1_inv1_v128b_ps
     
     pure function SphcY1_0_v128b_ps(z,r) result(y0)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY1_0_v128b_ps
            !dir$ attributes forceinline :: SphcY1_0_v128b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY1_0_v128b_ps
#endif          
            type(XMM4r4_t),   intent(in) :: z
            type(XMM4r4_t),   intent(in) :: r
            type(XMM4r4_t) :: y0
            type(XMM4r4_t),   parameter :: C1534990061919732732719327437334 = &
                                  XMM4r4_t(1.534990061919732732719327437334_sp)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)                                   
            !dir$ attributes align : 16 :: C1534990061919732732719327437334
#endif            
            y0.v = C1534990061919732732719327437334.v*(z.v/r.v)
     end function SphcY1_0_v128b_ps
     
     pure function SphcY1_1_v128b_ps(c,r) result(y1)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY1_1_v128b_ps
            !dir$ attributes forceinline :: SphcY1_1_v128b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY1_1_v128b_ps
#endif     
            type(XMM4c4),    intent(in) :: c
            type(XMM4r4_t),  intent(in) :: r
            type(XMM4c4) :: y1
            type(XMM4r4_t),  parameter :: C0345494149471335479265244646032 = &
                                   XMM4r4_t(-0.345494149471335479265244646032_sp)
            type(XMM4c4),    automatic :: tmp
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)             
            !dir$ attributes align : 16 :: C0345494149471335479265244646032
            !dir$ attributes align : 16 :: tmp
#endif            
            tmp = c/r
            y1  = tmp*C0345494149471335479265244646032
     end function SphcY1_1_v128b_ps
     
     ! Degree l=2
     
     pure function SphcY2_inv2_v128b_ps(c,r) result(y2)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY2_inv2_v128b_ps
            !dir$ attributes forceinline :: SphcY2_inv2_v128b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY2_inv2_v128b_ps
#endif     
            type(XMM4c4),    intent(in) :: c
            type(XMM4r4_t),  intent(in) :: r
            type(XMM4c4) :: y2
            type(XMM4r4_t),  parameter :: C0386274202023189580342192735311 = &
                                    XMM4r4_t(0.386274202023189580342192735311_sp)
            type(XMM4c4),    automatic :: tmp1
            type(XMM4c4),    automatic :: csqr
            type(XMM4r4_t),  automatic :: tmp2
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)             
            !dir$ attributes align : 16 :: C0386274202023189580342192735311
            !dir$ attributes align : 16 :: tmp
            !dir$ attributes align : 16 :: csqr
#endif
            tmp2.v = r.v*r.v
            csqr   = c*c
            tmp1   = csqr/tmp2
            y2     = tmp1*C0386274202023189580342192735311
     end function SphcY2_inv2_v128b_ps


     pure function SphcY2_inv1_v128b_ps(c,z,r) result(y2)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY2_inv1_v128b_ps
            !dir$ attributes forceinline :: SphcY2_inv1_v128b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY2_inv1_v128b_ps
#endif     
            type(XMM4c4),    intent(in) :: c
            type(XMM4r4_t),  intent(in) :: z  
            type(XMM4r4_t),  intent(in) :: r
            type(XMM4c4)  :: y2
            type(XMM4r4_t),  parameter :: C0772548404046379160684385470623 = &
                                    XMM4r4_t(0.772548404046379160684385470623_sp)
            type(XMM4c4),    automatic :: tmp
            type(XMM4r4_t),  automatic :: rsqr
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)             
            !dir$ attributes align : 16 :: C0772548404046379160684385470623
            !dir$ attributes align : 16 :: tmp
            !dir$ attributes align : 16 :: rsqr
#endif
            rsqr.v = r.v*r.v
            tmp    = c*z
            tmp    = tmp/rsqr
            y2     = tmp*C0772548404046379160684385470623           
     end function SphcY2_inv1_v128b_ps
     
     pure function SphcY2_0_v128b_ps(z,r) result(y2)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY2_0_v128b_ps
            !dir$ attributes forceinline :: SphcY2_0_v128b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY2_0_v128b_ps
#endif       
            type(XMM4r4_t),   intent(in) :: z
            type(XMM4r4_t),   intent(in) :: r
            type(XMM4r4_t)  :: y2
            type(XMM4r4_t),   parameter :: C0630783130505040012061787380591 = 
                                   XMM4r4_t(0.630783130505040012061787380591_sp)
            type(XMM4r4_t),   parameter :: C3 = XMM4r4_t(3.0_sp)
            type(XMM4r4_t),   automatic :: zsqr
            type(XMM4r4_t),   automatic :: rsqr
            type(XMM4r4_t),   automatic :: diff
            type(XMM4r4_t),   automatic :: rat 
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
     end function SphcY2_0_v128b_ps
     
     pure function SphcY2_1_v128b_ps(c,z,r) result(y2)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY2_1_v128b_ps
            !dir$ attributes forceinline :: SphcY2_1_v128b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY2_1_v128b_ps
#endif     
            type(XMM4c4),    intent(in) :: c
            type(XMM4r4_t),  intent(in) :: z  
            type(XMM4r4_t),  intent(in) :: r
            type(XMM4c4)  :: y2
            type(XMM4r4_t),  parameter :: C0772548404046379160684385470623 = &
                                    XMM4r4_t(-0.772548404046379160684385470623_sp)
            type(XMM4c4),    automatic :: tmp
            type(XMM4r4_t),  automatic :: rsqr
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)             
            !dir$ attributes align : 16 :: C0772548404046379160684385470623
            !dir$ attributes align : 16 :: tmp
            !dir$ attributes align : 16 :: rsqr
#endif  
            rsqr.v = r.v*r.v
            tmp    = c*z
            tmp    = tmp/rsqr
            y2     = tmp*C0772548404046379160684385470623     
     end function SphcY2_1_v128b_ps
     
     pure function SphcY2_2_v128b_ps(c,r) result(y2)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY2_2_v128b_ps
            !dir$ attributes forceinline :: SphcY2_2_v128b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY2_2_v128b_ps
#endif     
            type(XMM4c4),    intent(in) :: c
            type(XMM4r4_t),  intent(in) :: r
            type(XMM4c4) :: y2  
            y2 = Y2_inv2_v128b_ps(c,r)     
     end function SphcY2_2_v128b_ps

     
     ! Degree l=3

     pure function SphcY3_inv1_v128b_ps(c,z,r) result(y3)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)       
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY3_inv1_v128b_ps
            !dir$ attributes forceinline :: SphcY3_inv1_v128b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY3_inv1_v128b_ps
#endif
            type(XMM4c4),    intent(in) :: c
            type(XMM4r4_t),  intent(in) :: z
            type(XMM4r4_t),  intent(in) :: r
            type(XMM4c4)  ::               y3
            type(XMM4r4_t),  parameter  :: C032318018411415065300739416333 = &
                                    XMM4r4_t(0.32318018411415065300739416333_sp)
            type(XMM4r4_t),  parameter  :: C5 = XMM4r4_t(5.0_sp)
            type(XMM4c4),    automatic :: ct1
            type(XMM4c4),    automatic :: ct2
            type(XMM4r4_t),  automatic :: zz
            type(XMM4r4_t),  automatic :: rr
            type(XMM4r4_t),  automatic :: rrr
            type(XMM4r4_t),  automatic :: t0
            
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
     end function SphcY3_inv1_v128b_ps
     
     pure function SphcY3_0_v128b_ps(z,r) result(y3)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)  
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY3_0_v128b_ps
            !dir$ attributes forceinline :: SphcY3_0_v128b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY3_0_v128b_ps
#endif
            type(XMM4r4_t),  intent(in) :: z
            type(XMM4r4_t),  intent(in) :: r
            type(XMM4r4_t)  ::             y3
            type(XMM4r4_t),  parameter :: C0373176332590115391414395913199 = &
                                   XMM4r4_t(0.373176332590115391414395913199_sp)
            type(XMM4r4_t),  parameter :: C5 = XMM4r4_t(0.5_sp)
            type(XMM4r4_t),  parameter :: C3 = XMM4r4_t(3.0_sp)
            type(XMM4r4_t),  automatic :: zzz
            type(XMM4r4_t),  automatic :: rr
            type(XMM4r4_t),  automatic :: rrr
            type(XMM4r4_t),  automatic :: t0
            type(XMM4r4_t),  automatic :: t1
            type(XMM4r4_t),  automatic :: t2
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
     end function SphcY3_0_v128b_ps
     
     pure function SphcY3_1_v128b_ps(c,z,r) result(y3)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)  
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY3_1_v128b_ps
            !dir$ attributes forceinline :: SphcY3_1_v128b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY3_1_v128b_ps
#endif
            type(XMM4c4),    intent(in) :: c
            type(XMM4r4_t),  intent(in) :: z
            type(XMM4r4_t),  intent(in) :: r
            type(XMM4c4)  ::               y3
            type(XMM4r4_t),  parameter  :: C032318018411415065300739416333 = &
                                    XMM4r4_t(-0.32318018411415065300739416333_sp)
            type(XMM4r4_t),  parameter  :: C5 = XMM4r4_t(5.0_sp)
            type(XMM4c4),    automatic :: ct1
            type(XMM4c4),    automatic :: ct2
            type(XMM4r4_t),  automatic :: zz
            type(XMM4r4_t),  automatic :: rr
            type(XMM4r4_t),  automatic :: rrr
            type(XMM4r4_t),  automatic :: t0
            
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
     end function SphcY3_1_v128b_ps
     
     pure function SphcY3_2_v128b_ps(c,z,r) result(y32)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)       
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY3_2_v128b_ps
            !dir$ attributes forceinline :: SphcY3_2_v128b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY3_2_v128b_ps
#endif
            type(XMM4c4),    intent(in) :: c
            type(XMM4r4_t),  intent(in) :: z
            type(XMM4r4_t),  intent(in) :: r
            type(XMM4c4)  ::               y32
            type(XMM4r4_t),  parameter :: C10219854764332823633961144917 = &
                                   XMM4r4_t(1.0219854764332823633961144917)
            type(XMM4c4),    automatic :: csqr
            type(XMM4c4),    automatic :: ct0
            type(XMM4c4),    automatic :: ct1
            type(XMM4r4_t),  automatic :: rrr
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
     end function SphcY3_2_v128v_ps

     pure function SphcY3_3_v128b_ps(c,r) result(y33)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)     
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY3_3_v128b_ps
            !dir$ attributes forceinline :: SphcY3_3_v128b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY3_3_v128b_ps
#endif
            type(XMM4c4),    intent(in) :: c
            type(XMM4r4_t),  intent(in) :: r
            type(XMM4c4)  ::               y33
            type(XMM4r4_t),  parameter :: C0417223823632784089724427015737 =  &
                                  XMM4r4_t(0.417223823632784089724427015737_sp)
            type(XMM4c4),    automatic :: pow3c
            type(XMM4c4),    automatic :: rat
            type(XMM4r4_t),  automatic :: rrr
            !dir$ attributes align : 16 :: C0417223823632784089724427015737
            !dir$ attributes align : 16 :: pow3c
            !dir$ attributes align : 16 :: rat
            rrr.v = r.v*r.v*r.v
            pow3c = cpow_xmm4c4(c,3.0_sp)
            rat   = pow3c/rrr
            y33   = rat*C0417223823632784089724427015737
     end function SphcY3_3_v128b_ps
     
     
     ! Degree l=4
     pure function SphcY4_inv4_v128b_ps(c,r) result(y4)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)         
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY4_inv4_v128b_ps
            !dir$ attributes forceinline :: SphcY4_inv4_v128b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY4_inv4_v128b_ps
#endif
            type(XMM4c4),    intent(in) :: c
            type(XMM4r4_t),  intent(in) :: r
            type(XMM4c4)  ::               y4
            type(XMM4r4_t),  parameter :: C0442532692444982632759207708156 = &
                                  XMMr4_t(0.442532692444982632759207708156_sp)
            type(XMM4c4),    automatic :: powc4
            type(XMM4c4),    automatic :: rat
            type(XMM4r4_t),  automatic :: powr4
            !dir$ attributes align : 16 :: C0442532692444982632759207708156
            !dir$ attributes align : 16 :: powc4
            !dir$ attributes align : 16 :: rat
            !dir$ attributes align : 16 :: powr4
            pow4r = r.v*r.v*r.v*r.v
            powc4 = cpow_xmm4c4(c,4.0_sp)
            rat   = powc4/powr4
            y4    = rat*C0442532692444982632759207708156
     end function SphcY4_inv4_v128b_ps
     
     pure function SphcY4_inv3_v128b_ps(c,z,r) result(y43)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)          
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY4_inv3_v128b_ps
            !dir$ attributes forceinline :: SphcY4_inv3_v128b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY4_inv3_v128b_ps   
#endif       
            type(XMM4c4),    intent(in) :: c
            type(XMM4r4_t),  intent(in) :: z
            type(XMM4r4_t),  intent(in) :: r
            type(XMM4c4)  ::               y43
            type(XMM4r4_t),  parameter  :: C125167147089835226917328104721 = &
                                 XMM4r4_t(1.25167147089835226917328104721_sp)
            type(XMM4c4),    automatic :: powc3
            type(XMM4c4),    automatic :: ct0
            type(XMM4c4),    automatic :: rat
            type(XMM4r4_t),  automatic :: powr4
            !dir$ attributes align : 16 :: C125167147089835226917328104721
            !dir$ attributes align : 16 :: powc3
            !dir$ attributes align : 16 :: ct0
            !dir$ attributes align : 16 :: rat
            !dir$ attributes align : 16 :: powr4
            powr4.v = r.v*r.v*r.v*r.v
            powc3   = cpow_xmm4c4(c,3.0_sp)
            ct0     = powc3*z
            rat     = ct0/powr4
            y43     = rat*C125167147089835226917328104721
     end function SphcY4_inv3_v128b_ps
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     





















