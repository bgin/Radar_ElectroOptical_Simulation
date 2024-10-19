
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

module sph_complex_zmm16r4


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         sph_complex_zmm16r4
 !          
 !          Purpose:
 !                       Complex Spherical Harmonics up to degree (l=10).
 !                        
 !          History:
 !                        Date: 19-10-2024
 !                        Time: 06:57 GMT+2
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
    use avx512_cvec16
    use mod_vectypes, only : ZMM16r4_t
    
    public
    implicit none
    
    
      ! Major version
     integer(kind=i4),  parameter :: SPH_COMPLEX_ZMM16R4_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: SPH_COMPLEX_ZMM16R4_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: SPH_COMPLEX_ZMM16R4_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: SPH_COMPLEX_ZMM16R4_FULLVER =   &
            1000*SPH_COMPLEX_ZMM16R4_MAJOR+100*SPH_COMPLEX_ZMM16R4_MINOR+10*SPH_COMPLEX_ZMM16R4_MICRO
     ! Module creation date
     character(*),        parameter :: SPH_COMPLEX_ZMM16R4_CREATE_DATE = "19-10-2024 09:15AM +00200 (SAT 19 OCT 2024 GMT+2)"
     ! Module build date
     character(*),        parameter :: SPH_COMPLEX_ZMM16R4_BUILD_DATE  = __DATE__ " " __TIME__
     ! Module author info
     character(*),        parameter :: SPH_COMPLEX_ZMM16R4_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: SPH_COMPLEX_ZMM16R4_SYNOPSIS  = "Complex Spherical Harmonics up to degree (l=10)." 
     
     type(ZMM16c4), parameter, public :: I = ZMM16c4(0.0_sp,1.0_sp)
     ! Data types
     
   
     
     contains
     
     ! Degree l=0
     
     pure function SphcY0_0_v512b_ps() result(y0)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY0_0_v512b_ps
            !dir$ attributes forceinline :: SphcY0_0_v512b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY0_0_v512b_ps
#endif
            type(ZMM16r4_t) :: y0
            y0 = ZMM16r4_t(0.28209479177387814347403972578_sp)
     end function SphcY0_0_v512b_ps
     
     ! Degree l=1
     
     pure function SphcY1_inv1_v512b_ps(c,r) result(y1)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY1_inv1_v512b_ps
            !dir$ attributes forceinline :: SphcY1_inv1_v512b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY1_inv1_v512b_ps
#endif     
            type(ZMM16c4),    intent(in) :: c
            type(ZMM16r4_t),  intent(in) :: r
            type(ZMM16c4) :: y1
            type(ZMM16r4_t),  parameter :: C0345494149471335479265244646032 = &
                                   ZMM16r4_t(0.345494149471335479265244646032_sp)
            type(ZMM16c4),    automatic :: tmp
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)             
            !dir$ attributes align  : 64  :: C0345494149471335479265244646032 
            !dir$ attributes align  : 64  :: tmp
#endif
            tmp = c/r
            y0  = tmp*C0345494149471335479265244646032
     end function SphcY1_inv1_v512b_ps
     
     pure function SphcY1_0_v512b_ps(z,r) result(y0)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY1_0_v512b_ps
            !dir$ attributes forceinline :: SphcY1_0_v512b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY1_0_v512b_ps
#endif          
            type(ZMM16r4_t),   intent(in) :: z
            type(ZMM16r4_t),   intent(in) :: r
            type(ZMM16r4_t) :: y0
            type(ZMM16r4_t),   parameter :: C1534990061919732732719327437334 = &
                                  ZMM16r4_t(1.534990061919732732719327437334_sp)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)                                   
            !dir$ attributes align  : 64  :: C1534990061919732732719327437334
#endif            
            y0.v = C1534990061919732732719327437334.v*(z.v/r.v)
     end function SphcY1_0_v512b_ps
     
     pure function SphcY1_1_v512b_ps(c,r) result(y1)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY1_1_v512b_ps
            !dir$ attributes forceinline :: SphcY1_1_v512b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY1_1_v512b_ps
#endif     
            type(ZMM16c4),    intent(in) :: c
            type(ZMM16r4_t),  intent(in) :: r
            type(ZMM16c4) :: y1
            type(ZMM16r4_t),  parameter :: C0345494149471335479265244646032 = &
                                   ZMM16r4_t(-0.345494149471335479265244646032_sp)
            type(ZMM16c4),    automatic :: tmp
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)             
            !dir$ attributes align  : 64  :: C0345494149471335479265244646032
            !dir$ attributes align  : 64  :: tmp
#endif            
            tmp = c/r
            y1  = tmp*C0345494149471335479265244646032
     end function SphcY1_1_v512b_ps
     
     ! Degree l=2
     
     pure function SphcY2_inv2_v512b_ps(c,r) result(y2)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY2_inv2_v512b_ps
            !dir$ attributes forceinline :: SphcY2_inv2_v512b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY2_inv2_v512b_ps
#endif     
            type(ZMM16c4),    intent(in) :: c
            type(ZMM16r4_t),  intent(in) :: r
            type(ZMM16c4) :: y2
            type(ZMM16r4_t),  parameter :: C0386274202023189580342192735311 = &
                                    ZMM16r4_t(0.386274202023189580342192735311_sp)
            type(ZMM16c4),    automatic :: tmp1
            type(ZMM16c4),    automatic :: csqr
            type(ZMM16r4_t),  automatic :: tmp2
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)             
            !dir$ attributes align  : 64  :: C0386274202023189580342192735311
            !dir$ attributes align  : 64  :: tmp
            !dir$ attributes align  : 64  :: csqr
#endif
            tmp2.v = r.v*r.v
            csqr   = c*c
            tmp1   = csqr/tmp2
            y2     = tmp1*C0386274202023189580342192735311
     end function SphcY2_inv2_v512b_ps


     pure function SphcY2_inv1_v512b_ps(c,z,r) result(y2)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY2_inv1_v512b_ps
            !dir$ attributes forceinline :: SphcY2_inv1_v512b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY2_inv1_v512b_ps
#endif     
            type(ZMM16c4),    intent(in) :: c
            type(ZMM16r4_t),  intent(in) :: z  
            type(ZMM16r4_t),  intent(in) :: r
            type(ZMM16c4)  :: y2
            type(ZMM16r4_t),  parameter :: C0772548404046379160684385470623 = &
                                    ZMM16r4_t(0.772548404046379160684385470623_sp)
            type(ZMM16c4),    automatic :: tmp
            type(ZMM16r4_t),  automatic :: rsqr
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)             
            !dir$ attributes align  : 64  :: C0772548404046379160684385470623
            !dir$ attributes align  : 64  :: tmp
            !dir$ attributes align  : 64  :: rsqr
#endif
            rsqr.v = r.v*r.v
            tmp    = c*z
            tmp    = tmp/rsqr
            y2     = tmp*C0772548404046379160684385470623           
     end function SphcY2_inv1_v512b_ps
     
     pure function SphcY2_0_v512b_ps(z,r) result(y2)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY2_0_v512b_ps
            !dir$ attributes forceinline :: SphcY2_0_v512b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY2_0_v512b_ps
#endif       
            type(ZMM16r4_t),   intent(in) :: z
            type(ZMM16r4_t),   intent(in) :: r
            type(ZMM16r4_t)  :: y2
            type(ZMM16r4_t),   parameter :: C0630783130505040012061787380591 = 
                                   ZMM16r4_t(0.630783130505040012061787380591_sp)
            type(ZMM16r4_t),   parameter :: C3 = ZMM16r4_t(3.0_sp)
            type(ZMM16r4_t),   automatic :: zsqr
            type(ZMM16r4_t),   automatic :: rsqr
            type(ZMM16r4_t),   automatic :: diff
            type(ZMM16r4_t),   automatic :: rat 
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)       
            !dir$ attributes align  : 64  :: C0630783130505040012061787380591
            !dir$ attributes align  : 64  :: C3
            !dir$ attributes align  : 64  :: zsqr
            !dir$ attributes align  : 64  :: rsqr
            !dir$ attributes align  : 64  :: diff
            !dir$ attributes align  : 64  :: rat
#endif
            zsqr.v = z.v*z.v
            rsqr.v = r.v*r.v
            diff.v = (C3.v*zsqr.v)-rsqr.v
            rat.v  = diff.v/rsqr.v
            y2.v   = C0630783130505040012061787380591.v*rat.v
     end function SphcY2_0_v512b_ps
     
     pure function SphcY2_1_v512b_ps(c,z,r) result(y2)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY2_1_v512b_ps
            !dir$ attributes forceinline :: SphcY2_1_v512b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY2_1_v512b_ps
#endif     
            type(ZMM16c4),    intent(in) :: c
            type(ZMM16r4_t),  intent(in) :: z  
            type(ZMM16r4_t),  intent(in) :: r
            type(ZMM16c4)  :: y2
            type(ZMM16r4_t),  parameter :: C0772548404046379160684385470623 = &
                                    ZMM16r4_t(-0.772548404046379160684385470623_sp)
            type(ZMM16c4),    automatic :: tmp
            type(ZMM16r4_t),  automatic :: rsqr
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)             
            !dir$ attributes align  : 64  :: C0772548404046379160684385470623
            !dir$ attributes align  : 64  :: tmp
            !dir$ attributes align  : 64  :: rsqr
#endif  
            rsqr.v = r.v*r.v
            tmp    = c*z
            tmp    = tmp/rsqr
            y2     = tmp*C0772548404046379160684385470623     
     end function SphcY2_1_v512b_ps
     
     pure function SphcY2_2_v512b_ps(c,r) result(y2)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY2_2_v512b_ps
            !dir$ attributes forceinline :: SphcY2_2_v512b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY2_2_v512b_ps
#endif     
            type(ZMM16c4),    intent(in) :: c
            type(ZMM16r4_t),  intent(in) :: r
            type(ZMM16c4) :: y2  
            y2 = Y2_inv2_v512b_ps(c,r)     
     end function SphcY2_2_v512b_ps

     
     ! Degree l=3

     pure function SphcY3_inv1_v512b_ps(c,z,r) result(y3)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)       
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY3_inv1_v512b_ps
            !dir$ attributes forceinline :: SphcY3_inv1_v512b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY3_inv1_v512b_ps
#endif
            type(ZMM16c4),    intent(in) :: c
            type(ZMM16r4_t),  intent(in) :: z
            type(ZMM16r4_t),  intent(in) :: r
            type(ZMM16c4)  ::               y3
            type(ZMM16r4_t),  parameter  :: C032318018411415065300739416333 = &
                                    ZMM16r4_t(0.32318018411415065300739416333_sp)
            type(ZMM16r4_t),  parameter  :: C5 = ZMM16r4_t(5.0_sp)
            type(ZMM16c4),    automatic :: ct1
            type(ZMM16c4),    automatic :: ct2
            type(ZMM16r4_t),  automatic :: zz
            type(ZMM16r4_t),  automatic :: rr
            type(ZMM16r4_t),  automatic :: rrr
            type(ZMM16r4_t),  automatic :: t0
            
            !dir$ attributes align  : 64  :: C032318018411415065300739416333
            !dir$ attributes align  : 64  :: C5
            !dir$ attributes align  : 64  :: ct1
            !dir$ attributes align  : 64  :: ct2
            !dir$ attributes align  : 64  :: zz
            !dir$ attributes align  : 64  :: rr
            !dir$ attributes align  : 64  :: rrr
            !dir$ attributes align  : 64  :: t0
           
            zz.v = z.v*z.v
            rr.v = r.v*r.v
            ct0.v= C5.v*zz.v-rr.v
            rrr.v= rr.v*r.v
            ct1  = c*t0
            ct2  = ct1/rrr
            y3   = ct2*C032318018411415065300739416333
     end function SphcY3_inv1_v512b_ps
     
     pure function SphcY3_0_v512b_ps(z,r) result(y3)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)  
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY3_0_v512b_ps
            !dir$ attributes forceinline :: SphcY3_0_v512b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY3_0_v512b_ps
#endif
            type(ZMM16r4_t),  intent(in) :: z
            type(ZMM16r4_t),  intent(in) :: r
            type(ZMM16r4_t)  ::             y3
            type(ZMM16r4_t),  parameter :: C0373176332590115391414395913199 = &
                                   ZMM16r4_t(0.373176332590115391414395913199_sp)
            type(ZMM16r4_t),  parameter :: C5 = ZMM16r4_t(0.5_sp)
            type(ZMM16r4_t),  parameter :: C3 = ZMM16r4_t(3.0_sp)
            type(ZMM16r4_t),  automatic :: zzz
            type(ZMM16r4_t),  automatic :: rr
            type(ZMM16r4_t),  automatic :: rrr
            type(ZMM16r4_t),  automatic :: t0
            type(ZMM16r4_t),  automatic :: t1
            type(ZMM16r4_t),  automatic :: t2
            !dir$ attributes align  : 64  :: C0373176332590115391414395913199
            !dir$ attributes align  : 64  :: C5
            !dir$ attributes align  : 64  :: C3
            !dir$ attributes align  : 64  :: zzz
            !dir$ attributes align  : 64  :: rr
            !dir$ attributes align  : 64  :: rrr
            !dir$ attributes align  : 64  :: t0
            !dir$ attributes align  : 64  :: t1 
            !dir$ attributes align  : 64  :: t2
            zzz.v = z.v*z.v*z.v
            rr.v  = r.v*r.v
            rrr.v = rr.v*r.v
            t0.v  = C5.v*zzz.v
            t1.v  = C3.v*z.v*rr.v
            t2.v  = t0.v-t1.v
            y3.v  = C0373176332590115391414395913199.v*(t2.v/rrr.v)
     end function SphcY3_0_v512b_ps
     
     pure function SphcY3_1_v512b_ps(c,z,r) result(y3)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)  
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY3_1_v512b_ps
            !dir$ attributes forceinline :: SphcY3_1_v512b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY3_1_v512b_ps
#endif
            type(ZMM16c4),    intent(in) :: c
            type(ZMM16r4_t),  intent(in) :: z
            type(ZMM16r4_t),  intent(in) :: r
            type(ZMM16c4)  ::               y3
            type(ZMM16r4_t),  parameter  :: C032318018411415065300739416333 = &
                                    ZMM16r4_t(-0.32318018411415065300739416333_sp)
            type(ZMM16r4_t),  parameter  :: C5 = ZMM16r4_t(5.0_sp)
            type(ZMM16c4),    automatic :: ct1
            type(ZMM16c4),    automatic :: ct2
            type(ZMM16r4_t),  automatic :: zz
            type(ZMM16r4_t),  automatic :: rr
            type(ZMM16r4_t),  automatic :: rrr
            type(ZMM16r4_t),  automatic :: t0
            
            !dir$ attributes align  : 64  :: C032318018411415065300739416333
            !dir$ attributes align  : 64  :: C5
            !dir$ attributes align  : 64  :: ct1
            !dir$ attributes align  : 64  :: ct2
            !dir$ attributes align  : 64  :: zz
            !dir$ attributes align  : 64  :: rr
            !dir$ attributes align  : 64  :: rrr
            !dir$ attributes align  : 64  :: t0
           
            zz.v = z.v*z.v
            rr.v = r.v*r.v
            ct0.v= C5.v*zz.v-rr.v
            rrr.v= rr.v*r.v
            ct1  = c*t0
            ct2  = ct1/rrr
            y3   = ct2*C032318018411415065300739416333
     end function SphcY3_1_v512b_ps
     
     pure function SphcY3_2_v512b_ps(c,z,r) result(y32)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)       
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY3_2_v512b_ps
            !dir$ attributes forceinline :: SphcY3_2_v512b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY3_2_v512b_ps
#endif
            type(ZMM16c4),    intent(in) :: c
            type(ZMM16r4_t),  intent(in) :: z
            type(ZMM16r4_t),  intent(in) :: r
            type(ZMM16c4)  ::               y32
            type(ZMM16r4_t),  parameter :: C10219854764332823633961144917 = &
                                   ZMM16r4_t(1.0219854764332823633961144917)
            type(ZMM16c4),    automatic :: csqr
            type(ZMM16c4),    automatic :: ct0
            type(ZMM16c4),    automatic :: ct1
            type(ZMM16r4_t),  automatic :: rrr
            !dir$ attributes align  : 64  :: C10219854764332823633961144917
            !dir$ attributes align  : 64  :: csqr
            !dir$ attributes align  : 64  :: ct0
            !dir$ attributes align  : 64  :: ct1
            !dir$ attributes align  : 64  :: rrr
            rrr.v = r.v*r.v*r.v
            csqr  = c*c
            ct0   = csqr*z
            ct1   = ct0/rrr
            y32   = ct1*C10219854764332823633961144917
     end function SphcY3_2_v128v_ps

     pure function SphcY3_3_v512b_ps(c,r) result(y33)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)     
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY3_3_v512b_ps
            !dir$ attributes forceinline :: SphcY3_3_v512b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY3_3_v512b_ps
#endif
            type(ZMM16c4),    intent(in) :: c
            type(ZMM16r4_t),  intent(in) :: r
            type(ZMM16c4)  ::               y33
            type(ZMM16r4_t),  parameter :: C0417223823632784089724427015737 =  &
                                  ZMM16r4_t(0.417223823632784089724427015737_sp)
            type(ZMM16c4),    automatic :: pow3c
            type(ZMM16c4),    automatic :: rat
            type(ZMM16r4_t),  automatic :: rrr
            !dir$ attributes align  : 64  :: C0417223823632784089724427015737
            !dir$ attributes align  : 64  :: pow3c
            !dir$ attributes align  : 64  :: rat
            rrr.v = r.v*r.v*r.v
            pow3c = cpow_xmm4c4(c,3.0_sp)
            rat   = pow3c/rrr
            y33   = rat*C0417223823632784089724427015737
     end function SphcY3_3_v512b_ps
     
     
     ! Degree l=4
     pure function SphcY4_inv4_v512b_ps(c,r) result(y4)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)         
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY4_inv4_v512b_ps
            !dir$ attributes forceinline :: SphcY4_inv4_v512b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY4_inv4_v512b_ps
#endif
            type(ZMM16c4),    intent(in) :: c
            type(ZMM16r4_t),  intent(in) :: r
            type(ZMM16c4)  ::               y4
            type(ZMM16r4_t),  parameter :: C0442532692444982632759207708156 = &
                                  XMMr4_t(0.442532692444982632759207708156_sp)
            type(ZMM16c4),    automatic :: powc4
            type(ZMM16c4),    automatic :: rat
            type(ZMM16r4_t),  automatic :: powr4
            !dir$ attributes align  : 64  :: C0442532692444982632759207708156
            !dir$ attributes align  : 64  :: powc4
            !dir$ attributes align  : 64  :: rat
            !dir$ attributes align  : 64  :: powr4
            pow4r = r.v*r.v*r.v*r.v
            powc4 = cpow_xmm4c4(c,4.0_sp)
            rat   = powc4/powr4
            y4    = rat*C0442532692444982632759207708156
     end function SphcY4_inv4_v512b_ps
     
     pure function SphcY4_inv3_v512b_ps(c,z,r) result(y43)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)          
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY4_inv3_v512b_ps
            !dir$ attributes forceinline :: SphcY4_inv3_v512b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY4_inv3_v512b_ps   
#endif       
            type(ZMM16c4),    intent(in) :: c
            type(ZMM16r4_t),  intent(in) :: z
            type(ZMM16r4_t),  intent(in) :: r
            type(ZMM16c4)  ::               y43
            type(ZMM16r4_t),  parameter  :: C125167147089835226917328104721 = &
                                 ZMM16r4_t(1.25167147089835226917328104721_sp)
            type(ZMM16c4),    automatic :: powc3
            type(ZMM16c4),    automatic :: ct0
            type(ZMM16c4),    automatic :: rat
            type(ZMM16r4_t),  automatic :: powr4
            !dir$ attributes align  : 64  :: C125167147089835226917328104721
            !dir$ attributes align  : 64  :: powc3
            !dir$ attributes align  : 64  :: ct0
            !dir$ attributes align  : 64  :: rat
            !dir$ attributes align  : 64  :: powr4
            powr4.v = r.v*r.v*r.v*r.v
            powc3   = cpow_xmm4c4(c,3.0_sp)
            ct0     = powc3*z
            rat     = ct0/powr4
            y43     = rat*C125167147089835226917328104721
     end function SphcY4_inv3_v512b_ps
     
     pure function SphcY4_inv2_v512b_ps(c,z,r) result(y42)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)          
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY4_inv2_v512b_ps
            !dir$ attributes forceinline :: SphcY4_inv2_v512b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY4_inv2_v512b_ps   
#endif            
            type(ZMM16c4),    intent(in) :: c
            type(ZMM16r4_t),  intent(in) :: z
            type(ZMM16r4_t),  intent(in) :: r
            type(ZMM16c4)  ::               y42
            type(ZMM16r4_t),  parameter :: C0334523271778644583976056194856 = &
                                ZMM16r4_t(0.334523271778644583976056194856_sp)
            type(ZMM16r4_t),  parameter :: C7 = ZMM16r4_t(7.0_sp)
            type(ZMM16c4),    automatic :: powc2
            type(ZMM16c4),    automatic :: ct0
            type(ZMM16c4),    automatic :: rat
            type(ZMM16r4_t),  automatic :: zz
            type(ZMM16r4_t),  automatic :: rr
            type(ZMM16r4_t),  automatic :: powr4
            type(ZMM16r4_t),  automatic :: t0
            type(ZMM16r4_t),  automatic :: t1
            !dir$ attributes align  : 64  :: C0334523271778644583976056194856
            !dir$ attributes align  : 64  :: C7
            !dir$ attributes align  : 64  :: powc2
            !dir$ attributes align  : 64  :: ct0
            !dir$ attributes align  : 64  :: rat
            !dir$ attributes align  : 64  :: zz
            !dir$ attributes align  : 64  :: rr
            !dir$ attributes align  : 64  :: t0
            !dir$ attributes align  : 64  :: t1
            rr.v    = r.v*r.v
            powr4.v = rr.v*rr.v
            zz.v    = z.v*z.v
            powc2   = c*c
            t0      = C7.v*zz.v-rr.v
            ct0     = powc2*t0
            rat     = ct0/powr4
            y42     = rat*C0334523271778644583976056194856 
     end function SphcY4_inv2_v512b_ps
     
     pure function SphcY4_inv1_v512b_ps(c,z,r) result(y41)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)          
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY4_inv1_v512b_ps
            !dir$ attributes forceinline :: SphcY4_inv1_v512b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY4_inv1_v512b_ps   
#endif            
            type(ZMM16c4),    intent(in) :: c
            type(ZMM16r4_t),  intent(in) :: z
            type(ZMM16r4_t),  intent(in) :: r
            type(ZMM16c4)  ::               y41
            type(ZMM16r4_t),  parameter :: C0473087347878780009046340535444 = &
                                ZMM16r4_t(0.473087347878780009046340535444_sp)
            type(ZMM16r4_t),  parameter :: C7 = ZMM16r4_t(7.0_sp)
            type(ZMM16r4_t),  parameter :: C3 = ZMM16r4_t(3.0_sp)
            type(ZMM16c4),    automatic :: ct0
            type(ZMM16c4),    automatic :: rat
            type(ZMM16r4_t),  automatic :: zzz
            type(ZMM16r4_t),  automatic :: rr
            type(ZMM16r4_t),  automatic :: powr4
            type(ZMM16r4_t),  automatic :: t0
            type(ZMM16r4_t),  automatic :: t1
            type(ZMM16r4_t),  automatic :: t2
            !dir$ attributes align  : 64  :: C0473087347878780009046340535444
            !dir$ attributes align  : 64  :: C7
            !dir$ attributes align  : 64  :: C2
            !dir$ attributes align  : 64  :: ct0
            !dir$ attributes align  : 64  :: rat
            !dir$ attributes align  : 64  :: zzz
            !dir$ attributes align  : 64  :: rr
            !dir$ attributes align  : 64  :: t0
            !dir$ attributes align  : 64  :: t1
            !dir$ attributes align  : 64  :: t2
            zzz.v    = z.v*z.v*z.v
            rr.v     = r.v*r.v
            t0.v     = C3.v*z.v*rr.v
            powr4.v  = rr.v*rr.v
            t1.v     = C7.v*zzz.v
            t2.v     = t1.v-t0.v
            ct0      = c*t2.v
            rat      = ct0/powr4
            y41      = rat*C0473087347878780009046340535444
    end function SphcY4_inv1_v512b_ps 
    
    pure function SphcY4_0_v512b_ps(z,r) result(y40)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)          
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY4_0_v512b_ps
            !dir$ attributes forceinline :: SphcY4_0_v512b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY4_0_v512b_ps   
#endif           
            type(ZMM16r4_t),  intent(in) :: z
            type(ZMM16r4_t),  intent(in) :: r
            type(ZMM16r4_t)              :: y41   
            type(ZMM16r4_t),  parameter  :: C0105785546915204303802764897168 = &
                                ZMM16r4_t(0.105785546915204303802764897168_sp)
            type(ZMM16r4_t),  parameter  :: C35 = ZMM16r4_t(35.0_sp)
            type(ZMM16r4_t),  parameter  :: C30 = ZMM16r4_t(30.0_sp)
            type(ZMM16r4_t),  parameter  :: C3  = ZMM16r4_t(3.0_sp)
            type(ZMM16r4_t),  automatic  :: powr4
            type(ZMM16r4_t),  automatic  :: powz4
            type(ZMM16r4_t),  automatic  :: zz
            type(ZMM16r4_t),  automatic  :: rr 
            type(ZMM16r4_t),  automatic  :: t0
            type(ZMM16r4_t),  automatic  :: t1
            type(ZMM16r4_t),  automatic  :: rat
            !dir$ attributes align  : 64  :: C0105785546915204303802764897168
            !dir$ attributes align  : 64  :: C35
            !dir$ attributes align  : 64  :: C30
            !dir$ attributes align  : 64  :: C3
            !dir$ attributes align  : 64  :: powr4
            !dir$ attributes align  : 64  :: powzr
            !dir$ attributes align  : 64  :: zz
            !dir$ attributes align  : 64  :: rr
            !dir$ attributes align  : 64  :: t0
            !dir$ attributes align  : 64  :: t1
            !dir$ attributes align  : 64  :: rat
            zz.v    = z.v*z.v
            rr.v    = r.v*r.v
            powr4.v = rr.v*rr.v
            powz4.v = zz.v*zz.v
            t0.v    = C35.v*z-C30.v*zz.v*rr.v
            t1.v    = C3.v*powr4.v
            rat.v   = (t0.v+t1.v)/powr4.v
            y40.v   = rat.v*C0105785546915204303802764897168.v
    end function SphcY4_0_v512b_ps
    
    pure function SphycY4_1_v512b_ps(c,z,r) result(y41)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)          
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY4_1_v512b_ps
            !dir$ attributes forceinline :: SphcY4_1_v512b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY4_1_v512b_ps   
#endif            
            type(ZMM16c4),    intent(in) :: c
            type(ZMM16r4_t),  intent(in) :: z
            type(ZMM16r4_t),  intent(in) :: r
            type(ZMM16c4)  ::               y41
            type(ZMM16r4_t),  parameter :: C0473087347878780009046340535444 = &
                                ZMM16r4_t(-0.473087347878780009046340535444_sp)
            type(ZMM16r4_t),  parameter :: C7 = ZMM16r4_t(7.0_sp)
            type(ZMM16r4_t),  parameter :: C3 = ZMM16r4_t(3.0_sp)
            type(ZMM16c4),    automatic :: ct0
            type(ZMM16c4),    automatic :: rat
            type(ZMM16r4_t),  automatic :: zzz
            type(ZMM16r4_t),  automatic :: rr
            type(ZMM16r4_t),  automatic :: powr4
            type(ZMM16r4_t),  automatic :: t0
            type(ZMM16r4_t),  automatic :: t1
            type(ZMM16r4_t),  automatic :: t2
            !dir$ attributes align  : 64  :: C0473087347878780009046340535444
            !dir$ attributes align  : 64  :: C7
            !dir$ attributes align  : 64  :: C2
            !dir$ attributes align  : 64  :: ct0
            !dir$ attributes align  : 64  :: rat
            !dir$ attributes align  : 64  :: zzz
            !dir$ attributes align  : 64  :: rr
            !dir$ attributes align  : 64  :: t0
            !dir$ attributes align  : 64  :: t1
            !dir$ attributes align  : 64  :: t2
            zzz.v    = z.v*z.v*z.v
            rr.v     = r.v*r.v
            t0.v     = C3.v*z.v*rr.v
            powr4.v  = rr.v*rr.v
            t1.v     = C7.v*zzz.v
            t2.v     = t1.v-t0.v
            ct0      = c*t2.v
            rat      = ct0/powr4
            y41      = rat*C0473087347878780009046340535444
    end function SphycY4_1_v512b_ps
    
    pure function SphcY4_2_v512b_ps(c,z,r) result(y42)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)          
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY4_2_v512b_ps
            !dir$ attributes forceinline :: SphcY4_2_v512b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY4_2_v512b_ps   
#endif            
            type(ZMM16c4),    intent(in) :: c
            type(ZMM16r4_t),  intent(in) :: z
            type(ZMM16r4_t),  intent(in) :: r
            type(ZMM16c4)  ::               y42
            y42 = SphycY4_inv2_v512b_ps(c,z,r)
     end function SphcY4_2_v512b_ps
     
     pure function SphcY4_3_v512b_ps(c,z,r) result(y43)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)          
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY4_3_v512b_ps
            !dir$ attributes forceinline :: SphcY4_3_v512b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY4_3_v512b_ps   
#endif       
            type(ZMM16c4),    intent(in) :: c
            type(ZMM16r4_t),  intent(in) :: z
            type(ZMM16r4_t),  intent(in) :: r
            type(ZMM16c4)  ::               y43
            type(ZMM16r4_t),  parameter  :: C125167147089835226917328104721 = &
                                 ZMM16r4_t(-1.25167147089835226917328104721_sp)
            type(ZMM16c4),    automatic :: powc3
            type(ZMM16c4),    automatic :: ct0
            type(ZMM16c4),    automatic :: rat
            type(ZMM16r4_t),  automatic :: powr4
            !dir$ attributes align  : 64  :: C125167147089835226917328104721
            !dir$ attributes align  : 64  :: powc3
            !dir$ attributes align  : 64  :: ct0
            !dir$ attributes align  : 64  :: rat
            !dir$ attributes align  : 64  :: powr4
            powr4.v = r.v*r.v*r.v*r.v
            powc3   = cpow_xmm4c4(c,3.0_sp)
            ct0     = powc3*z
            rat     = ct0/powr4
            y43     = rat*C125167147089835226917328104721
     end function SphcY4_3_v512b_ps
     
     pure function SphcY4_4_v512b_ps(c,r) result(y44)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)         
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY4_4_v512b_ps
            !dir$ attributes forceinline :: SphcY4_4_v512b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY4_4_v512b_ps
#endif
            type(ZMM16c4),    intent(in) :: c
            type(ZMM16r4_t),  intent(in) :: r
            type(ZMM16c4)  ::               y44 
            y44 = SphcY4_inv4_v512b_ps(c,r)    
     end function SphcY4_4_v512b_ps
     
     
     ! Degree l=5
     
     pure function SphcY5_inv5_v512b_ps(tht,phi) result(y5i5)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)          
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY5_inv5_v512b_ps
            !dir$ attributes forceinline :: SphcY5_inv5_v512b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY5_inv5_v512b_ps  
#endif       
            type(ZMM16r4_t),   intent(in) :: tht
            type(ZMM16r4_t),   intent(in) :: phi
            type(ZMM16c4)                 :: y5i5
            type(ZMM16r4_t),   parameter  :: C0464132203440858160657998605534 = &
                                                  ZMM16r4_t(0.464132203440858160657998605534_sp)
            type(ZMM16r4_t),   parameter  :: C5  = ZMM16r4_t(-5.0_sp)
            type(ZMM16c4),     automatic  :: carg
            type(ZMM16c4),     automatic  :: cexp
            type(ZMM16c4),     automatic  :: ct0
            type(ZMM16r4_t),   automatic  :: sint
            type(ZMM16r4_t),   automatic  :: psin2
            type(ZMM16r4_t),   automatic  :: psin5
            !dir$ attributes align  : 64  :: C0464132203440858160657998605534
            !dir$ attributes align  : 64  :: C5
            !dir$ attributes align  : 64  :: carg
            !dir$ attributes align  : 64  :: ct0
            !dir$ attributes align  : 64  :: cexp
            !dir$ attributes align  : 64  :: sint
            !dir$ attributes align  : 64  :: psin2
            !dir$ attributes align  : 64  :: psin5
            ct0    = I*C5
            carg   = ct0*phi
            sint.v = sin(tht.v)
            psin2.v= sint.v*sint.v
            cexp   = cexp_xmm4c4(carg)
            psin5.v= psin2.v*psin2.v*sint.v
            ct0    = cexp*C0464132203440858160657998605534
            y5i5   = ct0*psin5
     end function SphcY5_inv5_v512b_ps
     
      pure function SphcY5_inv4_v512b_ps(tht,phi) result(y5i4)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)          
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY5_inv4_v512b_ps
            !dir$ attributes forceinline :: SphcY5_inv4_v512b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY5_inv4_v512b_ps  
#endif       
            type(ZMM16r4_t),   intent(in) :: tht
            type(ZMM16r4_t),   intent(in) :: phi
            type(ZMM16c4)                 :: y5i4
            type(ZMM16r4_t),   parameter  :: C1467714898305751163052026147529 = &
                                                 ZMM16r4_t(1.467714898305751163052026147529_sp)
            type(ZMM16r4_t),   parameter  :: C4 = ZMM16r4_t(-4.0_sp)       
            type(ZMM16c4),     automatic  :: carg
            type(ZMM16c4),     automatic  :: cexp
            type(ZMM16c4),     automatic  :: ct0
            type(ZMM16r4_t),   automatic  :: sint
            type(ZMM16r4_t),   automatic  :: cost
            type(ZMM16r4_t),   automatic  :: psin4
            !dir$ attributes align  : 64  ::  C1467714898305751163052026147529
            !dir$ attributes align  : 64  ::  C4
            !dir$ attributes align  : 64  ::  carg
            !dir$ attributes align  : 64  ::  cexp
            !dir$ attributes align  : 64  ::  ct0
            !dir$ attributes align  : 64  ::  sint
            !dir$ attributes align  : 64  ::  cost
            !dir$ attributes align  : 64  ::  psin4
            ct0    =  I*C4
            sint.v =  sin(tht.v)
            carg   =  ct0*phi
            cost.v =  cos(tht.v)
            psin4.v=  sint.v*sint.v*sint.v*sint.v
            cexp   =  cexp_xmm4c4(carg)
            psin4.v=  psin4.v*cost.v
            ct0    =  cexp*psin4
            y5i4   =  ct0*C1467714898305751163052026147529    
      end function SphcY5_inv4_v512b_ps
      
      pure function SphcY5_inv3_v512b_ps(tht,phi) result(y5i3)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)          
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY5_inv3_v512b_ps
            !dir$ attributes forceinline :: SphcY5_inv3_v512b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY5_inv3_v512b_ps  
#endif       
            type(ZMM16r4_t),   intent(in) :: tht
            type(ZMM16r4_t),   intent(in) :: phi
            type(ZMM16c4)                 :: y5i3
            type(ZMM16r4_t),   parameter  :: C0345943719146840213165966420433 = &
                                              ZMM16r4_t(0.345943719146840213165966420433_sp)
            type(ZMM16r4_t),   parameter  :: C9 = ZMM16r4_t(9.0_sp)
            type(ZMM16r4_t),   parameter  :: C1 = ZMM16r4_t(1.0_sp)
            type(ZMM16r4_t),   parameter  :: C3 = ZMM16r4_t(-3.0_sp)
            type(ZMM16c4),     automatic  :: carg
            type(ZMM16c4),     automatic  :: cexp
            type(ZMM16c4),     automatic  :: ct0
            type(ZMM16r4_t),   automatic  :: sint
            type(ZMM16r4_t),   automatic  :: psin3
            type(ZMM16r4_t),   automatic  :: cost
            type(ZMM16r4_t),   automatic  :: t0
            !dir$ attributes align  : 64  :: C0345943719146840213165966420433
            !dir$ attributes align  : 64  :: C9
            !dir$ attributes align  : 64  :: C1
            !dir$ attributes align  : 64  :: carg
            !dir$ attributes align  : 64  :: cexp
            !dir$ attributes align  : 64  :: ct0
            !dir$ attributes align  : 64  :: sint
            !dir$ attributes align  : 64  :: psin3
            !dir$ attributes align  : 64  :: cost
            ct0    = I*C3
            carg   = ct0*phi
            sint.v = sin(tht.v)
            cexp   = cexp_xmm4c4(carg)
            cost.v = cos(tht.v)
            t0.v   = (C9.v*cost.v*cost.v)-C1.v
            psin3.v= sint.v*sint.v*sint.v
            ct0    = cexp*C0345943719146840213165966420433
            t0.v   = t0.v*psin3.v
            y5i3   = ct0*t0
      end function SphcY5_inv3_v128_ps
      
      pure function SphcY5_inv2_v512b_ps(tht,phi) result(y5i2)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)          
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY5_inv2_v512b_ps
            !dir$ attributes forceinline :: SphcY5_inv2_v512b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY5_inv2_v512b_ps  
#endif       
            type(ZMM16r4_t),   intent(in) :: tht
            type(ZMM16r4_t),   intent(in) :: phi
            type(ZMM16c4)                 :: y5i2
            type(ZMM16r4_t),   parameter  :: C1694771183260899275815691555511 = 
                                            ZMM16r4_t(1.694771183260899275815691555511_sp)
            type(ZMM16r4_t),   parameter  :: C2 = ZMM16r4_t(-2.0_sp)
            type(ZMM16r4_t),   parameter  :: C3 = ZMM16r4_t(3.0_sp) 
            type(ZMM16c4),     automatic  :: carg
            type(ZMM16c4),     automatic  :: cexp
            type(ZMM16c4),     automatic  :: ct0
            type(ZMM16r4_t),   automatic  :: sint
            type(ZMM16r4_t),   automatic  :: psin
            type(ZMM16r4_t),   automatic  :: cost
            type(ZMM16r4_t),   automatic  :: pcos3
            type(ZMM16r4_t),   automatic  :: t0
             !dir$ attributes align  : 64  :: C1694771183260899275815691555511
             !dir$ attributes align  : 64  :: C2
             !dir$ attributes align  : 64  :: C3
             !dir$ attributes align  : 64  :: carg
             !dir$ attributes align  : 64  :: cexp
             !dir$ attributes align  : 64  :: ct0
             !dir$ attributes align  : 64  :: sint
             !dir$ attributes align  : 64  :: psin
             !dir$ attributes align  : 64  :: cost
             !dir$ attributes align  : 64  :: pcos3
             !dir$ attributes align  : 64  :: t0
            ct0    = I*C2
            cost.v = cos(tht.v)
            carg   = ct0*phi
            pcos3.v= cost.v*cost.v*cost.v
            t0.v   = C3.v*pcos3.v-cost.v
            cexp   = cexp_xmm4c4(carg)
            sint.v = sin(tht.v)
            t0.v   = sint.v*sint.v*t0.v
            ct0    = cexp*C1694771183260899275815691555511
            y5i2   = ct0*t0
      end function SphcY5_inv2_v512b_ps
     
      pure function SphcY5_inv1_v512b_ps(theta,phi) result(y5i1)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)          
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY5_inv1_v512b_ps
            !dir$ attributes forceinline :: SphcY5_inv1_v512b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY5_inv1_v512b_ps  
#endif       
            type(ZMM16r4_t),   intent(in) :: theta
            type(ZMM16r4_t),   intent(in) :: phi
            type(ZMM16c4)                 :: y5i1
            type(ZMM16r4_t),   parameter  :: C0320281648576215127537161432847 = 
                                              ZMM16r4_t(0.320281648576215127537161432847_sp)
            type(ZMM16r4_t),   parameter  :: C21 = ZMM16r4_t(21.0_sp)
            type(ZMM16r4_t),   parameter  :: C14 = ZMM16r4_t(14.0_sp)
            type(ZMM16r4_t),   parameter  :: C1  = ZMM16r4_t(1.0_sp)
            type(ZMM16r4_t),   parameter  :: CN1 = ZMM16r4_t(-1.0_sp)
            type(ZMM16c4),     automatic  :: carg
            type(ZMM16c4),     automatic  :: cexp
            type(ZMM16c4),     automatic  :: ct0
            type(ZMM16r4_t),   automatic  :: sint
            type(ZMM16r4_t),   automatic  :: cost
            type(ZMM16r4_t),   automatic  :: pcos4
            type(ZMM16r4_t),   automatic  :: t0
            type(ZMM16r4_t),   automatic  :: t1
            !dir$ attributes align  : 64  :: C0320281648576215127537161432847
            !dir$ attributes align  : 64  :: C14
            !dir$ attributes align  : 64  :: C21
            !dir$ attributes align  : 64  :: C1
            !dir$ attributes align  : 64  :: carg
            !dir$ attributes align  : 64  :: cexp
            !dir$ attributes align  : 64  :: ct0
            !dir$ attributes align  : 64  :: sint
            !dir$ attributes align  : 64  :: cost
            !dir$ attributes align  : 64  :: pcos4
            !dir$ attributes align  : 64  :: t0
            !dir$ attributes align  : 64  :: t1
            !dir$ attributes align  : 64  :: t2
            ct0    = I*CN1
            sint.v = sin(tht.v)
            carg   = ct0*phi
            cost.v = cos(tht.v)
            cexp   = cexp_xmm4c4(carg)
            t0.v   = (C14*cost.v*cost.v)-C1.V
            t1.v   = C21.v*cost.v*cost.v*cost.v*cost.v
            t2.v   = sint.v*(t0.v-t1.v)
            ct0    = cexp*C0320281648576215127537161432847
            y5i1   = ct0*t2
      end function SphcY5_inv1_v512b_ps
      
      pure function SphcY5_0_v512b_ps(theta,phi) result(y50)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)          
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY5_0_v512b_ps
            !dir$ attributes forceinline :: SphcY5_0_v512b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY5_0_v512b_ps  
#endif       
            type(ZMM16r4_t),   intent(in) :: theta
            type(ZMM16r4_t),   intent(in) :: phi
            type(ZMM16c4)                 :: y50
            type(ZMM16r4_t),   parameter  :: C011695032245342359643971519209 = &
                                                   ZMM16r4_t(0.11695032245342359643971519209_sp)
            type(ZMM16r4_t),   parameter  :: C63 =  ZMM16r4_t(63.0_sp)
            type(ZMM16r4_t),   parameter  :: C70 =  ZMM16r4_t(70.0_sp)
            type(ZMM16r4_t),   parameter  :: C15 =  ZMM16r4_t(15.0_sp)
            type(ZMM16r4_t),   automatic  :: cost
            type(ZMM16r4_t),   automatic  :: costp2
            type(ZMM16r4_t),   automatic  :: t0
            type(ZMM16r4_t),   automatic  :: t1
            type(ZMM16r4_t),   automatic  :: t2
            type(ZMM16r4_t),   automatic  :: t3
            !dir$ attributes align  : 64  :: C011695032245342359643971519209
            !dir$ attributes align  : 64  :: C63
            !dir$ attributes align  : 64  :: C70
            !dir$ attributes align  : 64  :: C15
            !dir$ attributes align  : 64  :: cost
            !dir$ attributes align  : 64  :: costp2
            !dir$ attributes align  : 64  :: t0
            !dir$ attributes align  : 64  :: t1
            !dir$ attributes align  : 64  :: t2
            !dir$ attributes align  : 64  :: t3
            cost.v  = cos(theta.v)
            t0.v    = C15.v*cost.v
            costp2.v= cost.v*cost.v
            t1.v    = C70.v*(costp2.v*cost.v)
            t2.v    = C63.v*(costp2.v*costp2.v*cost.v)
            t3.v    = t2.v-t1.v+t0.v
            y50.v   = C011695032245342359643971519209.v*t3.v
      end function SphcY5_0_v512b_ps
      
      pure function SphcY5_1_v512b_ps(theta,phi) result(y51)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)          
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY5_1_v512b_ps
            !dir$ attributes forceinline :: SphcY5_1_v512b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY5_1_v512b_ps  
#endif       
            type(ZMM16r4_t),   intent(in) :: theta
            type(ZMM16r4_t),   intent(in) :: phi
            type(ZMM16c4)                 :: y5i1
            type(ZMM16r4_t),   parameter  :: C0320281648576215127537161432847 = 
                                              ZMM16r4_t(0.320281648576215127537161432847_sp)
            type(ZMM16r4_t),   parameter  :: C21 = ZMM16r4_t(21.0_sp)
            type(ZMM16r4_t),   parameter  :: C14 = ZMM16r4_t(14.0_sp)
            type(ZMM16r4_t),   parameter  :: C1  = ZMM16r4_t(1.0_sp)
            type(ZMM16r4_t),   parameter  :: CN1 = ZMM16r4_t(-1.0_sp)
            type(ZMM16c4),     automatic  :: carg
            type(ZMM16c4),     automatic  :: cexp
            type(ZMM16c4),     automatic  :: ct0
            type(ZMM16r4_t),   automatic  :: sint
            type(ZMM16r4_t),   automatic  :: cost
            type(ZMM16r4_t),   automatic  :: pcos4
            type(ZMM16r4_t),   automatic  :: t0
            type(ZMM16r4_t),   automatic  :: t1
            !dir$ attributes align  : 64  :: C0320281648576215127537161432847
            !dir$ attributes align  : 64  :: C14
            !dir$ attributes align  : 64  :: C21
            !dir$ attributes align  : 64  :: C1
            !dir$ attributes align  : 64  :: carg
            !dir$ attributes align  : 64  :: cexp
            !dir$ attributes align  : 64  :: ct0
            !dir$ attributes align  : 64  :: sint
            !dir$ attributes align  : 64  :: cost
            !dir$ attributes align  : 64  :: pcos4
            !dir$ attributes align  : 64  :: t0
            !dir$ attributes align  : 64  :: t1
            !dir$ attributes align  : 64  :: t2
            ct0    = I
            sint.v = sin(tht.v)
            carg   = ct0*phi
            cost.v = cos(tht.v)
            cexp   = cexp_xmm4c4(carg)
            t0.v   = (C14*cost.v*cost.v)-C1.V
            t1.v   = C21.v*cost.v*cost.v*cost.v*cost.v
            t2.v   = sint.v*(t0.v-t1.v)
            ct0    = cexp*C0320281648576215127537161432847
            y51   = ct0*t2
      end function SphcY5_1_v512b_ps
      
      pure function SphcY5_2_v512b_ps(tht,phi) result(y52)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)          
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY5_2_v512b_ps
            !dir$ attributes forceinline :: SphcY5_2_v512b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY5_2_v512b_ps  
#endif       
            type(ZMM16r4_t),   intent(in) :: tht
            type(ZMM16r4_t),   intent(in) :: phi
            type(ZMM16c4)                 :: y5i2
            type(ZMM16r4_t),   parameter  :: C1694771183260899275815691555511 = 
                                            ZMM16r4_t(1.694771183260899275815691555511_sp)
            type(ZMM16r4_t),   parameter  :: C2 = ZMM16r4_t(-2.0_sp)
            type(ZMM16r4_t),   parameter  :: C3 = ZMM16r4_t(3.0_sp) 
            type(ZMM16c4),     automatic  :: carg
            type(ZMM16c4),     automatic  :: cexp
            type(ZMM16c4),     automatic  :: ct0
            type(ZMM16r4_t),   automatic  :: sint
            type(ZMM16r4_t),   automatic  :: psin
            type(ZMM16r4_t),   automatic  :: cost
            type(ZMM16r4_t),   automatic  :: pcos3
            type(ZMM16r4_t),   automatic  :: t0
             !dir$ attributes align  : 64  :: C1694771183260899275815691555511
             !dir$ attributes align  : 64  :: C2
             !dir$ attributes align  : 64  :: C3
             !dir$ attributes align  : 64  :: carg
             !dir$ attributes align  : 64  :: cexp
             !dir$ attributes align  : 64  :: ct0
             !dir$ attributes align  : 64  :: sint
             !dir$ attributes align  : 64  :: psin
             !dir$ attributes align  : 64  :: cost
             !dir$ attributes align  : 64  :: pcos3
             !dir$ attributes align  : 64  :: t0
            ct0    = I
            cost.v = cos(tht.v)
            carg   = ct0*phi
            pcos3.v= cost.v*cost.v*cost.v
            t0.v   = C3.v*pcos3.v-cost.v
            cexp   = cexp_xmm4c4(carg)
            sint.v = sin(tht.v)
            t0.v   = sint.v*sint.v*t0.v
            ct0    = cexp*C1694771183260899275815691555511
            y52   = ct0*t0
      end function SphcY5_2_v512b_ps
     
      pure function SphcY5_3_v512b_ps(tht,phi) result(y53)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)          
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY5_i3_v512b_ps
            !dir$ attributes forceinline :: SphcY5_i3_v512b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY5_i3_v512b_ps  
#endif       
            type(ZMM16r4_t),   intent(in) :: tht
            type(ZMM16r4_t),   intent(in) :: phi
            type(ZMM16c4)                 :: y53
            type(ZMM16r4_t),   parameter  :: C0345943719146840213165966420433 = &
                                              ZMM16r4_t(0.345943719146840213165966420433_sp)
            type(ZMM16r4_t),   parameter  :: C9 = ZMM16r4_t(9.0_sp)
            type(ZMM16r4_t),   parameter  :: C1 = ZMM16r4_t(1.0_sp)
            type(ZMM16r4_t),   parameter  :: C3 = ZMM16r4_t(-3.0_sp)
            type(ZMM16c4),     automatic  :: carg
            type(ZMM16c4),     automatic  :: cexp
            type(ZMM16c4),     automatic  :: ct0
            type(ZMM16r4_t),   automatic  :: sint
            type(ZMM16r4_t),   automatic  :: psin3
            type(ZMM16r4_t),   automatic  :: cost
            type(ZMM16r4_t),   automatic  :: t0
            !dir$ attributes align  : 64  :: C0345943719146840213165966420433
            !dir$ attributes align  : 64  :: C9
            !dir$ attributes align  : 64  :: C1
            !dir$ attributes align  : 64  :: carg
            !dir$ attributes align  : 64  :: cexp
            !dir$ attributes align  : 64  :: ct0
            !dir$ attributes align  : 64  :: sint
            !dir$ attributes align  : 64  :: psin3
            !dir$ attributes align  : 64  :: cost
            ct0    = I
            carg   = ct0*phi
            sint.v = sin(tht.v)
            cexp   = cexp_xmm4c4(carg)
            cost.v = cos(tht.v)
            t0.v   = (C9.v*cost.v*cost.v)-C1.v
            psin3.v= sint.v*sint.v*sint.v
            ct0    = cexp*C0345943719146840213165966420433
            t0.v   = t0.v*psin3.v
            y53   = ct0*t0
      end function SphcY5_3_v128_ps
      
      pure function SphcY5_4_v512b_ps(tht,phi) result(y54)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)          
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY5_4_v512b_ps
            !dir$ attributes forceinline :: SphcY5_4_v512b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY5_4_v512b_ps  
#endif       
            type(ZMM16r4_t),   intent(in) :: tht
            type(ZMM16r4_t),   intent(in) :: phi
            type(ZMM16c4)                 :: y5i4
            type(ZMM16r4_t),   parameter  :: C1467714898305751163052026147529 = &
                                                 ZMM16r4_t(1.467714898305751163052026147529_sp)
            type(ZMM16r4_t),   parameter  :: C4 = ZMM16r4_t(-4.0_sp)       
            type(ZMM16c4),     automatic  :: carg
            type(ZMM16c4),     automatic  :: cexp
            type(ZMM16c4),     automatic  :: ct0
            type(ZMM16r4_t),   automatic  :: sint
            type(ZMM16r4_t),   automatic  :: cost
            type(ZMM16r4_t),   automatic  :: psin4
            !dir$ attributes align  : 64  ::  C1467714898305751163052026147529
            !dir$ attributes align  : 64  ::  C4
            !dir$ attributes align  : 64  ::  carg
            !dir$ attributes align  : 64  ::  cexp
            !dir$ attributes align  : 64  ::  ct0
            !dir$ attributes align  : 64  ::  sint
            !dir$ attributes align  : 64  ::  cost
            !dir$ attributes align  : 64  ::  psin4
            ct0    =  I
            sint.v =  sin(tht.v)
            carg   =  ct0*phi
            cost.v =  cos(tht.v)
            psin4.v=  sint.v*sint.v*sint.v*sint.v
            cexp   =  cexp_xmm4c4(carg)
            psin4.v=  psin4.v*cost.v
            ct0    =  cexp*psin4
            y54   =  ct0*C1467714898305751163052026147529    
      end function SphcY5_4_v512b_ps
      
      pure function SphcY5_5_v512b_ps(tht,phi) result(y55)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)          
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: SphcY5_5_v512b_ps
            !dir$ attributes forceinline :: SphcY5_5_v512b_ps
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: SphcY5_5_v512b_ps  
#endif       
            type(ZMM16r4_t),   intent(in) :: tht
            type(ZMM16r4_t),   intent(in) :: phi
            type(ZMM16c4)                 :: y5i5
            type(ZMM16r4_t),   parameter  :: C0464132203440858160657998605534 = &
                                                  ZMM16r4_t(0.464132203440858160657998605534_sp)
            type(ZMM16r4_t),   parameter  :: C5  = ZMM16r4_t(-5.0_sp)
            type(ZMM16c4),     automatic  :: carg
            type(ZMM16c4),     automatic  :: cexp
            type(ZMM16c4),     automatic  :: ct0
            type(ZMM16r4_t),   automatic  :: sint
            type(ZMM16r4_t),   automatic  :: psin2
            type(ZMM16r4_t),   automatic  :: psin5
            !dir$ attributes align  : 64  :: C0464132203440858160657998605534
            !dir$ attributes align  : 64  :: C5
            !dir$ attributes align  : 64  :: carg
            !dir$ attributes align  : 64  :: ct0
            !dir$ attributes align  : 64  :: cexp
            !dir$ attributes align  : 64  :: sint
            !dir$ attributes align  : 64  :: psin2
            !dir$ attributes align  : 64  :: psin5
            ct0    = I
            carg   = ct0*phi
            sint.v = sin(tht.v)
            psin2.v= sint.v*sint.v
            cexp   = cexp_xmm4c4(carg)
            psin5.v= psin2.v*psin2.v*sint.v
            ct0    = cexp*C0464132203440858160657998605534
            y55   = ct0*psin5
     end function SphcY5_5_v512b_ps
      

end module sph_complex_zmm16r4      
           
           
       
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     





















