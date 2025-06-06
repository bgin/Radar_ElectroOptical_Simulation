

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

module rcs_cylinder_xmm4r4


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         rcs_cylinder_xmm4r4
 !          
 !          Purpose:
 !                        Various characteristics of analytically derived Radar
 !                        Cross Section of cylindrical objects  
 !                        Based  on George T. Ruck, Donald E. Barrick , William D. Stuart , 
 !                        - "Radar Cross Section Handbook 1 and 2" (1970, Kluwer Academic Plenum Publishers) 
 !                        This module contains only explicitly vectorized (SIMD)
 !                        
 !          History:
 !                        Date: 09-07-2023
 !                        Time: 14:21 GMT+2
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
    use mod_vectypes, only : XMM4r4_t
    use sse_cvec4
    
    public
    implicit none
    
     ! Major version
     integer(kind=i4),  parameter :: RCS_CYLINDER_XMM4R4_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: RCS_CYLINDER_XMM4R4_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: RCS_CYLINDER_XMM4R4_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: RCS_CYLINDER_XMM4R4_FULLVER =   &
            1000*RCS_CYLINDER_XMM4R4_MAJOR+100*RCS_CYLINDER_XMM4R4_MINOR+10*RCS_CYLINDER_XMM4R4_MICRO
     ! Module creation date
     character(*),        parameter :: RCS_CYLINDER_XMM4R4_CREATE_DATE = "09-07-2022 14:30 +00200 (SUN 09 JUL 2023 GMT+2)"
     ! Module build date
     character(*),        parameter :: RCS_CYLINDER_XMM4R4_BUILD_DATE  = __DATE__ " " __TIME__
     ! Module author info
     character(*),        parameter :: RCS_CYLINDER_XMM4R4_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: RCS_CYLINDER_XMM4R4_SYNOPSIS    = "Analytical Cylindrical objects RCS characteristics and models explicitly vectorized (SIMD)."
    
#ifndef __RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__
#define __RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__ 1
#endif 
    
     contains
     
     
                   !/* 
                   !      Low frequency scattering widths (k0a << 1).
                   !      Backscatter scattering width for E-field 
                   !      cylinder-parallel,formula 4.1-19
                   ! */
                   
                   
             pure function rcs_f419_xmm4r4(a,k0a) result(rcs)
                  
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f419_xmm4r4
                   !dir$ attributes forceinline :: rcs_f419_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f419_xmm4r4
                   type(XMM4r4_t),  intent(in) :: a
                   type(XMM4r4_t),  intent(in) :: k0a
                   type(XMM4r4_t) :: rcs
                   ! locals
                   type(XMM4r4_t), parameter :: C9869604401089358618834490999876 =             &
                                                 XMM4r4_t(9.869604401089358618834490999876_sp)
                   type(XMM4r4_t), parameter :: C2467401100272339654708622749969 =             &
                                                 XMM4r4_t(2.467401100272339654708622749969_sp)
                   type(XMM4r4_t), parameter :: C08905  = XMM4r4_t(0.8905_sp)
                     !dir$ attributes align : 16 :: C9869604401089358618834490999876
                     !dir$ attributes align : 16 :: C2467401100272339654708622749969
                     !dir$ attributes align : 16 :: C08905
                     !dir$ attributes align : 16 :: num
                     !dir$ attributes align : 16 :: arg
                     !dir$ attributes align : 16 :: ln
                     !dir$ attributes align : 16 :: ln2
                     !dir$ attributes align : 16 :: den
                   XMM4r4_t, automatic :: num,arg,ln,ln2,den
#if (GMS_EXPLICIT_VECTORIZE) == 1
                   integer(kind=i4) :: j
                    !dir$ loop_count(4)
                    !dir$ vector aligned
                    !dir$ vector vectorlength(4)
                    !dir$ vector always
                    do j=0, 3
                      num.v(j) = a.v(j)* &
                                 C9869604401089358618834490999876.v(j)
                      arg.v(j) = k0a.v(j)*C08905.v(j)
                      ln.v(j)  = log(arg.v(j))
                      ln2.v(j) = ln.v(j)*ln.v(j)
                      den.v(j) = k0a.v(j)*ln2.v(j)+ &
                                 C2467401100272339654708622749969.v(j)
                      rcs.v(j) = num.v(j)/den.v(j)
                   end do
#else                 
                   num.v = a.v*C9869604401089358618834490999876.v
                   arg.v = k0a.v*C08905.v
                   ln.v  = log(arg.v)
                   ln2.v = ln.v*ln.v
                   den.v = k0a.v*ln2.v+C2467401100272339654708622749969.v
                   rcs.v = num.v/den.v
#endif
             end function rcs_f419_xmm4r4
             
             
             subroutine rcs_f419_xmm4r4_unroll16x(pa,pk0a,prcs,n,PF_DIST)
             
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f419_xmm4r4_unroll16x
                   !dir$ attributes forceinline :: rcs_f419_xmm4r4_unroll16x
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f419_xmm4r4_unroll16x
                   type(XMM4r4_t), dimension(1:n), intent(in)    :: pa
                   type(XMM4r4_t), dimension(1:n), intent(in)    :: pk0a
                   type(XMM4r4_t), dimension(1:n), intent(out)   :: prcs 
                   integer(kind=i4)               , intent(in)    :: n
                   integer(kind=i4)               , intent(in)    :: PF_DIST
                   ! Locals
                     !dir$ attributes align : 16 :: a0
                     !dir$ attributes align : 16 :: a1
                     !dir$ attributes align : 16 :: a2
                     !dir$ attributes align : 16 :: a3
                     !dir$ attributes align : 16 :: k0a0
                     !dir$ attributes align : 16 :: k0a1
                     !dir$ attributes align : 16 :: k0a2
                     !dir$ attributes align : 16 :: k0a3
                     !dir$ attributes align : 16 :: rcs0
                     !dir$ attributes align : 16 :: rcs1
                     !dir$ attributes align : 16 :: rcs2
                     !dir$ attributes align : 16 :: rcs3
                   type(XMM4r4_t), automatic :: a0,a1,a2,a3
                   type(XMM4r4_t), automatic :: k0a0,k0a1,k0a2,k0a3
                   type(XMM4r4_t), automatic :: rcs0,rcs1,rcs2,rcs3
                   integer(kind=i4) :: i,m,m1
                   m = mod(n,16)
                   if(m/=0) then
                      do i=1,m
                         a0.v       = pa(i).v
                         k0a0.v     = pk0a(i).v
                         rcs0       = rcs_f419_xmm4r4(a0,k0a0)
                         prcs(i).v  = rcs0.v
                      end do
                      if(n<16) return
                   end if
                   m1=m+1
                    !dir$ assume_aligned pa:16
                    !dir$ assume_aligned pk0a:16
                    !dir$ assume_aligned prcs:16
                    !dir$ vector aligned
                    !dir$ ivdep
                    !dir$ vector vectorlength(4)
                    !dir$ vector multiple_gather_scatter_by_shuffles 
                    !dir$ vector always
                   do i=m1,n,16
#if (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 1
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 2   
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)   
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.) 
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.) 
#endif       
                        a0.v         = pa(i+0).v
                        k0a0.v       = pk0a(i+0).v
                        rcs0         = rcs_f419_xmm4r4(a0,k0a0)
                        prcs(i+0).v  = rcs0.v  
                        a1.v         = pa(i+1).v
                        k0a1.v       = pk0a(i+1).v
                        rcs1         = rcs_f419_xmm4r4(a1,k0a1)
                        prcs(i+1).v  = rcs1.v 
                        a2.v         = pa(i+2).v
                        k0a2.v       = pk0a(i+2).v
                        rcs2         = rcs_f419_xmm4r4(a2,k0a2)
                        prcs(i+2).v  = rcs2.v  
                        a3.v         = pa(i+3).v
                        k0a3.v       = pk0a(i+3).v
                        rcs3         = rcs_f419_xmm4r4(a3,k0a3)
                        prcs(i+3).v  = rcs3.v  
                        a0.v         = pa(i+4).v
                        k0a0.v       = pk0a(i+4).v
                        rcs0         = rcs_f419_xmm4r4(a0,k0a0)
                        prcs(i+4).v  = rcs0.v  
                        a1.v         = pa(i+5).v
                        k0a1.v       = pk0a(i+5).v
                        rcs1         = rcs_f419_xmm4r4(a1,k0a1)
                        prcs(i+5).v  = rcs1.v 
                        a2.v         = pa(i+6).v
                        k0a2.v       = pk0a(i+6).v
                        rcs2         = rcs_f419_xmm4r4(a2,k0a2)
                        prcs(i+6).v  = rcs2.v
                        a3.v         = pa(i+7).v
                        k0a3.v       = pk0a(i+7).v
                        rcs3         = rcs_f419_xmm4r4(a3,k0a3)
                        prcs(i+7).v  = rcs3.v  
                        a0.v         = pa(i+8).v
                        k0a0.v       = pk0a(i+8).v
                        rcs0         = rcs_f419_xmm4r4(a0,k0a0)
                        prcs(i+8).v  = rcs0.v  
                        a1.v         = pa(i+9).v
                        k0a1.v       = pk0a(i+9).v
                        rcs1         = rcs_f419_xmm4r4(a1,k0a1)
                        prcs(i+9).v  = rcs1.v 
                        a2.v         = pa(i+10).v
                        k0a2.v       = pk0a(i+10).v
                        rcs2         = rcs_f419_xmm4r4(a2,k0a2)
                        prcs(i+10).v = rcs2.v
                        a3.v         = pa(i+11).v
                        k0a3.v       = pk0a(i+11).v
                        rcs3         = rcs_f419_xmm4r4(a3,k0a3)
                        prcs(i+11).v = rcs3.v  
                        a0.v         = pa(i+12).v
                        k0a0.v       = pk0a(i+12).v
                        rcs0         = rcs_f419_xmm4r4(a0,k0a0)
                        prcs(i+12).v = rcs0.v  
                        a1.v         = pa(i+13).v
                        k0a1.v       = pk0a(i+13).v
                        rcs1         = rcs_f419_xmm4r4(a1,k0a1)
                        prcs(i+13).v = rcs1.v 
                        a2.v         = pa(i+14).v
                        k0a2.v       = pk0a(i+14).v
                        rcs2         = rcs_f419_xmm4r4(a2,k0a2)
                        prcs(i+14).v = rcs2.v
                        a3.v         = pa(i+15).v
                        k0a3.v       = pk0a(i+15).v
                        rcs3         = rcs_f419_xmm4r4(a3,k0a3)
                        prcs(i+15).v = rcs3.v                      
                   end do
             end subroutine rcs_f419_xmm4r4_unroll16x


             subroutine rcs_f419_xmm4r4_unroll12x(pa,pk0a,prcs,n,PF_DIST)
             
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f419_xmm4r4_unroll12x
                   !dir$ attributes forceinline :: rcs_f419_xmm4r4_unroll12x
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f419_xmm4r4_unroll12x
                   type(XMM4r4_t), dimension(1:n), intent(in)    :: pa
                   type(XMM4r4_t), dimension(1:n), intent(in)    :: pk0a
                   type(XMM4r4_t), dimension(1:n), intent(out)   :: prcs 
                   integer(kind=i4)               , intent(in)    :: n
                   integer(kind=i4)               , intent(in)    :: PF_DIST
                   ! Locals
                     !dir$ attributes align : 16 :: a0
                     !dir$ attributes align : 16 :: a1
                     !dir$ attributes align : 16 :: a2
                     !dir$ attributes align : 16 :: a3
                     !dir$ attributes align : 16 :: k0a0
                     !dir$ attributes align : 16 :: k0a1
                     !dir$ attributes align : 16 :: k0a2
                     !dir$ attributes align : 16 :: k0a3
                     !dir$ attributes align : 16 :: rcs0
                     !dir$ attributes align : 16 :: rcs1
                     !dir$ attributes align : 16 :: rcs2
                     !dir$ attributes align : 16 :: rcs3
                   type(XMM4r4_t), automatic :: a0,a1,a2,a3
                   type(XMM4r4_t), automatic :: k0a0,k0a1,k0a2,k0a3
                   type(XMM4r4_t), automatic :: rcs0,rcs1,rcs2,rcs3
                   integer(kind=i4) :: i,m,m1
                   m = mod(n,12)
                   if(m/=0) then
                      do i=1,m
                         a0.v       = pa(i).v
                         k0a0.v     = pk0a(i).v
                         rcs0       = rcs_f419_xmm4r4(a0,k0a0)
                         prcs(i).v  = rcs0.v
                      end do
                      if(n<12) return
                   end if
                   m1=m+1
                    !dir$ assume_aligned pa:16
                    !dir$ assume_aligned pk0a:16
                    !dir$ assume_aligned prcs:16
                    !dir$ vector aligned
                    !dir$ ivdep
                    !dir$ vector vectorlength(4)
                    !dir$ vector multiple_gather_scatter_by_shuffles 
                    !dir$ vector always
                   do i=m1,n,12
#if (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 1
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 2   
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)   
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.) 
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.) 
#endif       
                        a0.v         = pa(i+0).v
                        k0a0.v       = pk0a(i+0).v
                        rcs0         = rcs_f419_xmm4r4(a0,k0a0)
                        prcs(i+0).v  = rcs0.v  
                        a1.v         = pa(i+1).v
                        k0a1.v       = pk0a(i+1).v
                        rcs1         = rcs_f419_xmm4r4(a1,k0a1)
                        prcs(i+1).v  = rcs1.v 
                        a2.v         = pa(i+2).v
                        k0a2.v       = pk0a(i+2).v
                        rcs2         = rcs_f419_xmm4r4(a2,k0a2)
                        prcs(i+2).v  = rcs2.v  
                        a3.v         = pa(i+3).v
                        k0a3.v       = pk0a(i+3).v
                        rcs3         = rcs_f419_xmm4r4(a3,k0a3)
                        prcs(i+3).v  = rcs3.v  
                        a0.v         = pa(i+4).v
                        k0a0.v       = pk0a(i+4).v
                        rcs0         = rcs_f419_xmm4r4(a0,k0a0)
                        prcs(i+4).v  = rcs0.v  
                        a1.v         = pa(i+5).v
                        k0a1.v       = pk0a(i+5).v
                        rcs1         = rcs_f419_xmm4r4(a1,k0a1)
                        prcs(i+5).v  = rcs1.v 
                        a2.v         = pa(i+6).v
                        k0a2.v       = pk0a(i+6).v
                        rcs2         = rcs_f419_xmm4r4(a2,k0a2)
                        prcs(i+6).v  = rcs2.v
                        a3.v         = pa(i+7).v
                        k0a3.v       = pk0a(i+7).v
                        rcs3         = rcs_f419_xmm4r4(a3,k0a3)
                        prcs(i+7).v  = rcs3.v  
                        a0.v         = pa(i+8).v
                        k0a0.v       = pk0a(i+8).v
                        rcs0         = rcs_f419_xmm4r4(a0,k0a0)
                        prcs(i+8).v  = rcs0.v  
                        a1.v         = pa(i+9).v
                        k0a1.v       = pk0a(i+9).v
                        rcs1         = rcs_f419_xmm4r4(a1,k0a1)
                        prcs(i+9).v  = rcs1.v 
                        a2.v         = pa(i+10).v
                        k0a2.v       = pk0a(i+10).v
                        rcs2         = rcs_f419_xmm4r4(a2,k0a2)
                        prcs(i+10).v = rcs2.v
                        a3.v         = pa(i+11).v
                        k0a3.v       = pk0a(i+11).v
                        rcs3         = rcs_f419_xmm4r4(a3,k0a3)
                        prcs(i+11).v = rcs3.v  
                    end do
             end subroutine rcs_f419_xmm4r4_unroll12x
             
             
             subroutine rcs_f419_xmm4r4_unroll8x(pa,pk0a,prcs,n,PF_DIST)
             
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f419_xmm4r4_unroll8x
                   !dir$ attributes forceinline :: rcs_f419_xmm4r4_unroll8x
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f419_xmm4r4_unroll8x
                   type(XMM4r4_t), dimension(1:n), intent(in)    :: pa
                   type(XMM4r4_t), dimension(1:n), intent(in)    :: pk0a
                   type(XMM4r4_t), dimension(1:n), intent(out)   :: prcs 
                   integer(kind=i4)               , intent(in)    :: n
                   integer(kind=i4)               , intent(in)    :: PF_DIST
                   ! Locals
                     !dir$ attributes align : 16 :: a0
                     !dir$ attributes align : 16 :: a1
                     !dir$ attributes align : 16 :: a2
                     !dir$ attributes align : 16 :: a3
                     !dir$ attributes align : 16 :: k0a0
                     !dir$ attributes align : 16 :: k0a1
                     !dir$ attributes align : 16 :: k0a2
                     !dir$ attributes align : 16 :: k0a3
                     !dir$ attributes align : 16 :: rcs0
                     !dir$ attributes align : 16 :: rcs1
                     !dir$ attributes align : 16 :: rcs2
                     !dir$ attributes align : 16 :: rcs3
                   type(XMM4r4_t), automatic :: a0,a1,a2,a3
                   type(XMM4r4_t), automatic :: k0a0,k0a1,k0a2,k0a3
                   type(XMM4r4_t), automatic :: rcs0,rcs1,rcs2,rcs3
                   integer(kind=i4) :: i,m,m1
                   m = mod(n,8)
                   if(m/=0) then
                      do i=1,m
                         a0.v       = pa(i).v
                         k0a0.v     = pk0a(i).v
                         rcs0       = rcs_f419_xmm4r4(a0,k0a0)
                         prcs(i).v  = rcs0.v
                      end do
                      if(n<8) return
                   end if
                   m1=m+1
                    !dir$ assume_aligned pa:16
                    !dir$ assume_aligned pk0a:16
                    !dir$ assume_aligned prcs:16
                    !dir$ vector aligned
                    !dir$ ivdep
                    !dir$ vector vectorlength(4)
                    !dir$ vector multiple_gather_scatter_by_shuffles 
                    !dir$ vector always
                   do i=m1,n,8
#if (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 1
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 2   
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)   
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.) 
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.) 
#endif       
                        a0.v         = pa(i+0).v
                        k0a0.v       = pk0a(i+0).v
                        rcs0         = rcs_f419_xmm4r4(a0,k0a0)
                        prcs(i+0).v  = rcs0.v  
                        a1.v         = pa(i+1).v
                        k0a1.v       = pk0a(i+1).v
                        rcs1         = rcs_f419_xmm4r4(a1,k0a1)
                        prcs(i+1).v  = rcs1.v 
                        a2.v         = pa(i+2).v
                        k0a2.v       = pk0a(i+2).v
                        rcs2         = rcs_f419_xmm4r4(a2,k0a2)
                        prcs(i+2).v  = rcs2.v  
                        a3.v         = pa(i+3).v
                        k0a3.v       = pk0a(i+3).v
                        rcs3         = rcs_f419_xmm4r4(a3,k0a3)
                        prcs(i+3).v  = rcs3.v  
                        a0.v         = pa(i+4).v
                        k0a0.v       = pk0a(i+4).v
                        rcs0         = rcs_f419_xmm4r4(a0,k0a0)
                        prcs(i+4).v  = rcs0.v  
                        a1.v         = pa(i+5).v
                        k0a1.v       = pk0a(i+5).v
                        rcs1         = rcs_f419_xmm4r4(a1,k0a1)
                        prcs(i+5).v  = rcs1.v 
                        a2.v         = pa(i+6).v
                        k0a2.v       = pk0a(i+6).v
                        rcs2         = rcs_f419_xmm4r4(a2,k0a2)
                        prcs(i+6).v  = rcs2.v
                        a3.v         = pa(i+7).v
                        k0a3.v       = pk0a(i+7).v
                        rcs3         = rcs_f419_xmm4r4(a3,k0a3)
                        prcs(i+7).v  = rcs3.v  
                    end do
             end subroutine rcs_f419_xmm4r4_unroll8x
             
             
             subroutine rcs_f419_xmm4r4_unroll4x(pa,pk0a,prcs,n,PF_DIST)
             
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f419_xmm4r4_unroll4x
                   !dir$ attributes forceinline :: rcs_f419_xmm4r4_unroll4x
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f419_xmm4r4_unroll4x
                   type(XMM4r4_t), dimension(1:n), intent(in)    :: pa
                   type(XMM4r4_t), dimension(1:n), intent(in)    :: pk0a
                   type(XMM4r4_t), dimension(1:n), intent(out)   :: prcs 
                   integer(kind=i4)               , intent(in)    :: n
                   integer(kind=i4)               , intent(in)    :: PF_DIST
                   ! Locals
                     !dir$ attributes align : 16 :: a0
                     !dir$ attributes align : 16 :: a1
                     !dir$ attributes align : 16 :: a2
                     !dir$ attributes align : 16 :: a3
                     !dir$ attributes align : 16 :: k0a0
                     !dir$ attributes align : 16 :: k0a1
                     !dir$ attributes align : 16 :: k0a2
                     !dir$ attributes align : 16 :: k0a3
                     !dir$ attributes align : 16 :: rcs0
                     !dir$ attributes align : 16 :: rcs1
                     !dir$ attributes align : 16 :: rcs2
                     !dir$ attributes align : 16 :: rcs3
                   type(XMM4r4_t), automatic :: a0,a1,a2,a3
                   type(XMM4r4_t), automatic :: k0a0,k0a1,k0a2,k0a3
                   type(XMM4r4_t), automatic :: rcs0,rcs1,rcs2,rcs3
                   integer(kind=i4) :: i,m,m1
                   m = mod(n,4)
                   if(m/=0) then
                      do i=1,m
                         a0.v       = pa(i).v
                         k0a0.v     = pk0a(i).v
                         rcs0       = rcs_f419_xmm4r4(a0,k0a0)
                         prcs(i).v  = rcs0.v
                      end do
                      if(n<4) return
                   end if
                   m1=m+1
                    !dir$ assume_aligned pa:16
                    !dir$ assume_aligned pk0a:16
                    !dir$ assume_aligned prcs:16
                    !dir$ vector aligned
                    !dir$ ivdep
                    !dir$ vector vectorlength(4)
                    !dir$ vector multiple_gather_scatter_by_shuffles 
                    !dir$ vector always
                   do i=m1,n,4
#if (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 1
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 2   
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)   
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.) 
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.) 
#endif       
                        a0.v         = pa(i+0).v
                        k0a0.v       = pk0a(i+0).v
                        rcs0         = rcs_f419_xmm4r4(a0,k0a0)
                        prcs(i+0).v  = rcs0.v  
                        a1.v         = pa(i+1).v
                        k0a1.v       = pk0a(i+1).v
                        rcs1         = rcs_f419_xmm4r4(a1,k0a1)
                        prcs(i+1).v  = rcs1.v 
                        a2.v         = pa(i+2).v
                        k0a2.v       = pk0a(i+2).v
                        rcs2         = rcs_f419_xmm4r4(a2,k0a2)
                        prcs(i+2).v  = rcs2.v  
                        a3.v         = pa(i+3).v
                        k0a3.v       = pk0a(i+3).v
                        rcs3         = rcs_f419_xmm4r4(a3,k0a3)
                        prcs(i+3).v  = rcs3.v  
                    end do
             end subroutine rcs_f419_xmm4r4_unroll4x
             
             
             subroutine rcs_f419_xmm4r4_rolled(pa,pk0a,prcs,n,PF_DIST)
             
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f419_xmm4r4_unroll4x
                   !dir$ attributes forceinline :: rcs_f419_xmm4r4_unroll4x
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f419_xmm4r4_unroll4x
                   type(XMM4r4_t), dimension(1:n), intent(in)    :: pa
                   type(XMM4r4_t), dimension(1:n), intent(in)    :: pk0a
                   type(XMM4r4_t), dimension(1:n), intent(out)   :: prcs 
                   integer(kind=i4)               , intent(in)    :: n
                   integer(kind=i4)               , intent(in)    :: PF_DIST
                   ! Locals
                     !dir$ attributes align : 16 :: a0
                     !dir$ attributes align : 16 :: k0a0
                     !dir$ attributes align : 16 :: rcs0
                   type(XMM4r4_t), automatic :: a0
                   type(XMM4r4_t), automatic :: k0a0
                   type(XMM4r4_t), automatic :: rcs0
                   integer(kind=i4) :: i
                    !dir$ assume_aligned pa:16
                    !dir$ assume_aligned pk0a:16
                    !dir$ assume_aligned prcs:16
                    !dir$ vector aligned
                    !dir$ ivdep
                    !dir$ vector vectorlength(4)
                    !dir$ vector multiple_gather_scatter_by_shuffles 
                    !dir$ vector always
                   do i=1,n
#if (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 1
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 2   
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)   
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.) 
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.) 
#endif       
                        a0.v         = pa(i+0).v
                        k0a0.v       = pk0a(i+0).v
                        rcs0         = rcs_f419_xmm4r4(a0,k0a0)
                        prcs(i+0).v  = rcs0.v  
                     end do
             end subroutine rcs_f419_xmm4r4_rolled
             
             
             
             ! /* 
             !           Low frequency scattering widths (k0a << 1).
             !            Backscatter scattering width for H-field 
             !            cylinder-parallel,formula 4.1-20
             !       */
             
             
             pure function rcs_f4120_xmm4r4(a,k0a) result(rcs)
             
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4120_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4120_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4120_xmm4r4
                   type(XMM4r4_t),  intent(in) :: a
                   type(XMM4r4_t),  intent(in) :: k0a
                   type(XMM4r4_t) :: rcs
                   ! Locals
                  
                   type(XMM4r4_t), parameter :: C9869604401089358618834490999876 = &
                                             XMM4r4_t(9.869604401089358618834490999876_sp)
                   type(XMM4r4_t), parameter :: C225 = XMM4r4_t(2.25_sp)
                     !dir$ attributes align : 16 :: C9869604401089358618834490999876
                     !dir$ attributes align : 16 :: C225
                     !dir$ attributes align : 16 :: pi2a
                     !dir$ attributes align : 16 :: k0a3
                     !dir$ attributes align : 16 :: t0
                   type(XMM4r4_t), automatic :: pi2a,k0a3,t0
#if (GMS_EXPLICIT_VECTORIZE) == 1
                   integer(kind=i4) :: j
                    !dir$ loop_count(4)
                    !dir$ vector aligned
                    !dir$ vector vectorlength(4)
                    !dir$ vector always    
                   do j=0, 3
                      k0a3.v(j) = k0a.v(j)*k0a.v(j)*k0a.v(j)
                      t0.v(j)   = C225.v(j)*k0a3.v(j)
                      pi2a.v(j) = a.v(j)*C9869604401089358618834490999876.v(j)
                      rcs.v(j)  = pi2a.v(j)*t0.v(j)
                  end do
#else              
                   k0a3.v = k0a.v*k0a.v*k0a.v
                   t0.v   = C225.v*k0a3.v
                   pi2a.v = a.v*C9869604401089358618834490999876.v
                   rcs.v  = pi2a.v*t0.v
#endif
             end function rcs_f4120_xmm4r4
             
             
             subroutine rcs_f4120_xmm4r4_unroll16x(pa,pk0a,prcs,n,PF_DIST)
             
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4120_xmm4r4_unroll16x
                   !dir$ attributes forceinline :: rcs_f4120_xmm4r4_unroll16x
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4120_xmm4r4_unroll16x
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pa
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pk0a
                   type(XMM4r4_t), dimension(1:n), intent(out):: prcs
                   integer(kind=i4),                intent(in) :: n
                   integer(kind=i4),                intent(in) :: PF_DIST
                   !Locals
                     !dir$ attributes align : 16 :: a0
                     !dir$ attributes align : 16 :: a1
                     !dir$ attributes align : 16 :: a2
                     !dir$ attributes align : 16 :: a3
                     !dir$ attributes align : 16 :: k0a0
                     !dir$ attributes align : 16 :: k0a1
                     !dir$ attributes align : 16 :: k0a2
                     !dir$ attributes align : 16 :: k0a3
                     !dir$ attributes align : 16 :: rcs0
                     !dir$ attributes align : 16 :: rcs1
                     !dir$ attributes align : 16 :: rcs2
                     !dir$ attributes align : 16 :: rcs3
                   type(XMM4r4_t), automatic :: a0,a1,a2,a3
                   type(XMM4r4_t), automatic :: k0a0,k0a1,k0a2,k0a3
                   type(XMM4r4_t), automatic :: rcs0,rcs1,rcs2,rcs3
                   integer(kind=i4) :: i,m,m1
                   m = mod(n,16)
                   if(m/=0) then
                      do i=1,m
                         a0.v      = pa(i).v
                         k0a.v     = pk0a(i).v
                         rcs.v     = rcs_f4120_xmm4r4(a0,k0a)
                         prcs(i).v = rcs.v
                      end do
                      if(n<16) return
                   end if
                   m1 = m+1
                    !dir$ assume_aligned pa:16
                    !dir$ assume_aligned pk0a:16
                    !dir$ assume_aligned prcs:16
                    !dir$ vector aligned
                    !dir$ ivdep
                    !dir$ vector vectorlength(4)
                    !dir$ vector multiple_gather_scatter_by_shuffles 
                    !dir$ vector always
                   do i=m1,n,16
#if (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 1
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 2   
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)   
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.) 
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.) 
#endif                    
                         a0.v        = pa(i+0).v
                         k0a.v       = pk0a(i+0).v
                         rcs.v       = rcs_f4120_xmm4r4(a0,k0a)
                         prcs(i+0).v = rcs.v
                         a1.v        = pa(i+1).v
                         k0a1.v      = pk0a(i+1).v
                         rcs1.v      = rcs_f4120_xmm4r4(a1,k0a1)
                         prcs(i+1).v = rcs1.v
                         a2.v        = pa(i+2).v
                         k0a2.v      = pk0a(i+2).v
                         rcs2.v      = rcs_f4120_xmm4r4(a2,k0a2)
                         prcs(i+2).v = rcs2.v
                         a3.v        = pa(i+3).v
                         k0a3.v      = pk0a(i+3).v
                         rcs3.v      = rcs_f4120_xmm4r4(a03,k0a3)
                         prcs(i+3).v = rcs3.v
                         a0.v        = pa(i+4).v
                         k0a.v       = pk0a(i+4).v
                         rcs.v       = rcs_f4120_xmm4r4(a0,k0a)
                         prcs(i+4).v = rcs.v
                         a1.v        = pa(i+5).v
                         k0a1.v      = pk0a(i+5).v
                         rcs1.v      = rcs_f4120_xmm4r4(a1,k0a1)
                         prcs(i+5).v = rcs1.v
                         a2.v        = pa(i+6).v
                         k0a2.v      = pk0a(i+6).v
                         rcs2.v      = rcs_f4120_xmm4r4(a2,k0a2)
                         prcs(i+6).v = rcs2.v
                         a3.v        = pa(i+7).v
                         k0a3.v      = pk0a(i+7).v
                         rcs3.v      = rcs_f4120_xmm4r4(a03,k0a3)
                         prcs(i+7).v = rcs3.v
                         a0.v        = pa(i+8).v
                         k0a.v       = pk0a(i+8).v
                         rcs.v       = rcs_f4120_xmm4r4(a0,k0a)
                         prcs(i+8).v = rcs.v
                         a1.v        = pa(i+9).v
                         k0a1.v      = pk0a(i+9).v
                         rcs1.v      = rcs_f4120_xmm4r4(a1,k0a1)
                         prcs(i+9).v = rcs1.v
                         a2.v        = pa(i+10).v
                         k0a2.v      = pk0a(i+10).v
                         rcs2.v      = rcs_f4120_xmm4r4(a2,k0a2)
                         prcs(i+10).v = rcs2.v
                         a3.v        = pa(i+11).v
                         k0a3.v      = pk0a(i+11).v
                         rcs3.v      = rcs_f4120_xmm4r4(a03,k0a3)
                         prcs(i+11).v = rcs3.v
                         a0.v        = pa(i+12).v
                         k0a.v       = pk0a(i+12).v
                         rcs.v       = rcs_f4120_xmm4r4(a0,k0a)
                         prcs(i+12).v = rcs.v
                         a1.v        = pa(i+13).v
                         k0a1.v      = pk0a(i+13).v
                         rcs1.v      = rcs_f4120_xmm4r4(a1,k0a1)
                         prcs(i+13).v = rcs1.v
                         a2.v        = pa(i+14).v
                         k0a2.v      = pk0a(i+14).v
                         rcs2.v      = rcs_f4120_xmm4r4(a2,k0a2)
                         prcs(i+14).v = rcs2.v
                         a3.v        = pa(i+15).v
                         k0a3.v      = pk0a(i+15).v
                         rcs3.v      = rcs_f4120_xmm4r4(a03,k0a3)
                         prcs(i+15).v = rcs3.v
                   end do
             end subroutine rcs_f4120_xmm4r4_unroll16x
             
             
             subroutine rcs_f4120_xmm4r4_unroll12x(pa,pk0a,prcs,n,PF_DIST)
             
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4120_xmm4r4_unroll12x
                   !dir$ attributes forceinline :: rcs_f4120_xmm4r4_unroll12x
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4120_xmm4r4_unroll12x
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pa
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pk0a
                   type(XMM4r4_t), dimension(1:n), intent(out):: prcs
                   integer(kind=i4),                intent(in) :: n
                   integer(kind=i4),                intent(in) :: PF_DIST
                   !Locals
                     !dir$ attributes align : 16 :: a0
                     !dir$ attributes align : 16 :: a1
                     !dir$ attributes align : 16 :: a2
                     !dir$ attributes align : 16 :: a3
                     !dir$ attributes align : 16 :: k0a0
                     !dir$ attributes align : 16 :: k0a1
                     !dir$ attributes align : 16 :: k0a2
                     !dir$ attributes align : 16 :: k0a3
                     !dir$ attributes align : 16 :: rcs0
                     !dir$ attributes align : 16 :: rcs1
                     !dir$ attributes align : 16 :: rcs2
                     !dir$ attributes align : 16 :: rcs3
                   type(XMM4r4_t), automatic :: a0,a1,a2,a3
                   type(XMM4r4_t), automatic :: k0a0,k0a1,k0a2,k0a3
                   type(XMM4r4_t), automatic :: rcs0,rcs1,rcs2,rcs3
                   integer(kind=i4) :: i,m,m1
                   m = mod(n,12)
                   if(m/=0) then
                      do i=1,m
                         a0.v      = pa(i).v
                         k0a.v     = pk0a(i).v
                         rcs.v     = rcs_f4120_xmm4r4(a0,k0a)
                         prcs(i).v = rcs.v
                      end do
                      if(n<12) return
                   end if
                   m1 = m+1
                    !dir$ assume_aligned pa:16
                    !dir$ assume_aligned pk0a:16
                    !dir$ assume_aligned prcs:16
                    !dir$ vector aligned
                    !dir$ ivdep
                    !dir$ vector vectorlength(4)
                    !dir$ vector multiple_gather_scatter_by_shuffles 
                    !dir$ vector always
                   do i=m1,n,12
#if (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 1
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 2   
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)   
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.) 
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.) 
#endif                    
                         a0.v        = pa(i+0).v
                         k0a.v       = pk0a(i+0).v
                         rcs.v       = rcs_f4120_xmm4r4(a0,k0a)
                         prcs(i+0).v = rcs.v
                         a1.v        = pa(i+1).v
                         k0a1.v      = pk0a(i+1).v
                         rcs1.v      = rcs_f4120_xmm4r4(a1,k0a1)
                         prcs(i+1).v = rcs1.v
                         a2.v        = pa(i+2).v
                         k0a2.v      = pk0a(i+2).v
                         rcs2.v      = rcs_f4120_xmm4r4(a2,k0a2)
                         prcs(i+2).v = rcs2.v
                         a3.v        = pa(i+3).v
                         k0a3.v      = pk0a(i+3).v
                         rcs3.v      = rcs_f4120_xmm4r4(a03,k0a3)
                         prcs(i+3).v = rcs3.v
                         a0.v        = pa(i+4).v
                         k0a.v       = pk0a(i+4).v
                         rcs.v       = rcs_f4120_xmm4r4(a0,k0a)
                         prcs(i+4).v = rcs.v
                         a1.v        = pa(i+5).v
                         k0a1.v      = pk0a(i+5).v
                         rcs1.v      = rcs_f4120_xmm4r4(a1,k0a1)
                         prcs(i+5).v = rcs1.v
                         a2.v        = pa(i+6).v
                         k0a2.v      = pk0a(i+6).v
                         rcs2.v      = rcs_f4120_xmm4r4(a2,k0a2)
                         prcs(i+6).v = rcs2.v
                         a3.v        = pa(i+7).v
                         k0a3.v      = pk0a(i+7).v
                         rcs3.v      = rcs_f4120_xmm4r4(a03,k0a3)
                         prcs(i+7).v = rcs3.v
                         a0.v        = pa(i+8).v
                         k0a.v       = pk0a(i+8).v
                         rcs.v       = rcs_f4120_xmm4r4(a0,k0a)
                         prcs(i+8).v = rcs.v
                         a1.v        = pa(i+9).v
                         k0a1.v      = pk0a(i+9).v
                         rcs1.v      = rcs_f4120_xmm4r4(a1,k0a1)
                         prcs(i+9).v = rcs1.v
                         a2.v        = pa(i+10).v
                         k0a2.v      = pk0a(i+10).v
                         rcs2.v      = rcs_f4120_xmm4r4(a2,k0a2)
                         prcs(i+10).v = rcs2.v
                         a3.v        = pa(i+11).v
                         k0a3.v      = pk0a(i+11).v
                         rcs3.v      = rcs_f4120_xmm4r4(a03,k0a3)
                         prcs(i+11).v = rcs3.v
                     end do
             end subroutine rcs_f4120_xmm4r4_unroll12x
             

             subroutine rcs_f4120_xmm4r4_unroll8x(pa,pk0a,prcs,n,PF_DIST)
             
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4120_xmm4r4_unroll8x
                   !dir$ attributes forceinline :: rcs_f4120_xmm4r4_unroll8x
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4120_xmm4r4_unroll8x
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pa
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pk0a
                   type(XMM4r4_t), dimension(1:n), intent(out):: prcs
                   integer(kind=i4),                intent(in) :: n
                   integer(kind=i4),                intent(in) :: PF_DIST
                   !Locals
                     !dir$ attributes align : 16 :: a0
                     !dir$ attributes align : 16 :: a1
                     !dir$ attributes align : 16 :: a2
                     !dir$ attributes align : 16 :: a3
                     !dir$ attributes align : 16 :: k0a0
                     !dir$ attributes align : 16 :: k0a1
                     !dir$ attributes align : 16 :: k0a2
                     !dir$ attributes align : 16 :: k0a3
                     !dir$ attributes align : 16 :: rcs0
                     !dir$ attributes align : 16 :: rcs1
                     !dir$ attributes align : 16 :: rcs2
                     !dir$ attributes align : 16 :: rcs3
                   type(XMM4r4_t), automatic :: a0,a1,a2,a3
                   type(XMM4r4_t), automatic :: k0a0,k0a1,k0a2,k0a3
                   type(XMM4r4_t), automatic :: rcs0,rcs1,rcs2,rcs3
                   integer(kind=i4) :: i,m,m1
                   m = mod(n,8)
                   if(m/=0) then
                      do i=1,m
                         a0.v      = pa(i).v
                         k0a.v     = pk0a(i).v
                         rcs.v     = rcs_f4120_xmm4r4(a0,k0a)
                         prcs(i).v = rcs.v
                      end do
                      if(n<8) return
                   end if
                   m1 = m+1
                    !dir$ assume_aligned pa:16
                    !dir$ assume_aligned pk0a:16
                    !dir$ assume_aligned prcs:16
                    !dir$ vector aligned
                    !dir$ ivdep
                    !dir$ vector vectorlength(4)
                    !dir$ vector multiple_gather_scatter_by_shuffles 
                    !dir$ vector always
                   do i=m1,n,8
#if (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 1
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 2   
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)   
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.) 
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.) 
#endif                    
                         a0.v        = pa(i+0).v
                         k0a.v       = pk0a(i+0).v
                         rcs.v       = rcs_f4120_xmm4r4(a0,k0a)
                         prcs(i+0).v = rcs.v
                         a1.v        = pa(i+1).v
                         k0a1.v      = pk0a(i+1).v
                         rcs1.v      = rcs_f4120_xmm4r4(a1,k0a1)
                         prcs(i+1).v = rcs1.v
                         a2.v        = pa(i+2).v
                         k0a2.v      = pk0a(i+2).v
                         rcs2.v      = rcs_f4120_xmm4r4(a2,k0a2)
                         prcs(i+2).v = rcs2.v
                         a3.v        = pa(i+3).v
                         k0a3.v      = pk0a(i+3).v
                         rcs3.v      = rcs_f4120_xmm4r4(a03,k0a3)
                         prcs(i+3).v = rcs3.v
                         a0.v        = pa(i+4).v
                         k0a.v       = pk0a(i+4).v
                         rcs.v       = rcs_f4120_xmm4r4(a0,k0a)
                         prcs(i+4).v = rcs.v
                         a1.v        = pa(i+5).v
                         k0a1.v      = pk0a(i+5).v
                         rcs1.v      = rcs_f4120_xmm4r4(a1,k0a1)
                         prcs(i+5).v = rcs1.v
                         a2.v        = pa(i+6).v
                         k0a2.v      = pk0a(i+6).v
                         rcs2.v      = rcs_f4120_xmm4r4(a2,k0a2)
                         prcs(i+6).v = rcs2.v
                         a3.v        = pa(i+7).v
                         k0a3.v      = pk0a(i+7).v
                         rcs3.v      = rcs_f4120_xmm4r4(a03,k0a3)
                         prcs(i+7).v = rcs3.v
                     end do
             end subroutine rcs_f4120_xmm4r4_unroll8x
             

             subroutine rcs_f4120_xmm4r4_unroll4x(pa,pk0a,prcs,n,PF_DIST)
             
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4120_xmm4r4_unroll4x
                   !dir$ attributes forceinline :: rcs_f4120_xmm4r4_unroll4x
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4120_xmm4r4_unroll4x
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pa
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pk0a
                   type(XMM4r4_t), dimension(1:n), intent(out):: prcs
                   integer(kind=i4),                intent(in) :: n
                   integer(kind=i4),                intent(in) :: PF_DIST
                   !Locals
                     !dir$ attributes align : 16 :: a0
                     !dir$ attributes align : 16 :: a1
                     !dir$ attributes align : 16 :: a2
                     !dir$ attributes align : 16 :: a3
                     !dir$ attributes align : 16 :: k0a0
                     !dir$ attributes align : 16 :: k0a1
                     !dir$ attributes align : 16 :: k0a2
                     !dir$ attributes align : 16 :: k0a3
                     !dir$ attributes align : 16 :: rcs0
                     !dir$ attributes align : 16 :: rcs1
                     !dir$ attributes align : 16 :: rcs2
                     !dir$ attributes align : 16 :: rcs3
                   type(XMM4r4_t), automatic :: a0,a1,a2,a3
                   type(XMM4r4_t), automatic :: k0a0,k0a1,k0a2,k0a3
                   type(XMM4r4_t), automatic :: rcs0,rcs1,rcs2,rcs3
                   integer(kind=i4) :: i,m,m1
                   m = mod(n,4)
                   if(m/=0) then
                      do i=1,m
                         a0.v      = pa(i).v
                         k0a.v     = pk0a(i).v
                         rcs.v     = rcs_f4120_xmm4r4(a0,k0a)
                         prcs(i).v = rcs.v
                      end do
                      if(n<4) return
                   end if
                   m1 = m+1
                    !dir$ assume_aligned pa:16
                    !dir$ assume_aligned pk0a:16
                    !dir$ assume_aligned prcs:16
                    !dir$ vector aligned
                    !dir$ ivdep
                    !dir$ vector vectorlength(4)
                    !dir$ vector multiple_gather_scatter_by_shuffles 
                    !dir$ vector always
                   do i=m1,n,4
#if (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 1
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 2   
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)   
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.) 
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.) 
#endif                    
                         a0.v        = pa(i+0).v
                         k0a.v       = pk0a(i+0).v
                         rcs.v       = rcs_f4120_xmm4r4(a0,k0a)
                         prcs(i+0).v = rcs.v
                         a1.v        = pa(i+1).v
                         k0a1.v      = pk0a(i+1).v
                         rcs1.v      = rcs_f4120_xmm4r4(a1,k0a1)
                         prcs(i+1).v = rcs1.v
                         a2.v        = pa(i+2).v
                         k0a2.v      = pk0a(i+2).v
                         rcs2.v      = rcs_f4120_xmm4r4(a2,k0a2)
                         prcs(i+2).v = rcs2.v
                         a3.v        = pa(i+3).v
                         k0a3.v      = pk0a(i+3).v
                         rcs3.v      = rcs_f4120_xmm4r4(a03,k0a3)
                         prcs(i+3).v = rcs3.v
                      end do
             end subroutine rcs_f4120_xmm4r4_unroll4x
             
         
             subroutine rcs_f4120_xmm4r4_rolled(pa,pk0a,prcs,n,PF_DIST)
             
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4120_xmm4r4_rolled
                   !dir$ attributes forceinline :: rcs_f4120_xmm4r4_rolled
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4120_xmm4r4_rolled
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pa
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pk0a
                   type(XMM4r4_t), dimension(1:n), intent(out):: prcs
                   integer(kind=i4),                intent(in) :: n
                   integer(kind=i4),                intent(in) :: PF_DIST
                   !Locals
                     !dir$ attributes align : 16 :: a0
                     !dir$ attributes align : 16 :: k0a0
                     !dir$ attributes align : 16 :: rcs0
                  
                   type(XMM4r4_t), automatic :: a0
                   type(XMM4r4_t), automatic :: k0a0
                   type(XMM4r4_t), automatic :: rcs0
                   integer(kind=i4) :: i
                  
                    !dir$ assume_aligned pa:16
                    !dir$ assume_aligned pk0a:16
                    !dir$ assume_aligned prcs:16
                    !dir$ vector aligned
                    !dir$ ivdep
                    !dir$ vector vectorlength(4)
                    !dir$ vector multiple_gather_scatter_by_shuffles 
                    !dir$ vector always
                   do i=1,n
#if (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 1
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 2   
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)   
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.) 
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.) 
#endif                    
                         a0.v        = pa(i+0).v
                         k0a.v       = pk0a(i+0).v
                         rcs.v       = rcs_f4120_xmm4r4(a0,k0a)
                         prcs(i+0).v = rcs.v
                     end do
             end subroutine rcs_f4120_xmm4r4_rolled
             
             
             ! /*
             !           Bistatic scattering widths, E-field cylinder axis-parallel
             !           Formula 4.1-21
             !      */
             
             pure function rcs_f4121_xmm4r4(a,k0a) result(rcs)
                  
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4121_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4121_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4121_xmm4r4
                   type(XMM4r4_t), intent(in) :: a
                   type(XMM4r4_t), intent(in) :: k0a
                   type(XMM4r4_t) :: rcs
                   rcs = rcs_f4120_xmm4r4(a,k0a)
             end function rcs_f4121_xmm4r4
             
             
              !/*
              !          Bistatic scattering widths, H-field cylinder axis-parallel
              !          Formula 4.1-22
              !     */ 
              
              
              pure function rcs_f4122_xmm4r4(phi,a,k0a) result(rcs)
              
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4122_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4122_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4122_xmm4r4 
                   type(XMM4r4_t),   intent(in) :: phi
                   type(XMM4r4_t),   intent(in) :: a
                   type(XMM4r4_t),   intent(in) :: k0a
                   type(XMM4r4_t) :: rcs
                   ! Locals
                     !dir$ attributes align : 16 :: C9869604401089358618834490999876
                     !dir$ attributes align : 16 :: C05
                     !dir$ attributes align : 16 :: pi2a
                     !dir$ attributes align : 16 :: k0a3
                     !dir$ attributes align : 16 :: cosp
                     !dir$ attributes align : 16 :: frac
                     !dir$ attributes align : 16 :: sqr
                   type(XMM4r4_t), parameter :: C9869604401089358618834490999876 = &
                                              XMM4r4_t(9.869604401089358618834490999876_sp)
                   type(XMM4r4_t), parameter :: C05 = XMM4r4_t(0.5_sp)
                   type(XMM4r4_t), automatic :: pi2a,k0a3,cosp,frac,sqr
#if (GMS_EXPLICIT_VECTORIZE) == 1
                    integer(kind=i4) :: j
                    !dir$ loop_count(4)
                    !dir$ vector aligned
                    !dir$ vector vectorlength(4)
                    !dir$ vector always
                   do j=0, 3
                       pi2a.v(j) = a.v(j)*C9869604401089358618834490999876.v(j)
                       k0a3.v(j) = k0a.v(j)*k0a.v(j)*k0a.v(j)
                       cosp.v(j) = cos(phi.v(j))
                       frac.v(j) = C05.v(j)*cosp.v(j)
                       sqr.v(j)  = frac.v(j)*frac.v(j)
                       rcs.v(j)  = pi2a.v(j)*k0a3.v(j)*sqr.v(j)
                   end do
#else
                   pi2a.v = a.v*C9869604401089358618834490999876.v
                   k0a3.v = k0a.v*k0a.v*k0a.v
                   cosp.v = cos(phi.v)
                   frac.v = C05.v*cosp.v
                   sqr.v  = frac.v*frac.v
                   rcs.v  = pi2a.v*k0a3.v*sqr.v
#endif
              end function rcs_f4122_xmm4r4
              
              
              subroutine rcs_f4122_xmm4r4_unroll16x(pphi,pa,pk0a,prcs,n,PF_DIST)
              
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4122_xmm4r4_unroll16x
                   !dir$ attributes forceinline :: rcs_f4122_xmm4r4_unroll16x
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4122_xmm4r4_unroll16x
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pphi
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pa
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pk0a
                   type(XMM4r4_t), dimension(1:n), intent(out):: prcs
                   integer(kind=i4),                intent(in) :: n
                   integer(kind=i4),                intent(in) :: PF_DIST
                   ! Locals
                     !dir$ attributes align : 16 :: a0
                     !dir$ attributes align : 16 :: a1
                     !dir$ attributes align : 16 :: a2
                     !dir$ attributes align : 16 :: a3
                     !dir$ attributes align : 16 :: k0a0
                     !dir$ attributes align : 16 :: k0a1
                     !dir$ attributes align : 16 :: k0a2
                     !dir$ attributes align : 16 :: k0a3
                     !dir$ attributes align : 16 :: rcs0
                     !dir$ attributes align : 16 :: rcs1
                     !dir$ attributes align : 16 :: rcs2
                     !dir$ attributes align : 16 :: rcs3
                   type(XMM4r4_t), automatic :: phi0,phi1,phi2,phi3
                   type(XMM4r4_t), automatic :: a0,a1,a2,a3
                   type(XMM4r4_t), automatic :: k0a0,k0a1,k0a2,k0a3
                   type(XMM4r4_t), automatic :: rcs0,rcs1,rcs2,rcs3
                   integer(kind=i4) :: i,m,m1
                   m = mod(n,16)
                   if(m/=0) then
                      do i=1,m
                         phi0.v    = pphi(i).v
                         a0.v      = pa(i).v
                         k0a0.v    = pk0a(i).v
                         rcs0.v    = rcs_f4122_xmm4r4(phi0,a0,k0a0)
                         prcs(i).v = rcs0.v
                      end do
                      if(n<16) return
                   end if
                   m1 = m+1
                    !dir$ assume_aligned pa:16
                    !dir$ assume_aligned pk0a:16
                    !dir$ assume_aligned prcs:16
                    !dir$ assume_aligned pphi:16
                    !dir$ vector aligned
                    !dir$ ivdep
                    !dir$ vector vectorlength(4)
                    !dir$ vector multiple_gather_scatter_by_shuffles 
                    !dir$ vector always
                  do i=m1,n,16
#if (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 1
                       call mm_prefetch(pphi(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 2   
                       call mm_prefetch(pphi(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)   
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                       call mm_prefetch(pphi(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.) 
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                       call mm_prefetch(pphi(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.) 
#endif                  
                         phi0.v      = pphi(i+0).v
                         a0.v        = pa(i+0).v
                         k0a0.v      = pk0a(i+0).v
                         rcs0.v      = rcs_f4122_xmm4r4(phi0,a0,k0a0)
                         prcs(i+0).v = rcs0.v
                         phi1.v      = pphi(i+1).v
                         a1.v        = pa(i+1).v
                         k0a1.v      = pk0a(i+1).v
                         rcs1.v      = rcs_f4122_xmm4r4(phi1,a1,k0a1)
                         prcs(i+1).v = rcs1.v
                         phi2.v      = pphi(i+2).v
                         a2.v        = pa(i+2).v
                         k0a2.v      = pk0a(i+2).v
                         rcs2.v      = rcs_f4122_xmm4r4(phi2,a2,k0a2)
                         prcs(i+2).v = rcs2.v   
                         phi3.v      = pphi(i+3).v
                         a3.v        = pa(i+3).v
                         k0a3.v      = pk0a(i+3).v
                         rcs3.v      = rcs_f4122_xmm4r4(phi3,a3,k0a3)
                         prcs(i+3).v = rcs3.v
                         phi0.v      = pphi(i+4).v
                         a0.v        = pa(i+4).v
                         k0a0.v      = pk0a(i+4).v
                         rcs0.v      = rcs_f4122_xmm4r4(phi0,a0,k0a0)
                         prcs(i+4).v = rcs0.v
                         phi1.v      = pphi(i+5).v
                         a1.v        = pa(i+5).v
                         k0a1.v      = pk0a(i+5).v
                         rcs1.v      = rcs_f4122_xmm4r4(phi1,a1,k0a1)
                         prcs(i+5).v = rcs1.v
                         phi2.v      = pphi(i+6).v
                         a2.v        = pa(i+6).v
                         k0a2.v      = pk0a(i+6).v
                         rcs2.v      = rcs_f4122_xmm4r4(phi2,a2,k0a2)
                         prcs(i+6).v = rcs2.v   
                         phi3.v      = pphi(i+7).v
                         a3.v        = pa(i+7).v
                         k0a3.v      = pk0a(i+7).v
                         rcs3.v      = rcs_f4122_xmm4r4(phi3,a3,k0a3)
                         prcs(i+7).v = rcs3.v
                         phi0.v      = pphi(i+8).v
                         a0.v        = pa(i+8).v
                         k0a0.v      = pk0a(i+8).v
                         rcs0.v      = rcs_f4122_xmm4r4(phi0,a0,k0a0)
                         prcs(i+8).v = rcs0.v
                         phi1.v      = pphi(i+9).v
                         a1.v        = pa(i+9).v
                         k0a1.v      = pk0a(i+9).v
                         rcs1.v      = rcs_f4122_xmm4r4(phi1,a1,k0a1)
                         prcs(i+9).v = rcs1.v
                         phi2.v      = pphi(i+10).v
                         a2.v        = pa(i+10).v
                         k0a2.v      = pk0a(i+10).v
                         rcs2.v      = rcs_f4122_xmm4r4(phi2,a2,k0a2)
                         prcs(i+10).v= rcs2.v   
                         phi3.v      = pphi(i+11).v
                         a3.v        = pa(i+11).v
                         k0a3.v      = pk0a(i+11).v
                         rcs3.v      = rcs_f4122_xmm4r4(phi3,a3,k0a3)
                         prcs(i+11).v = rcs3.v
                         phi0.v      = pphi(i+12).v
                         a0.v        = pa(i+12).v
                         k0a0.v      = pk0a(i+12).v
                         rcs0.v      = rcs_f4122_xmm4r4(phi0,a0,k0a0)
                         prcs(i+12).v = rcs0.v
                         phi1.v      = pphi(i+13).v
                         a1.v        = pa(i+13).v
                         k0a1.v      = pk0a(i+13).v
                         rcs1.v      = rcs_f4122_xmm4r4(phi1,a1,k0a1)
                         prcs(i+13).v = rcs1.v
                         phi2.v      = pphi(i+14).v
                         a2.v        = pa(i+14).v
                         k0a2.v      = pk0a(i+14).v
                         rcs2.v      = rcs_f4122_xmm4r4(phi2,a2,k0a2)
                         prcs(i+14).v = rcs2.v   
                         phi3.v      = pphi(i+15).v
                         a3.v        = pa(i+15).v
                         k0a3.v      = pk0a(i+15).v
                         rcs3.v      = rcs_f4122_xmm4r4(phi3,a3,k0a3)
                         prcs(i+15).v = rcs3.v
                  end do
              end subroutine rcs_f4122_xmm4r4_unroll16x
              
             
              subroutine rcs_f4122_xmm4r4_unroll12x(pphi,pa,pk0a,prcs,n,PF_DIST)
              
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4122_xmm4r4_unroll12x
                   !dir$ attributes forceinline :: rcs_f4122_xmm4r4_unroll12x
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4122_xmm4r4_unroll12x
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pphi
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pa
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pk0a
                   type(XMM4r4_t), dimension(1:n), intent(out):: prcs
                   integer(kind=i4),                intent(in) :: n
                   integer(kind=i4),                intent(in) :: PF_DIST
                   ! Locals
                     !dir$ attributes align : 16 :: a0
                     !dir$ attributes align : 16 :: a1
                     !dir$ attributes align : 16 :: a2
                     !dir$ attributes align : 16 :: a3
                     !dir$ attributes align : 16 :: k0a0
                     !dir$ attributes align : 16 :: k0a1
                     !dir$ attributes align : 16 :: k0a2
                     !dir$ attributes align : 16 :: k0a3
                     !dir$ attributes align : 16 :: rcs0
                     !dir$ attributes align : 16 :: rcs1
                     !dir$ attributes align : 16 :: rcs2
                     !dir$ attributes align : 16 :: rcs3
                   type(XMM4r4_t), automatic :: phi0,phi1,phi2,phi3
                   type(XMM4r4_t), automatic :: a0,a1,a2,a3
                   type(XMM4r4_t), automatic :: k0a0,k0a1,k0a2,k0a3
                   type(XMM4r4_t), automatic :: rcs0,rcs1,rcs2,rcs3
                   integer(kind=i4) :: i,m,m1
                   m = mod(n,12)
                   if(m/=0) then
                      do i=1,m
                         phi0.v    = pphi(i).v
                         a0.v      = pa(i).v
                         k0a0.v    = pk0a(i).v
                         rcs0.v    = rcs_f4122_xmm4r4(phi0,a0,k0a0)
                         prcs(i).v = rcs0.v
                      end do
                      if(n<12) return
                   end if
                   m1 = m+1
                    !dir$ assume_aligned pa:16
                    !dir$ assume_aligned pk0a:16
                    !dir$ assume_aligned prcs:16
                    !dir$ assume_aligned pphi:16
                    !dir$ vector aligned
                    !dir$ ivdep
                    !dir$ vector vectorlength(4)
                    !dir$ vector multiple_gather_scatter_by_shuffles 
                    !dir$ vector always
                  do i=m1,n,12
#if (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 1
                       call mm_prefetch(pphi(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 2   
                       call mm_prefetch(pphi(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)   
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                       call mm_prefetch(pphi(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.) 
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                       call mm_prefetch(pphi(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.) 
#endif                  
                         phi0.v      = pphi(i+0).v
                         a0.v        = pa(i+0).v
                         k0a0.v      = pk0a(i+0).v
                         rcs0.v      = rcs_f4122_xmm4r4(phi0,a0,k0a0)
                         prcs(i+0).v = rcs0.v
                         phi1.v      = pphi(i+1).v
                         a1.v        = pa(i+1).v
                         k0a1.v      = pk0a(i+1).v
                         rcs1.v      = rcs_f4122_xmm4r4(phi1,a1,k0a1)
                         prcs(i+1).v = rcs1.v
                         phi2.v      = pphi(i+2).v
                         a2.v        = pa(i+2).v
                         k0a2.v      = pk0a(i+2).v
                         rcs2.v      = rcs_f4122_xmm4r4(phi2,a2,k0a2)
                         prcs(i+2).v = rcs2.v   
                         phi3.v      = pphi(i+3).v
                         a3.v        = pa(i+3).v
                         k0a3.v      = pk0a(i+3).v
                         rcs3.v      = rcs_f4122_xmm4r4(phi3,a3,k0a3)
                         prcs(i+3).v = rcs3.v
                         phi0.v      = pphi(i+4).v
                         a0.v        = pa(i+4).v
                         k0a0.v      = pk0a(i+4).v
                         rcs0.v      = rcs_f4122_xmm4r4(phi0,a0,k0a0)
                         prcs(i+4).v = rcs0.v
                         phi1.v      = pphi(i+5).v
                         a1.v        = pa(i+5).v
                         k0a1.v      = pk0a(i+5).v
                         rcs1.v      = rcs_f4122_xmm4r4(phi1,a1,k0a1)
                         prcs(i+5).v = rcs1.v
                         phi2.v      = pphi(i+6).v
                         a2.v        = pa(i+6).v
                         k0a2.v      = pk0a(i+6).v
                         rcs2.v      = rcs_f4122_xmm4r4(phi2,a2,k0a2)
                         prcs(i+6).v = rcs2.v   
                         phi3.v      = pphi(i+7).v
                         a3.v        = pa(i+7).v
                         k0a3.v      = pk0a(i+7).v
                         rcs3.v      = rcs_f4122_xmm4r4(phi3,a3,k0a3)
                         prcs(i+7).v = rcs3.v
                         phi0.v      = pphi(i+8).v
                         a0.v        = pa(i+8).v
                         k0a0.v      = pk0a(i+8).v
                         rcs0.v      = rcs_f4122_xmm4r4(phi0,a0,k0a0)
                         prcs(i+8).v = rcs0.v
                         phi1.v      = pphi(i+9).v
                         a1.v        = pa(i+9).v
                         k0a1.v      = pk0a(i+9).v
                         rcs1.v      = rcs_f4122_xmm4r4(phi1,a1,k0a1)
                         prcs(i+9).v = rcs1.v
                         phi2.v      = pphi(i+10).v
                         a2.v        = pa(i+10).v
                         k0a2.v      = pk0a(i+10).v
                         rcs2.v      = rcs_f4122_xmm4r4(phi2,a2,k0a2)
                         prcs(i+10).v= rcs2.v   
                         phi3.v      = pphi(i+11).v
                         a3.v        = pa(i+11).v
                         k0a3.v      = pk0a(i+11).v
                         rcs3.v      = rcs_f4122_xmm4r4(phi3,a3,k0a3)
                         prcs(i+11).v = rcs3.v
                      end do
              end subroutine rcs_f4122_xmm4r4_unroll12x
              

              subroutine rcs_f4122_xmm4r4_unroll8x(pphi,pa,pk0a,prcs,n,PF_DIST)
              
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4122_xmm4r4_unroll8x
                   !dir$ attributes forceinline :: rcs_f4122_xmm4r4_unroll8x
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4122_xmm4r4_unroll8x
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pphi
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pa
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pk0a
                   type(XMM4r4_t), dimension(1:n), intent(out):: prcs
                   integer(kind=i4),                intent(in) :: n
                   integer(kind=i4),                intent(in) :: PF_DIST
                   ! Locals
                     !dir$ attributes align : 16 :: a0
                     !dir$ attributes align : 16 :: a1
                     !dir$ attributes align : 16 :: a2
                     !dir$ attributes align : 16 :: a3
                     !dir$ attributes align : 16 :: k0a0
                     !dir$ attributes align : 16 :: k0a1
                     !dir$ attributes align : 16 :: k0a2
                     !dir$ attributes align : 16 :: k0a3
                     !dir$ attributes align : 16 :: rcs0
                     !dir$ attributes align : 16 :: rcs1
                     !dir$ attributes align : 16 :: rcs2
                     !dir$ attributes align : 16 :: rcs3
                   type(XMM4r4_t), automatic :: phi0,phi1,phi2,phi3
                   type(XMM4r4_t), automatic :: a0,a1,a2,a3
                   type(XMM4r4_t), automatic :: k0a0,k0a1,k0a2,k0a3
                   type(XMM4r4_t), automatic :: rcs0,rcs1,rcs2,rcs3
                   integer(kind=i4) :: i,m,m1
                   m = mod(n,8)
                   if(m/=0) then
                      do i=1,m
                         phi0.v    = pphi(i).v
                         a0.v      = pa(i).v
                         k0a0.v    = pk0a(i).v
                         rcs0.v    = rcs_f4122_xmm4r4(phi0,a0,k0a0)
                         prcs(i).v = rcs0.v
                      end do
                      if(n<8) return
                   end if
                   m1 = m+1
                    !dir$ assume_aligned pa:16
                    !dir$ assume_aligned pk0a:16
                    !dir$ assume_aligned prcs:16
                    !dir$ assume_aligned pphi:16
                    !dir$ vector aligned
                    !dir$ ivdep
                    !dir$ vector vectorlength(4)
                    !dir$ vector multiple_gather_scatter_by_shuffles 
                    !dir$ vector always
                  do i=m1,n,8
#if (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 1
                       call mm_prefetch(pphi(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 2   
                       call mm_prefetch(pphi(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)   
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                       call mm_prefetch(pphi(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.) 
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                       call mm_prefetch(pphi(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.) 
#endif                  
                         phi0.v      = pphi(i+0).v
                         a0.v        = pa(i+0).v
                         k0a0.v      = pk0a(i+0).v
                         rcs0.v      = rcs_f4122_xmm4r4(phi0,a0,k0a0)
                         prcs(i+0).v = rcs0.v
                         phi1.v      = pphi(i+1).v
                         a1.v        = pa(i+1).v
                         k0a1.v      = pk0a(i+1).v
                         rcs1.v      = rcs_f4122_xmm4r4(phi1,a1,k0a1)
                         prcs(i+1).v = rcs1.v
                         phi2.v      = pphi(i+2).v
                         a2.v        = pa(i+2).v
                         k0a2.v      = pk0a(i+2).v
                         rcs2.v      = rcs_f4122_xmm4r4(phi2,a2,k0a2)
                         prcs(i+2).v = rcs2.v   
                         phi3.v      = pphi(i+3).v
                         a3.v        = pa(i+3).v
                         k0a3.v      = pk0a(i+3).v
                         rcs3.v      = rcs_f4122_xmm4r4(phi3,a3,k0a3)
                         prcs(i+3).v = rcs3.v
                         phi0.v      = pphi(i+4).v
                         a0.v        = pa(i+4).v
                         k0a0.v      = pk0a(i+4).v
                         rcs0.v      = rcs_f4122_xmm4r4(phi0,a0,k0a0)
                         prcs(i+4).v = rcs0.v
                         phi1.v      = pphi(i+5).v
                         a1.v        = pa(i+5).v
                         k0a1.v      = pk0a(i+5).v
                         rcs1.v      = rcs_f4122_xmm4r4(phi1,a1,k0a1)
                         prcs(i+5).v = rcs1.v
                         phi2.v      = pphi(i+6).v
                         a2.v        = pa(i+6).v
                         k0a2.v      = pk0a(i+6).v
                         rcs2.v      = rcs_f4122_xmm4r4(phi2,a2,k0a2)
                         prcs(i+6).v = rcs2.v   
                         phi3.v      = pphi(i+7).v
                         a3.v        = pa(i+7).v
                         k0a3.v      = pk0a(i+7).v
                         rcs3.v      = rcs_f4122_xmm4r4(phi3,a3,k0a3)
                         prcs(i+7).v = rcs3.v
                     end do
              end subroutine rcs_f4122_xmm4r4_unroll8x
              

              subroutine rcs_f4122_xmm4r4_unroll4x(pphi,pa,pk0a,prcs,n,PF_DIST)
              
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4122_xmm4r4_unroll4x
                   !dir$ attributes forceinline :: rcs_f4122_xmm4r4_unroll4x
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4122_xmm4r4_unroll4x
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pphi
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pa
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pk0a
                   type(XMM4r4_t), dimension(1:n), intent(out):: prcs
                   integer(kind=i4),                intent(in) :: n
                   integer(kind=i4),                intent(in) :: PF_DIST
                   ! Locals
                     !dir$ attributes align : 16 :: a0
                     !dir$ attributes align : 16 :: a1
                     !dir$ attributes align : 16 :: a2
                     !dir$ attributes align : 16 :: a3
                     !dir$ attributes align : 16 :: k0a0
                     !dir$ attributes align : 16 :: k0a1
                     !dir$ attributes align : 16 :: k0a2
                     !dir$ attributes align : 16 :: k0a3
                     !dir$ attributes align : 16 :: rcs0
                     !dir$ attributes align : 16 :: rcs1
                     !dir$ attributes align : 16 :: rcs2
                     !dir$ attributes align : 16 :: rcs3
                   type(XMM4r4_t), automatic :: phi0,phi1,phi2,phi3
                   type(XMM4r4_t), automatic :: a0,a1,a2,a3
                   type(XMM4r4_t), automatic :: k0a0,k0a1,k0a2,k0a3
                   type(XMM4r4_t), automatic :: rcs0,rcs1,rcs2,rcs3
                   integer(kind=i4) :: i,m,m1
                   m = mod(n,4)
                   if(m/=0) then
                      do i=1,m
                         phi0.v    = pphi(i).v
                         a0.v      = pa(i).v
                         k0a0.v    = pk0a(i).v
                         rcs0.v    = rcs_f4122_xmm4r4(phi0,a0,k0a0)
                         prcs(i).v = rcs0.v
                      end do
                      if(n<4) return
                   end if
                   m1 = m+1
                    !dir$ assume_aligned pa:16
                    !dir$ assume_aligned pk0a:16
                    !dir$ assume_aligned prcs:16
                    !dir$ assume_aligned pphi:16
                    !dir$ vector aligned
                    !dir$ ivdep
                    !dir$ vector vectorlength(4)
                    !dir$ vector multiple_gather_scatter_by_shuffles 
                    !dir$ vector always
                  do i=m1,n,4
#if (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 1
                       call mm_prefetch(pphi(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 2   
                       call mm_prefetch(pphi(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)   
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                       call mm_prefetch(pphi(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.) 
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                       call mm_prefetch(pphi(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.) 
#endif                  
                         phi0.v      = pphi(i+0).v
                         a0.v        = pa(i+0).v
                         k0a0.v      = pk0a(i+0).v
                         rcs0.v      = rcs_f4122_xmm4r4(phi0,a0,k0a0)
                         prcs(i+0).v = rcs0.v
                         phi1.v      = pphi(i+1).v
                         a1.v        = pa(i+1).v
                         k0a1.v      = pk0a(i+1).v
                         rcs1.v      = rcs_f4122_xmm4r4(phi1,a1,k0a1)
                         prcs(i+1).v = rcs1.v
                         phi2.v      = pphi(i+2).v
                         a2.v        = pa(i+2).v
                         k0a2.v      = pk0a(i+2).v
                         rcs2.v      = rcs_f4122_xmm4r4(phi2,a2,k0a2)
                         prcs(i+2).v = rcs2.v   
                         phi3.v      = pphi(i+3).v
                         a3.v        = pa(i+3).v
                         k0a3.v      = pk0a(i+3).v
                         rcs3.v      = rcs_f4122_xmm4r4(phi3,a3,k0a3)
                         prcs(i+3).v = rcs3.v
                         phi0.v      = pphi(i+4).v
                         a0.v        = pa(i+4).v
                         k0a0.v      = pk0a(i+4).v
                         rcs0.v      = rcs_f4122_xmm4r4(phi0,a0,k0a0)
                         prcs(i+4).v = rcs0.v
                     end do
              end subroutine rcs_f4122_xmm4r4_unroll4x
              

              subroutine rcs_f4122_xmm4r4_rolled(pphi,pa,pk0a,prcs,n,PF_DIST)
              
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4122_xmm4r4_rolled
                   !dir$ attributes forceinline :: rcs_f4122_xmm4r4_rolled
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4122_xmm4r4_rolled
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pphi
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pa
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pk0a
                   type(XMM4r4_t), dimension(1:n), intent(out):: prcs
                   integer(kind=i4),                intent(in) :: n
                   integer(kind=i4),                intent(in) :: PF_DIST
                   ! Locals
                     !dir$ attributes align : 16 :: phi0
                     !dir$ attributes align : 16 :: a0
                     !dir$ attributes align : 16 :: k0a0
                     !dir$ attributes align : 16 :: rcs0
                   
                   type(XMM4r4_t), automatic :: phi0
                   type(XMM4r4_t), automatic :: a0
                   type(XMM4r4_t), automatic :: k0a0
                   type(XMM4r4_t), automatic :: rcs0
                   integer(kind=i4) :: i
                  
                    !dir$ assume_aligned pa:16
                    !dir$ assume_aligned pk0a:16
                    !dir$ assume_aligned prcs:16
                    !dir$ assume_aligned pphi:16
                    !dir$ vector aligned
                    !dir$ ivdep
                    !dir$ vector vectorlength(4)
                    !dir$ vector multiple_gather_scatter_by_shuffles 
                    !dir$ vector always
                  do i=1,n
#if (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 1
                       call mm_prefetch(pphi(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 2   
                       call mm_prefetch(pphi(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)   
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                       call mm_prefetch(pphi(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.) 
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                       call mm_prefetch(pphi(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.) 
#endif                  
                         phi0.v      = pphi(i+0).v
                         a0.v        = pa(i+0).v
                         k0a0.v      = pk0a(i+0).v
                         rcs0.v      = rcs_f4122_xmm4r4(phi0,a0,k0a0)
                         prcs(i+0).v = rcs0.v
                     end do
              end subroutine rcs_f4122_xmm4r4_rolled
              
              
               ! /*
               !        Forward scattering widths, E-field.
               !        Formula 4.1-23
               !    */
               
               pure function rcs_f4123_xmm4r4(a,k0a) result(rcs)
               
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4123_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4123_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4123_xmm4r4
                   type(XMM4r4_t), intent(in) :: a
                   type(XMM4r4_t), intent(in) :: k0a
                   type(XMM4r4_t) :: rcs
                   rcs = rcs_f4120_xmm4r4(a,k0a)
               end function rcs_f4123_xmm4r4
               
               
               !/*
               !        Forward scattering widths, H-field.
               !        Formula 4.1-24
               !    */
               
               pure function rcs_f4124_xmm4r4(a,k0a) result(rcs)
               
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4124_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4124_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4124_xmm4r4
                   type(XMM4r4_t), intent(in) :: a
                   type(XMM4r4_t), intent(in) :: k0a
                   type(XMM4r4_t) :: rcs
                   ! Locals
                     !dir$ attributes align : 16 :: C9869604401089358618834490999876
                     !dir$ attributes align : 16 :: C025
                     !dir$ attributes align : 16 :: pi2a
                     !dir$ attributes align : 16 :: k0a3
                   type(XMM4r4_t), parameter :: C9869604401089358618834490999876 = &
                                                   XMM4r4_t(9.869604401089358618834490999876_sp)
                   type(XMM4r4_t), parameter :: C025 = XMM4r4_t(0.25_sp)
                   type(XMM4r4_t), automatic :: pi2a,k0a3
#if (GMS_EXPLICIT_VECTORIZE) == 1
                    integer(kind=i4) :: j
                    !dir$ loop_count(4)
                    !dir$ vector aligned
                    !dir$ vector vectorlength(4)
                    !dir$ vector always
                    do j=0, 3
                        k0a3.v(j) = k0a.v(j)*k0a.v(j)*k0a.v(j)
                        pi2a.v(j) = C9869604401089358618834490999876.v(j)*a.v(j)
                        rcs.v(j)  = pi2a.v(j)*k0a3.v(j)*C025.v(j)
                    end do
#else
                   k0a3.v = k0a.v*k0a.v*k0a.v
                   pi2a.v = C9869604401089358618834490999876.v*a.v
                   rcs.v  = pi2a.v*k0a3.v*C025.v
#endif
               end function rcs_f4124_xmm4r4
               
               
               subroutine rcs_f4124_xmm4r4_unroll16x(pa,pk0a,prcs,n,PF_DIST)
             
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4124_xmm4r4_unroll16x
                   !dir$ attributes forceinline :: rcs_f4124_xmm4r4_unroll16x
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4124_xmm4r4_unroll16x
                   type(XMM4r4_t), dimension(1:n), intent(in)    :: pa
                   type(XMM4r4_t), dimension(1:n), intent(in)    :: pk0a
                   type(XMM4r4_t), dimension(1:n), intent(out)   :: prcs 
                   integer(kind=i4)               , intent(in)    :: n
                   integer(kind=i4)               , intent(in)    :: PF_DIST
                   ! Locals
                     !dir$ attributes align : 16 :: a0
                     !dir$ attributes align : 16 :: a1
                     !dir$ attributes align : 16 :: a2
                     !dir$ attributes align : 16 :: a3
                     !dir$ attributes align : 16 :: k0a0
                     !dir$ attributes align : 16 :: k0a1
                     !dir$ attributes align : 16 :: k0a2
                     !dir$ attributes align : 16 :: k0a3
                     !dir$ attributes align : 16 :: rcs0
                     !dir$ attributes align : 16 :: rcs1
                     !dir$ attributes align : 16 :: rcs2
                     !dir$ attributes align : 16 :: rcs3
                   type(XMM4r4_t), automatic :: a0,a1,a2,a3
                   type(XMM4r4_t), automatic :: k0a0,k0a1,k0a2,k0a3
                   type(XMM4r4_t), automatic :: rcs0,rcs1,rcs2,rcs3
                   integer(kind=i4) :: i,m,m1
                   m = mod(n,16)
                   if(m/=0) then
                      do i=1,m
                         a0.v       = pa(i).v
                         k0a0.v     = pk0a(i).v
                         rcs0       = rcs_f4124_xmm4r4(a0,k0a0)
                         prcs(i).v  = rcs0.v
                      end do
                      if(n<16) return
                   end if
                   m1=m+1
                    !dir$ assume_aligned pa:16
                    !dir$ assume_aligned pk0a:16
                    !dir$ assume_aligned prcs:16
                    !dir$ vector aligned
                    !dir$ ivdep
                    !dir$ vector vectorlength(4)
                    !dir$ vector multiple_gather_scatter_by_shuffles 
                    !dir$ vector always
                   do i=m1,n,16
#if (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 1
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 2   
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)   
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.) 
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.) 
#endif       
                        a0.v         = pa(i+0).v
                        k0a0.v       = pk0a(i+0).v
                        rcs0         = rcs_f4124_xmm4r4(a0,k0a0)
                        prcs(i+0).v  = rcs0.v  
                        a1.v         = pa(i+1).v
                        k0a1.v       = pk0a(i+1).v
                        rcs1         = rcs_f4124_xmm4r4(a1,k0a1)
                        prcs(i+1).v  = rcs1.v 
                        a2.v         = pa(i+2).v
                        k0a2.v       = pk0a(i+2).v
                        rcs2         = rcs_f4124_xmm4r4(a2,k0a2)
                        prcs(i+2).v  = rcs2.v  
                        a3.v         = pa(i+3).v
                        k0a3.v       = pk0a(i+3).v
                        rcs3         = rcs_f4124_xmm4r4(a3,k0a3)
                        prcs(i+3).v  = rcs3.v  
                        a0.v         = pa(i+4).v
                        k0a0.v       = pk0a(i+4).v
                        rcs0         = rcs_f4124_xmm4r4(a0,k0a0)
                        prcs(i+4).v  = rcs0.v  
                        a1.v         = pa(i+5).v
                        k0a1.v       = pk0a(i+5).v
                        rcs1         = rcs_f4124_xmm4r4(a1,k0a1)
                        prcs(i+5).v  = rcs1.v 
                        a2.v         = pa(i+6).v
                        k0a2.v       = pk0a(i+6).v
                        rcs2         = rcs_f4124_xmm4r4(a2,k0a2)
                        prcs(i+6).v  = rcs2.v
                        a3.v         = pa(i+7).v
                        k0a3.v       = pk0a(i+7).v
                        rcs3         = rcs_f4124_xmm4r4(a3,k0a3)
                        prcs(i+7).v  = rcs3.v  
                        a0.v         = pa(i+8).v
                        k0a0.v       = pk0a(i+8).v
                        rcs0         = rcs_f4124_xmm4r4(a0,k0a0)
                        prcs(i+8).v  = rcs0.v  
                        a1.v         = pa(i+9).v
                        k0a1.v       = pk0a(i+9).v
                        rcs1         = rcs_f4124_xmm4r4(a1,k0a1)
                        prcs(i+9).v  = rcs1.v 
                        a2.v         = pa(i+10).v
                        k0a2.v       = pk0a(i+10).v
                        rcs2         = rcs_f4124_xmm4r4(a2,k0a2)
                        prcs(i+10).v = rcs2.v
                        a3.v         = pa(i+11).v
                        k0a3.v       = pk0a(i+11).v
                        rcs3         = rcs_f4124_xmm4r4(a3,k0a3)
                        prcs(i+11).v = rcs3.v  
                        a0.v         = pa(i+12).v
                        k0a0.v       = pk0a(i+12).v
                        rcs0         = rcs_f4124_xmm4r4(a0,k0a0)
                        prcs(i+12).v = rcs0.v  
                        a1.v         = pa(i+13).v
                        k0a1.v       = pk0a(i+13).v
                        rcs1         = rcs_f4124_xmm4r4(a1,k0a1)
                        prcs(i+13).v = rcs1.v 
                        a2.v         = pa(i+14).v
                        k0a2.v       = pk0a(i+14).v
                        rcs2         = rcs_f4124_xmm4r4(a2,k0a2)
                        prcs(i+14).v = rcs2.v
                        a3.v         = pa(i+15).v
                        k0a3.v       = pk0a(i+15).v
                        rcs3         = rcs_f4124_xmm4r4(a3,k0a3)
                        prcs(i+15).v = rcs3.v                      
                   end do
             end subroutine rcs_f4124_xmm4r4_unroll16x
             
             
             subroutine rcs_f4124_xmm4r4_unroll12x(pa,pk0a,prcs,n,PF_DIST)
             
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4124_xmm4r4_unroll12x
                   !dir$ attributes forceinline :: rcs_f4124_xmm4r4_unroll12x
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4124_xmm4r4_unroll12x
                   type(XMM4r4_t), dimension(1:n), intent(in)    :: pa
                   type(XMM4r4_t), dimension(1:n), intent(in)    :: pk0a
                   type(XMM4r4_t), dimension(1:n), intent(out)   :: prcs 
                   integer(kind=i4)               , intent(in)    :: n
                   integer(kind=i4)               , intent(in)    :: PF_DIST
                   ! Locals
                     !dir$ attributes align : 16 :: a0
                     !dir$ attributes align : 16 :: a1
                     !dir$ attributes align : 16 :: a2
                     !dir$ attributes align : 16 :: a3
                     !dir$ attributes align : 16 :: k0a0
                     !dir$ attributes align : 16 :: k0a1
                     !dir$ attributes align : 16 :: k0a2
                     !dir$ attributes align : 16 :: k0a3
                     !dir$ attributes align : 16 :: rcs0
                     !dir$ attributes align : 16 :: rcs1
                     !dir$ attributes align : 16 :: rcs2
                     !dir$ attributes align : 16 :: rcs3
                   type(XMM4r4_t), automatic :: a0,a1,a2,a3
                   type(XMM4r4_t), automatic :: k0a0,k0a1,k0a2,k0a3
                   type(XMM4r4_t), automatic :: rcs0,rcs1,rcs2,rcs3
                   integer(kind=i4) :: i,m,m1
                   m = mod(n,12)
                   if(m/=0) then
                      do i=1,m
                         a0.v       = pa(i).v
                         k0a0.v     = pk0a(i).v
                         rcs0       = rcs_f4124_xmm4r4(a0,k0a0)
                         prcs(i).v  = rcs0.v
                      end do
                      if(n<12) return
                   end if
                   m1=m+1
                    !dir$ assume_aligned pa:16
                    !dir$ assume_aligned pk0a:16
                    !dir$ assume_aligned prcs:16
                    !dir$ vector aligned
                    !dir$ ivdep
                    !dir$ vector vectorlength(4)
                    !dir$ vector multiple_gather_scatter_by_shuffles 
                    !dir$ vector always
                   do i=m1,n,12
#if (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 1
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 2   
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)   
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.) 
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.) 
#endif       
                        a0.v         = pa(i+0).v
                        k0a0.v       = pk0a(i+0).v
                        rcs0         = rcs_f4124_xmm4r4(a0,k0a0)
                        prcs(i+0).v  = rcs0.v  
                        a1.v         = pa(i+1).v
                        k0a1.v       = pk0a(i+1).v
                        rcs1         = rcs_f4124_xmm4r4(a1,k0a1)
                        prcs(i+1).v  = rcs1.v 
                        a2.v         = pa(i+2).v
                        k0a2.v       = pk0a(i+2).v
                        rcs2         = rcs_f4124_xmm4r4(a2,k0a2)
                        prcs(i+2).v  = rcs2.v  
                        a3.v         = pa(i+3).v
                        k0a3.v       = pk0a(i+3).v
                        rcs3         = rcs_f4124_xmm4r4(a3,k0a3)
                        prcs(i+3).v  = rcs3.v  
                        a0.v         = pa(i+4).v
                        k0a0.v       = pk0a(i+4).v
                        rcs0         = rcs_f4124_xmm4r4(a0,k0a0)
                        prcs(i+4).v  = rcs0.v  
                        a1.v         = pa(i+5).v
                        k0a1.v       = pk0a(i+5).v
                        rcs1         = rcs_f4124_xmm4r4(a1,k0a1)
                        prcs(i+5).v  = rcs1.v 
                        a2.v         = pa(i+6).v
                        k0a2.v       = pk0a(i+6).v
                        rcs2         = rcs_f4124_xmm4r4(a2,k0a2)
                        prcs(i+6).v  = rcs2.v
                        a3.v         = pa(i+7).v
                        k0a3.v       = pk0a(i+7).v
                        rcs3         = rcs_f4124_xmm4r4(a3,k0a3)
                        prcs(i+7).v  = rcs3.v  
                        a0.v         = pa(i+8).v
                        k0a0.v       = pk0a(i+8).v
                        rcs0         = rcs_f4124_xmm4r4(a0,k0a0)
                        prcs(i+8).v  = rcs0.v  
                        a1.v         = pa(i+9).v
                        k0a1.v       = pk0a(i+9).v
                        rcs1         = rcs_f4124_xmm4r4(a1,k0a1)
                        prcs(i+9).v  = rcs1.v 
                        a2.v         = pa(i+10).v
                        k0a2.v       = pk0a(i+10).v
                        rcs2         = rcs_f4124_xmm4r4(a2,k0a2)
                        prcs(i+10).v = rcs2.v
                        a3.v         = pa(i+11).v
                        k0a3.v       = pk0a(i+11).v
                        rcs3         = rcs_f4124_xmm4r4(a3,k0a3)
                        prcs(i+11).v = rcs3.v  
                     end do
             end subroutine rcs_f4124_xmm4r4_unroll12x
             

             subroutine rcs_f4124_xmm4r4_unroll8x(pa,pk0a,prcs,n,PF_DIST)
             
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4124_xmm4r4_unroll8x
                   !dir$ attributes forceinline :: rcs_f4124_xmm4r4_unroll8x
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4124_xmm4r4_unroll8x
                   type(XMM4r4_t), dimension(1:n), intent(in)    :: pa
                   type(XMM4r4_t), dimension(1:n), intent(in)    :: pk0a
                   type(XMM4r4_t), dimension(1:n), intent(out)   :: prcs 
                   integer(kind=i4)               , intent(in)    :: n
                   integer(kind=i4)               , intent(in)    :: PF_DIST
                   ! Locals
                     !dir$ attributes align : 16 :: a0
                     !dir$ attributes align : 16 :: a1
                     !dir$ attributes align : 16 :: a2
                     !dir$ attributes align : 16 :: a3
                     !dir$ attributes align : 16 :: k0a0
                     !dir$ attributes align : 16 :: k0a1
                     !dir$ attributes align : 16 :: k0a2
                     !dir$ attributes align : 16 :: k0a3
                     !dir$ attributes align : 16 :: rcs0
                     !dir$ attributes align : 16 :: rcs1
                     !dir$ attributes align : 16 :: rcs2
                     !dir$ attributes align : 16 :: rcs3
                   type(XMM4r4_t), automatic :: a0,a1,a2,a3
                   type(XMM4r4_t), automatic :: k0a0,k0a1,k0a2,k0a3
                   type(XMM4r4_t), automatic :: rcs0,rcs1,rcs2,rcs3
                   integer(kind=i4) :: i,m,m1
                   m = mod(n,8)
                   if(m/=0) then
                      do i=1,m
                         a0.v       = pa(i).v
                         k0a0.v     = pk0a(i).v
                         rcs0       = rcs_f4124_xmm4r4(a0,k0a0)
                         prcs(i).v  = rcs0.v
                      end do
                      if(n<8) return
                   end if
                   m1=m+1
                    !dir$ assume_aligned pa:16
                    !dir$ assume_aligned pk0a:16
                    !dir$ assume_aligned prcs:16
                    !dir$ vector aligned
                    !dir$ ivdep
                    !dir$ vector vectorlength(4)
                    !dir$ vector multiple_gather_scatter_by_shuffles 
                    !dir$ vector always
                   do i=m1,n,8
#if (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 1
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 2   
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)   
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.) 
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.) 
#endif       
                        a0.v         = pa(i+0).v
                        k0a0.v       = pk0a(i+0).v
                        rcs0         = rcs_f4124_xmm4r4(a0,k0a0)
                        prcs(i+0).v  = rcs0.v  
                        a1.v         = pa(i+1).v
                        k0a1.v       = pk0a(i+1).v
                        rcs1         = rcs_f4124_xmm4r4(a1,k0a1)
                        prcs(i+1).v  = rcs1.v 
                        a2.v         = pa(i+2).v
                        k0a2.v       = pk0a(i+2).v
                        rcs2         = rcs_f4124_xmm4r4(a2,k0a2)
                        prcs(i+2).v  = rcs2.v  
                        a3.v         = pa(i+3).v
                        k0a3.v       = pk0a(i+3).v
                        rcs3         = rcs_f4124_xmm4r4(a3,k0a3)
                        prcs(i+3).v  = rcs3.v  
                        a0.v         = pa(i+4).v
                        k0a0.v       = pk0a(i+4).v
                        rcs0         = rcs_f4124_xmm4r4(a0,k0a0)
                        prcs(i+4).v  = rcs0.v  
                        a1.v         = pa(i+5).v
                        k0a1.v       = pk0a(i+5).v
                        rcs1         = rcs_f4124_xmm4r4(a1,k0a1)
                        prcs(i+5).v  = rcs1.v 
                        a2.v         = pa(i+6).v
                        k0a2.v       = pk0a(i+6).v
                        rcs2         = rcs_f4124_xmm4r4(a2,k0a2)
                        prcs(i+6).v  = rcs2.v
                        a3.v         = pa(i+7).v
                        k0a3.v       = pk0a(i+7).v
                        rcs3         = rcs_f4124_xmm4r4(a3,k0a3)
                        prcs(i+7).v  = rcs3.v  
                    end do
             end subroutine rcs_f4124_xmm4r4_unroll8x
               
               
             subroutine rcs_f4124_xmm4r4_unroll4x(pa,pk0a,prcs,n,PF_DIST)
             
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4124_xmm4r4_unroll4x
                   !dir$ attributes forceinline :: rcs_f4124_xmm4r4_unroll4x
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4124_xmm4r4_unroll4x
                   type(XMM4r4_t), dimension(1:n), intent(in)    :: pa
                   type(XMM4r4_t), dimension(1:n), intent(in)    :: pk0a
                   type(XMM4r4_t), dimension(1:n), intent(out)   :: prcs 
                   integer(kind=i4)               , intent(in)    :: n
                   integer(kind=i4)               , intent(in)    :: PF_DIST
                   ! Locals
                     !dir$ attributes align : 16 :: a0
                     !dir$ attributes align : 16 :: a1
                     !dir$ attributes align : 16 :: a2
                     !dir$ attributes align : 16 :: a3
                     !dir$ attributes align : 16 :: k0a0
                     !dir$ attributes align : 16 :: k0a1
                     !dir$ attributes align : 16 :: k0a2
                     !dir$ attributes align : 16 :: k0a3
                     !dir$ attributes align : 16 :: rcs0
                     !dir$ attributes align : 16 :: rcs1
                     !dir$ attributes align : 16 :: rcs2
                     !dir$ attributes align : 16 :: rcs3
                   type(XMM4r4_t), automatic :: a0,a1,a2,a3
                   type(XMM4r4_t), automatic :: k0a0,k0a1,k0a2,k0a3
                   type(XMM4r4_t), automatic :: rcs0,rcs1,rcs2,rcs3
                   integer(kind=i4) :: i,m,m1
                   m = mod(n,4)
                   if(m/=0) then
                      do i=1,m
                         a0.v       = pa(i).v
                         k0a0.v     = pk0a(i).v
                         rcs0       = rcs_f4124_xmm4r4(a0,k0a0)
                         prcs(i).v  = rcs0.v
                      end do
                      if(n<4) return
                   end if
                   m1=m+1
                    !dir$ assume_aligned pa:16
                    !dir$ assume_aligned pk0a:16
                    !dir$ assume_aligned prcs:16
                    !dir$ vector aligned
                    !dir$ ivdep
                    !dir$ vector vectorlength(4)
                    !dir$ vector multiple_gather_scatter_by_shuffles 
                    !dir$ vector always
                   do i=m1,n,4
#if (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 1
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 2   
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)   
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.) 
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.) 
#endif       
                        a0.v         = pa(i+0).v
                        k0a0.v       = pk0a(i+0).v
                        rcs0         = rcs_f4124_xmm4r4(a0,k0a0)
                        prcs(i+0).v  = rcs0.v  
                        a1.v         = pa(i+1).v
                        k0a1.v       = pk0a(i+1).v
                        rcs1         = rcs_f4124_xmm4r4(a1,k0a1)
                        prcs(i+1).v  = rcs1.v 
                        a2.v         = pa(i+2).v
                        k0a2.v       = pk0a(i+2).v
                        rcs2         = rcs_f4124_xmm4r4(a2,k0a2)
                        prcs(i+2).v  = rcs2.v  
                        a3.v         = pa(i+3).v
                        k0a3.v       = pk0a(i+3).v
                        rcs3         = rcs_f4124_xmm4r4(a3,k0a3)
                        prcs(i+3).v  = rcs3.v  
                   end do
             end subroutine rcs_f4124_xmm4r4_unroll4x
               
                 
             subroutine rcs_f4124_xmm4r4_rolled(pa,pk0a,prcs,n,PF_DIST)
             
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4124_xmm4r4_rolled
                   !dir$ attributes forceinline :: rcs_f4124_xmm4r4_rolled
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4124_xmm4r4_rolled
                   type(XMM4r4_t), dimension(1:n), intent(in)    :: pa
                   type(XMM4r4_t), dimension(1:n), intent(in)    :: pk0a
                   type(XMM4r4_t), dimension(1:n), intent(out)   :: prcs 
                   integer(kind=i4)               , intent(in)    :: n
                   integer(kind=i4)               , intent(in)    :: PF_DIST
                   ! Locals
                     !dir$ attributes align : 16 :: a0
                     !dir$ attributes align : 16 :: k0a0
                     !dir$ attributes align : 16 :: rcs0
                  
                   type(XMM4r4_t), automatic :: a0
                   type(XMM4r4_t), automatic :: k0a0
                   type(XMM4r4_t), automatic :: rcs0
                   integer(kind=i4) :: i
                 
                    !dir$ assume_aligned pa:16
                    !dir$ assume_aligned pk0a:16
                    !dir$ assume_aligned prcs:16
                    !dir$ vector aligned
                    !dir$ ivdep
                    !dir$ vector vectorlength(4)
                    !dir$ vector multiple_gather_scatter_by_shuffles 
                    !dir$ vector always
                   do i=1,n
#if (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 1
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 2   
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)   
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.) 
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                       call mm_prefetch(pa(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.) 
#endif       
                        a0.v         = pa(i+0).v
                        k0a0.v       = pk0a(i+0).v
                        rcs0         = rcs_f4124_xmm4r4(a0,k0a0)
                        prcs(i+0).v  = rcs0.v  
                    end do
             end subroutine rcs_f4124_xmm4r4_rolled
             
             
             !/*
             !             Surface currents (k0a << 1), for long cylinder (wire).
             !             E-field cylinder axis parallel.
             !             Formula 4.1-25
             !          */ 
             
             
             pure function Kz_f4125_xmm4r4(eps0,mu0,E,k0a) result(Kz)
             
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: Kz_f4125_xmm4r4
                   !dir$ attributes forceinline :: Kz_f4125_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: Kz_f4125_xmm4r4
                   use mod_vecconsts, only :  v4r4_0
                   real(kind=sp),   intent(in) :: eps0
                   real(kind=sp),   intent(in) :: mu0
                   type(  XMM4c4_t),   intent(in) :: E
                   type(XMM4r4_t), intent(in) :: k0a
                   type(  XMM4c4_t) :: Kz
                   ! Locals
                     !dir$ attributes align : 16 :: C157079632679489661923132169164
                     !dir$ attributes align : 16 :: C08905 
                     !dir$ attributes align : 16 :: t0
                     !dir$ attributes align : 16 :: t1
                     !dir$ attributes align : 16 :: div
                     !dir$ attributes align : 16 :: veps0
                     !dir$ attributes align : 16 :: vmu0
                     !dir$ attributes align : 16 :: lna
                     !dir$ attributes align : 16 :: ln
                     !dir$ attributes align : 16 :: x0
                   type(XMM4r4_t), parameter :: C157079632679489661923132169164 = 
                                         XMM4r4_t(1.57079632679489661923132169164_sp)
                   type(XMM4r4_t), parameter :: C08905 = XMM4r4_t(0.8905_sp)
                   type(  XMM4c4_t),   automatic :: t0,t1,div
                   type(XMM4r4_t), automatic :: veps0,vmu0
                   type(XMM4r4_t), automatic :: lna,ln
                   type(XMM4r4_t), automatic :: x0
                   veps0 = XMM4r4_t(eps0)
                   lna.v = k0a.v*C08905.v
                   vmu0  = XMM4r4_t(mu0)
                   ln.v  = log(lna.v)
                   x0.v  = veps0.v/vmu0.v
                   t0.re =  v4r4_0.v
                   t0.im = sqrt(x0.v)
                   t1.re = k0a.v*ln.v
                   t1.im = C157079632679489661923132169164.v
                   div   = E/t1
                   Kz    = div*t0
             end function Kz_f4125_xmm4r4
             
             
             subroutine Kz_f4125_xmm4r4_unroll16x(peps0,pmu0,pE,pk0a,pKz,PF_DIST1, &
                                                   n,PF_DIST1,PF_DIST2)
             
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: Kz_f4125_xmm4r4_unroll16x
                   !dir$ attributes forceinline :: Kz_f4125_xmm4r4_unroll16x
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: Kz_f4125_xmm4r4_unroll16x
                   real(kind=sp),   dimension(1:n), intent(in) :: peps0
                   real(kind=sp),   dimension(1:n), intent(in) :: pmu0
                   type(  XMM4c4_t),   dimension(1:n), intent(in) :: pE
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pk0a
                   type(  XMM4c4_t),   dimension(1:n), intent(in) :: pKz
                   integer(kind=i4),                intent(in) :: n
                   integer(kind=i4),                intent(in) :: PF_DIST1
                   integer(kind=i4),                intent(in) :: PF_DIST2
                   ! Locals
                      !dir$ attributes align : 16 :: Kz0
                      !dir$ attributes align : 16 :: Kz1
                      !dir$ attributes align : 16 :: Kz2
                      !dir$ attributes align : 16 :: Kz3
                      !dir$ attributes align : 16 :: E0
                      !dir$ attributes align : 16 :: E1
                      !dir$ attributes align : 16 :: E2
                      !dir$ attributes align : 16 :: E3
                      !dir$ attributes align : 16 :: k0a0
                      !dir$ attributes align : 16 :: k0a1
                      !dir$ attributes align : 16 :: k0a2
                      !dir$ attributes align : 16 :: k0a3
                      !dir$ attributes align : 16 :: eps00
                      !dir$ attributes align : 16 :: eps01
                      !dir$ attributes align : 16 :: eps02
                      !dir$ attributes align : 16 :: eps03
                      !dir$ attributes align : 16 :: mu00
                      !dir$ attributes align : 16 :: mu01
                      !dir$ attributes align : 16 :: mu02
                      !dir$ attributes align : 16 :: mu03
                   type(  XMM4c4_t),   automatic :: Kz0,Kz1,Kz2,Kz3
                   type(  XMM4c4_t),   automatic :: E0,E1,E2,E3
                   type(XMM4r4_t), automatic :: k0a0,k0a1,k0a2,k0a3
                   real(kind=sp),   automatic :: eps00,eps01,eps02,eps03
                   real(kind=sp),   automatic :: mu00,mu01,mu02,mu03
                   integer(kind=i4) :: i,m,m1
                   m = mod(n,16)
                   if(m/=0) then
                      do i=1,m
                         eps00  = peps0(i)
                         mu00   = pmu0(i)
                         E0.v   = pE(i).v
                         k0a0.v = pk0a(i).v
                         Kz0    = Kz_f4125_xmm4r4(eps00,mu00,E0,k0a0)
                         pKz(i) = Kz0
                      end do
                      if(n<16) return
                   end if
                   m1 = m+1
                    !dir$ assume_aligned peps0:16
                    !dir$ assume_aligned pmu0:16
                    !dir$ assume_aligned pE:16
                    !dir$ assume_aligned pk0a:16
                    !dir$ assume_aligned pKz:16
                    !dir$ vector aligned
                    !dir$ ivdep
                    !dir$ vector vectorlength(4)
                    !dir$ vector multiple_gather_scatter_by_shuffles 
                    !dir$ vector always
                 do i=m1,n,16
#if (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 1
                       call mm_prefetch(peps0(i+PF_DIST1),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST1),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST2),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST2),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 2   
                       call mm_prefetch(peps0(i+PF_DIST1),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST1),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST2),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST2),FOR_K_PREFETCH_T1,.true.,.false.) 
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                       call mm_prefetch(peps0(i+PF_DIST1),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST1),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST2),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST2),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                       call mm_prefetch(peps0(i+PF_DIST1),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST1),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST2),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST2),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif                 
                         eps00    = peps0(i+0)
                         mu00     = pmu0(i+0)
                         E0.v     = pE(i+0).v
                         k0a0.v   = pk0a(i+0).v
                         Kz0      = Kz_f4125_xmm4r4(eps00,mu00,E0,k0a0)
                         pKz(i+0) = Kz0 
                         eps01    = peps0(i+1)
                         mu01     = pmu0(i+1)
                         E1.v     = pE(i+1).v
                         k0a1.v   = pk0a(i+1).v
                         Kz1      = Kz_f4125_xmm4r4(eps01,mu01,E1,k0a1)
                         pKz(i+1) = Kz1 
                         eps02    = peps0(i+2)
                         mu02     = pmu0(i+2)
                         E2.v     = pE(i+2).v
                         k0a2.v   = pk0a(i+2).v
                         Kz2      = Kz_f4125_xmm4r4(eps02,mu02,E2,k0a2)
                         pKz(i+2) = Kz2
                         eps03    = peps0(i+3)
                         mu03     = pmu0(i+3)
                         E3.v     = pE(i+3).v
                         k0a3.v   = pk0a(i+3).v
                         Kz3      = Kz_f4125_xmm4r4(eps03,mu03,E3,k0a3)
                         pKz(i+3) = Kz3  
                         eps00    = peps0(i+4)
                         mu00     = pmu0(i+4)
                         E0.v     = pE(i+4).v
                         k0a0.v   = pk0a(i+4).v
                         Kz0      = Kz_f4125_xmm4r4(eps00,mu00,E0,k0a0)
                         pKz(i+4) = Kz0 
                         eps01    = peps0(i+5)
                         mu01     = pmu0(i+5)
                         E1.v     = pE(i+5).v
                         k0a1.v   = pk0a(i+5).v
                         Kz1      = Kz_f4125_xmm4r4(eps01,mu01,E1,k0a1)
                         pKz(i+5) = Kz1 
                         eps02    = peps0(i+6)
                         mu02     = pmu0(i+6)
                         E2.v     = pE(i+6).v
                         k0a2.v   = pk0a(i+6).v
                         Kz2      = Kz_f4125_xmm4r4(eps02,mu02,E2,k0a2)
                         pKz(i+6) = Kz2
                         eps03    = peps0(i+7)
                         mu03     = pmu0(i+7)
                         E3.v     = pE(i+7).v
                         k0a3.v   = pk0a(i+7).v
                         Kz3      = Kz_f4125_xmm4r4(eps03,mu03,E3,k0a3)
                         pKz(i+7) = Kz3  
                         eps00    = peps0(i+8)
                         mu00     = pmu0(i+8)
                         E0.v     = pE(i+8).v
                         k0a0.v   = pk0a(i+8).v
                         Kz0      = Kz_f4125_xmm4r4(eps00,mu00,E0,k0a0)
                         pKz(i+8) = Kz0 
                         eps01    = peps0(i+9)
                         mu01     = pmu0(i+9)
                         E1.v     = pE(i+9).v
                         k0a1.v   = pk0a(i+9).v
                         Kz1      = Kz_f4125_xmm4r4(eps01,mu01,E1,k0a1)
                         pKz(i+9) = Kz1 
                         eps02    = peps0(i+10)
                         mu02     = pmu0(i+10)
                         E2.v     = pE(i+10).v
                         k0a2.v   = pk0a(i+10).v
                         Kz2      = Kz_f4125_xmm4r4(eps02,mu02,E2,k0a2)
                         pKz(i+10) = Kz2
                         eps03    = peps0(i+11)
                         mu03     = pmu0(i+11)
                         E3.v     = pE(i+11).v
                         k0a3.v   = pk0a(i+11).v
                         Kz3      = Kz_f4125_xmm4r4(eps03,mu03,E3,k0a3)
                         pKz(i+11) = Kz3  
                         eps00    = peps0(i+12)
                         mu00     = pmu0(i+12)
                         E0.v     = pE(i+12).v
                         k0a0.v   = pk0a(i+12).v
                         Kz0      = Kz_f4125_xmm4r4(eps00,mu00,E0,k0a0)
                         pKz(i+12) = Kz0 
                         eps01    = peps0(i+13)
                         mu01     = pmu0(i+13)
                         E1.v     = pE(i+13).v
                         k0a1.v   = pk0a(i+13).v
                         Kz1      = Kz_f4125_xmm4r4(eps01,mu01,E1,k0a1)
                         pKz(i+13) = Kz1 
                         eps02    = peps0(i+14)
                         mu02     = pmu0(i+14)
                         E2.v     = pE(i+14).v
                         k0a2.v   = pk0a(i+14).v
                         Kz2      = Kz_f4125_xmm4r4(eps02,mu02,E2,k0a2)
                         pKz(i+14) = Kz2
                         eps03    = peps0(i+15)
                         mu03     = pmu0(i+15)
                         E3.v     = pE(i+15).v
                         k0a3.v   = pk0a(i+15).v
                         Kz3      = Kz_f4125_xmm4r4(eps03,mu03,E3,k0a3)
                         pKz(i+15) = Kz3  
                 end do
             end subroutine Kz_f4125_xmm4r4_unroll16x
             
             
             subroutine Kz_f4125_xmm4r4_unroll12x(peps0,pmu0,pE,pk0a,pKz,PF_DIST1, &
                                                   n,PF_DIST1,PF_DIST2)
             
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: Kz_f4125_xmm4r4_unroll12x
                   !dir$ attributes forceinline :: Kz_f4125_xmm4r4_unroll12x
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: Kz_f4125_xmm4r4_unroll12x
                   real(kind=sp),   dimension(1:n), intent(in) :: peps0
                   real(kind=sp),   dimension(1:n), intent(in) :: pmu0
                   type(  XMM4c4_t),   dimension(1:n), intent(in) :: pE
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pk0a
                   type(  XMM4c4_t),   dimension(1:n), intent(in) :: pKz
                   integer(kind=i4),                intent(in) :: n
                   integer(kind=i4),                intent(in) :: PF_DIST1
                   integer(kind=i4),                intent(in) :: PF_DIST2
                   ! Locals
                      !dir$ attributes align : 16 :: Kz0
                      !dir$ attributes align : 16 :: Kz1
                      !dir$ attributes align : 16 :: Kz2
                      !dir$ attributes align : 16 :: Kz3
                      !dir$ attributes align : 16 :: E0
                      !dir$ attributes align : 16 :: E1
                      !dir$ attributes align : 16 :: E2
                      !dir$ attributes align : 16 :: E3
                      !dir$ attributes align : 16 :: k0a0
                      !dir$ attributes align : 16 :: k0a1
                      !dir$ attributes align : 16 :: k0a2
                      !dir$ attributes align : 16 :: k0a3
                      !dir$ attributes align : 16 :: eps00
                      !dir$ attributes align : 16 :: eps01
                      !dir$ attributes align : 16 :: eps02
                      !dir$ attributes align : 16 :: eps03
                      !dir$ attributes align : 16 :: mu00
                      !dir$ attributes align : 16 :: mu01
                      !dir$ attributes align : 16 :: mu02
                      !dir$ attributes align : 16 :: mu03
                   type(  XMM4c4_t),   automatic :: Kz0,Kz1,Kz2,Kz3
                   type(  XMM4c4_t),   automatic :: E0,E1,E2,E3
                   type(XMM4r4_t), automatic :: k0a0,k0a1,k0a2,k0a3
                   real(kind=sp),   automatic :: eps00,eps01,eps02,eps03
                   real(kind=sp),   automatic :: mu00,mu01,mu02,mu03
                   integer(kind=i4) :: i,m,m1
                   m = mod(n,12)
                   if(m/=0) then
                      do i=1,m
                         eps00  = peps0(i)
                         mu00   = pmu0(i)
                         E0.v   = pE(i).v
                         k0a0.v = pk0a(i).v
                         Kz0    = Kz_f4125_xmm4r4(eps00,mu00,E0,k0a0)
                         pKz(i) = Kz0
                      end do
                      if(n<12) return
                   end if
                   m1 = m+1
                    !dir$ assume_aligned peps0:16
                    !dir$ assume_aligned pmu0:16
                    !dir$ assume_aligned pE:16
                    !dir$ assume_aligned pk0a:16
                    !dir$ assume_aligned pKz:16
                    !dir$ vector aligned
                    !dir$ ivdep
                    !dir$ vector vectorlength(4)
                    !dir$ vector multiple_gather_scatter_by_shuffles 
                    !dir$ vector always
                 do i=m1,n,12
#if (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 1
                       call mm_prefetch(peps0(i+PF_DIST1),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST1),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST2),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST2),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 2   
                       call mm_prefetch(peps0(i+PF_DIST1),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST1),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST2),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST2),FOR_K_PREFETCH_T1,.true.,.false.) 
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                       call mm_prefetch(peps0(i+PF_DIST1),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST1),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST2),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST2),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                       call mm_prefetch(peps0(i+PF_DIST1),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST1),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST2),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST2),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif                 
                         eps00    = peps0(i+0)
                         mu00     = pmu0(i+0)
                         E0.v     = pE(i+0).v
                         k0a0.v   = pk0a(i+0).v
                         Kz0      = Kz_f4125_xmm4r4(eps00,mu00,E0,k0a0)
                         pKz(i+0) = Kz0 
                         eps01    = peps0(i+1)
                         mu01     = pmu0(i+1)
                         E1.v     = pE(i+1).v
                         k0a1.v   = pk0a(i+1).v
                         Kz1      = Kz_f4125_xmm4r4(eps01,mu01,E1,k0a1)
                         pKz(i+1) = Kz1 
                         eps02    = peps0(i+2)
                         mu02     = pmu0(i+2)
                         E2.v     = pE(i+2).v
                         k0a2.v   = pk0a(i+2).v
                         Kz2      = Kz_f4125_xmm4r4(eps02,mu02,E2,k0a2)
                         pKz(i+2) = Kz2
                         eps03    = peps0(i+3)
                         mu03     = pmu0(i+3)
                         E3.v     = pE(i+3).v
                         k0a3.v   = pk0a(i+3).v
                         Kz3      = Kz_f4125_xmm4r4(eps03,mu03,E3,k0a3)
                         pKz(i+3) = Kz3  
                         eps00    = peps0(i+4)
                         mu00     = pmu0(i+4)
                         E0.v     = pE(i+4).v
                         k0a0.v   = pk0a(i+4).v
                         Kz0      = Kz_f4125_xmm4r4(eps00,mu00,E0,k0a0)
                         pKz(i+4) = Kz0 
                         eps01    = peps0(i+5)
                         mu01     = pmu0(i+5)
                         E1.v     = pE(i+5).v
                         k0a1.v   = pk0a(i+5).v
                         Kz1      = Kz_f4125_xmm4r4(eps01,mu01,E1,k0a1)
                         pKz(i+5) = Kz1 
                         eps02    = peps0(i+6)
                         mu02     = pmu0(i+6)
                         E2.v     = pE(i+6).v
                         k0a2.v   = pk0a(i+6).v
                         Kz2      = Kz_f4125_xmm4r4(eps02,mu02,E2,k0a2)
                         pKz(i+6) = Kz2
                         eps03    = peps0(i+7)
                         mu03     = pmu0(i+7)
                         E3.v     = pE(i+7).v
                         k0a3.v   = pk0a(i+7).v
                         Kz3      = Kz_f4125_xmm4r4(eps03,mu03,E3,k0a3)
                         pKz(i+7) = Kz3  
                         eps00    = peps0(i+8)
                         mu00     = pmu0(i+8)
                         E0.v     = pE(i+8).v
                         k0a0.v   = pk0a(i+8).v
                         Kz0      = Kz_f4125_xmm4r4(eps00,mu00,E0,k0a0)
                         pKz(i+8) = Kz0 
                         eps01    = peps0(i+9)
                         mu01     = pmu0(i+9)
                         E1.v     = pE(i+9).v
                         k0a1.v   = pk0a(i+9).v
                         Kz1      = Kz_f4125_xmm4r4(eps01,mu01,E1,k0a1)
                         pKz(i+9) = Kz1 
                         eps02    = peps0(i+10)
                         mu02     = pmu0(i+10)
                         E2.v     = pE(i+10).v
                         k0a2.v   = pk0a(i+10).v
                         Kz2      = Kz_f4125_xmm4r4(eps02,mu02,E2,k0a2)
                         pKz(i+10) = Kz2
                         eps03    = peps0(i+11)
                         mu03     = pmu0(i+11)
                         E3.v     = pE(i+11).v
                         k0a3.v   = pk0a(i+11).v
                         Kz3      = Kz_f4125_xmm4r4(eps03,mu03,E3,k0a3)
                         pKz(i+11) = Kz3  
                     end do
             end subroutine Kz_f4125_xmm4r4_unroll12x
             
             
             subroutine Kz_f4125_xmm4r4_unroll8x(peps0,pmu0,pE,pk0a,pKz,PF_DIST1, &
                                                   n,PF_DIST1,PF_DIST2)
             
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: Kz_f4125_xmm4r4_unroll8x
                   !dir$ attributes forceinline :: Kz_f4125_xmm4r4_unroll8x
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: Kz_f4125_xmm4r4_unroll8x
                   real(kind=sp),   dimension(1:n), intent(in) :: peps0
                   real(kind=sp),   dimension(1:n), intent(in) :: pmu0
                   type(  XMM4c4_t),   dimension(1:n), intent(in) :: pE
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pk0a
                   type(  XMM4c4_t),   dimension(1:n), intent(in) :: pKz
                   integer(kind=i4),                intent(in) :: n
                   integer(kind=i4),                intent(in) :: PF_DIST1
                   integer(kind=i4),                intent(in) :: PF_DIST2
                   ! Locals
                      !dir$ attributes align : 16 :: Kz0
                      !dir$ attributes align : 16 :: Kz1
                      !dir$ attributes align : 16 :: Kz2
                      !dir$ attributes align : 16 :: Kz3
                      !dir$ attributes align : 16 :: E0
                      !dir$ attributes align : 16 :: E1
                      !dir$ attributes align : 16 :: E2
                      !dir$ attributes align : 16 :: E3
                      !dir$ attributes align : 16 :: k0a0
                      !dir$ attributes align : 16 :: k0a1
                      !dir$ attributes align : 16 :: k0a2
                      !dir$ attributes align : 16 :: k0a3
                      !dir$ attributes align : 16 :: eps00
                      !dir$ attributes align : 16 :: eps01
                      !dir$ attributes align : 16 :: eps02
                      !dir$ attributes align : 16 :: eps03
                      !dir$ attributes align : 16 :: mu00
                      !dir$ attributes align : 16 :: mu01
                      !dir$ attributes align : 16 :: mu02
                      !dir$ attributes align : 16 :: mu03
                   type(  XMM4c4_t),   automatic :: Kz0,Kz1,Kz2,Kz3
                   type(  XMM4c4_t),   automatic :: E0,E1,E2,E3
                   type(XMM4r4_t), automatic :: k0a0,k0a1,k0a2,k0a3
                   real(kind=sp),   automatic :: eps00,eps01,eps02,eps03
                   real(kind=sp),   automatic :: mu00,mu01,mu02,mu03
                   integer(kind=i4) :: i,m,m1
                   m = mod(n,8)
                   if(m/=0) then
                      do i=1,m
                         eps00  = peps0(i)
                         mu00   = pmu0(i)
                         E0.v   = pE(i).v
                         k0a0.v = pk0a(i).v
                         Kz0    = Kz_f4125_xmm4r4(eps00,mu00,E0,k0a0)
                         pKz(i) = Kz0
                      end do
                      if(n<8) return
                   end if
                   m1 = m+1
                    !dir$ assume_aligned peps0:16
                    !dir$ assume_aligned pmu0:16
                    !dir$ assume_aligned pE:16
                    !dir$ assume_aligned pk0a:16
                    !dir$ assume_aligned pKz:16
                    !dir$ vector aligned
                    !dir$ ivdep
                    !dir$ vector vectorlength(4)
                    !dir$ vector multiple_gather_scatter_by_shuffles 
                    !dir$ vector always
                 do i=m1,n,8
#if (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 1
                       call mm_prefetch(peps0(i+PF_DIST1),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST1),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST2),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST2),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 2   
                       call mm_prefetch(peps0(i+PF_DIST1),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST1),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST2),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST2),FOR_K_PREFETCH_T1,.true.,.false.) 
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                       call mm_prefetch(peps0(i+PF_DIST1),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST1),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST2),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST2),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                       call mm_prefetch(peps0(i+PF_DIST1),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST1),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST2),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST2),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif                 
                         eps00    = peps0(i+0)
                         mu00     = pmu0(i+0)
                         E0.v     = pE(i+0).v
                         k0a0.v   = pk0a(i+0).v
                         Kz0      = Kz_f4125_xmm4r4(eps00,mu00,E0,k0a0)
                         pKz(i+0) = Kz0 
                         eps01    = peps0(i+1)
                         mu01     = pmu0(i+1)
                         E1.v     = pE(i+1).v
                         k0a1.v   = pk0a(i+1).v
                         Kz1      = Kz_f4125_xmm4r4(eps01,mu01,E1,k0a1)
                         pKz(i+1) = Kz1 
                         eps02    = peps0(i+2)
                         mu02     = pmu0(i+2)
                         E2.v     = pE(i+2).v
                         k0a2.v   = pk0a(i+2).v
                         Kz2      = Kz_f4125_xmm4r4(eps02,mu02,E2,k0a2)
                         pKz(i+2) = Kz2
                         eps03    = peps0(i+3)
                         mu03     = pmu0(i+3)
                         E3.v     = pE(i+3).v
                         k0a3.v   = pk0a(i+3).v
                         Kz3      = Kz_f4125_xmm4r4(eps03,mu03,E3,k0a3)
                         pKz(i+3) = Kz3  
                         eps00    = peps0(i+4)
                         mu00     = pmu0(i+4)
                         E0.v     = pE(i+4).v
                         k0a0.v   = pk0a(i+4).v
                         Kz0      = Kz_f4125_xmm4r4(eps00,mu00,E0,k0a0)
                         pKz(i+4) = Kz0 
                         eps01    = peps0(i+5)
                         mu01     = pmu0(i+5)
                         E1.v     = pE(i+5).v
                         k0a1.v   = pk0a(i+5).v
                         Kz1      = Kz_f4125_xmm4r4(eps01,mu01,E1,k0a1)
                         pKz(i+5) = Kz1 
                         eps02    = peps0(i+6)
                         mu02     = pmu0(i+6)
                         E2.v     = pE(i+6).v
                         k0a2.v   = pk0a(i+6).v
                         Kz2      = Kz_f4125_xmm4r4(eps02,mu02,E2,k0a2)
                         pKz(i+6) = Kz2
                         eps03    = peps0(i+7)
                         mu03     = pmu0(i+7)
                         E3.v     = pE(i+7).v
                         k0a3.v   = pk0a(i+7).v
                         Kz3      = Kz_f4125_xmm4r4(eps03,mu03,E3,k0a3)
                         pKz(i+7) = Kz3  
                     end do
             end subroutine Kz_f4125_xmm4r4_unroll8x
             
             
             subroutine Kz_f4125_xmm4r4_unroll4x(peps0,pmu0,pE,pk0a,pKz,PF_DIST1, &
                                                   n,PF_DIST1,PF_DIST2)
             
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: Kz_f4125_xmm4r4_unroll4x
                   !dir$ attributes forceinline :: Kz_f4125_xmm4r4_unroll4x
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: Kz_f4125_xmm4r4_unroll4x
                   real(kind=sp),   dimension(1:n), intent(in) :: peps0
                   real(kind=sp),   dimension(1:n), intent(in) :: pmu0
                   type(  XMM4c4_t),   dimension(1:n), intent(in) :: pE
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pk0a
                   type(  XMM4c4_t),   dimension(1:n), intent(in) :: pKz
                   integer(kind=i4),                intent(in) :: n
                   integer(kind=i4),                intent(in) :: PF_DIST1
                   integer(kind=i4),                intent(in) :: PF_DIST2
                   ! Locals
                      !dir$ attributes align : 16 :: Kz0
                      !dir$ attributes align : 16 :: Kz1
                      !dir$ attributes align : 16 :: Kz2
                      !dir$ attributes align : 16 :: Kz3
                      !dir$ attributes align : 16 :: E0
                      !dir$ attributes align : 16 :: E1
                      !dir$ attributes align : 16 :: E2
                      !dir$ attributes align : 16 :: E3
                      !dir$ attributes align : 16 :: k0a0
                      !dir$ attributes align : 16 :: k0a1
                      !dir$ attributes align : 16 :: k0a2
                      !dir$ attributes align : 16 :: k0a3
                      !dir$ attributes align : 16 :: eps00
                      !dir$ attributes align : 16 :: eps01
                      !dir$ attributes align : 16 :: eps02
                      !dir$ attributes align : 16 :: eps03
                      !dir$ attributes align : 16 :: mu00
                      !dir$ attributes align : 16 :: mu01
                      !dir$ attributes align : 16 :: mu02
                      !dir$ attributes align : 16 :: mu03
                   type(  XMM4c4_t),   automatic :: Kz0,Kz1,Kz2,Kz3
                   type(  XMM4c4_t),   automatic :: E0,E1,E2,E3
                   type(XMM4r4_t), automatic :: k0a0,k0a1,k0a2,k0a3
                   real(kind=sp),   automatic :: eps00,eps01,eps02,eps03
                   real(kind=sp),   automatic :: mu00,mu01,mu02,mu03
                   integer(kind=i4) :: i,m,m1
                   m = mod(n,4)
                   if(m/=0) then
                      do i=1,m
                         eps00  = peps0(i)
                         mu00   = pmu0(i)
                         E0.v   = pE(i).v
                         k0a0.v = pk0a(i).v
                         Kz0    = Kz_f4125_xmm4r4(eps00,mu00,E0,k0a0)
                         pKz(i) = Kz0
                      end do
                      if(n<4) return
                   end if
                   m1 = m+1
                    !dir$ assume_aligned peps0:16
                    !dir$ assume_aligned pmu0:16
                    !dir$ assume_aligned pE:16
                    !dir$ assume_aligned pk0a:16
                    !dir$ assume_aligned pKz:16
                    !dir$ vector aligned
                    !dir$ ivdep
                    !dir$ vector vectorlength(4)
                    !dir$ vector multiple_gather_scatter_by_shuffles 
                    !dir$ vector always
                 do i=m1,n,4
#if (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 1
                       call mm_prefetch(peps0(i+PF_DIST1),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST1),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST2),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST2),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 2   
                       call mm_prefetch(peps0(i+PF_DIST1),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST1),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST2),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST2),FOR_K_PREFETCH_T1,.true.,.false.) 
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                       call mm_prefetch(peps0(i+PF_DIST1),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST1),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST2),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST2),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                       call mm_prefetch(peps0(i+PF_DIST1),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST1),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST2),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST2),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif                 
                         eps00    = peps0(i+0)
                         mu00     = pmu0(i+0)
                         E0.v     = pE(i+0).v
                         k0a0.v   = pk0a(i+0).v
                         Kz0      = Kz_f4125_xmm4r4(eps00,mu00,E0,k0a0)
                         pKz(i+0) = Kz0 
                         eps01    = peps0(i+1)
                         mu01     = pmu0(i+1)
                         E1.v     = pE(i+1).v
                         k0a1.v   = pk0a(i+1).v
                         Kz1      = Kz_f4125_xmm4r4(eps01,mu01,E1,k0a1)
                         pKz(i+1) = Kz1 
                         eps02    = peps0(i+2)
                         mu02     = pmu0(i+2)
                         E2.v     = pE(i+2).v
                         k0a2.v   = pk0a(i+2).v
                         Kz2      = Kz_f4125_xmm4r4(eps02,mu02,E2,k0a2)
                         pKz(i+2) = Kz2
                         eps03    = peps0(i+3)
                         mu03     = pmu0(i+3)
                         E3.v     = pE(i+3).v
                         k0a3.v   = pk0a(i+3).v
                         Kz3      = Kz_f4125_xmm4r4(eps03,mu03,E3,k0a3)
                         pKz(i+3) = Kz3  
                     end do
             end subroutine Kz_f4125_xmm4r4_unroll4x
                

             subroutine Kz_f4125_xmm4r4_rolled(peps0,pmu0,pE,pk0a,pKz,PF_DIST1, &
                                                   n,PF_DIST1,PF_DIST2)
             
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: Kz_f4125_xmm4r4_rolled
                   !dir$ attributes forceinline :: Kz_f4125_xmm4r4_rolled
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: Kz_f4125_xmm4r4_rolled
                   real(kind=sp),   dimension(1:n), intent(in) :: peps0
                   real(kind=sp),   dimension(1:n), intent(in) :: pmu0
                   type(  XMM4c4_t),   dimension(1:n), intent(in) :: pE
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pk0a
                   type(  XMM4c4_t),   dimension(1:n), intent(in) :: pKz
                   integer(kind=i4),                intent(in) :: n
                   integer(kind=i4),                intent(in) :: PF_DIST1
                   integer(kind=i4),                intent(in) :: PF_DIST2
                   ! Locals
                      !dir$ attributes align : 16 :: Kz0
                      !dir$ attributes align : 16 :: E0
                      !dir$ attributes align : 16 :: k0a0
                      !dir$ attributes align : 16 :: eps00
                      !dir$ attributes align : 16 :: mu00
                   
                   type(  XMM4c4_t),   automatic :: Kz0
                   type(  XMM4c4_t),   automatic :: E0
                   type(XMM4r4_t), automatic :: k0a0
                   real(kind=sp),   automatic :: eps00
                   real(kind=sp),   automatic :: mu00
                   integer(kind=i4) :: i
                 
                    !dir$ assume_aligned peps0:16
                    !dir$ assume_aligned pmu0:16
                    !dir$ assume_aligned pE:16
                    !dir$ assume_aligned pk0a:16
                    !dir$ assume_aligned pKz:16
                    !dir$ vector aligned
                    !dir$ ivdep
                    !dir$ vector vectorlength(4)
                    !dir$ vector multiple_gather_scatter_by_shuffles 
                    !dir$ vector always
                 do i=1,n
#if (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 1
                       call mm_prefetch(peps0(i+PF_DIST1),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST1),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST2),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST2),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 2   
                       call mm_prefetch(peps0(i+PF_DIST1),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST1),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST2),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST2),FOR_K_PREFETCH_T1,.true.,.false.) 
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                       call mm_prefetch(peps0(i+PF_DIST1),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST1),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST2),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST2),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                       call mm_prefetch(peps0(i+PF_DIST1),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST1),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST2),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST2),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif                 
                         eps00    = peps0(i+0)
                         mu00     = pmu0(i+0)
                         E0.v     = pE(i+0).v
                         k0a0.v   = pk0a(i+0).v
                         Kz0      = Kz_f4125_xmm4r4(eps00,mu00,E0,k0a0)
                         pKz(i+0) = Kz0 
                     end do
             end subroutine Kz_f4125_xmm4r4_rolled
             
             
             !/*
             !            Surface currents (k0a << 1), for long cylinder (wire).
             !             H-field cylinder axis parallel.
             !             Formula 4.1-26
             !      */
             
             pure function Kph_f4126_xmm4r4(H) result(Kph)
                  
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: Kph_f4126_xmm4r4
                   !dir$ attributes forceinline :: Kph_f4126_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: Kph_f4126_xmm4r4
                   type(  XMM4c4_t),  intent(in) :: H
                   type(  XMM4c4_t) :: Kph
                   ! Locals
                     !dir$ attributes align : 16 :: I
                   type(  XMM4c4_t), automatic :: I
                   I.re = -1.0_sp
                   I.im = -1.0_sp
                   Kph  = I*H
             end function Kph_f4126_xmm4r4
             
             
              !/*
              !          The toal current along the wire.
              !         Formula 4.1-27 
              !!
              !!     */   
            
             pure function Iz_f4127_xmm4r4(eps0,mu0,E,k0a,k0) result(Iz)
                  
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: Iz_f4127_xmm4r4
                   !dir$ attributes forceinline :: Iz_f4127_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: Iz_f4127_xmm4r4
                   use mod_vecconsts, only :  v4r4_0,v4r4_n1
                   type(XMM4r4_t),  intent(in) :: eps0
                   type(XMM4r4_t),  intent(in) :: mu0
                   type(  XMM4c4_t),    intent(in) :: E
                   type(XMM4r4_t),  intent(in) :: k0a
                   type(XMM4r4_t),  intent(in) :: k0
                   type(  XMM4c4_t) :: Iz
                   ! Locals
                     !dir$ attributes align : 16 :: t0
                     !dir$ attributes align : 16 :: t1
                     !dir$ attributes align : 16 :: t2
                     !dir$ attributes align : 16 :: div
                     !dir$ attributes align : 16 :: sqr
                     !dir$ attributes align : 16 :: lna
                     !dir$ attributes align : 16 :: ln
                     !dir$ attributes align : 16 :: C6283185307179586476925286766559
                     !dir$ attributes align : 16 :: C157079632679489661923132169164 
                     !dir$ attributes align : 16 :: C08905
                   type(  XMM4c4_t),   automatic :: t0,t1
                   type(  XMM4c4_t),   automatic :: t2,div
                   type(XMM4r4_t), automatic :: sqr
                   type(XMM4r4_t), automatic :: lna,ln
                   type(XMM4r4_t), parameter :: C6283185307179586476925286766559 =    &
                                        XMM4r4_t(6.283185307179586476925286766559_sp) ! 2*PI
                   type(XMM4r4_t), parameter :: C157079632679489661923132169164  =    &
                                        XMM4r4_t(1.57079632679489661923132169164_sp)  ! PI/2
                   type(XMM4r4_t), parameter :: C08905 = XMM4r4_t(0.8905_sp)
                   lna.v  = k0a.v*C08905.v
                   t0.re  =  v4r4_0.v
                   t2     = E*C6283185307179586476925286766559
                   sqr.v  = sqrt(eps0.v/mu0.v)
                   t1.im  = v16_neg1.v*C157079632679489661923132169164.v  
                   ln.v   = log(lna.v)
                   t0.im  = v4r4_n1.v*sqr.v
                   div    = t2/t1
                   Iz     = t0*div
             end function Iz_f4127_xmm4r4


             subroutine Iz_f4127_xmm4r4_unroll16x(peps0,pmu0,pE,pk0a,
                                                   pk0,pIz,n,PF_DIST)
                                                   
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: Iz_f4127_xmm4r4_unroll16x
                   !dir$ attributes forceinline :: Iz_f4127_xmm4r4_unroll16x
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: Iz_f4127_xmm4r4_unroll16x
                   type(XMM4r4_t), dimension(1:n), intent(in) :: peps0
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pmu0
                   type(  XMM4c4_t),   dimension(1:n), intent(in) :: pE
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pk0a
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pk0
                   type(  XMM4c4_t),   dimension(1:n), intent(out):: pIz
                   integer(kind=i4),                intent(in) :: n
                   integer(kind=i4),                intent(in) :: PF_DIST
                   !Locals
                     !dir$ attributes align : 16 :: E0
                     !dir$ attributes align : 16 :: E1
                     !dir$ attributes align : 16 :: E2
                     !dir$ attributes align : 16 :: E3
                     !dir$ attributes align : 16 :: eps00
                     !dir$ attributes align : 16 :: eps01
                     !dir$ attributes align : 16 :: eps02
                     !dir$ attributes align : 16 :: eps03
                     !dir$ attributes align : 16 :: mu00
                     !dir$ attributes align : 16 :: mu01
                     !dir$ attributes align : 16 :: mu02
                     !dir$ attributes align : 16 :: mu03
                     !dir$ attributes align : 16 :: k0a0
                     !dir$ attributes align : 16 :: k0a1
                     !dir$ attributes align : 16 :: k0a2
                     !dir$ attributes align : 16 :: k0a3
                     !dir$ attributes align : 16 :: k00
                     !dir$ attributes align : 16 :: k01
                     !dir$ attributes align : 16 :: k02
                     !dir$ attributes align : 16 :: k03
                   type(  XMM4c4_t),   automatic :: E0,E1,E2,E3
                   !type(  XMM4c4_t),   automatic :: Iz0,Iz1,Iz2,Iz3
                   type(XMM4r4_t), automatic :: eps00,eps01,eps02,eps03
                   type(XMM4r4_t), automatic :: mu00,mu01,mu02,mu03
                   type(XMM4r4_t), automatic :: k0a0,k0a1,k0a2,k0a3
                   type(XMM4r4_t), automatic :: k00,k01,k02,k03
                   integer(kind=i4) :: i,m,m1
                   m = mod(n,16)
                   if(m/=0) then
                      do i=1,m
                         eps00.v  = peps0(i).v
                         mu00.v   = pmu0(i).v
                         E0.re    = pE(i).re
                         E0.im    = pE(i).im
                         k0a0.v   = pk0a(i).v
                         k00.v    = pk0(i).v
                         pIz(i)   = Iz_f4127_xmm4r4(eps00,mu00,E0,k0a0,k00)
                      end do
                      if(n<16) return
                   end if
                   m1 = m+1
                    !dir$ assume_aligned peps0:16
                    !dir$ assume_aligned pmu0:16
                    !dir$ assume_aligned pE:16
                    !dir$ assume_aligned pk0a:16
                    !dir$ assume_aligned pk0:16
                    !dir$ assume_aligned pIz:16
                    !dir$ vector aligned
                    !dir$ ivdep
                    !dir$ vector vectorlength(4)
                    !dir$ vector multiple_gather_scatter_by_shuffles 
                    !dir$ vector always
                  do i=m1,n,16
#if (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 1
                       call mm_prefetch(peps0(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pk0(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 2   
                       call mm_prefetch(peps0(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.) 
                       call mm_prefetch(pk0(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                       call mm_prefetch(peps0(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pk0(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                       call mm_prefetch(peps0(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pk0(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif         
                         eps00.v  = peps0(i+0).v
                         mu00.v   = pmu0(i+0).v
                         E0.re    = pE(i+0).re
                         E0.im    = pE(i+0).im
                         k0a0.v   = pk0a(i+0).v
                         k00.v    = pk0(i+0).v
                         pIz(i+0) = Iz_f4127_xmm4r4(eps00,mu00,E0,k0a0,k00)
                         eps01.v  = peps0(i+1).v
                         mu01.v   = pmu0(i+1).v
                         E1.re    = pE(i+1).re
                         E1.im    = pE(i+1).im
                         k0a1.v   = pk0a(i+1).v
                         k01.v    = pk0(i+1).v
                         pIz(i+1) = Iz_f4127_xmm4r4(eps01,mu01,E1,k0a1,k01)
                         eps02.v  = peps0(i+2).v
                         mu02.v   = pmu0(i+2).v
                         E2.re    = pE(i+2).re
                         E2.im    = pE(i+2).im
                         k0a2.v   = pk0a(i+2).v
                         k02.v    = pk0(i+2).v
                         pIz(i+2) = Iz_f4127_xmm4r4(eps02,mu02,E2,k0a2,k02)
                         eps03.v  = peps0(i+3).v
                         mu03.v   = pmu0(i+3).v
                         E3.re    = pE(i+3).re
                         E3.im    = pE(i+3).im
                         k0a3.v   = pk0a(i+3).v
                         k03.v    = pk0(i+3).v
                         pIz(i+3) = Iz_f4127_xmm4r4(eps03,mu03,E3,k0a3,k03)
                         eps00.v  = peps0(i+4).v
                         mu00.v   = pmu0(i+4).v
                         E0.re    = pE(i+4).re
                         E0.im    = pE(i+4).im
                         k0a0.v   = pk0a(i+4).v
                         k00.v    = pk0(i+4).v
                         pIz(i+4) = Iz_f4127_xmm4r4(eps00,mu00,E0,k0a0,k00)
                         eps01.v  = peps0(i+5).v
                         mu01.v   = pmu0(i+5).v
                         E1.re    = pE(i+5).re
                         E1.im    = pE(i+5).im
                         k0a1.v   = pk0a(i+5).v
                         k01.v    = pk0(i+5).v
                         pIz(i+5) = Iz_f4127_xmm4r4(eps01,mu01,E1,k0a1,k01)
                         eps02.v  = peps0(i+6).v
                         mu02.v   = pmu0(i+6).v
                         E2.re    = pE(i+6).re
                         E2.im    = pE(i+6).im
                         k0a2.v   = pk0a(i+6).v
                         k02.v    = pk0(i+6).v
                         pIz(i+6) = Iz_f4127_xmm4r4(eps02,mu02,E2,k0a2,k02)
                         eps03.v  = peps0(i+7).v
                         mu03.v   = pmu0(i+7).v
                         E3.re    = pE(i+7).re
                         E3.im    = pE(i+7).im
                         k0a3.v   = pk0a(i+7).v
                         k03.v    = pk0(i+7).v
                         pIz(i+7) = Iz_f4127_xmm4r4(eps03,mu03,E3,k0a3,k03)
                         eps00.v  = peps0(i+8).v
                         mu00.v   = pmu0(i+8).v
                         E0.re    = pE(i+8).re
                         E0.im    = pE(i+8).im
                         k0a0.v   = pk0a(i+8).v
                         k00.v    = pk0(i+8).v
                         pIz(i+8) = Iz_f4127_xmm4r4(eps00,mu00,E0,k0a0,k00)
                         eps01.v  = peps0(i+9).v
                         mu01.v   = pmu0(i+9).v
                         E1.re    = pE(i+9).re
                         E1.im    = pE(i+9).im
                         k0a1.v   = pk0a(i+9).v
                         k01.v    = pk0(i+9).v
                         pIz(i+9) = Iz_f4127_xmm4r4(eps01,mu01,E1,k0a1,k01)
                         eps02.v  = peps0(i+10).v
                         mu02.v   = pmu0(i+10).v
                         E2.re    = pE(i+10).re
                         E2.im    = pE(i+10).im
                         k0a2.v   = pk0a(i+10).v
                         k02.v    = pk0(i+10).v
                         pIz(i+10) = Iz_f4127_xmm4r4(eps02,mu02,E2,k0a2,k02)
                         eps03.v  = peps0(i+11).v
                         mu03.v   = pmu0(i+11).v
                         E3.re    = pE(i+11).re
                         E3.im    = pE(i+11).im
                         k0a3.v   = pk0a(i+11).v
                         k03.v    = pk0(i+11).v
                         pIz(i+11) = Iz_f4127_xmm4r4(eps03,mu03,E3,k0a3,k03)
                         eps03.v  = peps0(i+12).v
                         mu03.v   = pmu0(i+12).v
                         E3.re    = pE(i+12).re
                         E3.im    = pE(i+12).im
                         k0a3.v   = pk0a(i+12).v
                         k03.v    = pk0(i+12).v
                         pIz(i+12) = Iz_f4127_xmm4r4(eps03,mu03,E3,k0a3,k03)
                         eps03.v  = peps0(i+13).v
                         mu03.v   = pmu0(i+13).v
                         E3.re    = pE(i+13).re
                         E3.im    = pE(i+13).im
                         k0a3.v   = pk0a(i+13).v
                         k03.v    = pk0(i+13).v
                         pIz(i+13) = Iz_f4127_xmm4r4(eps03,mu03,E3,k0a3,k03)
                         eps03.v  = peps0(i+14).v
                         mu03.v   = pmu0(i+14).v
                         E3.re    = pE(i+14).re
                         E3.im    = pE(i+14).im
                         k0a3.v   = pk0a(i+14).v
                         k03.v    = pk0(i+14).v
                         pIz(i+14) = Iz_f4127_xmm4r4(eps03,mu03,E3,k0a3,k03)
                         eps03.v  = peps0(i+15).v
                         mu03.v   = pmu0(i+15).v
                         E3.re    = pE(i+15).re
                         E3.im    = pE(i+15).im
                         k0a3.v   = pk0a(i+15).v
                         k03.v    = pk0(i+15).v
                         pIz(i+15) = Iz_f4127_xmm4r4(eps03,mu03,E3,k0a3,k03)
                  end do
             end subroutine Iz_f4127_xmm4r4_unroll16x
             
             
             subroutine Iz_f4127_xmm4r4_unroll12x(peps0,pmu0,pE,pk0a,
                                                   pk0,pIz,n,PF_DIST)
                                                   
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: Iz_f4127_xmm4r4_unroll12x
                   !dir$ attributes forceinline :: Iz_f4127_xmm4r4_unroll12x
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: Iz_f4127_xmm4r4_unroll12x
                   type(XMM4r4_t), dimension(1:n), intent(in) :: peps0
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pmu0
                   type(  XMM4c4_t),   dimension(1:n), intent(in) :: pE
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pk0a
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pk0
                   type(  XMM4c4_t),   dimension(1:n), intent(out):: pIz
                   integer(kind=i4),                intent(in) :: n
                   integer(kind=i4),                intent(in) :: PF_DIST
                   !Locals
                     !dir$ attributes align : 16 :: E0
                     !dir$ attributes align : 16 :: E1
                     !dir$ attributes align : 16 :: E2
                     !dir$ attributes align : 16 :: E3
                     !dir$ attributes align : 16 :: eps00
                     !dir$ attributes align : 16 :: eps01
                     !dir$ attributes align : 16 :: eps02
                     !dir$ attributes align : 16 :: eps03
                     !dir$ attributes align : 16 :: mu00
                     !dir$ attributes align : 16 :: mu01
                     !dir$ attributes align : 16 :: mu02
                     !dir$ attributes align : 16 :: mu03
                     !dir$ attributes align : 16 :: k0a0
                     !dir$ attributes align : 16 :: k0a1
                     !dir$ attributes align : 16 :: k0a2
                     !dir$ attributes align : 16 :: k0a3
                     !dir$ attributes align : 16 :: k00
                     !dir$ attributes align : 16 :: k01
                     !dir$ attributes align : 16 :: k02
                     !dir$ attributes align : 16 :: k03
                   type(  XMM4c4_t),   automatic :: E0,E1,E2,E3
                   !type(  XMM4c4_t),   automatic :: Iz0,Iz1,Iz2,Iz3
                   type(XMM4r4_t), automatic :: eps00,eps01,eps02,eps03
                   type(XMM4r4_t), automatic :: mu00,mu01,mu02,mu03
                   type(XMM4r4_t), automatic :: k0a0,k0a1,k0a2,k0a3
                   type(XMM4r4_t), automatic :: k00,k01,k02,k03
                   integer(kind=i4) :: i,m,m1
                   m = mod(n,12)
                   if(m/=0) then
                      do i=1,m
                         eps00.v  = peps0(i).v
                         mu00.v   = pmu0(i).v
                         E0.re    = pE(i).re
                         E0.im    = pE(i).im
                         k0a0.v   = pk0a(i).v
                         k00.v    = pk0(i).v
                         pIz(i)   = Iz_f4127_xmm4r4(eps00,mu00,E0,k0a0,k00)
                      end do
                      if(n<12) return
                   end if
                   m1 = m+1
                    !dir$ assume_aligned peps0:16
                    !dir$ assume_aligned pmu0:16
                    !dir$ assume_aligned pE:16
                    !dir$ assume_aligned pk0a:16
                    !dir$ assume_aligned pk0:16
                    !dir$ assume_aligned pIz:16
                    !dir$ vector aligned
                    !dir$ ivdep
                    !dir$ vector vectorlength(4)
                    !dir$ vector multiple_gather_scatter_by_shuffles 
                    !dir$ vector always
                  do i=m1,n,12
#if (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 1
                       call mm_prefetch(peps0(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pk0(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 2   
                       call mm_prefetch(peps0(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.) 
                       call mm_prefetch(pk0(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                       call mm_prefetch(peps0(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pk0(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                       call mm_prefetch(peps0(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pk0(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif         
                         eps00.v  = peps0(i+0).v
                         mu00.v   = pmu0(i+0).v
                         E0.re    = pE(i+0).re
                         E0.im    = pE(i+0).im
                         k0a0.v   = pk0a(i+0).v
                         k00.v    = pk0(i+0).v
                         pIz(i+0) = Iz_f4127_xmm4r4(eps00,mu00,E0,k0a0,k00)
                         eps01.v  = peps0(i+1).v
                         mu01.v   = pmu0(i+1).v
                         E1.re    = pE(i+1).re
                         E1.im    = pE(i+1).im
                         k0a1.v   = pk0a(i+1).v
                         k01.v    = pk0(i+1).v
                         pIz(i+1) = Iz_f4127_xmm4r4(eps01,mu01,E1,k0a1,k01)
                         eps02.v  = peps0(i+2).v
                         mu02.v   = pmu0(i+2).v
                         E2.re    = pE(i+2).re
                         E2.im    = pE(i+2).im
                         k0a2.v   = pk0a(i+2).v
                         k02.v    = pk0(i+2).v
                         pIz(i+2) = Iz_f4127_xmm4r4(eps02,mu02,E2,k0a2,k02)
                         eps03.v  = peps0(i+3).v
                         mu03.v   = pmu0(i+3).v
                         E3.re    = pE(i+3).re
                         E3.im    = pE(i+3).im
                         k0a3.v   = pk0a(i+3).v
                         k03.v    = pk0(i+3).v
                         pIz(i+3) = Iz_f4127_xmm4r4(eps03,mu03,E3,k0a3,k03)
                         eps00.v  = peps0(i+4).v
                         mu00.v   = pmu0(i+4).v
                         E0.re    = pE(i+4).re
                         E0.im    = pE(i+4).im
                         k0a0.v   = pk0a(i+4).v
                         k00.v    = pk0(i+4).v
                         pIz(i+4) = Iz_f4127_xmm4r4(eps00,mu00,E0,k0a0,k00)
                         eps01.v  = peps0(i+5).v
                         mu01.v   = pmu0(i+5).v
                         E1.re    = pE(i+5).re
                         E1.im    = pE(i+5).im
                         k0a1.v   = pk0a(i+5).v
                         k01.v    = pk0(i+5).v
                         pIz(i+5) = Iz_f4127_xmm4r4(eps01,mu01,E1,k0a1,k01)
                         eps02.v  = peps0(i+6).v
                         mu02.v   = pmu0(i+6).v
                         E2.re    = pE(i+6).re
                         E2.im    = pE(i+6).im
                         k0a2.v   = pk0a(i+6).v
                         k02.v    = pk0(i+6).v
                         pIz(i+6) = Iz_f4127_xmm4r4(eps02,mu02,E2,k0a2,k02)
                         eps03.v  = peps0(i+7).v
                         mu03.v   = pmu0(i+7).v
                         E3.re    = pE(i+7).re
                         E3.im    = pE(i+7).im
                         k0a3.v   = pk0a(i+7).v
                         k03.v    = pk0(i+7).v
                         pIz(i+7) = Iz_f4127_xmm4r4(eps03,mu03,E3,k0a3,k03)
                         eps00.v  = peps0(i+8).v
                         mu00.v   = pmu0(i+8).v
                         E0.re    = pE(i+8).re
                         E0.im    = pE(i+8).im
                         k0a0.v   = pk0a(i+8).v
                         k00.v    = pk0(i+8).v
                         pIz(i+8) = Iz_f4127_xmm4r4(eps00,mu00,E0,k0a0,k00)
                         eps01.v  = peps0(i+9).v
                         mu01.v   = pmu0(i+9).v
                         E1.re    = pE(i+9).re
                         E1.im    = pE(i+9).im
                         k0a1.v   = pk0a(i+9).v
                         k01.v    = pk0(i+9).v
                         pIz(i+9) = Iz_f4127_xmm4r4(eps01,mu01,E1,k0a1,k01)
                         eps02.v  = peps0(i+10).v
                         mu02.v   = pmu0(i+10).v
                         E2.re    = pE(i+10).re
                         E2.im    = pE(i+10).im
                         k0a2.v   = pk0a(i+10).v
                         k02.v    = pk0(i+10).v
                         pIz(i+10) = Iz_f4127_xmm4r4(eps02,mu02,E2,k0a2,k02)
                         eps03.v  = peps0(i+11).v
                         mu03.v   = pmu0(i+11).v
                         E3.re    = pE(i+11).re
                         E3.im    = pE(i+11).im
                         k0a3.v   = pk0a(i+11).v
                         k03.v    = pk0(i+11).v
                         pIz(i+11) = Iz_f4127_xmm4r4(eps03,mu03,E3,k0a3,k03)
                     end do
             end subroutine Iz_f4127_xmm4r4_unroll12x
             

             subroutine Iz_f4127_xmm4r4_unroll8x(peps0,pmu0,pE,pk0a,
                                                   pk0,pIz,n,PF_DIST)
                                                   
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: Iz_f4127_xmm4r4_unroll8x
                   !dir$ attributes forceinline :: Iz_f4127_xmm4r4_unroll8x
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: Iz_f4127_xmm4r4_unroll8x
                   type(XMM4r4_t), dimension(1:n), intent(in) :: peps0
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pmu0
                   type(  XMM4c4_t),   dimension(1:n), intent(in) :: pE
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pk0a
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pk0
                   type(  XMM4c4_t),   dimension(1:n), intent(out):: pIz
                   integer(kind=i4),                intent(in) :: n
                   integer(kind=i4),                intent(in) :: PF_DIST
                   !Locals
                     !dir$ attributes align : 16 :: E0
                     !dir$ attributes align : 16 :: E1
                     !dir$ attributes align : 16 :: E2
                     !dir$ attributes align : 16 :: E3
                     !dir$ attributes align : 16 :: eps00
                     !dir$ attributes align : 16 :: eps01
                     !dir$ attributes align : 16 :: eps02
                     !dir$ attributes align : 16 :: eps03
                     !dir$ attributes align : 16 :: mu00
                     !dir$ attributes align : 16 :: mu01
                     !dir$ attributes align : 16 :: mu02
                     !dir$ attributes align : 16 :: mu03
                     !dir$ attributes align : 16 :: k0a0
                     !dir$ attributes align : 16 :: k0a1
                     !dir$ attributes align : 16 :: k0a2
                     !dir$ attributes align : 16 :: k0a3
                     !dir$ attributes align : 16 :: k00
                     !dir$ attributes align : 16 :: k01
                     !dir$ attributes align : 16 :: k02
                     !dir$ attributes align : 16 :: k03
                   type(  XMM4c4_t),   automatic :: E0,E1,E2,E3
                   !type(  XMM4c4_t),   automatic :: Iz0,Iz1,Iz2,Iz3
                   type(XMM4r4_t), automatic :: eps00,eps01,eps02,eps03
                   type(XMM4r4_t), automatic :: mu00,mu01,mu02,mu03
                   type(XMM4r4_t), automatic :: k0a0,k0a1,k0a2,k0a3
                   type(XMM4r4_t), automatic :: k00,k01,k02,k03
                   integer(kind=i4) :: i,m,m1
                   m = mod(n,8)
                   if(m/=0) then
                      do i=1,m
                         eps00.v  = peps0(i).v
                         mu00.v   = pmu0(i).v
                         E0.re    = pE(i).re
                         E0.im    = pE(i).im
                         k0a0.v   = pk0a(i).v
                         k00.v    = pk0(i).v
                         pIz(i)   = Iz_f4127_xmm4r4(eps00,mu00,E0,k0a0,k00)
                      end do
                      if(n<8) return
                   end if
                   m1 = m+1
                    !dir$ assume_aligned peps0:16
                    !dir$ assume_aligned pmu0:16
                    !dir$ assume_aligned pE:16
                    !dir$ assume_aligned pk0a:16
                    !dir$ assume_aligned pk0:16
                    !dir$ assume_aligned pIz:16
                    !dir$ vector aligned
                    !dir$ ivdep
                    !dir$ vector vectorlength(4)
                    !dir$ vector multiple_gather_scatter_by_shuffles 
                    !dir$ vector always
                  do i=m1,n,8
#if (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 1
                       call mm_prefetch(peps0(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pk0(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 2   
                       call mm_prefetch(peps0(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.) 
                       call mm_prefetch(pk0(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                       call mm_prefetch(peps0(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pk0(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                       call mm_prefetch(peps0(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pk0(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif         
                         eps00.v  = peps0(i+0).v
                         mu00.v   = pmu0(i+0).v
                         E0.re    = pE(i+0).re
                         E0.im    = pE(i+0).im
                         k0a0.v   = pk0a(i+0).v
                         k00.v    = pk0(i+0).v
                         pIz(i+0) = Iz_f4127_xmm4r4(eps00,mu00,E0,k0a0,k00)
                         eps01.v  = peps0(i+1).v
                         mu01.v   = pmu0(i+1).v
                         E1.re    = pE(i+1).re
                         E1.im    = pE(i+1).im
                         k0a1.v   = pk0a(i+1).v
                         k01.v    = pk0(i+1).v
                         pIz(i+1) = Iz_f4127_xmm4r4(eps01,mu01,E1,k0a1,k01)
                         eps02.v  = peps0(i+2).v
                         mu02.v   = pmu0(i+2).v
                         E2.re    = pE(i+2).re
                         E2.im    = pE(i+2).im
                         k0a2.v   = pk0a(i+2).v
                         k02.v    = pk0(i+2).v
                         pIz(i+2) = Iz_f4127_xmm4r4(eps02,mu02,E2,k0a2,k02)
                         eps03.v  = peps0(i+3).v
                         mu03.v   = pmu0(i+3).v
                         E3.re    = pE(i+3).re
                         E3.im    = pE(i+3).im
                         k0a3.v   = pk0a(i+3).v
                         k03.v    = pk0(i+3).v
                         pIz(i+3) = Iz_f4127_xmm4r4(eps03,mu03,E3,k0a3,k03)
                         eps00.v  = peps0(i+4).v
                         mu00.v   = pmu0(i+4).v
                         E0.re    = pE(i+4).re
                         E0.im    = pE(i+4).im
                         k0a0.v   = pk0a(i+4).v
                         k00.v    = pk0(i+4).v
                         pIz(i+4) = Iz_f4127_xmm4r4(eps00,mu00,E0,k0a0,k00)
                         eps01.v  = peps0(i+5).v
                         mu01.v   = pmu0(i+5).v
                         E1.re    = pE(i+5).re
                         E1.im    = pE(i+5).im
                         k0a1.v   = pk0a(i+5).v
                         k01.v    = pk0(i+5).v
                         pIz(i+5) = Iz_f4127_xmm4r4(eps01,mu01,E1,k0a1,k01)
                         eps02.v  = peps0(i+6).v
                         mu02.v   = pmu0(i+6).v
                         E2.re    = pE(i+6).re
                         E2.im    = pE(i+6).im
                         k0a2.v   = pk0a(i+6).v
                         k02.v    = pk0(i+6).v
                         pIz(i+6) = Iz_f4127_xmm4r4(eps02,mu02,E2,k0a2,k02)
                         eps03.v  = peps0(i+7).v
                         mu03.v   = pmu0(i+7).v
                         E3.re    = pE(i+7).re
                         E3.im    = pE(i+7).im
                         k0a3.v   = pk0a(i+7).v
                         k03.v    = pk0(i+7).v
                         pIz(i+7) = Iz_f4127_xmm4r4(eps03,mu03,E3,k0a3,k03)
                     end do
             end subroutine Iz_f4127_xmm4r4_unroll8x
             
             
             subroutine Iz_f4127_xmm4r4_unroll4x(peps0,pmu0,pE,pk0a,
                                                   pk0,pIz,n,PF_DIST)
                                                   
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: Iz_f4127_xmm4r4_unroll4x
                   !dir$ attributes forceinline :: Iz_f4127_xmm4r4_unroll4x
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: Iz_f4127_xmm4r4_unroll4x
                   type(XMM4r4_t), dimension(1:n), intent(in) :: peps0
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pmu0
                   type(  XMM4c4_t),   dimension(1:n), intent(in) :: pE
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pk0a
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pk0
                   type(  XMM4c4_t),   dimension(1:n), intent(out):: pIz
                   integer(kind=i4),                intent(in) :: n
                   integer(kind=i4),                intent(in) :: PF_DIST
                   !Locals
                     !dir$ attributes align : 16 :: E0
                     !dir$ attributes align : 16 :: E1
                     !dir$ attributes align : 16 :: E2
                     !dir$ attributes align : 16 :: E3
                     !dir$ attributes align : 16 :: eps00
                     !dir$ attributes align : 16 :: eps01
                     !dir$ attributes align : 16 :: eps02
                     !dir$ attributes align : 16 :: eps03
                     !dir$ attributes align : 16 :: mu00
                     !dir$ attributes align : 16 :: mu01
                     !dir$ attributes align : 16 :: mu02
                     !dir$ attributes align : 16 :: mu03
                     !dir$ attributes align : 16 :: k0a0
                     !dir$ attributes align : 16 :: k0a1
                     !dir$ attributes align : 16 :: k0a2
                     !dir$ attributes align : 16 :: k0a3
                     !dir$ attributes align : 16 :: k00
                     !dir$ attributes align : 16 :: k01
                     !dir$ attributes align : 16 :: k02
                     !dir$ attributes align : 16 :: k03
                   type(  XMM4c4_t),   automatic :: E0,E1,E2,E3
                   !type(  XMM4c4_t),   automatic :: Iz0,Iz1,Iz2,Iz3
                   type(XMM4r4_t), automatic :: eps00,eps01,eps02,eps03
                   type(XMM4r4_t), automatic :: mu00,mu01,mu02,mu03
                   type(XMM4r4_t), automatic :: k0a0,k0a1,k0a2,k0a3
                   type(XMM4r4_t), automatic :: k00,k01,k02,k03
                   integer(kind=i4) :: i,m,m1
                   m = mod(n,4)
                   if(m/=0) then
                      do i=1,m
                         eps00.v  = peps0(i).v
                         mu00.v   = pmu0(i).v
                         E0.re    = pE(i).re
                         E0.im    = pE(i).im
                         k0a0.v   = pk0a(i).v
                         k00.v    = pk0(i).v
                         pIz(i)   = Iz_f4127_xmm4r4(eps00,mu00,E0,k0a0,k00)
                      end do
                      if(n<4) return
                   end if
                   m1 = m+1
                    !dir$ assume_aligned peps0:16
                    !dir$ assume_aligned pmu0:16
                    !dir$ assume_aligned pE:16
                    !dir$ assume_aligned pk0a:16
                    !dir$ assume_aligned pk0:16
                    !dir$ assume_aligned pIz:16
                    !dir$ vector aligned
                    !dir$ ivdep
                    !dir$ vector vectorlength(4)
                    !dir$ vector multiple_gather_scatter_by_shuffles 
                    !dir$ vector always
                  do i=m1,n,4
#if (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 1
                       call mm_prefetch(peps0(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pk0(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 2   
                       call mm_prefetch(peps0(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.) 
                       call mm_prefetch(pk0(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                       call mm_prefetch(peps0(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pk0(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                       call mm_prefetch(peps0(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pk0(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif         
                         eps00.v  = peps0(i+0).v
                         mu00.v   = pmu0(i+0).v
                         E0.re    = pE(i+0).re
                         E0.im    = pE(i+0).im
                         k0a0.v   = pk0a(i+0).v
                         k00.v    = pk0(i+0).v
                         pIz(i+0) = Iz_f4127_xmm4r4(eps00,mu00,E0,k0a0,k00)
                         eps01.v  = peps0(i+1).v
                         mu01.v   = pmu0(i+1).v
                         E1.re    = pE(i+1).re
                         E1.im    = pE(i+1).im
                         k0a1.v   = pk0a(i+1).v
                         k01.v    = pk0(i+1).v
                         pIz(i+1) = Iz_f4127_xmm4r4(eps01,mu01,E1,k0a1,k01)
                         eps02.v  = peps0(i+2).v
                         mu02.v   = pmu0(i+2).v
                         E2.re    = pE(i+2).re
                         E2.im    = pE(i+2).im
                         k0a2.v   = pk0a(i+2).v
                         k02.v    = pk0(i+2).v
                         pIz(i+2) = Iz_f4127_xmm4r4(eps02,mu02,E2,k0a2,k02)
                         eps03.v  = peps0(i+3).v
                         mu03.v   = pmu0(i+3).v
                         E3.re    = pE(i+3).re
                         E3.im    = pE(i+3).im
                         k0a3.v   = pk0a(i+3).v
                         k03.v    = pk0(i+3).v
                         pIz(i+3) = Iz_f4127_xmm4r4(eps03,mu03,E3,k0a3,k03)
                      end do
             end subroutine Iz_f4127_xmm4r4_unroll4x
             
             
             subroutine Iz_f4127_xmm4r4_rolled(peps0,pmu0,pE,pk0a,
                                                pk0,pIz,n,PF_DIST)
                                                   
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: Iz_f4127_xmm4r4_rolled
                   !dir$ attributes forceinline :: Iz_f4127_xmm4r4_rolled
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: Iz_f4127_xmm4r4_rolled
                   type(XMM4r4_t), dimension(1:n), intent(in) :: peps0
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pmu0
                   type(  XMM4c4_t),   dimension(1:n), intent(in) :: pE
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pk0a
                   type(XMM4r4_t), dimension(1:n), intent(in) :: pk0
                   type(  XMM4c4_t),   dimension(1:n), intent(out):: pIz
                   integer(kind=i4),                intent(in) :: n
                   integer(kind=i4),                intent(in) :: PF_DIST
                   !Locals
                     !dir$ attributes align : 16 :: E0
                     !dir$ attributes align : 16 :: eps00
                     !dir$ attributes align : 16 :: mu00
                     !dir$ attributes align : 16 :: k0a0
                     !dir$ attributes align : 16 :: k00
                  
                   type(  XMM4c4_t),   automatic :: E0
                   !type(  XMM4c4_t),   automatic :: Iz0,Iz1,Iz2,Iz3
                   type(XMM4r4_t), automatic :: eps00
                   type(XMM4r4_t), automatic :: mu00
                   type(XMM4r4_t), automatic :: k0a0
                   type(XMM4r4_t), automatic :: k00
                   integer(kind=i4) :: i
                  
                    !dir$ assume_aligned peps0:16
                    !dir$ assume_aligned pmu0:16
                    !dir$ assume_aligned pE:16
                    !dir$ assume_aligned pk0a:16
                    !dir$ assume_aligned pk0:16
                    !dir$ assume_aligned pIz:16
                    !dir$ vector aligned
                    !dir$ ivdep
                    !dir$ vector vectorlength(4)
                    !dir$ vector multiple_gather_scatter_by_shuffles 
                    !dir$ vector always
                  do i=1,n
#if (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 1
                       call mm_prefetch(peps0(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
                       call mm_prefetch(pk0(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_XMM4R4_PF_CACHE_HINT__) == 2   
                       call mm_prefetch(peps0(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.) 
                       call mm_prefetch(pk0(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                       call mm_prefetch(peps0(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
                       call mm_prefetch(pk0(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                       call mm_prefetch(peps0(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pmu0(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pE(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pk0a(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
                       call mm_prefetch(pk0(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif         
                         eps00.v  = peps0(i+0).v
                         mu00.v   = pmu0(i+0).v
                         E0.re    = pE(i+0).re
                         E0.im    = pE(i+0).im
                         k0a0.v   = pk0a(i+0).v
                         k00.v    = pk0(i+0).v
                         pIz(i+0) = Iz_f4127_xmm4r4(eps00,mu00,E0,k0a0,k00)
                     end do
             end subroutine Iz_f4127_xmm4r4_rolled
             
             
              ! /*
              !          Approximation for upper-middle and high-frequency region
              !          (k0a > 2).
              !          Bistatic creeping wave approximation for resonance region
              !          (0<<phi<pi/2, k0a > 2)
              !          Electric-field.
              !      */
              
              
              pure function EO_f4129_xmm4r4(phi2,a,r,k0,k0a,E) result(EO)
                  
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: EO_f4129_xmm4r4
                   !dir$ attributes forceinline :: EO_f4129_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: EO_f4129_xmm4r4
                   use mod_vecconsts, only :  v4r4_0, v4r4_1
                   type(XMM4r4_t),  intent(in) :: phi2
                   type(XMM4r4_t),  intent(in) :: a
                   type(XMM4r4_t),  intent(in) :: r
                   type(XMM4r4_t),  intent(in) :: k0
                   type(XMM4r4_t),  intent(in) :: k0a
                   type(  XMM4c4_t),    intent(in) :: E
                   type(  XMM4c4_t) :: E0
                   ! Locals
                     !dir$ attributes align : 16 :: C0375
                     !dir$ attributes align : 16 :: c01171875
                     !dir$ attributes align : 16 :: C40
                     !dir$ attributes align : 16 :: C80
                     !dir$ attributes align : 16 :: C330
                     !dir$ attributes align : 16 :: C50
                     !dir$ attributes align : 16 :: C10
                     !dir$ attributes align : 16 :: tc0
                     !dir$ attributes align : 16 :: tc1
                     !dir$ attributes align : 16 :: ce
                     !dir$ attributes align : 16 :: ex
                     !dir$ attributes align : 16 :: t0
                     !dir$ attributes align : 16 :: t1
                     !dir$ attributes align : 16 :: t2
                     !dir$ attributes align : 16 :: cosf2
                     !dir$ attributes align : 16 :: t3
                     !dir$ attributes align : 16 :: t4
                     !dir$ attributes align : 16 :: t5
                     !dir$ attributes align : 16 :: k0a2
                     !dir$ attributes align : 16 :: cos4f2
                     !dir$ attributes align : 16 :: k0as
                     !dir$ attributes align : 16 :: fac
                     !dir$ attributes align : 16 :: a2
                     !dir$ attributes align : 16 :: cos2f2
                     !dir$ attributes align : 16 :: cos4f2
                     !dir$ attributes align : 16 :: r2
                     !dir$ attributes align : 16 :: ear
                   type(XMM4r4_t), parameter :: C0375     = XMM4r4_t(0.375_sp)
                   type(XMM4r4_t), parameter :: C01171875 = XMM4r4_t(0.1171875_sp)
                   type(XMM4r4_t), parameter :: C40       = XMM4r4_t(4.0_sp)
                   type(XMM4r4_t), parameter :: C80       = XMM4r4_t(8.0_sp)
                   type(XMM4r4_t), parameter :: C330      = XMM4r4_t(33.0_sp)
                   type(XMM4r4_t), parameter :: C50       = XMM4r4_t(5.0_sp)
                   type(XMM4r4_t), parameter :: C10       = XMM4r4_t(1.0_sp)
                   type(  XMM4c4_t),   automatic :: tc0,tc1
                   type(  XMM4c4_t),   automatic :: ce,ex
                   type(  XMM4c4_t),   automatic :: tc2
                   type(XMM4r4_t), automatic :: t0,t1,t2,cosf2
                   type(XMM4r4_t), automatic :: t3,t4,t5,k0a2
                   type(XMM4r4_t), automatic :: cos4f2,k0as,fac,a2
                   type(XMM4r4_t), automatic :: cos2f2,cos4f2,r2,ear
                   cosf2.v  = cos(phi2.v)
                   k0a2.v   = k0a.v+k0a.v
                   r2.v     = r.v*r.v
                   a2.v     = a.v*a.v
                   cos2f2.v = cosf2.v*cosf2.v
                   k0as.v   = k0a.v*k0a.v
                   cos4f2.v = cos2f2.v*cos2f2.v
                   t0.v     = a.v*cosf2.v
                   t1.v     = t0.v/r2.v
                   ear.v    = k0.v*r.v-(a2.v*cosf2.v)
                   fac.v    = sqrt(t1.v)
                   tc0.re   =  v4r4_0.v
                   tc0.im   = ear.v
                   ce       = cexp_xmm4c4(tc0)
                   t3.v     =  v4r4_1.v/cos2f2.v
                   tc1      = tc0*ce
                   t3.v     = t3.v*C0375.v
                   t0.v     = C40.v*k0as.v*cos2f2.v
                   t4.v     =  v4r4_1.v/t0.v
                   t1.v     = C80.v*cos2f2.v
                   t2.v     = C01171875.v*(C330.v/t1.v)
                   t0.v     = C70.v/cos4f2.v
                   !tc2.re   =  v4r4_0.v
                   tc2.im   =  v4r4_1.v/(k0a2.v*cosf2.v)
                   tc2.re   = C10.v
                   tc2.im   = C10.v+tc2.im
                   ex       = E*tc1
                   t5.v     = t4.v*(t2.v+t0.v)
                   tc2      = tc2+t5
                   E0       = ex*tc2
              end function EO_f4129_xmm4r4
              
              
              ! /*
              !         Approximation for upper-middle and high-frequency region
              !          (k0a > 2).
              !          Bistatic creeping wave approximation for resonance region
              !          (0<<phi<pi/2, k0a > 2)
              !          Magnetic-field.
              !      */
              
               pure function HO_f4131_xmm4r4(phi2,a,r,k0,k0a,H) result(HO)
                  
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: HO_f4131_xmm4r4
                   !dir$ attributes forceinline :: HO_f4131_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: HO_f4131_xmm4r4
                   use mod_vecconsts, only :  v4r4_0, v4r4_1
                   type(XMM4r4_t),  intent(in) :: phi2
                   type(XMM4r4_t),  intent(in) :: a
                   type(XMM4r4_t),  intent(in) :: r
                   type(XMM4r4_t),  intent(in) :: k0
                   type(XMM4r4_t),  intent(in) :: k0a
                   type(  XMM4c4_t),    intent(in) :: H
                   type(  XMM4c4_t) :: H0
                   ! Locals
                     !dir$ attributes align : 16 :: C0375
                     !dir$ attributes align : 16 :: c01171875
                     !dir$ attributes align : 16 :: C40
                     !dir$ attributes align : 16 :: C80
                     !dir$ attributes align : 16 :: C330
                     !dir$ attributes align : 16 :: C50
                     !dir$ attributes align : 16 :: C10
                     !dir$ attributes align : 16 :: tc0
                     !dir$ attributes align : 16 :: tc1
                     !dir$ attributes align : 16 :: ce
                     !dir$ attributes align : 16 :: hx
                     !dir$ attributes align : 16 :: t0
                     !dir$ attributes align : 16 :: t1
                     !dir$ attributes align : 16 :: t2
                     !dir$ attributes align : 16 :: cosf2
                     !dir$ attributes align : 16 :: t3
                     !dir$ attributes align : 16 :: t4
                     !dir$ attributes align : 16 :: t5
                     !dir$ attributes align : 16 :: k0a2
                     !dir$ attributes align : 16 :: cos4f2
                     !dir$ attributes align : 16 :: k0as
                     !dir$ attributes align : 16 :: fac
                     !dir$ attributes align : 16 :: a2
                     !dir$ attributes align : 16 :: cos2f2
                     !dir$ attributes align : 16 :: cos4f2
                     !dir$ attributes align : 16 :: r2
                     !dir$ attributes align : 16 :: ear
                   type(XMM4r4_t), parameter :: C0375     = XMM4r4_t(0.375_sp)
                   type(XMM4r4_t), parameter :: C01171875 = XMM4r4_t(0.1171875_sp)
                   type(XMM4r4_t), parameter :: C40       = XMM4r4_t(4.0_sp)
                   type(XMM4r4_t), parameter :: C80       = XMM4r4_t(8.0_sp)
                   type(XMM4r4_t), parameter :: C330      = XMM4r4_t(33.0_sp)
                   type(XMM4r4_t), parameter :: C50       = XMM4r4_t(5.0_sp)
                   type(XMM4r4_t), parameter :: C10       = XMM4r4_t(1.0_sp)
                   type(  XMM4c4_t),   automatic :: tc0,tc1
                   type(  XMM4c4_t),   automatic :: ce,hx
                   type(  XMM4c4_t),   automatic :: tc2
                   type(XMM4r4_t), automatic :: t0,t1,t2,cosf2
                   type(XMM4r4_t), automatic :: t3,t4,t5,k0a2
                   type(XMM4r4_t), automatic :: cos4f2,k0as,fac,a2
                   type(XMM4r4_t), automatic :: cos2f2,cos4f2,r2,ear
                   cosf2.v  = cos(phi2.v)
                   k0a2.v   = k0a.v+k0a.v
                   r2.v     = r.v*r.v
                   a2.v     = a.v*a.v
                   cos2f2.v = cosf2.v*cosf2.v
                   k0as.v   = k0a.v*k0a.v
                   cos4f2.v = cos2f2.v*cos2f2.v
                   t0.v     = a.v*cosf2.v
                   t1.v     = t0.v/r2.v
                   ear.v    = k0.v*r.v-(a2.v*cosf2.v)
                   fac.v    = sqrt(t1.v)
                   tc0.re   =  v4r4_0.v
                   tc0.im   = ear.v
                   ce       = cexp_xmm4c4(tc0)
                   t3.v     =  v4r4_1.v/cos2f2.v
                   tc1      = tc0*ce
                   t3.v     = t3.v*C0375.v
                   t0.v     = C40.v*k0as.v*cos2f2.v
                   t4.v     =  v4r4_1.v/t0.v
                   t1.v     = C80.v*cos2f2.v
                   t2.v     = C01171875.v*(C330.v/t1.v)
                   t0.v     = C70.v/cos4f2.v
                   !tc2.re   =  v4r4_0.v
                   tc2.im   =  v4r4_1.v/(k0a2.v*cosf2.v)
                   tc2.re   = C10.v
                   tc2.im   = C10.v-tc2.im
                   hx       = H*tc1
                   t5.v     = t4.v*(t2.v+t0.v)
                   tc2      = tc2+t5
                   H0       = hx*tc2
              end function HO_f4131_xmm4r4
              
              
               !/*
               !         Approximation for upper-middle and high-frequency region
               !         (k0a > 2).
               !         Bistatic creeping wave approximation for resonance region
               !         (0<<phi<pi/2, k0a > 2)
               !         Electric-field.
               !         Formula 4.1-30
               !     */
               
               
               pure function EC_f4130_xmm4r4(phi,a,r,k0,k0a,E) result(EC)
               
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: EC_f4130_xmm4r4
                   !dir$ attributes forceinline :: EC_f4130_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: EC_f4130_xmm4r4
                   use mod_vecconsts, only :  v4r4_0, v4r4_1, v4r4_pi
                   type(XMM4r4_t),  intent(in) :: phi
                   type(XMM4r4_t),  intent(in) :: a
                   type(XMM4r4_t),  intent(in) :: r
                   type(XMM4r4_t),  intent(in) :: k0
                   type(XMM4r4_t),  intent(in) :: k0a
                   type(  XMM4c4_t),    intent(in) :: E
                   type(  XMM4c4_t) :: EC
                   ! Locals
                     !dir$ attributes align : 16 :: C09358135i1607129
                     !dir$ attributes align : 16 :: C0057397i00994145
                     !dir$ attributes align : 16 :: C0261799387799149436538553615273 
                     !dir$ attributes align : 16 :: C0166666666666666666666666666667
                     !dir$ attributes align : 16 :: C0666666666666666666666666666667
                     !dir$ attributes align : 16 :: C1333333333333333333333333333333
                     !dir$ attributes align : 16 :: C091072
                     !dir$ attributes align : 16 :: e1a
                     !dir$ attributes align : 16 :: ce1
                     !dir$ attributes align : 16 :: e2a
                     !dir$ attributes align : 16 :: e3a
                     !dir$ attributes align : 16 :: ce2
                     !dir$ attributes align : 16 :: ce3
                     !dir$ attributes align : 16 :: tmp2
                     !dir$ attributes align : 16 :: tmp3
                     !dir$ attributes align : 16 :: Et
                     !dir$ attributes align : 16 :: tmp1
                     !dir$ attributes align : 16 :: tc0
                     !dir$ attributes align : 16 :: tc1
                     !dir$ attributes align : 16 :: k0ai16
                     !dir$ attributes align : 16 :: k0apaphi
                     !dir$ attributes align : 16 :: k0apsphi
                     !dir$ attributes align : 16 :: k0rp12
                     !dir$ attributes align : 16 :: k0an23
                     !dir$ attributes align : 16 :: k0an43
                     !dir$ attributes align : 16 :: t0
                     !dir$ attributes align : 16 :: sqr
                     !dir$ attributes align : 16 :: t1
                   type(  XMM4c4_t),   parameter :: C09358135i1607129 =   XMM4c4_t(0.9358135_sp,1.607129_sp)        
                   type(  XMM4c4_t),   parameter :: C0057397i00994145 =   XMM4c4_t(0.057397_sp,0.0994145_sp)
                   type(XMM4r4_t), parameter :: C0261799387799149436538553615273 = &
                                                    XMM4r4_t(0.261799387799149436538553615273_sp)
                   type(XMM4r4_t), parameter :: C0166666666666666666666666666667 = &
                                                    XMM4r4_t(0.166666666666666666666666666667_sp)
                   type(XMM4r4_t), parameter :: C0666666666666666666666666666667 = &
                                                    XMM4r4_t(0.666666666666666666666666666667_sp)
                   type(XMM4r4_t), parameter :: C1333333333333333333333333333333 = &
                                                    XMM4r4_t(1.333333333333333333333333333333_sp)
                   type(XMM4r4_t), parameter :: C0910721 = XMM4r4_t(0.910721_sp)
                   type(  XMM4c4_t),   automatic :: e1a,ce1
                   type(  XMM4c4_t),   automatic :: e2a,e3a                   
                   type(  XMM4c4_t),   automatic :: ce2,ce3
                   type(  XMM4c4_t),   automatic :: tmp2,tmp3
                   type(  XMM4c4_t),   automatic :: Et,tmp1
                   type(  XMM4c4_t),   automatic :: tc0,tc1
                   type(XMM4r4_t), automatic :: k0ai16,k0apaphi,k0apsphi,k0rp12
                   type(XMM4r4_t), automatic :: k0an23,k0an43,t0,sqr,t1
                   k0ai16.v   = k0a.v*C0166666666666666666666666666667.v
                   k0apaphi.v = k0a.v* v4r4_pi.v+phi.v
                   e2a.re     =  v4r4_0.v
                   e2a.im     = k0apaphi.v
                   t0.v       = k0ai16.v
                   ce2        = cexp_xmm4c4(e2a)
                   k0ai16.v   =  v4r4_1.v/t0.v
                   k0apsphi.v = k0a.v* v4r4_pi.v-phi.v
                   k0rp12.v   = k0.v*r.v+C0261799387799149436538553615273.v
                   e3a.re     =  v4r4_0.v
                   e3a.im     = k0apsphi.v
                   ce3        = cexp_xmm4c4(e3a)
                   t0.v       = a.v/(r.v*r.v)
                   sqr.v      = sqrt(t0.v)
                   Et.re      = E.re*sqr.v
                   Et.im      = E.im*sqr.v
                   t0.v       = k0a.v*C0666666666666666666666666666667.v
                   t1.v       = k0a.v*C1333333333333333333333333333333.v
                   e1a.re     =  v4r4_0.v
                   k0an23.v   =  v4r4_1.v/t0.v
                   e1a.im     = k0rp12
                   k0an43.v   =  v4r4_1.v/t1.v
                   ce1        = cexp_xmm4c4(e1a)
                   tc0        = C09358135i1607129* v4r4_1+k0an23
                   tc1        = C0057397i00994145*k0an43
                   tmp1       = tc0-tc1
                   tmp2       = ce2*tmp1
                   tmp3       = ce3*tmp1
                   tc0        = Et*tmp2
                   EC         = tc0+tmp3
               end function EC_f4130_xmm4r4
               
               
               !  /*
               !         Approximation for upper-middle and high-frequency region
               !         (k0a > 2).
               !         Bistatic creeping wave approximation for resonance region
               !         valid only for (0<<phi<pi/2, k0a > 2)
               !         Magnetic-field.
               !         Formula 4.1-32
               !     */
               
               
                pure function HC_f4132_xmm4r4(phi,a,r,k0,k0a,H) result(HC)
               
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: HC_f4132_xmm4r4
                   !dir$ attributes forceinline :: HC_f4132_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: HC_f4132_xmm4r4
                   use mod_vecconsts, only :  v4r4_0, v4r4_1, v4r4_pi
                   type(XMM4r4_t),  intent(in) :: phi
                   type(XMM4r4_t),  intent(in) :: a
                   type(XMM4r4_t),  intent(in) :: r
                   type(XMM4r4_t),  intent(in) :: k0
                   type(XMM4r4_t),  intent(in) :: k0a
                   type(  XMM4c4_t),    intent(in) :: H
                   type(  XMM4c4_t) :: HC
                   ! Locals
                     !dir$ attributes align : 16 :: C0404308i070028  
                     !dir$ attributes align : 16 :: C0072732i01259755
                     !dir$ attributes align : 16 :: C0261799387799149436538553615273
                     !dir$ attributes align : 16 :: C0166666666666666666666666666667
                     !dir$ attributes align : 16 :: C0666666666666666666666666666667
                     !dir$ attributes align : 16 :: C1333333333333333333333333333333
                     !dir$ attributes align : 16 :: C0910721
                     !dir$ attributes align : 16 :: e1a
                     !dir$ attributes align : 16 :: ce1
                     !dir$ attributes align : 16 :: e2a
                     !dir$ attributes align : 16 :: e3a
                     !dir$ attributes align : 16 :: ce2
                     !dir$ attributes align : 16 :: ce3
                     !dir$ attributes align : 16 :: Ht
                     !dir$ attributes align : 16 :: tmp1
                     !dir$ attributes align : 16 :: tc0
                     !dir$ attributes align : 16 :: tc1
                     !dir$ attributes align : 16 :: k0ai16
                     !dir$ attributes align : 16 :: k0apaphi
                     !dir$ attributes align : 16 :: k0apsphi
                     !dir$ attributes align : 16 :: k0rp12
                     !dir$ attributes align : 16 :: k0an23
                     !dir$ attributes align : 16 :: k0an43
                     !dir$ attributes align : 16 :: t0
                     !dir$ attributes align : 16 :: sqr
                     !dir$ attributes align : 16 :: t1
                   type(  XMM4c4_t),   parameter :: C0404308i070028   =   XMM4c4_t(0.404308_sp,0.70028_sp)        
                   type(  XMM4c4_t),   parameter :: C0072732i01259755 =   XMM4c4_t(0.072732_sp,-0.1259755_sp)
                   type(XMM4r4_t), parameter :: C0261799387799149436538553615273 = &
                                                    XMM4r4_t(0.261799387799149436538553615273_sp)
                   type(XMM4r4_t), parameter :: C0166666666666666666666666666667 = &
                                                    XMM4r4_t(0.166666666666666666666666666667_sp)
                   type(XMM4r4_t), parameter :: C0666666666666666666666666666667 = &
                                                    XMM4r4_t(0.666666666666666666666666666667_sp)
                   type(XMM4r4_t), parameter :: C1333333333333333333333333333333 = &
                                                    XMM4r4_t(1.333333333333333333333333333333_sp)
                   type(XMM4r4_t), parameter :: C0910721 = XMM4r4_t(0.910721_sp)
                   type(  XMM4c4_t),   automatic :: e1a,ce1
                   type(  XMM4c4_t),   automatic :: e2a,e3a                   
                   type(  XMM4c4_t),   automatic :: ce2,ce3
                   type(  XMM4c4_t),   automatic :: tmp2,tmp3
                   type(  XMM4c4_t),   automatic :: Ht,tmp1
                   type(  XMM4c4_t),   automatic :: tc0,tc1
                   type(XMM4r4_t), automatic :: k0ai16,k0apaphi,k0apsphi,k0rp12
                   type(XMM4r4_t), automatic :: k0an23,k0an43,t0,sqr,t1
                   k0ai16.v   = k0a.v*C0166666666666666666666666666667.v
                   k0apaphi.v = k0a.v* v4r4_pi.v+phi.v
                   e2a.re     =  v4r4_0.v
                   e2a.im     = k0apaphi.v
                   t0.v       = k0ai16.v
                   ce2        = cexp_xmm4c4(e2a)
                   k0ai16.v   =  v4r4_1.v/t0.v
                   k0apsphi.v = k0a.v* v4r4_pi.v-phi.v
                   k0rp12.v   = k0.v*r.v+C0261799387799149436538553615273.v
                   e3a.re     =  v4r4_0.v
                   e3a.im     = k0apsphi.v
                   ce3        = cexp_xmm4c4(e3a)
                   t0.v       = a.v/(r.v*r.v)
                   sqr.v      = sqrt(t0.v)
                   Ht.re      = H.re*sqr.v
                   Ht.im      = H.im*sqr.v
                   t0.v       = k0a.v*C0666666666666666666666666666667.v
                   t1.v       = k0a.v*C1333333333333333333333333333333.v
                   e1a.re     =  v4r4_0.v
                   k0an23.v   =  v4r4_1.v/t0.v
                   e1a.im     = k0rp12
                   k0an43.v   =  v4r4_1.v/t1.v
                   ce1        = cexp_xmm4c4(e1a)
                   tc0        = C0404308i070028* v4r4_1+k0an23
                   tc1        = C0072732i01259755*k0an43
                   tmp1       = tc0-tc1
                   tmp2       = ce2*tmp1
                   tmp3       = ce3*tmp1
                   tc0        = Ht*tmp2
                   HC         = tc0+tmp3
               end function HC_f4132_xmm4r4
               
               
                ! /*
                ! !
                !       Backscattering creeping-wave approximation for resonance region
                !       (phi == 0, k0a > 2).
                !       Optical wave component e-field, formula 4.1-33
                !   */
                
                
                pure function EO_f4133_xmm4r4(E,a,r,k0,k0a) result(EO)
                     
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: EO_f4133_xmm4r4
                   !dir$ attributes forceinline :: EO_f4133_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: EO_f4133_xmm4r4
                   use mod_vecconsts, only :  v4r4_0, v4r4_1
                   type(  XMM4c4_t),   intent(in) :: E
                   type(XMM4r4_t), intent(in) :: a
                   type(XMM4r4_t), intent(in) :: r
                   type(XMM4r4_t), intent(in) :: k0
                   type(XMM4r4_t), intent(in) :: k0a
                   type(  XMM4c4_t) :: EO
                   ! Locals
                     !dir$ attributes align : 16 :: C160
                     !dir$ attributes align : 16 :: C50
                     !dir$ attributes align : 16 :: C1270
                     !dir$ attributes align : 16 :: C5120
                     !dir$ attributes align : 16 :: ea
                     !dir$ attributes align : 16 :: ce
                     !dir$ attributes align : 16 :: fac
                     !dir$ attributes align : 16 :: tc0
                     !dir$ attributes align : 16 :: ce1
                     !dir$ attributes align : 16 :: r2
                     !dir$ attributes align : 16 :: k0a2
                     !dir$ attributes align : 16 :: k0r
                     !dir$ attributes align : 16 :: k0as
                     !dir$ attributes align : 16 :: t0
                     !dir$ attributes align : 16 :: t1
                     !dir$ attributes align : 16 :: t2
                     !dir$ attributes align : 16 :: t3
                   type(XMM4r4_t),  parameter :: C160 = XMM4r4_t(16.0_sp)
                   type(XMM4r4_t),  parameter :: C50  = XMM4r4_t(5.0_sp)
                   type(XMM4r4_t),  parameter :: C1270= XMM4r4_t(127.0_sp)
                   type(XMM4r4_t),  parameter :: C5120= XMM4r4_t(512.0_sp)
                   type(  XMM4c4_t),    automatic :: ea,ce,fac
                   type(  XMM4c4_t),    automatic :: tc0,ce1
                   type(XMM4r4_t),  automatic :: r2,k0a2,k0r,k0as
                   type(XMM4r4_t),  automatic :: t0,t1,t2,t3
                   r2.v   = r.v+r.v
                   k0a2.v = k0a.v+k0a.v
                   t1.v   = a.v/r2.v
                   k0r.v  = k0.v*r.v
                   ea.im  =  v4r4_0.v
                   t0.v   = k0r.v-k0a2.v
                   ea.re  = t0.v
                   t2.v   = sqrt(t1.v)
                   k0as.v = k0a.v*k0a.v
                   fac    = E*t2
                   ce     = cexp_xmm4c4(ea)
                   t1.v   = C160.v*k0a.v
                   t2.v   = C5120.v*k0as.v
                   tc0.re = C50.v/t1.v
                   tc0.im =  v4r4_0.v
                   t3.v   = C1270.v/t2.v
                   tc0.re = C10.v+tc0.re
                   tc0.re = t3.v+tc0.re
                   ce1    = fac*ce
                   EO     = ce1*tc0
               end function EO_f4133_xmm4r4
               
              
              !  /*
              !!
              !         Backscattering creeping-wave approximation for resonance region
              !         (phi == 0, k0a > 2).
              !         Optical wave component h-field, formula 4.1-35
              !     */
                                             
              
              pure function HO_f4135_xmm4r4(H,a,r,k0,k0a) result(HO)
                   
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: HO_f4135_xmm4r4
                   !dir$ attributes forceinline :: HO_f4135_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: HO_f4135_xmm4r4
                   use mod_vecconsts, only :  v4r4_0, v4r4_1
                   type(  XMM4c4_t),   intent(in) :: H
                   type(XMM4r4_t), intent(in) :: a
                   type(XMM4r4_t), intent(in) :: r
                   type(XMM4r4_t), intent(in) :: k0
                   type(XMM4r4_t), intent(in) :: k0a
                   type(  XMM4c4_t) :: HO
                   !Locals
                     !dir$ attributes align : 16 :: C160
                     !dir$ attributes align : 16 :: C110
                     !dir$ attributes align : 16 :: C3530
                     !dir$ attributes align : 16 :: C5120
                     !dir$ attributes align : 16 :: ea
                     !dir$ attributes align : 16 :: ce
                     !dir$ attributes align : 16 :: fac
                     !dir$ attributes align : 16 :: tc0
                     !dir$ attributes align : 16 :: ce1
                     !dir$ attributes align : 16 :: r2
                     !dir$ attributes align : 16 :: k0a2
                     !dir$ attributes align : 16 :: k0r
                     !dir$ attributes align : 16 :: k0as
                     !dir$ attributes align : 16 :: t0
                     !dir$ attributes align : 16 :: t1
                     !dir$ attributes align : 16 :: t2
                     !dir$ attributes align : 16 :: t3
                   type(XMM4r4_t),  parameter :: C160 = XMM4r4_t(16.0_sp)
                   type(XMM4r4_t),  parameter :: C110 = XMM4r4_t(11.0_sp)
                   type(XMM4r4_t),  parameter :: C3530= XMM4r4_t(353.0_sp)
                   type(XMM4r4_t),  parameter :: C5120= XMM4r4_t(512.0_sp)
                   type(  XMM4c4_t),    automatic :: ea,ce,fac
                   type(  XMM4c4_t),    automatic :: tc0,ce1
                   type(XMM4r4_t),  automatic :: r2,k0a2,k0r,k0as
                   type(XMM4r4_t),  automatic :: t0,t1,t2,t3
                   r2.v   = r.v+r.v
                   k0a2.v = k0a.v+k0a.v
                   t1.v   = a.v/r2.v
                   k0r.v  = k0.v*r.v
                   ea.im  =  v4r4_0.v
                   t0.v   = k0r.v-k0a2.v
                   ea.re  = t0.v
                   t2.v   = sqrt(t1.v)
                   k0as.v = k0a.v*k0a.v
                   fac    = H*t2
                   ce     = cexp_xmm4c4(ea)
                   t1.v   = C160.v*k0a.v
                   t2.v   = C5120.v*k0as.v
                   tc0.re = C110.v/t1.v
                   tc0.im =  v4r4_0.v
                   t3.v   = C3530.v/t2.v
                   tc0.re =  v4r4_1.v-tc0.re
                   tc0.re = t3.v+tc0.re
                   ce1    = fac*ce
                   HO     = ce1*tc0
              end function HO_f4135_xmm4r4
             
             
              !  /*
              !!
              !         Backscattering creeping-wave approximation for resonance region
              !         (phi == 0, k0a > 2).
              !         Creeping wave component e-field, formula 4.1-34
              !     */
              
              
              pure function EC_f4134_xmm4r4(E,a,r,k0) result(EC)
                   
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: EC_f4134_xmm4r4
                   !dir$ attributes forceinline :: EC_f4134_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: EC_f4134_xmm4r4 
                   use mod_vecconsts, only :  v4r4_0, v4r4_1
                   type(  XMM4c4_t),   intent(in) :: E
                   type(XMM4r4_t), intent(in) :: a
                   type(XMM4r4_t), intent(in) :: r
                   type(XMM4r4_t), intent(in) :: k0
                   type(  XMM4c4_t) :: EC
                   ! Locals
                     !dir$ attributes align : 16 :: C314159265358979323846264338328 
                     !dir$ attributes align : 16 :: C0261799387799149436538553615273
                     !dir$ attributes align : 16 :: C2939945
                     !dir$ attributes align : 16 :: C0180318
                     !dir$ attributes align : 16 :: C0333333333333333333333333333333333333
                     !dir$ attributes align : 16 :: C1821442
                     !dir$ attributes align : 16 :: C5048945 
                     !dir$ attributes align : 16 :: C0312320
                     !dir$ attributes align : 16 :: C0166666666666666666666666666667 
                     !dir$ attributes align : 16 :: tc0
                     !dir$ attributes align : 16 :: e1a
                     !dir$ attributes align : 16 :: ce1
                     !dir$ attributes align : 16 :: frac
                     !dir$ attributes align : 16 :: k0r
                     !dir$ attributes align : 16 :: k0a
                     !dir$ attributes align : 16 :: k0a13
                     !dir$ attributes align : 16 :: k0an13
                     !dir$ attributes align : 16 :: k0an16
                     !dir$ attributes align : 16 :: r2
                     !dir$ attributes align : 16 :: t0
                     !dir$ attributes align : 16 :: t1
                     !dir$ attributes align : 16 :: exar
                     !dir$ attributes align : 16 :: rex
                   type(XMM4r4_t), parameter :: C314159265358979323846264338328  = &
                                                     XMM4r4_t(3.14159265358979323846264338328_sp)
                   type(XMM4r4_t), parameter :: C0261799387799149436538553615273 = &
                                                     XMM4r4_t(0.261799387799149436538553615273_sp)
                   type(XMM4r4_t), parameter :: C2939945 = XMM4r4_t(2.939945_sp)
                   type(XMM4r4_t), parameter :: C0180318 = XMM4r4_t(0.180318_sp)
                   type(XMM4r4_t), parameter :: C0333333333333333333333333333333333333 = &
                                                     XMM4r4_t(0.333333333333333333333333333333333333_sp)
                   type(XMM4r4_t), parameter :: C1821442 = XMM4r4_t(1.821442_sp)
                   type(XMM4r4_t), parameter :: C5048945 = XMM4r4_t(-5.048945_sp)
                   type(XMM4r4_t), parameter :: C0312320 = XMM4r4_t(0.312320_sp)
                   type(XMM4r4_t), parameter :: C0166666666666666666666666666667  = &
                                                     XMM4r4_t(0.166666666666666666666666666667_sp)
                   type(  XMM4c4_t),   automatic :: tc0,e1a
                   type(  XMM4c4_t),   automatic :: ce1,frac
                   type(XMM4r4_t), automatic :: k0r,k0a
                   type(XMM4r4_t), automatic :: k0a13,k0an13
                   type(XMM4r4_t), automatic :: k0an16,r2
                   type(XMM4r4_t), automatic :: t0,t1
                   type(XMM4r4_t), automatic :: exar,rex   
                   k0r.v   = k0.v*r.v
                   k0a.v   = k0.v*a.v
                   k0a13.v = k0a.v**C0333333333333333333333333333333333333.v
                   r2.v    = r.v+r.v
                   t1.v    = k0a.v**C0166666666666666666666666666667.v
                   t0.v    = a.v/r2.v
                   k0an16.v=  v4r4_1.v/t1.v
                   t2.v    = sqrt(t0.v)
                   frac    = E*t2
                   t0.v    = C2939945.v*k0a13.v-(C0180318.v*k0an13.v)
                   t1.v    = k0a.v*C314159265358979323846264338328.v+ &
                             (C0261799387799149436538553615273.v+t0.v)
                   e1a.im  =  v4r4_0.v
                   t1.v    = k0r.v+t1.v
                   e1a.re  = t1.v
                   ce1     = cexp_xmm4c4(e1a)
                   exar.v  = C5048945.v*k0a13.v-(C0312320.v*k0an13.v)
                   t1.v    = C1821442.v*k0an16.v
                   t2.v    = exp(exar.v)
                   rex.v   =  v4r4_1.v/t2.v
                   tc0     = frac*ce1 
                   rex.v   = rex.v*t1.v
                   EC      = tc0*rex
              end function EC_f4134_xmm4r4
              
              
              !  /*
              !!
              !         Backscattering creeping-wave approximation for resonance region
              !         (phi == 0, k0a > 2).
              !         Creeping wave component h-field, formula 4.1-36
              !     */
              
              
              pure function HC_f4136_xmm4r4(H,a,r,k0) result(HC)
                   
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: HC_f4136_xmm4r4
                   !dir$ attributes forceinline :: HC_f4136_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: HC_f4136_xmm4r4 
                   use mod_vecconsts, only :  v4r4_0, v4r4_1
                   type(  XMM4c4_t),   intent(in) :: H
                   type(XMM4r4_t), intent(in) :: a
                   type(XMM4r4_t), intent(in) :: r
                   type(XMM4r4_t), intent(in) :: k0
                   type(  XMM4c4_t) :: HC
                   ! Locals
                     !dir$ attributes align : 16 :: C314159265358979323846264338328 
                     !dir$ attributes align : 16 :: C0261799387799149436538553615273
                     !dir$ attributes align : 16 :: C12701695
                     !dir$ attributes align : 16 :: C02284945
                     !dir$ attributes align : 16 :: C0333333333333333333333333333333333333
                     !dir$ attributes align : 16 :: C3063830
                     !dir$ attributes align : 16 :: C2200000 
                     !dir$ attributes align : 16 :: C03957635
                     !dir$ attributes align : 16 :: C0166666666666666666666666666667 
                     !dir$ attributes align : 16 :: tc0
                     !dir$ attributes align : 16 :: e1a
                     !dir$ attributes align : 16 :: ce1
                     !dir$ attributes align : 16 :: frac
                     !dir$ attributes align : 16 :: k0r
                     !dir$ attributes align : 16 :: k0a
                     !dir$ attributes align : 16 :: k0a13
                     !dir$ attributes align : 16 :: k0an13
                     !dir$ attributes align : 16 :: k0an16
                     !dir$ attributes align : 16 :: r2
                     !dir$ attributes align : 16 :: t0
                     !dir$ attributes align : 16 :: t1
                     !dir$ attributes align : 16 :: exar
                     !dir$ attributes align : 16 :: rex
                   type(XMM4r4_t), parameter :: C314159265358979323846264338328  = &
                                                     XMM4r4_t(3.14159265358979323846264338328_sp)
                   type(XMM4r4_t), parameter :: C0261799387799149436538553615273 = &
                                                     XMM4r4_t(0.261799387799149436538553615273_sp)
                   type(XMM4r4_t), parameter :: C12701695 = XMM4r4_t(1.2701695_sp)
                   type(XMM4r4_t), parameter :: C02284945 = XMM4r4_t(0.2284945_sp)
                   type(XMM4r4_t), parameter :: C0333333333333333333333333333333333333 = &
                                                     XMM4r4_t(0.333333333333333333333333333333333333_sp)
                   type(XMM4r4_t), parameter :: C3063830 = XMM4r4_t(3.063830_sp)
                   type(XMM4r4_t), parameter :: C2200000 = XMM4r4_t(-2.200000_sp)
                   type(XMM4r4_t), parameter :: C03957635 = XMM4r4_t(0.3957635_sp)
                   type(XMM4r4_t), parameter :: C0166666666666666666666666666667  = &
                                                     XMM4r4_t(0.166666666666666666666666666667_sp)
                   type(  XMM4c4_t),   automatic :: tc0,e1a
                   type(  XMM4c4_t),   automatic :: ce1,frac
                   type(XMM4r4_t), automatic :: k0r,k0a
                   type(XMM4r4_t), automatic :: k0a13,k0an13
                   type(XMM4r4_t), automatic :: k0an16,r2
                   type(XMM4r4_t), automatic :: t0,t1
                   type(XMM4r4_t), automatic :: exar,rex   
                   k0r.v   = k0.v*r.v
                   k0a.v   = k0.v*a.v
                   k0a13.v = k0a.v**C0333333333333333333333333333333333333.v
                   r2.v    = r.v+r.v
                   t1.v    = k0a.v**C0166666666666666666666666666667.v
                   t0.v    = a.v/r2.v
                   k0an16.v=  v4r4_1.v/t1.v
                   t2.v    = sqrt(t0.v)
                   frac    = H*t2
                   t0.v    = C12701695.v*k0a13.v-(C02284945.v*k0an13.v)
                   t1.v    = k0a.v*C314159265358979323846264338328.v+ &
                             (C0261799387799149436538553615273.v+t0.v)
                   e1a.im  =  v4r4_0.v
                   t1.v    = k0r.v+t1.v
                   e1a.re  = t1.v
                   ce1     = cexp_xmm4c4(e1a)
                   exar.v  = C2200000.v*k0a13.v-(C03957635.v*k0an13.v)
                   t1.v    = C3063830.v*k0an16.v
                   t2.v    = exp(exar.v)
                   rex.v   =  v4r4_1.v/t2.v
                   tc0     = frac*ce1 
                   rex.v   = rex.v*t1.v
                   HC      = tc0*rex
              end function HC_f4136_xmm4r4
              
              
              !  /*
              !          Bistatic scattering width in high frequency limit (k0a > 20)
              !          for |PI-phi| > k0a^0.3
              !          Formula 4.1-37
              !      */
              
              pure function rcs_f4137_xmm4r4(a,phi2) result(rcs)
                   
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4137_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4137_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4137_xmm4r4
                   type(XMM4r4_t), intent(in) :: a
                   type(XMM4r4_t), intent(in) :: phi2
                   type(XMM4r4_t) :: rcs
                   ! Locals
                     !dir$ attributes align : 16 :: C314159265358979323846264338328
                     !dir$ attributes align : 16 :: cosp2
                     !dir$ attributes align : 16 :: t0
                   type(XMM4r4_t), parameter :: C314159265358979323846264338328  = &
                                                     XMM4r4_t(3.14159265358979323846264338328_sp)
                   type(XMM4r4_t), automatic :: cosp2,t0
#if (GMS_EXPLICIT_VECTORIZE) == 1
                    integer(kind=i4) :: j
                    !dir$ loop_count(4)
                    !dir$ vector aligned
                    !dir$ vector vectorlength(4)
                    !dir$ vector always
                    do j=0, 3
                        cosp2.v(j) = cos(phi2.v(j))
                        t0.v(j)    = a.v*cosp2.v(j)
                        rcs.v(j)   = C314159265358979323846264338328.v(j)*t0.v(j)
                    end do
#else
                   cosp2.v = cos(phi2.v)
                   t0.v    = a.v*cosp2.v
                   rcs.v   = C314159265358979323846264338328.v*t0.v
#endif
              end function rcs_f4137_xmm4r4
              
              
              !    /*
              !           Backscattering Width in High-Frequency Limit (k0a > 20)
              !            Formula 4.1-38
              !       */
              
              
              pure function rcs_f4138_xmm4r4(a) result(rcs)
                   
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4138_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4138_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4138_xmm4r4
                   type(XMM4r4_t), intent(in) :: a
                   type(XMM4r4_t) :: rcs
                   ! LOcals
                     !dir$ attributes align : 16 :: C314159265358979323846264338328 
                   type(XMM4r4_t), parameter :: C314159265358979323846264338328  = &
                                                     XMM4r4_t(3.14159265358979323846264338328_sp)
                   rcs.v = a.v*C314159265358979323846264338328.v
              end function rcs_f4138_xmm4r4
              
              
               ! /*
               !          Forward scattering widths and pattern in high-frequency limit
               !          (k0a>20.0)
               !          Formula 4.1-40, RCS.
               !      */
               
               pure function rcs_f4140_xmm4r4(k0a,alpha) result(rcs)
                    
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4140_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4140_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4140_xmm4r4 
                   type(XMM4r4_t),  intent(in) :: k0a
                   type(XMM4r4_t),  intent(in) :: alpha
                   type(XMM4r4_t) :: rcs
                   ! Locals
                      !dir$ attributes align : 16 :: C40
                      !dir$ attributes align : 16 :: sinc
                      !dir$ attributes align : 16 :: k0alp
                      !dir$ attributes align : 16 :: k0as
                      !dir$ attributes align : 16 :: t0
                   type(XMM4r4_t), parameter :: C40 = XMM4r4_t(4.0_sp)
                   type(XMM4r4_t), automatic :: sinc,k0alp
                   type(XMM4r4_t), automatic :: k0as,t0
#if (GMS_EXPLICIT_VECTORIZE) == 1
                    integer(kind=i4) :: j
                    !dir$ loop_count(4)
                    !dir$ vector aligned
                    !dir$ vector vectorlength(4)
                    !dir$ vector always
                    do j=0, 3
                       k0alp.v(j) = k0a.v(j)*alpha.v(j)
                       t0.v(j)    = sin(k0alp.v(j))
                       sinc.v(j)  = t0.v(j)/k0alp.v(j)
                       k0as.v(j)  = C40.v(j)*k0a.v(j)*k0a.v(j)
                       rcs.v(j)   = k0as.v(j)*sinc.v(j)*sinc.v(j)
                    end do
#else
                   k0alp.v = k0a.v*alpha.v
                   t0.v    = sin(k0alp.v)
                   sinc.v  = t0.v/k0alp.v
                   k0as.v  = C40.v*k0a.v*k0a.v
                   rcs.v   = k0as.v*sinc.v*sinc.v
#endif
               end function rcs_f4140_xmm4r4
               
               
               !  /*
               !          Forward scattering widths and pattern in high-frequency limit
               !          (k0a>20.0), forward scattered (diffracted) e-field
               !          Formula 4.1-39.
               !!
               !        */
               
               pure function Es_f4139_xmm4r4(E,r,k0,alp,k0a) result(Es)
                    
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: Es_f4139_xmm4r4
                   !dir$ attributes forceinline :: Es_f4139_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: Es_f4139_xmm4r4
                   use mod_vecconsts, only :  v4r4_0
                   type(  XMM4c4_t),   intent(in) :: E
                   type(XMM4r4_t), intent(in) :: r
                   type(XMM4r4_t), intent(in) :: k0
                   type(XMM4r4_t), intent(in) :: alp
                   type(XMM4r4_t), intent(in) :: k0a
                   type(  XMM4c4_t) :: Es
                   ! Locals
                     !dir$ attributes align : 16 :: C20
                     !dir$ attributes align : 16 :: C078539816339744830961566084582
                     !dir$ attributes align : 16 :: fac
                     !dir$ attributes align : 16 :: ar
                     !dir$ attributes align : 16 :: tc0
                     !dir$ attributes align : 16 :: ce
                     !dir$ attributes align : 16 :: k0as2
                     !dir$ attributes align : 16 :: k0r
                     !dir$ attributes align : 16 :: k0alp
                     !dir$ attributes align : 16 :: sinc
                     !dir$ attributes align : 16 :: div
                     !dir$ attributes align : 16 :: t0
                   type(XMM4r4_t), parameter :: C20 = XMM4r4_t(2.0_sp)
                   type(XMM4r4_t), parameter :: C078539816339744830961566084582 = &
                                                       XMM4r4_t(0.78539816339744830961566084582_sp)
                   type(  XMM4c4_t),   automatic :: fac,ar
                   type(ZMM16C4),   automatic :: tc0,ce
                   type(XMM4r4_t), automatic :: k0as2,k0r,k0alp
                   type(XMM4r4_t), automatic :: sinc,div,t0
                   k0r.v   = k0.v*r.v
                   k0alp.v = k0a.v*alp.v
                   k0as2.v = c20.v+k0a.v*k0a.v
                   div.v   = k0as.v/C078539816339744830961566084582.v
                   ar.im   =  v4r4_0.v
                   t0.v    = sin(k0alp.v)
                   ar.re   = k0r.v-C078539816339744830961566084582.v
                   sinc.v  = t0.v/k0alp.v
                   ce      = cexp_xmm4c4(ar)
                   t0.v    = sqrt(div.v)
                   fac     = E*t0
                   tc0     = ce*sinc
                   Es      = fac*tc0
               end function Es_f4139_xmm4r4
               
               
               !  /*
               !          Forward scattering widths and pattern in high-frequency limit
               !          (k0a>20.0), constant angle (alpha=0)
               !          Formula 4.1-41, RCS.
               !      */
               
               
               pure function rcs_f4141_xmm4r4(k0a) result(rcs)
                    
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4141_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4141_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4141_xmm4r4
                   type(XMM4r4_t),  intent(in) :: k0a
                   type(XMM4r4_t) :: rcs
                   ! Locals
                     !dir$ attributes align : 16 :: C40
                   type(XMM4r4_t), parameter :: C40 = XMM4r4_t(4.0_sp)
                   rcs.v = C40.v*k0a.v*k0a.v 
               end function rcs_f4141_xmm4r4
               
               
               
               !    /*
               !         Approximations for the low frequency region (k0a<<1,k1a<<1)
               !         Scattered far-zone e-field, formula 4.1-45
               !     */
               
               
               pure function Es_f4145_xmm4r4(EI,r,k0,k0a,phi,  &
                                              eps0,eps1,mu0,mu1) result(Es)
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: Es_f4145_xmm4r4
                   !dir$ attributes forceinline :: Es_f4145_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: Es_f4145_xmm4r4
                   use mod_vecconsts, only :  v4r4_0, v4r4_1
                   type(  XMM4c4_t),   intent(in) :: EI
                   type(XMM4r4_t), intent(in) :: r
                   type(XMM4r4_t), intent(in) :: k0
                   type(XMM4r4_t), intent(in) :: k0a
                   type(XMM4r4_t), intent(in) :: phi
                   type(XMM4r4_t), intent(in) :: eps0
                   type(XMM4r4_t), intent(in) :: eps1
                   type(XMM4r4_t), intent(in) :: mu0
                   type(XMM4r4_t), intent(in) :: mu1
                   type(  XMM4c4_t) :: Es
                   !Locals
                     !dir$ attributes align : 16 :: C20
                     !dir$ attributes align : 16 :: C05
                     !dir$ attributes align : 16 :: C078539816339744830961566084582
                     !dir$ attributes align : 16 :: C1253314137315500251207882642406
                     !dir$ attributes align : 16 :: frac
                     !dir$ attributes align : 16 :: ea
                     !dir$ attributes align : 16 :: ce
                     !dir$ attributes align : 16 :: tc0
                     !dir$ attributes align : 16 :: k0r
                     !dir$ attributes align : 16 :: k0as
                     !dir$ attributes align : 16 :: k0as2
                     !dir$ attributes align : 16 :: t0
                     !dir$ attributes align : 16 :: t1
                     !dir$ attributes align : 16 :: cosp
                     !dir$ attributes align : 16 :: t2
                     !dir$ attributes align : 16 :: sk0r
                     !dir$ attributes align : 16 :: t3
                     !dir$ attributes align : 16 :: mul
                   type(XMM4r4_t), parameter :: C20 = XMM4r4_t(2.0_sp)
                   type(XMM4r4_t), parameter :: C05 = XMM4r4_t(0.5_sp)
                   type(XMM4r4_t), parameter :: C078539816339744830961566084582  = &
                                                       XMM4r4_t(0.78539816339744830961566084582_sp)
                   type(XMM4r4_t), parameter :: C1253314137315500251207882642406 = &
                                                       XMM4r4_t(1.253314137315500251207882642406_sp)
                   type(  XMM4c4_t),   automatic :: frac,ea
                   type(  XMM4c4_t),   automatic :: ce,tc0
                   type(XMM4r4_t), automatic :: k0r,k0as,k0as2,t0
                   type(XMM4r4_t), automatic :: t1,cosp,t2,sk0r
                   type(XMM4r4_t), automatic :: t3,mul
                   k0r.v   = k0.v*r.v
                   k0as.v  = k0a.v*k0a.v
                   k0as2.v = C05.v*k0as.v
                   sk0r.v  = sqrt(k0r.v)
                   frac    = EI*C1253314137315500251207882642406.v
                   cosp.v  = cos(phi)
                   ea.re   = k0r.v-C078539816339744830961566084582.v
                   t0.v    = (eps1.v/eps0.v)- v4r4_1.v
                   ea.im   =  v4r4_0.v
                   t1.v    = (mu1.v-mu0.v)/(mu1.v+mu0.v)
                   t2.v    = t1.v+t1.v
                   ce      = cexp_xmm4c4(ea)
                   t1.v    = t2.v*cosp.v
                   ce.re   = ce.re/sk0r.v
                   t3.v    = t0.v-t1.v
                   ce.im   = ce.im/sk0r.v
                   mul.v   = k0as2.v*t3.v
                   tc0     = ce*mul
                   Es      = frac*tc0
               end function Es_f4145_xmm4r4
               
               
               ! /*
               !         Approximations for the low frequency region (k0a<<1,k1a<<1)
               !         Scattered far-zone h-field, formula 4.1-46
               !     */
               
               pure function Hs_f4146_xmm4r4(HI,r,k0,k0a,phi,  &
                                              eps0,eps1,mu0,mu1) result(Hs)
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: Hs_f4146_xmm4r4
                   !dir$ attributes forceinline :: Hs_f4146_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: Hs_f4146_xmm4r4
                   use mod_vecconsts, only :  v4r4_0, v4r4_1
                   type(  XMM4c4_t),   intent(in) :: HI
                   type(XMM4r4_t), intent(in) :: r
                   type(XMM4r4_t), intent(in) :: k0
                   type(XMM4r4_t), intent(in) :: k0a
                   type(XMM4r4_t), intent(in) :: phi
                   type(XMM4r4_t), intent(in) :: eps0
                   type(XMM4r4_t), intent(in) :: eps1
                   type(XMM4r4_t), intent(in) :: mu0
                   type(XMM4r4_t), intent(in) :: mu1
                   type(  XMM4c4_t) :: Hs
                   !Locals
                     !dir$ attributes align : 16 :: C20
                     !dir$ attributes align : 16 :: C05
                     !dir$ attributes align : 16 :: C078539816339744830961566084582
                     !dir$ attributes align : 16 :: C1253314137315500251207882642406
                     !dir$ attributes align : 16 :: frac
                     !dir$ attributes align : 16 :: ea
                     !dir$ attributes align : 16 :: ce
                     !dir$ attributes align : 16 :: tc0
                     !dir$ attributes align : 16 :: k0r
                     !dir$ attributes align : 16 :: k0as
                     !dir$ attributes align : 16 :: k0as2
                     !dir$ attributes align : 16 :: t0
                     !dir$ attributes align : 16 :: t1
                     !dir$ attributes align : 16 :: cosp
                     !dir$ attributes align : 16 :: t2
                     !dir$ attributes align : 16 :: sk0r
                     !dir$ attributes align : 16 :: t3
                     !dir$ attributes align : 16 :: mul
                   type(XMM4r4_t), parameter :: C20 = XMM4r4_t(2.0_sp)
                   type(XMM4r4_t), parameter :: C05 = XMM4r4_t(0.5_sp)
                   type(XMM4r4_t), parameter :: C078539816339744830961566084582  = &
                                                       XMM4r4_t(0.78539816339744830961566084582_sp)
                   type(XMM4r4_t), parameter :: C1253314137315500251207882642406 = &
                                                       XMM4r4_t(1.253314137315500251207882642406_sp)
                   type(  XMM4c4_t),   automatic :: frac,ea
                   type(  XMM4c4_t),   automatic :: ce,tc0
                   type(XMM4r4_t), automatic :: k0r,k0as,k0as2,t0
                   type(XMM4r4_t), automatic :: t1,cosp,t2,sk0r
                   type(XMM4r4_t), automatic :: t3,mul
                   k0r.v   = k0.v*r.v
                   k0as.v  = k0a.v*k0a.v
                   k0as2.v = C05.v*k0as.v
                   sk0r.v  = sqrt(k0r.v)
                   frac    = HI*C1253314137315500251207882642406.v
                   cosp.v  = cos(phi)
                   ea.re   = k0r.v-C078539816339744830961566084582.v
                   t0.v    = (mu1.v/mu0.v)- v4r4_1.v
                   ea.im   =  v4r4_0.v
                   t1.v    = (eps1.v-eps0.v)/(eps1.v+eps0.v)
                   t2.v    = t1.v+t1.v
                   ce      = cexp_xmm4c4(ea)
                   t1.v    = t2.v*cosp.v
                   ce.re   = ce.re/sk0r.v
                   t3.v    = t0.v-t1.v
                   ce.im   = ce.im/sk0r.v
                   mul.v   = k0as2.v*t3.v
                   tc0     = ce*mul
                   Hs      = frac*tc0
               end function Hs_f4146_xmm4r4
               
               
               !/*
               !       Bistatic scattering width (k0a<<1, k1a<<1) at the angle 'phi'
               !       Formula 4.1-47
               !!
               !    */ 
               
               pure function rcs_f4147_xmm4r4(a,k0a,phi,eps1,   &
                                          eps0,mu1,mu0) result(rcs)
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4147_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4147_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4147_xmm4r4
                   use  mod_vecconsts, only :  v4r4_1
                   type(XMM4r4_t),  intent(in) :: a
                   type(XMM4r4_t),  intent(in) :: k0a
                   type(XMM4r4_t),  intent(in) :: phi
                   type(XMM4r4_t),  intent(in) :: eps1
                   type(XMM4r4_t),  intent(in) :: eps0
                   type(XMM4r4_t),  intent(in) :: mu1
                   type(XMM4r4_t),  intent(in) :: mu0
                   type(XMM4r4_t) :: rcs
                   ! Locals
                       !dir$ attributes align : 16 :: C314159265358979323846264338328
                       !dir$ attributes align : 16 :: C078539816339744830961566084582
                       !dir$ attributes align : 16 :: C20
                       !dir$ attributes align : 16 :: t0
                       !dir$ attributes align : 16 :: t1
                       !dir$ attributes align : 16 :: k0a3
                       !dir$ attributes align : 16 :: epst
                       !dir$ attributes align : 16 :: mut
                       !dir$ attributes align : 16 :: cosp
                       !dir$ attributes align : 16 :: sqr
                       !dir$ attributes align : 16 :: t2
                       !dir$ attributes align : 16 :: diff
                   type(XMM4r4_t), parameter :: C314159265358979323846264338328  =
                                                       XMM4r4_t(3.14159265358979323846264338328_sp)
                   type(XMM4r4_t), parameter :: C078539816339744830961566084582  = &
                                                       XMM4r4_t(0.78539816339744830961566084582_sp)
                   type(XMM4r4_t), parameter :: C20 = XMM4r4_t(2.0_sp)
                   type(XMM4r4_t), automatic :: t0,t1,k0a3,epst
                   type(XMM4r4_t), automatic :: mut,cosp,sqr
                   type(XMM4r4_t), automatic :: t2,diff
#if (GMS_EXPLICIT_VECTORIZE) == 1
                    integer(kind=i4) :: j
                    !dir$ loop_count(4)
                    !dir$ vector aligned
                    !dir$ vector vectorlength(4)
                    !dir$ vector always
                    do j=0, 3
                        k0a3.v(j)  = k0a.v(j)*k0a.v(j)*k0a.v(j)
                        cosp.v(j)  = cos(phi.v(j))
                        t0.v(j)    = C078539816339744830961566084582.v(j)* &
                                     C314159265358979323846264338328.v(j)*a.v(j)
                        epst.v(j)  = eps1.v(j)/eps0.v(j)- v4r4_1.v(j)
                        t1.v(j)    = mu1.v(j)-mu0.v(j)
                        t2.v(j)    = mu1.v(j)+mu0.v(j)
                        mut.v(j)   = C20.v(j)*(t1.v(j)/t2.v(j))
                        diff.v(j)  = epst.v(j)-mut.v(j)*cosp.v(j)
                        sqr.v(j)   = diff.v(j)*diff.v(j)
                        rcs.v(j)   = t0.v(j)*k0a3.v(j)*sqr.v(j)
                    end do
#else
                   k0a3.v  = k0a.v*k0a.v*k0a.v
                   cosp.v  = cos(phi.v)
                   t0.v    = C078539816339744830961566084582.v* &
                             C314159265358979323846264338328.v*a.v
                   epst.v  = eps1.v/eps0.v- v4r4_1.v
                   t1.v    = mu1.v-mu0.v
                   t2.v    = mu1.v+mu0.v
                   mut.v   = C20.v*(t1.v/t2.v)
                   diff.v  = epst.v-mut.v*cosp.v
                   sqr.v   = diff.v*diff.v
                   rcs.v   = t0.v*k0a3.v*sqr.v
#endif
               end function rcs_f4147_xmm4r4
               
               
               ! /*
               !       Bistatic scattering width (k0a<<1, k1a<<1) at the angle 'phi'
               !       Formula 4.1-48
               !!
               !    */   
               
               pure function rcs_f4148_xmm4r4(a,k0a,phi,eps1,   &
                                          eps0,mu1,mu0) result(rcs)
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4148_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4148_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4148_xmm4r4
                   use  mod_vecconsts, only :  v4r4_1
                   type(XMM4r4_t),  intent(in) :: a
                   type(XMM4r4_t),  intent(in) :: k0a
                   type(XMM4r4_t),  intent(in) :: phi
                   type(XMM4r4_t),  intent(in) :: eps1
                   type(XMM4r4_t),  intent(in) :: eps0
                   type(XMM4r4_t),  intent(in) :: mu1
                   type(XMM4r4_t),  intent(in) :: mu0
                   type(XMM4r4_t) :: rcs
                   ! Locals
                       !dir$ attributes align : 16 :: C314159265358979323846264338328
                       !dir$ attributes align : 16 :: C078539816339744830961566084582
                       !dir$ attributes align : 16 :: C20
                       !dir$ attributes align : 16 :: t0
                       !dir$ attributes align : 16 :: t1
                       !dir$ attributes align : 16 :: k0a3
                       !dir$ attributes align : 16 :: epst
                       !dir$ attributes align : 16 :: mut
                       !dir$ attributes align : 16 :: cosp
                       !dir$ attributes align : 16 :: sqr
                       !dir$ attributes align : 16 :: t2
                       !dir$ attributes align : 16 :: diff
                   type(XMM4r4_t), parameter :: C314159265358979323846264338328  =
                                                       XMM4r4_t(3.14159265358979323846264338328_sp)
                   type(XMM4r4_t), parameter :: C078539816339744830961566084582  = &
                                                       XMM4r4_t(0.78539816339744830961566084582_sp)
                   type(XMM4r4_t), parameter :: C20 = XMM4r4_t(2.0_sp)
                   type(XMM4r4_t), automatic :: t0,t1,k0a3,epst
                   type(XMM4r4_t), automatic :: mut,cosp,sqr
                   type(XMM4r4_t), automatic :: t2,diff
#if (GMS_EXPLICIT_VECTORIZE) == 1
                    integer(kind=i4) :: j
                    !dir$ loop_count(4)
                    !dir$ vector aligned
                    !dir$ vector vectorlength(4)
                    !dir$ vector always
                    do j=0, 3
                         k0a3.v(j)  = k0a.v(j)*k0a.v(j)*k0a.v(j)
                         cosp.v(j)  = cos(phi.v(j))
                         t0.v(j)    = C078539816339744830961566084582.v(j)* &
                                      C314159265358979323846264338328.v(j)*a.v(j)
                         epst.v(j)  = mu1.v(j)/mu0.v(j)- v4r4_1.v(j)
                         t1.v(j)    = eps1.v(j)-eps0.v(j)
                         t2.v(j)    = eps1.v(j)+eps0.v(j)
                         mut.v(j)   = C20.v(j)*(t1.v(j)/t2.v(j))
                         diff.v(j)  = epst.v(j)-mut.v(j)*cosp.v(j)
                         sqr.v(j)   = diff.v(j)*diff.v(j)
                         rcs.v(j)   = t0.v(j)*k0a3.v(j)*sqr.v(j)
                    end do
#else                   
                   k0a3.v  = k0a.v*k0a.v*k0a.v
                   cosp.v  = cos(phi.v)
                   t0.v    = C078539816339744830961566084582.v* &
                             C314159265358979323846264338328.v*a.v
                   epst.v  = mu1.v/mu0.v- v4r4_1.v
                   t1.v    = eps1.v-eps0.v
                   t2.v    = eps1.v+eps0.v
                   mut.v   = C20.v*(t1.v/t2.v)
                   diff.v  = epst.v-mut.v*cosp.v
                   sqr.v   = diff.v*diff.v
                   rcs.v   = t0.v*k0a3.v*sqr.v
#endif
               end function rcs_f4148_xmm4r4
               
               
               ! /*
               !          Backscattering width (k0a<<1,k1a<<1), when phi = 0
               !          Formula 4.1-49
               !     */ 
               
               pure function rcs_f4149_xmm4r4(a,k0a,eps1,   &
                                          eps0,mu1,mu0) result(rcs)
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4149_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4149_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4149_xmm4r4
                   use  mod_vecconsts, only :  v4r4_1
                   type(XMM4r4_t),  intent(in) :: a
                   type(XMM4r4_t),  intent(in) :: k0a
                   type(XMM4r4_t),  intent(in) :: eps1
                   type(XMM4r4_t),  intent(in) :: eps0
                   type(XMM4r4_t),  intent(in) :: mu1
                   type(XMM4r4_t),  intent(in) :: mu0
                   type(XMM4r4_t) :: rcs
                   ! Locals
                       !dir$ attributes align : 16 :: C314159265358979323846264338328
                       !dir$ attributes align : 16 :: C078539816339744830961566084582
                       !dir$ attributes align : 16 :: C20
                       !dir$ attributes align : 16 :: t0
                       !dir$ attributes align : 16 :: t1
                       !dir$ attributes align : 16 :: k0a3
                       !dir$ attributes align : 16 :: epst
                       !dir$ attributes align : 16 :: mut
                       !dir$ attributes align : 16 :: sqr
                       !dir$ attributes align : 16 :: t2
                       !dir$ attributes align : 16 :: diff
                   type(XMM4r4_t), parameter :: C314159265358979323846264338328  =
                                                       XMM4r4_t(3.14159265358979323846264338328_sp)
                   type(XMM4r4_t), parameter :: C078539816339744830961566084582  = &
                                                       XMM4r4_t(0.78539816339744830961566084582_sp)
                   type(XMM4r4_t), parameter :: C20 = XMM4r4_t(2.0_sp)
                   type(XMM4r4_t), automatic :: t0,t1,k0a3,epst
                   type(XMM4r4_t), automatic :: mut,sqr
                   type(XMM4r4_t), automatic :: t2,diff
#if (GMS_EXPLICIT_VECTORIZE) == 1
                    integer(kind=i4) :: j
                    !dir$ loop_count(4)
                    !dir$ vector aligned
                    !dir$ vector vectorlength(4)
                    !dir$ vector always
                    do j=0, 3
                         k0a3.v(j)  = k0a.v(j)*k0a.v(j)*k0a.v(j)
                         t0.v(j)    = C078539816339744830961566084582.v(j)* &
                                      C314159265358979323846264338328.v(j)*a.v(j)
                         epst.v(j)  = eps1.v(j)/eps0.v(j)- v4r4_1.v(j)
                         t1.v(j)    = mu1.v(j)-mu0.v(j)
                         t2.v(j)    = mu1.v(j)+mu0.v(j)
                         mut.v(j)   = C20.v*(t1.v(j)/t2.v(j))
                         diff.v(j)  = epst.v(j)-mut.v(j)
                         sqr.v(j)   = diff.v(j)*diff.v(j)
                         rcs.v(j)   = t0.v(j)*k0a3.v(j)*sqr.v(j)
                    end do   
#else                
                   k0a3.v  = k0a.v*k0a.v*k0a.v
                   t0.v    = C078539816339744830961566084582.v* &
                             C314159265358979323846264338328.v*a.v
                   epst.v  = eps1.v/eps0.v- v4r4_1.v
                   t1.v    = mu1.v-mu0.v
                   t2.v    = mu1.v+mu0.v
                   mut.v   = C20.v*(t1.v/t2.v)
                   diff.v  = epst.v-mut.v
                   sqr.v   = diff.v*diff.v
                   rcs.v   = t0.v*k0a3.v*sqr.v
#endif
               end function rcs_f4149_xmm4r4
               
               
              !  /*
              !           Backscattering width (k0a<<1,k1a<<1), when phi = 0
              !           Formula 4.1-50
              !      */ 
              
               pure function rcs_f4150_xmm4r4(a,k0a,eps1,   &
                                          eps0,mu1,mu0) result(rcs)
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4150_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4150_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4150_xmm4r4
                   use  mod_vecconsts, only :  v4r4_1
                   type(XMM4r4_t),  intent(in) :: a
                   type(XMM4r4_t),  intent(in) :: k0a
                   type(XMM4r4_t),  intent(in) :: eps1
                   type(XMM4r4_t),  intent(in) :: eps0
                   type(XMM4r4_t),  intent(in) :: mu1
                   type(XMM4r4_t),  intent(in) :: mu0
                   type(XMM4r4_t) :: rcs
                   ! Locals
                       !dir$ attributes align : 16 :: C314159265358979323846264338328
                       !dir$ attributes align : 16 :: C078539816339744830961566084582
                       !dir$ attributes align : 16 :: C20
                       !dir$ attributes align : 16 :: t0
                       !dir$ attributes align : 16 :: t1
                       !dir$ attributes align : 16 :: k0a3
                       !dir$ attributes align : 16 :: epst
                       !dir$ attributes align : 16 :: mut
                       !dir$ attributes align : 16 :: sqr
                       !dir$ attributes align : 16 :: t2
                       !dir$ attributes align : 16 :: diff
                   type(XMM4r4_t), parameter :: C314159265358979323846264338328  =
                                                       XMM4r4_t(3.14159265358979323846264338328_sp)
                   type(XMM4r4_t), parameter :: C078539816339744830961566084582  = &
                                                       XMM4r4_t(0.78539816339744830961566084582_sp)
                   type(XMM4r4_t), parameter :: C20 = XMM4r4_t(2.0_sp)
                   type(XMM4r4_t), automatic :: t0,t1,k0a3,epst
                   type(XMM4r4_t), automatic :: mut,sqr
                   type(XMM4r4_t), automatic :: t2,diff
#if (GMS_EXPLICIT_VECTORIZE) == 1
                    integer(kind=i4) :: j
                    !dir$ loop_count(4)
                    !dir$ vector aligned
                    !dir$ vector vectorlength(4)
                    !dir$ vector always
                    do j=0, 3
                         k0a3.v(j)  = k0a.v(j)*k0a.v(j)*k0a.v(j)
                         t0.v(j)    = C078539816339744830961566084582.v(j)* &
                                      C314159265358979323846264338328.v(j)*a.v(j)
                         epst.v(j)  = mu1.v(j)/mu0.v(j)- v4r4_1.v(j)
                         t1.v(j)    = eps1.v(j)-eps0.v(j)
                         t2.v(j)    = eps1.v(j)+eps0.v(j)
                         mut.v(j)   = C20.v(j)*(t1.v(j)/t2.v(j))
                         diff.v(j)  = epst.v(j)-mut.v(j)
                         sqr.v(j)   = diff.v(j)*diff.v(j)
                         rcs.v(j)   = t0.v(j)*k0a3.v(j)*sqr.v(j)
                   end do    
#else               
                   k0a3.v  = k0a.v*k0a.v*k0a.v
                   t0.v    = C078539816339744830961566084582.v* &
                             C314159265358979323846264338328.v*a.v
                   epst.v  = mu1.v/mu0.v- v4r4_1.v
                   t1.v    = eps1.v-eps0.v
                   t2.v    = eps1.v+eps0.v
                   mut.v   = C20.v*(t1.v/t2.v)
                   diff.v  = epst.v-mut.v
                   sqr.v   = diff.v*diff.v
                   rcs.v   = t0.v*k0a3.v*sqr.v
#endif
               end function rcs_f4150_xmm4r4
               
               
              ! /*
              !           Forward scattering width (k0a<<1, k1a<<1), phi = pi
              !           Formula 4.1-51
              !       */ 
              
                pure function rcs_f4151_xmm4r4(a,k0a,eps1,   &
                                          eps0,mu1,mu0) result(rcs)
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4151_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4151_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4151_xmm4r4
                   use  mod_vecconsts, only :  v4r4_1
                   type(XMM4r4_t),  intent(in) :: a
                   type(XMM4r4_t),  intent(in) :: k0a
                   type(XMM4r4_t),  intent(in) :: eps1
                   type(XMM4r4_t),  intent(in) :: eps0
                   type(XMM4r4_t),  intent(in) :: mu1
                   type(XMM4r4_t),  intent(in) :: mu0
                   type(XMM4r4_t) :: rcs
                   ! Locals
                       !dir$ attributes align : 16 :: C314159265358979323846264338328
                       !dir$ attributes align : 16 :: C078539816339744830961566084582
                       !dir$ attributes align : 16 :: C20
                       !dir$ attributes align : 16 :: t0
                       !dir$ attributes align : 16 :: t1
                       !dir$ attributes align : 16 :: k0a3
                       !dir$ attributes align : 16 :: epst
                       !dir$ attributes align : 16 :: mut 
                       !dir$ attributes align : 16 :: sqr
                       !dir$ attributes align : 16 :: t2
                       !dir$ attributes align : 16 :: diff
                   type(XMM4r4_t), parameter :: C314159265358979323846264338328  =
                                                       XMM4r4_t(3.14159265358979323846264338328_sp)
                   type(XMM4r4_t), parameter :: C078539816339744830961566084582  = &
                                                       XMM4r4_t(0.78539816339744830961566084582_sp)
                   type(XMM4r4_t), parameter :: C20 = XMM4r4_t(2.0_sp)
                   type(XMM4r4_t), automatic :: t0,t1,k0a3,epst
                   type(XMM4r4_t), automatic :: mut,sqr
                   type(XMM4r4_t), automatic :: t2,diff
#if (GMS_EXPLICIT_VECTORIZE) == 1
                    integer(kind=i4) :: j
                    !dir$ loop_count(4)
                    !dir$ vector aligned
                    !dir$ vector vectorlength(4)
                    !dir$ vector always
                    do j=0, 3
                         k0a3.v(j)  = k0a.v(j)*k0a.v(j)*k0a.v(j)
                         t0.v(j)    = C078539816339744830961566084582.v(j)* &
                                      C314159265358979323846264338328.v(j)*a.v(j)
                         epst.v(j)  = eps1.v(j)/eps0.v(j)- v4r4_1.v(j)
                         t1.v(j)    = mu1.v(j)-mu0.v(j)
                         t2.v(j)    = mu1.v(j)+mu0.v(j)
                         mut.v(j)   = C20.v(j)*(t1.v(j)/t2.v(j))
                         diff.v(j)  = epst.v(j)-mut.v(j)
                         sqr.v(j)   = diff.v(j)*diff.v(j)
                         rcs.v(j)   = t0.v(j)*k0a3.v(j)*sqr.v(j)
                    end do
#else
                   k0a3.v  = k0a.v*k0a.v*k0a.v
                   t0.v    = C078539816339744830961566084582.v* &
                             C314159265358979323846264338328.v*a.v
                   epst.v  = eps1.v/eps0.v- v4r4_1.v
                   t1.v    = mu1.v-mu0.v
                   t2.v    = mu1.v+mu0.v
                   mut.v   = C20.v*(t1.v/t2.v)
                   diff.v  = epst.v-mut.v
                   sqr.v   = diff.v*diff.v
                   rcs.v   = t0.v*k0a3.v*sqr.v
#endif
               end function rcs_f4151_xmm4r4
               
               
               ! /*
               !          Forward scattering width (k0a<<1, k1a<<1), phi = pi
               !          Formula 4.1-52
               !      */
              
              
               pure function rcs_f4152_xmm4r4(a,k0a,eps1,   &
                                          eps0,mu1,mu0) result(rcs)
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4152_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4152_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4152_xmm4r4
                   use  mod_vecconsts, only :  v4r4_1
                   type(XMM4r4_t),  intent(in) :: a
                   type(XMM4r4_t),  intent(in) :: k0a
                   type(XMM4r4_t),  intent(in) :: eps1
                   type(XMM4r4_t),  intent(in) :: eps0
                   type(XMM4r4_t),  intent(in) :: mu1
                   type(XMM4r4_t),  intent(in) :: mu0
                   type(XMM4r4_t) :: rcs
                   ! Locals
                       !dir$ attributes align : 16 :: C314159265358979323846264338328
                       !dir$ attributes align : 16 :: C078539816339744830961566084582
                       !dir$ attributes align : 16 :: C20
                       !dir$ attributes align : 16 :: t0
                       !dir$ attributes align : 16 :: t1
                       !dir$ attributes align : 16 :: k0a3
                       !dir$ attributes align : 16 :: epst
                       !dir$ attributes align : 16 :: mut
                       !dir$ attributes align : 16 :: sqr
                       !dir$ attributes align : 16 :: t2
                       !dir$ attributes align : 16 :: diff
                   type(XMM4r4_t), parameter :: C314159265358979323846264338328  =
                                                       XMM4r4_t(3.14159265358979323846264338328_sp)
                   type(XMM4r4_t), parameter :: C078539816339744830961566084582  = &
                                                       XMM4r4_t(0.78539816339744830961566084582_sp)
                   type(XMM4r4_t), parameter :: C20 = XMM4r4_t(2.0_sp)
                   type(XMM4r4_t), automatic :: t0,t1,k0a3,epst
                   type(XMM4r4_t), automatic :: mut,sqr
                   type(XMM4r4_t), automatic :: t2,diff
#if (GMS_EXPLICIT_VECTORIZE) == 1
                    integer(kind=i4) :: j
                    !dir$ loop_count(4)
                    !dir$ vector aligned
                    !dir$ vector vectorlength(4)
                    !dir$ vector always
                    do j=0, 3
                         k0a3.v(j)   = k0a.v(j) *k0a.v(j) *k0a.v(j) 
                         t0.v(j)     = C078539816339744830961566084582.v(j) * &
                                       C314159265358979323846264338328.v(j) *a.v(j) 
                         epst.v(j)   = mu1.v(j)/mu0.v(j)- v4r4_1.v(j) 
                         t1.v(j)     = eps1.v(j)-eps0.v(j)
                         t2.v(j)     = eps1.v(j)+eps0.v(j) 
                         mut.v(j)    = C20.v(j) *(t1.v(j)/t2.v(j))
                         diff.v(j)   = epst.v(j)-mut.v(j)
                         sqr.v(j)    = diff.v(j)*diff.v(j) 
                         rcs.v(j)    = t0.v(j)*k0a3.v(j)*sqr.v(j) 
                    end do
#else                   
                   k0a3.v  = k0a.v*k0a.v*k0a.v
                   t0.v    = C078539816339744830961566084582.v* &
                             C314159265358979323846264338328.v*a.v
                   epst.v  = mu1.v/mu0.v- v4r4_1.v
                   t1.v    = eps1.v-eps0.v
                   t2.v    = eps1.v+eps0.v
                   mut.v   = C20.v*(t1.v/t2.v)
                   diff.v  = epst.v-mut.v
                   sqr.v   = diff.v*diff.v
                   rcs.v   = t0.v*k0a3.v*sqr.v
#endif
               end function rcs_f4152_xmm4r4
               
               
               ! /*
               !            Fresnel reflection and transmission coefficients
               !            Formula 4.1-72
               !        */
               
               
               pure function Tin_f4172_xmm4r4(mu,eps,psi) result(Tin)
               
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: Tin_f4172_xmm4r4
                   !dir$ attributes forceinline :: Tin_f4172_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: Tin_f4172_xmm4r4
                   use mod_vecconsts, only :  v4r4_1, v4r4_0
                   type(  XMM4c4_t),    intent(in) :: mu
                   type(  XMM4c4_t),    intent(in) :: eps
                   type(XMM4r4_t),  intent(in) :: psi
                   type(  XMM4c4_t)  :: Tin
                   ! Locals
                      !dir$ attributes align : 16 :: div
                      !dir$ attributes align : 16 :: sq1
                      !dir$ attributes align : 16 :: sq2
                      !dir$ attributes align : 16 :: mul
                      !dir$ attributes align : 16 :: tc0
                      !dir$ attributes align : 16 :: tc1
                      !dir$ attributes align : 16 :: tc2
                      !dir$ attributes align : 16 :: tc3
                      !dir$ attributes align : 16 :: sin2p
                      !dir$ attributes align : 16 :: cosp
                      !dir$ attributes align : 16 :: t0
                      !dir$ attributes align : 16 :: t1
                   type(  XMM4c4_t),  automatic :: div,sq1
                   type(  XMM4c4_t),  automatic :: sq2,mul
                   type(  XMM4c4_t),  automatic :: tc0,tc1
                   type(  XMM4c4_t),  automatic :: tc2,tc3
                   type(XMM4r4_t),automatic :: sin2p,cosp
                   type(XMM4r4_t),automatic :: t0,t1
                   div    = mu/eps
                   t0.v   = sin(psi.v)
                   cosp.v = cos(psi.v)
                   sin2p.v= t0.v*t0.v
                   t1.v   =  v4r4_1.v-sin2p.v
                   mul    = mu*eps
                   sq1    = csqrt_xmm4c4(div)
                   tc0    = t1/mul
                   sq2    = csqrt_xmm4c4(tc0)
                   tc2.re = sq1.re+sq1.re
                   tc2.im =  v4r4_0.v
                   tc1    = tc2*sq2
                   tc3    = sq1*sq2
                   tc3.re = cosp.v+tc3.re
                   tc3.im =  v4r4_0.v
                   Tin    = tc1/tc3
               end function Tin_f4172_xmm4r4
               
               
               ! /*
               !            Fresnel reflection and transmission coefficients
               !            Formula 4.1-73
               !        */
               
               pure function Tin_f4173_xmm4r4(mu,eps,psi) result(Tin)
                    
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: Tin_f4173_xmm4r4
                   !dir$ attributes forceinline :: Tin_f4173_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: Tin_f4173_xmm4r4 
                   use mod_vecconsts, only :  v4r4_1
                   type(  XMM4c4_t),   intent(in) :: mu
                   type(  XMM4c4_t),   intent(in) :: eps
                   type(XMM4r4_t), intent(in) :: psi
                   type(  XMM4c4_t) :: Tin
                   ! Locals
                      !dir$ attributes align : 16 :: div
                      !dir$ attributes align : 16 :: sq1
                      !dir$ attributes align : 16 :: sq2
                      !dir$ attributes align : 16 :: mul
                      !dir$ attributes align : 16 :: tc0
                      !dir$ attributes align : 16 :: cosp
                      !dir$ attributes align : 16 :: cos2p
                      !dir$ attributes align : 16 :: sinp
                      !dir$ attributes align : 16 :: sin2p
                      !dir$ attributes align : 16 :: msp1
                   type(  XMM4c4_t),  automatic :: div,sq1
                   type(  XMM4c4_t),  automatic :: sq2,mul
                   type(  XMM4c4_t),  automatic :: tc0
                   type(XMM4r4_t),automatic :: cosp,cos2p
                   type(XMM4r4_t),automatic :: sinp,sin2p
                   type(XMM4r4_t),automatic :: msp1
                   mul    = mu*esp
                   cosp.v = cos(psi.v)
                   div    = eps/mu
                   sinp.v = sin(psi.v)
                   cos2p.v= cosp.v+cosp.v
                   sin2p.v= sinp.v+sinp.v
                   msp1.v =  v4r4_1.v-sin2p.v
                   sq1    = csqrt_xmm4c4(div)
                   tc0    = msp1/mul
                   sq2    = csqrt_xmm4c4(tc0)
                   Tin    = sq1*sq2+cosp
               end function Tin_f4173_xmm4r4
               
               
              
                 !   /*
                 !          Fresnel reflection and transmission coefficients
                 !          Formula 4.1-74
                 !      */
                 
               pure function Tout_f4174_xmm4r4(mu,eps,psi) result(Tout)
               
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: Tout_f4174_xmm4r4
                   !dir$ attributes forceinline :: Tout_f4174_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: Tout_f4174_xmm4r4 
                   use mod_vecconsts, only :  v4r4_1, v4r4_0
                   type(  XMM4c4_t),   intent(in) :: mu
                   type(  XMM4c4_t),   intent(in) :: eps
                   type(XMM4r4_t), intent(in) :: psi
                   type(  XMM4c4_t) :: Tout
                   ! Locals
                      !dir$ attributes align : 16 :: C20
                      !dir$ attributes align : 16 :: div
                      !dir$ attributes align : 16 :: sq1
                      !dir$ attributes align : 16 :: sq2
                      !dir$ attributes align : 16 :: mul
                      !dir$ attributes align : 16 :: tc0
                      !dir$ attributes align : 16 :: tc1
                      !dir$ attributes align : 16 :: num
                      !dir$ attributes align : 16 :: den
                      !dir$ attributes align : 16 :: cosp
                      !dir$ attributes align : 16 :: sinp
                      !dir$ attributes align : 16 :: sin2p
                      !dir$ attributes align : 16 :: t0
                   type(XMM4r4_t), parameter :: C20 = XMM4r4_t(2.0_sp)
                   type(  XMM4c4_t),   automatic :: div,sq1
                   type(  XMM4c4_t),   automatic :: sq2,mul
                   type(  XMM4c4_t),   automatic :: tc0,tc1
                   type(  XMM4c4_t),   automatic :: num,den
                   type(XMM4r4_t), automatic :: cosp,sinp
                   type(XMM4r4_t), automatic :: sin2p,t0
                   div    = eps/mu
                   cosp.v = cos(psi.v)
                   mul    = eps*mu
                   sinp.v = sin(psi.v)
                   sq1    = csqrt_xmm4c4(div)
                   sin2p.v= sinp.v*sinp.v
                   t0.v   = mul.re*sinp.v
                   tc0.re =  v4r4_1.v-t0.v
                   tc0.im = t0.v
                   sq2    = csqrt_xmm4c4(tc0)
                   tc1    = sq1*tc0
                   num.re = C20.v*tc1.re
                   num.im = C20.v*tc1.im
                   den.re = cosp.v+tc1.re
                   den.im =  v4r4_0.v
                   Tout   = num/den
               end function Tout_f4174_xmm4r4
               
               
               
               !    /*
               !            Fresnel reflection and transmission coefficients
               !            Formula 4.1-75
               !        */
               
               pure function Tout_f4175_xmm4r4(mu,eps,psi) result(Tout)
                    
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: Tout_f4175_xmm4r4
                   !dir$ attributes forceinline :: Tout_f4175_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: Tout_f4175_xmm4r4  
                   use mod_vecconsts, only :  v4r4_1, v4r4_0
                   type(  XMM4c4_t),   intent(in) :: mu
                   type(  XMM4c4_t),   intent(in) :: eps
                   type(XMM4r4_t), intent(in) :: psi
                   type(  XMM4c4_t) :: Tout
                      !dir$ attributes align : 16 :: div
                      !dir$ attributes align : 16 :: sq1
                      !dir$ attributes align : 16 :: sq2
                      !dir$ attributes align : 16 :: mul
                      !dir$ attributes align : 16 :: tc0
                      !dir$ attributes align : 16 :: num
                      !dir$ attributes align : 16 :: den
                      !dir$ attributes align : 16 :: cosp
                      !dir$ attributes align : 16 :: sinp
                      !dir$ attributes align : 16 :: sin2p
                      !dir$ attributes align : 16 :: cos2p
                   type(  XMM4c4_t),   automatic :: div,sq1
                   type(  XMM4c4_t),   automatic :: sq2,mul
                   type(  XMM4c4_t),   automatic :: tc0
                   type(  XMM4c4_t),   automatic :: num,den
                   type(XMM4r4_t), automatic :: cosp,sinp
                   type(XMM4r4_t), automatic :: sin2p,cos2p
                   div    = mu/eps
                   cosp.v = cos(psi.v)
                   mul    = eps*mu
                   sinp.  = sin(psi.v)
                   cos2p.v= cosp.v+cosp.v
                   sq2    = csqrt_xmm4c4(div)
                   sin2p.v= sinp.v*sinp.v
                   tc0.re =  v4r4_1.v-mul.re*sin2p.v
                   tc0.im = mul.im*sin2p.v
                   sq1    = csqrt_xmm4c4(tc0)
                   den    = sq1*sq2
                   den.re = den.re+cosp.v
                   Tout   = cos2p/den
               end function Tout_f4175_xmm4r4
               
               
               !  /*
               !            Fresnel reflection and transmission coefficients
               !            Formula 4.1-76
               !     */
               
               pure function Rin_f4176_xmm4r4(mu,eps,psi) result(Rin)
                    
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: Rin_f4176_xmm4r4
                   !dir$ attributes forceinline :: Rin_f4176_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: Rin_f4176_xmm4r4  
                   use mod_vecconsts, only :  v4r4_1
                   type(  XMM4c4_t),   intent(in) :: mu
                   type(  XMM4c4_t),   intent(in) :: eps
                   type(XMM4r4_t), intent(in) :: psi
                   type(  XMM4c4_t) :: Rin
                   ! Locals
                      !dir$ attributes align : 16 :: div
                      !dir$ attributes align : 16 :: mul
                      !dir$ attributes align : 16 :: den
                      !dir$ attributes align : 16 :: num
                      !dir$ attributes align : 16 :: sq1
                      !dir$ attributes align : 16 :: sq2
                      !dir$ attributes align : 16 :: tc0
                      !dir$ attributes align : 16 :: cosp
                      !dir$ attributes align : 16 :: sinp
                      !dir$ attributes align : 16 :: sin2p
                    
                   type(  XMM4c4_t),   automatic :: div,mul
                   type(  XMM4c4_t),   automatic :: den,num
                   type(  XMM4c4_t),   automatic :: sq1,sq2
                   type(  XMM4c4_t),   automatic :: tc0
                   type(XMM4r4_t), automatic :: cosp,sinp
                   type(XMM4r4_t), automatic :: sin2p
                   div    = mu/eps
                   sinp.v = sin(psi.v)
                   mul    = mu*eps
                   sq2    = csqrt_xmm4c4(div)
                   sin2p.v= sinp.v+sinp.v
                   tc0    = sin2p/mul
                   cosp.v = cos(psi.v)
                   tc0.re =  v4r4_1.v-tc0.re
                   tc0.im = -tc0.im
                   sq1    = csqrt_xmm4c4(tc0)
                   sq2.re = cosp.v*sq2.re
                   sq2.im = cosp.v*sq2.im
                   num    = sq2-sq1
                   den    = sq2+sq1
                   Rin    = num/den
               end function Rin_f4176_xmm4r4
               
               
                !  /*
                !           Fresnel reflection and transmission coefficients
                !           Formula 4.1-77
                !    */

               pure function Rin_f4177_xmm4r4(mu,eps,psi) result(Rin)
                    
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: Rin_f4177_xmm4r4
                   !dir$ attributes forceinline :: Rin_f4177_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: Rin_f4177_xmm4r4  
                   use mod_vecconsts, only :  v4r4_1
                   type(  XMM4c4_t),   intent(in) :: mu
                   type(  XMM4c4_t),   intent(in) :: eps
                   type(XMM4r4_t), intent(in) :: psi
                   type(  XMM4c4_t) :: Rin
                   ! Locals
                      !dir$ attributes align : 16 :: div
                      !dir$ attributes align : 16 :: mul
                      !dir$ attributes align : 16 :: den
                      !dir$ attributes align : 16 :: num
                      !dir$ attributes align : 16 :: sq1
                      !dir$ attributes align : 16 :: sq2
                      !dir$ attributes align : 16 :: tc0
                      !dir$ attributes align : 16 :: cosp
                      !dir$ attributes align : 16 :: sinp
                      !dir$ attributes align : 16 :: sin2p
                   type(  XMM4c4_t),   automatic :: div,mul
                   type(  XMM4c4_t),   automatic :: den,num
                   type(  XMM4c4_t),   automatic :: sq1,sq2
                   type(  XMM4c4_t),   automatic :: tc0
                   type(XMM4r4_t), automatic :: cosp,sinp
                   type(XMM4r4_t), automatic :: sin2p
                   div    = mu/eps
                   sinp.v = sin(psi.v)
                   mul    = mu*eps
                   sq2    = csqrt_xmm4c4(div)
                   sin2p.v= sinp.v+sinp.v
                   cosp.v = cos(psi.v)
                   tc0    = mul*sin2p
                   tc0.re =  v4r4_1.v-tc0.re
                   sq1    = csqrt_xmm4c4(tc0)
                   sq2.re = cosp.v*sq2.re
                   sq2.im = cosp.v*sq2.im
                   num    = sq2-sq1
                   den    = sq2+sq1
                   Rin    = num/den
               end function Rin_f4177_xmm4r4
               
               
               ! /*
               !           Specular rays reflection
               !           Formula 4.1-64
               !       */
               
               pure function Rext_f4164_xmm4r4(mu,eps) result(Rext)
                    
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: Rext_f4164_xmm4r4
                   !dir$ attributes forceinline :: Rext_f4164_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: Rext_f4164_xmm4r4
                   type(  XMM4c4_t),   intent(in) :: mu
                   type(  XMM4c4_t),   intent(in) :: eps
                   type(  XMM4c4_t) :: Rext
                   ! Locals
                     !dir$ attributes align : 16 :: sq1
                     !dir$ attributes align : 16 :: sq2
                     !dir$ attributes align : 16 :: dif
                     !dir$ attributes align : 16 :: summ
                   type(  XMM4c4_t),  automatic :: sq1,sq2
                   type(  XMM4c4_t),  automatic :: dif,summ
                   sq1  = csqrt_xmm4c4(mu)
                   sq2  = csqrt_xmm4c4(eps)
                   dif  = sq1-sq2
                   summ = sq1+sq2 
                   Rext = dif/summ
               end function Rext_f4164_xmm4r4
               
               
                ! /*
                !!
                !         Axial rays, when phi = 0
                !         Formula 4.1-67
                !    */
                
                pure function Tin_f4167_xmm4r4(mu,eps) result(Tin)
                     
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: Tin_f4167_xmm4r4
                   !dir$ attributes forceinline :: Tin_f4167_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: Tin_f4167_xmm4r4
                   type(  XMM4c4_t),   intent(in) :: mu
                   type(  XMM4c4_t),   intent(in) :: eps
                   type(  XMM4c4_t) :: Tin
                   ! Locals
                     !dir$ attributes align : 16 :: sq1
                     !dir$ attributes align : 16 :: sq2
                     !dir$ attributes align : 16 :: summ
                     !dir$ attributes align : 16 :: mu2
                   type(  XMM4c4_t),  automatic :: sq1,sq2
                   type(  XMM4c4_t),  automatic :: summ,mu2
                   sq1  = csqrt_xmm4c4(mu)
                   mu2  = sq1+sq1
                   sq2  = csqrt_xmm4c4(eps)
                   summ = sq1+sq2
                   Tin  = mu2/summ
                end function Tin_f4167_xmm4r4
                
                
                  !/*
                  !        Axial rays, when phi = 0
                  !        Formula 4.1-68
                  ! */
                  
                pure function Tout_f4168_xmm4r4(mu,eps) result(Tout)
                     
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: Tout_f4168_xmm4r4
                   !dir$ attributes forceinline :: Tout_f4168_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: Tout_f4168_xmm4r4
                   type(  XMM4c4_t),   intent(in) :: mu
                   type(  XMM4c4_t),   intent(in) :: eps
                   type(  XMM4c4_t) :: Tout
                   ! Locals
                     !dir$ attributes align : 16 :: sq1
                     !dir$ attributes align : 16 :: sq2
                     !dir$ attributes align : 16 :: summ
                     !dir$ attributes align : 16 :: eps2
                   type(  XMM4c4_t),  automatic :: sq1,sq2
                   type(  XMM4c4_t),  automatic :: summ,eps2
                   sq1  = csqrt_xmm4c4(eps)
                   eps2 = sq1+sq1
                   sq2  = csqrt_xmm4c4(mu)
                   summ = sq1+sq2
                   Tout = eps2/summ
                end function Tout_f4168_xmm4r4
                
                
                 !/*
                 !         Axial rays, when phi = 0
                 !         Formula 4.1-69
                 !  */
                 
                pure function Rint_f4169_xmm4r4(mu,eps) result(Rint)
                     
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: Rint_f4169_xmm4r4
                   !dir$ attributes forceinline :: Rint_f4169_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: Rint_f4169_xmm4r4
                   type(  XMM4c4_t),   intent(in) :: mu
                   type(  XMM4c4_t),   intent(in) :: eps
                   type(  XMM4c4_t) :: Rint
                   ! Locals
                     !dir$ attributes align : 16 :: Cn10
                     !dir$ attributes align : 16 :: tc0
                   type(XMM4r4_t), parameter :: Cn10 = XMM4r4_t(-1.0_sp)
                   type(  XMM4c4_t),   automatic :: tc0
                   tc0  = Rext_f4164_xmm4r4(mu,eps)
                   Rint = tc0*Cn10
                end function Rint_f4169_xmm4r4
                
                
                 !  /*
                 !      Backscatter widths in high-frequency limit.
                 !      Phi = 0, formula 4.1-91,for k1a>5.
                 !   */
                 
                pure function rcs_f4191_xmm4r4(a,mu,eps) result(rcs)
                     
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4191_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4191_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4191_xmm4r4
                   type(XMM4r4_t),  intent(in) :: a
                   type(  XMM4c4_t),    intent(in) :: mu
                   type(  XMM4c4_t),    intent(in) :: eps
                   type(XMM4r4_t) :: rcs
                   ! LOcals
                     !dir$ attributes align : 16 :: C314159265358979323846264338328
                     !dir$ attributes align : 16 :: tc0
                     !dir$ attributes align : 16 :: cab
                   type(XMM4r4_t), parameter :: C314159265358979323846264338328 = &
                                                    XMM4r4_t(3.14159265358979323846264338328_sp)
                   type(  XMM4c4_t),   automatic :: tc0
                   type(XMM4r4_t), automatic :: cab
   
                   tc0   = Rext_f4164_xmm4r4(mu,eps)
                   cab   =  cabs_xmm4c4(tc0)
                   rcs.v = cab.v*C314159265358979323846264338328.v*a.v

                end function rcs_f4191_xmm4r4
                
                
                !  /*
                !         Bistatic scattering width (k0a0<<1, k1a0<<1), function of phi angle.
                !         Formula 4.1-104
                !      */
                
                pure function rcs_f41104_xmm4r4(a0,a1,k0a0,phi,mu1,  &
                                                 mu0,eps1,eps0)     result(rcs)
                      
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f41104_xmm4r4
                   !dir$ attributes forceinline :: rcs_f41104_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f41104_xmm4r4
                   use mod_vecconsts, inly :  v4r4_1, v4r4_0
                   type(XMM4r4_t),  intent(in) :: a0
                   type(XMM4r4_t),  intent(in) :: a1
                   type(XMM4r4_t),  intent(in) :: k0a0
                   type(XMM4r4_t),  intent(in) :: phi
                   type(  XMM4c4_t),    intent(in) :: mu1
                   type(  XMM4c4_t),    intent(in) :: mu0
                   type(  XMM4c4_t),    intent(in) :: eps1
                   type(  XMM4c4_t),    intent(in) :: eps0
                   type(XMM4r4_t) :: rcs
                     !dir$ attributes align : 16 :: C078539816339744830961566084582
                     !dir$ attributes align : 16 :: C314159265358979323846264338328
                     !dir$ attributes align : 16 :: C20
                     !dir$ attributes align : 16 :: div
                     !dir$ attributes align : 16 :: e1m
                     !dir$ attributes align : 16 :: tc1
                     !dir$ attributes align : 16 :: tc0
                     !dir$ attributes align : 16 :: e0m
                     !dir$ attributes align : 16 :: div2
                     !dir$ attributes align : 16 :: num
                     !dir$ attributes align : 16 :: den
                     !dir$ attributes align : 16 :: pia
                     !dir$ attributes align : 16 :: k0a03
                     !dir$ attributes align : 16 :: a1a0
                     !dir$ attributes align : 16 :: pa1
                     !dir$ attributes align : 16 :: ma1
                     !dir$ attributes align : 16 :: cab
                     !dir$ attributes align : 16 :: cosp
                     !dir$ attributes align : 16 :: frac
                     !dir$ attributes align : 16 :: a1a0s
                   type(XMM4r4_t), parameter :: C078539816339744830961566084582 = &
                                                       XMM4r4_t(0.78539816339744830961566084582_sp)
                   type(XMM4r4_t), parameter :: C314159265358979323846264338328 = &
                                                       XMM4r4_t(3.14159265358979323846264338328_sp)
                   type(XMM4r4_t), parameter :: C20 = XMM4r4_t(2.0_sp)
                   type(  XMM4c4_t),   automatic :: div,e1m
                   type(  XMM4c4_t),   automatic :: tc1,tc0
                   type(  XMM4c4_t),   automatic :: e0m,div2
                   type(  XMM4c4_t),   automatic :: num,den
                   type(XMM4r4_t), automatic :: pia,k0a03
                   type(XMM4r4_t), automatic :: a1a0,pa1
                   type(XMM4r4_t), automatic :: ma1,cab
                   type(XMM4r4_t), automatic :: cosp,frac
                   type(XMM4r4_t), automatic :: a1a0s
                   k0a03.v =  k0a.v*k0a.v*k0a.v
                   pia.v   =  C314159265358979323846264338328.v*a.v
                   cosp.v  = cos(phi.v)
                   frac.v  = pia.v*C078539816339744830961566084582.v*k0a03.v
                   a1a0.v  = a1.v/a0.v
                   a1a0s.v = a1a0.v*a1a0.v
                   pa1.v   =  v4r4_1.v+a1a0s.v
                   e1m     = eps1*pa1
                   ma1.v   =  v4r4_1.v-a1a0s.v
                   div     = mu1/mu0
                   e0m     = eps0*ma1
                   tc0     = (div*ma1)- v4r4_1.v
                   num     = e1m-e0m
                   den     = e1m+e0m
                   div2    = num/den
                   div2    = C20*div2*cosp
                   tc1     = tc0-div2
                   cab     =  cabs_xmm4c4(tc1)
                   rcs.v   = frac.v*cab.v
                end function rcs_f41104_xmm4r4
                
                
                 ! /*
                 !        Backscattering  width (k0a0<<1, k1a0<<1), phi = 0
                 !        Formula 4.1-105
                 ! */
                 
                pure function rcs_f41105_xmm4r4(a0,a1,k0a0,mu1,  &
                                                 mu0,eps1,eps0)     result(rcs)
                      
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f41105_xmm4r4
                   !dir$ attributes forceinline :: rcs_f41105_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f41105_xmm4r4
                   use mod_vecconsts, inly :  v4r4_1, v4r4_0
                   type(XMM4r4_t),  intent(in) :: a0
                   type(XMM4r4_t),  intent(in) :: a1
                   type(XMM4r4_t),  intent(in) :: k0a0
                   type(  XMM4c4_t),    intent(in) :: mu1
                   type(  XMM4c4_t),    intent(in) :: mu0
                   type(  XMM4c4_t),    intent(in) :: eps1
                   type(  XMM4c4_t),    intent(in) :: eps0
                   type(XMM4r4_t) :: rcs
                     !dir$ attributes align : 16 :: C078539816339744830961566084582
                     !dir$ attributes align : 16 :: C314159265358979323846264338328
                     !dir$ attributes align : 16 :: C20
                     !dir$ attributes align : 16 :: div
                     !dir$ attributes align : 16 :: e1m
                     !dir$ attributes align : 16 :: tc1
                     !dir$ attributes align : 16 :: tc0
                     !dir$ attributes align : 16 :: e0m
                     !dir$ attributes align : 16 :: div2
                     !dir$ attributes align : 16 :: num
                     !dir$ attributes align : 16 :: den
                     !dir$ attributes align : 16 :: pia
                     !dir$ attributes align : 16 :: k0a03
                     !dir$ attributes align : 16 :: a1a0
                     !dir$ attributes align : 16 :: pa1
                     !dir$ attributes align : 16 :: ma1
                     !dir$ attributes align : 16 :: cab
                     !dir$ attributes align : 16 :: frac
                     !dir$ attributes align : 16 :: a1a0s
                   type(XMM4r4_t), parameter :: C078539816339744830961566084582 = &
                                                       XMM4r4_t(0.78539816339744830961566084582_sp)
                   type(XMM4r4_t), parameter :: C314159265358979323846264338328 = &
                                                       XMM4r4_t(3.14159265358979323846264338328_sp)
                   type(XMM4r4_t), parameter :: C20 = XMM4r4_t(2.0_sp)
                   type(  XMM4c4_t),   automatic :: div,e1m
                   type(  XMM4c4_t),   automatic :: tc1,tc0
                   type(  XMM4c4_t),   automatic :: e0m,div2
                   type(  XMM4c4_t),   automatic :: num,den
                   type(XMM4r4_t), automatic :: pia,k0a03
                   type(XMM4r4_t), automatic :: a1a0,pa1
                   type(XMM4r4_t), automatic :: ma1,cab
                   type(XMM4r4_t), automatic :: frac,a1a0s
                   k0a03.v =  k0a.v*k0a.v*k0a.v
                   pia.v   =  C314159265358979323846264338328.v*a.v
                   frac.v  = pia.v*C078539816339744830961566084582.v*k0a03.v
                   a1a0.v  = a1.v/a0.v
                   a1a0s.v = a1a0.v*a1a0.v
                   pa1.v   =  v4r4_1.v+a1a0s.v
                   e1m     = eps1*pa1
                   ma1.v   =  v4r4_1.v-a1a0s.v
                   div     = mu1/mu0
                   e0m     = eps0*ma1
                   tc0     = (div*ma1)- v4r4_1.v
                   num     = e1m-e0m
                   den     = e1m+e0m
                   div2    = num/den
                   div2    = C20*div2
                   tc1     = tc0-div2
                   cab     =  cabs_xmm4c4(tc1)
                   rcs.v   = frac.v*cab.v
                end function rcs_f41105_xmm4r4  
                
                
                !/*
                !      Forward scattering width (k0a0<<1, k1a0<<1), phi = pi.
                !      Formula 4.1-106
                ! */
                
                pure function rcs_f41106_xmm4r4(a0,a1,k0a0,mu1,  &
                                                 mu0,eps1,eps0)     result(rcs)
                      
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f41106_xmm4r4
                   !dir$ attributes forceinline :: rcs_f41106_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f41106_xmm4r4
                   use mod_vecconsts, inly :  v4r4_1, v4r4_0
                   type(XMM4r4_t),  intent(in) :: a0
                   type(XMM4r4_t),  intent(in) :: a1
                   type(XMM4r4_t),  intent(in) :: k0a0
                   type(  XMM4c4_t),    intent(in) :: mu1
                   type(  XMM4c4_t),    intent(in) :: mu0
                   type(  XMM4c4_t),    intent(in) :: eps1
                   type(  XMM4c4_t),    intent(in) :: eps0
                   type(XMM4r4_t) :: rcs
                     !dir$ attributes align : 16 :: C078539816339744830961566084582
                     !dir$ attributes align : 16 :: C314159265358979323846264338328
                     !dir$ attributes align : 16 :: C20
                     !dir$ attributes align : 16 :: div
                     !dir$ attributes align : 16 :: e1m
                     !dir$ attributes align : 16 :: tc1
                     !dir$ attributes align : 16 :: tc0
                     !dir$ attributes align : 16 :: e0m
                     !dir$ attributes align : 16 :: div2
                     !dir$ attributes align : 16 :: num
                     !dir$ attributes align : 16 :: den
                     !dir$ attributes align : 16 :: pia
                     !dir$ attributes align : 16 :: k0a03
                     !dir$ attributes align : 16 :: a1a0
                     !dir$ attributes align : 16 :: pa1
                     !dir$ attributes align : 16 :: ma1
                     !dir$ attributes align : 16 :: cab
                     !dir$ attributes align : 16 :: frac
                     !dir$ attributes align : 16 :: a1a0s
                   type(XMM4r4_t), parameter :: C078539816339744830961566084582 = &
                                                       XMM4r4_t(0.78539816339744830961566084582_sp)
                   type(XMM4r4_t), parameter :: C314159265358979323846264338328 = &
                                                       XMM4r4_t(3.14159265358979323846264338328_sp)
                   type(XMM4r4_t), parameter :: C20 = XMM4r4_t(2.0_sp)
                   type(  XMM4c4_t),   automatic :: div,e1m
                   type(  XMM4c4_t),   automatic :: tc1,tc0
                   type(  XMM4c4_t),   automatic :: e0m,div2
                   type(  XMM4c4_t),   automatic :: num,den
                   type(XMM4r4_t), automatic :: pia,k0a03
                   type(XMM4r4_t), automatic :: a1a0,pa1
                   type(XMM4r4_t), automatic :: ma1,cab
                   type(XMM4r4_t), automatic :: frac,a1a0s
                   k0a03.v =  k0a.v*k0a.v*k0a.v
                   pia.v   =  C314159265358979323846264338328.v*a.v
                   frac.v  = pia.v*C078539816339744830961566084582.v*k0a03.v
                   a1a0.v  = a1.v/a0.v
                   a1a0s.v = a1a0.v*a1a0.v
                   pa1.v   =  v4r4_1.v+a1a0s.v
                   e1m     = eps1*pa1
                   ma1.v   =  v4r4_1.v-a1a0s.v
                   div     = mu1/mu0
                   e0m     = eps0*ma1
                   tc0     = (div*ma1)- v4r4_1.v
                   num     = e1m-e0m
                   den     = e1m+e0m
                   div2    = num/den
                   div2    = C20*div2
                   tc1     = tc0+div2
                   cab     =  cabs_xmm4c4(tc1)
                   rcs.v   = frac.v*cab.v
                end function rcs_f41106_xmm4r4  
                
                
                 !/*
                 !      Hollow cylindrical shell.
                 !      Approximations for the low frequency region
                 ! !     (k0a0<<1, k1a0<<1).
                 !      Formula 4.1-124
                 ! */
                 
                pure function A0_f41124_xmm4r4(a1,a0,k0a0,eps1,eps0) result(A0)
                      
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: A0_f41124_xmm4r4
                   !dir$ attributes forceinline :: A0_f41124_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: A0_f41124_xmm4r4
                   use mod_vecconsts, only :  v4r4_1, v4r4_0
                   type(XMM4r4_t),  intent(in) :: a0
                   type(XMM4r4_t),  intent(in) :: a1
                   type(XMM4r4_t),  intent(in) :: k0a0
                   type(  XMM4c4_t),    intent(in) :: eps1
                   type(  XMM4c4_t),    intent(in) :: eps0
                   type(  XMM4c4_t) :: A0
                     !dir$ attributes align : 16 :: C078539816339744830961566084582
                     !dir$ attributes align : 16 :: frac
                     !dir$ attributes align : 16 :: div
                     !dir$ attributes align : 16 :: tc0
                     !dir$ attributes align : 16 :: k0a02
                     !dir$ attributes align : 16 :: a1a0
                     !dir$ attributes align : 16 :: a1a0s
                     !dir$ attributes align : 16 :: ma1
                   type(XMM4r4_t), parameter :: C078539816339744830961566084582 = &
                                                       XMM4r4_t(0.78539816339744830961566084582_sp)
                   type(  XMM4c4_t),   automatic :: frac,div
                   type(  XMM4c4_t),   automatic :: tc0
                   type(XMM4r4_t), automatic :: k0a02,a1a0
                   type(XMM4r4_t), automatic :: a1a0s,ma1
                   k0a02.v  = k0a.v*k0a.v
                   a1a0.v   = a1.v/a0.v
                   a1a0s.v  = a1a0.v*a1a0.v
                   frac.re  = C078539816339744830961566084582.v*k0a02.v
                   ma1.v    =  v4r4_1.v-a1a0s.v
                   frac.im  =  v4r4_0.v
                   div      = eps1/eps0
                   div      = div- v4r4_1.v
                   tc0      = div*ma1
                   A0       = frac*tc0
                end function A0_f41124_xmm4r4
                
                 !/*
                 !!
                 !      Hollow cylindrical shell.
                 !      Approximations for the low frequency region
                 !      (k0a0<<1, k1a0<<1).
                 !      Formula 4.1-126
                 !  */
                 
               pure function B0_f41126_xmm4r4(a1,a0,k0a0,mu1,mu0) result(B0)
                      
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: B0_f41126_xmm4r4
                   !dir$ attributes forceinline :: B0_f41126_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: B0_f41126_xmm4r4
                   use mod_vecconsts, only :  v4r4_1, v4r4_0
                   type(XMM4r4_t),  intent(in) :: a0
                   type(XMM4r4_t),  intent(in) :: a1
                   type(XMM4r4_t),  intent(in) :: k0a0
                   type(  XMM4c4_t),    intent(in) :: mu1
                   type(  XMM4c4_t),    intent(in) :: mu0
                   type(  XMM4c4_t) :: B0
                      !dir$ attributes align : 16 :: C078539816339744830961566084582
                     !dir$ attributes align : 16 :: frac
                     !dir$ attributes align : 16 :: div
                     !dir$ attributes align : 16 :: tc0
                     !dir$ attributes align : 16 :: k0a02
                     !dir$ attributes align : 16 :: a1a0
                     !dir$ attributes align : 16 :: a1a0s
                     !dir$ attributes align : 16 :: ma1
                   type(XMM4r4_t), parameter :: C078539816339744830961566084582 = &
                                                       XMM4r4_t(0.78539816339744830961566084582_sp)
                   type(  XMM4c4_t),   automatic :: frac,div
                   type(  XMM4c4_t),   automatic :: tc0
                   type(XMM4r4_t), automatic :: k0a02,a1a0
                   type(XMM4r4_t), automatic :: a1a0s,ma1
                   k0a02.v  = k0a.v*k0a.v
                   a1a0.v   = a1.v/a0.v
                   a1a0s.v  = a1a0.v*a1a0.v
                   frac.re  = C078539816339744830961566084582.v*k0a02.v
                   ma1.v    =  v4r4_1.v-a1a0s.v
                   frac.im  =  v4r4_0.v
                   div      = mu1/mu0
                   div      = div- v4r4_1.v
                   tc0      = div*ma1
                   B0       = frac*tc0
                end function B0_f41126_xmm4r4
                
                
                 !/*
                 ! !
                 !         Hollow cylindrical shell.
                 !         Approximations for the low frequency region
                 !         (k0a0<<1, k1a0<<1).
                 !          Formula 4.1-125
                 !   */
                 
                pure function A1_f41125_xmm4r4(a1,a0,k0a0,mu,mu0) result(A1)
                      
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: A1_f41125_xmm4r4
                   !dir$ attributes forceinline :: A1_f41125_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: A1_f41125_xmm4r4
                   use mod_vecconsts, only :  v4r4_1, v4r4_0
                   type(XMM4r4_t),  intent(in) :: a0
                   type(XMM4r4_t),  intent(in) :: a1
                   type(XMM4r4_t),  intent(in) :: k0a0
                   type(  XMM4c4_t),    intent(in) :: mu1
                   type(  XMM4c4_t),    intent(in) :: mu0
                   type(  XMM4c4_t) :: A1
                   ! Locals
                      !dir$ attributes align : 16 :: C078539816339744830961566084582
                     !dir$ attributes align : 16 :: rat
                     !dir$ attributes align : 16 :: div
                     !dir$ attributes align : 16 :: divs
                     !dir$ attributes align : 16 :: tc1
                     !dir$ attributes align : 16 :: sqp
                     !dir$ attributes align : 16 :: sqm
                     !dir$ attributes align : 16 :: tc0
                     !dir$ attributes align : 16 :: num
                     !dir$ attributes align : 16 :: den
                     !dir$ attributes align : 16 :: facr
                     !dir$ attributes align : 16 :: k0a2
                     !dir$ attributes align : 16 :: a1a0
                     !dir$ attributes align : 16 :: a1a0s
                     !dir$ attributes align : 16 :: ma1
                   type(XMM4r4_t), parameter :: C078539816339744830961566084582 = &
                                                       XMM4r4_t(0.78539816339744830961566084582_sp)
                   type(  XMM4c4_t),   automatic :: rat,div
                   type(  XMM4c4_t),   automatic :: divs,tc1
                   type(  XMM4c4_t),   automatic :: sqp,sqm
                   type(  XMM4c4_t),   automatic :: tc0,num
                   type(  XMM4c4_t),   automatic :: den,facr
                   type(XMM4r4_t), automatic :: k0a2,a1a0
                   type(  XMM4c4_t_t), automatic :: a1a0s,ma1
                   a1a0.v   = a1.v/a0.v
                   k0a2.v   = k0a.v*k0a.v
                   frac.im  =  v4r4_0.v
                   frac.re  = C078539816339744830961566084582.v*k0a2.v
                   div      = mu1/mu0
                   a1a0s.v  = a1a0.v*a1a0.v
                   ma1.v    =  v4r4_1.v-a1a0s.v
                   divs     = div*div
                   divs     = divs- v4r4_1.v
                   tc0.re   = div.re+ v4r4_1.v
                   tc0.im   =  v4r4_0.v
                   num      = divs*ma1
                   sqp      = tc0*tc0
                   tc1.re   = div.re- v4r4_1.v
                   tc1.im   =  v4r4_0.v
                   sqm      = tc1*tc1
                   sqm      = sqm*a1a02
                   den      = sqp*sqm
                   rat      = num/den
                   A1       = frac*rat
                end function A1_f41125_xmm4r4
                
                !  /*
                !!
                !          Hollow cylindrical shell.
                !          Approximations for the low frequency region
                !          (k0a0<<1, k1a0<<1).
                !           Formula 4.1-127
                !    */
                
                pure function B1_f41127_xmm4r4(a1,a0,k0a0,eps0,eps1) result(A1)
                      
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: B1_f41127_xmm4r4
                   !dir$ attributes forceinline :: B1_f41127_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: B1_f41127_xmm4r4
                   use mod_vecconsts, only :  v4r4_1, v4r4_0
                   type(XMM4r4_t),  intent(in) :: a0
                   type(XMM4r4_t),  intent(in) :: a1
                   type(XMM4r4_t),  intent(in) :: k0a0
                   type(  XMM4c4_t),    intent(in) :: eps0
                   type(  XMM4c4_t),    intent(in) :: eps1
                   type(  XMM4c4_t) :: B1
                   ! Locals
                       !dir$ attributes align : 16 :: C078539816339744830961566084582
                     !dir$ attributes align : 16 :: rat
                     !dir$ attributes align : 16 :: div
                     !dir$ attributes align : 16 :: divs
                     !dir$ attributes align : 16 :: tc1
                     !dir$ attributes align : 16 :: sqp
                     !dir$ attributes align : 16 :: sqm
                     !dir$ attributes align : 16 :: tc0
                     !dir$ attributes align : 16 :: num
                     !dir$ attributes align : 16 :: den
                     !dir$ attributes align : 16 :: facr
                     !dir$ attributes align : 16 :: k0a2
                     !dir$ attributes align : 16 :: a1a0
                     !dir$ attributes align : 16 :: a1a0s
                     !dir$ attributes align : 16 :: ma1
                   type(XMM4r4_t), parameter :: C078539816339744830961566084582 = &
                                                       XMM4r4_t(0.78539816339744830961566084582_sp)
                   type(  XMM4c4_t),   automatic :: rat,div
                   type(  XMM4c4_t),   automatic :: divs,tc1
                   type(  XMM4c4_t),   automatic :: sqp,sqm
                   type(  XMM4c4_t),   automatic :: tc0,num
                   type(  XMM4c4_t),   automatic :: den,facr
                   type(XMM4r4_t), automatic :: k0a2,a1a0
                   type(  XMM4c4_t_t), automatic :: a1a0s,ma1
                   a1a0.v   = a1.v/a0.v
                   k0a2.v   = k0a.v*k0a.v
                   frac.im  =  v4r4_0.v
                   frac.re  = C078539816339744830961566084582.v*k0a2.v
                   div      = eps1/eps0
                   a1a0s.v  = a1a0.v*a1a0.v
                   ma1.v    =  v4r4_1.v-a1a0s.v
                   divs     = div*div
                   divs     = divs- v4r4_1.v
                   tc0.re   = div.re+ v4r4_1.v
                   tc0.im   =  v4r4_0.v
                   num      = divs*ma1
                   sqp      = tc0*tc0
                   tc1.re   = div.re- v4r4_1.v
                   tc1.im   =  v4r4_0.v
                   sqm      = tc1*tc1
                   sqm      = sqm*a1a02
                   den      = sqp*sqm
                   rat      = num/den
                   B1       = frac*rat
                end function B1_f41127_xmm4r4
                
                !  /*
                !!
                !          Low-frequncy approximations (k0a<0.2)
                !          Cylindrical Luneberg lens (k0a<0.2).
                !          Formula 4.1-162
                !     */
                
                pure function A0_f41162_xmm4r4(k0a) result(A0)
                     
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: A0_f41162_xmm4r4
                   !dir$ attributes forceinline :: A0_f41162_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: A0_f41162_xmm4r4
                   use mod_vecconsts, only :  v4r4_0
                   type(XMM4r4_t),  intent(in) :: k0a
                   type(  XMM4c4_t) :: A0
                     !dir$ attributes align : 16 :: C078539816339744830961566084582
                     !dir$ attributes align : 16 :: C05
                     !dir$ attributes align : 16 :: k0a2
                     !dir$ attributes align : 16 :: k0ah
                   type(XMM4r4_t), parameter :: C078539816339744830961566084582 = &
                                                       XMM4r4_t(0.78539816339744830961566084582_sp)
                   type(XMM4r4_t), parameter :: C05 = XMM4r4_t(0.5_sp)
                   ! Locals
                   
                   type(XMM4r4_t), automatic :: k0a2,k0ah
#if (GMS_EXPLICIT_VECTORIZE) == 1
                    integer(kind=i4) :: j
                    !dir$ loop_count(4)
                    !dir$ vector aligned
                    !dir$ vector vectorlength(4)
                    !dir$ vector always
                    do j=0, 3
                       k0a2.v(j) = k0a.v(j)*k0a.v(j)
                       k0ah.v(j) = C05.v(j)*k0a2.v(j)
                       A0.re(j)  = C078539816339744830961566084582.v(j)*k0ah.v(j)
                       A0.im(j)  =  v4r4_0.v(j)
                    end do     
#else              
                   k0a2.v = k0a.v*k0a.v
                   k0ah.v = C05.v*k0a2.v
                   A0.re  = C078539816339744830961566084582.v*k0ah.v
                   A0.im  =  v4r4_0.v
#endif
                end function A0_f41162_xmm4r4
                
                
                pure function B1_f41162_xmm4r4(k0a) result(B1)
                    
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: B1_f41162_xmm4r4
                   !dir$ attributes forceinline :: B1_f41162_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: B1_f41162_xmm4r4
                   use mod_vecconsts, only :  v4r4_0
                   type(XMM4r4_t),  intent(in) :: k0a
                   type(  XMM4c4_t) :: B1
                     !dir$ attributes align : 16 :: C078539816339744830961566084582
                     !dir$ attributes align : 16 :: C18992
                     !dir$ attributes align : 16 :: k0a2
                     !dir$ attributes align : 16 :: k0ah
                   type(XMM4r4_t), parameter :: C078539816339744830961566084582 = &
                                                       XMM4r4_t(0.78539816339744830961566084582_sp)
                   type(XMM4r4_t), parameter :: C18992 = XMM4r4_t(1.8992_sp)
                   ! Locals
                   type(XMM4r4_t), automatic :: k0a2,k0ah
#if (GMS_EXPLICIT_VECTORIZE) == 1
                    integer(kind=i4) :: j
                    !dir$ loop_count(4)
                    !dir$ vector aligned
                    !dir$ vector vectorlength(4)
                    !dir$ vector always
                    do j=0, 3          
                         k0a2.v(j) = k0a.v(j)*k0a.v(j)
                         k0ah.v(j) = C18992.v(j)*k0a2.v(j)
                         B1.re(j)  = C078539816339744830961566084582.v(j)*k0ah.v(j)
                         B1.im(j)  =  v4r4_0.v(j)
                    end do
#else         
                   k0a2.v = k0a.v*k0a.v
                   k0ah.v = C18992.v*k0a2.v
                   B1.re  = C078539816339744830961566084582.v*k0ah.v
                   B1.im  =  v4r4_0.v
#endif
                end function B1_f41162_xmm4r4
                
                !  /*
                !          Low-frequncy approximations (k0a<0.2)
                !          Cylindrical Luneberg lens (k0a<0.2).  
                !          Scattering widths.
                !          Formula 4.1-163
                ! !     */
                
                pure function rcs_f41163_xmm4r4(a,k0a) result(rcs)
                     
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f41163_xmm4r4
                   !dir$ attributes forceinline :: rcs_f41163_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f41163_xmm4r4
                   type(XMM4r4_t),  intent(in) :: a
                   type(XMM4r4_t),  intent(in) :: k0a
                   type(XMM4r4_t) :: rcs
                     !dir$ attributes align : 16 :: C9869604401089358618834490999876
                     !dir$ attributes align : 16 :: C00625
                     !dir$ attributes align : 16 :: t0
                     !dir$ attributes align : 16 :: k0a3
                   type(XMM4r4_t), parameter :: C9869604401089358618834490999876 = &
                                                          XMM4r4_t(9.869604401089358618834490999876_sp)
                   type(XMM4r4_t), parameter :: C00625 = XMM4r4_t(0.0625_SP)
                   ! Locals
                   type(XMM4r4_t), automatic :: t0,k0a3
#if (GMS_EXPLICIT_VECTORIZE) == 1
                    integer(kind=i4) :: j
                    !dir$ loop_count(4)
                    !dir$ vector aligned
                    !dir$ vector vectorlength(4)
                    !dir$ vector always
                    do j=0, 3        
                       t0.v(j)   = C9869604401089358618834490999876.v(j)*a.v(j)
                       k0a3.v(j) = k0a.v(j)*k0a.v(j)*k0a.v(j)
                       rcs.v(j)  = k0a3.v(j)*t0.v(j)
                    end do
#else                  
                   t0.v   = C9869604401089358618834490999876.v*a.v
                   k0a3.v = k0a.v*k0a.v*k0a.v
                   rcs.v  = k0a3.v*t0.v
#endif
                end function rcs_f41163_xmm4r4
                
                ! /*
                !          Low-frequncy approximations (k0a<0.2)
                !          Cylindrical Luneberg lens (k0a<0.2).  
                !          Scattering widths.
                !          Formula 4.1-164
                !      */
                
                pure function rcs_f41164_xmm4r4(a,k0a,phi) result(rcs)
                    
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f41164_xmm4r4
                   !dir$ attributes forceinline :: rcs_f41164_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f41164_xmm4r4 
                   type(XMM4r4_t),  intent(in) :: a
                   type(XMM4r4_t),  intent(in) :: k0a
                   type(XMM4r4_t),  intent(in) :: phi
                   type(XMM4r4_t) :: rcs
                     !dir$ attributes align : 16 :: C9869604401089358618834490999876
                     !dir$ attributes align : 16 :: C003607
                     !dir$ attributes align : 16 :: t0
                     !dir$ attributes align : 16 :: k0a3
                     !dir$ attributes align : 16 :: cosp
                     !dir$ attributes align : 16 :: cos2p
                   type(XMM4r4_t), parameter :: C9869604401089358618834490999876 = &
                                                           XMM4r4_t(9.869604401089358618834490999876_sp)
                   type(XMM4r4_t), parameter :: C003607 = XMM4r4_t(0.03607_sp)
                   type(XMM4r4_t), automatic :: t0,cosp,k0a3,cos2p
#if (GMS_EXPLICIT_VECTORIZE) == 1
                    integer(kind=i4) :: j
                    !dir$ loop_count(4)
                    !dir$ vector aligned
                    !dir$ vector vectorlength(4)
                    !dir$ vector always
                    do j=0, 3 
                        k0a3.v(j)  = k0a.v(j)*k0a.v(j)*k0a.v(j)
                        cosp.v(j)  = cos(phi.v(j))
                        t0.v(j)    = C003607.v(j)* &
                                     C9869604401089358618834490999876.v(j)*a.v(j)
                        cos2p.v(j) = cosp.v(j)*cosp.v(j)
                        rcs.v(j)   = t0.v(j)*k0a3.v(j)*cos2p.v(j)
                    end do
#else                    
                   k0a3.v  = k0a.v*k0a.v*k0a.v
                   cosp.v  = cos(phi.v)
                   t0.v    = C003607.v*C9869604401089358618834490999876.v*a.v
                   cos2p.v = cosp.v*cosp.v
                   rcs.v   = t0.v*k0a3.v*cos2p.v
#endif
                end function rcs_f41164_xmm4r4
                
                !   /*
                !!
                !      Cylindrical Eaton-Lippman Lens, (k0a<0.2)
                !      Formulae 4.1-165
                !  */
                
                pure function A0_f41165_xmm4r4(k0a) result(A0)
                     
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: A0_f41165_xmm4r4
                   !dir$ attributes forceinline :: A0_f41165_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: A0_f41165_xmm4r4
                   use mod_vecconsts, only :  v4r4_0
                   type(XMM4r4_t),  intent(in) :: k0a
                   type(  XMM4c4_t)  :: A0
                   ! Locals
                     !dir$ attributes align : 16 :: C078539816339744830961566084582
                     !dir$ attributes align : 16 :: k0a2
                   type(XMM4r4_t),  parameter :: C078539816339744830961566084582 = &
                                                        XMM4r4_t(0.78539816339744830961566084582_sp)
                   type(XMM4r4_t),  automatic :: k0a2
#if (GMS_EXPLICIT_VECTORIZE) == 1
                    integer(kind=i4) :: j
                    !dir$ loop_count(4)
                    !dir$ vector aligned
                    !dir$ vector vectorlength(4)
                    !dir$ vector always
                    do j=0, 3 
                        k0a2.v(j)  = k0a.v(j)*ka0.v(j) 
                        A0.re(j)   = C078539816339744830961566084582.v(j)* &
                                     k0a2.v(j)
                        A0.im(j)   =  v4r4_0.v(j)
                    end do
#else
                   k0a2.v  = k0a.v*ka0.v 
                   A0.re   = C078539816339744830961566084582.v*k0a2.v
                   A0.im   =  v4r4_0.v
#endif
                end function A0_f41165_xmm4r4
                
                pure function B1_f41165_xmm4r4(k0a) result(B1)
                     
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: B1_f41165_xmm4r4
                   !dir$ attributes forceinline :: B1_f41165_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: B1_f41165_xmm4r4
                   use mod_vecconsts, only :  v4r4_0
                   type(XMM4r4_t),  intent(in) :: k0a
                   type(  XMM4c4_t)  :: B1
                   ! Locals
                     !dir$ attributes align : 16 :: C078539816339744830961566084582
                     !dir$ attributes align : 16 :: C043616
                     !dir$ attributes align : 16 :: k0a2
                     !dir$ attributes align : 16 :: t0
                   type(XMM4r4_t),  parameter :: C078539816339744830961566084582 = &
                                                            XMM4r4_t(0.78539816339744830961566084582_sp)
                   type(XMM4r4_t),  parameter :: C043616 = XMM4r4_t(0.43616_sp)
                   type(XMM4r4_t),  automatic :: k0a2,t0
#if (GMS_EXPLICIT_VECTORIZE) == 1
                    integer(kind=i4) :: j
                    !dir$ loop_count(4)
                    !dir$ vector aligned
                    !dir$ vector vectorlength(4)
                    !dir$ vector always
                    do j=0, 3 
                         k0a2.v(j)  = k0a.v(j)*ka0.v(j) 
                         t0.v(j)    = C043616.v(j)*k0a2.v(j)
                         B1.re(j)   = C078539816339744830961566084582.v(j)* &
                                      t0.v(j)
                         B1.im(j)   =  v4r4_0.v(j)
                    end do
#else
                   k0a2.v  = k0a.v*ka0.v 
                   t0.v    = C043616.v*k0a2.v
                   B1.re   = C078539816339744830961566084582.v*t0.v
                   B1.im   =  v4r4_0.v
#endif
                end function B1_f41165_xmm4r4
                
                ! /*
                !!
                !       Cylindrical Eaton-Lippman Lens, (k0a<0.2) 
                !       Scattering widths.
                !       Formula: 1.4-166
                !   */
                
                pure function rcs_f14166_xmm4r4(a,k0a) result(rcs)
                    
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f14166_xmm4r4
                   !dir$ attributes forceinline :: rcs_f14166_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f14166_xmm4r4
                   type(XMM4r4_t),  intent(in) :: a
                   type(XMM4r4_t),  intent(in) :: k0a
                   type(XMM4r4_t) :: rcs
                   ! Locals
                     !dir$ attributes align : 16 :: C9869604401089358618834490999876
                     !dir$ attributes align : 16 :: C025
                     !dir$ attributes align : 16 :: a4
                     !dir$ attributes align : 16 :: k0a3
                   type(XMM4r4_t),  parameter :: C9869604401089358618834490999876 = &
                                                         XMM4r4_t(9.869604401089358618834490999876_sp)
                   type(XMM4r4_t),  parameter :: C025 = XMM4r4_t(0.25_sp)
                   type(XMM4r4_t),  automatic :: a4,k0a3
#if (GMS_EXPLICIT_VECTORIZE) == 1
                    integer(kind=i4) :: j
                    !dir$ loop_count(4)
                    !dir$ vector aligned
                    !dir$ vector vectorlength(4)
                    !dir$ vector always
                    do j=0, 3    
                        a4.v(j)   = a.v(j)*C025.v(j)
                        k0a3.v = k0a.v(j)*k0a.v(j)*k0a.v(j)
                        rcs.v  = k0a3.v(j)*C9869604401089358618834490999876.v(j)* &
                                 a4.v(j)
                    end do
#else               
                   a4.v   = a.v*C025.v
                   k0a3.v = k0a.v*k0a.v*k0a.v
                   rcs.v  = k0a3.v*C9869604401089358618834490999876.v*a4.v
#endif
                end function rcs_f14166_xmm4r4
                
                 ! /*
                 !!
                 !      Cylindrical Eaton-Lippman Lens, (k0a<0.2) 
                 !      Scattering widths.
                 !      Formula: 1.4-167
                 !  */
                 
                 pure function rcs_f14167_xmm4r4(a,k0a,phi) result(rcs)
                     
                    !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f14167_xmm4r4
                   !dir$ attributes forceinline :: rcs_f14167_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f14167_xmm4r4
                   type(XMM4r4_t),  intent(in) :: a
                   type(XMM4r4_t),  intent(in) :: k0a
                   type(XMM4r4_t),  intent(in) :: phi
                   type(XMM4r4_t) :: rcs
                   ! Locals
                     !dir$ attributes align : 16 :: C9869604401089358618834490999876
                     !dir$ attributes align : 16 :: C019024
                     !dir$ attributes align : 16 :: cosp
                     !dir$ attributes align : 16 :: cos2p
                     !dir$ attributes align : 16 :: k0a3
                     !dir$ attributes align : 16 :: t0
                     !dir$ attributes align : 16 :: t1
                   type(XMM4r4_t),  parameter :: C9869604401089358618834490999876 = &
                                                             XMM4r4_t(9.869604401089358618834490999876_sp)
                   type(XMM4r4_t),  parameter :: C019024 =  XMM4r4_t(0.19024_sp)
                   type(XMM4r4_t),  automatic :: cosp,cos2p,k0a3,t0,t1
#if (GMS_EXPLICIT_VECTORIZE) == 1
                    integer(kind=i4) :: j
                    !dir$ loop_count(4)
                    !dir$ vector aligned
                    !dir$ vector vectorlength(4)
                    !dir$ vector always
                    do j=0, 3   
                         k0a3.v(j) = k0a.v(j)*k0a3.v(j)*k0a.v(j)
                         cosp.v(j) = cos(phi.v(j))
                         t0.v(j)   = C019024.v(j(j))* &
                                     C9869604401089358618834490999876.v(j)
                         t1.v(j)   = a.v(j)*k0a3.v(j)
                         cos2p.v(j)= cosp.v(j)*cosp.v(j)
                         rcs.v(j)  = t0.v(j)*t1.v(j)*cos2p.v(j)
                    end do
#else                   
                   k0a3.v = k0a.v*k0a3.v*k0a.v
                   cosp.v = cos(phi.v)
                   t0.v   = C019024.v*C9869604401089358618834490999876.v
                   t1.v   = a.v*k0a3.v
                   cos2p.v= cosp.v*cosp.v
                   rcs.v  = t0.v*t1.v*cos2p.v
#endif
                 end function rcs_f14167_xmm4r4
                 
                !  /*
                !!
                !        Infinitely long cylinder.
                !        Scattered fields (k0a0 sqrt(epsr*mur-sin^2(Psi) < 0.5)
                !        TM-incident E-field.
                !        Formula 4.2-48
                !    */
                
                pure function Ez_f4248_xmm4r4(E0,psi,phi,k0,z,r,      &
                                               a0,eps,mu)         result(Ez)
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: Ez_f4248_xmm4r4
                   !dir$ attributes forceinline :: Ez_f4248_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: Ez_f4248_xmm4r4
                   use mod_vecconsts, only :  v4r4_1, v4r4_0
                   type(  XMM4c4_t),   intent(in) :: E0
                   type(XMM4r4_t), intent(in) :: psi
                   type(XMM4r4_t), intent(in) :: phi
                   type(XMM4r4_t), intent(in) :: k0
                   type(XMM4r4_t), intent(in) :: z
                   type(XMM4r4_t), intent(in) :: a0
                   type(  XMM4c4_t),   intent(in) :: eps
                   type(  XMM4c4_t),   intent(in) :: mu
                   type(  XMM4c4_t) :: Ez
                   ! Locals
                     !dir$ attributes align : 16 :: C0886226925452758013649083741671 
                     !dir$ attributes align : 16 :: C078539816339744830961566084582 
                     !dir$ attributes align : 16 :: C05
                     !dir$ attributes align : 16 :: C20
                     !dir$ attributes align : 16 :: epsp1
                     !dir$ attributes align : 16 :: epsm1
                     !dir$ attributes align : 16 :: mup1
                     !dir$ attributes align : 16 :: mum1
                     !dir$ attributes align : 16 :: mul1
                     !dir$ attributes align : 16 :: mul2
                     !dir$ attributes align : 16 :: mul3
                     !dir$ attributes align : 16 :: tc0
                     !dir$ attributes align : 16 :: tc1
                     !dir$ attributes align : 16 :: tc2
                     !dir$ attributes align : 16 :: ea
                     !dir$ attributes align : 16 :: ce
                     !dir$ attributes align : 16 :: frac
                     !dir$ attributes align : 16 :: fre
                     !dir$ attributes align : 16 :: div1
                     !dir$ attributes align : 16 :: div2
                     !dir$ attributes align : 16 :: num
                     !dir$ attributes align : 16 :: k0r
                     !dir$ attributes align : 16 :: k0z
                     !dir$ attributes align : 16 :: k0a0
                     !dir$ attributes align : 16 :: cosp
                     !dir$ attributes align : 16 :: cosps
                     !dir$ attributes align : 16 :: cos2ps
                     !dir$ attributes align : 16 :: sinps
                     !dir$ attributes align : 16 :: sin2ps
                     !dir$ attributes align : 16 :: k0a02
                     !dir$ attributes align : 16 :: scosps
                   type(XMM4r4_t),  parameter :: C0886226925452758013649083741671 =   &
                                                         XMM4r4_t(0.886226925452758013649083741671_sp)
                   type(XMM4r4_t),  parameter :: C078539816339744830961566084582  =   &
                                                         XMM4r4_t(0.78539816339744830961566084582_sp)
                   type(XMM4r4_t),  parameter :: C05 =  XMM4r4_t(0.5_sp)
                   type(XMM4r4_t),  parameter :: C20 =  XMM4r4_t(2.0_sp)
                   type(  XMM4c4_t),    automatic :: epsp1,epsm1
                   type(  XMM4c4_t),    automatic :: mup1,mum1
                   type(  XMM4c4_t),    automatic :: mul1,mul2
                   type(  XMM4c4_t),    automatic :: mul3,tc0
                   type(  XMM4c4_t),    automatic :: tc1,tc2
                   type(  XMM4c4_t),    automatic :: ea,ce
                   type(  XMM4c4_t),    automatic :: frac,fre
                   type(  XMM4c4_t),    automatic :: div1,div2
                   type(  XMM4c4_t),    automatic :: num
                   type(XMM4r4_t),  automatic :: k0r,k0z,k0a0,cosp
                   type(XMM4r4_t),  automatic :: cosps,cos2ps,sinps,sin2ps
                   type(XMM4r4_t),  automatic :: k0a02,scosps
                   k0r.v   = k0.v*r.v
                   k0z.v   = k0.v*z.v
                   k0a0.v  = k0.v*a0.v
                   cosp.v  = cos(phi.v)
                   k0a02.v = C05.v*k0a0.v*k0a0.v
                   epsp1   = epsr+ v4r4_1.v
                   ea.re   =  v4r4_0.v
                   cosps.v = cos(psi.v)
                   epsm1   = epsr- v4r4_1.v
                   scosps.v= sqrt(cosps.v)
                   sinps.v = sin(psi.v)
                   mup1    = mu+ v4r4_1.v
                   cos2ps.v= cosps.v*cosps.v
                   mum1    = mu- v4r4_1.v
                   mul1    = epsr*mum1
                   sin2ps.v= sinps.v*sinps.v
                   t0.v    = k0z.v*sinps.v+k0r.v*cosps.v+ &
                                 0.78539816339744830961566084582.v
                   ea.re   = t0.v
                   t1.v    = sqrt(k0r.v)
                   ce      = cexp_xmm4c4(ea)
                   frac    = E0*scosps
                   fre     = frac*ce
                   div1    = C0886226925452758013649083741671.v*(fre/t1)
                   mul2    = epsm1*mup1
                   mul3    = epsr1*mup1
                   tc0     = epsm1*cos2ps
                   num     = mul2*sin2ps+mul1
                   div2    = num/mul3
                   tc1     = C20*(div2/cosp)
                   tc2     = k0a02*(tc0-tc1)
                   Ez      = div1*tc2
                end function Ez_f4248_xmm4r4 
                
                 ! /*
                 !!
                 !        Infinitely long cylinder.
                 !        Scattered fields (k0a0 sqrt(epsr*mur-sin^2(Psi) < 0.5)
                 !        TM-incident E-field.
                 !        Formula 4.2-49
                 !   */
                 
                pure function Eph_f4249_xmm4r4(E0,k0z,k0r,k0a0,psi,phi,eps,mu) result(Eph)
                     
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: Eph_f4249_xmm4r4
                   !dir$ attributes forceinline :: Eph_f4249_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: Eph_f4249_xmm4r4
                   use mod_vecconsts, only :  v4r4_1, v4r4_0
                   type(  XMM4c4_t),   intent(in) :: E0
                   type(XMM4r4_t), intent(in) :: k0z
                   type(XMM4r4_t), intent(in) :: k0r
                   type(XMM4r4_t), intent(in) :: k0a0
                   type(XMM4r4_t), intent(in) :: psi
                   type(XMM4r4_t), intent(in) :: phi
                   type(  XMM4c4_t),   intent(in) :: eps
                   type(  XMM4c4_t),   intent(in) :: mu
                   type(  XMM4c4_t) :: Eph
                   ! Locals
                     !dir$ attributes align : 16 :: C2506628274631000502415765284811 
                     !dir$ attributes align : 16 :: C078539816339744830961566084582
                     !dir$ attributes align : 16 :: ea
                     !dir$ attributes align : 16 :: ce
                     !dir$ attributes align : 16 :: frac
                     !dir$ attributes align : 16 :: tc0
                     !dir$ attributes align : 16 :: mul
                     !dir$ attributes align : 16 :: emum1
                     !dir$ attributes align : 16 :: epsp1
                     !dir$ attributes align : 16 :: mup1
                     !dir$ attributes align : 16 :: tc1
                     !dir$ attributes align : 16 :: den
                     !dir$ attributes align : 16 :: t0
                     !dir$ attributes align : 16 :: cosp
                     !dir$ attributes align : 16 :: sinps
                     !dir$ attributes align : 16 :: sinph
                     !dir$ attributes align : 16 :: k0a02
                     !dir$ attributes align : 16 :: cosps
                     !dir$ attributes align : 16 :: sinpsp
                   type(XMM4r4_t), parameter :: C2506628274631000502415765284811 = &
                                                         XMM4r4_t(2.506628274631000502415765284811_sp)
                   type(XMM4r4_t), parameter :: C078539816339744830961566084582  = &
                                                         XMM4r4_t(0.78539816339744830961566084582_sp)
                   type(  XMM4c4_t),   automatic :: ea,ce
                   type(  XMM4c4_t),   automatic :: frac,tc0
                   type(  XMM4c4_t),   automatic :: mul,emum1
                   type(  XMM4c4_t),   automatic :: epsp1,mup1
                   type(  XMM4c4_t),   automatic :: tc1
                   type(XMM4r4_t), automatic :: den,t0
                   type(XMM4r4_t), automatic :: cosp,sinps
                   type(XMM4r4_t), automatic :: sinph,k0a02
                   type(XMM4r4_t), automatic :: cosps,sinpsp
                   k0a02.v   = k0a0.v*k0a0.v
                   cosp.v    = cos(phi.v)
                   ea.im     = v16_v0.v
                   t0.v      = k0r.v*cosp.v
                   emum1     = eps*mu
                   den.v     = sqrt(t0.v)
                   emum1     = emum1- v4r4_1.v
                   epsp1     = eps- v4r4_1.v
                   sinps.v   = sin(psi.v)
                   mup1      = mu+ v4r4_1.v
                   sinph.v   = sin(phi.v)
                   cosps.v   = cos(psi.v)
                   t0.v      = k0z.v*sinps.v+k0r.v*cosp.v+ &
                               C078539816339744830961566084582.v
                   ea.re     = t0.v
                   ce        = cexp_xmm4c4(ea)
                   frac      = E0*ce
                   frac      = frac/den
                   tc0       = C2506628274631000502415765284811*frac*k0a02
                   mul       = epsp1*mup1
                   tc1       = emum1/mul
                   tc1       = sinpsp*tc1
                   Eph       = frac*tc1
                end function Eph_f4249_xmm4r4
                
               !   /*
               !          Infinitely long cylinder.
               !          Scattered fields (k0a0 sqrt(epsr*mur-sin^2(Psi) < 0.5)
               !          TM-incident H-field.
               !          Formula 4.2-50
               !!
               !   */
               
               pure function Hz_f4250_xmm4r4(E0,k0z,k0r,k0a0,psi,phi,eps,mu) result(Hz)
                    
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: Hz_f4250_xmm4r4
                   !dir$ attributes forceinline :: Hz_f4250_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: Hz_f4250_xmm4r4
                   use mod_vecconsts, only :  v4r4_1, v4r4_0
                   type(  XMM4c4_t),   intent(in) :: E0
                   type(XMM4r4_t), intent(in) :: k0z
                   type(XMM4r4_t), intent(in) :: k0r
                   type(XMM4r4_t), intent(in) :: k0a0
                   type(XMM4r4_t), intent(in) :: psi
                   type(XMM4r4_t), intent(in) :: phi
                   type(  XMM4c4_t),   intent(in) :: eps
                   type(  XMM4c4_t),   intent(in) :: mu
                   type(  XMM4c4_t) :: Hz
                   ! Locals
                     !dir$ attributes align : 16 :: C078539816339744830961566084582
                     !dir$ attributes align : 16 :: C000001763712109284471382861586
                     !dir$ attributes align : 16 :: ea
                     !dir$ attributes align : 16 :: ce
                     !dir$ attributes align : 16 :: frac
                     !dir$ attributes align : 16 :: tc0
                     !dir$ attributes align : 16 :: mul
                     !dir$ attributes align : 16 :: emum1
                     !dir$ attributes align : 16 :: epsp1
                     !dir$ attributes align : 16 :: mup1
                     !dir$ attributes align : 16 :: tc1
                     !dir$ attributes align : 16 :: den
                     !dir$ attributes align : 16 :: t0
                     !dir$ attributes align : 16 :: scosp
                     !dir$ attributes align : 16 :: cosp
                     !dir$ attributes align : 16 :: sinps
                     !dir$ attributes align : 16 :: sinph
                     !dir$ attributes align : 16 :: k0a02
                     !dir$ attributes align : 16 :: cosps
                     !dir$ attributes align : 16 :: sinpsp
                   type(XMM4r4_t), parameter :: C078539816339744830961566084582  = &
                                                         XMM4r4_t(0.78539816339744830961566084582_sp)
                   type(XMM4r4_t), parameter :: C000001763712109284471382861586  = &
                                                         XMM4r4_t(0.00001763712109284471382861586_sp)
                   type(  XMM4c4_t),   automatic :: ea,ce
                   type(  XMM4c4_t),   automatic :: frac,tc0
                   type(  XMM4c4_t),   automatic :: mul,emum1
                   type(  XMM4c4_t),   automatic :: epsp1,mup1
                   type(  XMM4c4_t),   automatic :: tc1
                   type(XMM4r4_t), automatic :: den,t0,scosp,cosp
                   type(XMM4r4_t), automatic :: sinps,sinph,k0a02,cosps
                   type(XMM4r4_t), automatic :: sinpsp
                   k0a02.v  = k0a0.v*k0a0.v
                   cosp.v   = cos(phi.v)
                   ea.im    =  v4r4_0.v
                   emum1    = eps*mu
                   den.v    = sqrt(k0r.v)
                   emum1    = emum1- v4r4_1.v
                   epsp1    = eps+ v4r4_1.v
                   sinps.v  = sin(psi.v)
                   mup1     = mu+ v4r4_1.v
                   sinph.v  = sin(phi.v)
                   t0.v     = k0a.v*sinps.v+k0r.v*cosp.v+  &
                              C078539816339744830961566084582.v
                   cosps.v  = cos(psi.v)
                   scosp.v  = sqrt(cosp.v)
                   sinpsp.v = sinps.v*sinph.v
                   ea.re    = t0.v
                   ce       = cexp_xmm4c4(ea)
                   ce       = scosp*ce
                   frac     = E0*ce
                   frac     = frac/den
                   tc0      = C000001763712109284471382861586*frac*k0a02
                   mul      = epsp1*mup1
                   tc1      = emum1/mul
                   tc1      = sinpsp*tc1
                   Hz       = frac*tc1
               end function Hz_f4250_xmm4r4
               
               ! /*
               !          Infinitely long cylinder.
               !          Scattered fields (k0a0 sqrt(epsr*mur-sin^2(Psi) < 0.5)
               !          TE-incident H-field.
               !          Formula 4.2-52
               !!
               !    */
               
               pure function Hz_f4252_xmm4r4(H0,psi,phi,k0r,k0z,k0a0,eps,mu) result(Hz)
               
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: Hz_f4252_xmm4r4
                   !dir$ attributes forceinline :: Hz_f4252_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: Hz_f4252_xmm4r4
                   use mod_vecconsts, only :  v4r4_1, v4r4_0
                   type(  XMM4c4_t),   intent(in) :: H0
                   type(XMM4r4_t), intent(in) :: psi
                   type(XMM4r4_t), intent(in) :: phi
                   type(XMM4r4_t), intent(in) :: k0r
                   type(XMM4r4_t), intent(in) :: k0z
                   type(XMM4r4_t), intent(in) :: k0a0
                   type(  XMM4c4_t),   intent(in) :: eps
                   type(  XMM4c4_t),   intent(in) :: mu
                   type(  XMM4c4_t) :: Hz
                   ! Locals
                     !dir$ attributes align : 16 :: C1253314137315500251207882642406
                     !dir$ attributes align : 16 :: C078539816339744830961566084582
                     !dir$ attributes align : 16 :: C20
                     !dir$ attributes align : 16 :: C05
                     !dir$ attributes align : 16 :: mul
                     !dir$ attributes align : 16 :: mul2
                     !dir$ attributes align : 16 :: mul3
                     !dir$ attributes align : 16 :: tc0
                     !dir$ attributes align : 16 :: tc1
                     !dir$ attributes align : 16 :: mum1
                     !dir$ attributes align : 16 :: epsm1
                     !dir$ attributes align : 16 :: num
                     !dir$ attributes align : 16 :: mup1
                     !dir$ attributes align : 16 :: epsp1
                     !dir$ attributes align : 16 :: mucs
                     !dir$ attributes align : 16 :: frac
                     !dir$ attributes align : 16 :: ea
                     !dir$ attributes align : 16 :: ce
                     !dir$ attributes align : 16 :: scosps
                     !dir$ attributes align : 16 :: sinps
                     !dir$ attributes align : 16 :: cosps
                     !dir$ attributes align : 16 :: k0a02
                     !dir$ attributes align : 16 :: sk0r
                     !dir$ attributes align : 16 :: cos2ps
                     !dir$ attributes align : 16 :: sin2ps
                     !dir$ attributes align : 16 :: cosp
                     !dir$ attributes align : 16 :: t0
                   type(XMM4r4_t), parameter :: C1253314137315500251207882642406 = &
                                                       XMM4r4_t(1.253314137315500251207882642406_sp)
                   type(XMM4r4_t), parameter :: C078539816339744830961566084582  = &
                                                       XMM4r4_t(0.78539816339744830961566084582_sp)
                   type(XMM4r4_t), parameter :: C20 = XMM4r4_t(2.0_sp)
                   type(XMM4r4_t), parameter :: C05 = XMM4r4_t(0.5_sp)
                   type(  XMM4c4_t),   automatic :: mul,mul2
                   type(  XMM4c4_t),   automatic :: mul3,tc0
                   type(  XMM4c4_t),   automatic :: tc1,mum1
                   type(  XMM4c4_t),   automatic :: epsm1,num
                   type(  XMM4c4_t),   automatic :: mup1,epsp1
                   type(  XMM4c4_t),   automatic :: mucs,frac
                   type(  XMM4c4_t),   automatic :: ea,ce
                   type(XMM4r4_t), automatic :: scosps,sinps
                   type(XMM4r4_t), automatic :: cosps,k0a02
                   type(XMM4r4_t), automatic :: sk0r,cos2ps
                   type(XMM4r4_t), automatic :: sin2ps,cosp
                   type(XMM4r4_t), automatic :: t0
                   cosps.v   = cos(psi.v)
                   k0a02.v   = C05.v*k0a.v*k0a.v
                   scosps.v  = sqrt(cosps.v)
                   cos2ps.v  = cosps.v*cosps.v
                   sinps.v   = sin(psi.v)
                   t0.v      = k0z.v*sinps.v+k0r.v*cosps.v+ &
                               C078539816339744830961566084582.v
                   ea.im     = v116_0.v
                   ea.re     = t0.v
                   cosp.v    = cos(phi.v)
                   ce        = cexp_xmm4c4(ea)
                   mum1      = mu- v4r4_1.v
                   frac      = H0*scosps
                   epsm1     = eps- v4r4_1.v
                   mup1      = mu+ v4r4_1.v
                   epsp1     = eps+ v4r4_1.v
                   tc0       = frac*ce
                   mucs      = mum1*cos2ps
                   tc0       = C1253314137315500251207882642406*(tc0/sk0r)
                   mul1      = epsm1*mup1
                   tc0       = tc0*k0a02
                   mul2      = epsp1*mum1
                   num       = mul2*sin2ps+mul1
                   mul3      = epsp1*mup1
                   tc1       = num/mul3
                   tc1       = C20*tc1*cosp
                   mucs      = mucs-tc1
                   Hz        = tc0*mucs
               end function Hz_f4252_xmm4r4
               
               !  /*
               !          Infinitely long cylinder.
               !          Scattered fields (k0a0 sqrt(epsr*mur-sin^2(Psi) < 0.5)
               !          TE-incident E-field.
               !          Formula 4.2-55
               !!
               !    */
               
               pure function Eph_f4255_xmm4r4(H0,psi,phi,k0r,k0z,k0a0,eps,mu) result(Ep)
                    
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: Eph_f4255_xmm4r4
                   !dir$ attributes forceinline :: Eph_f4255_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: Eph_f4255_xmm4r4
                   use mod_vecconsts, only :  v4r4_1, v4r4_0
                   type(  XMM4c4_t),   intent(in) :: H0
                   type(XMM4r4_t), intent(in) :: psi
                   type(XMM4r4_t), intent(in) :: phi
                   type(XMM4r4_t), intent(in) :: k0r
                   type(XMM4r4_t), intent(in) :: k0z
                   type(XMM4r4_t), intent(in) :: k0a0
                   type(  XMM4c4_t),   intent(in) :: eps
                   type(  XMM4c4_t),   intent(in) :: mu
                   type(  XMM4c4_t) :: Ep
                   ! Locals
                     !dir$ attributes align : 16 :: C000001763712109284471382861586
                     !dir$ attributes align : 16 :: C078539816339744830961566084582
                     !dir$ attributes align : 16 :: C20
                     !dir$ attributes align : 16 :: C05
                     !dir$ attributes align : 16 :: mul1
                     !dir$ attributes align : 16 :: mul2
                     !dir$ attributes align : 16 :: mul3
                     !dir$ attributes align : 16 :: tc0
                     !dir$ attributes align : 16 :: tc1
                     !dir$ attributes align : 16 :: mum1
                     !dir$ attributes align : 16 :: epsm1
                     !dir$ attributes align : 16 :: num
                     !dir$ attributes align : 16 :: mup1
                     !dir$ attributes align : 16 :: epsp1
                     !dir$ attributes align : 16 :: mucs
                     !dir$ attributes align : 16 :: frac
                     !dir$ attributes align : 16 :: ea
                     !dir$ attributes align : 16 :: ce
                     !dir$ attributes align : 16 :: sinps
                     !dir$ attributes align : 16 :: cosps
                     !dir$ attributes align : 16 :: k0a02
                     !dir$ attributes align : 16 :: sk0r
                     !dir$ attributes align : 16 :: cos2ps
                     !dir$ attributes align : 16 :: sin2ps
                     !dir$ attributes align : 16 :: cosp
                     !dir$ attributes align : 16 :: t0
                   type(XMM4r4_t),  parameter :: C000001763712109284471382861586 = &
                                                         XMM4r4_t(0.00001763712109284471382861586_sp)
                   type(XMM4r4_t),  parameter :: C078539816339744830961566084582 = &
                                                         XMM4r4_t(0.78539816339744830961566084582_sp)
                   type(XMM4r4_t),  parameter :: C20 =  XMM4r4_t(2.0_sp)
                   type(XMM4r4_t),  parameter :: C05 =  XMM4r4_t(0.5_sp)
                   type(  XMM4c4_t),    automatic :: mul1,mul2
                   type(  XMM4c4_t),    automatic :: mul3,tc0
                   type(  XMM4c4_t),    automatic :: tc1,mum1
                   type(  XMM4c4_t),    automatic :: epsm1,num
                   type(  XMM4c4_t),    automatic :: mup1,epsp1
                   type(  XMM4c4_t),    automatic :: mucs,frac
                   type(  XMM4c4_t),    automatic :: ea,ce
                   type(XMM4r4_t),  automatic :: sinps,cosps
                   type(XMM4r4_t),  automatic :: k0a02,sk0r
                   type(XMM4r4_t),  automatic :: cos2ps,sin2ps
                   type(XMM4r4_t),  automatic :: cosp,t0
                   k0a2.v   = C05.v*k0a.v*k0a.v
                   cosps.v  = cos(psi.v)
                   frac     = H0
                   ea.im    =  v4r4_0.v
                   sinps.v  = sin(psi.v)
                   cos2ps.v = cosps.v*cosps.v
                   t0.v     = k0z.v*sinps.v+k0r.v*cosps.v+ &
                              C078539816339744830961566084582.v
                   sk0r.v   = sqrt(k0r.v*cosps.v)
                   ea.re    = t0.v
                   ce       = cexp_xmm4c4(ea)
                   cosp.v   = cos(phi.v)
                   mum1     = mu- v4r4_1.v
                   epsm1    = eps- v4r4_1.v
                   epsp1    = eps+ v4r4_1.v
                   mup1     = mu+ v4r4_1.v
                   tc0      = frac*ce
                   mucs     = mum1*cos2ps
                   tc0      = C000001763712109284471382861586*(tc0/sk0r)
                   mul1     = epsm1*mup1
                   tc0      = tc0*k0a02
                   mul2     = epsp1*mum1
                   num      = mul2*sinps+mul1
                   mul3     = epsp1*mup1
                   tc1      = num/mul3
                   tc1      = C20*tc1*cosp
                   mucs     = mucs-tc1
                   Ep       = tc0*mucs                          
               end function Eph_f4255_xmm4r4
               
               ! /*
               !          Infinitely long cylinder.
               !          Scattered fields (k0a0 sqrt(epsr*mur-sin^2(Psi) < 0.5)
               !          TE-incident H-field.
               !          Formula 4.2-53
               ! !
               !   */
               
               pure function Hph_f4253_xmm4r4(H0,k0z,k0r,psi,phi,k0a0,eps,mu) result(Hp)
                    
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: Hph_f4253_xmm4r4
                   !dir$ attributes forceinline :: Hph_f4253_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: Hph_f4253_xmm4r4
                   use mod_vecconsts, only :  v4r4_1, v4r4_0
                   type(  XMM4c4_t),   intent(in) :: H0
                   type(XMM4r4_t), intent(in) :: k0z
                   type(XMM4r4_t), intent(in) :: k0r
                   type(XMM4r4_t), intent(in) :: psi
                   type(XMM4r4_t), intent(in) :: phi
                   type(XMM4r4_t), intent(in) :: k0a0
                   type(  XMM4c4_t),   intent(in) :: eps
                   type(  XMM4c4_t),   intent(in) :: mu
                   type(  XMM4c4_t) :: Hp
                   ! Locals
                     !dir$ attributes align : 16 :: C078539816339744830961566084582
                     !dir$ attributes align : 16 :: C2506628274631000502415765284811
                     !dir$ attributes align : 16 :: mul1
                     !dir$ attributes align : 16 :: mul2
                     !dir$ attributes align : 16 :: div
                     !dir$ attributes align : 16 :: epsp1
                     !dir$ attributes align : 16 :: mup1
                     !dir$ attributes align : 16 :: frac
                     !dir$ attributes align : 16 :: ea
                     !dir$ attributes align : 16 :: ce
                     !dir$ attributes align : 16 :: tc0
                     !dir$ attributes align : 16 :: sinps
                     !dir$ attributes align : 16 :: cosps
                     !dir$ attributes align : 16 :: scpk0r
                     !dir$ attributes align : 16 :: sinps
                     !dir$ attributes align : 16 :: k0a02
                     !dir$ attributes align : 16 :: spsph
                   type(XMM4r4_t),  parameter :: C078539816339744830961566084582 = &
                                                         XMM4r4_t(0.78539816339744830961566084582_sp)
                   type(XMM4r4_t),  parameter :: C2506628274631000502415765284811 = &
                                                         XMM4r4_t(2.506628274631000502415765284811_sp)
                   type(  XMM4c4_t),    automatic :: mul1,mul2
                   type(  XMM4c4_t),    automatic :: div,epsp1
                   type(  XMM4c4_t),    automatic :: mup1,frac
                   type(  XMM4c4_t),    automatic :: ea,ce
                   type(  XMM4c4_t),    automatic :: tc0
                   type(XMM4r4_t),  automatic :: sinps,cosps
                   type(XMM4r4_t),  automatic :: scpk0r,sinph
                   type(XMM4r4_t),  automatic :: k0a02,spsph
                   k0a02.v   = k0a.v*k0a.v
                   sinps.v   = sin(psi.v)
                   ea.im     =  v4r4_0.v
                   cosps.v   = cos(psi.v)
                   ea.re     = k0z.v*sinps.v+k0r.v*cosps.v+ &
                               C078539816339744830961566084582.v
                   sinph.v   = sin(phi.v)
                   ce        = cexp_xmm4c4(ea)
                   scpk0r.v  = sqrt(k0r.v*cosps.v)
                   frac      = H0*cer
                   spsph.v   = sinps.v*sinph.v
                   epsp1     = eps+ v4r4_1.v
                   mup1      = mu+ v4r4_1.v
                   mul1      = eps*mu
                   mul1      = mul1- v4r4_1.v
                   tc0       = C2506628274631000502415765284811*(frac/scpk0r)
                   mul2      = epsp1*mup1
                   div       = mul1/mul2
                   tc0       = tc0*k0a02
                   div       = div*spsph
                   Hp        = tc0*div
               end function Hph_f4253_xmm4r4
               
               !   /*
               !          Infinitely long cylinder.
               !          Scattered fields (k0a0 sqrt(epsr*mur-sin^2(Psi) < 0.5)
               !          TE-incident E-field.
               !          Formula 4.2-54
               !!
               !    */
               
               pure function Ez_f4254_xmm4r4(H0,k0z,k0r,psi,phi,k0a0,eps,mu) result(Hp)
                    
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: Ez_f4254_xmm4r4
                   !dir$ attributes forceinline :: Ez_f4254_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: Ez_f4254_xmm4r4
                   use mod_vecconsts, only :  v4r4_1, v4r4_0
                   type(  XMM4c4_t),   intent(in) :: H0
                   type(XMM4r4_t), intent(in) :: k0z
                   type(XMM4r4_t), intent(in) :: k0r
                   type(XMM4r4_t), intent(in) :: psi
                   type(XMM4r4_t), intent(in) :: phi
                   type(XMM4r4_t), intent(in) :: k0a0
                   type(  XMM4c4_t),   intent(in) :: eps
                   type(  XMM4c4_t),   intent(in) :: mu
                   type(  XMM4c4_t) :: Ez
                   ! Locals
                     !dir$ attributes align : 16 :: C000001763712109284471382861586
                     !dir$ attributes align : 16 :: C078539816339744830961566084582
                     !dir$ attributes align : 16 :: mul1
                     !dir$ attributes align : 16 :: mul2
                     !dir$ attributes align : 16 :: div
                     !dir$ attributes align : 16 :: epsp1
                     !dir$ attributes align : 16 :: mup1
                     !dir$ attributes align : 16 :: frac
                     !dir$ attributes align : 16 :: ea
                     !dir$ attributes align : 16 :: ce
                     !dir$ attributes align : 16 :: tc0
                     !dir$ attributes align : 16 :: sinps
                     !dir$ attributes align : 16 :: cosps
                     !dir$ attributes align : 16 :: scpk0r
                     !dir$ attributes align : 16 :: sinph
                     !dir$ attributes align : 16 :: k0a02
                     !dir$ attributes align : 16 :: spsph
                   type(XMM4r4_t),  parameter :: C000001763712109284471382861586 = &
                                                         XMM4r4_t(0.00001763712109284471382861586_sp)
                   type(XMM4r4_t),  parameter :: C078539816339744830961566084582 = &
                                                         XMM4r4_t(0.78539816339744830961566084582_sp)
                   type(  XMM4c4_t),    automatic :: mul1,mul2
                   type(  XMM4c4_t),    automatic :: div,epsp1
                   type(  XMM4c4_t),    automatic :: mup1,frac
                   type(  XMM4c4_t),    automatic :: ea,ce
                   type(  XMM4c4_t),    automatic :: tc0
                   type(XMM4r4_t),  automatic :: sinps,cosps
                   type(XMM4r4_t),  automatic :: scpk0r,sinph
                   type(XMM4r4_t),  automatic :: k0a02,spsph
                   k0a02.v  = k0a.v*k0a.v
                   sinps.v  = sin(psi.v)
                   ea.im    =  v4r4_0.v
                   cosps.v  = cos(psi.v)
                   ea.re    = k0z.v*sinps.v+k0r.v*cosps.v+ &
                              C078539816339744830961566084582.v
                   ce       = cexp_xmm4c4(ea)
                   scpk0r.v = sqrt(k0r.v)
                   frac     = H0*ce
                   sinph.v  = sin(phi.v)
                   frac     = frac*cosps
                   spsph.v  = sinps.v*sinph.v
                   mup1     = mu+ v4r4_1.v
                   epsp1    = eps+ v4r4_1.v
                   mul1     = eps*mu
                   tc0      = C000001763712109284471382861586*(frac/scpk0r)
                   mul1     = mul1- v4r4_1.v
                   mul2     = epsp1*mup1
                   div      = mul1/mul2
                   tc0      = tc0*k0a02
                   div      = div*spsph
                   Ez       = tc0*div
               end function Ez_f4254_xmm4r4
               
               ! /*
               !      Bistatic scattering width for (k0a0 sqrt(epsr*mur-sin^2(Psi) < 0.5)
               !      Infinitely long cylinder.
               !      TM-incident.
               !      Formula 4.2-56
               !  */
               
               pure function rcs_f4256_xmm4r4(a0,k0a0,psi,phi,eps,mu) result(rcs)
                    
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4256_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4256_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4256_xmm4r4
                   use mod_vecconsts, only :  v4r4_1, v4r4_0
                   type(XMM4r4_t),  intent(in) :: a0
                   type(XMM4r4_t),  intent(in) :: k0a0
                   type(XMM4r4_t),  intent(in) :: psi
                   type(XMM4r4_t),  intent(in) :: phi
                   type(  XMM4c4_t),    intent(in) :: eps
                   type(  XMM4c4_t),    intent(in) :: mu
                   type(XMM4r4_t) :: rcs
                   ! LOcals
                     !dir$ attributes align : 16 :: C9869604401089358618834490999876
                     !dir$ attributes align : 16 :: C20
                     !dir$ attributes align : 16 :: C40
                     !dir$ attributes align : 16 :: tc0
                     !dir$ attributes align : 16 :: tc1
                     !dir$ attributes align : 16 :: epsm1
                     !dir$ attributes align : 16 :: epsp1
                     !dir$ attributes align : 16 :: mum1
                     !dir$ attributes align : 16 :: mup1
                     !dir$ attributes align : 16 :: num
                     !dir$ attributes align : 16 :: epscps
                     !dir$ attributes align : 16 :: div
                     !dir$ attributes align : 16 :: mul1
                     !dir$ attributes align : 16 :: mul2
                     !dir$ attributes align : 16 :: mul3
                     !dir$ attributes align : 16 :: k0a03
                     !dir$ attributes align : 16 :: frac
                     !dir$ attributes align : 16 :: cosp
                     !dir$ attributes align : 16 :: cos2p
                     !dir$ attributes align : 16 :: cosps
                     !dir$ attributes align : 16 :: sinps
                     !dir$ attributes align : 16 :: sin2ps
                     !dir$ attributes align : 16 :: spia
                     !dir$ attributes align : 16 :: t0
                     !dir$ attributes align : 16 :: cab
                   type(XMM4r4_t),  parameter :: C9869604401089358618834490999876 = &
                                                        XMM4r4_t(9.869604401089358618834490999876_sp)
                   type(XMM4r4_t),  parameter :: C20 = XMM4r4_t(2.0_sp)
                   type(XMM4r4_t),  parameter :: C40 = XMM4r4_t(4.0_sp)
                   type(  XMM4c4_t),    automatic :: tc0,tc1
                   type(  XMM4c4_t),    automatic :: epsm1,epsp1
                   type(  XMM4c4_t),    automatic :: mum1,mup1
                   type(  XMM4c4_t),    automatic :: num,epscps
                   type(  XMM4c4_t),    automatic :: div,mul1
                   type(  XMM4c4_t),    automatic :: mul2,mul3
                   type(XMM4r4_t),  automatic :: k0a03,frac,cosp,cos2p
                   type(XMM4r4_t),  automatic :: cosps,sinps,sin2ps,spia
                   type(XMM4r4_t),  automatic :: t0,cab
                   spia.v   = a0.v*C9869604401089358618834490999876.v
                   cosps.v  = cos(psi.v)
                   k0a03.v  = k0a0.v*k0a0.v*k0a0.v
                   epsm1    = eps- v4r4_1.v
                   sinps.v  = sin(psi.v)
                   cos2ps.v = cosps.v*cosps.v
                   mum1     = mu- v4r4_1.v
                   epsp1    = eps+ v4r4_1.v
                   cosp.v   = cos(phi.v)
                   sin2ps.v = sinps.v*sinps.v
                   mup1     = mu+ v4r4_1.v
                   t0.v     = C40.v*cos2ps.v
                   frac.v   = C9869604401089358618834490999876.v/t0.v
                   epscps   = epsm1*cos2ps
                   frac.v   = frac.v*k0a03.v
                   mul1     = epsp1*mum1
                   mul2     = epsm1*mup1
                   num      = mul2*sinps*mul1
                   mul3     = epsp1*mup1
                   div      = num/mul3
                   tc0      = C20*div*cosp
                   tc1      = epscps-tc0
                   cab.v    =  cabs_xmm4c4(tc1)
                   rcs.v    = cab.v*frac.v
               end function rcs_f4256_xmm4r4
               
               !  /*
               !      Bistatic scattering width for (k0a0 sqrt(epsr*mur-sin^2(Psi) < 0.5)
               !      Infinitely long cylinder.
               !      TE-incident.
               !      Formula 4.2-58
               !  */
               
               pure function rcs_f4258_xmm4r4(a0,k0a0,psi,phi,eps,mu) result(rcs)
                   
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4258_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4258_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4258_xmm4r4
                   use mod_vecconsts, only :  v4r4_1, v4r4_0
                   type(XMM4r4_t),  intent(in) :: a0
                   type(XMM4r4_t),  intent(in) :: k0a0
                   type(XMM4r4_t),  intent(in) :: psi
                   type(XMM4r4_t),  intent(in) :: phi
                   type(  XMM4c4_t),    intent(in) :: eps
                   type(  XMM4c4_t),    intent(in) :: mu
                   type(XMM4r4_t) :: rcs
                   ! LOcals
                     !dir$ attributes align : 16 :: C9869604401089358618834490999876
                     !dir$ attributes align : 16 :: C20
                     !dir$ attributes align : 16 :: C40
                     !dir$ attributes align : 16 :: tc0
                     !dir$ attributes align : 16 :: tc1
                     !dir$ attributes align : 16 :: epsm1
                     !dir$ attributes align : 16 :: epsp1
                     !dir$ attributes align : 16 :: mum1
                     !dir$ attributes align : 16 :: mup1
                     !dir$ attributes align : 16 :: num
                     !dir$ attributes align : 16 :: mucps
                     !dir$ attributes align : 16 :: div
                     !dir$ attributes align : 16 :: mul1
                     !dir$ attributes align : 16 :: mul2
                     !dir$ attributes align : 16 :: mul3
                     !dir$ attributes align : 16 :: k0a03
                     !dir$ attributes align : 16 :: frac
                     !dir$ attributes align : 16 :: cosp
                     !dir$ attributes align : 16 :: cos2p
                     !dir$ attributes align : 16 :: cosps
                     !dir$ attributes align : 16 :: sinps
                     !dir$ attributes align : 16 :: sin2ps
                     !dir$ attributes align : 16 :: spia
                     !dir$ attributes align : 16 :: t0
                     !dir$ attributes align : 16 :: cab
                   type(XMM4r4_t),  parameter :: C9869604401089358618834490999876 = &
                                                        XMM4r4_t(9.869604401089358618834490999876_sp)
                   type(XMM4r4_t),  parameter :: C20 = XMM4r4_t(2.0_sp)
                   type(XMM4r4_t),  parameter :: C40 = XMM4r4_t(4.0_sp)
                   type(  XMM4c4_t),    automatic :: tc0,tc1
                   type(  XMM4c4_t),    automatic :: epsm1,epsp1
                   type(  XMM4c4_t),    automatic :: mum1,mup1
                   type(  XMM4c4_t),    automatic :: num,mucps
                   type(  XMM4c4_t),    automatic :: div,mul1
                   type(  XMM4c4_t),    automatic :: mul2,mul3
                   type(XMM4r4_t),  automatic :: k0a03,frac,cosp,cos2p
                   type(XMM4r4_t),  automatic :: cosps,sinps,sin2ps,spia
                   type(XMM4r4_t),  automatic :: t0,cab
                   spia.v   = a0.v*C9869604401089358618834490999876.v
                   cosps.v  = cos(psi.v)
                   k0a03.v  = k0a0.v*k0a0.v*k0a0.v
                   epsm1    = eps- v4r4_1.v
                   sinps.v  = sin(psi.v)
                   cos2ps.v = cosps.v*cosps.v
                   mum1     = mu- v4r4_1.v
                   epsp1    = eps+ v4r4_1.v
                   cosp.v   = cos(phi.v)
                   sin2ps.v = sinps.v*sinps.v
                   mup1     = mu+ v4r4_1.v
                   t0.v     = C40.v*cos2ps.v
                   frac.v   = C9869604401089358618834490999876.v/t0.v
                   mucps    = mum1*cos2ps
                   frac.v   = frac.v*k0a03.v
                   mul1     = epsp1*mum1
                   mul2     = epsm1*mup1
                   num      = mul2*sinps*mul1
                   mul3     = epsp1*mup1
                   div      = num/mul3
                   tc0      = C20*div*cosp
                   tc1      = mucps-tc0
                   cab.v    =  cabs_xmm4c4(tc1)
                   rcs.v    = cab.v*frac.v
               end function rcs_f4258_xmm4r4
               
               ! /*
               ! !
               !            Bistatic scattering width for (k0a0 sqrt(epsr*mur-sin^2(Psi) < 0.5)
               !            Infinitely long cylinder.
               !            TM-incident.
               !            Formula 4.2-57
               !      */
               
               pure function rcs_f4257_xmm4r4(a0,k0a0,psi,phi,eps,mu) result(rcs)
               
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4257_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4257_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4257_xmm4r4 
                   use mod_vecconsts, only :  v4r4_1, v4r4_0
                   type(XMM4r4_t),  intent(in) :: a0
                   type(XMM4r4_t),  intent(in) :: k0a0
                   type(XMM4r4_t),  intent(in) :: psi
                   type(XMM4r4_t),  intent(in) :: phi
                   type(  XMM4c4_t),    intent(in) :: eps
                   type(  XMM4c4_t),    intent(in) :: mu
                   type(XMM4r4_t) :: rcs
                   ! LOcals
                     !dir$ attributes align : 16 :: C9869604401089358618834490999876
                     !dir$ attributes align : 16 :: C20
                     !dir$ attributes align : 16 :: C40
                     !dir$ attributes align : 16 :: div
                     !dir$ attributes align : 16 :: mul1
                     !dir$ attributes align : 16 :: mul2
                     !dir$ attributes align : 16 :: epsp1
                     !dir$ attributes align : 16 :: mup1
                     !dir$ attributes align : 16 :: spi4
                     !dir$ attributes align : 16 :: cos2ps
                     !dir$ attributes align : 16 :: sinps
                     !dir$ attributes align : 16 :: sinp
                     !dir$ attributes align : 16 :: k0a03
                     !dir$ attributes align : 16 :: frac
                     !dir$ attributes align : 16 :: t0
                     !dir$ attributes align : 16 :: cab
                   
                   type(XMM4r4_t),  parameter :: C9869604401089358618834490999876 = &
                                                        XMM4r4_t(9.869604401089358618834490999876_sp)
                   type(XMM4r4_t),  parameter :: C20 = XMM4r4_t(2.0_sp)
                   type(XMM4r4_t),  parameter :: C40 = XMM4r4_t(4.0_sp)
                   type(  XMM4c4_t),    automatic :: div,mul1
                   type(  XMM4c4_t),    automatic :: mul2,epsp1
                   type(  XMM4c4_t),    automatic :: mup1
                   type(XMM4r4_t),  automatic :: spi4,cos2ps,sinps,sinp
                   type(XMM4r4_t),  automatic :: k0a03,frac,t0,cab
                   k0a03.v   = k0a0.v*k0a0.v*k0a0.v
                   epsp1     = eps+ v4r4_1.v
                   cosps.v   = cos(psi.v)
                   spi4.v    = C9869604401089358618834490999876.v* &
                               a0.v*a0.v
                   mup1      = mu1+ v4r4_1.v
                   cos2ps.v  = cosps.v*cosps.v
                   spi4.v    = C40.v*spi4.v
                   mul1      = eps*mu
                   sinps.v   = sin(psi.v)
                   mul1      = mul1- v4r4_1.v
                   sinp.v    = sin(phi.v)
                   mul2      = epsp1*mup1
                   frac.v    = k0a03.v*(spi4.v/cos2ps.v)
                   t0.v      = sinps.v*sinp.v
                   div       = mul1/mul2
                   div       = div*t0
                   cab       =  cabs_xmm4c4(div)
                   rcs.v     = frac.v*cab.v
               end function rcs_f4257_xmm4r4
               
               ! /*
               !        Circular cylinders of finite length.
               !        Cylinder radius small (k0a<1.0)
               !        Wire limit of cylinder (h>>a).
               !        E-field
               !        Formula 4.3-9
               !    */
               
               pure function ES_f439_xmm4r4(EI,r,k0,psii,psis,h,ln4h) result(ES)
                   
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: ES_f439_xmm4r4
                   !dir$ attributes forceinline :: ES_f439_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ES_f439_xmm4r4
                   use mod_vecconsts, only :  v4r4_1
                   type(  XMM4c4_t),    intent(in) :: EI
                   type(XMM4r4_t),  intent(in) :: r
                   type(XMM4r4_t),  intent(in) :: k0
                   type(XMM4r4_t),  intent(in) :: psii
                   type(XMM4r4_t),  intent(in) :: psis
                   type(XMM4r4_t),  intent(in) :: h
                   type(XMM4r4_t),  intent(in) :: ln4h
                   type(  XMM4c4_t) :: ES
                   ! Locals
                     !dir$ attributes align : 16 :: C0333333333333333333333333333333
                     !dir$ attributes align : 16 :: ea
                     !dir$ attributes align : 16 :: ce
                     !dir$ attributes align : 16 :: tc0
                     !dir$ attributes align : 16 :: mul
                     !dir$ attributes align : 16 :: ir
                     !dir$ attributes align : 16 :: k02
                     !dir$ attributes align : 16 :: h3
                     !dir$ attributes align : 16 :: cpsii
                     !dir$ attributes align : 16 :: cpsis
                     !dir$ attributes align : 16 :: rat
                     !dir$ attributes align : 16 :: num
                     !dir$ attributes align : 16 :: den
                   type(XMM4r4_t),  parameter :: C0333333333333333333333333333333 = &
                                                       XMM4r4_t(0.333333333333333333333333333333_sp)
                   type(  XMM4c4_t),    automatic :: ea,ce
                   type(  XMM4c4_t),    automatic :: tc0,mul
                   type(XMM4r4_t),  automatic :: ir,k02,h3
                   type(XMM4r4_t),  automatic :: cpsii,cpsis,rat
                   type(XMM4r4_t),  automatic :: num,den
                   k02.v   = C0333333333333333333333333333333.v*  &
                             k0.v*k0.v
                   cpsii.v = cos(psii.v)
                   den.v   = ln4h.v- v4r4_1.v
                   ir.v    =  v4r4_1.v/r.v
                   ea.re   = k0.v*r.v
                   cpsis.v = cos(psis.v)
                   ea.im   =  v4r4_0.v
                   h3.v    = h.v*h.v*h.v
                   ce      = cexp_xmm4c4(ea)
                   num.v   = h3.v*cpsis.v*cpsii.v
                   ce      = ce*ir
                   rat.v   = num.v/den.v
                   tc0     = EI*rat
                   mul     = ce*tc0
                   ES      = mul*k02
               end function ES_f439_xmm4r4
               
               !  /*
               !        Circular cylinders of finite length.
               !        Cylinder radius small (k0a<1.0)
               !        Wire limit of cylinder (h>>a).
               !        RCS.
               !        Formula 4.3-10
               !!
               !     */
               
               pure function rcs_f4310_xmm4r4(k0,h,psii,psis,ln4h) result(rcs)
                    
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4310_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4310_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4310_xmm4r4 
                   use mod_vecconsts, only :  v4r4_1
                   type(XMM4r4_t),   intent(in) :: k0
                   type(XMM4r4_t),   intent(in) :: h
                   type(XMM4r4_t),   intent(in) :: psii
                   type(XMM4r4_t),   intent(in) :: psis
                   type(XMM4r4_t),   intent(in) :: ln4h
                   type(XMM4r4_t) :: rcs
                   ! Locals
                     !dir$ attributes align : 16 :: C1396263401595463661538952614791
                     !dir$ attributes align : 16 :: cpsii
                     !dir$ attributes align : 16 :: cpsis
                     !dir$ attributes align : 16 :: c2psii
                     !dir$ attributes align : 16 :: c2psis
                     !dir$ attributes align : 16 :: den
                     !dir$ attributes align : 16 :: num
                     !dir$ attributes align : 16 :: t0
                     !dir$ attributes align : 16 :: k0r
                     !dir$ attributes align : 16 :: h6
                     !dir$ attributes align : 16 :: t1
                     !dir$ attributes align : 16 :: h2
                     !dir$ attributes align : 16 :: rat
                     !dir$ attributes align : 16 :: frac
                   type(XMM4r4_t),  parameter :: C1396263401595463661538952614791 = &
                                                        XMM4r4_t(1.396263401595463661538952614791_sp)
                   type(XMM4r4_t),  automatic :: cpsii,cpsis,c2psii,c2psis
                   type(XMM4r4_t),  automatic :: den,num,t0,k0r
                   type(XMM4r4_t),  automatic :: h6,t1,h2,rat
                   type(XMM4r4_t),  automatic :: frac
#if (GMS_EXPLICIT_VECTORIZE) == 1
                    integer(kind=i4) :: j
                    !dir$ loop_count(4)
                    !dir$ vector aligned
                    !dir$ vector vectorlength(4)
                    !dir$ vector always
                    do j=0, 3   
                         h2.v(j)     = h.v(j)*h.v(j)
                         k04.v(j)    = k0.v(j)*k0.v(j)*k0.v(j)*k0.v(j)
                         cpsii.v(j)  = cos(psii.v(j))
                         t0.v(j)     = ln4h.v(j)- v4r4_1.v(j)
                         c2psii.v(j) = cpsii.v(j)*cpsii.v(j)
                         den.v(j)    = t0.v(j)*t0.v(j)
                         t1.v(j)     = h.v(j)*h2.v(j)
                         h6.v(j)     = t1.v(j)*h2.v(j)
                         cpsis.v(j)  = cos(psis.v(j))
                         frac.v(j)   = C1396263401595463661538952614791.v(j)* &
                                       k04.v(j)*h6.v(j)
                         num.v(j)    = c2psis.v(j)*c2psii.v(j)
                         rat.v(j)    = num.v(j)/den.v(j)
                         rcs.v(j)    = frac.v(j)*rat.v(j)
                    end do
#else                  
                   h2.v     = h.v*h.v
                   k04.v    = k0.v*k0.v*k0.v*k0.v
                   cpsii.v  = cos(psii.v)
                   t0.v     = ln4h.v- v4r4_1.v
                   c2psii.v = cpsii.v*cpsii.v
                   den.v    = t0.v*t0.v
                   t1.v     = h.v*h2.v
                   h6.v     = t1.v*h2.v
                   cpsis.v  = cos(psis.v)
                   frac.v   = C1396263401595463661538952614791.v* &
                              k04.v*h6.v
                   num.v    = c2psis.v*c2psii.v
                   rat.v    = num.v/den.v
                   rcs.v    = frac.v*rat.v
#endif
               end function rcs_f4310_xmm4r4
               
               !   /*
               !          The average dipole scattering RCS when the incidence
               !          and scattered polarization direction coincide.
               !          Formula 4.3-11
               !     */
               
               pure function rcs_f4311_xmm4r4(k0,h,ln4h) result(rcs)
                    
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4311_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4311_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4311_xmm4r4 
                   use mod_vecconsts, only :  v4r4_1
                   type(XMM4r4_t),   intent(in) :: k0
                   type(XMM4r4_t),   intent(in) :: h
                   type(XMM4r4_t),   intent(in) :: ln4h
                   type(XMM4r4_t) :: rcs
                   ! Locals
                     !dir$ attributes align : 16 :: C0279252680319092732307790522958
                     !dir$ attributes align : 16 :: den
                     !dir$ attributes align : 16 :: inv
                     !dir$ attributes align : 16 :: k04
                     !dir$ attributes align : 16 :: h6
                     !dir$ attributes align : 16 :: t0
                     !dir$ attributes align : 16 :: t1
                   type(XMM4r4_t),  parameter :: C0279252680319092732307790522958 = &
                                                      XMM4r4_t(0.279252680319092732307790522958_sp)
                   type(XMM4r4_t),  automatic :: den,inv,k04,h6
                   type(XMM4r4_t),  automatic :: t0,t1
#if (GMS_EXPLICIT_VECTORIZE) == 1
                    integer(kind=i4) :: j
                    !dir$ loop_count(4)
                    !dir$ vector aligned
                    !dir$ vector vectorlength(4)
                    !dir$ vector always
                    do j=0, 3   
                         h2.v(j)  = h.v(j)*h.v(j)
                         k04.v(j) = k0.v(j)*k0.v(j)*k0.v(j)*k0.v(j)
                         t0.v(j)  = ln4h.v(j)- v4r4_1.v(j)
                         t1.v(j)  = h.v(j)*h2.v(j)
                         den.v(j) = t0.v(j)*t0.v(j)
                         h6.v(j)  = t1.v(j)*h2.v(j)
                         inv.v(j) =  v4r4_1.v(j)/den.v(j)
                         t0.v(j)  = C0279252680319092732307790522958.v(j)* &
                                    k04.v(j)*h6.v(j)
                         rcs.v(j) = t0.v(j)*inv.v(j)
                    end do     
#else              
                   h2.v     = h.v*h.v
                   k04.v    = k0.v*k0.v*k0.v*k0.v
                   t0.v     = ln4h.v- v4r4_1.v
                   t1.v     = h.v*h2.v
                   den.v    = t0.v*t0.v
                   h6.v     = t1.v*h2.v
                   inv.v    =  v4r4_1.v/den.v
                   t0.v     = C0279252680319092732307790522958.v* &
                              k04.v*h6.v
                   rcs.v    = t0.v*inv.v
#endif
               end function rcs_f4311_xmm4r4
               
               !  /*
               !            Disc limit of cylinder (h<<a).
               !            Scattered fields from the cylinder in the disc limit
               !            Formula 4.3-18
               !       */
               
               pure function ES_f4318_xmm4r4(EI,k0,r,psii,psis,phi,a) result(ES)
                    
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: ES_f4318_xmm4r4
                   !dir$ attributes forceinline :: ES_f4318_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ES_f4318_xmm4r4
                   use mod_vecconsts, only :  v4r4_0, v4r4_1
                   type(  XMM4c4_t),   intent(in) :: EI
                   type(XMM4r4_t), intent(in) :: k0
                   type(XMM4r4_t), intent(in) :: r
                   type(XMM4r4_t), intent(in) :: psii
                   type(XMM4r4_t), intent(in) :: psis
                   type(XMM4r4_t), intent(in) :: phi
                   type(XMM4r4_t), intent(in) :: a
                   type(  XMM4c4_t) :: ES
                   ! Locals
                     !dir$ attributes align : 16 :: C0424413181578387562050356702327
                     !dir$ attributes align : 16 :: ea
                     !dir$ attributes align : 16 :: ce
                     !dir$ attributes align : 16 :: tc0
                     !dir$ attributes align : 16 :: tc1
                     !dir$ attributes align : 16 :: ir
                     !dir$ attributes align : 16 :: a3
                     !dir$ attributes align : 16 :: k02
                     !dir$ attributes align : 16 :: cosp
                     !dir$ attributes align : 16 :: spsii
                     !dir$ attributes align : 16 :: spsis
                     !dir$ attributes align : 16 :: t0
                     !dir$ attributes align : 16 :: t1
                   type(XMM4r4_t),  parameter :: C0424413181578387562050356702327 = &
                                                        XMM4r4_t(0.424413181578387562050356702327_sp)
                   type(  XMM4c4_t),    automatic :: ea,ce
                   type(  XMM4c4_t),    automatic :: tc0,tc1
                   type(XMM4r4_t),  automatic :: ir,a3,k02,cosp
                   type(XMM4r4_t),  automatic :: spsii,spsis,t0,t1
                   a3.v     = a.v*a.v*a.v
                   ir.v     =  v4r4_1.v/r.v
                   k02.v    = k0.v*k0.v
                   spsis.v  = sin(psis.v)
                   ea.im    =  v4r4_1.v
                   ea.re    = k0.v*r0.v
                   cosp.v   = cos(phi.v)
                   t0.v     = C0424413181578387562050356702327.v* &
                              k02.v
                   ce       = cexp_xmm4c4(ea)
                   spsii.v  = sin(psii.v)
                   tc0      = t0*ce*ir
                   t1.v     = spsii.v*spsis.v*cosp.v
                   t0.v     = a3.v*t1.v
                   tc1      = EI*t0
                   ES       = tc0*tc1
               end function ES_f4318_xmm4r4
               
               !    /*
               !            Disc limit of cylinder (h<<a).
               !            Scattered fields from the cylinder in the disc limit
               !            Formula 4.3-19
               !       */
               
               pure function ES_f4319_xmm4r4(EI,k0,r,psii,phi,a) result(ES)
               
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: ES_f4319_xmm4r4
                   !dir$ attributes forceinline :: ES_f4319_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ES_f4319_xmm4r4
                   use mod_vecconsts, only :  v4r4_0, v4r4_1
                   type(  XMM4c4_t),   intent(in) :: EI
                   type(XMM4r4_t), intent(in) :: k0
                   type(XMM4r4_t), intent(in) :: r
                   type(XMM4r4_t), intent(in) :: psii
                   type(XMM4r4_t), intent(in) :: phi
                   type(XMM4r4_t), intent(in) :: a
                   type(  XMM4c4_t) :: ES
                   ! Locals
                     !dir$ attributes align : 16 :: C0424413181578387562050356702327
                     !dir$ attributes align : 16 :: ea
                     !dir$ attributes align : 16 :: ce
                     !dir$ attributes align : 16 :: tc0
                     !dir$ attributes align : 16 :: tc1
                     !dir$ attributes align : 16 :: ir
                     !dir$ attributes align : 16 :: a3
                     !dir$ attributes align : 16 :: k02
                     !dir$ attributes align : 16 :: sinp
                     !dir$ attributes align : 16 :: spsii
                     !dir$ attributes align : 16 :: t0
                     !dir$ attributes align : 16 :: t1
                   type(XMM4r4_t),  parameter :: C0424413181578387562050356702327 = &
                                                        XMM4r4_t(0.424413181578387562050356702327_sp)
                   type(  XMM4c4_t),    automatic :: ea,ce
                   type(  XMM4c4_t),    automatic :: tc0,tc1
                   type(XMM4r4_t),  automatic :: ir,a3,k02,sinp
                   type(XMM4r4_t),  automatic :: spsii,t0,t1
                   a3.v     = a.v*a.v*a.v
                   ir.v     =  v4r4_1.v/r.v
                   k02.v    = k0.v*k0.v
                   ea.im    =  v4r4_1.v
                   ea.re    = k0.v*r0.v
                   sinp.v   = sin(phi.v)
                   t0.v     = C0424413181578387562050356702327.v* &
                              k02.v
                   ce       = cexp_xmm4c4(ea)
                   spsii.v  = sin(psii.v)
                   tc0      = t0*ce*ir
                   t1.v     = spsii.v*sinp.v
                   t0.v     = a3.v*t1.v
                   tc1      = EI*t0
                   ES       = tc0*tc1
               end function ES_f4319_xmm4r4
               
               !  /*
               !            Disc limit of cylinder (h<<a).
               !            Scattered fields from the cylinder in the disc limit
               !            Formula 4.3-20
               !    */
               
               pure function ES_f4320_xmm4r4(EI,k0,r,psii,phi,a) result(ES)
               
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: ES_f4320_xmm4r4
                   !dir$ attributes forceinline :: ES_f4320_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ES_f4320_xmm4r4
                   type(  XMM4c4_t),   intent(in) :: EI
                   type(XMM4r4_t), intent(in) :: k0
                   type(XMM4r4_t), intent(in) :: r
                   type(XMM4r4_t), intent(in) :: psii
                   type(XMM4r4_t), intent(in) :: phi
                   type(XMM4r4_t), intent(in) :: a
                   type(  XMM4c4_t) :: ES
                   ES = ES_f4319_xmm4r4(EI,k0,r,psii,phi,a)
               end function ES_f4320_xmm4r4
               
               !  /*
               !            Disc limit of cylinder (h<<a).
               !            Scattered fields from the cylinder in the disc limit
               !            Formula 4.3-21
               !    */
               
               pure function ES_f4321_xmm4r4(EI,k0,r,psii,psis,phi,a) result(ES)
                   
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: ES_f4321_xmm4r4
                   !dir$ attributes forceinline :: ES_f4321_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ES_f4321_xmm4r4
                   use mod_vecconsts, only :  v4r4_1, v4r4_0
                   type(  XMM4c4_t),   intent(in) :: EI
                   type(XMM4r4_t), intent(in) :: k0
                   type(XMM4r4_t), intent(in) :: r
                   type(XMM4r4_t), intent(in) :: psii
                   type(XMM4r4_t), intent(in) :: psis
                   type(XMM4r4_t), intent(in) :: phi
                   type(XMM4r4_t), intent(in) :: a
                   type(  XMM4c4_t) :: ES
                   ! Locals
                     !dir$ attributes align : 16 :: C0424413181578387562050356702327
                     !dir$ attributes align : 16 :: ea
                     !dir$ attributes align : 16 :: ce
                     !dir$ attributes align : 16 :: tc0
                     !dir$ attributes align : 16 :: tc1
                     !dir$ attributes align : 16 :: ir
                     !dir$ attributes align : 16 :: a3
                     !dir$ attributes align : 16 :: k02
                     !dir$ attributes align : 16 :: cosp
                     !dir$ attributes align : 16 :: spsii
                     !dir$ attributes align : 16 :: spsis
                     !dir$ attributes align : 16 :: t0
                     !dir$ attributes align : 16 :: t1
                   type(XMM4r4_t),  parameter :: C0424413181578387562050356702327 = &
                                                        XMM4r4_t(0.424413181578387562050356702327_sp)
                   type(XMM4r4_t),  parameter :: C05 = XMM4r4_t(0.5_sp)
                   type(  XMM4c4_t),    automatic :: ea,ce
                   type(  XMM4c4_t),    automatic :: tc0,tc1
                   type(XMM4r4_t),  automatic :: ir,a3,k02,cosp
                   type(XMM4r4_t),  automatic :: cpsii,t0,t1,cpsis
                   a3.v   = a.v*a.v*a.v
                   cosp.v = cos(phi.v)
                   k02.v  = k0.v*k0.v 
                   ir.v   =  v4r4_1.v/r.v
                   ea.im  =  v4r4_0.v
                   cpsis.v= cos(psis.v)
                   ea.re  = k0.v*r.v
                   ce     = cexp_xmm4c4(ea)
                   t0.v   = C0424413181578387562050356702327.v* &
                            k02.v
                   cpsii.v= cos(psii.v)
                   tc0    = t0*ce*ir
                   cpsii.v= C05.v*cpsii.v
                   t1.v   = cpsii.v*cpsis.v+cosp.v
                   t0.v   = a3.v*t1.v
                   tc1    = EI*t0
                   ES     = tc0*tc1
               end function ES_f4321_xmm4r4
               
               
               !  /*
               !            Disc limit of cylinder (h<<a).
               !            Bistatic scattering RCS for cylinder in the disc limit
               !            Formula 4.3-22
               !       */
               
               pure function rcs_f4322_xmm4r4(k0,a,psii,psis,phi) result(rcs)
                   
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4322_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4322_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4322_xmm4r4
                   type(XMM4r4_t),  intent(in) :: k0
                   type(XMM4r4_t),  intent(in) :: a
                   type(XMM4r4_t),  intent(in) :: psii
                   type(XMM4r4_t),  intent(in) :: psis
                   type(XMM4r4_t),  intent(in) :: phi
                   type(XMM4r4_t)  :: rcs
                   ! Locals
                     !dir$ attributes align : 16 :: C2263536968418066997601902412409
                     !dir$ attributes align : 16 :: k04
                     !dir$ attributes align : 16 :: a6
                     !dir$ attributes align : 16 :: t0
                     !dir$ attributes align : 16 :: t1
                     !dir$ attributes align : 16 :: spsii
                     !dir$ attributes align : 16 :: spsis
                     !dir$ attributes align : 16 :: cosp
                     !dir$ attributes align : 16 :: s2psii
                     !dir$ attributes align : 16 :: s2psis
                     !dir$ attributes align : 16 :: cos2p
                     !dir$ attributes align : 16 :: t2
                   type(XMM4r4_t),  parameter :: C2263536968418066997601902412409 = &
                                                      XMM4r4_t(2.263536968418066997601902412409_sp)
                   type(XMM4r4_t),  automatic :: k04,a6,t0,t1
                   type(XMM4r4_t),  automatic :: spsii,spsis,cosp
                   type(XMM4r4_t),  automatic :: s2psii,s2psis,cos2p,t2
#if (GMS_EXPLICIT_VECTORIZE) == 1
                    integer(kind=i4) :: j
                    !dir$ loop_count(4)
                    !dir$ vector aligned
                    !dir$ vector vectorlength(4)
                    !dir$ vector always
                    do j=0, 3   
                         t0.v(j)    = k0.v(j)*k0.v(j)
                         t1.v(j)    = a.v(j)*a.v(j)
                         cosp.v(j)  = cos(phi.v(j))
                         k04.v(j)   = t0.v(j)
                         spsii.v(j) = sin(psii.v(j))
                         t2.v(j)    = C2263536968418066997601902412409.v(j)* &
                                      k04.v(j)*k04.v(j)
                         s2psii.v(j)= spsii.v(j)*spsii.v(j)
                         a6.v(j)    = t1.v(j)*t1.v(j)*t1.v(j)
                         spsis.v(j) = sin(psis.v(j))
                         s2psis.v(j)= psis.v(j)*psis.v(j)
                         t3.v(j)    = s2psii.v(j)*s2psis.v(j)*cosp.v(j)
                         rcs.v(j)   = t2.v(j)*a6.v(j)*t3.v(j)
                    end do
#else                   
                   t0.v    = k0.v*k0.v
                   t1.v    = a.v*a.v
                   cosp.v  = cos(phi.v)
                   k04.v   = t0.v
                   spsii.v = sin(psii.v)
                   t2.v    = C2263536968418066997601902412409.v* &
                            k04.v*k04.v
                   s2psii.v= spsii.v*spsii.v
                   a6.v    = t1.v*t1.v*t1.v
                   spsis.v = sin(psis.v)
                   s2psis.v= psis.v*psis.v
                   t3.v    = s2psii.v*s2psis.v*cosp.v
                   rcs.v   = t2.v*a6.v*t3.v
#endif
               end function rcs_f4322_xmm4r4
               
                !/*
                !           Disc limit of cylinder (h<<a).
                !           Bistatic scattering RCS for cylinder in the disc limit
                !           Formula 4.3-23
                !      */
                
               pure function rcs_f4323_xmm4r4(k0,a,psii,phi) result(rcs)
                   
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4323_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4323_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4323_xmm4r4
                   type(XMM4r4_t),  intent(in) :: k0
                   type(XMM4r4_t),  intent(in) :: a
                   type(XMM4r4_t),  intent(in) :: psii
                   type(XMM4r4_t),  intent(in) :: phi
                   type(XMM4r4_t)  :: rcs
                   ! Locals
                     !dir$ attributes align : 16 :: C2263536968418066997601902412409
                     !dir$ attributes align : 16 :: k04
                     !dir$ attributes align : 16 :: a6
                     !dir$ attributes align : 16 :: t0
                     !dir$ attributes align : 16 :: t1
                     !dir$ attributes align : 16 :: spsii
                     !dir$ attributes align : 16 :: sinp
                     !dir$ attributes align : 16 :: t2
                     !dir$ attributes align : 16 :: s2psii
                     !dir$ attributes align : 16 :: sin2p
                   type(XMM4r4_t),  parameter :: C2263536968418066997601902412409 = &
                                                      XMM4r4_t(2.263536968418066997601902412409_sp)
                   type(XMM4r4_t),  automatic :: k04,a6,t0,t1
                   type(XMM4r4_t),  automatic :: spsii,sinp,t2
                   type(XMM4r4_t),  automatic :: s2psii,sin2p
#if (GMS_EXPLICIT_VECTORIZE) == 1
                    integer(kind=i4) :: j
                    !dir$ loop_count(4)
                    !dir$ vector aligned
                    !dir$ vector vectorlength(4)
                    !dir$ vector always
                    do j=0, 3   
                        sinp.v(j)  = sin(phi.v(j))
                        t0.v(j)    = k0.v(j)*k0.v(j)
                        t1.v(j)    = a.v(j)*a.v(j)
                        k04.v(j)   = t0.v(j)
                        spsii.v(j) = sin(psii.v(j))
                        t2.v(j)    = C2263536968418066997601902412409.v(j)* &
                                     k04.v(j)*k04.v(j)
                        s2psii.v(j)= spsii.v(j)*spsii.v(j)
                        a6.v(j)    = t1.v(j)*t1.v(j)*t1.v(j)
                        s2psii.v(j)= spsii.v(j)*spsii.v(j)
                        t3.v(j)    = s2psii.v(j)*sinp.v(j)
                        rcs.v(j)   = t2.v(j)*a6.v(j)*t3.v(j)
                    end do
#else                   
                   sinp.v  = sin(phi.v)
                   t0.v    = k0.v*k0.v
                   t1.v    = a.v*a.v
                   k04.v   = k0.v*k0.v
                   spsii.v = sin(psii.v)
                   t2.v    = C2263536968418066997601902412409.v* &
                            k04.v*k04.v
                   s2psii.v= spsii.v*spsii.v
                   a6.v    = t1.v*t1.v*t1.v
                   s2psii.v= spsii.v*spsii.v
                   t3.v    = s2psii.v*sinp.v
                   rcs.v   = t2.v*a6.v*t3.v
#endif
               end function rcs_f4323_xmm4r4
               
               !/*
               !            Disc limit of cylinder (h<<a).
               !            Bistatic scattering RCS for cylinder in the disc limit
               !            Formula 4.3-24
               !       */
               
               pure function rcs_f4324_xmm4r4(k0,a,psis,phi) result(rcs)
                   
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4324_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4324_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4324_xmm4r4
                   type(XMM4r4_t),  intent(in) :: k0
                   type(XMM4r4_t),  intent(in) :: a
                   type(XMM4r4_t),  intent(in) :: psii
                   type(XMM4r4_t),  intent(in) :: phi
                   type(XMM4r4_t)  :: rcs
                   rcs = rcs_f4323_xmm4r4(k0,a,psis,phi)
               end function rcs_f4324_xmm4r4
               
               !  /*
               !            Disc limit of cylinder (h<<a).
               !            Bistatic scattering RCS for cylinder in the disc limit
               !            Formula 4.3-25
               !    */
               
               pure function rcs_f4325_xmm4r4(k0,a,psii,psis,phi) result(rcs)
                   
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4325_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4325_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4325_xmm4r4
                   type(XMM4r4_t),  intent(in) :: k0
                   type(XMM4r4_t),  intent(in) :: a
                   type(XMM4r4_t),  intent(in) :: psii
                   type(XMM4r4_t),  intent(in) :: psis
                   type(XMM4r4_t),  intent(in) :: phi
                   type(XMM4r4_t)  :: rcs
                   ! Locals
                     !dir$ attributes align : 16 :: C2263536968418066997601902412409
                     !dir$ attributes align : 16 :: k04
                     !dir$ attributes align : 16 :: a6
                     !dir$ attributes align : 16 :: t0
                     !dir$ attributes align : 16 :: t1
                     !dir$ attributes align : 16 :: cpsii
                     !dir$ attributes align : 16 :: cosp
                     !dir$ attributes align : 16 :: t2
                     !dir$ attributes align : 16 :: term
                     !dir$ attributes align : 16 :: cpsis
                   type(XMM4r4_t),  parameter :: C2263536968418066997601902412409 = &
                                                         XMM4r4_t(2.263536968418066997601902412409_sp)
                   type(XMM4r4_t),  parameter :: C05 =  XMM4r4_t(0.5_sp)
                   type(XMM4r4_t),  automatic :: k04,a6,t0,t1
                   type(XMM4r4_t),  automatic :: cpsii,cosp,t2
                   type(XMM4r4_t),  automatic :: term,cpsis
#if (GMS_EXPLICIT_VECTORIZE) == 1
                    integer(kind=i4) :: j
                    !dir$ loop_count(4)
                    !dir$ vector aligned
                    !dir$ vector vectorlength(4)
                    !dir$ vector always
                    do j=0, 3   
                         cosp.v(j)  = cos(phi.v(j))
                         t0.v(j)    = k0.v(j)*k0.v(j)
                         cpsis.v(j) = cos(psis.v(j))
                         t1.v(j)    = a.v(j)*a.v(j)
                         cpsii.v(j) = cos(psii.v(j))
                         k04.v(j)   = t0.v(j)*t0.v(j)
                         cpsii.v(j) = C05.v(j)*cpsii.v(j)
                         t2.v(j)    = C2263536968418066997601902412409.v(j)* &
                                      k04.v(j)*k04.v(j)
                         a6.v(j)    = t1.v(j)*t1.v(j)*t1.v(j)
                         term.v(j)  = cpsis.v(j)*cpsii.v(j)+cosp.v(j)
                         rcs.v(j)   = t2.v(j)*a6.v(j)*term.v(j)
                    end do    
#else               
                   cosp.v  = cos(phi.v)
                   t0.v    = k0.v*k0.v
                   cpsis.v = cos(psis.v)
                   t1.v    = a.v*a.v
                   cpsii.v = cos(psii.v)
                   k04.v   = t0.v*t0.v
                   cpsii.v = C05.v*cpsii.v
                   t2.v    = C2263536968418066997601902412409.v* &
                             k04.v*k04.v
                   a6.v    = t1.v*t1.v*t1.v
                   term.v  = cpsis.v*cpsii.v+cosp.v
                   rcs.v   = t2.v*a6.v*term.v
#endif
               end function rcs_f4325_xmm4r4
               
               !  /*
               !           Backscattering RCS for perfectly conducting wire.
               !           (2*h>gamma/4)
               !           Formula 4.3-29
               !!
               !      */

              !       /*
              !            Parameter a1,a2,a3 of equation 4.3-29
              !            Formula 4.3-30
              !        */
              
              pure function a1_f4330_xmm4r4(k0h,psi) result(a1)
                
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: a1_f4330_xmm4r4
                   !dir$ attributes forceinline :: a1_f4330_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: a1_f4330_xmm4r4
                   type(XMM4r4_t),   intent(in) :: k0h
                   type(XMM4r4_t),   intent(in) :: psi
                   type(XMM4r4_t) :: a1
                   ! Locals
                     !dir$ attributes align : 16 :: C20
                     !dir$ attributes align : 16 :: k0h2
                     !dir$ attributes align : 16 :: spsi
                     !dir$ attributes align : 16 :: arg
                     !dir$ attributes align : 16 :: spsi2
                     !dir$ attributes align : 16 :: sarg
                   type(XMM4r4_t),  parameter :: C20 = XMM4r4_t(2.0_sp)
                   type(XMM4r4_t),  automatic :: k0h2,spsi,arg,spsi2,sarg
#if (GMS_EXPLICIT_VECTORIZE) == 1
                    integer(kind=i4) :: j
                    !dir$ loop_count(4)
                    !dir$ vector aligned
                    !dir$ vector vectorlength(4)
                    !dir$ vector always
                    do j=0, 3   
                        k0h2.v(j)  = k0h.v(j)+k0h.v(j)
                        spsi.v(j)  = sin(psi.v(j))
                        arg.v(j)   = k0h2.v(j)*spsi.v(j)
                        spsi2.v(j) = spsi.v(j)+spsi.v(j)
                        sarg.v(j)  = sin(arg.v(j))
                        a1.v(j)    = sarg.v(j)/spsi2.v(j)
                    end do
#else                   
                   k0h2.v    = k0h.v+k0h.v
                   spsi.v  = sin(psi.v)
                   arg.v   = k0h2.v*spsi.v
                   spsi2.v = spsi.v+spsi.v
                   sarg.v  = sin(arg.v)
                   a1.v    = sarg.v/spsi2.v
#endif
              end function a1_f4330_xmm4r4
              
              
              pure function a2_f4330_xmm4r4(k0h,psi) result(a2)
                  
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: a2_f4330_xmm4r4
                   !dir$ attributes forceinline :: a2_f4330_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: a2_f4330_xmm4r4
                   use mod_vecconsts, only :  v4r4_1
                   type(XMM4r4_t),   intent(in) :: k0h
                   type(XMM4r4_t),   intent(in) :: psi
                   type(XMM4r4_t) :: a2
                   ! Locals
                     !dir$ attributes align : 16 :: spsi
                     !dir$ attributes align : 16 :: msp1
                     !dir$ attributes align : 16 :: arg
                     !dir$ attributes align : 16 :: sarg
                   type(XMM4r4_t),  automatic :: spsi,msp1,arg,sarg
#if (GMS_EXPLICIT_VECTORIZE) == 1
                    integer(kind=i4) :: j
                    !dir$ loop_count(4)
                    !dir$ vector aligned
                    !dir$ vector vectorlength(4)
                    !dir$ vector always
                    do j=0, 3   
                        spsi.v(j)  = sin(psi.v(j))
                        msp1.v(j)  =  v4r4_1.v(j)-spsi.v(j)
                        arg.v(j)   = k0h.v(j)*msp1.v(j)
                        sarg.v(j)  = sin(arg.v(j))
                        a2.v(j)    = sarg.v(j)/msp1.v(j)
                    end do
#else                   
                   spsi.v  = sin(psi.v)
                   msp1.v  =  v4r4_1.v-spsi.v
                   arg.v   = k0h.v*msp1.v
                   sarg.v  = sin(arg.v)
                   a2.v    = sarg.v/msp1.v
#endif
              end function a2_f4330_xmm4r4
              
              
              pure function a3_f4330_xmm4r4(k0h,psi) result(a3)
                  
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: a3_f4330_xmm4r4
                   !dir$ attributes forceinline :: a3_f4330_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: a3_f4330_xmm4r4
                   use mod_vecconsts, only :  v4r4_1
                   type(XMM4r4_t),   intent(in) :: k0h
                   type(XMM4r4_t),   intent(in) :: psi
                   type(XMM4r4_t) :: a3
                   a3 = a2_f4330_mm16r4(k0h,psi)
              end function a3_f4330_xmm4r4
              
              ! /*
              !            Parameter F1,F2 of equation 4.3-29
              !            Formula 4.3-31
              !        */
              
              pure function F1_f4331_xmm4r4(k0a) result(F1)
                  
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: F1_f4331_xmm4r4
                   !dir$ attributes forceinline :: F1_f4331_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: F1_f4331_xmm4r4
                   type(XMM4r4_t),   intent(in) :: k0a
                   type(XMM4r4_t) :: F1
                   ! Locals
                     !dir$ attributes align : 16 :: C9869604401089358618834490999876
                     !dir$ attributes align : 16 :: C08905
                     !dir$ attributes align : 16 :: C20
                     !dir$ attributes align : 16 :: om
                     !dir$ attributes align : 16 :: om2
                     !dir$ attributes align : 16 :: arg
                     !dir$ attributes align : 16 :: larg
                   type(XMM4r4_t),  parameter :: C9869604401089358618834490999876 = &
                                                           XMM4r4_t(9.869604401089358618834490999876_sp)
                   type(XMM4r4_t),  parameter :: C08905 = XMM4r4_t(0.8905_sp)
                   type(XMM4r4_t),  parameter :: C20    = XMM4r4_t(-2.0_sp)
                   type(XMM4r4_t),  automatic :: om,om2,arg,larg
#if (GMS_EXPLICIT_VECTORIZE) == 1
                    integer(kind=i4) :: j
                    !dir$ loop_count(4)
                    !dir$ vector aligned
                    !dir$ vector vectorlength(4)
                    !dir$ vector always
                    do j=0, 3  
                       arg.v(j)   = k0a.v(j)*C08905.v(j)
                       larg.v(j)  = log(arg.v(j))
                       om.v(j)    = C20.v(j)*larg.v(j)
                       om2.v(j)   = om.v(j)*om.v(j)
                       F1.v(j)    = om.v(j)/(om2.v(j)+ &
                                    C9869604401089358618834490999876.v(j))
                    end do
#else
                   arg.v   = k0a.v*C08905.v
                   larg.v  = log(arg.v)
                   om.v    = C20.v*larg.v
                   om2.v   = om.v*om.v
                   F1.v    = om.v/(om2.v+ &
                             C9869604401089358618834490999876.v)
#endif
             end function F1_f4331_xmm4r4
             
             
             pure function F2_f4331_xmm4r4(k0a) result(F2)
                  
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: F2_f4331_xmm4r4
                   !dir$ attributes forceinline :: F2_f4331_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: F2_f4331_xmm4r4
                   type(XMM4r4_t),   intent(in) :: k0a
                   type(XMM4r4_t) :: F2
                   ! Locals
                     !dir$ attributes align : 16 :: C9869604401089358618834490999876
                     !dir$ attributes align : 16 :: C314159265358979323846264338328
                     !dir$ attributes align : 16 :: C08905
                     !dir$ attributes align : 16 :: C20
                     !dir$ attributes align : 16 :: om
                     !dir$ attributes align : 16 :: om2
                     !dir$ attributes align : 16 :: arg
                     !dir$ attributes align : 16 :: larg
                   type(XMM4r4_t),  parameter :: C9869604401089358618834490999876 = &
                                                           XMM4r4_t(9.869604401089358618834490999876_sp)
                   type(XMM4r4_t),  parameter :: C314159265358979323846264338328  = &
                                                           XMM4r4_t(3.14159265358979323846264338328_sp)
                   type(XMM4r4_t),  parameter :: C08905 = XMM4r4_t(0.8905_sp)
                   type(XMM4r4_t),  parameter :: C20    = XMM4r4_t(-2.0_sp)
                   type(XMM4r4_t),  automatic :: om,om2,arg,larg
#if (GMS_EXPLICIT_VECTORIZE) == 1
                    integer(kind=i4) :: j
                    !dir$ loop_count(4)
                    !dir$ vector aligned
                    !dir$ vector vectorlength(4)
                    !dir$ vector always
                    do j=0, 3     
                        arg.v(j)   = k0a.v(j)*C08905.v(j)
                        larg.v(j)  = log(arg.v(j))
                        om.v(j)    = C20.v(j)*larg.v(j)
                        om2.v(j)   = om.v(j)*om.v(j)
                        F2.v(j)    = C314159265358979323846264338328.v(j)/(om2.v(j)+ &
                                     C9869604401089358618834490999876.v(j))
                    end do       
#else         
                   arg.v   = k0a.v*C08905.v
                   larg.v  = log(arg.v)
                   om.v    = C20.v*larg.v
                   om2.v   = om.v*om.v
                   F2.v    = C314159265358979323846264338328.v/(om2.v+ &
                             C9869604401089358618834490999876.v)
#endif
             end function F2_f4331_xmm4r4
             
             ! /*
             !             Parameter (helper) Lambda of equation 4.3-29
             !             Formula 4.3-34
             !         */
             
             pure function L_f4334_xmm4r4(k0h,k0a) result(L)
                 
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: L_f4334_xmm4r4
                   !dir$ attributes forceinline :: L_f4334_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: L_f4334_xmm4r4
                   use mod_vecconsts, only :  v4r4_0
                   type(XMM4r4_t),   intent(in) :: k0h
                   type(XMM4r4_t),   intent(in) :: k0a
                   type(XMM4r4_t) :: L
                     !dir$ attributes align : 16 :: C078539816339744830961566084582
                     !dir$ attributes align : 16 :: C411
                     !dir$ attributes align : 16 :: C05
                     !dir$ attributes align : 16 :: C20
                     !dir$ attributes align : 16 :: C08905
                     !dir$ attributes align : 16 :: om
                     !dir$ attributes align : 16 :: del
                     !dir$ attributes align : 16 :: ck0h
                     !dir$ attributes align : 16 :: sk0h
                     !dir$ attributes align : 16 :: t0
                     !dir$ attributes align : 16 :: ar1
                     !dir$ attributes align : 16 :: ar2
                     !dir$ attributes align : 16 :: lar1
                     !dir$ attributes align : 16 :: lar2
                   type(XMM4r4_t),   parameter :: C078539816339744830961566084582 = &
                                                             XMM4r4_t(0.78539816339744830961566084582_sp)
                   type(XMM4r4_t),   parameter :: C411    = XMM4r4_t(4.11_sp)
                   type(XMM4r4_t),   parameter :: C05     = XMM4r4_t(-0.5_sp)
                   type(XMM4r4_t),   parameter :: C20     = XMM4r4_t(-2.0_sp)
                   type(XMM4r4_t),   parameter :: C08905  = XMM4r4_t(0.8905_sp)
                   type(XMM4r4_t),   automatic :: om,del,ck0h,sk0h
                   type(XMM4r4_t),   automatic :: t0,ar1,ar2,lar1,lar2
#if (GMS_EXPLICIT_VECTORIZE) == 1
                    integer(kind=i4) :: j
                    !dir$ loop_count(4)
                    !dir$ vector aligned
                    !dir$ vector vectorlength(4)
                    !dir$ vector always
                    do j=0, 3     
                        ar1.v(j)   = k0a.v(j)*C08905.v(j)
                        ar2.v(j)   = k0h.v(j)*C411.v(j)
                        lar1.v(j)  = log(ar1.v(j))
                        lar2.v(j)  = log(ar2.v(j))
                        om.v(j)    = C20.v(j)*lar1.v(j)
                        del.v(j)   = C05.v(j)*lar2.v(j)
                        ck0h.v(j)  = cos(k0h.v(j))
                        t0.v(j)    =  v4r4_0.v(j)-om.v(j)-del.v(j)
                        sk0h.v(j)  = sin(k0h.v(j))
                        L.v(j)     = C078539816339744830961566084582.v(j)*sk0h.v(j)+ &
                                     ck0h.v(j)*t0.v(j)
                    end do
#else
                   ar1.v   = k0a.v*C08905.v
                   ar2.v   = k0h.v*C411.v
                   lar1.v  = log(ar1.v)
                   lar2.v  = log(ar2.v)
                   om.v    = C20.v*lar1.v
                   del.v   = C05.v*lar2.v
                   ck0h.v  = cos(k0h.v)
                   t0.v    =  v4r4_0.v-om.v-del.v
                   sk0h.v  = sin(k0h.v)
                   L.v     = C078539816339744830961566084582.v*sk0h.v+ &
                               ck0h.v*t0.v
#endif
             end function L_f4334_xmm4r4
             
             ! /*
             !             Parameter (helper) Sigma of equation 4.3-29
             ! !            Formula 4.3-35
             !         */
             
             pure function S_f4335_xmm4r4(k0a,k0h) result(S)
             
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: S_f4335_xmm4r4
                   !dir$ attributes forceinline :: S_f4335_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: S_f4335_xmm4r4
                   type(XMM4r4_t),   intent(in) :: k0a
                   type(XMM4r4_t),   intent(in) :: k0h
                   type(XMM4r4_t) :: S
                     !dir$ attributes align : 16 :: C078539816339744830961566084582
                     !dir$ attributes align : 16 :: C712
                     !dir$ attributes align : 16 :: C05
                     !dir$ attributes align : 16 :: ar
                     !dir$ attributes align : 16 :: lar
                     !dir$ attributes align : 16 :: sk0h
                     !dir$ attributes align : 16 :: ck0h
                     !dir$ attributes align : 16 :: t0
                
                   type(XMM4r4_t),   parameter :: C078539816339744830961566084582 = &
                                                             XMM4r4_t(0.78539816339744830961566084582_sp)
                   type(XMM4r4_t),   parameter :: C712    = XMM4r4_t(7.12_sp)
                   type(XMM4r4_t),   parameter :: C05     = XMM4r4_t(0.5_sp)
                   type(XMM4r4_t),   automatic :: ar,lar,sk0h,ck0h,t0
#if (GMS_EXPLICIT_VECTORIZE) == 1
                    integer(kind=i4) :: j
                    !dir$ loop_count(4)
                    !dir$ vector aligned
                    !dir$ vector vectorlength(4)
                    !dir$ vector always
                    do j=0, 3    
                       ar.v(j)   = C712.v(j)*k0a.v(j)
                       sk0h.v(j) = sin(k0h.v(j))
                       lar.v(j)  = log(ar.v(j))
                       t0.v(j)   = C05.v(j)*lar.v(j)
                       ck0h.v(j) = cos(k0h.v(j))
                       S.v(j)    = t0.v(j)*sk0h.v(j)+ &
                                   C078539816339744830961566084582.v(j)*ck0h.v(j)
                    end do
#else                   
                   ar.v   = C712.v*k0a.v
                   sk0h.v = sin(k0h.v)
                   lar.v  = log(ar.v)
                   t0.v   = C05.v*lar.v
                   ck0h.v = cos(k0h.v)
                   S.v    = t0.v*sk0h.v+ &
                            C078539816339744830961566084582.v*ck0h.v
#endif
             end function S_f4335_xmm4r4
             
             !  /*
             !!
             !             Parameter G1,G2 of equation 4.3-29
             !              Formula 4.3-32
             !       */
             
             pure function G2_f4332_xmm4r4(k0h,k0a) result(G2)
                 
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: G2_f4332_xmm4r4
                   !dir$ attributes forceinline :: G2_f4332_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: G2_f4332_xmm4r4
                   type(XMM4r4_t),   intent(in) :: k0h
                   type(XMM4r4_t),   intent(in) :: k0a
                   type(XMM4r4_t) :: G2
                     !dir$ attributes align : 16 :: C05
                     !dir$ attributes align : 16 :: L
                     !dir$ attributes align : 16 :: S
                     !dir$ attributes align : 16 :: num
                     !dir$ attributes align : 16 :: den
                   type(XMM4r4_t),   parameter :: C05 = XMM4r4_t(0.5_sp)
                   type(XMM4r4_t),   automatic :: L,S,num,den
                   L     = L_f4334_xmm4r4(k0h,k0a)
                   S     = S_f4334_xmm4r4(k0a,k0h)
                   num.v = C05.v/S.v
                   den.v = L.v*L.v+S.v*S.v
                   G2.v  = num.v/den.v
             end function G2_f4332_xmm4r4
             
             
             pure function G1_f4332_xmm4r4(k0h,k0a) result(G1)
                 
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: G1_f4332_xmm4r4
                   !dir$ attributes forceinline :: G1_f4332_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: G1_f4332_xmm4r4
                   type(XMM4r4_t),   intent(in) :: k0h
                   type(XMM4r4_t),   intent(in) :: k0a
                   type(XMM4r4_t) :: G1
                      !dir$ attributes align : 16 :: C314159265358979323846264338328 
                      !dir$ attributes align : 16 :: C05
                      !dir$ attributes align : 16 :: C20
                      !dir$ attributes align : 16 :: C08905
                      !dir$ attributes align : 16 :: L
                      !dir$ attributes align : 16 :: S
                      !dir$ attributes align : 16 :: om
                      !dir$ attributes align : 16 :: G2
                      !dir$ attributes align : 16 :: ln
                      !dir$ attributes align : 16 :: num
                      !dir$ attributes align : 16 :: den
                      !dir$ attributes align : 16 :: om2
                      !dir$ attributes align : 16 :: t0
                      !dir$ attributes align : 16 :: rat
                   type(XMM4r4_t),   parameter :: C314159265358979323846264338328 = &
                                                             XMM4r4_t(3.14159265358979323846264338328_sp)
                   type(XMM4r4_t),   parameter :: C05     = XMM4r4_t(0.5_sp)
                   type(XMM4r4_t),   parameter :: C20     = XMM4r4_t(-2.0_sp)
                   type(XMM4r4_t),   parameter :: C08905  = XMM4r4_t(0.8905_sp)
                   type(XMM4r4_t),   automatic :: L,S,om,G2
                   type(XMM4r4_t),   automatic :: ln,num,den
                   type(XMM4r4_t),   automatic :: om2,t0,rat
                   L     = L_f4334_xmm4r4(k0h,k0a)
                   S     = S_f4335_xmm4r4(k0a,k0h)
                   ln.v  = log(k0a.v*C08905.v)
                   om.v  = C20.v*ln.v
                   G2    = G2_f4332_xmm4r4(k0h,k0a)
                   om2.v = om.v+om.v
                   num.v = C50.v*L.v
                   t0.v  = C314159265358979323846264338328.v*G2.v
                   ln.v  = t0.v/om2.v
                   den.v = L.v*L.v+S.v*S.v
                   rat.v = num.v*den.v
                   G1.v  = rat.v-ln.v
             end function G1_f4332_xmm4r4
             
             !   /*
             !!
             !              Parameter H1,H2 of equation 4.3-29
             !              Formula 4.3-33
             !       */
             
             pure function H2_f4333_xmm4r4(k0h,k0a) result(H2)
                 
                 
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: H2_f4333_xmm4r4
                   !dir$ attributes forceinline :: H2_f4333_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: H2_f4333_xmm4r4
                   type(XMM4r4_t),   intent(in) :: k0h
                   type(XMM4r4_t),   intent(in) :: k0a
                   type(XMM4r4_t) :: H2
                      !dir$ attributes align : 16 :: C157079632679489661923132169164
                      !dir$ attributes align : 16 :: C05
                      !dir$ attributes align : 16 :: L
                      !dir$ attributes align : 16 :: S
                      !dir$ attributes align : 16 :: num
                      !dir$ attributes align : 16 :: den
                      !dir$ attributes align : 16 :: arg
                   type(XMM4r4_t),   parameter :: C157079632679489661923132169164 = &
                                                          XMM4r4_t(1.57079632679489661923132169164_sp)
                   type(XMM4r4_t),   parameter :: C05     = XMM4r4_t(0.5_sp)
                   type(XMM4r4_t),   automatic :: L,S,num,den,arg
                   arg.v = C157079632679489661923132169164.v*k0h.v
                   L     = L_f4334_xmm4r4(k0h,k0a)
                   S     = S_f4335_xmm4r4(k0a,k0h)
                   num.v = C05.v*S.v
                   den.v = L.v*L.v+S.v*S.v
                   H2.v  = num.v/den.v
             end function H2_f4333_xmm4r4
             
             
             pure function H1_f4333_xmm4r4(k0a,k0h) result(H1)
                 
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: H1_f4333_xmm4r4
                   !dir$ attributes forceinline :: H1_f4333_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: H1_f4333_xmm4r4
                   type(XMM4r4_t),   intent(in) :: k0h
                   type(XMM4r4_t),   intent(in) :: k0a
                   type(XMM4r4_t) :: H1
                     !dir$ attributes align : 16 :: C314159265358979323846264338328
                     !dir$ attributes align : 16 :: C157079632679489661923132169164
                     !dir$ attributes align : 16 :: C05  
                     !dir$ attributes align : 16 :: C20
                     !dir$ attributes align : 16 :: C08905
                     !dir$ attributes align : 16 :: H2
                     !dir$ attributes align : 16 :: om
                     !dir$ attributes align : 16 :: ar
                     !dir$ attributes align : 16 :: lar
                     !dir$ attributes align : 16 :: L
                     !dir$ attributes align : 16 :: S
                     !dir$ attributes align : 16 :: num
                     !dir$ attributes align : 16 :: den
                     !dir$ attributes align : 16 :: om2
                     !dir$ attributes align : 16 :: t0
                     !dir$ attributes align : 16 :: arg
                   type(XMM4r4_t),   parameter :: C314159265358979323846264338328 = &
                                                             XMM4r4_t(3.14159265358979323846264338328_sp)
                   type(XMM4r4_t),   parameter :: C157079632679489661923132169164 = &
                                                          XMM4r4_t(1.57079632679489661923132169164_sp)
                   type(XMM4r4_t),   parameter :: C05     = XMM4r4_t(0.5_sp)
                   type(XMM4r4_t),   parameter :: C20     = XMM4r4_t(-2.0_sp)
                   type(XMM4r4_t),   parameter :: C08905  = XMM4r4_t(0.8905_sp)
                   type(XMM4r4_t),   automatic :: H2,om,ar,lar
                   type(XMM4r4_t),   automatic :: L,S,num,den
                   type(XMM4r4_t),   automatic :: om2,t0,arg
                   ar.v   = k0a.v*C08905.v
                   arg.v  = k0h.v*C157079632679489661923132169164.v
                   lar.v  = log(ar.v)
                   S      = S_f4335_xmm4r4(k0a,k0h)
                   om.v   = C20.v*lar.v
                   om2.v  = om.v*om.v
                   L      = L_f4334_xmm4r4(k0h,k0a)
                   H2     = H2_f4333_xmm4r4(k0h,k0a)
                   num.v  = C05.v*L.v
                   t0.v   = (3.14159265358979323846264338328.v*H2.v)/ &
                             om2.v
                   den.v  = L.v*L.v+S.v*S.v
                   ar.v   = num.v/den.v
                   H1.v   = ar.v-t0.v    
             end function H1_f4333_xmm4r4
             
             ! /*
             !             Backscattering RCS for perfectly conducting wire.
             !             (2*h>gamma/4)
             !             Formula 4.3-29
             !!
             !        */
             
             pure function rcs_f4329_xmm4r4(k0,gami,gams,k0h,k0a,psi) result(rcs)
                 
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4329_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4329_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4329_xmm4r4
                   type(XMM4r4_t),  intent(in) :: k0
                   type(XMM4r4_t),  intent(in) :: gami
                   type(XMM4r4_t),  intent(in) :: gams
                   type(XMM4r4_t),  intent(in) :: k0h
                   type(XMM4r4_t),  intent(in) :: k0a
                   type(XMM4r4_t),  intent(in) :: psi
                   type(XMM4r4_t) :: rcs
                   ! Locals
                     !dir$ attributes align : 16 :: C50265482457436691815402294132472
                     !dir$ attributes align : 16 :: C20
                     !dir$ attributes align : 16 :: a1
                     !dir$ attributes align : 16 :: a2
                     !dir$ attributes align : 16 :: a3
                     !dir$ attributes align : 16 :: F1
                     !dir$ attributes align : 16 :: F2
                     !dir$ attributes align : 16 :: G1
                     !dir$ attributes align : 16 :: G2
                     !dir$ attributes align : 16 :: H2
                     !dir$ attributes align : 16 :: H1
                     !dir$ attributes align : 16 :: frst
                     !dir$ attributes align : 16 :: cgami
                     !dir$ attributes align : 16 :: cgams
                     !dir$ attributes align : 16 :: c2gami
                     !dir$ attributes align : 16 :: c2gams
                     !dir$ attributes align : 16 :: sinps
                     !dir$ attributes align : 16 :: arg
                     !dir$ attributes align : 16 :: sarg
                     !dir$ attributes align : 16 :: carg
                     !dir$ attributes align : 16 :: t0
                     !dir$ attributes align : 16 :: t1
                     !dir$ attributes align : 16 :: t2
                     !dir$ attributes align : 16 :: t3
                     !dir$ attributes align : 16 :: t4
                     !dir$ attributes align : 16 :: x0
                     !dir$ attributes align : 16 :: x1
                     !dir$ attributes align : 16 :: t5
                     !dir$ attributes align : 16 :: b0
                     !dir$ attributes align : 16 :: a1s
                     !dir$ attributes align : 16 :: F1F2
                     !dir$ attributes align : 16 :: G1G2
                     !dir$ attributes align : 16 :: a2pa3
                     !dir$ attributes align : 16 :: a2ma3
                     !dir$ attributes align : 16 :: H1H2
                     !dir$ attributes align : 16 :: a2sma3s
                     !dir$ attributes align : 16 :: GHGH
                     !dir$ attributes align : 16 :: a21
                     !dir$ attributes align : 16 :: FGFG
                     !dir$ attributes align : 16 :: FHFH
                     !dir$ attributes align : 16 :: tmp1
                     !dir$ attributes align : 16 :: tmp2
                     !dir$ attributes align : 16 :: tmp3
                   type(XMM4r4_t),  parameter  :: C50265482457436691815402294132472 = &
                                                          XMM4r4_t(50.265482457436691815402294132472_sp)
                   type(XMM4r4_t),  parameter  :: C20 =  XMM4r4_t(2.0_sp)
                   type(XMM4r4_t),  automatic  :: a1,a2,a3
                   type(XMM4r4_t),  automatic  :: F1,F2,G1
                   type(XMM4r4_t),  automatic  :: G2,H2,H1
                   type(XMM4r4_t),  automatic  :: frst,cgami,cgams
                   type(XMM4r4_t),  automatic  :: c2gami,c2gams,sinps
                   type(XMM4r4_t),  automatic  :: arg,sarg,carg
                   type(XMM4r4_t),  automatic  :: t0,t1,t2
                   type(XMM4r4_t),  automatic  :: t3,t4,x0
                   type(XMM4r4_t),  automatic  :: x1,t5,b0
                   type(XMM4r4_t),  automatic  :: a1s,F1F2,G1G2
                   type(XMM4r4_t),  automatic  :: a2pa3,a2ma3,H1H2
                   type(XMM4r4_t),  automatic  :: a2sma3s,GHGH,a21
                   type(XMM4r4_t),  automatic  :: FGFG,FHFH,tmp1
                   type(XMM4r4_t),  automatic  :: tmp2,tmp3
                   a1       = a1_f4330_xmm4r4(k0h,psi)
                   b0.v     = C50265482457436691815402294132472.v/ &
                              k0.v*k0.v
                   a21.v    = a1.v*a1.v
                   cgami.v  = cos(gami.v)
                   F1       = F1_f4331_xmm4r4(k0a)
                   c2gami.v = cgami.v*cgami.v
                   F2       = F2_f4331_xmm4r4(k0a)
                   cgams.v  = cos(gams.v)
                   G1       = G1_f4332_xmm4r4(k0h,k0a)
                   c2gams.v = cgams.v*cgams.v
                   a2.v     = a2_f4330_xmm4r4(k0h,psi)
                   frst.v   = b0.v*c2gami.v*c2gams.v
                   G2       = G1_f4332_xmm4r4(k0h,k0a)
                   sinps.v  = sin(psi.v)
                   a3       = a3_f4330_xmm4r4(k0h,psi)
                   H1       = H1_f4333_xmm4r4(k0h,k0a)
                   arg.v    = k0h.v*sinps.v
                   H2       = H2_f4333_xmm4r4(k0h,k0a)
                   sarg.v   = sin(arg.v)
                   a1s.v    = a1.v*a1.v
                   carg.v   = cos(arg.v)
                   x0.v     = a2.v+a3.v
                   a2pa3.v  = x0.v*x0.v
                   F1F2.v   = F1.v*F1.v+F2.v*F2.v
                   x1.v     = a2.v-a3.v
                   t0.v     = a1s.v*F1F2.v
                   a2ma3.v  = x1.v*x1.v
                   G1G2.v   = G1.v*G1.v+G2.v*G2.v
                   t1.v     = a2pa3.v*G1G2.v*carg.v
                   x0.v     = sarg.v*sarg.v
                   H1H2.v   = H1.v*H1.v+H2.v*H2.v
                   t2.v     = a2ma3.v*H1H2.v*x0.v
                   a2sma3s.v= C20.v*(a2.v*a2.v-a3.v*a3.v)
                   GHGH.v   = G1.v*H1.v+G2.v*H2.v
                   x1.v     = carg.v*sarg.v
                   t3.v     = a2sma3s.v*GHGH.v*x1.v
                   x0.v     = a21.v*a2pa3.v
                   FGFG.v   = F1.v*G1.v+F2.v*G2.v
                   t4.v     = x0.v*FGFG.v*carg.v
                   x1.v     = a21.v*a2ma3.v
                   FHFH.v   = F1.v*H1.v+F2.v*H2.v
                   t5.v     = x1.v*FHFH.v*sarg.v
                   tmp1.v   = t0.v+t1.v+t2.v
                   tmp2.v   = (t3.v+t4.v)-t5.v
                   tmp3.v   = tmp1.v-tmp2.v
                   rcs.v    = frst.v*tmp3.v
             end function rcs_f4329_xmm4r4


             ! /*
             !!
             !            Simplified back and bistatic scattering RCS for
             !            half and full-wave dipole (2*h == gam0/2, and gam0)
             !            gam0 -- wavelength.
             !       */
             
             pure function rcs_f4337_xmm4r4(gammi,gamms,psii,psis,g0) result(rcs)
                  
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4337_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4337_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4337_xmm4r4
                   type(XMM4r4_t),   intent(in) :: gammi
                   type(XMM4r4_t),   intent(in) :: gamms
                   type(XMM4r4_t),   intent(in) :: psii
                   type(XMM4r4_t),   intent(in) :: g0
                   type(XMM4r4_t) :: rcs
                     !dir$ attributes align : 16 :: C157079632679489661923132169164
                     !dir$ attributes align : 16 :: cgami
                     !dir$ attributes align : 16 :: cgams
                     !dir$ attributes align : 16 :: c2gami
                     !dir$ attributes align : 16 :: c2gams
                     !dir$ attributes align : 16 :: t0
                     !dir$ attributes align : 16 :: carg1
                     !dir$ attributes align : 16 :: carg2
                     !dir$ attributes align : 16 :: spsii
                     !dir$ attributes align : 16 :: spsis
                     !dir$ attributes align : 16 :: cpsii
                     !dir$ attributes align : 16 :: cpsis
                     !dir$ attributes align : 16 :: rat1
                     !dir$ attributes align : 16 :: rat2
                     !dir$ attributes align : 16 :: t1
                     !dir$ attributes align : 16 :: c1
                     !dir$ attributes align : 16 :: c2
                     !dir$ attributes align : 16 :: tmp0
                     !dir$ attributes align : 16 :: tmp1
                   type(XMM4r4_t),   parameter :: C157079632679489661923132169164 = 
                                                        XMM4r4_t(1.57079632679489661923132169164_sp)
                   type(XMM4r4_t),   automatic :: cgami,cgams
                   type(XMM4r4_t),   automatic :: c2gami,c2gams
                   type(XMM4r4_t),   automatic :: t0,carg1
                   type(XMM4r4_t),   automatic :: carg2,spsii
                   type(XMM4r4_t),   automatic :: spsis,cpsii
                   type(XMM4r4_t),   automatic :: cpsis,rat1
                   type(XMM4r4_t),   automatic :: rat2,t1
                   type(XMM4r4_t),   automatic :: c1,c2
                   type(XMM4r4_t),   automatic :: tmp0,tmp1
#if (GMS_EXPLICIT_VECTORIZE) == 1
                    integer(kind=i4) :: j
                    !dir$ loop_count(4)
                    !dir$ vector aligned
                    !dir$ vector vectorlength(4)
                    !dir$ vector always
                    do j=0, 3  
                        spsii.v(j)   = sin(psii.v(j))
                        spsis.v(j)   = sin(psis.v(j))
                        cpsii.v(j)   = cos(psii.v(j))
                        carg1.v(j)   = C157079632679489661923132169164.v(j)* &
                                       spsii.v(j)
                        cpsis.v(j)   = cos(psis.v(j))
                        carg2.v(j)   = C157079632679489661923132169164.v(j)* &
                                       spsis.v(j)
                        cgams.v(j)   = cos(gamms.v)
                        c2gams.v(j)  = cgams.v(j)*cgams.v(j)
                        cgami.v(j)   = cos(gammi.v(j))
                        c2gami.v(j)  = cgammi.v(j)*cgammi.v(j)
                        t0.v(j)      = g0.v(j)*c2gami.v(j)*c2gams.v(j)
                        c1.v(j)      = cos(carg1.v(j))
                        rat1.v(j)    = c1.v(j)/cpsii.v(j)
                        tmp0.v(j)    = rat1.v(j)*rat1.v(j)
                        c2.v(j)      = cos(carg2.v(j))
                        rat2.v(j)    = c2.v(j)/cpsis.v(j)
                        tmp1.v(j)    = rat2.v(j)*rat2.v(j)
                        t1.v(j)      = tmp0.v(j)*tmp1.v(j)
                        rcs.v(j)     = t0.v(j)*t1.v(j)
                    end do
#else                     
                   spsii.v   = sin(psii.v)
                   spsis.v   = sin(psis.v)
                   cpsii.v   = cos(psii.v)
                   carg1.v   = C157079632679489661923132169164.v* &
                               spsii.v
                   cpsis.v   = cos(psis.v)
                   carg2.v   = C157079632679489661923132169164.v* &
                               spsis.v
                   cgams.v   = cos(gamms.v)
                   c2gams.v  = cgams.v*cgams.v
                   cgami.v   = cos(gammi.v)
                   c2gami.v  = cgammi.v*cgammi.v
                   t0.v      = g0.v*c2gami.v*c2gams.v
                   c1.v      = cos(carg1.v)
                   rat1.v    = c1.v/cpsii.v
                   tmp0.v    = rat1.v*rat1.v
                   c2.v      = cos(carg2.v)
                   rat2.v    = c2.v/cpsis.v
                   tmp1.v    = rat2.v*rat2.v
                   t1.v      = tmp0.v*tmp1.v
                   rcs.v     = t0.v*t1.v
#endif
             end function rcs_f4337_xmm4r4
             
             !  /*
             !!
             !            Simplified back and bistatic scattering RCS for
             !            Full-wave dipole (2*h == gam0)
             !            gam0 -- wavelength.
             !       */
             
             pure function rcs_f4340_xmm4r4(gammi,gamms,psii,psis,g0) result(rcs)
                  
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4337_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4337_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4337_xmm4r4
                   type(XMM4r4_t),   intent(in) :: gammi
                   type(XMM4r4_t),   intent(in) :: gamms
                   type(XMM4r4_t),   intent(in) :: psii
                   type(XMM4r4_t),   intent(in) :: psis
                   type(XMM4r4_t),   intent(in) :: g0
                   type(XMM4r4_t) :: rcs
                     !dir$ attributes align : 16 :: C314159265358979323846264338328
                     !dir$ attributes align : 16 :: cgami
                     !dir$ attributes align : 16 :: cgams
                     !dir$ attributes align : 16 :: c2gami
                     !dir$ attributes align : 16 :: c2gams
                     !dir$ attributes align : 16 :: t0
                     !dir$ attributes align : 16 :: carg1
                     !dir$ attributes align : 16 :: carg2
                     !dir$ attributes align : 16 :: spsii
                     !dir$ attributes align : 16 :: spsis
                     !dir$ attributes align : 16 :: cpsii
                     !dir$ attributes align : 16 :: cpsis
                     !dir$ attributes align : 16 :: rat1
                     !dir$ attributes align : 16 :: rat2
                     !dir$ attributes align : 16 :: t1
                     !dir$ attributes align : 16 :: c1
                     !dir$ attributes align : 16 :: c2
                     !dir$ attributes align : 16 :: tmp0
                     !dir$ attributes align : 16 :: tmp1
                   type(XMM4r4_t),   parameter :: C314159265358979323846264338328 = &
                                                        XMM4r4_t(3.14159265358979323846264338328_sp)
                   type(XMM4r4_t),   automatic :: cgami,cgams,c2gami,c2gams
                   type(XMM4r4_t),   automatic :: t0,carg1,carg2,spsii
                   type(XMM4r4_t),   automatic :: spsis,cpsii,cpsis,rat1
                   type(XMM4r4_t),   automatic :: rat2,t1,c1,c2
                   type(XMM4r4_t),   automatic :: tmp0,tmp1
#if (GMS_EXPLICIT_VECTORIZE) == 1
                    integer(kind=i4) :: j
                    !dir$ loop_count(4)
                    !dir$ vector aligned
                    !dir$ vector vectorlength(4)
                    !dir$ vector always
                    do j=0, 3
                         spsii.v(j)   = sin(psii.v(j))
                         spsis.v(j)   = sin(psis.v(j))
                         cpsii.v(j)   = cos(psii.v(j))
                         carg1.v(j)   = C314159265358979323846264338328.v(j)* &
                                       spsii.v(j)
                         cpsii.v(j)   = cpsii.v(j)*cpsii.v(j)
                         cpsis.v(j)   = cos(psis.v(j))
                         carg2.v(j)   = C314159265358979323846264338328.v(j)* &
                                       spsis.v(j)
                         cpsis.v(j)   = cpsis.v(j)*cpsis.v(j)
                         cgams.v(j)   = cos(gams.v(j) )
                         c2gams.v(j)  = cgams.v(j) *cgams.v(j) 
                         cgami.v(j)   = cos(gammi.v(j))
                         c2gami.v(j)  = cgami.v(j)*cgami.v(j) 
                         t0.v(j)      = g0.v(j)*c2gami.v(j)*c2gams.v(j) 
                         c1.v(j)      = sin(carg1.v(j))
                         rat1.v(j)    = c1.v(j)/cpsii.v(j)
                         tmp0.v(j)    = rat1.v(j)*rat1.v(j)
                         c2.v(j)      = sin(carg2.v(j))
                         rat2.v(j)    = c2.v(j)/cpsis.v(j) 
                         tmp1.v(j)    = rat2.v(j)*rat2.v(j) 
                         t1.v(j)      = tmp0.v(j)*tmp1.v(j) 
                         rcs.v(j)     = t0.v(j)*t1.v(j) 
                    end do
#else                   
                   spsii.v  = sin(psii.v)
                   spsis.v  = sin(psis.v)
                   cpsii.v  = cos(psii.v)
                   carg1.v  = C314159265358979323846264338328.v* &
                              spsii.v
                   cpsii.v  = cpsii.v*cpsii.v
                   cpsis.v  = cos(psis.v)
                   carg2.v  = C314159265358979323846264338328.v* &
                              spsis.v
                   cpsis.v  = cpsis.v*cpsis.v
                   cgams.v  = cos(gams.v)
                   c2gams.v = cgams.v*cgams.v
                   cgami.v  = cos(gammi.v)
                   c2gami.v = cgami.v*cgami.v
                   t0.v     = g0.v*c2gami.v*c2gams.v
                   c1.v     = sin(carg1.v)
                   rat1.v   = c1.v/cpsii.v
                   tmp0.v   = rat1.v*rat1.v
                   c2.v     = sin(carg2.v)
                   rat2.v   = c2.v/cpsis.v
                   tmp1.v   = rat2.v*rat2.v
                   t1.v     = tmp0.v*tmp1.v
                   rcs.v    = t0.v*t1.v
#endif
             end function rcs_f4340_xmm4r4
             
            !   /*
            !               Cylinder length much greater then wavelength (h>>gamma).
            !               Biscattering RCS, formula 4.3-43
            !          */
            
            pure function rcs_f4343_xmm4r4(rcsi,k0,h0,psis,psii) result(rcs)
                 
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4343_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4343_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4343_xmm4r4
                   type(XMM4r4_t),   intent(in) :: rcsi
                   type(XMM4r4_t),   intent(in) :: k0
                   type(XMM4r4_t),   intent(in) :: psis
                   type(XMM4r4_t),   intent(in) :: psii
                   type(XMM4r4_t)  :: rcs
                     !dir$ attributes align : 16 :: C314159265358979323846264338328
                     !dir$ attributes align : 16 :: C40
                     !dir$ attributes align : 16 :: k0h
                     !dir$ attributes align : 16 :: x0
                     !dir$ attributes align : 16 :: term1
                     !dir$ attributes align : 16 :: cpsis
                     !dir$ attributes align : 16 :: c2psis
                     !dir$ attributes align : 16 :: term2
                     !dir$ attributes align : 16 :: spsii
                     !dir$ attributes align : 16 :: spsis
                     !dir$ attributes align : 16 :: arg
                     !dir$ attributes align : 16 :: sarg
                     !dir$ attributes align : 16 :: rat
                   type(XMM4r4_t),   parameter :: C314159265358979323846264338328 = &
                                                        XMM4r4_t(3.14159265358979323846264338328_sp)
                   type(XMM4r4_t),   parameter :: C40 = XMM4r4_t(4.0_sp)
                   type(XMM4r4_t),   automatic :: k0h,x0,term1
                   type(XMM4r4_t),   automatic :: cpsis,c2psis,term2
                   type(XMM4r4_t),   automatic :: spsii,spsis,arg
                   type(XMM4r4_t),   automatic :: sarg,rat
#if (GMS_EXPLICIT_VECTORIZE) == 1
                    integer(kind=i4) :: j
                    !dir$ loop_count(4)
                    !dir$ vector aligned
                    !dir$ vector vectorlength(4)
                    !dir$ vector always
                    do j=0, 3
                         k0h.v(j)   = C40.v(j)*k0.v(j)*h.v(j)
                         x0.v(j)    = k0h.v(j)*k0h.v(j)
                         cpsis.v(j) = cos(psis.v(j))
                         term1.v(j) = x0.v(j)/C314159265358979323846264338328.v(j)
                         c2psis.v(j)= cpsis.v(j)*cpsis.v(j)
                         term1.v(j) = term1.v(j)*c2psis.v(j)*rcsi.v(j)
                         spsis.v(j) = sin(psis.v(j))
                         spsii.v(j) = sin(psii.v(j))
                         x0.v(j)    = spsis.v(j)*spsii.v(j)
                         arg.v(j)   = k0.v(j)*x0.v(j)*h.v(j)
                         sarg.v(j)  = sin(arg.v(j))
                         rat.v(j)   = sarg.v(j)/arg.v(j)
                         term2.v(j) = rat.v(j)*rat.v(j)
                         rcs.v(j)   = term1.v(j)*term2.v(j) 
                    end do    
#else               
                   k0h.v   = C40.v*k0.v*h.v
                   x0.v    = k0h.v*k0h.v
                   cpsis.v = cos(psis.v)
                   term1.v = x0.v/C314159265358979323846264338328.v
                   c2psis.v= cpsis.v*cpsis.v
                   term1.v = term1.v*c2psis.v*rcsi.v
                   spsis.v = sin(psis.v)
                   spsii.v = sin(psii.v)
                   x0.v    = spsis.v*spsii.v
                   arg.v   = k0.v*x0.v*h.v
                   sarg.v  = sin(arg.v)
                   rat.v   = sarg.v/arg.v
                   term2.v = rat.v*rat.v
                   rcs.v   = term1.v*term2.v
#endif
            end function rcs_f4343_xmm4r4
            
            ! /*
            !             General bistatic scattering RCS from long thin wire.
            ! !            Formula 4.3-44
            !        */
            
            pure function rcs_f4344_xmm4r4(h,k0,k0a,psii,psis,gams,gami) result(rcs)
            
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4344_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4344_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4344_xmm4r4
                   use mod_vecconsts, only :  v4r4_1
                   type(XMM4r4_t),  intent(in) :: h
                   type(XMM4r4_t),  intent(in) :: h0
                   type(XMM4r4_t),  intent(in) :: k0a
                   type(XMM4r4_t),  intent(in) :: psii
                   type(XMM4r4_t),  intent(in) :: psis
                   type(XMM4r4_t),  intent(in) :: gams
                   type(XMM4r4_t),  intent(in) :: gami
                   type(XMM4r4_t) :: rcs
                     !dir$ attributes align : 16 :: C12566370614359172953850573533118
                     !dir$ attributes align : 16 :: C2467401100272339654708622749969
                     !dir$ attributes align : 16 :: c08905
                     !dir$ attributes align : 16 :: term1
                     !dir$ attributes align : 16 :: term2
                     !dir$ attributes align : 16 :: term3
                     !dir$ attributes align : 16 :: cgami
                     !dir$ attributes align : 16 :: cgams
                     !dir$ attributes align : 16 :: c2gami
                     !dir$ attributes align : 16 :: c2gams
                     !dir$ attributes align : 16 :: inv
                     !dir$ attributes align : 16 :: arg
                     !dir$ attributes align : 16 :: sarg
                     !dir$ attributes align : 16 :: rat1
                     !dir$ attributes align : 16 :: rat2
                     !dir$ attributes align : 16 :: x0
                     !dir$ attributes align : 16 :: x1
                     !dir$ attributes align : 16 :: larg
                     !dir$ attributes align : 16 :: cpsii
                     !dir$ attributes align : 16 :: cpsis
                     !dir$ attributes align : 16 :: fac
                     !dir$ attributes align : 16 :: c2psii
                     !dir$ attributes align : 16 :: c2psis
                     !dir$ attributes align : 16 :: spsii
                     !dir$ attributes align : 16 :: spsis
                   type(XMM4r4_t),   parameter :: C12566370614359172953850573533118 = &
                                                            XMM4r4_t(12.566370614359172953850573533118_sp)
                   type(XMM4r4_t),   parameter :: C2467401100272339654708622749969  = &
                                                            XMM4r4_t(2.467401100272339654708622749969_sp)
                   type(XMM4r4_t),   parameter :: C08905 = XMM4r4_t(0.8905_sp)
                   type(XMM4r4_t),   automatic :: term1,term2,term3
                   type(XMM4r4_t),   automatic :: cgami,cgams,c2gami
                   type(XMM4r4_t),   automatic :: c2gams,inv,arg
                   type(XMM4r4_t),   automatic :: sarg,rat1,rat2
                   type(XMM4r4_t),   automatic :: x0,x1,arg2
                   type(XMM4r4_t),   automatic :: larg,cpsii,cpsis
                   type(XMM4r4_t),   automatic :: fac,c2psii,c2psis
                   type(XMM4r4_t),   automatic :: spsii,spsis
#if (GMS_EXPLICIT_VECTORIZE) == 1
                    integer(kind=i4) :: j
                    !dir$ loop_count(4)
                    !dir$ vector aligned
                    !dir$ vector vectorlength(4)
                    !dir$ vector always
                    do j=0, 3
                         fac.v(j)   = C12566370614359172953850573533118.v(j) * &
                                      h.v(j) *h.v(j) 
                         arg2.v(j)   = k0a.v(j)*C2467401100272339654708622749969.v(j) 
                         cpsii.v(j)  = cos(psii.v(j))
                         cpsis.v(j)  = cos(psis.v(j))
                         c2psii.v(j) = cpsii.v(j)*cpsii.v(j) 
                         c2psis.v(j) = cpsis.v(j)*cpsis.v(j) 
                         arg2.v(j)   = cpsii.v(j)*arg2.v(j) 
                         rat1.v(j)   = c2psis.v(j)/c2psii.v(j) 
                         cgami.v(j)  = cos(gami.v(j))
                         c2gami.v(j) = cgami.v(j)*cgami.v(j) 
                         cgams.v(j)  = cos(gams.v(j))
                         c2gams.v(j) = cgams.v(j)*cgams.v(j) 
                         x0.v(j)     = c2gams.v(j)*c2gami.v(j) 
                         term1.v(j)  = fac.v(j)*rat1.v(j)*x0.v(j) 
                         larg.v(j)   = log(arg2.v(j))
                         spsii.v(j)  = sin(psii.v(j))
                         x1.v(j)     = larg.v(j)*larg.v(j)+ &
                                       C2467401100272339654708622749969.v(j)
                         inv.v(j)    =  v4r4_1.v(j)/x1.v(j)
                         spsis.v(j)  = sin(psis.v(j))
                         x0.v(j)     = spsii.v(j)+spsis.v(j)
                         arg.v(j)    = k0.v(j)*x0.v(j)*h.v(j) 
                         sarg.v(j)   = sin(arg.v(j))
                         rat2.v(j)   = sarg.v(j)/arg.v(j)
                         term2.v(j)  = rat2.v(j)*rat2.v(j)
                         rcs.v(j)    = term1.v(j)*inv.v(j)*term2.v(j)
                    end
#else                   
                   fac.v   = C12566370614359172953850573533118.v* &
                             h.v*h.v
                   arg2.v  = k0a.v*C2467401100272339654708622749969.v
                   cpsii.v = cos(psii.v)
                   cpsis.v = cos(psis.v)
                   c2psii.v= cpsii.v*cpsii.v
                   c2psis.v= cpsis.v*cpsis.v
                   arg2.v  = cpsii.v*arg2.v
                   rat1.v  = c2psis.v/c2psii.v
                   cgami.v = cos(gami.v)
                   c2gami.v= cgami.v*cgami.v
                   cgams.v = cos(gams.v)
                   c2gams.v= cgams.v*cgams.v
                   x0.v    = c2gams.v*c2gami.v
                   term1.v = fac.v*rat1.v*x0.v
                   larg.v  = log(arg2.v)
                   spsii.v = sin(psii.v)
                   x1.v    = larg.v*larg.v+ &
                             C2467401100272339654708622749969.v
                   inv.v   =  v4r4_1.v/x1.v
                   spsis.v = sin(psis.v)
                   x0.v    = spsii.v+spsis.v
                   arg.v   = k0.v*x0.v*h.v
                   sarg.v  = sin(arg.v)
                   rat2.v  = sarg.v/arg.v
                   term2.v = rat2.v*rat2.v
                   rcs.v   = term1.v*inv.v*term2.v
#endif
            end function rcs_f4344_xmm4r4
            
            !  /*
            !!
            !              General backscatter (only) scattering RCS from long thin wire.
            !              Formula 4.3-45
            !         */
            
            pure function rcs_f4345_xmm4r4(psi,k0a,gami,gams,k0,h) result(rcs)
                 
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4345_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4345_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4345_xmm4r4 
                   type(XMM4r4_t),   intent(in) :: psi
                   type(XMM4r4_t),   intent(in) :: k0a
                   type(XMM4r4_t),   intent(in) :: gami
                   type(XMM4r4_t),   intent(in) :: gams
                   type(XMM4r4_t),   intent(in) :: k0
                   type(XMM4r4_t),   intent(in) :: h
                   type(XMM4r4_t) :: rcs
                     !dir$ attributes align : 16 :: C2467401100272339654708622749969 
                     !dir$ attributes align : 16 :: C6283185307179586476925286766559
                     !dir$ attributes align : 16 :: C08905
                     !dir$ attributes align : 16 :: rat1
                     !dir$ attributes align : 16 :: arg
                     !dir$ attributes align : 16 :: sarg
                     !dir$ attributes align : 16 :: arg2
                     !dir$ attributes align : 16 :: larg2
                     !dir$ attributes align : 16 :: k0h
                     !dir$ attributes align : 16 :: rat
                     !dir$ attributes align : 16 :: cpsi
                     !dir$ attributes align : 16 :: cgami
                     !dir$ attributes align : 16 :: cgams
                     !dir$ attributes align : 16 :: c2gami
                     !dir$ attributes align : 16 :: c2gams
                     !dir$ attributes align : 16 :: spsi
                     !dir$ attributes align : 16 :: x0
                     !dir$ attributes align : 16 :: x1
                   type(XMM4r4_t),  parameter :: C2467401100272339654708622749969 = &
                                                           XMM4r4_t(2.467401100272339654708622749969_sp)
                   type(XMM4r4_t),  parameter :: C6283185307179586476925286766559 = &
                                                           XMM4r4_t(6.283185307179586476925286766559_sp)
                   type(XMM4r4_t),  parameter :: C08905 = XMM4r4_t(0.8905_sp)
                   type(XMM4r4_t),  automatic :: rat1,arg,sarg
                   type(XMM4r4_t),  automatic :: arg2,larg2,k0h
                   type(XMM4r4_t),  automatic :: rat,cpsi,cgami
                   type(XMM4r4_t),  automatic :: cgams,c2gami,c2gams
                   type(XMM4r4_t),  automatic :: spsi,x0,x1
#if (GMS_EXPLICIT_VECTORIZE) == 1
                    integer(kind=i4) :: j
                    !dir$ loop_count(4)
                    !dir$ vector aligned
                    !dir$ vector vectorlength(4)
                    !dir$ vector always
                    do j=0, 3    
                       k0h.v(j)   = k0.v(j)*h.v(j)
                       t0.v(j)    = C6283185307179586476925286766559.v(j)* &
                                    h.v(j)*h.v(j)
                       x0.v(j)    = k0h.v(j)+k0h.v(j)
                       spsi.v(j)  = sin(psi.v(j))
                       arg.v(j)   = x0.v(j)*spsi.v(j)
                       cpsi.v(j)  = cos(psi.v(j))
                       arg2.v(j)  = cpsi.v(j)*k0a.v(j)*C08905.v(j)
                       larg.v(j)  = arg2.v(j)*arg2.v(j)+ &
                                    C2467401100272339654708622749969.v(j)
                       sarg.v(j)  = sin(arg.v(j))
                       cgams.v(j) = cos(gams.v(j))
                       rat.v(j)   = sarg.v(j)/arg.v(j)
                       cgami.v(j) = cos(gami.v(j))
                       x1.v(j)    = rat.v(j)*rat.v(j)
                       c2gams.v(j)= cgams.v(j)*cgams.v(j)
                       c2gami.v(j)= cgami.v(j)*cgami.v(j)
                       x0.v(j)    = t0.v(j)*c2gams.v(j)*c2gami.v(j)
                       rat1.v(j)  = x0.v(j)*larg.v(j)
                       rcs.v(j)   = rat1.v(j)*x1.v(j)
                    end do
#else
                   k0h.v   = k0.v*h.v
                   t0.v    = C6283185307179586476925286766559.v* &
                             h.v*h.v
                   x0.v    = k0h.v+k0h.v
                   spsi.v  = sin(psi.v)
                   arg.v   = x0.v*spsi.v
                   cpsi.v  = cos(psi.v)
                   arg2.v  = cpsi.v*k0a.v*C08905.v
                   larg.v  = arg2.v*arg2.v+ &
                             C2467401100272339654708622749969.v
                   sarg.v  = sin(arg.v)
                   cgams.v = cos(gams.v)
                   rat.v   = sarg.v/arg.v
                   cgami.v = cos(gami.v)
                   x1.v    = rat.v*rat.v
                   c2gams.v= cgams.v*cgams.v
                   c2gami.v= cgami.v*cgami.v
                   x0.v    = t0.v*c2gams.v*c2gami.v
                   rat1.v  = x0.v*larg.v
                   rcs.v   = rat1.v*x1.v
#endif
            end function rcs_f4345_xmm4r4
            
            
            ! /*
            !            Backscattering From a Perfectly Conducting Cylinder With Flat Ends.
            !            Helper functions, M1,M2 for the main formula 4.3-48
            !            Formula 4.3-50
            !!
            !       */
            
            pure function M1_f4350_xmm4r4(psi) result(M1)
                
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: M1_f4350_xmm4r4
                   !dir$ attributes forceinline :: M1_f4350_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: M1_f4350_xmm4r4
                   use mod_vecconsts, only :  v4r4_1
                   type(XMM4r4_t),   intent(in) :: psi
                   type(XMM4r4_t) :: M1
                   ! Locals
                     !dir$ attributes align : 16 :: C314159265358979323846264338328
                     !dir$ attributes align : 16 :: C0333333333333333333333333333333333333333333
                     !dir$ attributes align : 16 :: C0577350269189625764509148780502   
                     !dir$ attributes align : 16 :: C0666666666666666666666666666667
                     !dir$ attributes align : 16 :: C05
                     !dir$ attributes align : 16 :: C40
                     !dir$ attributes align : 16 :: C10
                     !dir$ attributes align : 16 :: C20
                     !dir$ attributes align : 16 :: inv1
                     !dir$ attributes align : 16 :: inv2
                     !dir$ attributes align : 16 :: arg1
                     !dir$ attributes align : 16 :: arg2
                     !dir$ attributes align : 16 :: carg1
                     !dir$ attributes align : 16 :: carg2
                     !dir$ attributes align : 16 :: x0
                     !dir$ attributes align : 16 :: x1
                   type(XMM4r4_t),   parameter :: C314159265358979323846264338328              = &
                                                        XMM4r4_t(3.14159265358979323846264338328_sp)
                   type(XMM4r4_t),   parameter :: C0333333333333333333333333333333333333333333 = &
                                                           XMM4r4_t(0.333333333333333333333333333333333333333333_sp)
                   type(XMM4r4_t),   parameter :: C0577350269189625764509148780502             = &
                                                           XMM4r4_t(0.577350269189625764509148780502_sp)
                   type(XMM4r4_t),   parameter :: C0666666666666666666666666666667             = &
                                                           XMM4r4_t(0.666666666666666666666666666667_sp)
                   type(XMM4r4_t),   parameter :: C05 =   XMM4r4_t(0.5_sp)
                   type(XMM4r4_t),   parameter :: C40 =   XMM4r4_t(4.0_sp)
                   type(XMM4r4_t),   parameter :: C10 =   XMM4r4_t(-1.0_sp)
                   type(XMM4r4_t),   parameter :: C20 =   XMM4r4_t(2.0_sp)
                   type(XMM4r4_t),   automatic :: inv1,inv2,arg1,arg2
                   type(XMM4r4_t),   automatic :: carg1,carg2,x0,x1
#if (GMS_EXPLICIT_VECTORIZE) == 1
                    integer(kind=i4) :: j
                    !dir$ loop_count(4)
                    !dir$ vector aligned
                    !dir$ vector vectorlength(4)
                    !dir$ vector always
                    do j=0, 3
                         arg1.v(j)  = C0333333333333333333333333333333333333333333.v(j)* &
                                      C40.v(j)*psi.v(j)
                         carg1.v(j) = cos(arg1.v(j))
                         x0.v(j)    = C20.v(j)*psi.v(j)+ &
                                      C314159265358979323846264338328.v(j)
                         carg1.v(j) = C10.v(j)+carg1.v(j)
                         arg2.v(j)  = C0666666666666666666666666666667.v(j)* &
                                      x0.v(j)
                         inv1.v(j)  =  v4r4_1.v(j)/carg1.v(j)
                         carg2.v(j) = cos(arg2.v(j))
                         x1.v(j)    = C05.v(j)*carg2.v(j)
                         inv2.v(j)  =  v4r4_1.v(j)/x1.v(j)
                         x0.v(j)    = C10.v(j)*inv1.v(j)
                         M1.v(j)    = C0577350269189625764509148780502.v(j)* &
                                      (x0.v(j)+inv2.v(j))
                    end do
#else                   
                   arg1.v  = C0333333333333333333333333333333333333333333.v* &
                             C40.v*psi.v
                   carg1.v = cos(arg1.v)
                   x0.v    = C20.v*psi.v+ &
                             C314159265358979323846264338328.v
                   carg1.v = C10.v+carg1.v
                   arg2.v  = C0666666666666666666666666666667.v* &
                             x0.v
                   inv1.v  =  v4r4_1.v/carg1.v
                   carg2.v = cos(arg2.v)
                   x1.v    = C05.v*carg2.v
                   inv2.v  =  v4r4_1.v/x1.v
                   x0.v    = C10.v*inv1.v
                   M1.v    = C0577350269189625764509148780502.v* &
                             (x0.v+inv2.v)
#endif
            end function M1_f4350_xmm4r4
            
            
            pure function M2_f4350_xmm4r4(psi) result(M1)
                
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: M2_f4350_xmm4r4
                   !dir$ attributes forceinline :: M2_f4350_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: M2_f4350_xmm4r4 
                   use mod_vecconsts, only :  v4r4_1
                   type(XMM4r4_t),   intent(in) :: psi
                   type(XMM4r4_t) :: M2
                   ! Locals
                     !dir$ attributes align : 16 :: C314159265358979323846264338328
                     !dir$ attributes align : 16 :: C0333333333333333333333333333333333333333333
                     !dir$ attributes align : 16 :: C0577350269189625764509148780502
                     !dir$ attributes align : 16 :: C0666666666666666666666666666667 
                     !dir$ attributes align : 16 :: C05
                     !dir$ attributes align : 16 :: C40
                     !dir$ attributes align : 16 :: C10
                     !dir$ attributes align : 16 :: C20
                     !dir$ attributes align : 16 :: inv1
                     !dir$ attributes align : 16 :: inv2
                     !dir$ attributes align : 16 :: arg1
                     !dir$ attributes align : 16 :: arg2
                     !dir$ attributes align : 16 :: carg1
                     !dir$ attributes align : 16 :: carg2
                     !dir$ attributes align : 16 :: x0
                     !dir$ attributes align : 16 :: x1
                   type(XMM4r4_t),   parameter :: C314159265358979323846264338328              = &
                                                        XMM4r4_t(3.14159265358979323846264338328_sp)
                   type(XMM4r4_t),   parameter :: C0333333333333333333333333333333333333333333 = &
                                                           XMM4r4_t(0.333333333333333333333333333333333333333333_sp)
                   type(XMM4r4_t),   parameter :: C0577350269189625764509148780502             = &
                                                           XMM4r4_t(0.577350269189625764509148780502_sp)
                   type(XMM4r4_t),   parameter :: C0666666666666666666666666666667             = &
                                                           XMM4r4_t(0.666666666666666666666666666667_sp)
                   type(XMM4r4_t),   parameter :: C05 =   XMM4r4_t(0.5_sp)
                   type(XMM4r4_t),   parameter :: C40 =   XMM4r4_t(4.0_sp)
                   type(XMM4r4_t),   parameter :: C10 =   XMM4r4_t(-1.0_sp)
                   type(XMM4r4_t),   parameter :: C20 =   XMM4r4_t(2.0_sp)
                   type(XMM4r4_t),   automatic :: inv1,inv2,arg1,arg2
                   type(XMM4r4_t),   automatic :: carg1,carg2,x0,x1
#if (GMS_EXPLICIT_VECTORIZE) == 1
                    integer(kind=i4) :: j
                    !dir$ loop_count(4)
                    !dir$ vector aligned
                    !dir$ vector vectorlength(4)
                    !dir$ vector always
                    do j=0, 3  
                       arg1.v(j)  = C0333333333333333333333333333333333333333333.v(j)* &
                                    C40.v(j)*psi.v(j)
                       carg1.v(j) = cos(arg1.v(j))
                       x0.v(j)    = C20.v(j)*psi.v(j)+ &
                                    C314159265358979323846264338328.v(j)
                       carg1.v(j) = C10.v(j)+carg1.v(j)
                       arg2.v(j)  = C0666666666666666666666666666667.v(j)* &
                                    x0.v(j)
                       inv1.v(j)  =  v4r4_1.v(j)/carg1.v(j)
                       carg2.v(j) = cos(arg2.v(j))
                       x1.v(j)    = C05.v*carg2.v(j)
                       inv2.v(j)  =  v4r4_1.v(j)/x1.v(j)
                       x0.v(j)    = C10.v(j)*inv1.v(j)
                       M2.v(j)    = C0577350269189625764509148780502.v(j)* &
                                    (x0.v(j)-inv2.v(j))
                    end do
#else                
                   arg1.v  = C0333333333333333333333333333333333333333333.v* &
                             C40.v*psi.v
                   carg1.v = cos(arg1.v)
                   x0.v    = C20.v*psi.v+ &
                             C314159265358979323846264338328.v
                   carg1.v = C10.v+carg1.v
                   arg2.v  = C0666666666666666666666666666667.v* &
                             x0.v
                   inv1.v  =  v4r4_1.v/carg1.v
                   carg2.v = cos(arg2.v)
                   x1.v    = C05.v*carg2.v
                   inv2.v  =  v4r4_1.v/x1.v
                   x0.v    = C10.v*inv1.v
                   M2.v    = C0577350269189625764509148780502.v* &
                             (x0.v-inv2.v)
#endif
            end function M2_f4350_xmm4r4  
            
            ! /*
            !            Backscattering From a Perfectly Conducting Cylinder With Flat Ends.
            !            Helper functions, M1,M2 for the main formula 4.3-48
            !            Formula 4.3-51
            !!
            !       */    
            
            pure function N1_f4351_xmm4r4(psi) result(N1)
                
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: N1_f4351_xmm4r4
                   !dir$ attributes forceinline :: N1_f4351_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: N1_f4351_xmm4r4
                   use mod_vecconsts, only :  v4r4_1
                   type(XMM4r4_t),   intent(in) :: psi
                   type(XMM4r4_t) :: N1
                   ! Locals
                     !dir$ attributes align : 16 :: C314159265358979323846264338328
                     !dir$ attributes align : 16 :: C0333333333333333333333333333333333333333333
                     !dir$ attributes align : 16 :: C0577350269189625764509148780502
                     !dir$ attributes align : 16 :: C0666666666666666666666666666667 
                     !dir$ attributes align : 16 :: C05
                     !dir$ attributes align : 16 :: C40
                     !dir$ attributes align : 16 :: C40n
                     !dir$ attributes align : 16 :: C10
                     !dir$ attributes align : 16 :: C20
                     !dir$ attributes align : 16 :: inv1
                     !dir$ attributes align : 16 :: inv2
                     !dir$ attributes align : 16 :: arg1
                     !dir$ attributes align : 16 :: arg2
                     !dir$ attributes align : 16 :: carg1
                     !dir$ attributes align : 16 :: carg2
                     !dir$ attributes align : 16 :: x0
                     !dir$ attributes align : 16 :: x1
                   type(XMM4r4_t),   parameter :: C314159265358979323846264338328              = &
                                                        XMM4r4_t(3.14159265358979323846264338328_sp)
                   type(XMM4r4_t),   parameter :: C0333333333333333333333333333333333333333333 = &
                                                           XMM4r4_t(0.333333333333333333333333333333333333333333_sp)
                   type(XMM4r4_t),   parameter :: C0577350269189625764509148780502             = &
                                                           XMM4r4_t(0.577350269189625764509148780502_sp)
                   type(XMM4r4_t),   parameter :: C0666666666666666666666666666667             = &
                                                           XMM4r4_t(0.666666666666666666666666666667_sp)
                   type(XMM4r4_t),   parameter :: C05 =   XMM4r4_t(0.5_sp)
                   type(XMM4r4_t),   parameter :: C40 =   XMM4r4_t(4.0_sp)
                   type(XMM4r4_t),   parameter :: C40n=   XMM4r4_t(-4.0_sp)
                   type(XMM4r4_t),   parameter :: C10 =   XMM4r4_t(-1.0_sp)
                   type(XMM4r4_t),   parameter :: C20 =   XMM4r4_t(2.0_sp)
                   type(XMM4r4_t),   automatic :: inv1,inv2,arg1,arg2
                   type(XMM4r4_t),   automatic :: carg1,carg2,x0,x1
#if (GMS_EXPLICIT_VECTORIZE) == 1
                      integer(kind=i4) :: j
                      !dir$ loop_count(4)
                      !dir$ vector aligned
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do j=0, 3  
                         arg1.v(j)  = C0333333333333333333333333333333333333333333.v(j)* &
                                      C40.v(j)*psi.v(j)
                         carg1.v(j) = cos(arg1.v(j))
                         x0.v(j)    = C20.v(j)*psi.v(j)+ &
                                      C314159265358979323846264338328.v(j)
                         carg1.v(j) = C05.v(j)+carg1.v(j)
                         arg2.v(j)  = C0666666666666666666666666666667.v(j)* &
                                      x0.v(j)
                         inv1.v(j)  =  v4r4_1.v(j)/carg1.v(j)
                         carg2.v(j) = cos(arg2.v(j))
                         x1.v(j)    = C05.v(j)*carg2.v(j)
                         inv2.v(j)  =  v4r4_1.v(j)/x1.v(j)
                         x0.v(j)    = C10.v(j)*inv1.v(j)
                         N1.v(j)    = C0577350269189625764509148780502.v(j)* &
                                      (C40n.v(j)-x0.v(j)-inv2.v(j))
                      end do 
#else                  
                   arg1.v  = C0333333333333333333333333333333333333333333.v* &
                             C40.v*psi.v
                   carg1.v = cos(arg1.v)
                   x0.v    = C20.v*psi.v+ &
                             C314159265358979323846264338328.v
                   carg1.v = C05.v+carg1.v
                   arg2.v  = C0666666666666666666666666666667.v* &
                             x0.v
                   inv1.v  =  v4r4_1.v/carg1.v
                   carg2.v = cos(arg2.v)
                   x1.v    = C05.v*carg2.v
                   inv2.v  =  v4r4_1.v/x1.v
                   x0.v    = C10.v*inv1.v
                   N1.v    = C0577350269189625764509148780502.v* &
                             (C40n.v-x0.v-inv2.v)
#endif
            end function N1_f4350_xmm4r4    
            
            
            pure function N2_f4351_xmm4r4(psi) result(N2)
                
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: N2_f4351_xmm4r4
                   !dir$ attributes forceinline :: N2_f4351_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: N2_f4351_xmm4r4
                   use mod_vecconsts, only :  v4r4_1
                   type(XMM4r4_t),   intent(in) :: psi
                   type(XMM4r4_t) :: N2
                   ! Locals
                     !dir$ attributes align : 16 :: C314159265358979323846264338328
                     !dir$ attributes align : 16 :: C0333333333333333333333333333333333333333333
                     !dir$ attributes align : 16 :: C0577350269189625764509148780502
                     !dir$ attributes align : 16 :: C0666666666666666666666666666667 
                     !dir$ attributes align : 16 :: C05
                     !dir$ attributes align : 16 :: C40
                     !dir$ attributes align : 16 :: C40n
                     !dir$ attributes align : 16 :: C10
                     !dir$ attributes align : 16 :: C20
                     !dir$ attributes align : 16 :: inv1
                     !dir$ attributes align : 16 :: inv2
                     !dir$ attributes align : 16 :: arg1
                     !dir$ attributes align : 16 :: arg2
                     !dir$ attributes align : 16 :: carg1
                     !dir$ attributes align : 16 :: carg2
                     !dir$ attributes align : 16 :: x0
                     !dir$ attributes align : 16 :: x1
                   type(XMM4r4_t),   parameter :: C314159265358979323846264338328              = &
                                                        XMM4r4_t(3.14159265358979323846264338328_sp)
                   type(XMM4r4_t),   parameter :: C0333333333333333333333333333333333333333333 = &
                                                           XMM4r4_t(0.333333333333333333333333333333333333333333_sp)
                   type(XMM4r4_t),   parameter :: C0577350269189625764509148780502             = &
                                                           XMM4r4_t(0.577350269189625764509148780502_sp)
                   type(XMM4r4_t),   parameter :: C0666666666666666666666666666667             = &
                                                           XMM4r4_t(0.666666666666666666666666666667_sp)
                   type(XMM4r4_t),   parameter :: C05 =   XMM4r4_t(0.5_sp)
                   type(XMM4r4_t),   parameter :: C40 =   XMM4r4_t(4.0_sp)
                   type(XMM4r4_t),   parameter :: C40n=   XMM4r4_t(-4.0_sp)
                   type(XMM4r4_t),   parameter :: C10 =   XMM4r4_t(-1.0_sp)
                   type(XMM4r4_t),   parameter :: C20 =   XMM4r4_t(2.0_sp)
                   type(XMM4r4_t),   automatic :: inv1,inv2,arg1,arg2
                   type(XMM4r4_t),   automatic :: carg1,carg2,x0,x1
#if (GMS_EXPLICIT_VECTORIZE) == 1
                      integer(kind=i4) :: j
                      !dir$ loop_count(4)
                      !dir$ vector aligned
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do j=0, 3  
                           arg1.v(j)  = C0333333333333333333333333333333333333333333.v(j)* &
                                        C40.v(j)*psi.v(j)
                           carg1.v(j) = cos(arg1.v(j))
                           x0.v(j)    = C20.v(j)*psi.v(j)+ &
                                        C314159265358979323846264338328.v(j)
                           carg1.v(j) = C05.v(j)+carg1.v(j)
                           arg2.v(j)  = C0666666666666666666666666666667.v(j)* &
                                        x0.v(j)
                           inv1.v(j)  =  v4r4_1.v(j)/carg1.v(j)
                           carg2.v(j) = cos(arg2.v(j))
                           x1.v(j)    = C05.v(j)*carg2.v(j)
                           inv2.v(j)  =  v4r4_1.v(j)/x1.v(j)
                           x0.v(j)    = C10.v(j)*inv1.v(j)
                           N2.v(j)    = C0577350269189625764509148780502.v(j)* &
                                        (C40n.v(j)+x0.v(j)+inv2.v(j))
                      end do
#else                   
                   arg1.v  = C0333333333333333333333333333333333333333333.v* &
                             C40.v*psi.v
                   carg1.v = cos(arg1.v)
                   x0.v    = C20.v*psi.v+ &
                             C314159265358979323846264338328.v
                   carg1.v = C05.v+carg1.v
                   arg2.v  = C0666666666666666666666666666667.v* &
                             x0.v
                   inv1.v  =  v4r4_1.v/carg1.v
                   carg2.v = cos(arg2.v)
                   x1.v    = C05.v*carg2.v
                   inv2.v  =  v4r4_1.v/x1.v
                   x0.v    = C10.v*inv1.v
                   N2.v    = C0577350269189625764509148780502.v* &
                             (C40n.v+x0.v+inv2.v)
#endif
            end function N2_f4350_xmm4r4    
            
            
           ! /*
           !             Backscattering From a Perfectly Conducting Cylinder With Flat Ends.
           !             Helper functions, M1,M2 for the main formula 4.3-48
           !             Formula 4.3-52
           !!
           !        */ 
           
           pure function G_f4352_xmm4r4(psi) result(G)
               
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: G_f4352_xmm4r4
                   !dir$ attributes forceinline :: G_f4352_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: G_f4352_xmm4r4
                   use mod_vecconsts, only :  v4r4_1
                   type(XMM4r4_t),   intent(in) :: psi
                   type(XMM4r4_t) :: G
                   ! Locals
                     !dir$ attributes align : 16 :: C0333333333333333333333333333333333333333333
                     !dir$ attributes align : 16 :: C0577350269189625764509148780502
                     !dir$ attributes align : 16 :: C05
                     !dir$ attributes align : 16 :: C40
                     !dir$ attributes align : 16 :: C20
                     !dir$ attributes align : 16 :: inv
                     !dir$ attributes align : 16 :: arg
                     !dir$ attributes align : 16 :: carg
                     !dir$ attributes align : 16 :: x0
                   type(XMM4r4_t),   parameter :: C0333333333333333333333333333333333333333333 = &
                                                           XMM4r4_t(0.333333333333333333333333333333333333333333_sp)
                   type(XMM4r4_t),   parameter :: C0577350269189625764509148780502             = &
                                                           XMM4r4_t(0.577350269189625764509148780502_sp)
                   type(XMM4r4_t),   parameter :: C05 =   XMM4r4_t(0.5_sp)
                   type(XMM4r4_t),   parameter :: C40 =   XMM4r4_t(4.0_sp)
                   type(XMM4r4_t),   parameter :: C20 =   XMM4r4_t(-2.0_sp)
                   type(XMM4r4_t),   automatic :: inv,arg,carg,x0
#if (GMS_EXPLICIT_VECTORIZE) == 1
                      integer(kind=i4) :: j
                      !dir$ loop_count(4)
                      !dir$ vector aligned
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do j=0, 3  
                             arg.v(j)   = C0333333333333333333333333333333333333333333.v(j)* &
                                          C40.v(j)*psi.v(j)
                             carg.v(j)  = cos(arg.v(j))
                             x0.v(j)    = C05.v(j)+carg.v(j)
                             inv.v(j)   =  v4r4_1.v(j)/x0.v(j)
                             G.v(j)     = C0577350269189625764509148780502.v(j)* &
                                          (C20.v(j)-inv.v(j))
                      end do
#else                   
                   arg.v   = C0333333333333333333333333333333333333333333.v* &
                             C40.v*psi.v
                   carg.v  = cos(arg.v)
                   x0.v    = C05.v+carg.v
                   inv.v   =  v4r4_1.v/x0.v
                   G.v     = C0577350269189625764509148780502.v* &
                             (C20.v-inv.v)
#endif
           end function G_f4352_xmm4r4
           
           
           pure function F_f4352_xmm4r4(psi) result(G)
               
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: F_f4352_xmm4r4
                   !dir$ attributes forceinline :: F_f4352_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: F_f4352_xmm4r4
                   use mod_vecconsts, only :  v4r4_1
                   type(XMM4r4_t),   intent(in) :: psi
                   type(XMM4r4_t) :: F
                   ! Locals
                     !dir$ attributes align : 16 :: C0333333333333333333333333333333333333333333
                     !dir$ attributes align : 16 :: C0577350269189625764509148780502
                     !dir$ attributes align : 16 :: C05
                     !dir$ attributes align : 16 :: C40
                     !dir$ attributes align : 16 :: C20
                     !dir$ attributes align : 16 :: inv
                     !dir$ attributes align : 16 :: arg
                     !dir$ attributes align : 16 :: carg
                     !dir$ attributes align : 16 :: x0
                   type(XMM4r4_t),   parameter :: C0333333333333333333333333333333333333333333 = &
                                                           XMM4r4_t(0.333333333333333333333333333333333333333333_sp)
                   type(XMM4r4_t),   parameter :: C0577350269189625764509148780502             = &
                                                           XMM4r4_t(0.577350269189625764509148780502_sp)
                   type(XMM4r4_t),   parameter :: C05 =   XMM4r4_t(0.5_sp)
                   type(XMM4r4_t),   parameter :: C40 =   XMM4r4_t(4.0_sp)
                   type(XMM4r4_t),   parameter :: C20 =   XMM4r4_t(-2.0_sp)
                   type(XMM4r4_t),   automatic :: inv,arg,carg,x0
#if (GMS_EXPLICIT_VECTORIZE) == 1
                      integer(kind=i4) :: j
                      !dir$ loop_count(4)
                      !dir$ vector aligned
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do j=0, 3  
                             arg.v(j) = C0333333333333333333333333333333333333333333.v(j)* &
                                          C40.v(j)*psi.v(j)
                             carg.v(j) = cos(arg.v(j))
                             x0.v(j)   = C05.v(j)+carg.v(j)
                             inv.v(j)  =  v4r4_1.v(j)/x0.v(j)
                             F.v(j)    = C0577350269189625764509148780502.v(j)* &
                                         (C20.v(j)+inv.v(j))
                      end do
#else                   
                   arg.v   = C0333333333333333333333333333333333333333333.v* &
                             C40.v*psi.v
                   carg.v  = cos(arg.v)
                   x0.v    = C05.v+carg.v
                   inv.v   =  v4r4_1.v/x0.v
                   F.v     = C0577350269189625764509148780502.v* &
                             (C20.v+inv.v)
#endif
           end function F_f4352_xmm4r4
           
           
           
          !          /*
          !                 Scattering From Cylinder Near the Specular Direction.
          !                 Formula 4.3-53
          !            */
          
          pure function rcs_f4353_xmm4r4(k0a,k0,h,phi,psii,psis) result(rcs)
               
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4353_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4353_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4353_xmm4r4
                   type(XMM4r4_t),   intent(in) :: k0a
                   type(XMM4r4_t),   intent(in) :: k0
                   type(XMM4r4_t),   intent(in) :: h
                   type(XMM4r4_t),   intent(in) :: phi
                   type(XMM4r4_t),   intent(in) :: psii
                   type(XMM4r4_t),   intent(in) :: psis
                   type(XMM4r4_t) :: rcs
                     !dir$ attributes align : 16 :: C05
                     !dir$ attributes align : 16 :: C40
                     !dir$ attributes align : 16 :: trm1
                     !dir$ attributes align : 16 ::  trm2
                     !dir$ attributes align : 16 ::  trm3
                     !dir$ attributes align : 16 :: cphi
                     !dir$ attributes align : 16 :: cpsis
                     !dir$ attributes align : 16 :: c2psis
                     !dir$ attributes align : 16 :: c2psii
                     !dir$ attributes align : 16 :: spsii
                     !dir$ attributes align : 16 :: spsis
                     !dir$ attributes align : 16 :: arg
                     !dir$ attributes align : 16 :: sarg
                     !dir$ attributes align : 16 :: x0
                     !dir$ attributes align : 16 :: x1
                   type(XMM4r4_t),   parameter :: C05 = XMM4r4_t(0.5_sp)
                   type(XMM4r4_t),   parameter :: C40 = XMM4r4_t(4.0_sp)
                   type(XMM4r4_t),   automatic :: trm1,trm2,trm3
                   type(XMM4r4_t),   automatic :: cphi,cpsis,c2psis
                   type(XMM4r4_t),   automatic :: c2psii,spsii,spsis
                   type(XMM4r4_t),   automatic :: arg,sarg,x0,x1
#if (GMS_EXPLICIT_VECTORIZE) == 1
                      integer(kind=i4) :: j
                      !dir$ loop_count(4)
                      !dir$ vector aligned
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do j=0, 3  
                          x0.v(j)     = h.v(j)*h.v(j)
                          cpsii.v(j)  = cos(psii.v(j))
                          x1.v(j)     = C05.v(j)*phi.v(j)
                          cphi.v(j)   = cos(phi.v(j))
                          trm1.v(j)   = C40.v(j)*k0a.v(j)*x0.v(j)
                          spsii.v(j)  = sin(psi.v(j))
                          spsis.v(j)  = sin(psis.v(j))
                          x0.v(j)     = spsii.v(j)+spsis.v(j)
                          c2psis.v(j) = cpsis.v(j)*cpsis.v(j)
                          arg.v(j)    = k0.v(j)*x0.v(j)*h.v(j)
                          x1.v(j)     = c2psis.v(j)*cphi.v(j)
                          sarg.v(j)   = sin(arg.v(j))
                          trm2.v(j)   = x1.v(j)/cpsii.v(j)
                          trm3.v(j)   = sarg.v(j)/arg.v(j)
                          x1.v(j)     = trm1.v(j)*trm2.v(j)
                          x0.v(j)     = trm3.v(j)*trm3.v(j)
                          rcs.v(j)    = x1.v(j)*x0.v(j)
                      end do
#else                   
                   x0.v     = h.v*h.v
                   cpsii.v  = cos(psii.v)
                   x1.v     = C05.v*phi.v
                   cphi.v   = cos(phi.v)
                   trm1.v   = C40.v*k0a.v*x0.v
                   spsii.v  = sin(psi.v)
                   spsis.v  = sin(psis.v)
                   x0.v     = spsii.v+spsis.v
                   c2psis.v = cpsis.v*cpsis.v
                   arg.v    = k0.v*x0.v*h.v
                   x1.v     = c2psis.v*cphi.v
                   sarg.v   = sin(arg.v)
                   trm2.v   = x1.v/cpsii.v
                   trm3.v   = sarg.v/arg.v
                   x1.v     = trm1.v*trm2.v
                   x0.v     = trm3.v*trm3.v
                   rcs.v    = x1.v*x0.v
#endif
          end function rcs_f4353_xmm4r4
          
         
          !  /*
          !!
          !                  Specular direction -- RCS.
          !                  Formula 4.3-54
          !             */ 
          
          pure function rcs_f4354_xmm4r4(k0a,h,psii,phi) result(rcs)
               
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4354_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4354_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4354_xmm4r4
                   type(XMM4r4_t),   intent(in) :: k0a
                   type(XMM4r4_t),   intent(in) :: h
                   type(XMM4r4_t),   intent(in) :: psii
                   type(XMM4r4_t),   intent(in) :: phi 
                   type(XMM4r4_t) :: rcs
                      !dir$ attributes align : 16 :: C05
                      !dir$ attributes align : 16 :: C40
                      !dir$ attributes align : 16 :: trm1
                      !dir$ attributes align : 16 :: phi2
                      !dir$ attributes align : 16 :: h2
                      !dir$ attributes align : 16 :: cpsii
                      !dir$ attributes align : 16 :: cphi
                      !dir$ attributes align : 16 :: x0
                   type(XMM4r4_t),   parameter :: C05 = XMM4r4_t(0.5_sp)
                   type(XMM4r4_t),   parameter :: C40 = XMM4r4_t(4.0_sp)
                   type(XMM4r4_t),   automatic :: trm1,phi2,h2
                   type(XMM4r4_t),   automatic :: cpsii,cphi,x0
#if (GMS_EXPLICIT_VECTORIZE) == 1
                      integer(kind=i4) :: j
                      !dir$ loop_count(4)
                      !dir$ vector aligned
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do j=0, 3  
                           h2.v(j)   = h.v(j)*h.v(j)
                           cphi.v(j) = cos(phi.v(j))
                           phi2.v(j) = C05.v(j)*phi.v(j)
                           trm1.v(j) = C40.v(j)*k0a.v(j)*h2.v(j)
                           cpsii.v(j)= cos(psii.v(j))
                           x0.v(j)   = trm1.v(j)*cpsii.v(j)
                           rcs.v(j)  = x0.v(j)*cphi.v(j)
                      end do
#else                   
                   h2.v   = h.v*h.v
                   cphi.v = cos(phi.v)
                   phi2.v = C05.v*phi.v
                   trm1.v = C40.v*k0a.v*h2.v
                   cpsii.v= cos(psii.v)
                   x0.v   = trm1.v*cpsii.v
                   rcs.v  = x0.v*cphi.v
#endif
          end function rcs_f4354_xmm4r4
          
          
            !  /*
            !!
            !             Backscattering direction -- RCS for incidence angles
            !             near broadside.
            !             Formula 4.3-54
            !         */
            
            pure function rcs_f4354v2_xmm4r4(k0a,h,k0,psii) result(rcs)
                 
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4354v2_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4354v2_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4354v2_xmm4r4
                   type(XMM4r4_t),   intent(in) :: k0a
                   type(XMM4r4_t),   intent(in) :: h
                   type(XMM4r4_t),   intent(in) :: k0
                   type(XMM4r4_t),   intent(in) :: phi 
                   type(XMM4r4_t) :: rcs
                       !dir$ attributes align : 16 :: C40
                       !dir$ attributes align : 16 :: trm1
                       !dir$ attributes align : 16 :: trm2
                       !dir$ attributes align : 16 :: cpsii
                       !dir$ attributes align : 16 :: spsii
                       !dir$ attributes align : 16 :: x0
                       !dir$ attributes align : 16 :: x1
                       !dir$ attributes align : 16 :: k0h
                       !dir$ attributes align : 16 :: h2
                       !dir$ attributes align : 16 :: arg
                       !dir$ attributes align : 16 :: sarg
                   type(XMM4r4_t),   parameter :: C40 = XMM4r4_t(4.0_sp) 
                   type(XMM4r4_t),   automatic :: trm1,trm2,cpsii,spsii
                   type(XMM4r4_t),   automatic :: x0,x1,k0h,h2
                   type(XMM4r4_t),   automatic :: arg,sarg
#if (GMS_EXPLICIT_VECTORIZE) == 1
                      integer(kind=i4) :: j
                      !dir$ loop_count(4)
                      !dir$ vector aligned
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do j=0, 3  
                           k0h.v(j)  = k0.v(j)*h.v(j)
                           cpsii.v(j)= cos(psii.v(j)) 
                           h2.v(j)   = h.v(j)*h.v(j)
                           x0.v(j)   = k0h.v(j)+k0h.v(j)
                           x1.v(j)   = C40.v(j)*k0a.v(j)*h2.v(j)
                           spsii.v(j)= sin(psi.v(j))
                           trm1.v(j) = x1.v(j)*cpsii.v(j)
                           arg.v(j)  = x0.v(j)*spsii.v(j)
                           sarg.v(j) = sin(arg.v(j))
                           x0.v(j)   = sarg.v(j)/arg.v(j)
                           trm2.v(j) = x0.v(j)*x0.v(j)
                           rcs.v(j)  = trm1.v(j)*trm2.v(j)
                      end do    
#else               
                   k0h.v  = k0.v*h.v
                   cpsii.v= cos(psii.v) 
                   h2.v   = h.v*h.v
                   x0.v   = k0h.v+k0h.v
                   x1.v   = C40.v*k0a.v*h2.v
                   spsii.v= sin(psi.v)
                   trm1.v = x1.v*cpsii.v
                   arg.v  = x0.v*spsii.v
                   sarg.v = sin(arg.v)
                   x0.v   = sarg.v/arg.v
                   trm2.v = x0.v*x0.v
                   rcs.v  = trm1.v*trm2.v
#endif
            end function rcs_f4354v2_xmm4r4
            
            
            !  /*
            !!
            !            Broadside (psi == 0) RCS.
            !            Formula 4.3-56
            !       */
            
            pure function rcs_f4356_xmm4r4(k0a,h) result(rcs)
                
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4356_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4356_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4356_xmm4r4
                   type(XMM4r4_t),   intent(in) :: k0a
                   type(XMM4r4_t),   intent(in) :: h
                   type(XMM4r4_t)  :: rcs
                     !dir$ attributes align : 16 :: C40
                     !dir$ attributes align : 16 :: h2
                   type(XMM4r4_t),   parameter :: C40 = XMM4r4_t(4.0_sp) 
                   type(XMM4r4_t),   automatic :: h2
#if (GMS_EXPLICIT_VECTORIZE) == 1
                      integer(kind=i4) :: j
                      !dir$ loop_count(4)
                      !dir$ vector aligned
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do j=0, 3  
                          h2.v(j)  = h.v(j)*h.v(j)
                          rcs.v(j) = C40.v(j)*k0a.v(j)*h2.v(j)
                      end do
#else
                   h2.v  = h.v*h.v
                   rcs.v = C40.v*k0a.v*h2.v
#endif
             end function rcs_f4356_xmm4r4
             
            !  /*
            !           Elliptical cylinders.
            !       */


            !       /*
            !             Low-frequency approximations (k0a<0.5, k0b<0.5)
            !             TM-case,formula 4.4-11
            !        */ 
            
            pure function TM_f4411_xmm4r4(a,b,k0) result(TM)
                 
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: TM_f4411_xmm4r4
                   !dir$ attributes forceinline :: TM_f4411_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: TM_f4411_xmm4r4
                   use mod_vecconsts, only :  v4r4_1
                   type(XMM4r4_t),   intent(in) :: a
                   type(XMM4r4_t),   intent(in) :: b
                   type(XMM4r4_t),   intent(in) :: k0
                   type(  XMM4c4_t)    :: TM
                     !dir$ attributes align : 16 :: C157079632679489661923132169164n
                     !dir$ attributes align : 16 :: C157079632679489661923132169164 
                     !dir$ attributes align : 16 :: C08905
                     !dir$ attributes align : 16 :: C05
                     !dir$ attributes align : 16 :: num
                     !dir$ attributes align : 16 :: den
                     !dir$ attributes align : 16 :: inv
                     !dir$ attributes align : 16 :: ab2
                     !dir$ attributes align : 16 :: c0k0
                     !dir$ attributes align : 16 :: arg
                     !dir$ attributes align : 16 :: larg
                   type(XMM4r4_t), parameter :: C157079632679489661923132169164n = &
                                                          XMM4r4_t(-1.57079632679489661923132169164_sp)
                   type(XMM4r4_t), parameter :: C157079632679489661923132169164  = &
                                                          XMM4r4_t(1.57079632679489661923132169164_sp)
                   type(XMM4r4_t), parameter :: C08905 = XMM4r4_t(0.8905_sp)
                   type(XMM4r4_t), parameter :: C05    = XMM4r4_t(0.5_sp)
                   type(  XMM4c4_t),   automatic :: num,den,inv
                   type(XMM4r4_t), automatic :: ab2,c0k0,arg,larg
                   den.im  = C157079632679489661923132169164n.v
                   ab2.v   = C05.v*a.v*b.v
                   num.re  =  v4r4_1.v
                   c0k0.v  = C08905.v*k0.v
                   num.im  = num.re
                   arg.v   = ab2.v*c0k0.v
                   larg.v  = log(arg.v)
                   den.re  = larg.v
                   inv     = num/den
                   TM      = C157079632679489661923132169164*inv 
            end function TM_f4411_xmm4r4
            
            pure function TE_f4412_xmm4r4(k0a,a,b,phi1,phi2) result(TE)
                
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: TE_f4412_xmm4r4
                   !dir$ attributes forceinline :: TE_f4412_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: TE_f4412_xmm4r4
                   use mod_vecconsts, only :  v4r4_1, v4r4_0
                   type(XMM4r4_t),  intent(in) :: k0a
                   type(XMM4r4_t),  intent(in) :: a
                   type(XMM4r4_t),  intent(in) :: b
                   type(XMM4r4_t),  intent(in) :: phi1
                   type(XMM4r4_t),  intent(in) :: phi2
                   type(  XMM4c4_t)   :: TE
                     !dir$ attributes align : 16 :: C078539816339744830961566084582
                     !dir$ attributes align : 16 :: k0a2
                     !dir$ attributes align : 16 :: ba
                     !dir$ attributes align : 16 :: cphi1
                     !dir$ attributes align : 16 :: sphi1
                     !dir$ attributes align : 16 :: trm1
                     !dir$ attributes align : 16 :: trm2
                     !dir$ attributes align : 16 :: ba1
                     !dir$ attributes align : 16 :: x0
                     !dir$ attributes align : 16 :: x1
                     !dir$ attributes align : 16 :: cphi2
                     !dir$ attributes align : 16 :: sphi2
                   type(XMM4r4_t),  parameter :: C078539816339744830961566084582 = &
                                                           XMM4r4_t(0.78539816339744830961566084582_sp)
                   type(XMM4r4_t),  automatic :: k0a2,ba,cphi1,sphi1
                   type(XMM4r4_t),  automatic :: trm1,trm2,ba1
                   type(XMM4r4_t),  automatic :: x0,x1,cphi2,sphi2
#if (GMS_EXPLICIT_VECTORIZE) == 1
                      integer(kind=i4) :: j
                      !dir$ loop_count(4)
                      !dir$ vector aligned
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do j=0, 3  
                          cphi2.v(j)  = cos(phi2.v(j))
                          k0a2.v(j)   = k0a.v(j)*k0a.v(j)
                          ba.v(j)     = b.v(j)/a.v(j)
                          x0.v(j)     = C078539816339744830961566084582.v(j)* &
                                        k0a2.v(j)
                          cphi1.v(j)  = cos(phi1.v(j))
                          ba1.v(j)    =  v4r4_1.v(j)+ba.v(j)
                          x1.v(j)     = ba.v(j)+ba1.v(j)
                          sphi1.v(j)  = sin(phi1.v(j))
                          trm1.v(j)   = x0.v(j)+x1.v(j)
                          sphi2.v(j)  = sin(phi2.v(j))
                          x0.v(j)     = cphi2.v(j)*cphi1.v(j)+ &
                                        sphi2.v(j)*sphi1.v(j)
                          trm2.v(j)   = ba.v(j)*x0.v(j)
                          x1.v(j)     = trm1.v(j)*trm2.v(j)
                          TE.re(j)    =  v4r4_0.v(j)
                          TE.im(j)    = -x1.v(j)
                      end do
#else                   
                   cphi2.v  = cos(phi2.v)
                   k0a2.v   = k0a.v*k0a.v
                   ba.v     = b.v/a.v
                   x0.v     = C078539816339744830961566084582.v* &
                              k0a2.v
                   cphi1.v  = cos(phi1.v)
                   ba1.v    =  v4r4_1.v+ba.v
                   x1.v     = ba.v+ba1.v
                   sphi1.v  = sin(phi1.v)
                   trm1.v   = x0.v+x1.v
                   sphi2.v  = sin(phi2.v)
                   x0.v     = cphi2.v*cphi1.v+sphi2.v*sphi1.v
                   trm2.v   = ba.v*x0.v
                   x1.v     = trm1.v*trm2.v
                   TE.re    =  v4r4_0.v
                   TE.im    = -x1.v
#endif
            end function TE_f4412_xmm4r4
            
            !  /*
            !           TM-case, RCS.
            !           Formula 4.4-13
            !      */
            
            pure function rcs_f4413_xmm4r4(a,b,k0) result(rcs)
                
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4413_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4413_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4413_xmm4r4
                   type(XMM4r4_t),  intent(in) :: a
                   type(XMM4r4_t),  intent(in) :: b
                   type(XMM4r4_t),  intent(in) :: k0
                   type(XMM4r4_t)   :: rcs
                     !dir$ attributes align : 16 :: C9869604401089358618834490999876 
                     !dir$ attributes align : 16 :: C2467401100272339654708622749969 
                     !dir$ attributes align : 16 :: C08905
                     !dir$ attributes align : 16 :: C05
                     !dir$ attributes align : 16 :: abh
                     !dir$ attributes align : 16 :: k0abh
                     !dir$ attributes align : 16 :: num
                     !dir$ attributes align : 16 :: sqr1
                     !dir$ attributes align : 16 :: sqr2
                     !dir$ attributes align : 16 :: c0k0
                     !dir$ attributes align : 16 :: arg
                     !dir$ attributes align : 16 :: larg
                     !dir$ attributes align : 16 :: x0
                     !dir$ attributes align : 16 :: x1
                     !dir$ attributes align : 16 :: den
                   type(XMM4r4_t),  parameter :: C9869604401089358618834490999876 = &
                                                            XMM4r4_t(9.869604401089358618834490999876_sp)
                   type(XMM4r4_t),  parameter :: C2467401100272339654708622749969 = &
                                                            XMM4r4_t(2.467401100272339654708622749969_sp)
                   type(XMM4r4_t),  parameter :: C08905  = XMM4r4_t(0.8905_sp)
                   type(XMM4r4_t),  parameter :: C05     = XMM4r4_t(0.5_sp)
                   type(XMM4r4_t),  automatic :: abh,k0abh,num,sqr1
                   type(XMM4r4_t),  automatic :: sqr2,c0k0,arg,larg
                   type(XMM4r4_t),  automatic :: x0,x1,den
#if (GMS_EXPLICIT_VECTORIZE) == 1
                      integer(kind=i4) :: j
                      !dir$ loop_count(4)
                      !dir$ vector aligned
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do j=0, 3  
                            abh.v(j)   = a.v(j)*b.v(j)*C05.v(j)
                            c0k0.v(j)  = C08905.v(j)*k0.v(j)
                            num.v(j)   = C9869604401089358618834490999876.v(j)* &
                                         abh.v(j)
                            arg.v(j)   = c0k0.v(j)*abh.v(j)
                            larg.v(j)  = log(arg.v(j))
                            x0.v(j)    = larg.v(j)*larg.v(j)+ &
                                         C2467401100272339654708622749969.v(j)
                            sqr1.v(j)  = sqrt(k0.v(j)*abh.v(j))
                            sqr2.v(j)  = sqrt(x0.v(j))
                            den.v(j)   = sqr1.v(j)*sqr2.v(j)
                            x1.v(j)    = den.v(j)*den.v(j)
                            rcs.v(j)   = num.v(j)/x1.v(j)
                      end do  
#else                 
                   abh.v   = a.v*b.v*C05.v
                   c0k0.v  = C08905.v*k0.v
                   num.v   = C9869604401089358618834490999876.v* &
                             abh.v
                   arg.v   = c0k0.v*abh.v
                   larg.v  = log(arg.v)
                   x0.v    = larg.v*larg.v+ &
                             C2467401100272339654708622749969.v
                   sqr1.v  = sqrt(k0.v*abh.v)
                   sqr2.v  = sqrt(x0.v)
                   den.v   = sqr1.v*sqr2.v
                   x1.v    = den.v*den.v
                   rcs.v   = num.v/x1.v
#endif
            end function rcs_f4413_xmm4r4
            
            ! /*
            !             High frequency approximations (k0a>5, k0b>5)
            !             TM-case, formula 4.4-15
            !          */
            
            !  /*
            !            Helper function for testing the condition of high-frequency limit.
            !            Page. 322.
            !!
            !         */
            
            pure function TM_f4415_helper_xmm4r4(k0,a,phi1,phi2,b) result(msk)
                
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: TM_f4415_helper_xmm4r4
                   !dir$ attributes forceinline :: TM_f4415_helper_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: TM_f4415_helper_xmm4r4
                   use mod_vecconsts, only :  v4r4_1, v4r4_0
                   use mod_kinds, only : i1
                   type(XMM4r4_t),  intent(in) :: k0a
                   type(XMM4r4_t),  intent(in) :: a
                   type(XMM4r4_t),  intent(in) :: phi1
                   type(XMM4r4_t),  intent(in) :: phi2
                   type(XMM4r4_t),  intent(in) :: b
                   integer(kind=i2) :: msk
                     !dir$ attributes align : 16 :: C314159265358979323846264338328 
                     !dir$ attributes align : 16 :: C0166666666666666666666666666667
                     !dir$ attributes align : 16 :: a1
                     !dir$ attributes align : 16 :: b2
                     !dir$ attributes align : 16 :: sphi
                     !dir$ attributes align : 16 :: cphi
                     !dir$ attributes align : 16 :: trm1
                     !dir$ attributes align : 16 :: trm2
                     !dir$ attributes align : 16 :: rt6
                     !dir$ attributes align : 16 :: k02
                     !dir$ attributes align : 16 :: absp
                     !dir$ attributes align : 16 :: sphi1s
                     !dir$ attributes align : 16 :: cphi1s
                     !dir$ attributes align : 16 :: k0a2
                     !dir$ attributes align : 16 :: k0b2
                     !dir$ attributes align : 16 :: x0
                   type(XMM4r4_t),  parameter :: C314159265358979323846264338328  = &
                                                        XMM4r4_t(3.14159265358979323846264338328_sp)
                   type(XMM4r4_t),  parameter :: C0166666666666666666666666666667 = &
                                                        XMM4r4_t(0.166666666666666666666666666667_sp)
                   type(XMM4r4_t),  automatic :: a1,b2,sphi,cphi
                   type(XMM4r4_t),  automatic :: trm1,trm2,rt6,k02
                   type(XMM4r4_t),  automatic :: absp,sphi1s,cphi1s
                   type(XMM4r4_t),  automatic :: k0a2,k0b2,x0
                   logical(kind=i4), dimension(0:15) :: mre
#if (GMS_EXPLICIT_VECTORIZE) == 1
                      integer(kind=i4) :: j
                      !dir$ loop_count(4)
                      !dir$ vector aligned
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do j=0, 3  
                             k02.v(j)   = k0.v(j)*k0.v(j)
                             a2.v(j)    = a.v(j)*a.v(j)
                             k0a2.v(j)  = k02.v(j)*a2.v(j)
                             b2.v(j)    = b.v(j)*b.v(j)
                             k0b2.v(j)  = k02.v(j)*b2.v(j)
                             cphi1.v(j) = cos(phi1.v(j))
                             absp.v(j)  = abs(phi2.v(j)-phi1.v(j))
                             cphi1s.v(j)= cphi1.v(j)*cphi1.v(j)
                             sphi1.v(j) = sin(phi1.v(j))
                             trm1.v(j)  = C314159265358979323846264338328.v(j)* &
                                          absp.v(j)
                             sphi1s.v(j)= sphi1.v(j)*sphi1.v(j)
                             trm2.v(j)  = k02a2.v(j)*sphi1s.v(j)+ &
                                          k02b2.v(j)*cphi1s.v(j)
                             x0.v(j)    = trm2.v(j)** & 
                                          C0166666666666666666666666666667.v(j)
                             rt6.v(j)   =  v4r4_1.v(j)/x0.v(j)
                             mre(j)     = (trm1.v(j)>=rt6.v(j))
                      end do
                      msk = all(mre)
#else
                   k02.v   = k0.v*k0.v
                   a2.v    = a.v*a.v
                   k0a2.v  = k02.v*a2.v
                   b2.v    = b.v*b.v
                   k0b2.v  = k02.v*b2.v
                   cphi1.v = cos(phi1)
                   mre     = .false.
                   absp.v  = abs(phi2.v-phi1.v)
                   cphi1s.v= cphi1.v*cphi1.v
                   sphi1.v = sin(phi1.v)
                   trm1.v  = C314159265358979323846264338328.v* &
                             absp.v
                   sphi1s.v= sphi1.v*sphi1.v
                   trm2.v  = k02a2.v*sphi1s.v+k02b2.v*cphi1s.v
                   x0.v    = trm2.v** & 
                             C0166666666666666666666666666667.v
                   msk     = .false.
                   rt6.v   =  v4r4_1.v/x0.v
                   mre     = (trm1.v>=rt6.v)
                   msk     = all(mre)
#endif
            end function TM_f4415_helper_xmm4r4
            
            
            subroutine TM_f4415_xmm4r4(phi1,phi2,a,b,k0,TM,stat)
                
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: TM_f4415_xmm4r4
                   !dir$ attributes forceinline :: TM_f4415_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: TM_f4415_xmm4r4
                   use mod_kinds, only : i2
                   use mod_vecconsts, only :  v4r4_0
                   type(XMM4r4_t),  intent(in) :: phi1,
                   type(XMM4r4_t),  intent(in) :: phi2
                   type(XMM4r4_t),  intent(in) :: a
                   type(XMM4r4_t),  intent(in) :: b
                   type(XMM4r4_t),  intent(in) :: k0
                   type(  XMM4c4_t),    intent(out):: TM
                   logical(kind=i2), intent(out):: stat
                     !dir$ attributes align : 16 :: C314159265358979323846264338328
                     !dir$ attributes align : 16 :: C078539816339744830961566084582
                     !dir$ attributes align : 16 :: C05
                     !dir$ attributes align : 16 :: C15
                     !dir$ attributes align : 16 :: ea
                     !dir$ attributes align : 16 :: ce
                     !dir$ attributes align : 16 :: arg1
                     !dir$ attributes align : 16 :: arg2
                     !dir$ attributes align : 16 :: carg1
                     !dir$ attributes align : 16 :: carg2
                     !dir$ attributes align : 16 :: sarg2
                     !dir$ attributes align : 16 :: sqr1
                     !dir$ attributes align : 16 :: trm1
                     !dir$ attributes align : 16 :: f
                     !dir$ attributes align : 16 :: rho
                     !dir$ attributes align : 16 :: a2b2
                     !dir$ attributes align : 16 :: a2
                     !dir$ attributes align : 16 :: b2
                     !dir$ attributes align : 16 :: k0a
                     !dir$ attributes align : 16 :: cphi2
                     !dir$ attributes align : 16 :: cphi1
                     !dir$ attributes align : 16 :: sphi1
                     !dir$ attributes align : 16 :: sphi2
                     !dir$ attributes align : 16 :: frat
                     !dir$ attributes align : 16 :: cphis
                     !dir$ attributes align : 16 :: sphis
                     !dir$ attributes align : 16 :: rhod
                     !dir$ attributes align : 16 :: rhorat
                     !dir$ attributes align : 16 :: x0
                     !dir$ attributes align : 16 :: x1
                     !dir$ attributes align : 16 :: tmp1
                     !dir$ attributes align : 16 :: tmp2
                     !dir$ attributes align : 16 :: b2a2s
                     !dir$ attributes align : 16 :: carg2s
                     !dir$ attributes align : 16 :: sarg2s
                   type(XMM4r4_t),  parameter :: C314159265358979323846264338328  = &
                                                        XMM4r4_t(3.14159265358979323846264338328_sp)
                   type(XMM4r4_t),  parameter :: C078539816339744830961566084582 = &
                                                        XMM4r4_t(0.78539816339744830961566084582_sp)
                   type(XMM4r4_t),  parameter :: C05 = XMM4r4_t(0.5_sp)
                   type(XMM4r4_t),  parameter :: C15 = XMM4r4_t(1.5_sp)
                   type(  XMM4c4_t),    automatic :: ea,ce
                   type(XMM4r4_t),  automatic :: arg1,arg2,carg1,carg2
                   type(XMM4r4_t),  automatic :: sarg2,sqr1,trm1,f
                   type(XMM4r4_t),  automatic :: rho,a2b2,a2,b2
                   type(XMM4r4_t),  automatic :: k0a,cphi2,cphi1,sphi1
                   type(XMM4r4_t),  automatic :: sphi2,frat,cphis,sphis
                   type(XMM4r4_t),  automatic :: rhod,rhorat,x0,x1
                   type(XMM4r4_t),  automatic :: tmp1,tmp2,b2a2s,carg2s
                   type(XMM4r4_t),  automatic :: sarg2s
                   integer(kind=i2), automatic :: msk
                   msk      = TM_f4415_helper_xmm4r4(k0,a,phi1,phi2,b)
                   if(msk==.false.) then
                      stat = .false.
                      return
                   end if
                   arg1.v   = C05.v*(phi2.v-phi1.v)
                   cphi1.v  = cos(phi1.v)
                   a2.v     = a.v*a.v
                   b2.v     = b.v*b.v
                   sphi1.v  = sin(phi1.v)
                   k0a.v    = k0.v*a.v
                   arg2.v   = C05.v*(phi2.v+phi1.v)
                   carg1.v  = cos(arg1.v)
                   a2b2.v   = a2.v*b2.v
                   b2a2.v   = b2.v/a2.v
                   trm1.v   = sqrt(C314159265358979323846264338328.v* &
                                   carg1.v)
                   cphi2.v  = cos(phi2.v)
                   cphis.v  = cphi1.v*cphi2.v
                   sphi2.v  = sin(phi2.v)
                   sphis.v  = sphi1.v*sphi2.v
                   carg2.v  = cos(arg2.v)
                   sarg2.v  = sin(arg2.v)
                   x0.v     = carg2.v*carg2.v
                   x1.v     = sarg2.v*sarg2.v
                   rhod.v   = a2.v*x0.v+b2.v*x1.v
                   b2a2s.v  = b2a2.v*sphis.v
                   tmp1.v   = rhod.v*C15.v
                   rhorat.v = a2b2.v/tmp1.v
                   x0.v     = sarg2.v*b2a2s.v+carg2.v
                   carg2s.v = carg2.v*carg2.v
                   tmp2.v   = cphis.v*x0.v
                   sarg2s.v = sarg2.v*sarg2.v
                   x1.v     = b2a2.v*sarg2s.v+carg2s.v
                   tmp1.v   = sqrt(x1.v)
                   frat.v   = tmp2.v/tmp1.v
                   trm1.v   = -trm1.v
                   ea.re    = C078539816339744830961566084582.v
                   x0.v     = C05.v*sqrt(k0.v*rhorat.v)
                   ea.im    = -(k0a.v*frat.v)
                   x1.v     = trm1.v*x0.v
                   ea.im    = C078539816339744830961566084582.v* &
                              ea.im
                   ce       = cexp_xmm4c4(ea)
                   TM       = ce*x1
                   stat     = .true.
            end subroutine TM_f4415_xmm4r4
            
            
            
            !        /*
            !             High frequency approximations (k0a>5, k0b>5)
            !             TE-case, formula 4.4-16
            !          */
            
            subroutine TE_f4416_xmm4r4(phi1,phi2,a,b,k0,TE,stat) 
                 
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: TE_f4416_xmm4r4
                   !dir$ attributes forceinline :: TE_f4416_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: TE_f4416_xmm4r4
                   use mod_kinds, only : i2
                   type(XMM4r4_t),  intent(in) :: phi1,
                   type(XMM4r4_t),  intent(in) :: phi2
                   type(XMM4r4_t),  intent(in) :: a
                   type(XMM4r4_t),  intent(in) :: b
                   type(XMM4r4_t),  intent(in) :: k0
                   type(  XMM4c4_t),    intent(out):: TM
                   logical(kind=i2), intent(out):: stat 
                
                   TM_f4415_xmm4r4(phi1,phi2,a,b,k0,TE,stat)
            end subroutine TE_f4416_xmm4r4
            
            
            ! /*
            ! 
            !            Bistatic scattering width.
            !            Formula 4.4-19
            !       */
            
            pure function rcs_f4419_xmm4r4(phi1,phi2,a,b) result(rcs)
                
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4419_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4419_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4419_xmm4r4
                   type(XMM4r4_t),  intent(in) :: phi1,
                   type(XMM4r4_t),  intent(in) :: phi2
                   type(XMM4r4_t),  intent(in) :: a
                   type(XMM4r4_t),  intent(in) :: b
                   type(XMM4r4_t) :: rcs
                      !dir$ attributes align : 16 :: C314159265358979323846264338328 
                     !dir$ attributes align : 16 :: C05
                     !dir$ attributes align : 16 :: a2
                     !dir$ attributes align : 16 :: b2
                     !dir$ attributes align : 16 :: a2b2
                     !dir$ attributes align : 16 :: num
                     !dir$ attributes align : 16 :: carg
                     !dir$ attributes align : 16 :: carg2
                     !dir$ attributes align : 16 :: sarg
                     !dir$ attributes align : 16 :: sarg2
                     !dir$ attributes align : 16 :: pow32
                     !dir$ attributes align : 16 :: x0
                   type(XMM4r4_t),  parameter :: C314159265358979323846264338328  = &
                                                        XMM4r4_t(3.14159265358979323846264338328_sp)
                   type(XMM4r4_t),  parameter :: C05 = XMM4r4_t(0.5_sp)
                   type(XMM4r4_t),  parameter :: C15 = XMM4r4_t(1.5_sp)
                   type(XMM4r4_t),  automatic :: a2,b2,a2b2,num
                   type(XMM4r4_t),  automatic :: arg,carg,carg2
                   type(XMM4r4_t),  automatic :: sarg,sarg2
                   type(XMM4r4_t),  automatic :: pow32,x0
#if (GMS_EXPLICIT_VECTORIZE) == 1
                      integer(kind=i4) :: j
                      !dir$ loop_count(4)
                      !dir$ vector aligned
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do j=0, 3  
                           a2.v(j)    = a.v(j)*a.v(j)
                           arg.v(j)   = C05.v(j)*(phi2.v(j)+phi1.v(j))
                           b2.v(j)    = b.v(j)*b.v(j)
                           carg.v(j)  = cos(arg.v(j))
                           num.v(j)   = C314159265358979323846264338328.v(j)* &
                                        a2.v(j)*b2.v(j)
                           sarg.v(j)  = sin(arg.v(j))
                           carg2.v(j) = carg.v(j)*carg.v(j)
                           sarg2.v(j) = sarg.v(j)*sarg.v(j)
                           x0.v(j)    = a2.v(j)*carg2.v(j)+ & 
                                        b2.v(j)*sarg2.v(j)
                           pow32.v(j) = x0.v(j)**C15.v(j)
                           rcs.v(j)   = num.v(j)/pow32.v(j)
                      end do
#else
                   a2.v    = a.v*a.v
                   arg.v   = C05.v*(phi2.v+phi1.v)
                   b2.v    = b.v*b.v
                   carg.v  = cos(arg.v)
                   num.v   = C314159265358979323846264338328.v* &
                             a2.v*b2.v
                   sarg.v  = sin(arg.v)
                   carg2.v = carg.v*carg.v
                   sarg2.v = sarg.v*sarg.v
                   x0.v    = a2.v*carg2.v+b2.v*sarg2.v
                   pow32.v = x0.v**C15.v
                   rcs.v   = num.v/pow32.v
#endif
            end function rcs_f4419_xmm4r4
            
           
            
            !       /*
            !!
            !              Backscattering width, for phi2 == phi1.
            !              Formula 4.4-20
            !          */
            
            
            pure function rcs_f4420_xmm4r4(a,b,phi) result(rcs)
                
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4420_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4420_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4420_xmm4r4
                   type(XMM4r4_t),  intent(in) :: a
                   type(XMM4r4_t),  intent(in) :: b
                   type(XMM4r4_t),  intent(in) :: phi
                   type(XMM4r4_t) :: rcs
                     !dir$ attributes align : 16 :: C314159265358979323846264338328 
                     !dir$ attributes align : 16 :: C15
                     !dir$ attributes align : 16 :: a2
                     !dir$ attributes align : 16 :: b2
                     !dir$ attributes align : 16 :: a2b2
                     !dir$ attributes align : 16 :: num
                     !dir$ attributes align : 16 :: carg
                     !dir$ attributes align : 16 :: carg2
                     !dir$ attributes align : 16 :: sarg
                     !dir$ attributes align : 16 :: sarg2
                     !dir$ attributes align : 16 :: pow32
                     !dir$ attributes align : 16 :: x0
                   type(XMM4r4_t),  parameter :: C314159265358979323846264338328  = &
                                                        XMM4r4_t(3.14159265358979323846264338328_sp)
                   type(XMM4r4_t),  parameter :: C15 = XMM4r4_t(1.5_sp)
                   type(XMM4r4_t),  automatic :: a2,b2,a2b2
                   type(XMM4r4_t),  automatic :: num,carg,carg2
                   type(XMM4r4_t),  automatic :: sarg,sarg2
                   type(XMM4r4_t),  automatic :: pow32,x0
#if (GMS_EXPLICIT_VECTORIZE) == 1
                      integer(kind=i4) :: j
                      !dir$ loop_count(4)
                      !dir$ vector aligned
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do j=0, 3  
                           a2.v(j)   = a.v(j)*a.v(j)
                           carg.v(j) = cos(phi.v(j))
                           b2.v(j)   = b.v(j)*b.v(j)
                           carg2.v(j)= carg.v(j)*carg.v(j)
                           num.v(j)  = C314159265358979323846264338328.v(j)* &
                                       a2.v(j)*b2.v(j)
                           sarg.v(j) = sin(phi.v(j))
                           sarg2.v(j)= sarg.v(j)*sarg.v(j)
                           x0.v(j)   = a2.v(j)*carg2.v(j)+b2.v(j)*sarg2.v(j)
                           pow32.v(j)= x0.v(j)**C15.v(j)
                           rcs.v(j)  = num.v(j)/pow32.v(j)
                      end do
#else                   
                   a2.v   = a.v*a.v
                   carg.v = cos(phi.v)
                   b2.v   = b.v*b.v
                   carg2.v= carg.v*carg.v
                   num.v  = C314159265358979323846264338328.v* &
                            a2.v*b2.v
                   sarg.v = sin(phi.v)
                   sarg2.v= sarg.v*sarg.v
                   x0.v   = a2.v*carg2.v+b2.v*sarg2.v
                   pow32.v= x0.v**C15.v
                   rcs.v  = num.v/pow32.v
#endif
            end function rcs_f4420_xmm4r4
            
            
            !  /*
            !            Forward scattering pattern and width.
            !            Formula 4.4-23 a scattering amplitude
            !!
            !        */
            
            pure function T_f4423_helper_xmm4r4(k0,a,phi1,phi2,b) result(msk)
                 
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: T_f4423_helper_xmm4r4
                   !dir$ attributes forceinline :: T_f4423_helper_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: T_f4423_helper_xmm4r4
                   use mod_vecconsts, only :  v4r4_1
                   use mod_kinds,    only : i2
                   type(XMM4r4_t),   intent(in) :: k0
                   type(XMM4r4_t),   intent(in) :: a
                   type(XMM4r4_t),   intent(in) :: phi1
                   type(XMM4r4_t),   intent(in) :: phi2
                   type(XMM4r4_t),   intent(in) :: b
                   logical(kind=i2) :: msk
                     !dir$ attributes align : 16 :: C314159265358979323846264338328
                     !dir$ attributes align : 16 :: C0166666666666666666666666666667
                     !dir$ attributes align : 16 :: a2
                     !dir$ attributes align : 16 :: b2
                     !dir$ attributes align : 16 :: sphi1
                     !dir$ attributes align : 16 :: cphi1
                     !dir$ attributes align : 16 :: trm1
                     !dir$ attributes align : 16 :: trm2
                     !dir$ attributes align : 16 :: rt6
                     !dir$ attributes align : 16 :: k02
                     !dir$ attributes align : 16 :: absp
                     !dir$ attributes align : 16 :: sphi1s
                     !dir$ attributes align : 16 :: cphi1s
                     !dir$ attributes align : 16 :: k0a2
                     !dir$ attributes align : 16 :: k0b2
                     !dir$ attributes align : 16 :: x0
                   type(XMM4r4_t),  parameter :: C314159265358979323846264338328  = &
                                                        XMM4r4_t(3.14159265358979323846264338328_sp)
                   type(XMM4r4_t),   parameter :: C0166666666666666666666666666667 = &
                                                           XMM4r4_t(0.166666666666666666666666666667_sp)
                   type(XMM4r4_t),   automatic :: a2,b2,sphi1,cphi1
                   type(XMM4r4_t),   automatic :: trm1,trm2,rt6,k02
                   type(XMM4r4_t),   automatic :: absp,sphi1s,cphi1s
                   type(XMM4r4_t),   automatic :: k0a2,k0b2,x0
                   logical(kind=i4), dimension(0:15), automatic :: mre
#if (GMS_EXPLICIT_VECTORIZE) == 1
                      integer(kind=i4) :: j
                      !dir$ loop_count(4)
                      !dir$ vector aligned
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do j=0, 3    
                            k02.v(j)   = k0.v(j)*k0.v(j)
                            a2.v(j)    = a.v(j)*a.v(j)
                            cphi1.v(j) = cos(phi1.v(j))
                            k0a2.v(j)  = k02.v(j)*a2.v(j)
                            b2.v(j)    = b.v(j)*b.v(j)
                            sphi1.v(j) = sin(phi1.v(j))
                            k0b2.v(j)  = k02.v(j)*b2.v(j)
                            trm1.v(j)  = phi1.v(j)-phi2.v(j)
                            sphi1s.v(j)= sphi1.v(j)*sphi1.v(j)
                            cphi1s.v(j)= C314159265358979323846264338328.v(j)* &
                                         cphi1.v(j)*cphi1.v(j)
                            sphi1s.v(j)= sphi1.v(j)*sphi1.v(j)
                            trm2.v(j)  = k0a2.v(j)*sphi1s.v(j)+ &
                                         k02b2.v(j)*cphi1s.v(j)
                            x0.v(j)    = trm2.v(j)* &
                                         C0166666666666666666666666666667.v(j)
                            rt6.v(j)   =  v4r4_1.v(j)/x0.v(j)
                            mre(j)     = (abs(trm1.v(j))<rt6.v(j))
                      end do  
                      msk     = all(mre) 
#else             
                   k02.v   = k0.v*k0.v
                   a2.v    = a.v*a.v
                   cphi1.v = cos(phi1.v)
                   k0a2.v  = k02.v*a2.v
                   b2.v    = b.v*b.v
                   sphi1.v = sin(phi1.v)
                   k0b2.v  = k02.v*b2.v
                   trm1.v  = phi1.v-phi2.v
                   sphi1s.v= sphi1.v*sphi1.v
                   cphi1s.v= C314159265358979323846264338328.v* &
                             cphi1.v*cphi1.v
                   sphi1s.v= sphi1.v*sphi1.v
                   trm2.v  = k0a2.v*sphi1s.v+k02b2.v*cphi1s.v
                   x0.v    = trm2.v* &
                             C0166666666666666666666666666667.v
                   mre     = .false.
                   msk     = .false.
                   rt6.v   =  v4r4_1.v/x0.v
                   mre     = (abs(trm1.v)<rt6.v)
                   msk     = all(mre)
#endif
            end function T_f4423_helper_xmm4r4
            
            
            subroutine T_f4423_xmm4r4(a,b,phi1,phi2,k0,T,stat)
                
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: T_f4423_helper_xmm4r4
                   !dir$ attributes forceinline :: T_f4423_helper_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: T_f4423_helper_xmm4r4
                   use mod_vecconsts, only :  v4r4_1
                   use mod_kinds,     only : i2
                   type(XMM4r4_t),   intent(in) :: a
                   type(XMM4r4_t),   intent(in) :: b
                   type(XMM4r4_t),   intent(in) :: phi1
                   type(XMM4r4_t),   intent(in) :: phi2
                   type(XMM4r4_t),   intent(in) :: k0
                   type(XMM4r4_t),   intent(out):: T
                   logical(kind=i2),  intent(out):: stat 
                     !dir$ attributes align : 16 :: C314159265358979323846264338328 
                     !dir$ attributes align : 16 :: k0c
                     !dir$ attributes align : 16 :: c
                     !dir$ attributes align : 16 :: alp
                     !dir$ attributes align : 16 :: a2
                     !dir$ attributes align : 16 :: b2
                     !dir$ attributes align : 16 :: sphi
                     !dir$ attributes align : 16 :: sphi2
                     !dir$ attributes align : 16 :: cphi
                     !dir$ attributes align : 16 :: cphi2
                     !dir$ attributes align : 16 :: arg
                     !dir$ attributes align : 16 :: sarg
                     !dir$ attributes align : 16 :: rat
                     !dir$ attributes align : 16 :: x0
                     !dir$ attributes align : 16 :: tmp
                   type(XMM4r4_t),  parameter :: C314159265358979323846264338328  = &
                                                        XMM4r4_t(3.14159265358979323846264338328_sp)
                   type(XMM4r4_t),   automatic :: k0c,c,alp,a2,b2
                   type(XMM4r4_t),   automatic :: sphi,sphi2,cphi,cphi2
                   type(XMM4r4_t),   automatic :: arg,sarg,rat,x0
                   logical(kind=i2),  automatic :: tmp
                   tmp = T_f4423_helper_xmm4r4(k0,a,phi1,phi2,b)
                   if(tmp==.false.) then
                      stat = tmp
                      return
                   end if
#if (GMS_EXPLICIT_VECTORIZE) == 1
                      integer(kind=i4) :: j
                      !dir$ loop_count(4)
                      !dir$ vector aligned
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do j=0, 3    
                          a2.v(j)    = a.v(j)*a.v(j)
                          sphi.v(j)  = sin(phi1.v(j))
                          alp.v(j)   = C314159265358979323846264338328.v(j)* &
                                       (phi2.v(j)-phi1.v(j))
                          sphi2.v(j) = sphi.v*sphi.v(j)
                          cphi.v(j)  = cos(phi1.v(j))
                          b2.v(j)    = b.v(j)*b.v(j)
                          cphi2.v(j) = cphi.v(j)*cphi.v(j)
                          x0.v(j)    = a2.v(j)*cphi2.v(j)+ &
                                       b2.v(j)*sphi2.v(j)
                          c.v(j)     = sqrt(x0.v(j))
                          k0c.v(j)   = k0.v(j)*c.v(j)
                          arg.v(j)   = k0c.v(j)*alp.v(j)
                          sarg.v(j)  = sin(arg.v(j))
                          k0c.v(j)   = -k0c.v(j)
                          rat.v(j)   = sarg.v(j)/arg.v(j)
                          T.v(j)     = k0c.v(j)*rat.v(j)
                      end do
                   stat    = .true.
#else                   
                   a2.v    = a.v*a.v
                   sphi.v  = sin(phi1.v)
                   alp.v   = C314159265358979323846264338328.v* &
                             (phi2.v-phi1.v)
                   sphi2.v = sphi.v*sphi.v
                   cphi.v  = cos(phi1.v)
                   b2.v    = b.v*b.v
                   cphi2.v = cphi.v*cphi.v
                   x0.v    = a2.v*cphi2.v+b2.v*sphi2.v
                   c.v     = sqrt(x0.v)
                   k0c.v   = k0.v*c.v
                   arg.v   = k0c.v*alp.v
                   sarg.v  = sin(arg.v)
                   k0c.v   = -k0c.v
                   rat.v   = sarg.v/arg.v
                   T.v     = k0c.v*rat.v
                   stat    = .true.
#endif
            end subroutine T_f4423_xmm4r4
            
            
            !       /*
            !              Scattering width near the forward direction.
            !              Formula 4.4-24
            !!
            !         */
            
            subroutine rcs_f4424_xmm4r4(a,b,phi1,phi2,k0,rcs,stat)
                
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4424_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4424_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4424_xmm4r4
                   use mod_kinds,     only : i2
                   type(XMM4r4_t),   intent(in) :: a
                   type(XMM4r4_t),   intent(in) :: b
                   type(XMM4r4_t),   intent(in) :: phi1
                   type(XMM4r4_t),   intent(in) :: phi2
                   type(XMM4r4_t),   intent(in) :: k0
                   type(XMM4r4_t),   intent(out):: rcs
                   logical(kind=i2),  intent(out):: stat 
                     !dir$ attributes align : 16 :: C314159265358979323846264338328
                     !dir$ attributes align : 16 :: C40
                     !dir$ attributes align : 16 :: k0c
                     !dir$ attributes align : 16 :: c
                     !dir$ attributes align : 16 :: alp
                     !dir$ attributes align : 16 :: a2
                     !dir$ attributes align : 16 :: b2
                     !dir$ attributes align : 16 :: sphi
                     !dir$ attributes align : 16 :: sphi2
                     !dir$ attributes align : 16 :: cphi2
                     !dir$ attributes align : 16 :: cphi
                     !dir$ attributes align : 16 :: arg
                     !dir$ attributes align : 16 :: sarg
                     !dir$ attributes align : 16 :: rat
                     !dir$ attributes align : 16 :: x0
                     !dir$ attributes align : 16 :: x1
                     !dir$ attributes align : 16 :: x2
                   type(XMM4r4_t),   parameter :: C314159265358979323846264338328  = &
                                                        XMM4r4_t(3.14159265358979323846264338328_sp)
                   type(XMM4r4_t),   parameter :: C40 = XMM4r4_t(4.0_sp)
                   type(XMM4r4_t),   automatic :: k0c,c,alp
                   type(XMM4r4_t),   automatic :: a2,b2,
                   type(XMM4r4_t),   automatic :: sphi,sphi2
                   type(XMM4r4_t),   automatic :: cphi2,cphi
                   type(XMM4r4_t),   automatic :: arg,sarg
                   type(XMM4r4_t),   automatic :: rat,x0,x1,x2
#if (GMS_EXPLICIT_VECTORIZE) == 1
                      integer(kind=i4) :: j
                      !dir$ loop_count(4)
                      !dir$ vector aligned
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do j=0, 3    
                          a2.v(j)   = a.v(j)*a.v(j)
                          sphi.v(j) = sin(phi1.v(j))
                          alp.v(j)  = C314159265358979323846264338328.v(j)* &
                                      (phi2.v(j)-phi1.v(j))
                          sphi2.v(j)= sphi.v(j)*sphi.v(j)
                          b2.v(j)   = b.v(j)*b.v(j)
                          cphi.v(j) = cos(phi1.v(j))
                          cphi2.v(j)= cphi.v(j)*cphi.v(j)
                          x0.v(j)   = a2.v(j)*cphi2.v(j)+ &
                                      b2.v(j)*sphi2.v(j)
                          c.v(j)    = sqrt(x0.v(j))
                          k0c.v(j)  = k0.v(j)*c.v(j)
                          arg.v(j)  = k0c.v(j)*alp.v(j)
                          sarg.v(j) = sin(arg.v(j))
                          x1.v(j)   = C40.v(j)*k0c.v(j)*k0c.v(j)
                          rat.v(j)  = sarg.v(j)/arg.v(j)
                          x2.v(j)   = rat.v(j)*rat.v(j)
                          rcs.v(j)  = k0c.v(j)*rat.v(j)
                      end do
                   stat   = .true.
#else                   
                   a2.v   = a.v*a.v
                   sphi.v = sin(phi1.v)
                   alp.v  = C314159265358979323846264338328.v* &
                            (phi2.v-phi1.v)
                   sphi2.v= sphi.v*sphi.v
                   b2.v   = b.v*b.v
                   cphi.v = cos(phi1.v)
                   cphi2.v= cphi.v*cphi.v
                   x0.v   = a2.v*cphi2.v+b2.v*sphi2.v
                   c.v    = sqrt(x0.v)
                   k0c.v  = k0.v*c.v
                   arg.v  = k0c.v*alp.v
                   sarg.v = sin(arg.v)
                   x1.v   = C40.v*k0c.v*k0c.v
                   rat.v  = sarg.v/arg.v
                   x2.v   = rat.v*rat.v
                   rcs.v  = k0c.v*rat.v
                   stat   = .true.
#endif
            end subroutine rcs_f4424_xmm4r4
            
            
            
            !       /*
            !             Scattering width in the exact forward direction (alpha == 0).
            !             Formula 4.4-25
            !         */
            
            pure function rcs_f4425_xmm4r4(k0,a,b,phi) result(rcs)
                
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4425_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4425_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4425_xmm4r4
                   type(XMM4r4_t),    intent(in) :: k0
                   type(XMM4r4_t),    intent(in) :: a
                   type(XMM4r4_t),    intent(in) :: b
                   type(XMM4r4_t),    intent(in) :: phi
                   type(XMM4r4_t) :: rcs
                     !dir$ attributes align : 16 :: C40
                     !dir$ attributes align : 16 :: k04
                     !dir$ attributes align : 16 :: a2
                     !dir$ attributes align : 16 :: b2
                     !dir$ attributes align : 16 :: sphi
                     !dir$ attributes align : 16 :: spi2
                     !dir$ attributes align : 16 :: cphi
                     !dir$ attributes align : 16 :: cphi2
                   type(XMM4r4_t),   parameter :: C40 = XMM4r4_t(4.0_sp)
                   type(XMM4r4_t),   automatic :: k04,a2,b2
                   type(XMM4r4_t),   automatic :: sphi,sphi2
                   type(XMM4r4_t),   automatic :: cphi,cphi2
#if (GMS_EXPLICIT_VECTORIZE) == 1
                      integer(kind=i4) :: j
                      !dir$ loop_count(4)
                      !dir$ vector aligned
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do j=0, 3    
                           a2.v(j)   = a.v(j)*a.v(j)
                           sphi.v(j) = sin(phi.v(j))
                           b2.v(j)   = b.v(j)*b.v(j)
                           cphi.v(j) = cos(phi.v(j))
                           k04.v(j)  = C40.v(j)*k0.v(j)
                           cphi2.v(j)= cphi.v(j)*cphi.v(j)
                           sphi2.v(j)= sphi.v(j)*sphi.v(j)
                           x0.v(j)   = a2.v(j)*sphi2.v(j)+ &
                                       b2.v(j)*cphi2.v(j)
                           rcs.v(j)  = k04.v(j)*x0.v(j) 
                      end do 
#else                  
                   a2.v   = a.v*a.v
                   sphi.v = sin(phi.v)
                   b2.v   = b.v*b.v
                   cphi.v = cos(phi.v)
                   k04.v  = C40.v*k0.v
                   cphi2.v= cphi.v*cphi.v
                   sphi2.v= sphi.v*sphi.v
                   x0.v   = a2.v*sphi2.v+b2.v*cphi2.v
                   rcs.v  = k04.v*x0.v 
#endif
            end function rcs_f4425_xmm4r4
            
            !  /*
            !              Infinitely long homogenous cylinder at normal
            !              incidence.
            !              Low frequency approximation (k0a<0.5,k0b<0.5,k1a<0.5,k1b<0.5)
            !              TM-case, formula 4.4-26
            !         */
            
            subroutine TM_f4426_xmm4r4(k0,a,b,phi1,phi2,eps,mu,TM)
                
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: TM_f4426_xmm4r4
                   !dir$ attributes forceinline :: TM_f4426_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: TM_f4426_xmm4r4
                   use mod_vecconsts, only :  v4r4_1, v4r4_0
                   type(XMM4r4_t),   intent(in) :: k0
                   type(XMM4r4_t),   intent(in) :: a
                   type(XMM4r4_t),   intent(in) :: b
                   type(XMM4r4_t),   intent(in) :: phi1
                   type(XMM4r4_t),   intent(in) :: phi2
                   type(  XMM4c4_t),     intent(in) :: eps
                   type(  XMM4c4_t),     intent(in) :: mu
                   type(  XMM4c4_t),     intent(out):: TM
                     !dir$ attributes align : 16 :: C078539816339744830961566084582 
                     !dir$ attributes align : 16 :: epsm1
                     !dir$ attributes align : 16 :: mum1
                     !dir$ attributes align : 16 :: mupba
                     !dir$ attributes align : 16 :: mumba
                     !dir$ attributes align : 16 :: tc0
                     !dir$ attributes align : 16 :: tc1
                     !dir$ attributes align : 16 :: tc2
                     !dir$ attributes align : 16 :: fac
                     !dir$ attributes align : 16 :: tc3
                     !dir$ attributes align : 16 :: tmp
                     !dir$ attributes align : 16 :: k0a
                     !dir$ attributes align : 16 :: k0a2
                     !dir$ attributes align : 16 :: ba
                     !dir$ attributes align : 16 :: cphi2
                     !dir$ attributes align : 16 :: cphi1
                     !dir$ attributes align : 16 :: sphi2
                     !dir$ attributes align : 16 :: sphi1
                     !dir$ attributes align : 16 :: ba1
                     !dir$ attributes align : 16 :: cphit
                     !dir$ attributes align : 16 :: sphit
                   type(XMM4r4_t),   parameter :: C078539816339744830961566084582 = &
                                                        XMM4r4_t(0.78539816339744830961566084582_sp)
                   type(  XMM4c4_t),     automatic :: epsm1,mum1
                   type(  XMM4c4_t),     automatic :: mupba,mumba
                   type(  XMM4c4_t),     automatic :: tc0,tc1
                   type(  XMM4c4_t),     automatic :: tc2,fac
                   type(  XMM4c4_t),     automatic :: tc3,tmp
                   type(XMM4r4_t),   automatic :: k0a,k0a2,ba
                   type(XMM4r4_t),   automatic :: cphi2,cphi1
                   type(XMM4r4_t),   automatic :: sphi2,sphi1
                   type(XMM4r4_t),   automatic :: ba1,cphit,sphit
                   k0a.v   = k0.v*a.v
                   cphi1.v = cos(phi1.v)
                   ba.v    = b.v/a.v
                   epsm1   = eps- v4r4_1
                   sphi1.v = sin(phi1.v)
                   k0a2.v  = k0a.v*k0a.v
                   mum1    = mu- v4r4_1
                   cphi2.v = cos(phi2.v)
                   ba1.v   =  v4r4_1.v+ba.v
                   cphit.v = cphi2.v*cphi1.v
                   tc0     = epsm1-mum1
                   fac.im  =  v4r4_0.v
                   sphi2.v = sin(phi2.v)
                   fac.re  = C078539816339744830961566084582.v* &
                             k0a2.v*ba.v
                   sphit.v = sphi2.v*sphi1.v
                   mupba   = mu+ba
                   tc0     = cphit/mupba
                   mumba   = (mu*ba)+ v4r4_1
                   tc2     = sphit/mumba
                   tc3     = ba1*tc1+tc2
                   tmp     = tc0*tc3
                   TM      = fac*tmp
            end subroutine TM_f4426_xmm4r4
            
            
            !  /*
            !              Infinitely long homogenous cylinder at normal
            !              incidence.
            !              Low frequency approximation (k0a<0.5,k0b<0.5,k1a<0.5,k1b<0.5)
            !              TE-case, formula 4.4-27
            !         */
            
            subroutine TE_f4427_xmm4r4(k0,a,b,phi1,phi2,eps,mu,TE)
                
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: TE_f4426_xmm4r4
                   !dir$ attributes forceinline :: TM_f4426_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: TM_f4426_xmm4r4
                   use mod_vecconsts, only :  v4r4_1, v4r4_0
                   type(XMM4r4_t),   intent(in) :: k0
                   type(XMM4r4_t),   intent(in) :: a
                   type(XMM4r4_t),   intent(in) :: b
                   type(XMM4r4_t),   intent(in) :: phi1
                   type(XMM4r4_t),   intent(in) :: phi2
                   type(  XMM4c4_t),     intent(in) :: eps
                   type(  XMM4c4_t),     intent(in) :: mu
                   type(  XMM4c4_t),     intent(out):: TM
                     !dir$ attributes align : 16 :: C078539816339744830961566084582 
                   type(XMM4r4_t),   parameter :: C078539816339744830961566084582 = &
                                                        XMM4r4_t(0.78539816339744830961566084582_sp)
                     !dir$ attributes align : 16 :: epsm1
                     !dir$ attributes align : 16 :: mum1
                     !dir$ attributes align : 16 :: mupba
                     !dir$ attributes align : 16 :: mumba
                     !dir$ attributes align : 16 :: tc0
                     !dir$ attributes align : 16 :: tc1
                     !dir$ attributes align : 16 :: tc2
                     !dir$ attributes align : 16 :: fac
                     !dir$ attributes align : 16 :: tc3
                     !dir$ attributes align : 16 :: tmp
                     !dir$ attributes align : 16 :: k0a
                     !dir$ attributes align : 16 :: k0a2
                     !dir$ attributes align : 16 :: ba
                     !dir$ attributes align : 16 :: cphi2
                     !dir$ attributes align : 16 :: cphi1
                     !dir$ attributes align : 16 :: sphi2
                     !dir$ attributes align : 16 :: sphi1
                     !dir$ attributes align : 16 :: ba1
                     !dir$ attributes align : 16 :: cphit
                     !dir$ attributes align : 16 :: sphit
                   type(  XMM4c4_t),     automatic :: epsm1,mum1
                   type(  XMM4c4_t),     automatic :: epspba,epsmba
                   type(  XMM4c4_t),     automatic :: tc0,tc1
                   type(  XMM4c4_t),     automatic :: tc2,tc3
                   type(  XMM4c4_t),     automatic :: fac,tmp
                   type(XMM4r4_t),   automatic :: k0a,k0a2
                   type(XMM4r4_t),   automatic :: ba,cphi2
                   type(XMM4r4_t),   automatic :: cphi1,sphi2
                   type(XMM4r4_t),   automatic :: sphi1,ba1
                   type(XMM4r4_t),   automatic :: cphit,sphit
                   k0a.v   = k0.v*a.v
                   cphi1.v = cos(phi1.v)
                   ba.v    = b.v/a.v
                   epsm1   = eps- v4r4_1
                   sphi1.v = sin(phi1.v)
                   k0a2.v  = k0a.v*k0a.v
                   cphi2.v = cos(phi2.v)
                   cphit.v = cphi2.v*cphi1.v
                   mum1    = mu- v4r4_1
                   ba1.v   =  v4r4_1.v+ba.v
                   tc0     = mum1-epsm1
                   fac.im  =  v4r4_0.v
                   sphi2.v = sin(phi2.v)
                   fac.re  = C078539816339744830961566084582.v* &
                             k0a2.v*ba.v
                   epspba  = eps+ba
                   tc1     = cphit/epspba
                   epsmba  = eps*ba
                   tc2     = sphit/epsmba
                   tc3     = ba1*(tc1+tc2)
                   tmp     = tc0*tc3
                   TE      = fac*tmp
            end subroutine TE_f4427_xmm4r4
            
           !  /*
           !               Infinitely long homogenous cylinder at normal
           !               incidence.
           !               Low frequency approximation (k0a<0.5,k0b<0.5,k1a<0.5,k1b<0.5)
           !               Bistatic scattering width (RCS).
           !               TM-case.
           !               Formula 4.4-28
           !         */
           
           
           subroutine rcs_f4428_xmm4r4(k0,a,b,phi1,phi2,eps,mu,rcs) 
               
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4428_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4428_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4428_xmm4r4
                   use mod_vecconsts, only :  v4r4_1, v4r4_0
                   type(XMM4r4_t),   intent(in) :: k0
                   type(XMM4r4_t),   intent(in) :: a
                   type(XMM4r4_t),   intent(in) :: b
                   type(XMM4r4_t),   intent(in) :: phi1
                   type(XMM4r4_t),   intent(in) :: phi2
                   type(  XMM4c4_t),     intent(in) :: eps
                   type(  XMM4c4_t),     intent(in) :: mu
                   type(XMM4r4_t),   intent(out):: rcs
                      !dir$ attributes align : 16 :: C078539816339744830961566084582 
                   type(XMM4r4_t),   parameter :: C078539816339744830961566084582 = &
                                                        XMM4r4_t(0.78539816339744830961566084582_sp)
                   type(XMM4r4_t),   parameter :: C314159265358979323846264338328 = &
                                                        XMM4r4_t(3.14159265358979323846264338328_sp)
                     !dir$ attributes align : 16 :: epsm1
                     !dir$ attributes align : 16 :: mum1
                     !dir$ attributes align : 16 :: mupba
                     !dir$ attributes align : 16 :: mumba
                     !dir$ attributes align : 16 :: tc0
                     !dir$ attributes align : 16 :: tc1
                     !dir$ attributes align : 16 :: tc2
                     !dir$ attributes align : 16 :: tc3
                     !dir$ attributes align : 16 :: tmp
                     !dir$ attributes align : 16 :: k0a
                     !dir$ attributes align : 16 :: k0a2
                     !dir$ attributes align : 16 :: k0a3
                     !dir$ attributes align : 16 :: cphi2
                     !dir$ attributes align : 16 :: cphi1
                     !dir$ attributes align : 16 :: sphi2
                     !dir$ attributes align : 16 :: sphi1
                     !dir$ attributes align : 16 :: b2
                     !dir$ attributes align : 16 :: a2
                     !dir$ attributes align : 16 :: pia
                     !dir$ attributes align : 16 :: b2a2
                     !dir$ attributes align : 16 :: fac
                     !dir$ attributes align : 16 :: ba1
                     !dir$ attributes align : 16 :: cphit
                     !dir$ attributes align : 16 :: sphit
                     !dir$ attributes align : 16 :: cab
                   type(  XMM4c4_t),     automatic :: epsm1,mum1
                   type(  XMM4c4_t),     automatic :: mupba,mumba
                   type(  XMM4c4_t),     automatic :: tc0,tc1
                   type(  XMM4c4_t),     automatic :: tc2,tc3
                   type(  XMM4c4_t),     automatic :: tmp
                   type(XMM4r4_t),   automatic :: k0a,k0a2
                   type(  XMM4c4_t),     automatic :: k0a3,cphi2
                   type(  XMM4c4_t),     automatic :: cphi1,sphi2
                   type(  XMM4c4_t),     automatic :: sphi1,b2
                   type(  XMM4c4_t),     automatic :: a2,pia
                   type(  XMM4c4_t),     automatic :: b2a2,fac
                   type(  XMM4c4_t),     automatic :: ba1,cphit
                   type(  XMM4c4_t),     automatic :: sphit,cab
                   b2.v    = b.v*b.v
                   k0a.v   = k0.v*a.v
                   cphi1.v = cos(phi1.v)
                   ba.v    = b.v/a.v
                   pia.v   = C314159265358979323846264338328.v* &
                             a.v
                   a2.v    = a.v*a.v
                   epsm1   = eps- v4r4_1
                   sphi1.v = sin(phi1.v)
                   b2a2.v  = b2.v/a2.v
                   k0a2.v  = k0a.v*k0a.v
                   cphi2.v = cos(phi2.v)
                   cphit.v = cphi2.v*cphi1.v
                   k0a3.v  = k0a2.v*k0a.v
                   mum1    = mu- v4r4_1
                   ba1.v   = ba.v+ v4r4_1.v
                   tc0     = epsm1-mum1
                   sphi2.v = sin(phi2.v)
                   sphit.v = sphi2.v*sphi1.v
                   fac.v   = (pia.v*C078539816339744830961566084582.v)* &
                             (k0a3.v*b2a2.v)
                   mupba   = mu+ba
                   tc1     = cphit/mupba
                   mumba   = mu*ba
                   tc2     = sphit/mumba
                   tc3     = ba1*(tc1+tc2)
                   tmp     = tc0*tc3
                   cab     =  cabs_xmm4c4(tmp)
                   rcs     = fac.v*cab.v
           end subroutine rcs_f4428_xmm4r4
           
           
           !      /*
           !               Infinitely long homogenous cylinder at normal
           !               incidence.
           !               Low frequency approximation (k0a<0.5,k0b<0.5,k1a<0.5,k1b<0.5)
           !               Bistatic scattering width (RCS).
           !               TE-case.
           !               Formula 4.4-29
           !!
           !         */
           
           subroutine rcs_f4429_xmm4r4(k0,a,b,phi1,phi2,eps,mu,rcs) 
               
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f4429_xmm4r4
                   !dir$ attributes forceinline :: rcs_f4429_xmm4r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f4429_xmm4r4
                   use mod_vecconsts, only :  v4r4_1, v4r4_0
                   type(XMM4r4_t),   intent(in) :: k0
                   type(XMM4r4_t),   intent(in) :: a
                   type(XMM4r4_t),   intent(in) :: b
                   type(XMM4r4_t),   intent(in) :: phi1
                   type(XMM4r4_t),   intent(in) :: phi2
                   type(  XMM4c4_t),     intent(in) :: eps
                   type(  XMM4c4_t),     intent(in) :: mu
                   type(XMM4r4_t),   intent(out):: rcs
                      !dir$ attributes align : 16 :: C078539816339744830961566084582 
                   type(XMM4r4_t),   parameter :: C078539816339744830961566084582 = &
                                                        XMM4r4_t(0.78539816339744830961566084582_sp)
                   type(XMM4r4_t),   parameter :: C314159265358979323846264338328 = &
                                                        XMM4r4_t(3.14159265358979323846264338328_sp)
                     !dir$ attributes align : 16 :: epsm1
                     !dir$ attributes align : 16 :: mum1
                     !dir$ attributes align : 16 :: epspba
                     !dir$ attributes align : 16 :: epsmba
                     !dir$ attributes align : 16 :: tc0
                     !dir$ attributes align : 16 :: tc1
                     !dir$ attributes align : 16 :: tc2
                     !dir$ attributes align : 16 :: tc3
                     !dir$ attributes align : 16 :: tmp
                     !dir$ attributes align : 16 :: k0a
                     !dir$ attributes align : 16 :: k0a2
                     !dir$ attributes align : 16 :: k0a3
                     !dir$ attributes align : 16 :: cphi2
                     !dir$ attributes align : 16 :: cphi1
                     !dir$ attributes align : 16 :: sphi2
                     !dir$ attributes align : 16 :: sphi1
                     !dir$ attributes align : 16 :: b2
                     !dir$ attributes align : 16 :: a2
                     !dir$ attributes align : 16 :: pia
                     !dir$ attributes align : 16 :: b2a2
                     !dir$ attributes align : 16 :: fac
                     !dir$ attributes align : 16 :: ba1
                     !dir$ attributes align : 16 :: cphit
                     !dir$ attributes align : 16 :: sphit
                     !dir$ attributes align : 16 :: cab
                   type(  XMM4c4_t),     automatic :: epsm1,mum1
                   type(  XMM4c4_t),     automatic :: epspba,epsmba
                   type(  XMM4c4_t),     automatic :: tc0,tc1
                   type(  XMM4c4_t),     automatic :: tc2,tc3
                   type(  XMM4c4_t),     automatic :: tmp
                   type(XMM4r4_t),   automatic :: k0a,k0a2
                   type(  XMM4c4_t),     automatic :: k0a3,cphi2
                   type(  XMM4c4_t),     automatic :: cphi1,sphi2
                   type(  XMM4c4_t),     automatic :: sphi1,b2
                   type(  XMM4c4_t),     automatic :: a2,pia
                   type(  XMM4c4_t),     automatic :: b2a2,fac
                   type(  XMM4c4_t),     automatic :: ba1,cphit
                   type(  XMM4c4_t),     automatic :: sphit,cab
                   b2.v    = b.v*b.v
                   k0a.v   = k0.v*a.v
                   cphi1.v = cos(phi1.v)
                   ba.v    = b.v/a.v
                   pia.v   = C314159265358979323846264338328.v* &
                             a.v
                   a2.v    = a.v*a.v
                   epsm1   = eps- v4r4_1
                   sphi1.v = sin(phi1.v)
                   b2a2.v  = b2.v/a2.v
                   k0a2.v  = k0a.v*k0a.v
                   cphi2.v = cos(phi2.v)
                   cphit.v = cphi2.v*cphi1.v
                   k0a3.v  = k0a2.v*k0a.v
                   mum1    = mu- v4r4_1
                   ba1.v   = ba.v+ v4r4_1.v
                   tc0     = epsm1-mum1
                   sphi2.v = sin(phi2.v)
                   sphit.v = sphi2.v*sphi1.v
                   fac.v   = (pia.v*C078539816339744830961566084582.v)* &
                             (k0a3.v*b2a2.v)
                   epspba  = eps+ba
                   tc1     = cphit/epspba
                   epsmba  = eps*ba
                   tc2     = sphit/epsmba
                   tc3     = ba1*(tc1+tc2)
                   tmp     = tc0*tc3
                   cab     =  cabs_xmm4c4(tmp)
                   rcs     = fac.v*cab.v
           end subroutine rcs_f4429_xmm4r4
           


            
            

 
          
          

           
           
                                    



                
               
                 
                

                 

                
                
                

 




end module rcs_cylinder_xmm4r4
