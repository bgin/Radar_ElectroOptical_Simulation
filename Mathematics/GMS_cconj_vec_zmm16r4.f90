
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

module cconj_vec_zmm16r4


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name: cconj_vec_zmm16r4
 !                         
 !          
 !          Purpose:
 !                       Complex-conjugate vectorized deinterleaved, single-precision
 !                        
 !          History:
 !                        Date: 20-10-2024
 !                        Time: 10:11 GMT+2
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
 !                     Own project, C++ version originally
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
!==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.

    use mod_kinds,    only : i4,sp
    use mod_vectypes
    use omp_lib
    
    public
    implicit none
    
    
      ! Major version
     integer(kind=i4),  parameter :: CCONJ_VEC_ZMM16R4_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: CCONJ_VEC_ZMM16R4_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: CCONJ_VEC_ZMM16R4_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: CCONJ_VEC_ZMM16R4_FULLVER =   &
            1000*CCONJ_VEC_ZMM16R4_MAJOR+100*CCONJ_VEC_ZMM16R4_MINOR+10*CCONJ_VEC_ZMM16R4_MICRO
     ! Module creation date
     character(*),        parameter :: CCONJ_VEC_ZMM16R4_CREATE_DATE = "20-10-2024 10:13AM +00200 (SUN 20 OCT 2024 GMT+2)"
     ! Module build date
     character(*),        parameter :: CCONJ_VEC_ZMM16R4_BUILD_DATE  = __DATE__ " " __TIME__
     ! Module author info
     character(*),        parameter :: CCONJ_VEC_ZMM16R4_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: CCONJ_VEC_ZMM16R4_SYNOPSIS  = "Complex-conjugate vectorized deinterleaved, single-precision." 
     
     type(ZMM16r4_t),  parameter          :: CN1v16  = ZMM16r4_t(-1.0_sp)
     type(YMM8r4_t),   parameter          :: CN1v8   = YMM8r4_t(-1.0_sp)
     type(XMM4r4_t),   parameter          :: CN1v4   = XMM4r4_t(-1.0_sp)
     integer(kind=i4), parameter, private :: ZMM_LEN = 16
     
     contains
     
subroutine cconjv_v512_32x16_ps(xim,cxim,n)
#if defined(__ICC) || defined(__INTEL_COMPILER)    
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cconjv_v512_32x16_ps
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: cconjv_v512_32x16_ps
#endif     
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xim
         real(kind=sp), allocatable, dimension(:), intent(out) :: cxim
         integer(kind=i4),                         intent(in)  :: n
#if defined(__ICC) || defined(__INTEL_COMPILER)
      !DIR$ ASSUME_ALIGNED : 64 :: xim
      !DIR$ ASSUME_ALIGNED : 64 :: cxim
#endif
         
         type(ZMM16r4_t), automatic :: zmm0
         type(ZMM16r4_t), automatic :: zmm1
         type(ZMM16r4_t), automatic :: zmm2
         type(ZMM16r4_t), automatic :: zmm3
         type(ZMM16r4_t), automatic :: zmm4
         type(ZMM16r4_t), automatic :: zmm5
         type(ZMM16r4_t), automatic :: zmm6
         type(ZMM16r4_t), automatic :: zmm7
         type(ZMM16r4_t), automatic :: zmm8
         type(ZMM16r4_t), automatic :: zmm9
         type(ZMM16r4_t), automatic :: zmm10
         type(ZMM16r4_t), automatic :: zmm11
         type(ZMM16r4_t), automatic :: zmm12
         type(ZMM16r4_t), automatic :: zmm13
         type(ZMM16r4_t), automatic :: zmm14
         type(ZMM16r4_t), automatic :: zmm15
         type(ZMM16r4_t), automatic :: zmm16
         type(ZMM16r4_t), automatic :: zmm17
         type(ZMM16r4_t), automatic :: zmm18
         type(ZMM16r4_t), automatic :: zmm19
         type(ZMM16r4_t), automatic :: zmm20
         type(ZMM16r4_t), automatic :: zmm21
         type(ZMM16r4_t), automatic :: zmm22
         type(ZMM16r4_t), automatic :: zmm23
         type(ZMM16r4_t), automatic :: zmm24
         type(ZMM16r4_t), automatic :: zmm25
         type(ZMM16r4_t), automatic :: zmm26
         type(ZMM16r4_t), automatic :: zmm27
         type(ZMM16r4_t), automatic :: zmm28
         type(ZMM16r4_t), automatic :: zmm29
         type(ZMM16r4_t), automatic :: zmm30
         type(ZMM16r4_t), automatic :: zmm31
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm1
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm2
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm3
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm4
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm6
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm6
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm7
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm8
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm9
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm10
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm11
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm12
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm13
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm14
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm15 
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm16
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm17
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm18
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm19
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm20
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm21
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm22
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm23
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm24
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm25
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm26
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm27
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm28
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm29
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm30
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm31 
         type(XMM4r4_t),  automatic  :: xmm0
         type(YMM8r4_t),  automatic  :: ymm0 
         real(sp),         automatic :: z0
         integer(kind=i4), automatic :: i,ii,j
         if(n<=0) then
            return
         else if(n==1) then
            z0      = xim(0)
            cxim(0) = -1.0_sp*z0
            return
         else if(n>1 && n<=4) then
!$omp simd linear(i:1)
            do i=0, 3
               xmm0.v(i) = xim(i)
               cxim(i)   = CN1v4.v(i)*xmm0.v(i)
            end do
            return
         else if(n>4 && n<=8) then
!$omp simd linear(i:1)
            do i=0, 7
               ymm0.v(i) = xim(i)
               cxim(i)   = CN1v8.v(i)*ymm0.v(i)
            end do
            return
         else if(n>8 && n<=16) then
!$omp simd aligned(xim:64) aligned(cxim:64) linear(i:1)
            do i=0, 15
               zmm0.v(i) = xim(i)
               cxim(i)   = CN1v16.v(i)*zmm0.v(i)
            end do
            return
         else if(n>16 && n<=64) then
            do i = 0,iand(n-1,inot(15)),16
!$omp simd aligned(xim:64) aligned(cxim:64) linear(ii:1)
               do ii = 0, 15
                  zmm0.v(ii)  = xim(i+ii)
                  cxim(i+ii)  = CN1v16.v(ii)*zmm0.v(ii)
               end do
           end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
            do j = i,n-1
               dst(j) = src(j)
            end do
          return
       else if(n>64 && n<=128) then
          do i = 0,iand(n-1,inot(15)),16
!$omp simd aligned(xim:64) aligned(cxim:64) linear(ii:1)
               do ii = 0, 15
                  zmm0.v(ii)  = xim(i+ii)
                  cxim(i+ii)  = CN1v16.v(ii)*zmm0.v(ii)
               end do
           end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
            do j = i,n-1
               dst(j) = src(j)
            end do
          return
       else if(n>128) then
          do i=0, iand(n-1,inot(ZMM_LEN-1), ZMM_LEN*32
!$omp simd aligned(xim:64) aligned(cxim:64) linear(ii:1)              
              do ii = 0, ZMM_LEN-1
                  call mm_prefetch(xim(i+0+ii),FOR_K_PREFETCH_T1)
                  call mm_prefetch(xim(i+4*ZMM_LEN+ii),FOR_K_PREFETCH_T1)
                  call mm_prefetch(xim(i+8*ZMM_LEN+ii),FOR_K_PREFETCH_T1)
                  call mm_prefetch(xim(i+12*ZMM_LEN+ii),FOR_K_PREFETCH_T1)
                  call mm_prefetch(xim(i+16*ZMM_LEN+ii),FOR_K_PREFETCH_T1)
                  call mm_prefetch(xim(i+20*ZMM_LEN+ii),FOR_K_PREFETCH_T1)
                  call mm_prefetch(xim(i+24*ZMM_LEN+ii),FOR_K_PREFETCH_T1)
                  call mm_prefetch(xim(i+28*ZMM_LEN+ii),FOR_K_PREFETCH_T1)
                  zmm0.v(ii)            = xim(i+0+ii)
                  cxim(i+0+ii)          = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+1*ZMM_LEN+ii)
                  cxim(i+1*ZMM_LEN+ii)  = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+2*ZMM_LEN+ii)
                  cxim(i+2*ZMM_LEN+ii)  = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+3*ZMM_LEN+ii)
                  cxim(i+3*zmm_len+ii)  = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+4*ZMM_LEN+ii)
                  cxim(i+4*zmm_len+ii)  = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+5*ZMM_LEN+ii)
                  cxim(i+5*zmm_len+ii)  = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+6*ZMM_LEN+ii)
                  cxim(i+6*zmm_len+ii)  = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+7*ZMM_LEN+ii)
                  cxim(i+7*zmm_len+ii)  = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+8*ZMM_LEN+ii)
                  cxim(i+8*zmm_len+ii)  = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+9*ZMM_LEN+ii)
                  cxim(i+9*zmm_len+ii)  = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+10*ZMM_LEN+ii)
                  cxim(i+10*zmm_len+ii) = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+11*ZMM_LEN+ii)
                  cxim(i+11*zmm_len+ii) = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+12*ZMM_LEN+ii)
                  cxim(i+12*zmm_len+ii) = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+13*ZMM_LEN+ii)
                  cxim(i+13*zmm_len+ii) = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+14*ZMM_LEN+ii)
                  cxim(i+14*zmm_len+ii) = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+15*ZMM_LEN+ii)
                  cxim(i+15*zmm_len+ii) = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+15*ZMM_LEN+ii)
                  cxim(i+15*ZMM_LEN+ii) = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+16*ZMM_LEN+ii)
                  cxim(i+16*ZMM_LEN+ii) = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+17*ZMM_LEN+ii)
                  cxim(i+17*ZMM_LEN+ii) = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+18*ZMM_LEN+ii)
                  cxim(i+18*zmm_len+ii) = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+19*ZMM_LEN+ii)
                  cxim(i+19*zmm_len+ii) = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+20*ZMM_LEN+ii)
                  cxim(i+20*zmm_len+ii) = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+21*ZMM_LEN+ii)
                  cxim(i+21*zmm_len+ii)  = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+22*ZMM_LEN+ii)
                  cxim(i+22*zmm_len+ii)  = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+23*ZMM_LEN+ii)
                  cxim(i+23*zmm_len+ii)  = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+24*ZMM_LEN+ii)
                  cxim(i+24*zmm_len+ii)  = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+25*ZMM_LEN+ii)
                  cxim(i+25*zmm_len+ii) = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+26*ZMM_LEN+ii)
                  cxim(i+26*zmm_len+ii) = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+27*ZMM_LEN+ii)
                  cxim(i+27*zmm_len+ii) = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+28*ZMM_LEN+ii)
                  cxim(i+28*zmm_len+ii) = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+30*ZMM_LEN+ii)
                  cxim(i+30*zmm_len+ii) = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+31*ZMM_LEN+ii)
                  cxim(i+31*zmm_len+ii) = CN1v16.v(ii)*zmm0.v(ii)
              end do
          end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
          do j = i, n-1
            z0 =    xim(j)
            cxim(j) = -1.0_sp*z0
          end do
          return
       end if
end subroutine cconjv_v512_32x16_ps
          
     
subroutine cconjv_v512_16x16_ps(xim,cxim,n)
#if defined(__ICC) || defined(__INTEL_COMPILER)    
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cconjv_v512_16x16_ps
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: cconjv_v512_16x16_ps
#endif     
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xim
         real(kind=sp), allocatable, dimension(:), intent(out) :: cxim
         integer(kind=i4),                         intent(in)  :: n
#if defined(__ICC) || defined(__INTEL_COMPILER)
      !DIR$ ASSUME_ALIGNED : 64 :: xim
      !DIR$ ASSUME_ALIGNED : 64 :: cxim
#endif
         
         type(ZMM16r4_t), automatic :: zmm0
         type(ZMM16r4_t), automatic :: zmm1
         type(ZMM16r4_t), automatic :: zmm2
         type(ZMM16r4_t), automatic :: zmm3
         type(ZMM16r4_t), automatic :: zmm4
         type(ZMM16r4_t), automatic :: zmm5
         type(ZMM16r4_t), automatic :: zmm6
         type(ZMM16r4_t), automatic :: zmm7
         type(ZMM16r4_t), automatic :: zmm8
         type(ZMM16r4_t), automatic :: zmm9
         type(ZMM16r4_t), automatic :: zmm10
         type(ZMM16r4_t), automatic :: zmm11
         type(ZMM16r4_t), automatic :: zmm12
         type(ZMM16r4_t), automatic :: zmm13
         type(ZMM16r4_t), automatic :: zmm14
         type(ZMM16r4_t), automatic :: zmm15
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm1
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm2
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm3
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm4
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm6
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm6
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm7
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm8
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm9
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm10
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm11
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm12
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm13
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm14
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm15 
         type(XMM4r4_t),  automatic  :: xmm0
         type(YMM8r4_t),  automatic  :: ymm0 
         real(sp),         automatic :: z0
         integer(kind=i4), automatic :: i,ii,j
         if(n<=0) then
            return
         else if(n==1) then
            z0      = xim(0)
            cxim(0) = -1.0_sp*z0
            return
         else if(n>1 && n<=4) then
!$omp simd linear(i:1)
            do i=0, 3
               xmm0.v(i) = xim(i)
               cxim(i)   = CN1v4.v(i)*xmm0.v(i)
            end do
            return
         else if(n>4 && n<=8) then
!$omp simd linear(i:1)
            do i=0, 7
               ymm0.v(i) = xim(i)
               cxim(i)   = CN1v8.v(i)*ymm0.v(i)
            end do
            return
         else if(n>8 && n<=16) then
!$omp simd aligned(xim:64) aligned(cxim:64) linear(i:1)
            do i=0, 15
               zmm0.v(i) = xim(i)
               cxim(i)   = CN1v16.v(i)*zmm0.v(i)
            end do
            return
         else if(n>16 && n<=64) then
            do i = 0,iand(n-1,inot(15)),16
!$omp simd aligned(xim:64) aligned(cxim:64) linear(ii:1)
               do ii = 0, 15
                  zmm0.v(ii)  = xim(i+ii)
                  cxim(i+ii)  = CN1v16.v(ii)*zmm0.v(ii)
               end do
           end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
            do j = i,n-1
               dst(j) = src(j)
            end do
          return
       else if(n>64) then
          do i=0, iand(n-1,inot(ZMM_LEN-1), ZMM_LEN*16
!$omp simd aligned(xim:64) aligned(cxim:64) linear(ii:1)              
              do ii = 0, ZMM_LEN-1
                  call mm_prefetch(xim(i+0+ii),FOR_K_PREFETCH_T1)
                  call mm_prefetch(xim(i+4*ZMM_LEN+ii),FOR_K_PREFETCH_T1)
                  call mm_prefetch(xim(i+8*ZMM_LEN+ii),FOR_K_PREFETCH_T1)
                  call mm_prefetch(xim(i+12*ZMM_LEN+ii),FOR_K_PREFETCH_T1)
                  zmm0.v(ii)            = xim(i+0+ii)
                  cxim(i+0+ii)          = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+1*ZMM_LEN+ii)
                  cxim(i+1*ZMM_LEN+ii)  = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+2*ZMM_LEN+ii)
                  cxim(i+2*ZMM_LEN+ii)  = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+3*ZMM_LEN+ii)
                  cxim(i+3*zmm_len+ii)  = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+4*ZMM_LEN+ii)
                  cxim(i+4*zmm_len+ii)  = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+5*ZMM_LEN+ii)
                  cxim(i+5*zmm_len+ii)  = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+6*ZMM_LEN+ii)
                  cxim(i+6*zmm_len+ii)  = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+7*ZMM_LEN+ii)
                  cxim(i+7*zmm_len+ii)  = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+8*ZMM_LEN+ii)
                  cxim(i+8*zmm_len+ii)  = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+9*ZMM_LEN+ii)
                  cxim(i+9*zmm_len+ii)  = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+10*ZMM_LEN+ii)
                  cxim(i+10*zmm_len+ii) = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+11*ZMM_LEN+ii)
                  cxim(i+11*zmm_len+ii) = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+12*ZMM_LEN+ii)
                  cxim(i+12*zmm_len+ii) = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+13*ZMM_LEN+ii)
                  cxim(i+13*zmm_len+ii) = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+14*ZMM_LEN+ii)
                  cxim(i+14*zmm_len+ii) = CN1v16.v(ii)*zmm0.v(ii)
                  zmm0.v(ii)            = xim(i+15*ZMM_LEN+ii)
                  cxim(i+15*zmm_len+ii) = CN1v16.v(ii)*zmm0.v(ii)
              end do
          end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
          do j = i, n-1
            z0 =    xim(j)
            cxim(j) = -1.0_sp*z0
          end do
          return
       end if
end subroutine cconjv_v512_16x16_ps
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
end module cconj_vec_zmm16r4
     
