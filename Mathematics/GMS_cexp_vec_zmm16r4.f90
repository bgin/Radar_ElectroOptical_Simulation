
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

module cexp_vec_zmm16r4


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name: cexp_vec_zmm16r4
 !                         
 !          
 !          Purpose:
 !                       Complex-exponential vectorized deinterleaved, single-precision
 !                        
 !          History:
 !                        Date: 01-11-2024
 !                        Time: 06:50AM GMT+2
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

    use mod_kinds,     only : i4,sp
    use mod_vectypes,  only : XMM4r4_t,YMM8r4_t,ZMM16r4_t
    use omp_lib
    
    public
    implicit none
    
    
      ! Major version
     integer(kind=i4),  parameter :: CEXPV_ZMM16R4_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: CEXPV_ZMM16R4_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: CEXPV_ZMM16R4_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: CEXPV_ZMM16R4_FULLVER =   &
            1000*CEXPV_ZMM16R4_MAJOR+100*CEXPV_ZMM16R4_MINOR+10*CEXPV_ZMM16R4_MICRO
     ! Module creation date
     character(*),        parameter :: CEXPV_ZMM16R4_CREATE_DATE = "27-10-2024 06:59AM +00200 (SUN 27 OCT 2024 GMT+2)"
     ! Module build date
     character(*),        parameter :: CEXPV_ZMM16R4_BUILD_DATE  = __DATE__ " " __TIME__
     ! Module author info
     character(*),        parameter :: CEXPV_ZMM16R4_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: CEXPV_ZMM16R4_SYNOPSIS    = "Complex-exponential vectorized deinterleaved, single-precision." 
     
    
     integer(kind=i4), parameter, private :: ZMM_LEN = 16
     
     
     contains
     
     
subroutine cexpv_v512_32x16_ps(xre,xim,cexpr,cexpi,n)
#if defined(__ICC) || defined(__INTEL_COMPILER)    
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cexpv_v512_32x16_ps
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: cexpv_v512_32x16_ps
#endif     
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xre
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xim
         real(kind=sp), allocatable, dimension(:), intent(in)  :: cexpr
         real(kind=sp), allocatable, dimension(:), intent(in)  :: cexpi
         integer(kind=i4),                         intent(in)  :: n

         type(ZMM16r4_t), automatic :: zmm0
         type(ZMM16r4_t), automatic :: zmm1
         type(ZMM16r4_t), automatic :: zmm2
         type(ZMM16r4_t), automatic :: zmm3
         type(ZMM16r4_t), automatic :: zmm4
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm1
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm2
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm3
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm4
         type(XMM4r4_t),  automatic  :: xmm0
         type(XMM4r4_t),  automatic  :: xmm1
         type(XMM4r4_t),  automatic  :: xmm2
         type(XMM4r4_t),  automatic  :: xmm3
         type(XMM4r4_t),  automatic  :: xmm4
         type(YMM8r4_t),  automatic  :: ymm0 
         type(YMM8r4_t),  automatic  :: ymm1
         type(YMM8r4_t),  automatic  :: ymm2
         type(YMM8r4_t),  automatic  :: ymm3
         type(YMM8r4_t),  automatic  :: ymm4
         real(sp),        automatic  :: re
         real(sp),        automatic  :: im
         real(sp),        automatic  :: x
         real(sp),        automatic  :: y
         real(sp),        automatic  :: z
         integer(i4),     automatic  :: i,ii,j
         integer(i4),     automatic  :: idx1,idx2,idx3,idx4
         integer(i4),     automatic  :: idx5,idx6,idx7,idx8
         integer(i4),     automatic  :: idx9,idx10,idx11,idx12 
         integer(i4),     automatic  :: idx13,idx14,idx15,idx16
         integer(i4),     automatic  :: idx17,idx18,idx19,idx20
         integer(i4),     automatic  :: idx21,idx22,idx23,idx24
         integer(i4),     automatic  :: idx25,idx26,idx27,idx28
         integer(i4),     automatic  :: idx29,idx30,idx31,idx32
         
         if(n<=0) then
            return
         else if(n==1) then
                 re       = xre(0)
                 im       = xim(0)
                 x        = exp(re)
                 y        = x*sin(im)
                 cexpr(0) = y
                 z        = x*cos(im)
                 cexpi(0) = z
                 return
          else if(n>1 && n<=4) then
!$omp simd linear(i:1)
                 do i=0, 3
                    xmm0.v(i) = xre(i) ! re
                    xmm1.v(i) = xim(i) ! im
                    xmm2.v(i) = exp(xmm0.v(i)) ! x
                    xmm3.v(i) = xmm2.v(i)*sin(xmm1.v(i)) ! y
                    cexpr(i)  = xmm3.v(i)
                    xmm4.v(i) = xmm2.v(i)*cos(xmm1.v(i)) ! z
                    cexpi(i)  = xmm4.v(i)
                 end do
                 return
          else if(n>4 && n<=8) then
!$omp simd linear(i:1)
                 do i=0, 7
                    ymm0.v(i) = xre(i) ! re
                    ymm1.v(i) = xim(i) ! im
                    ymm2.v(i) = exp(ymm0.v(i)) ! x
                    ymm3.v(i) = ymm2.v(i)*sin(ymm1.v(i)) ! y
                    cexpr(i)  = ymm3.v(i)
                    ymm4.v(i) = ymm2.v(i)*cos(ymm1.v(i)) ! z
                    cexpi(i)  = ymm4.v(i)
                 end do
                 return
          else if(n>8 && n<=16) then
!$omp simd linear(i:1)
                 do i=0, 15
                    zmm0.v(i) = xre(i) ! re
                    zmm1.v(i) = xim(i) ! im
                    zmm2.v(i) = exp(zmm0.v(i)) ! x
                    zmm3.v(i) = zmm2.v(i)*sin(zmm1.v(i)) ! y
                    cexpr(i)  = zmm3.v(i)
                    zmm4.v(i) = zmm2.v(i)*cos(zmm1.v(i)) ! z
                    cexpi(i)  = zmm4.v(i)
                 end do
                 return
          else if(n>16 && n<=64) then
                 do i = 0,iand(n-1,inot(15)),16
!$omp simd aligned(xim:64,xre,cexpr,cexpi) linear(ii:1)
                     do ii = 0, 15  
                         zmm0.v(ii) = xre(i+ii) ! re
                         zmm1.v(ii) = xim(i+ii) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(i+ii)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(i+ii)= zmm4.v(ii)
                     end do
                 end do  
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                do j = i, n-1   
                    re       = xre(j)
                    im       = xim(j)
                    x        = exp(re)
                    y        = x*sin(im)
                    cexpr(j) = y
                    z        = x*cos(im)
                    cexpi(j) = z
                end do  
                return
          else if(n>64 && n<=128) then
                 do i = 0,iand(n-1,inot(15)),16
!$omp simd aligned(xim:64,xre,cexpr,cexpi) linear(ii:1)
                     do ii = 0, 15  
                         zmm0.v(ii) = xre(i+ii) ! re
                         zmm1.v(ii) = xim(i+ii) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(i+ii)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(i+ii)= zmm4.v(ii)
                     end do
                 end do  
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                do j = i, n-1   
                    re       = xre(j)
                    im       = xim(j)
                    x        = exp(re)
                    y        = x*sin(im)
                    cexpr(j) = y
                    z        = x*cos(im)
                    cexpi(j) = z
                end do  
                return
          else if(n>128) then
                do i=0, iand(n-1,inot(ZMM_LEN-1)), ZMM_LEN*32
                   call mm_prefetch(xre(i+32*ZMM_LEN),FOR_K_PREFETCH_T1)
                   call mm_prefetch(xim(i+32*ZMM_LEN),FOR_K_PREFETCH_T1)
#if defined(__ICC) || defined(__INTEL_COMPILER)
                   !dir$ assume_aligned  xre:64
                   !dir$ assume_aligned  xim:64
                   !dir$ assume_aligned  cexpr:64
                   !dir$ assume_aligned  cexpi:64
                   
#endif                   
!$omp simd aligned(xim:64,xre,yre,yim,zre,zim)  linear(ii:1)              
                  do ii = 0, ZMM_LEN-1 
                         zmm0.v(ii) = xre(i+0+ii) ! re
                         zmm1.v(ii) = xim(i+0+ii) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(i+0+ii)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(i+0+ii)= zmm4.v(ii)
                         idx1       = i+1*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx1) ! re
                         zmm1.v(ii) = xim(idx1) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx1)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx1)= zmm4.v(ii)
                         idx2       = i+2*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx2) ! re
                         zmm1.v(ii) = xim(idx2) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx2)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx2)= zmm4.v(ii)
                         idx3       = i+3*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx3) ! re
                         zmm1.v(ii) = xim(idx3) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx3)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx3)= zmm4.v(ii)
                         idx4       = i+4*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx4) ! re
                         zmm1.v(ii) = xim(idx4) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx4)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx4)= zmm4.v(ii)
                         idx5       = i+5*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx5) ! re
                         zmm1.v(ii) = xim(idx5) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx5)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx5)= zmm4.v(ii)
                         idx6       = i+6*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx6) ! re
                         zmm1.v(ii) = xim(idx6) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx6)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx6)= zmm4.v(ii)
                         idx7       = i+7*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx7) ! re
                         zmm1.v(ii) = xim(idx7) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx7)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx7)= zmm4.v(ii)
                         idx8       = i+8*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx8) ! re
                         zmm1.v(ii) = xim(idx8) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx8)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx8)= zmm4.v(ii)
                         idx9       = i+9*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx9) ! re
                         zmm1.v(ii) = xim(idx9) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx9)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx9)= zmm4.v(ii)
                         idx10      = i+10*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx10) ! re
                         zmm1.v(ii) = xim(idx10) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx10)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx10)= zmm4.v(ii)
                         idx11       = i+11*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx11) ! re
                         zmm1.v(ii) = xim(idx11) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx11)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx11)= zmm4.v(ii)
                         idx12      = i+12*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx12) ! re
                         zmm1.v(ii) = xim(idx12) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx12)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx12)= zmm4.v(ii)
                         idx13       = i+13*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx13) ! re
                         zmm1.v(ii) = xim(idx13) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx13)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx13)= zmm4.v(ii)
                         idx14      = i+14*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx14) ! re
                         zmm1.v(ii) = xim(idx14) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx14)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx14)= zmm4.v(ii)
                         idx15       = i+15*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx15) ! re
                         zmm1.v(ii) = xim(idx15) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx15)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx15)= zmm4.v(ii)
                         idx16       = i+16*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx16) ! re
                         zmm1.v(ii) = xim(idx16) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx16)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx16)= zmm4.v(ii)
                         idx17       = i+17*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx17) ! re
                         zmm1.v(ii) = xim(idx17) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx17)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx17)= zmm4.v(ii)
                         idx18       = i+18*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx18) ! re
                         zmm1.v(ii) = xim(idx18) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx18)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx18)= zmm4.v(ii)
                         idx19       = i+19*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx19) ! re
                         zmm1.v(ii) = xim(idx19) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx19)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx19)= zmm4.v(ii)
                         idx20       = i+20*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx20) ! re
                         zmm1.v(ii) = xim(idx20) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx20)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx20)= zmm4.v(ii)
                         idx21       = i+21*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx21) ! re
                         zmm1.v(ii) = xim(idx21) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx21)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx21)= zmm4.v(ii)
                         idx22       = i+22*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx22) ! re
                         zmm1.v(ii) = xim(idx22) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx22)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx22)= zmm4.v(ii)
                         idx23       = i+23*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx23) ! re
                         zmm1.v(ii) = xim(idx23) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx23)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx23)= zmm4.v(ii)
                         idx24       = i+24*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx24) ! re
                         zmm1.v(ii) = xim(idx24) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx24)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx24)= zmm4.v(ii)
                         idx25       = i+25*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx25) ! re
                         zmm1.v(ii) = xim(idx25) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx25)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx25)= zmm4.v(ii)
                         idx26       = i+26*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx26) ! re
                         zmm1.v(ii) = xim(idx26) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx26)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx26)= zmm4.v(ii)
                         idx27       = i+27*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx27) ! re
                         zmm1.v(ii) = xim(idx27) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx27)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx27)= zmm4.v(ii)
                         idx28       = i+28*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx28) ! re
                         zmm1.v(ii) = xim(idx28) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx28)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx28)= zmm4.v(ii)
                         idx29       = i+29*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx29) ! re
                         zmm1.v(ii) = xim(idx29) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx29)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx29)= zmm4.v(ii)
                         idx30       = i+30*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx30) ! re
                         zmm1.v(ii) = xim(idx30) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx30)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx30)= zmm4.v(ii)
                         idx31       = i+31*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx31) ! re
                         zmm1.v(ii) = xim(idx31) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx31)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx31)= zmm4.v(ii)
                  end do
               end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                do j = i, n-1   
                    re       = xre(j)
                    im       = xim(j)
                    x        = exp(re)
                    y        = x*sin(im)
                    cexpr(j) = y
                    z        = x*cos(im)
                    cexpi(j) = z
                end do  
                return     
           end if          
end subroutine cexpv_v512_32x16_ps


subroutine cexpv_v512_16x16_ps(xre,xim,cexpr,cexpi,n)
#if defined(__ICC) || defined(__INTEL_COMPILER)    
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cexpv_v512_16x16_ps
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: cexpv_v512_16x16_ps
#endif     
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xre
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xim
         real(kind=sp), allocatable, dimension(:), intent(in)  :: cexpr
         real(kind=sp), allocatable, dimension(:), intent(in)  :: cexpi
         integer(kind=i4),                         intent(in)  :: n

         type(ZMM16r4_t), automatic :: zmm0
         type(ZMM16r4_t), automatic :: zmm1
         type(ZMM16r4_t), automatic :: zmm2
         type(ZMM16r4_t), automatic :: zmm3
         type(ZMM16r4_t), automatic :: zmm4
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm1
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm2
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm3
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm4
         type(XMM4r4_t),  automatic  :: xmm0
         type(XMM4r4_t),  automatic  :: xmm1
         type(XMM4r4_t),  automatic  :: xmm2
         type(XMM4r4_t),  automatic  :: xmm3
         type(XMM4r4_t),  automatic  :: xmm4
         type(YMM8r4_t),  automatic  :: ymm0 
         type(YMM8r4_t),  automatic  :: ymm1
         type(YMM8r4_t),  automatic  :: ymm2
         type(YMM8r4_t),  automatic  :: ymm3
         type(YMM8r4_t),  automatic  :: ymm4
         real(sp),        automatic  :: re
         real(sp),        automatic  :: im
         real(sp),        automatic  :: x
         real(sp),        automatic  :: y
         real(sp),        automatic  :: z
         integer(i4),     automatic  :: i,ii,j
         integer(i4),     automatic  :: idx1,idx2,idx3,idx4
         integer(i4),     automatic  :: idx5,idx6,idx7,idx8
         integer(i4),     automatic  :: idx9,idx10,idx11,idx12 
         integer(i4),     automatic  :: idx13,idx14,idx15
         
         if(n<=0) then
            return
         else if(n==1) then
                 re       = xre(0)
                 im       = xim(0)
                 x        = exp(re)
                 y        = x*sin(im)
                 cexpr(0) = y
                 z        = x*cos(im)
                 cexpi(0) = z
                 return
          else if(n>1 && n<=4) then
!$omp simd linear(i:1)
                 do i=0, 3
                    xmm0.v(i) = xre(i) ! re
                    xmm1.v(i) = xim(i) ! im
                    xmm2.v(i) = exp(xmm0.v(i)) ! x
                    xmm3.v(i) = xmm2.v(i)*sin(xmm1.v(i)) ! y
                    cexpr(i)  = xmm3.v(i)
                    xmm4.v(i) = xmm2.v(i)*cos(xmm1.v(i)) ! z
                    cexpi(i)  = xmm4.v(i)
                 end do
                 return
          else if(n>4 && n<=8) then
!$omp simd linear(i:1)
                 do i=0, 7
                    ymm0.v(i) = xre(i) ! re
                    ymm1.v(i) = xim(i) ! im
                    ymm2.v(i) = exp(ymm0.v(i)) ! x
                    ymm3.v(i) = ymm2.v(i)*sin(ymm1.v(i)) ! y
                    cexpr(i)  = ymm3.v(i)
                    ymm4.v(i) = ymm2.v(i)*cos(ymm1.v(i)) ! z
                    cexpi(i)  = ymm4.v(i)
                 end do
                 return
          else if(n>8 && n<=16) then
!$omp simd linear(i:1)
                 do i=0, 15
                    zmm0.v(i) = xre(i) ! re
                    zmm1.v(i) = xim(i) ! im
                    zmm2.v(i) = exp(zmm0.v(i)) ! x
                    zmm3.v(i) = zmm2.v(i)*sin(zmm1.v(i)) ! y
                    cexpr(i)  = zmm3.v(i)
                    zmm4.v(i) = zmm2.v(i)*cos(zmm1.v(i)) ! z
                    cexpi(i)  = zmm4.v(i)
                 end do
                 return
          else if(n>16 && n<=64) then
                 do i = 0,iand(n-1,inot(15)),16
!$omp simd aligned(xim:64,xre,cexpr,cexpi) linear(ii:1)
                     do ii = 0, 15  
                         zmm0.v(ii) = xre(i+ii) ! re
                         zmm1.v(ii) = xim(i+ii) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(i+ii)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(i+ii)= zmm4.v(ii)
                     end do
                 end do  
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                do j = i, n-1   
                    re       = xre(j)
                    im       = xim(j)
                    x        = exp(re)
                    y        = x*sin(im)
                    cexpr(j) = y
                    z        = x*cos(im)
                    cexpi(j) = z
                end do  
                return
          else if(n>64) then
                do i=0, iand(n-1,inot(ZMM_LEN-1)), ZMM_LEN*16
                   call mm_prefetch(xre(i+16*ZMM_LEN),FOR_K_PREFETCH_T1)
                   call mm_prefetch(xim(i+16*ZMM_LEN),FOR_K_PREFETCH_T1)
#if defined(__ICC) || defined(__INTEL_COMPILER)
                   !dir$ assume_aligned  xre:64
                   !dir$ assume_aligned  xim:64
                   !dir$ assume_aligned  cexpr:64
                   !dir$ assume_aligned  cexpi:64
                   
#endif                   
!$omp simd aligned(xim:64,xre,yre,yim,zre,zim)  linear(ii:1)              
                  do ii = 0, ZMM_LEN-1 
                         zmm0.v(ii) = xre(i+0+ii) ! re
                         zmm1.v(ii) = xim(i+0+ii) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(i+0+ii)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(i+0+ii)= zmm4.v(ii)
                         idx1       = i+1*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx1) ! re
                         zmm1.v(ii) = xim(idx1) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx1)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx1)= zmm4.v(ii)
                         idx2       = i+2*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx2) ! re
                         zmm1.v(ii) = xim(idx2) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx2)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx2)= zmm4.v(ii)
                         idx3       = i+3*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx3) ! re
                         zmm1.v(ii) = xim(idx3) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx3)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx3)= zmm4.v(ii)
                         idx4       = i+4*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx4) ! re
                         zmm1.v(ii) = xim(idx4) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx4)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx4)= zmm4.v(ii)
                         idx5       = i+5*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx5) ! re
                         zmm1.v(ii) = xim(idx5) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx5)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx5)= zmm4.v(ii)
                         idx6       = i+6*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx6) ! re
                         zmm1.v(ii) = xim(idx6) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx6)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx6)= zmm4.v(ii)
                         idx7       = i+7*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx7) ! re
                         zmm1.v(ii) = xim(idx7) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx7)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx7)= zmm4.v(ii)
                         idx8       = i+8*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx8) ! re
                         zmm1.v(ii) = xim(idx8) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx8)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx8)= zmm4.v(ii)
                         idx9       = i+9*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx9) ! re
                         zmm1.v(ii) = xim(idx9) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx9)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx9)= zmm4.v(ii)
                         idx10      = i+10*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx10) ! re
                         zmm1.v(ii) = xim(idx10) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx10)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx10)= zmm4.v(ii)
                         idx11       = i+11*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx11) ! re
                         zmm1.v(ii) = xim(idx11) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx11)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx11)= zmm4.v(ii)
                         idx12      = i+12*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx12) ! re
                         zmm1.v(ii) = xim(idx12) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx12)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx12)= zmm4.v(ii)
                         idx13       = i+13*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx13) ! re
                         zmm1.v(ii) = xim(idx13) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx13)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx13)= zmm4.v(ii)
                         idx14      = i+14*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx14) ! re
                         zmm1.v(ii) = xim(idx14) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx14)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx14)= zmm4.v(ii)
                         idx15       = i+15*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx15) ! re
                         zmm1.v(ii) = xim(idx15) ! im
                         zmm2.v(ii) = exp(zmm0.v(ii)) ! x
                         zmm3.v(ii) = zmm2.v(ii)*sin(zmm1.v(ii)) ! y
                         cexpr(idx15)= zmm3.v(ii)
                         zmm4.v(ii) = zmm2.v(ii)*cos(zmm1.v(ii)) ! z
                         cexpi(idx15)= zmm4.v(ii)
                   end do
               end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                do j = i, n-1   
                    re       = xre(j)
                    im       = xim(j)
                    x        = exp(re)
                    y        = x*sin(im)
                    cexpr(j) = y
                    z        = x*cos(im)
                    cexpi(j) = z
                end do  
                return     
           end if          
end subroutine cexpv_v512_16x16_ps
















































    
     
     
     
     
     
     
     
     
     
     
     
end module cexp_vec_zmm16r4
