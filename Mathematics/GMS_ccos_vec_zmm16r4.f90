
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

module ccosv_zmm16r4


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name: ccosv_zmm16r4
 !                         
 !          
 !          Purpose:
 !                       Complex-cosine function vectorized deinterleaved, single-precision
 !                        
 !          History:
 !                        Date: 17-11-2024
 !                        Time: 03:27PM GMT+2
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
     integer(kind=i4),  parameter :: CCOSV_ZMM16R4_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: CCOSV_ZMM16R4_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: CCOSV_ZMM16R4_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: CCOSV_ZMM16R4_FULLVER =   &
            1000*CCOSV_ZMM16R4_MAJOR+100*CCOSV_ZMM16R4_MINOR+10*CCOSV_ZMM16R4_MICRO
     ! Module creation date
     character(*),        parameter :: CCOSV_ZMM16R4_CREATE_DATE = "17-11-2024 03:27PM +00200 (SUN 17 NOV 2024 GMT+2)"
     ! Module build date
     character(*),        parameter :: CCOSV_ZMM16R4_BUILD_DATE  = __DATE__ " " __TIME__
     ! Module author info
     character(*),        parameter :: CCOSV_ZMM16R4_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: CCOSV_ZMM16R4_SYNOPSIS  = "Complex-cosine function vectorized deinterleaved, single-precision." 
     
    
     integer(kind=i4), parameter, private :: ZMM_LEN = 16
     

     
     contains


subroutine ccosv_kernel_v512_cv_32x16_ps(xre,xim,csre,csim,n)
#if defined(__ICC) || defined(__INTEL_COMPILER)    
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ccosv_kernel_v512_cv_32x16_ps
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: ccosv_kernel_v512_cv_32x16_ps
#endif     
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xre
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xim
         real(kind=sp), allocatable, dimension(:), intent(in)  :: csre
         real(kind=sp), allocatable, dimension(:), intent(in)  :: csim
         integer(i4),                              intent(in)  :: n
         
         type(ZMM16r4_t), automatic :: zmm0
         type(ZMM16r4_t), automatic :: zmm1
         type(ZMM16r4_t), automatic :: zmm2
         type(ZMM16r4_t), automatic :: zmm3     
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm1
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm2
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm3
         type(XMM4r4_t),  automatic  :: xmm0
         type(XMM4r4_t),  automatic  :: xmm1
         type(XMM4r4_t),  automatic  :: xmm2
         type(XMM4r4_t),  automatic  :: xmm3
         type(YMM8r4_t),  automatic  :: ymm0 
         type(YMM8r4_t),  automatic  :: ymm1
         type(YMM8r4_t),  automatic  :: ymm2
         type(YMM8r4_t),  automatic  :: ymm3
         real(sp),        automatic  :: xr
         real(sp),        automatic  :: xi
         real(sp),        automatic  :: csr
         real(sp),        automatic  :: csi
         integer(i4),     automatic  :: i,ii,j
         integer(i4),     automatic  :: idx1,idx2,idx3,idx4
         integer(i4),     automatic  :: idx5,idx6,idx7,idx8
         integer(i4),     automatic  :: idx9,idx10,idx11,idx12 
         integer(i4),     automatic  :: idx13,idx14,idx15,idx16
         integer(i4),     automatic  :: idx17,idx18,idx19,idx20
         integer(i4),     automatic  :: idx21,idx22,idx23,idx24
         integer(i4),     automatic  :: idx25,idx26,idx27,idx28
         integer(i4),     automatic  :: idx29,idx30,idx31 

         if(n<=0) then 
            return
         else if(n==1) then
              xr      = xre(0) ! z0
              xi      = xim(0) ! z1
              csr     = cos(xr)*cosh(xi)
              csre(0) = csr
              csi     = sin(xr)*sinh(xi)
              csim(0) = csi
              return
         else if(n>1 && n<=4) then
                 !$omp simd linear(i:1)
                 do i=0, 3
                    xmm0.v(i) = xre(i) ! xr
                    xmm1.v(i) = xim(i) ! xi
                    xmm2.v(i) = cos(xmm0.v(i))*cosh(xmm1.v(i))
                    csre(i)   = xmm2.v(i)
                    xmm3.v(i) = sin(xmm0.v(i))*sinh(xmm1.v(i))
                    csim(i)   = xmm3.v(i)
                 end do
                 return
         else if(n>4 && n<=8) then
                 !$omp simd linear(i:1)
                 do i=0, 7
                    ymm0.v(i) = xre(i) ! xr
                    ymm1.v(i) = xim(i) ! xi
                    ymm2.v(i) = cos(ymm0.v(i))*cosh(ymm1.v(i))
                    csre(i)   = ymm2.v(i)
                    ymm3.v(i) = sin(ymm0.v(i))*sinh(ymm1.v(i))
                    csim(i)   = ymm3.v(i)
                 end do
                 return
         else if(n>8 && n<=16) then
                 !$omp simd linear(i:1)
                 do i=0, 15
                    zmm0.v(i) = xre(i) ! xr
                    zmm1.v(i) = xim(i) ! xi
                    zmm2.v(i) = cos(zmm0.v(i))*cosh(zmm1.v(i))
                    csre(i)   = zmm2.v(i)
                    zmm3.v(i) = sin(zmm0.v(i))*sinh(zmm1.v(i))
                    csim(i)   = zmm3.v(i)
                 end do
                 return
          else if(n>16 && n<=64) then
                   do i = 0,iand(n-1,inot(15)),16
                       !$omp simd aligned(xim:64,xre,yre,yim,zre,zim) linear(ii:1)
                       do ii = 0, 15 
                           zmm0.v(ii) = xre(i+ii) ! xr
                           zmm1.v(ii) = xim(i+ii) ! xi
                           zmm2.v(ii) = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                           csre(i+ii)   = zmm2.v(ii)
                           zmm3.v(ii) = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                           csim(i+ii)   = zmm3.v(ii)
                       end do
                   end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif                     
                   do j = i, n=1
                       xr      = xre(j) ! z0
                       xi      = xim(j) ! z1
                       csr     = cos(xr)*cosh(xi)
                       csre(j) = csr
                       csi     = sin(xr)*sinh(xi)
                       csim(j) = csi
                   end do
                   return
           else if(n>64 && n<=128) then
                   do i = 0,iand(n-1,inot(15)),16
                       !$omp simd aligned(xim:64,xre,yre,yim,zre,zim) linear(ii:1)
                       do ii = 0, 15 
                           zmm0.v(ii) = xre(i+ii) ! xr
                           zmm1.v(ii) = xim(i+ii) ! xi
                           zmm2.v(ii) = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                           csre(i+ii)   = zmm2.v(ii)
                           zmm3.v(ii) = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                           csim(i+ii)   = zmm3.v(ii)
                       end do
                   end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif                     
                   do j = i, n=1
                       xr      = xre(j) ! z0
                       xi      = xim(j) ! z1
                       csr     = cos(xr)*cosh(xi)
                       csre(j) = csr
                       csi     = sin(xr)*sinh(xi)
                       csim(j) = csi
                   end do                   
                   return   
           else if(n>128) then
                  do i=0, iand(n-1,inot(ZMM_LEN-1)), ZMM_LEN*32
                        call mm_prefetch(xre(i+32*ZMM_LEN),FOR_K_PREFETCH_T1)
                        call mm_prefetch(xim(i+32*ZMM_LEN),FOR_K_PREFETCH_T1)
#if defined(__ICC) || defined(__INTEL_COMPILER)
                        !dir$ assume_aligned  xre:64
                        !dir$ assume_aligned  xim:64
                        !dir$ assume_aligned  csre:64
                        !dir$ assume_aligned  csim:64
                  
#endif                   
                        !$omp simd aligned(xim:64,xre,csre,csim)  linear(ii:1)              
                         do ii = 0, ZMM_LEN-1  
                             zmm0.v(ii)     = xre(i+0+ii) ! xr
                             zmm1.v(ii)     = xim(i+0+ii) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(i+0+ii)   = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(i+0+ii)   = zmm3.v(ii)
                             idx1           = i+1*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx1) ! xr
                             zmm1.v(ii)     = xim(idx1) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx1)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx1)     = zmm3.v(ii)
                             idx2           = i+2*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx2) ! xr
                             zmm1.v(ii)     = xim(idx2) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx2)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx2)     = zmm3.v(ii)
                             idx3           = i+3*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx3) ! xr
                             zmm1.v(ii)     = xim(idx3) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx3)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx3)     = zmm3.v(ii)
                             idx4           = i+4*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx4) ! xr
                             zmm1.v(ii)     = xim(idx4) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx4)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx4)     = zmm3.v(ii)
                             idx5           = i+5*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx5) ! xr
                             zmm1.v(ii)     = xim(idx5) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx5)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx5)     = zmm3.v(ii)
                             idx6           = i+6*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx6) ! xr
                             zmm1.v(ii)     = xim(idx6) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx6)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx6)     = zmm3.v(ii)
                             idx7           = i+7*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx7) ! xr
                             zmm1.v(ii)     = xim(idx7) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx7)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx7)     = zmm3.v(ii)
                             idx8           = i+8*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx8) ! xr
                             zmm1.v(ii)     = xim(idx8) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx8)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx8)     = zmm3.v(ii)
                             idx9           = i+9*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx10) ! xr
                             zmm1.v(ii)     = xim(idx10) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx10)    = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx10)    = zmm3.v(ii)
                             idx11          = i+11*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx11) ! xr
                             zmm1.v(ii)     = xim(idx11) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx11)    = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx11)    = zmm3.v(ii)
                             idx12          = i+12*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx12) ! xr
                             zmm1.v(ii)     = xim(idx12) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx12)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx12)     = zmm3.v(ii)
                             idx13           = i+13*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx13) ! xr
                             zmm1.v(ii)     = xim(idx13) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx13)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx13)     = zmm3.v(ii)
                             idx14           = i+14*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx14) ! xr
                             zmm1.v(ii)     = xim(idx14) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx14)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx14)     = zmm3.v(ii)
                             idx15           = i+15*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx15) ! xr
                             zmm1.v(ii)     = xim(idx15) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx15)    = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx15)    = zmm3.v(ii)
                             idx16          = i+16*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx16) ! xr
                             zmm1.v(ii)     = xim(idx16) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx16)    = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx16)    = zmm3.v(ii)
                             idx17          = i+17*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx18) ! xr
                             zmm1.v(ii)     = xim(idx18) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx18)    = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx18)    = zmm3.v(ii)
                             idx19          = i+19*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx19) ! xr
                             zmm1.v(ii)     = xim(idx19) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx19)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx19)     = zmm3.v(ii)
                             idx20           = i+20*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx20) ! xr
                             zmm1.v(ii)     = xim(idx20) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx20)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx20)     = zmm3.v(ii)
                             idx21           = i+21*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx21) ! xr
                             zmm1.v(ii)     = xim(idx21) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx21)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx21)     = zmm3.v(ii)
                             idx22           = i+22*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx22) ! xr
                             zmm1.v(ii)     = xim(idx22) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx22)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx22)     = zmm3.v(ii)
                             idx23           = i+23*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx23) ! xr
                             zmm1.v(ii)     = xim(idx23) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx23)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx23)     = zmm3.v(ii)
                             idx24           = i+24*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx24) ! xr
                             zmm1.v(ii)     = xim(idx24) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx24)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx24)     = zmm3.v(ii)
                             idx25          = i+25*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx25) ! xr
                             zmm1.v(ii)     = xim(idx25) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx25)    = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx25)    = zmm3.v(ii)
                             idx26          = i+26*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx27) ! xr
                             zmm1.v(ii)     = xim(idx27) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx26)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx26)    = zmm3.v(ii)
                             idx27          = i+27*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx27) ! xr
                             zmm1.v(ii)     = xim(idx27) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx27)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx27)     = zmm3.v(ii)
                             idx28          = i+28*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx28) ! xr
                             zmm1.v(ii)     = xim(idx28) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx28)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx28)     = zmm3.v(ii)
                             idx29           = i+29*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx29) ! xr
                             zmm1.v(ii)     = xim(idx29) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx29)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx29)     = zmm3.v(ii)
                             idx30           = i+30*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx30) ! xr
                             zmm1.v(ii)     = xim(idx30) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx30)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx30)     = zmm3.v(ii)
                             idx31           = i+31*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx31) ! xr
                             zmm1.v(ii)     = xim(idx31) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx31)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx31)     = zmm3.v(ii)
                      end do
                  end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif                     
                   do j = i, n=1
                       xr      = xre(j) ! z0
                       xi      = xim(j) ! z1
                       csr     = cos(xr)*cosh(xi)
                       csre(j) = csr
                       csi     = sin(xr)*sinh(xi)
                       csim(j) = csi
                   end do                   
                   return   
           end if      
end subroutine ccosv_kernel_v512_cv_32x16_ps


subroutine ccosv_kernel_v512_cv_16x16_ps(xre,xim,csre,csim,n)
#if defined(__ICC) || defined(__INTEL_COMPILER)    
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ccosv_kernel_v512_cv_16x16_ps
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: ccosv_kernel_v512_cv_16x16_ps
#endif     
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xre
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xim
         real(kind=sp), allocatable, dimension(:), intent(in)  :: csre
         real(kind=sp), allocatable, dimension(:), intent(in)  :: csim
         integer(i4),                              intent(in)  :: n
         
         type(ZMM16r4_t), automatic :: zmm0
         type(ZMM16r4_t), automatic :: zmm1
         type(ZMM16r4_t), automatic :: zmm2
         type(ZMM16r4_t), automatic :: zmm3     
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm1
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm2
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm3
         type(XMM4r4_t),  automatic  :: xmm0
         type(XMM4r4_t),  automatic  :: xmm1
         type(XMM4r4_t),  automatic  :: xmm2
         type(XMM4r4_t),  automatic  :: xmm3
         type(YMM8r4_t),  automatic  :: ymm0 
         type(YMM8r4_t),  automatic  :: ymm1
         type(YMM8r4_t),  automatic  :: ymm2
         type(YMM8r4_t),  automatic  :: ymm3
         real(sp),        automatic  :: xr
         real(sp),        automatic  :: xi
         real(sp),        automatic  :: csr
         real(sp),        automatic  :: csi
         integer(i4),     automatic  :: i,ii,j
         integer(i4),     automatic  :: idx1,idx2,idx3,idx4
         integer(i4),     automatic  :: idx5,idx6,idx7,idx8
         integer(i4),     automatic  :: idx9,idx10,idx11,idx12 
         integer(i4),     automatic  :: idx13,idx14,idx15

         if(n<=0) then 
            return
         else if(n==1) then
              xr      = xre(0) ! z0
              xi      = xim(0) ! z1
              csr     = cos(xr)*cosh(xi)
              csre(0) = csr
              csi     = sin(xr)*sinh(xi)
              csim(0) = csi
              return
         else if(n>1 && n<=4) then
                 !$omp simd linear(i:1)
                 do i=0, 3
                    xmm0.v(i) = xre(i) ! xr
                    xmm1.v(i) = xim(i) ! xi
                    xmm2.v(i) = cos(xmm0.v(i))*cosh(xmm1.v(i))
                    csre(i)   = xmm2.v(i)
                    xmm3.v(i) = sin(xmm0.v(i))*sinh(xmm1.v(i))
                    csim(i)   = xmm3.v(i)
                 end do
                 return
         else if(n>4 && n<=8) then
                 !$omp simd linear(i:1)
                 do i=0, 7
                    ymm0.v(i) = xre(i) ! xr
                    ymm1.v(i) = xim(i) ! xi
                    ymm2.v(i) = cos(ymm0.v(i))*cosh(ymm1.v(i))
                    csre(i)   = ymm2.v(i)
                    ymm3.v(i) = sin(ymm0.v(i))*sinh(ymm1.v(i))
                    csim(i)   = ymm3.v(i)
                 end do
                 return
         else if(n>8 && n<=16) then
                 !$omp simd linear(i:1)
                 do i=0, 15
                    zmm0.v(i) = xre(i) ! xr
                    zmm1.v(i) = xim(i) ! xi
                    zmm2.v(i) = cos(zmm0.v(i))*cosh(zmm1.v(i))
                    csre(i)   = zmm2.v(i)
                    zmm3.v(i) = sin(zmm0.v(i))*sinh(zmm1.v(i))
                    csim(i)   = zmm3.v(i)
                 end do
                 return
          else if(n>16 && n<=64) then
                   do i = 0,iand(n-1,inot(15)),16
                       !$omp simd aligned(xim:64,xre,yre,yim,zre,zim) linear(ii:1)
                       do ii = 0, 15 
                           zmm0.v(ii) = xre(i+ii) ! xr
                           zmm1.v(ii) = xim(i+ii) ! xi
                           zmm2.v(ii) = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                           csre(i+ii)   = zmm2.v(ii)
                           zmm3.v(ii) = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                           csim(i+ii)   = zmm3.v(ii)
                       end do
                   end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif                     
                   do j = i, n=1
                       xr      = xre(j) ! z0
                       xi      = xim(j) ! z1
                       csr     = cos(xr)*cosh(xi)
                       csre(j) = csr
                       csi     = sin(xr)*sinh(xi)
                       csim(j) = csi
                   end do
                   return
           else if(n>64 && n<=128) then
                   do i = 0,iand(n-1,inot(15)),16
                       !$omp simd aligned(xim:64,xre,yre,yim,zre,zim) linear(ii:1)
                       do ii = 0, 15 
                           zmm0.v(ii) = xre(i+ii) ! xr
                           zmm1.v(ii) = xim(i+ii) ! xi
                           zmm2.v(ii) = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                           csre(i+ii)   = zmm2.v(ii)
                           zmm3.v(ii) = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                           csim(i+ii)   = zmm3.v(ii)
                       end do
                   end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif                     
                   do j = i, n=1
                       xr      = xre(j) ! z0
                       xi      = xim(j) ! z1
                       csr     = cos(xr)*cosh(xi)
                       csre(j) = csr
                       csi     = sin(xr)*sinh(xi)
                       csim(j) = csi
                   end do                   
                   return   
           else if(n>128) then
                  do i=0, iand(n-1,inot(ZMM_LEN-1)), ZMM_LEN*16
                        call mm_prefetch(xre(i+16*ZMM_LEN),FOR_K_PREFETCH_T1)
                        call mm_prefetch(xim(i+16*ZMM_LEN),FOR_K_PREFETCH_T1)
#if defined(__ICC) || defined(__INTEL_COMPILER)
                        !dir$ assume_aligned  xre:64
                        !dir$ assume_aligned  xim:64
                        !dir$ assume_aligned  csre:64
                        !dir$ assume_aligned  csim:64
                  
#endif                   
                        !$omp simd aligned(xim:64,xre,csre,csim)  linear(ii:1)              
                         do ii = 0, ZMM_LEN-1  
                             zmm0.v(ii)     = xre(i+0+ii) ! xr
                             zmm1.v(ii)     = xim(i+0+ii) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(i+0+ii)   = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(i+0+ii)   = zmm3.v(ii)
                             idx1           = i+1*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx1) ! xr
                             zmm1.v(ii)     = xim(idx1) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx1)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx1)     = zmm3.v(ii)
                             idx2           = i+2*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx2) ! xr
                             zmm1.v(ii)     = xim(idx2) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx2)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx2)     = zmm3.v(ii)
                             idx3           = i+3*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx3) ! xr
                             zmm1.v(ii)     = xim(idx3) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx3)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx3)     = zmm3.v(ii)
                             idx4           = i+4*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx4) ! xr
                             zmm1.v(ii)     = xim(idx4) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx4)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx4)     = zmm3.v(ii)
                             idx5           = i+5*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx5) ! xr
                             zmm1.v(ii)     = xim(idx5) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx5)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx5)     = zmm3.v(ii)
                             idx6           = i+6*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx6) ! xr
                             zmm1.v(ii)     = xim(idx6) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx6)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx6)     = zmm3.v(ii)
                             idx7           = i+7*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx7) ! xr
                             zmm1.v(ii)     = xim(idx7) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx7)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx7)     = zmm3.v(ii)
                             idx8           = i+8*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx8) ! xr
                             zmm1.v(ii)     = xim(idx8) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx8)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx8)     = zmm3.v(ii)
                             idx9           = i+9*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx10) ! xr
                             zmm1.v(ii)     = xim(idx10) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx10)    = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx10)    = zmm3.v(ii)
                             idx11          = i+11*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx11) ! xr
                             zmm1.v(ii)     = xim(idx11) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx11)    = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx11)    = zmm3.v(ii)
                             idx12          = i+12*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx12) ! xr
                             zmm1.v(ii)     = xim(idx12) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx12)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx12)     = zmm3.v(ii)
                             idx13           = i+13*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx13) ! xr
                             zmm1.v(ii)     = xim(idx13) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx13)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx13)     = zmm3.v(ii)
                             idx14           = i+14*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx14) ! xr
                             zmm1.v(ii)     = xim(idx14) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx14)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx14)     = zmm3.v(ii)
                             idx15           = i+15*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx15) ! xr
                             zmm1.v(ii)     = xim(idx15) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx15)    = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx15)    = zmm3.v(ii)
                      end do
                  end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif                     
                   do j = i, n=1
                       xr      = xre(j) ! z0
                       xi      = xim(j) ! z1
                       csr     = cos(xr)*cosh(xi)
                       csre(j) = csr
                       csi     = sin(xr)*sinh(xi)
                       csim(j) = csi
                   end do                   
                   return   
           end if      
end subroutine ccosv_kernel_v512_cv_16x16_ps


subroutine ccosv_kernel_v512_cv_8x16_ps(xre,xim,csre,csim,n)
#if defined(__ICC) || defined(__INTEL_COMPILER)    
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ccosv_kernel_v512_cv_8x16_ps
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: ccosv_kernel_v512_cv_8x16_ps
#endif     
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xre
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xim
         real(kind=sp), allocatable, dimension(:), intent(in)  :: csre
         real(kind=sp), allocatable, dimension(:), intent(in)  :: csim
         integer(i4),                              intent(in)  :: n
         
         type(ZMM16r4_t), automatic :: zmm0
         type(ZMM16r4_t), automatic :: zmm1
         type(ZMM16r4_t), automatic :: zmm2
         type(ZMM16r4_t), automatic :: zmm3     
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm1
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm2
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm3
         type(XMM4r4_t),  automatic  :: xmm0
         type(XMM4r4_t),  automatic  :: xmm1
         type(XMM4r4_t),  automatic  :: xmm2
         type(XMM4r4_t),  automatic  :: xmm3
         type(YMM8r4_t),  automatic  :: ymm0 
         type(YMM8r4_t),  automatic  :: ymm1
         type(YMM8r4_t),  automatic  :: ymm2
         type(YMM8r4_t),  automatic  :: ymm3
         real(sp),        automatic  :: xr
         real(sp),        automatic  :: xi
         real(sp),        automatic  :: csr
         real(sp),        automatic  :: csi
         integer(i4),     automatic  :: i,ii,j
         integer(i4),     automatic  :: idx1,idx2,idx3,idx4
         integer(i4),     automatic  :: idx5,idx6,idx7

         if(n<=0) then 
            return
         else if(n==1) then
              xr      = xre(0) ! z0
              xi      = xim(0) ! z1
              csr     = cos(xr)*cosh(xi)
              csre(0) = csr
              csi     = sin(xr)*sinh(xi)
              csim(0) = csi
              return
         else if(n>1 && n<=4) then
                 !$omp simd linear(i:1)
                 do i=0, 3
                    xmm0.v(i) = xre(i) ! xr
                    xmm1.v(i) = xim(i) ! xi
                    xmm2.v(i) = cos(xmm0.v(i))*cosh(xmm1.v(i))
                    csre(i)   = xmm2.v(i)
                    xmm3.v(i) = sin(xmm0.v(i))*sinh(xmm1.v(i))
                    csim(i)   = xmm3.v(i)
                 end do
                 return
         else if(n>4 && n<=8) then
                 !$omp simd linear(i:1)
                 do i=0, 7
                    ymm0.v(i) = xre(i) ! xr
                    ymm1.v(i) = xim(i) ! xi
                    ymm2.v(i) = cos(ymm0.v(i))*cosh(ymm1.v(i))
                    csre(i)   = ymm2.v(i)
                    ymm3.v(i) = sin(ymm0.v(i))*sinh(ymm1.v(i))
                    csim(i)   = ymm3.v(i)
                 end do
                 return
         else if(n>8 && n<=16) then
                 !$omp simd linear(i:1)
                 do i=0, 15
                    zmm0.v(i) = xre(i) ! xr
                    zmm1.v(i) = xim(i) ! xi
                    zmm2.v(i) = cos(zmm0.v(i))*cosh(zmm1.v(i))
                    csre(i)   = zmm2.v(i)
                    zmm3.v(i) = sin(zmm0.v(i))*sinh(zmm1.v(i))
                    csim(i)   = zmm3.v(i)
                 end do
                 return
          else if(n>16 && n<=32) then
                   do i = 0,iand(n-1,inot(15)),16
                       !$omp simd aligned(xim:64,xre,yre,yim,zre,zim) linear(ii:1)
                       do ii = 0, 15 
                           zmm0.v(ii) = xre(i+ii) ! xr
                           zmm1.v(ii) = xim(i+ii) ! xi
                           zmm2.v(ii) = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                           csre(i+ii)   = zmm2.v(ii)
                           zmm3.v(ii) = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                           csim(i+ii)   = zmm3.v(ii)
                       end do
                   end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif                     
                   do j = i, n=1
                       xr      = xre(j) ! z0
                       xi      = xim(j) ! z1
                       csr     = cos(xr)*cosh(xi)
                       csre(j) = csr
                       csi     = sin(xr)*sinh(xi)
                       csim(j) = csi
                   end do
                   return
        else if(n>32) then
                  do i=0, iand(n-1,inot(ZMM_LEN-1)), ZMM_LEN*8
                        call mm_prefetch(xre(i+8*ZMM_LEN),FOR_K_PREFETCH_T1)
                        call mm_prefetch(xim(i+8*ZMM_LEN),FOR_K_PREFETCH_T1)
#if defined(__ICC) || defined(__INTEL_COMPILER)
                        !dir$ assume_aligned  xre:64
                        !dir$ assume_aligned  xim:64
                        !dir$ assume_aligned  csre:64
                        !dir$ assume_aligned  csim:64
                  
#endif                   
                        !$omp simd aligned(xim:64,xre,csre,csim)  linear(ii:1)              
                         do ii = 0, ZMM_LEN-1  
                             zmm0.v(ii)     = xre(i+0+ii) ! xr
                             zmm1.v(ii)     = xim(i+0+ii) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(i+0+ii)   = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(i+0+ii)   = zmm3.v(ii)
                             idx1           = i+1*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx1) ! xr
                             zmm1.v(ii)     = xim(idx1) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx1)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx1)     = zmm3.v(ii)
                             idx2           = i+2*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx2) ! xr
                             zmm1.v(ii)     = xim(idx2) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx2)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx2)     = zmm3.v(ii)
                             idx3           = i+3*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx3) ! xr
                             zmm1.v(ii)     = xim(idx3) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx3)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx3)     = zmm3.v(ii)
                             idx4           = i+4*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx4) ! xr
                             zmm1.v(ii)     = xim(idx4) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx4)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx4)     = zmm3.v(ii)
                             idx5           = i+5*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx5) ! xr
                             zmm1.v(ii)     = xim(idx5) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx5)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx5)     = zmm3.v(ii)
                             idx6           = i+6*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx6) ! xr
                             zmm1.v(ii)     = xim(idx6) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx6)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx6)     = zmm3.v(ii)
                             idx7           = i+7*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx7) ! xr
                             zmm1.v(ii)     = xim(idx7) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx7)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx7)     = zmm3.v(ii)
                      end do
                  end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif                     
                   do j = i, n=1
                       xr      = xre(j) ! z0
                       xi      = xim(j) ! z1
                       csr     = cos(xr)*cosh(xi)
                       csre(j) = csr
                       csi     = sin(xr)*sinh(xi)
                       csim(j) = csi
                   end do                   
                   return   
           end if      
end subroutine ccosv_kernel_v512_cv_8x16_ps


subroutine ccosv_kernel_v512_cv_4x16_ps(xre,xim,csre,csim,n)
#if defined(__ICC) || defined(__INTEL_COMPILER)    
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: ccosv_kernel_v512_cv_4x16_ps
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: ccosv_kernel_v512_cv_4x16_ps
#endif     
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xre
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xim
         real(kind=sp), allocatable, dimension(:), intent(in)  :: csre
         real(kind=sp), allocatable, dimension(:), intent(in)  :: csim
         integer(i4),                              intent(in)  :: n
         
         type(ZMM16r4_t), automatic :: zmm0
         type(ZMM16r4_t), automatic :: zmm1
         type(ZMM16r4_t), automatic :: zmm2
         type(ZMM16r4_t), automatic :: zmm3     
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm1
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm2
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm3
         type(XMM4r4_t),  automatic  :: xmm0
         type(XMM4r4_t),  automatic  :: xmm1
         type(XMM4r4_t),  automatic  :: xmm2
         type(XMM4r4_t),  automatic  :: xmm3
         type(YMM8r4_t),  automatic  :: ymm0 
         type(YMM8r4_t),  automatic  :: ymm1
         type(YMM8r4_t),  automatic  :: ymm2
         type(YMM8r4_t),  automatic  :: ymm3
         real(sp),        automatic  :: xr
         real(sp),        automatic  :: xi
         real(sp),        automatic  :: csr
         real(sp),        automatic  :: csi
         integer(i4),     automatic  :: i,ii,j
         integer(i4),     automatic  :: idx1,idx2,idx3

         if(n<=0) then 
            return
         else if(n==1) then
              xr      = xre(0) ! z0
              xi      = xim(0) ! z1
              csr     = cos(xr)*cosh(xi)
              csre(0) = csr
              csi     = sin(xr)*sinh(xi)
              csim(0) = csi
              return
         else if(n>1 && n<=4) then
                 !$omp simd linear(i:1)
                 do i=0, 3
                    xmm0.v(i) = xre(i) ! xr
                    xmm1.v(i) = xim(i) ! xi
                    xmm2.v(i) = cos(xmm0.v(i))*cosh(xmm1.v(i))
                    csre(i)   = xmm2.v(i)
                    xmm3.v(i) = sin(xmm0.v(i))*sinh(xmm1.v(i))
                    csim(i)   = xmm3.v(i)
                 end do
                 return
         else if(n>4 && n<=8) then
                 !$omp simd linear(i:1)
                 do i=0, 7
                    ymm0.v(i) = xre(i) ! xr
                    ymm1.v(i) = xim(i) ! xi
                    ymm2.v(i) = cos(ymm0.v(i))*cosh(ymm1.v(i))
                    csre(i)   = ymm2.v(i)
                    ymm3.v(i) = sin(ymm0.v(i))*sinh(ymm1.v(i))
                    csim(i)   = ymm3.v(i)
                 end do
                 return
         else if(n>8 && n<=16) then
                 !$omp simd linear(i:1)
                 do i=0, 15
                    zmm0.v(i) = xre(i) ! xr
                    zmm1.v(i) = xim(i) ! xi
                    zmm2.v(i) = cos(zmm0.v(i))*cosh(zmm1.v(i))
                    csre(i)   = zmm2.v(i)
                    zmm3.v(i) = sin(zmm0.v(i))*sinh(zmm1.v(i))
                    csim(i)   = zmm3.v(i)
                 end do
                 return
          else if(n>16 && n<=32) then
                   do i = 0,iand(n-1,inot(15)),16
                       !$omp simd aligned(xim:64,xre,yre,yim,zre,zim) linear(ii:1)
                       do ii = 0, 15 
                           zmm0.v(ii) = xre(i+ii) ! xr
                           zmm1.v(ii) = xim(i+ii) ! xi
                           zmm2.v(ii) = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                           csre(i+ii)   = zmm2.v(ii)
                           zmm3.v(ii) = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                           csim(i+ii)   = zmm3.v(ii)
                       end do
                   end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif                     
                   do j = i, n=1
                       xr      = xre(j) ! z0
                       xi      = xim(j) ! z1
                       csr     = cos(xr)*cosh(xi)
                       csre(j) = csr
                       csi     = sin(xr)*sinh(xi)
                       csim(j) = csi
                   end do
                   return
        else if(n>32) then
                  do i=0, iand(n-1,inot(ZMM_LEN-1)), ZMM_LEN*4
                        call mm_prefetch(xre(i+4*ZMM_LEN),FOR_K_PREFETCH_T1)
                        call mm_prefetch(xim(i+4*ZMM_LEN),FOR_K_PREFETCH_T1)
#if defined(__ICC) || defined(__INTEL_COMPILER)
                        !dir$ assume_aligned  xre:64
                        !dir$ assume_aligned  xim:64
                        !dir$ assume_aligned  csre:64
                        !dir$ assume_aligned  csim:64
                  
#endif                   
                        !$omp simd aligned(xim:64,xre,csre,csim)  linear(ii:1)              
                         do ii = 0, ZMM_LEN-1  
                             zmm0.v(ii)     = xre(i+0+ii) ! xr
                             zmm1.v(ii)     = xim(i+0+ii) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(i+0+ii)   = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(i+0+ii)   = zmm3.v(ii)
                             idx1           = i+1*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx1) ! xr
                             zmm1.v(ii)     = xim(idx1) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx1)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx1)     = zmm3.v(ii)
                             idx2           = i+2*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx2) ! xr
                             zmm1.v(ii)     = xim(idx2) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx2)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx2)     = zmm3.v(ii)
                             idx3           = i+3*ZMM_LEN+ii
                             zmm0.v(ii)     = xre(idx3) ! xr
                             zmm1.v(ii)     = xim(idx3) ! xi
                             zmm2.v(ii)     = cos(zmm0.v(ii))*cosh(zmm1.v(ii))
                             csre(idx3)     = zmm2.v(ii)
                             zmm3.v(ii)     = sin(zmm0.v(ii))*sinh(zmm1.v(ii))
                             csim(idx3)     = zmm3.v(ii)
                      end do
                  end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif                     
                   do j = i, n=1
                       xr      = xre(j) ! z0
                       xi      = xim(j) ! z1
                       csr     = cos(xr)*cosh(xi)
                       csre(j) = csr
                       csi     = sin(xr)*sinh(xi)
                       csim(j) = csi
                   end do                   
                   return   
           end if      
end subroutine ccosv_kernel_v512_cv_4x16_ps

 

end module ccosv_zmm16r4
     