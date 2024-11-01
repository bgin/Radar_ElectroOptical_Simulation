
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

module cpow_vec_zmm16r4


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name: cpowv_smith_zmm16r4
 !                         
 !          
 !          Purpose:
 !                       Complex-power vectorized deinterleaved, single-precision
 !                        
 !          History:
 !                        Date: 01-11-2024
 !                        Time: 01:34PM GMT+2
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
     integer(kind=i4),  parameter :: CPOWV_ZMM16R4_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: CPOWV_ZMM16R4_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: CPOWV_ZMM16R4_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: CPOWV_ZMM16R4_FULLVER =   &
            1000*CPOWV_ZMM16R4_MAJOR+100*CPOWV_ZMM16R4_MINOR+10*CPOWV_ZMM16R4_MICRO
     ! Module creation date
     character(*),        parameter :: CPOWV_ZMM16R4_CREATE_DATE = "01-11-2024 01:59PM +00200 (SUN 01 NOV 2024 GMT+2)"
     ! Module build date
     character(*),        parameter :: CPOWV_ZMM16R4_BUILD_DATE  = __DATE__ " " __TIME__
     ! Module author info
     character(*),        parameter :: CPOWV_ZMM16R4_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: CPOWV_ZMM16R4_SYNOPSIS  = "Complex-Power vectorized deinterleaved, single-precision." 
     
    
     integer(kind=i4), parameter, private :: ZMM_LEN = 16
     
     
     contains
     
     
     
subroutine cpowv_v512_32x16_ps(xre,xim,vn,cpowr,cpowi,n)
#if defined(__ICC) || defined(__INTEL_COMPILER)    
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cpowv_v512_32x16_ps
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: cpowv_v512_32x16_ps
#endif     
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xre
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xim
         real(kind=sp), allocatable, dimension(:), intent(in)  :: vn
         real(kind=sp), allocatable, dimension(:), intent(in)  :: cpowr
         real(kind=sp), allocatable, dimension(:), intent(in)  :: cpowi
         integer(kind=i4),                         intent(in)  :: n

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
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm1
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm2
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm3
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm4
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm5
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm6
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm7
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm8
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm9
         type(XMM4r4_t),  automatic  :: xmm0
         type(XMM4r4_t),  automatic  :: xmm1
         type(XMM4r4_t),  automatic  :: xmm2
         type(XMM4r4_t),  automatic  :: xmm3
         type(XMM4r4_t),  automatic  :: xmm4
         type(XMM4r4_t),  automatic  :: xmm5
         type(XMM4r4_t),  automatic  :: xmm6
         type(XMM4r4_t),  automatic  :: xmm7 
         type(XMM4r4_t),  automatic  :: xmm8
         type(XMM4r4_t),  automatic  :: xmm9
         type(YMM8r4_t),  automatic  :: ymm0 
         type(YMM8r4_t),  automatic  :: ymm1
         type(YMM8r4_t),  automatic  :: ymm2
         type(YMM8r4_t),  automatic  :: ymm3
         type(YMM8r4_t),  automatic  :: ymm4
         type(YMM8r4_t),  automatic  :: ymm5
         type(YMM8r4_t),  automatic  :: ymm6
         type(YMM8r4_t),  automatic  :: ymm7     
         type(YMM8r4_t),  automatic  :: ymm8
         type(YMM8r4_t),  automatic  :: ymm9
         real(sp),        automatic  :: z0
         real(sp),        automatic  :: z1
         real(sp),        automatic  :: z2
         real(sp),        automatic  :: z3
         real(sp),        automatic  :: z4
         real(sp),        automatic  :: z5
         real(sp),        automatic  :: z6
         real(sp),        automatic  :: z7
         real(sp),        automatic  :: z8
         real(sp),        automatic  :: z9
         real(sp),        automatic  :: zx
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
                 z0 = xre(0)
                 z1 = z0*z0
                 z2 = xim(0)
                 z3 = z2*z2
                 z4 = sqrt(z1+z3)
                 zx = vn(0)
                 z5 = atan(z2/z0)
                 z6 = pow(z4,zx)
                 z7 = zx*z5
                 z8 = z6*cos(z7)
                 cpowr(0) = z8
                 z9 = z6*sin(z7)
                 cpowi(0) = z9
                 return
         else if(n>1 && n<=4)
!$omp simd linear(i:1)
                 do i=0, 3         
                    xmm0.v(i) = xre(i)
                    xmm1.v(i) = xmm0.v(i)*xmm0.v(i)
                    xmm2.v(i) = xim(i)
                    xmm3.v(i) = xmm2.v(i)*xmm2.v(i)
                    xmm4.v(i) = sqrt(xmm1.v(i)+xmm3.v(i))
                    xmm9.v(i) = vn(i)
                    xmm5.v(i) = atan(xmm2.v(i)/xmm0.v(i))
                    xmm6.v(i) = pow(xmm4.v(i),xmm9.v(i))
                    xmm7.v(i) = xmm9.v(i)*xmm5.v(i)
                    xmm8.v(i) = xmm6.v(i)*cos(xmm7.v(i))
                    cpowr(i)  = xmm8.v(i)
                    xmm9.v(i) = xmm6.v(i)*sin(xmm7.v(i))
                    cpowi(i)  = xmm9.v(i)
                 end do
                 return
         else if(n>4 && n<=8) then
!$omp simd linear(i:1)
                 do i=0, 7         
                    ymm0.v(i) = xre(i)
                    ymm1.v(i) = ymm0.v(i)*ymm0.v(i)
                    ymm2.v(i) = xim(i)
                    ymm3.v(i) = ymm2.v(i)*ymm2.v(i)
                    ymm4.v(i) = sqrt(ymm1.v(i)+ymm3.v(i))
                    ymm9.v(i) = vn(i)
                    ymm5.v(i) = atan(ymm2.v(i)/ymm0.v(i))
                    ymm6.v(i) = pow(ymm4.v(i),ymm9.v(i))
                    ymm7.v(i) = ymm9.v(i)*ymm5.v(i)
                    ymm8.v(i) = ymm6.v(i)*cos(ymm7.v(i))
                    cpowr(i)  = ymm8.v(i)
                    ymm9.v(i) = ymm6.v(i)*sin(ymm7.v(i))
                    cpowi(i)  = ymm9.v(i)
                 end do
                 return
         else if(n>8 && n<=16) then
!$omp simd linear(i:1)
                 do i=0, 15         
                    zmm0.v(i) = xre(i)
                    zmm1.v(i) = zmm0.v(i)*zmm0.v(i)
                    zmm2.v(i) = xim(i)
                    zmm3.v(i) = zmm2.v(i)*zmm2.v(i)
                    zmm4.v(i) = sqrt(zmm1.v(i)+zmm3.v(i))
                    zmm9.v(i) = vn(i)
                    zmm5.v(i) = atan(zmm2.v(i)/zmm0.v(i))
                    zmm6.v(i) = pow(zmm4.v(i),zmm9.v(i))
                    zmm7.v(i) = zmm9.v(i)*zmm5.v(i)
                    zmm8.v(i) = zmm6.v(i)*cos(zmm7.v(i))
                    cpowr(i)  = zmm8.v(i)
                    zmm9.v(i) = zmm6.v(i)*sin(zmm7.v(i))
                    cpowi(i)  = zmm9.v(i)
                 end do
                 return    
          else if(n>16 && n<=64) then
                  do i = 0,iand(n-1,inot(15)),16
!$omp simd aligned(xim:64,xre,vn,cpowr,cpowi) linear(ii:1)
                     do ii = 0, 15  
                        zmm0.v(ii) = xre(i+ii)
                        zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                        zmm2.v(ii) = xim(i+ii)
                        zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                        zmm4.v(ii) = sqrt(zmm1.v(ii)+zmm3.v(ii))
                        zmm9.v(ii) = vn(i+ii)
                        zmm5.v(ii) = atan(zmm2.v(ii)/zmm0.v(ii))
                        zmm6.v(ii) = pow(zmm4.v(ii),zmm9.v(ii))
                        zmm7.v(ii) = zmm9.v(ii)*zmm5.v(ii)
                        zmm8.v(ii) = zmm6.v(ii)*cos(zmm7.v(ii))
                        cpowr(i+ii)  = zmm8.v(ii)
                        zmm9.v(ii) = zmm6.v(ii)*sin(zmm7.v(ii))
                        cpowi(i+ii)  = zmm9.v(ii)
                     end do        
                  end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                 do j = i, n-1     
                     z0 = xre(j)
                     z1 = z0*z0
                     z2 = xim(j)
                     z3 = z2*z2
                     z4 = sqrt(z1+z3)
                     zx = vn(j)
                     z5 = atan(z2/z0)
                     z6 = pow(z4,zx)
                     z7 = zx*z5
                     z8 = z6*cos(z7)
                     cpowr(j) = z8
                     z9 = z6*sin(z7)
                     cpowi(j) = z9
                 end do             
                 return
         else if(n>64 && n<=128) then
                  do i = 0,iand(n-1,inot(15)),16
!$omp simd aligned(xim:64,xre,vn,cpowr,cpowi) linear(ii:1)
                     do ii = 0, 15  
                        zmm0.v(ii) = xre(i+ii)
                        zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                        zmm2.v(ii) = xim(i+ii)
                        zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                        zmm4.v(ii) = sqrt(zmm1.v(ii)+zmm3.v(ii))
                        zmm9.v(ii) = vn(i+ii)
                        zmm5.v(ii) = atan(zmm2.v(ii)/zmm0.v(ii))
                        zmm6.v(ii) = pow(zmm4.v(ii),zmm9.v(ii))
                        zmm7.v(ii) = zmm9.v(ii)*zmm5.v(ii)
                        zmm8.v(ii) = zmm6.v(ii)*cos(zmm7.v(ii))
                        cpowr(i+ii)  = zmm8.v(ii)
                        zmm9.v(ii) = zmm6.v(ii)*sin(zmm7.v(ii))
                        cpowi(i+ii)  = zmm9.v(ii)
                     end do        
                  end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                 do j = i, n-1     
                     z0 = xre(j)
                     z1 = z0*z0
                     z2 = xim(j)
                     z3 = z2*z2
                     z4 = sqrt(z1+z3)
                     zx = vn(j)
                     z5 = atan(z2/z0)
                     z6 = pow(z4,zx)
                     z7 = zx*z5
                     z8 = z6*cos(z7)
                     cpowr(j) = z8
                     z9 = z6*sin(z7)
                     cpowi(j) = z9
                 end do             
                 return
          else if(n>128) then
                do i=0, iand(n-1,inot(ZMM_LEN-1)), ZMM_LEN*32
                   call mm_prefetch(xre(i+32*ZMM_LEN),FOR_K_PREFETCH_T1)
                   call mm_prefetch(xim(i+32*ZMM_LEN),FOR_K_PREFETCH_T1)
                   call mm_prefetch(vn(i+32*ZMM_LEN),FOR_K_PREFETCH_T1)
#if defined(__ICC) || defined(__INTEL_COMPILER)
                   !dir$ assume_aligned  xre:64
                   !dir$ assume_aligned  xim:64
                   !dir$ assume_aligned  yre:64
                   !dir$ assume_aligned  yim:64
                   !dir$ assume_aligned  zre:64
                   !dir$ assume_aligned  zim:64
#endif                   
!$omp simd aligned(xim:64,xre,vn,cpowr,cpowi)  linear(ii:1)              
                   do ii = 0, ZMM_LEN-1
                        zmm0.v(ii) = xre(i+0+ii)
                        zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                        zmm2.v(ii) = xim(i+0+ii)
                        zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                        zmm4.v(ii) = sqrt(zmm1.v(ii)+zmm3.v(ii))
                        zmm9.v(ii) = vn(i+0+ii)
                        zmm5.v(ii) = atan(zmm2.v(ii)/zmm0.v(ii))
                        zmm6.v(ii) = pow(zmm4.v(ii),zmm9.v(ii))
                        zmm7.v(ii) = zmm9.v(ii)*zmm5.v(ii)
                        zmm8.v(ii) = zmm6.v(ii)*cos(zmm7.v(ii))
                        cpowr(i+0+ii)  = zmm8.v(ii)
                        zmm9.v(ii) = zmm6.v(ii)*sin(zmm7.v(ii))
                        cpowi(i+0+ii)  = zmm9.v(ii)
                        idx1           = i+1*ZMM_LEN+ii
                        zmm0.v(ii) = xre(idx1)
                        zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                        zmm2.v(ii) = xim(idx1)
                        zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                        zmm4.v(ii) = sqrt(zmm1.v(ii)+zmm3.v(ii))
                        zmm9.v(ii) = vn(idx1)
                        zmm5.v(ii) = atan(zmm2.v(ii)/zmm0.v(ii))
                        zmm6.v(ii) = pow(zmm4.v(ii),zmm9.v(ii))
                        zmm7.v(ii) = zmm9.v(ii)*zmm5.v(ii)
                        zmm8.v(ii) = zmm6.v(ii)*cos(zmm7.v(ii))
                        cpowr(idx1)  = zmm8.v(ii)
                        zmm9.v(ii) = zmm6.v(ii)*sin(zmm7.v(ii))
                        cpowi(idx1)  = zmm9.v(ii)
                        idx2         = i+2*ZMM_LEN+ii
                        zmm0.v(ii) = xre(idx2)
                        zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                        zmm2.v(ii) = xim(idx2)
                        zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                        zmm4.v(ii) = sqrt(zmm1.v(ii)+zmm3.v(ii))
                        zmm9.v(ii) = vn(idx2)
                        zmm5.v(ii) = atan(zmm2.v(ii)/zmm0.v(ii))
                        zmm6.v(ii) = pow(zmm4.v(ii),zmm9.v(ii))
                        zmm7.v(ii) = zmm9.v(ii)*zmm5.v(ii)
                        zmm8.v(ii) = zmm6.v(ii)*cos(zmm7.v(ii))
                        cpowr(idx2)  = zmm8.v(ii)
                        zmm9.v(ii) = zmm6.v(ii)*sin(zmm7.v(ii))
                        cpowi(idx2)  = zmm9.v(ii)
                        idx3         = i+3*ZMM_LEN+ii
                        zmm0.v(ii) = xre(idx3)
                        zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                        zmm2.v(ii) = xim(idx3)
                        zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                        zmm4.v(ii) = sqrt(zmm1.v(ii)+zmm3.v(ii))
                        zmm9.v(ii) = vn(idx3)
                        zmm5.v(ii) = atan(zmm2.v(ii)/zmm0.v(ii))
                        zmm6.v(ii) = pow(zmm4.v(ii),zmm9.v(ii))
                        zmm7.v(ii) = zmm9.v(ii)*zmm5.v(ii)
                        zmm8.v(ii) = zmm6.v(ii)*cos(zmm7.v(ii))
                        cpowr(idx3)  = zmm8.v(ii)
                        zmm9.v(ii) = zmm6.v(ii)*sin(zmm7.v(ii))
                        cpowi(idx3)  = zmm9.v(ii)
                        idx4         = i+4*ZMM_LEN+ii
                        zmm0.v(ii) = xre(idx4)
                        zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                        zmm2.v(ii) = xim(idx4)
                        zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                        zmm4.v(ii) = sqrt(zmm1.v(ii)+zmm3.v(ii))
                        zmm9.v(ii) = vn(idx4)
                        zmm5.v(ii) = atan(zmm2.v(ii)/zmm0.v(ii))
                        zmm6.v(ii) = pow(zmm4.v(ii),zmm9.v(ii))
                        zmm7.v(ii) = zmm9.v(ii)*zmm5.v(ii)
                        zmm8.v(ii) = zmm6.v(ii)*cos(zmm7.v(ii))
                        cpowr(idx4)  = zmm8.v(ii)
                        zmm9.v(ii) = zmm6.v(ii)*sin(zmm7.v(ii))
                        cpowi(idx4)  = zmm9.v(ii)
                        idx5         = i+5*ZMM_LEN+ii
                        zmm0.v(ii) = xre(idx5)
                        zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                        zmm2.v(ii) = xim(idx5)
                        zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                        zmm4.v(ii) = sqrt(zmm1.v(ii)+zmm3.v(ii))
                        zmm9.v(ii) = vn(idx5)
                        zmm5.v(ii) = atan(zmm2.v(ii)/zmm0.v(ii))
                        zmm6.v(ii) = pow(zmm4.v(ii),zmm9.v(ii))
                        zmm7.v(ii) = zmm9.v(ii)*zmm5.v(ii)
                        zmm8.v(ii) = zmm6.v(ii)*cos(zmm7.v(ii))
                        cpowr(idx5)  = zmm8.v(ii)
                        zmm9.v(ii) = zmm6.v(ii)*sin(zmm7.v(ii))
                        cpowi(idx5)  = zmm9.v(ii)
                        idx6         = i+6*ZMM_LEN+ii
                        zmm0.v(ii) = xre(idx6)
                        zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                        zmm2.v(ii) = xim(idx6)
                        zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                        zmm4.v(ii) = sqrt(zmm1.v(ii)+zmm3.v(ii))
                        zmm9.v(ii) = vn(idx6)
                        zmm5.v(ii) = atan(zmm2.v(ii)/zmm0.v(ii))
                        zmm6.v(ii) = pow(zmm4.v(ii),zmm9.v(ii))
                        zmm7.v(ii) = zmm9.v(ii)*zmm5.v(ii)
                        zmm8.v(ii) = zmm6.v(ii)*cos(zmm7.v(ii))
                        cpowr(idx6)  = zmm8.v(ii)
                        zmm9.v(ii) = zmm6.v(ii)*sin(zmm7.v(ii))
                        cpowi(idx6)  = zmm9.v(ii)
                        idx7         = i+7*ZMM_LEN+ii
                        zmm0.v(ii) = xre(idx7)
                        zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                        zmm2.v(ii) = xim(idx7)
                        zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                        zmm4.v(ii) = sqrt(zmm1.v(ii)+zmm3.v(ii))
                        zmm9.v(ii) = vn(idx7)
                        zmm5.v(ii) = atan(zmm2.v(ii)/zmm0.v(ii))
                        zmm6.v(ii) = pow(zmm4.v(ii),zmm9.v(ii))
                        zmm7.v(ii) = zmm9.v(ii)*zmm5.v(ii)
                        zmm8.v(ii) = zmm6.v(ii)*cos(zmm7.v(ii))
                        cpowr(idx7)  = zmm8.v(ii)
                        zmm9.v(ii) = zmm6.v(ii)*sin(zmm7.v(ii))
                        cpowi(idx7)  = zmm9.v(ii)
                        idx8         = i+8*ZMM_LEN+ii
                        zmm0.v(ii) = xre(idx8)
                        zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                        zmm2.v(ii) = xim(idx8)
                        zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                        zmm4.v(ii) = sqrt(zmm1.v(ii)+zmm3.v(ii))
                        zmm9.v(ii) = vn(idx8)
                        zmm5.v(ii) = atan(zmm2.v(ii)/zmm0.v(ii))
                        zmm6.v(ii) = pow(zmm4.v(ii),zmm9.v(ii))
                        zmm7.v(ii) = zmm9.v(ii)*zmm5.v(ii)
                        zmm8.v(ii) = zmm6.v(ii)*cos(zmm7.v(ii))
                        cpowr(idx8)  = zmm8.v(ii)
                        zmm9.v(ii) = zmm6.v(ii)*sin(zmm7.v(ii))
                        cpowi(idx8)  = zmm9.v(ii)
                        idx9         = i+9*ZMM_LEN+ii
                        zmm0.v(ii) = xre(idx9)
                        zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                        zmm2.v(ii) = xim(idx9)
                        zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                        zmm4.v(ii) = sqrt(zmm1.v(ii)+zmm3.v(ii))
                        zmm9.v(ii) = vn(idx9)
                        zmm5.v(ii) = atan(zmm2.v(ii)/zmm0.v(ii))
                        zmm6.v(ii) = pow(zmm4.v(ii),zmm9.v(ii))
                        zmm7.v(ii) = zmm9.v(ii)*zmm5.v(ii)
                        zmm8.v(ii) = zmm6.v(ii)*cos(zmm7.v(ii))
                        cpowr(idx9)  = zmm8.v(ii)
                        zmm9.v(ii) = zmm6.v(ii)*sin(zmm7.v(ii))
                        cpowi(idx9)  = zmm9.v(ii)
                        idx10       = i+10*ZMM_LEN+ii
                        zmm0.v(ii) = xre(idx10)
                        zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                        zmm2.v(ii) = xim(idx10)
                        zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                        zmm4.v(ii) = sqrt(zmm1.v(ii)+zmm3.v(ii))
                        zmm9.v(ii) = vn(idx10)
                        zmm5.v(ii) = atan(zmm2.v(ii)/zmm0.v(ii))
                        zmm6.v(ii) = pow(zmm4.v(ii),zmm9.v(ii))
                        zmm7.v(ii) = zmm9.v(ii)*zmm5.v(ii)
                        zmm8.v(ii) = zmm6.v(ii)*cos(zmm7.v(ii))
                        cpowr(idx10)  = zmm8.v(ii)
                        zmm9.v(ii) = zmm6.v(ii)*sin(zmm7.v(ii))
                        cpowi(idx10)  = zmm9.v(ii)
                        idx11         = i+11*ZMM_LEN+ii
                        zmm0.v(ii) = xre(idx11)
                        zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                        zmm2.v(ii) = xim(idx11)
                        zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                        zmm4.v(ii) = sqrt(zmm1.v(ii)+zmm3.v(ii))
                        zmm9.v(ii) = vn(idx11)
                        zmm5.v(ii) = atan(zmm2.v(ii)/zmm0.v(ii))
                        zmm6.v(ii) = pow(zmm4.v(ii),zmm9.v(ii))
                        zmm7.v(ii) = zmm9.v(ii)*zmm5.v(ii)
                        zmm8.v(ii) = zmm6.v(ii)*cos(zmm7.v(ii))
                        cpowr(idx11)  = zmm8.v(ii)
                        zmm9.v(ii) = zmm6.v(ii)*sin(zmm7.v(ii))
                        cpowi(idx11)  = zmm9.v(ii)
                        idx12         = i+12*ZMM_LEN+ii
                        zmm0.v(ii) = xre(idx12)
                        zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                        zmm2.v(ii) = xim(idx12)
                        zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                        zmm4.v(ii) = sqrt(zmm1.v(ii)+zmm3.v(ii))
                        zmm9.v(ii) = vn(idx12)
                        zmm5.v(ii) = atan(zmm2.v(ii)/zmm0.v(ii))
                        zmm6.v(ii) = pow(zmm4.v(ii),zmm9.v(ii))
                        zmm7.v(ii) = zmm9.v(ii)*zmm5.v(ii)
                        zmm8.v(ii) = zmm6.v(ii)*cos(zmm7.v(ii))
                        cpowr(idx12)  = zmm8.v(ii)
                        zmm9.v(ii) = zmm6.v(ii)*sin(zmm7.v(ii))
                        cpowi(idx12)  = zmm9.v(ii)
                        idx13         = i+13*ZMM_LEN+ii
                        zmm0.v(ii) = xre(idx13)
                        zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                        zmm2.v(ii) = xim(idx13)
                        zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                        zmm4.v(ii) = sqrt(zmm1.v(ii)+zmm3.v(ii))
                        zmm9.v(ii) = vn(idx13)
                        zmm5.v(ii) = atan(zmm2.v(ii)/zmm0.v(ii))
                        zmm6.v(ii) = pow(zmm4.v(ii),zmm9.v(ii))
                        zmm7.v(ii) = zmm9.v(ii)*zmm5.v(ii)
                        zmm8.v(ii) = zmm6.v(ii)*cos(zmm7.v(ii))
                        cpowr(idx13)  = zmm8.v(ii)
                        zmm9.v(ii) = zmm6.v(ii)*sin(zmm7.v(ii))
                        cpowi(idx13)  = zmm9.v(ii)
                        idx14         = i+14*ZMM_LEN+ii
                        zmm0.v(ii) = xre(idx14)
                        zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                        zmm2.v(ii) = xim(idx14)
                        zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                        zmm4.v(ii) = sqrt(zmm1.v(ii)+zmm3.v(ii))
                        zmm9.v(ii) = vn(idx14)
                        zmm5.v(ii) = atan(zmm2.v(ii)/zmm0.v(ii))
                        zmm6.v(ii) = pow(zmm4.v(ii),zmm9.v(ii))
                        zmm7.v(ii) = zmm9.v(ii)*zmm5.v(ii)
                        zmm8.v(ii) = zmm6.v(ii)*cos(zmm7.v(ii))
                        cpowr(idx14)  = zmm8.v(ii)
                        zmm9.v(ii) = zmm6.v(ii)*sin(zmm7.v(ii))
                        cpowi(idx14)  = zmm9.v(ii)
                        idx15         = i+15*ZMM_LEN+ii
                        zmm0.v(ii) = xre(idx15)
                        zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                        zmm2.v(ii) = xim(idx15)
                        zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                        zmm4.v(ii) = sqrt(zmm1.v(ii)+zmm3.v(ii))
                        zmm9.v(ii) = vn(idx15)
                        zmm5.v(ii) = atan(zmm2.v(ii)/zmm0.v(ii))
                        zmm6.v(ii) = pow(zmm4.v(ii),zmm9.v(ii))
                        zmm7.v(ii) = zmm9.v(ii)*zmm5.v(ii)
                        zmm8.v(ii) = zmm6.v(ii)*cos(zmm7.v(ii))
                        cpowr(idx15)  = zmm8.v(ii)
                        zmm9.v(ii) = zmm6.v(ii)*sin(zmm7.v(ii))
                        cpowi(idx15)  = zmm9.v(ii)
                        idx16         = i+16*ZMM_LEN+ii
                        zmm0.v(ii) = xre(idx16)
                        zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                        zmm2.v(ii) = xim(idx16)
                        zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                        zmm4.v(ii) = sqrt(zmm1.v(ii)+zmm3.v(ii))
                        zmm9.v(ii) = vn(idx16)
                        zmm5.v(ii) = atan(zmm2.v(ii)/zmm0.v(ii))
                        zmm6.v(ii) = pow(zmm4.v(ii),zmm9.v(ii))
                        zmm7.v(ii) = zmm9.v(ii)*zmm5.v(ii)
                        zmm8.v(ii) = zmm6.v(ii)*cos(zmm7.v(ii))
                        cpowr(idx16)  = zmm8.v(ii)
                        zmm9.v(ii) = zmm6.v(ii)*sin(zmm7.v(ii))
                        cpowi(idx16)  = zmm9.v(ii)
                        idx17         = i+17*ZMM_LEN+ii
                        zmm0.v(ii) = xre(idx17)
                        zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                        zmm2.v(ii) = xim(idx17)
                        zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                        zmm4.v(ii) = sqrt(zmm1.v(ii)+zmm3.v(ii))
                        zmm9.v(ii) = vn(idx17)
                        zmm5.v(ii) = atan(zmm2.v(ii)/zmm0.v(ii))
                        zmm6.v(ii) = pow(zmm4.v(ii),zmm9.v(ii))
                        zmm7.v(ii) = zmm9.v(ii)*zmm5.v(ii)
                        zmm8.v(ii) = zmm6.v(ii)*cos(zmm7.v(ii))
                        cpowr(idx17)  = zmm8.v(ii)
                        zmm9.v(ii) = zmm6.v(ii)*sin(zmm7.v(ii))
                        cpowi(idx17)  = zmm9.v(ii)
                        idx18         = i+18*ZMM_LEN+ii
                        zmm0.v(ii) = xre(idx18)
                        zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                        zmm2.v(ii) = xim(idx18)
                        zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                        zmm4.v(ii) = sqrt(zmm1.v(ii)+zmm3.v(ii))
                        zmm9.v(ii) = vn(idx18)
                        zmm5.v(ii) = atan(zmm2.v(ii)/zmm0.v(ii))
                        zmm6.v(ii) = pow(zmm4.v(ii),zmm9.v(ii))
                        zmm7.v(ii) = zmm9.v(ii)*zmm5.v(ii)
                        zmm8.v(ii) = zmm6.v(ii)*cos(zmm7.v(ii))
                        cpowr(idx18)  = zmm8.v(ii)
                        zmm9.v(ii) = zmm6.v(ii)*sin(zmm7.v(ii))
                        cpowi(idx18)  = zmm9.v(ii)
                        idx19         = i+19*ZMM_LEN+ii
                        zmm0.v(ii) = xre(idx19)
                        zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                        zmm2.v(ii) = xim(idx19)
                        zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                        zmm4.v(ii) = sqrt(zmm1.v(ii)+zmm3.v(ii))
                        zmm9.v(ii) = vn(idx19)
                        zmm5.v(ii) = atan(zmm2.v(ii)/zmm0.v(ii))
                        zmm6.v(ii) = pow(zmm4.v(ii),zmm9.v(ii))
                        zmm7.v(ii) = zmm9.v(ii)*zmm5.v(ii)
                        zmm8.v(ii) = zmm6.v(ii)*cos(zmm7.v(ii))
                        cpowr(idx19)  = zmm8.v(ii)
                        zmm9.v(ii) = zmm6.v(ii)*sin(zmm7.v(ii))
                        cpowi(idx19)  = zmm9.v(ii)
                        idx20         = i+20*ZMM_LEN+ii
                        zmm0.v(ii) = xre(idx20)
                        zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                        zmm2.v(ii) = xim(idx20)
                        zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                        zmm4.v(ii) = sqrt(zmm1.v(ii)+zmm3.v(ii))
                        zmm9.v(ii) = vn(idx20)
                        zmm5.v(ii) = atan(zmm2.v(ii)/zmm0.v(ii))
                        zmm6.v(ii) = pow(zmm4.v(ii),zmm9.v(ii))
                        zmm7.v(ii) = zmm9.v(ii)*zmm5.v(ii)
                        zmm8.v(ii) = zmm6.v(ii)*cos(zmm7.v(ii))
                        cpowr(idx20)  = zmm8.v(ii)
                        zmm9.v(ii) = zmm6.v(ii)*sin(zmm7.v(ii))
                        cpowi(idx20)  = zmm9.v(ii)
                        idx21         = i+21*ZMM_LEN+ii
                        zmm0.v(ii) = xre(idx21)
                        zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                        zmm2.v(ii) = xim(idx21)
                        zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                        zmm4.v(ii) = sqrt(zmm1.v(ii)+zmm3.v(ii))
                        zmm9.v(ii) = vn(idx21)
                        zmm5.v(ii) = atan(zmm2.v(ii)/zmm0.v(ii))
                        zmm6.v(ii) = pow(zmm4.v(ii),zmm9.v(ii))
                        zmm7.v(ii) = zmm9.v(ii)*zmm5.v(ii)
                        zmm8.v(ii) = zmm6.v(ii)*cos(zmm7.v(ii))
                        cpowr(idx21)  = zmm8.v(ii)
                        zmm9.v(ii) = zmm6.v(ii)*sin(zmm7.v(ii))
                        cpowi(idx21)  = zmm9.v(ii)
                        idx22         = i+22*ZMM_LEN+ii
                        zmm0.v(ii) = xre(idx22)
                        zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                        zmm2.v(ii) = xim(idx22)
                        zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                        zmm4.v(ii) = sqrt(zmm1.v(ii)+zmm3.v(ii))
                        zmm9.v(ii) = vn(idx22)
                        zmm5.v(ii) = atan(zmm2.v(ii)/zmm0.v(ii))
                        zmm6.v(ii) = pow(zmm4.v(ii),zmm9.v(ii))
                        zmm7.v(ii) = zmm9.v(ii)*zmm5.v(ii)
                        zmm8.v(ii) = zmm6.v(ii)*cos(zmm7.v(ii))
                        cpowr(idx22)  = zmm8.v(ii)
                        zmm9.v(ii) = zmm6.v(ii)*sin(zmm7.v(ii))
                        cpowi(idx22)  = zmm9.v(ii)
                        idx23         = i+23*ZMM_LEN+ii
                        zmm0.v(ii) = xre(idx23)
                        zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                        zmm2.v(ii) = xim(idx23)
                        zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                        zmm4.v(ii) = sqrt(zmm1.v(ii)+zmm3.v(ii))
                        zmm9.v(ii) = vn(idx23)
                        zmm5.v(ii) = atan(zmm2.v(ii)/zmm0.v(ii))
                        zmm6.v(ii) = pow(zmm4.v(ii),zmm9.v(ii))
                        zmm7.v(ii) = zmm9.v(ii)*zmm5.v(ii)
                        zmm8.v(ii) = zmm6.v(ii)*cos(zmm7.v(ii))
                        cpowr(idx23)  = zmm8.v(ii)
                        zmm9.v(ii) = zmm6.v(ii)*sin(zmm7.v(ii))
                        cpowi(idx23)  = zmm9.v(ii)
                        idx24         = i+24*ZMM_LEN+ii
                        zmm0.v(ii) = xre(idx24)
                        zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                        zmm2.v(ii) = xim(idx24)
                        zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                        zmm4.v(ii) = sqrt(zmm1.v(ii)+zmm3.v(ii))
                        zmm9.v(ii) = vn(idx24)
                        zmm5.v(ii) = atan(zmm2.v(ii)/zmm0.v(ii))
                        zmm6.v(ii) = pow(zmm4.v(ii),zmm9.v(ii))
                        zmm7.v(ii) = zmm9.v(ii)*zmm5.v(ii)
                        zmm8.v(ii) = zmm6.v(ii)*cos(zmm7.v(ii))
                        cpowr(idx24)  = zmm8.v(ii)
                        zmm9.v(ii) = zmm6.v(ii)*sin(zmm7.v(ii))
                        cpowi(idx24)  = zmm9.v(ii)
                        idx25         = i+25*ZMM_LEN+ii
                        zmm0.v(ii) = xre(idx25)
                        zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                        zmm2.v(ii) = xim(idx25)
                        zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                        zmm4.v(ii) = sqrt(zmm1.v(ii)+zmm3.v(ii))
                        zmm9.v(ii) = vn(idx25)
                        zmm5.v(ii) = atan(zmm2.v(ii)/zmm0.v(ii))
                        zmm6.v(ii) = pow(zmm4.v(ii),zmm9.v(ii))
                        zmm7.v(ii) = zmm9.v(ii)*zmm5.v(ii)
                        zmm8.v(ii) = zmm6.v(ii)*cos(zmm7.v(ii))
                        cpowr(idx25)  = zmm8.v(ii)
                        zmm9.v(ii) = zmm6.v(ii)*sin(zmm7.v(ii))
                        cpowi(idx25)  = zmm9.v(ii)
                        idx26         = i+26*ZMM_LEN+ii
                        zmm0.v(ii) = xre(idx26)
                        zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                        zmm2.v(ii) = xim(idx26)
                        zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                        zmm4.v(ii) = sqrt(zmm1.v(ii)+zmm3.v(ii))
                        zmm9.v(ii) = vn(idx26)
                        zmm5.v(ii) = atan(zmm2.v(ii)/zmm0.v(ii))
                        zmm6.v(ii) = pow(zmm4.v(ii),zmm9.v(ii))
                        zmm7.v(ii) = zmm9.v(ii)*zmm5.v(ii)
                        zmm8.v(ii) = zmm6.v(ii)*cos(zmm7.v(ii))
                        cpowr(idx26)  = zmm8.v(ii)
                        zmm9.v(ii) = zmm6.v(ii)*sin(zmm7.v(ii))
                        cpowi(idx26)  = zmm9.v(ii)
                        idx27         = i+27*ZMM_LEN+ii
                        zmm0.v(ii) = xre(idx27)
                        zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                        zmm2.v(ii) = xim(idx27)
                        zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                        zmm4.v(ii) = sqrt(zmm1.v(ii)+zmm3.v(ii))
                        zmm9.v(ii) = vn(idx27)
                        zmm5.v(ii) = atan(zmm2.v(ii)/zmm0.v(ii))
                        zmm6.v(ii) = pow(zmm4.v(ii),zmm9.v(ii))
                        zmm7.v(ii) = zmm9.v(ii)*zmm5.v(ii)
                        zmm8.v(ii) = zmm6.v(ii)*cos(zmm7.v(ii))
                        cpowr(idx27)  = zmm8.v(ii)
                        zmm9.v(ii) = zmm6.v(ii)*sin(zmm7.v(ii))
                        cpowi(idx27)  = zmm9.v(ii)
                        idx28         = i+28*ZMM_LEN+ii
                        zmm0.v(ii) = xre(idx29)
                        zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                        zmm2.v(ii) = xim(idx29)
                        zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                        zmm4.v(ii) = sqrt(zmm1.v(ii)+zmm3.v(ii))
                        zmm9.v(ii) = vn(idx29)
                        zmm5.v(ii) = atan(zmm2.v(ii)/zmm0.v(ii))
                        zmm6.v(ii) = pow(zmm4.v(ii),zmm9.v(ii))
                        zmm7.v(ii) = zmm9.v(ii)*zmm5.v(ii)
                        zmm8.v(ii) = zmm6.v(ii)*cos(zmm7.v(ii))
                        cpowr(idx29)  = zmm8.v(ii)
                        zmm9.v(ii) = zmm6.v(ii)*sin(zmm7.v(ii))
                        cpowi(idx29)  = zmm9.v(ii)
                        idx30         = i+30*ZMM_LEN+ii
                        zmm0.v(ii) = xre(idx30)
                        zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                        zmm2.v(ii) = xim(idx30)
                        zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                        zmm4.v(ii) = sqrt(zmm1.v(ii)+zmm3.v(ii))
                        zmm9.v(ii) = vn(idx30)
                        zmm5.v(ii) = atan(zmm2.v(ii)/zmm0.v(ii))
                        zmm6.v(ii) = pow(zmm4.v(ii),zmm9.v(ii))
                        zmm7.v(ii) = zmm9.v(ii)*zmm5.v(ii)
                        zmm8.v(ii) = zmm6.v(ii)*cos(zmm7.v(ii))
                        cpowr(idx30)  = zmm8.v(ii)
                        zmm9.v(ii) = zmm6.v(ii)*sin(zmm7.v(ii))
                        cpowi(idx30)  = zmm9.v(ii)
                        idx31         = i+31*ZMM_LEN+ii
                        zmm0.v(ii) = xre(idx31)
                        zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                        zmm2.v(ii) = xim(idx31)
                        zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                        zmm4.v(ii) = sqrt(zmm1.v(ii)+zmm3.v(ii))
                        zmm9.v(ii) = vn(idx31)
                        zmm5.v(ii) = atan(zmm2.v(ii)/zmm0.v(ii))
                        zmm6.v(ii) = pow(zmm4.v(ii),zmm9.v(ii))
                        zmm7.v(ii) = zmm9.v(ii)*zmm5.v(ii)
                        zmm8.v(ii) = zmm6.v(ii)*cos(zmm7.v(ii))
                        cpowr(idx31)  = zmm8.v(ii)
                        zmm9.v(ii) = zmm6.v(ii)*sin(zmm7.v(ii))
                        cpowi(idx31)  = zmm9.v(ii)
                   end do
               end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                 do j = i, n-1     
                     z0 = xre(j)
                     z1 = z0*z0
                     z2 = xim(j)
                     z3 = z2*z2
                     z4 = sqrt(z1+z3)
                     zx = vn(j)
                     z5 = atan(z2/z0)
                     z6 = pow(z4,zx)
                     z7 = zx*z5
                     z8 = z6*cos(z7)
                     cpowr(j) = z8
                     z9 = z6*sin(z7)
                     cpowi(j) = z9
                 end do  
                 return                     
          end if
end subroutine  cpowv_v512_32x16_ps
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
end module cpow_vec_zmm16r4
     
     

