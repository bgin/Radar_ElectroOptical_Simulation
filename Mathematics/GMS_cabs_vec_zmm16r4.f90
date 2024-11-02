

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

module cabsv_zmm16r4


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name: cabsv_zmm16r4
 !                         
 !          
 !          Purpose:
 !                       Complex-absolute vectorized deinterleaved, single-precision
 !                        
 !          History:
 !                        Date: 02-11-2024
 !                        Time: 12:47PM GMT+2
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
     integer(kind=i4),  parameter :: CABSV_ZMM16R4_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: CABSV_ZMM16R4_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: CABSV_ZMM16R4_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: CABSV_ZMM16R4_FULLVER =   &
            1000*CABSV_ZMM16R4_MAJOR+100*CABSV_ZMM16R4_MINOR+10*CABSV_ZMM16R4_MICRO
     ! Module creation date
     character(*),        parameter :: CABSV_ZMM16R4_CREATE_DATE = "02-11-2024 12:47PM +00200 (SAT 02 NOV 2024 GMT+2)"
     ! Module build date
     character(*),        parameter :: CABSV_ZMM16R4_BUILD_DATE  = __DATE__ " " __TIME__
     ! Module author info
     character(*),        parameter :: CABSV_ZMM16R4_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: CABSV_ZMM16R4_SYNOPSIS  = "Complex-absolute vectorized deinterleaved, single-precision." 
     
    
     integer(kind=i4), parameter, private :: ZMM_LEN = 16
     

     
     contains
     
     
subroutine cabsv_kernel_v512_32x16_ps(xre,xim,cabs,n)
#if defined(__ICC) || defined(__INTEL_COMPILER)    
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cabsv_kernel_v512_32x16_ps
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: cabsv_kernel_v512_32x16_ps
#endif     
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xre
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xim
         real(kind=sp), allocatable, dimension(:), intent(out) :: cabs    
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
         real(sp),        automatic  :: re2
         real(sp),        automatic  :: im2
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
                xr      = xre(0)
                re2     = xr*xr
                xi      = xim(0)
                im2     = xi*xi
                cabs(0) = sqrt(re2+im2)
                return
         else if(n>1 && n<=4) then
!$omp simd linear(i:1)
                 do i=0, 3
                    xmm0.v(i) = xre(i)
                    xmm1.v(i) = xmm0.v(i)*xmm0.v(i)
                    xmm2.v(i) = xim(i)
                    xmm3.v(i) = xmm2.v(i)*xmm2.v(i)
                    cabs(i)   = sqrt(xmm1.v(i)+xmm3.v(i))
                 end do                  
                 return
         else if(n>4 && n<=8) then
!$omp simd linear(i:1)
                 do i=0, 7
                    ymm0.v(i) = xre(i)
                    ymm1.v(i) = ymm0.v(i)*ymm0.v(i)
                    ymm2.v(i) = xim(i)
                    ymm3.v(i) = ymm2.v(i)*ymm2.v(i)
                    cabs(i)   = sqrt(ymm1.v(i)+ymm3.v(i))
                 end do    
                 return
         else if(n>8 && n<=16) then
!$omp simd linear(i:1)
                 do i=0, 15
                    zmm0.v(i) = xre(i)
                    zmm1.v(i) = zmm0.v(i)*zmm0.v(i)
                    zmm2.v(i) = xim(i)
                    zmm3.v(i) = zmm2.v(i)*zmm2.v(i)
                    cabs(i)   = sqrt(zmm1.v(i)+zmm3.v(i))
                 end do    
                 return    
         else if(n>16 && n<=64) then
                 do i = 0,iand(n-1,inot(15)),16
!$omp simd aligned(xim:64,xre,cabs) linear(ii:1)
                     do ii = 0, 15
                         zmm0.v(ii) = xre(i+ii)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(i+ii)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(i+ii)   = sqrt(zmm1.v(ii)+zmm3.v(ii))
                     end do
                 end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                do j = i, n-1
                     xr      = xre(j)
                     re2     = xr*xr
                     xi      = xim(j)
                     im2     = xi*xi
                     cabs(j) = sqrt(re2+im2)
                end do
                return
            else if(n>64 && n<=128) then
                 do i = 0,iand(n-1,inot(15)),16
!$omp simd aligned(xim:64,xre,cabs) linear(ii:1)
                     do ii = 0, 15
                         zmm0.v(ii) = xre(i+ii)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(i+ii)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(i+ii)   = sqrt(zmm1.v(ii)+zmm3.v(ii))
                     end do
                 end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                do j = i, n-1
                     xr      = xre(j)
                     re2     = xr*xr
                     xi      = xim(j)
                     im2     = xi*xi
                     cabs(j) = sqrt(re2+im2)
                end do
                return
          else if(n>128) then
                do i=0, iand(n-1,inot(ZMM_LEN-1)), ZMM_LEN*32
                   call mm_prefetch(xre(i+32*ZMM_LEN),FOR_K_PREFETCH_T1)
                   call mm_prefetch(xim(i+32*ZMM_LEN),FOR_K_PREFETCH_T1)
#if defined(__ICC) || defined(__INTEL_COMPILER)
                   !dir$ assume_aligned  xre:64
                   !dir$ assume_aligned  xim:64
                   !dir$ assume_aligned  cabs:64
                  
#endif                   
!$omp simd aligned(xim:64,xre,cabs)  linear(ii:1)              
                    do ii = 0, ZMM_LEN-1
                         zmm0.v(ii) = xre(i+0+ii)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(i+0+ii)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(i+0+ii)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx1        = i+1*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx1)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx1)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx1)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx2        = i+2*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx2)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx2)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx2)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx3        = i+3*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx3)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx3)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx3)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx4        = i+4*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx4)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx4)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx4)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx5        = i+5*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx5)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx5)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx5)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx6        = i+6*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx6)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx6)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx6)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx7       = i+7*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx7)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx7)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx7)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx8        = i+8*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx8)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx8)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx8)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx9       = i+9*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx9)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx9)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx9)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx10      = i+10*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx10)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx10)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx10)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx11      = i+11*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx11)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx11)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx11)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx12      = i+12*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx12)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx12)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx12)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx13      = i+13*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx13)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx13)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx13)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx14      = i+14*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx14)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx14)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx14)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx15      = i+15*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx15)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx15)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx15)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx16      = i+16*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx16)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx16)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx16)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx17      = i+17*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx17)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx17)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx17)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx18      = i+18*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx18)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx18)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx18)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx19      = i+19*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx19)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx19)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx19)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx20      = i+20*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx20)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx20)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx20)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx21      = i+21*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx21)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx21)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx21)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx22      = i+22*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx22)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx22)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx22)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx23      = i+23*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx23)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx23)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx23)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx24      = i+24*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx24)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx24)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx24)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx25      = i+25*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx25)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx25)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx25)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx26      = i+26*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx26)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx26)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx26)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx27      = i+27*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx27)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx27)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx27)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx28      = i+28*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx28)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx28)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx28)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx29      = i+29*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx29)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx29)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx29)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx30      = i+30*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx30)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx130)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx30)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx31      = i+31*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx31)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx31)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx31)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                    end do
                end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                do j = i, n-1
                     xr      = xre(j)
                     re2     = xr*xr
                     xi      = xim(j)
                     im2     = xi*xi
                     cabs(j) = sqrt(re2+im2)
                end do
                return
           end if
end subroutine cabsv_kernel_v512_32x16_ps     
     
     
     
subroutine cabsv_kernel_v512_16x16_ps(xre,xim,cabs,n)
#if defined(__ICC) || defined(__INTEL_COMPILER)    
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cabsv_kernel_v512_16x16_ps
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: cabsv_kernel_v512_16x16_ps
#endif     
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xre
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xim
         real(kind=sp), allocatable, dimension(:), intent(out) :: cabs    
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
         real(sp),        automatic  :: re2
         real(sp),        automatic  :: im2
         integer(i4),     automatic  :: i,ii,j
         integer(i4),     automatic  :: idx1,idx2,idx3,idx4
         integer(i4),     automatic  :: idx5,idx6,idx7,idx8
         integer(i4),     automatic  :: idx9,idx10,idx11,idx12 
         integer(i4),     automatic  :: idx13,idx14,idx15
         
         if(n<=0) then
            return
         else if(n==1) then
                xr      = xre(0)
                re2     = xr*xr
                xi      = xim(0)
                im2     = xi*xi
                cabs(0) = sqrt(re2+im2)
                return
         else if(n>1 && n<=4) then
!$omp simd linear(i:1)
                 do i=0, 3
                    xmm0.v(i) = xre(i)
                    xmm1.v(i) = xmm0.v(i)*xmm0.v(i)
                    xmm2.v(i) = xim(i)
                    xmm3.v(i) = xmm2.v(i)*xmm2.v(i)
                    cabs(i)   = sqrt(xmm1.v(i)+xmm3.v(i))
                 end do                  
                 return
         else if(n>4 && n<=8) then
!$omp simd linear(i:1)
                 do i=0, 7
                    ymm0.v(i) = xre(i)
                    ymm1.v(i) = ymm0.v(i)*ymm0.v(i)
                    ymm2.v(i) = xim(i)
                    ymm3.v(i) = ymm2.v(i)*ymm2.v(i)
                    cabs(i)   = sqrt(ymm1.v(i)+ymm3.v(i))
                 end do    
                 return
         else if(n>8 && n<=16) then
!$omp simd linear(i:1)
                 do i=0, 15
                    zmm0.v(i) = xre(i)
                    zmm1.v(i) = zmm0.v(i)*zmm0.v(i)
                    zmm2.v(i) = xim(i)
                    zmm3.v(i) = zmm2.v(i)*zmm2.v(i)
                    cabs(i)   = sqrt(zmm1.v(i)+zmm3.v(i))
                 end do    
                 return    
         else if(n>16 && n<=64) then
                 do i = 0,iand(n-1,inot(15)),16
!$omp simd aligned(xim:64,xre,cabs) linear(ii:1)
                     do ii = 0, 15
                         zmm0.v(ii) = xre(i+ii)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(i+ii)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(i+ii)   = sqrt(zmm1.v(ii)+zmm3.v(ii))
                     end do
                 end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                do j = i, n-1
                     xr      = xre(j)
                     re2     = xr*xr
                     xi      = xim(j)
                     im2     = xi*xi
                     cabs(j) = sqrt(re2+im2)
                end do
                return
        else if(n>64) then
                do i=0, iand(n-1,inot(ZMM_LEN-1)), ZMM_LEN*16
                   call mm_prefetch(xre(i+16*ZMM_LEN),FOR_K_PREFETCH_T1)
                   call mm_prefetch(xim(i+16*ZMM_LEN),FOR_K_PREFETCH_T1)
#if defined(__ICC) || defined(__INTEL_COMPILER)
                   !dir$ assume_aligned  xre:64
                   !dir$ assume_aligned  xim:64
                   !dir$ assume_aligned  cabs:64
                  
#endif                   
!$omp simd aligned(xim:64,xre,cabs)  linear(ii:1)              
                    do ii = 0, ZMM_LEN-1
                         zmm0.v(ii) = xre(i+0+ii)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(i+0+ii)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(i+0+ii)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx1        = i+1*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx1)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx1)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx1)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx2        = i+2*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx2)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx2)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx2)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx3        = i+3*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx3)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx3)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx3)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx4        = i+4*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx4)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx4)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx4)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx5        = i+5*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx5)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx5)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx5)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx6        = i+6*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx6)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx6)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx6)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx7       = i+7*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx7)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx7)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx7)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx8        = i+8*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx8)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx8)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx8)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx9       = i+9*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx9)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx9)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx9)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx10      = i+10*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx10)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx10)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx10)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx11      = i+11*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx11)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx11)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx11)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx12      = i+12*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx12)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx12)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx12)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx13      = i+13*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx13)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx13)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx13)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx14      = i+14*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx14)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx14)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx14)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                         idx15      = i+15*ZMM_LEN+ii
                         zmm0.v(ii) = xre(idx15)
                         zmm1.v(ii) = zmm0.v(ii)*zmm0.v(ii)
                         zmm2.v(ii) = xim(idx15)
                         zmm3.v(ii) = zmm2.v(ii)*zmm2.v(ii)
                         cabs(idx15)= sqrt(zmm1.v(ii)+zmm3.v(ii))
                    end do
                end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                do j = i, n-1
                     xr      = xre(j)
                     re2     = xr*xr
                     xi      = xim(j)
                     im2     = xi*xi
                     cabs(j) = sqrt(re2+im2)
                end do
                return
           end if
end subroutine cabsv_kernel_v512_16x16_ps     
          
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
end module cabsv_zmm16r4
     
