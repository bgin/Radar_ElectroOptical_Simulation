
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

module cdivv_smith_zmm16r4


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name: cdivv_smith_zmm16r4
 !                         
 !          
 !          Purpose:
 !                       Complex-division (Smith method) vectorized deinterleaved, single-precision
 !                        
 !          History:
 !                        Date: 27-10-2024
 !                        Time: 06:59AM GMT+2
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
    use mod_vectypes,  only : XMM4r4_t,YMM8r4_t,ZMM16r4_t,Mask4_t,Mask8_t,Mask16_t
    use mod_fpcompare, only : Is_Greater_Than_Single
    use omp_lib
    
    public
    implicit none
    
    
      ! Major version
     integer(kind=i4),  parameter :: CDIVV_SMITH_ZMM16R4_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: CDIVV_SMITH_ZMM16R4_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: CDIVV_SMITH_ZMM16R4_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: CDIVV_SMITH_ZMM16R4_FULLVER =   &
            1000*CDIVV_SMITH_ZMM16R4_MAJOR+100*CDIVV_SMITH_ZMM16R4_MINOR+10*CDIVV_SMITH_ZMM16R4_MICRO
     ! Module creation date
     character(*),        parameter :: CDIVV_SMITH_ZMM16R4_CREATE_DATE = "27-10-2024 06:59AM +00200 (SUN 27 OCT 2024 GMT+2)"
     ! Module build date
     character(*),        parameter :: CDIVV_SMITH_ZMM16R4_BUILD_DATE  = __DATE__ " " __TIME__
     ! Module author info
     character(*),        parameter :: CDIVV_SMITH_ZMM16R4_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: CDIVV_SMITH_ZMM16R4_SYNOPSIS  = "Complex-division (Smith method) vectorized deinterleaved, single-precision." 
     
    
     integer(kind=i4), parameter, private :: ZMM_LEN = 16
     

     
     contains
     
     
subroutine cdivv_smith_v512_32x16_ps(xre,xim,yre,yim,zre,zim,n)
#if defined(__ICC) || defined(__INTEL_COMPILER)    
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cdivv_smith_v512_32x16_ps
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: cdivv_smith_v512_32x16_ps
#endif     
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xre
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xim
         real(kind=sp), allocatable, dimension(:), intent(in)  :: yre
         real(kind=sp), allocatable, dimension(:), intent(in)  :: yim
         real(kind=sp), allocatable, dimension(:), intent(out)  :: zre
         real(kind=sp), allocatable, dimension(:), intent(out)  :: zim
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
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm1
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm2
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm3
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm4
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm5
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm6
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm7
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm8
         type(Mask16_t),  automatic  :: m16_yi_gt_yr
         type(Mask8_t),   automatic  :: m8_yi_gt_yr
         type(Mask4_t),   automatic  :: m4_yi_gt_yr
         type(XMM4r4_t),  automatic  :: xmm0
         type(XMM4r4_t),  automatic  :: xmm1
         type(XMM4r4_t),  automatic  :: xmm2
         type(XMM4r4_t),  automatic  :: xmm3
         type(XMM4r4_t),  automatic  :: xmm4
         type(XMM4r4_t),  automatic  :: xmm5
         type(XMM4r4_t),  automatic  :: xmm6
         type(XMM4r4_t),  automatic  :: xmm7 
         type(YMM8r4_t),  automatic  :: ymm0 
         type(YMM8r4_t),  automatic  :: ymm1
         type(YMM8r4_t),  automatic  :: ymm2
         type(YMM8r4_t),  automatic  :: ymm3
         type(YMM8r4_t),  automatic  :: ymm4
         type(YMM8r4_t),  automatic  :: ymm5
         type(YMM8r4_t),  automatic  :: ymm6
         type(YMM8r4_t),  automatic  :: ymm7
             
         real(sp),        automatic  :: xr
         real(sp),        automatic  :: xi
         real(sp),        automatic  :: yr
         real(sp),        automatic  :: yi
         real(sp),        automatic  :: ayi
         real(sp),        automatic  :: ayr
         real(sp),        automatic  :: r
         real(sp),        automatic  :: den
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
            xr = xre(0)
            yr = yre(0)
            ayr= abs(yr)
            yi = yim(0)
            ayi= abs(yi)
            xi = xim(0)
            if(ayi.GreaterThan.ayr) then
                r      = yi/yr
                den    = yr+r*yi
                zre(0) = (xr+xi*r)/den
                zim(0) = (xi-xr*r)/den
            else
                r      = yr/yi
                den    = yi+r*yr
                zre(0) = (xr*r+xi)/den
                zim(0) = (xi*r+xr)/den
            end if 
            return
         else if(n>1 && n<=4) then
!$omp simd linear(i:1)
            do i=0, 3
               xmm0.v(i)   = xre(i) ! x_re
               xmm1.v(i)   = xim(i) ! x_im
               xmm2.v(i)   = yre(i) ! y_re, yr
               xmm3.v(i)   = yim(i) ! y_im, yi            
               xmm4.v(i)   = abs(xmm2.v(i)) ! abs(yre)
               xmm5.v(i)   = abs(xmm3.v(i)) ! abs(yim)
               m4_yi_gt_yr.m(i) = (xmm5.v(i)-xmm4.v(i)) >= SPACING(MAX(ABS(xmm5.v(i)),ABS(xmm4.v(i))))
               if(m4_yi_gt_y4.v(i)) then
                    xmm6.v(i) = xmm3.v(i)/xmm2.v(i) ! r
                    xmm7.v(i) = xmm2.v(i)+xmm6.v(i)*xmm3.v(i) ! den
                    zre(i)    = (xmm0.v(i)+xmm1.v(i)*xmm6.v(i))/xmm7.v(i)
                    zim(i)    = (xmm1.v(i)-xmm0.v(i)*xmm6.v(i))/xmm7.v(i)
               else
                    xmm6.v(i) = xmm2.v(i)/xmm3.v(i) ! r
                    xmm7.v(i) = xmm3.v(i)+xmm6.v(i)*xmm2.v(i) ! den
                    zre(i)    = (xmm0.v(i)*xmm6.v(i)+xmm1.v(i))/xmm7.v(i)
                    zim(i)    = (xmm1.v(i)*xmm6.v(i)+xmm0.v(i))/xmm7.v(i)
               end if
            end do
            return
        else if(n>4 && n<=8) then
!$omp simd linear(i:1)
            do i=0, 7
               ymm0.v(i)   = xre(i) ! x_re
               ymm1.v(i)   = xim(i) ! x_im
               ymm2.v(i)   = yre(i) ! y_re, yr
               ymm3.v(i)   = yim(i) ! y_im, yi            
               ymm4.v(i)   = abs(ymm2.v(i)) ! abs(yre)
               ymm5.v(i)   = abs(ymm3.v(i)) ! abs(yim)
               m8_yi_gt_yr.m(i) = (ymm5.v(i)-ymm4.v(i)) >= SPACING(MAX(ABS(ymm5.v(i)),ABS(ymm4.v(i))))
               if(m8_yi_gt_y4.m(i)) then
                    ymm6.v(i) = ymm3.v(i)/ymm2.v(i) ! r
                    ymm7.v(i) = ymm2.v(i)+ymm6.v(i)*ymm3.v(i) ! den
                    zre(i)    = (ymm0.v(i)+ymm1.v(i)*ymm6.v(i))/ymm7.v(i)
                    zim(i)    = (ymm1.v(i)-ymm0.v(i)*ymm6.v(i))/ymm7.v(i)
               else
                    ymm6.v(i) = ymm2.v(i)/ymm3.v(i) ! r
                    ymm7.v(i) = ymm3.v(i)+ymm6.v(i)*ymm2.v(i) ! den
                    zre(i)    = (ymm0.v(i)*ymm6.v(i)+ymm1.v(i))/ymm7.v(i)
                    zim(i)    = (ymm1.v(i)*ymm6.v(i)+ymm0.v(i))/ymm7.v(i)
               end if
            end do
            return 
         else if(n>8 && n<=16) then
!$omp simd linear(i:1)
            do i=0, 15
               zmm0.v(i)   = xre(i) ! x_re
               zmm1.v(i)   = xim(i) ! x_im
               zmm2.v(i)   = yre(i) ! y_re, yr
               zmm3.v(i)   = yim(i) ! y_im, yi            
               zmm4.v(i)   = abs(zmm2.v(i)) ! abs(yre)
               zmm5.v(i)   = abs(zmm3.v(i)) ! abs(yim)
               m16_yi_gt_yr.m(i) = (zmm5.v(i)-zmm4.v(i)) >= SPACING(MAX(ABS(zmm5.v(i)),ABS(zmm4.v(i))))
               if(m16_yi_gt_y4.m(i)) then
                    zmm6.v(i) = zmm3.v(i)/zmm2.v(i) ! r
                    zmm7.v(i) = zmm2.v(i)+zmm6.v(i)*zmm3.v(i) ! den
                    zre(i)    = (zmm0.v(i)+zmm1.v(i)*zmm6.v(i))/zmm7.v(i)
                    zim(i)    = (zmm1.v(i)-zmm0.v(i)*zmm6.v(i))/zmm7.v(i)
               else
                    zmm6.v(i) = zmm2.v(i)/zmm3.v(i) ! r
                    zmm7.v(i) = zmm3.v(i)+zmm6.v(i)*zmm2.v(i) ! den
                    zre(i)    = (zmm0.v(i)*zmm6.v(i)+zmm1.v(i))/zmm7.v(i)
                    zim(i)    = (zmm1.v(i)*zmm6.v(i)+zmm0.v(i))/zmm7.v(i)
               end if
            end do
            return   
         else if(n>16 && n<=64) then
             do i = 0,iand(n-1,inot(15)),16
!$omp simd aligned(xim:64,xre,yre,yim,zre,zim) linear(ii:1)
               do ii = 0, 15
                    zmm0.v(ii)        = xre(i+ii) ! x_re
                    zmm1.v(ii)        = xim(i+ii) ! x_im
                    zmm2.v(ii)        = yre(i+ii) ! y_re, yr
                    zmm3.v(ii)        = yim(i+ii) ! y_im, yi            
                    zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                    zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                    m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                    if(m16_yi_gt_y4.m(ii)) then
                         zmm6.v(ii)   = zmm3.v(ii)/zmm2.v(ii) ! r
                         zmm7.v(ii)   = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                         zre(i+ii)    = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                         zim(i+ii)    = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                    else
                         zmm6.v(ii)   = zmm2.v(ii)/zmm3.v(ii) ! r
                         zmm7.v(ii)   = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                         zre(i+ii)    = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))/zmm7.v(ii)
                         zim(i+ii)    = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))/zmm7.v(ii)
                    end if
               end do
            end do 
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
           do j = i, n-1
               xr = xre(j)
               yr = yre(j)
               ayr= abs(yr)
               yi = yim(j)
               ayi= abs(yi)
               xi = xim(j)
               if(ayi.GreaterThan.ayr) then
                  r      = yi/yr
                  den    = yr+r*yi
                  zre(j) = (xr+xi*r)/den
                  zim(j) = (xi-xr*r)/den
               else
                  r      = yr/yi
                  den    = yi+r*yr
                  zre(j) = (xr*r+xi)/den
                  zim(j) = (xi*r+xr)/den
               end if 
           end do
           return
        else if(n>64 && n<=128) then
            do i = 0,iand(n-1,inot(15)),16
!$omp simd aligned(xim:64,xre,yre,yim,zre,zim) linear(ii:1)
               do ii = 0, 15
                    zmm0.v(ii)        = xre(i+ii) ! x_re
                    zmm1.v(ii)        = xim(i+ii) ! x_im
                    zmm2.v(ii)        = yre(i+ii) ! y_re, yr
                    zmm3.v(ii)        = yim(i+ii) ! y_im, yi            
                    zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                    zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                    m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                    if(m16_yi_gt_y4.m(ii)) then
                         zmm6.v(ii)   = zmm3.v(ii)/zmm2.v(ii) ! r
                         zmm7.v(ii)   = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                         zre(i+ii)    = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                         zim(i+ii)    = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                    else
                         zmm6.v(ii)   = zmm2.v(ii)/zmm3.v(ii) ! r
                         zmm7.v(ii)   = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                         zre(i+ii)    = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))/zmm7.v(ii)
                         zim(i+ii)    = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))/zmm7.v(ii)
                    end if
               end do
            end do 
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
           do j = i, n-1
               xr = xre(j)
               yr = yre(j)
               ayr= abs(yr)
               yi = yim(j)
               ayi= abs(yi)
               xi = xim(j)
               if(ayi.GreaterThan.ayr) then
                  r      = yi/yr
                  den    = yr+r*yi
                  zre(j) = (xr+xi*r)/den
                  zim(j) = (xi-xr*r)/den
               else
                  r      = yr/yi
                  den    = yi+r*yr
                  zre(j) = (xr*r+xi)/den
                  zim(j) = (xi*r+xr)/den
               end if 
           end do
           return
       else if(n>128) then
           do i=0, iand(n-1,inot(ZMM_LEN-1)), ZMM_LEN*32
                   call mm_prefetch(xre(i+32*ZMM_LEN),FOR_K_PREFETCH_T1)
                   call mm_prefetch(xim(i+32*ZMM_LEN),FOR_K_PREFETCH_T1)
                   call mm_prefetch(yre(i+32*ZMM_LEN),FOR_K_PREFETCH_T1)
                   call mm_prefetch(yim(i+32*ZMM_LEN),FOR_K_PREFETCH_T1)
#if defined(__ICC) || defined(__INTEL_COMPILER)
                   !dir$ assume_aligned  xre:64
                   !dir$ assume_aligned  xim:64
                   !dir$ assume_aligned  yre:64
                   !dir$ assume_aligned  yim:64
                   !dir$ assume_aligned  zre:64
                   !dir$ assume_aligned  zim:64
#endif                   
!$omp simd aligned(xim:64,xre,yre,yim,zre,zim)  linear(ii:1)              
                  do ii = 0, ZMM_LEN-1
                       zmm0.v(ii)        = xre(i+0+ii) ! x_re
                       zmm1.v(ii)        = xim(i+0+ii) ! x_im
                       zmm2.v(ii)        = yre(i+0+ii) ! y_re, yr
                       zmm3.v(ii)        = yim(i+0+ii) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(i+0+ii)    = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(i+0+ii)    = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(i+0+ii)    = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(i+0+ii)    = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx1              = i+1*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx1) ! x_re
                       zmm1.v(ii)        = xim(idx1) ! x_im
                       zmm2.v(ii)        = yre(idx1) ! y_re, yr
                       zmm3.v(ii)        = yim(idx1) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx1)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx1)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx1)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx1)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx2              = i+2*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx2) ! x_re
                       zmm1.v(ii)        = xim(idx2) ! x_im
                       zmm2.v(ii)        = yre(idx2) ! y_re, yr
                       zmm3.v(ii)        = yim(idx2) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx2)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx2)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx2)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx2)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx3               = i+3*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx3) ! x_re
                       zmm1.v(ii)        = xim(idx3) ! x_im
                       zmm2.v(ii)        = yre(idx3) ! y_re, yr
                       zmm3.v(ii)        = yim(idx3) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx3)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx3)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx3)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx3)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx4               = i+4*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx4) ! x_re
                       zmm1.v(ii)        = xim(idx4) ! x_im
                       zmm2.v(ii)        = yre(idx4) ! y_re, yr
                       zmm3.v(ii)        = yim(idx4) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx4)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx4)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx4)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx4)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx5              = i+5*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx5) ! x_re
                       zmm1.v(ii)        = xim(idx5) ! x_im
                       zmm2.v(ii)        = yre(idx5) ! y_re, yr
                       zmm3.v(ii)        = yim(idx5) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx5)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx5)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx5)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx5)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx6              = i+6*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx6) ! x_re
                       zmm1.v(ii)        = xim(idx6) ! x_im
                       zmm2.v(ii)        = yre(idx6) ! y_re, yr
                       zmm3.v(ii)        = yim(idx6) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx6)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx6)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx6)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx6)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx7              = i+7*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx7) ! x_re
                       zmm1.v(ii)        = xim(idx7) ! x_im
                       zmm2.v(ii)        = yre(idx7) ! y_re, yr
                       zmm3.v(ii)        = yim(idx7) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx7)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx7)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx7)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx7)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx8             = i+8*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx8) ! x_re
                       zmm1.v(ii)        = xim(idx8) ! x_im
                       zmm2.v(ii)        = yre(idx8) ! y_re, yr
                       zmm3.v(ii)        = yim(idx8) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx8)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx8)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx8)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx8)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx9              = i+9*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx9) ! x_re
                       zmm1.v(ii)        = xim(idx9) ! x_im
                       zmm2.v(ii)        = yre(idx9) ! y_re, yr
                       zmm3.v(ii)        = yim(idx9) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx9)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx9)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx9)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx9)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx10             = i+10*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx10) ! x_re
                       zmm1.v(ii)        = xim(idx10) ! x_im
                       zmm2.v(ii)        = yre(idx10) ! y_re, yr
                       zmm3.v(ii)        = yim(idx10) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx10)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx10)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx10)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx10)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx11             = i+11*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx11) ! x_re
                       zmm1.v(ii)        = xim(idx11) ! x_im
                       zmm2.v(ii)        = yre(idx11) ! y_re, yr
                       zmm3.v(ii)        = yim(idx11) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx11)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx11)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx11)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx11)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx12             = i+12*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx12) ! x_re
                       zmm1.v(ii)        = xim(idx12) ! x_im
                       zmm2.v(ii)        = yre(idx12) ! y_re, yr
                       zmm3.v(ii)        = yim(idx12) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx12)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx12)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx12)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx12)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx13             = i+13*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx13) ! x_re
                       zmm1.v(ii)        = xim(idx13) ! x_im
                       zmm2.v(ii)        = yre(idx13) ! y_re, yr
                       zmm3.v(ii)        = yim(idx13) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx13)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx13)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx13)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx13)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx14             = i+14*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx14) ! x_re
                       zmm1.v(ii)        = xim(idx14) ! x_im
                       zmm2.v(ii)        = yre(idx14) ! y_re, yr
                       zmm3.v(ii)        = yim(idx14) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx14)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx14)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx14)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx14)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx15             = i+15*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx15) ! x_re
                       zmm1.v(ii)        = xim(idx15) ! x_im
                       zmm2.v(ii)        = yre(idx15) ! y_re, yr
                       zmm3.v(ii)        = yim(idx15) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx15)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx15)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx15)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx15)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx16             = i+16*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx16) ! x_re
                       zmm1.v(ii)        = xim(idx16) ! x_im
                       zmm2.v(ii)        = yre(idx16) ! y_re, yr
                       zmm3.v(ii)        = yim(idx16) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx16)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx16)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx16)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx16)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx17             = i+17*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx17) ! x_re
                       zmm1.v(ii)        = xim(idx17) ! x_im
                       zmm2.v(ii)        = yre(idx17) ! y_re, yr
                       zmm3.v(ii)        = yim(idx17) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx17)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx17)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx17)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx17)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx18             = i+18*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx18) ! x_re
                       zmm1.v(ii)        = xim(idx18) ! x_im
                       zmm2.v(ii)        = yre(idx18) ! y_re, yr
                       zmm3.v(ii)        = yim(idx18) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx18)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx18)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx18)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx18)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx19             = i+19*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx19) ! x_re
                       zmm1.v(ii)        = xim(idx19) ! x_im
                       zmm2.v(ii)        = yre(idx19) ! y_re, yr
                       zmm3.v(ii)        = yim(idx19) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx19)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx19)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx19)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx19)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx20             = i+20*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx20) ! x_re
                       zmm1.v(ii)        = xim(idx20) ! x_im
                       zmm2.v(ii)        = yre(idx20) ! y_re, yr
                       zmm3.v(ii)        = yim(idx20) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx20)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx20)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx20)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx20)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx21             = i+21*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx21) ! x_re
                       zmm1.v(ii)        = xim(idx21) ! x_im
                       zmm2.v(ii)        = yre(idx21) ! y_re, yr
                       zmm3.v(ii)        = yim(idx21) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx21)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx21)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx21)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx21)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx22             = i+22*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx22) ! x_re
                       zmm1.v(ii)        = xim(idx22) ! x_im
                       zmm2.v(ii)        = yre(idx22) ! y_re, yr
                       zmm3.v(ii)        = yim(idx22) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx22)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx22)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx22)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx22)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx23             = i+23*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx23) ! x_re
                       zmm1.v(ii)        = xim(idx23) ! x_im
                       zmm2.v(ii)        = yre(idx23) ! y_re, yr
                       zmm3.v(ii)        = yim(idx23) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx23)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx23)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx23)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx23)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx24             = i+24*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx24) ! x_re
                       zmm1.v(ii)        = xim(idx24) ! x_im
                       zmm2.v(ii)        = yre(idx24) ! y_re, yr
                       zmm3.v(ii)        = yim(idx24) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx24)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx24)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx24)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx24)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx25             = i+25*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx25) ! x_re
                       zmm1.v(ii)        = xim(idx25) ! x_im
                       zmm2.v(ii)        = yre(idx25) ! y_re, yr
                       zmm3.v(ii)        = yim(idx25) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx25)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx25)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx25)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx25)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx26             = i+26*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx26) ! x_re
                       zmm1.v(ii)        = xim(idx26) ! x_im
                       zmm2.v(ii)        = yre(idx26) ! y_re, yr
                       zmm3.v(ii)        = yim(idx26) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx26)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx26)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx26)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx26)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx27             = i+27*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx27) ! x_re
                       zmm1.v(ii)        = xim(idx27) ! x_im
                       zmm2.v(ii)        = yre(idx27) ! y_re, yr
                       zmm3.v(ii)        = yim(idx27) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx27)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx27)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx27)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx27)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx28             = i+28*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx28) ! x_re
                       zmm1.v(ii)        = xim(idx28) ! x_im
                       zmm2.v(ii)        = yre(idx28) ! y_re, yr
                       zmm3.v(ii)        = yim(idx28) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx28)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx28)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx28)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx28)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx29             = i+29*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx29) ! x_re
                       zmm1.v(ii)        = xim(idx29) ! x_im
                       zmm2.v(ii)        = yre(idx29) ! y_re, yr
                       zmm3.v(ii)        = yim(idx29) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx29)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx29)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx29)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx29)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx30             = i+30*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx30) ! x_re
                       zmm1.v(ii)        = xim(idx30) ! x_im
                       zmm2.v(ii)        = yre(idx30) ! y_re, yr
                       zmm3.v(ii)        = yim(idx30) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx30)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx30)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx30)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx30)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx31             = i+31*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx31) ! x_re
                       zmm1.v(ii)        = xim(idx31) ! x_im
                       zmm2.v(ii)        = yre(idx31) ! y_re, yr
                       zmm3.v(ii)        = yim(idx31) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx31)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx31)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx31)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx31)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                  end do
          end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
           do j = i, n-1
               xr = xre(j)
               yr = yre(j)
               ayr= abs(yr)
               yi = yim(j)
               ayi= abs(yi)
               xi = xim(j)
               if(ayi.GreaterThan.ayr) then
                  r      = yi/yr
                  den    = yr+r*yi
                  zre(j) = (xr+xi*r)/den
                  zim(j) = (xi-xr*r)/den
               else
                  r      = yr/yi
                  den    = yi+r*yr
                  zre(j) = (xr*r+xi)/den
                  zim(j) = (xi*r+xr)/den
               end if 
           end do
           return
    end if               
end subroutine  cdivv_smith_v512_32x16_ps    


subroutine cdivv_smith_v512_16x16_ps(xre,xim,yre,yim,zre,zim,n)
#if defined(__ICC) || defined(__INTEL_COMPILER)    
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cdivv_smith_v512_16x16_ps
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: cdivv_smith_v512_16x16_ps
#endif     
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xre
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xim
         real(kind=sp), allocatable, dimension(:), intent(in)  :: yre
         real(kind=sp), allocatable, dimension(:), intent(in)  :: yim
         real(kind=sp), allocatable, dimension(:), intent(out)  :: zre
         real(kind=sp), allocatable, dimension(:), intent(out)  :: zim
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
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm1
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm2
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm3
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm4
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm5
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm6
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm7
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm8
         type(Mask16_t),  automatic  :: m16_yi_gt_yr
         type(Mask8_t),   automatic  :: m8_yi_gt_yr
         type(Mask4_t),   automatic  :: m4_yi_gt_yr
         type(XMM4r4_t),  automatic  :: xmm0
         type(XMM4r4_t),  automatic  :: xmm1
         type(XMM4r4_t),  automatic  :: xmm2
         type(XMM4r4_t),  automatic  :: xmm3
         type(XMM4r4_t),  automatic  :: xmm4
         type(XMM4r4_t),  automatic  :: xmm5
         type(XMM4r4_t),  automatic  :: xmm6
         type(XMM4r4_t),  automatic  :: xmm7 
         type(YMM8r4_t),  automatic  :: ymm0 
         type(YMM8r4_t),  automatic  :: ymm1
         type(YMM8r4_t),  automatic  :: ymm2
         type(YMM8r4_t),  automatic  :: ymm3
         type(YMM8r4_t),  automatic  :: ymm4
         type(YMM8r4_t),  automatic  :: ymm5
         type(YMM8r4_t),  automatic  :: ymm6
         type(YMM8r4_t),  automatic  :: ymm7
             
         real(sp),        automatic  :: xr
         real(sp),        automatic  :: xi
         real(sp),        automatic  :: yr
         real(sp),        automatic  :: yi
         real(sp),        automatic  :: ayi
         real(sp),        automatic  :: ayr
         real(sp),        automatic  :: r
         real(sp),        automatic  :: den
         integer(i4),     automatic  :: i,ii,j
         integer(i4),     automatic  :: idx1,idx2,idx3,idx4
         integer(i4),     automatic  :: idx5,idx6,idx7,idx8
         integer(i4),     automatic  :: idx9,idx10,idx11,idx12 
         integer(i4),     automatic  :: idx13,idx14,idx15
         
         if(n<=0) then
            return
         else if(n==1) then
            xr = xre(0)
            yr = yre(0)
            ayr= abs(yr)
            yi = yim(0)
            ayi= abs(yi)
            xi = xim(0)
            if(ayi.GreaterThan.ayr) then
                r      = yi/yr
                den    = yr+r*yi
                zre(0) = (xr+xi*r)/den
                zim(0) = (xi-xr*r)/den
            else
                r      = yr/yi
                den    = yi+r*yr
                zre(0) = (xr*r+xi)/den
                zim(0) = (xi*r+xr)/den
            end if 
            return
         else if(n>1 && n<=4) then
!$omp simd linear(i:1)
            do i=0, 3
               xmm0.v(i)   = xre(i) ! x_re
               xmm1.v(i)   = xim(i) ! x_im
               xmm2.v(i)   = yre(i) ! y_re, yr
               xmm3.v(i)   = yim(i) ! y_im, yi            
               xmm4.v(i)   = abs(xmm2.v(i)) ! abs(yre)
               xmm5.v(i)   = abs(xmm3.v(i)) ! abs(yim)
               m4_yi_gt_yr.m(i) = (xmm5.v(i)-xmm4.v(i)) >= SPACING(MAX(ABS(xmm5.v(i)),ABS(xmm4.v(i))))
               if(m4_yi_gt_y4.v(i)) then
                    xmm6.v(i) = xmm3.v(i)/xmm2.v(i) ! r
                    xmm7.v(i) = xmm2.v(i)+xmm6.v(i)*xmm3.v(i) ! den
                    zre(i)    = (xmm0.v(i)+xmm1.v(i)*xmm6.v(i))/xmm7.v(i)
                    zim(i)    = (xmm1.v(i)-xmm0.v(i)*xmm6.v(i))/xmm7.v(i)
               else
                    xmm6.v(i) = xmm2.v(i)/xmm3.v(i) ! r
                    xmm7.v(i) = xmm3.v(i)+xmm6.v(i)*xmm2.v(i) ! den
                    zre(i)    = (xmm0.v(i)*xmm6.v(i)+xmm1.v(i))/xmm7.v(i)
                    zim(i)    = (xmm1.v(i)*xmm6.v(i)+xmm0.v(i))/xmm7.v(i)
               end if
            end do
            return
        else if(n>4 && n<=8) then
!$omp simd linear(i:1)
            do i=0, 7
               ymm0.v(i)   = xre(i) ! x_re
               ymm1.v(i)   = xim(i) ! x_im
               ymm2.v(i)   = yre(i) ! y_re, yr
               ymm3.v(i)   = yim(i) ! y_im, yi            
               ymm4.v(i)   = abs(ymm2.v(i)) ! abs(yre)
               ymm5.v(i)   = abs(ymm3.v(i)) ! abs(yim)
               m8_yi_gt_yr.m(i) = (ymm5.v(i)-ymm4.v(i)) >= SPACING(MAX(ABS(ymm5.v(i)),ABS(ymm4.v(i))))
               if(m8_yi_gt_y4.m(i)) then
                    ymm6.v(i) = ymm3.v(i)/ymm2.v(i) ! r
                    ymm7.v(i) = ymm2.v(i)+ymm6.v(i)*ymm3.v(i) ! den
                    zre(i)    = (ymm0.v(i)+ymm1.v(i)*ymm6.v(i))/ymm7.v(i)
                    zim(i)    = (ymm1.v(i)-ymm0.v(i)*ymm6.v(i))/ymm7.v(i)
               else
                    ymm6.v(i) = ymm2.v(i)/ymm3.v(i) ! r
                    ymm7.v(i) = ymm3.v(i)+ymm6.v(i)*ymm2.v(i) ! den
                    zre(i)    = (ymm0.v(i)*ymm6.v(i)+ymm1.v(i))/ymm7.v(i)
                    zim(i)    = (ymm1.v(i)*ymm6.v(i)+ymm0.v(i))/ymm7.v(i)
               end if
            end do
            return 
         else if(n>8 && n<=16) then
!$omp simd linear(i:1)
            do i=0, 15
               zmm0.v(i)   = xre(i) ! x_re
               zmm1.v(i)   = xim(i) ! x_im
               zmm2.v(i)   = yre(i) ! y_re, yr
               zmm3.v(i)   = yim(i) ! y_im, yi            
               zmm4.v(i)   = abs(zmm2.v(i)) ! abs(yre)
               zmm5.v(i)   = abs(zmm3.v(i)) ! abs(yim)
               m16_yi_gt_yr.m(i) = (zmm5.v(i)-zmm4.v(i)) >= SPACING(MAX(ABS(zmm5.v(i)),ABS(zmm4.v(i))))
               if(m16_yi_gt_y4.m(i)) then
                    zmm6.v(i) = zmm3.v(i)/zmm2.v(i) ! r
                    zmm7.v(i) = zmm2.v(i)+zmm6.v(i)*zmm3.v(i) ! den
                    zre(i)    = (zmm0.v(i)+zmm1.v(i)*zmm6.v(i))/zmm7.v(i)
                    zim(i)    = (zmm1.v(i)-zmm0.v(i)*zmm6.v(i))/zmm7.v(i)
               else
                    zmm6.v(i) = zmm2.v(i)/zmm3.v(i) ! r
                    zmm7.v(i) = zmm3.v(i)+zmm6.v(i)*zmm2.v(i) ! den
                    zre(i)    = (zmm0.v(i)*zmm6.v(i)+zmm1.v(i))/zmm7.v(i)
                    zim(i)    = (zmm1.v(i)*zmm6.v(i)+zmm0.v(i))/zmm7.v(i)
               end if
            end do
            return   
         else if(n>16 && n<=64) then
             do i = 0,iand(n-1,inot(15)),16
!$omp simd aligned(xim:64,xre,yre,yim) linear(ii:1)
               do ii = 0, 15
                    zmm0.v(ii)        = xre(i+ii) ! x_re
                    zmm1.v(ii)        = xim(i+ii) ! x_im
                    zmm2.v(ii)        = yre(i+ii) ! y_re, yr
                    zmm3.v(ii)        = yim(i+ii) ! y_im, yi            
                    zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                    zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                    m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                    if(m16_yi_gt_y4.m(ii)) then
                         zmm6.v(ii)   = zmm3.v(ii)/zmm2.v(ii) ! r
                         zmm7.v(ii)   = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                         zre(i+ii)    = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                         zim(i+ii)    = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                    else
                         zmm6.v(ii)   = zmm2.v(ii)/zmm3.v(ii) ! r
                         zmm7.v(ii)   = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                         zre(i+ii)    = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))/zmm7.v(ii)
                         zim(i+ii)    = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))/zmm7.v(ii)
                    end if
               end do
            end do 
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
           do j = i, n-1
               xr = xre(j)
               yr = yre(j)
               ayr= abs(yr)
               yi = yim(j)
               ayi= abs(yi)
               xi = xim(j)
               if(ayi.GreaterThan.ayr) then
                  r      = yi/yr
                  den    = yr+r*yi
                  zre(j) = (xr+xi*r)/den
                  zim(j) = (xi-xr*r)/den
               else
                  r      = yr/yi
                  den    = yi+r*yr
                  zre(j) = (xr*r+xi)/den
                  zim(j) = (xi*r+xr)/den
               end if 
           end do
           return
       else if(n>64) then
           do i=0, iand(n-1,inot(ZMM_LEN-1)), ZMM_LEN*16
                   call mm_prefetch(xre(i+16*ZMM_LEN),FOR_K_PREFETCH_T1)
                   call mm_prefetch(xim(i+16*ZMM_LEN),FOR_K_PREFETCH_T1)
                   call mm_prefetch(yre(i+16*ZMM_LEN),FOR_K_PREFETCH_T1)
                   call mm_prefetch(yim(i+16*ZMM_LEN),FOR_K_PREFETCH_T1)
#if defined(__ICC) || defined(__INTEL_COMPILER)
                   !dir$ assume_aligned  xre:64
                   !dir$ assume_aligned  xim:64
                   !dir$ assume_aligned  yre:64
                   !dir$ assume_aligned  yim:64
                   !dir$ assume_aligned  zre:64
                   !dir$ assume_aligned  zim:64
#endif                   
!$omp simd aligned(xim:64,xre,yre,yim)  linear(ii:1)              
                  do ii = 0, ZMM_LEN-1
                       zmm0.v(ii)        = xre(i+0+ii) ! x_re
                       zmm1.v(ii)        = xim(i+0+ii) ! x_im
                       zmm2.v(ii)        = yre(i+0+ii) ! y_re, yr
                       zmm3.v(ii)        = yim(i+0+ii) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(i+0+ii)    = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(i+0+ii)    = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(i+0+ii)    = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(i+0+ii)    = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx1              = i+1*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx1) ! x_re
                       zmm1.v(ii)        = xim(idx1) ! x_im
                       zmm2.v(ii)        = yre(idx1) ! y_re, yr
                       zmm3.v(ii)        = yim(idx1) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx1)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx1)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx1)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx1)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx2              = i+2*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx2) ! x_re
                       zmm1.v(ii)        = xim(idx2) ! x_im
                       zmm2.v(ii)        = yre(idx2) ! y_re, yr
                       zmm3.v(ii)        = yim(idx2) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx2)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx2)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx2)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx2)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx3               = i+3*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx3) ! x_re
                       zmm1.v(ii)        = xim(idx3) ! x_im
                       zmm2.v(ii)        = yre(idx3) ! y_re, yr
                       zmm3.v(ii)        = yim(idx3) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx3)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx3)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx3)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx3)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx4               = i+4*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx4) ! x_re
                       zmm1.v(ii)        = xim(idx4) ! x_im
                       zmm2.v(ii)        = yre(idx4) ! y_re, yr
                       zmm3.v(ii)        = yim(idx4) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx4)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx4)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx4)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx4)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx5              = i+5*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx5) ! x_re
                       zmm1.v(ii)        = xim(idx5) ! x_im
                       zmm2.v(ii)        = yre(idx5) ! y_re, yr
                       zmm3.v(ii)        = yim(idx5) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx5)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx5)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx5)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx5)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx6              = i+6*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx6) ! x_re
                       zmm1.v(ii)        = xim(idx6) ! x_im
                       zmm2.v(ii)        = yre(idx6) ! y_re, yr
                       zmm3.v(ii)        = yim(idx6) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx6)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx6)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx6)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx6)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx7              = i+7*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx7) ! x_re
                       zmm1.v(ii)        = xim(idx7) ! x_im
                       zmm2.v(ii)        = yre(idx7) ! y_re, yr
                       zmm3.v(ii)        = yim(idx7) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx7)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx7)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx7)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx7)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx8             = i+8*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx8) ! x_re
                       zmm1.v(ii)        = xim(idx8) ! x_im
                       zmm2.v(ii)        = yre(idx8) ! y_re, yr
                       zmm3.v(ii)        = yim(idx8) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx8)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx8)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx8)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx8)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx9              = i+9*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx9) ! x_re
                       zmm1.v(ii)        = xim(idx9) ! x_im
                       zmm2.v(ii)        = yre(idx9) ! y_re, yr
                       zmm3.v(ii)        = yim(idx9) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx9)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx9)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx9)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx9)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx10             = i+10*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx10) ! x_re
                       zmm1.v(ii)        = xim(idx10) ! x_im
                       zmm2.v(ii)        = yre(idx10) ! y_re, yr
                       zmm3.v(ii)        = yim(idx10) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx10)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx10)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx10)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx10)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx11             = i+11*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx11) ! x_re
                       zmm1.v(ii)        = xim(idx11) ! x_im
                       zmm2.v(ii)        = yre(idx11) ! y_re, yr
                       zmm3.v(ii)        = yim(idx11) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx11)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx11)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx11)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx11)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx12             = i+12*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx12) ! x_re
                       zmm1.v(ii)        = xim(idx12) ! x_im
                       zmm2.v(ii)        = yre(idx12) ! y_re, yr
                       zmm3.v(ii)        = yim(idx12) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx12)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx12)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx12)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx12)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx13             = i+13*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx13) ! x_re
                       zmm1.v(ii)        = xim(idx13) ! x_im
                       zmm2.v(ii)        = yre(idx13) ! y_re, yr
                       zmm3.v(ii)        = yim(idx13) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx13)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx13)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx13)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx13)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx14             = i+14*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx14) ! x_re
                       zmm1.v(ii)        = xim(idx14) ! x_im
                       zmm2.v(ii)        = yre(idx14) ! y_re, yr
                       zmm3.v(ii)        = yim(idx14) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx14)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx14)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx14)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx14)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx15             = i+15*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx15) ! x_re
                       zmm1.v(ii)        = xim(idx15) ! x_im
                       zmm2.v(ii)        = yre(idx15) ! y_re, yr
                       zmm3.v(ii)        = yim(idx15) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx15)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx15)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx15)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx15)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                  end do
              end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
           do j = i, n-1
               xr = xre(j)
               yr = yre(j)
               ayr= abs(yr)
               yi = yim(j)
               ayi= abs(yi)
               xi = xim(j)
               if(ayi.GreaterThan.ayr) then
                  r      = yi/yr
                  den    = yr+r*yi
                  zre(j) = (xr+xi*r)/den
                  zim(j) = (xi-xr*r)/den
               else
                  r      = yr/yi
                  den    = yi+r*yr
                  zre(j) = (xr*r+xi)/den
                  zim(j) = (xi*r+xr)/den
               end if 
           end do
           return
    end if               
end subroutine  cdivv_smith_v512_16x16_ps     


subroutine cdivv_smith_v512_8x16_ps(xre,xim,yre,yim,zre,zim,n)
#if defined(__ICC) || defined(__INTEL_COMPILER)    
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cdivv_smith_v512_8x16_ps
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: cdivv_smith_v512_8x16_ps
#endif     
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xre
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xim
         real(kind=sp), allocatable, dimension(:), intent(in)  :: yre
         real(kind=sp), allocatable, dimension(:), intent(in)  :: yim
         real(kind=sp), allocatable, dimension(:), intent(out)  :: zre
         real(kind=sp), allocatable, dimension(:), intent(out)  :: zim
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
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm1
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm2
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm3
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm4
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm5
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm6
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm7
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm8
         type(Mask16_t),  automatic  :: m16_yi_gt_yr
         type(Mask8_t),   automatic  :: m8_yi_gt_yr
         type(Mask4_t),   automatic  :: m4_yi_gt_yr
         type(XMM4r4_t),  automatic  :: xmm0
         type(XMM4r4_t),  automatic  :: xmm1
         type(XMM4r4_t),  automatic  :: xmm2
         type(XMM4r4_t),  automatic  :: xmm3
         type(XMM4r4_t),  automatic  :: xmm4
         type(XMM4r4_t),  automatic  :: xmm5
         type(XMM4r4_t),  automatic  :: xmm6
         type(XMM4r4_t),  automatic  :: xmm7 
         type(YMM8r4_t),  automatic  :: ymm0 
         type(YMM8r4_t),  automatic  :: ymm1
         type(YMM8r4_t),  automatic  :: ymm2
         type(YMM8r4_t),  automatic  :: ymm3
         type(YMM8r4_t),  automatic  :: ymm4
         type(YMM8r4_t),  automatic  :: ymm5
         type(YMM8r4_t),  automatic  :: ymm6
         type(YMM8r4_t),  automatic  :: ymm7
             
         real(sp),        automatic  :: xr
         real(sp),        automatic  :: xi
         real(sp),        automatic  :: yr
         real(sp),        automatic  :: yi
         real(sp),        automatic  :: ayi
         real(sp),        automatic  :: ayr
         real(sp),        automatic  :: r
         real(sp),        automatic  :: den
         integer(i4),     automatic  :: i,ii,j
         integer(i4),     automatic  :: idx1,idx2,idx3,idx4
         integer(i4),     automatic  :: idx5,idx6,idx7
         
         if(n<=0) then
            return
         else if(n==1) then
            xr = xre(0)
            yr = yre(0)
            ayr= abs(yr)
            yi = yim(0)
            ayi= abs(yi)
            xi = xim(0)
            if(ayi.GreaterThan.ayr) then
                r      = yi/yr
                den    = yr+r*yi
                zre(0) = (xr+xi*r)/den
                zim(0) = (xi-xr*r)/den
            else
                r      = yr/yi
                den    = yi+r*yr
                zre(0) = (xr*r+xi)/den
                zim(0) = (xi*r+xr)/den
            end if 
            return
         else if(n>1 && n<=4) then
!$omp simd linear(i:1)
            do i=0, 3
               xmm0.v(i)   = xre(i) ! x_re
               xmm1.v(i)   = xim(i) ! x_im
               xmm2.v(i)   = yre(i) ! y_re, yr
               xmm3.v(i)   = yim(i) ! y_im, yi            
               xmm4.v(i)   = abs(xmm2.v(i)) ! abs(yre)
               xmm5.v(i)   = abs(xmm3.v(i)) ! abs(yim)
               m4_yi_gt_yr.m(i) = (xmm5.v(i)-xmm4.v(i)) >= SPACING(MAX(ABS(xmm5.v(i)),ABS(xmm4.v(i))))
               if(m4_yi_gt_y4.v(i)) then
                    xmm6.v(i) = xmm3.v(i)/xmm2.v(i) ! r
                    xmm7.v(i) = xmm2.v(i)+xmm6.v(i)*xmm3.v(i) ! den
                    zre(i)    = (xmm0.v(i)+xmm1.v(i)*xmm6.v(i))/xmm7.v(i)
                    zim(i)    = (xmm1.v(i)-xmm0.v(i)*xmm6.v(i))/xmm7.v(i)
               else
                    xmm6.v(i) = xmm2.v(i)/xmm3.v(i) ! r
                    xmm7.v(i) = xmm3.v(i)+xmm6.v(i)*xmm2.v(i) ! den
                    zre(i)    = (xmm0.v(i)*xmm6.v(i)+xmm1.v(i))/xmm7.v(i)
                    zim(i)    = (xmm1.v(i)*xmm6.v(i)+xmm0.v(i))/xmm7.v(i)
               end if
            end do
            return
        else if(n>4 && n<=8) then
!$omp simd linear(i:1)
            do i=0, 7
               ymm0.v(i)   = xre(i) ! x_re
               ymm1.v(i)   = xim(i) ! x_im
               ymm2.v(i)   = yre(i) ! y_re, yr
               ymm3.v(i)   = yim(i) ! y_im, yi            
               ymm4.v(i)   = abs(ymm2.v(i)) ! abs(yre)
               ymm5.v(i)   = abs(ymm3.v(i)) ! abs(yim)
               m8_yi_gt_yr.m(i) = (ymm5.v(i)-ymm4.v(i)) >= SPACING(MAX(ABS(ymm5.v(i)),ABS(ymm4.v(i))))
               if(m8_yi_gt_y4.m(i)) then
                    ymm6.v(i) = ymm3.v(i)/ymm2.v(i) ! r
                    ymm7.v(i) = ymm2.v(i)+ymm6.v(i)*ymm3.v(i) ! den
                    zre(i)    = (ymm0.v(i)+ymm1.v(i)*ymm6.v(i))/ymm7.v(i)
                    zim(i)    = (ymm1.v(i)-ymm0.v(i)*ymm6.v(i))/ymm7.v(i)
               else
                    ymm6.v(i) = ymm2.v(i)/ymm3.v(i) ! r
                    ymm7.v(i) = ymm3.v(i)+ymm6.v(i)*ymm2.v(i) ! den
                    zre(i)    = (ymm0.v(i)*ymm6.v(i)+ymm1.v(i))/ymm7.v(i)
                    zim(i)    = (ymm1.v(i)*ymm6.v(i)+ymm0.v(i))/ymm7.v(i)
               end if
            end do
            return 
         else if(n>8 && n<=16) then
!$omp simd linear(i:1)
            do i=0, 15
               zmm0.v(i)   = xre(i) ! x_re
               zmm1.v(i)   = xim(i) ! x_im
               zmm2.v(i)   = yre(i) ! y_re, yr
               zmm3.v(i)   = yim(i) ! y_im, yi            
               zmm4.v(i)   = abs(zmm2.v(i)) ! abs(yre)
               zmm5.v(i)   = abs(zmm3.v(i)) ! abs(yim)
               m16_yi_gt_yr.m(i) = (zmm5.v(i)-zmm4.v(i)) >= SPACING(MAX(ABS(zmm5.v(i)),ABS(zmm4.v(i))))
               if(m16_yi_gt_y4.m(i)) then
                    zmm6.v(i) = zmm3.v(i)/zmm2.v(i) ! r
                    zmm7.v(i) = zmm2.v(i)+zmm6.v(i)*zmm3.v(i) ! den
                    zre(i)    = (zmm0.v(i)+zmm1.v(i)*zmm6.v(i))/zmm7.v(i)
                    zim(i)    = (zmm1.v(i)-zmm0.v(i)*zmm6.v(i))/zmm7.v(i)
               else
                    zmm6.v(i) = zmm2.v(i)/zmm3.v(i) ! r
                    zmm7.v(i) = zmm3.v(i)+zmm6.v(i)*zmm2.v(i) ! den
                    zre(i)    = (zmm0.v(i)*zmm6.v(i)+zmm1.v(i))/zmm7.v(i)
                    zim(i)    = (zmm1.v(i)*zmm6.v(i)+zmm0.v(i))/zmm7.v(i)
               end if
            end do
            return   
         else if(n>16 && n<=32) then
             do i = 0,iand(n-1,inot(15)),16
!$omp simd aligned(xim:64,xre,yre,yim) linear(ii:1)
               do ii = 0, 15
                    zmm0.v(ii)        = xre(i+ii) ! x_re
                    zmm1.v(ii)        = xim(i+ii) ! x_im
                    zmm2.v(ii)        = yre(i+ii) ! y_re, yr
                    zmm3.v(ii)        = yim(i+ii) ! y_im, yi            
                    zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                    zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                    m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                    if(m16_yi_gt_y4.m(ii)) then
                         zmm6.v(ii)   = zmm3.v(ii)/zmm2.v(ii) ! r
                         zmm7.v(ii)   = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                         zre(i+ii)    = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                         zim(i+ii)    = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                    else
                         zmm6.v(ii)   = zmm2.v(ii)/zmm3.v(ii) ! r
                         zmm7.v(ii)   = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                         zre(i+ii)    = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))/zmm7.v(ii)
                         zim(i+ii)    = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))/zmm7.v(ii)
                    end if
               end do
            end do 
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
           do j = i, n-1
               xr = xre(j)
               yr = yre(j)
               ayr= abs(yr)
               yi = yim(j)
               ayi= abs(yi)
               xi = xim(j)
               if(ayi.GreaterThan.ayr) then
                  r      = yi/yr
                  den    = yr+r*yi
                  zre(j) = (xr+xi*r)/den
                  zim(j) = (xi-xr*r)/den
               else
                  r      = yr/yi
                  den    = yi+r*yr
                  zre(j) = (xr*r+xi)/den
                  zim(j) = (xi*r+xr)/den
               end if 
           end do
           return
       else if(n>32) then
           do i=0, iand(n-1,inot(ZMM_LEN-1)), ZMM_LEN*8
                   call mm_prefetch(xre(i+8*ZMM_LEN),FOR_K_PREFETCH_T1)
                   call mm_prefetch(xim(i+8*ZMM_LEN),FOR_K_PREFETCH_T1)
                   call mm_prefetch(yre(i+8*ZMM_LEN),FOR_K_PREFETCH_T1)
                   call mm_prefetch(yim(i+8*ZMM_LEN),FOR_K_PREFETCH_T1)
#if defined(__ICC) || defined(__INTEL_COMPILER)
                   !dir$ assume_aligned  xre:64
                   !dir$ assume_aligned  xim:64
                   !dir$ assume_aligned  yre:64
                   !dir$ assume_aligned  yim:64
                   !dir$ assume_aligned  zre:64
                   !dir$ assume_aligned  zim:64
#endif                   
!$omp simd aligned(xim:64,xre,yre,yim)  linear(ii:1)              
                  do ii = 0, ZMM_LEN-1
                       zmm0.v(ii)        = xre(i+0+ii) ! x_re
                       zmm1.v(ii)        = xim(i+0+ii) ! x_im
                       zmm2.v(ii)        = yre(i+0+ii) ! y_re, yr
                       zmm3.v(ii)        = yim(i+0+ii) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(i+0+ii)    = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(i+0+ii)    = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(i+0+ii)    = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(i+0+ii)    = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx1              = i+1*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx1) ! x_re
                       zmm1.v(ii)        = xim(idx1) ! x_im
                       zmm2.v(ii)        = yre(idx1) ! y_re, yr
                       zmm3.v(ii)        = yim(idx1) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx1)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx1)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx1)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx1)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx2              = i+2*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx2) ! x_re
                       zmm1.v(ii)        = xim(idx2) ! x_im
                       zmm2.v(ii)        = yre(idx2) ! y_re, yr
                       zmm3.v(ii)        = yim(idx2) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx2)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx2)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx2)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx2)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx3               = i+3*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx3) ! x_re
                       zmm1.v(ii)        = xim(idx3) ! x_im
                       zmm2.v(ii)        = yre(idx3) ! y_re, yr
                       zmm3.v(ii)        = yim(idx3) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx3)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx3)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx3)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx3)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx4               = i+4*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx4) ! x_re
                       zmm1.v(ii)        = xim(idx4) ! x_im
                       zmm2.v(ii)        = yre(idx4) ! y_re, yr
                       zmm3.v(ii)        = yim(idx4) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx4)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx4)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx4)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx4)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx5              = i+5*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx5) ! x_re
                       zmm1.v(ii)        = xim(idx5) ! x_im
                       zmm2.v(ii)        = yre(idx5) ! y_re, yr
                       zmm3.v(ii)        = yim(idx5) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx5)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx5)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx5)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx5)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx6              = i+6*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx6) ! x_re
                       zmm1.v(ii)        = xim(idx6) ! x_im
                       zmm2.v(ii)        = yre(idx6) ! y_re, yr
                       zmm3.v(ii)        = yim(idx6) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx6)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx6)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx6)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx6)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx7              = i+7*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx7) ! x_re
                       zmm1.v(ii)        = xim(idx7) ! x_im
                       zmm2.v(ii)        = yre(idx7) ! y_re, yr
                       zmm3.v(ii)        = yim(idx7) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx7)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx7)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx7)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx7)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                   end do
              end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
           do j = i, n-1
               xr = xre(j)
               yr = yre(j)
               ayr= abs(yr)
               yi = yim(j)
               ayi= abs(yi)
               xi = xim(j)
               if(ayi.GreaterThan.ayr) then
                  r      = yi/yr
                  den    = yr+r*yi
                  zre(j) = (xr+xi*r)/den
                  zim(j) = (xi-xr*r)/den
               else
                  r      = yr/yi
                  den    = yi+r*yr
                  zre(j) = (xr*r+xi)/den
                  zim(j) = (xi*r+xr)/den
               end if 
           end do
           return
    end if               
end subroutine  cdivv_smith_v512_8x16_ps   


subroutine cdivv_smith_v512_4x16_ps(xre,xim,yre,yim,zre,zim,n)
#if defined(__ICC) || defined(__INTEL_COMPILER)    
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: cdivv_smith_v512_4x16_ps
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: TARGET_ARCH=skylake_avx512 :: cdivv_smith_v512_4x16_ps
#endif     
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xre
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xim
         real(kind=sp), allocatable, dimension(:), intent(in)  :: yre
         real(kind=sp), allocatable, dimension(:), intent(in)  :: yim
         real(kind=sp), allocatable, dimension(:), intent(out)  :: zre
         real(kind=sp), allocatable, dimension(:), intent(out)  :: zim
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
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm1
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm2
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm3
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm4
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm5
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm6
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm7
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm8
         type(Mask16_t),  automatic  :: m16_yi_gt_yr
         type(Mask8_t),   automatic  :: m8_yi_gt_yr
         type(Mask4_t),   automatic  :: m4_yi_gt_yr
         type(XMM4r4_t),  automatic  :: xmm0
         type(XMM4r4_t),  automatic  :: xmm1
         type(XMM4r4_t),  automatic  :: xmm2
         type(XMM4r4_t),  automatic  :: xmm3
         type(XMM4r4_t),  automatic  :: xmm4
         type(XMM4r4_t),  automatic  :: xmm5
         type(XMM4r4_t),  automatic  :: xmm6
         type(XMM4r4_t),  automatic  :: xmm7 
         type(YMM8r4_t),  automatic  :: ymm0 
         type(YMM8r4_t),  automatic  :: ymm1
         type(YMM8r4_t),  automatic  :: ymm2
         type(YMM8r4_t),  automatic  :: ymm3
         type(YMM8r4_t),  automatic  :: ymm4
         type(YMM8r4_t),  automatic  :: ymm5
         type(YMM8r4_t),  automatic  :: ymm6
         type(YMM8r4_t),  automatic  :: ymm7
             
         real(sp),        automatic  :: xr
         real(sp),        automatic  :: xi
         real(sp),        automatic  :: yr
         real(sp),        automatic  :: yi
         real(sp),        automatic  :: ayi
         real(sp),        automatic  :: ayr
         real(sp),        automatic  :: r
         real(sp),        automatic  :: den
         integer(i4),     automatic  :: i,ii,j
         integer(i4),     automatic  :: idx1,idx2,idx3
         
         if(n<=0) then
            return
         else if(n==1) then
            xr = xre(0)
            yr = yre(0)
            ayr= abs(yr)
            yi = yim(0)
            ayi= abs(yi)
            xi = xim(0)
            if(ayi.GreaterThan.ayr) then
                r      = yi/yr
                den    = yr+r*yi
                zre(0) = (xr+xi*r)/den
                zim(0) = (xi-xr*r)/den
            else
                r      = yr/yi
                den    = yi+r*yr
                zre(0) = (xr*r+xi)/den
                zim(0) = (xi*r+xr)/den
            end if 
            return
         else if(n>1 && n<=4) then
!$omp simd linear(i:1)
            do i=0, 3
               xmm0.v(i)   = xre(i) ! x_re
               xmm1.v(i)   = xim(i) ! x_im
               xmm2.v(i)   = yre(i) ! y_re, yr
               xmm3.v(i)   = yim(i) ! y_im, yi            
               xmm4.v(i)   = abs(xmm2.v(i)) ! abs(yre)
               xmm5.v(i)   = abs(xmm3.v(i)) ! abs(yim)
               m4_yi_gt_yr.m(i) = (xmm5.v(i)-xmm4.v(i)) >= SPACING(MAX(ABS(xmm5.v(i)),ABS(xmm4.v(i))))
               if(m4_yi_gt_y4.v(i)) then
                    xmm6.v(i) = xmm3.v(i)/xmm2.v(i) ! r
                    xmm7.v(i) = xmm2.v(i)+xmm6.v(i)*xmm3.v(i) ! den
                    zre(i)    = (xmm0.v(i)+xmm1.v(i)*xmm6.v(i))/xmm7.v(i)
                    zim(i)    = (xmm1.v(i)-xmm0.v(i)*xmm6.v(i))/xmm7.v(i)
               else
                    xmm6.v(i) = xmm2.v(i)/xmm3.v(i) ! r
                    xmm7.v(i) = xmm3.v(i)+xmm6.v(i)*xmm2.v(i) ! den
                    zre(i)    = (xmm0.v(i)*xmm6.v(i)+xmm1.v(i))/xmm7.v(i)
                    zim(i)    = (xmm1.v(i)*xmm6.v(i)+xmm0.v(i))/xmm7.v(i)
               end if
            end do
            return
        else if(n>4 && n<=8) then
!$omp simd linear(i:1)
            do i=0, 7
               ymm0.v(i)   = xre(i) ! x_re
               ymm1.v(i)   = xim(i) ! x_im
               ymm2.v(i)   = yre(i) ! y_re, yr
               ymm3.v(i)   = yim(i) ! y_im, yi            
               ymm4.v(i)   = abs(ymm2.v(i)) ! abs(yre)
               ymm5.v(i)   = abs(ymm3.v(i)) ! abs(yim)
               m8_yi_gt_yr.m(i) = (ymm5.v(i)-ymm4.v(i)) >= SPACING(MAX(ABS(ymm5.v(i)),ABS(ymm4.v(i))))
               if(m8_yi_gt_y4.m(i)) then
                    ymm6.v(i) = ymm3.v(i)/ymm2.v(i) ! r
                    ymm7.v(i) = ymm2.v(i)+ymm6.v(i)*ymm3.v(i) ! den
                    zre(i)    = (ymm0.v(i)+ymm1.v(i)*ymm6.v(i))/ymm7.v(i)
                    zim(i)    = (ymm1.v(i)-ymm0.v(i)*ymm6.v(i))/ymm7.v(i)
               else
                    ymm6.v(i) = ymm2.v(i)/ymm3.v(i) ! r
                    ymm7.v(i) = ymm3.v(i)+ymm6.v(i)*ymm2.v(i) ! den
                    zre(i)    = (ymm0.v(i)*ymm6.v(i)+ymm1.v(i))/ymm7.v(i)
                    zim(i)    = (ymm1.v(i)*ymm6.v(i)+ymm0.v(i))/ymm7.v(i)
               end if
            end do
            return 
         else if(n>8 && n<=16) then
!$omp simd linear(i:1)
            do i=0, 15
               zmm0.v(i)   = xre(i) ! x_re
               zmm1.v(i)   = xim(i) ! x_im
               zmm2.v(i)   = yre(i) ! y_re, yr
               zmm3.v(i)   = yim(i) ! y_im, yi            
               zmm4.v(i)   = abs(zmm2.v(i)) ! abs(yre)
               zmm5.v(i)   = abs(zmm3.v(i)) ! abs(yim)
               m16_yi_gt_yr.m(i) = (zmm5.v(i)-zmm4.v(i)) >= SPACING(MAX(ABS(zmm5.v(i)),ABS(zmm4.v(i))))
               if(m16_yi_gt_y4.m(i)) then
                    zmm6.v(i) = zmm3.v(i)/zmm2.v(i) ! r
                    zmm7.v(i) = zmm2.v(i)+zmm6.v(i)*zmm3.v(i) ! den
                    zre(i)    = (zmm0.v(i)+zmm1.v(i)*zmm6.v(i))/zmm7.v(i)
                    zim(i)    = (zmm1.v(i)-zmm0.v(i)*zmm6.v(i))/zmm7.v(i)
               else
                    zmm6.v(i) = zmm2.v(i)/zmm3.v(i) ! r
                    zmm7.v(i) = zmm3.v(i)+zmm6.v(i)*zmm2.v(i) ! den
                    zre(i)    = (zmm0.v(i)*zmm6.v(i)+zmm1.v(i))/zmm7.v(i)
                    zim(i)    = (zmm1.v(i)*zmm6.v(i)+zmm0.v(i))/zmm7.v(i)
               end if
            end do
            return   
         else if(n>16 && n<=32) then
             do i = 0,iand(n-1,inot(15)),16
!$omp simd aligned(xim:64,xre,yre,yim) linear(ii:1)
               do ii = 0, 15
                    zmm0.v(ii)        = xre(i+ii) ! x_re
                    zmm1.v(ii)        = xim(i+ii) ! x_im
                    zmm2.v(ii)        = yre(i+ii) ! y_re, yr
                    zmm3.v(ii)        = yim(i+ii) ! y_im, yi            
                    zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                    zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                    m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                    if(m16_yi_gt_y4.m(ii)) then
                         zmm6.v(ii)   = zmm3.v(ii)/zmm2.v(ii) ! r
                         zmm7.v(ii)   = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                         zre(i+ii)    = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                         zim(i+ii)    = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                    else
                         zmm6.v(ii)   = zmm2.v(ii)/zmm3.v(ii) ! r
                         zmm7.v(ii)   = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                         zre(i+ii)    = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))/zmm7.v(ii)
                         zim(i+ii)    = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))/zmm7.v(ii)
                    end if
               end do
            end do 
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
           do j = i, n-1
               xr = xre(j)
               yr = yre(j)
               ayr= abs(yr)
               yi = yim(j)
               ayi= abs(yi)
               xi = xim(j)
               if(ayi.GreaterThan.ayr) then
                  r      = yi/yr
                  den    = yr+r*yi
                  zre(j) = (xr+xi*r)/den
                  zim(j) = (xi-xr*r)/den
               else
                  r      = yr/yi
                  den    = yi+r*yr
                  zre(j) = (xr*r+xi)/den
                  zim(j) = (xi*r+xr)/den
               end if 
           end do
           return
       else if(n>32) then
           do i=0, iand(n-1,inot(ZMM_LEN-1)), ZMM_LEN*4
                   call mm_prefetch(xre(i+4*ZMM_LEN),FOR_K_PREFETCH_T1)
                   call mm_prefetch(xim(i+4*ZMM_LEN),FOR_K_PREFETCH_T1)
                   call mm_prefetch(yre(i+4*ZMM_LEN),FOR_K_PREFETCH_T1)
                   call mm_prefetch(yim(i+4*ZMM_LEN),FOR_K_PREFETCH_T1)
#if defined(__ICC) || defined(__INTEL_COMPILER)
                   !dir$ assume_aligned  xre:64
                   !dir$ assume_aligned  xim:64
                   !dir$ assume_aligned  yre:64
                   !dir$ assume_aligned  yim:64
                   !dir$ assume_aligned  zre:64
                   !dir$ assume_aligned  zim:64
#endif                   
!$omp simd aligned(xim:64,xre,yre,yim)  linear(ii:1)              
                  do ii = 0, ZMM_LEN-1
                       zmm0.v(ii)        = xre(i+0+ii) ! x_re
                       zmm1.v(ii)        = xim(i+0+ii) ! x_im
                       zmm2.v(ii)        = yre(i+0+ii) ! y_re, yr
                       zmm3.v(ii)        = yim(i+0+ii) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(i+0+ii)    = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(i+0+ii)    = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(i+0+ii)    = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(i+0+ii)    = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx1              = i+1*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx1) ! x_re
                       zmm1.v(ii)        = xim(idx1) ! x_im
                       zmm2.v(ii)        = yre(idx1) ! y_re, yr
                       zmm3.v(ii)        = yim(idx1) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx1)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx1)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx1)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx1)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx2              = i+2*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx2) ! x_re
                       zmm1.v(ii)        = xim(idx2) ! x_im
                       zmm2.v(ii)        = yre(idx2) ! y_re, yr
                       zmm3.v(ii)        = yim(idx2) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx2)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx2)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx2)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx2)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                       idx3               = i+3*ZMM_LEN+ii
                       zmm0.v(ii)        = xre(idx3) ! x_re
                       zmm1.v(ii)        = xim(idx3) ! x_im
                       zmm2.v(ii)        = yre(idx3) ! y_re, yr
                       zmm3.v(ii)        = yim(idx3) ! y_im, yi            
                       zmm4.v(ii)        = abs(zmm2.v(ii)) ! abs(yre)
                       zmm5.v(ii)        = abs(zmm3.v(ii)) ! abs(yim)
                       m16_yi_gt_yr.m(ii) = (zmm5.v(ii)-zmm4.v(ii)) >= SPACING(MAX(ABS(zmm5.v(ii)),ABS(zmm4.v(ii))))
                       if(m16_yi_gt_y4.m(ii)) then
                           zmm6.v(ii)     = zmm3.v(ii)/zmm2.v(ii) ! r
                           zmm7.v(ii)     = zmm2.v(ii)+zmm6.v(ii)*zmm3.v(ii) ! den
                           zre(idx3)      = (zmm0.v(ii)+zmm1.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                           zim(idx3)      = (zmm1.v(ii)-zmm0.v(ii)*zmm6.v(ii))/zmm7.v(ii)
                       else
                           zmm6.v(ii)     = zmm2.v(ii)/zmm3.v(ii) ! r
                           zmm7.v(ii)     = zmm3.v(ii)+zmm6.v(ii)*zmm2.v(ii) ! den
                           zmm8.v(ii)     = 1.0_sp/zmm7.v(ii)
                           zre(idx3)      = (zmm0.v(ii)*zmm6.v(ii)+zmm1.v(ii))*zmm8.v(ii)
                           zim(idx3)      = (zmm1.v(ii)*zmm6.v(ii)+zmm0.v(ii))*zmm8.v(ii)
                       end if
                  end do
              end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
           do j = i, n-1
               xr = xre(j)
               yr = yre(j)
               ayr= abs(yr)
               yi = yim(j)
               ayi= abs(yi)
               xi = xim(j)
               if(ayi.GreaterThan.ayr) then
                  r      = yi/yr
                  den    = yr+r*yi
                  zre(j) = (xr+xi*r)/den
                  zim(j) = (xi-xr*r)/den
               else
                  r      = yr/yi
                  den    = yi+r*yr
                  zre(j) = (xr*r+xi)/den
                  zim(j) = (xi*r+xr)/den
               end if 
           end do
           return
    end if               
end subroutine  cdivv_smith_v512_4x16_ps     
          
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
end module cdivv_smith_zmm16r4
     
