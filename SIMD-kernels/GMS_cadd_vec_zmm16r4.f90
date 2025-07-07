
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

module caddv_zmm16r4


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name: caddv_zmm16r4
 !                         
 !          
 !          Purpose:
 !                       Complex-addition vectorized deinterleaved, single-precision
 !                        
 !          History:
 !                        Date: 10-11-2024
 !                        Time: 08:405AM GMT+2
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
    implicit none
    public
    
    
    
      ! Major version
     integer(kind=i4),  parameter :: CADDV_ZMM16R4_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: CADDV_ZMM16R4_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: CADDV_ZMM16R4_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: CADDV_ZMM16R4_FULLVER =   &
            1000*CADDV_ZMM16R4_MAJOR+100*CADDV_ZMM16R4_MINOR+10*CADDV_ZMM16R4_MICRO
     ! Module creation date
     character(*),        parameter :: CADDV_ZMM16R4_CREATE_DATE = "10-11-2024 01:42PM +00200 (SUN 10 NOV 2024 GMT+2)"
     ! Module build date
     character(*),        parameter :: CADDV_ZMM16R4_BUILD_DATE  = __DATE__ 

     character(*),        parameter :: CADDV_ZMM16R4_BUILD_TIME  = __TIME__ 
     ! Module author info
     character(*),        parameter :: CADDV_ZMM16R4_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: CADDV_ZMM16R4_SYNOPSIS  = "Complex-addition vectorized deinterleaved, single-precision." 
     
    
     integer(kind=i4), parameter, private :: ZMM_LEN = 16
     
#if !defined(CADD_VEC_ZMM16R4_SOFT_PREFETCH) 
#define CADD_VEC_ZMM16R4_SOFT_PREFETCH 1 
#endif 
     
     contains
     
     
subroutine caddv_kernel_v512_cv_cv_32x16_ps(xre,xim,yre,yim,zre,zim,n)
#if defined(__ICC) || defined(__INTEL_COMPILER)    
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: caddv_kernel_v512_cv_cv_32x16_ps
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER:"target_arch=skylake-avx512" :: caddv_kernel_v512_cv_cv_32x16_ps
#endif     
         real(kind=sp), dimension(:), intent(in)  :: xre
         real(kind=sp), dimension(:), intent(in)  :: xim
         real(kind=sp), dimension(:), intent(in)  :: yre
         real(kind=sp), dimension(:), intent(in)  :: yim
         real(kind=sp), dimension(:), intent(out) :: zre
         real(kind=sp), dimension(:), intent(out)  :: zim 
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
         real(sp),        automatic  :: yr
         real(sp),        automatic  :: yi
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
            xr = xre(0)
            yr = yre(0)
            xi = xim(0)
            yi = yim(0)
            zre(0) = xr+yr
            zim(0) = xi+yi
            return
        else if(n>1 .and. n<=4) then
!$omp simd linear(i:1)
                 do i=0, 3
                    xmm0.v(i) = xre(i) ! xr
                    xmm1.v(i) = yre(i) ! yr
                    xmm2.v(i) = xim(i) ! xi
                    xmm3.v(i) = yim(i) ! yi
                    zre(i)  = xmm0.v(i)+xmm1.v(i)
                    zim(i)  = xmm2.v(i)+xmm3.v(i)
                 end do
                 return
        else if(n>4 .and. n<=8) then
!$omp simd linear(i:1)
                 do i=0, 7
                    ymm0.v(i) = xre(i) ! xr
                    ymm1.v(i) = yre(i) ! yr
                    ymm2.v(i) = xim(i) ! xi
                    ymm3.v(i) = yim(i) ! yi
                    zre(i)  = ymm0.v(i)+ymm1.v(i)
                    zim(i)  = ymm2.v(i)+ymm3.v(i)
                 end do
                 return    
        else if(n>8 .and. n<=16) then
!$omp simd linear(i:1)
                 do i=0, 15
                    zmm0.v(i) = xre(i) ! xr
                    zmm1.v(i) = yre(i) ! yr
                    zmm2.v(i) = xim(i) ! xi
                    zmm3.v(i) = yim(i) ! yi
                    zre(i)  = zmm0.v(i)+zmm1.v(i)
                    zim(i)  = zmm2.v(i)+zmm3.v(i)
                 end do
                 return 
        else if(n>16 .and. n<=64) then
                   do i = 0,iand(n-1,inot(15)),16
!$omp simd aligned(xim,xre,yre,yim,zre,zim:64) linear(ii:1)
                       do ii = 0, 15  
                            zmm0.v(ii) = xre(i+ii) ! xr
                            zmm1.v(ii) = yre(i+ii) ! yr
                            zmm2.v(ii) = xim(i+ii) ! xi
                            zmm3.v(ii) = yim(i+ii) ! yi
                            zre(i+ii)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(i+ii)  = zmm2.v(ii)+zmm3.v(ii)  
                       end do
                   end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                  do j = i, n-1   
                      xr = xre(j)
                      yr = yre(j)
                      xi = xim(j)
                      yi = yim(j)
                      zre(j) = xr+yr
                      zim(j) = xi+yi
                  end do                   
                  return
       else if(n>64 .and. n<=128) then
                   do i = 0,iand(n-1,inot(15)),16
!$omp simd aligned(xim,xre,yre,yim,zre,zim:64) linear(ii:1)
                       do ii = 0, 15  
                            zmm0.v(ii) = xre(i+ii) ! xr
                            zmm1.v(ii) = yre(i+ii) ! yr
                            zmm2.v(ii) = xim(i+ii) ! xi
                            zmm3.v(ii) = yim(i+ii) ! yi
                            zre(i+ii)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(i+ii)  = zmm2.v(ii)+zmm3.v(ii)  
                       end do
                   end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                  do j = i, n-1   
                      xr = xre(j)
                      yr = yre(j)
                      xi = xim(j)
                      yi = yim(j)
                      zre(j) = xr+yr
                      zim(j) = xi+yi
                  end do                   
                  return   
       else if(n>128) then
               do i=0, iand(n-1,inot(ZMM_LEN-1)), ZMM_LEN*32

#if defined(__ICC) || defined(__INTEL_COMPILER)
                   !dir$ assume_aligned  xre:64
                   !dir$ assume_aligned  xim:64
                   !dir$ assume_aligned  yre:64
                   !dir$ assume_aligned  yim:64
                   !dir$ assume_aligned  zre:64
                   !dir$ assume_aligned  zim:64
#endif                   
!$omp simd aligned(xim,xre,yre,yim,zre,zim:64)  linear(ii:1)              
                      do ii = 0, ZMM_LEN-1  
 
                            zmm0.v(ii) = xre(i+0+ii) ! xr
                            zmm1.v(ii) = yre(i+0+ii) ! yr
                            zmm2.v(ii) = xim(i+0+ii) ! xi
                            zmm3.v(ii) = yim(i+0+ii) ! yi
                            zre(i+0+ii)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(i+0+ii)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx1        = i+1*ZMM_LEN+ii

                            zmm0.v(ii) = xre(idx1) ! xr
                            zmm1.v(ii) = yre(idx1) ! yr
                            zmm2.v(ii) = xim(idx1) ! xi
                            zmm3.v(ii) = yim(idx1) ! yi
                            zre(idx1)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx1)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx2        = i+2*ZMM_LEN+ii

                            zmm0.v(ii) = xre(idx2) ! xr
                            zmm1.v(ii) = yre(idx2) ! yr
                            zmm2.v(ii) = xim(idx2) ! xi
                            zmm3.v(ii) = yim(idx2) ! yi
                            zre(idx2)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx2)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx3        = i+3*ZMM_LEN+ii

                            zmm0.v(ii) = xre(idx3) ! xr
                            zmm1.v(ii) = yre(idx3) ! yr
                            zmm2.v(ii) = xim(idx3) ! xi
                            zmm3.v(ii) = yim(idx3) ! yi
                            zre(idx3)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx3)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx4        = i+4*ZMM_LEN+ii

                            zmm0.v(ii) = xre(idx4) ! xr
                            zmm1.v(ii) = yre(idx4) ! yr
                            zmm2.v(ii) = xim(idx4) ! xi
                            zmm3.v(ii) = yim(idx4) ! yi
                            zre(idx4)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx4)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx5        = i+5*ZMM_LEN+ii

                            zmm0.v(ii) = xre(idx5) ! xr
                            zmm1.v(ii) = yre(idx5) ! yr
                            zmm2.v(ii) = xim(idx5) ! xi
                            zmm3.v(ii) = yim(idx5) ! yi
                            zre(idx5)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx5)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx6        = i+6*ZMM_LEN+ii

                            zmm0.v(ii) = xre(idx6) ! xr
                            zmm1.v(ii) = yre(idx6) ! yr
                            zmm2.v(ii) = xim(idx6) ! xi
                            zmm3.v(ii) = yim(idx6) ! yi
                            zre(idx6)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx6)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx7        = i+7*ZMM_LEN+ii

                            zmm0.v(ii) = xre(idx7) ! xr
                            zmm1.v(ii) = yre(idx7) ! yr
                            zmm2.v(ii) = xim(idx7) ! xi
                            zmm3.v(ii) = yim(idx7) ! yi
                            zre(idx7)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx7)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx8        = i+8*ZMM_LEN+ii

                            zmm0.v(ii) = xre(idx8) ! xr
                            zmm1.v(ii) = yre(idx8) ! yr
                            zmm2.v(ii) = xim(idx8) ! xi
                            zmm3.v(ii) = yim(idx8) ! yi
                            zre(idx8)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx8)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx9        = i+9*ZMM_LEN+ii

                            zmm0.v(ii) = xre(idx9) ! xr
                            zmm1.v(ii) = yre(idx9) ! yr
                            zmm2.v(ii) = xim(idx9) ! xi
                            zmm3.v(ii) = yim(idx9) ! yi
                            zre(idx9)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx9)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx10        = i+10*ZMM_LEN+ii

                            zmm0.v(ii) = xre(idx10) ! xr
                            zmm1.v(ii) = yre(idx10) ! yr
                            zmm2.v(ii) = xim(idx10) ! xi
                            zmm3.v(ii) = yim(idx10) ! yi
                            zre(idx10)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx10)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx11        = i+11*ZMM_LEN+ii

                            zmm0.v(ii) = xre(idx11) ! xr
                            zmm1.v(ii) = yre(idx11) ! yr
                            zmm2.v(ii) = xim(idx11) ! xi
                            zmm3.v(ii) = yim(idx11) ! yi
                            zre(idx11)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx11)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx12        = i+12*ZMM_LEN+ii

                            zmm0.v(ii) = xre(idx12) ! xr
                            zmm1.v(ii) = yre(idx12) ! yr
                            zmm2.v(ii) = xim(idx12) ! xi
                            zmm3.v(ii) = yim(idx12) ! yi
                            zre(idx12)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx12)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx13        = i+13*ZMM_LEN+ii

                            zmm0.v(ii) = xre(idx13) ! xr
                            zmm1.v(ii) = yre(idx13) ! yr
                            zmm2.v(ii) = xim(idx13) ! xi
                            zmm3.v(ii) = yim(idx13) ! yi
                            zre(idx13)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx13)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx14        = i+14*ZMM_LEN+ii

                            zmm0.v(ii) = xre(idx14) ! xr
                            zmm1.v(ii) = yre(idx14) ! yr
                            zmm2.v(ii) = xim(idx14) ! xi
                            zmm3.v(ii) = yim(idx14) ! yi
                            zre(idx14)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx14)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx15        = i+15*ZMM_LEN+ii

                            zmm0.v(ii) = xre(idx15) ! xr
                            zmm1.v(ii) = yre(idx15) ! yr
                            zmm2.v(ii) = xim(idx15) ! xi
                            zmm3.v(ii) = yim(idx15) ! yi
                            zre(idx15)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx15)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx16        = i+16*ZMM_LEN+ii

                            zmm0.v(ii) = xre(idx16) ! xr
                            zmm1.v(ii) = yre(idx16) ! yr
                            zmm2.v(ii) = xim(idx16) ! xi
                            zmm3.v(ii) = yim(idx16) ! yi
                            zre(idx16)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx16)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx17        = i+17*ZMM_LEN+ii

                            zmm0.v(ii) = xre(idx17) ! xr
                            zmm1.v(ii) = yre(idx17) ! yr
                            zmm2.v(ii) = xim(idx17) ! xi
                            zmm3.v(ii) = yim(idx17) ! yi
                            zre(idx17)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx17)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx18        = i+18*ZMM_LEN+ii

                            zmm0.v(ii) = xre(idx18) ! xr
                            zmm1.v(ii) = yre(idx18) ! yr
                            zmm2.v(ii) = xim(idx18) ! xi
                            zmm3.v(ii) = yim(idx18) ! yi
                            zre(idx18)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx18)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx19        = i+19*ZMM_LEN+ii
                         
                            zmm0.v(ii) = xre(idx19) ! xr
                            zmm1.v(ii) = yre(idx19) ! yr
                            zmm2.v(ii) = xim(idx19) ! xi
                            zmm3.v(ii) = yim(idx19) ! yi
                            zre(idx19)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx19)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx20        = i+20*ZMM_LEN+ii
 
                            zmm0.v(ii) = xre(idx20) ! xr
                            zmm1.v(ii) = yre(idx20) ! yr
                            zmm2.v(ii) = xim(idx20) ! xi
                            zmm3.v(ii) = yim(idx20) ! yi
                            zre(idx20)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx20)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx21        = i+21*ZMM_LEN+ii
                            
                            zmm0.v(ii) = xre(idx21) ! xr
                            zmm1.v(ii) = yre(idx21) ! yr
                            zmm2.v(ii) = xim(idx21) ! xi
                            zmm3.v(ii) = yim(idx21) ! yi
                            zre(idx21)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx21)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx22        = i+22*ZMM_LEN+ii
                          
                            zmm0.v(ii) = xre(idx22) ! xr
                            zmm1.v(ii) = yre(idx22) ! yr
                            zmm2.v(ii) = xim(idx22) ! xi
                            zmm3.v(ii) = yim(idx22) ! yi
                            zre(idx22)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx22)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx23        = i+23*ZMM_LEN+ii
                           
                            zmm0.v(ii) = xre(idx23) ! xr
                            zmm1.v(ii) = yre(idx23) ! yr
                            zmm2.v(ii) = xim(idx23) ! xi
                            zmm3.v(ii) = yim(idx23) ! yi
                            zre(idx23)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx23)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx24        = i+24*ZMM_LEN+ii

                            zmm0.v(ii) = xre(idx24) ! xr
                            zmm1.v(ii) = yre(idx24) ! yr
                            zmm2.v(ii) = xim(idx24) ! xi
                            zmm3.v(ii) = yim(idx24) ! yi
                            zre(idx24)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx24)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx25        = i+25*ZMM_LEN+ii
                           
                            zmm0.v(ii) = xre(idx25) ! xr
                            zmm1.v(ii) = yre(idx25) ! yr
                            zmm2.v(ii) = xim(idx25) ! xi
                            zmm3.v(ii) = yim(idx25) ! yi
                            zre(idx25)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx25)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx26        = i+26*ZMM_LEN+ii
                         
                            zmm0.v(ii) = xre(idx26) ! xr
                            zmm1.v(ii) = yre(idx26) ! yr
                            zmm2.v(ii) = xim(idx26) ! xi
                            zmm3.v(ii) = yim(idx26) ! yi
                            zre(idx26)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx26)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx27        = i+27*ZMM_LEN+ii
                           
                            zmm0.v(ii) = xre(idx27) ! xr
                            zmm1.v(ii) = yre(idx27) ! yr
                            zmm2.v(ii) = xim(idx27) ! xi
                            zmm3.v(ii) = yim(idx27) ! yi
                            zre(idx27)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx27)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx28        = i+28*ZMM_LEN+ii
                           
                            zmm0.v(ii) = xre(idx28) ! xr
                            zmm1.v(ii) = yre(idx28) ! yr
                            zmm2.v(ii) = xim(idx28) ! xi
                            zmm3.v(ii) = yim(idx28) ! yi
                            zre(idx28)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx28)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx29        = i+29*ZMM_LEN+ii

                            zmm0.v(ii) = xre(idx29) ! xr
                            zmm1.v(ii) = yre(idx29) ! yr
                            zmm2.v(ii) = xim(idx29) ! xi
                            zmm3.v(ii) = yim(idx29) ! yi
                            zre(idx29)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx29)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx30        = i+30*ZMM_LEN+ii

                            zmm0.v(ii) = xre(idx30) ! xr
                            zmm1.v(ii) = yre(idx30) ! yr
                            zmm2.v(ii) = xim(idx30) ! xi
                            zmm3.v(ii) = yim(idx30) ! yi
                            zre(idx30)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx30)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx31        = i+31*ZMM_LEN+ii

                            zmm0.v(ii) = xre(idx31) ! xr
                            zmm1.v(ii) = yre(idx31) ! yr
                            zmm2.v(ii) = xim(idx31) ! xi
                            zmm3.v(ii) = yim(idx31) ! yi
                            zre(idx31)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx31)  = zmm2.v(ii)+zmm3.v(ii)  
                   end do
               end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                  do j = i, n-1   
                      xr = xre(j)
                      yr = yre(j)
                      xi = xim(j)
                      yi = yim(j)
                      zre(j) = xr+yr
                      zim(j) = xi+yi
                  end do                   
                  return                  
       end if                   
end subroutine caddv_kernel_v512_cv_cv_32x16_ps



subroutine caddv_kernel_v512_cv_cv_16x16_ps(xre,xim,yre,yim,zre,zim,n)
#if defined(__ICC) || defined(__INTEL_COMPILER)    
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: caddv_kernel_v512_cv_cv_16x16_ps
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER:"target_arch=skylake-avx512" :: caddv_kernel_v512_cv_cv_16x16_ps
#endif     
         real(kind=sp), dimension(:), intent(in)  :: xre
         real(kind=sp), dimension(:), intent(in)  :: xim
         real(kind=sp), dimension(:), intent(in)  :: yre
         real(kind=sp), dimension(:), intent(in)  :: yim
         real(kind=sp), dimension(:), intent(out) :: zre
         real(kind=sp), dimension(:), intent(out)  :: zim 
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
         !DIR$ ATTRIBUTES ALIGN : 16 :: xmm0
         !DIR$ ATTRIBUTES ALIGN : 16 :: xmm1
         !DIR$ ATTRIBUTES ALIGN : 16 :: xmm2
         !DIR$ ATTRIBUTES ALIGN : 16 :: xmm3
         type(YMM8r4_t),  automatic  :: ymm0 
         type(YMM8r4_t),  automatic  :: ymm1
         type(YMM8r4_t),  automatic  :: ymm2
         type(YMM8r4_t),  automatic  :: ymm3
         !DIR$ ATTRIBUTES ALIGN : 32 :: ymm0
         !DIR$ ATTRIBUTES ALIGN : 32 :: ymm1
         !DIR$ ATTRIBUTES ALIGN : 32 :: ymm2
         !DIR$ ATTRIBUTES ALIGN : 32 :: ymm3
         real(sp),        automatic  :: xr
         real(sp),        automatic  :: xi
         real(sp),        automatic  :: yr
         real(sp),        automatic  :: yi
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
            xi = xim(0)
            yi = yim(0)
            zre(0) = xr+yr
            zim(0) = xi+yi
            return
        else if(n>1 .and. n<=4) then
!$omp simd linear(i:1)
                 do i=0, 3
                    xmm0.v(i) = xre(i) ! xr
                    xmm1.v(i) = yre(i) ! yr
                    xmm2.v(i) = xim(i) ! xi
                    xmm3.v(i) = yim(i) ! yi
                    zre(i)  = xmm0.v(i)+xmm1.v(i)
                    zim(i)  = xmm2.v(i)+xmm3.v(i)
                 end do
                 return
        else if(n>4 .and. n<=8) then
!$omp simd linear(i:1)
                 do i=0, 7
                    ymm0.v(i) = xre(i) ! xr
                    ymm1.v(i) = yre(i) ! yr
                    ymm2.v(i) = xim(i) ! xi
                    ymm3.v(i) = yim(i) ! yi
                    zre(i)  = ymm0.v(i)+ymm1.v(i)
                    zim(i)  = ymm2.v(i)+ymm3.v(i)
                 end do
                 return    
        else if(n>8 .and. n<=16) then
!$omp simd linear(i:1)
                 do i=0, 15
                    zmm0.v(i) = xre(i) ! xr
                    zmm1.v(i) = yre(i) ! yr
                    zmm2.v(i) = xim(i) ! xi
                    zmm3.v(i) = yim(i) ! yi
                    zre(i)  = zmm0.v(i)+zmm1.v(i)
                    zim(i)  = zmm2.v(i)+zmm3.v(i)
                 end do
                 return 
        else if(n>16 .and. n<=64) then
                   do i = 0,iand(n-1,inot(15)),16
!$omp simd aligned(xim,xre,yre,yim,zre,zim:64) linear(ii:1)
                       do ii = 0, 15  
                            zmm0.v(ii) = xre(i+ii) ! xr
                            zmm1.v(ii) = yre(i+ii) ! yr
                            zmm2.v(ii) = xim(i+ii) ! xi
                            zmm3.v(ii) = yim(i+ii) ! yi
                            zre(i+ii)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(i+ii)  = zmm2.v(ii)+zmm3.v(ii)  
                       end do
                   end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                  do j = i, n-1   
                      xr = xre(j)
                      yr = yre(j)
                      xi = xim(j)
                      yi = yim(j)
                      zre(j) = xr+yr
                      zim(j) = xi+yi
                  end do                   
                  return
       else if(n>64 .and. n<=128) then
                   do i = 0,iand(n-1,inot(15)),16
!$omp simd aligned(xim,xre,yre,yim,zre,zim:64) linear(ii:1)
                       do ii = 0, 15  
                            zmm0.v(ii) = xre(i+ii) ! xr
                            zmm1.v(ii) = yre(i+ii) ! yr
                            zmm2.v(ii) = xim(i+ii) ! xi
                            zmm3.v(ii) = yim(i+ii) ! yi
                            zre(i+ii)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(i+ii)  = zmm2.v(ii)+zmm3.v(ii)  
                       end do
                   end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                  do j = i, n-1   
                      xr = xre(j)
                      yr = yre(j)
                      xi = xim(j)
                      yi = yim(j)
                      zre(j) = xr+yr
                      zim(j) = xi+yi
                  end do                   
                  return   
       else if(n>128) then
               do i=0, iand(n-1,inot(ZMM_LEN-1)), ZMM_LEN*16
#if (CADD_VEC_ZMM16R4_SOFT_PREFETCH) == 1
                   do ii=0, ZMM_LEN-1
                         call mm_prefetch(xre(i+0+ii),0,.false.,.true.)
                         call mm_prefetch(xim(i+0+ii),0,.false.,.true.)
                         call mm_prefetch(yre(i+0+ii),0,.false.,.true.)
                         call mm_prefetch(yim(i+0+ii),0,.false.,.true.)
                         call mm_prefetch(xre(i+1*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xim(i+1*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yre(i+1*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yim(i+1*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xre(i+2*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xim(i+2*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yre(i+2*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yim(i+2*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xre(i+3*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xim(i+3*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yre(i+3*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yim(i+3*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xre(i+4*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xim(i+4*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yre(i+4*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yim(i+4*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xre(i+5*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xim(i+5*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yre(i+5*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yim(i+5*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xre(i+6*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xim(i+6*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yre(i+6*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yim(i+6*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xre(i+7*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xim(i+7*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yre(i+7*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yim(i+7*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xre(i+8*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xim(i+8*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yre(i+8*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yim(i+8*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xre(i+9*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xim(i+9*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yre(i+9*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yim(i+9*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xre(i+10*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xim(i+10*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yre(i+10*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yim(i+10*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xre(i+11*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xim(i+11*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yre(i+11*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yim(i+11*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xre(i+12*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xim(i+12*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yre(i+12*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yim(i+12*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xre(i+13*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xim(i+13*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yre(i+13*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yim(i+13*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xre(i+14*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xim(i+14*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yre(i+14*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yim(i+14*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xre(i+15*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xim(i+15*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yre(i+15*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yim(i+15*ZMM_LEN+ii),0,.false.,.true.)
                    end do 
#endif 
#if defined(__ICC) || defined(__INTEL_COMPILER)
                   !dir$ assume_aligned  xre:64
                   !dir$ assume_aligned  xim:64
                   !dir$ assume_aligned  yre:64
                   !dir$ assume_aligned  yim:64
                   !dir$ assume_aligned  zre:64
                   !dir$ assume_aligned  zim:64
#endif                   
!$omp simd aligned(xim,xre,yre,yim,zre,zim:64)  linear(ii:1)              
                      do ii = 0, ZMM_LEN-1  
                            zmm0.v(ii) = xre(i+0+ii) ! xr
                            zmm1.v(ii) = yre(i+0+ii) ! yr
                            zmm2.v(ii) = xim(i+0+ii) ! xi
                            zmm3.v(ii) = yim(i+0+ii) ! yi
                            zre(i+0+ii)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(i+0+ii)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx1        = i+1*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx1) ! xr
                            zmm1.v(ii) = yre(idx1) ! yr
                            zmm2.v(ii) = xim(idx1) ! xi
                            zmm3.v(ii) = yim(idx1) ! yi
                            zre(idx1)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx1)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx2        = i+2*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx2) ! xr
                            zmm1.v(ii) = yre(idx2) ! yr
                            zmm2.v(ii) = xim(idx2) ! xi
                            zmm3.v(ii) = yim(idx2) ! yi
                            zre(idx2)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx2)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx3        = i+3*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx3) ! xr
                            zmm1.v(ii) = yre(idx3) ! yr
                            zmm2.v(ii) = xim(idx3) ! xi
                            zmm3.v(ii) = yim(idx3) ! yi
                            zre(idx3)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx3)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx4        = i+4*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx4) ! xr
                            zmm1.v(ii) = yre(idx4) ! yr
                            zmm2.v(ii) = xim(idx4) ! xi
                            zmm3.v(ii) = yim(idx4) ! yi
                            zre(idx4)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx4)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx5        = i+5*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx5) ! xr
                            zmm1.v(ii) = yre(idx5) ! yr
                            zmm2.v(ii) = xim(idx5) ! xi
                            zmm3.v(ii) = yim(idx5) ! yi
                            zre(idx5)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx5)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx6        = i+6*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx6) ! xr
                            zmm1.v(ii) = yre(idx6) ! yr
                            zmm2.v(ii) = xim(idx6) ! xi
                            zmm3.v(ii) = yim(idx6) ! yi
                            zre(idx6)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx6)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx7        = i+7*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx7) ! xr
                            zmm1.v(ii) = yre(idx7) ! yr
                            zmm2.v(ii) = xim(idx7) ! xi
                            zmm3.v(ii) = yim(idx7) ! yi
                            zre(idx7)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx7)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx8        = i+8*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx8) ! xr
                            zmm1.v(ii) = yre(idx8) ! yr
                            zmm2.v(ii) = xim(idx8) ! xi
                            zmm3.v(ii) = yim(idx8) ! yi
                            zre(idx8)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx8)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx9        = i+9*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx9) ! xr
                            zmm1.v(ii) = yre(idx9) ! yr
                            zmm2.v(ii) = xim(idx9) ! xi
                            zmm3.v(ii) = yim(idx9) ! yi
                            zre(idx9)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx9)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx10        = i+10*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx10) ! xr
                            zmm1.v(ii) = yre(idx10) ! yr
                            zmm2.v(ii) = xim(idx10) ! xi
                            zmm3.v(ii) = yim(idx10) ! yi
                            zre(idx10)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx10)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx11        = i+11*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx11) ! xr
                            zmm1.v(ii) = yre(idx11) ! yr
                            zmm2.v(ii) = xim(idx11) ! xi
                            zmm3.v(ii) = yim(idx11) ! yi
                            zre(idx11)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx11)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx12        = i+12*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx12) ! xr
                            zmm1.v(ii) = yre(idx12) ! yr
                            zmm2.v(ii) = xim(idx12) ! xi
                            zmm3.v(ii) = yim(idx12) ! yi
                            zre(idx12)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx12)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx13        = i+13*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx13) ! xr
                            zmm1.v(ii) = yre(idx13) ! yr
                            zmm2.v(ii) = xim(idx13) ! xi
                            zmm3.v(ii) = yim(idx13) ! yi
                            zre(idx13)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx13)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx14        = i+14*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx14) ! xr
                            zmm1.v(ii) = yre(idx14) ! yr
                            zmm2.v(ii) = xim(idx14) ! xi
                            zmm3.v(ii) = yim(idx14) ! yi
                            zre(idx14)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx14)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx15        = i+15*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx15) ! xr
                            zmm1.v(ii) = yre(idx15) ! yr
                            zmm2.v(ii) = xim(idx15) ! xi
                            zmm3.v(ii) = yim(idx15) ! yi
                            zre(idx15)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx15)  = zmm2.v(ii)+zmm3.v(ii)  
                   end do
               end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                  do j = i, n-1   
                      xr = xre(j)
                      yr = yre(j)
                      xi = xim(j)
                      yi = yim(j)
                      zre(j) = xr+yr
                      zim(j) = xi+yi
                  end do                   
                  return                  
       end if                   
end subroutine caddv_kernel_v512_cv_cv_16x16_ps




subroutine caddv_kernel_v512_cv_cv_8x16_ps(xre,xim,yre,yim,zre,zim,n)
#if defined(__ICC) || defined(__INTEL_COMPILER)    
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: caddv_kernel_v512_cv_cv_8x16_ps
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER:"target_arch=skylake-avx512" :: caddv_kernel_v512_cv_cv_8x16_ps
#endif     
         real(kind=sp), dimension(:), intent(in)  :: xre
         real(kind=sp), dimension(:), intent(in)  :: xim
         real(kind=sp), dimension(:), intent(in)  :: yre
         real(kind=sp), dimension(:), intent(in)  :: yim
         real(kind=sp), dimension(:), intent(out) :: zre
         real(kind=sp), dimension(:), intent(out)  :: zim 
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
         !DIR$ ATTRIBUTES ALIGN : 16 :: xmm0
         !DIR$ ATTRIBUTES ALIGN : 16 :: xmm1
         !DIR$ ATTRIBUTES ALIGN : 16 :: xmm2
         !DIR$ ATTRIBUTES ALIGN : 16 :: xmm3
         type(YMM8r4_t),  automatic  :: ymm0 
         type(YMM8r4_t),  automatic  :: ymm1
         type(YMM8r4_t),  automatic  :: ymm2
         type(YMM8r4_t),  automatic  :: ymm3
         !DIR$ ATTRIBUTES ALIGN : 32 :: ymm0
         !DIR$ ATTRIBUTES ALIGN : 32 :: ymm1
         !DIR$ ATTRIBUTES ALIGN : 32 :: ymm2
         !DIR$ ATTRIBUTES ALIGN : 32 :: ymm3
         real(sp),        automatic  :: xr
         real(sp),        automatic  :: xi
         real(sp),        automatic  :: yr
         real(sp),        automatic  :: yi
         integer(i4),     automatic  :: i,ii,j
         integer(i4),     automatic  :: idx1,idx2,idx3,idx4
         integer(i4),     automatic  :: idx5,idx6,idx7
 
         if(n<=0) then
            return
         else if(n==1) then
            xr = xre(0)
            yr = yre(0)
            xi = xim(0)
            yi = yim(0)
            zre(0) = xr+yr
            zim(0) = xi+yi
            return
        else if(n>1 .and. n<=4) then
!$omp simd linear(i:1)
                 do i=0, 3
                    xmm0.v(i) = xre(i) ! xr
                    xmm1.v(i) = yre(i) ! yr
                    xmm2.v(i) = xim(i) ! xi
                    xmm3.v(i) = yim(i) ! yi
                    zre(i)  = xmm0.v(i)+xmm1.v(i)
                    zim(i)  = xmm2.v(i)+xmm3.v(i)
                 end do
                 return
        else if(n>4 .and. n<=8) then
!$omp simd linear(i:1)
                 do i=0, 7
                    ymm0.v(i) = xre(i) ! xr
                    ymm1.v(i) = yre(i) ! yr
                    ymm2.v(i) = xim(i) ! xi
                    ymm3.v(i) = yim(i) ! yi
                    zre(i)  = ymm0.v(i)+ymm1.v(i)
                    zim(i)  = ymm2.v(i)+ymm3.v(i)
                 end do
                 return    
        else if(n>8 .and. n<=16) then
!$omp simd linear(i:1)
                 do i=0, 15
                    zmm0.v(i) = xre(i) ! xr
                    zmm1.v(i) = yre(i) ! yr
                    zmm2.v(i) = xim(i) ! xi
                    zmm3.v(i) = yim(i) ! yi
                    zre(i)  = zmm0.v(i)+zmm1.v(i)
                    zim(i)  = zmm2.v(i)+zmm3.v(i)
                 end do
                 return 
        else if(n>16 .and. n<=64) then
                   do i = 0,iand(n-1,inot(15)),16
!$omp simd aligned(xim,xre,yre,yim,zre,zim:64) linear(ii:1)
                       do ii = 0, 15  
                            zmm0.v(ii) = xre(i+ii) ! xr
                            zmm1.v(ii) = yre(i+ii) ! yr
                            zmm2.v(ii) = xim(i+ii) ! xi
                            zmm3.v(ii) = yim(i+ii) ! yi
                            zre(i+ii)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(i+ii)  = zmm2.v(ii)+zmm3.v(ii)  
                       end do
                   end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                  do j = i, n-1   
                      xr = xre(j)
                      yr = yre(j)
                      xi = xim(j)
                      yi = yim(j)
                      zre(j) = xr+yr
                      zim(j) = xi+yi
                  end do                   
                  return
        else if(n>64) then
               do i=0, iand(n-1,inot(ZMM_LEN-1)), ZMM_LEN*8
#if (CADD_VEC_ZMM16R4_SOFT_PREFETCH) == 1
                   do ii=0, ZMM_LEN-1
                         call mm_prefetch(xre(i+0+ii),0,.false.,.true.)
                         call mm_prefetch(xim(i+0+ii),0,.false.,.true.)
                         call mm_prefetch(yre(i+0+ii),0,.false.,.true.)
                         call mm_prefetch(yim(i+0+ii),0,.false.,.true.)
                         call mm_prefetch(xre(i+1*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xim(i+1*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yre(i+1*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yim(i+1*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xre(i+2*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xim(i+2*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yre(i+2*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yim(i+2*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xre(i+3*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xim(i+3*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yre(i+3*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yim(i+3*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xre(i+4*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xim(i+4*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yre(i+4*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yim(i+4*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xre(i+5*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xim(i+5*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yre(i+5*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yim(i+5*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xre(i+6*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xim(i+6*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yre(i+6*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yim(i+6*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xre(i+7*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xim(i+7*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yre(i+7*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yim(i+7*ZMM_LEN+ii),0,.false.,.true.)
                    end do 
#endif 
#if defined(__ICC) || defined(__INTEL_COMPILER)
                   !dir$ assume_aligned  xre:64
                   !dir$ assume_aligned  xim:64
                   !dir$ assume_aligned  yre:64
                   !dir$ assume_aligned  yim:64
                   !dir$ assume_aligned  zre:64
                   !dir$ assume_aligned  zim:64
#endif                   
!$omp simd aligned(xim,xre,yre,yim,zre,zim:64)  linear(ii:1)              
                      do ii = 0, ZMM_LEN-1  
                            zmm0.v(ii) = xre(i+0+ii) ! xr
                            zmm1.v(ii) = yre(i+0+ii) ! yr
                            zmm2.v(ii) = xim(i+0+ii) ! xi
                            zmm3.v(ii) = yim(i+0+ii) ! yi
                            zre(i+0+ii)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(i+0+ii)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx1        = i+1*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx1) ! xr
                            zmm1.v(ii) = yre(idx1) ! yr
                            zmm2.v(ii) = xim(idx1) ! xi
                            zmm3.v(ii) = yim(idx1) ! yi
                            zre(idx1)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx1)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx2        = i+2*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx2) ! xr
                            zmm1.v(ii) = yre(idx2) ! yr
                            zmm2.v(ii) = xim(idx2) ! xi
                            zmm3.v(ii) = yim(idx2) ! yi
                            zre(idx2)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx2)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx3        = i+3*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx3) ! xr
                            zmm1.v(ii) = yre(idx3) ! yr
                            zmm2.v(ii) = xim(idx3) ! xi
                            zmm3.v(ii) = yim(idx3) ! yi
                            zre(idx3)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx3)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx4        = i+4*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx4) ! xr
                            zmm1.v(ii) = yre(idx4) ! yr
                            zmm2.v(ii) = xim(idx4) ! xi
                            zmm3.v(ii) = yim(idx4) ! yi
                            zre(idx4)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx4)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx5        = i+5*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx5) ! xr
                            zmm1.v(ii) = yre(idx5) ! yr
                            zmm2.v(ii) = xim(idx5) ! xi
                            zmm3.v(ii) = yim(idx5) ! yi
                            zre(idx5)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx5)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx6        = i+6*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx6) ! xr
                            zmm1.v(ii) = yre(idx6) ! yr
                            zmm2.v(ii) = xim(idx6) ! xi
                            zmm3.v(ii) = yim(idx6) ! yi
                            zre(idx6)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx6)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx7        = i+7*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx7) ! xr
                            zmm1.v(ii) = yre(idx7) ! yr
                            zmm2.v(ii) = xim(idx7) ! xi
                            zmm3.v(ii) = yim(idx7) ! yi
                            zre(idx7)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx7)  = zmm2.v(ii)+zmm3.v(ii)  
                   end do
               end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                  do j = i, n-1   
                      xr = xre(j)
                      yr = yre(j)
                      xi = xim(j)
                      yi = yim(j)
                      zre(j) = xr+yr
                      zim(j) = xi+yi
                  end do                   
                  return                  
       end if                   
end subroutine caddv_kernel_v512_cv_cv_8x16_ps



subroutine caddv_kernel_v512_cv_cv_4x16_ps(xre,xim,yre,yim,zre,zim,n)
#if defined(__ICC) || defined(__INTEL_COMPILER) 
        !DIR$ OPTIMIZE : 3   
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: caddv_kernel_v512_cv_cv_4x16_ps
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER:"target_arch=skylake-avx512" :: caddv_kernel_v512_cv_cv_4x16_ps
#endif     
         real(kind=sp), dimension(:), intent(in)  :: xre
         real(kind=sp), dimension(:), intent(in)  :: xim
         real(kind=sp), dimension(:), intent(in)  :: yre
         real(kind=sp), dimension(:), intent(in)  :: yim
         real(kind=sp), dimension(:), intent(out) :: zre
         real(kind=sp), dimension(:), intent(out)  :: zim 
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
         !DIR$ ATTRIBUTES ALIGN : 16 :: xmm0
         !DIR$ ATTRIBUTES ALIGN : 16 :: xmm1
         !DIR$ ATTRIBUTES ALIGN : 16 :: xmm2
         !DIR$ ATTRIBUTES ALIGN : 16 :: xmm3
         type(YMM8r4_t),  automatic  :: ymm0 
         type(YMM8r4_t),  automatic  :: ymm1
         type(YMM8r4_t),  automatic  :: ymm2
         type(YMM8r4_t),  automatic  :: ymm3
         !DIR$ ATTRIBUTES ALIGN : 32 :: ymm0
         !DIR$ ATTRIBUTES ALIGN : 32 :: ymm1
         !DIR$ ATTRIBUTES ALIGN : 32 :: ymm2
         !DIR$ ATTRIBUTES ALIGN : 32 :: ymm3
         real(sp),        automatic  :: xr
         real(sp),        automatic  :: xi
         real(sp),        automatic  :: yr
         real(sp),        automatic  :: yi
         integer(i4),     automatic  :: i,ii,j
         integer(i4),     automatic  :: idx1,idx2,idx3
 
         if(n<=0) then
            return
         else if(n==1) then
            xr = xre(0)
            yr = yre(0)
            xi = xim(0)
            yi = yim(0)
            zre(0) = xr+yr
            zim(0) = xi+yi
            return
        else if(n>1 .and. n<=4) then
!$omp simd linear(i:1)
                 do i=0, 3
                    xmm0.v(i) = xre(i) ! xr
                    xmm1.v(i) = yre(i) ! yr
                    xmm2.v(i) = xim(i) ! xi
                    xmm3.v(i) = yim(i) ! yi
                    zre(i)  = xmm0.v(i)+xmm1.v(i)
                    zim(i)  = xmm2.v(i)+xmm3.v(i)
                 end do
                 return
        else if(n>4 .and. n<=8) then
!$omp simd linear(i:1)
                 do i=0, 7
                    ymm0.v(i) = xre(i) ! xr
                    ymm1.v(i) = yre(i) ! yr
                    ymm2.v(i) = xim(i) ! xi
                    ymm3.v(i) = yim(i) ! yi
                    zre(i)  = ymm0.v(i)+ymm1.v(i)
                    zim(i)  = ymm2.v(i)+ymm3.v(i)
                 end do
                 return    
        else if(n>8 .and. n<=16) then
!$omp simd linear(i:1)
                 do i=0, 15
                    zmm0.v(i) = xre(i) ! xr
                    zmm1.v(i) = yre(i) ! yr
                    zmm2.v(i) = xim(i) ! xi
                    zmm3.v(i) = yim(i) ! yi
                    zre(i)  = zmm0.v(i)+zmm1.v(i)
                    zim(i)  = zmm2.v(i)+zmm3.v(i)
                 end do
                 return 
        else if(n>16 .and. n<=32) then
                   do i = 0,iand(n-1,inot(15)),16
!$omp simd aligned(xim,xre,yre,yim,zre,zim:64) linear(ii:1)
                       do ii = 0, 15  
                            zmm0.v(ii) = xre(i+ii) ! xr
                            zmm1.v(ii) = yre(i+ii) ! yr
                            zmm2.v(ii) = xim(i+ii) ! xi
                            zmm3.v(ii) = yim(i+ii) ! yi
                            zre(i+ii)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(i+ii)  = zmm2.v(ii)+zmm3.v(ii)  
                       end do
                   end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                  do j = i, n-1   
                      xr = xre(j)
                      yr = yre(j)
                      xi = xim(j)
                      yi = yim(j)
                      zre(j) = xr+yr
                      zim(j) = xi+yi
                  end do                   
                  return
        else if(n>32) then
               do i=0, iand(n-1,inot(ZMM_LEN-1)), ZMM_LEN*4
#if (CADD_VEC_ZMM16R4_SOFT_PREFETCH) == 1
                   do ii=0, ZMM_LEN-1
                         call mm_prefetch(xre(i+0+ii),0,.false.,.true.)
                         call mm_prefetch(xim(i+0+ii),0,.false.,.true.)
                         call mm_prefetch(yre(i+0+ii),0,.false.,.true.)
                         call mm_prefetch(yim(i+0+ii),0,.false.,.true.)
                         call mm_prefetch(xre(i+1*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xim(i+1*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yre(i+1*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yim(i+1*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xre(i+2*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xim(i+2*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yre(i+2*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yim(i+2*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xre(i+3*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(xim(i+3*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yre(i+3*ZMM_LEN+ii),0,.false.,.true.)
                         call mm_prefetch(yim(i+3*ZMM_LEN+ii),0,.false.,.true.)
                   end do 
#endif                   
#if defined(__ICC) || defined(__INTEL_COMPILER)
                   !dir$ assume_aligned  xre:64
                   !dir$ assume_aligned  xim:64
                   !dir$ assume_aligned  yre:64
                   !dir$ assume_aligned  yim:64
                   !dir$ assume_aligned  zre:64
                   !dir$ assume_aligned  zim:64
#endif                   
!$omp simd aligned(xim,xre,yre,yim,zre,zim:64)  linear(ii:1)              
                      do ii = 0, ZMM_LEN-1  
                            zmm0.v(ii) = xre(i+0+ii) ! xr
                            zmm1.v(ii) = yre(i+0+ii) ! yr
                            zmm2.v(ii) = xim(i+0+ii) ! xi
                            zmm3.v(ii) = yim(i+0+ii) ! yi
                            zre(i+0+ii)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(i+0+ii)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx1        = i+1*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx1) ! xr
                            zmm1.v(ii) = yre(idx1) ! yr
                            zmm2.v(ii) = xim(idx1) ! xi
                            zmm3.v(ii) = yim(idx1) ! yi
                            zre(idx1)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx1)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx2        = i+2*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx2) ! xr
                            zmm1.v(ii) = yre(idx2) ! yr
                            zmm2.v(ii) = xim(idx2) ! xi
                            zmm3.v(ii) = yim(idx2) ! yi
                            zre(idx2)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx2)  = zmm2.v(ii)+zmm3.v(ii)  
                            idx3        = i+3*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx3) ! xr
                            zmm1.v(ii) = yre(idx3) ! yr
                            zmm2.v(ii) = xim(idx3) ! xi
                            zmm3.v(ii) = yim(idx3) ! yi
                            zre(idx3)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx3)  = zmm2.v(ii)+zmm3.v(ii)  
                   end do
               end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                  do j = i, n-1   
                      xr = xre(j)
                      yr = yre(j)
                      xi = xim(j)
                      yi = yim(j)
                      zre(j) = xr+yr
                      zim(j) = xi+yi
                  end do                   
                  return                  
       end if                   
end subroutine caddv_kernel_v512_cv_cv_4x16_ps

#if 0

subroutine caddv_kernel_v512_cv_sv_32x16_ps(xre,xim,yre,zre,zim,n)
#if defined(__ICC) || defined(__INTEL_COMPILER)  
        !DIR$ OPTIMIZE : 3  
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: caddv_kernel_v512_cv_sv_32x16_ps
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER:"target_arch=skylake-avx512" :: caddv_kernel_v512_cv_sv_32x16_ps
#endif     
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xre
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xim
         real(kind=sp), allocatable, dimension(:), intent(in)  :: yre
         real(kind=sp), allocatable, dimension(:), intent(out) :: zre
         real(kind=sp), allocatable, dimension(:), intent(out)  :: zim 
         integer(i4),                              intent(in)  :: n
         
         type(ZMM16r4_t), automatic :: zmm0
         type(ZMM16r4_t), automatic :: zmm1
         type(ZMM16r4_t), automatic :: zmm2
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm1
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm2
         type(XMM4r4_t),  automatic  :: xmm0
         type(XMM4r4_t),  automatic  :: xmm1
         type(XMM4r4_t),  automatic  :: xmm2
         type(YMM8r4_t),  automatic  :: ymm0 
         type(YMM8r4_t),  automatic  :: ymm1
         type(YMM8r4_t),  automatic  :: ymm2
         real(sp),        automatic  :: xr
         real(sp),        automatic  :: xi
         real(sp),        automatic  :: yr
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
            xr = xre(0)
            yr = yre(0)
            xi = xim(0)
            zre(0) = xr+yr
            zim(0) = xi+yr
            return
        else if(n>1 .and. n<=4) then
!$omp simd linear(i:1)
                 do i=0, 3
                    xmm0.v(i) = xre(i) ! xr
                    xmm1.v(i) = yre(i) ! yr
                    xmm2.v(i) = xim(i) ! xi
                    zre(i)  = xmm0.v(i)+xmm1.v(i)
                    zim(i)  = xmm2.v(i)+xmm1.v(i)
                 end do
                 return
        else if(n>4 .and. n<=8) then
!$omp simd linear(i:1)
                 do i=0, 7
                    ymm0.v(i) = xre(i) ! xr
                    ymm1.v(i) = yre(i) ! yr
                    ymm2.v(i) = xim(i) ! xi
                    zre(i)  = ymm0.v(i)+ymm1.v(i)
                    zim(i)  = ymm2.v(i)+ymm1.v(i)
                 end do
                 return    
        else if(n>8 .and. n<=16) then
!$omp simd linear(i:1)
                 do i=0, 15
                    zmm0.v(i) = xre(i) ! xr
                    zmm1.v(i) = yre(i) ! yr
                    zmm2.v(i) = xim(i) ! xi
                    zre(i)  = zmm0.v(i)+zmm1.v(i)
                    zim(i)  = zmm2.v(i)+zmm1.v(i)
                 end do
                 return 
        else if(n>16 .and. n<=64) then
                   do i = 0,iand(n-1,inot(15)),16
!$omp simd aligned(xim,xre,yre,zre,zim:64) linear(ii:1)
                       do ii = 0, 15  
                            zmm0.v(ii) = xre(i+ii) ! xr
                            zmm1.v(ii) = yre(i+ii) ! yr
                            zmm2.v(ii) = xim(i+ii) ! xi
                            zre(i+ii)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(i+ii)  = zmm2.v(ii)+zmm1.v(ii)  
                       end do
                   end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                  do j = i, n-1   
                      xr = xre(j)
                      yr = yre(j)
                      xi = xim(j)
                      zre(j) = xr+yr
                      zim(j) = xi+yr
                  end do                   
                  return
       else if(n>64 .and. n<=128) then
                   do i = 0,iand(n-1,inot(15)),16
!$omp simd aligned(xim,xre,yre,zre,zim:64) linear(ii:1)
                       do ii = 0, 15  
                            zmm0.v(ii) = xre(i+ii) ! xr
                            zmm1.v(ii) = yre(i+ii) ! yr
                            zmm2.v(ii) = xim(i+ii) ! xi
                            zre(i+ii)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(i+ii)  = zmm2.v(ii)+zmm1.v(ii)  
                       end do
                   end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                  do j = i, n-1   
                      xr = xre(j)
                      yr = yre(j)
                      xi = xim(j)
                      zre(j) = xr+yr
                      zim(j) = xi+yr
                  end do                   
                  return   
       else if(n>128) then
               do i=0, iand(n-1,inot(ZMM_LEN-1)), ZMM_LEN*32
                   call mm_prefetch(xre(i+32*ZMM_LEN),FOR_K_PREFETCH_T1)
                   call mm_prefetch(xim(i+32*ZMM_LEN),FOR_K_PREFETCH_T1)
                   call mm_prefetch(yre(i+32*ZMM_LEN),FOR_K_PREFETCH_T1)
#if defined(__ICC) || defined(__INTEL_COMPILER)
                   !dir$ assume_aligned  xre:64
                   !dir$ assume_aligned  xim:64
                   !dir$ assume_aligned  yre:64
                   !dir$ assume_aligned  zre:64
                   !dir$ assume_aligned  zim:64
#endif                   
!$omp simd aligned(xim,xre,yre,zre,zim:64)  linear(ii:1)              
                      do ii = 0, ZMM_LEN-1  
                            zmm0.v(ii) = xre(i+0+ii) ! xr
                            zmm1.v(ii) = yre(i+0+ii) ! yr
                            zmm2.v(ii) = xim(i+0+ii) ! xi
                            zre(i+0+ii)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(i+0+ii)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx1        = i+1*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx1) ! xr
                            zmm1.v(ii) = yre(idx1) ! yr
                            zmm2.v(ii) = xim(idx1) ! xi
                            zre(idx1)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx1)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx2        = i+2*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx2) ! xr
                            zmm1.v(ii) = yre(idx2) ! yr
                            zmm2.v(ii) = xim(idx2) ! xi
                            zre(idx2)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx2)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx3        = i+3*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx3) ! xr
                            zmm1.v(ii) = yre(idx3) ! yr
                            zmm2.v(ii) = xim(idx3) ! xi
                            zre(idx3)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx3)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx4        = i+4*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx4) ! xr
                            zmm1.v(ii) = yre(idx4) ! yr
                            zmm2.v(ii) = xim(idx4) ! xi
                            zre(idx4)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx4)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx5        = i+5*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx5) ! xr
                            zmm1.v(ii) = yre(idx5) ! yr
                            zmm2.v(ii) = xim(idx5) ! xi
                            zre(idx5)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx5)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx6        = i+6*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx6) ! xr
                            zmm1.v(ii) = yre(idx6) ! yr
                            zmm2.v(ii) = xim(idx6) ! xi
                            zre(idx6)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx6)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx7        = i+7*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx7) ! xr
                            zmm1.v(ii) = yre(idx7) ! yr
                            zmm2.v(ii) = xim(idx7) ! xi
                            zre(idx7)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx7)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx8        = i+8*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx8) ! xr
                            zmm1.v(ii) = yre(idx8) ! yr
                            zmm2.v(ii) = xim(idx8) ! xi
                            zre(idx8)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx8)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx9        = i+9*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx9) ! xr
                            zmm1.v(ii) = yre(idx9) ! yr
                            zmm2.v(ii) = xim(idx9) ! xi
                            zre(idx9)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx9)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx10        = i+10*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx10) ! xr
                            zmm1.v(ii) = yre(idx10) ! yr
                            zmm2.v(ii) = xim(idx10) ! xi
                            zre(idx10)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx10)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx11        = i+11*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx11) ! xr
                            zmm1.v(ii) = yre(idx11) ! yr
                            zmm2.v(ii) = xim(idx11) ! xi
                            zre(idx11)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx11)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx12        = i+12*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx12) ! xr
                            zmm1.v(ii) = yre(idx12) ! yr
                            zmm2.v(ii) = xim(idx12) ! xi
                            zre(idx12)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx12)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx13        = i+13*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx13) ! xr
                            zmm1.v(ii) = yre(idx13) ! yr
                            zmm2.v(ii) = xim(idx13) ! xi
                            zre(idx13)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx13)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx14        = i+14*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx14) ! xr
                            zmm1.v(ii) = yre(idx14) ! yr
                            zmm2.v(ii) = xim(idx14) ! xi
                            zre(idx14)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx14)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx15        = i+15*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx15) ! xr
                            zmm1.v(ii) = yre(idx15) ! yr
                            zmm2.v(ii) = xim(idx15) ! xi
                            zre(idx15)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx15)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx16        = i+16*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx16) ! xr
                            zmm1.v(ii) = yre(idx16) ! yr
                            zmm2.v(ii) = xim(idx16) ! xi
                            zre(idx16)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx16)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx17        = i+17*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx17) ! xr
                            zmm1.v(ii) = yre(idx17) ! yr
                            zmm2.v(ii) = xim(idx17) ! xi
                            zre(idx17)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx17)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx18        = i+18*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx18) ! xr
                            zmm1.v(ii) = yre(idx18) ! yr
                            zmm2.v(ii) = xim(idx18) ! xi
                            zre(idx18)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx18)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx19        = i+19*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx19) ! xr
                            zmm1.v(ii) = yre(idx19) ! yr
                            zmm2.v(ii) = xim(idx19) ! xi
                            zre(idx19)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx19)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx20        = i+20*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx21) ! xr
                            zmm1.v(ii) = yre(idx21) ! yr
                            zmm2.v(ii) = xim(idx21) ! xi
                            zre(idx21)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx21)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx22        = i+22*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx22) ! xr
                            zmm1.v(ii) = yre(idx22) ! yr
                            zmm2.v(ii) = xim(idx22) ! xi
                            zre(idx22)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx22)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx23        = i+23*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx24) ! xr
                            zmm1.v(ii) = yre(idx24) ! yr
                            zmm2.v(ii) = xim(idx24) ! xi
                            zre(idx24)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx24)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx25        = i+25*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx25) ! xr
                            zmm1.v(ii) = yre(idx25) ! yr
                            zmm2.v(ii) = xim(idx25) ! xi
                            zre(idx25)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx25)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx26        = i+26*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx26) ! xr
                            zmm1.v(ii) = yre(idx26) ! yr
                            zmm2.v(ii) = xim(idx26) ! xi
                            zre(idx26)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx26)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx27        = i+27*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx27) ! xr
                            zmm1.v(ii) = yre(idx27) ! yr
                            zmm2.v(ii) = xim(idx27) ! xi
                            zre(idx27)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx27)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx28        = i+28*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx28) ! xr
                            zmm1.v(ii) = yre(idx28) ! yr
                            zmm2.v(ii) = xim(idx28) ! xi
                            zre(idx28)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx28)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx29        = i+29*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx29) ! xr
                            zmm1.v(ii) = yre(idx29) ! yr
                            zmm2.v(ii) = xim(idx29) ! xi
                            zre(idx29)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx29)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx30        = i+30*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx30) ! xr
                            zmm1.v(ii) = yre(idx30) ! yr
                            zmm2.v(ii) = xim(idx30) ! xi
                            zre(idx30)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx30)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx31        = i+31*ZMM_LEN+ii
                             mm0.v(ii) = xre(idx31) ! xr
                            zmm1.v(ii) = yre(idx31) ! yr
                            zmm2.v(ii) = xim(idx31) ! xi
                            zre(idx31)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx31)  = zmm2.v(ii)+zmm1.v(ii)  
                   end do
               end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                  do j = i, n-1   
                      xr = xre(j)
                      yr = yre(j)
                      xi = xim(j)
                      zre(j) = xr+yr
                      zim(j) = xi+yr
                  end do                   
                  return                  
       end if                   
end subroutine caddv_kernel_v512_cv_sv_32x16_ps


subroutine caddv_kernel_v512_cv_sv_16x16_ps(xre,xim,yre,zre,zim,n)
#if defined(__ICC) || defined(__INTEL_COMPILER)
        !DIR$ OPTIMIZE : 3    
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: caddv_kernel_v512_cv_sv_16x16_ps
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER:"target_arch=skylake-avx512" :: caddv_kernel_v512_cv_sv_16x16_ps
#endif     
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xre
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xim
         real(kind=sp), allocatable, dimension(:), intent(in)  :: yre
         real(kind=sp), allocatable, dimension(:), intent(out) :: zre
         real(kind=sp), allocatable, dimension(:), intent(out)  :: zim 
         integer(i4),                              intent(in)  :: n
         
         type(ZMM16r4_t), automatic :: zmm0
         type(ZMM16r4_t), automatic :: zmm1
         type(ZMM16r4_t), automatic :: zmm2
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm1
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm2
         type(XMM4r4_t),  automatic  :: xmm0
         type(XMM4r4_t),  automatic  :: xmm1
         type(XMM4r4_t),  automatic  :: xmm2
         type(YMM8r4_t),  automatic  :: ymm0 
         type(YMM8r4_t),  automatic  :: ymm1
         type(YMM8r4_t),  automatic  :: ymm2
         real(sp),        automatic  :: xr
         real(sp),        automatic  :: xi
         real(sp),        automatic  :: yr
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
            xi = xim(0)
            zre(0) = xr+yr
            zim(0) = xi+yr
            return
        else if(n>1 .and. n<=4) then
!$omp simd linear(i:1)
                 do i=0, 3
                    xmm0.v(i) = xre(i) ! xr
                    xmm1.v(i) = yre(i) ! yr
                    xmm2.v(i) = xim(i) ! xi
                    zre(i)  = xmm0.v(i)+xmm1.v(i)
                    zim(i)  = xmm2.v(i)+xmm1.v(i)
                 end do
                 return
        else if(n>4 .and. n<=8) then
!$omp simd linear(i:1)
                 do i=0, 7
                    ymm0.v(i) = xre(i) ! xr
                    ymm1.v(i) = yre(i) ! yr
                    ymm2.v(i) = xim(i) ! xi
                    zre(i)  = ymm0.v(i)+ymm1.v(i)
                    zim(i)  = ymm2.v(i)+ymm1.v(i)
                 end do
                 return    
        else if(n>8 .and. n<=16) then
!$omp simd linear(i:1)
                 do i=0, 15
                    zmm0.v(i) = xre(i) ! xr
                    zmm1.v(i) = yre(i) ! yr
                    zmm2.v(i) = xim(i) ! xi
                    zre(i)  = zmm0.v(i)+zmm1.v(i)
                    zim(i)  = zmm2.v(i)+zmm1.v(i)
                 end do
                 return 
        else if(n>16 .and. n<=64) then
                   do i = 0,iand(n-1,inot(15)),16
!$omp simd aligned(xim,xre,yre,zre,zim:64) linear(ii:1)
                       do ii = 0, 15  
                            zmm0.v(ii) = xre(i+ii) ! xr
                            zmm1.v(ii) = yre(i+ii) ! yr
                            zmm2.v(ii) = xim(i+ii) ! xi
                            zre(i+ii)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(i+ii)  = zmm2.v(ii)+zmm1.v(ii)  
                       end do
                   end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                  do j = i, n-1   
                      xr = xre(j)
                      yr = yre(j)
                      xi = xim(j)
                      zre(j) = xr+yr
                      zim(j) = xi+yr
                  end do                   
                  return
       else if(n>64 .and. n<=128) then
                   do i = 0,iand(n-1,inot(15)),16
!$omp simd aligned(xim,xre,yre,zre,zim:64) linear(ii:1)
                       do ii = 0, 15  
                            zmm0.v(ii) = xre(i+ii) ! xr
                            zmm1.v(ii) = yre(i+ii) ! yr
                            zmm2.v(ii) = xim(i+ii) ! xi
                            zre(i+ii)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(i+ii)  = zmm2.v(ii)+zmm1.v(ii)  
                       end do
                   end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                  do j = i, n-1   
                      xr = xre(j)
                      yr = yre(j)
                      xi = xim(j)
                      zre(j) = xr+yr
                      zim(j) = xi+yr
                  end do                   
                  return   
       else if(n>128) then
               do i=0, iand(n-1,inot(ZMM_LEN-1)), ZMM_LEN*16
                   call mm_prefetch(xre(i+16*ZMM_LEN),FOR_K_PREFETCH_T1)
                   call mm_prefetch(xim(i+16*ZMM_LEN),FOR_K_PREFETCH_T1)
                   call mm_prefetch(yre(i+16*ZMM_LEN),FOR_K_PREFETCH_T1)
#if defined(__ICC) || defined(__INTEL_COMPILER)
                   !dir$ assume_aligned  xre:64
                   !dir$ assume_aligned  xim:64
                   !dir$ assume_aligned  yre:64
                   !dir$ assume_aligned  zre:64
                   !dir$ assume_aligned  zim:64
#endif                   
!$omp simd aligned(xim,xre,yre,zre,zim:64)  linear(ii:1)              
                      do ii = 0, ZMM_LEN-1  
                            zmm0.v(ii) = xre(i+0+ii) ! xr
                            zmm1.v(ii) = yre(i+0+ii) ! yr
                            zmm2.v(ii) = xim(i+0+ii) ! xi
                            zre(i+0+ii)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(i+0+ii)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx1        = i+1*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx1) ! xr
                            zmm1.v(ii) = yre(idx1) ! yr
                            zmm2.v(ii) = xim(idx1) ! xi
                            zre(idx1)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx1)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx2        = i+2*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx2) ! xr
                            zmm1.v(ii) = yre(idx2) ! yr
                            zmm2.v(ii) = xim(idx2) ! xi
                            zre(idx2)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx2)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx3        = i+3*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx3) ! xr
                            zmm1.v(ii) = yre(idx3) ! yr
                            zmm2.v(ii) = xim(idx3) ! xi
                            zre(idx3)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx3)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx4        = i+4*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx4) ! xr
                            zmm1.v(ii) = yre(idx4) ! yr
                            zmm2.v(ii) = xim(idx4) ! xi
                            zre(idx4)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx4)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx5        = i+5*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx5) ! xr
                            zmm1.v(ii) = yre(idx5) ! yr
                            zmm2.v(ii) = xim(idx5) ! xi
                            zre(idx5)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx5)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx6        = i+6*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx6) ! xr
                            zmm1.v(ii) = yre(idx6) ! yr
                            zmm2.v(ii) = xim(idx6) ! xi
                            zre(idx6)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx6)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx7        = i+7*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx7) ! xr
                            zmm1.v(ii) = yre(idx7) ! yr
                            zmm2.v(ii) = xim(idx7) ! xi
                            zre(idx7)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx7)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx8        = i+8*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx8) ! xr
                            zmm1.v(ii) = yre(idx8) ! yr
                            zmm2.v(ii) = xim(idx8) ! xi
                            zre(idx8)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx8)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx9        = i+9*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx9) ! xr
                            zmm1.v(ii) = yre(idx9) ! yr
                            zmm2.v(ii) = xim(idx9) ! xi
                            zre(idx9)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx9)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx10        = i+10*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx10) ! xr
                            zmm1.v(ii) = yre(idx10) ! yr
                            zmm2.v(ii) = xim(idx10) ! xi
                            zre(idx10)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx10)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx11        = i+11*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx11) ! xr
                            zmm1.v(ii) = yre(idx11) ! yr
                            zmm2.v(ii) = xim(idx11) ! xi
                            zre(idx11)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx11)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx12        = i+12*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx12) ! xr
                            zmm1.v(ii) = yre(idx12) ! yr
                            zmm2.v(ii) = xim(idx12) ! xi
                            zre(idx12)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx12)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx13        = i+13*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx13) ! xr
                            zmm1.v(ii) = yre(idx13) ! yr
                            zmm2.v(ii) = xim(idx13) ! xi
                            zre(idx13)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx13)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx14        = i+14*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx14) ! xr
                            zmm1.v(ii) = yre(idx14) ! yr
                            zmm2.v(ii) = xim(idx14) ! xi
                            zre(idx14)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx14)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx15        = i+15*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx15) ! xr
                            zmm1.v(ii) = yre(idx15) ! yr
                            zmm2.v(ii) = xim(idx15) ! xi
                            zre(idx15)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx15)  = zmm2.v(ii)+zmm1.v(ii)  
                         
                   end do
               end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                  do j = i, n-1   
                      xr = xre(j)
                      yr = yre(j)
                      xi = xim(j)
                      zre(j) = xr+yr
                      zim(j) = xi+yr
                  end do                   
                  return                  
       end if                   
end subroutine caddv_kernel_v512_cv_sv_16x16_ps

 
subroutine caddv_kernel_v512_cv_sv_8x16_ps(xre,xim,yre,zre,zim,n)
#if defined(__ICC) || defined(__INTEL_COMPILER)    
        !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: caddv_kernel_v512_cv_sv_8x16_ps
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER:"target_arch=skylake-avx512" :: caddv_kernel_v512_cv_sv_8x16_ps
#endif     
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xre
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xim
         real(kind=sp), allocatable, dimension(:), intent(in)  :: yre
         real(kind=sp), allocatable, dimension(:), intent(out) :: zre
         real(kind=sp), allocatable, dimension(:), intent(out)  :: zim 
         integer(i4),                              intent(in)  :: n
         
         type(ZMM16r4_t), automatic :: zmm0
         type(ZMM16r4_t), automatic :: zmm1
         type(ZMM16r4_t), automatic :: zmm2
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm1
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm2
         type(XMM4r4_t),  automatic  :: xmm0
         type(XMM4r4_t),  automatic  :: xmm1
         type(XMM4r4_t),  automatic  :: xmm2
         type(YMM8r4_t),  automatic  :: ymm0 
         type(YMM8r4_t),  automatic  :: ymm1
         type(YMM8r4_t),  automatic  :: ymm2
         real(sp),        automatic  :: xr
         real(sp),        automatic  :: xi
         real(sp),        automatic  :: yr
         integer(i4),     automatic  :: i,ii,j
         integer(i4),     automatic  :: idx1,idx2,idx3,idx4
         integer(i4),     automatic  :: idx5,idx6,idx7
 
         if(n<=0) then
            return
         else if(n==1) then
            xr = xre(0)
            yr = yre(0)
            xi = xim(0)
            zre(0) = xr+yr
            zim(0) = xi+yr
            return
        else if(n>1 .AND. n<=4) then
!$omp simd linear(i:1)
                 do i=0, 3
                    xmm0.v(i) = xre(i) ! xr
                    xmm1.v(i) = yre(i) ! yr
                    xmm2.v(i) = xim(i) ! xi
                    zre(i)  = xmm0.v(i)+xmm1.v(i)
                    zim(i)  = xmm2.v(i)+xmm1.v(i)
                 end do
                 return
        else if(n>4 .AND. n<=8) then
!$omp simd linear(i:1)
                 do i=0, 7
                    ymm0.v(i) = xre(i) ! xr
                    ymm1.v(i) = yre(i) ! yr
                    ymm2.v(i) = xim(i) ! xi
                    zre(i)  = ymm0.v(i)+ymm1.v(i)
                    zim(i)  = ymm2.v(i)+ymm1.v(i)
                 end do
                 return    
        else if(n>8 .AND. n<=16) then
!$omp simd linear(i:1)
                 do i=0, 15
                    zmm0.v(i) = xre(i) ! xr
                    zmm1.v(i) = yre(i) ! yr
                    zmm2.v(i) = xim(i) ! xi
                    zre(i)  = zmm0.v(i)+zmm1.v(i)
                    zim(i)  = zmm2.v(i)+zmm1.v(i)
                 end do
                 return 
        else if(n>16 .AND. n<=64) then
                   do i = 0,iand(n-1,inot(15)),16
!$omp simd aligned(xim,xre,yre,zre,zim:64) linear(ii:1)
                       do ii = 0, 15  
                            zmm0.v(ii) = xre(i+ii) ! xr
                            zmm1.v(ii) = yre(i+ii) ! yr
                            zmm2.v(ii) = xim(i+ii) ! xi
                            zre(i+ii)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(i+ii)  = zmm2.v(ii)+zmm1.v(ii)  
                       end do
                   end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                  do j = i, n-1   
                      xr = xre(j)
                      yr = yre(j)
                      xi = xim(j)
                      zre(j) = xr+yr
                      zim(j) = xi+yr
                  end do                   
                  return
          else if(n>64) then
               do i=0, iand(n-1,inot(ZMM_LEN-1)), ZMM_LEN*8
                   call mm_prefetch(xre(i+8*ZMM_LEN),FOR_K_PREFETCH_T1)
                   call mm_prefetch(xim(i+8*ZMM_LEN),FOR_K_PREFETCH_T1)
                   call mm_prefetch(yre(i+8*ZMM_LEN),FOR_K_PREFETCH_T1)
#if defined(__ICC) || defined(__INTEL_COMPILER)
                   !dir$ assume_aligned  xre:64
                   !dir$ assume_aligned  xim:64
                   !dir$ assume_aligned  yre:64
                   !dir$ assume_aligned  zre:64
                   !dir$ assume_aligned  zim:64
#endif                   
!$omp simd aligned(xim,xre,yre,zre,zim:64)  linear(ii:1)              
                      do ii = 0, ZMM_LEN-1  
                            zmm0.v(ii) = xre(i+0+ii) ! xr
                            zmm1.v(ii) = yre(i+0+ii) ! yr
                            zmm2.v(ii) = xim(i+0+ii) ! xi
                            zre(i+0+ii)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(i+0+ii)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx1        = i+1*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx1) ! xr
                            zmm1.v(ii) = yre(idx1) ! yr
                            zmm2.v(ii) = xim(idx1) ! xi
                            zre(idx1)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx1)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx2        = i+2*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx2) ! xr
                            zmm1.v(ii) = yre(idx2) ! yr
                            zmm2.v(ii) = xim(idx2) ! xi
                            zre(idx2)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx2)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx3        = i+3*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx3) ! xr
                            zmm1.v(ii) = yre(idx3) ! yr
                            zmm2.v(ii) = xim(idx3) ! xi
                            zre(idx3)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx3)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx4        = i+4*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx4) ! xr
                            zmm1.v(ii) = yre(idx4) ! yr
                            zmm2.v(ii) = xim(idx4) ! xi
                            zre(idx4)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx4)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx5        = i+5*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx5) ! xr
                            zmm1.v(ii) = yre(idx5) ! yr
                            zmm2.v(ii) = xim(idx5) ! xi
                            zre(idx5)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx5)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx6        = i+6*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx6) ! xr
                            zmm1.v(ii) = yre(idx6) ! yr
                            zmm2.v(ii) = xim(idx6) ! xi
                            zre(idx6)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx6)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx7        = i+7*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx7) ! xr
                            zmm1.v(ii) = yre(idx7) ! yr
                            zmm2.v(ii) = xim(idx7) ! xi
                            zre(idx7)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx7)  = zmm2.v(ii)+zmm1.v(ii)  
                                      
                   end do
               end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                  do j = i, n-1   
                      xr = xre(j)
                      yr = yre(j)
                      xi = xim(j)
                      zre(j) = xr+yr
                      zim(j) = xi+yr
                  end do                   
                  return                  
       end if                   
end subroutine caddv_kernel_v512_cv_sv_8x16_ps
 
 
subroutine caddv_kernel_v512_cv_sv_4x16_ps(xre,xim,yre,zre,zim,n)
#if defined(__ICC) || defined(__INTEL_COMPILER)    
       !DIR$ OPTIMIZE : 3
       !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: caddv_kernel_v512_cv_sv_4x16_ps
       !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER:"target_arch=skylake-avx512" :: caddv_kernel_v512_cv_sv_4x16_ps
#endif     
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xre
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xim
         real(kind=sp), allocatable, dimension(:), intent(in)  :: yre
         real(kind=sp), allocatable, dimension(:), intent(out) :: zre
         real(kind=sp), allocatable, dimension(:), intent(out)  :: zim 
         integer(i4),                              intent(in)  :: n
         
         type(ZMM16r4_t), automatic :: zmm0
         type(ZMM16r4_t), automatic :: zmm1
         type(ZMM16r4_t), automatic :: zmm2
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm1
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm2
         type(XMM4r4_t),  automatic  :: xmm0
         type(XMM4r4_t),  automatic  :: xmm1
         type(XMM4r4_t),  automatic  :: xmm2
         type(YMM8r4_t),  automatic  :: ymm0 
         type(YMM8r4_t),  automatic  :: ymm1
         type(YMM8r4_t),  automatic  :: ymm2
         real(sp),        automatic  :: xr
         real(sp),        automatic  :: xi
         real(sp),        automatic  :: yr
         integer(i4),     automatic  :: i,ii,j
         integer(i4),     automatic  :: idx1,idx2,idx3
 
         if(n<=0) then
            return
         else if(n==1) then
            xr = xre(0)
            yr = yre(0)
            xi = xim(0)
            zre(0) = xr+yr
            zim(0) = xi+yr
            return
        else if(n>1 .AND. n<=4) then
!$omp simd linear(i:1)
                 do i=0, 3
                    xmm0.v(i) = xre(i) ! xr
                    xmm1.v(i) = yre(i) ! yr
                    xmm2.v(i) = xim(i) ! xi
                    zre(i)  = xmm0.v(i)+xmm1.v(i)
                    zim(i)  = xmm2.v(i)+xmm1.v(i)
                 end do
                 return
        else if(n>4 .AND. n<=8) then
!$omp simd linear(i:1)
                 do i=0, 7
                    ymm0.v(i) = xre(i) ! xr
                    ymm1.v(i) = yre(i) ! yr
                    ymm2.v(i) = xim(i) ! xi
                    zre(i)  = ymm0.v(i)+ymm1.v(i)
                    zim(i)  = ymm2.v(i)+ymm1.v(i)
                 end do
                 return    
        else if(n>8 .AND. n<=16) then
!$omp simd linear(i:1)
                 do i=0, 15
                    zmm0.v(i) = xre(i) ! xr
                    zmm1.v(i) = yre(i) ! yr
                    zmm2.v(i) = xim(i) ! xi
                    zre(i)  = zmm0.v(i)+zmm1.v(i)
                    zim(i)  = zmm2.v(i)+zmm1.v(i)
                 end do
                 return 
        else if(n>16 .AND. n<=32) then
                   do i = 0,iand(n-1,inot(15)),16
!$omp simd aligned(xim,xre,yre,zre,zim:64) linear(ii:1)
                       do ii = 0, 15  
                            zmm0.v(ii) = xre(i+ii) ! xr
                            zmm1.v(ii) = yre(i+ii) ! yr
                            zmm2.v(ii) = xim(i+ii) ! xi
                            zre(i+ii)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(i+ii)  = zmm2.v(ii)+zmm1.v(ii)  
                       end do
                   end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                  do j = i, n-1   
                      xr = xre(j)
                      yr = yre(j)
                      xi = xim(j)
                      zre(j) = xr+yr
                      zim(j) = xi+yr
                  end do                   
                  return
          else if(n>32) then
               do i=0, iand(n-1,inot(ZMM_LEN-1)), ZMM_LEN*4
                   call mm_prefetch(xre(i+4*ZMM_LEN),FOR_K_PREFETCH_T1)
                   call mm_prefetch(xim(i+4*ZMM_LEN),FOR_K_PREFETCH_T1)
                   call mm_prefetch(yre(i+4*ZMM_LEN),FOR_K_PREFETCH_T1)
#if defined(__ICC) || defined(__INTEL_COMPILER)
                   !dir$ assume_aligned  xre:64
                   !dir$ assume_aligned  xim:64
                   !dir$ assume_aligned  yre:64
                   !dir$ assume_aligned  zre:64
                   !dir$ assume_aligned  zim:64
#endif                   
!$omp simd aligned(xim,xre,yre,zre,zim:64)  linear(ii:1)              
                      do ii = 0, ZMM_LEN-1  
                            zmm0.v(ii) = xre(i+0+ii) ! xr
                            zmm1.v(ii) = yre(i+0+ii) ! yr
                            zmm2.v(ii) = xim(i+0+ii) ! xi
                            zre(i+0+ii)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(i+0+ii)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx1        = i+1*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx1) ! xr
                            zmm1.v(ii) = yre(idx1) ! yr
                            zmm2.v(ii) = xim(idx1) ! xi
                            zre(idx1)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx1)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx2        = i+2*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx2) ! xr
                            zmm1.v(ii) = yre(idx2) ! yr
                            zmm2.v(ii) = xim(idx2) ! xi
                            zre(idx2)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx2)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx3        = i+3*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx3) ! xr
                            zmm1.v(ii) = yre(idx3) ! yr
                            zmm2.v(ii) = xim(idx3) ! xi
                            zre(idx3)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx3)  = zmm2.v(ii)+zmm1.v(ii)  
                                                                
                   end do
               end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                  do j = i, n-1   
                      xr = xre(j)
                      yr = yre(j)
                      xi = xim(j)
                      zre(j) = xr+yr
                      zim(j) = xi+yr
                  end do                   
                  return                  
       end if                   
end subroutine caddv_kernel_v512_cv_sv_4x16_ps
  
 
subroutine caddv_kernel_v512_cv_s_32x16_ps(xre,xim,re,zre,zim,n)
#if defined(__ICC) || defined(__INTEL_COMPILER)   
         !DIR$ OPTIMIZE : 3 
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: caddv_kernel_v512_cv_s_32x16_ps
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: "target_arch=skylake-avx512" :: caddv_kernel_v512_cv_s_32x16_ps
#endif     
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xre
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xim
         real(kind=sp),                            intent(in)  :: re
         real(kind=sp), allocatable, dimension(:), intent(out) :: zre
         real(kind=sp), allocatable, dimension(:), intent(out) :: zim 
         integer(i4),                              intent(in)  :: n
         
         type(ZMM16r4_t), automatic :: zmm0
         type(ZMM16r4_t), automatic :: zmm1
         type(ZMM16r4_t), automatic :: zmm2
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm1
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm2
         type(XMM4r4_t),  automatic  :: xmm0
         type(XMM4r4_t),  automatic  :: xmm1
         type(XMM4r4_t),  automatic  :: xmm2
         type(YMM8r4_t),  automatic  :: ymm0 
         type(YMM8r4_t),  automatic  :: ymm1
         type(YMM8r4_t),  automatic  :: ymm2
         real(sp),        automatic  :: xr
         real(sp),        automatic  :: xi
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
            xr = xre(0)
            xi = xim(0)
            zre(0) = xr+re
            zim(0) = xi+re
            return
        else if(n>1 .AND. n<=4) then
!$omp simd linear(i:1)
                 do i=0, 3
                    xmm0.v(i) = xre(i) ! xr
                    xmm1.v(i) = re ! re
                    xmm2.v(i) = xim(i) ! xi
                    zre(i)  = xmm0.v(i)+xmm1.v(i)
                    zim(i)  = xmm2.v(i)+xmm1.v(i)
                 end do
                 return
        else if(n>4 .AND. n<=8) then
!$omp simd linear(i:1)
                 do i=0, 7
                    ymm0.v(i) = xre(i) ! xr
                    ymm1.v(i) = re ! re
                    ymm2.v(i) = xim(i) ! xi
                    zre(i)  = ymm0.v(i)+ymm1.v(i)
                    zim(i)  = ymm2.v(i)+ymm1.v(i)
                 end do
                 return    
        else if(n>8 .AND. n<=16) then
!$omp simd linear(i:1)
                 do i=0, 15
                    zmm0.v(i) = xre(i) ! xr
                    zmm1.v(i) = re ! re
                    zmm2.v(i) = xim(i) ! xi
                    zre(i)  = zmm0.v(i)+zmm1.v(i)
                    zim(i)  = zmm2.v(i)+zmm1.v(i)
                 end do
                 return 
        else if(n>16 .AND. n<=64) then
                   zmm1.v = re ! re
                   do i = 0,iand(n-1,inot(15)),16
!$omp simd aligned(xim,xre,yre,zre,zim:64) linear(ii:1)
                       do ii = 0, 15  
                            zmm0.v(ii) = xre(i+ii) ! xr
                            zmm2.v(ii) = xim(i+ii) ! xi
                            zre(i+ii)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(i+ii)  = zmm2.v(ii)+zmm1.v(ii)  
                       end do
                   end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                  do j = i, n-1   
                      xr = xre(j)
                      xi = xim(j)
                      zre(j) = xr+re
                      zim(j) = xi+re
                  end do                   
                  return
       else if(n>64 .AND. n<=128) then
                   zmm1.v = re ! re
                   do i = 0,iand(n-1,inot(15)),16
!$omp simd aligned(xim,xre,yre,zre,zim:64) linear(ii:1)
                       do ii = 0, 15  
                            zmm0.v(ii) = xre(i+ii) ! xr
                            zmm2.v(ii) = xim(i+ii) ! xi
                            zre(i+ii)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(i+ii)  = zmm2.v(ii)+zmm1.v(ii)  
                       end do
                   end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                  do j = i, n-1   
                      xr = xre(j)
                      xi = xim(j)
                      zre(j) = xr+re
                      zim(j) = xi+re
                  end do                   
                  return   
       else if(n>128) then
               zmm1.v = re ! re
               do i=0, iand(n-1,inot(ZMM_LEN-1)), ZMM_LEN*32
                   call mm_prefetch(xre(i+32*ZMM_LEN),FOR_K_PREFETCH_T1)
                   call mm_prefetch(xim(i+32*ZMM_LEN),FOR_K_PREFETCH_T1)
#if defined(__ICC) || defined(__INTEL_COMPILER)
                   !dir$ assume_aligned  xre:64
                   !dir$ assume_aligned  xim:64
                    !dir$ assume_aligned  zre:64
                   !dir$ assume_aligned  zim:64
#endif                   
!$omp simd aligned(xim,xre,yre,zre,zim:64)  linear(ii:1)              
                      do ii = 0, ZMM_LEN-1  
                            zmm0.v(ii) = xre(i+0+ii) ! xr
                            zmm2.v(ii) = xim(i+0+ii) ! xi
                            zre(i+0+ii)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(i+0+ii)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx1        = i+1*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx1) ! xr
                            zmm2.v(ii) = xim(idx1) ! xi
                            zre(idx1)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx1)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx2        = i+2*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx2) ! xr
                            zmm2.v(ii) = xim(idx2) ! xi
                            zre(idx2)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx2)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx3        = i+3*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx3) ! xr
                            zmm2.v(ii) = xim(idx3) ! xi
                            zre(idx3)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx3)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx4        = i+4*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx4) ! xr
                            zmm2.v(ii) = xim(idx4) ! xi
                            zre(idx4)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx4)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx5        = i+5*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx5) ! xr
                            zmm2.v(ii) = xim(idx5) ! xi
                            zre(idx5)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx5)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx6        = i+6*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx6) ! xr
                            zmm2.v(ii) = xim(idx6) ! xi
                            zre(idx6)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx6)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx7        = i+7*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx7) ! xr
                            zmm2.v(ii) = xim(idx7) ! xi
                            zre(idx7)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx7)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx8        = i+8*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx8) ! xr
                            zmm2.v(ii) = xim(idx8) ! xi
                            zre(idx8)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx8)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx9        = i+9*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx9) ! xr
                            zmm2.v(ii) = xim(idx9) ! xi
                            zre(idx9)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx9)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx10        = i+10*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx10) ! xr
                            zmm2.v(ii) = xim(idx10) ! xi
                            zre(idx10)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx10)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx11        = i+11*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx11) ! xr
                            zmm2.v(ii) = xim(idx11) ! xi
                            zre(idx11)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx11)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx12        = i+12*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx12) ! xr
                            zmm2.v(ii) = xim(idx12) ! xi
                            zre(idx12)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx12)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx13        = i+13*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx13) ! xr
                            zmm2.v(ii) = xim(idx13) ! xi
                            zre(idx13)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx13)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx14        = i+14*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx14) ! xr
                            zmm2.v(ii) = xim(idx14) ! xi
                            zre(idx14)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx14)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx15        = i+15*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx15) ! xr
                            zmm2.v(ii) = xim(idx15) ! xi
                            zre(idx15)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx15)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx16        = i+16*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx16) ! xr
                            zmm2.v(ii) = xim(idx16) ! xi
                            zre(idx16)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx16)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx17        = i+17*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx17) ! xr
                            zmm2.v(ii) = xim(idx17) ! xi
                            zre(idx17)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx17)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx18        = i+18*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx18) ! xr
                            zmm2.v(ii) = xim(idx18) ! xi
                            zre(idx18)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx18)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx19        = i+19*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx19) ! xr
                            zmm2.v(ii) = xim(idx19) ! xi
                            zre(idx19)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx19)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx20        = i+20*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx21) ! xr
                            zmm2.v(ii) = xim(idx21) ! xi
                            zre(idx21)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx21)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx22        = i+22*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx22) ! xr
                            zmm2.v(ii) = xim(idx22) ! xi
                            zre(idx22)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx22)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx23        = i+23*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx24) ! xr
                            zmm2.v(ii) = xim(idx24) ! xi
                            zre(idx24)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx24)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx25        = i+25*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx25) ! xr
                            zmm2.v(ii) = xim(idx25) ! xi
                            zre(idx25)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx25)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx26        = i+26*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx26) ! xr
                            zmm2.v(ii) = xim(idx26) ! xi
                            zre(idx26)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx26)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx27        = i+27*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx27) ! xr
                            zmm2.v(ii) = xim(idx27) ! xi
                            zre(idx27)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx27)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx28        = i+28*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx28) ! xr
                            zmm2.v(ii) = xim(idx28) ! xi
                            zre(idx28)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx28)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx29        = i+29*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx29) ! xr
                            zmm2.v(ii) = xim(idx29) ! xi
                            zre(idx29)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx29)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx30        = i+30*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx30) ! xr
                            zmm2.v(ii) = xim(idx30) ! xi
                            zre(idx30)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx30)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx31        = i+31*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx31) ! xr
                            zmm2.v(ii) = xim(idx31) ! xi
                            zre(idx31)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx31)  = zmm2.v(ii)+zmm1.v(ii)  
                   end do
               end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                  do j = i, n-1   
                      xr = xre(j)
                      xi = xim(j)
                      zre(j) = xr+re
                      zim(j) = xi+re
                  end do                   
                  return                  
       end if                   
end subroutine caddv_kernel_v512_cv_s_32x16_ps
 
 
subroutine caddv_kernel_v512_cv_s_16x16_ps(xre,xim,re,zre,zim,n)
#if defined(__ICC) || defined(__INTEL_COMPILER)  
        !DIR$ OPTIMIZE : 3  
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: caddv_kernel_v512_cv_s_16x16_ps
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: "TARGET_ARCH=skylake-avx512" :: caddv_kernel_v512_cv_s_16x16_ps
#endif     
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xre
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xim
         real(kind=sp),                            intent(in)  :: re
         real(kind=sp), allocatable, dimension(:), intent(out) :: zre
         real(kind=sp), allocatable, dimension(:), intent(out) :: zim 
         integer(i4),                              intent(in)  :: n
         
         type(ZMM16r4_t), automatic :: zmm0
         type(ZMM16r4_t), automatic :: zmm1
         type(ZMM16r4_t), automatic :: zmm2
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm1
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm2
         type(XMM4r4_t),  automatic  :: xmm0
         type(XMM4r4_t),  automatic  :: xmm1
         type(XMM4r4_t),  automatic  :: xmm2
         type(YMM8r4_t),  automatic  :: ymm0 
         type(YMM8r4_t),  automatic  :: ymm1
         type(YMM8r4_t),  automatic  :: ymm2
         real(sp),        automatic  :: xr
         real(sp),        automatic  :: xi
         integer(i4),     automatic  :: i,ii,j
         integer(i4),     automatic  :: idx1,idx2,idx3,idx4
         integer(i4),     automatic  :: idx5,idx6,idx7,idx8
         integer(i4),     automatic  :: idx9,idx10,idx11,idx12 
         integer(i4),     automatic  :: idx13,idx14,idx15
 
         if(n<=0) then
            return
         else if(n==1) then
            xr = xre(0)
            xi = xim(0)
            zre(0) = xr+re
            zim(0) = xi+re
            return
        else if(n>1 .AND. n<=4) then
!$omp simd linear(i:1)
                 do i=0, 3
                    xmm0.v(i) = xre(i) ! xr
                    xmm1.v(i) = re ! re
                    xmm2.v(i) = xim(i) ! xi
                    zre(i)  = xmm0.v(i)+xmm1.v(i)
                    zim(i)  = xmm2.v(i)+xmm1.v(i)
                 end do
                 return
        else if(n>4 .AND. n<=8) then
!$omp simd linear(i:1)
                 do i=0, 7
                    ymm0.v(i) = xre(i) ! xr
                    ymm1.v(i) = re ! re
                    ymm2.v(i) = xim(i) ! xi
                    zre(i)  = ymm0.v(i)+ymm1.v(i)
                    zim(i)  = ymm2.v(i)+ymm1.v(i)
                 end do
                 return    
        else if(n>8 .AND. n<=16) then
!$omp simd linear(i:1)
                 do i=0, 15
                    zmm0.v(i) = xre(i) ! xr
                    zmm1.v(i) = re ! re
                    zmm2.v(i) = xim(i) ! xi
                    zre(i)  = zmm0.v(i)+zmm1.v(i)
                    zim(i)  = zmm2.v(i)+zmm1.v(i)
                 end do
                 return 
        else if(n>16 .AND. n<=64) then
                   zmm1.v = re ! re
                   do i = 0,iand(n-1,inot(15)),16
!$omp simd aligned(xim,xre,yre,zre,zim:64) linear(ii:1)
                       do ii = 0, 15  
                            zmm0.v(ii) = xre(i+ii) ! xr
                            zmm2.v(ii) = xim(i+ii) ! xi
                            zre(i+ii)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(i+ii)  = zmm2.v(ii)+zmm1.v(ii)  
                       end do
                   end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                  do j = i, n-1   
                      xr = xre(j)
                      xi = xim(j)
                      zre(j) = xr+re
                      zim(j) = xi+re
                  end do                   
                  return
       else if(n>64) then
               zmm1.v = re ! re
               do i=0, iand(n-1,inot(ZMM_LEN-1)), ZMM_LEN*16
                   call mm_prefetch(xre(i+16*ZMM_LEN),FOR_K_PREFETCH_T1)
                   call mm_prefetch(xim(i+16*ZMM_LEN),FOR_K_PREFETCH_T1)
#if defined(__ICC) || defined(__INTEL_COMPILER)
                   !dir$ assume_aligned  xre:64
                   !dir$ assume_aligned  xim:64
                    !dir$ assume_aligned  zre:64
                   !dir$ assume_aligned  zim:64
#endif                   
!$omp simd aligned(xim,xre,yre,zre,zim:64)  linear(ii:1)              
                      do ii = 0, ZMM_LEN-1  
                            zmm0.v(ii) = xre(i+0+ii) ! xr
                            zmm2.v(ii) = xim(i+0+ii) ! xi
                            zre(i+0+ii)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(i+0+ii)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx1        = i+1*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx1) ! xr
                            zmm2.v(ii) = xim(idx1) ! xi
                            zre(idx1)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx1)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx2        = i+2*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx2) ! xr
                            zmm2.v(ii) = xim(idx2) ! xi
                            zre(idx2)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx2)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx3        = i+3*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx3) ! xr
                            zmm2.v(ii) = xim(idx3) ! xi
                            zre(idx3)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx3)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx4        = i+4*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx4) ! xr
                            zmm2.v(ii) = xim(idx4) ! xi
                            zre(idx4)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx4)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx5        = i+5*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx5) ! xr
                            zmm2.v(ii) = xim(idx5) ! xi
                            zre(idx5)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx5)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx6        = i+6*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx6) ! xr
                            zmm2.v(ii) = xim(idx6) ! xi
                            zre(idx6)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx6)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx7        = i+7*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx7) ! xr
                            zmm2.v(ii) = xim(idx7) ! xi
                            zre(idx7)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx7)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx8        = i+8*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx8) ! xr
                            zmm2.v(ii) = xim(idx8) ! xi
                            zre(idx8)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx8)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx9        = i+9*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx9) ! xr
                            zmm2.v(ii) = xim(idx9) ! xi
                            zre(idx9)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx9)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx10        = i+10*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx10) ! xr
                            zmm2.v(ii) = xim(idx10) ! xi
                            zre(idx10)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx10)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx11        = i+11*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx11) ! xr
                            zmm2.v(ii) = xim(idx11) ! xi
                            zre(idx11)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx11)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx12        = i+12*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx12) ! xr
                            zmm2.v(ii) = xim(idx12) ! xi
                            zre(idx12)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx12)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx13        = i+13*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx13) ! xr
                            zmm2.v(ii) = xim(idx13) ! xi
                            zre(idx13)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx13)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx14        = i+14*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx14) ! xr
                            zmm2.v(ii) = xim(idx14) ! xi
                            zre(idx14)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx14)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx15        = i+15*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx15) ! xr
                            zmm2.v(ii) = xim(idx15) ! xi
                            zre(idx15)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx15)  = zmm2.v(ii)+zmm1.v(ii)  
                   end do
               end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                  do j = i, n-1   
                      xr = xre(j)
                      xi = xim(j)
                      zre(j) = xr+re
                      zim(j) = xi+re
                  end do                   
                  return                  
       end if                   
end subroutine caddv_kernel_v512_cv_s_16x16_ps 
 
 
subroutine caddv_kernel_v512_cv_s_8x16_ps(xre,xim,re,zre,zim,n)
#if defined(__ICC) || defined(__INTEL_COMPILER)    
          !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: caddv_kernel_v512_cv_s_8x16_ps
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: "TARGET_ARCH=skylake-avx512" :: caddv_kernel_v512_cv_s_8x16_ps
#endif     
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xre
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xim
         real(kind=sp),                            intent(in)  :: re
         real(kind=sp), allocatable, dimension(:), intent(out) :: zre
         real(kind=sp), allocatable, dimension(:), intent(out) :: zim 
         integer(i4),                              intent(in)  :: n
         
         type(ZMM16r4_t), automatic :: zmm0
         type(ZMM16r4_t), automatic :: zmm1
         type(ZMM16r4_t), automatic :: zmm2
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm1
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm2
         type(XMM4r4_t),  automatic  :: xmm0
         type(XMM4r4_t),  automatic  :: xmm1
         type(XMM4r4_t),  automatic  :: xmm2
         type(YMM8r4_t),  automatic  :: ymm0 
         type(YMM8r4_t),  automatic  :: ymm1
         type(YMM8r4_t),  automatic  :: ymm2
         real(sp),        automatic  :: xr
         real(sp),        automatic  :: xi
         integer(i4),     automatic  :: i,ii,j
         integer(i4),     automatic  :: idx1,idx2,idx3,idx4
         integer(i4),     automatic  :: idx5,idx6,idx7
 
         if(n<=0) then
            return
         else if(n==1) then
            xr = xre(0)
            xi = xim(0)
            zre(0) = xr+re
            zim(0) = xi+re
            return
        else if(n>1 .AND. n<=4) then
!$omp simd linear(i:1)
                 do i=0, 3
                    xmm0.v(i) = xre(i) ! xr
                    xmm1.v(i) = re ! re
                    xmm2.v(i) = xim(i) ! xi
                    zre(i)  = xmm0.v(i)+xmm1.v(i)
                    zim(i)  = xmm2.v(i)+xmm1.v(i)
                 end do
                 return
        else if(n>4 .AND. n<=8) then
!$omp simd linear(i:1)
                 do i=0, 7
                    ymm0.v(i) = xre(i) ! xr
                    ymm1.v(i) = re ! re
                    ymm2.v(i) = xim(i) ! xi
                    zre(i)  = ymm0.v(i)+ymm1.v(i)
                    zim(i)  = ymm2.v(i)+ymm1.v(i)
                 end do
                 return    
        else if(n>8 .AND. n<=16) then
!$omp simd linear(i:1)
                 do i=0, 15
                    zmm0.v(i) = xre(i) ! xr
                    zmm1.v(i) = re ! re
                    zmm2.v(i) = xim(i) ! xi
                    zre(i)  = zmm0.v(i)+zmm1.v(i)
                    zim(i)  = zmm2.v(i)+zmm1.v(i)
                 end do
                 return 
        else if(n>16 .AND. n<=64) then
                   zmm1.v = re ! re
                   do i = 0,iand(n-1,inot(15)),16
!$omp simd aligned(xim,xre,zre,zim:64) linear(ii:1)
                       do ii = 0, 15  
                            zmm0.v(ii) = xre(i+ii) ! xr
                            zmm2.v(ii) = xim(i+ii) ! xi
                            zre(i+ii)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(i+ii)  = zmm2.v(ii)+zmm1.v(ii)  
                       end do
                   end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                  do j = i, n-1   
                      xr = xre(j)
                      xi = xim(j)
                      zre(j) = xr+re
                      zim(j) = xi+re
                  end do                   
                  return
       else if(n>64) then
               zmm1.v = re ! re
               do i=0, iand(n-1,inot(ZMM_LEN-1)), ZMM_LEN*8
                   call mm_prefetch(xre(i+8*ZMM_LEN),FOR_K_PREFETCH_T1)
                   call mm_prefetch(xim(i+8*ZMM_LEN),FOR_K_PREFETCH_T1)
#if defined(__ICC) || defined(__INTEL_COMPILER)
                   !dir$ assume_aligned  xre:64
                   !dir$ assume_aligned  xim:64
                    !dir$ assume_aligned  zre:64
                   !dir$ assume_aligned  zim:64
#endif                   
!$omp simd aligned(xim,xre,zre,zim:64)  linear(ii:1)              
                      do ii = 0, ZMM_LEN-1  
                            zmm0.v(ii) = xre(i+0+ii) ! xr
                            zmm2.v(ii) = xim(i+0+ii) ! xi
                            zre(i+0+ii)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(i+0+ii)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx1        = i+1*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx1) ! xr
                            zmm2.v(ii) = xim(idx1) ! xi
                            zre(idx1)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx1)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx2        = i+2*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx2) ! xr
                            zmm2.v(ii) = xim(idx2) ! xi
                            zre(idx2)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx2)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx3        = i+3*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx3) ! xr
                            zmm2.v(ii) = xim(idx3) ! xi
                            zre(idx3)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx3)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx4        = i+4*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx4) ! xr
                            zmm2.v(ii) = xim(idx4) ! xi
                            zre(idx4)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx4)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx5        = i+5*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx5) ! xr
                            zmm2.v(ii) = xim(idx5) ! xi
                            zre(idx5)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx5)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx6        = i+6*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx6) ! xr
                            zmm2.v(ii) = xim(idx6) ! xi
                            zre(idx6)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx6)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx7        = i+7*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx7) ! xr
                            zmm2.v(ii) = xim(idx7) ! xi
                            zre(idx7)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx7)  = zmm2.v(ii)+zmm1.v(ii)  
                   end do
               end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                  do j = i, n-1   
                      xr = xre(j)
                      xi = xim(j)
                      zre(j) = xr+re
                      zim(j) = xi+re
                  end do                   
                  return                  
       end if                   
end subroutine caddv_kernel_v512_cv_s_8x16_ps 


subroutine caddv_kernel_v512_cv_s_4x16_ps(xre,xim,zre,zim,n)
#if defined(__ICC) || defined(__INTEL_COMPILER)    
          !DIR$ OPTIMIZE : 3
        !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: caddv_kernel_v512_cv_s_4x16_ps
        !DIR$ ATTRIBUTES OPTIMIZATION_PARAMETER: "TARGET_ARCH=skylake-avx512" :: caddv_kernel_v512_cv_s_4x16_ps
#endif     
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xre
         real(kind=sp), allocatable, dimension(:), intent(in)  :: xim
         real(kind=sp),                            intent(in)  :: re
         real(kind=sp), allocatable, dimension(:), intent(out) :: zre
         real(kind=sp), allocatable, dimension(:), intent(out) :: zim 
         integer(i4),                              intent(in)  :: n
         
         type(ZMM16r4_t), automatic :: zmm0
         type(ZMM16r4_t), automatic :: zmm1
         type(ZMM16r4_t), automatic :: zmm2
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm0
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm1
         !DIR$ ATTRIBUTES ALIGN : 64 :: zmm2
         type(XMM4r4_t),  automatic  :: xmm0
         type(XMM4r4_t),  automatic  :: xmm1
         type(XMM4r4_t),  automatic  :: xmm2
         type(YMM8r4_t),  automatic  :: ymm0 
         type(YMM8r4_t),  automatic  :: ymm1
         type(YMM8r4_t),  automatic  :: ymm2
         real(sp),        automatic  :: xr
         real(sp),        automatic  :: xi
         integer(i4),     automatic  :: i,ii,j
         integer(i4),     automatic  :: idx1,idx2,idx3

         if(n<=0) then
            return
         else if(n==1) then
            xr = xre(0)
            xi = xim(0)
            zre(0) = xr+re
            zim(0) = xi+re
            return
        else if(n>1 .AND. n<=4) then
!$omp simd linear(i:1)
                 do i=0, 3
                    xmm0.v(i) = xre(i) ! xr
                    xmm1.v(i) = re ! re
                    xmm2.v(i) = xim(i) ! xi
                    zre(i)  = xmm0.v(i)+xmm1.v(i)
                    zim(i)  = xmm2.v(i)+xmm1.v(i)
                 end do
                 return
        else if(n>4 .AND. n<=8) then
!$omp simd linear(i:1)
                 do i=0, 7
                    ymm0.v(i) = xre(i) ! xr
                    ymm1.v(i) = re ! re
                    ymm2.v(i) = xim(i) ! xi
                    zre(i)  = ymm0.v(i)+ymm1.v(i)
                    zim(i)  = ymm2.v(i)+ymm1.v(i)
                 end do
                 return    
        else if(n>8 .AND. n<=16) then
!$omp simd linear(i:1)
                 do i=0, 15
                    zmm0.v(i) = xre(i) ! xr
                    zmm1.v(i) = re ! re
                    zmm2.v(i) = xim(i) ! xi
                    zre(i)  = zmm0.v(i)+zmm1.v(i)
                    zim(i)  = zmm2.v(i)+zmm1.v(i)
                 end do
                 return 
        else if(n>16 .AND. n<=32) then
                   zmm1.v = re ! re
                   do i = 0,iand(n-1,inot(15)),16
!$omp simd aligned(xim,xre,zre,zim:64) linear(ii:1)
                       do ii = 0, 15  
                            zmm0.v(ii) = xre(i+ii) ! xr
                            zmm2.v(ii) = xim(i+ii) ! xi
                            zre(i+ii)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(i+ii)  = zmm2.v(ii)+zmm1.v(ii)  
                       end do
                   end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                  do j = i, n-1   
                      xr = xre(j)
                      xi = xim(j)
                      zre(j) = xr+re
                      zim(j) = xi+re
                  end do                   
                  return
       else if(n>32) then
               zmm1.v = re ! re
               do i=0, iand(n-1,inot(ZMM_LEN-1)), ZMM_LEN*4
                   call mm_prefetch(xre(i+4*ZMM_LEN),FOR_K_PREFETCH_T1)
                   call mm_prefetch(xim(i+4*ZMM_LEN),FOR_K_PREFETCH_T1)
#if defined(__ICC) || defined(__INTEL_COMPILER)
                   !dir$ assume_aligned  xre:64
                   !dir$ assume_aligned  xim:64
                    !dir$ assume_aligned  zre:64
                   !dir$ assume_aligned  zim:64
#endif                   
!$omp simd aligned(xim,xre,zre,zim:64)  linear(ii:1)              
                      do ii = 0, ZMM_LEN-1  
                            zmm0.v(ii) = xre(i+0+ii) ! xr
                            zmm2.v(ii) = xim(i+0+ii) ! xi
                            zre(i+0+ii)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(i+0+ii)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx1        = i+1*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx1) ! xr
                            zmm2.v(ii) = xim(idx1) ! xi
                            zre(idx1)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx1)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx2        = i+2*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx2) ! xr
                            zmm2.v(ii) = xim(idx2) ! xi
                            zre(idx2)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx2)  = zmm2.v(ii)+zmm1.v(ii)  
                            idx3        = i+3*ZMM_LEN+ii
                            zmm0.v(ii) = xre(idx3) ! xr
                            zmm2.v(ii) = xim(idx3) ! xi
                            zre(idx3)  = zmm0.v(ii)+zmm1.v(ii)
                            zim(idx3)  = zmm2.v(ii)+zmm1.v(ii)  
                    end do
               end do
#if defined(__ICC) || defined(__INTEL_COMPILER)
         !DIR$ LOOP COUNT MAX=16, MIN=1, AVG=8
#endif            
                  do j = i, n-1   
                      xr = xre(j)
                      xi = xim(j)
                      zre(j) = xr+re
                      zim(j) = xi+re
                  end do                   
                  return                  
       end if                   
end subroutine caddv_kernel_v512_cv_s_4x16_ps 

#endif
end module caddv_zmm16r4
     
