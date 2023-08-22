

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

module rcs_common_zmm16r4


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         rcs_common_zmm16r4
 !          
 !          Purpose:
 !                       Common functions used by various RCS implementations
 !                        
 !          History:
 !                        Date: 08-22-2023
 !                        Time: 16:53 GMT+2
 !                        
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  
 !                      Vectorized by Bernard Gingold (based on different authors work)
 !                      The details stated by the specific function description.
 !                 
 !          References:
 !         
 !                      Provided at the specific function description
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
!==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.

    use mod_kinds,    only : i4,sp
    use mod_vectypes, only : ZMM16r4_t
    
    public
    implicit none
    
    
      ! Major version
     integer(kind=i4),  parameter :: RCS_COMMON_ZMM16R4_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: RCS_COMMON_ZMM16R4_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: RCS_COMMON_ZMM16R4_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: RCS_COMMON_ZMM16R4_FULLVER =   &
            1000*RCS_COMMON_ZMM16R4_MAJOR+100*RCS_COMMON_ZMM16R4_MINOR+10*RCS_COMMON_ZMM16R4_MICRO
     ! Module creation date
     character(*),        parameter :: RCS_COMMON_ZMM16R4_CREATE_DATE = "22-08-2022 17:3 +00200 (TUE 22 AUG 2023 GMT+2)"
     ! Module build date
     character(*),        parameter :: RCS_COMMON_ZMM16R4_BUILD_DATE  = __DATE__ " " __TIME__
     ! Module author info
     character(*),        parameter :: RCS_COMMON_ZMM16R4_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: RCS_CYLINDER_ZMM16R4_SYNOPSIS  = "Helper functions for the RCS implementations" 
     
     
     contains
     
     
        ! /*
        !                Complex wavenumber, vector of 16 varying values of
        !                complex permeability and permitivity.
        !            */
        
        
        pure function k_zmm16c4(mu,eps,om) result(k)
             
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: k_zmm16c4
              !dir$ attributes forceinline :: k_zmm16c4
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: k_zmm16c4
              use avx512_cvec16_v2
              type(ZMM16c4),  intent(in) :: mu
              type(ZMM16c4),  intent(in) :: eps
              type(ZMM16r4_t),intent(in) :: om
              type(ZMM16c4) :: k
              !dir$ attributes align : 64 :: sqr
              !dir$ attributes align : 64 :: tc0
              type(ZMM16c4), automatic :: sqr
              type(ZMM16c4), automatic :: tc0
              tc0 = mu*eps
              sqr = csqrt_c16(tc0)
              k   = sqr*om
        end function k_zmm16c4


























end module rcs_common_zmm16r4
