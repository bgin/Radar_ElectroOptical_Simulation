

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

module spec_funcs_zmm8r8


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         spec_funcs_zmm8r8
 !          
 !          Purpose:
 !                       Various vectorized special functions.
 !                        
 !          History:
 !                        Date: 08-28-2023
 !                        Time: 16:09 GMT+2
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

    use mod_kinds,    only : i4,dp
    use mod_vectypes, only : ZMM8r8_t,Mask8_t
    
    public
    implicit none
    
    
      ! Major version
     integer(kind=i4),  parameter :: SPEC_FUNCS_ZMM8R8_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: SPEC_FUNCS_ZMM8R8_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: SPEC_FUNCS_ZMM8R8_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: SPEC_FUNCS_ZMM8R8_FULLVER =   &
            1000*SPEC_FUNCS_ZMM8R8_MAJOR+100*SPEC_FUNCS_ZMM8R8_MINOR+10*SPEC_FUNCS_ZMM8R8_MICRO
     ! Module creation date
     character(*),        parameter :: SPEC_FUNCS_ZMM8R8_CREATE_DATE = "28-08-2022 06:11 +00200 (MON 28 AUG 2023 GMT+2)"
     ! Module build date
     character(*),        parameter :: SPEC_FUNCS_ZMM8R8_BUILD_DATE  = __DATE__ " " __TIME__
     ! Module author info
     character(*),        parameter :: SPEC_FUNCS_ZMM8R8_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: SPEC_FUNCS_ZMM8R8_SYNOPSIS  = "Vectorized various special functions" 
     

     contains
     
     
#if 0
/*
               !*****************************************************************************80
!
!! BESEI0 evaluates the exponentially scaled Bessel I0(X) function.
!
!  Discussion:
!
!    This routine computes approximate values for the modified Bessel
!    function of the first kind of order zero multiplied by EXP(-ABS(X)).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 April 2007
!
!  Author:
!
!    Original FORTRAN77 version by William Cody.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) BESEI0, the value of the function.
!
               
*/

#endif


          pure function besei0_zmm8r8(x) result(val)
               
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: besei0_zmm8r8
              !dir$ attributes forceinline :: besei0_zmm8r8
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: besei0_zmm8r8
              type(ZMM8r8),   intent(in) :: x
              type(ZMM8r8)  :: val
              integer(kind=i4), automatic :: jint
              jint = 2
              val  = calci0_zmm8r8(x,jint)
          end function besei0_zmm8r8
          

#if 0
 /*
!*****************************************************************************80
!
!! BESEI1 evaluates the exponentially scaled Bessel I1(X) function.
!
!  Discussion:
!
!    This routine computes approximate values for the
!    modified Bessel function of the first kind of order one
!    multiplied by EXP(-ABS(X)).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 April 2007
!
!  Author:
!
!    Original FORTRAN77 version by William Cody.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) BESEI1, the value of the function.
*/	         

#endif      


          pure function besei1_zmm8r8(x) result(val)
               
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: besei1_zmm8r8
              !dir$ attributes forceinline :: besei1_zmm8r8
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: besei1_zmm8r8
              type(ZMM8r8),   intent(in) :: x
              type(ZMM8r8)  :: val
              integer(kind=i4), automatic :: jint
              jint = 2
              val  = calci1_zmm8r8(x,jint) 
          end function besei1_zmm8r8
          

#if 0         
/*
   !*****************************************************************************80
!
!! BESEK0 evaluates the exponentially scaled Bessel K0(X) function.
!
!  Discussion:
!
!    This routine computes approximate values for the
!    modified Bessel function of the second kind of order zero
!    multiplied by the exponential function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 April 2007
!
!  Author:
!
!    Original FORTRAN77 version by William Cody, Laura Stoltz.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the function.
!    0 < X.
!
!    Output, real ( kind = 8 ) BESK0, the value of the function.
*/   
#endif


           pure function besek0_zmm8r8(x) result(val)
               
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: besek0_zmm8r8
              !dir$ attributes forceinline :: besek0_zmm8r8
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: besek0_zmm8r8
              type(ZMM8r8),   intent(in) :: x
              type(ZMM8r8)  :: val
              integer(kind=i4), automatic :: jint
              jint = 2
              val  = calck0_zmm8r8(x,jint) 
          end function besek0_zmm8r8     



end module spec_funcs_zmm8r8
