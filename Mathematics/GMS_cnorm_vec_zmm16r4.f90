
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

module cnorm_vec_zmm16r4


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name: cnorm_vec_zmm16r4
 !                         
 !          
 !          Purpose:
 !                       Complex-norm product vectorized deinterleaved, single-precision
 !                        
 !          History:
 !                        Date: 20-10-2024
 !                        Time: 04:30PM GMT+2
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
     integer(kind=i4),  parameter :: CNORM_VEC_ZMM16R4_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: CNORM_VEC_ZMM16R4_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: CNORM_VEC_ZMM16R4_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: CNORM_VEC_ZMM16R4_FULLVER =   &
            1000*CNORM_VEC_ZMM16R4_MAJOR+100*CNORM_VEC_ZMM16R4_MINOR+10*CNORM_VEC_ZMM16R4_MICRO
     ! Module creation date
     character(*),        parameter :: CNORM_VEC_ZMM16R4_CREATE_DATE = "20-10-2024 04:35PM +00200 (SUN 20 OCT 2024 GMT+2)"
     ! Module build date
     character(*),        parameter :: CNORM_VEC_ZMM16R4_BUILD_DATE  = __DATE__ " " __TIME__
     ! Module author info
     character(*),        parameter :: CNORM_VEC_ZMM16R4_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: CNORM_VEC_ZMM16R4_SYNOPSIS  = "Complex-norm product vectorized deinterleaved, single-precision." 
     
    
     integer(kind=i4), parameter, private :: ZMM_LEN = 16
     
     
     
     
end module cnorm_vec_zmm16r4
     
