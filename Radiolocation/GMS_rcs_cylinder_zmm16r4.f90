



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

module rcs_cylinder_zmm16r4


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         rcs_cylinder_zmm16r4
 !          
 !          Purpose:
 !                        Various characteristics of analytically derived Radar
 !                        Cross Section of cylindrical objects  
 !                        Based  on George T. Ruck, Donald E. Barrick , William D. Stuart , 
 !                        - "Radar Cross Section Handbook 1 and 2" (1970, Kluwer Academic Plenum Publishers) 
 !                        This module contains only explicitly vectorized (SIMD)
 !                        
 !          History:
 !                        Date: 08-07-2023
 !                        Time: 14:21 GMT+2
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
 !                 
 !          References:
 !         
 !                      George T. Ruck, Donald E. Barrick , William D. Stuart
 !                      Radar Cross Section Handbook 1 and 2" (1970, Kluwer Academic Plenum Publishers)     
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
!==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.

    use mod_kinds,    only : i4,sp
    use mod_vectypes, only : ZMM16r4_t
    use avx512_cvec16
    
    public
    implicit none
    
     ! Major version
     integer(kind=i4),  parameter :: RCS_CYLINDER_ZMM16R4_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: RCS_CYLINDER_ZMM16R4_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: RCS_CYLINDER_ZMM16R4_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: RCS_CYLINDER_ZMM16R4_FULLVER =   &
            1000*RCS_CYLINDER_ZMM16R4_MAJOR+100*RCS_CYLINDER_ZMM16R4_MINOR+10*RCS_CYLINDER_ZMM16R4_MICRO
     ! Module creation date
     character(*),        parameter :: RCS_CYLINDER_ZMM16R4_CREATE_DATE = "18-07-2022 14:30 +00200 (TUE 18 JUL 2023 GMT+2)"
     ! Module build date
     character(*),        parameter :: RCS_CYLINDER_ZMM16R4_BUILD_DATE  = __DATE__ " " __TIME__
     ! Module author info
     character(*),        parameter :: RCS_CYLINDER_ZMM16R4_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: RCS_CYLINDER_ZMM16R4_SYNOPSIS    = "Analytical Cylindrical objects RCS characteristics and models explicitly vectorized (SIMD)."
    

    
     contains
     
     
                   !/* 
                   !      Low frequency scattering widths (k0a << 1).
                   !      Backscatter scattering width for E-field 
                   !      cylinder-parallel,formula 4.1-19
                   ! */
                   
                   
             pure function rcs_f419_zmm16r4(a,k0a) result(rcs)
                  
                   !dir$ optimize:3
                   !dir$ attributes code_align : 32 :: rcs_f419_zmm16r4
                   !dir$ attributes forceinline :: rcs_f419_zmm16r4
                   !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rcs_f419_zmm16r4
                   type(ZMM16r4_t),  intent(in) :: a
                   type(ZMM16r4_t),  intent(in) :: k0a
                   type(ZMM16r4_t) :: rcs
                   ! locals
                   type(ZMM16r4_t), parameter :: C9869604401089358618834490999876 =             &
                                                 ZMM16r4_t(9.869604401089358618834490999876_sp)
                   type(ZMM16r4_t), parameter :: C2467401100272339654708622749969 =             &
                                                 ZMM16r4_t(2.467401100272339654708622749969_sp)
                   type(ZMM16r4_t), parameter :: C08905  = ZMM16r4_t(0.8905_sp)
                   ZMM16r4_t, automatic :: num,arg,ln,ln2,den
                   num.v = a.v*C9869604401089358618834490999876.v
                   arg.v = k0a.v*C08905.v
                   ln.v  = log(arg.v)
                   ln2.v = ln.v*ln.v
                   den.v = k0a.v*ln2.v+C2467401100272339654708622749969.v
                   rcs.v = num.v/den.v
             end function rcs_f419_zmm16r4






















end module rcs_cylinder_zmm16r4
