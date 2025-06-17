

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

module spec_func_ymm4r8


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         spec_funcs_ymm4r8
 !          
 !          Purpose:
 !                       Various vectorized special functions.
 !                        
 !          History:
 !                        Date: 17-06-2025
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

    use mod_kinds,    only : i4,i8,dp
    use mod_vectypes, only : YMM4r8_t,Mask4_t
    implicit none 
    public
    
    
    
      ! Major version
     integer(kind=i4),  parameter :: SPEC_FUNCS_YMM4R8_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: SPEC_FUNCS_YMM4R8_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: SPEC_FUNCS_YMM4R8_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: SPEC_FUNCS_YMM4R8_FULLVER =   &
            1000*SPEC_FUNCS_YMM4R8_MAJOR+100*SPEC_FUNCS_YMM4R8_MINOR+10*SPEC_FUNCS_YMM4R8_MICRO
     ! Module creation date
     character(*),        parameter :: SPEC_FUNCS_YMM4R8_CREATE_DATE = "17-06-2023 11:14 +00200 (TUE 16 JUN 2025 GMT+2)"
     ! Module build date
     character(*),        parameter :: SPEC_FUNCS_YMM4R8_BUILD_DATE  =  __DATE__ 
     character(*),        parameter :: SPEC_FUNCS_YMM4R8_BUILD_TIME  =  __TIME__
     ! Module author info
     character(*),        parameter :: SPEC_FUNCS_YMM4R8_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: SPEC_FUNCS_YMM4R8_SYNOPSIS    = "Vectorized (some) Bessel special functions" 
   
     
    
    
    
      
     contains
     

 

     !*****************************************************************************80
!
!! BESI0 evaluates the Bessel I0(X) function.
!
!  Discussion:
!
!    This routine computes approximate values for
!    modified Bessel functions of the first kind of order zero for
!    arguments ABS(ARG) <= XMAX.
!
!    See comments heading CALCI0.
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
!    Output, real ( kind = 8 ) BESI0, the value of the function.




           function besi0_ymm4r8(x) result(val)
               
              !dir$ optimize:3
            
              !dir$ attributes forceinline :: besi0_ymm4r8
             
              type(YMM4r8_t),   intent(in) :: x
              type(YMM4r8_t)  :: val
              type(YMM4r8_t), automatic :: t0 
              call calci0_ymm4r8(x,t0) 
              val = t0 
          end function besi0_ymm4r8  
          
          


    !*****************************************************************************80
!
!! BESI1 evaluates the Bessel I1(X) function.
!
!  Discussion:
!
!    This routine computes approximate values for
!    modified Bessel functions of the first kind of order one for
!    arguments ABS(ARG) <= XMAX.
!
!    See comments heading CALCI1.
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
!    Output, real ( kind = 8 ) BESI1, the value of the function.        

 


            function besi1_xmm2r8(x) result(val)
               
              !dir$ optimize:3
            
              !dir$ attributes forceinline :: besi1_ymm4r8
              
              type(YMM4r8_t),   intent(in) :: x
              type(YMM4r8_t)  :: val
              type(YMM4r8_t),   automatic :: t0 
              call calci1_ymm4r8(x,t0) 
              val = t0 
          end function besi1_ymm4r8   
          
          




 


!
!! BESK0 evaluates the Bessel K0(X) function.
!
!  Discussion:
!
!    This routine computes approximate values for the
!    modified Bessel function of the second kind of order zero
!    for arguments 0.0 < ARG <= XMAX.
!
!    See comments heading CALCK0.
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
!
!    Output, real ( kind = 8 ) BESK0, the value of the function.



          function besk0_ymm4r8(x) result(val)
               
              !dir$ optimize:3
              
              !dir$ attributes forceinline :: besk0_ymm4r8
              
              type(YMM4r8_t),   intent(in) :: x
              type(YMM4r8_t)  :: val
              type(YMM4r8_t),   automatic :: t0 
              call calck0_ymm4r8(x,t0)
              val = t0  
          end function besk0_ymm4r8   
          
          

!
!! BESK1 evaluates the Bessel K1(X) function.
!
!  Discussion:
!
!    This routine computes approximate values for the
!    modified Bessel function of the second kind of order one
!    for arguments XLEAST <= ARG <= XMAX.
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
!    Output, real ( kind = 8 ) BESK1, the value of the function.        



          function besk1_ymm4r8(x) result(val)
               
              !dir$ optimize:3
              
              !dir$ attributes forceinline :: besk1_ymm4r8
             
              type(YMM4r8_t),   intent(in) :: x
              type(YMM4r8_t)  :: val
              type(YMM4r8_t), automatic :: t0 
              call calck1_ymm4r8(x,t0) 
              val = t0 
          end function besk1_ymm4r8   
          
         

!
!! BESY0 evaluates the Bessel Y0(X) function.
!
!  Discussion:
!
!    This routine computes approximate values for Bessel functions
!    of the second kind of order zero for arguments 0 < X <= XMAX.
!
!    See comments heading CALJY0.
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
!    Output, real ( kind = 8 ) BESY0, the value of the function.



           function besy0_ymm4r8(x) result(val)
               
              !dir$ optimize:3
              
              !dir$ attributes forceinline :: besy0_ymm4r8
             
              type(YMM4r8_t),   intent(in) :: x
              type(YMM4r8_t)  :: val
              type(YMM4r8_t), automatic :: t0 
              integer(kind=i4), automatic :: jint
              jint = 1
              call caljy0_ymm4r8(x,t0,jint) 
              val = t0 
          end function besy0_ymm4r8   
          
          





       
    

!
!! CALCI0 computes various I0 Bessel functions.
!
!  Discussion:
!
!    This routine computes modified Bessel functions of the first kind
!    and order zero, I0(X) and EXP(-ABS(X))*I0(X), for real
!    arguments X.
!
!    The main computation evaluates slightly modified forms of
!    minimax approximations generated by Blair and Edwards, Chalk
!    River (Atomic Energy of Canada Limited) Report AECL-4928,
!    October, 1974.
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
!    Input, real ( kind = 8 ) ARG, the argument.  If JINT = 1, then
!    the argument must be less than XMAX.
!
!    Output, real ( kind = 8 ) RESULT, the value of the function,
!    which depends on the input value of JINT:
!    1, RESULT = I0(x);
!    2, RESULT = exp(-x) * I0(x);
!
!    Input, integer ( kind = 4 ) JINT, chooses the function to be computed.
!    1, I0(x);
!    2, exp(-x) * I0(x);      



        subroutine calci0_ymm4r8(arg,val)
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: calci0_ymm4r8
              !dir$ attributes forceinline :: calci0_ymm4r8
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calci0_ymm4r8  
               type(YMM4r8_t),   intent(in)   :: arg
               type(YMM4r8_t),   intent(out)  :: val
              
               !dir$ attributes align : 32 :: one
               !dir$ attributes align : 32 :: one5
               !dir$ attributes align : 32 :: exp40
               !dir$ attributes align : 32 :: frty
               !dir$ attributes align : 32 :: rec15
               !dir$ attributes align : 32 :: two25
               !dir$ attributes align : 32 :: xsmall
               !dir$ attributes align : 32 :: xinf
               !dir$ attributes align : 32 :: xmax
               type(YMM4r8_t),   parameter    :: one   = YMM4r8_t([1.0e+0_dp,1.0e+0_dp,1.0e+0_dp,1.0e+0_dp])
               type(YMM4r8_t),   parameter    :: one5  = YMM4r8_t([15.0e+0_dp,15.0e+0_dp,15.0e+0_dp,15.0e+0_dp])
               type(YMM4r8_t),   parameter    :: exp40 = YMM4r8_t([2.353852668370199854e+17_dp,2.353852668370199854e+17_dp, &
                                                                   2.353852668370199854e+17_dp,2.353852668370199854e+17_dp])
               type(YMM4r8_t),   parameter    :: frty  = YMM4r8_t([40.0e+0_dp,40.0e+0_dp,40.0e+0_dp,40.0e+0_dp])
               type(YMM4r8_t),   parameter    :: rec15 = YMM4r8_t([6.6666666666666666666e-2_dp,6.6666666666666666666e-2_dp, &
                                                                   6.6666666666666666666e-2_dp,6.6666666666666666666e-2_dp])
               type(YMM4r8_t),   parameter    :: two25 = YMM4r8_t([225.0e+0_dp,225.0e+0_dp,225.0e+0_dp,225.0e+0_dp])
               type(YMM4r8_t),   parameter    :: xsmall= YMM4r8_t([5.55e-17_dp,5.55e-17_dp,5.55e-17_dp,5.55e-17_dp])
               type(YMM4r8_t),   parameter    :: xinf  = YMM4r8_t([1.79e+308_dp,1.79e+308_dp,1.79e+308_dp,1.79e+308_dp])
               type(YMM4r8_t),   parameter    :: xmax  = YMM4r8_t([713.986e+0_dp,713.986e+0_dp,713.986e+0_dp,713.986e+0_dp])
               type(YMM4r8_t),   parameter    :: p1    = YMM4r8_t([-5.2487866627945699800e-18_dp,-5.2487866627945699800e-18_dp, &
                                                                   -5.2487866627945699800e-18_dp,-5.2487866627945699800e-18_dp])
               type(YMM4r8_t),   parameter    :: p2    = YMM4r8_t([-1.5982226675653184646e-14_dp,-1.5982226675653184646e-14_dp, &
                                                                   -1.5982226675653184646e-14_dp,-1.5982226675653184646e-14_dp])
               type(YMM4r8_t),   parameter    :: p3    = YMM4r8_t([-2.6843448573468483278e-11_dp,-2.6843448573468483278e-11_dp, &
                                                                   -2.6843448573468483278e-11_dp,-2.6843448573468483278e-11_dp])
               type(YMM4r8_t),   parameter    :: p4    = YMM4r8_t([-3.0517226450451067446e-08_dp,-3.0517226450451067446e-08_dp, &
                                                                   -3.0517226450451067446e-08_dp,-3.0517226450451067446e-08_dp])
               type(YMM4r8_t),   parameter    :: p5    = YMM4r8_t([-2.5172644670688975051e-05_dp,-2.5172644670688975051e-05_dp, &
                                                                   -2.5172644670688975051e-05_dp,-2.5172644670688975051e-05_dp ])
               type(YMM4r8_t),   parameter    :: p6    = YMM4r8_t([-1.5453977791786851041e-02_dp,-1.5453977791786851041e-02_dp, &
                                                                   -1.5453977791786851041e-02_dp,-1.5453977791786851041e-02_dp])
               type(YMM4r8_t),   parameter    :: p7    = YMM4r8_t([-7.0935347449210549190e+00_dp,-7.0935347449210549190e+00_dp, &
                                                                   -7.0935347449210549190e+00_dp,-7.0935347449210549190e+00_dp ])
               type(YMM4r8_t),   parameter    :: p8    = YMM4r8_t([-2.4125195876041896775e+03_dp,-2.4125195876041896775e+03_dp, &
                                                                   -2.4125195876041896775e+03_dp,-2.4125195876041896775e+03_dp])
               type(YMM4r8_t),   parameter    :: p9    = YMM4r8_t([-5.9545626019847898221e+05_dp,-5.9545626019847898221e+05_dp, &
                                                                   -5.9545626019847898221e+05_dp,-5.9545626019847898221e+05_dp])
               type(YMM4r8_t),   parameter    :: p10   = YMM4r8_t([-1.0313066708737980747e+08_dp,-1.0313066708737980747e+08_dp, &
                                                                   -1.0313066708737980747e+08_dp,-1.0313066708737980747e+08_dp])
               type(YMM4r8_t),   parameter    :: p11   = YMM4r8_t([-1.1912746104985237192e+10_dp,-1.1912746104985237192e+10_dp, &
                                                                   -1.1912746104985237192e+10_dp,-1.1912746104985237192e+10_dp])
               type(YMM4r8_t),   parameter    :: p12   = YMM4r8_t([-8.4925101247114157499e+11_dp,-8.4925101247114157499e+11_dp, &
                                                                   -8.4925101247114157499e+11_dp,-8.4925101247114157499e+11_dp])
               type(YMM4r8_t),   parameter    :: p13   = YMM4r8_t([-3.2940087627407749166e+13_dp,-3.2940087627407749166e+13_dp, &
                                                                   -3.2940087627407749166e+13_dp,-3.2940087627407749166e+13_dp ])
               type(YMM4r8_t),   parameter    :: p14   = YMM4r8_t([-5.5050369673018427753e+14_dp,-5.5050369673018427753e+14_dp, &
                                                                   -5.5050369673018427753e+14_dp,-5.5050369673018427753e+14_dp])
               type(YMM4r8_t),   parameter    :: p15   = YMM4r8_t([-2.2335582639474375249e+15_dp,-2.2335582639474375249e+15_dp, &
                                                                   -2.2335582639474375249e+15_dp,-2.2335582639474375249e+15_dp])
               type(YMM4r8_t),   parameter    :: q1    = YMM4r8_t([-3.7277560179962773046e+03_dp,-3.7277560179962773046e+03_dp, &
                                                                   -3.7277560179962773046e+03_dp,-3.7277560179962773046e+03_dp])        
               type(YMM4r8_t),   parameter    :: q2    = YMM4r8_t([ 6.5158506418655165707e+06_dp,6.5158506418655165707e+06_dp, &
                                                                    6.5158506418655165707e+06_dp,6.5158506418655165707e+06_dp])         
               type(YMM4r8_t),   parameter    :: q3    = YMM4r8_t([-6.5626560740833869295e+09_dp,-6.5626560740833869295e+09_dp, &
                                                                   -6.5626560740833869295e+09_dp,-6.5626560740833869295e+09_dp])        
               type(YMM4r8_t),   parameter    :: q4    = YMM4r8_t([ 3.7604188704092954661e+12_dp,3.7604188704092954661e+12_dp, &
                                                                    3.7604188704092954661e+12_dp,3.7604188704092954661e+12_dp])         
               type(YMM4r8_t),   parameter    :: q5    = YMM4r8_t([-9.7087946179594019126e+14_dp,-9.7087946179594019126e+14_dp, &
                                                                   -9.7087946179594019126e+14_dp,-9.7087946179594019126e+14_dp])
               type(YMM4r8_t),   parameter    :: pp1   = YMM4r8_t([-3.9843750000000000000e-01_dp,-3.9843750000000000000e-01_dp, &
                                                                   -3.9843750000000000000e-01_dp,-3.9843750000000000000e-01_dp])         
               type(YMM4r8_t),   parameter    :: pp2   = YMM4r8_t([ 2.9205384596336793945e+00_dp,2.9205384596336793945e+00_dp, &
                                                                    2.9205384596336793945e+00_dp,2.9205384596336793945e+00_dp])         
               type(YMM4r8_t),   parameter    :: pp3   = YMM4r8_t([-2.4708469169133954315e+00_dp,-2.4708469169133954315e+00_dp, &
                                                                   -2.4708469169133954315e+00_dp,-2.4708469169133954315e+00_dp])        
               type(YMM4r8_t),   parameter    :: pp4   = YMM4r8_t([ 4.7914889422856814203e-01_dp,4.7914889422856814203e-01_dp, &
                                                                    4.7914889422856814203e-01_dp,4.7914889422856814203e-01_dp])         
               type(YMM4r8_t),   parameter    :: pp5   = YMM4r8_t([-3.7384991926068969150e-03_dp,-3.7384991926068969150e-03_dp, &
                                                                   -3.7384991926068969150e-03_dp,-3.7384991926068969150e-03_dp])        
               type(YMM4r8_t),   parameter    :: pp6   = YMM4r8_t([-2.6801520353328635310e-03_dp,-2.6801520353328635310e-03_d, &
                                                                   -2.6801520353328635310e-03_dp,-2.6801520353328635310e-03_d])        
               type(YMM4r8_t),   parameter    :: pp7   = YMM4r8_t([ 9.9168777670983678974e-05_dp,9.9168777670983678974e-05_dp, &
                                                                    9.9168777670983678974e-05_dp,9.9168777670983678974e-05_dp])        
               type(YMM4r8_t),   parameter    :: pp8   = YMM4r8_t([-2.1877128189032726730e-06_dp,-2.1877128189032726730e-06_dp, &
                                                                   -2.1877128189032726730e-06_dp,-2.1877128189032726730e-06_dp])
               type(YMM4r8_t),   parameter    :: qq1   = YMM4r8_t([-3.1446690275135491500e+01_dp,-3.1446690275135491500e+01_dp, &
                                                                   -3.1446690275135491500e+01_dp,-3.1446690275135491500e+01_dp])        
               type(YMM4r8_t),   parameter    :: qq2   = YMM4r8_t([ 8.5539563258012929600e+01_dp,8.5539563258012929600e+01_dp, &
                                                                    8.5539563258012929600e+01_dp,8.5539563258012929600e+01_dp])         
               type(YMM4r8_t),   parameter    :: qq3   = YMM4r8_t([-6.0228002066743340583e+01_dp,-6.0228002066743340583e+01_dp, &
                                                                   -6.0228002066743340583e+01_dp,-6.0228002066743340583e+01_dp])        
               type(YMM4r8_t),   parameter    :: qq4   = YMM4r8_t([ 1.3982595353892851542e+01_dp,1.3982595353892851542e+01_dp, &
                                                                    1.3982595353892851542e+01_dp,1.3982595353892851542e+01_dp])         
               type(YMM4r8_t),   parameter    :: qq5   = YMM4r8_t([-1.1151759188741312645e+00_dp,-1.1151759188741312645e+00_dp, &
                                                                   -1.1151759188741312645e+00_dp,-1.1151759188741312645e+00_dp])        
               type(YMM4r8_t),   parameter    :: qq6   = YMM4r8_t([ 3.2547697594819615062e-02_dp,3.2547697594819615062e-02_dp, &
                                                                    3.2547697594819615062e-02_dp,3.2547697594819615062e-02_dp])         
               type(YMM4r8_t),   parameter    :: qq7   = YMM4r8_t([-5.5194330231005480228e-04_dp,-5.5194330231005480228e-04_dp, &
                                                                   -5.5194330231005480228e-04_dp,-5.5194330231005480228e-04_dp])
               !dir$ attributes align : 32 :: a
               !dir$ attributes align : 32 :: b
               !dir$ attributes align : 32 :: sump
               !dir$ attributes align : 32 :: sumq
               !dir$ attributes align : 32 :: x
               !dir$ attributes align : 32 :: xx
               !dir$ attributes align : 32 :: t0
               !dir$ attributes align : 32 :: t1
               !dir$ attributes align : 32 :: p1 
               !dir$ attributes align : 32 :: p2 
               !dir$ attributes align : 32 :: p3 
               !dir$ attributes align : 32 :: p4 
               !dir$ attributes align : 32 :: p5 
               !dir$ attributes align : 32 :: p6 
               !dir$ attributes align : 32 :: p7 
               !dir$ attributes align : 32 :: p8 
               !dir$ attributes align : 32 :: p9 
               !dir$ attributes align : 32 :: p10 
               !dir$ attributes align : 32 :: p11 
               !dir$ attributes align : 32 :: p12 
               !dir$ attributes align : 32 :: p13 
               !dir$ attributes align : 32 :: p14 
               !dir$ attributes align : 32 :: p15
               !dir$ attributes align : 32 :: q1 
               !dir$ attributes align : 32 :: q2 
               !dir$ attributes align : 32 :: q3 
               !dir$ attributes align : 32 :: q4 
               !dir$ attributes align : 32 :: q5 
               !dir$ attributes align : 32 :: pp1 
               !dir$ attributes align : 32 :: pp2 
               !dir$ attributes align : 32 :: pp3 
               !dir$ attributes align : 32 :: pp5 
               !dir$ attributes align : 32 :: pp6 
               !dir$ attributes align : 32 :: pp7 
               !dir$ attributes align : 32 :: pp8
               !dir$ attributes align : 32 :: qq1 
               !dir$ attributes align : 32 :: qq2 
               !dir$ attributes align : 32 :: qq3 
               !dir$ attributes align : 32 :: qq4 
               !dir$ attributes align : 32 :: qq5 
               !dir$ attributes align : 32 :: qq6 
               !dir$ attributes align : 32 :: qq7
               type(YMM4r8_t),   automatic    :: a,b
               type(YMM4r8_t),   automatic    :: sump,sumq
               type(YMM4r8_t),   automatic    :: x,xx
               type(YMM4r8_t),   automatic    :: t0,t1
               
               type(Mask4_t),    automatic    :: msk1,msk2
               type(Mask4_t),    automatic    :: msk3,msk4
                                
               x.v    = abs(arg.v)
               msk1.m = (x.v<xsmall.v)
               msk2.m = (x.v<one5.v)
               msk3.m = (one5.v<=x.v)
               !if(any(msk1.m)) then
               where(msk1.m)
             
                  val.v = one.v
               else where(msk2.m) 
           
                  xx.v   = x.v*x.v
                  sump.v = p1.v
                  sump.v = sump.v*xx.v+p2.v
                  sump.v = sump.v*xx.v+p3.v
                  sump.v = sump.v*xx.v+p4.v
                  sump.v = sump.v*xx.v+p5.v
                  sump.v = sump.v*xx.v+p6.v
                  sump.v = sump.v*xx.v+p7.v
                  sump.v = sump.v*xx.v+p8.v
                  sump.v = sump.v*xx.v+p9.v
                  sump.v = sump.v*xx.v+p10.v
                  sump.v = sump.v*xx.v+p11.v
                  sump.v = sump.v*xx.v+p12.v
                  sump.v = sump.v*xx.v+p13.v
                  sump.v = sump.v*xx.v+p14.v
                  sump.v = sump.v*xx.v+p15.v
 
                  xx.v   = xx.v-two25.v
                  sumq.v = (((( &
                             xx.v+q1.v) &
                           * xx.v+q2.v) &
                           * xx.v+q3.v) &
                           * xx.v+q4.v) &
                           * xx.v+q5.v 
                  val.v  = sump.v/sumq.v
                  
               else where(msk3.m) 
              
                     msk4.m = (xmax.v<=x.v)
                     where(msk4.m)
                         val.v = xinf.v
                     else where 
                         xx.v    = one.v/x.v-rec15.v
                         sump.v  = ((((((   &
                                      pp1.v      &
                                   * xx.v+pp2.v)   &
                                   * xx.v+pp3.v)   &
                                   * xx.v+pp4.v)   &
                                   * xx.v+pp5.v)   &
                                   * xx.v+pp6.v)   &
                                   * xx.v+pp7.v)   &
                                   * xx.v+pp8.v
                         sumq.v  = ((((((    &
                                     xx.v+qq1.v) &
                                   * xx.v+qq2.v) &
                                   * xx.v+qq3.v) &
                                   * xx.v+qq4.v) &
                                   * xx.v+qq5.v) &
                                   * xx.v+qq6.v) &
                                   * xx.v+qq7.v
                        val.v    = sump.v/sumq.v
                        
                        
                           msk4.m = (x.v<=(xmax.v-one5.v))
                           where(msk4.m) 
                              a.v = exp(x.v)
                              b.v = one.v
                           else where 
                              a.v = exp(x.v-frty.v)
                              b.v = exp40.v
                            end where 
                            t0.v  = pp1.v*a.v
                            t1.v  = sqrt(x.v)
                            val.v = ((val.v*a.v-t0.v)/t1.v)*b.v
                    end where 
              end where 
               
        end subroutine calci0_ymm4r8
        
        


!! CALCI1 computes various I1 Bessel functions.
!
!  Discussion:
!
!    This routine computes modified Bessel functioons of the first kind
!    and order one, I1(X) and EXP(-ABS(X))*I1(X), for real
!    arguments X.
!
!    The main computation evaluates slightly modified forms of
!    minimax approximations generated by Blair and Edwards, Chalk
!    River (Atomic Energy of Canada Limited) Report AECL-4928,
!    October, 1974.
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
!    Input, real ( kind = 8 ) ARG, the argument.  If JINT = 1, then
!    the argument must be less than XMAX.
!
!    Output, real ( kind = 8 ) RESULT, the value of the function,
!    which depends on the input value of JINT:
!    1, RESULT = I1(x);
!    2, RESULT = exp(-x) * I1(x);
!
!    Input, integer ( kind = 4 ) JINT, chooses the function to be computed.
!    1, I1(x);
!    2, exp(-x) * I1(x);   



        subroutine calci1_ymm4r8(arg,val)
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: calci1_ymm4r8
              !dir$ attributes forceinline :: calci1_ymm4r8
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calci1_ymm4r8  
              type(YMM4r8_t),   intent(in)   :: arg
              type(YMM4r8_t),   intent(out)  :: val
              
              !dir$ attributes align : 32 :: one
              !dir$ attributes align : 32 :: one5
              !dir$ attributes align : 32 :: exp40
              !dir$ attributes align : 32 :: frty
              !dir$ attributes align : 32 :: rec15
              !dir$ attributes align : 32 :: two25
              !dir$ attributes align : 32 :: xsmall
              !dir$ attributes align : 32 :: xinf
              !dir$ attributes align : 32 :: xmax
              !dir$ attributes align : 32 :: half
              !dir$ attributes align : 32 :: zero
             
              type(YMM4r8_t),   parameter    :: one   = YMM4r8_t([1.0e+0_dp,1.0e+0_dp,1.0e+0_dp,1.0e+0_dp])
              type(YMM4r8_t),   parameter    :: one5  = YMM4r8_t([15.0e+0_dp,15.0e+0_dp,15.0e+0_dp,15.0e+0_dp])
              type(YMM4r8_t),   parameter    :: exp40 = YMM4r8_t([2.353852668370199854e+17_dp,2.353852668370199854e+17_dp, &
                                                                  2.353852668370199854e+17_dp,2.353852668370199854e+17_dp])
              type(YMM4r8_t),   parameter    :: frty  = YMM4r8_t([40.0e+0_dp,40.0e+0_dp,40.0e+0_dp,40.0e+0_dp])
              type(YMM4r8_t),   parameter    :: rec15 = YMM4r8_t([6.6666666666666666666e-2_dp,6.6666666666666666666e-2_dp, &
                                                                  6.6666666666666666666e-2_dp,6.6666666666666666666e-2_dp])
              type(YMM4r8_t),   parameter    :: two25 = YMM4r8_t([225.0e+0_dp,225.0e+0_dp,225.0e+0_dp,225.0e+0_dp])
              type(YMM4r8_t),   parameter    :: xsmall= YMM4r8_t([5.55e-17_dp,5.55e-17_dp,5.55e-17_dp,5.55e-17_dp])
              type(YMM4r8_t),   parameter    :: xinf  = YMM4r8_t([1.79e+308_dp,1.79e+308_dp,1.79e+308_dp,1.79e+308_dp])
              type(YMM4r8_t),   parameter    :: xmax  = YMM4r8_t(713.986e+0_dp)
              type(YMM4r8_t),   parameter    :: half  = YMM4r8_t(0.5e+00_dp)
              type(YMM4r8_t),   parameter    :: zero  = YMM4r8_t(0.0e+00_dp)
              type(YMM4r8_t),   parameter    :: pbar  = YMM4r8_t(3.98437500e-01_dp)
              type(YMM4r8_t),   parameter    :: p1    = YMM4r8_t(-1.9705291802535139930e-19_dp)           
              type(YMM4r8_t),   parameter    :: p2    = YMM4r8_t(-6.5245515583151902910e-16_dp)           
              type(YMM4r8_t),   parameter    :: p3    = YMM4r8_t(-1.1928788903603238754e-12_dp)           
              type(YMM4r8_t),   parameter    :: p4    = YMM4r8_t(-1.4831904935994647675e-09_dp)           
              type(YMM4r8_t),   parameter    :: p5    = YMM4r8_t(-1.3466829827635152875e-06_dp)           
              type(YMM4r8_t),   parameter    :: p6    = YMM4r8_t(-9.1746443287817501309e-04_dp)           
              type(YMM4r8_t),   parameter    :: p7    = YMM4r8_t(-4.7207090827310162436e-01_dp)           
              type(YMM4r8_t),   parameter    :: p8    = YMM4r8_t(-1.8225946631657315931e+02_dp)           
              type(YMM4r8_t),   parameter    :: p9    = YMM4r8_t(-5.1894091982308017540e+04_dp)           
              type(YMM4r8_t),   parameter    :: p10   = YMM4r8_t(-1.0588550724769347106e+07_dp)           
              type(YMM4r8_t),   parameter    :: p11   = YMM4r8_t(-1.4828267606612366099e+09_dp)           
              type(YMM4r8_t),   parameter    :: p12   = YMM4r8_t(-1.3357437682275493024e+11_dp)           
              type(YMM4r8_t),   parameter    :: p13   = YMM4r8_t(-6.9876779648010090070e+12_dp)           
              type(YMM4r8_t),   parameter    :: p14   = YMM4r8_t(-1.7732037840791591320e+14_dp)           
              type(YMM4r8_t),   parameter    :: p15   = YMM4r8_t(-1.4577180278143463643e+15_dp)
              type(YMM4r8_t),   parameter    :: q1    = YMM4r8_t(-4.0076864679904189921e+03_dp)           
              type(YMM4r8_t),   parameter    :: q2    = YMM4r8_t(7.4810580356655069138e+06_dp)            
              type(YMM4r8_t),   parameter    :: q3    = YMM4r8_t(-8.0059518998619764991e+09_dp)           
              type(YMM4r8_t),   parameter    :: q4    = YMM4r8_t(4.8544714258273622913e+12_dp)            
              type(YMM4r8_t),   parameter    :: q5    = YMM4r8_t(-1.3218168307321442305e+15_dp)
              type(YMM4r8_t),   parameter    :: pp1   = YMM4r8_t(-6.0437159056137600000e-02_dp)           
              type(YMM4r8_t),   parameter    :: pp2   = YMM4r8_t(4.5748122901933459000e-01_dp)            
              type(YMM4r8_t),   parameter    :: pp3   = YMM4r8_t(-4.2843766903304806403e-01_dp)           
              type(YMM4r8_t),   parameter    :: pp4   = YMM4r8_t(9.7356000150886612134e-02_dp)            
              type(YMM4r8_t),   parameter    :: pp5   = YMM4r8_t(-3.2457723974465568321e-03_dp)           
              type(YMM4r8_t),   parameter    :: pp6   = YMM4r8_t(-3.6395264712121795296e-04_dp)           
              type(YMM4r8_t),   parameter    :: pp7   = YMM4r8_t(1.6258661867440836395e-05_dp)           
              type(YMM4r8_t),   parameter    :: pp8   = YMM4r8_t(-3.6347578404608223492e-07_dp)
              type(YMM4r8_t),   parameter    :: qq1   = YMM4r8_t(-3.8806586721556593450e+00_dp)           
              type(YMM4r8_t),   parameter    :: qq2   = YMM4r8_t(3.2593714889036996297e+00_dp)            
              type(YMM4r8_t),   parameter    :: qq3   = YMM4r8_t(-8.5017476463217924408e-01_dp)           
              type(YMM4r8_t),   parameter    :: qq4   = YMM4r8_t(7.4212010813186530069e-02_dp)            
              type(YMM4r8_t),   parameter    :: qq5   = YMM4r8_t(-2.2835624489492512649e-03_dp)           
              type(YMM4r8_t),   parameter    :: qq6   = YMM4r8_t(3.7510433111922824643e-05_dp)
              !dir$ attributes align : 32 :: sump
              !dir$ attributes align : 32 :: sumq
              !dir$ attributes align : 32 :: x
              !dir$ attributes align : 32 :: a
              !dir$ attributes align : 32 :: b
              !dir$ attributes align : 32 :: t0
              !dir$ attributes align : 32 :: xx 
              !dir$ attributes align : 32 :: pbar 
              !dir$ attributes align : 32 :: p1
              !dir$ attributes align : 32 :: p2
              !dir$ attributes align : 32 :: p3
              !dir$ attributes align : 32 :: p4
              !dir$ attributes align : 32 :: p5
              !dir$ attributes align : 32 :: p6
              !dir$ attributes align : 32 :: p7
              !dir$ attributes align : 32 :: p8
              !dir$ attributes align : 32 :: p9
              !dir$ attributes align : 32 :: p10
              !dir$ attributes align : 32 :: p11
              !dir$ attributes align : 32 :: p12
              !dir$ attributes align : 32 :: p13
              !dir$ attributes align : 32 :: p14
              !dir$ attributes align : 32 :: p15
              !dir$ attributes align : 32 :: q1
              !dir$ attributes align : 32 :: q2
              !dir$ attributes align : 32 :: q3
              !dir$ attributes align : 32 :: q4
              !dir$ attributes align : 32 :: q5
              !dir$ attributes align : 32 :: pp1
              !dir$ attributes align : 32 :: pp2
              !dir$ attributes align : 32 :: pp3
              !dir$ attributes align : 32 :: pp4
              !dir$ attributes align : 32 :: pp5
              !dir$ attributes align : 32 :: pp6
              !dir$ attributes align : 32 :: pp7
              !dir$ attributes align : 32 :: pp8
              !dir$ attributes align : 32 :: qq1
              !dir$ attributes align : 32 :: qq2
              !dir$ attributes align : 32 :: qq3
              !dir$ attributes align : 32 :: qq4
              !dir$ attributes align : 32 :: qq5
              !dir$ attributes align : 32 :: qq6
              type(YMM4r8_t),   automatic    :: sump,sumq
              type(YMM4r8_t),   automatic    :: x,a
              type(YMM4r8_t),   automatic    :: b,t0
              type(YMM4r8_t),   automatic    :: xx
              type(Mask4_t),    automatic    :: msk1,msk2
              type(Mask4_t),    automatic    :: msk3,msk4
              type(Mask4_t),    automatic    :: msk5,mge15

              x.v    = abs(arg.v)

              msk1.m  = (x.v<xsmall.v)
              msk2.m  = (x.v<one5.v)
              mge15.m = (x.v>=one5.v)
              msk3.m  = (xmax.v<x.v)
              where(msk1.m) 
                 val.v = half.v*x.v
              else where(msk2.m)
                  xx.v  = x.v*x.v
                  sump.v= p1.v
                  sump.v= sump.v*xx.v+p2.v
                  sump.v= sump.v*xx.v+p3.v
                  sump.v= sump.v*xx.v+p4.v
                  sump.v= sump.v*xx.v+p5.v
                  sump.v= sump.v*xx.v+p6.v
                  sump.v= sump.v*xx.v+p7.v
                  sump.v= sump.v*xx.v+p8.v
                  sump.v= sump.v*xx.v+p9.v
                  sump.v= sump.v*xx.v+p10.v
                  sump.v= sump.v*xx.v+p11.v
                  sump.v= sump.v*xx.v+p12.v
                  sump.v= sump.v*xx.v+p13.v
                  sump.v= sump.v*xx.v+p14.v
                  sump.v= sump.v*xx.v+p15.v 
                  xx.v  = xx.v-two25.v
                  sumq.v= (((((  &
                            xx.v+q1.v)  &
                          * xx.v+q2.v)  &
                          * xx.v+q3.v)  &
                          * xx.v+q4.v)  &
                          * xx.v+q5.v)
                  val.v = (sump.v/sumq.v)*x.v
                  
                    
              else where
                      
                      xx.v   = one.v/x.v-rec15.v
                      sump.v = ((((((   &
                                      pp1.v     &
                               * xx.v+pp2.v)  &
                               * xx.v+pp3.v)  &
                               * xx.v+pp4.v)  &
                               * xx.v+pp5.v)  &
                               * xx.v+pp6.v)  &
                               * xx.v+pp7.v)  &
                               * xx.v+pp8.v
                      sumq.v = (((((    &
                                 xx.v+qq1.v)  &
                               * xx.v+qq2.v)  &
                               * xx.v+qq3.v)  &
                               * xx.v+qq4.v)  &
                               * xx.v+qq5.v)  &
                               * xx.v+qq6.v
                      val.v  = sump.v/sumq.v
                      msk4.m = (xmax.v-one5.v<x.v)
                      where(msk4.m)
                            a.v = exp(x.v-frty.v)
                            b.v = exp40.v
                      else where 
                          
                            a.v = exp(x.v)
                            b.v = one.v
                      end where 
                      t0.v   = val.v*a.v+pbar.v*a.v
                      val.v  = (t0.v/sqrt(x.v))*b.v
            end where  
           
             msk5.m = (arg.v<zero.v)
             where(msk5.m) 
                  val.v = -val.v
             end where 
        end subroutine calci1_ymm4r8
        
      

!
!! CALCK0 computes various K0 Bessel functions.
!
!  Discussion:
!
!    This routine computes modified Bessel functions of the second kind
!    and order zero, K0(X) and EXP(X)*K0(X), for real
!    arguments X.
!
!    The main computation evaluates slightly modified forms of near
!    minimax rational approximations generated by Russon and Blair,
!    Chalk River (Atomic Energy of Canada Limited) Report AECL-3461,
!    1969.
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
!    Input, real ( kind = 8 ) ARG, the argument.  0 < ARG is
!    always required.  If JINT = 1, then the argument must also be
!    less than XMAX.
!
!    Output, real ( kind = 8 ) RESULT, the value of the function,
!    which depends on the input value of JINT:
!    1, RESULT = K0(x);
!    2, RESULT = exp(x) * K0(x);
!
!    Input, integer ( kind = 4 ) JINT, chooses the function to be computed.
!    1, K0(x);
!    2, exp(x) * K0(x);



      
         subroutine calck0_ymm4r8(arg,val)
              
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: calck0_ymm4r8
              !dir$ attributes forceinline :: calck0_ymm4r8
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calck0_ymm4r8  
              type(YMM4r8_t),   intent(in)   :: arg
              type(YMM4r8_t),   intent(out)  :: val
                            
              type(YMM4r8_t),   parameter    :: zero   = YMM4r8_t(0.0_dp)
              type(YMM4r8_t),   parameter    :: one    = YMM4r8_t(1.0_dp)
              type(YMM4r8_t),   parameter    :: xsmall = YMM4r8_t(1.11e-16_dp)
              type(YMM4r8_t),   parameter    :: xinf   = YMM4r8_t(1.79e+308_dp)
              type(YMM4r8_t),   parameter    :: xmax   = YMM4r8_t(705.342e+00_dp)
              type(YMM4r8_t),   parameter    :: p1     = YMM4r8_t(5.8599221412826100000e-04_dp)        
	          type(YMM4r8_t),   parameter    :: p2     = YMM4r8_t(1.3166052564989571850e-01_dp)            
              type(YMM4r8_t),   parameter    :: p3     = YMM4r8_t(1.1999463724910714109e+01_dp)              
              type(YMM4r8_t),   parameter    :: p4     = YMM4r8_t(4.6850901201934832188e+02_dp)               
              type(YMM4r8_t),   parameter    :: p5     = YMM4r8_t(5.9169059852270512312e+03_dp)              
              type(YMM4r8_t),   parameter    :: p6     = YMM4r8_t(2.4708152720399552679e+03_dp)
              type(YMM4r8_t),   parameter    :: q1     = YMM4r8_t(-2.4994418972832303646e+02_dp)            
	          type(YMM4r8_t),   parameter    :: q2     = YMM4r8_t(2.1312714303849120380e+04_dp)
              type(YMM4r8_t),   parameter    :: f1     = YMM4r8_t(-1.6414452837299064100e+00_dp)
	          type(YMM4r8_t),   parameter    :: f2     = YMM4r8_t(-2.9601657892958843866e+02_dp)
              type(YMM4r8_t),   parameter    :: f3     = YMM4r8_t(-1.7733784684952985886e+04_dp)           
              type(YMM4r8_t),   parameter    :: f4     = YMM4r8_t(-4.0320340761145482298e+05_dp)
              type(YMM4r8_t),   parameter    :: g1     = YMM4r8_t(-2.5064972445877992730e+02_dp)
	          type(YMM4r8_t),   parameter    :: g2     = YMM4r8_t(2.9865713163054025489e+04_dp) 
              type(YMM4r8_t),   parameter    :: g3     = YMM4r8_t(-1.6128136304458193998e+06_dp)
              type(YMM4r8_t),   parameter    :: pp1    = YMM4r8_t(1.1394980557384778174e+02_dp)            
	          type(YMM4r8_t),   parameter    :: pp2    = YMM4r8_t(3.6832589957340267940e+03_dp)             
              type(YMM4r8_t),   parameter    :: pp3    = YMM4r8_t(3.1075408980684392399e+04_dp) 
              type(YMM4r8_t),   parameter    :: pp4    = YMM4r8_t(1.0577068948034021957e+05_dp) 
              type(YMM4r8_t),   parameter    :: pp5    = YMM4r8_t(1.7398867902565686251e+05_dp)
              type(YMM4r8_t),   parameter    :: pp6    = YMM4r8_t(1.5097646353289914539e+05_dp) 
              type(YMM4r8_t),   parameter    :: pp7    = YMM4r8_t(7.1557062783764037541e+04_dp) 
              type(YMM4r8_t),   parameter    :: pp8    = YMM4r8_t(1.8321525870183537725e+04_dp) 
              type(YMM4r8_t),   parameter    :: pp9    = YMM4r8_t(2.3444738764199315021e+03_dp) 
              type(YMM4r8_t),   parameter    :: pp10   = YMM4r8_t(1.1600249425076035558e+02_dp)
              type(YMM4r8_t),   parameter    :: qq1    = YMM4r8_t(2.0013443064949242491e+02_dp)
	          type(YMM4r8_t),   parameter    :: qq2    = YMM4r8_t(4.4329628889746408858e+03_dp)
              type(YMM4r8_t),   parameter    :: qq3    = YMM4r8_t(3.1474655750295278825e+04_dp)            
              type(YMM4r8_t),   parameter    :: qq4    = YMM4r8_t(9.7418829762268075784e+04_dp)            
              type(YMM4r8_t),   parameter    :: qq5    = YMM4r8_t(1.5144644673520157801e+05_dp)             
              type(YMM4r8_t),   parameter    :: qq6    = YMM4r8_t(1.2689839587977598727e+05_dp)             
              type(YMM4r8_t),   parameter    :: qq7    = YMM4r8_t(5.8824616785857027752e+04_dp)             
              type(YMM4r8_t),   parameter    :: qq8    = YMM4r8_t(1.4847228371802360957e+04_dp)             
              type(YMM4r8_t),   parameter    :: qq9    = YMM4r8_t(1.8821890840982713696e+03_dp)             
              type(YMM4r8_t),   parameter    :: qq10   = YMM4r8_t(9.2556599177304839811e+01_dp)
              type(YMM4r8_t),   automatic    :: sumf,sumg
              type(YMM4r8_t),   automatic    :: sump,sumq
              type(YMM4r8_t),   automatic    :: temp,x
              type(YMM4r8_t),   automatic    :: xx,t0,t1,t2
              type(Mask4_t),    automatic    :: msk1,msk2
              type(Mask4_t),    automatic    :: msk3,msk4
              !dir$ attributes align : 32 :: p1
              !dir$ attributes align : 32 :: p2 
              !dir$ attributes align : 32 :: p3 
              !dir$ attributes align : 32 :: p4 
              !dir$ attributes align : 32 :: p5 
              !dir$ attributes align : 32 :: p6 
              !dir$ attributes align : 32 :: q1 
              !dir$ attributes align : 32 :: q2 
              !dir$ attributes align : 32 :: f1 
              !dir$ attributes align : 32 :: f2 
              !dir$ attributes align : 32 :: f3 
              !dir$ attributes align : 32 :: f4 
              !dir$ attributes align : 32 :: g1 
              !dir$ attributes align : 32 :: g2 
              !dir$ attributes align : 32 :: g3 
              !dir$ attributes align : 32 :: pp1 
              !dir$ attributes align : 32 :: pp2 
              !dir$ attributes align : 32 :: pp3 
              !dir$ attributes align : 32 :: pp4 
              !dir$ attributes align : 32 :: pp5 
              !dir$ attributes align : 32 :: pp6
              !dir$ attributes align : 32 :: pp7 
              !dir$ attributes align : 32 :: pp8
              !dir$ attributes align : 32 :: pp9 
              !dir$ attributes align : 32 :: pp10
              !dir$ attributes align : 32 :: qq1
              !dir$ attributes align : 32 :: qq2
              !dir$ attributes align : 32 :: qq3
              !dir$ attributes align : 32 :: qq4
              !dir$ attributes align : 32 :: qq5
              !dir$ attributes align : 32 :: qq6
              !dir$ attributes align : 32 :: qq7
              !dir$ attributes align : 32 :: qq8
              !dir$ attributes align : 32 :: qq9
              !dir$ attributes align : 32 :: qq10
              !dir$ attributes align : 32 :: zero
              !dir$ attributes align : 32 :: one
              !dir$ attributes align : 32 :: xsmall
              !dir$ attributes align : 32 :: xinf
              !dir$ attributes align : 32 :: xmax
              !dir$ attributes align : 32 :: sumf
              !dir$ attributes align : 32 :: sumg
              !dir$ attributes align : 32 :: sump
              !dir$ attributes align : 32 :: sumq
              !dir$ attributes align : 32 :: temp
              !dir$ attributes align : 32 :: x
              !dir$ attributes align : 32 :: xx
              !dir$ attributes align : 32 :: t0
              !dir$ attributes align : 32 :: t1
              !dir$ attributes align : 32 :: t2
              x.v    = arg.v
              msk1.m = (zero.v<x.v)
              msk4.m = (xmax.v<x.v)
              where(msk1.m)
                    msk2.m = (x.v<=one.v)
                    where(msk2.m)
                        temp.v = log(x.v)
                        msk3.m = (x.v<=xsmall.v)
                        where(msk3.m)
                              val.v = p6.v/q2.v-temp.v 
                                 
                        else where 
                      
                              xx.v   = x.v*x.v
                              sump.v = ((((  &
                                        p1.v   &
                                 * xx.v+p2.v)  &
                                 * xx.v+p3.v)  &
                                 * xx.v+p4.v)  &
                                 * xx.v+p5.v)  &
                                 * xx.v+p6.v
                              sumq.v = (xx.v+q1.v) * &
                                   xx.v+q2.v
                              sumf.v = (((  &
                                         f1.v) &
                                  * xx.v+f2.v) &
                                  * xx.v+f3.v) &
                                  * xx.v+f4.v
                              sumg.v = ((xx.v+g1.v) * &
                                    xx.v+g2.v) * &
                                    xx.v+g3.v
                              !t0.v   = sump.v/sumq.v
                              !t1.v   = xx.v*sumf.v
                              !t2.v   = temp.v/sumg.v-temp.v
                              !val.v  = t0.v-t1.v*t2.v
                              val.v   = sump.v/sumq.v-xx.v*sumf.v*temp.v/sumg.v-temp.v 
                        end where    
                     
                    else where (msk4.m)
                         val.v = zero.v
                    else where 
                         xx.v  = one.v/x.v
                         t0.v  = sqrt(x.v)
                         sump.v= pp1.v
                         sump.v= sump.v*xx.v+pp2.v
                         sump.v= sump.v*xx.v+pp3.v
                         sump.v= sump.v*xx.v+pp4.v
                         sump.v= sump.v*xx.v+pp5.v
                         sump.v= sump.v*xx.v+pp6.v
                         sump.v= sump.v*xx.v+pp7.v
                         sump.v= sump.v*xx.v+pp8.v
                         sump.v= sump.v*xx.v+pp9.v
                         sump.v= sump.v*xx.v+pp10.v
                         sumq.v= xx.v
                         sumq.v= (sumq.v+qq1.v)*xx.v
                         sumq.v= (sumq.v+qq2.v)*xx.v
                         sumq.v= (sumq.v+qq3.v)*xx.v
                         sumq.v= (sumq.v+qq4.v)*xx.v
                         sumq.v= (sumq.v+qq5.v)*xx.v
                         sumq.v= (sumq.v+qq6.v)*xx.v
                         sumq.v= (sumq.v+qq7.v)*xx.v
                         sumq.v= (sumq.v+qq8.v)*xx.v
                         sumq.v= (sumq.v+qq9.v)*xx.v 
                         sumq.v= sumq.v+qq10.v
                         val.v = sump.v/sumq.v/t0.v
                         val.v = val.v*exp(-x.v)
                    end where 
            else where 
                 val.v = xinf.v
            end where 

         end subroutine calck0_ymm4r8
         
      

!
!! CALCK1 computes various K1 Bessel functions.
!
!  Discussion:
!
!    This routine computes modified Bessel functions of the second kind
!    and order one, K1(X) and EXP(X)*K1(X), for real arguments X.
!
!    The main computation evaluates slightly modified forms of near
!    minimax rational approximations generated by Russon and Blair,
!    Chalk River (Atomic Energy of Canada Limited) Report AECL-3461,
!    1969.
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
!    Input, real ( kind = 8 ) ARG, the argument.  XLEAST < ARG is
!    always required.  If JINT = 1, then the argument must also be
!    less than XMAX.
!
!    Output, real ( kind = 8 ) RESULT, the value of the function,
!    which depends on the input value of JINT:
!    1, RESULT = K1(x);
!    2, RESULT = exp(x) * K1(x);
!
!    Input, integer ( kind = 4 ) JINT, chooses the function to be computed.
!    1, K1(x);
!    2, exp(x) * K1(x);  



       subroutine calck1_ymm4r8(arg,val)
              
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: calck1_ymm4r8
              !dir$ attributes forceinline :: calck1_ymm4r8
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calck1_ymm4r8  
              type(YMM4r8_t),   intent(in)   :: arg
              type(YMM4r8_t),   intent(out)  :: val
              
              type(YMM4r8_t),   automatic    :: sumf,sumg
              type(YMM4r8_t),   automatic    :: sump,sumq
              type(YMM4r8_t),   automatic    :: x,xx
              type(YMM4r8_t),   automatic    :: t0,t1,t2
              type(Mask4_t),    automatic    :: msk1,msk2
              type(Mask4_t),    automatic    :: msk3,msk4  
              type(YMM4r8_t),   parameter    :: zero   = YMM4r8_t(0.0_dp)
              type(YMM4r8_t),   parameter    :: one    = YMM4r8_t(1.0_dp)
              type(YMM4r8_t),   parameter    :: xsmall = YMM4r8_t(1.11e-16_dp)
              type(YMM4r8_t),   parameter    :: xinf   = YMM4r8_t(1.79e+308_dp)
              type(YMM4r8_t),   parameter    :: xmax   = YMM4r8_t(705.342e+00_dp) 
              type(YMM4r8_t),   parameter    :: xleast = YMM4r8_t(2.23e-308_dp)
              type(YMM4r8_t),   parameter    :: p1     = YMM4r8_t(4.8127070456878442310e-1_dp)
              type(YMM4r8_t),   parameter    :: p2     = YMM4r8_t(9.9991373567429309922e+1_dp)
              type(YMM4r8_t),   parameter    :: p3     = YMM4r8_t(7.1885382604084798576e+3_dp)
              type(YMM4r8_t),   parameter    :: p4     = YMM4r8_t(1.7733324035147015630e+5_dp)
              type(YMM4r8_t),   parameter    :: p5     = YMM4r8_t(7.1938920065420586101e+5_dp)
              type(YMM4r8_t),   parameter    :: q1     = YMM4r8_t(-2.8143915754538725829e+2_dp)
              type(YMM4r8_t),   parameter    :: q2     = YMM4r8_t(3.7264298672067697862e+4_dp) 
              type(YMM4r8_t),   parameter    :: q3     = YMM4r8_t(-2.2149374878243304548e+6_dp)
              type(YMM4r8_t),   parameter    :: f1     = YMM4r8_t(-2.2795590826955002390e-1_dp)
              type(YMM4r8_t),   parameter    :: f2     = YMM4r8_t(-5.3103913335180275253e+1_dp) 
              type(YMM4r8_t),   parameter    :: f3     = YMM4r8_t(-4.5051623763436087023e+3_dp) 
              type(YMM4r8_t),   parameter    :: f4     = YMM4r8_t(-1.4758069205414222471e+5_dp) 
              type(YMM4r8_t),   parameter    :: f5     = YMM4r8_t(-1.3531161492785421328e+6_dp)
              type(YMM4r8_t),   parameter    :: g1     = YMM4r8_t(-3.0507151578787595807e+2_dp)
              type(YMM4r8_t),   parameter    :: g2     = YMM4r8_t(4.3117653211351080007e+4_dp)
              type(YMM4r8_t),   parameter    :: g3     = YMM4r8_t(-2.7062322985570842656e+6_dp)
              type(YMM4r8_t),   parameter    :: pp1    = YMM4r8_t(6.4257745859173138767e-2_dp)
              type(YMM4r8_t),   parameter    :: pp2    = YMM4r8_t(7.5584584631176030810e+0_dp)
              type(YMM4r8_t),   parameter    :: pp3    = YMM4r8_t(1.3182609918569941308e+2_dp)
              type(YMM4r8_t),   parameter    :: pp4    = YMM4r8_t(8.1094256146537402173e+2_dp)
              type(YMM4r8_t),   parameter    :: pp5    = YMM4r8_t(2.3123742209168871550e+3_dp)
              type(YMM4r8_t),   parameter    :: pp6    = YMM4r8_t(3.4540675585544584407e+3_dp)
              type(YMM4r8_t),   parameter    :: pp7    = YMM4r8_t(2.8590657697910288226e+3_dp)
              type(YMM4r8_t),   parameter    :: pp8    = YMM4r8_t(1.3319486433183221990e+3_dp)
              type(YMM4r8_t),   parameter    :: pp9    = YMM4r8_t(3.4122953486801312910e+2_dp)
              type(YMM4r8_t),   parameter    :: pp10   = YMM4r8_t(4.4137176114230414036e+1_dp)
              type(YMM4r8_t),   parameter    :: pp11   = YMM4r8_t(2.2196792496874548962e+0_dp) 
              type(YMM4r8_t),   parameter    :: qq1    = YMM4r8_t(3.6001069306861518855e+1_dp)
              type(YMM4r8_t),   parameter    :: qq2    = YMM4r8_t(3.3031020088765390854e+2_dp)
              type(YMM4r8_t),   parameter    :: qq3    = YMM4r8_t(1.2082692316002348638e+3_dp)
              type(YMM4r8_t),   parameter    :: qq4    = YMM4r8_t(2.1181000487171943810e+3_dp)
              type(YMM4r8_t),   parameter    :: qq5    = YMM4r8_t(1.9448440788918006154e+3_dp)
              type(YMM4r8_t),   parameter    :: qq6    = YMM4r8_t(9.6929165726802648634e+2_dp)
              type(YMM4r8_t),   parameter    :: qq7    = YMM4r8_t(2.5951223655579051357e+2_dp)
              type(YMM4r8_t),   parameter    :: qq8    = YMM4r8_t(3.4552228452758912848e+1_dp)
              type(YMM4r8_t),   parameter    :: qq9    = YMM4r8_t(1.7710478032601086579e+0_dp)
               
              !dir$ attributes align : 32 :: zero
              !dir$ attributes align : 32 :: one
              !dir$ attributes align : 32 :: xsmall
              !dir$ attributes align : 32 :: xinf
              !dir$ attributes align : 32 :: xmax
              !dir$ attributes align : 32 :: xleast 
              !dir$ attributes align : 32 :: sumf
              !dir$ attributes align : 32 :: sumg
              !dir$ attributes align : 32 :: sump
              !dir$ attributes align : 32 :: sumq
              !dir$ attributes align : 32 :: x
              !dir$ attributes align : 32 :: xx
              !dir$ attributes align : 32 :: t0
              !dir$ attributes align : 32 :: t1
              !dir$ attributes align : 32 :: t2
              !dir$ attributes align : 32 :: p1 
              !dir$ attributes align : 32 :: p2 
              !dir$ attributes align : 32 :: p3 
              !dir$ attributes align : 32 :: p4 
              !dir$ attributes align : 32 :: p5 
              !dir$ attributes align : 32 :: q1 
              !dir$ attributes align : 32 :: q2 
              !dir$ attributes align : 32 :: q3 
              !dir$ attributes align : 32 :: f1 
              !dir$ attributes align : 32 :: f2 
              !dir$ attributes align : 32 :: f3 
              !dir$ attributes align : 32 :: f4 
              !dir$ attributes align : 32 :: f5 
              !dir$ attributes align : 32 :: g1 
              !dir$ attributes align : 32 :: g2 
              !dir$ attributes align : 32 :: g3 
              !dir$ attributes align : 32 :: pp1 
              !dir$ attributes align : 32 :: pp2 
              !dir$ attributes align : 32 :: pp3 
              !dir$ attributes align : 32 :: pp4 
              !dir$ attributes align : 32 :: pp5 
              !dir$ attributes align : 32 :: pp6 
              !dir$ attributes align : 32 :: pp7 
              !dir$ attributes align : 32 :: pp8 
              !dir$ attributes align : 32 :: pp9 
              !dir$ attributes align : 32 :: pp10
              !dir$ attributes align : 32 :: pp11
              !dir$ attributes align : 32 :: qq1
              !dir$ attributes align : 32 :: qq2
              !dir$ attributes align : 32 :: qq3
              !dir$ attributes align : 32 :: qq4
              !dir$ attributes align : 32 :: qq5
              !dir$ attributes align : 32 :: qq6
              !dir$ attributes align : 32 :: qq7
              !dir$ attributes align : 32 :: qq8
              !dir$ attributes align : 32 :: qq9
              x.v    = arg.v
              msk1.m = (x.v<=xleast.v)
              msk2.m = (x.v<=one.v)
              msk4.m = (xmax.v<x.v)
              where(msk1.m)
                  val.v = xinf.v
              else where(msk2.m)
                  msk3.m = (x.v<xsmall.v)
                  where(msk3.m)
                      val.v = one.v/x.v
                  else where 
                      xx.v    = x.v*x.v
                      sump.v  = ((((    &
                                    p1.v       &
                               *    xx.v+p2.v) & 
                               *    xx.v+p3.v) &
                               *    xx.v+p4.v) &
                               *    xx.v+p5.v) &
                               *    xx.v+q3.v
                      sumq.v   = ((     &
                                    xx.v+q1.v) &
                                  * xx.v+q2.v) &
                                  * xx.v+q3.v
                      t1.v     = sump.v/sumq.v  
                      sumf.v   = (((    &
                                    f1.v       &
                               *    xx.v+f2.v) &
                               *    xx.v+f3.v) &
                               *    xx.v+f4.v) &
                               *    xx.v+f5.v
                      t2.v     = xx.v*log(x.v)
                      sumg.v   = ((    &
                                    xx.v+g1.v) &
                               *    xx.v+g2.v) &
                               *    xx.v+g3.v
                      t0.v     = sumf.v/sumg.v
                      val.v    = (t2.v*t0.v+t1.v)/x.v
                    
                  end where 
             else where(msk4.m)
                      val.v = zero.v
             else where 
                      xx.v  = one.v/x.v
                      sump.v= pp1.v
                      sump.v= sump.v*xx.v+pp2.v
                      sump.v= sump.v*xx.v+pp3.v
                      sump.v= sump.v*xx.v+pp4.v
                      sump.v= sump.v*xx.v+pp5.v
                      sump.v= sump.v*xx.v+pp6.v
                      sump.v= sump.v*xx.v+pp7.v
                      sump.v= sump.v*xx.v+pp8.v
                      sump.v= sump.v*xx.v+pp9.v
                      sump.v= sump.v*xx.v+pp10.v
                      sump.v= sump.v*xx.v+pp11.v
                      t0.v  = sqrt(x.v)
                      sumq.v= xx.v
                      sumq.v= (sumq.v+qq1.v)*xx.v
                      sumq.v= (sumq.v+qq2.v)*xx.v
                      sumq.v= (sumq.v+qq3.v)*xx.v
                      sumq.v= (sumq.v+qq4.v)*xx.v
                      sumq.v= (sumq.v+qq5.v)*xx.v
                      sumq.v= (sumq.v+qq6.v)*xx.v
                      sumq.v= (sumq.v+qq7.v)*xx.v
                      sumq.v= (sumq.v+qq8.v)*xx.v
                      sumq.v= sumq.v+qq9.v
                      val.v = sump.v/sumq.v/t0.v
                      val.v = val.v*exp(-x.v)
             end where 

        end subroutine calck1_ymm4r8
        
        







     
      


end module 