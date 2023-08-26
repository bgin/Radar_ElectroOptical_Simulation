

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
    use mod_vectypes, only : ZMM16r4_t,Mask16_t
    
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
     
     !dir$ attributes align : 64 :: sn0
     !dir$ attributes align : 64 :: sn1
     !dir$ attributes align : 64 :: sn2
     !dir$ attributes align : 64 :: sn3
     !dir$ attributes align : 64 :: sn4
     !dir$ attributes align : 64 :: sn5
     type(ZMM16r4_t), save :: sn0 = ZMM16r4_t(-2.99181919401019853726E3_sp)
     type(ZMM16r4_t), save :: sn1 = ZMM16r4_t(7.08840045257738576863E5_sp)
     type(ZMM16r4_t), save :: sn2 = ZMM16r4_t(-6.29741486205862506537E7_sp)
     type(ZMM16r4_t), save :: sn3 = ZMM16r4_t(2.54890880573376359104E9_sp)
     type(ZMM16r4_t), save :: sn4 = ZMM16r4_t(-4.42979518059697779103E10_sp)
     type(ZMM16r4_t), save :: sn5 = ZMM16r4_t(3.18016297876567817986E11_sp)
     
     !dir$ attributes align : 64 :: sd0
     !dir$ attributes align : 64 :: sd1
     !dir$ attributes align : 64 :: sd2
     !dir$ attributes align : 64 :: sd3
     !dir$ attributes align : 64 :: sd4
     !dir$ attributes align : 64 :: sd5
     type(ZMM16r4_t), save :: sd0 = ZMM16r4_t(2.81376268889994315696E2_sp)
     type(ZMM16r4_t), save :: sd1 = ZMM16r4_t(4.55847810806532581675E4_sp)
     type(ZMM16r4_t), save :: sd2 = ZMM16r4_t(5.17343888770096400730E6_sp)
     type(ZMM16r4_t), save :: sd3 = ZMM16r4_t(4.19320245898111231129E8_sp)
     type(ZMM16r4_t), save :: sd4 = ZMM16r4_t(2.24411795645340920940E10_sp)
     type(ZMM16r4_t), save :: sd5 = ZMM16r4_t(6.07366389490084639049E11_sp)
     
     !dir$ attributes align : 64 :: cn0
     !dir$ attributes align : 64 :: cn1
     !dir$ attributes align : 64 :: cn2
     !dir$ attributes align : 64 :: cn3
     !dir$ attributes align : 64 :: cn4
     !dir$ attributes align : 64 :: cn5
     type(ZMM16r4_t), save :: cn0 = ZMM16r4_t(-4.98843114573573548651E-8_sp)
     type(ZMM16r4_t), save :: cn1 = ZMM16r4_t(9.50428062829859605134E-6_sp)
     type(ZMM16r4_t), save :: cn2 = ZMM16r4_t(-6.45191435683965050962E-4_sp)
     type(ZMM16r4_t), save :: cn3 = ZMM16r4_t(1.88843319396703850064E-2_sp)
     type(ZMM16r4_t), save :: cn4 = ZMM16r4_t(-2.05525900955013891793E-1_sp)
     type(ZMM16r4_t), save :: cn5 = ZMM16r4_t(9.99999999999999998822E-1_sp)
     
     !dir$ attributes align : 64 :: cd0
     !dir$ attributes align : 64 :: cd1
     !dir$ attributes align : 64 :: cd2
     !dir$ attributes align : 64 :: cd3
     !dir$ attributes align : 64 :: cd4
     !dir$ attributes align : 64 :: cd5
     type(ZMM16r4_t), save :: cd0 = ZMM16r4_t(3.99982968972495980367E-12_sp)
     type(ZMM16r4_t), save :: cd1 = ZMM16r4_t(9.15439215774657478799E-10_sp)
     type(ZMM16r4_t), save :: cd2 = ZMM16r4_t(1.25001862479598821474E-7_sp)
     type(ZMM16r4_t), save :: cd3 = ZMM16r4_t(1.22262789024179030997E-5_sp)
     type(ZMM16r4_t), save :: cd4 = ZMM16r4_t(8.68029542941784300606E-4_sp)
     type(ZMM16r4_t), save :: cd5 = ZMM16r4_t(4.12142090722199792936E-2)
     type(ZMM16r4_t), save :: cd6 = ZMM16r4_t(1.00000000000000000118E0_sp)
     
     !dir$ attributes align : 64 :: fn0
     !dir$ attributes align : 64 :: fn1
     !dir$ attributes align : 64 :: fn2
     !dir$ attributes align : 64 :: fn3
     !dir$ attributes align : 64 :: fn4
     !dir$ attributes align : 64 :: fn5
     !dir$ attributes align : 64 :: fn6
     !dir$ attributes align : 64 :: fn7
     !dir$ attributes align : 64 :: fn8
     !dir$ attributes align : 64 :: fn9
     type(ZMM16r4_t), save :: fn0 = ZMM16r4_t(4.21543555043677546506E-1_sp)
     type(ZMM16r4_t), save :: fn1 = ZMM16r4_t(1.43407919780758885261E-1_sp)
     type(ZMM16r4_t), save :: fn2 = ZMM16r4_t(1.15220955073585758835E-2_sp)
     type(ZMM16r4_t), save :: fn3 = ZMM16r4_t(3.45017939782574027900E-4_sp)
     type(ZMM16r4_t), save :: fn4 = ZMM16r4_t(4.63613749287867322088E-6_sp)
     type(ZMM16r4_t), save :: fn5 = ZMM16r4_t(3.05568983790257605827E-8_sp)
     type(ZMM16r4_t), save :: fn6 = ZMM16r4_t(1.02304514164907233465E-10_sp)
     type(ZMM16r4_t), save :: fn7 = ZMM16r4_t(1.72010743268161828879E-13_sp)
     type(ZMM16r4_t), save :: fn8 = ZMM16r4_t(1.34283276233062758925E-16_sp)
     type(ZMM16r4_t), save :: fn9 = ZMM16r4_t(3.76329711269987889006E-20_sp)
     
     !dir$ attributes align : 64 :: fd0
     !dir$ attributes align : 64 :: fd1
     !dir$ attributes align : 64 :: fd2
     !dir$ attributes align : 64 :: fd3
     !dir$ attributes align : 64 :: fd4
     !dir$ attributes align : 64 :: fd5
     !dir$ attributes align : 64 :: fd6
     !dir$ attributes align : 64 :: fd7
     !dir$ attributes align : 64 :: fd8
     !dir$ attributes align : 64 :: fd9
     type(ZMM16r4_t), save :: fd0 = ZMM16r4_t(7.51586398353378947175E-1_sp)
     type(ZMM16r4_t), save :: fd1 = ZMM16r4_t(1.16888925859191382142E-1_sp)
     type(ZMM16r4_t), save :: fd2 = ZMM16r4_t(6.44051526508858611005E-3_sp)
     type(ZMM16r4_t), save :: fd3 = ZMM16r4_t(1.55934409164153020873E-4_sp)
     type(ZMM16r4_t), save :: fd4 = ZMM16r4_t(1.84627567348930545870E-6_sp)
     type(ZMM16r4_t), save :: fd5 = ZMM16r4_t(1.12699224763999035261E-8_sp)
     type(ZMM16r4_t), save :: fd6 = ZMM16r4_t(3.60140029589371370404E-11_sp)
     type(ZMM16r4_t), save :: fd7 = ZMM16r4_t(5.88754533621578410010E-14_sp)
     type(ZMM16r4_t), save :: fd8 = ZMM16r4_t(4.52001434074129701496E-17_sp)
     type(ZMM16r4_t), save :: fd9 = ZMM16r4_t(1.25443237090011264384E-20_sp)
     
     !dir$ attributes align : 64 :: gn0
     !dir$ attributes align : 64 :: gn1
     !dir$ attributes align : 64 :: gn2
     !dir$ attributes align : 64 :: gn3
     !dir$ attributes align : 64 :: gn4
     !dir$ attributes align : 64 :: gn5
     !dir$ attributes align : 64 :: gn6
     !dir$ attributes align : 64 :: gn7
     !dir$ attributes align : 64 :: gn8
     !dir$ attributes align : 64 :: gn9
     !dir$ attributes align : 64 :: gn10
     type(ZMM16r4_t), save :: gn0 = ZMM16r4_t(5.04442073643383265887E-1_sp)
     type(ZMM16r4_t), save :: gn1 = ZMM16r4_t(1.97102833525523411709E-1_sp)
     type(ZMM16r4_t), save :: gn2 = ZMM16r4_t(1.87648584092575249293E-2_sp)
     type(ZMM16r4_t), save :: gn3 = ZMM16r4_t(6.84079380915393090172E-4_sp)
     type(ZMM16r4_t), save :: gn4 = ZMM16r4_t(1.15138826111884280931E-5_sp)
     type(ZMM16r4_t), save :: gn5 = ZMM16r4_t(9.82852443688422223854E-8_sp)
     type(ZMM16r4_t), save :: gn6 = ZMM16r4_t(4.45344415861750144738E-10_sp)
     type(ZMM16r4_t), save :: gn7 = ZMM16r4_t(1.08268041139020870318E-12_sp)
     type(ZMM16r4_t), save :: gn8 = ZMM16r4_t(1.37555460633261799868E-15_sp)
     type(ZMM16r4_t), save :: gn9 = ZMM16r4_t(8.36354435630677421531E-19_sp)
     type(ZMM16r4_t), save :: gn10= ZMM16r4_t(1.86958710162783235106E-22_sp)
     
     !dir$ attributes align : 64 :: gd0
     !dir$ attributes align : 64 :: gd1
     !dir$ attributes align : 64 :: gd2
     !dir$ attributes align : 64 :: gd3
     !dir$ attributes align : 64 :: gd4
     !dir$ attributes align : 64 :: gd5
     !dir$ attributes align : 64 :: gd6
     !dir$ attributes align : 64 :: gd7
     !dir$ attributes align : 64 :: gd8
     !dir$ attributes align : 64 :: gd9
     !dir$ attributes align : 64 :: gd10
     type(ZMM16r4_t), save :: gd0 = ZMM16r4_t(1.47495759925128324529E0_sp)
     type(ZMM16r4_t), save :: gd1 = ZMM16r4_t(3.37748989120019970451E-1_sp)
     type(ZMM16r4_t), save :: gd2 = ZMM16r4_t(2.53603741420338795122E-2_sp)
     type(ZMM16r4_t), save :: gd3 = ZMM16r4_t(8.14679107184306179049E-4_sp)
     type(ZMM16r4_t), save :: gd4 = ZMM16r4_t(1.27545075667729118702E-5_sp)
     type(ZMM16r4_t), save :: gd5 = ZMM16r4_t(1.04314589657571990585E-7_sp)
     type(ZMM16r4_t), save :: gd6 = ZMM16r4_t(4.60680728146520428211E-10_sp)
     type(ZMM16r4_t), save :: gd7 = ZMM16r4_t(1.10273215066240270757E-12_sp)
     type(ZMM16r4_t), save :: gd8 = ZMM16r4_t(1.38796531259578871258E-15_sp)
     type(ZMM16r4_t), save :: gd9 = ZMM16r4_t(8.39158816283118707363E-19_sp)
     type(ZMM16r4_t), save :: gd10= ZMM16r4_t( 1.86958710162783236342E-22_sp)
     
     contains
     
     
     
        !!
        !! Helper function for bringing into L1D cache the
        !! 'saved' constant data , i.e. sn.
        !!
        pure function preload_sn() result(s)
             
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: preload_sn
              !dir$ attributes forceinline :: preload_sn
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: preload_sn
              type(ZMM16r4_t) :: s
              type(ZMM16r4_t), automatic :: t0,t1,t2
              t0.v = sn0.v+sn1.v
              t1.v = sn2.v+sn3.v
              t2.v = sn4.v+sn5.v
              s.v  = t0.v+t1.v+t2.v
        end function preload_sn
        
        
        !!
        !! Helper function for bringing into L1D cache the
        !! 'saved' constant data , i.e. sd.
        !!
        pure function preload_sd() result(s)
             
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: preload_sd
              !dir$ attributes forceinline :: preload_sd
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: preload_sd
              type(ZMM16r4_t) :: s
              type(ZMM16r4_t), automatic :: t0,t1,t2
              t0.v = sd0.v+sd1.v
              t1.v = sd2.v+sd3.v
              t2.v = sd4.v+sd5.v
              s.v  = t0.v+t1.v+t2.v
        end function preload_sd
        
        
        !!
        !! Helper function for bringing into L1D cache the
        !! 'saved' constant data , i.e. cn.
        !!
        pure function preload_cn() result(s)
             
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: preload_cn
              !dir$ attributes forceinline :: preload_cn
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: preload_cn
              type(ZMM16r4_t) :: s
              type(ZMM16r4_t), automatic :: t0,t1,t2
              t0.v = cn0.v+cn1.v
              t1.v = cn2.v+cn3.v
              t2.v = cn4.v+cn5.v
              s.v  = t0.v+t1.v+t2.v
        end function preload_cn
        
        
        !!
        !! Helper function for bringing into L1D cache the
        !! 'saved' constant data , i.e. cd.
        !!
        pure function preload_cd() result(s)
             
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: preload_cd
              !dir$ attributes forceinline :: preload_cd
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: preload_cd
              type(ZMM16r4_t) :: s
              type(ZMM16r4_t), automatic :: t0,t1,t2
              t0.v = cd0.v+cd1.v
              t1.v = cd2.v+cd3.v
              t2.v = cd4.v+cd5.v+cd6.v
              s.v  = t0.v+t1.v+t2.v
        end function preload_cd
        
        
        !!
        !! Helper function for bringing into L1D cache the
        !! 'saved' constant data , i.e. fn.
        !!
        pure function preload_fn() result(s)
             
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: preload_fn
              !dir$ attributes forceinline :: preload_fn
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: preload_fn
              type(ZMM16r4_t) :: s
              type(ZMM16r4_t), automatic :: t0,t1,t2,t3,t4
              t0.v = fn0.v+fn1.v
              t1.v = fn2.v+fn3.v
              t2.v = fn4.v+fn5.v
              t3.v = fn6.v+fn7.v
              t4.v = fn8.v+fn9.v
              s.v  = t0.v+t1.v+t2.v+t3.v+t4.v
        end function preload_fn
        
        
          !!
        !! Helper function for bringing into L1D cache the
        !! 'saved' constant data , i.e. fd.
        !!
        pure function preload_fd() result(s)
             
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: preload_fd
              !dir$ attributes forceinline :: preload_fd
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: preload_fd
              type(ZMM16r4_t) :: s
              type(ZMM16r4_t), automatic :: t0,t1,t2,t3,t4
              t0.v = fd0.v+fd1.v
              t1.v = fd2.v+fd3.v
              t2.v = fd4.v+fd5.v
              t3.v = fd6.v+fd7.v
              t4.v = fd8.v+fd9.v
              s.v  = t0.v+t1.v+t2.v+t3.v+t4.v
        end function preload_fd
        
        
        !!
        !! Helper function for bringing into L1D cache the
        !! 'saved' constant data , i.e. gn.
        !!
        pure function preload_gn() result(s)
             
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: preload_gn
              !dir$ attributes forceinline :: preload_gn
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: preload_gn
              type(ZMM16r4_t) :: s
              type(ZMM16r4_t), automatic :: t0,t1,t2,t3,t4
              t0.v = gn0.v+gn1.v
              t1.v = gn2.v+gn3.v
              t2.v = gn4.v+gn5.v
              t3.v = gn6.v+gn7.v
              t4.v = gn8.v+gn9.v+gn10.v
              s.v  = t0.v+t1.v+t2.v+t3.v+t4.v
        end function preload_gn
        
        
        !!
        !! Helper function for bringing into L1D cache the
        !! 'saved' constant data , i.e. gn.
        !!
        pure function preload_gd() result(s)
             
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: preload_gd
              !dir$ attributes forceinline :: preload_gd
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: preload_gd
              type(ZMM16r4_t) :: s
              type(ZMM16r4_t), automatic :: t0,t1,t2,t3,t4
              t0.v = gd0.v+gd1.v
              t1.v = gd2.v+gd3.v
              t2.v = gd4.v+gd5.v
              t3.v = gd6.v+gd7.v
              t4.v = gd8.v+gd9.v+gd10.v
              s.v  = t0.v+t1.v+t2.v+t3.v+t4.v
        end function preload_gd
        
        
        
        
        
        
        
        
     
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
        
        
#if 0

c*********************************************************************72
c
cc RC computes the elementary integral RC(X,Y).
c
c  Discussion:
c
c    This function computes the elementary integral
c
c      RC(X,Y) = Integral ( 0 <= T < oo )
c
c                              -1/2     -1
c                    (1/2)(T+X)    (T+Y)  DT,
c
c    where X is nonnegative and Y is positive.  The duplication
c    theorem is iterated until the variables are nearly equal,
c    and the function is then expanded in Taylor series to fifth
c    order.  
c
c    Logarithmic, inverse circular, and inverse hyperbolic 
c    functions can be expressed in terms of RC.  
c
c    Check by addition theorem: 
c
c      RC(X,X+Z) + RC(Y,Y+Z) = RC(0,Z),
c      where X, Y, and Z are positive and X * Y = Z * Z.
c
c  Modified:
c
c    27 May 2018
c
c  Author:
c
c    Bille Carlson, Elaine Notis
c
c  Reference:
c
c    Bille Carlson,
c    Computing Elliptic Integrals by Duplication,
c    Numerische Mathematik,
c    Volume 33, 1979, pages 1-16.
c
c    Bille Carlson, Elaine Notis,
c    Algorithm 577, Algorithms for Incomplete Elliptic Integrals,
c    ACM Transactions on Mathematical Software,
c    Volume 7, Number 3, pages 398-403, September 1981.
c
c  Parameters:
c
c    Input, double precision X, Y, the arguments in the integral.
c
c    Input, double precision ERRTOL, the error tolerance.
c    Relative error due to truncation is less than
c      16 * ERRTOL ^ 6 / (1 - 2 * ERRTOL).
c    Sample choices:  
c      ERRTOL   Relative truncation error less than
c      1.D-3    2.D-17
c      3.D-3    2.D-14
c      1.D-2    2.D-11
c      3.D-2    2.D-8
c      1.D-1    2.D-5
c
c    Output, integer IERR, the error flag.
c    0, no error occurred.
c    1, abnormal termination.
c

#endif        

 

        pure function rc_zmm16r4(x,y,errt) result(rc)
             
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: rc_zmm16r4
              !dir$ attributes forceinline :: rc_zmm16r4
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rc_zmm16r4
              use mod_vecconsts, only : v16_1
              type(ZMM16r4_t),   intent(in) :: x
              type(ZMM16r4_t),   intent(in) :: y
              type(ZMM16r4_t),   intent(in) :: errt
              type(ZMM16r4_t)  :: rc
              !dir$ attributes align : 64 :: C0142857142857142857142857142857 
              !dir$ attributes align : 64 :: C0409090909090909090909090909091
              !dir$ attributes align : 64 :: C0333333333333333333333333333333
              !dir$ attributes align : 64 :: C0375
              !dir$ attributes align : 64 :: C20
              !dir$ attributes align : 64 :: C025
              !dir$ attributes align : 64 :: C03
              !dir$ attributes align : 64 :: xn
              !dir$ attributes align : 64 :: yn
              !dir$ attributes align : 64 :: mu
              !dir$ attributes align : 64 :: s
              !dir$ attributes align : 64 :: sn
              !dir$ attributes align : 64 :: lamda
              !dir$ attributes align : 64 :: sxn
              !dir$ attributes align : 64 :: syn
              type(ZMM16r4_t),  parameter :: C0142857142857142857142857142857 = &
                                                 ZMM16r4_t(0.142857142857142857142857142857_sp)
              type(ZMM16r4_t),  parameter :: C0409090909090909090909090909091 = &
                                                 ZMM16r4_t(0.409090909090909090909090909091_sp)
              type(ZMM16r4_t),  parameter :: C0333333333333333333333333333333 = &
                                                 ZMM16r4_t(0.333333333333333333333333333333_sp)
              type(ZMM16r4_t),  parameter :: C0375 = ZMM16r4_t(0.375_sp)
              type(ZMM16r4_t),  parameter :: C20   = ZMM16r4_t(2.0_sp)
              type(ZMM16r4_t),  parameter :: C025  = ZMM16r4_t(0.25_sp)
              type(ZMM16r4_t),  parameter :: C03   = ZMM16r4_t(0.3_sp)
              type(ZMM16r4_t),  automatic :: xn,yn
              type(ZMM16r4_t),  automatic :: mu,s
              type(ZMM16r4_t),  automatic :: sn,lamda
              type(ZMM16r4_t),  automatic :: t0,t1,t2
              type(ZMM16r4_t),  automatic :: sxn,syn
              type(Mask16_t),   automatic :: msk
#if (GMS_EXPLICIT_VECTORIZE) == 1
              integer(kind=i4) :: j
#endif              
              xn.v = x.v
              yn.v = y.v

#if (GMS_EXPLICIT_VECTORIZE) == 1
              do while (.true.)
                      !dir$ loop_count(16)
                      !dir$ vector aligned
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                      do j=0,15    
                         mu.v(j) = (xn.v(j)+yn.v(j)+yn.v(j))* &
                                    C0333333333333333333333333333333.v(j)
                         sn.v(j) = (yn.v(j)+mu.v(j))/(mu.v(j)-C20.v(j))
                         msk.m(j)= (abs(sn.v(j))<errt.v(j))
                         if(msk.m(j)) then
                            t0.v(j) = sn.v(j)* &
                                       C0409090909090909090909090909091.v(j)+C0375.v(j)
                            t1.v(j) = t0.v(j)*sn.v(j)+ &
                                       C0142857142857142857142857142857.v(j)
                            t2.v(j) = t1.v(j)*sn.v(j)+C03.v(j)
                            s.v(j)  = t2.v(j)*sn.v(j)*sn.v(j)
                            rc.v(j) = (v16_1.v(j)+s.v(j))/sqrt(mu.v(j))
                            return
                         end if
                         sxn.v(j)  = sqrt(xn.v(j))
                         syn.v(j)  = sqrt(yn.v(j))
                         lamda.v(j)= C20.v(j)*sxn.v(j)*syn.v(j)+yn.v(j)
                         xn.v(j)   = C025.v(j)*(xn.v(j)+lamda.v(j))
                         yn.v(j)   = C025.v(j)*(yn.v(j)+lamda.v(j))
                      end do 
              end do
#else
             
              do while (.true.)
                 mu.v = (xn.v+yn.v+yn.v)* &
                        C0333333333333333333333333333333.v
                 sn.v = (yn.v+mu.v)/(mu.v-C20.v)
                 msk.m= (abs(sn.v)<errt.v)
                 if(all(msk.m)) then
                    t0.v  = sn.v* &
                            C0409090909090909090909090909091.v+C0375.v
                    t1.v  = t0.v*sn.v+ &
                            C0142857142857142857142857142857.v
                    t2.v  = t1.v*sn.v+C03.v
                    s.v   = t2.v*sn.v*sn.v
                    rc.v  = (v16_1.v+s.v)/sqrt(mu.v)
                    return
                 end if
                 sxn.v  = sqrt(xn.v)
                 syn.v  = sqrt(yn.v)
                 lamda.v= C20.v*sxn.v*syn.v+yn.v
                 xn.v   = C025.v*(xn.v+lamda.v)
                 yn.v   = C025.v*(yn.v+lamda.v)
              end do
#endif
        end function rc_zmm16r4
        
        
#if 0

  c*********************************************************************72
c
cc RD computes an incomplete elliptic integral of the second kind, RD(X,Y,Z).
c
c  Discussion:
c
c    This function computes an incomplete elliptic integral of the second kind.
c
c    RD(X,Y,Z) = Integral ( 0 <= T < oo )
c
c                                -1/2     -1/2     -3/2
c                      (3/2)(T+X)    (T+Y)    (T+Z)    DT,
c
c    where X and Y are nonnegative, X + Y is positive, and Z is positive.
c
c    If X or Y is zero, the integral is complete.
c
c    The duplication theorem is iterated until the variables are
c    nearly equal, and the function is then expanded in Taylor
c    series to fifth order.  
c
c    Check: 
c
c      RD(X,Y,Z) + RD(Y,Z,X) + RD(Z,X,Y) = 3 / sqrt ( X * Y * Z ), 
c      where X, Y, and Z are positive.
c
c  Modified:
c
c    27 May 2018
c
c  Author:
c
c    Bille Carlson, Elaine Notis
c
c  Reference:
c
c    Bille Carlson,
c    Computing Elliptic Integrals by Duplication,
c    Numerische Mathematik,
c    Volume 33, 1979, pages 1-16.
c
c    Bille Carlson, Elaine Notis,
c    Algorithm 577, Algorithms for Incomplete Elliptic Integrals,
c    ACM Transactions on Mathematical Software,
c    Volume 7, Number 3, pages 398-403, September 1981.
c
c  Parameters:
c
c    Input, double precision X, Y, Z, the arguments in the integral.
c
c    Input, double precision ERRTOL, the error tolerance.
c    The relative error due to truncation is less than
c      3 * ERRTOL ^ 6 / (1-ERRTOL) ^ 3/2.
c    Sample choices:
c      ERRTOL   Relative truncation error less than
c      1.D-3    4.D-18
c      3.D-3    3.D-15
c      1.D-2    4.D-12
c      3.D-2    3.D-9
c      1.D-1    4.D-6
c
c    Output, integer IERR, the error flag.
c    0, no error occurred.
c    1, abnormal termination.  

#endif


        pure function rd_zmm16r4(x,y,z,errt) result(rd)
             
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: rd_zmm16r4
              !dir$ attributes forceinline :: rd_zmm16r4
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rd_zmm16r4
              use mod_vecconsts, only : v16_1,v16_0
              type(ZMM16r4_t),   intent(in) :: x
              type(ZMM16r4_t),   intent(in) :: y
              type(ZMM16r4_t),   intent(in) :: z
              type(ZMM16r4_t),   intent(in) :: errt
              type(ZMM16r4_t)  :: rc 
              type(ZMM16r4_t),    parameter :: C0214285714285714285714285714286 = 
                                                    ZMM16r4_t(-0.214285714285714285714285714286_sp)
              type(ZMM16r4_t),    parameter :: C0166666666666666666666666666667 = 
                                                    ZMM16r4_t(0.166666666666666666666666666667_sp)
              type(ZMM16r4_t),    parameter :: C0409090909090909090909090909091 = 
                                                    ZMM16r4_t(-0.409090909090909090909090909091_sp)
              type(ZMM16r4_t),    parameter :: C0115384615384615384615384615385 =
                                                    ZMM16r4_t(0.115384615384615384615384615385_sp)
              type(ZMM16r4_t),    parameter :: C30 = ZMM16r4_t(3.0_sp)
              type(ZMM16r4_t),    parameter :: C60 = ZMM16r4_t(6.0_sp)
              type(ZMM16r4_t),    parameter :: C15 = ZMM16r4_t(1.5_sp)
              type(ZMM16r4_t),    parameter :: C02 = ZMM16r4_t(0.2_sp)
              type(ZMM16r4_t),    parameter :: C025= ZMM16r4_t(0.25_sp)
              type(ZMM16r4_t),    automatic :: xn,yn,zn,epslon
              type(ZMM16r4_t),    automatic :: sigma,pow4,mu,xndev
              type(ZMM16r4_t),    automatic :: yndev,zndev,ea,eb
              type(ZMM16r4_t),    automatic :: ec,ed,ef,s1
              type(ZMM16r4_t),    automatic :: s2,xnroot,ynroot,znroot
              type(ZMM16r4_t),    automatic :: lamda,x0,x1,x2
              type(ZMM16r4_t),    automatic :: x3,x4,x5,t0
              type(Mask16_t),     automatic :: msk
#if (GMS_EXPLICIT_VECTORIZE) == 1
              integer(kind=i4) :: j
#endif               
              xn.v    = x.v
              yn.v    = y.v
              zn.v    = z.v
              sigma.v = v16_0.v
              pow4.v  = v16_1.v
#if (GMS_EXPLICIT_VECTORIZE) == 1
              do while (.true.)
                      !dir$ loop_count(16)
                      !dir$ vector aligned
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
                  do j=0,15   
                      t0.v(j) = zn.v(j)*C30.v(j)+xn.v(j)+yn.v(j)
                      mu.v(j) = C02.v(j)*t0.v(j)
                      xndev.v(j) = (mu.v(j)-xn.v(j))/mu.v(j)
                      epslon.v(j)= abs(xndev.v(j))
                      yndev.v(j) = (mu.v(j)-yn.v(j))/mu.v(j)
                      epslon.v(j)= max(epslon.v(j),abs(yndev.v(j))
                      epslon.v(j)= max(epslon.v(j),abs(zndev.v(j))
                      msk.m(j)   = (epslon.v(j)<errt.v(j))
                  if(msk.m(j)) then
                      ea.v(j)  = xndev.v(j)*yndev.v(j)
                      eb.v(j)  = zndev.v(j)*zndev.v(j)
                      ec.v(j)  = ea.v(j)-eb.v(j)
                      ed.v(j)  = ea.v(j)-(C60.v(j)*eb.v(j))
                      ef.v(j)  = ed.v(j)+(ec.v(j)+ec.v(j))
                      x0.v(j)  = C0409090909090909090909090909091.v(j)* &
                                 C025.v(j)+C0214285714285714285714285714286.v(j)
                      x1.v(j)  = ed.v(j)- &
                                 (C15.v(j)-C0115384615384615384615384615385.v(j))
                      x2.v(j)  = zndev.v(j)*ef.v(j)
                      s1.v(j)  = ed.v(j)*x0.v(j)*x1.v(j)*x2.v(j)
                      x3.v(j)  = C0409090909090909090909090909091.v(j)* &
                                 ec.v(j)+(zndev.v(j)* &
                                 C0115384615384615384615384615385.v(j)*ea.v(j))
                      x4.v(j)  = x3.v(j)*zndev.v(j)+ef.v(j)* &
                                 C0166666666666666666666666666667.v(j)
                      s2.v(j)  = zndev.v(j)*x4.v(j)
                      x0.v(j)  = C30.v(j)*sigma.v(j)+pow4.v(j)
                      x1.v(j)  = v16_1.v(j)+s1.v(j)+s2.v(j)
                      x2.v(j)  = mu.v(j)*sqrt(mu.v(j))
                      rd.v(j)  = (x0.v(j)*x1.v(j))/x2.v(j)
                      return
                  end if
                  xnroot.v(j) = sqrt(xn.v(j))
                  ynroot.v(j) = sqrt(yn.v(j))
                  znroot.v(j) = sqrt(zn.v(j))
                  x0.v(j)     = ynroot.v(j)*znroot.v(j)+ &
                                ynroot.v(j)*znroot.v(j)
                  lamda.v(j)  = xnroot.v(j)*x0.v(j)
                  sigma.v  = (sigma.v(j)+pow4.v(j))/ &
                             (znroot.v(j)*(zn.v(j)+lamda.v(j)))
                  pow4.v   = pow4.v(j)*C025.v(j)
                  xn.v     = (xn.v(j)+lamda.v(j))*C025.v(j)
                  yn.v     = (yn.v(j)+lamda.v(j))*C025.v(j)
                  zn.v     = (zn.v(j)+lamda.v(j))*C025.v(j)
                  end do
              end do
#else              
              do while (.true.)
                  t0.v    = zn.v*C30.v+xn.v+yn.v
                  mu.v    = C02.v*t0.v
                  xndev.v = (mu.v-xn.v)/mu.v
                  epslon.v= abs(xndev.v)
                  yndev.v = (mu.v-yn.v)/mu.v
                  epslon.v= max(epslon.v,abs(yndev.v)
                  epslon.v= max(epslon.v,abs(zndev.v)
                  msk.m   = (epslon.v<errt.v)
                  if(all(msk.m)) then
                      ea.v  = xndev.v*yndev.v
                      eb.v  = zndev.v*zndev.v
                      ec.v  = ea.v-eb.v
                      ed.v  = ea.v-(C60.v*eb.v)
                      ef.v  = ed.v+(ec.v+ec.v)
                      x0.v  = C0409090909090909090909090909091.v* &
                              C025.v+C0214285714285714285714285714286.v
                      x1.v  = ed.v- &
                              (C15.v-C0115384615384615384615384615385.v)
                      x2.v  = zndev.v*ef.v
                      s1.v  = ed.v*x0.v*x1.v*x2.v
                      x3.v  = C0409090909090909090909090909091.v* &
                              ec.v+(zndev.v* &
                              C0115384615384615384615384615385.v*ea.v)
                      x4.v  = x3.v*zndev.v+ef.v* &
                              C0166666666666666666666666666667.v
                      s2.v  = zndev.v*x4.v
                      x0.v  = C30.v*sigma.v+pow4.v
                      x1.v  = v16_1.v+s1.v+s2.v
                      x2.v  = mu.v*sqrt(mu.v)
                      rd.v  = (x0.v*x1.v)/x2.v
                      return
                  end if
                  xnroot.v = sqrt(xn.v)
                  ynroot.v = sqrt(yn.v)
                  znroot.v = sqrt(zn.v)
                  x0.v     = ynroot.v*znroot.v+ &
                             ynroot.v*znroot.v
                  lamda.v  = xnroot.v*x0.v
                  sigma.v  = (sigma.v+pow4.v)/ &
                             (znroot.v*(zn.v+lamda.v))
                  pow4.v   = pow4.v*C025.v
                  xn.v     = (xn.v+lamda.v)*C025.v
                  yn.v     = (yn.v+lamda.v)*C025.v
                  zn.v     = (zn.v+lamda.v)*C025.v
              end do
#endif
        end function rd_zmm16r4
        
        
#if 0

/*							fresnl.c
 *
 *	Fresnel integral
 *
 *
 *
 * SYNOPSIS:
 *
 * double x, S, C;
 * void fresnl();
 *
 * fresnl( x, _&S, _&C );
 *
 *
 * DESCRIPTION:
 *
 * Evaluates the Fresnel integrals
 *
 *           x
 *           -
 *          | |
 * C(x) =   |   cos(pi/2 t**2) dt,
 *        | |
 *         -
 *          0
 *
 *           x
 *           -
 *          | |
 * S(x) =   |   sin(pi/2 t**2) dt.
 *        | |
 *         -
 *          0
 *
 *
 * The integrals are evaluated by a power series for x < 1.
 * For x >= 1 auxiliary functions f(x) and g(x) are employed
 * such that
 *
 * C(x) = 0.5 + f(x) sin( pi/2 x**2 ) - g(x) cos( pi/2 x**2 )
 * S(x) = 0.5 - f(x) cos( pi/2 x**2 ) - g(x) sin( pi/2 x**2 )
 *
 *
 *
 * ACCURACY:
 *
 *  Relative error.
 *
 * Arithmetic  function   domain     # trials      peak         rms
 *   IEEE       S(x)      0, 10       10000       2.0e-15     3.2e-16
 *   IEEE       C(x)      0, 10       10000       1.8e-15     3.3e-16
 *   DEC        S(x)      0, 10        6000       2.2e-16     3.9e-17
 *   DEC        C(x)      0, 10        5000       2.3e-16     3.9e-17
 */

#endif     


       subroutine fresnel_zmm16r4(xxa,ssa,cca)
            
              !dir$ optimize:3
              !dir$ attributes code_align : 32 :: fresnel_zmm16r4
              !dir$ attributes forceinline :: fresnel_zmm16r4
              !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: fresnel_zmm16r4
              use mod_vecconsts, only : v16_1,v16_0
              type(ZMM16r4_t),  intent(in) :: xxa
              type(ZMM16r4_t),  intent(out):: ssa
              type(ZMM16r4_t),  intent(out):: cca
              !dir$ attributes align : 64 :: C25625
              !dir$ attributes align : 64 :: C369740
              !dir$ attributes align : 64 :: C05
              !dir$ attributes align : 64 :: C314159265358979323846264338328
              !dir$ attributes align : 64 :: C157079632679489661923132169164
              !dir$ attributes align : 64 :: f
              !dir$ attributes align : 64 :: g
              !dir$ attributes align : 64 :: cc
              !dir$ attributes align : 64 :: ss
              !dir$ attributes align : 64 :: t
              !dir$ attributes align : 64 :: u
              !dir$ attributes align : 64 :: t0
              !dir$ attributes align : 64 :: t1
              !dir$ attributes align : 64 :: x
              !dir$ attributes align : 64 :: x2
              !dir$ attributes align : 64 :: acc1
              !dir$ attributes align : 64 :: acc2
              !dir$ attributes align : 64 :: acc3
              !dir$ attributes align : 64 :: acc4
              type(ZMM16r4_t),  parameter :: C25625 = ZMM16r4_t(2.5625_sp)
              type(ZMM16r4_t),  parameter :: C369740= ZMM16r4_t(36974.0_sp)
              type(ZMM16r4_t),  parameter :: C05    = ZMM16r4_t(0.5_sp)
              type(ZMM16r4_t),  parameter :: C314159265358979323846264338328 = &
                                                    ZMM16r4_t(3.14159265358979323846264338328_sp)
              type(ZMM16r4_t),  parameter :: C157079632679489661923132169164 = &
                                                    ZMM16r4_t(1.57079632679489661923132169164_sp)
              type(ZMM16r4_t),  automatic :: f,g,cc,ss
              type(ZMM16r4_t),  automatic :: t,u,t0,t1
              type(ZMM16r4_t),  automatic :: x,x2,acc1,acc2
              type(ZMM16r4_t),  automatic :: acc3,acc4
              type(Mask16_t),   automatic :: msk
#if (GMS_EXPLICIT_VECTORIZE) == 1
              integer(kind=i4) :: j
#endif   
#if (GMS_EXPLICIT_VECTORIZE) == 1
           
                      !dir$ loop_count(16)
                      !dir$ vector aligned
                      !dir$ vector vectorlength(4)
                      !dir$ vector always
              do j=0,15  
                   x.v(j)   = abs(xxa.v(j))
                   x2.v(j)  = x.v(j)*x.v(j)
                   msk.m(j) = (x.v(j)<C25625.v(j))
                   if(msk.m(j)) then
                      t.v(j)    = x2.v(j)*x2.v(j)
                      acc1.v(j) = sn0.v(j)
                      acc2.v(j) = t.v(j)+sd0.v(j)
                      acc3.v(j) = cn0.v(j)
                      acc4.v(j) = cd0.v(j)
                      acc1.v(j) = acc1.v(j)*t.v(j)+sn1.v(j)
                      acc2.v(j) = acc2.v(j)*t.v(j)+sd1.v(j)
                      acc1.v(j) = acc1.v(j)*t.v(j)+sn2.v(j)
                      acc2.v(j) = acc2.v(j)*t.v(j)+sd2.v(j)
                      acc1.v(j) = acc1.v(j)*t.v(j)+sn3.v(j)
                      acc2.v(j) = acc2.v(j)*t.v(j)+sd3.v(j)
                      acc1.v(j) = acc1.v(j)*t.v(j)+sn4.v(j)
                      acc2.v(j) = acc2.v(j)*t.v(j)+sd4.v(j)
                      acc2.v(j) = acc2.v(j)*t.v(j)+sd5.v(j)
                      t0.v(j)   = acc1.v(j)/acc2.v(j)
                      ss.v(j)   = x.v(j)*x2.v(j)*t0.v(j)
                      acc3.v(j) = acc3.v(j)*t.v(j)+cn1.v(j)
                 acc4.v = acc4.v*t.v+cd1.v
                 acc3.v = acc3.v*t.v+cn2.v
                 acc4.v = acc4.v*t.v+cd2.v
                 acc3.v = acc3.v*t.v+cn3.v
                 acc4.v = acc4.v*t.v+cd3.v
                 acc3.v = acc3.v*t.v+cn4.v
                 acc4.v = acc4.v*t.v+cd4.v
                 acc4.v = acc4.v*t.v+cd5.v
                 t1.v   = acc3.v/acc4.v
                 cc.v   = x.v*t1.v
                 goto 999
              end if
              msk.m = (x.v>C369740.v)
              if(all(msk.m)) then
                 cc.v = C05.v
                 ss.v = cc.v
                 goto 999
              end if
              !!
              !!Asymptotic power series auxiliary functions
              !!         *		for large argument
              !! 
              t.v    = C314159265358979323846264338328.v* &
                       x2.v
              u.v    = v16_1.v*t.v*t.v
              acc1.v = fn0.v
              acc2.v = u.v*fd0.v
              acc3.v = gn0.v
              acc4.v = u.v+gd0.v
              t.v    = v16_1.v/t.v
              acc1.v = acc1.v*u.v+fn1.v
              acc2.v = acc2.v*u.v+fd1.v
              acc1.v = acc1.v*u.v+fn2.v
              acc2.v = acc2.v*u.v+fd2.v
              acc1.v = acc1.v*u.v+fn3.v
              acc2.v = acc2.v*u.v+fd3.v
              acc1.v = acc1.v*u.v+fn4.v
              acc2.v = acc2.v*u.v+fd4.v
              acc1.v = acc1.v*u.v+fn5.v
              acc2.v = acc2.v*u.v+fd5.v
              acc1.v = acc1.v*u.v+fn6.v
              acc2.v = acc2.v*u.v+fd6.v
              acc1.v = acc1.v*u.v+fn7.v
              acc2.v = acc2.v*u.v+fd7.v
              acc1.v = acc1.v*u.v+fn8.v
              acc2.v = acc2.v*u.v+fd8.v
              acc2.v = acc2.v*u.v+fd9.v
              t0.v   = acc1.v/acc2.v
              f.v    = v16_1.v-(u.v*t0.v)
              acc3.v = acc3.v*u.v+gn1.v
              acc4.v = acc4.v*u.v+gd1.v
              acc3.v = acc3.v*u.v+gn2.v
              acc4.v = acc4.v*u.v+gd2.v 
              acc3.v = acc3.v*u.v+gn3.v
              acc4.v = acc4.v*u.v+gd3.v
              acc3.v = acc3.v*u.v+gn4.v
              acc4.v = acc4.v*u.v+gd4.v
              acc3.v = acc3.v*u.v+gn5.v
              acc4.v = acc4.v*u.v+gd5.v
              acc3.v = acc3.v*u.v+gn6.v
              acc4.v = acc4.v*u.v+gd6.v
              acc3.v = acc3.v*u.v+gn7.v
              acc4.v = acc4.v*u.v+gd7.v
              acc3.v = acc3.v*u.v+gn8.v
              acc4.v = acc4.v*u.v+gd8.v
              acc3.v = acc3.v*u.v+gn9.v
              acc4.v = acc4.v*u.v+gd10.v 
              t1.v   = acc3.v/acc4.v
              g.v    = t.v*t1.v
              t.v    = C157079632679489661923132169164.v* &
                       x2.v
              c.v    = cos(t.v)
              s.v    = sin(t.v)
              t.v    = C314159265358979323846264338328.v* &
                       x.v
              t0.v(j)   = f.v(j)*s.v(j)-(g.v(j)*c.v(j))
              cc.v(j)   = C05.v(j)+(t0.v(j)/t.v(j))
              t1.v(j)   = f.v(j)*c.v(j)+(g.v(j)*s.v(j))
              ss.v(j)   = C05.v(j)-(t1.v(j)/t.v(j))
999           msk.m(j)  = (xxa.v(j)<v16_0.v(j))
              if(msk.m(j)) then
                  cc.v(j) = -cc.v(j)
                  ss.v(j) = -ss.v(j)
              end if
              cca.v(j) = cc.v(j)
              ssa.v(j) = ss.v(j)
              end do
#else             
              x.v   = abs(xxa.v)
              x2.v  = x.v*x.v
              msk.m = (x.v<C25625.v)
              if(all(msk.m)) then
                 t.v    = x2.v*x2.v
                 acc1.v = sn0.v
                 acc2.v = t.v+sd0.v
                 acc3.v = cn0.v
                 acc4.v = cd0.v
                 acc1.v = acc1.v*t.v+sn1.v
                 acc2.v = acc2.v*t.v+sd1.v
                 acc1.v = acc1.v*t.v+sn2.v
                 acc2.v = acc2.v*t.v+sd2.v
                 acc1.v = acc1.v*t.v+sn3.v
                 acc2.v = acc2.v*t.v+sd3.v
                 acc1.v = acc1.v*t.v+sn4.v
                 acc2.v = acc2.v*t.v+sd4.v
                 acc2.v = acc2.v*t.v+sd5.v
                 t0.v   = acc1.v/acc2.v
                 ss.v   = x.v*x2.v*t0.v
                 acc3.v = acc3.v*t.v+cn1.v
                 acc4.v = acc4.v*t.v+cd1.v
                 acc3.v = acc3.v*t.v+cn2.v
                 acc4.v = acc4.v*t.v+cd2.v
                 acc3.v = acc3.v*t.v+cn3.v
                 acc4.v = acc4.v*t.v+cd3.v
                 acc3.v = acc3.v*t.v+cn4.v
                 acc4.v = acc4.v*t.v+cd4.v
                 acc4.v = acc4.v*t.v+cd5.v
                 t1.v   = acc3.v/acc4.v
                 cc.v   = x.v*t1.v
                 goto 999
              end if
              msk.m = (x.v>C369740.v)
              if(all(msk.m)) then
                 cc.v = C05.v
                 ss.v = cc.v
                 goto 999
              end if
              !!
              !!Asymptotic power series auxiliary functions
              !!         *		for large argument
              !! 
              t.v    = C314159265358979323846264338328.v* &
                       x2.v
              u.v    = v16_1.v*t.v*t.v
              acc1.v = fn0.v
              acc2.v = u.v*fd0.v
              acc3.v = gn0.v
              acc4.v = u.v+gd0.v
              t.v    = v16_1.v/t.v
              acc1.v = acc1.v*u.v+fn1.v
              acc2.v = acc2.v*u.v+fd1.v
              acc1.v = acc1.v*u.v+fn2.v
              acc2.v = acc2.v*u.v+fd2.v
              acc1.v = acc1.v*u.v+fn3.v
              acc2.v = acc2.v*u.v+fd3.v
              acc1.v = acc1.v*u.v+fn4.v
              acc2.v = acc2.v*u.v+fd4.v
              acc1.v = acc1.v*u.v+fn5.v
              acc2.v = acc2.v*u.v+fd5.v
              acc1.v = acc1.v*u.v+fn6.v
              acc2.v = acc2.v*u.v+fd6.v
              acc1.v = acc1.v*u.v+fn7.v
              acc2.v = acc2.v*u.v+fd7.v
              acc1.v = acc1.v*u.v+fn8.v
              acc2.v = acc2.v*u.v+fd8.v
              acc2.v = acc2.v*u.v+fd9.v
              t0.v   = acc1.v/acc2.v
              f.v    = v16_1.v-(u.v*t0.v)
              acc3.v = acc3.v*u.v+gn1.v
              acc4.v = acc4.v*u.v+gd1.v
              acc3.v = acc3.v*u.v+gn2.v
              acc4.v = acc4.v*u.v+gd2.v 
              acc3.v = acc3.v*u.v+gn3.v
              acc4.v = acc4.v*u.v+gd3.v
              acc3.v = acc3.v*u.v+gn4.v
              acc4.v = acc4.v*u.v+gd4.v
              acc3.v = acc3.v*u.v+gn5.v
              acc4.v = acc4.v*u.v+gd5.v
              acc3.v = acc3.v*u.v+gn6.v
              acc4.v = acc4.v*u.v+gd6.v
              acc3.v = acc3.v*u.v+gn7.v
              acc4.v = acc4.v*u.v+gd7.v
              acc3.v = acc3.v*u.v+gn8.v
              acc4.v = acc4.v*u.v+gd8.v
              acc3.v = acc3.v*u.v+gn9.v
              acc4.v = acc4.v*u.v+gd10.v 
              t1.v   = acc3.v/acc4.v
              g.v    = t.v*t1.v
              t.v    = C157079632679489661923132169164.v* &
                       x2.v
              c.v    = cos(t.v)
              s.v    = sin(t.v)
              t.v    = C314159265358979323846264338328.v* &
                       x.v
              t0.v   = f.v*s.v-(g.v*c.v)
              cc.v   = C05.v+(t0.v/t.v)
              t1.v   = f.v*c.v+(g.v*s.v)
              ss.v   = C05.v-(t1.v/t.v)
999           msk.m  = (xxa.v<v16_0.v)
              if(all(msk.m)) then
                  cc.v = -cc.v
                  ss.v = -ss.v
              end if
              cca.v = cc.v
              ssa.v = ss.v
#endif
       end subroutine fresnel_zmm16r4


             
























end module rcs_common_zmm16r4
