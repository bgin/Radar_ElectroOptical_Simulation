

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
























end module rcs_common_zmm16r4
