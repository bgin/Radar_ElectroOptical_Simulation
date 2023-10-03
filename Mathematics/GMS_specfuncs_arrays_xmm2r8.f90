

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

module  specfuncs_arrays_xmm2r8


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         specfuncs_arrays_xmm2r8
 !          
 !          Purpose:
 !                       Various vectorized special functions
 !                       packed into arrays of data
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
 !                       Bernard Gingold 
 !                 
 !          References:
 !         
 !                      Provided by the file: "GMS_spec_funcs_xmm2r8.f90"
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
!==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.
    use mod_kinds,    only : i4,i8,dp
    use mod_vectypes, only : XMM2r8_t,Mask2_t
    use spec_funcs_xmm2r8
    
    
    contains
    
#if 0
        !*****************************************************************************80
!
!! CALCEI computes various exponential integrals.
!
!  Discussion:
!
!    This routine computes the exponential integrals Ei(x),
!    E1(x), and  exp(-x)*Ei(x) for real arguments x where
!
!           integral (from t=-oo to t=x) (exp(t)/t),  x > 0,
!    Ei(x) =
!          -integral (from t=-x to t=+oo) (exp(t)/t),  x < 0,
!
!    and where the first integral is a principal value integral.
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
!  Reference:
!
!    William Cody, Henry Thacher,
!    Rational Chebyshev Approximations for the Exponential
!    Integral E1(x),
!    Mathematics of Computation,
!    Volume 22, Number 103, July 1968, pages 641-649.
!
!    William Cody, Henry Thacher,
!    Chebyshev Approximations for the Exponential
!    Integral Ei(x),
!    Mathematics of Computation,
!    Volume 23, Number 106, April 1969, pages 289-303.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) ARG, the argument.  The argument must not
!    be zero.  If JINT = 2, then the argument must be strictly positive.
!
!    Output, real ( kind = 8 ) RESULT, the value of the function,
!    which depends on the input value of JINT:
!    1, RESULT = EI ( ARG );
!    2, RESULT = EONE ( ARG );
!    3, RESULT = EXPEI ( ARG ).
!
!    Input, integer ( kind = 4 ) JINT, chooses the function to be computed.
!    1, Ei(x);
!    2, -Ei(-x);
!    3, exp(-x)*Ei(x).
!
#endif   
    
       subroutine calcei_unroll12x_nonblock_xmm2r8(parg,pval,n,jint,PF_DIST)
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calcei_unroll12x_nonblock_xmm2r8
             !dir$ attributes forceinline :: calcei_unroll12x_nonblock_xmm2r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calcei_unroll12x_nonblock_xmm2r8
             type(XMM2r8_t), dimension(1:n), intent(in) :: parg
             type(XMM2r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 16 :: arg0
             !dir$ attributes align : 16 :: arg1
             !dir$ attributes align : 16 :: arg2
             !dir$ attributes align : 16 :: arg3
             !dir$ attributes align : 16 :: val0
             !dir$ attributes align : 16 :: val1
             !dir$ attributes align : 16 :: val2
             !dir$ attributes align : 16 :: val3
             type(XMM2r8_t), automatic :: arg0
             type(XMM2r8_t), automatic :: arg1
             type(XMM2r8_t), automatic :: arg2
             type(XMM2r8_t), automatic :: arg3
             type(XMM2r8_t), automatic :: val0
             type(XMM2r8_t), automatic :: val1
             type(XMM2r8_t), automatic :: val2
             type(XMM2r8_t), automatic :: val3
             type(XMM2r8_t), automatic :: val3
             integer(kind=i4) :: i,m,m1
             m = mod(n,12)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call calcei_xmm2r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<12) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             !dir$ assume_aligned parg:16
             !dir$ assume_aligned pval:16
             !dir$ vector aligned
             !dir$ ivdep
             !dir$ vector vectorlength(8)
             !dir$ vector multiple_gather_scatter_by_shuffles 
             !dir$ vector always
             do i=m1,n,12
#if (__RCS_CYLINDER_PF_CACHE_HINT__) == 1
                       call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_PF_CACHE_HINT__) == 2   
                       call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                       call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                       call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif                 
                    arg0.v = parg(i+0).v
                    call calcei_xmm2r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call calcei_xmm2r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call calcei_xmm2r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call calcei_xmm2r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
                    arg0.v = parg(i+4).v
                    call calcei_xmm2r8(arg0,val0,jint)
                    pval(i+4).v = val0.v 
                    arg1.v = parg(i+5).v
                    call calcei_xmm2r8(arg1,val1,jint)
                    pval(i+5).v = val1.v  
                    arg2.v = parg(i+6).v
                    call calcei_xmm2r8(arg2,val2,jint)
                    pval(i+6).v = val2.v
                    arg3.v = parg(i+7).v
                    call calcei_xmm2r8(arg3,val3,jint)
                    pval(i+7).v = val3.v
                    arg0.v = parg(i+8).v
                    call calcei_xmm2r8(arg0,val0,jint)
                    pval(i+8).v = val0.v 
                    arg1.v = parg(i+9).v
                    call calcei_xmm2r8(arg1,val1,jint)
                    pval(i+9).v = val1.v  
                    arg2.v = parg(i+10).v
                    call calcei_xmm2r8(arg2,val2,jint)
                    pval(i+10).v = val2.v
                    arg3.v = parg(i+11).v
                    call calcei_xmm2r8(arg3,val3,jint)
                    pval(i+11).v = val3.v
             end do
       end subroutine calcei_unroll12x_nonblock_xmm2r8
    


end module specfuncs_arrays_xmm2r8
