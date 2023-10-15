

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

module  specfuncs_arrays_zmm8r8


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         specfuncs_arrays_zmm8r8
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
 !                      Provided by the file: "GMS_spec_funcs_zmm8r8.f90"
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
!==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.
    use mod_kinds,    only : i4,i8,dp
    use mod_vectypes, only : ZMM8r8_t,Mask8_t
    use spec_funcs_zmm8r8
 
#if !defined(__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__)
#define __SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__ 1
#endif   
    
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
    
       subroutine calcei_unroll12x_nonblock_zmm8r8(parg,pval,n,jint,PF_DIST)
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calcei_unroll12x_nonblock_zmm8r8
             !dir$ attributes forceinline :: calcei_unroll12x_nonblock_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calcei_unroll12x_nonblock_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             type(ZMM8r8_t), automatic :: val3
             integer(kind=i4) :: i,m,m1
             m = mod(n,12)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call calcei_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<12) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
             !dir$ vector aligned
             !dir$ ivdep
             !dir$ vector vectorlength(8)
             !dir$ vector multiple_gather_scatter_by_shuffles 
             !dir$ vector always
             do i=m1,n,12
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__ ) == 1
                       call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__ ) == 2   
                       call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__ ) == 3
                       call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__ ) == 4
                       call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif                 
                    arg0.v = parg(i+0).v
                    call calcei_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call calcei_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call calcei_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call calcei_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
                    arg0.v = parg(i+4).v
                    call calcei_zmm8r8(arg0,val0,jint)
                    pval(i+4).v = val0.v 
                    arg1.v = parg(i+5).v
                    call calcei_zmm8r8(arg1,val1,jint)
                    pval(i+5).v = val1.v  
                    arg2.v = parg(i+6).v
                    call calcei_zmm8r8(arg2,val2,jint)
                    pval(i+6).v = val2.v
                    arg3.v = parg(i+7).v
                    call calcei_zmm8r8(arg3,val3,jint)
                    pval(i+7).v = val3.v
                    arg0.v = parg(i+8).v
                    call calcei_zmm8r8(arg0,val0,jint)
                    pval(i+8).v = val0.v 
                    arg1.v = parg(i+9).v
                    call calcei_zmm8r8(arg1,val1,jint)
                    pval(i+9).v = val1.v  
                    arg2.v = parg(i+10).v
                    call calcei_zmm8r8(arg2,val2,jint)
                    pval(i+10).v = val2.v
                    arg3.v = parg(i+11).v
                    call calcei_zmm8r8(arg3,val3,jint)
                    pval(i+11).v = val3.v
             end do
       end subroutine calcei_unroll12x_nonblock_zmm8r8
    
    
       subroutine calcei_unroll8x_nonblock_zmm8r8(parg,pval,n,jint,PF_DIST)
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calcei_unroll8x_nonblock_zmm8r8
             !dir$ attributes forceinline :: calcei_unroll8x_nonblock_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calcei_unroll8x_nonblock_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             type(ZMM8r8_t), automatic :: val3
             integer(kind=i4) :: i,m,m1
             m = mod(n,8)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call calcei_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<8) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
             !dir$ vector aligned
             !dir$ ivdep
             !dir$ vector vectorlength(8)
             !dir$ vector multiple_gather_scatter_by_shuffles 
             !dir$ vector always
             do i=m1,n,8
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__ ) == 1
                       call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__ ) == 2   
                       call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__ ) == 3
                       call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__ ) == 4
                       call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif                 
                    arg0.v = parg(i+0).v
                    call calcei_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call calcei_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call calcei_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call calcei_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
                    arg0.v = parg(i+4).v
                    call calcei_zmm8r8(arg0,val0,jint)
                    pval(i+4).v = val0.v 
                    arg1.v = parg(i+5).v
                    call calcei_zmm8r8(arg1,val1,jint)
                    pval(i+5).v = val1.v  
                    arg2.v = parg(i+6).v
                    call calcei_zmm8r8(arg2,val2,jint)
                    pval(i+6).v = val2.v
                    arg3.v = parg(i+7).v
                    call calcei_zmm8r8(arg3,val3,jint)
                    pval(i+7).v = val3.v
              end do
       end subroutine calcei_unroll8x_nonblock_zmm8r8
       
       
       subroutine calcei_unroll4x_nonblock_zmm8r8(parg,pval,n,jint,PF_DIST)
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calcei_unroll4x_nonblock_zmm8r8
             !dir$ attributes forceinline :: calcei_unroll4x_nonblock_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calcei_unroll4x_nonblock_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             type(ZMM8r8_t), automatic :: val3
             integer(kind=i4) :: i,m,m1
             m = mod(n,4)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call calcei_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<4) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
             !dir$ vector aligned
             !dir$ ivdep
             !dir$ vector vectorlength(8)
             !dir$ vector multiple_gather_scatter_by_shuffles 
             !dir$ vector always
             do i=m1,n,4
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__ ) == 1
                       call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__ ) == 2   
                       call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__ ) == 3
                       call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__ ) == 4
                       call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif                 
                    arg0.v = parg(i+0).v
                    call calcei_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call calcei_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call calcei_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call calcei_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
                end do
       end subroutine calcei_unroll4x_nonblock_zmm8r8
       
       
       subroutine calcei_blocked64x32x16x8x_zmm8r8(parg,pval,n,jint,PF_DIST,which)         
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calcei_blocked64x32x16x8x_zmm8r8
             !dir$ attributes forceinline :: calcei_blocked64x32x16x8x_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calcei_blocked64x32x16x8x_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             logical(kind=i4),               intent(in) :: which
             integer(kind=i4), parameter :: BLOCK64X = 64
             integer(kind=i4), parameter :: BLOCK32X = 32
             integer(kind=i4), parameter :: BLOCK16X = 16
             integer(kind=i4), parameter :: BLOCK8X  = 8 
             !dir$ attributes align : 64 :: arg0
             type(ZMM8r8_t), automatic :: arg0
             !dir$ attributes align : 64 :: arg0
             type(ZMM8r8_t), automatic :: val0
             integer(kind=i4) :: i,ii
             
            
             select case(which)
             case (0)
                  do i=1,n,BLOCK64X
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__ ) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__ ) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__ ) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__ ) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK64X-1,n)
                          arg0.v = parg(ii).v
                          call calcei_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do
             case (1)
                  do i=1,n,BLOCK32X
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK32X-1,n)
                          arg0.v = parg(ii).v
                          call calcei_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do 
             case (2)
                  do i=1,n,BLOCK16X
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK16X-1,n)
                          arg0.v = parg(ii).v
                          call calcei_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do 
             case (3)
                  do i=1,n,BLOCK8X
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK8X-1,n)
                          arg0.v = parg(ii).v
                          call calcei_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do
             case default
                  return
             end select
             
       end subroutine calcei_blocked64x32x16x8x_zmm8r8
       
       
       subroutine calcei_blocked64x32x16x8x_omp_zmm8r8(parg,pval,n,jint,PF_DIST,which)         
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calcei_blocked64x32x16x8x_omp_zmm8r8
             !dir$ attributes forceinline :: calcei_blocked64x32x16x8x_omp_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calcei_blocked64x32x16x8x_omp_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             logical(kind=i4),               intent(in) :: which
             integer(kind=i4), parameter :: BLOCK64X = 64
             integer(kind=i4), parameter :: BLOCK32X = 32
             integer(kind=i4), parameter :: BLOCK16X = 16
             integer(kind=i4), parameter :: BLOCK8X  = 8 
             !dir$ attributes align : 64 :: arg0
             type(ZMM8r8_t), automatic :: arg0
             !dir$ attributes align : 64 :: arg0
             type(ZMM8r8_t), automatic :: val0
             integer(kind=i4) :: i,ii
             
            
             select case(which)
             case (0)
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
!$omp parallel do schedule(static,BLOCK64X) default(none)                &
!$omp firstprivate(BLOCK64X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T0,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2 
!$omp parallel do schedule(static,BLOCK64X) default(none)                &
!$omp firstprivate(BLOCK64X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T1,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
!$omp parallel do schedule(static,BLOCK64X) default(none)                &
!$omp firstprivate(BLOCK64X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T2,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
!$omp parallel do schedule(static,BLOCK64X) default(none)                &
!$omp firstprivate(BLOCK64X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_NTA,parg,pval)
#endif
                  do i=1,n,BLOCK64X

#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif   
         
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK64X-1,n)
                          arg0.v = parg(ii).v
                          call calcei_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do
             case (1)
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
!$omp parallel do schedule(static,BLOCK32X) default(none)                &
!$omp firstprivate(BLOCK32X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T0,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2 
!$omp parallel do schedule(static,BLOCK32X) default(none)                &
!$omp firstprivate(BLOCK32X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T1,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
!$omp parallel do schedule(static,BLOCK32X) default(none)                &
!$omp firstprivate(BLOCK32X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T2,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
!$omp parallel do schedule(static,BLOCK32X) default(none)                &
!$omp firstprivate(BLOCK32X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_NTA,parg,pval)
#endif             
                  do i=1,n,BLOCK32X
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK32X-1,n)
                          arg0.v = parg(ii).v
                          call calcei_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do 
             case (2)
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
!$omp parallel do schedule(static,BLOCK16X) default(none)                &
!$omp firstprivate(BLOCK16X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T0,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2 
!$omp parallel do schedule(static,BLOCK16X) default(none)                &
!$omp firstprivate(BLOCK16X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T1,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
!$omp parallel do schedule(static,BLOCK16X) default(none)                &
!$omp firstprivate(BLOCK16X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T2,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
!$omp parallel do schedule(static,BLOCK16X) default(none)                &
!$omp firstprivate(BLOCK16X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_NTA,parg,pval)
#endif                
                  do i=1,n,BLOCK16X
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK16X-1,n)
                          arg0.v = parg(ii).v
                          call calcei_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do 
             case (3)
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
!$omp parallel do schedule(static,BLOCK8X) default(none)                &
!$omp firstprivate(BLOCK8X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T0,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2 
!$omp parallel do schedule(static,BLOCK8X) default(none)                &
!$omp firstprivate(BLOCK8X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T1,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
!$omp parallel do schedule(static,BLOCK8X) default(none)                &
!$omp firstprivate(BLOCK8X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T2,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
!$omp parallel do schedule(static,BLOCK8X) default(none)                &
!$omp firstprivate(BLOCK8X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_NTA,parg,pval)
#endif                  
                  do i=1,n,BLOCK8X
#if (__RCS_CYLINDER_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK8X-1,n)
                          arg0.v = parg(ii).v
                          call calcei_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do
             case default
                  return
             end select
             
       end subroutine calcei_blocked64x32x16x8x_omp_zmm8r8
       
       
       subroutine calcei_unroll12x_omp_zmm8r8(parg,pval,n,jint,PF_DIST)
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calcei_unroll12x_omp_zmm8r8
             !dir$ attributes forceinline :: calcei_unroll12x_omp_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calcei_unroll12x_omp_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: arg4
             !dir$ attributes align : 64 :: arg5
             !dir$ attributes align : 64 :: arg6
             !dir$ attributes align : 64 :: arg7
             !dir$ attributes align : 64 :: arg8
             !dir$ attributes align : 64 :: arg9
             !dir$ attributes align : 64 :: arg10
             !dir$ attributes align : 64 :: arg11
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             !dir$ attributes align : 64 :: val4
             !dir$ attributes align : 64 :: val5
             !dir$ attributes align : 64 :: val6
             !dir$ attributes align : 64 :: val7
             !dir$ attributes align : 64 :: val8
             !dir$ attributes align : 64 :: val9
             !dir$ attributes align : 64 :: val10
             !dir$ attributes align : 64 :: val11
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: arg4
             type(ZMM8r8_t), automatic :: arg5
             type(ZMM8r8_t), automatic :: arg6
             type(ZMM8r8_t), automatic :: arg7
             type(ZMM8r8_t), automatic :: arg8
             type(ZMM8r8_t), automatic :: arg9
             type(ZMM8r8_t), automatic :: arg10
             type(ZMM8r8_t), automatic :: arg11
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             type(ZMM8r8_t), automatic :: val4
             type(ZMM8r8_t), automatic :: val5
             type(ZMM8r8_t), automatic :: val6
             type(ZMM8r8_t), automatic :: val7
             type(ZMM8r8_t), automatic :: val8
             type(ZMM8r8_t), automatic :: val9
             type(ZMM8r8_t), automatic :: val10
             type(ZMM8r8_t), automatic :: val11
             integer(kind=i4) :: i,m,m1
             m = mod(n,12)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call calcei_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<12) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             val4.v = 0.0_dp
             val5.v = 0.0_dp
             val6.v = 0.0_dp
             val7.v = 0.0_dp
             val8.v = 0.0_dp
             val9.v = 0.0_dp
             val10.v= 0.0_dp
             val11.v= 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
             !dir$ vector aligned
             !dir$ ivdep
             !dir$ vector vectorlength(8)
             !dir$ vector multiple_gather_scatter_by_shuffles 
             !dir$ vector always
!$omp parallel do proc_bind(close) schedule(static) default(none) firstprivate(m1)       &
!$omp firstprivate(val0,val1,val2,val3,val4,val5,val6,val7)                              &
!$omp firstprivate(val8,val9,val10,val11) private(i,arg0,arg1,arg2,arg3,arg4,arg5,arg6)  &
!$omp private(arg7,arg8,arg9,arg10,arg11) shared(parg,pval,jint,n,PF_DIST)               &
!$omp private(FOR_K_PREFETCH_T0,FOR_K_PREFETCH_T1,FOR_K_PREFETCH_T2,FOR_K_PREFETCH_NTA)  
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
                    call calcei_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call calcei_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call calcei_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call calcei_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
                    arg4.v = parg(i+4).v
                    call calcei_zmm8r8(arg4,val4,jint)
                    pval(i+4).v = val4.v 
                    arg5.v = parg(i+5).v
                    call calcei_zmm8r8(arg5,val5,jint)
                    pval(i+5).v = val5.v  
                    arg6.v = parg(i+6).v
                    call calcei_zmm8r8(arg6,val6,jint)
                    pval(i+6).v = val6.v
                    arg7.v = parg(i+7).v
                    call calcei_zmm8r8(arg7,val7,jint)
                    pval(i+7).v = val7.v
                    arg8.v = parg(i+8).v
                    call calcei_zmm8r8(arg8,val8,jint)
                    pval(i+8).v = val8.v 
                    arg9.v = parg(i+9).v
                    call calcei_zmm8r8(arg9,val9,jint)
                    pval(i+9).v = val9.v  
                    arg10.v = parg(i+10).v
                    call calcei_zmm8r8(arg10,val10,jint)
                    pval(i+10).v = val10.v
                    arg11.v = parg(i+11).v
                    call calcei_zmm8r8(arg11,val11,jint)
                    pval(i+11).v = val11.v
             end do
!$omp end parallel do
       end subroutine calcei_unroll12x_omp_zmm8r8
       
       
       subroutine calcei_unroll8x_omp_zmm8r8(parg,pval,n,jint,PF_DIST)
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calcei_unroll8x_omp_zmm8r8
             !dir$ attributes forceinline :: calcei_unroll8x_omp_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calcei_unroll8x_omp_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: arg4
             !dir$ attributes align : 64 :: arg5
             !dir$ attributes align : 64 :: arg6
             !dir$ attributes align : 64 :: arg7
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             !dir$ attributes align : 64 :: val4
             !dir$ attributes align : 64 :: val5
             !dir$ attributes align : 64 :: val6
             !dir$ attributes align : 64 :: val7
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: arg4
             type(ZMM8r8_t), automatic :: arg5
             type(ZMM8r8_t), automatic :: arg6
             type(ZMM8r8_t), automatic :: arg7
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             type(ZMM8r8_t), automatic :: val4
             type(ZMM8r8_t), automatic :: val5
             type(ZMM8r8_t), automatic :: val6
             type(ZMM8r8_t), automatic :: val7
             integer(kind=i4) :: i,m,m1
             m = mod(n,8)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call calcei_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<8) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             val4.v = 0.0_dp
             val5.v = 0.0_dp
             val6.v = 0.0_dp
             val7.v = 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
             !dir$ vector aligned
             !dir$ ivdep
             !dir$ vector vectorlength(8)
             !dir$ vector multiple_gather_scatter_by_shuffles 
             !dir$ vector always
!$omp parallel do proc_bind(close) schedule(static) default(none) firstprivate(m1)       &
!$omp firstprivate(val0,val1,val2,val3,val4,val5,val6,val7)                              &
!$omp private(i,arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7)  &
!$omp shared(parg,pval,jint,n,PF_DIST)               &
!$omp private(FOR_K_PREFETCH_T0,FOR_K_PREFETCH_T1,FOR_K_PREFETCH_T2,FOR_K_PREFETCH_NTA)  
             do i=m1,n,8
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
                    call calcei_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call calcei_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call calcei_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call calcei_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
                    arg4.v = parg(i+4).v
                    call calcei_zmm8r8(arg4,val4,jint)
                    pval(i+4).v = val4.v 
                    arg5.v = parg(i+5).v
                    call calcei_zmm8r8(arg5,val5,jint)
                    pval(i+5).v = val5.v  
                    arg6.v = parg(i+6).v
                    call calcei_zmm8r8(arg6,val6,jint)
                    pval(i+6).v = val6.v
                    arg7.v = parg(i+7).v
                    call calcei_zmm8r8(arg7,val7,jint)
                    pval(i+7).v = val7.v
              end do
!$omp end parallel do
       end subroutine calcei_unroll8x_omp_zmm8r8
       
       
       subroutine calcei_unroll4x_omp_zmm8r8(parg,pval,n,jint,PF_DIST)
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calcei_unroll4x_omp_zmm8r8
             !dir$ attributes forceinline :: calcei_unroll4x_omp_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calcei_unroll4x_omp_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             integer(kind=i4) :: i,m,m1
             m = mod(n,4)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call calcei_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<4) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
             !dir$ vector aligned
             !dir$ ivdep
             !dir$ vector vectorlength(8)
             !dir$ vector multiple_gather_scatter_by_shuffles 
             !dir$ vector always
!$omp parallel do proc_bind(close) schedule(static) default(none) firstprivate(m1)       &
!$omp firstprivate(val0,val1,val2,val3)                              &
!$omp private(i,arg0,arg1,arg2,arg3)  &
!$omp shared(parg,pval,jint,n,PF_DIST)               &
!$omp private(FOR_K_PREFETCH_T0,FOR_K_PREFETCH_T1,FOR_K_PREFETCH_T2,FOR_K_PREFETCH_NTA)  
             do i=m1,n,4
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
                    call calcei_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call calcei_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call calcei_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call calcei_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
               end do
!$omp end parallel do
       end subroutine calcei_unroll4x_omp_zmm8r8
       
#if 0
   *
    !*****************************************************************************80
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
*/
#endif


       subroutine calci0_unroll12x_nonblock_zmm8r8(parg,pval,n,jint,PF_DIST)         
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calci0_unroll12x_nonblock_zmm8r8
             !dir$ attributes forceinline :: calci0_unroll12x_nonblock_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calci0_unroll12x_nonblock_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             integer(kind=i4) :: i,m,m1
             m = mod(n,12)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call calci0_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<12) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
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
                    call calci0_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call calci0_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call calci0_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call calci0_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
                    arg0.v = parg(i+4).v
                    call calci0_zmm8r8(arg0,val0,jint)
                    pval(i+4).v = val0.v 
                    arg1.v = parg(i+5).v
                    call calci0_zmm8r8(arg1,val1,jint)
                    pval(i+5).v = val1.v  
                    arg2.v = parg(i+6).v
                    call calci0_zmm8r8(arg2,val2,jint)
                    pval(i+6).v = val2.v
                    arg3.v = parg(i+7).v
                    call calci0_zmm8r8(arg3,val3,jint)
                    pval(i+7).v = val3.v
                    arg0.v = parg(i+8).v
                    call calci0_zmm8r8(arg0,val0,jint)
                    pval(i+8).v = val0.v 
                    arg1.v = parg(i+9).v
                    call calci0_zmm8r8(arg1,val1,jint)
                    pval(i+9).v = val1.v  
                    arg2.v = parg(i+10).v
                    call calci0_zmm8r8(arg2,val2,jint)
                    pval(i+10).v = val2.v
                    arg3.v = parg(i+11).v
                    call calci0_zmm8r8(arg3,val3,jint)
                    pval(i+11).v = val3.v
             end do
       end subroutine calci0_unroll12x_nonblock_zmm8r8
       
       
       subroutine calci0_unroll8x_nonblock_zmm8r8(parg,pval,n,jint,PF_DIST)         
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calci0_unroll8x_nonblock_zmm8r8
             !dir$ attributes forceinline :: calci0_unroll8x_nonblock_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calci0_unroll8x_nonblock_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             integer(kind=i4) :: i,m,m1
             m = mod(n,8)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call calci0_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<8) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
             !dir$ vector aligned
             !dir$ ivdep
             !dir$ vector vectorlength(8)
             !dir$ vector multiple_gather_scatter_by_shuffles 
             !dir$ vector always
             do i=m1,n,8
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
                    call calci0_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call calci0_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call calci0_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call calci0_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
                    arg0.v = parg(i+4).v
                    call calci0_zmm8r8(arg0,val0,jint)
                    pval(i+4).v = val0.v 
                    arg1.v = parg(i+5).v
                    call calci0_zmm8r8(arg1,val1,jint)
                    pval(i+5).v = val1.v  
                    arg2.v = parg(i+6).v
                    call calci0_zmm8r8(arg2,val2,jint)
                    pval(i+6).v = val2.v
                    arg3.v = parg(i+7).v
                    call calci0_zmm8r8(arg3,val3,jint)
                    pval(i+7).v = val3.v
                end do
       end subroutine calci0_unroll8x_nonblock_zmm8r8
       
       
       subroutine calci0_unroll4x_nonblock_zmm8r8(parg,pval,n,jint,PF_DIST)         
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calci0_unroll4x_nonblock_zmm8r8
             !dir$ attributes forceinline :: calci0_unroll4x_nonblock_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calci0_unroll4x_nonblock_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             integer(kind=i4) :: i,m,m1
             m = mod(n,4)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call calci0_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<4) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
             !dir$ vector aligned
             !dir$ ivdep
             !dir$ vector vectorlength(8)
             !dir$ vector multiple_gather_scatter_by_shuffles 
             !dir$ vector always
             do i=m1,n,4
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
                    call calci0_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call calci0_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call calci0_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call calci0_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
                end do
       end subroutine calci0_unroll4x_nonblock_zmm8r8
       
       
       
       
       subroutine calci0_blocked64x32x16x8x_zmm8r8(parg,pval,n,jint,PF_DIST,which)         
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calci0_blocked64x32x16x8x_zmm8r8
             !dir$ attributes forceinline :: calci0_blocked64x32x16x8x_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calci0_blocked64x32x16x8x_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             logical(kind=i4),               intent(in) :: which
             integer(kind=i4), parameter :: BLOCK64X = 64
             integer(kind=i4), parameter :: BLOCK32X = 32
             integer(kind=i4), parameter :: BLOCK16X = 16
             integer(kind=i4), parameter :: BLOCK8X  = 8 
             !dir$ attributes align : 64 :: arg0
             type(ZMM8r8_t), automatic :: arg0
             !dir$ attributes align : 64 :: arg0
             type(ZMM8r8_t), automatic :: val0
             integer(kind=i4) :: i,ii
             
            
             select case(which)
             case (0)
                  do i=1,n,BLOCK64X
#if (__RCS_CYLINDER_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK64X-1,n)
                          arg0.v = parg(ii).v
                          call calci0_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do
             case (1)
                  do i=1,n,BLOCK32X
#if (__RCS_CYLINDER_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK32X-1,n)
                          arg0.v = parg(ii).v
                          call calci0_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do 
             case (2)
                  do i=1,n,BLOCK16X
#if (__RCS_CYLINDER_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK16X-1,n)
                          arg0.v = parg(ii).v
                          call calci0_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do 
             case (3)
                  do i=1,n,BLOCK8X
#if (__RCS_CYLINDER_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK8X-1,n)
                          arg0.v = parg(ii).v
                          call calci0_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do
             case default
                  return
             end select
             
       end subroutine calci0_blocked64x32x16x8x_zmm8r8
       
       
       subroutine calci0_blocked64x32x16x8x_omp_zmm8r8(parg,pval,n,jint,PF_DIST,which)         
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calci0_blocked64x32x16x8x_omp_zmm8r8
             !dir$ attributes forceinline :: calci0_blocked64x32x16x8x_omp_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calci0_blocked64x32x16x8x_omp_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             logical(kind=i4),               intent(in) :: which
             integer(kind=i4), parameter :: BLOCK64X = 64
             integer(kind=i4), parameter :: BLOCK32X = 32
             integer(kind=i4), parameter :: BLOCK16X = 16
             integer(kind=i4), parameter :: BLOCK8X  = 8 
             !dir$ attributes align : 64 :: arg0
             type(ZMM8r8_t), automatic :: arg0
             !dir$ attributes align : 64 :: arg0
             type(ZMM8r8_t), automatic :: val0
             integer(kind=i4) :: i,ii
             
            
             select case(which)
             case (0)
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
!$omp parallel do schedule(static,BLOCK64X) default(none)                &
!$omp firstprivate(BLOCK64X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T0,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2 
!$omp parallel do schedule(static,BLOCK64X) default(none)                &
!$omp firstprivate(BLOCK64X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T1,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
!$omp parallel do schedule(static,BLOCK64X) default(none)                &
!$omp firstprivate(BLOCK64X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T2,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
!$omp parallel do schedule(static,BLOCK64X) default(none)                &
!$omp firstprivate(BLOCK64X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_NTA,parg,pval)
#endif
                  do i=1,n,BLOCK64X

#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif   
         
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK64X-1,n)
                          arg0.v = parg(ii).v
                          call calci0_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do
             case (1)
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
!$omp parallel do schedule(static,BLOCK32X) default(none)                &
!$omp firstprivate(BLOCK32X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T0,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2 
!$omp parallel do schedule(static,BLOCK32X) default(none)                &
!$omp firstprivate(BLOCK32X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T1,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
!$omp parallel do schedule(static,BLOCK32X) default(none)                &
!$omp firstprivate(BLOCK32X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T2,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
!$omp parallel do schedule(static,BLOCK32X) default(none)                &
!$omp firstprivate(BLOCK32X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_NTA,parg,pval)
#endif             
                  do i=1,n,BLOCK32X
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK32X-1,n)
                          arg0.v = parg(ii).v
                          call calci0_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do 
             case (2)
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
!$omp parallel do schedule(static,BLOCK16X) default(none)                &
!$omp firstprivate(BLOCK16X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T0,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2 
!$omp parallel do schedule(static,BLOCK16X) default(none)                &
!$omp firstprivate(BLOCK16X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T1,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
!$omp parallel do schedule(static,BLOCK16X) default(none)                &
!$omp firstprivate(BLOCK16X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T2,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
!$omp parallel do schedule(static,BLOCK16X) default(none)                &
!$omp firstprivate(BLOCK16X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_NTA,parg,pval)
#endif                
                  do i=1,n,BLOCK16X
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK16X-1,n)
                          arg0.v = parg(ii).v
                          call calci0_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do 
             case (3)
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
!$omp parallel do schedule(static,BLOCK8X) default(none)                &
!$omp firstprivate(BLOCK8X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T0,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2 
!$omp parallel do schedule(static,BLOCK8X) default(none)                &
!$omp firstprivate(BLOCK8X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T1,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
!$omp parallel do schedule(static,BLOCK8X) default(none)                &
!$omp firstprivate(BLOCK8X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T2,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
!$omp parallel do schedule(static,BLOCK8X) default(none)                &
!$omp firstprivate(BLOCK8X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_NTA,parg,pval)
#endif                  
                  do i=1,n,BLOCK8X
#if (__RCS_CYLINDER_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK8X-1,n)
                          arg0.v = parg(ii).v
                          call calci0_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do
             case default
                  return
             end select
             
       end subroutine calci0_blocked64x32x16x8x_omp_zmm8r8
       
       
       subroutine calci0_unroll12x_omp_zmm8r8(parg,pval,n,jint,PF_DIST)
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calci0_unroll12x_omp_zmm8r8
             !dir$ attributes forceinline :: calci0_unroll12x_omp_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calci0_unroll12x_omp_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: arg4
             !dir$ attributes align : 64 :: arg5
             !dir$ attributes align : 64 :: arg6
             !dir$ attributes align : 64 :: arg7
             !dir$ attributes align : 64 :: arg8
             !dir$ attributes align : 64 :: arg9
             !dir$ attributes align : 64 :: arg10
             !dir$ attributes align : 64 :: arg11
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             !dir$ attributes align : 64 :: val4
             !dir$ attributes align : 64 :: val5
             !dir$ attributes align : 64 :: val6
             !dir$ attributes align : 64 :: val7
             !dir$ attributes align : 64 :: val8
             !dir$ attributes align : 64 :: val9
             !dir$ attributes align : 64 :: val10
             !dir$ attributes align : 64 :: val11
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: arg4
             type(ZMM8r8_t), automatic :: arg5
             type(ZMM8r8_t), automatic :: arg6
             type(ZMM8r8_t), automatic :: arg7
             type(ZMM8r8_t), automatic :: arg8
             type(ZMM8r8_t), automatic :: arg9
             type(ZMM8r8_t), automatic :: arg10
             type(ZMM8r8_t), automatic :: arg11
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             type(ZMM8r8_t), automatic :: val4
             type(ZMM8r8_t), automatic :: val5
             type(ZMM8r8_t), automatic :: val6
             type(ZMM8r8_t), automatic :: val7
             type(ZMM8r8_t), automatic :: val8
             type(ZMM8r8_t), automatic :: val9
             type(ZMM8r8_t), automatic :: val10
             type(ZMM8r8_t), automatic :: val11
             integer(kind=i4) :: i,m,m1
             m = mod(n,12)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call calci0_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<12) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             val4.v = 0.0_dp
             val5.v = 0.0_dp
             val6.v = 0.0_dp
             val7.v = 0.0_dp
             val8.v = 0.0_dp
             val9.v = 0.0_dp
             val10.v= 0.0_dp
             val11.v= 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
             !dir$ vector aligned
             !dir$ ivdep
             !dir$ vector vectorlength(8)
             !dir$ vector multiple_gather_scatter_by_shuffles 
             !dir$ vector always
!$omp parallel do proc_bind(close) schedule(static) default(none) firstprivate(m1)       &
!$omp firstprivate(val0,val1,val2,val3,val4,val5,val6,val7)                              &
!$omp firstprivate(val8,val9,val10,val11) private(i,arg0,arg1,arg2,arg3,arg4,arg5,arg6)  &
!$omp private(arg7,arg8,arg9,arg10,arg11) shared(parg,pval,jint,n,PF_DIST)               &
!$omp private(FOR_K_PREFETCH_T0,FOR_K_PREFETCH_T1,FOR_K_PREFETCH_T2,FOR_K_PREFETCH_NTA)  
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
                    call calci0_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call calci0_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call calci0_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call calci0_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
                    arg4.v = parg(i+4).v
                    call calci0_zmm8r8(arg4,val4,jint)
                    pval(i+4).v = val4.v 
                    arg5.v = parg(i+5).v
                    call calci0_zmm8r8(arg5,val5,jint)
                    pval(i+5).v = val5.v  
                    arg6.v = parg(i+6).v
                    call calci0_zmm8r8(arg6,val6,jint)
                    pval(i+6).v = val6.v
                    arg7.v = parg(i+7).v
                    call calci0_zmm8r8(arg7,val7,jint)
                    pval(i+7).v = val7.v
                    arg8.v = parg(i+8).v
                    call calci0_zmm8r8(arg8,val8,jint)
                    pval(i+8).v = val8.v 
                    arg9.v = parg(i+9).v
                    call calci0_zmm8r8(arg9,val9,jint)
                    pval(i+9).v = val9.v  
                    arg10.v = parg(i+10).v
                    call calci0_zmm8r8(arg10,val10,jint)
                    pval(i+10).v = val10.v
                    arg11.v = parg(i+11).v
                    call calci0_zmm8r8(arg11,val11,jint)
                    pval(i+11).v = val11.v
             end do
!$omp end parallel do
       end subroutine calci0_unroll12x_omp_zmm8r8
       
       
       subroutine calci0_unroll8x_omp_zmm8r8(parg,pval,n,jint,PF_DIST)
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calci0_unroll8x_omp_zmm8r8
             !dir$ attributes forceinline :: calci0_unroll8x_omp_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calci0_unroll8x_omp_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: arg4
             !dir$ attributes align : 64 :: arg5
             !dir$ attributes align : 64 :: arg6
             !dir$ attributes align : 64 :: arg7
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             !dir$ attributes align : 64 :: val4
             !dir$ attributes align : 64 :: val5
             !dir$ attributes align : 64 :: val6
             !dir$ attributes align : 64 :: val7
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: arg4
             type(ZMM8r8_t), automatic :: arg5
             type(ZMM8r8_t), automatic :: arg6
             type(ZMM8r8_t), automatic :: arg7
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             type(ZMM8r8_t), automatic :: val4
             type(ZMM8r8_t), automatic :: val5
             type(ZMM8r8_t), automatic :: val6
             type(ZMM8r8_t), automatic :: val7
             integer(kind=i4) :: i,m,m1
             m = mod(n,8)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call calci0_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<8) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             val4.v = 0.0_dp
             val5.v = 0.0_dp
             val6.v = 0.0_dp
             val7.v = 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
             !dir$ vector aligned
             !dir$ ivdep
             !dir$ vector vectorlength(8)
             !dir$ vector multiple_gather_scatter_by_shuffles 
             !dir$ vector always
!$omp parallel do proc_bind(close) schedule(static) default(none) firstprivate(m1)       &
!$omp firstprivate(val0,val1,val2,val3,val4,val5,val6,val7)                              &
!$omp private(i,arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7)  &
!$omp shared(parg,pval,jint,n,PF_DIST)               &
!$omp private(FOR_K_PREFETCH_T0,FOR_K_PREFETCH_T1,FOR_K_PREFETCH_T2,FOR_K_PREFETCH_NTA)  
             do i=m1,n,8
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
                    call calci0_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call calci0_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call calci0_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call calci0_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
                    arg4.v = parg(i+4).v
                    call calci0_zmm8r8(arg4,val4,jint)
                    pval(i+4).v = val4.v 
                    arg5.v = parg(i+5).v
                    call calci0_zmm8r8(arg5,val5,jint)
                    pval(i+5).v = val5.v  
                    arg6.v = parg(i+6).v
                    call calci0_zmm8r8(arg6,val6,jint)
                    pval(i+6).v = val6.v
                    arg7.v = parg(i+7).v
                    call calci0_zmm8r8(arg7,val7,jint)
                    pval(i+7).v = val7.v
              end do
!$omp end parallel do
       end subroutine calci0_unroll8x_omp_zmm8r8
       
       
       subroutine calci0_unroll4x_omp_zmm8r8(parg,pval,n,jint,PF_DIST)
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calci0_unroll4x_omp_zmm8r8
             !dir$ attributes forceinline :: calci0_unroll4x_omp_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calci0_unroll4x_omp_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             integer(kind=i4) :: i,m,m1
             m = mod(n,4)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call calci0_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<4) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
             !dir$ vector aligned
             !dir$ ivdep
             !dir$ vector vectorlength(8)
             !dir$ vector multiple_gather_scatter_by_shuffles 
             !dir$ vector always
!$omp parallel do proc_bind(close) schedule(static) default(none) firstprivate(m1)       &
!$omp firstprivate(val0,val1,val2,val3)                              &
!$omp private(i,arg0,arg1,arg2,arg3)  &
!$omp shared(parg,pval,jint,n,PF_DIST)               &
!$omp private(FOR_K_PREFETCH_T0,FOR_K_PREFETCH_T1,FOR_K_PREFETCH_T2,FOR_K_PREFETCH_NTA)  
             do i=m1,n,4
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
                    call calci0_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call calci0_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call calci0_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call calci0_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
               end do
!$omp end parallel do
       end subroutine calci0_unroll4x_omp_zmm8r8
       
       
#if 0
  !*****************************************************************************80
!
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
#endif
       
       
       subroutine calci1_unroll12x_nonblock_zmm8r8(parg,pval,n,jint,PF_DIST)         
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calci1_unroll12x_nonblock_zmm8r8
             !dir$ attributes forceinline :: calci1_unroll12x_nonblock_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calci1_unroll12x_nonblock_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             integer(kind=i4) :: i,m,m1
             m = mod(n,12)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call calci1_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<12) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
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
                    call calci1_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call calci1_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call calci1_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call calci1_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
                    arg0.v = parg(i+4).v
                    call calci1_zmm8r8(arg0,val0,jint)
                    pval(i+4).v = val0.v 
                    arg1.v = parg(i+5).v
                    call calci1_zmm8r8(arg1,val1,jint)
                    pval(i+5).v = val1.v  
                    arg2.v = parg(i+6).v
                    call calci1_zmm8r8(arg2,val2,jint)
                    pval(i+6).v = val2.v
                    arg3.v = parg(i+7).v
                    call calci1_zmm8r8(arg3,val3,jint)
                    pval(i+7).v = val3.v
                    arg0.v = parg(i+8).v
                    call calci1_zmm8r8(arg0,val0,jint)
                    pval(i+8).v = val0.v 
                    arg1.v = parg(i+9).v
                    call calci1_zmm8r8(arg1,val1,jint)
                    pval(i+9).v = val1.v  
                    arg2.v = parg(i+10).v
                    call calci1_zmm8r8(arg2,val2,jint)
                    pval(i+10).v = val2.v
                    arg3.v = parg(i+11).v
                    call calci1_zmm8r8(arg3,val3,jint)
                    pval(i+11).v = val3.v
             end do
       end subroutine calci1_unroll12x_nonblock_zmm8r8
       
       subroutine calci1_unroll8x_nonblock_zmm8r8(parg,pval,n,jint,PF_DIST)         
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calci1_unroll8x_nonblock_zmm8r8
             !dir$ attributes forceinline :: calci1_unroll8x_nonblock_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calci1_unroll8x_nonblock_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             integer(kind=i4) :: i,m,m1
             m = mod(n,8)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call calci1_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<8) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
             !dir$ vector aligned
             !dir$ ivdep
             !dir$ vector vectorlength(8)
             !dir$ vector multiple_gather_scatter_by_shuffles 
             !dir$ vector always
             do i=m1,n,8
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
                    call calci1_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call calci1_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call calci1_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call calci1_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
                    arg0.v = parg(i+4).v
                    call calci1_zmm8r8(arg0,val0,jint)
                    pval(i+4).v = val0.v 
                    arg1.v = parg(i+5).v
                    call calci1_zmm8r8(arg1,val1,jint)
                    pval(i+5).v = val1.v  
                    arg2.v = parg(i+6).v
                    call calci1_zmm8r8(arg2,val2,jint)
                    pval(i+6).v = val2.v
                    arg3.v = parg(i+7).v
                    call calci1_zmm8r8(arg3,val3,jint)
                    pval(i+7).v = val3.v
                end do
       end subroutine calci1_unroll8x_nonblock_zmm8r8
       
       
       subroutine calci1_unroll4x_nonblock_zmm8r8(parg,pval,n,jint,PF_DIST)         
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calci1_unroll4x_nonblock_zmm8r8
             !dir$ attributes forceinline :: calci1_unroll4x_nonblock_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calci1_unroll4x_nonblock_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             integer(kind=i4) :: i,m,m1
             m = mod(n,4)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call calci1_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<4) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
             !dir$ vector aligned
             !dir$ ivdep
             !dir$ vector vectorlength(8)
             !dir$ vector multiple_gather_scatter_by_shuffles 
             !dir$ vector always
             do i=m1,n,4
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
                    call calci1_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call calci1_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call calci1_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call calci1_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
                end do
       end subroutine calci1_unroll4x_nonblock_zmm8r8
       
       
       subroutine calci1_blocked64x32x16x8x_zmm8r8(parg,pval,n,jint,PF_DIST,which)         
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calci1_blocked64x32x16x8x_zmm8r8
             !dir$ attributes forceinline :: calci1_blocked64x32x16x8x_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calci1_blocked64x32x16x8x_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             logical(kind=i4),               intent(in) :: which
             integer(kind=i4), parameter :: BLOCK64X = 64
             integer(kind=i4), parameter :: BLOCK32X = 32
             integer(kind=i4), parameter :: BLOCK16X = 16
             integer(kind=i4), parameter :: BLOCK8X  = 8 
             !dir$ attributes align : 64 :: arg0
             type(ZMM8r8_t), automatic :: arg0
             !dir$ attributes align : 64 :: arg0
             type(ZMM8r8_t), automatic :: val0
             integer(kind=i4) :: i,ii
             
            
             select case(which)
             case (0)
                  do i=1,n,BLOCK64X
#if (__RCS_CYLINDER_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK64X-1,n)
                          arg0.v = parg(ii).v
                          call calci1_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do
             case (1)
                  do i=1,n,BLOCK32X
#if (__RCS_CYLINDER_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK32X-1,n)
                          arg0.v = parg(ii).v
                          call calci1_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do 
             case (2)
                  do i=1,n,BLOCK16X
#if (__RCS_CYLINDER_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK16X-1,n)
                          arg0.v = parg(ii).v
                          call calci1_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do 
             case (3)
                  do i=1,n,BLOCK8X
#if (__RCS_CYLINDER_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK8X-1,n)
                          arg0.v = parg(ii).v
                          call calci1_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do
             case default
                  return
             end select
             
       end subroutine calci1_blocked64x32x16x8x_zmm8r8
       
       
       subroutine calci1_blocked64x32x16x8x_omp_zmm8r8(parg,pval,n,jint,PF_DIST,which)         
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calci1_blocked64x32x16x8x_omp_zmm8r8
             !dir$ attributes forceinline :: calci1_blocked64x32x16x8x_omp_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calci1_blocked64x32x16x8x_omp_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             logical(kind=i4),               intent(in) :: which
             integer(kind=i4), parameter :: BLOCK64X = 64
             integer(kind=i4), parameter :: BLOCK32X = 32
             integer(kind=i4), parameter :: BLOCK16X = 16
             integer(kind=i4), parameter :: BLOCK8X  = 8 
             !dir$ attributes align : 64 :: arg0
             type(ZMM8r8_t), automatic :: arg0
             !dir$ attributes align : 64 :: arg0
             type(ZMM8r8_t), automatic :: val0
             integer(kind=i4) :: i,ii
             
            
             select case(which)
             case (0)
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
!$omp parallel do schedule(static,BLOCK64X) default(none)                &
!$omp firstprivate(BLOCK64X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T0,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2 
!$omp parallel do schedule(static,BLOCK64X) default(none)                &
!$omp firstprivate(BLOCK64X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T1,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
!$omp parallel do schedule(static,BLOCK64X) default(none)                &
!$omp firstprivate(BLOCK64X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T2,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
!$omp parallel do schedule(static,BLOCK64X) default(none)                &
!$omp firstprivate(BLOCK64X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_NTA,parg,pval)
#endif
                  do i=1,n,BLOCK64X

#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif   
         
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK64X-1,n)
                          arg0.v = parg(ii).v
                          call calci1_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do
             case (1)
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
!$omp parallel do schedule(static,BLOCK32X) default(none)                &
!$omp firstprivate(BLOCK32X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T0,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2 
!$omp parallel do schedule(static,BLOCK32X) default(none)                &
!$omp firstprivate(BLOCK32X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T1,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
!$omp parallel do schedule(static,BLOCK32X) default(none)                &
!$omp firstprivate(BLOCK32X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T2,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
!$omp parallel do schedule(static,BLOCK32X) default(none)                &
!$omp firstprivate(BLOCK32X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_NTA,parg,pval)
#endif             
                  do i=1,n,BLOCK32X
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK32X-1,n)
                          arg0.v = parg(ii).v
                          call calci1_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do 
             case (2)
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
!$omp parallel do schedule(static,BLOCK16X) default(none)                &
!$omp firstprivate(BLOCK16X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T0,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2 
!$omp parallel do schedule(static,BLOCK16X) default(none)                &
!$omp firstprivate(BLOCK16X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T1,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
!$omp parallel do schedule(static,BLOCK16X) default(none)                &
!$omp firstprivate(BLOCK16X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T2,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
!$omp parallel do schedule(static,BLOCK16X) default(none)                &
!$omp firstprivate(BLOCK16X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_NTA,parg,pval)
#endif                
                  do i=1,n,BLOCK16X
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK16X-1,n)
                          arg0.v = parg(ii).v
                          call calci1_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do 
             case (3)
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
!$omp parallel do schedule(static,BLOCK8X) default(none)                &
!$omp firstprivate(BLOCK8X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T0,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2 
!$omp parallel do schedule(static,BLOCK8X) default(none)                &
!$omp firstprivate(BLOCK8X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T1,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
!$omp parallel do schedule(static,BLOCK8X) default(none)                &
!$omp firstprivate(BLOCK8X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T2,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
!$omp parallel do schedule(static,BLOCK8X) default(none)                &
!$omp firstprivate(BLOCK8X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_NTA,parg,pval)
#endif                  
                  do i=1,n,BLOCK8X
#if (__RCS_CYLINDER_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK8X-1,n)
                          arg0.v = parg(ii).v
                          call calci1_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do
             case default
                  return
             end select
             
       end subroutine calci1_blocked64x32x16x8x_omp_zmm8r8
       
       subroutine calci1_unroll12x_omp_zmm8r8(parg,pval,n,jint,PF_DIST)
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calci1_unroll12x_omp_zmm8r8
             !dir$ attributes forceinline :: calci1_unroll12x_omp_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calci1_unroll12x_omp_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: arg4
             !dir$ attributes align : 64 :: arg5
             !dir$ attributes align : 64 :: arg6
             !dir$ attributes align : 64 :: arg7
             !dir$ attributes align : 64 :: arg8
             !dir$ attributes align : 64 :: arg9
             !dir$ attributes align : 64 :: arg10
             !dir$ attributes align : 64 :: arg11
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             !dir$ attributes align : 64 :: val4
             !dir$ attributes align : 64 :: val5
             !dir$ attributes align : 64 :: val6
             !dir$ attributes align : 64 :: val7
             !dir$ attributes align : 64 :: val8
             !dir$ attributes align : 64 :: val9
             !dir$ attributes align : 64 :: val10
             !dir$ attributes align : 64 :: val11
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: arg4
             type(ZMM8r8_t), automatic :: arg5
             type(ZMM8r8_t), automatic :: arg6
             type(ZMM8r8_t), automatic :: arg7
             type(ZMM8r8_t), automatic :: arg8
             type(ZMM8r8_t), automatic :: arg9
             type(ZMM8r8_t), automatic :: arg10
             type(ZMM8r8_t), automatic :: arg11
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             type(ZMM8r8_t), automatic :: val4
             type(ZMM8r8_t), automatic :: val5
             type(ZMM8r8_t), automatic :: val6
             type(ZMM8r8_t), automatic :: val7
             type(ZMM8r8_t), automatic :: val8
             type(ZMM8r8_t), automatic :: val9
             type(ZMM8r8_t), automatic :: val10
             type(ZMM8r8_t), automatic :: val11
             integer(kind=i4) :: i,m,m1
             m = mod(n,12)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call calci1_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<12) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             val4.v = 0.0_dp
             val5.v = 0.0_dp
             val6.v = 0.0_dp
             val7.v = 0.0_dp
             val8.v = 0.0_dp
             val9.v = 0.0_dp
             val10.v= 0.0_dp
             val11.v= 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
             !dir$ vector aligned
             !dir$ ivdep
             !dir$ vector vectorlength(8)
             !dir$ vector multiple_gather_scatter_by_shuffles 
             !dir$ vector always
!$omp parallel do proc_bind(close) schedule(static) default(none) firstprivate(m1)       &
!$omp firstprivate(val0,val1,val2,val3,val4,val5,val6,val7)                              &
!$omp firstprivate(val8,val9,val10,val11) private(i,arg0,arg1,arg2,arg3,arg4,arg5,arg6)  &
!$omp private(arg7,arg8,arg9,arg10,arg11) shared(parg,pval,jint,n,PF_DIST)               &
!$omp private(FOR_K_PREFETCH_T0,FOR_K_PREFETCH_T1,FOR_K_PREFETCH_T2,FOR_K_PREFETCH_NTA)  
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
                    call calci1_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call calci1_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call calci1_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call calci1_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
                    arg4.v = parg(i+4).v
                    call calci1_zmm8r8(arg4,val4,jint)
                    pval(i+4).v = val4.v 
                    arg5.v = parg(i+5).v
                    call calci1_zmm8r8(arg5,val5,jint)
                    pval(i+5).v = val5.v  
                    arg6.v = parg(i+6).v
                    call calci1_zmm8r8(arg6,val6,jint)
                    pval(i+6).v = val6.v
                    arg7.v = parg(i+7).v
                    call calci1_zmm8r8(arg7,val7,jint)
                    pval(i+7).v = val7.v
                    arg8.v = parg(i+8).v
                    call calci1_zmm8r8(arg8,val8,jint)
                    pval(i+8).v = val8.v 
                    arg9.v = parg(i+9).v
                    call calci1_zmm8r8(arg9,val9,jint)
                    pval(i+9).v = val9.v  
                    arg10.v = parg(i+10).v
                    call calci1_zmm8r8(arg10,val10,jint)
                    pval(i+10).v = val10.v
                    arg11.v = parg(i+11).v
                    call calci1_zmm8r8(arg11,val11,jint)
                    pval(i+11).v = val11.v
             end do
!$omp end parallel do
       end subroutine calci1_unroll12x_omp_zmm8r8
       
       subroutine calci1_unroll8x_omp_zmm8r8(parg,pval,n,jint,PF_DIST)
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calci1_unroll8x_omp_zmm8r8
             !dir$ attributes forceinline :: calci1_unroll8x_omp_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calci1_unroll8x_omp_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: arg4
             !dir$ attributes align : 64 :: arg5
             !dir$ attributes align : 64 :: arg6
             !dir$ attributes align : 64 :: arg7
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             !dir$ attributes align : 64 :: val4
             !dir$ attributes align : 64 :: val5
             !dir$ attributes align : 64 :: val6
             !dir$ attributes align : 64 :: val7
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: arg4
             type(ZMM8r8_t), automatic :: arg5
             type(ZMM8r8_t), automatic :: arg6
             type(ZMM8r8_t), automatic :: arg7
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             type(ZMM8r8_t), automatic :: val4
             type(ZMM8r8_t), automatic :: val5
             type(ZMM8r8_t), automatic :: val6
             type(ZMM8r8_t), automatic :: val7
             integer(kind=i4) :: i,m,m1
             m = mod(n,8)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call calci1_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<8) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             val4.v = 0.0_dp
             val5.v = 0.0_dp
             val6.v = 0.0_dp
             val7.v = 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
             !dir$ vector aligned
             !dir$ ivdep
             !dir$ vector vectorlength(8)
             !dir$ vector multiple_gather_scatter_by_shuffles 
             !dir$ vector always
!$omp parallel do proc_bind(close) schedule(static) default(none) firstprivate(m1)       &
!$omp firstprivate(val0,val1,val2,val3,val4,val5,val6,val7)                              &
!$omp private(i,arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7)  &
!$omp shared(parg,pval,jint,n,PF_DIST)               &
!$omp private(FOR_K_PREFETCH_T0,FOR_K_PREFETCH_T1,FOR_K_PREFETCH_T2,FOR_K_PREFETCH_NTA)  
             do i=m1,n,8
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
                    call calci1_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call calci1_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call calci1_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call calci1_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
                    arg4.v = parg(i+4).v
                    call calci1_zmm8r8(arg4,val4,jint)
                    pval(i+4).v = val4.v 
                    arg5.v = parg(i+5).v
                    call calci1_zmm8r8(arg5,val5,jint)
                    pval(i+5).v = val5.v  
                    arg6.v = parg(i+6).v
                    call calci1_zmm8r8(arg6,val6,jint)
                    pval(i+6).v = val6.v
                    arg7.v = parg(i+7).v
                    call calci1_zmm8r8(arg7,val7,jint)
                    pval(i+7).v = val7.v
              end do
!$omp end parallel do
       end subroutine calci1_unroll8x_omp_zmm8r8
       
       
       subroutine calci1_unroll4x_omp_zmm8r8(parg,pval,n,jint,PF_DIST)
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calci1_unroll4x_omp_zmm8r8
             !dir$ attributes forceinline :: calci1_unroll4x_omp_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calci1_unroll4x_omp_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             integer(kind=i4) :: i,m,m1
             m = mod(n,4)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call calci1_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<4) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
             !dir$ vector aligned
             !dir$ ivdep
             !dir$ vector vectorlength(8)
             !dir$ vector multiple_gather_scatter_by_shuffles 
             !dir$ vector always
!$omp parallel do proc_bind(close) schedule(static) default(none) firstprivate(m1)       &
!$omp firstprivate(val0,val1,val2,val3)                              &
!$omp private(i,arg0,arg1,arg2,arg3)  &
!$omp shared(parg,pval,jint,n,PF_DIST)               &
!$omp private(FOR_K_PREFETCH_T0,FOR_K_PREFETCH_T1,FOR_K_PREFETCH_T2,FOR_K_PREFETCH_NTA)  
             do i=m1,n,4
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
                    call calci1_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call calci1_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call calci1_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call calci1_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
               end do
!$omp end parallel do
       end subroutine calci1_unroll4x_omp_zmm8r8
       
       
#if 0
/*
*****************************************************************************80
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
*/
#endif


         subroutine calck0_unroll12x_nonblock_zmm8r8(parg,pval,n,jint,PF_DIST)         
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calck0_unroll12x_nonblock_zmm8r8
             !dir$ attributes forceinline :: calck0_unroll12x_nonblock_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calck0_unroll12x_nonblock_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             integer(kind=i4) :: i,m,m1
             m = mod(n,12)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call calck0_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<12) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
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
                    call calck0_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call calck0_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call calck0_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call calck0_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
                    arg0.v = parg(i+4).v
                    call calck0_zmm8r8(arg0,val0,jint)
                    pval(i+4).v = val0.v 
                    arg1.v = parg(i+5).v
                    call calck0_zmm8r8(arg1,val1,jint)
                    pval(i+5).v = val1.v  
                    arg2.v = parg(i+6).v
                    call calck0_zmm8r8(arg2,val2,jint)
                    pval(i+6).v = val2.v
                    arg3.v = parg(i+7).v
                    call calck0_zmm8r8(arg3,val3,jint)
                    pval(i+7).v = val3.v
                    arg0.v = parg(i+8).v
                    call calck0_zmm8r8(arg0,val0,jint)
                    pval(i+8).v = val0.v 
                    arg1.v = parg(i+9).v
                    call calck0_zmm8r8(arg1,val1,jint)
                    pval(i+9).v = val1.v  
                    arg2.v = parg(i+10).v
                    call calck0_zmm8r8(arg2,val2,jint)
                    pval(i+10).v = val2.v
                    arg3.v = parg(i+11).v
                    call calck0_zmm8r8(arg3,val3,jint)
                    pval(i+11).v = val3.v
             end do
       end subroutine calck0_unroll12x_nonblock_zmm8r8
       
       subroutine calck0_unroll8x_nonblock_zmm8r8(parg,pval,n,jint,PF_DIST)         
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calck0_unroll8x_nonblock_zmm8r8
             !dir$ attributes forceinline :: calck0_unroll8x_nonblock_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calck0_unroll8x_nonblock_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             integer(kind=i4) :: i,m,m1
             m = mod(n,8)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call calck0_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<8) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
             !dir$ vector aligned
             !dir$ ivdep
             !dir$ vector vectorlength(8)
             !dir$ vector multiple_gather_scatter_by_shuffles 
             !dir$ vector always
             do i=m1,n,8
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
                    call calck0_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call calck0_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call calck0_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call calck0_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
                    arg0.v = parg(i+4).v
                    call calck0_zmm8r8(arg0,val0,jint)
                    pval(i+4).v = val0.v 
                    arg1.v = parg(i+5).v
                    call calck0_zmm8r8(arg1,val1,jint)
                    pval(i+5).v = val1.v  
                    arg2.v = parg(i+6).v
                    call calck0_zmm8r8(arg2,val2,jint)
                    pval(i+6).v = val2.v
                    arg3.v = parg(i+7).v
                    call calck0_zmm8r8(arg3,val3,jint)
                    pval(i+7).v = val3.v
                end do
       end subroutine calck0_unroll8x_nonblock_zmm8r8
       
       
       subroutine calck0_unroll4x_nonblock_zmm8r8(parg,pval,n,jint,PF_DIST)         
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calck0_unroll4x_nonblock_zmm8r8
             !dir$ attributes forceinline :: calck0_unroll4x_nonblock_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calck0_unroll4x_nonblock_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             integer(kind=i4) :: i,m,m1
             m = mod(n,4)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call calck0_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<4) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
             !dir$ vector aligned
             !dir$ ivdep
             !dir$ vector vectorlength(8)
             !dir$ vector multiple_gather_scatter_by_shuffles 
             !dir$ vector always
             do i=m1,n,4
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
                    call calck0_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call calck0_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call calck0_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call calck0_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
                end do
       end subroutine calck0_unroll4x_nonblock_zmm8r8
       
       
       subroutine calck0_blocked64x32x16x8x_zmm8r8(parg,pval,n,jint,PF_DIST,which)         
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calck0_blocked64x32x16x8x_zmm8r8
             !dir$ attributes forceinline :: calck0_blocked64x32x16x8x_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calck0_blocked64x32x16x8x_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             logical(kind=i4),               intent(in) :: which
             integer(kind=i4), parameter :: BLOCK64X = 64
             integer(kind=i4), parameter :: BLOCK32X = 32
             integer(kind=i4), parameter :: BLOCK16X = 16
             integer(kind=i4), parameter :: BLOCK8X  = 8 
             !dir$ attributes align : 64 :: arg0
             type(ZMM8r8_t), automatic :: arg0
             !dir$ attributes align : 64 :: arg0
             type(ZMM8r8_t), automatic :: val0
             integer(kind=i4) :: i,ii
             
            
             select case(which)
             case (0)
                  do i=1,n,BLOCK64X
#if (__RCS_CYLINDER_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK64X-1,n)
                          arg0.v = parg(ii).v
                          call calck0_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do
             case (1)
                  do i=1,n,BLOCK32X
#if (__RCS_CYLINDER_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK32X-1,n)
                          arg0.v = parg(ii).v
                          call calck0_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do 
             case (2)
                  do i=1,n,BLOCK16X
#if (__RCS_CYLINDER_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK16X-1,n)
                          arg0.v = parg(ii).v
                          call calck0_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do 
             case (3)
                  do i=1,n,BLOCK8X
#if (__RCS_CYLINDER_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK8X-1,n)
                          arg0.v = parg(ii).v
                          call calck0_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do
             case default
                  return
             end select
             
       end subroutine calck0_blocked64x32x16x8x_zmm8r8
       
       subroutine calck0_blocked64x32x16x8x_omp_zmm8r8(parg,pval,n,jint,PF_DIST,which)         
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calck0_blocked64x32x16x8x_omp_zmm8r8
             !dir$ attributes forceinline :: calck0_blocked64x32x16x8x_omp_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calck0_blocked64x32x16x8x_omp_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             logical(kind=i4),               intent(in) :: which
             integer(kind=i4), parameter :: BLOCK64X = 64
             integer(kind=i4), parameter :: BLOCK32X = 32
             integer(kind=i4), parameter :: BLOCK16X = 16
             integer(kind=i4), parameter :: BLOCK8X  = 8 
             !dir$ attributes align : 64 :: arg0
             type(ZMM8r8_t), automatic :: arg0
             !dir$ attributes align : 64 :: arg0
             type(ZMM8r8_t), automatic :: val0
             integer(kind=i4) :: i,ii
             
            
             select case(which)
             case (0)
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
!$omp parallel do schedule(static,BLOCK64X) default(none)                &
!$omp firstprivate(BLOCK64X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T0,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2 
!$omp parallel do schedule(static,BLOCK64X) default(none)                &
!$omp firstprivate(BLOCK64X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T1,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
!$omp parallel do schedule(static,BLOCK64X) default(none)                &
!$omp firstprivate(BLOCK64X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T2,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
!$omp parallel do schedule(static,BLOCK64X) default(none)                &
!$omp firstprivate(BLOCK64X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_NTA,parg,pval)
#endif
                  do i=1,n,BLOCK64X

#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif   
         
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK64X-1,n)
                          arg0.v = parg(ii).v
                          call calck0_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do
             case (1)
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
!$omp parallel do schedule(static,BLOCK32X) default(none)                &
!$omp firstprivate(BLOCK32X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T0,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2 
!$omp parallel do schedule(static,BLOCK32X) default(none)                &
!$omp firstprivate(BLOCK32X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T1,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
!$omp parallel do schedule(static,BLOCK32X) default(none)                &
!$omp firstprivate(BLOCK32X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T2,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
!$omp parallel do schedule(static,BLOCK32X) default(none)                &
!$omp firstprivate(BLOCK32X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_NTA,parg,pval)
#endif             
                  do i=1,n,BLOCK32X
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK32X-1,n)
                          arg0.v = parg(ii).v
                          call calck0_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do 
             case (2)
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
!$omp parallel do schedule(static,BLOCK16X) default(none)                &
!$omp firstprivate(BLOCK16X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T0,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2 
!$omp parallel do schedule(static,BLOCK16X) default(none)                &
!$omp firstprivate(BLOCK16X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T1,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
!$omp parallel do schedule(static,BLOCK16X) default(none)                &
!$omp firstprivate(BLOCK16X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T2,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
!$omp parallel do schedule(static,BLOCK16X) default(none)                &
!$omp firstprivate(BLOCK16X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_NTA,parg,pval)
#endif                
                  do i=1,n,BLOCK16X
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK16X-1,n)
                          arg0.v = parg(ii).v
                          call calck0_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do 
             case (3)
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
!$omp parallel do schedule(static,BLOCK8X) default(none)                &
!$omp firstprivate(BLOCK8X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T0,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2 
!$omp parallel do schedule(static,BLOCK8X) default(none)                &
!$omp firstprivate(BLOCK8X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T1,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
!$omp parallel do schedule(static,BLOCK8X) default(none)                &
!$omp firstprivate(BLOCK8X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T2,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
!$omp parallel do schedule(static,BLOCK8X) default(none)                &
!$omp firstprivate(BLOCK8X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_NTA,parg,pval)
#endif                  
                  do i=1,n,BLOCK8X
#if (__RCS_CYLINDER_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK8X-1,n)
                          arg0.v = parg(ii).v
                          call calck0_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do
             case default
                  return
             end select
             
       end subroutine calck0_blocked64x32x16x8x_omp_zmm8r8
       
       
       subroutine calck0_unroll12x_omp_zmm8r8(parg,pval,n,jint,PF_DIST)
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calck0_unroll12x_omp_zmm8r8
             !dir$ attributes forceinline :: calck0_unroll12x_omp_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calck0_unroll12x_omp_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: arg4
             !dir$ attributes align : 64 :: arg5
             !dir$ attributes align : 64 :: arg6
             !dir$ attributes align : 64 :: arg7
             !dir$ attributes align : 64 :: arg8
             !dir$ attributes align : 64 :: arg9
             !dir$ attributes align : 64 :: arg10
             !dir$ attributes align : 64 :: arg11
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             !dir$ attributes align : 64 :: val4
             !dir$ attributes align : 64 :: val5
             !dir$ attributes align : 64 :: val6
             !dir$ attributes align : 64 :: val7
             !dir$ attributes align : 64 :: val8
             !dir$ attributes align : 64 :: val9
             !dir$ attributes align : 64 :: val10
             !dir$ attributes align : 64 :: val11
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: arg4
             type(ZMM8r8_t), automatic :: arg5
             type(ZMM8r8_t), automatic :: arg6
             type(ZMM8r8_t), automatic :: arg7
             type(ZMM8r8_t), automatic :: arg8
             type(ZMM8r8_t), automatic :: arg9
             type(ZMM8r8_t), automatic :: arg10
             type(ZMM8r8_t), automatic :: arg11
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             type(ZMM8r8_t), automatic :: val4
             type(ZMM8r8_t), automatic :: val5
             type(ZMM8r8_t), automatic :: val6
             type(ZMM8r8_t), automatic :: val7
             type(ZMM8r8_t), automatic :: val8
             type(ZMM8r8_t), automatic :: val9
             type(ZMM8r8_t), automatic :: val10
             type(ZMM8r8_t), automatic :: val11
             integer(kind=i4) :: i,m,m1
             m = mod(n,12)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call calck0_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<12) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             val4.v = 0.0_dp
             val5.v = 0.0_dp
             val6.v = 0.0_dp
             val7.v = 0.0_dp
             val8.v = 0.0_dp
             val9.v = 0.0_dp
             val10.v= 0.0_dp
             val11.v= 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
             !dir$ vector aligned
             !dir$ ivdep
             !dir$ vector vectorlength(8)
             !dir$ vector multiple_gather_scatter_by_shuffles 
             !dir$ vector always
!$omp parallel do proc_bind(close) schedule(static) default(none) firstprivate(m1)       &
!$omp firstprivate(val0,val1,val2,val3,val4,val5,val6,val7)                              &
!$omp firstprivate(val8,val9,val10,val11) private(i,arg0,arg1,arg2,arg3,arg4,arg5,arg6)  &
!$omp private(arg7,arg8,arg9,arg10,arg11) shared(parg,pval,jint,n,PF_DIST)               &
!$omp private(FOR_K_PREFETCH_T0,FOR_K_PREFETCH_T1,FOR_K_PREFETCH_T2,FOR_K_PREFETCH_NTA)  
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
                    call calck0_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call calck0_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call calck0_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call calck0_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
                    arg4.v = parg(i+4).v
                    call calck0_zmm8r8(arg4,val4,jint)
                    pval(i+4).v = val4.v 
                    arg5.v = parg(i+5).v
                    call calck0_zmm8r8(arg5,val5,jint)
                    pval(i+5).v = val5.v  
                    arg6.v = parg(i+6).v
                    call calck0_zmm8r8(arg6,val6,jint)
                    pval(i+6).v = val6.v
                    arg7.v = parg(i+7).v
                    call calck0_zmm8r8(arg7,val7,jint)
                    pval(i+7).v = val7.v
                    arg8.v = parg(i+8).v
                    call calck0_zmm8r8(arg8,val8,jint)
                    pval(i+8).v = val8.v 
                    arg9.v = parg(i+9).v
                    call calck0_zmm8r8(arg9,val9,jint)
                    pval(i+9).v = val9.v  
                    arg10.v = parg(i+10).v
                    call calck0_zmm8r8(arg10,val10,jint)
                    pval(i+10).v = val10.v
                    arg11.v = parg(i+11).v
                    call calck0_zmm8r8(arg11,val11,jint)
                    pval(i+11).v = val11.v
             end do
!$omp end parallel do
       end subroutine calck0_unroll12x_omp_zmm8r8
       
       
       subroutine calck0_unroll8x_omp_zmm8r8(parg,pval,n,jint,PF_DIST)
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calck0_unroll8x_omp_zmm8r8
             !dir$ attributes forceinline :: calck0_unroll8x_omp_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calck0_unroll8x_omp_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: arg4
             !dir$ attributes align : 64 :: arg5
             !dir$ attributes align : 64 :: arg6
             !dir$ attributes align : 64 :: arg7
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             !dir$ attributes align : 64 :: val4
             !dir$ attributes align : 64 :: val5
             !dir$ attributes align : 64 :: val6
             !dir$ attributes align : 64 :: val7
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: arg4
             type(ZMM8r8_t), automatic :: arg5
             type(ZMM8r8_t), automatic :: arg6
             type(ZMM8r8_t), automatic :: arg7
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             type(ZMM8r8_t), automatic :: val4
             type(ZMM8r8_t), automatic :: val5
             type(ZMM8r8_t), automatic :: val6
             type(ZMM8r8_t), automatic :: val7
             integer(kind=i4) :: i,m,m1
             m = mod(n,8)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call calck0_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<8) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             val4.v = 0.0_dp
             val5.v = 0.0_dp
             val6.v = 0.0_dp
             val7.v = 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
             !dir$ vector aligned
             !dir$ ivdep
             !dir$ vector vectorlength(8)
             !dir$ vector multiple_gather_scatter_by_shuffles 
             !dir$ vector always
!$omp parallel do proc_bind(close) schedule(static) default(none) firstprivate(m1)       &
!$omp firstprivate(val0,val1,val2,val3,val4,val5,val6,val7)                              &
!$omp private(i,arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7)  &
!$omp shared(parg,pval,jint,n,PF_DIST)               &
!$omp private(FOR_K_PREFETCH_T0,FOR_K_PREFETCH_T1,FOR_K_PREFETCH_T2,FOR_K_PREFETCH_NTA)  
             do i=m1,n,8
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
                    call calck0_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call calck0_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call calck0_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call calck0_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
                    arg4.v = parg(i+4).v
                    call calck0_zmm8r8(arg4,val4,jint)
                    pval(i+4).v = val4.v 
                    arg5.v = parg(i+5).v
                    call calck0_zmm8r8(arg5,val5,jint)
                    pval(i+5).v = val5.v  
                    arg6.v = parg(i+6).v
                    call calck0_zmm8r8(arg6,val6,jint)
                    pval(i+6).v = val6.v
                    arg7.v = parg(i+7).v
                    call calck0_zmm8r8(arg7,val7,jint)
                    pval(i+7).v = val7.v
              end do
!$omp end parallel do
       end subroutine calck0_unroll8x_omp_zmm8r8
       
       
       subroutine calck0_unroll4x_omp_zmm8r8(parg,pval,n,jint,PF_DIST)
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calck0_unroll4x_omp_zmm8r8
             !dir$ attributes forceinline :: calck0_unroll4x_omp_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calck0_unroll4x_omp_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             integer(kind=i4) :: i,m,m1
             m = mod(n,4)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call calck0_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<4) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
             !dir$ vector aligned
             !dir$ ivdep
             !dir$ vector vectorlength(8)
             !dir$ vector multiple_gather_scatter_by_shuffles 
             !dir$ vector always
!$omp parallel do proc_bind(close) schedule(static) default(none) firstprivate(m1)       &
!$omp firstprivate(val0,val1,val2,val3)                              &
!$omp private(i,arg0,arg1,arg2,arg3)  &
!$omp shared(parg,pval,jint,n,PF_DIST)               &
!$omp private(FOR_K_PREFETCH_T0,FOR_K_PREFETCH_T1,FOR_K_PREFETCH_T2,FOR_K_PREFETCH_NTA)  
             do i=m1,n,4
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
                    call calck0_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call calck0_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call calck0_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call calck0_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
               end do
!$omp end parallel do
       end subroutine calck0_unroll4x_omp_zmm8r8
       
#if 0
  !*****************************************************************************80
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
#endif


         subroutine calck1_unroll12x_nonblock_zmm8r8(parg,pval,n,jint,PF_DIST)         
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calck1_unroll12x_nonblock_zmm8r8
             !dir$ attributes forceinline :: calck1_unroll12x_nonblock_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calck1_unroll12x_nonblock_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             integer(kind=i4) :: i,m,m1
             m = mod(n,12)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call calck1_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<12) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
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
                    call calck1_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call calck1_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call calck1_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call calck1_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
                    arg0.v = parg(i+4).v
                    call calck1_zmm8r8(arg0,val0,jint)
                    pval(i+4).v = val0.v 
                    arg1.v = parg(i+5).v
                    call calck1_zmm8r8(arg1,val1,jint)
                    pval(i+5).v = val1.v  
                    arg2.v = parg(i+6).v
                    call calck1_zmm8r8(arg2,val2,jint)
                    pval(i+6).v = val2.v
                    arg3.v = parg(i+7).v
                    call calck1_zmm8r8(arg3,val3,jint)
                    pval(i+7).v = val3.v
                    arg0.v = parg(i+8).v
                    call calck1_zmm8r8(arg0,val0,jint)
                    pval(i+8).v = val0.v 
                    arg1.v = parg(i+9).v
                    call calck1_zmm8r8(arg1,val1,jint)
                    pval(i+9).v = val1.v  
                    arg2.v = parg(i+10).v
                    call calck1_zmm8r8(arg2,val2,jint)
                    pval(i+10).v = val2.v
                    arg3.v = parg(i+11).v
                    call calck1_zmm8r8(arg3,val3,jint)
                    pval(i+11).v = val3.v
             end do
       end subroutine calck1_unroll12x_nonblock_zmm8r8
       
       
       subroutine calck1_unroll8x_nonblock_zmm8r8(parg,pval,n,jint,PF_DIST)         
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calck1_unroll8x_nonblock_zmm8r8
             !dir$ attributes forceinline :: calck1_unroll8x_nonblock_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calck1_unroll8x_nonblock_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             integer(kind=i4) :: i,m,m1
             m = mod(n,8)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call calck1_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<8) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
             !dir$ vector aligned
             !dir$ ivdep
             !dir$ vector vectorlength(8)
             !dir$ vector multiple_gather_scatter_by_shuffles 
             !dir$ vector always
             do i=m1,n,8
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
                    call calck1_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call calck1_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call calck1_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call calck1_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
                    arg0.v = parg(i+4).v
                    call calck1_zmm8r8(arg0,val0,jint)
                    pval(i+4).v = val0.v 
                    arg1.v = parg(i+5).v
                    call calck1_zmm8r8(arg1,val1,jint)
                    pval(i+5).v = val1.v  
                    arg2.v = parg(i+6).v
                    call calck1_zmm8r8(arg2,val2,jint)
                    pval(i+6).v = val2.v
                    arg3.v = parg(i+7).v
                    call calck1_zmm8r8(arg3,val3,jint)
                    pval(i+7).v = val3.v
                end do
       end subroutine calck1_unroll8x_nonblock_zmm8r8
       
       
       subroutine calck1_unroll4x_nonblock_zmm8r8(parg,pval,n,jint,PF_DIST)         
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calck1_unroll4x_nonblock_zmm8r8
             !dir$ attributes forceinline :: calck1_unroll4x_nonblock_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calck1_unroll4x_nonblock_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             integer(kind=i4) :: i,m,m1
             m = mod(n,4)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call calck1_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<4) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
             !dir$ vector aligned
             !dir$ ivdep
             !dir$ vector vectorlength(8)
             !dir$ vector multiple_gather_scatter_by_shuffles 
             !dir$ vector always
             do i=m1,n,4
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
                    call calck1_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call calck1_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call calck1_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call calck1_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
                end do
       end subroutine calck1_unroll4x_nonblock_zmm8r8
       
       
       subroutine calck1_blocked64x32x16x8x_zmm8r8(parg,pval,n,jint,PF_DIST,which)         
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calck1_blocked64x32x16x8x_zmm8r8
             !dir$ attributes forceinline :: calck1_blocked64x32x16x8x_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calck1_blocked64x32x16x8x_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             logical(kind=i4),               intent(in) :: which
             integer(kind=i4), parameter :: BLOCK64X = 64
             integer(kind=i4), parameter :: BLOCK32X = 32
             integer(kind=i4), parameter :: BLOCK16X = 16
             integer(kind=i4), parameter :: BLOCK8X  = 8 
             !dir$ attributes align : 64 :: arg0
             type(ZMM8r8_t), automatic :: arg0
             !dir$ attributes align : 64 :: arg0
             type(ZMM8r8_t), automatic :: val0
             integer(kind=i4) :: i,ii
             
            
             select case(which)
             case (0)
                  do i=1,n,BLOCK64X
#if (__RCS_CYLINDER_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK64X-1,n)
                          arg0.v = parg(ii).v
                          call calck1_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do
             case (1)
                  do i=1,n,BLOCK32X
#if (__RCS_CYLINDER_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK32X-1,n)
                          arg0.v = parg(ii).v
                          call calck1_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do 
             case (2)
                  do i=1,n,BLOCK16X
#if (__RCS_CYLINDER_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK16X-1,n)
                          arg0.v = parg(ii).v
                          call calck1_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do 
             case (3)
                  do i=1,n,BLOCK8X
#if (__RCS_CYLINDER_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK8X-1,n)
                          arg0.v = parg(ii).v
                          call calck1_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do
             case default
                  return
             end select
             
       end subroutine calck1_blocked64x32x16x8x_zmm8r8
       
       
       subroutine calck1_blocked64x32x16x8x_omp_zmm8r8(parg,pval,n,jint,PF_DIST,which)         
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calck1_blocked64x32x16x8x_omp_zmm8r8
             !dir$ attributes forceinline :: calck1_blocked64x32x16x8x_omp_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calck1_blocked64x32x16x8x_omp_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             logical(kind=i4),               intent(in) :: which
             integer(kind=i4), parameter :: BLOCK64X = 64
             integer(kind=i4), parameter :: BLOCK32X = 32
             integer(kind=i4), parameter :: BLOCK16X = 16
             integer(kind=i4), parameter :: BLOCK8X  = 8 
             !dir$ attributes align : 64 :: arg0
             type(ZMM8r8_t), automatic :: arg0
             !dir$ attributes align : 64 :: arg0
             type(ZMM8r8_t), automatic :: val0
             integer(kind=i4) :: i,ii
             
            
             select case(which)
             case (0)
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
!$omp parallel do schedule(static,BLOCK64X) default(none)                &
!$omp firstprivate(BLOCK64X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T0,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2 
!$omp parallel do schedule(static,BLOCK64X) default(none)                &
!$omp firstprivate(BLOCK64X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T1,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
!$omp parallel do schedule(static,BLOCK64X) default(none)                &
!$omp firstprivate(BLOCK64X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T2,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
!$omp parallel do schedule(static,BLOCK64X) default(none)                &
!$omp firstprivate(BLOCK64X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_NTA,parg,pval)
#endif
                  do i=1,n,BLOCK64X

#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif   
         
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK64X-1,n)
                          arg0.v = parg(ii).v
                          call calck1_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do
             case (1)
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
!$omp parallel do schedule(static,BLOCK32X) default(none)                &
!$omp firstprivate(BLOCK32X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T0,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2 
!$omp parallel do schedule(static,BLOCK32X) default(none)                &
!$omp firstprivate(BLOCK32X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T1,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
!$omp parallel do schedule(static,BLOCK32X) default(none)                &
!$omp firstprivate(BLOCK32X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T2,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
!$omp parallel do schedule(static,BLOCK32X) default(none)                &
!$omp firstprivate(BLOCK32X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_NTA,parg,pval)
#endif             
                  do i=1,n,BLOCK32X
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK32X-1,n)
                          arg0.v = parg(ii).v
                          call calck1_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do 
             case (2)
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
!$omp parallel do schedule(static,BLOCK16X) default(none)                &
!$omp firstprivate(BLOCK16X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T0,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2 
!$omp parallel do schedule(static,BLOCK16X) default(none)                &
!$omp firstprivate(BLOCK16X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T1,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
!$omp parallel do schedule(static,BLOCK16X) default(none)                &
!$omp firstprivate(BLOCK16X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T2,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
!$omp parallel do schedule(static,BLOCK16X) default(none)                &
!$omp firstprivate(BLOCK16X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_NTA,parg,pval)
#endif                
                  do i=1,n,BLOCK16X
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK16X-1,n)
                          arg0.v = parg(ii).v
                          call calck1_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do 
             case (3)
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
!$omp parallel do schedule(static,BLOCK8X) default(none)                &
!$omp firstprivate(BLOCK8X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T0,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2 
!$omp parallel do schedule(static,BLOCK8X) default(none)                &
!$omp firstprivate(BLOCK8X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T1,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
!$omp parallel do schedule(static,BLOCK8X) default(none)                &
!$omp firstprivate(BLOCK8X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T2,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
!$omp parallel do schedule(static,BLOCK8X) default(none)                &
!$omp firstprivate(BLOCK8X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_NTA,parg,pval)
#endif                  
                  do i=1,n,BLOCK8X
#if (__RCS_CYLINDER_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK8X-1,n)
                          arg0.v = parg(ii).v
                          call calck1_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do
             case default
                  return
             end select
             
       end subroutine calck1_blocked64x32x16x8x_omp_zmm8r8
       
       
       subroutine calck1_unroll12x_omp_zmm8r8(parg,pval,n,jint,PF_DIST)
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calck1_unroll12x_omp_zmm8r8
             !dir$ attributes forceinline :: calck1_unroll12x_omp_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calck1_unroll12x_omp_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: arg4
             !dir$ attributes align : 64 :: arg5
             !dir$ attributes align : 64 :: arg6
             !dir$ attributes align : 64 :: arg7
             !dir$ attributes align : 64 :: arg8
             !dir$ attributes align : 64 :: arg9
             !dir$ attributes align : 64 :: arg10
             !dir$ attributes align : 64 :: arg11
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             !dir$ attributes align : 64 :: val4
             !dir$ attributes align : 64 :: val5
             !dir$ attributes align : 64 :: val6
             !dir$ attributes align : 64 :: val7
             !dir$ attributes align : 64 :: val8
             !dir$ attributes align : 64 :: val9
             !dir$ attributes align : 64 :: val10
             !dir$ attributes align : 64 :: val11
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: arg4
             type(ZMM8r8_t), automatic :: arg5
             type(ZMM8r8_t), automatic :: arg6
             type(ZMM8r8_t), automatic :: arg7
             type(ZMM8r8_t), automatic :: arg8
             type(ZMM8r8_t), automatic :: arg9
             type(ZMM8r8_t), automatic :: arg10
             type(ZMM8r8_t), automatic :: arg11
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             type(ZMM8r8_t), automatic :: val4
             type(ZMM8r8_t), automatic :: val5
             type(ZMM8r8_t), automatic :: val6
             type(ZMM8r8_t), automatic :: val7
             type(ZMM8r8_t), automatic :: val8
             type(ZMM8r8_t), automatic :: val9
             type(ZMM8r8_t), automatic :: val10
             type(ZMM8r8_t), automatic :: val11
             integer(kind=i4) :: i,m,m1
             m = mod(n,12)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call calck1_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<12) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             val4.v = 0.0_dp
             val5.v = 0.0_dp
             val6.v = 0.0_dp
             val7.v = 0.0_dp
             val8.v = 0.0_dp
             val9.v = 0.0_dp
             val10.v= 0.0_dp
             val11.v= 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
             !dir$ vector aligned
             !dir$ ivdep
             !dir$ vector vectorlength(8)
             !dir$ vector multiple_gather_scatter_by_shuffles 
             !dir$ vector always
!$omp parallel do proc_bind(close) schedule(static) default(none) firstprivate(m1)       &
!$omp firstprivate(val0,val1,val2,val3,val4,val5,val6,val7)                              &
!$omp firstprivate(val8,val9,val10,val11) private(i,arg0,arg1,arg2,arg3,arg4,arg5,arg6)  &
!$omp private(arg7,arg8,arg9,arg10,arg11) shared(parg,pval,jint,n,PF_DIST)               &
!$omp private(FOR_K_PREFETCH_T0,FOR_K_PREFETCH_T1,FOR_K_PREFETCH_T2,FOR_K_PREFETCH_NTA)  
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
                    call calck1_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call calck1_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call calck1_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call calck1_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
                    arg4.v = parg(i+4).v
                    call calck1_zmm8r8(arg4,val4,jint)
                    pval(i+4).v = val4.v 
                    arg5.v = parg(i+5).v
                    call calck1_zmm8r8(arg5,val5,jint)
                    pval(i+5).v = val5.v  
                    arg6.v = parg(i+6).v
                    call calck1_zmm8r8(arg6,val6,jint)
                    pval(i+6).v = val6.v
                    arg7.v = parg(i+7).v
                    call calck1_zmm8r8(arg7,val7,jint)
                    pval(i+7).v = val7.v
                    arg8.v = parg(i+8).v
                    call calck1_zmm8r8(arg8,val8,jint)
                    pval(i+8).v = val8.v 
                    arg9.v = parg(i+9).v
                    call calck1_zmm8r8(arg9,val9,jint)
                    pval(i+9).v = val9.v  
                    arg10.v = parg(i+10).v
                    call calck1_zmm8r8(arg10,val10,jint)
                    pval(i+10).v = val10.v
                    arg11.v = parg(i+11).v
                    call calck1_zmm8r8(arg11,val11,jint)
                    pval(i+11).v = val11.v
             end do
!$omp end parallel do
       end subroutine calck1_unroll12x_omp_zmm8r8
       
       
       subroutine calck1_unroll8x_omp_zmm8r8(parg,pval,n,jint,PF_DIST)
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calck1_unroll8x_omp_zmm8r8
             !dir$ attributes forceinline :: calck1_unroll8x_omp_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calck1_unroll8x_omp_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: arg4
             !dir$ attributes align : 64 :: arg5
             !dir$ attributes align : 64 :: arg6
             !dir$ attributes align : 64 :: arg7
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             !dir$ attributes align : 64 :: val4
             !dir$ attributes align : 64 :: val5
             !dir$ attributes align : 64 :: val6
             !dir$ attributes align : 64 :: val7
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: arg4
             type(ZMM8r8_t), automatic :: arg5
             type(ZMM8r8_t), automatic :: arg6
             type(ZMM8r8_t), automatic :: arg7
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             type(ZMM8r8_t), automatic :: val4
             type(ZMM8r8_t), automatic :: val5
             type(ZMM8r8_t), automatic :: val6
             type(ZMM8r8_t), automatic :: val7
             integer(kind=i4) :: i,m,m1
             m = mod(n,8)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call calck1_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<8) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             val4.v = 0.0_dp
             val5.v = 0.0_dp
             val6.v = 0.0_dp
             val7.v = 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
             !dir$ vector aligned
             !dir$ ivdep
             !dir$ vector vectorlength(8)
             !dir$ vector multiple_gather_scatter_by_shuffles 
             !dir$ vector always
!$omp parallel do proc_bind(close) schedule(static) default(none) firstprivate(m1)       &
!$omp firstprivate(val0,val1,val2,val3,val4,val5,val6,val7)                              &
!$omp private(i,arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7)  &
!$omp shared(parg,pval,jint,n,PF_DIST)               &
!$omp private(FOR_K_PREFETCH_T0,FOR_K_PREFETCH_T1,FOR_K_PREFETCH_T2,FOR_K_PREFETCH_NTA)  
             do i=m1,n,8
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
                    call calck1_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call calck1_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call calck1_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call calck1_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
                    arg4.v = parg(i+4).v
                    call calck1_zmm8r8(arg4,val4,jint)
                    pval(i+4).v = val4.v 
                    arg5.v = parg(i+5).v
                    call calck1_zmm8r8(arg5,val5,jint)
                    pval(i+5).v = val5.v  
                    arg6.v = parg(i+6).v
                    call calck1_zmm8r8(arg6,val6,jint)
                    pval(i+6).v = val6.v
                    arg7.v = parg(i+7).v
                    call calck1_zmm8r8(arg7,val7,jint)
                    pval(i+7).v = val7.v
              end do
!$omp end parallel do
       end subroutine calck1_unroll8x_omp_zmm8r8
       
       
       subroutine calck1_unroll4x_omp_zmm8r8(parg,pval,n,jint,PF_DIST)
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: calck1_unroll4x_omp_zmm8r8
             !dir$ attributes forceinline :: calck1_unroll4x_omp_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: calck1_unroll4x_omp_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             integer(kind=i4) :: i,m,m1
             m = mod(n,4)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call calck1_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<4) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
             !dir$ vector aligned
             !dir$ ivdep
             !dir$ vector vectorlength(8)
             !dir$ vector multiple_gather_scatter_by_shuffles 
             !dir$ vector always
!$omp parallel do proc_bind(close) schedule(static) default(none) firstprivate(m1)       &
!$omp firstprivate(val0,val1,val2,val3)                              &
!$omp private(i,arg0,arg1,arg2,arg3)  &
!$omp shared(parg,pval,jint,n,PF_DIST)               &
!$omp private(FOR_K_PREFETCH_T0,FOR_K_PREFETCH_T1,FOR_K_PREFETCH_T2,FOR_K_PREFETCH_NTA)  
             do i=m1,n,4
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
                    call calck1_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call calck1_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call calck1_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call calck1_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
               end do
!$omp end parallel do
       end subroutine calck1_unroll4x_omp_zmm8r8
       
       
#if 0
    /*
    !*****************************************************************************80
!
!! CALJY0 computes various J0 and Y0 Bessel functions.
!
!  Discussion:
!
!    This routine computes zero-order Bessel functions of the first and
!    second kind (J0 and Y0), for real arguments X, where 0 < X <= XMAX
!    for Y0, and |X| <= XMAX for J0.
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
!    John Hart, Ward Cheney, Charles Lawson, Hans Maehly,
!    Charles Mesztenyi, John Rice, Henry Thatcher,
!    Christoph Witzgall,
!    Computer Approximations,
!    Wiley, 1968,
!    LC: QA297.C64.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) ARG, the argument.  If JINT = 0, ARG
!    must satisfy
!     -XMAX < ARG < XMAX;
!    If JINT = 1, then ARG must satisfy
!      0 < ARG < XMAX.
!
!    Output, real ( kind = 8 ) RESULT, the value of the function,
!    which depends on the input value of JINT:
!    0, RESULT = J0(x);
!    1, RESULT = Y0(x);
!
!    Input, integer ( kind = 4 ) JINT, chooses the function to be computed.
!    0, J0(x);
!    1, Y0(x);  
#endif

       
       subroutine caljy0_unroll12x_nonblock_zmm8r8(parg,pval,n,jint,PF_DIST)         
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: caljy0_unroll12x_nonblock_zmm8r8
             !dir$ attributes forceinline :: calcjy0_unroll12x_nonblock_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: caljy0_unroll12x_nonblock_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             integer(kind=i4) :: i,m,m1
             m = mod(n,12)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call caljy0_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<12) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
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
                    call caljy0_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call caljy0_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call caljy0_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call caljy0_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
                    arg0.v = parg(i+4).v
                    call caljy0_zmm8r8(arg0,val0,jint)
                    pval(i+4).v = val0.v 
                    arg1.v = parg(i+5).v
                    call caljy0_zmm8r8(arg1,val1,jint)
                    pval(i+5).v = val1.v  
                    arg2.v = parg(i+6).v
                    call caljy0_zmm8r8(arg2,val2,jint)
                    pval(i+6).v = val2.v
                    arg3.v = parg(i+7).v
                    call caljy0_zmm8r8(arg3,val3,jint)
                    pval(i+7).v = val3.v
                    arg0.v = parg(i+8).v
                    call caljy0_zmm8r8(arg0,val0,jint)
                    pval(i+8).v = val0.v 
                    arg1.v = parg(i+9).v
                    call caljy0_zmm8r8(arg1,val1,jint)
                    pval(i+9).v = val1.v  
                    arg2.v = parg(i+10).v
                    call caljy0_zmm8r8(arg2,val2,jint)
                    pval(i+10).v = val2.v
                    arg3.v = parg(i+11).v
                    call caljy0_zmm8r8(arg3,val3,jint)
                    pval(i+11).v = val3.v
             end do
       end subroutine caljy0_unroll12x_nonblock_zmm8r8
       
       
       subroutine caljy0_unroll8x_nonblock_zmm8r8(parg,pval,n,jint,PF_DIST)         
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: caljy0_unroll8x_nonblock_zmm8r8
             !dir$ attributes forceinline :: caljy0_unroll8x_nonblock_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: caljy0_unroll8x_nonblock_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             integer(kind=i4) :: i,m,m1
             m = mod(n,8)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call caljy0_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<8) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
             !dir$ vector aligned
             !dir$ ivdep
             !dir$ vector vectorlength(8)
             !dir$ vector multiple_gather_scatter_by_shuffles 
             !dir$ vector always
             do i=m1,n,8
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
                    call caljy0_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call caljy0_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call caljy0_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call caljy0_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
                    arg0.v = parg(i+4).v
                    call caljy0_zmm8r8(arg0,val0,jint)
                    pval(i+4).v = val0.v 
                    arg1.v = parg(i+5).v
                    call caljy0_zmm8r8(arg1,val1,jint)
                    pval(i+5).v = val1.v  
                    arg2.v = parg(i+6).v
                    call caljy0_zmm8r8(arg2,val2,jint)
                    pval(i+6).v = val2.v
                    arg3.v = parg(i+7).v
                    call caljy0_zmm8r8(arg3,val3,jint)
                    pval(i+7).v = val3.v
                end do
       end subroutine caljy0_unroll8x_nonblock_zmm8r8
       
       
       subroutine caljy0_unroll4x_nonblock_zmm8r8(parg,pval,n,jint,PF_DIST)         
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: caljy0_unroll4x_nonblock_zmm8r8
             !dir$ attributes forceinline :: caljy0_unroll4x_nonblock_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: caljy0_unroll4x_nonblock_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             integer(kind=i4) :: i,m,m1
             m = mod(n,4)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call caljy0_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<4) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
             !dir$ vector aligned
             !dir$ ivdep
             !dir$ vector vectorlength(8)
             !dir$ vector multiple_gather_scatter_by_shuffles 
             !dir$ vector always
             do i=m1,n,4
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
                    call caljy0_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call caljy0_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call caljy0_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call caljy0_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
                end do
       end subroutine caljy0_unroll4x_nonblock_zmm8r8
       
       
       subroutine caljy0_blocked64x32x16x8x_zmm8r8(parg,pval,n,jint,PF_DIST,which)         
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: caljy0_blocked64x32x16x8x_zmm8r8
             !dir$ attributes forceinline :: caljy0_blocked64x32x16x8x_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: caljy0_blocked64x32x16x8x_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             logical(kind=i4),               intent(in) :: which
             integer(kind=i4), parameter :: BLOCK64X = 64
             integer(kind=i4), parameter :: BLOCK32X = 32
             integer(kind=i4), parameter :: BLOCK16X = 16
             integer(kind=i4), parameter :: BLOCK8X  = 8 
             !dir$ attributes align : 64 :: arg0
             type(ZMM8r8_t), automatic :: arg0
             !dir$ attributes align : 64 :: arg0
             type(ZMM8r8_t), automatic :: val0
             integer(kind=i4) :: i,ii
             
            
             select case(which)
             case (0)
                  do i=1,n,BLOCK64X
#if (__RCS_CYLINDER_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK64X-1,n)
                          arg0.v = parg(ii).v
                          call caljy0_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do
             case (1)
                  do i=1,n,BLOCK32X
#if (__RCS_CYLINDER_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK32X-1,n)
                          arg0.v = parg(ii).v
                          call caljy0_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do 
             case (2)
                  do i=1,n,BLOCK16X
#if (__RCS_CYLINDER_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK16X-1,n)
                          arg0.v = parg(ii).v
                          call caljy0_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do 
             case (3)
                  do i=1,n,BLOCK8X
#if (__RCS_CYLINDER_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK8X-1,n)
                          arg0.v = parg(ii).v
                          call caljy0_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do
             case default
                  return
             end select
             
       end subroutine caljy0_blocked64x32x16x8x_zmm8r8
       
       
       subroutine caljy0_blocked64x32x16x8x_omp_zmm8r8(parg,pval,n,jint,PF_DIST,which)         
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: caljy0_blocked64x32x16x8x_omp_zmm8r8
             !dir$ attributes forceinline :: caljy0_blocked64x32x16x8x_omp_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: caljy0_blocked64x32x16x8x_omp_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             logical(kind=i4),               intent(in) :: which
             integer(kind=i4), parameter :: BLOCK64X = 64
             integer(kind=i4), parameter :: BLOCK32X = 32
             integer(kind=i4), parameter :: BLOCK16X = 16
             integer(kind=i4), parameter :: BLOCK8X  = 8 
             !dir$ attributes align : 64 :: arg0
             type(ZMM8r8_t), automatic :: arg0
             !dir$ attributes align : 64 :: arg0
             type(ZMM8r8_t), automatic :: val0
             integer(kind=i4) :: i,ii
             
            
             select case(which)
             case (0)
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
!$omp parallel do schedule(static,BLOCK64X) default(none)                &
!$omp firstprivate(BLOCK64X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T0,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2 
!$omp parallel do schedule(static,BLOCK64X) default(none)                &
!$omp firstprivate(BLOCK64X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T1,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
!$omp parallel do schedule(static,BLOCK64X) default(none)                &
!$omp firstprivate(BLOCK64X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T2,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
!$omp parallel do schedule(static,BLOCK64X) default(none)                &
!$omp firstprivate(BLOCK64X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_NTA,parg,pval)
#endif
                  do i=1,n,BLOCK64X

#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif   
         
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK64X-1,n)
                          arg0.v = parg(ii).v
                          call caljy0_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do
             case (1)
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
!$omp parallel do schedule(static,BLOCK32X) default(none)                &
!$omp firstprivate(BLOCK32X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T0,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2 
!$omp parallel do schedule(static,BLOCK32X) default(none)                &
!$omp firstprivate(BLOCK32X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T1,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
!$omp parallel do schedule(static,BLOCK32X) default(none)                &
!$omp firstprivate(BLOCK32X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T2,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
!$omp parallel do schedule(static,BLOCK32X) default(none)                &
!$omp firstprivate(BLOCK32X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_NTA,parg,pval)
#endif             
                  do i=1,n,BLOCK32X
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK32X-1,n)
                          arg0.v = parg(ii).v
                          call caljy0_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do 
             case (2)
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
!$omp parallel do schedule(static,BLOCK16X) default(none)                &
!$omp firstprivate(BLOCK16X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T0,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2 
!$omp parallel do schedule(static,BLOCK16X) default(none)                &
!$omp firstprivate(BLOCK16X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T1,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
!$omp parallel do schedule(static,BLOCK16X) default(none)                &
!$omp firstprivate(BLOCK16X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T2,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
!$omp parallel do schedule(static,BLOCK16X) default(none)                &
!$omp firstprivate(BLOCK16X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_NTA,parg,pval)
#endif                
                  do i=1,n,BLOCK16X
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK16X-1,n)
                          arg0.v = parg(ii).v
                          call caljy0_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do 
             case (3)
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
!$omp parallel do schedule(static,BLOCK8X) default(none)                &
!$omp firstprivate(BLOCK8X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T0,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2 
!$omp parallel do schedule(static,BLOCK8X) default(none)                &
!$omp firstprivate(BLOCK8X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T1,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
!$omp parallel do schedule(static,BLOCK8X) default(none)                &
!$omp firstprivate(BLOCK8X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T2,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
!$omp parallel do schedule(static,BLOCK8X) default(none)                &
!$omp firstprivate(BLOCK8X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_NTA,parg,pval)
#endif                  
                  do i=1,n,BLOCK8X
#if (__RCS_CYLINDER_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK8X-1,n)
                          arg0.v = parg(ii).v
                          call caljy0_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do
             case default
                  return
             end select
             
       end subroutine caljy0_blocked64x32x16x8x_omp_zmm8r8
       
       
       subroutine caljy0_unroll12x_omp_zmm8r8(parg,pval,n,jint,PF_DIST)
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: caljy0_unroll12x_omp_zmm8r8
             !dir$ attributes forceinline :: caljy0_unroll12x_omp_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: caljy0_unroll12x_omp_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: arg4
             !dir$ attributes align : 64 :: arg5
             !dir$ attributes align : 64 :: arg6
             !dir$ attributes align : 64 :: arg7
             !dir$ attributes align : 64 :: arg8
             !dir$ attributes align : 64 :: arg9
             !dir$ attributes align : 64 :: arg10
             !dir$ attributes align : 64 :: arg11
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             !dir$ attributes align : 64 :: val4
             !dir$ attributes align : 64 :: val5
             !dir$ attributes align : 64 :: val6
             !dir$ attributes align : 64 :: val7
             !dir$ attributes align : 64 :: val8
             !dir$ attributes align : 64 :: val9
             !dir$ attributes align : 64 :: val10
             !dir$ attributes align : 64 :: val11
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: arg4
             type(ZMM8r8_t), automatic :: arg5
             type(ZMM8r8_t), automatic :: arg6
             type(ZMM8r8_t), automatic :: arg7
             type(ZMM8r8_t), automatic :: arg8
             type(ZMM8r8_t), automatic :: arg9
             type(ZMM8r8_t), automatic :: arg10
             type(ZMM8r8_t), automatic :: arg11
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             type(ZMM8r8_t), automatic :: val4
             type(ZMM8r8_t), automatic :: val5
             type(ZMM8r8_t), automatic :: val6
             type(ZMM8r8_t), automatic :: val7
             type(ZMM8r8_t), automatic :: val8
             type(ZMM8r8_t), automatic :: val9
             type(ZMM8r8_t), automatic :: val10
             type(ZMM8r8_t), automatic :: val11
             integer(kind=i4) :: i,m,m1
             m = mod(n,12)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call caljy0_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<12) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             val4.v = 0.0_dp
             val5.v = 0.0_dp
             val6.v = 0.0_dp
             val7.v = 0.0_dp
             val8.v = 0.0_dp
             val9.v = 0.0_dp
             val10.v= 0.0_dp
             val11.v= 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
             !dir$ vector aligned
             !dir$ ivdep
             !dir$ vector vectorlength(8)
             !dir$ vector multiple_gather_scatter_by_shuffles 
             !dir$ vector always
!$omp parallel do proc_bind(close) schedule(static) default(none) firstprivate(m1)       &
!$omp firstprivate(val0,val1,val2,val3,val4,val5,val6,val7)                              &
!$omp firstprivate(val8,val9,val10,val11) private(i,arg0,arg1,arg2,arg3,arg4,arg5,arg6)  &
!$omp private(arg7,arg8,arg9,arg10,arg11) shared(parg,pval,jint,n,PF_DIST)               &
!$omp private(FOR_K_PREFETCH_T0,FOR_K_PREFETCH_T1,FOR_K_PREFETCH_T2,FOR_K_PREFETCH_NTA)  
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
                    call caljy0_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call caljy0_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call caljy0_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call caljy0_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
                    arg4.v = parg(i+4).v
                    call caljy0_zmm8r8(arg4,val4,jint)
                    pval(i+4).v = val4.v 
                    arg5.v = parg(i+5).v
                    call caljy0_zmm8r8(arg5,val5,jint)
                    pval(i+5).v = val5.v  
                    arg6.v = parg(i+6).v
                    call caljy0_zmm8r8(arg6,val6,jint)
                    pval(i+6).v = val6.v
                    arg7.v = parg(i+7).v
                    call caljy0_zmm8r8(arg7,val7,jint)
                    pval(i+7).v = val7.v
                    arg8.v = parg(i+8).v
                    call caljy0_zmm8r8(arg8,val8,jint)
                    pval(i+8).v = val8.v 
                    arg9.v = parg(i+9).v
                    call caljy0_zmm8r8(arg9,val9,jint)
                    pval(i+9).v = val9.v  
                    arg10.v = parg(i+10).v
                    call caljy0_zmm8r8(arg10,val10,jint)
                    pval(i+10).v = val10.v
                    arg11.v = parg(i+11).v
                    call caljy0_zmm8r8(arg11,val11,jint)
                    pval(i+11).v = val11.v
             end do
!$omp end parallel do
       end subroutine caljy0_unroll12x_omp_zmm8r8
       
       
       subroutine caljy0_unroll8x_omp_zmm8r8(parg,pval,n,jint,PF_DIST)
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: caljy0_unroll8x_omp_zmm8r8
             !dir$ attributes forceinline :: caljy0_unroll8x_omp_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: caljy0_unroll8x_omp_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: arg4
             !dir$ attributes align : 64 :: arg5
             !dir$ attributes align : 64 :: arg6
             !dir$ attributes align : 64 :: arg7
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             !dir$ attributes align : 64 :: val4
             !dir$ attributes align : 64 :: val5
             !dir$ attributes align : 64 :: val6
             !dir$ attributes align : 64 :: val7
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: arg4
             type(ZMM8r8_t), automatic :: arg5
             type(ZMM8r8_t), automatic :: arg6
             type(ZMM8r8_t), automatic :: arg7
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             type(ZMM8r8_t), automatic :: val4
             type(ZMM8r8_t), automatic :: val5
             type(ZMM8r8_t), automatic :: val6
             type(ZMM8r8_t), automatic :: val7
             integer(kind=i4) :: i,m,m1
             m = mod(n,8)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call caljy0_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<8) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             val4.v = 0.0_dp
             val5.v = 0.0_dp
             val6.v = 0.0_dp
             val7.v = 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
             !dir$ vector aligned
             !dir$ ivdep
             !dir$ vector vectorlength(8)
             !dir$ vector multiple_gather_scatter_by_shuffles 
             !dir$ vector always
!$omp parallel do proc_bind(close) schedule(static) default(none) firstprivate(m1)       &
!$omp firstprivate(val0,val1,val2,val3,val4,val5,val6,val7)                              &
!$omp private(i,arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7)  &
!$omp shared(parg,pval,jint,n,PF_DIST)               &
!$omp private(FOR_K_PREFETCH_T0,FOR_K_PREFETCH_T1,FOR_K_PREFETCH_T2,FOR_K_PREFETCH_NTA)  
             do i=m1,n,8
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
                    call caljy0_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call caljy0_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call caljy0_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call caljy0_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
                    arg4.v = parg(i+4).v
                    call caljy0_zmm8r8(arg4,val4,jint)
                    pval(i+4).v = val4.v 
                    arg5.v = parg(i+5).v
                    call caljy0_zmm8r8(arg5,val5,jint)
                    pval(i+5).v = val5.v  
                    arg6.v = parg(i+6).v
                    call caljy0_zmm8r8(arg6,val6,jint)
                    pval(i+6).v = val6.v
                    arg7.v = parg(i+7).v
                    call caljy0_zmm8r8(arg7,val7,jint)
                    pval(i+7).v = val7.v
              end do
!$omp end parallel do
       end subroutine caljy0_unroll8x_omp_zmm8r8
       
       
       subroutine caljy0_unroll4x_omp_zmm8r8(parg,pval,n,jint,PF_DIST)
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: caljy0_unroll4x_omp_zmm8r8
             !dir$ attributes forceinline :: caljy0_unroll4x_omp_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: caljy0_unroll4x_omp_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             integer(kind=i4) :: i,m,m1
             m = mod(n,4)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call caljy0_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<4) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
             !dir$ vector aligned
             !dir$ ivdep
             !dir$ vector vectorlength(8)
             !dir$ vector multiple_gather_scatter_by_shuffles 
             !dir$ vector always
!$omp parallel do proc_bind(close) schedule(static) default(none) firstprivate(m1)       &
!$omp firstprivate(val0,val1,val2,val3)                              &
!$omp private(i,arg0,arg1,arg2,arg3)  &
!$omp shared(parg,pval,jint,n,PF_DIST)               &
!$omp private(FOR_K_PREFETCH_T0,FOR_K_PREFETCH_T1,FOR_K_PREFETCH_T2,FOR_K_PREFETCH_NTA)  
             do i=m1,n,4
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
                    call caljy0_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call caljy0_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call caljy0_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call caljy0_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
               end do
!$omp end parallel do
       end subroutine caljy0_unroll4x_omp_zmm8r8
       

#if 0
      /*
	             !*****************************************************************************80
!
!! CALJY1 computes various J1 and Y1 Bessel functions.
!
!  Discussion:
!
!    This routine computes first-order Bessel functions of the first and
!    second kind (J1 and Y1), for real arguments X, where 0 < X <= XMAX
!    for Y1, and |X| <= XMAX for J1.
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
!    John Hart, Ward Cheney, Charles Lawson, Hans Maehly,
!    Charles Mesztenyi, John Rice, Henry Thatcher,
!    Christoph Witzgall,
!    Computer Approximations,
!    Wiley, 1968,
!    LC: QA297.C64.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) ARG, the argument.  If JINT = 0, ARG
!    must satisfy
!     -XMAX < ARG < XMAX;
!    If JINT = 1, then ARG must satisfy
!      0 < ARG < XMAX.
!
!    Output, real ( kind = 8 ) RESULT, the value of the function,
!    which depends on the input value of JINT:
!    0, RESULT = J1(x);
!    1, RESULT = Y1(x);
!
!    Input, integer ( kind = 4 ) JINT, chooses the function to be computed.
!    0, J1(x);
!    1, Y1(x);  
#endif      


        subroutine caljy1_unroll12x_nonblock_zmm8r8(parg,pval,n,jint,PF_DIST)         
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: caljy1_unroll12x_nonblock_zmm8r8
             !dir$ attributes forceinline :: calcjy1_unroll12x_nonblock_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: caljy1_unroll12x_nonblock_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             integer(kind=i4) :: i,m,m1
             m = mod(n,12)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call caljy1_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<12) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
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
                    call caljy1_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call caljy1_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call caljy1_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call caljy1_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
                    arg0.v = parg(i+4).v
                    call caljy1_zmm8r8(arg0,val0,jint)
                    pval(i+4).v = val0.v 
                    arg1.v = parg(i+5).v
                    call caljy1_zmm8r8(arg1,val1,jint)
                    pval(i+5).v = val1.v  
                    arg2.v = parg(i+6).v
                    call caljy1_zmm8r8(arg2,val2,jint)
                    pval(i+6).v = val2.v
                    arg3.v = parg(i+7).v
                    call caljy1_zmm8r8(arg3,val3,jint)
                    pval(i+7).v = val3.v
                    arg0.v = parg(i+8).v
                    call caljy1_zmm8r8(arg0,val0,jint)
                    pval(i+8).v = val0.v 
                    arg1.v = parg(i+9).v
                    call caljy1_zmm8r8(arg1,val1,jint)
                    pval(i+9).v = val1.v  
                    arg2.v = parg(i+10).v
                    call caljy1_zmm8r8(arg2,val2,jint)
                    pval(i+10).v = val2.v
                    arg3.v = parg(i+11).v
                    call caljy1_zmm8r8(arg3,val3,jint)
                    pval(i+11).v = val3.v
             end do
       end subroutine caljy1_unroll12x_nonblock_zmm8r8  
       
       
       subroutine caljy1_unroll8x_nonblock_zmm8r8(parg,pval,n,jint,PF_DIST)         
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: caljy1_unroll8x_nonblock_zmm8r8
             !dir$ attributes forceinline :: caljy1_unroll8x_nonblock_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: caljy1_unroll8x_nonblock_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             integer(kind=i4) :: i,m,m1
             m = mod(n,8)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call caljy1_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<8) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
             !dir$ vector aligned
             !dir$ ivdep
             !dir$ vector vectorlength(8)
             !dir$ vector multiple_gather_scatter_by_shuffles 
             !dir$ vector always
             do i=m1,n,8
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
                    call caljy1_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call caljy1_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call caljy1_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call caljy1_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
                    arg0.v = parg(i+4).v
                    call caljy1_zmm8r8(arg0,val0,jint)
                    pval(i+4).v = val0.v 
                    arg1.v = parg(i+5).v
                    call caljy1_zmm8r8(arg1,val1,jint)
                    pval(i+5).v = val1.v  
                    arg2.v = parg(i+6).v
                    call caljy1_zmm8r8(arg2,val2,jint)
                    pval(i+6).v = val2.v
                    arg3.v = parg(i+7).v
                    call caljy1_zmm8r8(arg3,val3,jint)
                    pval(i+7).v = val3.v
                end do
       end subroutine caljy1_unroll8x_nonblock_zmm8r8
       
       
       subroutine caljy1_unroll4x_nonblock_zmm8r8(parg,pval,n,jint,PF_DIST)         
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: caljy1_unroll4x_nonblock_zmm8r8
             !dir$ attributes forceinline :: caljy1_unroll4x_nonblock_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: caljy1_unroll4x_nonblock_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             integer(kind=i4) :: i,m,m1
             m = mod(n,4)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call caljy1_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<4) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
             !dir$ vector aligned
             !dir$ ivdep
             !dir$ vector vectorlength(8)
             !dir$ vector multiple_gather_scatter_by_shuffles 
             !dir$ vector always
             do i=m1,n,4
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
                    call caljy1_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call caljy1_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call caljy1_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call caljy1_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
                end do
       end subroutine caljy1_unroll4x_nonblock_zmm8r8
       
       
       subroutine caljy1_blocked64x32x16x8x_zmm8r8(parg,pval,n,jint,PF_DIST,which)         
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: caljy1_blocked64x32x16x8x_zmm8r8
             !dir$ attributes forceinline :: caljy1_blocked64x32x16x8x_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: caljy1_blocked64x32x16x8x_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             logical(kind=i4),               intent(in) :: which
             integer(kind=i4), parameter :: BLOCK64X = 64
             integer(kind=i4), parameter :: BLOCK32X = 32
             integer(kind=i4), parameter :: BLOCK16X = 16
             integer(kind=i4), parameter :: BLOCK8X  = 8 
             !dir$ attributes align : 64 :: arg0
             type(ZMM8r8_t), automatic :: arg0
             !dir$ attributes align : 64 :: arg0
             type(ZMM8r8_t), automatic :: val0
             integer(kind=i4) :: i,ii
             
            
             select case(which)
             case (0)
                  do i=1,n,BLOCK64X
#if (__RCS_CYLINDER_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK64X-1,n)
                          arg0.v = parg(ii).v
                          call caljy1_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do
             case (1)
                  do i=1,n,BLOCK32X
#if (__RCS_CYLINDER_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK32X-1,n)
                          arg0.v = parg(ii).v
                          call caljy1_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do 
             case (2)
                  do i=1,n,BLOCK16X
#if (__RCS_CYLINDER_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK16X-1,n)
                          arg0.v = parg(ii).v
                          call caljy1_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do 
             case (3)
                  do i=1,n,BLOCK8X
#if (__RCS_CYLINDER_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK8X-1,n)
                          arg0.v = parg(ii).v
                          call caljy1_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do
             case default
                  return
             end select
             
       end subroutine caljy1_blocked64x32x16x8x_zmm8r8
       
       
       subroutine caljy1_blocked64x32x16x8x_omp_zmm8r8(parg,pval,n,jint,PF_DIST,which)         
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: caljy1_blocked64x32x16x8x_omp_zmm8r8
             !dir$ attributes forceinline :: caljy1_blocked64x32x16x8x_omp_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: caljy1_blocked64x32x16x8x_omp_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             logical(kind=i4),               intent(in) :: which
             integer(kind=i4), parameter :: BLOCK64X = 64
             integer(kind=i4), parameter :: BLOCK32X = 32
             integer(kind=i4), parameter :: BLOCK16X = 16
             integer(kind=i4), parameter :: BLOCK8X  = 8 
             !dir$ attributes align : 64 :: arg0
             type(ZMM8r8_t), automatic :: arg0
             !dir$ attributes align : 64 :: arg0
             type(ZMM8r8_t), automatic :: val0
             integer(kind=i4) :: i,ii
             
            
             select case(which)
             case (0)
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
!$omp parallel do schedule(static,BLOCK64X) default(none)                &
!$omp firstprivate(BLOCK64X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T0,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2 
!$omp parallel do schedule(static,BLOCK64X) default(none)                &
!$omp firstprivate(BLOCK64X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T1,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
!$omp parallel do schedule(static,BLOCK64X) default(none)                &
!$omp firstprivate(BLOCK64X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T2,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
!$omp parallel do schedule(static,BLOCK64X) default(none)                &
!$omp firstprivate(BLOCK64X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_NTA,parg,pval)
#endif
                  do i=1,n,BLOCK64X

#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif   
         
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK64X-1,n)
                          arg0.v = parg(ii).v
                          call caljy1_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do
             case (1)
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
!$omp parallel do schedule(static,BLOCK32X) default(none)                &
!$omp firstprivate(BLOCK32X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T0,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2 
!$omp parallel do schedule(static,BLOCK32X) default(none)                &
!$omp firstprivate(BLOCK32X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T1,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
!$omp parallel do schedule(static,BLOCK32X) default(none)                &
!$omp firstprivate(BLOCK32X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T2,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
!$omp parallel do schedule(static,BLOCK32X) default(none)                &
!$omp firstprivate(BLOCK32X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_NTA,parg,pval)
#endif             
                  do i=1,n,BLOCK32X
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK32X-1,n)
                          arg0.v = parg(ii).v
                          call caljy1_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do 
             case (2)
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
!$omp parallel do schedule(static,BLOCK16X) default(none)                &
!$omp firstprivate(BLOCK16X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T0,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2 
!$omp parallel do schedule(static,BLOCK16X) default(none)                &
!$omp firstprivate(BLOCK16X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T1,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
!$omp parallel do schedule(static,BLOCK16X) default(none)                &
!$omp firstprivate(BLOCK16X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T2,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
!$omp parallel do schedule(static,BLOCK16X) default(none)                &
!$omp firstprivate(BLOCK16X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_NTA,parg,pval)
#endif                
                  do i=1,n,BLOCK16X
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK16X-1,n)
                          arg0.v = parg(ii).v
                          call caljy1_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do 
             case (3)
#if (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 1
!$omp parallel do schedule(static,BLOCK8X) default(none)                &
!$omp firstprivate(BLOCK8X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T0,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 2 
!$omp parallel do schedule(static,BLOCK8X) default(none)                &
!$omp firstprivate(BLOCK8X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T1,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 3
!$omp parallel do schedule(static,BLOCK8X) default(none)                &
!$omp firstprivate(BLOCK8X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_T2,parg,pval)
#elif (__SPECFUNC_ARRAYS_ZMM8R8_PF_CACHE_HINT__) == 4
!$omp parallel do schedule(static,BLOCK8X) default(none)                &
!$omp firstprivate(BLOCK8X) private(i,ii,arg0,val0)                     &
!$omp shared(n,PF_DIST,jint,FOR_K_PREFETCH_NTA,parg,pval)
#endif                  
                  do i=1,n,BLOCK8X
#if (__RCS_CYLINDER_PF_CACHE_HINT__) == 1
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T0,.true.,.false.)
#elif (__RCS_CYLINDER_PF_CACHE_HINT__) == 2   
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T1,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 3
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_T2,.true.,.false.)
#elif (_RCS_CYLINDER_PF_CACHE_HINT__) == 4
                         call mm_prefetch(parg(i+PF_DIST),FOR_K_PREFETCH_NTA,.true.,.false.)
#endif             
                        !dir$ assume_aligned parg:64
                        !dir$ assume_aligned pval:64
                        !dir$ vector aligned
                        !dir$ ivdep
                        !dir$ vector vectorlength(8)
                        !dir$ vector multiple_gather_scatter_by_shuffles 
                        !dir$ vector always
                       do ii=i,min(i+BLOCK8X-1,n)
                          arg0.v = parg(ii).v
                          call caljy1_zmm8r8(arg0,val0,jint)
                          pval(ii).v = val0.v 
                       end do
                  end do
             case default
                  return
             end select
             
       end subroutine caljy1_blocked64x32x16x8x_omp_zmm8r8
       
       
       subroutine caljy1_unroll12x_omp_zmm8r8(parg,pval,n,jint,PF_DIST)
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: caljy1_unroll12x_omp_zmm8r8
             !dir$ attributes forceinline :: caljy1_unroll12x_omp_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: caljy1_unroll12x_omp_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: arg4
             !dir$ attributes align : 64 :: arg5
             !dir$ attributes align : 64 :: arg6
             !dir$ attributes align : 64 :: arg7
             !dir$ attributes align : 64 :: arg8
             !dir$ attributes align : 64 :: arg9
             !dir$ attributes align : 64 :: arg10
             !dir$ attributes align : 64 :: arg11
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             !dir$ attributes align : 64 :: val4
             !dir$ attributes align : 64 :: val5
             !dir$ attributes align : 64 :: val6
             !dir$ attributes align : 64 :: val7
             !dir$ attributes align : 64 :: val8
             !dir$ attributes align : 64 :: val9
             !dir$ attributes align : 64 :: val10
             !dir$ attributes align : 64 :: val11
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: arg4
             type(ZMM8r8_t), automatic :: arg5
             type(ZMM8r8_t), automatic :: arg6
             type(ZMM8r8_t), automatic :: arg7
             type(ZMM8r8_t), automatic :: arg8
             type(ZMM8r8_t), automatic :: arg9
             type(ZMM8r8_t), automatic :: arg10
             type(ZMM8r8_t), automatic :: arg11
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             type(ZMM8r8_t), automatic :: val4
             type(ZMM8r8_t), automatic :: val5
             type(ZMM8r8_t), automatic :: val6
             type(ZMM8r8_t), automatic :: val7
             type(ZMM8r8_t), automatic :: val8
             type(ZMM8r8_t), automatic :: val9
             type(ZMM8r8_t), automatic :: val10
             type(ZMM8r8_t), automatic :: val11
             integer(kind=i4) :: i,m,m1
             m = mod(n,12)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call caljy1_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<12) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             val4.v = 0.0_dp
             val5.v = 0.0_dp
             val6.v = 0.0_dp
             val7.v = 0.0_dp
             val8.v = 0.0_dp
             val9.v = 0.0_dp
             val10.v= 0.0_dp
             val11.v= 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
             !dir$ vector aligned
             !dir$ ivdep
             !dir$ vector vectorlength(8)
             !dir$ vector multiple_gather_scatter_by_shuffles 
             !dir$ vector always
!$omp parallel do proc_bind(close) schedule(static) default(none) firstprivate(m1)       &
!$omp firstprivate(val0,val1,val2,val3,val4,val5,val6,val7)                              &
!$omp firstprivate(val8,val9,val10,val11) private(i,arg0,arg1,arg2,arg3,arg4,arg5,arg6)  &
!$omp private(arg7,arg8,arg9,arg10,arg11) shared(parg,pval,jint,n,PF_DIST)               &
!$omp private(FOR_K_PREFETCH_T0,FOR_K_PREFETCH_T1,FOR_K_PREFETCH_T2,FOR_K_PREFETCH_NTA)  
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
                    call caljy1_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call caljy1_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call caljy1_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call caljy1_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
                    arg4.v = parg(i+4).v
                    call caljy1_zmm8r8(arg4,val4,jint)
                    pval(i+4).v = val4.v 
                    arg5.v = parg(i+5).v
                    call caljy1_zmm8r8(arg5,val5,jint)
                    pval(i+5).v = val5.v  
                    arg6.v = parg(i+6).v
                    call caljy1_zmm8r8(arg6,val6,jint)
                    pval(i+6).v = val6.v
                    arg7.v = parg(i+7).v
                    call caljy1_zmm8r8(arg7,val7,jint)
                    pval(i+7).v = val7.v
                    arg8.v = parg(i+8).v
                    call caljy1_zmm8r8(arg8,val8,jint)
                    pval(i+8).v = val8.v 
                    arg9.v = parg(i+9).v
                    call caljy1_zmm8r8(arg9,val9,jint)
                    pval(i+9).v = val9.v  
                    arg10.v = parg(i+10).v
                    call caljy1_zmm8r8(arg10,val10,jint)
                    pval(i+10).v = val10.v
                    arg11.v = parg(i+11).v
                    call caljy1_zmm8r8(arg11,val11,jint)
                    pval(i+11).v = val11.v
             end do
!$omp end parallel do
       end subroutine caljy1_unroll12x_omp_zmm8r8
       
       
       subroutine caljy1_unroll8x_omp_zmm8r8(parg,pval,n,jint,PF_DIST)
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: caljy1_unroll8x_omp_zmm8r8
             !dir$ attributes forceinline :: caljy1_unroll8x_omp_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: caljy1_unroll8x_omp_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: arg4
             !dir$ attributes align : 64 :: arg5
             !dir$ attributes align : 64 :: arg6
             !dir$ attributes align : 64 :: arg7
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             !dir$ attributes align : 64 :: val4
             !dir$ attributes align : 64 :: val5
             !dir$ attributes align : 64 :: val6
             !dir$ attributes align : 64 :: val7
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: arg4
             type(ZMM8r8_t), automatic :: arg5
             type(ZMM8r8_t), automatic :: arg6
             type(ZMM8r8_t), automatic :: arg7
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             type(ZMM8r8_t), automatic :: val4
             type(ZMM8r8_t), automatic :: val5
             type(ZMM8r8_t), automatic :: val6
             type(ZMM8r8_t), automatic :: val7
             integer(kind=i4) :: i,m,m1
             m = mod(n,8)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call caljy1_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<8) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             val4.v = 0.0_dp
             val5.v = 0.0_dp
             val6.v = 0.0_dp
             val7.v = 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
             !dir$ vector aligned
             !dir$ ivdep
             !dir$ vector vectorlength(8)
             !dir$ vector multiple_gather_scatter_by_shuffles 
             !dir$ vector always
!$omp parallel do proc_bind(close) schedule(static) default(none) firstprivate(m1)       &
!$omp firstprivate(val0,val1,val2,val3,val4,val5,val6,val7)                              &
!$omp private(i,arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7)  &
!$omp shared(parg,pval,jint,n,PF_DIST)               &
!$omp private(FOR_K_PREFETCH_T0,FOR_K_PREFETCH_T1,FOR_K_PREFETCH_T2,FOR_K_PREFETCH_NTA)  
             do i=m1,n,8
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
                    call caljy1_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call caljy1_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call caljy1_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call caljy1_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
                    arg4.v = parg(i+4).v
                    call caljy1_zmm8r8(arg4,val4,jint)
                    pval(i+4).v = val4.v 
                    arg5.v = parg(i+5).v
                    call caljy1_zmm8r8(arg5,val5,jint)
                    pval(i+5).v = val5.v  
                    arg6.v = parg(i+6).v
                    call caljy1_zmm8r8(arg6,val6,jint)
                    pval(i+6).v = val6.v
                    arg7.v = parg(i+7).v
                    call caljy1_zmm8r8(arg7,val7,jint)
                    pval(i+7).v = val7.v
              end do
!$omp end parallel do
       end subroutine caljy1_unroll8x_omp_zmm8r8
       
       
       subroutine caljy1_unroll4x_omp_zmm8r8(parg,pval,n,jint,PF_DIST)
             !dir$ optimize:3
             !dir$ attributes code_align : 32 :: caljy1_unroll4x_omp_zmm8r8
             !dir$ attributes forceinline :: caljy1_unroll4x_omp_zmm8r8
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: caljy1_unroll4x_omp_zmm8r8
             type(ZMM8r8_t), dimension(1:n), intent(in) :: parg
             type(ZMM8r8_t), dimension(1:n), intent(out):: pval
             integer(kind=i4),               intent(in) :: n
             integer(kind=i4),               intent(in) :: jint
             integer(kind=i4),               intent(in) :: PF_DIST
             !dir$ attributes align : 64 :: arg0
             !dir$ attributes align : 64 :: arg1
             !dir$ attributes align : 64 :: arg2
             !dir$ attributes align : 64 :: arg3
             !dir$ attributes align : 64 :: val0
             !dir$ attributes align : 64 :: val1
             !dir$ attributes align : 64 :: val2
             !dir$ attributes align : 64 :: val3
             type(ZMM8r8_t), automatic :: arg0
             type(ZMM8r8_t), automatic :: arg1
             type(ZMM8r8_t), automatic :: arg2
             type(ZMM8r8_t), automatic :: arg3
             type(ZMM8r8_t), automatic :: val0
             type(ZMM8r8_t), automatic :: val1
             type(ZMM8r8_t), automatic :: val2
             type(ZMM8r8_t), automatic :: val3
             integer(kind=i4) :: i,m,m1
             m = mod(n,4)
             if(m/=0) then
                val0.v = 0.0_dp
                do i=1,m
                    arg0.v = parg(i).v
                    call caljy1_zmm8r8(arg0,val0,jint)
                    pval(i).v = val0.v
                end do
                if(n<4) return
             end if
             m1=m+1
             val0.v = 0.0_dp
             val1.v = 0.0_dp
             val2.v = 0.0_dp
             val3.v = 0.0_dp
             !dir$ assume_aligned parg:64
             !dir$ assume_aligned pval:64
             !dir$ vector aligned
             !dir$ ivdep
             !dir$ vector vectorlength(8)
             !dir$ vector multiple_gather_scatter_by_shuffles 
             !dir$ vector always
!$omp parallel do proc_bind(close) schedule(static) default(none) firstprivate(m1)       &
!$omp firstprivate(val0,val1,val2,val3)                              &
!$omp private(i,arg0,arg1,arg2,arg3)  &
!$omp shared(parg,pval,jint,n,PF_DIST)               &
!$omp private(FOR_K_PREFETCH_T0,FOR_K_PREFETCH_T1,FOR_K_PREFETCH_T2,FOR_K_PREFETCH_NTA)  
             do i=m1,n,4
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
                    call caljy1_zmm8r8(arg0,val0,jint)
                    pval(i+0).v = val0.v 
                    arg1.v = parg(i+1).v
                    call caljy1_zmm8r8(arg1,val1,jint)
                    pval(i+1).v = val1.v  
                    arg2.v = parg(i+2).v
                    call caljy1_zmm8r8(arg2,val2,jint)
                    pval(i+2).v = val2.v
                    arg3.v = parg(i+3).v
                    call caljy1_zmm8r8(arg3,val3,jint)
                    pval(i+3).v = val3.v
               end do
!$omp end parallel do
       end subroutine caljy1_unroll4x_omp_zmm8r8
       
       


         
       
       
       
       
       
       
       
       
       
       
       
       
       
    


end module specfuncs_arrays_zmm8r8
