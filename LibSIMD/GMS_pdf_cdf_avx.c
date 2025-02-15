

/*MIT License
Copyright (c) 2020 Bernard Gingold
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

#include <float.h>
#include "GMS_pdf_cdf_avx.h"
#include "GMS_LUT_cdf_pdf_avx.h"
#include "GMS_simd_utils.h"

#if !defined(__AVX512F__) || !defined(__AVX512VL__)
#error "Support of AVX512F or AVX512VL ISA required!!"
#endif
 
/*
      !*****************************************************************************80
!
!! GAMMA_LOG calculates the natural logarithm of GAMMA ( X ).
!
!  Discussion:
!
!    Computation is based on an algorithm outlined in references 1 and 2.
!    The program uses rational functions that theoretically approximate
!    LOG(GAMMA(X)) to at least 18 significant decimal digits.  The
!    approximation for 12 < X is from Hart et al, while approximations
!    for X < 12.0D+00 are similar to those in Cody and Hillstrom,
!    but are unpublished.
!
!    The accuracy achieved depends on the arithmetic system, the compiler,
!    intrinsic functions, and proper selection of the machine dependent
!    constants.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 June 1999
!
!  Author:
!
!    Original FORTRAN77 version by William Cody, Laura Stoltz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    William Cody, Kenneth Hillstrom,
!    Chebyshev Approximations for the Natural Logarithm of the Gamma Function,
!    Mathematics of Computation,
!    Volume 21, 1967, pages 198-203.
!
!    Kenneth Hillstrom,
!    ANL/AMD Program ANLC366S, DGAMMA/DLGAMA,
!    May 1969.
!
!    John Hart, Ward Cheney, Charles Lawson, Hans Maehly,
!    Charles Mesztenyi, John Rice, Henry Thacher, Christoph Witzgall,
!    Computer Approximations,
!    Wiley, 1968.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the Gamma function.
!    X must be positive.
!
!    Output, real ( kind = 8 ) GAMMA_LOG, the logarithm of the Gamma
!    function of X.
!
!  Local Parameters:
!
!    Local, real ( kind = 8 ) BETA, the radix for the floating-point
!    representation.
!
!    Local, integer MAXEXP, the smallest positive power of BETA that overflows.
!
!    Local, real ( kind = 8 ) XBIG, the largest argument for which
!    LN(GAMMA(X)) is representable in the machine, the solution to the equation
!      LN(GAMMA(XBIG)) = BETA**MAXEXP.
!
!    Local, real ( kind = 8 ) FRTBIG, a rough estimate of the fourth root
!    of XBIG.
!
!  Approximate values for some important machines are:
!
!                            BETA      MAXEXP         XBIG     FRTBIG
!
!  CRAY-1        (S.P.)        2        8191       9.62D+2461  3.13D+615
!  Cyber 180/855 (S.P.)        2        1070       1.72D+319   6.44D+79
!  IEEE (IBM/XT) (S.P.)        2         128       4.08D+36    1.42D+9
!  IEEE (IBM/XT) (D.P.)        2        1024       2.55D+305   2.25D+76
!  IBM 3033      (D.P.)       16          63       4.29D+73    2.56D+18
!  VAX D-Format  (D.P.)        2         127       2.05D+36    1.20D+9
!  VAX G-Format  (D.P.)        2        1023       1.28D+305   1.89D+76
!    
*/

                    
		      __m256d gamma_log_ymm4r8(const __m256d x) {
		      
                         
			    const __m256d d1     = _mm256_set1_pd(-5.772156649015328605195174E-01);
			    const __m256d d2     = _mm256_set1_pd(4.227843350984671393993777E-01);
                            const __m256d d4     = _mm256_set1_pd(1.791759469228055000094023E+00);
                            const __m256d frtbig = _mm256_set1_pd(1.42E+09);
                            const __m256d pnt68  = _mm256_set1_pd(0.6796875E+00);
			    const __m256d sqrtpi = _mm256_set1_pd(0.9189385332046727417803297E+00);
			    const __m256d xbig   = _mm256_set1_pd(4.08E+36);
			    const __m256d _0     = _mm256_setzero_pd();
			    const __m256d _1_2   = _mm256_set1_pd(0.5);
			    const __m256d _1_5   = _mm256_set1_pd(1.5);
			    const __m256d _1     = _mm256_set1_pd(1.0);
			    const __m256d _4     = _mm256_set1_pd(4.0);
			    const __m256d _2     = _mm256_set1_pd(2.0);
			    const __m256d _12    = _mm256_set1_pd(12.0);
			    const __m256d huge   = _mm256_set1_pd(DBL_MAX);
			    const __m256d eps    = _mm256_set1_pd(DBL_EPSILON);
			    __m256d gamlog,res,xden;
			    __m256d xm1,xm2,xm4;
			    __m256d xnum,xsq,corr;
			    gamlog = _mm256_setzero_pd();
			   
			    if(_mm256_cmp_pd_mask(x,eps,_CMP_LE_OQ)) {
                               res = ymm4r8_negate(_mm256_log_pd(x));
			    }
			    else if(_mm256_cmp_pd_mask(x,_1_5,_CMP_LE_OQ)) {
                               const __mmask8 m0 = _mm256_cmp_pd_mask(x,pnt68,_CMP_LT_OQ);
			       corr = _mm256_mask_blend_pd(m0,_0,ymm4r8_negate(_mm256_log_pd(x)));
			       xm1  = _mm256_mask_blend_pd(m0,_mm256_sub_pd(
			                                                _mm256_sub_pd(x,_1_2),_1_2));

			       if(_mm256_cmp_pd_mask(x,_1_2,_CMP_LE_OQ) ||
			          _mm256_cmp_pd_mask(pnt68,x,_CMP_LE_OQ)) {
                                   xden = _1;
				   xnum = _0;
#if defined(__GNUC__) && !defined(__INTEL_COMPILER)				   
				   xnum = _mm256_fmadd_pd(xnum,xm1,*(__m256d*)&gamma_log_ymm4r8_p1[0]);
				   xden = _mm256_fmadd_pd(xden,xm1,*(__m256d*)&gamma_log_ymm4r8_q1[0]);
				   xnum = _mm256_fmadd_pd(xnum,xm1,*(__m256d*)&gamma_log_ymm4r8_p1[4]);
				   xden = _mm256_fmadd_pd(xden,xm1,*(__m256d*)&gamma_log_ymm4r8_q1[4]);
				   xnum = _mm256_fmadd_pd(xnum,xm1,*(__m256d*)&gamma_log_ymm4r8_p1[8]);
				   xden = _mm256_fmadd_pd(xden,xm1,*(__m256d*)&gamma_log_ymm4r8_q1[8]);
				   xnum = _mm256_fmadd_pd(xnum,xm1,*(__m256d*)&gamma_log_ymm4r8_p1[12]);
				   xden = _mm256_fmadd_pd(xden,xm1,*(__m256d*)&gamma_log_ymm4r8_q1[12]);
				   xnum = _mm256_fmadd_pd(xnum,xm1,*(__m256d*)&gamma_log_ymm4r8_p1[16]);
				   xden = _mm256_fmadd_pd(xden,xm1,*(__m256d*)&gamma_log_ymm4r8_q1[16]);
				   xnum = _mm256_fmadd_pd(xnum,xm1,*(__m256d*)&gamma_log_ymm4r8_p1[20]);
				   xden = _mm256_fmadd_pd(xden,xm1,*(__m256d*)&gamma_log_ymm4r8_q1[20]);
				   xnum = _mm256_fmadd_pd(xnum,xm1,*(__m256d*)&gamma_log_ymm4r8_p1[24]);
				   xden = _mm256_fmadd_pd(xden,xm1,*(__m256d*)&gamma_log_ymm4r8_q1[24]);
				   xnum = _mm256_fmadd_pd(xnum,xm1,*(__m256d*)&gamma_log_ymm4r8_p1[28]);
				   xden = _mm256_fmadd_pd(xden,xm1,*(__m256d*)&gamma_log_ymm4r8_q1[28]);
#elif !defined(__GNUC__) && defined(__INTEL_COMPILER)
                                   xnum = _mm256_fmadd_pd(xnum,xm1,gamma_log_ymm4r8_p1[0]);
				   xden = _mm256_fmadd_pd(xden,xm1,gamma_log_ymm4r8_q1[0]);
				   xnum = _mm256_fmadd_pd(xnum,xm1,gamma_log_ymm4r8_p1[1]);
				   xden = _mm256_fmadd_pd(xden,xm1,gamma_log_ymm4r8_q1[1]);
				   xnum = _mm256_fmadd_pd(xnum,xm1,gamma_log_ymm4r8_p1[2]);
				   xden = _mm256_fmadd_pd(xden,xm1,gamma_log_ymm4r8_q1[2]);
				   xnum = _mm256_fmadd_pd(xnum,xm1,gamma_log_ymm4r8_p1[3]);
				   xden = _mm256_fmadd_pd(xden,xm1,gamma_log_ymm4r8_q1[3]);
				   xnum = _mm256_fmadd_pd(xnum,xm1,gamma_log_ymm4r8_p1[4]);
				   xden = _mm256_fmadd_pd(xden,xm1,gamma_log_ymm4r8_q1[4]);
				   xnum = _mm256_fmadd_pd(xnum,xm1,gamma_log_ymm4r8_p1[5]);
				   xden = _mm256_fmadd_pd(xden,xm1,gamma_log_ymm4r8_q1[5]);
				   xnum = _mm256_fmadd_pd(xnum,xm1,gamma_log_ymm4r8_p1[6]);
				   xden = _mm256_fmadd_pd(xden,xm1,gamma_log_ymm4r8_q1[6]);
				   xnum = _mm256_fmadd_pd(xnum,xm1,gamma_log_ymm4r8_p1[7]);
				   xden = _mm256_fmadd_pd(xden,xm1,gamma_log_ymm4r8_q1[7]);
#endif				   
				   const __m256d t0 = _mm256_fmadd_pd(xm1,
				                                  _mm256_div_pd(xnum,xden),d1);
				   res  = _mm256_add_pd(corr,
				                    _mm256_mul_pd(xm1,t0));
				}
				else {

                                   xm2  = _mm256_sub_pd(_mm256_sub_pd(x,_1_2),_1_2);
				   xden = _1;
				   xnum = _0;
#if defined(__GNUC__) && !defined(__INTEL_COMPILER)					   
				   xnum = _mm256_fmadd_pd(xnum,xm2,*(__m256d*)&gamma_log_ymm4r8_p2[0]);
				   xden = _mm256_fmadd_pd(xden,xm2,*(__m256d*)&gamma_log_ymm4r8_q2[0]);
				   xnum = _mm256_fmadd_pd(xnum,xm2,*(__m256d*)&gamma_log_ymm4r8_p2[4]);
				   xden = _mm256_fmadd_pd(xden,xm2,*(__m256d*)&gamma_log_ymm4r8_q2[4]);
				   xnum = _mm256_fmadd_pd(xnum,xm2,*(__m256d*)&gamma_log_ymm4r8_p2[8]);
				   xden = _mm256_fmadd_pd(xden,xm2,*(__m256d*)&gamma_log_ymm4r8_q2[8]);
				   xnum = _mm256_fmadd_pd(xnum,xm2,*(__m256d*)&gamma_log_ymm4r8_p2[12]);
				   xden = _mm256_fmadd_pd(xden,xm2,*(__m256d*)&gamma_log_ymm4r8_q2[12]);
				   xnum = _mm256_fmadd_pd(xnum,xm2,*(__m256d*)&gamma_log_ymm4r8_p2[16]);
				   xden = _mm256_fmadd_pd(xden,xm2,*(__m256d*)&gamma_log_ymm4r8_q2[16]);
				   xnum = _mm256_fmadd_pd(xnum,xm2,*(__m256d*)&gamma_log_ymm4r8_p2[20]);
				   xden = _mm256_fmadd_pd(xden,xm2,*(__m256d*)&gamma_log_ymm4r8_q2[20]);
				   xnum = _mm256_fmadd_pd(xnum,xm2,*(__m256d*)&gamma_log_ymm4r8_p2[24]);
				   xden = _mm256_fmadd_pd(xden,xm2,*(__m256d*)&gamma_log_ymm4r8_q2[24]);
				   xnum = _mm256_fmadd_pd(xnum,xm2,*(__m256d*)&gamma_log_ymm4r8_p2[28]);
				   xden = _mm256_fmadd_pd(xden,xm2,*(__m256d*)&gamma_log_ymm4r8_q2[28]);
#elif !defined(__GNUC__) && defined(__INTEL_COMPILER)
                                   xnum = _mm256_fmadd_pd(xnum,xm2,gamma_log_ymm4r8_p2[0]);
				   xden = _mm256_fmadd_pd(xden,xm2,gamma_log_ymm4r8_q2[0]);
				   xnum = _mm256_fmadd_pd(xnum,xm2,gamma_log_ymm4r8_p2[1]);
				   xden = _mm256_fmadd_pd(xden,xm2,gamma_log_ymm4r8_q2[1]);
				   xnum = _mm256_fmadd_pd(xnum,xm2,gamma_log_ymm4r8_p2[2]);
				   xden = _mm256_fmadd_pd(xden,xm2,gamma_log_ymm4r8_q2[2]);
				   xnum = _mm256_fmadd_pd(xnum,xm2,gamma_log_ymm4r8_p2[3]);
				   xden = _mm256_fmadd_pd(xden,xm2,gamma_log_ymm4r8_q2[3]);
				   xnum = _mm256_fmadd_pd(xnum,xm2,gamma_log_ymm4r8_p2[4]);
				   xden = _mm256_fmadd_pd(xden,xm2,gamma_log_ymm4r8_q2[4]);
				   xnum = _mm256_fmadd_pd(xnum,xm2,gamma_log_ymm4r8_p2[5]);
				   xden = _mm256_fmadd_pd(xden,xm2,gamma_log_ymm4r8_q2[5]);
				   xnum = _mm256_fmadd_pd(xnum,xm2,gamma_log_ymm4r8_p2[6]);
				   xden = _mm256_fmadd_pd(xden,xm2,gamma_log_ymm4r8_q2[6]);
				   xnum = _mm256_fmadd_pd(xnum,xm2,gamma_log_ymm4r8_p2[7]);
				   xden = _mm256_fmadd_pd(xden,xm2,gamma_log_ymm4r8_q2[7]);
#endif				   
				   const __m256d t0 = _mm256_fmadd_pd(xm2,
				                                  _mm256_div_pd(xnum,xden),d2);
				   res  = _mm256_add_pd(corr,
				                    _mm256_mul_pd(xm2,t0));
				}
			    }
			    else if(_mm256_cmp_pd_mask(x,_4,_CMP_LE_OQ)) {
                                   xm2  = _mm256_sub_pd(x,_2);
				   xden = _1;
				   xnum = _0;
#if defined(__GNUC__) && !defined(__INTEL_COMPILER)					   
				   xnum = _mm256_fmadd_pd(xnum,xm2,*(__m256d*)&gamma_log_ymm4r8_p2[0]);
				   xden = _mm256_fmadd_pd(xden,xm2,*(__m256d*)&gamma_log_ymm4r8_q2[0]);
				   xnum = _mm256_fmadd_pd(xnum,xm2,*(__m256d*)&gamma_log_ymm4r8_p2[4]);
				   xden = _mm256_fmadd_pd(xden,xm2,*(__m256d*)&gamma_log_ymm4r8_q2[4]);
				   xnum = _mm256_fmadd_pd(xnum,xm2,*(__m256d*)&gamma_log_ymm4r8_p2[8]);
				   xden = _mm256_fmadd_pd(xden,xm2,*(__m256d*)&gamma_log_ymm4r8_q2[8]);
				   xnum = _mm256_fmadd_pd(xnum,xm2,*(__m256d*)&gamma_log_ymm4r8_p2[12]);
				   xden = _mm256_fmadd_pd(xden,xm2,*(__m256d*)&gamma_log_ymm4r8_q2[12]);
				   xnum = _mm256_fmadd_pd(xnum,xm2,*(__m256d*)&gamma_log_ymm4r8_p2[16]);
				   xden = _mm256_fmadd_pd(xden,xm2,*(__m256d*)&gamma_log_ymm4r8_q2[16]);
				   xnum = _mm256_fmadd_pd(xnum,xm2,*(__m256d*)&gamma_log_ymm4r8_p2[20]);
				   xden = _mm256_fmadd_pd(xden,xm2,*(__m256d*)&gamma_log_ymm4r8_q2[20]);
				   xnum = _mm256_fmadd_pd(xnum,xm2,*(__m256d*)&gamma_log_ymm4r8_p2[24]);
				   xden = _mm256_fmadd_pd(xden,xm2,*(__m256d*)&gamma_log_ymm4r8_q2[24]);
				   xnum = _mm256_fmadd_pd(xnum,xm2,*(__m256d*)&gamma_log_ymm4r8_p2[28]);
				   xden = _mm256_fmadd_pd(xden,xm2,*(__m256d*)&gamma_log_ymm4r8_q2[28]);
#elif !defined(__GNUC__) && defined(__INTEL_COMPILER)
                                   xnum = _mm256_fmadd_pd(xnum,xm2,gamma_log_ymm4r8_p2[0]);
				   xden = _mm256_fmadd_pd(xden,xm2,gamma_log_ymm4r8_q2[0]);
				   xnum = _mm256_fmadd_pd(xnum,xm2,gamma_log_ymm4r8_p2[1]);
				   xden = _mm256_fmadd_pd(xden,xm2,gamma_log_ymm4r8_q2[1]);
				   xnum = _mm256_fmadd_pd(xnum,xm2,gamma_log_ymm4r8_p2[2]);
				   xden = _mm256_fmadd_pd(xden,xm2,gamma_log_ymm4r8_q2[2]);
				   xnum = _mm256_fmadd_pd(xnum,xm2,gamma_log_ymm4r8_p2[3]);
				   xden = _mm256_fmadd_pd(xden,xm2,gamma_log_ymm4r8_q2[3]);
				   xnum = _mm256_fmadd_pd(xnum,xm2,gamma_log_ymm4r8_p2[4]);
				   xden = _mm256_fmadd_pd(xden,xm2,gamma_log_ymm4r8_q2[4]);
				   xnum = _mm256_fmadd_pd(xnum,xm2,gamma_log_ymm4r8_p2[5]);
				   xden = _mm256_fmadd_pd(xden,xm2,gamma_log_ymm4r8_q2[5]);
				   xnum = _mm256_fmadd_pd(xnum,xm2,gamma_log_ymm4r8_p2[6]);
				   xden = _mm256_fmadd_pd(xden,xm2,gamma_log_ymm4r8_q2[6]);
				   xnum = _mm256_fmadd_pd(xnum,xm2,gamma_log_ymm4r8_p2[7]);
				   xden = _mm256_fmadd_pd(xden,xm2,gamma_log_ymm4r8_q2[7]);
#endif				   
				   res  = _mm256_mul_pd(xm2,
				                    _mm256_fmadd_pd(xm2,
						                _mm256_div_pd(xnum,xden),d2));
			    }
			    else if(_mm256_cmp_pd_mask(x,_12,_CMP_LE_OQ)) {
                                   xm4  = _mm256_sub_pd(x,_4);
				   xden = ymm4r8_negate(_1);
				   xnum = _0;
#if defined(__GNUC__) && !defined(__INTEL_COMPILER)					   
				   xnum = _mm256_fmadd_pd(xnum,xm4,*(__m256d*)&gamma_log_ymm4r8_p4[0]);
				   xden = _mm256_fmadd_pd(xden,xm4,*(__m256d*)&gamma_log_ymm4r8_q4[0]);
				   xnum = _mm256_fmadd_pd(xnum,xm4,*(__m256d*)&gamma_log_ymm4r8_p4[4]);
				   xden = _mm256_fmadd_pd(xden,xm4,*(__m256d*)&gamma_log_ymm4r8_q4[4]);
				   xnum = _mm256_fmadd_pd(xnum,xm4,*(__m256d*)&gamma_log_ymm4r8_p4[8]);
				   xden = _mm256_fmadd_pd(xden,xm4,*(__m256d*)&gamma_log_ymm4r8_q4[8]);
				   xnum = _mm256_fmadd_pd(xnum,xm4,*(__m256d*)&gamma_log_ymm4r8_p4[12]);
				   xden = _mm256_fmadd_pd(xden,xm4,*(__m256d*)&gamma_log_ymm4r8_q4[12]);
				   xnum = _mm256_fmadd_pd(xnum,xm4,*(__m256d*)&gamma_log_ymm4r8_p4[16]);
				   xden = _mm256_fmadd_pd(xden,xm4,*(__m256d*)&gamma_log_ymm4r8_q4[16]);
				   xnum = _mm256_fmadd_pd(xnum,xm4,*(__m256d*)&gamma_log_ymm4r8_p4[20]);
				   xden = _mm256_fmadd_pd(xden,xm4,*(__m256d*)&gamma_log_ymm4r8_q4[20]);
				   xnum = _mm256_fmadd_pd(xnum,xm4,*(__m256d*)&gamma_log_ymm4r8_p4[24]);
				   xden = _mm256_fmadd_pd(xden,xm4,*(__m256d*)&gamma_log_ymm4r8_q4[24]);
				   xnum = _mm256_fmadd_pd(xnum,xm4,*(__m256d*)&gamma_log_ymm4r8_p4[28]);
				   xden = _mm256_fmadd_pd(xden,xm4,*(__m256d*)&gamma_log_ymm4r8_q4[28]);
#elif !defined(__GNUC__) && defined(__INTEL_COMPILER)
                                   xnum = _mm256_fmadd_pd(xnum,xm4,gamma_log_ymm4r8_p4[0]);
				   xden = _mm256_fmadd_pd(xden,xm4,gamma_log_ymm4r8_q4[0]);
				   xnum = _mm256_fmadd_pd(xnum,xm4,gamma_log_ymm4r8_p4[1]);
				   xden = _mm256_fmadd_pd(xden,xm4,gamma_log_ymm4r8_q4[1]);
				   xnum = _mm256_fmadd_pd(xnum,xm4,gamma_log_ymm4r8_p4[2]);
				   xden = _mm256_fmadd_pd(xden,xm4,gamma_log_ymm4r8_q4[2]);
				   xnum = _mm256_fmadd_pd(xnum,xm4,gamma_log_ymm4r8_p4[3]);
				   xden = _mm256_fmadd_pd(xden,xm4,gamma_log_ymm4r8_q4[3]);
				   xnum = _mm256_fmadd_pd(xnum,xm4,gamma_log_ymm4r8_p4[4]);
				   xden = _mm256_fmadd_pd(xden,xm4,gamma_log_ymm4r8_q4[4]);
				   xnum = _mm256_fmadd_pd(xnum,xm4,gamma_log_ymm4r8_p4[5]);
				   xden = _mm256_fmadd_pd(xden,xm4,gamma_log_ymm4r8_q4[5]);
				   xnum = _mm256_fmadd_pd(xnum,xm4,gamma_log_ymm4r8_p4[6]);
				   xden = _mm256_fmadd_pd(xden,xm4,gamma_log_ymm4r8_q4[6]);
				   xnum = _mm256_fmadd_pd(xnum,xm4,gamma_log_ymm4r8_p4[7]);
				   xden = _mm256_fmadd_pd(xden,xm4,gamma_log_ymm4r8_q4[7]);
#endif				   
				   res  = _mm256_fmadd_pd(xm4,_mm256_div_pd(xnum,xden),d4);
			    }
			    else {
                                   res  = _0;
				   if(_mm256_cmp_pd_mask(x,frtbig,_CMP_LE_OQ)) {
#if defined(__GNUC__) && !defined(__INTEL_COMPILER)					   
                                      res = *(__m256d*)&gamma_log_ymm4r8_c[24];
				      xsq = _mm256_mul_pd(x,x);
				      res = _mm256_add_pd(_mm256_div_pd(res,xsq),*(__m256d*)&gamma_log_ymm4r8_c[0]);
				      res = _mm256_add_pd(_mm256_div_pd(res,xsq),*(__m256d*)&gamma_log_ymm4r8_c[4]);
				      res = _mm256_add_pd(_mm256_div_pd(res,xsq),*(__m256d*)&gamma_log_ymm4r8_c[8]);
				      res = _mm256_add_pd(_mm256_div_pd(res,xsq),*(__m256d*)&gamma_log_ymm4r8_c[12]);
				      res = _mm256_add_pd(_mm256_div_pd(res,xsq),*(__m256d*)&gamma_log_ymm4r8_c[16]);
				      res = _mm256_add_pd(_mm256_div_pd(res,xsq),*(__m256d*)&gamma_log_ymm4r8_c[20]);
#elif !defined(__GNUC__) && defined(__INTEL_COMPILER)
                                      res = gamma_log_ymm4r8_c[6];
				      xsq = _mm256_mul_pd(x,x);
				      res = _mm256_add_pd(_mm256_div_pd(res,xsq),gamma_log_ymm4r8_c[0]);
				      res = _mm256_add_pd(_mm256_div_pd(res,xsq),gamma_log_ymm4r8_c[1]);
				      res = _mm256_add_pd(_mm256_div_pd(res,xsq),gamma_log_ymm4r8_c[2]);
				      res = _mm256_add_pd(_mm256_div_pd(res,xsq),gamma_log_ymm4r8_c[3]);
				      res = _mm256_add_pd(_mm256_div_pd(res,xsq),gamma_log_ymm4r8_c[4]);
				      res = _mm256_add_pd(_mm256_div_pd(res,xsq),gamma_log_ymm4r8_c[5]);
#endif				      
				   }
                                   res  = _mm256_div_pd(res,x);
				   corr = _mm256_log_pd(x);
				   res  = _mm256_sub_pd(_mm256_add_pd(res,sqrtpi),
				                        _mm256_mul_pd(_1_2,corr));
				   res  = _mm256_fmadd_pd(x,_mm256_sub_pd(corr,_1),res);
				   
			    }

			    gamlog = res;
			    return (gamlog);
			    
		  }
		  
		  
		   __m256 gamma_log_ymm8r4(const __m256 x) {
		      
                      
			    const __m256 d1     = _mm256_set1_ps(-5.772156649015328605195174E-01f);
			    const __m256 d2     = _mm256_set1_ps(4.227843350984671393993777E-01f);
                            const __m256 d4     = _mm256_set1_ps(1.791759469228055000094023E+00f);
                            const __m256 frtbig = _mm256_set1_ps(1.42E+09f);
                            const __m256 pnt68  = _mm256_set1_ps(0.6796875E+00f);
			    const __m256 sqrtpi = _mm256_set1_ps(0.9189385332046727417803297E+00f);
			    const __m256 xbig   = _mm256_set1_ps(4.08E+36);
			    const __m256 _0     = _mm256_setzero_ps();
			    const __m256 _1_2   = _mm256_set1_ps(0.5);
			    const __m256 _1_5   = _mm256_set1_ps(1.5);
			    const __m256 _1     = _mm256_set1_ps(1.0);
			    const __m256 _4     = _mm256_set1_ps(4.0);
			    const __m256 _2     = _mm256_set1_ps(2.0);
			    const __m256 _12    = _mm256_set1_ps(12.0);
			    const __m256 huge   = _mm256_set1_ps(FLT_MAX);
			    const __m256 eps    = _mm256_set1_ps(FLT_EPSILON);
			    __m256 gamlog,res,xden;
			    __m256 xm1,xm2,xm4;
			    __m256 xnum,xsq,corr;
			    gamlog = _mm256_setzero_ps();
			   
			    if(_mm256_cmp_ps_mask(x,eps,_CMP_LE_OQ)) {
                               res = ymm8r4_negate(_mm256_log_ps(x));
			    }
			    else if(_mm256_cmp_ps_mask(x,_1_5,_CMP_LE_OQ)) {
                               const __mmask16 m0 = _mm256_cmp_ps_mask(x,pnt68,_CMP_LT_OQ);
			       corr = _mm256_mask_blend_ps(m0,_0,ymm8r4_negate(_mm256_log_ps(x)));
			       xm1  = _mm256_mask_blend_ps(m0,_mm256_sub_ps(
			                                                _mm256_sub_ps(x,_1_2),_1_2));

			       if(_mm256_cmp_ps_mask(x,_1_2,_CMP_LE_OQ) ||
			          _mm256_cmp_ps_mask(pnt68,x,_CMP_LE_OQ)) {
                                   xden = _1;
				   xnum = _0;
#if defined(__GNUC__) && !defined(__INTEL_COMPILER)					   
				   xnum = _mm256_fmadd_ps(xnum,xm1,*(__m256*)&gamma_log_ymm8r4_p1[0]);
				   xden = _mm256_fmadd_ps(xden,xm1,*(__m256*)&gamma_log_ymm8r4_q1[0]);
				   xnum = _mm256_fmadd_ps(xnum,xm1,*(__m256*)&gamma_log_ymm8r4_p1[8]);
				   xden = _mm256_fmadd_ps(xden,xm1,*(__m256*)&gamma_log_ymm8r4_q1[8]);
				   xnum = _mm256_fmadd_ps(xnum,xm1,*(__m256*)&gamma_log_ymm8r4_p1[16]);
				   xden = _mm256_fmadd_ps(xden,xm1,*(__m256*)&gamma_log_ymm8r4_q1[16]);
				   xnum = _mm256_fmadd_ps(xnum,xm1,*(__m256*)&gamma_log_ymm8r4_p1[24]);
				   xden = _mm256_fmadd_ps(xden,xm1,*(__m256*)&gamma_log_ymm8r4_q1[24]);
				   xnum = _mm256_fmadd_ps(xnum,xm1,*(__m256*)&gamma_log_ymm8r4_p1[32]);
				   xden = _mm256_fmadd_ps(xden,xm1,*(__m256*)&gamma_log_ymm8r4_q1[32]);
				   xnum = _mm256_fmadd_ps(xnum,xm1,*(__m256*)&gamma_log_ymm8r4_p1[40]);
				   xden = _mm256_fmadd_ps(xden,xm1,*(__m256*)&gamma_log_ymm8r4_q1[40]);
				   xnum = _mm256_fmadd_ps(xnum,xm1,*(__m256*)&gamma_log_ymm8r4_p1[48]);
				   xden = _mm256_fmadd_ps(xden,xm1,*(__m256*)&gamma_log_ymm8r4_q1[48]);
				   xnum = _mm256_fmadd_ps(xnum,xm1,*(__m256*)&gamma_log_ymm8r4_p1[56]);
				   xden = _mm256_fmadd_ps(xden,xm1,*(__m256*)&gamma_log_ymm8r4_q1[56]);
#elif !defined(__GNUC__) && defined(__INTEL_COMPILER)
                                   xnum = _mm256_fmadd_ps(xnum,xm1,gamma_log_ymm8r4_p1[0]);
				   xden = _mm256_fmadd_ps(xden,xm1,gamma_log_ymm8r4_q1[0]);
				   xnum = _mm256_fmadd_ps(xnum,xm1,gamma_log_ymm8r4_p1[1]);
				   xden = _mm256_fmadd_ps(xden,xm1,gamma_log_ymm8r4_q1[1]);
				   xnum = _mm256_fmadd_ps(xnum,xm1,gamma_log_ymm8r4_p1[2]);
				   xden = _mm256_fmadd_ps(xden,xm1,gamma_log_ymm8r4_q1[2]);
				   xnum = _mm256_fmadd_ps(xnum,xm1,gamma_log_ymm8r4_p1[3]);
				   xden = _mm256_fmadd_ps(xden,xm1,gamma_log_ymm8r4_q1[3]);
				   xnum = _mm256_fmadd_ps(xnum,xm1,gamma_log_ymm8r4_p1[4]);
				   xden = _mm256_fmadd_ps(xden,xm1,gamma_log_ymm8r4_q1[4]);
				   xnum = _mm256_fmadd_ps(xnum,xm1,gamma_log_ymm8r4_p1[5]);
				   xden = _mm256_fmadd_ps(xden,xm1,gamma_log_ymm8r4_q1[5]);
				   xnum = _mm256_fmadd_ps(xnum,xm1,gamma_log_ymm8r4_p1[6]);
				   xden = _mm256_fmadd_ps(xden,xm1,gamma_log_ymm8r4_q1[6]);
				   xnum = _mm256_fmadd_ps(xnum,xm1,gamma_log_ymm8r4_p1[7]);
				   xden = _mm256_fmadd_ps(xden,xm1,gamma_log_ymm8r4_q1[7]);
#endif				   
				   const __m256 t0 = _mm256_fmadd_ps(xm1,
				                                  _mm256_div_ps(xnum,xden),d1);
				   res  = _mm256_add_ps(corr,
				                    _mm256_mul_ps(xm1,t0));
				}
				else {

                                   xm2  = _mm256_sub_ps(_mm256_sub_ps(x,_1_2),_1_2);
				   xden = _1;
				   xnum = _0;
#if defined(__GNUC__) && !defined(__INTEL_COMPILER)				   
				   xnum = _mm256_fmadd_ps(xnum,xm2,*(__m256*)&gamma_log_ymm8r4_p2[0]);
				   xden = _mm256_fmadd_ps(xden,xm2,*(__m256*)&gamma_log_ymm8r4_q2[0]);
				   xnum = _mm256_fmadd_ps(xnum,xm2,*(__m256*)&gamma_log_ymm8r4_p2[8]);
				   xden = _mm256_fmadd_ps(xden,xm2,*(__m256*)&gamma_log_ymm8r4_q2[8]);
				   xnum = _mm256_fmadd_ps(xnum,xm2,*(__m256*)&gamma_log_ymm8r4_p2[16]);
				   xden = _mm256_fmadd_ps(xden,xm2,*(__m256*)&gamma_log_ymm8r4_q2[16]);
				   xnum = _mm256_fmadd_ps(xnum,xm2,*(__m256*)&gamma_log_ymm8r4_p2[24]);
				   xden = _mm256_fmadd_ps(xden,xm2,*(__m256*)&gamma_log_ymm8r4_q2[24]);
				   xnum = _mm256_fmadd_ps(xnum,xm2,*(__m256*)&gamma_log_ymm8r4_p2[32]);
				   xden = _mm256_fmadd_ps(xden,xm2,*(__m256*)&gamma_log_ymm8r4_q2[32]);
				   xnum = _mm256_fmadd_ps(xnum,xm2,*(__m256*)&gamma_log_ymm8r4_p2[40]);
				   xden = _mm256_fmadd_ps(xden,xm2,*(__m256*)&gamma_log_ymm8r4_q2[40]);
				   xnum = _mm256_fmadd_ps(xnum,xm2,*(__m256*)&gamma_log_ymm8r4_p2[48]);
				   xden = _mm256_fmadd_ps(xden,xm2,*(__m256*)&gamma_log_ymm8r4_q2[48]);
				   xnum = _mm256_fmadd_ps(xnum,xm2,*(__m256*)&gamma_log_ymm8r4_p2[56]);
				   xden = _mm256_fmadd_ps(xden,xm2,*(__m256*)&gamma_log_ymm8r4_q2[56]);
#elif !defined(__GNUC__) && defined(__INTEL_COMPILER)
                                   xnum = _mm256_fmadd_ps(xnum,xm2,gamma_log_ymm8r4_p2[0]);
				   xden = _mm256_fmadd_ps(xden,xm2,gamma_log_ymm8r4_q2[0]);
				   xnum = _mm256_fmadd_ps(xnum,xm2,gamma_log_ymm8r4_p2[1]);
				   xden = _mm256_fmadd_ps(xden,xm2,gamma_log_ymm8r4_q2[1]);
				   xnum = _mm256_fmadd_ps(xnum,xm2,gamma_log_ymm8r4_p2[2]);
				   xden = _mm256_fmadd_ps(xden,xm2,gamma_log_ymm8r4_q2[2]);
				   xnum = _mm256_fmadd_ps(xnum,xm2,gamma_log_ymm8r4_p2[3]);
				   xden = _mm256_fmadd_ps(xden,xm2,gamma_log_ymm8r4_q2[3]);
				   xnum = _mm256_fmadd_ps(xnum,xm2,gamma_log_ymm8r4_p2[4]);
				   xden = _mm256_fmadd_ps(xden,xm2,gamma_log_ymm8r4_q2[4]);
				   xnum = _mm256_fmadd_ps(xnum,xm2,gamma_log_ymm8r4_p2[5]);
				   xden = _mm256_fmadd_ps(xden,xm2,gamma_log_ymm8r4_q2[5]);
				   xnum = _mm256_fmadd_ps(xnum,xm2,gamma_log_ymm8r4_p2[6]);
				   xden = _mm256_fmadd_ps(xden,xm2,gamma_log_ymm8r4_q2[6]);
				   xnum = _mm256_fmadd_ps(xnum,xm2,gamma_log_ymm8r4_p2[7]);
				   xden = _mm256_fmadd_ps(xden,xm2,gamma_log_ymm8r4_q2[7]);
#endif				   
				   const __m256 t0 = _mm256_fmadd_ps(xm2,
				                                  _mm256_div_ps(xnum,xden),d2);
				   res  = _mm256_add_ps(corr,
				                    _mm256_mul_ps(xm2,t0));
				}
			    }
			    else if(_mm256_cmp_ps_mask(x,_4,_CMP_LE_OQ)) {
                                   xm2  = _mm256_sub_ps(x,_2);
				   xden = _1;
				   xnum = _0;
#if defined(__GNUC__) && !defined(__INTEL_COMPILER)				   
				   xnum = _mm256_fmadd_ps(xnum,xm2,*(__m256*)&gamma_log_ymm8r4_p2[0]);
				   xden = _mm256_fmadd_ps(xden,xm2,*(__m256*)&gamma_log_ymm8r4_q2[0]);
				   xnum = _mm256_fmadd_ps(xnum,xm2,*(__m256*)&gamma_log_ymm8r4_p2[8]);
				   xden = _mm256_fmadd_ps(xden,xm2,*(__m256*)&gamma_log_ymm8r4_q2[8]);
				   xnum = _mm256_fmadd_ps(xnum,xm2,*(__m256*)&gamma_log_ymm8r4_p2[16]);
				   xden = _mm256_fmadd_ps(xden,xm2,*(__m256*)&gamma_log_ymm8r4_q2[16]);
				   xnum = _mm256_fmadd_ps(xnum,xm2,*(__m256*)&gamma_log_ymm8r4_p2[24]);
				   xden = _mm256_fmadd_ps(xden,xm2,*(__m256*)&gamma_log_ymm8r4_q2[24]);
				   xnum = _mm256_fmadd_ps(xnum,xm2,*(__m256*)&gamma_log_ymm8r4_p2[32]);
				   xden = _mm256_fmadd_ps(xden,xm2,*(__m256*)&gamma_log_ymm8r4_q2[32]);
				   xnum = _mm256_fmadd_ps(xnum,xm2,*(__m256*)&gamma_log_ymm8r4_p2[40]);
				   xden = _mm256_fmadd_ps(xden,xm2,*(__m256*)&gamma_log_ymm8r4_q2[40]);
				   xnum = _mm256_fmadd_ps(xnum,xm2,*(__m256*)&gamma_log_ymm8r4_p2[48]);
				   xden = _mm256_fmadd_ps(xden,xm2,*(__m256*)&gamma_log_ymm8r4_q2[48]);
				   xnum = _mm256_fmadd_ps(xnum,xm2,*(__m256*)&gamma_log_ymm8r4_p2[56]);
				   xden = _mm256_fmadd_ps(xden,xm2,*(__m256*)&gamma_log_ymm8r4_q2[56]);
#elif !defined(__GNUC__) && defined(__INTEL_COMPILER)
                                   xnum = _mm256_fmadd_ps(xnum,xm2,gamma_log_ymm8r4_p2[0]);
				   xden = _mm256_fmadd_ps(xden,xm2,gamma_log_ymm8r4_q2[0]);
				   xnum = _mm256_fmadd_ps(xnum,xm2,gamma_log_ymm8r4_p2[1]);
				   xden = _mm256_fmadd_ps(xden,xm2,gamma_log_ymm8r4_q2[1]);
				   xnum = _mm256_fmadd_ps(xnum,xm2,gamma_log_ymm8r4_p2[2]);
				   xden = _mm256_fmadd_ps(xden,xm2,gamma_log_ymm8r4_q2[2]);
				   xnum = _mm256_fmadd_ps(xnum,xm2,gamma_log_ymm8r4_p2[3]);
				   xden = _mm256_fmadd_ps(xden,xm2,gamma_log_ymm8r4_q2[3]);
				   xnum = _mm256_fmadd_ps(xnum,xm2,gamma_log_ymm8r4_p2[4]);
				   xden = _mm256_fmadd_ps(xden,xm2,gamma_log_ymm8r4_q2[4]);
				   xnum = _mm256_fmadd_ps(xnum,xm2,gamma_log_ymm8r4_p2[5]);
				   xden = _mm256_fmadd_ps(xden,xm2,gamma_log_ymm8r4_q2[5]);
				   xnum = _mm256_fmadd_ps(xnum,xm2,gamma_log_ymm8r4_p2[6]);
				   xden = _mm256_fmadd_ps(xden,xm2,gamma_log_ymm8r4_q2[6]);
				   xnum = _mm256_fmadd_ps(xnum,xm2,gamma_log_ymm8r4_p2[7]);
				   xden = _mm256_fmadd_ps(xden,xm2,gamma_log_ymm8r4_q2[7]);
#endif				   
				   res  = _mm256_mul_ps(xm2,
				                    _mm256_fmadd_ps(xm2,
						                _mm256_div_ps(xnum,xden),d2));
			    }
			    else if(_mm256_cmp_ps_mask(x,_12,_CMP_LE_OQ)) {
                                   xm4  = _mm256_sub_ps(x,_4);
				   xden = ymm8r4_negate(_1);
				   xnum = _0;
#if defined(__GNUC__) && !defined(__INTEL_COMPILER)				   
				   xnum = _mm256_fmadd_ps(xnum,xm4,*(__m256*)&gamma_log_ymm8r4_p4[0]);
				   xden = _mm256_fmadd_ps(xden,xm4,*(__m256*)&gamma_log_ymm8r4_q4[0]);
				   xnum = _mm256_fmadd_ps(xnum,xm4,*(__m256*)&gamma_log_ymm8r4_p4[8]);
				   xden = _mm256_fmadd_ps(xden,xm4,*(__m256*)&gamma_log_ymm8r4_q4[8]);
				   xnum = _mm256_fmadd_ps(xnum,xm4,*(__m256*)&gamma_log_ymm8r4_p4[16]);
				   xden = _mm256_fmadd_ps(xden,xm4,*(__m256*)&gamma_log_ymm8r4_q4[16]);
				   xnum = _mm256_fmadd_ps(xnum,xm4,*(__m256*)&gamma_log_ymm8r4_p4[24]);
				   xden = _mm256_fmadd_ps(xden,xm4,*(__m256*)&gamma_log_ymm8r4_q4[24]);
				   xnum = _mm256_fmadd_ps(xnum,xm4,*(__m256*)&gamma_log_ymm8r4_p4[32]);
				   xden = _mm256_fmadd_ps(xden,xm4,*(__m256*)&gamma_log_ymm8r4_q4[32]);
				   xnum = _mm256_fmadd_ps(xnum,xm4,*(__m256*)&gamma_log_ymm8r4_p4[40]);
				   xden = _mm256_fmadd_ps(xden,xm4,*(__m256*)&gamma_log_ymm8r4_q4[40]);
				   xnum = _mm256_fmadd_ps(xnum,xm4,*(__m256*)&gamma_log_ymm8r4_p4[48]);
				   xden = _mm256_fmadd_ps(xden,xm4,*(__m256*)&gamma_log_ymm8r4_q4[48]);
				   xnum = _mm256_fmadd_ps(xnum,xm4,*(__m256*)&gamma_log_ymm8r4_p4[56]);
				   xden = _mm256_fmadd_ps(xden,xm4,*(__m256*)&gamma_log_ymm8r4_q4[56]);
#elif !defined(__GNUC__) && defined(__INTEL_COMPILER)
                                   xnum = _mm256_fmadd_ps(xnum,xm4,gamma_log_ymm8r4_p4[0]);
				   xden = _mm256_fmadd_ps(xden,xm4,gamma_log_ymm8r4_q4[0]);
				   xnum = _mm256_fmadd_ps(xnum,xm4,gamma_log_ymm8r4_p4[1]);
				   xden = _mm256_fmadd_ps(xden,xm4,gamma_log_ymm8r4_q4[1]);
				   xnum = _mm256_fmadd_ps(xnum,xm4,gamma_log_ymm8r4_p4[2]);
				   xden = _mm256_fmadd_ps(xden,xm4,gamma_log_ymm8r4_q4[2]);
				   xnum = _mm256_fmadd_ps(xnum,xm4,gamma_log_ymm8r4_p4[3]);
				   xden = _mm256_fmadd_ps(xden,xm4,gamma_log_ymm8r4_q4[3]);
				   xnum = _mm256_fmadd_ps(xnum,xm4,gamma_log_ymm8r4_p4[4]);
				   xden = _mm256_fmadd_ps(xden,xm4,gamma_log_ymm8r4_q4[4]);
				   xnum = _mm256_fmadd_ps(xnum,xm4,gamma_log_ymm8r4_p4[5]);
				   xden = _mm256_fmadd_ps(xden,xm4,gamma_log_ymm8r4_q4[5]);
				   xnum = _mm256_fmadd_ps(xnum,xm4,gamma_log_ymm8r4_p4[6]);
				   xden = _mm256_fmadd_ps(xden,xm4,gamma_log_ymm8r4_q4[6]);
				   xnum = _mm256_fmadd_ps(xnum,xm4,gamma_log_ymm8r4_p4[7]);
				   xden = _mm256_fmadd_ps(xden,xm4,gamma_log_ymm8r4_q4[7]);
#endif				   
				   res  = _mm256_fmadd_ps(xm4,_mm256_div_ps(xnum,xden),d4);
			    }
			    else {
                                   res  = _0;
				   if(_mm256_cmp_ps_mask(x,frtbig,_CMP_LE_OQ)) {
#if defined(__GNUC__) && !defined(__INTEL_COMPILER)				   
                                      res = *(__m256*)&gamma_log_ymm8r4_c[48];
				      xsq = _mm256_mul_ps(x,x);
				      res = _mm256_add_ps(_mm256_div_ps(res,xsq),*(__m256*)&gamma_log_ymm8r4_c[0]);
				      res = _mm256_add_ps(_mm256_div_ps(res,xsq),*(__m256*)&gamma_log_ymm8r4_c[8]);
				      res = _mm256_add_ps(_mm256_div_ps(res,xsq),*(__m256*)&gamma_log_ymm8r4_c[16]);
				      res = _mm256_add_ps(_mm256_div_ps(res,xsq),*(__m256*)&gamma_log_ymm8r4_c[24]);
				      res = _mm256_add_ps(_mm256_div_ps(res,xsq),*(__m256*)&gamma_log_ymm8r4_c[32]);
				      res = _mm256_add_ps(_mm256_div_ps(res,xsq),*(__m256*)&gamma_log_ymm8r4_c[40]);
#elif !defined(__GNUC__) && defined(__INTEL_COMPILER)
                                      res = gamma_log_ymm8r4_c[6];
				      xsq = _mm256_mul_ps(x,x);
				      res = _mm256_add_ps(_mm256_div_ps(res,xsq),gamma_log_ymm8r4_c[0]);
				      res = _mm256_add_ps(_mm256_div_ps(res,xsq),gamma_log_ymm8r4_c[1]);
				      res = _mm256_add_ps(_mm256_div_ps(res,xsq),gamma_log_ymm8r4_c[2]);
				      res = _mm256_add_ps(_mm256_div_ps(res,xsq),gamma_log_ymm8r4_c[3]);
				      res = _mm256_add_ps(_mm256_div_ps(res,xsq),gamma_log_ymm8r4_c[4]);
				      res = _mm256_add_ps(_mm256_div_ps(res,xsq),gamma_log_ymm8r4_c[5]);
#endif				      
				   }
                                   res  = _mm256_div_ps(res,x);
				   corr = _mm256_log_ps(x);
				   res  = _mm256_sub_ps(_mm256_add_ps(res,sqrtpi),
				                        _mm256_mul_ps(_1_2,corr));
				   res  = _mm256_fmadd_ps(x,_mm256_sub_ps(corr,_1),res);
				   
			    }

			    gamlog = res;
			    return (gamlog);
			    
		  }
		  

/*
!*****************************************************************************80
!
!! GAMMA_INC computes the incomplete Gamma function.
!
!  Discussion:
!
!    GAMMA_INC(P,       0) = 0,
!    GAMMA_INC(P,Infinity) = 1.
!
!    GAMMA_INC(P,X) = Integral ( 0 <= T <= X ) T**(P-1) EXP(-T) DT / GAMMA(P).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 May 2001
!
!  Author:
!
!    Original FORTRAN77 version by B L Shea.
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    BL Shea,
!    Chi-squared and Incomplete Gamma Integral,
!    Algorithm AS239,
!    Applied Statistics,
!    Volume 37, Number 3, 1988, pages 466-473.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) P, the exponent parameter.
!    0.0D+00 < P.
!
!    Input, real ( kind = 8 ) X, the integral limit parameter.
!    If X is less than or equal to 0, GAMMA_INC is returned as 0.
!
!    Output, real ( kind = 8 ) GAMMA_INC, the value of the function.
!
*/		
	  
		  __m256d  
                      gamma_incomplete_ymm4r8(const __m256d p,
                                              const __m256d x) {
                                              
                            const __m256d exp_arg_min = _mm256_set1_pd(-88.0e+00);
                            const __m256d overflow    = _mm256_set1_pd(1.0e+37);
                            const __m256d plimit      = _mm256_set1_pd(1000.0e+00);
                            const __m256d tol         = _mm256_set1_pd(1.0e-7);
                            const __m256d xbig        = _mm256_set1_pd(1.0e+8);
                            const __m256d C0          = _mm256_setzero_pd();
                            const __m256d C0333333333 = _mm256_set1_pd(0.3333333333333333333333);
                            const __m256d C1          = _mm256_set1_pd(1.0);
                            const __m256d C2          = _mm256_set1_pd(2.0);
                            const __m256d C3          = _mm256_set1_pd(3.0);
                            const __m256d C9          = _mm256_set1_pd(9.0);
                            __m256d cdf,arg,b,c;
                            __m256d pn1,pn2,pn3,pn4;
                            __m256d pn5,pn6,rn,t0,t1;
                            __m256d gaminc;
                            __mmask8 m0,m1;
                            m0 = _mm256_cmp_pd_mask(plimit,p,_CMP_LT_OQ);
                            if(m0) {
                               __m256d sqrp,xp,_9p1,t0,t1;
                               xp     = _mm256_div_pd(x,p);
                               _9p1   = _mm256_fmsub_pd(C9,p,C1);
                               sqrp   = _mm256_mul_pd(C3,_mm256_sqrt_pd(p));
                               t0     = _mm256_pow_pd(xp,C0333333333);
                               t1     = _mm256_add_pd(t0,
                                            _mm256_div_pd(C1,_9p1));
                               pn1    = _mm256_mul_pd(sqrp,t1);
                               gaminc = normal_01_cdf_ymm4r8(pn1);
                               return (gaminc);
                            }   
                            m0 = _mm256_cmp_pd_mask(x,C1,_CMP_LE_OQ);
                            m1 = _mm256_cmp_pd_mask(x,p,_CMP_LT_OQ);
                            if(m0 || m1) {

                               t0  = _mm256_log_pd(x);
                          
                               t1  = gamma_log_ymm4r8(_mm256_add_pd(p,C1));
                               arg = _mm256_fmsub_pd(p,t0,_mm256_sub_pd(x,t1));
                               c   = C1;
                               gaminc = C1;
                               a   = p; 
                               while(true) {
                                    a      = _mm256_add_pd(a,C1);
                                    c      = _mm256_mul_pd(c,_mm256_div_pd(x,a));
                                    gaminc = _mm256_add_pd(gaminc,c);
                                    m0     = _mm256_cmp_pd_mask(c,tol,_CMP_LE_OQ);
                                    if(m0) break;
                               }

                               t0  = _mm256_log_pd(x);
                               arg = _mm256_add_pd(arg,t0);  
                               m1  = _mm256_cmp_pd_mask(exp_arg_min,arg,_CMP_LE_OQ);
                               gaminc = _mm256_mask_blend_pd(m1,C0,_mm256_exp_pd(arg));  
                                
                           } 
                           else {

                               t0  = _mm256_log_pd(x);
                             
                               t1  = gamma_log_ymm4r8(p);
                               arg = _mm256_fmsub_pd(p,t0,_mm256_sub_pd(x,t1));                               
                               a   = _mm256_sub_pd(C1,p);
                               b   = _mm256_add_pd(a,_mm256_add_pd(x,C1));
                               c   = C0;
                               pn1 = C1;
                               pn2 = x;
                               pn3 = _mm256_add_pd(x,C1);
                               pn4 = _mm256_mul_pd(x,b);
                               gaminc = _mm256_div_pd(pn3,pn4);
                               while(true) {
                                   a = _mm256_add_pd(a,C1);
                                   b = _mm256_add_pd(b,C2);
                                   c = _mm256_add_pd(c,C1);
                                   pn5 = _mm256_fmsub_pd(b,pn3,
                                                     _mm256_mul_pd(a,
                                                           _mm256_mul_pd(c,pn1)));
                                   pn6 = _mm256_fmsub_pd(b,pn4,
                                                     _mm256_mul_pd(a,
                                                           _mm256_mul_pd(c,pn2)));
                                   if(_mm256_cmp_pd_mask(C0,_mm256_abs_pd(pn6),
                                                                       _CMP_LT_OQ)) {
                                        rn = _mm256_div_pd(pn5,pn6);
                                        t0 = _mm256_abs_pd(_mm256_sub_pd(gaminc,rn));
                                        t1 = _mm256_min_pd(tol,_mm256_mul_pd(tol,rn));
                                        if(_mm256_cmp_pd_mask(t0,t1,_CMP_LE_OQ)) {
                                           arg  = _mm256_add_pd(_mm256_log_pd(gaminc));       
                                           m1   = _mm256_cmp_pd_mask(exp_arg_min,arg,_CMP_LE_OQ);
                                           gaminc = _mm256_mask_blend_pd(m1,C1,_mm256_sub_pd(C1,
                                                                                    _mm256_exp_pd(arg)));
      
                                           return (gaminc);                               
                                        }    
                                        gaminc = rn;                               
                                   }
                                   pn1 = pn3;
                                   pn2 = pn4;
                                   pn3 = pn5;
                                   pn4 = pn6;
                                   if(_mm256_cmp_pd_mask(overflow,
                                                   _mm256_abs_pd(pn5),_CMP_LE_OQ)) {
                                      t0 = _mm256_div_pd(C1,overflow);
                                      pn1= _mm256_mul_pd(pn1,t0);
                                      pn2= _mm256_mul_pd(pn2,t0);
                                      pn3= _mm256_mul_pd(pn3,t0);
                                      pn4= _mm256_mul_pd(pn4,t0);               
                                   }
                               }
                           } 
                           
                           return (gaminc);                   
                   }	
                   
                   
       /*
    !*****************************************************************************80
!
!! R8POLY_VALUE evaluates an R8POLY
!
!  Discussion:
!
!    For sanity's sake, the value of N indicates the NUMBER of
!    coefficients, or more precisely, the ORDER of the polynomial,
!    rather than the DEGREE of the polynomial.  The two quantities
!    differ by 1, but cause a great deal of confusion.
!
!    Given N and A, the form of the polynomial is:
!
!      p(x) = a(1) + a(2) * x + ... + a(n-1) * x^(n-2) + a(n) * x^(n-1)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 August 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the polynomial.
!
!    Input, real ( kind = 8 ) A(N), the coefficients of the polynomial.
!    A(1) is the constant term.
!
!    Input, real ( kind = 8 ) X, the point at which the polynomial is
!    to be evaluated.
!
!    Output, real ( kind = 8 ) R8POLY_VALUE, the value of the polynomial at X.
!     
*/


                      __m256d  
		      vpoly_eval_ymm4r8(const int32_t n,
		                        const __m256d * __restrict __attributes__((aligned(32))) a,
		                        const __m256d x) {
		         
		         register __m256d vpoly;
		         vpoly = _mm256_load_pd(&a[n]);
		         for(int32_t i=n; i != 0; --i) {
		             register __m256d t0 = a[i];
		             vpoly = _mm256_fmadd_pd(vpoly,x,t0);   
		         }  
		         return (vpoly);              
		    } 
		    
		     
		      __m256d  
		      vpoly_eval_ymm4r8_v2(const int32_t n,
		                        const __m256d * __restrict __attribute__((aligned(32))) a,
		                        const __m256d x) {
		         
		         register __m256d vpoly;
		         vpoly = _mm512_load_pd(&a[n-4]);
		         for(int32_t i=n-4; i != 0; i -= 4) {
		             register __m256d t0 = __mm256_load_pd(&a[i]);
		             vpoly = _mm256_fmadd_pd(vpoly,x,t0);   
		         }  
		         return (vpoly);              
		    } 
		    
		    
		     __m256  
		      vpoly_eval_ymm8r4(const int32_t n,
		                        const __m256 * __restrict __attributes__((aligned(32)))  a,
		                        const __m256 x) {
		         
		         register __m256 vpoly;
		         vpoly = _mm256_load_ps(&a[n]);
		         for(int32_t i=n; i != 0; --i) {
		             register __m256 t0 = a[i];
		             vpoly = _mm256_fmadd_ps(vpoly,x,t0);   
		         }  
		         return (vpoly);              
		    }	
		    
		    
		      __m256
		     vpoly_eval_ymm8r4_v2(const int32_t n,
		                        const __m256 * __restrict __attribute__((aligned(32))) a,
		                        const __m256 x) {
		         
		         register __m256 vpoly;
		         vpoly = _mm256_load_ps(&a[n-8]);
		         for(int32_t i=n-8; i != 0; i -= 8) {
		             register __m256 t0 = __mm256_load_ps(&a[i]);
		             vpoly = _mm256_fmadd_ps(vpoly,x,t0);   
		         }  
		         return (vpoly);              
		    }
		    
		    
/*

       !*****************************************************************************80
!
!! NORMAL_01_CDF_INV inverts the standard normal CDF.
!
!  Discussion:
!
!    The result is accurate to about 1 part in 10**16.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 June 2007
!
!  Author:
!
!    Original FORTRAN77 version by Michael Wichura.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Michael Wichura,
!    Algorithm AS241:
!    The Percentage Points of the Normal Distribution,
!    Applied Statistics,
!    Volume 37, Number 3, pages 477-484, 1988.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) P, the value of the cumulative probability
!    densitity function.  0 < P < 1.  If P is outside this range, an
!    "infinite" value will be returned.
!
!    Output, real ( kind = 8 ) X, the normal deviate value
!    with the property that the probability of a standard normal deviate being
!    less than or equal to the value is P.    

*/ 


                      __m256d    
		      normal_01_cdf_inv_ymm4r8(const __m256d p) {
		           
                          __m256d const1 = _mm256_set1_pd(0.180625e+00);
                          __m256d const2 = _mm256_set1_pd(1.6e+00);
                          __m256d split1 = _mm256_set1_pd(0.425e+00);
                          __m256d split2 = _mm256_set1_pd(5.0e+00);
                          __m256d C0     = _mm256_setzero_pd();
                          __m256d C1     = _mm256_set1_pd(1.0);
                          __m256d C05    = _mm256_set1_pd(0.5);
                          register __m256d q,r,t0,t1;
                          register __m256d x;
                          q = _mm256_sub_pd(p,C05);
                          if(_mm256_cmp_pd_mask(q,split1,_CMP_LE_OQ)) {
                             r = _mm256_sub_pd(const1,_mm256_mul_pd(q,q));
#if defined(__GNUC__) && !defined(__INTEL_COMPILER)                             
                             t0= vpoly_eval_ymm4r8_v2(32,normal_01_cdf_inv_ymm4r8_a,r);
                             t1= vpoly_eval_ymm4r8_v2(32,normal_01_cdf_inv_ymm4r8_b,r);
#elif !defined(__GNUC__) && defined(__INTEL_COMPILER)
                             t0= vpoly_eval_ymm4r8(8,normal_01_cdf_inv_ymm4r8_a,r);
                             t1= vpoly_eval_ymm4r8(8,normal_01_cdf_inv_ymm4r8_b,r);
#endif                             
                             x = _mm256_div_pd(_mm256_mul_pd(q,t0),t1);
                          } 
                          else {
                             const __mmask8 m = _mm256_cmp_pd_mask(q,C0,_CMP_LT_OQ);
                             r                = _mm256_mask_blend_pd(m,_mm256_sub_pd(C1,p),p);
                             if(_mm256_cmp_pd_mask(r,C0,_CMP_LE_OQ)) {
                                x = _mm256_set1_pd(DBL_MAX);
                             }
                             else {

                                r = _mm256_sqrt_pd(negate_ymm4r8(_mm256_log_pd(r)));
                        
                                const __mmask8 m = _mm256_cmp_pd_mask(r,split2,_CMP_LE_OQ);
                                r                = _mm256_mask_blend_pd(m,_mm256_sub_pd(r,split2),
                                                                          _mm256_sub_pd(r,const2));
#if defined(__GNUC__) && !defined(__INTEL_COMPILER)                                                                            
                                t0               = _mm256_div_pd(vpoly_eval_ymm4r8_v2(32,normal_01_cdf_inv_ymm4r8_c,r),
                                                                 vpoly_eval_ymm4r8_v2(32,normal_01_cdf_inv_ymm4r8_d,r));
                                t1               = _mm256_div_pd(vpoly_eval_ymm4r8_v2(32,normal_01_cdf_inv_ymm4r8_e,r),
                                                                 vpoly_eval_ymm4r8_v2(32,normal_01_cdf_inv_ymm4r8_f,r));
#elif !defined(__GNUC__) && defined(__INTEL_COMPILER)
                                t0               = _mm256_div_pd(vpoly_eval_ymm4r8(8,normal_01_cdf_inv_ymm4r8_c,r),
                                                                 vpoly_eval_ymm4r8(8,normal_01_cdf_inv_ymm4r8_d,r));
                                t1               = _mm256_div_pd(vpoly_eval_ymm4r8(8,normal_01_cdf_inv_ymm4r8_e,r),
                                                                 vpoly_eval_ymm4r8(8,normal_01_cdf_inv_ymm4r8_f,r));
#endif                                                                 
                                x                = _mm256_mask_blend_pd(m,t1,t0);      
                             }
                             if(_mm256_cmp_pd_mask(q,C0,_CMP_LT_OQ)) x = negate_ymm4r8(x);
                          }
                          return (x);
                          
		    }
		    
		    
		       __m256    
		      normal_01_cdf_inv_ymm8r4(const __m256 p) {
		          
                          __m256 const1 = _mm256_set1_ps(0.180625e+00f);
                          __m256 const2 = _mm256_set1_ps(1.6e+00f);
                          __m256 split1 = _mm256_set1_ps(0.425e+00f);
                          __m256 split2 = _mm256_set1_ps(5.0e+00f);
                          __m256 C0     = _mm256_setzero_ps();
                          __m256 C1     = _mm256_set1_ps(1.0f);
                          __m256 C05    = _mm256_set1_ps(0.5f);
                          register __m256 q,r,t0,t1;
                          register __m256 x;
                          q = _mm256_sub_ps(p,C05);
                          if(_mm256_cmp_ps_mask(q,split1,_CMP_LE_OQ)) {
                             r = _mm256_sub_ps(const1,_mm256_mul_ps(q,q));
#if defined(__GNUC__) && !defined(__INTEL_COMPILER)                             
                             t0= vpoly_eval_ymm8r4_v2(64,normal_01_cdf_inv_ymm8r4_a,r);
                             t1= vpoly_eval_ymm8r4_v2(64,normal_01_cdf_inv_ymm8r4_b,r);
#elif !defined(__GNUC__) && defined(__INTEL_COMPILER)
                             t0= vpoly_eval_ymm8r4(8,normal_01_cdf_inv_ymm8r4_a,r);
                             t1= vpoly_eval_ymm8r4(8,normal_01_cdf_inv_ymm8r4_b,r);
#endif                           
                             x = _mm256_div_ps(_mm256_mul_ps(q,t0),t1);
                          } 
                          else {
                             const __mmask16 m = _mm256_cmp_ps_mask(q,C0,_CMP_LT_OQ);
                             r                = _mm256_mask_blend_ps(m,_mm256_sub_ps(C1,p),p);
                             if(_mm256_cmp_ps_mask(r,C0,_CMP_LE_OQ)) {
                                x = _mm256_set1_pd(FLT_MAX);
                             }
                             else {

                                r = _mm256_sqrt_ps(negate_zmm16r4(_mm256_log_ps(r)));
                       
                                const __mmask8 m = _mm256_cmp_ps_mask(r,split2,_CMP_LE_OQ);
                                r                = _mm256_mask_blend_ps(m,_mm256_sub_ps(r,split2),
                                                                          _mm256_sub_ps(r,const2));
#if defined(__GNUC__) && !defined(__INTEL_COMPILER)                                                                           
                                t0               = _mm256_div_ps(vpoly_eval_ymm8r4_v2(64,normal_01_cdf_inv_ymm8r4_c,r),
                                                                 vpoly_eval_ymm8r4_v2(64,normal_01_cdf_inv_ymm8r4_d,r));
                                t1               = _mm256_div_ps(vpoly_eval_ymm8r4_v2(64,normal_01_cdf_inv_ymm8r4_e,r),
                                                                 vpoly_eval_ymm8r4_v2(64,normal_01_cdf_inv_ymm8r4_f,r));
#elif !defined(__GNUC__) && defined(__INTEL_COMPILER)
                                t0               = _mm256_div_ps(vpoly_eval_ymm8r4(8,normal_01_cdf_inv_ymm8r4_c,r),
                                                                 vpoly_eval_ymm8r4(8,normal_01_cdf_inv_ymm8r4_d,r));
                                t1               = _mm256_div_ps(vpoly_eval_ymm8r4(8,normal_01_cdf_inv_ymm8r4_e,r),
                                                                 vpoly_eval_ymm8r4(8,normal_01_cdf_inv_ymm8r4_f,r));
#endif                                                                 
                                x                = _mm256_mask_blend_ps(m,t1,t0);      
                             }
                             if(_mm256_cmp_ps_mask(q,C0,_CMP_LT_OQ)) x = negate_ymm8r4(x);
                          }
                          return (x);
                          
		    }
		    
		    
/*
       !*****************************************************************************80
!
!! RECIPROCAL_CDF evaluates the Reciprocal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0D+00 < A <= B.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
*/


               
		    
		   
		      __m256d 
		      reciprocal_cdf_ymm4r8(const __m256d x,
		                            const __m256d a,
		                            const __m256d b) {
		          
		          register __m256d ax,ab,l1,l2;
		          register __m256d cdf;       
		          ax = _mm256_div_pd(a,x);
                          l1 = _mm256_log_pd(ax);
	                  ab = _mm256_div_pd(a,b);
                          l2 = _mm256_log_pd(ab);
                          cdf= _mm256_div_pd(l1,l2);
                          return (cdf);
		     }
		     
		     
		    
		      __m256 
		      reciprocal_cdf_ymm8r4(const __m256 x,
		                             const __m256 a,
		                             const __m256 b) {
		          
		          register __m256 ax,ab,l1,l2;
		          register __m256 cdf;       
		          ax = _mm256_div_ps(a,x);
                          l1 = _mm256_log_ps(ax);
	                  ab = _mm256_div_ps(a,b);
                          l2 = _mm256_log_ps(ab);
                          cdf= _mm256_div_ps(l1,l2);
                          return (cdf);
		     }
		     
		     
/*
          !*****************************************************************************80
!
!! RECIPROCAL_CDF_INV inverts the Reciprocal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0D+00 < A <= B.
!
!    Output, real ( kind = 8 ) X, the corresponding argument of the CDF.
!                      
*/


                      __m256d 		     
		      reciprocal_cdf_inv_ymm4r8(const __m256d cdf,
		                                const __m256d a,
		                                const __m256d b) {
		         
		           register __m256d C1 = _mm256_set1_pd(1.0);
		           register __m256d pow1,pow2,cdf1;
		           register __m256d inv;
		           cdf1 = _mm256_sub_pd(cdf,C1);
		           pow2 = _mm256_pow_pd(b,cdf);
		           pow1 = _mm256_pow_pd(a,cdf1);
		           inv  = _mm256_div_pd(pow2,pow1);
		           return (inv);                          
		     }
		     
		     
		  
		      __m256		     
		      reciprocal_cdf_inv_ymm8r4(const __m256 cdf,
		                                const __m256 a,
		                                const __m256 b) {
		         
		           register __m256 C1 = _mm256_set1_ps(1.0f);
		           register __m256 pow1,pow2,cdf1;
		           register __m256 inv;
		           cdf1 = _mm256_sub_ps(cdf,C1);
		           pow2 = _mm256_pow_ps(b,cdf);
		           pow1 = _mm256_pow_ps(a,cdf1);
		           inv  = _mm256_div_ps(pow2,pow1);
		           return (inv);                          
		     }  
		     
		     
/*
     !*****************************************************************************80
!
!! RECIPROCAL_MEAN returns the mean of the Reciprocal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0D+00 < A <= B.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!    
*/


                      __m256d 
		      reciprocal_mean_ymm4r8(const __m256d a,
		                             const __m256d b) {
		           
		           register __m256d ab,amb,l1;
		           register __m256d mean;
		           amb = _mm256_sub_pd(a,b);
		           ab  = _mm256_div_pd(a,b);
                           l1  = _mm256_log_pd(ab);
	                   mean= _mm256_div_pd(amb,l1);
                           return (mean);
		     }	 
		     
		     
		     
		      __m256 
		      reciprocal_mean_ymm8r4(const __m256 a,
		                             const __m256 b) {
		           
		           register __m256 ab,amb,l1;
		           register __m256 mean;
		           amb = _mm256_sub_ps(a,b);
		           ab  = _mm256_div_ps(a,b);
                           l1  = _mm256_log_ps(ab);
		           mean= _mm256_div_ps(amb,l1);
                           return (mean);
		     }	 
		     		    
		    
/*
           !*****************************************************************************80
!
!! RECIPROCAL_PDF evaluates the Reciprocal PDF.
!
!  Discussion:
!
!    PDF(A,B;X) = 1.0D+00 / ( X * LOG ( B / A ) )
!    for 0.0D+00 <= X
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0D+00 < A <= B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!                
*/


                    
		      __m256d
		      reciprocal_pdf_ymm4r8(    const __m256d x,
		                                const __m256d a,
		                                const __m256d b) {
		          
		          register __m256d C1 = _mm256_set1_pd(1.0);
		          register __m256d ba,l1;
		          register __m256d pdf;
		          ba = _mm256_div_pd(b,a);
                          l1 = _mm256_mul_pd(x,_mm256_log_pd(ba));
	                  pdf= _mm256_div_pd(C1,l1);
                          return (pdf);                            
		    }
		    
		    
		    
		   
		      __m256
		      reciprocal_pdf_ymm8r4(    const __m256 x,
		                                const __m256 a,
		                                const __m256 b) {
		          
		          register __m256 C1 = _mm256_set1_ps(1.0f);
		          register __m256 ba,l1;
		          register __m256 pdf;
		          ba = _mm256_div_ps(b,a);
                          l1 = _mm256_mul_ps(x,_mm256_log_ps(ba));
	                  pdf= _mm256_div_ps(C1,l1);
                          return (pdf);                            
		    }
		    		           
                    
/*
 
    !*****************************************************************************80
!
!! CHI_CDF evaluates the Chi CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0 < B,
!    0 < C.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!                
   
*/

                    
		      __m256d
		      chi_cdf_ymm4r8(const __m256d x,
		                     const __m256d a,
		                     const __m256d b,
		                     const __m256d c) {
		       
		         const __m256d C05 = _mm256_set1_pd(0.5);
		         __m256d p2,x2,y;
		         __m256d cdf;
		         y  = _mm256_div_pd(_mm256_sub_pd(x,a),b);
		         x2 = _mm256_mul_pd(C05,_mm256_mul_pd(y,y));
		         p2 = _mm256_mul_pd(C05,c);
		         cdf= gamma_incomplete_ymm4r8(p2,x2);
		         return (cdf);               
		   }
		   
		   
		    
		      __m256
		      chi_cdf_ymm8r4(const __m256 x,
		                      const __m256 a,
		                      const __m256 b,
		                      const __m256 c) {
		       
		         const __m256 C05 = _mm256_set1_ps(0.5f);
		         __m256 p2,x2,y;
		         __m256 cdf;
		         y  = _mm256_div_ps(_mm256_sub_ps(x,a),b);
		         x2 = _mm256_mul_ps(C05,_mm256_mul_ps(y,y));
		         p2 = _mm256_mul_ps(C05,c);
		         cdf= gamma_incomplete_ymm8r4(p2,x2);
		         return (cdf);               
		   }
		   
		  
/*
   
     !*****************************************************************************80
!
!! CHI_SQUARE_CDF evaluates the Chi squared CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 October 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the value of the random deviate.
!
!    Input, real ( kind = 8 ) A, the parameter of the distribution, usually
!    the number of degrees of freedom.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!

*/


                     
		      __m256d
		      chi_square_cdf_ymm4r8(const __m256d x,
		                            const __m256d a) {
		          
		          const __m256d C05 = _mm256_set1_pd(0.5);
		          const __m256d C1  = _mm256_set1_pd(1.0);
		          register __m256d x2,a2,b2,c2;
		          register __m256d cdf;
		          a2 =  _mm256_setzero_pd();
		          x2 =  _mm256_mul_pd(C05,C1);
		          b2 =  C1;
		          c2 =  _mm256_mul_pd(C05,a);
		          cdf=  gamma_cdf_ymm4r8(x2,a2,b2,c2);
		          return (cdf);                   
		     }
		     
		     
		    
		      __m256
		      chi_square_cdf_ymm8r4(const __m256 x,
		                             const __m256 a) {
		          
		          const __m256 C05 = _mm256_set1_ps(0.5f);
		          const __m256 C1  = _mm256_set1_ps(1.0f);
		          register __m256 x2,a2,b2,c2;
		          register __m256 cdf;
		          a2 =  _mm256_setzero_ps();
		          x2 =  _mm256_mul_ps(C05,C1);
		          b2 =  C1;
		          c2 =  _mm256_mul_ps(C05,a);
		          cdf=  gamma_cdf_ymm8r4(x2,a2,b2,c2);
		          return (cdf);                   
		     }
		     
		     
/*
    
       !*****************************************************************************80
!
!! CHI_MEAN returns the mean of the Chi PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0 < B,
!    0 < C.
!
!    Output, real ( kind = 8 ) MEAN, the mean value.
!   

*/


                    
		      __m256d
                      chi_mean_ymm4r8(const __m256d a,
                                      const __m256d b,
                                      const __m256d c) {
                          
                          const __m256d C05 = _mm256_set1_pd(0.5);
                          const __m256d C1  = _mm256_set1_pd(1.0);
                          const __m256d C2  = _mm256_set1_pd(2.0);
                          const __m256d C141421356237309504880169 = 
                                              _mm256_set1_pd(1.41421356237309504880169);
                          register __m256d g0,g1,arg,t0;
                          register __m256d mean;
                          arg = _mm256_mul_pd(C05,_mm256_add_pd(c,C1));
                          g0  = gamma_ymm4r8(arg);
                          t0  = _mm256_mul_pd(C141421356237309504880169,b);
                          g1  = gamma_ymm4r8(_mm256_mul_pd(C05,c));
                          mean= _mm256_add_pd(a,_mm256_div_pd(_mm256_mul_pd(t0,g0),g1));
                          return (mean);
                    }	
                    
                    
                    
		      __m256
                      chi_mean_ymm8r4(const __m256 a,
                                      const __m256 b,
                                      const __m256 c) {
                          
                          const __m256 C05 = _mm256_set1_ps(0.5f);
                          const __m256 C1  = _mm256_set1_ps(1.0f);
                          const __m256 C2  = _mm256_set1_ps(2.0f);
                          const __m256 C141421356237309504880169 = 
                                              _mm256_set1_ps(1.41421356237309504880169f);
                          register __m256 g0,g1,arg,t0;
                          register __m256 mean;
                          arg = _mm256_mul_ps(C05,_mm256_add_pd(c,C1));
                          g0  = gamma_ymm8r4(arg);
                          t0  = _mm256_mul_ps(C141421356237309504880169,b);
                          g1  = gamma_ymm8r4(_mm256_mul_ps(C05,c));
                          mean= _mm256_add_ps(a,_mm256_div_ps(_mm256_mul_ps(t0,g0),g1));
                          return (mean);
                    }	   
                    
		      
/*
 !*****************************************************************************80
!
!! BESSEL_I0 evaluates the modified Bessel function I0(X).
!
!  Discussion:
!
!    The main computation evaluates slightly modified forms of
!    minimax approximations generated by Blair and Edwards, Chalk
!    River (Atomic Energy of Canada Limited) Report AECL-4928,
!    October, 1974.  This transportable program is patterned after
!    the machine dependent FUNPACK packet NATSI0, but cannot match
!    that version for efficiency or accuracy.  This version uses
!    rational functions that theoretically approximate I-SUB-0(X)
!    to at least 18 significant decimal digits.
!
!  Machine dependent constants:
!
!    beta   = Radix for the floating-point system
!    maxexp = Smallest power of beta that overflows
!    XMAX =   Largest argument acceptable to BESI0;  Solution to
!             equation:
!               W(X) * (1+1/(8*X)+9/(128*X^2) = beta^maxexp
!             where  W(X) = EXP(X)/sqrt(2*PI*X)
!
!    Approximate values for some important machines are:
!
!                             beta       maxexp       XMAX
!
!    CRAY-1        (S.P.)       2         8191       5682.810
!    Cyber 180/855
!      under NOS   (S.P.)       2         1070        745.893
!    IEEE (IBM/XT,
!      SUN, etc.)  (S.P.)       2          128         91.900
!    IEEE (IBM/XT,
!      SUN, etc.)  (D.P.)       2         1024        713.986
!    IBM 3033      (D.P.)      16           63        178.182
!    VAX           (S.P.)       2          127         91.203
!    VAX D-Format  (D.P.)       2          127         91.203
!    VAX G-Format  (D.P.)       2         1023        713.293
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 October 2008
!
!  Author:
!
!    Original FORTRAN77 version by William Cody, Laura Stoltz.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) ARG, the argument.
!
!    Output, real ( kind = 8 ) BESSEL_I0, the value of the modified
!    Bessel function of the first kind.
!
*/


                    
		      __m256d  bessesl_i0_ymm4r8(const __m256d arg) {
                          
			    const __m256d rec15                    =  _mm256_set1_pd(6.6666666666666666666E-02);
			    const __m256d xmax                     =  _mm256_set1_pd(91.9E+00);
			    const __m256d exp40                    =  _mm256_set1_pd(2.353852668370199854E+17);
			    const __m256d _1                       =  _mm256_set1_pd(1.0);
			    const __m256d _15                      =  _mm256_set1_pd(15.0);
			    const __m256d _225                     =  _mm256_set1_pd(225.0);
			    const __m256d _40                      =  _mm256_set1_pd(40.0);
			    const __m256d eps                      =  _mm256_set1_pd(DBL_EPSILON);
			    const __m256d huge                     =  _mm256_set1_pd(DBL_MAX);
			    __m256d value,a,b,bessel_i0;
			    __m256d sump,sumq,x,xx;
                            x = _mm256_abs_pd(arg);
			    if(_mm256_cmp_pd_mask(x,eps,_CMP_LT_OQ)) {
                               value = _1;
			    }
			    else if(_mm256_cmp_pd_mask(x,_15,_CMP_LT_OQ)) {
                               xx   = _mm256_mul_pd(x,x);
#if defined(__GNUC__) && !defined(__INTEL_COMPILER)                                 
			       sump = *(__m256d*)&bessel_i0_ymm4r8_p[0];
			       sump = _mm256_fmadd_pd(sump,xx,*(__m256d*)&bessel_i0_ymm4r8_p[4]);
			       sump = _mm256_fmadd_pd(sump,xx,*(__m256d*)&bessel_i0_ymm4r8_p[8]);
			       sump = _mm256_fmadd_pd(sump,xx,*(__m256d*)&bessel_i0_ymm4r8_p[12]);
			       sump = _mm256_fmadd_pd(sump,xx,*(__m256d*)&bessel_i0_ymm4r8_p[16]);
			       sump = _mm256_fmadd_pd(sump,xx,*(__m256d*)&bessel_i0_ymm4r8_p[20]);
			       sump = _mm256_fmadd_pd(sump,xx,*(__m256d*)&bessel_i0_ymm4r8_p[24]);
			       sump = _mm256_fmadd_pd(sump,xx,*(__m256d*)&bessel_i0_ymm4r8_p[28]);
			       sump = _mm256_fmadd_pd(sump,xx,*(__m256d*)&bessel_i0_ymm4r8_p[32]);
			       sump = _mm256_fmadd_pd(sump,xx,*(__m256d*)&bessel_i0_ymm4r8_p[36]);
			       sump = _mm256_fmadd_pd(sump,xx,*(__m256d*)&bessel_i0_ymm4r8_p[40]);
			       sump = _mm256_fmadd_pd(sump,xx,*(__m256d*)&bessel_i0_ymm4r8_p[44]);
			       sump = _mm256_fmadd_pd(sump,xx,*(__m256d*)&bessel_i0_ymm4r8_p[48]);
			       sump = _mm256_fmadd_pd(sump,xx,*(__m256d*)&bessel_i0_ymm4r8_p[52]);
			       sump = _mm256_fmadd_pd(sump,xx,*(__m256d*)&bessel_i0_ymm4r8_p[56]);
			       xx   = _mm256_sub_pd(xx,_225);
			       const __m256d xxq0 = _mm256_add_pd(xx,*(__m256d*)&bessel_i0_ymm4r8_q[0]);
			       const __m256d xxq1 = _mm256_add_pd(xx,*(__m256d*)&bessel_i0_ymm4r8_q[4]);
			       const __m256d xxq2 = _mm256_add_pd(xx,*(__m256d*)&bessel_i0_ymm4r8_q[8]);
			       const __m256d xxq3 = _mm256_add_pd(xx,*(__m256d*)&bessel_i0_ymm4r8_q[12]);
			       const __m256d xxq4 = _mm256_add_pd(xx,*(__m256d*)&bessel_i0_ymm4r8_q[16]);
#elif !defined(__GNUC__) && defined(__INTEL_COMPILER)
                               sump = bessel_i0_ymm4r8_p[0];
			       sump = _mm256_fmadd_pd(sump,xx,bessel_i0_ymm4r8_p[1]);
			       sump = _mm256_fmadd_pd(sump,xx,bessel_i0_ymm4r8_p[2]);
			       sump = _mm256_fmadd_pd(sump,xx,bessel_i0_ymm4r8_p[3]);
			       sump = _mm256_fmadd_pd(sump,xx,bessel_i0_ymm4r8_p[4]);
			       sump = _mm256_fmadd_pd(sump,xx,bessel_i0_ymm4r8_p[5]);
			       sump = _mm256_fmadd_pd(sump,xx,bessel_i0_ymm4r8_p[6]);
			       sump = _mm256_fmadd_pd(sump,xx,bessel_i0_ymm4r8_p[7]);
			       sump = _mm256_fmadd_pd(sump,xx,bessel_i0_ymm4r8_p[8]);
			       sump = _mm256_fmadd_pd(sump,xx,bessel_i0_ymm4r8_p[9]);
			       sump = _mm256_fmadd_pd(sump,xx,bessel_i0_ymm4r8_p[10]);
			       sump = _mm256_fmadd_pd(sump,xx,bessel_i0_ymm4r8_p[11]);
			       sump = _mm256_fmadd_pd(sump,xx,bessel_i0_ymm4r8_p[12]);
			       sump = _mm256_fmadd_pd(sump,xx,bessel_i0_ymm4r8_p[13]);
			       sump = _mm256_fmadd_pd(sump,xx,bessel_i0_ymm4r8_p[14]);
			       xx   = _mm256_sub_pd(xx,_225);
			       const __m256d xxq0 = _mm256_add_pd(xx,bessel_i0_ymm4r8_q[0]);
			       const __m256d xxq1 = _mm256_add_pd(xx,bessel_i0_ymm4r8_q[1]);
			       const __m256d xxq2 = _mm256_add_pd(xx,bessel_i0_ymm4r8_q[2]);
			       const __m256d xxq3 = _mm256_add_pd(xx,bessel_i0_ymm4r8_q[3]);
			       const __m256d xxq4 = _mm256_add_pd(xx,bessel_i0_ymm4r8_q[4]);
#endif			       
			       sumq = _mm256_mul_pd(xxq0,
			                        _mm256_mul_pd(xxq1,
						          _mm256_mul_pd(xxq2,
							            _mm256_mul_pd(xxq3,xxq4))));
			       value = _mm256_div_pd(sump,sumq);
			                                         
			    }
			    else if(_mm256_cmp_pd_mask(_15,x,_CMP_LE_OQ)) {
                                    if(_mm256_cmp_pd_mask(xmax,x,_CMP_LT_OQ)) {
                                       value = huge;
				    }
				    else {
                                           xx = _mm256_sub_pd(_mm256_div_pd(_1,x),rec15);
#if defined(__GNUC__) && !defined(__INTEL_COMPILER)                                                
					   const __m256d t0 = _mm256_fmadd_pd(*(__m256d*)&bessel_i0_ymm4r8_pp[0],xx,*(__m256d*)&bessel_i0_ymm4r8_pp[4]);
					   const __m256d c0 = _mm256_fmadd_pd(_mm256_add_pd(xx,*(__m256d*)&bessel_i0_ymm4r8_qq[0]),xx,*(__m256d*)&bessel_i0_ymm4r8_qq[4]);
					   const __m256d t1 = _mm256_fmadd_pd(t0,xx,*(__m256d*)&bessel_i0_ymm4r8_pp[8]);
					   const __m256d c1 = _mm256_fmadd_pd(c0,xx,*(__m256d*)&bessel_i0_ymm4r8_qq[8]);
					   const __m256d t2 = _mm256_fmadd_pd(t1,xx,*(__m256d*)&bessel_i0_ymm4r8_pp[12]);
					   const __m256d c2 = _mm256_fmadd_pd(c1,xx,*(__m256d*)&bessel_i0_ymm4r8_qq[12]);
					   const __m256d t3 = _mm256_fmadd_pd(t2,xx,*(__m256d*)&bessel_i0_ymm4r8_pp[16]);
					   const __m256d c3 = _mm256_fmadd_pd(c2,xx,*(__m256d*)&bessel_i0_ymm4r8_qq[16]);
					   const __m256d t4 = _mm256_fmadd_pd(t3,xx,*(__m256d*)&bessel_i0_ymm4r8_pp[20]);
					   const __m256d c4 = _mm256_fmadd_pd(c3,xx,*(__m256d*)&bessel_i0_ymm4r8_qq[20]);
					   const __m256d t5 = _mm256_fmadd_pd(t4,xx,*(__m256d*)&bessel_i0_ymm4r8_pp[24]);
					   const __m256d c5 = _mm256_fmadd_pd(c4,xx,*(__m256d*)&bessel_i0_ymm4r8_qq[24]);
					   const __m256d t6 = _mm256_fmadd_pd(t5,xx,*(__m256d*)&bessel_i0_ymm4r8_pp[28]);
#elif !defined(__GNUC__) && defined(__INTEL_COMPILER)
                                           const __m256d t0 = _mm256_fmadd_pd(bessel_i0_ymm4r8_pp[0],xx,bessel_i0_ymm4r8_pp[1]);
					   const __m256d c0 = _mm256_fmadd_pd(_mm256_add_pd(xx,bessel_i0_ymm4r8_qq[0]),xx,bessel_i0_ymm4r8_qq[1]);
					   const __m256d t1 = _mm256_fmadd_pd(t0,xx,bessel_i0_ymm4r8_pp[2]);
					   const __m256d c1 = _mm256_fmadd_pd(c0,xx,bessel_i0_ymm4r8_qq[2]);
					   const __m256d t2 = _mm256_fmadd_pd(t1,xx,bessel_i0_ymm4r8_pp[3]);
					   const __m256d c2 = _mm256_fmadd_pd(c1,xx,bessel_i0_ymm4r8_qq[3]);
					   const __m256d t3 = _mm256_fmadd_pd(t2,xx,bessel_i0_ymm4r8_pp[4]);
					   const __m256d c3 = _mm256_fmadd_pd(c2,xx,bessel_i0_ymm4r8_qq[4]);
					   const __m256d t4 = _mm256_fmadd_pd(t3,xx,bessel_i0_ymm4r8_pp[5]);
					   const __m256d c4 = _mm256_fmadd_pd(c3,xx,bessel_i0_ymm4r8_qq[5]);
					   const __m256d t5 = _mm256_fmadd_pd(t4,xx,bessel_i0_ymm4r8_pp[6]);
					   const __m256d c5 = _mm256_fmadd_pd(c4,xx,bessel_i0_ymm4r8_qq[6]);
					   const __m256d t6 = _mm256_fmadd_pd(t5,xx,bessel_i0_ymm4r8_pp[7]);
#endif					   
					   sump             = t6;
					   sumq             = c5;
					   value            = _mm256_div_pd(sump,sumq);
					   const __mmask8 m = _mm256_cmp_pd_mask(x,_mm256_sub_pd(xmax,_15),_CMP_LE_OQ);
					   a                = _mm256_mask_blend_pd(m,_mm256_exp_pd(_mm256_sub_pd(x,_40)),
					                                             _mm256_exp_pd(x));
					   b                = _mm256_mask_blend_pd(m,exp40,_1);
#if defined(__GNUC__) && !defined(__INTEL_COMPILER)    					   
					   const __m256 tmp = _mm256_sub_pd(_mm256_mul_pd(value,a),
					                                    _mm256_mul_pd(*(__m256d*)&bessel_i0_ymm4r8_pp[0],a));
#elif !defined(__GNUC__) && defined(__INTEL_COMPILER)
                                           const __m256 tmp = _mm256_sub_pd(_mm256_mul_pd(value,a),
					                                    _mm256_mul_pd(bessel_i0_ymm4r8_pp[0],a));
#endif					                                    
					   value            = _mm256_mul_pd(_mm256_div_pd(tmp,_mm256_sqrt_pd(x)),b);
				    }
			    }
			   
			    bessel_i0 = value;
			    return (bessel_i0);
		    }
		    
		    
                           __m256  bessesl_i0_ymm8r4(const __m256 arg) {
                           
			    const __m256 rec15                    =  _mm256_set1_ps(6.6666666666666666666E-02f);
			    const __m256 xmax                     =  _mm256_set1_ps(91.9E+00f);
			    const __m256 exp40                    =  _mm256_set1_ps(2.353852668370199854E+17f);
			    const __m256 _1                       =  _mm256_set1_ps(1.0f);
			    const __m256 _15                      =  _mm256_set1_ps(15.0f);
			    const __m256 _225                     =  _mm256_set1_ps(225.0f);
			    const __m256 _40                      =  _mm256_set1_ps(40.0f);
			    const __m256 eps                      =  _mm256_set1_pd(FLT_EPSILON);
			    const __m256 huge                     =  _mm256_set1_pd(FLT_MAX);
			    __m256 value,a,b,bessel_i0;
			    __m256 sump,sumq,x,xx;
                            x = _mm256_abs_ps(arg);
			    if(_mm256_cmp_ps_mask(x,eps,_CMP_LT_OQ)) {
                               value = _1;
			    }
			    else if(_mm256_cmp_ps_mask(x,_15,_CMP_LT_OQ)) {
                               xx   = _mm256_mul_ps(x,x);
#if defined(__GNUC__) && !defined(__INTEL_COMPILER)                                    
			       sump = *(__m256*)&bessel_i0_ymm8r4_p[0];
			       sump = _mm256_fmadd_ps(sump,xx,*(__m256*)&bessel_i0_ymm8r4_p[8]);
			       sump = _mm256_fmadd_ps(sump,xx,*(__m256*)&bessel_i0_ymm8r4_p[16]);
			       sump = _mm256_fmadd_ps(sump,xx,*(__m256*)&bessel_i0_ymm8r4_p[24]);
			       sump = _mm256_fmadd_ps(sump,xx,*(__m256*)&bessel_i0_ymm8r4_p[32]);
			       sump = _mm256_fmadd_ps(sump,xx,*(__m256*)&bessel_i0_ymm8r4_p[40]);
			       sump = _mm256_fmadd_ps(sump,xx,*(__m256*)&bessel_i0_ymm8r4_p[48]);
			       sump = _mm256_fmadd_ps(sump,xx,*(__m256*)&bessel_i0_ymm8r4_p[56]);
			       sump = _mm256_fmadd_ps(sump,xx,*(__m256*)&bessel_i0_ymm8r4_p[64]);
			       sump = _mm256_fmadd_ps(sump,xx,*(__m256*)&bessel_i0_ymm8r4_p[72]);
			       sump = _mm256_fmadd_ps(sump,xx,*(__m256*)&bessel_i0_ymm8r4_p[80]);
			       sump = _mm256_fmadd_ps(sump,xx,*(__m256*)&bessel_i0_ymm8r4_p[88]);
			       sump = _mm256_fmadd_ps(sump,xx,*(__m256*)&bessel_i0_ymm8r4_p[96]);
			       sump = _mm256_fmadd_ps(sump,xx,*(__m256*)&bessel_i0_ymm8r4_p[104]);
			       sump = _mm256_fmadd_ps(sump,xx,*(__m256*)&bessel_i0_ymm8r4_p[112]);
			       xx   = _mm256_sub_ps(xx,_225);
			       const __m256 xxq0 = _mm256_add_ps(xx,*(__m256*)&bessel_i0_ymm8r4_q[0]);
			       const __m256 xxq1 = _mm256_add_ps(xx,*(__m256*)&bessel_i0_ymm8r4_q[8]);
			       const __m256 xxq2 = _mm256_add_ps(xx,*(__m256*)&bessel_i0_ymm8r4_q[16]);
			       const __m256 xxq3 = _mm256_add_ps(xx,*(__m256*)&bessel_i0_ymm8r4_q[24]);
			       const __m256 xxq4 = _mm256_add_ps(xx,*(__m256*)&bessel_i0_ymm8r4_q[32]);
#elif !defined(__GNUC__) && defined(__INTEL_COMPILER)
                               sump = bessel_i0_ymm8r4_p[0];
			       sump = _mm256_fmadd_ps(sump,xx,bessel_i0_ymm8r4_p[1]);
			       sump = _mm256_fmadd_ps(sump,xx,bessel_i0_ymm8r4_p[2]);
			       sump = _mm256_fmadd_ps(sump,xx,bessel_i0_ymm8r4_p[3]);
			       sump = _mm256_fmadd_ps(sump,xx,bessel_i0_ymm8r4_p[4]);
			       sump = _mm256_fmadd_ps(sump,xx,bessel_i0_ymm8r4_p[5]);
			       sump = _mm256_fmadd_ps(sump,xx,bessel_i0_ymm8r4_p[6]);
			       sump = _mm256_fmadd_ps(sump,xx,bessel_i0_ymm8r4_p[7]);
			       sump = _mm256_fmadd_ps(sump,xx,bessel_i0_ymm8r4_p[8]);
			       sump = _mm256_fmadd_ps(sump,xx,bessel_i0_ymm8r4_p[9]);
			       sump = _mm256_fmadd_ps(sump,xx,bessel_i0_ymm8r4_p[10]);
			       sump = _mm256_fmadd_ps(sump,xx,bessel_i0_ymm8r4_p[11]);
			       sump = _mm256_fmadd_ps(sump,xx,bessel_i0_ymm8r4_p[12]);
			       sump = _mm256_fmadd_ps(sump,xx,bessel_i0_ymm8r4_p[13]);
			       sump = _mm256_fmadd_ps(sump,xx,bessel_i0_ymm8r4_p[14]);
			       xx   = _mm256_sub_ps(xx,_225);
			       const __m256 xxq0 = _mm256_add_ps(xx,bessel_i0_ymm8r4_q[0]);
			       const __m256 xxq1 = _mm256_add_ps(xx,bessel_i0_ymm8r4_q[1]);
			       const __m256 xxq2 = _mm256_add_ps(xx,bessel_i0_ymm8r4_q[2]);
			       const __m256 xxq3 = _mm256_add_ps(xx,bessel_i0_ymm8r4_q[3]);
			       const __m256 xxq4 = _mm256_add_ps(xx,bessel_i0_ymm8r4_q[4]);
#endif			       
			       sumq = _mm256_mul_ps(xxq0,
			                        _mm256_mul_ps(xxq1,
						          _mm256_mul_ps(xxq2,
							            _mm256_mul_ps(xxq3,xxq4))));
			       value = _mm256_div_ps(sump,sumq);
			                                         
			    }
			    else if(_mm256_cmp_ps_mask(_15,x,_CMP_LE_OQ)) {
                                    if(_mm256_cmp_ps_mask(xmax,x,_CMP_LT_OQ)) {
                                       value = huge;
				    }
				    else {
                                           xx = _mm256_sub_ps(_mm256_div_ps(_1,x),rec15);
#if defined(__GNUC__) && !defined(__INTEL_COMPILER)                                            
					   const __m256 t0 = _mm256_fmadd_ps(*(__m256*)&bessel_i0_ymm8r4_pp[0],xx,*(__m256*)&bessel_i0_ymm8r4_pp[8]);
					   const __m256 c0 = _mm256_fmadd_ps(_mm256_add_ps(xx,*(__m256*)&bessel_i0_ymm8r4_qq[0]),xx,*(__m256*)&bessel_i0_ymm8r4_qq[8]);
					   const __m256 t1 = _mm256_fmadd_ps(t0,xx,*(__m256*)&bessel_i0_ymm8r4_pp[16]);
					   const __m256 c1 = _mm256_fmadd_ps(c0,xx,*(__m256*)&bessel_i0_ymm8r4_qq[16]);
					   const __m256 t2 = _mm256_fmadd_ps(t1,xx,*(__m256*)&bessel_i0_ymm8r4_pp[24]);
					   const __m256 c2 = _mm256_fmadd_ps(c1,xx,*(__m256*)&bessel_i0_ymm8r4_qq[24]);
					   const __m256 t3 = _mm256_fmadd_ps(t2,xx,*(__m256*)&bessel_i0_ymm8r4_pp[32]);
					   const __m256 c3 = _mm256_fmadd_ps(c2,xx,*(__m256*)&bessel_i0_ymm8r4_qq[32]);
					   const __m256 t4 = _mm256_fmadd_ps(t3,xx,*(__m256*)&bessel_i0_ymm8r4_pp[40]);
					   const __m256 c4 = _mm256_fmadd_ps(c3,xx,*(__m256*)&bessel_i0_ymm8r4_qq[40]);
					   const __m256 t5 = _mm256_fmadd_ps(t4,xx,*(__m256*)&bessel_i0_ymm8r4_pp[48]);
					   const __m256 c5 = _mm256_fmadd_ps(c4,xx,*(__m256*)&bessel_i0_ymm8r4_qq[48]);
					   const __m256 t6 = _mm256_fmadd_ps(t5,xx,*(__m256*)&bessel_i0_ymm8r4_pp[56]);
#elif !defined(__GNUC__) && defined(__INTEL_COMPILER)
                                           const __m256 t0 = _mm256_fmadd_ps(bessel_i0_ymm8r4_pp[0],xx,bessel_i0_ymm8r4_pp[1]);
					   const __m256 c0 = _mm256_fmadd_ps(_mm256_add_ps(xx,bessel_i0_ymm8r4_qq[0]),xx,bessel_i0_ymm8r4_qq[1]);
					   const __m256 t1 = _mm256_fmadd_ps(t0,xx,bessel_i0_ymm8r4_pp[2]);
					   const __m256 c1 = _mm256_fmadd_ps(c0,xx,bessel_i0_ymm8r4_qq[2]);
					   const __m256 t2 = _mm256_fmadd_ps(t1,xx,bessel_i0_ymm8r4_pp[3]);
					   const __m256 c2 = _mm256_fmadd_ps(c1,xx,bessel_i0_ymm8r4_qq[3]);
					   const __m256 t3 = _mm256_fmadd_ps(t2,xx,bessel_i0_ymm8r4_pp[4]);
					   const __m256 c3 = _mm256_fmadd_ps(c2,xx,bessel_i0_ymm8r4_qq[4]);
					   const __m256 t4 = _mm256_fmadd_ps(t3,xx,bessel_i0_ymm8r4_pp[5]);
					   const __m256 c4 = _mm256_fmadd_ps(c3,xx,bessel_i0_ymm8r4_qq[5]);
					   const __m256 t5 = _mm256_fmadd_ps(t4,xx,bessel_i0_ymm8r4_pp[6]);
					   const __m256 c5 = _mm256_fmadd_ps(c4,xx,bessel_i0_ymm8r4_qq[6]);
					   const __m256 t6 = _mm256_fmadd_ps(t5,xx,bessel_i0_ymm8r4_pp[7]);
#endif					   
					   sump             = t6;
					   sumq             = c5;
					   value            = _mm256_div_ps(sump,sumq);
					   const __mmask8 m = _mm256_cmp_ps_mask(x,_mm256_sub_ps(xmax,_15),_CMP_LE_OQ);

					   a                = _mm256_mask_blend_ps(m,_mm256_exp_ps(_mm256_sub_ps(x,_40)),
					                                             _mm256_exp_ps(x));
   					   b                = _mm256_mask_blend_ps(m,exp40,_1);
#if defined(__GNUC__) && !defined(__INTEL_COMPILER)      					   
					   const __m256 tmp = _mm256_sub_ps(_mm256_mul_ps(value,a),
					                                    _mm256_mul_ps(*(__m256*)&bessel_i0_ymm8r4_pp[0],a));
#elif !defined(__GNUC__) && defined(__INTEL_COMPILER)
                                           const __m256 tmp = _mm256_sub_ps(_mm256_mul_ps(value,a),
					                                    _mm256_mul_ps(bessel_i0_ymm8r4_pp[0],a));
#endif					                                    
					   value            = _mm256_mul_ps(_mm256_div_ps(tmp,_mm256_sqrt_ps(x)),b);
				    }
			    }
			   
			    bessel_i0 = value;
			    return (bessel_i0);
		    }
		    
		    
/*
 !*****************************************************************************80
!
!! BESSEL_I1 evaluates the Bessel I function of order I.
!
!  Discussion:
!
!    The main computation evaluates slightly modified forms of
!    minimax approximations generated by Blair and Edwards.
!    This transportable program is patterned after the machine-dependent
!    FUNPACK packet NATSI1, but cannot match that version for efficiency
!    or accuracy.  This version uses rational functions that theoretically
!    approximate I-SUB-1(X) to at least 18 significant decimal digits.
!    The accuracy achieved depends on the arithmetic system, the compiler,
!    the intrinsic functions, and proper selection of the machine-dependent
!    constants.
!
!  Machine-dependent constants:
!
!    beta   = Radix for the floating-point system.
!    maxexp = Smallest power of beta that overflows.
!    XMAX =   Largest argument acceptable to BESI1;  Solution to
!             equation:
!               EXP(X) * (1-3/(8*X)) / SQRT(2*PI*X) = beta**maxexp
!
!
!    Approximate values for some important machines are:
!
!                            beta       maxexp    XMAX
!
!    CRAY-1        (S.P.)       2         8191    5682.810
!    Cyber 180/855
!      under NOS   (S.P.)       2         1070     745.894
!    IEEE (IBM/XT,
!      SUN, etc.)  (S.P.)       2          128      91.906
!    IEEE (IBM/XT,
!      SUN, etc.)  (D.P.)       2         1024     713.987
!    IBM 3033      (D.P.)      16           63     178.185
!    VAX           (S.P.)       2          127      91.209
!    VAX D-Format  (D.P.)       2          127      91.209
!    VAX G-Format  (D.P.)       2         1023     713.293
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 October 2004
!
!  Author:
!
!    Original FORTRAN77 version by William Cody, Laura Stoltz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Blair, Edwards,
!    Chalk River Report AECL-4928,
!    Atomic Energy of Canada, Limited,
!    October, 1974.
!
!  Parameters:
!
!    Input, real (kind = 8 ) ARG, the argument.
!
!    Output, real ( kind = 8 ) BESSEL_I1, the value of the Bessel
!    I1 function.
!
*/


                    
		      __m256d bessel_i1_ymm4r8(const __m256d arg) {
                          
			   const __m256d exp40                     =  _mm256_set1_pd(2.353852668370199854E+17);
			   const __m256d _40                       =  _mm256_set1_pd(40.0);
			   const __m256d _1_2                      =  _mm256_set1_pd(0.5);
			   const __m256d _1                        =  _mm256_set1_pd(1.0);
			   const __m256d _15                       =  _mm256_set1_pd(15.0);
			   const __m256d pbar                      =  _mm256_set1_pd(3.98437500E-01);
			   const __m256d rec15                     =  _mm256_set1_pd(6.6666666666666666666E-02);
			   const __m256d _225                      =  _mm256_set1_pd(225.0);
			   const __m256d xmax                      =  _mm256_set1_pd(713.987E+00);
			   const __m256d _0                        =  _mm256_setzero_pd();
			   const __m256d eps                       =  _mm256_set1_pd(DBL_EPSILON);
			   const __m256d huge                      =  _mm256_set1_pd(DBL_MAX);
			   __m256d a,b,bessel_i1,value;
			   __m256d sump,sumq,x,xx;

			   x  = _mm256_abs_pd(arg);
			   if(_mm256_cmp_pd_mask(x,eps,_CMP_LT_OQ)) {
                               value = _mm256_mul_pd(_1_2,x);
			   }
			   else if(_mm256_cmp_pd_mask(x,_15,_CMP_LT_OQ)) {
                               xx   = _mm256_mul_pd(x,x);
#if defined(__GNUC__) && !defined(__INTEL_COMPILER)                                  
			       sump = *(__m256d*)&bessel_i1_ymm4r8_p[0];
			       sump = _mm256_fmadd_pd(sump,xx,*(__m256d*)&bessel_i1_ymm4r8_p[4]);
			       sump = _mm256_fmadd_pd(sump,xx,*(__m256d*)&bessel_i1_ymm4r8_p[8]);
			       sump = _mm256_fmadd_pd(sump,xx,*(__m256d*)&bessel_i1_ymm4r8_p[12]);
			       sump = _mm256_fmadd_pd(sump,xx,*(__m256d*)&bessel_i1_ymm4r8_p[16]);
			       sump = _mm256_fmadd_pd(sump,xx,*(__m256d*)&bessel_i1_ymm4r8_p[20]);
			       sump = _mm256_fmadd_pd(sump,xx,*(__m256d*)&bessel_i1_ymm4r8_p[24]);
			       sump = _mm256_fmadd_pd(sump,xx,*(__m256d*)&bessel_i1_ymm4r8_p[28]);
			       sump = _mm256_fmadd_pd(sump,xx,*(__m256d*)&bessel_i1_ymm4r8_p[32]);
			       sump = _mm256_fmadd_pd(sump,xx,*(__m256d*)&bessel_i1_ymm4r8_p[36]);
			       sump = _mm256_fmadd_pd(sump,xx,*(__m256d*)&bessel_i1_ymm4r8_p[40]);
			       sump = _mm256_fmadd_pd(sump,xx,*(__m256d*)&bessel_i1_ymm4r8_p[44]);
			       sump = _mm256_fmadd_pd(sump,xx,*(__m256d*)&bessel_i1_ymm4r8_p[48]);
			       sump = _mm256_fmadd_pd(sump,xx,*(__m256d*)&bessel_i1_ymm4r8_p[52]);
			       sump = _mm256_fmadd_pd(sump,xx,*(__m256d*)&bessel_i1_ymm4r8_p[56]);
			       xx   = _mm256_sub_pd(xx,_225);
			       const __m256d t0 = _mm256_fmadd_pd(_mm256_add_pd(xx,*(__m256d*)&bessel_i1_ymm4r8_q[0]),xx,*(__m256d*)&bessel_i1_ymm4r8_q[4]);
			       const __m256d t1 = _mm256_fmadd_pd(t0,xx,*(__m256d*)&bessel_i1_ymm4r8_q[8]);
			       const __m256d t2 = _mm256_fmadd_pd(t1,xx,*(__m256d*)&bessel_i1_ymm4r8_q[12]);
			       const __m256d t3 = _mm256_fmadd_pd(t2,xx,*(__m256d*)&bessel_i1_ymm4r8_q[16]);
#elif !defined(__GNUC__) && defined(__INTEL_COMPILER)
                               sump = bessel_i1_ymm4r8_p[0];
			       sump = _mm256_fmadd_pd(sump,xx,bessel_i1_ymm4r8_p[1]);
			       sump = _mm256_fmadd_pd(sump,xx,bessel_i1_ymm4r8_p[2]);
			       sump = _mm256_fmadd_pd(sump,xx,bessel_i1_ymm4r8_p[3]);
			       sump = _mm256_fmadd_pd(sump,xx,bessel_i1_ymm4r8_p[4]);
			       sump = _mm256_fmadd_pd(sump,xx,bessel_i1_ymm4r8_p[5]);
			       sump = _mm256_fmadd_pd(sump,xx,bessel_i1_ymm4r8_p[6]);
			       sump = _mm256_fmadd_pd(sump,xx,bessel_i1_ymm4r8_p[7]);
			       sump = _mm256_fmadd_pd(sump,xx,bessel_i1_ymm4r8_p[8]);
			       sump = _mm256_fmadd_pd(sump,xx,bessel_i1_ymm4r8_p[9]);
			       sump = _mm256_fmadd_pd(sump,xx,bessel_i1_ymm4r8_p[10]);
			       sump = _mm256_fmadd_pd(sump,xx,bessel_i1_ymm4r8_p[11]);
			       sump = _mm256_fmadd_pd(sump,xx,bessel_i1_ymm4r8_p[12]);
			       sump = _mm256_fmadd_pd(sump,xx,bessel_i1_ymm4r8_p[13]);
			       sump = _mm256_fmadd_pd(sump,xx,bessel_i1_ymm4r8_p[14]);
			       xx   = _mm256_sub_pd(xx,_225);
			       const __m256d t0 = _mm256_fmadd_pd(_mm256_add_pd(xx,bessel_i1_ymm4r8_q[0]),xx,bessel_i1_ymm4r8_q[1]);
			       const __m256d t1 = _mm256_fmadd_pd(t0,xx,bessel_i1_ymm4r8_q[2]);
			       const __m256d t2 = _mm256_fmadd_pd(t1,xx,bessel_i1_ymm4r8_q[3]);
			       const __m256d t3 = _mm256_fmadd_pd(t2,xx,bessel_i1_ymm4r8_q[4]);
#endif			       
			       sumq             = t3;
			       value            = _mm256_mul_pd(_mm256_div_pd(sump,sumq),x);
			   }
			   else if(_mm256_cmp_pd_mask(xmax,x,_CMP_LT_OQ)) {
                               value            = huge;
			   }
			   else {
                               xx               = _mm256_sub_pd(_mm256_div_pd(_1,x),rec15);
#if defined(__GNUC__) && !defined(__INTEL_COMPILER)                                   
			       const __m256d t0 = _mm256_fmadd_pd(*(__m256d*)&bessel_i1_ymm4r8_pp[0],xx,*(__m256d*)&bessel_i1_ymm4r8_pp[4]);
			       const __m256d c0 = _mm256_fmadd_pd(_mm256_add_pd(xx,*(__m256d*)&bessel_i1_ymm4r8_qq[0]),xx,*(__m256d*)&bessel_i1_ymm4r8_qq[4]);
			       const __m256d t1 = _mm256_fmadd_pd(t0,xx,*(__m256d*)&bessel_i1_ymm4r8_pp[8]);
			       const __m256d c1 = _mm256_fmadd_pd(c0,xx,*(__m256d*)&bessel_i1_ymm4r8_qq[8]);
			       const __m256d t2 = _mm256_fmadd_pd(t1,xx,*(__m256d*)&bessel_i1_ymm4r8_pp[12]);
			       const __m256d c2 = _mm256_fmadd_pd(c1,xx,*(__m256d*)&bessel_i1_ymm4r8_qq[12]);
			       const __m256d t3 = _mm256_fmadd_pd(t2,xx,*(__m256d*)&bessel_i1_ymm4r8_pp[16]);
			       const __m256d c3 = _mm256_fmadd_pd(c2,xx,*(__m256d*)&bessel_i1_ymm4r8_qq[16]);
			       const __m256d t4 = _mm256_fmadd_pd(t3,xx,*(__m256d*)&bessel_i1_ymm4r8_pp[20]);
			       const __m256d c4 = _mm256_fmadd_pd(c3,xx,*(__m256d*)&bessel_i1_ymm4r8_qq[20]);
			       const __m256d t5 = _mm256_fmadd_pd(t4,xx,*(__m256d*)&bessel_i1_ymm4r8_pp[24]);
			       const __m256d c5 = _mm256_fmadd_pd(c4,xx,*(__m256d*)&bessel_i1_ymm4r8_qq[24]);
			       const __m256d t6 = _mm256_fmadd_pd(t5,xx,*(__m256d*)&bessel_i1_ymm4r8_pp[28]);
#elif !defined(__GNUC__) && defined(__INTEL_COMPILER)
                               const __m256d t0 = _mm256_fmadd_pd(bessel_i1_ymm4r8_pp[0],xx,bessel_i1_ymm4r8_pp[1]);
			       const __m256d c0 = _mm256_fmadd_pd(_mm256_add_pd(xx,bessel_i1_ymm4r8_qq[0]),xx,bessel_i1_ymm4r8_qq[1]);
			       const __m256d t1 = _mm256_fmadd_pd(t0,xx,bessel_i1_ymm4r8_pp[2]);
			       const __m256d c1 = _mm256_fmadd_pd(c0,xx,bessel_i1_ymm4r8_qq[2]);
			       const __m256d t2 = _mm256_fmadd_pd(t1,xx,bessel_i1_ymm4r8_pp[3]);
			       const __m256d c2 = _mm256_fmadd_pd(c1,xx,bessel_i1_ymm4r8_qq[3]);
			       const __m256d t3 = _mm256_fmadd_pd(t2,xx,bessel_i1_ymm4r8_pp[4]);
			       const __m256d c3 = _mm256_fmadd_pd(c2,xx,bessel_i1_ymm4r8_qq[4]);
			       const __m256d t4 = _mm256_fmadd_pd(t3,xx,bessel_i1_ymm4r8_pp[5]);
			       const __m256d c4 = _mm256_fmadd_pd(c3,xx,bessel_i1_ymm4r8_qq[5]);
			       const __m256d t5 = _mm256_fmadd_pd(t4,xx,bessel_i1_ymm4r8_pp[6]);
			       const __m256d c5 = _mm256_fmadd_pd(c4,xx,bessel_i1_ymm4r8_qq[6]);
			       const __m256d t6 = _mm256_fmadd_pd(t5,xx,bessel_i1_ymm4r8_pp[7]);
#endif			       
			       sump             = t6;
			       sumq             = c5;
			       value            = _mm256_div_pd(sump,sumq);
			       const __mmask8 m = _mm256_cmp_pd_mask(_mm256_sub_pd(xmax,_15),_CMP_LT_OQ);

			       a                = _mm256_mask_blend_pd(m,_mm256_exp_pd(x),
			                                                           _mm256_exp_pd(_mm256_sub_pd(x,_40)));
			       b                = _mm256_mask_blend_pd(m,_1,_40);
			       const __m256d tmp= _mm256_add_pd(_mm256_mul_pd(value,a),
			                                        _mm256_mul_pd(pbar,a));
			       value            = _mm256_mul_pd(_mm256_div_pd(tmp,_mm256_sqrt_pd(x)),b);
			   }
			   if(_mm256_cmp_pd_mask(arg,_0,_CMP_LT_OQ)) {
                              value             = ymm4r8_negate(value);
			   }
			   bessel_i1            = value
			   return (bessel_i1);
		    }
		    
		    
                    __m256 bessel_i1_ymm8r4(const __m256 arg) {
                          
			   const __m256 exp40                     =  _mm256_set1_ps(2.353852668370199854E+17f);
			   const __m256 _40                       =  _mm256_set1_ps(40.0f);
			   const __m256 _1_2                      =  _mm256_set1_ps(0.5f);
			   const __m256 _1                        =  _mm256_set1_ps(1.0f);
			   const __m256 _15                       =  _mm256_set1_ps(15.0f);
			   const __m256 pbar                      =  _mm256_set1_ps(3.98437500E-01f);
			   const __m256 rec15                     =  _mm256_set1_ps(6.6666666666666666666E-02f);
			   const __m256 _225                      =  _mm256_set1_ps(225.0f);
			   const __m256 xmax                      =  _mm256_set1_ps(713.987E+00f);
			   const __m256 _0                        =  _mm256_setzero_ps();
			   const __m256 eps                       =  _mm256_set1_ps(FLT_EPSILON);
			   const __m256 huge                      =  _mm256_set1_ps(FLT_MAX);
			   __m256 a,b,bessel_i1,value;
			   __m256 sump,sumq,x,xx;

			   x  = _mm256_abs_ps(arg);
			   if(_mm256_cmp_ps_mask(x,eps,_CMP_LT_OQ)) {
                               value = _mm256_mul_ps(_1_2,x);
			   }
			   else if(_mm256_cmp_ps_mask(x,_15,_CMP_LT_OQ)) {
                               xx   = _mm256_mul_ps(x,x);
#if defined(__GNUC__) && !defined(__INTEL_COMPILER)                                   
			       sump = *(__m256*)&bessel_i1_ymm8r4_p[0];
			       sump = _mm256_fmadd_ps(sump,xx, *(__m256*)&bessel_i1_ymm8r4_p[8]);
			       sump = _mm256_fmadd_ps(sump,xx, *(__m256*)&bessel_i1_ymm8r4_p[16]);
			       sump = _mm256_fmadd_ps(sump,xx, *(__m256*)&bessel_i1_ymm8r4_p[24]);
			       sump = _mm256_fmadd_ps(sump,xx, *(__m256*)&bessel_i1_ymm8r4_p[32]);
			       sump = _mm256_fmadd_ps(sump,xx, *(__m256*)&bessel_i1_ymm8r4_p[40]);
			       sump = _mm256_fmadd_ps(sump,xx, *(__m256*)&bessel_i1_ymm8r4_p[48]);
			       sump = _mm256_fmadd_ps(sump,xx, *(__m256*)&bessel_i1_ymm8r4_p[56]);
			       sump = _mm256_fmadd_ps(sump,xx, *(__m256*)&bessel_i1_ymm8r4_p[64]);
			       sump = _mm256_fmadd_ps(sump,xx, *(__m256*)&bessel_i1_ymm8r4_p[72]);
			       sump = _mm256_fmadd_ps(sump,xx, *(__m256*)&bessel_i1_ymm8r4_p[80]);
			       sump = _mm256_fmadd_ps(sump,xx, *(__m256*)&bessel_i1_ymm8r4_p[88]);
			       sump = _mm256_fmadd_ps(sump,xx, *(__m256*)&bessel_i1_ymm8r4_p[96]);
			       sump = _mm256_fmadd_ps(sump,xx, *(__m256*)&bessel_i1_ymm8r4_p[104]);
			       sump = _mm256_fmadd_ps(sump,xx, *(__m256*)&bessel_i1_ymm8r4_p[112]);
			       xx   = _mm256_sub_ps(xx,_225);
			       const __m256 t0 = _mm256_fmadd_ps(_mm256_add_ps(xx,*(__m256*)&bessel_i1_ymm8r4_q[0]),xx,*(__m256*)&bessel_i1_ymm8r4_q[8]);
			       const __m256 t1 = _mm256_fmadd_ps(t0,xx,*(__m256*)&bessel_i1_ymm8r4_q[16]);
			       const __m256 t2 = _mm256_fmadd_ps(t1,xx,*(__m256*)&bessel_i1_ymm8r4_q[24]);
			       const __m256 t3 = _mm256_fmadd_ps(t2,xx,*(__m256*)&bessel_i1_ymm8r4_q[32]);
#elif !defined(__GNUC__) && defined(__INTEL_COMPILER)
                               sump = bessel_i1_ymm8r4_p[0];
			       sump = _mm256_fmadd_ps(sump,xx,bessel_i1_ymm8r4_p[1]);
			       sump = _mm256_fmadd_ps(sump,xx,bessel_i1_ymm8r4_p[2]);
			       sump = _mm256_fmadd_ps(sump,xx,bessel_i1_ymm8r4_p[3]);
			       sump = _mm256_fmadd_ps(sump,xx,bessel_i1_ymm8r4_p[4]);
			       sump = _mm256_fmadd_ps(sump,xx,bessel_i1_ymm8r4_p[5]);
			       sump = _mm256_fmadd_ps(sump,xx,bessel_i1_ymm8r4_p[6]);
			       sump = _mm256_fmadd_ps(sump,xx,bessel_i1_ymm8r4_p[7]);
			       sump = _mm256_fmadd_ps(sump,xx,bessel_i1_ymm8r4_p[8]);
			       sump = _mm256_fmadd_ps(sump,xx,bessel_i1_ymm8r4_p[9]);
			       sump = _mm256_fmadd_ps(sump,xx,bessel_i1_ymm8r4_p[10]);
			       sump = _mm256_fmadd_ps(sump,xx,bessel_i1_ymm8r4_p[11]);
			       sump = _mm256_fmadd_ps(sump,xx,bessel_i1_ymm8r4_p[12]);
			       sump = _mm256_fmadd_ps(sump,xx,bessel_i1_ymm8r4_p[13]);
			       sump = _mm256_fmadd_ps(sump,xx,bessel_i1_ymm8r4_p[14]);
			       xx   = _mm256_sub_ps(xx,_225);
			       const __m256 t0 = _mm256_fmadd_ps(_mm256_add_ps(xx,bessel_i1_ymm8r4_q[0]),xx,bessel_i1_ymm8r4_q[1]);
			       const __m256 t1 = _mm256_fmadd_ps(t0,xx,bessel_i1_ymm8r4_q[2]);
			       const __m256 t2 = _mm256_fmadd_ps(t1,xx,bessel_i1_ymm8r4_q[3]);
			       const __m256 t3 = _mm256_fmadd_ps(t2,xx,qbessel_i1_ymm8r4_[4]);
#endif			       
			       sumq             = t3;
			       value            = _mm256_mul_ps(_mm256_div_ps(sump,sumq),x);
			   }
			   else if(_mm256_cmp_ps_mask(xmax,x,_CMP_LT_OQ)) {
                               value            = huge;
			   }
			   else {
                               xx               = _mm256_sub_ps(_mm256_div_ps(_1,x),rec15);
#if defined(__GNUC__) && !defined(__INTEL_COMPILER)                                
			       const __m256 t0 = _mm256_fmadd_ps(*(__m256*)&bessel_i1_ymm8r4_pp[0],xx,*(__m256*)&bessel_i1_ymm8r4_pp[8]);
			       const __m256 c0 = _mm256_fmadd_ps(_mm256_add_ps(xx,*(__m256*)&bessel_i1_ymm8r4_qq[0]),xx,*(__m256*)&bessel_i1_ymm8r4_qq[8]);
			       const __m256 t1 = _mm256_fmadd_ps(t0,xx,*(__m256*)&bessel_i1_ymm8r4_pp[16]);
			       const __m256 c1 = _mm256_fmadd_ps(c0,xx,*(__m256*)&bessel_i1_ymm8r4_qq[16]);
			       const __m256 t2 = _mm256_fmadd_ps(t1,xx,*(__m256*)&bessel_i1_ymm8r4_pp[24]);
			       const __m256 c2 = _mm256_fmadd_ps(c1,xx,*(__m256*)&bessel_i1_ymm8r4_qq[24]);
			       const __m256 t3 = _mm256_fmadd_ps(t2,xx,*(__m256*)&bessel_i1_ymm8r4_pp[32]);
			       const __m256 c3 = _mm256_fmadd_ps(c2,xx,*(__m256*)&bessel_i1_ymm8r4_qq[32]);
			       const __m256 t4 = _mm256_fmadd_ps(t3,xx,*(__m256*)&bessel_i1_ymm8r4_pp[40]);
			       const __m256 c4 = _mm256_fmadd_ps(c3,xx,*(__m256*)&bessel_i1_ymm8r4_qq[40]);
			       const __m256 t5 = _mm256_fmadd_ps(t4,xx,*(__m256*)&bessel_i1_ymm8r4_pp[48]);
			       const __m256 c5 = _mm256_fmadd_ps(c4,xx,*(__m256*)&bessel_i1_ymm8r4_qq[48]);
			       const __m256 t6 = _mm256_fmadd_ps(t5,xx,*(__m256*)&bessel_i1_ymm8r4_pp[56]);
#elif !defined(__GNUC__) && defined(__INTEL_COMPILER)
                               const __m256 t0 = _mm256_fmadd_ps(bessel_i1_ymm8r4_pp[0],xx,bessel_i1_ymm8r4_pp[1]);
			       const __m256 c0 = _mm256_fmadd_ps(_mm256_add_ps(xx,bessel_i1_ymm8r4_qq[0]),xx,bessel_i1_ymm8r4_qq[1]);
			       const __m256 t1 = _mm256_fmadd_ps(t0,xx,bessel_i1_ymm8r4_pp[2]);
			       const __m256 c1 = _mm256_fmadd_ps(c0,xx,bessel_i1_ymm8r4_qq[2]);
			       const __m256 t2 = _mm256_fmadd_ps(t1,xx,bessel_i1_ymm8r4_pp[3]);
			       const __m256 c2 = _mm256_fmadd_ps(c1,xx,bessel_i1_ymm8r4_qq[3]);
			       const __m256 t3 = _mm256_fmadd_ps(t2,xx,bessel_i1_ymm8r4_pp[4]);
			       const __m256 c3 = _mm256_fmadd_ps(c2,xx,bessel_i1_ymm8r4_qq[4]);
			       const __m256 t4 = _mm256_fmadd_ps(t3,xx,bessel_i1_ymm8r4_pp[5]);
			       const __m256 c4 = _mm256_fmadd_ps(c3,xx,bessel_i1_ymm8r4_qq[5]);
			       const __m256 t5 = _mm256_fmadd_ps(t4,xx,bessel_i1_ymm8r4_pp[6]);
			       const __m256 c5 = _mm256_fmadd_ps(c4,xx,bessel_i1_ymm8r4_qq[6]);
			       const __m256 t6 = _mm256_fmadd_ps(t5,xx,bessel_i1_ymm8r4_pp[7]);
#endif			       
			       sump             = t6;
			       sumq             = c5;
			       value            = _mm256_div_ps(sump,sumq);
			       const __mmask16 m = _mm256_cmp_ps_mask(_mm256_sub_ps(xmax,_15),_CMP_LT_OQ);

			       a                = _mm256_mask_blend_ps(m,_mm256_exp_ps(x),
			                                                           _mm256_exp_ps(_mm256_sub_ps(x,_40)));
			       b                = _mm256_mask_blend_ps(m,_1,_40);
			       const __m256 tmp= _mm256_add_ps(_mm256_mul_ps(value,a),
			                                        _mm256_mul_ps(pbar,a));
			       value            = _mm256_mul_ps(_mm256_div_ps(tmp,_mm256_sqrt_ps(x)),b);
			   }
			   if(_mm256_cmp_ps_mask(arg,_0,_CMP_LT_OQ)) {
                              value             = ymm8r4_negate(value);
			   }
			   bessel_i1            = value
			   return (bessel_i1);
		    }


/*
!*****************************************************************************80
!
!! BETA returns the value of the Beta function.
!
!  Discussion:
!
!    The Beta function is defined as
!
!      BETA(A,B) = ( GAMMA ( A ) * GAMMA ( B ) ) / GAMMA ( A + B )
!                = Integral ( 0 <= T <= 1 ) T**(A-1) (1-T)**(B-1) dT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 July 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the function.
!    0.0D+00 < A,
!    0.0D+00 < B.
!
!    Output, real ( kind = 8 ) BETA, the value of the function.
!

*/


                       __m256d beta_ymm4r8(const __m256d a,
		                          const __m256d b) {

                    	const __m256d _0  = _mm256_setzero_pd();
			const __m256d ab  = _mm256_add_pd(a,b);
			__m256d beta      = _mm256_setzero_pd();
			beta              = _mm256_exp_pd(
			                              _mm256_sub_pd(
						                 _mm256_add_pd(gamma_log_ymm4r8(a),
								               gamma_log_ymm4r8(b)),
			return (beta);						                     gamma_log_ymm4r8(ab)));
		    }
		    
		    
/*
 !*****************************************************************************80
!
!! GAMMA_CDF evaluates the Gamma CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    A <= X
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0D+00 < B,
!    0.0D+00 < C.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
*/


                    
		      __m256d
		      gamma_cdf_ymm4r8(const __m256d x,
		                       const __m256d a,
		                       const __m256d b,
		                       const __m256d c) {
		      
		          register __m256d x2,p2;
		          register __m256d cdf;
		          p2 = c;
		          x2 = _mm256_div_pd(_mm256_sub_pd(x,a),b);
		          cdf= gamma_incomplete_ymm4r8(p2,x2);
		          return (cdf);                  
		    }
		    
		    
		    
                   
		      __m256
		      gamma_cdf_ymm8r4(const __m256 x,
		                       const __m256 a,
		                       const __m256 b,
		                       const __m256 c) {
		      
		          register __m256 x2,p2;
		          register __m256 cdf;
		          p2 = c;
		          x2 = _mm256_div_ps(_mm256_sub_ps(x,a),b);
		          cdf= gamma_incomplete_ymm8r4(p2,x2);
		          return (cdf);                  
		    }
		    
		    
/*
      !*****************************************************************************80
!
!! R8_UNIFORM_01 returns a unit pseudorandom R8.
!
!  Discussion:
!
!    An R8 is a real ( kind = 8 ) value.
!
!    For now, the input quantity SEED is an integer ( kind = 4 ) variable.
!
!    This routine implements the recursion
!
!      seed = 16807 * seed mod ( 2**31 - 1 )
!      r8_uniform_01 = seed / ( 2**31 - 1 )
!
!    The integer arithmetic never requires more than 32 bits,
!    including a sign bit.
!
!    If the initial seed is 12345, then the first three computations are
!
!      Input     Output      R8_UNIFORM_01
!      SEED      SEED
!
!         12345   207482415  0.096616
!     207482415  1790989824  0.833995
!    1790989824  2035175616  0.947702
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 July 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Springer Verlag, pages 201-202, 1983.
!
!    Pierre L'Ecuyer,
!    Random Number Generation,
!    in Handbook of Simulation,
!    edited by Jerry Banks,
!    Wiley Interscience, page 95, 1998.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, pages 362-376, 1986.
!
!    Peter Lewis, Allen Goodman, James Miller
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, pages 136-143, 1969.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which should
!    NOT be 0. On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R8_UNIFORM_01, a new pseudorandom variate,
!    strictly between 0 and 1   
*/


        
		      __m256d 
		      uniform_01_ymm4r8( __m256i * __restrict seed) {
		         
		         register __m256i C127773 = _mm256_set1_epi32(127773);
		         register __m256i C16807  = _mm256_set1_epi32(16807);
		         register __m256i C2836   = _mm256_set1_epi32(2836);
		         register __m256d C4656612875 = _mm256_set1_pd(4.656612875e-10);
		         register __m256i k,t0,t1;
		         register __m256d uni01;
		         k  = _mm256_div_epi32(*seed,C127773);
		         t0 = _mm256_mul_epi32(k,C2836);
		         t1 = _mm256_sub_epi32(*seed,_mm256_mul_epi32(k,C127773));
		         *seed = _mm256_mul_epi32(C16807,_mm256_sub_epi32(t1,t0));
		         if(_mm256_cmp_epi32_mask(seed,_mm256_setzero_epi32(),_CMP_LT_OQ)) 
		            seed = _mm256_add_epi32(seed,_mm256_set1_epi32(2147483647));
		         uni01   = _mm256_mul_pd(_mm256_castsi256_pd(seed),C4656612875);
		         return (uni01);
		     }
		     
		     
		      __ATTR_REGCALL__
                      __ATTR_ALWAYS_INLINE__
		      __ATTR_HOT__
		      __ATTR_ALIGN__(32)
		      static inline
		      __m256
		      uniform_01_ymm8r4( __m256i * __restrict seed) {
		            
		            return (_mm256_castpd_ps(uniform_01_ymm4r8(*seed)));
		      }
		    

/*
     !*****************************************************************************80
!
!! NORMAL_CDF_INV inverts the Normal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0D+00 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0D+00 < B.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
! 
*/


                      __m256d 		   
		      normal_cdf_inv_zmm8r8(const __m256d cdf,
		                            const __m256d a,
		                            const __m256d b) {
		          
		          register __m256d x2;
		          register __m256d x;
		          x2 = normal_01_cdf_inv_ymm4r8(cdf);
		          x  = _mm256_add_pd(a,_mm256_mul_pd(b,x2));
		          return (x);      
		   }
		   
		   
		    
		      __m256 		   
		      normal_cdf_inv_zmm16r4(const __m256 cdf,
		                            const __m256 a,
		                            const __m256 b) {
		          
		          register __m256 x2;
		          register __m256 x;
		          x2 = normal_01_cdf_inv_ymm8r4(cdf);
		          x  = _mm256_add_ps(a,_mm256_mul_ps(b,x2));
		          return (x);      
		   } 
		   
		   
/*
   
     !*****************************************************************************80
!
!! NORMAL_01_PDF evaluates the Normal 01 PDF.
!
!  Discussion:
!
!    The Normal 01 PDF is also called the "Standard Normal" PDF, or
!    the Normal PDF with 0 mean and variance 1.
!
!    PDF(X) = exp ( - 0.5 * X^2 ) / sqrt ( 2 * PI )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF. 
   
*/

      
                    
                    
		      __m256d  
		      normal_01_pdf_ymm4r8(const __m256d x) {
		         
		          register __m256d C039894228040143267793995 = 
		                                   _mm256_set1_pd(0.39894228040143267793995);
		          register __m256d C05 = _mm256_set1_pd(-0.5);
		          register __m256d earg,pdf;
		          earg = _mm256_mul_pd(C05,_mm256_mul_pd(x,x));
                          pdf  = _mm256_mul_pd(_mm256_exp_pd(earg),C039894228040143267793995);
                        		   
                          return (pdf);
		     }  
		     
		     
		    __m256 
		      normal_01_pdf_ymm8r4(const __m256 x) {
		         
		          register __m256 C039894228040143267793995 = 
		                                   _mm256_set1_ps(0.39894228040143267793995f);
		          register __m256 C05 = _mm256_set1_ps(-0.5f);
		          register __m256 earg,pdf;
		          earg = _mm256_mul_ps(C05,_mm256_mul_ps(x,x));
                          pdf  = _mm256_mul_ps(_mm256_exp_ps(earg),C039894228040143267793995);
                        		   
                          return (pdf);
		     }   
		     
		     
/*
     !*****************************************************************************80
!
!! BETA_SAMPLE samples the Beta PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Kennedy, James Gentle,
!    Algorithm BN,
!    Statistical Computing,
!    Dekker, 1980.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0D+00 < A,
!    0.0D+00 < B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random 
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF. 
*/
                 

                     
		      __m256d 
                      beta_sample_ymm4r8(const __m256d a,
                                         const __m256d b,
                                         __m256i * __restrict seed) {
                         
                          __m256d C1 = _mm256_set1_pd(1.0);
                          __m256d C2 = _mm256_set1_pd(2.0);
                          __m256d C05= _mm256_set1_pd(0.5);
                          register __m256d mu,stdev,test,u,y;
                          register __m256d a1,b1,ab2,l0,l1,l2,t0;
                          register __m256d t1,t2,t3;
                          register __m256d x;
                          ab2 = _mm256_sub_pd(_mm256_add_pd(a,b),C2);
                          a1  = _mm256_sub_pd(a,C1);
                          b1  = _mm256_sub_pd(b,C1);
                          mu  = _mm256_div_pd(a1,ab2);
                          stdev = _mm256_div_pd(C05,_mm256_sqrt_pd(ab2));
                          while(true) {
                              
                              y = normal_01_sample_ymm4r8(*seed);
                              x = _mm256_add_pd(mu,_mm256_mul_pd(stdev,y));
                              if(_mm256_cmp_pd_mask(x,_mm256_setzero_pd(),_CMP_LT_OQ) || 
                                 _mm256_cmp_pd_mask(C1,x,_CMP_LT_OQ)) continue;
                              t0= _mm256_mul_pd(C05,_mm256_mul_pd(y,y));
                              u = uniform_01_ymm4r8(*seed);

                              l0 = _mm256_log_pd(_mm256_div_pd(x,a1));
                              t1 = _mm256_mul_pd(a1,l0);
                              l1 = _mm256_log_pd(_mm256_div_pd(_mm256_sub_pd(C1,x),b1));
                              t2 = _mm256_mul_pd(b1,l1);
                              l2 = _mm256_add_pd(_mm256_log_pd(ab2),t0); 
                              t3 = _mm256_mul_pd(ab2,l2);
                              test = _mm256_add_pd(t1,_mm256_add_pd(t2,t3));
                              if(_mm256_cmp_pd_mask(_mm256_log_pd(u),test,_CMP_LE_OQ)) break;
                                   
                          }  
                          return (x);                 
                    }
                    
                    
                   
		      __m256 
                      beta_sample_ymm8r4(const __m256d a,
                                          const __m256d b,
                                          __m256i * __restrict seed) {
                        
                         register __m256 sample;
                         sample = _mm256_castpd_ps(beta_sample_ymm4r8(a,b,*seed));
                         return (sample);                      
                   }
                   
                   
/*
 !*****************************************************************************80
!
!! CAUCHY_CDF_INV inverts the Cauchy CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0D+00 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0D+00 < B.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
!
*/     


                          
                      __m256d 
                      cauchy_cdf_inv_ymm4r8(const __m256d a,
                                            const __m256d b,
                                            const __m256d x) {
                           
                         const __m256d C314159265358979323846264 = 
                                               __m256_set1_pd(3.14159265358979323846264);
                         const __m256d C05 = _mm256_set1_pd(0.5);    
                         register __m256d cdf,t0,t1;
                         t0 = _mm256_mul_pd(C314159265358979323846264,
                                            _mm256_sub_pd(cdf,C05));
                         t1 = _mm256_tan_pd(t0);
                             
                         cdf = _mm256_fmadd_pd(a,b,t1);
                         return (cdf);
                   }    
                   
                   
                          
                      __m256
                      cauchy_cdf_inv_ymm8r4(const __m256 a,
                                            const __m256 b,
                                            const __m256 x) {
                           
                         const __m256 C314159265358979323846264 = 
                                               __m256_set1_pd(3.14159265358979323846264f);
                         const __m256 C05 = _mm256_set1_ps(0.5);    
                         register __m256 cdf,t0,t1;
                         t0 = _mm256_mul_ps(C314159265358979323846264,
                                            _mm256_sub_ps(cdf,C05));
                         t1 = _mm256_tan_ps(t0);
                                    
                         cdf = _mm256_fmadd_ps(a,b,t1);
                         return (cdf);
                   }  
                                          
                   		     
                   
/*
   !*****************************************************************************80
!
!! CAUCHY_SAMPLE samples the Cauchy PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0D+00 < B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random 
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
*/            
#if defined(__ICC) || defined(__INTEL_COMPILER)
#include <svrng.h>
#else
#error 'Required Intel Compiler distribution'
#endif

#include <math.h> // NAN
                         
                      __m256d 
                      cauchy_sample_ymm4r8(const __m256d a,
                                           const __m256d b) {
                         __m256d cdf;
			 svrng_engine_t engine;
			 svrng_distribution_t uniform;
			 uint32_t seed    = 0U;
			 int32_t result   = -9999;
			 int32_t err      = -9999;
			 result           = _rdrand32_step(&seed);
			 if(!result) seed = 1043915199U;
			 engine           = svrng_new_mt19937_engine(seed);
			 err              = svrng_get_status();
			 if(err!=SVRNG_STATUS_OK) {
                            const __m256d nan = _mm256_set1_pd(NAN);
			    return (nan);
			 }
			 uniform          = svrng_new_normal_distribution_double(0.0,1.0);
			 const double * __restrict ptr = (const double*)(&svrng_generate4_double(engine,uniform));
			 cdf              = cauchy_cdf_inv_ymm4r8(_mm256_loadu_pd(&ptr[0]));
			 svrng_delete_engine(engine);
			 return (cdf);                 
                    }
                    
                    
                          
                      __m256 
                      cauchy_sample_ymm8r4(const __m256 a,
                                            const __m256 b) {
                         __m256 cdf;
			 svrng_engine_t engine;
			 svrng_distribution_t uniform;
			 uint32_t seed    = 0U;
			 int32_t result   = -9999;
			 int32_t err      = -9999;
			 result           = _rdrand32_step(&seed);
			 if(!result) seed = 1043915199U;
			 engine           = svrng_new_mt19937_engine(seed);
			 err              = svrng_get_status();
			 if(err!=SVRNG_STATUS_OK) {
                            const __m256 nan = _mm256_set1_ps(nanf(""));
			    return (nan);
			 }
			 uniform          = svrng_new_normal_distribution_float(0.0f,1.0f);
			 const float * __restrict ptr = (const float*)(&svrng_generate8_float(engine,uniform));
			 cdf              = cauchy_cdf_inv_ymm8r4(_mm256_loadu_ps(&ptr[0]));
			 svrng_delete_engine(engine);
			 return (cdf);                 
                    }
 		      
       
/*
                      __ATTR_REGCALL__
                      __ATTR_ALWAYS_INLINE__
		      __ATTR_HOT__
		      __ATTR_ALIGN__(32)
		      static inline
		      __m256d anglit_sample_ymm4r8() {

                         __m256d cdf;
			 svrng_engine_t engine;
			 svrng_distribution_t uniform;
			 uint32_t seed    = 0U;
			 int32_t result   = -9999;
			 int32_t err      = -9999;
			 result           = _rdrand32_step(&seed);
			 if(!result) seed = 1563548129U;
			 engine           = svrng_new_mt19937_engine(seed);
			 err              = svrng_get_status();
			 if(err!=SVRNG_STATUS_OK) {
                            const __m256d nan = _mm256_set1_pd(std::numeric_limits<double>::quiet_NaN());
			    return (nan);
			 }
			 uniform          = svrng_new_uniform_distribution_double(0.0,1.0);
			 const double * __restrict ptr = (const double*)(&svrng_generate4_double(engine,uniform));
			 cdf              = anglit_cdf_inv_ymm4r8(_mm256_loadu_pd(&ptr[0]));
			 svrng_delete_engine(engine);
			 return (cdf);
		    }


		      __ATTR_REGCALL__
                      __ATTR_ALWAYS_INLINE__
		      __ATTR_HOT__
		      __ATTR_ALIGN__(32)
		      static inline
		      __m256d anglit_sample_ymm4r8(const __m256 cdf) {

                            return (anglit_cdf_inv_ymm4r8(cdf));
		    }
*/


/*
      !*****************************************************************************80
!
!! ARCSIN_CDF evaluates the Arcsin CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) A, the parameter of the CDF.
!    A must be positive.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
*/
		     
		      __m256d arcsin_cdf_ymm4r8(const __m256d x,
		                                const __m256d a) {

                         const __m256d invpi = _mm256_set1_pd(0.318309886183790671537767526745);
			 const __m256d _0    = _mm256_setzero_pd();
			 const __m256d _1_2  = _mm256_set1_pd(0.5);
			 const __m256d _1    = _mm256_set1_pd(1.0);
			 __m256d t0,cdf;
			 __mmask8 m0,m1;
			 m0  = _mm256_cmp_pd_mask(x,ymm4r8_negate(a),_CMP_LE_OQ);
                         t0  = _mm256_mul_pd(_mm256_asin_pd(_mm256_div_pd(x,a),invpi));
			 m1  = _mm256_cmp_pd_mask(x,a,_CMP_LT_OQ);
                         cdf = _mm256_mask_blend_pd(m0,_mm256_add_pd(_1_2,t0),_0);
			 cdf = _mm256_mask_blend_pd(m1,cdf,_1); 
                         return (cdf);
		   }


		   
		  
		      __m256 arcsin_cdf_ymm8r4(const __m256 x,
		                                const __m256 a) {

                         const __m256 invpi = _mm256_set1_ps(0.318309886183790671537767526745f);
			 const __m256 _0    = _mm256_setzero_ps();
			 const __m256 _1_2  = _mm256_set1_ps(0.5f);
			 const __m256 _1    = _mm256_set1_ps(1.0f);
			 __m256 t0,cdf;
			 __mmask16 m0,m1;
			 m0  = _mm256_cmp_ps_mask(x,ymm8r4_negate(a),_CMP_LE_OQ);
                         t0  = _mm256_mul_ps(_mm256_asin_ps(_mm256_div_ps(x,a),invpi));
			 m1  = _mm256_cmp_ps_mask(x,a,_CMP_LT_OQ);
                         cdf = _mm256_mask_blend_ps(m0,_mm256_add_pd(_1_2,t0),_0);
			 cdf = _mm256_mask_blend_ps(m1,cdf,_1); 
                         return (cdf);
		   }


		    
		      __m256d arcsin_cdf_inv_ymm4r8(const __m256d cdf,
		                                    const __m256d a) {

                           const __m256d pi    = _mm256_set1_pd(3.14159265358979323846264338328);
			   const __m256d _0    = _mm256_setzero_pd();
			   const __m256d _1    = _mm256_set1_pd(1.0);
			  
			   const __m256d _1_2  = _mm256_set1_pd(0.5);
			   __m256d x;
			
                             x = _mm256_mul_pd(_mm256_sin_pd(_mm256_mul_pd(pi,_mm256_sub_pd(cdf,_1_2))));
                             return (x);
		   }


		   
		      __m256 arcsin_cdf_inv_ymm8r4(const __m256 cdf,
		                                    const __m256 a) {

                           const __m256 pi    = _mm256_set1_ps(3.14159265358979323846264338328f);
			   const __m256 _0    = _mm256_setzero_ps();
			   const __m256 _1    = _mm256_set1_ps(1.0f);
			 
			   const __m256 _1_2  = _mm256_set1_ps(0.5f);
			   __m256 x;
			 
                             x = _mm256_mul_ps(_mm256_sin_ps(_mm256_mul_ps(pi,_mm256_sub_ps(cdf,_1_2))));
                             return (x);
		   }


		    
                      __m256d arcsin_mean_ymm4r8() {

		            return (_mm256_setzero_pd());
		      }


                      __m256 arcsin_mean_ymm8r4() {

		            return (_mm256_setzero_ps());
		      }
		      
		      
/*
!*****************************************************************************80
!
!! ARCSIN_PDF evaluates the Arcsin PDF.
!
!  Discussion:
!
!    The LOGISTIC EQUATION has the form:
!
!      X(N+1) = 4.0D+00 * LAMBDA * ( 1.0D+00 - X(N) ).
!
!    where 0 < LAMBDA <= 1.  This nonlinear difference equation maps
!    the unit interval into itself, and is a simple example of a system
!    exhibiting chaotic behavior.  Ulam and von Neumann studied the
!    logistic equation with LAMBDA = 1, and showed that iterates of the
!    function generated a sequence of pseudorandom numbers with
!    the Arcsin probability density function.
!
!    The derived sequence
!
!      Y(N) = ( 2 / PI ) * Arcsin ( SQRT ( X(N) ) )
!
!    is a pseudorandom sequence with the uniform probability density
!    function on [0,1].  For certain starting values, such as X(0) = 0, 0.75,
!    or 1.0D+00, the sequence degenerates into a constant sequence, and for
!    values very near these, the sequence takes a while before becoming
!    chaotic.
!
!    The formula is:
!
!      PDF(X) = 1 / ( pi * sqrt ( A^2 - X^2 ) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Daniel Zwillinger, Stephen Kokoska,
!    CRC Standard Probability and Statistics Tables and Formulae,
!    Chapman and Hall/CRC, 2000, pages 114-115.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    -A < X < A.
!
!    Input, real ( kind = 8 ) A, the parameter of the CDF.
!    A must be positive.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
*/


                   
		      __m256d arcsin_pdf_ymm4r8(const __m256d x,
		                                const __m256d a) {

                           const __m256d pi    = _mm256_set1_pd(3.14159265358979323846264338328);
			   const __m256d _0    = _mm256_setzero_pd();
			   const __m256d _1    = _mm256_set1_pd(1.0);
			 
			   __m256d pdf,t0;
			   __mmask8 m,m1;
			  
			   m  =  _mm256_cmp_pd_mask(x,ymm4r8_negate(a),_CMP_LE_OQ);
			   t0 =  _mm256_sqrt_pd(_mm256_sub_pd(_mm256_mul_pd(a,a),
			                                      _mm256_mul_pd(x,x)));
			   m1 = _mm256_cmp_pd_mask(x,a,_CMP_GE_OQ);
			   __mmask8 m2 = m || m1;
			   pdf = _mm256_mask_blend_pd(m2,_mm256_div_pd(_1,
			                                           _mm256_mul_pd(pi,t0)),_0);
			   return (pdf);
			   
		    }


		    
		      __m256 arcsin_pdf_ymm8r4(const __m256 x,
		                                const __m256 a) {

                           const __m256 pi    = _mm256_set1_ps(3.14159265358979323846264338328f);
			   const __m256 _0    = _mm256_setzero_ps();
			   const __m256 _1    = _mm256_set1_ps(1.0f);
			 
			   __m256 pdf,t0;
			   __mmask 16m,m1;
			  
			   m  =  _mm256_cmp_ps_mask(x,ymm8r4_negate(a),_CMP_LE_OQ);
			   t0 =  _mm256_sqrt_ps(_mm256_sub_ps(_mm256_mul_ps(a,a),
			                                      _mm256_mul_ps(x,x)));
			   m1 = _mm256_cmp_ps_mask(x,a,_CMP_GE_OQ);
			   const __mmask16 m2 = m || m1;
			   pdf = _mm256_mask_blend_ps(m2,_mm256_div_ps(_1,
			                                           _mm256_mul_ps(pi,t0)),_0);
			   return (pdf);
			   
		    }
		    
		    
/*
!*****************************************************************************80
!
!! ARCSIN_VARIANCE returns the variance of the Arcsin PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the CDF.
!    A must be positive.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!		    
*/


                    
                       __m256d arcsin_variance_ymm4r8(const __m256d a) {

                         const __m256d _1_2 = _mm256_set1_pd(0.5);
			 __m256d variance;
			 variance = _mm256_mul_pd(a,_mm256_mul_pd(a,_1_2));
			 return (variance);
		     }


		     
                       __m256 arcsin_variance_ymm8r4(const __m256 a) {

                         const __m256 _1_2 = _mm256_set1_ps(0.5f);
			 __m256 variance;
			 variance = _mm256_mul_ps(a,_mm256_mul_ps(a,_1_2));
			 return (variance);
		     }
		     
		     
/*
!*****************************************************************************80
!
!! ARCSIN_SAMPLE samples the Arcsin PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the CDF.
!    A must be positive.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
*/
		    
		      __m256d arcsin_sample_ymm4r8() {

                         __m256d cdf;
			 svrng_engine_t engine;
			 svrng_distribution_t uniform;
			 uint32_t seed    = 0U;
			 int32_t result   = -9999;
			 int32_t err      = -9999;
			 result           = _rdrand32_step(&seed);
			 if(!result) seed = 1043915199U;
			 engine           = svrng_new_mt19937_engine(seed);
			 err              = svrng_get_status();
			 if(err!=SVRNG_STATUS_OK) {
                            const __m256d nan = _mm256_set1_pd(NAN);
			    return (nan);
			 }
			 uniform          = svrng_new_uniform_distribution_double(0.0,1.0);
			 const double * __restrict ptr = (const double*)(&svrng_generate4_double(engine,uniform));
			 cdf              = arcsin_cdf_inv_ymm4r8(_mm256_loadu_pd(&ptr[0]));
			 svrng_delete_engine(engine);
			 return (cdf);
		    }


		    
		      __m256d arcsin_sample_ymm4r8(const __m256 cdf) {

                            return (arcsin_cdf_inv_ymm4r8(cdf));
		    }
		    

/*

!*****************************************************************************80
!
!! NORMAL_01_CDF evaluates the Normal 01 CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    AG Adams,
!    Algorithm 39,
!    Areas Under the Normal Curve,
!    Computer Journal,
!    Volume 12, pages 197-198, 1969.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
*/


                     
		      __m256d  
		      normal_01_cdf_ymm8r4(const __m256d x) {
		          
		          __m256d a1 = _mm256_set1_pd(0.398942280444e+00);
		          __m256d a2 = _mm256_set1_pd(0.399903438504e+00);
		          __m256d a3 = _mm256_set1_pd(5.75885480458e+00);
                          __m256d a4 = _mm256_set1_pd(29.8213557808e+00);
                          __m256d a5 = _mm256_set1_pd(2.62433121679e+00);
                          __m256d a6 = _mm256_set1_pd(48.6959930692e+00);
                          __m256d a7 = _mm256_set1_pd(5.92885724438e+00);
                          __m256d b0 = _mm256_set1_pd(0.398942280385e+00);
                          __m256d b1 = _mm256_set1_pd(3.8052e-08);
                          __m256d b2 = _mm256_set1_pd(1.00000615302e+00);
                          __m256d b3 = _mm256_set1_pd(3.98064794e-04);
                          __m256d b4 = _mm256_set1_pd(1.98615381364e+00);
                          __m256d b5 = _mm256_set1_pd(0.151679116635e+00);
                          __m256d b6 = _mm256_set1_pd(5.29330324926e+00);
                          __m256d b7 = _mm256_set1_pd(4.8385912808e+00);
                          __m256d b8 = _mm256_set1_pd(15.1508972451e+00);
                          __m256d b9 = _mm256_set1_pd(0.742380924027e+00);
                          __m256d b10= _mm256_set1_pd(30.789933034e+00);
                          __m256d b11= _mm256_set1_pd(3.99019417011e+00);
                          __m256d C1 = _mm256_set1_pd(1.0);
                          __m256d C128 = _mm256_set1_pd(1.28);
                          __m256d C05  = _mm256_set1_pd(0.5);
                          __m256d C127 = _mm256_set1_pd(12.7);
                          __m256d absx,y,q,cdf,t0,t1;
                          __mmask8 m0,m1,m2;
                          m2   = _mm256_cmp_pd_mask(x,_mm256_setzero_pd(),_CMP_LT_OQ);
                          absx = _mm256_abs_pd(x);
                          m0   = _mm256_cmp_pd_mask(x,C128,_CMP_LE_OQ);
                          y    = _mm256_mul_pd(C05,
                                        _mm256_mul_pd(x,x));
                          m1   = _mm256_cmp_pd_mask(x,C127,_CMP_LE_OQ);
                          if(m0) {
                             register __m256d ya3;
                             register __m256d ya5a6
                             register __m256d ya7;
                             register __m256d a2y;
                             ya7   = _mm256_add_pd(y,a7);
                             ya5a6 = _mm256_add_pd(y,_mm256_add_pd(a5,a6));
                             a2y   = _mm256_mul_pd(a2,y);
                             ya3a4 = _mm256_sub_pd(_mm256_add_pd(y,a3),a4);
                             q     = _mm256_sub_pd(a1,
                                           _mm256_div_pd(a2y,
                                                  _mm256_div_pd(ya3a4,
                                                        _mm256_div_pd(ya5a6,ya7))));
                          }
                          else if(m1) {
                             register __m256d expmy;
                             register __m256d absb1;
                             register __m256d absb3;
                             register __m256d absb5;
                             register __m256d absb7;
                             register __m256d absb9;
                             register __m256d absb11;

                             expmy = _mm256_mul_pd(_mm256_exp_pd(negate_ymm4r8(y)),b0); 
                             absb1 = _mm256_sub_pd(absx,b1);
                             absb3 = _mm256_add_pd(absx,b3);
                             absb5 = _mm256_sub_pd(absx,b5);
                             absb7 = _mm256_add_pd(absx,b7);
                             absb9 = _mm256_add_pd(absx,b9);
                             absb11= _mm256_add_pd(absx,b11);
                             t0    = (absb1+b2/(absb3+b4/(absb5+b6/(absb7-b8/(absb9+b10/(absb11))))));
                             q     = _mm256_div_pd(expmy,t0);
                          }
                          else {
                             q = _mm256_setzero_pd();
                          }
                          
                          cdf = _mm256_mask_blend_pd(m2,_mm256_sub_pd(C1,q),q);
                          return (cdf);
		    }
		    
		    
		    
		      __m256  
		      normal_01_cdf_ymm8r4(const __m256 x) {
		          
		          __m256 a1 = _mm256_set1_ps(0.398942280444f);
		          __m256 a2 = _mm256_set1_ps(0.399903438504f);
		          __m256 a3 = _mm256_set1_ps(5.75885480458f);
                          __m256 a4 = _mm256_set1_ps(29.8213557808f);
                          __m256 a5 = _mm256_set1_ps(2.62433121679f);
                          __m256 a6 = _mm256_set1_ps(48.6959930692f);
                          __m256 a7 = _mm256_set1_ps(5.92885724438f);
                          __m256 b0 = _mm256_set1_ps(0.398942280385f);
                          __m256 b1 = _mm256_set1_ps(3.8052e-08f);
                          __m256 b2 = _mm256_set1_ps(1.00000615302f);
                          __m256 b3 = _mm256_set1_ps(3.98064794e-04f);
                          __m256 b4 = _mm256_set1_ps(1.98615381364f);
                          __m256 b5 = _mm256_set1_ps(0.151679116635f);
                          __m256 b6 = _mm256_set1_ps(5.29330324926f);
                          __m256 b7 = _mm256_set1_ps(4.8385912808f);
                          __m256 b8 = _mm256_set1_ps(15.1508972451f);
                          __m256 b9 = _mm256_set1_ps(0.742380924027f);
                          __m256 b10= _mm256_set1_ps(30.789933034f);
                          __m256 b11= _mm256_set1_ps(3.99019417011f);
                          __m256 C1 = _mm256_set1_ps(1.0);
                          __m256 C128 = _mm256_set1_ps(1.28f);
                          __m256 C05  = _mm256_set1_ps(0.5f);
                          __m256 C127 = _mm256_set1_ps(12.7f);
                          __m256 absx,y,q,cdf,t0;
                          __mmask8 m0,m1,m2;
                          m2   = _mm256_cmp_ps_mask(x,_mm256_setzero_pd(),_CMP_LT_OQ);
                          absx = _mm256_abs_ps(x);
                          m0   = _mm256_cmp_ps_mask(x,C128,_CMP_LE_OQ);
                          y    = _mm256_mul_ps(C05,
                                        _mm256_mul_ps(x,x));
                          m1   = _mm256_cmp_ps_mask(x,C127,_CMP_LE_OQ);
                          if(m0) {
                             register __m256 ya3;
                             register __m256 ya5a6
                             register __m256 ya7;
                             register __m256 a2y;
                             ya7   = _mm256_add_ps(y,a7);
                             ya5a6 = _mm256_add_ps(y,_mm256_add_ps(a5,a6));
                             a2y   = _mm256_mul_ps(a2,y);
                             ya3a4 = _mm256_sub_ps(_mm256_add_ps(y,a3),a4);
                             q     = _mm256_sub_ps(a1,
                                           _mm256_div_ps(a2y,
                                                  _mm256_div_ps(ya3a4,
                                                        _mm256_div_ps(ya5a6,ya7))));
                          }
                          else if(m1) {
                             register __m256 expmy;
                             register __m256 absb1;
                             register __m256 absb3;
                             register __m256 absb5;
                             register __m256 absb7;
                             register __m256 absb9;
                             register __m256 absb11;

                             expmy = _mm256_mul_ps(_mm256_exp_ps(negate_ymm8r4(y)),b0); 
                             absb1 = _mm256_sub_ps(absx,b1);
                             absb3 = _mm256_add_ps(absx,b3);
                             absb5 = _mm256_sub_ps(absx,b5);
                             absb7 = _mm256_add_ps(absx,b7);
                             absb9 = _mm256_add_ps(absx,b9);
                             absb11= _mm256_add_ps(absx,b11);
                             t0    = (absb1+b2/(absb3+b4/(absb5+b6/(absb7-b8/(absb9+b10/(absb11))))));
                             q     = _mm256_div_ps(expmy,t0);
                          }
                          else {
                             q = _mm256_setzero_ps();
                          }
                          
                          cdf = _mm256_mask_blend_ps(m2,_mm256_sub_ps(C1,q),q);
                          return (cdf);
		    }
		    
		    
/*

      BRADFORD_CDF evaluates the Bradford CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    A < B,
!    0.0D+00 < C.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!

*/
		    
        
        	   
		      __m256d 
		      bradford_cdf_ymm4r8(const __m256d x,
		                          const __m256d a,
		                          const __m256d b,
		                          const __m256d c) {
		          
		            register __m256d C1 = _mm2562_set1_pd(1.0);
		            register __m256d cp1,xa,ba,ratio,l0,l1;
		            register __m256d cdf;
		            cp1 = _mm256_add_pd(c,C1);
		            xa  = _mm256_sub_pd(x,a);
		            ba  = _mm256_sub_pd(b,a);
                            l1  = _mm256_log_pd(cp1);
		             
                            ratio = _mm256_div_pd(_mm256_mul_pd(c,xa),ba);
                            l0  = _mm256_log_pd(_mm256_add_pd(C1,ratio));
       
                            cdf = _mm256_div_pd(l0,l1);
                            return (cdf);                   
		     }
		     
		     
		    
		      __m256 
		      bradford_cdf_ymm8r4(const __m256 x,
		                          const __m256 a,
		                          const __m256 b,
		                          const __m256 c) {
		          
		            register __m256 C1 = _mm256_set1_ps(1.0f);
		            register __m256 cp1,xa,ba,ratio,l0,l1;
		            register __m256 cdf;
		            cp1 = _mm256_add_ps(c,C1);
		            xa  = _mm256_sub_ps(x,a);
		            ba  = _mm256_sub_ps(b,a);
                            l1  = _mm256_log_ps(cp1);
		                         
                             ratio = _mm256_div_ps(_mm256_mul_ps(c,xa),ba);
                            l0  = _mm256_log_ps(_mm256_add_ps(C1,ratio));
        
                            cdf = _mm256_div_ps(l0,l1);
                            return (cdf);                   
		     }
		     
		     
/*

      !*****************************************************************************80
!
!! BRADFORD_CDF_INV inverts the Bradford CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0D+00 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    A < B,
!    0.0D+00 < C.
!
!    Output, real ( kind = 8 ) X, the corresponding argument of the CDF.
!

*/


                   
		      __m256d 
		      bradford_cdf_inv_ymm4r8(const __m256d cdf,
		                              const __m256d a,
		                              const __m256d b,
		                              const __m256d c) {
		        
		          register __m256d C1 = _mm256_set1_pd(1.0);
		          register __m256d ba,cp1,pow,t0;
		          register __m256d x;
		                	        
		              ba  = _mm256_sub_pd(b,a);
		              cp1 = _mm256_add_pd(c,C1);
		              pow = _mm256_sub_pd(_mm256_pow_pd(C1,cdf),C1);
		              t0  = _mm256_div_pd(pow,c);
		              x   = _mm256_mul_pd(_mm256_add_pd(a,ba),t0);
		              return (x);
		         		                     
		    }	
		    
		    
		   
		      __m256 
		      bradford_cdf_inv_ymm8r4(const __m256 cdf,
		                               const __m256 a,
		                               const __m256 b,
		                               const __m256 c) {
		        
		          register __m256 C1 = _mm256_set1_ps(1.0f);
		          register __m256 ba,cp1,pow,t0;
		          register __m256 x;
		          
		              ba  = _mm256_sub_ps(b,a);
		              cp1 = _mm256_add_ps(c,C1);
		              pow = _mm256_sub_ps(_mm256_pow_ps(C1,cdf),C1);
		              t0  = _mm256_div_ps(pow,c);
		              x   = _mm256_mul_ps(_mm256_add_ps(a,ba),t0);
		              return (x);
		                		                     
		    }	
		    
		     
/*
         
        !*****************************************************************************80
!
!! BRADFORD_MEAN returns the mean of the Bradford PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    A < B,
!    0.0D+00 < C.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!  

*/
	
	            
		      __m256d  
		      bradford_mean_ymm4r8(const __m256d a,
		                           const __m256d b,
		                           const __m256d c) {
		       
		         register __m256d C1 = _mm256_set1_pd(1.0);
		         register __m256d t1,t2,cp1,cba,acb,l0,l1;
		         register __m256d mean;
		         cp1  = _mm256_add_pd(c,C1);
		         cba  = _mm256_mul_pd(c,_mm256_sub_pd(b,a));
		         acb  = _mm256_sub_pd(_mm256_mul_pd(a,c1),b);

                         l0   = _mm256_log_pd(cp1);

                         t2   = _mm256_mul_pd(c,l0);
                         t1   = _mm256_mul_pd(l0,acb);
                         mean = _mm256_div_pd(_mm256_add_pd(cba,t1),t2);
                         return (mean);   		                                  
		   }
		   
		   
		   
		      __m256  
		      bradford_mean_ymm8r4(const __m256 a,
		                           const __m256 b,
		                           const __m256 c) {
		       
		         register __m256 C1 = _mm256_set1_ps(1.0f);
		         register __m256 t1,t2,cp1,cba,acb,l0,l1;
		         register __m256 mean;
		         cp1  = _mm256_add_ps(c,C1);
		         cba  = _mm256_mul_ps(c,_mm256_sub_ps(b,a));
		         acb  = _mm256_sub_ps(_mm256_mul_ps(a,c1),b);

                         l0   = _mm256_log_ps(cp1);

                         t2   = _mm256_mul_ps(c,l0);
                         t1   = _mm256_mul_ps(l0,acb);
                         mean = _mm256_div_ps(_mm256_add_ps(cba,t1),t2);
                         return (mean);   		                                  
		   }
		   
		   
/*

      BRADFORD_PDF evaluates the Bradford PDF.
!
!  Discussion:
!
!    The formula is:
!
!      PDF(A,B,C;X) =
!        C / ( ( C * ( X - A ) + B - A ) * log ( C + 1 ) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    A <= X
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    A < B,
!    0.0D+00 < C.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.   

*/	


      	             
		      __m256d  
		      bradford_pdf_ymm4r8(const __m256d a,
		                           const __m256d b,
		                           const __m256d c) {
		          
		           register __m256d C1 = _mm256_set1_pd(1.0);
		           register __m256d l0,c1p,xa,ba,t0,t1;
		           register __m256d pdf;
		           c1p = _mm256_add_pd(c,C1);
		           xa  = _mm256_sub_pd(x,a);
		           ba  = _mm256_sub_pd(b,a);

                           l0   = _mm256_log_pd(cp1);
		
                           t0   = _mm256_fmadd_pd(c,xa,ba);
                           t1   = _mm256_mul_pd(t0,l0);
                           pdf  = _mm256_div_pd(c,t1);
                           return (pdf);                              
		    }
		    
		    
		   
		      __m256  
		      bradford_pdf_ymm8r4(const __m256 a,
		                           const __m256 b,
		                           const __m256 c) {
		          
		           register __m256 C1 = _mm256_set1_ps(1.0f);
		           register __m256 l0,c1p,xa,ba,t0,t1;
		           register __m256 pdf;
		           c1p = _mm256_add_ps(c,C1);
		           xa  = _mm256_sub_ps(x,a);
		           ba  = _mm256_sub_ps(b,a);

                           l0   = _mm256_log_ps(cp1);
	                   t0   = _mm256_fmadd_ps(c,xa,ba);
                           t1   = _mm256_mul_ps(t0,l0);
                           pdf  = _mm256_div_ps(c,t1);
                           return (pdf);                              
		    }
		    
		    
/*
!*****************************************************************************80
!
!! BETA_BINOMIAL_CDF evaluates the Beta Binomial CDF.
!
!  Discussion:
!
!    A simple summing approach is used.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) A, B, parameters of the PDF.
!    0.0D+00 < A,
!    0.0D+00 < B.
!
!    Input, integer ( kind = 4 ) C, a parameter of the PDF.
!    0 <= C.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
*/


                      __ATTR_REGCALL__
                      __ATTR_ALWAYS_INLINE__
		      __ATTR_HOT__
		      __ATTR_ALIGN__(32)
		      static inline
		      __m256d beta_binomial_cdf_ymm4r8(const int32_t x,
		                                       const int32_t c,
						       const __m256d a,
						       const __m256d b) {

			      const __m256d _0  = _mm256_setzero_pd();
                              const __m256d _1  = _mm256_set1_pd(1.0);
			      __m256d vx,vy,vcy,vc1,vy1,vcy1;
			      __m256d cdf,pdf;
                              int32_t y;
			      if(x<0) {
                                 cdf = _0;
			      }
			      else if(x<c) {
                                 cdf = _0;
				 for(y = 0; y < x; ++y) {
                                     vy  = _mm256_set1_pd((double)y);
				     vx  = _mm256_set1_pd((double)x);
				     vcy = _mm256_set1_pd((double)(c-y));
				     vc1 = _mm256_set1_pd((double)(c+1));
				     vy1 = _mm256_set1_pd((double)(y+1));
				     vcy1= _mm256_set1_pd((double)(c-y+1));
				     const __m256d t0 = beta_ymm4r8(_mm256_add_pd(a,vy),
				                                    _mm256_add_pd(b,vcy));
				     const __m256d t1 = _mm256_mul_pd(vc1,beta_ymm4r8(vy1,vcy1));
				     const __m256d t2 = beta_ymm4r8(a,b);
				     pdf              = _mm256_div_pd(t0,_mm256_mul_pd(t1,t2));
				     cdf              = _mm256_add_pd(cdf,pdf);
				 }
			      }
			      else if(c<=x) {
                                  cdf = _1;
			      }
			      return (cdf);
		    }
		    
		    
/*!*****************************************************************************80
!
!! BETA_PDF evaluates the Beta PDF.
!
!  Discussion:
!
!    The formula for the PDF is:
!
!      PDF(A,B;X) = X**(A-1) * (1-X)**(B-1) / BETA(A,B).
!
!    A = B = 1 yields the Uniform distribution on [0,1].
!    A = B = 1/2 yields the Arcsin distribution.
!        B = 1 yields the power function distribution.
!    A = B -> Infinity tends to the Normal distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    0.0D+00 <= X <= 1.0.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0D+00 < A,
!    0.0D+00 < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!*/

                    
		      __m256d beta_pdf_ymm4r8(const __m256d x,
		                              const __m256d a,
					      const __m256d b) {

                         const __m256d _0 = _mm256_setzero_pd();
			 const __m256d _1 = _mm256_set1_pd(1.0);
			 const __m256d t0 = _mm256_sub_pd(a,_1);
			 const __m256d t1 = _mm256_sub_pd(_1,x);
			 const __m256d t2 = _mm256_sub_pd(b,_1);
			 __m256d pdf,term1,term2,term3;
			 __mmask8 m0,m1,m2;
			 term1            = _mm256_pow_pd(x,t0);
			 m0               = _mm256_cmp_pd_mask(x,_0,_CMP_LT_OQ);
			 term2            = _mm256_mul_pd(term1,_mm256_pow_pd(t1,t2));
			 m1               = _mm256_cmp_pd_mask(x,_1,_CMP_LT_OQ);
			 term3            = _mm256_div_pd(term2,beta_ymm4r8(a,b));
			 m                = m1||m2;
			 pdf              = _mm256_mask_blend_pd(m,term3,_0);
			 return (pdf);
		    }


		   
		      __m256
		      beta_pdf_ymm8r4(const __m256 x,
		                       const __m256 a,
				       const __m256 b) {

                         const __m256 _0 = _mm256_setzero_ps();
			 const __m256 _1 = _mm256_set1_ps(1.0);
			 const __m256 t0 = _mm256_sub_ps(a,_1);
			 const __m256 t1 = _mm256_sub_ps(_1,x);
			 const __m256 t2 = _mm256_sub_ps(b,_1);
			 __m256 pdf,term1,term2,term3;
			 __mmask16 m0,m1,m2;
			 term1            = _mm256_pow_ps(x,t0);
			 m0               = _mm256_cmp_ps_mask(x,_0,_CMP_LT_OQ);
			 term2            = _mm256_mul_ps(term1,_mm256_pow_pd(t1,t2));
			 m1               = _mm256_cmp_ps_mask(x,_1,_CMP_LT_OQ);
			 term3            = _mm256_div_ps(term2,beta_ymm8r4(a,b));
			 m                = m1||m2;
			 pdf              = _mm256_mask_blend_ps(m,term3,_0);
			 return (pdf);
		    }


		   
		      __m256d
		      beta_variance_ymm4r8(const __m256d a,
		                           const __m256d b) {

			  __m256d variance;
                          const __m256d _1  = _mm256_set1_pd(1.0);
			  const __m256d ab  = _mm256_add_pd(a,b);
			  const __m256d t0  = _mm256_mul_pd(_mm256_mul_pd(ab,ab),
			                                    _mm256_add_pd(_1,ab));
			  variance          = _mm256_div_pd(_mm256_mul_pd(a,b),t0);				   
			  
		    }


		 
		      __m256
		      beta_variance_ymm8r4(const __m256 a,
		                            const __m256 b) {

			  __m256 variance;
                          const __m256 _1  = _mm256_set1_ps(1.0f);
			  const __m256 ab  = _mm256_add_ps(a,b);
			  const __m256 t0  = _mm256_mul_ps(_mm256_mul_ps(ab,ab),
			                                    _mm256_add_ps(_1,ab));
			  variance          = _mm256_div_ps(_mm256_mul_ps(a,b),t0);				   
			  
		    }
		    
		    
/*
!*****************************************************************************80
!
!! WEIBULL_CDF evaluates the Weibull CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!    A <= X.
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0D+00 < B,
!    0.0D+00 < C.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!		    
*/

                      
                     
                      __m256d
		      weibull_cdf_ymm4r8(const __m256d x,
		                         const __m256d a,
					 const __m256d b,
					 const __m256d c) {

                          const __m256d  _0 = _mm256_setzero_pd();
			  const __m256d  _1 = _mm256_set1_pd(1.0);
			  const __m256d  y  = _mm256_div_pd(_mm256_sub_pd(x,a),b);
			  const __m256d  exc= _mm256_exp_pd(_mm256_pow_pd(y,c));
			  __m256d cdf;
			  const __mmask8 m  = _mm256_cmp_pd_mask(a,x,_CMP_LT_OQ);
			  cdf               = _mm256_mask_blend_pd(m,_mm256_sub_pd(_1,
			                                                       _mm256_div_pd(_1,exc)),_0);
			  return (cdf);
		   }
		    
		    
		    
                      __m256
		      weibull_cdf_ymm8r4(const __m256 x,
		                          const __m256 a,
					  const __m256 b,
					  const __m256 c) {

                          const __m256  _0 = _mm256_setzero_ps();
			  const __m256  _1 = _mm256_set1_ps(1.0f);
			  const __m256  y  = _mm256_div_ps(_mm256_sub_ps(x,a),b);
			  const __m256  exc= _mm256_exp_ps(_mm256_pow_ps(y,c));
			  __m256 cdf;
			  const __mmask16 m  = _mm256_cmp_ps_mask(a,x,_CMP_LT_OQ);
			  cdf               = _mm256_mask_blend_ps(m,_mm256_sub_ps(_1,
			                                                       _mm256_div_ps(_1,exc)),_0);
			  return (cdf);
		   }


		    
                      __m256d
		      weibull_cdf_inv_ymm4r8(const __m256d a,
		                             const __m256d b,
					     const __m256d c,
					     const __m256d cdf) {

                        const __m256d  _0  = _mm256_setzero_pd();
			const __m256d  _1  = _mm256_set1_pd(1.0);
			
			__m256d t0,t1,x;
			
			t0                 = ymm4r8_negate(_mm256_log_pd(_mm256_sub_pd(_1,cdf)));
			t1                 = _mm256_pow_pd(t0,_mm256_div_pd(_1,c));
			x                  = _mm256_fmadd_pd(a,b,t1);
			return (x);
			
		   }


		      __ATTR_REGCALL__
                      __ATTR_ALWAYS_INLINE__
		      __ATTR_HOT__
		      __ATTR_ALIGN__(32)
		      static inline
                      __m256
		      weibull_cdf_inv_ymm8r4(const __m256 a,
		                             const __m256 b,
					     const __m256 c,
					     const __m256 cdf) {

                        const __m256  _0  = _mm256_setzero_ps();
			const __m256  _1  = _mm256_set1_ps(1.0f);
			
			__m256 t0,t1,x;
			
			t0                 = ymm8r4_negate(_mm256_log_pd(_mm256_sub_ps(_1,cdf)));
			t1                 = _mm256_pow_ps(t0,_mm256_div_ps(_1,c));
			x                  = _mm256_fmadd_ps(a,b,t1);
			return (x);
			
		   }


		      __ATTR_REGCALL__
                      __ATTR_ALWAYS_INLINE__
		      __ATTR_HOT__
		      __ATTR_ALIGN__(32)
		      static inline
		      __m256d
		      weibull_sample_ymm4r8(const __m256d vrand,
		                            const __m256d a,
					    const __m256d b,
					    const __m256d c) {

                         return (weibull_cdf_ymm4r8(a,b,c,vrand));
		   }


		      __ATTR_REGCALL__
                      __ATTR_ALWAYS_INLINE__
		      __ATTR_HOT__
		      __ATTR_ALIGN__(32)
		      static inline
		      __m256
		      weibull_sample_ymm8r4(const __m256 vrand,
		                            const __m256 a,
					    const __m256 b,
					    const __m256 c) {

                         return (weibull_cdf_ymm8r4(a,b,c,vrand));
		   }
		   
		   
/*
!*****************************************************************************80
!
!! WEIBULL_VARIANCE returns the variance of the Weibull PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0D+00 < B,
!    0.0D+00 < C.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
*/


                     
		      __m256d
                      weibull_discrete_cdf_ymm4r8(const __m256d x,
		                                  const __m256d a,
					          const __m256d b) {

			    __m256d cdf;
                            const __m256d  _0 = _mm256_setzero_pd();
			    const __m256d  _1 = _mm256_set1_pd(1.0);
			    const __m256d  t0 = _mm256_pow_pd(_mm256_add_pd(x,_1),b);
			    const __m256d  t1 = _mm256_pow_pd(_mm256_sub_pd(_1,a),t0);
			    const __mmask8 m  = _mm256_cmp_pd_mask(x,_0,_CMP_LT_OQ);
			    cdf               = _mm256_mask_blend_pd(m,_mm256_sub_pd(_1,t1),_0);
			    return (cdf);
		    }


		     
		      __m256
                      weibull_discrete_cdf_ymm8r4(const __m256 x,
		                                  const __m256 a,
					          const __m256 b) {

			    __m256 cdf;
                            const __m256  _0 = _mm256_setzero_ps();
			    const __m256  _1 = _mm256_set1_ps(1.0f);
			    const __m256  t0 = _mm256_pow_ps(_mm256_add_ps(x,_1),b);
			    const __m256  t1 = _mm256_pow_ps(_mm256_sub_ps(_1,a),t0);
			    const __mmask16 m  = _mm256_cmp_ps_mask(x,_0,_CMP_LT_OQ);
			    cdf               = _mm256_mask_blend_ps(m,_mm256_sub_pd(_1,t1),_0);
			    return (cdf);
		    }


		    
		      __m256d
		      weibull_discrete_pdf_ymm4r8(const __m256d x,
		                                  const __m256d a,
					          const __m256d b) {

                            __m256d pdf;
                            const __m256d  _0 = _mm256_setzero_pd();
			    const __m256d  _1 = _mm256_set1_pd(1.0);
			    const __m256d  t0 = _mm256_pow_pd(_mm256_add_pd(x,_1),b);
			    const __m256d  _1a= _mm256_sub_pd(_1,a);
			    const __m256d  t1 = _mm256_pow_pd(_1a,t0);
                            const __m256d  t2 = _mm256_pow_pd(x,b);
			    const __m256d  t3 = _mm256_pow_pd(_1a,t2);
			    pdf               = _mm256_sub_pd(t3,t1);
			    return (pdf);
		   }


		    
		      __m256
		      weibull_discrete_pdf_ymm8r4(const __m256 x,
		                                  const __m256 a,
					          const __m256 b) {

                            __m256 pdf;
                            const __m256  _0 = _mm256_setzero_ps();
			    const __m256  _1 = _mm256_set1_ps(1.0);
			    const __m256  t0 = _mm256_pow_ps(_mm256_add_ps(x,_1),b);
			    const __m256  _1a= _mm256_sub_ps(_1,a);
			    const __m256  t1 = _mm256_pow_ps(_1a,t0);
                            const __m256  t2 = _mm256_pow_ps(x,b);
			    const __m256  t3 = _mm256_pow_ps(_1a,t2);
			    pdf               = _mm256_sub_ps(t3,t1);
			    return (pdf);
		   }
		   
		   
/*
!*****************************************************************************80
!
!! WEIBULL_DISCRETE_CDF_INV inverts the Discrete Weibull CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 October 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0D+00 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0D+00 <= A <= 1.0D+00,
!    0.0D+00 < B.
!
!    Output, integer ( kind = 4 ) X, the corresponding argument.
*/


		      __m256d
		      weibull_discr_icdf_ymm4r8(const __m256d cdf,
		                                const __m256d a,
						const __m256d b) {

                       
			  const __m256d  _0  = _mm256_setzero_pd();
			  const __m256d  _1  = _mm256_set1_pd(1.0);
			
			  const __m256d t0   =  _mm256_log_pd(_mm256_sub_pd(_1,cdf));
			  const __m256d t1   =  _mm256_log_pd(_mm256_sub_pd(_1,a));
			  const __m256d t2   =  _mm256_div_pd(t1,t2)
			  const __m256d t3   =  _mm256_pow_pd(t2,_mm256_div_pd(_1,b));
			  __m256d x;
			  x                  =  _mm256_ceil_pd(_mm256_sub_pd(t3,_1));
			  return (x);
		    }


		    
		      __m256
		      weibull_discr_icdf_ymm8r4(const __m256 cdf,
		                                const __m256 a,
						const __m256 b) {

                      
			  const __m256  _0  = _mm256_setzero_ps();
			  const __m256  _1  = _mm256_set1_ps(1.0f);
			
			  const __m256 t0   =  _mm256_log_ps(_mm256_sub_ps(_1,cdf));
			  const __m256 t1   =  _mm256_log_ps(_mm256_sub_ps(_1,a));
			  const __m256 t2   =  _mm256_div_ps(t1,t2)
			  const __m256 t3   =  _mm256_pow_ps(t2,_mm256_div_ps(_1,b));
			  __m256 x;
			  x                  =  _mm256_ceil_ps(_mm256_sub_ps(t3,_1));
			  return (x);
		    }


		    

/*
!*****************************************************************************80
!
!! WEIBULL_DISCRETE_SAMPLE samples the discrete Weibull PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0D+00 <= A <= 1.0D+00,
!    0.0D+00 < B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random 
!    number generator.
!
!    Output, integer ( kind = 4 ) X, a sample of the PDF.
!
*/


                         
                      __m256d
		      weibull_discr_samp_ymm4r8(   const __m256d vrand,
		                                   const __m256d a,
						   const __m256d b) {

                         return (weibull_discr_icdf_ymm4r8(vrand,a,b));
		    }
		    
		    
/*
!*****************************************************************************80
!
!! VON_MISES_CDF evaluates the von Mises CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 September 2005
!
!  Author:
!
!    Original FORTRAN77 version by Geoffrey Hill.
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Geoffrey Hill,
!    Algorithm 518,
!    Incomplete Bessel Function I0: The von Mises Distribution,
!    ACM Transactions on Mathematical Software,
!    Volume 3, Number 3, September 1977, pages 279-284.
!
!    Kanti Mardia, Peter Jupp,
!    Directional Statistics,
!    Wiley, 2000,
!    QA276.M335
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!    A - PI <= X <= A + PI.
!
!    Input, real ( kind = 8 ) A, a parameter of the PDF.
!    A is the preferred direction, in radians.
!    -PI <= A <= PI.
!
!    Input, real ( kind = 8 ) B, a parameter of the PDF.
!    B measures the "concentration" of the distribution around the
!    angle A.  B = 0 corresponds to a uniform distribution
!    (no concentration).  Higher values of B cause greater concentration
!    of probability near A.
!    0.0D+00 <= B.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
*/




                        
                      __m256d
		      von_misses_cdf_ymm4r8(const __m256d x,
		                            const __m256d a,
					    const __m256d b) {

                        //Early exit.
			const __m256d   _0  = _mm256_setzero_pd();
			const __m256d   _1  = _mm256_set1_pd(1.0);
			const __m256d   pi  = _mm256_set1_pd(3.14159265358979323846264338328);
			const __m256d   npi = _mm256_set1_pd(-3.14159265358979323846264338328);
			const __m256d   xsa = _mm256_sub_pd(x,a);
			if(__builtin_expect(_mm256_cmp_pd_mask(xsa,npi,_CMP_LE_OQ),0)) {
		             return (_0);
			}
			if(__builtin_expect(_mm256_cmp_pd_mask(npi,xsa,_CMP_LE_OQ),0)) {
                             return (_1); 
			}
			const __m256d  _2pi = _mm256_set1_pd(6.283185307179586476925286766559);
			const __m256d  a1  = _mm256_set1_pd(12.0);
			const __m256d  a2  = _mm256_set1_pd(0.8);
			const __m256d  a3  = _mm256_set1_pd(8.0);
			const __m256d  a4  = _mm256_set1_pd(1.0);
			const __m256d  c1  = _mm256_set1_pd(56.0);
			const __m256d  ck  = _mm256_set1_pd(10.5);
			const __m256d  _2  = _mm256_set1_pd(2.0);
			const __m256d  _1_2= _mm256_set1_pd(0.5);
			__m256d arg,cdf,cn,p,r,s,sn,u,v,y,z,uprv,erfx;
			//  Convert the angle (X - A) modulo 2 PI to the range ( 0, 2 * PI ).
			z    = b;
			u    = _mm256_castps_pd(_fmod_ymm8r4(_mm256_castpd_ps(_mm256_add_pd(xsa,pi)),
			                                           _mm256_castpd_ps(_2pi)));
			uprv = u;
			const __mmask8 m = _mm256_cmp_pd_mask(u,_0,_CMP_LT_OQ);
			u    = _mm256_add_pd(u,_2pi);
			u    = _mm256_mask_blend_pd(m,uprv,u);
			y    = _mm256_sub_pd(u,pi);
			
			//For small B, sum IP terms by backwards recursion.
			// Can not be vectorized manually, hence 0 is returned.
			// Only large B is computed.
			/*
                              This scalar code can not be vectorized.
                              ip = int ( z * a2 - a3 / ( z + a4 ) + a1 )
                              Used as loop control variable
                              do n = 2, ip
                         */
                        if(_mm256_cmp_pd_mask(z,ck,_CMP_LE_OQ)) {
                           return (_0);
			}
			else {
                           const __m256d t0 = _mm256_set1_pd(24.0);
			   const __m256d t1 = _mm256_set1_pd(54.0);
			   const __m256d t2 = _mm256_set1_pd(347.0);
			   const __m256d t3 = _mm256_set1_pd(26.0);
			   const __m256d t4 = _mm256_set1_pd(6.0);
			   const __m256d t5 = _mm256_set1_pd(12.0);
			   const __m256d t6 = _mm256_set1_pd(3.0);
			   const __m256d t7 = _mm256_set1_pd(16.0);
			   const __m256d t8 = _mm256_set1_pd(1.75);
			   const __m256d t9 = _mm256_set1_pd(83.5);
			   c                = _mm256_mul_pd(t0,z);
			   v                = _mm256_sub_pd(c,c1);
			   const __m256d tmp1 = _mm256_sub_pd(_mm256_add_pd(v,t3),c);
			   const __m256d tmp2 = _mm256_div_pd(t1,_mm256_div_pd(t2,tmp1));
			   const __m256d tmp3 = _mm256_add_pd(_mm256_sub_pd(tmp2,t4),c);
			   r                  = _mm256_sqrt_pd(_mm256_div_pd(tmp3,t5));

			   z                  = _mm256_mul_pd(_mm256_sin_pd(
			                                                _mm256_mul_pd(_1_2,y)),r);
                           s                  = _mm256_mul_pd(_2,_mm256_mul_pd(z,z));
			   v                  = _mm256_sub_pd(v,_mm256_add_pd(s,t6));
			   y                  = _mm256_div_pd(_mm256_sub_pd(_mm256_sub_pd(c,s),
			                                                    _mm256_sub_pd(s,t7)),t6);
			   tmp1               = _mm256_sub_pd(v,y);
			   y                  = _mm256_div_pd(_mm256_fmadd_pd(_mm256_add_pd(s,t8),s,t9),tmp1);
			   tmp2               = _mm256_mul_pd(y,y);
			   arg                = _mm256_mul_pd(z,_mm256_sub_pd(_1,
			                                                  _mm256_div_pd(s,tmp2)));
			   erfx               = _mm256_erf_pd(arg);
			   cdf                = _mm256_fmadd_pd(_1_2,erfx,_1_2);
			}
			cdf                   = _mm256_max_pd(cdf,_0);
			cdf                   = _mm256_min_pd(cdf,_1);
			return (cdf);
			
		   }
		   
		   
       
                      __m256
		      von_misses_cdf_ymm8r4(const __m256 x,
		                            const __m256 a,
					    const __m256 b) {

                        //Early exit.
			const __m256   _0  = _mm256_setzero_ps();
			const __m256   _1  = _mm256_set1_ps(1.0f);
			const __m256   pi  = _mm256_set1_ps(3.14159265358979323846264338328f);
			const __m256   npi = _mm256_set1_ps(-3.14159265358979323846264338328f);
			const __m256   xsa = _mm256_sub_ps(x,a);
			if(__builtin_expect(_mm256_cmp_ps_mask(xsa,npi,_CMP_LE_OQ),0)) {
		             return (_0);
			}
			if(__builtin_expect(_mm256_cmp_ps_mask(npi,xsa,_CMP_LE_OQ),0)) {
                             return (_1); 
			}
			const __m256  _2pi = _mm256_set1_ps(6.283185307179586476925286766559f);
			const __m256  a1  = _mm256_set1_ps(12.0f);
			const __m256  a2  = _mm256_set1_ps(0.8f);
			const __m256  a3  = _mm256_set1_ps(8.0f);
			const __m256  a4  = _mm256_set1_ps(1.0f);
			const __m256  c1  = _mm256_set1_ps(56.0f);
			const __m256  ck  = _mm256_set1_ps(10.5f);
			const __m256  _2  = _mm256_set1_ps(2.0f);
			const __m256  _1_2= _mm256_set1_ps(0.5f);
			__m256 arg,cdf,cn,p,r,s,sn,u,v,y,z,uprv,erfx;
			//  Convert the angle (X - A) modulo 2 PI to the range ( 0, 2 * PI ).
			z    = b;
			u    = fmod_ymm8r4(_mm256_add_ps(xsa,pi),_2pi);
			uprv = u;
			const __mmask8 m = _mm256_cmp_ps_mask(u,_0,_CMP_LT_OQ);
			u    = _mm256_add_ps(u,_2pi);
			u    = _mm256_mask_blend_ps(m,uprv,u);
			y    = _mm256_sub_ps(u,pi);
			
			//For small B, sum IP terms by backwards recursion.
			// Can not be vectorized manually, hence 0 is returned.
			// Only large B is computed.
			/*
                              This scalar code can not be vectorized.
                              ip = int ( z * a2 - a3 / ( z + a4 ) + a1 )
                              Used as loop control variable
                              do n = 2, ip
                         */
                        if(_mm256_cmp_ps_mask(z,ck,_CMP_LE_OQ)) {
                           return (_0);
			}
			else {
                           const __m256 t0 = _mm256_set1_ps(24.0f);
			   const __m256 t1 = _mm256_set1_ps(54.0f);
			   const __m256 t2 = _mm256_set1_ps(347.0f);
			   const __m256 t3 = _mm256_set1_ps(26.0f);
			   const __m256 t4 = _mm256_set1_ps(6.0f);
			   const __m256 t5 = _mm256_set1_ps(12.0f);
			   const __m256 t6 = _mm256_set1_ps(3.0f);
			   const __m256 t7 = _mm256_set1_ps(16.0f);
			   const __m256 t8 = _mm256_set1_ps(1.75f);
			   const __m256 t9 = _mm256_set1_ps(83.5f);
			   c                = _mm256_mul_ps(t0,z);
			   v                = _mm256_sub_ps(c,c1);
			   const __m256d tmp1 = _mm256_sub_ps(_mm256_add_ps(v,t3),c);
			   const __m256d tmp2 = _mm256_div_ps(t1,_mm256_div_ps(t2,tmp1));
			   const __m256d tmp3 = _mm256_add_ps(_mm256_sub_ps(tmp2,t4),c);
			   r                  = _mm256_sqrt_ps(_mm256_div_ps(tmp3,t5));

			   z                  = _mm256_mul_ps(_mm256_sin_ps(
			                                                _mm256_mul_ps(_1_2,y)),r);
                           s                  = _mm256_mul_ps(_2,_mm256_mul_ps(z,z));
			   v                  = _mm256_sub_ps(v,_mm256_add_ps(s,t6));
			   y                  = _mm256_div_ps(_mm256_sub_ps(_mm256_sub_ps(c,s),
			                                                    _mm256_sub_ps(s,t7)),t6);
			   tmp1               = _mm256_sub_ps(v,y);
			   y                  = _mm256_div_ps(_mm256_fmadd_ps(_mm256_add_ps(s,t8),s,t9),tmp1);
			   tmp2               = _mm256_mul_ps(y,y);
			   arg                = _mm256_mul_ps(z,_mm256_sub_ps(_1,
			                                                  _mm256_div_ps(s,tmp2)));
			   erfx               = _mm256_erf_ps(arg);
			   cdf                = _mm256_fmadd_ps(_1_2,erfx,_1_2);
			}
			cdf                   = _mm256_max_ps(cdf,_0);
			cdf                   = _mm256_min_ps(cdf,_1);
			return (cdf);
			
		   }
		   
		   
/*
   !*****************************************************************************80
!
!! VON_MISES_PDF evaluates the von Mises PDF.
!
!  Discussion:
!
!    PDF(A,B;X) = EXP ( B * COS ( X - A ) ) / ( 2 * PI * I0(B) )
!
!    where:
!
!      I0(*) is the modified Bessel function of the first
!      kind of order 0.
!
!    The von Mises distribution for points on the unit circle is
!    analogous to the normal distribution of points on a line.
!    The variable X is interpreted as a deviation from the angle A,
!    with B controlling the amount of dispersion.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 October 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Jerry Banks, editor,
!    Handbook of Simulation,
!    Engineering and Management Press Books, 1998, page 160.
!
!    Donald Best, Nicholas Fisher,
!    Efficient Simulation of the von Mises Distribution,
!    Applied Statistics,
!    Volume 28, Number 2, pages 152-157.
!
!    Merran Evans, Nicholas Hastings, Brian Peacock,
!    Statistical Distributions,
!    Wiley, 2000,
!    LC: QA273.6.E92, pages 189-191.
!
!    Kanti Mardia, Peter Jupp,
!    Directional Statistics,
!    Wiley, 2000,
!    LC: QA276.M335
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    A - PI <= X <= A + PI.
!
!    Input, real ( kind = 8 ) A, a parameter of the PDF.
!    A is the preferred direction, in radians.
!    -PI <= A <= PI.
!
!    Input, real ( kind = 8 ) B, a parameter of the PDF.
!    B measures the "concentration" of the distribution around the
!    angle A.  B = 0 corresponds to a uniform distribution
!    (no concentration).  Higher values of B cause greater concentration
!    of probability near A.
!    0.0D+00 <= B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!              
*/

    
                      __m256d
		      von_misses_pdf_ymm4r8(const __m256d x,
		                            const __m256d a,
					    const __m256d b) {
 
                           const __m256d   pi  = _mm256_set1_pd(3.14159265358979323846264338328);
			   const __m256d   _2pi= _mm256_set1_pd(6.283185307179586476925286766559);
			   const __m256d   _0  = _mm256_setzero_pd();
			   const __m256d   _2  = _mm256_set1_pd(2.0);
			   const __m256d   t0  = _mm256_sub_pd(a,pi);
			   const __m256d   t1  = _mm256_add_pd(a,pi);
			   __m256d pdf;
			   __mmask8 m1,m2;
			   m1                  = _mm256_cmp_pd_mask(x,t0,_CMP_LT_OQ);
			   pdf                 = _mm256_mask_blend_pd(m1,_0,_0);
			   m2                  = _mm256_cmp_pd_mask(x,t1,_CMP_LE_OQ);

                           const __m256d tmp1  = _mm256_exp_pd(_mm256_mul_pd(b,
			                                              _mm256_cos_pd(
								                _mm256_sub_pd(x,a))));
                           
			   pdf                 = _mm256_mask_blend_pd(m2,_0,_mm256_div_pd(tmp1,
			                                              _mm256_mul_pd(_2pi,bessesl_i0_ymm4r8(b))));
			   return (pdf);
		   }


		        
                      __m256
		      von_misses_pdf_ymm8r4(const __m256 x,
		                            const __m256 a,
					    const __m256 b) {
 
                           const __m256   pi  = _mm256_set1_pd(3.14159265358979323846264338328f);
			   const __m256   _2pi= _mm256_set1_pd(6.283185307179586476925286766559f);
			   const __m256   _0  = _mm256_setzero_pd();
			   const __m256   _2  = _mm256_set1_pd(2.0);
			   const __m256   t0  = _mm256_sub_pd(a,pi);
			   const __m256   t1  = _mm256_add_pd(a,pi);
			   __m256 pdf;
			   __mmask8 m1,m2;
			   m1                  = _mm256_cmp_pd_mask(x,t0,_CMP_LT_OQ);
			   pdf                 = _mm256_mask_blend_pd(m1,_0,_0);
			   m2                  = _mm256_cmp_pd_mask(x,t1,_CMP_LE_OQ);
                           const __m256 tmp1  = _mm256_exp_ps(_mm256_mul_pd(b,
			                                              _mm256_cos_pd(
								                _mm256_sub_pd(x,a))));
                           
			   pdf                 = _mm256_mask_blend_pd(m2,_0,_mm256_div_pd(tmp1,
			                                              _mm256_mul_pd(_2pi,bessesl_i0_ymm8r4(b))));
			   return (pdf);
		   }
		   
		   
/*
!*****************************************************************************80
!
!! VON_MISES_SAMPLE samples the von Mises PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Donald Best, Nicholas Fisher,
!    Efficient Simulation of the von Mises Distribution,
!    Applied Statistics,
!    Volume 28, Number 2, pages 152-157.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, a parameter of the PDF.
!    A is the preferred direction, in radians.
!    -PI <= A <= PI.
!
!    Input, real ( kind = 8 ) B, a parameter of the PDF.
!    B measures the "concentration" of the distribution around the
!    angle A.  B = 0 corresponds to a uniform distribution
!    (no concentration).  Higher values of B cause greater concentration
!    of probability near A.
!    0.0D+00 <= B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random 
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
	              
*/


                          
                      __m256d
                      von_misses_sample_ymm4r8(const __m256d a,
		                               const __m256d b) {

                          const __m256d  pi   = _mm256_set1_pd(3.14159265358979323846264338328);
			  const __m256d  _1   = _mm256_set1_pd(1.0);
			  const __m256d  _2   = _mm256_set1_pd(2.0);
			  const __m256d  _4   = _mm256_set1_pd(4.0);
			  const __m256d  _1_2 = _mm256_set1_pd(0.5);
			  __m256d c,f,rho,tau,u1,r;
			  __m256d u2,u3,x,z;
			  __m256d t0,t1,t2;
			  svrng_engine_t engine;
			  svrng_distribution_t uniform;
			  uint32_t seed    = 0U;
			  int32_t result   = -9999;
			  int32_t err      = -9999;
			  result           = _rdrand32_step(&seed);
			  if(!result) seed = 1563548129U;
			  engine           = svrng_new_mt19937_engine(seed);
			  err              = svrng_get_status();
			  if(err!=SVRNG_STATUS_OK) {
                             const __m256d nan = _mm256_set1_pd(NAN);
			     return (nan);
			  }
			  uniform             = svrng_new_normal_distribution_double(0.0,1.0);
			  t0                  = _mm256_fmadd_pd(_4,_mm256_mul_pd(b,b),_1);
			  tau                 = _mm256_add_pd(_1,_mm256_sqrt_pd(t0));
			  t1                  = _mm256_add_pd(b,b);
			  rho                 = _mm256_div_pd(_mm256_sub_pd(tau,
			                                                _mm256_sqrt_pd(_mm256_add_pd(tau,tau))),t1);
			  t2                  = _mm256_fmadd_pd(rho,rho,_1);
			  r                   = _mm256_div_pd(t2,_mm256_add_pd(rho,rho));
            
 			 while(true) {
                               
                              const double * __restrict ptr = (const double*)(&svrng_generate4_double(engine,uniform));
                              u1                            = _mm256_loadu_pd(&ptr[0]);

                              z                             = _mm256_cos_pd(_mm256_mul_pd(pi,u1));
                              f                             = _mm256_div_pd(_mm256_fmadd_pd(r,z,_1),
			                                                    _mm256_add_pd(r,z));
			      c                             = _mm256_mul_pd(b,_mm256_sub_pd(r,f));
			      t0                            = _mm256_mul_pd(c,_mm256_sub_pd(_2,c));
			                       
			      if(_mm256_cmp_mask_pd(u2,t0,_CMP_LT_OQ)) break;
			      t1                            = _mm256_add_pd(_mm256_log_pd(
			                                                  _mm256_div_pd(c,u2)),_1);
			      if(_mm256_cmp_mask_pd(c,t1,_CMP_LE_OQ)) break;
			 }
			 const double * __restrict ptr2 =
			                    (const double*)(&svrng_generate4_double(engine,uniform));
			 u3                             = _mm256_loadu_pd(&ptr2[0]);
		         t2                             = ymm4r8_sign_ymm4r8(_1,_mm256_sub_pd(u3,_1_2));
			 x                              = _mm256_fmadd_pd(t2,_mm256_acos_pd(f),a);
			 svrng_delete_engine(engine);
			 return (x)
		   }


		   
		   
		         
                      __m256
                      von_misses_sample_ymm8r4(const __m256 a,
		                                const __m256 b) {

                          const __m256   pi   = _mm256_set1_ps(3.14159265358979323846264338328f);
			  const __m256   _1   = _mm256_set1_ps(1.0f);
			  const __m256  _2    = _mm256_set1_ps(2.0f);
			  const __m256  _4    = _mm256_set1_ps(4.0f);
			  const __m256  _1_2  = _mm256_set1_ps(0.5f);
			  __m256 c,f,rho,tau,u1,r;
			  __m256 u2,u3,x,z;
			  __m256 t0,t1,t2;
			  svrng_engine_t engine;
			  svrng_distribution_t uniform;
			  uint32_t seed    = 0U;
			  int32_t result   = -9999;
			  int32_t err      = -9999;
			  result           = _rdrand32_step(&seed);
			  if(!result) seed = 1563548129U;
			  engine           = svrng_new_mt19937_engine(seed);
			  err              = svrng_get_status();
			  if(err!=SVRNG_STATUS_OK) {
                             const __m256 nan = _mm256_set1_ps(nanf);
			     return (nan);
			  }
			  uniform             = svrng_new_normal_distribution_float(0.0f,1.0f);
			  t0                  = _mm256_fmadd_ps(_4,_mm256_mul_ps(b,b),_1);
			  tau                 = _mm256_add_ps(_1,_mm256_sqrt_ps(t0));
			  t1                  = _mm256_add_ps(b,b);
			  rho                 = _mm256_div_ps(_mm256_sub_ps(tau,
			                                                _mm256_sqrt_ps(_mm256_add_ps(tau,tau))),t1);
			  t2                  = _mm256_fmadd_ps(rho,rho,_1);
			  r                   = _mm256_div_ps(t2,_mm256_add_ps(rho,rho));
            
 			 while(true) {
                               
                              const float * __restrict ptr = (const float*)(&svrng_generate8_float(engine,uniform));
                              u1                            = _mm256_loadu_ps(&ptr[0]);

                              z                             = _mm256_cos_ps(_mm256_mul_ps(pi,u1));
                              f                             = _mm256_div_ps(_mm256_fmadd_ps(r,z,_1),
			                                                    _mm256_add_ps(r,z));
			      c                             = _mm256_mul_ps(b,_mm256_sub_ps(r,f));
			      t0                            = _mm256_mul_ps(c,_mm256_sub_ps(_2,c));
			                       
			      if(_mm256_cmp_mask_ps(u2,t0,_CMP_LT_OQ)) break;
			      t1                            = _mm256_add_ps(_mm256_log_ps(
			                                                  _mm256_div_ps(c,u2)),_1);
			      if(_mm256_cmp_mask_ps(c,t1,_CMP_LE_OQ)) break;
			 }
			 const float * __restrict ptr2 =
			                    (const float*)(&svrng_generate8_float(engine,uniform));
			 u3                             = _mm256_loadu_ps(&ptr2[0]);
		         t2                             = ymm8r4_sign_ymm8r4(_1,_mm256_sub_ps(u3,_1_2));
			 x                              = _mm256_fmadd_ps(t2,_mm256_acos_ps(f),a);
			 svrng_delete_engine(engine);
			 return (x)
		   }



/*
!*****************************************************************************80
!
!! RAYLEIGH_PDF evaluates the Rayleigh PDF.
!
!  Discussion:
!
!    PDF(A;X) = ( X / A^2 ) * EXP ( - X^2 / ( 2 * A^2 ) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    0.0D+00 <= X
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0 < A.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
                      
*/


                             
                      __m256d
		      rayleigh_pdf_ymm4r8(const __m256d x,
		                          const __m256d a) {

                           const __m256d  _0 = _mm256_setzero_pd();
			   __m256d t0,t1,t2,t3,pdf;
			   const __mmask8 m  = _mm256_cmp_pd_mask(x,_0,_CMP_LT_OQ);
			   t0                = _mm256_mul_pd(a,a);
			   t1                = ymm4r8_negate(_mm256_div_pd(_mm256_mul_pd(x,x),
			                                                   _mm256_add_pd(t0,t0)));
			   t2                = _mm256_div_pd(x,t0);
			   t3               = _mm256_mul_pd(t2,_mm256_exp_pd(t1));
                           pdf              = _mm256_mask_blend_pd(m,t3,_0);
                           return (pdf);
		     }


		        
                      __m256
		      rayleigh_pdf_ymm8r4(const __m256 x,
		                           const __m256 a) {

                           const __m256  _0 = _mm256_setzero_ps();
			   __m256 t0,t1,t2t3,pdf;
			   const __mmask16 m  = _mm256_cmp_ps_mask(x,_0,_CMP_LT_OQ);
			   t0                = _mm256_mul_ps(a,a);
			   t1                = ymm8r4_negate(_mm256_div_ps(_mm256_mul_ps(x,x),
			                                                   _mm256_add_ps(t0,t0)));
			   t2                = _mm256_div_ps(x,t0);
			   t3                = _mm256_mul_ps(t2,_mm256_exp_ps(t1));
                           pdf               = _mm256_mask_blend_ps(m,_t3,_0);
                           return (pdf);
		     }

/*
!*****************************************************************************80
!
!! RAYLEIGH_MEAN returns the mean of the Rayleigh PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0D+00 < A.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.		     
*/


                      
      		      
                      __m256d
		      rayleigh_mean_ymm4r8(const __m256d a) {

                          const __m256d hpi =  _mm256_set1_pd(0.5*3.14159265358979323846264338328);
			  __m256d mean;
			  mean              =  _mm256_mul_pd(a,_mm256_sqrt_pd(hpi));
			  return (mean);
		     }

 
                      __m256
		      rayleigh_mean_ymm8r4(const __m256d a) {

                          const __m256 hpi =  _mm256_set1_ps(0.5f*3.14159265358979323846264338328f);
			  __m256 mean;
			  mean              =  _mm256_mul_ps(a,_mm256_sqrt_ps(hpi));
			  return (mean);
		   }


/*
!*****************************************************************************80
!
!! RAYLEIGH_CDF_INV inverts the Rayleigh CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0D+00 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0D+00 < A.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
*/


                         
                      __m256d
		      rayleigh_invcdf_ymm4r8(const __m256d cdf,
		                             const __m256d a) {

			 const __m256d _0 = _mm256_setzero_pd();
			 const __m256d _1 = _mm256_setzero_pd(1.0);
			 const __m256d n2 = _mm256_setzero_pd(-2.0);
			 __m256d inv,t0,t1,;
                        
			 t0  = _mm256_log_pd(_mm256_sub_pd(_1,cdf));
			 t1  = _mm256_mul_pd(_2,_mm256_mul_pd(a,a));
                         inv = _mm256_sqrt_pd(_mm256_mul_pd(t0,t1));
			 return (inv);
			   
		     }


		       
                      __m256
		      rayleigh_invcdf_ymm8r4(const __m256 cdf,
		                             const __m256 a) {

			 const __m256 _0 = _mm256_setzero_ps();
			 const __m256 _1 = _mm256_setzero_ps(1.0f);
			 const __m256 n2 = _mm256_setzero_ps(-2.0f);
			 __m256 inv,t0,t1,;
                     
			 t0  = _mm256_log_ps(_mm256_sub_ps(_1,cdf));
			 t1  = _mm256_mul_ps(_2,_mm256_mul_ps(a,a));
                         inv = _mm256_sqrt_ps(_mm256_mul_ps(t0,t1));
			 return (inv);
			   
		     }
		     
		     
/*
!*****************************************************************************80
!
!! RAYLEIGH_CDF evaluates the Rayleigh CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!    0.0D+00 <= X.
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0D+00 < A.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
*/


                          
                      __m256d
		      rayleigh_cdf_ymm4r8(const __m256d x,
		                          const __m256d a) {

                         const __m256d _0 = _mm256_setzero_pd();
			 const __m256d _1 = _mm256_setzero_pd(1.0);
			 __m256d cdf,t0,t1;
			 t0              = _mm256_mul_pd(_2,_mm256_mul_pd(a,a));
			 t1              = ymm4r8_negate(_mm256_mul_pd(x,x));
			 cdf             = _mm256_sub_pd(_1,
			                             _mm256_exp_pd(_mm256_div_pd(t1,t0)));
                         return (cdf);
		    }


		       
                      __m256
		      rayleigh_cdf_ymm8r4(const __m256 x,
		                           const __m256 a) {

                         const __m256 _0 = _mm256_setzero_ps();
			 const __m256 _1 = _mm256_setzero_ps(1.0f);
			 __m256 cdf,t0,t1;
			 t0              = _mm256_mul_pd(_2,_mm256_mul_ps(a,a));
			 t1              = ymm8r4_negate(_mm256_mul_ps(x,x));
			 cdf             = _mm256_sub_ps(_1,
			                             _mm256_exp_ps(_mm256_div_ps(t1,t0)));
                         return (cdf);
		    }


		    
		       
                      __m256d
		      rayleigh_sample_ymm4r8(const __m256d rand,
		                             const __m256d a) {

                          return (rayleigh_invcdf_ymm4r8(rand,a));
		     }


		       
                      __m256
		      rayleigh_sample_ymm8r4(const __m256 rand,
		                             const __m256 a) {

                          return (rayleigh_invcdf_ymm8r4(rand,a));
		     }
		     
/*		     
!*****************************************************************************80
!
!! CAUCHY_CDF evaluates the Cauchy CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0D+00 < B.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
! 
      */
      
        	          
                      __m256d    
                      cauchy_cdf_ymm4r8(const __m256d x,
                                        const __m256d a,
                                        const __m256d b) {
                        
                         const __m256d C314159265358979323846264 = 
                                               __m256_set1_pd(3.14159265358979323846264);
                         const __m256d C05 = _mm256_set1_pd(0.5);
                         register __m256d cdf,y,t0,t1;
                         t0 = _mm256_sub_pd(x,a);
                         t1 = _mm256_div_pd(_mm256_atan2_pd(t0,b),
                                     C314159265358979323846264);
                         cdf = _mm256_add_pd(C05,t1);
                         return (cdf);  
                    }
                    
                    
                          
                      __m256    
                      cauchy_cdf_ymm8r4( const __m256 x,
                                         const __m256 a,
                                         const __m256 b) {
                        
                         const __m256 C314159265358979323846264 = 
                                               __m256_set1_ps(3.14159265358979323846264f);
                         const __m256 C05 = _mm256_set1_ps(0.5f);
                         register __m256 cdf,y,t0,t1;
                         t0 = _mm256_sub_ps(x,a);
                         t1 = _mm256_div_ps(_mm256_atan2_ps(t0,b),
                                     C314159265358979323846264);
                         cdf = _mm256_add_ps(C05,t1);
                         return (cdf);  
                    }
                    
                    
/*
 !*****************************************************************************80
!
!! CAUCHY_CDF_INV inverts the Cauchy CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0D+00 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0D+00 < B.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
!
*/     


                           
                      __m256d 
                      cauchy_cdf_inv_ymm4r8(const __m256d a,
                                            const __m256d b,
                                            const __m256d x) {
                           
                         const __m256d C314159265358979323846264 = 
                                               __m256_set1_pd(3.14159265358979323846264);
                         const __m256d C05 = _mm256_set1_pd(0.5);    
                         register __m256d cdf,t0,t1;
                         t0 = _mm256_mul_pd(C314159265358979323846264,
                                            _mm256_sub_pd(cdf,C05));
                         t1 = _mm256_tan_pd(t0);
                         cdf = _mm256_fmadd_pd(a,b,t1);
                         return (cdf);
                   }    
                   
                   
                          
                      __m256
                      cauchy_cdf_inv_ymm8r4(const __m256 a,
                                            const __m256 b,
                                            const __m256 x) {
                           
                         const __m256 C314159265358979323846264 = 
                                               __m256_set1_pd(3.14159265358979323846264f);
                         const __m256 C05 = _mm256_set1_ps(0.5);    
                         register __m256 cdf,t0,t1;
                         t0 = _mm256_mul_ps(C314159265358979323846264,
                                            _mm256_sub_ps(cdf,C05));
                         t1 = _mm256_tan_ps(t0);
                         cdf = _mm256_fmadd_ps(a,b,t1);
                         return (cdf);
                   }  
                   
                                       
 /*
  !*****************************************************************************80
!
!! CAUCHY_PDF evaluates the Cauchy PDF.
!
!  Discussion:
!
!    PDF(A,B;X) = 1 / ( PI * B * ( 1 + ( ( X - A ) / B )^2 ) )
!
!    The Cauchy PDF is also known as the Breit-Wigner PDF.  It
!    has some unusual properties.  In particular, the integrals for the
!    expected value and higher order moments are "singular", in the
!    sense that the limiting values do not exist.  A result can be
!    obtained if the upper and lower limits of integration are set
!    equal to +T and -T, and the limit as T=>INFINITY is taken, but
!    this is a very weak and unreliable sort of limit.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0D+00 < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
*/       


                         
                      __m256d  
                      cauchy_pdf_ymm4r8( const __m256d x,
                                         const __m256d a,
                                         const __m256d b) {
                           
                         const __m256d C314159265358979323846264 = 
                                               __m256_set1_pd(3.14159265358979323846264);
                         const __m256d C1 = _mm256_set1_pd(1.0);
                         register __m256d pdf,t0,t1,y,pib;
                         y   = _mm256_div_pd(_mm256_sub_pd(x,a),b);
                         pib = _mm256_mul_pd(C314159265358979323846264,b);
                         t0  = _mm256_fmadd_pd(y,y,C1);
                         t1  = _mm256_mul_pd(pib,t0);
                         pdf = _mm256_div_pd(C1,t1);
                         return (pdf);                     
                   }
                   
                   
                             
                      __m256 
                      cauchy_pdf_ymm8r4(const __m256 x,
                                         const __m256 a,
                                         const __m256 b) {
                           
                         const __m256 C314159265358979323846264 = 
                                               __m256_set1_ps(3.14159265358979323846264f);
                         const __m256 C1 = _mm256_set1_ps(1.0);
                         register __m256 pdf,t0,t1,y,pib;
                         y   = _mm256_div_ps(_mm256_sub_ps(x,a),b);
                         pib = _mm256_mul_ps(C314159265358979323846264,b);
                         t0  = _mm256_fmadd_ps(y,y,C1);
                         t1  = _mm256_mul_ps(pib,t0);
                         pdf = _mm256_div_ps(C1,t1);
                         return (pdf);                     
                   }
                   
                  
/*
 !*****************************************************************************80
!
!! MAXWELL_CDF evaluates the Maxwell CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    0.0D+00 <= X
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0 < A.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!                    
*/


                
                          
                      __m256d 
                      maxwell_cdf_ymm4r8(const __m256d x,
                                         const __m256d a) {
                         
                         const __m256d C15 = _mm256_set1_pd(1.5);
                         register __m256d x2,cdf;
                         x2 = _mm256_div_pd(x,a);
                         cdf = gamma_incomplete_ymm4r8(C15,x2);
                         return (cdf);                      
                    }      
                    
                    
                          
                      __m256 
                      maxwell_cdf_ymm8r4(const __m256 x,
                                         const __m256 a) {
                         
                         const __m256 C15 = _mm256_set1_ps(1.5f);
                         register __m256 x2,cdf;
                         x2 = _mm256_div_ps(x,a);
                         cdf = gamma_incomplete_ymm8r4(C15,x2);
                         return (cdf);                      
                    }  
                    
                    
                                             
/*
!*****************************************************************************80
!
!! TFN calculates the T function of Owen.
!
!  Discussion:
!
!    Owen's T function is useful for computation of the bivariate normal
!    distribution and the distribution of a skewed normal distribution.
!
!    Although it was originally formulated in terms of the bivariate
!    normal function, the function can be defined more directly as
!
!      T(H,A) = 1 / ( 2 * pi ) *
!        Integral ( 0 <= X <= A ) e^( -H^2 * (1+X^2) / 2 ) / (1+X^2) dX
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 December 2004
!
!  Author:
!
!    Original FORTRAN77 version by J C Young, C E Minder.
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Donald Owen,
!    Tables for computing the bivariate normal distribution,
!    Annals of Mathematical Statistics,
!    Volume 27, pages 1075-1090, 1956.
!
!    JC Young, CE Minder,
!    Algorithm AS 76,
!    An Algorithm Useful in Calculating Non-Central T and
!    Bivariate Normal Distributions,
!    Applied Statistics,
!    Volume 23, Number 3, 1974, pages 455-457.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) H, A, the arguments of the T function.
!
!    Output, real ( kind = 8 ) TFN, the value of the T function.
!
*/


 /*                   
		      __m256d owen_tfunc_ymm4r8(const __m256d h,
		                                const __m256d a) {
		             
		             __attribute__((section(".rodata")))
		             __ATTR_ALIGN__(32) static __m256d  weight[10] = {
		                                _mm256_set1_pd(0.666713443086881375935688098933e-01),
                                                _mm256_set1_pd(0.149451349150580593145776339658e+00),
                                                _mm256_set1_pd(0.219086362515982043995534934228e+00),
                                                _mm256_set1_pd(0.269266719309996355091226921569e+00),
                                                _mm256_set1_pd(0.295524224714752870173892994651e+00),
                                                _mm256_set1_pd(0.295524224714752870173892994651e+00),
                                                _mm256_set1_pd(0.269266719309996355091226921569e+00),
                                                _mm256_set1_pd(0.219086362515982043995534934228e+00),
                                                _mm256_set1_pd(0.149451349150580593145776339658e+00), 
                                                _mm256_set1_pd(0.666713443086881375935688098933e-01)};
                           __attribute__((section(".rodata")))
		            __ATTR_ALIGN__(32) static __m256d  xtab[10] = {
		                                _mm256_set1_pd(-0.973906528517171720077964012084e+00),
                                                _mm256_set1_pd(-0.865063366688984510732096688423e+00),
                                                _mm256_set1_pd(-0.679409568299024406234327365115e+00), 
                                                _mm256_set1_pd(-0.433395394129247190799265943166e+00), 
                                                _mm256_set1_pd(-0.148874338981631210884826001130e+00), 
                                                _mm256_set1_pd(0.148874338981631210884826001130e+00), 
                                                _mm256_set1_pd(0.433395394129247190799265943166e+00), 
                                                _mm256_set1_pd(0.679409568299024406234327365115e+00), 
                                                _mm256_set1_pd(0.865063366688984510732096688423e+00), 
                                                _mm256_set1_pd(0.973906528517171720077964012084e+00)};
		           
		             const __m256d twopinv = _mm256_set1_pd(0.15915494309189533576888e+00);   
		             const __m256d tv1     = _mm256_set1_pd(1.0e-35);
		             const __m256d tv2     = _mm256_set1_pd(15.0);
		             const __m256d tv3     = tv2;
		             const __m256d tv4     = _mm256_set1_pd(1.0e-5);
		             const __m256d C05     = _mm256_set1_pd(0.5);
		             const __m256d C1      = _mm256_set1_pd(1.0);
		             const __m256d C2      = _mm256_set1_pd(2.0);
		             const __m256d C025    = _mm256_set1_pd(0.25);
		             const __m256d C0      = _mm256_setzero_pd();
		             __m256d x,rt,as,h1,h2,hs,t0,t1,t2;
		             __m256d tfn;
		             if(_mm256_cmp_pd_mask(_mm256_abs_pd(h),tv1,_CMP_LT_OQ)) {

                                tfn = _mm256_mul_pd(_mm256_atan_pd(a),twopinv);
	               
		             }
		             else if(_mm256_cmp_pd_mask(tv2,_mm256_abs_pd(h),_CMP_LT_OQ)) {
		                tfn = C0;
		             }
		             else if(_mm256_cmp_pd_mask(_mm256_abs_pd(a),tv1,_CMP_LT_OQ)) {
		                 tfn = C0;
		             }
		             else {
		                 hs = _mm256_mul_pd(negate_ymm4r8(C05),
		                            _mm256_mul_pd(h,h));
		                 h2 = a;
		                 as = _mm256_mul_pd(a,a);
                                 t0 = _mm256_log_pd(_mm256_add_pd(C1,as));
       
                                 __mmask8 m = _mm256_cmp_pd_mask(tv3,_mm256_sub_pd(t0,
                                                                     _mm256_mul_pd(hs,as)),_CMP_LE_OQ);
                                 if(m) {
                                    h1 = _mm256_mul_pd(C05,a);
                                    as = _mm256_mul_pd(C025,as);
                                    while(true) {
                                          rt = _mm256_add_pd(as,C1);
                                          t0 = _mm256_add_pd(h1,_mm256_fmadd_pd(hs,as,_mm256_log_pd(rt)));                                 
                                          t1 = _mm256_sub_pd(_mm256_div_pd(C1,rt),hs);
                                          t2 = _mm256_mul_pd(C2,_mm256_mul_pd(h1,t1));
                                          h2 = _mm256_div_pd(t0,t2);
                                          as = _mm256_mul_pd(h2,h2);
                                          if(_mm256_cmp_pd_mask(_mm256_abs_pd(
                                                          _mm256_mul_pd(h2,h1),tv4,_CMP_LT_OQ))) break;
                                          h1 = h2;                                               
                                    }
                                 }
                                 rt = C0;
                                 
                                 //for(int32_t i=0; i<10; ++i) 

                                 x  = _mm256_fmadd_pd(C05,h2,_mm256_add_pd(xtab[0],C1));
                                 t0 = _mm256_add_pd(C1,_mm256_mul_pd(x,x));
                                 rt = _mm256_add_pd(rt,_mm256_mul_pd(weight[0],
                                               _mm256_div_pd(_mm256_exp_pd(_mm256_mul_pd(hs,t0)),t0)));
                                 x  = _mm256_fmadd_pd(C05,h2,_mm256_add_pd(xtab[1],C1));
                                 t0 = _mm256_add_pd(C1,_mm256_mul_pd(x,x));
                                 rt = _mm256_add_pd(rt,_mm256_mul_pd(weight[1],
                                               _mm256_div_pd(_mm256_exp_pd(_mm256_mul_pd(hs,t0)),t0)));
                                 x  = _mm256_fmadd_pd(C05,h2,_mm256_add_pd(xtab[2],C1));
                                 t0 = _mm256_add_pd(C1,_mm256_mul_pd(x,x));
                                 rt = _mm256_add_pd(rt,_mm256_mul_pd(weight[2],
                                               _mm256_div_pd(_mm256_exp_pd(_mm256_mul_pd(hs,t0)),t0)));
                                 x  = _mm256_fmadd_pd(C05,h2,_mm256_add_pd(xtab[3],C1));
                                 t0 = _mm256_add_pd(C1,_mm256_mul_pd(x,x));
                                 rt = _mm256_add_pd(rt,_mm256_mul_pd(weight[3],
                                               _mm256_div_pd(_mm256_exp_pd(_mm256_mul_pd(hs,t0)),t0)));
                                 x  = _mm256_fmadd_pd(C05,h2,_mm256_add_pd(xtab[4],C1));
                                 t0 = _mm256_add_pd(C1,_mm256_mul_pd(x,x));
                                 rt = _mm256_add_pd(rt,_mm256_mul_pd(weight[4],
                                               _mm256_div_pd(_mm256_exp_pd(_mm256_mul_pd(hs,t0)),t0)));
                                 x  = _mm256_fmadd_pd(C05,h2,_mm256_add_pd(xtab[5],C1));
                                 t0 = _mm256_add_pd(C1,_mm256_mul_pd(x,x));
                                 rt = _mm256_add_pd(rt,_mm256_mul_pd(weight[5],
                                               _mm256_div_pd(_mm256_exp_pd(_mm256_mul_pd(hs,t0)),t0)));
                                 x  = _mm256_fmadd_pd(C05,h2,_mm256_add_pd(xtab[6],C1));
                                 t0 = _mm256_add_pd(C1,_mm256_mul_pd(x,x));
                                 rt = _mm256_add_pd(rt,_mm256_mul_pd(weight[6],
                                               _mm256_div_pd(_mm256_exp_pd(_mm256_mul_pd(hs,t0)),t0)));
                                 x  = _mm256_fmadd_pd(C05,h2,_mm256_add_pd(xtab[7],C1));
                                 t0 = _mm256_add_pd(C1,_mm256_mul_pd(x,x));
                                 rt = _mm256_add_pd(rt,_mm256_mul_pd(weight[7],
                                               _mm256_div_pd(_mm256_exp_pd(_mm256_mul_pd(hs,t0)),t0)));
                                 x  = _mm256_fmadd_pd(C05,h2,_mm256_add_pd(xtab[8],C1));
                                 t0 = _mm256_add_pd(C1,_mm256_mul_pd(x,x));
                                 rt = _mm256_add_pd(rt,_mm256_mul_pd(weight[8],
                                               _mm256_div_pd(_mm256_exp_pd(_mm256_mul_pd(hs,t0)),t0)));
                                 x  = _mm256_fmadd_pd(C05,h2,_mm256_add_pd(xtab[9],C1));
                                 t0 = _mm256_add_pd(C1,_mm256_mul_pd(x,x));
                                 rt = _mm256_add_pd(rt,_mm256_mul_pd(weight[9],
                                               _mm256_div_pd(_mm256_exp_pd(_mm256_mul_pd(hs,t0)),t0))); 


                                 t1 = _mm256_mul_pd(C05,h2);
                                 tfn= _mm256_mul_pd(rt,_mm256_mul_pd(t1,twopinv)); 
		             }
		             return (tfn);
		    }   
		    
*/

/*		    
                       __m256 owen_tfunc_zmm16r4(const __m256 h,
		                                const __m256 a) {
		             
		             __attribute__((section(".rodata")))
		             __ATTR_ALIGN__(32) static __m256  weight[10] = {
		                                _mm256_set1_ps(0.666713443086881375935688098933e-01f),
                                                _mm256_set1_ps(0.149451349150580593145776339658e+00f),
                                                _mm256_set1_ps(0.219086362515982043995534934228e+00f),
                                                _mm256_set1_ps(0.269266719309996355091226921569e+00f),
                                                _mm256_set1_ps(0.295524224714752870173892994651e+00f),
                                                _mm256_set1_ps(0.295524224714752870173892994651e+00f),
                                                _mm256_set1_ps(0.269266719309996355091226921569e+00f),
                                                _mm256_set1_ps(0.219086362515982043995534934228e+00f),
                                                _mm256_set1_ps(0.149451349150580593145776339658e+00f), 
                                                _mm256_set1_ps(0.666713443086881375935688098933e-01f)};
                            __attribute__((section(".rodata")))
		            __ATTR_ALIGN__(32) static __m256  xtab[10] = {
		                                _mm256_set1_ps(-0.973906528517171720077964012084e+00f),
                                                _mm256_set1_ps(-0.865063366688984510732096688423e+00f),
                                                _mm256_set1_ps(-0.679409568299024406234327365115e+00f), 
                                                _mm256_set1_ps(-0.433395394129247190799265943166e+00f), 
                                                _mm256_set1_ps(-0.148874338981631210884826001130e+00f), 
                                                _mm256_set1_ps(0.148874338981631210884826001130e+00f), 
                                                _mm256_set1_ps(0.433395394129247190799265943166e+00f), 
                                                _mm256_set1_ps(0.679409568299024406234327365115e+00f), 
                                                _mm256_set1_ps(0.865063366688984510732096688423e+00f), 
                                                _mm256_set1_ps(0.973906528517171720077964012084e+00f)};
		           
		             const __m256 twopinv = _mm256_set1_ps(0.15915494309189533576888e+00f);   
		             const __m256 tv1     = _mm256_set1_ps(FLT_MIN);
		             const __m256 tv2     = _mm256_set1_ps(15.0f);
		             const __m256 tv3     = tv2;
		             const __m256 tv4     = _mm256_set1_ps(1.0e-5f);
		             const __m256 C05     = _mm256_set1_ps(0.5f);
		             const __m256 C1      = _mm256_set1_ps(1.0f);
		             const __m256 C2      = _mm256_set1_ps(2.0f);
		             const __m256 C025    = _mm256_set1_ps(0.25f);
		             const __m256 C0      = _mm256_setzero_ps();
		             __m256 x,rt,as,h1,h2,hs,t0,t1,t2;
		             __m256 tfn;
		             if(_mm256_cmp_ps_mask(_mm256_abs_ps(h),tv1,_CMP_LT_OQ)) {
                                tfn = _mm256_mul_ps(_mm256_atan_ps(a),twopinv);
              		     }
		             else if(_mm256_cmp_ps_mask(tv2,_mm256_abs_ps(h),_CMP_LT_OQ)) {
		                tfn = C0;
		             }
		             else if(_mm256_cmp_pd_mask(_mm256_abs_ps(a),tv1,_CMP_LT_OQ)) {
		                 tfn = C0;
		             }
		             else {
		                 hs = _mm256_mul_ps(negate_zmm16r4(C05),
		                            _mm256_mul_ps(h,h));
		                 h2 = a;
		                 as = _mm256_mul_ps(a,a);
                                 t0 = _mm256_log_ps(_mm256_add_ps(C1,as));
           
                                 __mmask8 m = _mm256_cmp_ps_mask(tv3,_mm256_sub_ps(t0,
                                                                     _mm256_mul_ps(hs,as)),_CMP_LE_OQ);
                                 if(m) {
                                    h1 = _mm256_mul_ps(C05,a);
                                    as = _mm256_mul_ps(C025,as);
                                    while(true) {
                                          rt = _mm256_add_ps(as,C1);
                                          t0 = _mm256_add_ps(h1,_mm256_fmadd_ps(hs,as,_mm256_log_ps(rt)));
                                          t1 = _mm256_sub_ps(_mm256_div_ps(C1,rt),hs);
                                          t2 = _mm256_mul_ps(C2,_mm256_mul_ps(h1,t1));
                                          h2 = _mm256_div_ps(t0,t2);
                                          as = _mm256_mul_ps(h2,h2);
                                          if(_mm256_cmp_ps_mask(_mm256_abs_ps(
                                                          _mm256_mul_ps(h2,h1),tv4,_CMP_LT_OQ))) break;
                                          h1 = h2;                                               
                                    }
                                 }
                                 rt = C0;
                                 
                                 //for(int32_t i=0; i<10; ++i) 

                                 x  = _mm256_fmadd_ps(C05,h2,_mm256_add_ps(xtab[0],C1));
                                 t0 = _mm256_add_ps(C1,_mm256_mul_ps(x,x));
                                 rt = _mm256_add_ps(rt,_mm256_mul_ps(weight[0],
                                               _mm256_div_ps(_mm256_exp_ps(_mm256_mul_ps(hs,t0)),t0)));
                                 x  = _mm256_fmadd_ps(C05,h2,_mm256_add_ps(xtab[1],C1));
                                 t0 = _mm256_add_ps(C1,_mm256_mul_ps(x,x));
                                 rt = _mm256_add_ps(rt,_mm256_mul_ps(weight[1],
                                               _mm256_div_ps(_mm256_exp_ps(_mm256_mul_ps(hs,t0)),t0)));
                                 x  = _mm256_fmadd_ps(C05,h2,_mm256_add_ps(xtab[2],C1));
                                 t0 = _mm256_add_ps(C1,_mm256_mul_ps(x,x));
                                 rt = _mm256_add_ps(rt,_mm256_mul_ps(weight[2],
                                               _mm256_div_ps(_mm256_exp_ps(_mm256_mul_ps(hs,t0)),t0)));
                                 x  = _mm256_fmadd_ps(C05,h2,_mm256_add_ps(xtab[3],C1));
                                 t0 = _mm256_add_ps(C1,_mm256_mul_ps(x,x));
                                 rt = _mm256_add_ps(rt,_mm256_mul_ps(weight[3],
                                               _mm256_div_ps(_mm256_exp_ps(_mm256_mul_ps(hs,t0)),t0)));
                                 x  = _mm256_fmadd_ps(C05,h2,_mm256_add_ps(xtab[4],C1));
                                 t0 = _mm256_add_ps(C1,_mm256_mul_ps(x,x));
                                 rt = _mm256_add_ps(rt,_mm256_mul_ps(weight[4],
                                               _mm256_div_ps(_mm256_exp_ps(_mm256_mul_ps(hs,t0)),t0)));
                                 x  = _mm256_fmadd_ps(C05,h2,_mm256_add_ps(xtab[5],C1));
                                 t0 = _mm256_add_ps(C1,_mm256_mul_ps(x,x));
                                 rt = _mm256_add_ps(rt,_mm256_mul_ps(weight[5],
                                               _mm256_div_ps(_mm256_exp_ps(_mm256_mul_ps(hs,t0)),t0)));
                                 x  = _mm256_fmadd_ps(C05,h2,_mm256_add_ps(xtab[6],C1));
                                 t0 = _mm256_add_ps(C1,_mm256_mul_ps(x,x));
                                 rt = _mm256_add_ps(rt,_mm256_mul_ps(weight[6],
                                               _mm256_div_ps(_mm256_exp_ps(_mm256_mul_ps(hs,t0)),t0)));
                                 x  = _mm256_fmadd_ps(C05,h2,_mm256_add_ps(xtab[7],C1));
                                 t0 = _mm256_add_ps(C1,_mm256_mul_ps(x,x));
                                 rt = _mm256_add_ps(rt,_mm256_mul_ps(weight[7],
                                               _mm256_div_ps(_mm256_exp_ps(_mm256_mul_ps(hs,t0)),t0)));
                                 x  = _mm256_fmadd_ps(C05,h2,_mm256_add_ps(xtab[8],C1));
                                 t0 = _mm256_add_ps(C1,_mm256_mul_ps(x,x));
                                 rt = _mm256_add_ps(rt,_mm256_mul_ps(weight[8],
                                               _mm256_div_ps(_mm256_exp_ps(_mm256_mul_ps(hs,t0)),t0)));
                                 x  = _mm256_fmadd_ps(C05,h2,_mm256_add_ps(xtab[9],C1));
                                 t0 = _mm256_add_ps(C1,_mm256_mul_ps(x,x));
                                 rt = _mm256_add_ps(rt,_mm256_mul_ps(weight[9],
                                               _mm256_div_ps(_mm256_exp_ps(_mm256_mul_ps(hs,t0)),t0))); 


                                 t1 = _mm256_mul_ps(C05,h2);
                                 tfn= _mm256_mul_ps(rt,_mm256_mul_ps(t1,twopinv)); 
		             }
		             return (tfn);
		    }  
		    
*/		    		    
		    /*
       !*****************************************************************************80
!
!! R8_GAMMA evaluates Gamma(X) for a real argument.
!
!  Discussion:
!
!    This routine calculates the gamma function for a real argument X.
!
!    Computation is based on an algorithm outlined in reference 1.
!    The program uses rational functions that approximate the gamma
!    function to at least 20 significant decimal digits.  Coefficients
!    for the approximation over the interval (1,2) are unpublished.
!    Those for the approximation for 12 <= X are from reference 2.
!
!  Modified:
!
!    11 February 2008
!
!  Author:
!
!    Original FORTRAN77 version by William Cody, Laura Stoltz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    William Cody,
!    An Overview of Software Development for Special Functions,
!    in Numerical Analysis Dundee, 1975,
!    edited by GA Watson,
!    Lecture Notes in Mathematics 506,
!    Springer, 1976.
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
!    Input, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) R8_GAMMA, the value of the function.
!    Important notice: ******For argument interval: 2.0<=x<12.0 there is a scalarization in form
!    of 8 scalar for-loops*******!!
*/


                         
                      __m256d 
                      gamma_ymm4r8(const __m256d x) {
                       
                       __m256d pi                          = 
                                        _mm256_set1_pd(3.1415926535897932384626434e+00);
                       __m256d eps                         =
                                        _mm256_set1_pd(2.22e-16);
                       __m256d one                         =
                                        _mm256_set1_pd(1.0);
                       __m256d half                        =
                                        _mm256_set1_pd(0.5);
                       __m256d sqrtpi                      =
                                        _mm256_set1_pd(0.9189385332046727417803297e+00);
                       __m256d twelve                      = 
                                        _mm256_set1_pd(12.0);
                       __m256d two                         =
                                        _mm256_set1_pd(2.0);
                       __m256d xbig                        =
                                        _mm256_set1_pd(171.624);
                       __m256d xinf                        =
                                        _mm256_set1_pd(1.0e+30);
                       __m256d xminin                      =
                                        _mm256_set1_pd(2.23e-308);
                       
                       __m256d zero                        =
                                        _mm256_setzero_pd();
                       register __m256d res,sum,xden,xnum;
                       register __m256d y,y1,ysq,z,fact;
                       register __m256i n;
                       
                       bool     parity;
                       parity = false;
                       y      = x;
                       // Negative argument
                       if(_mm256_cmp_pd_mask(y,zero,_CMP_LE_OQ)) {
                          register __m256d t0,t1;
                          y  = negate_ymm4r8(x);
                          y1 = _mm256_castsi256_pd(_mm256_cvttpd_epu64(y));
                          res= _mm256_sub_pd(y,y1);
                          if(_mm256_cmp_pd_mask(res,zero,_CMP_NEQ_OQ)) {
                            
                             t0 = _mm256_mul_pd(_mm256_mul_pd(y1,half),two);
                             t1 = _mm256_castsi256_pd(_mm256_cvttpd_epu64(t0));
                             if(_mm256_cmp_pd_mask(y1,t1,_CMP_NEQ_OQ)) parity = true;
                             t0 = _mm256_sin_pd(_mm256_mul_pd(pi,res));
                             fact = _mm256_div_pd(negate_ymm4r8(pi),t0);
                             y    = _mm256_add_pd(y,one);
                          }
                          else {
                             res = xinf;
                             return (res);
                          }
                       }
                       // Positive argument
                       if(_mm256_cmp_pd_mask(y,eps,_CMP_LT_OQ)) {
                          __mmask8 m;
                          m = _mm256_cmp_pd_mask(xminin,y,_CMP_LE_OQ);
                          res = _mm256_mask_blend_pd(m,xinf,_mm256_div_pd(one,y));
                          return (res);
                       }
                  }
                  else if(_mm256_cmp_pd_mask(y,twelve,_CMP_LT_OQ)) {
                          y1 = y;
                          // 0.0 < argument < 1.0.
                          if(_mm256_cmp_pd_mask(y,one,_CMP_LT_OQ)) {
                             z = y;
                             y = _mm256_add_pd(y,one);
                          }
                          else {
                             //!  1.0 < argument < 12.0.
                             //!  Reduce argument if necessary.
                             n = _mm256_sub_epi64(mm256_castpd_si256(y),
                                                  _mm256_set1_epi64(1LL));
                             y = _mm256_sub_pd(y,_mm256_castsi256_pd(n));
                             z = _mm256_sub_pd(y,one);
                          }
                          //  Evaluate approximation for 1.0 < argument < 2.0.
                          xnum = zero;
                          xden = one;
#if defined(__GNUC__) && !defined(__INTEL_COMPILER)                             
                          xnum = _mm256_mul_pd(_mm256_add_pd(xnum,*(__m256d*)&gamma_ymm4r8_p[0]),z);
                          xden = _mm256_fmadd_pd(xden,z,*(__m256d*)&gamma_ymm4r8_q[0]);
                          xnum = _mm256_mul_pd(_mm256_add_pd(xnum,*(__m256d*)&gamma_ymm4r8_p[4]),z);
                          xden = _mm256_fmadd_pd(xden,z,*(__m256d*)&gamma_ymm4r8_q[4]);
                          xnum = _mm256_mul_pd(_mm256_add_pd(xnum,*(__m256d*)&gamma_ymm4r8_p[8]),z);
                          xden = _mm256_fmadd_pd(xden,z,*(__m256d*)&gamma_ymm4r8_q[8]);
                          xnum = _mm256_mul_pd(_mm256_add_pd(xnum,*(__m256d*)&gamma_ymm4r8_p[12]),z);
                          xden = _mm256_fmadd_pd(xden,z,*(__m256d*)&gamma_ymm4r8_q[12]);
                          xnum = _mm256_mul_pd(_mm256_add_pd(xnum,*(__m256d*)&gamma_ymm4r8_p[16]),z);
                          xden = _mm256_fmadd_pd(xden,z,*(__m256d*)&gamma_ymm4r8_q[16]);
                          xnum = _mm256_mul_pd(_mm256_add_pd(xnum,*(__m256d*)&gamma_ymm4r8_p[20]),z);
                          xden = _mm256_fmadd_pd(xden,z,*(__m256d*)&gamma_ymm4r8_q[20]);
                          xnum = _mm256_mul_pd(_mm256_add_pd(xnum,*(__m256d*)&gamma_ymm4r8_p[24]),z);
                          xden = _mm256_fmadd_pd(xden,z,*(__m256d*)&gamma_ymm4r8_q[28]);
                          xnum = _mm256_mul_pd(_mm256_add_pd(xnum,*(__m256d*)&gamma_ymm4r8_p[28]),z);
                          xden = _mm256_fmadd_pd(xden,z,*(__m256d*)&gamma_ymm4r8_q[28]);
#elif !defined(__GNUC__) && defined(__INTEL_COMPILER)
                          xnum = _mm256_mul_pd(_mm256_add_pd(xnum,gamma_ymm4r8_p[0]),z);
                          xden = _mm256_fmadd_pd(xden,z,gamma_ymm4r8_q[0]);
                          xnum = _mm256_mul_pd(_mm256_add_pd(xnum,gamma_ymm4r8_p[1]),z);
                          xden = _mm256_fmadd_pd(xden,z,gamma_ymm4r8_q[1]);
                          xnum = _mm256_mul_pd(_mm256_add_pd(xnum,gamma_ymm4r8_p[2]),z);
                          xden = _mm256_fmadd_pd(xden,z,gamma_ymm4r8_q[2]);
                          xnum = _mm256_mul_pd(_mm256_add_pd(xnum,gamma_ymm4r8_p[3]),z);
                          xden = _mm256_fmadd_pd(xden,z,gamma_ymm4r8_q[3]);
                          xnum = _mm256_mul_pd(_mm256_add_pd(xnum,gamma_ymm4r8_p[4]),z);
                          xden = _mm256_fmadd_pd(xden,z,gamma_ymm4r8_q[4]);
                          xnum = _mm256_mul_pd(_mm256_add_pd(xnum,gamma_ymm4r8_p[5]),z);
                          xden = _mm256_fmadd_pd(xden,z,gamma_ymm4r8_q[5]);
                          xnum = _mm256_mul_pd(_mm256_add_pd(xnum,gamma_ymm4r8_p[6]),z);
                          xden = _mm256_fmadd_pd(xden,z,gamma_ymm4r8_q[7]);
                          xnum = _mm256_mul_pd(_mm256_add_pd(xnum,gamma_ymm4r8_p[7]),z);
                          xden = _mm256_fmadd_pd(xden,z,gamma_ymm4r8_q[7]);
#endif                          
                          res  = _mm256_add_pd(_mm256_div_pd(xnum,xden),one);
                          // Adjust result for case  0.0 < argument < 1.0.
                          if(_mm256_cmp_pd_mask(y1,y,_CMP_LT_OQ)) 
                             res = _mm256_div_pd(res,y1);
                          else if(_mm256_cmp_pd_mask(y,y1,_CMP_LT_OQ)) {
                          //  Important notice: ******For argument interval: 2.0<=x<12.0 there is a scalarization in form
                          //  of 8 scalar for-loops*******!!
                             __ATTR_ALIGN__(32) int64_t sn[4];
                             __ATTR_ALIGN__(32) double  sres[4];
                             __ATTR_ALIGN__(32) double  sy[4];
                             __ATTR_ALIGN__(32) double  sone[4];
                             int64_t i;
                             _mm256_store_si256(&sn[0],n);
                             _mm256_store_pd(&sres[0],res);
                             _mm256_store_pd(&sy[0],y);
                             _mm256_store_pd(&sone[0],one);
                             for(i=0; i != sn[0]; ++i) {
                                 sres[0] *= sy[0];
                                 sy[0]   += sone[0];
                             }
                             for(i=0; i != sn[1]; ++i) {
                                 sres[1] *= sy[1];
                                 sy[1]   += sone[1];
                             }
                             for(i=0; i != sn[2]; ++i) {
                                 sres[2] *= sy[2];
                                 sy[2]   += sone[2];
                             }
                             for(i=0; i != sn[3]; ++i) {
                                 sres[3] *= sy[3];
                                 sy[3]   += sone[3];
                             }
                            
                             res = _mm256_load_pd(&sres[0]);
                             y   = _mm256_load_pd(&sy[0]);
                          }
                          
                       }
                       else {
                            //  Evaluate for 12.0 <= argument.
                            if(_mm256_cmp_pd_mask(y,xbig,_CMP_LE_OQ)) {
                               ysq = _mm256_mul_pd(y,y);
#if defined(__GNUC__) && !defined(__INTEL_COMPILER)                                 
                               sum = *(__m256d*)&gamma_ymm4r8_c[24];
                               sum = _mm256_add_pd(_mm256_div_pd(sum,ysq),*(__m256d*)&gamma_ymm4r8_c[0]);
                               sum = _mm256_add_pd(_mm256_div_pd(sum,ysq),*(__m256d*)&gamma_ymm4r8_c[4]);
                               sum = _mm256_add_pd(_mm256_div_pd(sum,ysq),*(__m256d*)&gamma_ymm4r8_c[8]);
                               sum = _mm256_add_pd(_mm256_div_pd(sum,ysq),*(__m256d*)&gamma_ymm4r8_c[12]);
                               sum = _mm256_add_pd(_mm256_div_pd(sum,ysq),*(__m256d*)&gamma_ymm4r8_c[16]);
                               sum = _mm256_add_pd(_mm256_div_pd(sum,ysq),*(__m256d*)&gamma_ymm4r8_c[20]);
#elif !defined(__GNUC__) && defined(__INTEL_COMPILER)
                               sum = gamma_ymm4r8_c[6];
                               sum = _mm256_add_pd(_mm256_div_pd(sum,ysq),gamma_ymm4r8_c[0]);
                               sum = _mm256_add_pd(_mm256_div_pd(sum,ysq),gamma_ymm4r8_c[1]);
                               sum = _mm256_add_pd(_mm256_div_pd(sum,ysq),gamma_ymm4r8_c[2]);
                               sum = _mm256_add_pd(_mm256_div_pd(sum,ysq),gamma_ymm4r8_c[3]);
                               sum = _mm256_add_pd(_mm256_div_pd(sum,ysq),gamma_ymm4r8_c[4]);
                               sum = _mm256_add_pd(_mm256_div_pd(sum,ysq),gamma_ymm4r8_c[5]);
#endif                               
                               sum = _mm256_sub_pd(_mm256_div_pd(sum,y),
                                                   _mm256_add_pd(y,sqrtpi));
                               sum = _mm256_mul_pd(_mm256_add_pd(sum,
                                                         _mm256_sub_pd(y,half),_mm256_log_pd(y)));
                               res = _mm256_exp_pd(sum);
                                                    
                            }
                            else {
                               res = xinf;
                               return (res);
                            }
                       }
                       // !  Final adjustments and return.
                       if(parity) res = negate_ymm4r8(res);
                       if(_mm256_cmp_pd_mask(fact,one,_CMP_NEQ_OQ)) res = _mm256_div_pd(fact,res);
                       return (res);
                  }
                  
                  
                  
          
                      __m256 
                      gamma_ymm8r4(const __m256 x) {
                        
                       __m256 pi                          = 
                                        _mm256_set1_ps(3.1415926535897932384626434e+00f);
                       __m256 eps                         =
                                        _mm256_set1_ps(2.22e-16f);
                       __m256 one                         =
                                        _mm256_set1_ps(1.0f);
                       __m256 half                        =
                                        _mm256_set1_ps(0.5f);
                       __m256 sqrtpi                      =
                                        _mm256_set1_ps(0.9189385332046727417803297e+00f);
                       __m256 twelve                      = 
                                        _mm256_set1_ps(12.0f);
                       __m256 two                         =
                                        _mm256_set1_ps(2.0f);
                       __m256 xbig                        =
                                        _mm256_set1_ps(171.624f);
                       __m256 xinf                        =
                                        _mm256_set1_ps(1.0e+30f);
                       __m256 xminin                      =
                                        _mm256_set1_ps(FLT_MIN);
                       
                       __m256 zero                        =
                                        _mm256_setzero_ps();
                       register __m256 res,sum,xden,xnum;
                       register __m256 y,y1,ysq,z,fact;
                       register __m256i n;
                       
                       bool     parity;
                       parity = false;
                       y      = x;
                       // Negative argument
                       if(_mm256_cmp_ps_mask(y,zero,_CMP_LE_OQ)) {
                          register __m256 t0,t1;
                          y  = negate_ymm8r4(x);
                          y1 = _mm256_castsi256_ps(_mm256_cvttps_epu32(y));
                          res= _mm256_sub_ps(y,y1);
                          if(_mm256_cmp_ps_mask(res,zero,_CMP_NEQ_OQ)) {
                            
                             t0 = _mm256_mul_ps(_mm256_mul_ps(y1,half),two);
                             t1 = _mm256_castsi256_ps(_mm256_cvttps_epu32(t0));
                             if(_mm256_cmp_ps_mask(y1,t1,_CMP_NEQ_OQ)) parity = true;
                             t0 = _mm256_sin_ps(_mm256_mul_ps(pi,res));
                             fact = _mm256_div_ps(negate_zmm16r4(pi),t0);
                             y    = _mm256_add_ps(y,one);
                          }
                          else {
                             res = xinf;
                             return (res);
                          }
                       }
                       // Positive argument
                       if(_mm256_cmp_ps_mask(y,eps,_CMP_LT_OQ)) {
                          __mmask8 m;
                          m = _mm256_cmp_ps_mask(xminin,y,_CMP_LE_OQ);
                          res = _mm256_mask_blend_ps(m,xinf,_mm256_div_ps(one,y));
                          return (res);
                       }
                  }
                  else if(_mm256_cmp_ps_mask(y,twelve,_CMP_LT_OQ)) {
                          y1 = y;
                          // 0.0 < argument < 1.0.
                          if(_mm256_cmp_ps_mask(y,one,_CMP_LT_OQ)) {
                             z = y;
                             y = _mm256_add_ps(y,one);
                          }
                          else {
                             //!  1.0 < argument < 12.0.
                             //!  Reduce argument if necessary.
                             n = _mm256_sub_epi32(_mm256_castps_si256(y),
                                                  _mm256_set1_epi32(1));
                             y = _mm256_sub_ps(y,_mm256_castsi256_ps(n));
                             z = _mm256_sub_ps(y,one);
                          }
                          //  Evaluate approximation for 1.0 < argument < 2.0.
                          xnum = zero;
                          xden = one;
#if defined(__GNUC__) && !defined(__INTEL_COMPILER)                          
                          xnum = _mm256_mul_ps(_mm256_add_ps(xnum,*(__m256*)&gamma_ymm8r4_p[0]),z);
                          xden = _mm256_fmadd_ps(xden,z,*(__m256*)&gamma_ymm8r4_q[0]);
                          xnum = _mm256_mul_ps(_mm256_add_ps(xnum,*(__m256*)&gamma_ymm8r4_p[8]),z);
                          xden = _mm256_fmadd_ps(xden,z,*(__m256*)&gamma_ymm8r4_q[8]);
                          xnum = _mm256_mul_ps(_mm256_add_ps(xnum,*(__m256*)&gamma_ymm8r4_p[16]),z);
                          xden = _mm256_fmadd_ps(xden,z,*(__m256*)&gamma_ymm8r4_q[16]);
                          xnum = _mm256_mul_ps(_mm256_add_ps(xnum,*(__m256*)&gamma_ymm8r4_p[24]),z);
                          xden = _mm256_fmadd_ps(xden,z,*(__m256*)&gamma_ymm8r4_q[24]);
                          xnum = _mm256_mul_ps(_mm256_add_ps(xnum,*(__m256*)&gamma_ymm8r4_p[32]),z);
                          xden = _mm256_fmadd_ps(xden,z,*(__m256*)&gamma_ymm8r4_q[32]);
                          xnum = _mm256_mul_ps(_mm256_add_ps(xnum,*(__m256*)&gamma_ymm8r4_p[40]),z);
                          xden = _mm256_fmadd_ps(xden,z,*(__m256*)&gamma_ymm8r4_q[40]);
                          xnum = _mm256_mul_ps(_mm256_add_ps(xnum,*(__m256*)&gamma_ymm8r4_p[48]),z);
                          xden = _mm256_fmadd_ps(xden,z,*(__m256*)&gamma_ymm8r4_q[56]);
                          xnum = _mm256_mul_ps(_mm256_add_ps(xnum,*(__m256*)&gamma_ymm8r4_p[56]),z);
                          xden = _mm256_fmadd_ps(xden,z,*(__m256*)&gamma_ymm8r4_q[56]);
#elif !defined(__GNUC__) && defined(__INTEL_COMPILER)
                          xnum = _mm256_mul_ps(_mm256_add_ps(xnum,gamma_ymm8r4_p[0]),z);
                          xden = _mm256_fmadd_ps(xden,z,gamma_ymm8r4_q[0]);
                          xnum = _mm256_mul_ps(_mm256_add_ps(xnum,gamma_ymm8r4_p[1]),z);
                          xden = _mm256_fmadd_ps(xden,z,gamma_ymm8r4_q[1]);
                          xnum = _mm256_mul_ps(_mm256_add_ps(xnum,gamma_ymm8r4_p[2]),z);
                          xden = _mm256_fmadd_ps(xden,z,gamma_ymm8r4_q[2]);
                          xnum = _mm256_mul_ps(_mm256_add_ps(xnum,gamma_ymm8r4_p[3]),z);
                          xden = _mm256_fmadd_ps(xden,z,gamma_ymm8r4_q[3]);
                          xnum = _mm256_mul_ps(_mm256_add_ps(xnum,gamma_ymm8r4_p[4]),z);
                          xden = _mm256_fmadd_ps(xden,z,gamma_ymm8r4_q[4]);
                          xnum = _mm256_mul_ps(_mm256_add_ps(xnum,gamma_ymm8r4_p[5]),z);
                          xden = _mm256_fmadd_ps(xden,z,gamma_ymm8r4_q[5]);
                          xnum = _mm256_mul_ps(_mm256_add_ps(xnum,gamma_ymm8r4_p[6]),z);
                          xden = _mm256_fmadd_ps(xden,z,gamma_ymm8r4_q[7]);
                          xnum = _mm256_mul_ps(_mm256_add_ps(xnum,gamma_ymm8r4_p[7]),z);
                          xden = _mm256_fmadd_ps(xden,z,gamma_ymm8r4_q[7]);
#endif                          
                          res  = _mm256_add_ps(_mm256_div_ps(xnum,xden),one);
                          // Adjust result for case  0.0 < argument < 1.0.
                          if(_mm256_cmp_ps_mask(y1,y,_CMP_LT_OQ)) 
                             res = _mm256_div_ps(res,y1);
                          else if(_mm256_cmp_ps_mask(y,y1,_CMP_LT_OQ)) {
                          //  Important notice: ******For argument interval: 2.0<=x<12.0 there is a scalarization in form
                          //  of 8 scalar for-loops*******!!
                             __ATTR_ALIGN__(32) int32_t sn[8];
                             __ATTR_ALIGN__(32) float  sres[8];
                             __ATTR_ALIGN__(32) float  sy[8];
                             __ATTR_ALIGN__(32) float  sone[8];
                             int32_t i;
                             _mm256_store_si256(&sn[0],n);
                             _mm256_store_ps(&sres[0],res);
                             _mm256_store_ps(&sy[0],y);
                             _mm256_store_ps(&sone[0],one);
                             for(i=0; i != sn[0]; ++i) {
                                 sres[0] *= sy[0];
                                 sy[0]   += sone[0];
                             }
                             for(i=0; i != sn[1]; ++i) {
                                 sres[1] *= sy[1];
                                 sy[1]   += sone[1];
                             }
                             for(i=0; i != sn[2]; ++i) {
                                 sres[2] *= sy[2];
                                 sy[2]   += sone[2];
                             }
                             for(i=0; i != sn[3]; ++i) {
                                 sres[3] *= sy[3];
                                 sy[3]   += sone[3];
                             }
                             for(i=0; i != sn[4]; ++i) {
                                 sres[4] *= sy[4];
                                 sy[4]   += sone[4];
                             }
                             for(i=0; i != sn[5]; ++i) {
                                 sres[5] *= sy[5];
                                 sy[5]   += sone[5];
                             }
                             for(i=0; i != sn[6]; ++i) {
                                 sres[6] *= sy[6];
                                 sy[6]   += sone[6];
                             }
                             for(i=0; i != sn[7]; ++i) {
                                 sres[7] *= sy[7];
                                 sy[7]   += sone[7];
                             }
                              
                             res = _mm256_load_ps(&sres[0]);
                             y   = _mm256_load_ps(&sy[0]);
                          }
                          
                       }
                       else {
                            //  Evaluate for 12.0 <= argument.
                            if(_mm256_cmp_ps_mask(y,xbig,_CMP_LE_OQ)) {
                               ysq = _mm256_mul_ps(y,y);
#if defined(__GNUC__) && !defined(__INTEL_COMPILER)                                 
                               sum = *(__m256*)&gamma_ymm8r4_c[48];
                               sum = _mm256_add_ps(_mm256_div_ps(sum,ysq),*(__m256*)&gamma_ymm8r4_c[0]);
                               sum = _mm256_add_ps(_mm256_div_ps(sum,ysq),*(__m256*)&gamma_ymm8r4_c[8]);
                               sum = _mm256_add_ps(_mm256_div_ps(sum,ysq),*(__m256*)&gamma_ymm8r4_c[16]);
                               sum = _mm256_add_ps(_mm256_div_ps(sum,ysq),*(__m256*)&gamma_ymm8r4_c[24]);
                               sum = _mm256_add_ps(_mm256_div_ps(sum,ysq),*(__m256*)&gamma_ymm8r4_c[32]);
                               sum = _mm256_add_ps(_mm256_div_ps(sum,ysq),*(__m256*)&gamma_ymm8r4_c[40]);
#elif !defined(__GNUC__) && defined(__INTEL_COMPILER)
                                sum = gamma_ymm8r4_c[6];
                               sum = _mm256_add_ps(_mm256_div_ps(sum,ysq),gamma_ymm8r4_c[0]);
                               sum = _mm256_add_ps(_mm256_div_ps(sum,ysq),gamma_ymm8r4_c[1]);
                               sum = _mm256_add_ps(_mm256_div_ps(sum,ysq),gamma_ymm8r4_c[2]);
                               sum = _mm256_add_ps(_mm256_div_ps(sum,ysq),gamma_ymm8r4_c[3]);
                               sum = _mm256_add_ps(_mm256_div_ps(sum,ysq),gamma_ymm8r4_c[4]);
                               sum = _mm256_add_ps(_mm256_div_ps(sum,ysq),gamma_ymm8r4_c[5]);
#endif                               
                               sum = _mm256_sub_ps(_mm256_div_ps(sum,y),
                                                   _mm256_add_ps(y,sqrtpi));

                               sum = _mm256_mul_ps(_mm256_add_ps(sum,
                                                         _mm256_sub_ps(y,half),_mm256_log_ps(y)));
                               res = _mm256_exp_ps(sum);


                                                    
                            }
                            else {
                               res = xinf;
                               return (res);
                            }
                       }
                       // !  Final adjustments and return.
                       if(parity) res = negate_ymm8r4(res);
                       if(_mm256_cmp_ps_mask(fact,one,_CMP_NEQ_OQ)) res = _mm256_div_ps(fact,res);
                       return (res);
                  }
                  
		                                                                      
 /*
!*****************************************************************************80
!
!! STUDENT_PDF evaluates the central Student T PDF.
!
!  Discussion:
!
!    PDF(A,B,C;X) = Gamma ( (C+1)/2 ) /
!      ( Gamma ( C / 2 ) * Sqrt ( PI * C )
!      * ( 1 + ((X-A)/B)^2/C )^(C + 1/2 ) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 November 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, shape parameters of the PDF,
!    used to transform the argument X to a shifted and scaled
!    value Y = ( X - A ) / B.  It is required that B be nonzero.
!    For the standard distribution, A = 0 and B = 1.
!
!    Input, real ( kind = 8 ) C, is usually called the number of
!    degrees of freedom of the distribution.  C is typically an
!    integer, but that is not essential.  It is required that
!    C be strictly positive.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
*/


          
		           
                      __m256d 
                      student_pdf_ymm4r8(const __m256d x,
                                         const __m256d a,
                                         const __m256d b,
                                         const __m256d c) {
                         
                         __m256d C314159265358979323846264 = 
                                           _mm256_set1_pd(3.14159265358979323846264);   
                         __m256d C1 = _mm256_set1_pd(1.0);
                         __m256d C05= _mm256_set1_pd(0.5);  
                         __m256d C2 = _mm256_set1_pd(2.0); 
                         register __m256d y,t0,t1,t2,t3;
                         register __m256d r8g1,r8g2,pdf;
                         t0   = _mm256_mul_pd(C05,_mm256_add_pd(c,C1));
                         y    = _mm256_div_pd(_mm256_sub_pd(x,a),b);
                         r8g1 = gamma_zmm8r8(t0);
                         t1   = _mm256_fmadd_pd(C2,c,C1);
                         t2   = _mm256_add_pd(_mm256_div_pd(_mm256_mul_pd(y,y),c),C1);
                         t0   = _mm256_pow_pd(t2,t1); //used
                         r8g2 = gamma_ymm4r8(_mm256_mul_pd(C05,c));
                         y    = _mm256_sqrt_pd(_mm256_mul_pd(C314159265358979323846264,c));
                         t3   = _mm256_mul_pd(y,_mm256_mul_pd(r8g2,t0));
                         pdf  = _mm256_div_pd(r8g1,t3);
                         return (pdf);
                   }
                   
                   
                           
                      __m256 
                      student_pdf_ymm8r4(const __m256 x,
                                         const __m256 a,
                                         const __m256 b,
                                         const __m256 c) {
                         
                         __m256 C314159265358979323846264 = 
                                           _mm256_set1_pd(3.14159265358979323846264f);   
                         __m256 C1 = _mm256_set1_ps(1.0f);
                         __m256 C05= _mm256_set1_ps(0.5f);  
                         __m256 C2 = _mm256_set1_ps(2.0f); 
                         register __m256 y,t0,t1,t2,t3;
                         register __m256 r8g1,r8g2,pdf;
                         t0   = _mm256_mul_ps(C05,_mm256_add_ps(c,C1));
                         y    = _mm256_div_ps(_mm256_sub_ps(x,a),b);
                         r8g1 = gamma_ymm8r4(t0);
                         t1   = _mm256_fmadd_ps(C2,c,C1);
                         t2   = _mm256_add_ps(_mm256_div_ps(_mm256_mul_ps(y,y),c),C1);
                         t0   = _mm256_pow_ps(t2,t1); //used
                         r8g2 = gamma_ymm8r4(_mm256_mul_ps(C05,c));
                         y    = _mm256_sqrt_ps(_mm256_mul_ps(C314159265358979323846264,c));
                         t3   = _mm256_mul_ps(y,_mm256_mul_ps(r8g2,t0));
                         pdf  = _mm256_div_ps(r8g1,t3);
                         return (pdf);
                   }
  
                  
                  
 /*                 
!*****************************************************************************80
!
!! STUDENT_VARIANCE returns the variance of the central Student T PDF.
!
!  Discussion:
!
!    The variance is not defined unless 2 < C.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 November 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, shape parameters of the PDF,
!    used to transform the argument X to a shifted and scaled
!    value Y = ( X - A ) / B.  It is required that B be nonzero.
!    For the standard distribution, A = 0 and B = 1.
!
!    Input, real ( kind = 8 ) C, is usually called the number of
!    degrees of freedom of the distribution.  C is typically an
!    integer, but that is not essential.  It is required that
!    C be strictly positive.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
*/  


                      
                      __m256d 
                      student_variance_ymm4r8(const __m256d a,
                                              const __m256d b,
                                              const __m256d c) {
                                              
                          __m256d C2 = _mm256_set1_pd(2.0);     
                          register __m256d bb,t1;
                          register __m256d var;
                          bb = _mm256_mul_pd(b,b);
                          t1 = _mm256_sub_pd(c,C2);
                          var= _mm256_mul_pd(bb,_mm256_div_pd(c,t1));
                          return (var);                    
                    }   
                    
                    
                           
                      __m256
                      student_variance_ymm8r4(const __m256 a,
                                              const __m256 b,
                                              const __m256 c) {
                                              
                          __m256 C2 = _mm256_set1_ps(2.0f);     
                          register __m256 bb,t1;
                          register __m256 var;
                          bb = _mm256_mul_ps(b,b);
                          t1 = _mm256_sub_ps(c,C2);
                          var= _mm256_mul_ps(bb,_mm256_div_ps(c,t1));
                          return (var);                    
                    }   
                    
                    
/*
  !*****************************************************************************80
!
!! WEIBULL_PDF evaluates the Weibull PDF.
!
!  Discussion:
!
!    PDF(A,B,C;X) = ( C / B ) * ( ( X - A ) / B )**( C - 1 )
!     * EXP ( - ( ( X - A ) / B )**C ).
!
!    The Weibull PDF is also known as the Frechet PDF.
!
!    WEIBULL_PDF(A,B,1;X) is the Exponential PDF.
!
!    WEIBULL_PDF(0,1,2;X) is the Rayleigh PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    A <= X
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0D+00 < B,
!    0.0D+00 < C.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
! 
*/


          
	                 
                      __m256d	 
                      weibull_pdf_ymm4r8(const __m256d x,
                                         const __m256d a,
                                         const __m256d b,
                                         const __m256d c) {
                        
                         register __m256d C1 = _mm256_set1_pd(1.0);
                         register __m256d y,t0,pow1,t1,exp;
                         register __m256d pdf;
                         t0 = _mm256_div_pd(_mm256_sub_pd(x,a),b);
                         pow1 = _mm256_pow_pd(t0,_mm256_sub_pd(c,C1));
                         exp  = _mm256_exp_pd(_mm256_pow_pd(y,c));
                         t1   = _mm256_div_pd(c,b);
                         pdf  = _mm256_div_pd(_mm256_mul_pd(t1,pow1),exp); 
                         return (pdf);     
                   }
                   
                   
                           
                      __m256	 
                      weibull_pdf_ymm8r4(const __m256 x,
                                         const __m256 a,
                                         const __m256 b,
                                         const __m256 c) {
                        
                         register __m256 C1 = _mm256_set1_ps(1.0f);
                         register __m256 y,t0,pow1,t1,exp;
                         register __m256 pdf;
                         t0 = _mm256_div_ps(_mm256_sub_ps(x,a),b);
                         pow1 = _mm256_pow_ps(t0,_mm256_sub_ps(c,C1));
                         exp  = _mm256_exp_ps(_mm256_pow_ps(y,c));
                         t1   = _mm256_div_ps(c,b);
                         pdf  = _mm256_div_ps(_mm256_mul_ps(t1,pow1),exp); 
                         return (pdf);     
                   }
                    		     
		     		     
 /*
     !*****************************************************************************80
!
!! TRIGAMMA calculates the TriGamma function.
!
!  Discussion:
!
!    TriGamma(x) = d^2 log ( Gamma ( x ) ) / dx^2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2000
!
!  Author:
!
!    FORTRAN77 original version by B Schneider
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    BE Schneider,
!    Algorithm AS 121:
!    Trigamma Function,
!    Applied Statistics,
!    Volume 27, Number 1, page 97-99, 1978.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the trigamma function.
!    0 < X.
!
!    Output, real ( kind = 8 ) TRIGAMMA, the value of the
!    trigamma function at X.
!     
*/

      
                      __m256d
                      trigamma_ymm4r8(const __m256d x) {
                         
                         __m256d a  = _mm256_setzero_pd();
                         __m256d C1 = _mm256_set1_pd(1.0);
                         __m256d C05=_mm256_set1_pd(0.5);
                         __m256d b  = _mm256_set1_pd(5.0);
                         __m256d b2 = _mm256_set1_pd(1.0/6.0);
                         __m256d b4 = _mm256_set1_pd(-1.0/30.0);
                         __m256d b6 = _mm256_set1_pd(1.0/42.0);
                         __m256d b8 = _mm256_set1_pd(-1.0/30.0);
                         register __m256d y,z,t0,t1;
                         register __m256d trig;
                         
                         if(_mm256_cmp_pd_mask(x,a,_CMP_LE_OQ)) {
                            trig = _mm256_div_pd(C1,_mm256_mul_pd(x,x));
                         }
                         else {
                            z = x;
                            trig = a;
                            while(_mm256_cmp_pd_mask(z,b,_CMP_LT_OQ)) {
                                  trig = _mm256_add_pd(_mm256_div_pd(C1,
                                                         _mm256_mul_pd(z,z)))
                                  z    = _mm256_add_pd(z,C1);
                            }
                            y    = _mm256_div_pd(C1,_mm256_mul_pd(z,z));
                            trig = trig+C05*y+(C1+y*(b2+y*(b4+y*(b6+y*b8))))/z; 
                         }
                         return (trig);
                    } 
                    
                        
                      __m256
                      trigamma_ymm8r4(const __m256 x) {
                         
                         __m256 a  = _mm256_setzero_ps();
                         __m256 C1 = _mm256_set1_ps(1.0f);
                         __m256 C05=_mm256_set1_ps(0.5f);
                         __m256 b  = _mm256_set1_ps(5.0f);
                         __m256 b2 = _mm256_set1_ps(1.0f/6.0f);
                         __m256 b4 = _mm256_set1_ps(-1.0f/30.0f);
                         __m256 b6 = _mm256_set1_ps(1.0f/42.0f);
                         __m256 b8 = _mm256_set1_ps(-1.0f/30.0f);
                         register __m256 y,z,t0,t1;
                         register __m256 trig;
                         
                         if(_mm256_cmp_ps_mask(x,a,_CMP_LE_OQ)) {
                            trig = _mm256_div_ps(C1,_mm256_mul_ps(x,x));
                         }
                         else {
                            z = x;
                            trig = a;
                            while(_mm256_cmp_ps_mask(z,b,_CMP_LT_OQ)) {
                                  trig = _mm256_add_ps(_mm256_div_ps(C1,
                                                         _mm256_mul_ps(z,z)))
                                  z    = _mm256_add_ps(z,C1);
                            }
                            y    = _mm256_div_ps(C1,_mm256_mul_ps(z,z));
                            trig = trig+C05*y+(C1+y*(b2+y*(b4+y*(b6+y*b8))))/z; 
                         }
                         return (trig);
                    } 
                    
 /*
    !*****************************************************************************80
!
!! CHI_PDF evaluates the Chi PDF.
!
!  Discussion:
!
!    PDF(A,B,C;X) = EXP ( - 0.5D+00 * ( ( X - A ) / B )^2 )
!      * ( ( X - A ) / B )^( C - 1 ) /
!      ( 2^( 0.5D+00 * C - 1 ) * B * GAMMA ( 0.5D+00 * C ) )
!
!    CHI(A,B,1) is the Half Normal PDF;
!    CHI(0,B,2) is the Rayleigh PDF;
!    CHI(0,B,3) is the Maxwell PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    A <= X
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0 < B,
!    0 < C.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
*/


                   
		      __m256d   
                      chi_pdf_ymm4r8(const __m256d x,
                                     const __m256d a,
                                     const __m256d b,
                                     const __m256d c) {
                          
                          const __m256d C05 = _mm256_set1_pd(0.5);
                          const __m256d C1  = _mm256_set1_pd(1.0);
                          const __m256d C2  = _mm256_set1_pd(2.0);
                          register __m256d y,t0,t1,t2,g0,exp;
                          register __m256d pdf;
                          t0 = _mm256_mul_pd(_mm256_mul_pd(negate_ymm4r8(C05,y),y),y);
                          t1 = _mm256_fmsub_pd(C05,c,C1);
                          y  = _mm256_div_pd(_mm256_sub_pd(x,a),b);
                          g0 = gamma_ymm4r8(_mm256_mul_pd(C05,c));  
                          exp= _mm256_mul_pd(_mm256_exp_pd(t0),_mm256_pow_pd(_mm256_sub_pd(c,C1)));
                          t2 = _mm256_mul_pd(_mm256_pow_pd(C2,t1),
                                             _mm256_mul_pd(b,g0));
                          pdf = _mm256_div_pd(exp,t2);
                          return (pdf);
                    }
                    
                    
                  
		      __m256   
                      chi_pdf_ymm8r4(const __m256 x,
                                     const __m256 a,
                                     const __m256 b,
                                     const __m256 c) {
                          
                          const __m256 C05 = _mm256_set1_ps(0.5f);
                          const __m256 C1  = _mm256_set1_ps(1.0f);
                          const __m256 C2  = _mm256_set1_ps(2.0f);
                          register __m256 y,t0,t1,t2,g0,exp;
                          register __m256 pdf;
                          t0 = _mm256_mul_ps(_mm256_mul_ps(negate_ymm8r4(C05,y),y),y);
                          t1 = _mm256_fmsub_ps(C05,c,C1);
                          y  = _mm256_div_ps(_mm256_sub_ps(x,a),b);
                          g0 = gamma_ymm8r4(_mm256_mul_ps(C05,c));  
                          exp= _mm256_mul_ps(_mm256_exp_ps(t0),_mm256_pow_ps(_mm256_sub_ps(c,C1)));
                          t2 = _mm256_mul_ps(_mm256_pow_ps(C2,t1),
                                             _mm256_mul_ps(b,g0));
                          pdf = _mm256_div_ps(exp,t2);
                          return (pdf);
                    }






               
		   		    
		    

		    		    

		     		  

