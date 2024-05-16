


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
#include "GMS_pdf_cdf_sse.h"
#include "GMS_simd_utils.h"

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


        __m128d gamma_log_xmm2r8(const __m128d x) {
		      
                           // if(__builtin_expect(_mm_cmp_pd_mask(x,_0,_CMP_LE_OQ),0) ||
			   //    __builtin_expect(_mm_cmp_pd_mask(x,xbig,_CMP_GT_OQ),0)) {
                           //    return (huge);
			   // }
                         __attribute__((section(".rodata")))
                         __attribute__((aligned(16))) static __m128d c[7] = { _mm_set1_pd(-1.910444077728E-03),
			                                     _mm_set1_pd(8.4171387781295E-04),
                                                             _mm_set1_pd(-5.952379913043012E-04), 
                                                             _mm_set1_pd(7.93650793500350248E-04), 
                                                             _mm_set1_pd(-2.777777777777681622553E-03), 
                                                             _mm_set1_pd(8.333333333333333331554247E-02), 
                                                             _mm_set1_pd(5.7083835261E-03)};
                         __attribute__((section(".rodata")))
			 __attribute__((aligned(16))) static __m128d p1[8] = {_mm_set1_pd(4.945235359296727046734888E+00), 
                                                             _mm_set1_pd(2.018112620856775083915565E+02), 
                                                             _mm_set1_pd(2.290838373831346393026739E+03), 
                                                             _mm_set1_pd(1.131967205903380828685045E+04),
                                                             _mm_set1_pd(2.855724635671635335736389E+04), 
                                                             _mm_set1_pd(3.848496228443793359990269E+04), 
                                                             _mm_set1_pd(2.637748787624195437963534E+04), 
                                                             _mm_set1_pd(7.225813979700288197698961E+03)};
                         __attribute__((section(".rodata")))
			 __attribute__((aligned(16))) static __m128d p2[8] = {_mm_set1_pd(4.974607845568932035012064E+00), 
                                                             _mm_set1_pd(5.424138599891070494101986E+02), 
                                                             _mm_set1_pd(1.550693864978364947665077E+04), 
                                                             _mm_set1_pd(1.847932904445632425417223E+05), 
                                                             _mm_set1_pd(1.088204769468828767498470E+06), 
                                                             _mm_set1_pd(3.338152967987029735917223E+06), 
                                                             _mm_set1_pd(5.106661678927352456275255E+06), 
                                                             _mm_set1_pd(3.074109054850539556250927E+06)};
                         __attribute__((section(".rodata")))
			 __attribute__((aligned(16))) static __m128d p4[8] = {_mm_set1_pd(1.474502166059939948905062E+04), 
                                                             _mm_set1_pd(2.426813369486704502836312E+06), 
                                                             _mm_set1_pd(1.214755574045093227939592E+08), 
                                                             _mm_set1_pd(2.663432449630976949898078E+09), 
                                                             _mm_set1_pd(2.940378956634553899906876E+10), 
                                                             _mm_set1_pd(1.702665737765398868392998E+11), 
                                                             _mm_set1_pd(4.926125793377430887588120E+11), 
                                                             _mm_set1_pd(5.606251856223951465078242E+11)};
                         __attribute__((section(".rodata")))
                         __attribute__((aligned(16))) static __m128d q1[8] = {_mm_set1_pd(6.748212550303777196073036E+01), 
                                                             _mm_set1_pd(1.113332393857199323513008E+03), 
                                                             _mm_set1_pd(7.738757056935398733233834E+03), 
                                                             _mm_set1_pd(2.763987074403340708898585E+04), 
                                                             _mm_set1_pd(5.499310206226157329794414E+04), 
                                                             _mm_set1_pd(6.161122180066002127833352E+04), 
                                                             _mm_set1_pd(3.635127591501940507276287E+04), 
                                                             _mm_set1_pd(8.785536302431013170870835E+03)};
                         __attribute__((section(".rodata")))
			 __attribute__((aligned(16))) static __m128d q2[8] = {_mm_set1_pd(1.830328399370592604055942E+02),
                                                             _mm_set1_pd(7.765049321445005871323047E+03), 
                                                             _mm_set1_pd(1.331903827966074194402448E+05),
                                                             _mm_set1_pd(1.136705821321969608938755E+06), 
                                                             _mm_set1_pd(5.267964117437946917577538E+06), 
                                                             _mm_set1_pd(1.346701454311101692290052E+07), 
                                                             _mm_set1_pd(1.782736530353274213975932E+07), 
                                                             _mm_set1_pd(9.533095591844353613395747E+06)};
                         __attribute__((section(".rodata")))
			 __attribute__((aligned(16))) static __m128d q4[8] = {_mm_set1_pd(2.690530175870899333379843E+03), 
                                                             _mm_set1_pd(6.393885654300092398984238E+05), 
                                                             _mm_set1_pd(4.135599930241388052042842E+07), 
                                                             _mm_set1_pd(1.120872109616147941376570E+09), 
                                                             _mm_set1_pd(1.488613728678813811542398E+10), 
                                                             _mm_set1_pd(1.016803586272438228077304E+11), 
                                                             _mm_set1_pd(3.417476345507377132798597E+11), 
                                                             _mm_set1_pd(4.463158187419713286462081E+11)};
			    const __m128d d1     = _mm_set1_pd(-5.772156649015328605195174E-01);
			    const __m128d d2     = _mm_set1_pd(4.227843350984671393993777E-01);
                            const __m128d d4     = _mm_set1_pd(1.791759469228055000094023E+00);
                            const __m128d frtbig = _mm_set1_pd(1.42E+09);
                            const __m128d pnt68  = _mm_set1_pd(0.6796875E+00);
			    const __m128d sqrtpi = _mm_set1_pd(0.9189385332046727417803297E+00);
			    const __m128d xbig   = _mm_set1_pd(4.08E+36);
			    const __m128d _0     = _mm_setzero_pd();
			    const __m128d _1_2   = _mm_set1_pd(0.5);
			    const __m128d _1_5   = _mm_set1_pd(1.5);
			    const __m128d _1     = _mm_set1_pd(1.0);
			    const __m128d _4     = _mm_set1_pd(4.0);
			    const __m128d _2     = _mm_set1_pd(2.0);
			    const __m128d _12    = _mm_set1_pd(12.0);
			    const __m128d huge   = _mm_set1_pd(DBL_MAX);
			    const __m128d eps    = _mm_set1_pd(DBL_EPSILON);
			    __m128d gamlog,res,xden;
			    __m128d xm1,xm2,xm4;
			    __m128d xnum,xsq,corr;
			    gamlog = _mm_setzero_pd();
			   
			    if(_mm_cmp_pd_mask(x,eps,_CMP_LE_OQ)) {
                               res = negate_xmm2r8(_mm_log_pd(x));
			    }
			    else if(_mm_cmp_pd_mask(x,_1_5,_CMP_LE_OQ)) {
                               const __mmask8 m0 = _mm_cmp_pd_mask(x,pnt68,_CMP_LT_OQ);
			       corr = _mm_mask_blend_pd(m0,_0,xmm2r8_negate(_mm_log_pd(x)));
			       xm1  = _mm_mask_blend_pd(m0,_mm_sub_pd(
			                                                _mm_sub_pd(x,_1_2),_1_2));

			       if(_mm_cmp_pd_mask(x,_1_2,_CMP_LE_OQ) ||
			          _mm_cmp_pd_mask(pnt68,x,_CMP_LE_OQ)) {
                                   xden = _1;
				   xnum = _0;
				   xnum = _mm_fmadd_pd(xnum,xm1,p1[0]);
				   xden = _mm_fmadd_pd(xden,xm1,q1[0]);
				   xnum = _mm_fmadd_pd(xnum,xm1,p1[1]);
				   xden = _mm_fmadd_pd(xden,xm1,q1[1]);
				   xnum = _mm_fmadd_pd(xnum,xm1,p1[2]);
				   xden = _mm_fmadd_pd(xden,xm1,q1[2]);
				   xnum = _mm_fmadd_pd(xnum,xm1,p1[3]);
				   xden = _mm_fmadd_pd(xden,xm1,q1[3]);
				   xnum = _mm_fmadd_pd(xnum,xm1,p1[4]);
				   xden = _mm_fmadd_pd(xden,xm1,q1[4]);
				   xnum = _mm_fmadd_pd(xnum,xm1,p1[5]);
				   xden = _mm_fmadd_pd(xden,xm1,q1[5]);
				   xnum = _mm_fmadd_pd(xnum,xm1,p1[6]);
				   xden = _mm_fmadd_pd(xden,xm1,q1[6]);
				   xnum = _mm_fmadd_pd(xnum,xm1,p1[7]);
				   xden = _mm_fmadd_pd(xden,xm1,q1[7]);
				   const __m128d t0 = _mm_fmadd_pd(xm1,
				                                  _mm_div_pd(xnum,xden),d1);
				   res  = _mm_add_pd(corr,
				                    _mm_mul_pd(xm1,t0));
				}
				else {

                                   xm2  = _mm_sub_pd(_mm_sub_pd(x,_1_2),_1_2);
				   xden = _1;
				   xnum = _0;
				   xnum = _mm_fmadd_pd(xnum,xm2,p2[0]);
				   xden = _mm_fmadd_pd(xden,xm2,q2[0]);
				   xnum = _mm_fmadd_pd(xnum,xm2,p2[1]);
				   xden = _mm_fmadd_pd(xden,xm2,q2[1]);
				   xnum = _mm_fmadd_pd(xnum,xm2,p2[2]);
				   xden = _mm_fmadd_pd(xden,xm2,q2[2]);
				   xnum = _mm_fmadd_pd(xnum,xm2,p2[3]);
				   xden = _mm_fmadd_pd(xden,xm2,q2[3]);
				   xnum = _mm_fmadd_pd(xnum,xm2,p2[4]);
				   xden = _mm_fmadd_pd(xden,xm2,q2[4]);
				   xnum = _mm_fmadd_pd(xnum,xm2,p2[5]);
				   xden = _mm_fmadd_pd(xden,xm2,q2[5]);
				   xnum = _mm_fmadd_pd(xnum,xm2,p2[6]);
				   xden = _mm_fmadd_pd(xden,xm2,q2[6]);
				   xnum = _mm_fmadd_pd(xnum,xm2,p2[7]);
				   xden = _mm_fmadd_pd(xden,xm2,q2[7]);
				   const __m128d t0 = _mm_fmadd_pd(xm2,
				                                  _mm_div_pd(xnum,xden),d2);
				   res  = _mm_add_pd(corr,
				                    _mm_mul_pd(xm2,t0));
				}
			    }
			    else if(_mm_cmp_pd_mask(x,_4,_CMP_LE_OQ)) {
                                   xm2  = _mm_sub_pd(x,_2);
				   xden = _1;
				   xnum = _0;
				   xnum = _mm_fmadd_pd(xnum,xm2,p2[0]);
				   xden = _mm_fmadd_pd(xden,xm2,q2[0]);
				   xnum = _mm_fmadd_pd(xnum,xm2,p2[1]);
				   xden = _mm_fmadd_pd(xden,xm2,q2[1]);
				   xnum = _mm_fmadd_pd(xnum,xm2,p2[2]);
				   xden = _mm_fmadd_pd(xden,xm2,q2[2]);
				   xnum = _mm_fmadd_pd(xnum,xm2,p2[3]);
				   xden = _mm_fmadd_pd(xden,xm2,q2[3]);
				   xnum = _mm_fmadd_pd(xnum,xm2,p2[4]);
				   xden = _mm_fmadd_pd(xden,xm2,q2[4]);
				   xnum = _mm_fmadd_pd(xnum,xm2,p2[5]);
				   xden = _mm_fmadd_pd(xden,xm2,q2[5]);
				   xnum = _mm_fmadd_pd(xnum,xm2,p2[6]);
				   xden = _mm_fmadd_pd(xden,xm2,q2[6]);
				   xnum = _mm_fmadd_pd(xnum,xm2,p2[7]);
				   xden = _mm_fmadd_pd(xden,xm2,q2[7]);
				   res  = _mm_mul_pd(xm2,
				                    _mm_fmadd_pd(xm2,
						                _mm_div_pd(xnum,xden),d2));
			    }
			    else if(_mm_cmp_pd_mask(x,_12,_CMP_LE_OQ)) {
                                   xm4  = _mm_sub_pd(x,_4);
				   xden = negate_xmm2r8(_1);
				   xnum = _0;
				   xnum = _mm_fmadd_pd(xnum,xm4,p4[0]);
				   xden = _mm_fmadd_pd(xden,xm4,q4[0]);
				   xnum = _mm_fmadd_pd(xnum,xm4,p4[1]);
				   xden = _mm_fmadd_pd(xden,xm4,q4[1]);
				   xnum = _mm_fmadd_pd(xnum,xm4,p4[2]);
				   xden = _mm_fmadd_pd(xden,xm4,q4[2]);
				   xnum = _mm_fmadd_pd(xnum,xm4,p4[3]);
				   xden = _mm_fmadd_pd(xden,xm4,q4[3]);
				   xnum = _mm_fmadd_pd(xnum,xm4,p4[4]);
				   xden = _mm_fmadd_pd(xden,xm4,q4[4]);
				   xnum = _mm_fmadd_pd(xnum,xm4,p4[5]);
				   xden = _mm_fmadd_pd(xden,xm4,q4[5]);
				   xnum = _mm_fmadd_pd(xnum,xm4,p4[6]);
				   xden = _mm_fmadd_pd(xden,xm4,q4[6]);
				   xnum = _mm_fmadd_pd(xnum,xm4,p4[7]);
				   xden = _mm_fmadd_pd(xden,xm4,q4[7]);
				   res  = _mm_fmadd_pd(xm4,_mm_div_pd(xnum,xden),d4);
			    }
			    else {
                                   res  = _0;
				   if(_mm_cmp_pd_mask(x,frtbig,_CMP_LE_OQ)) {
                                      res = c[6];
				      xsq = _mm_mul_pd(x,x);
				      res = _mm_add_pd(_mm_div_pd(res,xsq),c[0]);
				      res = _mm_add_pd(_mm_div_pd(res,xsq),c[1]);
				      res = _mm_add_pd(_mm_div_pd(res,xsq),c[2]);
				      res = _mm_add_pd(_mm_div_pd(res,xsq),c[3]);
				      res = _mm_add_pd(_mm_div_pd(res,xsq),c[4]);
				      res = _mm_add_pd(_mm_div_pd(res,xsq),c[5]);
				   }
                                   res  = _mm_div_pd(res,x);
				   corr = _mm_log_pd(x);
				   res  = _mm_sub_pd(_mm_add_pd(res,sqrtpi),
				                        _mm_mul_pd(_1_2,corr));
				   res  = _mm_fmadd_pd(x,_mm_sub_pd(corr,_1),res);
				   
			    }

			    gamlog = res;
			    return (gamlog);
			    
		  }
		  
		  
		 
		  __m128 gamma_log_xmm4r4(const __m128 x) {
		      
                          //  if(__builtin_expect(_mm_cmp_ps_mask(x,_0,_CMP_LE_OQ),0) ||
			 //      __builtin_expect(_mm_cmp_ps_mask(x,xbig,_CMP_GT_OQ),0)) {
                         //      return (huge);
			 //   }
                         __attribute__((section(".rodata")))
                         __attribute__((aligned(16))) static __m128 c[7] = { _mm_set1_ps(-1.910444077728E-03),
			                                     _mm_set1_ps(8.4171387781295E-04),
                                                             _mm_set1_ps(-5.952379913043012E-04), 
                                                             _mm_set1_ps(7.93650793500350248E-04), 
                                                             _mm_set1_ps(-2.777777777777681622553E-03), 
                                                             _mm_set1_ps(8.333333333333333331554247E-02), 
                                                             _mm_set1_ps(5.7083835261E-03)};
                         __attribute__((section(".rodata")))
			 __attribute__((aligned(16))) static __m128 p1[8] = {_mm_set1_ps(4.945235359296727046734888E+00), 
                                                             _mm_set1_ps(2.018112620856775083915565E+02), 
                                                             _mm_set1_ps(2.290838373831346393026739E+03), 
                                                             _mm_set1_ps(1.131967205903380828685045E+04),
                                                             _mm_set1_ps(2.855724635671635335736389E+04), 
                                                             _mm_set1_ps(3.848496228443793359990269E+04), 
                                                             _mm_set1_ps(2.637748787624195437963534E+04), 
                                                             _mm_set1_ps(7.225813979700288197698961E+03)};
                         __attribute__((section(".rodata")))
			 __attribute__((aligned(16))) static __m128 p2[8] = {_mm_set1_ps(4.974607845568932035012064E+00), 
                                                             _mm_set1_ps(5.424138599891070494101986E+02), 
                                                             _mm_set1_ps(1.550693864978364947665077E+04), 
                                                             _mm_set1_ps(1.847932904445632425417223E+05), 
                                                             _mm_set1_ps(1.088204769468828767498470E+06), 
                                                             _mm_set1_ps(3.338152967987029735917223E+06), 
                                                             _mm_set1_ps(5.106661678927352456275255E+06), 
                                                             _mm_set1_ps(3.074109054850539556250927E+06)};
                         __attribute__((section(".rodata")))
			 __attribute__((aligned(16))) static __m128 p4[8] = {_mm_set1_ps(1.474502166059939948905062E+04), 
                                                             _mm_set1_ps(2.426813369486704502836312E+06), 
                                                             _mm_set1_ps(1.214755574045093227939592E+08), 
                                                             _mm_set1_ps(2.663432449630976949898078E+09), 
                                                             _mm_set1_ps(2.940378956634553899906876E+10), 
                                                             _mm_set1_ps(1.702665737765398868392998E+11), 
                                                             _mm_set1_ps(4.926125793377430887588120E+11), 
                                                             _mm_set1_ps(5.606251856223951465078242E+11)};
                         __attribute__((section(".rodata")))
                         __attribute__((aligned(16))) static __m128 q1[8] = {_mm_set1_ps(6.748212550303777196073036E+01), 
                                                             _mm_set1_ps(1.113332393857199323513008E+03), 
                                                             _mm_set1_ps(7.738757056935398733233834E+03), 
                                                             _mm_set1_ps(2.763987074403340708898585E+04), 
                                                             _mm_set1_ps(5.499310206226157329794414E+04), 
                                                             _mm_set1_ps(6.161122180066002127833352E+04), 
                                                             _mm_set1_ps(3.635127591501940507276287E+04), 
                                                             _mm_set1_ps(8.785536302431013170870835E+03)};
                         __attribute__((section(".rodata")))
			 __attribute__((aligned(16))) static __m128 q2[8] = {_mm_set1_ps(1.830328399370592604055942E+02),
                                                             _mm_set1_ps(7.765049321445005871323047E+03), 
                                                             _mm_set1_ps(1.331903827966074194402448E+05),
                                                             _mm_set1_ps(1.136705821321969608938755E+06), 
                                                             _mm_set1_ps(5.267964117437946917577538E+06), 
                                                             _mm_set1_ps(1.346701454311101692290052E+07), 
                                                             _mm_set1_ps(1.782736530353274213975932E+07), 
                                                             _mm_set1_ps(9.533095591844353613395747E+06)};
                         __attribute__((section(".rodata")))
			 __attribute__((aligned(16))) static __m128 q4[8] = {_mm_set1_ps(2.690530175870899333379843E+03), 
                                                             _mm_set1_ps(6.393885654300092398984238E+05), 
                                                             _mm_set1_ps(4.135599930241388052042842E+07), 
                                                             _mm_set1_ps(1.120872109616147941376570E+09), 
                                                             _mm_set1_ps(1.488613728678813811542398E+10), 
                                                             _mm_set1_ps(1.016803586272438228077304E+11), 
                                                             _mm_set1_ps(3.417476345507377132798597E+11), 
                                                             _mm_set1_ps(4.463158187419713286462081E+11)};
			    const __m128 d1     = _mm_set1_ps(-5.772156649015328605195174E-01);
			    const __m128 d2     = _mm_set1_ps(4.227843350984671393993777E-01);
                            const __m128 d4     = _mm_set1_ps(1.791759469228055000094023E+00);
                            const __m128 frtbig = _mm_set1_ps(1.42E+09);
                            const __m128 pnt68  = _mm_set1_ps(0.6796875E+00);
			    const __m128 sqrtpi = _mm_set1_ps(0.9189385332046727417803297E+00);
			    const __m128 xbig   = _mm_set1_ps(4.08E+36);
			    const __m128 _0     = _mm_setzero_ps();
			    const __m128 _1_2   = _mm_set1_ps(0.5);
			    const __m128 _1_5   = _mm_set1_ps(1.5);
			    const __m128 _1     = _mm_set1_ps(1.0);
			    const __m128 _4     = _mm_set1_ps(4.0);
			    const __m128 _2     = _mm_set1_ps(2.0);
			    const __m128 _12    = _mm_set1_ps(12.0);
			    const __m128 huge   = _mm_set1_ps(FLT_MAX);
			    const __m128 eps    = _mm_set1_ps(FLT_EPSILON);
			    __m128 gamlog,res,xden;
			    __m128 xm1,xm2,xm4;
			    __m128 xnum,xsq,corr;
			    gamlog = _mm_setzero_ps();
			   
			    if(_mm_cmp_ps_mask(x,eps,_CMP_LE_OQ)) {
                               res = negate_xmm4r4(_mm_log_ps(x));
			    }
			    else if(_mm_cmp_ps_mask(x,_1_5,_CMP_LE_OQ)) {
                               const __mmask8 m0 = _mm_cmp_ps_mask(x,pnt68,_CMP_LT_OQ);
			       corr = _mm_mask_blend_ps(m0,_0,negate_xmm4r4(_mm_log_ps(x)));
			       xm1  = _mm_mask_blend_ps(m0,_mm_sub_ps(
			                                                _mm_sub_ps(x,_1_2),_1_2));

			       if(_mm_cmp_ps_mask(x,_1_2,_CMP_LE_OQ) ||
			          _mm_cmp_ps_mask(pnt68,x,_CMP_LE_OQ)) {
                                   xden = _1;
				   xnum = _0;
				   xnum = _mm_fmadd_ps(xnum,xm1,p1[0]);
				   xden = _mm_fmadd_ps(xden,xm1,q1[0]);
				   xnum = _mm_fmadd_ps(xnum,xm1,p1[1]);
				   xden = _mm_fmadd_ps(xden,xm1,q1[1]);
				   xnum = _mm_fmadd_ps(xnum,xm1,p1[2]);
				   xden = _mm_fmadd_ps(xden,xm1,q1[2]);
				   xnum = _mm_fmadd_ps(xnum,xm1,p1[3]);
				   xden = _mm_fmadd_ps(xden,xm1,q1[3]);
				   xnum = _mm_fmadd_ps(xnum,xm1,p1[4]);
				   xden = _mm_fmadd_ps(xden,xm1,q1[4]);
				   xnum = _mm_fmadd_ps(xnum,xm1,p1[5]);
				   xden = _mm_fmadd_ps(xden,xm1,q1[5]);
				   xnum = _mm_fmadd_ps(xnum,xm1,p1[6]);
				   xden = _mm_fmadd_ps(xden,xm1,q1[6]);
				   xnum = _mm_fmadd_ps(xnum,xm1,p1[7]);
				   xden = _mm_fmadd_ps(xden,xm1,q1[7]);
				   const __m128 t0 = _mm_fmadd_ps(xm1,
				                                  _mm_div_ps(xnum,xden),d1);
				   res  = _mm_add_ps(corr,
				                    _mm_mul_ps(xm1,t0));
				}
				else {

                                   xm2  = _mm_sub_ps(_mm_sub_ps(x,_1_2),_1_2);
				   xden = _1;
				   xnum = _0;
				   xnum = _mm_fmadd_ps(xnum,xm2,p2[0]);
				   xden = _mm_fmadd_ps(xden,xm2,q2[0]);
				   xnum = _mm_fmadd_ps(xnum,xm2,p2[1]);
				   xden = _mm_fmadd_ps(xden,xm2,q2[1]);
				   xnum = _mm_fmadd_ps(xnum,xm2,p2[2]);
				   xden = _mm_fmadd_ps(xden,xm2,q2[2]);
				   xnum = _mm_fmadd_ps(xnum,xm2,p2[3]);
				   xden = _mm_fmadd_ps(xden,xm2,q2[3]);
				   xnum = _mm_fmadd_ps(xnum,xm2,p2[4]);
				   xden = _mm_fmadd_ps(xden,xm2,q2[4]);
				   xnum = _mm_fmadd_ps(xnum,xm2,p2[5]);
				   xden = _mm_fmadd_ps(xden,xm2,q2[5]);
				   xnum = _mm_fmadd_ps(xnum,xm2,p2[6]);
				   xden = _mm_fmadd_ps(xden,xm2,q2[6]);
				   xnum = _mm_fmadd_ps(xnum,xm2,p2[7]);
				   xden = _mm_fmadd_ps(xden,xm2,q2[7]);
				   const __m128 t0 = _mm_fmadd_ps(xm2,
				                                  _mm_div_ps(xnum,xden),d2);
				   res  = _mm_add_ps(corr,
				                    _mm_mul_ps(xm2,t0));
				}
			    }
			    else if(_mm_cmp_ps_mask(x,_4,_CMP_LE_OQ)) {
                                   xm2  = _mm_sub_ps(x,_2);
				   xden = _1;
				   xnum = _0;
				   xnum = _mm_fmadd_ps(xnum,xm2,p2[0]);
				   xden = _mm_fmadd_ps(xden,xm2,q2[0]);
				   xnum = _mm_fmadd_ps(xnum,xm2,p2[1]);
				   xden = _mm_fmadd_ps(xden,xm2,q2[1]);
				   xnum = _mm_fmadd_ps(xnum,xm2,p2[2]);
				   xden = _mm_fmadd_ps(xden,xm2,q2[2]);
				   xnum = _mm_fmadd_ps(xnum,xm2,p2[3]);
				   xden = _mm_fmadd_ps(xden,xm2,q2[3]);
				   xnum = _mm_fmadd_ps(xnum,xm2,p2[4]);
				   xden = _mm_fmadd_ps(xden,xm2,q2[4]);
				   xnum = _mm_fmadd_ps(xnum,xm2,p2[5]);
				   xden = _mm_fmadd_ps(xden,xm2,q2[5]);
				   xnum = _mm_fmadd_ps(xnum,xm2,p2[6]);
				   xden = _mm_fmadd_ps(xden,xm2,q2[6]);
				   xnum = _mm_fmadd_ps(xnum,xm2,p2[7]);
				   xden = _mm_fmadd_ps(xden,xm2,q2[7]);
				   res  = _mm_mul_ps(xm2,
				                    _mm_fmadd_ps(xm2,
						                _mm_div_ps(xnum,xden),d2));
			    }
			    else if(_mm_cmp_ps_mask(x,_12,_CMP_LE_OQ)) {
                                   xm4  = _mm_sub_ps(x,_4);
				   xden = xmm4r4_negate(_1);
				   xnum = _0;
				   xnum = _mm_fmadd_ps(xnum,xm4,p4[0]);
				   xden = _mm_fmadd_ps(xden,xm4,q4[0]);
				   xnum = _mm_fmadd_ps(xnum,xm4,p4[1]);
				   xden = _mm_fmadd_ps(xden,xm4,q4[1]);
				   xnum = _mm_fmadd_ps(xnum,xm4,p4[2]);
				   xden = _mm_fmadd_ps(xden,xm4,q4[2]);
				   xnum = _mm_fmadd_ps(xnum,xm4,p4[3]);
				   xden = _mm_fmadd_ps(xden,xm4,q4[3]);
				   xnum = _mm_fmadd_ps(xnum,xm4,p4[4]);
				   xden = _mm_fmadd_ps(xden,xm4,q4[4]);
				   xnum = _mm_fmadd_ps(xnum,xm4,p4[5]);
				   xden = _mm_fmadd_ps(xden,xm4,q4[5]);
				   xnum = _mm_fmadd_ps(xnum,xm4,p4[6]);
				   xden = _mm_fmadd_ps(xden,xm4,q4[6]);
				   xnum = _mm_fmadd_ps(xnum,xm4,p4[7]);
				   xden = _mm_fmadd_ps(xden,xm4,q4[7]);
				   res  = _mm_fmadd_ps(xm4,_mm_div_ps(xnum,xden),d4);
			    }
			    else {
                                   res  = _0;
				   if(_mm_cmp_ps_mask(x,frtbig,_CMP_LE_OQ)) {
                                      res = c[6];
				      xsq = _mm_mul_ps(x,x);
				      res = _mm_add_ps(_mm_div_ps(res,xsq),c[0]);
				      res = _mm_add_ps(_mm_div_ps(res,xsq),c[1]);
				      res = _mm_add_ps(_mm_div_ps(res,xsq),c[2]);
				      res = _mm_add_ps(_mm_div_ps(res,xsq),c[3]);
				      res = _mm_add_ps(_mm_div_ps(res,xsq),c[4]);
				      res = _mm_add_ps(_mm_div_ps(res,xsq),c[5]);
				   }
                                   res  = _mm_div_ps(res,x);
				   corr = _mm_log_ps(x);
				   res  = _mm_sub_ps(_mm_add_ps(res,sqrtpi),
				                        _mm_mul_ps(_1_2,corr));
				   res  = _mm_fmadd_ps(x,_mm_sub_ps(corr,_1),res);
				   
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


                      __m128d  
                      gamma_incomplete_xmm2r8(const __m128d p,
                                              const __m128d x) {
                                              
                            const __m128d exp_arg_min = _mm_set1_pd(-88.0e+00);
                            const __m128d overflow    = _mm_set1_pd(1.0e+37);
                            const __m128d plimit      = _mm_set1_pd(1000.0e+00);
                            const __m128d tol         = _mm_set1_pd(1.0e-7);
                            const __m128d xbig        = _mm_set1_pd(1.0e+8);
                            const __m128d C0          = _mm_setzero_pd();
                            const __m128d C0333333333 = _mm_set1_pd(0.3333333333333333333333);
                            const __m128d C1          = _mm_set1_pd(1.0);
                            const __m128d C2          = _mm_set1_pd(2.0);
                            const __m128d C3          = _mm_set1_pd(3.0);
                            const __m128d C9          = _mm_set1_pd(9.0);
                            __m128d cdf,arg,b,c;
                            __m128d pn1,pn2,pn3,pn4;
                            __m128d pn5,pn6,rn,t0,t1;
                            __m128d gaminc;
                            __mmask8 m0,m1;
                            m0 = _mm_cmp_pd_mask(plimit,p,_CMP_LT_OQ);
                            if(m0) {
                               __m128d sqrp,xp,_9p1,t0,t1;
                               xp     = _mm_div_pd(x,p);
                               _9p1   = _mm_fmsub_pd(C9,p,C1);
                               sqrp   = _mm_mul_pd(C3,_mm_sqrt_pd(p));
                               t0     = _mm_pow_pd(xp,C0333333333);
                               t1     = _mm_add_pd(t0,
                                            _mm_div_pd(C1,_9p1));
                               pn1    = _mm_mul_pd(sqrp,t1);
                               gaminc = normal_01_cdf_xmm2r8(pn1);
                               return (gaminc);
                            }   
                            m0 = _mm_cmp_pd_mask(x,C1,_CMP_LE_OQ);
                            m1 = _mm_cmp_pd_mask(x,p,_CMP_LT_OQ);
                            if(m0 || m1) {

                               t0  = _mm_log_pd(x);
                          
                               t1  = gamma_log_xmm2r8(_mm_add_pd(p,C1));
                               arg = _mm_fmsub_pd(p,t0,_mm_sub_pd(x,t1));
                               c   = C1;
                               gaminc = C1;
                               a   = p; 
                               while(true) {
                                    a      = _mm_add_pd(a,C1);
                                    c      = _mm_mul_pd(c,_mm_div_pd(x,a));
                                    gaminc = _mm_add_pd(gaminc,c);
                                    m0     = _mm_cmp_pd_mask(c,tol,_CMP_LE_OQ);
                                    if(m0) break;
                               }

                               t0  = _mm_log_pd(x);
                               arg = _mm_add_pd(arg,t0);  
                               m1  = _mm_cmp_pd_mask(exp_arg_min,arg,_CMP_LE_OQ);
                               gaminc = _mm_mask_blend_pd(m1,C0,_mm_exp_pd(arg));  
                                
                           } 
                           else {

                               t0  = _mm_log_pd(x);
                             
                               t1  = gamma_log_xmm2r8(p);
                               arg = _mm_fmsub_pd(p,t0,_mm_sub_pd(x,t1));                               
                               a   = _mm_sub_pd(C1,p);
                               b   = _mm_add_pd(a,_mm_add_pd(x,C1));
                               c   = C0;
                               pn1 = C1;
                               pn2 = x;
                               pn3 = _mm_add_pd(x,C1);
                               pn4 = _mm_mul_pd(x,b);
                               gaminc = _mm_div_pd(pn3,pn4);
                               while(true) {
                                   a = _mm_add_pd(a,C1);
                                   b = _mm_add_pd(b,C2);
                                   c = _mm_add_pd(c,C1);
                                   pn5 = _mm_fmsub_pd(b,pn3,
                                                     _mm_mul_pd(a,
                                                           _mm_mul_pd(c,pn1)));
                                   pn6 = _mm_fmsub_pd(b,pn4,
                                                     _mm_mul_pd(a,
                                                           _mm_mul_pd(c,pn2)));
                                   if(_mm_cmp_pd_mask(C0,_mm_abs_pd(pn6),
                                                                       _CMP_LT_OQ)) {
                                        rn = _mm_div_pd(pn5,pn6);
                                        t0 = _mm_abs_pd(_mm_sub_pd(gaminc,rn));
                                        t1 = _mm_min_pd(tol,_mm_mul_pd(tol,rn));
                                        if(_mm_cmp_pd_mask(t0,t1,_CMP_LE_OQ)) {
                                           arg  = _mm_add_pd(_mm_log_pd(gaminc));       
                                           m1   = _mm_cmp_pd_mask(exp_arg_min,arg,_CMP_LE_OQ);
                                           gaminc = _mm_mask_blend_pd(m1,C1,_mm_sub_pd(C1,
                                                                                    _mm_exp_pd(arg)));
      
                                           return (gaminc);                               
                                        }    
                                        gaminc = rn;                               
                                   }
                                   pn1 = pn3;
                                   pn2 = pn4;
                                   pn3 = pn5;
                                   pn4 = pn6;
                                   if(_mm_cmp_pd_mask(overflow,
                                                   _mm_abs_pd(pn5),_CMP_LE_OQ)) {
                                      t0 = _mm_div_pd(C1,overflow);
                                      pn1= _mm_mul_pd(pn1,t0);
                                      pn2= _mm_mul_pd(pn2,t0);
                                      pn3= _mm_mul_pd(pn3,t0);
                                      pn4= _mm_mul_pd(pn4,t0);               
                                   }
                               }
                           } 
                           
                           return (gaminc);                   
                   }	
                   
                   
                 
                      __m128  
                      gamma_incomplete_xmm4r4(const __m128 p,
                                              const __m128 x) {
                                              
                            const __m128 exp_arg_min = _mm_set1_ps(-88.0e+00);
                            const __m128 overflow    = _mm_set1_ps(FLT_MAX);
                            const __m128 plimit      = _mm_set1_ps(1000.0e+00);
                            const __m128 tol         = _mm_set1_ps(1.0e-7f);
                            const __m128 xbig        = _mm_set1_ps(1.0e+8f);
                            const __m128 C0          = _mm_setzero_ps();
                            const __m128 C0333333333 = _mm_set1_ps(0.3333333333333333333333f);
                            const __m128 C1          = _mm_set1_ps(1.0);
                            const __m128 C2          = _mm_set1_ps(2.0);
                            const __m128 C3          = _mm_set1_ps(3.0);
                            const __m128 C9          = _mm_set1_ps(9.0);
                            __m128 cdf,arg,b,c;
                            __m128 pn1,pn2,pn3,pn4;
                            __m128 pn5,pn6,rn,t0,t1;
                            __m128 gaminc;
                            __mmask8 m0,m1;
                            m0 = _mm_cmp_ps_mask(plimit,p,_CMP_LT_OQ);
                            if(m0) {
                               __m128 sqrp,xp,_9p1,t0,t1;
                               xp     = _mm_div_ps(x,p);
                               _9p1   = _mm_fmsub_ps(C9,p,C1);
                               sqrp   = _mm_mul_ps(C3,_mm_sqrt_pd(p));
                               t0     = _mm_pow_ps(xp,C0333333333);
                               t1     = _mm_add_ps(t0,
                                            _mm_div_ps(C1,_9p1));
                               pn1    = _mm_mul_ps(sqrp,t1);
                               gaminc = normal_01_cdf_xmm4r4(pn1);
                               return (gaminc);
                            }   
                            m0 = _mm_cmp_ps_mask(x,C1,_CMP_LE_OQ);
                            m1 = _mm_cmp_ps_mask(x,p,_CMP_LT_OQ);
                            if(m0 || m1) {

                               t0  = _mm_log_ps(x);
                          
                               t1  = gamma_log_xmm4r4(_mm_add_ps(p,C1));
                               arg = _mm_fmsub_ps(p,t0,_mm_sub_ps(x,t1));
                               c   = C1;
                               gaminc = C1;
                               a   = p; 
                               while(true) {
                                    a      = _mm_add_ps(a,C1);
                                    c      = _mm_mul_ps(c,_mm_div_ps(x,a));
                                    gaminc = _mm_add_ps(gaminc,c);
                                    m0     = _mm_cmp_ps_mask(c,tol,_CMP_LE_OQ);
                                    if(m0) break;
                               }

                               t0  = _mm_log_ps(x);
                               arg = _mm_add_ps(arg,t0);  
                               m1  = _mm_cmp_ps_mask(exp_arg_min,arg,_CMP_LE_OQ);
                               gaminc = _mm_mask_blend_ps(m1,C0,_mm_exp_pd(arg));  
                                
                           } 
                           else {

                               t0  = _mm_log_ps(x);
                             
                               t1  = gamma_log_xmm4r4(p);
                               arg = _mm_fmsub_ps(p,t0,_mm_sub_pd(x,t1));                               
                               a   = _mm_sub_ps(C1,p);
                               b   = _mm_add_ps(a,_mm_add_pd(x,C1));
                               c   = C0;
                               pn1 = C1;
                               pn2 = x;
                               pn3 = _mm_add_ps(x,C1);
                               pn4 = _mm_mul_ps(x,b);
                               gaminc = _mm_div_ps(pn3,pn4);
                               while(true) {
                                   a = _mm_add_ps(a,C1);
                                   b = _mm_add_ps(b,C2);
                                   c = _mm_add_ps(c,C1);
                                   pn5 = _mm_fmsub_ps(b,pn3,
                                                     _mm_mul_ps(a,
                                                           _mm_mul_ps(c,pn1)));
                                   pn6 = _mm_fmsub_ps(b,pn4,
                                                     _mm_mul_ps(a,
                                                           _mm_mul_ps(c,pn2)));
                                   if(_mm_cmp_ps_mask(C0,_mm_abs_ps(pn6),
                                                                       _CMP_LT_OQ)) {
                                        rn = _mm_div_ps(pn5,pn6);
                                        t0 = _mm_abs_ps(_mm_sub_ps(gaminc,rn));
                                        t1 = _mm_min_ps(tol,_mm_mul_ps(tol,rn));
                                        if(_mm_cmp_ps_mask(t0,t1,_CMP_LE_OQ)) {
                                           arg  = _mm_add_ps(_mm_log_pd(gaminc));       
                                           m1   = _mm_cmp_ps_mask(exp_arg_min,arg,_CMP_LE_OQ);
                                           gaminc = _mm_mask_blend_ps(m1,C1,_mm_sub_ps(C1,
                                                                                    _mm_exp_ps(arg)));
      
                                           return (gaminc);                               
                                        }    
                                        gaminc = rn;                               
                                   }
                                   pn1 = pn3;
                                   pn2 = pn4;
                                   pn3 = pn5;
                                   pn4 = pn6;
                                   if(_mm_cmp_ps_mask(overflow,
                                                   _mm_abs_ps(pn5),_CMP_LE_OQ)) {
                                      t0 = _mm_div_ps(C1,overflow);
                                      pn1= _mm_mul_ps(pn1,t0);
                                      pn2= _mm_mul_ps(pn2,t0);
                                      pn3= _mm_mul_ps(pn3,t0);
                                      pn4= _mm_mul_ps(pn4,t0);               
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


                      __m128d  
		      vpoly_eval_xmm2r8(const int32_t n,
		                        const __m128d * __restrict a,
		                        const __m128d x) {
		         
		         register __m128d vpoly;
		         int i;
		         vpoly = _mm_load_pd(&a[n]);
		         for(i=n; i != 0; --i) {
		             register __m128d t0 = a[i];
		             vpoly = _mm_fmadd_pd(vpoly,x,t0);   
		         }  
		         return (vpoly);              
		    }
		    
		    
		    
		      
		      __m128  
		      vpoly_eval_xmm4r4(const int32_t n,
		                        const __m128 * __restrict a,
		                        const __m128 x) {
		         
		         register __m128 vpoly;
		         int32_t i;
		         vpoly = _mm_load_ps(&a[n]);
		         for(i=n; i != 0; --i) {
		             register __m128 t0 = a[i];
		             vpoly = _mm_fmadd_ps(vpoly,x,t0);   
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


                      __m128d    
		      normal_01_cdf_inv_xmm2r8(const __m128d p) {
		            __attribute__((section(".rodata")))
		            __attribute__((aligned(16))) static __m128d  a[8] = {
		                     _mm_set1_pd(3.3871328727963666080e+00),
                                     _mm_set1_pd(1.3314166789178437745e+02),
                                     _mm_set1_pd(1.9715909503065514427e+03),
                                     _mm_set1_pd(1.3731693765509461125e+04),
                                     _mm_set1_pd(4.5921953931549871457e+04),
                                     _mm_set1_pd(6.7265770927008700853e+04),
                                     _mm_set1_pd(3.3430575583588128105e+04),
                                     _mm_set1_pd(2.5090809287301226727e+03)};   
                            __attribute__((section(".rodata")))  
		            __attribute__((aligned(16))) static __m128d   b[8] = {
		                      _mm_set1_pd(1.0e+00),
                                      _mm_set1_pd(4.2313330701600911252e+01),
                                      _mm_set1_pd(6.8718700749205790830e+02),
                                      _mm_set1_pd(5.3941960214247511077e+03),
                                      _mm_set1_pd(2.1213794301586595867e+04),
                                      _mm_set1_pd(3.9307895800092710610e+04),
                                      _mm_set1_pd(2.8729085735721942674e+04),
                                      _mm_set1_pd(5.2264952788528545610e+03)}; 
                            __attribute__((section(".rodata")))
		            __attribute__((aligned(16))) static __m128d   c[8] = {
		                      _mm_set1_pd(1.42343711074968357734e+00),
                                      _mm_set1_pd(4.63033784615654529590e+00),
                                      _mm_set1_pd(5.76949722146069140550e+00),
                                      _mm_set1_pd(3.64784832476320460504e+00),
                                      _mm_set1_pd(1.27045825245236838258e+00),
                                      _mm_set1_pd(2.41780725177450611770e-01),
                                      _mm_set1_pd(2.27238449892691845833e-02),
                                      _mm_set1_pd(7.74545014278341407640e-04)};
                           __attribute__((section(".rodata")))
                           __attribute__((aligned(16))) static __m128d   d[8] = {
                                      _mm_set1_pd(1.0e+00),
                                      _mm_set1_pd(2.05319162663775882187e+00),
                                      _mm_set1_pd(1.67638483018380384940e+00),
                                      _mm_set1_pd(6.89767334985100004550e-01),
                                      _mm_set1_pd(1.48103976427480074590e-01),
                                      _mm_set1_pd(1.51986665636164571966e-02),
                                      _mm_set1_pd(5.47593808499534494600e-04),
                                      _mm_set1_pd(1.05075007164441684324e-09)};
                           __attribute__((section(".rodata")))
                           __attribute__((aligned(16))) static __m128d   e[8] = {
                                      _mm_set1_pd(6.65790464350110377720e+00),
                                      _mm_set1_pd(5.46378491116411436990e+00),
                                      _mm_set1_pd(1.78482653991729133580e+00),
                                      _mm_set1_pd(2.96560571828504891230e-01),
                                      _mm_set1_pd(2.65321895265761230930e-02),
                                      _mm_set1_pd(1.24266094738807843860e-03),
                                      _mm_set1_pd(2.71155556874348757815e-05),
                                      _mm_set1_pd(2.01033439929228813265e-07)};
                           __attribute__((section(".rodata")))
                           __attribute__((aligned(16))) static __m128d   f[8] = {
                                      _mm_set1_pd(1.0e+00),
                                      _mm_set1_pd(5.99832206555887937690e-01),
                                      _mm_set1_pd(1.36929880922735805310e-01),
                                      _mm_set1_pd(1.48753612908506148525e-02),
                                      _mm_set1_pd(7.86869131145613259100e-04), 
                                      _mm_set1_pd(1.84631831751005468180e-05),
                                      _mm_set1_pd(1.42151175831644588870e-07),
                                      _mm_set1_pd(2.04426310338993978564e-15)};
                          __m128d const1 = _mm_set1_pd(0.180625e+00);
                          __m128d const2 = _mm_set1_pd(1.6e+00);
                          __m128d split1 = _mm_set1_pd(0.425e+00);
                          __m128d split2 = _mm_set1_pd(5.0e+00);
                          __m128d C0     = _mm_setzero_pd();
                          __m128d C1     = _mm_set1_pd(1.0);
                          __m128d C05    = _mm_set1_pd(0.5);
                          register __m128d q,r,t0,t1;
                          register __m128d x;
                          q = _mm_sub_pd(p,C05);
                          if(_mm_cmp_pd_mask(q,split1,_CMP_LE_OQ)) {
                             r = _mm_sub_pd(const1,_mm_mul_pd(q,q));
                             t0= vpoly_eval_xmm2r8(8,a,r);
                             t1= vpoly_eval_xmm2r8(8,b,r);
                             x = _mm_div_pd(_mm_mul_pd(q,t0),t1);
                          } 
                          else {
                             const __mmask8 m = _mm_cmp_pd_mask(q,C0,_CMP_LT_OQ);
                             r                = _mm_mask_blend_pd(m,_mm_sub_pd(C1,p),p);
                             if(_mm_cmp_pd_mask(r,C0,_CMP_LE_OQ)) {
                                x = _mm_set1_pd(DBL_MAX);
                             }
                             else {

                                r = _mm_sqrt_pd(negate_xmm2r8(_mm_log_pd(r)));
                        
                                const __mmask8 m = _mm_cmp_pd_mask(r,split2,_CMP_LE_OQ);
                                r                = _mm_mask_blend_pd(m,_mm_sub_pd(r,split2),
                                                                          _mm_sub_pd(r,const2));
                                t0               = _mm_div_pd(vpoly_eval_xmm2r8(8,c,r),
                                                                 vpoly_eval_xmm2r8(8,d,r));
                                t1               = _mm_div_pd(vpoly_eval_xmm2r8(8,e,r),
                                                                 vpoly_eval_xmm2r8(8,f,r));
                                x                = _mm_mask_blend_pd(m,t1,t0);      
                             }
                             if(_mm_cmp_pd_mask(q,C0,_CMP_LT_OQ)) x = negate_xmm2r8(x);
                          }
                          return (x);
                          
		    }
		    
		    
		      __m128    
		      normal_01_cdf_inv_xmm4r4(const __m128 p) {
		            __attribute__((section(".rodata")))
		            __attribute__((aligned(16))) static __m128  a[8] = {
		                     _mm_set1_ps(3.3871328727963666080e+00f),
                                     _mm_set1_ps(1.3314166789178437745e+02f),
                                     _mm_set1_ps(1.9715909503065514427e+03f),
                                     _mm_set1_ps(1.3731693765509461125e+04f),
                                     _mm_set1_ps(4.5921953931549871457e+04f),
                                     _mm_set1_ps(6.7265770927008700853e+04f),
                                     _mm_set1_ps(3.3430575583588128105e+04f),
                                     _mm_set1_ps(2.5090809287301226727e+03f)};   
                            __attribute__((section(".rodata")))  
		            __attribute__((aligned(16))) static __m128   b[8] = {
		                      _mm_set1_ps(1.0e+00),
                                      _mm_set1_ps(4.2313330701600911252e+01f),
                                      _mm_set1_ps(6.8718700749205790830e+02f),
                                      _mm_set1_ps(5.3941960214247511077e+03f),
                                      _mm_set1_ps(2.1213794301586595867e+04f),
                                      _mm_set1_ps(3.9307895800092710610e+04f),
                                      _mm_set1_ps(2.8729085735721942674e+04f),
                                      _mm_set1_ps(5.2264952788528545610e+03f)}; 
                            __attribute__((section(".rodata")))
		            __attribute__((aligned(16))) static __m128   c[8] = {
		                      _mm_set1_ps(1.42343711074968357734e+00f),
                                      _mm_set1_ps(4.63033784615654529590e+00f),
                                      _mm_set1_ps(5.76949722146069140550e+00f),
                                      _mm_set1_ps(3.64784832476320460504e+00f),
                                      _mm_set1_ps(1.27045825245236838258e+00f),
                                      _mm_set1_ps(2.41780725177450611770e-01f),
                                      _mm_set1_ps(2.27238449892691845833e-02f),
                                      _mm_set1_ps(7.74545014278341407640e-04f)};
                           __attribute__((section(".rodata")))
                           __attribute__((aligned(16))) static __m128   d[8] = {
                                      _mm_set1_ps(1.0e+00),
                                      _mm_set1_ps(2.05319162663775882187e+00f),
                                      _mm_set1_ps(1.67638483018380384940e+00f),
                                      _mm_set1_ps(6.89767334985100004550e-01f),
                                      _mm_set1_ps(1.48103976427480074590e-01f),
                                      _mm_set1_ps(1.51986665636164571966e-02f),
                                      _mm_set1_ps(5.47593808499534494600e-04f),
                                      _mm_set1_ps(1.05075007164441684324e-09f)};
                           __attribute__((section(".rodata")))
                           __attribute__((aligned(16))) static __m128   e[8] = {
                                      _mm_set1_ps(6.65790464350110377720e+00f),
                                      _mm_set1_ps(5.46378491116411436990e+00f),
                                      _mm_set1_ps(1.78482653991729133580e+00f),
                                      _mm_set1_ps(2.96560571828504891230e-01f),
                                      _mm_set1_ps(2.65321895265761230930e-02f),
                                      _mm_set1_ps(1.24266094738807843860e-03f),
                                      _mm_set1_ps(2.71155556874348757815e-05f),
                                      _mm_set1_ps(2.01033439929228813265e-07f)};
                           __attribute__((section(".rodata")))
                           __attribute__((aligned(16))) static __m128   f[8] = {
                                      _mm_set1_ps(1.0e+00),
                                      _mm_set1_ps(5.99832206555887937690e-01f),
                                      _mm_set1_ps(1.36929880922735805310e-01f),
                                      _mm_set1_ps(1.48753612908506148525e-02f),
                                      _mm_set1_ps(7.86869131145613259100e-04f), 
                                      _mm_set1_ps(1.84631831751005468180e-05f),
                                      _mm_set1_ps(1.42151175831644588870e-07f),
                                      _mm_set1_ps(2.04426310338993978564e-15f)};
                          __m128 const1 = _mm_set1_ps(0.180625e+00f);
                          __m128 const2 = _mm_set1_ps(1.6e+00f);
                          __m128 split1 = _mm_set1_ps(0.425e+00f);
                          __m128 split2 = _mm_set1_ps(5.0e+00f);
                          __m128 C0     = _mm_setzero_ps();
                          __m128 C1     = _mm_set1_ps(1.0f);
                          __m128 C05    = _mm_set1_ps(0.5f);
                          register __m128 q,r,t0,t1;
                          register __m128 x;
                          q = _mm_sub_ps(p,C05);
                          if(_mm_cmp_ps_mask(q,split1,_CMP_LE_OQ)) {
                             r = _mm_sub_ps(const1,_mm_mul_ps(q,q));
                             t0= vpoly_eval_xmm4r4(8,a,r);
                             t1= vpoly_eval_xmm4r4(8,b,r);
                             x = _mm_div_ps(_mm_mul_ps(q,t0),t1);
                          } 
                          else {
                             const __mmask8 m = _mm_cmp_ps_mask(q,C0,_CMP_LT_OQ);
                             r                = _mm_mask_blend_ps(m,_mm_sub_ps(C1,p),p);
                             if(_mm_cmp_ps_mask(r,C0,_CMP_LE_OQ)) {
                                x = _mm_set1_pd(FLT_MAX);
                             }
                             else {

                                r = _mm_sqrt_ps(negate_xmm4r4(_mm_log_ps(r)));
                       
                                const __mmask8 m = _mm_cmp_ps_mask(r,split2,_CMP_LE_OQ);
                                r                = _mm_mask_blend_ps(m,_mm_sub_ps(r,split2),
                                                                          _mm_sub_ps(r,const2));
                                t0               = _mm_div_ps(vpoly_eval_xmm4r4(8,c,r),
                                                                 vpoly_eval_xmm4r4(8,d,r));
                                t1               = _mm_div_ps(vpoly_eval_xmm4r4(8,e,r),
                                                                 vpoly_eval_xmm4r4(8,f,r));
                                x                = _mm_mask_blend_ps(m,t1,t0);      
                             }
                             if(_mm_cmp_ps_mask(q,C0,_CMP_LT_OQ)) x = negate_xmm4r4(x);
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


               
		    
		     
		      __m128d 
		      reciprocal_cdf_xmm2r8(const __m128d x,
		                            const __m128d a,
		                            const __m128d b) {
		          
		          register __m128d ax,ab,l1,l2;
		          register __m128d cdf;       
		          ax = _mm_div_pd(a,x);
                          l1 = _mm_log_pd(ax);
	                  ab = _mm_div_pd(a,b);
                          l2 = _mm_log_pd(ab);
                          cdf= _mm_div_pd(l1,l2);
                          return (cdf);
		     }
		     
		     
		      
		      __m128 
		      reciprocal_cdf_xmm4r4(const __m128 x,
		                             const __m128 a,
		                             const __m128 b) {
		          
		          register __m128 ax,ab,l1,l2;
		          register __m128 cdf;       
		          ax = _mm_div_ps(a,x);
                          l1 = _mm_log_ps(ax);
	                  ab = _mm_div_ps(a,b);
                          l2 = _mm_log_ps(ab);
                          cdf= _mm_div_ps(l1,l2);
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


                      
		      __m128d 		     
		      reciprocal_cdf_inv_xmm2r8(const __m128d cdf,
		                                const __m128d a,
		                                const __m128d b) {
		         
		           register __m128d C1 = _mm_set1_pd(1.0);
		           register __m128d pow1,pow2,cdf1;
		           register __m128d inv;
		           cdf1 = _mm_sub_pd(cdf,C1);
		           pow2 = _mm_pow_pd(b,cdf);
		           pow1 = _mm_pow_pd(a,cdf1);
		           inv  = _mm_div_pd(pow2,pow1);
		           return (inv);                          
		     }
		     
		     
		       
		      __m128		     
		      reciprocal_cdf_inv_xmm4r4(const __m128 cdf,
		                                const __m128 a,
		                                const __m128 b) {
		         
		           register __m128 C1 = _mm_set1_ps(1.0f);
		           register __m128 pow1,pow2,cdf1;
		           register __m128 inv;
		           cdf1 = _mm_sub_ps(cdf,C1);
		           pow2 = _mm_pow_ps(b,cdf);
		           pow1 = _mm_pow_ps(a,cdf1);
		           inv  = _mm_div_ps(pow2,pow1);
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
		         
	  
	             
		      __m128d 
		      reciprocal_mean_xmm2r8(const __m128d a,
		                             const __m128d b) {
		           
		           register __m128d ab,amb,l1;
		           register __m128d mean;
		           amb = _mm_sub_pd(a,b);
		           ab  = _mm_div_pd(a,b);
                           l1  = _mm_log_pd(ab);
	                   mean= _mm_div_pd(amb,l1);
                           return (mean);
		     }	 
		     
		     
		    
		      __m128 
		      reciprocal_mean_xmm4r4(const __m128 a,
		                             const __m128 b) {
		           
		           register __m128 ab,amb,l1;
		           register __m128 mean;
		           amb = _mm_sub_ps(a,b);
		           ab  = _mm_div_ps(a,b);
                           l1  = _mm_log_ps(ab);
		           mean= _mm_div_ps(amb,l1);
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


                     
		      __m128d
		      reciprocal_pdf_xmm2r8(    const __m128d x,
		                                const __m128d a,
		                                const __m128d b) {
		          
		          register __m128d C1 = _mm_set1_pd(1.0);
		          register __m128d ba,l1;
		          register __m128d pdf;
		          ba = _mm_div_pd(b,a);
                          l1 = _mm_mul_pd(x,_mm_log_pd(ba));
	                  pdf= _mm_div_pd(C1,l1);
                          return (pdf);                            
		    }
		    
		    
		    
		   
                    
		      reciprocal_pdf_xmm4r4(    const __m128 x,
		                                const __m128 a,
		                                const __m128 b) {
		          
		          register __m128 C1 = _mm_set1_ps(1.0f);
		          register __m128 ba,l1;
		          register __m128 pdf;
		          ba = _mm_div_ps(b,a);
                          l1 = _mm_mul_ps(x,_mm_log_ps(ba));
	                  pdf= _mm_div_ps(C1,l1);
                          return (pdf);                            
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


                      
		      __m128d  bessesl_i0_xmm2r8(const __m128d arg) {
                            __attribute__((section(".rodata")))
                            __attribute__((aligned(16))) static __m128d p[15] = {_mm_set1_pd(-5.2487866627945699800E-18),
                                                                      _mm_set1_pd(-1.5982226675653184646E-14), 
                                                                      _mm_set1_pd(-2.6843448573468483278E-11), 
                                                                      _mm_set1_pd(-3.0517226450451067446E-08), 
                                                                      _mm_set1_pd(-2.5172644670688975051E-05), 
                                                                      _mm_set1_pd(-1.5453977791786851041E-02), 
                                                                      _mm_set1_pd(-7.0935347449210549190E+00), 
                                                                      _mm_set1_pd(-2.4125195876041896775E+03), 
                                                                      _mm_set1_pd(-5.9545626019847898221E+05), 
                                                                      _mm_set1_pd(-1.0313066708737980747E+08), 
                                                                      _mm_set1_pd(-1.1912746104985237192E+10), 
                                                                      _mm_set1_pd(-8.4925101247114157499E+11), 
                                                                      _mm_set1_pd(-3.2940087627407749166E+13), 
                                                                      _mm_set1_pd(-5.5050369673018427753E+14), 
                                                                      _mm_set1_pd(-2.2335582639474375249E+15)};
                            __attribute__((section(".rodata")))
			    __attribute__((aligned(16))) static __m128d pp[8] = {_mm_set1_pd(-3.9843750000000000000E-01), 
                                                                      _mm_set1_pd(2.9205384596336793945E+00), 
                                                                      _mm_set1_pd(-2.4708469169133954315E+00), 
                                                                      _mm_set1_pd(4.7914889422856814203E-01), 
                                                                      _mm_set1_pd(-3.7384991926068969150E-03), 
                                                                      _mm_set1_pd(-2.6801520353328635310E-03), 
                                                                      _mm_set1_pd(9.9168777670983678974E-05), 
                                                                      _mm_set1_pd(-2.1877128189032726730E-06)};
                            __attribute__((section(".rodata")))
			    __attribute__((aligned(16))) static __m128d q[5]  = {_mm_set1_pd(-3.7277560179962773046E+03), 
                                                                      _mm_set1_pd(6.5158506418655165707E+06), 
                                                                      _mm_set1_pd(-6.5626560740833869295E+09), 
                                                                      _mm_set1_pd(3.7604188704092954661E+12), 
                                                                      _mm_set1_pd(-9.7087946179594019126E+14)};
                            __attribute__((section(".rodata")))
			    __attribute__((aligned(16))) static __m128d qq[7] = {_mm_set1_pd(-3.1446690275135491500E+01), 
                                                                      _mm_set1_pd(8.5539563258012929600E+01), 
                                                                      _mm_set1_pd(-6.0228002066743340583E+01), 
                                                                      _mm_set1_pd(1.3982595353892851542E+01), 
                                                                      _mm_set1_pd(-1.1151759188741312645E+00), 
                                                                      _mm_set1_pd(3.2547697594819615062E-02), 
                                                                      _mm_set1_pd(-5.5194330231005480228E-04)};
			    const __m128d rec15                    =  _mm_set1_pd(6.6666666666666666666E-02);
			    const __m128d xmax                     =  _mm_set1_pd(91.9E+00);
			    const __m128d exp40                    =  _mm_set1_pd(2.353852668370199854E+17);
			    const __m128d _1                       =  _mm_set1_pd(1.0);
			    const __m128d _15                      =  _mm_set1_pd(15.0);
			    const __m128d _225                     =  _mm_set1_pd(225.0);
			    const __m128d _40                      =  _mm_set1_pd(40.0);
			    const __m128d eps                      =  _mm_set1_pd(DBL_EPSILON);
			    const __m128d huge                     =  _mm_set1_pd(DBL_MAX);
			    __m128d value,a,b,bessel_i0;
			    __m128d sump,sumq,x,xx;
                            x = _mm_abs_pd(arg);
			    if(_mm_cmp_pd_mask(x,eps,_CMP_LT_OQ)) {
                               value = _1;
			    }
			    else if(_mm_cmp_pd_mask(x,_15,_CMP_LT_OQ)) {
                               xx   = _mm_mul_pd(x,x);
			       sump = p[0];
			       sump = _mm_fmadd_pd(sump,xx,p[1]);
			       sump = _mm_fmadd_pd(sump,xx,p[2]);
			       sump = _mm_fmadd_pd(sump,xx,p[3]);
			       sump = _mm_fmadd_pd(sump,xx,p[4]);
			       sump = _mm_fmadd_pd(sump,xx,p[5]);
			       sump = _mm_fmadd_pd(sump,xx,p[6]);
			       sump = _mm_fmadd_pd(sump,xx,p[7]);
			       sump = _mm_fmadd_pd(sump,xx,p[8]);
			       sump = _mm_fmadd_pd(sump,xx,p[9]);
			       sump = _mm_fmadd_pd(sump,xx,p[10]);
			       sump = _mm_fmadd_pd(sump,xx,p[11]);
			       sump = _mm_fmadd_pd(sump,xx,p[12]);
			       sump = _mm_fmadd_pd(sump,xx,p[13]);
			       sump = _mm_fmadd_pd(sump,xx,p[14]);
			       xx   = _mm_sub_pd(xx,_225);
			       const __m128d xxq0 = _mm_add_pd(xx,q[0]);
			       const __m128d xxq1 = _mm_add_pd(xx,q[1]);
			       const __m128d xxq2 = _mm_add_pd(xx,q[2]);
			       const __m128d xxq3 = _mm_add_pd(xx,q[3]);
			       const __m128d xxq4 = _mm_add_pd(xx,q[4]);
			       sumq = _mm_mul_pd(xxq0,
			                        _mm_mul_pd(xxq1,
						          _mm_mul_pd(xxq2,
							            _mm_mul_pd(xxq3,xxq4))));
			       value = _mm_div_pd(sump,sumq);
			                                         
			    }
			    else if(_mm_cmp_pd_mask(_15,x,_CMP_LE_OQ)) {
                                    if(_mm_cmp_pd_mask(xmax,x,_CMP_LT_OQ)) {
                                       value = huge;
				    }
				    else {
                                           xx = _mm_sub_pd(_mm_div_pd(_1,x),rec15);
					   const __m128d t0 = _mm_fmadd_pd(pp[0],xx,pp[1]);
					   const __m128d c0 = _mm_fmadd_pd(_mm_add_pd(xx,qq[0]),xx,qq[1]);
					   const __m128d t1 = _mm_fmadd_pd(t0,xx,pp[2]);
					   const __m128d c1 = _mm_fmadd_pd(c0,xx,qq[2]);
					   const __m128d t2 = _mm_fmadd_pd(t1,xx,pp[3]);
					   const __m128d c2 = _mm_fmadd_pd(c1,xx,qq[3]);
					   const __m128d t3 = _mm_fmadd_pd(t2,xx,pp[4]);
					   const __m128d c3 = _mm_fmadd_pd(c2,xx,qq[4]);
					   const __m128d t4 = _mm_fmadd_pd(t3,xx,pp[5]);
					   const __m128d c4 = _mm_fmadd_pd(c3,xx,qq[5]);
					   const __m128d t5 = _mm_fmadd_pd(t4,xx,pp[6]);
					   const __m128d c5 = _mm_fmadd_pd(c4,xx,qq[6]);
					   const __m128d t6 = _mm_fmadd_pd(t5,xx,pp[7]);
					   sump             = t6;
					   sumq             = c5;
					   value            = _mm_div_pd(sump,sumq);
					   const __mmask8 m = _mm_cmp_pd_mask(x,_mm_sub_pd(xmax,_15),_CMP_LE_OQ);
					   a                = _mm_mask_blend_pd(m,_mm_exp_pd(_mm_sub_pd(x,_40)),
					                                             _mm_exp_pd(x));
					   b                = _mm_mask_blend_pd(m,exp40,_1);
					   const __m128d tmp = _mm_sub_pd(_mm_mul_pd(value,a),
					                                    _mm_mul_pd(pp[0],a));
					   value            = _mm_mul_pd(_mm_div_pd(tmp,_mm_sqrt_pd(x)),b);
				    }
			    }
			   
			    bessel_i0 = value;
			    return (bessel_i0);
		    }
		    
		    
		     
		      __m128  bessesl_i0_xmm4r4(const __m128 arg) {
                            __attribute__((section(".rodata")))
                            __attribute__((aligned(16))) static __m128 p[15] = {_mm_set1_ps(-5.2487866627945699800E-18f),
                                                                      _mm_set1_ps(-1.5982226675653184646E-14f), 
                                                                      _mm_set1_ps(-2.6843448573468483278E-11f), 
                                                                      _mm_set1_ps(-3.0517226450451067446E-08f), 
                                                                      _mm_set1_ps(-2.5172644670688975051E-05f), 
                                                                      _mm_set1_ps(-1.5453977791786851041E-02f), 
                                                                      _mm_set1_ps(-7.0935347449210549190E+00f), 
                                                                      _mm_set1_ps(-2.4125195876041896775E+03f), 
                                                                      _mm_set1_ps(-5.9545626019847898221E+05f), 
                                                                      _mm_set1_ps(-1.0313066708737980747E+08f), 
                                                                      _mm_set1_ps(-1.1912746104985237192E+10f), 
                                                                      _mm_set1_ps(-8.4925101247114157499E+11f), 
                                                                      _mm_set1_ps(-3.2940087627407749166E+13f), 
                                                                      _mm_set1_ps(-5.5050369673018427753E+14f), 
                                                                      _mm_set1_ps(-2.2335582639474375249E+15f)};
                            __attribute__((section(".rodata")))
			    __attribute__((aligned(16))) static __m128 pp[8] = {_mm_set1_ps(-3.9843750000000000000E-01f), 
                                                                      _mm_set1_ps(2.9205384596336793945E+00f), 
                                                                      _mm_set1_ps(-2.4708469169133954315E+00f), 
                                                                      _mm_set1_ps(4.7914889422856814203E-01f), 
                                                                      _mm_set1_ps(-3.7384991926068969150E-03f), 
                                                                      _mm_set1_ps(-2.6801520353328635310E-03f), 
                                                                      _mm_set1_ps(9.9168777670983678974E-05f), 
                                                                      _mm_set1_ps(-2.1877128189032726730E-06f)};
                            __attribute__((section(".rodata")))
			    __attribute__((aligned(16))) static __m128 q[5]  = {_mm_set1_ps(-3.7277560179962773046E+03f), 
                                                                      _mm_set1_ps(6.5158506418655165707E+06f), 
                                                                      _mm_set1_ps(-6.5626560740833869295E+09f), 
                                                                      _mm_set1_ps(3.7604188704092954661E+12f), 
                                                                      _mm_set1_ps(-9.7087946179594019126E+14f)};
                            __attribute__((section(".rodata")))
			    __attribute__((aligned(16))) static __m128 qq[7] = {_mm_set1_ps(-3.1446690275135491500E+01f), 
                                                                      _mm_set1_ps(8.5539563258012929600E+01f), 
                                                                      _mm_set1_ps(-6.0228002066743340583E+01f), 
                                                                      _mm_set1_ps(1.3982595353892851542E+01f), 
                                                                      _mm_set1_ps(-1.1151759188741312645E+00f), 
                                                                      _mm_set1_ps(3.2547697594819615062E-02f), 
                                                                      _mm_set1_ps(-5.5194330231005480228E-04f)};
			    const __m128 rec15                    =  _mm_set1_ps(6.6666666666666666666E-02f);
			    const __m128 xmax                     =  _mm_set1_ps(91.9E+00f);
			    const __m128 exp40                    =  _mm_set1_ps(2.353852668370199854E+17f);
			    const __m128 _1                       =  _mm_set1_ps(1.0f);
			    const __m128 _15                      =  _mm_set1_ps(15.0f);
			    const __m128 _225                     =  _mm_set1_ps(225.0f);
			    const __m128 _40                      =  _mm_set1_ps(40.0f);
			    const __m128 eps                      =  _mm_set1_pd(FLT_EPSILON);
			    const __m128 huge                     =  _mm_set1_pd(FLT_MAX);
			    __m128 value,a,b,bessel_i0;
			    __m128 sump,sumq,x,xx;
                            x = _mm_abs_ps(arg);
			    if(_mm_cmp_ps_mask(x,eps,_CMP_LT_OQ)) {
                               value = _1;
			    }
			    else if(_mm_cmp_ps_mask(x,_15,_CMP_LT_OQ)) {
                               xx   = _mm_mul_ps(x,x);
			       sump = p[0];
			       sump = _mm_fmadd_ps(sump,xx,p[1]);
			       sump = _mm_fmadd_ps(sump,xx,p[2]);
			       sump = _mm_fmadd_ps(sump,xx,p[3]);
			       sump = _mm_fmadd_ps(sump,xx,p[4]);
			       sump = _mm_fmadd_ps(sump,xx,p[5]);
			       sump = _mm_fmadd_ps(sump,xx,p[6]);
			       sump = _mm_fmadd_ps(sump,xx,p[7]);
			       sump = _mm_fmadd_ps(sump,xx,p[8]);
			       sump = _mm_fmadd_ps(sump,xx,p[9]);
			       sump = _mm_fmadd_ps(sump,xx,p[10]);
			       sump = _mm_fmadd_ps(sump,xx,p[11]);
			       sump = _mm_fmadd_ps(sump,xx,p[12]);
			       sump = _mm_fmadd_ps(sump,xx,p[13]);
			       sump = _mm_fmadd_ps(sump,xx,p[14]);
			       xx   = _mm_sub_ps(xx,_225);
			       const __m128 xxq0 = _mm_add_ps(xx,q[0]);
			       const __m128 xxq1 = _mm_add_ps(xx,q[1]);
			       const __m128 xxq2 = _mm_add_ps(xx,q[2]);
			       const __m128 xxq3 = _mm_add_ps(xx,q[3]);
			       const __m128 xxq4 = _mm_add_ps(xx,q[4]);
			       sumq = _mm_mul_ps(xxq0,
			                        _mm_mul_ps(xxq1,
						          _mm_mul_ps(xxq2,
							            _mm_mul_ps(xxq3,xxq4))));
			       value = _mm_div_ps(sump,sumq);
			                                         
			    }
			    else if(_mm_cmp_ps_mask(_15,x,_CMP_LE_OQ)) {
                                    if(_mm_cmp_ps_mask(xmax,x,_CMP_LT_OQ)) {
                                       value = huge;
				    }
				    else {
                                           xx = _mm_sub_ps(_mm_div_ps(_1,x),rec15);
					   const __m128 t0 = _mm_fmadd_ps(pp[0],xx,pp[1]);
					   const __m128 c0 = _mm_fmadd_ps(_mm_add_ps(xx,qq[0]),xx,qq[1]);
					   const __m128 t1 = _mm_fmadd_ps(t0,xx,pp[2]);
					   const __m128 c1 = _mm_fmadd_ps(c0,xx,qq[2]);
					   const __m128 t2 = _mm_fmadd_ps(t1,xx,pp[3]);
					   const __m128 c2 = _mm_fmadd_ps(c1,xx,qq[3]);
					   const __m128 t3 = _mm_fmadd_ps(t2,xx,pp[4]);
					   const __m128 c3 = _mm_fmadd_ps(c2,xx,qq[4]);
					   const __m128 t4 = _mm_fmadd_ps(t3,xx,pp[5]);
					   const __m128 c4 = _mm_fmadd_ps(c3,xx,qq[5]);
					   const __m128 t5 = _mm_fmadd_ps(t4,xx,pp[6]);
					   const __m128 c5 = _mm_fmadd_ps(c4,xx,qq[6]);
					   const __m128 t6 = _mm_fmadd_ps(t5,xx,pp[7]);
					   sump             = t6;
					   sumq             = c5;
					   value            = _mm_div_ps(sump,sumq);
					   const __mmask8 m = _mm_cmp_ps_mask(x,_mm_sub_ps(xmax,_15),_CMP_LE_OQ);

					   a                = _mm_mask_blend_ps(m,_mm_exp_ps(_mm_sub_ps(x,_40)),
					                                             _mm_exp_ps(x));
   					   b                = _mm_mask_blend_ps(m,exp40,_1);
					   const __m128 tmp = _mm_sub_ps(_mm_mul_ps(value,a),
					                                    _mm_mul_ps(pp[0],a));
					   value            = _mm_mul_ps(_mm_div_ps(tmp,_mm_sqrt_ps(x)),b);
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


                      
		      __m128d bessel_i1_xmm2r8(const __m128d arg) {
                           __attribute__((section(".rodata")))
                           __attribute__((aligned(16))) static __m128d  p[15] = {_mm_set1_pd(-1.9705291802535139930E-19), 
                                                                      _mm_set1_pd(-6.5245515583151902910E-16), 
                                                                      _mm_set1_pd(-1.1928788903603238754E-12), 
                                                                      _mm_set1_pd(-1.4831904935994647675E-09), 
                                                                      _mm_set1_pd(-1.3466829827635152875E-06), 
                                                                      _mm_set1_pd(-9.1746443287817501309E-04), 
                                                                      _mm_set1_pd(-4.7207090827310162436E-01), 
                                                                      _mm_set1_pd(-1.8225946631657315931E+02), 
                                                                      _mm_set1_pd(-5.1894091982308017540E+04), 
                                                                      _mm_set1_pd(-1.0588550724769347106E+07), 
                                                                      _mm_set1_pd(-1.4828267606612366099E+09), 
                                                                      _mm_set1_pd(-1.3357437682275493024E+11), 
                                                                      _mm_set1_pd(-6.9876779648010090070E+12), 
                                                                      _mm_set1_pd(-1.7732037840791591320E+14), 
                                                                      _mm_set1_pd(-1.4577180278143463643E+15)};
                           __attribute__((section(".rodata")))
			   __attribute__((aligned(16))) static __m128d pp[8]  = {_mm_set1_pd(-6.0437159056137600000E-02), 
                                                                      _mm_set1_pd(4.5748122901933459000E-01), 
                                                                      _mm_set1_pd(-4.2843766903304806403E-01), 
                                                                      _mm_set1_pd(9.7356000150886612134E-02), 
                                                                      _mm_set1_pd(-3.2457723974465568321E-03), 
                                                                      _mm_set1_pd(-3.6395264712121795296E-04), 
                                                                      _mm_set1_pd(1.6258661867440836395E-05), 
                                                                      _mm_set1_pd(-3.6347578404608223492E-07)};
                           __attribute__((section(".rodata")))
			   __attribute__((aligned(16))) static __m128d q[5]   = {_mm_set1_pd(-4.0076864679904189921E+03), 
                                                                      _mm_set1_pd(7.4810580356655069138E+06), 
                                                                      _mm_set1_pd(-8.0059518998619764991E+09), 
                                                                      _mm_set1_pd(4.8544714258273622913E+12), 
                                                                      _mm_set1_pd(-1.3218168307321442305E+15)};
                           __attribute__((section(".rodata")))
			   __attribute__((aligned(16))) static __m128d qq[6]  = {_mm_set1_pd(-3.8806586721556593450E+00), 
                                                                      _mm_set1_pd(3.2593714889036996297E+00), 
                                                                      _mm_set1_pd(-8.5017476463217924408E-01), 
                                                                      _mm_set1_pd(7.4212010813186530069E-02), 
                                                                      _mm_set1_pd(-2.2835624489492512649E-03), 
                                                                      _mm_set1_pd(3.7510433111922824643E-05)};
			   const __m128d exp40                     =  _mm_set1_pd(2.353852668370199854E+17);
			   const __m128d _40                       =  _mm_set1_pd(40.0);
			   const __m128d _1_2                      =  _mm_set1_pd(0.5);
			   const __m128d _1                        =  _mm_set1_pd(1.0);
			   const __m128d _15                       =  _mm_set1_pd(15.0);
			   const __m128d pbar                      =  _mm_set1_pd(3.98437500E-01);
			   const __m128d rec15                     =  _mm_set1_pd(6.6666666666666666666E-02);
			   const __m128d _225                      =  _mm_set1_pd(225.0);
			   const __m128d xmax                      =  _mm_set1_pd(713.987E+00);
			   const __m128d _0                        =  _mm_setzero_pd();
			   const __m128d eps                       =  _mm_set1_pd(DBL_EPSILON);
			   const __m128d huge                      =  _mm_set1_pd(DBL_MAX);
			   __m128d a,b,bessel_i1,value;
			   __m128d sump,sumq,x,xx;

			   x  = _mm_abs_pd(arg);
			   if(_mm_cmp_pd_mask(x,eps,_CMP_LT_OQ)) {
                               value = _mm_mul_pd(_1_2,x);
			   }
			   else if(_mm_cmp_pd_mask(x,_15,_CMP_LT_OQ)) {
                               xx   = _mm_mul_pd(x,x);
			       sump = p[0];
			       sump = _mm_fmadd_pd(sump,xx,p[1]);
			       sump = _mm_fmadd_pd(sump,xx,p[2]);
			       sump = _mm_fmadd_pd(sump,xx,p[3]);
			       sump = _mm_fmadd_pd(sump,xx,p[4]);
			       sump = _mm_fmadd_pd(sump,xx,p[5]);
			       sump = _mm_fmadd_pd(sump,xx,p[6]);
			       sump = _mm_fmadd_pd(sump,xx,p[7]);
			       sump = _mm_fmadd_pd(sump,xx,p[8]);
			       sump = _mm_fmadd_pd(sump,xx,p[9]);
			       sump = _mm_fmadd_pd(sump,xx,p[10]);
			       sump = _mm_fmadd_pd(sump,xx,p[11]);
			       sump = _mm_fmadd_pd(sump,xx,p[12]);
			       sump = _mm_fmadd_pd(sump,xx,p[13]);
			       sump = _mm_fmadd_pd(sump,xx,p[14]);
			       xx   = _mm_sub_pd(xx,_225);
			       const __m128d t0 = _mm_fmadd_pd(_mm_add_pd(xx,q[0]),xx,q[1]);
			       const __m128d t1 = _mm_fmadd_pd(t0,xx,q[2]);
			       const __m128d t2 = _mm_fmadd_pd(t1,xx,q[3]);
			       const __m128d t3 = _mm_fmadd_pd(t2,xx,q[4]);
			       sumq             = t3;
			       value            = _mm_mul_pd(_mm_div_pd(sump,sumq),x);
			   }
			   else if(_mm_cmp_pd_mask(xmax,x,_CMP_LT_OQ)) {
                               value            = huge;
			   }
			   else {
                               xx               = _mm_sub_pd(_mm_div_pd(_1,x),rec15);
			       const __m128d t0 = _mm_fmadd_pd(pp[0],xx,pp[1]);
			       const __m128d c0 = _mm_fmadd_pd(_mm_add_pd(xx,qq[0]),xx,qq[1]);
			       const __m128d t1 = _mm_fmadd_pd(t0,xx,pp[2]);
			       const __m128d c1 = _mm_fmadd_pd(c0,xx,qq[2]);
			       const __m128d t2 = _mm_fmadd_pd(t1,xx,pp[3]);
			       const __m128d c2 = _mm_fmadd_pd(c1,xx,qq[3]);
			       const __m128d t3 = _mm_fmadd_pd(t2,xx,pp[4]);
			       const __m128d c3 = _mm_fmadd_pd(c2,xx,qq[4]);
			       const __m128d t4 = _mm_fmadd_pd(t3,xx,pp[5]);
			       const __m128d c4 = _mm_fmadd_pd(c3,xx,qq[5]);
			       const __m128d t5 = _mm_fmadd_pd(t4,xx,pp[6]);
			       const __m128d c5 = _mm_fmadd_pd(c4,xx,qq[6]);
			       const __m128d t6 = _mm_fmadd_pd(t5,xx,pp[7]);
			       sump             = t6;
			       sumq             = c5;
			       value            = _mm_div_pd(sump,sumq);
			       const __mmask8 m = _mm_cmp_pd_mask(_mm_sub_pd(xmax,_15),_CMP_LT_OQ);

			       a                = _mm_mask_blend_pd(m,_mm_exp_pd(x),
			                                                           _mm_exp_pd(_mm_sub_pd(x,_40)));
			       b                = _mm_mask_blend_pd(m,_1,_40);
			       const __m128d tmp= _mm_add_pd(_mm_mul_pd(value,a),
			                                        _mm_mul_pd(pbar,a));
			       value            = _mm_mul_pd(_mm_div_pd(tmp,_mm_sqrt_pd(x)),b);
			   }
			   if(_mm_cmp_pd_mask(arg,_0,_CMP_LT_OQ)) {
                              value             = negate_xmm2r8(value);
			   }
			   bessel_i1            = value
			   return (bessel_i1);
		    }
		    
		    
                   
                          __m128 bessel_i1_xmm4r4(const __m128 arg) {
                           __attribute__((section(".rodata")))
                           __attribute__((aligned(16))) static __m128  p[15] = {_mm_set1_ps(-1.9705291802535139930E-19f), 
                                                                      _mm_set1_ps(-6.5245515583151902910E-16f), 
                                                                      _mm_set1_ps(-1.1928788903603238754E-12f), 
                                                                      _mm_set1_ps(-1.4831904935994647675E-09f), 
                                                                      _mm_set1_ps(-1.3466829827635152875E-06f), 
                                                                      _mm_set1_ps(-9.1746443287817501309E-04f), 
                                                                      _mm_set1_ps(-4.7207090827310162436E-01f), 
                                                                      _mm_set1_ps(-1.8225946631657315931E+02f), 
                                                                      _mm_set1_ps(-5.1894091982308017540E+04f), 
                                                                      _mm_set1_ps(-1.0588550724769347106E+07f), 
                                                                      _mm_set1_ps(-1.4828267606612366099E+09f), 
                                                                      _mm_set1_ps(-1.3357437682275493024E+11f), 
                                                                      _mm_set1_ps(-6.9876779648010090070E+12f), 
                                                                      _mm_set1_ps(-1.7732037840791591320E+14f), 
                                                                      _mm_set1_ps(-1.4577180278143463643E+15f)};
                           __attribute__((section(".rodata")))
			   __attribute__((aligned(16))) static __m128 pp[8]  = {_mm_set1_ps(-6.0437159056137600000E-02f), 
                                                                      _mm_set1_ps(4.5748122901933459000E-01f), 
                                                                      _mm_set1_ps(-4.2843766903304806403E-01f), 
                                                                      _mm_set1_ps(9.7356000150886612134E-02f), 
                                                                      _mm_set1_ps(-3.2457723974465568321E-03f), 
                                                                      _mm_set1_ps(-3.6395264712121795296E-04f), 
                                                                      _mm_set1_ps(1.6258661867440836395E-05f), 
                                                                      _mm_set1_ps(-3.6347578404608223492E-07f)};
                           __attribute__((section(".rodata")))
			   __attribute__((aligned(16))) static __m128 q[5]   = {_mm_set1_ps(-4.0076864679904189921E+03f), 
                                                                      _mm_set1_ps(7.4810580356655069138E+06f), 
                                                                      _mm_set1_ps(-8.0059518998619764991E+09f), 
                                                                      _mm_set1_ps(4.8544714258273622913E+12f), 
                                                                      _mm_set1_ps(-1.3218168307321442305E+15f)};
                          __attribute__((section(".rodata")))
			   __attribute__((aligned(16))) static __m128 qq[6]  = {_mm_set1_ps(-3.8806586721556593450E+00f), 
                                                                      _mm_set1_ps(3.2593714889036996297E+00f), 
                                                                      _mm_set1_ps(-8.5017476463217924408E-01f), 
                                                                      _mm_set1_ps(7.4212010813186530069E-02f), 
                                                                      _mm_set1_ps(-2.2835624489492512649E-03f), 
                                                                      _mm_set1_ps(3.7510433111922824643E-05f)};
			   const __m128 exp40                     =  _mm_set1_ps(2.353852668370199854E+17f);
			   const __m128 _40                       =  _mm_set1_ps(40.0f);
			   const __m128 _1_2                      =  _mm_set1_ps(0.5f);
			   const __m128 _1                        =  _mm_set1_ps(1.0f);
			   const __m128 _15                       =  _mm_set1_ps(15.0f);
			   const __m128 pbar                      =  _mm_set1_ps(3.98437500E-01f);
			   const __m128 rec15                     =  _mm_set1_ps(6.6666666666666666666E-02f);
			   const __m128 _225                      =  _mm_set1_ps(225.0f);
			   const __m128 xmax                      =  _mm_set1_ps(713.987E+00f);
			   const __m128 _0                        =  _mm_setzero_ps();
			   const __m128 eps                       =  _mm_set1_ps(FLT_EPSILON);
			   const __m128 huge                      =  _mm_set1_ps(FLT_MAX);
			   __m128 a,b,bessel_i1,value;
			   __m128 sump,sumq,x,xx;

			   x  = _mm_abs_ps(arg);
			   if(_mm_cmp_ps_mask(x,eps,_CMP_LT_OQ)) {
                               value = _mm_mul_ps(_1_2,x);
			   }
			   else if(_mm_cmp_ps_mask(x,_15,_CMP_LT_OQ)) {
                               xx   = _mm_mul_ps(x,x);
			       sump = p[0];
			       sump = _mm_fmadd_ps(sump,xx,p[1]);
			       sump = _mm_fmadd_ps(sump,xx,p[2]);
			       sump = _mm_fmadd_ps(sump,xx,p[3]);
			       sump = _mm_fmadd_ps(sump,xx,p[4]);
			       sump = _mm_fmadd_ps(sump,xx,p[5]);
			       sump = _mm_fmadd_ps(sump,xx,p[6]);
			       sump = _mm_fmadd_ps(sump,xx,p[7]);
			       sump = _mm_fmadd_ps(sump,xx,p[8]);
			       sump = _mm_fmadd_ps(sump,xx,p[9]);
			       sump = _mm_fmadd_ps(sump,xx,p[10]);
			       sump = _mm_fmadd_ps(sump,xx,p[11]);
			       sump = _mm_fmadd_ps(sump,xx,p[12]);
			       sump = _mm_fmadd_ps(sump,xx,p[13]);
			       sump = _mm_fmadd_ps(sump,xx,p[14]);
			       xx   = _mm_sub_ps(xx,_225);
			       const __m128 t0 = _mm_fmadd_ps(_mm_add_ps(xx,q[0]),xx,q[1]);
			       const __m128 t1 = _mm_fmadd_ps(t0,xx,q[2]);
			       const __m128 t2 = _mm_fmadd_ps(t1,xx,q[3]);
			       const __m128 t3 = _mm_fmadd_ps(t2,xx,q[4]);
			       sumq             = t3;
			       value            = _mm_mul_ps(_mm_div_ps(sump,sumq),x);
			   }
			   else if(_mm_cmp_ps_mask(xmax,x,_CMP_LT_OQ)) {
                               value            = huge;
			   }
			   else {
                               xx               = _mm_sub_ps(_mm_div_ps(_1,x),rec15);
			       const __m128 t0 = _mm_fmadd_ps(pp[0],xx,pp[1]);
			       const __m128 c0 = _mm_fmadd_ps(_mm_add_ps(xx,qq[0]),xx,qq[1]);
			       const __m128 t1 = _mm_fmadd_ps(t0,xx,pp[2]);
			       const __m128 c1 = _mm_fmadd_ps(c0,xx,qq[2]);
			       const __m128 t2 = _mm_fmadd_ps(t1,xx,pp[3]);
			       const __m128 c2 = _mm_fmadd_ps(c1,xx,qq[3]);
			       const __m128 t3 = _mm_fmadd_ps(t2,xx,pp[4]);
			       const __m128 c3 = _mm_fmadd_ps(c2,xx,qq[4]);
			       const __m128 t4 = _mm_fmadd_ps(t3,xx,pp[5]);
			       const __m128 c4 = _mm_fmadd_ps(c3,xx,qq[5]);
			       const __m128 t5 = _mm_fmadd_ps(t4,xx,pp[6]);
			       const __m128 c5 = _mm_fmadd_ps(c4,xx,qq[6]);
			       const __m128 t6 = _mm_fmadd_ps(t5,xx,pp[7]);
			       sump             = t6;
			       sumq             = c5;
			       value            = _mm_div_ps(sump,sumq);
			       const __mmask8 m = _mm_cmp_ps_mask(_mm_sub_ps(xmax,_15),_CMP_LT_OQ);

			       a                = _mm_mask_blend_ps(m,_mm_exp_ps(x),
			                                                           _mm_exp_ps(_mm_sub_ps(x,_40)));
			       b                = _mm_mask_blend_ps(m,_1,_40);
			       const __m128 tmp= _mm_add_ps(_mm_mul_ps(value,a),
			                                        _mm_mul_ps(pbar,a));
			       value            = _mm_mul_ps(_mm_div_ps(tmp,_mm_sqrt_ps(x)),b);
			   }
			   if(_mm_cmp_ps_mask(arg,_0,_CMP_LT_OQ)) {
                              value             = negate_xmm4r4(value);
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

		      
		      __m128d beta_xmm2r8(const __m128d a,
		                          const __m128d b) {

                       // const __m128d nan = _mm_set1_pd(std::numeric_limits<double>::quiet_NaN());
			const __m128d _0  = _mm_setzero_pd();
			//if(__builtin_expect(_mm_cmp_pd_mask(a,_0,_CMP_LE_OQ),0) ||
			//   __builtin_expect(_mm_cmp_pd_mask(b,_0,_CMP_LE_OQ),0)) {
                       //    return (nan);
			//}
			const __m128d ab  = _mm_add_pd(a,b);
			__m128d beta      = _mm_setzero_pd();
			beta              = _mm_exp_pd(
			                              _mm_sub_pd(
						                 _mm_add_pd(gamma_log_xmm2r8(a),
								               gamma_log_xmm2r8(b)),
			return (beta);						                     gamma_log_xmm2r8(ab)));
		    }
		    
		    

#if defined(__ICC) || defined(__INTEL_COMPILER)
#include <svrng.h>
#else
#error 'Required Intel Compiler distribution'
#endif


                /*
                     
		      __m128d anglit_sample_xmm2r8() {

                         __m128d cdf;
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
                            const __m128d nan = _mm_set1_pd(std::numeric_limits<double>::quiet_NaN());
			    return (nan);
			 }
			 uniform          = svrng_new_uniform_distribution_double(0.0,1.0);
			 const double * __restrict ptr = (const double*)(&svrng_generate8_double(engine,uniform));
			 cdf              = anglit_cdf_inv_xmm2r8(_mm_loadu_pd(&ptr[0]));
			 svrng_delete_engine(engine);
			 return (cdf);
		    }


		     
		      __m128d anglit_sample_xmm2r8(const __m128 cdf) {

                            return (anglit_cdf_inv_xmm2r8(cdf));
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
		     
		      __m128d arcsin_cdf_xmm2r8(const __m128d x,
		                                const __m128d a) {

                         const __m128d invpi = _mm_set1_pd(0.318309886183790671537767526745);
			 const __m128d _0    = _mm_setzero_pd();
			 const __m128d _1_2  = _mm_set1_pd(0.5);
			 const __m128d _1    = _mm_set1_pd(1.0);
			 __m128d t0,cdf;
			 __mmask8 m0,m1;
			 m0  = _mm_cmp_pd_mask(x,negate_xmm2r8(a),_CMP_LE_OQ);
                         t0  = _mm_mul_pd(_mm_asin_pd(_mm_div_pd(x,a),invpi));
			 m1  = _mm_cmp_pd_mask(x,a,_CMP_LT_OQ);
                         cdf = _mm_mask_blend_pd(m0,_mm_add_pd(_1_2,t0),_0);
			 cdf = _mm_mask_blend_pd(m1,cdf,_1); 
                         return (cdf);
		   }

                 
                    __m128 arcsin_cdf_xmm4r4(const __m128 x,
		                             const __m128 a) {

                         const __m128 invpi = _mm_set1_ps(0.318309886183790671537767526745f);
			 const __m128 _0    = _mm_setzero_ps();
			 const __m128 _1_2  = _mm_set1_ps(0.5f);
			 const __m128 _1    = _mm_set1_ps(1.0f);
			 __m128 t0,cdf;
			 __mmask8 m0,m1;
			 m0  = _mm_cmp_ps_mask(x,negate_xmm4r4(a),_CMP_LE_OQ);
                         t0  = _mm_mul_ps(_mm_asin_ps(_mm_div_ps(x,a),invpi));
			 m1  = _mm_cmp_ps_mask(x,a,_CMP_LT_OQ);
                         cdf = _mm_mask_blend_ps(m0,_mm_add_pd(_1_2,t0),_0);
			 cdf = _mm_mask_blend_ps(m1,cdf,_1); 
                         return (cdf);
		   }
		   
		   
		    __m128d arcsin_cdf_inv_xmm2r8(const __m128d cdf,
		                                    const __m128d a) {

                           const __m128d pi    = _mm_set1_pd(3.14159265358979323846264338328);
			   const __m128d _0    = _mm_setzero_pd();
			   const __m128d _1    = _mm_set1_pd(1.0);
			   const __m128d _1_2  = _mm_set1_pd(0.5);
			   __m128d x;
	                   x = _mm_mul_pd(_mm_sin_pd(_mm_mul_pd(pi,_mm_sub_pd(cdf,_1_2))));
                           return (x);
		   }


		   
		      __m128 arcsin_cdf_inv_xmm4r4(const __m128 cdf,
		                                    const __m128 a) {

                           const __m128 pi    = _mm_set1_ps(3.14159265358979323846264338328f);
			   const __m128 _0    = _mm_setzero_ps();
			   const __m128 _1    = _mm_set1_ps(1.0f);
			   const __m128 _1_2  = _mm_set1_ps(0.5f);
			   __m128 x;
	                   x = _mm_mul_ps(_mm_sin_ps(_mm_mul_ps(pi,_mm_sub_ps(cdf,_1_2))));
                           return (x);
		   }







    

		    
		     		     
		     
		     
		     
		     
		    
		    
		    
