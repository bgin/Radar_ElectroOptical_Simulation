

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
#include "GMS_pdf_cdf_avx512.h"
#if (USE_SLEEF_LIB) == 1
#include "GMS_sleefsimddp.hpp"
#include "GMS_sleefsimdsp.hpp"
#endif
#include "GMS_simd_utils.h"
#include "GMS_LUT_cdf_pdf.h"


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




                         __m512d gamma_log_zmm8r8(const __m512d x) {
		      
                            //if(__builtin_expect(_mm512_cmp_pd_mask(x,_0,_CMP_LE_OQ),0) ||
			   //    __builtin_expect(_mm512_cmp_pd_mask(x,xbig,_CMP_GT_OQ),0)) {
                           //    return (huge);
			   // }
                         __attribute__((section(".rodata")))
                         __attribute__((aligned(64))) static __m512d c[7] = { _mm512_set1_pd(-1.910444077728E-03),
			                                     _mm512_set1_pd(8.4171387781295E-04),
                                                             _mm512_set1_pd(-5.952379913043012E-04), 
                                                             _mm512_set1_pd(7.93650793500350248E-04), 
                                                             _mm512_set1_pd(-2.777777777777681622553E-03), 
                                                             _mm512_set1_pd(8.333333333333333331554247E-02), 
                                                             _mm512_set1_pd(5.7083835261E-03)};
                         __attribute__((section(".rodata")))
			 __attribute__((aligned(64))) static __m512d p1[8] = {_mm512_set1_pd(4.945235359296727046734888E+00), 
                                                             _mm512_set1_pd(2.018112620856775083915565E+02), 
                                                             _mm512_set1_pd(2.290838373831346393026739E+03), 
                                                             _mm512_set1_pd(1.131967205903380828685045E+04),
                                                             _mm512_set1_pd(2.855724635671635335736389E+04), 
                                                             _mm512_set1_pd(3.848496228443793359990269E+04), 
                                                             _mm512_set1_pd(2.637748787624195437963534E+04), 
                                                             _mm512_set1_pd(7.225813979700288197698961E+03)};
                         __attribute__((section(".rodata")))                                    
			 __attribute__((aligned(64))) static __m512d p2[8] = {_mm512_set1_pd(4.974607845568932035012064E+00), 
                                                             _mm512_set1_pd(5.424138599891070494101986E+02), 
                                                             _mm512_set1_pd(1.550693864978364947665077E+04), 
                                                             _mm512_set1_pd(1.847932904445632425417223E+05), 
                                                             _mm512_set1_pd(1.088204769468828767498470E+06), 
                                                             _mm512_set1_pd(3.338152967987029735917223E+06), 
                                                             _mm512_set1_pd(5.106661678927352456275255E+06), 
                                                             _mm512_set1_pd(3.074109054850539556250927E+06)};
                         __attribute__((section(".rodata")))
			 __attribute__((aligned(64))) static __m512d p4[8] = {_mm512_set1_pd(1.474502166059939948905062E+04), 
                                                             _mm512_set1_pd(2.426813369486704502836312E+06), 
                                                             _mm512_set1_pd(1.214755574045093227939592E+08), 
                                                             _mm512_set1_pd(2.663432449630976949898078E+09), 
                                                             _mm512_set1_pd(2.940378956634553899906876E+10), 
                                                             _mm512_set1_pd(1.702665737765398868392998E+11), 
                                                             _mm512_set1_pd(4.926125793377430887588120E+11), 
                                                             _mm512_set1_pd(5.606251856223951465078242E+11)};
                         __attribute__((section(".rodata")))
                         __attribute__((aligned(64))) static __m512d q1[8] = {_mm512_set1_pd(6.748212550303777196073036E+01), 
                                                             _mm512_set1_pd(1.113332393857199323513008E+03), 
                                                             _mm512_set1_pd(7.738757056935398733233834E+03), 
                                                             _mm512_set1_pd(2.763987074403340708898585E+04), 
                                                             _mm512_set1_pd(5.499310206226157329794414E+04), 
                                                             _mm512_set1_pd(6.161122180066002127833352E+04), 
                                                             _mm512_set1_pd(3.635127591501940507276287E+04), 
                                                             _mm512_set1_pd(8.785536302431013170870835E+03)};
                         __attribute__((section(".rodata")))
			 __attribute__((aligned(64))) static __m512d q2[8] = {_mm512_set1_pd(1.830328399370592604055942E+02),
                                                             _mm512_set1_pd(7.765049321445005871323047E+03), 
                                                             _mm512_set1_pd(1.331903827966074194402448E+05),
                                                             _mm512_set1_pd(1.136705821321969608938755E+06), 
                                                             _mm512_set1_pd(5.267964117437946917577538E+06), 
                                                             _mm512_set1_pd(1.346701454311101692290052E+07), 
                                                             _mm512_set1_pd(1.782736530353274213975932E+07), 
                                                             _mm512_set1_pd(9.533095591844353613395747E+06)};
                         __attribute__((section(".rodata")))
			 __attribute__((aligned(64))) static __m512d q4[8] = {_mm512_set1_pd(2.690530175870899333379843E+03), 
                                                             _mm512_set1_pd(6.393885654300092398984238E+05), 
                                                             _mm512_set1_pd(4.135599930241388052042842E+07), 
                                                             _mm512_set1_pd(1.120872109616147941376570E+09), 
                                                             _mm512_set1_pd(1.488613728678813811542398E+10), 
                                                             _mm512_set1_pd(1.016803586272438228077304E+11), 
                                                             _mm512_set1_pd(3.417476345507377132798597E+11), 
                                                             _mm512_set1_pd(4.463158187419713286462081E+11)};
			    const __m512d d1     = _mm512_set1_pd(-5.772156649015328605195174E-01);
			    const __m512d d2     = _mm512_set1_pd(4.227843350984671393993777E-01);
                            const __m512d d4     = _mm512_set1_pd(1.791759469228055000094023E+00);
                            const __m512d frtbig = _mm512_set1_pd(1.42E+09);
                            const __m512d pnt68  = _mm512_set1_pd(0.6796875E+00);
			    const __m512d sqrtpi = _mm512_set1_pd(0.9189385332046727417803297E+00);
			    const __m512d xbig   = _mm512_set1_pd(4.08E+36);
			    const __m512d _0     = _mm512_setzero_pd();
			    const __m512d _1_2   = _mm512_set1_pd(0.5);
			    const __m512d _1_5   = _mm512_set1_pd(1.5);
			    const __m512d _1     = _mm512_set1_pd(1.0);
			    const __m512d _4     = _mm512_set1_pd(4.0);
			    const __m512d _2     = _mm512_set1_pd(2.0);
			    const __m512d _12    = _mm512_set1_pd(12.0);
			    const __m512d huge   = _mm512_set1_pd(DBL_MAX);
			    const __m512d eps    = _mm512_set1_pd(DBL_EPSILON);
			    __m512d gamlog,res,xden;
			    __m512d xm1,xm2,xm4;
			    __m512d xnum,xsq,corr;
			    gamlog = _mm512_setzero_pd();
			   
			    if(_mm512_cmp_pd_mask(x,eps,_CMP_LE_OQ)) {
                               res = zmm8r8_negate(_mm512_log_pd(x));
			    }
			    else if(_mm512_cmp_pd_mask(x,_1_5,_CMP_LE_OQ)) {
                               const __mmask8 m0 = _mm512_cmp_pd_mask(x,pnt68,_CMP_LT_OQ);
			       corr = _mm512_mask_blend_pd(m0,_0,zmm8r8_negate(_mm512_log_pd(x)));
			       xm1  = _mm512_mask_blend_pd(m0,_mm512_sub_pd(
			                                                _mm512_sub_pd(x,_1_2),_1_2));

			       if(_mm512_cmp_pd_mask(x,_1_2,_CMP_LE_OQ) ||
			          _mm512_cmp_pd_mask(pnt68,x,_CMP_LE_OQ)) {
                                   xden = _1;
				   xnum = _0;
				   xnum = _mm512_fmadd_pd(xnum,xm1,p1[0]);
				   xden = _mm512_fmadd_pd(xden,xm1,q1[0]);
				   xnum = _mm512_fmadd_pd(xnum,xm1,p1[1]);
				   xden = _mm512_fmadd_pd(xden,xm1,q1[1]);
				   xnum = _mm512_fmadd_pd(xnum,xm1,p1[2]);
				   xden = _mm512_fmadd_pd(xden,xm1,q1[2]);
				   xnum = _mm512_fmadd_pd(xnum,xm1,p1[3]);
				   xden = _mm512_fmadd_pd(xden,xm1,q1[3]);
				   xnum = _mm512_fmadd_pd(xnum,xm1,p1[4]);
				   xden = _mm512_fmadd_pd(xden,xm1,q1[4]);
				   xnum = _mm512_fmadd_pd(xnum,xm1,p1[5]);
				   xden = _mm512_fmadd_pd(xden,xm1,q1[5]);
				   xnum = _mm512_fmadd_pd(xnum,xm1,p1[6]);
				   xden = _mm512_fmadd_pd(xden,xm1,q1[6]);
				   xnum = _mm512_fmadd_pd(xnum,xm1,p1[7]);
				   xden = _mm512_fmadd_pd(xden,xm1,q1[7]);
				   const __m512d t0 = _mm512_fmadd_pd(xm1,
				                                  _mm512_div_pd(xnum,xden),d1);
				   res  = _mm512_add_pd(corr,
				                    _mm512_mul_pd(xm1,t0));
				}
				else {

                                   xm2  = _mm512_sub_pd(_mm512_sub_pd(x,_1_2),_1_2);
				   xden = _1;
				   xnum = _0;
				   xnum = _mm512_fmadd_pd(xnum,xm2,p2[0]);
				   xden = _mm512_fmadd_pd(xden,xm2,q2[0]);
				   xnum = _mm512_fmadd_pd(xnum,xm2,p2[1]);
				   xden = _mm512_fmadd_pd(xden,xm2,q2[1]);
				   xnum = _mm512_fmadd_pd(xnum,xm2,p2[2]);
				   xden = _mm512_fmadd_pd(xden,xm2,q2[2]);
				   xnum = _mm512_fmadd_pd(xnum,xm2,p2[3]);
				   xden = _mm512_fmadd_pd(xden,xm2,q2[3]);
				   xnum = _mm512_fmadd_pd(xnum,xm2,p2[4]);
				   xden = _mm512_fmadd_pd(xden,xm2,q2[4]);
				   xnum = _mm512_fmadd_pd(xnum,xm2,p2[5]);
				   xden = _mm512_fmadd_pd(xden,xm2,q2[5]);
				   xnum = _mm512_fmadd_pd(xnum,xm2,p2[6]);
				   xden = _mm512_fmadd_pd(xden,xm2,q2[6]);
				   xnum = _mm512_fmadd_pd(xnum,xm2,p2[7]);
				   xden = _mm512_fmadd_pd(xden,xm2,q2[7]);
				   const __m512d t0 = _mm512_fmadd_pd(xm2,
				                                  _mm512_div_pd(xnum,xden),d2);
				   res  = _mm512_add_pd(corr,
				                    _mm512_mul_pd(xm2,t0));
				}
			    }
			    else if(_mm512_cmp_pd_mask(x,_4,_CMP_LE_OQ)) {
                                   xm2  = _mm512_sub_pd(x,_2);
				   xden = _1;
				   xnum = _0;
				   xnum = _mm512_fmadd_pd(xnum,xm2,p2[0]);
				   xden = _mm512_fmadd_pd(xden,xm2,q2[0]);
				   xnum = _mm512_fmadd_pd(xnum,xm2,p2[1]);
				   xden = _mm512_fmadd_pd(xden,xm2,q2[1]);
				   xnum = _mm512_fmadd_pd(xnum,xm2,p2[2]);
				   xden = _mm512_fmadd_pd(xden,xm2,q2[2]);
				   xnum = _mm512_fmadd_pd(xnum,xm2,p2[3]);
				   xden = _mm512_fmadd_pd(xden,xm2,q2[3]);
				   xnum = _mm512_fmadd_pd(xnum,xm2,p2[4]);
				   xden = _mm512_fmadd_pd(xden,xm2,q2[4]);
				   xnum = _mm512_fmadd_pd(xnum,xm2,p2[5]);
				   xden = _mm512_fmadd_pd(xden,xm2,q2[5]);
				   xnum = _mm512_fmadd_pd(xnum,xm2,p2[6]);
				   xden = _mm512_fmadd_pd(xden,xm2,q2[6]);
				   xnum = _mm512_fmadd_pd(xnum,xm2,p2[7]);
				   xden = _mm512_fmadd_pd(xden,xm2,q2[7]);
				   res  = _mm512_mul_pd(xm2,
				                    _mm512_fmadd_pd(xm2,
						                _mm512_div_pd(xnum,xden),d2));
			    }
			    else if(_mm512_cmp_pd_mask(x,_12,_CMP_LE_OQ)) {
                                   xm4  = _mm512_sub_pd(x,_4);
				   xden = zmm8r8_negate(_1);
				   xnum = _0;
				   xnum = _mm512_fmadd_pd(xnum,xm4,p4[0]);
				   xden = _mm512_fmadd_pd(xden,xm4,q4[0]);
				   xnum = _mm512_fmadd_pd(xnum,xm4,p4[1]);
				   xden = _mm512_fmadd_pd(xden,xm4,q4[1]);
				   xnum = _mm512_fmadd_pd(xnum,xm4,p4[2]);
				   xden = _mm512_fmadd_pd(xden,xm4,q4[2]);
				   xnum = _mm512_fmadd_pd(xnum,xm4,p4[3]);
				   xden = _mm512_fmadd_pd(xden,xm4,q4[3]);
				   xnum = _mm512_fmadd_pd(xnum,xm4,p4[4]);
				   xden = _mm512_fmadd_pd(xden,xm4,q4[4]);
				   xnum = _mm512_fmadd_pd(xnum,xm4,p4[5]);
				   xden = _mm512_fmadd_pd(xden,xm4,q4[5]);
				   xnum = _mm512_fmadd_pd(xnum,xm4,p4[6]);
				   xden = _mm512_fmadd_pd(xden,xm4,q4[6]);
				   xnum = _mm512_fmadd_pd(xnum,xm4,p4[7]);
				   xden = _mm512_fmadd_pd(xden,xm4,q4[7]);
				   res  = _mm512_fmadd_pd(xm4,_mm512_div_pd(xnum,xden),d4);
			    }
			    else {
                                   res  = _0;
				   if(_mm512_cmp_pd_mask(x,frtbig,_CMP_LE_OQ)) {
                                      res = c[6];
				      xsq = _mm512_mul_pd(x,x);
				      res = _mm512_add_pd(_mm512_div_pd(res,xsq),c[0]);
				      res = _mm512_add_pd(_mm512_div_pd(res,xsq),c[1]);
				      res = _mm512_add_pd(_mm512_div_pd(res,xsq),c[2]);
				      res = _mm512_add_pd(_mm512_div_pd(res,xsq),c[3]);
				      res = _mm512_add_pd(_mm512_div_pd(res,xsq),c[4]);
				      res = _mm512_add_pd(_mm512_div_pd(res,xsq),c[5]);
				   }
                                   res  = _mm512_div_pd(res,x);
				   corr = _mm512_log_pd(x);
				   res  = _mm512_sub_pd(_mm512_add_pd(res,sqrtpi),
				                        _mm512_mul_pd(_1_2,corr));
				   res  = _mm512_fmadd_pd(x,_mm512_sub_pd(corr,_1),res);
				   
			    }

			    gamlog = res;
			    return (gamlog);
			    
		  }
		  
		  
		   __m512 gamma_log_zmm16r4(const __m512 x) {
		      
                        
                         __attribute__((section(".rodata")))
                         __attribute__((aligned(64))) static __m512 c[7] = { _mm512_set1_ps(-1.910444077728E-03),
			                                     _mm512_set1_ps(8.4171387781295E-04),
                                                             _mm512_set1_ps(-5.952379913043012E-04), 
                                                             _mm512_set1_ps(7.93650793500350248E-04), 
                                                             _mm512_set1_ps(-2.777777777777681622553E-03), 
                                                             _mm512_set1_ps(8.333333333333333331554247E-02), 
                                                             _mm512_set1_ps(5.7083835261E-03)};
                         __attribute__((section(".rodata")))
			 __attribute__((aligned(64))) static __m512 p1[8] = {_mm512_set1_ps(4.945235359296727046734888E+00), 
                                                             _mm512_set1_ps(2.018112620856775083915565E+02), 
                                                             _mm512_set1_ps(2.290838373831346393026739E+03), 
                                                             _mm512_set1_ps(1.131967205903380828685045E+04),
                                                             _mm512_set1_ps(2.855724635671635335736389E+04), 
                                                             _mm512_set1_ps(3.848496228443793359990269E+04), 
                                                             _mm512_set1_ps(2.637748787624195437963534E+04), 
                                                             _mm512_set1_ps(7.225813979700288197698961E+03)};
                         __attribute__((section(".rodata")))
			 __attribute__((aligned(64))) static __m512 p2[8] = {_mm512_set1_ps(4.974607845568932035012064E+00), 
                                                             _mm512_set1_ps(5.424138599891070494101986E+02), 
                                                             _mm512_set1_ps(1.550693864978364947665077E+04), 
                                                             _mm512_set1_ps(1.847932904445632425417223E+05), 
                                                             _mm512_set1_ps(1.088204769468828767498470E+06), 
                                                             _mm512_set1_ps(3.338152967987029735917223E+06), 
                                                             _mm512_set1_ps(5.106661678927352456275255E+06), 
                                                             _mm512_set1_ps(3.074109054850539556250927E+06)};
                         __attribute__((section(".rodata")))                                    
			 __attribute__((aligned(64))) static __m512 p4[8] = {_mm512_set1_ps(1.474502166059939948905062E+04), 
                                                             _mm512_set1_ps(2.426813369486704502836312E+06), 
                                                             _mm512_set1_ps(1.214755574045093227939592E+08), 
                                                             _mm512_set1_ps(2.663432449630976949898078E+09), 
                                                             _mm512_set1_ps(2.940378956634553899906876E+10), 
                                                             _mm512_set1_ps(1.702665737765398868392998E+11), 
                                                             _mm512_set1_ps(4.926125793377430887588120E+11), 
                                                             _mm512_set1_ps(5.606251856223951465078242E+11)};
                         __attribute__((section(".rodata")))
                         __attribute__((aligned(64))) static __m512 q1[8] = {_mm512_set1_ps(6.748212550303777196073036E+01), 
                                                             _mm512_set1_ps(1.113332393857199323513008E+03), 
                                                             _mm512_set1_ps(7.738757056935398733233834E+03), 
                                                             _mm512_set1_ps(2.763987074403340708898585E+04), 
                                                             _mm512_set1_ps(5.499310206226157329794414E+04), 
                                                             _mm512_set1_ps(6.161122180066002127833352E+04), 
                                                             _mm512_set1_ps(3.635127591501940507276287E+04), 
                                                             _mm512_set1_ps(8.785536302431013170870835E+03)};
                         __attribute__((section(".rodata")))
			 __attribute__((aligned(64))) static __m512 q2[8] = {_mm512_set1_ps(1.830328399370592604055942E+02),
                                                             _mm512_set1_ps(7.765049321445005871323047E+03), 
                                                             _mm512_set1_ps(1.331903827966074194402448E+05),
                                                             _mm512_set1_ps(1.136705821321969608938755E+06), 
                                                             _mm512_set1_ps(5.267964117437946917577538E+06), 
                                                             _mm512_set1_ps(1.346701454311101692290052E+07), 
                                                             _mm512_set1_ps(1.782736530353274213975932E+07), 
                                                             _mm512_set1_ps(9.533095591844353613395747E+06)};
                         __attribute__((section(".rodata")))
			 __attribute__((aligned(64))) static __m512 q4[8] = {_mm512_set1_ps(2.690530175870899333379843E+03), 
                                                             _mm512_set1_ps(6.393885654300092398984238E+05), 
                                                             _mm512_set1_ps(4.135599930241388052042842E+07), 
                                                             _mm512_set1_ps(1.120872109616147941376570E+09), 
                                                             _mm512_set1_ps(1.488613728678813811542398E+10), 
                                                             _mm512_set1_ps(1.016803586272438228077304E+11), 
                                                             _mm512_set1_ps(3.417476345507377132798597E+11), 
                                                             _mm512_set1_ps(4.463158187419713286462081E+11)};
			    const __m512 d1     = _mm512_set1_ps(-5.772156649015328605195174E-01);
			    const __m512 d2     = _mm512_set1_ps(4.227843350984671393993777E-01);
                            const __m512 d4     = _mm512_set1_ps(1.791759469228055000094023E+00);
                            const __m512 frtbig = _mm512_set1_ps(1.42E+09);
                            const __m512 pnt68  = _mm512_set1_ps(0.6796875E+00);
			    const __m512 sqrtpi = _mm512_set1_ps(0.9189385332046727417803297E+00);
			    const __m512 xbig   = _mm512_set1_ps(4.08E+36);
			    const __m512 _0     = _mm512_setzero_ps();
			    const __m512 _1_2   = _mm512_set1_ps(0.5);
			    const __m512 _1_5   = _mm512_set1_ps(1.5);
			    const __m512 _1     = _mm512_set1_ps(1.0);
			    const __m512 _4     = _mm512_set1_ps(4.0);
			    const __m512 _2     = _mm512_set1_ps(2.0);
			    const __m512 _12    = _mm512_set1_ps(12.0);
			    const __m512 huge   = _mm512_set1_ps(FLT_MAX);
			    const __m512 eps    = _mm512_set1_ps(FLT_EPSILON);
			    __m512 gamlog,res,xden;
			    __m512 xm1,xm2,xm4;
			    __m512 xnum,xsq,corr;
			    gamlog = _mm512_setzero_ps();
			   
			    if(_mm512_cmp_ps_mask(x,eps,_CMP_LE_OQ)) {
                               res = zmm16r4_negate(_mm512_log_ps(x));
			    }
			    else if(_mm512_cmp_ps_mask(x,_1_5,_CMP_LE_OQ)) {
                               const __mmask16 m0 = _mm512_cmp_ps_mask(x,pnt68,_CMP_LT_OQ);
			       corr = _mm512_mask_blend_ps(m0,_0,zmm16r4_negate(_mm512_log_ps(x)));
			       xm1  = _mm512_mask_blend_ps(m0,_mm512_sub_ps(
			                                                _mm512_sub_ps(x,_1_2),_1_2));

			       if(_mm512_cmp_ps_mask(x,_1_2,_CMP_LE_OQ) ||
			          _mm512_cmp_ps_mask(pnt68,x,_CMP_LE_OQ)) {
                                   xden = _1;
				   xnum = _0;
				   xnum = _mm512_fmadd_ps(xnum,xm1,p1[0]);
				   xden = _mm512_fmadd_ps(xden,xm1,q1[0]);
				   xnum = _mm512_fmadd_ps(xnum,xm1,p1[1]);
				   xden = _mm512_fmadd_ps(xden,xm1,q1[1]);
				   xnum = _mm512_fmadd_ps(xnum,xm1,p1[2]);
				   xden = _mm512_fmadd_ps(xden,xm1,q1[2]);
				   xnum = _mm512_fmadd_ps(xnum,xm1,p1[3]);
				   xden = _mm512_fmadd_ps(xden,xm1,q1[3]);
				   xnum = _mm512_fmadd_ps(xnum,xm1,p1[4]);
				   xden = _mm512_fmadd_ps(xden,xm1,q1[4]);
				   xnum = _mm512_fmadd_ps(xnum,xm1,p1[5]);
				   xden = _mm512_fmadd_ps(xden,xm1,q1[5]);
				   xnum = _mm512_fmadd_ps(xnum,xm1,p1[6]);
				   xden = _mm512_fmadd_ps(xden,xm1,q1[6]);
				   xnum = _mm512_fmadd_ps(xnum,xm1,p1[7]);
				   xden = _mm512_fmadd_ps(xden,xm1,q1[7]);
				   const __m512 t0 = _mm512_fmadd_ps(xm1,
				                                  _mm512_div_ps(xnum,xden),d1);
				   res  = _mm512_add_ps(corr,
				                    _mm512_mul_ps(xm1,t0));
				}
				else {

                                   xm2  = _mm512_sub_ps(_mm512_sub_ps(x,_1_2),_1_2);
				   xden = _1;
				   xnum = _0;
				   xnum = _mm512_fmadd_ps(xnum,xm2,p2[0]);
				   xden = _mm512_fmadd_ps(xden,xm2,q2[0]);
				   xnum = _mm512_fmadd_ps(xnum,xm2,p2[1]);
				   xden = _mm512_fmadd_ps(xden,xm2,q2[1]);
				   xnum = _mm512_fmadd_ps(xnum,xm2,p2[2]);
				   xden = _mm512_fmadd_ps(xden,xm2,q2[2]);
				   xnum = _mm512_fmadd_ps(xnum,xm2,p2[3]);
				   xden = _mm512_fmadd_ps(xden,xm2,q2[3]);
				   xnum = _mm512_fmadd_ps(xnum,xm2,p2[4]);
				   xden = _mm512_fmadd_ps(xden,xm2,q2[4]);
				   xnum = _mm512_fmadd_ps(xnum,xm2,p2[5]);
				   xden = _mm512_fmadd_ps(xden,xm2,q2[5]);
				   xnum = _mm512_fmadd_ps(xnum,xm2,p2[6]);
				   xden = _mm512_fmadd_ps(xden,xm2,q2[6]);
				   xnum = _mm512_fmadd_ps(xnum,xm2,p2[7]);
				   xden = _mm512_fmadd_ps(xden,xm2,q2[7]);
				   const __m512 t0 = _mm512_fmadd_ps(xm2,
				                                  _mm512_div_ps(xnum,xden),d2);
				   res  = _mm512_add_ps(corr,
				                    _mm512_mul_ps(xm2,t0));
				}
			    }
			    else if(_mm512_cmp_ps_mask(x,_4,_CMP_LE_OQ)) {
                                   xm2  = _mm512_sub_ps(x,_2);
				   xden = _1;
				   xnum = _0;
				   xnum = _mm512_fmadd_ps(xnum,xm2,p2[0]);
				   xden = _mm512_fmadd_ps(xden,xm2,q2[0]);
				   xnum = _mm512_fmadd_ps(xnum,xm2,p2[1]);
				   xden = _mm512_fmadd_ps(xden,xm2,q2[1]);
				   xnum = _mm512_fmadd_ps(xnum,xm2,p2[2]);
				   xden = _mm512_fmadd_ps(xden,xm2,q2[2]);
				   xnum = _mm512_fmadd_ps(xnum,xm2,p2[3]);
				   xden = _mm512_fmadd_ps(xden,xm2,q2[3]);
				   xnum = _mm512_fmadd_ps(xnum,xm2,p2[4]);
				   xden = _mm512_fmadd_ps(xden,xm2,q2[4]);
				   xnum = _mm512_fmadd_ps(xnum,xm2,p2[5]);
				   xden = _mm512_fmadd_ps(xden,xm2,q2[5]);
				   xnum = _mm512_fmadd_ps(xnum,xm2,p2[6]);
				   xden = _mm512_fmadd_ps(xden,xm2,q2[6]);
				   xnum = _mm512_fmadd_ps(xnum,xm2,p2[7]);
				   xden = _mm512_fmadd_ps(xden,xm2,q2[7]);
				   res  = _mm512_mul_ps(xm2,
				                    _mm512_fmadd_ps(xm2,
						                _mm512_div_ps(xnum,xden),d2));
			    }
			    else if(_mm512_cmp_ps_mask(x,_12,_CMP_LE_OQ)) {
                                   xm4  = _mm512_sub_ps(x,_4);
				   xden = zmm16r4_negate(_1);
				   xnum = _0;
				   xnum = _mm512_fmadd_ps(xnum,xm4,p4[0]);
				   xden = _mm512_fmadd_ps(xden,xm4,q4[0]);
				   xnum = _mm512_fmadd_ps(xnum,xm4,p4[1]);
				   xden = _mm512_fmadd_ps(xden,xm4,q4[1]);
				   xnum = _mm512_fmadd_ps(xnum,xm4,p4[2]);
				   xden = _mm512_fmadd_ps(xden,xm4,q4[2]);
				   xnum = _mm512_fmadd_ps(xnum,xm4,p4[3]);
				   xden = _mm512_fmadd_ps(xden,xm4,q4[3]);
				   xnum = _mm512_fmadd_ps(xnum,xm4,p4[4]);
				   xden = _mm512_fmadd_ps(xden,xm4,q4[4]);
				   xnum = _mm512_fmadd_ps(xnum,xm4,p4[5]);
				   xden = _mm512_fmadd_ps(xden,xm4,q4[5]);
				   xnum = _mm512_fmadd_ps(xnum,xm4,p4[6]);
				   xden = _mm512_fmadd_ps(xden,xm4,q4[6]);
				   xnum = _mm512_fmadd_ps(xnum,xm4,p4[7]);
				   xden = _mm512_fmadd_ps(xden,xm4,q4[7]);
				   res  = _mm512_fmadd_ps(xm4,_mm512_div_ps(xnum,xden),d4);
			    }
			    else {
                                   res  = _0;
				   if(_mm512_cmp_ps_mask(x,frtbig,_CMP_LE_OQ)) {
                                      res = c[6];
				      xsq = _mm512_mul_ps(x,x);
				      res = _mm512_add_ps(_mm512_div_ps(res,xsq),c[0]);
				      res = _mm512_add_ps(_mm512_div_ps(res,xsq),c[1]);
				      res = _mm512_add_ps(_mm512_div_ps(res,xsq),c[2]);
				      res = _mm512_add_ps(_mm512_div_ps(res,xsq),c[3]);
				      res = _mm512_add_ps(_mm512_div_ps(res,xsq),c[4]);
				      res = _mm512_add_ps(_mm512_div_ps(res,xsq),c[5]);
				   }
                                   res  = _mm512_div_ps(res,x);
				   corr = _mm512_log_ps(x);
				   res  = _mm512_sub_ps(_mm512_add_ps(res,sqrtpi),
				                        _mm512_mul_ps(_1_2,corr));
				   res  = _mm512_fmadd_ps(x,_mm512_sub_ps(corr,_1),res);
				   
			    }

			    gamlog = res;
			    return (gamlog);
			    
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


                    
		      __m512d  bessesl_i0_zmm8r8(const __m512d arg) {

                            __attribute__((section(".rodata")))
                            __ATTR_ALIGN__(64) static __m512d p[15] = {_mm512_set1_pd(-5.2487866627945699800E-18),
                                                                      _mm512_set1_pd(-1.5982226675653184646E-14), 
                                                                      _mm512_set1_pd(-2.6843448573468483278E-11), 
                                                                      _mm512_set1_pd(-3.0517226450451067446E-08), 
                                                                      _mm512_set1_pd(-2.5172644670688975051E-05), 
                                                                      _mm512_set1_pd(-1.5453977791786851041E-02), 
                                                                      _mm512_set1_pd(-7.0935347449210549190E+00), 
                                                                      _mm512_set1_pd(-2.4125195876041896775E+03), 
                                                                      _mm512_set1_pd(-5.9545626019847898221E+05), 
                                                                      _mm512_set1_pd(-1.0313066708737980747E+08), 
                                                                      _mm512_set1_pd(-1.1912746104985237192E+10), 
                                                                      _mm512_set1_pd(-8.4925101247114157499E+11), 
                                                                      _mm512_set1_pd(-3.2940087627407749166E+13), 
                                                                      _mm512_set1_pd(-5.5050369673018427753E+14), 
                                                                      _mm512_set1_pd(-2.2335582639474375249E+15)};
                            __attribute__((section(".rodata")))                                          
			    __ATTR_ALIGN__(64) static __m512d pp[8] = {_mm512_set1_pd(-3.9843750000000000000E-01), 
                                                                      _mm512_set1_pd(2.9205384596336793945E+00), 
                                                                      _mm512_set1_pd(-2.4708469169133954315E+00), 
                                                                      _mm512_set1_pd(4.7914889422856814203E-01), 
                                                                      _mm512_set1_pd(-3.7384991926068969150E-03), 
                                                                      _mm512_set1_pd(-2.6801520353328635310E-03), 
                                                                      _mm512_set1_pd(9.9168777670983678974E-05), 
                                                                      _mm512_set1_pd(-2.1877128189032726730E-06)};
                            __attribute__((section(".rodata")))
			    __ATTR_ALIGN__(64) static __m512d q[5]  = {_mm512_set1_pd(-3.7277560179962773046E+03), 
                                                                      _mm512_set1_pd(6.5158506418655165707E+06), 
                                                                      _mm512_set1_pd(-6.5626560740833869295E+09), 
                                                                      _mm512_set1_pd(3.7604188704092954661E+12), 
                                                                      _mm512_set1_pd(-9.7087946179594019126E+14)};
                           __attribute__((section(".rodata")))                                           
			    __ATTR_ALIGN__(64) static __m512d qq[7] = {_mm512_set1_pd(-3.1446690275135491500E+01), 
                                                                      _mm512_set1_pd(8.5539563258012929600E+01), 
                                                                      _mm512_set1_pd(-6.0228002066743340583E+01), 
                                                                      _mm512_set1_pd(1.3982595353892851542E+01), 
                                                                      _mm512_set1_pd(-1.1151759188741312645E+00), 
                                                                      _mm512_set1_pd(3.2547697594819615062E-02), 
                                                                      _mm512_set1_pd(-5.5194330231005480228E-04)};
			    const __m512d rec15                    =  _mm512_set1_pd(6.6666666666666666666E-02);
			    const __m512d xmax                     =  _mm512_set1_pd(91.9E+00);
			    const __m512d exp40                    =  _mm512_set1_pd(2.353852668370199854E+17);
			    const __m512d _1                       =  _mm512_set1_pd(1.0);
			    const __m512d _15                      =  _mm512_set1_pd(15.0);
			    const __m512d _225                     =  _mm512_set1_pd(225.0);
			    const __m512d _40                      =  _mm512_set1_pd(40.0);
			    const __m512d eps                      =  _mm512_set1_pd(DBL_EPSILON);
			    const __m512d huge                     =  _mm512_set1_pd(DBL_MAX);
			    __m512d value,a,b,bessel_i0;
			    __m512d sump,sumq,x,xx;
                            x = _mm512_abs_pd(arg);
			    if(_mm512_cmp_pd_mask(x,eps,_CMP_LT_OQ)) {
                               value = _1;
			    }
			    else if(_mm512_cmp_pd_mask(x,_15,_CMP_LT_OQ)) {
                               xx   = _mm512_mul_pd(x,x);
			       sump = p[0];
			       sump = _mm512_fmadd_pd(sump,xx,p[1]);
			       sump = _mm512_fmadd_pd(sump,xx,p[2]);
			       sump = _mm512_fmadd_pd(sump,xx,p[3]);
			       sump = _mm512_fmadd_pd(sump,xx,p[4]);
			       sump = _mm512_fmadd_pd(sump,xx,p[5]);
			       sump = _mm512_fmadd_pd(sump,xx,p[6]);
			       sump = _mm512_fmadd_pd(sump,xx,p[7]);
			       sump = _mm512_fmadd_pd(sump,xx,p[8]);
			       sump = _mm512_fmadd_pd(sump,xx,p[9]);
			       sump = _mm512_fmadd_pd(sump,xx,p[10]);
			       sump = _mm512_fmadd_pd(sump,xx,p[11]);
			       sump = _mm512_fmadd_pd(sump,xx,p[12]);
			       sump = _mm512_fmadd_pd(sump,xx,p[13]);
			       sump = _mm512_fmadd_pd(sump,xx,p[14]);
			       xx   = _mm512_sub_pd(xx,_225);
			       const __m512d xxq0 = _mm512_add_pd(xx,q[0]);
			       const __m512d xxq1 = _mm512_add_pd(xx,q[1]);
			       const __m512d xxq2 = _mm512_add_pd(xx,q[2]);
			       const __m512d xxq3 = _mm512_add_pd(xx,q[3]);
			       const __m512d xxq4 = _mm512_add_pd(xx,q[4]);
			       sumq = _mm512_mul_pd(xxq0,
			                        _mm512_mul_pd(xxq1,
						          _mm512_mul_pd(xxq2,
							            _mm512_mul_pd(xxq3,xxq4))));
			       value = _mm512_div_pd(sump,sumq);
			                                         
			    }
			    else if(_mm512_cmp_pd_mask(_15,x,_CMP_LE_OQ)) {
                                    if(_mm512_cmp_pd_mask(xmax,x,_CMP_LT_OQ)) {
                                       value = huge;
				    }
				    else {
                                           xx = _mm512_sub_pd(_mm512_div_pd(_1,x),rec15);
					   const __m512d t0 = _mm512_fmadd_pd(pp[0],xx,pp[1]);
					   const __m512d c0 = _mm512_fmadd_pd(_mm512_add_pd(xx,qq[0]),xx,qq[1]);
					   const __m512d t1 = _mm512_fmadd_pd(t0,xx,pp[2]);
					   const __m512d c1 = _mm512_fmadd_pd(c0,xx,qq[2]);
					   const __m512d t2 = _mm512_fmadd_pd(t1,xx,pp[3]);
					   const __m512d c2 = _mm512_fmadd_pd(c1,xx,qq[3]);
					   const __m512d t3 = _mm512_fmadd_pd(t2,xx,pp[4]);
					   const __m512d c3 = _mm512_fmadd_pd(c2,xx,qq[4]);
					   const __m512d t4 = _mm512_fmadd_pd(t3,xx,pp[5]);
					   const __m512d c4 = _mm512_fmadd_pd(c3,xx,qq[5]);
					   const __m512d t5 = _mm512_fmadd_pd(t4,xx,pp[6]);
					   const __m512d c5 = _mm512_fmadd_pd(c4,xx,qq[6]);
					   const __m512d t6 = _mm512_fmadd_pd(t5,xx,pp[7]);
					   sump             = t6;
					   sumq             = c5;
					   value            = _mm512_div_pd(sump,sumq);
					   const __mmask8 m = _mm512_cmp_pd_mask(x,_mm512_sub_pd(xmax,_15),_CMP_LE_OQ);
					   a                = _mm512_mask_blend_pd(m,_mm512_exp_pd(_mm512_sub_pd(x,_40)),
					                                             _mm512_exp_pd(x));
					   b                = _mm512_mask_blend_pd(m,exp40,_1);
					   const __m512 tmp = _mm512_sub_pd(_mm512_mul_pd(value,a),
					                                    _mm512_mul_pd(pp[0],a));
					   value            = _mm512_mul_pd(_mm512_div_pd(tmp,_mm512_sqrt_pd(x)),b);
				    }
			    }
			   
			    bessel_i0 = value;
			    return (bessel_i0);
		    }


		    
		      __m512  bessesl_i0_zmm16r4(const __m512 arg) {

                            __attribute__((section(".rodata")))
                            __ATTR_ALIGN__(64) static __m512 p[15] = {_mm512_set1_ps(-5.2487866627945699800E-18f),
                                                                      _mm512_set1_ps(-1.5982226675653184646E-14f), 
                                                                      _mm512_set1_ps(-2.6843448573468483278E-11f), 
                                                                      _mm512_set1_ps(-3.0517226450451067446E-08f), 
                                                                      _mm512_set1_ps(-2.5172644670688975051E-05f), 
                                                                      _mm512_set1_ps(-1.5453977791786851041E-02f), 
                                                                      _mm512_set1_ps(-7.0935347449210549190E+00f), 
                                                                      _mm512_set1_ps(-2.4125195876041896775E+03f), 
                                                                      _mm512_set1_ps(-5.9545626019847898221E+05f), 
                                                                      _mm512_set1_ps(-1.0313066708737980747E+08f), 
                                                                      _mm512_set1_ps(-1.1912746104985237192E+10f), 
                                                                      _mm512_set1_ps(-8.4925101247114157499E+11f), 
                                                                      _mm512_set1_ps(-3.2940087627407749166E+13f), 
                                                                      _mm512_set1_ps(-5.5050369673018427753E+14f), 
                                                                      _mm512_set1_ps(-2.2335582639474375249E+15f)};
                            __attribute__((section(".rodata")))
			    __ATTR_ALIGN__(64) static __m512 pp[8] = {_mm512_set1_ps(-3.9843750000000000000E-01f), 
                                                                      _mm512_set1_ps(2.9205384596336793945E+00f), 
                                                                      _mm512_set1_ps(-2.4708469169133954315E+00f), 
                                                                      _mm512_set1_ps(4.7914889422856814203E-01f), 
                                                                      _mm512_set1_ps(-3.7384991926068969150E-03f), 
                                                                      _mm512_set1_ps(-2.6801520353328635310E-03f), 
                                                                      _mm512_set1_ps(9.9168777670983678974E-05f), 
                                                                      _mm512_set1_ps(-2.1877128189032726730E-06f)};
                            __attribute__((section(".rodata")))
			    __ATTR_ALIGN__(64) static __m512 q[5]  = {_mm512_set1_ps(-3.7277560179962773046E+03f), 
                                                                      _mm512_set1_ps(6.5158506418655165707E+06f), 
                                                                      _mm512_set1_ps(-6.5626560740833869295E+09f), 
                                                                      _mm512_set1_ps(3.7604188704092954661E+12f), 
                                                                      _mm512_set1_ps(-9.7087946179594019126E+14f)};
                            __attribute__((section(".rodata")))                                          
			    __ATTR_ALIGN__(64) static __m512 qq[7] = {_mm512_set1_ps(-3.1446690275135491500E+01f), 
                                                                      _mm512_set1_ps(8.5539563258012929600E+01f), 
                                                                      _mm512_set1_ps(-6.0228002066743340583E+01f), 
                                                                      _mm512_set1_ps(1.3982595353892851542E+01f), 
                                                                      _mm512_set1_ps(-1.1151759188741312645E+00f), 
                                                                      _mm512_set1_ps(3.2547697594819615062E-02f), 
                                                                      _mm512_set1_ps(-5.5194330231005480228E-04f)};
			    const __m512 rec15                    =  _mm512_set1_ps(6.6666666666666666666E-02f);
			    const __m512 xmax                     =  _mm512_set1_ps(91.9E+00f);
			    const __m512 exp40                    =  _mm512_set1_ps(2.353852668370199854E+17f);
			    const __m512 _1                       =  _mm512_set1_ps(1.0f);
			    const __m512 _15                      =  _mm512_set1_ps(15.0f);
			    const __m512 _225                     =  _mm512_set1_ps(225.0f);
			    const __m512 _40                      =  _mm512_set1_ps(40.0f);
			    const __m512 eps                      =  _mm512_set1_pd(FLT_EPSILON);
			    const __m512 huge                     =  _mm512_set1_pd(FLT_MAX);
			    __m512 value,a,b,bessel_i0;
			    __m512 sump,sumq,x,xx;
                            x = _mm512_abs_ps(arg);
			    if(_mm512_cmp_ps_mask(x,eps,_CMP_LT_OQ)) {
                               value = _1;
			    }
			    else if(_mm512_cmp_ps_mask(x,_15,_CMP_LT_OQ)) {
                               xx   = _mm512_mul_ps(x,x);
			       sump = p[0];
			       sump = _mm512_fmadd_ps(sump,xx,p[1]);
			       sump = _mm512_fmadd_ps(sump,xx,p[2]);
			       sump = _mm512_fmadd_ps(sump,xx,p[3]);
			       sump = _mm512_fmadd_ps(sump,xx,p[4]);
			       sump = _mm512_fmadd_ps(sump,xx,p[5]);
			       sump = _mm512_fmadd_ps(sump,xx,p[6]);
			       sump = _mm512_fmadd_ps(sump,xx,p[7]);
			       sump = _mm512_fmadd_ps(sump,xx,p[8]);
			       sump = _mm512_fmadd_ps(sump,xx,p[9]);
			       sump = _mm512_fmadd_ps(sump,xx,p[10]);
			       sump = _mm512_fmadd_ps(sump,xx,p[11]);
			       sump = _mm512_fmadd_ps(sump,xx,p[12]);
			       sump = _mm512_fmadd_ps(sump,xx,p[13]);
			       sump = _mm512_fmadd_ps(sump,xx,p[14]);
			       xx   = _mm512_sub_ps(xx,_225);
			       const __m512 xxq0 = _mm512_add_ps(xx,q[0]);
			       const __m512 xxq1 = _mm512_add_ps(xx,q[1]);
			       const __m512 xxq2 = _mm512_add_ps(xx,q[2]);
			       const __m512 xxq3 = _mm512_add_ps(xx,q[3]);
			       const __m512 xxq4 = _mm512_add_ps(xx,q[4]);
			       sumq = _mm512_mul_ps(xxq0,
			                        _mm512_mul_ps(xxq1,
						          _mm512_mul_ps(xxq2,
							            _mm512_mul_ps(xxq3,xxq4))));
			       value = _mm512_div_ps(sump,sumq);
			                                         
			    }
			    else if(_mm512_cmp_ps_mask(_15,x,_CMP_LE_OQ)) {
                                    if(_mm512_cmp_ps_mask(xmax,x,_CMP_LT_OQ)) {
                                       value = huge;
				    }
				    else {
                                           xx = _mm512_sub_ps(_mm512_div_ps(_1,x),rec15);
					   const __m512 t0 = _mm512_fmadd_ps(pp[0],xx,pp[1]);
					   const __m512 c0 = _mm512_fmadd_ps(_mm512_add_ps(xx,qq[0]),xx,qq[1]);
					   const __m512 t1 = _mm512_fmadd_ps(t0,xx,pp[2]);
					   const __m512 c1 = _mm512_fmadd_ps(c0,xx,qq[2]);
					   const __m512 t2 = _mm512_fmadd_ps(t1,xx,pp[3]);
					   const __m512 c2 = _mm512_fmadd_ps(c1,xx,qq[3]);
					   const __m512 t3 = _mm512_fmadd_ps(t2,xx,pp[4]);
					   const __m512 c3 = _mm512_fmadd_ps(c2,xx,qq[4]);
					   const __m512 t4 = _mm512_fmadd_ps(t3,xx,pp[5]);
					   const __m512 c4 = _mm512_fmadd_ps(c3,xx,qq[5]);
					   const __m512 t5 = _mm512_fmadd_ps(t4,xx,pp[6]);
					   const __m512 c5 = _mm512_fmadd_ps(c4,xx,qq[6]);
					   const __m512 t6 = _mm512_fmadd_ps(t5,xx,pp[7]);
					   sump             = t6;
					   sumq             = c5;
					   value            = _mm512_div_ps(sump,sumq);
					   const __mmask8 m = _mm512_cmp_ps_mask(x,_mm512_sub_ps(xmax,_15),_CMP_LE_OQ);
#if (USE_SLEEF_LIB) == 1
                                           a                = _mm512_mask_blend_ps(m,xexpf(_mm512_sub_ps(x,_40)),
					                                             xexpf(x));
#else
					   a                = _mm512_mask_blend_ps(m,_mm512_exp_ps(_mm512_sub_ps(x,_40)),
					                                             _mm512_exp_ps(x));
#endif     
					   b                = _mm512_mask_blend_ps(m,exp40,_1);
					   const __m512 tmp = _mm512_sub_ps(_mm512_mul_ps(value,a),
					                                    _mm512_mul_ps(pp[0],a));
					   value            = _mm512_mul_ps(_mm512_div_ps(tmp,_mm512_sqrt_ps(x)),b);
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


                      
		      __m512d bessel_i1_zmm8r8(const __m512d arg) {

                           __attribute__((section(".rodata")))
                           __ATTR_ALIGN__(64) static __m512d  p[15] = {_mm512_set1_pd(-1.9705291802535139930E-19), 
                                                                      _mm512_set1_pd(-6.5245515583151902910E-16), 
                                                                      _mm512_set1_pd(-1.1928788903603238754E-12), 
                                                                      _mm512_set1_pd(-1.4831904935994647675E-09), 
                                                                      _mm512_set1_pd(-1.3466829827635152875E-06), 
                                                                      _mm512_set1_pd(-9.1746443287817501309E-04), 
                                                                      _mm512_set1_pd(-4.7207090827310162436E-01), 
                                                                      _mm512_set1_pd(-1.8225946631657315931E+02), 
                                                                      _mm512_set1_pd(-5.1894091982308017540E+04), 
                                                                      _mm512_set1_pd(-1.0588550724769347106E+07), 
                                                                      _mm512_set1_pd(-1.4828267606612366099E+09), 
                                                                      _mm512_set1_pd(-1.3357437682275493024E+11), 
                                                                      _mm512_set1_pd(-6.9876779648010090070E+12), 
                                                                      _mm512_set1_pd(-1.7732037840791591320E+14), 
                                                                      _mm512_set1_pd(-1.4577180278143463643E+15)};
                           __attribute__((section(".rodata")))
			   __ATTR_ALIGN__(64) static __m512d pp[8]  = {_mm512_set1_pd(-6.0437159056137600000E-02), 
                                                                      _mm512_set1_pd(4.5748122901933459000E-01), 
                                                                      _mm512_set1_pd(-4.2843766903304806403E-01), 
                                                                      _mm512_set1_pd(9.7356000150886612134E-02), 
                                                                      _mm512_set1_pd(-3.2457723974465568321E-03), 
                                                                      _mm512_set1_pd(-3.6395264712121795296E-04), 
                                                                      _mm512_set1_pd(1.6258661867440836395E-05), 
                                                                      _mm512_set1_pd(-3.6347578404608223492E-07)};
                           __attribute__((section(".rodata")))
			   __ATTR_ALIGN__(64) static __m512d q[5]   = {_mm512_set1_pd(-4.0076864679904189921E+03), 
                                                                      _mm512_set1_pd(7.4810580356655069138E+06), 
                                                                      _mm512_set1_pd(-8.0059518998619764991E+09), 
                                                                      _mm512_set1_pd(4.8544714258273622913E+12), 
                                                                      _mm512_set1_pd(-1.3218168307321442305E+15)};
                           __attribute__((section(".rodata")))
			   __ATTR_ALIGN__(64) static __m512d qq[6]  = {_mm512_set1_pd(-3.8806586721556593450E+00), 
                                                                      _mm512_set1_pd(3.2593714889036996297E+00), 
                                                                      _mm512_set1_pd(-8.5017476463217924408E-01), 
                                                                      _mm512_set1_pd(7.4212010813186530069E-02), 
                                                                      _mm512_set1_pd(-2.2835624489492512649E-03), 
                                                                      _mm512_set1_pd(3.7510433111922824643E-05)};
			   const __m512d exp40                     =  _mm512_set1_pd(2.353852668370199854E+17);
			   const __m512d _40                       =  _mm512_set1_pd(40.0);
			   const __m512d _1_2                      =  _mm512_set1_pd(0.5);
			   const __m512d _1                        =  _mm512_set1_pd(1.0);
			   const __m512d _15                       =  _mm512_set1_pd(15.0);
			   const __m512d pbar                      =  _mm512_set1_pd(3.98437500E-01);
			   const __m512d rec15                     =  _mm512_set1_pd(6.6666666666666666666E-02);
			   const __m512d _225                      =  _mm512_set1_pd(225.0);
			   const __m512d xmax                      =  _mm512_set1_pd(713.987E+00);
			   const __m512d _0                        =  _mm512_setzero_pd();
			   const __m512d eps                       =  _mm512_set1_pd(DBL_EPSILON);
			   const __m512d huge                      =  _mm512_set1_pd(DBL_MAX);
			   __m512d a,b,bessel_i1,value;
			   __m512d sump,sumq,x,xx;

			   x  = _mm512_abs_pd(arg);
			   if(_mm512_cmp_pd_mask(x,eps,_CMP_LT_OQ)) {
                               value = _mm512_mul_pd(_1_2,x);
			   }
			   else if(_mm512_cmp_pd_mask(x,_15,_CMP_LT_OQ)) {
                               xx   = _mm512_mul_pd(x,x);
			       sump = p[0];
			       sump = _mm512_fmadd_pd(sump,xx,p[1]);
			       sump = _mm512_fmadd_pd(sump,xx,p[2]);
			       sump = _mm512_fmadd_pd(sump,xx,p[3]);
			       sump = _mm512_fmadd_pd(sump,xx,p[4]);
			       sump = _mm512_fmadd_pd(sump,xx,p[5]);
			       sump = _mm512_fmadd_pd(sump,xx,p[6]);
			       sump = _mm512_fmadd_pd(sump,xx,p[7]);
			       sump = _mm512_fmadd_pd(sump,xx,p[8]);
			       sump = _mm512_fmadd_pd(sump,xx,p[9]);
			       sump = _mm512_fmadd_pd(sump,xx,p[10]);
			       sump = _mm512_fmadd_pd(sump,xx,p[11]);
			       sump = _mm512_fmadd_pd(sump,xx,p[12]);
			       sump = _mm512_fmadd_pd(sump,xx,p[13]);
			       sump = _mm512_fmadd_pd(sump,xx,p[14]);
			       xx   = _mm512_sub_pd(xx,_225);
			       const __m512d t0 = _mm512_fmadd_pd(_mm512_add_pd(xx,q[0]),xx,q[1]);
			       const __m512d t1 = _mm512_fmadd_pd(t0,xx,q[2]);
			       const __m512d t2 = _mm512_fmadd_pd(t1,xx,q[3]);
			       const __m512d t3 = _mm512_fmadd_pd(t2,xx,q[4]);
			       sumq             = t3;
			       value            = _mm512_mul_pd(_mm512_div_pd(sump,sumq),x);
			   }
			   else if(_mm512_cmp_pd_mask(xmax,x,_CMP_LT_OQ)) {
                               value            = huge;
			   }
			   else {
                               xx               = _mm512_sub_pd(_mm512_div_pd(_1,x),rec15);
			       const __m512d t0 = _mm512_fmadd_pd(pp[0],xx,pp[1]);
			       const __m512d c0 = _mm512_fmadd_pd(_mm512_add_pd(xx,qq[0]),xx,qq[1]);
			       const __m512d t1 = _mm512_fmadd_pd(t0,xx,pp[2]);
			       const __m512d c1 = _mm512_fmadd_pd(c0,xx,qq[2]);
			       const __m512d t2 = _mm512_fmadd_pd(t1,xx,pp[3]);
			       const __m512d c2 = _mm512_fmadd_pd(c1,xx,qq[3]);
			       const __m512d t3 = _mm512_fmadd_pd(t2,xx,pp[4]);
			       const __m512d c3 = _mm512_fmadd_pd(c2,xx,qq[4]);
			       const __m512d t4 = _mm512_fmadd_pd(t3,xx,pp[5]);
			       const __m512d c4 = _mm512_fmadd_pd(c3,xx,qq[5]);
			       const __m512d t5 = _mm512_fmadd_pd(t4,xx,pp[6]);
			       const __m512d c5 = _mm512_fmadd_pd(c4,xx,qq[6]);
			       const __m512d t6 = _mm512_fmadd_pd(t5,xx,pp[7]);
			       sump             = t6;
			       sumq             = c5;
			       value            = _mm512_div_pd(sump,sumq);
			       const __mmask8 m = _mm512_cmp_pd_mask(_mm512_sub_pd(xmax,_15),_CMP_LT_OQ);
#if (USE_SLEEF_LIB) == 1
                               a                = _mm512_mask_blend_pd(m,xexp(x),
			                                                           xexp(_mm512_sub_pd(x,_40)));
#else
			       a                = _mm512_mask_blend_pd(m,_mm512_exp_pd(x),
			                                                           _mm512_exp_pd(_mm512_sub_pd(x,_40)));
#endif
			       b                = _mm512_mask_blend_pd(m,_1,_40);
			       const __m512d tmp= _mm512_add_pd(_mm512_mul_pd(value,a),
			                                        _mm512_mul_pd(pbar,a));
			       value            = _mm512_mul_pd(_mm512_div_pd(tmp,_mm512_sqrt_pd(x)),b);
			   }
			   if(_mm512_cmp_pd_mask(arg,_0,_CMP_LT_OQ)) {
                              value             = zmm8r8_negate(value);
			   }
			   bessel_i1            = value
			   return (bessel_i1);
		    }




		      
		      __m512 bessel_i1_zmm16r4(const __m512 arg) {

                           __attribute__((section(".rodata")))
                           __ATTR_ALIGN__(64) static __m512  p[15] = {_mm512_set1_ps(-1.9705291802535139930E-19f), 
                                                                      _mm512_set1_ps(-6.5245515583151902910E-16f), 
                                                                      _mm512_set1_ps(-1.1928788903603238754E-12f), 
                                                                      _mm512_set1_ps(-1.4831904935994647675E-09f), 
                                                                      _mm512_set1_ps(-1.3466829827635152875E-06f), 
                                                                      _mm512_set1_ps(-9.1746443287817501309E-04f), 
                                                                      _mm512_set1_ps(-4.7207090827310162436E-01f), 
                                                                      _mm512_set1_ps(-1.8225946631657315931E+02f), 
                                                                      _mm512_set1_ps(-5.1894091982308017540E+04f), 
                                                                      _mm512_set1_ps(-1.0588550724769347106E+07f), 
                                                                      _mm512_set1_ps(-1.4828267606612366099E+09f), 
                                                                      _mm512_set1_ps(-1.3357437682275493024E+11f), 
                                                                      _mm512_set1_ps(-6.9876779648010090070E+12f), 
                                                                      _mm512_set1_ps(-1.7732037840791591320E+14f), 
                                                                      _mm512_set1_ps(-1.4577180278143463643E+15f)};
                           __attribute__((section(".rodata")))
			   __ATTR_ALIGN__(64) static __m512 pp[8]  = {_mm512_set1_ps(-6.0437159056137600000E-02f), 
                                                                      _mm512_set1_ps(4.5748122901933459000E-01f), 
                                                                      _mm512_set1_ps(-4.2843766903304806403E-01f), 
                                                                      _mm512_set1_ps(9.7356000150886612134E-02f), 
                                                                      _mm512_set1_ps(-3.2457723974465568321E-03f), 
                                                                      _mm512_set1_ps(-3.6395264712121795296E-04f), 
                                                                      _mm512_set1_ps(1.6258661867440836395E-05f), 
                                                                      _mm512_set1_ps(-3.6347578404608223492E-07f)};
                           __attribute__((section(".rodata")))
			   __ATTR_ALIGN__(64) static __m512 q[5]   = {_mm512_set1_ps(-4.0076864679904189921E+03f), 
                                                                      _mm512_set1_ps(7.4810580356655069138E+06f), 
                                                                      _mm512_set1_ps(-8.0059518998619764991E+09f), 
                                                                      _mm512_set1_ps(4.8544714258273622913E+12f), 
                                                                      _mm512_set1_ps(-1.3218168307321442305E+15f)};
                           __attribute__((section(".rodata")))
			   __ATTR_ALIGN__(64) static __m512 qq[6]  = {_mm512_set1_ps(-3.8806586721556593450E+00f), 
                                                                      _mm512_set1_ps(3.2593714889036996297E+00f), 
                                                                      _mm512_set1_ps(-8.5017476463217924408E-01f), 
                                                                      _mm512_set1_ps(7.4212010813186530069E-02f), 
                                                                      _mm512_set1_ps(-2.2835624489492512649E-03f), 
                                                                      _mm512_set1_ps(3.7510433111922824643E-05f)};
			   const __m512 exp40                     =  _mm512_set1_ps(2.353852668370199854E+17f);
			   const __m512 _40                       =  _mm512_set1_ps(40.0f);
			   const __m512 _1_2                      =  _mm512_set1_ps(0.5f);
			   const __m512 _1                        =  _mm512_set1_ps(1.0f);
			   const __m512 _15                       =  _mm512_set1_ps(15.0f);
			   const __m512 pbar                      =  _mm512_set1_ps(3.98437500E-01f);
			   const __m512 rec15                     =  _mm512_set1_ps(6.6666666666666666666E-02f);
			   const __m512 _225                      =  _mm512_set1_ps(225.0f);
			   const __m512 xmax                      =  _mm512_set1_ps(713.987E+00f);
			   const __m512 _0                        =  _mm512_setzero_ps();
			   const __m512 eps                       =  _mm512_set1_ps(FLT_EPSILON);
			   const __m512 huge                      =  _mm512_set1_ps(FLT_MAX);
			   __m512 a,b,bessel_i1,value;
			   __m512 sump,sumq,x,xx;

			   x  = _mm512_abs_ps(arg);
			   if(_mm512_cmp_ps_mask(x,eps,_CMP_LT_OQ)) {
                               value = _mm512_mul_ps(_1_2,x);
			   }
			   else if(_mm512_cmp_ps_mask(x,_15,_CMP_LT_OQ)) {
                               xx   = _mm512_mul_ps(x,x);
			       sump = p[0];
			       sump = _mm512_fmadd_ps(sump,xx,p[1]);
			       sump = _mm512_fmadd_ps(sump,xx,p[2]);
			       sump = _mm512_fmadd_ps(sump,xx,p[3]);
			       sump = _mm512_fmadd_ps(sump,xx,p[4]);
			       sump = _mm512_fmadd_ps(sump,xx,p[5]);
			       sump = _mm512_fmadd_ps(sump,xx,p[6]);
			       sump = _mm512_fmadd_ps(sump,xx,p[7]);
			       sump = _mm512_fmadd_ps(sump,xx,p[8]);
			       sump = _mm512_fmadd_ps(sump,xx,p[9]);
			       sump = _mm512_fmadd_ps(sump,xx,p[10]);
			       sump = _mm512_fmadd_ps(sump,xx,p[11]);
			       sump = _mm512_fmadd_ps(sump,xx,p[12]);
			       sump = _mm512_fmadd_ps(sump,xx,p[13]);
			       sump = _mm512_fmadd_ps(sump,xx,p[14]);
			       xx   = _mm512_sub_ps(xx,_225);
			       const __m512 t0 = _mm512_fmadd_ps(_mm512_add_ps(xx,q[0]),xx,q[1]);
			       const __m512 t1 = _mm512_fmadd_ps(t0,xx,q[2]);
			       const __m512 t2 = _mm512_fmadd_ps(t1,xx,q[3]);
			       const __m512 t3 = _mm512_fmadd_ps(t2,xx,q[4]);
			       sumq             = t3;
			       value            = _mm512_mul_ps(_mm512_div_ps(sump,sumq),x);
			   }
			   else if(_mm512_cmp_ps_mask(xmax,x,_CMP_LT_OQ)) {
                               value            = huge;
			   }
			   else {
                               xx               = _mm512_sub_ps(_mm512_div_ps(_1,x),rec15);
			       const __m512 t0 = _mm512_fmadd_ps(pp[0],xx,pp[1]);
			       const __m512 c0 = _mm512_fmadd_ps(_mm512_add_ps(xx,qq[0]),xx,qq[1]);
			       const __m512 t1 = _mm512_fmadd_ps(t0,xx,pp[2]);
			       const __m512 c1 = _mm512_fmadd_ps(c0,xx,qq[2]);
			       const __m512 t2 = _mm512_fmadd_ps(t1,xx,pp[3]);
			       const __m512 c2 = _mm512_fmadd_ps(c1,xx,qq[3]);
			       const __m512 t3 = _mm512_fmadd_ps(t2,xx,pp[4]);
			       const __m512 c3 = _mm512_fmadd_ps(c2,xx,qq[4]);
			       const __m512 t4 = _mm512_fmadd_ps(t3,xx,pp[5]);
			       const __m512 c4 = _mm512_fmadd_ps(c3,xx,qq[5]);
			       const __m512 t5 = _mm512_fmadd_ps(t4,xx,pp[6]);
			       const __m512 c5 = _mm512_fmadd_ps(c4,xx,qq[6]);
			       const __m512 t6 = _mm512_fmadd_ps(t5,xx,pp[7]);
			       sump             = t6;
			       sumq             = c5;
			       value            = _mm512_div_ps(sump,sumq);
			       const __mmask16 m = _mm512_cmp_ps_mask(_mm512_sub_ps(xmax,_15),_CMP_LT_OQ);
#if (USE_SLEEF_LIB) == 1
                               a                = _mm512_mask_blend_ps(m,xexpf(x),
			                                                           xexpf(_mm512_sub_ps(x,_40)));
#else
			       a                = _mm512_mask_blend_ps(m,_mm512_exp_ps(x),
			                                                           _mm512_exp_ps(_mm512_sub_ps(x,_40)));
#endif
			       b                = _mm512_mask_blend_ps(m,_1,_40);
			       const __m512 tmp= _mm512_add_ps(_mm512_mul_ps(value,a),
			                                        _mm512_mul_ps(pbar,a));
			       value            = _mm512_mul_ps(_mm512_div_ps(tmp,_mm512_sqrt_ps(x)),b);
			   }
			   if(_mm512_cmp_ps_mask(arg,_0,_CMP_LT_OQ)) {
                              value             = zmm16r4_negate(value);
			   }
			   bessel_i1            = value
			   return (bessel_i1);
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


                    
		      __m512d  
		      normal_01_cdf_zmm8r8(const __m512d x) {
		          
		          __m512d a1 = _mm512_set1_pd(0.398942280444e+00);
		          __m512d a2 = _mm512_set1_pd(0.399903438504e+00);
		          __m512d a3 = _mm512_set1_pd(5.75885480458e+00);
                          __m512d a4 = _mm512_set1_pd(29.8213557808e+00);
                          __m512d a5 = _mm512_set1_pd(2.62433121679e+00);
                          __m512d a6 = _mm512_set1_pd(48.6959930692e+00);
                          __m512d a7 = _mm512_set1_pd(5.92885724438e+00);
                          __m512d b0 = _mm512_set1_pd(0.398942280385e+00);
                          __m512d b1 = _mm512_set1_pd(3.8052e-08);
                          __m512d b2 = _mm512_set1_pd(1.00000615302e+00);
                          __m512d b3 = _mm512_set1_pd(3.98064794e-04);
                          __m512d b4 = _mm512_set1_pd(1.98615381364e+00);
                          __m512d b5 = _mm512_set1_pd(0.151679116635e+00);
                          __m512d b6 = _mm512_set1_pd(5.29330324926e+00);
                          __m512d b7 = _mm512_set1_pd(4.8385912808e+00);
                          __m512d b8 = _mm512_set1_pd(15.1508972451e+00);
                          __m512d b9 = _mm512_set1_pd(0.742380924027e+00);
                          __m512d b10= _mm512_set1_pd(30.789933034e+00);
                          __m512d b11= _mm512_set1_pd(3.99019417011e+00);
                          __m512d C1 = _mm512_set1_pd(1.0);
                          __m512d C128 = _mm512_set1_pd(1.28);
                          __m512d C05  = _mm512_set1_pd(0.5);
                          __m512d C127 = _mm512_set1_pd(12.7);
                          __m512d absx,y,q,cdf,t0,t1;
                          __mmask8 m0,m1,m2;
                          m2   = _mm512_cmp_pd_mask(x,_mm512_setzero_pd(),_CMP_LT_OQ);
                          absx = _mm512_abs_pd(x);
                          m0   = _mm512_cmp_pd_mask(x,C128,_CMP_LE_OQ);
                          y    = _mm512_mul_pd(C05,
                                        _mm512_mul_pd(x,x));
                          m1   = _mm512_cmp_pd_mask(x,C127,_CMP_LE_OQ);
                          if(m0) {
                             register __m512d ya3;
                             register __m512d ya5a6
                             register __m512d ya7;
                             register __m512d a2y;
                             ya7   = _mm512_add_pd(y,a7);
                             ya5a6 = _mm512_add_pd(y,_mm512_add_pd(a5,a6));
                             a2y   = _mm512_mul_pd(a2,y);
                             ya3a4 = _mm512_sub_pd(_mm512_add_pd(y,a3),a4);
                             q     = _mm512_sub_pd(a1,
                                           _mm512_div_pd(a2y,
                                                  _mm512_div_pd(ya3a4,
                                                        _mm512_div_pd(ya5a6,ya7))));
                          }
                          else if(m1) {
                             register __m512d expmy;
                             register __m512d absb1;
                             register __m512d absb3;
                             register __m512d absb5;
                             register __m512d absb7;
                             register __m512d absb9;
                             register __m512d absb11;
#if (USE_SLEEF_LIB) == 1                          
                             expmy = _mm512_mul_pd(xexp(negate_zmm8r8(y)),b0);
#else
                             expmy = _mm512_mul_pd(_mm512_exp_pd(negate_zmm8r8(y)),b0); 
#endif 
                             absb1 = _mm512_sub_pd(absx,b1);
                             absb3 = _mm512_add_pd(absx,b3);
                             absb5 = _mm512_sub_pd(absx,b5);
                             absb7 = _mm512_add_pd(absx,b7);
                             absb9 = _mm512_add_pd(absx,b9);
                             absb11= _mm512_add_pd(absx,b11);
                             t0    = (absb1+b2/(absb3+b4/(absb5+b6/(absb7-b8/(absb9+b10/(absb11))))));
                             q     = _mm512_div_pd(expmy,t0);
                          }
                          else {
                             q = _mm512_setzero_pd();
                          }
                          
                          cdf = _mm512_mask_blend_pd(m2,_mm512_sub_pd(C1,q),q);
                          return (cdf);
		    }
		    
		      
		      __m512  
		      normal_01_cdf_zmm16r4(const __m512 x) {
		          
		          __m512 a1 = _mm512_set1_ps(0.398942280444f);
		          __m512 a2 = _mm512_set1_ps(0.399903438504f);
		          __m512 a3 = _mm512_set1_ps(5.75885480458f);
                          __m512 a4 = _mm512_set1_ps(29.8213557808f);
                          __m512 a5 = _mm512_set1_ps(2.62433121679f);
                          __m512 a6 = _mm512_set1_ps(48.6959930692f);
                          __m512 a7 = _mm512_set1_ps(5.92885724438f);
                          __m512 b0 = _mm512_set1_ps(0.398942280385f);
                          __m512 b1 = _mm512_set1_ps(3.8052e-08f);
                          __m512 b2 = _mm512_set1_ps(1.00000615302f);
                          __m512 b3 = _mm512_set1_ps(3.98064794e-04f);
                          __m512 b4 = _mm512_set1_ps(1.98615381364f);
                          __m512 b5 = _mm512_set1_ps(0.151679116635f);
                          __m512 b6 = _mm512_set1_ps(5.29330324926f);
                          __m512 b7 = _mm512_set1_ps(4.8385912808f);
                          __m512 b8 = _mm512_set1_ps(15.1508972451f);
                          __m512 b9 = _mm512_set1_ps(0.742380924027f);
                          __m512 b10= _mm512_set1_ps(30.789933034f);
                          __m512 b11= _mm512_set1_ps(3.99019417011f);
                          __m512 C1 = _mm512_set1_ps(1.0);
                          __m512 C128 = _mm512_set1_ps(1.28f);
                          __m512 C05  = _mm512_set1_ps(0.5f);
                          __m512 C127 = _mm512_set1_ps(12.7f);
                          __m512 absx,y,q,cdf,t0;
                          __mmask16 m0,m1,m2;
                          m2   = _mm512_cmp_ps_mask(x,_mm512_setzero_pd(),_CMP_LT_OQ);
                          absx = _mm512_abs_ps(x);
                          m0   = _mm512_cmp_ps_mask(x,C128,_CMP_LE_OQ);
                          y    = _mm512_mul_ps(C05,
                                        _mm512_mul_ps(x,x));
                          m1   = _mm512_cmp_ps_mask(x,C127,_CMP_LE_OQ);
                          if(m0) {
                             register __m512 ya3;
                             register __m512 ya5a6
                             register __m512 ya7;
                             register __m512 a2y;
                             ya7   = _mm512_add_ps(y,a7);
                             ya5a6 = _mm512_add_ps(y,_mm512_add_ps(a5,a6));
                             a2y   = _mm512_mul_ps(a2,y);
                             ya3a4 = _mm512_sub_ps(_mm512_add_ps(y,a3),a4);
                             q     = _mm512_sub_ps(a1,
                                           _mm512_div_ps(a2y,
                                                  _mm512_div_ps(ya3a4,
                                                        _mm512_div_ps(ya5a6,ya7))));
                          }
                          else if(m1) {
                             register __m512 expmy;
                             register __m512 absb1;
                             register __m512 absb3;
                             register __m512 absb5;
                             register __m512 absb7;
                             register __m512 absb9;
                             register __m512 absb11;
#if (USE_SLEEF_LIB) == 1                          
                             expmy = _mm512_mul_ps(xexpf(negate_zmm16r4(y)),b0);
#else
                             expmy = _mm512_mul_ps(_mm512_exp_ps(negate_zmm16r4(y)),b0); 
#endif 
                             absb1 = _mm512_sub_ps(absx,b1);
                             absb3 = _mm512_add_ps(absx,b3);
                             absb5 = _mm512_sub_ps(absx,b5);
                             absb7 = _mm512_add_ps(absx,b7);
                             absb9 = _mm512_add_ps(absx,b9);
                             absb11= _mm512_add_ps(absx,b11);
                             t0    = (absb1+b2/(absb3+b4/(absb5+b6/(absb7-b8/(absb9+b10/(absb11))))));
                             q     = _mm512_div_ps(expmy,t0);
                          }
                          else {
                             q = _mm512_setzero_ps();
                          }
                          
                          cdf = _mm512_mask_blend_ps(m2,_mm512_sub_ps(C1,q),q);
                          return (cdf);
		    }
		    
		    
		 /*
          !*****************************************************************************80
!
!! NORMAL_01_SAMPLE samples the standard normal probability distribution.
!
!  Discussion:
!
!    The standard normal probability distribution function (PDF) has
!    mean 0 and standard deviation 1.
!
!    The Box-Muller method is used, which is efficient, but
!    generates two values at a time.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 February 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random number
!    generator.
!
!    Output, real ( kind = 8 ) X, a sample of the standard normal PDF.
!
*/
	
	
	              
		      __m512d   	    
		      normal_01_sample_zmm8r8(__m512i * __restrict seed) {
		           
		           __m512d CN20 = _mm512_set1_pd(-2.0);
		           __m512d C628318530717958647692529  = 
		                                    _mm512_set1_pd(6.28318530717958647692529);
		           __m512d C00  = _mm512_setzero_pd();
		           register __m512d r1,r2,s,c,t0,t1,arg;
		           register __m512d x;
		           static __m512d y = _mm512_setzero_pd();
		           static int32_t used = -1;
		           if(used == -1) used = 0;
		           
		           if((used%2)==0) {
		           
		                 while(true) {
		                    r1 = uniform_01_zmm8r8(*seed);
		                    if(_mm512_cmp_pd_mask(r1,C00,_CMP_NE_OQ)) break;
		               }
		               r2 = uniform_01_zmm8r8(*seed);
		               arg= _mm512_mul_pd(r2,C628318530717958647692529)
#if (USE_SLEEF_LIB) == 1       
                               t0 = _mm512_mul_pd(CN20,xlog(r1));
                               c  = xcos(arg);
                               s  = xsin(arg);	
#else
                               t0 = _mm512_mul_pd(CN20,_mm512_log_pd(r1));
                               c  = _mm512_cos_pd(arg);
                               s  = _mm512_sin_pd(arg);
#endif                           	               
                               x  = _mm512_mul_pd(_mm512_sqrt_pd(t0),c);
                               y  = _mm512_mul_pd(_mm512_sqrt_pd(t0),s);
		           }
		           else {
		               x = y;
		           }
		           used += 1;
		           return (x);
		     }
		     
		     
		     
		      __m512  	    
		      normal_01_sample_zmm16r4(__m512i * __restrict seed) {
		          
		          return (_mm512_castpd_ps(normal_01_sample_zmm8r8(*seed)));
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


                    
		      __m512d 
		      reciprocal_cdf_zmm8r8(const __m512d x,
		                            const __m512d a,
		                            const __m512d b) {
		          
		          register __m512d ax,ab,l1,l2;
		          register __m512d cdf;       
		          ax = _mm512_div_pd(a,x);
#if (USE_SLEEF_LIB) == 1   
                          l1 = xlog(ax);
#else
                          l1 = _mm512_log_pd(ax);
#endif		                         
                          ab = _mm512_div_pd(a,b);
#if (USE_SLEEF_LIB) == 1 
                          l2 = xlog(ab);
#else
                          l2 = _mm512_log_pd(ab);
#endif                          
                          cdf= _mm512_div_pd(l1,l2);
                          return (cdf);
		     }
		     
		     
		     
		      __m512 
		      reciprocal_cdf_zmm16r4(const __m512 x,
		                             const __m512 a,
		                             const __m512 b) {
		          
		          register __m512 ax,ab,l1,l2;
		          register __m512 cdf;       
		          ax = _mm512_div_ps(a,x);
#if (USE_SLEEF_LIB) == 1   
                          l1 = xlogf(ax);
#else
                          l1 = _mm512_log_ps(ax);
#endif		                         
                          ab = _mm512_div_ps(a,b);
#if (USE_SLEEF_LIB) == 1 
                          l2 = xlogf(ab);
#else
                          l2 = _mm512_log_ps(ab);
#endif                          
                          cdf= _mm512_div_ps(l1,l2);
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


                    
		      __m512d 		     
		      reciprocal_cdf_inv_zmm8r8(const __m512d cdf,
		                                const __m512d a,
		                                const __m512d b) {
		         
		           register __m512d C1 = _mm512_set1_pd(1.0);
		           register __m512d pow1,pow2,cdf1;
		           register __m512d inv;
		           cdf1 = _mm512_sub_pd(cdf,C1);
		           pow2 = _mm512_pow_pd(b,cdf);
		           pow1 = _mm512_pow_pd(a,cdf1);
		           inv  = _mm512_div_pd(pow2,pow1);
		           return (inv);                          
		     }
		     
		     
		      
		      __m512		     
		      reciprocal_cdf_inv_zmm16r4(const __m512 cdf,
		                                const __m512 a,
		                                const __m512 b) {
		         
		           register __m512 C1 = _mm512_set1_ps(1.0f);
		           register __m512 pow1,pow2,cdf1;
		           register __m512 inv;
		           cdf1 = _mm512_sub_ps(cdf,C1);
		           pow2 = _mm512_pow_ps(b,cdf);
		           pow1 = _mm512_pow_ps(a,cdf1);
		           inv  = _mm512_div_ps(pow2,pow1);
		           return (inv);                          
		     }
		     		     
		     
		  
		  
	

