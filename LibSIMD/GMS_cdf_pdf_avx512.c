
#include <float.h>
#include "GMS_cdf_pdf_avx512.h"
#if (CDF_PDF_USE_SLEEF_LIB) == 1
#include "GMS_sleefsimddp.h"
#include "GMS_sleefsimdsp.h"
#endif
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

                     
		      __m512d gamma_log_zmm8r8(const __m512d x) {
		      
                            if(__builtin_expect(_mm512_cmp_pd_mask(x,_0,_CMP_LE_OQ),0) ||
			       __builtin_expect(_mm512_cmp_pd_mask(x,xbig,_CMP_GT_OQ),0)) {
                               const __m512d huge   = _mm512_set1_pd(DBL_MAX);
                               return (huge);
			    }
 
                         __ATTR_ALIGN__(64) const __m512d c[7] = { _mm512_set1_pd(-1.910444077728E-03),
			                                     _mm512_set1_pd(8.4171387781295E-04),
                                                             _mm512_set1_pd(-5.952379913043012E-04), 
                                                             _mm512_set1_pd(7.93650793500350248E-04), 
                                                             _mm512_set1_pd(-2.777777777777681622553E-03), 
                                                             _mm512_set1_pd(8.333333333333333331554247E-02), 
                                                             _mm512_set1_pd(5.7083835261E-03)};
			 __ATTR_ALIGN__(64) const __m512d p1[8] = {_mm512_set1_pd(4.945235359296727046734888E+00), 
                                                             _mm512_set1_pd(2.018112620856775083915565E+02), 
                                                             _mm512_set1_pd(2.290838373831346393026739E+03), 
                                                             _mm512_set1_pd(1.131967205903380828685045E+04),
                                                             _mm512_set1_pd(2.855724635671635335736389E+04), 
                                                             _mm512_set1_pd(3.848496228443793359990269E+04), 
                                                             _mm512_set1_pd(2.637748787624195437963534E+04), 
                                                             _mm512_set1_pd(7.225813979700288197698961E+03)};
			 __ATTR_ALIGN__(64) const __m512d p2[8] = {_mm512_set1_pd(4.974607845568932035012064E+00), 
                                                             _mm512_set1_pd(5.424138599891070494101986E+02), 
                                                             _mm512_set1_pd(1.550693864978364947665077E+04), 
                                                             _mm512_set1_pd(1.847932904445632425417223E+05), 
                                                             _mm512_set1_pd(1.088204769468828767498470E+06), 
                                                             _mm512_set1_pd(3.338152967987029735917223E+06), 
                                                             _mm512_set1_pd(5.106661678927352456275255E+06), 
                                                             _mm512_set1_pd(3.074109054850539556250927E+06)};
			 __ATTR_ALIGN__(64) const __m512d p4[8] = {_mm512_set1_pd(1.474502166059939948905062E+04), 
                                                             _mm512_set1_pd(2.426813369486704502836312E+06), 
                                                             _mm512_set1_pd(1.214755574045093227939592E+08), 
                                                             _mm512_set1_pd(2.663432449630976949898078E+09), 
                                                             _mm512_set1_pd(2.940378956634553899906876E+10), 
                                                             _mm512_set1_pd(1.702665737765398868392998E+11), 
                                                             _mm512_set1_pd(4.926125793377430887588120E+11), 
                                                             _mm512_set1_pd(5.606251856223951465078242E+11)};
                         __ATTR_ALIGN__(64) const __m512d q1[8] = {_mm512_set1_pd(6.748212550303777196073036E+01), 
                                                             _mm512_set1_pd(1.113332393857199323513008E+03), 
                                                             _mm512_set1_pd(7.738757056935398733233834E+03), 
                                                             _mm512_set1_pd(2.763987074403340708898585E+04), 
                                                             _mm512_set1_pd(5.499310206226157329794414E+04), 
                                                             _mm512_set1_pd(6.161122180066002127833352E+04), 
                                                             _mm512_set1_pd(3.635127591501940507276287E+04), 
                                                             _mm512_set1_pd(8.785536302431013170870835E+03)};
			 __ATTR_ALIGN__(64) const __m512d q2[8] = {_mm512_set1_pd(1.830328399370592604055942E+02),
                                                             _mm512_set1_pd(7.765049321445005871323047E+03), 
                                                             _mm512_set1_pd(1.331903827966074194402448E+05),
                                                             _mm512_set1_pd(1.136705821321969608938755E+06), 
                                                             _mm512_set1_pd(5.267964117437946917577538E+06), 
                                                             _mm512_set1_pd(1.346701454311101692290052E+07), 
                                                             _mm512_set1_pd(1.782736530353274213975932E+07), 
                                                             _mm512_set1_pd(9.533095591844353613395747E+06)};
			 __ATTR_ALIGN__(64) const __m512d q4[8] = {_mm512_set1_pd(2.690530175870899333379843E+03), 
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
		      
                            if(__builtin_expect(_mm512_cmp_ps_mask(x,_0,_CMP_LE_OQ),0) ||
			       __builtin_expect(_mm512_cmp_ps_mask(x,xbig,_CMP_GT_OQ),0)) {
                                const __m512 huge   = _mm512_set1_ps(FLT_MAX);
                               return (huge);
			    }
 
                         __ATTR_ALIGN__(64) const __m512 c[7] = { _mm512_set1_ps(-1.910444077728E-03),
			                                     _mm512_set1_ps(8.4171387781295E-04),
                                                             _mm512_set1_ps(-5.952379913043012E-04), 
                                                             _mm512_set1_ps(7.93650793500350248E-04), 
                                                             _mm512_set1_ps(-2.777777777777681622553E-03), 
                                                             _mm512_set1_ps(8.333333333333333331554247E-02), 
                                                             _mm512_set1_ps(5.7083835261E-03)};
			 __ATTR_ALIGN__(64) const __m512 p1[8] = {_mm512_set1_ps(4.945235359296727046734888E+00), 
                                                             _mm512_set1_ps(2.018112620856775083915565E+02), 
                                                             _mm512_set1_ps(2.290838373831346393026739E+03), 
                                                             _mm512_set1_ps(1.131967205903380828685045E+04),
                                                             _mm512_set1_ps(2.855724635671635335736389E+04), 
                                                             _mm512_set1_ps(3.848496228443793359990269E+04), 
                                                             _mm512_set1_ps(2.637748787624195437963534E+04), 
                                                             _mm512_set1_ps(7.225813979700288197698961E+03)};
			 __ATTR_ALIGN__(64) const __m512 p2[8] = {_mm512_set1_ps(4.974607845568932035012064E+00), 
                                                             _mm512_set1_ps(5.424138599891070494101986E+02), 
                                                             _mm512_set1_ps(1.550693864978364947665077E+04), 
                                                             _mm512_set1_ps(1.847932904445632425417223E+05), 
                                                             _mm512_set1_ps(1.088204769468828767498470E+06), 
                                                             _mm512_set1_ps(3.338152967987029735917223E+06), 
                                                             _mm512_set1_ps(5.106661678927352456275255E+06), 
                                                             _mm512_set1_ps(3.074109054850539556250927E+06)};
			 __ATTR_ALIGN__(64) const __m512 p4[8] = {_mm512_set1_ps(1.474502166059939948905062E+04), 
                                                             _mm512_set1_ps(2.426813369486704502836312E+06), 
                                                             _mm512_set1_ps(1.214755574045093227939592E+08), 
                                                             _mm512_set1_ps(2.663432449630976949898078E+09), 
                                                             _mm512_set1_ps(2.940378956634553899906876E+10), 
                                                             _mm512_set1_ps(1.702665737765398868392998E+11), 
                                                             _mm512_set1_ps(4.926125793377430887588120E+11), 
                                                             _mm512_set1_ps(5.606251856223951465078242E+11)};
                         __ATTR_ALIGN__(64) const __m512 q1[8] = {_mm512_set1_ps(6.748212550303777196073036E+01), 
                                                             _mm512_set1_ps(1.113332393857199323513008E+03), 
                                                             _mm512_set1_ps(7.738757056935398733233834E+03), 
                                                             _mm512_set1_ps(2.763987074403340708898585E+04), 
                                                             _mm512_set1_ps(5.499310206226157329794414E+04), 
                                                             _mm512_set1_ps(6.161122180066002127833352E+04), 
                                                             _mm512_set1_ps(3.635127591501940507276287E+04), 
                                                             _mm512_set1_ps(8.785536302431013170870835E+03)};
			 __ATTR_ALIGN__(64) const __m512 q2[8] = {_mm512_set1_ps(1.830328399370592604055942E+02),
                                                             _mm512_set1_ps(7.765049321445005871323047E+03), 
                                                             _mm512_set1_ps(1.331903827966074194402448E+05),
                                                             _mm512_set1_ps(1.136705821321969608938755E+06), 
                                                             _mm512_set1_ps(5.267964117437946917577538E+06), 
                                                             _mm512_set1_ps(1.346701454311101692290052E+07), 
                                                             _mm512_set1_ps(1.782736530353274213975932E+07), 
                                                             _mm512_set1_ps(9.533095591844353613395747E+06)};
			 __ATTR_ALIGN__(64) const __m512 q4[8] = {_mm512_set1_ps(2.690530175870899333379843E+03), 
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
			   
			    const __m512 eps    = _mm512_set1_ps(FLT_EPS);
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

                            __ATTR_ALIGN__(64) const __m512d p[15] = {_mm512_set1_pd(-5.2487866627945699800E-18),
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
			    __ATTR_ALIGN__(64) const __m512d pp[8] = {_mm512_set1_pd(-3.9843750000000000000E-01), 
                                                                      _mm512_set1_pd(2.9205384596336793945E+00), 
                                                                      _mm512_set1_pd(-2.4708469169133954315E+00), 
                                                                      _mm512_set1_pd(4.7914889422856814203E-01), 
                                                                      _mm512_set1_pd(-3.7384991926068969150E-03), 
                                                                      _mm512_set1_pd(-2.6801520353328635310E-03), 
                                                                      _mm512_set1_pd(9.9168777670983678974E-05), 
                                                                      _mm512_set1_pd(-2.1877128189032726730E-06)};
			    __ATTR_ALIGN__(64) const __m512d q[5]  = {_mm512_set1_pd(-3.7277560179962773046E+03), 
                                                                      _mm512_set1_pd(6.5158506418655165707E+06), 
                                                                      _mm512_set1_pd(-6.5626560740833869295E+09), 
                                                                      _mm512_set1_pd(3.7604188704092954661E+12), 
                                                                      _mm512_set1_pd(-9.7087946179594019126E+14)};
			    __ATTR_ALIGN__(64) const __m512d qq[7] = {_mm512_set1_pd(-3.1446690275135491500E+01), 
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
			    const __m512d eps                      =  _mm512_set1_pd(DBL_EPS);
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

                            __ATTR_ALIGN__(64) const __m512 p[15] = {_mm512_set1_ps(-5.2487866627945699800E-18f),
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
			    __ATTR_ALIGN__(64) const __m512 pp[8] = {_mm512_set1_ps(-3.9843750000000000000E-01f), 
                                                                      _mm512_set1_ps(2.9205384596336793945E+00f), 
                                                                      _mm512_set1_ps(-2.4708469169133954315E+00f), 
                                                                      _mm512_set1_ps(4.7914889422856814203E-01f), 
                                                                      _mm512_set1_ps(-3.7384991926068969150E-03f), 
                                                                      _mm512_set1_ps(-2.6801520353328635310E-03f), 
                                                                      _mm512_set1_ps(9.9168777670983678974E-05f), 
                                                                      _mm512_set1_ps(-2.1877128189032726730E-06f)};
			    __ATTR_ALIGN__(64) const __m512 q[5]  = {_mm512_set1_ps(-3.7277560179962773046E+03f), 
                                                                      _mm512_set1_ps(6.5158506418655165707E+06f), 
                                                                      _mm512_set1_ps(-6.5626560740833869295E+09f), 
                                                                      _mm512_set1_ps(3.7604188704092954661E+12f), 
                                                                      _mm512_set1_ps(-9.7087946179594019126E+14f)};
			    __ATTR_ALIGN__(64) const __m512 qq[7] = {_mm512_set1_ps(-3.1446690275135491500E+01f), 
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
			    const __m512 eps                      =  _mm512_set1_pd(FLT_EPS);
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
#if (CDF_PDF_USE_SLEEF_LIB) == 1
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

                           __ATTR_ALIGN__(64) const __m512d  p[15] = {_mm512_set1_pd(-1.9705291802535139930E-19), 
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
			   __ATTR_ALIGN__(64) const __m512d pp[8]  = {_mm512_set1_pd(-6.0437159056137600000E-02), 
                                                                      _mm512_set1_pd(4.5748122901933459000E-01), 
                                                                      _mm512_set1_pd(-4.2843766903304806403E-01), 
                                                                      _mm512_set1_pd(9.7356000150886612134E-02), 
                                                                      _mm512_set1_pd(-3.2457723974465568321E-03), 
                                                                      _mm512_set1_pd(-3.6395264712121795296E-04), 
                                                                      _mm512_set1_pd(1.6258661867440836395E-05), 
                                                                      _mm512_set1_pd(-3.6347578404608223492E-07)};
			   __ATTR_ALIGN__(64) const __m512d q[5]   = {_mm512_set1_pd(-4.0076864679904189921E+03), 
                                                                      _mm512_set1_pd(7.4810580356655069138E+06), 
                                                                      _mm512_set1_pd(-8.0059518998619764991E+09), 
                                                                      _mm512_set1_pd(4.8544714258273622913E+12), 
                                                                      _mm512_set1_pd(-1.3218168307321442305E+15)};
			   __ATTR_ALIGN__(64) const __m512d qq[6]  = {_mm512_set1_pd(-3.8806586721556593450E+00), 
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
			   const __m512d eps                       =  _mm512_set1_pd(DBL_EPS);
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
#if (CDF_PDF_USE_SLEEF_LIB) == 1
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

                           __ATTR_ALIGN__(64) const __m512  p[15] = {_mm512_set1_ps(-1.9705291802535139930E-19f), 
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
			   __ATTR_ALIGN__(64) const __m512 pp[8]  = {_mm512_set1_ps(-6.0437159056137600000E-02f), 
                                                                      _mm512_set1_ps(4.5748122901933459000E-01f), 
                                                                      _mm512_set1_ps(-4.2843766903304806403E-01f), 
                                                                      _mm512_set1_ps(9.7356000150886612134E-02f), 
                                                                      _mm512_set1_ps(-3.2457723974465568321E-03f), 
                                                                      _mm512_set1_ps(-3.6395264712121795296E-04f), 
                                                                      _mm512_set1_ps(1.6258661867440836395E-05f), 
                                                                      _mm512_set1_ps(-3.6347578404608223492E-07f)};
			   __ATTR_ALIGN__(64) const __m512 q[5]   = {_mm512_set1_ps(-4.0076864679904189921E+03f), 
                                                                      _mm512_set1_ps(7.4810580356655069138E+06f), 
                                                                      _mm512_set1_ps(-8.0059518998619764991E+09f), 
                                                                      _mm512_set1_ps(4.8544714258273622913E+12f), 
                                                                      _mm512_set1_ps(-1.3218168307321442305E+15f)};
			   __ATTR_ALIGN__(64) const __m512 qq[6]  = {_mm512_set1_ps(-3.8806586721556593450E+00f), 
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
			   const __m512 eps                       =  _mm512_set1_ps(FLT_EPS);
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
#if (CDF_PDF_USE_SLEEF_LIB) == 1
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

		    
		      __m512d beta_zmm8r8(const __m512d a,
		                          const __m512d b) {

                        //const __m512d nan = _mm512_set1_pd(std::numeric_limits<double>::quiet_NaN());
			const __m512d _0  = _mm512_setzero_pd();
			if(__builtin_expect(_mm512_cmp_pd_mask(a,_0,_CMP_LE_OQ),0) ||
			   __builtin_expect(_mm512_cmp_pd_mask(b,_0,_CMP_LE_OQ),0)) {
                           return (_0);
			}
			const __m512d ab  = _mm512_add_pd(a,b);
			__m512d beta      = _mm512_setzero_pd();
			beta              = _mm512_exp_pd(
			                              _mm512_sub_pd(
						                 _mm512_add_pd(gamma_log_zmm8r8(a),
								               gamma_log_zmm8r8(b)),
			return (beta);						                     gamma_log_zmm8r8(ab)));
		    }
                 







/*!*****************************************************************************80
!
!! ANGLIT_SAMPLE samples the Anglit PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
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
/*
                      __ATTR_REGCALL__
                      __ATTR_ALWAYS_INLINE__
		      __ATTR_HOT__
		      __ATTR_ALIGN__(32)
		      static inline
		      __m512d anglit_sample_zmm8r8() {
                         __m512d cdf;
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
                            const __m512d nan = _mm512_set1_pd(std::numeric_limits<double>::quiet_NaN());
			    return (nan);
			 }
			 uniform          = svrng_new_uniform_distribution_double(0.0,1.0);
			 const double * __restrict ptr = (const double*)(&svrng_generate8_double(engine,uniform));
			 cdf              = anglit_cdf_inv_zmm8r8(_mm512_loadu_pd(&ptr[0]));
			 svrng_delete_engine(engine);
			 return (cdf);
		    }
		      __ATTR_REGCALL__
                      __ATTR_ALWAYS_INLINE__
		      __ATTR_HOT__
		      __ATTR_ALIGN__(32)
		      static inline
		      __m512d anglit_sample_zmm8r8(const __m512 cdf) {
                            return (anglit_cdf_inv_zmm8r8(cdf));
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
		   
		      __m512d arcsin_cdf_zmm8r8(const __m512d x,
		                                const __m512d a) {

                         const __m512d invpi = _mm512_set1_pd(0.318309886183790671537767526745);
			 const __m512d _0    = _mm512_setzero_pd();
			 const __m512d _1_2  = _mm512_set1_pd(0.5);
			 const __m512d _1    = _mm512_set1_pd(1.0);
			 __m512d t0,cdf;
			 __mmask8 m0,m1;
			 m0  = _mm512_cmp_pd_mask(x,zmm8r8_negate(a),_CMP_LE_OQ);
#if (CDF_PDF_USE_SLEEF_LIB) == 1
                         t0  = _mm512_mul_pd(xasin(_mm512_div_pd(x,a),invpi));
#else
                         t0  = _mm512_mul_pd(_mm512_asin_pd(_mm512_div_pd(x,a),invpi));
#endif
			 m1  = _mm512_cmp_pd_mask(x,a,_CMP_LT_OQ);
                         cdf = _mm512_mask_blend_pd(m0,_mm512_add_pd(_1_2,t0),_0);
			 cdf = _mm512_mask_blend_pd(m1,cdf,_1); 
                         return (cdf);
		   }


		   
		    
		      __m512 arcsin_cdf_zmm16r4(const __m512 x,
		                                const __m512 a) {

                         const __m512 invpi = _mm512_set1_ps(0.318309886183790671537767526745f);
			 const __m512 _0    = _mm512_setzero_ps();
			 const __m512 _1_2  = _mm512_set1_ps(0.5f);
			 const __m512 _1    = _mm512_set1_ps(1.0f);
			 __m512 t0,cdf;
			 __mmask16 m0,m1;
			 m0  = _mm512_cmp_ps_mask(x,zmm16r4_negate(a),_CMP_LE_OQ);
                         t0  = _mm512_mul_ps(_mm512_asin_ps(_mm512_div_ps(x,a),invpi));
			 m1  = _mm512_cmp_ps_mask(x,a,_CMP_LT_OQ);
                         cdf = _mm512_mask_blend_ps(m0,_mm512_add_pd(_1_2,t0),_0);
			 cdf = _mm512_mask_blend_ps(m1,cdf,_1); 
                         return (cdf);
		   }


		    
		      __m512d arcsin_cdf_inv_zmm8r8(const __m512d cdf,
		                                    const __m512d a) {

                           const __m512d pi    = _mm512_set1_pd(3.14159265358979323846264338328);
			   const __m512d _0    = _mm512_setzero_pd();
			   const __m512d _1    = _mm512_set1_pd(1.0);
			   //const __m512d nan   = _mm512_set1_pd(std::numeric_limits<double>::quiet_NaN());
			   const __m512d _1_2  = _mm512_set1_pd(0.5);
			   __m512d x;
			   if(__builtin_expect(_mm512_cmp_pd_mask(cdf,_0,_CMP_LT_OQ),0) ||
			      __builtin_expect(_mm512_cmp_pd_mask(cdf,_1,_CMP_GT_OQ),0)) {
                              return (_0);
			   }
#if (CDF_PDF_USE_SLEEF_LIB) == 1
                             x = _mm512_mul_pd(xsin(_mm512_mul_pd(pi,_mm512_sub_pd(cdf,_1_2))));
			     
#else
                             x = _mm512_mul_pd(_mm512_sin_pd(_mm512_mul_pd(pi,_mm512_sub_pd(cdf,_1_2))));
#endif
                             return (x);
		   }


		      
		      __m512 arcsin_cdf_inv_zmm16r4(const __m512 cdf,
		                                    const __m512 a) {

                           const __m512 pi    = _mm512_set1_ps(3.14159265358979323846264338328f);
			   const __m512 _0    = _mm512_setzero_ps();
			   const __m512 _1    = _mm512_set1_ps(1.0f);
			   //const __m512 nan   = _mm512_set1_ps(std::numeric_limits<float>::quiet_NaN());
			   const __m512 _1_2  = _mm512_set1_ps(0.5f);
			   __m512 x;
			   if(__builtin_expect(_mm512_cmp_ps_mask(cdf,_0,_CMP_LT_OQ),0) ||
			      __builtin_expect(_mm512_cmp_ps_mask(cdf,_1,_CMP_GT_OQ),0)) {
                              return (_0);
			   }
#if (CDF_PDF_USE_SLEEF_LIB) == 1
                             x = _mm512_mul_ps(xsinf(_mm512_mul_ps(pi,_mm512_sub_ps(cdf,_1_2))));
			     
#else
                             x = _mm512_mul_ps(_mm512_sin_ps(_mm512_mul_ps(pi,_mm512_sub_ps(cdf,_1_2))));
#endif
                             return (x);
		   }


		     
                      __m512d arcsin_mean_zmm8r8() {

		            return (_mm512_setzero_pd());
		      }


		     
                      __m512 arcsin_mean_zmm16r4() {

		            return (_mm512_setzero_ps());
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


                   
		      __m512d arcsin_pdf_zmm8r8(const __m512d x,
		                                const __m512d a) {

                           const __m512d pi    = _mm512_set1_pd(3.14159265358979323846264338328);
			   const __m512d _0    = _mm512_setzero_pd();
			   const __m512d _1    = _mm512_set1_pd(1.0);
			   //const __m512d nan   = _mm512_set1_pd(std::numeric_limits<double>::quiet_NaN());
			   __m512d pdf,t0;
			   __mmask8 m,m1;
			   if(__builtin_expect(_mm512_cmp_pd_mask(a,_0,_CMP_LE_OQ))) {
                               return (_0);
			   }
			   m  =  _mm512_cmp_pd_mask(x,zmm8r8_negate(a),_CMP_LE_OQ);
			   t0 =  _mm512_sqrt_pd(_mm512_sub_pd(_mm512_mul_pd(a,a),
			                                      _mm512_mul_pd(x,x)));
			   m1 = _mm512_cmp_pd_mask(x,a,_CMP_GE_OQ);
			   __mmask8 m2 = m || m1;
			   pdf = _mm512_mask_blend_pd(m2,_mm512_div_pd(_1,
			                                           _mm512_mul_pd(pi,t0)),_0);
			   return (pdf);
			   
		    }


		   
		      __m512 arcsin_pdf_zmm16r4(const __m512 x,
		                                const __m512 a) {

                           const __m512 pi    = _mm512_set1_ps(3.14159265358979323846264338328f);
			   const __m512 _0    = _mm512_setzero_ps();
			   const __m512 _1    = _mm512_set1_ps(1.0f);
			   //const __m512 nan   = _mm512_set1_ps(std::numeric_limits<float>::quiet_NaN());
			   __m512 pdf,t0;
			   __mmask 16m,m1;
			   if(__builtin_expect(_mm512_cmp_ps_mask(a,_0,_CMP_LE_OQ))) {
                               return (_0);
			   }
			   m  =  _mm512_cmp_ps_mask(x,zmm16r4_negate(a),_CMP_LE_OQ);
			   t0 =  _mm512_sqrt_ps(_mm512_sub_ps(_mm512_mul_ps(a,a),
			                                      _mm512_mul_ps(x,x)));
			   m1 = _mm512_cmp_ps_mask(x,a,_CMP_GE_OQ);
			   const __mmask16 m2 = m || m1;
			   pdf = _mm512_mask_blend_ps(m2,_mm512_div_ps(_1,
			                                           _mm512_mul_ps(pi,t0)),_0);
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


                       __m512d arcsin_variance_zmm8r8(const __m512d a) {

                         const __m512d _1_2 = _mm512_set1_pd(0.5);
			 __m512d variance;
			 variance = _mm512_mul_pd(a,_mm512_mul_pd(a,_1_2));
			 return (variance);
		     }


		     
                       __m512 arcsin_variance_zmm16r4(const __m512 a) {

                         const __m512 _1_2 = _mm512_set1_ps(0.5f);
			 __m512 variance;
			 variance = _mm512_mul_ps(a,_mm512_mul_ps(a,_1_2));
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
		     
		     
		      __m512d arcsin_sample_zmm8r8() {

                         __m512d cdf;
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
                            const __m512d _0 = _mm512_set1_pd(0.0);
			    return (_0);
			 }
			 uniform          = svrng_new_uniform_distribution_double(0.0,1.0);
			 const double * __restrict ptr = (const double*)(&svrng_generate8_double(engine,uniform));
			 cdf              = arcsin_cdf_inv_zmm8r8(_mm512_loadu_pd(&ptr[0]));
			 svrng_delete_engine(engine);
			 return (cdf);
		    }


		    
		      __m512d arcsin_sample_zmm8r8(const __m512 cdf) {

                            return (arcsin_cdf_inv_zmm8r8(cdf));
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


                  
		      __m512d beta_binomial_cdf_zmm8r8(const int32_t x,
		                                       const int32_t c,
						       const __m512d a,
						       const __m512d b) {

			      const __m512d _0  = _mm512_setzero_pd();
                              const __m512d _1  = _mm512_set1_pd(1.0);
			      __m512d vx,vy,vcy,vc1,vy1,vcy1;
			      __m512d cdf,pdf;

			      if(x<0) {
                                 cdf = _0;
			      }
			      else if(x<c) {
                                 cdf = _0;
				 for(int32_t y = 0; y < x; ++y) {
                                     vy  = _mm512_set1_pd((double)y);
				     vx  = _mm512_set1_pd((double)x);
				     vcy = _mm512_set1_pd((double)(c-y));
				     vc1 = _mm512_set1_pd((double)(c+1));
				     vy1 = _mm512_set1_pd((double)(y+1));
				     vcy1= _mm512_set1_pd((double)(c-y+1));
				     const __m512d t0 = beta_zmm8r8(_mm512_add_pd(a,vy),
				                                    _mm512_add_pd(b,vcy));
				     const __m512d t1 = _mm512_mul_pd(vc1,beta_zmm8r8(vy1,vcy1));
				     const __m512d t2 = beta_zmm8r8(a,b);
				     pdf              = _mm512_div_pd(t0,_mm512_mul_pd(t1,t2));
				     cdf              = _mm512_add_pd(cdf,pdf);
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

                   
		      __m512d beta_pdf_zmm8r8(const __m512d x,
		                              const __m512d a,
					      const __m512d b) {

                         const __m512d _0 = _mm512_setzero_pd();
			 const __m512d _1 = _mm512_set1_pd(1.0);
			 const __m512d t0 = _mm512_sub_pd(a,_1);
			 const __m512d t1 = _mm512_sub_pd(_1,x);
			 const __m512d t2 = _mm512_sub_pd(b,_1);
			 __m512d pdf,term1,term2,term3;
			 __mmask8 m0,m1,m2;
			 term1            = _mm512_pow_pd(x,t0);
			 m0               = _mm512_cmp_pd_mask(x,_0,_CMP_LT_OQ);
			 term2            = _mm512_mul_pd(term1,_mm512_pow_pd(t1,t2));
			 m1               = _mm512_cmp_pd_mask(x,_1,_CMP_LT_OQ);
			 term3            = _mm512_div_pd(term2,beta_zmm8r8(a,b));
			 m                = m1||m2;
			 pdf              = _mm512_mask_blend_pd(m,term3,_0);
			 return (pdf);
		    }


		
		      __m512
		      beta_pdf_zmm16r4(const __m512 x,
		                       const __m512 a,
				       const __m512 b) {

                         const __m512 _0 = _mm512_setzero_ps();
			 const __m512 _1 = _mm512_set1_ps(1.0);
			 const __m512 t0 = _mm512_sub_ps(a,_1);
			 const __m512 t1 = _mm512_sub_ps(_1,x);
			 const __m512 t2 = _mm512_sub_ps(b,_1);
			 __m512 pdf,term1,term2,term3;
			 __mmask16 m0,m1,m2;
			 term1            = _mm512_pow_ps(x,t0);
			 m0               = _mm512_cmp_ps_mask(x,_0,_CMP_LT_OQ);
			 term2            = _mm512_mul_ps(term1,_mm512_pow_pd(t1,t2));
			 m1               = _mm512_cmp_ps_mask(x,_1,_CMP_LT_OQ);
			 term3            = _mm512_div_ps(term2,beta_zmm16r4(a,b));
			 m                = m1||m2;
			 pdf              = _mm512_mask_blend_ps(m,term3,_0);
			 return (pdf);
		    }


		     
		      __m512d
		      beta_variance_zmm8r8(const __m512d a,
		                           const __m512d b) {

			  __m512d variance;
                          const __m512d _1  = _mm512_set1_pd(1.0);
			  const __m512d ab  = _mm512_add_pd(a,b);
			  const __m512d t0  = _mm512_mul_pd(_mm512_mul_pd(ab,ab),
			                                    _mm512_add_pd(_1,ab));
			  variance          = _mm512_div_pd(_mm512_mul_pd(a,b),t0);				   
			  
		    }


		     
		      __m512
		      beta_variance_zmm16r4(const __m512 a,
		                            const __m512 b) {

			  __m512 variance;
                          const __m512 _1  = _mm512_set1_ps(1.0f);
			  const __m512 ab  = _mm512_add_ps(a,b);
			  const __m512 t0  = _mm512_mul_ps(_mm512_mul_ps(ab,ab),
			                                    _mm512_add_ps(_1,ab));
			  variance          = _mm512_div_ps(_mm512_mul_ps(a,b),t0);				   
			  
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

                      
                    
                      __m512d
		      weibull_cdf_zmm8r8(const __m512d x,
		                         const __m512d a,
					 const __m512d b,
					 const __m512d c) {

                          const __m512d  _0 = _mm512_setzero_pd();
			  const __m512d  _1 = _mm512_set1_pd(1.0);
			  const __m512d  y  = _mm512_div_pd(_mm512_sub_pd(x,a),b);
			  const __m512d  exc= _mm512_exp_pd(_mm512_pow_pd(y,c));
			  __m512d cdf;
			  const __mmask8 m  = _mm512_cmp_pd_mask(a,x,_CMP_LT_OQ);
			  cdf               = _mm512_mask_blend_pd(m,_mm512_sub_pd(_1,
			                                                       _mm512_div_pd(_1,exc)),_0);
			  return (cdf);
		   }
		    
		    
		  
                      __m512
		      weibull_cdf_zmm16r4(const __m512 x,
		                          const __m512 a,
					  const __m512 b,
					  const __m512 c) {

                          const __m512  _0 = _mm512_setzero_ps();
			  const __m512  _1 = _mm512_set1_ps(1.0f);
			  const __m512  y  = _mm512_div_ps(_mm512_sub_ps(x,a),b);
			  const __m512  exc= _mm512_exp_ps(_mm512_pow_ps(y,c));
			  __m512 cdf;
			  const __mmask16 m  = _mm512_cmp_ps_mask(a,x,_CMP_LT_OQ);
			  cdf               = _mm512_mask_blend_ps(m,_mm512_sub_ps(_1,
			                                                       _mm512_div_ps(_1,exc)),_0);
			  return (cdf);
		   }


		  
                      __m512d
		      weibull_cdf_inv_zmm8r8(const __m512d a,
		                             const __m512d b,
					     const __m512d c,
					     const __m512d cdf) {

                        const __m512d  _0  = _mm512_setzero_pd();
			const __m512d  _1  = _mm512_set1_pd(1.0);
			//const __m512d  nan = _mm512_set1_pd(std::numeric_limits<double>::quiet_NaN());
			__m512d t0,t1,x;
			if(__builtin_expect(_mm512_cmp_pd_mask(cdf,_0,_CMP_LT_OQ),0) ||
			   __builtin_expect(_mm512_cmp_pd_mask(cdf,_1,_CMP_GT_OQ),0)) {
                           return (_0);
			}
			t0                 = zmm8r8_negate(_mm512_log_pd(_mm512_sub_pd(_1,cdf)));
			t1                 = _mm512_pow_pd(t0,_mm512_div_pd(_1,c));
			x                  = _mm512_fmadd_pd(a,b,t1);
			return (x);
			
		   }


		    
                      __m512
		      weibull_cdf_inv_zmm16r4(const __m512 a,
		                             const __m512 b,
					     const __m512 c,
					     const __m512 cdf) {

                        const __m512  _0  = _mm512_setzero_ps();
			const __m512  _1  = _mm512_set1_ps(1.0f);
			//const __m512  nan = _mm512_set1_ps(std::numeric_limits<float>::quiet_NaN());
			__m512 t0,t1,x;
			if(__builtin_expect(_mm512_cmp_ps_mask(cdf,_0,_CMP_LT_OQ),0) ||
			   __builtin_expect(_mm512_cmp_ps_mask(cdf,_1,_CMP_GT_OQ),0)) {
                           return (_0);
			}
			t0                 = zmm16r4_negate(_mm512_log_pd(_mm512_sub_ps(_1,cdf)));
			t1                 = _mm512_pow_ps(t0,_mm512_div_ps(_1,c));
			x                  = _mm512_fmadd_ps(a,b,t1);
			return (x);
			
		   }


		   
		      __m512d
		      weibull_sample_zmm8r8(const __m512d vrand,
		                            const __m512d a,
					    const __m512d b,
					    const __m512d c) {

                         return (weibull_cdf_zmm8r8(a,b,c,vrand));
		   }


		    
		      __m512
		      weibull_sample_zmm16r4(const __m512 vrand,
		                            const __m512 a,
					    const __m512 b,
					    const __m512 c) {

                         return (weibull_cdf_zmm16r4(a,b,c,vrand));
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


                    
		      __m512d
                      weibull_discrete_cdf_zmm8r8(const __m512d x,
		                                  const __m512d a,
					          const __m512d b) {

			    __m512d cdf;
                            const __m512d  _0 = _mm512_setzero_pd();
			    const __m512d  _1 = _mm512_set1_pd(1.0);
			    const __m512d  t0 = _mm512_pow_pd(_mm512_add_pd(x,_1),b);
			    const __m512d  t1 = _mm512_pow_pd(_mm512_sub_pd(_1,a),t0);
			    const __mmask8 m  = _mm512_cmp_pd_mask(x,_0,_CMP_LT_OQ);
			    cdf               = _mm512_mask_blend_pd(m,_mm512_sub_pd(_1,t1),_0);
			    return (cdf);
		    }


		   
		      __m512
                      weibull_discrete_cdf_zmm16r4(const __m512 x,
		                                  const __m512 a,
					          const __m512 b) {

			    __m512 cdf;
                            const __m512  _0 = _mm512_setzero_ps();
			    const __m512  _1 = _mm512_set1_ps(1.0f);
			    const __m512  t0 = _mm512_pow_ps(_mm512_add_ps(x,_1),b);
			    const __m512  t1 = _mm512_pow_ps(_mm512_sub_ps(_1,a),t0);
			    const __mmask16 m  = _mm512_cmp_ps_mask(x,_0,_CMP_LT_OQ);
			    cdf               = _mm512_mask_blend_ps(m,_mm512_sub_pd(_1,t1),_0);
			    return (cdf);
		    }


		     
		      __m512d
		      weibull_discrete_pdf_zmm8r8(const __m512d x,
		                                  const __m512d a,
					          const __m512d b) {

                            __m512d pdf;
                            const __m512d  _0 = _mm512_setzero_pd();
			    const __m512d  _1 = _mm512_set1_pd(1.0);
			    const __m512d  t0 = _mm512_pow_pd(_mm512_add_pd(x,_1),b);
			    const __m512d  _1a= _mm512_sub_pd(_1,a);
			    const __m512d  t1 = _mm512_pow_pd(_1a,t0);
                            const __m512d  t2 = _mm512_pow_pd(x,b);
			    const __m512d  t3 = _mm512_pow_pd(_1a,t2);
			    pdf               = _mm512_sub_pd(t3,t1);
			    return (pdf);
		   }


		   
		      __m512
		      weibull_discrete_pdf_zmm16r4(const __m512 x,
		                                  const __m512 a,
					          const __m512 b) {

                            __m512 pdf;
                            const __m512  _0 = _mm512_setzero_ps();
			    const __m512  _1 = _mm512_set1_ps(1.0);
			    const __m512  t0 = _mm512_pow_ps(_mm512_add_ps(x,_1),b);
			    const __m512  _1a= _mm512_sub_ps(_1,a);
			    const __m512  t1 = _mm512_pow_ps(_1a,t0);
                            const __m512  t2 = _mm512_pow_ps(x,b);
			    const __m512  t3 = _mm512_pow_ps(_1a,t2);
			    pdf               = _mm512_sub_ps(t3,t1);
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


                     
		      __m512d
		      weibull_discr_icdf_zmm8r8(const __m512d cdf,
		                                const __m512d a,
						const __m512d b) {

                          //const __m512d  nan = _mm512_set1_pd(std::numeric_limits<double>::quiet_NaN());
			  const __m512d  _0  = _mm512_setzero_pd();
			  const __m512d  _1  = _mm512_set1_pd(1.0);
			  if(__builtin_expect(_mm512_cmp_pd_mask(cdf,_0,_CMP_LT_OQ),0) ||
			     __builtin_expect(_mm512_cmp_pd_mask(cdf,_1,_CMP_GT_OQ),0)) {
                           return (_0);
			  }
			  const __m512d t0   =  _mm512_log_pd(_mm512_sub_pd(_1,cdf));
			  const __m512d t1   =  _mm512_log_pd(_mm512_sub_pd(_1,a));
			  const __m512d t2   =  _mm512_div_pd(t1,t2)
			  const __m512d t3   =  _mm512_pow_pd(t2,_mm512_div_pd(_1,b));
			  __m512d x;
			  x                  =  _mm512_ceil_pd(_mm512_sub_pd(t3,_1));
			  return (x);
		    }


		  
		      __m512
		      weibull_discr_icdf_zmm16r4(const __m512 cdf,
		                                const __m512 a,
						const __m512 b) {

                          //const __m512  nan = _mm512_set1_pd(std::numeric_limits<float>::quiet_NaN());
			  const __m512  _0  = _mm512_setzero_ps();
			  const __m512  _1  = _mm512_set1_ps(1.0f);
			  if(__builtin_expect(_mm512_cmp_ps_mask(cdf,_0,_CMP_LT_OQ),0) ||
			     __builtin_expect(_mm512_cmp_ps_mask(cdf,_1,_CMP_GT_OQ),0)) {
                           return (_0);
			  }
			  const __m512 t0   =  _mm512_log_ps(_mm512_sub_ps(_1,cdf));
			  const __m512 t1   =  _mm512_log_ps(_mm512_sub_ps(_1,a));
			  const __m512 t2   =  _mm512_div_ps(t1,t2)
			  const __m512 t3   =  _mm512_pow_ps(t2,_mm512_div_ps(_1,b));
			  __m512 x;
			  x                  =  _mm512_ceil_ps(_mm512_sub_ps(t3,_1));
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


                             
                      __m512d
		      weibull_discr_samp_zmm8r8(   const __m512d vrand,
		                                   const __m512d a,
						   const __m512d b) {

                         return (weibull_discr_icdf_zmm8r8(vrand,a,b));
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

#include "GMS_rotations_avx512_helpers.hpp" // fmod


                            
                      __m512d
		      von_misses_cdf_zmm8r8(const __m512d x,
		                            const __m512d a,
					    const __m512d b) {

                        //Early exit.
			const __m512d   _0  = _mm512_setzero_pd();
			const __m512d   _1  = _mm512_set1_pd(1.0);
			const __m512d   pi  = _mm512_set1_pd(3.14159265358979323846264338328);
			const __m512d   npi = _mm512_set1_pd(-3.14159265358979323846264338328);
			const __m512d   xsa = _mm512_sub_pd(x,a);
			if(__builtin_expect(_mm512_cmp_pd_mask(xsa,npi,_CMP_LE_OQ),0)) {
		             return (_0);
			}
			if(__builtin_expect(_mm512_cmp_pd_mask(npi,xsa,_CMP_LE_OQ),0)) {
                             return (_1); 
			}
			const __m512d  _2pi = _mm512_set1_pd(6.283185307179586476925286766559);
			const __m512d  a1  = _mm512_set1_pd(12.0);
			const __m512d  a2  = _mm512_set1_pd(0.8);
			const __m512d  a3  = _mm512_set1_pd(8.0);
			const __m512d  a4  = _mm512_set1_pd(1.0);
			const __m512d  c1  = _mm512_set1_pd(56.0);
			const __m512d  ck  = _mm512_set1_pd(10.5);
			const __m512d  _2  = _mm512_set1_pd(2.0);
			const __m512d  _1_2= _mm512_set1_pd(0.5);
			__m512d arg,cdf,cn,p,r,s,sn,u,v,y,z,uprv,erfx;
			//  Convert the angle (X - A) modulo 2 PI to the range ( 0, 2 * PI ).
			z    = b;
			u    = fmod_zmm8r8(_mm512_add_pd(xsa,pi),_2pi);
			uprv = u;
			const __mmask8 m = _mm512_cmp_pd_mask(u,_0,_CMP_LT_OQ);
			u    = _mm512_add_pd(u,_2pi);
			u    = _mm512_mask_blend_pd(m,uprv,u);
			y    = _mm512_sub_pd(u,pi);
			
			//For small B, sum IP terms by backwards recursion.
			// Can not be vectorized manually, hence 0 is returned.
			// Only large B is computed.
			/*
                              This scalar code can not be vectorized.
                              ip = int ( z * a2 - a3 / ( z + a4 ) + a1 )
                              Used as loop control variable
                              do n = 2, ip
                         */
                        if(_mm512_cmp_pd_mask(z,ck,_CMP_LE_OQ)) {
                           return (_0);
			}
			else {
                           const __m512d t0 = _mm512_set1_pd(24.0);
			   const __m512d t1 = _mm512_set1_pd(54.0);
			   const __m512d t2 = _mm512_set1_pd(347.0);
			   const __m512d t3 = _mm512_set1_pd(26.0);
			   const __m512d t4 = _mm512_set1_pd(6.0);
			   const __m512d t5 = _mm512_set1_pd(12.0);
			   const __m512d t6 = _mm512_set1_pd(3.0);
			   const __m512d t7 = _mm512_set1_pd(16.0);
			   const __m512d t8 = _mm512_set1_pd(1.75);
			   const __m512d t9 = _mm512_set1_pd(83.5);
			   c                = _mm512_mul_pd(t0,z);
			   v                = _mm512_sub_pd(c,c1);
			   const __m512d tmp1 = _mm512_sub_pd(_mm512_add_pd(v,t3),c);
			   const __m512d tmp2 = _mm512_div_pd(t1,_mm512_div_pd(t2,tmp1));
			   const __m512d tmp3 = _mm512_add_pd(_mm512_sub_pd(tmp2,t4),c);
			   r                  = _mm512_sqrt_pd(_mm512_div_pd(tmp3,t5));
#if (CDF_PDF_USE_SLEEF_LIB) == 1
                           z                  = _mm512_mul_pd(xsin(_mm512_mul_pd(_1_2,y)),r);
#else
			   z                  = _mm512_mul_pd(_mm512_sin_pd(
			                                                _mm512_mul_pd(_1_2,y)),r);
#endif
                           s                  = _mm512_mul_pd(_2,_mm512_mul_pd(z,z));
			   v                  = _mm512_sub_pd(v,_mm512_add_pd(s,t6));
			   y                  = _mm512_div_pd(_mm512_sub_pd(_mm512_sub_pd(c,s),
			                                                    _mm512_sub_pd(s,t7)),t6);
			   tmp1               = _mm512_sub_pd(v,y);
			   y                  = _mm512_div_pd(_mm512_fmadd_pd(_mm512_add_pd(s,t8),s,t9),tmp1);
			   tmp2               = _mm512_mul_pd(y,y);
			   arg                = _mm512_mul_pd(z,_mm512_sub_pd(_1,
			                                                  _mm512_div_pd(s,tmp2)));
			   erfx               = _mm512_erf_pd(arg);
			   cdf                = _mm512_fmadd_pd(_1_2,erfx,_1_2);
			}
			cdf                   = _mm512_max_pd(cdf,_0);
			cdf                   = _mm512_min_pd(cdf,_1);
			return (cdf);
			
		   }


		      
	                    
                      __m512
		      von_misses_cdf_zmm16r4(const __m512 x,
		                            const __m512 a,
					    const __m512 b) {

                        //Early exit.
			const __m512   _0  = _mm512_setzero_ps();
			const __m512   _1  = _mm512_set1_ps(1.0f);
			const __m512   pi  = _mm512_set1_ps(3.14159265358979323846264338328f);
			const __m512   npi = _mm512_set1_ps(-3.14159265358979323846264338328f);
			const __m512   xsa = _mm512_sub_ps(x,a);
			if(__builtin_expect(_mm512_cmp_ps_mask(xsa,npi,_CMP_LE_OQ),0)) {
		             return (_0);
			}
			if(__builtin_expect(_mm512_cmp_ps_mask(npi,xsa,_CMP_LE_OQ),0)) {
                             return (_1); 
			}
			const __m512  _2pi = _mm512_set1_ps(6.283185307179586476925286766559f);
			const __m512  a1  = _mm512_set1_ps(12.0f);
			const __m512  a2  = _mm512_set1_ps(0.8f);
			const __m512  a3  = _mm512_set1_ps(8.0f);
			const __m512  a4  = _mm512_set1_ps(1.0f);
			const __m512  c1  = _mm512_set1_ps(56.0f);
			const __m512  ck  = _mm512_set1_ps(10.5f);
			const __m512  _2  = _mm512_set1_ps(2.0f);
			const __m512  _1_2= _mm512_set1_ps(0.5f);
			__m512 arg,cdf,cn,p,r,s,sn,u,v,y,z,uprv,erfx;
			//  Convert the angle (X - A) modulo 2 PI to the range ( 0, 2 * PI ).
			z    = b;
			u    = fmod_zmm16r4(_mm512_add_ps(xsa,pi),_2pi);
			uprv = u;
			const __mmask16 m = _mm512_cmp_ps_mask(u,_0,_CMP_LT_OQ);
			u    = _mm512_add_ps(u,_2pi);
			u    = _mm512_mask_blend_ps(m,uprv,u);
			y    = _mm512_sub_ps(u,pi);
			
			//For small B, sum IP terms by backwards recursion.
			// Can not be vectorized manually, hence 0 is returned.
			// Only large B is computed.
			/*
                              This scalar code can not be vectorized.
                              ip = int ( z * a2 - a3 / ( z + a4 ) + a1 )
                              Used as loop control variable
                              do n = 2, ip
                         */
                        if(_mm512_cmp_ps_mask(z,ck,_CMP_LE_OQ)) {
                           return (_0);
			}
			else {
                           const __m512 t0 = _mm512_set1_ps(24.0f);
			   const __m512 t1 = _mm512_set1_ps(54.0f);
			   const __m512 t2 = _mm512_set1_ps(347.0f);
			   const __m512 t3 = _mm512_set1_ps(26.0f);
			   const __m512 t4 = _mm512_set1_ps(6.0f);
			   const __m512 t5 = _mm512_set1_ps(12.0f);
			   const __m512 t6 = _mm512_set1_ps(3.0f);
			   const __m512 t7 = _mm512_set1_ps(16.0f);
			   const __m512 t8 = _mm512_set1_ps(1.75f);
			   const __m512 t9 = _mm512_set1_ps(83.5f);
			   c                = _mm512_mul_ps(t0,z);
			   v                = _mm512_sub_ps(c,c1);
			   const __m512d tmp1 = _mm512_sub_ps(_mm512_add_ps(v,t3),c);
			   const __m512d tmp2 = _mm512_div_ps(t1,_mm512_div_ps(t2,tmp1));
			   const __m512d tmp3 = _mm512_add_ps(_mm512_sub_ps(tmp2,t4),c);
			   r                  = _mm512_sqrt_ps(_mm512_div_ps(tmp3,t5));
#if (CDF_PDF_USE_SLEEF_LIB) == 1
                           z                  = _mm512_mul_ps(xsinf(_mm512_mul_ps(_1_2,y)),r);
#else
			   z                  = _mm512_mul_ps(_mm512_sin_ps(
			                                                _mm512_mul_ps(_1_2,y)),r);
#endif
                           s                  = _mm512_mul_ps(_2,_mm512_mul_ps(z,z));
			   v                  = _mm512_sub_ps(v,_mm512_add_ps(s,t6));
			   y                  = _mm512_div_ps(_mm512_sub_ps(_mm512_sub_ps(c,s),
			                                                    _mm512_sub_ps(s,t7)),t6);
			   tmp1               = _mm512_sub_ps(v,y);
			   y                  = _mm512_div_ps(_mm512_fmadd_ps(_mm512_add_ps(s,t8),s,t9),tmp1);
			   tmp2               = _mm512_mul_ps(y,y);
			   arg                = _mm512_mul_ps(z,_mm512_sub_ps(_1,
			                                                  _mm512_div_ps(s,tmp2)));
			   erfx               = _mm512_erf_ps(arg);
			   cdf                = _mm512_fmadd_ps(_1_2,erfx,_1_2);
			}
			cdf                   = _mm512_max_ps(cdf,_0);
			cdf                   = _mm512_min_ps(cdf,_1);
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


                           
                      __m512d
		      von_misses_pdf_zmm8r8(const __m512d x,
		                            const __m512d a,
					    const __m512d b) {
 
                           const __m512d   pi  = _mm512_set1_pd(3.14159265358979323846264338328);
			   const __m512d   _2pi= _mm512_set1_pd(6.283185307179586476925286766559);
			   const __m512d   _0  = _mm512_setzero_pd();
			   const __m512d   _2  = _mm512_set1_pd(2.0);
			   const __m512d   t0  = _mm512_sub_pd(a,pi);
			   const __m512d   t1  = _mm512_add_pd(a,pi);
			   __m512d pdf;
			   __mmask8 m1,m2;
			   m1                  = _mm512_cmp_pd_mask(x,t0,_CMP_LT_OQ);
			   pdf                 = _mm512_mask_blend_pd(m1,_0,_0);
			   m2                  = _mm512_cmp_pd_mask(x,t1,_CMP_LE_OQ);
#if (CDF_PDF_USE_SLEEF_LIB) == 1
			   const __m512d tmp1  = xexp(_mm512_mul_pd(b,xcos(_mm512_sub_pd(x,a))));
#else
                           const __m512d tmp1  = _mm512_exp(_mm512_mul_pd(b,
			                                              _mm512_cos_pd(
								                _mm512_sub_pd(x,a))));
#endif
                           
			   pdf                 = _mm512_mask_blend_pd(m2,_0,_mm512_div_pd(tmp1,
			                                              _mm512_mul_pd(_2pi,bessesl_i0_zmm8r8(b))));
			   return (pdf);
		   }


		           
                      __m512
		      von_misses_pdf_zmm16r4(const __m512 x,
		                            const __m512 a,
					    const __m512 b) {
 
                           const __m512   pi  = _mm512_set1_pd(3.14159265358979323846264338328f);
			   const __m512   _2pi= _mm512_set1_pd(6.283185307179586476925286766559f);
			   const __m512   _0  = _mm512_setzero_pd();
			   const __m512   _2  = _mm512_set1_pd(2.0);
			   const __m512   t0  = _mm512_sub_pd(a,pi);
			   const __m512   t1  = _mm512_add_pd(a,pi);
			   __m512 pdf;
			   __mmask16 m1,m2;
			   m1                  = _mm512_cmp_pd_mask(x,t0,_CMP_LT_OQ);
			   pdf                 = _mm512_mask_blend_pd(m1,_0,_0);
			   m2                  = _mm512_cmp_pd_mask(x,t1,_CMP_LE_OQ);
#if (CDF_PDF_USE_SLEEF_LIB) == 1
			   const __m512 tmp1  = xexpf(_mm512_mul_pd(b,xcosf(_mm512_sub_pd(x,a))));
#else
                           const __m512 tmp1  = _mm512_exp(_mm512_mul_pd(b,
			                                              _mm512_cos_pd(
								                _mm512_sub_pd(x,a))));
#endif
                           
			   pdf                 = _mm512_mask_blend_pd(m2,_0,_mm512_div_pd(tmp1,
			                                              _mm512_mul_pd(_2pi,bessesl_i0_zmm8r8(b))));
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


                           
                      __m512d
                      von_misses_sample_zmm8r8(const __m512d a,
		                               const __m512d b) {

                          const __m512d  pi   = _mm512_set1_pd(3.14159265358979323846264338328);
			  const __m512d  _1   = _mm512_set1_pd(1.0);
			  const __m512d  _2   = _mm512_set1_pd(2.0);
			  const __m512d  _4   = _mm512_set1_pd(4.0);
			  const __m512d  _1_2 = _mm512_set1_pd(0.5);
			  __m512d c,f,rho,tau,u1,r;
			  __m512d u2,u3,x,z;
			  __m512d t0,t1,t2;
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
                             const __m512d _0 = _mm512_set1_pd(0.0);
			     return (_0);
			  }
			  uniform             = svrng_new_uniform_distribution_double(0.0,1.0);
			  t0                  = _mm512_fmadd_pd(_4,_mm512_mul_pd(b,b),_1);
			  tau                 = _mm512_add_pd(_1,_mm512_sqrt_pd(t0));
			  t1                  = _mm512_add_pd(b,b);
			  rho                 = _mm512_div_pd(_mm512_sub_pd(tau,
			                                                _mm512_sqrt_pd(_mm512_add_pd(tau,tau))),t1);
			  t2                  = _mm512_fmadd_pd(rho,rho,_1);
			  r                   = _mm512_div_pd(t2,_mm512_add_pd(rho,rho));
            
 			 while(true) {
                               
                              const double * __restrict ptr = (const double*)(&svrng_generate8_double(engine,uniform));
                              u1                            = _mm512_loadu_pd(&ptr[0]);
#if (CDF_PDF_USE_SLEEF_LIB) == 1
			      z                             = xcos(_mm512_mul_pd(pi,u1));
#else
                              z                             = _mm512_cos_pd(_mm512_mul_pd(pi,u1));
#endif
                              f                             = _mm512_div_pd(_mm512_fmadd_pd(r,z,_1),
			                                                    _mm512_add_pd(r,z));
			      c                             = _mm512_mul_pd(b,_mm512_sub_pd(r,f));
			      t0                            = _mm512_mul_pd(c,_mm512_sub_pd(_2,c));
			                       
			      if(_mm512_cmp_mask_pd(u2,t0,_CMP_LT_OQ)) break;
			      t1                            = _mm512_add_pd(_mm512_log_pd(
			                                                  _mm512_div_pd(c,u2)),_1);
			      if(_mm512_cmp_mask_pd(c,t1,_CMP_LE_OQ)) break;
			 }
			 const double * __restrict ptr2 =
			                    (const double*)(&svrng_generate8_double(engine,uniform));
			 u3                             = _mm512_loadu_pd(&ptr2[0]);
		         t2                             = zmm8r8_sign_zmm8r8(_1,_mm512_sub_pd(u3,_1_2));
			 x                              = _mm512_fmadd_pd(t2,_mm512_acos_pd(f),a);
			 svrng_delete_engine(engine);
			 return (x)
		   }


		   
		         
                      __m512
                      von_misses_sample_zmm16r4(const __m512 a,
		                                const __m512 b) {

                          const __m512   pi   = _mm512_set1_ps(3.14159265358979323846264338328f);
			  const __m512   _1   = _mm512_set1_ps(1.0f);
			  const __m512  _2    = _mm512_set1_ps(2.0f);
			  const __m512  _4    = _mm512_set1_ps(4.0f);
			  const __m512  _1_2  = _mm512_set1_ps(0.5f);
			  __m512 c,f,rho,tau,u1,r;
			  __m512 u2,u3,x,z;
			  __m512 t0,t1,t2;
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
                             const __m512 _0 = _mm512_set1_ps(0.0F);
			     return (_0);
			  }
			  uniform             = svrng_new_uniform_distribution_float(0.0f,1.0f);
			  t0                  = _mm512_fmadd_ps(_4,_mm512_mul_ps(b,b),_1);
			  tau                 = _mm512_add_ps(_1,_mm512_sqrt_ps(t0));
			  t1                  = _mm512_add_ps(b,b);
			  rho                 = _mm512_div_ps(_mm512_sub_ps(tau,
			                                                _mm512_sqrt_ps(_mm512_add_ps(tau,tau))),t1);
			  t2                  = _mm512_fmadd_ps(rho,rho,_1);
			  r                   = _mm512_div_ps(t2,_mm512_add_ps(rho,rho));
            
 			 while(true) {
                               
                              const float * __restrict ptr = (const float*)(&svrng_generate16_float(engine,uniform));
                              u1                            = _mm512_loadu_ps(&ptr[0]);
#if (CDF_PDF_USE_SLEEF_LIB) == 1
			      z                             = xcosf(_mm512_mul_ps(pi,u1));
#else
                              z                             = _mm512_cos_ps(_mm512_mul_ps(pi,u1));
#endif
                              f                             = _mm512_div_ps(_mm512_fmadd_ps(r,z,_1),
			                                                    _mm512_add_ps(r,z));
			      c                             = _mm512_mul_ps(b,_mm512_sub_ps(r,f));
			      t0                            = _mm512_mul_ps(c,_mm512_sub_ps(_2,c));
			                       
			      if(_mm512_cmp_mask_ps(u2,t0,_CMP_LT_OQ)) break;
			      t1                            = _mm512_add_ps(_mm512_log_ps(
			                                                  _mm512_div_ps(c,u2)),_1);
			      if(_mm512_cmp_mask_ps(c,t1,_CMP_LE_OQ)) break;
			 }
			 const float * __restrict ptr2 =
			                    (const float*)(&svrng_generate16_float(engine,uniform));
			 u3                             = _mm512_loadu_ps(&ptr2[0]);
		         t2                             = zmm16r4_sign_zmm16r4(_1,_mm512_sub_ps(u3,_1_2));
			 x                              = _mm512_fmadd_ps(t2,_mm512_acos_ps(f),a);
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


                            
                      __m512d
		      rayleigh_pdf_zmm8r8(const __m512d x,
		                          const __m512d a) {

                           const __m512d  _0 = _mm512_setzero_pd();
			   __m512d t0,t1,t2,t3,pdf;
			   const __mmask8 m  = _mm512_cmp_pd_mask(x,_0,_CMP_LT_OQ);
			   t0                = _mm512_mul_pd(a,a);
			   t1                = zmm8r8_negate(_mm512_div_pd(_mm512_mul_pd(x,x),
			                                                   _mm512_add_pd(t0,t0)));
			   t2                = _mm512_div_pd(x,t0);
#if (CDF_PDF_USE_SLEEF_LIB) == 1
                           t3               = _mm512_mul_pd(t2,xexp(t1));
#else
			   t3               = _mm512_mul_pd(t2,_mm512_exp_pd(t1));
#endif
                           pdf              = _mm512_mask_blend_pd(m,t3,_0);
                           return (pdf);
		     }


		         
                      __m512
		      rayleigh_pdf_zmm16r4(const __m512 x,
		                           const __m512 a) {

                           const __m512  _0 = _mm512_setzero_ps();
			   __m512 t0,t1,t2t3,pdf;
			   const __mmask16 m  = _mm512_cmp_ps_mask(x,_0,_CMP_LT_OQ);
			   t0                = _mm512_mul_ps(a,a);
			   t1                = zmm16r4_negate(_mm512_div_ps(_mm512_mul_ps(x,x),
			                                                   _mm512_add_ps(t0,t0)));
			   t2                = _mm512_div_ps(x,t0);
#if (CDF_PDF_USE_SLEEF_LIB) == 1
                           t3                = _mm512_mul_ps(t2,xexpf(t1));
#else
			   t3                = _mm512_mul_ps(t2,_mm512_exp_ps(t1));
#endif
                           pdf               = _mm512_mask_blend_ps(m,_t3,_0);
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


                      
      		            
                      __m512d
		      rayleigh_mean_zmm8r8(const __m512d a) {

                          const __m512d hpi =  _mm512_set1_pd(0.5*3.14159265358979323846264338328);
			  __m512d mean;
			  mean              =  _mm512_mul_pd(a,_mm512_sqrt_pd(hpi));
			  return (mean);
		     }


		         
                      __m512
		      rayleigh_mean_zmmr16r4(const __m512d a) {

                          const __m512 hpi =  _mm512_set1_ps(0.5f*3.14159265358979323846264338328f);
			  __m512 mean;
			  mean              =  _mm512_mul_ps(a,_mm512_sqrt_ps(hpi));
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


                          
                      __m512d
		      rayleigh_invcdf_zmm8r8(const __m512d cdf,
		                             const __m512d a) {

			 const __m512d _0 = _mm512_setzero_pd();
			 const __m512d _1 = _mm512_setzero_pd(1.0);
			 const __m512d n2 = _mm512_setzero_pd(-2.0);
			 __m512d inv,t0,t1,;
                         if(__builtin_expect(_mm512_cmp_pd_mask(cdf,_0,_CMP_LT_OQ),0) ||
			    __builtin_expect(_mm512_cmp_pd_mask(_1,cdf,_CMP_LT_OQ),0)) {return;}
			 t0  = _mm512_log_pd(_mm512_sub_pd(_1,cdf));
			 t1  = _mm512_mul_pd(_2,_mm512_mul_pd(a,a));
                         inv = _mm512_sqrt_pd(_mm512_mul_pd(t0,t1));
			 return (inv);
			   
		     }


		           
                      __m512
		      rayleigh_invcdf_zmm16r4(const __m512 cdf,
		                             const __m512 a) {

			 const __m512 _0 = _mm512_setzero_ps();
			 const __m512 _1 = _mm512_setzero_ps(1.0f);
			 const __m512 n2 = _mm512_setzero_ps(-2.0f);
			 __m512 inv,t0,t1,;
                         if(__builtin_expect(_mm512_cmp_ps_mask(cdf,_0,_CMP_LT_OQ),0) ||
			    __builtin_expect(_mm512_cmp_ps_mask(_1,cdf,_CMP_LT_OQ),0)) {return;}
			 t0  = _mm512_log_ps(_mm512_sub_ps(_1,cdf));
			 t1  = _mm512_mul_ps(_2,_mm512_mul_ps(a,a));
                         inv = _mm512_sqrt_ps(_mm512_mul_ps(t0,t1));
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


                          
                      __m512d
		      rayleigh_cdf_zmm8r8(const __m512d x,
		                          const __m512d a) {

                         const __m512d _0 = _mm512_setzero_pd();
			 const __m512d _1 = _mm512_setzero_pd(1.0);
			 __m512d cdf,t0,t1;
			 t0              = _mm512_mul_pd(_2,_mm512_mul_pd(a,a));
			 t1              = zmm8r8_negate(_mm512_mul_pd(x,x));
#if (CDF_PDF_USE_SLEEF_LIB) == 1
                         cdf             = _mm512_sub_pd(_1,xexp(_mm512_div_pd(t1,t0)));
			                            
#else
			 cdf             = _mm512_sub_pd(_1,
			                             _mm512_exp_pd(_mm512_div_pd(t1,t0)));
#endif
                         return (cdf);
		    }


		            
                      __m512
		      rayleigh_cdf_zmm16r4(const __m512 x,
		                           const __m512 a) {

                         const __m512 _0 = _mm512_setzero_ps();
			 const __m512 _1 = _mm512_setzero_ps(1.0f);
			 __m512 cdf,t0,t1;
			 t0              = _mm512_mul_pd(_2,_mm512_mul_ps(a,a));
			 t1              = zmm16r4_negate(_mm512_mul_ps(x,x));
#if (CDF_PDF_USE_SLEEF_LIB) == 1
                         cdf             = _mm512_sub_ps(_1,xexpf(_mm512_div_ps(t1,t0)));
			                            
#else
			 cdf             = _mm512_sub_ps(_1,
			                             _mm512_exp_ps(_mm512_div_ps(t1,t0)));
#endif
                         return (cdf);
		    }


		    
		           
                      __m512d
		      rayleigh_sample_zmm8r8(const __m512d rand,
		                             const __m512d a) {

                          return (rayleigh_invcdf_zmm8r8(rand,a));
		     }


		      
                      __m512
		      rayleigh_sample_zmm16r4(const __m512 rand,
		                             const __m512 a) {

                          return (rayleigh_invcdf_zmm16r4(rand,a));
		     }



















