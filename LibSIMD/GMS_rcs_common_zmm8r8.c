

#include "GMS_rcs_common_zmm8r8.h"
#include "GMS_complex_zmm8r8.h"
#include "GMS_simd_utils.h"
#include "GMS_sleefsimddp.h"





                void k_zmm8r8(const __m512d mur,
                                  const __m512d mui
                                  const __m512d epsr,
                                  const __m512d epsi,
                                  const __m512d om,
                                  __m512d * __restrict kr,
                                  __m512d * __restrict ki) {

                        __m512d sqrr,sqri;
                        __m512d t0r,t0i;
                        __m512d wrkc = _mm512_setzero_pd();
                        cmul_zmm8r8(mur,mui,epsr,epsi,&t0r,&t0i);
                        csqrt_zmm8r8(t0r,t0i,&wrkc,&sqrr,&sqri);
                        *kr = _mm512_mul_pd(om,sqrr);
                        *ki = _mm512_mul_pd(om,sqri);
               }  


              ////////////////////////////////////////////////////////////////////////////////


                __m512d rc_zmm8r8(   const __m512d x,
                                     const __m512d y,
                                     const __m512d errtot) {

                          const register __m512d c1 = _mm512_set1_pd(0.142857142857142857142857142857);
                          const register __m512d c2 = _mm512_set1_pd(0.409090909090909090909090909091);
                          const register __m512d _1 = _mm512_set1_pd(1.0);
                          const register __m512d thr= _mm512_set1_pd(0.333333333333333333333333333333);
                          const register __m512d c3 = _mm512_set1_pd(0.375);
                          const register __m512d _2 = _mm512_set1_pd(2.0);
                          const register __m512d qtr= _mm512_set1_pd(0.25);
                          const register __m512d c4 = _mm512_set1_pd(0.3);
                          register __m512d rc,xn,yn,mu,s,sn,lamda;
                          xn = x;
                          yn = y;
                          
                          while(true) {

                                mu = _mm512_mul_pd(_mm512_add_pd(xn,
                                                          _mm512_add_pd(yn,yn)),thr);
                                sn = _mm512_div_pd(_mm512_add_pd(yn,mu),
                                                   _mm512_sub_pd(mu,_2));
                                if(_mm512_cmp_mask_pd(
                                             _mm512_abs_pd(sn),errtot,_CMP_LT_OQ)) {
                                   register __m512d t0 = _mm512_fmadd_pd(sn,c2,c3);
                                   register __m512d t1 = _mm512_fmadd_pd(t0,sn,c1);
                                   register __m512d t2 = _mm512_fmadd_pd(t1,sn,c4);
                                   s                  = _mm512_mul_pd(_mm512_mul_pd(sn,sn),t2);
                                   rc                 = _mm512_div_pd(_mm512_add_pd(_1,s),
                                                                      _mm512_sqrt_pd(mu));
                                   return (rc);
                                }
                                register __m512d sxn = _mm512_sqrt_pd(xn);
                                register __m512d syn = _mm512_sqrt_pd(yn);
                                lamda =  _mm512_fmadd_pd(_mm512_mul_pd(_2,sxn),syn,yn);
                                xn    = _mm512_mul_pd(_mm512_add_pd(xn,lamda),qtr);
                                yn    = _mm512_mul_pd(_mm512_add_pd(ym,lamda),qtr);
                         }
                } 


                __m512d rc_zmm8r8_a(   const double * __restrict __attributes__((aligned(64))) px,
                                       const double * __restrict __attributes__((aligned(64))) py,
                                       const double * __restrict __attributes__((aligned(64))) perrtot) {

                          register __m512d x       = _mm512_load_pd(&px[0]);
                          register __m512d y       = _mm512_load_pd(&py[0]);
                          register __m512d errtot  = _mm512_load_pd(&perrtot[0]);
                          const register __m512d c1 = _mm512_set1_pd(0.142857142857142857142857142857);
                          const register __m512d c2 = _mm512_set1_pd(0.409090909090909090909090909091);
                          const register __m512d _1 = _mm512_set1_pd(1.0);
                          const register __m512d thr= _mm512_set1_pd(0.333333333333333333333333333333);
                          const register __m512d c3 = _mm512_set1_pd(0.375);
                          const register __m512d _2 = _mm512_set1_pd(2.0);
                          const register __m512d qtr= _mm512_set1_pd(0.25);
                          const register __m512d c4 = _mm512_set1_pd(0.3);
                          register __m512d rc,xn,yn,mu,s,sn,lamda;
                          xn = x;
                          yn = y;
                          
                          while(true) {

                                mu = _mm512_mul_pd(_mm512_add_pd(xn,
                                                          _mm512_add_pd(yn,yn)),thr);
                                sn = _mm512_div_pd(_mm512_add_pd(yn,mu),
                                                   _mm512_sub_pd(mu,_2));
                                if(_mm512_cmp_mask_pd(
                                             _mm512_abs_pd(sn),errtot,_CMP_LT_OQ)) {
                                   register __m512d t0 = _mm512_fmadd_pd(sn,c2,c3);
                                   register __m512d t1 = _mm512_fmadd_pd(t0,sn,c1);
                                   register __m512d t2 = _mm512_fmadd_pd(t1,sn,c4);
                                   s                  = _mm512_mul_pd(_mm512_mul_pd(sn,sn),t2);
                                   rc                 = _mm512_div_pd(_mm512_add_pd(_1,s),
                                                                      _mm512_sqrt_pd(mu));
                                   return (rc);
                                }
                                register __m512d sxn = _mm512_sqrt_pd(xn);
                                register __m512d syn = _mm512_sqrt_pd(yn);
                                lamda =  _mm512_fmadd_pd(_mm512_mul_pd(_2,sxn),syn,yn);
                                xn    = _mm512_mul_pd(_mm512_add_pd(xn,lamda),qtr);
                                yn    = _mm512_mul_pd(_mm512_add_pd(ym,lamda),qtr);
                         }
                }


                  __m512d rc_zmm8r8_u( const double * __restrict  px,
                                       const double * __restrict  py,
                                       const double * __restrict  perrtot) {

                          register __m512d x       = _mm512_loadu_pd(&px[0]);
                          register __m512d y       = _mm512_loadu_pd(&py[0]);
                          register __m512d errtot  = _mm512_loadu_pd(&perrtot[0]);
                          const register __m512d c1 = _mm512_set1_pd(0.142857142857142857142857142857);
                          const register __m512d c2 = _mm512_set1_pd(0.409090909090909090909090909091);
                          const register __m512d _1 = _mm512_set1_pd(1.0);
                          const register __m512d thr= _mm512_set1_pd(0.333333333333333333333333333333);
                          const register __m512d c3 = _mm512_set1_pd(0.375);
                          const register __m512d _2 = _mm512_set1_pd(2.0);
                          const register __m512d qtr= _mm512_set1_pd(0.25);
                          const register __m512d c4 = _mm512_set1_pd(0.3);
                          register __m512d rc,xn,yn,mu,s,sn,lamda;
                          xn = x;
                          yn = y;
                          
                          while(true) {

                                mu = _mm512_mul_pd(_mm512_add_pd(xn,
                                                          _mm512_add_pd(yn,yn)),thr);
                                sn = _mm512_div_pd(_mm512_add_pd(yn,mu),
                                                   _mm512_sub_pd(mu,_2));
                                if(_mm512_cmp_mask_pd(
                                             _mm512_abs_pd(sn),errtot,_CMP_LT_OQ)) {
                                   register __m512d t0 = _mm512_fmadd_pd(sn,c2,c3);
                                   register __m512d t1 = _mm512_fmadd_pd(t0,sn,c1);
                                   register __m512d t2 = _mm512_fmadd_pd(t1,sn,c4);
                                   s                  = _mm512_mul_pd(_mm512_mul_pd(sn,sn),t2);
                                   rc                 = _mm512_div_pd(_mm512_add_pd(_1,s),
                                                                      _mm512_sqrt_pd(mu));
                                   return (rc);
                                }
                                register __m512d sxn = _mm512_sqrt_pd(xn);
                                register __m512d syn = _mm512_sqrt_pd(yn);
                                lamda =  _mm512_fmadd_pd(_mm512_mul_pd(_2,sxn),syn,yn);
                                xn    = _mm512_mul_pd(_mm512_add_pd(xn,lamda),qtr);
                                yn    = _mm512_mul_pd(_mm512_add_pd(ym,lamda),qtr);
                         }
                }


                ////////////////////////////////////////////////////////////////////////


                 __m512d rd_zmm8r8(  const __m512d x,
                                     const __m512d y,
                                     const __m512d z,
                                     const __m512d errtot) {

                          const register __m512d _3 = _mm512_set1_pd(3.0);
                          const register __m512d _1 = _mm512_set1_pd(1.0);
                          const register __m512d c1 = _mm512_set1_pd(-0.214285714285714285714285714286);
                          const register __m512d c2 = _mm512_set1_pd(0.166666666666666666666666666667);
                          const register __m512d c3 = _mm512_set1_pd(-0.409090909090909090909090909091);
                          const register __m512d c4 = _mm512_set1_pd(0.115384615384615384615384615385);
                          const register __m512d c5 = _mm512_set1_pd(6.0);
                          const register __m512d c6 = _mm512_set1_pd(1.5);
                          const register __m512d c7 = _mm512_set1_pd(0.20;
                          const register __m512d c8 = _mm512_set1_pd(0.25);
                          register __m512d rd,xn,yn,zn,epslon,sigma,pow4,mu;
                          register __m512d xndev,yndev,zndev,ea,eb,ec,ed,ef;
                          register __m512d s1,s2,xnroot,ynroot,znroot,lamda;
                          register __m512d x0,x1,x2,x3,x4,x5;

                          xn    = x;
                          yn    = y;
                          zn    = z;
                          sigma = _mm512_setzero_pd();
                          pow4  = _1; 
                          while(true) {
                                mu    = _mm512_mul_pd(c7,_mm512_fmadd_pd(zn,_3,
                                                          _mm512_add_pd(xn,yn));
                                xndev = _mm512_div_pd(_mm512_sub_pd(mu,xn),mu);
                                yndev = _mm512_div_pd(_mm512_sub_pd(mu,yn),mu);
                                zndev = _mm512_div_pd(_mm512_sub_pd(mu,zn),mu);
                                epslon= _mm512_abs_pd(xndev);
                                epslon= _mm512_max_pd(epslon,_mm512_abs_pd(yndev));
                                epslon= _mm512_max_pd(epslon,_mm512_abs_pd(zndev));

                                if(_mm512_cmp_mask_pd(epslon,errtot,_CMP_LT_OQ)) {
                                   ea = _mm512_mul_pd(xndev,yndev);
                                   eb = _mm512_mul_pd(zndev,zndev);
                                   ec = _mm512_sub_pd(ea,eb);
                                   ed = _mm512_sub_pd(ea,_mm512_mul_pd(c5,eb));
                                   ef = _mm512_add_pd(ed,_mm512_add_pd(ec,ec));
                                   x0 = _mm512_fmadd_pd(c3,c8,c1);
                                   x1 = _mm512_sub_pd(ed,_mm512_sub_pd(c6,c4));
                                   x2 = _mm512_mul_pd(zndev,ef);
                                   s1 = _mm512_mul_pd(ed,_mm512_mul_pd(x0,
                                                               _mm512_mul_pd(x1,x2)));
                                   x3 = _mm512_fmadd_pd(c3,ec,_mm512_mul_pd(zndev,
                                                               _mm512_mul_pd(c4,ea)));
                                   x4 = _mm512_fmadd_pd(x3,zndev,_mm512_mul_pd(ef,c2));
                                   s2 = _mm512_mul_pd(zndev,x4);
                                   x0 = _mm512_fmadd_pd(_3,sigma,pow4);
                                   x1 = _mm512_add_pd(_1,_mm512_add_pd(s1,s2));
                                   x2 = _mm512_mul_pd(mu,_mm512_sqrt_pd(mu));
                                   rd = _mm512_div_pd(_mm512_mul_pd(x0,x1),x2);
                                   return (rd);
                                } 

                                xnroot = _mm512_sqrt_pd(xn);
                                ynroot = _mm512_sqrt_pd(yn);
                                znroot = _mm512_sqrt_pd(zn);
                                x0     = _mm512_fmadd_pd(ynroot,znroot,_mm512_add_pd(ynroot,znroot));
                                lamda  = _mm512_mul_pd(xnroot,x0);
                                sigma  = _mm512_div_pd(_mm512_add_pd(sigma,pow4),
                                                       _mm512_mul_pd(znroot,_mm512_add_pd(zn,lamda)));
                                pow4   = _mm512_mul_pd(pow4,c8);
                                xn     = _mm512_mul_pd(_mm512_add_pd(xn,lamda),c8);
                                yn     = _mm512_mul_pd(_mm512_add_pd(yn,lamda),c8);
                                zn     = _mm512_mul_pd(_mm512_add_pd(zn,lamda),c8);
                         }
                 }


                    __m512d rd_zmm8r8_a(const double * __restrict __attributes__((aligned(64))) px,
                                        const double * __restrict __attributes__((aligned(64))) py,
                                        const double * __restrict __attributes__((aligned(64))) pz,
                                        const double * __restrict __attributes__((aligned(64))) perrtot) {

                          register __m512d x       = _mm512_load_pd(&px[0]);
                          register __m512d y       = _mm512_load_pd(&py[0]);
                          register __m512d z       = _mm512_load_pd(&pz[0]);
                          register __m512d errtot  = _mm512_load_pd(&perrtot[0]);
                          const register __m512d _3 = _mm512_set1_pd(3.0);
                          const register __m512d _1 = _mm512_set1_pd(1.0);
                          const register __m512d c1 = _mm512_set1_pd(-0.214285714285714285714285714286);
                          const register __m512d c2 = _mm512_set1_pd(0.166666666666666666666666666667);
                          const register __m512d c3 = _mm512_set1_pd(-0.409090909090909090909090909091);
                          const register __m512d c4 = _mm512_set1_pd(0.115384615384615384615384615385);
                          const register __m512d c5 = _mm512_set1_pd(6.0);
                          const register __m512d c6 = _mm512_set1_pd(1.5);
                          const register __m512d c7 = _mm512_set1_pd(0.2);
                          const register __m512d c8 = _mm512_set1_pd(0.25);
                          register __m512d rd,xn,yn,zn,epslon,sigma,pow4,mu;
                          register __m512d xndev,yndev,zndev,ea,eb,ec,ed,ef;
                          register __m512d s1,s2,xnroot,ynroot,znroot,lamda;
                          register __m512d x0,x1,x2,x3,x4,x5;

                          xn    = x;
                          yn    = y;
                          zn    = z;
                          sigma = _mm512_setzero_pd();
                          pow4  = _1; 
                          while(true) {
                                mu    = _mm512_mul_pd(c7,_mm512_fmadd_pd(zn,_3,
                                                          _mm512_add_pd(xn,yn));
                                xndev = _mm512_div_pd(_mm512_sub_pd(mu,xn),mu);
                                yndev = _mm512_div_pd(_mm512_sub_pd(mu,yn),mu);
                                zndev = _mm512_div_pd(_mm512_sub_pd(mu,zn),mu);
                                epslon= _mm512_abs_pd(xndev);
                                epslon= _mm512_max_pd(epslon,_mm512_abs_pd(yndev));
                                epslon= _mm512_max_pd(epslon,_mm512_abs_pd(zndev));

                                if(_mm512_cmp_mask_pd(epslon,errtot,_CMP_LT_OQ)) {
                                   ea = _mm512_mul_pd(xndev,yndev);
                                   eb = _mm512_mul_pd(zndev,zndev);
                                   ec = _mm512_sub_pd(ea,eb);
                                   ed = _mm512_sub_pd(ea,_mm512_mul_pd(c5,eb));
                                   ef = _mm512_add_pd(ed,_mm512_add_pd(ec,ec));
                                   x0 = _mm512_fmadd_pd(c3,c8,c1);
                                   x1 = _mm512_sub_pd(ed,_mm512_sub_pd(c6,c4));
                                   x2 = _mm512_mul_pd(zndev,ef);
                                   s1 = _mm512_mul_pd(ed,_mm512_mul_pd(x0,
                                                               _mm512_mul_pd(x1,x2)));
                                   x3 = _mm512_fmadd_pd(c3,ec,_mm512_mul_pd(zndev,
                                                               _mm512_mul_pd(c4,ea)));
                                   x4 = _mm512_fmadd_pd(x3,zndev,_mm512_mul_pd(ef,c2));
                                   s2 = _mm512_mul_pd(zndev,x4);
                                   x0 = _mm512_fmadd_pd(_3,sigma,pow4);
                                   x1 = _mm512_add_pd(_1,_mm512_add_pd(s1,s2));
                                   x2 = _mm512_mul_pd(mu,_mm512_sqrt_pd(mu));
                                   rd = _mm512_div_pd(_mm512_mul_pd(x0,x1),x2);
                                   return (rd);
                                } 

                                xnroot = _mm512_sqrt_pd(xn);
                                ynroot = _mm512_sqrt_pd(yn);
                                znroot = _mm512_sqrt_pd(zn);
                                x0     = _mm512_fmadd_pd(ynroot,znroot,_mm512_add_pd(ynroot,znroot));
                                lamda  = _mm512_mul_pd(xnroot,x0);
                                sigma  = _mm512_div_pd(_mm512_add_pd(sigma,pow4),
                                                       _mm512_mul_pd(znroot,_mm512_add_pd(zn,lamda)));
                                pow4   = _mm512_mul_pd(pow4,c8);
                                xn     = _mm512_mul_pd(_mm512_add_pd(xn,lamda),c8);
                                yn     = _mm512_mul_pd(_mm512_add_pd(yn,lamda),c8);
                                zn     = _mm512_mul_pd(_mm512_add_pd(zn,lamda),c8);
                         }
                 }


                    __m512d rd_zmm8r8_u(const double * __restrict  px,
                                        const double * __restrict  py,
                                        const double * __restrict  pz,
                                        const double * __restrict  perrtot) {

                          register __m512d x       = _mm512_loadu_pd(&px[0]);
                          register __m512d y       = _mm512_loadu_pd(&py[0]);
                          register __m512d z       = _mm512_loadu_pd(&pz[0]);
                          register __m512d errtot  = _mm512_loadu_pd(&perrtot[0]);
                          const register __m512d _3 = _mm512_set1_pd(3.0);
                          const register __m512d _1 = _mm512_set1_pd(1.0);
                          const register __m512d c1 = _mm512_set1_pd(-0.214285714285714285714285714286);
                          const register __m512d c2 = _mm512_set1_pd(0.166666666666666666666666666667);
                          const register __m512d c3 = _mm512_set1_pd(-0.409090909090909090909090909091);
                          const register __m512d c4 = _mm512_set1_pd(0.115384615384615384615384615385);
                          const register __m512d c5 = _mm512_set1_pd(6.0);
                          const register __m512d c6 = _mm512_set1_pd(1.5);
                          const register __m512d c7 = _mm512_set1_pd(0.2);
                          const register __m512d c8 = _mm512_set1_pd(0.25);
                          register __m512d rd,xn,yn,zn,epslon,sigma,pow4,mu;
                          register __m512d xndev,yndev,zndev,ea,eb,ec,ed,ef;
                          register __m512d s1,s2,xnroot,ynroot,znroot,lamda;
                          register __m512d x0,x1,x2,x3,x4,x5;

                          xn    = x;
                          yn    = y;
                          zn    = z;
                          sigma = _mm512_setzero_pd();
                          pow4  = _1; 
                          while(true) {
                                mu    = _mm512_mul_pd(c7,_mm512_fmadd_pd(zn,_3,
                                                          _mm512_add_pd(xn,yn));
                                xndev = _mm512_div_pd(_mm512_sub_pd(mu,xn),mu);
                                yndev = _mm512_div_pd(_mm512_sub_pd(mu,yn),mu);
                                zndev = _mm512_div_pd(_mm512_sub_pd(mu,zn),mu);
                                epslon= _mm512_abs_pd(xndev);
                                epslon= _mm512_max_pd(epslon,_mm512_abs_pd(yndev));
                                epslon= _mm512_max_pd(epslon,_mm512_abs_pd(zndev));

                                if(_mm512_cmp_mask_pd(epslon,errtot,_CMP_LT_OQ)) {
                                   ea = _mm512_mul_pd(xndev,yndev);
                                   eb = _mm512_mul_pd(zndev,zndev);
                                   ec = _mm512_sub_pd(ea,eb);
                                   ed = _mm512_sub_pd(ea,_mm512_mul_pd(c5,eb));
                                   ef = _mm512_add_pd(ed,_mm512_add_pd(ec,ec));
                                   x0 = _mm512_fmadd_pd(c3,c8,c1);
                                   x1 = _mm512_sub_pd(ed,_mm512_sub_pd(c6,c4));
                                   x2 = _mm512_mul_pd(zndev,ef);
                                   s1 = _mm512_mul_pd(ed,_mm512_mul_pd(x0,
                                                               _mm512_mul_pd(x1,x2)));
                                   x3 = _mm512_fmadd_pd(c3,ec,_mm512_mul_pd(zndev,
                                                               _mm512_mul_pd(c4,ea)));
                                   x4 = _mm512_fmadd_pd(x3,zndev,_mm512_mul_pd(ef,c2));
                                   s2 = _mm512_mul_pd(zndev,x4);
                                   x0 = _mm512_fmadd_pd(_3,sigma,pow4);
                                   x1 = _mm512_add_pd(_1,_mm512_add_pd(s1,s2));
                                   x2 = _mm512_mul_pd(mu,_mm512_sqrt_pd(mu));
                                   rd = _mm512_div_pd(_mm512_mul_pd(x0,x1),x2);
                                   return (rd);
                                } 

                                xnroot = _mm512_sqrt_pd(xn);
                                ynroot = _mm512_sqrt_pd(yn);
                                znroot = _mm512_sqrt_pd(zn);
                                x0     = _mm512_fmadd_pd(ynroot,znroot,_mm512_add_pd(ynroot,znroot));
                                lamda  = _mm512_mul_pd(xnroot,x0);
                                sigma  = _mm512_div_pd(_mm512_add_pd(sigma,pow4),
                                                       _mm512_mul_pd(znroot,_mm512_add_pd(zn,lamda)));
                                pow4   = _mm512_mul_pd(pow4,c8);
                                xn     = _mm512_mul_pd(_mm512_add_pd(xn,lamda),c8);
                                yn     = _mm512_mul_pd(_mm512_add_pd(yn,lamda),c8);
                                zn     = _mm512_mul_pd(_mm512_add_pd(zn,lamda),c8);
                         }
                 }


                 ///////////////////////////////////////////////////////////////////////////////


 static const __m512d sn[6] = { 
 _mm512_set1_pd(-2.99181919401019853726E3),
 _mm512_set1_pd(7.08840045257738576863E5),
 _mm512_set1_pd(-6.29741486205862506537E7),
 _mm512_set1_pd(2.54890880573376359104E9),
 _mm512_set1_pd(-4.42979518059697779103E10),
 _mm512_set1_pd(3.18016297876567817986E11)};

static const __m512d sd[6] = {
 _mm512_set1_pd(2.81376268889994315696E2),
 _mm512_set1_pd(4.55847810806532581675E4),
 _mm512_set1_pd(5.17343888770096400730E6),
 _mm512_set1_pd(4.19320245898111231129E8),
 _mm512_set1_pd(2.24411795645340920940E10),
 _mm512_set1_pd(6.07366389490084639049E11)};

static const __m512d cn[6] = {
 _mm512_set1_pd(-4.98843114573573548651E-8),
 _mm512_set1_pd(9.50428062829859605134E-6),
 _mm512_set1_pd(-6.45191435683965050962E-4),
 _mm512_set1_pd(1.88843319396703850064E-2),
 _mm512_set1_pd(-2.05525900955013891793E-1),
 _mm512_set1_pd(9.99999999999999998822E-1)};

static const __m512d cd[7] = {
 _mm512_set1_pd(3.99982968972495980367E-12),
 _mm512_set1_pd(9.15439215774657478799E-10),
 _mm512_set1_pd(1.25001862479598821474E-7),
 _mm512_set1_pd(1.22262789024179030997E-5),
 _mm512_set1_pd(8.68029542941784300606E-4),
 _mm512_set1_pd(4.12142090722199792936E-2),
 _mm512_set1_pd(1.00000000000000000118E0)};

static const __m512d fn[10] = {
  _mm512_set1_pd(4.21543555043677546506E-1),
  _mm512_set1_pd(1.43407919780758885261E-1),
  _mm512_set1_pd(1.15220955073585758835E-2),
  _mm512_set1_pd(3.45017939782574027900E-4),
  _mm512_set1_pd(4.63613749287867322088E-6),
  _mm512_set1_pd(3.05568983790257605827E-8),
  _mm512_set1_pd(1.02304514164907233465E-10),
  _mm512_set1_pd(1.72010743268161828879E-13),
  _mm512_set1_pd(1.34283276233062758925E-16),
  _mm512_set1_pd(3.76329711269987889006E-20)};

static const __m512d fd[10] = {
  _mm512_set1_pd(7.51586398353378947175E-1),
  _mm512_set1_pd(1.16888925859191382142E-1),
  _mm512_set1_pd(6.44051526508858611005E-3),
  _mm512_set1_pd(1.55934409164153020873E-4),
  _mm512_set1_pd(1.84627567348930545870E-6),
  _mm512_set1_pd(1.12699224763999035261E-8),
  _mm512_set1_pd(3.60140029589371370404E-11),
  _mm512_set1_pd(5.88754533621578410010E-14),
  _mm512_set1_pd(4.52001434074129701496E-17),
  _mm512_set1_pd(1.25443237090011264384E-20)};

static const __m512d gn[11] = {
  _mm512_set1_pd(5.04442073643383265887E-1),
  _mm512_set1_pd(1.97102833525523411709E-1),
  _mm512_set1_pd(1.87648584092575249293E-2),
  _mm512_set1_pd(6.84079380915393090172E-4),
  _mm512_set1_pd(1.15138826111884280931E-5),
  _mm512_set1_pd(9.82852443688422223854E-8),
  _mm512_set1_pd(4.45344415861750144738E-10),
  _mm512_set1_pd(1.08268041139020870318E-12),
  _mm512_set1_pd(1.37555460633261799868E-15),
  _mm512_set1_pd(8.36354435630677421531E-19),
  _mm512_set1_pd(1.86958710162783235106E-22)};

static const __m512d gd[11] = {
  _mm512_set1_pd(1.47495759925128324529E0),
  _mm512_set1_pd(3.37748989120019970451E-1),
  _mm512_set1_pd(2.53603741420338795122E-2),
  _mm512_set1_pd(8.14679107184306179049E-4),
  _mm512_set1_pd(1.27545075667729118702E-5),
  _mm512_set1_pd(1.04314589657571990585E-7),
  _mm512_set1_pd(4.60680728146520428211E-10),
  _mm512_set1_pd(1.10273215066240270757E-12),
  _mm512_set1_pd(1.38796531259578871258E-15),
  _mm512_set1_pd(8.39158816283118707363E-19),
  _mm512_set1_pd(1.86958710162783236342E-22)};




                     void fresnel_zmm8r8(const __m512d xxa,
                                          __m512d * __restrict ssa,
                                          __m512d * __restrict cca) {

                        
                        const __m512d c0   = _mm512_set1_pd(2.5625);
                        const __m512d c1   = _mm512_set1_pd(36974.0);
                        const __m512d hlf  = _mm512_set1_pd(0.5);
                        const __m512d _0   = _mm512_setzero_pd();
                        const __m512d _1   = _mm512_set1_pd(1.0); 
                        const __m512d pi   = _mm512_set1_pd(3.14159265358979323846264338328);
                        const __m512d pio2 = _mm512_set1_pd(1.57079632679489661923132169164);
                        register __m512d f,g,cc,ss,c,s,t,u,t0,t1;
                        register __m512d x,x2,acc1,acc2,acc3,acc4;
                       
                        x   = _mm512_abs_pd(xxa);
                        x2  = _mm512_mul_pd(x,x);
                        if(_mm512_cmp_pd_mask(x,c0,_CMP_LT_OQ)) {
			   volatile __m512d prefsn = _mm_prefetch((const char*)&sn[0],_MM_HINT_T0);
                           volatile __m512d prefsd = _mm_prefetch((const char*)&sd[0],_MM_HINT_T0);
                           volatile __m512d prefcn = _mm_prefetch((const char*)&cn[0],_MM_HINT_T0);
                           volatile __m512d prefcd = _mm_prefetch((const char*)&cd[0],_MM_HINT_T0);
                           t = _mm512_mul_pd(x2,x2);
                           acc1 = sn[0]; 
                           acc2 = _mm512_add_pd(t,sd[0]);
                           acc3 = cn[0];
                           acc4 = cd[0];
                           acc1 = _mm512_fmadd_pd(acc1,t,sn[1]);
                           acc2 = _mm512_fmadd_pd(acc2,t,sd[1]);
                           acc1 = _mm512_fmadd_pd(acc1,t,sn[2]);
                           acc2 = _mm512_fmadd_pd(acc2,t,sd[2]);
                           acc1 = _mm512_fmadd_pd(acc1,t,sn[3]);
                           acc2 = _mm512_fmadd_pd(acc2,t,sd[3]);
                           acc1 = _mm512_fmadd_pd(acc1,t,sn[4]);
                           acc2 = _mm512_fmadd_pd(acc2,t,sd[4]);
                           acc2 = _mm512_fmadd_pd(acc2,t,sd[5]);
                           t0   = _mm512_div_pd(acc1,acc2);
                           ss   = _mm512_mul_pd(_mm512_mul_pd(x,x2),t0);
                           acc3 = _mm512_fmadd_pd(acc3,t,cn[1]);
                           acc4 = _mm512_fmadd_pd(acc4,t,cd[1]);
                           acc3 = _mm512_fmadd_pd(acc3,t,cn[2]);
                           acc4 = _mm512_fmadd_pd(acc4,t,cd[2]);
                           acc3 = _mm512_fmadd_pd(acc3,t,cn[3]);
                           acc4 = _mm512_fmadd_pd(acc4,t,cd[3]);
                           acc3 = _mm512_fmadd_pd(acc3,t,cn[4]);
                           acc4 = _mm512_fmadd_pd(acc4,t,cd[5]);
                           acc4 = _mm512_fmadd_pd(acc4,t,cd[6]);
                           t1   = _mm512_div_pd(acc3,acc4);
                           cc   = _mm512_mul_pd(x,t1);
                           goto done;
                        }

                       if(_mm512_cmp_pd_mask(x,c1,_CMP_GT_OQ)) {
                          cc = hlf;
                          ss = hlf;
                          goto done;
                      }

                      /*		Asymptotic power series auxiliary functions
                       *		for large argument
                       */

                        volatile __m512d prefsn = _mm_prefetch((const char*)&fn[0],_MM_HINT_T0);
                        volatile __m512d prefsd = _mm_prefetch((const char*)&fd[0],_MM_HINT_T0);
                        volatile __m512d prefcn = _mm_prefetch((const char*)&gn[0],_MM_HINT_T0);
                        volatile __m512d prefcd = _mm_prefetch((const char*)&gd[0],_MM_HINT_T0);
                        t = _mm512_mul_pd(pi,x2);
                        u = _mm512_div_pd(_1,_mm512_mul_pd(t,t));
                        acc1 = fn[0];
                        acc2 = _mm512_add_pd(u,fd[0]);
                        acc3 = gn[0];
                        acc4 = _mm512_add_pd(u,gd[0]);
                        t = _mm512_div_pd(_1,t);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[1]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[1]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[2]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[2]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[3]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[3]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[4]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[4]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[5]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[5]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[6]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[6]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[7]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[7]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[8]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[8]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[9]);
                        t0   = _mm512_div_pd(acc1,acc2);
                        f    = _mm512_sub_pd(_1,_mm512_mul_pd(u,t0));
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[1]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[1]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[2]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[2]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[3]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[3]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[4]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[4]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[5]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[5]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[6]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[6]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[7]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[7]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[8]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[8]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gd[9]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[10]);
                        t1   = _mm512_div_pd(acc3,acc4);
                        g    = _mm512_mul_pd(t,t1);
                        
                        t    = _mm512_mul_pd(pio2,x2);
                        c    = xcosf(t);
                        s    = xsinf(t);
                        t    = _mm512_mul_pd(pi,x);
                        t0   = _mm512_fmsub_pd(f,s,_mm512_mul_pd(g,c));
                        cc   = _mm512_add_pd(hlf,_mm512_div_pd(t0,t));
                        t1   = _mm512_fmadd_pd(f,c,_mm512_mul_pd(g,s));
                        ss   = _mm512_sub_pd(hlf,_mm512_div_pd(t1,t));
done:
                     if(_mm512_cmp_pd_mask(xxa,
                                     _mm512_setzero_pd(),_CMP_LT_OQ)) {
                         cc = negate_zmm8r8(cc);
                         ss = negate_zmm8r8(ss);
                     }
                     
                     *cca = cc;
                     *ssa = ss;
              }


                 void fresnel_zmm8r8_a(  const double * __restrict __attributes__((aligned(64))) pxxa,
                                          double * __restrict __attributes__((aligned(64))) ssa,
                                          double * __restrict __attributes__((aligned(64))) cca) {

                        
                        register __m512d xxa = _mm512_load_pd(&pxxa[0]);
                        const __m512d c0   = _mm512_set1_pd(2.5625);
                        const __m512d c1   = _mm512_set1_pd(36974.0);
                        const __m512d hlf  = _mm512_set1_pd(0.5);
                        const __m512d _0   = _mm512_setzero_pd();
                        const __m512d _1   = _mm512_set1_pd(1.0f); 
                        const __m512d pi   = _mm512_set1_pd(3.14159265358979323846264338328);
                        const __m512d pio2 = _mm512_set1_pd(1.57079632679489661923132169164);
                        register __m512d f,g,cc,ss,c,s,t,u,t0,t1;
                        register __m512d x,x2,acc1,acc2,acc3,acc4;
                       
                        x   = _mm512_abs_pd(xxa);
                        x2  = _mm512_mul_pd(x,x);
                        if(_mm512_cmp_pd_mask(x,c0,_CMP_LT_OQ)) {
			   volatile __m512d prefsn = _mm_prefetch((const char*)&sn[0],_MM_HINT_T0);
                           volatile __m512d prefsd = _mm_prefetch((const char*)&sd[0],_MM_HINT_T0);
                           volatile __m512d prefcn = _mm_prefetch((const char*)&cn[0],_MM_HINT_T0);
                           volatile __m512d prefcd = _mm_prefetch((const char*)&cd[0],_MM_HINT_T0);
                           t = _mm512_mul_pd(x2,x2);
                           acc1 = sn[0]; 
                           acc2 = _mm512_add_pd(t,sd[0]);
                           acc3 = cn[0];
                           acc4 = cd[0];
                           acc1 = _mm512_fmadd_pd(acc1,t,sn[1]);
                           acc2 = _mm512_fmadd_pd(acc2,t,sd[1]);
                           acc1 = _mm512_fmadd_pd(acc1,t,sn[2]);
                           acc2 = _mm512_fmadd_pd(acc2,t,sd[2]);
                           acc1 = _mm512_fmadd_pd(acc1,t,sn[3]);
                           acc2 = _mm512_fmadd_pd(acc2,t,sd[3]);
                           acc1 = _mm512_fmadd_pd(acc1,t,sn[4]);
                           acc2 = _mm512_fmadd_pd(acc2,t,sd[4]);
                           acc2 = _mm512_fmadd_pd(acc2,t,sd[5]);
                           t0   = _mm512_div_pd(acc1,acc2);
                           ss   = _mm512_mul_pd(_mm512_mul_pd(x,x2),t0);
                           acc3 = _mm512_fmadd_pd(acc3,t,cn[1]);
                           acc4 = _mm512_fmadd_pd(acc4,t,cd[1]);
                           acc3 = _mm512_fmadd_pd(acc3,t,cn[2]);
                           acc4 = _mm512_fmadd_pd(acc4,t,cd[2]);
                           acc3 = _mm512_fmadd_pd(acc3,t,cn[3]);
                           acc4 = _mm512_fmadd_pd(acc4,t,cd[3]);
                           acc3 = _mm512_fmadd_pd(acc3,t,cn[4]);
                           acc4 = _mm512_fmadd_pd(acc4,t,cd[5]);
                           acc4 = _mm512_fmadd_pd(acc4,t,cd[6]);
                           t1   = _mm512_div_pd(acc3,acc4);
                           cc   = _mm512_mul_pd(x,t1);
                           goto done;
                        }

                       if(_mm512_cmp_pd_mask(x,c1,_CMP_GT_OQ)) {
                          cc = hlf;
                          ss = hlf;
                          goto done;
                      }

                      /*		Asymptotic power series auxiliary functions
                       *		for large argument
                       */

                        volatile __m512d prefsn = _mm_prefetch((const char*)&fn[0],_MM_HINT_T0);
                        volatile __m512d prefsd = _mm_prefetch((const char*)&fd[0],_MM_HINT_T0);
                        volatile __m512d prefcn = _mm_prefetch((const char*)&gn[0],_MM_HINT_T0);
                        volatile __m512d prefcd = _mm_prefetch((const char*)&gd[0],_MM_HINT_T0);
                        t = _mm512_mul_pd(pi,x2);
                        u = _mm512_div_pd(_1,_mm512_mul_pd(t,t));
                        acc1 = fn[0];
                        acc2 = _mm512_add_pd(u,fd[0]);
                        acc3 = gn[0];
                        acc4 = _mm512_add_pd(u,gd[0]);
                        t = _mm512_div_pd(_1,t);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[1]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[1]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[2]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[2]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[3]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[3]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[4]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[4]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[5]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[5]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[6]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[6]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[7]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[7]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[8]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[8]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[9]);
                        t0   = _mm512_div_pd(acc1,acc2);
                        f    = _mm512_sub_pd(_1,_mm512_mul_pd(u,t0));
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[1]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[1]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[2]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[2]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[3]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[3]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[4]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[4]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[5]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[5]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[6]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[6]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[7]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[7]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[8]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[8]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gd[9]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[10]);
                        t1   = _mm512_div_pd(acc3,acc4);
                        g    = _mm512_mul_pd(t,t1);
                        
                        t    = _mm512_mul_pd(pio2,x2);
                        c    = xcosf(t);
                        s    = xsinf(t);
                        t    = _mm512_mul_pd(pi,x);
                        t0   = _mm512_fmsub_pd(f,s,_mm512_mul_pd(g,c));
                        cc   = _mm512_add_pd(hlf,_mm512_div_pd(t0,t));
                        t1   = _mm512_fmadd_pd(f,c,_mm512_mul_pd(g,s));
                        ss   = _mm512_sub_pd(hlf,_mm512_div_pd(t1,t));
done:
                     if(_mm512_cmp_pd_mask(xxa,
                                     _mm512_setzero_pd(),_CMP_LT_OQ)) {
                         cc = negate_zmm8r8(cc);
                         ss = negate_zmm8r8(ss);
                     }
                     
                     _mm512_store_pd(&cca[0] ,cc);
                     _mm512_store_pd(&ssa[0] ,ss);
              }


                 void fresnel_zmm8r8_u(  const double * __restrict  pxxa,
                                          double * __restrict  ssa,
                                          double * __restrict  cca) {

                      
                        register __m512d xxa = _mm512_loadu_pd(&pxxa[0]);
                        const __m512d c0   = _mm512_set1_pd(2.5625);
                        const __m512d c1   = _mm512_set1_pd(36974.0);
                        const __m512d hlf  = _mm512_set1_pd(0.5);
                        const __m512d _0   = _mm512_setzero_pd();
                        const __m512d _1   = _mm512_set1_pd(1.0); 
                        const __m512d pi   = _mm512_set1_pd(3.14159265358979323846264338328);
                        const __m512d pio2 = _mm512_set1_pd(1.57079632679489661923132169164);
                        register __m512d f,g,cc,ss,c,s,t,u,t0,t1;
                        register __m512d x,x2,acc1,acc2,acc3,acc4;
                       
                        x   = _mm512_abs_pd(xxa);
                        x2  = _mm512_mul_pd(x,x);
                        if(_mm512_cmp_pd_mask(x,c0,_CMP_LT_OQ)) {
			   volatile __m512d prefsn = _mm_prefetch((const char*)&sn[0],_MM_HINT_T0);
                           volatile __m512d prefsd = _mm_prefetch((const char*)&sd[0],_MM_HINT_T0);
                           volatile __m512d prefcn = _mm_prefetch((const char*)&cn[0],_MM_HINT_T0);
                           volatile __m512d prefcd = _mm_prefetch((const char*)&cd[0],_MM_HINT_T0);
                           t = _mm512_mul_pd(x2,x2);
                           acc1 = sn[0]; 
                           acc2 = _mm512_add_pd(t,sd[0]);
                           acc3 = cn[0];
                           acc4 = cd[0];
                           acc1 = _mm512_fmadd_pd(acc1,t,sn[1]);
                           acc2 = _mm512_fmadd_pd(acc2,t,sd[1]);
                           acc1 = _mm512_fmadd_pd(acc1,t,sn[2]);
                           acc2 = _mm512_fmadd_pd(acc2,t,sd[2]);
                           acc1 = _mm512_fmadd_pd(acc1,t,sn[3]);
                           acc2 = _mm512_fmadd_pd(acc2,t,sd[3]);
                           acc1 = _mm512_fmadd_pd(acc1,t,sn[4]);
                           acc2 = _mm512_fmadd_pd(acc2,t,sd[4]);
                           acc2 = _mm512_fmadd_pd(acc2,t,sd[5]);
                           t0   = _mm512_div_pd(acc1,acc2);
                           ss   = _mm512_mul_pd(_mm512_mul_pd(x,x2),t0);
                           acc3 = _mm512_fmadd_pd(acc3,t,cn[1]);
                           acc4 = _mm512_fmadd_pd(acc4,t,cd[1]);
                           acc3 = _mm512_fmadd_pd(acc3,t,cn[2]);
                           acc4 = _mm512_fmadd_pd(acc4,t,cd[2]);
                           acc3 = _mm512_fmadd_pd(acc3,t,cn[3]);
                           acc4 = _mm512_fmadd_pd(acc4,t,cd[3]);
                           acc3 = _mm512_fmadd_pd(acc3,t,cn[4]);
                           acc4 = _mm512_fmadd_pd(acc4,t,cd[5]);
                           acc4 = _mm512_fmadd_pd(acc4,t,cd[6]);
                           t1   = _mm512_div_pd(acc3,acc4);
                           cc   = _mm512_mul_pd(x,t1);
                           goto done;
                        }

                       if(_mm512_cmp_pd_mask(x,c1,_CMP_GT_OQ)) {
                          cc = hlf;
                          ss = hlf;
                          goto done;
                      }

                      /*		Asymptotic power series auxiliary functions
                       *		for large argument
                       */

                        volatile __m512d prefsn = _mm_prefetch((const char*)&fn[0],_MM_HINT_T0);
                        volatile __m512d prefsd = _mm_prefetch((const char*)&fd[0],_MM_HINT_T0);
                        volatile __m512d prefcn = _mm_prefetch((const char*)&gn[0],_MM_HINT_T0);
                        volatile __m512d prefcd = _mm_prefetch((const char*)&gd[0],_MM_HINT_T0);
                        t = _mm512_mul_pd(pi,x2);
                        u = _mm512_div_pd(_1,_mm512_mul_pd(t,t));
                        acc1 = fn[0];
                        acc2 = _mm512_add_pd(u,fd[0]);
                        acc3 = gn[0];
                        acc4 = _mm512_add_pd(u,gd[0]);
                        t = _mm512_div_pd(_1,t);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[1]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[1]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[2]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[2]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[3]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[3]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[4]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[4]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[5]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[5]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[6]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[6]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[7]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[7]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[8]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[8]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[9]);
                        t0   = _mm512_div_pd(acc1,acc2);
                        f    = _mm512_sub_pd(_1,_mm512_mul_pd(u,t0));
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[1]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[1]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[2]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[2]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[3]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[3]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[4]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[4]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[5]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[5]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[6]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[6]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[7]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[7]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[8]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[8]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gd[9]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[10]);
                        t1   = _mm512_div_pd(acc3,acc4);
                        g    = _mm512_mul_pd(t,t1);
                        
                        t    = _mm512_mul_pd(pio2,x2);
                        c    = xcosf(t);
                        s    = xsinf(t);
                        t    = _mm512_mul_pd(pi,x);
                        t0   = _mm512_fmsub_pd(f,s,_mm512_mul_pd(g,c));
                        cc   = _mm512_add_pd(hlf,_mm512_div_pd(t0,t));
                        t1   = _mm512_fmadd_pd(f,c,_mm512_mul_pd(g,s));
                        ss   = _mm512_sub_pd(hlf,_mm512_div_pd(t1,t));
done:
                     if(_mm512_cmp_pd_mask(xxa,
                                     _mm512_setzero_pd(),_CMP_LT_OQ)) {
                         cc = negate_zmm8r8(cc);
                         ss = negate_zmm8r8(ss);
                     }
                     
                     _mm512_storeu_pd(&cca[0] ,cc);
                     _mm512_storeu_pd(&ssa[0] ,ss);
              }


             //////////////////////////////////////////////////////////////////////////////


                
                  /*
                           Same as above -- divided into Fresnel 'C' integral
                           and Fresnel 'S' integral.
                     */


              __m512d fresnel_C_zmm8r8(const __m512d xxa) {
                                        
                        
                        const __m512d c0   = _mm512_set1_pd(2.5625);
                        const __m512d c1   = _mm512_set1_pd(36974.0);
                        const __m512d hlf  = _mm512_set1_pd(0.5);
                        const __m512d _0   = _mm512_setzero_pd();
                        const __m512d _1   = _mm512_set1_pd(1.0); 
                        const __m512d pi   = _mm512_set1_pd(3.14159265358979323846264338328);
                        const __m512d pio2 = _mm512_set1_pd(1.57079632679489661923132169164);
                        register __m512d f,g,cc,c,t,u,t0,t1;
                        register __m512d cca,x,x2,acc1,acc2,acc3,acc4;
                       
                        x   = _mm512_abs_pd(xxa);
                        x2  = _mm512_mul_pd(x,x);
                        if(_mm512_cmp_pd_mask(x,c0,_CMP_LT_OQ)) {
			   volatile __m512d prefcn = _mm_prefetch((const char*)&cn[0],_MM_HINT_T0);
                           volatile __m512d prefcd = _mm_prefetch((const char*)&cd[0],_MM_HINT_T0);
                          
                           t = _mm512_mul_pd(x2,x2);
                           acc3 = cn[0];
                           acc4 = cd[0];
                           acc3 = _mm512_fmadd_pd(acc3,t,cn[1]);
                           acc4 = _mm512_fmadd_pd(acc4,t,cd[1]);
                           acc3 = _mm512_fmadd_pd(acc3,t,cn[2]);
                           acc4 = _mm512_fmadd_pd(acc4,t,cd[2]);
                           acc3 = _mm512_fmadd_pd(acc3,t,cn[3]);
                           acc4 = _mm512_fmadd_pd(acc4,t,cd[3]);
                           acc3 = _mm512_fmadd_pd(acc3,t,cn[4]);
                           acc4 = _mm512_fmadd_pd(acc4,t,cd[5]);
                           acc4 = _mm512_fmadd_pd(acc4,t,cd[6]);
                           t1   = _mm512_div_pd(acc3,acc4);
                           cc   = _mm512_mul_pd(x,t1);
                           goto done;
                        }

                       if(_mm512_cmp_pd_mask(x,c1,_CMP_GT_OQ)) {
                          cc = hlf;
                          goto done;
                      }

                      /*		Asymptotic power series auxiliary functions
                       *		for large argument
                       */

                        volatile __m512d prefsn = _mm_prefetch((const char*)&fn[0],_MM_HINT_T0);
                        volatile __m512d prefsd = _mm_prefetch((const char*)&fd[0],_MM_HINT_T0);
                        volatile __m512d prefcn = _mm_prefetch((const char*)&gn[0],_MM_HINT_T0);
                        volatile __m512d prefcd = _mm_prefetch((const char*)&gd[0],_MM_HINT_T0);
                        t = _mm512_mul_pd(pi,x2);
                        u = _mm512_div_pd(_1,_mm512_mul_pd(t,t));
                        acc1 = fn[0];
                        acc2 = _mm512_add_pd(u,fd[0]);
                        acc3 = gn[0];
                        acc4 = _mm512_add_pd(u,gd[0]);
                        t = _mm512_div_pd(_1,t);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[1]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[1]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[2]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[2]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[3]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[3]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[4]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[4]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[5]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[5]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[6]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[6]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[7]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[7]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[8]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[8]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[9]);
                        t0   = _mm512_div_pd(acc1,acc2);
                        f    = _mm512_sub_pd(_1,_mm512_mul_pd(u,t0));
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[1]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[1]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[2]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[2]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[3]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[3]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[4]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[4]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[5]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[5]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[6]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[6]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[7]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[7]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[8]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[8]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gd[9]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[10]);
                        t1   = _mm512_div_pd(acc3,acc4);
                        g    = _mm512_mul_pd(t,t1);
                        
                        t    = _mm512_mul_pd(pio2,x2);
                        c    = xcosf(t);
                        t    = _mm512_mul_pd(pi,x);
                        t0   = _mm512_fmsub_pd(f,s,_mm512_mul_pd(g,c));
                        cc   = _mm512_add_pd(hlf,_mm512_div_pd(t0,t));
done:
                     if(_mm512_cmp_pd_mask(xxa,
                                     _mm512_setzero_pd(),_CMP_LT_OQ)) {
                         cc = negate_zmm8r8(cc);
                     }
                     
                     cca = cc;
                     return (cca);
              }


                __m512d fresnel_C_zmm8r8_a(const double * __restrict __attribute__((aligned(64))) pxxa) {
                                        
                       
                        register __m512d xxa = _mm512_load_pd(&pxxa[0]);
                        const __m512d c0   = _mm512_set1_pd(2.5625);
                        const __m512d c1   = _mm512_set1_pd(36974.0);
                        const __m512d hlf  = _mm512_set1_pd(0.5);
                        const __m512d _0   = _mm512_setzero_pd();
                        const __m512d _1   = _mm512_set1_pd(1.0); 
                        const __m512d pi   = _mm512_set1_pd(3.14159265358979323846264338328);
                        const __m512d pio2 = _mm512_set1_pd(1.57079632679489661923132169164);
                        register __m512d f,g,cc,c,t,u,t0,t1;
                        register __m512d cca,x,x2,acc1,acc2,acc3,acc4;
                       
                        x   = _mm512_abs_pd(xxa);
                        x2  = _mm512_mul_pd(x,x);
                        if(_mm512_cmp_pd_mask(x,c0,_CMP_LT_OQ)) {
			   volatile __m512d prefcn = _mm_prefetch((const char*)&cn[0],_MM_HINT_T0);
                           volatile __m512d prefcd = _mm_prefetch((const char*)&cd[0],_MM_HINT_T0);
                          
                           t = _mm512_mul_pd(x2,x2);
                           
                           acc3 = cn[0];
                           acc4 = cd[0];
                           acc3 = _mm512_fmadd_pd(acc3,t,cn[1]);
                           acc4 = _mm512_fmadd_pd(acc4,t,cd[1]);
                           acc3 = _mm512_fmadd_pd(acc3,t,cn[2]);
                           acc4 = _mm512_fmadd_pd(acc4,t,cd[2]);
                           acc3 = _mm512_fmadd_pd(acc3,t,cn[3]);
                           acc4 = _mm512_fmadd_pd(acc4,t,cd[3]);
                           acc3 = _mm512_fmadd_pd(acc3,t,cn[4]);
                           acc4 = _mm512_fmadd_pd(acc4,t,cd[5]);
                           acc4 = _mm512_fmadd_pd(acc4,t,cd[6]);
                           t1   = _mm512_div_pd(acc3,acc4);
                           cc   = _mm512_mul_pd(x,t1);
                           goto done;
                        }

                       if(_mm512_cmp_pd_mask(x,c1,_CMP_GT_OQ)) {
                          cc = hlf;
                          goto done;
                      }

                      /*		Asymptotic power series auxiliary functions
                       *		for large argument
                       */

                        volatile __m512d prefsn = _mm_prefetch((const char*)&fn[0],_MM_HINT_T0);
                        volatile __m512d prefsd = _mm_prefetch((const char*)&fd[0],_MM_HINT_T0);
                        volatile __m512d prefcn = _mm_prefetch((const char*)&gn[0],_MM_HINT_T0);
                        volatile __m512d prefcd = _mm_prefetch((const char*)&gd[0],_MM_HINT_T0);
                        t = _mm512_mul_pd(pi,x2);
                        u = _mm512_div_pd(_1,_mm512_mul_pd(t,t));
                        acc1 = fn[0];
                        acc2 = _mm512_add_pd(u,fd[0]);
                        acc3 = gn[0];
                        acc4 = _mm512_add_pd(u,gd[0]);
                        t = _mm512_div_pd(_1,t);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[1]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[1]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[2]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[2]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[3]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[3]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[4]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[4]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[5]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[5]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[6]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[6]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[7]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[7]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[8]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[8]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[9]);
                        t0   = _mm512_div_pd(acc1,acc2);
                        f    = _mm512_sub_pd(_1,_mm512_mul_pd(u,t0));
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[1]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[1]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[2]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[2]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[3]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[3]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[4]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[4]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[5]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[5]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[6]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[6]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[7]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[7]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[8]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[8]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gd[9]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[10]);
                        t1   = _mm512_div_pd(acc3,acc4);
                        g    = _mm512_mul_pd(t,t1);
                        
                        t    = _mm512_mul_pd(pio2,x2);
                        c    = xcosf(t);
                        t    = _mm512_mul_pd(pi,x);
                        t0   = _mm512_fmsub_pd(f,s,_mm512_mul_pd(g,c));
                        cc   = _mm512_add_pd(hlf,_mm512_div_pd(t0,t));
done:
                     if(_mm512_cmp_pd_mask(xxa,
                                     _mm512_setzero_pd(),_CMP_LT_OQ)) {
                         cc = negate_zmm8r8(cc);
                     }
                     
                     cca = cc;
                     return (cca);
              }


                __m512d fresnel_C_zmm8r8_u(const double * __restrict  pxxa) {
                                        
                        
                        register __m512d xxa = _mm512_loadu_pd(&pxxa[0]);
                        const __m512d c0   = _mm512_set1_pd(2.5625);
                        const __m512d c1   = _mm512_set1_pd(36974.0);
                        const __m512d hlf  = _mm512_set1_pd(0.5);
                        const __m512d _0   = _mm512_setzero_pd();
                        const __m512d _1   = _mm512_set1_pd(1.0); 
                        const __m512d pi   = _mm512_set1_pd(3.14159265358979323846264338328);
                        const __m512d pio2 = _mm512_set1_pd(1.57079632679489661923132169164);
                        register __m512d f,g,cc,c,t,u,t0,t1;
                        register __m512d cca,x,x2,acc1,acc2,acc3,acc4;
                       
                        x   = _mm512_abs_pd(xxa);
                        x2  = _mm512_mul_pd(x,x);
                        if(_mm512_cmp_pd_mask(x,c0,_CMP_LT_OQ)) {
			   volatile __m512d prefsn = _mm_prefetch((const char*)&sn[0],_MM_HINT_T0);
                           volatile __m512d prefsd = _mm_prefetch((const char*)&sd[0],_MM_HINT_T0);
                          
                           t = _mm512_mul_pd(x2,x2);
                           acc3 = cn[0];
                           acc4 = cd[0];
                           acc3 = _mm512_fmadd_pd(acc3,t,cn[1]);
                           acc4 = _mm512_fmadd_pd(acc4,t,cd[1]);
                           acc3 = _mm512_fmadd_pd(acc3,t,cn[2]);
                           acc4 = _mm512_fmadd_pd(acc4,t,cd[2]);
                           acc3 = _mm512_fmadd_pd(acc3,t,cn[3]);
                           acc4 = _mm512_fmadd_pd(acc4,t,cd[3]);
                           acc3 = _mm512_fmadd_pd(acc3,t,cn[4]);
                           acc4 = _mm512_fmadd_pd(acc4,t,cd[5]);
                           acc4 = _mm512_fmadd_pd(acc4,t,cd[6]);
                           t1   = _mm512_div_pd(acc3,acc4);
                           cc   = _mm512_mul_pd(x,t1);
                           goto done;
                        }

                       if(_mm512_cmp_pd_mask(x,c1,_CMP_GT_OQ)) {
                          cc = hlf;
                          goto done;
                      }

                      /*		Asymptotic power series auxiliary functions
                       *		for large argument
                       */

                        volatile __m512d prefsn = _mm_prefetch((const char*)&fn[0],_MM_HINT_T0);
                        volatile __m512d prefsd = _mm_prefetch((const char*)&fd[0],_MM_HINT_T0);
                        volatile __m512d prefcn = _mm_prefetch((const char*)&gn[0],_MM_HINT_T0);
                        volatile __m512d prefcd = _mm_prefetch((const char*)&gd[0],_MM_HINT_T0);
                        t = _mm512_mul_pd(pi,x2);
                        u = _mm512_div_pd(_1,_mm512_mul_pd(t,t));
                        acc1 = fn[0];
                        acc2 = _mm512_add_pd(u,fd[0]);
                        acc3 = gn[0];
                        acc4 = _mm512_add_pd(u,gd[0]);
                        t = _mm512_div_pd(_1,t);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[1]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[1]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[2]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[2]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[3]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[3]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[4]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[4]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[5]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[5]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[6]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[6]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[7]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[7]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[8]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[8]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[9]);
                        t0   = _mm512_div_pd(acc1,acc2);
                        f    = _mm512_sub_pd(_1,_mm512_mul_pd(u,t0));
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[1]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[1]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[2]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[2]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[3]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[3]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[4]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[4]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[5]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[5]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[6]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[6]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[7]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[7]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[8]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[8]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gd[9]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[10]);
                        t1   = _mm512_div_pd(acc3,acc4);
                        g    = _mm512_mul_pd(t,t1);
                        
                        t    = _mm512_mul_pd(pio2,x2);
                        c    = xcosf(t);
                        t    = _mm512_mul_pd(pi,x);
                        t0   = _mm512_fmsub_pd(f,s,_mm512_mul_pd(g,c));
                        cc   = _mm512_add_pd(hlf,_mm512_div_pd(t0,t));
done:
                     if(_mm512_cmp_pd_mask(xxa,
                                     _mm512_setzero_pd(),_CMP_LT_OQ)) {
                         cc = negate_zmm8r8(cc);
                     }
                     
                     cca = cc;
                     return (cca);
              }


         /////////////////////////////////////////////////////////////////////////////////////////////
       
                   __m512d fresnel_S_zmm8r8(const __m512d xxa) {
                                        
                        
                        const __m512d c0   = _mm512_set1_pd(2.5625);
                        const __m512d c1   = _mm512_set1_pd(36974.0);
                        const __m512d hlf  = _mm512_set1_pd(0.5);
                        const __m512d _0   = _mm512_setzero_pd();
                        const __m512d _1   = _mm512_set1_pd(1.0); 
                        const __m512d pi   = _mm512_set1_pd(3.14159265358979323846264338328);
                        const __m512d pio2 = _mm512_set1_pd(1.57079632679489661923132169164);
                        register __m512d f,g,ss,s,t,u,t0,t1;
                        register __m512d ssa,x,x2,acc1,acc2,acc3,acc4;
                       
                        x   = _mm512_abs_pd(xxa);
                        x2  = _mm512_mul_pd(x,x);
                        if(_mm512_cmp_pd_mask(x,c0,_CMP_LT_OQ)) {
			   volatile __m512d prefsn = _mm_prefetch((const char*)&sn[0],_MM_HINT_T0);
                           volatile __m512d prefsd = _mm_prefetch((const char*)&sd[0],_MM_HINT_T0);
                           
                           t = _mm512_mul_pd(x2,x2);
                           acc1 = sn[0]; 
                           acc2 = _mm512_add_pd(t,sd[0]);
                           acc1 = _mm512_fmadd_pd(acc1,t,sn[1]);
                           acc2 = _mm512_fmadd_pd(acc2,t,sd[1]);
                           acc1 = _mm512_fmadd_pd(acc1,t,sn[2]);
                           acc2 = _mm512_fmadd_pd(acc2,t,sd[2]);
                           acc1 = _mm512_fmadd_pd(acc1,t,sn[3]);
                           acc2 = _mm512_fmadd_pd(acc2,t,sd[3]);
                           acc1 = _mm512_fmadd_pd(acc1,t,sn[4]);
                           acc2 = _mm512_fmadd_pd(acc2,t,sd[4]);
                           acc2 = _mm512_fmadd_pd(acc2,t,sd[5]);
                           t0   = _mm512_div_pd(acc1,acc2);
                           ss   = _mm512_mul_pd(_mm512_mul_pd(x,x2),t0);
                           goto done;
                        }

                       if(_mm512_cmp_pd_mask(x,c1,_CMP_GT_OQ)) {
                          ss = hlf;
                          goto done;
                      }

                      /*		Asymptotic power series auxiliary functions
                       *		for large argument
                       */

                        volatile __m512d prefsn = _mm_prefetch((const char*)&fn[0],_MM_HINT_T0);
                        volatile __m512d prefsd = _mm_prefetch((const char*)&fd[0],_MM_HINT_T0);
                        volatile __m512d prefcn = _mm_prefetch((const char*)&gn[0],_MM_HINT_T0);
                        volatile __m512d prefcd = _mm_prefetch((const char*)&gd[0],_MM_HINT_T0);
                        t = _mm512_mul_pd(pi,x2);
                        u = _mm512_div_pd(_1,_mm512_mul_pd(t,t));
                        acc1 = fn[0];
                        acc2 = _mm512_add_pd(u,fd[0]);
                        acc3 = gn[0];
                        acc4 = _mm512_add_pd(u,gd[0]);
                        t = _mm512_div_pd(_1,t);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[1]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[1]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[2]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[2]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[3]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[3]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[4]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[4]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[5]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[5]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[6]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[6]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[7]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[7]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[8]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[8]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[9]);
                        t0   = _mm512_div_pd(acc1,acc2);
                        f    = _mm512_sub_pd(_1,_mm512_mul_pd(u,t0));
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[1]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[1]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[2]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[2]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[3]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[3]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[4]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[4]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[5]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[5]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[6]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[6]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[7]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[7]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[8]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[8]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gd[9]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[10]);
                        t1   = _mm512_div_pd(acc3,acc4);
                        g    = _mm512_mul_pd(t,t1);
                        
                        t    = _mm512_mul_pd(pio2,x2);
                        s    = xsinf(t);
                        t    = _mm512_mul_pd(pi,x);
                        t1   = _mm512_fmadd_pd(f,c,_mm512_mul_pd(g,s));
                        ss   = _mm512_sub_pd(hlf,_mm512_div_pd(t1,t));
done:
                     if(_mm512_cmp_pd_mask(xxa,
                                     _mm512_setzero_pd(),_CMP_LT_OQ)) {
                         ss = negate_zmm8r8(ss);
                     }
                     
                     ssa = ss;
                     return (ssa);
              }


               __m512d fresnel_S_zmm8r8_a(const double * __restrict __attribute__((aligned(64))) pxxa) {
                                        
                        register __m512d xxa = _mm512_load_pd(&pxxa[0]);
                        const __m512d c0   = _mm512_set1_pd(2.5625);
                        const __m512d c1   = _mm512_set1_pd(36974.0);
                        const __m512d hlf  = _mm512_set1_pd(0.5);
                        const __m512d _0   = _mm512_setzero_pd();
                        const __m512d _1   = _mm512_set1_pd(1.0); 
                        const __m512d pi   = _mm512_set1_pd(3.14159265358979323846264338328);
                        const __m512d pio2 = _mm512_set1_pd(1.57079632679489661923132169164);
                        register __m512d f,g,ss,s,t,u,t0,t1;
                        register __m512d ssa,x,x2,acc1,acc2,acc3,acc4;
                       
                        x   = _mm512_abs_pd(xxa);
                        x2  = _mm512_mul_pd(x,x);
                        if(_mm512_cmp_pd_mask(x,c0,_CMP_LT_OQ)) {
			   volatile __m512d prefsn = _mm_prefetch((const char*)&sn[0],_MM_HINT_T0);
                           volatile __m512d prefsd = _mm_prefetch((const char*)&sd[0],_MM_HINT_T0);
                           
                           t = _mm512_mul_pd(x2,x2);
                           acc1 = sn[0]; 
                           acc2 = _mm512_add_pd(t,sd[0]);
                           acc1 = _mm512_fmadd_pd(acc1,t,sn[1]);
                           acc2 = _mm512_fmadd_pd(acc2,t,sd[1]);
                           acc1 = _mm512_fmadd_pd(acc1,t,sn[2]);
                           acc2 = _mm512_fmadd_pd(acc2,t,sd[2]);
                           acc1 = _mm512_fmadd_pd(acc1,t,sn[3]);
                           acc2 = _mm512_fmadd_pd(acc2,t,sd[3]);
                           acc1 = _mm512_fmadd_pd(acc1,t,sn[4]);
                           acc2 = _mm512_fmadd_pd(acc2,t,sd[4]);
                           acc2 = _mm512_fmadd_pd(acc2,t,sd[5]);
                           t0   = _mm512_div_pd(acc1,acc2);
                           ss   = _mm512_mul_pd(_mm512_mul_pd(x,x2),t0);
                           goto done;
                        }

                       if(_mm512_cmp_pd_mask(x,c1,_CMP_GT_OQ)) {
                          ss = hlf;
                          goto done;
                      }

                      /*		Asymptotic power series auxiliary functions
                       *		for large argument
                       */

                        volatile __m512d prefsn = _mm_prefetch((const char*)&fn[0],_MM_HINT_T0);
                        volatile __m512d prefsd = _mm_prefetch((const char*)&fd[0],_MM_HINT_T0);
                        volatile __m512d prefcn = _mm_prefetch((const char*)&gn[0],_MM_HINT_T0);
                        volatile __m512d prefcd = _mm_prefetch((const char*)&gd[0],_MM_HINT_T0);
                        t = _mm512_mul_pd(pi,x2);
                        u = _mm512_div_pd(_1,_mm512_mul_pd(t,t));
                        acc1 = fn[0];
                        acc2 = _mm512_add_pd(u,fd[0]);
                        acc3 = gn[0];
                        acc4 = _mm512_add_pd(u,gd[0]);
                        t = _mm512_div_pd(_1,t);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[1]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[1]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[2]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[2]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[3]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[3]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[4]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[4]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[5]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[5]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[6]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[6]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[7]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[7]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[8]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[8]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[9]);
                        t0   = _mm512_div_pd(acc1,acc2);
                        f    = _mm512_sub_pd(_1,_mm512_mul_pd(u,t0));
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[1]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[1]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[2]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[2]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[3]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[3]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[4]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[4]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[5]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[5]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[6]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[6]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[7]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[7]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[8]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[8]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gd[9]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[10]);
                        t1   = _mm512_div_pd(acc3,acc4);
                        g    = _mm512_mul_pd(t,t1);
                        
                        t    = _mm512_mul_pd(pio2,x2);
                        s    = xsinf(t);
                        t    = _mm512_mul_pd(pi,x);
                        t1   = _mm512_fmadd_pd(f,c,_mm512_mul_pd(g,s));
                        ss   = _mm512_sub_pd(hlf,_mm512_div_pd(t1,t));
done:
                     if(_mm512_cmp_pd_mask(xxa,
                                     _mm512_setzero_pd(),_CMP_LT_OQ)) {
                         ss = negate_zmm8r8(ss);
                     }
                     
                     ssa = ss;
                     return (ssa);
              }


               __m512d fresnel_S_zmm8r8_u(const double * __restrict  pxxa) {
                                        
                       
                        register __m512d xxa = _mm512_loadu_pd(&pxxa[0]);
                        const __m512d c0   = _mm512_set1_pd(2.5625);
                        const __m512d c1   = _mm512_set1_pd(36974.0);
                        const __m512d hlf  = _mm512_set1_pd(0.5);
                        const __m512d _0   = _mm512_setzero_pd();
                        const __m512d _1   = _mm512_set1_pd(1.0); 
                        const __m512d pi   = _mm512_set1_pd(3.14159265358979323846264338328);
                        const __m512d pio2 = _mm512_set1_pd(1.57079632679489661923132169164);
                        register __m512d f,g,ss,s,t,u,t0,t1;
                        register __m512d ssa,x,x2,acc1,acc2,acc3,acc4;
                       
                        x   = _mm512_abs_pd(xxa);
                        x2  = _mm512_mul_pd(x,x);
                        if(_mm512_cmp_pd_mask(x,c0,_CMP_LT_OQ)) {
			   volatile __m512d prefsn = _mm_prefetch((const char*)&sn[0],_MM_HINT_T0);
                           volatile __m512d prefsd = _mm_prefetch((const char*)&sd[0],_MM_HINT_T0);
                           
                           t = _mm512_mul_pd(x2,x2);
                           acc1 = sn[0]; 
                           acc2 = _mm512_add_pd(t,sd[0]);
                           acc1 = _mm512_fmadd_pd(acc1,t,sn[1]);
                           acc2 = _mm512_fmadd_pd(acc2,t,sd[1]);
                           acc1 = _mm512_fmadd_pd(acc1,t,sn[2]);
                           acc2 = _mm512_fmadd_pd(acc2,t,sd[2]);
                           acc1 = _mm512_fmadd_pd(acc1,t,sn[3]);
                           acc2 = _mm512_fmadd_pd(acc2,t,sd[3]);
                           acc1 = _mm512_fmadd_pd(acc1,t,sn[4]);
                           acc2 = _mm512_fmadd_pd(acc2,t,sd[4]);
                           acc2 = _mm512_fmadd_pd(acc2,t,sd[5]);
                           t0   = _mm512_div_pd(acc1,acc2);
                           ss   = _mm512_mul_pd(_mm512_mul_pd(x,x2),t0);
                           goto done;
                        }

                       if(_mm512_cmp_pd_mask(x,c1,_CMP_GT_OQ)) {
                          ss = hlf;
                          goto done;
                      }

                      /*		Asymptotic power series auxiliary functions
                       *		for large argument
                       */

                        volatile __m512d prefsn = _mm_prefetch((const char*)&fn[0],_MM_HINT_T0);
                        volatile __m512d prefsd = _mm_prefetch((const char*)&fd[0],_MM_HINT_T0);
                        volatile __m512d prefcn = _mm_prefetch((const char*)&gn[0],_MM_HINT_T0);
                        volatile __m512d prefcd = _mm_prefetch((const char*)&gd[0],_MM_HINT_T0);
                        t = _mm512_mul_pd(pi,x2);
                        u = _mm512_div_pd(_1,_mm512_mul_pd(t,t));
                        acc1 = fn[0];
                        acc2 = _mm512_add_pd(u,fd[0]);
                        acc3 = gn[0];
                        acc4 = _mm512_add_pd(u,gd[0]);
                        t = _mm512_div_pd(_1,t);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[1]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[1]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[2]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[2]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[3]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[3]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[4]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[4]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[5]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[5]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[6]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[6]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[7]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[7]);
                        acc1 = _mm512_fmadd_pd(acc1,u,fn[8]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[8]);
                        acc2 = _mm512_fmadd_pd(acc2,u,fd[9]);
                        t0   = _mm512_div_pd(acc1,acc2);
                        f    = _mm512_sub_pd(_1,_mm512_mul_pd(u,t0));
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[1]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[1]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[2]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[2]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[3]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[3]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[4]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[4]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[5]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[5]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[6]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[6]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[7]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[7]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gn[8]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[8]);
                        acc3 = _mm512_fmadd_pd(acc3,u,gd[9]);
                        acc4 = _mm512_fmadd_pd(acc4,u,gd[10]);
                        t1   = _mm512_div_pd(acc3,acc4);
                        g    = _mm512_mul_pd(t,t1);
                        
                        t    = _mm512_mul_pd(pio2,x2);
                        s    = xsinf(t);
                        t    = _mm512_mul_pd(pi,x);
                        t1   = _mm512_fmadd_pd(f,c,_mm512_mul_pd(g,s));
                        ss   = _mm512_sub_pd(hlf,_mm512_div_pd(t1,t));
done:
                     if(_mm512_cmp_pd_mask(xxa,
                                     _mm512_setzero_pd(),_CMP_LT_OQ)) {
                         ss = negate_zmm8r8(ss);
                     }
                     
                     ssa = ss;
                     return (ssa);
              }
     
       
  



    
    

                


 



















