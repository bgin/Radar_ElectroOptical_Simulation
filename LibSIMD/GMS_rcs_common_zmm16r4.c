

#include "GMS_rcs_common_zmm16r4.h"
#include "GMS_complex_zmm16r4.h"
#include "GMS_simd_utils.h"
#include "GMS_sleefsimdsp.h"





                void k_zmm16r4(const __m512 mur,
                                  const __m512 mui
                                  const __m512 epsr,
                                  const __m512 epsi,
                                  const __m512 om,
                                  __m512 * __restrict kr,
                                  __m512 * __restrict ki) {

                        __m512 sqrr,sqri;
                        __m512 t0r,t0i;
                        __m512 wrkc = _mm512_setzero_ps();
                        cmul_zmm16r4(mur,mui,epsr,epsi,&t0r,&t0i);
                        csqrt_zmm16r4(t0r,t0i,&wrkc,&sqrr,&sqri);
                        *kr = _mm512_mul_ps(om,sqrr);
                        *ki = _mm512_mul_ps(om,sqri);
               }  


              ////////////////////////////////////////////////////////////////////////////////


                __m512 rc_zmm16r4(   const __m512 x,
                                     const __m512 y,
                                     const __m512 errtot) {

                          const register __m512 c1 = _mm512_set1_ps(0.142857142857142857142857142857f);
                          const register __m512 c2 = _mm512_set1_ps(0.409090909090909090909090909091f);
                          const register __m512 _1 = _mm512_set1_ps(1.0f);
                          const register __m512 thr= _mm512_set1_ps(0.333333333333333333333333333333f);
                          const register __m512 c3 = _mm512_set1_ps(0.375f);
                          const register __m512 _2 = _mm512_set1_ps(2.0f);
                          const register __m512 qtr= _mm512_set1_ps(0.25f);
                          const register __m512 c4 = _mm512_set1_ps(0.3f);
                          register __m512 rc,xn,yn,mu,s,sn,lamda;
                          xn = x;
                          yn = y;
                          
                          while(true) {

                                mu = _mm512_mul_ps(_mm512_add_ps(xn,
                                                          _mm512_add_ps(yn,yn)),thr);
                                sn = _mm512_div_ps(_mm512_add_ps(yn,mu),
                                                   _mm512_sub_ps(mu,_2));
                                if(_mm512_cmp_mask_ps(
                                             _mm512_abs_ps(sn),errtot,_CMP_LT_OQ)) {
                                   register __m512 t0 = _mm512_fmadd_ps(sn,c2,c3);
                                   register __m512 t1 = _mm512_fmadd_ps(t0,sn,c1);
                                   register __m512 t2 = _mm512_fmadd_ps(t1,sn,c4);
                                   s                  = _mm512_mul_ps(_mm512_mul_ps(sn,sn),t2);
                                   rc                 = _mm512_div_ps(_mm512_add_ps(_1,s),
                                                                      _mm512_sqrt_ps(mu));
                                   return (rc);
                                }
                                register __m512 sxn = _mm512_sqrt_ps(xn);
                                register __m512 syn = _mm512_sqrt_ps(yn);
                                lamda =  _mm512_fmadd_ps(_mm512_mul_ps(_2,sxn),syn,yn);
                                xn    = _mm512_mul_ps(_mm512_add_ps(xn,lamda),qtr);
                                yn    = _mm512_mul_ps(_mm512_add_ps(ym,lamda),qtr);
                         }
                } 


                __m512 rc_zmm16r4_a(   const float * __restrict __attributes__((aligned(64))) px,
                                       const float * __restrict __attributes__((aligned(64))) py,
                                       const float * __restrict __attributes__((aligned(64))) perrtot) {

                          register __m512 x       = _mm512_load_ps(&px[0]);
                          register __m512 y       = _mm512_load_ps(&py[0]);
                          register __m512 errtot  = _mm512_load_ps(&perrtot[0]);
                          const register __m512 c1 = _mm512_set1_ps(0.142857142857142857142857142857f);
                          const register __m512 c2 = _mm512_set1_ps(0.409090909090909090909090909091f);
                          const register __m512 _1 = _mm512_set1_ps(1.0f);
                          const register __m512 thr= _mm512_set1_ps(0.333333333333333333333333333333f);
                          const register __m512 c3 = _mm512_set1_ps(0.375f);
                          const register __m512 _2 = _mm512_set1_ps(2.0f);
                          const register __m512 qtr= _mm512_set1_ps(0.25f);
                          const register __m512 c4 = _mm512_set1_ps(0.3f);
                          register __m512 rc,xn,yn,mu,s,sn,lamda;
                          xn = x;
                          yn = y;
                          
                          while(true) {

                                mu = _mm512_mul_ps(_mm512_add_ps(xn,
                                                          _mm512_add_ps(yn,yn)),thr);
                                sn = _mm512_div_ps(_mm512_add_ps(yn,mu),
                                                   _mm512_sub_ps(mu,_2));
                                if(_mm512_cmp_mask_ps(
                                             _mm512_abs_ps(sn),errtot,_CMP_LT_OQ)) {
                                   register __m512 t0 = _mm512_fmadd_ps(sn,c2,c3);
                                   register __m512 t1 = _mm512_fmadd_ps(t0,sn,c1);
                                   register __m512 t2 = _mm512_fmadd_ps(t1,sn,c4);
                                   s                  = _mm512_mul_ps(_mm512_mul_ps(sn,sn),t2);
                                   rc                 = _mm512_div_ps(_mm512_add_ps(_1,s),
                                                                      _mm512_sqrt_ps(mu));
                                   return (rc);
                                }
                                register __m512 sxn = _mm512_sqrt_ps(xn);
                                register __m512 syn = _mm512_sqrt_ps(yn);
                                lamda =  _mm512_fmadd_ps(_mm512_mul_ps(_2,sxn),syn,yn);
                                xn    = _mm512_mul_ps(_mm512_add_ps(xn,lamda),qtr);
                                yn    = _mm512_mul_ps(_mm512_add_ps(ym,lamda),qtr);
                         }
                }


                  __m512 rc_zmm16r4_u( const float * __restrict  px,
                                       const float * __restrict  py,
                                       const float * __restrict  perrtot) {

                          register __m512 x       = _mm512_loadu_ps(&px[0]);
                          register __m512 y       = _mm512_loadu_ps(&py[0]);
                          register __m512 errtot  = _mm512_loadu_ps(&perrtot[0]);
                          const register __m512 c1 = _mm512_set1_ps(0.142857142857142857142857142857f);
                          const register __m512 c2 = _mm512_set1_ps(0.409090909090909090909090909091f);
                          const register __m512 _1 = _mm512_set1_ps(1.0f);
                          const register __m512 thr= _mm512_set1_ps(0.333333333333333333333333333333f);
                          const register __m512 c3 = _mm512_set1_ps(0.375f);
                          const register __m512 _2 = _mm512_set1_ps(2.0f);
                          const register __m512 qtr= _mm512_set1_ps(0.25f);
                          const register __m512 c4 = _mm512_set1_ps(0.3f);
                          register __m512 rc,xn,yn,mu,s,sn,lamda;
                          xn = x;
                          yn = y;
                          
                          while(true) {

                                mu = _mm512_mul_ps(_mm512_add_ps(xn,
                                                          _mm512_add_ps(yn,yn)),thr);
                                sn = _mm512_div_ps(_mm512_add_ps(yn,mu),
                                                   _mm512_sub_ps(mu,_2));
                                if(_mm512_cmp_mask_ps(
                                             _mm512_abs_ps(sn),errtot,_CMP_LT_OQ)) {
                                   register __m512 t0 = _mm512_fmadd_ps(sn,c2,c3);
                                   register __m512 t1 = _mm512_fmadd_ps(t0,sn,c1);
                                   register __m512 t2 = _mm512_fmadd_ps(t1,sn,c4);
                                   s                  = _mm512_mul_ps(_mm512_mul_ps(sn,sn),t2);
                                   rc                 = _mm512_div_ps(_mm512_add_ps(_1,s),
                                                                      _mm512_sqrt_ps(mu));
                                   return (rc);
                                }
                                register __m512 sxn = _mm512_sqrt_ps(xn);
                                register __m512 syn = _mm512_sqrt_ps(yn);
                                lamda =  _mm512_fmadd_ps(_mm512_mul_ps(_2,sxn),syn,yn);
                                xn    = _mm512_mul_ps(_mm512_add_ps(xn,lamda),qtr);
                                yn    = _mm512_mul_ps(_mm512_add_ps(ym,lamda),qtr);
                         }
                }


                ////////////////////////////////////////////////////////////////////////


                 __m512 rd_zmm16r4(  const __m512 x,
                                     const __m512 y,
                                     const __m512 z,
                                     const __m512 errtot) {

                          const register __m512 _3 = _mm512_set1_ps(3.0f);
                          const register __m512 _1 = _mm512_set1_ps(1.0f);
                          const register __m512 c1 = _mm512_set1_ps(-0.214285714285714285714285714286f);
                          const register __m512 c2 = _mm512_set1_ps(0.166666666666666666666666666667f);
                          const register __m512 c3 = _mm512_set1_ps(-0.409090909090909090909090909091f);
                          const register __m512 c4 = _mm512_set1_ps(0.115384615384615384615384615385f);
                          const register __m512 c5 = _mm512_set1_ps(6.0f);
                          const register __m512 c6 = _mm512_set1_ps(1.5f);
                          const register __m512 c7 = _mm512_set1_ps(0.2f);
                          const register __m512 c8 = _mm512_set1_ps(0.25f);
                          register __m512 rd,xn,yn,zn,epslon,sigma,pow4,mu;
                          register __m512 xndev,yndev,zndev,ea,eb,ec,ed,ef;
                          register __m512 s1,s2,xnroot,ynroot,znroot,lamda;
                          register __m512 x0,x1,x2,x3,x4,x5;

                          xn    = x;
                          yn    = y;
                          zn    = z;
                          sigma = _mm512_setzero_ps();
                          pow4  = _1; 
                          while(true) {
                                mu    = _mm512_mul_ps(c7,_mm512_fmadd_ps(zn,_3,
                                                          _mm512_add_ps(xn,yn));
                                xndev = _mm512_div_ps(_mm512_sub_ps(mu,xn),mu);
                                yndev = _mm512_div_ps(_mm512_sub_ps(mu,yn),mu);
                                zndev = _mm512_div_ps(_mm512_sub_ps(mu,zn),mu);
                                epslon= _mm512_abs_ps(xndev);
                                epslon= _mm512_max_ps(epslon,_mm512_abs_ps(yndev));
                                epslon= _mm512_max_ps(epslon,_mm512_abs_ps(zndev));

                                if(_mm512_cmp_mask_ps(epslon,errtot,_CMP_LT_OQ)) {
                                   ea = _mm512_mul_ps(xndev,yndev);
                                   eb = _mm512_mul_ps(zndev,zndev);
                                   ec = _mm512_sub_ps(ea,eb);
                                   ed = _mm512_sub_ps(ea,_mm512_mul_ps(c5,eb));
                                   ef = _mm512_add_ps(ed,_mm512_add_ps(ec,ec));
                                   x0 = _mm512_fmadd_ps(c3,c8,c1);
                                   x1 = _mm512_sub_ps(ed,_mm512_sub_ps(c6,c4));
                                   x2 = _mm512_mul_ps(zndev,ef);
                                   s1 = _mm512_mul_ps(ed,_mm512_mul_ps(x0,
                                                               _mm512_mul_ps(x1,x2)));
                                   x3 = _mm512_fmadd_ps(c3,ec,_mm512_mul_ps(zndev,
                                                               _mm512_mul_ps(c4,ea)));
                                   x4 = _mm512_fmadd_ps(x3,zndev,_mm512_mul_ps(ef,c2));
                                   s2 = _mm512_mul_ps(zndev,x4);
                                   x0 = _mm512_fmadd_ps(_3,sigma,pow4);
                                   x1 = _mm512_add_ps(_1,_mm512_add_ps(s1,s2));
                                   x2 = _mm512_mul_ps(mu,_mm512_sqrt_ps(mu));
                                   rd = _mm512_div_ps(_mm512_mul_ps(x0,x1),x2);
                                   return (rd);
                                } 

                                xnroot = _mm512_sqrt_ps(xn);
                                ynroot = _mm512_sqrt_ps(yn);
                                znroot = _mm512_sqrt_ps(zn);
                                x0     = _mm512_fmadd_ps(ynroot,znroot,_mm512_add_ps(ynroot,znroot));
                                lamda  = _mm512_mul_ps(xnroot,x0);
                                sigma  = _mm512_div_ps(_mm512_add_ps(sigma,pow4),
                                                       _mm512_mul_ps(znroot,_mm512_add_ps(zn,lamda)));
                                pow4   = _mm512_mul_ps(pow4,c8);
                                xn     = _mm512_mul_ps(_mm512_add_ps(xn,lamda),c8);
                                yn     = _mm512_mul_ps(_mm512_add_ps(yn,lamda),c8);
                                zn     = _mm512_mul_ps(_mm512_add_ps(zn,lamda),c8);
                         }
                 }


                    __m512 rd_zmm16r4_a(const float * __restrict __attributes__((aligned(64))) px,
                                        const float * __restrict __attributes__((aligned(64))) py,
                                        const float * __restrict __attributes__((aligned(64))) pz,
                                        const float * __restrict __attributes__((aligned(64))) perrtot) {

                          register __m512 x       = _mm512_load_ps(&px[0]);
                          register __m512 y       = _mm512_load_ps(&py[0]);
                          register __m512 z       = _mm512_load_ps(&pz[0]);
                          register __m512 errtot  = _mm512_load_ps(&perrtot[0]);
                          const register __m512 _3 = _mm512_set1_ps(3.0f);
                          const register __m512 _1 = _mm512_set1_ps(1.0f);
                          const register __m512 c1 = _mm512_set1_ps(-0.214285714285714285714285714286f);
                          const register __m512 c2 = _mm512_set1_ps(0.166666666666666666666666666667f);
                          const register __m512 c3 = _mm512_set1_ps(-0.409090909090909090909090909091f);
                          const register __m512 c4 = _mm512_set1_ps(0.115384615384615384615384615385f);
                          const register __m512 c5 = _mm512_set1_ps(6.0f);
                          const register __m512 c6 = _mm512_set1_ps(1.5f);
                          const register __m512 c7 = _mm512_set1_ps(0.2f);
                          const register __m512 c8 = _mm512_set1_ps(0.25f);
                          register __m512 rd,xn,yn,zn,epslon,sigma,pow4,mu;
                          register __m512 xndev,yndev,zndev,ea,eb,ec,ed,ef;
                          register __m512 s1,s2,xnroot,ynroot,znroot,lamda;
                          register __m512 x0,x1,x2,x3,x4,x5;

                          xn    = x;
                          yn    = y;
                          zn    = z;
                          sigma = _mm512_setzero_ps();
                          pow4  = _1; 
                          while(true) {
                                mu    = _mm512_mul_ps(c7,_mm512_fmadd_ps(zn,_3,
                                                          _mm512_add_ps(xn,yn));
                                xndev = _mm512_div_ps(_mm512_sub_ps(mu,xn),mu);
                                yndev = _mm512_div_ps(_mm512_sub_ps(mu,yn),mu);
                                zndev = _mm512_div_ps(_mm512_sub_ps(mu,zn),mu);
                                epslon= _mm512_abs_ps(xndev);
                                epslon= _mm512_max_ps(epslon,_mm512_abs_ps(yndev));
                                epslon= _mm512_max_ps(epslon,_mm512_abs_ps(zndev));

                                if(_mm512_cmp_mask_ps(epslon,errtot,_CMP_LT_OQ)) {
                                   ea = _mm512_mul_ps(xndev,yndev);
                                   eb = _mm512_mul_ps(zndev,zndev);
                                   ec = _mm512_sub_ps(ea,eb);
                                   ed = _mm512_sub_ps(ea,_mm512_mul_ps(c5,eb));
                                   ef = _mm512_add_ps(ed,_mm512_add_ps(ec,ec));
                                   x0 = _mm512_fmadd_ps(c3,c8,c1);
                                   x1 = _mm512_sub_ps(ed,_mm512_sub_ps(c6,c4));
                                   x2 = _mm512_mul_ps(zndev,ef);
                                   s1 = _mm512_mul_ps(ed,_mm512_mul_ps(x0,
                                                               _mm512_mul_ps(x1,x2)));
                                   x3 = _mm512_fmadd_ps(c3,ec,_mm512_mul_ps(zndev,
                                                               _mm512_mul_ps(c4,ea)));
                                   x4 = _mm512_fmadd_ps(x3,zndev,_mm512_mul_ps(ef,c2));
                                   s2 = _mm512_mul_ps(zndev,x4);
                                   x0 = _mm512_fmadd_ps(_3,sigma,pow4);
                                   x1 = _mm512_add_ps(_1,_mm512_add_ps(s1,s2));
                                   x2 = _mm512_mul_ps(mu,_mm512_sqrt_ps(mu));
                                   rd = _mm512_div_ps(_mm512_mul_ps(x0,x1),x2);
                                   return (rd);
                                } 

                                xnroot = _mm512_sqrt_ps(xn);
                                ynroot = _mm512_sqrt_ps(yn);
                                znroot = _mm512_sqrt_ps(zn);
                                x0     = _mm512_fmadd_ps(ynroot,znroot,_mm512_add_ps(ynroot,znroot));
                                lamda  = _mm512_mul_ps(xnroot,x0);
                                sigma  = _mm512_div_ps(_mm512_add_ps(sigma,pow4),
                                                       _mm512_mul_ps(znroot,_mm512_add_ps(zn,lamda)));
                                pow4   = _mm512_mul_ps(pow4,c8);
                                xn     = _mm512_mul_ps(_mm512_add_ps(xn,lamda),c8);
                                yn     = _mm512_mul_ps(_mm512_add_ps(yn,lamda),c8);
                                zn     = _mm512_mul_ps(_mm512_add_ps(zn,lamda),c8);
                         }
                 }


                    __m512 rd_zmm16r4_u(const float * __restrict  px,
                                        const float * __restrict  py,
                                        const float * __restrict  pz,
                                        const float * __restrict  perrtot) {

                          register __m512 x       = _mm512_loadu_ps(&px[0]);
                          register __m512 y       = _mm512_loadu_ps(&py[0]);
                          register __m512 z       = _mm512_loadu_ps(&pz[0]);
                          register __m512 errtot  = _mm512_loadu_ps(&perrtot[0]);
                          const register __m512 _3 = _mm512_set1_ps(3.0f);
                          const register __m512 _1 = _mm512_set1_ps(1.0f);
                          const register __m512 c1 = _mm512_set1_ps(-0.214285714285714285714285714286f);
                          const register __m512 c2 = _mm512_set1_ps(0.166666666666666666666666666667f);
                          const register __m512 c3 = _mm512_set1_ps(-0.409090909090909090909090909091f);
                          const register __m512 c4 = _mm512_set1_ps(0.115384615384615384615384615385f);
                          const register __m512 c5 = _mm512_set1_ps(6.0f);
                          const register __m512 c6 = _mm512_set1_ps(1.5f);
                          const register __m512 c7 = _mm512_set1_ps(0.2f);
                          const register __m512 c8 = _mm512_set1_ps(0.25f);
                          register __m512 rd,xn,yn,zn,epslon,sigma,pow4,mu;
                          register __m512 xndev,yndev,zndev,ea,eb,ec,ed,ef;
                          register __m512 s1,s2,xnroot,ynroot,znroot,lamda;
                          register __m512 x0,x1,x2,x3,x4,x5;

                          xn    = x;
                          yn    = y;
                          zn    = z;
                          sigma = _mm512_setzero_ps();
                          pow4  = _1; 
                          while(true) {
                                mu    = _mm512_mul_ps(c7,_mm512_fmadd_ps(zn,_3,
                                                          _mm512_add_ps(xn,yn));
                                xndev = _mm512_div_ps(_mm512_sub_ps(mu,xn),mu);
                                yndev = _mm512_div_ps(_mm512_sub_ps(mu,yn),mu);
                                zndev = _mm512_div_ps(_mm512_sub_ps(mu,zn),mu);
                                epslon= _mm512_abs_ps(xndev);
                                epslon= _mm512_max_ps(epslon,_mm512_abs_ps(yndev));
                                epslon= _mm512_max_ps(epslon,_mm512_abs_ps(zndev));

                                if(_mm512_cmp_mask_ps(epslon,errtot,_CMP_LT_OQ)) {
                                   ea = _mm512_mul_ps(xndev,yndev);
                                   eb = _mm512_mul_ps(zndev,zndev);
                                   ec = _mm512_sub_ps(ea,eb);
                                   ed = _mm512_sub_ps(ea,_mm512_mul_ps(c5,eb));
                                   ef = _mm512_add_ps(ed,_mm512_add_ps(ec,ec));
                                   x0 = _mm512_fmadd_ps(c3,c8,c1);
                                   x1 = _mm512_sub_ps(ed,_mm512_sub_ps(c6,c4));
                                   x2 = _mm512_mul_ps(zndev,ef);
                                   s1 = _mm512_mul_ps(ed,_mm512_mul_ps(x0,
                                                               _mm512_mul_ps(x1,x2)));
                                   x3 = _mm512_fmadd_ps(c3,ec,_mm512_mul_ps(zndev,
                                                               _mm512_mul_ps(c4,ea)));
                                   x4 = _mm512_fmadd_ps(x3,zndev,_mm512_mul_ps(ef,c2));
                                   s2 = _mm512_mul_ps(zndev,x4);
                                   x0 = _mm512_fmadd_ps(_3,sigma,pow4);
                                   x1 = _mm512_add_ps(_1,_mm512_add_ps(s1,s2));
                                   x2 = _mm512_mul_ps(mu,_mm512_sqrt_ps(mu));
                                   rd = _mm512_div_ps(_mm512_mul_ps(x0,x1),x2);
                                   return (rd);
                                } 

                                xnroot = _mm512_sqrt_ps(xn);
                                ynroot = _mm512_sqrt_ps(yn);
                                znroot = _mm512_sqrt_ps(zn);
                                x0     = _mm512_fmadd_ps(ynroot,znroot,_mm512_add_ps(ynroot,znroot));
                                lamda  = _mm512_mul_ps(xnroot,x0);
                                sigma  = _mm512_div_ps(_mm512_add_ps(sigma,pow4),
                                                       _mm512_mul_ps(znroot,_mm512_add_ps(zn,lamda)));
                                pow4   = _mm512_mul_ps(pow4,c8);
                                xn     = _mm512_mul_ps(_mm512_add_ps(xn,lamda),c8);
                                yn     = _mm512_mul_ps(_mm512_add_ps(yn,lamda),c8);
                                zn     = _mm512_mul_ps(_mm512_add_ps(zn,lamda),c8);
                         }
                 }


                 ///////////////////////////////////////////////////////////////////////////////


 static const __m512 sn[6] = { 
 _mm512_set1_ps(-2.99181919401019853726E3f),
 _mm512_set1_ps(7.08840045257738576863E5f),
 _mm512_set1_ps(-6.29741486205862506537E7f),
 _mm512_set1_ps(2.54890880573376359104E9f),
 _mm512_set1_ps(-4.42979518059697779103E10f),
 _mm512_set1_ps(3.18016297876567817986E11f)};

static const __m512 sd[6] = {
 _mm512_set1_ps(2.81376268889994315696E2f),
 _mm512_set1_ps(4.55847810806532581675E4f),
 _mm512_set1_ps(5.17343888770096400730E6f),
 _mm512_set1_ps(4.19320245898111231129E8f),
 _mm512_set1_ps(2.24411795645340920940E10f),
 _mm512_set1_ps(6.07366389490084639049E11f)};

static const __m512 cn[6] = {
 _mm512_set1_ps(-4.98843114573573548651E-8f),
 _mm512_set1_ps(9.50428062829859605134E-6f),
 _mm512_set1_ps(-6.45191435683965050962E-4f),
 _mm512_set1_ps(1.88843319396703850064E-2f),
 _mm512_set1_ps(-2.05525900955013891793E-1f),
 _mm512_set1_ps(9.99999999999999998822E-1f)};

static const __m512 cd[7] = {
 _mm512_set1_ps(3.99982968972495980367E-12f),
 _mm512_set1_ps(9.15439215774657478799E-10f),
 _mm512_set1_ps(1.25001862479598821474E-7f),
 _mm512_set1_ps(1.22262789024179030997E-5f),
 _mm512_set1_ps(8.68029542941784300606E-4f),
 _mm512_set1_ps(4.12142090722199792936E-2f),
 _mm512_set1_ps(1.00000000000000000118E0f)};

static const __m512 fn[10] = {
  _mm512_set1_ps(4.21543555043677546506E-1f),
  _mm512_set1_ps(1.43407919780758885261E-1f),
  _mm512_set1_ps(1.15220955073585758835E-2f),
  _mm512_set1_ps(3.45017939782574027900E-4f),
  _mm512_set1_ps(4.63613749287867322088E-6f),
  _mm512_set1_ps(3.05568983790257605827E-8f),
  _mm512_set1_ps(1.02304514164907233465E-10f),
  _mm512_set1_ps(1.72010743268161828879E-13f),
  _mm512_set1_ps(1.34283276233062758925E-16f),
  _mm512_set1_ps(3.76329711269987889006E-20f)};

static const __m512 fd[10] = {
  _mm512_set1_ps(7.51586398353378947175E-1f),
  _mm512_set1_ps(1.16888925859191382142E-1f),
  _mm512_set1_ps(6.44051526508858611005E-3f),
  _mm512_set1_ps(1.55934409164153020873E-4f),
  _mm512_set1_ps(1.84627567348930545870E-6f),
  _mm512_set1_ps(1.12699224763999035261E-8f),
  _mm512_set1_ps(3.60140029589371370404E-11),
  _mm512_set1_ps(5.88754533621578410010E-14f),
  _mm512_set1_ps(4.52001434074129701496E-17f),
  _mm512_set1_ps(1.25443237090011264384E-20f)};

static const __m512 gn[11] = {
  _mm512_set1_ps(5.04442073643383265887E-1f),
  _mm512_set1_ps(1.97102833525523411709E-1f),
  _mm512_set1_ps(1.87648584092575249293E-2f),
  _mm512_set1_ps(6.84079380915393090172E-4f),
  _mm512_set1_ps(1.15138826111884280931E-5f),
  _mm512_set1_ps(9.82852443688422223854E-8f),
  _mm512_set1_ps(4.45344415861750144738E-10f),
  _mm512_set1_ps(1.08268041139020870318E-12f),
  _mm512_set1_ps(1.37555460633261799868E-15f),
  _mm512_set1_ps(8.36354435630677421531E-19f),
  _mm512_set1_ps(1.86958710162783235106E-22f)};

static const __m512 gd[11] = {
  _mm512_set1_ps(1.47495759925128324529E0f),
  _mm512_set1_ps(3.37748989120019970451E-1f),
  _mm512_set1_ps(2.53603741420338795122E-2f),
  _mm512_set1_ps(8.14679107184306179049E-4f),
  _mm512_set1_ps(1.27545075667729118702E-5f),
  _mm512_set1_ps(1.04314589657571990585E-7f),
  _mm512_set1_ps(4.60680728146520428211E-10f),
  _mm512_set1_ps(1.10273215066240270757E-12f),
  _mm512_set1_ps(1.38796531259578871258E-15f),
  _mm512_set1_ps(8.39158816283118707363E-19f),
  _mm512_set1_ps(1.86958710162783236342E-22f)};




                     void fresnel_zmm16r4(const __m512 xxa,
                                          __m512 * __restrict ssa,
                                          __m512 * __restrict cca) {

                        
                        const __m512 c0   = _mm512_set1_ps(2.5625f);
                        const __m512 c1   = _mm512_set1_ps(36974.0f);
                        const __m512 hlf  = _mm512_set1_ps(0.5f);
                        const __m512 _0   = _mm512_setzero_ps();
                        const __m512 _1   = _mm512_set1_ps(1.0f); 
                        const __m512 pi   = _mm512_set1_ps(3.14159265358979323846264338328f);
                        const __m512 pio2 = _mm512_set1_ps(1.57079632679489661923132169164f);
                        register __m512 f,g,cc,ss,c,s,t,u,t0,t1;
                        register __m512 x,x2,acc1,acc2,acc3,acc4;
                       
                        x   = _mm512_abs_ps(xxa);
                        x2  = _mm512_mul_ps(x,x);
                        if(_mm512_cmp_ps_mask(x,c0,_CMP_LT_OQ)) {
			   volatile __m512 prefsn = _mm_prefetch((const char*)&sn[0],_MM_HINT_T0);
                           volatile __m512 prefsd = _mm_prefetch((const char*)&sd[0],_MM_HINT_T0);
                           volatile __m512 prefcn = _mm_prefetch((const char*)&cn[0],_MM_HINT_T0);
                           volatile __m512 prefcd = _mm_prefetch((const char*)&cd[0],_MM_HINT_T0);
                           t = _mm512_mul_ps(x2,x2);
                           acc1 = sn[0]; 
                           acc2 = _mm512_add_ps(t,sd[0]);
                           acc3 = cn[0];
                           acc4 = cd[0];
                           acc1 = _mm512_fmadd_ps(acc1,t,sn[1]);
                           acc2 = _mm512_fmadd_ps(acc2,t,sd[1]);
                           acc1 = _mm512_fmadd_ps(acc1,t,sn[2]);
                           acc2 = _mm512_fmadd_ps(acc2,t,sd[2]);
                           acc1 = _mm512_fmadd_ps(acc1,t,sn[3]);
                           acc2 = _mm512_fmadd_ps(acc2,t,sd[3]);
                           acc1 = _mm512_fmadd_ps(acc1,t,sn[4]);
                           acc2 = _mm512_fmadd_ps(acc2,t,sd[4]);
                           acc2 = _mm512_fmadd_ps(acc2,t,sd[5]);
                           t0   = _mm512_div_ps(acc1,acc2);
                           ss   = _mm512_mul_ps(_mm512_mul_ps(x,x2),t0);
                           acc3 = _mm512_fmadd_ps(acc3,t,cn[1]);
                           acc4 = _mm512_fmadd_ps(acc4,t,cd[1]);
                           acc3 = _mm512_fmadd_ps(acc3,t,cn[2]);
                           acc4 = _mm512_fmadd_ps(acc4,t,cd[2]);
                           acc3 = _mm512_fmadd_ps(acc3,t,cn[3]);
                           acc4 = _mm512_fmadd_ps(acc4,t,cd[3]);
                           acc3 = _mm512_fmadd_ps(acc3,t,cn[4]);
                           acc4 = _mm512_fmadd_ps(acc4,t,cd[5]);
                           acc4 = _mm512_fmadd_ps(acc4,t,cd[6]);
                           t1   = _mm512_div_ps(acc3,acc4);
                           cc   = _mm512_mul_ps(x,t1);
                           goto done;
                        }

                       if(_mm512_cmp_ps_mask(x,c1,_CMP_GT_OQ)) {
                          cc = hlf;
                          ss = hlf;
                          goto done;
                      }

                      /*		Asymptotic power series auxiliary functions
                       *		for large argument
                       */

                        volatile __m512 prefsn = _mm_prefetch((const char*)&fn[0],_MM_HINT_T0);
                        volatile __m512 prefsd = _mm_prefetch((const char*)&fd[0],_MM_HINT_T0);
                        volatile __m512 prefcn = _mm_prefetch((const char*)&gn[0],_MM_HINT_T0);
                        volatile __m512 prefcd = _mm_prefetch((const char*)&gd[0],_MM_HINT_T0);
                        t = _mm512_mul_ps(pi,x2);
                        u = _mm512_div_ps(_1,_mm512_mul_ps(t,t));
                        acc1 = fn[0];
                        acc2 = _mm512_add_ps(u,fd[0]);
                        acc3 = gn[0];
                        acc4 = _mm512_add_ps(u,gd[0]);
                        t = _mm512_div_ps(_1,t);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[1]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[1]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[2]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[2]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[3]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[3]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[4]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[4]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[5]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[5]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[6]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[6]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[7]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[7]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[8]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[8]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[9]);
                        t0   = _mm512_div_ps(acc1,acc2);
                        f    = _mm512_sub_ps(_1,_mm512_mul_ps(u,t0));
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[1]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[1]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[2]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[2]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[3]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[3]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[4]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[4]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[5]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[5]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[6]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[6]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[7]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[7]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[8]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[8]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gd[9]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[10]);
                        t1   = _mm512_div_ps(acc3,acc4);
                        g    = _mm512_mul_ps(t,t1);
                        
                        t    = _mm512_mul_ps(pio2,x2);
                        c    = xcosf(t);
                        s    = xsinf(t);
                        t    = _mm512_mul_ps(pi,x);
                        t0   = _mm512_fmsub_ps(f,s,_mm512_mul_ps(g,c));
                        cc   = _mm512_add_ps(hlf,_mm512_div_ps(t0,t));
                        t1   = _mm512_fmadd_ps(f,c,_mm512_mul_ps(g,s));
                        ss   = _mm512_sub_ps(hlf,_mm512_div_ps(t1,t));
done:
                     if(_mm512_cmp_ps_mask(xxa,
                                     _mm512_setzero_ps(),_CMP_LT_OQ)) {
                         cc = negate_zmm16r4(cc);
                         ss = negate_zmm16r4(ss);
                     }
                     
                     *cca = cc;
                     *ssa = ss;
              }


                 void fresnel_zmm16r4_a(  const float * __restrict __attributes__((aligned(64))) pxxa,
                                          float * __restrict __attributes__((aligned(64))) ssa,
                                          float * __restrict __attributes__((aligned(64))) cca) {

                        
                        register __m512 xxa = _mm512_load_ps(&pxxa[0]);
                        const __m512 c0   = _mm512_set1_ps(2.5625f);
                        const __m512 c1   = _mm512_set1_ps(36974.0f);
                        const __m512 hlf  = _mm512_set1_ps(0.5f);
                        const __m512 _0   = _mm512_setzero_ps();
                        const __m512 _1   = _mm512_set1_ps(1.0f); 
                        const __m512 pi   = _mm512_set1_ps(3.14159265358979323846264338328f);
                        const __m512 pio2 = _mm512_set1_ps(1.57079632679489661923132169164f);
                        register __m512 f,g,cc,ss,c,s,t,u,t0,t1;
                        register __m512 x,x2,acc1,acc2,acc3,acc4;
                       
                        x   = _mm512_abs_ps(xxa);
                        x2  = _mm512_mul_ps(x,x);
                        if(_mm512_cmp_ps_mask(x,c0,_CMP_LT_OQ)) {
			   volatile __m512 prefsn = _mm_prefetch((const char*)&sn[0],_MM_HINT_T0);
                           volatile __m512 prefsd = _mm_prefetch((const char*)&sd[0],_MM_HINT_T0);
                           volatile __m512 prefcn = _mm_prefetch((const char*)&cn[0],_MM_HINT_T0);
                           volatile __m512 prefcd = _mm_prefetch((const char*)&cd[0],_MM_HINT_T0);
                           t = _mm512_mul_ps(x2,x2);
                           acc1 = sn[0]; 
                           acc2 = _mm512_add_ps(t,sd[0]);
                           acc3 = cn[0];
                           acc4 = cd[0];
                           acc1 = _mm512_fmadd_ps(acc1,t,sn[1]);
                           acc2 = _mm512_fmadd_ps(acc2,t,sd[1]);
                           acc1 = _mm512_fmadd_ps(acc1,t,sn[2]);
                           acc2 = _mm512_fmadd_ps(acc2,t,sd[2]);
                           acc1 = _mm512_fmadd_ps(acc1,t,sn[3]);
                           acc2 = _mm512_fmadd_ps(acc2,t,sd[3]);
                           acc1 = _mm512_fmadd_ps(acc1,t,sn[4]);
                           acc2 = _mm512_fmadd_ps(acc2,t,sd[4]);
                           acc2 = _mm512_fmadd_ps(acc2,t,sd[5]);
                           t0   = _mm512_div_ps(acc1,acc2);
                           ss   = _mm512_mul_ps(_mm512_mul_ps(x,x2),t0);
                           acc3 = _mm512_fmadd_ps(acc3,t,cn[1]);
                           acc4 = _mm512_fmadd_ps(acc4,t,cd[1]);
                           acc3 = _mm512_fmadd_ps(acc3,t,cn[2]);
                           acc4 = _mm512_fmadd_ps(acc4,t,cd[2]);
                           acc3 = _mm512_fmadd_ps(acc3,t,cn[3]);
                           acc4 = _mm512_fmadd_ps(acc4,t,cd[3]);
                           acc3 = _mm512_fmadd_ps(acc3,t,cn[4]);
                           acc4 = _mm512_fmadd_ps(acc4,t,cd[5]);
                           acc4 = _mm512_fmadd_ps(acc4,t,cd[6]);
                           t1   = _mm512_div_ps(acc3,acc4);
                           cc   = _mm512_mul_ps(x,t1);
                           goto done;
                        }

                       if(_mm512_cmp_ps_mask(x,c1,_CMP_GT_OQ)) {
                          cc = hlf;
                          ss = hlf;
                          goto done;
                      }

                      /*		Asymptotic power series auxiliary functions
                       *		for large argument
                       */

                        volatile __m512 prefsn = _mm_prefetch((const char*)&fn[0],_MM_HINT_T0);
                        volatile __m512 prefsd = _mm_prefetch((const char*)&fd[0],_MM_HINT_T0);
                        volatile __m512 prefcn = _mm_prefetch((const char*)&gn[0],_MM_HINT_T0);
                        volatile __m512 prefcd = _mm_prefetch((const char*)&gd[0],_MM_HINT_T0);
                        t = _mm512_mul_ps(pi,x2);
                        u = _mm512_div_ps(_1,_mm512_mul_ps(t,t));
                        acc1 = fn[0];
                        acc2 = _mm512_add_ps(u,fd[0]);
                        acc3 = gn[0];
                        acc4 = _mm512_add_ps(u,gd[0]);
                        t = _mm512_div_ps(_1,t);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[1]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[1]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[2]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[2]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[3]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[3]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[4]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[4]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[5]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[5]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[6]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[6]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[7]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[7]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[8]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[8]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[9]);
                        t0   = _mm512_div_ps(acc1,acc2);
                        f    = _mm512_sub_ps(_1,_mm512_mul_ps(u,t0));
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[1]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[1]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[2]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[2]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[3]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[3]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[4]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[4]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[5]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[5]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[6]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[6]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[7]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[7]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[8]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[8]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gd[9]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[10]);
                        t1   = _mm512_div_ps(acc3,acc4);
                        g    = _mm512_mul_ps(t,t1);
                        
                        t    = _mm512_mul_ps(pio2,x2);
                        c    = xcosf(t);
                        s    = xsinf(t);
                        t    = _mm512_mul_ps(pi,x);
                        t0   = _mm512_fmsub_ps(f,s,_mm512_mul_ps(g,c));
                        cc   = _mm512_add_ps(hlf,_mm512_div_ps(t0,t));
                        t1   = _mm512_fmadd_ps(f,c,_mm512_mul_ps(g,s));
                        ss   = _mm512_sub_ps(hlf,_mm512_div_ps(t1,t));
done:
                     if(_mm512_cmp_ps_mask(xxa,
                                     _mm512_setzero_ps(),_CMP_LT_OQ)) {
                         cc = negate_zmm16r4(cc);
                         ss = negate_zmm16r4(ss);
                     }
                     
                     _mm512_store_ps(&cca[0] ,cc);
                     _mm512_store_ps(&ssa[0] ,ss);
              }


                 void fresnel_zmm16r4_u(  const float * __restrict  pxxa,
                                          float * __restrict  ssa,
                                          float * __restrict  cca) {

                      
                        register __m512 xxa = _mm512_loadu_ps(&pxxa[0]);
                        const __m512 c0   = _mm512_set1_ps(2.5625f);
                        const __m512 c1   = _mm512_set1_ps(36974.0f);
                        const __m512 hlf  = _mm512_set1_ps(0.5f);
                        const __m512 _0   = _mm512_setzero_ps();
                        const __m512 _1   = _mm512_set1_ps(1.0f); 
                        const __m512 pi   = _mm512_set1_ps(3.14159265358979323846264338328f);
                        const __m512 pio2 = _mm512_set1_ps(1.57079632679489661923132169164f);
                        register __m512 f,g,cc,ss,c,s,t,u,t0,t1;
                        register __m512 x,x2,acc1,acc2,acc3,acc4;
                       
                        x   = _mm512_abs_ps(xxa);
                        x2  = _mm512_mul_ps(x,x);
                        if(_mm512_cmp_ps_mask(x,c0,_CMP_LT_OQ)) {
			   volatile __m512 prefsn = _mm_prefetch((const char*)&sn[0],_MM_HINT_T0);
                           volatile __m512 prefsd = _mm_prefetch((const char*)&sd[0],_MM_HINT_T0);
                           volatile __m512 prefcn = _mm_prefetch((const char*)&cn[0],_MM_HINT_T0);
                           volatile __m512 prefcd = _mm_prefetch((const char*)&cd[0],_MM_HINT_T0);
                           t = _mm512_mul_ps(x2,x2);
                           acc1 = sn[0]; 
                           acc2 = _mm512_add_ps(t,sd[0]);
                           acc3 = cn[0];
                           acc4 = cd[0];
                           acc1 = _mm512_fmadd_ps(acc1,t,sn[1]);
                           acc2 = _mm512_fmadd_ps(acc2,t,sd[1]);
                           acc1 = _mm512_fmadd_ps(acc1,t,sn[2]);
                           acc2 = _mm512_fmadd_ps(acc2,t,sd[2]);
                           acc1 = _mm512_fmadd_ps(acc1,t,sn[3]);
                           acc2 = _mm512_fmadd_ps(acc2,t,sd[3]);
                           acc1 = _mm512_fmadd_ps(acc1,t,sn[4]);
                           acc2 = _mm512_fmadd_ps(acc2,t,sd[4]);
                           acc2 = _mm512_fmadd_ps(acc2,t,sd[5]);
                           t0   = _mm512_div_ps(acc1,acc2);
                           ss   = _mm512_mul_ps(_mm512_mul_ps(x,x2),t0);
                           acc3 = _mm512_fmadd_ps(acc3,t,cn[1]);
                           acc4 = _mm512_fmadd_ps(acc4,t,cd[1]);
                           acc3 = _mm512_fmadd_ps(acc3,t,cn[2]);
                           acc4 = _mm512_fmadd_ps(acc4,t,cd[2]);
                           acc3 = _mm512_fmadd_ps(acc3,t,cn[3]);
                           acc4 = _mm512_fmadd_ps(acc4,t,cd[3]);
                           acc3 = _mm512_fmadd_ps(acc3,t,cn[4]);
                           acc4 = _mm512_fmadd_ps(acc4,t,cd[5]);
                           acc4 = _mm512_fmadd_ps(acc4,t,cd[6]);
                           t1   = _mm512_div_ps(acc3,acc4);
                           cc   = _mm512_mul_ps(x,t1);
                           goto done;
                        }

                       if(_mm512_cmp_ps_mask(x,c1,_CMP_GT_OQ)) {
                          cc = hlf;
                          ss = hlf;
                          goto done;
                      }

                      /*		Asymptotic power series auxiliary functions
                       *		for large argument
                       */

                        volatile __m512 prefsn = _mm_prefetch((const char*)&fn[0],_MM_HINT_T0);
                        volatile __m512 prefsd = _mm_prefetch((const char*)&fd[0],_MM_HINT_T0);
                        volatile __m512 prefcn = _mm_prefetch((const char*)&gn[0],_MM_HINT_T0);
                        volatile __m512 prefcd = _mm_prefetch((const char*)&gd[0],_MM_HINT_T0);
                        t = _mm512_mul_ps(pi,x2);
                        u = _mm512_div_ps(_1,_mm512_mul_ps(t,t));
                        acc1 = fn[0];
                        acc2 = _mm512_add_ps(u,fd[0]);
                        acc3 = gn[0];
                        acc4 = _mm512_add_ps(u,gd[0]);
                        t = _mm512_div_ps(_1,t);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[1]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[1]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[2]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[2]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[3]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[3]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[4]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[4]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[5]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[5]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[6]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[6]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[7]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[7]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[8]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[8]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[9]);
                        t0   = _mm512_div_ps(acc1,acc2);
                        f    = _mm512_sub_ps(_1,_mm512_mul_ps(u,t0));
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[1]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[1]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[2]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[2]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[3]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[3]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[4]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[4]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[5]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[5]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[6]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[6]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[7]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[7]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[8]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[8]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gd[9]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[10]);
                        t1   = _mm512_div_ps(acc3,acc4);
                        g    = _mm512_mul_ps(t,t1);
                        
                        t    = _mm512_mul_ps(pio2,x2);
                        c    = xcosf(t);
                        s    = xsinf(t);
                        t    = _mm512_mul_ps(pi,x);
                        t0   = _mm512_fmsub_ps(f,s,_mm512_mul_ps(g,c));
                        cc   = _mm512_add_ps(hlf,_mm512_div_ps(t0,t));
                        t1   = _mm512_fmadd_ps(f,c,_mm512_mul_ps(g,s));
                        ss   = _mm512_sub_ps(hlf,_mm512_div_ps(t1,t));
done:
                     if(_mm512_cmp_ps_mask(xxa,
                                     _mm512_setzero_ps(),_CMP_LT_OQ)) {
                         cc = negate_zmm16r4(cc);
                         ss = negate_zmm16r4(ss);
                     }
                     
                     _mm512_storeu_ps(&cca[0] ,cc);
                     _mm512_storeu_ps(&ssa[0] ,ss);
              }


             //////////////////////////////////////////////////////////////////////////////


                
                  /*
                           Same as above -- divided into Fresnel 'C' integral
                           and Fresnel 'S' integral.
                     */


              __m512 fresnel_C_zmm16r4(const __m512 xxa) {
                                        
                        
                        const __m512 c0   = _mm512_set1_ps(2.5625f);
                        const __m512 c1   = _mm512_set1_ps(36974.0f);
                        const __m512 hlf  = _mm512_set1_ps(0.5f);
                        const __m512 _0   = _mm512_setzero_ps();
                        const __m512 _1   = _mm512_set1_ps(1.0f); 
                        const __m512 pi   = _mm512_set1_ps(3.14159265358979323846264338328f);
                        const __m512 pio2 = _mm512_set1_ps(1.57079632679489661923132169164f);
                        register __m512 f,g,cc,c,t,u,t0,t1;
                        register __m512 cca,x,x2,acc1,acc2,acc3,acc4;
                       
                        x   = _mm512_abs_ps(xxa);
                        x2  = _mm512_mul_ps(x,x);
                        if(_mm512_cmp_ps_mask(x,c0,_CMP_LT_OQ)) {
			   volatile __m512 prefcn = _mm_prefetch((const char*)&cn[0],_MM_HINT_T0);
                           volatile __m512 prefcd = _mm_prefetch((const char*)&cd[0],_MM_HINT_T0);
                          
                           t = _mm512_mul_ps(x2,x2);
                           acc3 = cn[0];
                           acc4 = cd[0];
                           acc3 = _mm512_fmadd_ps(acc3,t,cn[1]);
                           acc4 = _mm512_fmadd_ps(acc4,t,cd[1]);
                           acc3 = _mm512_fmadd_ps(acc3,t,cn[2]);
                           acc4 = _mm512_fmadd_ps(acc4,t,cd[2]);
                           acc3 = _mm512_fmadd_ps(acc3,t,cn[3]);
                           acc4 = _mm512_fmadd_ps(acc4,t,cd[3]);
                           acc3 = _mm512_fmadd_ps(acc3,t,cn[4]);
                           acc4 = _mm512_fmadd_ps(acc4,t,cd[5]);
                           acc4 = _mm512_fmadd_ps(acc4,t,cd[6]);
                           t1   = _mm512_div_ps(acc3,acc4);
                           cc   = _mm512_mul_ps(x,t1);
                           goto done;
                        }

                       if(_mm512_cmp_ps_mask(x,c1,_CMP_GT_OQ)) {
                          cc = hlf;
                          goto done;
                      }

                      /*		Asymptotic power series auxiliary functions
                       *		for large argument
                       */

                        volatile __m512 prefsn = _mm_prefetch((const char*)&fn[0],_MM_HINT_T0);
                        volatile __m512 prefsd = _mm_prefetch((const char*)&fd[0],_MM_HINT_T0);
                        volatile __m512 prefcn = _mm_prefetch((const char*)&gn[0],_MM_HINT_T0);
                        volatile __m512 prefcd = _mm_prefetch((const char*)&gd[0],_MM_HINT_T0);
                        t = _mm512_mul_ps(pi,x2);
                        u = _mm512_div_ps(_1,_mm512_mul_ps(t,t));
                        acc1 = fn[0];
                        acc2 = _mm512_add_ps(u,fd[0]);
                        acc3 = gn[0];
                        acc4 = _mm512_add_ps(u,gd[0]);
                        t = _mm512_div_ps(_1,t);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[1]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[1]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[2]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[2]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[3]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[3]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[4]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[4]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[5]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[5]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[6]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[6]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[7]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[7]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[8]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[8]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[9]);
                        t0   = _mm512_div_ps(acc1,acc2);
                        f    = _mm512_sub_ps(_1,_mm512_mul_ps(u,t0));
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[1]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[1]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[2]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[2]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[3]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[3]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[4]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[4]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[5]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[5]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[6]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[6]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[7]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[7]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[8]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[8]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gd[9]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[10]);
                        t1   = _mm512_div_ps(acc3,acc4);
                        g    = _mm512_mul_ps(t,t1);
                        
                        t    = _mm512_mul_ps(pio2,x2);
                        c    = xcosf(t);
                        t    = _mm512_mul_ps(pi,x);
                        t0   = _mm512_fmsub_ps(f,s,_mm512_mul_ps(g,c));
                        cc   = _mm512_add_ps(hlf,_mm512_div_ps(t0,t));
done:
                     if(_mm512_cmp_ps_mask(xxa,
                                     _mm512_setzero_ps(),_CMP_LT_OQ)) {
                         cc = negate_zmm16r4(cc);
                     }
                     
                     cca = cc;
                     return (cca);
              }


                __m512 fresnel_C_zmm16r4_a(const float * __restrict __attribute__((aligned(64))) pxxa) {
                                        
                       
                        register __m512 xxa = _mm512_load_ps(&pxxa[0]);
                        const __m512 c0   = _mm512_set1_ps(2.5625f);
                        const __m512 c1   = _mm512_set1_ps(36974.0f);
                        const __m512 hlf  = _mm512_set1_ps(0.5f);
                        const __m512 _0   = _mm512_setzero_ps();
                        const __m512 _1   = _mm512_set1_ps(1.0f); 
                        const __m512 pi   = _mm512_set1_ps(3.14159265358979323846264338328f);
                        const __m512 pio2 = _mm512_set1_ps(1.57079632679489661923132169164f);
                        register __m512 f,g,cc,c,t,u,t0,t1;
                        register __m512 cca,x,x2,acc1,acc2,acc3,acc4;
                       
                        x   = _mm512_abs_ps(xxa);
                        x2  = _mm512_mul_ps(x,x);
                        if(_mm512_cmp_ps_mask(x,c0,_CMP_LT_OQ)) {
			   volatile __m512 prefcn = _mm_prefetch((const char*)&cn[0],_MM_HINT_T0);
                           volatile __m512 prefcd = _mm_prefetch((const char*)&cd[0],_MM_HINT_T0);
                          
                           t = _mm512_mul_ps(x2,x2);
                           
                           acc3 = cn[0];
                           acc4 = cd[0];
                           acc3 = _mm512_fmadd_ps(acc3,t,cn[1]);
                           acc4 = _mm512_fmadd_ps(acc4,t,cd[1]);
                           acc3 = _mm512_fmadd_ps(acc3,t,cn[2]);
                           acc4 = _mm512_fmadd_ps(acc4,t,cd[2]);
                           acc3 = _mm512_fmadd_ps(acc3,t,cn[3]);
                           acc4 = _mm512_fmadd_ps(acc4,t,cd[3]);
                           acc3 = _mm512_fmadd_ps(acc3,t,cn[4]);
                           acc4 = _mm512_fmadd_ps(acc4,t,cd[5]);
                           acc4 = _mm512_fmadd_ps(acc4,t,cd[6]);
                           t1   = _mm512_div_ps(acc3,acc4);
                           cc   = _mm512_mul_ps(x,t1);
                           goto done;
                        }

                       if(_mm512_cmp_ps_mask(x,c1,_CMP_GT_OQ)) {
                          cc = hlf;
                          goto done;
                      }

                      /*		Asymptotic power series auxiliary functions
                       *		for large argument
                       */

                        volatile __m512 prefsn = _mm_prefetch((const char*)&fn[0],_MM_HINT_T0);
                        volatile __m512 prefsd = _mm_prefetch((const char*)&fd[0],_MM_HINT_T0);
                        volatile __m512 prefcn = _mm_prefetch((const char*)&gn[0],_MM_HINT_T0);
                        volatile __m512 prefcd = _mm_prefetch((const char*)&gd[0],_MM_HINT_T0);
                        t = _mm512_mul_ps(pi,x2);
                        u = _mm512_div_ps(_1,_mm512_mul_ps(t,t));
                        acc1 = fn[0];
                        acc2 = _mm512_add_ps(u,fd[0]);
                        acc3 = gn[0];
                        acc4 = _mm512_add_ps(u,gd[0]);
                        t = _mm512_div_ps(_1,t);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[1]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[1]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[2]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[2]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[3]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[3]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[4]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[4]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[5]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[5]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[6]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[6]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[7]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[7]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[8]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[8]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[9]);
                        t0   = _mm512_div_ps(acc1,acc2);
                        f    = _mm512_sub_ps(_1,_mm512_mul_ps(u,t0));
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[1]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[1]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[2]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[2]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[3]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[3]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[4]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[4]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[5]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[5]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[6]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[6]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[7]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[7]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[8]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[8]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gd[9]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[10]);
                        t1   = _mm512_div_ps(acc3,acc4);
                        g    = _mm512_mul_ps(t,t1);
                        
                        t    = _mm512_mul_ps(pio2,x2);
                        c    = xcosf(t);
                        t    = _mm512_mul_ps(pi,x);
                        t0   = _mm512_fmsub_ps(f,s,_mm512_mul_ps(g,c));
                        cc   = _mm512_add_ps(hlf,_mm512_div_ps(t0,t));
done:
                     if(_mm512_cmp_ps_mask(xxa,
                                     _mm512_setzero_ps(),_CMP_LT_OQ)) {
                         cc = negate_zmm16r4(cc);
                     }
                     
                     cca = cc;
                     return (cca);
              }


                __m512 fresnel_C_zmm16r4_u(const float * __restrict  pxxa) {
                                        
                        
                        register __m512 xxa = _mm512_loadu_ps(&pxxa[0]);
                        const __m512 c0   = _mm512_set1_ps(2.5625f);
                        const __m512 c1   = _mm512_set1_ps(36974.0f);
                        const __m512 hlf  = _mm512_set1_ps(0.5f);
                        const __m512 _0   = _mm512_setzero_ps();
                        const __m512 _1   = _mm512_set1_ps(1.0f); 
                        const __m512 pi   = _mm512_set1_ps(3.14159265358979323846264338328f);
                        const __m512 pio2 = _mm512_set1_ps(1.57079632679489661923132169164f);
                        register __m512 f,g,cc,c,t,u,t0,t1;
                        register __m512 cca,x,x2,acc1,acc2,acc3,acc4;
                       
                        x   = _mm512_abs_ps(xxa);
                        x2  = _mm512_mul_ps(x,x);
                        if(_mm512_cmp_ps_mask(x,c0,_CMP_LT_OQ)) {
			   volatile __m512 prefsn = _mm_prefetch((const char*)&sn[0],_MM_HINT_T0);
                           volatile __m512 prefsd = _mm_prefetch((const char*)&sd[0],_MM_HINT_T0);
                          
                           t = _mm512_mul_ps(x2,x2);
                           acc3 = cn[0];
                           acc4 = cd[0];
                           acc3 = _mm512_fmadd_ps(acc3,t,cn[1]);
                           acc4 = _mm512_fmadd_ps(acc4,t,cd[1]);
                           acc3 = _mm512_fmadd_ps(acc3,t,cn[2]);
                           acc4 = _mm512_fmadd_ps(acc4,t,cd[2]);
                           acc3 = _mm512_fmadd_ps(acc3,t,cn[3]);
                           acc4 = _mm512_fmadd_ps(acc4,t,cd[3]);
                           acc3 = _mm512_fmadd_ps(acc3,t,cn[4]);
                           acc4 = _mm512_fmadd_ps(acc4,t,cd[5]);
                           acc4 = _mm512_fmadd_ps(acc4,t,cd[6]);
                           t1   = _mm512_div_ps(acc3,acc4);
                           cc   = _mm512_mul_ps(x,t1);
                           goto done;
                        }

                       if(_mm512_cmp_ps_mask(x,c1,_CMP_GT_OQ)) {
                          cc = hlf;
                          goto done;
                      }

                      /*		Asymptotic power series auxiliary functions
                       *		for large argument
                       */

                        volatile __m512 prefsn = _mm_prefetch((const char*)&fn[0],_MM_HINT_T0);
                        volatile __m512 prefsd = _mm_prefetch((const char*)&fd[0],_MM_HINT_T0);
                        volatile __m512 prefcn = _mm_prefetch((const char*)&gn[0],_MM_HINT_T0);
                        volatile __m512 prefcd = _mm_prefetch((const char*)&gd[0],_MM_HINT_T0);
                        t = _mm512_mul_ps(pi,x2);
                        u = _mm512_div_ps(_1,_mm512_mul_ps(t,t));
                        acc1 = fn[0];
                        acc2 = _mm512_add_ps(u,fd[0]);
                        acc3 = gn[0];
                        acc4 = _mm512_add_ps(u,gd[0]);
                        t = _mm512_div_ps(_1,t);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[1]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[1]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[2]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[2]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[3]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[3]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[4]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[4]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[5]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[5]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[6]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[6]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[7]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[7]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[8]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[8]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[9]);
                        t0   = _mm512_div_ps(acc1,acc2);
                        f    = _mm512_sub_ps(_1,_mm512_mul_ps(u,t0));
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[1]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[1]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[2]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[2]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[3]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[3]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[4]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[4]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[5]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[5]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[6]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[6]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[7]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[7]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[8]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[8]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gd[9]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[10]);
                        t1   = _mm512_div_ps(acc3,acc4);
                        g    = _mm512_mul_ps(t,t1);
                        
                        t    = _mm512_mul_ps(pio2,x2);
                        c    = xcosf(t);
                        t    = _mm512_mul_ps(pi,x);
                        t0   = _mm512_fmsub_ps(f,s,_mm512_mul_ps(g,c));
                        cc   = _mm512_add_ps(hlf,_mm512_div_ps(t0,t));
done:
                     if(_mm512_cmp_ps_mask(xxa,
                                     _mm512_setzero_ps(),_CMP_LT_OQ)) {
                         cc = negate_zmm16r4(cc);
                     }
                     
                     cca = cc;
                     return (cca);
              }


         /////////////////////////////////////////////////////////////////////////////////////////////
       
                   __m512 fresnel_S_zmm16r4(const __m512 xxa) {
                                        
                        
                        const __m512 c0   = _mm512_set1_ps(2.5625f);
                        const __m512 c1   = _mm512_set1_ps(36974.0f);
                        const __m512 hlf  = _mm512_set1_ps(0.5f);
                        const __m512 _0   = _mm512_setzero_ps();
                        const __m512 _1   = _mm512_set1_ps(1.0f); 
                        const __m512 pi   = _mm512_set1_ps(3.14159265358979323846264338328f);
                        const __m512 pio2 = _mm512_set1_ps(1.57079632679489661923132169164f);
                        register __m512 f,g,ss,s,t,u,t0,t1;
                        register __m512 ssa,x,x2,acc1,acc2,acc3,acc4;
                       
                        x   = _mm512_abs_ps(xxa);
                        x2  = _mm512_mul_ps(x,x);
                        if(_mm512_cmp_ps_mask(x,c0,_CMP_LT_OQ)) {
			   volatile __m512 prefsn = _mm_prefetch((const char*)&sn[0],_MM_HINT_T0);
                           volatile __m512 prefsd = _mm_prefetch((const char*)&sd[0],_MM_HINT_T0);
                           
                           t = _mm512_mul_ps(x2,x2);
                           acc1 = sn[0]; 
                           acc2 = _mm512_add_ps(t,sd[0]);
                           acc1 = _mm512_fmadd_ps(acc1,t,sn[1]);
                           acc2 = _mm512_fmadd_ps(acc2,t,sd[1]);
                           acc1 = _mm512_fmadd_ps(acc1,t,sn[2]);
                           acc2 = _mm512_fmadd_ps(acc2,t,sd[2]);
                           acc1 = _mm512_fmadd_ps(acc1,t,sn[3]);
                           acc2 = _mm512_fmadd_ps(acc2,t,sd[3]);
                           acc1 = _mm512_fmadd_ps(acc1,t,sn[4]);
                           acc2 = _mm512_fmadd_ps(acc2,t,sd[4]);
                           acc2 = _mm512_fmadd_ps(acc2,t,sd[5]);
                           t0   = _mm512_div_ps(acc1,acc2);
                           ss   = _mm512_mul_ps(_mm512_mul_ps(x,x2),t0);
                           goto done;
                        }

                       if(_mm512_cmp_ps_mask(x,c1,_CMP_GT_OQ)) {
                          ss = hlf;
                          goto done;
                      }

                      /*		Asymptotic power series auxiliary functions
                       *		for large argument
                       */

                        volatile __m512 prefsn = _mm_prefetch((const char*)&fn[0],_MM_HINT_T0);
                        volatile __m512 prefsd = _mm_prefetch((const char*)&fd[0],_MM_HINT_T0);
                        volatile __m512 prefcn = _mm_prefetch((const char*)&gn[0],_MM_HINT_T0);
                        volatile __m512 prefcd = _mm_prefetch((const char*)&gd[0],_MM_HINT_T0);
                        t = _mm512_mul_ps(pi,x2);
                        u = _mm512_div_ps(_1,_mm512_mul_ps(t,t));
                        acc1 = fn[0];
                        acc2 = _mm512_add_ps(u,fd[0]);
                        acc3 = gn[0];
                        acc4 = _mm512_add_ps(u,gd[0]);
                        t = _mm512_div_ps(_1,t);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[1]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[1]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[2]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[2]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[3]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[3]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[4]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[4]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[5]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[5]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[6]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[6]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[7]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[7]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[8]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[8]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[9]);
                        t0   = _mm512_div_ps(acc1,acc2);
                        f    = _mm512_sub_ps(_1,_mm512_mul_ps(u,t0));
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[1]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[1]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[2]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[2]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[3]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[3]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[4]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[4]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[5]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[5]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[6]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[6]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[7]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[7]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[8]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[8]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gd[9]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[10]);
                        t1   = _mm512_div_ps(acc3,acc4);
                        g    = _mm512_mul_ps(t,t1);
                        
                        t    = _mm512_mul_ps(pio2,x2);
                        s    = xsinf(t);
                        t    = _mm512_mul_ps(pi,x);
                        t1   = _mm512_fmadd_ps(f,c,_mm512_mul_ps(g,s));
                        ss   = _mm512_sub_ps(hlf,_mm512_div_ps(t1,t));
done:
                     if(_mm512_cmp_ps_mask(xxa,
                                     _mm512_setzero_ps(),_CMP_LT_OQ)) {
                         ss = negate_zmm16r4(ss);
                     }
                     
                     ssa = ss;
                     return (ssa);
              }


               __m512 fresnel_S_zmm16r4_a(const float * __restrict __attribute__((aligned(64))) pxxa) {
                                        
                        register __m512 xxa = _mm512_load_ps(&pxxa[0]);
                        const __m512 c0   = _mm512_set1_ps(2.5625f);
                        const __m512 c1   = _mm512_set1_ps(36974.0f);
                        const __m512 hlf  = _mm512_set1_ps(0.5f);
                        const __m512 _0   = _mm512_setzero_ps();
                        const __m512 _1   = _mm512_set1_ps(1.0f); 
                        const __m512 pi   = _mm512_set1_ps(3.14159265358979323846264338328f);
                        const __m512 pio2 = _mm512_set1_ps(1.57079632679489661923132169164f);
                        register __m512 f,g,ss,s,t,u,t0,t1;
                        register __m512 ssa,x,x2,acc1,acc2,acc3,acc4;
                       
                        x   = _mm512_abs_ps(xxa);
                        x2  = _mm512_mul_ps(x,x);
                        if(_mm512_cmp_ps_mask(x,c0,_CMP_LT_OQ)) {
			   volatile __m512 prefsn = _mm_prefetch((const char*)&sn[0],_MM_HINT_T0);
                           volatile __m512 prefsd = _mm_prefetch((const char*)&sd[0],_MM_HINT_T0);
                           
                           t = _mm512_mul_ps(x2,x2);
                           acc1 = sn[0]; 
                           acc2 = _mm512_add_ps(t,sd[0]);
                           acc1 = _mm512_fmadd_ps(acc1,t,sn[1]);
                           acc2 = _mm512_fmadd_ps(acc2,t,sd[1]);
                           acc1 = _mm512_fmadd_ps(acc1,t,sn[2]);
                           acc2 = _mm512_fmadd_ps(acc2,t,sd[2]);
                           acc1 = _mm512_fmadd_ps(acc1,t,sn[3]);
                           acc2 = _mm512_fmadd_ps(acc2,t,sd[3]);
                           acc1 = _mm512_fmadd_ps(acc1,t,sn[4]);
                           acc2 = _mm512_fmadd_ps(acc2,t,sd[4]);
                           acc2 = _mm512_fmadd_ps(acc2,t,sd[5]);
                           t0   = _mm512_div_ps(acc1,acc2);
                           ss   = _mm512_mul_ps(_mm512_mul_ps(x,x2),t0);
                           goto done;
                        }

                       if(_mm512_cmp_ps_mask(x,c1,_CMP_GT_OQ)) {
                          ss = hlf;
                          goto done;
                      }

                      /*		Asymptotic power series auxiliary functions
                       *		for large argument
                       */

                        volatile __m512 prefsn = _mm_prefetch((const char*)&fn[0],_MM_HINT_T0);
                        volatile __m512 prefsd = _mm_prefetch((const char*)&fd[0],_MM_HINT_T0);
                        volatile __m512 prefcn = _mm_prefetch((const char*)&gn[0],_MM_HINT_T0);
                        volatile __m512 prefcd = _mm_prefetch((const char*)&gd[0],_MM_HINT_T0);
                        t = _mm512_mul_ps(pi,x2);
                        u = _mm512_div_ps(_1,_mm512_mul_ps(t,t));
                        acc1 = fn[0];
                        acc2 = _mm512_add_ps(u,fd[0]);
                        acc3 = gn[0];
                        acc4 = _mm512_add_ps(u,gd[0]);
                        t = _mm512_div_ps(_1,t);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[1]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[1]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[2]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[2]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[3]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[3]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[4]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[4]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[5]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[5]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[6]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[6]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[7]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[7]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[8]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[8]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[9]);
                        t0   = _mm512_div_ps(acc1,acc2);
                        f    = _mm512_sub_ps(_1,_mm512_mul_ps(u,t0));
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[1]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[1]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[2]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[2]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[3]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[3]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[4]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[4]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[5]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[5]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[6]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[6]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[7]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[7]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[8]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[8]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gd[9]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[10]);
                        t1   = _mm512_div_ps(acc3,acc4);
                        g    = _mm512_mul_ps(t,t1);
                        
                        t    = _mm512_mul_ps(pio2,x2);
                        s    = xsinf(t);
                        t    = _mm512_mul_ps(pi,x);
                        t1   = _mm512_fmadd_ps(f,c,_mm512_mul_ps(g,s));
                        ss   = _mm512_sub_ps(hlf,_mm512_div_ps(t1,t));
done:
                     if(_mm512_cmp_ps_mask(xxa,
                                     _mm512_setzero_ps(),_CMP_LT_OQ)) {
                         ss = negate_zmm16r4(ss);
                     }
                     
                     ssa = ss;
                     return (ssa);
              }


               __m512 fresnel_S_zmm16r4_u(const float * __restrict  pxxa) {
                                        
                       
                        register __m512 xxa = _mm512_loadu_ps(&pxxa[0]);
                        const __m512 c0   = _mm512_set1_ps(2.5625f);
                        const __m512 c1   = _mm512_set1_ps(36974.0f);
                        const __m512 hlf  = _mm512_set1_ps(0.5f);
                        const __m512 _0   = _mm512_setzero_ps();
                        const __m512 _1   = _mm512_set1_ps(1.0f); 
                        const __m512 pi   = _mm512_set1_ps(3.14159265358979323846264338328f);
                        const __m512 pio2 = _mm512_set1_ps(1.57079632679489661923132169164f);
                        register __m512 f,g,ss,s,t,u,t0,t1;
                        register __m512 ssa,x,x2,acc1,acc2,acc3,acc4;
                       
                        x   = _mm512_abs_ps(xxa);
                        x2  = _mm512_mul_ps(x,x);
                        if(_mm512_cmp_ps_mask(x,c0,_CMP_LT_OQ)) {
			   volatile __m512 prefsn = _mm_prefetch((const char*)&sn[0],_MM_HINT_T0);
                           volatile __m512 prefsd = _mm_prefetch((const char*)&sd[0],_MM_HINT_T0);
                           
                           t = _mm512_mul_ps(x2,x2);
                           acc1 = sn[0]; 
                           acc2 = _mm512_add_ps(t,sd[0]);
                           acc1 = _mm512_fmadd_ps(acc1,t,sn[1]);
                           acc2 = _mm512_fmadd_ps(acc2,t,sd[1]);
                           acc1 = _mm512_fmadd_ps(acc1,t,sn[2]);
                           acc2 = _mm512_fmadd_ps(acc2,t,sd[2]);
                           acc1 = _mm512_fmadd_ps(acc1,t,sn[3]);
                           acc2 = _mm512_fmadd_ps(acc2,t,sd[3]);
                           acc1 = _mm512_fmadd_ps(acc1,t,sn[4]);
                           acc2 = _mm512_fmadd_ps(acc2,t,sd[4]);
                           acc2 = _mm512_fmadd_ps(acc2,t,sd[5]);
                           t0   = _mm512_div_ps(acc1,acc2);
                           ss   = _mm512_mul_ps(_mm512_mul_ps(x,x2),t0);
                           goto done;
                        }

                       if(_mm512_cmp_ps_mask(x,c1,_CMP_GT_OQ)) {
                          ss = hlf;
                          goto done;
                      }

                      /*		Asymptotic power series auxiliary functions
                       *		for large argument
                       */

                        volatile __m512 prefsn = _mm_prefetch((const char*)&fn[0],_MM_HINT_T0);
                        volatile __m512 prefsd = _mm_prefetch((const char*)&fd[0],_MM_HINT_T0);
                        volatile __m512 prefcn = _mm_prefetch((const char*)&gn[0],_MM_HINT_T0);
                        volatile __m512 prefcd = _mm_prefetch((const char*)&gd[0],_MM_HINT_T0);
                        t = _mm512_mul_ps(pi,x2);
                        u = _mm512_div_ps(_1,_mm512_mul_ps(t,t));
                        acc1 = fn[0];
                        acc2 = _mm512_add_ps(u,fd[0]);
                        acc3 = gn[0];
                        acc4 = _mm512_add_ps(u,gd[0]);
                        t = _mm512_div_ps(_1,t);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[1]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[1]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[2]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[2]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[3]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[3]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[4]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[4]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[5]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[5]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[6]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[6]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[7]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[7]);
                        acc1 = _mm512_fmadd_ps(acc1,u,fn[8]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[8]);
                        acc2 = _mm512_fmadd_ps(acc2,u,fd[9]);
                        t0   = _mm512_div_ps(acc1,acc2);
                        f    = _mm512_sub_ps(_1,_mm512_mul_ps(u,t0));
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[1]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[1]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[2]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[2]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[3]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[3]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[4]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[4]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[5]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[5]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[6]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[6]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[7]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[7]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gn[8]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[8]);
                        acc3 = _mm512_fmadd_ps(acc3,u,gd[9]);
                        acc4 = _mm512_fmadd_ps(acc4,u,gd[10]);
                        t1   = _mm512_div_ps(acc3,acc4);
                        g    = _mm512_mul_ps(t,t1);
                        
                        t    = _mm512_mul_ps(pio2,x2);
                        s    = xsinf(t);
                        t    = _mm512_mul_ps(pi,x);
                        t1   = _mm512_fmadd_ps(f,c,_mm512_mul_ps(g,s));
                        ss   = _mm512_sub_ps(hlf,_mm512_div_ps(t1,t));
done:
                     if(_mm512_cmp_ps_mask(xxa,
                                     _mm512_setzero_ps(),_CMP_LT_OQ)) {
                         ss = negate_zmm16r4(ss);
                     }
                     
                     ssa = ss;
                     return (ssa);
              }
     
       
  



    
    

                


 



















