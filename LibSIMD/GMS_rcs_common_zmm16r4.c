

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


 



















