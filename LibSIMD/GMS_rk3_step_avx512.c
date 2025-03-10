
#include "GMS_rk3_step_avx512.h"
#include "GMS_simd_utils.h"




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
/*
     Adapted from: http://www.mymathlib.com/diffeq/runge-kutta/runge_kutta_v1_3.html
     Manually vectorized by Bernard Gingold, beniekg@gmail.com
*/


                        __m512d
			rk3_step_zmm8r8(__m512d(*f)(__m512d,
			                             __m512d),
					 __m512d y0,
					 __m512d x0,
					 const __m512d h,
					 const int32_t n) {

			   const __m512d _1_6 = _mm512_set1_pd(0.1666666666666666666667);
			   const __m512d _0_5 = _mm512_set1_pd(0.5);
			   const __m512d _4   = _mm512_set1_pd(4.0);
			   const __m512d h2   = _mm512_mul_pd(_0_5,h);
			   const __m512d h6   = _mm512_mul_pd(_1_6,h);
                           __m512d k1,k2;
			   __m512d nk1,t0;
			   while(--n >= 0) {
                                 k1 = f(x0,y0);
				 k2 = f(_mm512_add_pd(x0,h2),
				        _mm512_fmadd_pd(h2,k1,y0));
				 t0 = _mm512_fmadd_pd(h,_mm512_add_pd(zmm8r8_negate(k1),
				                                      _mm512_add_pd(k2,k2)));
				 y0 = _mm512_add_pd(y0,_mm512_mul_pd(h6,
				                 _mm512_add_pd(_mm512_fmadd_pd(_4,k2,k1),f(_mm512_add_pd(x0,h),t0))));
				 x0 = _mm512_add_pd(x0,h);
			   }
			   return (y0);
		      }


		       
			__m512
			rk3_step_zmm16r4(__m512(*f)(__m512,
			                             __m512),
					 __m512 y0,
					 __m512 x0,
					 const __m512 h,
					 const int32_t n) {

			   const __m512 _1_6 = _mm512_set1_ps(0.1666666666666666666667f);
			   const __m512 _0_5 = _mm512_set1_ps(0.5f);
			   const __m512 _4   = _mm512_set1_ps(4.0f);
			   const __m512 h2   = _mm512_mul_ps(_0_5,h);
			   const __m512 h6   = _mm512_mul_ps(_1_6,h);
                           __m512 k1,k2;
			   __m512 t0;
			   while(--n >= 0) {
                                 k1 = f(x0,y0);
				 k2 = f(_mm512_add_ps(x0,h2),
				        _mm512_fmadd_ps(h2,k1,y0));
				 t0 = _mm512_fmadd_ps(h,_mm512_add_ps(zmm16r4_negate(k1),
				                                      _mm512_add_ps(k2,k2)));
				 y0 = _mm512_add_ps(y0,_mm512_mul_ps(h6,
				                 _mm512_add_ps(_mm512_fmadd_ps(_4,k2,k1),f(_mm512_add_ps(x0,h),t0))));
				 x0 = _mm512_add_ps(x0,h);
			   }
			   return (y0);
		      }


		        __m512d
			rk3_richardson_zmm8r8(__m512d(*f)(__m512d,
			                                     __m512d),
						 __m512d y0,
						 __m512d x0,
						 const __m512d h,
						 const int32_t n,
						 int32_t n_cols) {

			   __ATTR_ALIGN__(64) const __m512d richardson[] = {_mm512_set1_pd(1.0/7.0),
			                                                    _mm512_set1_pd(1.0/15.0),
								            _mm512_set1_pd(1.0/31.0),
								            _mm512_set1_pd(1.0/63.0),
								            _mm512_set1_pd(1.0/127.0)};
			   constexpr int32_t MAX_COLS = 6;
			   __ATTR_ALIGN__(64) __m512d dt[MAX_COLS];
			   const __m512d _1_2 = _mm512_set1_pd(0.5);
			   __m512d integral,delta,h_used;
			   int32_t n_subints;
			   n_cols = max(1,min(MAX_COLS,n_cols));
			   while(--n >= 0) {
                                 h_used = h;
				 n_subints = 1;
				 #pragma loop_count min(1),max(6),avg(3)
				 for(int32_t j = 0; j < n_cols; ++j) {
                                     integral = rk3_step_zmm8r8(f,y0,x0,h_used,n_subints);
				     for(int32_t k = 0; k < j; ++k) {
                                         delta = _mm512_sub_pd(integral,dt[k]);
					 dt[k] = integral;
					 integral = _mm512_fmadd_pd(richardson[k],delta,integral);
				     }
				     dt[j] = integral;
				     h_used = _mm512_mul_pd(h_used,_1_2);
				     n_subints += n_subints;
				 }
				 y0 = integral;
				 x0 = _mm512_add_pd(x0,h);
			   }
			   return (y0);
		    }


		        __ATTR_ALWAYS_INLINE__
                        __ATTR_HOT__
                        __ATTR_ALIGN__(32)
			__ATTR_VECTORCALL__
	                static inline
			__m512
			rk3_richardson_zmm16r4(__m512(*f)(__m512,
			                                     __m512),
						 __m512 y0,
						 __m512 x0,
						 const __m512 h,
						 const int32_t n,
						 int32_t n_cols) {

			   __ATTR_ALIGN__(64) const __m512 richardson[] = { _mm512_set1_ps(1.0f/7.0f),
			                                                    _mm512_set1_ps(1.0f/15.0f),
								            _mm512_set1_ps(1.0f/31.0f),
								            _mm512_set1_ps(1.0f/63.0f),
								            _mm512_set1_ps(1.0f/127.0f)};
			   constexpr int32_t MAX_COLS = 6;
			   __ATTR_ALIGN__(64) __m512 dt[MAX_COLS];
			   const __m512 _1_2 = _mm512_set1_ps(0.5f);
			   __m512 integral,delta,h_used;
			   int32_t n_subints;
			   n_cols = max(1,min(MAX_COLS,n_cols));
			   while(--n >= 0) {
                                 h_used = h;
				 n_subints = 1;
				 #pragma loop_count min(1),max(6),avg(3)
				 for(int32_t j = 0; j < n_cols; ++j) {
                                     integral = rk3_step_zmm16r4(f,y0,x0,h_used,n_subints);
				     for(int32_t k = 0; k < j; ++k) {
                                         delta = _mm512_sub_ps(integral,dt[k]);
					 dt[k] = integral;
					 integral = _mm512_fmadd_ps(richardson[k],delta,integral);
				     }
				     dt[j] = integral;
				     h_used = _mm512_mul_ps(h_used,_1_2);
				     n_subints += n_subints;
				 }
				 y0 = integral;
				 x0 = _mm512_add_ps(x0,h);
			   }
			   return (y0);
		    }


