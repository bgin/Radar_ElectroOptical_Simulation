#include "GMS_rk4_step_avx.h"
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
   Adapted from: http://www.mymathlib.com/diffeq/runge-kutta/
   Manually vectorized by Bernard Gingold, beniekg@gmail.com
*/


                        __m256d
		        rk4_step_ymm4r8(__m256d(*f)(__m256d,
			                            __m256d),
				        __m256d y0,
				        __m256d x0,
					const __m256d h,
					const int32_t n) {

                           const __m256d _1_6 = _mm256_set1_pd(0.1666666666666666666667);
			   const __m256d _0_5 = _mm256_set1_pd(0.5);
			   const __m256d h2   = _mm256_mul_pd(_0_5,h);
			   __m256d k1,k2,k3,k4,tmp;
			   while(--n >= 0) { // for 'n' attempts do:
                                 k1 = _mm256_mul_pd(h,f(x0,y0));
				 k2 = _mm256_mul_pd(h,f(_mm256_add_pd(x0,h2),
				                                  _mm256_fmadd_pd(_0_5,k1,y0)));
				 tmp= _mm256_add_pd(k1,k2);
				 k3 = _mm256_mul_pd(h,f(_mm256_add_pd(x0,h2),
				                                  _mm256_fmadd_pd(_0_5,k2,y0)));
				 tmp= _mm256_add_pd(tmp,_mm256_add_pd(k2,k3));
				 x0 = _mm256_add_pd(x0,h);
				 k4 = _mm256_mul_pd(h,f(x0,_mm256_add_pd(y0,k3)));
				 tmp= _mm256_add_pd(tmp,_mm256_add_pd(k3,k4));
				 y0 = _mm256_fmadd_pd(y0,_1_6,tmp);
			  }
			  return (y0);
		     }


		        __m256
		        rk4_step_ymm8r44(__m256(*f)(__m256,
			                            __m256),
				         __m256 y0,
				         __m256 x0,
					 const __m256 h,
					 const int32_t n) {

                           const __m256 _1_6 = _mm256_set1_ps(0.1666666666666666666667f);
			   const __m256 _0_5 = _mm256_set1_ps(0.5f);
			   const __m256 h2   = _mm256_mul_ps(_0_5,h);
			   __m256 k1,k2,k3,k4,tmp;
			   while(--n >= 0) { // for 'n' attempts do:
                                 k1 = _mm256_mul_ps(h,f(x0,y0));
				 k2 = _mm256_mul_ps(h,f(_mm256_add_ps(x0,h2),
				                                  _mm256_fmadd_ps(_0_5,k1,y0)));
				 tmp= _mm256_add_ps(k1,k2);
				 k3 = _mm256_mul_ps(h,f(_mm256_add_ps(x0,h2),
				                                  _mm256_fmadd_ps(_0_5,k2,y0)));
				 tmp= _mm256_add_ps(tmp,_mm256_add_ps(k2,k3));
				 x0 = _mm256_add_ps(x0,h);
				 k4 = _mm256_mul_ps(h,f(x0,_mm256_add_ps(y0,k3)));
				 tmp= _mm256_add_ps(tmp,_mm256_add_ps(k3,k4));
				 y0 = _mm256_fmadd_ps(y0,_1_6,tmp);
			  }
			  return (y0);
		     }


		        __m256d
			rk4_richardson_ymm4r8(__m256d(*f)(__m256d,
			                                     __m256d),
						 __m256d y0,
						 __m256d x0,
						 const __m256d h,
						 const int32_t n,
						 int32_t n_cols) {

			   __ATTR_ALIGN__(32) const __m256d richardson[] = {_mm256_set1_pd(1.0/15.0),
			                                                    _mm256_set1_pd(1.0/31.0),
								            _mm256_set1_pd(1.0/63.0),
								            _mm256_set1_pd(1.0/127.0),
								            _mm256_set1_pd(1.0/255.0),
									    _mm256_set1_pd(1.0/511.0)};
			   constexpr int32_t MAX_COLS = 7;
			   __ATTR_ALIGN__(32) __m256d dt[MAX_COLS];
			   const __m256d _1_2 = _mm256_set1_pd(0.5);
			   __m256d integral,delta,h_used;
			   int32_t n_subints;
			   n_cols = max(1,min(MAX_COLS,n_cols));
			   while(--n >= 0) {
                                 h_used = h;
				 n_subints = 1;
				 #pragma loop_count min(1),max(6),avg(3)
				 for(int32_t j = 0; j < n_cols; ++j) {
                                     integral = rk4_step_ymm4r8(f,y0,x0,h_used,n_subints);
				     for(int32_t k = 0; k < j; ++k) {
                                         delta = _mm256_sub_pd(integral,dt[k]);
					 dt[k] = integral;
					 integral = _mm256_fmadd_pd(richardson[k],delta,integral);
				     }
				     dt[j] = integral;
				     h_used = _mm256_mul_pd(h_used,_1_2);
				     n_subints += n_subints;
				 }
				 y0 = integral;
				 x0 = _mm256_add_pd(x0,h);
			   }
			   return (y0);
		    }


		     
			__m256
			rk4_richardson_ymm8r44(__m256(*f)(__m256,
			                                  __m256),
						 __m256 y0,
						 __m256 x0,
						 const __m256 h,
						 const int32_t n,
						 int32_t n_cols) {

			   __ATTR_ALIGN__(32) const __m256 richardson[] = { _mm256_set1_ps(1.0f/15.0f),
			                                                    _mm256_set1_ps(1.0f/31.0f),
								            _mm256_set1_ps(1.0f/63.0f),
								            _mm256_set1_ps(1.0f/127.0f),
								            _mm256_set1_ps(1.0f/255.0f),
									    _mm256_set1_ps(1.0f/511.0f)};
			   constexpr int32_t MAX_COLS = 7;
			   __ATTR_ALIGN__(32) __m256 dt[MAX_COLS];
			   const __m256 _1_2 = _mm256_set1_ps(0.5f);
			   __m256 integral,delta,h_used;
			   int32_t n_subints;
			   n_cols = max(1,min(MAX_COLS,n_cols));
			   while(--n >= 0) {
                                 h_used = h;
				 n_subints = 1;
				 #pragma loop_count min(1),max(6),avg(3)
				 for(int32_t j = 0; j < n_cols; ++j) {
                                     integral = rk4_step_ymm8r44(f,y0,x0,h_used,n_subints);
				     for(int32_t k = 0; k < j; ++k) {
                                         delta = _mm256_sub_ps(integral,dt[k]);
					 dt[k] = integral;
					 integral = _mm256_fmadd_ps(richardson[k],delta,integral);
				     }
				     dt[j] = integral;
				     h_used = _mm256_mul_ps(h_used,_1_2);
				     n_subints += n_subints;
				 }
				 y0 = integral;
				 x0 = _mm256_add_ps(x0,h);
			   }
			   return (y0);
		    }

