

#include "GMS_setv_avx_unrolled16x.h"



          void
          ssetv_u_ymm8r4_unroll16x(const int32_t n,
                                   const float alpha,
			           float * __restrict x,
			           const int32_t incx) {

                  if(__builtin_expect(0==n,0)) { return;}
                         __m256 alphav;
			 int32_t i;
			 
		   if(__builtin_expect(1==incx,1)) {
			 
                       alphav = _mm256_broadcast_ss(alpha);
		      for(i = 0; (i+127) < n; i += 128) {
			    
                                 _mm256_storeu_ps(&x[i+0],alphav);
				 _mm256_storeu_ps(&x[i+8],alphav);
				 _mm256_storeu_ps(&x[i+16],alphav);
				 _mm256_storeu_ps(&x[i+24],alphav);
				 _mm256_storeu_ps(&x[i+32],alphav);
				 _mm256_storeu_ps(&x[i+40],alphav);
				 _mm256_storeu_ps(&x[i+48],alphav);
				 _mm256_storeu_ps(&x[i+56],alphav);
				 _mm256_storeu_ps(&x[i+64],alphav);
				 _mm256_storeu_ps(&x[i+72],alphav);
				 _mm256_storeu_ps(&x[i+80],alphav);
				 _mm256_storeu_ps(&x[i+88],alphav);
				 _mm256_storeu_ps(&x[i+96],alphav);
				 _mm256_storeu_ps(&x[i+104],alphav);
				 _mm256_storeu_ps(&x[i+112],alphav);
				 _mm256_storeu_ps(&x[i+120],alphav);
			    }

			    for(; (i+79) < n; i += 80) {

			         _mm256_storeu_ps(&x[i+0],alphav);
				 _mm256_storeu_ps(&x[i+8],alphav);
				 _mm256_storeu_ps(&x[i+16],alphav);
				 _mm256_storeu_ps(&x[i+24],alphav);
				 _mm256_storeu_ps(&x[i+32],alphav);
				 _mm256_storeu_ps(&x[i+40],alphav);
				 _mm256_storeu_ps(&x[i+48],alphav);
				 _mm256_storeu_ps(&x[i+56],alphav);
				 _mm256_storeu_ps(&x[i+64],alphav);
				 _mm256_storeu_ps(&x[i+72],alphav);
			    }

			    for(; (i+63) < n; i += 64) {
 
                                 _mm256_storeu_ps(&x[i+0],alphav);
				 _mm256_storeu_ps(&x[i+8],alphav);
				 _mm256_storeu_ps(&x[i+16],alphav);
				 _mm256_storeu_ps(&x[i+24],alphav);
				 _mm256_storeu_ps(&x[i+32],alphav);
				 _mm256_storeu_ps(&x[i+40],alphav);
				 _mm256_storeu_ps(&x[i+48],alphav);
				 _mm256_storeu_ps(&x[i+56],alphav);
			    }

			    for(; (i+31) < n; i += 32) {

                                 _mm256_storeu_ps(&x[i+0],alphav);
				 _mm256_storeu_ps(&x[i+8],alphav);
				 _mm256_storeu_ps(&x[i+16],alphav);
				 _mm256_storeu_ps(&x[i+24],alphav);
			    }

			    for(; (i+15) < n; i += 16) {

			         _mm256_storeu_ps(&x[i+0],alphav);
				 _mm256_storeu_ps(&x[i+8],alphav);
			    }

			    for(; i (i+7) < n; i += 8) {

                                   _mm256_storeu_ps(&x[i+0],alphav);
			    }

			    for(; (i+0) < n; i += 1) {

			          x[i] = alpha;
			    }
		      }
		      else {

                              for(i = 0; i != n; ++i) {

			           *x = alpha;
				    x += incx;
			      }
		      }
	  }



          
          void
          ssetv_a_ymm8r4_unroll16x(const int32_t n,
                                   const float alpha,
			           float * __restrict __ATTR_ALIGN__(32) x,
			           const int32_t incx) {


                       if(__builtin_expect(0==n,0)) { return;}
                         __m256 alphav;
			 int32_t i;
			 
			 if(__builtin_expect(1==incx,1)) {
			 
                            alphav = _mm256_broadcast_ss(alpha);
#if defined(__INTEL_COMPILER) || defined(__ICC)
                             __assume_aligned(x,32);
#pragma code_align(32)
#elif defined(__GNUC__) && (!defined(__INTEL_COMPILER) || !defined(__ICC))
                             x = (float*)__builtin_assume_aligned(x,32);
#endif			    
			    for(i = 0; (i+127) < n; i += 128) {
			    
                                 _mm256_store_ps(&x[i+0],alphav);
				 _mm256_store_ps(&x[i+8],alphav);
				 _mm256_store_ps(&x[i+16],alphav);
				 _mm256_store_ps(&x[i+24],alphav);
				 _mm256_store_ps(&x[i+32],alphav);
				 _mm256_store_ps(&x[i+40],alphav);
				 _mm256_store_ps(&x[i+48],alphav);
				 _mm256_store_ps(&x[i+56],alphav);
				 _mm256_store_ps(&x[i+64],alphav);
				 _mm256_store_ps(&x[i+72],alphav);
				 _mm256_store_ps(&x[i+80],alphav);
				 _mm256_store_ps(&x[i+88],alphav);
				 _mm256_store_ps(&x[i+96],alphav);
				 _mm256_store_ps(&x[i+104],alphav);
				 _mm256_store_ps(&x[i+112],alphav);
				 _mm256_store_ps(&x[i+120],alphav);
			    }

			    for(; (i+79) < n; i += 80) {

			         _mm256_store_ps(&x[i+0],alphav);
				 _mm256_store_ps(&x[i+8],alphav);
				 _mm256_store_ps(&x[i+16],alphav);
				 _mm256_store_ps(&x[i+24],alphav);
				 _mm256_store_ps(&x[i+32],alphav);
				 _mm256_store_ps(&x[i+40],alphav);
				 _mm256_store_ps(&x[i+48],alphav);
				 _mm256_store_ps(&x[i+56],alphav);
				 _mm256_store_ps(&x[i+64],alphav);
				 _mm256_store_ps(&x[i+72],alphav);
			    }

			    for(; (i+63) < n; i += 64) {
 
                                 _mm256_store_ps(&x[i+0],alphav);
				 _mm256_store_ps(&x[i+8],alphav);
				 _mm256_store_ps(&x[i+16],alphav);
				 _mm256_store_ps(&x[i+24],alphav);
				 _mm256_store_ps(&x[i+32],alphav);
				 _mm256_store_ps(&x[i+40],alphav);
				 _mm256_store_ps(&x[i+48],alphav);
				 _mm256_store_ps(&x[i+56],alphav);
			    }

			    for(; (i+31) < n; i += 32) {

                                 _mm256_store_ps(&x[i+0],alphav);
				 _mm256_store_ps(&x[i+8],alphav);
				 _mm256_store_ps(&x[i+16],alphav);
				 _mm256_store_ps(&x[i+24],alphav);
			    }

			    for(; (i+15) < n; i += 16) {

			         _mm256_store_ps(&x[i+0],alphav);
				 _mm256_store_ps(&x[i+8],alphav);
			    }

			    for(; i (i+7) < n; i += 8) {

                                   _mm256_store_ps(&x[i+0],alphav);
			    }

			    for(; (i+0) < n; i += 1) {

			          x[i] = alpha;
			    }
		      }
		      else {

                              for(i = 0; i != n; ++i) {

			           *x = alpha;
				    x += incx;
			      }
		      }
	  }



           void
	   dsetv_u_ymm4r8_unroll16x(const int32_t n,
		                    const double alpha,
				    double * __restrict x,
				    const int32_t incx) {

                 if(__builtin_expect(0==n,0)) { return;}
                         __m256d alphav;
			 int32_t i;
			 
			 if(__builtin_expect(1==incx,1)) {

                             alphav = _mm256_broadcast_sd(alpha);

			     for(i = 0; (i+63) < n; i += 64) {

                                 _mm256_storeu_pd(&x[i+0],alphav);
				 _mm256_storeu_pd(&x[i+4],alphav);
				 _mm256_storeu_pd(&x[i+8],alphav);
				 _mm256_storeu_pd(&x[i+12],alphav);
				 _mm256_storeu_pd(&x[i+16],alphav);
				 _mm256_storeu_pd(&x[i+20],alphav);
				 _mm256_storeu_pd(&x[i+24],alphav);
				 _mm256_storeu_pd(&x[i+28],alphav);
				 _mm256_storeu_pd(&x[i+32],alphav);
				 _mm256_storeu_pd(&x[i+36],alphav);
				 _mm256_storeu_pd(&x[i+40],alphav);
				 _mm256_storeu_pd(&x[i+44],alphav);
				 _mm256_storeu_pd(&x[i+48],alphav);
				 _mm256_storeu_pd(&x[i+52],alphav);
				 _mm256_storeu_pd(&x[i+56],alphav);
				 _mm256_storeu_pd(&x[i+60],alphav);
			     }

			     for(; (i+39) < n; i += 40) {

			          _mm256_storeu_pd(&x[i+0],alphav);
				  _mm256_storeu_pd(&x[i+4],alphav);
				  _mm256_storeu_pd(&x[i+8],alphav);
				  _mm256_storeu_pd(&x[i+12],alphav);
				  _mm256_storeu_pd(&x[i+16],alphav);
				  _mm256_storeu_pd(&x[i+20],alphav);
				  _mm256_storeu_pd(&x[i+24],alphav);
				  _mm256_storeu_pd(&x[i+28],alphav);
				  _mm256_storeu_pd(&x[i+32],alphav);
				  _mm256_storeu_pd(&x[i+36],alphav);
			     }

			     for(; (i+31) < n; i += 32) {

                                    _mm256_storeu_pd(&x[i+0],alphav);
				    _mm256_storeu_pd(&x[i+4],alphav);
				    _mm256_storeu_pd(&x[i+8],alphav);
				    _mm256_storeu_pd(&x[i+12],alphav);
				    _mm256_storeu_pd(&x[i+16],alphav);
				    _mm256_storeu_pd(&x[i+20],alphav);
				    _mm256_storeu_pd(&x[i+24],alphav);
				    _mm256_storeu_pd(&x[i+28],alphav);
			     }

			     for(; (i+15) < n; i += 16) {

                                    _mm256_storeu_pd(&x[i+0],alphav);
				    _mm256_storeu_pd(&x[i+4],alphav);
				    _mm256_storeu_pd(&x[i+8],alphav);
				    _mm256_storeu_pd(&x[i+12],alphav);
			     }

			     for(; (i+7) < n; i += 8) {

                                   _mm256_storeu_pd(&x[i+0],alphav);
				   _mm256_storeu_pd(&x[i+4],alphav);
			     }

			     for(; (i+3) < n; i += 4) {

			           _mm256_storeu_pd(&x[i+0],alphav);
			     }

			     for(; (i+0) < n; i += 1) {

			           x[i] = alpha;
			     }
			     
			 }
			 else {

			       for(i = 0; i != n; ++i) {
                                    *x = alpha;
				     x += incx;
			       }
			 }
	   }



          void
	  dsetv_a_ymm4r8_unroll16x(const int32_t n,
		                   const double alpha,
				   double * __restrict __ATTR_ALIGN__(32) x,
				   const int32_t incx) {

                   if(__builtin_expect(0==n,0)) { return;}
                         __m256d alphav;
			 int32_t i;
			 
			 if(__builtin_expect(1==incx,1)) {

                             alphav = _mm256_broadcast_sd(alpha);
#if defined(__INTEL_COMPILER) || defined(__ICC)
                             __assume_aligned(x,32);
#pragma code_align(32)
#elif defined(__GNUC__) && (!defined(__INTEL_COMPILER) || !defined(__ICC))
                             x = (double*)__builtin_assume_aligned(x,32);
#endif
			     for(i = 0; (i+63) < n; i += 64) {

                                 _mm256_store_pd(&x[i+0],alphav);
				 _mm256_store_pd(&x[i+4],alphav);
				 _mm256_store_pd(&x[i+8],alphav);
				 _mm256_store_pd(&x[i+12],alphav);
				 _mm256_store_pd(&x[i+16],alphav);
				 _mm256_store_pd(&x[i+20],alphav);
				 _mm256_store_pd(&x[i+24],alphav);
				 _mm256_store_pd(&x[i+28],alphav);
				 _mm256_store_pd(&x[i+32],alphav);
				 _mm256_store_pd(&x[i+36],alphav);
				 _mm256_store_pd(&x[i+40],alphav);
				 _mm256_store_pd(&x[i+44],alphav);
				 _mm256_store_pd(&x[i+48],alphav);
				 _mm256_store_pd(&x[i+52],alphav);
				 _mm256_store_pd(&x[i+56],alphav);
				 _mm256_store_pd(&x[i+60],alphav);
			     }

			     for(; (i+39) < n; i += 40) {

			          _mm256_store_pd(&x[i+0],alphav);
				  _mm256_store_pd(&x[i+4],alphav);
				  _mm256_store_pd(&x[i+8],alphav);
				  _mm256_store_pd(&x[i+12],alphav);
				  _mm256_store_pd(&x[i+16],alphav);
				  _mm256_store_pd(&x[i+20],alphav);
				  _mm256_store_pd(&x[i+24],alphav);
				  _mm256_store_pd(&x[i+28],alphav);
				  _mm256_store_pd(&x[i+32],alphav);
				  _mm256_store_pd(&x[i+36],alphav);
			     }

			     for(; (i+31) < n; i += 32) {

                                    _mm256_store_pd(&x[i+0],alphav);
				    _mm256_store_pd(&x[i+4],alphav);
				    _mm256_store_pd(&x[i+8],alphav);
				    _mm256_store_pd(&x[i+12],alphav);
				    _mm256_store_pd(&x[i+16],alphav);
				    _mm256_store_pd(&x[i+20],alphav);
				    _mm256_store_pd(&x[i+24],alphav);
				    _mm256_store_pd(&x[i+28],alphav);
			     }

			     for(; (i+15) < n; i += 16) {

                                    _mm256_store_pd(&x[i+0],alphav);
				    _mm256_store_pd(&x[i+4],alphav);
				    _mm256_store_pd(&x[i+8],alphav);
				    _mm256_store_pd(&x[i+12],alphav);
			     }

			     for(; (i+7) < n; i += 8) {

                                   _mm256_store_pd(&x[i+0],alphav);
				   _mm256_store_pd(&x[i+4],alphav);
			     }

			     for(; (i+3) < n; i += 4) {

			           _mm256_store_pd(&x[i+0],alphav);
			     }

			     for(; (i+0) < n; i += 1) {

			           x[i] = alpha;
			     }
			     
			 }
			 else {

			       for(i = 0; i != n; ++i) {
                                    *x = alpha;
				     x += incx;
			       }
			 }
	  }
