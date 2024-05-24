

#ifndef __GMS_SIMD_UTILS_H__
#define __GMS_SIMD_UTILS_H__ 040120220918


namespace file_info {

const unsigned int GMS_SIMD_UTILS_MAJOR = 1U;
const unsigned int GMS_SIMD_UTILS_MINOR = 0U;
const unsigned int GMS_SIMD_UTILS_MICRO = 1U;
const unsigned int GMS_SIMD_UTILS_FULLVER =
       1000U*GMS_SIMD_UTILS_MAJOR+
       100U*GMS_SIMD_UTILS_MINOR +
       10U*GMS_SIMD_UTILS_MICRO;
const char * const GMS_SIMD_UTILS_CREATION_DATE = "04-01-2022 09:18 AM +00200 (TUE 04 JAN 2022 GMT+2)";
const char * const GMS_SIMD_UTILS_BUILD_DATE    = __DATE__ ":" __TIME__;
const char * const GMS_SIMD_UTILS_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com";
const char * const GMS_SIMD_UTILS_DESCRIPTION   = "Various SIMD utility functions.";


}



#include <immintrin.h>






         


                       

                          static const __m128  _0PS     = _mm_set1_ps(0.0F);
			  static const __m256d _0PD     = _mm256_setzero_pd();
			  static const __m128   NZ128SP = _mm_set1_ps(-0.0F);
			  static const __m256   NZ256SP = _mm256_set1_ps(-0.0F);
			  static const __m256d  NZ256DP = _mm256_set1_ps(-0.0);
			  static const __m512   NZ512SP = _mm512_set1_ps(-0.0F);
			  static const __m512d  NZ512DP = _mm512_set1_pd(-0.0F);
		     


		      // Load only 3 elements (lower) of XMM register.
		      // Single-precision
	             __attribute__((regcall))
                     __attribute__((always_inline))
                     __attribute__((hot))
		     __attribute__((aligned(32)))
		     static inline
		      __m128
		      xmm4r4_load_3u_avx512(const float * __restrict v) {
                            const __mmask8 k = 0x7;
                            return (_mm_mask_loadu_ps(_0PS,k,v));
		    }


                      __attribute__((regcall))
                     __attribute__((always_inline))
                     __attribute__((hot))
		     __attribute__((aligned(32)))
                     static inline
		      __mmask8 isinf_zmm8r8(__m512d x) {

                         union {
                           __m512i u;
			   __m512d f;
			 } ieee754;
			 const __m512i c0 = _mm512_set1_epi64(0x7fffffff);
			 const __m512i c1 = _mm512_set1_epi64(0x7ff00000);
			 const __m512i _0 = _mm512_setzero_si512();
			 __m512i t0,t1;
			 __mmask8 b0,b1;
			 ieee754.f = x;
			 t0 = _mm512_and_epi64(_mm512_srli_epi64(ieee754.u,32),c0);
			 b0 = _mm512_cmp_epi64_mask(t0,c1,_MM_CMPINT_EQ);
			 b1 = _mm512_cmp_epi64_mask(ieee754.u,_0,_MM_CMPINT_EQ);
			 return (b0 && b1);
		    }


		        __attribute__((regcall))
                     __attribute__((always_inline))
                     __attribute__((hot))
		     __attribute__((aligned(32)))
		      static inline
		      __mmask8 isnan_zmm8r8(__m512d x) {

		        union {
                           __m512i u;
			   __m512d f;
			 } ieee754;
			 const __m512i c0 = _mm512_set1_epi64(0x7fffffff);
			 const __m512i c1 = _mm512_set1_epi64(0x7ff00000);
			 const __m512i _0 = _mm512_setzero_si512();
			 __m512i t0,t1;
			 __mmask8 b0,b1;
			 ieee754.f = x;
			 t0 = _mm512_and_epi64(_mm512_srli_epi64(ieee754.u,32),c0);
			 b0 = _mm512_cmp_epi64_mask(ieee754.u,_0,_MM_CMPINT_NE);
			 t1 = _mm512_movm_epi64(b0);
			 b1 = _mm512_cmp_epi64_mask(t1,c1,_MM_CMPINT_LT);
			 b0 = _mm512_movm_epi64(t0);
			 return (b0+b1);

		      }


		     __attribute__((regcall))
                     __attribute__((always_inline))
                     __attribute__((hot))
		     __attribute__((aligned(32)))
		      static inline
		      __m128
		      xmm4r4_load_3a_avx512(const float * __restrict __ATTR_ALIGN__(16) v) {
                            const __mmask8 k = 0x7;
                            return (_mm_mask_loada_ps(_0,k,v));
		    }


		     __attribute__((regcall))
                     __attribute__((always_inline))
                     __attribute__((hot))
		     __attribute__((aligned(32)))
		      static inline
		      __m128
		      xmm4r4_load_3u_avx(const float * __restrict v) {

                          const __m128i k = _mm_set_epi32(0,-1,-1,-1);
			  return (_mm_maskload_ps(v,(__m128i)k);
		    }


		     __attribute__((regcall))
                     __attribute__((always_inline))
                     __attribute__((hot))
		     __attribute__((aligned(32)))
		      static inline
		      __m128
		      xmm4r4_load_3a_avx(const float * __restrict __ATTR_ALIGN__(16) v) {

                          const __m128i k = _mm_set_epi32(0,-1,-1,-1);
			  return (_mm_maskload_ps(v,(__m128i)k);
		    }

                    // Load only 3 elements (lower) of XMM register.
		    // Double-precision
		     __attribute__((regcall))
                     __attribute__((always_inline))
                     __attribute__((hot))
		     __attribute__((aligned(32)))
		      static inline
		      __m256d
		      ymm8r4_load_3u_avx512(const double * __restrict v) {

		          const __mmask8 k = 0x7;
			  return (_mm256_mask_loadu_pd(_0PD,k,v));
		    }


		     __attribute__((regcall))
                     __attribute__((always_inline))
                     __attribute__((hot))
		     __attribute__((aligned(32)))
		      static inline
		      __m256d
		      ymm8r4_load_3a_avx512(const double * __restrict __ATTR_ALIGN__(32) v) {

		          const __mmask8 k = 0x7;
			  return (_mm256_mask_load_pd(_0PD,k,v));
		    }


		        __attribute__((regcall))
                     __attribute__((always_inline))
                     __attribute__((hot))
		     __attribute__((aligned(32)))
		      static inline
		      __m256d
		      ymm8r4_load_3u_avx(const double * __restrict v) {

		          const __m256i k = _mm256_set_epi32(0,-1,-1,-1);
			  return (_mm256_maskload_pd(v,k));
		    }


		        __attribute__((regcall))
                     __attribute__((always_inline))
                     __attribute__((hot))
		     __attribute__((aligned(32)))
		      static inline
		      __m256d
		      ymm8r4_load_3a_avx(const double * __restrict __ATTR_ALIGN__(32) v) {

		          const __m256i k = _mm256_set_epi32(0,-1,-1,-1);
			  return (_mm256_maskload_pd(v,k));
		    }
		    
		  // Store only 3 elements (lower) of XMM register.

		         __attribute__((regcall))
                     __attribute__((always_inline))
                     __attribute__((hot))
		     __attribute__((aligned(32)))
		      static inline   
                      void
		      xmm4r4_store_3u_avx512(float * __restrict v,
		                             const __m128 x) {

                          const __mmask8 k = 0x7;
			  return (_mm_mask_storeu_ps(v,k,x));
		   }


		         __attribute__((regcall))
                     __attribute__((always_inline))
                     __attribute__((hot))
		     __attribute__((aligned(32)))
		      static inline   
                      void
		      xmm4r4_store_3a_avx512(float * __restrict  __ATTR_ALIGN__(16) v,
		                             const __m128 x) {

                          const __mmask8 k = 0x7;
			  return (_mm_mask_store_ps(v,k,x));
		   }


		         __attribute__((regcall))
                     __attribute__((always_inline))
                     __attribute__((hot))
		     __attribute__((aligned(32)))
		      static inline   
                      void
		      xmm4r4_store_3u_avx(float * __restrict v,
		                          const __m128 x) {

                          const __m128i k = _mm_set_epi32(0,-1,-1,-1);
			  return (_mm_maskstore_ps(v,(__m128i)k,x));
		   }


		        __attribute__((regcall))
                     __attribute__((always_inline))
                     __attribute__((hot))
		     __attribute__((aligned(32)))
		      static inline   
                      void
		      xmm4r4_store_3a_avx(float * __restrict __ATTR_ALIGN__(16) v,
		                          const __m128 x) {

                          const __m128i k = _mm_set_epi32(0,-1,-1,-1);
			  return (_mm_maskstore_pd(v,(__m128i)k,x));
		   }


		        __attribute__((regcall))
                     __attribute__((always_inline))
                     __attribute__((hot))
		     __attribute__((aligned(32)))
		      static inline   
                      void
		      ymm4r8_store_3u_avx512(double * __restrict v,
		                          const __m256d x) {

                          const __mmask8 k = 0x7;
			  return (_mm256_mask_storeu_pd(v,k,x));
		   }


		        __attribute__((regcall))
                     __attribute__((always_inline))
                     __attribute__((hot))
		     __attribute__((aligned(32)))
		      static inline   
                      void
		      ymm8r4_store_3a_avx512(double * __restrict __ATTR_ALIGN__(16) v,
		                             const __m256d x) {

                          const __mmask8 k = 0x7;
			  return (_mm256_mask_store_pd(v,k,x));
		   }


		   // The whole register negated
                   
		        __attribute__((regcall))
                     __attribute__((always_inline))
                     __attribute__((hot))
		     __attribute__((aligned(32)))
		      static inline 
                      __m256
		      ymm8r4_negate(const __m256 v) {

		           return (_mm256_xor_ps(v,NZ256PS));
		    }


		       __attribute__((regcall))
                     __attribute__((always_inline))
                     __attribute__((hot))
		     __attribute__((aligned(32)))
		      static inline 
                      __m256d
		      ymm4r8_negate(const __m256d v) {

		           return (_mm256_xor_pd(v,NZ256PD));
		    }


		         __attribute__((regcall))
                     __attribute__((always_inline))
                     __attribute__((hot))
		     __attribute__((aligned(32)))
		      static inline 
                      __m512
		      zmm16r4_negate(const __m512 v) {

                          return (_mm512_xor_ps(v,NZ512SP));
		    }


		       __attribute__((regcall))
                     __attribute__((always_inline))
                     __attribute__((hot))
		     __attribute__((aligned(32)))
		      static inline 
                      __m512d
		      zmm8r8_negate(const __m512d v) {

                          return (_mm512_xor_pd(v,NZ512DP));
		    }


		    // Dot product
		       __attribute__((regcall))
                     __attribute__((always_inline))
                     __attribute__((hot))
		     __attribute__((aligned(32)))
		      static inline 
                      __m128d
		      ymm8r4_dot(const __m256d x,
		                 const __m256d y) {

			  const __m256d t   = _mm256_mul_pd(x,y);
                          const __m256d slo = _mm256_hadd_pd(t, t);
                          const __m128d shi = _mm256_extractf128_pd(slo, 0x1);
			  return _mm_add_pd(shi, _mm256_castpd256_pd128(slo));
		   }
		    
                   // Getting a single value form SIMD register

		        __attribute__((regcall))
                     __attribute__((always_inline))
                     __attribute__((hot))
		     __attribute__((aligned(32)))
		      static inline 
		      float
		      ymm8r4_0_elem(__m256 a) {
                           return _mm_cvtss_f32(_mm256_castps256_ps128(a));
                      }
		      

		       __attribute__((regcall))
                     __attribute__((always_inline))
                     __attribute__((hot))
		     __attribute__((aligned(32)))
		      static inline 
                      float
		      ymm8r4_1_elem(__m256 a) {
                           __m128 t = _mm256_castps256_ps128(a);
                           return _mm_cvtss_f32(_mm_shuffle_ps(t,t,_MM_SHUFFLE(0,0,0,1)));
                      }

		       __attribute__((regcall))
                     __attribute__((always_inline))
                     __attribute__((hot))
		     __attribute__((aligned(32)))
		      static inline 
                     float
		     ymm8r4_2_elem(__m256 a) {
                           __m128 t = _mm256_castps256_ps128(a);
                           return _mm_cvtss_f32(_mm_shuffle_ps(t,t,_MM_SHUFFLE(0,0,0,2)));
                     }

		        __attribute__((regcall))
                     __attribute__((always_inline))
                     __attribute__((hot))
		     __attribute__((aligned(32)))
		      static inline 
                      float
		      ymm8r4_3_elem(__m256 a) {
                           __m128 t = _mm256_castps256_ps128(a);
                           return _mm_cvtss_f32(_mm_shuffle_ps(t,t,_MM_SHUFFLE(0,0,0,3)));
                     }

		       __attribute__((regcall))
                     __attribute__((always_inline))
                     __attribute__((hot))
		     __attribute__((aligned(32)))
		      static inline 
                      float
		      ymm8r4_4_elem(__m256 a) {
                         return _mm_cvtss_f32(_mm256_extractf128_ps(a,0x1));
                      }

		      __attribute__((regcall))
                     __attribute__((always_inline))
                     __attribute__((hot))
		     __attribute__((aligned(32)))
		      static inline 
                      float
		      ymm8r4_5_elem(__m256 a) {
                           __m128 t = _mm256_extractf128_ps(a,0x1);
                           return _mm_cvtss_f32(_mm_shuffle_ps(t,t,_MM_SHUFFLE(0,0,0,1)));
                     }
		     
		        __attribute__((regcall))
                     __attribute__((always_inline))
                     __attribute__((hot))
		     __attribute__((aligned(32)))
		      static inline 
                      float
		      ymm8r4_6_elem(__m256 a) {
                            __m128 t = _mm256_extractf128_ps(a,0x1);
                            return _mm_cvtss_f32(_mm_shuffle_ps(t,t,_MM_SHUFFLE(0,0,0,2)));
                      }

		        __attribute__((regcall))
                     __attribute__((always_inline))
                     __attribute__((hot))
		     __attribute__((aligned(32)))
		      static inline 
                      float
		      ymm8r4_7_elem(__m256 a) {
                            __m128 t = _mm256_extractf128_ps(a,0x1);
                            return _mm_cvtss_f32(_mm_shuffle_ps(t,t,_MM_SHUFFLE(0,0,0,3)));
                      }

		        __attribute__((regcall))
                     __attribute__((always_inline))
                     __attribute__((hot))
		     __attribute__((aligned(32)))
		      static inline 
                      double
		      ymm4r8_0_elem(__m256d a) {
                          return _mm_cvtsd_f64(_mm256_castpd256_pd128(a));
                      }

		        __attribute__((regcall))
                     __attribute__((always_inline))
                     __attribute__((hot))
		     __attribute__((aligned(32)))
		      static inline 
                      double
		      ymm4r8_1_elem(__m256d a) {
                            __m128d t = _mm256_castpd256_pd128(a);
                           return _mm_cvtsd_f64(_mm_shuffle_pd(t,t,_MM_SHUFFLE2(0,1)));
                      }

		         __attribute__((regcall))
                     __attribute__((always_inline))
                     __attribute__((hot))
		     __attribute__((aligned(32)))
		      static inline 
                      double
		      ymm4r8_2_elem(__m256d a) {
                            return _mm_cvtsd_f64(_mm256_extractf128_pd(a,0x1));
                      }

		         __attribute__((regcall))
                     __attribute__((always_inline))
                     __attribute__((hot))
		     __attribute__((aligned(32)))
		      static inline 
                      double
		      ymm4r8_3_elem(__m256d a) {
                           __m128d t = _mm256_extractf128_pd(a,0x1);
                           return _mm_cvtsd_f64(_mm_shuffle_pd(t,t,_MM_SHUFFLE2(0,1)));
                     }


		     // Horizontal summation.
                        __attribute__((regcall))
                     __attribute__((always_inline))
                     __attribute__((hot))
		     __attribute__((aligned(32)))
		      static inline 
		      float
		      ymm8r4_horizontal_sum(const __m256 a) {
                          __m256 sum    = _mm256_hadd_ps(a, a);
                          sum           = _mm256_hadd_ps(sum, sum);
                          __m128 r      = _mm_add_ps(_mm256_castps256_ps128(sum),
			                          _mm256_extractf128_ps(sum, 0x1));
                          return _mm_cvtss_f32(r);
                      }

		       __attribute__((regcall))
                     __attribute__((always_inline))
                     __attribute__((hot))
		     __attribute__((aligned(32)))
		      static inline 
                      double
		      ymm4r8_horizontal_sum(const __m256d a) {
                            __m256d sum     = _mm256_hadd_pd(a, a);
                            const __m128d r = _mm_add_sd(_mm256_castpd256_pd128(sum),
			                                 _mm256_extractf128_pd(sum, 0x1));
                             return _mm_cvtsd_f64(r);
                      }

		        __attribute__((regcall))
                     __attribute__((always_inline))
                     __attribute__((hot))
		     __attribute__((aligned(32)))
		      static inline 
                      double
		      ymm4r8_horizontal_prod(const __m256d a) {
                           const __m256d sum = _mm256_mul_pd(a, _mm256_shuffle_pd(a,a,0x5));
                           const __m128d sh = _mm256_extractf128_pd(sum, 0x1);
                           __m128d r        = _mm_mul_sd(sh, _mm256_castpd256_pd128(sum));
                           return _mm_cvtsd_f64(r);
                      }

		      
                        __attribute__((regcall))
                     __attribute__((always_inline))
                     __attribute__((hot))
		     __attribute__((aligned(32)))
		      static inline 
		      __m256
		      ymm8r4_abs(const __m256 x) {
                          const __m256 mask = _mm256_set1_ps(-0.0f); 
                          return _mm256_andnot_ps(mask, x);
                     }
		     

		       __attribute__((regcall))
                     __attribute__((always_inline))
                     __attribute__((hot))
		     __attribute__((aligned(32)))
		      static inline 
                     __m256d
		     ymm4r8_abs(const __m256d x) {
                           const __m256d mask = _mm256_set1_pd(-0.0); 
                           return _mm256_andnot_pd(mask, x); 
                     }

                              
                                 __attribute__((regcall))
                               __attribute__((always_inline))
			       __attribute__((aligned(32)))
			       static inline
			       __m128
			       xmm4r4_abs_xmm4r4(const __m128 v) {
                        	     const __m128 mask = _mm_set1_ps(0x7FFFFFFF);
	                             return (_mm_and_ps(x,mask));
                               }

                               __attribute__((regcall))
			       __attribute__((always_inline))
			       __attribute__((aligned(32)))
			       static inline
			       __m128d
			       xmm2r8_abs_xmm2r8(const __m128d v) {
                        	     const __m128d mask = _mm_set1_pd(0x7FFFFFFFFFFFFFFF);
	                             return (_mm_and_pd(x,mask));
                               }

                                  __attribute__((regcall))
			       __attribute__((always_inline))
			       __attribute__((aligned(32)))
			       static inline
			       __m128
			       xmm4r4_sign_xmm4r4(const __m128 va,
			                          const __m128 vb) {
                                   register __m128 vret = v8_0;
				   register __m128 t0   = xmm4r4_abs_xmm4r4(va);
				   register __m128 mask = _mm_cmp_ps(vb,v8_0,_CMP_GE_OQ);
				   vret = _mm_blendv_ps(t0,_mm_sub_ps(v8_0,t0),mask);
                                   return (vret);
			       }

                                   __attribute__((regcall))
			       __attribute__((always_inline))
			       __attribute__((aligned(32)))
			       static inline
			       __m128d
			       xmm2r8_sign_xmm2r8(const __m128d va,
			                          const __m128d vb) {
                                   register __m128d vret = v4_0;
				   const register __m128d t0   = xmm2r8_abs_xmm2r8(va);
				   const register __m128d mask = _mm_cmp_pd(vb,v4_0,_CMP_GE_OQ);
				   vret = _mm_blendv_pd(t0,_mm_sub_pd(v4_0,t0),mask);
                                   return (vret);
			       }

                                
                                   __attribute__((regcall))
                               __attribute__((always_inline))
			       __attribute__((aligned(32)))
			       static inline
			       __m128d
			       xmm2r8_truncate(const __m128d v) {
                                  
                                    return (_mm_cvtepi64_pd(_mm_cvttpd_epi64(v)));
			       }

			    
                                  __attribute__((regcall))
			       __attribute__((always_inline))
			       __attribute__((aligned(32)))
			       static inline
			       __m128
			       xmm4r4_truncate(const __m128 v) {
                                  
                                    return (_mm_cvtepi32_ps(_mm_cvttps_epi32(v)));
			       }


                                  __attribute__((regcall))
			       __attribute__((always_inline))
			       __attribute__((aligned(32)))
			       static inline
			       __m128 fmod_xmm4r4(const __m128 a,
			                          const __m128 b) {
                                   const register __m128 v = xmm4r4_truncate(_mm_div_ps(a,b));
                                   return (_mm_sub_ps(a,_mm_mul_ps(v,b)));  
			      }
			      
			      
			         __attribute__((regcall))
			       __attribute__((always_inline))
			       __attribute__((aligned(32)))
			       static inline
			       __m128d fmod_xmm2r8(const __m128d a,
			                          const __m128d b) {
                                   const register __m128d v = xmm2r8_truncate(_mm_div_pd(a,b));
                                   return (_mm_sub_pd(a,_mm_mul_pd(v,b)));  
			      }
                             
			          __attribute__((regcall))
                               __attribute__((always_inline))
			       __attribute__((aligned(32)))
			      static inline
		              __m512 fmod_zmm16r4(const __m512 a,
		                                  const __m512 b) {

                                     __m512 v = _mm512_sub_ps(a,_mm512_mul_ps(
			             _mm512_div_round_ps(a,b,_MM_FROUND_TO_ZERO|_MM_FROUND_NO_EXEC),b));
			             return (v);
			  
		               }

		                
		                  __attribute__((regcall))
                             __attribute__((always_inline))
			       __attribute__((aligned(32)))
		              static inline
		              __m512d fmod_zmm8r8(const __m512d a,
		                                  const __m512d b) {

                                    __m512d v = _mm512_sub_pd(a,_mm512_mul_pd(
			             _mm512_div_round_pd(a,b,_MM_FROUND_TO_ZERO|_MM_FROUND_NO_EXEC),b));
			       return (v);
			  
		            }


			    

                                __attribute__((regcall))
			    __attribute__((always_inline))
			    __attribute__((aligned(32)))
		           static inline
		           __m128d norm2_xmm2r8(const __m128d y,
		                                const __m128d z,
					        const __m128d w) {

                                 const __m128d t0 = _mm_mul_pd(y,y);
			         const __m128d t1 = _mm_mul_pd(z,z);
			         const __m128d t2 = _mm_mul_pd(w,w);
			         const __m128d v  = _mm_add_pd(t0,_mm_add_pd(t1,t2));
			         return (_mm_sqrt_pd(v));
			    
		           }


                              __attribute__((regcall))
		          __attribute__((always_inline))
			  __attribute__((aligned(32)))
		          static inline
		          __m128 norm2_xmm4r4(  const __m128 y,
		                                const __m128 z,
					        const __m128 w) {

                                const __m128 t0 = _mm_mul_ps(y,y);
			        const __m128 t1 = _mm_mul_ps(z,z);
			        const __m128 t2 = _mm_mul_ps(w,w);
			        const __m128 v  = _mm_add_ps(t0,_mm_add_ps(t1,t2));
			        return (_mm_sqrt_ps(v));
			    
		          }

                          
                             __attribute__((regcall))
			 __attribute__((always_inline))
			 __attribute__((aligned(32)))
		         static inline
		         __m128 clip_xmm4r4( const __m128 x,
		                             const __m128 lo,
					     const __m128 hi) {

                               return (_mm_max_ps(lo,_mm_min_ps(x,hi)));
		          }


                             __attribute__((regcall))
		         __attribute__((always_inline))
			 __attribute__((aligned(32)))
		         static inline
		          __m128d clip_xmm2r8(const __m128d x,
		                              const __m128d lo,
					      const __m128d hi) {

                               return (_mm_max_pd(lo,_mm_min_pd(x,hi)));
		          }


		     
   











#endif /*__GMS_SIMD_UTILS_H__*/
