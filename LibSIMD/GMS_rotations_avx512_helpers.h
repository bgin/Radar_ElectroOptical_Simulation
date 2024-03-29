
#ifndef __GMS_ROTATIONS_AVX512_HELPERS_H__
#define __GMS_ROTATIONS_AVX512_HELPERS_H__



#include <immintrin.h>

#if !defined(GMS_INTERLEAVE_SIMD_OPS_SCHEDULE)
    #define GMS_INTERLEAVE_SIMD_OPS_SCHEDULE 1
#endif


                               const __m512 v16_0      = _mm512_set1_ps(0.0F);
		               const __m512 v16_2      = _mm512_set1_ps(2.0f);
                               const __m512 v16_n1     = _mm512_set1_ps(-1.0f);
			       const __m512 v16_n2     = _mm512_set1_ps(-2.0F);
			       const __m512 v16_1o2    = _mm512_set1_ps(0.5F);
			       const __m512 v16_spi    = _mm512_set1_ps(1.7724538509055160272982F);
			       const __m512 v16_s6pi   = _mm512_set1_ps(1.381976597885341917061F);
			       const __m512 v16_a      = _mm512_set1_ps(1.9257490199582527754939F);
			       const __m512 v16_ap     = _mm512_set1_ps(2.1450293971110256000775F);
			       const __m512 v16_sc     = _mm512_set1_ps(0.8977727869612861128953F);
			       const __m512 v16_beta   = _mm512_set1_ps(0.9628745099791263877469F);
			       const __m512 v16_r1     = _mm512_set1_ps(1.3306700394914687909256F);
			       const __m512 v16_r2     = _mm512_set1_ps(1.4142135623730950488017F);
			       const __m512 v16_pi12   = _mm512_set1_ps(0.2617993877991494365386F);
			       const __m512 v16_prek   = _mm512_set1_ps(1.6434564029725030125017F);
			       const __m512 v16_pi     = _mm512_set1_ps(3.1415926535897932384626F);
			       const __m512 v16_2pi    = _mm512_set1_ps(6.2831853071795864769253F);
			      
			       const __m512d v8_1o2    = _mm512_set1_pd(0.5);
                               const __m512d v8_0      = _mm512_set1_pd(0.0);
		               const __m512d v8_2      = _mm512_set1_pd(2.0);
			       const __m512d v8_n1     = _mm512_set1_pd(-1.0);
			       const __m512d  v8_pi     = _mm512_set1_pd(3.1415926535897932384626);
			       const __m512d  v8_2pi    = _mm512_set1_pd(6.2831853071795864769253);
			       const __m512d v8_spi    = _mm512_set1_pd(1.7724538509055160272982);
			       const __m512d v8_s6pi   = _mm512_set1_pd(1.381976597885341917061);
			       const __m512d v8_a      = _mm512_set1_pd(1.9257490199582527754939);
			       const __m512d v8_ap     = _mm512_set1_pd(2.1450293971110256000775);
			       const __m512d v8_sc     = _mm512_set1_pd(0.8977727869612861128953);
			       const __m512d v8_beta   = _mm512_set1_pd(0.9628745099791263877469);
			       const __m512d v8_r1     = _mm512_set1_pd(1.3306700394914687909256);
			       const __m512d v8_r2     = _mm512_set1_pd(1.4142135623730950488017);
			       const __m512d v8_pi12   = _mm512_set1_pd(0.2617993877991494365386);
			       const __m512d v8f_prek   = _mm512_set1_pd(1.6434564029725030125017);


                               __attribute__((always_inline))
			       __attribute__((aligned(32)))
                               static inline
			       __m512d
			       zmm8r8_sign_zmm8r8(const __m512d va,
				                  const __m512d vb) {
				       
				       register __m512d vret = _0;
				       register __m512d t0   = _mm512_abs_pd(va);
                                       __mmask8 gez = 0x0;
				       gez  = _mm512_cmp_pd_mask(vb,v8_0,_CMP_GE_OQ); // Lat=3refc,Thr=1refc
				       vret = _mm512_mask_blend_pd(gez,t0,_mm512_sub_pd(v8_0,t0)); //Lat=1refc,Thr=0.5refc,Lat=4refc,Thr=1refc
				       return (vret);
				                                       
			       }

			       
                               __attribute__((always_inline))
			       __attribute__((aligned(32)))
			        static inline
				__m512
				zmm16r4_sign_zmm16r4(const __m512 va,
				                         const __m512 vb) {
				       
				       register __m512 vret = _0;
				       register __m512 t0   = _mm512_abs_ps(va);
                                       __mmask8 gez = 0x0;
				       gez  = _mm512_cmp_ps_mask(vb,v16_0,_CMP_GE_OQ); // Lat=3refc,Thr=1refc
				       vret = _mm512_mask_blend_ps(gez,t0,_mm512_sub_ps(v16_0,t0)); //Lat=1refc,Thr=0.5refc,Lat=4refc,Thr=1refc
				       return (vret);
				                                       
			       }

			       
                               __attribute__((always_inline))
			       __attribute__((aligned(32)))
			      static inline
		              __m512 fmod_zmm16r4(const __m512 a,
		                                  const __m512 b) {

                                     __m512 v = _mm512_sub_ps(a,_mm512_mul_ps(
			             _mm512_div_round_ps(a,b,_MM_FROUND_TO_ZERO|_MM_FROUND_NO_EXEC),b));
			             return (v);
			  
		               }

		      
                              __attribute__((always_inline))
			       __attribute__((aligned(32)))
		              static inline
		              __m512d fmod_zmm8r8(const __m512d a,
		                                  const __m512d b) {

                                    __m512d v = _mm512_sub_pd(a,_mm512_mul_pd(
			             _mm512_div_round_pd(a,b,_MM_FROUND_TO_ZERO|_MM_FROUND_NO_EXEC),b));
			       return (v);
			  
		            }


			    __attribute__((always_inline))
			    __attribute__((aligned(32)))
		           static inline
		           __m512d norm2_zmm8r8(const __m512d y,
		                                const __m512d z,
					        const __m512d w) {

                                 const __m512d t0 = _mm512_mul_pd(y,y);
			         const __m512d t1 = _mm512_mul_pd(z,z);
			         const __m512d t2 = _mm512_mul_pd(w,w);
			         const __m512d v  = _mm512_add_pd(t0,_mm512_add_pd(t1,t2));
			         return (_mm512_sqrt_pd(v));
			    
		           }


		          __attribute__((always_inline))
			  __attribute__((aligned(32)))
		          static inline
		          __m512d norm2_zmm16r4(const __m512 y,
		                                const __m512 z,
					        const __m512 w) {

                                const __m512 t0 = _mm512_mul_ps(y,y);
			        const __m512 t1 = _mm512_mul_ps(z,z);
			        const __m512 t2 = _mm512_mul_ps(w,w);
			        const __m512 v  = _mm512_add_ps(t0,_mm512_add_ps(t1,t2));
			        return (_mm512_sqrt_ps(v));
			    
		          }


			 __attribute__((always_inline))
			 __attribute__((aligned(32)))
		         static inline
		         __m512 clip_zmm16r4(const __m512 x,
		                             const __m512 lo,
					     const __m512 hi) {

                               return (_mm512_max_ps(lo,_mm512_min_ps(x,hi)));
		          }


		         __attribute__((always_inline))
			 __attribute__((aligned(32)))
		         static inline
		          __m512d clip_zmm8r8(const __m512d x,
		                              const __m512d lo,
					      const __m512d hi) {

                               return (_mm512_max_pd(lo,_mm512_min_pd(x,hi)));
		          }













#endif /*__GMS_ROTATIONS_AVX512_HELPERS_H__*/
