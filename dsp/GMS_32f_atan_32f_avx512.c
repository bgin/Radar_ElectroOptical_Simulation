

#include "GMS_cephes.h"


#if !defined(DSP_32F_ATAN_32F_AVX512_BLOCK)
    #define DSP_32F_ATAN_32F_AVX512_BLOCK                        \
       int32_t idx = 0;                                          \
       const int32_t len = npoints/16;                           \
       register __m512 aVal,pio2, x, y, z, arctangent;           \
       register __m512 n_third,p_third,fzeroes,fones;	         \
       register __m512 ftwos,ffours,t0;                          \
       __mmask16 condition = 0;                                  \
       pio2 = _mm512_set1_ps(1.5707963267948966192f);            \
       fzeroes = _mm512_setzero_ps();                            \
       t0 = fzeroes;                                             \
       fones = _mm512_set1_ps(1.0f);                             \
       ftwos = _mm512_set1_ps(2.0f);                             \
       ffours  = _mm512_set1_ps(4.0f);                           \
       n_third = _mm512_set1_ps(-0.3333333333333333333333333f);  \
       p_third = _mm512_set1_ps(0.3333333333333333333333333f);
#endif

          void
          atan_u_zmm16r4_zmm16r4_looped(float * __restrict b,
			                float * __restrict a,
			                const int32_t npoints) {
                DSP_32F_ATAN_32F_AVX512_BLOCK
#if defined __ICC || defined __INTEL_COMPILER
#pragma code_align(32)
#endif
                for(; idx != len; ++idx) {
                      _mm_prefetch((const char*)&a+32,_MM_HINT_T0);
		      aVal = _mm512_loadu_ps(a);
		      z = aVal;
		      condition = _mm512_cmp_ps_mask(z,fzeroes,_CMP_LT_OQ);
		      z = _mm512_maskz_sub_ps(condition,z,
		                                    _mm512_mul_ps(z,ftwos));
		      condition = _mm512_cmp_ps_mask(z,fones,_CMP_LT_OQ);
		      x = _mm512_maskz_add_ps(z,
		                            _mm512_sub_ps(_mm512_div_ps(fones,z),z));
		       // Original loop of 2-cycles removed
		      /*
                             for (i = 0; i < 2; i++) {
                                 x = _mm256_add_ps(x, _mm256_sqrt_ps(_mm256_fmadd_ps(x, x, fones)));
                             }
                       */
		      t0 = _mm512_sqrt_ps(_mm512_fmadd_ps(x, x, fones);
		      x  = _mm512_add_ps(x,t0);
		      x  = _mm512_add_ps(x,t0);
		      x  = _mm512_div_ps(fones,x);
		       // Original loop of 2-cycles removed
		      /*
                            for (j = TERMS - 1; j >= 0; j--) {
                                 y = _mm256_fmadd_ps(
                                 y, _mm256_mul_ps(x, x), _mm256_set1_ps(pow(-1, j) / (2 * j + 1)));
                            }
                       */
		       y = _mm512_fmadd_ps(
                             y, _mm512_mul_ps(x, x),n_third); // removing call to pow
		       y = _mm512_fmadd_ps(
                             y, _mm512_mul_ps(x, x), p_third);  // removed call to pow
		       y = _mm512_mul_ps(y, _mm512_mul_ps(x, ffours));
		       condition = _mm512_cmp_ps_mask(z,fones,_CMP_GT_OQ);
		       y = _mm512_maskz_add_ps(condition,y,_mm512_fnmadd_ps(y,ftwos,pio2));
		       arctangent = y;
		       condition = _mm512_cmp_ps_mask(aVal,fzeroes,_CMP_LT_OQ);
		       arctangent = _mm512_maskz_sub_ps(condition,arctangent,
			                               _mm512_mul_ps(arctangent,ftwos));
		       _mm512_storeu_ps(b,arctangent);
		       a += 16;
		       b += 16;
		}
		 idx = len*16;
#if defined __ICC || defined __INTEL_COMPILER
#pragma loop_count min(1),avg(4),max(7)
#endif
                for(; idx != npoints; ++idx) {
                      b[i] = ceph_atanf(a[i]);
	          }  

	  }


	  void
          atan_a_zmm16r4_zmm16r4_looped(float * __restrict __ATTR_ALIGN__(64) b,
			                float * __restrict __ATTR_ALIGN__(64) a,
			                const int32_t npoints) {

                  DSP_32F_ATAN_32F_AVX512_BLOCK
#if defined __ICC || defined __INTEL_COMPILER
              __assume_aligned(b,64);
	      __assume_aligned(a,64);
#elif defined __GNUC__ && !defined __INTEL_COMPILER
              b = (float*)__builtin_assume_aligned(b,64);
	      a = (float*)__builtin_assume_aligned(a,64);
#endif
#if defined __ICC || defined __INTEL_COMPILER
#pragma code_align(32)
#endif
                for(; idx != len; ++idx) {
                      _mm_prefetch((const char*)&a+32,_MM_HINT_T0);
		      aVal = _mm512_load_ps(a);
		      z = aVal;
		      condition = _mm512_cmp_ps_mask(z,fzeroes,_CMP_LT_OQ);
		      z = _mm512_maskz_sub_ps(condition,z,
		                                    _mm512_mul_ps(z,ftwos));
		      condition = _mm512_cmp_ps_mask(z,fones,_CMP_LT_OQ);
		      x = _mm512_maskz_add_ps(z,
		                            _mm512_sub_ps(_mm512_div_ps(fones,z),z));
		       // Original loop of 2-cycles removed
		      /*
                             for (i = 0; i < 2; i++) {
                                 x = _mm256_add_ps(x, _mm256_sqrt_ps(_mm256_fmadd_ps(x, x, fones)));
                             }
                       */
		      t0 = _mm512_sqrt_ps(_mm512_fmadd_ps(x, x, fones);
		      x  = _mm512_add_ps(x,t0);
		      x  = _mm512_add_ps(x,t0);
		      x  = _mm512_div_ps(fones,x);
		       // Original loop of 2-cycles removed
		      /*
                            for (j = TERMS - 1; j >= 0; j--) {
                                 y = _mm256_fmadd_ps(
                                 y, _mm256_mul_ps(x, x), _mm256_set1_ps(pow(-1, j) / (2 * j + 1)));
                            }
                       */
		      y = _mm512_fmadd_ps(
                             y, _mm512_mul_ps(x, x),n_third); // removing call to pow
		      y = _mm512_fmadd_ps(
                             y, _mm512_mul_ps(x, x), p_third);  // removed call to pow
		      y = _mm512_mul_ps(y, _mm512_mul_ps(x, ffours));
		      condition = _mm512_cmp_ps_mask(z,fones,_CMP_GT_OQ);
		      y = _mm512_maskz_add_ps(condition,y,_mm512_fnmadd_ps(y,ftwos,pio2));
		      arctangent = y;
		      condition = _mm512_cmp_ps_mask(aVal,fzeroes,_CMP_LT_OQ);
		      arctangent = _mm512_maskz_sub_ps(condition,arctangent,
			                               _mm512_mul_ps(arctangent,ftwos));
		      _mm512_store_ps(b,arctangent);
		      a += 16;
		      b += 16;
		}
		 idx = len*16;
#if defined __ICC || defined __INTEL_COMPILER
#pragma loop_count min(1),avg(4),max(7)
#endif
                for(; idx != npoints; ++idx) {
                      b[i] = ceph_atanf(a[i]);
	          }
	  }


	  __m512
          atan_zmm16r4_zmm16r4(const __m512 v) {

                     register __m512 aVal,pio2, x, y, z, arctangent;           
                     register __m512 n_third,p_third,fzeroes,fones;	         
                     register __m512 ftwos,ffours,t0;                          
                     __mmask16 condition = 0;                                  
                     pio2 = _mm512_set1_ps(1.5707963267948966192f);            
                     fzeroes = _mm512_setzero_ps();                            
                     t0 = fzeroes;                                             
                     fones = _mm512_set1_ps(1.0f);                             
                     ftwos = _mm512_set1_ps(2.0f);                             
                     ffours  = _mm512_set1_ps(4.0f);                           
                     n_third = _mm512_set1_ps(-0.3333333333333333333333333f);  
                     p_third = _mm512_set1_ps(0.3333333333333333333333333f);
		     aVal = v;
		     z = aVal;
		     condition = _mm512_cmp_ps_mask(z,fzeroes,_CMP_LT_OQ);
		     z = _mm512_maskz_sub_ps(condition,z,
		                                    _mm512_mul_ps(z,ftwos));
		     condition = _mm512_cmp_ps_mask(z,fones,_CMP_LT_OQ);
		     x = _mm512_maskz_add_ps(z,
		                            _mm512_sub_ps(_mm512_div_ps(fones,z),z));
		       // Original loop of 2-cycles removed
		      /*
                             for (i = 0; i < 2; i++) {
                                 x = _mm256_add_ps(x, _mm256_sqrt_ps(_mm256_fmadd_ps(x, x, fones)));
                             }
                       */
		     t0 = _mm512_sqrt_ps(_mm512_fmadd_ps(x, x, fones);
		     x  = _mm512_add_ps(x,t0);
		     x  = _mm512_add_ps(x,t0);
		     x  = _mm512_div_ps(fones,x);
		       // Original loop of 2-cycles removed
		      /*
                            for (j = TERMS - 1; j >= 0; j--) {
                                 y = _mm256_fmadd_ps(
                                 y, _mm256_mul_ps(x, x), _mm256_set1_ps(pow(-1, j) / (2 * j + 1)));
                            }
                       */
		     y = _mm512_fmadd_ps(
                             y, _mm512_mul_ps(x, x),n_third); // removing call to pow
		     y = _mm512_fmadd_ps(
                             y, _mm512_mul_ps(x, x), p_third);  // removed call to pow
		     y = _mm512_mul_ps(y, _mm512_mul_ps(x, ffours));
		     condition = _mm512_cmp_ps_mask(z,fones,_CMP_GT_OQ);
	             y = _mm512_maskz_add_ps(condition,y,_mm512_fnmadd_ps(y,ftwos,pio2));
		     arctangent = y;
		     condition = _mm512_cmp_ps_mask(aVal,fzeroes,_CMP_LT_OQ);
		     arctangent = _mm512_maskz_sub_ps(condition,arctangent,
			                               _mm512_mul_ps(arctangent,ftwos));
		     return (arctangent);
	  }
