

#include "GMS_cephes.h"


#if !defined(DSP_32F_ACOS_32F_BLOCK)
    #define DSP_32F_ACOS_32F_BLOCK                               \
       int32_t idx = 0;                                          \
       const int32_t len = npoints/8;                            \
       register __m256 aVal, d, pi, pio2, x, y, z, arccosine;    \
       register __m256 n_third,p_third,condition,fzeroes,fones;	 \
       register __m256 ftwos,ffours;                             \ 
       pi = _mm256_set1_ps(3.14159265358979323846f);             \
       pio2 = _mm256_set1_ps(1.5707963267948966192f);            \
       fzeroes = _mm256_setzero_ps();                            \
       fones = _mm256_set1_ps(1.0f);                             \
       ftwos = _mm256_set1_ps(2.0f);                             \
       ffours  = _mm256_set1_ps(4.0f);                           \
       n_third = _mm256_set1_ps(-0.3333333333333333333333333f);  \
       p_third = _mm256_set1_ps(0.3333333333333333333333333f);
#endif


void
acos_u_ymm8r4_ymm8r4_looped(float * __restrict b,
			    float * __restrict a,
			    const int32_t npoints) {

      DSP_32F_ACOS_32F_BLOCK
#if defined __ICC || defined __INTEL_COMPILER
#pragma code_align(32)
#endif
            for(; idx != len; ++idx) {
		    _mm_prefetch((const char*)&a+32,_MM_HINT_T0);
                  aVal = _mm256_loadu_ps(a);
		  d    = aVal;
		  aVal = _mm256_div_ps(_mm256_sqrt_ps(_mm256_mul_ps(_mm256_add_ps(fones, aVal),
                                                          _mm256_sub_ps(fones, aVal))), aVal);
                  z = aVal;
		  condition =  _mm256_cmp_ps(z, fzeroes, _CMP_LT_OQ);
		  z = _mm256_sub_ps(z, _mm256_and_ps(_mm256_mul_ps(z, ftwos), condition));
                  condition = _mm256_cmp_ps(z, fones, _CMP_LT_OS);
                  x = _mm256_add_ps(
                        z, _mm256_and_ps(_mm256_sub_ps(_mm256_div_ps(fones, z), z), condition));
		  /* for (i = 0; i < 2; i++) <-- Possibly branch mispredict and loop overhead */ 
		  x = _mm256_add_ps(x, _mm256_sqrt_ps(_mm256_fmadd_ps(x, x, fones)));
		  x = _mm256_add_ps(x, _mm256_sqrt_ps(_mm256_fmadd_ps(x, x, fones))); // THis line Compiler may eliminate
		  x = _mm256_div_ps(fones, x);
                  y = fzeroes;
		  // loop unrolled
		  y = _mm256_fmadd_ps(
                         y, _mm256_mul_ps(x, x),n_third); // removing call to pow
		  y = _mm256_fmadd_ps(
                         y, _mm256_mul_ps(x, x), p_third);  // removed call to pow
		  y = _mm256_mul_ps(y, _mm256_mul_ps(x, ffours));
                  condition = _mm256_cmp_ps(z, fones, _CMP_GT_OS);
                  y = _mm256_add_ps(y, _mm256_and_ps(_mm256_fnmadd_ps(y, ftwos, pio2), condition));
                  arccosine = y;
                  condition = _mm256_cmp_ps(aVal, fzeroes, _CMP_LT_OQ);
                  arccosine = _mm256_sub_ps(
                              arccosine, _mm256_and_ps(_mm256_mul_ps(arccosine, ftwos), condition));
                  condition = _mm256_cmp_ps(d, fzeroes, _CMP_LT_OS);
                  arccosine = _mm256_add_ps(arccosine, _mm256_and_ps(pi, condition));
                  _mm256_storeu_ps(b, arccosine);
                  a += 8;
                  b += 8;
	      }
	      idx = len*8;
#if defined __ICC || defined __INTEL_COMPILER
#pragma loop_count min(1),avg(4),max(7)
#endif
              for(; idx != npoints; ++idx) {
                  b[i] = ceph_acosf(a[i]);
	      }
}


void
acos_a_ymm8r4_ymm8r4_looped(float * __restrict __attribute__((aligned(32))) b,
			    float * __restrict __attribute__((aligned(32))) a,
			    const int32_t npoints) {

        DSP_32F_ACOS_S32F_BLOCK
#if defined __ICC || defined __INTEL_COMPILER
              __assume_aligned(b,32);
	      __assume_aligned(a,32);
#elif defined __GNUC__ && !defined __INTEL_COMPILER
              b = (float*)__builtin_assume_aligned(b,32);
	      a = (float*)__builtin_assume_aligned(a,32);
#endif
#if defined __ICC || defined __INTEL_COMPILER
#pragma code_align(32)
#endif
              for(; idx != len; ++idx) {
		    _mm_prefetch((const char*)&a+32,_MM_HINT_T0);
                  aVal = _mm256_load_ps(a);
		  d    = aVal;
		  aVal = _mm256_div_ps(_mm256_sqrt_ps(_mm256_mul_ps(_mm256_add_ps(fones, aVal),
                                                          _mm256_sub_ps(fones, aVal))), aVal);
                  z = aVal;
		  condition =  _mm256_cmp_ps(z, fzeroes, _CMP_LT_OQ);
		  z = _mm256_sub_ps(z, _mm256_and_ps(_mm256_mul_ps(z, ftwos), condition));
                  condition = _mm256_cmp_ps(z, fones, _CMP_LT_OS);
                  x = _mm256_add_ps(
                        z, _mm256_and_ps(_mm256_sub_ps(_mm256_div_ps(fones, z), z), condition));
		  /* for (i = 0; i < 2; i++) <-- Possibly branch mispredict and loop overhead */ 
		  x = _mm256_add_ps(x, _mm256_sqrt_ps(_mm256_fmadd_ps(x, x, fones)));
		  x = _mm256_add_ps(x, _mm256_sqrt_ps(_mm256_fmadd_ps(x, x, fones))); // THis line Compiler may eliminate
		  x = _mm256_div_ps(fones, x);
                  y = fzeroes;
		  // loop unrolled
		  y = _mm256_fmadd_ps(
                         y, _mm256_mul_ps(x, x),n_third); // removing call to pow
		  y = _mm256_fmadd_ps(
                         y, _mm256_mul_ps(x, x), p_third);  // removed call to pow
		  y = _mm256_mul_ps(y, _mm256_mul_ps(x, ffours));
                  condition = _mm256_cmp_ps(z, fones, _CMP_GT_OS);
                  y = _mm256_add_ps(y, _mm256_and_ps(_mm256_fnmadd_ps(y, ftwos, pio2), condition));
                  arccosine = y;
                  condition = _mm256_cmp_ps(aVal, fzeroes, _CMP_LT_OQ);
                  arccosine = _mm256_sub_ps(
                              arccosine, _mm256_and_ps(_mm256_mul_ps(arccosine, ftwos), condition));
                  condition = _mm256_cmp_ps(d, fzeroes, _CMP_LT_OS);
                  arccosine = _mm256_add_ps(arccosine, _mm256_and_ps(pi, condition));
                  _mm256_store_ps(b, arccosine);
                  a += 8;
                  b += 8;
	      }
	       idx = len*8;
#if defined __ICC || defined __INTEL_COMPILER
#pragma loop_count min(1),avg(4),max(7)
#endif
              for(; idx != npoints; ++idx) {
                  b[i] = ceph_acosf(a[i]);
	      }
}


        __m256
       acos_ymm8r4_ymm8r4(const __m256 v) {

                  register __m256 aVal, d, pi, pio2, x, y, z, arccosine;    
                 register __m256 n_third,p_third,condition,fzeroes,fones;	 
                 register __m256 ftwos,ffours;                              
                 pi = _mm256_set1_ps(3.14159265358979323846f);             
                 pio2 = _mm256_set1_ps(1.5707963267948966192f);            
                 fzeroes = _mm256_setzero_ps();                            
                 fones = _mm256_set1_ps(1.0f);                             
                 ftwos = _mm256_set1_ps(2.0f);                             
                 ffours  = _mm256_set1_ps(4.0f);                           
                 n_third = _mm256_set1_ps(-0.3333333333333333333333333f);  
                 p_third = _mm256_set1_ps(0.3333333333333333333333333f);
		 arccosine = _mm256_setzero_ps();
		 aVal = v;
		 d    = aVal;
		 aVal = _mm256_div_ps(_mm256_sqrt_ps(_mm256_mul_ps(_mm256_add_ps(fones, aVal),
                                                          _mm256_sub_ps(fones, aVal))), aVal);
                 z = aVal;
		 condition =  _mm256_cmp_ps(z, fzeroes, _CMP_LT_OQ);
		 z = _mm256_sub_ps(z, _mm256_and_ps(_mm256_mul_ps(z, ftwos), condition));
                 condition = _mm256_cmp_ps(z, fones, _CMP_LT_OS);
                 x = _mm256_add_ps(
                        z, _mm256_and_ps(_mm256_sub_ps(_mm256_div_ps(fones, z), z), condition));
		  /* for (i = 0; i < 2; i++) <-- Possibly branch mispredict and loop overhead */ 
		 x = _mm256_add_ps(x, _mm256_sqrt_ps(_mm256_fmadd_ps(x, x, fones)));
		 x = _mm256_add_ps(x, _mm256_sqrt_ps(_mm256_fmadd_ps(x, x, fones))); // THis line Compiler may eliminate
		 x = _mm256_div_ps(fones, x);
                 y = fzeroes;
		  // loop unrolled
		 y = _mm256_fmadd_ps(
                         y, _mm256_mul_ps(x, x),n_third); // removing call to pow
		 y = _mm256_fmadd_ps(
                         y, _mm256_mul_ps(x, x), p_third);  // removed call to pow
		 y = _mm256_mul_ps(y, _mm256_mul_ps(x, ffours));
                 condition = _mm256_cmp_ps(z, fones, _CMP_GT_OS);
                 y = _mm256_add_ps(y, _mm256_and_ps(_mm256_fnmadd_ps(y, ftwos, pio2), condition));
                 arccosine = y;
                 condition = _mm256_cmp_ps(aVal, fzeroes, _CMP_LT_OQ);
                 arccosine = _mm256_sub_ps(
                              arccosine, _mm256_and_ps(_mm256_mul_ps(arccosine, ftwos), condition));
                 condition = _mm256_cmp_ps(d, fzeroes, _CMP_LT_OS);
                 arccosine = _mm256_add_ps(arccosine, _mm256_and_ps(pi, condition));
		 return (arccosine);
}
