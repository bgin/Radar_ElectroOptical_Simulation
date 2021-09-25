

#include "GMS_cephes.h"


#if !defined(DSP_32F_ACOS_32F_AVX512_BLOCK)
    #define DSP_32F_ACOS_32F_AVX512_BLOCK                         \
        int32_t idx = 0;                                          \
	const int32_t len = npoints/16;                           \
	__mmask16 condition = 0;                                  \
	register __m512 aVal,d,pi,pio2,x,y,z,arccosine;           \
	register __m512 n_third,p_third,fzeroes,fones;            \             
	register __m512 ftwos,ffours;                             \
	arccosine = _mm512_setzero_ps();                          \
	pi = _mm512_set1_ps(3.14159265358979323846f);             \
	pio2 = _mm512_set1_ps(1.5707963267948966192f);            \
        fzeroes = _mm512_setzero_ps();                            \
        fones = _mm512_set1_ps(1.0f);                             \
        ftwos = _mm512_set1_ps(2.0f);                             \
        ffours  = _mm512_set1_ps(4.0f);                           \
        n_third = _mm512_set1_ps(-0.3333333333333333333333333f);  \
        p_third = _mm512_set1_ps(0.3333333333333333333333333f);
#endif


          void
          acos_u_zmm16r4_zmm16r4_looped(float * __restrict b,
			                float * __restrict a,
			                const int32_t npoints) {
                  DSP_32F_ACOS_32F_AVX512_BLOCK
#if defined __ICC || defined __INTEL_COMPILER
#pragma code_align(32)
#endif
                   for(; idx != len; ++idx) {
                         _mm_prefetch((const char*)&a+32,_MM_HINT_T0);
                         aVal = _mm512_loadu_ps(a);
			 d    = aVal;
			 aVal = _mm512_div_ps(_mm512_sqrt_ps(_mm512_mul_ps(
			                             _mm512_add_ps(fones, aVal),
                                                             _mm512_sub_ps(fones, aVal))), aVal);
			 z    = aVal;
			 condition = _mm512_cmp_ps_mask(z,fzeroes,_MM_LT_OQ);
			 z = _mm512_maskz_sub_ps(condition,z,_mm512_mul_ps(z,ftwos));
			 condition = _mm512_cmp_ps(z,fones,_CMP_LT_OQ);
			 x = _mm512_maskz_add_ps(condition,z,
			                           _mm512_sub_ps(
						            _mm512_div_ps(fones,z),z));
			 /* for (i = 0; i < 2; i++) <-- Possibly branch mispredict and loop overhead */
			 x = _mm512_add_ps(x, _mm512_sqrt_ps(_mm512_fmadd_ps(x, x, fones)));
		         x = _mm512_add_ps(x, _mm512_sqrt_ps(_mm512_fmadd_ps(x, x, fones))); // THis line Compiler may eliminate
		         x = _mm512_div_ps(fones, x);
                         y = fzeroes;
		         // loop unrolled
			 y = _mm512_fmadd_ps(
                         y, _mm512_mul_ps(x, x),n_third); // removing call to pow
		         y = _mm512_fmadd_ps(
                         y, _mm512_mul_ps(x, x), p_third);  // removed call to pow
		         y = _mm512_mul_ps(y, _mm512_mul_ps(x, ffours));
			 condition = _mm512_cmp_ps_mask(z,fones,_CMP_GT_OQ);
			 y = _mm512_maskz_add_ps(condition,y,
			                                 _mm512_fnmadd_ps(y,ftwos,pio2));
			 arccosine = y;
			 condition = _mm512_cmp_ps_mask(aVal,fzeroes,_CMP_LT_OQ);
			 arccosine = _mm512_maskz_sub_ps(condition,
			                                 arccosine,
							     _mm512_mul_ps(arccosine,ftwos));
			 condition = _mm512_cmp_ps_mask(d,fzeroes,_CMP_LT_OQ);
			 arccosine = _mm512_maskz_add_ps(condition,arccosine,pi);
			 _mm512_storeu_ps(b,arccosine);
                         a += 16;
			 b += 16;           
		 }
#if defined __ICC || defined __INTEL_COMPILER
#pragma loop_count min(1),avg(8),max(15)
#endif
                      for(; idx != npoints; ++idx) {
                           b[i] = ceph_acosf(a[i]);
	              }

	  }


          void
          acos_a_zmm16r4_zmm16r4_looped(float * __restrict __attribute__((aligned(64))) b,
			                float * __restrict __attribute__((aligned(64))) a,
			                const int32_t npoints) {

                    DSP_32F_ACOS_32F_AVX512_BLOCK
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
			 d    = aVal;
			 aVal = _mm512_div_ps(_mm512_sqrt_ps(_mm512_mul_ps(
			                             _mm512_add_ps(fones, aVal),
                                                             _mm512_sub_ps(fones, aVal))), aVal);
			 z    = aVal;
			 condition = _mm512_cmp_ps_mask(z,fzeroes,_MM_LT_OQ);
			 z = _mm512_maskz_sub_ps(condition,z,_mm512_mul_ps(z,ftwos));
			 condition = _mm512_cmp_ps(z,fones,_CMP_LT_OQ);
			 x = _mm512_maskz_add_ps(condition,z,
			                           _mm512_sub_ps(
						            _mm512_div_ps(fones,z),z));
			 /* for (i = 0; i < 2; i++) <-- Possibly branch mispredict and loop overhead */
			 x = _mm512_add_ps(x, _mm512_sqrt_ps(_mm512_fmadd_ps(x, x, fones)));
		         x = _mm512_add_ps(x, _mm512_sqrt_ps(_mm512_fmadd_ps(x, x, fones))); // THis line Compiler may eliminate
		         x = _mm512_div_ps(fones, x);
                         y = fzeroes;
		         // loop unrolled
			 y = _mm512_fmadd_ps(
                         y, _mm512_mul_ps(x, x),n_third); // removing call to pow
		         y = _mm512_fmadd_ps(
                         y, _mm512_mul_ps(x, x), p_third);  // removed call to pow
		         y = _mm512_mul_ps(y, _mm512_mul_ps(x, ffours));
			 condition = _mm512_cmp_ps_mask(z,fones,_CMP_GT_OQ);
			 y = _mm512_maskz_add_ps(condition,y,
			                                 _mm512_fnmadd_ps(y,ftwos,pio2));
			 arccosine = y;
			 condition = _mm512_cmp_ps_mask(aVal,fzeroes,_CMP_LT_OQ);
			 arccosine = _mm512_maskz_sub_ps(condition,
			                                 arccosine,
							     _mm512_mul_ps(arccosine,ftwos));
			 condition = _mm512_cmp_ps_mask(d,fzeroes,_CMP_LT_OQ);
			 arccosine = _mm512_maskz_add_ps(condition,arccosine,pi);
			 _mm512_store_ps(b,arccosine);
                         a += 16;
			 b += 16;           
		 }
#if defined __ICC || defined __INTEL_COMPILER
#pragma loop_count min(1),avg(8),max(15)
#endif
                      for(; idx != npoints; ++idx) {
                           b[i] = ceph_acosf(a[i]);
	              }      
	  }


          __m512
          acos_zmm16r4_zmm16r4(const __m512 v) {

                  register __m512 aVal,d,pi,pio2,x,y,z,arccosine;           
	          register __m512 n_third,p_third,fzeroes,fones;                         
	          register __m512 ftwos,ffours;
		  __mmask16 condition = 0;  
	          arccosine = _mm512_setzero_ps();                          
	          pi = _mm512_set1_ps(3.14159265358979323846f);             
	          pio2 = _mm512_set1_ps(1.5707963267948966192f);            
                  fzeroes = _mm512_setzero_ps();                            
                  fones = _mm512_set1_ps(1.0f);                             
                  ftwos = _mm512_set1_ps(2.0f);                             
                  ffours  = _mm512_set1_ps(4.0f);                           
                  n_third = _mm512_set1_ps(-0.3333333333333333333333333f);  
                  p_third = _mm512_set1_ps(0.3333333333333333333333333f);
		  aVal = v;
		  d    = aVal;
		  aVal = _mm512_div_ps(_mm512_sqrt_ps(_mm512_mul_ps(
			                             _mm512_add_ps(fones, aVal),
                                                             _mm512_sub_ps(fones, aVal))), aVal);
		  z    = aVal;
		  condition = _mm512_cmp_ps_mask(z,fzeroes,_MM_LT_OQ);
		  z = _mm512_maskz_sub_ps(condition,z,_mm512_mul_ps(z,ftwos));
		  condition = _mm512_cmp_ps(z,fones,_CMP_LT_OQ);
		  x = _mm512_maskz_add_ps(condition,z,
			                           _mm512_sub_ps(
						            _mm512_div_ps(fones,z),z));
			 /* for (i = 0; i < 2; i++) <-- Possibly branch mispredict and loop overhead */
		  x = _mm512_add_ps(x, _mm512_sqrt_ps(_mm512_fmadd_ps(x, x, fones)));
		  x = _mm512_add_ps(x, _mm512_sqrt_ps(_mm512_fmadd_ps(x, x, fones))); // THis line Compiler may eliminate
		  x = _mm512_div_ps(fones, x);
                  y = fzeroes;
		         // loop unrolled
		  y = _mm512_fmadd_ps(
                  y, _mm512_mul_ps(x, x),n_third); // removing call to pow
		  y = _mm512_fmadd_ps(
                  y, _mm512_mul_ps(x, x), p_third);  // removed call to pow
		  y = _mm512_mul_ps(y, _mm512_mul_ps(x, ffours));
		  condition = _mm512_cmp_ps_mask(z,fones,_CMP_GT_OQ);
		  y = _mm512_maskz_add_ps(condition,y,
			                      _mm512_fnmadd_ps(y,ftwos,pio2));
		  arccosine = y;
		  condition = _mm512_cmp_ps_mask(aVal,fzeroes,_CMP_LT_OQ);
		  arccosine = _mm512_maskz_sub_ps(condition,
			                                 arccosine,
							     _mm512_mul_ps(arccosine,ftwos));
		  condition = _mm512_cmp_ps_mask(d,fzeroes,_CMP_LT_OQ);
		  arccosine = _mm512_maskz_add_ps(condition,arccosine,pi);
		  return (arccosine);
	  }
