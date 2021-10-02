
#include "GMS_32f_sin_32f.h"
#include "GMS_cephes.h"


#if !defined(DSP_32F_SIN_32F_BLOCK)
#define DSP_32F_SIN_32F_BLOCK                                             \
         const __m256 m4pio   =  _mm256_set1_ps(1.273239545F);            \
	 const __m256 pio4A   =  _mm256_set1_ps(0.78515625F);             \
	 const __m256 pio4B   =  _mm256_set1_ps(0.241876e-3F);            \
	 const __m256 finv8   =  _mm256_set1_ps(0.125F);                  \
	 const __m256 ffours  = _mm256_set1_ps(4.0F);                     \
	 const __m256 fftwos  = _mm256_set1_ps(2.0F);                     \
	 const __m256 fones   = _mm256_set1_ps(1.0F);                     \
	 const __m256 fzeroes = _mm256_setzero_ps();                      \
	 const __m256 finv2   = _mm256_set1_ps(0.5f);                     \
	 const __m256 cp1     = _mm256_set1_ps(1.0F);                     \
	 const __m256 cp2     = _mm256_set1_ps(0.83333333e-1F);           \
	 const __m256 cp3     = _mm256_set1_ps(0.2777778e-2F);            \
	 const __m256 cp4     = _mm256_set1_ps(0.49603e-4F);              \
         const __m256 cp5     = _mm256_set1_ps(0.551e-6F);                \
	 const __m256i ones   = _mm256_set1_epi32(1);                     \
	 const __m256i twos   = _mm256_set1_epi32(2);                     \
	 const __m256i fours  = _mm256_set1_epi32(4);                     \
	 __m256 sine = fzeroes;                                           \
	 __m256 s    = fzeroes;                                           \
	 __m256 cosine = fzeroes;                                         \
	 __m256 t0     = fzeroes;                                         \
	 __m256 condition1;                                               \
	 __m256 condition2;                                               \
	 __m256i q;                                                       \
	 __m256i r;                                                       \
	 int32_t idx = 0;                                                 \
	 int32_t len = npoints/8;
#endif



          void
          sin_u_ymm8r4_ymm8r4_looped(float * __restrict b,
			             float * __restrict a ,
			             const int32_t npoints) {

                  DSP_32F_SIN_32F_BLOCK
#if defined __ICC || defined __INTEL_COMPILER
#pragma code_align(32)
#endif
                  for(; idx != len; ++idx) {
                       _mm_prefetch((const char*)&a+32,_MM_HINT_T0);
		       aVal = _mm256_loadu_ps(a);
		       s    = _mm256_sub_ps(aVal,
                                    _mm256_and_ps(_mm256_mul_ps(aVal, ftwos),
                                                  _mm256_cmp_ps(aVal, fzeroes, _CMP_LT_OQ)));
                       q = _mm256_cvtps_epi32(_mm256_floor_ps(_mm256_mul_ps(s, m4pi)));
                       r = _mm256_add_epi32(q, _mm256_and_si256(q, ones));
                       s = _mm256_fnmadd_ps(_mm256_cvtepi32_ps(r), pio4A, s);
                       s = _mm256_fnmadd_ps(_mm256_cvtepi32_ps(r), pio4B, s);
		       s = _mm256_mul_ps(s,finv8);
		       s = _mm256_mul_ps(s,s);
		       s = _mm256_mul_ps(
                                    _mm256_fmadd_ps(
                                             _mm256_fmsub_ps(
                                                  _mm256_fmadd_ps(
						           _mm256_fmsub_ps(s, cp5, cp4), s, cp3), s, cp2),
                                                  s,
                                            cp1),
                                     s);
		        t0 = _mm256_sub_ps(ffours,s);
			s  = _mm256_mul_ps(s,t0);
			s  = _mm256_mul_ps(s,t0);
			s  = _mm256_mul_ps(s,t0);
			s  = _mm256_mul_ps(s,finv2);
			sine = _mm256_sqrt_ps(
			               _mm256_mul_ps(_mm256_sub_ps(ftwos, s), s));
                        cosine = _mm256_sub_ps(fones, s);
                        condition1 = _mm256_cmp_ps(
                                        _mm256_cvtepi32_ps(
					         _mm256_and_si256(_mm256_add_epi32(q, ones), twos)),
                                                    fzeroes,
                                                       _CMP_NEQ_UQ);
                        condition2 = _mm256_cmp_ps(
                                          _mm256_cmp_ps(
                                             _mm256_cvtepi32_ps(
					              _mm256_and_si256(q, fours)), fzeroes, _CMP_NEQ_UQ),
                                                       _mm256_cmp_ps(aVal, fzeroes, _CMP_LT_OS),
                                                                                            _CMP_NEQ_UQ);
			sine =
                              _mm256_add_ps(sine,
			               _mm256_and_ps(_mm256_sub_ps(cosine, sine), condition1));
                        sine = _mm256_sub_ps(
                              sine, _mm256_and_ps(_mm256_mul_ps(sine, ftwos), condition2));
			_mm256_storeu_ps(b,sine);
			a += 8;
			b += 8;
		 }
		 idx = len*8;
#if defined __ICC || defined __INTEL_COMPILER
#pragma loop_count min(1),avg(4),max(7)
#endif
              for(; idx != npoints; ++idx) {
                  b[i] = ceph_sinf(a[i]);
	      }		 
	  }



          void
          sin_a_ymm8r4_ymm8r4_looped(float * __restrict __attribute__((aligned(32))) b,
			             float * __restrict __attribute__((aligned(32))) a,
			             const int32_t npoints) {

                 DSP_32F_SIN_32F_BLOCK
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
		       s    = _mm256_sub_ps(aVal,
                                    _mm256_and_ps(_mm256_mul_ps(aVal, ftwos),
                                                  _mm256_cmp_ps(aVal, fzeroes, _CMP_LT_OQ)));
                       q = _mm256_cvtps_epi32(_mm256_floor_ps(_mm256_mul_ps(s, m4pi)));
                       r = _mm256_add_epi32(q, _mm256_and_si256(q, ones));
                       s = _mm256_fnmadd_ps(_mm256_cvtepi32_ps(r), pio4A, s);
                       s = _mm256_fnmadd_ps(_mm256_cvtepi32_ps(r), pio4B, s);
		       s = _mm256_mul_ps(s,finv8);
		       s = _mm256_mul_ps(s,s);
		       s = _mm256_mul_ps(
                                    _mm256_fmadd_ps(
                                             _mm256_fmsub_ps(
                                                  _mm256_fmadd_ps(
						           _mm256_fmsub_ps(s, cp5, cp4), s, cp3), s, cp2),
                                                  s,
                                            cp1),
                                     s);
		        t0 = _mm256_sub_ps(ffours,s);
			s  = _mm256_mul_ps(s,t0);
			s  = _mm256_mul_ps(s,t0);
			s  = _mm256_mul_ps(s,t0);
			s  = _mm256_mul_ps(s,finv2);
			sine = _mm256_sqrt_ps(
			               _mm256_mul_ps(_mm256_sub_ps(ftwos, s), s));
                        cosine = _mm256_sub_ps(fones, s);
                        condition1 = _mm256_cmp_ps(
                                        _mm256_cvtepi32_ps(
					         _mm256_and_si256(_mm256_add_epi32(q, ones), twos)),
                                                    fzeroes,
                                                       _CMP_NEQ_UQ);
                        condition2 = _mm256_cmp_ps(
                                          _mm256_cmp_ps(
                                             _mm256_cvtepi32_ps(
					              _mm256_and_si256(q, fours)), fzeroes, _CMP_NEQ_UQ),
                                                       _mm256_cmp_ps(aVal, fzeroes, _CMP_LT_OS),
                                                                                            _CMP_NEQ_UQ);
			sine =
                              _mm256_add_ps(sine,
			               _mm256_and_ps(_mm256_sub_ps(cosine, sine), condition1));
                        sine = _mm256_sub_ps(
                              sine, _mm256_and_ps(_mm256_mul_ps(sine, ftwos), condition2));
			_mm256_store_ps(b,sine);
			a += 8;
			b += 8;
		 }
		 idx = len*8;
#if defined __ICC || defined __INTEL_COMPILER
#pragma loop_count min(1),avg(4),max(7)
#endif
              for(; idx != npoints; ++idx) {
                  b[i] = ceph_sinf(a[i]);
	      }		 
	  }



          __m256
          sin_ymm8r4_ymm8r4(const __m256 v) {

                  const __m256 m4pio   =  _mm256_set1_ps(1.273239545F);           
	          const __m256 pio4A   =  _mm256_set1_ps(0.78515625F);            
	          const __m256 pio4B   =  _mm256_set1_ps(0.241876e-3F);           
	          const __m256 finv8   =  _mm256_set1_ps(0.125F);                  
	          const __m256 ffours  =  _mm256_set1_ps(4.0F);                    
	          const __m256 fftwos  =  _mm256_set1_ps(2.0F);                  
	          const __m256 fones   =  _mm256_set1_ps(1.0F);                    
	          const __m256 fzeroes =  _mm256_setzero_ps();                     
	          const __m256 finv2   =  _mm256_set1_ps(0.5f);                   
	          const __m256 cp1     =  _mm256_set1_ps(1.0F);                    
	          const __m256 cp2     =  _mm256_set1_ps(0.83333333e-1F);          
	          const __m256 cp3     =  _mm256_set1_ps(0.2777778e-2F);           
	          const __m256 cp4     =  _mm256_set1_ps(0.49603e-4F);              
                  const __m256 cp5     =  _mm256_set1_ps(0.551e-6F);               
	          const __m256i ones   =  _mm256_set1_epi32(1);                     
	          const __m256i twos   =  _mm256_set1_epi32(2);                    
	          const __m256i fours  =  _mm256_set1_epi32(4);                    
	          __m256 sine = fzeroes;                                        
	          __m256 s    = fzeroes;                                          
	          __m256 cosine = fzeroes;                                        
	          __m256 t0     = fzeroes;                                      
	          __m256 condition1;                                              
	          __m256 condition2;                                             
	          __m256i q;                                                     
	          __m256i r;
		  aVal = v;
		  s    = _mm256_sub_ps(aVal,
                                    _mm256_and_ps(_mm256_mul_ps(aVal, ftwos),
                                                  _mm256_cmp_ps(aVal, fzeroes, _CMP_LT_OQ)));
                  q = _mm256_cvtps_epi32(_mm256_floor_ps(_mm256_mul_ps(s, m4pi)));
                  r = _mm256_add_epi32(q, _mm256_and_si256(q, ones));
                  s = _mm256_fnmadd_ps(_mm256_cvtepi32_ps(r), pio4A, s);
                  s = _mm256_fnmadd_ps(_mm256_cvtepi32_ps(r), pio4B, s);
		  s = _mm256_mul_ps(s,finv8);
		  s = _mm256_mul_ps(s,s);
		  s = _mm256_mul_ps(
                                    _mm256_fmadd_ps(
                                             _mm256_fmsub_ps(
                                                  _mm256_fmadd_ps(
						           _mm256_fmsub_ps(s, cp5, cp4), s, cp3), s, cp2),
                                                  s,
                                            cp1),
                                     s);
		   t0 = _mm256_sub_ps(ffours,s);
		   s  = _mm256_mul_ps(s,t0);
		   s  = _mm256_mul_ps(s,t0);
		   s  = _mm256_mul_ps(s,t0);
		   s  = _mm256_mul_ps(s,finv2);
		   sine = _mm256_sqrt_ps(
			               _mm256_mul_ps(_mm256_sub_ps(ftwos, s), s));
                   cosine = _mm256_sub_ps(fones, s);
                   condition1 = _mm256_cmp_ps(
                                        _mm256_cvtepi32_ps(
					         _mm256_and_si256(_mm256_add_epi32(q, ones), twos)),
                                                    fzeroes,
                                                       _CMP_NEQ_UQ);
                    condition2 = _mm256_cmp_ps(
                                          _mm256_cmp_ps(
                                             _mm256_cvtepi32_ps(
					              _mm256_and_si256(q, fours)), fzeroes, _CMP_NEQ_UQ),
                                                       _mm256_cmp_ps(aVal, fzeroes, _CMP_LT_OS),
                                                                                            _CMP_NEQ_UQ);
		    sine =
                              _mm256_add_ps(sine,
			               _mm256_and_ps(_mm256_sub_ps(cosine, sine), condition1));
                    sine = _mm256_sub_ps(
                              sine, _mm256_and_ps(_mm256_mul_ps(sine, ftwos), condition2));
		    return (sine);
	  }
 
